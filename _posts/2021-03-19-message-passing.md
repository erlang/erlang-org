---
layout: post
title: A few notes on message passing
tags: erts messages signals
author: John Högberg
---

Message passing has always been central to Erlang, and while reasonably
well-documented we've avoided going into too much detail to give us more
freedom when implementing it. There's nothing preventing us from describing it
in a blog post though, so let's have a closer look!

Erlang processes communicate with each other by sending each other _signals_
(not to be confused with Unix signals). There are many different kinds and
_messages_ are just the most common. Practically everything involving more than
one process uses signals internally: for example, the `link/1` function is
implemented by having the involved processes talk back and forth until they've
agreed on a link.

This helps us avoid a great deal of locks and would make an interesting blog
post on its own, but for now we only need to keep two things in mind: all
signals (including messages) are continuously received and handled behind the
scenes, and they have a [defined order]:

Signals between two processes are guaranteed to arrive in the order they were
sent. In other words, if process `A` sends signal `1` and then `2` to process
`B`, signal `1` is guaranteed to arrive before signal `2`.

Why is this important? Consider the request-response idiom:

```erlang
%% Send a monitor signal to `Pid`, requesting a 'DOWN' message
%% when `Pid` dies.
Mref = monitor(process, Pid),
%% Send a message signal to `Pid` with our `Request`
Pid ! {self(), Mref, Request},
receive
    {Mref, Response} ->
        %% Send a demonitor signal to `Pid`, and remove the
        %% corresponding 'DOWN' message that might have
        %% arrived in the meantime.
        erlang:demonitor(Mref, [flush]),
        {ok, Response};
    {'DOWN', Mref, _, _, Reason} ->
        {error, Reason}
end
```

Since dead processes cannot send messages we know that the response must come
before any eventual `'DOWN'` message, but without a guaranteed order the
`'DOWN'` message could arrive before the response and we'd have no idea whether
a response was coming or not, which would be very annoying to deal with.

Having a defined order saves us quite a bit of hassle and doesn't come at much
of a cost, but the guarantees stop there. If more than one process sends
signals to a common process, they can arrive in any order even when you "know"
that one of the signals was sent first. For example, this sequence of events
is legal and entirely possible:

1. `A` sends signal `1` to `B`
1. `A` sends signal `2` to `C`
1. `C`, in response to signal `2`, sends signal `3` to `B`
1. `B` receives signal `3`
1. `B` receives signal `1`

Luckily, global orders are rarely needed and are easy to impose yourself
(outside distributed cases): just let all involved parties synchronize with a
common process.

### Sending messages

Sending a message is straightforward: we try to find the process associated
with the process identifier, and if one exists we insert the message into its
signal queue.

Messages are always copied before being inserted into the queue. As wasteful as
this may sound it greatly reduces garbage collection (GC) latency as the GC
never has to look beyond a single process. Non-copying implementations have
been tried in the past, but they turned out to be a bad fit as low latency is
more important than sheer throughput for the kind of soft-realtime systems that
Erlang is designed to build.

By default, messages are copied directly into the receiving process' heap but
when this isn't possible (or desired -- see the [message_queue_data] flag) we
allocate the message outside of the heap instead.

Memory allocation makes such "off-heap" messages slightly more expensive but
they're very neat for processes that receive a ton of messages. We don't need
to interact with the receiver when copying the message -- only when adding it
to the queue -- and since the only way a process can see a message is by
matching them in a `receive` expression, the GC doesn't need to consider
unmatched messages which further reduces latency.

Sending messages to processes on other Erlang nodes works in the same way,
albeit there's now a risk of messages being lost in transit. Messages are
guaranteed to be delivered as long as the distribution link between the nodes
is active, but it gets tricky when the link goes down.

Using `monitor/2` on the remote process (or node) will tell you when this
happens, acting as if the process died (with reason `noconnection`), but that
doesn't always help: the link could have died _after_ the message was received
and handled on the other end, all we know is that the link went down _before_
we got any eventual response.

As with everything else there's no free lunch, and you need to decide how your
applications should handle [these scenarios].

### Receiving messages

One might guess that processes receive messages through `receive` expressions,
but `receive` is a bit of a misnomer. As with all other signals the process
continuously handles them in the background, moving received messages from the
_signal_ queue to the _message_ queue.

`receive` searches for matching messages in the message queue (in the order
they arrived), or waits for new messages if none were found. Searching through
the message queue rather than the signal queue means it doesn't have to worry
about processes that send messages, which greatly increases performance.

This ability to "selectively receive" specific messages is very convenient:
we're not always in a context where we can decide what to do with a message and
having to manually lug around all unhandled messages is certainly annoying.

Unfortunately, sweeping the search under the rug doesn't make it go away:

```erlang
receive
    {reply, Result} ->
        {ok, Result}
end
```

The above expression finishes instantly if the next message in the queue
matches `{reply, Result}`, but if there's no matching message it has to walk
through them all before giving up. This is expensive when there are a lot of
messages queued up which is common for server-like processes, and since
`receive` expressions can match on just about anything there's little that can
be done to optimize the search itself.

The only optimization we do at the moment is to mark a starting point for the
search when we know that a message couldn't exist prior to a certain point.
Let's revisit the request-response idiom:

```erlang
Mref = monitor(process, Pid),
Pid ! {self(), Mref, Request},
receive
    {Mref, Response} ->
        erlang:demonitor(Mref, [flush]),
        {ok, Response};
    {'DOWN', Mref, _, _, Reason} ->
        {error, Reason}
end
```

Since the reference created by `monitor/2` is globally unique and cannot exist
before said call, and the `receive` only matches messages that contain said
reference, we don't need to look at any of the messages received before then.

This makes the idiom efficient even on processes that have absurdly long
message queues, but unfortunately it isn't something we can do in the general
case. While you as a programmer can be sure that a certain response must come
after its request even without a reference, for example by using your own
sequence numbers, the compiler can't read your intent and has to assume that
you want _any_ message that matches.

Figuring out whether the above optimization has kicked in is rather annoying at
the moment. It requires inspecting BEAM assembly and even then you're not
guaranteed that it will work due to some annoying limitations:

1. We only support one message position at a time: a function that creates a
   reference, calls another function that uses this optimization, and then
   returns to `receive` with the first reference, will end up searching through
   the entire message queue.
1. It only works within a single function clause: both reference creation and
   `receive` need to be next to each other and you can't have multiple
   functions calling a common `receive` helper.

We've addressed these shortcomings in the upcoming OTP 24 release, and have
added a compiler option to help you spot where it's applied:

```bash
$ erlc +recv_opt_info example.erl
```
<!-- ignore me -->
```erlang
-module(example).
-export([t/2]).

t(Pid, Request) ->
    %% example.erl:5: OPTIMIZED: reference used to mark a 
    %%                           message queue position
    Mref = monitor(process, Pid),
    Pid ! {self(), Mref, Request},
    %% example.erl:7: INFO: passing reference created by
    %%                      monitor/2 at example.erl:5
    await_result(Mref).

await_result(Mref) ->
    %% example.erl:10: OPTIMIZED: all clauses match reference
    %%                            in function parameter 1
    receive
        {Mref, Response} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Response};
        {'DOWN', Mref, _, _, Reason} ->
            {error, Reason}
    end.
```

[defined order]: https://erlang.org/doc/apps/erts/communication.html#passing-of-signals
[message_queue_data]: https://erlang.org/doc/man/erlang.html#process_flag_message_queue_data
[these scenarios]: https://en.wikipedia.org/wiki/Network_partition
