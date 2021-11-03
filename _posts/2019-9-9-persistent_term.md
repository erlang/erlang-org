---
layout: post
title: Clever use of persistent_term
tags: persistent_term literal
author: Lukas Larsson
---

This blog post will go through three different uses of [persistent_term](/doc/man/persistent_term.html)
that I have used since its release and explain a bit why they work so well with
[persistent\_term](/doc/man/persistent_term.html).

# Global counters

Let's say you want to have some global counters in your system. For example the number
of times an http request has been made. If the system is very busy that counter
will be incremented many many times per second by many different processes. Before
OTP-22 the best way that I know of to get the best performance is by using a striped
ets tables. i.e. something like the code below:

{% raw %}
```erlang
incr(Counter) ->
  ets:update_counter(?MODULE,{Counter,erlang:system_info(scheduler_id)},1).

read(Counter) ->
  lists:sum(ets:select(?MODULE,[{{{Counter,'_'},'$1'},[],['$1']}])).
```
{% endraw %}

The code above would make sure that there is very little contention on the ets table
as each scheduler will get a separate slot in the table to update. This comes at the
cost of more memory usage and that when reading the value you may not get an exact
value.

In OTP-22 the same can be achieved by using [counters](/doc/man/counters.html).
[Counters](/doc/man/counters.html) have built-in
support for striping by using the `write_concurrency` option, so we don't have
to write our own implementation for that. They are also faster and use less memory
than ets tables, so lots of wins.

The remaining problem then is finding the reference to the counter. We could put it
into ets and then do an [ets:lookup\_element/3](/doc/man/ets.html#lookup_element-3)
when updating a counter.

```erlang
cnt_incr(Counter) ->
    counters:add(ets:lookup_element(?MODULE,Counter,2),1,1).

cnt_read(Counter) ->
    counters:get(ets:lookup_element(?MODULE,Counter,2),1).
```

This gives a performance degradation of about 20%, so not really what we want.
However, if we place the counter in [persistent\_term](/doc/man/persistent_term.html)
like the code below we get a performance increase by about 140%, which is much
more in line with what we wanted.

```erlang
cnt_pt_incr(Counter) ->
    counters:add(persistent_term:get({?MODULE,Counter}),1,1).

cnt_pt_read(Counter) ->
    counters:get(persistent_term:get({?MODULE,Counter}),1).
```

The reason for this huge difference is because when the [counters](/doc/man/counters.html)
are placed into [persistent\_term](/doc/man/persistent_term.html)
they are placed there as literals which means that at each increment we not longer
have to make a copy of the [counters](/doc/man/counters.html) reference.
This is good for two reasons:

1) The amount of garbage will decrease. In my benchmarks the amount of garbage generated
by `cnt_incr` is 6 words while both `ets_incr` and `cnt_pt_incr` create 3 words.

2) No reference counts have to be modified. What I mean by this is that the
[counters](/doc/man/counters.html) reference
is what is called a magic reference or nif resource. These references work much in the same
way as reference counted binaries in that they are not copied when sent to different
processes. Instead only a reference count is incremented at copy and then decremented later
by the GC. This means that for `cnt_incr` we actually have 3 counters that are modified for
each call. First we increment the reference count on the counter when copying from ets, then
we update the actual counter and then eventually we decrement the reference counter. If we
use [persistent\_term](/doc/man/persistent_term.html), the term is never
copied so we don't have to update any reference counters, instead we just have to update the
actual counter.

However, placing the counter in [persistent\_term](/doc/man/persistent_term.html)
is not trouble free. In order to delete or replace the counter reference in
[persistent\_term](/doc/man/persistent_term.html) we have to do a global
GC which depending on the system could be very very expensive.

So this method is best to only be used by global persistent counters that will never be deleted.

You can find the code for all the above examples and the benchmark I ran
[here](https://gist.github.com/garazdawi/17cdb5914b950f0acae21d9fcf7e8d41).

# Logger level check

In [logger](/doc/man/logger.html) there is a primary logging
level that is the first test to be done for each potential log message to be generated.
This check can be done many times per second and needs to be very quick. At the
moment of writing (OTP-22) logger uses an ets table to keep all its configuration which
includes the primary logging level.

This is not really ideal as doing a lookup from the ets table means that we have to take
a read-lock to protect against parallel writes to the value. Taking such a read lock is not
terribly expensive, but when done thousands of times per second it adds up.

So in [this PR](https://github.com/erlang/otp/pull/2356) I've used
[persistent\_term](/doc/man/persistent_term.html) as
a cache for the primary logging level. Now when reading the value from the hot path
logger will instead use [persistent\_term](/doc/man/persistent_term.html).
This removes all locks from the hot path and we only need to do a lookup in the
[persistent\_term](/doc/man/persistent_term.html) hash table.

But what if we need to update the primary logger level? Don't we force a global GC then?
No, because the small integer representing the primary logger level is an immediate.
This means that the value fits in one machine word and is always copied in its
entirety to the calling process. Which in turn means that we don't have to do a global
GC when replacing the value.

When doing this we have to be very careful so that the value does not become a heap value
as the cost of doing an update would explode. However, it works great for logger and
has reduced the overhead of a ?LOG_INFO call by about 65% when no logging should be done.

# Large constant data

We use an internal tool here at the OTP-team called the "ticket tool". It basically
manages all of the OTP-XYZ tickets that you see in the release notes that comes with
each release of Erlang/OTP. It is an ancient tool from late 90's or early 00's that
no one really wants to touch.

One part of it is a server that contains a cache of all the 17000 or so tickets that
have been created through the years. In that server there is a single process that
has each ticket and its state in order to speed up searching in the tickets. The state
of this process is quite large and when it is doing a GC it takes somewhere around 10
seconds for it to finish. This means that about every 10 minutes the server freezes for
10 seconds and we get to experience the joy of being Java programmers for a while.

Being a VM developer I've always thought the solution to this problem is to implement
either an incremental GC or at least a mark and sweep GC for large heaps. However, the
ticket tool server has never been of high enough priority to make me spend a year or two
rewriting the GC.

So, two weeks ago I decided to take a look and instead I used
[persistent\_term](/doc/man/persistent_term.html)
to move the data from the heap into the literal area instead. This was possible to do because
I know that the majority of tickets are only searched and never changed, so they will
remain in the literal area forever, while the tickets that do get edited move onto the
heap of the ticket server. Basically my code change was this:

```erlang
handle_info(timeout, State) ->
  persistent_term:put(?MODULE,State),
  erlang:start_timer(60 * 60 * 1000, self(), timeout),
  {noreply,persistent_term:get(?MODULE)}.
```

This small change puts the entire gen_server state into the literal area and then
any changes done to it will pull the data into the heap. This dropped the GC pauses
down to be non-noticeable and took considerable less time to implement than a new GC
algorithm.
