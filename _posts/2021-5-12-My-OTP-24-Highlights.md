---
layout: post
title: Erlang/OTP 24 Highlights
tags: erlang otp 24 release
author: Lukas Larsson
---

Finally Erlang/OTP 24 is here! A release that for me has been [about 10 years]
in the making. [As] is [tradition] by [now], this blog post will go through the
additions to Erlang/OTP that I am most excited about!

Erlang/OTP 24 includes contributions from 60+ external contributors totalling
1400+ commits, 300+ PRs and changing 0.5 million(!) lines of code. Though I'm not
sure the line number should count as we vendored all of [AsmJit] and
re-generated the wxWidgets support. If we ignore AsmJit and wx, there are still
260k lines of code added and 320k lines removed, which is about 100k more than
what our releases normally contain.

You can download the readme describing the changes here: [Erlang/OTP 24 Readme].
Or, as always, look at the release notes of the application you are interested in.
For instance here: [Erlang/OTP 24 - Erts Release Notes - Version 12.0].

This years highlights are:
* [BeamAsm - the JIT compiler for Erlang](#beamasm---the-jit-compiler-for-erlang)
* [Improved error messages](#improved-error-messages)
* [Improved receive optimizations](#improved-receive-optimizations)
* [EEP-53: Process aliases](#eep-53-process-aliases)
* [EEP-48: Documentation chunks for edoc](#eep-48-documentation-chunks-for-edoc)
* [socket support in gen_tcp](#socket-support-in-gen_tcp)
* [EEP-56: Supervisor automatic shutdown](#EEP-56-supervisor-automatic-shutdown)
* [Edwards-curve Digital Signature Algorithm](#edwards-curve-digital-signature-algorithm)

[about 10 years]: https://vimeo.com/44231138
[Erlang/OTP 24 Readme]: http://erlang.org/download/otp_src_24.0.readme
[Erlang/OTP 24 - Erts Release Notes - Version 12.0]: http://erlang.org/doc/apps/erts/notes.html#erts-12.0
[AsmJit]: https://asmjit.com/
[As]: https://blog.erlang.org/My-OTP-21-Highlights/
[tradition]: https://blog.erlang.org/OTP-22-Highlights/
[now]: https://blog.erlang.org/OTP-23-Highlights/

# BeamAsm - the JIT compiler for Erlang #

The most anticipated feature of Erlang/OTP 24 has to be the JIT compiler.
A lot has already been said about it:

* [Initial PR](https://github.com/erlang/otp/pull/2745)
* [A first look at the JIT](https://blog.erlang.org/a-first-look-at-the-jit/)
* [Further adventures in the JIT](https://blog.erlang.org/jit-part-2/)
* [The Road to the JIT](https://blog.erlang.org/the-road-to-the-jit/)
* [BeamAsm, the Erlang JIT](http://erlang.org/doc/apps/erts/BeamAsm.html)

and even before released the WhatsApp team has [shown what it is capable of].

However, besides the performance gains that the JIT brings, what I am the most
excited about is the benefits that come with running native code
instead of interpreting. What I'm talking about is the native code tooling that
now becomes available to all Erlang programmers, such as integration with [perf].

As an example, when building a dialyzer plt of a small core of Erlang, the
previous way to profile would be via something like [eprof].

```erlang
> eprof:profile(fun() ->
    dialyzer:run([{analysis_type,'plt_build'},{apps,[erts]}])
  end).
```

This increases the time to build the PLT from about 1.2 seconds to 15 seconds on
my system. In the end, you get something like the below that will guide you to
what you need to optimize. Maybe take a look at `erl_types:t_has_var*/1`
and check if you really need to call it 13-15 million times!

```
> eprof:analyze(total).
FUNCTION                      CALLS        %     TIME [uS / CALLS]
--------                      -----  -------     ---- [----------]
erl_types:t_sup1/2          2744805     1.68   752795 [      0.27]
erl_types:t_subst/2         2803211     1.92   858180 [      0.31]
erl_types:t_limit_k/2       3783173     2.04   913217 [      0.24]
maps:find/2                 4798032     2.14   957223 [      0.20]
erl_types:t_has_var/1      15943238     5.89  2634428 [      0.17]
erl_types:t_has_var_list/1 13736485     7.51  3360309 [      0.24]
------------------------  ---------  ------- -------- [----------]
Total:                    174708211  100.00% 44719837 [      0.26]
```

In Erlang/OTP 24 we can get the same result without having to pay the pretty
steep cost of profiling with eprof. When running the same analysis as above
using [perf] it takes roughly 1.3 seconds to run.

```sh
$ ERL_FLAGS="+JPperf true" perf record dialyzer --build_plt \
    --apps erts
```

Then we can use tools such as [perf report], [hotspot] or [speedscope] to
analyze the results.

```sh
$ hotspot perf.data
```

![alt text](/images/hotspot-dialyzer.png "Hotspot dialyzer")

In the above, we can see that we get roughly the same result as when using
`eprof`, though interestingly not exactly the same. I'll leave the whys of
this up to the reader to find out :)

With this little overhead when profiling, we can run scenarios that previously
would take too long to run when profiling. For those brave enough it might even
be possible to run always-on profiling in production!

The journey with what can be done with [perf] has only started. In [PR-4676] we
will be adding frame pointer support which will give a much more accurate call
frames when profiling and, in the end, the goal is to have mappings to Erlang
source code lines instead of only functions when using [perf report] and
[hotspot] to analyze a perf recording.

[shown what it is capable of]: https://twitter.com/garazdawi/status/1385263924803735556
[perf]: https://perf.wiki.kernel.org/index.php/Main_Page
[gdb]: https://www.gnu.org/software/gdb/
[eprof]: https://erlang.org/doc/man/eprof.html
[perf report]: https://man7.org/linux/man-pages/man1/perf-report.1.html
[hotspot]: https://github.com/KDAB/hotspot
[speedscope]: https://twitter.com/michalslaski/status/1391381431335669765
[PR-4676]: https://github.com/erlang/otp/pull/4676

# Improved error messages #

Erlang's error messages tend to get a lot of (valid) critisism for being hard to
understand. Two great new features have been added to help the user understand
why something has failed.

## Column number in warnings and errors ##

Thanks to the work of [Richard Carlsson] and [Hans Bolinder], when you compile
Erlang code you now get the line and column of errors and warnings printed in
the shell together with a `^`-sign showing exactly where the error
actually was. For example, if you compile the below:

```erlang
foo(A, B) ->
  #{ a => A, b := B }.
```

you would in Erlang/OTP 23 and earlier get:

```
$ erlc t.erl
t.erl:6: only association operators '=>' are allowed in map construction
```

but in Erlang/OTP 24 you now also get the following printout:

```
$ erlc test.erl
t.erl:6:16: only association operators '=>' are allowed in map construction
%    6|   #{ a => A, b := B }.
%     |                ^
```

This behavior also extends into most of the Erlang code editors so that
when you use VSCode or Emacs through [Erlang LS] or [flycheck] you also
get a narrower warning/error indicator, for example in Emacs using [Erlang LS].

![alt text](/images/column-numbers-highlight.png "Emacs columns numbers with Erlang-LS")

[Richard Carlsson]: https://github.com/richcarl
[Hans Bolinder]: https://github.com/uabboli
[flycheck]: https://www.flycheck.org/

## EEP-54: Improved BIF error information ##

One of the other big changes when it comes to error information is the
introduction of [EEP-54]. In the past many of the [BIFs] (built-in functions)
would give very cryptic error messages:

```erlang
1> element({a,b,c}, 1).
** exception error: bad argument
     in function  element/2
        called as element({a,b,c},1)
```

In the example above, the only thing we know is that one or more of the
arguments are invalid, but without checking
[the documentation](https://erlang.org/doc/man/erlang.html#element-2)
there is no way of knowing which one and why. This is especially a problem for
BIFs where the arguments may fail for different reasons depending on factors not
visible in the arguments. For example in the `ets:update_counter` call below:

```erlang
> ets:update_counter(table, k, 1).
** exception error: bad argument
     in function  ets:update_counter/3
        called as ets:update_counter(table,k,1)
```

We don't know if the call failed because the table did not exist at all
or if the key `k` that we wanted to update did not exist in the table.

In Erlang/OTP 24 both of the examples above will have a much clearer error
messages.

```erlang
1> element({a,b,c}, 1).
** exception error: bad argument
     in function  element/2
        called as element({a,b,c},1)
        *** argument 1: not an integer
        *** argument 2: not a tuple
2> ets:new(table,[named_table]).
table
3> ets:update_counter(table, k, 1).
** exception error: bad argument
     in function  ets:update_counter/3
        called as ets:update_counter(table,k,1)
        *** argument 2: not a key that exists in the table
```

That looks much better and now we can see what the problem was!
The standard logging formatters also include the additional information
so that if this type of error happens in a production environment you will
get the extra error information:

```
1> proc_lib:spawn(fun() -> ets:update_counter(table, k, 1) end).
<0.94.0>
=CRASH REPORT==== 10-May-2021::11:20:35.367023 ===
  crasher:
    initial call: erl_eval:'-expr/5-fun-3-'/0
    pid: <0.94.0>
    registered_name: []
    exception error: bad argument
      in function  ets:update_counter/3
         called as ets:update_counter(table,k,1)
         *** argument 1: the table identifier does
                         not refer to an existing ETS table
    ancestors: [<0.92.0>]
```

[EEP-54] is not only useful for error messages coming from BIFs but can be used
by any application that wants to provide extra information about their exceptions.
For example, we have been working on providing better error information around
`io:format` in [PR-4757](https://github.com/erlang/otp/pull/4757).

[EEP-54]: https://www.erlang.org/erlang-enhancement-proposals/eep-0054.html
[BIFs]: https://erlang.org/doc/reference_manual/functions.html#built-in-functions--bifs-

# Improved receive optimizations #

Since Erlang/OTP R14 (released in 2010), the Erlang compiler and run-time system
have co-operated to optimize for the pattern of code used by
`gen_server:call` like functionality to avoid scanning a potentially
huge mailbox. The basic pattern looks like this:

```erlang
call(To, Msg) ->
  Ref = make_ref(),
  To ! {call, Ref, self(), Msg},
  receive
    {reply, Ref, Reply} -> Reply
  end.
```

The compiler can from this figure out that when `Ref` is created, there can be
no messages in the mailbox of the process that contains `Ref` and therefore it
can skip all of those when receiving the `Reply`.

This has always worked great in simple scenarios like this, but as soon as you
had to make the scenarios a little more complex it tended to break the
compiler's analysis and you would end up scanning the entire mailbox. For example,
in the code below Erlang/OTP 23 will not optimize the receive.

```erlang
call(To, Msg, Async) ->
  Ref = make_ref(),
  To ! {call, Ref, self(), Msg},
  if
    Async ->
      {ok, Ref};
    not Async ->
      receive
        {reply, Ref, Reply} -> Reply
      end
  end.
```

That all changes with Erlang/OTP 24! Many more complex scenarios are now
covered by the optimization and a new compiler flag has been added to tell the
user if an optimization is done.

```sh
$ erlc +recv_opt_info test.erl
test.erl:6: Warning: OPTIMIZED: reference used to mark
                                a message queue position
%    6|   Ref = make_ref(),
test.erl:12: Warning: OPTIMIZED: all clauses match reference
                                 created by make_ref/0
                                 at test.erl:6
%   12|       receive
```

Even patterns such as multi_call are now optimized to not scan the mailbox of
the process.

```erlang
multi_call(ToList, Msg) ->
  %% OPTIMIZED: reference used to mark a message queue position
  Ref = make_ref(),
  %% INFO: passing reference created by make_ref/0 at test.erl:18
  [To ! {call, Ref, self(), Msg} || To <- ToList],
  %% INFO: passing reference created by make_ref/0 at test.erl:18
  %% OPTIMIZED: all clauses match reference
  %%            in function parameter 2
  [receive {reply, Ref, Reply} -> Reply end || _ <- ToList].
```

There are still a lot of places where this optimization does not trigger. For
instance as soon as any of the make_ref/send/receive are in different modules it
will not work. However, the new improvements in Erlang/OTP 24 make the number of
scenarios a lot fewer and now we also have the tools to check and see if the
optimization is triggered!

You can read more about this optimization and others in the [Efficiency Guide].

[Efficiency Guide]: https://erlang.org/doc/efficiency_guide/processes.html#process-messages

# EEP-53: Process aliases #

When doing a call to another Erlang process, the pattern used by
`gen_server:call`, `gen_statem:call` and others normally looks something
like this:

```erlang
call(To, Msg, Tmo) ->
  MonRef = erlang:monitor(process, To),
  To ! {call, MonRef, self(), Msg},
  receive
    {'DOWN',MonRef,_,_,Reason} ->
      {error, Reason};
    {reply, MonRef, Reply}
      erlang:demonitor(MonRef,[flush]),
      {ok, Reply}
    after Tmo ->
      erlang:demonitor(MonRef,[flush]),
      {error, timeout}
  end.
```

This normally works well except for when a timeout happens. When a timeout
happens the process on the other end has no way to know that the reply is no
longer needed and so will send it anyway when it is done with it. This causes
all kinds of problems as the user of a third-party library would never know what
messages to expect to be present in the mailbox.

There have been numerous attempts to solve this problem using the primitives
that Erlang gives you, but in the end, most ended up just adding a `handle_info`
in their `gen_server`s that ignored any unknown messages.

In Erlang/OTP 24, [EEP-53] has introduced the `alias` functionality to solve this problem.
An `alias` is a temporary reference to a process that can be used
to send messages to. In most respects, it works just as a PID except that
the lifetime of an alias is not tied with the lifetime of the process it
represents. So when you try to send a late reply to an alias that has been
deactivated the message will just be dropped.

The code changes needed to make this happen are very small and are already used
behind the scenes in all the standard behaviors of Erlang/OTP. The only thing
needed to be changed in the example code above is that a new option must be
given to `erlang:monitor` and the reply reference should now be the alias
instead of the calling PID. That is, like this:

```erlang
call(To, Msg, Tmo) ->
  MonAlias = erlang:monitor(process, To, [{alias, demonitor}]),
  To ! {call, MonAlias, MonAlias, Msg},
  receive
    {'DOWN', MonAlias, _ , _, Reason} ->
      {error, Reason};
    {reply, MonAlias, Reply}
      erlang:demonitor(MonAlias,[flush]),
      {ok, Reply}
    after Tmo ->
      erlang:demonitor(MonAlias,[flush]),
      {error, timeout}
  end.
```

You can read more about this functionality in the [alias documentation].

[EEP-53]: https://www.erlang.org/erlang-enhancement-proposals/eep-0053.html
[alias documentation]: https://erlang.org/doc/reference_manual/processes.html#process-aliases

# EEP-48: Documentation chunks for edoc #

In Erlang/OTP 23 [erl_docgen] was extended to be able to emit [EEP-48] style
documentation. This allowed the documentation to be used by `h(lists)` in
the Erlang shell and external tools such as [Erlang LS]. However, there
are very few applications outside Erlang/OTP that use `erl_docgen` to
create documentation, so [EEP-48] style documentation was unavailable to
those applications. Until now!

[Radek Szymczyszyn] has [added] support for [EEP-48] into [edoc] which means
that from Erlang/OTP 24 you can view both the documentation of `lists:foldl/3`
and `recon:info/1`.

```
$ rebar3 as docs shell
Erlang/OTP 24 [erts-12.0] [source] [jit]

Eshell V11.2.1  (abort with ^G)
1> h(recon,info,1).
 -spec info(PidTerm) ->
   [{info_type(), [{info_key(), Value}]}, ...]
     when PidTerm :: pid_term().

  Allows to be similar to erlang:process_info/1, but excludes
  fields such as the mailbox, which tend to grow
  and be unsafe when called in production systems. Also includes
  a few more fields than what is usually given (monitors,
  monitored_by, etc.), and separates the fields in a more
  readable format based on the type of information contained.
```

For more information about how to enable this in your project see
the [Doc chunks section in the Edoc User's Guide].

[erl_docgen]: http://erlang.org/doc/man/erl_docgen_app.html
[EEP-48]: https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html
[Erlang LS]: https://erlang-ls.github.io/
[Radek Szymczyszyn]: https://github.com/erszcz
[edoc]: https://erlang.org/doc/man/edoc.html
[added]: https://github.com/erlang/otp/pull/2803
[Doc chunks section in the Edoc User's Guide]: https://erlang.org/doc/apps/edoc/chapter.html#doc-chunks

# `socket` support in `gen_tcp` #

The [gen_tcp] module has gotten support for optionally using the new [socket]
nif API instead of the previous inet driver. The new interface can be configured
to be used either on a system level through setting the application
configuration parameter like this: `-kernel inet_backend socket`, or on a per
connection bases like this: `gen_tcp:connect(localhost,8080,[{inet_backend,socket}])`.

If you do this you will notice that the `Socket` returned by `gen_tcp` no longer
is a port but instead of a tuple containing (among other things) a PID and a
reference.

```erlang
1> gen_tcp:connect(localhost,8080,[{inet_backend,socket}]).
{ok,{'$inet',gen_tcp_socket,
             {<0.88.0>,{'$socket',#Ref<0.2959644163.2576220161.68602>}}}}
```

This data structure is and always has been [opaque], and therefore should not be inspected
directly but instead only used as an argument to other [gen_tcp] and [inet]
functions.

You can then use [inet:i/0] to get a listing of all open sockets in the system:

```erlang
2> inet:i().
Port      Module         Recv Sent Owner    Local Address   Foreign Address    State Type   
esock[19] gen_tcp_socket 0    0    <0.98.0> localhost:44082 localhost:http-alt CD:SD STREAM 
```

The [gen_tcp] API should be completely backward compatible with the old
implementation, so if you can, please test it and report any bugs that you find
back to us.

Why should you want to test this? Because in some of our benchmarks, we get up
to 4 times the throughput vs the old implementation. In others,
there is no difference or even a loss of throughput. So, as always, you need to
measure and check for yourself!

[gen_tcp]: https://erlang.org/doc/man/gen_tcp.html
[inet]: https://erlang.org/doc/man/inet.html
[inet:i/0]: https://erlang.org/doc/man/inet.html#i-0
[socket]: https://erlang.org/doc/man/socket.html
[opaque]: http://erlang.org/doc/reference_manual/typespec.html#type-declarations-of-user-defined-types

# EEP-56: Supervisor automatic shutdown #

When creating supervisor hierarchies for applications that manage connections
such as [ssl] or [ssh], there are times when there is a need for terminating
that supervisor hierarchy from within. Some event happens on the socket that
should trigger a graceful shutdown of the processes associated with the
connection.

Normally this would be done by using [supervisor:terminate_child/2]. However,
this has two problems.

1. It requires the child to know the ID of the child that needs to be terminated
   and the PID of the supervisor to talk to. This is simple when there is just
   one process in the supervisor, but when there are supervisors under
   supervisors, this becomes harder and harder to figure out.
2. Calling [supervisor:terminate_child/2] is a synchronous operation. This means
   that if you do the call in the child, you may end up in a deadlock as the top
   supervisor wants to terminate the child while the child is blocking in the call
   to terminate itself.

To solve this problem [EEP-56] has added a mechanism in which a child can be
marked as significant and if such a child terminates, it can trigger an automatic
shutdown of the supervisor that it is part of.

This way a child process can trigger the shutdown of a supervisor hierarchy from
within, without the child having to know anything about the supervisor hierarchy
nor risking dead-locking itself during termination.

You can read more about automatic shutdown in the [supervisor documentation].

[ssl]: https://erlang.org/doc/man/ssl.html
[ssh]: https://erlang.org/doc/man/ssh.html
[EEP-56]: https://www.erlang.org/erlang-enhancement-proposals/eep-0056.html
[supervisor:terminate_child/2]: https://erlang.org/doc/man/supervisor.html#terminate_child-2
[supervisor documentation]: https://erlang.org/doc/man/supervisor.html#auto_shutdown


# Edwards-curve Digital Signature Algorithm #

With Erlang/OTP 24 comes support for [Edwards-curve Digital Signature Algorithm]
(`EdDSA`). `EdDSA` can be used when connecting to or acting as a TLS 1.3
client/server.

`EdDSA` is a type of [elliptic curve signature algorithm] (`ECDSA`)
that can be used for secure communication. The security of `ECDSA` relies on a
[strong cryptographically secure random number] which can cause issues when
the random number is by mistake not secure enough, as has been the case in several
uses of ECDSA (none of them in Erlang as far as we know :).

`EdDSA` does not rely on a strong random number to be secure. This means that
when you are using `EdDSA`, the communication is secure even if your random
number generator is not.

Despite the added security, `EdDSA` is claimed to be faster than other eliptic
curve signature algorithms. If you have [OpenSSL] 1.1.1 or later, then as of
Erlang/OTP 24 you will have access to this algorithm!

```erlang
> crypto:supports(curves).
[...
  c2tnb359v1, c2tnb431r1, ed25519, ed448, ipsec3, ipsec4
 ...]                     ^        ^
```

[Edwards-curve Digital Signature Algorithm]: https://datatracker.ietf.org/doc/html/rfc8032
[OpenSSL]: https://www.openssl.org/
[strong cryptographically secure random number]: http://erlang.org/doc/man/crypto.html#strong_rand_bytes-1
[elliptic curve signature algorithm]: https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm
