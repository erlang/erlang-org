---
layout: post
title: Erlang/OTP 28 Highlights
tags: erlang otp 28 release
author: Isabell Huang
---

Erlang/OTP 28 is finally here. This blog post will introduce the new
features that we are most excited about.

A list of all changes is found in [Erlang/OTP 28 Readme](https://erlang.org/patches/OTP-28.0).
Or, as always, look at the release notes of the application you are interested in.
For instance:
[Erlang/OTP 28 - Erts Release Notes - Version 16.0](https://www.erlang.org/doc/apps/erts/notes.html#erts-16.0).

This year's highlights mentioned in this blog post are:

* [Priority Messages](#priority-messages)
* [Improvements of Comprehensions](#improvements-of-comprehensions)
* [Smarter Error Suggestions](#smarter-error-suggestions)
* [Improvements to the Shell](#improvements-to-the-shell)
* [New `erlang:hibernate/0`](#new-erlanghibernate0)
* [Warnings for Use of Old-style Catch](#warnings-for-use-of-old-style-catch)
* [PCRE2](#pcre2)
* [Optimizations to TLS 1.3](#optimizations-to-tls-13)
* [Based Floating Point Literals](#based-floating-point-literals)
* [Nominal Types](#nominal-types)
* [New Emacs Erlang Mode](#new-emacs-erlang-mode)


# Priority Messages

Sometimes, it is important for urgent messages to _skip the queue_ and
be read by the receiving process as soon as possible. Erlang/OTP 28 introduces
priority messages, an opt-in mechanism that allows the receiving process
to let certain messages get priority status.

By default, all messages are inserted to the end of the message queue of
a process. This can become cumbersome when the queue is long. An urgent
message may need to be read as soon as possible.

For example, the current message overload protection mechanism for [`logger`](https://www.erlang.org/doc/apps/kernel/logger.html)
polls its message queue length in order to know when it should start shedding
messages. It would have benefitted from using the [`long_message_queue`](https://www.erlang.org/doc/apps/erts/erlang.html#system_monitor/2)
monitoring functionality introduced in Erlang/OTP 27, but the only way to
get information like that is via a message, which would be inserted at the
end of the very long queue.

Priority message solves this problem by letting selected messages be inserted
before all ordinary messages, but still in the order they are received.

A receiver process can allow other processes to send priority message
to itself in two simple steps. The first step is to create a process alias
using [`alias/1`](https://erlang.org/doc/apps/erts/erlang.html#alias/1):

````erlang
PrioAlias = alias([priority])
````

This alias can then be distributed to other processes that should be able
to send priority messages to the receiver process. A sender process can
send a priority message by using [`erlang:send/3`](https://erlang.org/doc/apps/erts/erlang.html#send/3),
passing the `PrioAlias` as the first argument, and the option `priority` in
the option list as the third argument:

````erlang
erlang:send(PrioAlias, Message, [priority])
````

In this way, messages sent to a priority alias with the `priority` flag will
be inserted before ordinary messages in the message queue. Other processes
can still send ordinary messages to the priority alias by not using the
`priority` flag. If a message is sent to the priority alias without using
the `priority` flag, it will be treated as an ordinary message.

It is also possible to send an exit signal as a priority signal, like this:

````erlang
exit(PrioAlias, Message, [priority])
````

If the receiver process wants to stop receiving priority messages, it
can do so by deactivating its priority alias:

````erlang
true = unalias(PrioAlias)
````

After this, no priority message can be sent to the receiver process, because
the priority alias is no longer active. The receiver process can activate
and deactivate its priority alias again at any time.

Priority message reception can also be enabled for exit signals due to broken
links and messages triggered due to monitors. Since these signals are not
sent when a process calls a specific function for sending a signal, but
when specific events occur in the system, a priority alias cannot be used
for this. In order to enable such priority messages, you can pass the
`priority` option to either [`erlang:monitor/3`](https://erlang.org/doc/apps/erts/erlang.html#monitor/3)
or [`erlang:link/2`](https://erlang.org/doc/apps/erts/erlang.html#link/2).

Priority messages respect Erlang's existing guarantee: Signals still arrive
in the same order as they are sent, if they arrive at all. This change
only affects where messages are inserted in the queue. Performance-wise,
this feature preserves Erlang's selective receive optimization. There is
no performance penalty for ordinary messages or priority messages.

For more details, see [the documentation of *priority messages*](https://www.erlang.org/doc/system/ref_man_processes.html#priority-messages)
and [EEP-76](https://www.erlang.org/eeps/eep-0076). 


# Improvements of Comprehensions

Erlang/OTP 28 introduces many useful updates in its comprehensions. All
of them are new language features that have been suggested as [EEPs](https://www.erlang.org/eeps).
Between the release of Erlang/OTP 27 and 28, there were 4 accepted EEPs related
to comprehensions. Features described by two of them are included in Erlang/OTP
28. The other two are postponed to a later release. The [documentation for
comprehensions](https://www.erlang.org/doc/system/expressions.html#comprehensions)
contains an up-to-date overview of all relevant features.

## Strict Generators

Strict generator as described in [EEP 70](https://www.erlang.org/eeps/eep-0070)
aims to improve expressiveness and safety for comprehensions.

In OTP 27 and earlier, when the right-hand side expression does not match
the left-hand side pattern in a comprehension generator, the term is ignored
and the evaluation continues on. In the following example, the element
`error` is silently skipped in the comprehension.

````erlang
1> [X ||{ok, X} <- [{ok, 1}, error, {ok, 3}]].
[1,3]
````

This behavior can hide the presence of unexpected elements in the input
data. In the example above, what if the list should not contain anything
other than 2-tuples with the first element being `ok`? By using a strict
generator, the comprehension crashes when the pattern-matching fails with
the element `error`.

````erlang
2> [X ||{ok, X} <:- [{ok, 1}, error, {ok, 3}]].
** exception error: no match of right hand side value error
````

Strict generators can be used in list generators (`<:-`), binary generators
(`<:=`), and map generators (`<:-`). In contrast, the previously existing
generators are called _relaxed_ generators.

Strict generators and relaxed generators can convey different intentions from
the programmer. The following example is rewritten from a comprehension in
the Erlang linter. It finds all nifs from an abstract form, and output them.
Obviously, not all forms are nifs. We want to ignore all forms that are not
nifs here. Using a relaxed generator here is correct.

````erlang
Nifs = [Args || {attribute, _Anno, nifs, Args} <- Forms].
````

More examples about strict and relaxed generators can be found in
[List Comprehensions](https://www.erlang.org/doc/system/list_comprehensions.html).

Sometimes, using either strict or relaxed generators is fine. When the
left-hand side pattern is a fresh variable, pattern matching cannot fail.
Using either leads to the same behavior. While the preference and use cases
might be individual, it is recommended to use strict generators when either
can be used. Using strict generators by default aligns with Erlang's "Let
it crash" philosophy.

Now you can pick a more fitting tool for the job, without losing the brevity
of comprehensions. It is also a good time to review old code, and see if
strict generators are more fitting in certain places. The compiler team in
OTP has done [that](https://github.com/erlang/otp/pull/9004). Take a look
if you are curious. 


## Zip Generators

Zip generators as described in [EEP 73](https://www.erlang.org/eeps/eep-0073)
makes it easier to iterate over multiple lists, binaries, or maps in parallel.

Erlang's list comprehension extract elements in a nested or cartesian way
by default:

````erlang
1> [{X, Y} || X <- [1, 2], Y <- [a, b]].
[{1,a},{1,b},{2,a},{2,b}]
````

Using zip generators `&&`, we can change the default behavior and "zip"
generators together as if using [`lists:zip/2`](https://erlang.org/doc/apps/stdlib/lists.html#zip/2):

````erlang
2> [{X, Y} || X <- [1, 2] && Y <- [a, b]].
[{1,a},{2,b}]
````

Zip generators can be used with lists, binaries, and maps, and can be
mixed freely with all existing generators and filters. Unlike [`lists:zip/2`](https://erlang.org/doc/apps/stdlib/lists.html#zip/2)
and [`lists:zip/3`](https://erlang.org/doc/apps/stdlib/lists.html#zip/3), you
can zip any number of generators together using `&&`s. The compiler avoids
creating intermediate tuples, yet preserving the same error behaviors as
these helper functions. 

# Smarter Error Suggestions

The Erlang/OTP 28 compiler has levelled up its ability in spotting typos.
Now it gives you suggestions on how to fix them, whenever possible.

For example, the following code exports an undefined function `bar/1`.

````erlang
-export([bar/1]).
baz(X) -> X.
````

The Erlang/OTP 27 compiler correctly points out the undefined function.

````erlang
t.erl:3:2: function bar/1 undefined
%   3| -export([bar/1]).
%    |  ^
````

The Erlang/OTP 28 compiler goes one step further. It suggests a possible
correction, according to all the defined functions in the module.

````erlang
t.erl:3:2: function bar/1 undefined, did you mean baz/1?
%   3| -export([bar/1]).
%    |  ^
````

This applies to common error types, like `undefined_nif`, `unbound_var`,
`undefined_function`, `undefined_record`, and so on.

It also works for wrong arity. If you call a function with the wrong number
of arguments, the compiler will suggest available arities, like the following:

````erlang
t.erl:6:12: function bar/2 undefined, did you mean bar/1,3,4?
````

This makes compilation errors easier to understand, and small mistakes
faster to fix. Try it out and you'll notice the change!

# Improvements to the Shell

Erlang/OTP 28 brings several improvements to the shell interface, making
it more flexible, interactive and powerful than before.

## Lazy Reads from `stdin`

Previously, Erlang's `stdin` greedily read all input data, which could cause
problems with special characters. This is changed by [PR-8962](https://github.com/erlang/otp/pull/8962).
Now in Erlang/OTP 28, all reads from `stdin` are done upon request, like
only when an [`io:get_line/2`](https://www.erlang.org/doc/apps/stdlib/io.html#get_line/2)
or equivalent is called. This removes the need to use the `-noinput` flag, and
 resolves issues like [Issue-8113](https://github.com/erlang/otp/issues/8113).

## Raw and Cooked Modes for `noshell`

The `noshell` mode now supports two "submodes":
- `cooked` is the default behavior, same as before.
- `raw` is the new option that can bypass the line editing support of the
native terminal.

In `raw` mode, you can build more interactive applications. It offers the
possibility to read keystrokes as they happen without the user typing Enter,
while disabling the line editing support and the echoing to `stdout`. The
following example is an escript that can read raw input (and immediately
prints it back out) without requiring the user to press Enter:

````erlang
#!/usr/bin/env escript
%% t.es

main(_Args) ->
    shell:start_interactive({noshell, raw}),
    io:format("Press any key, or press q to quit.\n"),
    loop().

loop() ->
    case io:get_chars("", 1024) of
        "q" ->
            io:format("Exit now.\n");
        Chars ->
            io:format("~p", [Chars]),
            loop();
        {error, Reason} ->
            io:format("Error reason: ~p~n", [Reason]),
            ok
    end.
````

With this, Erlang's shell becomes a platform for building interactive
terminal applications.
The [custom shell](https://www.erlang.org/doc/apps/stdlib/custom_shell.html)
documentation shows how to create a custom shell. The [terminal interface](https://www.erlang.org/doc/apps/stdlib/terminal_interface.html)
documentation shows how to implement a tic-tac-toe game.

Try it out. We look forward to see more interactive applications created
using this feature.

## Using `fun Name/Arity` to create funs in shell

Thanks to [PR-8987](https://github.com/erlang/otp/pull/8987), you can
now use `fun Name/Arity` to create funs in shell. The fun can be created
from an auto-imported BIF, such as [`is_atom/1`](https://www.erlang.org/doc/apps/erts/erlang.html#is_atom/1), as in the example below.

````erlang
1> F = fun is_atom/1.
fun erlang:is_atom/1
2> F(a).
true
3> F(42).
false
````

Or from a local function defined in shell, as in the following example.

````erlang
1> I = fun id/1.
#Fun<erl_eval.42.18682967>
2> I(42).
** exception error: undefined shell command id/1
3> id(I) -> I.
ok
4> I(42).
42
````


# New `erlang:hibernate/0`

Erlang/OTP 28 introduces a new [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0)
function. This built-in function puts the calling process into a wait state
where its memory footprint is reduced as much as possible. When the process
receives its next message, it will wake up. Unlike the existing [`erlang:hibernate/3`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/3),
it does not discard the call stack.

This makes [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0)
useful for processes that expect long idle time, but want to have a simpler
hibernation.

## Memory Usage Experiment

To demonstrate how efficient [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0)
is, we can make a benchmark that can spawn different number of processes
(starting from 1, going up to 1 million), let them either waiting for a
message using `receive` or using [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0),
and then compare memory usage.

Here's the test code for the first scenario, which uses [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0):

````erlang
-module(benchmark_hibernate).
-export([worker/0, spawn_all/1]).

worker() ->
    erlang:hibernate().

spawn_all(0) ->
    timer:sleep(1000),
    io:format("Memory usage: ~p~n", [erlang:memory()]),
    timer:sleep(1000),
    io:format("Memory usage after 1s: ~p~n", [erlang:memory()]),
    ok;
spawn_all(N) ->
    spawn(?MODULE, worker, []),
    spawn_all(N-1).
````

Here's the test code for the second scenario. Processes stay idle but they
do not hibernate:

````erlang
-module(benchmark_receive).
-export([worker2/0, spawn_all/1]).

worker2() ->
    receive
        _  -> ok
    end.

spawn_all(0) ->
    timer:sleep(1000),
    io:format("Memory usage: ~p~n", [erlang:memory()]),
    timer:sleep(1000),
    io:format("Memory usage after 1s: ~p~n", [erlang:memory()]),
    ok;
spawn_all(N) ->
    spawn(?MODULE, worker, []),
    spawn_all(N-1).
````

Memory usage is measured by [`erlang:memory()`](https://www.erlang.org/doc/apps/erts/erlang.html#memory/0)
after 1 million processes have been spawned. For the final result, we take
the average of two measurements.

We spawn 1, 10 thousand, 100 thousand, and 1 million processes for both
scenarios. Results are summarized in the following table:

|       | Number of Processes | Memory Used (Mb)
| ----------- | ----------- | ----------
| Hibernated      | 1       | 44.8
| Without `hibernate/0` | 1        | 47.0
| Hibernated      | 10,000       | 55.5
| Without `hibernate/0`   | 10,000        | 73.4
| Hibernated      | 100,000       | 130.3
| Without `hibernate/0`   | 100,000        | 307.1
| Hibernated      | 1,000,000       | 827.9
| Without `hibernate/0`   | 1,000,000        | 2687.1

When there is only 1 process, the memory usage reduction is not obvious
yet. When there are 1 million mostly idle processes, that's more than 75%
reduction in memory usage if you use [`erlang:hibernate/0`](https://www.erlang.org/doc/apps/erts/erlang.html#hibernate/0)!


# Warnings for Use of Old-Style Catch

Erlang/OTP 28 introduces a warning for using the old style `catch Expr`,
instead of `try ... catch ... end`.

The more simplistic `catch Expr` is problematic in that it catches
_all_ exceptions and can therefore hide bugs. For example, if the
intention is to catch exceptions raised by [`throw/1`](https://www.erlang.org/doc/apps/erts/erlang.html#throw/1),
the old-style `catch` will also catch runtime errors. Using its alternative
`try ... catch ... end` can offer better clarity.

In a future release, the use of the old `catch` construct will by
default result in compiler warnings. To facilitate removing usages of
the old-style `catch`, the compiler now has an option
`warn_deprecated_catch`. It can be enabled on the project level or the
module level to prevent new uses of the old-style catch.

If you have added `warn_deprecated_catch` at the project-level, the
warning can be suppressed in individual modules that have not yet been
updated by adding the `-compile(nowarn_deprecated_catch)` to them.

Here are some common uses of the old style `catch Expr`. We will show how
to replace them with `try ... catch ... end` and briefly explain why it is
a better solution.

_Example 1_: Using `catch Expr` to handle a possible `throw`

[`throw/1`](https://www.erlang.org/doc/apps/erts/erlang.html#throw/1) is
often used to quickly return from a deep recursion. If `tree_walker/1` is
a function that traverses a tree and sometimes throws a value, the old-style
catch could be used like this:

````erlang
Result = catch maybe_throw().
````

It can be refactored to the following code:

````erlang
Result = try tree_walker(Tree) of
             Value -> Value
         catch
             throw:Reason -> Reason
         end.
````

This is a bit longer, but it is also safer. For example, if the caller
of `tree_walker/1` passes in an invalid tree (such as `not_a_tree`),
the `try/catch` will not catch the resulting crash, allowing the
bug to be noticed and fixed early.

To have the same ensurance that crashes are not hidden when using the
old-style `catch`, you would have to write, which is as much code as the
new `try/catch`:

````erlang
Result = case catch tree_walker(Tree) of
            {'EXIT',Error} ->
                 error(Error);
            Value ->
                 Value
         end.
````

_Example 2_: Using `catch Expr` to match a specific error in a test case


````erlang
test_bad_argument(Term) ->
    {'EXIT',{badarg,_}} = catch list_to_atom(Term).
````

It can be refactored to the following code:

````erlang
test_bad_argument(Term) ->
    try list_to_atom(Term) of
        _Value -> error(not_supposed_to_succeed)
    catch
        error:badarg -> ok
    end.
````

An easier way is to include the following header file:

````erlang
-include_lib("stdlib/include/assert.hrl").
````

With that in place, you can simply write:

````erlang
test_bad_argument(Term) ->
    ?assertError(badarg, list_to_atom(Term)).
````

That will also result in more information being given if the test case
fails:

````erlang
1> t:test_bad_argument("ok").
** exception error: {assertException,[{module,t},
                                      {line,6},
                                      {expression,"list_to_atom ( Term )"},
                                      {pattern,"{ error , badarg , [...] }"},
                                      {unexpected_success,ok}]}
     in function  t:test_bad_argument/1 (t.erl:6)
````

It is likely that the compiler will start generate warnings for the
old-style `catch` in Erlang/OTP 29 or 30. If you are still using the
old style `catch Expr` in your code, now is a good time to start
refactoring.


# Based Floating Point Literals

Erlang/OTP 28 extends its floating point syntax to support floating point
literals using any bases, similar to Ada and C99/C++17. This is based on
[EEP-75](https://www.erlang.org/eeps/eep-0075).

In Erlang, you can already write integers in different bases:

````erlang
1> 2#100.
4
2> 3#100.
9
````

Now, you can do the same with floating point numbers:

````erlang
1> 2#0.011.
0.375
2> 3#0.011.
0.14814814814814814
3> 16#0.011#e5.
4352.0
````

Such an exact representation of floating point numbers is especially useful
in code generating tools. With only the base 10 representation, it is difficult
to convert floats from and to other bases without precision loss. With
based literals, you can even preserve bit level precision. For example,
`2#0.10101#e8` represents the exact layout of a binary float.


# PCRE2

Erlang/OTP 28 uplifts the [`re`](https://www.erlang.org/doc/apps/stdlib/re.html)
module to use PCRE2, instead of the PCRE library. This change is mostly
backward compatible with PCRE with respect to regular expression syntax, but
it also introduces some different behaviors.

The full documentation about breaking changes and incompatibilities can
be found in [PCRE2 Migration](https://www.erlang.org/doc/apps/stdlib/re_incompat.html).

## Why PCRE2 instead of PCRE?

PCRE2 is more in line with modern standards, especially Perl regex, which
is stricter about pattern syntax and catches invalid patterns early. This
makes your regex code safer, at the cost of breaking some old regex patterns.

## Notable Changes:

- Stricter Syntax Validation: For example, `\i`, `\B`, `\8` all result
in errors.

````erlang
% Erlang/OTP 27
1> re:run("AMM", ~S"\M").
{match,[{1,1}]}

% Erlang/OTP 28
1> re:run("AMM", ~S"\M").
** exception error: bad argument
     in function  re:run/2
        called as re:run("AMM",~S"\M")
        *** argument 2: could not parse regular expression
                        unrecognized character follows \ on character 1

````

- Unicode Property Updates: Characters matched by properties using `\p{...}`
may have changed, according to the updated Unicode character property data.

- [`re:split/3`](https://www.erlang.org/doc/apps/stdlib/re.html#split/3)
with Branch Reset Groups (`(?|...)`): The following example may evaluate to
`[[],"abc",[],[]]` in some interpretations of PCRE and Perl versions,
differing from PCRE2's result.

````erlang
1> re:split("abcabc", ~S"(?|(abc)|(xyz))\1", [{return, list}]).
[[],"abc",[]]
````

It is worth noting that the internal format produced by [`re:compile/2`](https://www.erlang.org/doc/apps/stdlib/re.html#compile/2)
has changed in Erlang/OTP 28. It cannot be reused across nodes or OTP versions.

This upgrade offers better long-term maintainability, but you may need to
test your existing regex code before upgrading.

# Optimizations to TLS 1.3

The performance of [SSL](https://www.erlang.org/doc/apps/ssl/ssl.html)
with TLS 1.3 has been optimized. The optimization reduces the general
overhead for application data transmission. To measure the improvement
from Erlang/OTP 27.1 to Erlang/OTP 28, we ran a small message echo benchmark
and measure the time for roundtrips.

Results are shown in the following table:

|       | Samples | Average | Std Dev | Median | P99 |Iteration
| ----------- | ----------- | ---------- | ------ | ------ | ----- | -----
| Erlang/OTP 28 | 25 | 65186 | 5.87% | 66828 | 68749 | 38352 ns
| Erlang/OTP 27 | 25 | 51730 | 4.64% | 51418 | 57296 | 48328 ns

In general, you can expect a 15% - 25% speed-up in Erlang/OTP 28 if you
are using TLS 1.3. No changes are needed in your code. If your application
uses TLS 1.3, this is a good reason to upgrade to Erlang/OTP 28.


# Nominal Types

Nominal type-checking as described in [EEP 69](https://www.erlang.org/eeps/eep-0069)
adds an alternative type system to Dialyzer. Nominal types can be declared
using the syntax `-nominal`. The main use case of nominal types is to prevent
accidental misuse of types with the same structure.

To start with, we can declare two nominal types `meter/0` and `foot/0`
like the following: 

````erlang
-nominal meter() :: integer().
-nominal foot() :: integer().
````

Because `meter/0` and `foot/0` have different names and they are both nominal
types, they are not compatible. Dialyzer performs nominal type-checking
on input and output types of functions and specifications. For example,
we can define functions `int_to_meter/1` and `foo/0` like the following:

````erlang
-spec int_to_meter(integer()) -> meter().
int_to_meter(X) -> X.

-spec foo() -> foot().
foo() -> int_to_meter(24).
````

The specification of `int_to_meter/1` declares the function's return type
to be `meter()`, so the result of `int_to_meter(24)` has type `meter()`.
However, the specification of `foo/0` declares the function's return type
to be `foot()`. The two nominal types are not compatible. Therefore, Dialyzer
raises the following warning for our example:

````erlang
Invalid type specification for function foo/0.
The success typing is foo() -> (meter() :: integer())
But the spec is foo() -> foot()
The return types do not overlap
````

On the other hand, a nominal type is compatible with a non-opaque, non-nominal
type with the same structure. We can define the function `return_integer/0`
like this:

````erlang
-spec return_integer() -> integer().
return_integer() -> int_to_meter(24).
````

The specification says that `return_integer/0` should return an `integer()`
type. However, the result of `int_to_meter(24)` has type `meter()`, so
`return_integer/0` will also return a `meter()` type. `integer()` is not
a nominal type.
The structure of `meter()` is compatible with `integer()`. Dialyzer can
analyze the function above without raising a warning.

There are exceptions to the nominal type-checking rules shown above. For more
details, see [Nominals](https://www.erlang.org/doc/system/nominals.html) in the
reference manual.

# New Emacs Erlang Mode

Althought this is not included in the Erlang/OTP 28 release, members of the OTP
team are developing a new Emacs Erlang mode using treesitter. If you are an
Emacs user, you can get it from [Github](https://github.com/erlang/emacs-erlang-ts)
or [Melpa](https://melpa.org/#/erlang-ts) and try it out.

The new Erlang mode handles strings and documentation a lot better than the
old one. See the screenshot below for an example:

![Source Code of `ssl:send/2` in the New Emacs Mode](/blog/images/28-emacs.png)

If you are interested in contributing to this project, all help is appreciated.