---
layout: post
title: Erlang/OTP 25 Highlights
tags: erlang otp 25 release
author: Kenneth Lundin
---

OTP 25 is finally here. This post will introduce the new features that I am most excited about.

Erlang/OTP 25 includes contributions from ??+ external contributors totalling
??+ commits, ??+ PRs.

You can download the readme describing all the changes here: [Erlang/OTP 25 Readme].
Or, as always, look at the release notes of the application you are interested in.
For instance here: [Erlang/OTP 25 - Erts Release Notes - Version 13.0].

This years highlights are:
* [New functions in the `maps`and `lists` modules](#new-functions-in-the-mapsand-lists-modules)
* Selectable features as of EEP-60
* The new `maybe`expression (`maybe_expr) EEP-49
* The JIT now works for 64-bit ARM processors.
* Type-Based optimizations in the JIT.
* Improved the JIT’s support for external tools like `perf` and `gdb`
* ETS-tables with adaptive support for write concurrency
* Compiler news
* Relocatable installation directory for Erlang
* New option `short` to the functions `erlang:float_to_list/2` and `erlang:float_to_binary/2` 
* Introduction of quote/1 and unquote/1 functions in the uri_string module
* The new module peer supersedes the slave module
* global will now by default prevent overlapping partitions
* gen_server, gen_statem and gen_event has got a new format_status/1 callback.
* The timer module has been modernized and made more efficient


# New functions in the `maps`and `lists` modules

Triggered by suggestions from the users we have introduced new functions in the `maps` and `lists` modules in `stdlib.

## `maps:groups_from_list/2,3`

For short we can say that this function take a list of elements and group them. The result is a map `#{Group1 => [Group1Elements], GroupN => [GroupNElements]}`.

Let us look at some examples from the shell:

```erlang
> maps:groups_from_list(fun(X) -> X rem 2 end, [1,2,3]).
#{0 => [2], 1 => [1, 3]}
```
The provided fun calculates X rem 2 for every element X in the input list and then group the elements in a map with the result of X rem 2 as key and the corresponding elements as a list for that key.

```erlang
> maps:groups_from_list(fun erlang:length/1, ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["ant", "cat"], 5 => ["dingo"], 7 => ["buffalo"]}
```

In the example above the strings in the input list are grouped into a map based on their length.

## `maps:groups_from_list/3`

There is also a variant of groups_from_list with an additional fun by which the values can be converted or changed before they are put into their groups.

```erlang
> maps:groups_from_list(fun(X) -> X rem 2 end, fun(X) -> X*X end, [1,2,3]).
#{0 => [4], 1 => [1, 9]}
```
In the example above the elements X in the list are grouped according the X rem 2 calculation but the values stored in the groups are the elements multiplied by them selves (X*X).  

```erlang
> maps:groups_from_list(fun erlang:length/1, fun lists:reverse/1, ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["tna","tac"],5 => ["ognid"],7 => ["olaffub"]}
```

In the example above the strings from the input list are grouped according to their length and they are reversed before they are stored in the groups.

## `lists:enumerate/1,2`

Returns List1 with each element H replaced by a tuple of form {I, H} where I is the position of H in List1. The enumeration starts with 1 and increases by 1 in each step.

That is, enumerate/1 behaves as if it had been defined as follows:

```erlang
enumerate(List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc+1} end, 1, List),
  List1.
```

Example:

```erlang
> lists:enumerate([a,b,c]).
[{1,a},{2,b},{3,c}]
```

```erlang
> lists:enumerate(10, [a,b,c]).
[{10,a},{11,b},{12,c}]
```

## `lists:uniq/1,2`

Returns a list containing the elements of List1 with duplicated elements removed (preserving the order of the elements). The first occurrence of each element is kept.

Examples:

```erlang
> lists:uniq(fun([3,3,1,2,1,2,3]).
[3,1,2]
> lists:uniq([a, a, 1, b, 2, a, 3]).
[a, 1, b, 2, 3]
```
Returns a list containing the elements of List1 without the elements for which Fun returned duplicate values (preserving the order of the elements). The first occurrence of each element is kept.

Examples:
```erlang
> lists:uniq(fun({X, _}) -> X end, [{b, 2}, {a, 1}, {c, 3}, {a, 2}]).
[{b, 2}, {a, 1}, {c, 3}]
```
# Selectable features and the new `maybe_expr` feature

Selectable features is a new mechanism and concept where a new potentially incompatible feature (language or runtime), can be introduced and tested without causing troubles for those that don't use it.

When it comes to language features the intention is that they can be activated per module with no impact on modules where they are not activated.

Let's use the new `maybe_expr` feature as an example.

In module my_experiment the feature is activated and used like this:

```erlang
-module(my_experiment).
-export([foo/1]).

%% enable the feature maybe_expr in this module only
%% makes maybe a keyword which might be compatible 
%% in modules using maybe as a function name or an atom  
-feature(maybe_expr,enable) 
foo() ->
  maybe
    Foo = bar(),            % normal exprs still allowed
    {ok, X} ?= f(Foo),
    [H|T] ?= g([1,2,3]),
    ...
  else
    {error, Y} ->
        {ok, "default"};
    {ok, _Term} ->
        {error, "unexpected wrapper"}
end
```
The compiler will note that the feature maybe_expr is enabled and will handle the maybe construct correctly. In the generated `.beam` file it will also be noted that
the module has enabled the feature.

When starting an Erlang node the specific feature (or all) must be enabled otherwise the `.beam` file with the feature will not be allowed for loading.

```
erl -enable-feature maybe_expr
```
Or 
```
erl -enable-feature all
```

## The new `maybe_expr` feature EEP-49

The EEP-49 "Value-Based Error Handling Mechanisms", was suggested by Fred Hebért already 2018 and now finally
it has been implemented as the first feature in the new feature concept.

Specification
=============

We propose the `maybe ... end` construct which is similar to `begin
... end` in that it is used to group multiple distinct expression as a
single block. But there is one important difference in that the
`maybe` block does not export its variables while `begin` does
export its variables.

We propose a new
type of expressions (denoted `MatchOrReturnExprs`), which are only valid within a
`maybe ... end` expression:

    maybe
        Exprs | MatchOrReturnExprs
    end

`MatchOrReturnExprs` are defined as having the following form:

    Pattern ?= Expr

This definition means that `MatchOrReturnExprs` are only allowed at the
top-level of `maybe ... end` expressions.

The `?=` operator takes the value returned by `Expr` and pattern matches on
it against `Pattern`.

If the pattern matches, all variables from `Pattern` are bound in the local
environment, and the expression is equivalent to a successful `Pattern = Expr`
call. If the value does not match, the `maybe ... end` expression returns the
failed expression directly.

A special case exists in which we extend `maybe ... end` into the following form:

    maybe
        Exprs | MatchOrReturnExprs
    else
        Pattern -> Exprs;
        ...
        Pattern -> Exprs
    end

This form exists to capture non-matching expressions in a `MatchOrReturnExprs`
to handle failed matches rather than returning their value. In such a case, an
unhandled failed match will raise an `else_clause` error, otherwise identical to
a `case_clause` error.

This extended form is useful to properly identify and handle successful and
unsuccessful matches within the same construct without risking to confuse
happy and unhappy paths.

Given the structure described here, the final expression may look like:

    maybe
        Foo = bar(),            % normal exprs still allowed
        {ok, X} ?= f(Foo),
        [H|T] ?= g([1,2,3]),
        ...
    else
        {error, Y} ->
            {ok, "default"};
        {ok, _Term} ->
            {error, "unexpected wrapper"}
    end

Do note that to allow easier pattern matching and more intuitive usage,
the `?=` operator should have associativity rules lower than `=`, such that:

    maybe
        X = [H|T] ?= exp()
    end

is a valid `MatchOrReturnExprs` equivalent to the non-infix form `'?='('='(X,
[H|T]), exp())`, since reversing the priorities would give `'='('?='(X, [H|T]),
exp())`, which would create a `MatchOrReturnExp` out of context and be invalid.

Motivation
==========

Erlang has some of the most flexible error handling available across a
large number of programming languages. The language supports:

1. three types of exceptions (`throw`, `error`, `exit`)
   - handled by `catch Exp`
   - handled by `try ... [of ...] catch ... [after ...] end`
2. links, `exit/2`, and `trap_exit`
3. monitors
4. return values such as `{ok, Val} | {error, Term}`,
    `{ok, Val} | false`, or `ok | {error, Val}`
5. a combination of one or more of the above

So why should we look to add more? There are various reasons for this,
including trying to reduce deeply nested conditional expressions,
cleaning up some messy patterns found in the wild, and providing a better
separation of concerns when implementing functions.

Reducing Nesting
----------------

One common pattern that can be seen in Erlang is deep nesting of `case
... end` expressions, to check complex conditionals.

Take the following code taken from
[Mnesia](https://github.com/erlang/otp/blob/a0ae44f324576104760a63fe6cf63e0ca31756fc/lib/mnesia/src/mnesia_backup.erl#L106-L126),
for example:

    commit_write(OpaqueData) ->
        B = OpaqueData,
        case disk_log:sync(B#backup.file_desc) of
            ok ->
                case disk_log:close(B#backup.file_desc) of
                    ok ->
                        case file:rename(B#backup.tmp_file, B#backup.file) of
                           ok ->
                                {ok, B#backup.file};
                           {error, Reason} ->
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end.

The code is nested to the extent that shorter aliases must be introduced
for variables (`OpaqueData` renamed to `B`), and half of the code just
transparently returns the exact values each function was given.

By comparison, the same code could be written as follows with the new
construct:

    commit_write(OpaqueData) ->
        maybe
            ok ?= disk_log:sync(OpaqueData#backup.file_desc),
            ok ?= disk_log:close(OpaqueData#backup.file_desc),
            ok ?= file:rename(OpaqueData#backup.tmp_file, OpaqueData#backup.file),
            {ok, OpaqueData#backup.file}
        end.

Or, to protect against `disk_log` calls returning something else than `ok |
{error, Reason}`, the following form could be used:

    commit_write(OpaqueData) ->
        maybe
            ok ?= disk_log:sync(OpaqueData#backup.file_desc),
            ok ?= disk_log:close(OpaqueData#backup.file_desc),
            ok ?= file:rename(OpaqueData#backup.tmp_file, OpaqueData#backup.file),
            {ok, OpaqueData#backup.file}
        else
            {error, Reason} -> {error, Reason}
        end.

The semantics of these calls are identical, except that it is now
much easier to focus on the flow of individual operations and either
success or error paths.

Obsoleting Messy Patterns
-------------------------

Frequent ways in which people work with sequences of failable operations
include folds over lists of functions, and abusing list comprehensions.
Both patterns have heavy weaknesses that makes them less than ideal.

Folds over list of functions use patterns such as those defined in
[posts from the
mailing list](http://erlang.org/pipermail/erlang-questions/2017-September/093575.html):

    pre_check(Action, User, Context, ExternalThingy) ->
        Checks =
            [fun check_request/1,
             fun check_permission/1,
             fun check_dispatch_target/1,
             fun check_condition/1],
        Args = {Action, User, Context, ExternalThingy},
        Harness =
            fun
                (Check, ok)    -> Check(Args);
                (_,     Error) -> Error
            end,
        case lists:foldl(Harness, ok, Checks) of
            ok    -> dispatch(Action, User, Context);
            Error -> Error
        end.

This code requires declaring the functions one by one, ensuring the
entire context is carried from function to function. Since there is no
shared scope between functions, all functions must operate on all
arguments.

By comparison, the same code could be implemented with the new construct
as:

    pre_check(Action, User, Context, ExternalThingy) ->
        maybe
            ok ?= check_request(Context, User),
            ok ?= check_permissions(Action, User),
            ok ?= check_dispatch_target(ExternalThingy),
            ok ?= check_condition(Action, Context),
            dispatch(Action, User, Context)
        end.

And if there was a need for derived state between any two steps, it
would be easy to weave it in:

    pre_check(Action, User, Context, ExternalThingy) ->
        maybe
            ok ?= check_request(Context, User),
            ok ?= check_permissions(Action, User),
            ok ?= check_dispatch_target(ExternalThingy),
            DispatchData = dispatch_target(ExternalThingy),
            ok ?= check_condition(Action, Context),
            dispatch(Action, User, Context, DispatchData)
        end.

The list comprehension _hack_, by comparison, is a bit more rare. In
fact, it is mostly theoretical. Some things that hint at how it could
work can be found in [Diameter test
cases](https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/diameter/test/diameter_examples_SUITE.erl#L254-L266)
or the [PropEr plugin for
Rebar3](https://github.com/ferd/rebar3_proper/blob/e7eb96498a9d31f41c919474ec6800df62e237e1/src/rebar3_proper_prv.erl#L298-L308).

Its overall form uses generators in list comprehensions to tunnel a happy
path:

    [Res] =
        [f(Z) || {ok, W} <- [b()],
                 {ok, X} <- [c(W)],
                 {ok, Y} <- [d(X)],
                 Z <- [e(Y)]],
    Res.

This form doesn't see too much usage since it is fairly obtuse and I
suspect most people have either been reasonable enough not to use it, or
did not think about it. Obviously the new form would be cleaner:

    maybe
        {ok, W} ?= b(),
        {ok, X} ?= c(W),
        {ok, Y} ?= d(X),
        Z = e(Y),
        f(Z)
    end

which on top of it, has the benefit of returning an error value if one
is found.

# Compiler news
* Add compile attribute `-nifs()` to empower compiler and loader with information     about   which functions may be overridden as NIFs by `erlang:load_nif/2`.
    Improved and more detailed error messages when binary construction with the binary syntax fails. This applies both for error messages in the shell and for `erl_error:format_exception/3,4`.


# Crypto
`crypto:hash_equals/2` which is a constant time comparision of hashvalues.

# Dialyzer

Optimize operations in the erl_types module. Parallelize the Dialyzer pass remote.
Added the missing_return and extra_return options to raise warnings when specifications differ from inferred types. These are similar to, but not quite as verbose as overspecs and underspecs.
Dialyzer now better understands the types for min/2, max/2, and erlang:raise/3. Because of that, Dialyzer can potentially generate new warnings. In particular, functions that use erlang:raise/3 could now need a spec with a no_return() return type to avoid an unwanted warning.


# Improvements of the JIT

The [JIT compiler][jit] introduced in Erlang/OTP 24 improved
the performance for Erlang applications.

Erlang/OTP 25 introduces some major improvements of the JIT:

* The JIT now supports the [AArch64 (ARM64)][aarch64] architecture,
used by (for example) [Apple Silicon][apple_silicon] Macs and newer
[Raspberry Pi][rpi] devices.

* Better code generated based on types provided by the Erlang compiler.

* Better support for `perf` and `gdb` with line numbers for Erlang code.

[jit]: https://www.erlang.org/blog/my-otp-24-highlights/#beamasm---the-jit-compiler-for-erlang
[aarch64]: https://en.wikipedia.org/wiki/AArch64
[apple_silicon]: https://en.wikipedia.org/wiki/Apple_silicon
[rpi]: https://en.wikipedia.org/wiki/Raspberry_Pi

### Support for AArch64 (ARM64)

How much speedup one can expect from the JIT compared to the interpreter
varies from nothing to up to four times.

To get some more concrete figures we have run three different
benchmarks with the JIT disabled and enabled on a MacBook Pro (M1
processor; released in 2020).

First we ran the [EStone benchmark][estone]. Without the JIT, 691,962
EStones were achieved and with the JIT 1,597,949 EStones. That is,
more than twice as many EStones with the JIT.

Next we tried running Dialyzer to build a small PLT:

```
$ dialyzer --build_plt --apps erts kernel stdlib
```

With the JIT, the time for building the PLT was reduced from 18.38 seconds
down to 9.64 seconds. That is, almost but not quite twice as fast.

Finally, we ran a benchmark for the [base64][base64] module included
in [this Github issue][gh-5639].

Without the JIT:

```
== Testing with 1 MB ==
fun base64:encode/1: 1000 iterations in 11846 ms: 84 it/sec
fun base64:decode/1: 1000 iterations in 14617 ms: 68 it/sec
```

With the JIT:

```
== Testing with 1 MB ==
fun base64:encode/1: 1000 iterations in 25938 ms: 38 it/sec
fun base64:decode/1: 1000 iterations in 20603 ms: 48 it/sec
```

Encoding with the JIT is almost two and half times as fast, while the
decoding time with the JIT is about 75 percent of the decoding time
without the JIT.

[estone]: https://github.com/erlang/otp/blob/be860185407d6747dca32e8d328b041cc75ffdb3/erts/emulator/test/estone_SUITE.erl
[base64]: https://www.erlang.org/doc/man/base64.html
[gh-5639]: https://github.com/erlang/otp/issues/5639

### Type-based optimizations

The JIT translates one BEAM instruction at the time to native code
without any knowledge of previous instructions. For example, the native
code for the `+` operator must work for any operands: small integers that
fit in 64-bit word, large insters, floats, and non-numbers that should
result in raising an exception.

In Erlang/OTP 25, the compiler embeds type information in the BEAM file
to the help the JIT generate better native code without unnecessary type
tests.

For more details, see the blog post [Type-Based Optimizations in the
JIT][type-based-opts].

[type-based-opts]: https://www.erlang.org/blog/type-based-optimizations-in-the-jit

# Better support for `perf` and `gdb`

**Ask John or Lukas to provide examples.**

# Improved error information for failing binary construction

Erlang/OTP 24 introduced [improved BIF error information][ext_bif_info] to provide
more information when a call to a BIF failed.

[ext_bif_info]: https://www.erlang.org/blog/my-otp-24-highlights/#eep-54-improved-bif-error-information

In Erlang/OTP 25, improved error infomation is also given when the
creation of a binary using the [bit syntax][bit_syntax] fails.

Consider this function:

```
bin(A, B, C, D) ->
    <<A/float,B:4/binary,C:16,D/binary>>.
```

If we call this function with incorrect arguments in past releases
we will just be told that something was wrong and the line number:


```
1> t:bin(<<"abc">>, 2.0, 42, <<1:7>>).
** exception error: bad argument
     in function  t:bin/4 (t.erl, line 5)
```

But which part of line 5? Imagine that `t:bin/4` was called from deep
within an application and we had no idea what the actual values for
the arguments were. It could take a while to figure out exactly what
went wrong.

Erlang/OTP 25 gives us more information:

```
1> c(t).
{ok,t}
2> t:bin(<<"abc">>, 2.0, 42, <<1:7>>).
** exception error: construction of binary failed
     in function  t:bin/4 (t.erl, line 5)
        *** segment 1 of type 'float': expected a float or an integer but got: <<"abc">>
```

Note that the module must be compiled by the compiler in Erlang/OTP 25 in
order to get the more informative error message. The old-style message will
be shown if the module was compiled by a previous release.

Here the message tells us that first segment in the construction was given
the binary `<<"abc">>` instead of a float or an integer, which is the expected
type for a `float` segment.

It seems that we switched the first and second arguments for `bin/4`,
so we try again:

```
3> t:bin(2.0, <<"abc">>, 42, <<1:7>>).
** exception error: construction of binary failed
     in function  t:bin/4 (t.erl, line 5)
        *** segment 2 of type 'binary': the value <<"abc">> is shorter than the size of the segment
```

It seems that there was more than one incorrect argument. In this
case, the message tells us that the given binary is shorter than the
size of the segment.

Fixing that:

```
4> t:bin(2.0, <<"abcd">>, 42, <<1:7>>).
** exception error: construction of binary failed
     in function  t:bin/4 (t.erl, line 5)
        *** segment 4 of type 'binary': the size of the value <<1:7>> is not a multiple of the unit for the segment
```

A `binary` segment has a default unit of 8. Therefore, passing a bitstring of
size 7 will fail.

Finally:

```
5> t:bin(2.0, <<"abcd">>, 42, <<1:8>>).
<<64,0,0,0,0,0,0,0,97,98,99,100,0,42,1>>
```

[bit_syntax]: https://www.erlang.org/doc/reference_manual/expressions.html#bit-syntax-expressions
[pr5281]: https://github.com/erlang/otp/pull/5281
[eep54]: https://www.erlang.org/eeps/eep-0054.html

# Improved error information for failed record matching

Another improvement is the exceptions when matching of a record fails.

Consider this record and function:

```
-record(rec, {count}).

rec_add(R) ->
    R#rec{count = R#rec.count + 1}.

```

In past releases, failure to match a record or retrieve an element from
a record would result in the following exception:

```
1> t:rec_add({wrong,0}).
** exception error: {badrecord,rec}
     in function  t:rec_add/1 (t.erl, line 8)
```

Before Erlang/OTP 15 that introduced line numbers in exceptions, knowing
which record that was expected could be useful if the error occurred in
a large function.

Nowadays, unless several different records are accessed on the same
line, the line number makes it obvious which record was expected.

Therefore, in Erlang/OTP 25 the `badrecord` exception has been changed
to show the actual incorrect value:

```
2> t:rec_add({wrong,0}).
** exception error: {badrecord,{wrong,0}}
     in function  t:rec_add/1 (t.erl, line 8)
```

The new `badrecord` exceptions will show up for code that has been compiled
with Erlang/OTP 25.

Misc #

A new DEVELOPMENT HOWTO guide has been added that describes how to build and test Erlang/OTP when fixing bugs or developing new functionality.
Testing has been added to the Github actions run for each opened PR so that more bugs are caught earlier when bug fixes and new features are proposed.

For more details about new features and potential incompatibilities see

    https://erlang.org/download/otp_src_25.0.readme


