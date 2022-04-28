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
* New functions in the `maps`and `lists` modules
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

Misc #

A new DEVELOPMENT HOWTO guide has been added that describes how to build and test Erlang/OTP when fixing bugs or developing new functionality.
Testing has been added to the Github actions run for each opened PR so that more bugs are caught earlier when bug fixes and new features are proposed.

For more details about new features and potential incompatibilities see

    https://erlang.org/download/otp_src_25.0.readme


