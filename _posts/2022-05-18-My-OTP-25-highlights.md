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

`maps:groups_from_list(Fun, List) -> MapOut`

Fun = fun((Elem :: T) -> Selected)
MapOut = #{Selected => List}
Selected = term()
List = [T]
T = term()

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

* [BeamAsm - the JIT compiler for Erlang](#beamasm---the-jit-compiler-for-erlang)
* [Improved error messages](#improved-error-messages)
* [Improved receive optimizations](#improved-receive-optimizations)
* [EEP-53: Process aliases](#eep-53-process-aliases)
* [EEP-48: Documentation chunks for edoc](#eep-48-documentation-chunks-for-edoc)
* [socket support in gen_tcp](#socket-support-in-gen_tcp)
* [EEP-56: Supervisor automatic shutdown](#EEP-56-supervisor-automatic-shutdown)
* [Edwards-curve Digital Signature Algorithm](#edwards-curve-digital-signature-algorithm)

