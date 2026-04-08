---
layout: post
title: Erlang/OTP 29 Highlights
tags: erlang otp 29 release
author: Björn Gustavsson
---

Erlang/OTP 29 is finally here. This blog post introduces the new
features that we are most excited about.

A list of all changes can be found in [Erlang/OTP 29
Readme](https://erlang.org/patches/OTP-29.0). Or, as always, look at
the release notes of the application you are interested in. For
instance: [Erlang/OTP 29 - Erts Release Notes - Version
17.0](https://www.erlang.org/doc/apps/erts/notes.html#erts-17-0).

This year's highlights mentioned in this blog post are:

* [Native Records](#native-records)
* [Security](#security)
  * [Secure Coding Guidelines](#secure-coding-guidelines)
  * [Warnings for Unsafe Functions and Potentially Unsafe Functions](#warnings-for-unsafe-functions-and-potentially-unsafe-functions)
  * [Xref Improvements](#xref-improvements)
  * [Safer Default Code Path](#safer-default-code-path)
* [New Compiler Warnings](#new-compiler-warnings)
  * [Old-Style `catch`](#old-style-catch)
  * [Using the `and` and `or` Operators](#using-the-and-and-or-operators)
  * [Exporting Variables Out of a Subexpression](#exporting-variables-out-of-a-subexpression)
  * [Unifying Constructors](#unifying-constructors)
* [New Language Features](#new-language-features)
  * [New Guard BIF `is_integer/3`](#new-guard-bif-is_integer3)
  * [Assignment in Comprehensions](#assignment-in-comprehensions)
  * [Multi-Valued Comprehensions](#multi-valued-comprehensions)
* [New Functionality in STDLIB](#new-functionality-in-stdlib)
  * [`array` Module Updates](#array-module-updates)
  * [Iteration Order of Maps](#iteration-order-of-maps)
  * [Shuffling Lists](#shuffling-lists)
  * [`io_ansi`: Controlling Colors and Style](#io_ansi-controlling-colors-and-style)
  * [New Safeguards in `gb_trees` and `gb_sets`](#new-safeguards-in-gb_trees-and-gb_sets)
  * [`graph`: Functional digraphs](#graph-functional-digraphs)
  * [`erl_tar` Improvements](#erl_tar-improvements)
* [SSL and SSH Security Improvements](#ssl-and-ssh-security-improvements)
  * [More Secure Defaults for SSL and SSH](#more-secure-defaults-for-ssl-and-ssh)
  * [Safer SSH Daemon By Default](#safer-ssh-daemon-by-default)
* [Hardening Guides](#hardening-guides)

# Native Records

A native record is a data structure similar to traditional tuple-based
records, except that it is a true data type, natively supported by the
runtime system.

A native record is defined with a slightly different syntax compared
to a tuple-based record. As an example, consider the definition of a
tuple-based record with no elements:

```erlang
-record(empty, {}).
```

To instead make that a native record, the following syntax is used:

```erlang
-record #empty{}.
```

To create, update, match, and access individual elements, the same
syntax is used as for tuple-based records.

Here is an example:

```erlang
-module(geom).
-export([make_vec/2]).
-record #vec{x=0.0, y=0.0}.

make_vec(X, Y) ->
    #vec{x=X, y=Y}.
```

Given this module, a record can be created by calling `geom:make_vec/2`:

```erlang
1> geom:make_vec(10.0, 20.0).
#geom:vec{x = 10.0,y = 20.0}
```

When a native record is printed, its name is prefixed with the module
where the record definition is located.

By default, a native record definition is private to the containing
module. Attempting to use the record from other modules will fail.
For example:

```erlang
-module(example).
-export([create/2, match/1]).

create(X, Y) ->
    #geom:vec{x=X, y=Y}.

match(#geom:vec{x=X, y=Y}) ->
    {X, Y}.
```

Given this module, we can try to call it:

```erlang
1> example:create(1.0, 2.0).
** exception error: {badrecord,{geom,vec}}
     in function  example:create/2 (example.erl:5)
2> Vec = geom:make_vec(1.0, 2.0).
#geom:vec{x = 1.0,y = 2.0}
3> example:match(Vec).
** exception error: no function clause matching example:match(#geom:vec{x = 1.0,y = 2.0})
```

However, matching only on the module name and record name will work.
For example:

```erlang
is_vec(#geom:vec{}) -> true;
is_vec(_) -> false.
```

To make a record accessible from other modules, it must be exported
using `-export_record()`. For example:

```erlang
-module(geom).
-export([make_vec/2]).
-export_record([vec]).
-record #vec{x=0.0, y=0.0}.

make_vec(X, Y) ->
    #vec{x=X, y=Y}.
```

With the record exported, the call to `example` will now work:

```erlang
1> V = example:create(1.0, 2.0).
#geom:vec{x = 1.0,y = 2.0}
2> example:match(V).
{1.0,2.0}
```

Native records are considered experimental in Erlang/OTP 29, and possibly also
in Erlang/OTP 30, meaning that their behavior may change, potentially
requiring updates to applications that use them.

For more information, see
[Native Records](https://www.erlang.org/doc/system/ref_man_native_records.html)
and [EEP-79](https://www.erlang.org/eeps/eep-0079).

# Security

## Secure Coding Guidelines

The Erlang/OTP 28.5 patch release includes [Secure Coding
Guidelines](https://www.erlang.org/doc/system/secure_coding.html)
(found under the [Design
Principles](https://www.erlang.org/doc/system/design_principles.html)
header). It provides guidelines for writing secure Erlang code,
describing common pitfalls and weaknesses best avoided, including a
section with concrete secure coding rules.

## Warnings for Unsafe Functions and Potentially Unsafe Functions

As a complement to the Secure Coding Guidelines, the compiler can now
warn about always unsafe and potentially unsafe functions. For example,
when compiling a module containing this function:

```erlang
unsafe(CipherText, Key) ->
    public_key:decrypt_public(CipherText, Key).
```
the compiler will emit the following warning:

```erlang
some_module.erl:5:5: Warning: public_key:decrypt_public/2 is unsafe; see the documentation for details
%    5|     public_key:decrypt_public(CipherText, Key).
%     |     ^
```

`public_key:decrypt_public/2` is one of the functions that is always
considered unsafe.

It is also possible to enable warnings for potentially unsafe
functions. For example, when compiling a module containing this
function with the `warn_possibly_unsafe_function` option:

```erlang
potentially_unsafe(L) ->
    list_to_atom(L).
```

the compiler will emit the following warning:


```
% erlc +warn_possibly_unsafe_function some_module.erl
some_module.erl:8:5: Warning: erlang:list_to_atom/1 is unsafe; however, it is possible to use it safely under some conditions. See the documentation for details
%    8|     list_to_atom(L).
%     |     ^
```

Functions that can create new atoms are considered potentially unsafe.
For more information, see [Do Not Abuse
Atoms](https://www.erlang.org/doc/system/secure_coding.html#rule-dsg-003).

## Xref Improvements

[`xref`](https://www.erlang.org/doc/apps/tools/xref.html) will now
recognize `-unsafe` attributes, which are used to mark functions as
unsafe.

The following new pre-defined analyses have been added:

* `xref:analyze(S, unsafe_function_calls)` -- find calls to functions
  marked with the `-unsafe` attribute.

* `xref:analyze(S, undocumented_function_calls)` -- find calls to
  functions in another application that lack a `-doc` attribute.

* `xref:analyze(S, private_function_calls)` -- find calls to
  functions in another application that are marked as private
  (the `-doc` attribute is either `false` or `hidden`).

## Safer Default Code Path

Before Erlang/OTP 29, the code server had the current working
directory (`.`) as the first item in the code path. That means that
the code server would search for modules to load first in the current
working directory, then in OTP or the application being run.

That was problematic, because if a corrupt BEAM file with the same
name as a module in OTP was placed in the current working directory,
the system could fail to start.

For example, if we created an empty BEAM file named `global.beam`,
Erlang/OTP 28 would not start:

```
% touch global.beam
% erl
2026-04-27 09:09:57.704408 ~s~n
	"beam/beam_load.c(146): Error loading module global:\n  corrupt file header\n"
=SUPERVISOR REPORT==== 27-Apr-2026::09:09:57.704431 ===
    supervisor: {local,kernel_sup}
    errorContext: start_error
    reason: {'EXIT',
                {undef,
                    [{global,start_link,[],[]},
                     {supervisor,do_start_child_i,3,
                         [{file,"supervisor.erl"},{line,996}]},
                     {supervisor,do_start_child,3,
                         [{file,"supervisor.erl"},{line,982}]},
                     {supervisor,'-start_children/2-fun-0-',3,
                         [{file,"supervisor.erl"},{line,966}]},
                     {supervisor,children_map,4,
                         [{file,"supervisor.erl"},{line,1891}]},
                     {supervisor,init_children,2,
                         [{file,"supervisor.erl"},{line,932}]},
                     {gen_server,init_it,2,
                         [{file,"gen_server.erl"},{line,2276}]},
                     {gen_server,init_it,6,
                         [{file,"gen_server.erl"},{line,2236}]}]}}
    offender: [{pid,undefined},
               {id,global_name_server},
               {mfargs,{global,start_link,[]}},
               {restart_type,permanent},
               {significant,false},
               {shutdown,2000},
               {child_type,worker}]

    .
    .
    .

Crash dump is being written to: erl_crash.dump...done
```

Even worse, if a modified or out-of-date version of a BEAM file were
placed in the current working directory, either deliberately or
accidentally, the system could misbehave in subtle ways.

To eliminate this attack vector, in Erlang/OTP 29 the current working
directory is added as the last item in the code path.

BEAM files having the same name as a BEAM file in OTP will no longer be loaded:

```
% touch global.beam
% erl
Erlang/OTP 29 . . .

Eshell V17.0 (press Ctrl+G to abort, type help(). for help)
1>
```

# New Compiler Warnings

There are several new warnings for old language constructs that are
problematic from a security and/or maintainability perspective.

## Old-Style `catch`

The [Secure Coding
Guidelines](https://www.erlang.org/doc/system/secure_coding.html) discourage
the use of the old-style `catch`;
see [Do not use `catch`](https://www.erlang.org/doc/system/secure_coding.html#rule-lng-002).

The compiler will now emit a warning when the old-style `catch` is
used. It is recommended to instead use `try`...`catch`...`end`.

This is an [open-source contribution](https://github.com/erlang/otp/pull/10421)
by Richard Carlsson.

## Using the `and` and `or` Operators

The Secure Coding Guidelines discourage the use of the `and` and `or`
operators (see [Do not use `and` and
`or`](https://www.erlang.org/doc/system/secure_coding.html#rule-lng-003)).

When compiling with the option `warn_obsolete_bool_op`, the compiler will emit
warnings for use of `and` and `or`. This option will probably become default
in a future release.

This is an [open-source contribution](https://github.com/erlang/otp/pull/9115)
by Richard Carlsson.

## Exporting Variables Out of a Subexpression

Binding a variable in a subexpression and then using it later is often
confusing. Therefore, the compiler will now warn about such code.

For example:

```erlang
case file:open(File, AllOpts = [write,{encoding,utf8}]) of
   {ok,Fd} ->
       {Fd,AllOpts}
end
```

This can be written more clearly like so:

```erlang
AllOpts = [write,{encoding,utf8}],
case file:open(File, AllOpts) of
    {ok,Fd} ->
        {Fd,AllOpts}
end
```

This is an [open-source contribution](https://github.com/erlang/otp/pull/9134)
by Richard Carlsson.

## Unifying Constructors

The compiler will now warn about matches that unify constructors, such
as the following:

```erlang
m({a,B} = {Y,Z}) -> . . .
```

Such a match can be rewritten as:

```erlang
m({a=Y,B=Z}) -> . . .
```

This is an [open-source contribution](https://github.com/erlang/otp/pull/10433)
by Richard Carlsson.

# New Language Features

## New Guard BIF `is_integer/3`

When writing ranges checks for integer or characters, it's easy to
write the check in a way that will let floats slip through. For
example:

```erlang
is_digit(C) ->
    $0 =< C andalso C =< $9.
```

`is_digit/1` is supposed to test whether the given character is a digit,
but it will also return `true` for some floats:

```erlang
1> some_module:is_digit($1).
true
1> some_module:is_digit($1 + 0.5).
true
```

To exclude floats, one must add an `is_integer/1` test:

```erlang
is_digit(C) ->
    is_integer(C) andalso $0 =< C andalso C =< $9.
```

Using the new guard BIF `is_integer/3`, the expression can be
simplified:

```erlang
is_digit(C) ->
    is_integer(C, $0, $9).
```

## Assignment in Comprehensions

Sometimes one might want to evaluate an expression in a comprehension
and use it more than once, for example to first use the value in a
filter and also include in the result.

Just as an example, say we want to calculate the hash value and only
keep hash values that can be evenly divided by 10. The most obvious
way to do it is probably to calculate the hash value twice:

```erlang
1> fh(List) ->
    [erlang:phash2(E) || E <- List,
                         erlang:phash2(E) rem 10 =:= 0].
ok
2> fh(lists:seq(1, 10)).
[2614250]
```

This feels clumsy, and is unnecessarily slow particularly if the terms
in the list are large. A somewhat better way to do it is to utilize a
single-element list generator:

```erlang
1> fh(List) -> [H || E <- List,
                     H <- [erlang:phash2(E)], H rem 10 =:= 0].
ok
2> fh(lists:seq(1, 10)).
[2614250]
```

That certainly works, and in Erlang/OTP 29 the compiler will optimize
the single-element list generator to avoid actually building the list,
but it still feels clumsy.

In Erlang/OTP 29, there is a more elegant way to do it using the
`compr_assign` feature:

```erlang
$ erl -enable-feature compr_assign
. . .
1> fh(List) -> [H || E <- List,
                     H = erlang:phash2(E), H rem 10 =:= 0].
ok
2> fh(lists:seq(1, 10)).
[2614250]
```

Note the command line option for `erl`. This is necessary to allow
the `compr_assign` feature to be used in the shell.

The reason this doesn't work out of the box is that the syntax was
accepted before Erlang/OTP 29, but the behavior was totally useless.
The `H = erlang:phash2(E)` expression was seen as an illegal filter
(since it didn't evaluate to a boolean).

```erlang
Erlang/OTP 28 . . .
$ erl
1> fh(List) -> [H || E <- List, H = erlang:phash2(E), H rem 10 =:= 0].
ok
2> fh(lists:seq(1, 10)).
* exception error: bad filter 2614250
```

In Erlang/OTP 29, this comprehension will not compile if the
`compr_assign` feature is not enabled:

```
%% Erlang/OTP 29
$ erl
Erlang/OTP 29 . . .
1> fh(List) -> [H || E <- List, H = erlang:phash2(E), H rem 10 =:= 0].
* 5:14: matches using '=' are not allowed in comprehension qualifiers
unless the experimental 'compr_assign' language feature is enabled.
With 'compr_assign' enabled, a match 'P = E' will behave as a
strict generator 'P <-:- [E]'."
```

It is expected that the `compr_assign` feature will be enabled by
default in Erlang/OTP 30.

Let's look at another example:

```erlang
-module(example).
-feature(compr_assign, enable).
-export([cat/1]).

cat(Files) ->
	[Char || F <- Files,
			 {ok, Bin} = file:read_file(F),
			 Char <- unicode:characters_to_list(Bin)].
```

This feature is described in [EEP 77: Assignment in
Comprehensions](https://www.erlang.org/eeps/eep-0077).

The implementation is an [open-source
contribution](https://github.com/erlang/otp/pull/9153) by Richard
Carlsson.

## Multi-Valued Comprehensions

Suppose one needs to generate a list where each positive integer is
followed by the negative value of that same integer (perhaps in a test
case). Thus:

```erlang
[1,-1,2,-2,3,-3,4,-4,5,-5]
```

That can be done using list comprehensions in a few different ways.
For example:

```erlang
1> lists:append([[I, -I] || I <- lists:seq(1, 5)]).
[1,-1,2,-2,3,-3,4,-4,5,-5]
2> [I || N <- lists:seq(1, 5), I <- [N,-N]]
[1,-1,2,-2,3,-3,4,-4,5,-5]
```

This works, but extra two-element lists are allocated, and it is not
that clear what exactly is being generated.

Multi-valued comprehensions as described in [EEP
78](https://www.erlang.org/eeps/eep-0078) allow us to generate
the list in a more elegant way:

```erlang
1> [I, -I || I <- lists:seq(1, 5)].
[1,-1,2,-2,3,-3,4,-4,5,-5]
```

As another example, let's say that for testing we'll need to generate
a list of powers of 2 as well as the integer before and after each
power of 2.

Combining multi-valued comprehensions with the `compr_assign` feature
allows us to implement that in a single list comprehension:

```erlang
$ erl -enable-feature compr_assign
Erlang/OTP 29 . . .

Eshell V17.0 (press Ctrl+G to abort, type help(). for help)
1> [B-1, B, B+1 || P <- lists:seq(2, 5), B = 1 bsl P].
[3,4,5,7,8,9,15,16,17,31,32,33]
```

So the `P` variable is bound to each 2, 3, 4, and 5 in turn. `B`
is then bound to the corresponding power of 2 (4, 8, 16, and 32).
Finally, `B-1, B, B+1` creates three values out of each `B`.

The implementation is an [open-source
contribution](https://github.com/erlang/otp/pull/9374) by Michał
Muskała.

# New Functionality in STDLIB

## `array` Module Updates

The `array` module now has a write cache, which can make sequential
writes up to three times faster.

Many new functions have been added:

* `prepend/2` and `append/2` for prepending or appending a single element
  to an array.

* `concat/1` and `concat/2` for concatenating arrays.

* `slice/3` for extracting part of an array.

* `shift/2` for shifting an array left or right. Shifting it left drops
  elements from the beginning of the array, and shifting it right adds
  space in the beginning.

* `from/2` and `from/3` for creating an array by repeatedly calling
  a fun. For example, that makes it possible to create an array from
  a binary without first converting it to a list.

* New traversal functions that also take a lower and an upper index
  to limit the part of the array being traversed. For example, in
  addition to the existing `foldl(Fun, Init, Array)` there is now also
  `foldl(Low, High, Fun, Init, Array)`.

* A new family of `mapfold` functions, for example `mapfoldl/3` and
  `sparse_mapfoldr/5`.

This is an [open-source contribution](https://github.com/erlang/otp/pull/10578)
by Richard Carlsson.

## Iteration Order of Maps

The order of the keys in a map is undefined. On my computer, at the
time of writing, I get the following order:

```
1> maps:keys(#{q => 1, w => 2, x => 3, y => 4}).
[x,y,q,w]
```

An annoying thing in OTP 28 and earlier is that the order of keys
would be different depending on how one extracted the keys. For example,
let's create a map with 33 elements:

```
1> M = #{I => I * I || I <- lists:seq(0, 32)}.
#{18 => 324,4 => 16,12 => 144,19 => 361,29 => 841,13 => 169,
  2 => 4,7 => 49,31 => 961,8 => 64,10 => 100,23 => 529,
  9 => 81,15 => 225,32 => 1024,1 => 1,25 => 625,28 => 784,
  20 => 400,6 => 36,11 => 121,17 => 289,24 => 576,14 => 196,
  3 => 9,16 => 256,30 => 900,21 => 441,5 => 25,...}
```

One would expect that extracting the keys with `maps:keys/1` and using
a list comprehension with a map generator would yield the same result,
but alas, no:

```
%% Erlang/OTP 28
2> maps:keys(M).
[22,26,27,0,5,21,30,16,3,14,24,17,11,6,20,28,25,1,32,15,9,
 23,10,8,31,7,2,13,29|...]
3> [K || K := _ <- M].
[18,4,12,19,29,13,2,7,31,8,10,23,9,15,32,1,25,28,20,6,11,17,
 24,14,3,16,30,21,5|...]
4> lists:reverse(maps:keys(M)) =:= [K || K := _ <- M].
true
```

In Erlang/OTP 29, it is guaranteed that all ways of iterating over
maps generate the elements in the same order.

```
%% Erlang/OTP 29
2> maps:keys(M).
[18,4,12,19,29,13,2,7,31,8,10,23,9,15,32,1,25,28,20,6,11,17,
 24,14,3,16,30,21,5|...]
3> [K || K := _ <- M].
[18,4,12,19,29,13,2,7,31,8,10,23,9,15,32,1,25,28,20,6,11,17,
 24,14,3,16,30,21,5|...]
4> hd(maps:to_list(M)).
{18,324}
5> hd(maps:to_list(maps:iterator(M))).
{18,324}
```

This is an [open-source contribution](https://github.com/erlang/otp/pull/10626)
by Michał Muskała.


## Shuffling Lists

The `rand` module can now shuffle the contents of a list. For example:

```
1> rand:shuffle(lists:seq(1, 10)).
[6,8,4,2,9,5,10,3,1,7]
2> rand:shuffle(lists:seq(1, 10)).
[8,2,7,5,6,3,9,4,1,10]
```

Here is a recipe for generating a random poker hand:

```
3> lists:sublist(rand:shuffle([{Suit,Rank} ||
                       Suit <- [clubs,diamonds,hearts,spades],
                       Rank <- lists:seq(1, 13)]), 5).
[{hearts,12},
 {hearts,5},
 {diamonds,1},
 {diamonds,8},
 {hearts,4}]
```

## `io_ansi`: Controlling Colors and Style

The new [`io_ansi`](https://www.erlang.org/doc/apps/stdlib/io_ansi.html)
module lets you emit ANSI (Virtual Terminal) sequences
to the terminal, enabling colored/styled text and fully-fledged
terminal applications.

For example, the following call creates the ANSI sequences necessary
to output the "wrong answer: 99" in red and bold text:

```erlang
1> io_ansi:format([bold, red, "wrong answer: ", "~p\n"], [99]).
<<"\e[1m\e[31mwrong answer: 99\n\e(B\e[m">>
```

To print the text directly to the terminal, call `io_ansi:fwrite/2`
with the same arguments.

## New Safeguards in `gb_trees` and `gb_sets`

`gb_sets:from_ordset/1` creates a *gb\_set* from an already ordered
list of elements and is therefore more efficient than
`gb_sets:from_list/1`, which needs to sort the elements before
creating the *gb\_set*.

However, if we happen to give `gb_sets:from_ordset/1` a list that is
unordered, the created *gb\_set* would be invalid and anything could
happen. For example:

```
%% Erlang/OTP 28
1> S = gb_sets:from_ordset([3,2,1]).
{3,{2,{3,nil,nil},{1,nil,nil}}}
2> gb_sets:is_element(1, S).
false
3> gb_sets:is_element(2, S).
true
4> gb_sets:is_element(3, S).
false
```

In this particular case, `gb_sets:is_element/2` would not consider
the integers 1 and 3 to be members of the set.

In Erlang/OTP 29, `gb_sets:from_ordset/1` will verify that the
elements are ordered while building the *gb\_set*. This will still be
faster than `gb_sets:from_list/1` that will need to sort the input
list before building the *gb\_set*.

```erlang
%% Erlang/OTP 29
1> S = gb_sets:from_ordset([3,2,1]).
** exception error: bad argument: not_ordset
     in function  gb_sets:balance_list_checked_1/2 (gb_sets.erl:432)
     in call from gb_sets:balance_list_checked_1/2 (gb_sets.erl:422)
     in call from gb_sets:balance_list_checked/2 (gb_sets.erl:417)
     in call from gb_sets:from_ordset/1 (gb_sets.erl:515)
%% Oops!
2> S = gb_sets:from_ordset([1,2,3]).
{3,{2,{1,nil,nil},{3,nil,nil}}}
3> gb_sets:is_element(1, S).
true
```

The same safeguards have been added to `gb_trees:from_orddict/1`.

For convenience, there is also a new `gb_trees:from_list/1` function
for directly creating a *gb\_tree* from a list.

This is an [open-source contribution](https://github.com/erlang/otp/pull/10910)
by Maria Scott and Jan Uhlig.

## `graph`: Functional digraphs

The new [`graph`](https://www.erlang.org/doc/apps/stdlib/graph.html)
module is a functional equivalent of the
[`digraph`](https://www.erlang.org/doc/apps/stdlib/digraph.html) and
[`digraph_utils`](https://www.erlang.org/doc/apps/stdlib/digraph_utils.html)
modules.

To compare the APIs of `graph` and `digraph`, let's create a simple
digraph with a single edge. First, here is how to do it with `digraph`:

```erlang
1> G = digraph:new().
{digraph,#Ref<0.2128870979.1452670981.25546>,
         #Ref<0.2128870979.1452670981.25547>,
         #Ref<0.2128870979.1452670981.25548>,true}
2> digraph:add_vertex(G, a).
a
3> digraph:add_vertex(G, b).
b
4> digraph:add_edge(G, a, b).
['$e'|0]
```

Here is how to create the same graph using `graph`:

```erlang
1> G0 = graph:new().
{graph,#{},#{},#{},true,0}
2> G1 = graph:add_vertex(G0, a).
{graph,#{a => []},#{},#{},true,0}
3> G2 = graph:add_vertex(G1, b).
{graph,#{a => [],b => []},#{},#{},true,0}
4> G3 = graph:add_edge(G2, a, b).
{graph,#{a => [],b => []},
       #{b => [{a,b,[]}]},
       #{a => [{a,b,[]}]},
       true,0}
```

Since the API for `graph` is functional, every operation that updates
the graph returns the new graph.

For some use cases, a functional representation of a graph is much
more convenient because one can easily revert to a previous state of
the graph, for example in
[compiler optimizations](https://github.com/erlang/otp/blob/2f659dedef2cdd0740b7f0f9152ff696cd90b52f/lib/compiler/src/beam_ssa_bool.erl#L831-L842).

This is an [open-source contribution](https://github.com/erlang/otp/pull/10532)
by Richard Carlsson.

## `erl_tar` Improvements

[`erl_tar`](https://www.erlang.org/doc/apps/stdlib/erl_tar.html)
will use less memory when extracting large tar entries to
disk. Instead of reading each tar entry into memory, `erl_tar` now
streams data in chunks of 64 KB. The chunk size can be configured using
the new `{chunks,ChunkSize}` option. This is an [open-source
contribution](https://github.com/erlang/otp/pull/10818) by Eric
Meadows-Jönsson.

The new `{max_size,Size}` option will set a limit on the total size of
extracted data to protect against filling up the disk. This is an
[open-source contribution](https://github.com/erlang/otp/pull/10821)
by Eric Meadows-Jönsson.

Symlink checking has been improved. Some symlinks that were safe (such
as `dir/link -> ../file`) used to be rejected. This is an [open-source
contribution](https://github.com/erlang/otp/pull/10814) by Eric
Meadows-Jönsson.

# SSL and SSH Security Improvements

## More Secure Defaults for SSL and SSH

For both the `ssl` and `ssh` applications, by default the most
preferred key exchange algorithm is now a hybrid quantum-resistant
algorithm combining [ML-KEM-768](https://en.wikipedia.org/wiki/ML-KEM)
with [X25519](https://en.wikipedia.org/wiki/Curve25519). This provides
protection against both classical and quantum computer attacks while
maintaining backward compatibility through automatic fallback to other
algorithms when peers don't support it.

## Safer SSH Daemon By Default

The SSH daemon now has more secure defaults, in that the shell, exec
services, and the SFTP subsystem are no longer enabled by default.
This shrinks the attack surface. Applications that need these services
must explicitly enable them.

The Erlang shell is enabled like this:

```erlang
ssh:daemon(Port, [{shell, {shell, start, []}} | Options])
```

Erlang term evaluation is enabled like this:

```erlang
ssh:daemon(Port, [{exec, erlang_eval} | Options])
```

The SFTP subsystem is enabled like this:

```erlang
ssh:daemon(Port, [{subsystems, [ssh_sftpd:subsystem_spec([])]} | Options])
```

All services can also be enabled in one call:

```erlang
ssh:daemon(Port, [{shell, {shell, start, []}},
                  {exec, erlang_eval},
                  {subsystems, [ssh_sftpd:subsystem_spec([])]}
                  | Options])
```

# Hardening Guides

The `ssl` application has a new [TLS Hardening
Guide](https://www.erlang.org/doc/apps/ssl/ssl_hardening.html) with
guidelines on how to make TLS connections harder to attack.

The `inets` application also has a new
[Hardening](https://www.erlang.org/doc/apps/inets/hardening.html) guide.

The existing
[Hardening](https://www.erlang.org/doc/apps/ssh/hardening.html)
guide for the `ssh` application has been updated for Erlang/OTP 29.
