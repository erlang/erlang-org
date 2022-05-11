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
* [Selectable features and the new `maybe_expr` feature](#selectable-features-and-the-new-maybe_expr-feature)
* [Improvements of the JIT](#improvements-of-the-jit)
* ETS-tables with adaptive support for write concurrency
* Compiler news
* Relocatable installation directory for Erlang
* New option `short` to the functions `erlang:float_to_list/2` and `erlang:float_to_binary/2` 
* The new module peer supersedes the slave module
* global will now by default prevent overlapping partitions
* gen_server, gen_statem and gen_event has got a new format_status/1 callback.
* The timer module has been modernized and made more efficient
* CA-certificates can be fetched from the OS standard place 


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

The compiler will note that the feature `maybe_expr` is enabled and will handle the maybe construct correctly. In the generated `.beam` file it will also be noted that
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

The EEP-49 "Value-Based Error Handling Mechanisms", was suggested by Fred Hebèrt already 2018 and now finally
it has been implemented as the first feature in the new feature concept.

The `maybe ... end` construct which is similar to `begin ... end` in that it is used to group multiple distinct expression as a
single block. But there is one important difference in that the
`maybe` block does not export its variables while `begin` does
export its variables.

A new type of expressions (denoted `MatchOrReturnExprs`) are introduced, which are only valid within a
`maybe ... end` expression:

```
maybe
    Exprs | MatchOrReturnExprs
end
```

`MatchOrReturnExprs` are defined as having the following form:

```
Pattern ?= Expr
```

This definition means that `MatchOrReturnExprs` are only allowed at the
top-level of `maybe ... end` expressions.

The `?=` operator takes the value returned by `Expr` and pattern matches on
it against `Pattern`.

If the pattern matches, all variables from `Pattern` are bound in the local
environment, and the expression is equivalent to a successful `Pattern = Expr`
call. If the value does not match, the `maybe ... end` expression returns the
failed expression directly.

A special case exists in which we extend `maybe ... end` into the following form:

```
maybe
    Exprs | MatchOrReturnExprs
else
    Pattern -> Exprs;
    ...
    Pattern -> Exprs
end
```

This form exists to capture non-matching expressions in a `MatchOrReturnExprs`
to handle failed matches rather than returning their value. In such a case, an
unhandled failed match will raise an `else_clause` error, otherwise identical to
a `case_clause` error.

This extended form is useful to properly identify and handle successful and
unsuccessful matches within the same construct without risking to confuse
happy and unhappy paths.

Given the structure described here, the final expression may look like:

```erlang
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

Do note that to allow easier pattern matching and more intuitive usage,
the `?=` operator should have associativity rules lower than `=`, such that:

```erlang
maybe
    X = [H|T] ?= exp()
end
```

is a valid `MatchOrReturnExprs` equivalent to the non-infix form `'?='('='(X,
[H|T]), exp())`, since reversing the priorities would give `'='('?='(X, [H|T]),
exp())`, which would create a `MatchOrReturnExp` out of context and be invalid.

### Motivation

With the `maybe` construct it is possible to reduce deeply nested conditional expressions and make messy patterns found in the wild unnecessary. It also provides a better separation of concerns when implementing functions.

#### Reducing Nesting

One common pattern that can be seen in Erlang is deep nesting of `case
... end` expressions, to check complex conditionals.

Take the following code taken from
[Mnesia](https://github.com/erlang/otp/blob/a0ae44f324576104760a63fe6cf63e0ca31756fc/lib/mnesia/src/mnesia_backup.erl#L106-L126),
for example:

```erlang
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
```

The code is nested to the extent that shorter aliases must be introduced
for variables (`OpaqueData` renamed to `B`), and half of the code just
transparently returns the exact values each function was given.

By comparison, the same code could be written as follows with the new
construct:

```erlang
commit_write(OpaqueData) ->
    maybe
        ok ?= disk_log:sync(OpaqueData#backup.file_desc),
        ok ?= disk_log:close(OpaqueData#backup.file_desc),
        ok ?= file:rename(OpaqueData#backup.tmp_file, OpaqueData#backup.file),
        {ok, OpaqueData#backup.file}
    end.
```

Or, to protect against `disk_log` calls returning something else than `ok |
{error, Reason}`, the following form could be used:

```erlang
commit_write(OpaqueData) ->
    maybe
        ok ?= disk_log:sync(OpaqueData#backup.file_desc),
        ok ?= disk_log:close(OpaqueData#backup.file_desc),
        ok ?= file:rename(OpaqueData#backup.tmp_file, OpaqueData#backup.file),
        {ok, OpaqueData#backup.file}
    else
        {error, Reason} -> {error, Reason}
    end.
```

The semantics of these calls are identical, except that it is now
much easier to focus on the flow of individual operations and either
success or error paths.

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

It is now possible to profile Erlang systems with perf and get a mapping from the JIT code to the corresponding Erlang code. This will make it easy to find bottlenecks in the code.

The same goes for `gdb` which also can show which line of Erlang code a specific address in the JIT code corresponds to.

An Erlang node running under `perf` can be started like this:
```
sudo perf record bin/erl +JPperf true --call-graph
```
and then the result from perf could be viewed like this:
```
sudo perf report
```
If an Erlang node is started like this:
```
erl +JPperf true
```
And the pid for the node is `4711`

You can attach `perf` to the node like this:
```
sudo perf record --call-graph -p 4711
```
![alt text](/blog/images/perf_callgraph.png "perf call-graph")


## Linux perf support

perf can also be instrumented using BeamAsm symbols to provide more information. As with
gdb, only the currently executing function will show up in the stack trace, which means
that perf provides functionality similar to that of [eprof](https://erlang.org/doc/man/eprof.html).

You can run perf on BeamAsm like this:

    perf record erl +JPperf true

and then look at the results using `perf report` as you normally would with
perf.

Frame pointers are enabled when the `+JPperf true` option is passed, so you can
use `perf record --call-graph=fp` to get more context. This will give you
accurate call graphs for pure Erlang code, but in rare cases it fails to track
transitions from Erlang to C code and back. [`perf record --call-graph=lbr`](https://lwn.net/Articles/680985/)
may work better in those cases, but it's worse at tracking in general.

For example, you can run perf to analyze dialyzer building a PLT like this:

     ERL_FLAGS="+JPperf true +S 1" perf record --call-graph=fp \
       dialyzer --build_plt -Wunknown --apps compiler crypto erts kernel stdlib \
       syntax_tools asn1 edoc et ftp inets mnesia observer public_key \
       sasl runtime_tools snmp ssl tftp wx xmerl tools

The above code is run using `+S 1` to make the perf output easier to understand.
If you then run `perf report -f --no-children` you may get something similar to this:

![Linux Perf report: dialyzer PLT build](figures/perf-beamasm.png)

Any Erlang function in the report is prefixed with a `$` and all C functions have
their normal names. Any Erlang function that has the prefix `$global::` refers
to a global shared fragment.

So in the above, we can see that we spend the most time doing `eq`, i.e. comparing two terms.
By expanding it and looking at its parents we can see that it is the function
`erl_types:t_is_equal/2` that contributes the most to this value. Go and have a look
at it in the source code to see if you can figure out why so much time is spent there.

After `eq` we see the function `erl_types:t_has_var/1` where we spend almost
5% of the entire execution in. A while further down you can see `copy_struct_x`
which is the function used to copy terms. If we expand it to view the parents
we find that it is mostly `ets:lookup_element/3` that contributes to this time
via the Erlang function `dialyzer_plt:ets_table_lookup/2`.

### Flame Graph

You can also create a Flame Graph from the perf output. Flame Graphs are basically
just another way to look at the same data as the `perf report` output, but can
be more easily shared with others and manipulated to give a graph tailor-made for
your needs. For instance, if we run dialyzer with all schedulers:

    ## Run dialyzer with multiple schedulers
    ERL_FLAGS="+JPperf true" perf record --call-graph=fp \
      dialyzer --build_plt -Wunknown --apps compiler crypto erts kernel stdlib \
      syntax_tools asn1 edoc et ftp inets mnesia observer public_key \
      sasl runtime_tools snmp ssl tftp wx xmerl tools --statistics

And then use the scripts found at Brendan Gregg's [CPU Flame Graphs](http://www.brendangregg.com/FlameGraphs/cpuflamegraphs)
web page as follows:

    ## Collect the results
    perf script > out.perf
    ## run stackcollapse
    stackcollapse-perf.pl out.perf > out.folded
    ## Create the svg
    flamegraph.pl out.folded > out.svg

We get a graph that would look something like this:

![Linux Perf FlameGraph: dialyzer PLT build](figures/perf-beamasm.svg)

You can view a larger version [here](seefile/figures/perf-beamasm.svg). It contains
the same information, but it is easier to share with others as it does
not need the symbols in the executable.

Using the same data we can also produce a graph where the scheduler profile data
has been merged by using `sed`:

    ## Strip [0-9]+_ from all scheduler names
    sed -e 's/^[0-9]\+_//' out.folded > out.folded_sched
    ## Create the svg
    flamegraph.pl out.folded_sched > out_sched.svg

![Linux Perf FlameGraph: dialyzer PLT build](figures/perf-beamasm-merged.svg)

You can view a larger version [here](seefile/figures/perf-beamasm-merged.svg).
There are many different transformations that you can do to make the graph show
you what you want.

### Annotate perf functions

If you want to be able to use the `perf annotate` functionality (and in extension
the annotate functionality in the `perf report` gui) you need to use a monotonic
clock when calling `perf record`, i.e. `perf record -k mono`. So for a dialyzer
run you would do this:

    ERL_FLAGS="+JPperf true +S 1" perf record -k mono --call-graph=fp \
      dialyzer --build_plt -Wunknown --apps compiler crypto erts kernel stdlib \
      syntax_tools asn1 edoc et ftp inets mnesia observer public_key \
      sasl runtime_tools snmp ssl tftp wx xmerl tools

In order to use the `perf.data` produced by this record you need to first call
`perf inject --jit` like this:

    perf inject --jit -i perf.data -o perf.jitted.data

and then you can view an annotated function like this:

    perf annotate -M intel -i perf.jitted.data erl_types:t_has_var/1

or by pressing `a` in the `perf report` ui. Then you get something like this:

![Linux Perf FlameGraph: dialyzer PLT build](figures/beamasm-perf-annotate.png)

`perf annotate` will interleave the listing with the original source code
whenever possible. You can use the `+{source,Filename}` or `+absolute_paths`
compiler options to tell `perf` where to find the source code.

> *WARNING*: Calling `perf inject --jit` will create a lot of files in `/tmp/`
> and in `~/.debug/tmp/`. So make sure to cleanup in those directories from time to
> time or you may run out of inodes.

### perf tips and tricks

You can do a lot of neat things with `perf`. Below is a list of some of the options
we have found useful:

* `perf report --no-children`
    Do not include the accumulation of all children in a call.
* `perf report  --call-graph callee`
    Show the callee rather than the caller when expanding a function call.
* `perf archive`
    Create an archive with all the artifacts needed to inspect the data
    on another host. In early version of perf this command does not work,
    instead you can use [this bash script](https://github.com/torvalds/linux/blob/master/tools/perf/perf-archive.sh).
* `perf report` gives "failed to process sample" and/or "failed to process type: 68"
    This probably means that you are running a bugge version of perf. We have
    seen this when running Ubuntu 18.04 with kernel version 4. If you update
    to Ubuntu 20.04 or use Ubuntu 18.04 with kernel version 5 the problem
    should go away.
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

# Relocatable installation directory

Previously shell scripts (e.g., `erl` and `start`) and the `RELEASES` file
for an Erlang installation depended on a hard coded absolute path to the
installation's root directory. This made it cumbersome to move an
installation to a different directory which can be problematic for platforms
such as Android (#2879) where the
installation directory is unknown at compile time. This is fixed by:

* Changing the shell scripts so that they can dynamically find the
  `ROOTDIR`. The dynamically found `ROOTDIR` is selected if it differs
  from the hard-coded `ROOTDIR` and seems to point to a valid Erlang
  installation. The `dyn_erl` program has been changed so that it can
  return its absolute canonicalized path when given the --realpath
  argument (dyn_erl gets its absolute canonicalized path from the
  realpath POSIX function). The dyn_erl's --realpath
  functionality is used by the scripts to get the root dir dynamically.

* Changing the release_handler module that reads and writes to the
  RELEASES file so that it prepends code:root_dir() whenever it
  encounters relative paths. This is necessary since the current
  working directory can be changed so it is something different than
  `code:root_dir()`.



Misc #

A new DEVELOPMENT HOWTO guide has been added that describes how to build and test Erlang/OTP when fixing bugs or developing new functionality.
Testing has been added to the Github actions run for each opened PR so that more bugs are caught earlier when bug fixes and new features are proposed.

For more details about new features and potential incompatibilities see

    https://erlang.org/download/otp_src_25.0.readme


