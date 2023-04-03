---
layout: post
title: Erlang/OTP 26 Highlights
tags: erlang otp 26 release
author: Björn Gustavsson
---

Erlang/OTP 26 is finally here. This blog post will introduce the new
features that we are most excited about.

A list of all changes is found in [Erlang/OTP 26 Readme](/patches/OTP-26.0).
Or, as always, look at the release notes of the application you are interested in.
For instance: [Erlang/OTP 26 - Erts Release Notes - Version 14.0](https://www.erlang.org/doc/apps/erts/notes.html#erts-14.0).

This year's highlights mentioned in this blog post are:

* [The shell](#the-shell)
* [Improvements of maps](#improvements-of-maps)
* [Improvements of the `lists` module](#improvements-of-the-lists-module)
* [No need to enable feature `maybe` in the runtime system](#no-need-to-enable-feature-maybe-in-the-runtime-system)
* [Improvements in the Erlang compiler and JIT](#improvements-in-the-Erlang-compiler-and-jit)
* [Incremental mode for Dialyzer](#incremental-mode-for-dialyzer)
* [argparse: A command line parser for Erlang](#argparse-a-command-line-parser-for-erlang)
* [SSL: Safer defaults](#ssl-safer-defaults)
* [SSL: Improved checking of options](#ssl-improved-checking-of-options)


# The shell

OTP 26 brings many improvements to the experience of using the Erlang shell.

For example, functions can now be defined directly in the shell:

```
1> factorial(N) -> factorial(N, 1).
ok
2> factorial(N, F) when N > 1 -> factorial(N - 1, F * N);
.. factorial(_, F) -> F.
ok
3> factorial(5).
120
```

The shell prompt changes to `..` when the previous line is not a
complete Erlang construct.

Functions defined in this way are evaluated using the
[erl_eval](https://www.erlang.org/doc/man/erl_eval.html) module, not
compiled by the Erlang compiler. That means that the performance will
not be comparable to compiled Erlang code.

It also possible to define types, specs, and records, making it
possible to paste code from a module directly into the shell for
testing. For example:

```
1> -record(coord, {x=0.0 :: float(), y=0.0 :: float()}).
ok
2> -type coord() :: #coord{}.
ok
3> -spec add(coord(), coord()) -> coord().
ok
4> add(#coord{x=X1, y=Y1}, #coord{x=X2, y=Y2}) ->
..     #coord{x=X1+X2, y=Y1+Y2}.
ok
5> Origin = #coord{}.
#coord{x = 0.0,y = 0.0}
6> add(Origin, #coord{y=10.0}).
#coord{x = 0.0,y = 10.0}
```

The auto-completion feature in the shell has been vastly improved,
supporting auto-completion of variables, record names, record fields
names, map keys, function parameter types, and file names.

For example, instead of typing the variable name `Origin`, I can just
type `O` and press TAB to expand it to `Origin` since the only
variable defined in the shell with the initial letter `O` is
`Origin`. That is a little bit difficult to illustrate in a blog post,
so let's introduce another variable starting with `O`:

```
7> Oxford = #coord{x=51.752022, y=-1.257677}.
#coord{x = 51.752022,y = -1.257677}
```

If I now press `O` and TAB, the shell shows the possible completions:

```
8> O
bindings
Origin    Oxford
```

(The word `bindings` is shown in bold and underlined.)

If I press `x` and TAB, the word is completed to `Oxford`:

```
8> Oxford.
#coord{x = 51.752022,y = -1.257677}
```

To type `#coord{` is is sufficient to type `#` and TAB (because there is
only one record currently defined in the shell):

```
9> #coord{
```

Pressing TAB one more time causes the field names in the record to be
printed:

```
9> #coord{
fields
x=    y=
```

When trying to complete something which has many possible expansions,
the shell attempts to show the most likely completions first.  For
example, if I type `l` and press TAB, the shell shows a list of BIFs
beginning with the letter `l`:

```
10> l
bifs
length(                   link(                     list_to_atom(
list_to_binary(           list_to_bitstring(        list_to_existing_atom(
list_to_float(            list_to_integer(          list_to_pid(
list_to_port(             list_to_ref(              list_to_tuple(
Press tab to see all 37 expansions
```

Pressing TAB again, more BIFs are shown, as well as possible shell commands
and modules:

```
10> l
bifs
length(                   link(                     list_to_atom(
list_to_binary(           list_to_bitstring(        list_to_existing_atom(
list_to_float(            list_to_integer(          list_to_pid(
list_to_port(             list_to_ref(              list_to_tuple(
load_module(
commands
l(     lc(    lm(    ls(
modules
lcnt:                      leex:                      lists:
local_tcp:                 local_udp:                 log_mf_h:
logger:                    logger_backend:            logger_config:
logger_disk_log_h:         logger_filters:            logger_formatter:
logger_h_common:           logger_handler_watcher:    logger_olp:
logger_proxy:              logger_server:             logger_simple_h:
logger_std_h:              logger_sup:
```

Typing `ists:` (to complete the word `lists`) and pressing TAB, a
partial list of functions in the `lists` modules are shown:

```
10> lists:
functions
all(            any(            append(         concat(         delete(
droplast(       dropwhile(      duplicate(      enumerate(      filter(
filtermap(      flatlength(     flatmap(        flatten(        foldl(
foldr(          foreach(        join(           keydelete(      keyfind(
Press tab to see all 72 expansions
```

Typing `m` and pressing TAB, the list of functions is narrowed down to
just those beginning with the letter `m`:

```
10> lists:m
functions
map(            mapfoldl(       mapfoldr(       max(            member(
merge(          merge3(         min(            module_info(
```

## Animations showing shell features

* [Local functions in the shell](https://asciinema.org/a/iLU2CVuH7kOHFLaCxe6GLI2D5)

* [Multi-line editing in the shell](https://asciinema.org/a/iZTr7Wz2HBbDUOikhkplT2VUS)

* [File name and function name completion](https://asciinema.org/a/RmBrWarb1wiUUBg6Rz0Ylqii8)

* [Bindings and records in the shell](https://asciinema.org/a/I2DsfnEaeXijVGiW8aI6YzJNT)


# Improvements of maps

## Changed ordering of atom keys

OTP 25 and earlier releases printed small maps (up to 32 elements)
with atom keys according to the term order of their keys:

```erlang
1> AM = #{a => 1, b => 2, c => 3}.
#{a => 1,b => 2,c => 3}
2> maps:to_list(AM).
[{a,1},{b,2},{c,3}]
```

In OTP 26, as an optimization for certain map operations, such as
`maps:from_list/1`, maps with atom keys are now sorted in a different
order. The new order is undefined and may change between different
invocations of the Erlang VM. On my computer at the time of writing,
I got the following order:

```erlang
1> AM = #{a => 1, b => 2, c => 3}.
#{c => 3,a => 1,b => 2}
2> maps:to_list(AM).
[{c,3},{a,1},{b,2}]
```

There is a new modifier `k` for format strings to specify that maps should
be sorted according to the term order of their keys before printing:

```erlang
3> io:format("~kp\n", [AM]).
#{a => 1,b => 2,c => 3}
ok
```

It is also possible to use a [custom ordering
fun](https://www.erlang.org/doc/man/io.html#format-1).  For example,
to order the map elements in reverse order based on their keys:

```erlang
4> io:format("~Kp\n", [fun(A, B) -> A > B end, AM]).
#{c => 3,b => 2,a => 1}
ok
```

There is also a new
[maps:iterator/2](https://www.erlang.org/doc/man/maps.html#iterator-2)
function that supports iterating over the elements of the map in a more
intuitive order. Examples will be shown in the next section.

## Map comprehensions

In OTP 25 and earlier, it was common to combine `maps:from_list/1` and
`maps:to_list/1` with list comprehensions. For example:

```erlang
1> M = maps:from_list([{I,I*I} || I <- lists:seq(1, 5)]).
#{1 => 1,2 => 4,3 => 9,4 => 16,5 => 25}
```

In OTP 26, that can be written more succinctly with a [**map comprehension**](https://www.erlang.org/doc/reference_manual/expressions.html#comprehensions):

```erlang
1> M = #{I => I*I || I <- lists:seq(1, 5)}.
#{1 => 1,2 => 4,3 => 9,4 => 16,5 => 25}
```

With a **map generator**, a comprehension can now iterate over the
elements of a map. For example:

```erlang
2> [K || K := V <- M, V < 10].
[1,2,3]
```

Using a map comprehension with a map generator, here is an example
showing how keys and values can be swapped:

```erlang
3> #{V => K || K := V <- M}.
#{1 => 1,4 => 2,9 => 3,16 => 4,25 => 5}
```

Map generators accept map iterators as well as maps. Especially useful
are the ordered iterators returned from the new
[maps:iterator/2](https://www.erlang.org/doc/man/maps.html#iterator-2)
function:

```erlang
4> AM = #{a => 1, b => 2, c => 1}.
#{c => 1,a => 1,b => 2}
5> [{K,V} || K := V <- maps:iterator(AM, ordered)].
[{a,1},{b,2},{c,1}]
6> [{K,V} || K := V <- maps:iterator(AM, reversed)].
[{c,1},{b,2},{a,1}]
7> [{K,V} || K := V <- maps:iterator(AM, fun(A, B) -> A > B end)].
[{c,1},{b,2},{a,1}]

```

Map comprehensions were first suggested in [EEP 58](https://www.erlang.org/eeps/eep-0058).

## Inlined `maps:get/3`

In OTP 26, the compiler will inline calls to
[maps:get/3](https://www.erlang.org/doc/man/maps.html#get-3), making them slightly
more efficient.

## Improved `maps:merge/2`

When merging two maps, the
[maps:merge/2](https://www.erlang.org/doc/man/maps.html#merge-2)
function will now try to reuse the [key
tuple](https://www.erlang.org/doc/efficiency_guide/maps.html#how-small-maps-are-implemented)
from one of the maps in order to reduce the memory usage for maps.

For example:

```erlang
1> maps:merge(#{x => 13, y => 99, z => 100}, #{x => 0, z => -7}).
#{y => 99,x => 0,z => -7}
```

The resulting map has the same three keys as the first map, so it can reuse the
key tuple from the first map.

This optimization is not possible if one of the maps has any key not present
in the other map. For example:

```erlang
2> maps:merge(#{x => 1000}, #{y => 2000}).
#{y => 2000,x => 1000}
```

## Improved map updates

Updating of a map using the `=>` operator has been improved to avoid
updates that don't change the value of the map or its [key
tuple](https://www.erlang.org/doc/efficiency_guide/maps.html#how-small-maps-are-implemented).
For example:

```erlang
1> M = #{a => 42}.
#{a => 42}
2> M#{a => 42}.
#{a => 42}
```

The update operation does not change the value of the map, so in order
to save memory, the original map is returned.

(A [similar optimization for the `:=`
operator](https://github.com/erlang/otp/pull/1889) was implemented 5
years ago.)

When updating the values of keys that already exist in a map using the
`=>` operator, the key tuple will now be re-used. For example:

```erlang
3> M#{a => 100}.
#{a => 100}
```

## The pull requests for map improvements

For anyone who wants to dig deeper, here are the main pull requests
for maps for OTP 26:

* [Implement map comprehensions (EEP-58)](https://github.com/erlang/otp/pull/6727)
* [Use in-memory atom ordering for map ordering](https://github.com/erlang/otp/pull/6151)
* [Add maps:iterator/2 with ~k and ~K format options for printing ordered maps](https://github.com/erlang/otp/pull/6718)
* [sys_core_fold: Inline maps:get/3](https://github.com/erlang/otp/pull/7003)
* [Optimize maps:merge/2 of small maps](https://github.com/erlang/otp/pull/7004)
* [Inline creation of small maps with literal keys](https://github.com/erlang/otp/pull/6267)
* [Enhance creation of maps with literal keys](https://github.com/erlang/otp/pull/6178)
* [Do not allocate a new map when the value is the same encore](https://github.com/erlang/otp/pull/6657)


# Improvements of the `lists` module

## New function `lists:enumerate/3`

In OTP 25, [lists_enumerate()](https://erlang.org/doc/man/lists.html#enumerate-1)
was introduced. For example:

```erlang
1> lists:enumerate([a,b,c]).
[{1,a},{2,b},{3,c}]
2> lists:enumerate(0, [a,b,c]).
[{0,a},{1,b},{2,c}]
```

In OTP 26, [lists:enumerate/3](https://erlang.org/doc/man/lists.html#enumerate-3)
completes the family of functions by allowing an increment to be specified:

```erlang
3> lists:enumerate(0, 10, [a,b,c]).
[{0,a},{10,b},{20,c}]
4> lists:enumerate(0, -1, [a,b,c]).
[{0,a},{-1,b},{-2,c}]
```

## New options for the `zip` family of functions

The `zip` family of functions in the `lists` module combines two or three lists
into a single list of tuples. For example:

```erlang
1> lists:zip([a,b,c], [1,2,3]).
[{a,1},{b,2},{c,3}]

```

The existing `zip` functions fail if the lists don't have the same length:

```erlang
2> lists:zip([a,b,c,d], [1,2,3]).
** exception error: no function clause matching . . .
```

In OTP 26, the [`zip`
functions](https://www.erlang.org/doc/man/lists.html#zip-2) now take
an extra `How` parameter that determines what should happen when the
lists are of unequal length.

For some use cases for `zip`, ignoring the superfluous elements in the
longer list or lists can make sense. That can be done using the `trim`
option:

```erlang
3> lists:zip([a,b,c,d], [1,2,3], trim).
[{a,1},{b,2},{c,3}]
```

For other use cases it could make more sense to extend the shorter
list or lists to the length of the longest list. That can be done
using the `{pad, Defaults}` option, where `Defaults` should be a tuple
having the same number of elements as the number of lists. For
`lists:zip/3`, that means that the `Defaults` tuple should have two
elements:

```erlang
4> lists:zip([a,b,c,d], [1,2,3], {pad, {zzz, 999}}).
[{a,1},{b,2},{c,3},{d,999}]
5> lists:zip([a,b,c], [1,2,3,4,5], {pad, {zzz, 999}}).
[{a,1},{b,2},{c,3},{zzz,4},{zzz,5}]
```

For `lists:zip3/3` the `Defaults` tuple should have three elements:

```erlang
6> lists:zip3([], [a], [1,2,3], {pad, {0.0, zzz, 999}}).
[{0.0,a,1},{0.0,zzz,2},{0.0,zzz,3}]
```

# No need to enable feature `maybe` in the runtime system

In OTP 25, the [feature
concept](https://www.erlang.org/doc/reference_manual/features.html)
and the [maybe
feature](https://www.erlang.org/doc/reference_manual/expressions.html#maybe)
were introduced. In order to use `maybe` in OTP 25, it is necessary to
enable it in both the compiler and the runtime system. For example:

{% raw %}

```
$ cat t.erl
-module(t).
-feature(maybe_expr, enable).
-export([listen_port/2]).
listen_port(Port, Options) ->
    maybe
        {ok, ListenSocket} ?= inet_tcp:listen(Port, Options),
        {ok, Address} ?= inet:sockname(ListenSocket),
        {ok, {ListenSocket, Address}}
    end.
$ erlc t.erl
$ erl
Erlang/OTP 25 . . .

Eshell V13.1.1  (abort with ^G)
1> t:listen_port(50000, []).
=ERROR REPORT==== 6-Apr-2023::12:01:20.373223 ===
Loading of . . ./t.beam failed: {features_not_allowed,
                                 [maybe_expr]}

** exception error: undefined function t:listen_port/2
2> q().
$ erl -enable-feature maybe_expr
Erlang/OTP 25 . . .

Eshell V13.1.1  (abort with ^G)
1> t:listen_port(50000, []).
{ok,{#Port<0.5>,{{0,0,0,0},50000}}}
```
{% endraw %}

In OTP 26, it is no longer necessary to enable a feature in the
runtime system in order to load modules that are using it.
It is sufficient to have `-feature(maybe_expr, enable).` in the module.

For example:

{% raw %}

```
$ erlc t.erl
$ erl
Erlang/OTP 26 . . .

Eshell V14.0 (press Ctrl+G to abort, type help(). for help)
1> t:listen_port(50000, []).
{ok,{#Port<0.4>,{{0,0,0,0},50000}}}
```

{% endraw %}

# Improvements in the Erlang compiler and JIT

OTP 26 improves on the type-based optimizations in the JIT introduced
last year, but the most noticable improvements are for matching and
construction of binaries using the bit syntax. Those improvements,
combined with changes to the `base64` module itself, makes encoding to
Base64 about 4 times faster and decoding from Base64 more than 3
times faster.

More details about these improvements can be found in the blog post
[More Optimizations in the Compiler and JIT](https://www.erlang.org/blog/more-optimizations).

Worth mentioning here is also the re-introduction of an optimization
that was lost when the JIT was introduced in OTP 24:

[erts: Reintroduce literal fun optimization](https://github.com/erlang/otp/pull/6963)

It turns out that this optimization is important for the
[jason](https://github.com/michalmuskala/jason) library. Without it,
[JSON decoding is 10 percent
slower](https://github.com/michalmuskala/jason/pull/161).


# Incremental mode for Dialyzer

Dialyzer has a new incremental mode implemented by Tom Davies. The
incremental mode can greatly speed up the analysis when only small
changes have been done to a code base.

Let's jump straight into an example. Assuming that we want to prepare
a pull request for the `stdlib` application, here is how we can use Dialyzer's
incremental mode to show warnings for any issues in `stdlib`:


```
$ dialyzer --incremental --apps erts kernel stdlib compiler crypto --warning_apps stdlib
Proceeding with incremental analysis... done in 0m14.91s
done (passed successfully)
```

Let's break down the command line:

* The `--incremental` option tells Dialyzer to use the incremental mode.

* The `--warning_apps stdlib` lists the application that we want
warnings for. In this case, it's the `stdlib` application.

* The `--apps erts kernel stdlib compiler crypto` option lists the
applications that should be analyzed, but without generating any
warnings.

Dialyzer analyzed all modules given for the `--apps` and
`--warning_apps` options. On my computer, the analysis finished in
about 15 seconds.

If I immediately run Dialyzer with the same arguments, it finishes pretty much
instantaneously because nothing has been changed:

```
$ dialyzer --incremental --warning_apps stdlib --apps erts kernel stdlib compiler crypto
done (passed successfully)
```

If I do any change to the `lists` module (for example, by adding a new
function), Dialyzer will re-analyze all modules that depend on it
directly or indirectly:

```
$ dialyzer --incremental --warning_apps stdlib --apps erts kernel stdlib compiler crypto
There have been changes to analyze
    Of the 270 files being tracked, 1 have been changed or removed,
    resulting in 270 requiring analysis because they depend on those changes
Proceeding with incremental analysis... done in 0m14.95s
done (passed successfully)
```

It turns out that all modules in the analyzed applications depend on
the `lists` module directly or indirectly.

If I change something in the `base64` module, the re-analysis will be
much quicker because there are fewer dependencies:

```
$ dialyzer --incremental --warning_apps stdlib --apps erts kernel stdlib compiler crypto
There have been changes to analyze
    Of the 270 files being tracked, 1 have been changed or removed,
    resulting in 3 requiring analysis because they depend on those changes
Proceeding with incremental analysis... done in 0m1.07s
done (passed successfully)
```

In this case only three modules needed to be re-analyzed, which was
done in about one second.

## Using the dialyzer.config file

Note that all of the examples above used the same command line.

When running Dialyzer in the incremental mode, the list of
applications to be analyzed and the list of applications to produce
warnings for must be supplied every time Dialyzer is invoked.

To avoid having to supply the application lists on the command line,
they can be put into a configuration file named `dialyzer.config`.
To find out in which directory Dialyzer will look for the configuration
file, run the following command:

```
$ dialyzer --help
  .
  .
  .
Configuration file:
     Dialyzer's configuration file may also be used to augment the default
     options and those given directly to the Dialyzer command. It is commonly
     used to avoid repeating options which would otherwise need to be given
     explicitly to Dialyzer on every invocation.

     The location of the configuration file can be set via the
     DIALYZER_CONFIG environment variable, and defaults to
     within the user_config location given by filename:basedir/3.

     On your system, the location is currently configured as:
       /Users/bjorng/Library/Application Support/erlang/dialyzer.config

     An example configuration file's contents might be:

       {incremental,
         {default_apps,[stdlib,kernel,erts]},
         {default_warning_apps,[stdlib]}
       }.
       {warnings, [no_improper_lists]}.
       {add_pathsa,["/users/samwise/potatoes/ebin"]}.
       {add_pathsz,["/users/smeagol/fish/ebin"]}.

  .
  .
  .

```

Near the end there is information about the configuration file and where Dialyzer
will look for it.

To shorten the command line for our previous examples, the following term can
be stored in the `dialyzer.config`:

```
{incremental,
 {default_apps, [erts,kernel,stdlib,compiler,crypto]},
 {default_warning_apps, [stdlib]}
}.
```

Now it is sufficient to just give the `--incremental` option to Dialyzer:

```
$ dialyzer --incremental
done (passed successfully)
```

## Running Dialyzer on proper

As a final example, let's run Dialyzer on
[PropER](https://github.com/proper-testing/proper/).

To do that, the `default_warnings_apps` option in the configuration
file must be changed to `proper`. It is also necessary to add the
`add_pathsa` option to prepend the path of the `proper` application to
the code path:

```
{incremental,
 {default_apps, [erts,kernel,stdlib,compiler,crypto]},
 {default_warning_apps, [proper]}
}.
{add_pathsa, ["/Users/bjorng/git/proper/_build/default/lib/proper"]}.
```

Running Dialyzer:

```
$ dialyzer --incremental
There have been changes to analyze
    Of the 296 files being tracked,
    26 have been changed or removed,
    resulting in 26 requiring analysis because they depend on those changes
Proceeding with incremental analysis...
proper.erl:2417:13: Unknown function cover:start/1
proper.erl:2426:13: Unknown function cover:stop/1
proper_symb.erl:249:9: Unknown function erl_syntax:atom/1
proper_symb.erl:250:5: Unknown function erl_syntax:revert/1
proper_symb.erl:250:23: Unknown function erl_syntax:application/3
proper_symb.erl:257:51: Unknown function erl_syntax:nil/0
proper_symb.erl:259:49: Unknown function erl_syntax:cons/2
proper_symb.erl:262:5: Unknown function erl_syntax:revert/1
proper_symb.erl:262:23: Unknown function erl_syntax:tuple/1
 done in 0m2.36s
done (warnings were emitted)
```

Dialyzer found 26 new files to analyze (the BEAM files in the `proper` application).
Those were analyzed in about two and a half seconds.

Dialyzer emitted warnings for unknown functions because `proper` calls
functions in applications that were not being analyzed. To eliminate those warnings,
the `tools` and `syntax_tools` applications can be added to the list of applications
in the list of `default_apps`:

```
{incremental,
 {default_apps, [erts,kernel,stdlib,compiler,crypto,tools,syntax_tools]},
 {default_warning_apps, [proper]}
}.
{add_pathsa, ["/Users/bjorng/git/proper/_build/default/lib/proper"]}.
```

With that change to the configuration file, no more warnings are printed:

```
$ dialyzer --incremental
There have been changes to analyze
    Of the 319 files being tracked,
    23 have been changed or removed,
    resulting in 38 requiring analysis because they depend on those changes
Proceeding with incremental analysis... done in 0m6.47s
```

It is also possible to include warning options in the configuration
file, for example to disable warnings for non-proper lists or to enable
warnings for unmatched returns. Let's enable warnings for unmatched
returns:

```
{incremental,
 {default_apps, [erts,kernel,stdlib,compiler,crypto,tools,syntax_tools]},
 {default_warning_apps, [proper]}
}.
{warnings, [unmatched_returns]}.
{add_pathsa, ["/Users/bjorng/git/proper/_build/default/lib/proper"]}.
```

When warnings options are changed, Dialyzer will re-analyze all modules:

```
$ dialyzer --incremental
PLT was built for a different set of enabled warnings,
so an analysis must be run for 319 modules to rebuild it
Proceeding with incremental analysis... done in 0m19.43s
done (passed successfully)
```

## Pull request

[dialyzer: Add incremental analysis mode](https://github.com/erlang/otp/pull/5997)

# argparse: A command line parser for Erlang

New in OTP 26 is the
[argparse](https://www.erlang.org/doc/man/argparse.html) module, which
simplifies parsing of the command line in
[escripts](https://www.erlang.org/doc/man/escript.html).  `argparse`
was implemented by Maxim Fedorov.

To show only a few of the features, let's implement the command-line
parsing for an escript called `ehead`, inspired by the Unix command
[head](https://en.wikipedia.org/wiki/Head_(Unix)):

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-

main(Args) ->
    argparse:run(Args, cli(), #{progname => ehead}).

cli() ->
    #{
      arguments =>
          [#{name => lines, type => {integer, [{min, 1}]},
             short => $n, long => "-lines", default => 10,
             help => "number of lines to print"},
           #{name => files, nargs => nonempty_list, action => extend,
             help => "lists of files"}],
      handler => fun(Args) ->
                         io:format("~p\n", [Args])
                 end
     }.
```

As currently written, the `ehead` script will simply print the
arguments collected by `argparse` and quit.

If `ehead` is run without any arguments an error message will be
shown:

```
$ ehead
error: ehead: required argument missing: files
Usage:
  ehead [-n <lines>] [--lines <lines>] <files>...

Arguments:
  files       lists of files

Optional arguments:
  -n, --lines number of lines to print (int >= 1, 10)
```

The message tells us that at least one file name must be given:

```
$ ehead foo bar baz
#{lines => 10,files => ["foo","bar","baz"]}
```

Since the command line was valid, `argparse` collected the arguments
into a map, which was then printed by the `handler` fun.

The number of lines to be printed from each file defaults to `10`, but
can be changed using either the `-n` or `--lines` option:

```
$ ehead -n 42 foo bar baz
#{lines => 42,files => ["foo","bar","baz"]}
$ ehead foo --lines=42 bar baz
#{lines => 42,files => ["foo","bar","baz"]}
$ ehead --lines 42 foo bar baz
#{lines => 42,files => ["foo","bar","baz"]}
$ ehead foo bar --lines 42 baz
#{lines => 42,files => ["foo","bar","baz"]}
```

Attempting to give the number of lines as `0` results in an error message:

```
$ ehead -n 0 foobar
error: ehead: invalid argument for lines: 0 is less than accepted minimum
Usage:
  ehead [-n <lines>] [--lines <lines>] <files>...

Arguments:
  files       lists of files

Optional arguments:
  -n, --lines number of lines to print (int >= 1, 10)
```

## Pull request

[[argparse] Command line parser for Erlang](https://github.com/erlang/otp/pull/6852)


# SSL: Safer defaults

In OTP 25, the default options for
[ssl:connect/3](https://www.erlang.org/doc/man/ssl.html#connect-3)
would allow setting up a connection without verifying the
authenticity of the server (that is, without checking the server's
certificate chain). For example:

```erlang
Erlang/OTP 25 . . .

Eshell V13.1.1  (abort with ^G)
1> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
2> ssl:connect("www.erlang.org", 443, []).
=WARNING REPORT==== 6-Apr-2023::12:29:20.824457 ===
Description: "Authenticity is not established by certificate path validation"
     Reason: "Option {verify, verify_peer} and cacertfile/cacerts is missing"

{ok,{sslsocket,{gen_tcp,#Port<0.6>,tls_connection,undefined},
               [<0.122.0>,<0.121.0>]}}
```

A warning report would be generated, but a connection would be set up.

In OTP 26, the default value for the `verify` option is now
`verify_peer` instead of `verify_none`. Host verification
requires trusted CA certificates to be supplied using one of the options
`cacerts` or `cacertsfile`. Therefore, a connection attempt with an empty
option list will fail in OTP 26:

```erlang
Erlang/OTP 26 . . .

Eshell V14.0 (press Ctrl+G to abort, type help(). for help)
1> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
2> ssl:connect("www.erlang.org", 443, []).
{error,{options,incompatible,
                [{verify,verify_peer},{cacerts,undefined}]}}
```

The default value for the `cacerts` option is `undefined`,
which is not compatible with the `{verify,verify_peer}` option.

To make the connection succeed, the recommended way is to
use the `cacerts` option to supply CA certificates to be used
for verifying. For example:

```erlang
1> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
2> ssl:connect("www.erlang.org", 443, [{cacerts, public_key:cacerts_get()}]).
{ok,{sslsocket,{gen_tcp,#Port<0.5>,tls_connection,undefined},
               [<0.137.0>,<0.136.0>]}}
```

Alternatively, host verification can be explicitly disabled. For example:

```erlang
1> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
2> ssl:connect("www.erlang.org", 443, [{verify,verify_none}]).
{ok,{sslsocket,{gen_tcp,#Port<0.6>,tls_connection,undefined},
               [<0.143.0>,<0.142.0>]}}
```

Another way that OTP 26 is safer is that legacy algorithms such as SHA1 and
DSA are no longer allowed by default.

# SSL: Improved checking of options

In OTP 26, the checking of options is strengthened to return errors
for incorrect options that used to be silently ignored. For example,
`ssl` now rejects the `fail_if_no_peer_cert` option if used for the
client:

```erlang
1> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
2> ssl:connect("www.erlang.org", 443, [{fail_if_no_peer_cert, true}, {verify, verify_peer}, {cacerts, public_key:cacerts_get()}]).
{error,{option,server_only,fail_if_no_peer_cert}}
```

In OTP 25, the option would be silently ignored.


`ssl` in OTP 26 also returns clearer error reasons. In the example in
the previous section the following connection attempt was shown:

```erlang
2> ssl:connect("www.erlang.org", 443, []).
{error,{options,incompatible,
                [{verify,verify_peer},{cacerts,undefined}]}}
```

In OTP 25, the corresponding error return is less clear:

```erlang
2> ssl:connect("www.erlang.org", 443, [{verify,verify_peer}]).
{error,{options,{cacertfile,[]}}}
```
