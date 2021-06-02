---
layout: post
title: Core Erlang Wrap Up
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post wraps up the exploration of Core Erlang started in the
previous two blog posts. The remaining default Core Erlang
passes are described, followed by a look at how Core Erlang is
represented internally in the compiler.

Here are the Core Erlang passes that will be run when using
OTP 21 RC1 or the `master` branch in the git repository:

```
$ erlc +time core_wrapup.erl
Compiling "core_wrapup"
     .
     .
     .
 core                          :      0.000 s      15.7 kB
 sys_core_fold                 :      0.000 s       9.0 kB
 sys_core_alias                :      0.000 s       9.0 kB
 core_transforms               :      0.000 s       9.0 kB
 sys_core_bsm                  :      0.000 s       9.0 kB
 sys_core_dsetel               :      0.000 s       9.0 kB
     .
     .
     .
```

We have covered `core` and `sys_core_fold` in the two previous
blog posts about Core Erlang.

## The other Core Erlang passes

### sys_core_alias

In the upcoming OTP 21 release, there is a new `sys_core_alias` pass
contributed by [José Valim](https://github.com/josevalim).

The purpose of the pass is to avoid rebuilding terms that
have been matched, such as in this example:

```erlang
remove_even([{Key,Val}|T]) ->
    case Val rem 2 =:= 0 of
        true -> remove_even(T);
        false ->  [{Key,Val}|remove_even(T)]
    end;
remove_even([]) -> [].
```

In the function head, the pattern `{Key,Val}` binds two elements of a
tuple to the variables `Key` and `Val`, but the original tuple is not
captured. In the `false` clause of the `case`, a new tuple will be
constructed from `Key` and `Val`.

It is possible to avoid creating a new tuple by using the `=` operator
to bind the complete tuple to a variable:

```erlang
remove_even([{Key,Val}=Tuple|T]) ->
    case Val rem 2 =:= 0 of
        true -> remove_even(T);
        false ->  [Tuple|remove_even(T)]
    end;
remove_even([]) -> [].
```

Essentially, the new `sys_core_alias` pass does that transformation
automatically. Here is the Core Erlang code before applying this
optimization:

```
'remove_even'/1 =
    fun (_0) ->
	case _0 of
	  <[{Key,Val}|T]> when 'true' ->
	      let <_1> =
		  call
		       'erlang':'rem'(Val, 2)
	      in
		  case <> of
		    <>
			when call 'erlang':'=:='(_1, 0) ->
			    apply 'remove_even'/1(T)
		    <> when 'true' ->
			let <_2> =
			    apply 'remove_even'/1(T)
			in
                            [{Key,Val}|_2]      % BUILDING TUPLE
		  end
	  <[]> when 'true' ->
	      []
	  <_4> when 'true' ->
		primop 'match_fail'({'function_clause',_4})
	end
```

Here is the code after running the `sys_core_alias` pass:

```
'remove_even'/1 =
    fun (_0) ->
	case _0 of
	  <[_@r0 = {Key,Val}|T]> when 'true' ->
	      let <_1> =
		  call 'erlang':'rem'(Val, 2)
	      in
		  case <> of
		    <>
			when call 'erlang':'=:='(_1, 0) ->
			    apply 'remove_even'/1(T)
		    <> when 'true' ->
			let <_2> =
			    apply 'remove_even'/1(T)
			in
			    [_@r0|_2]          % REUSING EXISTING TUPLE
		  end
	  <[]> when 'true' ->
	      []
	  <_4> when 'true' ->
		primop 'match_fail'({'function_clause',_4})
	end
```

### core_transforms

Similar to parse transforms, the `core_transforms` pass makes it possible to
add compiler passes that transform the Core Erlang code without modifying
the compiler.

As an example, here is a simple core transform module:

```erlang
-module(my_core_transform).
-export([core_transform/2]).

core_transform(Core, _Options) ->
    Module = cerl:concrete(cerl:module_name(Core)),
    io:format("Module name: ~p\n", [Module]),
    io:format("Number of nodes in Core Erlang tree: ~p\n",
              [cerl_trees:size(Core)]),
    Core.
```

Before explaining the code, let's see it in action:

```
$ erlc my_core_transform
$ erlc -pa . '+{core_transform,my_core_transform}' core_wrapup.erl
Module name: core_wrapup
Number of nodes in Core Erlang tree: 220
$
```

The `{core_transform,Name}` option instructs the compiler to run a
core transformation. In this case, the core transform module is
`my_core_transform`. After doing the standard optimizing passes,
the compiler will call `my_core_transform:core_transform/2`, passing
the Core Erlang code as the first argument and the compiler options
as the second argument.

The first line in the `core_transform/2` functions calls
`cerl:module_name(Core)` to retrieve the module name. The return value
of `cerl:module_name/1` is a record representing any literal term. To
retrieve the actual term (an atom in this case), `cerl:concrete/1`
is called.

In the second `io:format/2` call, we call `cerl_trees:size/1` to
count the number of nodes in the tree that represents the Core Erlang
code for the module.

This core transform does not do any real transforming, since the last
line returns the Core Erlang code without any modifications.

### sys_core_bsm

`sys_core_bsm` is the first of three passes that implement the delayed
sub binary optimization described in the [Efficiency
Guide][efficiency_guide].  `sys_core_bsm` adds annotations that are
later used by `v3_codegen` and `beam_bsm` to optimize matching of
binaries.

[efficiency_guide]: http://erlang.org/doc/efficiency_guide/binaryhandling.html

### sys_core_dsetel

The `sys_core_dsetel` pass will optimize chained or nested
applications of `setelement/3` as in this example:

```erlang
update_tuple(T0) ->
    T = setelement(3, T0, y),
    setelement(2, T, x).
```

Translated to Core Erlang it looks like this:

```
'update_tuple'/1 =
    fun (_0) ->
	let <T> =
	    call 'erlang':'setelement'(3, _0, 'y')
	in
	    call 'erlang':'setelement'(2, T, 'x')
```

The `sys_core_dsetel` pass replaces the second call to `setelement/3`
with the primop `dsetelement/3`, which destructively updates a tuple:

```
'update_tuple'/1 =
    fun (_0) ->
	let <T> =
	    call 'erlang':'setelement'(3, _0, 'y')
	in  do
		primop 'dsetelement'(2, T, 'x')
		T
```

`do` evalutes two expressions in sequence, ignoring the value of the
first expression. It is used here because the primop `dsetelement/3`
updates its tuple argument without returning a value.

The `sys_core_dsetel` pass is intentionally run as the very last
Core Erlang pass. Doing other optimizations might render the optimization
unsafe. For example, there must not occur a garbage collection between
the call to `setelement/3` and `dsetelement/3`.

Why is this optimization useful? Surely a sequence of `setelement/3`
calls must be rare?

Consider this function that updates two elements in a record:

```
-record(rec, {a,b,c,d,e,f,g,h}).

update_record(R) ->
    R#rec{a=x,b=y}.
```

In [a previous blog post][lost in translation] we saw that the `-E` option
will produce an `.E` file with the code after records have been translated
to tuple operations:

[lost in translation]: http://blog.erlang.org/compiler-lost-in-translation/

```
$ erlc -E core_wrapup.erl
```

Here is the code for `update_record/1` after record translation:

```
update_record(R) ->
    begin
        rec0 = R,
        case rec0 of
            {rec,_,_,_,_,_,_,_,_} ->
                setelement(2, setelement(3, rec0, y), x);
            _ ->
                error({badrecord,rec})
        end
    end.
```

After verifying that `R` is indeed a record of the correct type (that is,
that the size and first element of the tuple are correct), nested calls
to `setelement/3` is used to update two elements of the tuple.

The optimized Core Erlang code for `update_record/1` will look like
this:

```
'update_record'/1 =
    fun (_0) ->
	case _0 of
	  <{'rec',_5,_6,_7,_8,_9,_10,_11,_12}> when 'true' ->
	      let <_2> =
		  call 'erlang':'setelement'(3, _0, 'y')
	      in  do  primop 'dsetelement'(2, _2, 'x')
		      _2
	  <_13> when 'true' ->
		call 'erlang':'error'({'badrecord','rec'})
	end
```

## The representation of Core Erlang code

So far we have looked at the external (pretty-printed) representation of
Core Erlang. Before leaving Core Erlang, we will take a brief look at
the internal representation of Core Erlang that the compiler uses.

There are ~~two~~ three ways to work with Core Erlang within an
optimizer pass:

* Using the API functions in the `cerl` module

* Using the `c_*` records defined in `core_parse.hrl`

* Mixing use of records with use of the API functions

### Using the cerl module and friends

The `cerl` module provides API functions to construct, deconstruct, update,
and query each of the constructs in Core Erlang.

Here are some examples:

* `cerl:c_var(Name)` constructs the Core Erlang representation
of a variable with the name `Name`.

* `cerl:is_c_var(Core)` returns `true` if the `Core` represents a Core
Erlang variable, and `false` otherwise.

* `cerl:var_name(Core)` returns the name of a variable (and crashes if
`Core` does not represent a Core Erlang variable).

There are also the `cerl_trees` and `cerl_clauses` modules that provide
useful utility functions for manipulating Core Erlang code.

### Using the records

In `core_parse.hrl`, there is one record for each kind of Core Erlang
construct. All record names start with the prefix `c_`.

For example, the record `#c_var{}` represents a variable, the record
`#c_call{}` the `call` expression, the record `c_tuple{}` a tuple, and
so on.

As a complete example, we can rewrite our previous core transform
to use record matching instead of `cerl` to retrieve the module name:

```
-module(my_core_transform).
-export([core_transform/2]).

-include_lib("compiler/src/core_parse.hrl").

core_transform(Core, _Options) ->
    #c_module{name=#c_literal{val=Module}} = Core,
    io:format("Module name: ~p\n", [Module]),
    io:format("Number of nodes in Core Erlang tree: ~p\n",
              [cerl_trees:size(Core)]),
    Core.
```

### Mixing the `cerl` API with records

The `cerl` module internally use the records in `core_parse.hrl`, so
the two approaches can be mixed. For example, `sys_core_fold` mostly
use the records, but sometimes uses `cerl` when it is more convenient.

## Wrapping up the wrap up

It seems that there is enough material for several more blog posts
about Core Erlang. For instance, I haven't even mentioned the inliners
(not a typo, there are *two* inliners). That means that there might be
more blog posts about Core Erlang in the future.

But in the very near future, it is time to explore the compiler passes
that follow Core Erlang, and perhaps answer the eternal question about
the `v3_` prefix. Was there ever a `v2_kernel` (spoiler: yes) or a
`v1_kernel` (spoiler: no)?
