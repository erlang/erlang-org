---
layout: post
title: Core Erlang Optimizations
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post continues the exploration of Core Erlang by
looking at some optimizations done by the `sys_core_fold`
compiler pass. The Core Erlang language was introduced in
the [previous blog post](http://blog.erlang.org/core-erlang-by-example/).

To prepare the examples in this blog post I used two
commands.

```
$ erlc +time +dcore core_fold_example.erl
Compiling "core_fold_example"
 parse_module                  :      0.000 s       9.4 kB
 transform_module              :      0.000 s       9.4 kB
 lint_module                   :      0.005 s       9.4 kB
 expand_records                :      0.000 s       9.4 kB
 core                          :      0.000 s      59.3 kB
 listing                       :      0.003 s      59.3 kB
```

The `dcore` option produces the file `core_fold_example.core`
containing a listing of the Core Erlang code produced by the `core`
parse (implemented by the module `v3_core`).

```
$ erlc +time +dcopt core_fold_example.erl
Compiling "core_fold_example"
 parse_module                  :      0.000 s       9.4 kB
 transform_module              :      0.000 s       9.4 kB
 lint_module                   :      0.002 s       9.4 kB
 expand_records                :      0.000 s       9.4 kB
 core                          :      0.000 s      59.3 kB
 sys_core_fold                 :      0.000 s      25.3 kB
 core_transforms               :      0.000 s      25.3 kB
 listing                       :      0.002 s      25.3 kB
```

The `dcopt` option produces the file `core_fold_example.copt`
containing a listing of the Core Erlang code as it looks
after optimization by the `sys_core_fold` pass.

As was mentioned in my first blog post about the compiler,
`compile:options()` will print most of the hidden options for
the compiler.

## The most basic optimization

The most basic optimization done by `sys_core_fold` is constant propagation.

Consider this Erlang function:

```erlang
a() ->
    A = 42,
    {ok,A}.
```

It can be translated to Core Erlang like this:

```
'a'/0 =
    fun () ->
       let <A> = 42
       in {'ok',A}
```

The variable `A` is bound to a constant (as opposed to an expression such
as function call). We can replace all occurrences of the variable `A` with
the constant value `42` and eliminate the `let`:

```
'a'/0 =
    fun () ->
	{'ok',42}
```

## Optimizing `case` expressions

Actually, the first version of `a/0` that I showed was already
slightly optimized by me.

Here is the actual Core Erlang code (only slightly edited to
remove annotations and unnecessary line breaks):

```
'a'/0 =
    fun () ->
        case <> of
	  <> when 'true' ->
	      let <A> = 42
	      in {'ok',A}
	  <> when 'true' ->
		primop 'match_fail'({'function_clause'})
	end
```

The `let` has been wrapped in a useless outer `case`. The
`case` would serve some purpose if there had been some function
arguments, but why complicate the code generator if `sys_core_fold` is
perfectly capable of simplifying this code?

`sys_core_fold` will simplify the code in several steps.

First it will look at each clause. If a clause can't possibly
be executed (for example, it its guard is `false`) it will be
dropped. If a clause will always match, all clauses following
the clause will be dropped.

In this case, the first clause will always match, because the
pattern is a list of no variables that can't fail to match, and
the guard is `true`. Thus the second clause is unreachable and
is dropped:


```
'a'/0 =
    fun () ->
        case <> of
	  <> when 'true' ->
	      let <A> = 42
	      in {'ok',A}
	end
```

The next step is to see if there is only one clause remaining.
If it is, the body of the clause can be kept and the `case`
eliminated:


```
'a'/0 =
    fun () ->
       let <A> = 42
       in {'ok',A}
```

## Another case example

Let's see how a more complicated function can be optimized
following the steps just described. Consider this Erlang
function:

```erlang
aa() ->
    case {a,tuple} of
	[List] -> List;
	{A,B} ->  {tuple,A,B};
	_ ->      something_else
    end.
```

Translated to Core Erlang code (with the outer `case` and
annotations removed) it will look this:

```
'aa'/0 =
    fun () ->
      case {'a','tuple'} of
	<[List|[]]> when 'true' ->
	    List
	<{A,B}> when 'true' ->
	    {'tuple',A,B}
	<_@c1> when 'true' ->
	    'something_else'
	<_@c0> when 'true' ->
	    primop 'match_fail'({'case_clause',_@c0})
      end
```

Let's go through the clauses one by one:

* The first clause will only match a list with exactly one element.
The `case` expression is a tuple, so the first clause can't
possibly match. It will be dropped.

* The second clause will match a tuple with (any) two elements.
The case expression is a tuple with two elements, so this clause
will always match.

* There is no need to look at the remaining clauses, since the
second clause will always match. The remaining clauses are dropped.

We now have:

```
'aa'/0 =
    fun () ->
      case {'a','tuple'} of
	<{A,B}> when 'true' ->
	    {'tuple',A,B}
      end
```

This is a `case` with just one clause, so we can keep
the body of the clause and remove the `case`. But there is
a problem if we do that naively:

```
'aa'/0 =
    fun () ->
       {'tuple',A,B}
```

The variables `A` and `B` are used, but they don't have
any values bound to them. We must use a `let` to bind
the variables before they can be used:

```
'aa'/0 =
    fun () ->
      let <A,B> = <'a','tuple'>
      in {'tuple',A,B}
```

Propagating constants, the final code is:

```
'aa'/0 =
    fun () ->
	{'tuple','a','tuple'}
```

## Avoiding tuple building

Here is an example of a common pattern of matching
several expressions in parallel:

```erlang
b(A, B) ->
    case {A,B} of
	{true,false} -> ok;
	{false,true} -> not_ok;
	{_,_} -> error
    end.
```

The unoptimized Core Erlang code looks like this:

```
'b'/2 =
    fun (_@c1,_@c0) ->
	case <_@c1,_@c0> of
	  <A,B> when 'true' ->
	      case {A,B} of
		<{'true','false'}> when 'true' ->
		    'ok'
		<{'false','true'}> when 'true' ->
		    'not_ok'
		<{_@c5,_@c6}> when 'true' ->
		    'error'
		<_@c2> when 'true' ->
		      primop 'match_fail'({'case_clause',_@c2})
	      end
	end
```

The `case` expression is `{A,B}`. When executing the `case`
a tuple will built, and then almost immediately discarded.
That is wasteful. Therefore `sys_core_fold` rewrites the
code to eliminate the tuple building:

```
'b'/2 =
    fun (_@c1,_@c0) ->
	case <_@c1,_@c0> of
	  <'true','false'> when 'true' ->
	      'ok'
	  <'false','true'> when 'true' ->
	      'not_ok'
	  <_@c5,_@c6> when 'true' ->
	      'error'
	end
```

Here a value list is used instead of a tuple. (See
[previous blog post](http://blog.erlang.org/core-erlang-by-example/)
for several examples of value lists.)

Another common pattern where tuples are built and immediately
discarded is shown in this example:

```erlang
c(X) ->
    {A,B} = case X of
		a1 -> {10,1};
		b2 -> {20,2};
		_ ->  {100,42}
	    end,
    A+B.
```

The unoptimized Core Erlang code looks like this:

```
'c'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <X> when 'true' ->
	      let <_@c2> =
		  case X of
		    <'a1'> when 'true' ->
			{10,1}
		    <'b2'> when 'true' ->
			{20,2}
		    <_@c5> when 'true' ->
			{100,42}
		    <_@c1> when 'true' ->
			  primop 'match_fail'({'case_clause',_@c1})
		  end
	      in
		  case _@c2 of
		    <{A,B}> when 'true' ->
			call 'erlang':'+'(A, B)
		    <_@c3> when 'true' ->
			  primop 'match_fail'({'badmatch',_@c3})
		  end
	  <_@c4> when 'true' ->
		  primop 'match_fail'({'function_clause',_@c4})
	end
```

Here a tuple is built and assigned to `_@c2`. It is then matched
in a `case`.

First the code is optimized like this to eliminate the tuple building
in each clause of the first `case`:

```
'c'/1 =
    fun (_@c0) ->
	let <_@f4,_@f5> =
	    case _@c0 of
	      <'a1'> when 'true' ->
		  <10,1>
	      <'b2'> when 'true' ->
		  <20,2>
	      <_@c5> when 'true' ->
		  <100,42>
	    end
	in
            let <_@c2> = {_@f4,_@f5}
            in
	          case _@c2 of
		    <{A,B}> when 'true' ->
			call 'erlang':'+'(A, B)
		    <_@c3> when 'true' ->
			  primop 'match_fail'({'badmatch',_@c3})
		  end
	end
```

Applying all of the optimizations previously described,
the remaining tuple building and matching can be eliminated:

```
'c'/1 =
    fun (_@c0) ->
	let <_@f4,_@f5> =
	    case _@c0 of
	      <'a1'> when 'true' ->
		  <10,1>
	      <'b2'> when 'true' ->
		  <20,2>
	      <_@c5> when 'true' ->
		  <100,42>
	    end
	in
	    call 'erlang':'+'(_@f4, _@f5)
```

## Conclusion

That was a quick look at some of the optimizations done by
`sys_core_fold`.

Some of the optimizations are very simple. The power of the
`sys_core_fold` pass comes from the combination of optimizations.  One
optimization gives opportunities for other optimizations, as could be
seen in the examples.

## Points to Ponder

Why is the optimization pass called `sys_core_fold`?

A hint can be found in the title of this Wikipedia article:
[Constant folding](https://en.wikipedia.org/wiki/Constant_folding).

