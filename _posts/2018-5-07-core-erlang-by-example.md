---
layout: post
title: Core Erlang by Example
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post is the first about the Core Erlang format. In this
blog post, we introduce the Core Erlang format through examples
that compare Erlang code to the corresponding Core Erlang
code.

I used the following command to translate my example module to
Core Erlang code:

```
$ erlc +time +to_core core_example.erl
Compiling "core_example"
 parse_module                  :      0.000 s      10.8 kB
 transform_module              :      0.000 s      10.8 kB
 lint_module                   :      0.003 s      10.8 kB
 expand_records                :      0.000 s      10.8 kB
 core                          :      0.000 s      89.9 kB
 sys_core_fold                 :      0.000 s      58.6 kB
 core_transforms               :      0.000 s      58.6 kB
 listing                       :      0.002 s      58.6 kB
```

The [previous blog post](http://blog.erlang.org/compiler-lost-in-translation/)
explored the passes from `parse_module` to `expand_records`. The
`core` passes translates from the abstract code to Core Erlang. We
will talk more about the Core Erlang passes in future blog posts.

I have slightly edited the examples to make them somewhat easier to
read. There will be an unedited example at the very end of this blog post.

There's a lot to cover, so let's get started!

## The simplest function

Let start with the simplest possible function, a function with no
arguments returning an atom:

```erlang
simplest() -> 'ok'.
```

In Core Erlang, that will be:

```
'simplest'/0 =
    fun () ->
	'ok'
```

From that example, we can work out the following principles:

* Atoms are always quoted.

* Naming of the function has been separated from implementation
of the function.

* The body of a `fun` is not followed by an `end` as in Erlang.


## Slightly less simple

Here is as slightly more complicated function:

```erlang
id(I) -> I.
```

In Core Erlang:

```
'id'/1 =
    fun (_@c0) ->
	_@c0
```

**Note**: All examples were compiled with OTP 20. The name of the
generated variables will be different in the upcoming OTP 21.

Essentially, variables are named as in Erlang. In the translation
to Core Erlang, the compiler generates new variable names for the
arguments in a function head. The following code is also valid
Core Erlang:


```
'id'/1 =
    fun (I) ->
	I
```

## More than one clause

Here is a function with more than one clause:

```erlang
a(42) -> ok;
a(_) -> error.
```

In Core Erlang:

```
'a'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  <_@c2> when 'true' ->
	      'error'
	end
```

* A `fun` can only have a single clause.

* Pattern matching must be done in a `case`, not in the `fun` head.

* Guards are mandatory for each clause in a `case`.

* `_` is **not** a valid variable name in Core Erlang. Uninteresting
values must be bound to a new variable.

* The `<` and `>` around the patterns will be explained soon.

In Erlang, multiple function clauses can also be written with a
`case` like this:

```erlang
b(N) ->
    case N of
        42 -> ok;
        _ -> error
    end.
```

The Core Erlang code will be essentially the same as the Core Erlang
code for `a/1`:

```
'b'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  <_@c3> when 'true' ->
	      'error'
	end
```

## Two clauses, three arguments

Let's try multiple arguments:

```erlang
c(inc, Base, N) ->
    Base+N;
c(_, Base, _) ->
    Base.
```

In Core Erlang:

```
'c'/3 =
    fun (_@c2,_@c1,_@c0) ->
	case <_@c2,_@c1,_@c0> of
	  <'inc',Base,N> when 'true' ->
	      call 'erlang':'+'(Base, N)
	  <_@c6,Base,_@c7> when 'true' ->
	      Base
	end
```

* `<` and `>` denote a **value list**. The patterns in each clause in
the `case` are always part of a value list. The `case` expression is
a value list unless there is only one expression.

* Operators such as `+` are not part of the Core Erlang language,
so the compiler has translated the use of `+` to a call to the
BIF `erlang:'+'/2`.

## If

Let's see how `if` is implemented:

```erlang
d(A, B) ->
    if
        A > B ->
            greater;
        true ->
            not_greater
    end.
```

In Core Erlang:

```
'd'/2 =
    fun (_@c1,_@c0) ->
	case <> of
	  <> when call 'erlang':'>'(_@c1, _@c0) ->
	      'greater'
	  <> when 'true' ->
	      'not_greater'
	end
```

* The `case` expression and the patterns are each value lists with
zero elements. All the action is in the guards.

## Repeated variables

In Erlang, a variable can be repeated in a clause or within a
pattern to indicate that the values must be the same:

```erlang
cmp(Same, Same) -> same;
cmp(_, _) -> different.
```

Core Erlang does not allow repeating a variable:

```
'cmp'/2 =
    fun (_@c1,_@c0) ->
	case <_@c1,_@c0> of
	  <Same,_@c4> when call 'erlang':'=:='(_@c4, Same) ->
	      'same'
	  <_@c5,_@c6> when 'true' ->
	      'different'
	end
```

* Here the second occurence of the variable `Same` has been renamed to
a new variable named `_@c4`, and a guard has been added to compare
`Same` and `_@c4`.

## Exceptions

This function will fail with a `function_clause` exception if it is called
with any other value than `42`:

```erlang
e(42) -> ok.
```

In Core Erlang:

```
'e'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  <_@c1> when 'true' ->
	      primop 'match_fail'({'function_clause',_@c1})
	end
```

* A `case` in Core Erlang must not fall off at the end, that is,
there must always be a clause that will match.

* In this example, the last clause with a variable pattern and
a `true` guard is guaranteed to match.

* The body for the last clause calls a **primop** to generate
a function clause exception. Primops are primitive operations
provided by the Erlang implementation, but not specified in the
Core Erlang language specification.

Here is a similar function excepts that is uses `case` and therefore
will generate a `case_clause` exception if called with any other
argument than `42`:

```erlang
f(N) ->
    case N of
        42 -> ok
    end.
```

The Core Erlang code is similar to the code for `e/1`:


```
'f'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  <_@c1> when 'true' ->
	      primop 'match_fail'({'case_clause',_@c1})
	end
```

* The only difference is the argument for the `match_fail` primop.

Let's rewrite this function one more time:

```erlang
g(N) ->
    42 = N,
    ok.
```

In Core Erlang:

```
'g'/1 =
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  <_@c1> when 'true' ->
	      primop 'match_fail'({'badmatch',_@c1})
	end
```

* Again, the only difference is the argument for the `match_fail` primop.

## Binding variables using 'let'

Here is a function that binds the variable `I`:

```erlang
h(A) ->
    I = id(A),
    I + A.
```

In Core Erlang:

```
'h'/1 =
    fun (_@c0) ->
	let <I> =
              apply 'id'/1(_@c0)
	in
              call 'erlang':'+'(I, _@c0)
```


* `apply` calls a fun or local function.

* The return value of the `apply` is bound to the variable `I`.

* The variable `I` can only be used in the code that follows the
`in` keyword.

* The variable name is in a value list. That is because `let`
can bind several variables at once.

## Binding more than one variable in a 'let'

Erlang has essentially no scoping. When a variable has been bound,
it remains bound to the end of the function. For example, variables bound
in a `case` can be used after the `case`:

```erlang
i(E) ->
    case E of
        a ->
            X = 1,
            Y = 10;
        b ->
            X = 23,
            Y = 17
    end,
    {X,Y}.
```

In Core Erlang:

```
'i'/1 =
    fun (_@c0) ->
	let <_@c7,X,Y> =
	    case _@c0 of
	      <'a'> when 'true' ->
		  <10,1,10>
	      <'b'> when 'true' ->
		  <17,23,17>
	      <_@c5> when 'true' ->
		  primop 'match_fail'({'case_clause',_@c5})
	    end
	in
	    {X,Y}
```

* A `case` in Core Erlang does not export any variables. All variables
that are to be used after the `case` must be explicitly returned.

* In this example, the first two clauses of the `case` return a
value list with **three** values. The first value is the return value
of the case, which in this case is ignored. The other two values are
the values assigned to the `X` and `Y` variables, respectively.

* The values returned from the `case` is bound in the `let`. The ignored
return value is bound to a new variable (`_@c7`), which is never used.
The exported values are bound to the `X` and `Y` variables.

## The unedited Core Erlang code

So far all Core Erlang examples have been edited to make the points
I am trying to make stand out clearer. Let's have a look at the unedited
version of a previous example:

```
'e'/1 =
    %% Line 33
    fun (_@c0) ->
	case _@c0 of
	  <42> when 'true' ->
	      'ok'
	  ( <_@c1> when 'true' ->
		( primop 'match_fail'
		      ({'function_clause',_@c1})
		  -| [{'function_name',{'e',1}}] )
	    -| ['compiler_generated'] )
	end
```

* The `-|` associates an annotation with a Core Erlang construct.
The meaning of an annotation is not specified in the Core Erlang
language specification.

* The `compiler_generated` annotation associated with the last clause
is a hint added by the compiler that subsequent optimization passes should
not generate a warning if the clause was found to never match and dropped.

* The comment "Line 33" at the beginning is actually an annotation that
the pretty printer has turned into a comment to avoid rendering the
pretty-printed code unreadable.

## Conclusion

Core Erlang is less complicated than Erlang, and is therefore more
suited than the abstract format for code analyzing tools (such as
[Dialyzer][dialyzer]) and optimizers.

[dialyzer]: http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html

## To learn more about Core Erlang

All details can be found in [Core Erlang 1.0.3 language specification][core].

[core]: https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
