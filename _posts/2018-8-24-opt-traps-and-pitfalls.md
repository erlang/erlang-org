---
layout: post
title: Optimization Traps and Pitfalls
tags: compiler BEAM
author: Björn Gustavsson
---

Back after the summer holidays, this blog will now change tracks and
start a series of blog posts about Static Single Assignment (SSA).
This first installment will set the scene for the posts that follow by
looking at the traps and pitfalls one can fall into when trying to
optimize BEAM assembly code.

## A brief introduction to BEAM assembly language

We will look at the BEAM code for the following function:

```erlang
foo({tag,A,_,_}) ->
    {ok,A}.
```

The (unoptimized) BEAM code looks like this:

```
{function, foo, 1, 2}.
  {label,1}.
    {line,[{location,"ex1.erl",4}]}.
    {func_info,{atom,ex1},{atom,foo},1}.
  {label,2}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,3},[{x,1},{atom,tag}]}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
  {label,3}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"ex1.erl",4}]}.
    {call_ext_only,2,{extfunc,erlang,error,2}}.
```

We will concentrate on the part of the code that does
the actual work:

```
    {test,is_tuple,{f,3},[{x,0}]}.
    {test,test_arity,{f,3},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,3},[{x,1},{atom,tag}]}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
  {label,3}.
    %% Cause a function_clause exception.
```

We will now explain what each instruction does.

```
    {test,is_tuple,{f,3},[{x,0}]}.
```

`test` instructions test whether a condition is true. If it
is, the next instruction will be executed. Otherwise,
there will be a branch to the failure label.

The condition tested by this instruction is `is_tuple`, that is
whether its operand is a tuple.  The operand is `{x,0}`, which is the
register for the first argument for the function. If `{x,0}` does not
contain a tuple, execution will continue at the failure label. `{f,3}`
means that that the failure label is `3`. The code at label `3` will
cause a `function_clause` exception.

```
    {test,test_arity,{f,3},[{x,0},4]}.
```

The `test_arity` instruction tests whether the first operand (which
must be a tuple) has the size given by the second operand. The first
operand is `{x,0}` and the second operand is `4`. The failure label is
the same as for the previous instruction.

```
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
```

When those two instructions are executed, the previous instructions have
established that `{x,0}` contains a tuple of arity 4.
`get_tuple_element` takes three operands. The first is the source
tuple, `{x,0}`, the second is the **zero-based** index into the tuple,
and the third operand is the register into which the element from the
tuple should be stored. Note that there is no failure label because it
cannot fail.

So the first `get_tuple_element` instruction fetches the first element
of the tuple and stores it in the `{x,1}` register, and the second
`get_tuple_element` instruction fetches the second element and stores
it into the `{x,2}` register.

```
    {test,is_eq_exact,{f,3},[{x,1},{atom,tag}]}.
```

`is_eq_exact` is again a `test` instruction. It tests
whether the contents of `{x,1}` is exactly equal (that is,
`=:=`) to the atom `tag`. If not, execution will continue
at the failure label `3`.

That concludes the function header. The next instruction is in the
body of the function that will build the `{ok,A}` tuple:

```
    {test_heap,3,3}.
```

The `test_heap` instruction ensures that there is sufficient free
space on the heap to construct a term. The first operand (the first
`3`) says that the following instructions will need 3 words on the
heap. A tuple has a header word, followed by the elements, so a tuple
with 2 elements needs 3 heap words in total.

If there is not sufficient room on the heap, the `test_heap`
instruction will do a garbage collection to find some fresh heap
space. The second operand (the second `3`) is the number of `x`
registers that have values that must be preserved during garbage
collection. The `3` means that `{x,0}`, `{x,1}`, and `{x,2}` have live
values.

```
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
```

Those three instructions build the tuple, putting a tagged
pointer to the tuple in `{x,0}`.

```
    return.
```

`return` returns from the function. The return value is the
value in `{x,0}`.

## Optimizing this code

Testing that a term is a tuple of a certain size with a specific atom
as the first element is a common operation (think records). Therefore
the BEAM machine has an `is_tagged_tuple` instruction that does the
work of 4 other instructions.

Using that instruction, this code:

<pre class="highlight">
    <b>{test,is_tuple,{f,3},[{x,0}]}.</b>
    <b>{test,test_arity,{f,3},[{x,0},4]}.</b>
    <b>{get_tuple_element,{x,0},0,{x,1}}.</b>
    {get_tuple_element,{x,0},1,{x,2}}.
    <b>{test,is_eq_exact,{f,3},[{x,1},{atom,tag}]}.</b>
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
</pre>

can be rewritten like this:

<pre class="highlight">
    <b>{test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.</b>
    {get_tuple_element,{x,0},1,{x,2}}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
</pre>

This is a nice reduction in code size and execution
time. However, this optimization is not safe.

Why?

Consider the `{test_heap,3,3}` instruction. The second `3` says
that 3 `x` registers are live, namely `{x,0}`, `{x,1}`, and `{x,2}`.
Clearly, `{x,0}` and `{x,2}` are live, but what about `{x,1}`?
We removed the `get_tuple_element` instruction that assigned `{x,1}`
a value, so the value of `{x,1}` is undefined.

Passing undefined register values to the garbage collector is the kind
of bug that could take weeks to track down. In fact, there will
probably be a future blog post about that kind of bug and how two
tools were born as result of that bug.

Reluctantly, in order to make the optimization safe, we must keep
the `get_tuple_element` instruction that assigns to `{x,1}`:

<pre class="highlight">
    {test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.
    <b>{get_tuple_element,{x,0},0,{x,1}}.</b>
    {get_tuple_element,{x,0},1,{x,2}}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
</pre>

Another possibility in this case would be to assign an empty list
(called `nil` in the BEAM assembly language) to `{x,1}`:

<pre class="highlight">
    {test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.
    <b>{move,nil,{x,1}}.</b>
    {get_tuple_element,{x,0},1,{x,2}}.
    {test_heap,3,3}.
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
</pre>

However, in this very simple example, another optimization will
actually allow the compiler to remove the assignment to `{x,1}`:

<pre class="highlight">
    {test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.
    <b>{test_heap,3,1}.</b>
    <b>{get_tuple_element,{x,0},1,{x,2}}.</b>
    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.
    return.
</pre>

The `test_heap` and `get_tuple_element` instructions have been swapped.
Note that the number of live register have been adjusted in the `test_heap`
instruction. It is now `1` instead of `3`.

In general, though, the compiler might have to abandon an optimization
or keep an instruction that assigns a register to avoiding feeding the
garbage collector undefined values.

## The final straw

During the development of OTP 21, we realized that we have reached the
limit for improving the optimizations that operates on the BEAM
assembly language. In particular, we wanted to make the optimization
called the [delayed sub binary creation][bin_matching] applicable in
more circumstances. It turned out that would it be hard or impossible
to substantially improve the optimization by working on BEAM assembly
language.

[bin_matching]: http://erlang.org/doc/efficiency_guide/binaryhandling.html#matching-binaries

Apart from the problem of leaving undefined registers, as illustrated
in the previous optimization example, there is also the complexity of
traversing and analyzing BEAM instructions. The BEAM instruction set
was not designed to be optimizer-friendly.

## Conclusion

As I have tried to show with the example above, one of the hardest
parts of working with BEAM code is that register allocation has
already been done and that instructions that may do a garbage
collection (such as `test_heap`) have already been added.

Early this year (2018), we decided that we should introduce a new
intermediate format to alleviate the problems with optimizing BEAM
code. It should be close enough to BEAM code to allow
low-level optimizations such as the `is_tagged_tuple` optimization
described in this blog post, but register allocation should not have
been done, and `test_heap` and similar instructions should not have
been added. It should also be more regular to make it easier to
traverse while doing optimizations.

We decided to make the new intermediate format [SSA-based][ssa].
In the next blog post, we will re-visit the example from this blog
post and see what it looks like in the [new SSA-based intermediate
format][pr1935].

[ssa]: https://en.wikipedia.org/wiki/Static_single_assignment_form
[pr1935]: https://github.com/erlang/otp/pull/1935
