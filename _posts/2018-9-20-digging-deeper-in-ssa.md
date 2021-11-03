---
layout: post
title: Digging deeper in SSA
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post continues the exploration of the [new SSA-based
intermediate representation][pr1935] through multiple examples. Make
sure to read the [Introduction to SSA][prev] if you missed it.

## Calling a BIF that may fail

The first example calls a guard BIF that may fail with
an exception:

```erlang
element_body(T) ->
    element(2, T).
```

The (optimized) SSA code looks like this:

```
function blog:element_body(_0) {
0:
  %% blog.erl:5
  _1 = bif:element literal 2, _0
  @ssa_bool = succeeded _1
  br @ssa_bool, label 3, label 1

3:
  ret _1

1:
  @ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  ret @ssa_ret
}
```

Let's go through the code a few lines at a time:

```
  %% blog.erl:5
  _1 = bif:element literal 2, _0
  @ssa_bool = succeeded _1
```

The `bif:element` instruction calls the guard BIF `element/2`, assigning
the value to the variable `_1` if the call is successful.

What if the call is not successful?

The `succeeded _1` instruction tests whether the previous instruction
assigning to `_1` was successful. `true` will be assigned to `@ssa_bool`
if the second element of the tuple was successfully fetched from the tuple,
and `false` will be assigned otherwise.

```
  br @ssa_bool, label 3, label 1
```

The `br` instruction tests whether `@ssa_bool` is `true`. If `true`, execution
continues at block 3, which returns the value of the second element from the
tuple. If `false`, execution continues at block 1.

It was mentioned in the [previous blog post][prev] that block 1 is a
special block that the SSA code generator always emits. In the
previous examples, it was never referenced and therefore removed by
one of the optimization passes.

In this example, it is used as the target when the call to `element/2` fails.

The BEAM code generator treats references to block 1 specially. Here follows the
BEAM code for the function. As usual, I have omitted the function header.

      %% Block 0.
      {line,[{location,"blog.erl",5}]}.
      {bif,element,{f,0},[{integer,2},{x,0}],{x,0}}.
      return.

Note that no code has been generated for block 1.

The `line` instructions gives the file name and line number of the source file.
It will be used in the stack backtrace if the following instruction fails.

The `bif` instruction calls the given guard BIF, `element/2` in this case.
The `{f,0}` operand gives the action to take if the `element/2` fails. The number
`0` is a special case, meaning that a `badarg` exception should be raised if the
call of `element/2` fails.

## A failing BIF call in a guard

In the next example, `element/2` is called in a guard:

```erlang
element_guard(T) when element(2, T) =:= true ->
    ok;
element_guard(_) ->
    error.
```

The SSA code looks like this:

```
function blog:element_guard(_0) {
0:
  %% blog.erl:7
  _1 = bif:element literal 2, _0
  @ssa_bool = succeeded _1
  br @ssa_bool, label 4, label 3

4:
  @ssa_bool:5 = bif:'=:=' _1, literal true
  br @ssa_bool:5, label 6, label 3

6:
  ret literal ok

3:
  ret literal error
}
```

The first two instructions in block 0 are the same as in the previous
example.  The `br` instruction has different labels, though. The
failure label refers to block 3, which returns the value `error`. The
success label continues execution at block 4.

```
4:
  @ssa_bool:5 = bif:'=:=' _1, literal true
  br @ssa_bool:5, label 6, label 3
```

Block 4 is the translation of `=:= true` part of the Erlang code.
If the second element in the tuple is equal to `true`, execution
continues at block 6, which returns the value `ok`. Otherwise
execution continues at block 3, which returns the value `error`.

Here is the BEAM code:

      {bif,element,{f,5},[{integer,2},{x,0}],{x,0}}.
      {test,is_eq_exact,{f,5},[{x,0},{atom,true}]}.
      {move,{atom,ok},{x,0}}.
      return.
    {label,5}.
      {move,{atom,error},{x,0}}.
      return.

In the `bif` instruction, `{f,5}` means that execution should
continue at label 5 if the `element/2` call fails. Otherwise
execution will continue at the next instruction.

## Our first case

Here is the next example:

```erlang
case1(X) ->
    case X of
        1 -> a;
        2 -> b;
        _ -> c
    end.
```

Translated to SSA code:

```
function blog:case1(_0) {
0:
  switch _0, label 3, [ { literal 2, label 5 }, { literal 1, label 4 } ]

4:
  ret literal a

5:
  ret literal b

3:
  ret literal c
}
```

The `switch` instruction is a multi-way branch to one of any number of
other blocks, based on the value of a variable. In this example, it
branches based on the value of the variable `_0`. If `_0` is equal
to 2, execution continues at block 5. If `_0` is equal to 1,
execution continues at block 4. If the value is not equal to any
of the values in the switch list, execution continues at the block
referred to by the failure label, in this example block 3.

The BEAM code looks like this:

      {select_val,{x,0},{f,10},{list,[{integer,2},{f,9},{integer,1},{f,8}]}}.
    {label,8}.
      {move,{atom,a},{x,0}}.
      return.
    {label,9}.
      {move,{atom,b},{x,0}}.
      return.
    {label,10}.
      {move,{atom,c},{x,0}}.
      return.

## Terminators

As mentioned in the [previous blog post][prev], the last instruction in a block is
called a *terminator*. A terminator either returns from the function or transfers
control to another block. With the introduction of `switch`, the terminator story
is complete. To summarize, a block can end in one of the following terminators:

* `ret` to return a value from the function.

* `br` to either branch to another block (one-way branch), or branch to one
of two possible other blocks based on a variable (two-way branch).

* `switch` to branch to one of any number of other blocks.

## Another case

Here is a slightly different example:

```erlang
case2(X) ->
    case X of
        1 -> a;
        2 -> b;
        3 -> c
    end.
```

In this case, `X` must be one of the integers 1, 2, or 3. Otherwise, there will
be a `{case_clause,X}` exception. Here is the SSA code:

```
function blog:case2(_0) {
0:
  switch _0, label 3, [ { literal 3, label 6 }, { literal 2, label 5 }, { literal 1, label 4 } ]

4:
  ret literal a

5:
  ret literal b

6:
  ret literal c

3:
  _2 = put_tuple literal case_clause, _0

  %% blog.erl:20
  @ssa_ret:7 = call remote (literal erlang):(literal error)/1, _2
  ret @ssa_ret:7
}
```

The failure label for the `switch` is 3. Block 3 builds the `{case_clause,X}`
tuple and calls `erlang:error/1`.

Here is the BEAM code:

      {select_val,{x,0},
                  {f,16},
                  {list,[{integer,3},
                         {f,15},
                         {integer,2},
                         {f,14},
                         {integer,1},
                         {f,13}]}}.
    {label,13}.
      {move,{atom,a},{x,0}}.
      return.
    {label,14}.
      {move,{atom,b},{x,0}}.
      return.
    {label,15}.
      {move,{atom,c},{x,0}}.
      return.
    {label,16}.
      {line,[{location,"blog.erl",20}]}.
      {case_end,{x,0}}.

The `case_end` instruction is an optimization to save space. It is shorter than
the equivalent:

      {test_heap,3,1}.
      {put_tuple2,{x,0},{list,[{atom,case_clause},{x,0}]}}.
      {line,[{location,"blog.erl",20}]}.
      {call_ext_only,1,{extfunc,erlang,error,1}}.

(The `put_tuple2` instruction was introduced in
[#1947: Introduce a put_tuple2 instruction][pr1947], which was recently merged
to `master`.)

## Our final case

It's time to address the kind of `case` similar to what was teased at the
end of the previous blog post.

In this example, the variable `Y` will be assigned different values in
each clause of the `case`:

```erlang
case3a(X) ->
    case X of
        zero ->
            Y = 0;
        something ->
            Y = X;
        _ ->
            Y = no_idea
    end,
    {ok,Y}.
```

Perhaps a more common way to write this `case` would be:

```erlang
case3b(X) ->
    Y = case X of
            zero -> 0;
            something -> X;
            _ -> no_idea
        end,
    {ok,Y}.
```

In either case, the problem remains. Static Single Assignment means that each
variable can only be given a value once. So how can this example be translated
to SSA code?

Here follows the SSA code for `case3a/1`. The SSA code for `case3b/1` is almost
identical except for variable naming.

```
function blog:case3a(_0) {
0:
  switch _0, label 4, [ { literal something, label 6 }, { literal zero, label 5 } ]

5:
  br label 3

6:
  br label 3

4:
  br label 3

3:
  Y = phi { literal no_idea, 4 }, { literal 0, 5 }, { _0, 6 }
  _7 = put_tuple literal ok, Y
  ret _7
}
```

Let's jump right to the interesting (and confusing) part of the code:

```
3:
  Y = phi { literal no_idea, 4 }, { literal 0, 5 }, { _0, 6 }
```

Clearly, `Y` is only given a value once, so the SSA property is
preserved.

That's good, but exactly what is the value that is being assigned?

The name of the instruction is `phi`, which is the name of the
Greek letter [&phi;][phi]. Having an unusual name, the instruction
deserves to have unusual operands, too. Each operand is a pair, the
first element in the pair being a value and the second element a block
number of a predecessor block. The value of the `phi` node will be one
of the values from one the pairs. But from which pair? That depends on
the number of the previous block that branched to the `phi` instruction.

To make that somewhat clearer, let's look at all operands:

* `{ literal no_idea, 4 }`: If the number of block that executed `br label 3`
was 4, the value of the `phi` instruction will be the value in this pair,
that is, the atom `no_idea`. The failure label for the `switch` instruction
is 4, so this pair will be chosen when `_0` does not match any of the values
in the switch list.

* `{ literal 0, 5 }`: If the number of block that executed `br label 3`
was 5, the value of the `phi` instruction will be the integer 0. The
`switch` instruction will transfer control to block 5 if the value of
`_0` is the atom `zero`.

* `{ _0, 6 }`: Finally, if `_0` is the atom `something`, the `switch`
will transfer control to block 6, which will transfer control to
block 3. The value of the `phi` instruction will be the value of the
variable `_0`.

The concept of `phi` instructions probably feels a bit strange at
first sight (and at second sight), and one might also think they
must be terribly inefficient.

Leaving the strangeness aside, let's talk about the efficiency. `phi`
instructions is a fiction convenient for representing and optimizing
the code. When translating to BEAM code, the `phi` instructions are
eliminated.

Here follows an example that is **not** SSA code, because it assigns
the variable `Y` three times, but gives an idea how the `phi`
instruction is eliminated:

```
%% Not SSA code!
function blog:case3a(_0) {
0:
  switch _0, label 4, [ { literal something, label 6 }, { literal zero, label 5 } ]

5:
  Y := literal 0
  br label 3

6:
  Y := _0
  br label 3

4:
  Y := no_idea
  br label 3

3:
  _7 = put_tuple literal ok, Y
  ret _7
}
```

The BEAM code generator (`beam_ssa_codegen`) does a similar rewrite
during code generation.

Here is the unoptimized BEAM code, slightly edited for clarity:

    %% Block 0.
    {select_val,{x,0},
                {f,53},
                {list,[{atom,something},{f,55},{atom,zero},{f,57}]}}.

    %% Block 5.
    {label,57}.
      {move,{integer,0},{x,0}}.
      {jump,{f,59}}.

    %% Block 6.
    {label,55}.
      %% The result is already in {x,0}.
      {jump,{f,59}}.

    %% Block 4.
    {label,53}.
      {move,{atom,no_idea},{x,0}}.
      {jump,{f,59}}.

    %% Block 3.
    {label,59}.
       {test_heap,3,1}.
       {put_tuple2,{x,0},{list,[{atom,ok},{x,0}]}}.
       return.

Here is the final BEAM code after some more optimizations:

    {label,18}.
      {select_val,{x,0},
                  {f,20},
                  {list,[{atom,something},{f,21},{atom,zero},{f,19}]}}.
    {label,19}.
      {move,{integer,0},{x,0}}.
      {jump,{f,21}}.
    {label,20}.
      {move,{atom,no_idea},{x,0}}.
    {label,21}.
      {test_heap,3,1}.
      {put_tuple2,{x,0},{list,[{atom,ok},{x,0}]}}.
      return.

## The cold case

Here is the example from the end of the previous blog post:

```erlang
bar(X) ->
    case X of
        none ->
            Y = 0;
        _ ->
            Y = X
    end,
    Y + 1.
```

And here is the SSA code:

```
function blog:bar(_0) {
0:
  @ssa_bool = bif:'=:=' _0, literal none
  br @ssa_bool, label 5, label 4

5:
  br label 3

4:
  br label 3

3:
  Y = phi { _0, 4 }, { literal 0, 5 }

  %% blog.erl:52
  _6 = bif:'+' Y, literal 1
  @ssa_bool:6 = succeeded _6
  br @ssa_bool:6, label 7, label 1

7:
  ret _6

1:
  @ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  ret @ssa_ret
}
```

It is left as an exercise to the reader to read and understand the code.

Here is the BEAM code:

    {label,28}.
      {test,is_eq_exact,{f,29},[{x,0},{atom,none}]}.
      {move,{integer,0},{x,0}}.
    {label,29}.
      {line,[{location,"blog.erl",52}]}.
      {gc_bif,'+',{f,0},1,[{x,0},{integer,1}],{x,0}}.
      return.

The `gc_bif` instruction calls a guard BIF that might need to do a
garbage collection. Since integers can be of essentially unlimited
size in Erlang, the result of `+` might not fit in a word. The
`1` following `{f,0}` is the number of registers that must be
preserved; in this case, only `{x,0}`.

[prev]: http://blog.erlang.org/introducing-ssa/
[pr1935]: https://github.com/erlang/otp/pull/1935
[pr1947]: https://github.com/erlang/otp/pull/1947
[phi]: https://en.wikipedia.org/wiki/Phi
