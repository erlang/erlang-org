---
layout: post
title: Introduction to SSA
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post is an introduction to the [new SSA-based intermediate
representation][pr1935] that has recently been merged to the `master`
branch in the [Erlang/OTP repository][otp]. It uses the same
example as in the [previous blog post][prev], first looking at the
generated SSA code, and then at some optimizations.

Here again is the example function that does the kind of tuple matching
typically done when matching records:

```erlang
foo({tag,A,_,_}) ->
    {ok,A}.
```

At the end of this blog post there will be section on [how to generate
listing files](#generating_listings) to inspect the code from the
compiler passes.

## A brief introduction to the SSA intermediate format

SSA stands for [Static Single Assignment][ssa]. Strictly speaking, SSA
is the property of an [intermediate representation][intermediate] where
each variable is assigned exactly once, and where every variable is
defined before it is used. In this blog post, we will use the term
*SSA code* to refer to the new intermediate representation in the
Erlang compiler.

Here is the SSA code for the `foo/1` function:

```
function blog:foo(_0) {
0:
  @ssa_bool:6 = bif:is_tuple _0
  br @ssa_bool:6, label 7, label 3

7:
  @ssa_arity = bif:tuple_size _0
  @ssa_bool:8 = bif:'=:=' @ssa_arity, literal 4
  br @ssa_bool:8, label 5, label 3

5:
  _8 = get_tuple_element _0, literal 0
  _7 = get_tuple_element _0, literal 1
  @ssa_bool = bif:'=:=' _8, literal tag
  br @ssa_bool, label 4, label 3

4:
  _9 = put_tuple literal ok, _7
  ret _9

3:
  _4 = put_list _0, literal []

  %% blog.erl:4
  @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
  ret @ssa_ret:9

%% Unreachable blocks

1:
  @ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  ret @ssa_ret
}
```

### A deeper look at the example

We will go through the code a few lines at the time.

```
function blog:foo(_0) {
```

This is the head of the function. It gives the module name (`blog`),
function name (`foo`), and the arguments (the single variable _0).

Variables named as `_` followed by an integer are inherited from
[Core Erlang][core_erlang]. In OTP 22, variable names in Core Erlang
are integers (to avoid filling the atom table when compiling huge
functions).

```
0:
```

Following the function head is one or more *blocks* (sometimes called
*nodes*).  A integer followed by a colon gives the number of the block
that follows.

The block number `0` is special.  It is the first block that will be
executed in this function.

```
  @ssa_bool:6 = bif:is_tuple _0
```

Here is the first real instruction! All instructions have this
basic format. First there is a variable, followed by `=`, followed
by the name of the instruction, followed by its operands.

The variable to the left, `@ssa_bool:6` in this example, will be
assigned the value of the expression to right of the `=`.

Each variable can only be assigned once, just as in Erlang. The name
of this variable consists of two parts, the base part `@ssa_bool` and
the numeric suffix `6`. Whenever the base name itself is not unique,
the numeric suffix is added to make the name unique.

The instruction name is `bif:is_tuple`. This is one of the
instructions that use a two-part name.  The `bif` prefix means that
the second part must be the name of an Erlang guard BIF, in this case
`is_tuple/1`.

Following the name of the instruction is the operand `_0`, which is
the name of the function argument for the `foo/1` function.

Thus, this instruction will call `is_tuple/1` and assign the result
(either `true` or `false`) to `ssa_bool:6`.

```
  br @ssa_bool:6, label 7, label 3
```

This is the last instruction of block 0. Instructions at the end
of a block are called *terminators* and they have a different format
compared to instructions in the interior of a block. Terminators
either transfer control to another block or returns from the
function.

`br` transfers control to another block. The first operand is a
variable, whose value must be `true` or `false`. If the value of
`ssa_bool:6` is `true`, the second operand (`label 7`) is used as the
block number for the block where execution will continue. In this
example: block 7. Similarly, if the value of `ssa_bool:6` is `false`,
the third operand (`label 3`) will be used to transfer control to
block 3.


```
7:
  @ssa_arity = bif:tuple_size _0
```

This is the beginning of block 7. This block will be executed
if `_0` was found to be a tuple. `@ssa_arity` will be assigned
the value of the call `tuple_size(_0)`.

Note that `@ssa_arity` does not have a numeric suffix, since there
is no other variable in this function having the same base name.

```
  @ssa_bool:8 = bif:'=:=' @ssa_arity, literal 4
```

Here `bif:=:=` compares `@ssa_arity` and `4` and assigns the
result to `@ssa_bool:8`. (Note that `=:=` is a guard BIF
in Erlang; it is allowed but unusual to write
`erlang:'=:='(Arity, 4)` instead of `Arity =:= 4`.)

```
  br @ssa_bool:8, label 5, label 3
```

Here is another `br` instruction. It will transfer control to
block 5 if `@ssa_bool:8` is `true` (that is, if `@ssa_arity`
is equal to 4), and to block 3 otherwise.

```
5:
  _8 = get_tuple_element _0, literal 0
  _7 = get_tuple_element _0, literal 1
```

Block 5 is executed if `_0` has been found to be a tuple of
size 4. The `get_tuple_element` instruction extracts an element
from a tuple at the given position. The position is zero-based.

The `get_tuple_element` instruction in SSA in named after the
BEAM instruction with the same name:

    {get_tuple_element,{x,0},0,{x,1}}.

Notice the similarity between the SSA instruction and the BEAM
instruction. The SSA form uses variables instead of registers,
and the destination variable is to the left of the `=` as in
all SSA instructions.

```
  @ssa_bool = bif:'=:=' _8, literal tag
  br @ssa_bool, label 4, label 3
```

Here comes the test that the first element of the tuple
is equal to the atom `tag`. If the first element is `tag`,
execution continues at block 4, otherwise at block 3.

```
4:
  _9 = put_tuple literal ok, _7
```

This instruction constructs the `{ok,A}` tuple. The variable `_7`
contains the second element of the tuple.

The `put_tuple` instruction takes any number of operands and
constructs a tuple from them. The result is assigned to the 
variable `_9`.

In this case, the `put_tuple` instruction in SSA does more than the
corresponding BEAM instruction. To construct the same tuple, three
BEAM instructions are needed:

    {put_tuple,2,{x,0}}.
    {put,{atom,ok}}.
    {put,{x,2}}.

Having a single instruction to construct a tuple instead of
the multiple BEAM instructions simplifies optimizations
immensely. Also note that SSA has no equivalent of the `test_heap`
instruction that caused so much trouble in the [previous blog post][prev].

```
  ret _9
```

`ret` is another terminator instruction. `ret` returns from the function
with the value of variable `_9` as the return value.

That concludes the successful path through the function.

```
3:
  _4 = put_list _0, literal []

  %% blog.erl:4
  @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
  ret @ssa_ret:9
```

This block is executed if any of `br` instructions in the previous blocks
were given the value `false`, that is if the function argument was not a tuple or
had the wrong size or wrong first element.

The comment line (starting with `%%`) has been added by the pretty printer based on
annotation in the `call` instruction.

It is left as an exercise to the reader to figure out exactly what the
instructions in the block do.  As a hint, here is the code for the
block translated back to Erlang code:

```erlang
erlang:error(function_clause, [_0]).
```

Moving on to the part of the function that is not executed at all:

```
%% Unreachable blocks

1:
  @ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  ret @ssa_ret
```

The comment (`Unreachable blocks`) was added by the pretty printer to
indicate that the blocks that follow can never be executed, because no
block will ever branch to them.

Why is there an unreachable block?

Block 1 is a special block. It generates a `badarg` exeception, just
as a call to `error:error(badarg)`. The SSA code generator always
includes block 1 with the exact same instructions in every function,
even if it never actually used.

We will not go into details about the purpose of this block in this
blog post (but we will see how it is used in the next blog post).

## Optimizing the code

Now it's time to see how the SSA code can be optimized. The SSA
optimizations follow the same idea as the [Core Erlang
optimizations][sys_core_fold] of using many simple optimizations
working together rather than a few complicated optimizations.

Here is the code for the function again as it looks after running a few preliminary
optimization passes:

```
function blog:foo(_0) {
0:
  @ssa_bool:6 = bif:is_tuple _0
  br @ssa_bool:6, label 7, label 3

7:
  @ssa_arity = bif:tuple_size _0
  @ssa_bool:8 = bif:'=:=' @ssa_arity, literal 4
  br @ssa_bool:8, label 5, label 3

5:
  _8 = get_tuple_element _0, literal 0
  _7 = get_tuple_element _0, literal 1
  @ssa_bool = bif:'=:=' _8, literal tag
  br @ssa_bool, label 4, label 3

4:
  _9 = put_tuple literal ok, _7
  ret _9

3:
  _4 = put_list _0, literal []
  br label 10

10:
  %% blog.erl:4
  @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
  ret @ssa_ret:9
}
```

The unreachable block 1 has been deleted.

A pass that [splits blocks][ssa_opt_split_blocks] before certain
instructions has also been run (in order to make the passes for
[sinking `get_tuple_element` instructions][ssa_opt_sink] and [swapping
`element/2` calls][ssa_opt_element] more effective). This pass has
split block 3 into two blocks. At the end of block 3 there is a
variant of the `br` terminator that we have not seen before.  `br label
10` unconditionally continues the execution at block 10.

The first interesting optimization for our example is the
[ssa_opt_record] optimizations, which attempts to translate tuple
matching instructions with an `is_tagged_tuple` instruction.
Here is the part of the code that will be optimized:

<pre class="highlight">
    0:
      @ssa_bool:6 = bif:is_tuple _0
      br @ssa_bool:6, label 7, <b>label 3</b>

    7:
      @ssa_arity = bif:tuple_size _0
      @ssa_bool:8 = bif:'=:=' @ssa_arity, <b>literal 4</b>
      br @ssa_bool:8, label 5, <b>label 3</b>

    5:
      _8 = get_tuple_element _0, literal 0
      @ssa_bool = bif:'=:=' _8, <b>literal tag</b>
      br @ssa_bool, label 4, <b>label 3</b>
</pre>

The optimization is done in two stages. First the code is analyzed to find out
whether the optimization is applicable. There must be a test for a tuple of
a certain size (4 in this example) and with a certain first element
(`tag` in this example). Furthermore all failure labels must be the same.

If all conditions are fulfilled, the optimization is done in the second stage.
Here is the code again, with the optimized part of the code highlighted:

<pre class="highlight">
    function blog:foo(_0) {
    0:
      @ssa_bool:6 = <b>is_tagged_tuple _0, literal 4, literal tag</b>
      br @ssa_bool:6, label 7, label 3

    7:
      @ssa_arity = bif:tuple_size _0
      @ssa_bool:8 = bif:'=:=' @ssa_arity, literal 4
      br @ssa_bool:8, label 5, label 3

    5:
      _8 = get_tuple_element _0, literal 0
      @ssa_bool = bif:'=:=' _8, literal tag
      br @ssa_bool, label 4, label 3

    4:
      _7 = get_tuple_element _0, literal 1
      _9 = put_tuple literal ok, _7
      ret _9

    3:
      _4 = put_list _0, literal []
      br label 10

    10:
      %% blog.erl:4
      @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
      ret @ssa_ret:9
    }
</pre>

Yes, it really is this simple, but so far it is more of a
[pessimization] than an optimization, because the `bif:is_tuple`
instruction has been replaced with the more expensive
`is_tagged_tuple` instruction.

The next optimization is a type analysis pass, which is implemented in
the module [beam_ssa_type]. Here is the code after running `beam_ssa_type`:

<pre class="highlight">
    function blog:foo(_0) {
    0:
      @ssa_bool:6 = is_tagged_tuple _0, literal 4, literal tag
      br @ssa_bool:6, label 7, label 3

    7:
      @ssa_arity = bif:tuple_size _0
      @ssa_bool:8 = bif:'=:=' <b>literal 4, literal 4</b>
      <b>br label 5</b>

    5:
      _8 = get_tuple_element _0, literal 0
      @ssa_bool = bif:'=:=' <b>literal tag, literal tag</b>
      <b>br label 4</b>

    4:
      _7 = get_tuple_element _0, literal 1
      _9 = put_tuple literal ok, _7
      ret _9

    3:
      _4 = put_list _0, literal []
      br label 10

    10:
      %% blog.erl:4
      @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
      ret @ssa_ret:9
    }
</pre>

`beam_ssa_type` analyzes the code in execution order, remembering the
type of each variable seen. Based on the types, `beam_ssa_type` replaces
variables with known values with the values themselves.

Two of the conditional branchs have been converted to unconditional
branches.

The next optimization is [liveness analysis][ssa_opt_live]. The code
is scanned in reverse execution order, and if an expression is never
used, and has no observable side effect, it can be deleted. The
highlighted instructions in the code that follows was identified by
the liveness analysis pass as unused:

<pre class="highlight">
    function blog:foo(_0) {
    0:
      @ssa_bool:6 = is_tagged_tuple _0, literal 4, literal tag
      br @ssa_bool:6, label 7, label 3

    7:
      <b>@ssa_arity = bif:tuple_size _0</b>
      <b>@ssa_bool:8 = bif:'=:=' literal 4, literal 4</b>
      br label 5

    5:
      <b>_8 = get_tuple_element _0, literal 0</b>
      <b>@ssa_bool = bif:'=:=' literal tag, literal tag</b>
      br label 4

    4:
      _7 = get_tuple_element _0, literal 1
      _9 = put_tuple literal ok, _7
      ret _9

    3:
      _4 = put_list _0, literal []
      br label 10

    10:
      %% blog.erl:4
      @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
      ret @ssa_ret:9
    }
</pre>

Because those expressions don't have any side effects, they can be deleted:

<pre class="highlight">
    function blog:foo(_0) {
    0:
      @ssa_bool:6 = is_tagged_tuple _0, literal 4, literal tag
      br @ssa_bool:6, label 7, label 3

    7:
      br label 5

    5:
      br label 4

    4:
      _7 = get_tuple_element _0, literal 1
      _9 = put_tuple literal ok, _7
      ret _9

    3:
      _4 = put_list _0, literal []
      br label 10

    10:
      %% blog.erl:4
      @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
      ret @ssa_ret:9
    }
</pre>

After running a pass that [merges blocks][ssa_opt_merge_blocks], the final code
looks like this:

```
function blog:foo(_0) {
0:
  @ssa_bool:6 = is_tagged_tuple _0, literal 4, literal tag
  br @ssa_bool:6, label 5, label 3

5:
  _7 = get_tuple_element _0, literal 1
  _9 = put_tuple literal ok, _7
  ret _9

3:
  _4 = put_list _0, literal []

  %% blog.erl:4
  @ssa_ret:9 = call remote (literal erlang):(literal error)/2, literal function_clause, _4
  ret @ssa_ret:9
}
```

Now it's time to look at the resulting BEAM code. Here is the successful part of the
function:

    %% Block 0.
    {test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.

    %% Block 5.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {put_tuple,2,{x,1}}.
    {put,{atom,ok}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    return.

Since register allocation was done after the `is_tagged_tuple`
optimization, the `get_tuple_instruction` will extract the second
element of the tuple to the first available register, namely
`{x,0}`. That avoids any potential problem of registers being
undefined at a `test_heap` instruction. The `put_tuple` instruction
will put the built tuple into `{x,1}` since the following
`{put,{x,0}}` instruction still needs the contents of `{x,0}`. To
return the built tuple, the `{move,{x,1},{x,0}}` instruction just
before the `return` instruction copies the contents of `{x,1}`
to `{x,0}`.

It happens that for this particular example, the OTP 21 compiler will produce
slightly better code:

<pre class="highlight">
    {test,is_tagged_tuple,{f,1},[{x,0},4,{atom,tag}]}.
    {test_heap,3,1}.
    {get_tuple_element,{x,0},1,<b>{x,2}</b>}.
    {put_tuple,2,<b>{x,0}</b>}.
    {put,{atom,ok}}.
    {put,<b>{x,2}</b>}.
    return.
</pre>

(The tuple can be built to `{x,0}` directly, avoiding the `move`
instruction before the `return`.)

## Getting rid of the `move` instruction

Perhaps I should have chosen another example to avoid revealing that
the SSA-based compiler sometimes produces worse code than the old
compiler.

Anyway, now that the secret is out, let's see what can been done
about that extra `move` instruction.

Let's look at another example:

```erlang
make_tuple(A) ->
    {ok,A}.
```

The BEAM code produced by either the compiler in OTP 21
or the new SSA-based compiler looks like this:

    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,ok}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    return.

Clearly, the way the tuple building instructions work, it would be
impossible to avoid the `move` instruction. When building a tuple, the
destination register for the built tuple must not be the same
as one of the source registers. It seems that we will need
better instructions for constructing tuples if we are to avoid
the `move` instruction.

The problem doesn't exist when building a list:

    %% build_list(A) -> [A].
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,0}}.
    return.

The `put_list` instruction can safely place the built list into the
same register as either of the source registers.

Introducing a new `put_tuple2` instruction that builds a tuple in a
single instruction, the `move` instruction can be eliminated:

    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,ok},{x,0}]}}.
    return.

At the time of writing, the implementation of `put_tuple2` has not yet
been merged to the `master` branch, but can be found in [#1947:
Introduce a put_tuple2 instruction][pr1947].

## Next time

As we have seen, a variable in the SSA code can only be assigned
once (just as in Erlang). So how can the following code be
translated to SSA code?

```
bar(X) ->
    case X of
        none ->
            Y = 0;
        _ ->
            Y = X
    end,
    Y + 1.
```

## <a name="generating_listings"></a>How to generate listing files

To generate the unoptimized SSA code for a module, use the `dssa` option:

```
erlc +dssa blog.erl
```

The SSA code will be pretty printed into the file `blog.ssa`.

Use the `dssaopt` option to generate the optimized SSA code, printing
it to the file `blog.ssaopt`.

```
erlc +dssaopt blog.erl
```

To see how the SSA code looked when not all optimization passes had been
run, I used variations of the following command line

```
erlc +dssaopt +no_ssa_opt_type +no_ssa_opt_live +no_ssa_opt_merge_blocks blog.erl
```

Those options are intentionally not documented. Skipping optimization
is only intended for debugging or exploring how the optimization
passes work. Skipping some optimizations passes that are actually
mandatory will crash the compiler.

To find the names of the options for skipping passes, see the [list of
sub passes of `beam_ssa_opt`][passes] and add `no_` to the name of the
pass.

To generate `blog.S` with the BEAM code, use the `-S` option:

```
erlc -S blog.erl
```

To skip all SSA optimizations, use the `no_ssa_opt` option:

```
erlc +no_ssa_opt -S blog.erl
```

[pr1935]: https://github.com/erlang/otp/pull/1935
[pr1947]: https://github.com/erlang/otp/pull/1947
[otp]: https://github.com/erlang/otp
[intermediate]: https://en.wikipedia.org/wiki/Intermediate_representation
[ssa]: https://en.wikipedia.org/wiki/Static_single_assignment_form
[prev]: http://blog.erlang.org/opt-traps-and-pitfalls/
[sys_core_fold]: http://blog.erlang.org/core-erlang-optimizations/
[pessimization]: https://stackoverflow.com/questions/32618848/what-is-pessimization
[core_erlang]: http://blog.erlang.org/core-erlang-by-example/

[passes]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L49
[ssa_opt_split_blocks]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L100
[ssa_opt_element]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L119
[ssa_opt_record]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L194
[ssa_opt_live]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L683
[ssa_opt_merge_blocks]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L976
[ssa_opt_sink]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_opt.erl#L1025

[beam_ssa_type]: https://github.com/erlang/otp/blob/869537a9bf799c8d12fc46c2b413e532d6e3b10c/lib/compiler/src/beam_ssa_type.erl
