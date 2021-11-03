---
layout: post
title: SSA History
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post looks back on the development of
the [SSA-based intermediate representation][pr1935]
from the beginning of this year to the end
of August when the branch was merged.

## January 2018

In January this year we realized that we have reached
the limit of the optimizations that we could do working
on BEAM code.

[John][john] had finished the work on extending [`beam_bsm`][beam_bsm]
(a pass that attempts to [delay creation of sub
binaries][bin_matching]). The extended `beam_bsm` pass could apply the
optimization in a few more cases than it could before, but the amount
of code in `beam_bsm` to achieve that modest improvement of the
optimization was insane.

John, [Lukas][lukas], and I discussed what we should do about
it. Clearly, we needed a better intermediate format. But what should
it be? Could we use the existing BEAM code but with variables instead of
BEAM registers and do register allocation later? That would solve
some of the problems but not all of them. The irregular nature of
BEAM instructions makes it cumbersome to traverse and analyze
BEAM code.

So we decided to do like most modern compilers and use an [SSA][ssa]-based
intermediate format.

### Rewrites are scary!

Introducing a new intermediate format would require rewriting at
least some parts of the compiler. The problem with rewrites is
that they always take longer time than expected and that they often
get abandoned before they are finished.

To increase the odds that this rewrite would be successful, we come
up with this plan to do the minimum amount of work to get something working
as soon as possible:

1. Write a new pass that translates from [Kernel Erlang][kernel] to SSA code.

2. Write a new pass that translates from SSA code to BEAM code.

3. Keep all existing optimization passes.

4. Rewrite the optimization passes one at a time.

It didn't quite work out according to the plan, as will soon be evident.

## February 2018

I made the first the commit February 1 this year.

### beam_kernel_to_ssa

The first pass I wrote was the translator from Kernel Erlang to SSA code.
We named it `beam_kernel_to_ssa`.

My first thought was to write the pass from scratch, as opposed to
base it on `v3_codegen`. After all, there are fundamental differences
between BEAM code and SSA code. BEAM code is a flat list of instructions.
SSA code consists of blocks of numbered blocks stored in a map, and there
are also the phi nodes.

On the other hand, the input for both `v3_codegen` and
`beam_kernel_to_ssa` was Kernel Erlang. There was nothing wrong with
the code that handled the Kernel Erlang records and I didn't want to
rewrite that code from scratch.  Instead, I rewrote the part of the
code that produced a list of BEAM instructions to produce a list of
SSA instructions.  I then wrote a simple pass (about 100 lines of
code) that [packaged the SSA instructions into blocks and added the
phi nodes][beam_kernel_to_ssa_finalize].

### Testing beam_kernel_to_ssa

I prefer to test the code I write a soon as possible after writing it.
It is much easier to find and fix bugs in code that has been recently
written.

How can one test `beam_kernel_to_ssa` before the code generator for
BEAM code has been written?

One cannot, not completely, but there are ways to find major problems.

One such way is [smoke testing][software_smoke_testing]. I modified
the compiler so that it would first run `beam_kernel_to_ssa` but
discard its output, then run `v3_codegen` and the rest of the compiler
passes. That allowed me to run the entire compiler test suite, and
if the `beam_kernel_to_ssa` pass crashed, I've had found a bug.

Another way was to write a validator or linter of the SSA code.
[John][john] wrote the `beam_ssa_lint` pass (actually called
`beam_ssa_validator` at that time and later renamed), which would
verify that a variable was only defined once, that variables were
defined before they were used, that labels in terminators and phi
nodes referred to defined blocks, and so on. It helped me find a
few bugs.

[Dialyzer][dialyzer] also helped me find some bugs. I made sure
that I added types for all fields in all new records and
specifications for all exported functions. Dialyzer pointed out
some bugs when I ran it and thinking about the types when writing
the `-type` declarations was also useful.

### Finishing beam_kernel_to_ssa

I am not sure exactly how long time I spent on the initial
implementation of `beam_kernel_to_ssa`, but it was probably less
than two weeks. There were a few snags along the way, most of
them bugs in `v3_kernel` that did not cause any problems
with the old `v3_codegen`.

Here is an example. I chose to fix it in OTP 21 even though it was
harmless in that release:

[v3_kernel: Stop ensuring one return value in #k_try{}][v3_kernel_bug]

### beam_ssa_pre_codegen

Next up was the translation from SSA code to BEAM code.

I have already decided that the translation was sufficiently complicated
that to better be split into two major passes.

The first pass of those passes,
[`beam_ssa_pre_codegen`][beam_ssa_pre_codegen], would work on the SSA
code, rewriting it, and adding annotations for another pass that would
generate the BEAM code, but the output would still be valid SSA code
so that `ssa_lint` could be used to validate the output. The pretty-printed
SSA code also includes the annotations to facilitate debugging.

The `dprecg` option can be used to produce a pretty-printed listing of
the SSA code. The following command will create the file `blog.precodegen`:

```
erlc +dprecg blog.erl
```

The next section will dig deeper into the workings of `beam_ssa_pre_codegen`.
On a first reading, you might want to skip that section and jump ahead to
the section about [beam_ssa_codegen](#beam_ssa_codegen).

### Digging deeper in beam_ssa_pre_codegen

To provide some context for the description of `beam_ssa_pre_codegen`,
we will first look at some BEAM code and talk about stack frames and
Y registers.

Here is the example in Erlang:

```erlang
foo(C, L) ->
    Sum = lists:sum(L),
    C + Sum.
```

The BEAM code looks like this:

    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"blog.erl",5}]}.
    {call_ext,1,{extfunc,lists,sum,1}}.
    {line,[{location,"blog.erl",6}]}.
    {gc_bif,'+',{f,0},1,[{y,0},{x,0}],{x,0}}.
    {deallocate,1}.
    return.

As usual, we will walk through the code one or a few lines at a time.

    {allocate,1,2}.

The `allocate` instruction allocates a stack frame. The `1` operand
means that there should be room for one slot in the stack frame for
storing one value.  The slots in the stack frame are called *Y
registers*.

The `2` operand means that two X registers (`{x,0}` and `{x,1}`) are
live and must be preserved if `allocate` needs to do a garbage
collection in order to allocate space for the stack frame.

    {move,{x,0},{y,0}}.

The `C` argument for `foo/2` is in `{x,0}`. The `move` instruction
copies the value of `{x,0}` to `{y,0}`, which is the zeroth slot
in the stack frame. The reason for doing this copy will soon become
clear.

    {move,{x,1},{x,0}}.

Preparing for the call of `lists:sum/1`, the value of `L` in `{x,1}`
is copied to `{x,0}`.

    {line,[{location,"blog.erl",5}]}.
    {call_ext,1,{extfunc,lists,sum,1}}.

Here `lists:sum/1` is called. The argument is in `{x,0}`. The result
(the sum of all numbers in the list) is returned in `{x,0}`. Also,
the contents of all other X registers are destroyed. That means that
any value that is to be used after a function call must be saved to
a Y register.

    {gc_bif,'+',{f,0},1,[{y,0},{x,0}],{x,0}}.

This instruction calculates the sum of `C` (in `{y,0}`) and `Sum` (in `{x,0}`),
storing the result in `{x,0}`.

    {deallocate,1}.

Preparing to return from the function, the `deallocate` instruction
removes the stack frame that `allocate` created.

    return.

`return` returns from the function. The return value is in `{x,0}`.

Here is the SSA code for the function:

```
function blog:foo(_0, _1) {
0:
  %% blog.erl:5
  _2 = call remote (literal lists):(literal sum)/1, _1

  %% blog.erl:5
  _3 = bif:'+' _0, _2
  @ssa_bool = succeeded _3
  br @ssa_bool, label 3, label 1

3:
  ret _3

1:
  @ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  ret @ssa_ret
}
```

After running `beam_ssa_pre_codegen`, the SSA code looks like this:

```
function blog:foo(x0/_0, x1/_1) {
  %% _0: 0..1
  %% _1: 0..1 0..3
%% #{frame_size => 1,yregs => [0]}
0:
  %% _0:4: 1..5
  [1] y0/_0:4 = copy x0/_0

  %% blog.erl:5
  %% _2: 3..5
  [3] x0/_2 = call remote (literal lists):(literal sum)/1, x1/_1

  %% blog.erl:5
  %% _3: 5..11
  [5] x0/_3 = bif:'+' y0/_0:4, x0/_2

  %% @ssa_bool: 7..9
  [7] z0/@ssa_bool = succeeded x0/_3
  [9] br z0/@ssa_bool, label 3, label 1

3:
  [11] ret x0/_3

1:
  %% @ssa_ret: 13..15
  [13] x0/@ssa_ret = call remote (literal erlang):(literal error)/1, literal badarg
  [15] ret x0/@ssa_ret
}
```

We will describe what the important (for this example) sub passes of
`beam_ssa_pre_codegen` do, and point to the relevant part of code while
doing so.

The sub pass [place_frames] determines where stack frames should be allocated.
In the example, block 0 needs a stack frame.

The sub pass [find_yregs] determines which variables that are to be
placed in Y registers. The result will be a `yregs` annotation added
to each block that allocates a stack frame. For this example, the
annotation will look like:

<pre class="highlight">
    %% #{frame_size => 1,<b>yregs => [0]</b>}
</pre>

Variable `_0` is `C` from the Erlang code. It needs to be saved across the
call to `lists:sum/1`.

The sub pass [reserve_yregs] uses the `yregs` annotations and inserts `copy` instructions
to copy each variable that needs saving to a new variable. For the example,
the following instruction will be added

```
  [1] y0/_0:4 = copy x0/_0
```
It copies the value of `_0` to `_0:4`.

The sub pass [number_instructions] numbers all instructions as a preparation for register
allocation. In the listing, those numbers are in brackets before each instruction:
`[1]`, `[3]`, `[5]`, and so on.

The sub pass [live_intervals] calculates the intervals in which each variable is live.
In the listing, the live intervals are shown as comments before the definition
of the variable:

```
  %% _0:4: 1..5
  [1] y0/_0:4 = copy x0/_0
```

The variable `_0:4` is live from instruction `[1]` (its definition) to
`[5]` (its last use).

The sub pass [linear_scan] uses the [linear scan][linear_scan_polleto] algorithm
to allocate registers for each variable. The result is saved as annotation
for the function. In the listing of the SSA code, the register will be added
to the definition and each use of a variable. For example:

```
  [1] y0/_0:4 = copy x0/_0
```

Variable `_0` (the argument `L`) is in `{x,0}`. Its copy in `_0:4` is in
`{y,0}`.

But what is `z0`?

<pre class="highlight">
      [7] <b>z0</b>/@ssa_bool = succeeded x0/_3
      [9] br <b>z0</b>/@ssa_bool, label 3, label 1
</pre>

`succeeded` is not a BEAM instruction. It will be combined with the previous
instruction (`bif:+` in this example) and the `br` instruction that follows it
to the following BEAM instruction:

    {gc_bif,'+',{f,0},1,[{y,0},{x,0}],{x,0}}.

Thus, the value `@ssa_bool` is never explicitly stored in a BEAM
register.  Before I invented Z registers, `@ssa_bool` would have been
assigned to an X register.  That worked most of the time, but sometimes
an X register would seem to be occupied when it was not, and prevent
another instruction from using that register.

Here are the [references that I used when implementing linear scan][linear_scan_references].

The sub pass [frame_size] uses the information from the linear scan pass to calculate the size
of each stack frame. The result is stored as an annotation:

<pre class="highlight">
    %% #{<b>frame_size => 1</b>,yregs => [0]}
</pre>

[place_frames]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L715

[find_yregs]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1114

[reserve_yregs]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1645

[number_instructions]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1487

[live_intervals]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1515

[linear_scan]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L2118

[frame_size]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1741

### beam_ssa_codegen

The [`beam_ssa_codegen`][beam_ssa_codegen] pass generates BEAM code
from the annotated SSA code. Testing of this pass was easier, because
I could compile some sample code and try to run it.

Often I did not even have to run the code to know that it was wrong.
The compiler would tell me, loudly:

{% raw %}
```
blog: function bar/2+4:
  Internal consistency check failed - please report this bug.
  Instruction: {test_heap,2,3}
  Error:       {{x,2},not_live}:
```
{% endraw %}

It's time to introduce the `beam_validator` pass.

#### beam_validator

The [`beam_validator`][beam_validator] pass was introduced in one of
the R10B releases (probably in 2006). It is run directly before the
BEAM code is packaged into a binary and written to a BEAM file. The
purpose of `beam_validator` is to find unsafe instructions that
could crash the runtime system or cause it to misbehave in other
ways.

Let's look at a simple example:

```erlang
bar(H, T) ->
    [H|T].
```

Here is the BEAM code, but edited by me to contain an unsafe instruction:

<pre class="highlight">
      {label,4}.
        {test_heap,2,<b>3</b>}.
        {put_list,{x,0},{x,1},{x,0}}.
        return.
</pre>

The number of live registers is here given as `3` instead of `2`.
That means that `{x,0}`, `{x,1}`, and `{x,2}` are supposed to contain
valid Erlang terms. Because `bar/2` is only called with two arguments,
`{x,2}` can contain any old garbage.

When running this code, it could crash the runtime system, or it could
be completely harmless. It depends on whether there will be a garbage
collection during execution of the `test_heap` instruction, and on the
exact nature of the garbage in `{x,2}`. For example, if the garbage
happens to be an atom nothing bad will happen. That means that this
type of compiler bug is difficult to reliably catch in a test case.

`beam_validator` will find this bug immediately. It keeps track of
which registers are initialized at any point in the function. If it
finds a reference to a register that is not initialized it will
complain:

{% raw %}
```
blog: function bar/2+4:
  Internal consistency check failed - please report this bug.
  Instruction: {test_heap,2,3}
  Error:       {{x,2},not_live}:
```
{% endraw %}

#### Friend and foe

During the implementation of `beam_ssa_codegen`, the `beam_validator`
pass pointed out many bugs for me. It was my friend.

It was also my foe, sort of. It would complain that some perfectly safe
code was unsafe. When that happened, I had to thoroughly investigate the
code to make doubly sure it was safe, and then extend `beam_validator`
to make it smarter so that it would understand that the code was safe.

Here is one example where I had to make `beam_validator` smarter.
Consider this code:

    {move,{x,0},{y,0}}.
    {test,is_map,{f,777},[{x,0}]}.
    {put_map_assoc,{f,0},{y,0},...}.

The `move` instruction stores a copy of `{x,0}` in `{y,0}` (a location
on the stack). The following `test` instruction tests whether `{x,0}`
is a map and branches to label 777 if not. The `put_map_assoc` instruction
updates the map in `{y,0}`.

The `put_map_assoc` instruction will crash if its source argument is
not a map.  Therefore, `beam_validator` complains if `put_map_assoc` is
used with a source argument that is not a map. In this example,
`beam_validator` had not seen a `test` instruction that ensured that
`{y,0}` was a map, so it complained. It is obvious (for a human) that
`{y,0}` is a map because it is a copy of `{x,0}`, which is a map.

`v3_codegen` never generated such code; in fact, it explicitly
[avoided generating such code][v3_codegen_kludge].
I did not want to add similar kludges to the new code generator, so
[`beam_validator` had to become smarter][beam_validator_maps].

[v3_codegen_kludge]: https://github.com/erlang/otp/blob/64422fcac9c602641dcf24ef2d35e3491376304d/lib/compiler/src/v3_codegen.erl#L1600

#### Unsafe optimization passes

Some of the unsafe code that `beam_validator` found was really unsafe,
but it was not the fault of my new compiler passes, but of the
optimization passes that optimized the generated BEAM code.

The problem was that some of the optimization passes had implicit
assumptions of the kind of code that `v3_codegen` would generate
(or, rather, would **not** generate). The new code generator broke
those assumptions.

At first, when I saw those bugs, I removed the broken part of the
optimization pass. Making the optimizations safe would be non-trivial
and ultimately wasted work because we intended to rewrite all those
optimization passes to work on SSA code.

When I have seen a few too many of those unsafe optimizations, [I ripped
out all of the unsafe optimization passes][unsafe_passes].

That meant that we would have to re-implement all of the optimizations
before the generated code would be as good as the code from the old
compiler. I had also noticed that the new BEAM code generator in a
few ways generated better code than the old one, but in other ways the
code was worse. For example, the generated code used more stack space and
did a lot of register shuffling. Eventually, that had to be addressed in
some way.

Meanwhile, I had worse problems to worry about.

## March 2018

On March 14 I presented my progress on the new compiler passes for
the OTP team. One of my slides had the following text:

> * Can compile all modules in OTP (and run many of them correctly)

Yes, I had finished the initial implementation of `beam_ssa_codegen`
so that I could compile all code in OTP.

The problem that I only at hinted in the slide was that Erlang could
crash and dump core when running test suites. Not every time, and
never in the same test case twice. It only happened when I have
compiled OTP with the new compiler.

The crash didn't seem related to the test cases themselves, but to the
writing of log files. I soon narrowed it down to that the crash could
happen if [`file_io_server`][file_io_server] had been compiled with
the new compiler passes. However, that was not much help. The module
contains complicated code that uses the binary syntax, `try`/`catch`,
and `receive`, all of which are complicated instructions that might
not be correctly translated by the new compiler passes.

`beam_validator` was supposed to catch those kinds of bugs before
they can cause a crash. Either there was some kind of bug that
`beam_validator` didn't look for, or there was a bug in the
implementation of some of the instructions in the BEAM interpreter.

I ended up spending the rest of March trying to hunt down that bug.

[file_io_server]: https://github.com/erlang/otp/blob/OTP-21.0.9/lib/kernel/src/file_io_server.erl

## April 2018

At the beginning of April, the bug still eluded me. I had narrowed
it down somewhat. I was pretty sure it had something to do with
`receive`.

Then [Rickard] gave me some information that I could connect to another
piece of information that I had absorbed during my hunt for the bug.

### The bug

This section is somewhat advanced, and if you wish you can
skip to [the fix](#the-fix).

Reading about the [Erlang Garbage Collector][lukas_gc] can give some
background to better understand this section.

Rickard reminded me about the `message_queue_data` option that
had been added to [`process_flag/2`][process_flag] in OTP 19. After
calling `process_flag(message_queue_data, off_heap)` all messages
that have not yet been received would be stored outside the process
heap. Storing the messages outside the heap means that the garbage
collector doesn't have to spend time copying the unreceived messages
during garbage collection, which can be a huge win for processes that
have many messages in its message queue.

The implementations details of messages outside the heap are
crucial. Consider this selective `receive`:

```erlang
receive
    {tagged_message,Message} -> Message
end.
```

When the BEAM interpreter executes this code, it will retrieve a
reference message from the external message queue and match it against
the tuple pattern. If the message does not match, the next message
will be processed in the same way, and so on.

If a message does not match, there **must not** be any remaining
references to it stored on the stack. The reason is that if there is a
garbage collection, the garbage collector will copy the message (or
part of the message) to the heap, and, even worse, it will destroy the
original message during the copy operation. The message is still in
the external message queue, but it has now been corrupted by the
garbage collector. If the message is later matched out in a `receive`,
it will likely cause a crash.

When Rickard first implemented off-heap messages, he asked me whether
the compiler could ever store references to unreceived messages
on the stack. I assured him that it could not happen.

Yes, that was true, it could not happen because of the way
`v3_codegen` generated the code for `receive`.

With the new compiler passes, [it **could** happen][bad_receive]. When
I first discussed the bug with Richard in March, he did mention that
it is forbidden to store references to off-heap messages on the
stack. At that time, I was not aware that the compiler could store
references to off-heap messages on the stack.

When Rickard reminded me about that for the second time in April, I remember
seeing during my bug hunt generated code that stored off-heap message
references on the stack.

[rickard]: https://github.com/rickard-green
[process_flag]: http://erlang.org/doc/man/erlang.html#process_flag-2
[lukas_gc]: https://github.com/erlang/otp/blob/OTP-21.0.9/erts/emulator/internal_doc/GarbageCollection.md
[bad_receive]: https://github.com/erlang/otp/blob/333e4c5a1406cdeb9d1d5cf9bf4a4fadb232fca8/lib/compiler/test/beam_validator_SUITE_data/receive_stacked.S#L22

### The fix

After finding the reason for the bug, I first taught `beam_validator`
to [complain about "fragile references" on the stack][beam_validator_fragile].
I included that commit in OTP 21.

I then added a sub pass to `beam_ssa_pre_codegen` to [rewrite `receive`][receive_fix].
It introduces new variables and `copy` instructions to ensure that
any references to the message being matched are kept in X registers.

With no known bugs in the code generator, I could start rewriting the optimization
passes I had removed.

[receive_fix]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L919

### More optimizations

#### beam_ssa_recv

[`beam_ssa_recv`][beam_ssa_recv] is a replacement for the unsafe
[`beam_receive`][beam_receive] pass.  The purpose is to optimize a
`receive` that can only match a newly created reference.  The
optimization avoids scanning the messages that were placed in the
message queue before the reference was created.

I actually wrote `beam_ssa_recv` at the beginning of March as an
experiment to see how easy it would be to write optimizations of SSA code.
It turned out to be pretty easy. `beam_ssa_recv` can apply the optimization
in more places than `beam_receive` could, using slightly less code.

In the old `beam_receive` pass, a lot of code is needed to handle
the many variants of BEAM instructions. For example, in
[`opt_update_regs/3`][beam_ssa_recv_opt_update_regs] there are three
clauses just to handle three variants of a `call` instruction (calling
a local function, calling an external function, and calling a fun).

Here is an example of a function that `beam_receive` did not optimize, but
[`beam_ssa_recv` can optimize][yes_14].

[beam_receive]: https://github.com/erlang/otp/blob/OTP-21.0.9/lib/compiler/src/beam_receive.erl
[beam_ssa_recv]: https://github.com/erlang/otp/blob/367f4a3fabb12cda3f2547e9908acbf28cb34e3a/lib/compiler/src/beam_ssa_recv.erl
[beam_ssa_recv_opt_update_regs]: https://github.com/erlang/otp/blob/333e4c5a1406cdeb9d1d5cf9bf4a4fadb232fca8/lib/compiler/src/beam_receive.erl#L185
[yes_14]: https://github.com/erlang/otp/blob/6bee2ac7d11668888d93ec4f93730bcae3e5fa79/lib/compiler/test/receive_SUITE_data/ref_opt/yes_14.erl

#### beam_ssa_opt

The [`beam_ssa_opt`][beam_ssa_opt] pass runs a [number of
optimizations][beam_ssa_opt_passes]. Many of the optimizations are replacements
for the optimizations I removed earlier.

[beam_ssa_opt]: https://github.com/erlang/otp/blob/81d34181d391709e9d2c404fa730ee9b5c72b5e3/lib/compiler/src/beam_ssa_opt.erl

[beam_ssa_opt_passes]: https://github.com/erlang/otp/blob/81d34181d391709e9d2c404fa730ee9b5c72b5e3/lib/compiler/src/beam_ssa_opt.erl#L49

#### beam_ssa_type

[`beam_ssa_type`][beam_ssa_type] replaces the unsafe [`beam_type`][beam_type] pass.

The `beam_type` pass did a local type analysis (basically for extended basic blocks),
and tried to simplify the code, for example by removing unnecessary type tests.

The `beam_ssa_type` pass analyzes the types in an entire function and
simplifies the code, for example by removing unnecessary type
tests. It finds many more opportunities for optimizations than
`beam_type` did.

[beam_ssa_type]: https://github.com/erlang/otp/blob/81d34181d391709e9d2c404fa730ee9b5c72b5e3/lib/compiler/src/beam_ssa_type.erl

## May 2018

At the beginning of May, [John][john] started working on what was to become
this pull request:

[#1958: Rewrite BSM optimizations in the new SSA-based intermediate format][pr1958]

I continued to write optimizations and fix bugs that John found while
developing his optimizations.

### Rethinking the binary matching instructions

While working on his binary optimizations, John realized that the SSA
instructions for binary matching were difficult to optimize. The
binary match instructions I had designed were close to the semantics
of the BEAM instructions. John suggested that the `bs_get` instruction
should be broken up into a `bs_match` instruction and a `bs_extract`
instruction to simplify optimizations.

The breaking up of the instructions meant that [`beam_ssa_pre_codegen`
would have to work harder to combine
them][beam_ssa_pre_codegen_fix_bs], but it vastly simplified John's
optimizations. It turned out that it also enabled other optimizations:
the [liveness optimizations][beam_ssa_opt_live] could remove unused
instructions more aggressively.

[beam_ssa_pre_codegen_fix_bs]: https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_ssa_pre_codegen.erl#L209
[beam_ssa_opt_live]: https://github.com/erlang/otp/blob/e6c3dd9f701d354c06b9b1b043a3d7e9cc050b1c/lib/compiler/src/beam_ssa_opt.erl#L777

On the first day of [Code BEAM STO 2018][code_beam_2018] May 31, I didn't
know of any bugs in the new compiler passes and my list of optimizations to
re-implement was shrinking steadily. I met [Michał Muskała][michal]
(a frequent contributor to Erlang/OTP and a member of the
[Elixir Core Team][elixir_core_team]) there and told him about my work
on the compiler and that it was stable enough be tested outside OTP,
for example to compile Elixir code...

[pr1958]: https://github.com/erlang/otp/pull/1958
[code_beam_2018]: https://codesync.global/conferences/code-beam-sto-2018/
[elixir_core_team]: https://elixirforum.com/groups/Elixir-Core-Team

## June 2018

### Michał's feedback

I received an email from Michał in the middle of June. He had tried out
my compiler branch. He wrote:

> First impression is that it took a loooong time to compile Elixir's unicode module,
so long that I had to shut it down after about 10 minutes.

He sent me an Erlangified version of [Elixir's unicode module][elixir_unicode].
The size of the Erlang source for the module was almost 82,000 lines or
about 3,700,000 bytes. Based on the size, compilation could be expected to be
a little bit slow, but not that slow. On his computer, OTP 21.0-RC2 finished
the compilation in 16 seconds.

I compiled the module using the `time` option. The slowest pass was `beam_ssa_type`.
After some further profiling, I found the bottleneck in the joining of two maps.
Here is the [corrected code][beam_ssa_type_bottleneck]. The original code didn't compare
the size of maps and swap them as needed. I might have done some other improvements,
too. Anyway, that took care of that bottleneck. Now `beam_ssa_pre_codegen` was the
slowest pass.

I fixed several bottlenecks in the [linear scan sub pass][linear_scan], and after
that some other bottleneck in `beam_ssa_pre_codegen`. I think that reduced the
compilation time to well under one minute.

[beam_ssa_type_bottleneck]: https://github.com/erlang/otp/blob/81d34181d391709e9d2c404fa730ee9b5c72b5e3/lib/compiler/src/beam_ssa_type.erl#L944

### Optimizing code generation

After having finished the re-implementation of the last optimization
pass (I think it was the [optimization of floating point
operations][beam_ssa_opt_float], as previously done by the unsafe
[`beam_type`][beam_type] pass), I started to compare the code
generated by OTP 21 with code from the new compiler passes.

I used [scripts/diffable][diffable], which compiles about 1000
modules from OTP to BEAM code and massages the BEAM code to make it more friendly
for diffing. I then ran `diff -u old new` to compare the new code to
the old code.

In the last part of June and the first week of July, I then improved
`beam_ssa_pre_codegen` and `beam_ssa_codegen` to address the issues that I
noticed when reading the diff.

#### `beam_ssa_pre_codegen` improvements

I did not change the [linear scan][linear_scan] sub pass of `beam_ssa_pre_codegen`
itself. Instead I added transformations of the SSA code that would help
linear scan do a better job of allocating registers.

The most obvious issue I noticed was unnecessary `move` instructions.
Here are two of the sub passes I added to address that issue:

* [reserve_xregs] gives hints to the linear scan sub pass that a
certain X register should be used for a certain variable, if possible.

* [opt_get_list] tries to eliminate the extra `move` instruction that
is frequently added when matching out elements from a list.  See the
comments in the code for an example and an explanation.

Another frequent issue was that the code generated from the new code
generator used more stack space because two variables that were not
strictly live at the same time were allocated different Y registers
(slots on the stack) instead of re-using the same Y register. I addressed
that issue in [copy_retval]. See the comments in the code for an example.

#### `beam_ssa_codegen` improvements

Michał noticed that when a value was stored in both an X register and
a Y register (on the stack), instructions using the value would
always use the Y register. The old code generator would use the X
register.  The new code could be slower because the BEAM interpreter
is generally optimized for operands being in X registers.

I added [prefer_xregs] to address that issue. See the comments in the code
for examples.

[reserve_xregs]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1916
[copy_retval]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1222
[opt_get_list]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L1422

[prefer_xregs]: https://github.com/erlang/otp/blob/7fbb86c77fa99caddabedfb992f47ddeece80652/lib/compiler/src/beam_ssa_codegen.erl#L359

[beam_type]: https://github.com/erlang/otp/blob/OTP-21.0.9/lib/compiler/src/beam_type.erl

[beam_ssa_opt_float]: https://github.com/erlang/otp/blob/e6c3dd9f701d354c06b9b1b043a3d7e9cc050b1c/lib/compiler/src/beam_ssa_opt.erl#L517

## July 2018

Vacation.

## August 2018

Before I left for vacation, it seemed that the new compiler passes generally
generated code at least as good as the old compiler passes. In some cases,
the code would be much better.

Back after my vacation, I did some final polishing.

On Aug 17 I created a [pull request][pr1935].

Before merging the pull request, I sneaked in a few final optimizations.

On Aug 24 I [merged][merged_ssa] the pull request.

[merged_ssa]: https://github.com/erlang/otp/commit/9facb02b91979ef90b47ac0a54d1eb71fdaa1ee1

[beam_ssa_pre_codegen]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl
[beam_ssa_codegen]: https://github.com/erlang/otp/blob/ec1f35c9f52be894ba295b9a48237020855e3c46/lib/compiler/src/beam_ssa_codegen.erl
[beam_validator]: https://github.com/erlang/otp/blob/e2a939dc4d23d75a0588722d0a08aef129b4c0be/lib/compiler/src/beam_validator.erl

[v3_kernel_bug]: https://github.com/erlang/otp/commit/c896f08f5c028b1e31290e6a5502597401acd39f
[beam_validator_fragile]: https://github.com/erlang/otp/commit/90853d8e7b50be13a3b71f4a1ed6b0407e1f7c2f
[unsafe_passes]: https://github.com/erlang/otp/commit/3fc40fd57fa01b097b4c363860c4d4762e13db8b
[beam_validator_maps]: https://github.com/erlang/otp/commit/1f221b27f1336e747f7409692f260055dd3ddf79

[john]: https://github.com/jhogberg
[lukas]: https://github.com/garazdawi
[michal]: https://github.com/michalmuskala

[elixir_unicode]: https://github.com/elixir-lang/elixir/blob/54cb02c2407856f4063c75a440507dacb6a31dbc/lib/elixir/unicode/unicode.ex
[diffable]: https://github.com/erlang/otp/blob/master/scripts/diffable

[pr1935]: https://github.com/erlang/otp/pull/1935

[bin_matching]: http://erlang.org/doc/efficiency_guide/binaryhandling.html#matching-binaries

[ssa]: https://en.wikipedia.org/wiki/Static_single_assignment_form

[kernel]: http://blog.erlang.org/beam-compiler-history#r6b-enter-kernel-erlang

[beam_bsm]: https://github.com/erlang/otp/blob/2e40d8d1c51ad1c3d3750490ecac6b290233f085/lib/compiler/src/beam_bsm.erl

[software_smoke_testing]: https://en.wikipedia.org/wiki/Smoke_testing_(software)

[beam_kernel_to_ssa_finalize]: https://github.com/erlang/otp/blob/6bee2ac7d11668888d93ec4f93730bcae3e5fa79/lib/compiler/src/beam_kernel_to_ssa.erl#L1231

[dialyzer]: http://erlang.org/doc/apps/dialyzer/index.html

[linear_scan_polleto]: http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
[linear_scan_references]: https://github.com/erlang/otp/blob/494cb3be4a98653c212d673008085bc3ea70dc7e/lib/compiler/src/beam_ssa_pre_codegen.erl#L52

## The Future

The SSA-based intermediate representation provides a solid framework for
future improvements of the compiler. After the merging of the pull request in
August, several pull requests have already added further improvements:

* [Introduce a put_tuple2 instruction][pr1947]

* [Replace beam_dead with beam_ssa_dead][pr1955]

* [Rewrite BSM optimizations in the new SSA-based intermediate format][pr1958]

* [Clean up variable-related cruft in new SSA passes][pr1959]

* [beam_validator: Use set_aliased_type in more operations][pr1960]

* [Minor cleanups and bug fixes of the compiler][pr1965]

Here is a list of possible further improvements that could be implemented either
by OTP members or external contributors before OTP 22 is released:

* Rewrite `sys_core_dsetel` to be SSA-based.

* Rewrite the guard optimizing sub pass `guard_opt/2` in `v3_kernel`
  to an SSA-based optimization pass.

* Rewrite `beam_trim`. It would probably have to be a part of `beam_ssa_codegen`.

* Optimize `switch` branches. If two branches jump to blocks
  that do the same thing, let both branches jump to the same
  block. `beam_jump` does this kind of optimization, but doing it
  earlier in the SSA representation could speed up compilation of functions
  with many clauses.

* Get rid of the `beam_utils` module, especially the `is_killed()` and
  `is_not_used()` family of functions. The functions in `beam_utils`
  used by `beam_jump` could be moved into `beam_jump`.

* Rewrite `beam_bs` to be SSA-based. This rewrite might not improve
  the generated code, but it might speed up compilation of modules
  with heavy use of the binary syntax.

[pr1947]: https://github.com/erlang/otp/pull/1947
[pr1955]: https://github.com/erlang/otp/pull/1955
[pr1959]: https://github.com/erlang/otp/pull/1959
[pr1960]: https://github.com/erlang/otp/pull/1960
[pr1965]: https://github.com/erlang/otp/pull/1965
