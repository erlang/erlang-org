---
layout: post
title: The Optimizations in Erlang/OTP 27
tags: BEAM JIT
author: Björn Gustavsson
---

This post explores the new optimizations for record updates as well as
some of the other improvements. It also gives a brief historic
overview of recent optimizations leading up to Erlang/OTP 27.

### A brief history of recent optimizations

The modern history of optimizations for Erlang begins in
January 2018. We had realized that we had reached the limit of the
optimizations that were possible working on [BEAM
code](https://www.erlang.org/blog/a-brief-beam-primer/) in the Erlang
compiler.

* Erlang/OTP 22 introduced a new [SSA-based intermediate
  representation](https://en.wikipedia.org/wiki/Static_single-assignment_form)
  in the compiler. Read the full story in [SSA
  History](https://www.erlang.org/blog/ssa-history/).

* Erlang/OTP 24 introduced the [JIT (Just In Time
  compiler)](https://www.erlang.org/blog/a-first-look-at-the-jit/),
  which improved performance by emitting native code for BEAM instructions
  at load-time.

* Erlang/OTP 25 introduced [type-based optimization in the
  JIT](https://www.erlang.org/blog/type-based-optimizations-in-the-jit/),
  which allowed the Erlang compiler to pass type information to the
  JIT to help it emit better native code. While that improved the
  native code emitted by the JIT, limitations in both the compiler and
  the JIT prevented the JIT to take full advantage of the type information.

* Erlang/OTP 26 [improved the type-based
  optimizations](https://www.erlang.org/blog/more-optimizations/).
  The most noticeable performance improvements were matching and
  construction of binaries using the bit syntax. Those improvements,
  combined with changes to the `base64` module itself, made encoding
  to Base64 about 4 times as fast and decoding from Base64 more than 3
  times as fast.

### What to expect of the JIT in Erlang/OTP 27

The major compiler and JIT improvement in Erlang/OTP 27 is
optimization of record operations, but there are also many smaller
optimizations that make the code smaller and/or faster.

### Please try this at home!

While this blog post will show many examples of generated code, I have
attempted to explain the optimizations in English as well. Feel free
to skip the code examples.

On the other hand, if you want more code examples...

To examine the native code for loaded modules, start the runtime system like this:

```bash
erl +JDdump true
```

The native code for all modules that are loaded will be dumped to files with the
extension `.asm`.

To examine the BEAM code for a module, use the `-S` option when
compiling. For example:

```bash
erlc -S base64.erl
```

### A simple record optimization

To get started, let's look at a simple record optimization that was not done
in Erlang/OTP 26 and earlier. Suppose we have this module:


```erlang
-record(foo, {a,b,c,d,e}).

update(N) ->
    R0 = #foo{},
    R1 = R0#foo{a=N},
    R2 = R1#foo{b=2},
    R2#foo{c=3}.
```

Here is [BEAM code](https://www.erlang.org/blog/a-brief-beam-primer/) for the
record operations:

```
    {update_record,{atom,reuse},
                   6,
                   {literal,{foo,undefined,undefined,undefined,undefined,
                                 undefined}},
                   {x,0},
                   {list,[2,{x,0}]}}.
    {update_record,{atom,copy},6,{x,0},{x,0},{list,[3,{integer,2}]}}.
    {update_record,{atom,copy},6,{x,0},{x,0},{list,[4,{integer,3}]}}.
```

That is, all three record update operations have been retained as separate
[`update_record`](https://www.erlang.org/blog/more-optimizations/#updating-records-in-otp-26)
instructions. Each operation creates a new record by copying the unchanged parts of the
record and filling in the new values in the correct position.

The compiler in Erlang/OTP 27 will essentially rewrite `update/1` to:

```erlang
update(N) ->
    #foo{a=N,b=2,c=3}.
```

which will produce the following BEAM code for the record creation:

```
    {put_tuple2,{x,0},
                {list,[{atom,foo},
                       {x,0},
                       {integer,2},
                       {integer,3},
                       {atom,undefined},
                       {atom,undefined}]}}.
```

Those optimizations were implemented in the following pull requests:

* [#7491: Merge consecutive record updates](https://github.com/erlang/otp/pull/7491)

* [#8086: Combine creation of a record with subsequent record updates](https://github.com/erlang/otp/pull/8086)

### Updating records in place

To explore the more sophisticated record optimization introduced in Erlang/OTP 27,
consider this example:

```erlang
-module(count1).
-export([count/1]).

-record(s, {atoms=0,other=0}).

count(L) ->
    count(L, #s{}).

count([X|Xs], #s{atoms=C}=S) when is_atom(X) ->
    count(Xs, S#s{atoms=C+1});
count([_|Xs], #s{other=C}=S) ->
    count(Xs, S#s{other=C+1});
count([], S) ->
    S.
```

`count(List)` counts the number of atoms and the number of other terms in the
given list. For example:

```erlang
1> -record(s, {atoms=0,other=0}).
ok
2> count1:count([a,b,c,1,2,3,4,5]).
#s{atoms = 3,other = 5}
```

Here follows the BEAM code emitted for `count/2`:

```
    {test,is_nonempty_list,{f,6},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,0}}.
    {test,is_atom,{f,5},[{x,2}]}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {gc_bif,'+',{f,0},3,[{tr,{x,2},{t_integer,{0,'+inf'}}},{integer,1}],{x,2}}.
    {test_heap,4,3}.
    {update_record,{atom,inplace},
                   3,
                   {tr,{x,1},
                       {t_tuple,3,true,
                                #{1 => {t_atom,[s]},
                                  2 => {t_integer,{0,'+inf'}},
                                  3 => {t_integer,{0,'+inf'}}}}},
                   {x,1},
                   {list,[2,{tr,{x,2},{t_integer,{1,'+inf'}}}]}}.
    {call_only,2,{f,4}}. % count/2
  {label,5}.
    {get_tuple_element,{x,1},2,{x,2}}.
    {gc_bif,'+',{f,0},3,[{tr,{x,2},{t_integer,{0,'+inf'}}},{integer,1}],{x,2}}.
    {test_heap,4,3}.
    {update_record,{atom,inplace},
                   3,
                   {tr,{x,1},
                       {t_tuple,3,true,
                                #{1 => {t_atom,[s]},
                                  2 => {t_integer,{0,'+inf'}},
                                  3 => {t_integer,{0,'+inf'}}}}},
                   {x,1},
                   {list,[3,{tr,{x,2},{t_integer,{1,'+inf'}}}]}}.
    {call_only,2,{f,4}}. % count/2
  {label,6}.
    {test,is_nil,{f,3},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.
```

The first two instructions test whether the first argument in `{x,0}` is a non-empty list
and if so extracts the first element of the list:

```
    {test,is_nonempty_list,{f,6},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,0}}.
```

The next instruction tests whether the first element is an atom. If not, a jump
is made to the code for the second clause.

```
    {test,is_atom,{f,5},[{x,2}]}.
```

Next the counter for the number of atoms seen is fetched from the record and
incremented by one:

```
    {get_tuple_element,{x,1},2,{x,2}}.
    {gc_bif,'+',{f,0},3,[{tr,{x,2},{t_integer,{0,'+inf'}}},{integer,1}],{x,2}}.
```

Next follows allocation of heap space and the updating of the record:

```
    {test_heap,4,3}.
    {update_record,{atom,inplace},
                   3,
                   {tr,{x,1},
                       {t_tuple,3,true,
                                #{1 => {t_atom,[s]},
                                  2 => {t_integer,{0,'+inf'}},
                                  3 => {t_integer,{0,'+inf'}}}}},
                   {x,1},
                   {list,[3,{tr,{x,2},{t_integer,{1,'+inf'}}}]}}.
```

The `test_heap` instruction ensures that there is sufficient room on the heap
for copying the record (4 words).

The `update_record` instruction was introduced in Erlang/OTP 26. Its
first operand is an atom that is a hint from the compiler to help the
JIT emit better code. In Erlang/OTP 26 the hints `reuse` and `copy`
are used. For more about those hints, see
[Updating records in OTP 26](https://www.erlang.org/blog/more-optimizations/#updating-records-in-otp-26).

In Erlang/OTP 27, there is a new hint called `inplace`. The compiler
emits that hint when it has determined that nowhere in the runtime
system is there another reference to the tuple except for the
reference used for the `update_record` instruction. In other
words, from the **compiler's** point of view, if the runtime system
were to directly update the existing record without first copying it,
the observable behavior of the program would not change. As soon will
be seen, from the **runtime system's** point of view, directly updating
the record is not always safe.

This new optimization was implemented by Frej Drejhammar. It builds
on and extends the compiler passes added in Erlang/OTP 26 for
[appending to a binary](https://www.erlang.org/blog/more-optimizations/#appending-to-binaries-in-otp-26).

Now let's see what the JIT will do when a `record_update` instruction
has an `inplace` hint. Here is the complete native code for the
instruction:

```
# update_record_in_place_IsdI
    mov rax, qword ptr [rbx+8]
    mov rcx, qword ptr [rbx+16]
    test cl, 1
    short je L38           ; Update directly if small integer.

    ; The new value is a bignum.
    ; Test whether the tuple is in the safe part of the heap.

    mov rdi, [r13+480]     ; Get the high water mark
    cmp rax, r15           ; Compare tuple pointer to heap top
    short jae L39          ; Jump and copy if above
    cmp rax, rdi           ; Compare tuple pointer to high water
    short jae L38          ; Jump and overwrite if above high water

    ; The tuple is not in the safe part of the heap.
    ; Fall through to the copy code.

L39:                       ; Copy the current record
    vmovups ymm0, [rax-2]
    vmovups [r15], ymm0
    lea rax, qword ptr [r15+2] ; Set up tagged pointer to copy
    add r15, 32            ; Advance heap top past the copy

L38:
    mov rdi, rcx           ; Get new value for atoms field
    mov qword ptr [rax+22], rdi
    mov qword ptr [rbx+8], rax
```

(Lines starting with `#` are comments emitted by the JIT, while the text
that follows `;` is a comment added by me for clarification.)

The BEAM loader renames an `update_record` instruction with an `inplace` hint
to `update_record_in_place`.

The first two instructions load the tuple to be update into CPU register `rax` and
the new counter value (`C + 1`) into `rcx`.

```
    mov rax, qword ptr [rbx+8]
    mov rcx, qword ptr [rbx+16]
```

The next two instructions test whether the new counter value is a
small integer that fits into a word. The test has been simplified to a
more efficient test that is only safe when the value is known to be an
integer. If it is a small integer, it is always safe to jump to the code
that updates the existing tuple:

```
    test cl, 1
    short je L38           ; Update directly if small integer.
```

If it is not a small integer, it must be a **bignum**, that is a
signed integer that does not fit in 60 bits and therefore have to be
stored on the heap with `rcx` containing a tagged pointer to the bignum on
the heap.

If `rcx` is a pointer to a term on the heap, it is not always safe to
directly updating the existing tuple. That is because of the way the
Erlang [generational garbage
collector](https://www.erlang.org/doc/apps/erts/garbagecollection#generational-garbage-collection)
works. Each Erlang process has two heaps for keeping Erlang terms:
the young heap and the old heap. Terms on the young heap are allowed to
reference terms on the old heap, but not vice versa. That means that
if the tuple to be updated resides on the old heap, it is not safe to
update one of its elements so that it will reference a term on the young
heap.

Therefore, the JIT needs to emit code to ensure that the pointer to
the tuple resides in the "safe part" of the young heap:

```
    mov rdi, [r13+480]     ; Get the high water mark
    cmp rax, r15           ; Compare tuple pointer to heap top
    short jae L39          ; Jump and copy if above
    cmp rax, rdi           ; Compare tuple pointer to high water
    short jae L38          ; Jump and overwrite if above high water
```

The safe part of the heap is between the high water mark and the heap
top.  If the tuple is below the high water mark, if it is still alive,
it will be copied to the old heap in the next garbage collection.

If the tuple is in the safe part, the copy code is skipped by jumping
to the code that stores the new value into the existing tuple.

If not, the next part will copy the existing record to the heap.

```
L39:                       ; Copy the current record
    vmovups ymm0, [rax-2]
    vmovups [r15], ymm0
    lea rax, qword ptr [r15+2] ; Set up tagged pointer to copy
    add r15, 32            ; Advance heap top past the copy
```

The copying is done using [AVX instructions][avx].

Next follows the code that writes the new value into the tuple:

```
L38:
    mov rdi, rcx           ; Get new value for atoms field
    mov qword ptr [rax+22], rdi
    mov qword ptr [rbx+8], rax
```

[avx]: https://en.wikipedia.org/wiki/Advanced_Vector_Extensions

If all the new values being written into the existing record are known
never to be tagged pointers, the native instructions can be
simplified. Consider this module:

```erlang
-module(whatever).
-export([main/1]).

-record(bar, {bool,pid}).

main(Bool) when is_boolean(Bool) ->
    flip_state(#bar{bool=Bool,pid=self()}).

flip_state(R) ->
    R#bar{bool=not R#bar.bool}.
```

The `update_record` instruction looks like this:

```
    {update_record,{atom,inplace},
                   3,
                   {tr,{x,0},
                       {t_tuple,3,true,
                                #{1 => {t_atom,[bar]},
                                  2 => {t_atom,[false,true]},
                                  3 => pid}}},
                   {x,0},
                   {list,[2,{tr,{x,1},{t_atom,[false,true]}}]}}.
```

Based on the type for the new value, `{t_atom,[false,true]}`, the
JIT is able to generate much shorter code than for the previous example:

```
# update_record_in_place_IsdI
    mov rax, qword ptr [rbx]
# skipped copy fallback because all new values are safe
    mov rdi, qword ptr [rbx+8]
    mov qword ptr [rax+14], rdi
    mov qword ptr [rbx], rax
```

References to literals (such as `[1,2,3]`) are also safe, because literals
are stored in a special literal area, and the garbage collector
handles them specially. Consider this code:

```erlang
-record(state, {op, data}).

update_state(R0, Op0, Data) ->
    R = R0#state{data=Data},
    case Op0 of
        add -> R#state{op=fun erlang:'+'/2};
        sub -> R#state{op=fun erlang:'-'/2}
    end.
```

Both of the record updates in the `case` can be done in place. Here
is the BEAM code for the record update in the first clause:

```
    {update_record,{atom,inplace},
                   3,
                   {tr,{x,0},{t_tuple,3,true,#{1 => {t_atom,[state]}}}},
                   {x,0},
                   {list,[2,{literal,fun erlang:'+'/2}]}}.
```

Since the value to be written is a literal, the JIT emits simpler
code without the copy fallback:

```
# update_record_in_place_IsdI
    mov rax, qword ptr [rbx]
# skipped copy fallback because all new values are safe
    long mov rdi, 9223372036854775807  ; Placeholder for address to fun
    mov qword ptr [rax+14], rdi
    mov qword ptr [rbx], rax
```
The large integer `9223372036854775807` is a placeholder that will be
patched later when the address of the literal fun will be known.

Here is the pull request for updating tuples in place:

* [#8090: Destructive tuple update](https://github.com/erlang/otp/pull/8090)


### Optimizing by generating less garbage

When updating a record in place, omitting the copying of the existing
record should be a clear win, except perhaps for very small records.

What is less clear is the effect on garbage collection. Updating a
tuple in place is an example of optimizing by generating less
garbage. By creating less garbage, the expectation is that garbage
collections should occur less often, which should improve the
performance of the program.

Because of the highly variable execution time for doing a garbage
collection, it is notoriously difficult to benchmark optimizations
that reduce the amount of garbage created. Often the outcomes of
benchmarks do not apply to performing the same tasks in a real
application.

My own [anecdotal evidence](https://en.wikipedia.org/wiki/Anecdotal_evidence)
suggests that in most cases there are no measurable performance wins by
producing less garbage.

I also remember when an optimization that reduced the size of an
Erlang term resulted in a benchmark being consistently slower. It took
the author of that optimization several days of investigation to confirm
that the slowdown in the benchmark was not the fault of his optimization,
but by creating less garbage, garbage collection happened at a later time
when it happened to be much more expensive.

On average we expect that this optimization should improve performance,
especially for large records.

### Optimization of funs

The internal representation of funs in the runtime system has
changed in Erlang/OTP 27, making possible several new optimizations.

As an example, consider this function:


```erlang
madd(A, C) ->
    fun(B) -> A * B + C end.
```

In Erlang/OTP 26, the native code for creating the fun looks like so:

```
# i_make_fun3_FStt
L38:
    long mov rsi, 9223372036854775807 ; Placeholder for dispatch table
    mov edx, 1
    mov ecx, 2
    mov qword ptr [r13+80], r15
    mov rbp, rsp
    lea rsp, qword ptr [rbx-128]
    vzeroupper
    mov rdi, r13
    call 4337160320       ; Call helper function in runtime system
    mov rsp, rbp
    mov r15, qword ptr [r13+80]
# Move fun environment
    mov rdi, qword ptr [rbx]
    mov qword ptr [rax+40], rdi
    mov rdi, qword ptr [rbx+8]
    mov qword ptr [rax+48], rdi
# Create boxed ptr
    or al, 2
    mov qword ptr [rbx], rax
```
The large integer `9223372036854775807` is a placeholder
for a value that will be filled in later.

Most of the work of actually creating the fun object is done by
calling a helper function (the `call 4337160320` instruction) in the
runtime system.

In Erlang/OTP 27, the part of fun that resides on the heap of the
calling process has been simplified so that it is now smaller than in
Erlang/OTP 26, and most importantly does not contain anything that is
too tricky to initialize in inline code.

The code for creating the fun is not only shorter, but it also doesn't
need to call any function in the runtime system:

```
# i_make_fun3_FStt
L38:
    long mov rax, 9223372036854775807 ; Placeholder for dispatch table
# Create fun thing
    mov qword ptr [r15], 196884
    mov qword ptr [r15+8], rax
# Move fun environment
# (moving two items)
    vmovups xmm0, xmmword ptr [rbx]
    vmovups xmmword ptr [r15+16], xmm0
L39:
    long mov rdi, 9223372036854775807 ; Placeholder for fun reference
    mov qword ptr [r15+32], rdi
# Create boxed ptr
    lea rax, qword ptr [r15+2]
    add r15, 40
    mov qword ptr [rbx], rax
```

The difference from Erlang/OTP 26 is that the parts of the fun that is only
needed when loading and unloading code are no longer stored on the heap.
Instead those parts are stored in the literal pool area belonging to the loaded
code for the module, and are shared by all instances of the same fun.

The part of the fun that resides on the process heap is two words smaller
compared to Erlang/OTP 26.

The creation of the fun environment has also been optimized. In Erlang/OTP 26,
four instructions were needed:

```
# Move fun environment
    mov rdi, qword ptr [rbx]
    mov qword ptr [rax+40], rdi
    mov rdi, qword ptr [rbx+8]
    mov qword ptr [rax+48], rdi
```

In Erlang/OTP 27, using [AVX instructions][avx] both variables (`A` and `C`)
can be moved using only two instructions:

```
# Move fun environment
# (moving two items)
    vmovups xmm0, xmmword ptr [rbx]
    vmovups xmmword ptr [r15+16], xmm0
```

Another optimization made possible by the changed fun representation
is testing for a fun having a specific arity (the number of expected
arguments when calling it). For example:


```erlang
ensure_fun_0(F) when is_function(F, 0) -> ok.
```

Here is the native code emitted by the JIT in Erlang/OTP 26:

```
# is_function2_fss
    mov rdi, qword ptr [rbx]   ; Fetch `F` from {x,0}.

    rex test dil, 1            ; Test whether the term is a tagged pointer...
    short jne label_3          ; ... otherwise fail.

    mov eax, dword ptr [rdi-2] ; Pick up the header word.
    cmp eax, 212               ; Test whether it is a fun...
    short jne label_3          ; ... otherwise fail.

    cmp byte ptr [rdi+22], 0   ; Test whether the arity is 0...
    short jne label_3          ; ... otherwise fail.
```

In Erlang/OTP 27, the arity for the fun (the number of expected arguments) is
stored in the header word of the fun term, which means that the test
for a fun can be combined with the test for its arity:

```
# is_function2_fss
    mov rdi, qword ptr [rbx]   ; Fetch `F` from {x,0}.

    rex test dil, 1            ; Test whether the term is a tagged pointer...
    short jne label_3          ; ... otherwise fail.

    cmp word ptr [rdi-2], 20   ; Test whether this is a fun with arity 0...
    short jne label_3          ; ... otherwise fail.
```

All external funs are now literals stored outside all process heaps. As an
example, consider the following functions:

```erlang
my_fun() ->
    fun ?MODULE:some_function/0.

mfa(M, F, A) ->
    fun M:F/A.
```

In Erlang/OTP 26, the external fun returned by `my_fun/0` would not occupy
any room on the heap of the calling process, while the dynamic external fun
returned by `mfa/3` would need 5 words on the heap of the calling process.

In Erlang/OTP 27, neither of the funs will require any room on the heap of
the calling process.

Those optimizations were implemented in the following pull requests:

* [#7948: Optimize reference counting of local funs](https://github.com/erlang/otp/pull/7948)

* [#7314: Shrink and optimize funs (again)](https://github.com/erlang/otp/pull/7314)

* [#7894: Share external funs globally](https://github.com/erlang/otp/pull/7894)

* [x86_64: Optimize creation of fun
  environment](https://github.com/erlang/otp/pull/7713/commits/ae127203ac2423d057e1ef151d4ca8b114740b84)
  (part of [#7713](https://github.com/erlang/otp/pull/7713))

### Integer arithmetic improvements

In the end of June last year, we released the [OTP 26.0.2 patch](https://www.erlang.org/patches/otp-26.0.2)
for Erlang/OTP 26 that made
[`binary_to_integer/1`](https://www.erlang.org/doc/man/erlang#binary_to_integer-1) faster.

To find out how much faster, run this benchmark:

```erlang
bench() ->
    Size = 1_262_000,
    String = binary:copy(<<"9">>, Size),
    {Time, _Val} = timer:tc(erlang, binary_to_integer, [String]),
    io:format("Size: ~p, seconds: ~p\n", [Size, Time / 1_000_000]).
```

It measures the time to convert a binary holding 1,262,000 digits to an integer.

Running an unpatched Erlang/OTP 26 on my Intel-based iMac from 2017,
the benchmark finishes in about 10 seconds.

The same benchmark run using Erlang/OTP 26.0.2 finishes in about 0.4 seconds.

The speed-up was achieved by three separate optimizations:

* `binary_to_integer/1` was implemented as a BIF in C using a naive
  algorithm that didn't scale well.  It was replaced with a
  [divide-and-conquer
  algorithm](https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm)
  implemented in Erlang. (Implementing the new algorithm as a BIF wasn't
  faster than the Erlang version.)

* The runtime system's function for doing multiplication of large
  integers was modified to use the [Karatsuba
  algorithm](https://en.wikipedia.org/wiki/Karatsuba_algorithm), which
  is a divide-and-conquer multiplication algorithm invented in the 1960s.

* Some of the low-level helper functions for arithmetic with large
  integers (bignums) were modified to take advantage of a 128-bit integer data
  type on 64-bit CPUs when supported by the C compiler.

Those improvements were implemented in the following pull request:

* [#7426: Optimize binary_to_integer/1 and friends](https://github.com/erlang/otp/pull/7426)

In Erlang/OTP 27, some additional improvement of integer arithemetic
were implemented.  That reduced the execution time for the
`binary_to_integer/1` benchmark to about 0.3 seconds.

Those improvements are found in the following pull request:

* [#7553: Optimize integer arithmetic](https://github.com/erlang/otp/pull/7553)

Those arithmetic enchancements improve the running times for the
[pidigits
benchmark](https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/pidigits-erlang-2.html):

| -------- || -------- |
| Version  || Seconds  |
| -------- || -------- |
| 26.0     || `7.635`  |
| 26.2.1   || `2.959`  |
| 27.0     || `2.782`  |
| -------- || -------- |

(Run on my M1 MacBook Pro.)

### Numerous miscellaneous enhancements

Many enhancements have been made to the code generation for many
instructions, as well as a few to the Erlang compiler.  Here follows a
single example to show one of the improvements to the `=:=` operator:

```erlang
ensure_empty_map(Map) when Map =:= #{} ->
    ok.
```

Here is the BEAM code for the `=:=` operator as used in this example:

```
    {test,is_eq_exact,{f,1},[{x,0},{literal,#{}}]}.
```

Here is the native code for Erlang/OTP 26:

```
# is_eq_exact_fss
L45:
    long mov rsi, 9223372036854775807
    mov rdi, qword ptr [rbx]
    cmp rdi, rsi
    short je L44                  ; Succeeded if the same term.

    rex test dil, 1
    short jne label_1             ; Fail quickly if not a tagged pointer.

    ; Call the general runtime function for comparing two terms.
    mov rbp, rsp
    lea rsp, qword ptr [rbx-128]
    vzeroupper
    call 4549723200
    mov rsp, rbp

    test eax, eax
    short je label_1               ; Fail if unequal.
L44:
```

The code begins with a few tests to quickly succeed or fail, but in
practice those are unlikely to trigger for this example, which means
that the call to the general routine in the runtime system for
comparing two terms will almost always be called.

In Erlang/OTP 27, the JIT emits special code for testing whether a
term is an empty map:

```
# is_eq_exact_fss
# optimized equality test with empty map
    mov rdi, qword ptr [rbx]
    rex test dil, 1
    short jne label_1              ; Fail if not a tagged pointer.

    cmp dword ptr [rdi-2], 300
    short jne label_1              ; Fail if not a map.

    cmp dword ptr [rdi+6], 0
    short jne label_1              ; Fail if size is not zero.
```

Here follows the main pull requests for miscellaneous enhancements in Erlang/OTP 27:

* [#7563: Enhance type analysis](https://github.com/erlang/otp/pull/7563)

* [#7956: Do some minor enhancements of the code generation in the JIT](https://github.com/erlang/otp/pull/7956)

* [#7713: Improve code generation for the JIT](https://github.com/erlang/otp/pull/7713)

* [#8040: Improve caching of BEAM registers](https://github.com/erlang/otp/pull/8040)
