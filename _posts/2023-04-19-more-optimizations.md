---
layout: post
title: More Optimizations in the Compiler and JIT
tags: BEAM JIT
author: Björn Gustavsson
---

This post explores the enhanced type-based optimizations
and the other performance improvements in Erlang/OTP 26.

### What to expect of the JIT in OTP 26

In OTP 25, the compiler was updated to embed type information in
the BEAM file and the JIT was extended to emit better code based
on that type information. Those improvements were described in
the blog post [Type-Based Optimizations in the JIT][blog2022].

As mentioned in that blog post, there were limitations in both the
compiler and the JIT that prevented many optimizations. In OTP 26, the
compiler will produce better type information and the JIT will take
better advantage of the improved type information, typically resulting
in fewer redundant type tests and smaller native code size.

A new BEAM instruction introduced in OTP 26 makes record updates
faster by a small but measurable amount.

The most noticable performance improvements in OTP 26 are probably for
matching and construction of binaries using the bit syntax. Those
improvements, combined with changes to the `base64` module itself,
makes encoding to Base64 about 4 times as fast and decoding from
Base64 more than 3 times as fast.

[blog2022]: https://www.erlang.org/blog/type-based-optimizations-in-the-jit/


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

### Quick overview of type-based optimizations in OTP 25

Let's quickly summarize the type-based optimizations in OTP 25. For more
details, see the [aformentioned blog post][blog2022].

First consider an addition of two values with nothing known about
their types:

```erlang
add1(X, Y) ->
    X + Y.
```

The [BEAM code][beam_code] looks like this:

```
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.
```

Without any information about the operands, the JIT must emit code
that can handle all possible types for the operands. For the x86_64
architecture, 14 native instructions are needed.

If the type of the operands are known to be integers sufficiently
small making overflow impossible, the JIT needs to emit only 5 native
instructions for the addition.

Here is an example where the types and ranges of the operands for the
`+` operator are known:

```erlang
add5(X, Y) when X =:= X band 16#3FF,
                Y =:= Y band 16#3FF ->
    X + Y.
```

The BEAM code for this function is as follows:

```
    {gc_bif,'band',{f,24},2,[{x,0},{integer,1023}],{x,2}}.
    {test,is_eq_exact,
          {f,24},
          [{tr,{x,0},{t_integer,any}},{tr,{x,2},{t_integer,{0,1023}}}]}.
    {gc_bif,'band',{f,24},2,[{x,1},{integer,1023}],{x,2}}.
    {test,is_eq_exact,
          {f,24},
          [{tr,{x,1},{t_integer,any}},{tr,{x,2},{t_integer,{0,1023}}}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,1023}}},{tr,{x,1},{t_integer,{0,1023}}}],
            {x,0}}.
    return.
```

The register operands (`{x,0}` and `{x,1}`) have now been annotated with
type information:

```erlang
{tr,Register,Type}
```

That is, each register operand is a three-tuple with `tr` as the first
element. `tr` stands for **typed register**. The second element is the
BEAM register (`{x,0}` or `{x,1}` in this case), and the third element
is the type of the register in the compiler's internal type
representation. `{t_integer,{0,1023}}` means that the value is an
integer in the inclusive range 0 through 1023.

With that type information, the JIT emits the following native code
for the `+` operator:

```nasm
# i_plus_ssjd
# add without overflow check
    mov rax, qword ptr [rbx]
    mov rsi, qword ptr [rbx+8]
    and rax, -16               ; Zero the tag bits
    add rax, rsi
    mov qword ptr [rbx], rax
```

(Lines starting with `#` are comments emitted by the JIT, while the
text that follows `;` is a comment added by me for clarification.)

The reduction in code size from 14 instructions down to 5 is nice, but
having to express the range check in that convoluted way using `band`
can hardly be called nice nor natural.

If we try to express the range checks in a more natural way:

```erlang
add4(X, Y) when is_integer(X), 0 =< X, X < 16#400,
                is_integer(Y), 0 =< Y, Y < 16#400 ->
    X + Y.
```

the compiler in OTP 25 will no longer be able to figure out the
ranges for the operands. Here is the BEAM code:

```
    {test,is_integer,{f,22},[{x,0}]}.
    {test,is_ge,{f,22},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
    {test,is_lt,{f,22},[{tr,{x,0},{t_integer,any}},{integer,1024}]}.
    {test,is_integer,{f,22},[{x,1}]}.
    {test,is_ge,{f,22},[{tr,{x,1},{t_integer,any}},{integer,0}]}.
    {test,is_lt,{f,22},[{tr,{x,1},{t_integer,any}},{integer,1024}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,any}},{tr,{x,1},{t_integer,any}}],
            {x,0}}.
    return.
```

Because of that severe limitation in the compiler's value range
analysis, I wrote:

> We aim to improve the type analysis and optimizations in OTP 26 and
generate better code for this example.

### The enhanced type-based optimizations in OTP 26

Compiling the same example with OTP 26, the result is:


```
    {test,is_integer,{f,19},[{x,0}]}.
    {test,is_ge,{f,19},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
    {test,is_ge,{f,19},[{integer,1023},{tr,{x,0},{t_integer,{0,'+inf'}}}]}.
    {test,is_integer,{f,19},[{x,1}]}.
    {test,is_ge,{f,19},[{tr,{x,1},{t_integer,any}},{integer,0}]}.
    {test,is_ge,{f,19},[{integer,1023},{tr,{x,1},{t_integer,{0,'+inf'}}}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,1023}}},{tr,{x,1},{t_integer,{0,1023}}}],
            {x,0}}.
```

The BEAM instruction for the `+` operator now have ranges for its operands.

Let's look at little bit closer at the first three instructions, which
corresponds to the guard test `is_integer(X), 0 =< X, X < 16#400`.

First is the guard check for an integer:

```
    {test,is_integer,{f,19},[{x,0}]}.
```

It is followed by the guard test `0 =< X` (rewritten to `X >= 0` by the compiler):

```
    {test,is_ge,{f,19},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
```

As a result of the `is_integer/1` test it is known that `{x,0}`
is an integer.

The third instruction corresponds to `X < 16#400`, which the compiler
has rewritten to `16#3FF >= X` (`1023 >= X`):

```
    {test,is_ge,{f,19},[{integer,1023},{tr,{x,0},{t_integer,{0,'+inf'}}}]}.
```

In the type for the `{x,0}` register there is something new for
OTP 26. It says that the range is 0 through `'+inf'`, that is, from 0 up
to positive infinity. Combining that range with the range from this
instruction, the Erlang compiler can infer that if this instruction
succeeds, the type for `{x,0}` is `t_integer,{0,1023}}`.


### Combining guard tests

In OTP 25, the JIT would emit native code for each BEAM instruction
in the guard individually. When translated individually, the three guards
tests for one of the variables each require 11 native instructions, or 33
instructions for all three.

By having the BEAM loader combine the three guard tests into a
single `is_int_range` instruction, the JIT is capable of doing a much
better job and emit a mere 6 native instructions.

How is that possible?

As individual BEAM instructions, each guard test needs 5 instructions
to fetch the value from `{x,0}` and test that the value is a small
integer. As a combined instruction, that only needs to be done once.
Other parts of the guard tests also become redundant in the combined
instruction and can be omitted. For example, the `is_integer/1` type
test will also succeed if its argument is a **bignum** (an integer
that does not fit in a machine word). Clearly, a bignum will fall well
outside the range 0 through 1023, so if the argument is not a small
integer, the combined guard test will fail immediately.

With those and some other simplifications, we end up with the following
native instructions:

```nasm
# is_int_in_range_fScc
    mov rax, qword ptr [rbx]
    sub rax, 15
    test al, 15
    short jne label_19
    cmp rax, 16368
    short ja label_19
```

The first instruction fetches the value of `{x,0}` to the CPU
register `rax`:

```nasm
    mov rax, qword ptr [rbx]
```

The next instruction subtracts the [tagged value][tags] for the lower
bound of the range. Since the lower bound of the range is 0 and the
tag for small integers is 15, the value that is subtracted
is `16 * 0 + 15` or simply 15. (For small integers, the runtime system
uses the 4 least significant bits of the word as tag bits.)
If the lower bound would have been 1, the value to be subtracted would
have been `16 * 1 + 15` or 31:

```nasm
    sub rax, 15
```

The subtraction achieves two aims at once. Firstly, it simplifies the
tag test in the next two instructions because if the value of of
`{x,0}` is a small integer, the 4 least significants bits will now be
zero:

```nasm
    test al, 15
    short jne label_19
```

The `test al, 15` instruction does a bitwise AND operation of the
lower byte of the CPU register `rax`, discarding the result but
setting CPU flags depending on the value. The next instruction tests
whether the result was nonzero (the tag was not the tag for a small
integer), in which case the test fails and a jump to the failure
label is made.

The second aim for the subtraction is to simplify the range check.
If the value being tested was below the lower bound, the value
of `rax` will be negative after the subtraction.

Since integers are represented in [two's complement notation][two], a
signed negative integer interpreted as an unsigned integer will be a
very large integer. Therefore, both bounds can be checked at once
using the old trick of treating the value in `rax` as unsigned:

```nasm
    cmp rax, 16368
    short ja label_19
```

The `cmp rax, 16368` instruction compares the value in `rax` with the
difference of the tagged upper bound and the tagged lower bound, that
is:

```
(16 * 1023 + 15) - (16 * 0 + 15)
```

`ja` stands for "Jump (if) Above", that is, jump if the CPU flags
indicates that in previous comparison of unsigned integers the first
integer was greater than the second. Since a negative number
represented in two's complement notation looks like a huge integer
when interpreted as an unsigned integer, `short ja label_19` will
transfer control to the failure label for values both below the lower
bound and above the upper bound.

[two]: https://en.wikipedia.org/wiki/Two%27s_complement

### More code generation improvements

The JIT in OTP 26 generates better code for common combinations of
relational operators. In order to reduce the number of combinations
that the JIT will need to handle, the compiler rewrites the `<`
operator to `>=` if possible. In the previous example, it was shown
that the compiler rewrote `X < 1024` to `1023 >= X`.

Let's look at a contrived example to show (off) a few more
improvements in the code generation:

```erlang
add6(M) when is_map(M) ->
    A = map_size(M),
    if
        9 < A, A < 100 ->
            A + 6
    end.
```

The main part of the BEAM code looks like this:

```
    {test,is_map,{f,41},[{x,0}]}.
    {gc_bif,map_size,{f,0},1,[{tr,{x,0},{t_map,any,any}}],{x,0}}.
    {test,is_ge,
          {f,43},
          [{tr,{x,0},{t_integer,{0,288230376151711743}}},{integer,10}]}.
    {test,is_ge,
          {f,43},
          [{integer,99},{tr,{x,0},{t_integer,{10,288230376151711743}}}]}.
    {gc_bif,'+',{f,0},1,[{tr,{x,0},{t_integer,{10,99}}},{integer,6}],{x,0}}.
    return.
```

In OTP 26, the JIT will inline the code for many of the most
frequently used guard BIFs. Here is the native code for the
`map_size/1` call:

```nasm
# bif_map_size_jsd
    mov rax, qword ptr [rbx]      ; Fetch map from {x,0}
# skipped type check because the argument is always a map
    mov rax, qword ptr [rax+6]    ; Fetch size of map
    shl rax, 4
    or al, 15                     ; Tag as small integer
    mov qword ptr [rbx], rax      ; Store size in {x,0}
```

The two `is_ge` instructions are combined by the BEAM loader into
an `is_in_range` instruction:

```nasm
# is_in_range_ffScc
# simplified fetching of BEAM register
    mov rdi, rax
# skipped test for small operand since it always small
    sub rdi, 175
    cmp rdi, 1424
    ja label_43
```

The first instruction is a new optimization in OTP 26. Normally `{x,0}` is
fetched using the instruction `mov rax, qword ptr [rbx]`. However, in this
case, the last instruction in the previous BEAM instruction is the instruction
`mov qword ptr [rbx], rax`. Therefore, since it is known that the contents of
`{x,0}` is already in CPU register `rax`, the instruction can be simplified
to:

```nasm
# simplified fetching of BEAM register
    mov rdi, rax
```

The size of a map that will fit in memory on a 64-bit computer is always
a small integer, so the test for a small integer is skipped:

```nasm
# skipped test for small operand since it always small
    sub rdi, 175     ; Subtract 16 * 10 + 15
    cmp rdi, 1424    ; Compare with (16*99+15)-(16*10+15)
    ja label_43
```

The native code for the `+` operator looks like this:

```nasm
# i_plus_ssjd
# add without overflow check
    mov rax, qword ptr [rbx]
    add rax, 96      ; 16 * 6 + 0
    mov qword ptr [rbx], rax
```


### New BEAM instructions in OTP 26

The previous example of combining guard tests showed that the JIT can
often generate better code if multiple BEAM instructions are combined
into one. While the [BEAM loader][beam_loader] is capable of combining
instructions it is often more practical to let the Erlang compiler
emit combined instructions.

[beam_loader]: https://www.erlang.org/blog/beam-compiler-history/#the-ever-changing-beam-instructions
OTP 26 introduces two new instructions, each of which replaces a sequence of
any number of simpler instructions:

* `update_record` for updating any number of fields in a record.

* `bs_match` for matching multiple segments of fixed size.

In OTP 25, the `bs_create_bin` instruction for constructing a binary
with any number of segments was introduced, but its full potential for
generating efficient code was not leveraged in OTP 25.


### Updating records in OTP 25

Consider the following example of a record definition and three functions
that update the record:

```erlang
-record(r, {a,b,c,d,e}).

update_a(R) ->
    R#r{a=42}.

update_ce(R) ->
    R#r{c=99,e=777}.

update_bcde(R) ->
    R#r{b=2,c=3,d=4,e=5}.
```

In OTP 25 and earlier, the way in which a record is updated depends on
both the number of fields being updated and the size of the record.

When a single field in a record is updated, as in `update_a/1`, the
[setelement/3](https://www.erlang.org/doc/man/erlang.html#setelement-3)
BIF is called:

```
    {test,is_tagged_tuple,{f,34},[{x,0},6,{atom,r}]}.
    {move,{x,0},{x,1}}.
    {move,{integer,42},{x,2}}.
    {move,{integer,2},{x,0}}.
    {call_ext_only,3,{extfunc,erlang,setelement,3}}.
```

When updating more than one field but fewer than approximately half of
the fields, as in `update_ce/1`, code similar to the following is
emitted:

```
    {test,is_tagged_tuple,{f,37},[{x,0},6,{atom,r}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,777},{x,2}}.
    {move,{integer,6},{x,0}}.
    {call_ext,3,{extfunc,erlang,setelement,3}}.
    {set_tuple_element,{integer,99},{x,0},3}.
    {deallocate,0}.
    return.
```

Here the `e` field is updated using `setelement/3`, followed by
`set_tuple_element` to update the `c` field destructively. Erlang does
not allow mutation of terms, but here it is done "under the hood" in a
safe way.

When a majority of the fields are updated, as in `update_bcde/1`, a
new tuple is built:

```
    {test,is_tagged_tuple,{f,40},[{x,0},6,{atom,r}]}.
    {test_heap,7,1}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {put_tuple2,{x,0},
                {list,[{atom,r},
                       {x,0},
                       {integer,2},
                       {integer,3},
                       {integer,4},
                       {integer,5}]}}.
    return.
```

### Updating records in OTP 26

In OTP 26, all records are updated using the new BEAM instruction
`update_record`.  For example, here is the main part of the BEAM code
for `update_1`:

```
    {test,is_tagged_tuple,{f,34},[{x,0},6,{atom,r}]}.
    {test_heap,7,1}.
    {update_record,{atom,reuse},6,{x,0},{x,0},{list,[2,{integer,42}]}}.
    return.
```

The last operand is a list of positions in the tuple and their corresponding
new values.

The first operand, `{atom,reuse}`, is a hint to the JIT that it is possible
that the source tuple is already up to date and does not need to be updated.
Another possible value for the hint operand is `{atom,copy}`, meaning that
the source tuple is definitely not up to date.

The JIT emits the following native code for the `update_record` instruction:

```nasm
# update_record_aIsdI
    mov rax, qword ptr [rbx]
    mov rdi, rax
    cmp qword ptr [rdi+14], 687
    je L130
    vmovups xmm0, [rax-2]
    vmovups [r15], xmm0
    mov qword ptr [r15+16], 687
    vmovups ymm0, [rax+22]
    vmovups [r15+24], ymm0
    lea rax, qword ptr [r15+2]
    add r15, 56
L130:
    mov qword ptr [rbx], rax
```

Let's walk through those instructions. First the value of `{x,0}` is fetched:

```nasm
    mov rax, qword ptr [rbx]
```

Since the hint operand is the atom `reuse`, is is possible that it is
unnecessary to copy the tuple. Therefore, the JIT emits an instruction
sequence to test whether the `a` field (position 2 in the tuple)
already contains the value 42. If so, the source tuple can be reused:

```nasm
    mov rdi, rax
    cmp qword ptr [rdi+14], 687   ; 42
    je L130                       ; Reuse source tuple
```

Next follows the copy and update sequence. First the header word for
the tuple and its first element (the `r` atom) are copied using
[AVX instructions][avx]:

```nasm
    vmovups xmm0, [rax-2]
    vmovups [r15], xmm0
```

Next the value 42 is stored into position 2 of the copy of the tuple:

```nasm
    mov qword ptr [r15+16], 687   ; 42
```

Finally the remaining four elements of the tuple are copied:

```nasm
    vmovups ymm0, [rax+22]
    vmovups [r15+24], ymm0
```

All that remains is to create a tagged pointer to the newly created
tuple and increment the heap pointer:

```nasm
    lea rax, qword ptr [r15+2]
    add r15, 56
```

The last instruction stores the tagged pointer to either the original
or the updated tuple to `{x,0}`:

```nasm
L130:
    mov qword ptr [rbx], rax
```

[avx]: https://en.wikipedia.org/wiki/Advanced_Vector_Extensions

The BEAM code for `update_ce/1` is very similar to the code for `update_a/1`:

```
    {test,is_tagged_tuple,{f,37},[{x,0},6,{atom,r}]}.
    {test_heap,7,1}.
    {update_record,{atom,reuse},
                   6,
                   {x,0},
                   {x,0},
                   {list,[4,{integer,99},6,{integer,777}]}}.
    return.
```

The native code looks like this:


```nasm
# update_record_aIsdI
    mov rax, qword ptr [rbx]
    vmovups ymm0, [rax-2]
    vmovups [r15], ymm0
    mov qword ptr [r15+32], 1599   ; 99
    mov rdi, [rax+38]
    mov [r15+40], rdi
    mov qword ptr [r15+48], 12447  ; 777
    lea rax, qword ptr [r15+2]
    add r15, 56
    mov qword ptr [rbx], rax
```

Note that the copying and updating is done unconditionally, despite
the `reuse` hint. The JIT is free to ignore the hints. When multiple
fields are being updated, the test for whether the update is
unnecessary would be more expensive and it is also much less likely
that all of the fields would turn out to be unchanged. Therefore,
trying to reuse the original tuple is more likely to be a
[pessimization](https://en.wiktionary.org/wiki/pessimization) rather
than an optimization.


### Matching and constructing binaries in OTP 25

To explore the optimizations of binaries, the following example will
be used:

```erlang
bin_swap(<<A:8,B:24>>) ->
    <<B:24,A:8>>.
```

Somewhat simplified, the main part of the BEAM code as emitted by
the compiler in OTP 25 looks like this:

```
    {test,bs_start_match3,{f,1},1,[{x,0}],{x,1}}.
    {bs_get_position,{x,1},{x,0},2}.
    {test,bs_get_integer2,
          {f,2},
          2,
          [{x,1},
           {integer,8},
           1,
           {field_flags,[unsigned,big]}],
          {x,2}}.
    {test,bs_get_integer2,
          {f,2},
          3,
          [{x,1},
           {integer,24},
           1,
           {field_flags,[unsigned,big]}],
          {x,3}}.
    {test,bs_test_tail2,{f,2},[{x,1},0]}.
    {bs_create_bin,{f,0},
                   0,4,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,nil,
                          {tr,{x,3},{t_integer,{0,16777215}}},
                          {integer,24},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{0,255}}},
                          {integer,8}]}}.
    return.
```

Let's walk through the code. The first instruction sets up a [match
context](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html#how-binaries-are-implemented):

```
    {test,bs_start_match3,{f,1},1,[{x,0}],{x,1}}.
```

A match context holds several pieces of information needed for
matching a binary.

The next instruction saves information that will be needed if matching
of the binary fails for some reason:

```
    {bs_get_position,{x,1},{x,0},2}.
```

The next two instructions match out two segments as integers (comments added by me):

```
    {test,bs_get_integer2,
          {f,2},          % Failure label
          2,              % Number of live X registers (needed for GC)
          [{x,1},         % Match context register
           {integer,8},   % Size of segment in units
           1,             % Unit value
           {field_flags,[unsigned,big]}],
          {x,2}}.         % Destination register
    {test,bs_get_integer2,
          {f,2},
          3,
          [{x,1},
           {integer,24},
           1,
           {field_flags,unsigned,big]}],
          {x,3}}.
```

The next instruction makes sure that the end of the binary has now been
reached:

```
    {test,bs_test_tail2,{f,2},[{x,1},0]}.
```

The next instruction creates the binary with the segments swapped:

```
    {bs_create_bin,{f,0},
                   0,4,1,
                   {x,0},
                   {list,[{atom,integer},
                          1,1,nil,
                          {tr,{x,3},{t_integer,{0,16777215}}},
                          {integer,24},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{0,255}}},
                          {integer,8}]}}.
```

Before OTP 25, creation of binaries was done using multiple
instructions, similar to how binary matching is still done in
OTP 25. The reason for creating the `bs_create_bin` instruction in OTP 25
was to be able to provide improved error information when construction
of a binary fails, similar to the [improved BIF error
information](https://www.erlang.org/blog/my-otp-24-highlights/#eep-54-improved-bif-error-information).

When the size of a segment of size 8, 16, 32, or 64 is matched,
specialized instructions are used for x86_64. The specialized
instructions do everything inline provided that the segment is
byte-aligned. (The JIT in OTP 25 for AArch64/ARM64 does not have these
specialized instructions.) Here is the instruction for matching a
segment of size 8:

```nasm
# i_bs_get_integer_8_Stfd
    mov rcx, qword ptr [rbx+8]
    mov rsi, qword ptr [rcx+22]
    lea rdx, qword ptr [rsi+8]
    cmp rdx, qword ptr [rcx+30]
    ja label_25
    rex test sil, 7
    short je L91
    mov edx, 64
    call L92
    short jmp L90
L91:
    mov rdi, qword ptr [rcx+14]
    shr rsi, 3
    mov qword ptr [rcx+22], rdx
    movzx rax, byte ptr [rdi+rsi]
    shl rax, 4
    or rax, 15
L90:
    mov qword ptr [rbx+16], rax
```

The first two instructions pick up the pointer to the match context
and from the match context the current bit offset into the binary:

```nasm
    mov rcx, qword ptr [rbx+8]   ; Load pointer to match context
    mov rsi, qword ptr [rcx+22]  ; Get offset in bits into binary
```

The next three instructions ensure that the length of the binary is at
least 8 bits:

```nasm
    lea rdx, qword ptr [rsi+8]   ; Add 8 to the offset
    cmp rdx, qword ptr [rcx+30]  ; Compare offset+8 with size of binary
    ja label_25                  ; Fail if the binary is too short
```

The next five instructions test whether the current byte in the binary
is aligned at a byte boundary. If not, a helper code fragment is
called:


```nasm
    rex test sil, 7    ; Test the 3 least significant bits
    short je L91       ; Jump if 0 (meaning byte-aligned)
    mov edx, 64        ; Load size and flags
    call L92           ; Call helper fragment
    short jmp L90      ; Done
```

A **helper code fragment** is a shared block of code that can be
called from the native code generated for BEAM instructions, typically
to handle cases that are uncommon and/or would require more native
instructions than are practial to include inline. Each such code
fragment has its own calling convention, typically tailor-made to be
as convenient for the caller as possible. (See [Further adventures in
the JIT](https://www.erlang.org/blog/jit-part-2/) for more information
about helper code fragments.)

The remaining instructions read one byte from memory, convert it to a
tagged Erlang terms, store it in `{x,2}`, and advance the bit offset
in the match context:

```nasm
L91:
    mov rdi, qword ptr [rcx+14]    ; Load base pointer for binary
    shr rsi, 3                     ; Convert bit offset to byte offset
    mov qword ptr [rcx+22], rdx    ; Update bit offset in match context
    movzx rax, byte ptr [rdi+rsi]  ; Read one byte from the binary
    shl rax, 4                     ; Multiply by 16...
    or rax, 15                     ; ... and add tag for a small integer

L90:
    mov qword ptr [rbx+16], rax    ; Store extracted integer
```

When matching a segment of size other than one of the special sizes
mentioned earlier, the JIT will always emit a call to a general
routine that can handle matching of any integer segment with any
aligment, endianness, and signedness.

In OTP 25, the full potential for optimization of the `bs_create_bin`
instruction is not realized. The construction of each segment is done
by calling a helper routine that builds the segment. Here is the
native for the part of the `bs_create_bin` instruction that builds the
integer segments:

```nasm
# construct integer segment
    mov edx, 24
    mov rsi, qword ptr [rbx+24]
    xor ecx, ecx
    lea rdi, qword ptr [rbx-80]
    call 4387496416
# construct integer segment
    mov edx, 8
    mov rsi, qword ptr [rbx+16]
    xor ecx, ecx
    lea rdi, qword ptr [rbx-80]
    call 4387496416
```

### Binary pattern matching in OTP 26

In OTP 26, there is a new BEAM `bs_match` instruction used for
matching segments with sizes known at compile time. The BEAM code for
the matching code in the function head for `bin_swap/1` is as follows:

```
    {test,bs_start_match3,{f,1},1,[{x,0}],{x,1}}.
    {bs_get_position,{x,1},{x,0},2}.
    {bs_match,{f,2},
              {x,1},
              {commands,[{ensure_exactly,32},
                         {integer,2,{literal,[]},8,1,{x,2}},
                         {integer,3,{literal,[]},24,1,{x,3}}]}}.
```

The first two instructions are identical to their OTP 25 counterparts.

The first operand of the `bs_match` instruction, `{f,2}`, is the
failure label and the second operand `{x,2}` is the register holding
the match context. The third operand, `{commands,[...]}`, is a list of
matching commands.

The first command in the `commands` list, `{ensure_exactly,32}`, tests
that the remaining number of bits in the binary being matched is
exactly 32. If not, a jump is made to the failure label.

The second command extracts an integer of 8 bits and stores it in
`{x,2}`. The third command extracts an integer of 24 bits and store it
in `{x,3}`.

Having matching of multiple segments contained in a single BEAM
instruction makes it much easier for the JIT to generate efficient
code. Here is what the native code will do:

* Test that there are at exactly 32 bits left in the binary.

* If the segment is byte-aligned, read a 4-byte word from the binary
  and store it in a CPU register.

* If the segment is not byte-aligned, read an 8-byte word from the binary
and shift to extract the 32 bits needed.

* Shift and mask out 8 bits and tag as an integer. Store into `{x,2}`.

* Shift and mask out 24 bits and tag as an integer. Store into `{x,3}`.

The native code for the `bs_match` instruction (slightly simplifed) is
as follows:

```nasm
# i_bs_match_fS
# ensure_exactly 32
    mov rsi, qword ptr [rbx+8]
    mov rax, qword ptr [rsi+30]
    mov rcx, qword ptr [rsi+22]
    sub rax, rcx
    cmp rax, 32
    jne label_3
# read 32
    mov rdi, qword ptr [rsi+14]
    add qword ptr [rsi+22], 32
    mov rax, rcx
    shr rax, 3
    add rdi, rax
    and ecx, 7
    jnz L38
    movbe edx, dword ptr [rdi]
    add ecx, 32
    short jmp L40
L38:
    mov rdx, qword ptr [rdi-3]
    shr rdx, 24
    bswap rdx
L40:
    shl rdx, cl
# extract integer 8
    mov rax, rdx
# store extracted integer as a small
    shr rax, 52
    or rax, 15
    mov qword ptr [rbx+16], rax
    shl rdx, 8
# extract integer 24
    shr rdx, 36
    or rdx, 15
    mov qword ptr [rbx+24], rdx
```

The first part of the code ensures that there are exactly 32 bits
remaining in the binary:


```nasm
# ensure_exactly 32
    mov rsi, qword ptr [rbx+8]    ; Get pointer to match context
    mov rax, qword ptr [rsi+30]   ; Get size of binary in bits
    mov rcx, qword ptr [rsi+22]   ; Get offset in bits into binary
    sub rax, rcx
    cmp rax, 32
    jne label_3
```

The next part of the code does not directly correspond to the commands
in the `bs_match` BEAM instruction. Instead, the code reads 32 bits
from the binary:

```nasm
# read 32
    mov rdi, qword ptr [rsi+14]
    add qword ptr [rsi+22], 32  ; Increment bit offset in match context
    mov rax, rcx
    shr rax, 3
    add rdi, rax
    and ecx, 7                  ; Test alignment
    jnz L38                     ; Jump if segment not byte-aligned

    ; Read 32 bits (4 bytes) byte-aligned and convert to big-endian
    movbe edx, dword ptr [rdi]
    add ecx, 32
    short jmp L40

L38:
    ; Read a 8-byte word and extract the 32 bits that are needed.
    mov rdx, qword ptr [rdi-3]
    shr rdx, 24
    bswap rdx                   ; Convert to big-endian

L40:
    ; Shift the read bytes to the most significant bytes of the word
    shl rdx, cl
```

The 4 bytes read will be converted to big-endian and placed as the
most significant bytes of CPU register `rdx` with the rest of the
register zeroed.

The following instructions extracts the 8 bits for the first segment and
stores it as a tagged integer in `{x,2}`:

```nasm
# extract integer 8
    mov rax, rdx
# store extracted integer as a small
    shr rax, 52
    or rax, 15
    mov qword ptr [rbx+16], rax
    shl rdx, 8
```

The following instructions extracts the 24 bits for the second segment and
stores it as a tagged integer in `{x,3}`:

```nasm
# extract integer 24
    shr rdx, 36
    or rdx, 15
    mov qword ptr [rbx+24], rdx
```

### Binary construction in OTP 26

For binary construction in OTP 26, the compiler emits a
`bs_create_bin` BEAM instruction just as in OTP 25. However, the
native code that the JIT in OTP 26 emits for that instruction bears
little resemblance to the native code emitted by OTP 25. The native
code will do the following:

* Allocate room on the heap for a binary and initialize it with
inlined native code. A helper code fragment is called to do a garbage
collection if there is not sufficient room left on the heap.

* Read the integer from `{x,3}` and untag it.

* Read the integer from `{x,2}` and untag it. Combine the value with
the previous 24-bit value to obtain a 32-bit value.

* Write the combined 32 bits into the binary.

Here follows the complete native code for the `bs_create_bin`
instruction (somewhat simplified):

```nasm
# i_bs_create_bin_jItd
# allocate heap binary
    lea rdx, qword ptr [r15+56]
    cmp rdx, rsp
    short jbe L43
    mov ecx, 4
.db 0x90
    call 4343630296
L43:
    lea rax, qword ptr [r15+2]
    mov qword ptr [rbx-120], rax
    mov qword ptr [r15], 164
    mov qword ptr [r15+8], 4
    add r15, 16
    mov qword ptr [rbx-64], r15
    mov qword ptr [rbx-56], 0
    add r15, 8
# accumulate value for integer segment
    xor r8d, r8d
    mov rdi, qword ptr [rbx+24]
    sar rdi, 4
    or r8, rdi
# accumulate value for integer segment
    shl r8, 8
    mov rdi, qword ptr [rbx+16]
    sar rdi, 4
    or r8, rdi
# construct integer segment from accumulator
    bswap r8d
    mov rdi, qword ptr [rbx-64]
    mov qword ptr [rbx-56], 32
    mov dword ptr [rdi], r8d
```

Let's walk through it.

The first part of the code, starting with `# allocate heap binary` and
ending before the next comment line allocates a **heap binary** with
inlined native code. The only call to a helper code fragment is in case
there is not sufficient space left on the heap.

Next follows the construction of the segments of the binary.

Instead of writing the value of each segment to memory one at a time,
multiple segments are accumulated into a CPU register. Here
follows the code for the first segment to be constructed (24 bits):

```nasm
# accumulate value for integer segment
    xor r8d, r8d                ; Initialize accumulator
    mov rdi, qword ptr [rbx+24] ; Fetch {x,3}
    sar rdi, 4                  ; Untag
    or r8, rdi                  ; OR into accumulator
```

Here follows the code for the second segment (8 bits):

```nasm
# accumulate value for integer segment
    shl r8, 8                   ; Make room for 8 bits
    mov rdi, qword ptr [rbx+16] ; Fetch {x,2}
    sar rdi, 4                  ; Untag
    or r8, rdi                  ; OR into accumulator
```

Since there are no segments of the binary left, the accumulated
value will be written out to memory:

```nasm
# construct integer segment from accumulator
    bswap r8d                   ; Make accumulator big-endian
    mov rdi, qword ptr [rbx-64] ; Get pointer into binary
    mov qword ptr [rbx-56], 32  ; Update size of binary
    mov dword ptr [rdi], r8d    ; Write 32 bits
```

### Appending to binaries in OTP 25

The ancient OTP R12B release introduced an optimization for
[efficiently appending to a
binary](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html). Let's
look at an example to see the optimization in action:


```erlang
-module(append).
-export([expand/1, expand_bc/1]).

expand(Bin) when is_binary(Bin) ->
    expand(Bin, <<>>).

expand(<<B:8,T/binary>>, Acc) ->
    expand(T, <<Acc/binary,B:16>>);
expand(<<>>, Acc) ->
    Acc.

expand_bc(Bin) when is_binary(Bin) ->
    << <<B:16>> || <<B:8>> <= Bin >>.
```

Both `append:expand/1` and `append:expand_bc/1` take a binary and
double its size by expanding each byte to two bytes. For example:

```erlang
1> append:expand(<<1,2,3>>).
<<0,1,0,2,0,3>>
2> append:expand_bc(<<4,5,6>>).
<<0,4,0,5,0,6>>
```

Both functions accept only binaries:

```erlang
3> append:expand(<<1,7:4>>).
** exception error: no function clause matching append:expand(<<1,7:4>>,<<>>)
4> append:expand_bc(<<1,7:4>>).
** exception error: no function clause matching append:expand_bc(<<1,7:4>>)
```

Before looking at the BEAM code, let's do some benchmarking using
[erlperf][erlperf] to find out which function is faster:


```
erlperf --init_runner_all 'rand:bytes(10_000).' \
        'r(Bin) -> append:expand(Bin).' \
        'r(Bin) -> append:expand_bc(Bin).'
Code                                     ||        QPS       Time   Rel
r(Bin) -> append:expand_bc(Bin).          1       7936     126 us  100%
r(Bin) -> append:expand(Bin).             1       4369     229 us   55%
```

The expression for the `--init_runner_all` option uses
[rand:bytes/1][rand_bytes_1] to create a binary with 10,000 random
bytes, which will be passed to both expand functions.

From the benchmark results, it can be seen that the `expand_bc/1` function is
almost twice as fast.

To find out why, let's compare the BEAM code for the two functions. Here is
the instruction that appends to the binary in `expand/1`:

```
    {bs_create_bin,{f,0},
                   0,3,8,
                   {x,1},
                   {list,[{atom,append},  % Append operation
                          1,8,nil,
                          {tr,{x,1},{t_bitstring,1}}, % Source/destination
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{0,255}}},
                          {integer,16}]}}.
```

The first segment is an `append` operation. The operand
`{tr,{x,1},{t_bitstring,1}}` denotes both source and destination of
the operation. That is, the binary referenced by `{x,1}` will be
mutated. Erlang normally does not allow mutation, but this mutation
is done under the hood in a way not observable from outside. That
makes the append operation much more efficient than it would be if the
source binary had to be copied.

For the binary comprehension in `expand_bc/1`, there is a similar
BEAM instruction for appending to the binary:

```
    {bs_create_bin,{f,0},
                   0,3,1,
                   {x,1},
                   {list,[{atom,private_append}, % Private append operation
                          1,1,nil,
                          {x,1},
                          {atom,all},
                          {atom,integer},
                          2,1,nil,
                          {tr,{x,2},{t_integer,{0,255}}},
                          {integer,16}]}}.
```

The main difference is that the binary comprehension uses the more
efficient `private_append` operation instead of `append`.

The `append` operation has more overhead because it must produce the
correct result for code such as:

```erlang
bins(Bin) ->
    bins(Bin, <<>>).

bins(<<H,T/binary>>, Acc) ->
    [Acc|bins(T, <<Acc/binary,H>>)];
bins(<<>>, Acc) ->
    [Acc].
```

Running it:

```erlang
1> example:bins(<<"abcde">>).
[<<>>,<<"a">>,<<"ab">>,<<"abc">>,<<"abcd">>,<<"abcde">>]
```

In the `expand/1` function, only the final value binary being appended
to was needed. In `bins/1`, all of the intermediate values of binary
are collected in a list. For correctness, the `append` operations must
ensure that the binary `Acc` is copied before `H` is appended to
it. To be able to know when it is necessary to copy the binary, the
`append` operation does some extra bookeeping that does not come
for free.

### Appending to binaries in OTP 26

In OTP 26, there is a new optimization in the compiler that replaces
an `append` operation with a `private_append` operation whenever it is
correct and safe to do so. This optimization was implemented by Frej
Drejhammar. That is, the optimization will rewrite `append:expand/2`
to use `private_append`, but not `examples:bins/2`.

The difference between `append:expand/1` and `append:expand_bc/1` is now
much smaller:

```
erlperf --init_runner_all 'rand:bytes(10_000).' \
        'r(Bin) -> append:expand(Bin).' \
        'r(Bin) -> append:expand_bc(Bin).'
Code                                     ||        QPS       Time   Rel
r(Bin) -> append:expand_bc(Bin).          1      13164   75988 ns  100%
r(Bin) -> append:expand(Bin).             1      12419   80550 ns   94%
```

`expand_bc/1` is still a bit faster because the compiler emits
somewhat more efficient binary matching code for it than for the
`expand/1` function.

### The benefit of `is_binary/1` guards

The `expand/1` function has an `is_binary/1` guard test that may seem
unnecessary:

```erlang
expand(Bin) when is_binary(Bin) ->
    expand(Bin, <<>>).
```

The guard test is not necessary for correctness, because `expand/2`
will raise a `function_clause` exception if its argument is not a
binary. However, better code will be generated for `expand/2` with
the guard test.

With the guard test, the first BEAM instruction in `expand/2` is:

```
    {bs_start_match4,{atom,no_fail},2,{x,0},{x,0}}.
```

Without the guard test, the first BEAM instruction is:

```
    {test,bs_start_match3,{f,3},2,[{x,0}],{x,2}}.
```

The `bs_start_match4` instruction is more efficient because it does
not have to test that `{x,0}` contains a binary.

The benchmark results show measurable increased execution time for
`expand/1` if the guard test is removed:

```
erlperf --init_runner_all 'rand:bytes(10_000).' \
        'r(Bin) -> append:expand(Bin).' \
        'r(Bin) -> append:expand_bc(Bin).'
Code                                     ||        QPS       Time   Rel
r(Bin) -> append:expand_bc(Bin).          1      13273   75366 ns  100%
r(Bin) -> append:expand(Bin).             1      11875   84236 ns   89%
```

### Revisiting the `base64` module

Traditionally, up to OTP 25, the clause in the `base64` module that does
most of the work of encoding a binary to Base64 looked like this:

```erlang
encode_binary(<<B1:8, B2:8, B3:8, Ls/bits>>, A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_binary(Ls,
                  <<A/bits,(b64e(BB bsr 18)):8,
                    (b64e((BB bsr 12) band 63)):8,
                    (b64e((BB bsr 6) band 63)):8,
                    (b64e(BB band 63)):8>>).
```

The reason is that matching out segments of size 8 has always been
specially optimized and has been much faster than matching out a
segment of size 6. That is no longer true in OTP 26. With the
improvements in binary matching described in this blog post, the
clause can be written in a more natural way:

```erlang
encode_binary(<<B1:6, B2:6, B3:6, B4:6, Ls/bits>>, A) ->
    encode_binary(Ls,
                  <<A/bits,
                    (b64e(B1)):8,
                    (b64e(B2)):8,
                    (b64e(B3)):8,
                    (b64e(B4)):8>>);
```

(This is not the exact code in OTP 26, because of
[additional](https://github.com/erlang/otp/pull/6280)
[features](https://github.com/erlang/otp/pull/6711) added later.)

The benchmark results for encoding a random binary of 1,000,000 bytes
to Base64 for OTP 25 is:

```
erlperf --init_runner_all 'rand:bytes(1_000_000).' \
        'r(Bin) -> base64:encode(Bin).'
Code                                  ||        QPS       Time
r(Bin) -> base64:encode(Bin).          1         61   16489 us
```

The benchmark results for encoding a random binary of 1,000,000 bytes
to Base64 for OTP 26 is:

```
erlperf --init_runner_all 'rand:bytes(1_000_000).' \
        'r(Bin) -> base64:encode(Bin).'
Code                                  ||        QPS       Time
r(Bin) -> base64:encode(Bin).          1        249    4023 us
```

That is, encoding is about 4 times faster.


### Pull requests

Here are the main pull requests for the optimizations mentioned in
this blog post:

* [compiler: Improve the type analysis](https://github.com/erlang/otp/pull/5999)
* [JIT: Optimise common combinations of relational operators](https://github.com/erlang/otp/pull/6025)
* [JIT: Minor optimizations](https://github.com/erlang/otp/pull/6298), which includes
  the optimization that avoids fetching an operand that is already in a CPU register.
* [compiler: Optimize record updates](https://github.com/erlang/otp/pull/6033)
* [JIT: Optimize binary matching for fixed-width segments](https://github.com/erlang/otp/pull/6259)
* [JIT: Optimize creation of binaries](https://github.com/erlang/otp/pull/6031)
* [compiler: `private_append` optimization for binaries](https://github.com/erlang/otp/pull/6804)

[beam_code]: https://www.erlang.org/blog/a-brief-beam-primer
[tags]: http://www.it.uu.se/research/publications/reports/2000-029/2000-029-nc.pdf
[erlperf]: https://github.com/max-au/erlperf
[rand_bytes_1]: https://www.erlang.org/doc/man/rand.html#bytes-1
