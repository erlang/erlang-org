---
layout: post
title: Type-Based Optimizations in the JIT
tags: BEAM JIT
author: Björn Gustavsson
---

This post explores the new type-based optimizations in Erlang/OTP 25 where
the compiler embeds type information in the BEAM files to help the
[JIT (Just-In-Time compiler)][jit] to generate better code.

[jit]: https://www.erlang.org/blog/a-first-look-at-the-jit/

### The best of both worlds

The [SSA-based compiler passes][ssa] introduced in OTP 22 does a
sophisticated type analysis, which allows for more optimizations and
better code generation. There are, however, limits to what kind of
optimizations the Erlang compiler can do because a BEAM file must be
possible to load on any BEAM machine running on a 32-bit or 64-bit
computer. Therefore, the compiler cannot do optimizations that depend
on the size of integers that fit in a machine word or on how
Erlang terms are represented.

The JIT (introduced in OTP 24) knows that it is running on a 64-bit
computer and knows how Erlang terms are represented. The JIT is still
limited in how much optimization it can do because it translates a
single BEAM instruction at the time. For example, the `+` operator can
add floats or integers of any size or any combination
thereof. Previously executed BEAM instructions might have made it
clear that the operands can only be small integers, but the JIT does
not know that since it only looks at one instruction at the time, and
therefore it must emit native code that handles all possible operands.

In OTP 25, the compiler has been updated to embed type information in
the BEAM file and the JIT has been extended to emit better code based
on that type information.

The embedded type information is versioned so that we can continue to
improve the type-based optimizations in every OTP release. The loader
will ignore versions it does not recognize so that the module can
still be loaded without the type-based optimizations.

[ssa]: https://blog.erlang.org/ssa-history/

### What to expect of the JIT in OTP 25

OTP 25 is just the beginning for type-based optimizations. We hope to
improve both the type information from the compiler and the
optimizations in the JIT in OTP 26.

How much better the native code emitted by the JIT will be depends
on the nature of the code in the module.

The most commonly applied optimization is simplified tests. For
example, a test for a tuple can frequently be reduced from 5
instructions down to 3 instructions, and a test for small integer
operands can frequently be reduced from 5 instructions down to 4
instructions.

Less commonly applied but more significant are the simplifications
that can be made when an integer is known to be "small" (fits in 60
bits). For example, a relational operator (such as `<`) used in a
guard can be reduced from 11 instructions down to 4 if the operands
are known to be small integers. This kind of optimization is most
often applied in modules that use binary pattern matching because
integers matched out from a binary have a well-defined range.

In the Erlang/OTP code base, the first kind of optimizations (shaving
off one or two instructions) are applied roughly ten times as often as
the second kind.

We will see later in this blog post that the optimizations of the
second kind applied to the `base64` module resulted in a significant
speed up.

### Simplifications of type tests

Let's dive right into some examples.

Consider this module:

```erlang
-module(example).
-export([tuple_matching/1]).

tuple_matching(X) ->
    case increment(X) of
        {ok,Result} -> Result;
        error -> X
    end.

increment(X) when is_integer(X) -> {ok,X+1};
increment(_) -> error.
```

The [BEAM code][beam_code] for the `tuple_matching/1` function emitted
by the compiler in OTP 24 is (somewhat simplified):

```
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {call,1,{f,5}}.
    {test,is_tuple,{f,3},[{x,0}]}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {deallocate,1}.
    return.
  {label,3}.
    {move,{y,0},{x,0}}.
    {deallocate,1}.
    return.
```

The compiler has figured out that the `increment/1` returns either the
atom `error` or a two-tuple with `ok` as the first element. Therefore,
to distinguish between those two possible return values, a single
instruction suffices:

```
    {test,is_tuple,{f,3},[{x,0}]}.
```

There is no need to explicitly test for the value `error` because it
**must** be `error` if it is not a tuple. Similarly, there is no need
to test that the first element of the tuple is `ok` because it must be.

In OTP 24, the JIT translates that instruction to a sequence of 5 native
instructions for x86_64:

```assembly
# i_is_tuple_fs
    mov rsi, qword ptr [rbx]
    rex test sil, 1
    jne L2
    test byte ptr [rsi-2], 63
    jne L2
```

(Lines starting with `#` are comments.)

The `mov` instruction fetches the value of the BEAM register `{x,0}`
to the CPU register `rsi`. The next two instructions test whether the
term is a pointer to an object on the heap. If it is, the header word
for the heap object is tested to make sure it is a tuple. The second
test is needed because the heap object could be some other Erlang term,
such as a binary, a map, or an integer that does not fit in a machine
word.

Now let's see what the compiler and the JIT in OTP 25 do with this
instruction. The BEAM code is now:

```erlang
    {test,is_tuple,
          {f,3},
          [{tr,{x,0},
               {t_union,{t_atom,[error]},
                        none,none,
                        [{{2,{t_atom,[ok]}},
                          {t_tuple,2,true,
                                   #{1 => {t_atom,[ok]},
                                     2 => {t_integer,any}}}}],
                        none}}]}.
```

The operand that was `{x,0}` in OTP 24 is now a tuple:

```erlang
{tr,Register,Type}
```

That is, it is a three-tuple with `tr` as the first element. `tr`
stands for **typed register**. The second element is the BEAM register
(`{x,0}` in this case), and the third element is the type of the
register in the compiler's internal type representation. The type
is equivalent to the following type spec:

```erlang
'error' | {'ok', integer()}
```

The JIT cannot take advantage of that level of detail in the types,
so the compiler embeds a [simplified
version](https://github.com/erlang/otp/blob/de5bb49320db22159de52e677c5f7499b763b0cd/lib/compiler/src/beam_types.erl#L1153-L1241)
of that type into the BEAM file. The embedded type is equivalent to:

```erlang
atom() | tuple()
```

By knowing that `{x,0}` must be an atom or a tuple, the JIT in OTP 25
emits the following simplified native code:

```assembly
# i_is_tuple_fs
    mov rsi, qword ptr [rbx]
# simplified tuple test since the source is always a tuple when boxed
    rex test sil, 1
    jne label_3
```

(The JIT generally emits a comment when type information made a simplification
possible.)

Only the first test is now necessary, because if the term is a pointer
to a heap object, according to the type information, it **must** be a tuple.

### Simplification of relational operators

As another example, let's look at how the relational operators in
guards are translated. Consider this function:

```erlang
my_less_than(A, B) ->
    if
        A < B -> smaller;
        true -> larger_or_equal
    end.
```

The BEAM code looks like this:

```
    {test,is_lt,{f,9},[{x,0},{x,1}]}.
    {move,{atom,smaller},{x,0}}.
    return.
  {label,9}.
    {move,{atom,larger_or_equal},{x,0}}.
    return.
```

When relational operators are used as guard tests, the compiler rewrites
them as special instructions. Thus, the `<` operator is rewritten to an
`is_lt` instruction.

The `<` operator can compare any Erlang terms. It would be impractical
for the JIT to emit the code to handle all kinds of terms. Therefore, the
JIT emits code that directly handles the most common case and
calls a generic routine to handle everything else:

```assembly
# is_lt_fss
    mov rsi, qword ptr [rbx+8]
    mov rdi, qword ptr [rbx]
    mov eax, edi
    and eax, esi
    and al, 15
    cmp al, 15
    short jne L39
    cmp rdi, rsi
    short jmp L40
L39:
    call 5447639136
L40:
    jge label_9
```

Let's walk through the code. The first two instructions:

```assembly
    mov rsi, qword ptr [rbx+8]
    mov rdi, qword ptr [rbx]
```

fetches the BEAM registers `{x,1}` and `{x,0}` into CPU registers.

The most common comparison is between two integers. Depending on the
magnitude, integers can be represented in two different ways. On a 64-bit
computer, signed integers that fit in 60 bits will be stored directly
in a 64-bit word. The remaining 4 bits in the words are used for the
[tag][tags], which for a small integer is `15`. If the integer does
not fit, it will be represented as a **bignum**, which is pointer to
an object on the heap.

Here is the native code for testing that both operands are small:

```assembly
    mov eax, edi
    and eax, esi
    and al, 15
    cmp al, 15
    short jne L39
```

If one or both of the operands have another tag than `15` (are not
small integers), control is transferred to code at label `L39` that
handles all other types of terms.

The next lines do the comparison of the small integers. The code is
written in a slightly convoluted way so that the conditional jump
(`jge label_9`) that transfers control to the failure label can be
shared with the generic code:

```assembly
    cmp rdi, rsi
    short jmp L40
L39:
    call 5447639136
L40:
    jge label_9
```

Thus, without type information, 11 instructions are needed to implement
`is_lt`.

Now let's see what happens when types are available:

```erlang
my_less_than(A, B) when is_integer(A), is_integer(B) ->
    .
    .
    .
```

When compiled by the compiler in OTP 25, the BEAM code is:

```
    {test,is_integer,{f,7},[{x,0}]}.
    {test,is_integer,{f,7},[{x,1}]}.
    {test,is_lt,{f,9},[{tr,{x,0},{t_integer,any}},{tr,{x,1},{t_integer,any}}]}.
    {move,{atom,smaller},{x,0}}.
    return.
  {label,9}.
    {move,{atom,larger_or_equal},{x,0}}.
    return.

```

The operands for the `is_lt` instruction now have types. The BEAM
registers `{x,0}` and `{x,1}` have the type `{t_integer,any}`, which
means an integer with an unknown range.

Having that knowledge of the types, the JIT can emit a slightly
shorter test for a small integer:

```assembly
# simplified small test since all other types are boxed
    mov eax, edi
    and eax, esi
    test al, 1
    short je L39
```

To do a better job, the JIT will need better type information. For example:

```erlang
map_size_less_than(Map1, Map2) ->
    if
        map_size(Map1) < map_size(Map2) -> smaller;
        true -> larger_or_equal
    end.
```

The BEAM code looks like this:

```
    {gc_bif,map_size,{f,12},2,[{x,0}],{x,0}}.
    {gc_bif,map_size,{f,12},2,[{x,1}],{x,1}}.
    {test,is_lt,
          {f,12},
          [{tr,{x,0},{t_integer,{0,288230376151711743}}},
           {tr,{x,1},{t_integer,{0,288230376151711743}}}]}.
    {move,{atom,smaller},{x,0}}.
    return.
  {label,12}.
    {move,{atom,larger_or_equal},{x,0}}.
    return.
```

Both operands for `is_lt` now have the type
`{t_integer,{0,288230376151711743}}`, meaning an integer in the range
0 through 288230376151711743 (that is, `(1 bsl 58) - 1`). There is no
documented upper limit for the number of elements in a map, but for
the foreseeable future, there is no way that the number of elements in
a map will exceed or even get close to 288230376151711743.

Since both the lower and upper bounds for `{x,0}` and `{x,1}` fit in
60 bits, there is no need to test the type of the operands:

```assembly
# is_lt_fss
    mov rsi, qword ptr [rbx+8]
    mov rdi, qword ptr [rbx]
# skipped test for small operands since they are always small
    cmp rdi, rsi
L42:
L43:
    jge label_12
```

Since the operands are always small, the call to the generic routine
(following label `L42`) has been omitted.

### Simplification of addition

Looking at arithmetic instructions, we will see the potential for nice
simplifications by the JIT, but unfortunately we will also see the
limitations of the type analysis done by the Erlang compiler in
OTP 25.

Let's look at the generated code for this function:

```erlang
add1(X, Y) ->
    X + Y.
```

The BEAM code looks like this:


```
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.
```

The JIT translates the `+` instruction to the following native instructions:

```assembly
# i_plus_ssjd
    mov rsi, qword ptr [rbx]
    mov rdx, qword ptr [rbx+8]
# are both operands small?
    mov eax, esi
    and eax, edx
    and al, 15
    cmp al, 15
    short jne L15
# add with overflow check
    mov rax, rsi
    mov rcx, rdx
    and rcx, -16
    add rax, rcx
    short jno L14
L15:
    call 4328985696
L14:
    mov qword ptr [rbx], rax
```

The first two instructions:

```assembly
    mov rsi, qword ptr [rbx]
    mov rdx, qword ptr [rbx+8]
```
loads the operands for the `+` operation BEAM registers into CPU registers.

The next 5 instructions tests for small operands:

```assembly
# are both operands small?
    mov eax, esi
    and eax, edx
    and al, 15
    cmp al, 15
    short jne L15
```

The code is almost identical to the code in the `is_lt` instruction
that we examined earlier. The only difference is that other CPU
registers are used. If one or both of the operands is not a small
integer, a jump is made to label `L15`, which looks like this:

```assembly
L15:
    call 4328985696
```

This code calls a generic routine that can add any combination of
small, bignums, or floats. The generic routine will also handle
non-number operands by raising a `badarith` exception.

If both operands indeed are smalls, the following code adds them and
checks for overflow:

```assembly
# add with overflow check
    mov rax, rsi
    mov rcx, rdx
    and rcx, -16
    add rax, rcx
    short jno L14
```

If the addition overflowed, the generic addition routine is
called. Otherwise, control is transferred to the following
instruction:

```assembly
    mov qword ptr [rbx], rax
```

which stores the result in `{x,0}`.

To summarize, the addition itself (including dealing with the [tags][tags]) requires
4 instructions. However, 10 more instructions are needed to:

* Fetch operands from BEAM registers.
* Check that the operands are small integers (at most 60 bits).
* Calling the generic addition routine.
* Storing the result to a BEAM register.

Now let's see what happens if types are introduced.

Consider:

```erlang
add2(X0, Y0) ->
    X = 2 * X0,
    Y = 2 * Y0,
    X + Y.
```

The BEAM code looks like:

```
    {gc_bif,'*',{f,0},2,[{x,0},{integer,2}],{x,0}}.
    {gc_bif,'*',{f,0},2,[{x,1},{integer,2}],{x,1}}.
    {gc_bif,'+',{f,0},2,[{tr,{x,0},number},{tr,{x,1},number}],{x,0}}.
    return.
```

Types are propagated from arithmetic instructions to other arithmetic
instructions. Because the result of `*` (if it succeeds) is a number
(integer or float), the operands for the `+` instruction now have the
type `number`.

Based on our experience of adding types to the `<` operator, we might
guess that we would save only one instruction in the type test. We
would be right:

```assembly
# simplified test for small operands since both are numbers
    mov eax, esi
    and eax, edx
    test al, 1
    short je L22
```

Returning to the simpler example with addition and no multiplication,
let's add a guard to ensure that `X` and `Y` are integers:

```erlang
add3(X, Y) when is_integer(X), is_integer(Y) ->
    X + Y.
```

That results in the following BEAM code:


```
    {test,is_integer,{f,5},[{x,0}]}.
    {test,is_integer,{f,5},[{x,1}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,any}},{tr,{x,1},{t_integer,any}}],
            {x,0}}.
    return.
```

The types for both operands are now `{t_integer,any}`. However, that
will still result in the same simplified four-instruction sequence for
testing small integers, because the integers might not fit in 60 bits.

Clearly, based on our experience with `is_lt`, we will need to establish
a range for `X` and `Y`. A reasonable way to do that would be:

```erlang
add4(X, Y) when is_integer(X), 0 =< X, X < 16#400,
                is_integer(Y), 0 =< Y, Y < 16#400 ->
    X + Y.
```

However, because of limitations in the compiler's value range analysis,
the types for the `+` operator will **not** improve:

```
    {test,is_integer,{f,19},[{x,0}]}.
    {test,is_ge,{f,19},[{tr,{x,0},{t_integer,any}},{integer,0}]}.
    {test,is_lt,{f,19},[{tr,{x,0},{t_integer,any}},{integer,1024}]}.
    {test,is_integer,{f,19},[{x,1}]}.
    {test,is_ge,{f,19},[{tr,{x,1},{t_integer,any}},{integer,0}]}.
    {test,is_lt,{f,19},[{tr,{x,1},{t_integer,any}},{integer,1024}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,any}},{tr,{x,1},{t_integer,any}}],
            {x,0}}.
    return.
```

To add insult to injury, the first 6 instructions cannot be simplified
by the JIT because there is not sufficient type information. That is,
the `is_lt` and `is_ge` instructions will comprise 11 instructions each.

We aim to improve the type analysis and optimizations in OTP 26 and
generate better code for this example. We are also considering adding
a new guard BIF in OTP 26 for testing that a term is an integer in a
given range.

Meanwhile, while we wait for OTP 26, there is a way in
OTP 25 to write an equivalent guard that will result in
much more efficient code **and** establish known ranges for `X` and
`Y`:

```assembly
add5(X, Y) when X =:= X band 16#3FF,
                Y =:= Y band 16#3FF ->
    X + Y.
```

We are showing this way of writing guard for illustrative purposes
only; we don't recommend rewriting your guards in this way.

The `band` operator fails if not both of its operands are integers, so
no `is_integer/1` test is needed. The `=:=` comparison will return
`false` if the corresponding variable is outside the range `0` through
`16#3FF`.

That will result in the following BEAM code, where the compiler now
has been able to figure out the possible ranges for the operands of
the `+` operator:

```
    {gc_bif,'band',{f,21},2,[{x,0},{integer,1023}],{x,2}}.
    {test,is_eq_exact,
          {f,21},
          [{tr,{x,0},{t_integer,any}},{tr,{x,2},{t_integer,{0,1023}}}]}.
    {gc_bif,'band',{f,21},2,[{x,1},{integer,1023}],{x,2}}.
    {test,is_eq_exact,
          {f,21},
          [{tr,{x,1},{t_integer,any}},{tr,{x,2},{t_integer,{0,1023}}}]}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,1023}}},{tr,{x,1},{t_integer,{0,1023}}}],
            {x,0}}.
    return.
```

Also, the 4 instructions that precede the `+` instructions are now
relatively efficient.

The `band` instruction needs to test the operands and be prepared to handle
integers that don't fit in 60 bits:

```assembly
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov eax, 16383
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L97
    and rax, rsi
    short jmp L98
L97:
    call 4456532680
    short je label_25
L98:
    mov qword ptr [rbx+16], rax
```

The `is_eq_exact` instruction benefits from type information derived from
executing the `band` instruction. Since the right-hand side operand is known
to be a small integer that fits in a machine word, a simple comparison is
sufficient with no need for fallback code to handle other Erlang terms:

```assembly
# is_eq_exact_fss
# simplified check since one argument is an immediate
    mov rdi, qword ptr [rbx+16]
    cmp qword ptr [rbx], rdi
    short jne label_25
```

The JIT generates the following code for the `+` operator:

```assembly
# i_plus_ssjd
# add without overflow check
    mov rax, qword ptr [rbx]
    mov rsi, qword ptr [rbx+8]
    and rax, -16
    add rax, rsi
    mov qword ptr [rbx], rax
```

### Simplifications for `base64`

As far as we know, `base64` is the module in OTP that has benefited
the most of the improvements in OTP 25.

Here follows benchmark results for a benchmark included in a [Github
issue](https://github.com/erlang/otp/issues/5639). First the results
for OTP 24 on my computer:

```
== Testing with 1 MB ==
fun base64:encode/1: 1000 iterations in 19805 ms: 50 it/sec
fun base64:decode/1: 1000 iterations in 20075 ms: 49 it/sec
```

The results for OTP 25 on the same computer:

```
== Testing with 1 MB ==
fun base64:encode/1: 1000 iterations in 16024 ms: 62 it/sec
fun base64:decode/1: 1000 iterations in 18306 ms: 54 it/sec
```

In OTP 25, the encoding is done in 80 percent of the time that OTP 24 needs.
Decoding is also more than a second faster.

The `base64` module has not been modified in OTP 25, so the improvements
are entirely down to improvements in the compiler and the JIT.

Here is the clause of `encode_binary/2` in the `base64` module that does
most of the work of encoding a binary to Base64:

```erlang
encode_binary(<<B1:8, B2:8, B3:8, Ls/bits>>, A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_binary(Ls,
                  <<A/bits,(b64e(BB bsr 18)):8,
                    (b64e((BB bsr 12) band 63)):8,
                    (b64e((BB bsr 6) band 63)):8,
                    (b64e(BB band 63)):8>>).
```

The binary matching in the function head establishes ranges for the
the variables `B1`, `B2`, and `B3`. (The types for all three variables
will be `{t_integer,{0,255}}`.)

Because of the ranges, all of the `bsl`, `bsr`, `band`, and `bor`
operations that follow do not need any type checks. Also, in the
creation of the binary, there is no need to test whether the binary
creation succeeded because all values are known to be small integers.

The 4 calls to the `b64e/1` functions are inlined. The function
looks like this:

```erlang
-compile({inline, [{b64e, 1}]}).
b64e(X) ->
    element(X+1,
	    {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
	     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
	     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
	     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
	     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/}).
```

In OTP 25, the JIT will optimize calls to `element/2` where the
position argument is an integer and the tuple argument is a literal
tuple. For the way `element/2` is used in `be64e/1`, all type tests
and range checks will be removed:

```assembly
# bif_element_jssd
# skipped tuple test since source is always a literal tuple
L302:
    long mov rsi, 9223372036854775807
    mov rdi, qword ptr [rbx+24]
    lea rcx, qword ptr [rsi-2]
# skipped test for small position since it is always small
    mov rax, rdi
    sar rax, 4
# skipped check for position =:= 0 since it is always >= 1
# skipped check for negative position and position beyond tuple
    mov rax, qword ptr [rcx+rax*8]
L300:
L301:
    mov qword ptr [rbx+24], rax
```

That is 7 instructions with no conditional branches.

### Please try this at home!

If you want to follow along and examine the native code for loaded
modules, start the runtime system like this:

```sh
erl +JDdump true
```

The native code for all modules that are loaded will be dumped to files with the
extension `.asm`.

To find code that has been simplified by the JIT, use this command:

```sh
egrep "simplified|skipped|without overflow" *.asm
```

To examine the BEAM code for a module, use the `-S` option. For example:

```sh
erlc -S base64.erl
```

### Pull requests

Here are the main pull requests that implement type-based optimizations:

* [jit: Optimize instructions based on operand types](https://github.com/erlang/otp/pull/5316)
* [JIT: Strengthen type-based optimizations](https://github.com/erlang/otp/pull/5664)
* [Further strengthen the type-based optimizations](https://github.com/erlang/otp/pull/5688)
* [jit: Fix integer ranges](https://github.com/erlang/otp/pull/5727)
* [JIT: Optimize bsl and bxor with known small operands](https://github.com/erlang/otp/pull/5849)
* [Compiler: Improve bounds calculation for bitwise operators](https://github.com/erlang/otp/pull/5855)

[beam_code]: https://www.erlang.org/blog/a-brief-beam-primer
[tags]: http://www.it.uu.se/research/publications/reports/2000-029/2000-029-nc.pdf
