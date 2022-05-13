---
layout: post
title:  Fast random integers
tags:   BEAM JIT, PRNG, rand, random
author: Raimo Niskanen
---

When you need "random" integers, and it is essential
to generate them fast and cheap; then maybe the full featured
Pseudo Random Number Generators in the `rand` module are overkill.
This blog post will dive in to new additions to the
said module, how the Just-In-Time compiler optimizes them,
known tricks, and tries to compare these apples and potatoes.

#### Contents
* [Speed over quality?](#speed-over-quality)
* [Suggested solutions](#suggested-solutions)
* [Quality](#quality)
* [Storing the state](#storing-the-state)
* [Seeding](#seeding)
* [JIT optimizations](#jit-optimizations)
* [Implementing a PRNG](#implementing-a-prng)
* [`rand_SUITE:measure/1`](#rand_suitemeasure1)
* [Measurement results](#measurement-results)
* [Summary](#summary)


Speed over quality?
-------------------

The Pseudo Random Number Generators implemented in
the `rand` module offers many useful features such as
repeatable sequences, non-biased range generation,
any size range, non-overlapping sequences,
generating floats, normal distribution floats, etc.
Many of those features are implemented through
a plug-in framework, with a performance cost.

The different algorithms offered by the `rand` module are selected
to have excellent statistical quality and to perform well
in serious PRNG tests (see section [PRNG tests]).

Most of these algorithms are designed for machines with
64-bit arithmetic (unsigned), but in Erlang such integers
become bignums and almost an order of magnitude slower
to handle than immediate integers.

Erlang terms in the 64-bit VM are tagged 64-bit words.
The tag for an immediate integer is 4 bit, leaving 60 bits
for the signed integer value.  The largest positive
immediate integer value is therefore 2<sup>59</sup>-1.

Many algorithms work on unsigned integers so we have
59 bits useful for that.  It could be theoretically
possible to pretend 60 bits unsigned using split code paths
for negative and positive values, but extremely impractical.

We decided to choose 58 bit unsigned integers in this context
since then we can for example add two integers, and check
for overflow or simply mask back to 58 bit, without
the intermediate result becoming a bignum.  To work with
59 bit integers would require having to check for overflow
before even doing an addition so the code that avoids
bignums would eat up much of the speed gained from
avoiding bignums.  So 58-bit integers it is!

The algorithms that perform well in Erlang are the ones
that have been redesigned to work on 58-bit integers.
But still, when executed in Erlang, they are far from
as fast as their C origins.  Achieving good PRNG quality
costs much more in Erlang than in C.  In the section
[Measurement results] we see that the algorithm `exsp`
that boasts sub-ns speed in C needs 17 ns in Erlang.

32-bit Erlang is a sad story in this regard.  The bignum limit
on such an Erlang system is so low, calculations would have
to use 26-bit integers, that designing a PRNG
not using bignums must be so small in period and size
that it becomes too bad to be useful.
The known trick `erlang:phash2(erlang:unique_integer(), Range)`
is still fairly fast, but all `rand` generators work exactly the same
as on a 64-bit system, hence operates on bignums so they are much slower.

If your application needs a "random" integer for an non-critical
purpose such as selecting a worker, choosing a route, etc,
and performance is much more important than repeatability
and statistical quality, what are then the options?



Suggested solutions
-------------------

* [Use `rand` anyway]
* [Write a BIF]
* [Write a NIF]
* [Use the system time]
* [Hash a "unique" value]
* [Write a simple PRNG]

Reasoning and measurement results are in the following sections,
but, in short:

* Writing a NIF, we deemed, does not achieve a performance worth the effort.
* Neither does writing a BIF.  But, ... a BIF (and a NIF, maybe) could
  implement a combination of performance and quality that cannot
  be achieved in any other way.  If a high demand on this combination
  would emerge, we could reconsider this decision.
* Using the system time is a bad idea.
* `erlang:phash2(erlang:unique_integer(), Range)` has its use cases.
* We have implemented a simple PRNG to fill the niche of non-critical
  but very fast number generation: `mwc59`.


### Use `rand` anyway

Is `rand` slow, really?  Well, perhaps not considering what it does.

In the [Measurement results] at the end of this text,
it shows that generating a good quality random number using
the `rand` module's default algorithm is done in 45 ns.

Generating a number as fast as possible (`rand:mwc59/1`) can be done
in less than 4 ns, but that algorithm has problems with the
statistical quality.  See section [PRNG tests] and [Implementing a PRNG].

Using a good quality algorithm instead (`rand:exsp_next/1`) takes 16 ns,
if you can store the generator's state in a loop variable.

If you can not store the generator state in a loop variable
there will be more overhead, see section [Storing the state].

Now, if you also need a number in an awkward range, as in not much smaller
than the generator's size, you might have to implement a reject-and-resample
loop, or even concatenate numbers.

The overhead of code that has to implement this much of the features
that the `rand` module already offers will easily approach
its 26 ns overhead, so often there is no point in
re-implementing this wheel...



### Write a BIF

There has been a discussion thread on Erlang Forums:
[Looking for a faster RNG].  Triggered by this Andrew Bennett
(aka [potatosalad]) wrote an [experimental BIF].

The suggested BIF `erlang:random_integer(Range)` offered
no repeatability, generator state per scheduler, guaranteed
sequence separation between schedulers, and high generator
quality.  All this thanks to using one of the good generators from
the `rand` module, but now written in its original
programming language, C, in the BIF.

The performance was a bit slower than the `mwc59` generator state update,
but with top of the line quality. See section [Measurement results].

Questions arised regarding maintenance burden, what more to implement, etc.
For example we probably also would need `erlang:random_integer/0`,
`erlang:random_float/0`, and some system info
to get the generator bit size...

A BIF could achieve good performance on a 32-bit system, if it there
would return a 27-bit integer, which became another open question.
Should a BIF generator be platform independent with respect to
generated numbers or with respect to performance?



### Write a NIF

[potatosalad] also wrote [a NIF], since we (The Erlang/OTP team)
suggested that it could have good enough performance.

Measurements, however, showed that the overhead is significantly larger
than for a BIF.  Although the NIF used the same trick as the BIF to store
the state in thread specific data it ended up with the same
performance as `erlang:phash2(erlang:unique_integer(), Range)`,
which is about 2 to 3 times slower than the BIF.

As a speed improvement we tried was to have the NIF generate
a list of numbers, and use that list as a cache in Erlang.
The performance with such a cache was as fast as the BIF,
but introduced problems such as that you would have to decide
on a cache size, the application would have to keep the cache on the heap,
and when generating in a number range the application would have to know
in generate numbers in the same range for the whole cache.

A NIF could like a BIF also achieve good performance on a 32-bit system,
with the same open question &mdash; platform independent numbers or performance?



### Use the system time

One suggested trick is to use `os:system_time(microseconds)` to get
a number.  The trick has some peculiarities:
* When called repeatedly you might get the same number several times.
* The resolution is system dependent, so on some systems you get
  the same number even more several times.
* Time can jump backwards and repeat in some cases.
* Historically it has been a bottleneck, especially on virtualized
  platforms.  Getting the OS time is harder then expected.

See section [Measurement results] for the performance for this "solution".



### Hash a "unique" value

The best combination would most certainly be
`erlang:phash2(erlang:unique_integer(), Range)` or
`erlang:phash2(erlang:unique_integer())` which is slightly faster.

`erlang:unique_integer/0` is designed to return an unique integer
with a very small overhead.  It is hard to find a better candidate
for an integer to hash.

`erlang:phash2/1,2` is the current generic hash function for Erlang terms.
It has a default return size well suited for 32-bit Erlang systems,
and it has a `Range` argument.  The range capping is done with a simple
`rem` in C (`%`) which is much faster than in Erlang.  This works good
 only for ranges much smaller than 32-bit as in if the range is larger
 than 16 bits the bias in the range capping starts to be noticable..

Alas this solution does not perform well in [PRNG tests].

See section [Measurement results] for the performance for this solution.



### Write a simple PRNG

To be fast, the implementation of a PRNG algorithm cannot
execute many operations.  The operations have to be
on immediate values (not bignums), and the the return
value from a function have to be an immediate value
(a compound term would burden the garbage collector).
This seriously limits how powerful algorithms that can be used.

We wrote one and named it `mwc59` because it has a 59-bit
state, and the most thorough scrambling function returns
a 59-bit value.  There is also a faster, intermediate scrambling
function, that returns a 32-bit value, which is the "digit" size
of the MWC generator.  It is also possible to directly
use the low 16 bits of the state without scrambling.
See section [Implementing a PRNG] for how this generator
was designed and why.

As another gap filler between really fast with low quality,
and full featured, an internal function in `rand` has been exported:
`rand:exsp_next/1`.  This function implements Xoroshiro116+ that exists
within the `rand` plug-in framework as algorithm `exsp`.
It has been exported so it is possible to get good quality without
the plug-in framework overhead, for applications that do not
need any framework features.

See section [Measurement results] for speed comparisons.



Quality
-------

There are many different aspects of a PRNG:s quality.
Here are some.

### Period

`erlang:phash2(erlang:unique_integer(), Range)` has, conceptually,
an infinite period, since the time it will take for it to repeat
is assumed to be longer than the Erlang node will survive.

For the new fast `mwc59` generator the period it is about 2<sup>59</sup>.
For the regular ones in `rand` it is at least 2<sup>116</sup>&nbsp;-&nbsp;1,
which is a huge difference.  It might be possible to consume
2<sup>59</sup> numbers during an Erlang node's lifetime,
but not 2<sup>116</sup>.

There are also generators in `rand` with a period of
2<sup>928</sup>&nbsp;-&nbsp;1 which might seem ridiculously long,
but this facilitates generating very many parallel sub-sequences
guaranteed to not overlap.

In, for example, a physical simulation it is common practice to only
use a fraction of the generator's period, both regarding how many numbers
you generate and on how large range you generate, or it may affect
the simulation for example that specific numbers do not reoccur.
If you have pulled 3 aces from a deck you know there is only one left.

Some applications may be sensitive to the generator period,
while others are not, and this needs to be considered.

### Size

The value size of the new fast `mwc59` generators is 59, 32, or 16 bits,
depending on the scrambling function that is used.
Most of the regular generators in the `rand` module has got
a value size of 58 bits.

If you need numbers in a power of 2 range then you can
simply mask out the low bits:

``` erlang
V = X band ((1 bsl RangeBits) - 1).
```

Or shift down the required number of bits:

``` erlang
V = X bsr (GeneratorBits - RangeBits).
```

This, depending on if the generator is known to have weak high or low bits.

If the range you need is not a power of 2, but still
much smaller than the generator's size you can use `rem`:

``` erlang
V = X rem Range.
```

The rule of thumb is that `Range` should be less than
the square root of the generator's size.  This is much slower
than bit-wise operations, and the operation propagates low bits,
which can be a problem if the generator is known to have weak low bits.

Another way is to use truncated multiplication:

``` erlang
V = (X * Range) bsr GeneratorBits
```

The rule of thumb here is that `Range` should be less than
2<sup>GeneratorBits</sup>.  Also, `X * Range`
should not create a bignum, so not more than 59 bits.
This method propagates high bits, which can be a problem
if the generator is known to have weak high bits.

Other tricks are possible, for example if you need numbers
in the range 0 through 999 you may use bit-wise operations to get
a number 0 through 1023, and if too high re-try, which actually
may be faster on average than using `rem`.  This method is also
completely free from bias in the generated numbers.  The previous
methods have the rules of thumb to get a so small bias
that it becomes hard to notice.

### Spectral score

The spectral score of a generator, measures how much a sequence of numbers
from the generator are unrelated.  A sequence of N numbers are interpreted
as an N-dimensional vector and the spectral score for dimension N is a measure
on how evenly these vectors are distributed in an N-dimensional (hyper)cube.

`os:system_time(microseconds)` simply increments so it should have
a lousy spectral score.

`erlang:phash2(erlang:unique_integer(), Range)` has got unknown
spectral score, since that is not part of the math behind a hash function.
But a hash function is designed to distribute the hash value well
for any input, so one can hope that the statistical
distribution of the numbers is decent and "random" anyway.
Unfortunately this does not seem to hold in [PRNG tests]

All regular PRNG:s in the `rand` module has got good spectral scores.
The new `mwc59` generator mostly, but not in 2 and 3 dimensions,
due to its unbalanced design and power of 2 multiplier.
Scramblers are used to compensate for those flaws.


### PRNG tests

There are test frameworks that tests the statistical properties
of PRNG:s, such as the [TestU01] framework, or [PractRand].

The regular generators in the `rand` module perform well
in such tests, and pass thorough test suites.

Although the `mcg59` generator pass [PractRand] 2 TB
and [TestU01] with its low 16 bits without any scrambling,
its statistical problems show when the test parameters
are tweaked just a little.  To perform well in more cases,
and with more bits, scrambling functions are needed.
Still, the small state space and the flaws of the base generator
makes it hard to pass all tests with flying colors.
With the thorough double Xorshift scrambler it gets very good, though.

`erlang:phash2(N, Range)` over an incrementing sequence does not do well
in [TestU01], which suggests that a hash functions has got different
design criteria from PRNG:s.

However, these kind of tests may be completely irrelevant
for your application.

### Predictability

For some applications, a generated number may have to be even
cryptographically unpredictable, while for others there are
no strict requirements.

There is a grey-zone for "non-critical" applications where for example
a rouge party may be able to affect input data, and if it knows the PRNG
sequence can steer all data to a hash table slot, overload one particular
worker process, or something similar, and in this way attack an application.
And, an application that starts out as "non-critical" may one day
silently have become business critical...

This is an aspect that needs to be considered.



Storing the state
-----------------

If the state of a PRNG can be kept in a loop variable, the cost
can be almost nothing.  But as soon as it has to be stored in
a heap variable it will cost performance due to heap data
allocation, term building, and garbage collection.

In the section [Measurement results] we see that the fastest PRNG
can generate a new state that is also the generated integer
in just under 4 ns.  Unfortunately, just to return both
the value and the new state in a 2-tuple adds roughly 10 ns.

The application state in which the PRNG state must be stored
is often more complex, so the cost for updating it will
probably be even larger.



Seeding
-------

Seeding is related to predictability.  If you can guess
the seed you know the generator output.

The seed is generator dependent and how to create a good
seed usually takes much longer than generating a number.
Sometimes the seed and its predictability is so unimportant
that a constant can be used.   If a generator instance
generates just a few numbers per seeding, then seeding
can be the harder problem.

`erlang:phash2(erlang:unique_integer(), Range)` is pre-seeded,
or rather cannot be seeded, so it has no seeding cost, but can
on the other hand be rather predictable, if it is possible to estimate
how many unique integers that have been generated since node start.

The default seeding in the `rand` module uses a combination
of a hash value of the node name, the system time,
and `erlang:unique_integer()`, to create a seed,
which is hopefully sufficiently unpredictable.

The suggested NIF and BIF solutions would also need
a way to create a good enough seed, where "good enough"
is hard to put a number on.



JIT optimizations
-----------------

The speed of the newly implemented `mwc59` generator
is partly thanks to the recent [type-based optimizations] in the compiler
and the Just-In-Time compiling BEAM code loader.

### With no type-based optimization

This is the Erlang code for the `mwc59` generator:

``` erlang
mwc59(CX) ->
    C = CX band ((1 bsl 32)-1),
    X = CX bsr 32,
    16#7fa6502 * X + C.
```

The code compiles to this Erlang BEAM assembler, (`erlc -S rand.erl`),
using the `no_type_opt` flag to disable type-based optimizations:

``` text
    {gc_bif,'bsr',{f,0},1,[{x,0},{integer,32}],{x,1}}.
    {gc_bif,'band',{f,0},2,[{x,0},{integer,4294967295}],{x,0}}.
    {gc_bif,'*',{f,0},2,[{x,0},{integer,133850370}],{x,0}}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
```

When loaded by the JIT (x86) (`erl +JDdump true`)
the machine code becomes:

```nasm
# i_bsr_ssjd
    mov rsi, qword ptr [rbx]
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L2271
```

Above was a test if `{x,0}` is a small integer and if not
the fallback at `L2271` is called to handle any term.

Then follows the machine code for right shift, Erlang `bsr 32`,
x86 `sar rax, 32`, and a skip over the fallback code:

```nasm
    mov rax, rsi
    sar rax, 32
    or rax, 15
    short jmp L2272
L2271:
    mov eax, 527
    call 140439031217336
L2272:
    mov qword ptr [rbx+8], rax
# line_I
```

Here follows `band` with similar test and fallback code:

```nasm
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 68719476735
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L2273
    and rax, rsi
    short jmp L2274
L2273:
    call 140439031216768
L2274:
    mov qword ptr [rbx], rax
```

Below comes `*` with test, fallback code, and overflow check:

```nasm
# line_I
# i_times_jssd
    mov rsi, qword ptr [rbx]
    mov edx, 2141605935
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L2276
# mul with overflow check, imm RHS
    mov rax, rsi
    mov rcx, 133850370
    and rax, -16
    imul rax, rcx
    short jo L2276
    or rax, 15
    short jmp L2275
L2276:
    call 140439031220000
L2275:
    mov qword ptr [rbx], rax
```

The following is `+` with tests, fallback code, and overflow check:

```nasm
# i_plus_ssjd
    mov rsi, qword ptr [rbx]
    mov rdx, qword ptr [rbx+8]
# are both operands small?
    mov eax, esi
    and eax, edx
    and al, 15
    cmp al, 15
    short jne L2278
# add with overflow check
    mov rax, rsi
    mov rcx, rdx
    and rcx, -16
    add rax, rcx
    short jno L2277
L2278:
    call 140439031219296
L2277:
    mov qword ptr [rbx], rax
```

### With type-based optimization

When the compiler can figure out type information about the arguments
it can emit more efficient code.  One would like to add a guard
that restricts the argument to a 59 bit integer, but unfortunately
the compiler cannot yet make use of such a guard test.

But adding a redundant input bit mask to the Erlang code puts the compiler
on the right track.  This is a kludge, and will only be used
until the compiler has been improved to deduce the same information
from a guard instead.

The Erlang code now has a first redundant mask to 59 bits:

``` erlang
mwc59(CX0) ->
    CX = CX0 band ((1 bsl 59)-1),
    C = CX band ((1 bsl 32)-1),
    X = CX bsr 32,
    16#7fa6502 * X + C.
```

The BEAM assembler then becomes, with the default type-based optimizations
in the compiler the OTP-25.0 release:

``` text
    {gc_bif,'band',{f,0},1,[{x,0},{integer,576460752303423487}],{x,0}}.
    {gc_bif,'bsr',{f,0},1,[{tr,{x,0},{t_integer,{0,576460752303423487}}},
             {integer,32}],{x,1}}.
    {gc_bif,'band',{f,0},2,[{tr,{x,0},{t_integer,{0,576460752303423487}}},
             {integer,4294967295}],{x,0}}.
    {gc_bif,'*',{f,0},2,[{tr,{x,0},{t_integer,{0,4294967295}}},
             {integer,133850370}],{x,0}}.
    {gc_bif,'+',{f,0},2,[{tr,{x,0},{t_integer,{0,572367635452168875}}},
             {tr,{x,1},{t_integer,{0,134217727}}}],{x,0}}.
```

Note that after the initial input `band` operation,
type information `{tr,{x_},{t_integer,Range}}` has been propagated
all the way down.

Now the JIT:ed code becomes noticeably shorter.

The input mask operation knows nothing about the value so it has
the operand test and the fallback to any term code:

```nasm
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 9223372036854775807
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L1816
    and rax, rsi
    short jmp L1817
L1816:
    call 139812177115776
L1817:
    mov qword ptr [rbx], rax
```

For all the following operations, operand tests and fallback code
has been optimized away to become a straight sequence of machine code:

```nasm
# line_I
# i_bsr_ssjd
    mov rsi, qword ptr [rbx]
# skipped test for small left operand because it is always small
    mov rax, rsi
    sar rax, 32
    or rax, 15
L1818:
L1819:
    mov qword ptr [rbx+8], rax
# line_I
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 68719476735
# skipped test for small operands since they are always small
    and rax, rsi
    mov qword ptr [rbx], rax
# line_I
# i_times_jssd
# multiplication without overflow check
    mov rax, qword ptr [rbx]
    mov esi, 2141605935
    and rax, -16
    sar rsi, 4
    imul rax, rsi
    or rax, 15
    mov qword ptr [rbx], rax
# i_plus_ssjd
# add without overflow check
    mov rax, qword ptr [rbx]
    mov rsi, qword ptr [rbx+8]
    and rax, -16
    add rax, rsi
    mov qword ptr [rbx], rax
```

The execution time goes down from 3.7 ns to 3.3 ns which is
10% faster just by avoiding redundant checks and tests,
despite adding a not needed initial input mask operation.

And there is room for improvement.  The values are moved back and forth
to BEAM `{x,_}` registers (`qword ptr [rbx]`) between operations.
Moving back from the `{x,_}` register could be avoided by the JIT
since it is possible to know that the value is in a process register.
Moving out to the `{x,_}` register could be optimized away if the compiler
would emit the information that the value will not be used
from the `{x,_}` register after the operation.



Implementing a PRNG
-------------------

To create a really fast PRNG in Erlang there are some
limitations coming with the language implementation:

* If the generator state is a complex term, that is, a heap term,
  instead of an immediate value, state updates gets much slower.
  Therefore the state should be a max 59-bit integer.
* If an intermediate result creates a bignum, that is,
  overflows 59 bits, arithmetic operations gets much slower,
  so intermediate results must produce values that fit in 59 bits.
* If the generator returns both a generated value
  and a new state in a compound term, then, again,
  updating heap data makes it much slower.  Therefore
  a generator should only return an immediate integer state.
* If the returned state integer cannot be used as a generated number,
  then a separate value function that operates on the state
  can be used.  Two calls, however, double the call overhead.

### LCG and MCG

The first attempt was to try a classical power of 2
Linear Congruential Generator:

``` erlang
X1 = (A * X0 + C) band (P-1)
```

And a Multiplicative Congruential Generator:

``` erlang
X1 = (A * X0) rem P
```

To avoid bignum operations the product `A * X0`
must fit in 59 bits. The classical paper "Tables of
Linear Congruential Generators of Different Sizes and
Good Lattice Structure" by Pierre L'Ecuyer lists two generators
that are 35 bit, that is, an LCG with `P`&nbsp;=&nbsp;2<sup>35</sup>
and an MCG with P being a prime number just below 2<sup>35</sup>.
These were the largest generators to be found for which
the muliplication did not overflow 59 bits.

The speed of the LCG is very good.  The MCG less so since it has
to do an integer division by `rem`, but thanks to `P` being
close to 2<sup>35</sup> that could be optimized so the speed
reached only about 50% slower than the LCG.

The short period and know quirks of a power of 2 LCG unfortunately
showed in [PRNG tests].

They failed miserably.

### MWC

[Sebastiano Vigna] of the University of Milano, who also helped
design our current 58-bit Xorshift family generators,
suggested to use a Multiply With Carry generator instead:

``` erlang
T  = A * X0 + C0,
X1 = T band ((1 bsl Bits)-1),
C0 = T bsr Bits.
```

This generator operates on "digits" of size `Bits`, and if a digit
is half a machine word then the multiplication does not overflow.
Instead of having the state as a digit `X` and a carry `C` these
can be merged to have `T` as the state instead.  We get:

``` erlang
X  = T0 band ((1 bsl Bits)-1),
C  = T0 bsr Bits,
T1 = A * X + C
```

An MWC generator is actually a different form of a MCG generator
with a power of 2 multiplier, so this is an equivalent generator:

``` erlang
T0 = (T1 bsl Bits) rem ((A bsl Bits) - 1)
```

In this form the generator updates the state in the reverse order,
hence `T0` and `T1` are swapped.  The modulus `(A bsl Bits) - 1`
has to be a safe prime number or else the generator
does not have maximum period.

#### The base generator

Because the multiplier (or its multiplicative inverse) is a power of 2,
the MWC generator gets bad [Spectral score] in 3 dimensions,
so using a scrambling function on the state to get a number would
be necessary to improve the quality.

A search for a suitable digit size and multiplier started,
mostly done by using programs that try multipliers for
safe prime numbers, and estimates spectral scores, such as [CPRNG].

When the generator is balanced, that is, the multiplier `A`
has got close to `Bits` bits, the spectral scores are the best,
apart from the known problem in 3 dimensions.  But since a scrambling
function would be needed anyway there was an opportunity to
try to generate a comfortable 32-bit digit using a 27-bit multiplier.
With these sizes the product `A * X0` does not create a bignum,
and with a 32-bit digit it becomes possible to use standard
[PRNG tests] to test the generator during development.

Because of using such slightly unbalanced parameters, unfortunately
the spectral scores for 2 dimensions also gets bad, but the scrambler
could solve that too...

The final generator is:

``` erlang
mwc59(T) ->
    C = T bsr 32,
    X = T band ((1 bsl 32)-1),
    16#7fa6502 * X + C.
```

The 32-bit digits of this base generator do not perform very
well in [PRNG tests], but actually the low 16 bits pass
2 TB in [PractRand] and 1 TB with the bits reversed,
which is surprisingly good.  The problem of bad spectral scores
for 2 and 3 dimensions lie in the higher bits of the MWC digit.

#### Scrambling

The scrambler has to be fast as in use only a few
and fast operations.  For an arithmetic generator like this,
Xorshift is a suitable scrambler.  We looked at single
Xorshift, double Xorshift and double XorRot.  Double XorRot
was slower than double Xorshift but not better,
probably since the generator has got good low bits, so they
need to be shifted up to improve the high bits.
Rotating down high bits to the low is no improvement.

This is a single Xorshift scrambler:

``` erlang
V = T bxor (T bsl Shift)
```

When trying `Shift` constants it showed that with a large
shift constant the generator performed better in [PractRand],
and with a small one it performed better in birthday spacing tests
(such as in [TestU01] BigCrush) and collision tests.
Alas, it was not possible to find a constant good for both.

The choosen single Xorshift constant is `8` that passes
4 TB in [PractRand] and BigCrush in [TestU01] but fails
more thorough birthday spacing tests.  The failures are few,
such as the lowest bit in 8 and 9 dimensions,
and some intermediate bits in 2 and 3 dimensions.
This is something unlikely to affect most applications,
and if using the high bits of the 32 generated,
these imperfections should stay under the rug.

The final scrambler has to avoid bignum operations
and masks the value to 32 bits so it looks like this:

``` erlang
mwc59_value32(T) ->
    V0 = T  band ((1 bsl 32)-1),
    V1 = V0 band ((1 bsl (32-8))-1),
    V0 bxor (V1 bsl 8).
```

A better scrambler would be a double Xorshift that can
have both a small shift and a large shift.
Using the small shift `4` makes the combined generator
do very well in birthday spacings and collision tests,
and following up with a large shift `27` shifts the
whole improved 32-bit MWC digit all the way up
to the top bit of the generator's 59-bit state.
That was the idea, and it turned out work fine.

The double Xorshift scrambler produces a 59-bit
number where the low, the high, reversed low,
reversed high, etc... all perform very well in [PractRand],
[TestU01] BigCrush, and in exhaustive birthday spacing
and collision tests.  It is also not terribly much slower
than the single Xorshift scrambler.

Here is a double Xorshift scrambler 4 then 27:

``` erlang
V1 = T bxor (T bsl 4),
V  = V1 bxor (V1 bsl 27).
```

Which, avoiding bignum operations and producing a 59-bit value,
becomes the final scrambler:

``` erlang
mwc59_value(T) ->
    V0 = T  band ((1 bsl (59-4))),
    V1 = T  bxor (V0 bsl 4),
    V2 = V1 band ((1 bsl (59-27))),
    V1 bxor (V2 bsl 27).
```

Many thanks to [Sebastiano Vigna] that has done most of
(practically all) the parameter searching and extensive testing
of the generator and scramblers, backed by knowledge of what could work.
Using an MWC generator in this particular way is rather uncharted
territory regarding the math, so extensive testing is
the way to trust the quality of the generator.



`rand_SUITE:measure/1`
----------------------

The test suite for the `rand` module &mdash; [`rand_SUITE`],
in the Erlang/OTP source tree, contains a test case [`measure/1`].
This test case is a micro-benchmark of all the algorithms
in the `rand` module, and some more.  It measures the execution
time in nanoseconds per generated number, and presents the
times both absolute and relative to the default algorithm
`exsss` that is considered to be 100%.  See [Measurement Results].

[`measure/1`] is runnable also without a test framework.
As long as `rand_SUITE.beam` is in the code path
`rand_SUITE:measure(N)` will run the benchmark with `N`
as an effort factor.  `N = 1` is the default and
for example `N = 5` gives a slower
and more thorough measurement.

The test case is divided in sections where each first runs
a warm-up with the default generator, then runs an empty
benchmark generator to measure the benchmark overhead,
and after that runs all generators for the specific section.
The benchmark overhead is subtracted from the presented
results after the overhead run.

The warm-up and overhead measurement & compensation are
recent improvements to the `measure/1` test case.
Overhead has also been reduced by in-lining 10 PRNG iterations
per test case loop iteration, which got the overhead down to
one third of without such in-lining, and the overhead is now
about as large as the fastest generator itself, approaching the
function call overhead in Erlang.

The different `measure/1` sections are different use cases such as
"uniform integer half range + 1", etc.  Many of these test the performance
of plug-in framework features.  The test sections that are interesting
for this text are "uniform integer range 10000", "uniform integer 32-bit",
and "uniform integer full range".



Measurement results
-------------------

Here are some selected results from the author's laptop
from running `rand_SUITE:measure(20)`:

The `{mwc59,Tag}` generator is `rand:mwc59/1`, where
`Tag` indicates if the `raw` generator, the `rand:mwc59_value32/1`,
or the `rand:mwc59_value/1` scrambler was used.

The `{exsp,_}` generator is `rand:exsp_next/1` which
is a newly exported internal function that does not use
the plug-in framework.  When called from the plug-in
framework it is called `exsp` below.

`unique_phash2` is `erlang:phash2(erlang:unique_integer(), Range)`.

`system_time` is `os:system_time(microsecond)`.


``` text
RNG uniform integer range 10000 performance
                   exsss:     57.5 ns (warm-up)
                overhead:      3.9 ns      6.8%
                   exsss:     53.7 ns    100.0%
                    exsp:     49.2 ns     91.7%
         {mwc59,raw_mod}:      9.8 ns     18.2%
       {mwc59,value_mod}:     18.8 ns     35.0%
              {exsp,mod}:     22.5 ns     41.9%
          {mwc59,raw_tm}:      3.5 ns      6.5%
      {mwc59,value32_tm}:      8.0 ns     15.0%
        {mwc59,value_tm}:     11.7 ns     21.8%
               {exsp,tm}:     18.1 ns     33.7%
           unique_phash2:     23.6 ns     44.0%
             system_time:     30.7 ns     57.2%
```

The first two are the warm-up and overhead measurements.
The measured overhead is subtracted from all measurements
after the "overhead:" line.  The measured overhead here
is 3.9 ns which matches well that `exsss` measures
3.8 ns more during the warm-up run than after `overhead`.
The warm-up run is, however, a bit unpredictable.

`{_,*mod}` and `system_time` all use `(X rem 10000) + 1`
to achieve the desired range.  The `rem` operation is expensive,
which we will see when comparing with the next section.

`{_,*tm}` use truncated multiplication to achieve the range,
that is `((X * 10000) bsr GeneratorBits) + 1`,
which is much faster than using `rem`.

`erlang:phash2/2` has got a range argument, that performs
the `rem 10000` operation in the BIF, which is fairly cheap,
as we also will see when comparing with the next section.


``` text
RNG uniform integer 32 bit performance
                   exsss:     55.3 ns    100.0%
                    exsp:     51.4 ns     93.0%
        {mwc59,raw_mask}:      2.7 ns      4.9%
         {mwc59,value32}:      6.6 ns     12.0%
     {mwc59,value_shift}:      8.6 ns     15.5%
            {exsp,shift}:     16.6 ns     30.0%
           unique_phash2:     22.1 ns     40.0%
             system_time:     23.5 ns     42.6%
```

In this section, to generate a number in a 32-bit range,
`{mwc59,raw_mask}` and `system_time` use a bit mask
`X band 16#ffffffff`, `{_,*shift}` use `bsr`
to shift out the low bits, and `{mwc59_value32}` has got
the right range in itself.  Here we see that bit operations
are up to 10 ns faster than the `rem` operation in the previous section.
`{mwc59,raw_*}` is more than 3 times faster.

Compared to the truncated multiplication variants in the previous section,
the bit operations here are up to 3 ns faster.

`unique_phash2` still uses BIF coded integer division to achieve
the range, which gives it about the same speed as in the previous section,
but it seems integer division with a power of 2 is a bit faster.


``` text
RNG uniform integer full range performance
                   exsss:     45.1 ns    100.0%
                    exsp:     39.8 ns     88.3%
                   dummy:     25.5 ns     56.6%
             {mwc59,raw}:      3.7 ns      8.3%
         {mwc59,value32}:      6.9 ns     15.2%
           {mwc59,value}:      8.5 ns     18.8%
             {exsp,next}:     16.8 ns     37.2%
       {splitmix64,next}:    331.1 ns    734.3%
           unique_phash2:     21.1 ns     46.8%
                procdict:     75.2 ns    166.7%
        {mwc59,procdict}:     16.6 ns     36.8%
```

In this section no range capping is done.  The raw generator output is used.

Here we have the `dummy` generator, which is an undocumented generator
within the `rand` plug-in framework that only does a minimal state
update and returns a constant.  It is used here to measure
plug-in framework overhead.

The plug-in framework overhead is measured to 25.5 ns that matches
`exsp`&nbsp;-&nbsp;`{exsp,next}`&nbsp;=&nbsp;23.0 ns fairly well,
which is the same algorithm within and without the plug-in framework,
giving another measure of the framework overhead.

`procdict` is the default algorithm `exsss` but makes the plug-in
framework store the generator state in the process dictionary,
which here costs 30 ns.

`{mwc59,procdict}` stores the generator state in the process dictionary,
which here costs 12.9 ns. The state term that is stored is much smaller
than for the plug-in framework.  Compare to `procdict`
in the previous paragraph.



Summary
-------

The [new fast] generator's functions in the `rand` module
fills a niche for speed over quality where the type-based
[JIT optimizations] have elevated the performance.

The combination of high speed and high quality can only
be fulfilled with a [BIF implementation], but we hope that
to be a combination we do not need to address...

[Implementing a PRNG] is tricky business.

Recent improvements in [`rand_SUITE:measure/1`]
highlights what the precious CPU cycles are used for.



[Use `rand` anyway]:        #use-rand-anyway
[BIF implementation]:       #write-a-bif
[Write a BIF]:              #write-a-bif
[Write a NIF]:              #write-a-nif
[new fast]:                 #write-a-simple-prng
[Use the system time]:      #use-the-system-time
[Hash a "unique" value]:    #hash-a-unique-value
[Write a simple PRNG]:      #write-a-simple-prng

[Quality]:                  #quality
[Spectral score]:           #spectral-score
[PRNG tests]:               #prng-tests
[Storing the state]:        #storing-the-state

[JIT optimizations]:        #jit-optimizations
[Implementing a PRNG]:      #implementing-a-prng
[`rand_SUITE:measure/1`]:   #rand_suitemeasure1
[Measurement results]:      #measurement-results

[Looking for a faster RNG]: https://erlangforums.com/t/looking-for-a-faster-rng/
[potatosalad]:              https://github.com/potatosalad/
[experimental BIF]:         https://erlangforums.com/t/looking-for-a-faster-rng/1163/17
[a NIF]:                    https://erlangforums.com/t/looking-for-a-faster-rng/1163/23
[latrules]:                 https://www.iro.umontreal.ca/~lecuyer/myftp/papers/latrules.ps
[TestU01]:                  http://simul.iro.umontreal.ca/testu01/
[PractRand]:                http://pracrand.sourceforge.net/
[type-based optimizations]: https://www.erlang.org/blog/type-based-optimizations-in-the-jit/
[Sebastiano Vigna]:         https://vigna.di.unimi.it/
[CPRNG]:                    https://github.com/vigna/CPRNG/
[`rand_SUITE`]:             https://github.com/erlang/otp/blob/master/lib/stdlib/test/rand_SUITE.erl
[`measure/1`]:              https://github.com/erlang/otp/blob/08f343bed4f75bf345b04b4c1fac7e1026a50ab3/lib/stdlib/test/rand_SUITE.erl#L1064
