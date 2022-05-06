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
that uses non-bignums must be so small in period and size
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
the `rand` module's default algorithm is done in 43 ns.

Generating a number as fast as possible (`rand:mwc59/1`) can be done
in just above 3 ns, but that algorithm has not got good
statistical quality by today's standards.  See section [PRNG tests].
Using a good quality algorithm instead (`rand:exsp_next/1`) takes 16 ns,
if you can store the generator's state in a loop variable.

If you can not store the generator state in a loop variable
there will be more overhead, see section [Storing the state].

Now, if you also need a number in an awkward range, as in not much smaller
than the generator's size, you might have to implement a reject-and-resample
loop, or must concatenate numbers.

The overhead of code that has to implement this much of the features
that the `rand` module already offers will easily approach
its 26 ns overhead, so often there is no point in
re-implementing this wheel...



### Write a BIF

There was been a discussion thread on Erlang Forums:
[Looking for a faster RNG].  Triggered by this Andrew Bennett
[potatosalad] wrote an experimental BIF.

The suggested BIF `erlang:random_integer(Range)` offered
no repeatability, generator state per scheduler, guaranteed
sequence separation between schedulers, and high generator
quality.  All this due to using one of the good generators from
the `rand` module, but now written in C in the BIF.

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

[potatosalad] also wrote a NIF, since we (The Erlang/OTP team)
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
and when generating in a range the application would have to know
in advance the range for the whole cache.

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
 only for ranges much smaller than 32-bit.

Alas this solution does not perform well in [PRNG tests].

See section [Measurement results] for the performance for this solution.



### Write a simple PRNG

To be fast, the implementation of a PRNG algorithm cannot
execute many operations.  The operations have to be
on immediate values (not bignums), and the state as well
as the returned number also have to be immediate values.
This seriously limits how powerful algorithms that can be used.

A Multiply With Carry generator is one of the classical PRNG:s
and is a special form of a Multiplicative Congruential Generator.
It is a well researched PRNG, and can be implemented
like this (in Erlang):
``` erlang
    C   = CX0 bsr B,
    X   = CX0 band ((1 bsl B) - 1)
    CX1 = A * X + C
```
For this to produce a sequence of numbers that appear random,
there are a number of requirements on the constants `A` and `B`.

One criteria for statistical quality is the spectral score,
see section [Spectral score].

To get a decent spectral score `A` cannot be too small, not (much)
smaller than 2<sup>B</sup>.  And `A * X + C` produces a number
as large as `A`&nbsp;*&nbsp;2<sup>B</sup>, which must not become a bignum.

To get a full sequence, that is, to use all numbers in the state range,
there are more restrictions imposed on `A` and `B`, but we will
not dig deeper into this field and instead consult the profession,
in this case Prof. Sebastiano Vigna at the University of Milano
that also helped develop our current 58-bit Xorshift family generators.

After trying many parameters in spectral score programs and
programs for [PRNG tests] we selected the parameters
`A = 16#7fa6502` and `B = 32`, which I named `mwc59`.

It has a 59-bit state space and an MWC "digit" size of 32 bits which
gives the low 32 bits mathematical guarantees about their spectral score.

On the flip side, since an MWC generator corresponds to an MCG
with a power of 2 multiplier, it gets a bad spectral score for
3 dimensions.  This can be fixed with a scrambling function
to convert the state into a generated number.  We selected
two different scrambling functions with increasing quality.

As another gap filler between really fast with low quality,
and full featured, an internal function in `rand` has been exported:
`rand:exsp_next/1`.  This function implements Xoroshiro116+ and exists
in the `rand` plug-in framework as algorithm `exsp`.  It has been
exported so it is possible to get good quality without
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

For the new fast generators the period it is about 2<sup>35</sup>,
and for the good ones in `rand` it is at least 2<sup>116</sup>&nbsp;-&nbsp;1,
which is a huge difference and also much more than what can be consumed
during an Erlang node's lifetime.

There are also generators in `rand` with a period of
2<sup>928</sup>&nbsp;-&nbsp;1 which is ridiculously long,
but facilitates generating very many parallel sub-sequences
that are guaranteed to not overlap.

For example in physical simulation it is common practice to only
use a fraction of the period, both regarding how many numbers
you generate and on how large range you generate, or it may affect
the simulation for example that specific numbers do not reoccur.
If you have pulled 3 aces from a deck you know there is only one left.

Some applications may be sensitive to the generator period,
while others are not, and this needs to be considered.

### Size

The size of the new fast generators is about 35 bits, which is both
the generator's state size (period) and value size.
The good quality generators in the `rand` module has got a size
(value size) of 58 bits.

If you need numbers in a power of 2 range then you can
simply mask out or shift down the required number of bits,
depending on if the generator is known to have weak high or low bits.

If the range you need is not a power of 2, but still
much smaller than the generator's size you can use the `rem`
operator, but it is noticeably slower than a bit-wise operation.

Other tricks are possible, for example if you need numbers
in the range 0 through 999 you may use bit-wise operations to get
a number 0 through 1023, and if too high re-try, which actually
may be faster on average than using `rem`.

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
for any input, so we can assume that the statistical
distribution of the numbers is decent and "random".

All PRNG generators discussed here have got good spectral scores.

### PRNG tests

There are test frameworks that tests the statistical properties
of PRNG:s, such as the [TestU01] framework, or [PractRand].

The good quality generators in the `rand` module perform well
in such tests, and pass thorough test suites.

The `mcg59` generator pass [PractRand] 2 TB with its low 16 bits
without any scrambling.  To perform well in [PractRand] and [TestU01]
with more bits the scrambling functions are needed,
and still, the small state space makes it impossible
to pass all tests with flying colors.

`erlang:phash2(N, Range)` over an incrementing sequence does not do well
in [TestU01], which shows that a hash function has got different
design criteria than PRNG:s.

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

This is an aspect that needs to be considered.



Storing the state
-----------------

If the state of a PRNG can be kept in a loop variable, the cost
can be almost nothing.  But as soon as it has to be stored in
a heap variable it will cost performance due to heap data
allocation, term building, and garbage collection.

In the section [Measurement results] we see that the fastest PRNG
can generate a new state that is also the generated integer
in just over 3 ns.  Unfortunately, just to return both
the value and the new state in a 2-tuple adds about 10 ns.

The application state in which the PRNG state must be stored
is often more complex, so the cost for updating it will be even larger.



Seeding
-------

Seeding is related to predictability.  If you can guess
the seed you know the generator output.

The seed is generator dependent and how to create a good
seed usually takes much longer than generating a number.
Sometimes the seed and its predictability is so unimportant
that a constant may be used.   If a generator instance
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
    16#7fa6502 * (CX band ((1 bsl 32)-1)) + (CX bsr 32).
```
which compiles to (Erlang BEAM assembler, `erlc -S rand.erl`),
using the `no_type_opt` flag:
``` erlang
    {gc_bif,'bsr',{f,0},1,[{x,0},{integer,32}],{x,1}}.
    {gc_bif,'band',{f,0},2,[{x,0},{integer,4294967295}],{x,0}}.
    {gc_bif,'*',{f,0},2,[{x,0},{integer,133850370}],{x,0}}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
```
when loaded by the JIT (x86) (`erl +JDdump true`) the machine code becomes:
```
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
```
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
Here is `band` with similar test and fallback code:
```
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
Below comes `*` with test, fallback code,
and overflow check:
```
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
```
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
    16#7fa6502 * (CX band ((1 bsl 32)-1)) + (CX bsr 32).
```
The BEAM assembler now becomes, with the default type-based optimizations
in the compiler on the master branch (in the upcoming OTP-25.0):
``` erlang
    {gc_bif,'band',{f,0},1,[{x,0},{integer,576460752303423487}],{x,0}}.
    {gc_bif,'bsr',
            {f,0},
            1,
            [{tr,{x,0},{t_integer,{0,576460752303423487}}},{integer,32}],
            {x,1}}.
    {gc_bif,'band',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,576460752303423487}}},
             {integer,4294967295}],
            {x,0}}.
    {gc_bif,'*',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,4294967295}}},{integer,133850370}],
            {x,0}}.
    {gc_bif,'+',
            {f,0},
            2,
            [{tr,{x,0},{t_integer,{0,572367635452168875}}},
             {tr,{x,1},{t_integer,{0,134217727}}}],
            {x,0}}.
```
Note that after the initial input `band` operation,
type information `{tr,{x_},{t_integer,Range}}` has been propagated
all the way down.

Now the JIT:ed code becomes noticeably shorter.

The input mask operation knows nothing about the value so it has
the operand test and the fallback:
```
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
```
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
10% faster just by avoiding some more redundant checks and tests,
even though adding a not needed initial input mask operation.

And there is room for improvement.  The values are moved back and forth
to BEAM `{x,_}` registers (`qword ptr [rbx]`) between operations.
Moving back from the `{x,_}` register could be avoided by the JIT
since it could know that the value is in a process register.
Moving out to the `{x,_}` register could be optimized away if the compiler
would emit the information that the value will not be used
from the `{x,_}` register after the operation.



`rand_SUITE:measure/1`
----------------------

The test suite for the `rand` module &mdash; `rand_SUITE`,
in the Erlang/OTP source tree, contains a test case `measure/1`.
This test case is a micro-benchmark of all the algorithms
in the `rand` module, and some more.  It measures the execution
time in nanoseconds per generated number, and presents the
times both absolute and relative to the default algorithm
`exsss` that is considered to be 100%.

`rand_SUITE:measure/1` is runnable also without a test framework.
As long as `rand_SUITE.beam` is in the code path
`rand_SUITE:measure(N)` will run the benchmark with `N`
as an effort factor.  `N = 1` is the default and e.g `N = 5`
gives a slower and more thorough measurement.

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
from running `rand_SUITE:measure(5)`:

The `{mwc59,Tag}` generator is `rand:mwc59/1`, where
`Tag` indicates if the `raw` generator,
the `rand:mwc59_value/1` or the `rand:mwc59_full_value/1`
scrambler was used.

The `{exsp,_}` generator is `rand:exsp_next/1` which
is a newly exported internal function that does not use
the plug-in framework.  When called from the plug-in
framework it is called `exsp` below.

`unique_phash2` is `erlang:phash2(erlang:unique_integer(), Range)`.

`system_time` is `os:system_time(microsecond)`.


```
RNG uniform integer range 10000 performance
                   exsss:     58.0 ns (warm-up)
                overhead:      3.3 ns      5.7%
                   exsss:     54.3 ns    100.0%
                    exsp:     50.7 ns     93.4%
         {mwc59,raw_mod}:     10.6 ns     19.6%
       {mwc59,value_mod}:     15.3 ns     28.2%
        {mwc59,full_mod}:     19.5 ns     35.8%
              {exsp,mod}:     23.2 ns     42.7%
           unique_phash2:     23.8 ns     43.9%
             system_time:     31.5 ns     58.0%
```
The first two are the warm-up and overhead measurements.
The measured overhead is subtracted from all measurements
after the "overhead:" line.  Note that the measured overhead
is 3.3 ns which matches pretty well that `exsss` when adjusted
for overhead got 3.7 ns shorter time than on the warm-up run.

`{mwc59,raw_mod}`, `{mwc59,value_mod}`,  `{mwc59,full_mod}`,
`{exsp,mod}` and `system_time` all use `(X rem 10000) + 1`
to achieve the desired range.
This operation is expensive, which we will see when comparing
with the next section.

`erlang:phash2/2` has got a range argument, that performs
the `rem 10000` operation in the BIF, which is fairly cheap,
as we also will see when comparing with the next section.


```
RNG uniform integer 32 bit performance
                   exsss:     55.5 ns    100.0%
                    exsp:     50.7 ns     91.4%
        {mwc59,raw_mask}:      3.0 ns      5.4%
      {mwc59,value_mask}:      5.5 ns      9.9%
       {mwc59,full_mask}:      7.5 ns     13.5%
            {exsp,shift}:     18.1 ns     32.6%
           unique_phash2:     21.8 ns     39.2%
             system_time:     23.6 ns     42.6%
```
In this section `{mwc59,raw_mask}`, `{mwc59,value_mask}`,
`{mwc59,full_mask}`, `{exsp,shift}`, and `system_time`
use bit operations such as `X band 16#ffffffff` or `X bsr 3`
to achieve the desired range, and now we see that this is
about 5 to 12 ns faster than for the `rem` operation
in the previous section.  `{mwc59,raw_*}` is now 3 times faster.

`unique_phash2` still uses BIF coded integer division to achieve
the range, which gives it about the same speed as in the previous section.


```
RNG uniform integer full range performance
                   exsss:     43.5 ns    100.0%
                    exsp:     41.4 ns     95.3%
                   dummy:     26.1 ns     59.9%
             {mwc59,raw}:      3.3 ns      7.7%
           {mwc59,value}:      6.8 ns     15.6%
      {mwc59,full_value}:      8.6 ns     19.8%
             {exsp,next}:     18.1 ns     41.7%
           unique_phash2:     21.2 ns     48.8%
                procdict:     75.7 ns    174.1%
        {mwc59,procdict}:     15.5 ns     35.7%
```
In this section no range capping is done.  The raw generator output is used.

Here we have the `dummy` generator, which is an undocumented generator
within the `rand` plug-in framework that only does a minimal state
update and returns a constant.  It is used here to measure
plug-in framework overhead.

The plug-in framework overhead is measured to 26.1 ns that matches
`exsp` - `{exsp,next}` = 23.3 ns well, which is the same algorithm within
and without the plug-in framework.

`procdict` is the default algorithm `exsss` but makes the plug-in
framework store the generator state in the process dictionary,
which here costs 32.2 ns.

`{mwc59,procdict}` stores the generator state in the process dictionary,
which here costs 12.2 ns. The state term that is stored is much smaller
than for the plug-in framework.  Compare to the above.



Summing up
----------

The new fast generators functions in the `rand` module
fills a niche for speed over quality where the type-based
JIT optimizations have elevated the performance.

The combination of high speed and high quality can only
be fulfilled with a BIF implementation, but we hope that
to be a combination we do not need to address...

Recent improvements in `rand_suite:measure/1` highlights what
the precious CPU cycles are used for.



[Use `rand` anyway]:        #use-rand-anyway
[Write a BIF]:              #write-a-bif
[Write a NIF]:              #write-a-nif
[Use the system time]:      #use-the-system-time
[Hash a "unique" value]:    #hash-a-unique-value
[Write a simple PRNG]:      #write-a-simple-prng

[Quality]:                  #quality
[Spectral score]:           #spectral-score
[PRNG tests]:               #prng-tests
[Storing the state]:        #storing-the-state
[Measurement results]:      #measurement-results

[Looking for a faster RNG]:
https://erlangforums.com/t/looking-for-a-faster-rng/

[potatosalad]:
https://github.com/potatosalad/

[latrules]:
https://www.iro.umontreal.ca/~lecuyer/myftp/papers/latrules.ps

[TestU01]:
http://simul.iro.umontreal.ca/testu01/

[PractRand]:
http://pracrand.sourceforge.net/

[type-based optimizations]:
https://www.erlang.org/blog/type-based-optimizations-in-the-jit/
