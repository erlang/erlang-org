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
known tricks, and and tries to compare these apples and potatoes.



Speed over quality?
-------------------

The Pseudo Random Number Generators implemented in
the `rand` module offers many useful features such as
repeatable sequences, non-biased range generation,
any size (bignum) range, non-overlapping sequences,
generating floats, normal distribution floats, etc.
Many of those features are implemented through
a plug-in framework, whith a performance cost.

The different algorithms offered by the `rand` module are selected
to have excellent statistical quality and to perform well
in serious PRNG tests, (see section [PRNG tests]).

Most of these algorithms are designed for machines with
64-bit arithmetic (unsigned), but in Erlang such integers
become bignums and almost an order of magnitude slower
to handle than immediate integers.

The algorithms that perform well in Erlang are the ones
that have been redesigned to work on 58-bit words.
But still, when executed in Erlang, they are far from
as fast as their C origins.  Achieving good PRNG quality
costs much more in Erlang than in C.  In the section
[Measurement results] we see that the algorithm `exsp`
that boasts sub-ns speed in C needs 17 ns in Erlang.

32-bit Erlang is a sad story in this regard.  The bignum limit
on such an Erlang system is so low that designing a PRNG
that uses non-bignums has to use such a small word size
that the generator becomes too bad to be used.
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
* [Hash an "unique" value]
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
* We have implemented two simple PRNGs to fill the niche of non-critical
  but very fast number generation: `lcg35` and `mcg35`.


### Use `rand` anyway

Is `rand` slow, really?  Well, perhaps not considering what it does.

In the [Measurement results] at the end of this text,
it shows that generating a good quality random number using
the `rand` module's default algorithm is done in 43 ns.

Generating a number as fast as possible (`rand:lcg35/1`) can be done
in just above 3 ns, but that algorithm has got pretty bad
statistical quality by to today's standards.  See section [PRNG tests].
Using a good quality algorithm instead (`rand:exsp_next/1`) takes 16 ns,
if you can store the generator's state in a loop variable.

If you can not store the generator state in a loop variable
there will be more overhead, see section [Storing the state].

Now, if you also need a number on an awkward range, as in not much smaller
than the generator's size, you might have to implement a reject-and-resample
loop, or must concatenate numbers.

The overhead of code that has to implement this much of the features
that the `rand` module already offers will easily approach
it's 26 ns overhead, so often there is no point in
re-implementing this wheel...



### Write a BIF

There was been a discussion thread on Erlang Forums:
[Looking for a faster RNG].  Triggered by this the user "potatosallad"
wrote an experimental BIF.

The suggested BIF `erlang:random_integer(Range)` offered
no repeatability, generator state per scheduler, guaranteed
sequence separatiton between schedulers, and high generator
quality.  All this due to using one of the good generators from
the `rand` module, but now written in C in the BIF.

The performance was comparable to the `mcg35` generator, so a bit slower
than the (fastest) 'lcg53' generator, but with top of the line quality.
See section [Measurement results].

Questions arised regarding maintenance burden, what more to implement, etc.
For example we probably also would need `erlang:random_integer/0`,
`erlang:random_float/0`, and some system info
to get the generator bit size...

A BIF could achieve good performance on a 32-bit system, if it there
would return a 27-bit integer, which became another open question.
Should a BIF generator be platform independent with respect to
generated numbers or with respect to performance?



### Write a NIF

"potatosallad" also wrote a NIF, since we (The Erlang/OTP team)
suggested that it could have good enough performance.

Measurements, however, showed that the overhead is significantly larger
than for a BIF.  Although the NIF used the same trick as the BIF to store
the state in thread specific data it ended up with the same
performance as `erlang:phash2(erlang:unique_integer(), Range)`,
which is about 2..3 times slower than the BIF.

As a speed improvement we tried was to have the NIF generate
a list of numbers, and use that list as a cache in Erlang.
The performance with such a cache was as fast as the BIF,
but introduced problems such as that you would have to decide
on a cache size, the application would have to keep the cache on the heap,
and when generating on a range the application would have to know
in advance the range for the whole cache.

A NIF could like a BIF also achieve good performance on a 32-bit system,
with the same open question - platform independent numbers or performance?



### Use the system time

One suggested trick is to use `os:system_time(microseconds)` to get
a number.  The trick has some pecularities:
* When called repeatedly you might get the same number several times.
* The resolution is system dependent, so on some systems you get
  the same number even more several times.
* Time can jump backwards and repeat in some cases.
* Historically it has been a bottleneck, especially on virtualized
  platforms.  Getting the OS time is harder then expected.

See sectom [Measurement results] for the performance for this "solution".



### Hash an "unique" value

The best combination would most certainly be
`erlang:phash2(erlang:unique_integer(), Range)` or
`erlang:phash2(erlang:unique_integer())` which is slightly faster.

`erlang:unique_integer/0` is designed to return a unique integer
with a very small overhead.  It is hard to find a better candidate
for an integer to hash.

`erlang:phash2/1,2` is the current generic hash function for Erlang terms.
It has a default return size well suited for 32-bit Erlang systems,
and it has a `Range` argument.  The range capping is done with a simple
`rem` in C (`%`) which is much faster than in Erlang.  This works good
 only for ranges much smaller than 32-bit.

See section [Measurement results] for the performance for this solution.



### Write a simple PRNG

To be fast, the implementation of a PRNG algorithm cannot
execute many operations operations.  The operations have to be
on immediate values (not bignums), and the state as well
as the returned number also have to be immediate values.
This seriously limits how powerful algorithms that can be used.

A Linear Congruential Generator is one of the most researched
and oldest PRNG:s.  It is implemented like this (in Erlang):
``` erlang
    X1 = (A * X0 + C) rem P
```
For this to produce a sequence of numbers that appear random,
there are a number of requirements on the constants A, C and P.

One criteria for statistical quality is the spectral score,
see section [Spectral score].

To get a decent spectral score `A` cannot be too small, not (much)
smaller than the square root of `P`.  And `(A * X0 + C)` produces a number
as large as `A * P`, which must not become a bignum.

The `rem` operation is expensive, but if `P` is a power of 2,
we can use `band (P-1)` instead, which is much faster.

To get a full sequence, that is, to use all numbers in the state range,
there are more restrictions imposed on `A`, `P` and `C`, but we will
not dig deeper into this field and instead use established research.

There is a classical (1999) paper by Pierre L'Ecuyer:
"Tables of linear congruential generators of different sizes
and good lattice structure".  Lattice structure is the property
that the spectral score measures.

From that paper I selected two generators that avoids bignums:
``` erlang
    X1 = (15319397 * X0 + 15366142135) band ((1 bsl 35)-1)
```
which I named `lcg35`, and
``` erlang
    X1 = (185852 * X0) rem ((1 bsl 35)-31)
```
which I named `mcg35`

`lcg35` is a power of 2 generator with an odd addition constant
and a state interval of `0 =< X < 2^35`.

`mcg35` is a prime modulus multiplicative generator (Lehmer generator)
with a state interval of `1 =< X < 2^35-31`.  Since the modulus,
the prime number `2^35 - 31`, is close to a power of 2, the expensive
`rem` operation can be optimized, but the generator is still
slightly slower than `lcg35`.

Because state is not a power of 2, generating for example a 32-bit number
with this generator gets tricky if you want it non-biased.  Simply masking
off the high 3 bits gives 8 possibilities for all values but the top 31
and zero, that only get 7 possibilities.  So the probability for
a zero or one of the top 31 values is 7/8 of the probability for all others.

Note that it is unwise to use more than say half the bits
corresponding to a generator's period anyway. See section [Quality],
about that and other differences between these generators.

As gap filler between really fast with low quality, and full featured,
an internal function in `rand` has been exported: `rand:exsp_next/1`.
This function implements Xoroshiro116+ and is used from the
`rand` plug-in framework as algorithm `exsp`.  It has been
exported so it is possible to get good quality without
the plug-in framework overhead, for applications that do not
need any framework features.

See section [Measurement results] for speed comparisions.



Quality
-------

There are many different aspects of a PRNG:s quality.
Here are some.

### Period

`erlang:phash2(erlang:unique_integer(), Range)` has, conceptually,
an infinite period, since the time it will take for it to repeat
is assumed to be longer than the Erlang node will survive.

For the new fast generators the period it is about `2^35`,
and for the good ones in `rand` it is at least `2^116 - 1`,
which is a huge difference and also much more than what can be consumed
during an Erlang node's lifetime.

There are also generators in `rand` with a period of
`2^928 -1` which is ridiculously long, but facilitates generating
very many parallel sub-sequences that are guaranteed to not overlap.

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
(value size) of 58 bits..

If you need numbers on a power of two range then you can
simply mask out or shift down the required number of bits,
depending on if the generator is known to have weak high or low bits.

If the range you need is not a power of two, but still
much smaller than the generator's size you can use the `rem`
operator, but it is noticably slower than a bitwise operation.

Other tricks are possible, for example if you need numbers
on the range 0..999 you may use bitwise operations to get
a number 0..1023, and if too high re-try, which actually
may be faster on average than using `rem`.

### Spectral score

The specral score of a generator, measures how a much a sequence of numbers
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
of PRNG:s, such as the TestU01 framework, or PractRand.

The good quality generators in the `rand` module perform well
in such tests, and pass even their most thorough.

When testing the `lcg35` and `mct35` generators in TestU01
it quickly becomes obvious that a good spectral score is not
the whole picture.  They do not perform well at all.  Partly
because of their short period and small size.  TestU01 measures
32-bit random numbers and trying to get good test results with
just a 35-bit generator is kind of futile.

There is a known property of the family of generators that `lcg35`
belongs to.  The lowest bit alternates.  The next to lowest has a period
of 4, and so on.  This is detected immediately in PRNG test programs.

`erlang:phash2(N, Range)` over an incrementing sequence also does not do well
in TestU01, which shows that a hash function has got different
design criterias than PRNG:s.

However, these kind of tests may be completely irrelevant
for your application.

### Predictability

For some applications, a generated number may have to be even
cryptographically unpredictable, while for others there are
no strict requirements.

There is a greyzone for "non-critical" applications where for example
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

The speed of the newly implemented `lcg53` and `mcg53` algorithms
is much thanks to the recent [type-based optimizations] in the compiler
and the Just-In-Time compliling .BEAM code loader.

### Without type-based optimization

This is the Erlang code for the `lcg35` generator:
``` erlang
lcg35(X0) ->
    (15319397 * X0 + 15366142135) band ((1 bsl 35)-1).
```
which compiles to (Erlang .BEAM assembler, erlc -S rand.erl):
``` erlang
    {gc_bif,'*',{f,0},1,[{x,0},{integer,15319397}],{x,0}}.
    {gc_bif,'+',{f,0},1,[{tr,{x,0},number},{integer,15366142135}],{x,0}}.
    {gc_bif,'band',{f,0},1,[{tr,{x,0},number},{integer,34359738367}],{x,0}}.
```
when loaded by the JIT (x86) (erl +JDdump true) the machine code becomes:
```
# i_test_yield
    lea rdx, qword ptr [lcg35/1+24]
    dec r14
    jle L423
# i_times_jssd
    mov rsi, qword ptr [rbx]
    mov edx, 245110367
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L1891
```
Above was a test for if X0 is a small integer.
Below is a multiplication that has to check for overflow
```
# mul with overflow check, imm RHS
    mov rax, rsi
    mov rcx, 15319397
    and rax, -16
    imul rax, rcx
    short jo L1891
    or rax, 15
    short jmp L1890
```
If X0 was not a small integer the following fallback code is used
that handles multiplication of any term.
```
L1891:
    call 140408504026912
L1890:
    mov qword ptr [rbx], rax
```
The following addition knows that the argument is a number,
so it can use a simplified test for if the argument
is a small integer and then it has a fallback for other terms
which could be a bignum or a float.
```
# i_increment_SWd
    mov rsi, qword ptr [rbx]
    mov rdx, 245858274160
# simplified test for small operand since it is a number
    mov rax, rsi
    test al, 1
    short je L1892
    add rax, rdx
    short jno L1893
L1892:
    call 140408504025560
L1893:
    mov qword ptr [rbx], rax
```
Here is a mask operation to 35 bits that also
knows that the argument is a number.
```
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 549755813887
# simplified test for small operand since it is a number
    test esi, 1
    short je L1894
    and rax, rsi
    short jmp L1895
L1894:
    call 140408504023680
L1895:
    mov qword ptr [rbx], rax
# return
    dec r14
    jl L424
    ret
```

### With type-based optimization

When the compiler can figure out type information about the arguments
it can emit more effective code.  One would like to add a guard
that restricts the argument to a 58 bit integer, but unfortunately
the compiler cannot yet make use of such a guard test.

But adding a redundant input bit mask to the Erlang code puts the compiler
on the right track.  This is a kludge, and will only be used
until the compiler has been improved to deduce the same information
from a guard instead:

Erlang code; note the first redundant mask to 35 bits:
``` erlang
lcg35(X0) ->
    X = X0 band ((1 bsl 35)-1),
    (15319397 * X + 15366142135) band ((1 bsl 35)-1).
```
The .BEAM assembler now becomes:
``` erlang
    {gc_bif,'band',{f,0},1,[{x,0},{integer,34359738367}],{x,0}}.
    {gc_bif,'*',
            {f,0},
            1,
            [{tr,{x,0},{t_integer,{0,34359738367}}},{integer,15319397}],
            {x,0}}.
    {gc_bif,'+',
            {f,0},
            1,
            [{tr,{x,0},{t_integer,{0,526370472860204699}}},
             {integer,15366142135}],
            {x,0}}.
    {gc_bif,'band',
            {f,0},
            1,
            [{tr,{x,0},{t_integer,{15366142135,526370488226346834}}},
             {integer,34359738367}],
            {x,0}}.
```
Note the `{t_integer,Range}` type information that instead of just declaring
that the argument is a number, now states that it is an integer on
a specific range, and this information is propagated to all operations
following the initial input mask operation.

Now the JIT:ed code becomes noticably shorter:
```
# i_test_yield
    lea rdx, qword ptr [lcg35/1+24]
    dec r14
    jle L423
```
The input mask operation knows nothing about the value so it has all
the checks and fallbacks:
```
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 549755813887
# is the operand small?
    mov edi, esi
    and edi, 15
    cmp edi, 15
    short jne L1890
    and rax, rsi
    short jmp L1891
L1890:
    call 140007608482432
L1891:
    mov qword ptr [rbx], rax
```
All the following operations has optimized away checks and fallback code,
and becomes a straight sequence of machine code:
```
# i_times_jssd
# multiplication without overflow check
    mov rax, qword ptr [rbx]
    mov esi, 245110367
    and rax, -16
    sar rsi, 4
    imul rax, rsi
    or rax, 15
    mov qword ptr [rbx], rax
# i_increment_SWd
# skipped operand and overflow checks
    mov rax, qword ptr [rbx]
    mov rdi, 245858274160
    add rax, rdi
    mov qword ptr [rbx], rax
# i_band_ssjd
    mov rsi, qword ptr [rbx]
    mov rax, 549755813887
# skipped test for small operands since they are always small
    and rax, rsi
    mov qword ptr [rbx], rax
# return
    dec r14
    jl L424
    ret
```
The execution time goes down from 4.0 ns to 3.2 ns which is
20% faster just by avoiding redundant checks and tests.

And there is room for improvement.  The values are moved back and forth
to .BEAM X registers (`qword ptr [rbx]`) between operations.
Moving back from the X register could be avoided by the JIT
since it could know that the value is in a process register.
Moving out to the X register could be optimized away if the compiler
would emit the information that the value will not be used
from the X register after the operation.

The shown type-based optimizations actually gives more improvement
for the `mcg35` generator since it has got more operations on the value
after the initial input mask, but the `lcg35` is shorter to explain.



`rand_SUITE:measure/1`
----------------------

The test suite for the `rand` module - `rand_SUITE`,
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
Overhead has also been reduced by inlining 10 PRNG iterations
per test case loop iteration, which got the overhead down to
one third of without such inlining, and the overhead is now
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

The `mcg35_inline` and `lcg35_inline` generators are
the newly implemented `rand:mcg35/1` and `rand:lcg35/1`.

The `exsp_inline` generator is `rand:exsp_next/1` which
is a newly exported internal function that does not use
the plug-in framework.  When called from the plug-in
framework it is called `exsp` below.

`unique_phash2` is `erlang:phash2(erlang:unique_integer(), Range)`.

`system_time` is `os:system_time(microsecond)`.


```
RNG uniform integer range 10000 performance
                   exsss:     58.2 ns (warm-up)
                overhead:      3.3 ns      5.7%
                   exsss:     54.6 ns    100.0%
                    exsp:     49.7 ns     90.9%
            mcg35_inline:     11.7 ns     21.5%
            lcg35_inline:     10.4 ns     19.0%
             exsp_inline:     22.2 ns     40.6%
           unique_phash2:     24.7 ns     45.1%
             system_time:     32.5 ns     59.5%
```
The first two are the warm-up and overhead measurements.
The measured overhead is subtracted from all measurements
after the "overhead:" line.  Note that the measured overhead
is 3.3 ns which matches pretty well that `exsss` when adjusted
for overhead got 3.6 ns shorter time than on the warm-up run.

`mcg35_inline`, `lcg35_inline`, `exsp_inline` and `system_time`
all use `(X rem 10000) + 1` to achieve the desired range.
This operation is expensive, which we will see when comparing
with the next section.

`erlang:phash2/2` has got a range argument, that performs
the `rem 10000` operation in the BIF, which is cheap,
as we also will see when comparing with the next section.


```
RNG uniform integer 32 bit performance
                   exsss:     54.4 ns    100.0%
                    exsp:     49.4 ns     90.8%
            lcg35_inline:      3.0 ns      5.6%
             exsp_inline:     16.8 ns     30.9%
           unique_phash2:     22.0 ns     40.4%
             system_time:     23.6 ns     43.4%
```
In this section `lcg35_inline`, `exsp_inline` and `system_time`
use bit operations such as `X band 16#ffffffff` or `X bsr 3`
to achieve the desired range, and now we see that this is
about 5..10 ns faster than for the `rem` operation
in the previous section.  `lcg35_inline` is now 3 times faster.

`unique_phash2` still uses BIF coded integer division to achieve
the range, which gives it about the same speed as in the previous section.

The `mcg35_inline` generator does not participate in this section since
range capping with binary operations would give noticable bias
because the generator has got a state range that is 1 .. 2^35-31,
which is not a power of two and has got a very low margin down to 32 bits.


```
RNG uniform integer full range performance
                   exsss:     43.0 ns    100.0%
                    exsp:     40.6 ns     94.4%
                   dummy:     26.0 ns     60.5%
            mcg35_inline:      5.1 ns     12.0%
            lcg35_inline:      3.2 ns      7.4%
             exsp_inline:     16.6 ns     38.7%
           unique_phash2:     18.8 ns     43.8%
                procdict:     70.3 ns    163.6%
          lcg35_procdict:     14.4 ns     33.6%
```
In this section no range capping is done.  The raw generator output is used.

Here we have the `dummy` generator, which is an undocumented generator
within the `rand` plug-in framework that only does a minimal state
update and returns a constant.  It is used here to measure
plug-in framework overhead.

The plug-in framework overhead is measured to 26 ns that matches
`exsp` - `exsp_inline` = 24 ns well, which is the same algorithm within
and without the plug-in framework.

`procdict` is the default algorithm `exsss` but makes the plug-in
framework store the generator state in the process dictionary,
which here costs 27.3 ns.

`lcg35_procdict` is `lcg35_inline` but stores the generator
state in the process dictionary, which here costs 11.2 ns.
The state term that is stored is much smaller than for
the plug-in framework.  Compare to the above.



Summing up
----------

The new functions in the `rand` module for fast and dirty
generators fills a niche for speed over quality where
the type-based JIT optimizations have elevated the performance.

The combination of high speed and high quality can only
be fulfilled with a BIF implementation, but we hope that
to be a combination we do not need to address...

Recent improvements in `rand_suite:measure/1` highlights what
the precisous CPU cycles are used for.



[Use `rand` anyway]:        #use-rand-anyway
[Write a BIF]:              #write-a-bif
[Write a NIF]:              #write-a-nif
[Use the system time]:      #use-the-system-time
[Hash an "unique" value]:   #hash-an-unique-value
[Write a simple PRNG]:      #write-a-simple-prng

[Quality]:                  #quality
[Spectral score]:           #spectral-score
[PRNG tests]:               #prng-tests
[Storing the state]:        #storing-the-state
[Measurement results]:      #measurement-results

[Looking for a faster RNG]:
https://erlangforums.com/t/looking-for-a-faster-rng/

[type-based optimizations]:
https://www.erlang.org/blog/type-based-optimizations-in-the-jit/
