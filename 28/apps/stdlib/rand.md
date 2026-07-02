# `rand`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/rand.erl#L30)

Pseudo random number generation

This module provides Pseudo Random Number Generation and implements
a number of [base generator algorithms](#algorithms).  Most are provided
through a [plug-in framework](#plug-in-framework)
that adds essential features to the base generators.

PRNGs in general, and so the algorithms in this module, are mostly used
for test and simulation.  They are designed for good statistical
quality and high generation speed.

A generator algorithm, for each iteration, takes a state as input
and produces a raw pseudo random number and a new state to be used
for the next iteration.

A particular state always produces the same number and new state.
The initial state is produced from a [seed](`seed/1`).
This makes it possible to repeat for example a simulation with the same
random number sequence, by re-using the same seed.
There are also the functions `export_seed/0` and `export_seed_s/1`
that capture the PRNG state in an `t:export_state/0`,
that can be used to start from a known state.

This property, and others, make the algorithms in this module
unsuitable for cryptographical applications, but in the `m:crypto` module
there are suitable generators, for this module's
[plug-in framework](#plug-in-framework).
See `crypto:rand_seed_s/0` and `crypto:rand_seed_alg_s/1`.

At the end of this module documentation there are some
[niche algorithms](#niche-algorithms) that do not use
this module's normal [plug-in framework](#plug-in-framework).
They are useful for special purposes like fast generation
when quality is not essential, for seeding other generators, and such.

[](){: #plug-in-framework } Plug-in framework
---------------------------------------------

The raw pseudo random numbers produced by the base generators
are only appropriate in some cases such as power of two ranges
less than the generator size, and some have quirks,
for example weak low bits.  Therefore, the Plug-in Framework
implements a common [API](#plug-in-framework-api) for all base generators,
that add essential or useful funcionality:

* Keeping the generator [state](`seed/1`) in the process dictionary.
* Automatic [seeding](`seed/1`).
* Seeding support for [manual seeding](`seed/2`) to avoid common pitfalls.
* Generating [integers](`t:integer/0`) with
  [uniform distribution](`uniform/1`), in *any* range, without bias.
  The range is not limited; it may be larger than
  the base generator's size (but that costs some performance).
* Generating [floating-point numbers](`t:float/0`) with
  [uniform distribution](`uniform/0`).
* Generating [floating-point numbers](`t:float/0`) with
  [normal distribution](`normal/0`), standard normal distribution
  or [specified mean and variance](`normal/2`).
* Generating any number of [bytes](`bytes/1`).
* [Jumping](`jump/1`) the generator ahead, in algorithms that support that.

[](){: #usage }
#### Usage and examples

A generator has to be initialized.  This is done by one of the
`seed/1` or `seed_s/1` functions, which also select which
[algorithm](#algorithms) to use.  The `seed/1` functions
store the generator and state in the process dictionary,
while the `seed_s/1` functions only return the state, which requires
the calling code to handle the state and updates to it.

The seed functions that do not have a `Seed` value as an argument
create an automatic seed that should be unique to the created
generator instance; see `seed_s/1`.

If an automatic seed is not desired, the seed functions that have a
[`Seed`](`t:seed/0`) argument can be used.  The argument has
3 possible formats; see the `t:seed/0` type description.

[Plug-in framework API](#plug-in-framework-api) functions
named with the suffix `_s` take an explicit state as the last argument
and return the new state as the last element in the returned tuple.
The process dictionary is not used.

Sibling functions without that suffix take an implicit state from
and store the new state in the process dictionary, and only return
their "interesting " output value.  If the process dictionary
does not contain a state, [`seed(default)`](`seed/1`)
is implicitly called to create an automatic seed for the
[_default algorithm_](#default-algorithm) as initial state.

#### _Usage_

First initialize a generator by calling one of the [seed](`seed/1`)
functions, which also selects a PRNG algorithm.

Then call a [Plug-in framework API](#plug-in-framework-api) function
either with an explicit state from the seed function
and use the returned new state in the next call,
or call an API function without an explicit state argument
to operate on the state in the process dictionary.

#### _Examples_

```erlang
%% Generate two uniformly distibuted floating point numbers.
%%
%% By not calling a [seed](`seed/1`) function, this uses
%% the generator state and algorithm in the process dictionary.
%% If there is no state there, [`seed(default)`](`seed/1`)
%% is implicitly called first:
%%
1> R0 = rand:uniform(),
   is_float(R0) andalso 0.0 =< R0 andalso R0 < 1.0.
true
2> R1 = rand:uniform(),
   is_float(R1) andalso 0.0 =< R1 andalso R1 < 1.0.
true

%% Generate a uniformly distributed integer in the range 1..4711:
%%
3> K0 = rand:uniform(4711),
   is_integer(K0) andalso 1 =< K0 andalso K0 =< 4711.
true

%% Generate a binary with 16 bytes, uniformly distributed:
%%
4> B0 = rand:bytes(16),
   byte_size(B0) == 16.
true

%% Select and initialize a specified algorithm,
%% with an automatic default seed, then generate
%% a floating point number:
%%
5> rand:seed(exro928ss).
6> R2 = rand:uniform(),
   is_float(R2) andalso 0.0 =< R2 andalso R2 < 1.0.
true

%% Select and initialize a specified algorithm
%% with a specified seed, then generate
%% a floating point number:
%%
7> rand:seed(exro928ss, 123456789).
8> R3 = rand:uniform().
0.48303622772415256

%% Select and initialize a specific algorithm,
%% with an automatic default seed, using the functional API
%% with explicit generator state, then generate
%% two floating point numbers.
%%
9>  S0 = rand:seed_s(exsss).
10> {R4, S1} = rand:uniform_s(S0),
    is_float(R4) andalso 0.0 =< R4 andalso R4 < 1.0.
true
11> {R5, S2} = rand:uniform_s(S1),
    is_float(R5) andalso 0.0 =< R5 andalso R5 < 1.0.
true
%% Repeat the first after seed
12> {R4, _} = rand:uniform_s(S0).

%% Generate a standard normal distribution number
%% using the built-in fast Ziggurat Method:
%%
13> {SND0, S3} = rand:normal_s(S2),
    is_float(SND0).
true

%% Generate a normal distribution number
%% with mean -3 and variance 0.5:
%%
14> {ND0, S4} = rand:normal_s(-3, 0.5, S3),
    is_float(ND0).
true

%% Generate a textbook basic form Box-Muller
%% standard normal distribution number, which has the same
%% distribution as the built-in Ziggurat method above,
%% but is much slower:
%%
15> R6 = rand:uniform_real(),
    is_float(R6) andalso 0.0 < R6 andalso R6 < 1.0.
true
16> R7 = rand:uniform(),
    is_float(R7) andalso 0.0 =< R7 andalso R7 < 1.0.
true
%% R6 cannot be equal to 0.0 so math:log/1 will never fail
17> SND1 = math:sqrt(-2 * math:log(R6)) * math:cos(math:pi() * R7).
```

[](){: #algorithms } Algorithms
-------------------------------

The base generator algorithms implement the
[Xoroshiro and Xorshift algorithms](http://xorshift.di.unimi.it)
by Sebastiano Vigna.  During an iteration they generate an integer
(at least 58-bit) and operate on a state of several integers.
The size of these integers is chosen to not require bignum arithmetic
on 64-bit platforms, which facilitates fast integer operations,
in particular when handled by the JIT VM.

For most algorithms, jump functions are provided for generating
non-overlapping sequences. A jump function perform a calculation
equivalent to a large number of repeated state iterations,
but execute in a time roughly equivalent to one regular iteration
per generator bit.

By using a jump function instead of starting several generators
from different seeds it is assured that the generated sequences
do not overlap.  The alternative of using different seeds
may accidentally start the generators in sequence positions
that are close to each other, but a jump function jumps
to a sequence position very far ahead.

To create numbers with normal distribution the
[Ziggurat Method by Marsaglia and Tsang](http://www.jstatsoft.org/v05/i08)
is used on the output from a base generator.

The following algorithms are provided:

- **`exsss`**, the [_default algorithm_](#default-algorithm)
  *(Since OTP 22.0)*  
  Xorshift116\*\*, 58 bits precision and period of 2^116-1.

  Jump function: equivalent to 2^64 calls.

  This is the Xorshift116 generator combined with the StarStar scrambler from
  the 2018 paper by David Blackman and Sebastiano Vigna:
  [Scrambled Linear Pseudorandom Number Generators](http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf)

  The generator does not use 58-bit rotates so it is faster than the
  Xoroshiro116 generator, and when combined with the StarStar scrambler
  it does not have any weak low bits like `exrop` (Xoroshiro116+).

  Alas, this combination is about 10% slower than `exrop`, but despite that
  it is the [_default algorithm_](#default-algorithm) thanks to
  its statistical qualities.

- **`exro928ss`** *(Since OTP 22.0)*  
  Xoroshiro928\*\*, 58 bits precision and a period of 2^928-1.

  Jump function: equivalent to 2^512 calls.

  This is a 58 bit version of Xoroshiro1024\*\*, from the 2018 paper by
  David Blackman and Sebastiano Vigna:
  [Scrambled Linear Pseudorandom Number Generators](http://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf)
  that on a 64 bit Erlang system executes only about 40% slower than the
  [*default `exsss` algorithm*](#default-algorithm)
  but with much longer period and better statistical properties,
  but on the flip side a larger state.

  Many thanks to Sebastiano Vigna for his help with the 58 bit adaption.

- **`exrop`** *(Since OTP 20.0)*  
  Xoroshiro116+, 58 bits precision and period of 2^116-1.

  Jump function: equivalent to 2^64 calls.

- **`exs1024s`** *(Since OTP 20.0)*  
  Xorshift1024\*, 64 bits precision and a period of 2^1024-1

  Jump function: equivalent to 2^512 calls.

  Since this generator operates on 64-bit integers that are bignums
  on 64 bit platforms, it is much slower than `exro928ss` above.

- **`exsp`** *(Since OTP 20.0)*  
  Xorshift116+, 58 bits precision and period of 2^116-1

  Jump function: equivalent to 2^64 calls.

  This is a corrected version of a previous
  [_default algorithm_](#default-algorithm) (`exsplus`, _deprecated_),
  that was superseded by Xoroshiro116+ (`exrop`).  Since this algorithm
  does not use rotate operations it executes a little (say < 15%) faster
  than `exrop` (that has to do a 58 bit rotate,
  for which there is no native instruction).
  See the [algorithms' homepage](http://xorshift.di.unimi.it).

[](){: #default-algorithm }
#### Default Algorithm

The current _default algorithm_ is
[`exsss` (Xorshift116\*\*)](#algorithms). If a specific algorithm is
required, ensure to always use `seed/1` to initialize the state.

Which algorithm that is the default may change between Erlang/OTP releases,
and is selected to be one with high speed, small state and "good enough"
statistical properties.  So to ensure that the same sequence is reproduced
on a later Erlang/OTP release, use a `seed/2` or `seed_s/2` to select
both a specific algorithm and the seed value.

#### Old Algorithms

Undocumented (old) algorithms are deprecated but still implemented so old code
relying on them will produce the same pseudo random sequences as before.

> #### Note {: .info }
>
> There were a number of problems in the implementation of
> the now undocumented algorithms, which is why they are deprecated.
> The new algorithms are a bit slower but do not have these problems:
>
> Uniform integer ranges had a skew in the probability distribution
> that was not noticable for small ranges but for large ranges
> less than the generator's precision the probability to produce
> a low number could be twice the probability for a high.
>
> Uniform integer ranges larger than or equal to the generator's precision
> used a floating point fallback that only calculated with 52 bits
> which is smaller than the requested range and therefore all numbers
> in the requested range were not even possible to produce.
>
> Uniform floats had a non-uniform density so small values for example
> less than 0.5 had got smaller intervals decreasing as the generated value
> approached 0.0 although still uniformly distributed for sufficiently large
> subranges. The new algorithms produces uniformly distributed floats
> of the form `N * 2.0^(-53)` hence they are equally spaced.

#### Quality of the Generated Numbers

> #### Note {: .info }
>
> The builtin random number generator algorithms are not cryptographically
> strong. If a cryptographically strong random number generator is needed,
> use for example `crypto:rand_seed_s/0` or `crypto:rand_seed_alg_s/1`.

For all these generators except `exro928ss` and `exsss` the lowest bit(s)
have got a slightly less random behaviour than all other bits.
1 bit for `exrop` (and `exsp`), and 3 bits for `exs1024s`. See for example
this explanation in the
[Xoroshiro128+](http://xoroshiro.di.unimi.it/xoroshiro128plus.c)
generator source code:

> Beside passing BigCrush, this generator passes the PractRand test suite
> up to (and included) 16TB, with the exception of binary rank tests,
> which fail due to the lowest bit being an LFSR; all other bits pass all
> tests. We suggest to use a sign test to extract a random Boolean value.

If this is a problem; to generate a boolean with these algorithms,
use something like this:

```erlang
(rand:uniform(256) > 128) % -> boolean()
```

```erlang
((rand:uniform(256) - 1) bsr 7) % -> 0 | 1
```

For a general range, with `N = 1` for `exrop`, and `N = 3` for `exs1024s`:

```erlang
(((rand:uniform(Range bsl N) - 1) bsr N) + 1)
```

The floating point generating functions in this module waste the lowest bits
when converting from an integer so they avoid this snag.

[](){: #niche-algorithms } Niche algorithms
-------------------------------------------

The [niche algorithms API](#niche-algorithms-api) contains
special purpose algorithms that do not use the
[plug-in framework](#plug-in-framework), mainly for performance reasons.

Since these algorithms lack the plug-in framework support, generating numbers
in a range other than the base generator's range may become a problem.

There are at least four ways to do this, assuming the `Range` is less than
the generator's range:

[](){: #modulo-method }
- **Modulo**  
  To generate a number `V` in the range `0..Range-1`:

  > Generate a number `X`.  
  > Use `V = X rem Range` as your value.

  This method uses `rem`, that is, the remainder of an integer division,
  which is a slow operation.

  Low bits from the generator propagate straight through to
  the generated value, so if the generator has got weaknesses
  in the low bits this method propagates them too.

  If `Range` is not a divisor of the generator range, the generated numbers
  have a bias.  Example:

  Say the generator generates a byte, that is, the generator range
  is `0..255`, and the desired range is `0..99` (`Range = 100`).
  Then there are 3 generator outputs that produce the value `0`,
  these are; `0`, `100` and `200`.
  But there are only 2 generator outputs that produce the value `99`,
  which are; `99` and `199`. So the probability for a value `V` in `0..55`
  is 3/2 times the probability for the other values `56..99`.

  If `Range` is much smaller than the generator range, then this bias
  gets hard to detect. The rule of thumb is that if `Range` is smaller
  than the square root of the generator range, the bias is small enough.
  Example:

  A byte generator when `Range = 20`. There are 12 (`256 div 20`)
  possibilities to generate the highest numbers and one more to generate
  a number `V < 16` (`256 rem 20`). So the probability is 13/12
  for a low number versus a high. To detect that difference with
  some confidence you would need to generate a lot more numbers
  than the generator range, `256` in this small example.

[](){: #truncated-multiplication-method }
- **Truncated multiplication**  
  To generate a number `V` in the range `0..Range-1`, when you have
  a generator with a power of 2 range (`0..2^Bits-1`):

  > Generate a number `X`.  
  > Use `V = X * Range bsr Bits` as your value.

  If the multiplication `X * Range` creates a bignum
  this method becomes very slow.

  High bits from the generator propagate through to the generated value,
  so if the generator has got weaknesses in the high bits this method
  propagates them too.

  If `Range` is not a divisor of the generator range, the generated numbers
  have a bias, pretty much as for the [Modulo](#modulo-method) method above.

[](){: #shift-or-mask-method }
- **Shift or mask**  
  To generate a number in a power of 2 range (`0..2^RBits-1`),
  when you have a generator with a power of 2 range (`0..2^Bits`):

  > Generate a number `X`.  
  > Use `V = X band ((1 bsl RBits)-1)` or `V = X bsr (Bits-RBits)`
  > as your value.

  Masking with `band` preserves the low bits, and right shifting
  with `bsr` preserves the high, so if the generator has got weaknesses
  in high or low bits; choose the right operator.

  If the generator has got a range that is not a power of 2
  and this method is used anyway, it introduces bias in the same way
  as for the [Modulo](#modulo-method) method above.

[](){: #rejection-method }
- **Rejection**  

  > Generate a number `X`.  
  > If `X` is in the range, use it as your value,
  > otherwise reject it and repeat.

  In theory it is not certain that this method will ever complete,
  but in practice you ensure that the probability of rejection is low.
  Then the probability for yet another iteration decreases exponentially
  so the expected mean number of iterations will often be between 1 and 2.
  Also, since the base generator is a full length generator,
  a value that will break the loop must eventually be generated.

  These methods can be combined, such as using
  the [Modulo](#modulo-method) method and only if the generator value
  would create bias use [Rejection](#rejection-method).
  Or using [Shift or mask](#shift-or-mask-method) to reduce the size
  of a generator value so that
  [Truncated multiplication](#truncated-multiplication-method)
  will not create a bignum.

  The recommended way to generate a floating point number
  (IEEE 745 Double, that has got a 53-bit mantissa) in the range
  `0..1`, that is `0.0 =< V < 1.0` is to generate a 53-bit number `X`
  and then use `V = X * (1.0/((1 bsl 53)))` as your value.
  This will create a value of the form N*2^-53 with equal probability
  for every possible N for the range.

# `alg`
*since OTP 18.0* 

```elixir
-type alg() :: builtin_alg() | atom().
```

# `alg_handler`
*since OTP 18.0* 

```elixir
-type alg_handler() ::
          #{type := alg(),
            bits => non_neg_integer(),
            weak_low_bits => non_neg_integer(),
            max => non_neg_integer(),
            next := fun((alg_state()) -> {non_neg_integer(), alg_state()}),
            uniform => fun((state()) -> {float(), state()}),
            uniform_n => fun((pos_integer(), state()) -> {pos_integer(), state()}),
            jump => fun((state()) -> state())}.
```

# `alg_state`
*since OTP 18.0* 

```elixir
-type alg_state() ::
          exsplus_state() |
          exro928_state() |
          exrop_state() |
          exs1024_state() |
          exs64_state() |
          dummy_state() |
          term().
```

# `builtin_alg`
*since OTP 18.0* 

```elixir
-type builtin_alg() :: exsss | exro928ss | exrop | exs1024s | exsp | exs64 | exsplus | exs1024 | dummy.
```

# `dummy_state`
*since OTP 18.0* 

```elixir
-type dummy_state() :: uint58().
```

Algorithm specific internal state

# `export_state`
*since OTP 18.0* 

```elixir
-type export_state() :: {alg(), alg_state()}.
```

Algorithm-dependent state that can be printed or saved to file.

# `exro928_state`
*since OTP 18.0* 

```elixir
-opaque exro928_state() :: {[uint58()], [uint58()]}.
```

Algorithm specific internal state

# `exrop_state`
*since OTP 18.0* 

```elixir
-opaque exrop_state() :: nonempty_improper_list(uint58(), uint58()).
```

Algorithm specific internal state

# `exs64_state`
*since OTP 18.0* 

```elixir
-opaque exs64_state() :: uint64().
```

Algorithm specific internal state

# `exs1024_state`
*since OTP 18.0* 

```elixir
-opaque exs1024_state() :: {[uint64()], [uint64()]}.
```

Algorithm specific internal state

# `exsplus_state`
*since OTP 18.0* 

```elixir
-opaque exsplus_state() :: nonempty_improper_list(uint58(), uint58()).
```

Algorithm specific internal state

# `mwc59_state`
*since OTP 18.0* 

```elixir
-type mwc59_state() :: 1..133850370 bsl 32 - 1 - 1.
```

`1 .. (16#1ffb072 bsl 29) - 2`

# `seed`
*since OTP 18.0* 

```elixir
-type seed() :: [integer()] | integer() | {integer(), integer(), integer()}.
```

Generator seed value.

A single integer is the easiest to use.  It is set as the initial state
of a [SplitMix64](`splitmix64_next/1`) generator.  The sequential
output values of that generator are then used for setting the actual
generator's internal state, after masking to the proper word size
and avoiding zero values, if necessary.

A list of integers sets the generator's internal state directly, after
algorithm-dependent checks of the value and masking to the proper word size.
The number of integers must be equal to the number of state words
in the generator.  This format would only be needed in special cases.

A traditional 3-tuple of integers is passed through algorithm-dependent
hashing functions to create the generator's initial state.  This format is
inherited from this module's predecessor, the `m:random` module,
where the 3-tuple from `erlang:now/0` (also now deprectated) was often used
for seeding to get some uniqueness.

# `splitmix64_state`
*since OTP 18.0* 

```elixir
-type splitmix64_state() :: uint64().
```

Algorithm specific state

# `state`
*since OTP 18.0* 

```elixir
-type state() :: {alg_handler(), alg_state()}.
```

Algorithm-dependent state.

# `uint58`
*since OTP 18.0* 

```elixir
-type uint58() :: 0..1 bsl 58 - 1.
```

`0 .. (2^58 - 1)`

# `uint64`
*since OTP 18.0* 

```elixir
-type uint64() :: 0..1 bsl 64 - 1.
```

`0 .. (2^64 - 1)`

# `exsp_jump`
*since OTP 25.0* 

```elixir
-spec exsp_jump(AlgState :: exsplus_state()) -> NewAlgState :: exsplus_state().
```

Jump the generator state forward.

Performs a [`State`](`t:state/0`) jump calculation
that is equivalent to a 2^64 state iterations.

Returns the [`NewState`](`t:state/0`).

This feature can be used to create many non-overlapping
random number sequences from one start state.

See the description of jump functions at the top of this module description.

See `exsp_next/1` about why this internal implementation function
has been exposed.

# `exsp_next`
*since OTP 25.0* 

```elixir
-spec exsp_next(AlgState :: exsplus_state()) -> {X :: uint58(), NewAlgState :: exsplus_state()}.
```

Generate an Xorshift116+ random integer and new algorithm state.

From the specified [`AlgState`](`t:exsplus_state/0`),
generates a random 58-bit integer [`X`](`t:uint58/0`)
and a new algorithm state [`NewAlgState`](`t:exsplus_state/0`),
according to the Xorshift116+ algorithm.

This is an API function exposing the internal implementation of the
[`exsp`](#algorithms) algorithm that enables using it without the
overhead of the plug-in framework, which might be useful for time critial
applications. On a typical 64 bit Erlang VM this approach executes
in just above 30% (1/3) of the time for the default algorithm through
this module's normal plug-in framework.

To seed this generator use [`{_, AlgState} = rand:seed_s(exsp)`](`seed_s/1`)
or [`{_, AlgState} = rand:seed_s(exsp, Seed)`](`seed_s/1`)
with a specific [`Seed`](`t:seed/0`).

> #### Note {: .info }
>
> This function offers no help in generating a number on a selected range,
> nor in generating floating point numbers.  It is easy to accidentally
> mess up the statistical properties of this generator or to loose
> the performance advantage when doing either.
> See the recipes at the start of this
> [Niche algorithms API](#niche-algorithms-api) description.
>
> Note also the caveat about weak low bits that this generator suffers from.
>
> The generator is exported in this form primarily for performance reasons.

# `mwc59`
*since OTP 25.0* 

```elixir
-spec mwc59(CX0 :: mwc59_state()) -> CX1 :: mwc59_state().
```

Generate a new MWC59 state.

From the specified generator state [`CX0`](`t:mwc59_state/0`) generate
a new state [`CX1`](`t:mwc59_state/0`), according to a Multiply With Carry
generator, which is an efficient implementation of
a Multiplicative Congruential Generator with a power of 2 multiplier
and a prime modulus.

This generator uses the multiplier `2^32` and the modulus
`16#7fa6502 * 2^32 - 1`, which have been selected, in collaboration with
Sebastiano Vigna, to avoid bignum operations and still get
good statistical quality. It has been named "MWC59" and can be written as:

```erlang
C = CX0 bsr 32
X = CX0 band ((1 bsl 32)-1))
CX1 = 16#7fa6502 * X + C
```

Because the generator uses a multiplier that is a power of 2 it gets
statistical flaws for collision tests and birthday spacings tests
in 2 and 3 dimensions, and these caveats apply even when looking
only at the MWC "digit", that is the low 32 bits (the multiplier)
of the generator state.  The higher bits of the state are worse.

The quality of the output value improves much by using a scrambler,
instead of just taking the low bits.
Function [`mwc59_value32`](`mwc59_value32/1`) is a fast scrambler
that returns a decent 32-bit number. The slightly slower
[`mwc59_value`](`mwc59_value/1`) scrambler returns 59 bits of
very good quality, and [`mwc59_float`](`mwc59_float/1`) returns
a `t:float/0` of very good quality.

The low bits of the base generator are surprisingly good, so the lowest
16 bits actually pass fairly strict PRNG tests, despite the generator's
weaknesses that lie in the high bits of the 32-bit MWC "digit".
It is recommended to use `rem` on the the generator state, or bit mask
extracting the lowest bits to produce numbers in a range 16 bits or less.
See the recipes at the start of this
[Niche algorithms API](#niche-algorithms-api) description.

On a typical 64 bit Erlang VM this generator executes in below 8% (1/13)
of the time for the default algorithm in the
[plug-in framework API](#plug-in-framework-api) of this module.
With the [`mwc59_value32`](`mwc59_value32/1`) scrambler the total time
becomes 16% (1/6), and with [`mwc59_value`](`mwc59_value/1`)
it becomes 20% (1/5) of the time for the default algorithm.
With [`mwc59_float`](`mwc59_float/1`) the total time
is 60% of the time for the default algorithm generating a `t:float/0`.

> #### Note {: .info }
>
> This generator is a niche generator for high speed applications.
> It has a much shorter period than the default generator, which in itself
> is a quality concern, although when used with the value scramblers
> it passes strict PRNG tests.  The generator is much faster than
> `exsp_next/1` but with a bit lower quality and much shorter period.

# `mwc59_float`
*since OTP 25.0* 

```elixir
-spec mwc59_float(CX :: mwc59_state()) -> V :: float().
```

Calculate a scrambled `t:float/0` from a [MWC59 state](`t:mwc59_state/0`).

Returns a value `V ::` `t:float/0` from a generator state `CX`,
in the range `0.0 =< V < 1.0` like for example `uniform_s/1`.

The generator state is scrambled as with
[`mwc59_value/1`](`mwc59_value/1`) before converted to a `t:float/0`.

# `mwc59_seed`
*since OTP 25.0* 

```elixir
-spec mwc59_seed() -> CX :: mwc59_state().
```

Create a [MWC59 generator state](`t:mwc59_state/0`).

Like `mwc59_seed/1` but it hashes the default seed value
of [`seed_s(atom())`](`seed_s/1`).

# `mwc59_seed`
*since OTP 25.0* 

```elixir
-spec mwc59_seed(S :: 0..1 bsl 58 - 1) -> CX :: mwc59_state().
```

Create a [MWC59 generator state](`t:mwc59_state/0`).

Returns a generator state [`CX`](`t:mwc59_state/0`).
The 58-bit seed value `S` is hashed to create the generator state,
to avoid that similar seeds create similar sequences.

# `mwc59_value32`
*since OTP 25.0* 

```elixir
-spec mwc59_value32(CX :: mwc59_state()) -> V :: 0..1 bsl 32 - 1.
```

Calculate a 32-bit scrambled value from a [MWC59 state](`t:mwc59_state/0`).

Returns a 32-bit value [`V`](`t:integer/0`) from a generator state `CX`.
The generator state is scrambled using an 8-bit xorshift which masks
the statistical imperfecions of the base generator [`mwc59`](`mwc59/1`)
enough to produce numbers of decent quality. Still some problems
in 2- and 3-dimensional birthday spacing and collision tests show through.

When using this scrambler it is in general better to use the high bits of the
value than the low. The lowest 8 bits are of good quality and are passed
right through from the base generator. They are combined with the next 8
in the xorshift making the low 16 good quality, but in the range
16..31 bits there are weaker bits that should not become high bits
of the generated values.

Therefore it is in general safer to shift out low bits. See the recipes
at the start of this [Niche algorithms API](#niche-algorithms-api)
description.

For a non power of 2 range less than about 16 bits (to not get
too much bias and to avoid bignums) truncated multiplication can be used,
that is: `(Range*V) bsr 32`, which is much faster than using `rem`.

# `mwc59_value`
*since OTP 25.0* 

```elixir
-spec mwc59_value(CX :: mwc59_state()) -> V :: 0..1 bsl 59 - 1.
```

Calculate a 59-bit scrambled value from a [MWC59 state](`t:mwc59_state/0`).

Returns a 59-bit value [`V`](`t:integer/0`) from a generator state `CX`.
The generator state is scrambled using an 4-bit followed by a 27-bit xorshift,
which masks the statistical imperfecions of the [MWC59](`mwc59/1`)
base generator enough that all 59 bits are of very good quality.

Be careful to not accidentaly create a bignum when handling the value `V`.

It is in general general better to use the high bits from this scrambler than
the low. See the recipes at the start of this
[Niche algorithms API](#niche-algorithms-api) description.

For a non power of 2 range less than about 29 bits (to not get
too much bias and to avoid bignums) truncated multiplication can be used,
which is much faster than using `rem`. Example for range 1'000'000'000;
the range is 30 bits, we use 29 bits from the generator,
adding up to 59 bits, which is not a bignum (on a 64-bit VM ):
`(1000000000 * (V bsr (59-29))) bsr 29`.

# `splitmix64_next`
*since OTP 25.0* 

```elixir
-spec splitmix64_next(AlgState :: integer()) -> {X :: uint64(), NewAlgState :: splitmix64_state()}.
```

Generate a SplitMix64 random 64-bit integer and new algorithm state.

From the specified `AlgState` generates a random 64-bit integer
[`X`](`t:uint64/0`) and a new generator state
[`NewAlgState`](`t:splitmix64_state/0`),
according to the SplitMix64 algorithm.

This generator is used internally in the `rand` module for seeding other
generators since it is of a quite different breed which reduces
the probability for creating an accidentally bad seed.

# `bytes`
*since OTP 24.0* 

```elixir
-spec bytes(N :: non_neg_integer()) -> Bytes :: binary().
```

Generate random bytes as a `t:binary()`,
using the state in the process dictionary.

Like `bytes_s/2` but operates on the state stored in
the process dictionary.  Returns the generated [`Bytes`](`t:binary/0`).

# `bytes_s`
*since OTP 24.0* 

```elixir
-spec bytes_s(N :: non_neg_integer(), State :: state()) -> {Bytes :: binary(), NewState :: state()}.
```

Generate random bytes as a `t:binary()`.

For a specified integer `N >= 0`, generates a `t:binary/0`
with that number of random bytes.

The selected algorithm is used to generate as many random numbers
as required to compose the `t:binary/0`.  Returns the generated
[`Bytes`](`t:binary/0`) and a [`NewState`](`t:state/0`).

> ### Note {: .info }
>
> The `m:crypto` module contains a function `crypto:strong_rand_bytes/1`
> that does the same thing, but cryptographically secure.
> It is pretty fast and efficient on modern systems.
>
> This function, however, offers the possibility to reproduce
> a byte sequence by re-using seed, which a cryptographically secure
> function cannot do.
>
> Alas, because this function is based on a PRNG that produces
> random integers, thus has to create bytes from integers,
> it becomes rather slow.
>
> Particularly inefficient and slow is to use
> a [`rand` plug-in generator](#plug-in-framework) from `m:crypto`
> such as `crypto:rand_seed_s/0` to call this function for generating
> bytes.  Since in that case it is not possible to reproduce
> the byte sequence anyway; it is better to use
> `crypto:strong_rand_bytes/1` directly.

# `export_seed`
*since OTP 18.0* 

```elixir
-spec export_seed() -> undefined | export_state().
```

Export the seed value.

Returns the random number state in an external format.
To be used with `seed/1`.

# `export_seed_s`
*since OTP 18.0* 

```elixir
-spec export_seed_s(State :: state()) -> export_state().
```

Export the seed value.

Returns the random number generator state in an external format.
To be used with `seed/1`.

# `jump`
*since OTP 20.0* 

```elixir
-spec jump() -> NewState :: state().
```

Jump the generator state forward.

Like `jump/1` but operates on the state stored in
the process dictionary.  Returns the [`NewState`](`t:state/0`).

# `jump`
*since OTP 20.0* 

```elixir
-spec jump(State :: state()) -> NewState :: state().
```

Jump the generator state forward.

Performs an algorithm specific [`State`](`t:state/0`) jump calculation
that is equivalent to a large number of state iterations.
See this module's [algorithms list](#algorithms).

Returns the [`NewState`](`t:state/0`).

This feature can be used to create many non-overlapping
random number sequences from one start state;
see the start of section [Algorithms](#algorithms)
describing jump functions.

This function raises a `not_implemented` error exception if there is
no jump function implemented for the [`State`](`t:state/0`)'s algorithm.

# `normal`
*since OTP 18.0* 

```elixir
-spec normal() -> X :: float().
```

Generate a random number with standard normal distribution.

Like `normal_s/1` but operates on the state stored in
the process dictionary.  Returns the generated number `X`.

# `normal`
*since OTP 20.0* 

```elixir
-spec normal(Mean :: number(), Variance :: number()) -> X :: float().
```

Generate a random number with specified normal distribution 𝒩 *(μ, σ²)*.

Like `normal_s/3` but operates on the state stored in
the process dictionary.  Returns the generated number `X`.

# `normal_s`
*since OTP 18.0* 

```elixir
-spec normal_s(State :: state()) -> {X :: float(), NewState :: state()}.
```

Generate a random number with standard normal distribution.

From the specified `State`, generates a random number `X ::` `t:float/0`,
with standard normal distribution, that is with mean value `0.0`
and variance `1.0`.

Returns the generated number [`X`](`t:float/0`)
and the [`NewState`](`t:state/0`).

# `normal_s`
*since OTP 20.0* 

```elixir
-spec normal_s(Mean, Variance, State) -> {X :: float(), NewState :: state()}
                  when Mean :: number(), Variance :: number(), State :: state().
```

Generate a random number with specified normal distribution 𝒩 *(μ, σ²)*.

From the specified `State`, generates a random number `X ::` `t:float/0`,
with normal distribution 𝒩 *(μ, σ²)*, that is 𝒩 (Mean, Variance)
where `Variance >= 0.0`.

Returns [`X`](`t:float/0`) and the [`NewState`](`t:state/0`).

# `seed`
*since OTP 18.0* 

```elixir
-spec seed(Alg | State) -> state()
              when Alg :: builtin_alg() | default, State :: state() | export_state().
```

Seed the random number generator and select algorithm.

The same as [`seed_s(Alg_or_State)`](`seed_s/1`),
but also stores the generated state in the process dictionary.

The argument `default` is an alias for the
[_default algorithm_](#default-algorithm)
that has been implemented *(Since OTP 24.0)*.

# `seed`
*since OTP 18.0* 

```elixir
-spec seed(Alg, Seed) -> state() when Alg :: builtin_alg() | default, Seed :: seed().
```

Seed the random number generator and select algorithm.

The same as [`seed_s(Alg, Seed)`](`seed_s/2`),
but also stores the generated state in the process dictionary.

`Alg = default` is an alias for the
[_default algorithm_](#default-algorithm)
that has been implemented *(Since OTP 24.0)*.

# `seed_s`
*since OTP 18.0* 

```elixir
-spec seed_s(Alg | State) -> state()
                when Alg :: builtin_alg() | default, State :: state() | export_state().
```

Seed the random number generator and select algorithm.

With the argument `Alg`, select that algorithm and seed random number
generation with reasonably unpredictable time dependent data
that should be unique to the created generator instance.
It is (for now) based on the node name, the calling `t:pid/0`,
the system time, and a system unique integer.  This set of
fairly unique items may change in the future, if necessary.

`Alg = default` is an alias for the
[_default algorithm_](#default-algorithm)
*(Since OTP 24.0)*.

With the argument `State`, re-creates the state and returns it.
See also `export_seed/0`.

# `seed_s`
*since OTP 18.0* 

```elixir
-spec seed_s(Alg, Seed) -> state() when Alg :: builtin_alg() | default, Seed :: seed().
```

Seed the random number generator and select algorithm.

Creates and returns a generator state for the specified algorithm
from the specified `t:seed/0` integers.

`Alg = default` is an alias for the [_default algorithm_](#default-algorithm)
that has been implemented *since OTP 24.0*.

# `uniform`
*since OTP 18.0* 

```elixir
-spec uniform() -> X :: float().
```

Generate a uniformly distributed random number `0.0 =< X < 1.0`,
using the state in the process dictionary.

Like `uniform_s/1` but operates on the state stored in
the process dictionary.  Returns the generated number `X`.

# `uniform`
*since OTP 18.0* 

```elixir
-spec uniform(N :: pos_integer()) -> X :: pos_integer().
```

Generate a uniformly distributed random integer `1 =< X =< N`,
using the state in the process dictionary.

Like `uniform_s/2` but operates on the state stored in
the process dictionary.  Returns the generated number `X`.

# `uniform_real`
*since OTP 21.0* 

```elixir
-spec uniform_real() -> X :: float().
```

Generate a uniformly distributed random number `0.0 < X < 1.0`,
using the state in the process dictionary.

Like `uniform_real_s/1` but operates on the state stored in
the process dictionary.  Returns the generated number `X`.

See `uniform_real_s/1`.

# `uniform_real_s`
*since OTP 21.0* 

```elixir
-spec uniform_real_s(State :: state()) -> {X :: float(), NewState :: state()}.
```

Generate a uniformly distributed random number `0.0 < X < 1.0`.

From the specified state, generates a random float, uniformly distributed
in the value range `DBL_MIN =< X < 1.0`.

Conceptually, a random real number `R` is generated from the interval
`0.0 =< R < 1.0` and then the closest rounded down nonzero
normalized number in the IEEE 754 Double Precision Format is returned.

> #### Note {: .info }
>
> The generated numbers from this function has got better granularity
> for small numbers than the regular `uniform_s/1` because all bits
> in the mantissa are random. This property, in combination with the fact
> that exactly zero is never returned is useful for algorithms doing
> for example `1.0 / X` or `math:log(X)`.

The concept implicates that the probability to get exactly zero is extremely
low; so low that this function in fact never returns `0.0`.
The smallest number that it *might* return is `DBL_MIN`,
which is `2.0^(-1022)`.  However, the generators in this module
have technical limitations on how many zero words in a row they
*can* return, which limits the number of leading zeros
that *can* be generated, which sets an upper limit for the smallest
generated number, that is still extremely small.

The value range stated at the top of this function description is
technically correct, but `0.0 =< X < 1.0` is a better description
of the generated numbers' statistical distribution.  That this function
never returns exactly `0.0` is impossible to observe.

For all sub ranges `N*2.0^(-53) =< X < (N+1)*2.0^(-53)` where
`0 =< integer(N) < 2.0^53`, the probability to generate a number
in a sub range is the same, very much like the numbers generated by
`uniform_s/1`.

Having to generate extra random bits for occasional small numbers
costs a little performance. This function is about 20% slower
than the regular `uniform_s/1`

# `uniform_s`
*since OTP 18.0* 

```elixir
-spec uniform_s(State :: state()) -> {X :: float(), NewState :: state()}.
```

Generate a uniformly distributed random number `0.0 =< X < 1.0`.

From the specified `State`, generates a random number `X ::` `t:float/0`,
uniformly distributed in the value range `0.0 =< X < 1.0`.
Returns the number `X` and the updated `NewState`.

The generated numbers are of the form `N * 2.0^(-53)`, that is;
equally spaced in the interval.

> #### Warning {: .warning }
>
> This function may return exactly `0.0` which can be fatal for certain
> applications. If that is undesired you can use `(1.0 - rand:uniform())`
> to get the interval `0.0 < X =< 1.0`, or instead use `uniform_real/0`.
>
> If neither endpoint is desired you can achieve the range
> `0.0 < X < 1.0` using test and re-try like this:
>
> ```erlang
> my_uniform() ->
>     case rand:uniform() of
>         X when 0.0 < X -> X;
>         _ -> my_uniform()
>     end.
> ```

# `uniform_s`
*since OTP 18.0* 

```elixir
-spec uniform_s(N :: pos_integer(), State :: state()) -> {X :: pos_integer(), NewState :: state()}.
```

Generate a uniformly distributed random integer `1 =< X =< N`.

From the specified `State`, generates a random number `X ::` `t:integer/0`,
uniformly distributed in the specified range `1 =< X =< N`.
Returns the number `X` and the updated `NewState`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
