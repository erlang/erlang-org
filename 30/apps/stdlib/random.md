# `random`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/random.erl#L22)

> This module is deprecated. Use the module 'rand' instead.

Pseudo-random number generation.

This module provides a random number generator. The method is attributed to B.A.
Wichmann and I.D. Hill in 'An efficient and portable pseudo-random number
generator', Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

The algorithm is a modification of the version attributed to Richard A. O'Keefe
in the standard Prolog library.

Every time a random number is requested, a state is used to calculate it, and a
new state is produced. The state can either be implicit (kept in the process
dictionary) or be an explicit argument and return value. In this implementation,
the state (the type `t:ran/0`) consists of a tuple of three integers.

> #### Note {: .info }
>
> This random number generator is not cryptographically strong. If a strong
> cryptographic random number generator is needed, use one of functions in the
> `m:crypto` module, for example, [`crypto:strong_rand_bytes/1`](`m:crypto`).

> #### Note {: .info }
>
> The improved `m:rand` module is to be used instead of this module.

## Note

Some of the functions use the process dictionary variable `random_seed` to
remember the current seed.

If a process calls `uniform/0` or `uniform/1` without setting a seed first,
`seed/0` is called automatically.

The implementation changed in Erlang/OTP R15. Upgrading to R15 breaks
applications that expect a specific output for a specified seed. The output is
still deterministic number series, but different compared to releases older than
R15. Seed `{0,0,0}` does, for example, no longer produce a flawed series of only
zeros.

# `ran`
*not exported* 

```erlang
-type ran() :: {integer(), integer(), integer()}.
```

The state.

# `seed0`

> This function is deprecated. random:seed0/0 is deprecated; use the 'rand' module instead.

```erlang
-spec seed0() -> ran().
```

Returns the default state.

# `seed`

> This function is deprecated. random:seed/0 is deprecated; use the 'rand' module instead.

```erlang
-spec seed() -> ran().
```

Seeds random number generation with default (fixed) values in the process
dictionary and returns the old state.

# `seed`

> This function is deprecated. random:seed/1 is deprecated; use the 'rand' module instead.

```erlang
-spec seed(SValue) -> undefined | ran()
              when SValue :: {A1, A2, A3} | integer(), A1 :: integer(), A2 :: integer(), A3 :: integer().
```

[`seed({A1, A2, A3})`](`seed/1`) is equivalent to
[`seed(A1, A2, A3)`](`seed/3`).

# `seed`

> This function is deprecated. random:seed/3 is deprecated; use the 'rand' module instead.

```erlang
-spec seed(A1, A2, A3) -> undefined | ran() when A1 :: integer(), A2 :: integer(), A3 :: integer().
```

Seeds random number generation with integer values in the process dictionary and
returns the old state.

The following is an easy way of obtaining a unique value to seed with:

```erlang
random:seed(erlang:phash2([node()]),
            erlang:monotonic_time(),
            erlang:unique_integer())
```

For details, see `erlang:phash2/1`, `erlang:node/0`, `erlang:monotonic_time/0`,
and `erlang:unique_integer/0`.

# `uniform`

> This function is deprecated. random:uniform/0 is deprecated; use the 'rand' module instead.

```erlang
-spec uniform() -> float().
```

Returns a random float uniformly distributed between `0.0` and `1.0`, updating
the state in the process dictionary.

# `uniform`

> This function is deprecated. random:uniform/1 is deprecated; use the 'rand' module instead.

```erlang
-spec uniform(N) -> pos_integer() when N :: pos_integer().
```

Returns, for a specified integer `N >= 1`, a random integer uniformly
distributed between `1` and `N`, updating the state in the process dictionary.

# `uniform_s`

> This function is deprecated. random:uniform_s/1 is deprecated; use the 'rand' module instead.

```erlang
-spec uniform_s(State0) -> {float(), State1} when State0 :: ran(), State1 :: ran().
```

Returns, for a specified state, a random float uniformly distributed between
`0.0` and `1.0`, and a new state.

# `uniform_s`

> This function is deprecated. random:uniform_s/2 is deprecated; use the 'rand' module instead.

```erlang
-spec uniform_s(N, State0) -> {integer(), State1}
                   when N :: pos_integer(), State0 :: ran(), State1 :: ran().
```

Returns, for a specified integer `N >= 1` and a state, a random integer
uniformly distributed between `1` and `N`, and a new state.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
