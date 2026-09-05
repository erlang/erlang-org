# `atomics`
[🔗](https://github.com/erlang/otp/blob/master/erts/preloaded/src/atomics.erl#L25)

Atomic Functions

This module provides a set of functions to do atomic operations towards mutable
atomic variables. The implementation utilizes only atomic hardware instructions
without any software level locking, which makes it very efficient for concurrent
access. The atomics are organized into arrays with the following semantics:

- Atomics are 64 bit integers.
- Atomics can be represented as either signed or unsigned.
- Atomics wrap around at overflow and underflow operations.
- All operations guarantee atomicity. No intermediate results can be seen. The
  result of one mutation can only be the input to one following mutation.
- All atomic operations are mutually ordered. If atomic B is updated _after_
  atomic A, then that is how it will appear to any concurrent readers. No one
  can read the new value of B and then read the old value of A.
- Indexes into atomic arrays are one-based. An atomic array of arity N contains
  N atomics with index from 1 to N.

# `atomics_ref`
*since OTP 21.2* 

```erlang
-opaque atomics_ref()
```

Identifies an atomic array returned from `new/2`.

# `add`
*since OTP 21.2* 

```erlang
-spec add(Ref, Ix, Incr) -> ok when Ref :: atomics_ref(), Ix :: pos_integer(), Incr :: integer().
```

Add `Incr` to atomic.

# `add_get`
*since OTP 21.2* 

```erlang
-spec add_get(Ref, Ix, Incr) -> integer()
                 when Ref :: atomics_ref(), Ix :: pos_integer(), Incr :: integer().
```

Atomically add `Incr` to atomic and return the result.

# `compare_exchange`
*since OTP 21.2* 

```erlang
-spec compare_exchange(Ref, Ix, Expected, Desired) -> ok | integer()
                          when
                              Ref :: atomics_ref(),
                              Ix :: pos_integer(),
                              Expected :: integer(),
                              Desired :: integer().
```

Atomically compare the atomic with `Expected`, and if those are equal, set
atomic to `Desired`.

Return `ok` if `Desired` was written. Return the actual atomic value if
not equal to `Expected`.

# `exchange`
*since OTP 21.2* 

```erlang
-spec exchange(Ref, Ix, Desired) -> integer()
                  when Ref :: atomics_ref(), Ix :: pos_integer(), Desired :: integer().
```

Atomically replace the value of the atomic with `Desired` and return the previous value.

# `get`
*since OTP 21.2* 

```erlang
-spec get(Ref, Ix) -> integer() when Ref :: atomics_ref(), Ix :: pos_integer().
```

Read atomic value.

# `info`
*since OTP 21.2* 

```erlang
-spec info(Ref) -> Info
              when
                  Ref :: atomics_ref(),
                  Info :: #{size := Size, max := Max, min := Min, memory := Memory},
                  Size :: non_neg_integer(),
                  Max :: integer(),
                  Min :: integer(),
                  Memory :: non_neg_integer().
```

Return information about an atomic array in a map.

The map has the following keys:

- **`size`** - The number of atomics in the array.
- **`max`** - The highest possible value an atomic in this array can hold.
- **`min`** - The lowest possible value an atomic in this array can hold.
- **`memory`** - Approximate memory consumption for the array in bytes.

# `new`
*since OTP 21.2* 

```erlang
-spec new(Arity, Opts) -> atomics_ref()
             when Arity :: pos_integer(), Opts :: [Opt], Opt :: {signed, boolean()}.
```

Create a new array of `Arity` number of atomics. All atomics in the array are
initially set to zero.

Argument `Opts` is a list of the following possible options:

- **`{signed, boolean()}`** - Indicate if the elements of the array will be
  treated as signed or unsigned integers. Default is `true` (signed).

  The integer interval for signed atomics are from `-(1 bsl 63)` to
  `(1 bsl 63)-1` and for unsigned atomics from `0` to `(1 bsl 64)-1`.

Atomics are not tied to the current process and are automatically garbage
collected when they are no longer referenced.

# `put`
*since OTP 21.2* 

```erlang
-spec put(Ref, Ix, Value) -> ok when Ref :: atomics_ref(), Ix :: pos_integer(), Value :: integer().
```

Set atomic to `Value`.

# `sub`
*since OTP 21.2* 

```erlang
-spec sub(Ref, Ix, Decr) -> ok when Ref :: atomics_ref(), Ix :: pos_integer(), Decr :: integer().
```

Subtract `Decr` from atomic.

# `sub_get`
*since OTP 21.2* 

```erlang
-spec sub_get(Ref, Ix, Decr) -> integer()
                 when Ref :: atomics_ref(), Ix :: pos_integer(), Decr :: integer().
```

Atomically subtract `Decr` from atomic and return the result.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
