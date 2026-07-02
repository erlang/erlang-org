# `counters`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/erts/preloaded/src/counters.erl#L25)

Counter Functions

This module provides a set of functions to do operations towards shared mutable
counter variables. The implementation does not utilize any software level
locking, which makes it very efficient for concurrent access. The counters are
organized into arrays with the following semantics:

- Counters are 64 bit signed integers.
- Counters wrap around at overflow and underflow operations.
- Counters are initialized to zero.
- Write operations guarantee atomicity. No intermediate results can be seen from
  a single write operation.
- Two types of counter arrays can be created with options `atomics` or
  `write_concurrency`. The `atomics` counters have good allround performance
  with nice consistent semantics while `write_concurrency` counters offers even
  better concurrent write performance at the expense of some potential read
  inconsistencies. See `new/2`.
- Indexes into counter arrays are one-based. A counter array of size N contains
  N counters with index from 1 to N.

# `counters_ref`
*since OTP 21.2* 

```erlang
-opaque counters_ref()
```

Identifies a counter array returned from `new/2`.

# `add`
*since OTP 21.2* 

```erlang
-spec add(Ref, Ix, Incr) -> ok when Ref :: counters_ref(), Ix :: pos_integer(), Incr :: integer().
```

Add `Incr` to counter at index `Ix`.

# `get`
*since OTP 21.2* 

```erlang
-spec get(Ref, Ix) -> integer() when Ref :: counters_ref(), Ix :: pos_integer().
```

Read counter value.

# `info`
*since OTP 21.2* 

```erlang
-spec info(Ref) -> Info
              when
                  Ref :: counters_ref(),
                  Info :: #{size := Size, memory := Memory},
                  Size :: non_neg_integer(),
                  Memory :: non_neg_integer().
```

Return information about a counter array in a map.

The map has the following keys (at least):

- **`size`** - The number of counters in the array.
- **`memory`** - Approximate memory consumption for the array in bytes.

# `new`
*since OTP 21.2* 

```erlang
-spec new(Size, Opts) -> counters_ref()
             when Size :: pos_integer(), Opts :: [Opt], Opt :: atomics | write_concurrency.
```

Create a new counter array of `Size` counters. All counters in the array are
initially set to zero.

Argument `Opts` is a list of the following possible options:

- **`atomics` (Default)** - Counters will be sequentially consistent. If write
  operation A is done sequentially before write operation B, then a concurrent
  reader may see the result of none of them, only A, or both A and B. It cannot
  see the result of only B.

- **`write_concurrency`** - This is an optimization to achieve very efficient
  concurrent [`add`](`add/3`) and [`sub`](`sub/3`) operations at the expense of
  potential read inconsistency and memory consumption per counter.

  Read operations may see sequentially inconsistent results with regard to
  concurrent write operations. Even if write operation A is done sequentially
  before write operation B, a concurrent reader may see any combination of A and
  B, including only B. A read operation is only guaranteed to see all writes
  done sequentially before the read. No writes are ever lost, but will
  eventually all be seen.

  The typical use case for `write_concurrency` is when concurrent calls to
  [`add`](`add/3`) and [`sub`](`sub/3`) toward the same counters are very
  frequent, while calls to [`get` ](`get/2`)and [`put`](`put/3`) are much less
  frequent. The lack of absolute read consistency must also be acceptable.

Counters are not tied to the current process and are automatically garbage
collected when they are no longer referenced.

# `put`
*since OTP 21.2* 

```erlang
-spec put(Ref, Ix, Value) -> ok when Ref :: counters_ref(), Ix :: pos_integer(), Value :: integer().
```

Write `Value` to counter at index `Ix`.

> #### Note {: .info }
>
> Despite its name, the `write_concurrency` optimization does not improve `put`.
> A call to `put` is a relatively heavy operation compared to the very
> lightweight and scalable [`add`](`add/3`) and [`sub`](`sub/3`). The cost for a
> `put` with `write_concurrency` is like a [`get` ](`get/2`)plus a `put` without
> `write_concurrency`.

# `sub`
*since OTP 21.2* 

```erlang
-spec sub(Ref, Ix, Decr) -> ok when Ref :: counters_ref(), Ix :: pos_integer(), Decr :: integer().
```

Subtract `Decr` from counter at index `Ix`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
