# `dict`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/dict.erl#L39)

A Key-value dictionary.

The representation of a dictionary is not defined.

This module provides the same interface as the `m:orddict` module. One
difference is that while this module considers two keys as different if they do
not match (`=:=`), `orddict` considers two keys as different if and only if they
do not compare equal (`==`).

## Notes

[](){: #notes }

Functions `append` and `append_list` are included so that keyed values can be
stored in a list _accumulator_, for example:

```erlang
> D0 = dict:new(),
  D1 = dict:store(files, [], D0),
  D2 = dict:append(files, f1, D1),
  D3 = dict:append(files, f2, D2),
  D4 = dict:append(files, f3, D3),
  dict:fetch(files, D4).
[f1,f2,f3]
```

This saves the trouble of first fetching a keyed value, appending a new value to
the list of stored values, and storing the result.

Function `fetch` is to be used if the key is known to be in the dictionary,
otherwise function `find`.

## See Also

`m:gb_trees`, `m:orddict`

# `dict`

```elixir
-type dict() :: dict(_, _).
```

# `dict`

```elixir
-opaque dict(Key, Value) ::
            #dict{segs :: segs(Key, Value),
                  size :: non_neg_integer(),
                  n :: non_neg_integer(),
                  maxn :: non_neg_integer(),
                  bso :: non_neg_integer(),
                  exp_size :: non_neg_integer(),
                  con_size :: non_neg_integer(),
                  empty :: tuple()}.
```

Dictionary as returned by `new/0`.

# `append`

```elixir
-spec append(Key, Value, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Appends a new `Value` to the current list of values associated with `Key`.

See also section [Notes](`m:dict#module-notes`).

# `append_list`

```elixir
-spec append_list(Key, ValList, Dict1) -> Dict2
                     when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value), ValList :: [Value].
```

Appends a list of values `ValList` to the current list of values associated with
`Key`. An exception is generated if the initial value associated with `Key` is
not a list of values.

See also section [Notes](`m:dict#module-notes`).

# `erase`

```elixir
-spec erase(Key, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Erases all items with a given key from a dictionary.

# `fetch`

```elixir
-spec fetch(Key, Dict) -> Value when Dict :: dict(Key, Value).
```

Returns the value associated with `Key` in dictionary `Dict`. This function
assumes that `Key` is present in dictionary `Dict`, and an exception is
generated if `Key` is not in the dictionary.

See also section [Notes](`m:dict#module-notes`).

# `fetch_keys`

```elixir
-spec fetch_keys(Dict) -> Keys when Dict :: dict(Key, Value :: term()), Keys :: [Key].
```

Returns a list of all keys in dictionary `Dict`.

# `filter`

```elixir
-spec filter(Pred, Dict1) -> Dict2
                when
                    Pred :: fun((Key, Value) -> boolean()),
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value).
```

`Dict2` is a dictionary of all keys and values in `Dict1` for which
`Pred(Key, Value)` is `true`.

# `find`

```elixir
-spec find(Key, Dict) -> {ok, Value} | error when Dict :: dict(Key, Value).
```

Searches for a key in dictionary `Dict`. Returns `{ok, Value}`, where `Value` is
the value associated with `Key`, or `error` if the key is not present in the
dictionary.

See also section [Notes](`m:dict#module-notes`).

# `fold`

```elixir
-spec fold(Fun, Acc0, Dict) -> Acc1
              when
                  Fun :: fun((Key, Value, AccIn) -> AccOut),
                  Dict :: dict(Key, Value),
                  Acc0 :: Acc,
                  Acc1 :: Acc,
                  AccIn :: Acc,
                  AccOut :: Acc.
```

Calls `Fun` on successive keys and values of dictionary `Dict` together with an
extra argument `Acc` (short for accumulator). `Fun` must return a new
accumulator that is passed to the next call. `Acc0` is returned if the
dictionary is empty. The evaluation order is undefined.

# `from_list`

```elixir
-spec from_list(List) -> Dict when Dict :: dict(Key, Value), List :: [{Key, Value}].
```

Converts the `Key`-`Value` list `List` to dictionary `Dict`.

# `is_empty`
*since OTP 17.0* 

```elixir
-spec is_empty(Dict) -> boolean() when Dict :: dict().
```

Returns `true` if dictionary `Dict` has no elements, otherwise `false`.

# `is_key`

```elixir
-spec is_key(Key, Dict) -> boolean() when Dict :: dict(Key, Value :: term()).
```

Tests if `Key` is contained in dictionary `Dict`.

# `map`

```elixir
-spec map(Fun, Dict1) -> Dict2
             when
                 Fun :: fun((Key, Value1) -> Value2),
                 Dict1 :: dict(Key, Value1),
                 Dict2 :: dict(Key, Value2).
```

Calls `Fun` on successive keys and values of dictionary `Dict1` to return a new
value for each key. The evaluation order is undefined.

# `merge`

```elixir
-spec merge(Fun, Dict1, Dict2) -> Dict3
               when
                   Fun :: fun((Key, Value1, Value2) -> Value),
                   Dict1 :: dict(Key, Value1),
                   Dict2 :: dict(Key, Value2),
                   Dict3 :: dict(Key, Value).
```

Merges two dictionaries, `Dict1` and `Dict2`, to create a new dictionary. All
the `Key`-`Value` pairs from both dictionaries are included in the new
dictionary. If a key occurs in both dictionaries, `Fun` is called with the key
and both values to return a new value. `merge` can be defined as follows, but is
faster:

```erlang
merge(Fun, D1, D2) ->
    fold(fun (K, V1, D) ->
                 update(K, fun (V2) -> Fun(K, V1, V2) end, V1, D)
         end, D2, D1).
```

# `new`

```elixir
-spec new() -> dict().
```

Creates a new dictionary.

# `size`

```elixir
-spec size(Dict) -> non_neg_integer() when Dict :: dict().
```

Returns the number of elements in dictionary `Dict`.

# `store`

```elixir
-spec store(Key, Value, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Stores a `Key`-`Value` pair in dictionary `Dict2`. If `Key` already exists in
`Dict1`, the associated value is replaced by `Value`.

# `take`
*since OTP 20.0* 

```elixir
-spec take(Key, Dict) -> {Value, Dict1} | error
              when Dict :: dict(Key, Value), Dict1 :: dict(Key, Value), Key :: term(), Value :: term().
```

This function returns value from dictionary and a new dictionary without this
value. Returns `error` if the key is not present in the dictionary.

# `to_list`

```elixir
-spec to_list(Dict) -> List when Dict :: dict(Key, Value), List :: [{Key, Value}].
```

Converts dictionary `Dict` to a list representation.

# `update`

```elixir
-spec update(Key, Fun, Dict1) -> Dict2
                when
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value),
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value).
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value. An exception is generated if `Key` is not present in the dictionary.

# `update`

```elixir
-spec update(Key, Fun, Initial, Dict1) -> Dict2
                when
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value),
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value),
                    Initial :: Value.
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value. If `Key` is not present in the dictionary, `Initial` is stored as the
first value. For example, [`append/3`](`append/3`) can be defined as:

```erlang
append(Key, Val, D) ->
    update(Key, fun (Old) -> Old ++ [Val] end, [Val], D).
```

# `update_counter`

```elixir
-spec update_counter(Key, Increment, Dict1) -> Dict2
                        when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value), Increment :: number().
```

Adds `Increment` to the value associated with `Key` and stores this value. If
`Key` is not present in the dictionary, `Increment` is stored as the first
value.

This can be defined as follows, but is faster:

```erlang
update_counter(Key, Incr, D) ->
    update(Key, fun (Old) -> Old + Incr end, Incr, D).
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
