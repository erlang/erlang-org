# `dict`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/dict.erl#L39)

A Key-value dictionary.

The representation of a dictionary is not defined.

This module provides the same interface as the `m:orddict` module. One
difference is that while this module considers two keys as different
if they do not match (`=:=`), `orddict` considers two keys as
different if and only if they do not compare equal (`==`).

> #### Note {: .info }
>
> For new code, prefer `m:maps` over this module.

### See Also

`m:gb_trees`, `m:orddict`, `m:maps`

# `dict`

```erlang
-type dict() :: dict(_, _).
```

# `dict`

```erlang
-opaque dict(Key, Value)
```

Dictionary as returned by `new/0`.

# `append`

```erlang
-spec append(Key, Value, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Appends a new `Value` to the current list of values associated with `Key`.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,[]}]).
2> Dict1 = dict:append(a, 1, Dict0).
3> dict:to_list(Dict1).
[{a,[1]}]
4> Dict2 = dict:append(a, 10, Dict1).
5> dict:to_list(Dict2).
[{a,[1,10]}]
```

# `append_list`

```erlang
-spec append_list(Key, ValList, Dict1) -> Dict2
                     when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value), ValList :: [Value].
```

Appends a list of values `ValList` to the current list of values associated with
`Key`.

An exception is generated if the initial value associated with `Key` is
not a list of values.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,[]}]).
2> Dict1 = dict:append_list(a, [1,2,3], Dict0).
3> dict:to_list(Dict1).
[{a,[1,2,3]}]
```

# `erase`

```erlang
-spec erase(Key, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Erases an item with a given key from a dictionary.

## Examples

```erlang
1> Dict0 = dict:from_list([{2,b},{1,a}]).
2> Dict1 = dict:erase(2, Dict0).
3> dict:to_list(Dict1).
[{1,a}]
4> Dict2 = dict:erase(99, Dict0).
```

# `fetch`

```erlang
-spec fetch(Key, Dict) -> Value when Dict :: dict(Key, Value).
```

Returns the value associated with `Key` in dictionary `Dict`.

This function assumes that `Key` is present in dictionary `Dict`, and
an exception is generated if `Key` is not in the dictionary.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> dict:fetch(1, Dict).
a
```

# `fetch_keys`

```erlang
-spec fetch_keys(Dict) -> Keys when Dict :: dict(Key, Value :: term()), Keys :: [Key].
```

Returns a list of all keys in dictionary `Dict`.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> lists:sort(dict:fetch_keys(Dict)).
[1,2]
```

# `filter`

```erlang
-spec filter(Pred, Dict1) -> Dict2
                when
                    Pred :: fun((Key, Value) -> boolean()),
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value).
```

Returns a dictionary of all keys and values in `Dict1` for which
`Pred(Key, Value)` is `true`.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,1},{b,2},{c,3}]).
2> Dict1 = dict:filter(fun(_, V) -> V rem 2 =:= 1 end, Dict0).
3> lists:sort(dict:to_list(Dict1)).
[{a,1},{c,3}]
```

# `find`

```erlang
-spec find(Key, Dict) -> {ok, Value} | error when Dict :: dict(Key, Value).
```

Searches for a key in dictionary `Dict`.

Returns `{ok, Value}`, where `Value` is the value associated with
`Key`, or `error` if the key is not present in the dictionary.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> dict:find(1, Dict).
{ok,a}
3> dict:find(99, Dict).
error
```

# `fold`

```erlang
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
extra argument `Acc` (short for accumulator).

`Fun` must return a new accumulator that is passed to the next
call. `Acc0` is returned if the dictionary is empty.

The evaluation order is undefined.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,1},{b,2},{c,3}]).
2> dict:fold(fun(_, N, Acc) -> Acc + N end, 0, Dict0).
6
```

# `from_list`

```erlang
-spec from_list(List) -> Dict when Dict :: dict(Key, Value), List :: [{Key, Value}].
```

Converts the `Key`-`Value` list `List` to a dictionary.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> lists:sort(dict:to_list(Dict)).
[{1,a},{2,b}]
```

# `is_empty`
*since OTP 17.0* 

```erlang
-spec is_empty(Dict) -> boolean() when Dict :: dict().
```

Returns `true` if dictionary `Dict` has no elements; otherwise,
returns `false`.

## Examples

```erlang
1> Dict = dict:new().
2> dict:is_empty(Dict).
true
```

# `is_key`

```erlang
-spec is_key(Key, Dict) -> boolean() when Dict :: dict(Key, Value :: term()).
```

Tests whether `Key` is contained in dictionary `Dict`.

## Examples

```erlang
1> Dict = dict:from_list([{count,0}]).
2> dict:is_key(count, Dict).
true
3> dict:is_key(table, Dict).
false
```

# `map`

```erlang
-spec map(Fun, Dict1) -> Dict2
             when
                 Fun :: fun((Key, Value1) -> Value2),
                 Dict1 :: dict(Key, Value1),
                 Dict2 :: dict(Key, Value2).
```

Calls `Fun` on successive keys and values of dictionary `Dict1` to return a new
value for each key.

The evaluation order is undefined.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,1},{b,2},{c,3}]).
2> Dict1 = dict:map(fun(_, V) -> 2 * V end, Dict0).
3> lists:sort(dict:to_list(Dict1)).
[{a,2},{b,4},{c,6}]
```

# `merge`

```erlang
-spec merge(Fun, Dict1, Dict2) -> Dict3
               when
                   Fun :: fun((Key, Value1, Value2) -> Value),
                   Dict1 :: dict(Key, Value1),
                   Dict2 :: dict(Key, Value2),
                   Dict3 :: dict(Key, Value).
```

Merges two dictionaries, `Dict1` and `Dict2`, to create a new dictionary.

All the `Key`-`Value` pairs from both dictionaries are included in the
new dictionary. If a key occurs in both dictionaries, `Fun` is called
with the key and both values to return a new value.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,1},{b,2}]).
2> Dict1 = dict:from_list([{b,7},{c,99}]).
3> Dict2 = dict:merge(fun(_, V1, V2) -> V1 + V2 end, Dict0, Dict1).
4> lists:sort(dict:to_list(Dict2)).
[{a,1},{b,9},{c,99}]
```

# `new`

```erlang
-spec new() -> dict().
```

Creates a new dictionary.

## Examples

```erlang
1> dict:new().
```

# `size`

```erlang
-spec size(Dict) -> non_neg_integer() when Dict :: dict().
```

Returns the number of elements in dictionary `Dict`.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> dict:size(Dict).
2
```

# `store`

```erlang
-spec store(Key, Value, Dict1) -> Dict2 when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value).
```

Stores a `Key`-`Value` pair in a dictionary.

If `Key` already exists in `Dict1`, the associated value is replaced
by `Value`.

## Examples

```erlang
1> Dict0 = dict:new().
2> Dict1 = dict:store(name, arne, Dict0).
3> dict:to_list(Dict1).
[{name,arne}]
4> Dict2 = dict:store(name, kalle, Dict1).
5> dict:to_list(Dict2).
[{name,kalle}]
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(Key, Dict) -> {Value, Dict1} | error
              when Dict :: dict(Key, Value), Dict1 :: dict(Key, Value), Key :: term(), Value :: term().
```

This function returns a value from a dictionary and a new dictionary
without this value.

Returns `error` if the key is not present in the dictionary.

## Examples

```erlang
1> Dict0 = dict:from_list([{2,b},{1,a}]).
2> {V, Dict2} = dict:take(1, Dict0).
3> V.
a
4> dict:to_list(Dict2).
[{2,b}]
```

# `to_list`

```erlang
-spec to_list(Dict) -> List when Dict :: dict(Key, Value), List :: [{Key, Value}].
```

Converts a dictionary to a list representation.

## Examples

```erlang
1> Dict = dict:from_list([{2,b},{1,a}]).
2> lists:sort(dict:to_list(Dict)).
[{1,a},{2,b}]
```

# `update`

```erlang
-spec update(Key, Fun, Dict1) -> Dict2
                when
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value),
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value).
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value.

## Examples

```erlang
1> Dict0 = dict:from_list([{a,10}]).
2> Dict1 = dict:update(a, fun(N) -> N + 1 end, Dict0).
3> dict:to_list(Dict1).
[{a,11}]
```

An exception is generated if `Key` is not present in the dictionary.

# `update`

```erlang
-spec update(Key, Fun, Initial, Dict1) -> Dict2
                when
                    Dict1 :: dict(Key, Value),
                    Dict2 :: dict(Key, Value),
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value),
                    Initial :: Value.
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value.

If `Key` is not present in the dictionary, `Initial` is stored as the
first value.

## Examples

```erlang
1> Dict0 = dict:new().
2> Inc = fun(N) -> N + 1 end.
3> Dict1 = dict:update(a, Inc, 0, Dict0).
4> dict:to_list(Dict1).
[{a,0}]
5> Dict2 = dict:update(a, Inc, 0, Dict1).
6> dict:to_list(Dict2).
[{a,1}]
```

# `update_counter`

```erlang
-spec update_counter(Key, Increment, Dict1) -> Dict2
                        when Dict1 :: dict(Key, Value), Dict2 :: dict(Key, Value), Increment :: number().
```

Adds `Increment` to the value associated with `Key` and stores this value.

If `Key` is not present in the dictionary, `Increment` is stored as
the first value.

## Examples

```erlang
1> Dict0 = dict:new().
2> Dict1 = dict:update_counter(a, 10, Dict0).
3> dict:to_list(Dict1).
[{a,10}]
4> Dict2 = dict:update_counter(a, 10, Dict1).
5> dict:to_list(Dict2).
[{a,20}]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
