# `orddict`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/orddict.erl#L23)

Key-value dictionary as ordered list.

This module provides a `Key`-`Value` dictionary. An `orddict` is a
representation of a dictionary, where a list of pairs is used to store the keys
and values. The list is ordered after the keys in the
[Erlang term order](`e:system:expressions.md#term-comparisons`).

This module provides the same interface as the `m:dict` module but with a
defined representation. One difference is that while `dict` considers two keys
as different if they do not match (`=:=`), this module considers two keys as
different if and only if they do not compare equal (`==`).

### See Also

`m:dict`, `m:gb_trees`, `m:maps`

# `orddict`

```erlang
-type orddict() :: orddict(_, _).
```

# `orddict`

```erlang
-type orddict(Key, Value) :: [{Key, Value}].
```

Dictionary as returned by `new/0`.

# `append`

```erlang
-spec append(Key, Value, Orddict1) -> Orddict2
                when Orddict1 :: orddict(Key, Value), Orddict2 :: orddict(Key, Value).
```

Appends a new `Value` to the current list of values associated with `Key`.

An exception is generated if the initial value associated with `Key`
is not a list of values.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{x, []}]).
[{x,[]}]
2> OrdDict2 = orddict:append(x, 1, OrdDict1).
[{x,[1]}]
3> OrdDict3 = orddict:append(x, 2, OrdDict2).
[{x,[1,2]}]
4> orddict:append(y, 3, OrdDict3).
[{x,[1,2]},{y,[3]}]
```

```erlang
1> OrdDict1 = orddict:from_list([{a, no_list}]).
[{a,no_list}]
2> orddict:append(a, 1, OrdDict1).
** exception error: bad argument
     in operator  ++/2
        called as no_list ++ [1]
     in call from orddict:append/3
```

# `append_list`

```erlang
-spec append_list(Key, ValList, Orddict1) -> Orddict2
                     when
                         ValList :: [Value],
                         Orddict1 :: orddict(Key, Value),
                         Orddict2 :: orddict(Key, Value).
```

Appends a list of values `ValList` to the current list of values associated with
`Key`.

An exception is generated if the initial value associated with `Key`
is not a list of values.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{x, []}]).
[{x,[]}]
2> OrdDict2 = orddict:append_list(x, [1,2], OrdDict1).
[{x,[1,2]}]
3> OrdDict3 = orddict:append_list(y, [3,4], OrdDict2).
[{x,[1,2]},{y,[3,4]}]
```

# `erase`

```erlang
-spec erase(Key, Orddict1) -> Orddict2
               when Orddict1 :: orddict(Key, Value), Orddict2 :: orddict(Key, Value).
```

Removes the item with key `Key` from dictionary `OrdDict1`.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:erase(a, OrdDict1).
[{b,2}]
3> orddict:erase(z, OrdDict1).
[{a,1},{b,2}]
```

# `fetch`

```erlang
-spec fetch(Key, Orddict) -> Value when Orddict :: orddict(Key, Value).
```

Returns the value associated with `Key` in dictionary `Orddict`.

This function assumes that the `Key` is present in the dictionary. An
exception is generated if `Key` is not in the dictionary.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fetch(a, OrdDict1).
1
3> orddict:fetch(missing, OrdDict1).
** exception error: no function clause matching orddict:fetch(missing,[])
```

# `fetch_keys`

```erlang
-spec fetch_keys(Orddict) -> Keys when Orddict :: orddict(Key, Value :: term()), Keys :: [Key].
```

Returns a list of all keys in a dictionary.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fetch_keys(OrdDict1).
[a,b]
```

# `filter`

```erlang
-spec filter(Pred, Orddict1) -> Orddict2
                when
                    Pred :: fun((Key, Value) -> boolean()),
                    Orddict1 :: orddict(Key, Value),
                    Orddict2 :: orddict(Key, Value).
```

Filters keys and values in `Orddict1` using predicate function `Pred`.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:filter(fun (K, V) -> V > 1 end, OrdDict1).
[{b,2}]
```

# `find`

```erlang
-spec find(Key, Orddict) -> {ok, Value} | error when Orddict :: orddict(Key, Value).
```

Searches for a key in a dictionary.

Returns `{ok, Value}`, where `Value` is the value associated with
`Key`, or `error` if the key is not present in the dictionary.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:find(a, OrdDict1).
{ok,1}
3> orddict:find(c, OrdDict1).
error
```

# `fold`

```erlang
-spec fold(Fun, Acc0, Orddict) -> Acc1
              when
                  Fun :: fun((Key, Value, AccIn) -> AccOut),
                  Orddict :: orddict(Key, Value),
                  Acc0 :: Acc,
                  Acc1 :: Acc,
                  AccIn :: Acc,
                  AccOut :: Acc.
```

Calls `Fun` on successive keys and values of `Orddict` together with an extra
argument `Acc` (short for accumulator).

`Fun` must return a new accumulator that is passed to the next
call. `Acc0` is returned if the dictionary is empty.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fold(fun (K, V, Acc) -> [{K, V+100} | Acc] end, [], OrdDict1).
[{b,102},{a,101}]
```

# `from_list`

```erlang
-spec from_list(List) -> Orddict when List :: [{Key, Value}], Orddict :: orddict(Key, Value).
```

Converts the `Key`-`Value` list `List` to a dictionary.

## Examples

```erlang
1> OrdDict = orddict:from_list([{2,b},{1,a},{3,c}]).
[{1,a},{2,b},{3,c}]
```

# `is_empty`
*since OTP 17.0* 

```erlang
-spec is_empty(Orddict) -> boolean() when Orddict :: orddict().
```

Returns `true` if `Orddict` has no elements; otherwise, returns `false`.

## Examples

```erlang
1> orddict:is_empty(orddict:new()).
true
2> orddict:is_empty(orddict:from_list([{a,1}])).
false
```

# `is_key`

```erlang
-spec is_key(Key, Orddict) -> boolean() when Orddict :: orddict(Key, Value :: term()).
```

Returns `true` if `Key` is contained in dictionary `Orddict`;
otherwise, returns `false`.

## Examples

```erlang
1> OrdDict = orddict:from_list([{1,a},{2,b},{3,c}]).
[{1,a},{2,b},{3,c}]
2> orddict:is_key(2, OrdDict).
true
3> orddict:is_key(aa, OrdDict).
false
```

# `map`

```erlang
-spec map(Fun, Orddict1) -> Orddict2
             when
                 Fun :: fun((Key, Value1) -> Value2),
                 Orddict1 :: orddict(Key, Value1),
                 Orddict2 :: orddict(Key, Value2).
```

Calls `Fun` on successive keys and values of `Orddict1` to return a new value
for each key.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:map(fun (_K, V) -> V + 100 end, OrdDict1).
[{a,101},{b,102}]
```

# `merge`

```erlang
-spec merge(Fun, Orddict1, Orddict2) -> Orddict3
               when
                   Fun :: fun((Key, Value1, Value2) -> Value),
                   Orddict1 :: orddict(Key, Value1),
                   Orddict2 :: orddict(Key, Value2),
                   Orddict3 :: orddict(Key, Value).
```

Merges two dictionaries, `Orddict1` and `Orddict2`, to create a new dictionary.

All the `Key`-`Value` pairs from both dictionaries are included in the new
dictionary.

If a key occurs in both dictionaries, `Fun` is called with the key
and both values to return a new value.

[`merge/3`](`merge/3`) can be defined as follows, but is faster:

```erlang
merge(Fun, D1, D2) ->
    fold(fun (K, V1, D) ->
                 update(K, fun (V2) -> Fun(K, V1, V2) end, V1, D)
         end, D2, D1).
```

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> OrdDict2 = orddict:from_list([{b, 7}, {c, 8}]).
[{b,7},{c,8}]
3> orddict:merge(fun (K, V1, V2) -> V1 * V2 end, OrdDict1, OrdDict2).
[{a,1},{b,14},{c,8}]
```

# `new`

```erlang
-spec new() -> orddict(none(), none()).
```

Creates a new dictionary.

## Examples

```erlang
1> orddict:new().
[]
```

# `size`

```erlang
-spec size(Orddict) -> non_neg_integer() when Orddict :: orddict().
```

Returns the number of elements in an `Orddict`.

## Examples

```erlang
1> orddict:size(orddict:new()).
0
2> orddict:size(orddict:from_list([{a,1},{b,2},{c,3}])).
3
```

# `store`

```erlang
-spec store(Key, Value, Orddict1) -> Orddict2
               when Orddict1 :: orddict(Key, Value), Orddict2 :: orddict(Key, Value).
```

Stores a `Key`-`Value` pair in a dictionary.

If the `Key` already exists in `Orddict1`, the associated value is
replaced by `Value`.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:store(a, 99, OrdDict1).
[{a,99},{b,2}]
3> orddict:store(c, 100, OrdDict1).
[{a,1},{b,2},{c,100}]
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(Key, Orddict) -> {Value, Orddict1} | error
              when
                  Orddict :: orddict(Key, Value),
                  Orddict1 :: orddict(Key, Value),
                  Key :: term(),
                  Value :: term().
```

This function returns a value from a dictionary and a new dictionary without
this value.

Returns `error` if the key is not present in the dictionary.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:take(a, OrdDict1).
{1,[{b,2}]}
3> orddict:take(missing, OrdDict1).
error
```

# `to_list`

```erlang
-spec to_list(Orddict) -> List when Orddict :: orddict(Key, Value), List :: [{Key, Value}].
```

Converts a dictionary to a list representation.

## Examples

```erlang
1> OrdDict = orddict:from_list([{2,b},{1,a}]).
[{1,a},{2,b}]
2> orddict:to_list(OrdDict).
[{1,a},{2,b}]
```

# `update`

```erlang
-spec update(Key, Fun, Orddict1) -> Orddict2
                when
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value),
                    Orddict1 :: orddict(Key, Value),
                    Orddict2 :: orddict(Key, Value).
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value.

An exception is generated if `Key` is not present in the dictionary.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:update(a, fun (V) -> V + 100 end, OrdDict1).
[{a,101},{b,2}]
```

# `update`

```erlang
-spec update(Key, Fun, Initial, Orddict1) -> Orddict2
                when
                    Initial :: Value,
                    Fun :: fun((Value1 :: Value) -> Value2 :: Value),
                    Orddict1 :: orddict(Key, Value),
                    Orddict2 :: orddict(Key, Value).
```

Updates a value in a dictionary by calling `Fun` on the value to get a new
value.

If `Key` is not present in the dictionary, `Initial` is stored as the
first value.

For example, [`append/3`](`append/3`) can be defined as follows:

```erlang
append(Key, Val, D) ->
    update(Key, fun (Old) -> Old ++ [Val] end, [Val], D).
```

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> OrdDict2 = orddict:update(c, fun (V) -> V + 100 end, 99, OrdDict1).
[{a,1},{b,2},{c,99}]
3> orddict:update(a, fun (V) -> V + 100 end, 99, OrdDict2).
[{a,101},{b,2},{c,99}]
```

# `update_counter`

```erlang
-spec update_counter(Key, Increment, Orddict1) -> Orddict2
                        when
                            Orddict1 :: orddict(Key, Value),
                            Orddict2 :: orddict(Key, Value),
                            Increment :: number().
```

Adds `Increment` to the value associated with `Key` and stores this value.

If `Key` is not present in the dictionary, `Increment` is stored as
the first value.

## Examples

```erlang
1> OrdDict1 = orddict:from_list([{a,0},{b,0}]).
[{a,0},{b,0}]
2> OrdDict2 = orddict:update_counter(a, 1, OrdDict1).
[{a,1},{b,0}]
3> OrdDict3 = orddict:update_counter(b, 2, OrdDict2).
[{a,1},{b,2}]
4> orddict:update_counter(z, 7, OrdDict3).
[{a,1},{b,2},{z,7}]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
