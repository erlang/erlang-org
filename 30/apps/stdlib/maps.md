# `maps`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/maps.erl#L23)

Maps processing functions.

This module contains functions for maps processing. The Efficiency Guide
contains a chapter that describes
[how to use maps efficiently](`e:system:maps.md`).

# `iterator`
*since OTP 17.0* 

```erlang
-type iterator() :: iterator(term(), term()).
```

# `iterator`
*since OTP 17.0* 

```erlang
-opaque iterator(Key, Value)
```

An iterator representing the associations in a map with keys of type `Key` and
values of type `Value`.

Created using [`maps:iterator/1`](`iterator/1`) or
[`maps:iterator/2`](`iterator/2`).

Consumed by:

- [`maps:next/1`](`next/1`)
- [`maps:filter/2`](`filter/2`)
- [`maps:filtermap/2`](`filtermap/2`)
- [`maps:fold/3`](`fold/3`)
- [`maps:foreach/2`](`foreach/2`)
- [`maps:map/2`](`map/2`)
- [`maps:to_list/1`](`to_list/1`)

# `iterator_order`
*since OTP 17.0* 

```erlang
-type iterator_order() :: iterator_order(term()).
```

# `iterator_order`
*since OTP 17.0* 

```erlang
-type iterator_order(Key) :: undefined | ordered | reversed | fun((A :: Key, B :: Key) -> boolean()).
```

Key-based iterator order option that can be one of `undefined` (default for
[`maps:iterator/1`](`iterator/1`)), `ordered` (sorted in map-key order),
`reversed` (sorted in reverse map-key order), or a custom sorting function.

Used by [`maps:iterator/2`](`iterator/2`).

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of how terms are ordered.

# `filter`
*since OTP 18.0* 

```erlang
-spec filter(Pred, MapOrIter) -> Map
                when
                    Pred :: fun((Key, Value) -> boolean()),
                    MapOrIter :: #{Key => Value} | iterator(Key, Value),
                    Map :: #{Key => Value}.
```

Returns a map `Map` where each key-value pair from `MapOrIter` satisfies
the predicate `Pred(Key, Value)`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Pred(Key, Value)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Pred` is not a function of arity 2.

## Examples

```erlang
1> M = #{a => 2, b => 3, "a" => 1, "b" => 2}.
2> Pred = fun(K, V) -> is_atom(K) andalso V rem 2 =:= 0 end.
3> maps:filter(Pred, M).
#{a => 2}
```

# `filtermap`
*since OTP 24.0* 

```erlang
-spec filtermap(Fun, MapOrIter) -> Map
                   when
                       Fun :: fun((Key, Value1) -> boolean() | {true, Value2}),
                       MapOrIter :: #{Key => Value1} | iterator(Key, Value1),
                       Map :: #{Key => Value1 | Value2}.
```

Calls `Fun(Key, Value1)` on each key-value pair of `MapOrIter` to
update or remove associations from `MapOrIter`.

If `Fun(Key, Value1)` returns `true`, the association is copied to the result
map. If it returns `false`, the association is not copied. If it returns
`{true, NewValue}`, the value for `Key` is replaced with `NewValue` in the
result map.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value1)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K, V) when is_atom(K) -> {true, V*2};
            (_, V) -> V rem 2 =:= 0
   end.
2> Map = #{k1 => 1, "k2" => 2, "k3" => 3}.
3> maps:filtermap(Fun, Map).
#{k1 => 2,"k2" => 2}
```

# `find`
*since OTP 17.0* 

```erlang
-spec find(Key, Map) -> {ok, Value} | error when Map :: #{Key => Value, _ => _}.
```

Returns a tuple `{ok, Value}`, where `Value` is the value associated with `Key`,
or `error` if no value is associated with `Key` in `Map`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{"hi" => 42}.
2> Key = "hi".
3> maps:find(Key, Map).
{ok,42}
```

# `fold`
*since OTP 17.0* 

```erlang
-spec fold(Fun, Init, MapOrIter) -> Acc
              when
                  Fun :: fun((Key, Value, AccIn) -> AccOut),
                  Init :: term(),
                  Acc :: AccOut,
                  AccIn :: Init | AccOut,
                  MapOrIter :: #{Key => Value} | iterator(Key, Value).
```

Calls `Fun(Key, Value, AccIn)` for every `Key` to value `Value` association in
`MapOrIter`, starting with `AccIn` bound to `Acc0`.

The `Fun/3` fun must return a new accumulator, which is passed to the
next call. The function returns the final value of the
accumulator. The initial accumulator value `Init` is returned if the
map is empty.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value, AccIn)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a
map or valid iterator, or with `badarg` if `Fun` is not a function of
arity 3.

## Examples

```erlang
1> Fun = fun(K, V, AccIn) -> AccIn + V end.
2> Map = #{k1 => 1, k2 => 2, k3 => 3}.
3> maps:fold(Fun, 0, Map).
6
```

# `foreach`
*since OTP 24.0* 

```erlang
-spec foreach(Fun, MapOrIter) -> ok
                 when
                     Fun :: fun((Key, Value) -> term()),
                     MapOrIter :: #{Key => Value} | iterator(Key, Value).
```

Calls `Fun(Key, Value)` for every `Key` to `Value` association in
`MapOrIter`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K, V) -> self() ! {K,V} end.
2> Map = #{p => 1, q => 2,x => 10, y => 20, z => 30}.
3> maps:foreach(Fun, maps:iterator(Map, ordered)).
ok
4> [receive X -> X end || _ <- [1,2,3,4,5]].
[{p,1},{q,2},{x,10},{y,20},{z,30}]
```

# `from_keys`
*since OTP 24.0* 

```erlang
-spec from_keys(Keys, Value) -> Map when Keys :: list(), Value :: term(), Map :: map().
```

Takes a list of keys and a value and builds a map where all keys are
associated with the same value.

## Examples

```erlang
1> Keys = ["a", "b", "c"].
2> maps:from_keys(Keys, ok).
#{"a" => ok,"b" => ok,"c" => ok}
```

# `from_list`
*since OTP 17.0* 

```erlang
-spec from_list(List) -> Map when List :: [{Key, Value}], Key :: term(), Value :: term(), Map :: map().
```

Takes a list of key-value tuples and builds a map.

If the same key appears more than once, the last (rightmost) value is
used, and previous values are ignored.

## Examples

```erlang
1> List = [{"a",ignored},{1337,"value two"},{42,value_three},{"a",1}].
2> maps:from_list(List).
#{42 => value_three,1337 => "value two","a" => 1}
```

# `get`
*since OTP 17.0* 

```erlang
-spec get(Key, Map) -> Value when Key :: term(), Map :: map(), Value :: term().
```

Returns value `Value` associated with `Key` if `Map` contains `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

## Examples

```erlang
1> Key = 1337.
2> Map = #{42 => value_two,1337 => "value one","a" => 1}.
3> maps:get(Key, Map).
"value one"
```

# `get`
*since OTP 17.1* 

```erlang
-spec get(Key, Map, Default) -> Value | Default when Map :: #{Key => Value, _ => _}.
```

Returns the value associated with key `Key` in `Map`, or `Default` if
`Key` is not present in the map.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{key1 => val1, key2 => val2}.
#{key1 => val1,key2 => val2}
2> maps:get(key1, Map, "Default value").
val1
3> maps:get(key3, Map, "Default value").
"Default value"
```

# `groups_from_list`
*since OTP 25.0* 

```erlang
-spec groups_from_list(KeyFun, List) -> GroupsMap
                          when
                              KeyFun :: fun((Elem) -> Key),
                              GroupsMap :: #{Key => Group},
                              Key :: term(),
                              List :: [Elem],
                              Group :: [Elem],
                              Elem :: term().
```

Partitions the given `List` into a map of groups.

The result is a map where each key is given by `KeyFun` and each value is a list
of elements from the given `List` for which `KeyFun` returned the same key.

The order of elements within each group list is preserved from the original
list.

## Examples

```erlang
1> EvenOdd = fun(X) when X rem 2 =:= 0 -> even;
                (_) -> odd
             end.
2> maps:groups_from_list(EvenOdd, [1, 2, 3]).
#{even => [2], odd => [1, 3]}
3> maps:groups_from_list(fun length/1, ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["ant", "cat"], 5 => ["dingo"], 7 => ["buffalo"]}
```

# `groups_from_list`
*since OTP 25.0* 

```erlang
-spec groups_from_list(KeyFun, ValueFun, List) -> GroupsMap
                          when
                              KeyFun :: fun((Elem) -> Key),
                              ValueFun :: fun((Elem) -> Value),
                              GroupsMap :: #{Key := Group},
                              Key :: term(),
                              Value :: term(),
                              List :: [Elem],
                              Group :: [Value],
                              Elem :: term().
```

Partitions the given `List` into a map of groups.

The result is a map where each key is given by `KeyFun` and each value is a list
of elements from the given `List`, mapped via `ValueFun`, for which `KeyFun`
returned the same key.

The order of elements within each group list is preserved from the original
list.

## Examples

```erlang
1> EvenOdd = fun(X) -> case X rem 2 of 0 -> even; 1 -> odd end end.
2> Square = fun(X) -> X * X end.
3> maps:groups_from_list(EvenOdd, Square, [1, 2, 3]).
#{even => [4], odd => [1, 9]}
4> maps:groups_from_list(
    fun length/1,
    fun lists:reverse/1,
    ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["tna", "tac"],5 => ["ognid"],7 => ["olaffub"]}
```

# `intersect`
*since OTP 24.0* 

```erlang
-spec intersect(Map1, Map2) -> Map3
                   when Map1 :: #{Key => term()}, Map2 :: #{term() => Value2}, Map3 :: #{Key => Value2}.
```

Computes the intersection of maps `Map1` and `Map2`, producing a
single map `Map3`.

If a key exists in both maps, the value in `Map1` is superseded by the
value in `Map2`. Keys existing in only one of the maps are discarded
along with their values.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:intersect(Map1, Map2).
#{a => 1}
```

# `intersect_with`
*since OTP 24.0* 

```erlang
-spec intersect_with(Combiner, Map1, Map2) -> Map3
                        when
                            Map1 :: #{Key => Value1},
                            Map2 :: #{term() => Value2},
                            Combiner :: fun((Key, Value1, Value2) -> CombineResult),
                            Map3 :: #{Key => CombineResult}.
```

Computes the intersection of maps `Map1` and `Map2`, producing a
single map `Map3`, where values having the same key are combined using
the `Combiner` fun.

When `Combiner` is applied, the key that exists in both maps is the
first parameter, the value from `Map1` is the second parameter, and
the value from `Map2` is the third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:intersect_with(fun(_Key, Val1, Val2) -> {Val1, Val2} end, Map1, Map2).
#{a => {"one",1}}
```

# `is_key`
*since OTP 17.0* 

```erlang
-spec is_key(Key, Map) -> boolean() when Key :: term(), Map :: map().
```

Returns `true` if map `Map` contains `Key`; otherwise, returns `false`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{"42" => value}.
#{"42" => value}
2> maps:is_key("42", Map).
true
3> maps:is_key(value, Map).
false
```

# `iterator`
*since OTP 21.0* 

```erlang
-spec iterator(Map) -> Iterator when Map :: #{Key => Value}, Iterator :: iterator(Key, Value).
```

Returns a map iterator `Iterator` that can be used by [`maps:next/1`](`next/1`)
to traverse the key-value associations in a map.

The order of iteration is undefined. When iterating over a map, the
memory usage is guaranteed to be bounded no matter the size of the
map.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> M = #{ "foo" => 1, "bar" => 2 }.
#{"foo" => 1,"bar" => 2}
2> I = maps:iterator(M).
3> {K1, V1, I2} = maps:next(I), {K1, V1}.
{"bar",2}
4> {K2, V2, I3} = maps:next(I2),{K2, V2}.
{"foo",1}
5> maps:next(I3).
none
```

# `iterator`
*since OTP 26.0* 

```erlang
-spec iterator(Map, Order) -> Iterator
                  when
                      Map :: #{Key => Value},
                      Order :: iterator_order(Key),
                      Iterator :: iterator(Key, Value).
```

Returns a map iterator `Iterator` that can be used by [`maps:next/1`](`next/1`)
to traverse the key-value associations in a map sorted by key using the given
`Order`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or
with a `badarg` exception if `Order` is invalid.

## Examples

Ordered iterator:

```erlang
1> M = #{a => 1, b => 2}.
2> OrdI = maps:iterator(M, ordered).
3> {K1, V1, OrdI2} = maps:next(OrdI), {K1, V1}.
{a,1}
4> {K2, V2, OrdI3} = maps:next(OrdI2),{K2, V2}.
{b,2}
5> maps:next(OrdI3).
none
```

Iterator ordered in reverse:

```erlang
1> M = #{a => 1, b => 2}.
2> RevI = maps:iterator(M, reversed).
3> {K2, V2, RevI2} = maps:next(RevI), {K2, V2}.
{b,2}
4> {K1, V1, RevI3} = maps:next(RevI2),{K1, V1}.
{a,1}
5> maps:next(RevI3).
none
6> maps:to_list(RevI).
[{b,2},{a,1}]
```

Using a custom ordering function that orders binaries by size:

```erlang
1> M = #{<<"abcde">> => d, <<"y">> => b, <<"x">> => a, <<"pqr">> => c}.
2> SizeI = fun(A, B) when byte_size(A) < byte_size(B) -> true;
              (A, B) when byte_size(A) > byte_size(B) -> false;
              (A, B) -> A =< B
           end.
3> SizeOrdI = maps:iterator(M, SizeI).
4> maps:to_list(SizeOrdI).
[{<<"x">>,a},{<<"y">>,b},{<<"pqr">>,c},{<<"abcde">>,d}]
```

# `keys`
*since OTP 17.0* 

```erlang
-spec keys(Map) -> Keys when Map :: #{Key => _}, Keys :: [Key].
```

Returns a complete list of keys contained in `Map`, in any order.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{42 => three,1337 => "two","a" => 1}.
2> maps:keys(Map).
[42,1337,"a"]
```

# `map`
*since OTP 17.0* 

```erlang
-spec map(Fun, MapOrIter) -> Map
             when
                 Fun :: fun((Key, Value1) -> Value2),
                 MapOrIter :: #{Key => Value1} | iterator(Key, Value1),
                 Map :: #{Key => Value2}.
```

Produces a new map `Map` by calling function `Fun(Key, Value1)` for every
`Key` to value `Value1` association in `MapOrIter`.

The `Fun/2` fun must return value `Value2` to be associated with key
`Key` for the new map `Map`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value1)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K,V1) when is_list(K) -> V1*2 end.
2> Map = #{"k1" => 1, "k2" => 2, "k3" => 3}.
3> maps:map(Fun, Map).
#{"k1" => 2,"k2" => 4,"k3" => 6}
```

# `merge`
*since OTP 17.0* 

```erlang
-spec merge(Map1, Map2) -> Map3 when Map1 :: map(), Map2 :: map(), Map3 :: map().
```

Merges maps `Map1` and `Map2` into a single map `Map3`, where values
from `Map2` override those from `Map1` for duplicate keys.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:merge(Map1, Map2).
#{a => 1,b => "two",c => 3}
```

# `merge_with`
*since OTP 24.0* 

```erlang
-spec merge_with(Combiner, Map1, Map2) -> Map3
                    when
                        Map1 :: #{Key1 => Value1},
                        Map2 :: #{Key2 => Value2},
                        Combiner :: fun((Key1, Value1, Value2) -> CombineResult),
                        Map3 :: #{Key1 => CombineResult, Key1 => Value1, Key2 => Value2}.
```

Merges maps `Map1` and `Map2` into a single map `Map3`, combining values for
duplicate keys using the `Combiner` fun.

When `Combiner` is applied, the key that exists in both maps is the
first parameter, the value from `Map1` is the second parameter, and
the value from `Map2` is the third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

## Examples

```erlang
1> Map1 = #{a => 3, b => 5}.
2> Map2 = #{a => 4, c => 17}.
3> maps:merge_with(fun(_Key, Val1, Val2) -> Val1 + Val2 end, Map1, Map2).
#{a => 7,b => 5,c => 17}
```

# `new`
*since OTP 17.0* 

```erlang
-spec new() -> Map when Map :: #{}.
```

Returns a new empty map.

## Examples

```text
1> maps:new().
#{}
```

# `next`
*since OTP 21.0* 

```erlang
-spec next(Iterator) -> {Key, Value, NextIterator} | none
              when Iterator :: iterator(Key, Value), NextIterator :: iterator(Key, Value).
```

Returns the next key-value association in `Iterator` and a new iterator for the
remaining associations in the iterator.

If there are no more associations in the iterator, `none` is returned.

## Examples

```erlang
1> Map = #{a => 1, b => 2, c => 3}.
#{a => 1,b => 2,c => 3}
2> I = maps:iterator(Map, ordered).
3> {K1, V1, I1} = maps:next(I), {K1, V1}.
{a,1}
4> {K2, V2, I2} = maps:next(I1), {K2, V2}.
{b,2}
5> {K3, V3, I3} = maps:next(I2), {K3, V3}.
{c,3}
6> maps:next(I3).
none
```

# `put`
*since OTP 17.0* 

```erlang
-spec put(Key, Value, Map1) -> Map2 when Key :: term(), Value :: term(), Map1 :: map(), Map2 :: map().
```

Associates `Key` with `Value` in Map1, replacing any existing value, and
returns a new map `Map2` with the updated association alongside the
original entries from `Map1`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:put("a", 42, Map).
#{"a" => 42}
3> maps:put("b", 1337, Map).
#{"a" => 1,"b" => 1337}
```

# `remove`
*since OTP 17.0* 

```erlang
-spec remove(Key, Map1) -> Map2 when Key :: term(), Map1 :: map(), Map2 :: map().
```

Removes `Key` and its associated value from `Map1`, if it exists, and
returns a new map `Map2` without `Key`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:remove("a", Map).
#{}
3> maps:remove("b", Map).
#{"a" => 1}
```

# `size`
*since OTP 17.0* 

```erlang
-spec size(Map) -> non_neg_integer() when Map :: map().
```

Returns the number of key-value associations in `Map`.

This operation occurs in constant time.

## Examples

```erlang
1> Map = #{42 => value_two,1337 => "value one","a" => 1}.
2> maps:size(Map).
3
```

# `take`
*since OTP 19.0* 

```erlang
-spec take(Key, Map1) -> {Value, Map2} | error when Map1 :: #{Key => Value, _ => _}, Map2 :: #{_ => _}.
```

Removes `Key` and its associated value from `Map1`, if it exists,
returning a tuple with the removed value `Value` and the new map
`Map2`; otherwise, returns error.

The call will fail with a `{badmap,Map}` exception if `Map1` is not a map.

Example:

```erlang
1> Map = #{"a" => "hello", "b" => "world"}.
#{"a" => "hello", "b" => "world"}
2> maps:take("a", Map).
{"hello",#{"b" => "world"}}
3> maps:take("does not exist", Map).
error
```

# `to_list`
*since OTP 17.0* 

```erlang
-spec to_list(MapOrIterator) -> [{Key, Value}]
                 when MapOrIterator :: #{Key => Value} | iterator(Key, Value).
```

Returns a list of pairs representing the key-value associations of
`MapOrIterator`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `{Key, Value}` tuples in the resulting list is not
defined.

The call fails with a `{badmap,Map}` exception if `MapOrIterator` is not a map
or an iterator obtained by a call to `iterator/1` or `iterator/2`.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> maps:to_list(Map).
[{42,value_three},{1337,"value two"},{"a",1}]
```

Using an ordered iterator to return an ordered list:

```erlang
1> Map = #{z => 1, y => 2, x => 3}.
2> maps:to_list(maps:iterator(Map, ordered)).
[{x,3},{y,2},{z,1}]
```

# `update`
*since OTP 17.0* 

```erlang
-spec update(Key, Value, Map1) -> Map2 when Map1 :: #{Key := _, _ => _}, Map2 :: #{Key := Value, _ => _}.
```

If `Key` exists in `Map1`, its value is replaced with `Value`, and the
function returns a new map `Map2` with the updated association.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:update("a", 42, Map).
#{"a" => 42}
```

# `update_with`
*since OTP 19.0* 

```erlang
-spec update_with(Key, Fun, Map1) -> Map2
                     when
                         Map1 :: #{Key := Value1, _ => _},
                         Map2 :: #{Key := Value2, _ => _},
                         Fun :: fun((Value1) -> Value2).
```

Updates a value in a `Map1` associated with `Key` by calling `Fun` on the old
value to produce a new value.

The call fails with a `{badkey,Key}` exception if `Key` is not present
in the map.

## Examples

```erlang
1> Map = #{counter => 1}.
2> Fun = fun(V) -> V + 1 end.
3> maps:update_with(counter, Fun, Map).
#{counter => 2}
```

# `update_with`
*since OTP 19.0* 

```erlang
-spec update_with(Key, Fun, Init, Map1) -> Map2
                     when
                         Map1 :: #{Key => Value1, _ => _},
                         Map2 :: #{Key := Value2 | Init, _ => _},
                         Fun :: fun((Value1) -> Value2).
```

Updates the value in `Map1` for `Key` by applying `Fun` to the old value or
using `Init` if `Key` is not present in the map.

## Examples

```erlang
1> Map = #{"counter" => 1}.
2> Fun = fun(V) -> V + 1 end.
3> maps:update_with("counter", Fun, 42, Map).
#{"counter" => 2}
4> maps:update_with("new counter", Fun, 42, Map).
#{"counter" => 1,"new counter" => 42}
```

# `values`
*since OTP 17.0* 

```erlang
-spec values(Map) -> Values when Map :: #{_ => Value}, Values :: [Value].
```

Returns a complete list of values contained in map `Map`, in any order.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> maps:values(Map).
[value_three,"value two",1]
```

# `with`
*since OTP 17.3* 

```erlang
-spec with(Ks, Map1) -> Map2 when Ks :: [K], Map1 :: #{K => V, _ => _}, Map2 :: #{K => V}.
```

Returns a new map `Map2` with the keys `K1` through `Kn` and their associated
values from map `Map1`.

Any key in `Ks` that does not exist in `Map1` is ignored.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> Keys = ["a",42,"other key"].
3> maps:with(Keys, Map).
#{42 => value_three,"a" => 1}
```

# `without`
*since OTP 17.0* 

```erlang
-spec without(Ks, Map1) -> Map2 when Ks :: [K], Map1 :: map(), Map2 :: map(), K :: term().
```

Returns a new map `Map2` without keys `K1` through `Kn` and their associated
values from map `Map1`.

Any key in `Ks` that does not exist in `Map1` is ignored.

## Examples

```erlang
1> Map = #{42 => value_three, 1337 => "value two", "a" => 1}.
2> Keys = ["a",42,"other key"].
3> maps:without(Keys, Map).
#{1337 => "value two"}
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
