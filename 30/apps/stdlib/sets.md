# `sets`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/sets.erl#L42)

Sets are collections of elements with no duplicate elements.

The data representing a set as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

This module provides the same interface as the `m:ordsets` module but
with an undefined representation. One key difference is that this
module considers two elements as different if they do not match
(`=:=`), whereas `ordsets` considers them different if and only if
they do not compare equal (`==`).

Erlang/OTP 24.0 introduced a new more performant representation for sets which
has become the default in Erlang/OTP 28. Developers can use the old representation
by passing the `{version, 1}` flag to `new/1` and `from_list/2`. Functions that
work on two sets, such as `union/2`, will work with sets of different
versions. In such cases, there is no guarantee about the version of the returned set.
Explicit conversion from the old version to the new one can be done with
`sets:from_list(sets:to_list(Old), [{version,2}])`.

## Compatibility

The following functions in this module also exist and provide the same
functionality in the `m:gb_sets` and `m:ordsets` modules. That is, by only
changing the module name for each call, you can try out different set
representations.

- `add_element/2`
- `del_element/2`
- `filter/2`
- `filtermap/2`
- `fold/3`
- `from_list/1`
- `intersection/1`
- `intersection/2`
- `is_disjoint/2`
- `is_element/2`
- `is_empty/1`
- `is_equal/2`
- `is_set/1`
- `is_subset/2`
- `map/2`
- `new/0`
- `size/1`
- `subtract/2`
- `to_list/1`
- `union/1`
- `union/2`

> #### Note {: .info }
>
> While the three set implementations offer the same _functionality_ with
> respect to the aforementioned functions, their overall _behavior_ may differ.
> As mentioned, this module considers elements as different if and only if they
> do not match (`=:=`), while both `m:ordsets` and `m:gb_sets` consider elements
> as different if and only if they do not compare equal (`==`).
>
> ### Examples
>
> ```erlang
> 1> sets:is_element(1.0, sets:from_list([1])).
> false
> 2> ordsets:is_element(1.0, ordsets:from_list([1])).
> true
> 3> gb_sets:is_element(1.0, gb_sets:from_list([1])).
> true
> ```

### See Also

`m:gb_sets`, `m:ordsets`

# `set`

```erlang
-type set() :: set(_).
```

# `set`

```erlang
-opaque set(Element)
```

As returned by `new/0`.

# `add_element`

```erlang
-spec add_element(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a new set formed from `Set1` with `Element` inserted.

## Examples

```erlang
1> S0 = sets:new().
2> S1 = sets:add_element(7, S0).
3> sets:to_list(S1).
[7]
4> S2 = sets:add_element(42, S1).
5> lists:sort(sets:to_list(S2)).
[7,42]
6> S2 = sets:add_element(42, S1).
7> lists:sort(sets:to_list(S2)).
[7,42]
```

# `del_element`

```erlang
-spec del_element(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a copy of `Set1` with `Element` removed.

## Examples

```erlang
1> S = sets:from_list([a,b]).
2> sets:to_list(sets:del_element(b, S)).
[a]
3> S = sets:del_element(x, S).
4> lists:sort(sets:to_list(S)).
[a,b]
```

# `filter`

```erlang
-spec filter(Pred, Set1) -> Set2
                when Pred :: fun((Element) -> boolean()), Set1 :: set(Element), Set2 :: set(Element).
```

Filters elements in `Set1` using predicate function `Pred`.

## Examples

```erlang
1> S = sets:from_list([1,2,3,4,5,6,7]).
2> IsEven = fun(N) -> N rem 2 =:= 0 end.
3> Filtered = sets:filter(IsEven, S).
4> lists:sort(sets:to_list(Filtered)).
[2,4,6]
```

# `filtermap`
*since OTP 27.0* 

```erlang
-spec filtermap(Fun, Set1) -> Set2
                   when
                       Fun :: fun((Element1) -> boolean() | {true, Element2}),
                       Set1 :: set(Element1),
                       Set2 :: set(Element1 | Element2).
```

Calls `Fun(Elem)` for each `Elem` of `Set1` to update or remove
elements from `Set1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The
function returns the set of elements for which `Fun` returns a new
value, with `true` being equivalent to `{true, Elem}`.

`sets:filtermap/2` behaves as if it were defined as follows:

```erlang
filtermap(Fun, Set1) ->
    sets:from_list(lists:filtermap(Fun, sets:to_list(Set1))).
```

## Examples

```erlang
1> S = sets:from_list([2,4,5,6,8,9]).
2> F = fun(X) ->
           case X rem 2 of
               0 -> {true, X div 2};
               1 -> false
           end
        end.
3> Set = sets:filtermap(F, S).
4> lists:sort(sets:to_list(Set)).
[1,2,3,4]
```

# `fold`

```erlang
-spec fold(Function, Acc0, Set) -> Acc1
              when
                  Function :: fun((Element, AccIn) -> AccOut),
                  Set :: set(Element),
                  Acc0 :: Acc,
                  Acc1 :: Acc,
                  AccIn :: Acc,
                  AccOut :: Acc.
```

Folds `Function` over every element in `Set` and returns the final value of
the accumulator.

The evaluation order is undefined.

## Examples

```erlang
1> S = sets:from_list([1,2,3,4]).
2> Plus = fun erlang:'+'/2.
3> sets:fold(Plus, 0, S).
10
```

# `from_list`

```erlang
-spec from_list(List) -> Set when List :: [Element], Set :: set(Element).
```

Returns a set of the elements in `List`.

## Examples

```erlang
1> S = sets:from_list([a,b,c]).
2> lists:sort(sets:to_list(S)).
[a,b,c]
```

# `from_list`
*since OTP 24.0* 

```erlang
-spec from_list(List, [{version, 1..2}]) -> Set when List :: [Element], Set :: set(Element).
```

Returns a set of the elements in `List` of the given version.

## Examples

```erlang
1> S = sets:from_list([a,b,c], [{version, 1}]).
2> lists:sort(sets:to_list(S)).
[a,b,c]
```

# `intersection`

```erlang
-spec intersection(SetList) -> Set when SetList :: [set(Element), ...], Set :: set(Element).
```

Returns the intersection of the non-empty list of sets.

The intersection of multiple sets is a new set that contains only the
elements that are present in all sets.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([d,e,f]).
3> S2 = sets:from_list([q,r]).
4> Sets = [S0, S1, S2].
5> sets:to_list(sets:intersection([S0, S1, S2])).
[]
6> sets:to_list(sets:intersection([S0, S1])).
[d]
7> sets:intersection([]).
** exception error: no function clause matching sets:intersection([])
```

# `intersection`

```erlang
-spec intersection(Set1, Set2) -> Set3
                      when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns the intersection of `Set1` and `Set2`.

The intersection of two sets is a new set that contains only the
elements that are present in both sets.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([c,d,e,f]).
3> S2 = sets:from_list([q,r]).
4> Intersection = sets:intersection(S0, S1).
5> lists:sort(sets:to_list(Intersection)).
[c,d]
6> sets:to_list(sets:intersection(S1, S2)).
[]
```

# `is_disjoint`

```erlang
-spec is_disjoint(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `true` if `Set1` and `Set2` are disjoint; otherwise, returns
`false`.

Two sets are disjoint if they have no elements in common.

This function is equivalent to `sets:is_empty(sets:intersection(Set1, Set2))`,
but faster.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([d,e,f]).
3> S2 = sets:from_list([q,r]).
4> sets:is_disjoint(S0, S1).
false
5> sets:is_disjoint(S1, S2).
true
```

# `is_element`

```erlang
-spec is_element(Element, Set) -> boolean() when Set :: set(Element).
```

Returns `true` if `Element` is an element of `Set`; otherwise, returns
`false`.

## Examples

```erlang
1> S = sets:from_list([a,b,c]).
2> sets:is_element(42, S).
false
3> sets:is_element(b, S).
true
```

# `is_empty`
*since OTP 21.0* 

```erlang
-spec is_empty(Set) -> boolean() when Set :: set().
```

Returns `true` if `Set` is an empty set; otherwise, returns `false`.

## Examples

```erlang
1> sets:is_empty(sets:new()).
true
2> sets:is_empty(sets:from_list([1])).
false
```

# `is_equal`
*since OTP 27.0* 

```erlang
-spec is_equal(Set1, Set2) -> boolean() when Set1 :: set(), Set2 :: set().
```

Returns `true` if `Set1` and `Set2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
1> Empty = sets:new().
2> S = sets:from_list([a,b]).
3> sets:is_equal(S, S).
true
4> sets:is_equal(S, Empty).
false
5> OldSet = sets:from_list([a,b], [{version,1}]).
6> sets:is_equal(S, OldSet).
true
7> S =:= OldSet.
false
```

# `is_set`

```erlang
-spec is_set(Set) -> boolean() when Set :: term().
```

Returns `true` if `Set` appears to be a set of elements; otherwise,
returns `false`.

> #### Note {: .info }
>
> Note that the test is shallow and will return `true` for any term that
> coincides with the possible representations of a set. See also note on
> [data types](`e:system:data_types.md#no_user_types`).
>
> Furthermore, since sets are opaque, calling this function on terms
> that are not sets could result in `m:dialyzer` warnings.

## Examples

```erlang
1> sets:is_set(sets:new()).
true
2> sets:is_set(sets:new([{version,1}])).
true
3> sets:is_set(0).
false
```

# `is_subset`

```erlang
-spec is_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `true` when every element of `Set1` is also a member of `Set2`;
otherwise, returns `false`.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([c,d]).
3> sets:is_subset(S1, S0).
true
4> sets:is_subset(S0, S1).
false
5> sets:is_subset(S0, S0).
true
```

# `map`
*since OTP 27.0* 

```erlang
-spec map(Fun, Set1) -> Set2
             when Fun :: fun((Element1) -> Element2), Set1 :: set(Element1), Set2 :: set(Element2).
```

Maps elements in `Set1` with mapping function `Fun`.

## Examples

```erlang
1> S = sets:from_list([1,2,3,4,5,6,7]).
2> F = fun(N) -> N div 2 end.
3> Mapped = sets:map(F, S).
4> lists:sort(sets:to_list(Mapped)).
[0,1,2,3]
```

# `new`

```erlang
-spec new() -> set(none()).
```

Returns a new empty set.

## Examples

```erlang
1> sets:to_list(sets:new()).
[]
```

# `new`
*since OTP 24.0* 

```erlang
-spec new([{version, 1..2}]) -> set(none()).
```

Returns a new empty set of the given version.

## Examples

```erlang
1> sets:to_list(sets:new([{version, 1}])).
[]
2> sets:new() =:= sets:new([{version, 2}]).
true
```

# `size`

```erlang
-spec size(Set) -> non_neg_integer() when Set :: set().
```

Returns the number of elements in `Set`.

## Examples

```erlang
1> sets:size(sets:new()).
0
2> sets:size(sets:from_list([4,5,6])).
3
```

# `subtract`

```erlang
-spec subtract(Set1, Set2) -> Set3 when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns a new set containing the elements of `Set1`
that are not elements in `Set2`.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([c,d,e,f]).
3> lists:sort(sets:to_list(sets:subtract(S0, S1))).
[a,b]
4> lists:sort(sets:to_list(sets:subtract(S1, S0))).
[e,f]
```

# `to_list`

```erlang
-spec to_list(Set) -> List when Set :: set(Element), List :: [Element].
```

Returns the elements of `Set` as a list.

The order of the returned elements is undefined.

## Examples

```erlang
1> S = sets:from_list([1,2,3]).
2> lists:sort(sets:to_list(S)).
[1,2,3]
```

# `union`

```erlang
-spec union(SetList) -> Set when SetList :: [set(Element)], Set :: set(Element).
```

Returns the union of a list of sets.

The union of multiple sets is a new set that contains all the elements from
all sets, without duplicates.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([d,e,f]).
3> S2 = sets:from_list([q,r]).
4> Sets = [S0, S1, S2].
5> Union = sets:union(Sets).
6> lists:sort(sets:to_list(Union)).
[a,b,c,d,e,f,q,r]
```

# `union`

```erlang
-spec union(Set1, Set2) -> Set3 when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns the union of `Set1` and `Set2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
1> S0 = sets:from_list([a,b,c,d]).
2> S1 = sets:from_list([c,d,e,f]).
3> Union = sets:union(S0, S1).
4> lists:sort(sets:to_list(Union)).
[a,b,c,d,e,f]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
