# `gb_sets`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/gb_sets.erl#L32)

Sets represented by general balanced trees.

This module provides ordered sets using Prof. Arne Andersson's General Balanced
Trees. Ordered sets can be much more efficient than using ordered lists, for
larger sets, but depends on the application.

The data representing a set as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

This module considers two elements as different if and only if they do not
compare equal (`==`).

## Complexity Note

The complexity on set operations is bounded by either _O(|S|)_ or _O(|T| _
log(|S|))\*, where S is the largest given set, depending on which is fastest for
any particular function call. For operating on sets of almost equal size, this
implementation is about 3 times slower than using ordered-list sets directly.
For sets of very different sizes, however, this solution can be arbitrarily much
faster; in practical cases, often 10-100 times. This implementation is
particularly suited for accumulating elements a few at a time, building up a
large set (> 100-200 elements), and repeatedly testing for membership in the
current set.

As with normal tree structures, lookup (membership testing), insertion, and
deletion have logarithmic complexity.

## Compatibility

See the [Compatibility Section in the `sets` module](`m:sets#module-compatibility`)
for information about the compatibility of the different implementations of sets
in the Standard Library.

## See Also

`m:gb_trees`, `m:ordsets`, `m:sets`

# `iter`

```elixir
-type iter() :: iter(_).
```

# `iter`

```elixir
-opaque iter(Element) :: {ordered | reversed, [gb_set_node(Element)]}.
```

A general balanced set iterator.

# `set`

```elixir
-type set() :: set(_).
```

# `set`

```elixir
-opaque set(Element) :: {non_neg_integer(), gb_set_node(Element)}.
```

A general balanced set.

# `add`

```elixir
-spec add(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

# `add_element`

```elixir
-spec add_element(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a new set formed from `Set1` with `Element` inserted.

If `Element` is already an element in `Set1`, nothing is changed.

## Examples

```erlang
1> S0 = gb_sets:new().
2> S1 = gb_sets:add_element(7, S0).
3> gb_sets:to_list(S1).
[7]
4> S2 = gb_sets:add_element(42, S1).
5> S2 = gb_sets:add_element(42, S1).
6> gb_sets:to_list(S2).
[7,42]
```

# `balance`

```elixir
-spec balance(Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Rebalances the tree representation of `Set1`.

This is rarely necessary, but can be motivated when a large number of
elements have been deleted from the tree without further
insertions. Forcing rebalancing can minimize lookup times, as deletion
does not rebalance the tree.

## Examples

```erlang
1> S0 = gb_sets:from_ordset(lists:seq(1, 100)).
2> Delete = fun(E, Set) -> gb_sets:delete(E, Set) end.
3> S1 = lists:foldl(Delete, S0, lists:seq(1, 50)).
4> gb_sets:size(S1).
50
5> S2 = gb_sets:balance(S1).
```

# `del_element`

```elixir
-spec del_element(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

# `delete`

```elixir
-spec delete(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a new set formed from `Set1` with `Element` removed, assuming
`Element` is present in `Set1`.

Use `delete_any/2` when deleting from a set where `Element` is potentially
missing.

## Examples

```erlang
1> S = gb_sets:from_list([a,b]).
2> gb_sets:to_list(gb_sets:delete(b, S)).
[a]
```

# `delete_any`

```elixir
-spec delete_any(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a new set formed from `Set1` with `Element` removed.

If `Element` is not an element in `Set1`, nothing is changed.

## Examples

```erlang
1> S = gb_sets:from_list([a,b]).
2> gb_sets:to_list(gb_sets:delete_any(b, S)).
[a]
3> S = gb_sets:delete_any(x, S).
```

# `difference`

```elixir
-spec difference(Set1, Set2) -> Set3
                    when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

# `empty`

```elixir
-spec empty() -> Set when Set :: set(none()).
```

Returns a new empty set.

## Examples

```erlang
1> gb_sets:to_list(gb_sets:empty()).
[]
```

# `filter`

```elixir
-spec filter(Pred, Set1) -> Set2
                when Pred :: fun((Element) -> boolean()), Set1 :: set(Element), Set2 :: set(Element).
```

Filters elements in `Set1` using predicate function `Pred`.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4,5,6,7]).
2> IsEven = fun(N) -> N rem 2 =:= 0 end.
3> Filtered = gb_sets:filter(IsEven, S).
4> gb_sets:to_list(Filtered).
[2,4,6]
```

# `filtermap`
*since OTP 27.0* 

```elixir
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

`gb_sets:filtermap/2` behaves as if it were defined as follows:

```erlang
filtermap(Fun, Set1) ->
    gb_sets:from_list(lists:filtermap(Fun, Set1)).
```

## Examples

```erlang
1> S = gb_sets:from_list([2,4,5,6,8,9])
2> F = fun(X) ->
           case X rem 2 of
               0 -> {true, X div 2};
               1 -> false
           end
        end.
3> Set = gb_sets:filtermap(F, S).
4> gb_sets:to_list(Set).
[1,2,3,4]
```

# `fold`

```elixir
-spec fold(Function, Acc0, Set) -> Acc1
              when
                  Function :: fun((Element, AccIn) -> AccOut),
                  Acc0 :: Acc,
                  Acc1 :: Acc,
                  AccIn :: Acc,
                  AccOut :: Acc,
                  Set :: set(Element).
```

Folds `Function` over every element in `Set` and returns the final value of
the accumulator.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4]).
2> Plus = fun erlang:'+'/2.
3> gb_sets:fold(Plus, 0, S).
10
```

# `from_list`

```elixir
-spec from_list(List) -> Set when List :: [Element], Set :: set(Element).
```

Returns a set of the elements in `List`, where `List` can be unordered and
contain duplicates.

## Examples

```erlang
1> Unordered = [x,y,a,x,y,b,b,z]
2> gb_sets:to_list(gb_sets:from_list(Unordered)).
[a,b,x,y,z]
```

# `from_ordset`

```elixir
-spec from_ordset(List) -> Set when List :: [Element], Set :: set(Element).
```

Turns an ordered list without duplicates `List` into a set.

See `from_list/1` for a function that accepts unordered lists with
duplicates.

## Examples

```erlang
1> Ordset = [1,2,3].
2> gb_sets:to_list(gb_sets:from_ordset(Ordset)).
[1,2,3]
```

# `insert`

```elixir
-spec insert(Element, Set1) -> Set2 when Set1 :: set(Element), Set2 :: set(Element).
```

Returns a new set formed from `Set1` with `Element` inserted,
assuming `Element` is not already present.

Use `add/2` for inserting into a set where `Element` is potentially
already present.

## Examples

```erlang
1> S0 = gb_sets:new().
2> S1 = gb_sets:insert(7, S0).
3> gb_sets:to_list(S1).
[7]
4> S2 = gb_sets:insert(42, S1).
5> gb_sets:to_list(S2).
[7,42]
```

# `intersection`

```elixir
-spec intersection(SetList) -> Set when SetList :: [set(Element), ...], Set :: set(Element).
```

Returns the intersection of the non-empty list of sets.

The intersection of multiple sets is a new set that contains only the
elements that are present in all sets.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> gb_sets:to_list(gb_sets:intersection([S0, S1, S2])).
[]
6> gb_sets:to_list(gb_sets:intersection([S0, S1])).
[d]
7> gb_sets:intersection([]).
** exception error: no function clause matching gb_sets:intersection([])
```

# `intersection`

```elixir
-spec intersection(Set1, Set2) -> Set3
                      when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns the intersection of `Set1` and `Set2`.

The intersection of two sets is a new set that contains only the
elements that are present in both sets.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> S2 = gb_sets:from_list([q,r]).
4> gb_sets:to_list(gb_sets:intersection(S0, S1)).
[c,d]
5> gb_sets:to_list(gb_sets:intersection(S1, S2)).
[]
```

# `is_disjoint`

```elixir
-spec is_disjoint(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `true` if `Set1` and `Set2` are disjoint; otherwise, returns
`false`.

Two sets are disjoint if they have no elements in common.

This function is equivalent to `gb_sets:intersection(Set1, Set2) =:= []`,
but faster.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> gb_sets:is_disjoint(S0, S1).
false
5> gb_sets:is_disjoint(S1, S2).
true
```

# `is_element`

```elixir
-spec is_element(Element, Set) -> boolean() when Set :: set(Element).
```

# `is_empty`

```elixir
-spec is_empty(Set) -> boolean() when Set :: set().
```

Returns `true` if `Set` is an empty set; otherwise, returns `false`.

## Examples

```erlang
1> gb_sets:is_empty(gb_sets:new()).
true
2> gb_sets:is_empty(gb_sets:singleton(1)).
false
```

# `is_equal`
*since OTP 27.0* 

```elixir
-spec is_equal(Set1, Set2) -> boolean() when Set1 :: set(), Set2 :: set().
```

Returns `true` if `Set1` and `Set2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
1> Empty = gb_sets:new().
2> S = gb_sets:from_list([a,b]).
3> gb_sets:is_equal(S, S)
true
4> gb_sets:is_equal(S, Empty)
false
```

# `is_member`

```elixir
-spec is_member(Element, Set) -> boolean() when Set :: set(Element).
```

Returns `true` if `Element` is an element of `Set`; otherwise, returns
`false`.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:is_member(42, S).
false
3> gb_sets:is_member(b, S).
true
```

# `is_set`

```elixir
-spec is_set(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` appears to be a set; otherwise, returns `false`.

> #### Note {: .info }
>
> This function will return `true` for any term that coincides with the
> representation of a `gb_set`, while not really being a `gb_set`, thus
> it might return false positive results. See also note on [data
> types](`e:system:data_types.md#no_user_types`).
>
> Furthermore, since gb_sets are opaque, calling this function on terms
> that are not gb_sets could result in `m:dialyzer` warnings.

## Examples

```erlang
1> gb_sets:is_set(gb_sets:new()).
true
2> gb_sets:is_set(gb_sets:singleton(42)).
true
3> gb_sets:is_set(0).
false
```

# `is_subset`

```elixir
-spec is_subset(Set1, Set2) -> boolean() when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `true` when every element of `Set1` is also a member of `Set2`;
otherwise, returns `false`.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d]).
3> gb_sets:is_subset(S1, S0).
true
4> gb_sets:is_subset(S0, S1).
false
5> gb_sets:is_subset(S0, S0).
true
```

# `iterator`

```elixir
-spec iterator(Set) -> Iter when Set :: set(Element), Iter :: iter(Element).
```

Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Equivalent to [`iterator(Set, ordered)`](`iterator/2`).

# `iterator`
*since OTP 27.0* 

```elixir
-spec iterator(Set, Order) -> Iter
                  when Set :: set(Element), Iter :: iter(Element), Order :: ordered | reversed.
```

Returns an iterator that can be used for traversing the entries of `Set` in
either `ordered` or `reversed` direction; see `next/1`.

The implementation is very efficient; traversing the whole set using
[`next/1`](`next/1`) is only slightly slower than getting the list of
all elements using `to_list/1` and traversing that. The main advantage
of the iterator approach is that it avoids building the complete list
of all elements to be built in memory at once.

```erlang
1> S = gb_sets:from_ordset([1,2,3,4,5]).
2> Iter0 = gb_sets:iterator(S, ordered).
3> element(1, gb_sets:next(Iter0)).
1
4> Iter1 = gb_sets:iterator(S, reversed).
5> element(1, gb_sets:next(Iter1)).
5
```

# `iterator_from`
*since OTP 18.0* 

```elixir
-spec iterator_from(Element, Set) -> Iter when Set :: set(Element), Iter :: iter(Element).
```

Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Unlike the iterator returned by `iterator/1` or `iterator/2`, this
iterator starts with the first element greater than or equal to
`Element`.

Equivalent to [`iterator_from(Element, Set, ordered)`](`iterator_from/3`).

## Examples

```erlang
1> S = gb_sets:from_ordset([10,20,30,40,50]).
2> Iter = gb_sets:iterator_from(17, S).
3> element(1, gb_sets:next(Iter)).
20
```

# `iterator_from`
*since OTP 27.0* 

```elixir
-spec iterator_from(Element, Set, Order) -> Iter
                       when Set :: set(Element), Iter :: iter(Element), Order :: ordered | reversed.
```

Returns an iterator that can be used for traversing the entries of `Set`; see
`next/1`.

Unlike the iterator returned by `iterator/1` or `iterator/2`, this
iterator starts with the first element greater than or equal to
`Element`.

## Examples

```erlang
1> S = gb_sets:from_ordset([10,20,30,40,50]).
2> Iter = gb_sets:iterator_from(17, S, reversed).
3> element(1, gb_sets:next(Iter)).
10
```

# `larger`
*since OTP 27.0* 

```elixir
-spec larger(Element1, Set) -> none | {found, Element2}
                when Element1 :: Element, Element2 :: Element, Set :: set(Element).
```

Returns `{found, Element2}`, where `Element2` is the least element strictly
greater than `Element1`.

Returns `none` if no such element exists.

## Examples

```erlang
1> S = gb_sets:from_list([10,20,30]).
2> gb_sets:larger(1, S).
{found,10}
3> gb_sets:larger(10, S).
{found,20}
4> gb_sets:larger(19, S).
{found,20}
5> gb_sets:larger(30, S).
none
```

# `largest`

```elixir
-spec largest(Set) -> Element when Set :: set(Element).
```

Returns the largest element in `Set`.

Assumes that `Set` is not empty.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:largest(S).
c
```

# `map`
*since OTP 27.0* 

```elixir
-spec map(Fun, Set1) -> Set2
             when Fun :: fun((Element1) -> Element2), Set1 :: set(Element1), Set2 :: set(Element2).
```

Maps elements in `Set1` with mapping function `Fun`.

## Examples

```erlang
1> S = gb_sets:from_list([1,2,3,4,5,6,7]).
2> F = fun(N) -> N div 2 end.
3> Mapped = gb_sets:map(F, S).
4> gb_sets:to_list(Mapped).
[0,1,2,3]
```

# `new`

```elixir
-spec new() -> Set when Set :: set(none()).
```

Returns a new empty set.

## Examples

```erlang
1> gb_sets:to_list(gb_sets:new()).
[]
```

# `next`

```elixir
-spec next(Iter1) -> {Element, Iter2} | none when Iter1 :: iter(Element), Iter2 :: iter(Element).
```

Returns `{Element, Iter2}`, where `Element` is the first element referred to
by iterator `Iter1`, and `Iter2` is the new iterator to be used for traversing
the remaining elements, or the atom `none` if no elements remain.

```erlang
1> S = gb_sets:from_ordset([1,2,3,4,5]).
2> Iter0 = gb_sets:iterator(S).
3> {Element0, Iter1} = gb_sets:next(Iter0).
4> Element0.
1
5> {Element1, Iter2} = gb_sets:next(Iter1).
6> Element1.
2
```

# `singleton`

```elixir
-spec singleton(Element) -> set(Element).
```

Returns a set containing only element `Element`.

## Examples

```erlang
1> S = gb_sets:singleton(42).
2> gb_sets:to_list(S).
[42]
```

# `size`

```elixir
-spec size(Set) -> non_neg_integer() when Set :: set().
```

Returns the number of elements in `Set`.

## Examples

```erlang
1> gb_sets:size(gb_sets:new()).
0
2> gb_sets:size(gb_sets:from_list([4,5,6])).
3
```

# `smaller`
*since OTP 27.0* 

```elixir
-spec smaller(Element1, Set) -> none | {found, Element2}
                 when Element1 :: Element, Element2 :: Element, Set :: set(Element).
```

Returns `{found, Element2}`, where `Element2` is the greatest element strictly
less than `Element1`.

Returns `none` if no such element exists.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:smaller(b, S).
{found,a}
3> gb_sets:smaller(z, S).
{found,c}
4> gb_sets:smaller(a, S).
none
```

# `smallest`

```elixir
-spec smallest(Set) -> Element when Set :: set(Element).
```

Returns the smallest element in `Set`.

Assumes that `Set` is not empty.

## Examples

```erlang
1> S = gb_sets:from_list([a,b,c]).
2> gb_sets:smallest(S).
a
```

# `subtract`

```elixir
-spec subtract(Set1, Set2) -> Set3 when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns the elements of `Set1` that are not elements in `Set2`.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> gb_sets:to_list(gb_sets:subtract(S0, S1)).
[a,b]
4> gb_sets:to_list(gb_sets:subtract(S1, S0)).
[e,f]
```

# `take_largest`

```elixir
-spec take_largest(Set1) -> {Element, Set2} when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `{Element, Set2}`, where `Element` is the largest element in
`Set1`, and `Set2` is this set with `Element` deleted.

Assumes that `Set1` is not empty.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c]).
2> {Largest,S1} = gb_sets:take_largest(S0).
3> Largest.
c
4> gb_sets:to_list(S1).
[a,b]
```

# `take_smallest`

```elixir
-spec take_smallest(Set1) -> {Element, Set2} when Set1 :: set(Element), Set2 :: set(Element).
```

Returns `{Element, Set2}`, where `Element` is the smallest element in
`Set1`, and `Set2` is this set with `Element` deleted.

Assumes that `Set1` is not empty.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c]).
2> {Smallest,S1} = gb_sets:take_smallest(S0).
3> Smallest.
a
4> gb_sets:to_list(S1).
[b,c]
```

# `to_list`

```elixir
-spec to_list(Set) -> List when Set :: set(Element), List :: [Element].
```

Returns the elements of `Set` as an ordered list.

```erlang
1> gb_sets:to_list(gb_sets:from_list([4,3,5,1,2])).
[1,2,3,4,5]
```

# `union`

```elixir
-spec union(SetList) -> Set when SetList :: [set(Element), ...], Set :: set(Element).
```

Returns the union of a list of sets.

The union of multiple sets is a new set that contains all the elements from
all sets, without duplicates.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([d,e,f]).
3> S2 = gb_sets:from_list([q,r])
4> Sets = [S0, S1, S2].
5> Union = gb_sets:union(Sets).
6> gb_sets:to_list(Union).
[a,b,c,d,e,f,q,r]
```

# `union`

```elixir
-spec union(Set1, Set2) -> Set3 when Set1 :: set(Element), Set2 :: set(Element), Set3 :: set(Element).
```

Returns the union of `Set1` and `Set2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
1> S0 = gb_sets:from_list([a,b,c,d]).
2> S1 = gb_sets:from_list([c,d,e,f]).
3> Union = gb_sets:union(S0, S1).
4> gb_sets:to_list(Union).
[a,b,c,d,e,f]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
