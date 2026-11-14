# `ordsets`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/ordsets.erl#L22)

Functions for manipulating sets as ordered lists.

Sets are collections of elements with no duplicate elements. An `ordset` is a
representation of a set, where an ordered list is used to store the elements of
the set. An ordered list is more efficient than an unordered list. Elements are
ordered according to the _Erlang term order_.

This module provides the same interface as the `m:sets` module but with a
defined representation. One difference is that while `sets` considers two
elements as different if they do not match (`=:=`), this module considers two
elements as different if and only if they do not compare equal (`==`).

See the [Compatibility Section in the `sets` module](`m:sets#module-compatibility`)
for more information about the compatibility of the different implementations of
sets in the Standard Library.

### See Also

`m:gb_sets`, `m:sets`

# `ordset`

```erlang
-type ordset(T) :: [T].
```

As returned by `new/0`.

# `add_element`

```erlang
-spec add_element(Element, Ordset1) -> Ordset2
                     when Element :: E, Ordset1 :: ordset(T), Ordset2 :: ordset(T | E).
```

Returns a new ordered set formed from `Ordset1` with `Element` inserted.

## Examples

```erlang
1> S0 = ordsets:new().
[]
2> S1 = ordsets:add_element(7, S0).
[7]
3> S2 = ordsets:add_element(42, S1).
[7,42]
4> ordsets:add_element(42, S2).
[7,42]
```

# `del_element`

```erlang
-spec del_element(Element, Ordset1) -> Ordset2
                     when Element :: term(), Ordset1 :: ordset(T), Ordset2 :: ordset(T).
```

Returns a copy of `Ordset1` with `Element` removed.

## Examples

```erlang
1> S = ordsets:from_list([a,b,c]).
2> ordsets:del_element(c, S).
[a,b]
3> ordsets:del_element(x, S).
[a,b,c]
```

# `filter`

```erlang
-spec filter(Pred, Ordset1) -> Ordset2
                when
                    Pred :: fun((Element :: T) -> boolean()), Ordset1 :: ordset(T), Ordset2 :: ordset(T).
```

Filters elements in `Ordset1` using predicate function `Pred`.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4,5,6,7]).
2> IsEven = fun(N) -> N rem 2 =:= 0 end.
3> ordsets:filter(IsEven, S).
[2,4,6]
```

# `filtermap`
*since OTP 27.0* 

```erlang
-spec filtermap(Fun, Ordset1) -> Ordset2
                   when
                       Fun :: fun((Element1 :: T1) -> boolean() | {true, Element2 :: T2}),
                       Ordset1 :: ordset(T1),
                       Ordset2 :: ordset(T1 | T2).
```

Calls `Fun(Elem)` for each `Elem` of `Ordset1` to update or remove
elements from `Ordset1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The
function returns the set of elements for which `Fun` returns a new
value, with `true` being equivalent to `{true, Elem}`.

`ordsets:filtermap/2` behaves as if it were defined as follows:

```erlang
filtermap(Fun, Ordset1) ->
    ordsets:from_list(lists:filtermap(Fun, ordsets:to_list(Ordset1))).
```

## Examples

```erlang
1> S = ordsets:from_list([2,4,5,6,8,9]).
2> F = fun(X) ->
           case X rem 2 of
               0 -> {true, X div 2};
               1 -> false
           end
        end.
3> ordsets:filtermap(F, S).
[1,2,3,4]
```

# `fold`

```erlang
-spec fold(Function, Acc0, Ordset) -> Acc1
              when
                  Function :: fun((Element :: T, AccIn :: term()) -> AccOut :: term()),
                  Ordset :: ordset(T),
                  Acc0 :: term(),
                  Acc1 :: term().
```

Folds `Function` over every element in `Ordset` and returns the final value of
the accumulator.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4]).
2> Plus = fun erlang:'+'/2.
3> ordsets:fold(Plus, 0, S).
10
```

# `from_list`

```erlang
-spec from_list(List) -> Ordset when List :: [T], Ordset :: ordset(T).
```

Returns an ordered set of the elements in `List`.

## Examples

```erlang
1> ordsets:from_list([a,b,a,b,b,c]).
[a,b,c]
```

# `intersection`

```erlang
-spec intersection(OrdsetList) -> Ordset when OrdsetList :: [ordset(_), ...], Ordset :: ordset(_).
```

Returns the intersection of the non-empty list of sets.

The intersection of multiple sets is a new set that contains only the
elements that are present in all sets.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r]).
4> Sets = [S0, S1, S2].
5> ordsets:intersection([S0, S1, S2]).
[]
6> ordsets:intersection([S0, S1]).
[d]
7> ordsets:intersection([]).
** exception error: no function clause matching ordsets:intersection([])
```

# `intersection`

```erlang
-spec intersection(Ordset1, Ordset2) -> Ordset3
                      when Ordset1 :: ordset(_), Ordset2 :: ordset(_), Ordset3 :: ordset(_).
```

Returns the intersection of `Ordset1` and `Ordset2`.

The intersection of two sets is a new set that contains only the
elements that are present in both sets.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> S2 = ordsets:from_list([q,r]).
4> ordsets:intersection(S0, S1).
[c,d]
5> ordsets:intersection(S1, S2).
[]
```

# `is_disjoint`

```erlang
-spec is_disjoint(Ordset1, Ordset2) -> boolean() when Ordset1 :: ordset(_), Ordset2 :: ordset(_).
```

Returns `true` if `Ordset1` and `Ordset2` are disjoint; otherwise,
returns `false`.

Two sets are disjoint if they have no elements in common.

This function is equivalent to `ordsets:is_empty(ordsets:intersection(Ordset1, Ordset2))`, but faster.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r]).
4> ordsets:is_disjoint(S0, S1).
false
5> ordsets:is_disjoint(S1, S2).
true
```

# `is_element`

```erlang
-spec is_element(Element, Ordset) -> boolean() when Element :: term(), Ordset :: ordset(_).
```

Returns `true` if `Element` is an element of `Ordset`; otherwise, returns `false`.

## Examples

```erlang
1> S = ordsets:from_list([a,b,c]).
2> ordsets:is_element(42, S).
false
3> ordsets:is_element(b, S).
true
```

# `is_empty`
*since OTP 21.0* 

```erlang
-spec is_empty(Ordset) -> boolean() when Ordset :: ordset(_).
```

Returns `true` if `Ordset` is an empty set; otherwise, returns `false`.

## Examples

```erlang
1> ordsets:is_empty(ordsets:new()).
true
2> ordsets:is_empty(ordsets:from_list([1])).
false
```

# `is_equal`
*since OTP 27.0* 

```erlang
-spec is_equal(Ordset1, Ordset2) -> boolean() when Ordset1 :: ordset(_), Ordset2 :: ordset(_).
```

Returns `true` if `Ordset1` and `Ordset2` are equal, that is, if every element
of one set is also a member of the other set; otherwise, returns `false`.

## Examples

```erlang
1> Empty = ordsets:new().
2> S = ordsets:from_list([a,b]).
3> ordsets:is_equal(S, S).
true
4> ordsets:is_equal(S, Empty).
false
```

# `is_set`

```erlang
-spec is_set(Ordset) -> boolean() when Ordset :: term().
```

Returns `true` if `Ordset` is an ordered set of elements; otherwise,
returns `false`.

> #### Note {: .info }
>
> This function returns true for any ordered list, even if it was not
> constructed using the functions in this module.

## Examples

```erlang
1> ordsets:is_set(ordsets:from_list([a,x,13,{p,q}])).
true
2> ordsets:is_set([a,b,c]).
true
3> ordsets:is_set([z,a]).
false
4> ordsets:is_set({a,b}).
false
```

# `is_subset`

```erlang
-spec is_subset(Ordset1, Ordset2) -> boolean() when Ordset1 :: ordset(_), Ordset2 :: ordset(_).
```

Returns `true` when every element of `Ordset1` is also a member of `Ordset2`;
otherwise, returns `false`.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d]).
3> ordsets:is_subset(S1, S0).
true
4> ordsets:is_subset(S0, S1).
false
5> ordsets:is_subset(S0, S0).
true
```

# `map`
*since OTP 27.0* 

```erlang
-spec map(Fun, Ordset1) -> Ordset2
             when
                 Fun :: fun((Element1 :: T1) -> Element2 :: T2),
                 Ordset1 :: ordset(T1),
                 Ordset2 :: ordset(T2).
```

Maps elements in `Ordset1` with mapping function `Fun`.

## Examples

```erlang
1> S = ordsets:from_list([1,2,3,4,5,6,7]).
2> F = fun(N) -> N div 2 end.
3> ordsets:map(F, S).
[0,1,2,3]
```

# `new`

```erlang
-spec new() -> ordset(none()).
```

Returns a new empty ordered set.

## Examples

```erlang
1> ordsets:new().
[]
```

# `size`

```erlang
-spec size(Ordset) -> non_neg_integer() when Ordset :: ordset(_).
```

Returns the number of elements in `Ordset`.

## Examples

```erlang
1> ordsets:size(ordsets:new()).
0
2> ordsets:size(ordsets:from_list([4,5,6])).
3
```

# `subtract`

```erlang
-spec subtract(Ordset1, Ordset2) -> Ordset3
                  when Ordset1 :: ordset(_), Ordset2 :: ordset(_), Ordset3 :: ordset(_).
```

Returns a new ordered set containing the elements of `Ordset1`
that are not elements in `Ordset2`.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> ordsets:subtract(S0, S1).
[a,b]
4> ordsets:subtract(S1, S0).
[e,f]
```

# `to_list`

```erlang
-spec to_list(Ordset) -> List when Ordset :: ordset(T), List :: [T].
```

Returns the elements of `Ordset` as a list.

## Examples

```erlang
1> S = ordsets:from_list([a,b]).
2> ordsets:to_list(S).
[a,b]
```

# `union`

```erlang
-spec union(OrdsetList) -> Ordset when OrdsetList :: [ordset(T)], Ordset :: ordset(T).
```

Returns the union of a list of sets.

The union of multiple sets is a new set that contains all the elements from
all sets, without duplicates.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([d,e,f]).
3> S2 = ordsets:from_list([q,r]).
4> Sets = [S0, S1, S2].
5> ordsets:union(Sets).
[a,b,c,d,e,f,q,r]
```

# `union`

```erlang
-spec union(Ordset1, Ordset2) -> Ordset3
               when Ordset1 :: ordset(T1), Ordset2 :: ordset(T2), Ordset3 :: ordset(T1 | T2).
```

Returns the union of `Ordset1` and `Ordset2`.

The union of two sets is a new set that contains all the elements from
both sets, without duplicates.

## Examples

```erlang
1> S0 = ordsets:from_list([a,b,c,d]).
2> S1 = ordsets:from_list([c,d,e,f]).
3> ordsets:union(S0, S1).
[a,b,c,d,e,f]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
