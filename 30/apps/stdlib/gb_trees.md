# `gb_trees`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/gb_trees.erl#L33)

General balanced trees.

This module provides Prof. Arne Andersson's General Balanced Trees. These have
no storage overhead compared to unbalanced binary trees, and their performance
is better than AVL trees.

This module considers two keys as different if and only if they do not compare
equal (`==`).

## Data Structure

Trees and iterators are built using opaque data structures that should not be
pattern-matched from outside this module.

There is no attempt to balance trees after deletions. As deletions do not
increase the height of a tree, this should be OK.

The original balance condition `h(T) <= ceil(c * log(|T|))` has been changed to
the similar (but not quite equivalent) condition `2 ^ h(T) <= |T| ^ c`. This
should also be OK.

### See Also

`m:dict`, `m:gb_sets`, `m:maps`

# `iter`

```erlang
-type iter() :: iter(_, _).
```

# `iter`

```erlang
-opaque iter(Key, Value)
```

A general balanced tree iterator.

# `tree`

```erlang
-type tree() :: tree(_, _).
```

# `tree`

```erlang
-opaque tree(Key, Value)
```

A general balanced tree.

# `balance`

```erlang
-spec balance(Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Rebalances `Tree1`.

Note that this is rarely necessary, but can be motivated
when many nodes have been deleted from the tree without further insertions.
Rebalancing can then be forced to minimize lookup times, as deletion does not
rebalance the tree.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{I,2*I} || I <- lists:seq(1, 100)]).
2> Delete = fun gb_trees:delete/2.
3> Tree2 = lists:foldl(Delete, Tree1, lists:seq(1, 50)).
4> gb_trees:size(Tree2).
50
5> Tree3 = gb_trees:balance(Tree2).
```

# `delete`

```erlang
-spec delete(Key, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Removes the node with key `Key` from `Tree1`, returning the new tree;
raises an exception if `Key` is not present.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{a,1},{b,2}]).
2> Tree2 = gb_trees:delete(a, Tree1).
3> gb_trees:to_list(Tree2).
[{b,2}]
```

# `delete_any`

```erlang
-spec delete_any(Key, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Removes the node with key `Key` from `Tree1` if present and returns the
resulting tree; otherwise, returns `Tree1` unchanged.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{a,1},{b,2}]).
2> Tree2 = gb_trees:delete_any(a, Tree1).
3> gb_trees:to_list(Tree2).
[{b,2}]
4> Tree3 = gb_trees:delete_any(z, Tree2).
5> Tree2 == Tree3.
true
```

# `empty`

```erlang
-spec empty() -> tree(none(), none()).
```

Returns a new empty tree.

## Examples

```erlang
1> gb_trees:to_list(gb_trees:empty()).
[]
```

# `enter`

```erlang
-spec enter(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Inserts `Key` with value `Value` into `Tree1` if not present, or
updates the value for `Key` to `Value` if present; returns the new
tree.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{a,1},{b,2}]).
2> Tree2 = gb_trees:enter(c, 10, Tree1).
3> Tree3 = gb_trees:enter(a, 100, Tree2).
4> gb_trees:to_list(Tree3).
[{a,100},{b,2},{c,10}]
```

# `from_list`
*since OTP 29.0* 

```erlang
-spec from_list(List) -> Tree when List :: [{Key, Value}], Tree :: tree(Key, Value).
```

Returns a tree of the key-value tuples in `List`,
where `List` can be unordered and contain duplicate keys.

## Examples

```erlang
1> Unordered = [{x, 1}, {y, 2}, {a, 3}, {x, 4}, {y, 5}, {b, 6}].
2> gb_trees:to_list(gb_trees:from_list(Unordered)).
[{a,3},{b,6},{x,4},{y,5}]
```

# `from_orddict`

```erlang
-spec from_orddict(List) -> Tree when List :: [{Key, Value}], Tree :: tree(Key, Value).
```

Turns an ordered list `List` of key-value tuples into a tree.

The list must not contain duplicate keys.

## Examples

```erlang
1> Tree = gb_trees:from_orddict([{a,1},{b,2}]).
2> gb_trees:to_list(Tree).
[{a,1},{b,2}]
```

# `get`

```erlang
-spec get(Key, Tree) -> Value when Tree :: tree(Key, Value).
```

Retrieves the value stored with `Key` in `Tree`; raises an exception
if `Key` is not present.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2}]).
2> gb_trees:get(b, Tree).
2
```

# `insert`

```erlang
-spec insert(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Inserts `Key` with value `Value` into `Tree1`, returning the new
tree; raises an exception if `Key` is already present.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{a,1},{b,2}]).
2> Tree2 = gb_trees:insert(c, 10, Tree1).
3> gb_trees:to_list(Tree2).
[{a,1},{b,2},{c,10}]
```

# `is_defined`

```erlang
-spec is_defined(Key, Tree) -> boolean() when Tree :: tree(Key, Value :: term()).
```

Returns `true` if `Key` is present in `Tree`; otherwise, returns
`false`.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:is_defined(a, Tree).
true
3> gb_trees:is_defined(x, Tree).
false
```

# `is_empty`

```erlang
-spec is_empty(Tree) -> boolean() when Tree :: tree().
```

Returns `true` if `Tree` is an empty tree; otherwise, returns `false`.

## Examples

```erlang
1> gb_trees:is_empty(gb_trees:empty()).
true
2> gb_trees:is_empty(gb_trees:from_list([{a,99}])).
false
```

# `iterator`

```erlang
-spec iterator(Tree) -> Iter when Tree :: tree(Key, Value), Iter :: iter(Key, Value).
```

# `iterator`
*since OTP 27.0* 

```erlang
-spec iterator(Tree, Order) -> Iter
                  when Tree :: tree(Key, Value), Iter :: iter(Key, Value), Order :: ordered | reversed.
```

Returns an iterator that can be used for traversing the entries of `Tree` in
either `ordered` or `reversed` direction; see `next/1`.

The implementation is very efficient; traversing the whole tree using
[`next/1`](`next/1`) is only slightly slower than getting the list of all
elements using `to_list/1` and traversing that. The main advantage of the
iterator approach is that it avoids building the complete list of all elements
in memory at once.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> Iter0 = gb_trees:iterator(Tree, ordered).
3> {a,1,Iter1} = gb_trees:next(Iter0).
4> RevIter0 = gb_trees:iterator(Tree, reversed).
5> {c,3,RevIter1} = gb_trees:next(RevIter0).
```

# `iterator_from`
*since OTP 18.0* 

```erlang
-spec iterator_from(Key, Tree) -> Iter when Tree :: tree(Key, Value), Iter :: iter(Key, Value).
```

# `iterator_from`
*since OTP 27.0* 

```erlang
-spec iterator_from(Key, Tree, Order) -> Iter
                       when
                           Tree :: tree(Key, Value),
                           Iter :: iter(Key, Value),
                           Order :: ordered | reversed.
```

Returns an iterator over the entries of `Tree` in the given `Order`, starting
from `Key` or, if absent, the first key that follows in the iteration order,
if any; see `next/1`.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3},{d,4}]).
2> Iter0 = gb_trees:iterator_from(aa, Tree, ordered).
3> {b,2,Iter1} = gb_trees:next(Iter0).
4> RevIter0 = gb_trees:iterator_from(c, Tree, reversed).
5> {c,3,RevIter1} = gb_trees:next(RevIter0).
6> {b,2,RevIter2} = gb_trees:next(RevIter1).
```

# `keys`

```erlang
-spec keys(Tree) -> [Key] when Tree :: tree(Key, Value :: term()).
```

Returns the keys in `Tree` as an ordered list.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:keys(Tree).
[a,b,c]
3> gb_trees:keys(gb_trees:empty()).
[]
```

# `larger`
*since OTP 27.0* 

```erlang
-spec larger(Key1, Tree) -> none | {Key2, Value} when Key1 :: Key, Key2 :: Key, Tree :: tree(Key, Value).
```

Returns `{Key2, Value}`, where `Key2` is the least key strictly greater than
`Key1`, and `Value` is the value associated with this key.

Returns `none` if no such pair exists.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:larger(c, Tree).
none
3> gb_trees:larger(bb, Tree).
{c,3}
4> gb_trees:larger(a, Tree).
{b,2}
```

# `largest`

```erlang
-spec largest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
```

Returns `{Key, Value}`, where `Key` is the largest key in `Tree`, and `Value` is
the value associated with this key.

Assumes that the tree is not empty.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:largest(Tree).
{c,3}
```

# `lookup`

```erlang
-spec lookup(Key, Tree) -> none | {value, Value} when Tree :: tree(Key, Value).
```

Looks up `Key` in `Tree` and returns `{value, Value}` if found, or `none` if not present.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:lookup(a, Tree).
{value,1}
3> gb_trees:lookup(z, Tree).
none
```

# `map`

```erlang
-spec map(Function, Tree1) -> Tree2
             when
                 Function :: fun((K :: Key, V1 :: Value1) -> V2 :: Value2),
                 Tree1 :: tree(Key, Value1),
                 Tree2 :: tree(Key, Value2).
```

Maps `Function(K, V1) -> V2` to all key-value pairs of tree `Tree1`,
returning a new tree `Tree2` with the same set of keys as
`Tree1` and the new set of values `V2`.

## Examples

```erlang
1> Tree0 = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> Tree1 = gb_trees:map(fun(_, V) -> 2 * V end, Tree0).
3> gb_trees:to_list(Tree1).
[{a,2},{b,4},{c,6}]
```

# `next`

```erlang
-spec next(Iter1) -> none | {Key, Value, Iter2}
              when Iter1 :: iter(Key, Value), Iter2 :: iter(Key, Value).
```

Returns `{Key, Value, Iter2}`, where `Key` is the next key referred to by
iterator `Iter1`, and `Iter2` is the new iterator to be used for traversing the
remaining nodes, or the atom `none` if no nodes remain.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> Iter0 = gb_trees:iterator(Tree).
3> {a,1,Iter1} = gb_trees:next(Iter0).
4> {b,2,Iter2} = gb_trees:next(Iter1).
5> {c,3,Iter3} = gb_trees:next(Iter2).
6> none = gb_trees:next(Iter3).
```

# `size`

```erlang
-spec size(Tree) -> non_neg_integer() when Tree :: tree().
```

Returns the number of nodes in `Tree`.

## Examples

```erlang
1> gb_trees:size(gb_trees:empty()).
0
2> gb_trees:size(gb_trees:from_list([{a,1},{b,2}])).
2
```

# `smaller`
*since OTP 27.0* 

```erlang
-spec smaller(Key1, Tree) -> none | {Key2, Value}
                 when Key1 :: Key, Key2 :: Key, Tree :: tree(Key, Value).
```

Returns `{Key2, Value}`, where `Key2` is the greatest key strictly less than
`Key1`, and `Value` is the value associated with this key.

Returns `none` if no such pair exists.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:smaller(c, Tree).
{b,2}
3> gb_trees:smaller(bb, Tree).
{b,2}
4> gb_trees:smaller(a, Tree).
none
```

# `smallest`

```erlang
-spec smallest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
```

Returns `{Key, Value}`, where `Key` is the smallest key in `Tree`, and `Value`
is the value associated with this key.

Assumes that the tree is not empty.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:smallest(Tree).
{a,1}
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(Key, Tree1) -> {Value, Tree2}
              when Tree1 :: tree(Key, _), Tree2 :: tree(Key, _), Key :: term(), Value :: term().
```

Returns a value `Value` from the node with key `Key` and a new tree `Tree2`
with that node removed.

Assumes that `Key` is present in the tree.

## Examples

```erlang
1> Tree0 = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> {Value,Tree1} = gb_trees:take(b, Tree0).
3> Value.
2
4> gb_trees:to_list(Tree1).
[{a,1},{c,3}]
```

# `take_any`
*since OTP 20.0* 

```erlang
-spec take_any(Key, Tree1) -> {Value, Tree2} | error
                  when Tree1 :: tree(Key, _), Tree2 :: tree(Key, _), Key :: term(), Value :: term().
```

Returns a value `Value` from the node with key `Key` and a new tree `Tree2`
with that node removed; returns `error` if `Key` is not present in `Tree1`.

## Examples

```erlang
1> Tree0 = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> {Value,Tree1} = gb_trees:take_any(b, Tree0).
3> Value.
2
4> gb_trees:to_list(Tree1).
[{a,1},{c,3}]
5> gb_trees:take_any(x, Tree0).
error
```

# `take_largest`

```erlang
-spec take_largest(Tree1) -> {Key, Value, Tree2}
                      when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Returns `{Key, Value, Tree2}`, where `Key` is the largest key in
`Tree1`, `Value` is the value associated with this key, and `Tree2` is
this tree with the corresponding node deleted.

Assumes that the tree is not empty.

## Examples

```erlang
1> Tree0 = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> {Key,Value,Tree1} = gb_trees:take_largest(Tree0).
3> Key.
c
4> Value.
3
5> gb_trees:to_list(Tree1).
[{a,1},{b,2}]
```

# `take_smallest`

```erlang
-spec take_smallest(Tree1) -> {Key, Value, Tree2}
                       when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Returns `{Key, Value, Tree2}`, where `Key` is the smallest key in `Tree1`,
`Value` is the value associated with that key, and `Tree2` is the tree
with the corresponding node removed.

Assumes that the tree is not empty.

## Examples

```erlang
1> Tree0 = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> {Key,Value,Tree1} = gb_trees:take_smallest(Tree0).
3> Key.
a
4> Value.
1
5> gb_trees:to_list(Tree1).
[{b,2},{c,3}]
```

# `to_list`

```erlang
-spec to_list(Tree) -> [{Key, Value}] when Tree :: tree(Key, Value).
```

Converts a tree into an ordered list of key-value tuples.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3}]).
2> gb_trees:to_list(Tree).
[{a,1},{b,2},{c,3}]
3> gb_trees:to_list(gb_trees:empty()).
[]
```

# `update`

```erlang
-spec update(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Updates `Key` to value `Value` in `Tree1` and returns the new tree.

Assumes that the key is present in the tree.

## Examples

```erlang
1> Tree1 = gb_trees:from_list([{a,1},{b,2}]).
2> Tree2 = gb_trees:update(a, 99, Tree1).
3> gb_trees:to_list(Tree2).
[{a,99},{b,2}]
```

# `values`

```erlang
-spec values(Tree) -> [Value] when Tree :: tree(Key :: term(), Value).
```

Returns the values in `Tree` as an ordered list, sorted by their
corresponding keys.

Duplicates are not removed.

## Examples

```erlang
1> Tree = gb_trees:from_list([{a,1},{b,2},{c,3},{d,1}]).
2> gb_trees:values(Tree).
[1,2,3,1]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
