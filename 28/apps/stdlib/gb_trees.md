# `gb_trees`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/gb_trees.erl#L140)

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

## See Also

`m:dict`, `m:gb_sets`

# `iter`

```elixir
-type iter() :: iter(_, _).
```

# `iter`

```elixir
-opaque iter(Key, Value) :: {ordered | reversed, [gb_tree_node(Key, Value)]}.
```

A general balanced tree iterator.

# `tree`

```elixir
-type tree() :: tree(_, _).
```

# `tree`

```elixir
-opaque tree(Key, Value) :: {non_neg_integer(), gb_tree_node(Key, Value)}.
```

A general balanced tree.

# `balance`

```elixir
-spec balance(Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Rebalances `Tree1`.

Notice that this is rarely necessary, but can be motivated
when many nodes have been deleted from the tree without further insertions.
Rebalancing can then be forced to minimize lookup times, as deletion does not
rebalance the tree.

# `delete`

```elixir
-spec delete(Key, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Removes the node with key `Key` from `Tree1` and returns the new tree. Assumes
that the key is present in the tree, crashes otherwise.

# `delete_any`

```elixir
-spec delete_any(Key, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Removes the node with key `Key` from `Tree1` if the key is present in the tree,
otherwise does nothing. Returns the new tree.

# `empty`

```elixir
-spec empty() -> tree(none(), none()).
```

Returns a new empty tree.

# `enter`

```elixir
-spec enter(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Inserts `Key` with value `Value` into `Tree1` if the key is not present in the
tree, otherwise updates `Key` to value `Value` in `Tree1`. Returns the new tree.

# `from_orddict`

```elixir
-spec from_orddict(List) -> Tree when List :: [{Key, Value}], Tree :: tree(Key, Value).
```

Turns an ordered list `List` of key-value tuples into a tree. The list must not
contain duplicate keys.

# `get`

```elixir
-spec get(Key, Tree) -> Value when Tree :: tree(Key, Value).
```

Retrieves the value stored with `Key` in `Tree`. Assumes that the key is present
in the tree, crashes otherwise.

# `insert`

```elixir
-spec insert(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Inserts `Key` with value `Value` into `Tree1` and returns the new tree. Assumes
that the key is not present in the tree, crashes otherwise.

# `is_defined`

```elixir
-spec is_defined(Key, Tree) -> boolean() when Tree :: tree(Key, Value :: term()).
```

Returns `true` if `Key` is present in `Tree`, otherwise `false`.

# `is_empty`

```elixir
-spec is_empty(Tree) -> boolean() when Tree :: tree().
```

Returns `true` if `Tree` is an empty tree, othwewise `false`.

# `iterator`

```elixir
-spec iterator(Tree) -> Iter when Tree :: tree(Key, Value), Iter :: iter(Key, Value).
```

Returns an iterator that can be used for traversing the entries of `Tree`; see
`next/1`.

Equivalent to [`iterator(Tree, ordered)`](`iterator/2`).

# `iterator`
*since OTP 27.0* 

```elixir
-spec iterator(Tree, Order) -> Iter
                  when Tree :: tree(Key, Value), Iter :: iter(Key, Value), Order :: ordered | reversed.
```

Returns an iterator that can be used for traversing the entries of `Tree` in
either `ordered` or `reversed` direction; see `next/1`.

The implementation of this is very efficient; traversing the whole tree using
[`next/1`](`next/1`) is only slightly slower than getting the list of all
elements using `to_list/1` and traversing that. The main advantage of the
iterator approach is that it does not require the complete list of all elements
to be built in memory at one time.

# `iterator_from`
*since OTP 18.0* 

```elixir
-spec iterator_from(Key, Tree) -> Iter when Tree :: tree(Key, Value), Iter :: iter(Key, Value).
```

Returns an iterator that can be used for traversing the entries of `Tree`; see
`next/1`. The difference as compared to the iterator returned by `iterator/1` is
that the iterator starts with the first key greater than or equal to `Key`.

Equivalent to [`iterator_from(Key, Tree, ordered)`](`iterator_from/3`).

# `iterator_from`
*since OTP 27.0* 

```elixir
-spec iterator_from(Key, Tree, Order) -> Iter
                       when
                           Tree :: tree(Key, Value),
                           Iter :: iter(Key, Value),
                           Order :: ordered | reversed.
```

Returns an iterator that can be used for traversing the entries of `Tree` in
either `ordered` or `reversed` direction; see `next/1`. The difference as
compared to the iterator returned by `iterator/2` is that the iterator starts
with the first key next to or equal to `Key`.

# `keys`

```elixir
-spec keys(Tree) -> [Key] when Tree :: tree(Key, Value :: term()).
```

Returns the keys in `Tree` as an ordered list.

# `larger`
*since OTP 27.0* 

```elixir
-spec larger(Key1, Tree) -> none | {Key2, Value} when Key1 :: Key, Key2 :: Key, Tree :: tree(Key, Value).
```

Returns `{Key2, Value}`, where `Key2` is the least key strictly greater than
`Key1`, `Value` is the value associated with this key.

Returns `none` if no such pair exists.

# `largest`

```elixir
-spec largest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
```

Returns `{Key, Value}`, where `Key` is the largest key in `Tree`, and `Value` is
the value associated with this key. Assumes that the tree is not empty.

# `lookup`

```elixir
-spec lookup(Key, Tree) -> none | {value, Value} when Tree :: tree(Key, Value).
```

Looks up `Key` in `Tree`. Returns `{value, Value}`, or `none` if `Key` is not
present.

# `map`

```elixir
-spec map(Function, Tree1) -> Tree2
             when
                 Function :: fun((K :: Key, V1 :: Value1) -> V2 :: Value2),
                 Tree1 :: tree(Key, Value1),
                 Tree2 :: tree(Key, Value2).
```

Maps function F(K, V1) -> V2 to all key-value pairs of tree `Tree1`. Returns a
new tree `Tree2` with the same set of keys as `Tree1` and the new set of values
`V2`.

# `next`

```elixir
-spec next(Iter1) -> none | {Key, Value, Iter2}
              when Iter1 :: iter(Key, Value), Iter2 :: iter(Key, Value).
```

Returns `{Key, Value, Iter2}`, where `Key` is the next key referred to by
iterator `Iter1`, and `Iter2` is the new iterator to be used for traversing the
remaining nodes, or the atom `none` if no nodes remain.

# `size`

```elixir
-spec size(Tree) -> non_neg_integer() when Tree :: tree().
```

Returns the number of nodes in `Tree`.

# `smaller`
*since OTP 27.0* 

```elixir
-spec smaller(Key1, Tree) -> none | {Key2, Value}
                 when Key1 :: Key, Key2 :: Key, Tree :: tree(Key, Value).
```

Returns `{Key2, Value}`, where `Key2` is the greatest key strictly less than
`Key1`, `Value` is the value associated with this key.

Returns `none` if no such pair exists.

# `smallest`

```elixir
-spec smallest(Tree) -> {Key, Value} when Tree :: tree(Key, Value).
```

Returns `{Key, Value}`, where `Key` is the smallest key in `Tree`, and `Value`
is the value associated with this key. Assumes that the tree is not empty.

# `take`
*since OTP 20.0* 

```elixir
-spec take(Key, Tree1) -> {Value, Tree2}
              when Tree1 :: tree(Key, _), Tree2 :: tree(Key, _), Key :: term(), Value :: term().
```

Returns a value `Value` from node with key `Key` and new `Tree2` without the
node with this value. Assumes that the node with key is present in the tree,
crashes otherwise.

# `take_any`
*since OTP 20.0* 

```elixir
-spec take_any(Key, Tree1) -> {Value, Tree2} | error
                  when Tree1 :: tree(Key, _), Tree2 :: tree(Key, _), Key :: term(), Value :: term().
```

Returns a value `Value` from node with key `Key` and new `Tree2` without the
node with this value. Returns `error` if the node with the key is not present in
the tree.

# `take_largest`

```elixir
-spec take_largest(Tree1) -> {Key, Value, Tree2}
                      when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Returns `{Key, Value, Tree2}`, where `Key` is the largest key in `Tree1`,
`Value` is the value associated with this key, and `Tree2` is this tree with the
corresponding node deleted. Assumes that the tree is not empty.

# `take_smallest`

```elixir
-spec take_smallest(Tree1) -> {Key, Value, Tree2}
                       when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Returns `{Key, Value, Tree2}`, where `Key` is the smallest key in `Tree1`,
`Value` is the value associated with this key, and `Tree2` is this tree with the
corresponding node deleted. Assumes that the tree is not empty.

# `to_list`

```elixir
-spec to_list(Tree) -> [{Key, Value}] when Tree :: tree(Key, Value).
```

Converts a tree into an ordered list of key-value tuples.

# `update`

```elixir
-spec update(Key, Value, Tree1) -> Tree2 when Tree1 :: tree(Key, Value), Tree2 :: tree(Key, Value).
```

Updates `Key` to value `Value` in `Tree1` and returns the new tree. Assumes that
the key is present in the tree.

# `values`

```elixir
-spec values(Tree) -> [Value] when Tree :: tree(Key :: term(), Value).
```

Returns the values in `Tree` as an ordered list, sorted by their corresponding
keys. Duplicates are not removed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
