# `cerl_trees`
[🔗](https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl_trees.erl#L25)

Basic functions on Core Erlang abstract syntax trees.

> #### Note {: .info }
>
> The public interface of the Erlang compiler can be found in
> module `m:compile`.
>
> This module is an internal part of the compiler. Its API is not guaranteed
> to remain compatible between releases.

Syntax trees are defined in the module `m:cerl`.

# `cerl`
*not exported* 

```erlang
-type cerl() :: cerl:cerl().
```

# `depth`

```erlang
-spec depth(Tree :: cerl()) -> non_neg_integer().
```

Returns the length of the longest path in the tree.

A leaf node has depth zero, the tree representing "`{foo, bar}`" has
depth one, and so on.

# `fold`

```erlang
-spec fold(Function :: fun((cerl(), term()) -> term()), Unit :: term(), Term :: cerl()) -> term().
```

Does a fold operation over the nodes of the tree.

The result is the value of `Function(X1, Function(X2, ... Function(Xn,
Unit) ... ))`, where `X1, ..., Xn` are the nodes of `Tree` in a
post-order traversal.

_See also: _`mapfold/3`.

# `free_variables`

```erlang
-spec free_variables(Tree :: cerl()) -> [cerl:var_name()].
```

Like [`variables/1`](`variables/1`), but only includes variables that are free
in the tree.

_See also: _`next_free_variable_name/1`, `variables/1`.

# `get_label`

```erlang
-spec get_label(Tree :: cerl()) -> top | integer().
```

Retrieves the label for `Tree`.

An exception is thrown if `Tree` does not have a label, or if `Tree`
does not represent a well-formed Core Erlang syntax tree.

# `label`

```erlang
-spec label(cerl()) -> {cerl(), integer()}.
```

Equivalent to [label(Tree, 0)](`label/2`).

# `label`

```erlang
-spec label(Tree :: cerl(), N :: integer()) -> {cerl(), integer()}.
```

Labels each expression in the tree.

A term `{label, L}` is prefixed to the annotation list of each
expression node, where L is a unique number for every node, except for
variables (and function name variables) which get the same label if
they represent the same variable. Constant literal nodes are not
labeled.

The returned value is a tuple `{NewTree, Max}`, where `NewTree` is the labeled
tree and `Max` is 1 plus the largest label value used. All previous annotation
terms of the form `{label, X}` are deleted.

The values of L used in the tree is a dense range from `N` to `Max - 1`, where
`N =< Max =< N + size(Tree)`. Note that it is possible that no labels are used
at all, i.e., `N = Max`.

Note: All instances of free variables will be given distinct labels.

_See also: _`label/1`, `size/1`.

# `map`

```erlang
-spec map(Function :: fun((cerl()) -> cerl()), Tree :: cerl()) -> cerl().
```

Maps a function onto the nodes of a tree.

This replaces each node in the tree by the result of applying the
given function on the original node, bottom-up.

_See also: _`mapfold/3`.

# `mapfold`

```erlang
-spec mapfold(Function :: fun((cerl(), term()) -> {cerl(), term()}), Initial :: term(), Tree :: cerl()) ->
                 {cerl(), term()}.
```

Does a combined map/fold operation on the nodes of the tree.

This is similar to [`map/2`](`map/2`), but also propagates a value
from each application of `Function` to the next, starting with the
given value `Initial`, while doing a post-order traversal of the tree,
much like [`fold/3`](`fold/3`).

This is equivalent to `mapfold/4` with an identity function as the
pre-operation.

_See also:_ `fold/3`, `map/2`, `mapfold/4`.

# `mapfold`

```erlang
-spec mapfold(Pre :: fun((cerl(), term()) -> {cerl(), term()} | skip),
              Post :: fun((cerl(), term()) -> {cerl(), term()}),
              Initial :: term(),
              Tree :: cerl()) ->
                 {cerl(), term()}.
```

Does a combined map/fold operation on the nodes of the tree.

It begins by calling `Pre` on the tree, using the `Initial`
value. `Pre` must either return a tree with an updated accumulator or
the atom `skip`.

If a tree is returned, this function deconstructs the top node of the returned
tree and recurses on the children, using the returned value as the new initial
and carrying the returned values from one call to the next. Finally it
reassembles the top node from the children, calls `Post` on it and returns the
result.

If `skip` is returned, it returns the tree and accumulator as is.

# `next_free_variable_name`

```erlang
-spec next_free_variable_name(Tree :: cerl()) -> integer().
```

Returns a integer variable name higher than any other integer variable name in
the syntax tree.

An exception is thrown if `Tree` does not represent a well-formed Core
Erlang syntax tree.

_See also: _`free_variables/1`, `variables/1`.

# `size`

```erlang
-spec size(Tree :: cerl()) -> non_neg_integer().
```

Returns the number of nodes in `Tree`.

# `variables`

```erlang
-spec variables(Tree :: cerl()) -> [cerl:var_name()].
```

Returns an ordered-set list of the names of all variables in the syntax tree
(including function-name variables.)

An exception is thrown if `Tree` does not represent a well-formed Core
Erlang syntax tree.

_See also: _`free_variables/1`, `next_free_variable_name/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
