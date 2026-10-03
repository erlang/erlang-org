# `cerl_clauses`
[🔗](https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl_clauses.erl#L25)

Utility functions for Core Erlang case/receive clauses.

> #### Note {: .info }
>
> The documentation of the public interface for the Erlang compiler can be
> found in module `m:compile`.
>
> This module is an internal part of the compiler. Its API is not guaranteed
> to remain compatible between releases.

Syntax trees are defined in the module `m:cerl`.

# `bindings`
*not exported* 

```erlang
-type bindings() :: [{cerl(), cerl()}].
```

# `cerl`
*not exported* 

```erlang
-type cerl() :: cerl:cerl().
```

# `expr`
*not exported* 

```erlang
-type expr() :: any | cerl().
```

# `match_ret`
*not exported* 

```erlang
-type match_ret() :: none | {true, bindings()} | {false, bindings()}.
```

# `any_catchall`

```erlang
-spec any_catchall(Clauses :: [cerl()]) -> boolean().
```

Returns `true` if any of the abstract clauses in the list is a catch-all,
otherwise `false`.

See [`is_catchall/1`](`is_catchall/1`) for details.

Note: each node in `Clauses` must have type `clause`.

_See also: _`is_catchall/1`.

# `eval_guard`

```erlang
-spec eval_guard(Expr :: cerl()) -> none | {value, term()}.
```

Tries to reduce a guard expression to a single constant value, if possible.

The returned value is `{value, Term}` if the guard expression `Expr`
always yields the constant value `Term`, and is otherwise `none`.

Note that although guard expressions should only yield boolean values, this
function does not guarantee that `Term` is either `true` or `false`. Also note
that only simple constructs like let-expressions are examined recursively;
general constant folding is not performed.

_See also: _`is_catchall/1`.

# `is_catchall`

```erlang
-spec is_catchall(Clause :: cerl:c_clause()) -> boolean().
```

Returns `true` if an abstract clause is a catch-all, otherwise `false`.

A clause is a catch-all if all its patterns are variables, and its
guard expression always evaluates to `true`;
cf. [`eval_guard/1`](`eval_guard/1`).

Note: `Clause` must have type `clause`.

_See also: _`any_catchall/1`, `eval_guard/1`.

# `match`

```erlang
-spec match(Pattern :: cerl(), Expr :: expr()) -> match_ret().
```

Matches a pattern against an expression.

The returned value is `none` if a match is impossible, `{true,
Bindings}` if `Pattern` definitely matches `Expr`, and `{false,
Bindings}` if a match is not definite, but cannot be excluded.
`Bindings` is then a list of pairs `{Var, SubExpr}`, associating each
variable in the pattern with either the corresponding subexpression of
`Expr`, or with the atom `any` if no matching subexpression
exists. (Recall that variables may not be repeated in a Core Erlang
pattern.) The list of bindings is given in innermost-first order; this
should only be of interest if `Pattern` contains one or more alias
patterns. If the returned value is `{true, []}`, it implies that the
pattern and the expression are syntactically identical.

Instead of a syntax tree, the atom `any` can be passed for `Expr` (or, more
generally, be used for any subtree of `Expr`, in as much the abstract syntax
tree implementation allows it); this means that it cannot be decided whether the
pattern will match or not, and the corresponding variable bindings will all map
to `any`. The typical use is for producing bindings for `receive` clauses.

Note: Binary-syntax patterns are never structurally matched against
binary-syntax expressions by this function.

Examples:

- Matching a pattern "`{X, Y}`" against the expression "`{foo, f(Z)}`" yields
  `{true, Bindings}` where `Bindings` associates "`X`" with the subtree "`foo`"
  and "`Y`" with the subtree "`f(Z)`".
- Matching pattern "`{X, {bar, Y}}`" against expression "`{foo, f(Z)}`" yields
  `{false, Bindings}` where `Bindings` associates "`X`" with the subtree "`foo`"
  and "`Y`" with `any` (because it is not known if "`{foo, Y}`" might match the
  run-time value of "`f(Z)`" or not).
- Matching pattern "`{foo, bar}`" against expression "`{foo, f()}`" yields
  `{false, []}`, telling us that there might be a match, but we cannot deduce
  any bindings.
- Matching `{foo, X = {bar, Y}}` against expression "`{foo, {bar, baz}}`" yields
  `{true, Bindings}` where `Bindings` associates "`Y`" with "`baz`", and "`X`"
  with "`{bar, baz}`".
- Matching a pattern "`{X, Y}`" against `any` yields `{false, Bindings}` where
  `Bindings` associates both "`X`" and "`Y`" with `any`.

# `match_list`

```erlang
-spec match_list(Patterns :: [cerl()], Exprs :: [expr()]) -> match_ret().
```

Like [`match/2`](`match/2`), but matching a sequence of patterns against a
sequence of expressions.

Passing an empty list for `Exprs` is equivalent to passing a list of
`any` atoms of the same length as `Patterns`.

_See also: _`match/2`.

# `reduce`

```erlang
-spec reduce([cerl:c_clause()]) -> {true, {cerl:c_clause(), bindings()}} | {false, [cerl:c_clause()]}.
```

Equivalent to [reduce(Cs, [])](`reduce/2`).

# `reduce`

```erlang
-spec reduce(Clauses :: [cerl:c_clause()], Exprs :: [expr()]) ->
                {true, {cerl:c_clause(), bindings()}} | {false, [cerl:c_clause()]}.
```

Selects a single clause, if possible, or otherwise reduces the list of
selectable clauses.

The input is a list `Clauses` of abstract clauses (i.e.,
syntax trees of type `clause`), and a list of switch expressions `Exprs`. The
function tries to uniquely select a single clause or discard unselectable
clauses, with respect to the switch expressions. All abstract clauses in the
list must have the same number of patterns. If `Exprs` is not the empty list, it
must have the same length as the number of patterns in each clause; see
[`match_list/2`](`match_list/2`) for details.

A clause can only be selected if its guard expression always yields the atom
`true`, and a clause whose guard expression always yields the atom `false` can
never be selected. Other guard expressions are considered to have unknown value;
cf. [`eval_guard/1`](`eval_guard/1`).

If a particular clause can be selected, the function returns
`{true, {Clause, Bindings}}`, where `Clause` is the selected clause and
`Bindings` is a list of pairs `{Var, SubExpr}` associating the variables
occurring in the patterns of `Clause` with the corresponding subexpressions in
`Exprs`. The list of bindings is given in innermost-first order; see the
[`match/2`](`match/2`) function for details.

If no clause could be definitely selected, the function returns
`{false, NewClauses}`, where `NewClauses` is the list of entries in `Clauses`
that remain after eliminating unselectable clauses, preserving the relative
order.

_See also: _`eval_guard/1`, `match/2`, `match_list/2`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
