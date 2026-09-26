# `erl_recomment`
[🔗](https://github.com/erlang/otp/blob/master/lib/syntax_tools/src/erl_recomment.erl#L33)

Inserting comments into abstract Erlang syntax trees

This module contains functions for inserting comments, described by position,
indentation and text, as attachments on an abstract syntax tree, at the correct
places.

# `syntaxTree`
*not exported* 

```erlang
-type syntaxTree() :: erl_syntax:syntaxTree().
```

An abstract syntax tree. See the `m:erl_syntax` module for details.

# `quick_recomment_forms`

```erlang
-spec quick_recomment_forms(erl_syntax:forms(), [erl_comment_scan:comment()]) -> syntaxTree().
```

Like `recomment_forms/2`, but only inserts top-level comments.

Comments within function definitions or declarations ("forms") are
simply ignored.

# `recomment_forms`

```erlang
-spec recomment_forms(erl_syntax:forms(), [erl_comment_scan:comment()]) -> syntaxTree().
```

Attaches comments to the syntax tree/trees representing a program.

The given `Forms` should be a single syntax tree of type `form_list`,
or a list of syntax trees representing "program forms". The syntax
trees must contain valid position information (for details, see
[`recomment_tree/2`](`recomment_tree/2`)). The result is a
corresponding syntax tree of type `form_list` in which all comments in
the list `Comments` have been attached at the proper places.

Assuming `Forms` represents a program (or any sequence of "program forms"), any
comments whose first lines are not directly associated with a specific program
form will become standalone comments inserted between the neighbouring program
forms. Furthermore, comments whose column position is less than or equal to one
will not be attached to a program form that begins at a conflicting line number
(this can happen with preprocessor-generated `line`\-attributes).

If `Forms` is a syntax tree of some other type than `form_list`, the comments
will be inserted directly using [`recomment_tree/2`](`recomment_tree/2`), and
any comments left over from that process are added as postcomments on the
result.

Entries in `Comments` represent multi-line comments. For each entry, `Line` is
the line number and `Column` the left column of the comment (the column of the
first comment-introducing "`%`" character). `Indentation` is the number of
character positions between the last non-whitespace character before the comment
(or the left margin) and the left column of the comment. `Text` is a list of
strings representing the consecutive comment lines in top-down order, where each
string contains all characters following (but not including) the
comment-introducing "`%`" and up to (but not including) the terminating newline.
(see module `m:erl_comment_scan`.)

Evaluation exits with reason `{bad_position, Pos}` if the associated position
information `Pos` of some subtree in the input does not have a recognizable
format, or with reason `{bad_tree, L, C}` if insertion of a comment at line `L`,
column `C`, fails because the tree structure is ill-formed.

_See also: _`m:erl_comment_scan`, `quick_recomment_forms/2`, `recomment_tree/2`.

# `recomment_tree`

```erlang
-spec recomment_tree(syntaxTree(), [erl_comment_scan:comment()]) ->
                        {syntaxTree(), [erl_comment_scan:comment()]}.
```

Attaches comments to a syntax tree.

The result is a pair `{NewTree, Remainder}` where `NewTree` is the
given `Tree` where comments from the list `Comments` have been
attached at the proper places. `Remainder` is the list of entries in
`Comments` which have not been inserted, because their line numbers
are greater than those of any node in the tree. The entries in
`Comments` are inserted in order; if two comments become attached to
the same node, they will appear in the same order in the program text.

The nodes of the syntax tree must contain valid position information. This can
be single integers, assumed to represent a line number, or 2- or 3-tuples where
the first or second element is an integer, in which case the leftmost integer
element is assumed to represent the line number. Line numbers less than one are
ignored (usually, the default line number for newly created nodes is zero).

For details on the `Line`, `Column` and `Indentation` fields, and the behaviour
in case of errors, see [`recomment_forms/2`](`recomment_forms/2`).

_See also: _`recomment_forms/2`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
