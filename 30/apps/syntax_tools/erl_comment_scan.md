# `erl_comment_scan`
[🔗](https://github.com/erlang/otp/blob/master/lib/syntax_tools/src/erl_comment_scan.erl#L33)

Functions for reading comment lines from Erlang source code.

# `comment`

```erlang
-type comment() ::
          {Line :: integer(), Column :: integer(), Indentation :: integer(), Text :: [string()]}.
```

# `commentLine`
*not exported* 

```erlang
-type commentLine() :: {Line :: integer(), Column :: integer(), Indent :: integer(), Text :: string()}.
```

# `file`

```erlang
-spec file(file:filename()) -> [comment()].
```

Extracts comments from an Erlang source code file.

Returns a list of entries representing _multi-line_ comments, listed
in order of increasing line-numbers.  For each entry, `Text` is a list
of strings representing the consecutive comment lines in top-down
order; the strings contain _all_ characters following (but not
including) the first comment-introducing `%` character on the line, up
to (but not including) the line-terminating newline.

Furthermore, `Line` is the line number and `Column` the left column of the
comment (that is, the column of the comment-introducing `%` character). `Indent` is
the indentation (or padding), measured in character positions between the last
non-whitespace character before the comment (or the left margin), and the left
column of the comment. `Line` and `Column` are always positive integers, and
`Indentation` is a nonnegative integer.

Evaluation exits with reason `{read, Reason}` if a read error occurred, where
`Reason` is an atom corresponding to a Posix error code; see the module
[`//kernel/file`](`m:file`) for details.

# `join_lines`

```erlang
-spec join_lines([commentLine()]) -> [comment()].
```

join_lines(CommentLines) ->

Joins individual comment lines into multi-line comments.

The input is a list of entries representing individual comment lines,
_in order of decreasing line-numbers_; see `scan_lines/1` for
details. The result is a list of entries representing _multi-line_
comments, _still listed in order of decreasing line-numbers_, but
where for each entry, `Text` is a list of consecutive comment lines in
order of _increasing_ line-numbers (that is, top-down).

_See also: _`scan_lines/1`.

# `scan_lines`

```erlang
-spec scan_lines(string()) -> [commentLine()].
```

Extracts individual comment lines from a source code string.

Returns a list of comment lines found in the text, listed in order of
_decreasing_ line-numbers, that is, the last comment line in the input
is first in the resulting list. `Text` is a single string, containing
all characters following (but not including) the first
comment-introducing `%` character on the line, up to (but not
including) the line-terminating newline. For details on `Line`,
`Column` and `Indent`, see `file/1`.

# `string`

```erlang
-spec string(string()) -> [comment()].
```

Extracts comments from a string containing Erlang source code.

Except for reading directly from a string, the behavior is the same
as for `file/1`.

_See also: _`file/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
