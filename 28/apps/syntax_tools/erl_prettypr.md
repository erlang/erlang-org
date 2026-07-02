# `erl_prettypr`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/syntax_tools/src/erl_prettypr.erl#L37)

Pretty printing of abstract Erlang syntax trees.

This module is a front end to the pretty-printing library module `prettypr`, for
text formatting of abstract syntax trees defined by the module `erl_syntax`.

# `clause_t`
*not exported* 

```elixir
-type clause_t() ::
          case_expr | fun_expr | if_expr | maybe_expr | receive_expr | try_expr |
          {function, prettypr:document()} |
          spec.
```

# `context`
*not exported* 

```elixir
-type context() ::
          #ctxt{prec :: integer(),
                sub_indent :: non_neg_integer(),
                break_indent :: non_neg_integer(),
                clause :: clause_t() | undefined,
                hook :: hook(),
                paper :: integer(),
                ribbon :: integer(),
                user :: term(),
                encoding :: epp:source_encoding(),
                empty_lines :: sets:set(integer())}.
```

# `hook`
*not exported* 

```elixir
-type hook() :: none | fun((syntaxTree(), _, _) -> prettypr:document()).
```

# `syntaxTree`
*not exported* 

```elixir
-type syntaxTree() :: erl_syntax:syntaxTree().
```

An abstract syntax tree.

See the `m:erl_syntax` module for details.

# `best`

```elixir
-spec best(syntaxTree()) -> empty | prettypr:document().
```

# `best`

```elixir
-spec best(syntaxTree(), [term()]) -> empty | prettypr:document().
```

Creates a fixed "best" abstract layout for a syntax tree.

This is similar to the [`layout/2`](`layout/2`) function, except that
here, the final layout has been selected with respect to the given
options. The atom `empty` is returned if no such layout could be
produced. For information on the options, see the
[`format/2`](`format/2`) function.

_See also: _`best/1`, `format/2`, `layout/2`, `prettypr:best/3`.

# `format`

```elixir
-spec format(syntaxTree()) -> string().
```

# `format`

```elixir
-spec format(syntaxTree(), [term()]) -> string().
```

Prettyprint-formats an abstract Erlang syntax tree as text.

For example, if you have a `.beam` file that has been compiled with
`debug_info`, the following should print the source code for the
module (as it looks in the debug info representation):

```text
     {ok,{_,[{abstract_code,{_,AC}}]}} =
             beam_lib:chunks("myfile.beam",[abstract_code]),
     io:put_chars(erl_prettypr:format(erl_syntax:form_list(AC)))
```

Available options:

- **`{hook, none | hook()}`** - Unless the value is `none`, the
  given function is called for each node whose list of annotations is
  not empty. The default value is `none`.

- **`{paper, integer()}`** - Specifies the preferred maximum number of
  characters on any line, including indentation. The default value is 80.

- **`{ribbon, integer()}`** - Specifies the preferred maximum number of
  characters on any line, not counting indentation. The default value is 65.

- **`{user, term()}`** - User-specific data for use in hook functions. The
  default value is `undefined`.

- **`{encoding, epp:source_encoding()}`** - Specifies the encoding of the
  generated file.

A hook function (see the [`hook()`](`t:hook/0`) type) is passed the current
syntax tree node, the context, and a continuation. The context can be examined
and manipulated by functions such as [`get_ctxt_user/1`](`get_ctxt_user/1`) and
[`set_ctxt_user/2`](`set_ctxt_user/2`). The hook must return a "document" data
structure (see `layout/2` and `best/2`); this may be constructed in part or in
whole by applying the continuation function. For example, the following is a
trivial hook:

```text
      fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
```

which yields the same result as if no hook was given. The following, however:

```text
      fun (Node, Ctxt, Cont) ->
          Doc = Cont(Node, Ctxt),
          prettypr:beside(prettypr:text("<b>"),
                          prettypr:beside(Doc,
                                          prettypr:text("</b>")))
      end
```

will place the text of any annotated node (regardless of the annotation data)
between HTML "boldface begin" and "boldface end" tags.

_See also: _`m:erl_syntax`, `best/2`, `format/1`, `get_ctxt_user/1`, `layout/2`,
`set_ctxt_user/2`.

# `get_ctxt_hook`

```elixir
-spec get_ctxt_hook(context()) -> hook().
```

Returns the hook function field of the prettyprinter context.

_See also: _`set_ctxt_hook/2`.

# `get_ctxt_linewidth`

```elixir
-spec get_ctxt_linewidth(context()) -> integer().
```

Returns the line widh field of the prettyprinter context.

_See also: _`set_ctxt_linewidth/2`.

# `get_ctxt_paperwidth`

```elixir
-spec get_ctxt_paperwidth(context()) -> integer().
```

Returns the paper widh field of the prettyprinter context.

_See also: _`set_ctxt_paperwidth/2`.

# `get_ctxt_precedence`

```elixir
-spec get_ctxt_precedence(context()) -> integer().
```

Returns the operator precedence field of the prettyprinter context.

_See also: _`set_ctxt_precedence/2`.

# `get_ctxt_user`

```elixir
-spec get_ctxt_user(context()) -> term().
```

Returns the user data field of the prettyprinter context.

_See also: _`set_ctxt_user/2`.

# `layout`

```elixir
-spec layout(syntaxTree()) -> prettypr:document().
```

# `layout`

```elixir
-spec layout(syntaxTree(), [term()]) -> prettypr:document().
```

Creates an abstract document layout for a syntax tree.

The result represents a set of possible layouts (see module
`m:prettypr`). For information on the options, see `format/2`;
however, note that the `paper` and `ribbon` options are ignored by
this function.

This function provides a low-level interface to the pretty printer, returning a
flexible representation of possible layouts, independent of the paper width
eventually to be used for formatting. This can be included as part of another
document and/or further processed directly by the functions in the `prettypr`
module, or used in a hook function (see [`format/2`](`format/2`) for details).

_See also: _`m:prettypr`, `format/2`, `layout/1`.

# `set_ctxt_hook`

```elixir
-spec set_ctxt_hook(context(), hook()) -> context().
```

Updates the hook function field of the prettyprinter context.

_See also: _`get_ctxt_hook/1`.

# `set_ctxt_linewidth`

```elixir
-spec set_ctxt_linewidth(context(), integer()) -> context().
```

Updates the line widh field of the prettyprinter context.

> #### Note {: .info }
>
> Changing this value (and passing the resulting context to a
> continuation function) does not affect the normal formatting, but may
> affect user-defined behaviour in hook functions.

_See also: _`get_ctxt_linewidth/1`.

# `set_ctxt_paperwidth`

```elixir
-spec set_ctxt_paperwidth(context(), integer()) -> context().
```

Updates the paper widh field of the prettyprinter context.

> #### Note {: .info }
>
> Changing this value (and passing the resulting context to a
> continuation function) does not affect the normal formatting, but may
> affect user-defined behaviour in hook functions.

_See also: _`get_ctxt_paperwidth/1`.

# `set_ctxt_precedence`

```elixir
-spec set_ctxt_precedence(context(), integer()) -> context().
```

Updates the operator precedence field of the prettyprinter context.

See the [`//stdlib/erl_parse`](`m:erl_parse`) module for operator
precedences.

_See also: _[//stdlib/erl_parse](`m:erl_parse`), `get_ctxt_precedence/1`.

# `set_ctxt_user`

```elixir
-spec set_ctxt_user(context(), term()) -> context().
```

Updates the user data field of the prettyprinter context.

_See also: _`get_ctxt_user/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
