# `epp_dodger`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/syntax_tools/src/epp_dodger.erl#L70)

Bypassing the Erlang preprocessor.

This module tokenises and parses most Erlang source code without expanding
preprocessor directives and macro applications, as long as these are
syntactically "well-behaved". Because the normal parse trees of the `erl_parse`
module cannot represent these things (normally, they are expanded by the Erlang
preprocessor [`//stdlib/epp`](`m:epp`) before the parser sees them), an extended
syntax tree is created, using the `m:erl_syntax` module.

# `errorinfo`
*not exported* 

```elixir
-type errorinfo() :: erl_scan:error_info().
```

# `option`
*not exported* 

```elixir
-type option() :: atom() | {atom(), term()}.
```

# `parse`

```elixir
-spec parse(file:io_device()) -> {ok, erl_syntax:forms()}.
```

# `parse`

```elixir
-spec parse(file:io_device(), erl_anno:location()) -> {ok, erl_syntax:forms()}.
```

# `parse`

```elixir
-spec parse(file:io_device(), erl_anno:location(), [option()]) -> {ok, erl_syntax:forms()}.
```

Reads and parses program text from an I/O stream.

Characters are read from `IODevice` until end-of-file; apart from
this, the behavior is the same as for `parse_file/2`. `StartLocation`
is the initial location.

_See also: _`parse/2`, `parse_file/2`, `parse_form/2`, `quick_parse/3`.

# `parse_file`

```elixir
-spec parse_file(file:filename()) -> {ok, erl_syntax:forms()} | {error, errorinfo()}.
```

# `parse_file`

```elixir
-spec parse_file(file:filename(), [option()]) -> {ok, erl_syntax:forms()} | {error, errorinfo()}.
```

Reads and parses a file.

If successful, `{ok, Forms}` is returned, where `Forms` is a list of
abstract syntax trees representing the "program forms" of the file
(see `erl_syntax:is_form/1`). Otherwise, `{error, errorinfo()}` is
returned, typically if the file could not be opened. Note that parse
errors show up as error markers in the returned list of forms; they do
not cause this function to fail or return `{error, errorinfo()}`.

Options:

- **`{no_fail, boolean()}`** - If `true`, this makes `epp_dodger` replace any
  program forms that could not be parsed with nodes of type `text` (see
  `erl_syntax:text/1`), representing the raw token sequence of the form, instead
  of reporting a parse error. The default value is `false`.

- **`{clever, boolean()}`** - If set to `true`, this makes `epp_dodger` try to
  repair the source code as it seems fit, in certain cases where parsing would
  otherwise fail. Currently, it inserts `++` operators between string literals
  and macros where it looks like concatenation was intended. The default value
  is `false`.

_See also: _`parse/2`, `quick_parse_file/1`, `erl_syntax:is_form/1`.

# `parse_form`

```elixir
-spec parse_form(file:io_device(), erl_anno:location()) ->
                    {ok, erl_syntax:forms(), erl_anno:location()} |
                    {eof, erl_anno:location()} |
                    {error, errorinfo(), erl_anno:location()}.
```

# `parse_form`

```elixir
-spec parse_form(file:io_device(), erl_anno:location(), [option()]) ->
                    {ok, erl_syntax:forms(), erl_anno:location()} |
                    {eof, erl_anno:location()} |
                    {error, errorinfo(), erl_anno:location()}.
```

Reads and parses a single program form from an I/O stream.

Characters are read from `IODevice` until an end-of-form marker is
found (a period character followed by whitespace), or until
end-of-file; apart from this, the behavior is similar to that of
[`parse/3`](`parse/3`), except that the return values also contain the
final location given that `StartLocation` is the initial location, and
that `{eof, Location}` may be returned.

_See also: _`parse/3`, `parse_form/2`, `quick_parse_form/3`.

# `quick_parse`

```elixir
-spec quick_parse(file:io_device()) -> {ok, erl_syntax:forms()}.
```

# `quick_parse`

```elixir
-spec quick_parse(file:io_device(), erl_anno:location()) -> {ok, erl_syntax:forms()}.
```

# `quick_parse`

```elixir
-spec quick_parse(file:io_device(), erl_anno:location(), [option()]) -> {ok, erl_syntax:forms()}.
```

Similar to `parse/3`, but does a more quick-and-dirty processing of the code.

See `quick_parse_file/2` for details.

_See also: _`parse/3`, `quick_parse/2`, `quick_parse_file/2`,
`quick_parse_form/2`.

# `quick_parse_file`

```elixir
-spec quick_parse_file(file:filename()) -> {ok, erl_syntax:forms()} | {error, errorinfo()}.
```

# `quick_parse_file`

```elixir
-spec quick_parse_file(file:filename(), [option()]) -> {ok, erl_syntax:forms()} | {error, errorinfo()}.
```

Similar to `parse_file/2`, but does a more quick-and-dirty processing of the
code.

Macro definitions and other preprocessor directives are discarded, and all
macro calls are replaced with atoms. This is useful when only the main structure
of the code is of interest, and not the details. Furthermore, the quick-parse
method can usually handle more strange cases than the normal, more exact
parsing.

Options: see `parse_file/2`. However, note that for
[`quick_parse_file/2`](`quick_parse_file/2`), the option `no_fail` is `true` by
default.

_See also: _`parse_file/2`, `quick_parse/2`.

# `quick_parse_form`

```elixir
-spec quick_parse_form(file:io_device(), erl_anno:location()) ->
                          {ok, erl_syntax:forms(), erl_anno:location()} |
                          {eof, erl_anno:location()} |
                          {error, errorinfo(), erl_anno:location()}.
```

# `quick_parse_form`

```elixir
-spec quick_parse_form(file:io_device(), erl_anno:location(), [option()]) ->
                          {ok, erl_syntax:forms(), erl_anno:location()} |
                          {eof, erl_anno:location()} |
                          {error, errorinfo(), erl_anno:location()}.
```

Similar to `parse_form/3`, but does a more quick-and-dirty processing of the
code. See `quick_parse_file/2` for details.

_See also: _`parse/3`, `parse_form/3`, `quick_parse_form/2`.

# `tokens_to_string`

```elixir
-spec tokens_to_string([term()]) -> string().
```

Generates a string corresponding to the given token sequence.

The string can be re-tokenized to yield the same token list again.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
