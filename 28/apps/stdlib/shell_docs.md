# `shell_docs`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/shell_docs.erl#L22)

Functions used to render [EEP-48](`e:kernel:eep48_chapter.md`) style documentation for a shell.

This module can be used to render function and type documentation to be printed
in a shell. This is the module that is used to render the documentation accessed through
the shell through [`c:h/1,2,3`](`\\c:h/1`). Example:

```txt
1> h(maps,new,0).

  -spec new() -> Map when Map :: #{}.

Since:
  OTP 17.0

  Returns a new empty map.

  Example:

    > maps:new().
    #{}
```

This module formats and renders EEP-48 documentation of the format
`application/erlang+html`. For more information about this format see
[Documentation Storage](`e:edoc:doc_storage.md`) in EDoc's User's
Guide. It can also render any other format of "text" type, although those will
be rendered as is.

# `chunk_element`
*not exported* *since OTP 23.0* 

```elixir
-type chunk_element() :: {chunk_element_type(), chunk_element_attrs(), chunk_elements()} | binary().
```

# `chunk_element_attr`
*since OTP 23.0* 

```elixir
-type chunk_element_attr() :: {atom(), unicode:chardata()}.
```

# `chunk_element_attrs`
*not exported* *since OTP 23.0* 

```elixir
-type chunk_element_attrs() :: [chunk_element_attr()].
```

# `chunk_element_block_type`
*not exported* *since OTP 23.0* 

```elixir
-type chunk_element_block_type() ::
          p | 'div' | blockquote | br | pre | ul | ol | li | dl | dt | dd | h1 | h2 | h3 | h4 | h5 | h6.
```

# `chunk_element_inline_type`
*not exported* *since OTP 23.0* 

```elixir
-type chunk_element_inline_type() :: a | code | em | strong | i | b.
```

# `chunk_element_type`
*not exported* *since OTP 23.0* 

```elixir
-type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type().
```

The HTML tags allowed in `application/erlang+html`.

# `chunk_elements`
*since OTP 23.0* 

```elixir
-type chunk_elements() :: [chunk_element()].
```

# `config`
*not exported* *since OTP 23.2* 

```elixir
-type config() :: #{encoding => unicode | latin1, columns => pos_integer(), ansi => boolean()}.
```

The configuration of how the documentation should be rendered.

- **encoding** - Configure the encoding that should be used by the renderer for
  graphical details such as bullet-points. By default `shell_docs` uses the
  value returned by [`io:getopts()`](`io:getopts/0`).

- **ansi** - Configure whether
  [ansi escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code) should be
  used to render graphical details such as bold and underscore. By default
  `shell_docs` will try to determine if the receiving shell supports ansi escape
  codes. It is possible to override the automated check by setting the kernel
  configuration parameter `shell_docs_ansi` to a `t:boolean/0` value.

- **columns** - Configure how wide the target documentation should be rendered.
  By default `shell_docs` used the value returned by
  [`io:columns()`](`io:columns/0`). It is possible to override this default
  by setting the stdlib configuration parameter `shell_docs_columns`
  to a `t:pos_integer/0` value.

# `docs_v1`
*not exported* *since OTP 23.0* 

```elixir
-type docs_v1() ::
          #docs_v1{anno :: term(),
                   beam_language :: term(),
                   format :: term(),
                   module_doc :: term(),
                   metadata :: term(),
                   docs :: term()}.
```

The record holding EEP-48 documentation for a module. You can use
`code:get_doc/1` to fetch this information from a module.

# `normalize`
*since OTP 23.0* 

```elixir
-spec normalize(Docs) -> NormalizedDocs
                   when Docs :: chunk_elements(), NormalizedDocs :: chunk_elements().
```

This function can be used to do whitespace normalization of
`application/erlang+html` documentation.

# `render`
*since OTP 23.0* 

```elixir
-spec render(Module, Docs) -> unicode:chardata() when Module :: module(), Docs :: docs_v1().
```

# `render`
*since OTP 23.0* 

```elixir
-spec render(Module, Docs, Config) -> unicode:chardata()
                when Module :: module(), Docs :: docs_v1(), Config :: config();
            (Module, Function, Docs) -> Res
                when
                    Module :: module(),
                    Function :: atom(),
                    Docs :: docs_v1(),
                    Res :: unicode:chardata() | {error, function_missing}.
```

Render module or function documentation.

Renders the module documentation if called as `render(Module, Docs, Config)`.

Equivalent to [`render(Module, Function, Docs, #{})`](`render/4`) if called
as `render(Module, Function, Docs)`.

# `render`
*since OTP 23.0* 

```elixir
-spec render(Module, Function, Docs, Config) -> Res
                when
                    Module :: module(),
                    Function :: atom(),
                    Docs :: docs_v1(),
                    Config :: config(),
                    Res :: unicode:chardata() | {error, function_missing};
            (Module, Function, Arity, Docs) -> Res
                when
                    Module :: module(),
                    Function :: atom(),
                    Arity :: arity(),
                    Docs :: docs_v1(),
                    Res :: unicode:chardata() | {error, function_missing}.
```

Render function documentation.

Renders the function documentation if called as `render(Module, Function, Docs, Config)`.

Equivalent to [`render(Module, Function, Arity, Docs, #{})`](`render/4`) if called
as `render(Module, Function, Arity, Docs)`.

# `render`
*since OTP 23.2* 

```elixir
-spec render(Module, Function, Arity, Docs, Config) -> Res
                when
                    Module :: module(),
                    Function :: atom(),
                    Arity :: arity(),
                    Docs :: docs_v1(),
                    Config :: config(),
                    Res :: unicode:chardata() | {error, function_missing}.
```

Render the documentation for a function.

# `render_callback`
*since OTP 23.0* 

```elixir
-spec render_callback(Module, Docs) -> unicode:chardata() when Module :: module(), Docs :: docs_v1().
```

# `render_callback`
*since OTP 23.0* 

```elixir
-spec render_callback(Module, Docs, Config) -> unicode:chardata()
                         when Module :: module(), Docs :: docs_v1(), Config :: config();
                     (Module, Callback, Docs) -> Res
                         when
                             Module :: module(),
                             Callback :: atom(),
                             Docs :: docs_v1(),
                             Res :: unicode:chardata() | {error, callback_missing}.
```

Render all callbacks in a module or callback documentation.

Renders a list with all callbacks if called as `render_callback(Module, Docs, Config)`.

Equivalent to [`render_callback(Module, Callback, Docs, #{})`](`render_callback/4`) if called
as `render_callback(Module, Callback, Docs)`.

# `render_callback`
*since OTP 23.0* 

```elixir
-spec render_callback(Module, Callback, Docs, Config) -> Res
                         when
                             Module :: module(),
                             Callback :: atom(),
                             Docs :: docs_v1(),
                             Config :: config(),
                             Res :: unicode:chardata() | {error, callback_missing};
                     (Module, Callback, Arity, Docs) -> Res
                         when
                             Module :: module(),
                             Callback :: atom(),
                             Arity :: arity(),
                             Docs :: docs_v1(),
                             Res :: unicode:chardata() | {error, callback_missing}.
```

Render callback documentation.

Renders the callback documentation if called as `render_callback(Module, Callback, Docs, Config)`.

Equivalent to [`render_callback(Module, Callback, Arity, Docs, #{})`](`render_callback/4`) if called
as `render_callback(Module, Callback, Arity, Docs)`.

# `render_callback`
*since OTP 23.2* 

```elixir
-spec render_callback(Module, Callback, Arity, Docs, Config) -> Res
                         when
                             Module :: module(),
                             Callback :: atom(),
                             Arity :: arity(),
                             Docs :: docs_v1(),
                             Config :: config(),
                             Res :: unicode:chardata() | {error, callback_missing}.
```

Render the documentation of a callback in a module.

# `render_type`
*since OTP 23.0* 

```elixir
-spec render_type(Module, Docs) -> unicode:chardata() when Module :: module(), Docs :: docs_v1().
```

# `render_type`
*since OTP 23.0* 

```elixir
-spec render_type(Module, Docs, Config) -> unicode:chardata()
                     when Module :: module(), Docs :: docs_v1(), Config :: config();
                 (Module, Type, Docs) -> Res
                     when
                         Module :: module(),
                         Type :: atom(),
                         Docs :: docs_v1(),
                         Res :: unicode:chardata() | {error, type_missing}.
```

Render all types in a module or type documentation.

Renders a list with all types if called as `render_type(Module, Docs, Config)`.

Equivalent to [`render_type(Module, Type, Docs, #{})`](`render_type/4`) if called
as `render_type(Module, Type, Docs)`.

# `render_type`
*since OTP 23.0* 

```elixir
-spec render_type(Module, Type, Docs, Config) -> Res
                     when
                         Module :: module(),
                         Type :: atom(),
                         Docs :: docs_v1(),
                         Config :: config(),
                         Res :: unicode:chardata() | {error, type_missing};
                 (Module, Type, Arity, Docs) -> Res
                     when
                         Module :: module(),
                         Type :: atom(),
                         Arity :: arity(),
                         Docs :: docs_v1(),
                         Res :: unicode:chardata() | {error, type_missing}.
```

Render type documentation.

Renders the type documentation if called as `render_type(Module, Type, Docs, Config)`.

Equivalent to [`render_type(Module, Type, Arity, Docs, #{})`](`render_type/4`) if called
as `render_type(Module, Type, Arity, Docs)`.

# `render_type`
*since OTP 23.2* 

```elixir
-spec render_type(Module, Type, Arity, Docs, Config) -> Res
                     when
                         Module :: module(),
                         Type :: atom(),
                         Arity :: arity(),
                         Docs :: docs_v1(),
                         Config :: config(),
                         Res :: unicode:chardata() | {error, type_missing}.
```

Render the documentation of a type in a module.

# `supported_tags`
*since OTP 24.0* 

```elixir
-spec supported_tags() -> [chunk_element_type()].
```

This function can be used to find out which tags are supported by
`application/erlang+html` documentation.

# `validate`
*since OTP 23.0* 

```elixir
-spec validate(Module) -> ok when Module :: module() | docs_v1().
```

This function can be used to do a basic validation of the doc content of
`application/erlang+html` format.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
