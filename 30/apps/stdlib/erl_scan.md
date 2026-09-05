# `erl_scan`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_scan.erl#L52)

The Erlang token scanner.

This module contains functions for tokenizing (scanning) characters into Erlang
tokens.

### Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLocation, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

### Notes

The continuation of the first call to the re-entrant input functions must be
`[]`. For a complete description of how the re-entrant input scheme works, see
Armstrong, Virding and Williams: 'Concurrent Programming in Erlang', Chapter 13.

### See Also

`m:erl_anno`, `m:erl_parse`, `m:io`

# `category`
*not exported* 

```erlang
-type category() :: atom().
```

# `char_spec`
*not exported* 

```erlang
-type char_spec() :: string() | eof.
```

# `error_description`
*not exported* 

```erlang
-type error_description() :: term().
```

# `error_info`

```erlang
-type error_info() :: {erl_anno:location(), module(), error_description()}.
```

# `option`
*not exported* 

```erlang
-type option() ::
          return | return_white_spaces | return_comments | text |
          {reserved_word_fun, resword_fun()} |
          {text_fun, text_fun()} |
          {compiler_internal, [term()]}.
```

# `options`

```erlang
-type options() :: option() | [option()].
```

# `resword_fun`
*not exported* 

```erlang
-type resword_fun() :: fun((atom()) -> boolean()).
```

# `return_cont`

```erlang
-opaque return_cont()
```

# `symbol`
*not exported* 

```erlang
-type symbol() :: atom() | float() | integer() | string().
```

# `text_fun`
*not exported* 

```erlang
-type text_fun() :: fun((atom(), string()) -> boolean()).
```

# `token`

```erlang
-type token() :: {category(), Anno :: erl_anno:anno(), symbol()} | {category(), Anno :: erl_anno:anno()}.
```

# `tokens`

```erlang
-type tokens() :: [token()].
```

# `tokens_result`

```erlang
-type tokens_result() ::
          {ok, Tokens :: tokens(), EndLocation :: erl_anno:location()} |
          {eof, EndLocation :: erl_anno:location()} |
          {error, ErrorInfo :: error_info(), EndLocation :: erl_anno:location()}.
```

# `category`
*since OTP 18.0* 

```erlang
-spec category(Token) -> category() when Token :: token().
```

Returns the category of `Token`.

# `column`
*since OTP 18.0* 

```erlang
-spec column(Token) -> erl_anno:column() | undefined when Token :: token().
```

Returns the column of `Token`'s collection of annotations.

# `end_location`
*since OTP 18.0* 

```erlang
-spec end_location(Token) -> erl_anno:location() | undefined when Token :: token().
```

Returns the end location of the text of `Token`'s collection of annotations. If
there is no text, `undefined` is returned.

# `format_error`

```erlang
-spec format_error(ErrorDescriptor) -> string() when ErrorDescriptor :: error_description().
```

Uses an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when an `ErrorInfo`
structure is processed (see section
[Error Information](`m:erl_scan#module-error-information`)).

# `line`
*since OTP 18.0* 

```erlang
-spec line(Token) -> erl_anno:line() when Token :: token().
```

Returns the line of `Token`'s collection of annotations.

# `location`
*since OTP 18.0* 

```erlang
-spec location(Token) -> erl_anno:location() when Token :: token().
```

Returns the location of `Token`'s collection of annotations.

# `reserved_word`

```erlang
-spec reserved_word(Atom :: atom()) -> boolean().
```

Returns `true` if `Atom` is an Erlang reserved word, otherwise `false`.

# `string`

```erlang
-spec string(String) -> Return
                when
                    String :: string(),
                    Return ::
                        {ok, Tokens :: tokens(), EndLocation} |
                        {error, ErrorInfo :: error_info(), ErrorLocation},
                    EndLocation :: erl_anno:location(),
                    ErrorLocation :: erl_anno:location().
```

# `string`

```erlang
-spec string(String, StartLocation) -> Return
                when
                    String :: string(),
                    Return ::
                        {ok, Tokens :: tokens(), EndLocation} |
                        {error, ErrorInfo :: error_info(), ErrorLocation},
                    StartLocation :: erl_anno:location(),
                    EndLocation :: erl_anno:location(),
                    ErrorLocation :: erl_anno:location().
```

# `string`

```erlang
-spec string(String, StartLocation, Options) -> Return
                when
                    String :: string(),
                    Options :: options(),
                    Return ::
                        {ok, Tokens :: tokens(), EndLocation} |
                        {error, ErrorInfo :: error_info(), ErrorLocation},
                    StartLocation :: erl_anno:location(),
                    EndLocation :: erl_anno:location(),
                    ErrorLocation :: erl_anno:location().
```

Takes the list of characters `String` and tries to scan (tokenize) them.

Returns one of the following:

- **`{ok, Tokens, EndLocation}`** - `Tokens` are the Erlang tokens from
  `String`. `EndLocation` is the first location after the last token.

- **`{error, ErrorInfo, ErrorLocation}`** - An error occurred. `ErrorLocation`
  is the first location after the erroneous token.

`StartLocation` indicates the initial location when scanning starts. If
`StartLocation` is a line, `Anno`, `EndLocation`, and `ErrorLocation` are lines.
If `StartLocation` is a pair of a line and a column, `Anno` takes the form of an
opaque compound data type, and `EndLocation` and `ErrorLocation` are pairs of a
line and a column. The _token annotations_ contain information about the column
and the line where the token begins, as well as the text of the token (if option
`text` is specified), all of which can be accessed by calling `column/1`,
`line/1`, `location/1`, and `text/1`.

A _token_ is a tuple containing information about syntactic category, the token
annotations, and the terminal symbol. For punctuation characters (such as `;`
and `|`) and reserved words, the category and the symbol coincide, and the token
is represented by a two-tuple. Three-tuples have one of the following forms:

- `{atom, Anno, atom()}`
- `{char, Anno, char()}`
- `{comment, Anno, string()}`
- `{float, Anno, float()}`
- `{integer, Anno, integer()}`
- `{var, Anno, atom()}`
- `{white_space, Anno, string()}`

Valid options:

- **`{reserved_word_fun, reserved_word_fun()}`** - A callback function that is
  called when the scanner has found an unquoted atom. If the function returns
  `true`, the unquoted atom itself becomes the category of the token. If the
  function returns `false`, `atom` becomes the category of the unquoted atom.

- **`return_comments`** - Return comment tokens.

- **`return_white_spaces`** - Return white space tokens. By convention, a
  newline character, if present, is always the first character of the text
  (there cannot be more than one newline in a white space token).

- **`return`** - Short for `[return_comments, return_white_spaces]`.

- **`text`{: #text }** - Include the token text in the token annotation. The
  text is the part of the input corresponding to the token. See also
  [`text_fun`](`m:erl_scan#text_fun`).

- **`{text_fun, text_fun()}`{: #text_fun }** - A callback function used to
  determine whether the full text for the token shall be included in the token
  annotation. Arguments of the function are the category of the token and the
  full token string. This is only used when [`text`](`m:erl_scan#text`) is not
  present. If neither are present the text will not be saved in the token
  annotation.

- **`{compiler_internal, term()}`{: #compiler_interal }** - Pass
  compiler-internal options to the scanner. The set of internal options
  understood by the scanner should be considered experimental and can thus be
  changed at any time without prior warning.

  The following options are currently understood:

  - **`ssa_checks`** - Tokenizes source code annotations used for encoding tests
    on the BEAM SSA code produced by the compiler.

# `symbol`
*since OTP 18.0* 

```erlang
-spec symbol(Token) -> symbol() when Token :: token().
```

Returns the symbol of `Token`.

# `text`
*since OTP 18.0* 

```erlang
-spec text(Token) -> erl_anno:text() | undefined when Token :: token().
```

Returns the text of `Token`'s collection of annotations. If there is no text,
`undefined` is returned.

# `tokens`

```erlang
-spec tokens(Continuation, CharSpec, StartLocation) -> Return
                when
                    Continuation :: return_cont() | [],
                    CharSpec :: char_spec(),
                    StartLocation :: erl_anno:location(),
                    Return ::
                        {done, Result :: tokens_result(), LeftOverChars :: char_spec()} |
                        {more, Continuation1 :: return_cont()}.
```

# `tokens`

```erlang
-spec tokens(Continuation, CharSpec, StartLocation, Options) -> Return
                when
                    Continuation :: return_cont() | [],
                    CharSpec :: char_spec(),
                    StartLocation :: erl_anno:location(),
                    Options :: options(),
                    Return ::
                        {done, Result :: tokens_result(), LeftOverChars :: char_spec()} |
                        {more, Continuation1 :: return_cont()}.
```

This is the re-entrant scanner, which scans characters until either a _dot_ ('.'
followed by a white space) or `eof` is reached.

It returns:

- **`{done, Result, LeftOverChars}`** - Indicates that there is sufficient input
  data to get a result. `Result` is:

  - **`{ok, Tokens, EndLocation}`** - The scanning was successful. `Tokens` is
    the list of tokens including _dot_.

  - **`{eof, EndLocation}`** - End of file was encountered before any more
    tokens.

  - **`{error, ErrorInfo, EndLocation}`** - An error occurred. `LeftOverChars`
    is the remaining characters of the input data, starting from `EndLocation`.

- **`{more, Continuation1}`** - More data is required for building a term.
  `Continuation1` must be passed in a new call to `tokens/3,4` when more data is
  available.

The `CharSpec` `eof` signals end of file. `LeftOverChars` then takes the value
`eof` as well.

For a description of the options, see `string/3`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
