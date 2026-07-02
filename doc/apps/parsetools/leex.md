# `leex`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/parsetools/src/leex.erl#L42)

Lexical analyzer generator for Erlang

A regular expression based lexical analyzer generator for Erlang, similar to
`lex` or `flex`.

## Default Leex Options

The (host operating system) environment variable `ERL_COMPILER_OPTIONS` can be
used to give default Leex options. Its value must be a valid Erlang term. If the
value is a list, it is used as is. If it is not a list, it is put into a list.

The list is appended to any options given to `file/2`.

The list can be retrieved with `compile:env_compiler_options/0`.

## Input File Format

Erlang style comments starting with a `%` are allowed in scanner files. A
definition file has the following format:

```text
<Header>

Definitions.

<Macro Definitions>

Rules.

<Token Rules>

Erlang code.

<Erlang code>
```

The `Definitions.`, `Rules.`, and `Erlang code` headings are mandatory
and must start at the beginning of a source line. The `<Header>`,
`<Macro Definitions>`, and `<Erlang code>` sections are allowed to be
empty, but there must be at least one rule.

Macro definitions have the following format:

```text
NAME = VALUE
```

and there must be spaces around `=`. Macros can be used in the regular
expressions of rules by writing `{NAME}`.

> #### Note {: .info }
>
> When macros are expanded in expressions, the macro calls are replaced by the
> macro value without any form of quoting or enclosing in parentheses.

Rules have the following format:

```text
<Regexp> : <Erlang code>.
```

The `<Regexp>` must occur at the start of a line and not include any blanks; use
`\t` and `\s` to include TAB and SPACE characters in the regular expression. If
`<Regexp>` matches then the corresponding `<Erlang code>` is evaluated to generate a
token. With the Erlang code the following predefined variables are available:

- **`TokenChars`** - A list of the characters in the matched token.

- **`TokenLen`** - The number of characters in the matched token.

- **`TokenLine`** - The line number where the token occurred.

- **`TokenCol`** - The column number where the token occurred (column of the
  first character included in the token).

- **`TokenLoc`** - Token location. Expands to `{TokenLine,TokenCol}` (even when
  `error_location` is set to `line`).

The code must return:

- **`{token,Token}`** - Return `Token` to the caller.

- **`{end_token,Token}`** - Return `Token` and is last token in a tokens call.

- **`skip_token`** - Skip this token completely.

- **`{error,ErrString}`** - An error in the token, `ErrString` is a string
  describing the error.

It is also possible to push back characters into the input characters with the
following returns:

- `{token,Token,PushBackList}`
- `{end_token,Token,PushBackList}`
- `{skip_token,PushBackList}`

These have the same meanings as the normal returns but the characters in
`PushBackList` will be prepended to the input characters and scanned for the
next token. Note that pushing back a newline will mean the line numbering will
no longer be correct.

> #### Note {: .info }
>
> Pushing back characters gives you unexpected possibilities to cause the
> scanner to loop\!

The following example would match a simple Erlang integer or float and return a
token which could be sent to the Erlang parser:

```erlang
D = [0-9]

{D}+ :
  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
  {token,{float,TokenLine,list_to_float(TokenChars)}}.
```

The Erlang code in the `Erlang code.` section is written into the output file
directly after the module declaration and predefined exports declaration, making
it possible to add extra exports, define imports, and other attributes, which are
visible in the whole file.

## Regular Expressions

The regular expressions allowed here is a subset of the set found in `egrep` and
in the AWK programming language, as defined in the book _The AWK Programming
Language_ by A. V. Aho, B. W. Kernighan, and P. J. Weinberger. They are composed of
the following characters:

- **`c`** - Matches the non-metacharacter c.

- **`\c`** - Matches the escape sequence or literal character c.

- **`.`** - Matches any character.

- **`^`** - Matches the beginning of a string.

- **`$`** - Matches the end of a string.

- **`[abc...]`** - Character class, which matches any of the characters
  `abc...`. Character ranges are specified by a pair of characters separated by
  a `-`.

- **`[^abc...]`** - Negated character class, which matches any character except
  `abc...`.

- **`r1 | r2`** - Alternation. It matches either `r1` or `r2`.

- **`r1r2`** - Concatenation. It matches `r1` and then `r2`.

- **`r+`** - Matches one or more `r`s.

- **`r*`** - Matches zero or more `r`s.

- **`r?`** - Matches zero or one `r`s.

- **`(r)`** - Grouping. It matches `r`.

The escape sequences allowed are the same as for Erlang strings:

- **`\b`** - Backspace.

- **`\f`** - Form feed.

- **`\n`** - Newline (line feed).

- **`\r`** - Carriage return.

- **`\t`** - Tab.

- **`\e`** - Escape.

- **`\v`** - Vertical tab.

- **`\s`** - Space.

- **`\d`** - Delete.

- **`\ddd`** - The octal value `ddd`.

- **`\xhh`** - The hexadecimal value `hh`.

- **`\x{h...}`** - The hexadecimal value `h...`.

- **`\c`** - Any other character literally, for example `\\` for backslash, `\"`
  for `"`.

The following examples define simplified versions of a few Erlang data types:

```text
Atoms [a-z][0-9a-zA-Z_]*

Variables [A-Z_][0-9a-zA-Z_]*

Floats (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
```

> #### Note {: .info }
>
> Anchoring a regular expression with `^` and `$` is not implemented in the
> current version of `leex` and generates a parse error.

# `error_info`
*not exported* 

```erlang
-type error_info() :: {erl_anno:line() | none, module(), ErrorDescriptor :: term()}.
```

The standard `t:error_info/0` structure that is returned from all I/O modules.
`ErrorDescriptor` is formattable by `format_error/1`.

# `error_ret`
*not exported* 

```erlang
-type error_ret() :: error | {error, Errors :: errors(), Warnings :: warnings()}.
```

# `errors`
*not exported* 

```erlang
-type errors() :: [{file:filename(), [error_info()]}].
```

# `leex_ret`
*not exported* 

```erlang
-type leex_ret() :: ok_ret() | error_ret().
```

# `ok_ret`
*not exported* 

```erlang
-type ok_ret() ::
          {ok, Scannerfile :: file:filename()} | {ok, Scannerfile :: file:filename(), warnings()}.
```

# `warnings`
*not exported* 

```erlang
-type warnings() :: [{file:filename(), [error_info()]}].
```

# `string`

```erlang
-spec string(String) -> StringRet
                when
                    String :: string(),
                    StringRet :: {ok, Tokens, EndLoc} | ErrorInfo,
                    Tokens :: [Token],
                    Token :: term(),
                    ErrorInfo :: {error, error_info(), erl_anno:location()},
                    EndLoc :: erl_anno:location().
```

# `string`

```erlang
-spec string(String, StartLoc) -> StringRet
                when
                    String :: string(),
                    StringRet :: {ok, Tokens, EndLoc} | ErrorInfo,
                    Tokens :: [Token],
                    Token :: term(),
                    ErrorInfo :: {error, error_info(), erl_anno:location()},
                    StartLoc :: erl_anno:location(),
                    EndLoc :: erl_anno:location().
```

Scans `String` and returns either all the tokens in it or an `error` tuple.

`StartLoc` and `EndLoc` are either [`erl_anno:line()`](`t:erl_anno:line/0`)
or [`erl_anno:location()`](`t:erl_anno:location/0`), depending on the
`error_location` option.

> #### Note {: .info }
>
> It is an error if not all of the characters in `String` are consumed.

# `token`

```erlang
-spec token(Cont, Chars) -> {more, Cont1} | {done, TokenRet, RestChars}
               when
                   Cont :: [] | Cont1,
                   Cont1 :: tuple(),
                   Chars :: string() | eof,
                   RestChars :: string() | eof,
                   TokenRet :: {ok, Token, EndLoc} | {eof, EndLoc} | ErrorInfo,
                   ErrorInfo :: {error, error_info(), erl_anno:location()},
                   Token :: term(),
                   EndLoc :: erl_anno:location().
```

# `token`

```erlang
-spec token(Cont, Chars, StartLoc) -> {more, Cont1} | {done, TokenRet, RestChars}
               when
                   Cont :: [] | Cont1,
                   Cont1 :: tuple(),
                   Chars :: string() | eof,
                   RestChars :: string() | eof,
                   TokenRet :: {ok, Token, EndLoc} | {eof, EndLoc} | ErrorInfo,
                   ErrorInfo :: {error, error_info(), erl_anno:location()},
                   Token :: term(),
                   StartLoc :: erl_anno:location(),
                   EndLoc :: erl_anno:location().
```

This is a re-entrant call to try and scan a single token from `Chars`.

If there are enough characters in `Chars` to either scan a token or
detect an error then this will be returned with
`{done,...}`. Otherwise `{more,Cont}` will be returned where `Cont` is
used in the next call to `token()` with more characters to try an scan
the token. This is continued until a token has been scanned. `Cont` is
initially `[]`.

It is not designed to be called directly by an application, but is
used through the I/O system where it can typically be called in an
application by:

```erlang
io:request(InFile, {get_until,unicode,Prompt,Module,token,[Loc]})
  -> TokenRet
```

# `tokens`

```erlang
-spec tokens(Cont, Chars) -> {more, Cont1} | {done, TokensRet, RestChars}
                when
                    Cont :: [] | Cont1,
                    Cont1 :: tuple(),
                    Chars :: string() | eof,
                    RestChars :: string() | eof,
                    TokensRet :: {ok, Tokens, EndLoc} | {eof, EndLoc} | ErrorInfo,
                    Tokens :: [Token],
                    Token :: term(),
                    ErrorInfo :: {error, error_info(), erl_anno:location()},
                    EndLoc :: erl_anno:location().
```

# `tokens`

```erlang
-spec tokens(Cont, Chars, StartLoc) -> {more, Cont1} | {done, TokensRet, RestChars}
                when
                    Cont :: [] | Cont1,
                    Cont1 :: tuple(),
                    Chars :: string() | eof,
                    RestChars :: string() | eof,
                    TokensRet :: {ok, Tokens, EndLoc} | {eof, EndLoc} | ErrorInfo,
                    Tokens :: [Token],
                    Token :: term(),
                    ErrorInfo :: {error, error_info(), erl_anno:location()},
                    StartLoc :: erl_anno:location(),
                    EndLoc :: erl_anno:location().
```

This is a re-entrant call to try and scan tokens from `Chars`.

If there are enough characters in `Chars` to either scan tokens or
detect an error then this will be returned with
`{done,...}`. Otherwise `{more,Cont}` will be returned where `Cont` is
used in the next call to `tokens()` with more characters to try an
scan the tokens. This is continued until all tokens have been
scanned. `Cont` is initially `[]`.

This functions differs from `token` in that it will continue to scan tokens up
to and including an `{end_token,Token}` has been scanned (see next section). It
will then return all the tokens. This is typically used for scanning grammars
like Erlang where there is an explicit end token, `'.'`. If no end token is
found then the whole file will be scanned and returned. If an error occurs then
all tokens up to and including the next end token will be skipped.

It is not designed to be called directly by an application, but used through the
I/O system where it can typically be called in an application by:

```erlang
io:request(InFile, {get_until,unicode,Prompt,Module,tokens,[Loc]})
  -> TokensRet
```

# `file`

```erlang
-spec file(FileName) -> leex_ret() when FileName :: file:filename().
```

# `file`
*since OTP R16B02* 

```erlang
-spec file(FileName, Options) -> leex_ret()
              when
                  FileName :: file:filename(),
                  Options :: Option | [Option],
                  Option ::
                      {dfa_graph, boolean()} |
                      {includefile, Includefile :: file:filename()} |
                      {report_errors, boolean()} |
                      {report_warnings, boolean()} |
                      {report, boolean()} |
                      {return_errors, boolean()} |
                      {return_warnings, boolean()} |
                      {return, boolean()} |
                      {scannerfile, Scannerfile :: file:filename()} |
                      {verbose, boolean()} |
                      {warnings_as_errors, boolean()} |
                      {deterministic, boolean()} |
                      {error_location, line | column} |
                      {tab_size, pos_integer()} |
                      dfa_graph | report_errors | report_warnings | report | return_errors |
                      return_warnings | return | verbose | warnings_as_errors.
```

Generates a lexical analyzer from the definition in the input file.

The input file has the extension `.xrl`. This is added to the filename
if it is not given.  The resulting module is the Xrl filename without
the `.xrl` extension.

The current options are:

- **`dfa_graph`** - Generates a `.dot` file which contains a description of the
  DFA in a format which can be viewed with Graphviz, `www.graphviz.com`.

- **`{includefile,Includefile}`** - Uses a specific or customised prologue file
  instead of default `lib/parsetools/include/leexinc.hrl` which is otherwise
  included.

- **`{report_errors, boolean()}`** - Causes errors to be printed as they occur.
  Default is `true`.

- **`{report_warnings, boolean()}`** - Causes warnings to be printed as they
  occur. Default is `true`.

- **`{report, boolean()}`** - This is a short form for both `report_errors` and
  `report_warnings`.

- **`{return_errors, boolean()}`** - If this flag is set,
  `{error, Errors, Warnings}` is returned when there are errors. Default is
  `false`.

- **`{return_warnings, boolean()}`** - If this flag is set, an extra field
  containing `Warnings` is added to the tuple returned upon success. Default is
  `false`.

- **`{return, boolean()}`** - This is a short form for both `return_errors` and
  `return_warnings`.

- **`{scannerfile, Scannerfile}`** - `Scannerfile` is the name of the file that
  will contain the Erlang scanner code that is generated. The default (`""`) is
  to add the extension `.erl` to `FileName` stripped of the `.xrl` extension.

- **`{verbose, boolean()}`** - Outputs information from parsing the input file
  and generating the internal tables.

- **`{warnings_as_errors, boolean()}`** - Causes warnings to be treated as
  errors.

- **`{deterministic, boolean()}`** - Causes generated `-file()` attributes to only
  include the basename of the file path.

- **`{error_location, line | column}`** - If set to `column`, error location
  will be `{Line,Column}` tuple instead of just `Line`. Also, `StartLoc` and
  `EndLoc` in [`string/2`](`string/2`), [`token/3`](`token/3`), and
  [`tokens/3`](`tokens/3`) functions will be `{Line,Column}` tuple instead of
  just `Line`. Default is `line`. Note that you can use `TokenLoc` for token
  location independently, even if the `error_location` is set to `line`.

  Unicode characters are counted as many columns as they use bytes to represent.

- **`{tab_size, pos_integer()}`** - Sets the width of `\t` character (only
  relevant if `error_location` is set to `column`). Default is `8`.

Any of the Boolean options can be set to `true` by stating the name of the
option. For example, `verbose` is equivalent to `{verbose, true}`.

Leex will add the extension `.hrl` to the `Includefile` name and the extension
`.erl` to the `Scannerfile` name, unless the extension is already there.

# `format_error`

```erlang
-spec format_error(ErrorDescriptor) -> io_lib:chars() when ErrorDescriptor :: term().
```

Returns a descriptive string in English of an error reason `ErrorDescriptor`
returned by [`leex:file/1,2`](`file/1`) when there is an error in a regular
expression.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
