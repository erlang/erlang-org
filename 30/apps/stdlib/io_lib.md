# `io_lib`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/io_lib.erl#L63)

I/O library functions.

This module contains functions for converting to and from strings (lists of
characters). They are used for implementing the functions in the `m:io` module.
There is no guarantee that the character lists returned from some of the
functions are flat, they can be deep lists. Function `lists:flatten/1` can be
used for flattening deep lists.

# `chars`

```erlang
-type chars() :: [char() | chars()].
```

An possibly deep list containing only `t:char/0`s.

# `chars_limit`

```erlang
-type chars_limit() :: integer().
```

A soft limit on the number of characters returned.

When the number of characters is reached, remaining structures are
replaced by "`...`". `CharsLimit` defaults to -1, which means no limit on the
number of characters returned.

# `continuation`

```erlang
-opaque continuation()
```

A continuation as returned by `fread/3`.

# `depth`
*not exported* 

```erlang
-type depth() :: -1 | non_neg_integer().
```

# `format_options`

```erlang
-type format_options() :: [{chars_limit, chars_limit()}].
```

Options that can be passed to `format/3` and `fwrite/3`.

# `format_spec`

```erlang
-type format_spec() ::
          #{control_char := char(),
            args := [any()],
            width := none | integer(),
            adjust := left | right,
            precision := none | integer(),
            pad_char := char(),
            encoding := unicode | latin1,
            strings := boolean(),
            maps_order => maps:iterator_order()}.
```

A map describing the contents of a format string.

- `control_char` is the type of control sequence: `$P`, `$w`, and so on.
- `args` is a list of the arguments used by the control sequence, or an empty
  list if the control sequence does not take any arguments.
- `width` is the field width.
- `adjust` is the adjustment.
- `precision` is the precision of the printed argument.
- `pad_char` is the padding character.
- `encoding` is set to `true` if translation modifier `t` is present.
- `strings` is set to `false` if modifier `l` is present.
- `maps_order` is set to `undefined` by default, `ordered` if modifier `k` is
  present, or `reversed` or `CmpFun` if modifier `K` is present.

# `fread_error`

```erlang
-type fread_error() :: atom | based | character | float | format | input | integer | string | unsigned.
```

# `fread_item`

```erlang
-type fread_item() :: string() | atom() | integer() | float().
```

# `latin1_string`

```erlang
-type latin1_string() :: [unicode:latin1_char()].
```

# `bformat`
*since OTP 28.0* 

```erlang
-spec bformat(Format, Data) -> unicode:unicode_binary() when Format :: io:format(), Data :: [term()].
```

# `bformat`
*since OTP 28.0* 

```erlang
-spec bformat(Format, Data, Options) -> unicode:unicode_binary()
                 when Format :: io:format(), Data :: [term()], Options :: format_options().
```

# `bfwrite`
*since OTP 28.0* 

```erlang
-spec bfwrite(Format, Data) -> unicode:unicode_binary() when Format :: io:format(), Data :: [term()].
```

# `bfwrite`
*since OTP 28.0* 

```erlang
-spec bfwrite(Format, Data, Options) -> unicode:unicode_binary()
                 when Format :: io:format(), Data :: [term()], Options :: format_options().
```

Binary variant of `fwrite/3`

Returns a UTF-8 encoded binary string.

# `build_text`
*since OTP 18.0* 

```erlang
-spec build_text(FormatList) -> chars() when FormatList :: [char() | format_spec()].
```

For details, see `scan_format/2`.

# `bwrite`
*since OTP 28.0* 

```erlang
-spec bwrite(Term, Options) -> unicode:unicode_binary()
                when
                    Term :: term(),
                    Options :: [Option],
                    Option ::
                        {chars_limit, CharsLimit} |
                        {depth, Depth} |
                        {encoding, latin1 | utf8 | unicode} |
                        {maps_order, maps:iterator_order()},
                    CharsLimit :: chars_limit(),
                    Depth :: depth().
```

Behaves as `write/2` but returns a UTF-8 encoded binary string.

# `bwrite_string`
*since OTP 28.0* 

```erlang
-spec bwrite_string(String, Qoute, InEnc) -> unicode:unicode_binary()
                       when
                           String :: string() | binary(),
                           Qoute :: integer() | [],
                           InEnc :: unicode | latin1.
```

Returns the UTF-8 encoded binary `String` surrounded by `Qoute`.

# `char_list`

```erlang
-spec char_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a flat list of characters in the Unicode range,
otherwise `false`.

# `deep_char_list`

```erlang
-spec deep_char_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a, possibly deep, list of characters in the Unicode
range, otherwise `false`.

# `deep_latin1_char_list`
*since OTP R16B* 

```erlang
-spec deep_latin1_char_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a, possibly deep, list of characters in the ISO
Latin-1 range, otherwise `false`.

# `format`

```erlang
-spec format(Format, Data) -> chars() when Format :: io:format(), Data :: [term()].
```

# `format`
*since OTP 21.0* 

```erlang
-spec format(Format, Data, Options) -> chars()
                when Format :: io:format(), Data :: [term()], Options :: format_options().
```

# `fread`

```erlang
-spec fread(Format, String) -> Result
               when
                   Format :: string(),
                   String :: string(),
                   Result ::
                       {ok, InputList :: [fread_item()], LeftOverChars :: string()} |
                       {more,
                        RestFormat :: string(),
                        Nchars :: non_neg_integer(),
                        InputStack :: chars()} |
                       {error, {fread, What :: fread_error()}}.
```

Tries to read `String` in accordance with the control sequences in `Format`.

For a detailed description of the available formatting options, see `io:fread/3`.
It is assumed that `String` contains whole lines.

The function returns:

- **`{ok, InputList, LeftOverChars}`** - The string was read. `InputList` is the
  list of successfully matched and read items, and `LeftOverChars` are the input
  characters not used.

- **`{more, RestFormat, Nchars, InputStack}`** - The string was read, but more
  input is needed to complete the original format string. `RestFormat` is the
  remaining format string, `Nchars` is the number of characters scanned, and
  `InputStack` is the reversed list of inputs matched up to that point.

- **`{error, What}`** - The read operation failed and parameter `What` gives a
  hint about the error.

_Example:_

```erlang
3> io_lib:fread("~f~f~f", "15.6 17.3e-6 24.5").
{ok,[15.6,1.73e-5,24.5],[]}
```

# `fread`

```erlang
-spec fread(Continuation, CharSpec, Format) -> Return
               when
                   Continuation :: continuation() | [],
                   CharSpec :: string() | eof,
                   Format :: string(),
                   Return ::
                       {more, Continuation1 :: continuation()} |
                       {done, Result, LeftOverChars :: string()},
                   Result ::
                       {ok, InputList :: [fread_item()]} | eof | {error, {fread, What :: fread_error()}}.
```

This is the re-entrant formatted reader. The continuation of the first call to
the functions must be `[]`.

For a complete description of how the re-entrant input scheme works,
see Armstrong, Virding, Williams: 'Concurrent Programming in
Erlang', Chapter 13.

The function returns:

- **`{done, Result, LeftOverChars}`** - The input is complete. The result is one
  of the following:

  - **`{ok, InputList}`** - The string was read. `InputList` is the list of
    successfully matched and read items, and `LeftOverChars` are the remaining
    characters.

  - **`eof`** - End of file was encountered. `LeftOverChars` are the input
    characters not used.

  - **`{error, What}`** - An error occurred and parameter `What` gives a hint
    about the error.

- **`{more, Continuation}`** - More data is required to build a term.
  `Continuation` must be passed to [`fread/3`](`fread/3`) when more data becomes
  available.

# `fwrite`

```erlang
-spec fwrite(Format, Data) -> chars() when Format :: io:format(), Data :: [term()].
```

Returns a character list that represents `Data` formatted in accordance with
`Format`.

For a detailed description of the available formatting options, see
[`io:fwrite/1,2,3`](`io:fwrite/1`). If the format string or argument list
contains an error, a fault is generated.

If and only if the Unicode translation modifier is used in the format string
(that is, `~ts` or `~tc`), the resulting list can contain characters beyond the
ISO Latin-1 character range (that is, numbers > 255). If so, the result is still
an ordinary Erlang `t:string/0`, and can well be used in any context where
Unicode data is allowed.

# `fwrite`
*since OTP 21.0* 

```erlang
-spec fwrite(Format, Data, Options) -> chars()
                when Format :: io:format(), Data :: [term()], Options :: format_options().
```

Returns a character list that represents `Data` formatted in accordance with
`Format` in the same way as `fwrite/2` and `format/2`, but takes an extra
argument, a list of options.

Valid option:

- **`{chars_limit, CharsLimit}`** - A soft limit on the number of characters
  returned. When the number of characters is reached, remaining structures are
  replaced by "`...`". `CharsLimit` defaults to -1, which means no limit on the
  number of characters returned.

# `indentation`

```erlang
-spec indentation(String, StartIndent) -> integer() when String :: string(), StartIndent :: integer().
```

Returns the indentation if `String` has been printed, starting at `StartIndent`.

# `latin1_char_list`
*since OTP R16B* 

```erlang
-spec latin1_char_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a flat list of characters in the ISO Latin-1 range,
otherwise `false`.

# `nl`

```erlang
-spec nl() -> string().
```

Returns a character list that represents a new line character.

# `print`

```erlang
-spec print(Term) -> chars() when Term :: term().
```

# `print`

```erlang
-spec print(Term, Column, LineLength, Depth) -> chars()
               when
                   Term :: term(),
                   Column :: non_neg_integer(),
                   LineLength :: non_neg_integer(),
                   Depth :: depth().
```

Returns a list of characters that represents `Term`, but breaks representations
longer than one line into many lines and indents each line sensibly.

Also tries to detect and output lists of printable characters as strings.

- `Column` is the starting column; defaults to 1.
- `LineLength` is the maximum line length; defaults to 80. The value 0 can be used
   to specify infinite length, meaning that no line breaks are inserted.
- `Depth` is the maximum print depth; defaults to -1, which means no limitation.

# `printable_latin1_list`
*since OTP R16B* 

```erlang
-spec printable_latin1_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a flat list of printable ISO Latin-1 characters,
otherwise `false`.

# `printable_list`

```erlang
-spec printable_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a flat list of printable characters, otherwise
`false`.

What is a printable character in this case is determined by startup flag `+pc`
to the Erlang VM; see `io:printable_range/0` and
[`erl(1)`](`e:erts:erl_cmd.md`).

# `printable_unicode_list`
*since OTP R16B* 

```erlang
-spec printable_unicode_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a flat list of printable Unicode characters,
otherwise `false`.

# `scan_format`
*since OTP 18.0* 

```erlang
-spec scan_format(Format, Data) -> FormatList
                     when
                         Format :: io:format(), Data :: [term()], FormatList :: [char() | format_spec()].
```

Returns a list corresponding to the specified format string, where control
sequences have been replaced with corresponding tuples. This list can be passed
to:

- `build_text/1` to have the same effect as [`format(Format, Args)`](`format/2`)
- `unscan_format/1` to get the corresponding pair of `Format` and `Args` (with
  every `*` and corresponding argument expanded to numeric values)

A typical use of this function is to replace unbounded-size control sequences
like `~w` and `~p` with the depth-limited variants `~W` and `~P` before
formatting to text in, for example, a logger.

# `unscan_format`
*since OTP 18.0* 

```erlang
-spec unscan_format(FormatList) -> {Format, Data}
                       when
                           FormatList :: [char() | format_spec()],
                           Format :: io:format(),
                           Data :: [term()].
```

For details, see `scan_format/2`.

# `write`

```erlang
-spec write(Term) -> chars() when Term :: term().
```

# `write`

```erlang
-spec write(Term, Depth) -> chars() when Term :: term(), Depth :: depth();
           (Term, Options) -> chars()
               when
                   Term :: term(),
                   Options :: [Option],
                   Option ::
                       {chars_limit, CharsLimit} |
                       {depth, Depth} |
                       {encoding, latin1 | utf8 | unicode} |
                       {maps_order, maps:iterator_order()},
                   CharsLimit :: chars_limit(),
                   Depth :: depth().
```

Returns a character list that represents `Term`. Option `Depth` controls the
depth of the structures written.

When the specified depth is reached, everything below this level is replaced by
"`...`".

`Depth` defaults to -1, which means no limitation. Option `CharsLimit` puts a
soft limit on the number of characters returned. When the number of characters is
reached, remaining structures are replaced by "`...`". `CharsLimit` defaults to -1,
which means no limit on the number of characters returned.

_Example:_

```erlang
1> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9})).
"{1,[2],[3],[4,5],6,7,8,9}"
2> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9}, 5)).
"{1,[2],[3],[...],...}"
3> lists:flatten(io_lib:write({[1,2,3],[4,5],6,7,8,9}, [{chars_limit,20}])).
"{[1,2|...],[4|...],...}"
```

# `write_atom`

```erlang
-spec write_atom(Atom) -> chars() when Atom :: atom().
```

Returns the list of characters needed to print atom `Atom`.

# `write_atom_as_latin1`
*since OTP 20.0* 

```erlang
-spec write_atom_as_latin1(Atom) -> latin1_string() when Atom :: atom().
```

Returns the list of characters needed to print atom `Atom`. Non-Latin-1
characters are escaped.

# `write_char`

```erlang
-spec write_char(Char) -> chars() when Char :: char().
```

Returns the list of characters needed to print a character constant in the
Unicode character set.

# `write_char_as_latin1`
*since OTP R16B* 

```erlang
-spec write_char_as_latin1(Char) -> latin1_string() when Char :: char().
```

Returns the list of characters needed to print a character constant in the
Unicode character set. Non-Latin-1 characters are escaped.

# `write_latin1_char`
*since OTP R16B* 

```erlang
-spec write_latin1_char(Latin1Char) -> latin1_string() when Latin1Char :: unicode:latin1_char().
```

Returns the list of characters needed to print a character constant in the ISO
Latin-1 character set.

# `write_latin1_string`
*since OTP R16B* 

```erlang
-spec write_latin1_string(Latin1String) -> latin1_string() when Latin1String :: latin1_string().
```

Returns the list of characters needed to print `Latin1String` as a string.

# `write_string`

```erlang
-spec write_string(String) -> chars() when String :: string().
```

Returns the list of characters needed to print `String` as a string.

# `write_string_as_latin1`
*since OTP R16B* 

```erlang
-spec write_string_as_latin1(String) -> latin1_string() when String :: string().
```

Returns the list of characters needed to print `String` as a string. Non-Latin-1
characters are escaped.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
