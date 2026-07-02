# `unicode`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/unicode.erl#L22)

Functions for converting Unicode characters.

This module contains functions for converting between different character
representations. It converts between ISO Latin-1 characters and Unicode
characters, but it can also convert between different Unicode encodings (like
UTF-8, UTF-16, and UTF-32).

The default Unicode encoding in Erlang binaries is UTF-8, which is also the
format in which built-in functions and libraries in OTP expect to find binary
Unicode data. In lists, Unicode data is encoded as integers, each integer
representing one character and encoded simply as the Unicode code point for the
character.

Other Unicode encodings than integers representing code points or UTF-8 in
binaries are referred to as "external encodings". The ISO Latin-1 encoding is in
binaries and lists referred to as latin1-encoding.

It is recommended to only use external encodings for communication with external
entities where this is required. When working inside the Erlang/OTP environment,
it is recommended to keep binaries in UTF-8 when representing Unicode
characters. ISO Latin-1 encoding is supported both for backward compatibility
and for communication with external entities not supporting Unicode character
sets.

Programs should always operate on a normalized form and compare
canonical-equivalent Unicode characters as equal. All characters should thus be
normalized to one form once on the system borders. One of the following
functions can convert characters to their normalized forms
`characters_to_nfc_list/1`, `characters_to_nfc_binary/1`,
`characters_to_nfd_list/1` or `characters_to_nfd_binary/1`. For general text
`characters_to_nfc_list/1` or `characters_to_nfc_binary/1` is preferred, and for
identifiers one of the compatibility normalization functions, such as
`characters_to_nfkc_list/1`, is preferred for security reasons. The
normalization functions where introduced in OTP 20. Additional information on
normalization can be found in the
[Unicode FAQ](http://unicode.org/faq/normalization.html).

# `chardata`

```elixir
-type chardata() :: charlist() | unicode_binary().
```

# `charlist`

```elixir
-type charlist() :: maybe_improper_list(char() | unicode_binary() | charlist(), unicode_binary() | []).
```

# `encoding`

```elixir
-type encoding() :: latin1 | unicode | utf8 | utf16 | {utf16, endian()} | utf32 | {utf32, endian()}.
```

# `endian`
*not exported* 

```elixir
-type endian() :: big | little.
```

# `external_chardata`

```elixir
-type external_chardata() :: external_charlist() | external_unicode_binary().
```

# `external_charlist`

```elixir
-type external_charlist() ::
          maybe_improper_list(char() | external_unicode_binary() | external_charlist(),
                              external_unicode_binary() | []).
```

# `external_unicode_binary`
*not exported* 

```elixir
-type external_unicode_binary() :: binary().
```

A `t:binary/0` with characters coded in a user-specified Unicode encoding other
than UTF-8 (that is, UTF-16 or UTF-32).

# `latin1_binary`

```elixir
-type latin1_binary() :: binary().
```

A `t:binary/0` with characters coded in ISO Latin-1.

# `latin1_char`

```elixir
-type latin1_char() :: byte().
```

An `t:integer/0` representing a valid ISO Latin-1 character (0-255).

# `latin1_chardata`

```elixir
-type latin1_chardata() :: latin1_charlist() | latin1_binary().
```

Equivalent to `t:iodata/0`.

# `latin1_charlist`

```elixir
-type latin1_charlist() ::
          maybe_improper_list(latin1_char() | latin1_binary() | latin1_charlist(), latin1_binary() | []).
```

Equivalent to `t:iolist/0`.

# `unicode_binary`

```elixir
-type unicode_binary() :: binary().
```

A `t:binary/0` with characters encoded in the UTF-8 coding standard.

# `bom_to_encoding`

```elixir
-spec bom_to_encoding(Bin) -> {Encoding, Length}
                         when
                             Bin :: binary(),
                             Encoding :: latin1 | utf8 | {utf16, endian()} | {utf32, endian()},
                             Length :: non_neg_integer().
```

Checks for a UTF Byte Order Mark (BOM) in the beginning of a binary.

If the supplied binary `Bin` begins with a valid BOM for either UTF-8, UTF-16, or
UTF-32, the function returns the encoding identified along with the BOM length
in bytes.

If no BOM is found, the function returns `{latin1,0}`.

# `characters_to_binary`

```elixir
-spec characters_to_binary(Data) -> Result
                              when
                                  Data :: latin1_chardata() | chardata() | external_chardata(),
                                  Result ::
                                      binary() |
                                      {error, binary(), RestData} |
                                      {incomplete, binary(), binary()},
                                  RestData :: latin1_chardata() | chardata() | external_chardata().
```

# `characters_to_binary`

```elixir
-spec characters_to_binary(Data, InEncoding) -> Result
                              when
                                  Data :: latin1_chardata() | chardata() | external_chardata(),
                                  InEncoding :: encoding(),
                                  Result ::
                                      binary() |
                                      {error, binary(), RestData} |
                                      {incomplete, binary(), binary()},
                                  RestData :: latin1_chardata() | chardata() | external_chardata().
```

# `characters_to_binary`

```elixir
-spec characters_to_binary(Data, InEncoding, OutEncoding) -> Result
                              when
                                  Data :: latin1_chardata() | chardata() | external_chardata(),
                                  InEncoding :: encoding(),
                                  OutEncoding :: encoding(),
                                  Result ::
                                      binary() |
                                      {error, binary(), RestData} |
                                      {incomplete, binary(), binary()},
                                  RestData :: latin1_chardata() | chardata() | external_chardata().
```

Behaves as `characters_to_list/2`, but produces a binary instead of a Unicode
list.

`InEncoding` defines how input is to be interpreted if binaries are present in
`Data`

`OutEncoding` defines in what format output is to be generated.

Options:

- **`unicode`** - An alias for `utf8`, as this is the preferred encoding for
  Unicode characters in binaries.

- **`utf16`** - An alias for `{utf16,big}`.

- **`utf32`** - An alias for `{utf32,big}`.

The atoms `big` and `little` denote big- or little-endian encoding.

Errors and exceptions occur as in `characters_to_list/2`, but the second element
in tuple `error` or `incomplete` is a `t:binary/0` and not a `t:list/0`.

# `characters_to_list`

```elixir
-spec characters_to_list(Data) -> Result
                            when
                                Data :: latin1_chardata() | chardata() | external_chardata(),
                                Result ::
                                    string() |
                                    {error, string(), RestData} |
                                    {incomplete, string(), binary()},
                                RestData :: latin1_chardata() | chardata() | external_chardata().
```

# `characters_to_list`

```elixir
-spec characters_to_list(Data, InEncoding) -> Result
                            when
                                Data :: latin1_chardata() | chardata() | external_chardata(),
                                InEncoding :: encoding(),
                                Result ::
                                    string() |
                                    {error, string(), RestData} |
                                    {incomplete, string(), binary()},
                                RestData :: latin1_chardata() | chardata() | external_chardata().
```

Converts a possibly deep list of integers and binaries into a list of integers
representing Unicode characters. The binaries in the input can have characters
encoded as one of the following:

- ISO Latin-1 (0-255, one character per byte). Here, case parameter `InEncoding`
  is to be specified as `latin1`.
- One of the UTF-encodings, which is specified as parameter `InEncoding`.

Note that integers in the list always represent code points regardless of
`InEncoding` passed. If `InEncoding latin1` is passed, only code points < 256
are allowed; otherwise, all valid unicode code points are allowed.

If `InEncoding` is `latin1`, parameter `Data` corresponds to the `t:iodata/0`
type, but for `unicode`, parameter `Data` can contain integers > 255 (Unicode
characters beyond the ISO Latin-1 range), which makes it invalid as
`t:iodata/0`.

The purpose of the function is mainly to convert combinations of Unicode
characters into a pure Unicode string in list representation for further
processing. For writing the data to an external entity, the reverse function
`characters_to_binary/3` comes in handy.

Option `unicode` is an alias for `utf8`, as this is the preferred encoding for
Unicode characters in binaries. `utf16` is an alias for `{utf16,big}` and
`utf32` is an alias for `{utf32,big}`. The atoms `big` and `little` denote big-
or little-endian encoding.

If the data cannot be converted, either because of illegal Unicode/ISO Latin-1
characters in the list, or because of invalid UTF encoding in any binaries, an
error tuple is returned. The error tuple contains the tag `error`, a list
representing the characters that could be converted before the error occurred
and a representation of the characters including and after the offending
integer/bytes. The last part is mostly for debugging, as it still constitutes a
possibly deep or mixed list, or both, not necessarily of the same depth as the
original data. The error occurs when traversing the list and whatever is left to
decode is returned "as is".

However, if the input `Data` is a pure binary, the third part of the error tuple
is guaranteed to be a binary as well.

Errors occur for the following reasons:

- Integers out of range.

  If `InEncoding` is `latin1`, an error occurs whenever an integer > 255 is
  found in the lists.

  If `InEncoding` is of a Unicode type, an error occurs whenever either of the
  following is found:

  - An integer > 16#10FFFF (the maximum Unicode character)
  - An integer in the range 16#D800 to 16#DFFF (invalid range reserved for
    UTF-16 surrogate pairs)

- Incorrect UTF encoding.

  If `InEncoding` is one of the UTF types, the bytes in any binaries must be
  valid in that encoding.

  Errors can occur for various reasons, including the following:

  - "Pure" decoding errors (like the upper bits of the bytes being wrong).
  - The bytes are decoded to a too large number.
  - The bytes are decoded to a code point in the invalid Unicode range.
  - Encoding is "overlong", meaning that a number should have been encoded in
    fewer bytes.

  The case of a truncated UTF is handled specially, see the paragraph about
  incomplete binaries below.

  If `InEncoding` is `latin1`, binaries are always valid as long as they contain
  whole bytes, as each byte falls into the valid ISO Latin-1 range.

A special type of error is when no actual invalid integers or bytes are found,
but a trailing `t:binary/0` consists of too few bytes to decode the last
character. This error can occur if bytes are read from a file in chunks or if
binaries in other ways are split on non-UTF character boundaries. An
`incomplete` tuple is then returned instead of the `error` tuple. It consists of
the same parts as the `error` tuple, but the tag is `incomplete` instead of
`error` and the last element is always guaranteed to be a binary consisting of
the first part of a (so far) valid UTF character.

If one UTF character is split over two consecutive binaries in the `Data`, the
conversion succeeds. This means that a character can be decoded from a range of
binaries as long as the whole range is specified as input without errors
occurring.

_Example:_

```erlang
decode_data(Data) ->
   case unicode:characters_to_list(Data,unicode) of
      {incomplete,Encoded, Rest} ->
            More = get_some_more_data(),
            Encoded ++ decode_data([Rest, More]);
      {error,Encoded,Rest} ->
            handle_error(Encoded,Rest);
      List ->
            List
   end.
```

However, bit strings that are not whole bytes are not allowed, so a UTF
character must be split along 8-bit boundaries to ever be decoded.

A `badarg` exception is thrown for the following cases:

- Any parameters are of the wrong type.
- The list structure is invalid (a number as tail).
- The binaries do not contain whole bytes (bit strings).

# `characters_to_nfc_binary`
*since OTP 20.0* 

```elixir
-spec characters_to_nfc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
4> unicode:characters_to_nfc_binary([<<"abc..a">>,[778],$a,[776],$o,[776]]).
<<"abc..åäö"/utf8>>
```

# `characters_to_nfc_list`
*since OTP 20.0* 

```elixir
-spec characters_to_nfc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
3> unicode:characters_to_nfc_list([<<"abc..a">>,[778],$a,[776],$o,[776]]).
"abc..åäö"
```

# `characters_to_nfd_binary`
*since OTP 20.0* 

```elixir
-spec characters_to_nfd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Decomposed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
2> unicode:characters_to_nfd_binary("abc..åäö").
<<97,98,99,46,46,97,204,138,97,204,136,111,204,136>>
```

# `characters_to_nfd_list`
*since OTP 20.0* 

```elixir
-spec characters_to_nfd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of canonical equivalent Decomposed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
1> unicode:characters_to_nfd_list("abc..åäö").
[97,98,99,46,46,97,778,97,776,111,776]
```

# `characters_to_nfkc_binary`
*since OTP 20.0* 

```elixir
-spec characters_to_nfkc_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
4> unicode:characters_to_nfkc_binary([<<"abc..a">>,[778],$a,[776],$o,[776],[65299,65298]]).
<<"abc..åäö32"/utf8>>
```

# `characters_to_nfkc_list`
*since OTP 20.0* 

```elixir
-spec characters_to_nfkc_list(chardata()) -> [char()] | {error, [char()], chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Composed characters according to the Unicode standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
3> unicode:characters_to_nfkc_list([<<"abc..a">>,[778],$a,[776],$o,[776],[65299,65298]]).
"abc..åäö32"
```

# `characters_to_nfkd_binary`
*since OTP 20.0* 

```elixir
-spec characters_to_nfkd_binary(chardata()) -> unicode_binary() | {error, unicode_binary(), chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Decomposed characters according to the Unicode
standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is an utf8 encoded binary.

```erlang
2> unicode:characters_to_nfkd_binary(["abc..åäö",[65299,65298]]).
<<97,98,99,46,46,97,204,138,97,204,136,111,204,136,51,50>>
```

# `characters_to_nfkd_list`
*since OTP 20.0* 

```elixir
-spec characters_to_nfkd_list(chardata()) -> [char()] | {error, [char()], chardata()}.
```

Converts a possibly deep list of characters and binaries into a Normalized Form
of compatibly equivalent Decomposed characters according to the Unicode
standard.

Any binaries in the input must be encoded with utf8 encoding.

The result is a list of characters.

```erlang
1> unicode:characters_to_nfkd_list(["abc..åäö",[65299,65298]]).
[97,98,99,46,46,97,778,97,776,111,776,51,50]
```

# `encoding_to_bom`

```elixir
-spec encoding_to_bom(InEncoding) -> Bin when Bin :: binary(), InEncoding :: encoding().
```

Creates a UTF Byte Order Mark (BOM) as a binary from the supplied `InEncoding`.

The BOM is, if supported at all, expected to be placed first in UTF encoded
files or messages.

The function returns `<<>>` for `latin1` encoding, as there is no BOM for ISO
Latin-1.

Notice that the BOM for UTF-8 is seldom used, and it is really not a _byte
order_ mark. There are obviously no byte order issues with UTF-8, so the BOM is
only there to differentiate UTF-8 encoding from other UTF formats.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
