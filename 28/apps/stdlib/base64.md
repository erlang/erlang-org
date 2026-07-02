# `base64`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/base64.erl#L24)

Provides base64 encode and decode, see
[RFC 2045](https://www.ietf.org/rfc/rfc2045.txt).

# `base64_alphabet`
*not exported* 

```elixir
-type base64_alphabet() :: $A..$Z | $a..$z | $0..$9 | $+ | $/ | $- | $_ | $=.
```

Base 64 Encoding alphabet, see
[RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648).

# `base64_binary`
*not exported* 

```elixir
-type base64_binary() :: binary().
```

Base 64 encoded binary.

# `base64_mode`
*not exported* 

```elixir
-type base64_mode() :: standard | urlsafe.
```

Selector for the Base 64 Encoding alphabet used for [encoding](`encode/2`) and
[decoding](`decode/2`). See
[RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648) Sections
[4](https://datatracker.ietf.org/doc/html/rfc4648#section-4) and
[5](https://datatracker.ietf.org/doc/html/rfc4648#section-5).

# `base64_string`
*not exported* 

```elixir
-type base64_string() :: [base64_alphabet()].
```

Base 64 encoded string.

# `byte_string`
*not exported* 

```elixir
-type byte_string() :: [byte()].
```

Arbitrary sequences of octets.

# `decode_options`
*not exported* 

```elixir
-type decode_options() :: #{padding => boolean(), mode => base64_mode()}.
```

Customizes the behaviour of the decode functions.

Default value if omitted entirely or partially is `#{mode => standard, padding => true}`.

The `mode` option can be one of the following:

- **`standard`** - Default. Decode the given string using the standard base64
  alphabet according to
  [RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4),
  that is `"+"` and `"/"` are representing bytes `62` and `63` respectively,
  while `"-"` and `"_"` are illegal characters.

- **`urlsafe`** - Decode the given string using the alternative "URL and
  Filename safe" base64 alphabet according to
  [RFC 4648 Section 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5),
  that is `"-"` and `"_"` are representing bytes `62` and `63` respectively,
  while `"+"` and `"/"` are illegal characters.

The `padding` option can be one of the following:

- **`true`** - Default. Checks the correct number of `=` padding characters at
  the end of the encoded string.

- **`false`** - Accepts an encoded string with missing `=` padding characters at
  the end.

# `encode_options`
*not exported* 

```elixir
-type encode_options() :: #{padding => boolean(), mode => base64_mode()}.
```

Customizes the behaviour of the encode functions.

Default value if omitted entirely or partially is `#{mode => standard, padding => true}`.

The `mode` option can be one of the following:

- **`standard`** - Default. Encode the given string using the standard base64
  alphabet according to
  [RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4).

- **`urlsafe`** - Encode the given string using the alternative "URL and
  Filename safe" base64 alphabet according to
  [RFC 4648 Section 5](https://datatracker.ietf.org/doc/html/rfc4648#section-5).

The `padding` option can be one of the following:

- **`true`** - Default. Appends correct number of `=` padding characters to the
  encoded string.

- **`false`** - Skips appending `=` padding characters to the encoded string.

# `decode`

```elixir
-spec decode(Base64) -> Data when Base64 :: base64_string() | base64_binary(), Data :: binary().
```

# `decode`
*since OTP 26.0* 

```elixir
-spec decode(Base64, Options) -> Data
                when
                    Base64 :: base64_string() | base64_binary(),
                    Options :: decode_options(),
                    Data :: binary().
```

Decodes a base64 string encoded using the standard alphabet according to
[RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4) to
plain ASCII.

The function will strip away any whitespace characters and check for the
the correct number of `=` padding characters at the end of the encoded string.

See `t:decode_options/0` for details on which options can be passed.

_Example_:
```erlang
1> base64:decode("AQIDBA==").
<<1,2,3,4>>
2> base64:decode("AQ ID BA==").
<<1,2,3,4>>
3> base64:decode("AQIDBA=").
** exception error: missing_padding
     in function  base64:decode_list/7 (base64.erl, line 734)
        *** data to decode is missing final = padding characters, if this is intended, use the `padding => false` option
4> base64:decode("AQIDBA=", #{ padding => false }).
<<1,2,3,4>>
```

# `decode_to_string`

```elixir
-spec decode_to_string(Base64) -> DataString
                          when Base64 :: base64_string() | base64_binary(), DataString :: byte_string().
```

Equivalent to [`decode(Base64)`](`decode/1`), but returns a `t:byte_string/0`.

# `decode_to_string`
*since OTP 26.0* 

```elixir
-spec decode_to_string(Base64, Options) -> DataString
                          when
                              Base64 :: base64_string() | base64_binary(),
                              Options :: decode_options(),
                              DataString :: byte_string().
```

Equivalent to [`decode(Base64, Options)`](`decode/2`), but returns a `t:byte_string/0`.

# `encode`

```elixir
-spec encode(Data) -> Base64 when Data :: byte_string() | binary(), Base64 :: base64_binary().
```

# `encode`
*since OTP 26.0* 

```elixir
-spec encode(Data, Options) -> Base64
                when
                    Data :: byte_string() | binary(),
                    Options :: encode_options(),
                    Base64 :: base64_binary().
```

Encodes a plain ASCII string into base64 using the alphabet indicated by the
`mode` option. The result is 33% larger than the data.

See `t:encode_options/0` for details on which options can be passed.

# `encode_to_string`

```elixir
-spec encode_to_string(Data) -> Base64String
                          when Data :: byte_string() | binary(), Base64String :: base64_string().
```

Equivalent to [`encode(Data)`](`encode/1`), but returns a `t:byte_string/0`.

# `encode_to_string`
*since OTP 26.0* 

```elixir
-spec encode_to_string(Data, Options) -> Base64String
                          when
                              Data :: byte_string() | binary(),
                              Options :: encode_options(),
                              Base64String :: base64_string().
```

Equivalent to [`encode(Data, Options)`](`encode/2`), but returns a `t:byte_string/0`.

# `mime_decode`

```elixir
-spec mime_decode(Base64) -> Data when Base64 :: base64_string() | base64_binary(), Data :: binary().
```

# `mime_decode`
*since OTP 26.0* 

```elixir
-spec mime_decode(Base64, Options) -> Data
                     when
                         Base64 :: base64_string() | base64_binary(),
                         Options :: decode_options(),
                         Data :: binary().
```

Decodes a base64 "mime" string encoded using the standard alphabet according to
[RFC 4648 Section 4](https://datatracker.ietf.org/doc/html/rfc4648#section-4) to
plain ASCII.

The function will strip away any illegal characters. It does *not* check for the
the correct number of `=` padding characters at the end of the encoded string.

See `t:decode_options/0` for details on which options can be passed.

_Example_:
```erlang
1> base64:mime_decode("AQIDBA==").
<<1,2,3,4>>
2> base64:mime_decode("AQIDB=A=").
<<1,2,3,4>>
```

# `mime_decode_to_string`

```elixir
-spec mime_decode_to_string(Base64) -> DataString
                               when
                                   Base64 :: base64_string() | base64_binary(),
                                   DataString :: byte_string().
```

Equivalent to [`mime_decode(Base64)`](`mime_decode/1`),
but returns a `t:byte_string/0`.

# `mime_decode_to_string`
*since OTP 26.0* 

```elixir
-spec mime_decode_to_string(Base64, Options) -> DataString
                               when
                                   Base64 :: base64_string() | base64_binary(),
                                   Options :: decode_options(),
                                   DataString :: byte_string().
```

Equivalent to [`mime_decode(Base64, Options)`](`mime_decode/2`),
but returns a `t:byte_string/0`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
