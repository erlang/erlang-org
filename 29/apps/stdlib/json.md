# `json`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/json.erl#L23)

A library for encoding and decoding JSON.

This module implements [EEP68](https://github.com/erlang/eep/blob/master/eeps/eep-0068.md).

Both encoder and decoder fully conform to
[RFC 8259](https://tools.ietf.org/html/rfc8259) and
[ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/)
standards. The decoder is tested using [JSONTestSuite](https://github.com/nst/JSONTestSuite).

# `array_finish_fun`
*since OTP 27.0* 

```erlang
-type array_finish_fun() :: fun((ArrayAcc :: dynamic(), OldAcc :: dynamic()) -> {dynamic(), dynamic()}).
```

# `array_push_fun`
*since OTP 27.0* 

```erlang
-type array_push_fun() :: fun((Value :: dynamic(), Acc :: dynamic()) -> NewAcc :: dynamic()).
```

# `array_start_fun`
*since OTP 27.0* 

```erlang
-type array_start_fun() :: fun((Acc :: dynamic()) -> ArrayAcc :: dynamic()).
```

# `continuation_state`
*since OTP 27.0* 

```erlang
-opaque continuation_state()
```

# `decode_value`
*since OTP 27.0* 

```erlang
-type decode_value() ::
          integer() |
          float() |
          boolean() |
          null |
          binary() |
          [decode_value()] |
          #{binary() => decode_value()}.
```

# `decoders`
*since OTP 27.0* 

```erlang
-type decoders() ::
          #{array_start => array_start_fun(),
            array_push => array_push_fun(),
            array_finish => array_finish_fun(),
            object_start => object_start_fun(),
            object_push => object_push_fun(),
            object_finish => object_finish_fun(),
            float => from_binary_fun(),
            integer => from_binary_fun(),
            string => from_binary_fun(),
            null => term()}.
```

# `encode_map`
*not exported* *since OTP 27.0* 

```erlang
-type encode_map(Value) :: #{binary() | atom() | integer() => Value}.
```

# `encode_value`
*since OTP 27.0* 

```erlang
-type encode_value() ::
          integer() |
          float() |
          boolean() |
          null |
          binary() |
          atom() |
          [encode_value()] |
          encode_map(encode_value()).
```

Simple JSON value encodeable with `json:encode/1`.

# `encoder`
*since OTP 27.0* 

```erlang
-type encoder() :: fun((dynamic(), encoder()) -> iodata()).
```

# `formatter`
*since OTP 27.0* 

```erlang
-type formatter() :: fun((Term :: dynamic(), Encoder :: formatter(), State :: map()) -> iodata()).
```

# `from_binary_fun`
*since OTP 27.0* 

```erlang
-type from_binary_fun() :: fun((binary()) -> dynamic()).
```

# `object_finish_fun`
*since OTP 27.0* 

```erlang
-type object_finish_fun() ::
          fun((ObjectAcc :: dynamic(), OldAcc :: dynamic()) -> {dynamic(), dynamic()}).
```

# `object_push_fun`
*since OTP 27.0* 

```erlang
-type object_push_fun() ::
          fun((Key :: dynamic(), Value :: dynamic(), Acc :: dynamic()) -> NewAcc :: dynamic()).
```

# `object_start_fun`
*since OTP 27.0* 

```erlang
-type object_start_fun() :: fun((Acc :: dynamic()) -> ObjectAcc :: dynamic()).
```

# `decode`
*since OTP 27.0* 

```erlang
-spec decode(binary()) -> decode_value().
```

Parses a JSON value from `Binary`.

Supports basic data mapping:

| **JSON** | **Erlang**             |
|----------|------------------------|
| Number   | `integer() \| float()` |
| Boolean  | `true \| false`        |
| Null     | `null`                 |
| String   | `binary()`             |
| Object   | `#{binary() => _}`     |

## Errors

* `error(unexpected_end)` if `Binary` contains incomplete JSON value
* `error({invalid_byte, Byte})` if `Binary` contains unexpected byte or invalid UTF-8 byte
* `error({unexpected_sequence, Bytes})` if `Binary` contains invalid UTF-8 escape

## Example

```erlang
> json:decode(<<"{\"foo\": 1}">>).
#{<<"foo">> => 1}
```

# `decode`
*since OTP 27.0* 

```erlang
-spec decode(binary(), dynamic(), decoders()) -> {Result :: dynamic(), Acc :: dynamic(), binary()}.
```

Parses a JSON value from `Binary`.

Similar to `decode/1` except the decoding process
can be customized with the callbacks specified in
`Decoders`. The callbacks will use the `Acc` value
as the initial accumulator.

Any leftover, unparsed data in `Binary` will be returned.

## Default callbacks

All callbacks are optional. If not provided, they will fall back to
implementations used by the `decode/1` function:

* for `array_start`: `fun(_) -> [] end`
* for `array_push`: `fun(Elem, Acc) -> [Elem | Acc] end`
* for `array_finish`: `fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end`
* for `object_start`: `fun(_) -> [] end`
* for `object_push`: `fun(Key, Value, Acc) -> [{Key, Value} | Acc] end`
* for `object_finish`: `fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end`
* for `float`: `fun erlang:binary_to_float/1`
* for `integer`: `fun erlang:binary_to_integer/1`
* for `string`: `fun (Value) -> Value end`
* for `null`: the atom `null`

## Errors

* `error({invalid_byte, Byte})` if `Binary` contains unexpected byte or invalid UTF-8 byte
* `error({unexpected_sequence, Bytes})` if `Binary` contains invalid UTF-8 escape
* `error(unexpected_end)` if `Binary` contains incomplete JSON value

## Example

Decoding object keys as atoms:

```erlang
> Push = fun(Key, Value, Acc) -> [{binary_to_existing_atom(Key), Value} | Acc] end.
> json:decode(<<"{\"foo\": 1}">>, ok, #{object_push => Push}).
{#{foo => 1},ok,<<>>}
```

# `decode_continue`
*since OTP 27.0* 

```erlang
-spec decode_continue(binary() | end_of_input, State :: continuation_state()) ->
                         {Result :: dynamic(), Acc :: dynamic(), binary()} |
                         {continue, continuation_state()}.
```

Continue parsing a stream of bytes of a JSON value.

Similar to `decode_start/3`, if the function returns `{continue, State}` and
there is no more data, use `end_of_input` instead of a binary.

```erlang
> {continue, State} = json:decode_start(<<"{\"foo\":">>, ok, #{}).
> json:decode_continue(<<"1}">>, State).
{#{foo => 1},ok,<<>>}
```
```erlang
> {continue, State} = json:decode_start(<<"123">>, ok, #{}).
> json:decode_continue(end_of_input, State).
{123,ok,<<>>}
```

# `decode_start`
*since OTP 27.0* 

```erlang
-spec decode_start(binary(), dynamic(), decoders()) ->
                      {Result :: dynamic(), Acc :: dynamic(), binary()} |
                      {continue, continuation_state()}.
```

Begin parsing a stream of bytes of a JSON value.

Similar to `decode/3` but returns when a complete JSON value can be parsed or
returns `{continue, State}` for incomplete data,
the `State` can be fed to the `decode_continue/2` function when more data is available.

# `encode`
*since OTP 27.0* 

```erlang
-spec encode(encode_value()) -> iodata().
```

Generates JSON corresponding to `Term`.

Supports basic data mapping:

| **Erlang**             | **JSON** |
|------------------------|----------|
| `integer() \| float()` | Number   |
| `true \| false `       | Boolean  |
| `null`                 | Null     |
| `binary()`             | String   |
| `atom()`               | String   |
| `list()`               | Array    |
| `#{binary() => _}`     | Object   |
| `#{atom() => _}`       | Object   |
| `#{integer() => _}`    | Object   |

This is equivalent to `encode(Term, fun json:encode_value/2)`.

## Examples

```erlang
> iolist_to_binary(json:encode(#{foo => <<"bar">>})).
<<"{\"foo\":\"bar\"}">>
```

# `encode`
*since OTP 27.0* 

```erlang
-spec encode(dynamic(), encoder()) -> iodata().
```

Generates JSON corresponding to `Term`.

Can be customised with the `Encoder` callback.
The callback will be recursively called for all the data
to be encoded and is expected to return the corresponding
encoded JSON as iodata.

Various `encode_*` functions in this module can be used
to help in constructing such callbacks.

## Examples

An encoder that uses a heuristic to differentiate object-like
lists of key-value pairs from plain lists:

```erlang
> encoder([{_, _} | _] = Value, Encode) -> json:encode_key_value_list(Value, Encode);
> encoder(Other, Encode) -> json:encode_value(Other, Encode).
> custom_encode(Value) -> json:encode(Value, fun(Value, Encode) -> encoder(Value, Encode) end).
> iolist_to_binary(custom_encode([{a, []}, {b, 1}])).
<<"{\"a\":[],\"b\":1}">>
```

# `encode_atom`
*since OTP 27.0* 

```erlang
-spec encode_atom(atom(), encoder()) -> iodata().
```

Default encoder for atoms used by `json:encode/1`.

Encodes the atom `null` as JSON `null`,
atoms `true` and `false` as JSON booleans,
and everything else as JSON strings calling the `Encode`
callback with the corresponding binary.

# `encode_binary`
*since OTP 27.0* 

```erlang
-spec encode_binary(binary()) -> iodata().
```

Default encoder for binaries as JSON strings used by `json:encode/1`.

## Errors

* `error(unexpected_end)` if the binary contains incomplete UTF-8 sequences.
* `error({invalid_byte, Byte})` if the binary contains invalid UTF-8 sequences.

# `encode_binary_escape_all`
*since OTP 27.0* 

```erlang
-spec encode_binary_escape_all(binary()) -> iodata().
```

Encoder for binaries as JSON strings producing pure-ASCII JSON.

For any non-ASCII unicode character, a corresponding `\\uXXXX` sequence is used.

## Errors

* `error(unexpected_end)` if the binary contains incomplete UTF-8 sequences.
* `error({invalid_byte, Byte})` if the binary contains invalid UTF-8 sequences.

# `encode_float`
*since OTP 27.0* 

```erlang
-spec encode_float(float()) -> iodata().
```

Default encoder for floats as JSON numbers used by `json:encode/1`.

# `encode_integer`
*since OTP 27.0* 

```erlang
-spec encode_integer(integer()) -> iodata().
```

Default encoder for integers as JSON numbers used by `json:encode/1`.

# `encode_key_value_list`
*since OTP 27.0* 

```erlang
-spec encode_key_value_list([{term(), term()}], encoder()) -> iodata().
```

Encoder for lists of key-value pairs as JSON objects.

Accepts lists with atom, binary, integer, or float keys.

# `encode_key_value_list_checked`
*since OTP 27.0* 

```erlang
-spec encode_key_value_list_checked([{term(), term()}], encoder()) -> iodata().
```

Encoder for lists of key-value pairs as JSON objects.

Accepts lists with atom, binary, integer, or float keys.
Verifies that no duplicate keys will be produced in the
resulting JSON object.

## Errors

Raises `error({duplicate_key, Key})` if there are duplicates.

# `encode_list`
*since OTP 27.0* 

```erlang
-spec encode_list(list(), encoder()) -> iodata().
```

Default encoder for lists as JSON arrays used by `json:encode/1`.

# `encode_map`
*since OTP 27.0* 

```erlang
-spec encode_map(encode_map(dynamic()), encoder()) -> iodata().
```

Default encoder for maps as JSON objects used by `json:encode/1`.

Accepts maps with atom, binary, integer, or float keys.

# `encode_map_checked`
*since OTP 27.0* 

```erlang
-spec encode_map_checked(map(), encoder()) -> iodata().
```

Encoder for maps as JSON objects.

Accepts maps with atom, binary, integer, or float keys.
Verifies that no duplicate keys will be produced in the
resulting JSON object.

## Errors

Raises `error({duplicate_key, Key})` if there are duplicates.

# `encode_value`
*since OTP 27.0* 

```erlang
-spec encode_value(dynamic(), encoder()) -> iodata().
```

Default encoder used by `json:encode/1`.

Recursively calls `Encode` on all the values in `Value`.

# `format`
*since OTP 27.1* 

```erlang
-spec format(Term :: encode_value()) -> iodata().
```

Generates formatted JSON corresponding to `Term`.

Similiar to `encode/1` but with added whitespaces for formatting.

```erlang
> io:put_chars(json:format(#{foo => <<"bar">>, baz => 52})).
{
  "baz": 52,
  "foo": "bar"
}
ok
```

# `format`
*since OTP 27.1* 

```erlang
-spec format(Term :: encode_value(), Opts :: map()) -> iodata();
            (Term :: dynamic(), Encoder :: formatter()) -> iodata().
```

Generates formatted JSON corresponding to `Term`.

Equivalent to `format(Term, fun json:format_value/3, Options)` or `format(Term, Encoder, #{})`

# `format`
*since OTP 27.1* 

```erlang
-spec format(Term :: dynamic(), Encoder :: formatter(), Options :: map()) -> iodata().
```

Generates formatted JSON corresponding to `Term`.

Similar to `encode/2`, can be customised with the `Encoder` callback and `Options`.

`Options` can include 'indent' to specify number of spaces per level and 'max' which loosely limits
the width of lists.

The `Encoder` will get a 'State' argument which contains the 'Options' maps merged with other data
when recursing through 'Term'.

`format_value/3` or various `encode_*` functions in this module can be used
to help in constructing such callbacks.

```erlang
> formatter({posix_time, SysTimeSecs}, Encode, State) ->
    TimeStr = calendar:system_time_to_rfc3339(SysTimeSecs, [{offset, "Z"}]),
    json:format_value(unicode:characters_to_binary(TimeStr), Encode, State);
> formatter(Other, Encode, State) -> json:format_value(Other, Encode, State).
>
> Fun = fun(Value, Encode, State) -> formatter(Value, Encode, State) end.
> Options = #{indent => 4}.
> Term = #{id => 1, time => {posix_time, erlang:system_time(seconds)}}.
>
> io:put_chars(json:format(Term, Fun, Options)).
{
    "id": 1,
    "time": "2024-05-23T16:07:48Z"
}
ok
```

# `format_key_value_list`
*since OTP 27.2* 

```erlang
-spec format_key_value_list([{term(), term()}], Encode :: formatter(), State :: map()) -> iodata().
```

Format function for lists of key-value pairs as JSON objects.

Accepts lists with atom, binary, integer, or float keys.

# `format_key_value_list_checked`
*since OTP 27.2* 

```erlang
-spec format_key_value_list_checked([{term(), term()}], Encoder :: formatter(), State :: map()) ->
                                       iodata().
```

Format function for lists of key-value pairs as JSON objects.

Accepts lists with atom, binary, integer, or float keys.
Verifies that no duplicate keys will be produced in the
resulting JSON object.

## Errors

Raises `error({duplicate_key, Key})` if there are duplicates.

# `format_value`
*since OTP 27.1* 

```erlang
-spec format_value(Value :: dynamic(), Encode :: formatter(), State :: map()) -> iodata().
```

Default format function used by `json:format/1`.

Recursively calls `Encode` on all the values in `Value`,
and indents objects and lists.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
