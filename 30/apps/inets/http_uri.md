# `http_uri`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_lib/http_uri.erl#L22)

Old URI utility module, use uri_string instead

This module is deprecated since OTP 23. Use the module `m:uri_string` to
properly handle URIs, this is the recommended module since OTP 21.

### Data types

Type definitions that are related to URI:

- **`uri_part() = [byte()] | binary()`** - Syntax according to the URI
  definition in RFC 3986, for example, "http://www.erlang.org/"

For more information about URI, see
[RFC 3986](http://www.ietf.org/rfc/rfc3986.txt).

# `decode`
*since OTP R15B01* 

> This function is deprecated. http_uri:decode/1 is deprecated; use uri_string:unquote function instead.

```erlang
-spec decode(QuotedData) -> Data when QuotedData :: unicode:chardata(), Data :: unicode:chardata().
```

Decodes a possibly percent encoded URI part

> #### Warning {: .warning }
>
> Use `uri_string:unquote/1` instead

# `encode`
*since OTP R15B01* 

> This function is deprecated. http_uri:encode/1 is deprecated; use uri_string:quote function instead.

```erlang
-spec encode(Data) -> QuotedData when Data :: unicode:chardata(), QuotedData :: unicode:chardata().
```

Performs percent encoding.

> #### Warning {: .warning }
>
> Use `uri_string:quote/1` instead

---

*Consult [api-reference.md](api-reference.md) for complete listing*
