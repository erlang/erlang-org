# `uri_string`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/uri_string.erl#L226)

URI processing functions.

This module contains functions for parsing and handling URIs
([RFC 3986](https://www.ietf.org/rfc/rfc3986.txt)) and form-urlencoded query
strings ([HTML 5.2](https://www.w3.org/TR/html52/)).

Parsing and serializing non-UTF-8 form-urlencoded query strings are also
supported ([HTML 5.0](https://www.w3.org/TR/html50/)).

A URI is an identifier consisting of a sequence of characters matching the
syntax rule named _URI_ in [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).

The generic URI syntax consists of a hierarchical sequence of components
referred to as the scheme, authority, path, query, and fragment:

```text
    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
    hier-part   = "//" authority path-abempty
                   / path-absolute
                   / path-rootless
                   / path-empty
    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    authority   = [ userinfo "@" ] host [ ":" port ]
    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

    reserved    = gen-delims / sub-delims
    gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
    sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                / "*" / "+" / "," / ";" / "="

    unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
```

The interpretation of a URI depends only on the characters used and not on how
those characters are represented in a network protocol.

The functions implemented by this module cover the following use cases:

- Parsing URIs into its components and returing a map: `parse/1`
- Recomposing a map of URI components into a URI string: `recompose/1`
- Changing inbound binary and percent-encoding of URIs: `transcode/2`
- Transforming URIs into a normalized form: `normalize/1`, `normalize/2`
- Composing form-urlencoded query strings from a list of key-value pairs:
  `compose_query/1`, `compose_query/2`
- Dissecting form-urlencoded query strings into a list of key-value pairs:
  `dissect_query/1`
- Decoding percent-encoded triplets in URI map or a specific component of URI:
  `percent_decode/1`
- Preparing and retrieving application specific data included in URI
  components:
  `quote/1`, `quote/2`, `unquote/1`

There are four different encodings present during the handling of URIs:

- Inbound binary encoding in binaries
- Inbound percent-encoding in lists and binaries
- Outbound binary encoding in binaries
- Outbound percent-encoding in lists and binaries

Functions with `t:uri_string/0` argument accept lists, binaries and mixed lists
(lists with binary elements) as input type. All of the functions but
[`transcode/2`](`transcode/2`) expects input as lists of unicode codepoints,
UTF-8 encoded binaries and UTF-8 percent-encoded URI parts ("%C3%B6" corresponds
to the unicode character "ö").

Unless otherwise specified the return value type and encoding are the same as
the input type and encoding. That is, binary input returns binary output, list
input returns a list output but mixed input returns list output.

In case of lists there is only percent-encoding. In binaries, however, both
binary encoding and percent-encoding shall be considered.
[`transcode/2`](`transcode/2`) provides the means to convert between the
supported encodings, it takes a `t:uri_string/0` and a list of options
specifying inbound and outbound encodings.

[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) does not mandate any specific
character encoding and it is usually defined by the protocol or surrounding
text. This library takes the same assumption, binary and percent-encoding are
handled as one configuration unit, they cannot be set to different values.

Quoting functions are intended to be used by URI producing application during
component preparation or retrieval phase to avoid conflicts between data and
characters used in URI syntax. Quoting functions use percent encoding, but with
different rules than for example during execution of
[`recompose/1`](`recompose/1`). It is user responsibility to provide quoting
functions with application data only and using their output to combine an URI
component.  
Quoting functions can for instance be used for constructing a path component
with a segment containing '/' character which should not collide with '/' used
as general delimiter in path component.

# `error`
*since OTP 21.0* 

```erlang
-type error() :: {error, atom(), term()}.
```

Error tuple indicating the type of error. Possible values of the second
component:

- `invalid_character`
- `invalid_encoding`
- `invalid_input`
- `invalid_map`
- `invalid_percent_encoding`
- `invalid_scheme`
- `invalid_uri`
- `invalid_utf8`
- `missing_value`

The third component is a term providing additional information about the cause
of the error.

# `uri_map`
*since OTP 21.0* 

```erlang
-type uri_map() ::
          #{fragment => unicode:chardata(),
            host => unicode:chardata(),
            path => unicode:chardata(),
            port => non_neg_integer() | undefined,
            query => unicode:chardata(),
            scheme => unicode:chardata(),
            userinfo => unicode:chardata()}.
```

Map holding the main components of a URI.

# `uri_string`
*since OTP 21.0* 

```erlang
-type uri_string() :: iodata().
```

List of unicode codepoints, a UTF-8 encoded binary, or a mix of the two,
representing an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant URI
(_percent-encoded form_). A URI is a sequence of characters from a very limited
set: the letters of the basic Latin alphabet, digits, and a few special
characters.

# `allowed_characters`
*since OTP 23.2* 

```erlang
-spec allowed_characters() -> [{atom(), list()}].
```

This is a utility function meant to be used in the shell for printing the
allowed characters in each major URI component, and also in the most important
characters sets.

Note that this function does not replace the ABNF rules defined by the standards,
these character sets are derived directly from those aformentioned rules. For more
information see the
[Uniform Resource Identifiers](uri_string_usage.md#percent_encoding) chapter in
stdlib's Users Guide.

# `compose_query`
*since OTP 21.0* 

```erlang
-spec compose_query(QueryList) -> QueryString
                       when
                           QueryList :: [{unicode:chardata(), unicode:chardata() | true}],
                           QueryString :: uri_string() | error().
```

Composes a form-urlencoded `QueryString` based on a `QueryList`, a list of
non-percent-encoded key-value pairs.

Form-urlencoding is defined in section 4.10.21.6 of the [HTML 5.2](https://www.w3.org/TR/html52/)
specification and in section 4.10.22.6 of the [HTML 5.0](https://www.w3.org/TR/html50/)
specification for non-UTF-8 encodings.

See also the opposite operation `dissect_query/1`.

_Example:_

```erlang
1> uri_string:compose_query([{"foo bar","1"},{"city","örebro"}]).
"foo+bar=1&city=%C3%B6rebro"
2> uri_string:compose_query([{<<"foo bar">>,<<"1">>},
2> {<<"city">>,<<"örebro"/utf8>>}]).
<<"foo+bar=1&city=%C3%B6rebro">>
```

# `compose_query`
*since OTP 21.0* 

```erlang
-spec compose_query(QueryList, Options) -> QueryString
                       when
                           QueryList :: [{unicode:chardata(), unicode:chardata() | true}],
                           Options :: [{encoding, atom()}],
                           QueryString :: uri_string() | error().
```

Same as [`compose_query/1`](`compose_query/1`) but with an additional `Options`
parameter, that controls the encoding ("charset") used by the encoding
algorithm.

There are two supported encodings: `utf8` (or `unicode`) and `latin1`.

Each character in the entry's name and value that cannot be expressed using the
selected character encoding, is replaced by a string consisting of a U+0026
AMPERSAND character (&), a "#" (U+0023) character, one or more ASCII digits
representing the Unicode code point of the character in base ten, and finally a
";" (U+003B) character.

Bytes that are out of the range 0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A,
0x5F, 0x61 to 0x7A, are percent-encoded (U+0025 PERCENT SIGN character (%)
followed by uppercase ASCII hex digits representing the hexadecimal value of the
byte).

See also the opposite operation `dissect_query/1`.

_Example:_

```erlang
1> uri_string:compose_query([{"foo bar","1"},{"city","örebro"}],
1> [{encoding, latin1}]).
"foo+bar=1&city=%F6rebro"
2> uri_string:compose_query([{<<"foo bar">>,<<"1">>},
2> {<<"city">>,<<"東京"/utf8>>}], [{encoding, latin1}]).
<<"foo+bar=1&city=%26%2326481%3B%26%2320140%3B">>
```

# `dissect_query`
*since OTP 21.0* 

```erlang
-spec dissect_query(QueryString) -> QueryList
                       when
                           QueryString :: uri_string(),
                           QueryList :: [{unicode:chardata(), unicode:chardata() | true}] | error().
```

Dissects an urlencoded `QueryString` and returns a `QueryList`, a list of
non-percent-encoded key-value pairs.

Form-urlencoding is defined in section 4.10.21.6 of the [HTML 5.2](https://www.w3.org/TR/html52/)
specification and in section 4.10.22.6 of the [HTML 5.0](https://www.w3.org/TR/html50/)
specification for non-UTF-8 encodings.

See also the opposite operation `compose_query/1`.

_Example:_

```erlang
1> uri_string:dissect_query("foo+bar=1&city=%C3%B6rebro").
[{"foo bar","1"},{"city","örebro"}]
2> uri_string:dissect_query(<<"foo+bar=1&city=%26%2326481%3B%26%2320140%3B">>).
[{<<"foo bar">>,<<"1">>},
 {<<"city">>,<<230,157,177,228,186,172>>}]
```

# `normalize`
*since OTP 21.0* 

```erlang
-spec normalize(URI) -> NormalizedURI
                   when URI :: uri_string() | uri_map(), NormalizedURI :: uri_string() | error().
```

Transforms an `URI` into a normalized form using Syntax-Based Normalization as
defined by [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).

This function implements case normalization, percent-encoding normalization,
path segment normalization and scheme based normalization for HTTP(S) with basic
support for FTP, SSH, SFTP and TFTP.

_Example:_

```erlang
1> uri_string:normalize("/a/b/c/./../../g").
"/a/g"
2> uri_string:normalize(<<"mid/content=5/../6">>).
<<"mid/6">>
3> uri_string:normalize("http://localhost:80").
"http://localhost/"
4> uri_string:normalize(#{scheme => "http",port => 80,path => "/a/b/c/./../../g",
4> host => "localhost-örebro"}).
"http://localhost-%C3%B6rebro/a/g"
```

# `normalize`
*since OTP 21.0* 

```erlang
-spec normalize(URI, Options) -> NormalizedURI
                   when
                       URI :: uri_string() | uri_map(),
                       Options :: [return_map],
                       NormalizedURI :: uri_string() | uri_map() | error().
```

Same as [`normalize/1`](`normalize/1`) but with an additional `Options`
parameter, that controls whether the normalized URI shall be returned as an
uri_map().

There is one supported option: `return_map`.

_Example:_

```erlang
1> uri_string:normalize("/a/b/c/./../../g", [return_map]).
#{path => "/a/g"}
2> uri_string:normalize(<<"mid/content=5/../6">>, [return_map]).
#{path => <<"mid/6">>}
3> uri_string:normalize("http://localhost:80", [return_map]).
#{scheme => "http",path => "/",host => "localhost"}
4> uri_string:normalize(#{scheme => "http",port => 80,path => "/a/b/c/./../../g",
4> host => "localhost-örebro"}, [return_map]).
#{scheme => "http",path => "/a/g",host => "localhost-örebro"}
```

# `parse`
*since OTP 21.0* 

```erlang
-spec parse(URIString) -> URIMap when URIString :: uri_string(), URIMap :: uri_map() | error().
```

Parses an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`t:uri_string/0` into a `t:uri_map/0`, that holds the parsed components of the
`URI`. If parsing fails, an error tuple is returned.

See also the opposite operation `recompose/1`.

_Example:_

```erlang
1> uri_string:parse("foo://user@example.com:8042/over/there?name=ferret#nose").
#{fragment => "nose",host => "example.com",
  path => "/over/there",port => 8042,query => "name=ferret",
  scheme => foo,userinfo => "user"}
2> uri_string:parse(<<"foo://user@example.com:8042/over/there?name=ferret">>).
#{host => <<"example.com">>,path => <<"/over/there">>,
  port => 8042,query => <<"name=ferret">>,scheme => <<"foo">>,
  userinfo => <<"user">>}
```

# `percent_decode`
*since OTP 23.2* 

```erlang
-spec percent_decode(URI) -> Result
                        when
                            URI :: uri_string() | uri_map(),
                            Result ::
                                uri_string() |
                                uri_map() |
                                {error, {invalid, {atom(), {term(), term()}}}} |
                                error().
```

Decodes all percent-encoded triplets in the input that can be both a
`t:uri_string/0` and a `t:uri_map/0`.

Note, that this function performs raw decoding and it shall be used on already
parsed URI components. Applying this function directly on a standard URI can
effectively change it.

If the input encoding is not UTF-8, an error tuple is returned.

_Example:_

```erlang
1> uri_string:percent_decode(#{host => "localhost-%C3%B6rebro",path => [],
1> scheme => "http"}).
#{host => "localhost-örebro",path => [],scheme => "http"}
2> uri_string:percent_decode(<<"%C3%B6rebro">>).
<<"örebro"/utf8>>
```

> #### Warning {: .warning }
>
> Using `uri_string:percent_decode/1` directly on a URI is not safe. This
> example shows, that after each consecutive application of the function the
> resulting URI will be changed. None of these URIs refer to the same resource.
>
> ```erlang
> 3> uri_string:percent_decode(<<"http://local%252Fhost/path">>).
> <<"http://local%2Fhost/path">>
> 4> uri_string:percent_decode(<<"http://local%2Fhost/path">>).
> <<"http://local/host/path">>
> ```

# `quote`
*since OTP 25.0* 

```erlang
-spec quote(Data) -> QuotedData when Data :: unicode:chardata(), QuotedData :: unicode:chardata().
```

Replaces characters out of unreserved set with their percent encoded
equivalents.

Unreserved characters defined in
[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) are not quoted.

_Example:_

```erlang
1> uri_string:quote("SomeId/04").
"SomeId%2F04"
2> uri_string:quote(<<"SomeId/04">>).
<<"SomeId%2F04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.

# `quote`
*since OTP 25.0* 

```erlang
-spec quote(Data, Safe) -> QuotedData
               when Data :: unicode:chardata(), Safe :: string(), QuotedData :: unicode:chardata().
```

Same as [`quote/1`](`quote/1`), but `Safe` allows user to provide a list of
characters to be protected from encoding.

_Example:_

```erlang
1> uri_string:quote("SomeId/04", "/").
"SomeId/04"
2> uri_string:quote(<<"SomeId/04">>, "/").
<<"SomeId/04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.

# `recompose`
*since OTP 21.0* 

```erlang
-spec recompose(URIMap) -> URIString when URIMap :: uri_map(), URIString :: uri_string() | error().
```

Creates an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`URIString` (percent-encoded), based on the components of `URIMap`. If the
`URIMap` is invalid, an error tuple is returned.

See also the opposite operation `parse/1`.

_Example:_

```erlang
1> URIMap = #{fragment => "nose", host => "example.com", path => "/over/there",
1> port => 8042, query => "name=ferret", scheme => "foo", userinfo => "user"}.
#{fragment => "nose",host => "example.com",
  path => "/over/there",port => 8042,query => "name=ferret",
  scheme => "foo",userinfo => "user"}

2> uri_string:recompose(URIMap).
"foo://example.com:8042/over/there?name=ferret#nose"
```

# `resolve`
*since OTP 22.3* 

```erlang
-spec resolve(RefURI, BaseURI) -> TargetURI
                 when
                     RefURI :: uri_string() | uri_map(),
                     BaseURI :: uri_string() | uri_map(),
                     TargetURI :: uri_string() | error().
```

Convert a `RefURI` reference that might be relative to a given base URI into the
parsed components of the reference's target, which can then be recomposed to
form the target URI.

_Example:_

```erlang
1> uri_string:resolve("/abs/ol/ute", "http://localhost/a/b/c?q").
"http://localhost/abs/ol/ute"
2> uri_string:resolve("../relative", "http://localhost/a/b/c?q").
"http://localhost/a/relative"
3> uri_string:resolve("http://localhost/full", "http://localhost/a/b/c?q").
"http://localhost/full"
4> uri_string:resolve(#{path => "path", query => "xyz"}, "http://localhost/a/b/c?q").
"http://localhost/a/b/path?xyz"
```

# `resolve`
*since OTP 22.3* 

```erlang
-spec resolve(RefURI, BaseURI, Options) -> TargetURI
                 when
                     RefURI :: uri_string() | uri_map(),
                     BaseURI :: uri_string() | uri_map(),
                     Options :: [return_map],
                     TargetURI :: uri_string() | uri_map() | error().
```

Same as [`resolve/2`](`resolve/2`) but with an additional `Options` parameter,
that controls whether the target URI shall be returned as an uri_map(). There is
one supported option: `return_map`.

_Example:_

```erlang
1> uri_string:resolve("/abs/ol/ute", "http://localhost/a/b/c?q", [return_map]).
#{host => "localhost",path => "/abs/ol/ute",scheme => "http"}
2> uri_string:resolve(#{path => "/abs/ol/ute"}, #{scheme => "http",
2> host => "localhost", path => "/a/b/c?q"}, [return_map]).
#{host => "localhost",path => "/abs/ol/ute",scheme => "http"}
```

# `transcode`
*since OTP 21.0* 

```erlang
-spec transcode(URIString, Options) -> Result
                   when
                       URIString :: uri_string(),
                       Options ::
                           [{in_encoding, unicode:encoding()} | {out_encoding, unicode:encoding()}],
                       Result :: uri_string() | error().
```

Transcodes an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`URIString`, where `Options` is a list of tagged tuples, specifying the inbound
(`in_encoding`) and outbound (`out_encoding`) encodings.

`in_encoding` and `out_encoding` specifies both binary encoding and percent-encoding
for the input and output data. Mixed encoding, where binary encoding is not the same as
percent-encoding, is not supported. If an argument is invalid, an error tuple is
returned.

_Example:_

```erlang
1> uri_string:transcode(<<"foo%00%00%00%F6bar"/utf32>>,
1> [{in_encoding, utf32},{out_encoding, utf8}]).
<<"foo%C3%B6bar"/utf8>>
2> uri_string:transcode("foo%F6bar", [{in_encoding, latin1},
2> {out_encoding, utf8}]).
"foo%C3%B6bar"
```

# `unquote`
*since OTP 25.0* 

```erlang
-spec unquote(QuotedData) -> Data when QuotedData :: unicode:chardata(), Data :: unicode:chardata().
```

Percent decode characters.

_Example:_

```erlang
1> uri_string:unquote("SomeId%2F04").
"SomeId/04"
2> uri_string:unquote(<<"SomeId%2F04">>).
<<"SomeId/04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
