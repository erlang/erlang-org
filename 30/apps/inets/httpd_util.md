# `httpd_util`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_server/httpd_util.erl#L23)

Miscellaneous utility functions to be used when implementing Erlang web server
API modules.

This module provides the Erlang web server API module programmer with
miscellaneous utility functions.

> #### Note {: .info }
>
> Note the module is only recommended for using with httpd - for other cases it
> should be considered as deprecated.

[](){: #convert_request_date }

### See also

`m:httpd`

# `convert_request_date`

```erlang
-spec convert_request_date(DateString) -> ErlDate | bad_date
                              when DateString :: string(), ErlDate :: calendar:datetime().
```

`convert_request_date/1` converts `DateString` to
the Erlang date format. `DateString` must be in one of the three date formats
defined in [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt).

# `create_etag`

```erlang
-spec create_etag(FileInfo) -> Etag when FileInfo :: file:file_info(), Etag :: string().
```

`create_etag/1` calculates the Etag for a file from its size and time for last
modification. `FileInfo` is a record defined in `kernel/include/file.hrl`.

# `day`

```erlang
-spec day(NthDayOfWeek) -> DayOfWeek when NthDayOfWeek :: 1..7, DayOfWeek :: string().
```

`day/1` converts the day of the week (`NthDayOfWeek`) from an integer
(1-7) to an abbreviated string, that is:

1 = "Mon", 2 = "Tue", ..., 7 = "Sat".

# `lookup`

```erlang
-spec lookup(EtsTable, Key) -> Result
                when EtsTable :: ets:table(), Key :: term(), Result :: term() | undefined.
```

# `lookup`

```erlang
-spec lookup(EtsTable, Key, Undefined) -> Result
                when
                    EtsTable :: ets:table(),
                    Key :: term(),
                    Undefined :: term(),
                    Result :: term() | Undefined.
```

`lookup` extracts `{Key, Value}` tuples from `ETSTable` and returns the `Value`
associated with `Key`. If `ETSTable` is of type `bag`, only the first `Value`
associated with `Key` is returned. `lookup/2` returns `undefined`
and `lookup/3` returns `Undefined` if no `Value` is found.

# `lookup_mime`

```erlang
-spec lookup_mime(ConfigDB, Suffix) -> MimeType
                     when ConfigDB :: ets:tid(), Suffix :: string(), MimeType :: string() | undefined.
```

# `lookup_mime`

```erlang
-spec lookup_mime(ConfigDB, Suffix, Undefined) -> MimeType
                     when
                         ConfigDB :: ets:tid(),
                         Suffix :: string(),
                         Undefined :: term(),
                         MimeType :: string() | Undefined.
```

`lookup_mime` returns the MIME type associated with a specific file suffix as
specified in the file `mime.types` (located in the config directory).

# `lookup_mime_default`

```erlang
-spec lookup_mime_default(ConfigDB, Suffix) -> MimeType
                             when
                                 ConfigDB :: ets:tid(),
                                 Suffix :: string(),
                                 MimeType :: string() | undefined.
```

# `lookup_mime_default`

```erlang
-spec lookup_mime_default(ConfigDB, Suffix, Undefined) -> MimeType
                             when
                                 ConfigDB :: ets:tid(),
                                 Suffix :: string(),
                                 Undefined :: term(),
                                 MimeType :: string() | Undefined.
```

`lookup_mime_default` returns the MIME type associated with a specific file
suffix as specified in the `mime.types` file (located in the config directory).
If no appropriate association is found, the value of `DefaultType` is returned.

# `message`

```erlang
-spec message(StatusCode, PhraseArgs, ConfigDB) -> Message
                 when
                     StatusCode ::
                         301 | 304 | 400 | 401 | 403 | 404 | 408 | 412 | 413 | 414 | 500 | 501 | 503 |
                         504,
                     PhraseArgs :: term(),
                     ConfigDB :: ets:tid(),
                     Message :: string().
```

`message/3` returns an informative HTTP 1.1 status string in
HTML. Each `StatusCode` requires a specific `PhraseArgs`:

- **`301`** - `t:string/0`: A URL pointing at the new document position.

- **`400 | 401 | 500`** - `none` (no `PhraseArgs`).

- **`403 | 404`** - `t:string/0`: A `Request-URI` as described in
  [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt).

- **`501`** - `{Method, RequestURI, HTTPVersion}`: The HTTP `Method`,
  `Request-URI`, and `HTTP-Version` as defined in RFC 2616.

- **`504`** - `t:string/0`: A string describing why the service was unavailable.

# `month`

```erlang
-spec month(NthMonth) -> Month when NthMonth :: 1..12, Month :: string().
```

`month/1` converts the month `NthMonth` as an integer (1-12) to an
abbreviated string, that is:

1 = "Jan", 2 = "Feb", ..., 12 = "Dec".

# `multi_lookup`

```erlang
-spec multi_lookup(EtsTable, Key) -> Result
                      when EtsTable :: ets:tid(), Key :: term(), Result :: list() | [term()].
```

`multi_lookup` extracts all `{Key, Value}` tuples from an `ETSTable` and returns
_all_ `Values` associated with `Key` in a list.

# `reason_phrase`

```erlang
-spec reason_phrase(StatusCode) -> Description
                       when
                           StatusCode ::
                               100 | 101 | 102 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 207 | 226 |
                               300 | 301 | 302 | 303 | 304 | 305 | 306 | 307 | 308 | 400 | 401 | 402 |
                               403 | 404 | 405 | 406 | 407 | 408 | 409 | 410 | 411 | 412 | 413 | 414 |
                               415 | 416 | 417 | 422 | 423 | 424 | 425 | 426 | 500 | 501 | 502 | 503 |
                               504 | 505 | 507,
                           Description :: string().
```

`reason_phrase` returns `Description` of an HTTP 1.1 `StatusCode`, for example,
200 is "OK" and 201 is "Created". For more information, see
[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt).

# `rfc1123_date`

```erlang
-spec rfc1123_date() -> RFC1123Date when RFC1123Date :: string().
```

# `rfc1123_date`

```erlang
-spec rfc1123_date(LocalTime) -> RFC1123Date
                      when
                          LocalTime :: calendar:datetime() | undefined,
                          RFC1123Date :: string() | undefined.
```

`rfc1123_date/0` returns the current date in RFC 1123 format. `rfc_date/1`
converts the date in the Erlang format to the RFC 1123 date format.

# `split`

```erlang
-spec split(String, RegExp, N) -> SplitRes
               when
                   String :: string(),
                   RegExp :: string(),
                   N :: non_neg_integer(),
                   SplitRes :: {ok, FieldList} | {error, term()},
                   FieldList :: [string()].
```

`split/3` splits `String` in `N` chunks using `RegExp`. `split/3` is equivalent
to `re:split/3` with the exception that `N` defines the maximum number of
fields in `FieldList`.

# `split_path`

```erlang
-spec split_path(URIString) -> {Path, QueryStringOrPathInfo}
                    when URIString :: string(), Path :: string(), QueryStringOrPathInfo :: string().
```

`split_path/1` splits `RequestLine` in a file reference
(`Path`), and a `QueryString` or a `PathInfo` string as specified in
[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt). A `QueryString` is isolated
from `Path` with a question mark (`?`) and `PathInfo` with a slash (/). In the
case of a `QueryString`, everything before `?` is a `Path` and everything after
`?` is a `QueryString`. In the case of a `PathInfo`, `RequestLine` is scanned
from left-to-right on the hunt for longest possible `Path` being a file or a
directory. Everything after the longest possible `Path`, isolated with a `/`, is
regarded as `PathInfo`

# `split_script_path`

```erlang
-spec split_script_path(URIString) -> Split
                           when
                               URIString :: string(),
                               Split :: not_a_script | {Path, {PathInfo, QueryString}} | {Path, []},
                               Path :: string(),
                               QueryString :: string(),
                               PathInfo :: string().
```

`split_script_path/1` is equivalent to `split_path/1` with one exception. If
the longest possible path is not a regular, accessible, and executable file,
then `not_a_script` is returned.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
