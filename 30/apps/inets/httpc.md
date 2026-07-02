# `httpc`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_client/httpc.erl#L29)

An HTTP/1.1 client

This module provides the API to an HTTP/1.1 compatible client according to
[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt). Caching is not supported.

> #### Note {: .info }
>
> When starting the `Inets` application, a manager process for the default
> profile is started. The functions in this API that do not explicitly use a
> profile accesses the default profile. A profile keeps track of proxy options,
> cookies, and other options that can be applied to more than one request.
>
> If the scheme `https` is used, the `SSL` application must be started. When
> `https` links need to go through a proxy, the CONNECT method extension to
> HTTP-1.1 is used to establish a tunnel and then the connection is upgraded to
> TLS. However, "TLS upgrade" according to
> [RFC 2817](http://www.ietf.org/rfc/rfc2817.txt) is not supported.
>
> Pipelining is only used if the pipeline time-out is set, otherwise persistent
> connections without pipelining are used. That is, the client always waits for
> the previous response before sending the next request.

Some examples are provided in the [Inets User's Guide](http_client.md).

## HTTP client service start & stop

An HTTP client can be configured to start when starting the `Inets` application
or started dynamically in runtime by calling the `Inets` application API
`inets:start(httpc, ServiceConfig)` or `inets:start(httpc, ServiceConfig, How)`,
see `m:inets`. The configuration options are as follows:

- **\{profile, Profile :: atom() | pid()\}** - Name of the profile. This option
  is mandatory.

- **\{data_dir, Path :: string()\}** - Directory where the profile can save
  persistent data. If omitted, all cookies are treated as session cookies.
  `Path` represents a file path or directory path.

The client can be stopped using [`inets:stop(httpc, Pid)`](`inets:stop/2`) or
[`inets:stop(httpc, Profile)`](`inets:stop/2`).

> #### Warning {: .warning }
>
> Please note that `httpc` normalizes input URIs before internal processing and
> special care shall be taken when the URI has percent ("%") characters. A
> percent serves as the indicator for percent-encoded octets and it must be
> percent-encoded as "%25" for that octet to be used as data within the URI.
>
> For example, in order to send an `HTTP GET` request with the URI
> `http://localhost/foo%25bar`, the percent character must be percent-encoded
> when creating the request: `httpc:request("http://localhost/foo%2525bar").`

## See also

[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt), `m:inets`, `m:gen_tcp`, `m:ssl`

# `cancel_request`
*since OTP R13B04* 

```erlang
-spec cancel_request(RequestId) -> ok when RequestId :: any().
```

# `cancel_request`
*since OTP R13B04* 

```erlang
-spec cancel_request(RequestId, Profile) -> ok when RequestId :: any(), Profile :: atom() | pid().
```

Cancels an asynchronous HTTP request. Notice that this does not guarantee that
the request response is not delivered. Because it is asynchronous, the request
can already have been completed when the cancellation arrives.

# `cookie_header`
*since OTP R13B04* 

```erlang
-spec cookie_header(Url) -> HttpHeader | {error, Reason}
                       when
                           Url :: uri_string:uri_string(),
                           HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                           Reason :: term().
```

# `cookie_header`
*since OTP R13B04* 

```erlang
-spec cookie_header(Url, ProfileOrOpts) -> HttpHeader | {error, Reason}
                       when
                           Url :: uri_string:uri_string(),
                           HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                           ProfileOrOpts :: Profile | Opts,
                           Profile :: atom() | pid(),
                           Opts :: [CookieHeaderOpt],
                           CookieHeaderOpt :: {ipv6_host_with_brackets, boolean()},
                           Reason :: term().
```

Returns the cookie header that would have been sent when making a request to
`Url` using profile `Profile`. If no profile is specified, the default profile
is used.

Option `ipv6_host_with_bracket` deals with how to parse IPv6 addresses. For
details, see argument `Options` of [request/4,5](`request/4`).

# `cookie_header`
*since OTP R15B* 

```erlang
-spec cookie_header(Url, Opts, Profile) -> HttpHeader | {error, Reason}
                       when
                           Url :: uri_string:uri_string(),
                           HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                           Profile :: atom() | pid(),
                           Opts :: [CookieHeaderOpt],
                           CookieHeaderOpt :: {ipv6_host_with_brackets, boolean()},
                           Reason :: term().
```

Returns the cookie header that would have been sent when making a request to
`Url` using profile `Profile`. If no profile is specified, the default profile
is used.

Option `ipv6_host_with_bracket` deals with how to parse IPv6 addresses. For
details, see argument `Options` of [request/4,5](`request/4`).

# `get_options`
*since OTP R15B01* 

```erlang
-spec get_options(OptionItems) -> {ok, Values} | {error, Reason}
                     when
                         OptionItems :: all | [OptionItem],
                         OptionItem ::
                             proxy | https_proxy | max_sessions | max_connections_open |
                             keep_alive_timeout | max_keep_alive_length | pipeline_timeout |
                             max_pipeline_length | cookies | ipfamily | ip | port | socket_opts |
                             verbose | unix_socket,
                         Values :: [{OptionItem, term()}],
                         Reason :: term().
```

# `get_options`
*since OTP R15B01* 

```erlang
-spec get_options(OptionItems, Profile) -> {ok, Values} | {error, Reason}
                     when
                         OptionItems :: all | [OptionItem],
                         OptionItem ::
                             proxy | https_proxy | max_sessions | max_connections_open |
                             keep_alive_timeout | max_keep_alive_length | pipeline_timeout |
                             max_pipeline_length | cookies | ipfamily | ip | port | socket_opts |
                             verbose | unix_socket,
                         Values :: [{OptionItem, term()}],
                         Profile :: atom() | pid(),
                         Reason :: term().
```

Retrieves the options currently used by the client.

# `info`
*since OTP R15B02* 

```erlang
-spec info() -> list() | {error, Reason} when Reason :: term().
```

# `info`
*since OTP R15B02* 

```erlang
-spec info(Profile) -> list() | {error, Reason} when Reason :: term(), Profile :: atom() | pid().
```

Produces a list of miscellaneous information. Intended for debugging. If no
profile is specified, the default profile is used.

# `request`
*since OTP R13B04* 

```erlang
-spec request(uri_string:uri_string()) -> {ok, Result} | {error, term()}
                 when
                     Result ::
                         {StatusLine :: {HttpVersion, StatusCode, string()},
                          [HttpHeader],
                          HttpBodyResult} |
                         {StatusCode, HttpBodyResult} |
                         RequestId | saved_to_file,
                     HttpBodyResult :: string() | binary(),
                     HttpVersion :: string(),
                     StatusCode :: non_neg_integer(),
                     HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                     RequestId :: any().
```

# `request`
*since OTP R13B04* 

```erlang
-spec request(Url, Profile) -> {ok, Result} | {error, term()}
                 when
                     Url :: uri_string:uri_string(),
                     Profile :: atom() | pid(),
                     Result ::
                         {StatusLine, [HttpHeader], HttpBodyResult} |
                         {StatusCode, HttpBodyResult} |
                         RequestId | saved_to_file,
                     HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                     HttpBodyResult :: string() | binary(),
                     StatusLine :: {HttpVersion, StatusCode, string()},
                     HttpVersion :: string(),
                     StatusCode :: non_neg_integer(),
                     RequestId :: any().
```

Equivalent to [`httpc:request(get, {Url, []}, [], [])`](`request/4`).

# `request`
*since OTP R13B04* 

```erlang
-spec request(Method, Request, HttpOptions, Options) -> {ok, Result} | {error, term()}
                 when
                     Method :: head | get | put | patch | post | trace | options | delete,
                     Request ::
                         {uri_string:uri_string(), [HttpHeader]} |
                         {uri_string:uri_string(), [HttpHeader], ContentType :: string(), HttpBody},
                     HttpBody ::
                         iolist() |
                         binary() |
                         {fun((Accumulator :: term()) -> eof | {ok, iolist(), Accumulator :: term()}),
                          Accumulator :: term()} |
                         {chunkify,
                          fun((Accumulator :: term()) -> eof | {ok, iolist(), Accumulator :: term()}),
                          Accumulator :: term()},
                     HttpOptions :: [HttpOption],
                     HttpOption ::
                         {timeout, timeout()} |
                         {connect_timeout, timeout()} |
                         {ssl, [ssl:tls_option()]} |
                         {autoredirect, boolean()} |
                         {autoretry, timeout()} |
                         {proxy_auth, {string(), string()}} |
                         {version, HttpVersion} |
                         {relaxed, boolean()},
                     Options :: [OptionRequest],
                     OptionRequest ::
                         {sync, boolean()} |
                         {stream, StreamTo} |
                         {body_format, BodyFormat} |
                         {full_result, boolean()} |
                         {headers_as_is, boolean()} |
                         {socket_opts, [SocketOpt]} |
                         {receiver, Receiver} |
                         {ipv6_host_with_brackets, boolean()},
                     StreamTo :: none | self | {self, once} | file:name_all(),
                     SocketOpt :: term(),
                     BodyFormat :: string | binary,
                     Receiver ::
                         pid() |
                         fun((term()) -> term()) |
                         {ReceiverModule :: atom(), ReceiverFunction :: atom(), ReceiverArgs :: list()},
                     Result ::
                         {StatusLine, [HttpHeader], HttpBodyResult} |
                         {StatusCode, HttpBodyResult} |
                         RequestId | saved_to_file,
                     StatusCode :: non_neg_integer(),
                     StatusLine :: {HttpVersion, StatusCode, string()},
                     HttpVersion :: string(),
                     HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                     HttpBodyResult :: string() | binary(),
                     RequestId :: any().
```

# `request`
*since OTP R13B04* 

```erlang
-spec request(Method, Request, HttpOptions, Options, Profile) -> {ok, Result} | {error, term()}
                 when
                     Method :: head | get | put | patch | post | trace | options | delete,
                     Request ::
                         {uri_string:uri_string(), [HttpHeader]} |
                         {uri_string:uri_string(), [HttpHeader], ContentType :: string(), HttpBody},
                     HttpBody ::
                         iolist() |
                         binary() |
                         {fun((Accumulator :: term()) -> eof | {ok, iolist(), Accumulator :: term()}),
                          Accumulator :: term()} |
                         {chunkify,
                          fun((Accumulator :: term()) -> eof | {ok, iolist(), Accumulator :: term()}),
                          Accumulator :: term()},
                     HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                     HttpOptions :: [HttpOption],
                     HttpOption ::
                         {timeout, timeout()} |
                         {connect_timeout, timeout()} |
                         {ssl, [ssl:tls_option()]} |
                         {autoredirect, boolean()} |
                         {autoretry, timeout()} |
                         {proxy_auth, {string(), string()}} |
                         {version, HttpVersion} |
                         {relaxed, boolean()},
                     Options :: [OptionRequest],
                     OptionRequest ::
                         {sync, boolean()} |
                         {stream, StreamTo} |
                         {body_format, BodyFormat} |
                         {full_result, boolean()} |
                         {headers_as_is, boolean()} |
                         {socket_opts, [SocketOpt]} |
                         {receiver, Receiver} |
                         {ipv6_host_with_brackets, boolean()},
                     StreamTo :: none | self | {self, once} | file:name_all(),
                     BodyFormat :: string | binary,
                     SocketOpt :: term(),
                     Receiver ::
                         pid() |
                         fun((term()) -> term()) |
                         {ReceiverModule :: atom(), ReceiverFunction :: atom(), ReceiverArgs :: list()},
                     Profile :: atom() | pid(),
                     HttpVersion :: string(),
                     Result ::
                         {StatusLine, [HttpHeader], HttpBodyResult} |
                         {StatusCode, HttpBodyResult} |
                         RequestId | saved_to_file,
                     StatusLine :: {HttpVersion, StatusCode, string()},
                     StatusCode :: non_neg_integer(),
                     HttpBodyResult :: string() | binary(),
                     RequestId :: any().
```

Sends an HTTP request. The function can be both synchronous and asynchronous. In
the latter case, the function returns `{ok, RequestId}` and then the information
is delivered to the `receiver` depending on that value.

When `Profile` is `stand_alone` only the pid can be used.

HTTP options:

- [](){: #opt_timeout } **`timeout`** - Time-out time for the request.

  The clock starts ticking when the request is sent.

  Time is in milliseconds.

  Default is `infinity`.

- [](){: #opt_connect_timeout } **`connect_timeout`** - Connection time-out time, used during the initial
  request, when the client is _connecting_ to the server.

  Time is in milliseconds.

  Default is the value of option `timeout`.

- [](){: #opt_ssl } **`ssl`** - This is the `SSL/TLS` connecting configuration option.

  Default value is obtained by calling
  [`httpc:ssl_verify_host_options(true)`. ](`ssl_verify_host_options/1`). See
  [ssl:connect/2,3,4](`m:ssl`) for available options.

- [](){: #opt_autoredirect } **`autoredirect`** - The client automatically retrieves the information from
  the new URI and returns that as the result, instead of a 30X-result code.

  For some 30X-result codes, automatic redirect is not allowed. In these cases
  the 30X-result is always returned.

  Default is `true`.

- [](){: #opt_autoretry } **`autoretry`** - The client automatically retries the request **once** after receiving
  a Retry-After header from the server.

  Sometimes servers can suggest a value that is not suitable for application,
  so this option allows limiting the wait time **(in miliseconds)** inbetween requests, or disabling
  the retry with a value of `0`. If a value of Retry-After header exceeds the set
  value, no retry will be done.

  Default is atom `infinity`.

  Since OTP 28.4

- [](){: #opt_proxy_auth } **`proxy_auth`** - A proxy-authorization header using a tuple where the first
  element is the `username` and the second element of the tuple is the
  `password` added to the request.

- [](){: #opt_version } **`version`** - Can be used to make the client act as an `HTTP/1.0` client. By
  default this is an `HTTP/1.1` client. When using `HTTP/1.0` persistent
  connections are not used.

  Default is the string `"HTTP/1.1"`.

- [](){: #opt_relaxed } **`relaxed`** - If set to `true`, workarounds for known server deviations from
  the HTTP-standard are enabled.

  Default is `false`.

Options details:

- [](){: #opt_sync } **`sync`** - Option for the request to be synchronous or asynchronous.

  Default is `true`.

- [](){: #opt_stream } **`stream`** - Streams the body of a 200 or 206 response to the calling
  process or to a file. When streaming to the calling process using option
  `self`, the following stream messages are sent to that process:
  `{http, {RequestId, stream_start, Headers}}, {http, {RequestId, stream, BinBodyPart}}, and {http, {RequestId, stream_end, Headers}}`.

  When streaming to the calling processes using option `{self, once}`, the first
  message has an extra element, that is,
  `{http, {RequestId, stream_start, Headers, Pid}}`. This is the process id to
  be used as an argument to `httpc:stream_next/1` to trigger the next message to
  be sent to the calling process.

  Notice that chunked encoding can add headers so that there are more headers in
  the `stream_end` message than in `stream_start`. When streaming to a file and
  the request is asynchronous, the message `{http, {RequestId, saved_to_file}}`
  is sent.

  Default is `none`.

- [](){: #opt_body_format } **`body_format`** - Defines if the body is to be delivered as a string or
  binary. This option is only valid for the synchronous request.

  Default is `string`. Asynchronous requests always use `binary`.

- [](){: #opt_full_result } **`full_result`** - Defines if a "full result" is to be returned to the caller
  (that is, the body, the headers, and the entire status line) or not (the body
  and the status code).

  Default is `true`.

- [](){: #opt_headers_as_is } **`headers_as_is`** - Defines if the headers provided by the user are to be
  made lower case or to be regarded as case sensitive.

  The HTTP standard requires them to be case insensitive. Use this feature only
  if there is no other way to communicate with the server or for testing
  purpose. When this option is used, no headers are automatically added. All
  necessary headers must be provided by the user.

  Default is `false`.

- [](){: #opt_socket_opts } **`socket_opts`** - Socket options to be used for this request.

  See the options used by `m:gen_tcp` and `m:ssl`

  Overrides any value set by function [set_options](`set_options/1`).

  The validity of the options is _not_ checked by the HTTP client they are
  assumed to be correct and passed on to ssl application and inet driver, which
  may reject them if they are not correct.

  > #### Note {: .info }
  >
  > Persistent connections are not supported when setting the `socket_opts`
  > option. When `socket_opts` is not set the current implementation assumes the
  > requests to the same host, port combination will use the same socket
  > options.

  By default the socket options set by function
  [set_options/1,2](`set_options/1`) are used when establishing a connection.

- [](){: #opt_receiver } **`receiver`** - Defines how the client delivers the result of an asynchronous
  request (`sync` has the value `false`).

  - **`t:pid/0`** - Messages are sent to this process in the format
    `{http, ReplyInfo}`.

  - **`alias/0`** - Messages are sent to this special reference in the format
    `{http, ReplyInfo}`.

  - **`function/1`** - Information is delivered to the receiver through calls to
    the provided fun `Receiver(ReplyInfo)`.

  - **`{Module, Function, Args}`** - Information is delivered to the receiver
    through calls to the callback function
    [`apply(Module, Function, [ReplyInfo | Args])`](`apply/3`).

  In all of these cases, `ReplyInfo` has the following structure:

  ```erlang
   {RequestId, saved_to_file}
   {RequestId, {error, Reason}}
   {RequestId, Result}
   {RequestId, stream_start, Headers}
   {RequestId, stream_start, Headers, HandlerPid}
   {RequestId, stream, BinBodyPart}
   {RequestId, stream_end, Headers}
  ```

  Default is the `pid` of the process calling the request function (`self/0`).

  [](){: #ipv6_host_with_brackets }

- **`ipv6_host_with_brackets`** - Defines when parsing the Host-Port part of an
  URI with an IPv6 address with brackets, if those brackets are to be retained
  (`true`) or stripped (`false`).

  Default is `false`.

# `reset_cookies`
*since OTP R13B04* 

```erlang
-spec reset_cookies() -> Void when Void :: term().
```

# `reset_cookies`
*since OTP R13B04* 

```erlang
-spec reset_cookies(Profile) -> Void when Profile :: atom() | pid(), Void :: term().
```

Resets (clears) the cookie database for the specified `Profile`. If no profile
is specified the default profile is used.

# `set_options`
*since OTP R13B04* 

```erlang
-spec set_options(Options) -> ok | {error, Reason}
                     when
                         Options :: [Option],
                         Option ::
                             {proxy, {Proxy, NoProxy}} |
                             {https_proxy, {Proxy, NoProxy}} |
                             {max_connections_open, MaxConnectionsOpen} |
                             {max_sessions, MaxSessions} |
                             {max_keep_alive_length, MaxKeepAlive} |
                             {keep_alive_timeout, KeepAliveTimeout} |
                             {max_pipeline_length, MaxPipeline} |
                             {pipeline_timeout, PipelineTimeout} |
                             {cookies, CookieMode} |
                             {ipfamily, IpFamily} |
                             {ip, IpAddress} |
                             {port, Port} |
                             {socket_opts, SocketOpts} |
                             {verbose, VerboseMode} |
                             {unix_socket, UnixSocket},
                         Proxy :: {HostName, Port},
                         Port :: non_neg_integer(),
                         NoProxy :: [DomainDesc | HostName | IpAddressDesc],
                         MaxSessions :: integer(),
                         MaxConnectionsOpen :: integer(),
                         MaxKeepAlive :: integer(),
                         KeepAliveTimeout :: integer(),
                         MaxPipeline :: integer(),
                         PipelineTimeout :: integer(),
                         CookieMode :: enabled | disabled | verify,
                         IpFamily :: inet | inet6 | local | inet6fb4,
                         IpAddressDesc :: string(),
                         IpAddress :: inet:ip_address(),
                         VerboseMode :: false | verbose | debug | trace,
                         SocketOpts :: [SocketOpt],
                         SocketOpt :: term(),
                         UnixSocket :: file:name_all(),
                         Reason :: term(),
                         DomainDesc :: string(),
                         HostName :: string().
```

# `set_options`
*since OTP R13B04* 

```erlang
-spec set_options(Options, Profile) -> ok | {error, Reason}
                     when
                         Options :: [Option],
                         Option ::
                             {proxy, {Proxy, NoProxy}} |
                             {https_proxy, {Proxy, NoProxy}} |
                             {max_connections_open, MaxConnectionsOpen} |
                             {max_sessions, MaxSessions} |
                             {max_keep_alive_length, MaxKeepAlive} |
                             {keep_alive_timeout, KeepAliveTimeout} |
                             {max_pipeline_length, MaxPipeline} |
                             {pipeline_timeout, PipelineTimeout} |
                             {cookies, CookieMode} |
                             {ipfamily, IpFamily} |
                             {ip, IpAddress} |
                             {port, Port} |
                             {socket_opts, [SocketOpt]} |
                             {verbose, VerboseMode} |
                             {unix_socket, UnixSocket},
                         Profile :: atom() | pid(),
                         SocketOpt :: term(),
                         Proxy :: {HostName, Port},
                         Port :: non_neg_integer(),
                         NoProxy :: [DomainDesc | HostName | IpAddressDesc],
                         MaxConnectionsOpen :: integer() | infinity,
                         MaxSessions :: integer(),
                         MaxKeepAlive :: integer(),
                         KeepAliveTimeout :: integer(),
                         MaxPipeline :: integer(),
                         PipelineTimeout :: integer(),
                         CookieMode :: enabled | disabled | verify,
                         IpFamily :: inet | inet6 | local | inet6fb4,
                         IpAddressDesc :: string(),
                         IpAddress :: inet:ip_address(),
                         VerboseMode :: false | verbose | debug | trace,
                         UnixSocket :: string(),
                         Reason :: term(),
                         DomainDesc :: string(),
                         HostName :: string().
```

Sets options to be used for subsequent requests.

- **`HostName`** - Example: "localhost" or "foo.bar.se"

- **`DomainDesc`** - Example `"*.Domain"` or `"*.ericsson.se"`

- **`IpAddressDesc`** - Example: "134.138" or "\[FEDC:BA98" (all IP addresses
  starting with 134.138 or FEDC:BA98), "66.35.250.150" or
  "[2010:836B:4179::836B:4179]" (a complete IP address). `proxy` defaults to
  `{undefined, []}`, that is, no proxy is configured and `https_proxy` defaults
  to the value of `proxy`.

- [](){: #opt_max_connections_open } **`MaxConnectionsOpen`** - `MaxConnectionsOpen` Maximum number of handlers that can be
  opened at the same time. Default is `infinity` which means that it's not limited.

- [](){: #opt_max_sessions } **`MaxSessions`** - `MaxSessions` Maximum number of persistent connections to
  a host. Default is `2`.

- [](){: #opt_max_keep_alive_length } **`MaxKeepAlive`** - `MaxKeepAlive` Maximum number of outstanding requests on
  the same connection to a host. Default is `5`.

- [](){: #opt_keep_alive_timeout } **`KeepAliveTimeout`** - `KeepAliveTimeout` If a persistent connection is idle
  longer than the `keep_alive_timeout` in milliseconds, the client closes the
  connection. The server can also have such a time-out but do not take that for
  granted. Default is `120000` (= 2 min).

- [](){: #opt_max_pipeline_length } **`MaxPipeline`** - `MaxPipeline` Maximum number of outstanding requests on a
  pipelined connection to a host. Default is `2`.

- [](){: #opt_pipeline_timeout } **`PipelineTimeout`** - `PipelineTimeout` If a persistent connection is idle
  longer than the `pipeline_timeout` in milliseconds, the client closes the
  connection. Default is `0`, which results in pipelining not being used.

- [](){: #opt_cookies } **`CookieMode`** - If cookies are enabled, all valid cookies are automatically
  saved in the cookie database of the client manager. If option `verify` is
  used, function [`store_cookies/2`](`store_cookies/2`) has to be called for the
  cookies to be saved. Default is `disabled`.

- **`IpFamily`** - Default is `inet`. With `inet6fb4` option, IPv6 will be
  preferred but if connection fails, an IPv4 fallback connection attempt will be
  made.

- **`IpAddress`** - If the host has several network interfaces, this option
  specifies which one to use. See [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`) for
  details.

- **`Port`** - Example: `8080`. Local port number to use. See
  [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`) for details.

- **`SocketOpts`** - The options are appended to the socket options used by the
  client. These are the default values when a new request handler is started
  (for the initial connect). They are passed directly to the underlying
  transport (`gen_tcp` or `SSL`) without verification.

  See the options used by `m:gen_tcp` and `m:ssl`

- [](){: #opt_verbose } **`VerboseMode`** - Default is `false`. This option is used to switch on (or
  off) different levels of Erlang trace on the client. It is a debug feature.

- **`Profile`** - When started `stand_alone` only the pid can be used.

- **`UnixSocket`** - Experimental option for sending HTTP requests over a unix
  domain socket. The value of `unix_socket` shall be the full path to a unix
  domain socket file with read/write permissions for the erlang process. Default
  is `undefined`.

> #### Note {: .info }
>
> If possible, the client keeps its connections alive and uses persistent
> connections with or without pipeline depending on configuration and current
> circumstances. The HTTP/1.1 specification does not provide a guideline for how
> many requests that are ideal to be sent on a persistent connection. This
> depends much on the application.
>
> A long queue of requests can cause a user-perceived delay, as earlier requests
> can take a long time to complete. The HTTP/1.1 specification suggests a limit
> of two persistent connections per server, which is the default value of option
> `max_sessions`.
>
> The current implementation assumes the requests to the same host, port
> combination will use the same socket options.

# `ssl_verify_host_options`
*since OTP 25.1* 

```erlang
-spec ssl_verify_host_options(WildcardHostName) -> list() when WildcardHostName :: boolean().
```

Returns ssl options which can be used to verify the host, uses
[`public_key:cacerts_get()`](`public_key:cacerts_get/0`) to read CA certicates
and if `WildcardHostName` is true adds the hostname check from
[`public_key:public_key:pkix_verify_hostname_match_fun(https)`](`public_key:pkix_verify_hostname_match_fun/1`)
to the options.

# `store_cookies`
*since OTP R14B02* 

```erlang
-spec store_cookies(SetCookieHeaders, Url) -> ok | {error, Reason}
                       when
                           SetCookieHeaders :: [HttpHeader],
                           HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                           Url :: term(),
                           Reason :: term().
```

# `store_cookies`
*since OTP R14B02* 

```erlang
-spec store_cookies(SetCookieHeaders, Url, Profile) -> ok | {error, Reason}
                       when
                           SetCookieHeaders :: [HttpHeader],
                           HttpHeader :: {Field :: [byte()], Value :: binary() | iolist()},
                           Url :: term(),
                           Profile :: atom() | pid(),
                           Reason :: term().
```

Saves the cookies defined in `SetCookieHeaders` in the client profile cookie
database. Call this function if option `cookies` is set to `verify`. If no
profile is specified, the default profile is used.

# `stream_next`
*since OTP R13B04* 

```erlang
-spec stream_next(Pid) -> ok when Pid :: pid().
```

Triggers the next message to be streamed, that is, the same behavior as active
ones for sockets.

# `which_cookies`
*since OTP R13B04* 

```erlang
-spec which_cookies() -> [CookieStores]
                       when
                           CookieStores :: {cookies, Cookies} | {session_cookies, Cookies},
                           Cookies :: [term()].
```

# `which_cookies`
*since OTP R13B04* 

```erlang
-spec which_cookies(Profile) -> [CookieStores]
                       when
                           Profile :: atom() | pid(),
                           CookieStores :: {cookies, Cookies} | {session_cookies, Cookies},
                           Cookies :: [term()].
```

Produces a list of the entire cookie database. Intended for debugging/testing
purposes. If no profile is specified, the default profile is used.

# `which_sessions`
*since OTP R15B02* 

```erlang
-spec which_sessions() -> SessionInfo
                        when
                            SessionInfo :: {GoodSession, BadSessions, NonSessions},
                            GoodSession :: [Session],
                            BadSessions :: [term()],
                            NonSessions :: [term()],
                            Session :: term().
```

# `which_sessions`
*since OTP R15B02* 

```erlang
-spec which_sessions(Profile) -> SessionInfo
                        when
                            Profile :: atom() | pid(),
                            SessionInfo :: {GoodSession, BadSessions, NonSessions},
                            GoodSession :: [Session],
                            BadSessions :: [term()],
                            NonSessions :: [term()],
                            Session :: term().
```

This function is intended for debugging only. It produces a slightly processed
dump of the session database. The first list of the session information tuple
will contain session information on an internal format. The last two lists of
the session information tuple should always be empty if the code is working as
intended. If no profile is specified, the default profile is used.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
