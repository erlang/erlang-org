# `mod_esi`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_server/mod_esi.erl#L23)

Erlang Server Interface

This module defines the Erlang Server Interface (ESI) API. It is a more
efficient way of writing Erlang scripts for your `Inets` web server than writing
them as common CGI scripts.

# `env`
*not exported* 

```erlang
-type env() ::
          {server_software, string()} |
          {server_name, string()} |
          {gateway_interface, string()} |
          {server_protocol, string()} |
          {server_port, integer()} |
          {request_method, string()} |
          {remote_adress, inet:ip_address()} |
          {peer_cert, undefined | no_peercert | public_key:der_encoded()} |
          {script_name, string()} |
          {http_LowerCaseHTTPHeaderName, string()}.
```

Environment data associated with a request.

## Possible values

- **`{server_software, string()}`** - Indicates the inets version.

- **`{server_name, string()}`** - The local hostname.

- **`{gateway_interface, string()}`** - Legacy string used in CGI, just
ignore.

- **`{server_protocol, string()}`** - HTTP version, currently "HTTP/1.1"

- **`{server_port, integer()}`** - Servers port number.

- **`{request_method, "GET" | "PUT" | "DELETE" | "POST" | "PATCH"}`** - HTTP
request method.

- **`{remote_adress, inet:ip_address()}`** - The clients ip address.

- **`{peer_cert, undefined | no_peercert | DER:binary()}`** - For TLS
connections where client certificates are used this will be an ASN.1
DER-encoded X509-certificate as an Erlang binary. If client certificates are
not used the value will be `no_peercert`, and if TLS is not used (HTTP or
connection is lost due to network failure) the value will be `undefined`.

- **`{script_name, string()}`** - Request URI

- **`{http_LowerCaseHTTPHeaderName, string()}`** - example:
`{http_content_type, "text/html"}`

# `session_id`
*since OTP 28.0* 

```erlang
-opaque session_id()
```

Identifies the requesting client.

# `Function`
*optional* 

```erlang
-callback 'Function'(SessionID, Env, Input) -> {continue, State} | _
                        when
                            SessionID :: session_id(),
                            Env :: [env()],
                            Input :: string() | ChunkedData,
                            ChunkedData ::
                                {first, Data :: binary()} |
                                {continue, Data :: binary(), State :: term()} |
                                {last, Data :: binary(), State :: term()},
                            State :: term().
```

Called by `mod_esi` in response to requests.

`Module` must be found in the code path and export `Function` with an arity of
three. An `erl_script_alias` must also be set up in the configuration file for
the web server, see [the ESI properties documentation](`m:httpd#prop_esi_alias`).

The `Module` and `Function` that are called depend on the URL. See [the ESI
introductory documentation](http_server.md#esi) for more details.

`mod_esi:deliver/2` shall be used to generate the response to the client, and
`SessionID` shall be passed as the first argument.

## Chunking

This function may be called several times to chunk the response data. Notice
that the first chunk of data sent to the client must at least contain all HTTP
header fields that the response will generate. If the first chunk does not
contain the _end of HTTP header_, that is, `"\r\n\r\n"`, the server assumes
that no HTTP header fields will be generated. This behaviour depends on the
`httpd` configuration, see below.

## Parameters

- `SessionID`: request identifier.

  Pass this to `mod_esi:deliver/2` when generating a response.

- `Env`: environment data of the request, see `t:env/0`.

- `Input`: query data of a GET request or the body of a PUT or POST request.

  The default behavior (legacy reasons) for delivering the body, is that the
  whole body is gathered and converted to a string. But if the httpd config
  parameter [`max_client_body_chunk`](`m:httpd#max_client_body_chunk`) is set,
  the body will be delivered as binary chunks instead. The maximum size of the
  chunks is either [`max_client_body_chunk`](`m:httpd#max_client_body_chunk`) or
  decided by the client if it uses HTTP chunked encoding to send the body.

  When using the chunking mechanism, this callback must return `{continue,
  State::term()}` for all calls where `Input` is `{first, Data::binary()}` or
  `{continue, Data::binary(), State::term()}`. When `Input` is `{last,
  Data::binary(), State::term()}` the return value will be ignored.

  The input `State` is the last returned `State`, in it the callback can include
  any data that it needs to keep track of when handling the chunks.

> #### Note {: .info }
>
> Note that if the body is small all data may be delivered in only one chunk and
> then the callback will be called with `{last, Data::binary(), undefined}`
> without getting called with `{first, Data::binary()}`.

## Setting a response status

To set the response status code, the special `status` response header can be
sent. For instance, to acknowledge creation of a resource and send an empty
JSON response body, one could pass the following:

```erlang
"status: 201 Created\r\ncontent-type: application/json\r\n\r\n{}"
```

# `deliver`

```erlang
-spec deliver(SessionID, Data) -> ok | {error, Reason}
                 when SessionID :: session_id(), Data :: iolist(), Reason :: bad_sessionID.
```

Sends data from an ESI script back to the client.

This function is _only_ intended to be used from functions called by the ESI
interface to deliver parts of the content to the user.

> #### Note {: .info }
>
> If any HTTP header fields are added by the script, they must be in the first
> call to [`deliver/2`](`deliver/2`), and the data in the call must be a string.
> Calls after the headers are complete can contain binary data to reduce copying
> overhead. Do not assume anything about the data type of `SessionID`.
> `SessionID` must be the value given as input to the ESI callback function that
> you implemented.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
