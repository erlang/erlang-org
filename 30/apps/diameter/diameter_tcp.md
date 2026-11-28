# `diameter_tcp`
[🔗](https://github.com/erlang/otp/blob/master/lib/diameter/src/transport/diameter_tcp.erl#L23)

Diameter transport over TCP.

This module implements diameter transport over TCP using `m:gen_tcp`. It can be
specified as the value of a `transport_module` option to
`diameter:add_transport/2` and implements the behaviour documented in
`m:diameter_transport`. TLS security is supported, either as an upgrade
following capabilities exchange or at connection establishment.

Note that the ssl application is required for TLS and must be started before
configuring TLS capability on diameter transports.

[](){: #start }

## SEE ALSO

`m:diameter`, `m:diameter_transport`, `m:gen_tcp`, `m:inet`, `m:ssl`

# `connect_option`
*since OTP R14B03* 

```erlang
-type connect_option() ::
          {raddr, inet:ip_address()} |
          {rport, pos_integer()} |
          {ssl_options, true | [ssl:tls_client_option()]} |
          option() |
          ssl:tls_client_option() |
          gen_tcp:connect_option().
```

# `listen_option`
*since OTP R14B03* 

```erlang
-type listen_option() ::
          {accept, match()} |
          {ssl_options, true | [ssl:tls_server_option()]} |
          option() |
          ssl:tls_server_option() |
          gen_tcp:listen_option().
```

# `match`
*not exported* *since OTP R14B03* 

```erlang
-type match() :: inet:ip_address() | string() | [match()].
```

# `option`
*not exported* *since OTP R14B03* 

```erlang
-type option() ::
          {port, non_neg_integer()} |
          {sender, boolean()} |
          sender |
          {message_cb, false | diameter:eval()} |
          {fragment_timer, 0..4294967295}.
```

# `start`
*since OTP R14B03* 

```erlang
-spec start({accept, Ref},
            #diameter_service{pid :: term(), capabilities :: term(), applications :: term()},
            [listen_option()]) ->
               {ok, pid(), [inet:ip_address()]}
               when Ref :: diameter:transport_ref();
           ({connect, Ref},
            #diameter_service{pid :: term(), capabilities :: term(), applications :: term()},
            [connect_option()]) ->
               {ok, pid()}
               when Ref :: diameter:transport_ref().
```

start({Type, Ref}, Svc, [Opt]) -> {ok, Pid} | {ok, Pid, [LAddr]} | {error,
Reason}

The start function required by `m:diameter_transport`.

Options `raddr` and `rport` specify the remote address and port for a connecting
transport and are not valid for a listening transport.

Option `accept` specifies remote addresses for a listening transport and is not
valid for a connecting transport. If specified, a remote address that does not
match one of the specified addresses causes the connection to be aborted.
Multiple `accept` options can be specified. A string-valued `Match` that does
not parse as an address is interpreted as a regular expression.

Option `ssl_options` must be specified for a transport that should support TLS:
a value of `true` results in a TLS handshake immediately upon connection
establishment while `t:list/0` specifies options to be passed to `ssl:connect/2`
or `ssl:handshake/2` after capabilities exchange if TLS is negotiated.

Option `fragment_timer` specifies the timeout, in milliseconds, of a timer used
to flush messages from the incoming byte stream even if the number of bytes
indicated in the Message Length field of its Diameter Header have not yet been
accumulated: such a message is received over the transport interface after two
successive timeouts without the reception of additional bytes. Defaults to 1000.

[](){: #sender }

Option `sender` specifies whether or not to use a dedicated process for sending
outgoing messages, which avoids the possibility of send blocking reception.
Defaults to `false`. If set to `true` then a `message_cb` that avoids the
possibility of messages being queued in the sender process without bound should
be configured.

Option `message_cb` specifies a callback that is invoked on incoming and
outgoing messages, that can be used to implement flow control. It is applied to
two arguments: an atom indicating the reason for the callback (`send`, `recv`,
or `ack` after a completed send), and the message in question (binary() on
`recv`, binary() or diameter*packet record on `send` or `ack`, or `false` on
`ack` when an incoming request has been discarded). It should return a list of
actions and a new callback as tail; eg. `[fun cb/3, State]`. Valid actions are
the atoms `send` or `recv`, to cause a following message-valued action to be
sent/received, a message to send/receive (binary() or diameter_packet record),
or a boolean() to enable/disable reading on the socket. More than one
`send`/`recv`/message sequence can be returned from the same callback, and an
initial `send`/`recv` can be omitted if the same as the value passed as the
callback's first argument. Reading is initially enabled, and returning `false`
does not imply there cannot be subsequent `recv` callbacks since messages may
already have been read. An empty tail is equivalent to the prevailing callback.
Defaults to a callback equivalent to `fun(ack, *) -> []; (\_, Msg) -> [Msg]
end`.

Remaining options are any accepted by `ssl:connect/3` or `gen_tcp:connect/3` for
a connecting transport, or `ssl:listen/2` or `gen_tcp:listen/2` for a listening
transport, depending on whether or not `{ssl_options, true}` has been specified.
Options `binary`, `packet` and `active` cannot be specified. Also, option `port`
can be specified for a listening transport to specify the local listening port,
the default being the standardized 3868. Note that the option `ip` specifies the
local address.

An `ssl_options` list must be specified if and only if the transport in question
has set `Inband-Security-Id` to 1 (`TLS`), as specified to either
`diameter:start_service/2` or `diameter:add_transport/2`, so that the transport
process will receive notification of whether or not to commence with a TLS
handshake following capabilities exchange. Failing to specify an options list on
a TLS-capable transport for which TLS is negotiated will cause TLS handshake to
fail. Failing to specify TLS capability when `ssl_options` has been specified
will cause the transport process to wait for a notification that will not be
forthcoming, which will eventually cause the RFC 3539 watchdog to take down the
connection.

The first element of a non-empty `Host-IP-Address` list in `Svc` provides the
local IP address if an `ip` option is not specified. The local address is either
returned from`start/3` or passed in a `connected` message over the transport
interface.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
