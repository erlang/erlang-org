# `diameter_sctp`
[🔗](https://github.com/erlang/otp/blob/master/lib/diameter/src/transport/diameter_sctp.erl#L23)

Diameter transport over SCTP.

This module implements diameter transport over SCTP using `m:gen_sctp`. It can
be specified as the value of a transport_module option to
`diameter:add_transport/2` and implements the behaviour documented in
`m:diameter_transport`.

[](){: #start }

## SEE ALSO

`m:diameter`, `m:diameter_transport`, `m:gen_sctp`, `m:inet`

# `connect_option`
*since OTP R14B03* 

```erlang
-type connect_option() :: {raddr, inet:ip_address()} | {rport, inet:port_number()} | option() | term().
```

# `listen_option`
*since OTP R14B03* 

```erlang
-type listen_option() :: {accept, match()} | option() | term().
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
          {sender, boolean()} |
          sender |
          {packet, boolean() | raw} |
          {message_cb, false | diameter:eval()}.
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
               {ok, pid(), [inet:ip_address()]}
               when Ref :: diameter:transport_ref().
```

The start function required by `m:diameter_transport`.

Options `raddr` and `rport` specify the remote address and port for a connecting
transport and not valid for a listening transport: the former is required while
latter defaults to 3868 if unspecified. Multiple `raddr` options can be
specified, in which case the connecting transport in question attempts each in
sequence until an association is established.

Option `accept` specifies remote addresses for a listening transport and is not
valid for a connecting transport. If specified, a remote address that does not
match one of the specified addresses causes the association to be aborted.
Multiple `accept` options can be specified. A string-valued `Match` that does
not parse as an address is interpreted as a regular expression.

Option `unordered` specifies whether or not to use unordered delivery, integer
`N` being equivalent to `N =< OS`, where `OS` is the number of outbound streams
negotiated on the association in question. Regardless of configuration, sending
is ordered on stream 0 until reception of a second incoming message, to ensure
that a peer receives capabilities exchange messages before any other. Defaults
to `false`.

Option `packet` determines how/if an incoming message is packaged into a
diameter_packet record. If `false` then messages are received as binary(). If
`true` then as a record with the binary() message in the `bin` field and a
`{stream, Id}` tuple in the `transport_data` field, where `Id` is the identifier
of the inbound stream the message was received on. If `raw` then as a record
with the received ancillary sctp_sndrcvinfo record in the `transport_data`
field. Defaults to `true`.

Options `message_cb` and `sender` have semantics identical to those documented
in [diameter_tcp(3)](`m:diameter_tcp#sender`), but with the message argument to
a `recv` callback being as directed by the `packet` option.

An `{outstream, Id}` tuple in the `transport_data` field of a outgoing
diameter_packet record sets the outbound stream on which the message is sent,
modulo the negotiated number of outbound streams. Any other value causes
successive such sends to cycle though all outbound streams.

Remaining options are any accepted by `gen_sctp:open/1`, with the exception of
options `mode`, `binary`, `list`, `active` and `sctp_events`. Note that options
`ip` and `port` specify the local address and port respectively.

Multiple `ip` options can be specified for a multihomed peer. If none are
specified then the values of `Host-IP-Address` in the `diameter_service` record
are used. Option `port` defaults to 3868 for a listening transport and 0 for a
connecting transport.

> #### Warning {: .warning }
>
> An small receive buffer may result in a peer having to resend incoming
> messages: set the `m:inet` option `recbuf` to increase the buffer size.
>
> An small send buffer may result in outgoing messages being discarded: set the
> `m:inet` option `sndbuf` to increase the buffer size.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
