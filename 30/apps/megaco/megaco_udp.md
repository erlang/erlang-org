# `megaco_udp`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/udp/megaco_udp.erl#L27)

Interface module to UDP transport protocol for Megaco/H.248.

This module contains the public interface to the UDP/IP version
transport protocol for Megaco/H.248.

# `counter`

```erlang
-type counter() ::
          medGwyGatewayNumInMessages | medGwyGatewayNumInOctets | medGwyGatewayNumOutMessages |
          medGwyGatewayNumOutOctets | medGwyGatewayNumErrors.
```

Defines the different counters handled by this transport.

# `handle`

```erlang
-opaque handle()
```

An opaque data type representing an UDP socket.

# `send_handle`

```erlang
-opaque send_handle()
```

An opaque data type representing an UDP socket, used when sending.

# `block`

```erlang
-spec block(Handle) -> ok when Handle :: handle() | send_handle().
```

Stop receiving incoming messages on the socket.

# `close`

```erlang
-spec close(Handle) -> ok when Handle :: handle() | send_handle().
```

This function is used for closing an active UDP socket.

# `create_send_handle`

```erlang
-spec create_send_handle(Handle, Host, Port) -> send_handle()
                            when
                                Handle :: handle(),
                                Host :: inet:ip4_address() | inet:hostname(),
                                Port :: inet:port_number().
```

Creates a send handle from a transport handle. The send handle is intended to be
used by megaco_udp:send_message/2.

# `get_stats`

```erlang
-spec get_stats() -> {ok, TotalStats} | {error, Reason}
                   when
                       TotalStats :: [{SH, [{Counter, integer()}]}],
                       SH :: send_handle(),
                       Counter :: counter(),
                       Reason :: term().
```

Get all counter values for all known connections.

# `get_stats`

```erlang
-spec get_stats(SH) -> {ok, Stats} | {error, Reason}
                   when
                       SH :: send_handle(),
                       Stats :: [{Counter, integer()}],
                       Counter :: counter(),
                       Reason :: term().
```

Get all counter values for a given handle.

# `get_stats`

```erlang
-spec get_stats(SH, Counter) -> {ok, integer()} | {error, Reason}
                   when SH :: send_handle(), Counter :: counter(), Reason :: term().
```

Get the value of a specific counter.

# `open`

```erlang
-spec open(TransportRef, Opts) -> {ok, Handle, ControlPid} | {error, Reason}
              when
                  TransportRef :: pid(),
                  Opts ::
                      {inet_backend, default | inet | socket} |
                      {port, PortNum} |
                      {options, list()} |
                      {receive_handle, term()} |
                      {module, atom()},
                  PortNum :: inet:port_number(),
                  Handle :: handle(),
                  ControlPid :: pid(),
                  Reason :: term().
```

This function is used to open an UDP/IP socket.

- **`module`** - The option makes it possible for the user to provide their own
  callback module. The functions `receive_message/4` or
  `process_received_message/4` of this module is called when a new message is
  received. Which one depends on the size of the message:

  - **`small`** - receive_message

  - **`large`** - process_received_message

  Default value is _megaco_.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).

# `reset_stats`

```erlang
-spec reset_stats() -> megaco:void().
```

Reset all counters for all UDP handles.

# `reset_stats`

```erlang
-spec reset_stats(SH) -> megaco:void() when SH :: send_handle().
```

Reset all counters for the given UDP handle.

# `send_message`

```erlang
-spec send_message(SH, Msg) -> ok when SH :: send_handle(), Msg :: binary() | iolist().
```

Sends a message on a socket. The send handle is obtained by
megaco_udp:create_send_handle/3. Increments the NumOutMessages and NumOutOctets
counters if message successfully sent. In case of a failure to send, the
NumErrors counter is _not_ incremented. This is done elsewhere in the megaco
app.

# `socket`

```erlang
-spec socket(Handle) -> Socket when Handle :: handle() | send_handle(), Socket :: inet:socket().
```

This function is used to convert a socket `handle()` to a inet `socket()`.

# `start_transport`

```erlang
-spec start_transport() -> {ok, TransportRef} when TransportRef :: pid().
```

This function is used for starting the UDP/IP transport service. Use
exit(TransportRef, Reason) to stop the transport service.

# `unblock`

```erlang
-spec unblock(Handle) -> ok when Handle :: handle() | send_handle().
```

Starting to receive incoming messages from the socket again.

# `upgrade_receive_handle`

```erlang
-spec upgrade_receive_handle(ControlPid, NewRecvHandle) -> ok
                                when ControlPid :: pid(), NewRecvHandle :: term().
```

Update the receive handle of the control process (e.g. after having changed
protocol version).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
