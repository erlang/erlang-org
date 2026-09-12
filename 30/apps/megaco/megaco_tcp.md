# `megaco_tcp`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/tcp/megaco_tcp.erl#L30)

Interface module to TPKT transport protocol for Megaco/H.248.

This module contains the public interface to the TPKT (TCP/IP) version transport
protocol for Megaco/H.248.

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

An opaque data type representing a TPKT connection.

# `block`

```erlang
-spec block(Handle) -> ok when Handle :: handle().
```

Stop receiving incoming messages on the socket.

# `close`

```erlang
-spec close(Handle) -> ok when Handle :: handle().
```

This function is used for closing an active TPKT connection.

# `connect`

```erlang
-spec connect(TransportRef, Opts) -> {ok, Handle, ControlPid} | {error, Reason}
                 when
                     TransportRef :: pid() | RegName,
                     RegName :: atom(),
                     Opts :: [Option],
                     Option ::
                         {inet_backend, default | inet | socket} |
                         {host, Host} |
                         {port, PortNum} |
                         {options, list()} |
                         {receive_handle, term()} |
                         {module, atom()},
                     Host :: inet:socket_address() | inet:hostname(),
                     PortNum :: inet:port_number(),
                     Handle :: handle(),
                     ControlPid :: pid(),
                     Reason :: term().
```

This function is used to open a TPKT connection.

- **`module`** - This option makes it possible for the user to provide their own
  callback module. The `receive_message/4` or `process_received_message/4`
  functions of this module is called when a new message is received. Which one
  is called depends on the size of the message;

  - **`small`** - receive_message

  - **`large`** - process_received_message

  Default value is _megaco_.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).

# `get_stats`

```erlang
-spec get_stats() -> {ok, TotalStats} | {error, Reason}
                   when
                       TotalStats :: [{Handle, [{Counter, integer()}]}],
                       Handle :: handle(),
                       Counter :: counter(),
                       Reason :: term().
```

Get all counter values for all known connections.

# `get_stats`

```erlang
-spec get_stats(Handle) -> {ok, Stats} | {error, Reason}
                   when
                       Handle :: handle(),
                       Stats :: [{Counter, integer()}],
                       Counter :: counter(),
                       Reason :: term().
```

Get all counter values for a given (connection) handle.

# `get_stats`

```erlang
-spec get_stats(Handle, Counter) -> {ok, integer()} | {error, Reason}
                   when Handle :: handle(), Counter :: counter(), Reason :: term().
```

Get the value of a specific counter.

# `listen`

```erlang
-spec listen(TransportRef, Options) -> ok
                when
                    TransportRef :: pid() | RegName,
                    RegName :: atom(),
                    Options :: [Option],
                    Option ::
                        {inet_backend, default | inet | socket} |
                        {port, inet:port_number()} |
                        {options, list()} |
                        {receive_handle, term()}.
```

This function is used for starting new TPKT listening socket for TCP/IP. The
option list contains the socket definitions.

- **`inet_backend`** - Choose the inet-backend.

  This option make it possible to use a different inet-backend ('default',
  'inet' or 'socket').

  Default is `default` (system default).

# `reset_stats`

```erlang
-spec reset_stats() -> megaco:void().
```

Reset all counters for all connections.

# `reset_stats`

```erlang
-spec reset_stats(Handle) -> megaco:void() when Handle :: handle().
```

Reset all counters for the given connection.

# `send_message`

```erlang
-spec send_message(Handle, Msg) -> ok when Handle :: handle(), Msg :: binary() | iolist().
```

Sends a message on a TPKT connection.

# `socket`

```erlang
-spec socket(Handle) -> Socket when Handle :: handle(), Socket :: inet:socket().
```

This function is used to convert a socket `handle()` to a inet `socket()`.

# `start_transport`

```erlang
-spec start_transport() -> {ok, TransportRef} when TransportRef :: pid().
```

This function is used for starting the TCP/IP transport service. Use
exit(TransportRef, Reason) to stop the transport service.

# `unblock`

```erlang
-spec unblock(Handle) -> ok when Handle :: handle().
```

Starting to receive incoming messages from the socket again.

# `upgrade_receive_handle`

```erlang
-spec upgrade_receive_handle(ControlPid, NewRecvHandle) -> ok
                                when ControlPid :: pid(), NewRecvHandle :: term().
```

Upgrade the receive handle of the control process (e.g. after having changed
protocol version).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
