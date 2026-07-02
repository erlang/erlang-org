# `gen_udp`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/gen_udp.erl#L22)

Interface to UDP sockets.

This module provides functions for communicating over UDP
protocol sockets.

> #### Note {: .info }
>
> Functions that create sockets can take an optional option;
> `{inet_backend, Backend}` that, if specified, has to be the first option. This
> selects the implementation backend towards the platform's socket API.
>
> This is a _temporary_ option that will be ignored in a future release.
>
> The default is `Backend = inet` that selects the traditional `inet_drv.c`
> driver. The other choice is `Backend = socket` that selects the new `m:socket`
> module and its NIF implementation.
>
> The system default can be changed when the node is started with the
> application `kernel`'s configuration variable `inet_backend`.
>
> For `gen_udp` with `inet_backend = socket` we have tried to be as "compatible"
> as possible which has sometimes been impossible. Here is a list of cases when
> the behaviour of inet-backend `inet` (default) and `socket` are different:
>
> - The option [read_packets](`m:inet#option-read_packets`) is currently
>   _ignored_.
> - Windows require sockets (domain = `inet | inet6`) to be bound.
>
>   _Currently_ all sockets created on Windows with `inet_backend = socket` will
>   be bound. If the user does not provide an address, gen_udp will try to
>   'figure out' an address itself.

# `ip6_membership`

```erlang
-type ip6_membership() :: {MultiAddress :: inet:ip6_address(), IfIndex :: integer()}.
```

# `ip6_multicast_if`

```erlang
-type ip6_multicast_if() :: integer().
```

IPv6 this multicast interface index (an integer).

# `ip_membership`

```erlang
-type ip_membership() ::
          {MultiAddress :: inet:ip4_address(), Interface :: inet:ip4_address()} |
          {MultiAddress :: inet:ip4_address(), Address :: inet:ip4_address(), IfIndex :: integer()}.
```

IP multicast membership.

The 3-tuple form _isn't_ supported on all platforms.
'ifindex' defaults to zero (0) on platforms that supports the 3-tuple variant.

# `ip_multicast_if`

```erlang
-type ip_multicast_if() :: inet:ip4_address().
```

# `membership`

```erlang
-type membership() :: ip_membership() | ip6_membership().
```

# `multicast_if`

```erlang
-type multicast_if() :: ip_multicast_if() | ip6_multicast_if().
```

# `open_option`

```erlang
-type open_option() ::
          {ip, inet:socket_address()} |
          {fd, non_neg_integer()} |
          {ifaddr, socket:sockaddr_in() | socket:sockaddr_in6() | inet:socket_address()} |
          inet:address_family() |
          {port, inet:port_number()} |
          {netns, file:filename_all()} |
          {bind_to_device, binary()} |
          option().
```

# `option`

```erlang
-type option() ::
          {active, true | false | once | -32768..32767} |
          {add_membership, membership()} |
          {broadcast, boolean()} |
          {buffer, non_neg_integer()} |
          {debug, boolean()} |
          {deliver, port | term} |
          {dontroute, boolean()} |
          {drop_membership, membership()} |
          {exclusiveaddruse, boolean()} |
          {header, non_neg_integer()} |
          {high_msgq_watermark, pos_integer()} |
          {low_msgq_watermark, pos_integer()} |
          {mode, list | binary} |
          list | binary |
          {multicast_if, multicast_if()} |
          {multicast_loop, boolean()} |
          {multicast_ttl, non_neg_integer()} |
          {priority, non_neg_integer()} |
          {raw, Protocol :: non_neg_integer(), OptionNum :: non_neg_integer(), ValueBin :: binary()} |
          {read_packets, non_neg_integer()} |
          {recbuf, non_neg_integer()} |
          {reuseaddr, boolean()} |
          {reuseport, boolean()} |
          {reuseport_lb, boolean()} |
          {sndbuf, non_neg_integer()} |
          {tos, non_neg_integer()} |
          {tclass, non_neg_integer()} |
          {ttl, non_neg_integer()} |
          {recvtos, boolean()} |
          {recvtclass, boolean()} |
          {recvttl, boolean()} |
          {ipv6_v6only, boolean()}.
```

# `option_name`

```erlang
-type option_name() ::
          active | broadcast | buffer | debug | deliver | dontroute | exclusiveaddruse | header |
          high_msgq_watermark | low_msgq_watermark | mode | multicast_if | multicast_loop |
          multicast_ttl | priority |
          {raw,
           Protocol :: non_neg_integer(),
           OptionNum :: non_neg_integer(),
           ValueSpec :: (ValueSize :: non_neg_integer()) | (ValueBin :: binary())} |
          read_packets | recbuf | reuseaddr | reuseport | reuseport_lb | sndbuf | tos | tclass | ttl |
          recvtos | recvtclass | recvttl | pktoptions | ipv6_v6only.
```

# `socket`

```erlang
-type socket() :: inet:socket().
```

A socket as returned by [`open/1,2`](`open/1`).

# `close`

```erlang
-spec close(Socket) -> ok when Socket :: socket().
```

Closes a UDP socket.

# `connect`
*since OTP 24.3* 

```erlang
-spec connect(Socket, SockAddr) -> ok | {error, Reason}
                 when
                     Socket :: socket(),
                     SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
                     Reason :: inet:posix().
```

Connect a UDP socket.

Connecting a UDP socket only means storing the specified (destination) socket
address, as specified by `SockAddr`, so that the system knows where to send
data.

When the socket is "connected" it is not necessary to specify
the destination address when sending a datagram.
That is; `send/2` may be used.

It also means that the socket will only receive data from
the connected address.  Other messages are discarded on arrival
by the OS protocol stack.

# `connect`
*since OTP 24.3* 

```erlang
-spec connect(Socket, Address, Port) -> ok | {error, Reason}
                 when
                     Socket :: socket(),
                     Address :: inet:socket_address() | inet:hostname(),
                     Port :: inet:port_number(),
                     Reason :: inet:posix().
```

Connect a UDP socket.

See `connect/2`.

With this function the destination is specified
with separate `Address` and `Port` arguments where `Address` may be
an [IP address](`t:inet:socket_address/0`)
or a [host name](`t:inet:hostname/0`).

# `controlling_process`

```erlang
-spec controlling_process(Socket, Pid) -> ok | {error, Reason}
                             when
                                 Socket :: socket(),
                                 Pid :: pid(),
                                 Reason :: closed | not_owner | badarg | inet:posix().
```

Change the controlling process (owner) of a socket.

Assigns a new controlling process `Pid` to `Socket`. The controlling process
is the process that the socket sends messages to.  If this function
is called from any other process than the current controlling process,
`{error, not_owner}` is returned.

If the process identified by `Pid` is not an existing local `t:pid/0`,
`{error, badarg}` is returned. `{error, badarg}` may also be returned
in some cases when `Socket` is closed during the execution of this function.

If the socket is in _active mode_, this function will transfer any messages
from the socket in the mailbox of the caller to the new controlling process.

If any other process is interacting with the socket during the transfer,
it may not work correctly and messages may remain in the caller's mailbox.
For instance, changing the sockets active mode during the transfer
could cause this.

# `open`

```erlang
-spec open(Port) -> {ok, Socket} | {error, Reason}
              when Port :: inet:port_number(), Socket :: socket(), Reason :: system_limit | inet:posix().
```

# `open`

```erlang
-spec open(Port, Opts) -> {ok, Socket} | {error, Reason}
              when
                  Port :: inet:port_number(),
                  Opts :: [inet:inet_backend() | open_option()],
                  Socket :: socket(),
                  Reason :: system_limit | inet:posix().
```

Open a UDP socket.

The created socket is bound to the UDP port number `Port`.
If `Port == 0`, the underlying OS assigns a free (ephemeral) UDP port;
use `inet:port/1` to retrieve it.

The process that calls this function becomes the `Socket`'s
controlling process (socket owner).

### UDP socket options

- **`list`** - Received `Packet` is delivered as a list.

- **`binary`** - Received `Packet` is delivered as a binary.

- **`{ip, Address}`** - If the local host has many IP addresses,
  this option specifies which one to use.

- **`{ifaddr, Address}`** - Same as `{ip, Address}`.

  However, if this instead is a `t:socket:sockaddr_in/0` or
  `t:socket:sockaddr_in6/0` this takes precedence over any value
  previously set with the `ip` options. If the `ip` option comes
  _after_ the `ifaddr` option, it may be used to _update_ its corresponding
  field of the `ifaddr` option (the `addr` field).

- **`{fd, integer() >= 0}`** - If a socket has somehow been opened without
  using `gen_udp`, use this option to pass the file descriptor for it.
  If `Port` is not set to `0` and/or `{ip, ip_address()}` is combined
  with this option, the `fd` is bound to the specified interface
  and port after it is being opened.  If these options are not specified,
  it is assumed that the `fd` is already bound appropriately.

- **`inet6`** - Sets up the socket for IPv6.

- **`inet`** - Sets up the socket for IPv4.

- **`local`** - Sets up a Unix Domain Socket. See `t:inet:local_address/0`

- **`{udp_module, module()}`** - Overrides which callback module is used.
  Defaults to `inet_udp` for IPv4 and `inet6_udp` for IPv6.

- **`{multicast_if, Address}`** - Sets the local device for a multicast socket.

- **`{multicast_loop, true | false}`** - When `true`, sent multicast packets
  are looped back to the local sockets.

- **`{multicast_ttl, Integer}`** - Option `multicast_ttl` changes the
  time-to-live (TTL) for outgoing multicast datagrams to control the scope of
  the multicasts.

  Datagrams with a TTL of 1 are not forwarded beyond the local network.
  Defaults to `1`.

- **`{add_membership, {MultiAddress, InterfaceAddress}}`** -
  Joins a multicast group.

- **`{drop_membership, {MultiAddress, InterfaceAddress}}`** -
  Leaves a multicast group.

- **`t:option/0`** - See `inet:setopts/2`.

UDP packets are sent with this socket using [`send(Socket, ...)`](`send/3`).
When UDP packets arrive to the `Socket`'s UDP port, and the socket is in
an _active mode_, the packets are delivered as messages to the
controlling process (socket owner):

```erlang
{udp, Socket, PeerIP, PeerPort, Packet} % Without ancillary data
{udp, Socket, PeerIP, PeerPort, AncData, Packet} % With ancillary data
```

`PeerIP` and `PeerPort` are the address from which `Packet` was sent.
`Packet` is a list of bytes (`[`[`byte/0`](`t:byte/0`)`]` if option `list`
is active and a `t:binary/0` if option `binary`is active
(they are mutually exclusive).

The message contains an `AncData` field only if any of the socket
[options](`t:option/0`) [`recvtos`](`m:inet#option-recvtos`),
[`recvtclass`](`m:inet#option-recvtclass`) or
[`recvttl`](`m:inet#option-recvttl`) are active.

When a socket in `{active, N}` mode (see `inet:setopts/2` for details),
transitions to passive (`{active, false}`) mode (`N` counts down to `0`),
the controlling process is notified by a message on this form:

```erlang
{udp_passive, Socket}
```

If the OS protocol stack reports an error for the socket, the following
message is sent to the controlling process:

```erlang
{udp_error, Socket, Reason}
```
`Reason` is mostly a [POSIX Error Code](`m:inet#posix-error-codes`).

If the socket is in _passive mode_ (not in an _active mode_), received data
can be retrieved with the`recv/2,3`](`recv/2`) calls. Note that incoming
UDP packets that are longer than the receive buffer option specifies
can be truncated without warning.

The default value for the receive buffer option is `{recbuf, 9216}`.

# `recv`

```erlang
-spec recv(Socket, Length) -> {ok, RecvData} | {error, Reason}
              when
                  Socket :: socket(),
                  Length :: non_neg_integer(),
                  RecvData :: {Address, Port, Packet} | {Address, Port, AncData, Packet},
                  Address :: inet:ip_address() | inet:returned_non_ip_address(),
                  Port :: inet:port_number(),
                  AncData :: inet:ancillary_data(),
                  Packet :: string() | binary(),
                  Reason :: not_owner | inet:posix().
```

# `recv`

```erlang
-spec recv(Socket, Length, Timeout) -> {ok, RecvData} | {error, Reason}
              when
                  Socket :: socket(),
                  Length :: non_neg_integer(),
                  Timeout :: timeout(),
                  RecvData :: {Address, Port, Packet} | {Address, Port, AncData, Packet},
                  Address :: inet:ip_address() | inet:returned_non_ip_address(),
                  Port :: inet:port_number(),
                  AncData :: inet:ancillary_data(),
                  Packet :: string() | binary(),
                  Reason :: not_owner | timeout | inet:posix().
```

Receive a packet from a socket in _passive mode_.

`Timeout` specifies a time-out in milliseconds.

If any of the socket [options](`t:option/0`)
[`recvtos`](`m:inet#option-recvtos`),
[`recvtclass`](`m:inet#option-recvtclass`)
or [`recvttl`](`m:inet#option-recvttl`) are active,
the `RecvData` tuple contains an `AncData` field,
otherwise it doesn't.

# `send`
*since OTP 24.3* 

```erlang
-spec send(Socket, Packet) -> ok | {error, Reason}
              when Socket :: socket(), Packet :: iodata(), Reason :: not_owner | inet:posix().
```

Send a packet on a connected UDP socket.

To connect a UDP socket, use `connect/2` or `connect/3`.

# `send`
*since OTP 22.1* 

```erlang
-spec send(Socket, Destination, Packet) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Destination ::
                      {inet:ip_address(), inet:port_number()} |
                      inet:family_address() |
                      socket:sockaddr_in() |
                      socket:sockaddr_in6(),
                  Packet :: iodata(),
                  Reason :: not_owner | inet:posix().
```

Equivalent to [`send(Socket, Destination, [], Packet)`](#send-4-AncData).

# `send`

```erlang
-spec send(Socket, Host, Port, Packet) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Host :: inet:hostname() | inet:ip_address(),
                  Port :: inet:port_number() | atom(),
                  Packet :: iodata(),
                  Reason :: not_owner | inet:posix();
          (Socket, Destination, AncData, Packet) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Destination ::
                      {inet:ip_address(), inet:port_number()} |
                      inet:family_address() |
                      socket:sockaddr_in() |
                      socket:sockaddr_in6(),
                  AncData :: inet:ancillary_data(),
                  Packet :: iodata(),
                  Reason :: not_owner | inet:posix();
          (Socket, Destination, PortZero, Packet) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Destination :: {inet:ip_address(), inet:port_number()} | inet:family_address(),
                  PortZero :: inet:port_number(),
                  Packet :: iodata(),
                  Reason :: not_owner | inet:posix().
```

Send a UDP packet to the specified destination.

### With arguments `Host` and `Port`

Argument `Host` can be a hostname or a socket address, and `Port`
can be a port number or a service name atom. These are resolved to
a `Destination` and then this function is equivalent to
[`send(Socket, Destination, [], Packet)`](#send-4-AncData)
just below.

### [](){: #send-4-AncData } With arguments `Destination` and `AncData` _(since OTP 22.1)_

Sends a packet to the specified `Destination` with ancillary data `AncData`.

> #### Note {: .info }
>
> The ancillary data `AncData` contains options that for this single message
> override the default options for the socket, an operation that may not be
> supported on all platforms, and if so return `{error, einval}`. Using more
> than one of an ancillary data item type may also not be supported.
> `AncData =:= []` is always supported.

### With arguments `Destination` and `PortZero` _(since OTP 22.1)_

Sends a packet to the specified `Destination`.  Since `Destination`
is a complete address, `PortZero` is redundant and has to be `0`.

This is a legacy clause mostly for `Destination = {local, Binary}`
where `PortZero` is superfluous. Equivalent to
[`send(Socket, Destination, [], Packet)`](#send-4-AncData), right above here.

# `send`
*since OTP 22.1* 

```erlang
-spec send(Socket, Host, Port, AncData, Packet) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Host :: inet:hostname() | inet:ip_address() | inet:local_address(),
                  Port :: inet:port_number() | atom(),
                  AncData :: inet:ancillary_data(),
                  Packet :: iodata(),
                  Reason :: not_owner | inet:posix().
```

Send a packet to the specified destination, with ancillary data.

Equivalent to [`send(Socket, Host, Port, Packet)`](`send/4`)
regarding `Host` and `Port` and also equivalent to
[`send(Socket, Destination, AncData, Packet)`](#send-4-AncData)
regarding the ancillary data: `AncData`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
