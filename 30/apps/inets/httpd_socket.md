# `httpd_socket`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_server/httpd_socket.erl#L23)

Communication utility functions to be used by the Erlang web server API
programmer.

This module provides the Erlang web server API module programmer with utility
functions for generic sockets communication. The appropriate communication
mechanism is transparently used, that is, `ip_comm` or `ssl`.

### See also

`m:httpd`

# `deliver`

```erlang
-spec deliver(SocketType, Socket, Data) -> Result
                 when
                     SocketType :: httpd:socket_type(),
                     Socket :: inet:socket(),
                     Data :: iolist() | binary(),
                     Result :: ok | socket_closed.
```

`deliver/3` sends `Data` over `Socket` using the specified `SocketType`.
`Socket` and `SocketType` is to be the socket and the `socket_type` form the
`mod` record as defined in `httpd.hrl`

# `peername`

```erlang
-spec peername(SocketType, Socket) -> {Port, IpAdress}
                  when
                      SocketType :: httpd:socket_type(),
                      Socket :: inet:socket() | ssl:sslsocket(),
                      Port :: inet:port_number(),
                      IpAdress :: inet:ip4_address() | inet:ip6_address() | string().
```

`peername/2` returns the `Port` and `IPAddress` of the remote `Socket`.

# `resolve`

```erlang
-spec resolve() -> HostName when HostName :: inet:hostname().
```

`resolve/0` returns the official `HostName` of the current host.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
