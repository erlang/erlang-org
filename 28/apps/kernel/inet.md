# `inet`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/kernel/src/inet.erl#L22)

Access to Network protocols.

This module, together with `m:gen_tcp`, `m:gen_udp` and `m:gen_sctp`
provides access to the Network protocols TCP, SCTP and UDP over IP,
as well as stream and datagram protocols over the local (unix)
address domain / protocol domain.

See also [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`)
or more information about how to configure an Erlang runtime system
for IP communication.

The following four Kernel configuration parameters affect the behavior of all
`m:gen_tcp`, `m:gen_udp` and `m:gen_sctp` sockets opened on an Erlang node:

- `inet_default_connect_options` can contain a list of
  default options used for all sockets created by
  a `gen_tcp:connect/2,3,4`](`gen_tcp:connect/2`) call.
- `inet_default_listen_options` can contain a list of default options
  used for sockets created by a `gen_tcp:listen/2` call.
- `inet_default_udp_options` can contain a list of
  default options used for all sockets created by
  a `gen_udp:open/1,2`](`gen_udp:open/2`) call.
- `inet_default_sctp_options` can contain a list of
  default options used for all sockets created by
  a `gen_sctp:open/0,1`](`gen_sctp:open/1`) call.

For the [`gen_tcp:accept/1,2`](`gen_tcp:accept/1`) call,
the values of the listening socket options are inherited.
Therefore there is no corresponding application variable for `accept`.

Using the Kernel configuration parameters above, one can set default options
for all TCP sockets on a node, but use this with care. Options such as
`{delay_send,true}` can be specified in this way. The following is an example
of starting an Erlang node with all sockets using delayed send:

```text
$ erl -sname test -kernel \
inet_default_connect_options '[{delay_send,true}]' \
inet_default_listen_options '[{delay_send,true}]'
```

**Please note** that the default option `{active, true}` cannot be changed,
for internal implementation reasons.

Addresses as inputs to functions can be either a string or a tuple.
For example, the IP address 150.236.20.73 can be passed to
`gethostbyaddr/1`, either as a string `"150.236.20.73"`
or as a tuple `{150, 236, 20, 73}`.

_IPv4 address examples:_

```text
Address          ip_address()
-------          ------------
127.0.0.1        {127,0,0,1}
192.168.42.2     {192,168,42,2}
```

_IPv6 address examples:_

```erlang
Address          ip_address()
-------          ------------
::1             {0,0,0,0,0,0,0,1}
::192.168.42.2  {0,0,0,0,0,0,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
::FFFF:192.168.42.2
                {0,0,0,0,0,16#FFFF,(192 bsl 8) bor 168,(42 bsl 8) bor 2}
3ffe:b80:1f8d:2:204:acff:fe17:bf38
                {16#3ffe,16#b80,16#1f8d,16#2,16#204,16#acff,16#fe17,16#bf38}
fe80::204:acff:fe17:bf38
                {16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}
```

Function `parse_address/1` can be useful:

```erlang
1> inet:parse_address("192.168.42.2").
{ok,{192,168,42,2}}
2> inet:parse_address("::FFFF:192.168.42.2").
{ok,{0,0,0,0,0,65535,49320,10754}}
```

[](){: #posix-error-codes } POSIX Error Codes
---------------------------------------------

- `e2big` - Too long argument list
- `eacces` - Permission denied
- `eaddrinuse` - Address already in use
- `eaddrnotavail` - Cannot assign requested address
- `eadv` - Advertise error
- `eafnosupport` - Address family not supported by protocol family
- `eagain` - Resource temporarily unavailable
- `ealign` - EALIGN
- `ealready` - Operation already in progress
- `ebade` - Bad exchange descriptor
- `ebadf` - Bad file number
- `ebadfd` - File descriptor in bad state
- `ebadmsg` - Not a data message
- `ebadr` - Bad request descriptor
- `ebadrpc` - Bad RPC structure
- `ebadrqc` - Bad request code
- `ebadslt` - Invalid slot
- `ebfont` - Bad font file format
- `ebusy` - File busy
- `echild` - No children
- `echrng` - Channel number out of range
- `ecomm` - Communication error on send
- `econnaborted` - Software caused connection abort
- `econnrefused` - Connection refused
- `econnreset` - Connection reset by peer
- `edeadlk` - Resource deadlock avoided
- `edeadlock` - Resource deadlock avoided
- `edestaddrreq` - Destination address required
- `edirty` - Mounting a dirty fs without force
- `edom` - Math argument out of range
- `edotdot` - Cross mount point
- `edquot` - Disk quota exceeded
- `eduppkg` - Duplicate package name
- `eexist` - File already exists
- `efault` - Bad address in system call argument
- `efbig` - File too large
- `ehostdown` - Host is down
- `ehostunreach` - Host is unreachable
- `eidrm` - Identifier removed
- `einit` - Initialization error
- `einprogress` - Operation now in progress
- `eintr` - Interrupted system call
- `einval` - Invalid argument
- `eio` - I/O error
- `eisconn` - Socket is already connected
- `eisdir` - Illegal operation on a directory
- `eisnam` - Is a named file
- `el2hlt` - Level 2 halted
- `el2nsync` - Level 2 not synchronized
- `el3hlt` - Level 3 halted
- `el3rst` - Level 3 reset
- `elbin` - ELBIN
- `elibacc` - Cannot access a needed shared library
- `elibbad` - Accessing a corrupted shared library
- `elibexec` - Cannot exec a shared library directly
- `elibmax` - Attempting to link in more shared libraries than system limit
- `elibscn` - `.lib` section in `a.out` corrupted
- `elnrng` - Link number out of range
- `eloop` - Too many levels of symbolic links
- `emfile` - Too many open files
- `emlink` - Too many links
- `emsgsize` - Message too long
- `emultihop` - Multihop attempted
- `enametoolong` - Filename too long
- `enavail` - Unavailable
- `enet` - ENET
- `enetdown` - Network is down
- `enetreset` - Network dropped connection on reset
- `enetunreach` - Network is unreachable
- `enfile` - File table overflow
- `enoano` - Anode table overflow
- `enobufs` - No buffer space available
- `enocsi` - No CSI structure available
- `enodata` - No data available
- `enodev` - No such device
- `enoent` - No such file or directory
- `enoexec` - Exec format error
- `enolck` - No locks available
- `enolink` - Link has been severed
- `enomem` - Not enough memory
- `enomsg` - No message of desired type
- `enonet` - Machine is not on the network
- `enopkg` - Package not installed
- `enoprotoopt` - Bad protocol option
- `enospc` - No space left on device
- `enosr` - Out of stream resources or not a stream device
- `enosym` - Unresolved symbol name
- `enosys` - Function not implemented
- `enotblk` - Block device required
- `enotconn` - Socket is not connected
- `enotdir` - Not a directory
- `enotempty` - Directory not empty
- `enotnam` - Not a named file
- `enotsock` - Socket operation on non-socket
- `enotsup` - Operation not supported
- `enotty` - Inappropriate device for `ioctl`
- `enotuniq` - Name not unique on network
- `enxio` - No such device or address
- `eopnotsupp` - Operation not supported on socket
- `eperm` - Not owner
- `epfnosupport` - Protocol family not supported
- `epipe` - Broken pipe
- `eproclim` - Too many processes
- `eprocunavail` - Bad procedure for program
- `eprogmismatch` - Wrong program version
- `eprogunavail` - RPC program unavailable
- `eproto` - Protocol error
- `eprotonosupport` - Protocol not supported
- `eprototype` - Wrong protocol type for socket
- `erange` - Math result unrepresentable
- `erefused` - EREFUSED
- `eremchg` - Remote address changed
- `eremdev` - Remote device
- `eremote` - Pathname hit remote filesystem
- `eremoteio` - Remote I/O error
- `eremoterelease` - EREMOTERELEASE
- `erofs` - Read-only filesystem
- `erpcmismatch` - Wrong RPC version
- `erremote` - Object is remote
- `eshutdown` - Cannot send after socket shutdown
- `esocktnosupport` - Socket type not supported
- `espipe` - Invalid seek
- `esrch` - No such process
- `esrmnt` - Srmount error
- `estale` - Stale remote file handle
- `esuccess` - Error 0
- `etime` - Timer expired
- `etimedout` - Connection timed out
- `etoomanyrefs` - Too many references
- `etxtbsy` - Text file or pseudo-device busy
- `euclean` - Structure needs cleaning
- `eunatch` - Protocol driver not attached
- `eusers` - Too many users
- `eversion` - Version mismatch
- `ewouldblock` - Operation would block
- `exdev` - Cross-device link
- `exfull` - Message tables full
- `nxdomain` - Hostname or domain name cannot be found

# `address_family`

```elixir
-type address_family() :: inet | inet6 | local.
```

# `ancillary_data`

```elixir
-type ancillary_data() :: [{tos, byte()} | {tclass, byte()} | {ttl, byte()}].
```

Ancillary data / control messages.

Ancillary data received with a data packet, read with the socket option
[`pktoptions`](`t:gen_tcp:pktoptions_value/0`) from a TCP socket,
or to set in a call to [`gen_udp:send/4`](`m:gen_udp#send-4-AncData`)
or `gen_udp:send/5`.

The value(s) correspond to the currently active socket
[options](`t:socket_setopt/0`) [`recvtos`](#option-recvtos),
[`recvtclass`](#option-recvtclass) and [`recvttl`](#option-recvttl),
or for a single send operation the option(s) to override
the currently active socket option(s).

# `family_address`

```elixir
-type family_address() :: inet_address() | inet6_address() | local_address().
```

A general network address.

A general network address format of the form `{Family, Destination}`
where `Family` is an atom such as `local` and the format of `Destination`
depends on `Family`.  `Destination` is a complete address (for example
an IP address with port number).

# `hostent`

```elixir
-type hostent() ::
          #hostent{h_name :: inet:hostname(),
                   h_aliases :: [inet:hostname()],
                   h_addrtype :: inet | inet6,
                   h_length :: non_neg_integer(),
                   h_addr_list :: [inet:ip_address()]}.
```

A record describing a host; name and address.

Corresponds to the `C`: `struct hostent` as returned by for example
`gethostbyname(3)`.

The record is defined in the Kernel include file `"inet.hrl"`.

Add the following directive to the module:

```erlang
-include_lib("kernel/include/inet.hrl").
```

# `hostname`

```elixir
-type hostname() :: atom() | string().
```

# `inet_backend`

```elixir
-type inet_backend() :: {inet_backend, inet | socket}.
```

Implementation backend selector for `t:socket/0`.

Selects the implementation backend for [sockets](`t:socket/0`).
The current default is `inet` which uses `inet_drv.c` to call
the platform's socket API. The value `socket` instead uses
the `m:socket` module and its NIF implementation.

This is a _temporary_ option that will be ignored in a future release.

# `ip4_address`

```elixir
-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
```

# `ip6_address`

```elixir
-type ip6_address() :: {0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535}.
```

# `ip_address`

```elixir
-type ip_address() :: ip4_address() | ip6_address().
```

# `local_address`

```elixir
-type local_address() :: {local, File :: binary() | string()}.
```

A network address for the `local` family (`AF_LOCAL | AF_UNIX`)

This address family, also known as "Unix domain sockets" only works
on Unix-like systems.

`File` is normally a file pathname in a local filesystem. It is limited in
length by the operating system, traditionally to 108 bytes.

A `t:binary/0` is passed as is to the operating system,
but a `t:string/0` is encoded according to the
[system filename encoding mode.](`file:native_name_encoding/0`)

Other addresses are possible, for example Linux implements
"Abstract Addresses".  See the documentation for Unix Domain Sockets
on your system, normally `unix` in manual section 7.

In most API functions where you can use this address family
the port number must be `0`.

# `port_number`

```elixir
-type port_number() :: 0..65535.
```

# `posix`

```elixir
-type posix() ::
          eaddrinuse | eaddrnotavail | eafnosupport | ealready | econnaborted | econnrefused |
          econnreset | edestaddrreq | ehostdown | ehostunreach | einprogress | eisconn | emsgsize |
          enetdown | enetunreach | enopkg | enoprotoopt | enotconn | enotty | enotsock | eproto |
          eprotonosupport | eprototype | esocktnosupport | etimedout | ewouldblock | exbadport |
          exbadseq |
          file:posix().
```

POSIX Error Code `t:atom/0`.

An atom that is named from the POSIX error codes used in Unix,
and in the runtime libraries of most C compilers.
See section [POSIX Error Codes](#posix-error-codes).

# `returned_non_ip_address`

```elixir
-type returned_non_ip_address() :: {local, binary()} | {unspec, <<>>} | {undefined, any()}.
```

a non-IP network address.

Addresses besides `t:ip_address/0` ones that are returned from
socket API functions. See in particular `t:local_address/0`.
The `unspec` family corresponds to `AF_UNSPEC` and can occur
if the other side has no socket address. The `undefined`
family can only occur in the unlikely event of an address family
that the VM doesn't recognize.

# `socket`

```elixir
-type socket() :: port() | module_socket().
```

A socket recognized by this module and its siblings.

See `t:gen_tcp:socket/0` and `t:gen_udp:socket/0`.

# `socket_address`

```elixir
-type socket_address() :: ip_address() | any | loopback | local_address().
```

# `socket_getopt`

```elixir
-type socket_getopt() :: gen_sctp:option_name() | gen_tcp:option_name() | gen_udp:option_name().
```

# `socket_optval`

```elixir
-type socket_optval() ::
          gen_sctp:option_value() | gen_tcp:option() | gen_udp:option() | gen_tcp:pktoptions_value().
```

# `socket_protocol`

```elixir
-type socket_protocol() :: tcp | udp | sctp.
```

# `socket_setopt`

```elixir
-type socket_setopt() :: gen_sctp:option() | gen_tcp:option() | gen_udp:option().
```

# `stat_option`

```elixir
-type stat_option() ::
          recv_cnt | recv_max | recv_avg | recv_oct | recv_dvi | send_cnt | send_max | send_avg |
          send_oct | send_pend.
```

# `getifaddrs_ifopts`
*not exported* 

```elixir
-type getifaddrs_ifopts() ::
          [Ifopt ::
               {flags, Flags :: [up | broadcast | loopback | pointtopoint | running | multicast]} |
               {addr, Addr :: ip_address()} |
               {netmask, Netmask :: ip_address()} |
               {broadaddr, Broadaddr :: ip_address()} |
               {dstaddr, Dstaddr :: ip_address()} |
               {hwaddr, Hwaddr :: [byte()]}].
```

Interface address description list.

A list returned from [`getifaddrs/0,1`](`getifaddrs/0`)
for a named interface, translated from the
returned data of the POSIX API function `getaddrinfo()`.

`Hwaddr` is hardware dependent, for example, on Ethernet interfaces it is the
6-byte Ethernet address (MAC address (EUI-48 address)).

The tuples `{addr,Addr}`, `{netmask,Netmask}`, and possibly
`{broadaddr,Broadaddr}` or `{dstaddr,Dstaddr}` are repeated in the list
if the interface has got multiple addresses.  An interface may have multiple
`{flag,_}` tuples for example if it has different flags for different
address families.

Multiple `{hwaddr,Hwaddr}` tuples is hard to say anything definite about,
though. The tuple `{flag,Flags}` is mandatory, all others are optional.

Do not rely too much on the order of `Flags` atoms or the `Ifopt` tuples.
There are however some rules:

- A `{flag,_}` tuple applies to all other tuples that follow.
- Immediately after `{addr,_}` follows `{netmask,_}`.
- Immediately thereafter may `{broadaddr,_}` follow if `broadcast`
  is member of `Flags`, or `{dstaddr,_}` if `pointtopoint`
  is member of `Flags`. Both `{dstaddr,_}` and `{broadaddr,_}` doesn't
  occur for the same `{addr,_}`.
- Any `{netmask,_}`, `{broadaddr,_}`, or `{dstaddr,_}` tuples that follow an
  `{addr,Addr}` tuple concerns the address `Addr`.

The tuple `{hwaddr,_}` is not returned on Solaris, as the hardware address
historically belongs to the link layer and it is not returned
by the Solaris API function `getaddrinfo()`.

> #### Warning {: .warning }
>
> On Windows, the data is fetched from different OS API functions, so the
> `Netmask` and `Broadaddr` values may be calculated, just as some `Flags`
> values.

# `inet6_address`
*not exported* 

```elixir
-type inet6_address() :: {inet6, {ip6_address() | any | loopback, port_number()}}.
```

A network address for the `inet6` family (`AF_INET6`, IPv6)
> #### Warning {: .warning }
>
> This address format is currently experimental and for completeness
> to make all address families have a `{Family, Destination}` representation.

# `inet_address`
*not exported* 

```elixir
-type inet_address() :: {inet, {ip4_address() | any | loopback, port_number()}}.
```

A network address for the `inet` family (`AF_INET`, IPv4)
> #### Warning {: .warning }
>
> This address format is currently experimental and for completeness
> to make all address families have a `{Family, Destination}` representation.

# `ether_address`
*not exported* 

```elixir
-type ether_address() :: [0..255].
```

# `i_option`
*not exported* 

```elixir
-type i_option() ::
          port | module | recv | sent | owner | local_address |
          {local_address, ShowPorts :: boolean()} |
          foreign_address |
          {foreign_address, ShowPorts :: boolean()} |
          state | type.
```

Options for selecting statistics items.

Regarding `ShowPorts`, see `show_ports` as described in the `i/2` function,
defaults to `false`.

# `module_socket`
*not exported* 

```elixir
-type module_socket() :: {'$inet', Handler :: module(), Handle :: term()}.
```

# `cancel_monitor`
*since OTP 24.0* 

```elixir
-spec cancel_monitor(MRef) -> boolean() when MRef :: reference().
```

Cancel a socket monitor.

If `MRef` is a reference that the calling process obtained by calling
`monitor/1`, this monitor is removed. If the monitoring is already removed,
nothing happens.

The returned value is one of the following:

- **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
  message corresponding to this monitor has been delivered and will not be
  delivered.

- **`false`** - The monitor was not found and couldn't be removed.
  Probably because the monitor has already triggered and there is
  a corresponding `'DOWN'` message in the caller message queue.

# `close`

```elixir
-spec close(Socket) -> ok when Socket :: socket().
```

Close a socket of any type.

# `format_error`

```elixir
-spec format_error(Reason) -> string() when Reason :: posix() | system_limit.
```

Format an error code into a `t:string/0`.

Returns a diagnostic error string. For possible POSIX values
and corresponding strings, see section
[POSIX Error Codes](#posix-error-codes).

# `get_rc`

```elixir
-spec get_rc() -> [{Par :: atom(), Val :: any()} | {Par :: atom(), Val1 :: any(), Val2 :: any()}].
```

Get the `inet` configuration.

Returns the state of the `inet` configuration database in form of
a list of recorded configuration parameters. For more information, see
[ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`).

Only actual parameters with other than default values are returned,
for example not directives that specify other sources for configuration
parameters nor directives that clear parameters.

# `getaddr`

```elixir
-spec getaddr(Host, Family) -> {ok, Address} | {error, posix()}
                 when
                     Host :: ip_address() | hostname(),
                     Family :: address_family(),
                     Address :: ip_address().
```

Resolve a host to an address, in a specific addresss family.

Returns the [IP address](`t:ip_address/0`) for `Host` as a tuple of integers.
`Host` can be an [IP address](`t:ip_address/0`), a single `t:hostname/0`,
or a fully qualified `t:hostname/0`.

# `getaddrs`

```elixir
-spec getaddrs(Host, Family) -> {ok, Addresses} | {error, posix()}
                  when
                      Host :: ip_address() | hostname(),
                      Family :: address_family(),
                      Addresses :: [ip_address()].
```

Resolve a host to a list of addresses, in a specific address family.

Returns a list of all IP addresses for `Host`.
`Host` can be an [IP address](`t:ip_address/0`),
a single `t:hostname/0`, or a fully qualified `t:hostname/0`.

# `gethostbyaddr`

```elixir
-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, posix()}
                       when Address :: string() | ip_address(), Hostent :: hostent().
```

Resolve (reverse) an address to a [`#hostent{}`](`t:hostent/0`) record.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified address.

# `gethostbyname`

```elixir
-spec gethostbyname(Hostname) -> {ok, Hostent} | {error, posix()}
                       when Hostname :: hostname(), Hostent :: hostent().
```

Resolve a hostname to a [`#hostent{}`](`t:hostent/0`) record.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified `Hostname`.

This function uses the resolver, which is often the native (OS) resolver.

If resolver option `inet6` is `true`, an IPv6 address is looked up.

See [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`) for
information about the resolver configuration.

A quirk of many resolvers is that an integer string is interpreted
as an IP address. For instance, the integer string "3232235521"
and the string "192.168.0.1" are both translated
to the IP address `{192,168,0,1}`.

# `gethostbyname`

```elixir
-spec gethostbyname(Hostname, Family) -> {ok, Hostent} | {error, posix()}
                       when Hostname :: hostname(), Family :: address_family(), Hostent :: hostent().
```

Resolve a hostname to a [`#hostent{}`](`t:hostent/0`) record,
in a specific address family.

Returns a [`#hostent{}`](`t:hostent/0`) record for the host
with the specified `Hostname`, restricted to the specified address `Family`.

See also `gethostbyname/1`.

# `gethostname`

```elixir
-spec gethostname() -> {ok, Hostname} when Hostname :: string().
```

Get the local hostname.

Returns the local hostname. Never fails.

# `getifaddrs`
*since OTP R14B01* 

```elixir
-spec getifaddrs() -> {ok, [{Ifname :: string(), Ifopts :: getifaddrs_ifopts()}]} | {error, posix()}.
```

Get interface names and addresses.

Returns a list of 2-tuples containing interface names and the interfaces'
addresses. `Ifname` is a Unicode string and `Ifopts` is a list of interface
address description tuples.

The interface address description tuples are documented under
the type of the [`Ifopts`](`t:getifaddrs_ifopts/0`) value.

# `getifaddrs`
*since OTP 21.2* 

```elixir
-spec getifaddrs([Option :: inet_backend() | {netns, Namespace :: file:filename_all()}] | socket()) ->
                    {ok, [{Ifname :: string(), Ifopts :: getifaddrs_ifopts()}]} | {error, posix()}.
```

Get interface names and addresses, in a specific namespace.

Equivalent to `getifaddrs/0`, but accepts an `Option`
`{netns, Namespace}` that, on platforms that support the feature (Linux),
sets a network namespace for the OS call.
Also,
If the option 'inet_backend' is *first* in the options list,
the specified backend will be used (for 'inet', inet and
for 'socket' the equivalent net functions will be used).
                                         

See the socket option [`{netns, Namespace}`](#option-netns)
under `setopts/2`.

# `getopts`

```elixir
-spec getopts(Socket, Options) -> {ok, OptionValues} | {error, posix()}
                 when
                     Socket :: socket(), Options :: [socket_getopt()], OptionValues :: [socket_optval()].
```

Get one or more options for a socket.

Gets all options in the list `Options` from `Socket`.
See `setopts/2` for a list of available options. See also
the descriptions of protocol specific types referenced by
[`socket_optval()` ](`t:socket_optval/0`).

The number of elements in the returned `OptionValues` list does not necessarily
correspond to the number of options asked for. If the operating system fails to
support an option, it is left out in the returned list. An error tuple is
returned only when getting options for the socket is impossible (that is, the
socket is closed or the buffer size in a raw request is too large). This
behavior is kept for backward compatibility reasons.

A raw option request `RawOptReq = {raw, Protocol, OptionNum, ValueSpec}` can be
used to get information about socket options not (explicitly) supported by the
emulator. The use of raw socket options makes the code non-portable, but allows
the Erlang programmer to take advantage of unusual features present on a
particular platform.

`RawOptReq` consists of tag `raw` followed by the protocol level, the option
number, and either a binary or the size, in bytes, of the buffer in which the
option value is to be stored. A binary is to be used when the underlying
`getsockopt` requires _input_ in the argument field. In this case, the binary
size shall correspond to the required buffer size of the return value. The
supplied values in a `RawOptReq` correspond to the second, third, and
fourth/fifth parameters to the `getsockopt` call in the C socket API. The value
stored in the buffer is returned as a binary `ValueBin`, where all values are
coded in native endianness.

Asking for and inspecting raw socket options require low-level information about
the current operating system and TCP stack.

_Example:_

Consider a Linux machine where option `TCP_INFO` can be used to collect TCP
statistics for a socket. Assume you are interested in field `tcpi_sacked` of
`struct tcp_info` filled in when asking for `TCP_INFO`. To be able to access
this information, you need to know the following:

- The numeric value of protocol level `IPPROTO_TCP`
- The numeric value of option `TCP_INFO`
- The size of `struct tcp_info`
- The size and offset of the specific field

By inspecting the headers or writing a small C program, it is found that
`IPPROTO_TCP` is 6, `TCP_INFO` is 11, the structure size is 92 (bytes), the
offset of `tcpi_sacked` is 28 bytes, and the value is a 32-bit integer. The
following code can be used to retrieve the value:

```erlang
get_tcpi_sacked(Sock) ->
    {ok,[{raw,_,_,Info}]} = inet:getopts(Sock,[{raw,6,11,92}]),
    <<_:28/binary,TcpiSacked:32/native,_/binary>> = Info,
    TcpiSacked.
```

Preferably, you would check the machine type, the operating system, and the
Kernel version before executing anything similar to this code.

# `getstat`

```elixir
-spec getstat(Socket) -> {ok, OptionValues} | {error, posix()}
                 when Socket :: socket(), OptionValues :: [{stat_option(), integer()}].
```

# `getstat`

```elixir
-spec getstat(Socket, Options) -> {ok, OptionValues} | {error, posix()}
                 when
                     Socket :: socket(),
                     Options :: [stat_option()],
                     OptionValues :: [{stat_option(), integer()}].
```

Get one or more statistics options for a socket.

[`getstat(Socket)`](`getstat/1`) is equivalent to
[`getstat(Socket, [recv_avg, recv_cnt, recv_dvi, recv_max, recv_oct, send_avg, send_cnt, send_pend, send_max, send_oct])`](`getstat/2`).

The following options are available:

- **`recv_avg`** - Average size of packets, in bytes, received by the socket.

- **`recv_cnt`** - Number of packets received by the socket.

- **`recv_dvi`** - Average packet size deviation, in bytes, received by the
  socket.

- **`recv_max`** - Size of the largest packet, in bytes, received by the socket.

- **`recv_oct`** - Number of bytes received by the socket.

- **`send_avg`** - Average size of packets, in bytes, sent from the socket.

- **`send_cnt`** - Number of packets sent from the socket.

- **`send_pend`** - Number of bytes waiting to be sent by the socket.

- **`send_max`** - Size of the largest packet, in bytes, sent from the socket.

- **`send_oct`** - Number of bytes sent from the socket.

# `i`
*since OTP 21.0* 

```elixir
-spec i() -> ok.
```

Equivalent to `i/1` for the protocols `tcp`, `udp`, and `sctp`

# `i`
*since OTP 21.0* 

```elixir
-spec i(show_ports | socket_protocol() | [i_option()]) -> ok.
```

List network sockets.

With argument `Proto` equivalent to [`i(Proto, Options)`](`i/2`)
where `Options` is a list of all `t:atom/0`s in `t:i_option/0`.

With argument `Options`, equivalent to [`i(Proto, Options)](`i/2`)
for `Proto`: `tcp`, `udp`, and `sctp`.

With argument `show_ports` **(since OTP 27.0)** equivalent to
[`i(Proto, Options)](`i/2`) where `Option` is a list of all
options in `t:i_option/0` with `ShowPorts = true`.

# `i`
*since OTP 21.0* 

```elixir
-spec i(socket_protocol(), show_ports | (Options :: [i_option()])) -> ok.
```

List network sockets.

Lists all TCP, UDP and SCTP sockets on the terminal, those created by
the Erlang runtime system as well as by the application.

The following options are available:

- **`port`** - An internal index of the port.

- **`module`** - The callback module of the socket.

- **`recv`** - Number of bytes received by the socket.

- **`sent`** - Number of bytes sent from the socket.

- **`owner`** - The socket owner process.

- **`local_address`** - The local address of the socket.

- **`foreign_address`** - The address and port of the other end of the
  connection.

- **`state`** - The connection state.

- **`type`** - STREAM or DGRAM or SEQPACKET.

The `Options` argument may also be **(since OTP 27.0)**:

- **`show_ports`** - Do *not* translate the port numbers
  (of 'local_address' and 'foreign_address') to service name(s).

# `info`
*since OTP 24.0* 

```elixir
-spec info(Socket) -> Info when Socket :: socket(), Info :: term().
```

Get information about a socket.

Returns a term containing miscellaneous information about a socket.

# `ipv4_mapped_ipv6_address`
*since OTP 21.0* 

```elixir
-spec ipv4_mapped_ipv6_address(ip_address()) -> ip_address().
```

Convert between an IPv4 address and an IPv4-mapped IPv6 address.

Convert an IPv4 address to an IPv4-mapped IPv6 address or the reverse.
When converting from an IPv6 address all but the 2 low words are ignored
so this function also works on some other types of IPv6 addresses
than IPv4-mapped.

# `is_ip_address`
*since OTP 25.0* 

```elixir
-spec is_ip_address(IPAddress) -> boolean() when IPAddress :: ip_address() | term().
```

Test for an IP address.

Tests if the argument `IPAddress` is an `t:ip_address/0`
and if so returns `true`, otherwise `false`.

# `is_ipv4_address`
*since OTP 25.0* 

```elixir
-spec is_ipv4_address(IPv4Address) -> boolean() when IPv4Address :: ip4_address() | term().
```

Test for an IPv4 address.

Tests if the argument `IPv4Address` is an `t:ip4_address/0`
and if so returns `true`, otherwise `false`.

# `is_ipv6_address`
*since OTP 25.0* 

```elixir
-spec is_ipv6_address(IPv6Address) -> boolean() when IPv6Address :: ip6_address() | term().
```

Test for an IPv6 address.

Tests if the argument `IPv6Address` is an `t:ip6_address/0`
and if so returns `true`, otherwise `false`.

# `monitor`
*since OTP 24.0* 

```elixir
-spec monitor(Socket) -> reference() when Socket :: socket().
```

Start a socket monitor.

If the `Socket` to monitor doesn't exist or when the monitor is triggered,
a `'DOWN'` message is sent that has the following pattern:

```erlang
	    {'DOWN', MonitorRef, Type, Object, Info}
```

- **`MonitorRef`** - The return value from this function.

- **`Type`** - The type of socket, can be one of the following
  `t:atom/0`s: `port` or `socket`.

- **`Object`** - The monitored entity, the socket, which triggered the event.

- **`Info`** - Either the termination reason of the socket or `nosock`
  (the `Socket` did not exist when this function was called).

Making several calls to `inet:monitor/1` for the same `Socket`
is not an error; one monitor is created per call.

The monitor is triggered when the socket is closed in any way such as
an API call, remote end close, closed by signal when owner exits, ...

# `ntoa`
*since OTP R16B02* 

```elixir
-spec ntoa(IpAddress) -> Address | {error, einval} when Address :: string(), IpAddress :: ip_address().
```

Parse an `t:ip_address/0` to an IPv4 or IPv6 address string.

# `parse_address`
*since OTP R16B* 

```elixir
-spec parse_address(Address) -> {ok, IPAddress} | {error, einval}
                       when Address :: string(), IPAddress :: ip_address().
```

Parse an IP address string to an `t:ip_address/0`.

Returns an `t:ip4_address/0` or an `t:ip6_address/0` depending
on which parsing that succeeds.

Accepts a short form IPv4 address string like `parse_ipv4_address/1`.

# `parse_ipv4_address`
*since OTP R16B* 

```elixir
-spec parse_ipv4_address(Address) -> {ok, IPv4Address} | {error, einval}
                            when Address :: string(), IPv4Address :: ip4_address().
```

Parse (relaxed) an IPv4 address string to an `t:ip4_address/0`.

Accepts a short form IPv4 address string (less than 4 fields)
such as `"127.1"` or `"0x7f000001"`.

# `parse_ipv4strict_address`
*since OTP R16B* 

```elixir
-spec parse_ipv4strict_address(Address) -> {ok, IPv4Address} | {error, einval}
                                  when Address :: string(), IPv4Address :: ip4_address().
```

Parse an IPv4 address string to an `t:ip4_address/0`.

Requires an IPv4 address string containing four fields,
that is; _not_ a short form address string.

# `parse_ipv6_address`
*since OTP R16B* 

```elixir
-spec parse_ipv6_address(Address) -> {ok, IPv6Address} | {error, einval}
                            when Address :: string(), IPv6Address :: ip6_address().
```

Parse (relaxed) an IPv6 address string to an `t:ip6_address/0`.

Also accepts a (relaxed) IPv4 address string like `parse_ipv4_address/1`
and returns an IPv4-mapped IPv6 address.

# `parse_ipv6strict_address`
*since OTP R16B* 

```elixir
-spec parse_ipv6strict_address(Address) -> {ok, IPv6Address} | {error, einval}
                                  when Address :: string(), IPv6Address :: ip6_address().
```

Parse an IPv6 address string to an `t:ip6_address/0`.

_Doesn't_ accept an IPv4 address string.  An IPv6 address string, though,
allows an IPv4 tail like this: `"::127.0.0.1"`
(which is the same as `"::7f00:0001"`).

# `parse_strict_address`
*since OTP R16B* 

```elixir
-spec parse_strict_address(Address) -> {ok, IPAddress} | {error, einval}
                              when Address :: string(), IPAddress :: ip_address().
```

Parse an IP address string to an `t:ip_address/0`.

Like `parse_address/1` but _doesn't_ accept a short form IPv4 address string.

# `peername`

```elixir
-spec peername(Socket :: socket()) ->
                  {ok, {ip_address(), port_number()} | returned_non_ip_address()} | {error, posix()}.
```

Return the address of the socket's remote end.

Returns the address and port for the other end of a connection.

Notice that for SCTP sockets, this function returns only one of
the peer addresses of the socket. Function [`peernames/1,2`](`peernames/1`)
returns all.

# `peernames`
*since OTP R16B03* 

```elixir
-spec peernames(Socket :: socket()) ->
                   {ok, [{ip_address(), port_number()} | returned_non_ip_address()]} | {error, posix()}.
```

Equivalent to [`peernames(Socket, 0)`](`peernames/2`).

Notice that the behavior of this function for an SCTP one-to-many style socket
is not defined by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13).

# `peernames`
*since OTP R16B03* 

```elixir
-spec peernames(Socket, Assoc) -> {ok, [{Address, Port}]} | {error, posix()}
                   when
                       Socket :: socket(),
                       Assoc ::
                           #sctp_assoc_change{state :: term(),
                                              error :: term(),
                                              outbound_streams :: term(),
                                              inbound_streams :: term(),
                                              assoc_id :: term()} |
                           gen_sctp:assoc_id(),
                       Address :: ip_address(),
                       Port :: non_neg_integer().
```

Return the addresses of all remote ends of a socket.

Returns a list of all address/port number pairs for the remote end of an
association `Assoc` of a socket.

This function can return multiple addresses for multihomed sockets,
such as SCTP sockets. For other sockets it returns a one-element list.

Notice that parameter `Assoc` is by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
defined to be ignored for one-to-one style sockets.
What the special value `0` means, is unfortunately undefined,
and hence the behavior for one-to-many style sockets.

# `port`

```elixir
-spec port(Socket) -> {ok, Port} | {error, any()} when Socket :: socket(), Port :: port_number().
```

Return the local port number for a socket.

# `setopts`

```elixir
-spec setopts(Socket, Options) -> ok | {error, posix()}
                 when Socket :: socket(), Options :: [socket_setopt()].
```

Set one or more options for a socket.

Sets the list of `Options` on `Socket`.

The following options are available:

- **`{active, true | false | once | N}`** [](){: #option-active } -
  If the value is `true`, which is the default, everything received
  from the socket is sent as messages to the receiving process.

  If the value is `false` (passive mode), the process must explicitly receive
  incoming data by calling [`gen_tcp:recv/2,3`](`gen_tcp:recv/2`),
  [`gen_udp:recv/2,3`](`gen_udp:recv/2`), or
  [`gen_sctp:recv/1,2`](`gen_sctp:recv/1`) (depending on the type of socket).

  If the value is `once` (`{active, once}`), _one_ data message from the socket
  is sent to the process. To receive one more message,
  [`setopts/2`](`setopts/2`) must be called again with option `{active, once}`.

  If the value is an integer `N` in the range -32768 to 32767 (inclusive), the
  value is added to the socket's count of data messages sent to the controlling
  process. A socket's default message count is `0`. If a negative value is
  specified, and its magnitude is equal to or greater than the socket's current
  message count, the socket's message count is set to `0`. Once the socket's
  message count reaches `0`, either because of sending received data messages to
  the process or by being explicitly set, the process is then notified by a
  special message, specific to the type of socket, that the socket has entered
  passive mode. Once the socket enters passive mode, to receive more messages
  [`setopts/2`](`setopts/2`) must be called again to set the socket back into an
  active mode.

  When using `{active, once}` or `{active, N}`, the socket changes behavior
  automatically when data is received. This can be confusing in combination with
  connection-oriented sockets (that is, `gen_tcp`), as a socket with
  `{active, false}` behavior reports closing differently than a socket with
  `{active, true}` behavior. To simplify programming, a socket where the peer
  closed, and this is detected while in `{active, false}` mode, still generates
  message `{tcp_closed, Socket}` when set to `{active, once}`, `{active, true}`,
  or `{active, N}` mode. It is therefore safe to assume that message
  `{tcp_closed, Socket}`, possibly followed by socket port termination (depending
  on option `exit_on_close`) eventually appears when a socket changes back and
  forth between `{active, true}` and `{active, false}` mode. However, _when_
  peer closing is detected it is all up to the underlying TCP/IP stack and
  protocol.

  Notice that `{active, true}` mode provides no flow control; a fast sender can
  easily overflow the receiver with incoming messages. The same is true for
  `{active, N}` mode, while the message count is greater than zero.

  Use active mode only if your high-level protocol provides its own flow control
  (for example, acknowledging received messages) or the amount of data exchanged
  is small. Using `{active, false}` mode, `{active, once}` mode, or
  `{active, N}` mode with values of `N` appropriate for the application to
  provide flow control, ensures the other side cannot send faster than the
  receiver can read.

- **`{broadcast, Boolean}` (UDP sockets)** [](){: #option-broadcast } -
  Enables/disables permission to send broadcasts.

- **`{buffer, Size}`** [](){: #option-buffer } -
  The size of the user-level buffer used by the driver.  Not to be confused
  with options `sndbuf` and `recbuf`, which correspond to the
  Kernel socket buffers. For TCP it is recommended to have
  `val(buffer) >= val(recbuf)` to avoid performance issues because of
  unnecessary copying. For UDP the same recommendation applies, but the max
  should not be larger than the MTU of the network path. `val(buffer)` is
  automatically set to the above maximum when `recbuf` is set. However, as the
  size set for `recbuf` usually become larger, you are encouraged to use
  `getopts/2` to analyze the behavior of your operating system.

  Note that this is also the maximum amount of data that can be received from a
  single recv call. If you are using higher than normal MTU consider setting
  buffer higher.

- **`{delay_send, Boolean}`** - Normally, when an Erlang process sends to a
  socket, the driver tries to send the data immediately. If that fails, the
  driver uses any means available to queue up the message to be sent whenever
  the operating system says it can handle it. Setting `{delay_send, true}` makes
  _all_ messages queue up. The messages sent to the network are then larger but
  fewer. The option affects the scheduling of send requests versus Erlang
  processes instead of changing any real property of the socket. The option is
  implementation-specific. Defaults to `false`.

- **`{deliver, port | term}`** - When `{active, true}`, data is delivered on the
  form `port` : `{S, {data, [H1,..Hsz | Data]}}`
  or `term` : `{tcp, S, [H1..Hsz | Data]}`.

- **`{dontroute, Boolean}`** - Enables/disables routing bypass for outgoing
  messages.

- **`{exit_on_close, Boolean}`** - This option is set to `true` by default.

  The only reason to set it to `false` is if you want to continue sending data
  to the socket after a close is detected, for example, if the peer uses
  `gen_tcp:shutdown/2` to shut down the write side.

- **`{exclusiveaddruse, Boolean}`** [](){: #option-exclusiveaddruse } -
  Enables/disables exclusive address/port usage on Windows. That is, by enabling
  this option you can prevent other sockets from binding to the same
  address/port.  By default this option is disabled. That is, other sockets
  may use the same address/port by setting
  [`{reuseaddr, true}`](#option-reuseaddr) in combination with
  [`{reuseport, true}`](#option-reuseport) unless
  `{exclusiveaddruse, true}` has been set on `Socket`. On non-Windows systems
  this option is silently ignored.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{header, Size}`** - This option is only meaningful if option `binary` was
  specified when the socket was created. If option `header` is specified, the
  first `Size` number bytes of data received from the socket are elements of a
  list, and the remaining data is a binary specified as the tail of the same
  list. For example, if `Size == 2`, the data received matches
  `[Byte1, Byte2 | Binary]`.

- **`{high_msgq_watermark, Size}`** - The socket message queue is set to a busy
  state when the amount of data on the message queue reaches this limit. Notice
  that this limit only concerns data that has not yet reached the ERTS internal
  socket implementation. Defaults to 8 kB.

  Senders of data to the socket are suspended if either the socket message queue
  is busy or the socket itself is busy.

  For more information, see options `low_msgq_watermark`, `high_watermark`, and
  `low_watermark`.

  Notice that distribution sockets disable the use of `high_msgq_watermark` and
  `low_msgq_watermark`. Instead use the
  [distribution buffer busy limit](`m:erlang#system_info_dist_buf_busy_limit`),
  which is a similar feature.

- **`{high_watermark, Size}` (TCP/IP sockets)** - The socket is set to a busy
  state when the amount of data queued internally by the ERTS socket
  implementation reaches this limit. Defaults to 8 kB.

  Senders of data to the socket are suspended if either the socket message queue
  is busy or the socket itself is busy.

  For more information, see options `low_watermark`, `high_msgq_watermark`, and
  `low_msqg_watermark`.

- **`{ipv6_v6only, Boolean}`** - Restricts the socket to use only IPv6,
  prohibiting any IPv4 connections. This is only applicable for IPv6 sockets
  (option `inet6`).

  On most platforms this option must be set on the socket before associating it
  to an address. It is therefore only reasonable to specify it when creating the
  socket and not to use it when calling function (`setopts/2`) containing this
  description.

  The behavior of a socket with this option set to `true` is the only portable
  one. The original idea when IPv6 was new of using IPv6 for all traffic is now
  not recommended by FreeBSD (you can use `{ipv6_v6only,false}` to override the
  recommended system default value), forbidden by OpenBSD (the supported GENERIC
  kernel), and impossible on Windows (which has separate IPv4 and IPv6 protocol
  stacks). Most Linux distros still have a system default value of `false`. This
  policy shift among operating systems to separate IPv6 from IPv4 traffic has
  evolved, as it gradually proved hard and complicated to get a dual stack
  implementation correct and secure.

  On some platforms, the only allowed value for this option is `true`, for
  example, OpenBSD and Windows. Trying to set this option to `false`, when
  creating the socket, fails in this case.

  Setting this option on platforms where it does not exist is ignored. Getting
  this option with `getopts/2` returns no value, that is, the returned list does
  not contain an `{ipv6_v6only,_}` tuple. On Windows, the option does not exist,
  but it is emulated as a read-only option with value `true`.

  Therefore, setting this option to `true` when creating a socket never fails,
  except possibly on a platform where you have customized the kernel to only
  allow `false`, which can be doable (but awkward) on, for example, OpenBSD.

  If you read back the option value using `getopts/2` and get no value, the
  option does not exist in the host operating system. The behavior of both an
  IPv6 and an IPv4 socket listening on the same port, and for an IPv6 socket
  getting IPv4 traffic is then no longer predictable.

- **`{keepalive, Boolean}` (TCP/IP sockets)** - Enables/disables periodic
  transmission on a connected socket when no other data is exchanged. If the
  other end does not respond, the connection is considered broken and an error
  message is sent to the controlling process. Defaults to `false`.

- **`{keepcnt, Integer}` (TCP/IP sockets)** - Linux specific `TCP_KEEPCNT`.

- **`{keepidle, Integer}` (TCP/IP sockets)** - Linux specific `TCP_KEEPIDLE`.

- **`{keepintvl, Integer}` (TCP/IP sockets)** - Linux specific `TCP_KEEPINTVL`.

- **`{linger, {true|false, Seconds}}`** [](){: #option-linger } -
  Determines the time-out, in seconds, for flushing unsent data
  in the [`close/1`](`close/1`) socket call.

  The first component is if linger is enabled, the second component is the
  flushing time-out, in seconds. There are 3 alternatives:

  - **`{false, _}`** - close/1 or shutdown/2 returns immediately, not waiting
    for data to be flushed, with closing happening in the background.

  - **`{true, 0}`** - Aborts the connection when it is closed. Discards any data
    still remaining in the send buffers and sends RST to the peer.

    This avoids TCP's TIME_WAIT state, but leaves open the possibility that
    another "incarnation" of this connection being created.

  - **`{true, Time} when Time > 0`** - close/1 or shutdown/2 will not return
    until all queued messages for the socket have been successfully sent or the
    linger timeout (Time) has been reached.

- **`{low_msgq_watermark, Size}`** - If the socket message queue is in a busy
  state, the socket message queue is set in a not busy state when the amount of
  data queued in the message queue falls below this limit. Notice that this
  limit only concerns data that has not yet reached the ERTS internal socket
  implementation. Defaults to 4 kB.

  Senders that are suspended because of either a busy message queue or a busy
  socket are resumed when the socket message queue and the socket are not busy.

  For more information, see options `high_msgq_watermark`, `high_watermark`, and
  `low_watermark`.

  Notice that distribution sockets disable the use of `high_msgq_watermark` and
  `low_msgq_watermark`. Instead they use the
  [distribution buffer busy limit](`m:erlang#system_info_dist_buf_busy_limit`),
  which is a similar feature.

- **`{low_watermark, Size}` (TCP/IP sockets)** - If the socket is in a busy
  state, the socket is set in a not busy state when the amount of data queued
  internally by the ERTS socket implementation falls below this limit. Defaults
  to 4 kB.

  Senders that are suspended because of a busy message queue or a busy socket
  are resumed when the socket message queue and the socket are not busy.

  For more information, see options `high_watermark`, `high_msgq_watermark`, and
  `low_msgq_watermark`.

- **`{mode, Mode :: binary | list}`** - Received `Packet` is delivered as
  defined by `Mode`.

- **`{netns, Namespace :: file:filename_all()}`{: #option-netns }** - Sets a
  network namespace for the socket. Parameter `Namespace` is a filename defining
  the namespace, for example, `"/var/run/netns/example"`, typically created by
  command `ip netns add example`. This option must be used in a function call
  that creates a socket, that is, [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`),
  `gen_tcp:listen/2`, [`gen_udp:open/1,2`](`gen_udp:open/1`) or
  [`gen_sctp:open/0,1,2`](`gen_sctp:open/0`), and also `getifaddrs/1`.

  This option uses the Linux-specific syscall `setns()`, such as in Linux kernel
  3.0 or later, and therefore only exists when the runtime system is compiled
  for such an operating system.

  The virtual machine also needs elevated privileges, either running as
  superuser or (for Linux) having capability `CAP_SYS_ADMIN` according to the
  documentation for `setns(2)`. However, during testing also `CAP_SYS_PTRACE`
  and `CAP_DAC_READ_SEARCH` have proven to be necessary.

  _Example:_

  ```text
  setcap cap_sys_admin,cap_sys_ptrace,cap_dac_read_search+epi beam.smp
  ```

  Notice that the filesystem containing the virtual machine executable
  (`beam.smp` in the example) must be local, mounted without flag `nosetuid`,
  support extended attributes, and the kernel must support file capabilities.
  All this runs out of the box on at least Ubuntu 12.04 LTS, except that SCTP
  sockets appear to not support network namespaces.

  `Namespace` is a filename and is encoded and decoded as discussed in module
  `m:file`, with the following exceptions:

  - Emulator flag `+fnu` is ignored.
  - `getopts/2` for this option returns a binary for the filename if the stored
    filename cannot be decoded. This is only to occur if you set the option
    using a binary that cannot be decoded with the emulator's filename encoding:
    `file:native_name_encoding/0`.

- **`{bind_to_device, Ifname :: binary()}`** - Binds a socket to a specific
  network interface. This option must be used in a function call that creates a
  socket, that is, [`gen_tcp:connect/3,4`](`gen_tcp:connect/3`),
  `gen_tcp:listen/2`, [`gen_udp:open/1,2`](`gen_udp:open/1`), or
  [`gen_sctp:open/0,1,2`](`gen_sctp:open/0`).

  Unlike `getifaddrs/0`, Ifname is encoded a binary. In the unlikely case that a
  system is using non-7-bit-ASCII characters in network device names, special
  care has to be taken when encoding this argument.

  This option uses the Linux-specific socket option `SO_BINDTODEVICE`, such as
  in Linux kernel 2.0.30 or later, and therefore only exists when the runtime
  system is compiled for such an operating system.

  Before Linux 3.8, this socket option could be set, but could not retrieved
  with `getopts/2`. Since Linux 3.8, it is readable.

  The virtual machine also needs elevated privileges, either running as
  superuser or (for Linux) having capability `CAP_NET_RAW`.

  The primary use case for this option is to bind sockets into
  [Linux VRF instances](http://www.kernel.org/doc/Documentation/networking/vrf.txt).

- **`list`** - Received `Packet` is delivered as a list.

- **`binary`** - Received `Packet` is delivered as a binary.

- **`{nodelay, Boolean}` (TCP/IP sockets)** [](){: #option-nodelay } -
  If `Boolean == true`, option `TCP_NODELAY` is turned on for the socket,
  which means that also small amounts of data are sent immediately.

  This option is _not_ supported for `domain = local`, but if
  `inet_backend =/= socket` this error will be _ignored_.

- **`{nopush, Boolean}` (TCP/IP sockets)** - This translates to `TCP_NOPUSH` on
  BSD and to `TCP_CORK` on Linux.

  If `Boolean == true`, the corresponding option is turned on for the socket,
  which means that small amounts of data are accumulated until a full MSS-worth
  of data is available or this option is turned off.

  Note that while `TCP_NOPUSH` socket option is available on OSX, its semantics
  is very different (e.g., unsetting it does not cause immediate send of
  accumulated data). Hence, `nopush` option is intentionally ignored on OSX.

- **`{packet, PacketType}` (TCP/IP sockets)** [](){: #option-packet } -
  Defines the type of packets to use for a socket. Possible values:

  - **`raw | 0`** - No packaging is done.

  - **`1 | 2 | 4`** - Packets consist of a header specifying the number of bytes
    in the packet, followed by that number of bytes. The header length can be
    one, two, or four bytes, and containing an unsigned integer in big-endian
    byte order. Each send operation generates the header, and the header is
    stripped off on each receive operation.

    The 4-byte header is limited to 2Gb.

  - **`asn1 | cdr | sunrm | fcgi | tpkt | line`** - These packet types only have
    effect on receiving. When sending a packet, it is the responsibility of the
    application to supply a correct header. On receiving, however, one message
    is sent to the controlling process for each complete packet received, and,
    similarly, each call to `gen_tcp:recv/2,3` returns one complete packet. The
    header is _not_ stripped off.

    The meanings of the packet types are as follows:

    - `asn1` - ASN.1 BER
    - `sunrm` - Sun's RPC encoding
    - `cdr` - CORBA (GIOP 1.1)
    - `fcgi` - Fast CGI
    - `tpkt` - TPKT format \[RFC1006]
    - `line` - Line mode, a packet is a line-terminated with newline, lines
      longer than the receive buffer are truncated

  - **`http | http_bin`** - The Hypertext Transfer Protocol. The packets are
    returned with the format according to `HttpPacket` described in
    `erlang:decode_packet/3` in ERTS. A socket in passive mode returns
    `{ok, HttpPacket}` from `gen_tcp:recv` while an active socket sends messages
    like `{http, Socket, HttpPacket}`.

  - **`httph | httph_bin`** - These two types are often not needed, as the
    socket automatically switches from `http`/`http_bin` to `httph`/`httph_bin`
    internally after the first line is read. However, there can be occasions
    when they are useful, such as parsing trailers from chunked encoding.

- **`{packet_size, Integer}`(TCP/IP sockets)** - Sets the maximum allowed length
  of the packet body. If the packet header indicates that the length of the
  packet is longer than the maximum allowed length, the packet is considered
  invalid. The same occurs if the packet header is too large for the socket
  receive buffer.

  For line-oriented protocols (`line`, `http*`), option `packet_size` also
  guarantees that lines up to the indicated length are accepted and not
  considered invalid because of internal buffer limitations.

- **`{line_delimiter, Char}` (TCP/IP sockets)**
  [](){: #option-line_delimiter } -
  Sets the line delimiting character for line-oriented protocols (`line`).
  Defaults to `$\n`.

- **`{raw, Protocol, OptionNum, ValueBin}`** - See below.

- **`{read_ahead, Boolean}`** [](){: #option-read_ahead } -
  If set to `false` avoids reading ahead from the OS socket layer.
  The default for this option is `true` which speeds up packet header parsing.
  Setting `false` has a performance penalty because the packet header
  has to be read first, to know exactly how many bytes to read for the body,
  which roughly doubles the number of read operations.

  The use of this option is essential for example before switching to kTLS
  which activates OS socket layer encryption and decryption by setting
  special (raw) socket options.  So if the Erlang socket layer has read ahead,
  it has read bytes that was for the OS socket layer to decrypt,
  which makes packet decryption derail for the connection.

  > #### Warning {: .warning }
  >
  > For packet modes that doesn't have the packet length at a fixed location
  > in a packet header, such as `line` or `asn1`, not reading ahead
  > can become very inefficient since sometimes the only way to accomplish
  > this is to read one byte at the time until the length
  > or packet end is found.

- **`{read_packets, Integer}` (UDP sockets)** [](){: #option-read_packets } -
  Sets the maximum number of UDP packets to read without intervention
  from the socket when data is available.  When this many packets
  have been read and delivered to the destination process,
  new packets are not read until a new notification of available data
  has arrived. Defaults to `5`. If this parameter is set too high, the system
  can become unresponsive because of UDP packet flooding.

- **`{recbuf, Size}`** [](){: #option-recbuf } -
  The minimum size of the receive buffer to use for the socket.
  You are encouraged to use `getopts/2` to retrieve the size
  set by your operating system.

- **`{recvtclass, Boolean}`** [](){: #option-recvtclass } -
  If set to `true` activates returning the received `TCLASS` value
  on platforms that implements the protocol `IPPROTO_IPV6` option
  `IPV6_RECVTCLASS` or `IPV6_2292RECVTCLASS` for the socket.
  The value is returned as a `{tclass,TCLASS}` tuple regardless of if
  the platform returns an `IPV6_TCLASS` or an `IPV6_RECVTCLASS` CMSG value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TCLASS` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TCLASS` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{recvtos, Boolean}`** [](){: #option-recvtos } -
  If set to `true` activates returning the received `TOS` value
  on platforms that implements the protocol `IPPROTO_IP` option
  `IP_RECVTOS` for the socket. The value is returned as a `{tos,TOS}` tuple
  regardless of if the platform returns an `IP_TOS` or an `IP_RECVTOS` CMSG
  value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TOS` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TOS` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{recvttl, Boolean}`** [](){: #option-recvttl } -
  If set to `true` activates returning the received `TTL` value
  on platforms that implements the protocol `IPPROTO_IP` option `IP_RECVTTL`
  for the socket. The value is returned as a `{ttl,TTL}` tuple
  regardless of if the platform returns an `IP_TTL` or an `IP_RECVTTL` CMSG
  value.

  For packet oriented sockets that supports receiving ancillary data with the
  payload data (`gen_udp` and `gen_sctp`), the `TTL` value is returned in an
  extended return tuple contained in an
  [ancillary data](`t:ancillary_data/0`) list. For stream oriented sockets
  (`gen_tcp`) the only way to get the `TTL` value is if the platform supports
  the [`pktoptions`](`t:gen_tcp:pktoptions_value/0`) option.

- **`{reuseaddr, Boolean}`[](){: #option-reuseaddr }** -
   Allows or disallows reuse of local address. By default, reuse is disallowed.

  > #### Note {: .info }
  >
  > On windows `{reuseaddr, true}` will have no effect unless also
  > [`{reuseport, true}`](#option-reuseport) is set. If both are set,
  > the `SO_REUSEADDR` Windows socket option will be enabled. This since setting
  > `SO_REUSEADDR` on Windows more or less has the same behavior as setting both
  > `SO_REUSEADDR` and `SO_REUSEPORT` on BSD. This behavior was introduced as of
  > OTP 26.0.
  >
  > > #### Change {: .info }
  > >
  > > Previous behavior on Windows:
  > >
  > > - Prior to OTP 25.0, the `{reuseaddr, true}` option was silently ignored.
  > > - Between OTP 25.0 and up to the predecessor of OTP 25.2, the underlying
  > >   `SO_REUSEADDR` socket option was set if `{reuseaddr, true}` was set.
  > > - Between OTP 25.2 and up to the predecessor of OTP 26.0, the underlying
  > >   `SO_REUSEADDR` socket option was only set on UDP sockets if
  > >   `{reuseaddr, true}` was set, and silently ignored on other sockets.
  >
  > See also the [`exclusiveaddruse`](#option-exclusiveaddruse) option.

- **`{reuseport, Boolean}`[](){: #option-reuseport }** -
   Allows or disallows reuse of local port which _may or may not_
  have load balancing depending on the underlying OS. By default,
  reuse is disallowed. See also [`reuseport_lb`](#option-reuseport_lb).

  > #### Note {: .info }
  >
  > On windows `{reuseport, true}` will have no effect unless also
  > [`{reuseaddr, true}`](#option-reuseaddr) is set. If both are set,
  > the `SO_REUSEADDR` Windows socket option will be enabled. This since setting
  > `SO_REUSEADDR` on Windows more or less has the same behavior as setting both
  > `SO_REUSEADDR` and `SO_REUSEPORT` on BSD. The `reuseport` option was
  > introduced as of OTP 26.0.
  >
  > See also the [`exclusiveaddruse`](#option-exclusiveaddruse) option.

  > #### Note {: .info }
  >
  > `reuseport` _may or may not_ be the same underlying option as
  > [`reuseport_lb`](#option-reuseport_lb) depending on the underlying
  > OS. They, for example, are on Linux. When they are the same underlying
  > option, operating on both may cause them to interact in surprising ways. For
  > example, by enabling `reuseport` and then disabling `reuseport_lb` both will
  > end up being disabled.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{reuseport_lb, Boolean}`[](){: #option-reuseport_lb }** -
  Allows or disallows reuse of local port _with_ load balancing.
  By default, reuse is disallowed.  See also [`reuseport`](#option-reuseport).

  > #### Note {: .info }
  >
  > `reuseport_lb` _may or may not_ be the same underlying option as
  > [`reuseport`](#option-reuseport) depending on the underlying OS.
  > On Linux, for example, they are.  And when they are the same
  >  underlying option, operating on both may cause them to interact
  > in surprising ways. For example, by enabling `reuseport_lb`,
  > and then disabling `reuseport`, both will end up being disabled.

  > #### Note {: .info }
  >
  > This option is _currently_ not supported for socket created with
  > `inet_backend = socket`

- **`{send_timeout, Integer}`** - Only allowed for connection-oriented sockets.

  Specifies a longest time to wait for a send operation to be accepted by the
  underlying TCP stack. When the limit is exceeded, the send operation returns
  `{error, timeout}`. How much of a packet that got sent is unknown; the socket
  is therefore to be closed whenever a time-out has occurred (see
  `send_timeout_close` below). Defaults to `infinity`.

- **`{send_timeout_close, Boolean}`** - Only allowed for connection-oriented
  sockets.

  Used together with `send_timeout` to specify whether the socket is to be
  automatically closed when the send operation returns `{error, timeout}`. The
  recommended setting is `true`, which automatically closes the socket. Defaults
  to `false` because of backward compatibility.

- **`{show_econnreset, Boolean}` (TCP/IP sockets)**
  [](){: #option-show_econnreset } -
  When this option is set to `false`, which is default, an RST
  received from the TCP peer is treated as a normal close
  (as though an FIN was sent). A caller to `gen_tcp:recv/2` gets
  `{error, closed}`. In active mode, the controlling process receives a
  `{tcp_closed, Socket}` message, indicating that the peer has closed the
  connection.

  Setting this option to `true` allows you to distinguish between a connection
  that was closed normally, and one that was aborted (intentionally or
  unintentionally) by the TCP peer. A call to `gen_tcp:recv/2` returns
  `{error, econnreset}`. In active mode, the controlling process receives a
  `{tcp_error, Socket, econnreset}` message before the usual
  `{tcp_closed, Socket}`, as is the case for any other socket error. Calls to
  `gen_tcp:send/2` also returns `{error, econnreset}` when it is detected that a
  TCP peer has sent an RST.

  A connected socket returned from `gen_tcp:accept/1` inherits the
  `show_econnreset` setting from the listening socket.

- **`{sndbuf, Size}`** [](){: #option-sndbuf } -
  The minimum size of the send buffer to use for the socket.
  You are encouraged to use `getopts/2`, to retrieve the size
  set by your operating system.

- **`{priority, Integer}`** - Sets the `SO_PRIORITY` socket level option on
  platforms where this is implemented. The behavior and allowed range varies
  between different systems. The option is ignored on platforms where it is not
  implemented. Use with caution.

- **`{tos, Integer}`** - Sets `IP_TOS IP` level options on platforms where this
  is implemented. The behavior and allowed range varies between different
  systems. The option is ignored on platforms where it is not implemented. Use
  with caution.

- **`{tclass, Integer}`** - Sets `IPV6_TCLASS IP` level options on platforms
  where this is implemented. The behavior and allowed range varies between
  different systems. The option is ignored on platforms where it is not
  implemented. Use with caution.

- **`{user_timeout, Integer}` (TCP/IP sockets)** - Linux specific
  `TCP_USER_TIMEOUT`.

In addition to these options, _raw_ option specifications can be used. The raw
options are specified as a tuple of arity four, beginning with tag `raw`,
followed by the protocol level, the option number, and the option value
specified as a binary. This corresponds to the second, third, and fourth
arguments to the `setsockopt` call in the C socket API. The option value must be
coded in the native endianness of the platform and, if a structure is required,
must follow the structure alignment conventions on the specific platform.

Using raw socket options requires detailed knowledge about the current operating
system and TCP stack.

_Example:_

This example concerns the use of raw options. Consider a Linux system where you
want to set option `TCP_LINGER2` on protocol level `IPPROTO_TCP` in the stack.
You know that on this particular system it defaults to 60 (seconds), but you
want to lower it to 30 for a particular socket. Option `TCP_LINGER2` is not
explicitly supported by `inet`, but you know that the protocol level translates
to number 6, the option number to number 8, and the value is to be specified as
a 32-bit integer. You can use this code line to set the option for the socket
named `Sock`:

```text
inet:setopts(Sock, [{raw,6,8,<<30:32/native>>}]),
```

As many options are silently discarded by the stack if they are specified out of
range; it can be a good idea to check that a raw option is accepted. The
following code places the value in variable `TcpLinger2:`

```text
{ok,[{raw,6,8,<<TcpLinger2:32/native>>}]}=inet:getopts(Sock,[{raw,6,8,4}]),
```

Code such as these examples is inherently non-portable, even different versions
of the same OS on the same platform can respond differently to this kind of
option manipulation. Use with care.

Notice that the default options for TCP/IP sockets can be changed with the
Kernel configuration parameters mentioned in the beginning of this manual page.

# `sockname`

```elixir
-spec sockname(Socket :: socket()) ->
                  {ok, {ip_address(), port_number()} | returned_non_ip_address()} | {error, posix()}.
```

Return the local address and port number for a socket.

Notice that for SCTP sockets this function returns only one of the socket
addresses. Function [`socknames/1,2`](`socknames/1`) returns all.

# `socknames`
*since OTP R16B03* 

```elixir
-spec socknames(Socket :: socket()) ->
                   {ok, [{ip_address(), port_number()} | returned_non_ip_address()]} | {error, posix()}.
```

Equivalent to [`socknames(Socket, 0)`](`socknames/2`).

# `socknames`
*since OTP R16B03* 

```elixir
-spec socknames(Socket, Assoc) -> {ok, [{Address, Port}]} | {error, posix()}
                   when
                       Socket :: socket(),
                       Assoc ::
                           #sctp_assoc_change{state :: term(),
                                              error :: term(),
                                              outbound_streams :: term(),
                                              inbound_streams :: term(),
                                              assoc_id :: term()} |
                           gen_sctp:assoc_id(),
                       Address :: ip_address(),
                       Port :: non_neg_integer().
```

Return all localaddresses for a socket.

Returns a list of all local address/port number pairs for a socket,
for the specified association `Assoc`.

This function can return multiple addresses for multihomed sockets,
such as SCTP sockets. For other sockets it returns a one-element list.

Notice that parameter `Assoc` is by the
[SCTP Sockets API Extensions](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
defined to be ignored for one-to-one style sockets.  For one-to-many style
sockets, the special value `0` is defined to mean that the returned addresses
must be without any particular association. How different SCTP implementations
interpret this varies somewhat.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
