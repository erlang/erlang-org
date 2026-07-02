# `socket`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/socket.erl#L23)

Socket interface.

This module provides an API for network sockets. Functions are provided to
create, delete and manipulate the sockets as well as sending and receiving data
on them.

The intent is that it shall be as "close as possible" to the OS level socket
interface. The only significant additions are that some of the functions, e.g.
`recv/3`, have a time-out argument, and that [`recv/*`](`recv/1`) for a
[`stream`](`t:type/0`) socket iterates until the requested amount of data
has been received.

[](){: #asynchronous-calls }

> #### Note {: .info }
>
> #### Asynchronous Calls
>
> Some functions feature _asynchronous calls_.  This is achieved by setting
> the `Timeout` argument to `nowait` or to a `Handle ::` `t:reference/0`.
> See the respective function's type specification.
>
> This module has two different implementations of asynchronous calls.
> One on the _Unix_ family of operating systems:
> `select` - based on the standard socket interface's
> `select(3)`/`poll(3)` calls, and one on _Windows_: `completion` -
> based on asynchronous I/O Completion Ports.
> The difference shows in the return values and message formats
> because they have slightly different semantics.
>
> #### The `completion` and `select` Return Values
>
> For instance, the call [`recv(Socket, 0, nowait)`](#recv-nowait),
> when there is no data available for reading, will,
> depending on the operating system, return one of:
>
> - `{completion, `[`CompletionInfo`](`t:completion_info/0`)`}`
> - `{select, `[`SelectInfo`](`t:select_info/0`)`}`
>
> Where `CompletionInfo` is
> `{completion_info, _, `[`CompletionHandle`](`t:completion_handle/0`)`}`
> and `SelectInfo` is
> `{select_info, _, `[`SelectHandle`](`t:select_handle/0`)`}`.
> Both the `CompletionHandle` and the `SelectHandle`
> are of type `t:reference/0`.
>
> When the operation can continue, a `completion` message containing
> the `CompletionHandle` or a `select` message containing
> the `SelectHandle` is sent to the calling process.
>
> On `select` systems, [`recv/2,3,4`](`recv/2`) may also return:
>
> - `{select, {`[`SelectInfo`](`t:select_info/0`)`, Data}`
>
> This may happen for sockets of type [`stream`](`t:type/0`)
> when `Length > 1` since the OS may split a data stream at any point
> and deliver just the first part of the requested data.
> For the next [`recv/2,3,4`](`recv/2`) call; the `Length` to receive
> will probably have to be adjusted due to the already delivered data
> in this return value.
>
> On `select` systems, when the `{otp, select_read}` option is `true`,
> the asynchronous [`recv/3,4`](#recv-nowait),
> [`recvfrom/3,4`](#recvfrom-nowait), and
> [`recvmsg/3,4,5`](#recvmsg-nowait) functions may also return:
>
> - `{select_read, {`[`SelectInfo`](`t:select_info/0`)`, Data}`
>
> This indicates that the receive operation was completed;
> all requested data has been delivered,  and that the calling process
> will get a `select` message when there is data available
> for the next receive operation.
>
> #### The `completion` and `select` Messages [](){: #async-messages }
>
> The `completion` message has the format:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, completion,
>   {`[`CompletionHandle`](`t:completion_handle/0`)`, CompletionStatus}}`
>
> The `select` message has the format:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, select,
>   `[`SelectHandle`](`t:select_handle/0`)`}`
>
> When a `completion` message is received (which contains the _result_
> of the operation), it means that the operation has been _completed_ and
> `CompletionStatus` is the return value for the operation,
> which is what the function that initiated the operation
> could have returned, with the `nowait` argument,
> except for the `completion` and `select` return values.
>
> When a `select` message is received, it only means that the operation
> _may now continue_, by retrying the operation (which may return
> a new `{select, _}` value).  Some operations are retried by repeating
> the same function call, and some have a dedicated function variant
> to be used for the retry.  See the respective function's documentation.
>
> #### Operation Queuing on `select` Systems
>
> On `select` systems, all other processes are _locked out_ until the
> current process has completed the operation as in a continuation
> call has returned a value indicating success or failure
> (not a `select` or `select_read` return).  Other processes are queued
> and get a `select` return which makes them wait for their turn.
>
> Note that receiving data from parallel processes is only suitable
> for some protocols.  For a [`stream`](`t:type/0`) socket
> it is in general a recipe for disaster.
>
> #### Cancelling an operation
>
> An operation that is in progress (not completed) may be canceled
> using `cancel/2` both on `completion` and `select` systems.
>
> Cancelling an operation ensures that there is no `completion`,
> `select`, nor `abort` message in the inbox after the `cancel/2` call.
>
> #### Using a `Handle`
>
> If creating a `t:reference/0` with [`make_ref()`](`erlang:make_ref/0`)
> and using that as the `Timeout | Handle` argument, the same `Handle`
> will then be the [`SelectHandle`](`t:select_handle/0`) in the returned
> `t:select_info/0` and the received `select` message, or be
> the [`CompletionHandle`](`t:completion_handle/0`) in the returned
> `t:completion_info/0` and the received `completion` message.
>
> The compiler may then optimize a following `receive` statement
> to only scan the messages that arrive after the `t:reference/0`
> is created.  If the message queue is large this is a big optimization.
>
> It is not possible to have more than one operation in progress
> with the same `t:reference/0`.
>
> #### Repeating an Operation on a `select` Systems
>
> On`select` systems, if a call would be repeated _before_ the `select`
> message has been received it replaces the operation in progress:
>
> ```erlang
>     {select, {select_info, Handle}} = socket:accept(LSock, nowait),
>     {ok, Socket} = socket:accept(LSock, 1000),
>     :
> ```
> Above, `Handle` is _no longer_ valid once the second `accept/2`, call
> has been made (the first call is automatically canceled).
> After the second `accept/2` call returns, the accept operation
> has completed.
>
> Note that there is a race here; there is no way to know if the call
> is repeated _before_ the `select` message is sent since it _may_
> have been sent just before the repeated call.  So now there
> might be a `select` message containing `Handle` in the inbox.
>
> #### The `abort` Message
>
> Another message the user must be prepared for
> (when using `nowait | Handle`) is the `abort` message:
>
> - `{'$socket', `[`Socket`](`t:socket/0`)`, abort, Info}`
>
> This message indicates that the operation in progress has been aborted.
> For instance, if the socket has been closed by another process;
> `Info` will be `{Handle, closed}`.

> #### Note {: .info }
>
> Support for IPv6 has been implemented but not _fully_ tested.

This module was introduced in OTP 22.0, as experimental code.
* In OTP 22.1, the `nowait` argument was added for many functions,
  and the `cancel/2` and `info/1` functions were also added.
* In OTP 22.3, the `number_of/0` function was added.
* In OTP 23.0, the functions [`is_supported/1,2`](`is_supported/1`)
  and the [`open/1,2`](`open/1`) functions with an `FD` argument were added.
* In OTP 23.1, the `use_registry/1` function was added.
* In OTP 24.0, the `t:select_handle/0` argument was added for many functions,
  the `cancel/1`, `cancel_monitor/1`, `getopt/3` with tuple options,
  `getopt_native/3`, `info/0`, `monitor/1`, `open/3` with an option list,
  many variants of the [`recv/*`](`recv/2`),
  [`recvfrom/*`](`recvfrom/1`), [`recvmsg/*`](`recvmsg/1`),
  [`send/*`](`send/2`), [`sendmsg/*`](`sendmsg/2`),
  the [`sendto/*`](`sendto/3`) functions,
  the [`sendfile/*`](`sendfile/5`) functions,
  and the `setopt/3`, `setopt_native/3` functions, were added.
* In OTP 24.1, the [`i/*`](`i/0`) functions were added.
* In OTP 24.2, the [`ioctl/*`](`ioctl/2`) functions were added.
* In OTP 26.0, the `t:completion_handle/0` argument was added for
  many functions, and the `cancel/2` function with `t:completion_info/0`
  argument was added.  That is: support for Windows asynchronous
  I/O Completion Ports was added.  The Unix-ish flavored
  ([select handle](`t:select_handle/0`)) API features could be considered
  no longer experimental.
* In OTP 27.0, the Windows flavored
  ([completion handle](`t:completion_handle/0`))
  API features could be considered no longer experimental.
* In OTP 29.0, (experimental) complete support for SCTP was added
  (functionally feature compatible with inet).
  Not (yet) supported for FreeBSD.

## Examples

```erlang
client(SAddr, SPort) ->
   {ok, Sock} = socket:open(inet, stream, tcp),
   ok = socket:connect(Sock, #{family => inet,
                               addr   => SAddr,
                               port   => SPort}),
   Msg = <<"hello">>,
   ok = socket:send(Sock, Msg),
   ok = socket:shutdown(Sock, write),
   {ok, Msg} = socket:recv(Sock),
   ok = socket:close(Sock).

server(Addr, Port) ->
   {ok, LSock} = socket:open(inet, stream, tcp),
   ok = socket:bind(LSock, #{family => inet,
                             port   => Port,
                             addr   => Addr}),
   ok = socket:listen(LSock),
   {ok, Sock} = socket:accept(LSock),
   {ok, Msg} = socket:recv(Sock),
   ok = socket:send(Sock, Msg),
   ok = socket:close(Sock),
   ok = socket:close(LSock).
```

# `cmsg`
*since OTP 22.0* 

```erlang
-type cmsg() :: cmsg_recv() | cmsg_send().
```

Control messages (ancillary messages).

# `cmsg_recv`
*since OTP 22.0* 

```erlang
-type cmsg_recv() ::
          #{level := socket, type := timestamp, data := binary(), value => timeval()} |
          #{level := socket, type := timestampns, data := binary(), value => timespec()} |
          #{level := socket, type := rights, data := binary()} |
          #{level := socket, type := credentials, data := binary()} |
          #{level := ip, type := tos, data := binary(), value => ip_tos()} |
          #{level := ip, type := recvtos, data := binary(), value := ip_tos()} |
          #{level := ip, type := ttl, data := binary(), value => integer()} |
          #{level := ip, type := recvttl, data := binary(), value := integer()} |
          #{level := ip, type := pktinfo, data := binary(), value => ip_pktinfo()} |
          #{level := ip, type := origdstaddr, data := binary(), value => sockaddr_recv()} |
          #{level := ip, type := recverr, data := binary(), value => extended_err()} |
          #{level := ipv6, type := hoplimit, data := binary(), value => integer()} |
          #{level := ipv6, type := pktinfo, data := binary(), value => ipv6_pktinfo()} |
          #{level := ipv6, type := recverr, data := binary(), value => extended_err()} |
          #{level := ipv6, type := tclass, data := binary(), value => integer()}.
```

Control messages (ancillary messages) returned by
[`recvmsg/1,2,3,5`](`recvmsg/1`).

A control message has got a `data` field with a native (`binary`) value for the
message data, and may also have a decoded `value` field if this socket library
successfully decoded the data.

# `cmsg_send`
*since OTP 22.0* 

```erlang
-type cmsg_send() ::
          #{level := sctp, type := sndrcv, value := sctp_snd_rcv_info()} |
          #{level := socket, type := timestamp, data => native_value(), value => timeval()} |
          #{level := socket, type := rights, data := native_value()} |
          #{level := socket, type := credentials, data := native_value()} |
          #{level := ip, type := tos, data => native_value(), value => iptos_value()} |
          #{level := ip, type := ttl, data => native_value(), value => integer()} |
          #{level := ip, type := hoplimit, data => native_value(), value => integer()} |
          #{level := ipv6, type := tclass, data => native_value(), value => integer()}.
```

Control messages (ancillary messages) accepted by
[`sendmsg/2,3,4`](`sendmsg/2`).

A control message may for some message types have a `value` field with a
symbolic value, or a `data` field with a native value, that has to be binary
compatible what is defined in the platform's header files.

# `completion_handle`
*since OTP 26.0* 

```erlang
-type completion_handle() :: reference().
```

[Completion operation](#asynchronous-calls) handle.

A `t:reference/0` that uniquely identifies the (completion) operation,
contained in the returned `t:completion_info/0`.

# `completion_info`
*since OTP 26.0* 

```erlang
-type completion_info() ::
          {completion_info, CompletionTag :: completion_tag(), CompletionHandle :: completion_handle()}.
```

[Completion operation](#asynchronous-calls) info.

Returned by an operation that requires the caller to wait for a
[`completion` message](#async-messages) containing the
[`CompletionHandle`](`t:completion_handle/0`) _and_ the result of the operation;
the `CompletionStatus`.

# `completion_tag`
*since OTP 26.0* 

```erlang
-type completion_tag() ::
          accept | connect |
          recv | recvfrom | recvmsg | recvmmsg | send | sendv | sendto | sendmsg | sendmmsg | sendfile.
```

[Completion operation](#asynchronous-calls) tag.

A tag that describes the ongoing (completion) operation (= function name),
contained in the returned `t:completion_info/0`.

# `domain`
*since OTP 22.0* 

```erlang
-type domain() :: inet | inet6 | local | unspec.
```

Protocol _domain_ a.k.a address _family_.

A lowercase `t:atom/0` representing a protocol _domain_
on the platform named `AF_*` (or `PF_*`).  For example
`inet` corresponds to `AF_INET`.

[`is_supported(ipv6)` ](`is_supported/1`) tells if the IPv6 protocol,
protocol domain `inet6`, is supported.

[`is_supported(local)` ](`is_supported/1`) tells if the
protocol domain `local` is supported.

`supports/0` reports both values, but also many more, with a single call.

# `ee_origin`
*since OTP 22.0* 

```erlang
-type ee_origin() :: none | local | icmp | icmp6.
```

# `eei`
*since OTP 22.0* 

```erlang
-type eei() ::
          #{info := econnreset | econnaborted | netname_deleted | too_many_cmds | atom(),
            raw_info := term()}.
```

Extended Error Information.

A term containing additional (error) information
_if_ the socket NIF has been configured to produce it.

# `extended_err`
*since OTP 22.0* 

```erlang
-type extended_err() ::
          #{error := posix(),
            origin := icmp,
            type := dest_unreach,
            code := icmp_dest_unreach() | 0..255,
            info := 0..4294967295,
            data := 0..4294967295,
            offender := sockaddr_recv()} |
          #{error := posix(),
            origin := icmp,
            type := time_exceeded | 0..255,
            code := 0..255,
            info := 0..4294967295,
            data := 0..4294967295,
            offender := sockaddr_recv()} |
          #{error := posix(),
            origin := icmp6,
            type := dest_unreach,
            code := icmpv6_dest_unreach() | 0..255,
            info := 0..4294967295,
            data := 0..4294967295,
            offender := sockaddr_recv()} |
          #{error := posix(),
            origin := icmp6,
            type := pkt_toobig | time_exceeded | 0..255,
            code := 0..255,
            info := 0..4294967295,
            data := 0..4294967295,
            offender := sockaddr_recv()} |
          #{error := posix(),
            origin := ee_origin() | 0..255,
            type := 0..255,
            code := 0..255,
            info := 0..4294967295,
            data := 0..4294967295,
            offender := sockaddr_recv()}.
```

# `hatype`
*not exported* *since OTP 22.0* 

```erlang
-type hatype() ::
          netrom | eether | ether | ax25 | pronet | chaos | ieee802 | arcnet | appletlk | dlci | atm |
          metricom | ieee1394 | eui64 | infiniband | tunnel | tunnel6 | loopback | localtlk | none |
          void |
          non_neg_integer().
```

# `icmp_dest_unreach`
*since OTP 22.0* 

```erlang
-type icmp_dest_unreach() ::
          net_unreach | host_unreach | port_unreach | frag_needed | net_unknown | host_unknown.
```

# `icmpv6_dest_unreach`
*since OTP 22.0* 

```erlang
-type icmpv6_dest_unreach() ::
          noroute | adm_prohibited | not_neighbour | addr_unreach | port_unreach | policy_fail |
          reject_route.
```

# `in6_addr`
*since OTP 22.0* 

```erlang
-type in6_addr() :: {0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535, 0..65535}.
```

# `in6_flow_info`
*not exported* *since OTP 22.0* 

```erlang
-type in6_flow_info() :: 0..1048575.
```

# `in6_scope_id`
*not exported* *since OTP 22.0* 

```erlang
-type in6_scope_id() :: 0..4294967295.
```

# `in_addr`
*since OTP 22.0* 

```erlang
-type in_addr() :: {0..255, 0..255, 0..255, 0..255}.
```

# `info`
*not exported* *since OTP 22.0* 

```erlang
-type info() ::
          #{counters := #{atom() := non_neg_integer()},
            iov_max := non_neg_integer(),
            use_registry := boolean(),
            io_backend := #{name := atom()},
            load_nif_result := undefined | ok | {error, term()}}.
```

Platform dependent information items.

The value of `iov_max` is the value of the `IOV_MAX` constant
in the system headers, which is the largest allowed I/O vector.
See also `sendmsg/4` regarding the `iov` key of `t:msg_send/0`.
The smallest allowed `IOV_MAX` value according to POSIX is `16`,
but check your platform documentation to be sure.

About the `use_registry` key, see `use_registry/1`
and the `t:otp_socket_option/0` with the same name.

# `info_keys`
*since OTP 22.0* 

```erlang
-type info_keys() ::
          [domain | type | protocol | fd | owner | local_address | remote_address | recv | sent | state].
```

Information element designators for the  `i/1` and `i/2` functions.

# `interface_type`
*since OTP 22.0* 

```erlang
-type interface_type() ::
          other | hdh1822 | x25ddh | x25 | ether | ppp | loop | ipv4 | ipv6 | '6to4' | gif | faith |
          stf | bridge | cellular |
          non_neg_integer().
```

The interface type (of the datalink). We only translate a few values to atoms,
the rest are left as (unsigned) integer values.

# `invalid`
*since OTP 22.0* 

```erlang
-type invalid() :: {invalid, What :: term()}.
```

# `ioctl_device_flag`
*since OTP 22.0* 

```erlang
-type ioctl_device_flag() ::
          up | broadcast | debug | loopback | pointopoint | notrailers | knowsepoch | running | noarp |
          promisc | allmulti | master | oactive | slave | simplex | link0 | link1 | link2 | multicast |
          portsel | automedia | cantconfig | ppromisc | dynamic | monitor | staticarp | dying |
          renaming | nogroup | lower_up | dormant | echo.
```

# `ioctl_device_map`
*since OTP 22.0* 

```erlang
-type ioctl_device_map() ::
          #{mem_start := non_neg_integer(),
            mem_end := non_neg_integer(),
            base_addr := non_neg_integer(),
            irq := non_neg_integer(),
            dma := non_neg_integer(),
            port := non_neg_integer()}.
```

# `ip_mreq`
*since OTP 22.0* 

```erlang
-type ip_mreq() :: #{multiaddr := in_addr(), interface := in_addr()}.
```

C: `struct ip_mreq`

Corresponds to the C `struct ip_mreq` for managing multicast groups.

# `ip_mreq_source`
*since OTP 22.0* 

```erlang
-type ip_mreq_source() :: #{multiaddr := in_addr(), interface := in_addr(), sourceaddr := in_addr()}.
```

C: `struct ip_mreq_source`

Corresponds to the C `struct ip_mreq_source` for managing multicast groups.

# `ip_msfilter`
*since OTP 22.0* 

```erlang
-type ip_msfilter() ::
          #{multiaddr := in_addr(),
            interface := in_addr(),
            mode := include | exclude,
            slist := [in_addr()]}.
```

C: `struct ip_msfilter`

Corresponds to the C `struct ip_msfilter` for managing
multicast source filtering (RFC 3376).

# `ip_pktinfo`
*since OTP 22.0* 

```erlang
-type ip_pktinfo() :: #{ifindex := non_neg_integer(), spec_dst := in_addr(), addr := in_addr()}.
```

C: `struct ip_pktinfo`

# `ip_pmtudisc`
*since OTP 22.0* 

```erlang
-type ip_pmtudisc() :: want | dont | do | probe.
```

C: `IP_PMTUDISC_*` values.
=>
Lowercase `t:atom/0` values corresponding to the C library constants
`IP_PMTUDISC_*`. Some constant(s) may be unsupported by the platform.

# `ip_tos`
*since OTP 22.0* 

```erlang
-type ip_tos() ::
          #{native := iptos_native(), tos := iptos_tos(), dscp := iptos_dscp() | non_neg_integer()}.
```

C: `IPTOS_*` values.

Note that since there are two different representations of TOS;
according to RFC 1349 ("classic TOS") and RFC 2474 (DSCP),
we have three different value representations for tos:
`native` (the raw unencoded value of the TOS octet), `tos` (classic),
and `dscp`.

When sending or setting (the ip tos option), the user can choose
between the three different (value) representations.
When reading, the value is represented as a map with all three
representations, since 'socket' does not know which one is expected.
Its then up to the user pick the one they want.

An integer `dscp` value is a DSCP field value that is not known
from the IANA registry (see `t:iptos_dscp/0`).

# `iptos_dscp`
*since OTP 22.0* 

```erlang
-type iptos_dscp() ::
          cs0 | le | cs1 | af11 | af12 | af13 | cs2 | af21 | af22 | af23 | cs3 | af31 | af32 | af33 |
          cs4 | af41 | af42 | af43 | cs5 | voice_admit | nqb | ef | cs6 | cs7.
```

These symbolic DSCP values are according to IANA's
[Differentiated Services Field Codepoints registry]
(https://www.iana.org/assignments/dscp-registry/dscp-registry.xhtml).

# `iptos_native`
*since OTP 22.0* 

```erlang
-type iptos_native() :: non_neg_integer().
```

# `iptos_tos`
*since OTP 22.0* 

```erlang
-type iptos_tos() ::
          #{precedence := iptos_tos_prec() | non_neg_integer(),
            tos := iptos_tos_value() | non_neg_integer()}.
```

Lowercase `t:atom/0` values corresponding to the C library constants `IPTOS_*`.
The atoms are named like The C library names, but to avoid platform depencendy,
the set of names and values follow RFC 1349, not the C library header files.

An integer value is a field value that is not named in the RFC.

# `iptos_tos_prec`
*not exported* *since OTP 22.0* 

```erlang
-type iptos_tos_prec() ::
          netcontrol | internetcontrol | critical_ecp | flashoverride | flash | immediate | priority |
          routine.
```

# `iptos_tos_value`
*not exported* *since OTP 22.0* 

```erlang
-type iptos_tos_value() :: default | lowdelay | throughput | reliability | mincost.
```

# `iptos_value`
*since OTP 22.0* 

```erlang
-type iptos_value() :: iptos_tos() | iptos_dscp() | iptos_native().
```

# `ipv6_hops`
*since OTP 22.0* 

```erlang
-type ipv6_hops() :: default | 0..255.
```

IPv6 hop limit value.

The value `default` is only valid to _set_ and is translated to the C value
`-1`, meaning the route default.

# `ipv6_mreq`
*since OTP 22.0* 

```erlang
-type ipv6_mreq() :: #{multiaddr := in6_addr(), interface := non_neg_integer()}.
```

C: `struct ipv6_mreq`

Corresponds to the C `struct ipv6_mreq` for managing multicast groups. See also
RFC 2553.

# `ipv6_pktinfo`
*since OTP 22.0* 

```erlang
-type ipv6_pktinfo() :: #{addr := in6_addr(), ifindex := integer()}.
```

C: `struct in6_pktinfo`

# `ipv6_pmtudisc`
*since OTP 22.0* 

```erlang
-type ipv6_pmtudisc() :: want | dont | do | probe.
```

C: `IPV6_PMTUDISC_*` values

Lowercase `t:atom/0` values corresponding to the C library constants
`IPV6_PMTUDISC_*`. Some constant(s) may be unsupported by the platform.

# `level`
*since OTP 22.0* 

```erlang
-type level() :: socket | protocol().
```

Protocol level.

A lowercase `t:atom/0` OS protocol level, that is:
`socket` or a `t:protocol/0` name.

`socket` is the `SOL_SOCKET` protocol level in the OS header files,
with options named `SO_`\* .

# `linger`
*since OTP 22.0* 

```erlang
-type linger() :: #{onoff := boolean(), linger := non_neg_integer()}.
```

C: `struct linger`

Corresponds to the C `struct linger` for managing the
[socket option](`t:socket_option/0`) `{socket, linger}`.

# `msg`
*since OTP 22.0* 

```erlang
-type msg() :: msg_send() | msg_recv().
```

C: `struct msghdr`

# `msg_data_recv`
*not exported* *since OTP 22.0* 

```erlang
-type msg_data_recv() ::
          #{addr => sockaddr_recv(),
            iov := erlang:iovec(),
            ctrl := [cmsg_recv() | #{level := level() | integer(), type := integer(), data := binary()}],
            flags := [msg_flag() | integer()]}.
```

Message returned by [`recvmsg/1,2,3,5`](`recvmsg/1`).

Corresponds to a C `struct msghdr`, see your platform documentation for
[`recvmsg(2)`](`recvmsg/1`).

- **`addr`** - Optional peer address, used on unconnected sockets. Corresponds
  to `msg_name` and `msg_namelen` fields of a `struct msghdr`. If `NULL` the map
  key is not present.

- **`iov`** - Data as a list of binaries. The `msg_iov` and `msg_iovlen` fields
  of a `struct msghdr`.

- **`ctrl`** - A possibly empty list of control messages (CMSG). Corresponds to
  the `msg_control` and `msg_controllen` fields of a `struct msghdr`.

- **`flags`** - Message flags. Corresponds to the `msg_flags` field of a
  `struct msghdr`. Unknown flags, if any, are returned in one `t:integer/0`,
  last in the containing list.

# `msg_flag`
*since OTP 22.0* 

```erlang
-type msg_flag() ::
          cmsg_cloexec | confirm | ctrunc | dontroute | eor | errqueue | more | oob | peek | trunc.
```

Platform dependent message flags.

Translates to/from message flag constants on the platform.
These flags are lowercase while the constants are uppercase
with prefix `MSG_`; for example `oob` translates to `MSG_OOB`.

Some flags are only used for sending, some only for receiving, some in received
control messages, and some for several of these. Not all flags are supported on
all platforms. See the platform's documentation,
[`supports(msg_flags)`](`supports/1`), and
[`is_supported(msg_flags, MsgFlag)`](`is_supported/2`).

# `msg_notification_recv`
*not exported* *since OTP 29.0* 

```erlang
-type msg_notification_recv() ::
          #{addr => sockaddr_recv(),
            notification := sctp_notification(),
            ctrl := [#{level := level() | integer(), type := integer(), data := binary()}],
            flags := [notification | [msg_flag() | integer()]]}.
```

Notifications can be received on a SCTP socket (type = seqpacket and
protocol = sctp).

# `msg_recv`
*since OTP 22.0* 

```erlang
-type msg_recv() :: msg_data_recv() | msg_notification_recv().
```

Message returned by [`recvmsg/1,2,3,5`](`recvmsg/1`).

# `msg_send`
*since OTP 22.0* 

```erlang
-type msg_send() ::
          #{addr => sockaddr(),
            iov := erlang:iovec(),
            ctrl => [cmsg_send() | #{level := level() | integer(), type := integer(), data := binary()}]}.
```

Message sent by [`sendmsg/2,3,4`](`sendmsg/2`).

Corresponds to a C `struct msghdr`, see your platform documentation for
`sendmsg(2)`.

- **`addr`** - Optional peer address, used on unconnected sockets. Corresponds
  to `msg_name` and `msg_namelen` fields of a `struct msghdr`. If not used they
  are set to `NULL`, `0`.

- **`iov`** - Mandatory data as a list of binaries. The `msg_iov` and
  `msg_iovlen` fields of a `struct msghdr`.

- **`ctrl`** - Optional list of control messages (CMSG). Corresponds to the
  `msg_control` and `msg_controllen` fields of a `struct msghdr`. If not used
  they are set to `NULL`, `0`.

The `msg_flags` field of the `struct msghdr` is set to `0`.

# `native_value`
*not exported* *since OTP 22.0* 

```erlang
-type native_value() :: integer() | boolean() | binary().
```

# `otp_socket_option`
*since OTP 22.0* 

```erlang
-type otp_socket_option() ::
          debug | iow | controlling_process | rcvbuf | rcvctrlbuf | sndctrlbuf | select_read | meta |
          use_registry | fd | domain.
```

Protocol level `otp` socket option.

Socket options for the `otp` pseudo protocol level,
that is: `{otp, Name}` options.

This protocol level is the Erlang/OTP's socket implementation layer,
hence above all OS protocol levels.

- **`debug`** - `t:boolean/0` \- Activate debug logging.

- **`iow`** - `t:boolean/0` \- Inform On Wrap of statistics counters.

- **`controlling_process`** - `t:pid/0` \- The socket "owner". Only the current
  controlling process can set this option.

- **`rcvbuf`** -
  `BufSize :: (default | integer()>0) | {N :: integer()>0, BufSize :: (default | integer()>0)} `\-
  Receive buffer size.

  The value `default` is only valid to _set_.

  `N` specifies the number of read attempts to do in a tight loop before
  assuming no more data is pending.

  This is the allocation size for the receive buffer used when calling the OS
  protocol stack's receive API, when no specific size (size 0) is requested.
  When the receive function returns the receive buffer is reallocated to the
  actually received size. If the data is copied or shrunk in place is up to
  the allocator, and can to some extent be configured in the Erlang VM.

  The similar socket option; `{socket,rcvbuf}` is a related option for the OS'
  protocol stack that on Unix corresponds to `SOL_SOCKET,SO_RCVBUF`.

- **`rcvctrlbuf`** - `BufSize :: (default | integer()>0) `\- Allocation size for
  the ancillary data buffer used when calling the OS protocol stack's receive
  API.

  The value `default` is only valid to _set_.

- **`sndctrlbuf`** - `BufSize :: (default | integer()>0) `\- Allocation size for
  the ancillary data buffer used when calling the OS protocol stack's
  [sendmsg](`sendmsg/2`) API.

  The value `default` is only valid to _set_.

  It is the user's responsibility to set a buffer size that has room for the
  encoded ancillary data in the message to send.

  See [sendmsg](`sendmsg/2`) and also the `ctrl` field of the `t:msg_send/0`
  type.

- **`select_read`** - `t:boolean/0` \-
  On `select` implementations, see [Asynchronous Calls](#asynchronous-calls),
  automatically activate select after a completed read.

  Instead of `{ok, Data}` the receive operation returns
  [`{select_read, {SelectInfo, Data}}`](`t:select_info/0`),
  and the calling process can wait for a [`select` message](#async-messages)
  containing `SelectInfo` when there is data available again.

  Setting this option locks out other processes from receiving any data
  since the current process continues its operation, so it effectively
  disables receive operation queuing.

- **`fd`** - `t:integer/0` \- Only valid to _get_. The OS protocol levels'
  socket descriptor. Functions [`open/1,2`](`open/1`) can be used to create a
  socket according to this module from an existing OS socket descriptor.

- **`use_registry`** - `t:boolean/0` \- Only valid to _get_. The value is set
  when the socket is created with `open/2` or `open/4`.

Options not described here are intentionally undocumented and for Erlang/OTP
internal use only.

# `packet_type`
*not exported* *since OTP 22.0* 

```erlang
-type packet_type() ::
          host | broadcast | multicast | otherhost | outgoing | loopback | user | kernel | fastroute |
          non_neg_integer().
```

# `port_number`
*since OTP 22.0* 

```erlang
-type port_number() :: 0..65535.
```

# `posix`
*not exported* *since OTP 22.0* 

```erlang
-type posix() :: inet:posix().
```

Posix error codes.

Local alias for `t:inet:posix/0`, a set of `t:atom/0`s.

# `protocol`
*since OTP 22.0* 

```erlang
-type protocol() :: atom().
```

Protocol name.

A lowercase `t:atom/0` representing an OS protocol name.
To be used for example in `t:socket_option/0`
in [control messages](`t:cmsg/0`).

They have the following names in the OS header files:

- **`ip`** - `IPPROTO_IP` a.k.a `SOL_IP` with options named `IP_`\*.

- **`ipv6`** - `IPPROTO_IPV6` a.k.a `SOL_IPV6` with options named `IPV6_`\*.

- **`tcp`** - `IPPROTO_TCP` with options named `TCP_`\*.

- **`mptcp`** - `IPPROTO_MPTCP` with a few options named `MPTCP_`\*.
  Most `TCP_`\* options apply which is the purpose of Multi-Path TCP.

- **`udp`** - `IPPROTO_UDP` with options named `UDP_`\*.

- **`sctp`** - `IPPROTO_SCTP` with options named `SCTP_`\*.

There are many other possible protocols, but the ones above are those for which
this socket library implements socket options and/or control messages.

All protocols known to the OS are enumerated when the Erlang VM is started,
through the `C` library call `getprotoent()`. See the OS man page for
protocols(5). Those in the list above are valid if supported by the platform,
even if they aren't enumerated.

The calls [`is_supported(ipv6)` ](`is_supported/1`),
[`is_supported(sctp)` ](`is_supported/1`) and
[`is_supported(mptcp)` ](`is_supported/1`) can be used to find out
if the protocols `ipv6`, `sctp` and `mptcp` are supported on the platform
as in; appropriate header file and libraries exist.

The call [`is_supported(protocols, Protocol)` ](`is_supported/2`)
can only be used to find out if the platform knows the protocol number
for a named `Protocol`.

See [`open/2,3,4`](`open/3`)

# `sctp_adapt_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_adapt_event() ::
          #{type := adaptation_event,
            flags := uint16(),
            adaption_ind := uint32(),
            assoc_id := sctp_assoc_id()}.
```

A peer has sent a Adaptation Layer Indication parameter.

C: `struct sctp_adaptation_event`

# `sctp_assoc_change`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_assoc_change() ::
          #{type := assoc_change,
            flags := integer(),
            state := sctp_assoc_change_state(),
            error := sctp_operation_error(),
            outbound_streams := integer(),
            inbound_streams := integer(),
            assoc_id := sctp_assoc_id()}.
```

An SCTP association has either begun or ended.

C: `struct sctp_assoc_change`

# `sctp_assoc_change_state`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_assoc_change_state() ::
          comm_up | comm_lost | restart | shutdown_comp | cant_str_assoc | integer().
```

# `sctp_assoc_id`
*since OTP 22.0* 

```erlang
-type sctp_assoc_id() :: integer().
```

C: `sctp_assoc_t`

# `sctp_assoc_reset_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_assoc_reset_event() ::
          #{type := assoc_reset,
            flags := [denied | failed],
            assoc_id := sctp_assoc_id(),
            local_tsn := uint32(),
            remote_tsn := uint32()}.
```

C: `struct sctp_assoc_reset_event`

# `sctp_assoc_stats`
*since OTP 22.0* 

```erlang
-type sctp_assoc_stats() ::
          #{assoc_id := sctp_assoc_id(),
            max_rto_addr := sockaddr(),
            max_rto := uint64(),
            in_sacks := uint64(),
            out_sacks := uint64(),
            in_packets := uint64(),
            out_packets := uint64(),
            rtx_chunks := uint64(),
            out_of_seq_tsns := uint64(),
            in_dup_chunks := uint64(),
            gap_ack_recv := uint64(),
            in_unordered_chunks := uint64(),
            out_unordered_chunks := uint64(),
            in_ordered_chunks := uint64(),
            out_ordered_chunks := uint64(),
            in_ctrl_chunks := uint64(),
            out_ctrl_chunks := uint64()}.
```

C: `struct sctp_assoc_stats`.

# `sctp_assocparams`
*since OTP 22.0* 

```erlang
-type sctp_assocparams() ::
          #{assoc_id := sctp_assoc_id(),
            asocmaxrxt := uint16(),
            number_peer_destinations := uint16(),
            peer_rwnd := uint32(),
            local_rwnd := uint32(),
            cookie_life := uint32()}.
```

C: `struct sctp_assocparams`

# `sctp_authkey`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_authkey() ::
          #{type := authkey,
            flags := uint16(),
            keynumber := uint16(),
            altkeynumber := uint16(),
            indication := uint32(),
            assoc_id := sctp_assoc_id()}.
```

When a receiver is using authentication, info about new keys and errors are
provided in this notification.

C: `struct sctp_authkey_event`

# `sctp_event_subscribe`
*since OTP 22.0* 

```erlang
-type sctp_event_subscribe() ::
          #{data_io := boolean(),
            association := boolean(),
            address := boolean(),
            send_failure := boolean(),
            peer_error := boolean(),
            shutdown := boolean(),
            partial_delivery := boolean(),
            adaptation_layer => boolean(),
            sender_dry => boolean(),
            stream_reset => boolean(),
            assoc_reset => boolean(),
            stream_change => boolean()}.
```

C: `struct sctp_event_subscribe`.

Not all fields are implemented on all platforms; unimplemented fields are
ignored, but implemented fields are mandatory. Note that the '\_event' suffixes
have been stripped from the C struct field names, for convenience.

# `sctp_initmsg`
*since OTP 22.0* 

```erlang
-type sctp_initmsg() ::
          #{num_ostreams := uint16(),
            max_instreams := uint16(),
            max_attempts := uint16(),
            max_init_timeo := uint16()}.
```

C: `struct sctp_initmsg`.

# `sctp_notification`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_notification() ::
          sctp_assoc_change() |
          sctp_paddr_change() |
          sctp_send_failed() |
          sctp_remote_error() |
          sctp_shutdown_event() |
          sctp_adapt_event() |
          sctp_pdapi_event() |
          sctp_authkey() |
          sctp_sender_dry() |
          sctp_stream_reset_event() |
          sctp_assoc_reset_event() |
          sctp_stream_change_event() |
          sctp_send_failed_event() |
          sctp_notification_generic().
```

All possible notification types. All of them has *at least* two fields:
'type' and 'flags' ('flags' are not allways used).

C: `union sctp_notification`

# `sctp_notification_generic`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_notification_generic() :: #{type := uint16(), flags := uint16()}.
```

C: `union sctp_notification`

This is intended as a fallback type for any notification
we have not yet implemented.

# `sctp_operation_error`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_operation_error() ::
          unknown | bad_sid | missing_parm | stale_cookie | no_resources | bad_addr | unrec_chunk |
          bad_mandparm | unrec_parm | no_usr_data | cookie_shut | restart_new_addrs | user_abort |
          delete_lastaddr | resource_shortage | delete_srcaddr | auth_err |
          pos_integer().
```

These error codes are (currently) defined in RFC 4960,
and named as *Operation Errors*.

# `sctp_paddr_change`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_paddr_change() ::
          #{type := peer_addr_change,
            flags := pos_integer(),
            addr := socket:sockaddr(),
            state := sctp_peer_addr_change_state(),
            error := pos_integer(),
            assoc_id := sctp_assoc_id()}.
```

A destination address on a multi-homed peer has encountered a change.

C: `struct sctp_paddr_change`

# `sctp_pap_flag`
*since OTP 22.0* 

```erlang
-type sctp_pap_flag() ::
          enable_heartbeats | disable_heartbeats | send_heartbeat_immediately | enable_pmtu_discovery |
          disable_pmtu_discovery | enable_sack | disable_sack | set_heartbeat_delay_to_zero |
          ipv6_flowlabel | dscp.
```

C: `enum  sctp_spp_flags`.

# `sctp_pap_flags`
*since OTP 22.0* 

```erlang
-type sctp_pap_flags() :: integer() | [sctp_pap_flag()].
```

There are three pairs of flags that cannot be both be set (maybe obviously) 
at the same time:
- `enable_heartbeats` and `disable_heartbeats`
- `enable_pmtu_discovery` and `disable_pmtu_discovery`
- `enable_sack` and `disable_sack`

# `sctp_pdapi_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_pdapi_event() ::
          #{type := partial_delivery_event,
            flags := uint16(),
            indication := uint16(),
            assoc_id := sctp_assoc_id(),
            stream => uint32(),
            seq => uint32()}.
```

A receiver is engaged in a partial delivery.

Note that not all fields are available on all platforms.
The *stream* and/or *seq* fields may not be present.

C: `struct sctp_pdapi_event`

# `sctp_peer_addr_change_state`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_peer_addr_change_state() ::
          addr_available | addr_unreachable | addr_removed | addr_added | addr_made_prim |
          addr_confirmed | addr_potentially_failed |
          integer().
```

# `sctp_peer_address_info`
*since OTP 22.0* 

```erlang
-type sctp_peer_address_info() ::
          #{assoc_id := sctp_assoc_id(),
            address := sockaddr(),
            state := sctp_peer_address_state(),
            cwnd := uint32(),
            srtt := uint32(),
            rto := uint32(),
            mtu := uint32()}.
```

C: `struct sctp_paddrinfo`.

# `sctp_peer_address_parameters`
*since OTP 22.0* 

```erlang
-type sctp_peer_address_parameters() ::
          #{assoc_id := sctp_assoc_id(),
            addr := sockaddr(),
            heatbeat_interval := uint32(),
            path_max_rxt := uint16(),
            path_mtu => uint32(),
            sack_delay => uint32(),
            flags => sctp_pap_flags(),
            ipv6_flowlabel => uint32(),
            dscp => uint8()}.
```

C: `struct sctp_paddrparams`.

# `sctp_peer_address_state`
*since OTP 22.0* 

```erlang
-type sctp_peer_address_state() :: inactive | potentially_failed | active | unconfirmed | unknown.
```

C: `enum sctp_spinfo_state`.

# `sctp_remote_error`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_remote_error() ::
          #{type := remote_error,
            flags := uint16(),
            error := sctp_operation_error(),
            assoc_id := sctp_assoc_id(),
            remote_causes := [integer()]}.
```

A remote peer may send an operational error message to its peer.

C: `struct sctp_remote_error`

# `sctp_rtoinfo`
*since OTP 22.0* 

```erlang
-type sctp_rtoinfo() ::
          #{assoc_id := sctp_assoc_id(), initial := uint32(), max := uint32(), min := uint32()}.
```

C: `struct sctp_rtoinfo`.

# `sctp_send_failed`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_send_failed() ::
          #{type := send_failed,
            flags := uint16(),
            error := uint32(),
            info := sctp_snd_rcv_info(),
            assoc_id := sctp_assoc_id(),
            data := binary()}.
```

SCTP cannot deliver a message.

C: `struct sctp_send_failed`

Deprecated.

# `sctp_send_failed_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_send_failed_event() ::
          #{type := send_failed_event,
            flags := [data_unsent | data_sent],
            error := uint32(),
            info := sctp_snd_info(),
            assoc_id := sctp_assoc_id(),
            data := binary()}.
```

SCTP cannot deliver a message.

C: `struct sctp_send_failed_event`

# `sctp_sender_dry`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_sender_dry() :: #{type := sender_dry, flags := uint16(), assoc_id := sctp_assoc_id()}.
```

The SCTP stack has no more user data to send or retransmit.

C: `struct sctp_sender_dry_event`

# `sctp_set_adaptation_layer_ind`
*since OTP 22.0* 

```erlang
-type sctp_set_adaptation_layer_ind() :: #{ind := uint32()}.
```

C: `struct sctp_setadaptation`.

# `sctp_set_peer_primary_address`
*since OTP 22.0* 

```erlang
-type sctp_set_peer_primary_address() :: #{assoc_id := sctp_assoc_id(), addr := sockaddr()}.
```

C: `struct sctp_setpeerprim`.

# `sctp_set_primary_address`
*since OTP 22.0* 

```erlang
-type sctp_set_primary_address() :: #{assoc_id := sctp_assoc_id(), addr := sockaddr()}.
```

C: `struct sctp_prim`.

# `sctp_setadaption`
*since OTP 22.0* 

```erlang
-type sctp_setadaption() :: #{adaption_ind := uint32()}.
```

C: `struct sctp_setadaption`

# `sctp_shutdown_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_shutdown_event() :: #{type := shutdown_event, flags := uint16(), assoc_id := sctp_assoc_id()}.
```

A peer has sent a SHUTDOWN.

C: `struct sctp_shutdown_event`

# `sctp_snd_info`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_snd_info() ::
          #{sid := uint16(),
            flags := uint16(),
            ppid := uint16(),
            context := uint32(),
            assic_id := sctp_assoc_id()}.
```

C: `struct sctp_sndinfo`

# `sctp_snd_rcv_info`
*since OTP 22.0* 

```erlang
-type sctp_snd_rcv_info() ::
          #{stream := uint16(),
            ssn => uint16(),
            flags => sctp_snd_rcv_info_flags(),
            ppid => uint32(),
            context => uint32(),
            time_to_live => uint32(),
            tsn => uint32(),
            cum_tsn => uint32(),
            assoc_id := sctp_assoc_id()}.
```

SCTP options for 
[`sendmsg()`](`socket:sendmsg/4`) and
SCTP header information about a received message through
[`recvmsg()`](`socket:recvmsg/5`).

When sending, only the *stream* and *assoc_id* fields needs to be
assigned. When receiving all values will be assigned.

C: `struct sctp_sndrcvinfo`

# `sctp_snd_rcv_info_flags`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_snd_rcv_info_flags() :: [unordered | addr_over | abort | eof].
```

# `sctp_status`
*since OTP 22.0* 

```erlang
-type sctp_status() ::
          #{assoc_id := sctp_assoc_id(),
            state := sctp_peer_address_state(),
            rwnd := uint32(),
            unacked_data := uint16(),
            pending_data := uint16(),
            in_streams := uint16(),
            out_streams := uint16(),
            fragmentation_point := uint32(),
            primary := sctp_peer_address_info()}.
```

C: `struct sctp_status`.

# `sctp_stream_change_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_stream_change_event() ::
          #{type := stream_change,
            flags := [denied | failed],
            assoc_id := sctp_assoc_id(),
            inbound_streams := uint16(),
            outbound_streams := uint16()}.
```

C: `struct sctp_stream_change_event`

# `sctp_stream_reset_event`
*not exported* *since OTP 22.0* 

```erlang
-type sctp_stream_reset_event() ::
          #{type := stream_reset,
            flags := [incoming_ssn | outgoing_ssn | denied | failed],
            assoc_id := sctp_assoc_id(),
            stream_list := [uint16()]}.
```

C: `struct sctp_stream_reset_event`

# `select_handle`
*since OTP 22.0* 

```erlang
-type select_handle() :: reference().
```

[Select operation](#asynchronous-calls) handle.

A `t:reference/0` that uniquely identifies the (select) operation,
contained in the returned `t:select_info/0`.

# `select_info`
*since OTP 22.0* 

```erlang
-type select_info() :: {select_info, SelectTag :: select_tag(), SelectHandle :: select_handle()}.
```

[Select operation](#asynchronous-calls) info.

Returned by an operation that requires the caller to wait for a
[`select` message](#async-messages) containing the
[`SelectHandle`](`t:select_handle/0`).

On `select` systems, if the option
[`{otp, select_read}`](`t:otp_socket_option/0`) is set,
[`{select_read, {select_info(), _}}`](`t:select_info/0`)
is returned instead of `{ok, _}` to indicate that a new
asynchronous receive operation has been initiated
and the caller should wait for a
[`select` message](#async-messages) containing the
[`SelectHandle`](`t:select_handle/0`).

# `select_tag`
*since OTP 22.0* 

```erlang
-type select_tag() ::
          accept | connect |
          recv | recvfrom | recvmsg | recvmmsg | send | sendv | sendto | sendmsg | sendmmsg | sendfile |
          {recv | recvfrom | recvmsg | recvmmsg | send | sendv | sendto | sendmsg | sendmmsg | sendfile,
           ContData :: term()}.
```

[Select operation](#asynchronous-calls) tag.

A tag that describes the (select) operation (= function name),
contained in the returned `t:select_info/0`.

# `sockaddr`
*since OTP 22.0* 

```erlang
-type sockaddr() ::
          sockaddr_in() |
          sockaddr_in6() |
          sockaddr_un() |
          sockaddr_ll() |
          sockaddr_dl() |
          sockaddr_unspec() |
          sockaddr_native().
```

# `sockaddr_dl`
*since OTP 22.0* 

```erlang
-type sockaddr_dl() ::
          #{family := link,
            index := non_neg_integer(),
            type := interface_type(),
            nlen := non_neg_integer(),
            alen := non_neg_integer(),
            slen := non_neg_integer(),
            data := binary()}.
```

C: `struct sockaddr_dl`

Link level address (PF_LINK) on BSD:s.

# `sockaddr_in6`
*since OTP 22.0* 

```erlang
-type sockaddr_in6() ::
          #{family := inet6,
            port := port_number(),
            addr := any | loopback | in6_addr(),
            flowinfo := in6_flow_info(),
            scope_id := in6_scope_id()}.
```

C: `struct sockaddr_in6`

[Domain `inet6`](`t:domain/0`) (IPv6) address.

# `sockaddr_in`
*since OTP 22.0* 

```erlang
-type sockaddr_in() ::
          #{family := inet, port := port_number(), addr := any | broadcast | loopback | in_addr()}.
```

C: `struct sockaddr_in`

[Domain `inet`](`t:domain/0`) (IPv4) address.

# `sockaddr_ll`
*since OTP 22.0* 

```erlang
-type sockaddr_ll() ::
          #{family := packet,
            protocol := non_neg_integer(),
            ifindex := integer(),
            pkttype := packet_type(),
            hatype := hatype(),
            addr := binary()}.
```

C: `struct sockaddr_ll`

[Domain `packet`](`t:domain/0`), [type `raw`](`t:type/0`)
(link level) address.

# `sockaddr_native`
*since OTP 22.0* 

```erlang
-type sockaddr_native() :: #{family := integer(), addr := binary()}.
```

C: `struct sockaddr`

In C, a `struct sockaddr` with the integer value of `sa_family`
in the `t:map/0` key `family`,
and the content of `sa_data` in the `t:map/0` key `addr`.

# `sockaddr_recv`
*since OTP 22.0* 

```erlang
-type sockaddr_recv() :: sockaddr() | binary().
```

# `sockaddr_un`
*since OTP 22.0* 

```erlang
-type sockaddr_un() :: #{family := local, path := binary() | string()}.
```

C: `struct sockaddr_un`.

A Unix Domain socket address, a.k.a local address (`AF_LOCAL`).

The `path` element will always be a `binary` when returned from this module.
When supplied to an API function in this module it may be a `t:string/0`, which
will be encoded into a binary according to the
[native file name encoding ](`file:native_name_encoding/0`)on the platform.

A terminating zero character will be appended before the address path is given
to the OS, and the terminating zero will be stripped before giving the address
path to the caller.

Linux's non-portable abstract socket address extension is handled by not doing
any terminating zero processing in either direction, if the first byte of the
address is zero.

# `sockaddr_unspec`
*since OTP 22.0* 

```erlang
-type sockaddr_unspec() :: #{family := unspec, addr := binary()}.
```

C: `struct sockaddr` of `AF_UNSPEC`

In C, a `struct sockaddr` with `sa_family = AF_UNSPEC`
and the content of `sa_data` in the `t:map/0` key `addr`.

# `socket`
*since OTP 22.0* 

```erlang
-type socket() :: {'$socket', socket_handle()}.
```

A socket, according to this module.

Created and returned by [`open/1,2,3,4`](`open/1`)
and [`accept/1,2`](`accept/1`).

# `socket_counters`
*since OTP 22.0* 

```erlang
-type socket_counters() ::
          #{read_byte := non_neg_integer(),
            read_fails := non_neg_integer(),
            read_pkg := non_neg_integer(),
            read_pkg_max := non_neg_integer(),
            read_tries := non_neg_integer(),
            read_waits := non_neg_integer(),
            write_byte := non_neg_integer(),
            write_fails := non_neg_integer(),
            write_pkg := non_neg_integer(),
            write_pkg_max := non_neg_integer(),
            write_tries := non_neg_integer(),
            write_waits := non_neg_integer(),
            sendfile => non_neg_integer(),
            sendfile_byte => non_neg_integer(),
            sendfile_fails => non_neg_integer(),
            sendfile_max => non_neg_integer(),
            sendfile_pkg => non_neg_integer(),
            sendfile_pkg_max => non_neg_integer(),
            sendfile_tries => non_neg_integer(),
            sendfile_waits => non_neg_integer(),
            acc_success := non_neg_integer(),
            acc_fails := non_neg_integer(),
            acc_tries := non_neg_integer(),
            acc_waits := non_neg_integer()}.
```

A `t:map/0` of `Name := Counter` associations.

# `socket_handle`
*since OTP 22.0* 

```erlang
-opaque socket_handle()
```

Opaque socket handle unique for the socket.

# `socket_info`
*since OTP 22.0* 

```erlang
-type socket_info() ::
          #{domain := domain() | integer(),
            type := type() | integer(),
            protocol := protocol() | integer(),
            owner := pid(),
            ctype := normal | fromfd | {fromfd, integer()},
            counters := socket_counters(),
            num_readers := non_neg_integer(),
            num_writers := non_neg_integer(),
            num_acceptors := non_neg_integer(),
            writable := boolean(),
            readable := boolean(),
            rstates := [atom()],
            wstates := [atom()]}.
```

# `socket_option`
*since OTP 22.0* 

```erlang
-type socket_option() ::
          {Level :: socket,
           Opt ::
               acceptconn | acceptfilter | bindtodevice | broadcast | bsp_state | busy_poll | debug |
               domain | dontroute | error | exclusiveaddruse | keepalive | linger | mark | maxdg |
               max_msg_size | oobinline | passcred | peek_off | peercred | priority | protocol |
               rcvbuf | rcvbufforce | rcvlowat | rcvtimeo | reuseaddr | reuseport | rxq_ovfl | setfib |
               sndbuf | sndbufforce | sndlowat | sndtimeo | timestamp | timestampns | type} |
          {Level :: ip,
           Opt ::
               add_membership | add_source_membership | block_source | dontfrag | drop_membership |
               drop_source_membership | freebind | hdrincl | minttl | msfilter | mtu | mtu_discover |
               multicast_all | multicast_if | multicast_loop | multicast_ttl | nodefrag | options |
               pktinfo | recvdstaddr | recverr | recvif | recvopts | recvorigdstaddr | recvtos |
               recvttl | retopts | router_alert | sndsrcaddr | tos | transparent | ttl | unblock_source} |
          {Level :: ipv6,
           Opt ::
               addrform | add_membership | authhdr | auth_level | checksum | drop_membership | dstopts |
               esp_trans_level | esp_network_level | faith | flowinfo | hopopts | ipcomp_level |
               join_group | leave_group | mtu | mtu_discover | multicast_hops | multicast_if |
               multicast_loop | portrange | pktoptions | recverr | recvhoplimit | hoplimit |
               recvpktinfo | pktinfo | recvtclass | router_alert | rthdr | tclass | unicast_hops |
               use_min_mtu | v6only} |
          {Level :: tcp,
           Opt ::
               congestion | cork | info | keepcnt | keepidle | keepintvl | maxseg | md5sig | nodelay |
               noopt | nopush | syncnt | user_timeout} |
          {Level :: udp, Opt :: cork} |
          {Level :: sctp,
           Opt ::
               adaption_layer | associnfo | auth_active_key | auth_asconf | auth_chunk | auth_key |
               auth_delete_key | autoclose | context | default_send_param | delayed_ack_time |
               disable_fragments | hmac_ident | events | explicit_eor | fragment_interleave |
               get_peer_addr_info | initmsg | i_want_mapped_v4_addr | local_auth_chunks | maxseg |
               maxburst | nodelay | partial_delivery_point | peer_addr_params | peer_auth_chunks |
               primary_addr | reset_streams | rtoinfo | set_peer_primary_addr | status |
               use_ext_recvinfo}.
```

Socket option.

Socket options of the form `{Level, Opt}` where the OS protocol `Level` =
`t:level/0` and `Opt` is a socket option on that protocol level.

The OS name for an options is, except where otherwise noted, the `Opt` atom, in
capitals, with prefix according to `t:level/0`.

> #### Note {: .info }
>
> The `IPv6` option `pktoptions` is a special (barf) case. It is intended for
> backward compatibility usage only.
>
> Do _not_ use this option.

> #### Note {: .info }
>
> See the OS documentation for every socket option.

An option below that has the value type `t:boolean/0` will translate the value
`false` to a C `int` with value `0`, and the value `true` to `!!0` (not (not
false)).

An option with value type `t:integer/0` will be translated to a C `int` that may
have a restricted range, for example byte: `0..255`. See the OS documentation.

The calls [`supports(options)`](`supports/1`),
[`supports(options, Level)`](`supports/1`) and
[`is_supported(options, {Level, Opt})` ](`is_supported/2`)can be used to find
out which socket options that are supported by the platform.

_Options for protocol level_ [_`socket`_:](`t:level/0`)

- **`{socket, acceptconn}`** - `Value = boolean()`

- **`{socket, bindtodevice}`** - `Value = string()`

- **`{socket, broadcast}`** - `Value = boolean()`

- **`{socket, debug}`** - `Value = integer()`

- **`{socket, domain}`** - `Value =` `t:domain/0`

  Only valid to _get_.

  The socket's protocol domain. Does _not_ work on for instance FreeBSD.

- **`{socket, dontroute}`** - `Value = boolean()`

- **`{socket, keepalive}`** - `Value = boolean()`

- **`{socket, linger}`** - `Value = abort |` `t:linger/0`

  The value `abort` is shorthand for `#{onoff => true, linger => 0}`, and only
  valid to _set_.

- **`{socket, oobinline}`** - `Value = boolean()`

- **`{socket, passcred}`** - `Value = boolean()`

- **`{socket, peek_off}`** - `Value = integer()`

  Currently disabled due to a possible infinite loop when calling
  [`recv/1-4`](`recv/1`) with [`peek`](`t:msg_flag/0`) in `Flags`.

- **`{socket, priority}`** - `Value = integer()`

- **`{socket, protocol}`** - `Value =` `t:protocol/0`

  Only valid to _get_.

  The socket's protocol. Does _not_ work on for instance Darwin.

- **`{socket, rcvbuf}`** - `Value = integer()`

- **`{socket, rcvlowat}`** - `Value = integer()`

- **`{socket, rcvtimeo}`** - `Value =` `t:timeval/0`

  This option is unsupported per default; OTP has to be explicitly built with
  the `--enable-esock-rcvsndtimeo` configure option for this to be available.

  Since our implementation uses non-blocking sockets, it is unknown if and how
  this option works, or even if it may cause malfunction. Therefore, we do not
  recommend setting this option.

  Instead, use the `Timeout` argument to, for instance, the `recv/3` function.

- **`{socket, reuseaddr}`** - `Value = boolean()`

- **`{socket, reuseport}`** - `Value = boolean()`

- **`{socket, sndbuf}`** - `Value = integer()`

- **`{socket, sndlowat}`** - `Value = integer()`

- **`{socket, sndtimeo}`** - `Value =` `t:timeval/0`

  This option is unsupported per default; OTP has to be explicitly built with
  the `--enable-esock-rcvsndtimeo` configure option for this to be available.

  Since our implementation uses non-blocking sockets, it is unknown if and how
  this option works, or even if it may cause malfunction. Therefore, we do not
  recommend setting this option.

  Instead, use the `Timeout` argument to, for instance, the `send/3` function.

- **`{socket, timestamp}`** - `Value = boolean()`

  Enable or disable the `SO_TIMESTAMP` socket option. When enabled, the socket
  will receive timestamps in control messages for received packets.

- **`{socket, timestampns}`** - `Value = boolean()`

  Enable or disable the `SO_TIMESTAMPNS` socket option. When enabled, the socket
  will receive nanosecond-precision timestamps in control messages for received
  packets.

- **`{socket, type}`** - `Value =` `t:type/0`

  Only valid to _get_.

  The socket's type.

_Options for protocol level_ [_`ip`_:](`t:level/0`)

- **`{ip, add_membership}`** - `Value =` `t:ip_mreq/0`

  Only valid to _set_.

- **`{ip, add_source_membership}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, block_source}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, drop_membership}`** - `Value =` `t:ip_mreq/0`

  Only valid to _set_.

- **`{ip, drop_source_membership}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

- **`{ip, freebind}`** - `Value = boolean()`

- **`{ip, hdrincl}`** - `Value = boolean()`

- **`{ip, minttl}`** - `Value = integer()`

- **`{ip, msfilter}`** - `Value =` `null |` `t:ip_msfilter/0`

  Only valid to _set_.

  The value `null` passes a `NULL` pointer and size `0` to the C library call.

- **`{ip, mtu}`** - `Value = integer()`

  Only valid to _get_.

- **`{ip, mtu_discover}`** - `Value =`
  [`ip_pmtudisc()` ](`t:ip_pmtudisc/0`)`| integer()`

  An `t:integer/0` value according to the platform's header files.

- **`{ip, multicast_all}`** - `Value = boolean()`

- **`{ip, multicast_if}`** - `Value =` `any |` `t:in_addr/0`

- **`{ip, multicast_loop}`** - `Value = boolean()`

- **`{ip, multicast_ttl}`** - `Value = integer()`

- **`{ip, nodefrag}`** - `Value = boolean()`

- **`{ip, pktinfo}`** - `Value = boolean()`

- **`{ip, recvdstaddr}`** - `Value = boolean()`

- **`{ip, recverr}`** - `Value = boolean()`

  Enable extended reliable error message passing.

  _Warning\!_ When this option is enabled, error messages may arrive on the
  socket's error queue, which should be read using the message flag
  [`errqueue`](`t:msg_flag/0`), and using [`recvmsg/1,2,3,4,5`](`recvmsg/1`) to
  get all error information in the [message's](`t:msg_recv/0`) `ctrl` field as a
  [control message](`t:cmsg_recv/0`) `#{level := ip, type := recverr}`.

  A working strategy should be to first poll the error queue using
  [`recvmsg/2,3,4` ](#recvmsg-timeout)with `Timeout =:= 0` and `Flags`
  containing `errqueue` (ignore the return value `{error, timeout}`) before
  reading the actual data to ensure that the error queue gets cleared. And read
  the data using one of the `nowait |`
  [`select_handle()` ](`t:select_handle/0`)recv functions:
  [`recv/3,4`](#recv-nowait),
  [`recvfrom/3,4`](#recvfrom-nowait) or
  [`recvmsg/3,4,5`](#recvmsg-nowait). Otherwise you might accidentally
  cause a busy loop in and out of 'select' for the socket.

- **`{ip, recvif}`** - `Value = boolean()`

- **`{ip, recvopts}`** - `Value = boolean()`

- **`{ip, recvorigdstaddr}`** - `Value = boolean()`

- **`{ip, recvtos}`** - `Value = boolean()`

- **`{ip, recvttl}`** - `Value = boolean()`

- **`{ip, retopts}`** - `Value = boolean()`

- **`{ip, router_alert}`** - `Value = integer()`

- **`{ip, sendsrcaddr}`** - `Value = boolean()`

- **`{ip, tos}`** - `Value =` [`iptos_value()` ](`t:iptos_value/0`) | [`ip_tos()` ](`t:ip_tos/0`)

  When sending/setting the value is according to `t:iptos_value/0`.
  When reading/getting the value is according to `t:ip_tos/0`.

- **`{ip, transparent}`** - `Value = boolean()`

- **`{ip, ttl}`** - `Value = integer()`

- **`{ip, unblock_source}`** - `Value =` `t:ip_mreq_source/0`

  Only valid to _set_.

_Options for protocol level_ [_`ipv6`_:](`t:level/0`)

- **`{ipv6, addrform}`** - `Value =` `t:domain/0`

  As far as we know the only valid value is `inet` and it is only allowed for an
  IPv6 socket that is connected and bound to an IPv4-mapped IPv6 address.

- **`{ipv6, add_membership}`** - `Value =` `t:ipv6_mreq/0`

  Only valid to _set_.

- **`{ipv6, authhdr}`** - `Value = boolean()`

- **`{ipv6, drop_membership}`** - `Value =` `t:ipv6_mreq/0`

  Only valid to _set_.

- **`{ipv6, dstopts}`** - `Value = boolean()`

- **`{ipv6, flowinfo}`** - `Value = boolean()`

- **`{ipv6, hoplimit}`** - `Value = boolean()`

- **`{ipv6, hopopts}`** - `Value = boolean()`

- **`{ipv6, mtu}`** - `Value = integer()`

- **`{ipv6, mtu_discover}`** - `Value =`
  [`ipv6_pmtudisc()` ](`t:ipv6_pmtudisc/0`)`| integer()`

  An `t:integer/0` value is according to the platform's header files.

- **`{ipv6, multicast_hops}`** - `Value =` `t:ipv6_hops/0`

- **`{ipv6, multicast_if}`** - `Value = integer()`

- **`{ipv6, multicast_loop}`** - `Value = boolean()`

- **`{ipv6, recverr}`** - `Value = boolean()`

  _Warning\!_ See the socket option `{ip, recverr}` regarding the socket's error
  queue. The same warning applies for this option.

- **`{ipv6, recvhoplimit}`** - `Value = boolean()`

- **`{ipv6, recvpktinfo}`** - `Value = boolean()`

- **`{ipv6, recvtclass}`** - `Value = boolean()`

- **`{ipv6, router_alert}`** - `Value = integer()`

- **`{ipv6, rthdr}`** - `Value = boolean()`

- **`{ipv6, tclass}`** - `Value = boolean()`

- **`{ipv6, unicast_hops}`** - `Value =` `t:ipv6_hops/0`

- **`{ipv6, v6only}`** - `Value = boolean()`

_Options for protocol level_ [_`sctp`_](`t:level/0`). See also RFC 6458.

- **`{sctp, associnfo}`** - `Value =` `t:sctp_assocparams/0`

- **`{sctp, autoclose}`** - `Value = integer()`

- **`{sctp, disable_fragments}`** - `Value = boolean()`

- **`{sctp, events}`** - `Value =` `t:sctp_event_subscribe/0`

  Only valid to _set_.

- **`{sctp, initmsg}`** - `Value =` `t:sctp_initmsg/0`

- **`{sctp, maxseg}`** - `Value = integer()`

- **`{sctp, nodelay}`** - `Value = boolean()`

- **`{sctp, rtoinfo}`** - `Value =` `t:sctp_rtoinfo/0`

- **`{sctp, get_peer_addr_info}`** - `Value =` `t:sctp_peer_address_info/0`

  Only valid for _get_.
  Also, requires an OptValue (containing `t:sctp_assoc_id/0` and
  `t:sockaddr/0`) specifying the peer. See [`getopt/3`](`getopt/3`)
  for more info.

- **`{sctp, status}`** - `Value =` `t:sctp_status/0`

  Only valid for _get_.
  Also, requires an OptValue (containing `t:sctp_assoc_id/0`)
  specifying the association. See [`getopt/3`](`getopt/3`) for more info.

_Options for protocol level_ [_`tcp`:_](`t:level/0`)

- **`{tcp, congestion}`** - `Value = string()`

- **`{tcp, cork}`** - `Value = boolean()`

- **`{tcp, keepcnt}`** - `Value = integer()`

- **`{tcp, keepidle}`** - `Value = integer()`

- **`{tcp, keepintvl}`** - `Value = integer()`

- **`{tcp, maxseg}`** - `Value = integer()`

- **`{tcp, nodelay}`** - `Value = boolean()`

- **`{tcp, nopush}`** - `Value = boolean()`

- **`{tcp, user_timeout}`** - `Value = non_neg_integer()`

_Options for protocol level_ [_`udp`:_](`t:level/0`)

- **`{udp, cork}`** - `Value = boolean()`

# `timespec`
*since OTP 22.0* 

```erlang
-type timespec() :: #{sec := integer(), nsec := integer()}.
```

C: `struct timespec`

Corresponds to the C `struct timespec`. The field `sec` holds seconds, and `nsec`
nanoseconds.

# `timeval`
*since OTP 22.0* 

```erlang
-type timeval() :: #{sec := integer(), usec := integer()}.
```

C: `struct timeval`

Corresponds to the C `struct timeval`. The field `sec` holds seconds, and `usec`
microseconds.

# `type`
*since OTP 22.0* 

```erlang
-type type() :: stream | dgram | raw | rdm | seqpacket.
```

Protocol type.

A lowercase `t:atom/0` representing a protocol _type_
on the platform named `SOCK_*`.  For example
`stream` corresponds to `SOCK_STREAM`.

# `uint8`
*not exported* *since OTP 22.0* 

```erlang
-type uint8() :: 0..255.
```

# `uint16`
*not exported* *since OTP 22.0* 

```erlang
-type uint16() :: 0..65535.
```

# `uint32`
*not exported* *since OTP 22.0* 

```erlang
-type uint32() :: 0..4294967295.
```

# `uint64`
*not exported* *since OTP 22.0* 

```erlang
-type uint64() :: 0..18446744073709551615.
```

# `accept`
*since OTP 22.0* 

```erlang
-spec accept(ListenSocket) -> Result
                when
                    Result :: {ok, Socket} | {error, Reason},
                    ListenSocket :: socket(),
                    Socket :: socket(),
                    Reason :: dynamic().
```

Equivalent to [`accept(ListenSocket, infinity)`](`accept/2`).

# `accept`
*since OTP 22.0* 

```erlang
-spec accept(ListenSocket, Timeout :: infinity) -> {ok, Socket} | {error, Reason}
                when
                    ListenSocket :: socket(),
                    Socket :: socket(),
                    Reason ::
                        posix() |
                        closed |
                        invalid() |
                        {create_accept_socket, posix()} |
                        {add_socket, posix()} |
                        {update_accept_context, posix()};
            (ListenSocket, Timeout :: non_neg_integer()) -> {ok, Socket} | {error, Reason}
                when
                    ListenSocket :: socket(),
                    Socket :: socket(),
                    Reason ::
                        posix() |
                        closed |
                        invalid() |
                        timeout |
                        {create_accept_socket, posix()} |
                        {add_socket, posix()} |
                        {update_accept_context, posix()};
            (ListenSocket, nowait | (Handle :: select_handle() | completion_handle())) ->
                {ok, Socket} | {select, SelectInfo} | {completion, CompletionInfo} | {error, Reason}
                when
                    ListenSocket :: socket(),
                    Socket :: socket(),
                    SelectInfo :: select_info(),
                    CompletionInfo :: completion_info(),
                    Reason ::
                        posix() |
                        closed |
                        invalid() |
                        {create_accept_socket, posix()} |
                        {add_accept_socket, posix()} |
                        {update_accept_context, posix()}.
```

Accept a connection on a listening socket.

`ListenSocket` has to be of a connection oriented type
(types `stream` or `seqpacket`, see `open/1`), and set to listen
(see `listen/1`).

[](){: #accept-infinity }

If the `Timeout` argument is `infinity`; accepts the first pending
incoming connection for the listen socket or wait for one to arrive,
and return the new connection socket.

[](){: #accept-timeout }

If the `Timeout` argument is a time-out value (`t:non_neg_integer/0`);
returns `{error, timeout}` if no connection has arrived after `Timeout`
milliseconds.

[](){: #accept-nowait }

If the `Handle` argument `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`, *(since OTP 24.0)*,
or on _Windows_, the equivalent `t:completion_handle/0` *(since OTP 26.0)*,
starts an [asynchronous call](#asynchronous-calls) like for `nowait`.

[](){: #accept-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`{ok, NewSocket}`** - Success; A connection has been accepted.
- **`{error, Reason}`** - An error occured and no connection was
  established.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

# `bind`
*since OTP 22.0* 

```erlang
-spec bind(Socket, Addr) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Addr :: sockaddr() | any | broadcast | loopback,
                  Reason :: posix() | closed | invalid().
```

Bind a name to a socket.

When a socket is created (with [`open`](`open/2`)), it has no address assigned
to it. `bind` assigns the address specified by the `Addr` argument.

The rules used for name binding vary between domains.

If you bind a socket to an address in for example the `inet` or `inet6`
address families, with an ephemeral port number (`0`), and want to know
which port that was chosen, you can find out using something like:
`{ok, #{port := Port}} =`[`socket:sockname(Socket)`](`sockname/1`)

# `bind`
*since OTP 29.0* 

```erlang
-spec bind(Socket, Addrs, Action) -> ok | {error, Reason}
              when
                  Socket :: socket(),
                  Addrs :: [sockaddr_in()] | [sockaddr_in() | sockaddr_in6()],
                  Action :: add | remove,
                  Reason :: posix() | closed.
```

Bind a list of socket addreses to a socket.

When a socket is created (with [`open`](`open/2`)), it has no address assigned
to it. This `bind` assigns the address specified by the `Addr` argument.

Calling this function is only valid if the socket is
`type`  = `seqpacket` and `protocol` = `sctp`.

If the domain is inet, then all addresses *must* be IPv4.
If the domain is inet6, the addresses can be *either* IPv4 or IPv6.

# `cancel`
*since OTP 22.1* 

```erlang
-spec cancel(Socket, SelectInfo | CompletionInfo) -> ok | {error, Reason}
                when
                    Socket :: socket(),
                    SelectInfo :: select_info(),
                    CompletionInfo :: completion_info(),
                    Reason :: closed | invalid().
```

Cancel an asynchronous call in progress.

Call this function to cancel an [asynchronous call](#asynchronous-calls)
in progress, that is; it returned a value containing
a `t:completion_info/0` or `t:select_info/0`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

If another process tries an operation of the same basic type
(`accept/1` | `send/2` | `recv/2`) it will be enqueued and notified
through a [`select` or `completion` message](#async-messages)
when the current operation and all enqueued before it has been completed.
If the current operation is canceled by this function it is treated
as a completed operation; the process first in queue is notified.

If [`SelectInfo`](`t:select_info/0`) `|`
[`CompletionInfo`](`t:completion_info/0`) does not match
an operation in progress for the calling process, this function returns
`{error, {invalid, SelectInfo | CompletionInfo}}`.

# `cancel_monitor`
*since OTP 24.0* 

```erlang
-spec cancel_monitor(MRef :: reference()) -> boolean().
```

Cancel a socket monitor.

If `MRef` is a reference that the calling process obtained by calling
`monitor/1`, this monitor is removed. If there is no such monitor
for the calling process (or MRef doesn't correspond to a monitor),
nothing happens.

The returned value is one of the following:

- **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
  message corresponding to this monitor has been delivered and will not be
  delivered.

- **`false`** - The monitor was not found so it couldn't be removed. This
  might be because the monitor has already triggered and there is
  a `'DOWN'` message from this monitor in the caller message queue.

# `close`
*since OTP 22.0* 

```erlang
-spec close(Socket) -> ok | {error, Reason}
               when Socket :: socket(), Reason :: posix() | closed | timeout.
```

Close a socket.

> #### Note {: .info }
>
> Note that for `Protocol = tcp` (see `open/3`), although
> TCP guarantees that when the other side sees the stream close
> all data that we sent before closing has been delivered,
> there is no way for us to know that the other side got all data
> and the stream close.  All kinds of network and OS issues
> may obliterate that.
>
> To get such a guarantee we need to implement an in-band acknowledge
> protocol on the connection, or we can use the [`shutdown`](`shutdown/2`)
> function to signal that no more data will be sent and then wait
> for the other end to close the socket.  Then we will see our read side
> getting a socket close.  In this way we implement a small
> acknowledge protocol using `shutdown/2`.  The other side cannot
> know that we ever saw the socket close, but in a client/server
> scenario that is often not relevant.

# `connect`
*since OTP 24.0* 

```erlang
-spec connect(Socket :: socket()) -> ok | {error, Reason} when Reason :: posix() | closed | invalid().
```

Finalize a [`connect/3`](#connect-nowait) operation.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

On `select` systems this function finalizes a connection setup
on a socket, after receiving a [`select` message](#async-messages)
`{'$socket',` [`Socket`](`t:socket/0`)`, select,
`[`SelectHandle`](`t:select_handle/0`)`}`,
and returns whether the connection setup was successful or not.

Instead of calling this function, for backwards compatibility,
it is allowed to call [`connect/2,3`](`connect/2`) again,
but that incurs more overhead since the connect address and
time-out argument are processed in vain.

The call that completes the connect operation, the second call,
cannot return a `select` return value.

# `connect`
*since OTP 22.0* 

```erlang
-spec connect(Socket :: socket(), SockAddr :: sockaddr() | [SockAddr :: term()]) ->
                 ok | {error, Reason :: dynamic()}.
```

Equivalent to
[`connect(Socket, SockAddr, infinity)`](#connect-infinity) or
[`connect(Socket, SockAddrs, undefined, infinity)`](#connect-infinity).

# `connect`
*since OTP 22.0* 

```erlang
-spec connect(Socket, SockAddrs, TimeoutOrHandle) -> {ok, AssocId} | {error, Reason}
                 when
                     Socket :: socket(),
                     SockAddrs :: [sockaddr_in()] | [sockaddr_in6()],
                     TimeoutOrHandle :: infinity | Timeout | nowait | Handle,
                     Timeout :: non_neg_integer(),
                     Handle :: select_handle(),
                     AssocId :: sctp_assoc_id(),
                     Reason :: posix() | closed | invalid() | already;
             (Socket, SockAddr, Timeout :: infinity) -> ok | {error, Reason}
                 when
                     Socket :: socket(),
                     SockAddr :: sockaddr(),
                     Reason ::
                         posix() |
                         closed |
                         invalid() |
                         already | not_bound |
                         {add_socket, posix()} |
                         {update_connect_context, posix()};
             (Socket, SockAddr, Timeout :: non_neg_integer()) -> ok | {error, Reason}
                 when
                     Socket :: socket(),
                     SockAddr :: sockaddr(),
                     Reason ::
                         posix() |
                         closed |
                         invalid() |
                         already | not_bound | timeout |
                         {add_socket, posix()} |
                         {update_connect_context, posix()};
             (Socket, SockAddr, nowait | Handle) ->
                 ok | {select, SelectInfo} | {completion, CompletionInfo} | {error, Reason}
                 when
                     Socket :: socket(),
                     SockAddr :: sockaddr(),
                     Handle :: select_handle() | completion_handle(),
                     SelectInfo :: select_info(),
                     CompletionInfo :: completion_info(),
                     Reason ::
                         posix() |
                         closed |
                         invalid() |
                         already | not_bound |
                         {add_socket, posix()} |
                         {update_connect_context, posix()}.
```

Connect the socket to the given address(s).

This function connects the socket to the address(s) specified
by the `SockAddr`|`SockAddrs` argument.

If a connection attempt is already in progress (by another process),
`{error, already}` is returned.

> #### Note {: .info }
>
> On _Windows_ the socket has to be [_bound_](`bind/2`).

[](){: #connect-infinity }

If the time-out argument (argument 3) is `infinity` it is
up to the OS implementation to decide when the connection
attempt failed and then what to return; probably `{error, etimedout}`.
The OS time-out may be very long.

[](){: #connect-timeout }

If the time-out argument (argument 3) is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if the connection hasn't been established within `Timeout` milliseconds.

> #### Note {: .info }
>
> Note that when this call has returned `{error, timeout}`
> the connection state of the socket is uncertain since the platform's
> network stack may complete the connection at any time,
> up to some platform specific time-out.
>
> Repeating a connection attempt towards the same address would be ok, but
> towards a different address could end up with a connection to either address.
>
> The safe play is to close the socket and start over.
>
> Also note that this applies to cancelling a `nowait` connect call
> described below.

[](){: #connect-nowait }

If the time-out argument (argument 2) is `nowait` *(since OTP 22.1)*,
start an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the time-out argument (argument 2) is a `Handle ::` `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`Handle ::` `t:completion_handle/0` *(since OTP 26.0)*,
start an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a [`select` message](#async-messages); call `connect/1`
to complete the operation.

If cancelling the operation with `cancel/2` see the note above
about [connection time-out](#connect-timeout).

[](){: #connect-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`ok`** - Complete success; A connection has been established.
- **`{error, Reason}`** - An error occured and no connection was
  established.

> #### Note {: .info }
>
> Note that calling with a list of socket addresses only works for
> SCTP sockets (type = `seqpacket`). And that the family of *all*
> addresses in the list is either IPv4 (`inet`) or IPv6 (`inet6`).

# `getopt`
*since OTP 24.0* 

```erlang
-spec getopt(socket(), SocketOption :: {Level :: otp, Opt :: otp_socket_option()}) ->
                {ok, Value :: term()} | {error, invalid() | closed};
            (socket(), SocketOption :: socket_option()) ->
                {ok, Value :: term()} | {error, posix() | invalid() | closed}.
```

Get the value of a socket option.

Gets the value of an OS protocol level socket option, or from
the `otp` pseudo protocol level, which is this module's
implementation level above the OS protocol levels.

See the type [otp_socket_option() ](`t:otp_socket_option/0`)
for a description of the `otp` protocol level.

See the type `t:socket_option/0` for which OS protocol level options
that this implementation knows about, how they are related to OS option names,
and if there are known peculiarities with any of them.

What options that are valid depends on the OS, and on the kind of socket
(`t:domain/0`,`t:type/0` and `t:protocol/0`).  See the type
`t:socket_option()` and the
[socket options ](socket_usage.md#socket_options) chapter
in the User's Guide for more info.

> #### Note {: .info }
>
> Not all options are valid, nor possible to get, on all platforms. That is,
> even if this `socket` implementation  support an option; it doesn't mean
> that the underlying OS does.

# `getopt`
*since OTP 22.0* 

```erlang
-spec getopt(Socket :: socket(),
             SocketOption :: {sctp, get_peer_addr_info},
             OptValue :: #{assoc_id := sctp_assoc_id(), addr := sockaddr()}) ->
                {ok, Value :: sctp_peer_address_info()} | {error, posix() | invalid() | closed};
            (Socket :: socket(),
             SocketOption :: {sctp, status},
             OptValue :: #{assoc_id := sctp_assoc_id()}) ->
                {ok, Value :: sctp_status()} | {error, posix() | invalid() | closed};
            (Socket :: socket(), Level :: level(), Opt :: term()) ->
                {ok, Value :: term()} | {error, posix() | invalid() | closed}.
```

Get a socket option, with a specifier (extra input) `OptValue`.

This function is used when the option takes an input 'value' argument.
We only support this for a limited set of options:
`{sctp, get_peer_addr_info}` and `{sctp, status}`.

Some uses of this function is for _backwards compatibility reasons_.

For instance, `getopt(Socket, Level, Opt)` is 
equivalent to [`getopt(Socket, {Level, Opt})`](`getopt/2`),
or as a special case if
`Opt = {NativeOpt :: `[`integer/0`](`t:integer/0`)`, ValueSpec}`
equivalent to
[`getopt_native(Socket, {Level, NativeOpt}, ValueSpec)`](`getopt_native/3`).

Use `getopt/2` or `getopt_native/3` instead to handle
the option level and name as a single term, and to make the
difference between known options and native options clear.

# `getopt_native`
*since OTP 24.0* 

```erlang
-spec getopt_native(socket(),
                    SocketOption ::
                        socket_option() |
                        {Level :: level() | (NativeLevel :: integer()), NativeOpt :: integer()},
                    ValueType :: integer) ->
                       {ok, Value :: integer()} | {error, posix() | invalid() | closed};
                   (socket(),
                    SocketOption ::
                        socket_option() |
                        {Level :: level() | (NativeLevel :: integer()), NativeOpt :: integer()},
                    ValueType :: boolean) ->
                       {ok, Value :: boolean()} | {error, posix() | invalid() | closed};
                   (socket(),
                    SocketOption ::
                        socket_option() |
                        {Level :: level() | (NativeLevel :: integer()), NativeOpt :: integer()},
                    ValueSize :: non_neg_integer()) ->
                       {ok, Value :: binary()} | {error, posix() | invalid() | closed};
                   (socket(),
                    SocketOption ::
                        socket_option() |
                        {Level :: level() | (NativeLevel :: integer()), NativeOpt :: integer()},
                    ValueSpec :: binary()) ->
                       {ok, Value :: binary()} | {error, posix() | invalid() | closed}.
```

Get a "native" socket option.

Gets a socket option that may be unknown to our implementation, or that has a
type not compatible with our implementation, that is; in "native mode".

The socket option may be specified with an ordinary
[`socket_option()` ](`t:socket_option/0`) tuple, with a known
[`Level = level()` ](`t:level/0`) and an integer `NativeOpt`,
or with both an integer `NativeLevel` and `NativeOpt`.

How to decode the option value has to be specified either with `ValueType`,
by specifying the `ValueSize` for a `t:binary/0` that will contain the fetched
option value, or by specifying a `t:binary/0` `ValueSpec` that will be copied
to a buffer for the `getsockopt()` call to write the value in which will be
returned as a new `t:binary/0`.

If `ValueType` is `integer` a `C` type `(int)` will be fetched, if it is
`boolean` a `C` type `(int)` will be fetched and converted into a `t:boolean/0`
according to the `C` implementation's notion about true and false.

If an option is valid depends both on the platform and on
what kind of socket it is (`t:domain/0`, `t:type/0` and `t:protocol/0`).

The integer values for `NativeLevel` and `NativeOpt` as well as the `Value`
encoding has to be deduced from the header files for the running system.

# `i`
*since OTP 24.1* 

```erlang
-spec i() -> ok.
```

Print information to the erlang shell in table format
for all sockets.

The information printed for each socket is specified by the default set
of `t:info_keys/0` (all keys).

The sockets that are printed are all sockets created by this
`socket` module's implementation.

# `i`
*since OTP 24.1* 

```erlang
-spec i(InfoKeys :: info_keys()) -> ok;
       (Domain :: inet | inet6 | local) -> ok;
       (Proto :: sctp | tcp | udp) -> ok;
       (Type :: dgram | seqpacket | stream) -> ok.
```

Print information to the erlang shell in table format
for all sockets.

If the argument is a list of `t:info_keys/0`, print the specified
information for all sockets.  See `i/0`.

Otherwise the same as `i/2` with the same first argument
and the default information (see `i/0`).

# `i`
*since OTP 24.1* 

```erlang
-spec i(Domain :: inet | inet6 | local, InfoKeys) -> ok when InfoKeys :: info_keys();
       (Proto :: sctp | tcp | udp, InfoKeys) -> ok when InfoKeys :: info_keys();
       (Type :: dgram | seqpacket | stream, InfoKeys) -> ok when InfoKeys :: info_keys().
```

Print information to the erlang shell in table format
for a selection of sockets.

The argument [`InfoKeys`](`t:info_keys/0`) specifies which information
is printed for each socket.

If the first argument is `Domain` print information for
all sockets of that specific `t:domain/0`.

If the first argument is `Proto` print information for
all sockets of that specific `t:protocol/0`.

If the first argument is `Type` print information for
all sockets of that specific `t:type/0`.

# `info`
*since OTP 24.0* 

```erlang
-spec info() -> info().
```

Get miscellaneous information about this `socket` library.

The function returns a map with each information item as a key-value pair.

> #### Note {: .info }
>
> In order to ensure data integrity, mutexes are taken when needed.
> So, don't call this function often.

# `info`
*since OTP 22.1* 

```erlang
-spec info(Socket) -> socket_info() when Socket :: socket().
```

Get miscellaneous info about a socket.

The function returns a map with each information item as a key-value pair
reflecting the "current" state of the socket (such as counter values).

> #### Note {: .info }
>
> In order to ensure data integrity, mutexes are taken when needed.
> So, don't call this function often.

# `ioctl`
*since OTP 24.2* 

```erlang
-spec ioctl(Socket, GetRequest :: gifconf) ->
               {ok, IFConf :: [#{name := string, addr := sockaddr()}]} | {error, Reason}
               when Socket :: socket(), Reason :: posix() | closed;
           (Socket, GetRequest :: nread | nwrite | nspace) ->
               {ok, NumBytes :: non_neg_integer()} | {error, Reason}
               when Socket :: socket(), Reason :: posix() | closed;
           (Socket, GetRequest :: atmark) -> {ok, Available :: boolean()} | {error, Reason}
               when Socket :: socket(), Reason :: posix() | closed;
           (Socket, GetRequest :: tcp_info) -> {ok, Info :: map()} | {error, Reason}
               when Socket :: socket(), Reason :: posix() | closed.
```

Set socket (device) parameters.

This function retrieves a specific parameter, according to
the `GetRequest` argument.

- **`gifconf`** - Get a list of interface (transport layer) addresses.

  Result; a list of `t:map/0`s, one for each interface,
  with its name and address.

- **`nread`** - Get the number of bytes immediately available for reading
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`nwrite`** - Get the number of bytes in the send queue
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`nspace`** - Get the free space in the send queue
  *(since OTP 26.1)*.

  Result; the number of bytes, `t:integer/0`.

- **`atmark`** - Test if there is OOB (out-of-bound) data waiting to be read
  *(since OTP 26.1)*.

  Result; a `t:boolean/0`.

- **`tcp_info`** - Get miscellaneous TCP related information for a
  _connected_ socket *(since OTP 26.1)*.

  Result; a `t:map/0` with information items as key-value pairs.

> #### Note {: .info }
>
> Not all requests are supported by all platforms.
> To see if a ioctl request is supported on the current platform:
>
> ```erlang
> 	    Request = nread,
> 	    true = socket:is_supported(ioctl_requests, Request),
> 	    :
> ```

# `ioctl`
*since OTP 24.2* 

```erlang
-spec ioctl(Socket, GetRequest, NameOrIndex) -> {ok, Result} | {error, Reason}
               when
                   Socket :: socket(),
                   GetRequest ::
                       gifname | gifindex | gifaddr | gifdstaddr | gifbrdaddr | gifnetmask | gifhwaddr |
                       genaddr | gifmtu | giftxqlen | gifflags | tcp_info,
                   NameOrIndex :: string() | integer(),
                   Result :: dynamic(),
                   Reason :: posix() | closed;
           (Socket, SetRequest, Value) -> ok | {error, Reason}
               when
                   Socket :: socket(),
                   SetRequest :: rcvall,
                   Value :: off | on | iplevel,
                   Reason :: posix() | closed;
           (Socket, SetRequest, Value) -> ok | {error, Reason}
               when
                   Socket :: socket(),
                   SetRequest :: rcvall_igmpmcast | rcvall_mcast,
                   Value :: off | on,
                   Reason :: posix() | closed.
```

Get or set socket (device) parameters.

[](){: #ioctl-get }

This function retrieves a specific parameter, according to
one of the following `GetRequest` arguments. The third argument is
the (lookup) "key", identifying the interface, for most requests
the name of the interface as a `t:string/0`.
Also, see the note above.

- **`gifname`** - Get the name of the interface with the specified index
  (`t:integer/0`).

  Result; the name of the interface, `t:string/0`.

- **`gifindex`** - Get the index of the interface with the specified name.

  Result; the interface index, `t:integer/0`.

- **`gifaddr`** - Get the address of the interface with the specified name.

  Result; the address of the interface, `t:sockaddr/0`.

- **`gifdstaddr`** - Get the destination address of the point-to-point
  interface with the specified name.

  Result; the destination address of the interface, `t:sockaddr/0`.

- **`gifbrdaddr`** - Get the broadcast address of the interface with the
  specified name.

  Result; broadcast address of the interface, `t:sockaddr/0`.

- **`gifnetmask`** - Get the network mask of the interface with
  the specified name.

  Result; the network mask of the interface, `t:sockaddr/0`.

- **`gifhwaddr` | `genaddr`** - Get the hardware address for the interface with the
  specified name.

  Result; the hardware address of the interface, `t:sockaddr/0` | `t:binary/0`.
  The family field contains the 'ARPHRD' device type (or an integer).

- **`gifmtu`** - Get the MTU (Maximum Transfer Unit) for the interface with the
  specified name.

  Result; MTU of the interface, `t:integer/0`.

- **`giftxqlen`** - Get the transmit queue length of the interface with the
  specified name.

  Result; transmit queue length of the interface, `t:integer/0`.

- **`gifflags`** - Get the active flag word of the interface
  with the specified name.

  Result; the active flag word of the interface, is a list of
  `t:ioctl_device_flag/0` `|` `t:integer( )`.

[](){: #ioctl-set }

With the following `SetRequest` argument this function sets
the `Value` for the request parameter *(since OTP 26.1)*.

- **`rcvall`** - Enables (or disables) a socket to receive all IPv4 or IPv6
  packages passing through a network interface.

  The `Socket` has to be one of:

  - **An IPv4 socket** - Created with the address
    [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
    and [protocol `ip`](`t:protocol/0`).

  - **An IPv6 socket** - Created with the address
    [domain `inet6`](`t:domain/0`), socket [type `raw`](`t:type/0`)
    and [protocol `ipv6`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local IPv4 or IPv6 interface
  (`any` isn't allowed).

  Setting this IOCTL requires elevated privileges.

With the following `SetRequest` arguments this function sets
the `Value` for the request parameter *(since OTP 26.1)*.

- **`rcvall_igmpmcall`** - Enables (or disables) a socket to receive IGMP
  multicast IP traffic, _without_ receiving any other IP traffic.

  The socket has to be created with the address
  [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
  and [protocol `igmp`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local interface
  (`any` isn't allowed).

  The receive buffer must be sufficiently large.

  Setting this IOCTL requires elevated privileges.

- **`rcvall_mcall`** - Enables (or disables) a socket to receive all multicast
  IP traffic (as in; all IP packets destined for IP addresses in the range
  224.0.0.0 to 239.255.255.255).

  The socket has to be created with the address
  [domain `inet`](`t:domain/0`), socket [type `raw`](`t:type/0`)
  and [protocol `udp`](`t:protocol/0`).

  The socket must also be bound to an (explicit) local interface
  (`any` isn't allowed), And bound to port `0`.

  The receive buffer must be sufficiently large.

  Setting this IOCTL requires elevated privileges.

# `ioctl`
*since OTP 24.2* 

```erlang
-spec ioctl(Socket, SetRequest, Name, Value) -> ok | {error, Reason}
               when
                   Socket :: socket(),
                   SetRequest ::
                       sifflags | sifaddr | sifdstaddr | sifbrdaddr | sifnetmask | sifhwaddr | sifmtu |
                       siftxqlen,
                   Name :: string(),
                   Value :: dynamic(),
                   Reason :: posix() | closed.
```

Set socket (device) parameters.

This function sets a specific parameter, according to the `SetRequest`
argument. The `Name` argument is the name of the interface,
and the `Value` argument is the value to set.

These operations require elevated privileges.

- **`sifflags`** - Set the the active flag word, `#{Flag => boolean()}`, of the
  interface with the specified name.

  Each flag to be changed should be added to the value `t:map/0`,
  with the value `true` if the `Flag` should be set and `false`
  if the flag should be cleared.

- **`sifaddr`** - Set the address, `t:sockaddr/0`, of the interface with the
  specified name.

- **`sifdstaddr`** - Set the destination address, `t:sockaddr/0`, of a
  point-to-point interface with the specified name.

- **`sifbrdaddr`** - Set the broadcast address, `t:sockaddr/0`,
of the interface with the specified name.

- **`sifnetmask`** - Set the network mask, `t:sockaddr/0`, of the interface
  with the specified name.

- **`sifhwaddr`** - Set the hardware address, `t:sockaddr/0`,
of the interface with the specified name.

- **`sifmtu`** - Set the MTU (Maximum Transfer Unit), `t:integer/0`,
  for the interface with the specified name.

- **`siftxqlen`** - Set the transmit queue length, `t:integer/0`,
  of the interface with the specified name.

# `is_supported`
*since OTP 23.0* 

```erlang
-spec is_supported(Key1 :: term()) -> boolean().
```

Check if a socket feature is supported.

Returns `true` if `supports/0` has a `{Key1, true}` tuple
or a `{Key1, list()}` tuple in its returned list,
otherwise `false` (also for unknown keys).

Example:
``` erlang
true = socket:is_supported(local),
```

# `is_supported`
*since OTP 23.0* 

```erlang
-spec is_supported(Key1 :: term(), Key2 :: term()) -> boolean().
```

Check if a socket feature is supported.

Returns `true` if [`supports(Key1)`](`supports/1`) has a `{Key2, true}` tuple
in its returned list, otherwise `false` (also for unknown keys).

Example:
``` erlang
true = socket:is_supported(msg_flags, errqueue),
```

# `listen`
*since OTP 22.0* 

```erlang
-spec listen(Socket :: socket()) -> ok | {error, Reason :: posix() | closed}.
```

Make a socket listen for connections.

Equivalent to [`listen(Socket, Backlog)`](`listen/2`) with a default
value for `Backlog` (currently `5`).

# `listen`
*since OTP 22.0* 

```erlang
-spec listen(Socket, IsServer) -> ok | {error, Reason}
                when Socket :: socket(), IsServer :: boolean(), Reason :: posix() | closed;
            (Socket, Backlog) -> ok | {error, Reason}
                when Socket :: socket(), Backlog :: pos_integer(), Reason :: posix() | closed.
```

Make a socket listen for connections.

The `IsServer` clauses are intended to be used for SCTP sockets.
The `Backlog` argument states the length of the queue for
incoming not yet accepted connections.
Exactly how that number is interpreted is up to the OS'
protocol stack, but the resulting effective queue length
will most probably be perceived as at least that long.

> #### Note {: .info }
>
> On _Windows_ the socket has to be _bound_.

# `monitor`
*since OTP 24.0* 

```erlang
-spec monitor(Socket :: socket()) -> MonitorRef :: reference().
```

Start a socket monitor.

If the `Socket` doesn't exist or when later the monitor is triggered,
a `'DOWN'` message is sent to the process that called `monitor/1`
with the following pattern:

``` erlang
	    {'DOWN', MonitorRef, socket, Socket, Info}
```
`Info` is the termination reason of the socket or `nosock` if
`Socket` did not exist when the monitor was started.

Making several calls to `socket:monitor/1` for the same `Socket` is not an
error; each call creates an independent monitor instance.

# `number_of`
*since OTP 22.3* 

```erlang
-spec number_of() -> non_neg_integer().
```

Return the number of active sockets.

# `open`
*since OTP 23.0* 

```erlang
-spec open(FD :: integer()) -> dynamic().
```

Equivalent to [`open(FD, #{})`](`open/2`).

# `open`
*since OTP 22.0* 

```erlang
-spec open(FD, Opts) -> {ok, Socket} | {error, Reason}
              when
                  FD :: integer(),
                  Opts ::
                      #{domain => domain() | integer(),
                        type => type() | integer(),
                        protocol => default | protocol() | integer(),
                        dup => boolean(),
                        debug => boolean(),
                        use_registry => boolean()},
                  Socket :: socket(),
                  Reason :: posix() | domain | type | protocol;
          (Domain, Type) -> {ok, Socket} | {error, Reason}
              when
                  Domain :: domain(),
                  Type :: type() | integer(),
                  Socket :: socket(),
                  Reason :: posix() | protocol.
```

Create a socket.

#### With arguments `Domain` and `Type`

Equivalent to [`open(Domain, Type, default, #{})`](`open/4`).

#### With arguments `FD` and `Opts` *(since OTP 23.0)*

Creates an endpoint for communication (socket) based on
an already existing file descriptor that must be a socket.
This function attempts to retrieve the file descriptor's
`domain`, `type` and `protocol` from the system.
This is however not possible on all platforms;
in that case they should be specified in `Opts`.

The `Opts` argument can provide extra information:

- **`domain`** - The file descriptor's communication domain. See also

  [`open/2,3,4`](`open/3`).

- **`type`** - The file descriptor's socket type.

  See also [`open/2,3,4`](`open/3`).

- **`protocol`** - The file descriptor's protocol. The atom `default` is
  equivalent to the integer protocol number `0` which means the default
  protocol for a given domain and type.

  If the protocol can not be retrieved from the platform for the socket, and
  `protocol` is not specified, the default protocol is used, which may
  or may not be correct.

  See also [`open/2,3,4`](`open/3`).

- **`dup`** - If `false` don't duplicate the provided file descriptor.

  Defaults to `true`; do duplicate the file descriptor.

- **`debug`** - If `true` enable socket debug logging.

  Defaults to `false`; don't enable socket debug logging.

- **`use_registry`** - Enable or disable use of the socket registry
  for this socket. This overrides the global setting.

  Defaults to the global setting, see `use_registry/1`.

> #### Note {: .info }
>
> This function should be used with care\!
>
> On some platforms it is _necessary_ to provide `domain`, `type` and `protocol`
> since they cannot be retrieved from the platform.
>
> On some platforms it is not easy to get hold of a file descriptor
> to use in this function.

# `open`
*since OTP 22.0* 

```erlang
-spec open(Domain, Type, Opts | Protocol) -> {ok, Socket} | {error, Reason}
              when
                  Domain :: domain() | integer(),
                  Type :: type() | integer(),
                  Opts :: map(),
                  Protocol :: default | protocol() | integer(),
                  Socket :: socket(),
                  Reason :: posix() | protocol.
```

Create a socket.

#### With arguments `Domain`, `Type` and `Protocol`

Equivalent to [`open(Domain, Type, Protocol, #{})`](`open/4`).

#### With arguments `Domain`, `Type` and `Opts` *(since OTP 24.0)*

Equivalent to [`open(Domain, Type, default, #{})`](`open/4`).

# `open`
*since OTP 22.0* 

```erlang
-spec open(Domain, Type, Protocol, Opts) -> {ok, Socket} | {error, Reason}
              when
                  Domain :: domain() | integer(),
                  Type :: type() | integer(),
                  Protocol :: default | protocol() | integer(),
                  Opts :: #{netns => string(), debug => boolean(), use_registry => boolean()},
                  Socket :: socket(),
                  Reason :: posix() | protocol.
```

Create a socket.

Creates an endpoint for communication (socket).

`Domain` and `Type` may be `t:integer/0`s, as defined in the platform's
header files. The same goes for `Protocol` as defined in the platform's
`services(5)` database. See also the OS man page for the library call
`socket(2)`.

> #### Note {: .info }
>
> For some combinations of `Domain` and `Type` the platform has got
> a default protocol that can be selected with `Protocol = default`,
> and the platform may allow or require selecting the default protocol,
> or a specific protocol.
>
> Examples:
>
> - **`socket:open(inet, stream, tcp)`** - It is common that for
>   protocol domain and type `inet,stream` it is allowed to select
>   the `tcp` protocol although that mostly is the default.
> - **`socket:open(local, dgram)`** - It is common that for
>   the protocol domain `local` it is mandatory to not select a protocol,
>   that is; to select the default protocol.

The `Opts` argument is intended for "other" options.
The supported option(s) are described below:

- **`netns: string()`** - Used to set the network namespace during the open
  call. Only supported on Linux.

- **`debug: boolean()`** - Enable or disable debug logging.

  Defaults to `false`.

- **`use_registry: boolean()`** - Enable or disable use of the socket registry
  for this socket. This overrides the global value.

  Defaults to the global value, see `use_registry/1`.

# `peeloff`
*since OTP 29.0* 

```erlang
-spec peeloff(Socket, AssocId) -> {ok, NewSock} | {ok, NewSock, InheritErrs} | {error, Reason}
                 when
                     Socket :: socket(),
                     AssocId :: sctp_assoc_id(),
                     NewSock :: socket(),
                     InheritErrs :: [{SockOpt, get | set, Reason}],
                     SockOpt :: socket_option(),
                     Reason :: posix() | closed.
```

Branch off an association into a separate socket.

Equivalent to [`peeloff(Socket, AssocId, [])`](`peeloff/3`)

# `peeloff`
*since OTP 29.0* 

```erlang
-spec peeloff(Socket, AssocId, InheritOpts) ->
                 {ok, NewSock} | {ok, NewSock, InheritErrs} | {error, Reason}
                 when
                     Socket :: socket(),
                     AssocId :: sctp_assoc_id(),
                     InheritOpts :: [socket_option()],
                     NewSock :: socket(),
                     InheritErrs :: [{SockOpt, get | set, Reason}],
                     SockOpt :: socket_option(),
                     Reason :: posix() | closed.
```

Branch off an association into a separate socket.

Create a new one-to-one socket from an existing one-to-many socket.
The specified `InheritOpts` will be inherited by the new socket (get from
existing socket and then set on the new socket).

If the peeloff operation is successful but some (or all) of the `InheritOpts`
failed to transfer to the new socket, `{'ok', NewSock, InheritErrs}` is
returned. It is then up to the caller to decide if this is acceptable.

# `peername`
*since OTP 22.0* 

```erlang
-spec peername(Socket :: socket()) -> {ok, SockAddr} | {error, Reason}
                  when SockAddr :: sockaddr_recv(), Reason :: posix() | closed.
```

Return the remote address of a socket.

Returns the address of the connected peer, that is,
the remote end of the socket.

# `peernames`
*since OTP 29.0* 

```erlang
-spec peernames(Socket :: socket(), AssocId :: sctp_assoc_id()) -> {ok, [SockAddr]} | {error, Reason}
                   when SockAddr :: sockaddr_recv(), Reason :: posix() | closed.
```

Return the remote (peer) address(s) of an association of a socket.

If the socket is IPv4 then all returned addresess will be IPv4.
If the socket is IPv6, then the returned addresess can be a mix of
IPv4 and IPv6.

For a one-to-many socket, AssocId specifies the association.
For a one-to-one socket, AssocId is ignored.

The behaviour if AssocId is 0 (zero), is undefined for one-to-many sockets.

# `recv`
*since OTP 22.0* 

```erlang
-spec recv(Socket :: socket()) -> dynamic().
```

Equivalent to [`recv(Socket, 0, [], infinity)`](`recv/4`).

# `recv`
*since OTP 22.0* 

```erlang
-spec recv(Socket :: socket(), Flags :: [msg_flag() | integer()]) -> dynamic();
          (Socket :: socket(), Length :: non_neg_integer()) -> dynamic().
```

Receive data on a connected socket.

With argument `Length`; equivalent to
[`recv(Socket, Length, [], infinity)`](`recv/4`).

With argument `Flags`; equivalent to
[`recv(Socket, 0, Flags, infinity)`](`recv/4`) *(since OTP 24.0)*.

# `recv`
*since OTP 22.0* 

```erlang
-spec recv(Socket, Flags, TimeoutOrHandle) -> dynamic()
              when
                  Socket :: socket(),
                  Flags :: [msg_flag() | integer()],
                  TimeoutOrHandle :: timeout() | nowait | select_handle() | completion_handle();
          (Socket :: socket(), Length :: non_neg_integer(), Flags :: [msg_flag() | integer()]) ->
              dynamic();
          (Socket :: socket(), Length :: non_neg_integer(), TimeoutOrHandle) -> dynamic()
              when TimeoutOrHandle :: timeout() | nowait | select_handle() | completion_handle().
```

Receive data on a connected socket.

With arguments `Length` and `Flags`; equivalent to
[`recv(Socket, Length, Flags, infinity)`](`recv/4`).

With arguments `Length` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, Length, [], TimeoutOrHandle)`](`recv/4`).
`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.
`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.

With arguments `Flags` and `TimeoutOrHandle`; equivalent to
[`recv(Socket, 0, Flags, TimeoutOrHandle)`](`recv/4`)
*(since OTP 24.0)*.

# `recv`
*since OTP 22.0* 

```erlang
-spec recv(Socket, Length, Flags, Timeout :: infinity) ->
              {ok, Data} | {error, Reason} | {error, {Reason, Data}}
              when
                  Socket :: socket(),
                  Length :: non_neg_integer(),
                  Flags :: [msg_flag() | integer()],
                  Data :: binary(),
                  Reason :: posix() | closed | invalid();
          (Socket, Length, Flags, Timeout :: non_neg_integer()) ->
              {ok, Data} | {error, Reason} | {error, {Reason, Data}}
              when
                  Socket :: socket(),
                  Length :: non_neg_integer(),
                  Flags :: [msg_flag() | integer()],
                  Data :: binary(),
                  Reason :: posix() | closed | invalid() | timeout;
          (Socket, Length, Flags, nowait | Handle) ->
              {ok, Data} |
              {select, SelectInfo} |
              {select, {SelectInfo, Data}} |
              {select_read, {SelectInfo, Data}} |
              {completion, CompletionInfo} |
              {error, Reason}
              when
                  Socket :: socket(),
                  Length :: non_neg_integer(),
                  Flags :: [msg_flag() | integer()],
                  Handle :: select_handle() | completion_handle(),
                  Data :: binary(),
                  SelectInfo :: select_info(),
                  CompletionInfo :: completion_info(),
                  Reason :: posix() | closed | invalid().
```

Receive data on a connected socket.

The argument `Length` specifies the size of the receive buffer.
Packet oriented sockets truncate the packet if the size is too small.

If `Length == 0`; a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).

For a socket of [type `stream`](`t:type/0`), when a `Timeout` argument
is used, the operation iterates until `Length` bytes has been received,
or the operation times out.  If `Length == 0` all readily available
data is returned.

On a `select` system, when the default receive buffer size option
[`{otp,recvbuf}`](`t:otp_socket_option/0`) special value `{N,BufSize}`
is used, `N` limits how many `BufSize` buffers that may be received
in a tight loop before the receive operation returns.  The option value
`{1,BufSize}` is equivalent to just specifying a size value `BufSize`.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

When there is a socket error this function returns `{error, Reason}`,
or if some data arrived before the error; `{error, {Reason, Data}}`
(can only happen for a socket of [type `stream`](`t:type/0`)).

[](){: #recv-infinity }

If the `Timeout` argument is `infinity`; waits for the data to arrive.
For a socket of [type `stream`](`t:type/0`) this call
won't return until _all_ requested data can be delivered,
or if "all available" was requested when the first data chunk arrives,
or if the OS reports an error for the operation.

[](){: #recv-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has arrived after `Timeout` milliseconds,
or `{error, {timeout, Data}}` if some but not enough data
has been received on a socket of [type `stream`](`t:type/0`) with Length > 0.
It *can* also return directly with `{ok, Data}` ([type `dgram`](`t:type/0`)).

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no data
is immediately available `{error, timeout} is returned.
`On a socket of type [`stream`](`t:type/0`), `{error, {timeout, Data}}`
is returned if there is an insufficient amount of data immediately available.

[](){: #recv-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #recv-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`{ok, Data}`** - Complete success; All requested data was read.
- **`{more, Data}`** - Partial success; Some, but not all, data was read.
- **`{error, Reason}`** - An error occured and no data was read.

On `select` systems, for a socket of type [`stream`](`t:type/0`),
if `Length > 0` and there is some but not enough data available,
this function will return [`{select, {SelectInfo, Data}}`](`t:select_info/0`)
with partial `Data`.  A repeated call to complete the operation
may need an updated `Length` argument.

On `select` systems, if the option
[`{otp, select_read}`](`t:otp_socket_option/0`) is set,
[`{select_read, {SelectInfo, Data}}`](`t:select_info/0`)
is returned instead of `{ok, Data}` and a new asynchronous
receive operation has been initiated, which can be seen
as an automatic [nowait](#recv-nowait) call whenever
a receive operation is completed.

# `recvfrom`
*since OTP 22.0* 

```erlang
-spec recvfrom(Socket :: socket()) -> dynamic().
```

Equivalent to [`recvfrom(Socket, 0, [], infinity)`](`recvfrom/4`).

# `recvfrom`
*since OTP 22.0* 

```erlang
-spec recvfrom(Socket :: socket(), Flags :: list()) -> dynamic();
              (Socket :: socket(), BufSz :: non_neg_integer()) -> dynamic().
```

Receive a message on a socket.

With argument `BufSz`; equivalent to
[`recvfrom(Socket, BufSz, [], infinity)`](`recvfrom/4`).

With argument `Flags`; equivalent to
[`recvfrom(Socket, 0, Flags, infinity)`](`recvfrom/4`) *(since OTP 24.0)*.

# `recvfrom`
*since OTP 22.0* 

```erlang
-spec recvfrom(Socket :: socket(), Flags :: [msg_flag() | integer()], TimeoutOrHandle :: dynamic()) ->
                  dynamic();
              (Socket :: socket(), BufSz :: non_neg_integer(), Flags :: [msg_flag() | integer()]) ->
                  dynamic();
              (Socket :: socket(), BufSz :: non_neg_integer(), TimeoutOrHandle) -> dynamic()
                  when TimeoutOrHandle :: timeout() | nowait | select_handle() | completion_handle().
```

Receive a message on a socket.

With arguments `BufSz` and `Flags`; equivalent to
[`recvfrom(Socket, BufSz, Flags, infinity)`](`recvfrom/4`).

With arguments `BufSz` and `TimeoutOrHandle`; equivalent to
[`recvfrom(Socket, BufSz, [], TimeoutOrHandle)`](`recvfrom/4`).

With arguments `Flags` and `TimeoutOrHandle`; equivalent to
[`recvfrom(Socket, 0, Flags, TimeoutOrHandle)`](`recvfrom/4`)

`TimeoutOrHandle :: 'nowait'` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.

# `recvfrom`
*since OTP 22.0* 

```erlang
-spec recvfrom(Socket, BufSz, Flags, Timeout :: infinity) -> {ok, {Source, Data}} | {error, Reason}
                  when
                      Socket :: socket(),
                      BufSz :: non_neg_integer(),
                      Flags :: [msg_flag() | integer()],
                      Source :: sockaddr_recv(),
                      Data :: binary(),
                      Reason :: posix() | closed | invalid();
              (Socket, BufSz, Flags, Timeout :: non_neg_integer()) ->
                  {ok, {Source, Data}} | {error, Reason}
                  when
                      Socket :: socket(),
                      BufSz :: non_neg_integer(),
                      Flags :: [msg_flag() | integer()],
                      Source :: sockaddr_recv(),
                      Data :: binary(),
                      Reason :: posix() | closed | invalid() | timeout;
              (Socket, BufSz, Flags, nowait | Handle) ->
                  {ok, {Source, Data}} |
                  {select, SelectInfo} |
                  {select_read, {SelectInfo, {Source, Data}}} |
                  {completion, CompletionInfo} |
                  {error, Reason}
                  when
                      Socket :: socket(),
                      BufSz :: non_neg_integer(),
                      Flags :: [msg_flag() | integer()],
                      Handle :: select_handle() | completion_handle(),
                      Source :: sockaddr_recv(),
                      Data :: binary(),
                      SelectInfo :: select_info(),
                      CompletionInfo :: completion_info(),
                      Reason :: posix() | closed | invalid().
```

Receive a message on a socket.

This function is intended primarily for sockets that are not connection
oriented such as type [`dgram`](`t:type/0`) or [`seqpacket`](`t:type/0`)
where messages may arrive from different source addresses.

Argument `BufSz` specifies the number of bytes for the receive buffer.
If the buffer size is too small, the message will be truncated.

If `BufSz` is `0`, a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).

If there is no known appropriate buffer size, it may be possible
to use the receive [message flag](`t:msg_flag/0`) `peek`.
When this flag is used, the message is _not_ "consumed" from
the underlying buffers, so another `recvfrom/1,2,3,4` call
is needed, possibly with an adjusted buffer size.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s, as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

[](){: #recvfrom-infinity }

If the `Timeout` argument is `infinity`; waits for a message
to arrive, or for a socket error.

[](){: #recvfrom-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); returns `{error, timeout}`
if no message has arrived after `Timeout` milliseconds.

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no message
is immediately available `{error, timeout}` is returned.

[](){: #recvfrom-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the 'Handle' argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*,
starts an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #recvfrom-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`{ok, {Source, Data}}`** - Success.
- **`{error, Reason}`** - An error occured and no data was read.

# `recvmmsg`
*since OTP 29.0* 

```erlang
-spec recvmmsg(Socket, VLen, BufSz, CtrlSz, Flags, Timeout :: infinity) -> {ok, Msgs} | {error, Reason}
                  when
                      Socket :: socket(),
                      VLen :: non_neg_integer(),
                      BufSz :: non_neg_integer(),
                      CtrlSz :: non_neg_integer(),
                      Flags :: [msg_flag() | integer()],
                      Msgs :: [msg_recv()],
                      Reason :: posix() | closed | invalid();
              (Socket, VLen, BufSz, CtrlSz, Flags, Timeout :: non_neg_integer()) ->
                  {ok, Msgs} | {error, Reason}
                  when
                      Socket :: socket(),
                      VLen :: non_neg_integer(),
                      BufSz :: non_neg_integer(),
                      CtrlSz :: non_neg_integer(),
                      Flags :: [msg_flag() | integer()],
                      Msgs :: [msg_recv()],
                      Reason :: posix() | closed | invalid() | timeout;
              (Socket, VLen, BufSz, CtrlSz, Flags, nowait | Handle) ->
                  {ok, Msgs} |
                  {select, SelectInfo} |
                  {select_read, {SelectInfo, Msgs}} |
                  {completion, CompletionInfo} |
                  {error, Reason}
                  when
                      Socket :: socket(),
                      VLen :: non_neg_integer(),
                      BufSz :: non_neg_integer(),
                      CtrlSz :: non_neg_integer(),
                      Handle :: select_handle() | completion_handle(),
                      Flags :: [msg_flag() | integer()],
                      Msgs :: [msg_recv()],
                      SelectInfo :: select_info(),
                      CompletionInfo :: completion_info(),
                      Reason :: posix() | closed | invalid().
```

Receive multiple messages on a socket.

This function is equivalent to calling [`recvmsg/5`](`recvmsg/5`) multiple times,
but uses the platform's `recvmmsg` syscall for better performance when receiving
multiple datagrams.

> #### Note {: .info }
>
> This function is only available on Linux and BSD systems (not macOS/Darwin or Windows).
> On unsupported platforms, it will return `{error, notsup}`.

Arguments:
- `VLen` - Maximum number of messages to receive (must be >= 1).
- `BufSz` - Buffer size for each message (0 uses default).
- `CtrlSz` - Control message buffer size for each message (0 uses default).
- `Flags` - Receive flags (same as `recvmsg/5`).
- `Timeout` - Timeout or `nowait` or `infinity`.

Returns a list of message maps, one per received message. The list length
indicates how many messages were actually received.

# `recvmsg`
*since OTP 22.0* 

```erlang
-spec recvmsg(Socket :: socket()) -> dynamic().
```

Equivalent to [`recvmsg(Socket, 0, 0, [], infinity)`](`recvmsg/5`).

# `recvmsg`
*since OTP 22.0* 

```erlang
-spec recvmsg(Socket :: socket(), Flags :: list()) -> dynamic();
             (Socket :: socket(), TimeoutOrHandle) -> dynamic()
                 when
                     TimeoutOrHandle :: timeout() | nowait | Handle,
                     Handle :: select_handle() | completion_handle().
```

Receive a message on a socket.

With argument `Flags`; equivalent to
[`recvmsg(Socket, 0, 0, Flags, infinity)`](`recvmsg/5`).

With argument `TimeoutOrHandle`; equivalent to
[`recvmsg(Socket, 0, 0, [], TimeoutOrHandle)`](`recvmsg/5`).

`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.

# `recvmsg`
*since OTP 22.0* 

```erlang
-spec recvmsg(Socket :: dynamic(), Flags :: list(), TimeoutOrHandle :: dynamic()) -> dynamic();
             (Socket :: dynamic(), BufSz :: integer(), CtrlSz :: integer()) -> dynamic().
```

Receive a message on a socket.

With arguments  `Flags`; equivalent to
[`recvmsg(Socket, 0, 0, Flags, infinity)`](`recvmsg/5`).

With argument `TimeoutOrHandle`; equivalent to
[`recvmsg(Socket, 0, 0, [], TimeoutOrHandle)`](`recvmsg/5`).

`TimeoutOrHandle :: nowait` has been allowed *since OTP 22.1*.

`TimeoutOrHandle :: Handle` has been allowed *since OTP 24.0*.

# `recvmsg`
*since OTP 24.0* 

```erlang
-spec recvmsg(Socket :: socket(),
              BufSz :: non_neg_integer(),
              CtrlSz :: non_neg_integer(),
              TimeoutOrHandle :: dynamic()) ->
                 dynamic().
```

Equivalent to
[`recvmsg(Socket, BufSz, CtrlSz, [], TimeoutOrHandle)`](`recvmsg/5`).

# `recvmsg`
*since OTP 22.0* 

```erlang
-spec recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout :: infinity) -> {ok, Msg} | {error, Reason}
                 when
                     Socket :: socket(),
                     BufSz :: non_neg_integer(),
                     CtrlSz :: non_neg_integer(),
                     Flags :: [msg_flag() | integer()],
                     Msg :: msg_recv(),
                     Reason :: posix() | closed | invalid();
             (Socket, BufSz, CtrlSz, Flags, Timeout :: non_neg_integer()) -> {ok, Msg} | {error, Reason}
                 when
                     Socket :: socket(),
                     BufSz :: non_neg_integer(),
                     CtrlSz :: non_neg_integer(),
                     Flags :: [msg_flag() | integer()],
                     Msg :: msg_recv(),
                     Reason :: posix() | closed | invalid() | timeout;
             (Socket, BufSz, CtrlSz, Flags, nowait | Handle) ->
                 {ok, Msg} |
                 {select, SelectInfo} |
                 {select_read, {SelectInfo, Msg}} |
                 {completion, CompletionInfo} |
                 {error, Reason}
                 when
                     Socket :: socket(),
                     BufSz :: non_neg_integer(),
                     CtrlSz :: non_neg_integer(),
                     Handle :: select_handle() | completion_handle(),
                     Flags :: [msg_flag() | integer()],
                     Msg :: msg_recv(),
                     SelectInfo :: select_info(),
                     CompletionInfo :: completion_info(),
                     Reason :: posix() | closed | invalid().
```

Receive a message on a socket.

This function receives a data message with control messages
as well as its source address.

Arguments `BufSz` and `CtrlSz` specifies the number of bytes for the
receive buffer and the control message buffer. If the buffer size(s)
is(are) too small, the message and/or control message list will be truncated.

If `BufSz` is `0`, a default buffer size is used, which can be set by
[`socket:setopt(Socket, {otp,recvbuf}, BufSz)`](`setopt/3`).
The same applies to `CtrlSz` and
[`socket:setopt(Socket, {otp,recvctrlbuf}, CtrlSz)`](`setopt/3`).

If there is no known appropriate buffer size, it may be possible
to use the receive [message flag](`t:msg_flag/0`) `peek`.
When this flag is used, the message is _not_ "consumed" from
the underlying buffers, so another `recvfrom/1,2,3,4` call
is needed, possibly with an adjusted buffer size.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s, as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

[](){: #recvmsg-infinity }

If the `Timeout` argument is `infinity`; waits for the message
to arrive, or for a socket error.

[](){: #recvmsg-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no message has arrived after `Timeout` milliseconds.

`Timeout = 0` only polls the OS receive call and doesn't
engage the Asynchronous Calls mechanisms.  If no message
is immediately available `{error, timeout}` is returned.

[](){: #recvmsg-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the 'Handle' argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*,
starts an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #recvmsg-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`{ok, Msg}`** - Success.
- **`{error, Reason}`** - An error occured and no data was read.

# `rest_iov`
*since OTP 28.0.2* 

```erlang
-spec rest_iov(Written, IOV) -> RestIOV
                  when Written :: non_neg_integer(), IOV :: erlang:iovec(), RestIOV :: erlang:iovec().
```

Calculate the rest I/O vector after a partially successful sendv
(CompletionStatus was {ok, Written}).

# `send`
*since OTP 22.0* 

```erlang
-spec send(Socket, Data) -> Result
              when
                  Socket :: socket(),
                  Data :: iodata(),
                  Result ::
                      ok |
                      {ok, RestData :: binary()} |
                      {select, SelectInfo :: dynamic()} |
                      {completion, CompletionInfo :: dynamic()} |
                      {error, Reason :: dynamic()}.
```

Equivalent to [`send(Socket, Data, [], infinity)`](`send/4`).

# `send`
*since OTP 22.0* 

```erlang
-spec send(Socket :: term(), Data :: term(), Cont :: tuple()) -> _;
          (Socket :: term(), Data :: term(), Flags :: list()) -> _;
          (Socket :: term(), Data :: term(), Timeout :: timeout()) -> _.
```

Send data on a connected socket.

With argument `Timeout`; equivalent to
[`send(Socket, Data, [], Timeout)`](`send/4`).

With argument `Flags`; equivalent to
[`send(Socket, Data, Flags, infinity)`](`send/4`).

With argument `Cont`; equivalent to
[`send(Socket, Data, Cont, infinity)`](`send/4`) *(since OTP 24.0)*.

# `send`
*since OTP 22.0* 

```erlang
-spec send(Socket, Data, Flags | Cont, Timeout :: infinity) ->
              ok | {ok, RestData} | {error, Reason} | {error, {Reason, RestData}}
              when
                  Socket :: socket(),
                  Data :: iodata(),
                  Flags :: [msg_flag() | integer()],
                  Cont :: select_info(),
                  RestData :: binary(),
                  Reason :: posix() | closed | invalid() | netname_deleted | too_many_cmds | eei();
          (Socket, Data, Flags | Cont, Timeout :: non_neg_integer()) ->
              ok | {ok, RestData} | {error, Reason | timeout} | {error, {Reason | timeout, RestData}}
              when
                  Socket :: socket(),
                  Data :: iodata(),
                  Flags :: [msg_flag() | integer()],
                  Cont :: select_info(),
                  RestData :: binary(),
                  Reason :: posix() | closed | invalid() | netname_deleted | too_many_cmds | eei();
          (Socket, Data, Flags | Cont, nowait | Handle) ->
              ok |
              {ok, RestData} |
              {select, SelectInfo} |
              {select, {SelectInfo, RestData}} |
              {completion, CompletionInfo} |
              {error, Reason}
              when
                  Socket :: socket(),
                  Data :: iodata(),
                  Flags :: [msg_flag() | integer()],
                  Cont :: select_info(),
                  Handle :: select_handle() | completion_handle(),
                  RestData :: binary(),
                  SelectInfo :: select_info(),
                  CompletionInfo :: completion_info(),
                  Reason :: posix() | closed | invalid() | netname_deleted | too_many_cmds | eei().
```

Send data on a connected socket.

The message `Flags` may be symbolic `t:msg_flag/0`s and/or
`t:integer/0`s as in the platform's appropriate header files.
The values of all symbolic flags and integers are or:ed together.

The `Data`, if it is not a `t:binary/0`, is copied into one before
calling the platform network API, because a single buffer is required.
A returned `RestData` is a sub binary of it.

The return value indicates the result from the platform's network layer:

- **`ok`** - All data was accepted by the OS for delivery

- **`{ok, RestData}`** - Some but not all data was accepted,
  but no error was reported (partially successful send).  `RestData`
  is the tail of `Data` that wasn't accepted.

  This cannot happen for a socket of [type `stream`](`t:type/0`) where
  such a partially successful send is retried until the data is either
  accepted for delivery or there is an error.

  For a socket of [type `dgram`](`t:type/0`) this should probably
  also not happen since a message that cannot be passed atomically
  should render an error.

  It is nevertheless possible for the platform's network layer
  to return this,  surely more possible for a socket of
  [type `seqpacket`](`t:type/0`).

- **`{error, Reason}`** - An error has been reported and no data
  was accepted for delivery.  [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, RestData}}`** - An error was reported but before that
  some data was accepted for delivery. `RestData` is the tail of `Data`
  that wasn't accepted. See `{error, Reason}` above.

  This can only happen for a socket of [type `stream`](`t:type/0`)
  when a partially successful send is retried until there is an error.

[](){: #send-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #send-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #send-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #send-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`ok`** - Complete success; The data was written in its entirety.
- **`{ok, Written}`** - Partial success; Some but not all data was written,
  but no error was reported. `Written` is the number of bytes that was written.
- **`{error, Reason}`** - An error occured and no data was sent.

[](){: #send-cont }

If the function is called with a `Cont` argument, that is;
the [`SelectInfo`](`t:select_info/0`) from the previous
[`send/3,4`](`send/3`) call; the send is continued with
preprocessed send parameters in the `SelectInfo`.
Using this argument variant avoids for example having to validate
and encode message flags in every call but the first.

# `sendfile`
*since OTP 24.0* 

```erlang
-spec sendfile(Socket, FileHandle | Continuation) -> dynamic()
                  when Socket :: socket(), FileHandle :: file:fd(), Continuation :: select_info().
```

Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, 0, 0, infinity)`](`sendfile/5`).

# `sendfile`
*since OTP 24.0* 

```erlang
-spec sendfile(Socket, FileHandle | Continuation, Timeout | Handle) -> dynamic()
                  when
                      Socket :: socket(),
                      FileHandle :: file:fd(),
                      Continuation :: select_info(),
                      Timeout :: infinity | non_neg_integer(),
                      Handle :: nowait | select_handle().
```

Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, 0, 0, Timeout_or_Handle)`](`sendfile/5`).

# `sendfile`
*since OTP 24.0* 

```erlang
-spec sendfile(Socket, FileHandle | Continuation, Offset, Count) -> dynamic()
                  when
                      Socket :: socket(),
                      FileHandle :: file:fd(),
                      Continuation :: select_info(),
                      Offset :: integer(),
                      Count :: non_neg_integer().
```

Send a file on a socket.

Equivalent to
[`sendfile(Socket, FileHandle_or_Continuation, Offset, Count, infinity)`](`sendfile/5`).

# `sendfile`
*since OTP 24.0* 

```erlang
-spec sendfile(Socket, FileHandle | Continuation, Offset, Count, Timeout :: infinity) ->
                  {ok, BytesSent} | {error, Reason} | {error, {Reason, BytesSent}}
                  when
                      Socket :: socket(),
                      FileHandle :: file:fd(),
                      Continuation :: select_info(),
                      Offset :: integer(),
                      Count :: non_neg_integer(),
                      BytesSent :: non_neg_integer(),
                      Reason :: posix() | closed | invalid();
              (Socket, FileHandle | Continuation, Offset, Count, Timeout :: non_neg_integer()) ->
                  {ok, BytesSent} | {error, Reason} | {error, {Reason, BytesSent}}
                  when
                      Socket :: socket(),
                      FileHandle :: file:fd(),
                      Continuation :: select_info(),
                      Offset :: integer(),
                      Count :: non_neg_integer(),
                      BytesSent :: non_neg_integer(),
                      Reason :: posix() | closed | invalid() | timeout;
              (Socket,
               FileHandle | Continuation,
               Offset, Count,
               nowait | (SelectHandle :: select_handle())) ->
                  {ok, BytesSent} |
                  {select, SelectInfo} |
                  {select, {SelectInfo, BytesSent}} |
                  {error, Reason}
                  when
                      Socket :: socket(),
                      FileHandle :: file:fd(),
                      Continuation :: select_info(),
                      Offset :: integer(),
                      Count :: non_neg_integer(),
                      BytesSent :: non_neg_integer(),
                      SelectInfo :: select_info(),
                      Reason :: posix() | closed | invalid().
```

Send a file on a socket.

> #### Note {: .info }
> This function unsupported on Windows.

The `FileHandle` argument must refer to an open raw file
as described in `file:open/2`.

The `Offset` argument is the file offset to start reading from.
The default offset is `0`.

The `Count` argument is the number of bytes to transfer
from `FileHandle` to `Socket`. If `Count = 0` (the default)
the transfer stops at the end of file.

The return value indicates the result from the platform's network layer:

- **`{ok, BytesSent}`** - The transfer completed successfully after `BytesSent`
  bytes of data.

- **`{error, Reason}`** - An error has been reported and no data
  was transferred. [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, BytesSent}}`** - An error has been reported
  but before that some data was transferred. See `{error, Reason}`
  and `{ok, BytesSent}` above.

[](){: #sendfile-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendfile-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, BytesSent}}` if some but not all data was sent
(accepted by the OS for delivery).

[](){: #sendfile-nowait }

If the `Handle` argument is `nowait`,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

After receiving a [`select` message](#async-messages);
call [`sendfile/2,3,4,5`](`sendfile/2`)
with `SelectInfo` as the `Continuation` argument,
to complete the operation.

[](){: #sendfile-cont }

If the function is called with a `Continuation` argument, that is;
the [`SelectInfo`](`t:select_info/0`) from the previous
`sendfile/5` call; the transfer is continued with
preprocessed parameters in the `SelectInfo`.

The `Offset` and maybe `Count` arguments will probably
need to be updated between continuation calls.

# `sendmmsg`
*since OTP 29.0* 

```erlang
-spec sendmmsg(Socket, Msgs, Flags, Timeout :: infinity) -> ok | {ok, Rest} | {error, Reason}
                  when
                      Socket :: socket(),
                      Msgs :: [msg_send()],
                      Flags :: [msg_flag() | integer()],
                      Rest :: [erlang:iovec()],
                      Reason :: posix() | closed | invalid();
              (Socket, Msgs, Flags, Timeout :: non_neg_integer()) ->
                  ok | {ok, Rest} | {error, Reason | timeout}
                  when
                      Socket :: socket(),
                      Msgs :: [msg_send()],
                      Flags :: [msg_flag() | integer()],
                      Rest :: [erlang:iovec()],
                      Reason :: posix() | closed | invalid();
              (Socket, Msgs, Flags, nowait | Handle) ->
                  ok |
                  {ok, Rest} |
                  {select_write, {SelectInfo, SentCount}} |
                  {select, SelectInfo} |
                  {completion, CompletionInfo} |
                  {error, Reason}
                  when
                      Socket :: socket(),
                      Msgs :: [msg_send()],
                      Flags :: [msg_flag() | integer()],
                      Handle :: select_handle() | completion_handle(),
                      Rest :: [erlang:iovec()],
                      SentCount :: non_neg_integer(),
                      SelectInfo :: select_info(),
                      CompletionInfo :: completion_info(),
                      Reason :: posix() | closed | invalid().
```

Send multiple messages on a socket.

This function is equivalent to calling [`sendmsg/4`](`sendmsg/4`) multiple times,
but uses the platform's `sendmmsg` syscall for better performance when sending
multiple datagrams.

> #### Note {: .info }
>
> This function is only available on Linux and BSD systems (not macOS/Darwin or Windows).
> On unsupported platforms, it will return `{error, notsup}`.

On success, returns either:
- **`ok`** – when all messages were sent in full (or there were zero messages).
- **`{ok, Rest}`** – when one or more messages had a partial write. `Rest` is a list with one
  element per message that was not fully sent, in message order. Each element is the
  remaining data for that message in the same form as [`sendmsg/4`](`sendmsg/4`)'s rest data
  (`t:erlang:iovec/0`), so you can retry with `sendmsg` for each.

On error returns `{error, Reason}`.

# `sendmsg`
*since OTP 22.0* 

```erlang
-spec sendmsg(Socket, Msg) -> Result
                 when Socket :: socket(), Msg :: msg_send() | erlang:iovec(), Result :: dynamic().
```

Equivalent to [`sendmsg(Socket, Msg, [], infinity)`](`sendmsg/4`).

# `sendmsg`
*since OTP 22.0* 

```erlang
-spec sendmsg(Socket :: socket(), Msg :: msg_send(), Flags :: list()) -> dynamic();
             (Socket :: socket(), Data :: msg_send() | erlang:iovec(), Cont :: select_info()) ->
                 dynamic();
             (Socket :: socket(), Msg :: msg_send(), Timeout :: infinity) -> dynamic().
```

Send data and control messages on a socket.

With arguments `Msg` and `Timeout`; equivalent to
[`sendmsg(Socket, Msg, [], Timeout)`](`sendmsg/4`).

With arguments `Msg` and `Flags`; equivalent to
[`sendmsg(Socket, Msg, Flags, infinity)`](`sendmsg/4`).

With arguments `Data` and `Cont`; equivalent to
[`sendmsg(Socket, Data, Cont, infinity)`](`sendmsg/4`) *since OTP 24.0*.

# `sendmsg`
*since OTP 22.0* 

```erlang
-spec sendmsg(Socket, Msg, Flags, Timeout :: infinity) ->
                 ok | {ok, RestData} | {error, Reason} | {error, {Reason, RestData}}
                 when
                     Socket :: socket(),
                     Msg :: msg_send(),
                     Flags :: [msg_flag() | integer()],
                     RestData :: erlang:iovec(),
                     Reason :: posix() | closed | invalid();
             (Socket, Msg, Flags, Timeout :: non_neg_integer()) ->
                 ok | {ok, RestData} | {error, Reason | timeout} | {error, {Reason | timeout, RestData}}
                 when
                     Socket :: socket(),
                     Msg :: msg_send(),
                     Flags :: [msg_flag() | integer()],
                     RestData :: erlang:iovec(),
                     Reason :: posix() | closed | invalid();
             (Socket, Msg, Flags, nowait | Handle) ->
                 ok |
                 {ok, RestData} |
                 {select, SelectInfo} |
                 {select, {SelectInfo, RestData}} |
                 {completion, CompletionInfo} |
                 {error, Reason} |
                 {error, {Reason, RestData}}
                 when
                     Socket :: socket(),
                     Msg :: msg_send(),
                     Flags :: [msg_flag() | integer()],
                     Handle :: select_handle() | completion_handle(),
                     RestData :: erlang:iovec(),
                     SelectInfo :: select_info(),
                     CompletionInfo :: completion_info(),
                     Reason :: posix() | closed | invalid();
             (Socket, Data, Cont, Timeout :: infinity) ->
                 ok | {ok, RestData} | {error, Reason} | {error, {Reason, RestData}}
                 when
                     Socket :: socket(),
                     Data :: msg_send() | erlang:iovec(),
                     Cont :: select_info(),
                     RestData :: erlang:iovec(),
                     Reason :: posix() | closed | invalid();
             (Socket, Data, Cont, Timeout :: non_neg_integer()) ->
                 ok | {ok, RestData} | {error, Reason | timeout} | {error, {Reason | timeout, RestData}}
                 when
                     Socket :: socket(),
                     Data :: msg_send() | erlang:iovec(),
                     Cont :: select_info(),
                     RestData :: erlang:iovec(),
                     Reason :: posix() | closed | invalid();
             (Socket, Data, Cont, nowait | Handle) ->
                 ok |
                 {ok, RestData} |
                 {select, SelectInfo} |
                 {select, {SelectInfo, RestData}} |
                 {completion, CompletionInfo} |
                 {error, Reason} |
                 {error, {Reason, RestData}}
                 when
                     Socket :: socket(),
                     Data :: msg_send() | erlang:iovec(),
                     Cont :: select_info(),
                     Handle :: select_handle(),
                     RestData :: erlang:iovec(),
                     SelectInfo :: select_info(),
                     CompletionInfo :: completion_info(),
                     Reason :: posix() | closed | invalid().
```

Send data and control messages on a socket.

The argument `Msg` is a map that contains the data to be sent
under the key `iov` as an`t:erlang:iovec/0` (list of `t:binary/0`).
It may also contain the destination address under the key `addr`,
which is mandatory if the socket isn't connected.  If the socket
_is_ connected it is best to not have an `addr` key since
the platform may regard that as an error (or ignore it).
Under the key `ctrl` there may be a list of protocol and platform dependent
control messages (a.k.a ancillary data, a.k.a control information)
to send.

[](){: #sendmsg-iov }

The message data is given to the platform's network layer as an
I/O vector without copying the content. If the number of elements
in the I/O vector is larger than allowed on the platform (reported
in the [`iov_max`](`t:info/0`) field from `info/0`), on a socket of
[type `stream`](`t:type/0`) the send is iterated over all elements,
but for other socket types the call fails.

See `send/4` for a description of the `Flags` argument
and the return values.

> #### Note {: .info }
>
> On Windows, this function can only be used with datagram and raw sockets.

[](){: #sendmsg-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendmsg-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #sendmsg-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #sendmsg-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`ok`** - Complete success; The data was written in its entirety.
- **`{ok, Written}`** - Partial success; Some but not all data was written,
  but no error was reported. `Written` is the number of bytes that was written.
- **`{error, Reason}`** - An error occured and no data was sent.

After receiving a [`select` message](#async-messages);
call [`sendmsg/3,4`](`sendmsg/3`) with `SelectInfo` as the `Cont` argument,
to complete the operation.

[](){: #sendmsg-cont }

With the arguments `Data` and [`Cont`](`t:select_info/0`),
continues the send operation.  `Cont` should be
the [`SelectInfo`](`t:select_info/0`) returned from the previous
[`sendmsg/2,3,4`](`sendmsg/2`) call.

`Data` can be a [`Msg`](`t:msg_send/0`) `t:map/0`
where only the key `iov` is used, or an `t:erlang:iovec/0`.

# `sendto`
*since OTP 22.0* 

```erlang
-spec sendto(Socket :: socket(), Data :: iodata(), Cont | Dest) -> Result
                when
                    Cont :: select_info(),
                    Dest :: sockaddr(),
                    Result :: ok | {ok, RestData} | {error, Reason} | {error, {Reason, RestData}},
                    RestData :: binary(),
                    Reason :: posix() | closed | invalid().
```

Send data on a socket.

With argument `Dest`; equivalent to
[`sendto(Socket, Data, Dest, [], infinity)`](`sendto/5`).

With argument `Cont`; equivalent to
[`sendto(Socket, Data, Cont, infinity)`](`sendto/4`) *since OTP 24.0*.

# `sendto`
*since OTP 22.0* 

```erlang
-spec sendto(Socket :: socket(),
             Data :: iodata(),
             Dest :: sockaddr(),
             Flags :: [msg_flag() | integer()]) ->
                dynamic();
            (Socket :: socket(),
             Data :: iodata(),
             Cont :: select_info(),
             Timeout :: timeout() | nowait | (Handle :: select_handle())) ->
                dynamic();
            (Socket :: socket(),
             Data :: iodata(),
             Dest :: sockaddr(),
             Timeout :: timeout() | nowait | (Handle :: select_handle() | completion_handle())) ->
                dynamic().
```

Send data on a socket.

With arguments `Dest` and `TimeoutOrHandle`; equivalent to
[`sendto(Socket, Data, Dest, [], TimeoutOrHandle)`](`sendto/5`).

With arguments `Dest` and `Flags`; equivalent to
[`sendto(Socket, Data, Dest, Flags, infinity)`](`sendto/5`).

With arguments `Cont` and `TimeoutOrHandle`; `Cont` must be
the [`SelectInfo`](`t:select_info/0`) from the previous
[`sendto/3,4,5`](`sendto/3`) call and the send is continued with
preprocessed send parameters in the `SelectInfo`.
Using this argument variant avoids for example having o validate
and encode message flags in every call but the first.
*(Since OTP 24.0)*

See the last argument (argument 5) of `sendto/5` for
an explanation of `TimeoutOrHandle`.

# `sendto`
*since OTP 22.0* 

```erlang
-spec sendto(Socket, Data, Dest, Flags, Timeout :: infinity) ->
                ok | {ok, RestData} | {error, Reason} | {error, {Reason, RestData}}
                when
                    Socket :: socket(),
                    Data :: iodata(),
                    Dest :: sockaddr(),
                    Flags :: [msg_flag() | integer()],
                    RestData :: binary(),
                    Reason :: posix() | closed | invalid();
            (Socket, Data, Dest, Flags, Timeout :: non_neg_integer()) ->
                ok | {ok, RestData} | {error, Reason | timeout} | {error, {Reason | timeout, RestData}}
                when
                    Socket :: socket(),
                    Data :: iodata(),
                    Dest :: sockaddr(),
                    Flags :: [msg_flag() | integer()],
                    RestData :: binary(),
                    Reason :: posix() | closed | invalid();
            (Socket, Data, Dest, Flags, nowait | Handle) ->
                ok |
                {ok, RestData} |
                {select, SelectInfo} |
                {select, {SelectInfo, RestData}} |
                {completion, CompletionInfo} |
                {error, Reason}
                when
                    Socket :: socket(),
                    Data :: iodata(),
                    Dest :: sockaddr(),
                    Flags :: [msg_flag() | integer()],
                    Handle :: select_handle() | completion_handle(),
                    RestData :: binary(),
                    SelectInfo :: select_info(),
                    CompletionInfo :: completion_info(),
                    Reason :: posix() | closed | invalid().
```

Send data on a socket.

The `To` argument is the destination address where to send the data.
For a connected socket this argument is still passed to the OS call
that may ignore the address or return an error.

See `send/4` for a description of the `Flags` and `Data` arguments,
and the return values.

[](){: #sendto-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendto-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestData}}` if some data was sent
(accepted by the OS for delivery).  `RestData` is the tail of the data
that hasn't been sent.

[](){: #sendto-nowait }

If the `Handle` argument is `nowait` *(since OTP 22.1)*,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
*(since OTP 24.0)*, or on _Windows_, the equivalent
`t:completion_handle/0` *(since OTP 26.0)*, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #sendto-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`ok`** - Complete success; The data was written in its entirety.
- **`{ok, Written}`** - Partial success; Some but not all data was written,
  but no error was reported. `Written` is the number of bytes that was written.
- **`{error, Reason}`** - An error occured and no data was sent.

After receiving a [`select` message](#async-messages);
call [`sendto/3,4`](`sendto/3`) with `SelectInfo` as the `Cont` argument,
to complete the operation.

# `sendv`
*since OTP 27.0* 

```erlang
-spec sendv(Socket, IOV) -> ok | {ok, RestIOV} | {error, Reason} | {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid().
```

# `sendv`
*since OTP 27.0* 

```erlang
-spec sendv(Socket, IOV, Timeout :: infinity) ->
               ok | {ok, RestIOV} | {error, Reason} | {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid();
           (Socket, IOV, Timeout :: non_neg_integer()) ->
               ok | {ok, RestIOV} | {error, Reason} | {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid() | timeout;
           (Socket, IOV, nowait | Handle) ->
               ok |
               {ok, RestIOV} |
               {select, SelectInfo} |
               {select, {SelectInfo, RestIOV}} |
               {completion, CompletionInfo} |
               {completion, {CompletionInfo, RestIOV}} |
               {error, Reason} |
               {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   Handle :: select_handle() | completion_handle(),
                   RestIOV :: erlang:iovec(),
                   SelectInfo :: select_info(),
                   CompletionInfo :: completion_info(),
                   Reason :: posix() | closed | invalid();
           (Socket, IOV, Cont) -> ok | {ok, RestIOV} | {error, Reason} | {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   Cont :: select_info(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid().
```

Send `t:erlang:iovec/0` data on a connected socket.

See [`sendmsg/4`](#sendmsg-iov) about how the [`IOV`](`t:erlang:iovec/0`)
data is handled towards the platform's network layer.

The return value indicates the result from the platform's network layer:

- **`ok`** - All data has been accepted by the OS for delivery.

- **`{ok, RestIOV}`** - Some but not all data was accepted,
  but no error was reported (partially successful send).  `RestIOV`
  is the tail of `IOV` that wasn't accepted.

- **`{error, Reason}`** - An error has been reported and no data
  was accepted for delivery.  [`Reason :: posix/0`](`t:posix/0`)
  is what the platform's network layer reported.  `closed` means
  that this socket library was informed that the socket was closed,
  and `t:invalid/0` means that this socket library found
  an argument to be invalid.

- **`{error, {Reason, RestIOV}}`** -  - An error was reported but before that
  some data was accepted for delivery. `RestIOV` is the tail of `IOV`
  that wasn't accepted. See `{error, Reason}` above.

[](){: #sendv-infinity }

If the `Timeout` argument is `infinity`; wait for the OS to
complete the send operation (take responsibility for the data),
or return an error.

[](){: #sendv-timeout }

If the `Timeout` argument is a time-out value
(`t:non_neg_integer/0`); return `{error, timeout}`
if no data has been sent within `Timeout` millisecond,
or `{error, {timeout, RestIOV}}` if some data was sent
(accepted by the OS for delivery).  `RestIOV` is the tail of the data
that hasn't been sent.

[](){: #sendv-nowait }

If the `Handle` argument is `nowait`,
starts an [asynchronous call](#asynchronous-calls) if the operation
couldn't be completed immediately.

If the `Handle` argument is a `t:select_handle/0`,
or on _Windows_, the equivalent `t:completion_handle/0`, starts
an [asynchronous call](#asynchronous-calls) like for `nowait`.

See the note [Asynchronous Calls](#asynchronous-calls)
at the start of this module reference manual page.

[](){: #sendv-completion_status }

The possible values for `CompletionStatus` in the completion message are:
- **`ok`** - Complete success; The I/O vector was written in its entirety.
- **`{ok, Written}`** - Partial success; Some but not all data was written,
  but no error was reported. `Written` is the number of bytes that was written.
  [`rest_iov(Written, IOV)`](`rest_iov/2`) can be used to calculate the rest
  I/O vector (from the original IOV).
- **`{error, Reason}`** - An error occured and no data was sent.

[](){: #sendv-cont }

With the argument [`Cont`](`t:select_info/0`), equivalent to
[`sendv(Socket, IOV, Cont, infinity)`](`sendv/4`).

# `sendv`
*since OTP 27.0* 

```erlang
-spec sendv(Socket, IOV, Cont, Timeout :: infinity) ->
               ok | {ok, RestIOV} | {error, Reason} | {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   Cont :: select_info(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid();
           (Socket, IOV, Cont, Timeout :: non_neg_integer()) ->
               ok | {ok, RestIOV} | {error, Reason} | {error, {Reason | RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   Cont :: select_info(),
                   RestIOV :: erlang:iovec(),
                   Reason :: posix() | closed | invalid() | timeout;
           (Socket, IOV, Cont, nowait | SelectHandle) ->
               ok |
               {ok, RestIOV} |
               {select, SelectInfo} |
               {select, {SelectInfo, RestIOV}} |
               {error, Reason} |
               {error, {Reason, RestIOV}}
               when
                   Socket :: socket(),
                   IOV :: erlang:iovec(),
                   Cont :: select_info(),
                   SelectHandle :: select_handle(),
                   RestIOV :: erlang:iovec(),
                   SelectInfo :: select_info(),
                   Reason :: posix() | closed | invalid().
```

Send data on a connected socket, continuation.

Continues sending data on a connected socket.
`Cont` is the [`SelectInfo`](`t:select_info/0`) returned from
the previous [`sendv/2,3`](`sendv/2`) call.
`IOV` should be the rest data that wasn't sent.

See [asynchronous calls](#asynchronous-calls) about continuing
unfinished calls.

See `sendv/3` about the return values.

# `setopt`
*since OTP 24.0* 

```erlang
-spec setopt(Socket, SocketOption, Value) -> ok | {error, invalid() | closed}
                when
                    Socket :: socket(),
                    SocketOption :: {Level :: otp, Opt :: otp_socket_option()},
                    Value :: dynamic();
            (Socket, SocketOption, Value) -> ok | {error, posix() | invalid() | closed}
                when Socket :: socket(), SocketOption :: socket_option(), Value :: dynamic().
```

Set a socket option.

Set an OS protocol level option, or an `otp` pseudo protocol level option.
The latter level is this module's implementation level above
the OS protocol levels.

See the type [otp_socket_option() ](`t:otp_socket_option/0`)
for a description of the `otp` protocol level.

See the type `t:socket_option/0` for which OS protocol level options
that this implementation knows about, how they are related to OS option names,
and if there are known peculiarities with any of them.

What options that are valid depends on the OS, and on the kind of socket
(`t:domain/0`,`t:type/0` and `t:protocol/0`).  See the type
`t:socket_option()` and the
[socket options ](socket_usage.md#socket_options) chapter
in the User's Guide for more info.

> #### Note {: .info }
>
> Not all options are valid, nor possible to set, on all platforms. That is,
> even if this `socket` implementation  support an option; it doesn't mean
> that the underlying OS does.

# `setopt`
*since OTP 22.0* 

```erlang
-spec setopt(socket(), Level :: term(), Opt :: term(), Value :: term()) -> _.
```

Set a socket option _(backwards compatibility function)_.

Equivalent to [`setopt(Socket, {Level, Opt}, Value)`](`setopt/3`),
or as a special case if `Opt = NativeOpt ::` `t:integer/0`
and `Value =` `t:binary/0` equivalent to
[`setopt_native(Socket, {Level, NativeOpt}, ValueSpec)`](`setopt_native/3`).

Use `setopt/3` or `setopt_native/3` instead to handle
the option level and name as a single term, and to make the
difference between known options and native options clear.

# `setopt_native`
*since OTP 24.0* 

```erlang
-spec setopt_native(Socket, Option, Value) -> ok | {error, posix() | invalid() | closed}
                       when
                           Socket :: socket(),
                           Option :: socket_option() | {Level, NativeOpt} | {NativeLevel, NativeOpt},
                           Value :: native_value(),
                           Level :: level(),
                           NativeLevel :: integer(),
                           NativeOpt :: integer().
```

Set a "native" socket option.

Sets a socket option that may be unknown to our implementation, or that has a
type not compatible with our implementation, that is; in "native mode".

If `Value` is an `t:integer/0` it will be used as a `C` type `(int)`,
if it is a `t:boolean/0` it will be used as a `C` type `(int)`
with the `C` implementations values for `false` or `true`,
and if it is a `t:binary/0` its content and size will be used
as the option value.

The socket option may be specified with an ordinary
`t:socket_option/0` tuple, with a symbolic `Level` as
`{`[`Level :: level/0`](`t:level/0`)`, `[`NativeOpt :: integer/0`](`t:integer/0`)`}`,
or with integers for both `NativeLevel` and `NativeOpt` as
`{`[`NativeLevel :: integer/0`](`t:integer/0`)`, `[`NativeOpt :: integer/0`](`t:integer/0`)`}`.

If an option is valid depends both on the platform and on
what kind of socket it is (`t:domain/0`, `t:type/0` and `t:protocol/0`).

The integer values for `NativeLevel` and `NativeOpt` as well as the `Value`
encoding has to be deduced from the header files for the running system.

# `shutdown`
*since OTP 22.0* 

```erlang
-spec shutdown(Socket, How) -> ok | {error, Reason}
                  when Socket :: socket(), How :: read | write | read_write, Reason :: posix() | closed.
```

Shut down all or part of a full-duplex connection.

# `sockname`
*since OTP 22.0* 

```erlang
-spec sockname(Socket :: socket()) -> {ok, SockAddr} | {error, Reason}
                  when SockAddr :: sockaddr_recv(), Reason :: posix() | closed.
```

Get the socket's address.

Returns the address to which the socket is currently bound.
If the bind address had the wildcard port `0`,
the address returned by this function contains the ephemeral port
selected by the OS.

# `socknames`
*since OTP 29.0* 

```erlang
-spec socknames(Socket :: socket(), AssocId :: sctp_assoc_id()) -> {ok, [SockAddr]} | {error, Reason}
                   when SockAddr :: sockaddr_recv(), Reason :: posix() | closed.
```

Get the socket's address.

Returns all the locally bound addresses to which the socket is currently bound.

If the socket is IPv4 then all returned addresess will be IPv4.
If the socket is IPv6, then the returned addresess can be a mix of
IPv4 and IPv6.

For a one-to-many socket, AssocId specifies the association.
For a one-to-one socket, AssocId is ignored.

If AssocId is 0 (zero), then the returned addresses are without any
particular association.

# `supports`
*since OTP 22.0* 

```erlang
-spec supports() ->
                  [{Key1 :: term(),
                    boolean() | [{Key2 :: term(), boolean() | [{Key3 :: term(), boolean()}]}]}].
```

Retrieve information about what socket features
the module and the platform supports.

Returns a list of, in no particular order,
`{Key1, `[`supports(Key1)`](`supports/1`)`}` tuples
for every `Key1` described in `supports/1`,
and `{Key, boolean()}` tuples for each of the following keys:

- **`sctp`** - SCTP support

- **`ipv6`** - IPv6 support

- **`local`** - Unix Domain sockets support (`AF_UNIX | AF_LOCAL`)

- **`netns`** - Network Namespaces support (Linux, `setns(2)`)

- **`sendfile`** - Sendfile support (`sendfile(2)`)

# `supports`
*since OTP 22.0* 

```erlang
-spec supports(Key1 :: term()) -> [{Key2 :: term(), boolean() | [{Key3 :: term(), boolean()}]}].
```

Retrieve information about what socket features
the module and the platform supports.

If `Key1 = msg_flags` returns a list of `{Flag, boolean()}`
tuples for every `Flag` in `t:msg_flag/0` with the `t:boolean/0`
indicating if the flag is supported on this platform.

If `Key1 = protocols` returns a list of `{Name, boolean()}`
tuples for every `Name` in`t:protocol/0` with the `t:boolean/0`
indicating if the protocol is supported on this platform.

If `Key1 = options` returns a list of `{SocketOption, boolean()}`
tuples for every `SocketOption` in `t:socket_option/0` with the `t:boolean/0`
indicating if the socket option is supported on this platform.

There is no particular order of any of the returned lists.

For other values of `Key1` returns `[]`.
Note that in future versions of this module or on different platforms,
there might be more supported keys.

# `supports`
*since OTP 22.0* 

```erlang
-spec supports(Key1 :: term(), Key2 :: term()) -> [{Key3 :: term(), boolean()}].
```

Retrieve information about what socket features
the module and the platform supports.

If `Key1 = options`, for a `Key2` in `t:level/0` returns
a list of `{Opt, boolean()}` tuples for all known socket options
[`Opt` on that `Level = Key2`](`t:socket_option/0`) with the `t:boolean/0`
indicating if the socket option is supported on this platform.
See `setopt/3` and `getopt/2`.

There is no particular order of any of the returned lists.

For other values of `Key1` or `Key2` returns `[]`.
Note that in future versions of this module or on different platforms,
there might be more supported keys.

# `use_registry`
*since OTP 23.1* 

```erlang
-spec use_registry(D :: boolean()) -> ok.
```

Set the global [`use_registry`](`t:otp_socket_option/0`)
option default value.

Globally change if the socket registry is to be used or not.
Note that its still possible to override this explicitly
when creating an individual sockets, see [`open/2,3,4`](`open/2`)
for more info (the [`Opts :: map/0`](`t:map/0`)).

# `which_sockets`
*since OTP 22.3* 

```erlang
-spec which_sockets() -> [socket()].
```

Return a list of all known sockets.

Equivalent to [`which_sockets(fun (_) -> true end)`](`which_sockets/1`).

# `which_sockets`
*since OTP 22.3* 

```erlang
-spec which_sockets(FilterRule) -> [socket()]
                       when
                           FilterRule ::
                               inet | inet6 | local | stream | dgram | seqpacket | sctp | tcp | udp |
                               pid() |
                               fun((socket_info()) -> boolean()).
```

Return a filtered list of known sockets.

There are several predefined `FilterRule`s and one general:

- **`inet | inet6`** - Only the sockets with matching `t:domain/0`
  are returned.

- **`stream | dgram | seqpacket`** - Only the sockets with
  matching `t:type/0` are returned.

- **`sctp | tcp | udp`** - Only the sockets with
  matching `t:protocol/0` are returned.

- **`t:pid/0`** - Only the sockets with matching Controlling Process
  are returned. See the OTP socket option
  [`controlling_process`](`t:otp_socket_option/0`).

- **`fun((socket_info()) -> boolean())`** - The general filter rule.
  A fun that takes the socket info and returns a `t:boolean/0`
  indicating if the socket should be returned or not.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
