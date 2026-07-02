# `net`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/net.erl#L23)

Network interface.

This module provides an API for the network interface.

# `address_info`
*since OTP 22.0* 

```erlang
-type address_info() ::
          #{family := socket:domain(),
            socktype := any | socket:type() | integer(),
            protocol := socket:protocol(),
            address := socket:sockaddr()}.
```

# `ifaddrs`
*since OTP 22.0* 

```erlang
-type ifaddrs() ::
          #{name := string(),
            flags := ifaddrs_flags(),
            addr => socket:sockaddr(),
            netmask => socket:sockaddr(),
            broadaddr => socket:sockaddr(),
            dstaddr => socket:sockaddr()}.
```

Interface addresses and flags.

This type defines addresses and flags for an interface.

> #### Note {: .info }
>
> Not all fields of this map has to be present. The flags field can be used to
> test for some of the fields. For example `broadaddr` will only be present if
> the `broadcast` flag is present in flags.

# `ifaddrs_filter`
*not exported* *since OTP 22.0* 

```erlang
-type ifaddrs_filter() ::
          all | default | inet | inet6 | packet | link | hwaddr |
          ifaddrs_filter_map() |
          ifaddrs_filter_fun().
```

Interface address filtering selector.

- **all** - All interfaces

- **default** - Interfaces with address family `inet` _or_ `inet6`

- **inet | inet6 | packet | link** - Interfaces with _only_ the specified
 address family
- **hwaddr** - Interfaces with address family `packet` _or_ `link`

# `ifaddrs_filter_fun`
*not exported* *since OTP 22.0* 

```erlang
-type ifaddrs_filter_fun() :: fun((ifaddrs()) -> boolean()).
```

Interface address filtering selector `t:function/0`.

For each `ifaddrs` entry, return either `true` to keep the entry
or `false` to discard the entry.

For example, to get an interface list which only contains
non-`loopback` `inet` interfaces:

```erlang
net:getifaddrs(
    fun (#{ addr  := #{family := inet},
            flags := Flags}) ->
          not lists:member(loopback, Flags);
        (_) ->
          false
    end).
```

# `ifaddrs_filter_map`
*not exported* *since OTP 22.0* 

```erlang
-type ifaddrs_filter_map() ::
          #{family :=
                all | default | local | inet | inet6 | packet | link |
                [local | inet | inet6 | packet | link],
            flags := any | [ifaddrs_flag()]}.
```

Interface address filtering selector map.

The `family` field can only have the (above) specified values
(and not all the values of socket:domain()).
It can also be a list of values, to cover the situation when
any of the specified families are accepted.
For example, family can be set to `[inet,inet6]` if either `inet` or `inet6`
is accepted.

The use of the `flags` field is that any flag provided must exist for the
interface.

For example, if `family` is set to `inet` and `flags` to
`[broadcast, multicast]` only interfaces with address family `inet`
and the flags `broadcast` and `multicast` will be listed.

# `ifaddrs_flag`
*since OTP 22.0* 

```erlang
-type ifaddrs_flag() ::
          up | broadcast | debug | loopback | pointopoint | notrailers | running | noarp | promisc |
          master | slave | multicast | portsel | automedia | dynamic.
```

# `ifaddrs_flags`
*since OTP 22.0* 

```erlang
-type ifaddrs_flags() :: [ifaddrs_flag()].
```

# `name_info`
*since OTP 22.0* 

```erlang
-type name_info() :: #{host := string(), service := string()}.
```

# `name_info_flag`
*since OTP 22.0* 

```erlang
-type name_info_flag() :: namereqd | dgram | nofqdn | numerichost | numericserv.
```

# `name_info_flag_ext`
*since OTP 22.0* 

```erlang
-type name_info_flag_ext() :: idn.
```

# `name_info_flags`
*since OTP 22.0* 

```erlang
-type name_info_flags() :: [name_info_flag() | name_info_flag_ext()].
```

# `network_interface_index`
*since OTP 22.0* 

```erlang
-type network_interface_index() :: non_neg_integer().
```

# `network_interface_name`
*since OTP 22.0* 

```erlang
-type network_interface_name() :: string().
```

# `getaddrinfo`
*since OTP 22.0* 

```erlang
-spec getaddrinfo(Host) -> {ok, Info} | {error, Reason}
                     when Host :: string(), Info :: [address_info()], Reason :: term().
```

# `getaddrinfo`
*since OTP 22.0* 

```erlang
-spec getaddrinfo(Host, undefined) -> {ok, Info} | {error, Reason}
                     when Host :: string(), Info :: [address_info()], Reason :: term();
                 (undefined, Service) -> {ok, Info} | {error, Reason}
                     when Service :: string(), Info :: [address_info()], Reason :: term();
                 (Host, Service) -> {ok, Info} | {error, Reason}
                     when
                         Host :: string(),
                         Service :: string(),
                         Info :: [address_info()],
                         Reason :: term().
```

Network address and service translation.

This function is the inverse of [`getnameinfo`](`getnameinfo/1`). It converts
host and service to a corresponding socket address.

One of the `Host` and `Service` may be `undefined` but _not_ both.

# `gethostname`
*since OTP 22.0* 

```erlang
-spec gethostname() -> {ok, HostName} | {error, Reason} when HostName :: string(), Reason :: term().
```

Return the name of the current host.

# `getifaddrs`
*since OTP 22.3* 

```erlang
-spec getifaddrs() -> {ok, IfAddrs} | {error, Reason} when IfAddrs :: [ifaddrs()], Reason :: term().
```

# `getifaddrs`
*since OTP 22.3* 

```erlang
-spec getifaddrs(Filter) -> {ok, IfAddrs} | {error, Reason}
                    when Filter :: ifaddrs_filter(), IfAddrs :: [ifaddrs()], Reason :: term();
                (Namespace) -> {ok, IfAddrs} | {error, Reason}
                    when Namespace :: file:filename_all(), IfAddrs :: [ifaddrs()], Reason :: term().
```

Get interface addresses.

With argument 'Filter: get the machines interface addresses,
filtered according to `Filter`.

With argument `Namespace`: equivalent to
[`getifaddrs(default, Namespace)`](`getifaddrs/2`).

# `getifaddrs`
*since OTP 22.3* 

```erlang
-spec getifaddrs(Filter, Namespace) -> {ok, IfAddrs} | {error, Reason}
                    when
                        Filter :: ifaddrs_filter(),
                        Namespace :: file:filename_all(),
                        IfAddrs :: [ifaddrs()],
                        Reason :: term().
```

Get interface addresses in a namespace.

The same as [`getifaddrs(Filter)`](`getifaddrs/1`) but
in the specified `Namespace`.

# `getnameinfo`
*since OTP 22.0* 

```erlang
-spec getnameinfo(SockAddr) -> {ok, Info} | {error, Reason}
                     when SockAddr :: socket:sockaddr(), Info :: name_info(), Reason :: term().
```

# `getnameinfo`
*since OTP 22.0* 

```erlang
-spec getnameinfo(SockAddr, Flags) -> {ok, Info} | {error, Reason}
                     when
                         SockAddr :: socket:sockaddr(),
                         Flags :: name_info_flags() | undefined,
                         Info :: name_info(),
                         Reason :: term().
```

Address-to-name translation in a protocol-independant manner.

This function is the inverse of [`getaddrinfo`](`getaddrinfo/1`). It converts a
socket address to a corresponding host and service.

# `getservbyname`
*since OTP 27.1* 

```erlang
-spec getservbyname(Name) -> {ok, PortNumber} | {error, Reason}
                       when
                           Name :: atom() | string(),
                           PortNumber :: socket:port_number(),
                           Reason :: term().
```

# `getservbyname`
*since OTP 27.1* 

```erlang
-spec getservbyname(Name, Protocol) -> {ok, PortNumber} | {error, Reason}
                       when
                           Name :: atom() | string(),
                           PortNumber :: socket:port_number(),
                           Protocol :: any | socket:protocol(),
                           Reason :: term().
```

Get service by name.

This function is used to get the port number of the specified protocol
for the named service.

# `getservbyport`
*since OTP 27.1* 

```erlang
-spec getservbyport(PortNumber) -> {ok, Name} | {error, Reason}
                       when
                           PortNumber :: socket:port_number(),
                           Name :: atom() | string(),
                           Reason :: term().
```

# `getservbyport`
*since OTP 27.1* 

```erlang
-spec getservbyport(PortNumber, Protocol) -> {ok, Name} | {error, Reason}
                       when
                           PortNumber :: socket:port_number(),
                           Protocol :: any | socket:protocol(),
                           Name :: atom() | string(),
                           Reason :: term().
```

Get service by name.

This function is used to get the service name of the specified protocol
for the given port number.

# `if_index2name`
*since OTP 22.0* 

```erlang
-spec if_index2name(Idx) -> {ok, Name} | {error, Reason}
                       when
                           Idx :: network_interface_index(),
                           Name :: network_interface_name(),
                           Reason :: term().
```

Mappings between network interface index and names.

# `if_name2index`
*since OTP 22.0* 

```erlang
-spec if_name2index(Name) -> {ok, Idx} | {error, Reason}
                       when
                           Name :: network_interface_name(),
                           Idx :: network_interface_index(),
                           Reason :: term().
```

Mappings between network interface names and indexes.

# `if_names`
*since OTP 22.0* 

```erlang
-spec if_names() -> {ok, Names} | {error, Reason}
                  when
                      Names :: [{Idx, If}],
                      Idx :: network_interface_index(),
                      If :: network_interface_name(),
                      Reason :: term().
```

Get network interface names and indexes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
