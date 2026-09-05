# `erl_epmd`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/erl_epmd.erl#L22)

Erlang interface towards epmd

This module communicates with the EPMD daemon, see [epmd](`e:erts:epmd_cmd.md`).
To implement your own epmd module please see
[ERTS User's Guide: How to Implement an Alternative Node Discovery for Erlang Distribution](`e:erts:alt_disco.md`)

# `address_please`
*since OTP 21.0* 

```erlang
-spec address_please(Name, Host, AddressFamily) -> Success | {error, term()}
                        when
                            Name :: string(),
                            Host :: string() | inet:ip_address(),
                            AddressFamily :: inet | inet6,
                            Port :: non_neg_integer(),
                            Version :: non_neg_integer(),
                            Success :: {ok, inet:ip_address()} | {ok, inet:ip_address(), Port, Version}.
```

Called by the distribution module to resolves the `Host` to an IP address of a
remote node.

As an optimization this function may also return the port and version of the
remote node. If port and version are returned `port_please/3` will not be
called.

# `listen_port_please`
*since OTP 23.0* 

```erlang
-spec listen_port_please(Name, Host) -> {ok, Port}
                            when
                                Name :: atom() | string(),
                                Host :: atom() | string() | inet:ip_address(),
                                Port :: non_neg_integer().
```

Called by the distribution module to get which port the local node should listen
to when accepting new distribution requests.

# `names`
*since OTP 21.0* 

```erlang
-spec names(Host) -> {ok, [{Name, Port}]} | {error, Reason}
               when
                   Host :: atom() | string() | inet:ip_address(),
                   Name :: string(),
                   Port :: non_neg_integer(),
                   Reason :: address | file:posix().
```

Called by [`net_adm:names/0`](`m:net_adm`). `Host` defaults to the localhost.
Returns the names and associated port numbers of the Erlang nodes that `epmd`
registered at the specified host. Returns `{error, address}` if `epmd` is not
operational.

_Example:_

```erlang
(arne@dunn)1> erl_epmd:names(localhost).
{ok,[{"arne",40262}]}
```

# `port_please`
*since OTP 21.0* 

```erlang
-spec port_please(Name, Host) -> {port, Port, Version} | noport | closed | {error, term()}
                     when
                         Name :: atom() | string(),
                         Host :: atom() | string() | inet:ip_address(),
                         Port :: non_neg_integer(),
                         Version :: non_neg_integer().
```

# `port_please`
*since OTP 21.0* 

```erlang
-spec port_please(Name, Host, Timeout) -> {port, Port, Version} | noport | closed | {error, term()}
                     when
                         Name :: atom() | string(),
                         Host :: atom() | string() | inet:ip_address(),
                         Timeout :: non_neg_integer() | infinity,
                         Port :: non_neg_integer(),
                         Version :: non_neg_integer().
```

Requests the distribution port for the given node of an EPMD instance. Together
with the port it returns a distribution protocol version which has been 5 since
Erlang/OTP R6.

# `register_node`
*since OTP 21.0* 

```erlang
-spec register_node(Name, Port) -> Result
                       when
                           Name :: string(),
                           Port :: non_neg_integer(),
                           Creation :: non_neg_integer(),
                           Result :: {ok, Creation} | {error, already_registered} | term().
```

# `register_node`
*since OTP 21.0* 

```erlang
-spec register_node(Name, Port, Driver) -> Result
                       when
                           Name :: string(),
                           Port :: non_neg_integer(),
                           Driver :: inet_tcp | inet6_tcp | inet | inet6,
                           Creation :: non_neg_integer() | -1,
                           Result :: {ok, Creation} | {error, already_registered} | term().
```

Registers the node with `epmd` and tells epmd what port will be used for the
current node. It returns a creation number. This number is incremented on each
register to help differentiate a new node instance connecting to epmd with the
same name.

After the node has successfully registered with epmd it will automatically
attempt reconnect to the daemon if the connection is broken.

# `start_link`
*since OTP 21.0* 

```erlang
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
```

This function is invoked as this module is added as a child of the
`erl_distribution` supervisor.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
