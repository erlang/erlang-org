# `erl_boot_server`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/erl_boot_server.erl#L28)

Boot server for other Erlang machines.

This server is used to assist diskless Erlang nodes that fetch all Erlang code
from another machine.

This server is used to fetch all code, including the start script, if an Erlang
runtime system is started with command-line flag `-loader inet`. All hosts
specified with command-line flag `-hosts Host` must have one instance of this
server running.

This server can be started with the Kernel configuration parameter
`start_boot_server`.

The `erl_boot_server` can read regular files and files in archives. See `m:code`
and `m:erl_prim_loader` in ERTS.

> #### Warning {: .warning }
>
> The support for loading code from archive files is experimental. It is
> released before it is ready to obtain early feedback. The file format,
> semantics, interfaces, and so on, can be changed in a future release.

## SEE ALSO

[`erts:init`](`m:init`), [`erts:erl_prim_loader`](`m:erl_prim_loader`)

# `add_slave`

```erlang
-spec add_slave(Slave) -> ok | {error, Reason}
                   when
                       Slave :: Host,
                       Host :: inet:ip_address() | inet:hostname(),
                       Reason :: {badarg, Slave}.
```

Adds a `Slave` node to the list of allowed slave hosts.

# `delete_slave`

```erlang
-spec delete_slave(Slave) -> ok | {error, Reason}
                      when
                          Slave :: Host,
                          Host :: inet:ip_address() | inet:hostname(),
                          Reason :: {badarg, Slave}.
```

Deletes a `Slave` node from the list of allowed slave hosts.

# `start`

```erlang
-spec start(Slaves) -> {ok, Pid} | {error, Reason}
               when
                   Slaves :: [Host],
                   Host :: inet:ip_address() | inet:hostname(),
                   Pid :: pid(),
                   Reason :: any().
```

The same as [`start(Slaves, #{})`](`start/2`).

# `start`
*since OTP 29.0* 

```erlang
-spec start(Slaves, Options) -> {ok, Pid} | {error, Reason}
               when
                   Slaves :: [Host],
                   Host :: inet:ip_address() | inet:hostname(),
                   Options :: #{listen_port => inet:port_number()},
                   Pid :: pid(),
                   Reason :: any().
```

Starts the boot server. `Slaves` is a list of IP addresses for hosts, which are
allowed to use this server as a boot server. `Options` is a map with
configuration options.

The boot server listening port can be configured with `listen_port`.
If an empty map is provided, or `listen_port` is zero, then an ephemeral port
is used.

# `start_link`

```erlang
-spec start_link(Slaves) -> {ok, Pid} | {error, Reason}
                    when
                        Slaves :: [Host],
                        Host :: inet:ip_address() | inet:hostname(),
                        Pid :: pid(),
                        Reason :: any().
```

The same as [`start_link(Slaves, #{})`](`start_link/2`).

# `start_link`
*since OTP 29.0* 

```erlang
-spec start_link(Slaves, Options) -> {ok, Pid} | {error, Reason}
                    when
                        Slaves :: [Host],
                        Host :: inet:ip_address() | inet:hostname(),
                        Options :: #{listen_port => inet:port_number()},
                        Pid :: pid(),
                        Reason :: any().
```

The same as [`start(Slaves, Options)`](`start/2`), but it also links to the
caller.

# `which_slaves`

```erlang
-spec which_slaves() -> Slaves
                      when
                          Slaves :: [Slave],
                          Slave :: {Netmask :: inet:ip_address(), Address :: inet:ip_address()}.
```

Returns the current list of allowed slave hosts.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
