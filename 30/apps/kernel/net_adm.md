# `net_adm`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/net_adm.erl#L22)

Various Erlang net administration routines.

This module contains various network utility functions.

## Files

File `.hosts.erlang` consists of a number of host names written as Erlang terms.
It is looked for in the current work directory, the user's home directory, and
`$OTPROOT` (the root directory of Erlang/OTP), in that order.

The format of file `.hosts.erlang` must be one host name per line. The host
names must be within quotes.

_Example:_

```text
'super.eua.ericsson.se'.
'renat.eua.ericsson.se'.
'grouse.eua.ericsson.se'.
'gauffin1.eua.ericsson.se'.
^ (new line)
```

# `verbosity`
*not exported* 

```erlang
-type verbosity() :: silent | verbose.
```

# `dns_hostname`

```erlang
-spec dns_hostname(Host) -> {ok, Name} | {error, Host} when Host :: atom() | string(), Name :: string().
```

Returns the official name of `Host`, or `{error, Host}` if no such name is
found. See also `m:inet`.

# `host_file`

```erlang
-spec host_file() -> Hosts | {error, Reason}
                   when
                       Hosts :: [Host :: atom()],
                       Reason ::
                           file:posix() |
                           badarg | terminated | system_limit |
                           {Line :: integer(), Mod :: module(), Term :: term()}.
```

Reads file `.hosts.erlang`, see section [Files](`m:net_adm#module-files`). Returns the
hosts in this file as a list. Returns `{error, Reason}` if the file cannot be
read or the Erlang terms on the file cannot be interpreted.

# `localhost`

```erlang
-spec localhost() -> Name when Name :: string().
```

Returns the name of the local host. If Erlang was started with command-line flag
`-name`, `Name` is the fully qualified name.

# `names`

```erlang
-spec names() -> {ok, [{Name, Port}]} | {error, Reason}
               when Name :: string(), Port :: non_neg_integer(), Reason :: address | file:posix().
```

# `names`

```erlang
-spec names(Host) -> {ok, [{Name, Port}]} | {error, Reason}
               when
                   Host :: atom() | string() | inet:ip_address(),
                   Name :: string(),
                   Port :: non_neg_integer(),
                   Reason :: address | file:posix().
```

Returns the names and associated port numbers of the Erlang nodes that `epmd`
registered at the specified host.

Similar to `epmd -names`, see [`erts:epmd`](`e:erts:epmd_cmd.md`).

Returns `{error, address}` if `epmd` is not operational.

_Example:_

```erlang
(arne@dunn)1> net_adm:names().
{ok,[{"arne",40262}]}
```

# `ping`

```erlang
-spec ping(Node) -> pong | pang when Node :: atom().
```

Sets up a connection to `Node`. Returns `pong` if it is successful, otherwise
`pang`.

# `world`

```erlang
-spec world() -> [node()].
```

# `world`

```erlang
-spec world(Arg) -> [node()] when Arg :: verbosity().
```

Calls [`names(Host)`](`names/1`) for all hosts that are specified in the Erlang
host file `.hosts.erlang`, collects the replies, and then evaluates
[`ping(Node)`](`ping/1`) on all those nodes. Returns the list of all nodes that
are successfully pinged.

If `Arg == verbose`, the function writes information about which nodes it is
pinging to `stdout`.

This function can be useful when a node is started, and the names of the other
network nodes are not initially known.

Returns `{error, Reason}` if `host_file/0` returns `{error, Reason}`.

# `world_list`

```erlang
-spec world_list(Hosts) -> [node()] when Hosts :: [atom()].
```

# `world_list`

```erlang
-spec world_list(Hosts, Arg) -> [node()] when Hosts :: [atom()], Arg :: verbosity().
```

Same as [`world/0,1`](`world/1`), but the hosts are specified as argument
instead of being read from `.hosts.erlang`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
