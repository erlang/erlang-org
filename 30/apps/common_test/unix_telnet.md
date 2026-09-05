# `unix_telnet`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/unix_telnet.erl#L23)

Callback module for `m:ct_telnet`, for connecting to a Telnet server on a UNIX
host.

It requires the following entry in the configuration file:

```erlang
{unix,[{telnet,HostNameOrIpAddress},
       {port,PortNum},                 % optional
       {username,UserName},
       {password,Password},
       {keep_alive,Bool}]}.            % optional
```

To communicate through Telnet to the host specified by `HostNameOrIpAddress`,
use the interface functions in `m:ct_telnet`, for example, `open(Name)` and
`cmd(Name,Cmd)`.

`Name` is the name you allocated to the Unix host in your `require` statement,
for example:

```erlang
suite() -> [{require,Name,{unix,[telnet]}}].
```

or

```erlang
ct:require(Name,{unix,[telnet]}).
```

The "keep alive" activity (that is, that `Common Test` sends NOP to the server
every 10 seconds if the connection is idle) can be enabled or disabled for one
particular connection as described here. It can be disabled for all connections
using `telnet_settings` (see `m:ct_telnet`).

The `{port,PortNum}` tuple is optional and if omitted, default Telnet port 23 is
used. Also the `keep_alive` tuple is optional, and the value default to `true`
(enabled).

### See Also

`m:ct`, `m:ct_telnet`

# `connect`
*since OTP 18.3.3* 

```erlang
-spec connect(ConnName, Ip, Port, Timeout, KeepAlive, TCPNoDelay, Extra) ->
                 {ok, Handle} | {error, Reason}
                 when
                     ConnName :: ct:target_name(),
                     Ip :: inet:socket_address() | inet:hostname(),
                     Port :: inet:port_number(),
                     Timeout :: timeout(),
                     KeepAlive :: boolean(),
                     TCPNoDelay :: boolean(),
                     Extra :: {Username, Password} | KeyOrName,
                     Username :: iodata(),
                     Password :: iodata(),
                     KeyOrName :: ct:key_or_name(),
                     Handle :: ct_telnet:handle(),
                     Reason :: term().
```

Callback for `ct_telnet.erl`.

[](){: #connect-6 }

Setup Telnet connection to a Unix host.

# `get_prompt_regexp`

```erlang
-spec get_prompt_regexp() -> Pattern when Pattern :: ct_telnet:prompt_regexp().
```

Callback for `ct_telnet.erl`.

Returns a suitable `regexp` string matching common prompts for users on Unix
hosts.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
