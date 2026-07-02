# `slave`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/slave.erl#L22)

This module provides functions for starting Erlang slave nodes.

All slave nodes that are started by a master terminate automatically when the
master terminates. All terminal output produced at the slave is sent back to
the master node. File I/O is done through the master.

Slave nodes on other hosts than the current one are started with the `ssh`
program. The user must be allowed to `ssh` to the remote hosts without being
prompted for a password. This can be arranged in a number of ways (for details,
see the `ssh` documentation). A slave node started on the same host as the
master inherits certain environment values from the master, such as the current
directory and the environment variables. For what can be assumed about the
environment when a slave is started on another host, see the documentation for
the `ssh` program.

An alternative to the `ssh` program can be specified on the command line to
[`erl(1)`](`e:erts:erl_cmd.md`) as follows:

```text
-rsh Program
```

Note that the command specified with the `-rsh` flag is treated as a file name
which may contain spaces. It is thus not possible to include any command line
options. The remote node will be launched as
`"$RSH" "$REMOTE_HOSTNAME" erl -detached -noinput ...`, so the `erl` command
must be found in the path on the remote host.

The slave node is to use the same file system at the master. At least,
Erlang/OTP is to be installed in the same place on both computers and the same
version of Erlang is to be used.

A node running on Windows can only start slave nodes on the host on which it is
running.

The master node must be alive.

# `pseudo`

> This function is deprecated. slave:pseudo/1 is deprecated; use the 'peer' module instead.

```elixir
-spec pseudo([Master :: node() | (ServerList :: [atom()])]) -> ok.
```

Calls [`pseudo(Master, ServerList)`](`pseudo/2`). If you want to start a node
from the command line and set up a number of pseudo servers, an Erlang runtime
system can be started as follows:

```text
% erl -name abc -s slave pseudo klacke@super x --
```

# `pseudo`

> This function is deprecated. slave:pseudo/2 is deprecated; use the 'peer' module instead.

```elixir
-spec pseudo(Master, ServerList) -> ok when Master :: node(), ServerList :: [atom()].
```

Starts a number of pseudo servers. A pseudo server is a server with a registered
name that does nothing but pass on all message to the real server that executes
at a master node. A pseudo server is an intermediary that only has the same
registered name as the real server.

For example, if you have started a slave node `N` and want to execute `pxw`
graphics code on this node, you can start server `pxw_server` as a pseudo server
at the slave node. This is illustrated as follows:

```erlang
rpc:call(N, slave, pseudo, [node(), [pxw_server]]).
```

# `relay`

> This function is deprecated. slave:relay/1 is deprecated; use the 'peer' module instead.

```elixir
-spec relay(Pid) -> no_return() when Pid :: pid().
```

Runs a pseudo server. This function never returns any value and the process that
executes the function receives messages. All messages received are simply passed
on to `Pid`.

# `start`

> This function is deprecated. slave:start/1 is deprecated; use the 'peer' module instead.

```elixir
-spec start(Host) -> {ok, Node} | {error, Reason}
               when
                   Host :: inet:hostname(),
                   Node :: node(),
                   Reason :: timeout | no_rsh | {already_running, Node}.
```

Equivalent to [`start(Host, Name)`](`start/2`) where `Name` is the same
as the node that executes this call.

# `start`

> This function is deprecated. slave:start/2 is deprecated; use the 'peer' module instead.

```elixir
-spec start(Host, Name) -> {ok, Node} | {error, Reason}
               when
                   Host :: inet:hostname(),
                   Name :: atom() | string(),
                   Node :: node(),
                   Reason :: timeout | no_rsh | {already_running, Node}.
```

# `start`

> This function is deprecated. slave:start/3 is deprecated; use the 'peer' module instead.

```elixir
-spec start(Host, Name, Args) -> {ok, Node} | {error, Reason}
               when
                   Host :: inet:hostname(),
                   Name :: atom() | string(),
                   Args :: string(),
                   Node :: node(),
                   Reason :: timeout | no_rsh | {already_running, Node}.
```

Starts a slave node on host `Host`. Host names need not necessarily be specified
as fully qualified names; short names can also be used. This is the same
condition that applies to names of distributed Erlang nodes.

The name of the started node becomes `Name@Host`.

The slave node resets its `t:io:user/0` process so that all terminal I/O that is
produced at the slave is automatically relayed to the master. Also, the file
server is relayed to the master.

Argument `Args` is used to set `erl` command-line arguments. It is
passed to the new node and can be used for a variety of purposes; see
[`erl(1)`](`e:erts:erl_cmd.md`).

As an example, suppose that you want to start a slave node at host `H` with node
name `Name@H` and want the slave node to have the following properties:

- Directory `Dir` is to be added to the code path.
- The Mnesia directory is to be set to `M`.
- The Unix `DISPLAY` environment variable is to be set to the display of the
  master node.

The following code is executed to achieve this:

```erlang
E = " -env DISPLAY " ++ net_adm:localhost() ++ ":0 ",
Arg = "-mnesia_dir " ++ M ++ " -pa " ++ Dir ++ E,
slave:start(H, Name, Arg).
```

The function returns `{ok, Node}`, where `Node` is the name of the new node,
otherwise `{error, Reason}`, where `Reason` can be one of:

- **`timeout`** - The master node failed to get in contact with the slave node.
  This can occur in a number of circumstances:

  - Erlang/OTP is not installed on the remote host.
  - The file system on the other host has a different structure to the the
    master.
  - The Erlang nodes have different cookies.

- **`no_rsh`** - No remote shell program was found on the computer. Note that
  `ssh` is used by default, but this can be overridden with the `-rsh` flag.

- **`{already_running, Node}`** - A node with name `Name@Host` already exists.

# `start_link`

> This function is deprecated. slave:start_link/1 is deprecated; use the 'peer' module instead.

```elixir
-spec start_link(Host) -> {ok, Node} | {error, Reason}
                    when
                        Host :: inet:hostname(),
                        Node :: node(),
                        Reason :: timeout | no_rsh | {already_running, Node}.
```

# `start_link`

> This function is deprecated. slave:start_link/2 is deprecated; use the 'peer' module instead.

```elixir
-spec start_link(Host, Name) -> {ok, Node} | {error, Reason}
                    when
                        Host :: inet:hostname(),
                        Name :: atom() | string(),
                        Node :: node(),
                        Reason :: timeout | no_rsh | {already_running, Node}.
```

# `start_link`

> This function is deprecated. slave:start_link/3 is deprecated; use the 'peer' module instead.

```elixir
-spec start_link(Host, Name, Args) -> {ok, Node} | {error, Reason}
                    when
                        Host :: inet:hostname(),
                        Name :: atom() | string(),
                        Args :: string(),
                        Node :: node(),
                        Reason :: timeout | no_rsh | {already_running, Node}.
```

Starts a slave node in the same way as `start/1,2,3`, except that the slave node
is linked to the currently executing process. If that process terminates, the
slave node also terminates.

For a description of arguments and return values, see
[`start/1,2,3`](`start/1`).

# `stop`

> This function is deprecated. slave:stop/1 is deprecated; use the 'peer' module instead.

```elixir
-spec stop(Node) -> ok when Node :: node().
```

Stops (kills) a node.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
