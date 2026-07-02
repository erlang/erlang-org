# `pool`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/pool.erl#L22)

Load distribution facility.

This module can be used to run a set of Erlang nodes as a pool of computational
processors. It is organized as a master and a set of slave nodes and includes
the following features:

- The slave nodes send regular reports to the master about their current load.
- Queries can be sent to the master to determine which node will have the least
  load.

The BIF [`statistics(run_queue)`](`statistics/1`) is used for estimating future
loads. It returns the length of the queue of ready to run processes in the
Erlang runtime system.

The slave nodes are started with the `m:slave` module. This effects terminal
I/O, file I/O, and code loading.

If the master node fails, the entire pool exits.

[](){: #files }

## Files

`.hosts.erlang` is used to pick hosts where nodes can be started. For
information about format and location of this file, see `net_adm:host_file/0`.

`$HOME/.erlang.slave.out.HOST` is used for all extra I/O that can come from the
slave nodes on standard I/O. If the startup procedure does not work, this file
can indicate the reason.

# `attach`

```elixir
-spec attach(Node) -> already_attached | attached when Node :: node().
```

Ensures that a pool master is running and includes `Node` in the pool master's
pool of nodes.

# `get_node`

```elixir
-spec get_node() -> node().
```

Returns the node with the expected lowest future load.

# `get_nodes`

```elixir
-spec get_nodes() -> [node()].
```

Returns a list of the current member nodes of the pool.

# `pspawn`

```elixir
-spec pspawn(Mod, Fun, Args) -> pid() when Mod :: module(), Fun :: atom(), Args :: [term()].
```

Spawns a process on the pool node that is expected to have the lowest future
load.

# `pspawn_link`

```elixir
-spec pspawn_link(Mod, Fun, Args) -> pid() when Mod :: module(), Fun :: atom(), Args :: [term()].
```

Spawns and links to a process on the pool node that is expected to have the
lowest future load.

# `start`

```elixir
-spec start(Name) -> Nodes when Name :: atom(), Nodes :: [node()].
```

# `start`

```elixir
-spec start(Name, Args) -> Nodes when Name :: atom(), Args :: string(), Nodes :: [node()].
```

Starts a new pool.

The file `.hosts.erlang` is read to find host names where the
pool nodes can be started; see section [Files](`m:pool#module-files`). The startup
procedure fails if the file is not found.

The slave nodes are started with [`slave:start/2,3`](`slave:start/2`), passing
along `Name` and, if provided, `Args`. `Name` is used as the first part of the
node names, `Args` is used to specify command-line arguments.

Access rights must be set so that all nodes in the pool have the authority to
access each other.

The function is synchronous and all the nodes, and all the system servers, are
running when it returns a value.

# `stop`

```elixir
-spec stop() -> stopped.
```

Stops the pool and kills all the slave nodes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
