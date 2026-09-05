# `ct_slave`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_slave.erl#L28)

`Common Test` framework functions for starting and stopping nodes for
Large-Scale Testing.

This module exports functions used by the `Common Test` Master to start and stop
"slave" nodes. It is the default callback module for the `{init, node_start}`
term in the Test Specification.

# `start_options`
*since OTP R14B* 

```erlang
-type start_options() ::
          [{username, string()} |
           {password, string()} |
           {boot_timeout, non_neg_integer()} |
           {init_timeout, non_neg_integer()} |
           {startup_timeout, non_neg_integer()} |
           {startup_functions, [mfa()]} |
           {monitor_master, boolean()} |
           {kill_if_fail, boolean()} |
           {erl_flags, string()} |
           {env, [{Name :: os:env_var_name(), Val :: os:env_var_value() | false}]} |
           {ssh_port, inet:port_number()} |
           {ssh_opts, ssh:client_options()}].
```

Options used for starting `ct_slave` node.

# `stop_options`
*since OTP R14B* 

```erlang
-type stop_options() :: [{stop_timeout, non_neg_integer()}].
```

Options used for stopping `ct_slave` node.

# `start`
*since OTP R14B* 

> This function is deprecated. ct_slave:start/1 is deprecated; use ?CT_PEER(), or the 'peer' module instead.

```erlang
-spec start(Node) -> ok | {error, Reason, Node} when Node :: node(), Reason :: atom().
```

Starts an Erlang node with name `Node` on the local host.

See also [`ct_slave:start/3`](`start/3`).

# `start`
*since OTP R14B* 

> This function is deprecated. ct_slave:start/2 is deprecated; use ?CT_PEER(), or the 'peer' module instead.

```erlang
-spec start(HostOrNode, NodeOrOpts) -> ok | {error, Reason, Node}
               when
                   HostOrNode :: atom() | node(),
                   NodeOrOpts :: node() | start_options(),
                   Reason :: atom(),
                   Node :: node().
```

Starts an Erlang node with default options on a specified host, or on the local
host with specified options. That is, the call is interpreted as
[`start(Host, Node)`](`start/2`) when the second argument is atom-valued and
[`start(Node, Opts)`](`start/2`) when it is list-valued.

See also [`ct_slave:start/3`](`start/3`).

# `start`
*since OTP R14B* 

> This function is deprecated. ct_slave:start/3 is deprecated; use ?CT_PEER(), or the 'peer' module instead.

```erlang
-spec start(Host, Node, Opts) -> ok | {error, Reason, Node}
               when Host :: atom(), Node :: node(), Opts :: start_options(), Reason :: atom().
```

Starts an Erlang node with name `Node` on host `Host` as specified by the
combination of options in `Opts`.

Options `Username` and `Password` are used to log on to the remote host `Host`.
`Username`, if omitted, defaults to the current username. `Password` is empty by
default.

A list of functions specified in option `Startup` are executed after startup of
the node. Notice that all used modules are to be present in the code path on
`Host`.

The time-outs are applied as follows:

- **`BootTimeout`** - The time to start the Erlang node, in seconds. Defaults to
  3 seconds. If the node is not pingable within this time, the result
  `{error, boot_timeout, NodeName}` is returned.

- **`InitTimeout`** - The time to wait for the node until it calls the internal
  callback function informing master about a successful startup. Defaults to 1
  second. In case of a timed out message, the result
  `{error, init_timeout, NodeName}` is returned.

- **`StartupTimeout`** - The time to wait until the node stops to run
  `StartupFunctions`. Defaults to 1 second. If this time-out occurs, the result
  `{error, startup_timeout, NodeName}` is returned.

_Options:_

- **`monitor_master`** - Specifies if the slave node is to be stopped if the
  master node stops. Defaults to `false`.

- **`kill_if_fail`** - Specifies if the slave node is to be killed if a time-out
  occurs during initialization or startup. Defaults to `true`. Notice that the
  node can also be still alive it the boot time-out occurred, but it is not
  killed in this case.

- **`erl_flags`** - Specifies which flags are added to the parameters of the
  executable `erl`.

- **`env`** - Specifies a list of environment variables that will extend the
  environment.

_Special return values:_

- `{error, already_started, NodeName}` if the node with the specified name is
  already started on a specified host.
- `{error, started_not_connected, NodeName}` if the node is started, but not
  connected to the master node.
- `{error, not_alive, NodeName}` if the node on which
  [`ct_slave:start/3`](`start/3`) is called, is not alive. Notice that
  `NodeName` is the name of the current node in this case.

# `stop`
*since OTP R14B* 

> This function is deprecated. ct_slave:stop/1 is deprecated; use ?CT_PEER(), or the 'peer' module instead.

```erlang
-spec stop(Node) -> {ok, Node} | {error, Reason, Node} when Node :: node(), Reason :: atom().
```

Stops the running Erlang node with name `Node` on the local host.

# `stop`
*since OTP R14B* 

> This function is deprecated. ct_slave:stop/2 is deprecated; use ?CT_PEER(), or the 'peer' module instead.

```erlang
-spec stop(HostOrNode, NodeOrOpts) -> {ok, Node} | {error, Reason, Node}
              when
                  HostOrNode :: atom() | Node,
                  Node :: node(),
                  NodeOrOpts :: Node | stop_options(),
                  Reason :: atom().
```

Stops the running Erlang node with name `Node` on host `Host`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
