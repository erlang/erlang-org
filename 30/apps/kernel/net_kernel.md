# `net_kernel`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/net_kernel.erl#L22)

Erlang networking kernel.

The net kernel is a system process, registered as `net_kernel`, which must be
operational for distributed Erlang to work. The purpose of this process is to
implement parts of the BIFs [`spawn/4`](`spawn/4`) and
[`spawn_link/4`](`spawn_link/4`), and to provide monitoring of the network.

An Erlang node is started using command-line flag `-name` or `-sname`:

```text
$ erl -sname foobar
```

It is also possible to call [`net_kernel:start(foobar, #{})`](`start/2`)
directly from the normal Erlang shell prompt:

```erlang
1> net_kernel:start(foobar, #{name_domain => shortnames}).
{ok,<0.64.0>}
(foobar@gringotts)2>
```

If the node is started with command-line flag `-sname`, the node name is
`foobar@Host`, where `Host` is the short name of the host (not the fully
qualified domain name). If started with flag `-name`, the node name is
`foobar@Host`, where `Host` is the fully qualified domain name. For more
information, see [`erl`](`e:erts:erl_cmd.md`).

Normally, connections are established automatically when another node is
referenced. This functionality can be disabled by setting Kernel configuration
parameter `dist_auto_connect` to `never`, see [`kernel(6)`](kernel_app.md). In
this case, connections must be established explicitly by calling
`connect_node/1`.

Which nodes that are allowed to communicate with each other is handled by the
magic cookie system, see section [Distributed Erlang](`e:system:distributed.md`)
in the Erlang Reference Manual.

> #### Warning {: .warning }
>
> Starting a distributed node without also specifying
> [`-proto_dist inet_tls`](`e:erts:erl_cmd.md#proto_dist`) will expose the node
> to attacks that may give the attacker complete access to the node and in
> extension the cluster. When using un-secure distributed nodes, make sure that
> the network is configured to keep potential attackers out. See the
> [Using SSL for Erlang Distribution](`e:ssl:ssl_distribution.md`) User's Guide
> for details on how to setup a secure distributed node.

# `connection_state`
*not exported* 

```erlang
-type connection_state() :: check_pending | pending | up | up_pending.
```

# `connection_type`
*not exported* 

```erlang
-type connection_type() :: normal | hidden.
```

# `allow`

```erlang
-spec allow(Nodes) -> ok | error | ignored when Nodes :: [node()].
```

Permits access to the specified set of nodes.

Before the first call to [`allow/1`](`allow/1`), any node with the correct
cookie can be connected. When [`allow/1`](`allow/1`) is called, a list of
allowed nodes is established. Any access attempts made from (or to) nodes not in
that list will be rejected.

Subsequent calls to [`allow/1`](`allow/1`) will add the specified nodes to the
list of allowed nodes. It is not possible to remove nodes from the list.

Disallowing an already connected node will not cause it to be disconnected. It
will, however, prevent any future reconnection attempts.

Passing `Nodes` as an empty list has never any affect at all.

Returns `error` if any element in `Nodes` is not an atom, and `ignored` if the
local node is not alive.

# `allowed`
*since OTP 28.0* 

```erlang
-spec allowed() -> {ok, Nodes} | ignored when Nodes :: [node()].
```

Returns a list of nodes that are explicitly allowed to connect to the node by calling
[`allow/1`](`allow/1`). If empty list is returned, it means that any node using the
same cookie will be able to connect.

# `connect_node`

```erlang
-spec connect_node(Node) -> boolean() | ignored when Node :: node().
```

Establishes a connection to `Node`.

Returns `true` if a connection was established or was already established or if
`Node` is the local node itself. Returns `false` if the connection attempt failed,
and `ignored` if the local node is not alive.

# `get_net_ticktime`

```erlang
-spec get_net_ticktime() -> Res
                          when
                              Res :: NetTicktime | {ongoing_change_to, NetTicktime} | ignored,
                              NetTicktime :: pos_integer().
```

Returns currently used net tick time in seconds.

For more information see the [`net_ticktime`](kernel_app.md#net_ticktime)
`Kernel` parameter.

Defined return values (`Res`):

- **`NetTicktime`** - `net_ticktime` is `NetTicktime` seconds.

- **`{ongoing_change_to, NetTicktime}`** - `net_kernel` is currently changing
  `net_ticktime` to `NetTicktime` seconds.

- **`ignored`** - The local node is not alive.

# `get_state`
*since OTP 25.0* 

```erlang
-spec get_state() ->
                   #{started => no | static | dynamic,
                     name => atom(),
                     name_type => static | dynamic,
                     name_domain => shortnames | longnames}.
```

Get the current state of the distribution for the local node.

Returns a map with (at least) the following key-value pairs:

- **`started => Started`** - Valid values for `Started`:

  - **`no`** - The distribution is not started. In this state none of the other
    keys below are present in the map.

  - **`static`** - The distribution was started with command line arguments
    [`-name`](`e:erts:erl_cmd.md#name`) or
    [`-sname`](`e:erts:erl_cmd.md#sname`).

  - **`dynamic`** - The distribution was started with
    [`net_kernel:start/1`](`start/1`) and can be stopped with
    [`net_kernel:stop/0`](`start/1`).

- **`name => Name`** - The name of the node. Same as returned by `erlang:node/0`
  except when `name_type` is `dynamic` in which case `Name` may be `undefined`
  (instead of `nonode@nohost`).

- **`name_type => NameType`** - Valid values for `NameType`:

  - **`static`** - The node has a static node name set by the node itself.

  - **`dynamic`** - The distribution was started in
    [dynamic node name](`e:system:distributed.md#dyn_node_name`) mode, and will
    get its node name assigned from the first node it connects to. If key `name`
    has value `undefined` that has not happened yet.

- **`name_domain => NameDomain`** - Valid values for `NameDomain`:

  - **`shortnames`** - The distribution was started to use node names with a
    short host portion (not fully qualified).

  - **`longnames`** - The distribution was started to use node names with a long
    fully qualified host portion.

# `getopts`
*since OTP 19.1* 

```erlang
-spec getopts(Node, Options) -> {ok, OptionValues} | {error, Reason} | ignored
                 when
                     Node :: node(),
                     Options :: [inet:socket_getopt()],
                     OptionValues :: [inet:socket_setopt()],
                     Reason :: inet:posix() | noconnection.
```

Get one or more options for the distribution socket connected to `Node`.

If `Node` is a connected node the return value is the same as from
[`inet:getopts(Sock, Options)`](`inet:getopts/2`) where `Sock` is the
distribution socket for `Node`.

Returns `ignored` if the local node is not alive or `{error, noconnection}` if
`Node` is not connected.

# `monitor_nodes`

```erlang
-spec monitor_nodes(Flag) -> ok | Error when Flag :: boolean(), Error :: error | {error, term()}.
```

# `monitor_nodes`

```erlang
-spec monitor_nodes(Flag, Options) -> ok | Error
                       when
                           Flag :: boolean(),
                           Options :: OptionsList | OptionsMap,
                           OptionsList :: [ListOption],
                           ListOption :: connection_id | {node_type, NodeType} | nodedown_reason,
                           OptionsMap ::
                               #{connection_id => boolean(),
                                 node_type => NodeType,
                                 nodedown_reason => boolean()},
                           NodeType :: visible | hidden | all,
                           Error :: error | {error, term()}.
```

The calling process subscribes or unsubscribes to node status change messages. A
`nodeup` message is delivered to all subscribing processes when a new node is
connected, and a `nodedown` message is delivered when a node is disconnected.

If `Flag` is `true`, a new subscription is started. If `Flag` is `false`, all
previous subscriptions started with the same `Options` are stopped. Two option
lists are considered the same if they contain the same set of options.

Delivery guarantees of `nodeup`/`nodedown` messages:

- `nodeup` messages are delivered before delivery of any signals from the remote
  node through the newly established connection.
- `nodedown` messages are delivered after all the signals from the remote node
  over the connection have been delivered.
- `nodeup` messages are delivered after the corresponding node appears in
  results from `erlang:nodes()`.
- `nodedown` messages are delivered after the corresponding node has disappeared
  in results from `erlang:nodes()`.
- As of OTP 23.0, a `nodedown` message for a connection being taken down will be
  delivered before a `nodeup` message due to a new connection to the same node.
  Prior to OTP 23.0, this was not guaranteed to be the case.

The format of the node status change messages depends on `Options`. If `Options`
is the empty list or if `net_kernel:monitor_nodes/1` is called, the format is as
follows:

```erlang
{nodeup, Node} | {nodedown, Node}
  Node = node()
```

When `Options` is the empty map or empty list, the caller will only subscribe
for status change messages for visible nodes. That is, only nodes that appear in
the result of `erlang:nodes/0`.

If `Options` equals anything other than the empty list, the format of the status
change messages is as follows:

```erlang
{nodeup, Node, Info} | {nodedown, Node, Info}
  Node = node()
  Info = #{Tag => Val} | [{Tag, Val}]
```

`Info` is either a map or a list of 2-tuples. Its content depends on `Options`.
If `Options` is a map, `Info` will also be a map. If `Options` is a list, `Info`
will also be a list.

When `Options` is a map, currently the following associations are allowed:

- **`connection_id => boolean()`** - If the value of the association equals
  `true`, a `connection_id => ConnectionId` association will be included in the
  `Info` map where `ConnectionId` is the connection identifier of the connection
  coming up or going down. For more info about this connection identifier see
  the documentation of [erlang:nodes/2](`m:erlang#connection_id`).

- **`node_type => NodeType`** - Valid values for `NodeType`:

  - **`visible`** - Subscribe to node status change messages for visible nodes
    only. The association `node_type => visible` will be included in the `Info`
    map.

  - **`hidden`** - Subscribe to node status change messages for hidden nodes
    only. The association `node_type => hidden` will be included in the `Info`
    map.

  - **`all`** - Subscribe to node status change messages for both visible and
    hidden nodes. The association `node_type => visible | hidden` will be
    included in the `Info` map.

  If no `node_type => NodeType` association is included in the `Options` map,
  the caller will subscribe for status change messages for visible nodes only,
  but _no_ `node_type => visible` association will be included in the `Info`
  map.

- **`nodedown_reason => boolean()`** - If the value of the association equals
  `true`, a `nodedown_reason => Reason` association will be included in the
  `Info` map for `nodedown` messages.

  [](){: #nodedown_reasons } `Reason` can, depending on which distribution
  module or process that is used, be any term, but for the standard TCP
  distribution module it is one of the following:

  - **`connection_setup_failed`** - The connection setup failed (after `nodeup`
    messages were sent).

  - **`no_network`** - No network is available.

  - **`net_kernel_terminated`** - The `net_kernel` process terminated.

  - **`shutdown`** - Unspecified connection shutdown.

  - **`connection_closed`** - The connection was closed.

  - **`disconnect`** - The connection was disconnected (forced from the current
    node).

  - **`net_tick_timeout`** - Net tick time-out.

  - **`send_net_tick_failed`** - Failed to send net tick over the connection.

  - **`get_status_failed`** - Status information retrieval from the `Port`
    holding the connection failed.

When `Options` is a list, currently `ListOption` can be one of the following:

- **`connection_id`** - A `{connection_id, ConnectionId}` tuple will be included
  in `Info` where `ConnectionId` is the connection identifier of the connection
  coming up or going down. For more info about this connection identifier see
  the documentation of [erlang:nodes/2](`m:erlang#connection_id`).

- **`{node_type, NodeType}`** - Valid values for `NodeType`:

  - **`visible`** - Subscribe to node status change messages for visible nodes
    only. The tuple `{node_type, visible}` will be included in the `Info` list.

  - **`hidden`** - Subscribe to node status change messages for hidden nodes
    only. The tuple `{node_type, hidden}` will be included in the `Info` list.

  - **`all`** - Subscribe to node status change messages for both visible and
    hidden nodes. The tuple `{node_type, visible | hidden}` will be included in
    the `Info` list.

  If no `{node_type, NodeType}` option has been given. The caller will subscribe
  for status change messages for visible nodes only, but _no_
  `{node_type, visible}` tuple will be included in the `Info` list.

- **`nodedown_reason`** - The tuple `{nodedown_reason, Reason}` will be included
  in the `Info` list for `nodedown` messages.

  See the documentation of the
  [`nodedown_reason => boolean()`](`m:net_kernel#nodedown_reasons`) association
  above for information about possible `Reason` values.

Example:

```erlang
(a@localhost)1> net_kernel:monitor_nodes(true, #{connection_id=>true, node_type=>all, nodedown_reason=>true}).
ok
(a@localhost)2> flush().
Shell got {nodeup,b@localhost,
                  #{connection_id => 3067552,node_type => visible}}
Shell got {nodeup,c@localhost,
                  #{connection_id => 13892107,node_type => hidden}}
Shell got {nodedown,b@localhost,
                    #{connection_id => 3067552,node_type => visible,
                      nodedown_reason => connection_closed}}
Shell got {nodedown,c@localhost,
                    #{connection_id => 13892107,node_type => hidden,
                      nodedown_reason => net_tick_timeout}}
Shell got {nodeup,b@localhost,
                  #{connection_id => 3067553,node_type => visible}}
ok
(a@localhost)3>
```

# `set_net_ticktime`

```erlang
-spec set_net_ticktime(NetTicktime) -> Res
                          when
                              NetTicktime :: pos_integer(),
                              Res :: unchanged | change_initiated | {ongoing_change_to, NewNetTicktime},
                              NewNetTicktime :: pos_integer().
```

# `set_net_ticktime`

```erlang
-spec set_net_ticktime(NetTicktime, TransitionPeriod) -> Res
                          when
                              NetTicktime :: pos_integer(),
                              TransitionPeriod :: non_neg_integer(),
                              Res :: unchanged | change_initiated | {ongoing_change_to, NewNetTicktime},
                              NewNetTicktime :: pos_integer().
```

Sets `net_ticktime` (see [`kernel(6)`](kernel_app.md)) to `NetTicktime` seconds.
`TransitionPeriod` defaults to `60`.

Some definitions:

- **Minimum transition traffic interval (`MTTI`)** -
  `minimum(NetTicktime, PreviousNetTicktime)*1000 div 4` milliseconds.

- **Transition period** - The time of the least number of consecutive `MTTI`s to
  cover `TransitionPeriod` seconds following the call to
  [`set_net_ticktime/2`](`set_net_ticktime/2`) (that is,
  ((`TransitionPeriod*1000 - 1) div MTTI + 1)*MTTI` milliseconds).

If `NetTicktime < PreviousNetTicktime`, the `net_ticktime` change is done at the
end of the transition period; otherwise at the beginning. During the transition
period, `net_kernel` ensures that there is outgoing traffic on all connections
at least every `MTTI` millisecond.

> #### Note {: .info }
>
> The `net_ticktime` changes must be initiated on all nodes in the network (with
> the same `NetTicktime`) before the end of any transition period on any node;
> otherwise connections can erroneously be disconnected.

Returns one of the following:

- **`unchanged`** - `net_ticktime` already has the value of `NetTicktime` and is
  left unchanged.

- **`change_initiated`** - `net_kernel` initiated the change of `net_ticktime`
  to `NetTicktime` seconds.

- **`{ongoing_change_to, NewNetTicktime}`** - The request is _ignored_ because
  `net_kernel` is busy changing `net_ticktime` to `NewNetTicktime` seconds.

# `setopts`
*since OTP 19.1* 

```erlang
-spec setopts(Node, Options) -> ok | {error, Reason} | ignored
                 when
                     Node :: node() | new,
                     Options :: [inet:socket_setopt()],
                     Reason :: inet:posix() | noconnection.
```

Set one or more options for distribution sockets. Argument `Node` can be either
one node name or the atom `new` to affect the distribution sockets of all future
connected nodes.

The return value is the same as from `inet:setopts/2` or `{error, noconnection}`
if `Node` is not a connected node or `new`.

If `Node` is `new` the `Options` will then also be added to kernel configuration
parameters [inet_dist_listen_options](kernel_app.md#inet_dist_listen_options)
and [inet_dist_connect_options](kernel_app.md#inet_dist_connect_options).

Returns `ignored` if the local node is not alive.

# `start`

> This function is deprecated. Use start/2 instead.

```erlang
-spec start(Options) -> {ok, pid()} | {error, Reason}
               when
                   Options :: [Name | NameDomain | TickTime, ...],
                   Name :: atom(),
                   NameDomain :: shortnames | longnames,
                   TickTime :: pos_integer(),
                   Reason :: {already_started, pid()} | term().
```

Turns a non-distributed node into a distributed node by starting `net_kernel`
and other necessary processes.

`Options` list can only be exactly one of the following lists (order is
imporant):

- **`[Name]`** - The same as `net_kernel:start([Name, longnames, 15000])`.

- **`[Name, NameDomain]`** - The same as
  `net_kernel:start([Name, NameDomain, 15000])`.

- **`[Name, NameDomain, TickTime]`** - The same as
  [`net_kernel:start(Name, #{name_domain => NameDomain, net_ticktime => ((TickTime*4-1) div 1000) + 1, net_tickintensity => 4})`](`start/2`).
  Note that `TickTime` is _not_ the same as net tick time expressed in
  milliseconds. `TickTime` is the time between ticks when net tick intensity
  equals `4`.

# `start`
*since OTP 24.3* 

```erlang
-spec start(Name, Options) -> {ok, pid()} | {error, Reason}
               when
                   Options ::
                       #{name_domain => NameDomain,
                         net_ticktime => NetTickTime,
                         net_tickintensity => NetTickIntensity,
                         dist_listen => boolean(),
                         hidden => boolean()},
                   Name :: atom(),
                   NameDomain :: shortnames | longnames,
                   NetTickTime :: pos_integer(),
                   NetTickIntensity :: 4..1000,
                   Reason :: {already_started, pid()} | term().
```

Turns a non-distributed node into a distributed node by starting `net_kernel`
and other necessary processes.

If `Name` is set to _`undefined`_ the distribution will be started to request a
dynamic node name from the first node it connects to. See
[Dynamic Node Name](`e:system:distributed.md#dyn_node_name`). Setting `Name` to
`undefined` implies options `dist_listen => false` and `hidden => true`.

Currently supported options:

- **`name_domain => NameDomain`** - Determines the host name part of the node
  name. If `NameDomain` equals `longnames`, fully qualified domain names will be
  used which also is the default. If `NameDomain` equals `shortnames`, only the
  short name of the host will be used.

- **`net_ticktime => NetTickTime`** - _Net tick time_ to use in seconds.
  Defaults to the value of the [`net_ticktime`](kernel_app.md#net_ticktime)
  `kernel(6)` parameter. For more information about _net tick time_, see the
  `kernel` parameter. However, note that if the value of the `kernel` parameter
  is invalid, it will silently be replaced by a valid value, but if an invalid
  `NetTickTime` value is passed as option value to this function, the call will
  fail.

- **`net_tickintensity => NetTickIntensity`** - _Net tick intensity_ to use.
  Defaults to the value of the
  [`net_tickintensity`](kernel_app.md#net_tickintensity) `kernel(6)` parameter.
  For more information about _net tick intensity_, see the `kernel` parameter.
  However, note that if the value of the `kernel` parameter is invalid, it will
  silently be replaced by a valid value, but if an invalid `NetTickIntensity`
  value is passed as option value to this function, the call will fail.

- **`dist_listen => boolean()`** - Enable or disable listening for incoming
  connections. Defaults to the value of the
  [`-dist_listen`](`e:erts:erl_cmd.md#dist_listen`) `erl` command line argument.
  Note that `dist_listen => false` implies `hidden => true`.

  If `undefined` has been passed as `Name`, the `dist_listen` option will be
  overridden with `dist_listen => false`.

- **`hidden => boolean()`** - Enable or disable hidden node. Defaults to `true`
  if the [`-hidden`](`e:erts:erl_cmd.md#hidden`) `erl` command line argument has
  been passed; otherwise `false`.

  If `undefined` has been passed as `Name`, or the option `dist_listen` equals
  `false`, the `hidden` option will be overridden with `hidden => true`.

# `stop`

```erlang
-spec stop() -> ok | {error, Reason} when Reason :: not_allowed | not_found.
```

Turns a distributed node into a non-distributed node.

For other nodes in the network, this is the same as the node going down.
Only possible when the net kernel was started using `start/2`, otherwise
`{error, not_allowed}` is returned. Returns `{error, not_found}` if the local
node is not alive.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
