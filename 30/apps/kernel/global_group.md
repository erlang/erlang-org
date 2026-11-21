# `global_group`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/global_group.erl#L22)

Grouping nodes to global name registration groups.

This module makes it possible to partition the nodes of a system into _global
groups_. Each global group has its own global namespace, see `m:global`.

The main advantage of dividing systems into global groups is that the background
load decreases while the number of nodes to be updated is reduced when
manipulating globally registered names.

The Kernel configuration parameter [`global_groups`](kernel_app.md#global_groups)
defines the global groups:

```erlang
{global_groups, [GroupTuple :: group_tuple()]}
```

For the processes and nodes to run smoothly using the global group
functionality, the following criteria must be met:

- An instance of the global group server, `global_group`, must be running on
  each node. The processes are automatically started and synchronized when a
  node is started.
- All involved nodes must agree on the global group definition, otherwise the
  behavior of the system is undefined.
- _All_ nodes in the system must belong to exactly one global group.

In the following descriptions, a _group node_ is a node belonging to the same
global group as the local node.

## Notes

- In the situation where a node has lost its connections to other nodes in its
  global group, but has connections to nodes in other global groups, a request
  from another global group can produce an incorrect or misleading result. For
  example, the isolated node can have inaccurate information about registered
  names in its global group.
- Function [`send/2,3`](`send/2`) is not secure.
- Distribution of applications is highly dependent of the global group
  definitions. It is not recommended that an application is distributed over
  many global groups, as the registered names can be moved to another global
  group at failover/takeover. Nothing prevents this to be done, but the
  application code must then handle the situation.

### See Also

`m:global`, [`erl`](`e:erts:erl_cmd.md`)

# `group_name`
*not exported* 

```erlang
-type group_name() :: atom().
```

# `group_tuple`
*not exported* 

```erlang
-type group_tuple() ::
          {GroupName :: group_name(), [node()]} |
          {GroupName :: group_name(), PublishType :: publish_type(), [node()]}.
```

A `GroupTuple` without `PublishType` is the same as a `GroupTuple` with
`PublishType` equal to `normal`.

# `info_item`
*not exported* 

```erlang
-type info_item() ::
          {state, State :: sync_state()} |
          {own_group_name, GroupName :: group_name()} |
          {own_group_nodes, Nodes :: [node()]} |
          {synched_nodes, Nodes :: [node()]} |
          {sync_error, Nodes :: [node()]} |
          {no_contact, Nodes :: [node()]} |
          {other_groups, Groups :: [group_tuple()]} |
          {monitoring, Pids :: [pid()]}.
```

# `name`
*not exported* 

```erlang
-type name() :: atom().
```

A registered name.

# `publish_type`
*not exported* 

```erlang
-type publish_type() :: hidden | normal.
```

A node started with command-line flag `-hidden` (see
[`erl`](`e:erts:erl_cmd.md`)) is said to be a _hidden_ node. A hidden node
establishes hidden connections to nodes not part of the same global group, but
normal (visible) connections to nodes part of the same global group.

A global group defined with `PublishType` equal to `hidden` is said to be a
hidden global group. All nodes in a hidden global group are hidden nodes,
whether they are started with command-line flag `-hidden` or not.

# `sync_state`
*not exported* 

```erlang
-type sync_state() :: no_conf | synced.
```

# `where`
*not exported* 

```erlang
-type where() :: {node, node()} | {group, group_name()}.
```

# `global_groups`

```erlang
-spec global_groups() -> {GroupName, GroupNames} | undefined
                       when GroupName :: group_name(), GroupNames :: [GroupName].
```

Returns a tuple containing the name of the global group that the local node
belongs to, and the list of all other known group names.

Returns `undefined` if no global groups are defined.

# `info`

```erlang
-spec info() -> [info_item()].
```

Returns a list containing information about the global groups. Each list element
is a tuple. The order of the tuples is undefined.

- **`{state, State}`** - If the local node is part of a global group, `State` is
  equal to `synced`. If no global groups are defined, `State` is equal to
  `no_conf`.

- **`{own_group_name, GroupName}`** - The name (atom) of the group that the
  local node belongs to.

- **`{own_group_nodes, Nodes}`** - A list of node names (atoms), the group
  nodes.

- **`{synced_nodes, Nodes}`** - A list of node names, the group nodes currently
  synchronized with the local node.

- **`{sync_error, Nodes}`** - A list of node names, the group nodes with which
  the local node has failed to synchronize.

- **`{no_contact, Nodes}`** - A list of node names, the group nodes to which
  there are currently no connections.

- **`{other_groups, Groups}`** - `Groups` is a list of tuples
  `{GroupName, Nodes}`, specifying the name and nodes of the other global
  groups.

- **`{monitoring, Pids}`** - A list of pids, specifying the processes that have
  subscribed to `nodeup` and `nodedown` messages.

# `monitor_nodes`

```erlang
-spec monitor_nodes(Flag) -> ok when Flag :: boolean().
```

Alter the calling process' subscription of node status change messages.

If `Flag` is equal to `true` the calling process starts subscribing to
node status change messages. If equal to `false` it stops subscribing.

A process that has subscribed receives the messages `{nodeup, Node}` and
`{nodedown, Node}` when a group node connects or disconnects, respectively.

# `own_nodes`

```erlang
-spec own_nodes() -> Nodes when Nodes :: [Node :: node()].
```

Returns the names of all group nodes, regardless of their current status.

# `registered_names`

```erlang
-spec registered_names(Where) -> Names when Where :: where(), Names :: [Name :: name()].
```

Returns a list of all names that are globally registered on the specified node
or in the specified global group.

# `send`

```erlang
-spec send(Name, Msg) -> pid() | {badarg, {Name, Msg}} when Name :: name(), Msg :: term().
```

Sends `Msg` to the pid represented by the globally registered name `Name`.

`send/2` searches for `Name` any any global group. The global groups are searched
in the order that they appear in the value of configuration parameter
[`global_groups`](kernel_app.md#global_groups).

If `Name` is found, message `Msg` is sent to the corresponding pid. The pid is
also the return value of the function. If the name is not found, the function
returns `{badarg, {Name, Msg}}`.

# `send`

```erlang
-spec send(Where, Name, Msg) -> pid() | {badarg, {Name, Msg}}
              when Where :: where(), Name :: name(), Msg :: term().
```

Equivalent to [`send(Name, Msg)`](`send/2`) except that he search is limited
to the node or global group specified by `Where`.

# `sync`

```erlang
-spec sync() -> ok.
```

Synchronizes the group nodes, that is, the global name servers on the group
nodes. Also checks the names globally registered in the current global group and
unregisters them on any known node not part of the group.

If synchronization is not possible, an error report is sent to the error logger
(see also `m:error_logger`.

Returns `{error, {'invalid global_groups definition', Bad}}` if configuration
parameter `global_groups` has an invalid value `Bad`.

# `whereis_name`

```erlang
-spec whereis_name(Name) -> pid() | undefined when Name :: name().
```

Searched for `Name` in any global group.

The global groups are searched in the order that they appear in the value
of configuration parameter `global_groups`.

If `Name` is found, the corresponding pid is returned. If the name is not found,
the function returns `undefined`.

# `whereis_name`

```erlang
-spec whereis_name(Where, Name) -> pid() | undefined when Where :: where(), Name :: name().
```

Equivalent to [`whereis_name(Name)`](`whereis_name/1`) except that he search is limited
to the node or global group specified by `Where`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
