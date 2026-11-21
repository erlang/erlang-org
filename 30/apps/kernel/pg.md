# `pg`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/pg.erl#L60)

Distributed named process groups.

This module implements process groups. A message can be sent to one, some, or
all group members.

Up until OTP 17 there used to exist an experimental `pg` module in `stdlib`.
This `pg` module is not the same module as that experimental `pg` module, and
only share the same module name.

A group of processes can be accessed by a common name. For example, if there is
a group named `foobar`, there can be a set of processes (which can be located on
different nodes) that are all members of the group `foobar`. There are no
special functions for sending a message to the group. Instead, client functions
are to be written with the functions `get_members/1` and `get_local_members/1`
to determine which processes are members of the group. Then the message can be
sent to one or more group members.

If a member terminates, it is automatically removed from the group.

A process may join multiple groups. It may join the same group multiple times.
It is only allowed to join processes running on local node.

Process Groups implement strong eventual consistency. Process Groups membership
view may temporarily diverge. For example, when processes on `node1` and `node2`
join concurrently, `node3` and `node4` may receive updates in a different order.

Membership view is not transitive. If `node1` is not directly connected to
`node2`, they will not see each other's groups. But if both are connected to
`node3`, `node3` will have the full view.

Groups are automatically created when any process joins, and are removed when
all processes leave the group. Non-existing group is considered empty
(containing no processes).

Process groups can be organised into multiple scopes. Scopes are completely
independent of each other. A process may join any number of groups in any number
of scopes. Scopes are designed to decouple single mesh into a set of overlay
networks, reducing amount of traffic required to propagate group membership
information. Default scope `pg` is started automatically when
[Kernel](kernel_app.md#start_pg) is configured to do so.

> #### Note {: .info }
>
> Scope name is used to register process locally, and to name an ETS table. If
> there is another process registered under this name, or another ETS table
> exists, scope fails to start.
>
> Local membership is not preserved if scope process exits and restarts.
>
> A scope can be kept local-only by using a scope name that is unique
> cluster-wide, e.g. the node name: `pg:start_link(node()).`

### See Also

[Kernel](kernel_app.md)

# `global_view`
*not exported* *since OTP 23.0* 

```erlang
-type global_view() :: {atom(), #{pid() => {reference(), [group()]}}}.
```

# `group`
*not exported* *since OTP 23.0* 

```erlang
-type group() :: term().
```

The identifier of a process group.

# `local_data`
*not exported* *since OTP 23.0* 

```erlang
-type local_data() :: #{group() => [pid()]}.
```

# `options`
*not exported* *since OTP 23.0* 

```erlang
-type options() :: map().
```

# `scope`
*not exported* *since OTP 23.0* 

```erlang
-type scope() :: atom().
```

# `subscribe_result`
*not exported* *since OTP 23.0* 

```erlang
-type subscribe_result() :: #{group() => [pid()]} | [pid()].
```

# `subscription`
*not exported* *since OTP 23.0* 

```erlang
-type subscription() :: scope | {group, group()}.
```

# `subscriptions`
*not exported* *since OTP 23.0* 

```erlang
-type subscriptions() :: #{subscription() => #{reference() => pid()}}.
```

# `update`
*not exported* *since OTP 23.0* 

```erlang
-type update() :: [{group(), Add :: [pid()], Remove :: [pid()]}].
```

# `version`
*not exported* *since OTP 23.0* 

```erlang
-type version() :: 0 | 1.
```

# `data_diff`
*since OTP 29.0* 

```erlang
-spec data_diff(Old :: local_data(), New :: local_data()) -> update().
```

# `demonitor`
*since OTP 25.1* 

```erlang
-spec demonitor(Ref :: reference()) -> ok | false.
```

# `demonitor`
*since OTP 25.1* 

```erlang
-spec demonitor(Scope :: atom(), Ref :: reference()) -> ok | false.
```

Unsubscribes the caller from updates (scope or group). Flushes all outstanding
updates that were already in the message queue of the calling process.

# `get_local_members`
*since OTP 23.0* 

```erlang
-spec get_local_members(Group :: group()) -> [pid()].
```

# `get_local_members`
*since OTP 23.0* 

```erlang
-spec get_local_members(Scope :: atom(), Group :: group()) -> [pid()].
```

Returns all processes running on the local node in the group `Group`. Processes
are returned in no specific order. This function is optimised for speed.

# `get_members`
*since OTP 23.0* 

```erlang
-spec get_members(Group :: group()) -> [pid()].
```

# `get_members`
*since OTP 23.0* 

```erlang
-spec get_members(Scope :: atom(), Group :: group()) -> [pid()].
```

Returns all processes in the group `Group`. Processes are returned in no
specific order. This function is optimised for speed.

# `init_global_view`
*since OTP 29.0* 

```erlang
-spec init_global_view(scope(), options()) -> global_view().
```

# `init_local_data`
*since OTP 29.0* 

```erlang
-spec init_local_data(options()) -> local_data().
```

# `join`
*since OTP 23.0* 

```erlang
-spec join(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
```

# `join`
*since OTP 23.0* 

```erlang
-spec join(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok.
```

Joins single process or multiple processes to the group `Group`. A process can
join a group many times and must then leave the group the same number of times.

`PidOrPids` may contain the same process multiple times.

# `leave`
*since OTP 23.0* 

```erlang
-spec leave(Group :: group(), PidOrPids :: pid() | [pid()]) -> ok | not_joined.
```

# `leave`
*since OTP 23.0* 

```erlang
-spec leave(Scope :: atom(), Group :: group(), PidOrPids :: pid() | [pid()]) -> ok | not_joined.
```

Makes the process `PidOrPids` leave the group `Group`. If the process is not a
member of the group, `not_joined` is returned.

When list of processes is passed as `PidOrPids`, function returns `not_joined`
only when all processes of the list are not joined.

# `monitor`
*since OTP 25.1* 

```erlang
-spec monitor(Group :: group()) -> {reference(), [pid()]}.
```

# `monitor`
*since OTP 25.1* 

```erlang
-spec monitor(Scope :: atom(), Group :: group()) -> {reference(), [pid()]}.
```

Subscribes the caller to updates for the specified group.

Returns list of processes currently in the group, and a reference to match the
upcoming notifications.

See `monitor_scope/0` for the update message structure.

# `monitor_scope`
*since OTP 25.1* 

```erlang
-spec monitor_scope() -> {reference(), #{group() => [pid()]}}.
```

# `monitor_scope`
*since OTP 25.1* 

```erlang
-spec monitor_scope(Scope :: atom()) -> {reference(), #{group() => [pid()]}}.
```

Subscribes the caller to updates from the specified scope.

Returns content of the entire scope and a reference to match the upcoming
notifications.

Whenever any group membership changes, an update message is sent to the
subscriber:

```erlang
{Ref, join, Group, [JoinPid1, JoinPid2]}
```

```erlang
{Ref, leave, Group, [LeavePid1]}
```

# `new_subscription`
*since OTP 29.0* 

```erlang
-spec new_subscription(subscription(), global_view()) -> subscribe_result().
```

# `start`
*since OTP 23.0* 

```erlang
-spec start(Scope :: atom()) -> {ok, pid()} | {error, term()}.
```

Starts additional scope.

# `start_link`
*since OTP 23.0* 

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

Starts the default `pg` scope within supervision tree.

Kernel may be configured to do it automatically by setting
the Kernel configuration parameter [`start_pg`](kernel_app.md#start_pg).

# `start_link`
*since OTP 23.0* 

```erlang
-spec start_link(Scope :: atom()) -> {ok, pid()} | {error, term()}.
```

Equivalent to [`start(Scope)`](`start/1`), except that it also creates
a `link/1` with the calling process.

# `stop_global_view`
*since OTP 29.0* 

```erlang
-spec stop_global_view(global_view()) -> term().
```

# `translate_local_data`
*since OTP 29.0* 

```erlang
-spec translate_local_data(MyVersion :: version(), PeerVersion :: version(), local_data()) ->
                              local_data() | {'$plain_message', dynamic()}.
```

# `translate_message`
*since OTP 29.0* 

```erlang
-spec translate_message({join, pid(), group(), pid() | [pid()]}, global_view()) ->
                           {update, pid(), update(), global_view()};
                       ({leave, pid(), pid() | [pid()], [group()]}, global_view()) ->
                           {update, pid(), update(), global_view()};
                       ({{'DOWN', pg}, reference(), process, pid(), dynamic()}, global_view()) ->
                           {update, update(), global_view()} | {drop, global_view()};
                       ({sync, pid(), [{group(), [pid()]}]}, global_view()) ->
                           {local_data, pid(), version(), local_data(), global_view()};
                       ({discover, pid()}, global_view()) -> {discover, pid(), version(), global_view()}.
```

# `translate_update`
*since OTP 29.0* 

```erlang
-spec translate_update(MyVersion :: version(), PeerVersion :: version(), update()) ->
                          update() | {'$plain_messages', [dynamic()]}.
```

# `update_global_view_and_notify`
*since OTP 29.0* 

```erlang
-spec update_global_view_and_notify(node(), update(), subscriptions(), global_view()) -> global_view().
```

# `update_local_data`
*since OTP 29.0* 

```erlang
-spec update_local_data(update(), local_data()) -> local_data().
```

# `version`
*since OTP 29.0* 

```erlang
-spec version() -> version().
```

# `which_groups`
*since OTP 23.0* 

```erlang
-spec which_groups() -> [Group :: group()].
```

# `which_groups`
*since OTP 23.0* 

```erlang
-spec which_groups(Scope :: atom()) -> [Group :: group()].
```

Returns a list of all known groups.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
