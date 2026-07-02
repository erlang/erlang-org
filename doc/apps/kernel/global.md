# `global`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/global.erl#L22)

A global name registration facility.

This module consists of the following services:

- Registration of global names
- Global locks
- Maintenance of the fully connected network

[](){: #prevent_overlapping_partitions }

As of OTP 25, `global` will by default prevent overlapping partitions due to
network issues by actively disconnecting from nodes that reports that they have
lost connections to other nodes. This will cause fully connected partitions to
form instead of leaving the network in a state with overlapping partitions.

> #### Warning {: .warning }
>
> Prevention of overlapping partitions can be disabled using the
> [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
> Kernel parameter, making `global` behave like it used to do. This is,
> however, problematic for all applications expecting a fully connected network
> to be provided, such as for example `mnesia`, but also for `global` itself. A
> network of overlapping partitions might cause the internal state of `global`
> to become inconsistent. Such an inconsistency can remain even after such
> partitions have been brought together to form a fully connected network again.
> The effect on other applications that expects that a fully connected network
> is maintained may vary, but they might misbehave in very subtle hard to detect
> ways during such a partitioning. Since you might get hard to detect issues
> without this fix, you are _strongly_ advised _not_ to disable this fix. Also
> note that this fix _has_ to be enabled on _all_ nodes in the network in order
> to work properly.

> #### Note {: .info }
>
> None of the above services will be reliably delivered unless both of the
> kernel parameters [`connect_all`](kernel_app.md#connect_all) and
> [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
> are enabled. Calls to the `global` API will, however, _not_ fail even though
> one or both of them are disabled. You will just get unreliable results.

These services are controlled through the process `global_name_server` that
exists on every node. The global name server starts automatically when a node is
started. With the term _global_ is meant over a system consisting of many Erlang
nodes.

The ability to globally register names is a central concept in the programming
of distributed Erlang systems. In this module, the equivalent of the
[`register/2`](`register/2`) and [`whereis/1`](`whereis/1`) BIFs (for local name
registration) are provided, but for a network of Erlang nodes. A registered name
is an alias for a process identifier (pid). The global name server monitors
globally registered pids. If a process terminates, the name is also globally
unregistered.

The registered names are stored in replica global name tables on every node.
There is no central storage point. Thus, the translation of a name to a pid is
fast, as it is always done locally. For any action resulting in a change to the
global name table, all tables on other nodes are automatically updated.

Global locks have lock identities and are set on a specific resource. For
example, the specified resource can be a pid. When a global lock is set, access
to the locked resource is denied for all resources other than the lock
requester.

Both the registration and lock services are atomic. All nodes involved in these
actions have the same view of the information.

The global name server also performs the critical task of continuously
monitoring changes in node configuration. If a node that runs a globally
registered process goes down, the name is globally unregistered. To this end,
the global name server subscribes to `nodeup` and `nodedown` messages sent from
module `net_kernel`. Relevant Kernel application variables in this context are
[`net_setuptime`](kernel_app.md#net_setuptime), [`net_ticktime`](kernel_app.md#net_ticktime),
and [`dist_auto_connect`](kernel_app.md#dist_auto_connect).

The name server also maintains a fully connected network. For example, if node
`N1` connects to node `N2` (which is already connected to `N3`), the global name
servers on the nodes `N1` and `N3` ensure that also `N1` and `N3` are connected.
In this case, the name registration service cannot be used, but the lock
mechanism still works.

If the global name server fails to connect nodes (`N1` and `N3` in the example),
a warning event is sent to the error logger. The presence of such an event does
not exclude the nodes to connect later (you can, for example, try command
`rpc:call(N1, net_adm, ping, [N2])` in the Erlang shell), but it indicates a
network problem.

> #### Note {: .info }
>
> If the fully connected network is not set up properly, try first to increase
> the value of `net_setuptime`.

### See Also

`m:global_group`, `m:net_kernel`

# `id`
*not exported* 

```erlang
-type id() :: {ResourceId :: term(), LockRequesterId :: term()}.
```

A lock id used to set or delete lock `ResourceId` on behalf of `LockRequesterId`.

# `method`
*not exported* 

```erlang
-type method() :: fun((Name :: term(), Pid :: pid(), Pid2 :: pid()) -> pid() | none).
```

# `retries`
*not exported* 

```erlang
-type retries() :: non_neg_integer() | infinity.
```

# `trans_fun`
*not exported* 

```erlang
-type trans_fun() :: function() | {module(), atom()}.
```

# `del_lock`

```erlang
-spec del_lock(Id) -> true when Id :: id().
```

# `del_lock`

```erlang
-spec del_lock(Id, Nodes) -> true when Id :: id(), Nodes :: [node()].
```

Deletes the lock `Id` synchronously.

# `disconnect`
*since OTP 25.1* 

```erlang
-spec disconnect() -> [node()].
```

Disconnect from all other nodes known to `global`.

A list of node names (in an unspecified order) is returned which corresponds to
the nodes that were disconnected. All disconnect operations performed have completed when
`global:disconnect/0` returns.

The disconnects will be made in such a way that only the current node will be
removed from the cluster of `global` nodes. If
[`prevent_overlapping_partitions`] is
enabled and you disconnect, from other nodes in the cluster of `global` nodes,
by other means, `global` on the other nodes may partition the remaining nodes in
order to ensure that no overlapping partitions appear. Even if
[`prevent_overlapping_partitions`] is disabled, you should preferably use
`global:disconnect/0` in order to remove current node from a cluster of `global`
nodes, since you otherwise likely _will_ create overlapping partitions which
might [cause problems](`m:global#prevent_overlapping_partitions`).

Note that if the node is going to be halted, there is _no_ need to remove it
from a cluster of `global` nodes explicitly by calling `global:disconnect/0`
before halting it. The removal from the cluster is taken care of automatically
when the node halts regardless of whether [`prevent_overlapping_partitions`] is
enabled or not.

If current node has been configured to be part of a
[_global group_](`m:global_group`), only connected and/or synchronized nodes in
that group are known to `global`, so `global:disconnect/0` will _only_
disconnect from those nodes. If current node is _not_ part of a _global group_,
all [connected visible nodes](`erlang:nodes/0`) will be known to `global`, so
`global:disconnect/0` will disconnect from all those nodes.

Note that information about connected nodes does not instantaneously reach
`global`, so the caller might see a node part of the result returned by
[`nodes()`](`erlang:nodes/0`) while it still is not known to `global`. The
disconnect operation will, however, still not cause any overlapping partitions
when [`prevent_overlapping_partitions`] is enabled. If
[`prevent_overlapping_partitions`] is disabled, overlapping partitions might form
in this case.

Note that when [`prevent_overlapping_partitions`] is enabled, you may see warning
reports on other nodes when they detect that current node has disconnected.
These are in this case completely harmless and can be ignored.

[`prevent_overlapping_partitions`]: kernel_app.md#prevent_overlapping_partitions

# `notify_all_name`

```erlang
-spec notify_all_name(Name, Pid1, Pid2) -> none when Name :: term(), Pid1 :: pid(), Pid2 :: pid().
```

The function unregisters both pids and sends the message
`{global_name_conflict, Name, OtherPid}` to both processes.

Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

# `random_exit_name`

```erlang
-spec random_exit_name(Name, Pid1, Pid2) -> pid() when Name :: term(), Pid1 :: pid(), Pid2 :: pid().
```

The function randomly selects one of the pids for registration and kills the
other one.

Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

# `random_notify_name`

```erlang
-spec random_notify_name(Name, Pid1, Pid2) -> pid() when Name :: term(), Pid1 :: pid(), Pid2 :: pid().
```

The function randomly selects one of the pids for registration, and sends the
message `{global_name_conflict, Name}` to the other pid.

Can be used as a name resolving function for `register_name/3` and
`re_register_name/3`.

# `re_register_name`

```erlang
-spec re_register_name(Name, Pid) -> yes when Name :: term(), Pid :: pid().
```

# `re_register_name`

```erlang
-spec re_register_name(Name, Pid, Resolve) -> yes when Name :: term(), Pid :: pid(), Resolve :: method().
```

Atomically changes the registered name `Name` on all nodes to refer to `Pid`.

Function `Resolve` has the same behavior as in
[`register_name/2,3`](`register_name/2`).

# `register_name`

```erlang
-spec register_name(Name, Pid) -> yes | no when Name :: term(), Pid :: pid().
```

# `register_name`

```erlang
-spec register_name(Name, Pid, Resolve) -> yes | no
                       when Name :: term(), Pid :: pid(), Resolve :: method().
```

Globally associates name `Name` with a pid, that is, globally notifies all nodes
of a new global name in a network of Erlang nodes.

When new nodes are added to the network, they are informed of the globally
registered names that already exist. The network is also informed of any global
names in newly connected nodes. If any name clashes are discovered, function
`Resolve` is called. Its purpose is to decide which pid is correct. If the
function crashes, or returns anything other than one of the pids, the name is
unregistered. This function is called once for each name clash.

> #### Warning {: .warning }
>
> If you plan to change code without restarting your system, you must use an
> external fun (`fun Module:Function/Arity`) as function `Resolve`. If you use a
> local fun, you can never replace the code for the module that the fun belongs
> to.

Three predefined resolve functions exist:
[`random_exit_name/3`](`random_exit_name/3`),
[`random_notify_name/3`](`random_notify_name/3`), and
[`notify_all_name/3`](`notify_all_name/3`).

This function is completely synchronous, that is, when this function returns,
the name is either registered on all nodes or none.

The function returns `yes` if successful, `no` if it fails. For example, `no` is
returned if an attempt is made to register an already registered process or to
register a process with a name that is already in use.

> #### Note {: .info }
>
> Releases up to and including Erlang/OTP R10 did not check if the process was
> already registered. The global name table could therefore become inconsistent.
> The old (buggy) behavior can be chosen by giving the Kernel application
> variable `global_multi_name_action` the value `allow`.

If a process with a registered name dies, or the node goes down, the name is
unregistered on all nodes.

# `registered_names`

```erlang
-spec registered_names() -> [Name] when Name :: term().
```

Returns a list of all globally registered names.

# `send`

```erlang
-spec send(Name, Msg) -> Pid when Name :: term(), Msg :: term(), Pid :: pid().
```

Sends message `Msg` to the pid globally registered as `Name`.

If `Name` is not a globally registered name, the calling function exits with
reason `{badarg, {Name, Msg}}`.

# `set_lock`

```erlang
-spec set_lock(Id) -> boolean() when Id :: id().
```

# `set_lock`

```erlang
-spec set_lock(Id, Nodes) -> boolean() when Id :: id(), Nodes :: [node()].
```

# `set_lock`

```erlang
-spec set_lock(Id, Nodes, Retries) -> boolean() when Id :: id(), Nodes :: [node()], Retries :: retries().
```

Sets a lock on the specified nodes on using `t:id/0`.

If a lock already exists on `ResourceId` for another requester than `LockRequesterId`,
and `Retries` is not equal to `0`, the process sleeps for a while and tries to
execute the action later. When `Retries` attempts have been made, `false` is
returned, otherwise `true`. If `Retries` is `infinity`, `true` is eventually
returned (unless the lock is never released).

This function is completely synchronous.

If a process that holds a lock dies, or the node goes down, the locks held by
the process are deleted.

The global name server keeps track of all processes sharing the same lock, that
is, if two processes set the same lock, both processes must delete the lock.

This function does not address the problem of a deadlock. A deadlock can never
occur as long as processes only lock one resource at a time. A deadlock can
occur if some processes try to lock two or more resources. It is up to the
application to detect and rectify a deadlock.

> #### Note {: .info }
>
> Avoid the following values of `ResourceId`, otherwise Erlang/OTP does not work
> properly:
>
> - `dist_ac`
> - `global`
> - `mnesia_adjust_log_writes`
> - `mnesia_table_lock`

# `sync`

```erlang
-spec sync() -> ok | {error, Reason :: term()}.
```

Synchronizes the global name server with all nodes known to this node.

These are the nodes that are returned from [`nodes()`](`erlang:nodes/0`). When
this function returns, the global name server receives global information from
all nodes. This function can be called when new nodes are added to the network.

The only possible error reason `Reason` is
`{"global_groups definition error", Error}`.

# `trans`

```erlang
-spec trans(Id, Fun) -> Res | aborted when Id :: id(), Fun :: trans_fun(), Res :: term().
```

# `trans`

```erlang
-spec trans(Id, Fun, Nodes) -> Res | aborted
               when Id :: id(), Fun :: trans_fun(), Nodes :: [node()], Res :: term().
```

# `trans`

```erlang
-spec trans(Id, Fun, Nodes, Retries) -> Res | aborted
               when
                   Id :: id(),
                   Fun :: trans_fun(),
                   Nodes :: [node()],
                   Retries :: retries(),
                   Res :: term().
```

Sets a lock on `Id` (using `set_lock/3`).

If this succeeds, `Fun()` is evaluated and the result `Res` is returned.
Returns `aborted` if the lock attempt fails. If `Retries` is set to `infinity`,
the transaction does not abort.

`infinity` is the default setting and is used if no value is specified for
`Retries`.

# `unregister_name`

```erlang
-spec unregister_name(Name) -> _ when Name :: term().
```

Removes the globally registered name `Name` from the network of Erlang nodes.

# `whereis_name`

```erlang
-spec whereis_name(Name) -> pid() | undefined when Name :: term().
```

Returns the pid with the globally registered name `Name`. Returns `undefined` if
the name is not globally registered.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
