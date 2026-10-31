# `supervisor_bridge`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/supervisor_bridge.erl#L22)

Generic supervisor bridge behavior.

This behavior module provides a supervisor bridge, a process that connects a
subsystem not designed according to the OTP design principles to a supervision
tree. The supervisor bridge sits between a supervisor and the subsystem. It
behaves like a real supervisor to its own supervisor, but has a different
interface than a real supervisor to the subsystem. For more information, see
[Supervisor Behaviour](`e:system:sup_princ.md`) in OTP Design Principles.

A supervisor bridge assumes the functions for starting and stopping the
subsystem to be located in a callback module exporting a predefined set of
functions.

The `m:sys` module can be used for debugging a supervisor bridge.

Unless otherwise stated, all functions in this module fail if the specified
supervisor bridge does not exist or if bad arguments are specified.

### See Also

`m:supervisor`, `m:sys`

# `init`

```erlang
-callback init(Args :: term()) -> {ok, Pid :: pid(), State :: term()} | ignore | {error, Error :: term()}.
```

Whenever a supervisor bridge is started using
[`start_link/2,3`](`start_link/2`), this function is called by the new process
to start the subsystem and initialize.

`Args` is the `Args` argument provided to the start function.

The function is to return `{ok,Pid,State}`, where `Pid` is the pid of the main
process in the subsystem and `State` is any term.

If later `Pid` terminates with a reason `Reason`, the supervisor bridge
terminates with reason `Reason` as well. If later the supervisor bridge is
stopped by its supervisor with reason `Reason`, it calls
[`Module:terminate(Reason,State)`](`c:terminate/2`) to terminate.

If the initialization fails, the function is to return `{error,Error}`, where
`Error` is any term, or `ignore`.

# `terminate`

```erlang
-callback terminate(Reason :: shutdown | term(), State :: term()) -> Ignored :: term().
```

This function is called by the supervisor bridge when it is about to terminate.
It is to be the opposite of [`Module:init/1`](`c:init/1`) and stop the subsystem
and do any necessary cleaning up. The return value is ignored.

`Reason` is `shutdown` if the supervisor bridge is terminated by its supervisor.
If the supervisor bridge terminates because a linked process (apart from the
main process of the subsystem) has terminated with reason `Term`, then `Reason`
becomes `Term`.

`State` is taken from the return value of [`Module:init/1`](`c:init/1`).

# `start_link`

```erlang
-spec start_link(Module, Args) -> Result
                    when
                        Module :: module(),
                        Args :: term(),
                        Result :: {ok, Pid} | ignore | {error, Error},
                        Error :: {already_started, Pid} | term(),
                        Pid :: pid().
```

Creates a nameless supervisor bridge process as part of a supervision tree.

Equivalent to `start_link/3` except that the supervisor process is not
[`registered`](`erlang:register/2`).

# `start_link`

```erlang
-spec start_link(SupBridgeName, Module, Args) -> Result
                    when
                        SupBridgeName :: {local, Name} | {global, GlobalName} | {via, Module, ViaName},
                        Name :: atom(),
                        GlobalName :: term(),
                        ViaName :: term(),
                        Module :: module(),
                        Args :: term(),
                        Result :: {ok, Pid} | ignore | {error, Error},
                        Error :: {already_started, Pid} | term(),
                        Pid :: pid().
```

Creates a supervisor bridge process, linked to the calling process, which calls
[`Module:init/1`](`c:init/1`) to start the subsystem.

To ensure a synchronized startup procedure, this function does not return until
[`Module:init/1`](`c:init/1`) has returned.

- If `SupBridgeName={local,Name}`, the supervisor bridge is registered locally
  as `Name` using [`register/2`](`register/2`).
- If `SupBridgeName={global,GlobalName}`, the supervisor bridge is registered
  globally as `GlobalName` using `global:register_name/2`.
- If `SupBridgeName={via,Module,ViaName}`, the supervisor bridge is registered
  as `ViaName` using a registry represented by Module. The `Module` callback is
  to export functions `register_name/2`, `unregister_name/1`, and `send/2`,
  which are to behave like the corresponding functions in `m:global`. Thus,
  `{via,global,GlobalName}` is a valid reference.

`Module` is the name of the callback module.

`Args` is an arbitrary term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

- If the supervisor bridge and the subsystem are successfully started, the
  function returns `{ok,Pid}`, where `Pid` is is the pid of the supervisor
  bridge.
- If there already exists a process with the specified `SupBridgeName`, the
  function returns `{error,{already_started,Pid}}`, where `Pid` is the pid of
  that process.
- If [`Module:init/1`](`c:init/1`) returns `ignore`, this function returns
  `ignore` as well and the supervisor bridge terminates with reason `normal`.
- If [`Module:init/1`](`c:init/1`) fails or returns an error tuple or an
  incorrect value, this function returns `{error,Error}`, where `Error` is a
  term with information about the error, and the supervisor bridge terminates
  with reason `Error`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
