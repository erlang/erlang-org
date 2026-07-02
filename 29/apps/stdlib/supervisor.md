# `supervisor`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/supervisor.erl#L22)

Generic supervisor behavior.

This behavior module provides a supervisor, a process that supervises other
processes called child processes. A child process can either be another
supervisor or a worker process. Worker processes are normally implemented using
one of the `m:gen_event`, `m:gen_server`, or `m:gen_statem` behaviors. A
supervisor implemented using this module has a standard set of interface
functions and includes functionality for tracing and error reporting.
Supervisors are used to build a hierarchical process structure called a
supervision tree, a nice way to structure a fault-tolerant application. For more
information, see [Supervisor Behaviour](`e:system:sup_princ.md`) in OTP Design
Principles.

A supervisor expects the definition of which child processes to supervise to be
specified in a callback module exporting a predefined set of functions.

Unless otherwise stated, all functions in this module fail if the specified
supervisor does not exist or if bad arguments are specified.

[](){: #supervision_princ }

## Supervision Principles

The supervisor is responsible for starting, stopping, and monitoring its child
processes. The basic idea of a supervisor is that it must keep its child
processes alive by restarting them when necessary.

The children of a supervisor are defined as a list of _child specifications_.
When the supervisor is started, the child processes are started in order from
left to right according to this list. When the supervisor is going to terminate,
it first terminates its child processes in reversed start order, from right to
left.

[](){: #sup_flags }

### Supervisor flags

The supervisor properties are defined by the supervisor flags. The type
definition for the supervisor flags is as follows:

```erlang
sup_flags() = #{strategy => strategy(),           % optional
                intensity => non_neg_integer(),   % optional
                period => pos_integer(),          % optional
                hibernate_after => timeout(),     % optional, available since OTP 28.0
                auto_shutdown => auto_shutdown()} % optional
```

#### Restart Strategies

A supervisor can have one of the following _restart strategies_ specified with
the `strategy` key in the above map:

- `one_for_one` \- If one child process terminates and is to be restarted, only
  that child process is affected. This is the default restart strategy.
- `one_for_all` \- If one child process terminates and is to be restarted, all
  other child processes are terminated and then all child processes are
  restarted.
- `rest_for_one` \- If one child process terminates and is to be restarted, the
  'rest' of the child processes (that is, the child processes after the
  terminated child process in the start order) are terminated. Then the
  terminated child process and all child processes after it are restarted.
- `simple_one_for_one` \- A simplified `one_for_one` supervisor, where all child
  processes are dynamically added instances of the same process type, that is,
  running the same code.

  Functions `delete_child/2` and `restart_child/2` are invalid for
  `simple_one_for_one` supervisors and return `{error,simple_one_for_one}` if
  the specified supervisor uses this restart strategy.

  Function `terminate_child/2` can be used for children under
  `simple_one_for_one` supervisors by specifying the child's `t:pid/0` as the
  second argument. If instead the child specification identifier is used,
  [`terminate_child/2`](`terminate_child/2`) return
  `{error,simple_one_for_one}`.

  As a `simple_one_for_one` supervisor can have many children, it shuts them all
  down asynchronously. This means that the children do their cleanup in
  parallel, and therefore the order in which they are stopped is not defined.

#### Restart intensity and period

To prevent a supervisor from getting into an infinite loop of child process
terminations and restarts, a _maximum restart intensity_ is defined using two
integer values specified with keys `intensity` and `period` in the above map.
Assuming the values `MaxR` for `intensity` and `MaxT` for `period`, then, if
more than `MaxR` restarts occur within `MaxT` seconds, the supervisor terminates
all child processes and then itself. The termination reason for the supervisor
itself in that case will be `shutdown`. `intensity` defaults to `1` and `period`
defaults to `5`.

#### Hibernate after

In order to save memory, a supervisor, like any other process, can go into
hibernation. By default, a `simple_one_for_one` supervisor will never hibernate,
as it is expected its children will come and go at potentially high rates.
In counterpart, other strategies rather expect children to be stable and
therefore will default to hibernating after a certain period of time of
inactivity, in order to be responsive to bursts of restarts and save memory in
periods of stability. You can finetune this flag by setting `hibernate_after`,
when for example the supervisor will be regularly queried for `which_child/1` or
similar and hibernation is to be better controlled.

[](){: #auto_shutdown }

#### Automatic Shutdown

A supervisor can be configured to automatically shut itself down with exit
reason `shutdown` when [significant children](`m:supervisor#significant_child`)
terminate with the `auto_shutdown` key in the above map:

- `never` \- Automic shutdown is disabled. This is the default setting.

  With `auto_shutdown` set to `never`, child specs with the `significant` flag
  set to `true` are considered invalid and will be rejected.

- `any_significant` \- The supervisor will shut itself down when _any_
  significant child terminates, that is, when a `transient` significant child
  terminates normally or when a `temporary` significant child terminates
  normally or abnormally.
- `all_significant` \- The supervisor will shut itself down when _all_
  significant children have terminated, that is, when the _last active_
  significant child terminates. The same rules as for `any_significant` apply.

For more information, see the section
[Automatic Shutdown](`e:system:sup_princ.md#automatic-shutdown`) in Supervisor
Behavior in OTP Design Principles.

> #### Warning {: .warning }
>
> The automatic shutdown feature appeared in OTP 24.0, but applications using
> this feature will also compile and run with older OTP versions.
>
> However, such applications, when compiled with an OTP version that predates
> the appearance of the automatic shutdown feature, will leak processes because
> the automatic shutdowns they rely on will not happen.
>
> It is up to implementors to take proper precautions if they expect that their
> applications may be compiled with older OTP versions.

[](){: #child_spec }

### Child specification

The type definition of a child specification is as follows:

```erlang
child_spec() = #{id => child_id(),             % mandatory
                 start => mfargs(),            % mandatory
                 restart => restart(),         % optional
                 significant => significant(), % optional
                 shutdown => shutdown(),       % optional
                 type => worker(),             % optional
                 modules => modules()}         % optional
```

The old tuple format is kept for backwards compatibility, see `t:child_spec/0`,
but the map is preferred.

- `id` is used to identify the child specification internally by the supervisor.

  The `id` key is mandatory.

  Notice that this identifier on occasion has been called "name". As far as
  possible, the terms "identifier" or "id" are now used but to keep backward
  compatibility, some occurences of "name" can still be found, for example in
  error messages.

- `start` defines the function call used to start the child process. It must be
  a module-function-arguments tuple `{M,F,A}` used as
  [`apply(M,F,A)`](`apply/3`).

  The start function _must create and link to_ the child process, and must
  return `{ok,Child}` or `{ok,Child,Info}`, where `Child` is the pid of the
  child process and `Info` any term that is ignored by the supervisor.

  The start function can also return `ignore` if the child process for some
  reason cannot be started, in which case the child specification is kept by the
  supervisor (unless it is a temporary child) but the non-existing child process
  is ignored.

  If something goes wrong, the function can also return an error tuple
  `{error,Error}`.

  Notice that the `start_link` functions of the different behavior modules
  fulfill the above requirements.

  The `start` key is mandatory.

- [](){: #restart } `restart` defines when a terminated child process must be
  restarted. A `permanent` child process is always restarted. A `temporary`
  child process is never restarted (even when the supervisor's restart strategy
  is `rest_for_one` or `one_for_all` and a sibling's death causes the temporary
  process to be terminated). A `transient` child process is restarted only if it
  terminates abnormally, that is, with another exit reason than `normal`,
  `shutdown`, or `{shutdown,Term}`.

  The `restart` key is optional. If it is not specified, it defaults to
  `permanent`.

- [](){: #significant_child } `significant` defines if a child is considered
  significant for [automatic self-shutdown](`m:supervisor#auto_shutdown`) of the
  supervisor.

  Setting this option to `true` when the [restart type](`m:supervisor#restart`)
  is `permanent` is invalid. Also, it is considered invalid to start children
  with this option set to `true` in a supervisor when the
  [`auto_shutdown`](`m:supervisor#auto_shutdown`) supervisor flag is set to
  `never`.

  The `significant` key is optional. If it is not specified, it defaults to
  `false`.

- `shutdown` defines how a child process must be terminated. `brutal_kill` means
  that the child process is unconditionally terminated using
  [`exit_signal(Child,kill)`](`exit_signal/2`). An integer time-out value means that the
  supervisor tells the child process to terminate by calling
  [`exit_signal(Child,shutdown)`](`exit_signal/2`) and then wait for an exit signal with
  reason `shutdown` back from the child process. If no exit signal is received
  within the specified number of milliseconds, the child process is
  unconditionally terminated using [`exit_signal(Child,kill)`](`exit_signal/2`).

  If the child process is another supervisor, the shutdown time must be set to
  `infinity` to give the subtree ample time to shut down.

  > #### Warning {: .warning }
  >
  > Setting the shutdown time to anything other than `infinity` for a child of
  > type `supervisor` can cause a race condition where the child in question
  > unlinks its own children, but fails to terminate them before it is killed.

  It is also allowed to set it to `infinity`, if the child process is a worker.

  > #### Warning {: .warning }
  >
  > Be careful when setting the shutdown time to `infinity` when the child
  > process is a worker. Because, in this situation, the termination of the
  > supervision tree depends on the child process, it must be implemented in a
  > safe way and its cleanup procedure must always return.

  Notice that all child processes implemented using the standard OTP behavior
  modules automatically adhere to the shutdown protocol.

  The `shutdown` key is optional. If it is not specified, it defaults to `5000`
  if the child is of type `worker` and it defaults to `infinity` if the child is
  of type `supervisor`.

- `type` specifies if the child process is a supervisor or a worker.

  The `type` key is optional. If it is not specified, it defaults to `worker`.

- `modules` is used by the release handler during code replacement to determine
  which processes are using a certain module. As a rule of thumb, if the child
  process is a `m:supervisor`, `m:gen_server` or, `m:gen_statem`, this is to be a list
  with one element `[Module]`, where `Module` is the callback module. If the
  child process is an event manager (`m:gen_event`) with a dynamic set of callback
  modules, value `dynamic` must be used. For more information about release
  handling, see [Release Handling](`e:system:release_handling.md`) in OTP Design
  Principles.

  The `modules` key is optional. If it is not specified, it defaults to `[M]`,
  where `M` comes from the child's start `{M,F,A}`.

- Internally, the supervisor also keeps track of the pid `Child` of the child
  process, or `undefined` if no pid exists.

### See Also

`m:gen_event`, `m:gen_statem`, `m:gen_server`, `m:sys`

# `auto_shutdown`
*not exported* 

```erlang
-type auto_shutdown() :: never | any_significant | all_significant.
```

# `child`
*not exported* 

```erlang
-type child() :: undefined | pid().
```

# `child_id`
*not exported* 

```erlang
-type child_id() :: term().
```

Not a `t:pid/0`.

# `child_rec`
*not exported* 

```erlang
-type child_rec() ::
          #child{pid :: child() | {restarting, pid() | undefined} | [pid()],
                 id :: child_id(),
                 mfargs :: mfargs(),
                 restart_type :: restart(),
                 significant :: significant(),
                 shutdown :: shutdown(),
                 child_type :: worker(),
                 modules :: modules()}.
```

# `child_spec`

```erlang
-type child_spec() ::
          #{id := child_id(),
            start := mfargs(),
            restart => restart(),
            significant => significant(),
            shutdown => shutdown(),
            type => worker(),
            modules => modules()} |
          {Id :: child_id(),
           StartFunc :: mfargs(),
           Restart :: restart(),
           Shutdown :: shutdown(),
           Type :: worker(),
           Modules :: modules()}.
```

The tuple format is kept for backward compatibility only. A map is preferred;
see more details [above](`m:supervisor#child_spec`).

# `children`
*not exported* 

```erlang
-type children() :: {Ids :: [child_id()], Db :: #{child_id() => child_rec()}}.
```

# `mfargs`
*not exported* 

```erlang
-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
```

Value `undefined` for `A` (the argument list) is only to be used internally in
`m:supervisor`. If the restart type of the child is `temporary`, the process is
never to be restarted and therefore there is no need to store the real argument
list. Value `undefined` is then stored instead.

# `modules`
*not exported* 

```erlang
-type modules() :: [module()] | dynamic.
```

# `restart`
*not exported* 

```erlang
-type restart() :: permanent | transient | temporary.
```

# `shutdown`
*not exported* 

```erlang
-type shutdown() :: brutal_kill | timeout().
```

# `significant`
*not exported* 

```erlang
-type significant() :: boolean().
```

# `startchild_err`

```erlang
-type startchild_err() :: already_present | {already_started, Child :: child()} | term().
```

# `startchild_ret`

```erlang
-type startchild_ret() ::
          {ok, Child :: child()} | {ok, Child :: child(), Info :: term()} | {error, startchild_err()}.
```

# `startlink_err`

```erlang
-type startlink_err() :: {already_started, pid()} | {shutdown, term()} | term().
```

# `startlink_ret`

```erlang
-type startlink_ret() :: {ok, pid()} | ignore | {error, startlink_err()}.
```

# `strategy`

```erlang
-type strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.
```

# `sup_flags`

```erlang
-type sup_flags() ::
          #{strategy => strategy(),
            intensity => non_neg_integer(),
            period => pos_integer(),
            auto_shutdown => auto_shutdown(),
            hibernate_after => timeout()} |
          {RestartStrategy :: strategy(), Intensity :: non_neg_integer(), Period :: pos_integer()}.
```

The tuple format is kept for backward compatibility only. A map is preferred;
see more details [above](`m:supervisor#sup_flags`).

# `sup_name`

```erlang
-type sup_name() ::
          {local, Name :: atom()} | {global, Name :: term()} | {via, Module :: module(), Name :: any()}.
```

Name specification to use when starting a `supervisor`. See function
[`start_link/2,3`](`start_link/2`) and the type `t:sup_ref/0` below.

- **`{local,LocalName}`** - Register the `supervisor` locally as `LocalName`
  using [`register/2`](`erlang:register/2`).

- **`{global,GlobalName}`** - Register the `supervisor` process id globally as
  `GlobalName` using `global:register_name/2`.

- **`{via,RegMod,ViaName}`** - Register the `supervisor` process with the
  registry represented by `RegMod`. The `RegMod` callback is to export the
  functions `register_name/2`, `unregister_name/1`, `whereis_name/1`, and
  `send/2`, which are to behave like the corresponding functions in `m:global`.
  Thus, `{via,global,GlobalName}` is a valid reference equivalent to
  `{global,GlobalName}`.

# `sup_ref`

```erlang
-type sup_ref() ::
          (Name :: atom()) |
          {Name :: atom(), Node :: node()} |
          {global, Name :: term()} |
          {via, Module :: module(), Name :: any()} |
          pid().
```

Supervisor specification to use when addressing a `supervisor`. See
[`count_children/1`](`count_children/1`), [`delete_child/2`](`delete_child/2`),
[`get_childspec/2`](`get_childspec/2`), [`restart_child/2`](`restart_child/2`),
[`start_child/2`](`start_child/2`), [`terminate_child/2`](`terminate_child/2`),
[`which_children/1`](`which_children/1`) and the type `t:sup_name/0` above.

It can be:

- **`t:pid/0`** - The `supervisor`'s process identifier.

- **`LocalName`** - The `supervisor` is locally registered as `LocalName` with
  [`register/2`](`erlang:register/2`).

- **`{Name,Node}`** - The `supervisor` is locally registered on another node.

- **`{global,GlobalName}`** - The `supervisor` is globally registered in
  `m:global`.

- **`{via,RegMod,ViaName}`** - The `supervisor` is registered in an alternative
  process registry. The registry callback module `RegMod` is to export functions
  `register_name/2`, `unregister_name/1`, `whereis_name/1`, and `send/2`, which
  are to behave like the corresponding functions in `m:global`. Thus,
  `{via,global,GlobalName}` is the same as `{global,GlobalName}`.

# `worker`
*not exported* 

```erlang
-type worker() :: worker | supervisor.
```

# `init`

```erlang
-callback init(Args :: term()) -> {ok, {SupFlags :: sup_flags(), [ChildSpec :: child_spec()]}} | ignore.
```

Whenever a supervisor is started using [`start_link/2,3`](`start_link/2`), this
function is called by the new process to find out about restart strategy,
maximum restart intensity, and child specifications.

`Args` is the `Args` argument provided to the start function.

`SupFlags` is the supervisor flags defining the restart strategy and maximum
restart intensity for the supervisor. `[ChildSpec]` is a list of valid child
specifications defining which child processes the supervisor must start and
monitor. See the discussion in section
[`Supervision Principles`](`m:supervisor#supervision_princ`) earlier.

Notice that when the restart strategy is `simple_one_for_one`, the list of child
specifications must be a list with one child specification only. (The child
specification identifier is ignored.) No child process is then started during
the initialization phase, but all children are assumed to be started dynamically
using `start_child/2`.

The function can also return `ignore`.

Notice that this function can also be called as a part of a code upgrade
procedure. Therefore, the function is not to have any side effects. For more
information about code upgrade of supervisors, see section
[Changing a Supervisor](`e:system:appup_cookbook.md#sup`) in OTP Design
Principles.

# `check_childspecs`

```erlang
-spec check_childspecs(ChildSpecs) -> Result
                          when ChildSpecs :: [child_spec()], Result :: ok | {error, Error :: term()}.
```

# `check_childspecs`
*since OTP 24.0* 

```erlang
-spec check_childspecs(ChildSpecs, AutoShutdown) -> Result
                          when
                              ChildSpecs :: [child_spec()],
                              AutoShutdown :: undefined | auto_shutdown(),
                              Result :: ok | {error, Error :: term()}.
```

Takes a list of child specification as argument and returns `ok` if all of them
are syntactically correct, otherwise `{error,Error}`.

If the `AutoShutdown` argument is not `undefined`, also
checks if the child specifications are allowed for the given
[auto_shutdown](`m:supervisor#auto_shutdown`) option.

# `count_children`
*since OTP R13B04* 

```erlang
-spec count_children(SupRef) -> PropListOfCounts
                        when
                            SupRef :: sup_ref(),
                            PropListOfCounts :: [Count],
                            Count ::
                                {specs, ChildSpecCount :: non_neg_integer()} |
                                {active, ActiveProcessCount :: non_neg_integer()} |
                                {supervisors, ChildSupervisorCount :: non_neg_integer()} |
                                {workers, ChildWorkerCount :: non_neg_integer()}.
```

Returns a [property list](`t:proplists:proplist/0`) containing the counts for each of
the following elements of the supervisor's child specifications and managed
processes:

- `specs` \- The total count of children, dead or alive.
- `active` \- The count of all actively running child processes managed by this
  supervisor. For a `simple_one_for_one` supervisors, no check is done to ensure
  that each child process is still alive, although the result provided here is
  likely to be very accurate unless the supervisor is heavily overloaded.
- `supervisors` \- The count of all children marked as `child_type = supervisor`
  in the specification list, regardless if the child process is still alive.
- `workers` \- The count of all children marked as `child_type = worker` in the
  specification list, regardless if the child process is still alive.

# `delete_child`

```erlang
-spec delete_child(SupRef, Id) -> Result
                      when
                          SupRef :: sup_ref(),
                          Id :: child_id(),
                          Result :: ok | {error, Error},
                          Error :: running | restarting | not_found | simple_one_for_one.
```

Tells supervisor `SupRef` to delete the child specification identified by `Id`.
The corresponding child process must not be running. Use `terminate_child/2` to
terminate it.

If successful, the function returns `ok`. If the child specification identified
by `Id` exists but the corresponding child process is running or is about to be
restarted, the function returns `{error,running}` or `{error,restarting}`,
respectively. If the child specification identified by `Id` does not exist, the
function returns `{error,not_found}`.

# `get_childspec`
*since OTP 18.0* 

```erlang
-spec get_childspec(SupRef, Id) -> Result
                       when
                           SupRef :: sup_ref(),
                           Id :: pid() | child_id(),
                           Result :: {ok, child_spec()} | {error, Error},
                           Error :: not_found.
```

Returns the child specification map for the child identified by `Id` under
supervisor `SupRef`. The returned map contains all keys, both mandatory and
optional.

# `restart_child`

```erlang
-spec restart_child(SupRef, Id) -> Result
                       when
                           SupRef :: sup_ref(),
                           Id :: child_id(),
                           Result ::
                               {ok, Child :: child()} |
                               {ok, Child :: child(), Info :: term()} |
                               {error, Error},
                           Error :: running | restarting | not_found | simple_one_for_one | term().
```

Tells supervisor `SupRef` to restart a child process corresponding to the child
specification identified by `Id`. The child specification must exist, and the
corresponding child process must not be running.

Notice that for temporary children, the child specification is automatically
deleted when the child terminates; thus, it is not possible to restart such
children.

If the child specification identified by `Id` does not exist, the function
returns `{error,not_found}`. If the child specification exists but the
corresponding process is already running, the function returns
`{error,running}`.

If the child process start function returns `{ok,Child}` or `{ok,Child,Info}`,
the pid is added to the supervisor and the function returns the same value.

If the child process start function returns `ignore`, the pid remains set to
`undefined` and the function returns `{ok,undefined}`.

If the child process start function returns an error tuple or an erroneous
value, or if it fails, the function returns `{error,Error}`, where `Error` is a
term containing information about the error.

# `start_child`

```erlang
-spec start_child(SupRef, ChildSpec) -> startchild_ret()
                     when SupRef :: sup_ref(), ChildSpec :: child_spec();
                 (SupRef, ExtraArgs) -> startchild_ret() when SupRef :: sup_ref(), ExtraArgs :: [term()].
```

Dynamically adds a child specification to supervisor `SupRef`, which starts the
corresponding child process.

For `one_for_one`, `one_for_all` and `rest_for_one` supervisors, the second
argument must be a valid child specification `ChildSpec`. The child process
is started by using the start function as defined in the child specification.

For `simple_one_for_one` supervisors, the child specification defined in
[`Module:init/1`](`c:init/1`) is used, and the second argument must instead
be an arbitrary list of terms `ExtraArgs`. The child process is then started
by appending `ExtraArgs` to the existing start function arguments, that is, by
calling [`apply(M, F, A++ExtraArgs)`](`apply/3`), where `{M,F,A}` is the start
function defined in the child specification.

- If there already exists a child specification with the specified identifier,
  `ChildSpec` is discarded, and the function returns `{error,already_present}`
  or `{error,{already_started,Child}}`, depending on if the corresponding child
  process is running or not.
- If the child process start function returns `{ok,Child}` or `{ok,Child,Info}`,
  the child specification and pid are added to the supervisor and the function
  returns the same value.
- If the child process start function returns `ignore`, the child specification
  `ChildSpec` is added to the supervisor if it is an `one_for_one`, `one_for_all`
  or `rest_for_one` supervisor, and the pid is set to `undefined`. For
  `simple_one_for_one` supervisors, no child is added to the supervisor. The
  function returns `{ok,undefined}`.

If the child process start function returns an error tuple or an erroneous
value, or if it fails, the child specification is discarded, and the function
returns `{error,Error}`, where `Error` is a term containing information about
the error and child specification.

# `start_link`

```erlang
-spec start_link(Module, Args) -> startlink_ret() when Module :: module(), Args :: term().
```

Creates a nameless supervisor process as part of a supervision tree.

Equivalent to `start_link/3` except that the supervisor process is not
[`registered`](`erlang:register/2`).

# `start_link`

```erlang
-spec start_link(SupName, Module, Args) -> startlink_ret()
                    when SupName :: sup_name(), Module :: module(), Args :: term().
```

Creates a supervisor process as part of a supervision tree.

For example, the function ensures that the supervisor is linked to the calling
process (its supervisor).

The created supervisor process calls [`Module:init/1`](`c:init/1`) to find out
about restart strategy, maximum restart intensity, and child processes. To
ensure a synchronized startup procedure, `start_link/2,3` does not return until
[`Module:init/1`](`c:init/1`) has returned and all child processes have been
started.

- If `SupName={local,Name}`, the supervisor is registered locally as `Name`
  using [`register/2`](`register/2`).
- If `SupName={global,Name}`, the supervisor is registered globally as `Name`
  using `global:register_name/2`.
- If `SupName={via,Module,Name}`, the supervisor is registered as `Name` using
  the registry represented by `Module`. The `Module` callback must export the
  functions `register_name/2`, `unregister_name/1`, and `send/2`, which must
  behave like the corresponding functions in `m:global`. Thus,
  `{via,global,Name}` is a valid reference.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

- If the supervisor and its child processes are successfully created (that is,
  if all child process start functions return `{ok,Child}`, `{ok,Child,Info}`,
  or `ignore`), the function returns `{ok,Pid}`, where `Pid` is the pid of the
  supervisor.
- If there already exists a process with the specified `SupName`, the function
  returns `{error,{already_started,Pid}}`, where `Pid` is the pid of that
  process.
- If [`Module:init/1`](`c:init/1`) returns `ignore`, this function returns
  `ignore` as well, and the supervisor terminates with reason `normal`.
- If [`Module:init/1`](`c:init/1`) fails or returns an incorrect value, this
  function returns `{error,Term}`, where `Term` is a term with information about
  the error, and the supervisor terminates with reason `Term`.
- If any child process start function fails or returns an error tuple or an
  erroneous value, the supervisor first terminates all already started child
  processes with reason `shutdown` and then terminate itself and returns
  `{error, {shutdown, Reason}}`.

# `stop`
*since OTP 29.0* 

```erlang
-spec stop(SupRef :: sup_ref()) -> ok.
```

# `stop`
*since OTP 29.0* 

```erlang
-spec stop(SupRef :: sup_ref(), Reason :: term(), Timeout :: timeout()) -> ok.
```

Stop a supervisor.

Orders the supervisor specified by `SupRef` to exit
with the specified `Reason` and waits for it to terminate.
The supervisor will terminate all its children
before exiting.

The function returns `ok` if the supervisor terminates
with the expected reason. Any other reason than `normal`, `shutdown`,
or `{shutdown,Term}` causes an error report to be issued using `m:logger`.
An exit signal with the same reason is sent to linked processes and ports.

`Timeout` is an integer that specifies how many milliseconds to wait
for the supervisor to terminate, or the atom `infinity` to wait indefinitely.
If the supervisor has not terminated within the specified time,
the call exits the calling process with reason `timeout`.

If the process does not exist, the call exits the calling process
with reason `noproc`, or with reason `{nodedown,Node}`
if the connection fails to the remote `Node` where the supervisor runs.

> #### Warning {: .warning }
>
> Calling this function from a (sub-)child process of the given supervisor
> will result in a deadlock which will last until either the shutdown timeout
> of the child or the timeout given to `stop/3` has expired.

# `terminate_child`

```erlang
-spec terminate_child(SupRef, Id) -> Result
                         when
                             SupRef :: sup_ref(),
                             Id :: pid() | child_id(),
                             Result :: ok | {error, Error},
                             Error :: not_found | simple_one_for_one.
```

Tells supervisor `SupRef` to terminate the specified child.

If the supervisor is not `simple_one_for_one`, `Id` must be the child
specification identifier. The process, if any, is terminated and, unless it is a
temporary child, the child specification is kept by the supervisor. The child
process can later be restarted by the supervisor. The child process can also be
restarted explicitly by calling `restart_child/2`. Use `delete_child/2` to
remove the child specification.

If the child is temporary, the child specification is deleted as soon as the
process terminates. This means that [`delete_child/2`](`delete_child/2`) has no
meaning and [`restart_child/2`](`restart_child/2`) cannot be used for these
children.

If the supervisor is `simple_one_for_one`, `Id` must be the `t:pid/0` of the
child process. If the specified process is alive, but is not a child of the
specified supervisor, the function returns `{error,not_found}`. If the child
specification identifier is specified instead of a `t:pid/0`, the function
returns `{error,simple_one_for_one}`.

If successful, the function returns `ok`. If there is no child specification
with the specified `Id`, the function returns `{error,not_found}`.

# `which_child`
*since OTP 28.0* 

```erlang
-spec which_child(SupRef, Id) -> Result
                     when
                         SupRef :: sup_ref(),
                         Id :: pid() | child_id(),
                         Result :: {ok, {Id, Child, Type, Modules}} | {error, Error},
                         Child :: child() | restarting,
                         Type :: worker(),
                         Modules :: modules(),
                         Error :: not_found.
```

Returns information about the child specification and child process identified
by the given `Id`.

See `which_children/1` for an explanation of the information returned.

If no child with the given `Id` exists, returns `{error, not_found}`.

# `which_children`

```erlang
-spec which_children(SupRef) -> [{Id, Child, Type, Modules}]
                        when
                            SupRef :: sup_ref(),
                            Id :: child_id() | undefined,
                            Child :: child() | restarting,
                            Type :: worker(),
                            Modules :: modules().
```

Returns a newly created list with information about all child specifications and
child processes belonging to supervisor `SupRef`.

Notice that calling this function when supervising many children under low
memory conditions can cause an out of memory exception.

The following information is given for each child specification/process:

- `Id` \- As defined in the child specification or `undefined` for a
  `simple_one_for_one` supervisor.
- `Child` \- The pid of the corresponding child process, the atom `restarting`
  if the process is about to be restarted, or `undefined` if there is no such
  process.
- `Type` \- As defined in the child specification.
- `Modules` \- As defined in the child specification.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
