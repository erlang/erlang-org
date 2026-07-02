# `application`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/kernel/src/application.erl#L22)

Generic OTP application functions

In OTP, _application_ denotes a component implementing some specific
functionality, that can be started and stopped as a unit, and that can be reused
in other systems. This module interacts with _application controller_, a process
started at every Erlang runtime system. This module contains functions for
controlling applications (for example, starting and stopping applications), and
functions to access information about applications (for example, configuration
parameters).

An application is defined by an _application specification_. The specification
is normally located in an _application resource file_ named `Application.app`,
where `Application` is the application name. For details about the application
specification, see [`app`](app.md).

This module can also be viewed as a behaviour for an application implemented
according to the OTP design principles as a supervision tree. The definition of
how to start and stop the tree is to be located in an _application callback
module_, exporting a predefined set of functions.

For details about applications and behaviours, see
[OTP Design Principles](`e:system:design_principles.md`).

## See Also

[OTP Design Principles](`e:system:design_principles.md`),
[kernel](kernel_app.md), [app](app.md)

# `application_opt`
*not exported* 

```elixir
-type application_opt() ::
          {description, Description :: string()} |
          {vsn, Vsn :: string()} |
          {id, Id :: string()} |
          {modules, [Module :: module()]} |
          {registered, Names :: [Name :: atom()]} |
          {applications, [Application :: atom()]} |
          {included_applications, [Application :: atom()]} |
          {env, [{Par :: atom(), Val :: term()}]} |
          {start_phases, [{Phase :: atom(), PhaseArgs :: term()}] | undefined} |
          {maxT, MaxT :: timeout()} |
          {maxP, MaxP :: pos_integer() | infinity} |
          {mod, Start :: {Module :: module(), StartArgs :: term()}}.
```

The built-in options available to an application.

See [app](app.md) for descriptions of the options.

# `application_spec`
*not exported* 

```elixir
-type application_spec() :: {application, Application :: atom(), AppSpecKeys :: [application_opt()]}.
```

An application specification.

# `restart_type`

```elixir
-type restart_type() :: permanent | transient | temporary.
```

The type of restart behaviour an application should have.

# `start_type`

```elixir
-type start_type() :: normal | {takeover, Node :: node()} | {failover, Node :: node()}.
```

The reason for the application to be started on the current node.

# `tuple_of`
*not exported* 

```elixir
-type tuple_of(_T) :: tuple().
```

A tuple where the elements are of type `T`.

# `config_change`
*optional* 

```elixir
-callback config_change(Changed, New, Removed) -> ok
                           when
                               Changed :: [{Par, Val}],
                               New :: [{Par, Val}],
                               Removed :: [Par],
                               Par :: atom(),
                               Val :: term().
```

This function is called by an application after a code replacement, if the
configuration parameters have changed.

`Changed` is a list of parameter-value tuples including all configuration
parameters with changed values.

`New` is a list of parameter-value tuples including all added configuration
parameters.

`Removed` is a list of all removed parameters.

# `prep_stop`
*optional* 

```elixir
-callback prep_stop(State) -> NewState when State :: term(), NewState :: term().
```

This function is called when an application is about to be stopped, before
shutting down the processes of the application.

`State` is the state returned from [`Module:start/2`](`c:start/2`), or `[]` if
no state was returned. `NewState` is any term and is passed to
[`Module:stop/1`](`c:stop/1`).

The function is optional. If it is not defined, the processes are terminated and
then [`Module:stop(State)`](`c:stop/1`) is called.

# `start`

```elixir
-callback start(StartType :: start_type(), StartArgs :: term()) ->
                   {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
```

This function is called whenever an application is started using `start/1,2`,
and is to start the processes of the application. If the application is
structured according to the OTP design principles as a supervision tree, this
means starting the top supervisor of the tree.

`StartType`{: #start_type } defines the type of start:

- `normal` if it is a normal startup.
- `normal` also if the application is distributed and started at the current
  node because of a failover from another node, and the application
  specification key `start_phases == undefined`.
- `{takeover,Node}` if the application is distributed and started at the current
  node because of a takeover from `Node`, either because
  [`takeover/2`](`takeover/2`) has been called or because the current node has
  higher priority than `Node`.
- `{failover,Node}` if the application is distributed and started at the current
  node because of a failover from `Node`, and the application specification key
  `start_phases /= undefined`.

`StartArgs` is the `StartArgs` argument defined by the application specification
key `mod`.

The function is to return `{ok,Pid}` or `{ok,Pid,State}`, where `Pid` is the pid
of the top supervisor and `State` is any term. If omitted, `State` defaults to
`[]`. If the application is stopped later, `State` is passed to
[`Module:prep_stop/1`](`c:prep_stop/1`).

# `start_phase`
*optional* 

```elixir
-callback start_phase(Phase, StartType, PhaseArgs) -> ok | {error, Reason}
                         when
                             Phase :: atom(),
                             StartType :: start_type(),
                             PhaseArgs :: term(),
                             Reason :: term().
```

Starts an application with included applications, when synchronization is needed
between processes in the different applications during startup.

The start phases are defined by the application specification key
`start_phases == [{Phase,PhaseArgs}]`. For included applications, the set of
phases must be a subset of the set of phases defined for the including
application.

The function is called for each start phase (as defined for the primary
application) for the primary application and all included applications, for
which the start phase is defined.

For a description of `StartType`, see [`Module:start/2`](`c:start/2`).

# `stop`

```elixir
-callback stop(State :: term()) -> term().
```

This function is called whenever an application has stopped. It is intended to
be the opposite of [`Module:start/2`](`c:start/2`) and is to do any necessary
cleaning up. The return value is ignored.

`State` is the return value of [`Module:prep_stop/1`](`c:prep_stop/1`), if such
a function exists. Otherwise `State` is taken from the return value of
[`Module:start/2`](`c:start/2`).

# `ensure_all_started`
*since OTP R16B02* 

```elixir
-spec ensure_all_started(Applications) -> {ok, Started} | {error, Reason}
                            when
                                Applications :: atom() | [atom()], Started :: [atom()], Reason :: term().
```

# `ensure_all_started`
*since OTP R16B02* 

```elixir
-spec ensure_all_started(Applications, Type) -> {ok, Started} | {error, AppReason}
                            when
                                Applications :: atom() | [atom()],
                                Type :: restart_type(),
                                Started :: [atom()],
                                AppReason :: {atom(), term()}.
```

# `ensure_all_started`
*since OTP 26.0* 

```elixir
-spec ensure_all_started(Applications, Type, Mode) -> {ok, Started} | {error, AppReason}
                            when
                                Applications :: atom() | [atom()],
                                Type :: restart_type(),
                                Mode :: serial | concurrent,
                                Started :: [atom()],
                                AppReason :: {atom(), term()}.
```

`Applications` is either an an `t:atom/0` or a list of `t:atom/0` representing
multiple applications.

This function is equivalent to calling [`start/1,2`](`start/1`) repeatedly on
all dependencies that are not yet started of each application. Optional
dependencies will also be loaded and started if they are available.

The `Mode` argument controls if the applications should be started in `serial`
mode (one at a time) or `concurrent` mode. In concurrent mode, a dependency
graph is built and the leaves of the graph are started concurrently and
recursively. In both modes, no assertion can be made about the order the
applications are started. If not supplied, it defaults to `serial`.

Returns `{ok, AppNames}` for a successful start or for an already started
application (which is, however, omitted from the `AppNames` list).

The function reports `{error, {AppName,Reason}}` for errors, where `Reason` is
any possible reason returned by [`start/1,2`](`start/1`) when starting a
specific dependency.

If an error occurs, the applications started by the function are stopped to
bring the set of running applications back to its initial state.

# `ensure_started`
*since OTP R16B01* 

```elixir
-spec ensure_started(Application) -> ok | {error, Reason} when Application :: atom(), Reason :: term().
```

Equivalent to [`start(Application)`](`start/1`) except it returns `ok` for
already started applications.

# `ensure_started`
*since OTP R16B01* 

```elixir
-spec ensure_started(Application, Type) -> ok | {error, Reason}
                        when Application :: atom(), Type :: restart_type(), Reason :: term().
```

Equivalent to [`start(Application, Type)`](`start/2`) except it returns `ok` for
already started applications.

# `get_all_env`

```elixir
-spec get_all_env() -> Env when Env :: [{Par :: atom(), Val :: term()}].
```

# `get_all_env`

```elixir
-spec get_all_env(Application) -> Env
                     when Application :: atom(), Env :: [{Par :: atom(), Val :: term()}].
```

Returns the configuration parameters and their values for `Application`.

If the specified application is not loaded, or if the process executing the call
does not belong to any application, the function returns `[]`.

# `get_all_key`

```elixir
-spec get_all_key() -> [] | {ok, Keys} when Keys :: [{Key :: atom(), Val :: term()}, ...].
```

# `get_all_key`

```elixir
-spec get_all_key(Application) -> undefined | Keys
                     when Application :: atom(), Keys :: {ok, [{Key :: atom(), Val :: term()}, ...]}.
```

Returns the application specification keys and their values for `Application`.
If the argument is omitted, it defaults to the application of the calling
process.

If the specified application is not loaded, the function returns `undefined`. If
the process executing the call does not belong to any application, the function
returns `[]`.

# `get_application`

```elixir
-spec get_application() -> undefined | {ok, Application} when Application :: atom().
```

# `get_application`

```elixir
-spec get_application(PidOrModule) -> undefined | {ok, Application}
                         when
                             PidOrModule :: (Pid :: pid()) | (Module :: module()), Application :: atom().
```

Returns the name of the application to which the process `Pid` or the module
`Module` belongs.

If the specified process does not belong to any application, or if the specified
process or module does not exist, the function returns `undefined`.

# `get_env`

```elixir
-spec get_env(Par) -> undefined | {ok, Val} when Par :: atom(), Val :: term().
```

# `get_env`

```elixir
-spec get_env(Application, Par) -> undefined | {ok, Val}
                 when Application :: atom(), Par :: atom(), Val :: term().
```

Returns the value of configuration parameter `Par` for `Application`.

Returns `undefined` if any of the following applies:

- The specified application is not loaded.
- The configuration parameter does not exist.
- The process executing the call does not belong to any application.

# `get_env`
*since OTP R16B* 

```elixir
-spec get_env(Application, Par, Def) -> Val
                 when Application :: atom(), Par :: atom(), Def :: term(), Val :: term().
```

Works like `get_env/2` but returns value `Def` when configuration parameter
`Par` does not exist.

# `get_key`

```elixir
-spec get_key(Key) -> undefined | {ok, Val} when Key :: atom(), Val :: term().
```

# `get_key`

```elixir
-spec get_key(Application, Key) -> undefined | {ok, Val}
                 when Application :: atom(), Key :: atom(), Val :: term().
```

Returns the value of the application specification key `Key` for `Application`.

Returns `undefined` if any of the following applies:

- The specified application is not loaded.
- The specification key does not exist.
- The process executing the call does not belong to any application.

# `get_supervisor`
*since OTP 26.0* 

```elixir
-spec get_supervisor(Application) -> undefined | {ok, Pid} when Pid :: pid(), Application :: atom().
```

Returns the `Pid` of the supervisor running at the root of `Application`.

If the specified application does not exist or does not define a callback
module, the function returns `undefined`.

# `load`

```elixir
-spec load(AppDescr) -> ok | {error, Reason}
              when
                  AppDescr :: Application | (AppSpec :: application_spec()),
                  Application :: atom(),
                  Reason :: term().
```

# `load`

```elixir
-spec load(AppDescr, Distributed) -> ok | {error, Reason}
              when
                  AppDescr :: Application | (AppSpec :: application_spec()),
                  Application :: atom(),
                  Distributed :: {Application, Nodes} | {Application, Time, Nodes} | default,
                  Nodes :: [node() | tuple_of(node())],
                  Time :: pos_integer(),
                  Reason :: term().
```

Loads the application specification for an application into the application
controller. It also loads the application specifications for any included
applications. Notice that the function does not load the Erlang object code.

The application can be specified by its name `Application`. In this case, the
application controller searches the code path for the application resource file
`Application.app` and loads the specification it contains.

The application specification can also be specified directly as a tuple
`AppSpec`, having the format and contents as described in [`app`](app.md).

If `Distributed == {Application,[Time,]Nodes}`, the application becomes
distributed. The argument overrides the value for the application in the Kernel
configuration parameter `distributed`. `Application` must be the application
name (same as in the first argument). If a node crashes and `Time` is specified,
the application controller waits for `Time` milliseconds before attempting to
restart the application on another node. If `Time` is not specified, it defaults
to `0` and the application is restarted immediately.

`Nodes` is a list of node names where the application can run, in priority from
left to right. Node names can be grouped using tuples to indicate that they have
the same priority.

_Example:_

```erlang
Nodes = [cp1@cave, {cp2@cave, cp3@cave}]
```

This means that the application is preferably to be started at `cp1@cave`. If
`cp1@cave` is down, the application is to be started at `cp2@cave` or
`cp3@cave`.

If `Distributed == default`, the value for the application in the Kernel
configuration parameter `distributed` is used.

# `loaded_applications`

```elixir
-spec loaded_applications() -> [{Application, Description, Vsn}]
                             when Application :: atom(), Description :: string(), Vsn :: string().
```

Returns a list with information about the applications, and included
applications, which are loaded using `load/1,2`. `Application` is the
application name. `Description` and `Vsn` are the values of their `description`
and `vsn` application specification keys, respectively.

# `permit`

```elixir
-spec permit(Application, Permission) -> ok | {error, Reason}
                when Application :: atom(), Permission :: boolean(), Reason :: term().
```

Changes the permission for `Application` to run at the current node. The
application must be loaded using `load/1,2` for the function to have effect.

If the permission of a loaded, but not started, application is set to `false`,
`start` returns `ok` but the application is not started until the permission is
set to `true`.

If the permission of a running application is set to `false`, the application is
stopped. If the permission later is set to `true`, it is restarted.

If the application is distributed, setting the permission to `false` means that
the application will be started at, or moved to, another node according to how
its distribution is configured (see `load/2`).

The function does not return until the application is started, stopped, or
successfully moved to another node. However, in some cases where permission is
set to `true`, the function returns `ok` even though the application is not
started. This is true when an application cannot start because of dependencies
to other applications that are not yet started. When they are started,
`Application` is started as well.

By default, all applications are loaded with permission `true` on all nodes. The
permission can be configured using the Kernel configuration parameter
`permissions`.

# `set_env`
*since OTP 21.3* 

```elixir
-spec set_env(Config) -> ok
                 when
                     Config :: [{Application, Env}],
                     Application :: atom(),
                     Env :: [{Par :: atom(), Val :: term()}].
```

# `set_env`
*since OTP 21.3* 

```elixir
-spec set_env(Config, Opts) -> ok
                 when
                     Config :: [{Application, Env}],
                     Application :: atom(),
                     Env :: [{Par :: atom(), Val :: term()}],
                     Opts :: [{timeout, timeout()} | {persistent, boolean()}].
```

Sets the configuration `Config` for multiple applications.

It is equivalent to calling [`set_env/4`](`set_env/4`) on each application
individually, except it is more efficient. The given `Config` is validated before
the configuration is set.

[`set_env/2`](`set_env/2`) uses the standard `gen_server` time-out value (5000
ms). Option `timeout` can be specified if another time-out value is useful, for
example, in situations where the application controller is heavily loaded.

Option `persistent` can be set to `true` to guarantee that parameters set with
[`set_env/2`](`set_env/2`) are not overridden by those defined in the
application resource file on load. This means that persistent values will stick
after the application is loaded and also on application reload.

If an application is given more than once or if an application has the same key
given more than once, the behaviour is undefined and a warning message will be
logged. In future releases, an error will be raised.

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.

# `set_env`

```elixir
-spec set_env(Application, Par, Val) -> ok when Application :: atom(), Par :: atom(), Val :: term().
```

# `set_env`

```elixir
-spec set_env(Application, Par, Val, Opts) -> ok
                 when
                     Application :: atom(),
                     Par :: atom(),
                     Val :: term(),
                     Opts :: [{timeout, timeout()} | {persistent, boolean()}].
```

Sets the value of configuration parameter `Par` for `Application`.

[`set_env/4`](`set_env/4`) uses the standard `gen_server` time-out value (5000
ms). Option `timeout` can be specified if another time-out value is useful, for
example, in situations where the application controller is heavily loaded.

If [`set_env/4`](`set_env/4`) is called before the application is loaded, the
application environment values specified in file `Application.app` override the
ones previously set. This is also true for application reloads.

Option `persistent` can be set to `true` to guarantee that parameters set with
[`set_env/4`](`set_env/4`) are not overridden by those defined in the
application resource file on load. This means that persistent values will stick
after the application is loaded and also on application reload.

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.

# `start`

```elixir
-spec start(Application) -> ok | {error, Reason} when Application :: atom(), Reason :: term().
```

# `start`

```elixir
-spec start(Application, Type) -> ok | {error, Reason}
               when Application :: atom(), Type :: restart_type(), Reason :: term().
```

Starts `Application`. If it is not loaded, the application controller first
loads it using [`load/1`](`load/1`). It ensures that any included applications
are loaded, but does not start them. That is assumed to be taken care of in the
code for `Application`.

The application controller checks the value of the application specification key
`applications`, to ensure that all applications needed to be started before this
application are running. If an application is missing and the application is not
marked as optional, `{error,{not_started,App}}` is returned, where `App` is the
name of the missing application. Note this function makes no attempt to start
any of the applications listed in `applications`, not even optional ones. See
[`ensure_all_started/1,2`](`ensure_all_started/1`) for recursively starting the
current application and its dependencies.

Once validated, the application controller then creates an _application master_
for the application. The application master becomes the group leader of all the
processes in the application. I/O is forwarded to the previous group leader,
though, this is just a way to identify processes that belong to the application.
Used for example to find itself from any process, or, reciprocally, to kill them
all when it terminates.

The application master starts the application by calling the application
callback function [`Module:start/2`](`c:start/2`) as defined by the application
specification key `mod`.

Argument `Type` specifies the type of the application. If omitted, it defaults
to `temporary`.

- If a permanent application terminates, all other applications and the entire
  Erlang node are also terminated.
- If a transient application terminates:
  - with `Reason == normal`, this is reported but no other applications are
    terminated.
  - abnormally, all other applications and the entire Erlang node are also
    terminated.
- If a temporary application terminates, this is reported but no other
  applications are terminated.

Notice that an application can always be stopped explicitly by calling
[`stop/1`](`stop/1`). Regardless of the type of the application, no other
applications are affected.

Notice also that the transient type is of little practical use, because when a
supervision tree terminates, the reason is set to `shutdown`, not `normal`.

# `start_type`

```elixir
-spec start_type() -> StartType | undefined | local when StartType :: start_type().
```

This function is intended to be called by a process belonging to an application,
when the application is started, to determine the start type, which is
`StartType` or `local`.

For a description of `StartType`, see
[`Module:start/2`](`m:application#start_type`).

`local` is returned if only parts of the application are restarted (by a
supervisor), or if the function is called outside a startup.

If the process executing the call does not belong to any application, the
function returns `undefined`.

# `stop`

```elixir
-spec stop(Application) -> ok | {error, Reason} when Application :: atom(), Reason :: term().
```

Stops `Application`. The application master calls
[`Module:prep_stop/1`](`c:prep_stop/1`), if such a function is defined, and then
tells the top supervisor of the application to shut down (see `m:supervisor`).

This means that the entire supervision tree, including included applications, is
terminated in reversed start order. After the shutdown, the application master
calls [`Module:stop/1`](`c:stop/1`). `Module` is the callback module as defined
by the application specification key `mod`.

Last, the application master terminates. Notice that all processes with the
application master as group leader, that is, processes spawned from a process
belonging to the application, are also terminated.

When stopped, the application is still loaded.

To stop a distributed application, [`stop/1`](`stop/1`) must be called on all
nodes where it can execute (that is, on all nodes where it has been started).
The call to [`stop/1`](`stop/1`) on the node where the application currently
executes stops its execution. The application is not moved between nodes, as
[`stop/1`](`stop/1`) is called on the node where the application currently
executes before [`stop/1`](`stop/1`) is called on the other nodes.

# `takeover`

```elixir
-spec takeover(Application, Type) -> ok | {error, Reason}
                  when Application :: atom(), Type :: restart_type(), Reason :: term().
```

Takes over the distributed application `Application`, which executes at another
node `Node`.

At the current node, the application is restarted by calling
[`Module:start({takeover,Node},StartArgs)`](`c:start/2`). `Module` and
`StartArgs` are retrieved from the loaded application specification. The
application at the other node is not stopped until the startup is completed,
that is, when [`Module:start/2`](`c:start/2`) and any calls to
[`Module:start_phase/3`](`c:start_phase/3`) have returned.

Thus, two instances of the application run simultaneously during the takeover,
so that data can be transferred from the old to the new instance. If this is not
an acceptable behavior, parts of the old instance can be shut down when the new
instance is started. However, the application cannot be stopped entirely, at
least the top supervisor must remain alive.

For a description of `Type`, see [`start/1,2`](`start/1`).

# `unload`

```elixir
-spec unload(Application) -> ok | {error, Reason} when Application :: atom(), Reason :: term().
```

Unloads the application specification for `Application` from the application
controller. It also unloads the application specifications for any included
applications. Notice that the function does not purge the Erlang object code.

# `unset_env`

```elixir
-spec unset_env(Application, Par) -> ok when Application :: atom(), Par :: atom().
```

# `unset_env`

```elixir
-spec unset_env(Application, Par, Opts) -> ok
                   when
                       Application :: atom(),
                       Par :: atom(),
                       Opts :: [{timeout, timeout()} | {persistent, boolean()}].
```

Removes the configuration parameter `Par` and its value for `Application`.

[`unset_env/3`](`unset_env/3`) uses the standard `gen_server` time-out value
(5000 ms). Option `timeout` can be specified if another time-out value is
useful, for example, in situations where the application controller is heavily
loaded.

[`unset_env/3`](`unset_env/3`) also allows the persistent option to be passed
(see `set_env/4`).

> #### Warning {: .warning }
>
> Use this function only if you know what you are doing, that is, on your own
> applications. It is very application-dependent and configuration
> parameter-dependent when and how often the value is read by the application.
> Careless use of this function can put the application in a weird,
> inconsistent, and malfunctioning state.

# `which_applications`

```elixir
-spec which_applications() -> [{Application, Description, Vsn}]
                            when Application :: atom(), Description :: string(), Vsn :: string().
```

# `which_applications`

```elixir
-spec which_applications(Timeout) -> [{Application, Description, Vsn}]
                            when
                                Timeout :: timeout(),
                                Application :: atom(),
                                Description :: string(),
                                Vsn :: string().
```

Returns a list with information about the applications that are currently
running.

`Application` is the application name. `Description` and `Vsn` are the
values of their `description` and `vsn` application specification keys,
respectively.

A `Timeout` argument can be specified in situations where the application
controller is heavily loaded.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
