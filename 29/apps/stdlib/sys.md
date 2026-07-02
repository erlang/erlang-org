# `sys`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/sys.erl#L22)

A functional interface to system messages.

This module contains functions for sending system messages used by programs, and
messages used for debugging purposes.

Functions used for implementation of processes are also expected to understand
system messages, such as debug messages and code change. These functions must be
used to implement the use of system messages for a process; either directly, or
through standard behaviors, such as `m:gen_server`.

The default time-out is 5000 ms, unless otherwise specified. `timeout` defines
the time to wait for the process to respond to a request. If the process does
not respond, the function evaluates [`exit({timeout, {M, F, A}})`](`exit/1`).

[](){: #dbg_opt }

The functions make references to a debug structure. The debug structure is a
list of `t:dbg_opt/0`, which is an internal data type used by function
`handle_system_msg/6`. No debugging is performed if it is an empty list.

## System Messages

Processes that are not implemented as one of the standard behaviors must still
understand system messages. The following three messages must be understood:

- Plain system messages. These are received as `{system, From, Msg}`. The
  content and meaning of this message are not interpreted by the receiving
  process module. When a system message is received, function
  `handle_system_msg/6` is called to handle the request.
- Shutdown messages. If the process traps exits, it must be able to handle a
  shutdown request from its parent, the supervisor. The message
  `{'EXIT', Parent, Reason}` from the parent is an order to terminate. The
  process must terminate when this message is received, normally with the same
  `Reason` as `Parent`.
- If the modules used to implement the process change dynamically during
  runtime, the process must understand one more message. An example is the
  `m:gen_event` processes. The message is `{_Label, {From, Ref}, get_modules}`.
  The reply to this message is `From ! {Ref, Modules}`, where `Modules` is a
  list of the currently active modules in the process.

  This message is used by the release handler to find which processes that
  execute a certain module. The process can later be suspended and ordered to
  perform a code change for one of its modules.

## System Events

When debugging a process with the functions of this module, the process
generates _system_events_, which are then treated in the debug function. For
example, `trace` formats the system events to the terminal.

Four predefined system events are used when a process receives or sends a
message. The process can also define its own system events. It is always up to
the process itself to format these events.

# `dbg_fun`

```erlang
-type dbg_fun() ::
          fun((FuncState :: _, Event :: system_event(), ProcState :: _) -> done | (NewFuncState :: _)).
```

# `dbg_opt`

```erlang
-opaque dbg_opt()
```

See the introduction of this manual page.

# `debug_option`

```erlang
-type debug_option() ::
          trace | log |
          {log, N :: pos_integer()} |
          statistics |
          {log_to_file, FileName :: file:name()} |
          {install,
           {Func :: dbg_fun(), FuncState :: term()} |
           {FuncId :: term(), Func :: dbg_fun(), FuncState :: term()}}.
```

# `format_fun`
*not exported* 

```erlang
-type format_fun() ::
          fun((Device :: io:device() | file:io_device(), Event :: system_event(), Extra :: term()) ->
                  any()).
```

# `name`
*not exported* 

```erlang
-type name() :: pid() | atom() | {global, term()} | {via, module(), term()}.
```

# `system_event`

```erlang
-type system_event() ::
          {in, Msg :: _} |
          {in, Msg :: _, State :: _} |
          {out, Msg :: _, To :: _} |
          {out, Msg :: _, To :: _, State :: _} |
          {noreply, State :: _} |
          {continue, Continuation :: _} |
          {postpone, Event :: _, State :: _, NextState :: _} |
          {consume, Event :: _, State :: _, NextState :: _} |
          {start_timer, Action :: _, State :: _} |
          {insert_timeout, Event :: _, State :: _} |
          {enter, Module :: module(), State :: _} |
          {module, Module :: module(), State :: _} |
          {terminate, Reason :: _, State :: _} |
          term().
```

Debug events produced by `m:gen_server`, `m:gen_statem` and `m:gen_event`

- **`{in,Msg}`** - Is produced by `m:gen_server` and `m:gen_event` when the message
  `Msg` arrives.

- **`{in,Msg,State}`** - Is produced by `m:gen_statem` when the message `Msg`
  arrives in state `State`.

  For `m:gen_statem` the `Msg` term is an `{EventType,EventContent}` tuple.

- **`{out,Msg,To}`** - Is produced by `m:gen_statem` when the reply `Msg` is sent
  back to `To` by returning a `{reply,To,Msg}` action from the callback module.

  `To` is of the same type as the first argument to `gen_statem:reply/2`.

- **`{out,Msg,To,State}`** - Is produced by `m:gen_server` when the reply `Msg` is
  sent back to `To` by returning a `{reply,...}` tuple from the callback module.

  `To` is of the same type as the first argument to `gen_server:reply/2`.

  `State` is the new server state.

- **`{noreply,State}`** - Is produced by `m:gen_server` when a `{noreply,...}`
  tuple is returned from the callback module.

  `State` is the new server state.

- **`{continue,Continuation}`** - Is produced by `m:gen_server` when a
  `{continue,Continuation}` tuple is returned from the callback module.

- **`{postpone,Event,State,NextState}`** - Is produced by `m:gen_statem` when the
  message `Event` is postponed in state `State`. `NextState` is the new state.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{consume,Event,State,NextState}`** - Is produced by `m:gen_statem` when the
  message `Event` is consumed in state `State`. `NextState` is the new state.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{start_timer,Action,State}`** - Is produced by `m:gen_statem` when the action
  `Action` starts a timer in state `State`.

- **`{insert_timeout,Event,State}`** - Is produced by `m:gen_statem` when a
  timeout zero action inserts event `Event` in state `State`.

  `Event` is an `{EventType,EventContent}` tuple.

- **`{enter,Module,State}`** - Is produced by `m:gen_statem` when module `Module`
  enters the first state `State`.

- **`{module,Module,State}`** - Is produced by `m:gen_statem` when setting module
  `Module` in state `State`.

- **`{terminate,Reason,State}`** - Is produced by `m:gen_statem` when it
  terminates with reason `Reason` in state `State`.

# `system_code_change`

```erlang
-callback system_code_change(Misc, Module, OldVsn, Extra) -> {ok, NMisc}
                                when
                                    Misc :: term(),
                                    OldVsn :: undefined | term(),
                                    Module :: atom(),
                                    Extra :: term(),
                                    NMisc :: term().
```

Called from `handle_system_msg/6` when the process is to perform a code change.
The code change is used when the internal data structure has changed. This
function converts argument `Misc` to the new data structure. `OldVsn` is
attribute _vsn_ of the old version of the `Module`. If no such attribute is
defined, the atom `undefined` is sent.

# `system_continue`

```erlang
-callback system_continue(Parent, Debug, Misc) -> no_return()
                             when Parent :: pid(), Debug :: [dbg_opt()], Misc :: term().
```

Called from `handle_system_msg/6` when the process is to continue its execution
(for example, after it has been suspended). This function never returns.

# `system_get_state`
*since OTP 17.0* 

```erlang
-callback system_get_state(Misc) -> {ok, State} when Misc :: term(), State :: term().
```

Called from `handle_system_msg/6` when the process is to return a term that
reflects its current state. `State` is the value returned by `get_state/2`.

# `system_replace_state`
*since OTP 17.0* 

```erlang
-callback system_replace_state(StateFun, Misc) -> {ok, NState, NMisc}
                                  when
                                      Misc :: term(),
                                      NState :: term(),
                                      NMisc :: term(),
                                      StateFun :: fun((State :: term()) -> NState).
```

Called from `handle_system_msg/6` when the process is to replace its current
state. `NState` is the value returned by `replace_state/3`.

# `system_terminate`

```erlang
-callback system_terminate(Reason, Parent, Debug, Misc) -> no_return()
                              when Reason :: term(), Parent :: pid(), Debug :: [dbg_opt()], Misc :: term().
```

Called from `handle_system_msg/6` when the process is to terminate. For example,
this function is called when the process is suspended and its parent orders
shutdown. It gives the process a chance to do a cleanup. This function never
returns.

# `change_code`

```erlang
-spec change_code(Name, Module, OldVsn, Extra) -> ok | {error, Reason}
                     when
                         Name :: name(),
                         Module :: module(),
                         OldVsn :: undefined | term(),
                         Extra :: term(),
                         Reason :: term().
```

# `change_code`

```erlang
-spec change_code(Name, Module, OldVsn, Extra, Timeout) -> ok | {error, Reason}
                     when
                         Name :: name(),
                         Module :: module(),
                         OldVsn :: undefined | term(),
                         Extra :: term(),
                         Timeout :: timeout(),
                         Reason :: term().
```

Tells the process to change code.

The process must be suspended to handle this message.
Argument `Extra` is reserved for each process to use as its own.
Function [`Module:system_code_change/4`](`c:system_code_change/4`) is called.
`OldVsn` is the old version of the `Module`.

# `get_state`
*since OTP R16B01* 

```erlang
-spec get_state(Name) -> State when Name :: name(), State :: term().
```

# `get_state`
*since OTP R16B01* 

```erlang
-spec get_state(Name, Timeout) -> State when Name :: name(), Timeout :: timeout(), State :: term().
```

Gets the state of the process.

> #### Note {: .info }
>
> These functions are intended only to help with debugging. They are provided
> for convenience, allowing developers to avoid having to create their own state
> extraction functions and also avoid having to interactively extract the state
> from the return values of `get_status/1` or `get_status/2` while debugging.

The value of `State` varies for different types of processes, as follows:

- For a `m:gen_server` process, the returned `State` is the state of the
  callback module.
- For a `m:gen_statem` process, `State` is the tuple
  `{CurrentState,CurrentData}`.
- For a `m:gen_event` process, `State` is a list of tuples, where each tuple
  corresponds to an event handler registered in the process and contains
  `{Module, Id, HandlerState}`, as follows:

  - **`Module`** - The module name of the event handler.

  - **`Id`** - The ID of the handler (which is `false` if it was registered
    without an ID).

  - **`HandlerState`** - The state of the handler.

If the callback module exports a function
[`system_get_state/1`](`c:system_get_state/1`), it is called in the target
process to get its state. Its argument is the same as the `Misc` value returned
by [`get_status/1,2`](`get_status/1`), and function
[`Module:system_get_state/1`](`c:system_get_state/1`) is expected to extract the
state of the callback module from it. Function
[`system_get_state/1`](`c:system_get_state/1`) must return `{ok, State}`, where
`State` is the state of the callback module.

If the callback module does not export a
[`system_get_state/1`](`c:system_get_state/1`) function, `get_state/1,2` assumes
that the `Misc` value is the state of the callback module and returns it
directly instead.

If the callback module's [`system_get_state/1`](`c:system_get_state/1`) function
crashes or throws an exception, the caller exits with error
`{callback_failed, {Module, system_get_state}, {Class, Reason}}`, where `Module`
is the name of the callback module and `Class` and `Reason` indicate details of
the exception.

Function [`system_get_state/1`](`c:system_get_state/1`) is primarily useful for
user-defined behaviors and modules that implement OTP
[special processes](`m:sys#process-implementation-functions`). The `m:gen_server`,
`m:gen_statem`, and `m:gen_event` OTP behavior modules export this function, so
callback modules for those behaviors need not to supply their own.

For more information about a process, including its state, see `get_status/1`
and `get_status/2`.

# `get_status`

```erlang
-spec get_status(Name) -> Status
                    when
                        Name :: name(),
                        Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
                        SItem ::
                            (PDict :: [{Key :: term(), Value :: term()}]) |
                            (SysState :: running | suspended) |
                            (Parent :: pid()) |
                            (Dbg :: [dbg_opt()]) |
                            (Misc :: term()).
```

# `get_status`

```erlang
-spec get_status(Name, Timeout) -> Status
                    when
                        Name :: name(),
                        Timeout :: timeout(),
                        Status :: {status, Pid :: pid(), {module, Module :: module()}, [SItem]},
                        SItem ::
                            (PDict :: [{Key :: term(), Value :: term()}]) |
                            (SysState :: running | suspended) |
                            (Parent :: pid()) |
                            (Dbg :: [dbg_opt()]) |
                            (Misc :: term()).
```

Gets the status of the process.

The value of `Misc` varies for different types of processes, for example:

- A `m:gen_server` process returns the state of the callback module.
- A `m:gen_statem` process returns information, such as its current state name
  and state data.
- A `m:gen_event` process returns information about each of its registered
  handlers.
- A bare `m:sys` process returns the value passed as `Misc` to
  `handle_system_msg/6`.

Callback modules for `m:gen_server`, `m:gen_statem`, and `m:gen_event` can also change
the value of `Misc` by exporting a function `format_status/1`, which contributes
module-specific information. For details, see `c:gen_server:format_status/1`,
`c:gen_statem:format_status/1`, and `c:gen_event:format_status/1`.

# `install`

```erlang
-spec install(Name, FuncSpec) -> ok
                 when
                     Name :: name(),
                     FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
                     FuncId :: term(),
                     Func :: dbg_fun(),
                     FuncState :: term().
```

# `install`

```erlang
-spec install(Name, FuncSpec, Timeout) -> ok
                 when
                     Name :: name(),
                     FuncSpec :: {Func, FuncState} | {FuncId, Func, FuncState},
                     FuncId :: term(),
                     Func :: dbg_fun(),
                     FuncState :: term(),
                     Timeout :: timeout().
```

Enables installation of alternative debug functions. An example of such a
function is a trigger, a function that waits for some special event and performs
some action when the event is generated. For example, turning on low-level
tracing.

`Func` is called whenever a system event is generated. This function is to
return `done`, or a new `Func` state. In the first case, the function is
removed. It is also removed if the function fails. If one debug function should
be installed more times, a unique `FuncId` must be specified for each
installation.

# `log`

```erlang
-spec log(Name, Flag) -> ok | {ok, [system_event()]}
             when Name :: name(), Flag :: true | {true, N :: pos_integer()} | false | get | print.
```

# `log`

```erlang
-spec log(Name, Flag, Timeout) -> ok | {ok, [system_event()]}
             when
                 Name :: name(),
                 Flag :: true | {true, N :: pos_integer()} | false | get | print,
                 Timeout :: timeout().
```

Turns the logging of system events on or off. If on, a maximum of `N` events are
kept in the debug structure (default is 10).

If `Flag` is `get`, a list of all logged events is returned.

If `Flag` is `print`, the logged events are printed to
[`standard_io`](`t:io:standard_io/0`).

The events are formatted with a function that is defined by the process that
generated the event (with a call to [`handle_debug/4`)](`handle_debug/4`).

# `log_to_file`

```erlang
-spec log_to_file(Name, Flag) -> ok | {error, open_file}
                     when Name :: name(), Flag :: (FileName :: string()) | false.
```

# `log_to_file`

```erlang
-spec log_to_file(Name, Flag, Timeout) -> ok | {error, open_file}
                     when Name :: name(), Flag :: (FileName :: string()) | false, Timeout :: timeout().
```

Enables or disables the logging of all system events in text format to the file.
The events are formatted with a function that is defined by the process that
generated the event (with a call to `handle_debug/4`). The file is opened with
encoding UTF-8.

# `no_debug`

```erlang
-spec no_debug(Name) -> ok when Name :: name().
```

# `no_debug`

```erlang
-spec no_debug(Name, Timeout) -> ok when Name :: name(), Timeout :: timeout().
```

Turns off all debugging for the process. This includes functions that are
installed explicitly with function [`install/2,3`](`install/2`), for example,
triggers.

# `remove`

```erlang
-spec remove(Name, Func | FuncId) -> ok when Name :: name(), Func :: dbg_fun(), FuncId :: term().
```

# `remove`

```erlang
-spec remove(Name, Func | FuncId, Timeout) -> ok
                when Name :: name(), Func :: dbg_fun(), FuncId :: term(), Timeout :: timeout().
```

Removes an installed debug function from the process. `Func` or `FuncId` must be
the same as previously installed.

# `replace_state`
*since OTP R16B01* 

```erlang
-spec replace_state(Name, StateFun) -> NewState
                       when
                           Name :: name(),
                           StateFun :: fun((State :: term()) -> NewState :: term()),
                           NewState :: term().
```

# `replace_state`
*since OTP R16B01* 

```erlang
-spec replace_state(Name, StateFun, Timeout) -> NewState
                       when
                           Name :: name(),
                           StateFun :: fun((State :: term()) -> NewState :: term()),
                           Timeout :: timeout(),
                           NewState :: term().
```

Replaces the state of the process, and returns the new state.

> #### Note {: .info }
>
> These functions are intended only to help with debugging, and are not to be
> called from normal code. They are provided for convenience, allowing
> developers to avoid having to create their own custom state replacement
> functions.

Function `StateFun` provides a new state for the process. Argument `State` and
the `NewState` return value of `StateFun` vary for different types of processes
as follows:

- For a `m:gen_server` process, `State` is the state of the callback module and
  `NewState` is a new instance of that state.
- For a `m:gen_statem` process, `State` is the tuple
  `{CurrentState,CurrentData}`, and `NewState` is a similar tuple, which can
  contain a new current state, new state data, or both.
- For a `m:gen_event` process, `State` is the tuple `{Module, Id, HandlerState}`
  as follows:

  - **`Module`** - The module name of the event handler.

  - **`Id`** - The ID of the handler (which is `false` if it was registered
    without an ID).

  - **`HandlerState`** - The state of the handler.

  `NewState` is a similar tuple where `Module` and `Id` are to have the same
  values as in `State`, but the value of `HandlerState` can be different.
  Returning a `NewState`, whose `Module` or `Id` values differ from those of
  `State`, leaves the state of the event handler unchanged. For a `m:gen_event`
  process, `StateFun` is called once for each event handler registered in the
  `m:gen_event` process.

If a `StateFun` function decides not to effect any change in process state, then
regardless of process type, it can return its `State` argument.

If a `StateFun` function crashes or throws an exception, the original state of
the process is unchanged for `m:gen_server`, and `m:gen_statem` processes. For
`m:gen_event` processes, a crashing or failing `StateFun` function means that only
the state of the particular event handler it was working on when it failed or
crashed is unchanged; it can still succeed in changing the states of other event
handlers registered in the same `m:gen_event` process.

If the callback module exports a `c:system_replace_state/2` function, it is
called in the target process to replace its state using `StateFun`. Its two
arguments are `StateFun` and `Misc`, where `Misc` is the same as the `Misc`
value returned by [`get_status/1,2`](`get_status/1`). A
[`system_replace_state/2`](`c:system_replace_state/2`) function is expected to
return `{ok, NewState, NewMisc}`, where `NewState` is the new state of the
callback module, obtained by calling `StateFun`, and `NewMisc` is a possibly new
value used to replace the original `Misc` (required as `Misc` often contains the
state of the callback module within it).

If the callback module does not export a
[`system_replace_state/2`](`c:system_replace_state/2`) function,
[`replace_state/2,3`](`replace_state/2`) assumes that `Misc` is the state of the
callback module, passes it to `StateFun` and uses the return value as both the
new state and as the new value of `Misc`.

If the callback module's function
[`system_replace_state/2`](`c:system_replace_state/2`) crashes or throws an
exception, the caller exits with error
`{callback_failed, {Module, system_replace_state}, {Class, Reason}}`, where
`Module` is the name of the callback module and `Class` and `Reason` indicate
details of the exception. If the callback module does not provide a
[`system_replace_state/2`](`c:system_replace_state/2`) function and `StateFun`
crashes or throws an exception, the caller exits with error
`{callback_failed, StateFun, {Class, Reason}}`.

Function [`system_replace_state/2`](`c:system_replace_state/2`) is primarily
useful for user-defined behaviors and modules that implement OTP
[special processes](`m:sys#process-implementation-functions`). The OTP behavior
modules `m:gen_server`, `m:gen_statem`, and `m:gen_event` export this function, so
callback modules for those behaviors need not to supply their own.

# `resume`

```erlang
-spec resume(Name) -> ok when Name :: name().
```

# `resume`

```erlang
-spec resume(Name, Timeout) -> ok when Name :: name(), Timeout :: timeout().
```

Resumes a suspended process.

# `statistics`

```erlang
-spec statistics(Name, Flag) -> ok | {ok, Statistics}
                    when
                        Name :: name(),
                        Flag :: true | false | get,
                        Statistics :: [StatisticsTuple] | no_statistics,
                        StatisticsTuple ::
                            {start_time, DateTime1} |
                            {current_time, DateTime2} |
                            {reductions, non_neg_integer()} |
                            {messages_in, non_neg_integer()} |
                            {messages_out, non_neg_integer()},
                        DateTime1 :: file:date_time(),
                        DateTime2 :: file:date_time().
```

# `statistics`

```erlang
-spec statistics(Name, Flag, Timeout) -> ok | {ok, Statistics}
                    when
                        Name :: name(),
                        Flag :: true | false | get,
                        Statistics :: [StatisticsTuple] | no_statistics,
                        StatisticsTuple ::
                            {start_time, DateTime1} |
                            {current_time, DateTime2} |
                            {reductions, non_neg_integer()} |
                            {messages_in, non_neg_integer()} |
                            {messages_out, non_neg_integer()},
                        DateTime1 :: file:date_time(),
                        DateTime2 :: file:date_time(),
                        Timeout :: timeout().
```

Enables or disables the collection of statistics. If `Flag` is `get`, the
statistical collection is returned.

# `suspend`

```erlang
-spec suspend(Name) -> ok when Name :: name().
```

# `suspend`

```erlang
-spec suspend(Name, Timeout) -> ok when Name :: name(), Timeout :: timeout().
```

Suspends the process. When the process is suspended, it only responds to other
system messages, but not other messages.

# `terminate`
*since OTP 18.0* 

```erlang
-spec terminate(Name, Reason) -> ok when Name :: name(), Reason :: term().
```

# `terminate`
*since OTP 18.0* 

```erlang
-spec terminate(Name, Reason, Timeout) -> ok when Name :: name(), Reason :: term(), Timeout :: timeout().
```

Orders the process to terminate with the specified `Reason`. The termination is
done asynchronously, so it is not guaranteed that the process is terminated when
the function returns.

# `trace`

```erlang
-spec trace(Name, Flag) -> ok when Name :: name(), Flag :: boolean().
```

# `trace`

```erlang
-spec trace(Name, Flag, Timeout) -> ok when Name :: name(), Flag :: boolean(), Timeout :: timeout().
```

Prints all system events on [`standard_io`](`t:io:standard_io/0`). The events
are formatted with a function that is defined by the process that generated the
event (with a call to `handle_debug/4`).

# `debug_options`

```erlang
-spec debug_options([Opt :: debug_option()]) -> [dbg_opt()].
```

Can be used by a process that initiates a debug structure from a list of
options. The values of argument `Opt` are the same as for the corresponding
functions.

# `get_debug`

> This function is deprecated. sys:get_debug/3 is deprecated; incorrectly documented and only for internal use. Can often be replaced with sys:get_log/1.

```erlang
-spec get_debug(Item, Debug, Default) -> term()
                   when Item :: log | statistics, Debug :: [dbg_opt()], Default :: term().
```

Gets the data associated with a debug option. `Default` is returned if `Item` is
not found. Can be used by the process to retrieve debug data for printing before
it terminates.

# `get_log`
*since OTP-22.0* 

```erlang
-spec get_log(Debug) -> [system_event()] when Debug :: [dbg_opt()].
```

Returns the logged system events in the debug structure, that is the last
argument to `handle_debug/4`.

# `handle_debug`

```erlang
-spec handle_debug(Debug, FormFunc, Extra, Event) -> [dbg_opt()]
                      when
                          Debug :: [dbg_opt()],
                          FormFunc :: format_fun(),
                          Extra :: term(),
                          Event :: system_event().
```

This function is called by a process when it generates a system event.
`FormFunc` is a formatting function, called as `FormFunc(Device, Event, Extra)`
to print the events, which is necessary if tracing is activated. `Extra` is any
extra information that the process needs in the format function, for example,
the process name.

# `handle_system_msg`

```erlang
-spec handle_system_msg(Msg, From, Parent, Module, Debug, Misc) -> no_return()
                           when
                               Msg :: term(),
                               From :: {pid(), Tag :: _},
                               Parent :: pid(),
                               Module :: module(),
                               Debug :: [dbg_opt()],
                               Misc :: term().
```

This function is used by a process module to take care of system messages. The
process receives a `{system, From, Msg}` message and passes `Msg` and `From` to
this function.

This function _never_ returns. It calls either of the following functions:

- [`Module:system_continue(Parent, NDebug, Misc)`](`c:system_continue/3`), where
  the process continues the execution.
- [`Module:system_terminate(Reason, Parent, Debug, Misc)`](`c:system_terminate/4`),
  if the process is to terminate.

`Module` must export the following:

- [`system_continue/3`](`c:system_continue/3`)
- [`system_terminate/4`](`c:system_terminate/4`)
- [`system_code_change/4`](`c:system_code_change/4`)
- [`system_get_state/1`](`c:system_get_state/1`)
- [`system_replace_state/2`](`c:system_replace_state/2`)

Argument `Misc` can be used to save internal data in a process, for example, its
state. It is sent to [`Module:system_continue/3`](`c:system_continue/3`) or
[`Module:system_terminate/4`](`c:system_terminate/4`).

# `print_log`

```erlang
-spec print_log(Debug) -> ok when Debug :: [dbg_opt()].
```

Prints the logged system events in the debug structure, using `FormFunc` as
defined when the event was generated by a call to `handle_debug/4`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
