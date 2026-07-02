# `gen_server`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/gen_server.erl#L22)

Generic server behavior.

This behavior module provides the server in a client-server relation.
A generic server process (`gen_server`) implemented using this module
has a standard set of interface functions and includes functionality
for tracing and error reporting.  It also fits into
an OTP supervision tree. For more information, see section
[gen_server Behaviour](`e:system:gen_server_concepts.md`)
in OTP Design Principles.

A `gen_server` process assumes all specific parts to be located
in a callback module exporting a predefined set of functions.
The relationship between the behavior functions
and the callback functions is as follows:

```text
gen_server module            Callback module
-----------------            ---------------
gen_server:start
gen_server:start_monitor
gen_server:start_link -----> Module:init/1

gen_server:stop       -----> Module:terminate/2

gen_server:call
gen_server:send_request
gen_server:multi_call -----> Module:handle_call/3

gen_server:cast
gen_server:abcast     -----> Module:handle_cast/2

-                     -----> Module:handle_info/2

-                     -----> Module:handle_continue/2

-                     -----> Module:terminate/2

-                     -----> Module:code_change/3
```

If a callback function fails or returns a bad value,
the `gen_server` process terminates.  However, an exception of class
[`throw`](`erlang:throw/1`) is not regarded as an error
but as a valid return, from all callback functions.

A `gen_server` process handles system messages as described in `m:sys`.
The `m:sys` module can be used for debugging a `gen_server` process.

Notice that a `gen_server` process does not trap exit signals
automatically, this must be explicitly initiated in the callback module.

Unless otherwise stated, all functions in this module fail
if the specified `gen_server` process does not exist
or if bad arguments are specified.

The `gen_server` process can go into hibernation (see `erlang:hibernate/3`)
if a callback function specifies `'hibernate'` instead of a time-out value.
This can be useful if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies at least
two garbage collections (when hibernating and shortly after waking up)
and is not something you want to do between each call to a busy server.

If the `gen_server` process needs to perform an action after
initialization or to break the execution of a callback into multiple steps,
it can return `{continue, Continue}` in place of
the time-out or hibernation value, which will invoke
the [`Module:handle_continue/2`](`c:handle_continue/2`) callback,
before receiving any external message / request.

If the `gen_server` process terminates, e.g. as a result of a function
in the callback module returning `{stop,Reason,NewState}`,
an exit signal with this `Reason` is sent to linked processes and ports.
See [Processes](`e:system:ref_man_processes.md#errors`)
in the Reference Manual for details regarding error handling
using exit signals.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_][1]
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
> Blocking signaling can, for example, cause call time-outs
> in `gen_server` to be significantly delayed.

[1]: `e:system:ref_man_processes.md#blocking-signaling-over-distribution`

## See Also

`m:gen_event`, `m:gen_statem`, `m:proc_lib`, `m:supervisor`, `m:sys`

# `action`

```elixir
-type action() ::
          (Time :: timeout()) |
          hibernate |
          {timeout, Time :: timeout(), Message :: term()} |
          {timeout,
           Time :: timeout(),
           Message :: term(),
           Options :: timeout_option() | [timeout_option()]} |
          {hibernate, Time :: timeout(), Message :: term()} |
          {hibernate,
           Time :: timeout(),
           Message :: term(),
           Options :: timeout_option() | [timeout_option()]} |
          {continue, Continue :: term()}.
```

Callback and server loop action.

Returned by the callbacks [`Module:init/1`](`c:init/1`),
[`Module:handle_call/3`](`c:handle_call/3`),
[`Module:handle_cast/2`](`c:handle_cast/2`),
[`Module:handle_info/2`](`c:handle_info/2`),
and [`Module:handle_continue/2`](`c:handle_continue/2`).
It is also the last argument to [`enter_loop/4,5`](`enter_loop/4`).

`t:action/0` is one of:

- **`Time :: `[`timeout()`](`t:timeout/0`)** -
  If the action is an integer `Time`, a time-out occurs
  unless a request or a message is received within that many milliseconds.
  A time-out is represented by the atom `timeout` as the `Info` argument
  to be handled by the [`Module:handle_info/2`](`c:handle_info/2`)
  callback function.  The action `infinity` can be used to wait indefinitely,
  which is the default when there is no `t:action/0` specified.

  For `Time = 0`, if there is a request or message waiting to be received,
  it interrupts (cancels) the time-out.

  > #### Note {: .info }
  > A system message restarts the time-out, which is a known
  > and unfortunate flaw in its implementation.  This also applies to
  > stray (cancelled) timer messages from the
  > `{timeout|hibernate, ...}` time-outs described below,
  > so it is recommended to not use them
  > in combination with this legacy time-out type.

- **`{timeout, Time, Message}`\
  `{timeout, Time, Message, Options}`** - Like `Time` above,
  but the delivered `Info` argument is specified by `Message`,
  and the time-out is not affected by system messages.

  `Options` can be used to trigger the time-out at an absolute point in time
  instead.  See `t:timeout_option/0` and `erlang:start_timer/4` for details.

  A relative time-out with `Time = 0` is immediately delivered,
  before any request or message is received, including system messages.

  A time-out with `Time = infinity` will never be delivered
  so the action is ignored; no time-out is started.

- **`{hibernate, Time, Message}`\
  `{hibernate, Time, Message, Options}`** - A combination of
  the action `hibernate` below, and  `{timeout, Time, Message}`
  or `{timeout, Time, Message, Options}` above.

  The process goes into hibernation while waiting for the next
  request or message to arrive, or for the time-out to expire.

- **`hibernate`** - The process goes into hibernation
  (by calling `erlang:hibernate/0`), waiting for the next
  request or message to arrive

- **`{continue, Continue}`** - The process will immediately execute the
  [`Module:handle_continue/2`](`c:handle_continue/2`) callback function,
  with `Continue` as the first argument.

# `enter_loop_opt`

```elixir
-type enter_loop_opt() ::
          {hibernate_after, HibernateAfterTimeout :: timeout()} | {debug, Dbgs :: [sys:debug_option()]}.
```

Server start options for the [`start`](`start_link/4`) or
[`enter_loop`](`enter_loop/5`) functions.

Options that can be used when starting a `gen_server` server through
[`enter_loop/3-5`](`enter_loop/5`) or the start functions such as
[`start_link/3,4`](`start_link/4`).

- **`{hibernate_after, HibernateAfterTimeout}`** - Specifies that the
  `gen_server` process awaits any message for `HibernateAfterTimeout`
  milliseconds and if no message is received, the process goes into
  hibernation automatically (by calling `proc_lib:hibernate/3`).

- **`{debug, Dbgs}`** - For every entry in `Dbgs`,
  the corresponding function in `m:sys` is called.

# `format_status`

```elixir
-type format_status() ::
          #{state => term(), message => term(), reason => term(), log => [sys:system_event()]}.
```

A map that describes the `gen_server` status.

The keys are:
- **`state`** - The internal state of the `gen_server` process.
- **`message`** - The message that caused the server to terminate.
- **`reason`** - The reason that caused the server to terminate.
- **`log`** - The [sys log](`sys:log/2`) of the server.

New associations may be added to the status map without prior notice.

# `from`

```elixir
-type from() :: {Client :: pid(), Tag :: reply_tag()}.
```

A call's reply destination.

Destination, given to the `gen_server` as the first argument
to the callback function [`Module:handle_call/3`](`c:handle_call/3`),
to be used by the when replying through `reply/2` (instead of
through the callback function's return value), to the process `Client`
that has called the `gen_server` using [`call/2,3`](`call/2`).
`Tag` is a term that is unique for this call/request instance.

# `reply_tag`

```elixir
-opaque reply_tag() :: gen:reply_tag().
```

A handle that associates a reply to the corresponding request.

# `request_id`

```elixir
-opaque request_id() :: gen:request_id().
```

An opaque request identifier. See `send_request/2` for details.

# `request_id_collection`

```elixir
-opaque request_id_collection() :: gen:request_id_collection().
```

An opaque collection of request identifiers (`t:request_id/0`).

Each request identifier can be associated with a label
chosen by the user.  For more information see `reqids_new/0`.

# `response_timeout`
*not exported* 

```elixir
-type response_timeout() :: timeout() | {abs, integer()}.
```

Response time-out for an asynchronous call.

Used to set a time limit on how long to wait for a response using either
`receive_response/2`, `receive_response/3`, `wait_response/2`, or
`wait_response/3`. The time unit used is `millisecond`.

Currently valid values:

- **`0..4294967295`** - Time-out relative to current time in milliseconds.

- **`infinity`** - Infinite time-out. That is,
  the operation will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`)
  time-out in milliseconds. That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`)
  returns a value larger than or equal to `Timeout`.
  `Timeout` is not allowed to identify a time further into the future
  than `4294967295` milliseconds.  Specifying the time-out
  using an absolute value is especially handy when you have
  a deadline for responses corresponding to a complete collection
  of requests (`t:request_id_collection/0`), since you do not have to
  recalculate the relative time until the deadline over and over again.

# `server_name`

```elixir
-type server_name() ::
          {local, LocalName :: atom()} |
          {global, GlobalName :: term()} |
          {via, RegMod :: module(), ViaName :: term()}.
```

Server name specification: `local`, `global`, or `via` registered.

To be used when starting a `gen_server`.  See functions
[`start/3,4`](`start/3`),
[`start_link/3,4`](`start_link/3`),
[`start_monitor/3,4`](`start_monitor/3`),
[`enter_loop/3,4,5`](`enter_loop/3`), and the type `t:server_ref/0`.

- **`{local, LocalName}`** - Register the `gen_server` locally
  as `LocalName` using [`register/2`](`erlang:register/2`).

- **`{global, GlobalName}`** - Register the `gen_server` process id
  globally as `GlobalName` using `global:register_name/2`.

- **`{via, RegMod, ViaName}`** - Register the `gen_server` process
  with the registry represented by `RegMod`. The `RegMod` callback
  is to export the functions `register_name/2`, `unregister_name/1`,
  `whereis_name/1`, and `send/2`, which are to behave like
  the corresponding functions in `m:global`.
  Thus, `{via, global, GlobalName}` is a valid reference
  equivalent to `{global, GlobalName}`.

# `server_ref`

```elixir
-type server_ref() ::
          pid() |
          (LocalName :: atom()) |
          {Name :: atom(), Node :: atom()} |
          {global, GlobalName :: term()} |
          {via, RegMod :: module(), ViaName :: term()}.
```

Server specification: `t:pid/0` or registered `t:server_name/0`.

To be used when addressing a `gen_server`.  See [`call/2,3`](`call/2`),
`cast/2`, `send_request/2`, `check_response/2`, `wait_response/2`,
[`stop/2,3`](`stop/1`) and the type `t:server_name/0`.

It can be:

- **`t:pid/0`** - The `gen_server`'s process identifier.

- **`LocalName`** - The `gen_server` is locally registered
  as `LocalName` with [`register/2`](`erlang:register/2`).

- **`{Name,Node}`** - The `gen_server` is locally registered
  on another node.

- **`{global, GlobalName}`** - The `gen_server` is globally registered
  in `m:global`.

- **`{via, RegMod, ViaName}`** - The `gen_server` is registered
  in an alternative process registry.  See the same term
  described for `t:server_name/0`.

# `start_mon_ret`

```elixir
-type start_mon_ret() ::
          {ok, {Pid :: pid(), MonRef :: reference()}} | ignore | {error, Reason :: term()}.
```

Return value from the [`start_monitor/3,4`](`start_monitor/3`) functions.

The same as type `t:start_ret/0` except that for a succesful start
it returns both the process identifier `Pid`
and a [`monitor/2,3`](`erlang:monitor/2`) [`MonRef`](`t:reference/0`).

# `start_opt`

```elixir
-type start_opt() ::
          {timeout, Timeout :: timeout()} |
          {spawn_opt, SpawnOptions :: [proc_lib:start_spawn_option()]} |
          enter_loop_opt().
```

Server start options for the [`start` functions](`start_link/3`).

Options that can be used when starting a `gen_server` server through,
for example, [`start_link/3,4`](`start_link/4`).

- **`{timeout, Timeout}`** - How many milliseconds
  the `gen_server` process is allowed to spend initializing
  or it is terminated and the start function returns `{error, timeout}`.

- **`{spawn_opt, SpawnOptions}`** - The `SpawnOptions` option list
  is passed to the function used to spawn the `gen_server`;
  see `t:proc_lib:start_spawn_option/0`).

  > #### Note {: .info }
  >
  > Using spawn option `monitor` is not allowed -
  > it causes a `badarg` failure.

- **`t:enter_loop_opt/0`** - See the type `t:enter_loop_opt/0`
  below for more start options that are also allowed
  by [`enter_loop/3,4,5`](`enter_loop/3`).

# `start_ret`

```elixir
-type start_ret() :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
```

Return value from the [`start/3,4`](`start/3`) and
[`start_link/3,4`](`start_link/3`) functions.

- **`{ok, Pid}`** - The `gen_server` process was succesfully created and
  initialized, with the process identifier `Pid`.

- **`{error, {already_started, OtherPid}}`** - A process with the specified
  `ServerName` exists already with the process identifier `OtherPid`.
  This function failed to start a `gen_server`.  It exited with reason
  `normal` before calling [`Module:init/1`](`c:init/1`).

- **`{error, timeout}`** - The `gen_server` process failed to initialize
  since [`Module:init/1`](`c:init/1`) did not return within the
  [start time-out](`t:start_opt/0`). The `gen_server` process was killed
  with [`exit(_, kill)`](`erlang:exit/2`).

- **`ignore`** - The `gen_server` process failed to initialize since
  [`Module:init/1`](`c:init/1`) returned `ignore`.

- **`{error,Reason}`** - The `gen_server` process failed to initialize since
  [`Module:init/1`](`c:init/1`) returned `{stop,Reason}`, `{error,Reason}`,
  or it failed with reason `Reason`.

See [`Module:init/1`](`c:init/1`) about the exit reason
for the `gen_server` process when it fails to initialize.

# `timeout_option`
*not exported* 

```elixir
-type timeout_option() :: {abs, Abs :: boolean()}.
```

Time-out timer start option, to select absolute time of expiry.

If `Abs` is `true` an absolute timer is started,
and if it is `false` a relative, which is the default.
See [`erlang:start_timer/4`](`erlang:start_timer/4`) for details.

# `code_change`
*optional* 

```elixir
-callback code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) ->
                         {ok, NewState :: term()} | {error, Reason :: term()}.
```

Update the server state after code change.

This function is called by a `gen_server` process when it is to update
its internal state during a release upgrade/downgrade, that is,
when the instruction `{update, Module, Change, ...}`, is specified
in the [`appup`](`e:sasl:appup.md`) file.

For more information, see section
[Release Handling Instructions](`e:system:release_handling.md#instr`)
in OTP Design Principles.

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade, `OldVsn` is
`{down,Vsn}`.  `Vsn` is defined by the `vsn` attribute(s)
of the old version of the callback module `Module`.  If no such attribute
is defined, the version is the checksum of the Beam file.

`State` is the internal state of the `gen_server` process.

`Extra` is passed "as is" from the `{advanced,Extra}` part
of the update instruction.

If successful, the function must return the updated internal state.

If the function returns `{error,Reason}`,
the ongoing upgrade fails and rolls back to the old release.

> #### Note {: .info }
>
> If a release upgrade/downgrade with `Change = {advanced, Extra}`
> specified in the [`.appup`](`e:sasl:appup.md`) file is made when
> [`Module:code_change/3`](`c:code_change/3`) is not implemented,
> the callback call will crash with an `undef` error reason.

# `format_status`
*since OTP 25.0* *optional* 

```elixir
-callback format_status(Status) -> NewStatus when Status :: format_status(), NewStatus :: format_status().
```

Format/limit the status value.

This function is called by a `gen_server` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_server` status.
- The `gen_server` process terminates abnormally and logs an error.

This callback is used to limit the status of the process returned by
[`sys:get_status/1,2`](`sys:get_status/1`) or sent to `m:logger`.

The callback gets a map `Status` describing the current status
and shall return a map `NewStatus` with the same keys,
but it may transform some values.

Two possible use cases for this callback is to remove
sensitive information from the state to prevent it from being printed
in log files, or to compact large irrelevant status items
that would only clutter the logs.

Example:

```erlang
format_status(Status) ->
  maps:map(
    fun(state,State) ->
            maps:remove(private_key, State);
       (message,{password, _Pass}) ->
            {password, removed};
       (_,Value) ->
            Value
    end, Status).
```

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it. The
> `gen_server` module provides a default implementation
> of this function that returns the callback module state.
>
> If this callback is exported but fails,
> to hide possibly sensitive data,
> the default function will instead return the fact that
> [`Module:format_status/1`](`c:format_status/1`) has crashed.

# `format_status`
*since OTP R13B04* *optional* 

> This callback is deprecated. the callback gen_server:format_status(_,_) is deprecated; use format_status/1 instead.

```elixir
-callback format_status(Opt, StatusData) -> Status
                           when
                               Opt :: normal | terminate,
                               StatusData :: [PDict | State],
                               PDict :: [{Key :: term(), Value :: term()}],
                               State :: term(),
                               Status :: term().
```

Format/limit the status value.

This function is called by a `gen_server` process
in in order to format/limit the server state
for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get the
  `gen_server` status. `Opt` is set to the atom `normal`.
- The `gen_server` process terminates abnormally and logs an error.
  `Opt` is set to the atom `terminate`.

This function is useful for changing the form and appearance
of the `gen_server` status for these cases. A callback module
wishing to change the `sys:get_status/1,2` return value,
as well as how its status appears in termination error logs,
exports an instance of [`Module:format_status/2`](`c:format_status/2`)
that returns a term describing the current status
of the `gen_server` process.

`PDict` is the current value of the process dictionary
of the `gen_server` process..

`State` is the internal state of the `gen_server` process.

The function is to return `Status`, a term that changes the details
of the current state and status of the `gen_server` process.
There are no restrictions of the form `Status` can take,
but for the `sys:get_status/1,2` case (when `Opt` is `normal`),
the recommended form for the `Status` value is
`[{data, [{"State", Term}]}]`, where `Term` provides relevant details
of the `gen_server` state.  Following this recommendation is not required,
but it makes the callback module status consistent with the rest of
the `sys:get_status/1,2` return value.

One use for this function is to return compact alternative
state representations to avoid that large state terms are printed
in log files.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> of this function that returns the callback module state.

# `handle_call`

```elixir
-callback handle_call(Request :: term(), From :: from(), State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Action :: action()} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Action :: action()} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
```

Handle a call.

Whenever a `gen_server` process receives a request sent using
[`call/2,3`](`call/3`), [`multi_call/2,3,4`](`multi_call/4`),
or [`send_request/2,4`](`send_request/4`), this function is called
to handle the request.

`State` is the internal state of the `gen_server` process,
and `NewState` a possibly updated one.

`Request` is passed from the same argument provided
to `call` or `multi_call`.

The return value `Result` is interpreted as follows:

- **`{reply,Reply,NewState}`\
  `{reply,Reply,NewState,_}`** - The `Reply` value is sent back
 to the client request and there becomes its return value.

  The `gen_server` process continues executing with the possibly updated
  internal state `NewState`.

- **`{noreply,NewState}`\
  `{noreply,NewState,_}`** - The `gen_server` process
  continues executing with the possibly updated internal state `NewState`.

  A reply to the client request has to be created by calling
  [`reply(From, Reply)`](`reply/2`), either in this
  or in a later callback.

- **`{reply,_,_,Action}`\
  `{noreply,_,Action}`** - `Action` is described by the `t:action/0` type.

- **`{stop,Reason,NewState}`\
  `{stop,Reason,Reply,NewState}`** - The `gen_server` process will call
  [`Module:terminate(Reason,NewState)`](`c:terminate/2`),
  and then terminate.

  `{stop,_,Reply,_}` will create a reply to the client request just as
  `{reply,Reply,...}` while `{stop,_,_}` will not, so just as for
  `{noreply,NewState,...}` a reply has to be created by calling
  [`reply(From, Reply)`](`reply/2`) before returning `{stop,_,_}`.

# `handle_cast`

```elixir
-callback handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Action :: action()} |
                         {stop, Reason :: term(), NewState :: term()}.
```

Handle a cast message.

Whenever a `gen_server` process receives a request sent using `cast/2`
or [`abcast/2,3`](`abcast/2`), this function is called
to handle the request.

For a description of the arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).

# `handle_continue`
*since OTP 21.0* *optional* 

```elixir
-callback handle_continue(Info :: term(), State :: term()) ->
                             {noreply, NewState :: term()} |
                             {noreply, NewState :: term(), Action :: action()} |
                             {stop, Reason :: term(), NewState :: term()}.
```

Handle a callback continuation.

This function is called by a `gen_server` process whenever
a previous callback returns one of the tuples containing
`{continue, Continue}`.  The call is invoked immediately after
the previous callback, which makes it useful for performing work
after initialization or, for splitting the work in a callback
into multiple steps, updating the process state along the way.

For a description of the other arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need to export it
> only if they return one of the tuples containing `{continue,Continue}`
> from another callback.  If such a `{continue,_}` tuple is used
> and the callback is not implemented, the process will exit
> with `undef` error.

# `handle_info`
*optional* 

```elixir
-callback handle_info(Info :: timeout | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Action :: action()} |
                         {stop, Reason :: term(), NewState :: term()}.
```

Handle an info message (regular process message).

This function is called by a `gen_server` process when a time-out occurs
or when it receives any other message than a synchronous
or asynchronous request (or a system message).

`Info` is either the atom `timeout`, if a time-out has occurred,
or the received message.

For a description of the other arguments and possible return values,
see [`Module:handle_call/3`](`c:handle_call/3`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> of this function that logs about the unexpected `Info` message,
> drops it and returns `{noreply, State}`.

# `init`

```elixir
-callback init(Args :: term()) ->
                  {ok, State :: term()} |
                  {ok, State :: term(), Action :: action()} |
                  {stop, Reason :: term()} |
                  ignore |
                  {error, Reason :: term()}.
```

Initialize the server.

Whenever a `gen_server` process is started using [`start/3,4`](`start/3`),
[`start_monitor/3,4`](`start_monitor/3`),
or [`start_link/3,4`](`start_link/3`), this function is called
by the new process to initialize the server.

`Args` is the `Args` argument provided to the start function.

The return value `Result` is interpreted as follows:

- **`{ok,State}`\
  `{ok,State,_}`** - Initialization was succesful
   and `State` is the internal state of the `gen_server` process.

- **`{ok,_,Action}`**  - `Action` is described by the `t:action/0` type.

- **`{stop,Reason}`** - Initialization failed.  The `gen_server`
  process exits with reason `Reason`.

- **`{error,Reason}` _since OTP 26.0_\
  `ignore`** - Initialization failed. The `gen_server` process exits
  with reason `normal`.

See function [`start_link/3,4`](`start_link/3`)'s return value
`t:start_ret/0` in these different cases.

# `terminate`
*optional* 

```elixir
-callback terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: term()) -> term().
```

Handle server termination.

This function is called by a `gen_server` process
when it is about to terminate.

It is to be the opposite of [`Module:init/1`](`c:init/1`)
and do any necessary cleaning up.  When it returns,
the `gen_server` process terminates with `Reason`.
The return value is ignored.

`Reason` is a term denoting the stop reason and `State`
is the internal state of the `gen_server` process.

`Reason` depends on why the `gen_server` process is terminating.
If it is because another callback function has returned a stop tuple
`{stop,..}`, `Reason` has the value specified in that tuple.
If it is because of a failure, `Reason` is the error reason.

If the `gen_server` process is part of a supervision tree
and is ordered by its supervisor to terminate, this function is called
with `Reason=shutdown` if the following conditions apply:

- The `gen_server` process has been set to trap exit signals.
- The shutdown strategy as defined in the child specification
  of the supervisor is an integer time-out value, not `brutal_kill`.

Even if the `gen_server` process is _not_ part of a supervision tree,
this function is called if it receives an `'EXIT'` message from its parent.
`Reason` is the same as in the `'EXIT'` message.

If the `gen_server` process does not trap exits,
the `gen_server` process terminates immediately.

Notice that for any other reason than `normal`, `shutdown`, or
`{shutdown,Term}`, see `stop/3`, the `gen_server` process is assumed
to terminate because of an error, and an error report is issued
using `m:logger`.

When the gen_server process exits, an exit signal with the same reason
is sent to linked processes and ports.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_server` module provides a default implementation
> with no cleanup.

# `abcast`

```elixir
-spec abcast(Name :: atom(), Request :: term()) -> abcast.
```

Cast a request to multiple nodes.

Equivalent to [`abcast(Nodes, Name, Request)`](`abcast/3`)
where `Nodes` is all nodes connected to the calling node,
including the calling node itself.

# `abcast`

```elixir
-spec abcast(Nodes :: [node()], Name :: atom(), Request :: term()) -> abcast.
```

Cast a request to multiple nodes.

Sends an asynchronous request to the `gen_server` processes
locally registered as `Name` at the specified nodes.
The function returns immediately and ignores nodes that do not exist,
or where the `gen_server` `Name` does not exist.  The  `gen_server`
processes call [`Module:handle_cast/2`](`c:handle_cast/2`)
to handle the request.

For a description of the arguments,
see [`multi_call/2,3,4`](`multi_call/2`).

# `call`

```elixir
-spec call(ServerRef :: server_ref(), Request :: term()) -> Reply :: term().
```

# `call`

```elixir
-spec call(ServerRef :: server_ref(), Request :: term(), Timeout :: timeout()) -> Reply :: term().
```

Call a server: send request and wait for response.

Makes a synchronous call to the `ServerRef` of the `gen_server` process
by sending a request and waiting until a reply arrives
or a time-out occurs.  The `gen_server` process calls
[`Module:handle_call/3`](`c:handle_call/3`) to handle the request.

See also `ServerRef`'s type `t:server_ref/0`.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).

`Timeout` is an integer that specifies how many milliseconds to wait
for a reply, or the atom `infinity` to wait indefinitely.  If no reply
is received within the specified time, this function exits the calling
process with an exit term containing `Reason = timeout` as described below.

> #### Note {: .info }
>
> Before OTP 24, if the caller uses (`try`...)`catch`
> to avoid process exit, and the server happens to just be late
> with the reply, it may arrive to the process message queue
> any time later. The calling process must therefore after
> catching a time-out exit be prepared to receive garbage message(s)
> of the form `{reference(), _}` and deal with them appropriately
> (discard them) so they do not clog the process message queue,
> or gets mistaken for other messages.
>
> Starting with OTP 24, `gen_server:call` uses process aliases,
> so late replies will not be received.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

This call may exit the calling process with an exit term on the form
`{Reason, Location}` where `Location = {gen_server, call, ArgList}`
and `Reason` can be (at least) one of:

- **`timeout`** - The call was aborted after waiting `Timeout` milliseconds
  for a reply, as described above.

- **`noproc`** - The `ServerRef` refers to a server by name (it is not a
  `t:pid/0`) and looking up the server process failed, or the `t:pid/0`
  was already terminated.

- **`{nodedown,Node}`** - The `ServerRef` refers to a server
  on the remote node `Node` and the connection to that node failed.

- **`calling_self`** - A call to `self/0` would hang indefinitely.

- **`shutdown`** - The server was stopped during the call
  by its supervisor.  See also `stop/3`.

- **`normal`\
  `{shutdown,Term}`** - The server stopped during the call
  by returning `{stop,Reason,_}` from one of its callbacks
  without replying to this call. See also `stop/3`.

- **`_OtherTerm`** - The server process exited during the call,
  with reason `Reason`. Either by returning `{stop,Reason,_}`
  from one of its callbacks (without replying to this call),
  by raising an exception, or due to getting an exit signal
  it did not trap.

# `cast`

```elixir
-spec cast(ServerRef :: server_ref(), Request :: term()) -> ok.
```

Cast a request to a server.

Sends an asynchronous request to the `gen_server`
[`ServerRef`](`t:server_ref/0`) and returns `ok` immediately,
ignoring if the destination node or `gen_server`
process does not exist.

The `gen_server` process calls
[`Module:handle_cast(Request, _)`](`c:handle_cast/2`)
to handle the request.

# `check_response`
*since OTP 23.0* 

```elixir
-spec check_response(Msg, ReqId) -> Result
                        when
                            Msg :: term(),
                            ReqId :: request_id(),
                            Response ::
                                {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                            Result :: Response | no_reply.
```

Check if a received message is a request response.

Checks if `Msg` is a response corresponding to
the request identifier `ReqId`.  The request must have been made
by `send_request/2`, and by the same process calling this function.

If `Msg` is a reply to the handle `ReqId` the result of the request
is returned in `Reply`.  Otherwise this function returns `no_reply`
and no cleanup is done, and thus the function shall be invoked repeatedly
until the response is returned.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

If the `gen_statem` server process has died when this function
is called, that is; `Msg` reports the server's death,
this function returns an `error` return with the exit `Reason`.

# `check_response`
*since OTP 25.0* 

```elixir
-spec check_response(Msg, ReqIdCollection, Delete) -> Result
                        when
                            Msg :: term(),
                            ReqIdCollection :: request_id_collection(),
                            Delete :: boolean(),
                            Response ::
                                {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                            Result ::
                                {Response,
                                 Label :: term(),
                                 NewReqIdCollection :: request_id_collection()} |
                                no_request | no_reply.
```

Check if a received message is a request response in a collection.

Check if `Msg` is a response corresponding to a request identifier
stored in `ReqIdCollection`.  All request identifiers of `ReqIdCollection`
must correspond to requests that have been made using `send_request/2`
or `send_request/4`, by the process calling this function.

The `Label` in the response equals the `Label` associated
with the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [storing the request id](`reqids_add/3`) in a collection,
or when sending the request using `send_request/4`.

Compared to `check_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `check_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If `Msg` does not correspond to any of the request identifiers
in `ReqIdCollection`, `no_reply` is returned.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`check_response/3`, `receive_response/3`, and `wait_response/3`.

However, without deleting handled associations,
the above calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests to
this function, it will always return `no_reply`.

# `enter_loop`

```elixir
-spec enter_loop(Module :: module(), Options :: [enter_loop_opt()], State :: term()) -> no_return().
```

# `enter_loop`

```elixir
-spec enter_loop(Module :: module(),
                 Options :: [enter_loop_opt()],
                 State :: term(),
                 ServerName :: server_name() | pid()) ->
                    no_return();
                (Module :: module(), Options :: [enter_loop_opt()], State :: term(), Action :: action()) ->
                    no_return().
```

Make the calling process become a `gen_server` process.

With argument `ServerName` equivalent to
[`enter_loop(Module, Options,
  State, ServerName, infinity)`](`enter_loop/5`).

With argument `Action` equivalent to
[`enter_loop(Module, Options, State, self(), Action)`](`enter_loop/5`).

# `enter_loop`

```elixir
-spec enter_loop(Module :: module(),
                 Options :: [enter_loop_opt()],
                 State :: term(),
                 ServerName :: server_name() | pid(),
                 Action :: action()) ->
                    no_return().
```

Make the calling process become a `gen_server` process.

Does not return, instead the calling process enters the `gen_server`
process receive loop and becomes a `gen_server` process.
The process _must_ have been started using one of the start functions
in `m:proc_lib`.  The user is responsible for any initialization
of the process, including registering a name for it.

This function is useful when a more complex initialization procedure
is needed than the `gen_server` [`Module:init/1`](`c:init/1`);
callback provides.

`Module`, `Options`, and `ServerName` have the same meanings
as when calling [`start[_link|_monitor]/3,4`](`start_link/3`)
or `ServerName` can be `self/0` for an anonymous server,
which is the same as calling an `enter_loop/3,4` function
without a `ServerName` argument.  However, if `ServerName`
is specified (and not as `self/0`), the process must have been registered
accordingly _before_ this function is called.

`State` has the same meanings as in the return value of
[`Module:init/1`](`c:init/1`), which is _not_ called when
[`enter_loop/3,4,5`](`enter_loop/3`) is used.  Note that
to adhere to the [gen_server Behaviour](`e:system:gen_server_concepts.md`)
such a callback function needs to be defined, and it might as well
be the one used when starting the `gen_server` process
through `proc_lib`, and then be the one that calls `enter_loop/3,4,5`.
But if such a [`Module:init/1`](`c:init/1`) function,
in for example error cases, cannot call `enter_loop/3,4,5`,
it should return a value that follows the type specification
for [`Module:init/1`](`c:init/1`) such as `ignore`,
although that value will be lost when returning to the spawning function.

`Action` is described by the `t:action/0` type.

This function fails if the calling process was not started
by a `proc_lib` start function, or if it is not registered
according to `ServerName`.

# `multi_call`

```elixir
-spec multi_call(Name :: atom(), Request :: term()) ->
                    {Replies :: [{Node :: node(), Reply :: term()}], BadNodes :: [node()]}.
```

Call servers on multiple nodes in parallel.

Equivalent to [`multi_call(Nodes, Name, Request)`](`multi_call/3`)
where `Nodes` is all nodes connected to the calling node,
including the calling node itself.

# `multi_call`

```elixir
-spec multi_call(Nodes :: [node()], Name :: atom(), Request :: term()) ->
                    {Replies :: [{Node :: node(), Reply :: term()}], BadNodes :: [node()]}.
```

# `multi_call`

```elixir
-spec multi_call(Nodes :: [node()], Name :: atom(), Request :: term(), Timeout :: timeout()) ->
                    {Replies :: [{Node :: node(), Reply :: term()}], BadNodes :: [node()]}.
```

Call servers on multiple nodes in parallel.

Makes a synchronous call to all `gen_server` processes
locally registered as `Name` at the specified nodes,
by first sending the request to the nodes, and then waiting
for the replies. The `gen_server` processes on the nodes call
[`Module:handle_call/3`](`c:handle_call/3`) to handle the request.

The function returns a tuple `{Replies, BadNodes}`,
where `Replies` is a list of `{Node, Reply}` tuples,
and `BadNodes` is a list of nodes that either did not exist,
where `Name` was not a registered `gen_server`,
or where it did not reply.

`Nodes` is a list of node names to which the request is to be sent.

`Name` is the locally registered name for each `gen_server` process.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).

`Timeout` is an integer that specifies how many milliseconds
to wait for all replies, or the atom `infinity` to wait indefinitely.
If no reply is received from a node within the specified time,
the node is added to `BadNodes`.

When a reply `Reply` is received from the `gen_server` process
at a node `Node`, `{Node,Reply}` is added to `Replies`.
`Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

> #### Warning {: .warning }
>
> If one of the nodes cannot process monitors, for example,
> C or Java nodes, and the `gen_server` process is not started
> when the requests are sent, but starts within 2 seconds,
> this function waits the whole `Timeout`, which may be infinity.
>
> This problem does not exist if all nodes are Erlang nodes.

To prevent late answers (after the time-out)
from polluting the message queue of the caller,
a middleman process is used to do the calls.
Late answers are then discarded when they arrive to
the terminated middleman process.

# `receive_response`
*since OTP 24.0* 

```elixir
-spec receive_response(ReqId, Timeout) -> Result
                          when
                              ReqId :: request_id(),
                              Timeout :: response_timeout(),
                              Response ::
                                  {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                              Result :: Response | timeout.
```

Receive a request response.

Receive a response corresponding to the request identifier `ReqId`.
The request must have been made by `send_request/2`,
and it must have been made by the same process calling this function.

`Timeout` specifies how long to wait for a response.
If no response is received within the specified time,
this function returns `timeout`.  Assuming that the
server executes on a node supporting aliases (introduced in OTP 24)
the request will also be abandoned.  That is,
no response will be received after a time-out.
Otherwise, a stray response might be received at a later time.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

The function returns an error if the `gen_server` died
before a reply was sent.

The difference between `receive_response/2` and `wait_response/2`
is that `receive_response/2` abandons the request at time-out
so that a potential future response is ignored,
while `wait_response/2` does not.

# `receive_response`
*since OTP 25.0* 

```elixir
-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result
                          when
                              ReqIdCollection :: request_id_collection(),
                              Timeout :: response_timeout(),
                              Delete :: boolean(),
                              Response ::
                                  {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                              Result ::
                                  {Response,
                                   Label :: term(),
                                   NewReqIdCollection :: request_id_collection()} |
                                  no_request | timeout.
```

Receive a request response in a collection.

Receive a response in `ReqIdCollection`. All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/2` or `send_request/4`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/4`.

Compared to `receive_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `receive_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

`Timeout` specifies how long to wait for a response.  If no response
is received within the specified time, the function returns `timeout`.
Assuming that the server executes on a node supporting aliases
(introduced in OTP 24) all requests identified by `ReqIdCollection`
will also be abandoned.  That is, no responses will be received
after a time-out.  Otherwise, stray responses might be received
at a later time.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons the requests at time-out
so that potential future responses are ignored,
while [`wait_response/3`](`wait_response/3`) does not.

If `Delete` is `true`, the association with `Label`
is deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`. If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`receive_response/3`, `check_response/3`, and `wait_response/3`.

However, without deleting handled associations,
the above calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests to
this function, it will always block until `Timeout` expires
and then return `timeout`.

# `reply`

```elixir
-spec reply(Client :: from(), Reply :: term()) -> ok.
```

Send a reply to a client.

This function can be used by a `gen_server` process to explicitly send
a reply to a client that called [`call/2,3`](`call/2`) or
[`multi_call/2,3,4`](`multi_call/2`), when the reply cannot be passed
in the return value of [`Module:handle_call/3`](`c:handle_call/3`).

`Client` must be the `From` argument provided to the `c:handle_call/3`
callback function. `Reply` is any term passed back to the client
as the return value of `call/2,3` or `multi_call/2,3,4`.

# `reqids_add`
*since OTP 25.0* 

```elixir
-spec reqids_add(ReqId :: request_id(), Label :: term(), ReqIdCollection :: request_id_collection()) ->
                    NewReqIdCollection :: request_id_collection().
```

Store a request identifier in a colletion.

Stores `ReqId` and associates a `Label` with the request identifier
by adding this information to `ReqIdCollection` and returning
the resulting request identifier collection.

# `reqids_new`
*since OTP 25.0* 

```elixir
-spec reqids_new() -> NewReqIdCollection :: request_id_collection().
```

Create an empty request identifier collection.

Returns a new empty request identifier collection.
A request identifier collection can be utilized to handle
multiple outstanding requests.

Request identifiers of requests made by `send_request/2`
can be stored in a collection using `reqids_add/3`.
Such a collection of request identifiers can later be used
in order to get one response corresponding to a request
in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or, `check_response/3`.

`reqids_size/1` can be used to determine the number of
request identifiers in a collection.

# `reqids_size`
*since OTP 25.0* 

```elixir
-spec reqids_size(ReqIdCollection :: request_id_collection()) -> non_neg_integer().
```

Returns the number of request identifiers in `ReqIdCollection`.

# `reqids_to_list`
*since OTP 25.0* 

```elixir
-spec reqids_to_list(ReqIdCollection :: request_id_collection()) ->
                        [{ReqId :: request_id(), Label :: term()}].
```

Convert a request identifier collection to a list.

Returns a list of `{ReqId, Label}` tuples which corresponds to
all request identifiers with their associated labels
in [`ReqIdCollection`](`t:request_id_collection/0`).

# `send_request`
*since OTP 23.0* 

```elixir
-spec send_request(ServerRef :: server_ref(), Request :: term()) -> ReqId :: request_id().
```

Send an asynchronous `call` request.

Sends `Request` to the `gen_server` process identified by `ServerRef`
and returns a request identifier `ReqId`.

The return value `ReqId` shall later be used with `receive_response/2`,
`wait_response/2`, or `check_response/2` to fetch the actual result
of the request.  Besides passing the request identifier directly
to these functions, it can also be stored in
a request identifier collection using `reqids_add/3`.
Such a collection of request identifiers can later be used
in order to get one response corresponding to a
request in the collection by passing the collection
as argument to `receive_response/3`, `wait_response/3`,
or `check_response/3`.  If you are about to store the request identifier
in a collection, you may want to consider using `send_request/4` instead.

The call
`gen_server:receive_response(gen_server:send_request(ServerRef, Request), Timeout)`
can be seen as equivalent to
[`gen_server:call(ServerRef, Request, Timeout)`](`call/3`),
ignoring the error handling.

The `gen_server` process calls [`Module:handle_call/3`](`c:handle_call/3`) to
handle the request.

See the type `t:server_ref/0` for the possible values for `ServerRef`.

`Request` is any term that is passed as the first argument to
[`Module:handle_call/3`](`c:handle_call/3`).

# `send_request`
*since OTP 25.0* 

```elixir
-spec send_request(ServerRef :: server_ref(),
                   Request :: term(),
                   Label :: term(),
                   ReqIdCollection :: request_id_collection()) ->
                      NewReqIdCollection :: request_id_collection().
```

Send an asynchronous `call` request and add it
to a request identifier collection.

Sends `Request` to the `gen_server` process identified by `ServerRef`.
The `Label` will be associated with the request identifier
of the operation and added to the returned request identifier collection
`NewReqIdCollection`.  The collection can later be used in order to
get one response corresponding to a request in the collection
by passing the collection as argument to `receive_response/3`,
`wait_response/3`, or `check_response/3`.

The same as calling
[`reqids_add`](`reqids_add/3`)`(`[`send_request`](`send_request/2`)`(ServerRef, Request), Label, ReqIdCollection)`,
but slightly more efficient.

# `start`

```elixir
-spec start(Module :: module(), Args :: term(), Options :: [start_opt()]) -> start_ret().
```

Start a server, neither linked nor registered.

Equivalent to `start/4` except that the `gen_server` process is not
registered with any [name service](`t:server_name/0`).

# `start`

```elixir
-spec start(ServerName :: server_name(), Module :: module(), Args :: term(), Options :: [start_opt()]) ->
               start_ret().
```

Start a server, registered but not linked.

Creates a standalone `gen_server` process, that is,
a `gen_server` process that is not part of a supervision tree,
and thus has no supervisor.

Other than that see `start_link/4`.

# `start_link`

```elixir
-spec start_link(Module :: module(), Args :: term(), Options :: [start_opt()]) -> start_ret().
```

Start a server, linked but not registered.

Equivalent to `start_link/4` except that the `gen_server` process is
not registered with any [name service](`t:server_name/0`).

# `start_link`

```elixir
-spec start_link(ServerName :: server_name(),
                 Module :: module(),
                 Args :: term(),
                 Options :: [start_opt()]) ->
                    start_ret().
```

Start a server, linked and registered.

Creates a `gen_server` process as part of a supervision tree.
This function is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the `gen_server` process is spawned
as linked to the caller (supervisor).

The `gen_server` process calls [`Module:init/1`](`c:init/1`)
to initialize.  To ensure a synchronized startup procedure,
`start_link/3,4` does not return until [`Module:init/1`](`c:init/1`)
has returned or failed.

[`ServerName`](`t:server_name/0`) specifies with what name
and now to register the server name.  See type `t:server_name/0`
for different name registrations.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

See type `t:start_opt/0` for `Options` for starting
the `gen_server` process.

See type `t:start_ret/0` for a description this function's return values.

If `start_link/3,4` returns `ignore` or `{error, _}`,
the started `gen_server` process has terminated.  If an `'EXIT'` message
was delivered to the calling process (due to the process link),
that message has been consumed.

> #### Warning {: .warning }
>
> Before OTP 26.0, if the started `gen_server` process returned e.g.
> `{stop, Reason}` from [`Module:init/1`](`c:init/1`), this function
> could return `{error, Reason}` _before_ the started `m:gen_server` process
> had terminated so starting again might fail because VM resources
> such as the registered name was not yet unregistered. An `'EXIT'` message
> could arrive later to the process calling this function.
>
> But if the started `gen_server` process instead failed during
> [`Module:init/1`](`c:init/1`), a process link `{'EXIT', Pid, Reason}`
> message caused this function to return `{error, Reason}`,
> so the `'EXIT'` message had been consumed and the started
> `m:gen_server` process had terminated.
>
> Since it was impossible to tell the difference between these two cases
> from `start_link/3,4`'s return value, this inconsistency was cleaned up
> in OTP 26.0.

The difference between returning `{stop, _}` and `{error, _}` from
[`Module:init/1`](`c:init/1`), is that `{error, _}` results in a graceful
("silent") termination since the `gen_server` process exits
with reason `normal`.

# `start_monitor`
*since OTP 23.0* 

```elixir
-spec start_monitor(Module :: module(), Args :: term(), Options :: [start_opt()]) -> start_mon_ret().
```

Start a server, monitored but neither linked nor registered.

Equivalent to `start_monitor/4` except that the `gen_server` process
is not registered with any [name service](`t:server_name/0`).

# `start_monitor`
*since OTP 23.0* 

```elixir
-spec start_monitor(ServerName :: server_name(),
                    Module :: module(),
                    Args :: term(),
                    Options :: [start_opt()]) ->
                       start_mon_ret().
```

Start a server, monitored and registered, but not linked.

Creates a standalone `gen_server` process, that is,
a `gen_server` process that is not part of a supervision tree
(and thus has no supervisor) and atomically sets up a monitor
to the newly created server.

Other than that see [`start_link/3,4`](`start_link/3`).
Note that the return value for a successful start differs in that
it returns a monitor `reference`.  See type `t:start_mon_ret/0`.

If the start is not successful, the caller will be blocked
until the monitor's `'DOWN'` message has been received
and removed from the message queue.

# `stop`
*since OTP 18.0* 

```elixir
-spec stop(ServerRef :: server_ref()) -> ok.
```

# `stop`
*since OTP 18.0* 

```elixir
-spec stop(ServerRef :: server_ref(), Reason :: term(), Timeout :: timeout()) -> ok.
```

Stop a server.

Orders the generic server specified by `ServerRef` to exit
with the specified `Reason` and waits for it to terminate.
The `gen_server` process calls [`Module:terminate/2`](`c:terminate/2`)
before exiting.

The function returns `ok` if the server terminates
with the expected reason. Any other reason than `normal`, `shutdown`,
or `{shutdown,Term}` causes an error report to be issued using `m:logger`.
An exit signal with the same reason is sent to linked processes and ports.

`Timeout` is an integer that specifies how many milliseconds to wait
for the server to terminate, or the atom `infinity` to wait indefinitely.
If the server has not terminated within the specified time,
the call exits the calling process with reason `timeout`.

If the process does not exist, the call exits the calling process
with reason `noproc`, or with reason `{nodedown,Node}`
if the connection fails to the remote `Node` where the server runs.

# `wait_response`
*since OTP 23.0* 

```elixir
-spec wait_response(ReqId, WaitTime) -> Result
                       when
                           ReqId :: request_id(),
                           WaitTime :: response_timeout(),
                           Response ::
                               {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                           Result :: Response | timeout.
```

Wait for a request response.

Wait for the response to the request identifier `ReqId`. The request
must have been made by `send_request/2`, and it must have been made
by the same process calling this function.

`WaitTime` specifies how long to wait for a reply.
If no reply is received within the specified time,
the function returns `timeout` and no cleanup is done.
Thus the function can be invoked repeatedly until a reply is returned.

The return value `Reply` is passed from the return value of
[`Module:handle_call/3`](`c:handle_call/3`).

The function returns an error if the `gen_server`
died before a reply was sent.

The difference between `receive_response/2` and
`wait_response/2` is that `receive_response/2` abandons
the request at time-out so that a potential future response is ignored,
while [`wait_response/2`](`wait_response/2`) does not.

# `wait_response`
*since OTP 25.0* 

```elixir
-spec wait_response(ReqIdCollection, WaitTime, Delete) -> Result
                       when
                           ReqIdCollection :: request_id_collection(),
                           WaitTime :: response_timeout(),
                           Delete :: boolean(),
                           Response ::
                               {reply, Reply :: term()} | {error, {Reason :: term(), server_ref()}},
                           Result ::
                               {Response,
                                Label :: term(),
                                NewReqIdCollection :: request_id_collection()} |
                               no_request | timeout.
```

Wait for any request response in a collection.

Wait for a response in a `ReqIdCollection`.  All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/2` or `send_request/4`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/4`.

Compared to `wait_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `wait_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If no response is received before `WaitTime` has expired,
`timeout` is returned.  It is valid to continue waiting
for a response as many times as needed up until a response
has been received and completed by `check_response()`,
`receive_response()`, or `wait_response()`.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons requests at time-out
so that potential future responses are ignored, while
`wait_response/3` does not.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
will equal `ReqIdCollection`.  Note that deleting an association
is not for free and that a collection containing already handled
requests can still be used by subsequent calls to
`wait_response/3`, `check_response/3`, and `receive_response/3`.

However, without deleting handled associations, the above
calls will not be able to detect when there are
no more outstanding requests to handle, so you will have to keep track
of this some other way than relying on a `no_request` return.
Note that if you pass a collection only containing
associations of already handled or abandoned requests
to this function, it will always block until `WaitTime` expires
and then return `timeout`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
