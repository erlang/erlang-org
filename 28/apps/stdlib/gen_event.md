# `gen_event`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/gen_event.erl#L22)

Generic event handling behavior.

This behavior module provides event handling functionality.
It consists of a generic event manager process with any number of
event handlers that are added and deleted dynamically.

An event manager implemented using this module has a standard set of
interface functions and includes functionality for tracing
and error reporting.  It also fits into an OTP supervision tree.
For more information, see [gen_event section in OTP Design Principles](`e:system:events.md`).

Each event handler is implemented as a callback module
exporting a predefined set of functions. The relationship between
the behavior functions and the callback functions is as follows:

```text
gen_event module                   Callback module
----------------                   ---------------
gen_event:start
gen_event:start_monitor
gen_event:start_link       ----->  -

gen_event:add_handler
gen_event:add_sup_handler  ----->  Module:init/1

gen_event:notify
gen_event:sync_notify      ----->  Module:handle_event/2

gen_event:send_request
gen_event:call             ----->  Module:handle_call/2

-                          ----->  Module:handle_info/2

gen_event:delete_handler   ----->  Module:terminate/2

gen_event:swap_handler
gen_event:swap_sup_handler ----->  Module1:terminate/2
                                   Module2:init/1

gen_event:which_handlers   ----->  -

gen_event:stop             ----->  Module:terminate/2

-                          ----->  Module:code_change/3
```

As each event handler is one callback module, an event manager
has many callback modules that are added and deleted dynamically.
`gen_event` is therefore more tolerant of callback module errors
than the other behaviors.  If a callback function for an installed
event handler fails with `Reason`, or returns a bad value `Term`,
the event manager does not fail.  It deletes the event handler
by calling callback function [`Module:terminate/2`](`c:terminate/2`),
giving as argument `{error, {'EXIT', Reason}}` or `{error, Term}`,
respectively.  No other event handler is affected.

A `gen_event` process handles system messages as described in `m:sys`.
The `sys` module can be used for debugging an event manager.

Notice that an event manager _does_ trap exit signals automatically.

The `gen_event` process can go into hibernation
(see `erlang:hibernate/3`) if a callback function in a handler module
specifies `hibernate` in its return value.  This can be useful
if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies
at least two garbage collections (when hibernating
and shortly after waking up) and is not something you want to do
between each event handled by a busy event manager.

Notice that when multiple event handlers are invoked,
it is sufficient that one single event handler returns a `hibernate`
request for the whole event manager to go into hibernation.

Unless otherwise stated, all functions in this module fail
if the specified event manager does not exist
or if bad arguments are specified.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_
> ](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
> Blocking signaling can, for example, cause call timeouts in `gen_event`
> to be significantly delayed.

## See Also

`m:supervisor`, `m:sys`

# `add_handler_ret`

```elixir
-type add_handler_ret() :: ok | term() | {'EXIT', term()}.
```

# `debug_flag`
*not exported* 

```elixir
-type debug_flag() :: trace | log | statistics | debug | {logfile, string()}.
```

# `del_handler_ret`

```elixir
-type del_handler_ret() :: ok | term() | {'EXIT', term()}.
```

# `emgr_name`
*not exported* 

```elixir
-type emgr_name() :: {local, atom()} | {global, term()} | {via, atom(), term()}.
```

Event manager name specification: `local`, `global`, or `via` registered.

- *`{local, Name}`* - the event manager is registered locally as
  `Name` using [`register/2`](`register/2`).
- *`{global, GlobalName}`* - The event manager is registered
  globally as `GlobalName` using `global:register_name/2`.
  If no name is provided, the event manager is not registered.
- *`{via, Module, ViaName}`*, the event manager registers with the
  registry represented by `Module`. The `Module` callback is to export
  the functions `register_name/2`, `unregister_name/1`, `whereis_name/1`,
  and `send/2`, which are to behave as the corresponding functions
  in `m:global`.  Thus, `{via, global, GlobalName}` is a valid reference.

# `emgr_ref`
*not exported* 

```elixir
-type emgr_ref() :: atom() | {atom(), node()} | {global, term()} | {via, atom(), term()} | pid().
```

A reference used to locate an event manager.

The reference can be any of the following:

- The pid of the event manager
- `Name`, if the event manager is locally registered
- `{Name, Node}`, if the event manager is locally registered
  at another node
- `{global, GlobalName}`, if the event manager is globally registered
- `{via, Module, ViaName}`, if the event manager is registered through
  an alternative process registry

# `format_status`

```elixir
-type format_status() ::
          #{state => term(), message => term(), reason => term(), log => [sys:system_event()]}.
```

A map that describes the `gen_event` process status.

The keys are:
- **`state`** - The internal state of the event handler.
- **`message`** - The message that caused the event handler to terminate.
- **`reason`** - The reason that caused the event handler to terminate.
- **`log`** - The [sys log](`sys:log/2`) of the server.

New associations may be added into the status map without prior notice.

# `handler`

```elixir
-type handler() :: atom() | {atom(), term()}.
```

# `handler_args`

```elixir
-type handler_args() :: term().
```

# `options`
*not exported* 

```elixir
-type options() ::
          [{timeout, timeout()} |
           {debug, [debug_flag()]} |
           {spawn_opt, [proc_lib:start_spawn_option()]} |
           {hibernate_after, timeout()}].
```

Options that can be used to configure an event handler
when it is started.

# `request_id`

```elixir
-opaque request_id() :: gen:request_id().
```

An opaque request identifier. See `send_request/3` for details.

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

- **`0..4294967295`** - Timeout relative to current time in milliseconds.

- **`infinity`** - Infinite timeout. That is, the operation
  will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`) timeout
  in milliseconds.  That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`)
  returns a value larger than or equal to `Timeout`.
 `Timeout` is not allowed to identify a time further into the future
  than `4294967295` milliseconds. Identifying the timeout using
  an absolute timeout value is especially handy when you have a
  deadline for responses corresponding to a complete collection
  of requests (`t:request_id_collection/0`) , since you do not have to
  recalculate the relative time until the deadline over and over again.

# `start_mon_ret`
*not exported* 

```elixir
-type start_mon_ret() :: {ok, {pid(), reference()}} | {error, term()}.
```

# `start_ret`
*not exported* 

```elixir
-type start_ret() :: {ok, pid()} | {error, term()}.
```

# `code_change`
*optional* 

```elixir
-callback code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) ->
                         {ok, NewState :: term()}.
```

Update the event handler state after code change.

This function is called for an installed event handler
that is to update its internal state during a release upgrade/downgrade,
that is, when the instruction `{update, Module, Change,...}`,
is specified in the [`appup`](`e:sasl:appup.md`) file.

For more information, see [OTP Design Principles](`e:system:index.html`).

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade,
`OldVsn` is `{down, Vsn}`.  `Vsn` is defined by the `vsn` attribute(s)
of the old version of the callback module `Module`.  If no such attribute
is defined, the version is the checksum of the Beam file.

`State` is the internal state of the event handler.

`Extra` is passed "as is" from the `{advanced, Extra}` part
of the update instruction.

The function is to return the updated internal state.

> #### Note {: .info }
>
> If a release upgrade/downgrade with `Change={advanced, Extra}`
> specified in the [`.appup`](`e:sasl:appup.md`) file is made
> when `c:code_change/3` is not implemented the event handler will crash
> with an `undef` error reason.

# `format_status`
*since OTP 25.0* *optional* 

```elixir
-callback format_status(Status) -> NewStatus when Status :: format_status(), NewStatus :: format_status().
```

Format/limit the status value.

This function is called by a `gen_event` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_event` status.

- The event handler terminates abnormally and `gen_event` logs an error.

This callback is used to limit the status of the event handler returned by
[`sys:get_status/1,2`](`sys:get_status/1`) or sent to `m:logger`.

The callback gets a map `Status` describing the current status
and shall return a map `NewStatus` with the same keys,
but it may transform some values.

Two possible use cases for this callback is to remove
sensitive information from the state to prevent it from being printed
in log files, or to compact large irrelevant status items
that would only clutter the logs.

_Example_:

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
> This callback is optional, so event handler modules need not export it.
> If a handler does not export this function, the `gen_event` module
> uses the handler state directly for the purposes described below.
>
> If this callback is exported but fails, to hide possibly sensitive data,
> the default function will instead return the fact that
> [`format_status/1`](`c:format_status/1`) has crashed.

# `format_status`
*since OTP R14B* *optional* 

> This callback is deprecated. the callback gen_event:format_status(_,_) is deprecated; use format_status/1 instead.

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

This function is called by a `gen_event` process in in order to
format/limit the server state for debugging and logging purposes.

It is called in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked
  to get the `gen_event` status. `Opt` is set to the atom `normal`
  for this case.

- The event handler terminates abnormally and `gen_event` logs an error.
  `Opt` is set to the atom `terminate` for this case.

This function is useful for changing the form and appearance of the event
handler state for these cases. An event handler callback module
wishing to change the `sys:get_status/1,2` return value as well as
how its state appears in termination error logs, exports an instance of
[`format_status/2`](`c:format_status/2`) that returns a term
describing the current state of the event handler.

`PDict` is the current value of the process dictionary of `gen_event`.

`State` is the internal state of the event handler.

The function is to return `Status`, a term that change the details of
the current state of the event handler. Any term is allowed for `Status`.
The `gen_event` module uses `Status` as follows:

- When `sys:get_status/1,2` is called, `gen_event` ensures that
  its return value contains `Status` in place of the state term
  of the event handler.

- When an event handler terminates abnormally, `gen_event` logs `Status`
  in place of the state term of the event handler.

One use for this function is to return compact alternative
state representations to avoid that large state terms
are printed in log files.

> #### Note {: .info }
>
> This callback is optional, so event handler modules need not export it.
> If a handler does not export this function, the `gen_event` module
> uses the handler state directly for the purposes described below.

# `handle_call`

```elixir
-callback handle_call(Request :: term(), State :: term()) ->
                         {ok, Reply :: term(), NewState :: term()} |
                         {ok, Reply :: term(), NewState :: term(), hibernate} |
                         {swap_handler,
                          Reply :: term(),
                          Args1 :: term(),
                          NewState :: term(),
                          Handler2 :: atom() | {atom(), Id :: term()},
                          Args2 :: term()} |
                         {remove_handler, Reply :: term()}.
```

Handle a call.

Whenever an event manager receives a request sent using
[`call/3,4`](`call/3`), this function is called
for the specified event handler to handle the request.

`Request` is the `Request` argument of `call/3,4`.

`State` is the internal state of the event handler.

The return values are the same as for
[`Module:handle_event/2`](`c:handle_event/2`) except that
they also contain a term `Reply`, which is the reply to the client
as the return value of `call/3,4`.

# `handle_event`

```elixir
-callback handle_event(Event :: term(), State :: term()) ->
                          {ok, NewState :: term()} |
                          {ok, NewState :: term(), hibernate} |
                          {swap_handler,
                           Args1 :: term(),
                           NewState :: term(),
                           Handler2 :: atom() | {atom(), Id :: term()},
                           Args2 :: term()} |
                          remove_handler.
```

Handle an event.

Whenever an event manager receives an event sent using `notify/2` or
`sync_notify/2`, this function is called for each installed event handler
to handle the event.

`Event` is the `Event` argument of `notify/2` / `sync_notify/2`.

`State` is the internal state of the event handler.

- If `{ok, NewState}` or `{ok, NewState, hibernate}` is returned,
  the event handler remains in the event manager with the possibly
  updated internal state `NewState`.

- If `{ok, NewState, hibernate}` is returned, the event manager
  also goes into hibernation (by calling `proc_lib:hibernate/3`),
  waiting for the next event to occur.  It is sufficient
  that one of the event handlers return `{ok, NewState, hibernate}`
  for the whole event manager process to hibernate.

- If `{swap_handler, Args1, NewState, Handler2, Args2}` is returned,
  the event handler is replaced by `Handler2` by first calling
  [`Module:terminate(Args1, NewState)`](`c:terminate/2`) and then
  [`Module2:init({Args2, Term})`](`c:init/1`), where `Term`
  is the return value of [`Module:terminate/2`](`c:terminate/2`).
  For more information, see `swap_handler/3`.

- If `remove_handler` is returned, the event handler is deleted by calling
  [`Module:terminate(remove_handler, State)`](`c:terminate/2`).

# `handle_info`
*optional* 

```elixir
-callback handle_info(Info :: term(), State :: term()) ->
                         {ok, NewState :: term()} |
                         {ok, NewState :: term(), hibernate} |
                         {swap_handler,
                          Args1 :: term(),
                          NewState :: term(),
                          Handler2 :: atom() | {atom(), Id :: term()},
                          Args2 :: term()} |
                         remove_handler.
```

Handle an info message (regular process message).

This function is called for each installed event handler when
an event manager receives any other message than an event
or a synchronous request (or a system message).

`Info` is the received message.

In particular, this callback will be made when a process terminated
after calling `add_sup_handler/3`. Any event handler attached to
an event manager which in turn has a supervised handler
should expect callbacks of the shape
[`Module:handle_info({'EXIT', Pid, Reason}, State)`](`c:handle_info/2`).

For a description of `State` and possible return values,
see [`Module:handle_event/2`](`c:handle_event/2`).

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_event` module provides a default implementation
> of this function that logs about the unexpected `Info` message,
> drops it and returns `{ok, State}`.

# `init`

```elixir
-callback init(InitArgs :: term()) ->
                  {ok, State :: term()} | {ok, State :: term(), hibernate} | {error, Reason :: term()}.
```

Initialize the event handler.

Whenever a new event handler is added to an event manager,
this function is called to initialize the event handler.

If the event handler is added because of a call to `add_handler/3` or
`add_sup_handler/3`, `InitArgs` is the `Args` argument of these functions.

If the event handler replaces another event handler because of
a call to `swap_handler/3` or `swap_sup_handler/3`, or because of
a `swap` return tuple from one of the other callback functions,
`InitArgs` is a tuple `{Args, Term}`, where `Args` is the argument
provided in the function call/return tuple and `Term` is the result
of terminating the old event handler, see `swap_handler/3`.

If successful, the function returns `{ok, State}` or
`{ok, State, hibernate}`, where `State` is the initial internal state
of the event handler.

If `{ok, State, hibernate}` is returned, the event manager
goes into hibernation (by calling `proc_lib:hibernate/3`),
waiting for the next event to occur.

# `terminate`
*optional* 

```elixir
-callback terminate(Args ::
                        term() |
                        {stop, Reason :: term()} |
                        stop | remove_handler |
                        {error, {'EXIT', Reason :: term()}} |
                        {error, term()},
                    State :: term()) ->
                       term().
```

Handle event handler termination.

Whenever an event handler is deleted from an event manager,
this function is called. It is to be the opposite
of [`Module:init/1`](`c:init/1`) and do any necessary cleaning up.

If the event handler is deleted because of a call to `delete_handler/3`,
`swap_handler/3`, or `swap_sup_handler/3`, `Arg` is
the `Args` argument of this function call.

`Arg = {stop, Reason}` if the event handler has a supervised connection
to a process that has terminated with reason `Reason`.

`Arg = stop` if the event handler is deleted because
the event manager is terminating.

The event manager terminates if it is part of a supervision tree
and it is ordered by its supervisor to terminate.  Even if
it is _not_ part of a supervision tree, it terminates if it receives
an `'EXIT'` message from its parent.

`Arg = remove_handler` if the event handler is deleted
because another callback function has returned `remove_handler`
or `{remove_handler, Reply}`.

`Arg = {error, Term}` if the event handler is deleted because
a callback function returned an unexpected value `Term`,
or `Arg = {error, {'EXIT', Reason}}` if a callback function failed.

`State` is the internal state of the event handler.

The function can return any term.  If the event handler
is deleted because of a call to `gen_event:delete_handler/3`,
the return value of that function becomes the return value
of this function. If the event handler is to be replaced with
another event handler because of a swap, the return value
is passed to the `init` function of the new event handler.
Otherwise the return value is ignored.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_event` module provides a default implementation
> without cleanup.

# `add_handler`

```elixir
-spec add_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
```

Add a new event handler to an event manager.

The new event handler is added to event manager `EventMgrRef`.
The event manager calls [`Module:init/1`](`c:init/1`)
to initiate the event handler and its internal state.

`Handler` is the name of the callback module `Module`
or a tuple `{Module, Id}`, where `Id` is any term.
The `{Module, Id}` representation makes it possible to
identify a specific event handler, when many event handlers
use the same callback module.

`Args` is any term that is passed as the argument to
[`Module:init/1`](`c:init/1`).

If [`Module:init/1`](`c:init/1`) returns a correct value
indicating successful completion, the event manager
adds the event handler and this function returns `ok`.
If [`Module:init/1`](`c:init/1`) fails with `Reason` or returns
`{error,Reason}`, the event handler is ignored and this function
returns `{'EXIT',Reason}` or `{error,Reason}`, respectively.

# `add_sup_handler`

```elixir
-spec add_sup_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
```

Add a new event handler to an event manager, supervised.

The new event handler is added as for `add_handler/3`,
but the event manager also supervises the connection
by linking the event handler and the calling process.

- If the calling process later terminates with `Reason`,
  the event manager deletes any supervised event handlers by calling
  [`Module:terminate/2`](`c:terminate/2`), then calls
  [`Module:handle_info/2`](`c:handle_info/2`) for each remaining handler.

- If the event handler is deleted later, the event manager
  sends a message `{gen_event_EXIT,Handler,Reason}`
  to the calling process. `Reason` is one of the following:

  + `normal`, if the event handler has been removed because of
    a call to [`delete_handler/3`](`delete_handler/3`),
    or `remove_handler` has been returned by a callback function
    (see below).
  + `shutdown`, if the event handler has been removed
    because the event manager is terminating.
  + `{swapped, NewHandler, Pid}`, if the process `Pid` has replaced
    the event handler with another event handler `NewHandler`,
    through a call to `swap_handler/3` or `swap_sup_handler/3`.
  + Other `t:term/0`, if the event handler is removed
    because of an error.  Which term depends on the error.

For a description of the arguments and return values, see `add_handler/3`.

# `call`

```elixir
-spec call(EventMgrRef :: emgr_ref(), Handler :: handler(), Request :: term()) -> term().
```

# `call`

```elixir
-spec call(EventMgrRef :: emgr_ref(), Handler :: handler(), Request :: term(), Timeout :: timeout()) ->
              term().
```

Make a synchronous call to an event handler.

The call is sent to `Handler`, installed in event manager `EventMgrRef`,
by sending a request and waiting until a reply arrives,
or a time-out occurs.  The event manager calls
[`Module:handle_call/2`](`c:handle_call/2`) to handle the request.

`Request` is any term that is passed as one of the arguments to
[`Module:handle_call/2`](`c:handle_call/2`).

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for a reply, or the atom `infinity`
to wait indefinitely.  Defaults to 5000.  If no reply is received
within the specified time, the function call fails.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).  If the specified
event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`,
or returns an unexpected value `Term`, this function returns
`{error, {'EXIT', Reason}}` or `{error, Term}`, respectively.

When this call fails it [exits](`erlang:exit/1`) the calling process.
The exit term is of the form `{Reason, Location}` where
`Location = {gen_event, call, ArgList}`. See `gen_server:call/3`
that has a description of relevant values for the `Reason`
in the exit term.

# `check_response`
*since OTP 23.0* 

```elixir
-spec check_response(Msg, ReqId) -> Result
                        when
                            Msg :: term(),
                            ReqId :: request_id(),
                            Response ::
                                {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                            Result :: Response | no_reply.
```

Check if a received message is a request response.

Check if `Msg` is a response corresponding to
the request identifier `ReqId`.  The request must have been made
by `send_request/3`, and by the same process calling this function.

If `Msg` is a response corresponding to `ReqId` the response is returned
in `Reply`.  Otherwise this function returns `no_reply`
and no cleanup is done.  Thus this function must be invoked repeatedly
until a response is returned.

If the specified event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`
or returns an unexpected value `Term`, this function returns
`{error, {'EXIT', Reason}}` or `{error, Term}`, respectively.
If the event manager has died before this function is called,
that is; `Msg` reports the server's death, this function returns
`{error,{Reason, EventMgrRef}}` where `Reason` is the exit reason.

# `check_response`
*since OTP 25.0* 

```elixir
-spec check_response(Msg, ReqIdCollection, Delete) -> Result
                        when
                            Msg :: term(),
                            ReqIdCollection :: request_id_collection(),
                            Delete :: boolean(),
                            Response ::
                                {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                            Result ::
                                {Response,
                                 Label :: term(),
                                 NewReqIdCollection :: request_id_collection()} |
                                no_request | no_reply.
```

Check if a received message is a request response in a collection.

Check if `Msg` is a response corresponding to a request identifier
stored in `ReqIdCollection`.  All request identifiers of `ReqIdCollection`
must correspond to requests that have been made using `send_request/3`
or `send_request/5`, and all requests must have been made
by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [storing the request id](`reqids_add/3`) in a collection,
or when sending the request using `send_request/5`.

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

If `Delete` is `true`, the association with `Label` has been deleted
from `ReqIdCollection` in the resulting `NewReqIdCollection`.
If `Delete` is `false`, `NewReqIdCollection` will equal `ReqIdCollection`.
Note that deleting an association is not for free and that
a collection containing already handled requests
can still be used by subsequent calls to `check_response/3`),
`receive_response/3`, and `wait_response/3`.

However, without deleting handled associations, the above calls
will not be able to detect when there are no more outstanding requests
to handle, so you will have to keep track of this some other way
than relying on a `no_request` return.  Note that if you pass
a collection only containing associations of already handled
or abandoned requests to `check_response/3`,
it will always return `no_reply`.

# `delete_handler`

```elixir
-spec delete_handler(EventMgrRef :: emgr_ref(), Handler :: handler(), Args :: term()) -> term().
```

Deletes an event handler from an event manager.

This function deletes event handler `Handler` from event manager
`EventMgrRef`. The event manager calls
[`Module:terminate/2`](`c:terminate/2`) to terminate the event handler.

`Args` is any term that is passed as one of the arguments to
[`Module:terminate/2`](`c:terminate/2`).

The return value is the return value of
[`Module:terminate/2`](`c:terminate/2`).  If the specified
event handler is not installed, the function returns
`{error, module_not_found}`. If the callback function fails
with `Reason`, the function returns `{'EXIT', Reason}`.

# `notify`

```elixir
-spec notify(EventMgrRef :: emgr_ref(), Event :: term()) -> ok.
```

Send an asynchronous event notification to an event manager.

The event is sent to `EventMgrRef`, that calls
[`Module:handle_event/2`](`c:handle_event/2`) for each installed
event handler to handle the event.

`Event` is any term that is passed as one of the arguments to
[`Module:handle_event/2`](`c:handle_event/2`).

`notify/1` does not fail even if the specified event manager
does not exist, unless it is specified as `Name`.

# `receive_response`
*since OTP 24.0* 

```elixir
-spec receive_response(ReqId, Timeout) -> Result
                          when
                              ReqId :: request_id(),
                              Timeout :: response_timeout(),
                              Response ::
                                  {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                              Result :: Response | timeout.
```

Receive a request response.

Receive a response corresponding to the request identifier `ReqId`.
The request must have been made by `send_request/3`,
and it must have been made from the same process calling this function.

`Timeout` specifies how long to wait for a response.
If no response is received within the specified time,
this function returns `timeout`. Assuming that the
server executes on a node supporting aliases (introduced in OTP 24)
the request will also be abandoned.  That is,
no response will be received after a timeout.
Otherwise, a stray response might be received at a later time.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).

If the specified event handler is not installed, this function returns
`{error, bad_module}`. If the callback function fails
with `Reason` or returns an unexpected value `Term`,
this function returns `{error, {'EXIT', Reason}}` or`{error,Term}`,
respectively.  If the event manager dies before or during the
request this function returns `{error, {Reason, EventMgrRef}}`.

The difference between `wait_response/2` and `receive_response/2`
is that `receive_response/2` abandons the request at time-out
so that a potential future response is ignored,
while [`wait_response/2`](`wait_response/2`) does not.

# `receive_response`
*since OTP 25.0* 

```elixir
-spec receive_response(ReqIdCollection, Timeout, Delete) -> Result
                          when
                              ReqIdCollection :: request_id_collection(),
                              Timeout :: response_timeout(),
                              Delete :: boolean(),
                              Response ::
                                  {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                              Result ::
                                  {Response,
                                   Label :: term(),
                                   NewReqIdCollection :: request_id_collection()} |
                                  no_request | timeout.
```

Receive a request response in a collection.

Receive a response in `ReqIdCollection`. All request identifiers
of `ReqIdCollection` must correspond to requests that have been
made using `send_request/3` or `send_request/5`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/5`.

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
while `wait_response/3` does not.

If `Delete` is `true`, the association with `Label`
is deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`.  If `Delete` is `false`, `NewReqIdCollection`
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
`receive_response/3`, it will always block until `Timeout` expires
and then return `timeout`.

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

Request identifiers of requests made by `send_request/3`
can be saved in a request identifier collection using `reqids_add/3`.
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

Returns a list of `{ReqId, Label}` tuples which corresponds to
all request identifiers with their associated labels
in [`ReqIdCollection`](`t:request_id_collection/0`).

# `send_request`
*since OTP 23.0* 

```elixir
-spec send_request(EventMgrRef :: emgr_ref(), Handler :: handler(), Request :: term()) ->
                      ReqId :: request_id().
```

Send an asynchronous `call` request to an event handler.

This function sends the call request `Request` to the event handler
`Handler` installed in the event manager identified by `EventMgrRef`,
and returns a request identifier `ReqId`.  The return value `ReqId`
shall later be used with `receive_response/2`, `wait_response/2`,
or `check_response/2` to fetch the actual result of the request.

Besides passing the request identifier directly to these functions,
it can also be stored in a request identifier collection
using `reqids_add/3`.  Such a collection of request identifiers
can later be used in order to get one response corresponding to
a request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or `check_response/3`.
If you are about to store the request identifier in a collection,
you may want to consider using `send_request/5` instead.

The calls
`gen_event:receive_response(gen_event:send_request(EventMgrRef,
Handler, Request), Timeout)`
can be seen as equivalent to
[`gen_event:call(EventMgrRef, Handler, Request, Timeout)`](`call/3`),
ignoring the error handling.

The event manager calls [`Module:handle_call/2`](`c:handle_call/2`)
to handle the request.

`Request` may be any term and is passed as one of the arguments to
[`Module:handle_call/2`](`c:handle_call/2`).

# `send_request`
*since OTP 25.0* 

```elixir
-spec send_request(EventMgrRef :: emgr_ref(),
                   Handler :: handler(),
                   Request :: term(),
                   Label :: term(),
                   ReqIdCollection :: request_id_collection()) ->
                      NewReqIdCollection :: request_id_collection().
```

Send an asynchronous `call` request to an event handler,
storing it in a request identifier collection.

This function sends the call request `Request` to the event handler
`Handler` installed in the event manager identified by `EventMgrRef`.
The `Label` will be associated with the request identifier
of the operation and added to the returned
request identifier collection `NewReqIdCollection`.

The collection can later be used in order to get one response
corresponding to a request in the collection by passing the collection
as argument to `receive_response/3`, `wait_response/3`,
or `check_response/3`.

The same as calling
[`gen_event:reqids_add`](`reqids_add/3`)`(`[`gen_event:send_request`](`send_request/3`)`(EventMgrRef, Handler, Request), Label, ReqIdCollection)`,
but slightly more efficient.

# `start`

```elixir
-spec start() -> start_ret().
```

# `start`

```elixir
-spec start(EventMgrName :: emgr_name()) -> start_ret();
           (Options :: options()) -> start_ret().
```

Create a stand-alone event manager process, possibly nameless.

Equivalent to [`start(EventMgrName, Options)`](`start/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values, see `start_link/2`.

# `start`
*since OTP 20.0* 

```elixir
-spec start(EventMgrName :: emgr_name(), Options :: options()) -> start_ret().
```

Create a stand-alone event manager process.

The created event manager process is not part of a supervision tree
and thus has no supervisor.

For a description of the arguments and return values, see `start_link/2`.

# `start_link`

```elixir
-spec start_link() -> start_ret().
```

# `start_link`

```elixir
-spec start_link(EventMgrName :: emgr_name()) -> start_ret();
                (Options :: options()) -> start_ret().
```

Create an event manager process as part of a supervision tree,
possibly nameless.

Equivalent to [`start_link(EventMgrName, Options)`](`start_link/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values, see `start_link/2`.

# `start_link`
*since OTP 20.0* 

```elixir
-spec start_link(EventMgrName :: emgr_name(), Options :: options()) -> start_ret().
```

Create an event manager process as part of a supervision tree.

The function is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the event manager is linked
to the caller (supervisor).

- If option `{hibernate_after, HibernateAfterTimeout}` is present, the
  `gen_event` process awaits any message for `HibernateAfterTimeout`
  milliseconds and if no message is received, the process
  goes into hibernation automatically (by calling `proc_lib:hibernate/3`).

If the event manager is successfully created,
the function returns `{ok, Pid}` where `Pid` is the `t:pid/0`
of the event manager.

If a process with the specified `EventMgrName` exists already,
the function returns `{error,{already_started,OtherPid}}`,
where `OtherPid` is the pid of that process, and the event manager process
exits with reason `normal`.

If the event manager fails to start within the specified start timeout
`{timeout, Time}`, which is very unlikely since the start
does not interact with other processes, the function returns
`{error, timeout}` and the failed event manager is killed with
[`exit(_, kill)`](`erlang:exit/2`).

If `start_link/1,2` returns `{error, _}`, the started event manager process
has terminated.  If an `'EXIT'` message was delivered
to the calling process (due to the process link), that message
has been consumed.

> #### Warning {: .warning }
>
> Before OTP 26.0, if the started event manager failed to register
> its name, this founction could return
> `{error, {already_started, OtherPid}}` _before_
> the started event manager process had terminated,
> so starting again might fail because the registered name
> was not yet unregistered, and an `'EXIT'` message could arrive later
> to the process calling this function.
>
> But if the start timed out, this function killed
> the started event manager process and returned `{error, timeout}`,
> and then the process link `{'EXIT', Pid, killed}` message _was_ consumed.
>
> The start was made synchronous in OTP 26.0 and a guarantee
> was implemented that no process link `'EXIT'` message
> from a failed start will linger in the caller's inbox.

# `start_monitor`
*since OTP 23.0* 

```elixir
-spec start_monitor() -> start_mon_ret().
```

# `start_monitor`
*since OTP 23.0* 

```elixir
-spec start_monitor(EventMgrNameOrOptions :: emgr_name() | options()) -> start_mon_ret().
```

Creates a stand-alone event manager process,
monitored, possibly nameless.

Equivalent to [`start_monitor(EventMgrName, Options)`](`start_monitor/2`).

With argument `EventMgrName`, `Options` is `[]`.

With argument `Options` a nameless event manager is created.

For a description of the arguments and return values,
see `start_monitor/2` and `start_link/1`.

# `start_monitor`
*since OTP 23.0* 

```elixir
-spec start_monitor(EventMgtName :: emgr_name(), Options :: options()) -> start_mon_ret().
```

Creates a stand-alone event manager process, monitored.

The created event manager process is not part of a supervision tree
and thus has no supervisor.  A monitor is atomically set up
to the newly created process.

For a description of the arguments and return values, see
[`start_link/2`](`start_link/2`). Note that the return value
for a successful start differs from `start_link/2`.
`start_monitor/0,1,2` will return `{ok, {Pid, Mon}}`
where `Pid` is the process identifier of the process,
and `Mon` is a reference to the monitor set up to monitor the process.
If the start is not successful, the caller will be blocked
until the `DOWN` message has been received and removed
from the message queue.

# `stop`

```elixir
-spec stop(EventMgrRef :: emgr_ref()) -> ok.
```

# `stop`
*since OTP 18.0* 

```elixir
-spec stop(EventMgrRef :: emgr_ref(), Reason :: term(), Timeout :: timeout()) -> ok.
```

Stop an event manager.

Orders event manager `EventMgrRef` to exit with the specifies `Reason`,
and waits for it to terminate.  Before terminating, `gen_event` calls
[`Module:terminate(stop,...)`](`c:terminate/2`)
for each installed event handler.

The function returns `ok` if the event manager terminates
with the expected reason.  Any other reason than `normal`,
`shutdown`, or `{shutdown, Term}` causes an error report
to be issued using `m:logger`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for the event manager to terminate,
or the atom `infinity` to wait indefinitely.  If the event manager
has not terminated within the specified time, the call exits
the calling process with reason `timeout`.

If the process does not exist,
the call exits the calling process with reason `noproc`,
and with reason `{nodedown, Node}` if the connection fails
to the remote `Node` where the server runs.

# `swap_handler`

```elixir
-spec swap_handler(EventMgrRef :: emgr_ref(),
                   OldHandler :: {handler(), term()},
                   NewHandler :: {handler(), term()}) ->
                      ok | {error, term()}.
```

Replace an event handler.

This function replaces an event handler in event manager `EventMgrRef`.

For a description of `OldHandler` and `NewHandler`, see `add_handler/3`.

First the old event handler `OldHandler` is deleted. The event manager
calls `OldModule:terminate(Args1, ...)`, where `OldModule`
is the callback module of `OldHandler`, and collects the return value.

Then the new event handler `NewHandler` is added and initiated
by calling [`NewModule:init({Args2,Term})`](`c:init/1`), where `NewModule`
is the callback module of `NewHandler`, and `Term` is the return value
of [`OldModule:terminate/2`](`c:terminate/2`).  This makes it possible
to transfer information from `OldHandler` to `NewHandler`.

The new handler is added even if the the specified old event handler
is not installed, in which case `Term = error`, or if
[`OldModule:terminate/2`](`c:terminate/2`) fails with `Reason`,
in which case `Term = {'EXIT', Reason}`.  The old handler
is deleted even if [`NewModule:init/1`](`c:init/1`) fails.

If there was a supervised connection
between `OldHandler` and a process `Pid`,
there is a supervised connection between `NewHandler` and `Pid` instead.

If [`NewModule:init/1`](`c:init/1`) returns a correct value,
this function returns `ok`. If [`NewModule:init/1`](`c:init/1`) fails
with `Reason` or returns an unexpected value `Term`,
this function returns `{error, {'EXIT', Reason}}` or
`{error, Term}`, respectively.

# `swap_sup_handler`

```elixir
-spec swap_sup_handler(EventMgrRef :: emgr_ref(),
                       OldHandler :: {handler(), term()},
                       NewHandler :: {handler(), term()}) ->
                          ok | {error, term()}.
```

Replace an event handler, and supervise it.

Replaces an event handler in event manager `EventMgrRef`
in the same way as [`swap_handler/3`](`swap_handler/3`),
but also supervises the connection between `NewHandler`
and the calling process.

For a description of the arguments and return values, see `swap_handler/3`.

# `sync_notify`

```elixir
-spec sync_notify(EventMgrRef :: emgr_ref(), Event :: term()) -> ok.
```

Send a synchronous event notification to an event manager.

The event is sent to `EventMgrRef` that callsr calls
[`Module:handle_event/2`](`c:handle_event/2`) for each installed
event handler to handle the event. This function will return `ok`
after the event has been handled by all event handlers.

`Event` is any term that is passed as one of the arguments to
[`Module:handle_event/2`](`c:handle_event/2`).

# `wait_response`
*since OTP 23.0* 

```elixir
-spec wait_response(ReqId, WaitTime) -> Result
                       when
                           ReqId :: request_id(),
                           WaitTime :: response_timeout(),
                           Response ::
                               {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                           Result :: Response | timeout.
```

Wait for a request resonse.

Wait for the response to the request identifier `ReqId`. The request
must have been made by `send_request/3`, from the same process
that called `send_request/3`.

`WaitTime` specifies how long to wait for a response.
If no response is received within the specified time,
the function returns `timeout` and no cleanup is done,
Thus the function can be invoked repeatedly until a reply is returned.

The return value `Reply` is defined in the return value of
[`Module:handle_call/2`](`c:handle_call/2`).

If the specified event handler is not installed, the function returns
`{error, bad_module}`.  If the callback function fails with `Reason`,
or returns an unexpected value `Term`, this function returns
`{error,{'EXIT',Reason}}` or `{error,Term}`, respectively.
If the event manager dies before or during the request
this function returns `{error, {Reason, EventMgrRef}}`.

The difference between `receive_response/2` and
`wait_response/2` is that `receive_response/2` abandons the request
at timeout so that a potential future response is ignored,
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
                               {reply, Reply :: term()} | {error, {Reason :: term(), emgr_ref()}},
                           Result ::
                               {Response,
                                Label :: term(),
                                NewReqIdCollection :: request_id_collection()} |
                               no_request | timeout.
```

Wait for any request response in a collection.

Wait for a response in a `ReqIdCollection`.  All request identifiers
of `ReqIdCollection` must correspond to requests that have been made
using `send_request/3` or `send_request/5`, and all requests
must have been made by the process calling this function.

The `Label` in the response is the `Label` associated with
the request identifier that the response corresponds to.
The `Label` of a request identifier is associated
when [adding the request id](`reqids_add/3`) to a collection,
or when sending the request using `send_request/5`.

Compared to `wait_response/2`, the returned result or exception
associated with a specific request identifier will be wrapped
in a 3-tuple `{Response, Label, NewReqIdCollection}`.
`Response` is the value that would have been produced
by `wait_response/2`, `Label` is the value associated with
the specific [request identifier](`t:request_id/0`)
and `NewReqIdCollection` is a possibly modified
request identifier collection.

If `ReqIdCollection` is empty, `no_request` will be returned.

If no response is received before the `WaitTime` has expired,
`timeout` is returned.  It is valid to continue waiting
for a response as many times as needed up until a response
has been received and completed by `check_response()`,
`receive_response()`, or `wait_response()`.

The difference between `receive_response/3` and `wait_response/3`
is that `receive_response/3` abandons requests at time-out
so that potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` is `true`, the association with `Label`
has been deleted from `ReqIdCollection` in the resulting
`NewReqIdCollection`. If `Delete` is`false`, `NewReqIdCollection`
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

# `which_handlers`

```elixir
-spec which_handlers(EventMgrRef :: emgr_ref()) -> [handler()].
```

Return all event handlers in an event manager.

This function returns a list of all event handlers
installed in event manager `EventMgrRef`.

For a description of `Handler`, see `add_handler/3`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
