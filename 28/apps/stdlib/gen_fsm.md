# `gen_fsm`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/gen_fsm.erl#L22)

Deprecated and replaced by `m:gen_statem` in OTP 20.

Migration to gen_statem
-----------------------

Here follows a simple example of turning a gen_fsm into a `m:gen_statem`.
The example comes from the previous User's Guide for `gen_fsm`

```erlang
-module(code_lock).
-define(NAME, code_lock).
%-define(BEFORE_REWRITE, true).

-ifdef(BEFORE_REWRITE).
-behaviour(gen_fsm).
-else.
-behaviour(gen_statem).
-endif.

-export([start_link/1, button/1, stop/0]).

-ifdef(BEFORE_REWRITE).
-export([init/1, locked/2, open/2, handle_sync_event/4, handle_event/3,
     handle_info/3, terminate/3, code_change/4]).
-else.
-export([init/1, callback_mode/0, locked/3, open/3,
     terminate/3, code_change/4]).
%% Add callback__mode/0
%% Change arity of the state functions
%% Remove handle_info/3
-endif.

-ifdef(BEFORE_REWRITE).
start_link(Code) ->
    gen_fsm:start_link({local, ?NAME}, ?MODULE, Code, []).
-else.
start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
-endif.

-ifdef(BEFORE_REWRITE).
button(Digit) ->
    gen_fsm:send_event(?NAME, {button, Digit}).
-else.
button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).
    %% send_event is asynchronous and becomes a cast
-endif.

-ifdef(BEFORE_REWRITE).
stop() ->
    gen_fsm:sync_send_all_state_event(?NAME, stop).
-else.
stop() ->
    gen_statem:call(?NAME, stop).
    %% sync_send is synchronous and becomes call
    %% all_state is handled by callback code in gen_statem
-endif.

init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok, locked, Data}.

-ifdef(BEFORE_REWRITE).
-else.
callback_mode() ->
    state_functions.
%% state_functions mode is the mode most similar to
%% gen_fsm. There is also handle_event mode which is
%% a fairly different concept.
-endif.

-ifdef(BEFORE_REWRITE).
locked({button, Digit}, Data0) ->
    case analyze_lock(Digit, Data0) of
    {open = StateName, Data} ->
        {next_state, StateName, Data, 10000};
    {StateName, Data} ->
        {next_state, StateName, Data}
    end.
-else.
locked(cast, {button,Digit}, Data0) ->
    case analyze_lock(Digit, Data0) of
    {open = StateName, Data} ->
        {next_state, StateName, Data, 10000};
    {StateName, Data} ->
        {next_state, StateName, Data}
    end;
locked({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
locked({info, Msg}, StateName, Data) ->
    handle_info(Msg, StateName, Data).
%% Arity differs
%% All state events are dispatched to handle_call and handle_info help
%% functions. If you want to handle a call or cast event specifically
%% for this state you would add a special clause for it above.
-endif.

-ifdef(BEFORE_REWRITE).
open(timeout, State) ->
     do_lock(),
    {next_state, locked, State};
open({button,_}, Data) ->
    {next_state, locked, Data}.
-else.
open(timeout, _, Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, locked, Data};
open({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
open(info, Msg, Data) ->
    handle_info(Msg, open, Data).
%% Arity differs
%% All state events are dispatched to handle_call and handle_info help
%% functions. If you want to handle a call or cast event specifically
%% for this state you would add a special clause for it above.
-endif.

-ifdef(BEFORE_REWRITE).
handle_sync_event(stop, _From, _StateName, Data) ->
    {stop, normal, ok, Data}.

handle_event(Event, StateName, Data) ->
    {stop, {shutdown, {unexpected, Event, StateName}}, Data}.

handle_info(Info, StateName, Data) ->
    {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.
-else.
-endif.

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Internal functions
-ifdef(BEFORE_REWRITE).
-else.
handle_call(From, stop, Data) ->
     {stop_and_reply, normal,  {reply, From, ok}, Data}.

handle_info(Info, StateName, Data) ->
    {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.
%% These are internal functions for handling all state events
%% and not behaviour callbacks as in gen_fsm
-endif.

analyze_lock(Digit, #{code := Code, remaining := Remaining} = Data) ->
     case Remaining of
         [Digit] ->
         do_unlock(),
         {open,  Data#{remaining := Code}};
         [Digit|Rest] -> % Incomplete
             {locked, Data#{remaining := Rest}};
         _Wrong ->
             {locked, Data#{remaining := Code}}
     end.

do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).
```

OTP 19 Documentation
--------------------

### Module

`gen_fsm`

### Module Summary

Generic finite state machine behavior.

### Description

This behavior module provides a finite state machine.
A generic finite state machine process (`gen_fsm`) implemented
using this module has a standard set of interface functions
and includes functionality for tracing and error reporting.
It also fits into an OTP supervision tree.  For more information,
see [OTP Design Principles](`e:system:design_principles.md`).

A `gen_fsm` process assumes all specific parts to be located
in a callback module exporting a predefined set of functions.
The relationship between the behavior functions
and the callback functions is as follows:

``` text
gen_fsm module                    Callback module
--------------                    ---------------
gen_fsm:start
gen_fsm:start_link                -----> Module:init/1

gen_fsm:stop                      -----> Module:terminate/3

gen_fsm:send_event                -----> Module:StateName/2

gen_fsm:send_all_state_event      -----> Module:handle_event/3

gen_fsm:sync_send_event           -----> Module:StateName/3

gen_fsm:sync_send_all_state_event -----> Module:handle_sync_event/4

-                                 -----> Module:handle_info/3

-                                 -----> Module:terminate/3

-                                 -----> Module:code_change/4
```

If a callback function fails or returns a bad value,
the `gen_fsm` process terminates.

A `gen_fsm` process handles system messages as described
in [sys(3)](`m:sys`).  The sys module can be used for
debugging a `gen_fsm` process.

Notice that a `gen_fsm` process does not trap exit signals automatically,
this must be explicitly initiated in the callback module.

Unless otherwise stated, all functions in this module fail
if the specified `gen_fsm` process does not exist
or if bad arguments are specified.

The gen_fsm process can go into hibernation (see `erlang:hibernate/3`)
if a callback function specifies `hibernate` instead of a time-out value.
This can be useful if the server is expected to be idle for a long time.
However, use this feature with care, as hibernation implies at least
two garbage collections (when hibernating and shortly after waking up)
and is not something you want to do between each call
to a busy state machine.

### Callback Functions

See the [Callback Functions](#callbacks-deprecated) section
for the functions to be exported from a `gen_fsm` callback module.

[]() {: #state-name }
**State name** denotes a state of the state machine.

[]() {: #state-data }
**State data** denotes the internal state of the Erlang process
that implements the state machine.

# `enter_loop_opt`
*not exported* 

```elixir
-type enter_loop_opt() :: {debug, Dbgs :: [sys:debug_option()]}.
```

[Start options](#start-options) for the
[`enter_loop/4,5,6`](`enter_loop/6`), [`start/3,4`](`start/3`),
and [`start_link/3,4`](`start_link/3`) functions.

See `start_link/4`.

# `from`
*not exported* 

```elixir
-type from() :: {To :: pid(), Tag :: term()}.
```

Reply destination. See `reply/2`

# `fsm_name`
*not exported* 

```elixir
-type fsm_name() ::
          {local, LocalName :: atom()} |
          {global, GlobalName :: term()} |
          {via, RegMod :: module(), ViaName :: term()}.
```

[FSM name](#fsm-name) specification:
`local`, `global`, or `via` registered.

To be used when starting a `gen_fsm`. See `start_link/4`.

# `fsm_ref`
*not exported* 

```elixir
-type fsm_ref() ::
          pid() |
          (LocalName :: atom()) |
          {Name :: atom(), Node :: atom()} |
          {global, GlobalName :: term()} |
          {via, RegMod :: module(), ViaName :: term()}.
```

[FSM reference](#fsm-ref) `t:pid/0` or registered `t:fsm_name/0`.

To be used in for example `send_event/2` to specify the server.

# `start_opt`
*not exported* 

```elixir
-type start_opt() ::
          {timeout, Time :: timeout()} | {spawn_opt, [proc_lib:start_spawn_option()]} | enter_loop_opt().
```

[Start options](#start-options) for the [`start/3,4`](`start/3`),
and [`start_link/3,4`](`start_link/3`) functions.

See `start_link/4`.

# `code_change`
*optional* 

> This callback is deprecated. the callback gen_fsm:code_change(_,_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback code_change(OldVsn, StateName, StateData, Extra) -> {ok, NextStateName, NewStateData}
                         when
                             OldVsn :: Vsn | {down, Vsn},
                             Vsn :: term(),
                             StateName :: atom(),
                             NextStateName :: atom(),
                             StateData :: term(),
                             NewStateData :: term(),
                             Extra :: term().
```

Update the internal [*state data*](#state-data) during upgrade/downgrade.

This function is called by a `gen_fsm` process when it is to update
its internal [*state data*](#state-data)
during a release upgrade/downgrade, that is,
when instruction `{update, Module, Change, ...}`,
where `Change = {advanced, Extra}`, is given in the appup file;
see [Release Handling Instructions in OTP Design Principles](`e:system:release_handling.md#instr`).

For an upgrade, `OldVsn` is `Vsn`, and for a downgrade,
`OldVsn` is `{down, Vsn}`. `Vsn` is defined by the vsn attribute(s)
of the old version of the callback module `Module`.  If no such
 attribute is defined, the version is the checksum of the Beam file.

`StateName` is the current [*state name*](#state-name)
 and `StateData` the internal [*state data*](#state-data)
 of the `gen_fsm` process.

`Extra` is passed "as is" from the `{advanced, Extra}` part
 of the update instruction.

The function is to return the new current [*state name*](#state-name)
and updated internal data.

# `format_status`
*optional* 

> This callback is deprecated. the callback gen_fsm:format_status(_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback format_status(Opt, nonempty_improper_list(PDict, [StateData])) -> Status
                           when
                               Opt :: normal | terminate,
                               PDict :: [{Key :: term(), Value :: term()}],
                               StateData :: term(),
                               Status :: term().
```

Optional function for providing a term describing
the current `gen_fsm` process status.

The second argument is `[PDict, StateData]`, that is, a list
with the 2 elements, in that order.

> #### Note {: .info }
>
> This callback is optional, so callback modules need not export it.
> The `gen_fsm` module provides a default implementation
> of this function that returns the callback module
> [*state data*](#state-data).

This function is called by a `gen_fsm` process
in the following situations:

- One of [`sys:get_status/1,2`](`sys:get_status/1`) is invoked to get
  the `gen_fsm` status. `Opt` is set to the atom `normal` for this case.
- The `gen_fsm` process terminates abnormally and logs an error.
  `Opt` is set to the atom terminate for this case.

This function is useful for changing the form and appearance
of the `gen_fsm` status for these cases.  A callback module
wishing to change the [`sys:get_status/1,2`](`sys:get_status/1`)
return value as well as how its status appears in termination error logs,
exports an instance of `c:format_status/2` that returns a term
describing the current status of the `gen_fsm` process.

`PDict` is the current value of the process dictionary
of the `gen_fsm` process.

`StateData` is the internal [*state data*](#state-data)
of the `gen_fsm` process.

The function is to return `Status`, a term that change the details
of the current state and status of the `gen_fsm` process.
There are no restrictions of the form `Status` can take,
but for the [`sys:get_status/1,2`](`sys:get_status/1`) case
(when `Opt` is `normal`), the recommended form for the `Status` value
is `[{data, [{"StateData", Term}]}]`, where `Term` provides
relevant details of the `gen_fsm` [*state data*](#state-data).
Following this recommendation is not required, but it makes
the callback module status consistent with the rest of
the [`sys:get_status/1,2`](`sys:get_status/1`) return value.

One use for this function is to return compact alternative
[*state data*](#state-data) representations to avoid
that large state terms are printed in log files.

# `handle_event`

> This callback is deprecated. the callback gen_fsm:handle_event(_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback handle_event(Event, StateName, StateData) -> Result
                          when
                              Event :: term(),
                              StateName :: atom(),
                              StateData :: term(),
                              Result ::
                                  {next_state, NextStateName, NewStateData} |
                                  {next_state, NextStateName, NewStateData, Timeout} |
                                  {next_state, NextStateName, NewStateData, hibernate} |
                                  {stop, Reason, NewStateData},
                              NextStateName :: atom(),
                              NewStateData :: term(),
                              Timeout :: timeout(),
                              Reason :: term().
```

Handle an asynchronous event.

Whenever a `gen_fsm` process receives an event sent using
`send_all_state_event/2`, this function is called to handle the event.

`StateName` is the current [*state name*](#state-name)
of the `gen_fsm` process.

For a description of the other arguments and possible return values,
see [`Module:StateName/2`](`c:'StateName'/2`).

# `handle_info`
*optional* 

> This callback is deprecated. the callback gen_fsm:handle_info(_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback handle_info(Info, StateName, StateData) -> Result
                         when
                             Info :: term(),
                             StateName :: atom(),
                             StateData :: term(),
                             Result ::
                                 {next_state, NextStateName, NewStateData} |
                                 {next_state, NextStateName, NewStateData, Timeout} |
                                 {next_state, NextStateName, NewStateData, hibernate} |
                                 {stop, Reason, NewStateData},
                             NextStateName :: atom(),
                             NewStateData :: term(),
                             Timeout :: timeout(),
                             Reason :: normal | term().
```

Handle an incoming message

This function is called by a `gen_fsm` process when it receives
any other message than a synchronous or asynchronous event
(or a system message).

`Info` is the received message.

For a description of the other arguments and possible return values,
see [`Module:StateName/2`](`c:'StateName'/2`).

# `handle_sync_event`

> This callback is deprecated. the callback gen_fsm:handle_sync_event(_,_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback handle_sync_event(Event, From, StateName, StateData) -> Result
                               when
                                   Event :: term(),
                                   From :: from(),
                                   StateName :: atom(),
                                   StateData :: term(),
                                   Result ::
                                       {reply, Reply, NextStateName, NewStateData} |
                                       {reply, Reply, NextStateName, NewStateData, Timeout} |
                                       {reply, Reply, NextStateName, NewStateData, hibernate} |
                                       {next_state, NextStateName, NewStateData} |
                                       {next_state, NextStateName, NewStateData, Timeout} |
                                       {next_state, NextStateName, NewStateData, hibernate} |
                                       {stop, Reason, Reply, NewStateData} |
                                       {stop, Reason, NewStateData},
                                   Reply :: term(),
                                   NextStateName :: atom(),
                                   NewStateData :: term(),
                                   Timeout :: timeout(),
                                   Reason :: term().
```

Handle a synchronous event.

Whenever a `gen_fsm` process receives an event sent using
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`),
this function is called to handle the event.

`StateName` is the current [*state name*](#state-name)
of the `gen_fsm` process.

For a description of the other arguments and possible return values,
see [`Module:StateName/3`](`c:'StateName'/3`).

# `init`

> This callback is deprecated. the callback gen_fsm:init(_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback init(Args) -> Result
                  when
                      Args :: term(),
                      Result ::
                          {ok, StateName, StateData} |
                          {ok, StateName, StateData, Timeout} |
                          {ok, StateName, StateData, hibernate} |
                          {stop, Reason} |
                          ignore,
                      StateName :: atom(),
                      StateData :: term(),
                      Timeout :: timeout(),
                      Reason :: term().
```

Initialize process and internal [*state name*](#state-name)
and [*state data*](#state-data).

Whenever a `gen_fsm` process is started using
[`start/3,4`](`start/4`) or [`start_link/3,4`](`start_link/4`),
this function is called by the new process to initialize.

`Args` is the `Args` argument provided to the start function.

If initialization is successful, the function is to return
{ok, StateName, StateData}, {ok, StateName, StateData, Timeout},
or {ok, StateName, StateData, hibernate}, where `StateName`
is the initial [*state name*](#state-name) and `StateData`
the initial [*state data*](#state-data) of the `gen_fsm` process.

If an `t:integer/0` time-out value is provided, a time-out occurs
unless an event or a message is received within `Timeout` milliseconds.
A time-out is represented by the atom `timeout` and is to be handled
by the [`Module:StateName/2`](`c:'StateName'/2`) callback functions.
The atom `infinity` can be used to wait indefinitely, this is
the default value.

If `hibernate` is specified instead of a time-out value,
the process goes into hibernation when waiting for the next message
to arrive (by calling `proc_lib:hibernate/3`).

If the initialization fails, the function returns `{stop, Reason}`,
where `Reason` is any term, or `ignore`.

# `StateName`
*optional* 

> This callback is deprecated. the callback gen_fsm:'StateName'(_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback 'StateName'(Event, StateData) -> Result
                         when
                             Event :: timeout | term(),
                             StateData :: term(),
                             Result ::
                                 {next_state, NextStateName, NewStateData} |
                                 {next_state, NextStateName, NewStateData, Timeout} |
                                 {next_state, NextStateName, NewStateData, hibernate} |
                                 {stop, Reason, NewStateData},
                             NextStateName :: atom(),
                             NewStateData :: term(),
                             Timeout :: timeout(),
                             Reason :: term().
```

Handle an asynchronous event.

There is to be one instance of this function
for each possible [*state name*](#state-name).
Whenever a `gen_fsm` process receives an event sent using `send_event/2`,
the instance of this function with the same name as the current
[*state name*](#state-name) `StateName` is called to handle the event.
It is also called if a time-out occurs.

`Event` is either the atom `timeout`, if a time-out has occurred,
or the `Event` argument provided to `send_event/2`.

`StateData` is the [*state data*](#state-data) of the `gen_fsm` process.

If the function returns `{next_state, NextStateName, NewStateData}`,
`{next_state, NextStateName, NewStateData, Timeout}`,
or `{next_state, NextStateName, NewStateData, hibernate}`,
the `gen_fsm` process continues executing with
the current [*state name*](#state-name) set to `NextStateName`
and with the possibly updated [*state data*](#state-data)
`NewStateData`.  For a description of `Timeout` and `hibernate`,
see [`Module:init/1`](`c:init/1`).

If the function returns `{stop ,Reason, NewStateData}`,
the `gen_fsm` process calls
[`Module:terminate(Reason, StateName, NewStateData)`](`c:terminate/3`)
and terminates.

# `StateName`
*optional* 

> This callback is deprecated. the callback gen_fsm:'StateName'(_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback 'StateName'(Event, From, StateData) -> Result
                         when
                             Event :: term(),
                             From :: from(),
                             StateData :: term(),
                             Result ::
                                 {reply, Reply, NextStateName, NewStateData} |
                                 {reply, Reply, NextStateName, NewStateData, Timeout} |
                                 {reply, Reply, NextStateName, NewStateData, hibernate} |
                                 {next_state, NextStateName, NewStateData} |
                                 {next_state, NextStateName, NewStateData, Timeout} |
                                 {next_state, NextStateName, NewStateData, hibernate} |
                                 {stop, Reason, Reply, NewStateData} |
                                 {stop, Reason, NewStateData},
                             Reply :: term(),
                             NextStateName :: atom(),
                             NewStateData :: term(),
                             Timeout :: timeout(),
                             Reason :: normal | term().
```

Handle a synchronous event.

There is to be one instance of this function
for each possible [*state name*](#state-name).
Whenever a `gen_fsm` process receives an event sent using
[`sync_send_event/2,3`](`sync_send_event/3`),
the instance of this function with the same name
as the current [*state name*](#state-name) `StateName` is called
to handle the event.

`Event` is the `Event` argument provided to
[`sync_send_event/2,3`](`sync_send_event/3`).

`From` is a tuple `{Pid, Tag}` where `Pid` is the `t:pid/0`
of the process that called [`sync_send_event/2,3`](`sync_send_event/3`),
`Tag` is a unique tag.

`StateData` is the [*state data*](#state-data) of the `gen_fsm` process.

- If `{reply, Reply, NextStateName, NewStateData}`,
  `{reply, Reply, NextStateName, NewStateData, Timeout}`,
  or `{reply, Reply, NextStateName, NewStateData, hibernate}` is returned,
  `Reply` is given back to `From` as the return value of
  [`sync_send_event/2,3`](`sync_send_event/3`).
  The `gen_fsm` process then continues executing
  with the current [*state name*](#state-name) set to `NextStateName`
  and with the possibly updated [*state data*](#state-data) `NewStateData`.
  For a description of `Timeout` and `hibernate`,
  see [`Module:init/1`](`c:init/1`).

- If `{next_state, NextStateName, NewStateData}`,
  `{next_state, NextStateName, NewStateData, Timeout}`,
  or `{next_state, NextStateName, NewStateData, hibernate}` is returned,
  the `gen_fsm` process continues executing in `NextStateName`
  with `NewStateData`.  Any reply to `From`
  must be specified explicitly using `reply/2`.

- If the function returns `{stop, Reason, Reply, NewStateData}`,
  `Reply` is given back to `From`.  If the function returns
  {stop, Reason, NewStateData}, any reply to `From` must be specified
  explicitly using `reply/2`.  The `gen_fsm` process then calls
  [`Module:terminate(Reason, StateName, NewStateData)`](`c:terminate/3`)
  and terminates.

# `terminate`
*optional* 

> This callback is deprecated. the callback gen_fsm:terminate(_,_,_) is deprecated; use the 'gen_statem' module instead.

```elixir
-callback terminate(Reason, StateName, StateData) -> _
                       when
                           Reason :: normal | shutdown | {shutdown, term()} | term(),
                           StateName :: atom(),
                           StateData :: term().
```

Clean up before termination.

This function is called by a `gen_fsm` process
when it is about to terminate.  It is to be the opposite of
[`Module:init/1`](`c:init/1`) and do any necessary cleaning up.
When it returns, the `gen_fsm` process terminates with `Reason`.
The return value is ignored.

`Reason` is a term denoting the stop reason, `StateName` is
the current [*state name*](#state-name),
and `StateData` is the [*state data*](#state-data)
of the `gen_fsm` process.

`Reason` depends on why the `gen_fsm` process is terminating.
If it is because another callback function has returned a stop tuple
`{stop, ...}`, `Reason` has the value specified in that tuple.
If it is because of a failure, `Reason` is the error reason.

If the `gen_fsm` process is part of a supervision tree
and is ordered by its supervisor to terminate, this function
is called with `Reason = shutdown` if the following conditions apply:

- The gen_fsm process has been set to trap exit signals.

- The shutdown strategy as defined in the child specification
  of the supervisor is an integer time-out value, not brutal_kill.

Even if the gen_fsm process is **not** part of a supervision tree,
this function is called if it receives an `'EXIT'` message
from its parent. `Reason` is the same as in the `'EXIT'` message.

Otherwise, the gen_fsm process terminates immediately.

Notice that for any other reason than `normal`, `shutdown`,
or `{shutdown, Term}` the `gen_fsm` process is assumed to terminate
because of an error and an error report is issued
using `error_logger:format/2`.

# `cancel_timer`

> This function is deprecated. gen_fsm:cancel_timer/1 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec cancel_timer(Ref) -> RemainingTime | false
                      when Ref :: reference(), RemainingTime :: non_neg_integer().
```

Cancel an internal timer in a generic FSM.

Cancels an internal timer referred by `Ref` in the `gen_fsm` process
that calls this function.

`Ref` is a reference returned from `send_event_after/2`
or `start_timer/2`.

If the timer has already timed out, but the event not yet been delivered,
it is cancelled as if it had not timed out, so there is no false
timer event after returning from this function.

Returns the remaining time in milliseconds until the timer
would have expired if `Ref` referred to an active timer,
otherwise `false`.

# `enter_loop`

> This function is deprecated. gen_fsm:enter_loop/4 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec enter_loop(Module, Options, StateName, StateData) -> no_return()
                    when
                        Module :: module(),
                        Options :: [enter_loop_opt()],
                        StateName :: atom(),
                        StateData :: term().
```

Enter the `gen_fsm` receive loop.

Equivalent to `enter_loop/6` with `Timeout = infinity`
but the started server is not registered as for `start_link/3`.

# `enter_loop`

> This function is deprecated. gen_fsm:enter_loop/5 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec enter_loop(Module, Options, StateName, StateData, FsmName) -> no_return()
                    when
                        Module :: module(),
                        Options :: [enter_loop_opt()],
                        StateName :: atom(),
                        StateData :: term(),
                        FsmName :: fsm_name();
                (Module, Options, StateName, StateData, Timeout) -> no_return()
                    when
                        Module :: module(),
                        Options :: enter_loop_opt(),
                        StateName :: atom(),
                        StateData :: term(),
                        Timeout :: timeout().
```

Enter the `gen_fsm` receive loop.

With argument `FsmName` equivalent to `enter_loop/6`
with `Timeout = infinity`.

With argument `Timeout` equivalent to `enter_loop/6`
but the started server is not registered as for `start_link/3`.

# `enter_loop`

> This function is deprecated. gen_fsm:enter_loop/6 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec enter_loop(Module, Options, StateName, StateData, FsmName, Timeout) -> no_return()
                    when
                        Module :: module(),
                        Options :: [enter_loop_opt()],
                        StateName :: atom(),
                        StateData :: term(),
                        FsmName :: fsm_name() | pid(),
                        Timeout :: timeout().
```

Enter the `gen_fsm` receive loop.

Makes an existing process into a `gen_fsm` process.  Does not return,
instead the calling process enters the `gen_fsm` receive loop
and becomes a `gen_fsm` process.  The process must have been started
using one of the start functions in `m:proc_lib`.  The user is responsible
for any initialization of the process, including registering a name for it.

This function is useful when a more complex initialization procedure
is needed than the `gen_fsm` behavior provides.

`Module`, `Options`, and `FsmName` have the same meanings
as when calling [`start[_link]/3,4`](`start_link/4`).
However, the process must have been registered according to
`FsmName` before this function is called.

`StateName`, `StateData`, and `Timeout` have the same meanings
as in the return value of [`Module:init/1`](`c:init/1`).
The callback module `Module` does not need to export
an `c:init/1` function.

The function fails if the calling process was not started
by a `m:proc_lib` start function, or if it is not registered
according to `FsmName`.

# `reply`

> This function is deprecated. gen_fsm:reply/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec reply(Caller, Reply) -> Result when Caller :: from(), Reply :: term(), Result :: term().
```

Send a reply to a caller.

This function can be used by a `gen_fsm` process to explicitly send
a reply to a client process that called
[`sync_send_event/2,3`](`sync_send_event/3`) or
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`)
when the reply cannot be defined in the return value of
[`Module:StateName/3`](`c:'StateName'/3`) or
[`Module:handle_sync_event/4`](`c:handle_sync_event/4`).

`Caller` must be the `From` argument provided to the callback function.
`Reply` is any term given back to the client as the return value of
[`sync_send_event/2,3`](`sync_send_event/3`) or
[`sync_send_all_state_event/2,3`](`sync_send_all_state_event/3`).

Return value `Result` is not further defined, and is always to be ignored.

# `send_all_state_event`

> This function is deprecated. gen_fsm:send_all_state_event/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec send_all_state_event(FsmRef, Event) -> ok when FsmRef :: fsm_ref(), Event :: term().
```

Send an event asynchronously to a generic FSM.

Sends an event asynchronously to the `FsmRef` of the `gen_fsm` process
and returns `ok` immediately.  The `gen_fsm` process calls
[`Module:handle_event/3`](`c:handle_event/3`) to handle the event.

For a description of the arguments, see `send_event/2`.

The difference between `send_event/2` and `send_all_state_event/2`
is which callback function is used to handle the event.
This function is useful when sending events that are handled
the same way in every state, as only one `handle_event` clause
is needed to handle the event instead of one clause
in each state name function.

# `send_event`

> This function is deprecated. gen_fsm:send_event/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec send_event(FsmRef, Event) -> ok when FsmRef :: fsm_ref(), Event :: term().
```

Send an event asynchronously to a generic FSM.

Sends `Event` to the `FsmRef` of the `gen_fsm` process
and returns `ok` immediately.  The `gen_fsm` process calls
[`Module:StateName/2`](`c:'StateName'/2`) to handle the event,
where `StateName` is the name of the current state
of the `gen_fsm` process.

[](){: #fsm-ref }
`FsmRef` can be any of the following:

- The `t:pid/0`
- `Name`, if the `gen_fsm` process is locally registered
- `{Name, Node}`, if the `gen_fsm` process is locally registered
  at another node
- `{global, GlobalName}`, if the `gen_fsm` process is globally registered
- `{via, Module, ViaName}`, if the `gen_fsm` process is registered
  through an alternative process registry

`Event` is any term that is passed as one of the arguments
to `Module:StateName/2`.

# `send_event_after`

> This function is deprecated. gen_fsm:send_event_after/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec send_event_after(Time, Event) -> Ref
                          when Time :: non_neg_integer(), Event :: term(), Ref :: reference().
```

Send a delayed event internally in a generic FSM.

Sends a delayed event internally in the `gen_fsm` process
that calls this function after `Time` milliseconds.
Returns immediately a reference that can be used to cancel
the delayed send using `cancel_timer/1`.

The `gen_fsm` process calls [`Module:StateName/2`](`c:'StateName'/2`)
to handle the event, where `'StateName'` is the name of
the current state of the `gen_fsm` process at the time
the delayed event is delivered.

`Event` is any term that is passed as one of the arguments
to [`Module:StateName/2`](`c:'StateName'/2`).

# `start`

> This function is deprecated. gen_fsm:start/3 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec start(Module, Args, Options) -> Result
               when
                   Module :: module(),
                   Args :: term(),
                   Options :: [start_opt()],
                   Result :: {ok, Pid} | ignore | {error, Reason},
                   Pid :: pid(),
                   Reason :: term().
```

Create a standalone `gen_fsm` process, not registered.

Equivalent to [`start(Name, Mod, Args, Options)`](`start/4`)
without registering a `Name`.

For a description of arguments and return values,
see [`start_link/3,4`](`start_link/3`).

# `start`

> This function is deprecated. gen_fsm:start/4 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec start(FsmName, Module, Args, Options) -> Result
               when
                   FsmName :: fsm_name(),
                   Module :: module(),
                   Args :: term(),
                   Options :: [start_opt()],
                   Result :: {ok, Pid} | ignore | {error, Reason},
                   Pid :: pid(),
                   Reason :: {already_started, Pid} | term().
```

Create a standalone `gen_fsm` process.

The created process is not part of a supervision tree
and thus has no supervisor.

For a description of arguments and return values,
see [`start_link/3,4`](`start_link/4`).

# `start_link`

> This function is deprecated. gen_fsm:start_link/3 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec start_link(Module, Args, Options) -> Result
                    when
                        Module :: module(),
                        Args :: term(),
                        Options :: [start_opt()],
                        Result :: {ok, Pid} | ignore | {error, Reason},
                        Pid :: pid(),
                        Reason :: term().
```

Create a `gen_fsm` process in a supervision tree, not registered.

Equivalent to [`start_link(Name, Mod, Args, Options)`](`start_link/4`)
without registering a `Name`.

# `start_link`

> This function is deprecated. gen_fsm:start_link/4 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec start_link(FsmName, Module, Args, Options) -> Result
                    when
                        FsmName :: fsm_name(),
                        Module :: module(),
                        Args :: term(),
                        Options :: [start_opt()],
                        Result :: {ok, Pid} | ignore | {error, Reason},
                        Pid :: pid(),
                        Reason :: {already_started, Pid} | term().
```

Create a `gen_fsm` process in a supervision tree.

The process is created as part of a supervision tree.  The function
is to be called, directly or indirectly, by the supervisor.
For example, it ensures that the `gen_fsm` process
is linked to the supervisor.

The `gen_fsm` process calls [`Module:init/1`](`c:init/1`) to initialize.
To ensure a synchronized startup procedure,
[`start_link/3,4`](`start_link/4`) does not return
until `Module:init/1` has returned.

[]() {: #fsm-name }

- If **`FsmName = {local, Name}`**, the `gen_fsm` process
  is registered locally as `Name` using `register/2`.

- If **`FsmName = {global, GlobalName}`**, the `gen_fsm` process
  is registered globally as `GlobalName` using `global:register_name/2`.

- If **`FsmName = {via, Module, ViaName}`**,
  the `gen_fsm` process registers with the registry
  represented by `Module`.  The `Module` callback is to export
  the functions `register_name/2`, `unregister_name/1`,
  `whereis_name/1`, and `send/2`, which are to behave like
  the corresponding functions in `m:global`.
  Thus, `{via, global, GlobalName}` is a valid reference.

`Module` is the name of the callback module.

`Args` is any term that is passed as the argument to `Module:init/1`.

[]() {: #start-options }

If option **`{timeout, Time}`** is present, the `gen_fsm` process
is allowed to spend `Time` milliseconds initializing or it terminates
and the start function returns `{error, timeout}`.

If option **`{debug, Dbgs}`** is present, the corresponding `sys` function
is called for each item in `Dbgs`; see [`sys(3)`](`m:sys`).

If option **`{spawn_opt, SOpts}`** is present, `SOpts` is passed
as option list to the `spawn_opt` BIF that is used
to spawn the `gen_fsm` process; see `spawn_opt/2`.

> #### Note {: .info }
> Using spawn option `monitor` is not allowed, it causes
> the function to fail with reason `badarg`.

If the `gen_fsm` process is successfully created and initialized,
the function returns `{ok, Pid}`, where `Pid` is the pid
of the `gen_fsm` process.  If a process with the specified `FsmName`
exists already, the function returns `{error, {already_started, Pid}}`,
where `Pid` is the pid of that process.

If `Module:init/1` fails with `Reason`, the function returns
`{error, Reason}`.  If `Module:init/1` returns `{stop, Reason}`
or `ignore`, the process is terminated and the function returns
`{error, Reason}` or `ignore`, respectively.

# `start_timer`

> This function is deprecated. gen_fsm:start_timer/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec start_timer(Time, Msg) -> Ref when Time :: non_neg_integer(), Msg :: term(), Ref :: reference().
```

Send a time-out event internally in a generic FSM.

Sends a time-out event internally in the `gen_fsm process`
that calls this function after `Time` milliseconds.
Returns immediately a reference that can be used to cancel the timer
using `cancel_timer/1`.

The `gen_fsm` process calls [`Module:StateName/2`](`c:'StateName'/2`)
to handle the event, where `'StateName'` is the name
of the current state of the `gen_fsm` process at the time
the time-out message is delivered.

`Msg` is any term that is passed in the time-out message,
`{timeout, Ref, Msg}`, as one of the arguments
to [`Module:StateName/2`](`c:'StateName'/2`).

# `stop`

> This function is deprecated. gen_fsm:stop/1 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec stop(FsmRef) -> ok when FsmRef :: fsm_ref().
```

# `stop`

> This function is deprecated. gen_fsm:stop/3 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec stop(FsmRef, Reason, Timeout) -> ok
              when FsmRef :: fsm_ref(), Reason :: term(), Timeout :: timeout().
```

Synchronously stop a generic FSM.

Orders a generic finite state machine to exit with the specified `Reason`
and waits for it to terminate.  The `gen_fsm` process calls
[`Module:terminate/3`](`c:terminate/3`) before exiting.

The function returns `ok` if the generic finite state machine terminates
with the expected reason.  Any other reason than `normal`, `shutdown`,
or `{shutdown, Term}` causes an error report to be issued using
`error_logger:format/2`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for the generic FSM to terminate,
or the atom `infinity` to wait indefinitely.
If the generic finite state machine has not terminated
within the specified time, a `timeout` exception is raised.

If the process does not exist, a `noproc` exception is raised.

# `sync_send_all_state_event`

> This function is deprecated. gen_fsm:sync_send_all_state_event/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec sync_send_all_state_event(FsmRef, Event) -> Reply
                                   when FsmRef :: fsm_ref(), Event :: term(), Reply :: term().
```

# `sync_send_all_state_event`

> This function is deprecated. gen_fsm:sync_send_all_state_event/3 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec sync_send_all_state_event(FsmRef, Event, Timeout) -> Reply
                                   when
                                       FsmRef :: fsm_ref(),
                                       Event :: term(),
                                       Timeout :: timeout(),
                                       Reply :: term().
```

Send an event synchronously to a generic FSM.

Sends an event to the `FsmRef` of the `gen_fsm` process and waits
until a reply arrives or a time-out occurs.  The `gen_fsm` process calls
[`Module:handle_sync_event/4`](`c:handle_sync_event/4`)
to handle the event.

For a description of `FsmRef` and `Event`, see `send_event/2`.
For a description of `Timeout` and `Reply`, see `sync_send_event/3`.

For a discussion about the difference between `sync_send_event`
and `sync_send_all_state_event`, see `send_all_state_event/2`.

# `sync_send_event`

> This function is deprecated. gen_fsm:sync_send_event/2 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec sync_send_event(FsmRef, Event) -> Reply when FsmRef :: fsm_ref(), Event :: term(), Reply :: term().
```

# `sync_send_event`

> This function is deprecated. gen_fsm:sync_send_event/3 is deprecated; use the 'gen_statem' module instead.

```elixir
-spec sync_send_event(FsmRef, Event, Timeout) -> Reply
                         when
                             FsmRef :: fsm_ref(), Event :: term(), Timeout :: timeout(), Reply :: term().
```

Send an event synchronously to a generic FSM.

Sends an event to the `FsmRef` of the `gen_fsm` process
and waits until a reply arrives or a time-out occurs.
The `gen_fsm` process calls [`Module:StateName/3`](`c:'StateName'/3`)
to handle the event, where `'StateName'` is the name
of the current state of the `gen_fsm` process.

For a description of `FsmRef` and `Event`, see `send_event/2`.

`Timeout` is an integer greater than zero that specifies
how many milliseconds to wait for a reply, or the atom `infinity`
to wait indefinitely.  If no reply is received within the specified time,
the function call fails.

Return value `Reply` is defined in the return value of
[`Module:StateName/3`](`c:'StateName'/3`)

> #### Note {: .info }
> The ancient behavior of sometimes consuming the server exit message
> if the server died during the call while linked to the client
> was removed in Erlang 5.6/OTP R12B.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
