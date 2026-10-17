# `timer`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/timer.erl#L22)

Timer functions.

This module provides functions related to time. Unless otherwise stated,
time is always measured in _milliseconds_. All timer functions return
immediately; the requested operations are performed asynchronously.

On success, the timer functions return `{ok, TRef}`, where `TRef` is a timer
reference. A timer reference is an opaque term whose contents must not be
changed, and which can be passed to `cancel/1` to cancel the requested
action.

The timeouts are not exact, but are _at least_ as long as requested.

Creating timers using `erlang:send_after/3` and `erlang:start_timer/3` is more
efficient than using the timers provided by this module. However, the timer
module has been improved in OTP 25, making it more efficient and less
susceptible to being overloaded. See
[the Timer Module section in the Efficiency Guide](`e:system:commoncaveats.md#timer-module`).

For more information on timers in Erlang in general, see the
[*Timers*](`e:erts:time_correction.md#timers`) section of the
[*Time and Time Correction in Erlang*](`e:erts:time_correction.md`)
ERTS User's guide.

## Examples

_Example 1_

The following example shows how to print "Hello World!" after 5 seconds:

```erlang
1> timer:apply_after(5000, io, format, ["~nHello World!~n", []]).
{ok,TRef}
Hello World!
```

_Example 2_

The following example shows a process that is killed if it does not finish
its task within a certain time limit:

```erlang
Pid = spawn(Mod, Fun, Args),
%% If the process is not done in 10 seconds, kill it
{ok, TRef} = timer:kill_after(timer:seconds(10), Pid),
...
%% We changed our mind...
timer:cancel(TRef),
...
```

## Notes on timer behavior

A timer can always be removed by calling `cancel/1`.

An interval timer created by evaluating any of the functions
`apply_interval/2`, `apply_interval/3`, `apply_interval/4`,
`apply_repeatedly/2`, `apply_repeatedly/3`, and `apply_repeatedly/4` is
automatically cancelled when the process that created it terminates.

An interval timer created by evaluating `send_interval/2` or `send_interval/3`
is automatically cancelled when the destination process terminates.

A one-shot timer, that is, a timer created by evaluating any of the functions
`apply_after/2`, `apply_after/3`, `apply_after/4`, `send_after/2`,
`send_after/3`, `exit_after/2`, `exit_after/3`, `kill_after/1`, and
`kill_after/2` is removed only when it reaches its timeout, or if it is
explicitly removed by a call to `cancel/1`.

The functions given to `apply_after/2`, `apply_after/3`, `apply_interval/2`,
`apply_interval/3`, `apply_repeatedly/2`, and `apply_repeatedly/3`, or denoted
by `Module`, `Function` and `Arguments` given to `apply_after/4`,
`apply_interval/4`, and `apply_repeatedly/4` are executed in a freshly spawned
process, and therefore calls to `self/0` in those functions will return the
process identifier of this process, which is different from the process that
called `timer:apply_*`.

_Example_

In the following example, a timer is set to execute a function after 1 second.
The function performs a fictional task and then informs the process that set
the timer about its completion, by sending it a `done` message.

The code below, which uses `self/0` _inside_ the timed function, does not work
as intended. The task gets done, but the `done` message gets sent to the wrong
process and is lost.

```erlang
1> timer:apply_after(1000, fun() -> do_something(), self() ! done end).
{ok,TRef}
2> receive done -> done after 5000 -> timeout end.
%% ... 5s pass...
timeout
```

The code below works as intended. It calls `self/0` in the process that sets
the timer, assigns the result to a variable, and uses that variable in the
function as the destination of the `done` message.

```erlang
1> Target = self().
<0.82.0>
2> timer:apply_after(1000, fun() -> do_something(), Target ! done end).
{ok,TRef}
3> receive done -> done after 5000 -> timeout end.
%% ... 1s passes...
done
```

Another option is to pass the message target as a parameter to the function.

```erlang
1> timer:apply_after(1000, fun(Target) -> do_something(), Target ! done end, [self()]).
{ok,TRef}
2> receive done -> done after 5000 -> timeout end.
%% ... 1s passes...
done
```

# `time`
*since OTP 28.0* 

```erlang
-nominal time() :: non_neg_integer().
```

Time in milliseconds.

# `tref`

```erlang
-opaque tref()
```

A timer reference.

# `apply_after`
*since OTP 27.0* 

```erlang
-spec apply_after(Time, Function) -> {ok, TRef} | {error, Reason}
                     when Time :: time(), Function :: fun(() -> _), TRef :: tref(), Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) after `Time`
milliseconds.

# `apply_after`
*since OTP 27.0* 

```erlang
-spec apply_after(Time, Function, Arguments) -> {ok, TRef} | {error, Reason}
                     when
                         Time :: time(),
                         Function :: fun((...) -> _),
                         Arguments :: [term()],
                         TRef :: tref(),
                         Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) after
`Time` milliseconds.

# `apply_after`

```erlang
-spec apply_after(Time, Module, Function, Arguments) -> {ok, TRef} | {error, Reason}
                     when
                         Time :: time(),
                         Module :: module(),
                         Function :: atom(),
                         Arguments :: [term()],
                         TRef :: tref(),
                         Reason :: term().
```

Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) after `Time`
milliseconds.

# `apply_interval`
*since OTP 27.0* 

```erlang
-spec apply_interval(Time, Function) -> {ok, TRef} | {error, Reason}
                        when Time :: time(), Function :: fun(() -> _), TRef :: tref(), Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) repeatedly at
intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.

# `apply_interval`
*since OTP 27.0* 

```erlang
-spec apply_interval(Time, Function, Arguments) -> {ok, TRef} | {error, Reason}
                        when
                            Time :: time(),
                            Function :: fun((...) -> _),
                            Arguments :: [term()],
                            TRef :: tref(),
                            Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) repeatedly
at intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.

# `apply_interval`

```erlang
-spec apply_interval(Time, Module, Function, Arguments) -> {ok, TRef} | {error, Reason}
                        when
                            Time :: time(),
                            Module :: module(),
                            Function :: atom(),
                            Arguments :: [term()],
                            TRef :: tref(),
                            Reason :: term().
```

Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) repeatedly at
intervals of `Time`, irrespective of whether a previously spawned process has
finished or not.

> #### Warning {: .warning }
>
> If the execution time of the spawned process is, on average, greater than the
> given `Time`, multiple such processes will run at the same time. With long
> execution times, short intervals, and many interval timers running, this may
> even lead to exceeding the number of allowed processes. As an extreme example,
> consider
> `[timer:apply_interval(1, timer, sleep, [1000]) || _ <- lists:seq(1, 1000)]`,
> that is, 1,000 interval timers executing a process that takes 1s to complete,
> started in intervals of 1ms, which would result in 1,000,000 processes running
> at the same time, far more than a node started with default settings allows
> (see the
> [System Limits section in the Efficiency Guide](`e:system:system_limits.md`)).

# `apply_repeatedly`
*since OTP 27.0* 

```erlang
-spec apply_repeatedly(Time, Function) -> {ok, TRef} | {error, Reason}
                          when
                              Time :: time(), Function :: fun(() -> _), TRef :: tref(), Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, []])`](`spawn/3`) repeatedly at
intervals of `Time`, waiting for the spawned process to finish before starting
the next.

# `apply_repeatedly`
*since OTP 27.0* 

```erlang
-spec apply_repeatedly(Time, Function, Arguments) -> {ok, TRef} | {error, Reason}
                          when
                              Time :: time(),
                              Function :: fun((...) -> _),
                              Arguments :: [term()],
                              TRef :: tref(),
                              Reason :: term().
```

Evaluates [`spawn(erlang, apply, [Function, Arguments])`](`spawn/3`) repeatedly
at intervals of `Time`, waiting for the spawned process to finish before
starting the next.

# `apply_repeatedly`
*since OTP 26.0* 

```erlang
-spec apply_repeatedly(Time, Module, Function, Arguments) -> {ok, TRef} | {error, Reason}
                          when
                              Time :: time(),
                              Module :: module(),
                              Function :: atom(),
                              Arguments :: [term()],
                              TRef :: tref(),
                              Reason :: term().
```

Evaluates [`spawn(Module, Function, Arguments)`](`spawn/3`) repeatedly at
intervals of `Time`, waiting for the spawned process to finish before starting
the next.

If the execution time of the spawned process is greater than the given `Time`,
the next process is spawned immediately after the one currently running has
finished. Assuming that execution times of the spawned processes performing the
applies on average are smaller than `Time`, the amount of applies made over a
large amount of time will be the same even if some individual execution times
are larger than `Time`. The system will try to catch up as soon as possible. For
example, if one apply takes `2.5*Time`, the following two applies will be made
immediately one after the other in sequence.

# `cancel`

```erlang
-spec cancel(TRef) -> {ok, cancel} | {error, Reason} when TRef :: tref(), Reason :: term().
```

Cancels a previously requested timer. `TRef` is a unique timer reference
returned by the timer function that requested the timer.

Returns `{ok, cancel}`, or `{error, Reason}` when `TRef` is not a timer
reference.

# `exit_after`

```erlang
-spec exit_after(Time, Reason1) -> {ok, TRef} | {error, Reason2}
                    when Time :: time(), TRef :: tref(), Reason1 :: term(), Reason2 :: term().
```

# `exit_after`

```erlang
-spec exit_after(Time, Target, Reason1) -> {ok, TRef} | {error, Reason2}
                    when
                        Time :: time(),
                        Target :: pid() | (RegName :: atom()),
                        TRef :: tref(),
                        Reason1 :: term(),
                        Reason2 :: term().
```

Sends an exit signal with reason `Reason1` to `Target`, which can be a local
process identifier or a registered name.

# `hms`

```erlang
-spec hms(Hours, Minutes, Seconds) -> MilliSeconds
             when
                 Hours :: non_neg_integer(),
                 Minutes :: non_neg_integer(),
                 Seconds :: non_neg_integer(),
                 MilliSeconds :: time().
```

Returns the number of milliseconds in `Hours + Minutes + Seconds`.

# `hours`

```erlang
-spec hours(Hours) -> MilliSeconds when Hours :: non_neg_integer(), MilliSeconds :: time().
```

Returns the number of milliseconds in `Hours`.

# `kill_after`

```erlang
-spec kill_after(Time) -> {ok, TRef} | {error, Reason2}
                    when Time :: time(), TRef :: tref(), Reason2 :: term().
```

# `kill_after`

```erlang
-spec kill_after(Time, Target) -> {ok, TRef} | {error, Reason2}
                    when
                        Time :: time(),
                        Target :: pid() | (RegName :: atom()),
                        TRef :: tref(),
                        Reason2 :: term().
```

# `minutes`

```erlang
-spec minutes(Minutes) -> MilliSeconds when Minutes :: non_neg_integer(), MilliSeconds :: time().
```

Returns the number of milliseconds in `Minutes`.

# `now_diff`

```erlang
-spec now_diff(T2, T1) -> Tdiff
                  when T1 :: erlang:timestamp(), T2 :: erlang:timestamp(), Tdiff :: integer().
```

Calculates the time difference `Tdiff = T2 - T1` in _microseconds_, where `T1`
and `T2` are timestamp tuples in the same format as returned by
`erlang:timestamp/0` or `os:timestamp/0`.

# `seconds`

```erlang
-spec seconds(Seconds) -> MilliSeconds when Seconds :: non_neg_integer(), MilliSeconds :: time().
```

Returns the number of milliseconds in `Seconds`.

# `send_after`

```erlang
-spec send_after(Time, Message) -> {ok, TRef} | {error, Reason}
                    when Time :: time(), Message :: term(), TRef :: tref(), Reason :: term().
```

# `send_after`

```erlang
-spec send_after(Time, Destination, Message) -> {ok, TRef} | {error, Reason}
                    when
                        Time :: time(),
                        Destination :: pid() | (RegName :: atom()) | {RegName :: atom(), Node :: node()},
                        Message :: term(),
                        TRef :: tref(),
                        Reason :: term().
```

Evaluates `Destination ! Message` after `Time` milliseconds.

`Destination` can be a local or remote process identifier, a registered name,
or a tuple `{RegName, Node}` for a registered name on the node `Node`.

See also [the Timer Module section in the Efficiency Guide](`e:system:commoncaveats.md#timer-module`).

# `send_interval`

```erlang
-spec send_interval(Time, Message) -> {ok, TRef} | {error, Reason}
                       when Time :: time(), Message :: term(), TRef :: tref(), Reason :: term().
```

# `send_interval`

```erlang
-spec send_interval(Time, Destination, Message) -> {ok, TRef} | {error, Reason}
                       when
                           Time :: time(),
                           Destination ::
                               pid() | (RegName :: atom()) | {RegName :: atom(), Node :: node()},
                           Message :: term(),
                           TRef :: tref(),
                           Reason :: term().
```

Evaluates `Destination ! Message` repeatedly after `Time` milliseconds.

`Destination` can be a local or remote process identifier, a registered name,
or a tuple `{RegName, Node}` for a registered name on the node `Node`.

# `sleep`

```erlang
-spec sleep(Time) -> ok when Time :: time() | infinity.
```

Suspends the process calling this function for `Time` milliseconds and then
returns `ok`, or suspends the process forever if `Time` is the atom `infinity`.
Naturally, this function does _not_ return immediately.

> #### Note {: .info }
>
> Before OTP 25, `timer:sleep/1` did not accept integer timeout values greater
> than `16#ffffffff`, that is, `2^32-1`. Since OTP 25, arbitrarily high integer
> values are accepted.

# `start`

```erlang
-spec start() -> ok.
```

Starts the timer server.

Normally, the server does not need to be started explicitly. It is started
dynamically when it is needed. This is useful during development, but in a
target system the server should be started explicitly. To this end, set the
[Kernel configuration parameter `start_timer`](`e:kernel:kernel_app.md#start_timer`)
to `true`.

# `tc`
*since OTP R14B03* 

```erlang
-spec tc(Fun) -> {Time, Value} when Fun :: fun(() -> term()), Time :: integer(), Value :: term().
```

# `tc`
*since OTP R14B* 

```erlang
-spec tc(Fun, Arguments) -> {Time, Value}
            when Fun :: fun((...) -> term()), Arguments :: [term()], Time :: integer(), Value :: term();
        (Fun, TimeUnit) -> {Time, Value}
            when
                Fun :: fun(() -> term()),
                TimeUnit :: erlang:time_unit(),
                Time :: integer(),
                Value :: term().
```

Measures the execution time of `Fun`.

Equivalent to [`tc(Fun, Arguments, microsecond)`](`tc/3`) if called as `tc(Fun, Arguments)`.

Measures the execution time of `Fun` in `TimeUnit` if called as `tc(Fun, TimeUnit)`. Added in OTP 26.0.

# `tc`

```erlang
-spec tc(Module, Function, Arguments) -> {Time, Value}
            when
                Module :: module(),
                Function :: atom(),
                Arguments :: [term()],
                Time :: integer(),
                Value :: term();
        (Fun, Arguments, TimeUnit) -> {Time, Value}
            when
                Fun :: fun((...) -> term()),
                Arguments :: [term()],
                TimeUnit :: erlang:time_unit(),
                Time :: integer(),
                Value :: term().
```

Measures the execution time of `Fun` or `apply(Module, Function, Arguments)`.

Equivalent to [`tc(Module, Function, Arguments, microsecond)`](`tc/4`) if called as `tc(Module, Function, Arguments)`.

Equivalent to [`tc(erlang, apply, [Fun, Arguments], TimeUnit)`](`tc/4`) if called as `tc(Fun, Arguments, TimeUnit)`. Added in OTP 26.0

# `tc`
*since OTP 26.0* 

```erlang
-spec tc(Module, Function, Arguments, TimeUnit) -> {Time, Value}
            when
                Module :: module(),
                Function :: atom(),
                Arguments :: [term()],
                TimeUnit :: erlang:time_unit(),
                Time :: integer(),
                Value :: term().
```

Evaluates [`apply(Module, Function, Arguments)`](`apply/3`) and measures the elapsed
real time as reported by `erlang:monotonic_time/0`.

Returns `{Time, Value}`, where `Time` is the elapsed real time in the
specified `TimeUnit`, and `Value` is what is returned from the apply.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
