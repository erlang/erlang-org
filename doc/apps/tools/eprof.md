# `eprof`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/tools/src/eprof.erl#L26)

A Time Profiling Tool for Erlang

The module `eprof` provides a set of functions for time profiling of Erlang
programs to find out how the execution time is used. The profiling is done using
the Erlang trace BIFs. Tracing of local function calls for a specified set of
processes is enabled when profiling is begun, and disabled when profiling is
stopped.

When using Eprof, expect a slowdown in program execution.

# `analyze_type`
*not exported* 

```erlang
-type analyze_type() :: procs | total.
```

# `trace_pattern_mfa`
*not exported* 

```erlang
-type trace_pattern_mfa() :: {atom(), atom(), arity() | '_'}.
```

# `analyze`
*since OTP R14B* 

```erlang
-spec analyze() -> ok | nothing_to_analyze.
```

# `analyze`
*since OTP R14B* 

```erlang
-spec analyze(TypeOpts) -> ok | nothing_to_analyze when TypeOpts :: analyze_type().
```

If `TypeOpts` is an atom, it is assumed to be a module name, and this
call is equivalent to [`analyze(TypeOpts, [])`](`analyze/2`).

Otherwise, if `TypeOpts` is a list, it assumed to be a list of options, and this
call is equivalent to [`analyze(procs, TypeOpts)`](`analyze/2`).

# `analyze`
*since OTP R14B* 

```erlang
-spec analyze(Type, Options) -> ok | nothing_to_analyze
                 when
                     Type :: analyze_type(),
                     Options :: [Option],
                     Option :: {filter, Filter} | {sort, Sort},
                     Filter :: [{calls, non_neg_integer()} | {time, float()}],
                     Sort :: time | calls | mfa.
```

Call this function when profiling has been stopped to display the results.

If `Type` is `procs`, the time spent in each function is shown separately
for each profiled process.

If `Type` is `total`, the time spent in each function is shown combined
for each profiled process.

Time is shown as percentage of total time and as absolute time in micro seconds.

# `log`

```erlang
-spec log(File) -> ok when File :: atom() | file:filename().
```

Call this function to ensure that the results displayed by
[`analyze/0,1,2`](`analyze/0`) are printed to the file `File` as well as to the
screen.

# `profile`

```erlang
-spec profile(Fun) -> {ok, Value} | {error, Reason}
                 when Fun :: fun(() -> term()), Value :: term(), Reason :: term();
             (Rootset) -> profiling | {error, Reason} when Rootset :: [atom() | pid()], Reason :: term().
```

If `FunRootset` is a fun, this call is equivalent to
[`profile([], FunRootset)`](`profile/2`).

If `FunRootset` is a list, it is assumed to be a `Rootset`, and this
call is equivalent to [`start_profiling(Rootset)`](`start_profiling/1`).

# `profile`

```erlang
-spec profile(Fun, Options) -> {ok, Value} | {error, Reason}
                 when
                     Fun :: fun(() -> term()),
                     Options :: [set_on_spawn | {set_on_spawn, boolean()}],
                     Value :: term(),
                     Reason :: term();
             (Rootset, Fun) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Fun :: fun(() -> term()),
                     Value :: term(),
                     Reason :: term().
```

If `Arg1` is a fun and `Arg2` is list, this call is equivalent to
[`profile([], Arg1, {'_','_','_'}, Arg2)`](`profile/4`).

If `Arg1` is a list and `Arg2` is a fun, this call is equivalent to
[`profile(Arg1, Arg2, {'_','_','_'}, Arg1)`](`profile/4`).

# `profile`
*since OTP R14B* 

```erlang
-spec profile(Rootset, Fun, Pattern) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Fun :: fun(() -> term()),
                     Pattern :: trace_pattern_mfa(),
                     Value :: term(),
                     Reason :: term().
```

# `profile`

```erlang
-spec profile(Rootset, Module, Function, Args) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Module :: module(),
                     Function :: atom(),
                     Args :: [term()],
                     Value :: term(),
                     Reason :: term();
             (Rootset, Fun, Pattern, Options) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Fun :: fun(() -> term()),
                     Pattern :: trace_pattern_mfa(),
                     Options :: [set_on_spawn | {set_on_spawn, boolean()}],
                     Value :: term(),
                     Reason :: term().
```

This function spawns a process that applies a fun or an an function,
and then starts profiling for the spawned proceses as well as the
processes in `Rootset` (and any new processes spawned from them).

If `Arg1` is a fun, `Arg2` is expected to be a trace pattern, and
`Arg3` a list of options. In that case, this call is equivalent to:

[`profile(Rootset, erlang, apply, [Arg1, []], Arg2, Arg3)`](`profile/6`)

If `Arg1` is an atom, `Arg1` is assumed to be a module name, `Arg2` the
name of the function in that module, and `Arg3` a list of arguments to
be used when calling that function. In that case, this call is equivalent
to:

[`profile(Rootset, Arg1, Arg2, Arg3, {'_','_','_'}, [{set_on_spawn, true}])`](`profile/6`)

# `profile`
*since OTP R14B* 

```erlang
-spec profile(Rootset, Module, Function, Args, Pattern) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Module :: module(),
                     Function :: atom(),
                     Args :: [term()],
                     Pattern :: trace_pattern_mfa(),
                     Value :: term(),
                     Reason :: term().
```

# `profile`
*since OTP R16B01* 

```erlang
-spec profile(Rootset, Module, Function, Args, Pattern, Options) -> {ok, Value} | {error, Reason}
                 when
                     Rootset :: [atom() | pid()],
                     Module :: module(),
                     Function :: atom(),
                     Args :: [term()],
                     Pattern :: trace_pattern_mfa(),
                     Options :: [set_on_spawn | {set_on_spawn, boolean()}],
                     Value :: term(),
                     Reason :: term().
```

This function spawns a process `P` that [`apply(Module, Function,
Args)`](`apply/3`), and then starts profiling for `P` and the
processes in `Rootset` (and any new processes spawned from them).

`Rootset` is a list of pids and registered names.

Information about activity in any profiled process is stored in the Eprof
database.

If tracing could be enabled for `P` and all processes in `Rootset`, the function
returns `{ok,Value}` when `Fun()`/`apply` returns with the value `Value`, or
`{error,Reason}` if `Fun()`/`apply` fails with exit reason `Reason`. Otherwise
it returns `{error, Reason}` immediately.

The `set_on_spawn` option will active call time tracing for all processes
spawned by processes in the rootset. This is the default behaviour.

The programmer must ensure that the function given as argument is truly
synchronous and that no work continues after the function has returned a value.

# `start`

```erlang
-spec start() -> {ok, Pid} | {error, Reason} when Pid :: pid(), Reason :: {already_started, Pid}.
```

Starts the Eprof server which holds the internal state of the collected data.

# `start_profiling`

```erlang
-spec start_profiling(Rootset) -> profiling | {error, Reason}
                         when Rootset :: [atom() | pid()], Reason :: term().
```

# `start_profiling`
*since OTP R14B* 

```erlang
-spec start_profiling(Rootset, Pattern) -> profiling | {error, Reason}
                         when
                             Rootset :: [atom() | pid()],
                             Pattern :: trace_pattern_mfa(),
                             Reason :: term().
```

# `start_profiling`
*since OTP R16B01* 

```erlang
-spec start_profiling(Rootset, Pattern, Options) -> profiling | {error, Reason}
                         when
                             Rootset :: [atom() | pid()],
                             Pattern :: trace_pattern_mfa(),
                             Options :: [set_on_spawn | {set_on_spawn, boolean()}],
                             Reason :: term().
```

Starts profiling for the processes in `Rootset` (and any new processes spawned
from them).

Information about activity in any profiled process is stored in the
Eprof database.

`Rootset` is a list of pids and registered names.

The function returns `profiling` if tracing could be enabled for all processes
in `Rootset`, or `error` otherwise.

A pattern can be selected to narrow the profiling. For instance a specific
module can be selected, and only the code executed in that module will be
profiled.

The `set_on_spawn` option will active call time tracing for all processes
spawned by processes in the rootset. This is the default behaviour.

# `stop`

```erlang
-spec stop() -> stopped.
```

Stops the Eprof server.

# `stop_profiling`

```erlang
-spec stop_profiling() -> profiling_stopped | profiling_already_stopped.
```

Stops profiling started with `start_profiling/1` or `profile/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
