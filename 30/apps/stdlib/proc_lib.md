# `proc_lib`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/proc_lib.erl#L22)

Functions for asynchronous and synchronous start of processes adhering to the
OTP design principles.

This module is used to start processes adhering to the
[OTP Design Principles](`e:system:design_principles.md`). Specifically, the
functions in this module are used by the OTP standard behaviors (for example,
`m:gen_server` and `m:gen_statem`) when starting new processes. The functions can
also be used to start _special processes_, user-defined processes that comply to
the OTP design principles. For an example, see section
[sys and proc_lib](`e:system:spec_proc.md`) in OTP Design Principles.

Some useful information is initialized when a process starts. The registered
names, or the process identifiers, of the parent process, and the parent
ancestors, are stored together with information about the function initially
called in the process.

While in "plain Erlang", a process is said to terminate normally only for exit
reason `normal`, a process started using `m:proc_lib` is also said to terminate
normally if it exits with reason `shutdown` or `{shutdown,Term}`. `shutdown` is
the reason used when an application (supervision tree) is stopped.

When a process that is started using `m:proc_lib` terminates abnormally (that is,
with another exit reason than `normal`, `shutdown`, or `{shutdown,Term}`), a
_crash report_ is generated, which is written to terminal by the default logger
handler setup by Kernel. For more information about how crash reports were
logged prior to Erlang/OTP 21.0, see
[SASL Error Logging](`e:sasl:error_logging.md`) in the SASL User's Guide.

Unlike in "plain Erlang", `m:proc_lib` processes will not generate _error
reports_, which are written to the terminal by the emulator. All exceptions are
converted to _exits_ which are ignored by the default `logger` handler.

The crash report contains the previously stored information, such as ancestors
and initial function, the termination reason, and information about other
processes that terminate as a result of this process terminating.

### See Also

`m:logger`

# `dict_or_pid`
*not exported* 

```erlang
-type dict_or_pid() :: pid() | (ProcInfo :: [_]) | {X :: integer(), Y :: integer(), Z :: integer()}.
```

# `exception`
*not exported* 

```erlang
-type exception() ::
          {Class :: error | exit | throw, Reason :: term()} |
          {Class :: error | exit | throw, Reason :: term(), Stacktrace :: erlang:raise_stacktrace()}.
```

An exception passed to `init_fail/3`. See `erlang:raise/3` for a description
of `Class`, `Reason` and `Stacktrace`.

# `spawn_option`

```erlang
-type spawn_option() :: erlang:spawn_opt_option().
```

Equivalent to `t:erlang:spawn_opt_option/0`.

# `start_spawn_option`

```erlang
-type start_spawn_option() ::
          link |
          {priority, erlang:priority_level()} |
          {fullsweep_after, non_neg_integer()} |
          {min_heap_size, non_neg_integer()} |
          {min_bin_vheap_size, non_neg_integer()} |
          {max_heap_size, erlang:max_heap_size()} |
          {message_queue_data, erlang:message_queue_data()}.
```

A restricted set of [spawn options](`t:spawn_option/0`). Most notably `monitor`
is _not_ part of these options.

# `format`

```erlang
-spec format(CrashReport) -> string() when CrashReport :: [term()].
```

Equivalent to [`format(CrashReport, latin1)`](`format/2`).

# `format`
*since OTP R16B* 

```erlang
-spec format(CrashReport, Encoding) -> string()
                when CrashReport :: [term()], Encoding :: latin1 | unicode | utf8.
```

> #### Note {: .info }
>
> This function is deprecated in the sense that the `error_logger` is no longer
> the preferred interface for logging in Erlang/OTP. A new
> [logging API](`e:kernel:logger_chapter.md`) was added in Erlang/OTP 21.0, but
> legacy `error_logger` handlers can still be used. New Logger handlers do not
> need to use this function, since the formatting callback (`report_cb`) is
> included as metadata in the log event.

This function can be used by a user-defined legacy `error_logger` event handler
to format a crash report. The crash report is sent using `m:logger`, and the
event to be handled is of the format
`{error_report, GL, {Pid, crash_report, CrashReport}}`, where `GL` is the group
leader pid of process `Pid` that sent the crash report.

# `format`
*since OTP 18.1* 

```erlang
-spec format(CrashReport, Encoding, Depth) -> string()
                when
                    CrashReport :: [term()],
                    Encoding :: latin1 | unicode | utf8,
                    Depth :: unlimited | pos_integer().
```

> #### Note {: .info }
>
> This function is deprecated in the sense that the `error_logger` is no longer
> the preferred interface for logging in Erlang/OTP. A new
> [logging API](`e:kernel:logger_chapter.md`) was added in Erlang/OTP 21.0, but
> legacy `error_logger` handlers can still be used. New Logger handlers do not
> need to used this function, since the formatting callback (`report_cb`) is
> included as metadata in the log event.

This function can be used by a user-defined legacy `error_logger` event handler
to format a crash report. When Depth is specified as a positive integer, it is
used in the format string to limit the output as follows:
`io_lib:format("~P", [Term,Depth])`.

# `get_label`
*since OTP 27.0* 

```erlang
-spec get_label(Pid) -> undefined | term() when Pid :: pid().
```

Returns either `undefined` or the label for the process Pid set with
[`proc_lib:set_label/1`](`set_label/1`).

# `hibernate`

```erlang
-spec hibernate(Module, Function, Args) -> no_return()
                   when Module :: module(), Function :: atom(), Args :: [term()].
```

This function does the same as (and does call) the
[`hibernate/3`](`erlang:hibernate/3`) BIF, but ensures that exception handling
and logging continues to work as expected when the process wakes up.

Always use this function instead of the BIF for processes started using
`proc_lib` functions.

# `init_ack`

```erlang
-spec init_ack(Ret) -> ok when Ret :: term().
```

Equivalent to [`init_ack(Parent, Ret)`](`init_ack/2`) where `Parent` is
the process that called `start/5`.

# `init_ack`

```erlang
-spec init_ack(Parent, Ret) -> ok when Parent :: pid(), Ret :: term().
```

This function must only be used by a process that has been started by a
[`start[_link|_monitor]/3,4,5`](`start/5`) function. It tells `Parent` that the
process has initialized itself and started.

Function [`init_ack/1`](`init_ack/1`) uses the parent value previously stored by
the start function used.

If neither this function nor [`init_fail/2,3`](`init_fail/3`) is called by the
started process, the start function returns an error tuple when the started
process exits, or when the start function time-out (if used) has passed, see
[`start/3,4,5`](`start/5`).

> #### Warning {: .warning }
>
> Do not use this function to return an error indicating that the process start
> failed. When doing so the start function can return before the failing process
> has exited, which may block VM resources required for a new start attempt to
> succeed. Use [`init_fail/2,3`](`init_fail/3`) for that purpose.

The following example illustrates how this function and `proc_lib:start_link/3`
are used:

```erlang
-module(my_proc).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    proc_lib:start_link(my_proc, init, [self()]).

init(Parent) ->
    case do_initialization() of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()});
        {error, Reason} ->
            exit(Reason)
    end,
    loop().

...
```

# `init_fail`
*since OTP 26.0* 

```erlang
-spec init_fail(Return :: term(), Exception :: exception()) -> no_return().
```

Equivalent to [`init_fail(Parent, Return, Exception)`](`init_fail/3`) where
`Parent` is the process that called `start/5`.

# `init_fail`
*since OTP 26.0* 

```erlang
-spec init_fail(Parent :: pid(), Return :: term(), Exception :: exception()) -> no_return().
```

This function must only be used by a process that has been started by a
[`start[_link|_monitor]/3,4,5`](`start/3`) function. It tells `Parent` that the
process has failed to initialize, and immediately raises an exception according
to `Exception`. The start function then returns `Ret`.

See `erlang:raise/3` for a description of `Class`, `Reason` and `Stacktrace`.

> #### Warning {: .warning }
>
> Do not consider catching the exception from this function. That would defeat
> its purpose. A process started by a [`start[_link|_monitor]/3,4,5`](`start/3`)
> function should end in a value (that will be ignored) or an exception that
> will be handled by this module. See [Description](`m:proc_lib`).

If neither this function nor [`init_ack/1,2`](`init_ack/1`) is called by the
started process, the start function returns an error tuple when the started
process exits, or when the start function time-out (if used) has passed, see
[`start/3,4,5`](`start/3`).

The following example illustrates how this function and `proc_lib:start_link/3`
can be used:

```erlang
-module(my_proc).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    proc_lib:start_link(my_proc, init, [self()]).

init(Parent) ->
    case do_initialization() of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()});
        {error, Reason} = Error ->
            proc_lib:init_fail(Parent, Error, {exit, normal})
    end,
    loop().

...
```

# `initial_call`

```erlang
-spec initial_call(Process) -> {Module, Function, Args} | false
                      when
                          Process :: dict_or_pid(),
                          Module :: module(),
                          Function :: atom(),
                          Args :: [atom()].
```

Extracts the initial call of a process that was started using one of the spawn
or start functions in this module. `Process` can either be a pid, an integer
tuple (from which a pid can be created), or the process information of a process
`Pid` fetched through an `erlang:process_info(Pid)` function call.

> #### Note {: .info }
>
> The list `Args` no longer contains the arguments, but the same number of atoms
> as the number of arguments; the first atom is `'Argument__1'`, the second
> `'Argument__2'`, and so on. The reason is that the argument list could waste a
> significant amount of memory, and if the argument list contained funs, it
> could be impossible to upgrade the code for the module.
>
> If the process was spawned using a fun, [`initial_call/1`](`initial_call/1`)
> no longer returns the fun, but the module, function for the local function
> implementing the fun, and the arity, for example,
> `{some_module,-work/3-fun-0-,0}` (meaning that the fun was created in function
> `some_module:work/3`). The reason is that keeping the fun would prevent code
> upgrade for the module, and that a significant amount of memory could be
> wasted.

# `set_label`
*since OTP 27.0* 

```erlang
-spec set_label(Label) -> ok when Label :: term().
```

Set a label for the current process. The primary purpose is to aid in debugging
unregistered processes. The process label can be used in tools and crash reports
to identify processes but it doesn't have to be unique or an atom, as a
registered name needs to be. The process label can be any term, for example
`{worker_process, 1..N}`.

Use [`proc_lib:get_label/1`](`get_label/1`) to lookup the process description.

# `spawn`

```erlang
-spec spawn(Fun) -> pid() when Fun :: function().
```

# `spawn`

```erlang
-spec spawn(Node, Fun) -> pid() when Node :: node(), Fun :: function().
```

# `spawn`

```erlang
-spec spawn(Module, Function, Args) -> pid()
               when Module :: module(), Function :: atom(), Args :: [term()].
```

# `spawn`

```erlang
-spec spawn(Node, Module, Function, Args) -> pid()
               when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the [`spawn`](`erlang:spawn/1`) BIFs.

# `spawn_link`

```erlang
-spec spawn_link(Fun) -> pid() when Fun :: function().
```

# `spawn_link`

```erlang
-spec spawn_link(Node, Fun) -> pid() when Node :: node(), Fun :: function().
```

# `spawn_link`

```erlang
-spec spawn_link(Module, Function, Args) -> pid()
                    when Module :: module(), Function :: atom(), Args :: [term()].
```

# `spawn_link`

```erlang
-spec spawn_link(Node, Module, Function, Args) -> pid()
                    when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the
[`spawn_link`](`erlang:spawn_link/1`) BIFs.

# `spawn_opt`

```erlang
-spec spawn_opt(Fun, SpawnOpts) -> pid() | {pid(), reference()}
                   when Fun :: function(), SpawnOpts :: [erlang:spawn_opt_option()].
```

# `spawn_opt`

```erlang
-spec spawn_opt(Node, Fun, SpawnOpts) -> pid() | {pid(), reference()}
                   when Node :: node(), Fun :: function(), SpawnOpts :: [erlang:spawn_opt_option()].
```

# `spawn_opt`

```erlang
-spec spawn_opt(Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()}
                   when
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       SpawnOpts :: [erlang:spawn_opt_option()].
```

# `spawn_opt`

```erlang
-spec spawn_opt(Node, Module, Function, Args, SpawnOpts) -> pid() | {pid(), reference()}
                   when
                       Node :: node(),
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       SpawnOpts :: [erlang:spawn_opt_option()].
```

Spawns a new process and initializes it as described in the beginning of this
manual page. The process is spawned using the
[`erlang:spawn_opt`](`erlang:spawn_opt/2`) BIFs.

# `start`

```erlang
-spec start(Module, Function, Args) -> Ret
               when
                   Module :: module(),
                   Function :: atom(),
                   Args :: [term()],
                   Ret :: term() | {error, Reason :: term()}.
```

# `start`

```erlang
-spec start(Module, Function, Args, Time) -> Ret
               when
                   Module :: module(),
                   Function :: atom(),
                   Args :: [term()],
                   Time :: timeout(),
                   Ret :: term() | {error, Reason :: term()}.
```

# `start`

```erlang
-spec start(Module, Function, Args, Time, SpawnOpts) -> Ret
               when
                   Module :: module(),
                   Function :: atom(),
                   Args :: [term()],
                   Time :: timeout(),
                   SpawnOpts :: [start_spawn_option()],
                   Ret :: term() | {error, Reason :: term()}.
```

Starts a new process synchronously. Spawns the process and waits for it to
start.

To indicate a succesful start, the started process _must_ call
[`init_ack(Parent, Ret)`](`init_ack/2`) where `Parent` is the process that
evaluates this function, or [`init_ack(Ret)`](`init_ack/1`). `Ret` is then
returned by this function.

If the process fails to start, it _must_ fail; preferably by calling
[`init_fail(Parent, Ret, Exception)` ](`init_fail/3`) where `Parent` is the
process that evaluates this function, or
[`init_fail(Ret, Exception)`](`init_fail/2`). `Ret` is then returned by this
function, and the started process fails with `Exception`.

If the process instead fails before calling `init_ack/1,2` or `init_fail/2,3`,
this function returns `{error, Reason}` where `Reason` depends a bit on the
exception just like for a process link `{'EXIT',Pid,Reason}` message.

If `Time` is specified as an integer, this function waits for `Time`
milliseconds for the new process to call `init_ack/1,2` or `init_fail/2,3`,
otherwise the process gets killed and `Ret = {error, timeout}` is returned.

Argument `SpawnOpts`, if specified, is passed as the last argument to the
[`spawn_opt/4`](`erlang:spawn_opt/4`) BIF.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.
>
> Using spawn option `link` will set a link to the spawned process, just like
> [start_link/3,4,5](`start_link/3`).

# `start_link`

```erlang
-spec start_link(Module, Function, Args) -> Ret
                    when
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Ret :: term() | {error, Reason :: term()}.
```

# `start_link`

```erlang
-spec start_link(Module, Function, Args, Time) -> Ret
                    when
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Time :: timeout(),
                        Ret :: term() | {error, Reason :: term()}.
```

# `start_link`

```erlang
-spec start_link(Module, Function, Args, Time, SpawnOpts) -> Ret
                    when
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Time :: timeout(),
                        SpawnOpts :: [start_spawn_option()],
                        Ret :: term() | {error, Reason :: term()}.
```

Starts a new process synchronously. Spawns the process and waits for it to
start. A link is atomically set on the newly spawned process.

> #### Note {: .info }
>
> If the started process gets killed or crashes with a reason that is not
> `normal`, the process link will kill the calling process so this function does
> not return, unless the calling process traps exits. For example, if this
> function times out it will kill the spawned process, and then the link might
> kill the calling process.

Besides setting a link on the spawned process this function behaves like
[start/5](`start/5`).

When the calling process traps exits; if this function returns due to the
spawned process exiting (any error return), this function receives (consumes)
the `'EXIT'` message, also when this function times out and kills the spawned
process.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.

# `start_monitor`
*since OTP 23.0* 

```erlang
-spec start_monitor(Module, Function, Args) -> {Ret, Mon}
                       when
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           Mon :: reference(),
                           Ret :: term() | {error, Reason :: term()}.
```

# `start_monitor`
*since OTP 23.0* 

```erlang
-spec start_monitor(Module, Function, Args, Time) -> {Ret, Mon}
                       when
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           Time :: timeout(),
                           Mon :: reference(),
                           Ret :: term() | {error, Reason :: term()}.
```

# `start_monitor`
*since OTP 23.0* 

```erlang
-spec start_monitor(Module, Function, Args, Time, SpawnOpts) -> {Ret, Mon}
                       when
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           Time :: timeout(),
                           SpawnOpts :: [start_spawn_option()],
                           Mon :: reference(),
                           Ret :: term() | {error, Reason :: term()}.
```

Starts a new process synchronously. Spawns the process and waits for it to
start. A monitor is atomically set on the newly spawned process.

Besides setting a monitor on the spawned process this function behaves like
[start/5](`start/5`).

The return value is `{Ret, Mon}` where `Ret` corresponds to the `Ret` argument
in the call to `init_ack/1,2` or `init_fail/2,3`, and `Mon` is the monitor
reference of the monitor that has been set up.

If this function returns due to the spawned process exiting, that is returns any
error value, a `'DOWN'` message will be delivered to the calling process, also
when this function times out and kills the spawned process.

> #### Note {: .info }
>
> Using spawn option `monitor` is not allowed. It causes the function to fail
> with reason `badarg`.
>
> Using spawn option `link` will set a link to the spawned process, just like
> [start_link/3,4,5](`start_link/3`).

# `stop`
*since OTP 18.0* 

```erlang
-spec stop(Process) -> ok when Process :: pid() | RegName | {RegName, node()}, RegName :: atom().
```

Equivalent to [`stop(Process, normal, infinity)`](`stop/3`).

# `stop`
*since OTP 18.0* 

```erlang
-spec stop(Process, Reason, Timeout) -> ok
              when
                  Process :: pid() | RegName | {RegName, node()},
                  RegName :: atom(),
                  Reason :: term(),
                  Timeout :: timeout().
```

Orders the process to exit with the specified `Reason` and waits for it to
terminate.

Returns `ok` if the process exits with the specified `Reason` within `Timeout`
milliseconds.

If the call times out, a `timeout` exception is raised.

If the process does not exist, a `noproc` exception is raised.

The implementation of this function is based on the `terminate` system message,
and requires that the process handles system messages correctly. For information
about system messages, see `m:sys` and section
[sys and proc_lib](`e:system:spec_proc.md`) in OTP Design Principles.

# `translate_initial_call`

```erlang
-spec translate_initial_call(Process) -> {Module, Function, Arity}
                                when
                                    Process :: dict_or_pid(),
                                    Module :: module(),
                                    Function :: atom(),
                                    Arity :: byte().
```

This function is used by functions `\c:i/0` and `\c:regs/0` to present process
information.

This function extracts the initial call of a process that was started using one
of the spawn or start functions in this module, and translates it to more useful
information. `Process` can either be a pid, an integer tuple (from which a pid
can be created), or the process information of a process `Pid` fetched through
an `erlang:process_info(Pid)` function call.

If the initial call is to one of the system-defined behaviors such as
`gen_server` or `gen_event`, it is translated to more useful information. If a
`gen_server` is spawned, the returned `Module` is the name of the callback
module and `Function` is `init` (the function that initiates the new server).

A `supervisor` and a `supervisor_bridge` are also `gen_server` processes. To
return information that this process is a supervisor and the name of the
callback module, `Module` is `supervisor` and `Function` is the name of the
supervisor callback module. `Arity` is `1`, as the `init/1` function is called
initially in the callback module.

By default, `{proc_lib,init_p,5}` is returned if no information about the
initial call can be found. It is assumed that the caller knows that the process
has been spawned with the `proc_lib` module.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
