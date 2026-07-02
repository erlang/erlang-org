# `tprof`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/tools/src/tprof.erl#L28)

Process Tracing Profiling Tool

`tprof` provides convenience helpers for Erlang process profiling using
the trace BIFs.

> #### Warning {: .warning }
>
> This module aims to replace `eprof` and `cprof` into a unified API for
> measuring call count, time, and allocation. It is experimental in Erlang/OTP
> 27.0.

It is possible to analyze the number of calls, the time spent by function, and
heap allocations by function. Profiling can be done [ad-hoc](#module-ad-hoc-profiling)
 or run in a [server-aided mode](#module-server-aided-profiling) for deeper
introspection of the code running in production. The server-aided mode can be
run using the default tprof server or an isolated `t:server/0` started through
[`start(#{ session => atom() })`](`start/1`).

There are [three kinds of profiling](`t:trace_type/0`) supported by this module:

- `call_count`
- `call_time`
- `call_memory`

The default is `call_count`, which has the smallest performance impact
and memory footprint, but it does not support per-process
profiling. For this reason, all of the examples below uses
`call_memory`, which measures heap allocation, and provide a more complex
feature set to demonstrate.

Erlang terms that do not fit in a single machine word are allocated on
the process heap. For example, a function returning a tuple of two
elements needs to allocate the tuple on the process heap. The actual
consumption is three words, because the runtime systems also need an
extra word to store the tuple size.

> #### Note {: .info }
>
> Expect a slowdown in the program execution when profiling is enabled.
>
> For profiling convenience, measurements are accumulated for functions that are
> not enabled in some trace pattern. Consider this call stack example:
>
> ```text
> top_traced_function(...)
> not_traced_function()
> bottom_traced_function()
> ```
>
> Allocations that happened within `not_traced_function` will be added to
> the allocations for `top_traced_function`. However, allocations that occurred
> within `bottom_traced_function` are not included in the `top_traced_function`.
> To only keep track of each function own allocations, it is necessary to
> trace all functions.

> #### Warning {: .warning }
>
> Avoid hot code reloading for modules participating in the tracing.
> Reloading a module disables tracing and discards the accumulated statistics.
> The `tprof` results will probably be incorrect when the profiled code was
> reloading during a profiling session.

## Ad-hoc profiling

Ad-hoc profiling is convenient for profiling a single function call.

For example:

```erlang
1> tprof:profile(lists, seq, [1, 16], #{type => call_memory}).

****** Process <0.92.0>  --  100.00% of total *** 
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32      6.40  [100.00]
                            32            [ 100.0]
ok
```

By default tracing is enabled for all functions in all modules. When funs
are created in the interactive shell, parts of shell code are also traced:

```erlang
1> tprof:profile(fun() -> lists:seq(1, 16) end, #{type => call_memory}).

****** Process <0.95.0>  --  100.00% of total *** 
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
erl_eval:do_apply/7            1      3      3.00  [ 3.61]
erl_eval:match_list/6          1      3      3.00  [ 3.61]
lists:reverse/1                1      4      4.00  [ 4.82]
erl_eval:expr_list/7           3      7      2.33  [ 8.43]
erl_eval:ret_expr/3            4     16      4.00  [19.28]
erl_eval:merge_bindings/4      3     18      6.00  [21.69]
lists:seq_loop/3               5     32      6.40  [38.55]
                                     83            [100.0]
ok
```

However, it is possible to limit the trace to specific functions or modules:

```erlang
2> tprof:profile(fun() -> lists:seq(1, 16) end,
                 #{type => call_memory, pattern => [{lists, seq_loop, '_'}]}).
****** Process <0.98.0>  --  100.00% of total *** 
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32      6.40  [100.00]
                            32            [ 100.0]

ok
```

Ad-hoc profiling results can be printed in a few different ways. The following
examples use the `test` module defined like this:

```erlang
-module(test).
-export([test_spawn/0]).
test_spawn() ->
    {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
    receive
        {'DOWN', MRef, process, Pid, normal} ->
            done
    end.
```

By default per-process statistics is shown:

```erlang
1> tprof:profile(test, test_spawn, [], #{type => call_memory}).

****** Process <0.176.0>    -- 23.66 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
erlang:spawn_opt/4          1      6         6  [27.27]
test:test_spawn/0           1     14        14  [63.64]
                                  22            [100.0]

****** Process <0.177.0>    -- 76.34 % of total allocations ***
FUNCTION           CALLS  WORDS  PER CALL  [    %]
erlang:apply/2         1      7         7  [ 9.86]
lists:seq_loop/3       9     64         7  [90.14]
                             71            [100.0]
```

The following example prints the combined memory allocation of all
processes, sorted by the total number of allocated words in descending
order:

```erlang
2> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, report => {total, {measurement, descending}}}).

FUNCTION                CALLS  WORDS  PER CALL  [    %]
lists:seq_loop/3            9     64         7  [68.82]
test:test_spawn/0           1     14        14  [15.05]
erlang:apply/2              1      7         7  [ 7.53]
erlang:spawn_opt/4          1      6         6  [ 6.45]
erlang:spawn_monitor/1      1      2         2  [ 2.15]
                                  93            [100.0]
```

The profiling data can also be collected for further inspection:

```erlang
3> {done, ProfileData} = tprof:profile(fun test:test_spawn/0,
                                       #{type => call_memory, report => return}).
<...>
4> tprof:format(tprof:inspect(ProfileData, process, {percent, descending})).

****** Process <0.223.0>    -- 23.66 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
test:test_spawn/0           1     14        14  [63.64]
erlang:spawn_opt/4          1      6         6  [27.27]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
                                  22            [100.0]

****** Process <0.224.0>    -- 76.34 % of total allocations ***
FUNCTION           CALLS  WORDS  PER CALL  [    %]
lists:seq_loop/3       9     64         7  [90.14]
erlang:apply/2         1      7         7  [ 9.86]
                             71            [100.0]
```

Which processes that are profiled depends on the profiling type.

* `call_count` (default) counts calls in all processes.

* `call_time` and `call_memory` limits the profiling to the processes
  spawned from the user-provided function (using the `set_on_spawn`
  option for `trace:process/4`).

`call_time` and `call_memory` can be restricted to profile a single process:

```erlang
2> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, set_on_spawn => false}).

****** Process <0.183.0>    -- 100.00 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
erlang:spawn_opt/4          1      6         6  [27.27]
test:test_spawn/0           1     14        14  [63.64]
```

[](){: #pg_example }

Erlang programs can perform expensive operations in other processes
than the original one. You can include multiple, new, or even all
processes in the trace when measuring time or memory:

```erlang
7> pg:start_link().
{ok,<0.252.0>}
8> tprof:profile(fun() -> pg:join(group, self()) end,
                 #{type => call_memory, rootset => [pg]}).
****** Process <0.252.0>    -- 52.86 % of total allocations ***
FUNCTION                      CALLS  WORDS  PER CALL  [    %]
pg:leave_local_update_ets/5       1      2         2  [ 1.80]
gen:reply/2                       1      3         3  [ 2.70]
erlang:monitor/2                  1      3         3  [ 2.70]
gen_server:try_handle_call/4      1      3         3  [ 2.70]
gen_server:try_dispatch/4         1      3         3  [ 2.70]
maps:iterator/1                   2      4         2  [ 3.60]
maps:take/2                       1      6         6  [ 5.41]
pg:join_local_update_ets/5        1      8         8  [ 7.21]
pg:handle_info/2                  1      8         8  [ 7.21]
pg:handle_call/3                  1      9         9  [ 8.11]
gen_server:loop/7                 2      9         4  [ 8.11]
ets:lookup/2                      2     10         5  [ 9.01]
pg:join_local/3                   1     11        11  [ 9.91]
pg:notify_group/5                 2     16         8  [14.41]
erlang:setelement/3               2     16         8  [14.41]
                                       111            [100.0]

****** Process <0.255.0>    -- 47.14 % of total allocations ***
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
erl_eval:match_list/6          1      3         3  [ 3.03]
erlang:monitor/2               1      3         3  [ 3.03]
lists:reverse/1                2      4         2  [ 4.04]
pg:join/3                      1      4         4  [ 4.04]
erl_eval:add_bindings/2        1      5         5  [ 5.05]
erl_eval:do_apply/7            2      6         3  [ 6.06]
gen:call/4                     1      8         8  [ 8.08]
erl_eval:expr_list/7           4     10         2  [10.10]
gen:do_call/4                  1     16        16  [16.16]
erl_eval:ret_expr/3            4     16         4  [16.16]
erl_eval:merge_bindings/4      3     24         8  [24.24]
                                     99            [100.0]
```

By default, there is no limit for the profiling time. For ad-hoc
profiling, it is possible to configure a time limit. If the profiled
function does not return before that time expires, the process is
terminated with reason `kill`. Any unlinked children processes started
by the user-supplied function are kept; it is the responsibility of
the developer to take care of such processes.

```erlang
9> tprof:profile(timer, sleep, [100000], #{timeout => 1000}).
```

By default, only one ad-hoc or server-aided profiling session is
allowed at any point in time. It is possible to force multiple ad-hoc
sessions concurrently, but it is the responsibility of the developer
to ensure that trace patterns do not overlap:

```erlang
1> tprof:profile(fun() -> lists:seq(1, 32) end,
    #{registered => false, pattern => [{lists, '_', '_'}]}).
```

## Server-aided profiling

Server-aided profiling can be done on a system that is up and
running. To do that, start the `tprof` server, and then add trace
patterns and processes to trace while the system handles actual
traffic. Data can extracted, inspected, and printed at any time. The
following example traces activity of all processes supervised by
the Kernel supervisor:

```erlang
1> tprof:start(#{type => call_memory}).
{ok,<0.200.0>}
2> tprof:enable_trace({all_children, kernel_sup}).
34
3> tprof:set_pattern('_', '_' , '_').
16728
4> Sample = tprof:collect().
{call_memory,
    [{gen_server,try_dispatch,4,[{<0.154.0>,2,6}]},
     {erlang,iolist_to_iovec,1,[{<0.161.0>,1,8}]},
<...>
5 > tprof:format(tprof:inspect(Sample)).

****** Process <0.154.0>    -- 14.21 % of total allocations ***
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
maps:iterator/1                2      4         2  [15.38]
gen_server:try_dispatch/4      2      6         3  [23.08]
net_kernel:handle_info/2       2     16         8  [61.54]
                                     26            [100.0]

****** Process <0.161.0>    -- 85.79 % of total allocations ***
FUNCTION                        CALLS  WORDS  PER CALL  [    %]
disk_log:handle/2                   2      2         1  [ 1.27]
disk_log_1:maybe_start_timer/1      1      3         3  [ 1.91]
disk_log_1:mf_write_cache/1         1      3         3  [ 1.91]
<...>
```

[](){: #inspect_example }

It is possible to profile the entire running system, and then examine individual
processes:

```erlang
1> tprof:start(#{type => call_memory}).
2> tprof:enable_trace(all), tprof:set_pattern('_', '_' , '_').
9041
3> timer:sleep(10000), tprof:disable_trace(all), Sample = tprof:collect().
{call_memory,
    [{user_drv,server,3,[{<0.64.0>,12,136}]},
     {user_drv,contains_ctrl_g_or_ctrl_c,1,[{<0.64.0>,80,10}]},
<...>
4> Inspected = tprof:inspect(Sample, process, measurement), Shell = maps:get(self(), Inspected).
{call_memory, 2743,
    [{shell,{enc,0},1,2,2,0.07291286912139992},
<...>
5> tprof:format(Shell).

FUNCTION                           CALLS  WORDS  PER CALL  [    %]
<...>
erl_lint:start/2                       2    300       150  [10.94]
shell:used_records/1                 114    342         3  [12.47]
```

# `column`
*not exported* *since OTP 27.0* 

```erlang
-type column() :: module | function | calls | measurement | measurement_per_call | percent.
```

Column to sort by `inspect/3` or [`profile/4`](`profile/4`).

- **`module`** - Module name.

- **`function`** - Function name.

- **`calls`** - Number of calls to the function.

- **`measurement`** - Total measurement (call count, time, or heap allocation)
  throughout all calls to the function.

- **`measurement_per_call`** - Measurement (call count, time, or heap
  allocation) on average per function call.

- **`percent`** - Percentage of measurement to total amount during the entire
  profile collection.

# `process`
*not exported* *since OTP 27.0* 

```erlang
-type process() :: pid() | atom().
```

A process identifier (pid) or a registered process name.

# `profile_line`
*not exported* *since OTP 27.0* 

```erlang
-type profile_line() ::
          {module(),
           Function :: {atom(), arity()},
           Count :: pos_integer(),
           Measurement :: pos_integer(),
           MeasurementPerCall :: non_neg_integer(),
           Percent :: float()}.
```

Inspected data for a single function of the specified `Module`.

# `profile_options`
*not exported* *since OTP 27.0* 

```erlang
-type profile_options() ::
          #{type => trace_type(),
            timeout => timeout(),
            pattern => trace_pattern() | [trace_pattern()],
            set_on_spawn => boolean(),
            rootset => rootset(),
            report => return | process | total | {process, sort_by()} | {total, sort_by()},
            device => io:device()}.
```

Ad-hoc profiler options; see [`profile/4`](`profile/4`).

# `profile_result`
*not exported* *since OTP 27.0* 

```erlang
-type profile_result() :: {trace_type(), TotalMeasurement :: non_neg_integer(), [profile_line()]}.
```

Profile of a single process, or combined profile of multiple processes, sorted
by a selected column.

# `rootset`
*not exported* *since OTP 27.0* 

```erlang
-type rootset() :: [process()] | all | existing | new.
```

# `server`
*not exported* *since OTP 27.0* 

```erlang
-type server() :: pid() | tprof.
```

A tprof server.

Each server uses a separate `t:trace:session/0` in order to
keep profiling isolated.

# `sort_by`
*not exported* *since OTP 27.0* 

```erlang
-type sort_by() :: column() | {column(), ascending} | {column(), descending}.
```

# `start_options`
*not exported* *since OTP 27.0* 

```erlang
-type start_options() :: #{type => trace_type(), session => atom()}.
```

# `trace_info`
*not exported* *since OTP 27.0* 

```erlang
-type trace_info() ::
          {module(),
           Fun :: atom(),
           Arity :: non_neg_integer(),
           [{pid(), Count :: pos_integer(), Measurement :: pos_integer()}]}.
```

Raw data extracted from tracing BIFs.

# `trace_map`
*not exported* *since OTP 27.0* 

```erlang
-type trace_map() :: #{module() => [{Fun :: atom(), arity()}]} | all.
```

Traced functions (with their arities) grouped by module name,
or `all` if all code is traced.

# `trace_options`
*not exported* *since OTP 27.0* 

```erlang
-type trace_options() :: #{set_on_spawn => boolean()}.
```

Options for enabling profiling of the selected processes; see `enable_trace/2`.

# `trace_pattern`
*not exported* *since OTP 27.0* 

```erlang
-type trace_pattern() :: {module(), Fun :: atom(), arity() | '_'}.
```

# `trace_type`
*not exported* *since OTP 27.0* 

```erlang
-type trace_type() :: call_count | call_time | call_memory.
```

The type of profiling that the tprof server will do.

- **call_count** - Counts the number of calls made to functions. This
  is a global profiling event that cannot be limited to specific processes.
  See [call_count](`trace#call_count`) in `trace:function/4` for more details.
- **call_time** - Counts the accumulated time spent in functions.
  See [call_time](`trace#call_time`) in `trace:function/4` for more details.
- **call_memory** - Counts the accumulated memory allocated in functions.
  See [call_memory](`trace#call_memory`) in `trace:function/4` for more details.

# `clear_pattern`
*since OTP 27.0* 

```erlang
-spec clear_pattern(module(), atom(), arity() | '_') -> ok.
```

Disables tracing functions matching the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:clear_pattern(lists, seq, 3).
1
3> tprof:get_trace_map().
#{lists => [{seq,2}]}
```

Requires that the default `tprof` server has been [`started`](`start/1`).

# `clear_pattern`
*since OTP 27.0* 

```erlang
-spec clear_pattern(server(), module(), atom(), arity() | '_') -> ok.
```

Equivalent to [`clear_pattern(Mod, Fun, Arity)`](`clear_pattern/3`) but uses the provided `Server`.

# `collect`
*since OTP 27.0* 

```erlang
-spec collect() -> {trace_type(), [trace_info()]}.
```

Returns statistics for current trace map.

# `collect`
*since OTP 27.0* 

```erlang
-spec collect(server()) -> {trace_type(), [trace_info()]}.
```

Equivalent to `collect/0` but uses the provided `Server`.

# `continue`
*since OTP 27.0* 

```erlang
-spec continue() -> ok | not_paused.
```

Resumes previously paused profiling.

# `continue`
*since OTP 27.0* 

```erlang
-spec continue(server()) -> ok | not_paused.
```

Equivalent to `continue/0` but uses the provided `Server`.

# `disable_trace`
*since OTP 27.0* 

```erlang
-spec disable_trace(Spec) -> non_neg_integer()
                       when Spec :: pid() | all | new | existing | {children | all_children, process()};
                   ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
```

# `disable_trace`
*since OTP 27.0* 

```erlang
-spec disable_trace(Spec, trace_options()) -> non_neg_integer()
                       when Spec :: pid() | all | new | existing | {children | all_children, process()};
                   ([process()], trace_options()) ->
                       non_neg_integer() | {non_neg_integer(), [process()]}.
```

Stops accumulating traces for specified processes.

See `enable_trace/2` for a description of the options.

The profile data accumulated before the process is removed from the
traced list is retained. This makes it possible to enable tracing for
many or all processes in the system, sleep for a short period of
time, then disable tracing for all processes (to avoid system
overload), but keeping profile data.

# `disable_trace`
*since OTP 27.0* 

```erlang
-spec disable_trace(server(), Spec, trace_options()) -> non_neg_integer()
                       when Spec :: pid() | all | new | existing | {children | all_children, process()};
                   (server(), [process()], trace_options()) ->
                       non_neg_integer() | {non_neg_integer(), [process()]}.
```

# `enable_trace`
*since OTP 27.0* 

```erlang
-spec enable_trace(Spec) -> non_neg_integer()
                      when Spec :: pid() | all | new | existing | {children | all_children, process()};
                  ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
```

# `enable_trace`
*since OTP 27.0* 

```erlang
-spec enable_trace(Spec, trace_options()) -> Traced :: non_neg_integer()
                      when Spec :: pid() | all | new | existing;
                  (Spec, trace_options()) ->
                      Traced :: non_neg_integer() | {Traced :: non_neg_integer(), Failed :: [process()]}
                      when Spec :: [process()] | {children | all_children, process()}.
```

Similar to `trace:process/4`, but supports a few more options for tracing
convenience.

Tracing per process is not supported by `call_count` profilers.

`Spec` is either a process identifier (pid) for a local process, one of the
following atoms, or a list of local process identifiers or their registered
names:

- **`all`** - All currently existing processes and all that will be
  created in the future.

- **`existing`** - All currently existing processes.

- **`new`** - All processes that will be created in the future.

- **`children`** - All currently running processes that were directly spawned by
  the specified process. This mode is helpful for tracing workers of a single
  supervisor.

- **`all_children`** - All currently running processes that were spawned by the
  specified process, or any recursive descendant of it. This mode is designed to
  facilitate tracing of supervision trees.

Returns the number of processes for which tracing was enabled.

When a list of pids, `children` or `all_children` is used, the processes that
tracing failed to be enabled on will also be returned. Tracing can fail to be
enabled if the process has terminated before tracing could be enabled.

> #### Note {: .info }
>
> The profiling server does not keep track of processes that were added to the
> tracing set. It is permitted to stop the profiling server (wiping out any
> accumulated data), restart the server, set entirely different tracing pattern
> keeping the list of traced processes for future use. Use
> [`disable_trace(Processes)`](`disable_trace/2`) to clear the list of traced
> processes.

Specify `Options` to modify tracing behavior:

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. On by default.

# `enable_trace`
*since OTP 27.0* 

```erlang
-spec enable_trace(server(), Spec, trace_options()) -> non_neg_integer()
                      when Spec :: pid() | all | new | existing | {children | all_children, process()};
                  (server(), [process()], trace_options()) ->
                      non_neg_integer() | {non_neg_integer(), [process()]}.
```

Equivalent to `enable_trace/2` but uses the provided `Server`.

# `format`
*since OTP 27.0* 

```erlang
-spec format(profile_result() | #{pid() | all => profile_result()}) -> ok.
```

Formats profile data transformed with [`inspect/3`](`inspect/3`), outputting to
the default output device.

# `format`
*since OTP 27.0* 

```erlang
-spec format(io:device(), profile_result() | #{pid() | all => profile_result()}) -> ok.
```

Formats profile transformed with [`inspect/3`](`inspect/3`),
outputting to device `IoDevice`.

# `get_trace_map`
*since OTP 27.0* 

```erlang
-spec get_trace_map() -> trace_map().
```

Returns a map of module names to functions with their arities.

# `get_trace_map`
*since OTP 27.0* 

```erlang
-spec get_trace_map(server()) -> trace_map().
```

Equivalent to `get_trace_map/0` but uses the provided `Server`.

# `inspect`
*since OTP 27.0* 

```erlang
-spec inspect({trace_type(), [trace_info()]}) -> #{all => profile_result()}.
```

Equivalent to [`inspect(Profile, process, percent)`](`inspect/3`).

Transforms raw profile into a map of process identifiers to a tuple containing total count
of words allocated, and a list of all traced functions sorted in the ascending
order by the allocation percentage.

# `inspect`
*since OTP 27.0* 

```erlang
-spec inspect(Profile :: {trace_type(), [trace_info()]}, Type :: process | total, SortBy :: sort_by()) ->
                 #{pid() | all => profile_result()}.
```

Transforms raw data returned by tracing BIFs into a form convenient for
subsequent analysis and formatting.

* When the `Type` argument is `process`, this function returns a map of process
  identifiers with corresponding profiling results sorted by the selected column.

* When `Type` argument is `total` or when profiling by `call_count`, this function
  returns a map with a single `all` key with profiling results from all processes.

The inspected profile data can be leveraged to
[print profiling results](`m:tprof#inspect_example`).

# `pause`
*since OTP 27.0* 

```erlang
-spec pause() -> ok | not_running.
```

Pauses trace collection for all currently traced functions, retaining existing traces.

Use `continue/0` to resume trace collection.

# `pause`
*since OTP 27.0* 

```erlang
-spec pause(server()) -> ok | not_running.
```

Equivalent to `pause/0` but uses the provided `Server`.

# `profile`
*since OTP 27.0* 

```erlang
-spec profile(fun(() -> term())) -> ok | {term(), [trace_info()]}.
```

# `profile`
*since OTP 27.0* 

```erlang
-spec profile(fun(() -> term()), profile_options()) -> ok | {term(), {trace_type(), [trace_info()]}}.
```

Does ad-hoc profiling of the call `Fun()`.

By default, the result is formatted to the output device; use the `report`
option to change this behavior.

Ad-hoc profiling starts a new instance of `tprof` server, runs the
profiling routine, extracts results, and shuts down the server.

See `profile/4` for a list of the supported options.

# `profile`
*since OTP 27.0* 

```erlang
-spec profile(module(), Fun :: atom(), Args :: [term()]) ->
                 ok | {term(), {trace_type(), [trace_info()]}}.
```

# `profile`
*since OTP 27.0* 

```erlang
-spec profile(module(), Fun :: atom(), Args :: [term()], profile_options()) ->
                 ok | {term(), {trace_type(), [trace_info()]}}.
```

Does ad-hoc profiling for the call `apply(Module, Function, Args)`.

By default, the result is formatted to the output device; use option `report`
to change this behavior.

Ad-hoc profiling starts a new instance of `tprof` server, runs the
profiling routine, extracts results, and shuts down the server.

The ad-hoc profiler supports the following `Options`:

- **`type`** - The type of profiling to perform.

- **`device`** - Specifies I/O devices to print the profile to. Useful to
  redirect text output to console or `standard_error`.

- **`pattern`** - Specifies a trace pattern, or a list of trace patterns to
  enable. By default, all functions (`{'_', '_', '_'}`) are traced.

- **`report`** - Controls output format. The default is `process`; printing
  per-process profiling data sorted by percentage of the total allocation.
  Specify `report => return` to suppress printing and get the raw data for
  further evaluation with `inspect/3` and formatting with `format/2`.

- **`rootset`** - Includes extra processes in the trace list. Useful for
  profiling allocations for `m:gen_server`, calls, or other allocations caused
  by inter-process communications. See [this example](`m:tprof#pg_example`).

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. Enabled by default.

- **`timeout`** - Terminate profiling after the specified amount of time
  (milliseconds).

# `restart`
*since OTP 27.0* 

```erlang
-spec restart() -> ok.
```

Clears accumulated profiles and starts profiling if it was paused.

# `restart`
*since OTP 27.0* 

```erlang
-spec restart(server()) -> ok.
```

Equivalent to `restart/0` but uses the provided `Server`.

# `set_pattern`
*since OTP 27.0* 

```erlang
-spec set_pattern(module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
```

Enables tracing for all functions matching the supplied pattern.

Patterns are additive, following the same rules as `trace:function/4`.
Returns the number of functions matching the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:set_pattern(lists, keyfind, 3).
1
3> tprof:get_trace_map().
#{lists => [{keyfind,3},{seq,2},{seq,3}]}
```

If no functions match the pattern, an `error` tuple is returned:

```erlang
> tprof:set_pattern(no_module, func, '_').
{error,{trace_pattern,no_module,func,'_'}}
```

Requires that the default `tprof` server has been [`started`](`start/1`).

# `set_pattern`
*since OTP 27.0* 

```erlang
-spec set_pattern(server(), module(), atom(), arity() | '_') ->
                     ok | {error, {trace_pattern, trace_pattern()}}.
```

Equivalent to `set_pattern/3` but uses the provided `Server`.

# `start`
*since OTP 27.0* 

```erlang
-spec start() -> {ok, Server} | {error, Reason}
               when Server :: server(), Reason :: {already_started, pid()}.
```

# `start`
*since OTP 27.0* 

```erlang
-spec start(Config :: start_options()) -> {ok, Server} | {error, Reason}
               when Server :: server(), Reason :: {already_started, pid()}.
```

Starts the server, not supervised.

Profiling server stores current trace patterns and owns the [trace session](`t:trace:session/0`)
used for profiling.

If no `session` is provided in `Config`, then a default session called `tprof` is
used and the profiling server is [registered](`register/2`) as `tprof`.

If `session` is provided in `Config`, then a session with that name is created
and all profiling is done within that session. The profiling server is not [registered](`register/2`)
in this case. When using `m:tprof` like this the `t:pid/0` returned from this
function needs to be provided to the functions in this module.

# `start_link`
*since OTP 27.0* 

```erlang
-spec start_link() -> {ok, Server} | {error, Reason}
                    when Server :: server(), Reason :: {already_started, pid()}.
```

# `start_link`
*since OTP 27.0* 

```erlang
-spec start_link(Config :: start_options()) -> {ok, Server} | {error, Reason}
                    when Server :: server(), Reason :: {already_started, pid()}.
```

Equivalent to `start/1` but also links the profiling server to the caller.

# `stop`
*since OTP 27.0* 

```erlang
-spec stop() -> ok.
```

Stops the default `tprof` server and disable tracing enabled by the server.

# `stop`
*since OTP 27.0* 

```erlang
-spec stop(server()) -> ok.
```

Equivalent to `stop/0` but uses the provided `Server`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
