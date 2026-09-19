# `fprof`
[🔗](https://github.com/erlang/otp/blob/master/lib/tools/src/fprof.erl#L30)

A Time Profiling Tool using trace to file for minimal runtime performance
impact.

This module is used to profile a program to find out how the execution time is
used. Tracing to file is used to minimize the runtime performance degradation.

The `fprof` module uses tracing to collect profiling data, hence there is no
need for special compilation of any module to be profiled. When it starts
tracing, `fprof` will erase all previous tracing in the node and set the
necessary trace flags on the profiling target processes as well as local call
trace on all functions in all loaded modules and all modules to be loaded.
`fprof` disable all tracing in the node when it stops tracing.

`fprof` presents both _own time_ that is, how much time a function has
used for its own execution, and _accumulated time_ that is, including
called functions. All presented times are collected using trace
timestamps. `fprof` tries to collect CPU time timestamps, if the host
machine OS supports it. Therefore, the times can be wallclock times and
OS scheduling will randomly strike all called functions in a
presumably fair way.

However, if the profiling time is short, and the host machine OS does
not support high resolution CPU time measurements, a few OS
schedulings can show up as ridiculously long execution times for
functions doing practically nothing. As an example, it has been
observed that a function that more or less just composing a tuple, was
running 100 times slower than normal. When tracing was repeated, the
execution time was normal.

Profiling is essentially done in 3 steps:

- Tracing: to a file. The trace data contains entries for function
  calls, returns to function, process scheduling, other process
  related events (for example `spawn`), and garbage collection. All trace
  entries are timestamped.

- Profiling: the trace file is read, the execution call stack is
  simulated, and raw profile data is calculated from the simulated call stack
  and the trace timestamps. The profile data is stored in the `fprof` server
  state. During this step the trace data may be dumped in text format to file or
  console.

- Analysing: the raw profile data is sorted, filtered and dumped in
  text format either to file or console. The text format intended to be both
  readable for a human reader, as well as parsable with the standard erlang
  parsing tools.

Since `fprof` sends trace data to afile, the runtime performance
degradation is minimized, but still far from negligible, especially
for programs that themselves use the filesystem heavily. Where the
trace file is placed is also important, for example, on Unix systems
`/tmp` is usually a good choice since it is essentially a RAM disk,
while any network-mounted disk is a bad idea.

`fprof` can also skip the file step and trace to a tracer process that does the
profiling in runtime.

[](){: #analysis }

## Analysis format

This section describes the output format of the `analyse/1` function.

The format is parsable with the standard Erlang parsing tools
`m:erl_scan` and `m:erl_parse`, `file:consult/1`, or `io:read/2`. The
parse format is not described here — it should be easy enough for the
interested reader to try it out. Note that some flags to
[`analyse/1`](`analyse/1`) will affect the format.

The following example was run on Erlang/OTP R8 on Solaris 8; all OTP
internals in this example are version dependent.

As an example, we will use the following function, which is a
slightly modified benchmark function from module `m:file`:

```erlang
-module(foo).
-export([create_file_slow/2]).

create_file_slow(Name, N) when is_integer(N), N >= 0 ->
    {ok, FD} =
        file:open(Name, [raw, write, delayed_write, binary]),
    if N > 256 ->
            ok = file:write(FD,
                            lists:map(fun (X) -> <<X:32/unsigned>> end,
                            lists:seq(0, 255))),
            ok = create_file_slow(FD, 256, N);
       true ->
            ok = create_file_slow(FD, 0, N)
    end,
    ok = file:close(FD).

create_file_slow(FD, M, M) ->
    ok;
create_file_slow(FD, M, N) ->
    ok = file:write(FD, <<M:32/unsigned>>),
    create_file_slow(FD, M+1, N).
```

Let us have a look at the printout after running:

```erlang
1> fprof:apply(foo, create_file_slow, [junk, 1024]).
2> fprof:profile().
3> fprof:analyse().
```

The printout starts with:

```erlang
%% Analysis results:
{  analysis_options,
 [{callers, true},
  {sort, acc},
  {totals, false},
  {details, true}]}.

%                                       CNT       ACC       OWN
[{ totals,                             9627, 1691.119, 1659.074}].  %%%
```

The `CNT` column shows the total number of function calls that was found in the
trace. In the `ACC` column is the total time of the trace from first timestamp to
last. And in the `OWN` column is the sum of the execution time in functions found
in the trace, not including called functions. In this case it is very close to
the `ACC` time since the emulator had practically nothing to do except
executing our test program.

All time values in the printout are in milliseconds.

The printout continues:

```erlang
%                                       CNT       ACC       OWN
[{ "<0.28.0>",                         9627,undefined, 1659.074}].   %%
```

This is the printout header of one process. The printout contains only this one
process since we called `fprof:apply/3` that traces only the current process.
Therefore the `CNT` and `OWN` columns perfectly matches the totals above. The `ACC`
column is undefined since summing the `ACC` times of all calls in the process
makes no sense — one would get something like the `ACC` value from totals above
multiplied by the average depth of the call stack.

All paragraphs up to the next process header only concerns function calls within
this process.

Now we come to something more interesting:

```erlang
{[{undefined,                             0, 1691.076,    0.030}],
 { {fprof,apply_start_stop,4},            0, 1691.076,    0.030},     %
 [{{foo,create_file_slow,2},              1, 1691.046,    0.103},
  {suspend,                               1,    0.000,    0.000}]}.

{[{{fprof,apply_start_stop,4},            1, 1691.046,    0.103}],
 { {foo,create_file_slow,2},              1, 1691.046,    0.103},     %
 [{{file,close,1},                        1, 1398.873,    0.019},
  {{foo,create_file_slow,3},              1,  249.678,    0.029},
  {{file,open,2},                         1,   20.778,    0.055},
  {{lists,map,2},                         1,   16.590,    0.043},
  {{lists,seq,2},                         1,    4.708,    0.017},
  {{file,write,2},                        1,    0.316,    0.021}]}.
```

The printout consists of one paragraph per called function. The function
_marked_ with `%` is the one the paragraph concerns — `foo:create_file_slow/2`.
Above the marked function are the _calling_ functions — those that has called
the marked, and below are those _called_ by the marked function.

The paragraphs are per default sorted in descending order of the `ACC` column for
the marked function. The calling list and called list within one paragraph are
also per default sorted in descending order of their `ACC` column.

The columns are:

* `CNT` - the number of times the function has been called
* `ACC` - the time spent in the function including called functions
* `OWN` - the time spent in the function not including called functions

The rows for the _calling_ functions contain statistics for the _marked_
function with the constraint that only the occasions when a call was made from
the _row's_ function to the _marked_ function are accounted for.

The row for the _marked_ function simply contains the sum of all _calling_ rows.

The rows for the _called_ functions contains statistics for the _row's_ function
with the constraint that only the occasions when a call was made from the
_marked_ to the _row's_ function are accounted for.

So, we see that `foo:create_file_slow/2` used very little time for its own
execution. It spent most of its time in `file:close/1`. The function
`foo:create_file_slow/3` that writes 3/4 of the file contents is the second
biggest time thief.

We also see that the call to `file:write/2` that writes 1/4 of the file contents
takes very little time in itself. What takes time is to build the data
(`lists:seq/2` and `lists:map/2`).

The function `undefined` that has called `fprof:apply_start_stop/4` is an
unknown function because that call was not recorded in the trace. It was only
recorded that the execution returned from `fprof:apply_start_stop/4` to some
other function above in the call stack, or that the process exited from there.

Let us continue down the printout to find:

```erlang
{[{{foo,create_file_slow,2},              1,  249.678,    0.029},
  {{foo,create_file_slow,3},            768,    0.000,   23.294}],
 { {foo,create_file_slow,3},            769,  249.678,   23.323},     %
 [{{file,write,2},                      768,  220.314,   14.539},
  {suspend,                              57,    6.041,    0.000},
  {{foo,create_file_slow,3},            768,    0.000,   23.294}]}.
```

If you compare with the code you will see there also that
`foo:create_file_slow/3` was called only from `foo:create_file_slow/2` and
itself, and called only `file:write/2`, note the number of calls to
`file:write/2`. But here we see that `suspend` was called a few times. This is a
pseudo function that indicates that the process was suspended while executing in
`foo:create_file_slow/3`, and since there is no `receive` or `erlang:yield/0` in
the code, it must be Erlang scheduling suspensions, or the trace file driver
compensating for large file write operations (these are regarded as a schedule
out followed by a schedule in to the same process).

Let us find the `suspend` entry:

```erlang
{[{{file,write,2},                       53,    6.281,    0.000},
  {{foo,create_file_slow,3},             57,    6.041,    0.000},
  {{prim_file,drv_command,4},            50,    4.582,    0.000},
  {{prim_file,drv_get_response,1},       34,    2.986,    0.000},
  {{lists,map,2},                        10,    2.104,    0.000},
  {{prim_file,write,2},                  17,    1.852,    0.000},
  {{erlang,port_command,2},              15,    1.713,    0.000},
  {{prim_file,drv_command,2},            22,    1.482,    0.000},
  {{prim_file,translate_response,2},     11,    1.441,    0.000},
  {{prim_file,'-drv_command/2-fun-0-',1},  15,    1.340,    0.000},
  {{lists,seq,4},                         3,    0.880,    0.000},
  {{foo,'-create_file_slow/2-fun-0-',1},   5,    0.523,    0.000},
  {{erlang,bump_reductions,1},            4,    0.503,    0.000},
  {{prim_file,open_int_setopts,3},        1,    0.165,    0.000},
  {{prim_file,i32,4},                     1,    0.109,    0.000},
  {{fprof,apply_start_stop,4},            1,    0.000,    0.000}],
 { suspend,                             299,   32.002,    0.000},     %
 [ ]}.
```

We find no particularly long suspend times, so no function seems to have waited
in a receive statement. Actually, `prim_file:drv_command/4` contains a receive
statement, but in this test program, the message lies in the process receive
buffer when the receive statement is entered. We also see that the total suspend
time for the test run is small.

The `suspend` pseudo function has an `OWN` time of zero. This is to prevent
the process total `OWN` time from including time in suspension. Whether suspend
time is really `ACC` or `OWN` time is more of a philosophical question.

Now we look at another interesting pseudo function, `garbage_collect`:

```erlang
{[{{prim_file,drv_command,4},            25,    0.873,    0.873},
  {{prim_file,write,2},                  16,    0.692,    0.692},
  {{lists,map,2},                         2,    0.195,    0.195}],
 { garbage_collect,                      43,    1.760,    1.760},     %
 [ ]}.
```

Here we see that no function stands out, which is very normal.

The `garbage_collect` pseudo function has not an `OWN` time of zero like
`suspend`, instead it is equal to the `ACC` time.

Garbage collection often occurs while a process is suspended, but `fprof` hides
this fact by pretending that the suspended function was first unsuspended and
then garbage collected. Otherwise the printout would show `garbage_collect`
being called from `suspend`, but not which function that might have caused the
garbage collection.

Let us now get back to the test code:

```erlang
{[{{foo,create_file_slow,3},            768,  220.314,   14.539},
  {{foo,create_file_slow,2},              1,    0.316,    0.021}],
 { {file,write,2},                      769,  220.630,   14.560},     %
 [{{prim_file,write,2},                 769,  199.789,   22.573},
  {suspend,                              53,    6.281,    0.000}]}.
```

Not unexpectedly, we see that `file:write/2` was called from
`foo:create_file_slow/3` and `foo:create_file_slow/2`. The number of calls in
each case as well as the used time are also confirms the previous results.

We see that `file:write/2` only calls `prim_file:write/2`, but let us refrain
from digging into the internals of the kernel application.

If we nevertheless _do_ dig down we find the call to the linked-in driver
that does the file operations towards the host operating system:

```erlang
{[{{prim_file,drv_command,4},           772, 1458.356, 1456.643}],
 { {erlang,port_command,2},             772, 1458.356, 1456.643},     %
 [{suspend,                              15,    1.713,    0.000}]}.
```

This is 86 % of the total run time, and as we saw before it is the close
operation the absolutely biggest contributor. We find a comparison ratio a
little bit up in the call stack:

```erlang
{[{{prim_file,close,1},                   1, 1398.748,    0.024},
  {{prim_file,write,2},                 769,  174.672,   12.810},
  {{prim_file,open_int,4},                1,   19.755,    0.017},
  {{prim_file,open_int_setopts,3},        1,    0.147,    0.016}],
 { {prim_file,drv_command,2},           772, 1593.322,   12.867},     %
 [{{prim_file,drv_command,4},           772, 1578.973,   27.265},
  {suspend,                              22,    1.482,    0.000}]}.
```

The time for file operations in the linked in driver distributes itself as 1 %
for open, 11 % for write, and 87 % for close. All data is probably buffered in
the operating system until the close.

The observant reader may notice that the ACC times for
`prim_file:drv_command/2` and `prim_file:drv_command/4` is not equal between the
paragraphs above, even though it is easy to believe that
`prim_file:drv_command/2` is just a passthrough function.

The missing time can be found in the paragraph for `prim_file:drv_command/4`
where it is evident that not only `prim_file:drv_command/2` is called but also a
fun:

```erlang
{[{{prim_file,drv_command,2},           772, 1578.973,   27.265}],
 { {prim_file,drv_command,4},           772, 1578.973,   27.265},     %
 [{{erlang,port_command,2},             772, 1458.356, 1456.643},
  {{prim_file,'-drv_command/2-fun-0-',1}, 772,   87.897,   12.736},
  {suspend,                              50,    4.582,    0.000},
  {garbage_collect,                      25,    0.873,    0.873}]}.
```

And some more missing time can be explained by the fact that
`prim_file:open_int/4` both calls `prim_file:drv_command/2` directly as well as
through `prim_file:open_int_setopts/3`, which complicates the picture.

```erlang
{[{{prim_file,open,2},                    1,   20.309,    0.029},
  {{prim_file,open_int,4},                1,    0.000,    0.057}],
 { {prim_file,open_int,4},                2,   20.309,    0.086},     %
 [{{prim_file,drv_command,2},             1,   19.755,    0.017},
  {{prim_file,open_int_setopts,3},        1,    0.360,    0.032},
  {{prim_file,drv_open,2},                1,    0.071,    0.030},
  {{erlang,list_to_binary,1},             1,    0.020,    0.020},
  {{prim_file,i32,1},                     1,    0.017,    0.017},
  {{prim_file,open_int,4},                1,    0.000,    0.057}]}.
.
.
.
{[{{prim_file,open_int,4},                1,    0.360,    0.032},
  {{prim_file,open_int_setopts,3},        1,    0.000,    0.016}],
 { {prim_file,open_int_setopts,3},        2,    0.360,    0.048},     %
 [{suspend,                               1,    0.165,    0.000},
  {{prim_file,drv_command,2},             1,    0.147,    0.016},
  {{prim_file,open_int_setopts,3},        1,    0.000,    0.016}]}.
```

## Notes on accuracy of measurements

The actual supervision of execution times is in itself a CPU-intensive activity.
A message is written on the trace file for every function call that is made by
the profiled code.

The `ACC` time calculation is sometimes difficult to make correct, since it is
difficult to define. This happens especially when a function occurs in several
instances in the call stack, for example by calling itself perhaps through other
functions and perhaps even non-tail recursively.

To produce sensible results, `fprof` tries not to charge any function more than
once for `ACC` time. The instance highest up (with longest duration) in the call
stack is chosen.

Sometimes a function can unexpectedly waste a lot (some 10 ms or more depending
on host machine OS) of `OWN` (and `ACC`) time, even functions that do practically
nothing at all. The problem may be that the OS has chosen to schedule out the
Erlang runtime system process for a while, and if the OS does not support high
resolution CPU time measurements `fprof` will use wallclock time for its
calculations, and it will appear as if functions are randomly burning virtual
machine time.

### See Also

[fprof - The File Trace Profiler](fprof_chapter.md), `m:dbg`, `m:eprof`

# `analyse_option`
*not exported* 

```erlang
-type analyse_option() ::
          append | callers |
          {callers, boolean()} |
          {cols, Cols :: non_neg_integer()} |
          dest |
          {dest, Dest :: pid() | (Destfile :: file:filename())} |
          details |
          {details, boolean()} |
          no_callers | no_details |
          {sort, SortSpec :: acc | own} |
          totals |
          {totals, boolean()}.
```

# `apply_option`
*not exported* 

```erlang
-type apply_option() ::
          continue | {procs, PidList :: [pid()]} | start | (TraceStartOption :: trace_option()).
```

# `pid_spec`
*not exported* 

```erlang
-type pid_spec() :: pid() | atom().
```

# `profile_option`
*not exported* 

```erlang
-type profile_option() ::
          append | dump |
          {dump, pid() | (Dump :: (Dumpfile :: file:filename() | []))} |
          file |
          {file, Filename :: file:filename()} |
          start | stop.
```

# `trace_option`
*not exported* 

```erlang
-type trace_option() ::
          cpu_time |
          {cpu_time, boolean()} |
          file |
          {file, Filename :: file:filename()} |
          {procs, PidSpec :: pid_spec()} |
          {procs, [PidSpec :: pid_spec()]} |
          start | stop |
          {tracer, Tracer :: pid() | port()} |
          verbose |
          {verbose, boolean()}.
```

# `analyse`

```erlang
-spec analyse() -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when ServerPid :: pid(), Reason :: term().
```

# `analyse`

```erlang
-spec analyse(OptionName) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when OptionName :: atom(), ServerPid :: pid(), Reason :: term();
             ({OptionName, OptionValue}) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when OptionName :: atom(), OptionValue :: term(), ServerPid :: pid(), Reason :: term();
             (OptionList) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when
                     OptionList :: [Option],
                     Option :: analyse_option(),
                     ServerPid :: pid(),
                     Reason :: term().
```

Analyses raw profile data in the `fprof` server.

If `Arg` is an atom, this call is equivalent to `analyse([Arg])`.

If `Arg` is a a tuple `{Option, _}`, this call is equivalent to
`analyse([Option])`.

Otherwise `Arg` must be a list of valid options.

If called when no raw profile data is available, `{error, no_profile}`
is returned.

`Destfile` is used to call `file:open/2`.

Option description:

- **`dest` | `{dest, Dest}`** - Specifies the destination for the analysis. If
  this option is not given or it is `dest`, the destination will be the caller's
  group leader, otherwise the destination `Dest` is either the `t:pid/0` of an
  I/O device or a filename. If the filename is `[]`, `"fprof.analysis"` is used
  instead.

- **`append`** - Causes the analysis to be appended to the destination file.
  This option is only allowed with the `{dest, Destfile}` option.

- **`{cols, Cols}`** - Specifies the number of columns in the analysis text. If
  this option is not given the number of columns is set to 80.

- **`callers` | `{callers, true}`** - Prints callers and called information in
  the analysis. This is the default.

- **`{callers, false}` | `no_callers`** - Suppresses the printing of callers and
  called information in the analysis.

- **`{sort, SortSpec}`** - Specifies if the analysis should be sorted according
  to the ACC column, which is the default, or the OWN column. See
  [Analysis Format](`m:fprof#analysis`) below.

- **`totals` | `{totals, true}`** - Includes a section containing call
  statistics for all calls regardless of process, in the analysis.

- **`{totals, false}`** - Suppresses the totals section in the analysis, which
  is the default.

- **`details` | `{details, true}`** - Prints call statistics for each process in
  the analysis. This is the default.

- **`{details, false}` | `no_details`** - Suppresses the call statistics for
  each process from the analysis.

# `analyse`

```erlang
-spec analyse(OptionName, OptionValue) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when OptionName :: atom(), OptionValue :: term(), ServerPid :: pid(), Reason :: term().
```

# `apply`

```erlang
-spec apply(Func, Args) -> term()
               when Func :: fun() | {Module :: module(), Function :: atom()}, Args :: [term()].
```

# `apply`

```erlang
-spec apply(Module, Function, Args) -> term()
               when Module :: module(), Function :: atom(), Args :: [term()];
           (Func, Args, OptionList) -> term()
               when
                   Func :: fun() | {Module :: module(), Function :: atom()},
                   Args :: [term()],
                   OptionList :: [Option],
                   Option :: apply_option().
```

Calls the given function surrounded by
[`trace([start, ...])`](`trace/1`) and
[`trace(stop)`](`trace/1`).

If the function arguments (`Arg1`, `Arg2`, and `Arg3`) are `Module`
(an atom), `Function` (an atom), and `Args` (a list), the function
will be called using
[`erlang:apply(Module, Function, Args)`](`erlang:apply/3`).

If the function arguments are `Func` (a fun), `Args` (a list), and
`OptionList` (a list of options), the fun will be called using
[`erlang:apply(Func, Args)`](`erlang:apply/2`).

Some effort is made to keep the trace clean from unnecessary trace messages;
tracing is started and stopped from a spawned process while `erlang:apply/2`
is called in the current process only surrounded by `receive` and `send`
statements towards the trace starting process. The trace starting process exits
when it is not needed any more.

The `TraceStartOption` is any option allowed for `trace/1`. The
options `[start, {procs, [self() | PidList]} | OptList]` are given to
[`trace/1`](`trace/1`), where `OptList` is `OptionList` with the
`continue`, `start` and `{procs, _}` options removed.

The `continue` option inhibits the call to [`trace(stop)`](`trace/1`) and leaves
it up to the caller to stop tracing at a suitable time.

# `apply`

```erlang
-spec apply(Module, Function, Args, OptionList) -> term()
               when
                   Module :: module(),
                   Function :: atom(),
                   Args :: [term()],
                   OptionList :: [Option],
                   Option :: apply_option().
```

# `profile`

```erlang
-spec profile() -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when ServerPid :: pid(), Reason :: term().
```

# `profile`

```erlang
-spec profile(OptionName) -> ok | {ok, Tracer} | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when OptionName :: atom(), Tracer :: pid(), ServerPid :: pid(), Reason :: term();
             ({OptionName, OptionValue}) ->
                 ok | {ok, Tracer} | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when
                     OptionName :: atom(),
                     OptionValue :: term(),
                     Tracer :: pid(),
                     ServerPid :: pid(),
                     Reason :: term();
             (OptionList) -> ok | {ok, Tracer} | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when
                     OptionList :: [Option],
                     Option :: profile_option(),
                     Tracer :: pid(),
                     ServerPid :: pid(),
                     Reason :: term().
```

Compiles a trace into raw profile data held by the `fprof` server.

If `Arg` is an atom, this call is equivalent to `profile([Arg])`.

If `Arg` is a tuple `{OptionName, OptionValue}`,
this call is equivalent to `profile([Arg])`.

Otherwise, `Arg` must be a list of options.

`Dumpfile` is used to call `file:open/2`, and `Filename` is used to call
[`dbg:trace_port(file, Filename)`](`dbg:trace_port/2`).

The following options are supported:

- **`file` | `{file, Filename}`** - Reads the file `Filename` and creates raw
  profile data that is stored in RAM by the `fprof` server. If the option `file`
  is given, or none of these options are given, the file `fprof.trace` is
  read. The call will return when the whole trace has been read with the return
  value `ok` if successful. This option is not allowed with the `start` or
  `stop` options.

- **`dump` | `{dump, Dump}`** - Specifies the destination for the trace text
  dump. If this option is not given, no dump is generated, if it is `dump` the
  destination will be the caller's group leader, otherwise the destination
  `Dump` is either the pid of an I/O device or a filename. If the
  filename is `[]`, `"fprof.dump"` is used instead. This option cannot be
  combined with the `stop` option.

- **`append`** - Causes the trace text dump to be appended to the destination
  file. This option is only allowed with the `{dump, Dumpfile}` option.

- **`start`** - Starts a tracer process that profiles trace data in runtime. The
  call will return immediately with the return value `{ok, Tracer}` if
  successful. This option is not allowed with the `stop`, `file`, or
  `{file, Filename}` options.

- **`stop`** - Stops the tracer process that profiles trace data in runtime. The
  return value will be value `ok` if successful. This option cannot be combined
  with the `start`, `file`, or `{file, Filename}` options.

# `profile`

```erlang
-spec profile(OptionName, OptionValue) ->
                 ok | {ok, Tracer} | {error, Reason} | {'EXIT', ServerPid, Reason}
                 when
                     OptionName :: atom(),
                     OptionValue :: term(),
                     Tracer :: pid(),
                     ServerPid :: pid(),
                     Reason :: term().
```

# `start`

```erlang
-spec start() -> {ok, Pid} | {error, {already_started, Pid}} when Pid :: pid().
```

Starts the `fprof` server.

Note that there is seldom any need to call this function directly, since
the server will be automatically started by any function that will need it.

# `stop`

```erlang
-spec stop() -> ok.
```

# `stop`

```erlang
-spec stop(Reason) -> ok when Reason :: term().
```

Stops the `fprof` server.

The supplied `Reason` becomes the exit reason for the server process. By default,
any `Reason` other than `kill` sends a request to the server and waits for it to
clean up, reply, and exit. If `Reason` is `kill`, the server is bluntly killed.

If the `fprof` server is not running, this function returns immediately.

> #### Note {: .info }
>
> When the `fprof` server is stopped the collected raw profile data is lost.

# `trace`

```erlang
-spec trace(verbose) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when ServerPid :: pid(), Reason :: term();
           (OptionName) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when OptionName :: atom(), ServerPid :: pid(), Reason :: term();
           ({OptionName, OptionValue}) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when OptionName :: atom(), OptionValue :: term(), ServerPid :: pid(), Reason :: term();
           (OptionList) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when
                   OptionList :: [Option],
                   Option :: trace_option(),
                   ServerPid :: pid(),
                   Reason :: term().
```

Starts or stops tracing.

If `Arg` is atom `verbose`, this call is equivalent to
`trace([start, verbose])`.

If `Arg` is an atom, this call is equivalent to
`trace([Arg])`.

If `Arg` is a tuple `{OptionName, OptionValue}`, this call is equivalent to
`trace([Arg])`.

Otherwise, `Arg` has to be a list of [trace options](`t:trace_option/0`).

`PidSpec` and `Tracer` are used in calls to
[`erlang:trace(PidSpec, true, [{tracer, Tracer} | Flags])`](`erlang:trace/3`),
and `Filename` is used to call
[`dbg:trace_port(file, Filename)`](`dbg:trace_port/2`).

Option description:

- **`stop`** - Stops a running `fprof` trace and clears all tracing from the
  node. Either option `stop` or `start` must be specified, but not both.

- **`start`** - Clears all tracing from the node and starts a new `fprof` trace.
  Either option `start` or `stop` must be specified, but not both.

- **`verbose` | `{verbose, boolean()}`** - The `verbose` or
  `{verbose, true}` options add some trace flags that `fprof` does not need, but that
  can be interesting for general debugging purposes. These options are only allowed
  with the `start` option.

- **`cpu_time` | `{cpu_time, boolean()}`** - The `cpu_time` or
  `{cpu_time, true}` options make the timestamps in the trace be in CPU time instead of
  the default wallclock time. These options are only allowed with the
  `start` option.

  > #### Note {: .info }
  >
  > Getting correct values out of `cpu_time` can be difficult. The best way to get
  > correct values is to run using a single scheduler and bind that scheduler to
  > a specific CPU. For example:
  >
  > ```bash
  > erl +S 1 +sbt db`
  > ```

- **`{procs, PidSpec}` | `{procs, [PidSpec]}`** - Specifies which processes that
  should be traced. If this option is not given, the calling process is traced.
  All processes spawned by the traced processes are also traced. This option is
  only allowed with the `start` option.

- **`file` | `{file, Filename}`** - Specifies the filename of the trace. If the
  option `file` is given, or none of these options are given, the file
  `fprof.trace` is used. This option is only allowed with the `start` option,
  but not with the `{tracer, Tracer}` option.

- **`{tracer, Tracer}`** - Specifies that trace to process or port shall be done
  instead of trace to file. This option is only allowed with the `start` option,
  but not with the `{file, Filename}` option.

# `trace`

```erlang
-spec trace(start, Filename) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when Filename :: file:filename(), ServerPid :: pid(), Reason :: term();
           (verbose, Filename) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when Filename :: file:filename(), ServerPid :: pid(), Reason :: term();
           (OptionName, OptionValue) -> ok | {error, Reason} | {'EXIT', ServerPid, Reason}
               when OptionName :: atom(), OptionValue :: term(), ServerPid :: pid(), Reason :: term().
```

Starts or stop tracing.

If `What` is atom `start`, this call is equivalent to
[`trace([start, {file, Value}])`](`trace/1`).

If `What` is atom `verbose`, this call is equivalent to
[`trace([start, verbose, {file, Value}])`](`trace/1`).

If `What` is a tuple `{OptionName, OptionValue}`,
this call is equivalent to
[`trace([What])`](`trace/1`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
