# `msacc`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/runtime_tools/src/msacc.erl#L30)

Convenience functions for microstate accounting

This module implements some convenience functions for analyzing microstate
accounting data. For details about how to use the basic API and what the
different states represent, see
[`erlang:statistics(microstate_accounting)`](`m:erlang#statistics_microstate_accounting`).

[](){: #msacc_print_example }

_Basic Scenario_

```erlang
1> msacc:start(1000).
ok
2> msacc:print().
Average thread real-time    : 1000513 us
Accumulated system run-time :    2213 us
Average scheduler run-time  :    1076 us

        Thread      aux check_io emulator       gc    other     port    sleep

Stats per thread:
     async( 0)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
     async( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
       aux( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%   99.99%
 scheduler( 1)    0.00%    0.03%    0.13%    0.00%    0.01%    0.00%   99.82%
 scheduler( 2)    0.00%    0.00%    0.00%    0.00%    0.03%    0.00%   99.97%

Stats per type:
         async    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
           aux    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%   99.99%
     scheduler    0.00%    0.02%    0.06%    0.00%    0.02%    0.00%   99.89%
ok
```

This first command enables microstate accounting for 1000 milliseconds. See
`start/0`, `stop/0`, `reset/0`, and `start/1` for more details. The second
command prints the statistics gathered during that time. First three general
statistics are printed.

- **Average real-time** - The average time spent collecting data in the threads.
  This should be close to the time which data was collected.

- **System run-time** - The total run-time of all threads in the system. This is
  what you get if you call `msacc:stats(total_runtime,Stats).`

- **Average scheduler run-time** - The average run-time for the schedulers. This
  is the average amount of time the schedulers did not sleep.

Then one column per state is printed with a the percentage of time this thread
spent in the state out of it's own real-time. After the thread specific time,
the accumulated time for each type of thread is printed in a similar format.

Since we have the average real-time and the percentage spent in each state we
can easily calculate the time spent in each state by multiplying
`Average thread real-time` with `Thread state %`, that is, to get the time Scheduler
1 spent in the emulator state we do `1000513us * 0.13% = 1300us`.

# `msacc_data`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_data() :: [msacc_data_thread()].
```

# `msacc_data_counters`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_data_counters() :: #{msacc_state() => non_neg_integer()}.
```

A map containing the different microstate accounting states and the number of
microseconds spent in it.

# `msacc_data_thread`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_data_thread() ::
          #{'$type' := msacc_data,
            type := msacc_type(),
            id := msacc_id(),
            counters := msacc_data_counters()}.
```

# `msacc_id`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_id() :: non_neg_integer().
```

# `msacc_print_options`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_print_options() :: #{system => boolean()}.
```

The different options that can be given to `print/2`.

# `msacc_state`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_state() ::
          alloc | aux | bif | busy_wait | check_io | emulator | ets | gc | gc_fullsweep | nif | other |
          port | send | sleep | timers.
```

The different states that a thread can be in. See
[erlang:statistics(microstate_accounting)](`m:erlang#statistics_microstate_accounting`)
for details.

# `msacc_stats`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_stats() :: [msacc_stats_thread()].
```

# `msacc_stats_counters`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_stats_counters() :: #{msacc_state() => #{thread := float(), system := float()}}.
```

A map containing the different microstate accounting states. Each value in the
map contains another map with the percentage of time that this thread has spent
in the specific state. Both the percentage of `system` time and the time for
that specific `thread` is part of the map.

# `msacc_stats_thread`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_stats_thread() ::
          #{'$type' := msacc_stats,
            type := msacc_type(),
            id := msacc_id(),
            system := float(),
            counters := msacc_stats_counters()}.
```

A map containing information about a specific thread. The percentages in the map
can be either run-time or real-time depending on if `runtime` or `realtime` was
requested from `stats/2`. `system` is the percentage of total system time for
this specific thread.

# `msacc_type`
*not exported* *since OTP 19.0* 

```erlang
-type msacc_type() :: aux | async | dirty_cpu_scheduler | dirty_io_scheduler | poll | scheduler.
```

# `available`
*since OTP 19.0* 

```erlang
-spec available() -> boolean().
```

This function checks whether microstate accounting is available or not.

# `from_file`
*since OTP 19.0* 

```erlang
-spec from_file(Filename) -> msacc_data() when Filename :: file:name_all().
```

Read a file dump produced by [to_file(Filename)](`to_file/1`).

# `print`
*since OTP 19.0* 

```erlang
-spec print() -> ok.
```

Prints the current microstate accounting to standard out. Equivalent to
[`msacc:print(msacc:stats(), #{}).`](`print/1`)

# `print`
*since OTP 19.0* 

```erlang
-spec print(DataOrStats) -> ok when DataOrStats :: msacc_data() | msacc_stats().
```

# `print`
*since OTP 19.0* 

```erlang
-spec print(DataOrStats, Options) -> ok
               when DataOrStats :: msacc_data() | msacc_stats(), Options :: msacc_print_options().
```

Print the given microstate statistics values to standard out. With many states
this can be verbose. See the top of this reference manual for a brief
description of what the fields mean.

It is possible to print more specific types of statistics by first manipulating
the `DataOrStats` using `stats/2`. For instance if you want to print the
percentage of run-time for each thread you can do:

```erlang
msacc:print(msacc:stats(runtime, msacc:stats())).
```

If you want to only print run-time per thread type you can do:

```erlang
msacc:print(msacc:stats(type, msacc:stats(runtime, msacc:stats()))).
```

_Options_

- **`system`** - Print percentage of time spent in each state out of system time
  as well as thread time. Default: false.

# `print`
*since OTP 19.0* 

```erlang
-spec print(FileOrDevice, DataOrStats, Options) -> ok
               when
                   FileOrDevice :: file:filename() | io:device(),
                   DataOrStats :: msacc_data() | msacc_stats(),
                   Options :: msacc_print_options().
```

Print the given microstate statistics values to the given file or device. The
other arguments behave the same way as for `print/2`.

# `reset`
*since OTP 19.0* 

```erlang
-spec reset() -> boolean().
```

Reset microstate accounting counters. Returns whether is was enabled or
disabled.

# `start`
*since OTP 19.0* 

```erlang
-spec start() -> boolean().
```

Start microstate accounting. Returns whether it was previously enabled or
disabled.

# `start`
*since OTP 19.0* 

```erlang
-spec start(Time) -> true when Time :: timeout().
```

Resets all counters and then starts microstate accounting for the given
milliseconds.

# `stats`
*since OTP 19.0* 

```erlang
-spec stats() -> msacc_data().
```

Returns a runtime system independent version of the microstate statistics data
presented by
[`erlang:statistics(microstate_accounting)`](`m:erlang#statistics_microstate_accounting`).
All counters have been normalized to be in microsecond resolution.

# `stats`
*since OTP 19.0* 

```erlang
-spec stats(Analysis, Stats) -> non_neg_integer()
               when Analysis :: system_realtime | system_runtime, Stats :: msacc_data();
           (Analysis, Stats) -> msacc_stats() when Analysis :: realtime | runtime, Stats :: msacc_data();
           (Analysis, StatsOrData) -> msacc_data() | msacc_stats()
               when Analysis :: type, StatsOrData :: msacc_data() | msacc_stats().
```

Returns the system time for the given microstate statistics values. System time
is the accumulated time of all threads.

- **`realtime`** - Returns all time recorded for all threads.

- **`runtime`** - Returns all time spent doing work for all threads, i.e. all
  time not spent in the `sleep` state.

Returns fractions of real-time or run-time spent in the various threads from the
given microstate statistics values.

Returns a list of microstate statistics values where the values for all threads
of the same type has been merged.

# `stop`
*since OTP 19.0* 

```erlang
-spec stop() -> boolean().
```

Stop microstate accounting. Returns whether is was previously enabled or
disabled.

# `to_file`
*since OTP 19.0* 

```erlang
-spec to_file(Filename) -> ok | {error, file:posix()} when Filename :: file:name_all().
```

Dumps the current microstate statistics counters to a file that can be parsed
with `file:consult/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
