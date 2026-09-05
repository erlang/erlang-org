# `scheduler`
[🔗](https://github.com/erlang/otp/blob/master/lib/runtime_tools/src/scheduler.erl#L26)

Measure scheduler utilization

This module contains utility functions for easy measurement and calculation of
scheduler utilization. It act as a wrapper around the more primitive API
[`erlang:statistics(scheduler_wall_time)`](`m:erlang#statistics_scheduler_wall_time`).

The simplest usage is to call the blocking
[`scheduler:utilization(Seconds)`](`utilization/1`).

For non blocking and/or continuous calculation of scheduler utilization, the
recommended usage is:

- First call
  [`erlang:system_flag(scheduler_wall_time, true)`](`m:erlang#system_flag_scheduler_wall_time`)
  to enable scheduler wall time measurements.
- Call `get_sample/0` to collect samples with some time in between.
- Call `utilization/2` to calculate the scheduler utilization in the interval
  between two samples.
- When done call
  [`erlang:system_flag(scheduler_wall_time, false)`](`m:erlang#system_flag_scheduler_wall_time`)
  to disable scheduler wall time measurements and avoid unecessary CPU overhead.

To get correct values from `utilization/2`, it is important that
`scheduler_wall_time` is kept enabled during the entire interval between the two
samples. To ensure this, the process that called
[`erlang:system_flag(scheduler_wall_time, true)`](`m:erlang#system_flag_scheduler_wall_time`)
must be kept alive, as `scheduler_wall_time` will automatically be disabled if
it terminates.

# `sched_id`
*not exported* *since OTP 21.0* 

```erlang
-type sched_id() :: integer().
```

# `sched_sample`
*since OTP 21.0* 

```erlang
-opaque sched_sample()
```

# `sched_type`
*not exported* *since OTP 21.0* 

```erlang
-type sched_type() :: normal | cpu | io.
```

# `sched_util_result`
*not exported* *since OTP 21.0* 

```erlang
-type sched_util_result() ::
          [{sched_type(), sched_id(), float(), string()} |
           {total, float(), string()} |
           {weighted, float(), string()}].
```

A list of tuples containing results for individual schedulers as well as
aggregated averages. `Util` is the scheduler utilization as a floating point
value between 0.0 and 1.0. `Percent` is the same utilization as a more human
readable string expressed in percent.

- **`{normal, SchedulerId, Util, Percent}`** - Scheduler utilization of a normal
  scheduler with number `SchedulerId`. Schedulers that are not online will also
  be included. [Online schedulers](`m:erlang#system_info_schedulers_online`)
  have the lowest `SchedulerId`.

- **`{cpu, SchedulerId, Util, Percent}`** - Scheduler utilization of a dirty-cpu
  scheduler with number `SchedulerId`.

- **`{io, SchedulerId, Util, Percent}`** - Scheduler utilization of a dirty-io
  scheduler with number `SchedulerId`. This tuple will only exist if both
  samples were taken with `sample_all/0`.

- **`{total, Util, Percent}`** - Total utilization of all normal and dirty-cpu
  schedulers.

- **`{weighted, Util, Percent}`** - Total utilization of all normal and
  dirty-cpu schedulers, weighted against maximum amount of available CPU time.

# `get_sample`
*since OTP 24.3* 

```erlang
-spec get_sample() -> sched_sample() | undefined.
```

Returns a scheduler utilization sample for normal and dirty-cpu schedulers.
Returns `undefined` if system flag
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) has not been
enabled.

# `get_sample_all`
*since OTP 24.3* 

```erlang
-spec get_sample_all() -> sched_sample() | undefined.
```

Return a scheduler utilization sample for all schedulers, including dirty-io
schedulers. Returns `undefined` if system flag
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) has not been
enabled.

# `sample`
*since OTP 21.0* 

```erlang
-spec sample() -> sched_sample().
```

Return a scheduler utilization sample for normal and dirty-cpu schedulers. Will
call
[`erlang:system_flag(scheduler_wall_time, true)`](`m:erlang#system_flag_scheduler_wall_time`)
first if not already already enabled.

> #### Note {: .info }
>
> This function is _not recommended_ as there is no way to detect if
> `scheduler_wall_time` already was enabled or not. If `scheduler_wall_time` has
> been disabled between two samples, passing them to
> [`utilization/2`](`utilization/1`) will yield invalid results.
>
> Instead use `get_sample/0` together with
> [`erlang:system_flag(scheduler_wall_time, _)`](`m:erlang#system_flag_scheduler_wall_time`).

# `sample_all`
*since OTP 21.0* 

```erlang
-spec sample_all() -> sched_sample().
```

Return a scheduler utilization sample for all schedulers, including dirty-io
schedulers. Will call
[`erlang:system_flag(scheduler_wall_time, true)`](`m:erlang#system_flag_scheduler_wall_time`)
first if not already already enabled.

> #### Note {: .info }
>
> This function is _not recommended_ for same reason as `sample/0`. Instead use
> `get_sample_all/0` together with
> [`erlang:system_flag(scheduler_wall_time,_)`](`m:erlang#system_flag_scheduler_wall_time`).

# `utilization`
*since OTP 21.0* 

```erlang
-spec utilization(Seconds) -> sched_util_result() when Seconds :: pos_integer();
                 (Sample) -> sched_util_result() when Sample :: sched_sample().
```

Measure utilization for normal and dirty-cpu schedulers during `Seconds`
seconds, and then return the result.

Will automatically first enable and then disable
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`).

Calculate scheduler utilizations for the time interval from when `Sample` was
taken and "now". The same as calling
`scheduler:utilization(Sample, scheduler:sample_all())`.

> #### Note {: .info }
>
> This function is _not recommended_ as it's so easy to get invalid results
> without noticing. In particular do not do this:
>
> ```erlang
> scheduler:utilization(scheduler:sample()). % DO NOT DO THIS!
> ```
>
> The above example takes two samples in rapid succession and calculates the
> scheduler utilization between them. The resulting values will probably be more
> misleading than informative.
>
> Instead use [`scheduler:utilization/2`](`utilization/2`) and call
> `get_sample/0` to get samples with some time in between.

# `utilization`
*since OTP 21.0* 

```erlang
-spec utilization(Sample1, Sample2) -> sched_util_result()
                     when Sample1 :: sched_sample(), Sample2 :: sched_sample().
```

Calculates scheduler utilizations for the time interval between the two samples
obtained from calling [`get_sample/0`](`sample/0`) or
[`get_sample_all/0`](`sample_all/0`).

This function itself, does not need
[`scheduler_wall_time`](`m:erlang#system_flag_scheduler_wall_time`) to be
enabled. However, for a correct result, `scheduler_wall_time` must have been
enabled during the entire interval between the two samples.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
