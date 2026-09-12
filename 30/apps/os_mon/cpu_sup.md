# `cpu_sup`
[🔗](https://github.com/erlang/otp/blob/master/lib/os_mon/src/cpu_sup.erl#L22)

A CPU Load and CPU Utilization Supervisor Process

`cpu_sup` is a process which supervises the CPU load and CPU utilization. It is
part of the OS_Mon application, see [os_mon(6)](os_mon_app.md). Available for
Unix, although CPU utilization values (`util/0,1`) are only available for
Solaris, Linux, FreeBSD and OpenBSD.

The load values are proportional to how long time a runnable Unix process has to
spend in the run queue before it is scheduled. Accordingly, higher values mean
more system load. The returned value divided by 256 produces the figure
displayed by `rup` and `top`. What is displayed as 2.00 in `rup`, is displayed
as load up to the second mark in `xload`.

For example, `rup` displays a load of 128 as 0.50, and 512 as 2.00.

If the user wants to view load values as percentage of machine capacity, then
this way of measuring presents a problem, because the load values are not
restricted to a fixed interval. In this case, the following simple mathematical
transformation can produce the load value as a percentage:

```text
PercentLoad = 100 * (1 - D/(D + Load))
```

`D` determines which load value should be associated with which percentage.
Choosing `D` = 50 means that 128 is 60% load, 256 is 80%, 512 is 90%, and so on.

Another way of measuring system load is to divide the number of busy CPU cycles
by the total number of CPU cycles. This produces values in the 0-100 range
immediately. However, this method hides the fact that a machine can be more or
less saturated. CPU utilization is therefore a better name than system load for
this measure.

A server which receives just enough requests to never become idle will score a
CPU utilization of 100%. If the server receives 50% more requests, it will still
score 100%. When the system load is calculated with the percentage formula shown
previously, the load will increase from 80% to 87%.

The `avg1/0`, `avg5/0`, and `avg15/0` functions can be used for retrieving
system load values, and the `util/0` and [`util/1`](`util/1`) functions can be
used for retrieving CPU utilization values.

When run on Linux, `cpu_sup` assumes that the `/proc` file system is present and
accessible by `cpu_sup`. If it is not, `cpu_sup` will terminate.

### See Also

[os_mon(3)](os_mon_app.md)

# `util_cpus`
*not exported* 

```erlang
-type util_cpus() :: all | integer() | [integer()].
```

# `util_desc`
*not exported* 

```erlang
-type util_desc() :: {util_cpus(), util_value(), util_value(), []}.
```

# `util_state`
*not exported* 

```erlang
-type util_state() :: user | nice_user | kernel | wait | idle.
```

# `util_value`
*not exported* 

```erlang
-type util_value() :: [{util_state(), number()}] | number().
```

# `avg1`

```erlang
-spec avg1() -> SystemLoad :: integer() | {error, any()}.
```

Returns the average system load in the last minute, as described above. 0
represents no load, 256 represents the load reported as 1.00 by `rup`.

Returns 0 if `cpu_sup` is not available.

# `avg5`

```erlang
-spec avg5() -> SystemLoad :: integer() | {error, any()}.
```

Returns the average system load in the last five minutes, as described above. 0
represents no load, 256 represents the load reported as 1.00 by `rup`.

Returns 0 if `cpu_sup` is not available.

# `avg15`

```erlang
-spec avg15() -> SystemLoad :: integer() | {error, any()}.
```

Returns the average system load in the last 15 minutes, as described above. 0
represents no load, 256 represents the load reported as 1.00 by `rup`.

Returns 0 if `cpu_sup` is not available.

# `nprocs`

```erlang
-spec nprocs() -> UnixProcesses :: integer() | {error, any()}.
```

Returns the number of UNIX processes running on this machine. This is a crude
way of measuring the system load, but it may be of interest in some cases.

Returns 0 if `m:cpu_sup` is not available.

# `util`

```erlang
-spec util() -> CpuUtil :: number() | {error, any()}.
```

Returns CPU utilization since the last call to `util/0` or [`util/1`](`util/1`)
by the calling process.

> #### Note {: .info }
>
> The returned value of the first call to `util/0` or [`util/1`](`util/1`) by a
> process will on most systems be the CPU utilization since system boot, but
> this is not guaranteed and the value should therefore be regarded as garbage.
> This also applies to the first call after a restart of `cpu_sup`.

The CPU utilization is defined as the sum of the percentage shares of the CPU
cycles spent in all busy processor states (see [`util/1`](`util/1`)) in
average on all CPUs.

Returns 0 if `cpu_sup` is not available.

# `util`

```erlang
-spec util([detailed | per_cpu]) -> util_desc() | [util_desc()] | {error, any()}.
```

Returns CPU utilization since the last call to `util/0` or [`util/1`](`util/1`)
by the calling process, in more detail than `util/0`.

> #### Note {: .info }
>
> The returned value of the first call to `util/0` or [`util/1`](`util/1`) by a
> process will on most systems be the CPU utilization since system boot, but
> this is not guaranteed and the value should therefore be regarded as garbage.
> This also applies to the first call after a restart of `cpu_sup`.

Currently recognized options:

- **`detailed`** - The returned `UtilDesc`(s) will be even more detailed.

- **`per_cpu`** - Each CPU will be specified separately (assuming this
  information can be retrieved from the operating system), that is, a list with
  one `UtilDesc` per CPU will be returned.

Description of `UtilDesc = {Cpus, Busy, NonBusy, Misc}`:

- **`Cpus`** - If the `detailed` and/or `per_cpu` option is given, this is the
  CPU number, or a list of the CPU numbers.

  If not, this is the atom `all` which implies that the `UtilDesc` contains
  information about all CPUs.

- **`Busy`** - If the `detailed` option is given, this is a list of
  `{State, Share}` tuples, where each tuple contains information about a
  processor state that has been identified as a busy processor state (see
  below). The atom `State` is the name of the state, and the float `Share`
  represents the percentage share of the CPU cycles spent in this state since
  the last call to `util/0` or [`util/1`](`util/1`).

  If not, this is the sum of the percentage shares of the CPU cycles spent in
  all states identified as busy.

  If the `per_cpu` is not given, the value(s) presented are the average of all
  CPUs.

- **`NonBusy`** - Similar to `Busy`, but for processor states that have been
  identified as non-busy (see below).

- **`Misc`** - Currently unused; reserved for future use.

Currently these processor states are identified as busy:

- **`user`** - Executing code in user mode.

- **`nice_user`** - Executing code in low priority (nice) user mode. This state
  is currently only identified on Linux.

- **`kernel`** - Executing code in kernel mode.

Currently these processor states are identified as non-busy:

- **`wait`** - Waiting. This state is currently only identified on Solaris.

- **`idle`** - Idle.

> #### Note {: .info }
>
> Identified processor states may be different on different operating systems
> and may change between different versions of `cpu_sup` on the same operating
> system. The sum of the percentage shares of the CPU cycles spent in all busy
> and all non-busy processor states will always add up to 100%, though.

Returns `{all,0,0,[]}` if `cpu_sup` is not available.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
