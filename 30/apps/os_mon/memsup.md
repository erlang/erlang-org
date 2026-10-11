# `memsup`
[🔗](https://github.com/erlang/otp/blob/master/lib/os_mon/src/memsup.erl#L22)

A Memory Supervisor Process

`memsup` is a process which supervises the memory usage for the system and for
individual processes. It is part of the OS_Mon application, see
[os_mon](os_mon_app.md). Available for Unix and Windows.

Periodically performs a memory check:

- If more than a certain amount of available system memory is allocated, as
  reported by the underlying operating system, the alarm
  `{system_memory_high_watermark, []}` is set. How the amount of available
  memory is determined depends on the underlying OS and may change as better
  values become available.
- If any Erlang process `Pid` in the system has allocated more than a certain
  amount of total system memory, the alarm
  `{process_memory_high_watermark, Pid}` is set.

Alarms are reported to the SASL alarm handler, see `m:alarm_handler`. To set an
alarm, `alarm_handler:set_alarm(Alarm)` is called where `Alarm` is either of the
alarms specified above.

The alarms are cleared automatically when the alarm cause is no longer valid.

The function [get_memory_data()](`get_memory_data/0`) can be used to retrieve
the result of the latest periodic memory check.

There is also a interface to system dependent memory data,
[get_system_memory_data()](`get_system_memory_data/0`). The result is highly
dependent on the underlying operating system and the interface is targeted
primarily for systems without virtual memory. However, the output on other
systems is still valid, although sparse.

A call to `get_system_memory_data/0` is more costly than a call to
`get_memory_data/0` as data is collected synchronously when this function is
called.

The total system memory reported under UNIX is the number of physical pages of
memory times the page size, and the available memory is the number of available
physical pages times the page size. This is a reasonable measure as swapping
should be avoided anyway, but the task of defining total memory and available
memory is difficult because of virtual memory and swapping.

## Configuration

The following configuration parameters can be used to change the default values
for time intervals and thresholds:

- **`memory_check_interval = int()>0`** - The time interval, in minutes, for the
  periodic memory check. The default is one minute.

- **`system_memory_high_watermark = float()`** - The threshold, as percentage of
  system memory, for how much system memory can be allocated before the
  corresponding alarm is set. The default is 0.80 (80%).

- **`process_memory_high_watermark = float()`** - The threshold, as percentage
  of system memory, for how much system memory can be allocated by one Erlang
  process before the corresponding alarm is set. The default is 0.05 (5%).

- **`memsup_helper_timeout = int()>0`** - A timeout, in seconds, for how long
  the `memsup` process should wait for a result from a memory check. If the
  timeout expires, a warning message `"OS_MON (memsup) timeout"` is issued via
  `error_logger` and any pending, synchronous client calls will return a dummy
  value. Normally, this situation should not occur. There have been cases on
  Linux, however, where the pseudo file from which system data is read is
  temporarily unavailable when the system is heavily loaded.

  The default is 30 seconds.

- **`memsup_system_only = bool()`** - Specifies whether the `memsup` process
  should only check system memory usage (`true`) or not. The default is `false`,
  meaning that information regarding both system memory usage and Erlang process
  memory usage is collected.

  It is recommended to set this parameter to `true` on systems with many
  concurrent processes, as each process memory check makes a traversal of the
  entire list of processes.

See [config](`e:kernel:config.md`) for information about how to change the
value of configuration parameters.

### See Also

`m:alarm_handler`, [os_mon](os_mon_app.md)

# `get_check_interval`

```erlang
-spec get_check_interval() -> Milliseconds :: timer:time().
```

Returns the time interval, in milliseconds, for the periodic memory check.

# `get_helper_timeout`

```erlang
-spec get_helper_timeout() -> Seconds :: integer().
```

Returns the timeout value, in seconds, for memory checks.

# `get_memory_data`

```erlang
-spec get_memory_data() -> {Total, Allocated, Worst}
                         when
                             Total :: integer(),
                             Allocated :: integer(),
                             Worst :: {Pid, PidAllocated} | undefined,
                             Pid :: pid(),
                             PidAllocated :: integer().
```

Returns the result of the latest memory check, where `Total` is the total memory
size and `Allocated` the allocated memory size, in bytes.

`Worst` is the pid and number of allocated bytes of the largest Erlang process
on the node. If `memsup` should not collect process data, that is if the
configuration parameter `memsup_system_only` was set to `true`, `Worst` is
`undefined`.

The function is normally asynchronous in the sense that it does not invoke a
memory check, but returns the latest available value. The one exception if is
the function is called before a first memory check is finished, in which case it
does not return a value until the memory check is finished.

Returns `{0,0,{pid(),0}}` or `{0,0,undefined}` if `memsup` is not available, or
if all memory checks so far have timed out.

# `get_os_wordsize`

```erlang
-spec get_os_wordsize() -> Wordsize when Wordsize :: 32 | 64 | unsupported_os.
```

Returns the wordsize of the current running operating system.

# `get_procmem_high_watermark`

```erlang
-spec get_procmem_high_watermark() -> integer().
```

Returns the threshold, in percent, for process memory allocation.

# `get_sysmem_high_watermark`

```erlang
-spec get_sysmem_high_watermark() -> integer().
```

Returns the threshold, in percent, for system memory allocation.

# `get_system_memory_data`

```erlang
-spec get_system_memory_data() -> MemDataList
                                when MemDataList :: [{Tag, Size}], Tag :: atom(), Size :: integer().
```

Invokes a memory check and returns the resulting, system dependent, data as a
list of tagged tuples, where `Tag` currently can be one of the following:

- **`total_memory`** - The total amount of memory available to the Erlang
  emulator, allocated and free. May or may not be equal to the amount of memory
  configured in the system.

- **`available_memory`** - Informs about the amount memory that is available for
  increased usage if there is an increased memory need. This value is not based
  on a calculation of the other provided values and should give a better value
  of the amount of memory that actually is available than calculating a value
  based on the other values reported. This value is currently only present on
  newer Linux kernels. If this value is not available on Linux, you can use the
  sum of `cached_memory`, `buffered_memory`, and `free_memory` as an
  approximation.

- **`free_memory`** - The amount of free memory available to the Erlang emulator
  for allocation.

- **`system_total_memory`** - The amount of memory available to the whole
  operating system. This may well be equal to `total_memory` but not
  necessarily.

- **`buffered_memory`** - The amount of memory the system uses for temporary
  storing raw disk blocks.

- **`cached_memory`** - The amount of memory the system uses for cached files
  read from disk. On Linux, also memory marked as reclaimable in the kernel slab
  allocator will be added to this value.

- **`total_swap`** - The amount of total amount of memory the system has
  available for disk swap.

- **`free_swap`** - The amount of memory the system has available for disk swap.

> #### Note {: .info }
>
> Note that new tagged tuples may be introduced in the result at any time
> without prior notice

Note that the order of the tuples in the resulting list is undefined and may
change at any time.

All memory sizes are presented as number of _bytes_.

Returns the empty list [] if `memsup` is not available, or if the memory check
times out.

# `set_check_interval`

```erlang
-spec set_check_interval(Minutes :: non_neg_integer()) -> ok.
```

Changes the time interval, given in minutes, for the periodic memory check.

The change will take effect after the next memory check and is non-persistent.
That is, in case of a process restart, this value is forgotten and the default
value will be used. See [Configuration](`m:memsup#module-configuration`).

# `set_helper_timeout`

```erlang
-spec set_helper_timeout(Seconds :: non_neg_integer()) -> ok.
```

Changes the timeout value, given in seconds, for memory checks.

The change will take effect for the next memory check and is non-persistent.
That is, in the case of a process restart, this value is forgotten and the
default value will be used. See [Configuration](`m:memsup#module-configuration`) above.

# `set_procmem_high_watermark`

```erlang
-spec set_procmem_high_watermark(Float :: term()) -> ok.
```

Changes the threshold, given as a float, for process memory allocation.

The change will take effect during the next periodic memory check and is
non-persistent. That is, in case of a process restart, this value is forgotten
and the default value will be used. See [Configuration](`m:memsup#module-configuration`).

# `set_sysmem_high_watermark`

```erlang
-spec set_sysmem_high_watermark(Float :: term()) -> ok.
```

Changes the threshold, given as a float, for system memory allocation.

The change will take effect during the next periodic memory check and is
non-persistent. That is, in case of a process restart, this value is forgotten
and the default value will be used. See [Configuration](`m:memsup#module-configuration`)
above.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
