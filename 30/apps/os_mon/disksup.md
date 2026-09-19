# `disksup`
[🔗](https://github.com/erlang/otp/blob/master/lib/os_mon/src/disksup.erl#L22)

A Disk Supervisor Process

`disksup` is a process which supervises the available disk space in the system.
It is part of the OS_Mon application, see [os_mon(6)](os_mon_app.md). Available
for Unix and Windows.

Periodically checks the disks. For each disk or partition which uses more than a
certain amount of the available space, the alarm
`{{disk_almost_full, MountedOn}, []}` is set.

- **On Unix** - All (locally) mounted disks are checked, including the swap disk
  if it is present.

- **On WIN32** - All logical drives of type "FIXED_DISK" are checked.

Alarms are reported to the SASL alarm handler, see `m:alarm_handler`. To set an
alarm, [`alarm_handler:set_alarm(Alarm)`](`alarm_handler:set_alarm/1`) is called
where `Alarm` is the alarm specified above.

The alarms are cleared automatically when the alarm cause is no longer valid.

## Configuration

The following configuration parameters can be used to change the default values
for time interval and threshold:

- **`disk_space_check_interval = ` `t:time/0`** - The time interval for the
  periodic disk space check. The default is 30 minutes.

- **`disk_almost_full_threshold = float()`** - The threshold, as percentage of
  total disk space, for how much disk can be utilized before the
  `disk_almost_full` alarm is set. The default is 0.80 (80%).

- **`disksup_posix_only = bool()`** - Specifies whether the `disksup` helper
  process should only use POSIX conformant commands (`true`) or not. The default
  is `false`. Setting this parameter to `true` can be necessary on embedded
  systems with stripped-down versions of Unix tools like `df`. The returned disk
  data and alarms can be different when using this option.

  The parameter is ignored on platforms that are known to not be POSIX
  compatible (Windows and SunOS).

See [config](`e:kernel:config.md`) for information about how to change the
value of configuration parameters.

### See Also

`m:alarm_handler`, [os_mon](os_mon_app.md)

# `time`
*not exported* 

```erlang
-type time() :: pos_integer() | {TimeUnit :: erlang:time_unit(), Time :: pos_integer()}.
```

Time unit used for disklog APIs.

Supported units are:

- **`integer() >= 1`** - The time interval in minutes.

- **`{TimeUnit, Time}`** - The time interval `Time` in a time unit specified by
  `TimeUnit` where `TimeUnit` is of the type `t:erlang:time_unit/0` and `Time`
  is a positive integer. The time interval needs to be at least one millisecond
  long.

# `get_almost_full_threshold`

```erlang
-spec get_almost_full_threshold() -> Percent :: integer().
```

Returns the threshold, in percent, for disk space utilization.

# `get_check_interval`

```erlang
-spec get_check_interval() -> Milliseconds :: timer:time().
```

Returns the time interval, in milliseconds, for the periodic disk space check.

# `get_disk_data`

```erlang
-spec get_disk_data() -> [DiskData]
                       when
                           DiskData :: {Id, TotalKiB, Capacity},
                           Id :: string(),
                           TotalKiB :: integer(),
                           Capacity :: integer().
```

Returns the result of the latest disk check.

`Id` is a string that identifies the disk or partition. `TotalKiB` is the
total size of the disk or partition in kibibytes. `Capacity` is the
percentage of disk space used.

The function is asynchronous in the sense that it does not invoke a disk check,
but returns the latest available value.

Returns `[{"none",0,0}]` if `disksup` is not available.

# `get_disk_info`
*since OTP 26.0* 

```erlang
-spec get_disk_info() -> [DiskData]
                       when
                           DiskData :: {Id, TotalKiB, AvailableKiB, Capacity},
                           Id :: string(),
                           TotalKiB :: integer(),
                           AvailableKiB :: integer(),
                           Capacity :: integer().
```

Immediately fetches total space, available space and capacity for local disks.

`Id` is a string that identifies the disk or partition. `TotalKiB` is the total
size of the disk or partition in kibibytes. `AvailableKiB` is the disk space
used in kibibytes. `Capacity` is the percentage of disk space used.

Returns `[{"none",0,0,0}]` if `disksup` is not available.

# `get_disk_info`
*since OTP 26.0* 

```erlang
-spec get_disk_info(Path :: string()) -> [DiskData]
                       when
                           DiskData :: {Id, TotalKiB, AvailableKiB, Capacity},
                           Id :: string(),
                           TotalKiB :: integer(),
                           AvailableKiB :: integer(),
                           Capacity :: integer().
```

Immediately fetches total space, available space and capacity for a path.

`Id` is a string that identifies the disk or partition. `TotalKiB` is the total size
of the disk or partition in kibibytes. `AvailableKiB` is the disk space used in
kibibytes. `Capacity` is the percentage of disk space used.

Returns `[{Path,0,0,0}]` if the `Path` is invalid or space can't be determined.
Returns `[{"none",0,0,0}]` if `disksup` is not available.

# `set_almost_full_threshold`

```erlang
-spec set_almost_full_threshold(Float :: float()) -> ok.
```

Changes the threshold, given as a float (`0.0 =< Float =< 1.0`), for disk space
utilization.

The change will take effect during the next disk space check and is non-persist.
That is, in case of a process restart, this value is forgotten and the default
value will be used. See [Configuration](`m:disksup#module-configuration`) above.

# `set_check_interval`

```erlang
-spec set_check_interval(time()) -> ok.
```

Changes the time interval for the periodic disk space check.

The change will take effect after the next disk space check and is non-persist.
That is, in case of a process restart, this value is forgotten and the default
value will be used. See [Configuration](`m:disksup#module-configuration`) above.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
