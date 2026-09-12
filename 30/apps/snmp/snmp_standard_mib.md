# `snmp_standard_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_standard_mib.erl#L22)

Instrumentation Functions for STANDARD-MIB and SNMPv2-MIB

The module `snmp_standard_mib` implements the instrumentation functions for the
STANDARD-MIB and SNMPv2-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.

# `configure`

```erlang
-spec configure(ConfDir) -> snmp:void() when ConfDir :: string().
```

This function is called from the supervisor at system start-up.

Inserts all data in the configuration files into the database and destroys all
old rows with StorageType `volatile`. The rows created from the configuration
file will have StorageType `nonVolatile`.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `standard.conf`.

# `inc`

```erlang
-spec inc(Name) -> snmp:void() when Name :: atom().
```

# `inc`

```erlang
-spec inc(Name, N) -> snmp:void() when Name :: atom(), N :: integer().
```

Increments a variable in the MIB with `N`, or one if `N` is not specified.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-STANDARD-MIB and SNMPv2-MIB, after this function has
been called, is from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `standard.conf`.

# `reset`

```erlang
-spec reset() -> snmp:void().
```

Resets all `snmp` counters to 0.

# `sys_up_time`

```erlang
-spec sys_up_time() -> Time when Time :: integer().
```

Gets the system up time in hundredth of a second.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
