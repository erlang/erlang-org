# `snmp_notification_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_notification_mib.erl#L22)

Instrumentation Functions for SNMP-NOTIFICATION-MIB

The module `snmp_notification_mib` implements the instrumentation functions for
the SNMP-NOTIFICATION-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.

# `notify_name`

```erlang
-type notify_name() :: snmp_framework_mib:admin_string().
```

`SnmpAdminString (SIZE(1..32))`

# `notify_tag`

```erlang
-type notify_tag() :: snmp_target_mib:tag_value().
```

# `notify_type`

```erlang
-type notify_type() :: trap | inform.
```

# `add_notify`

```erlang
-spec add_notify(Name, Tag, Type) -> {ok, Key} | {error, Reason}
                    when
                        Name :: notify_name(),
                        Tag :: notify_tag(),
                        Type :: notify_type(),
                        Key :: term(),
                        Reason :: term().
```

Adds a notify definition to the agent config. Equivalent to one line in the
`notify.conf` file.

# `configure`

```erlang
-spec configure(ConfDir) -> snmp:void() when ConfDir :: string().
```

This function is called from the supervisor at system start-up.

Inserts all data in the configuration files into the database and destroys all
old rows with StorageType `volatile`. The rows created from the configuration
file will have StorageType `nonVolatile`.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `notify.conf`.

# `delete_notify`

```erlang
-spec delete_notify(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a notify definition from the agent config.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-NOTIFICATION-MIB, after this function has been
called, is from the configuration files.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `notify.conf`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
