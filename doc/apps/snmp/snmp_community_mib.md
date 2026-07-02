# `snmp_community_mib`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/snmp/src/agent/snmp_community_mib.erl#L22)

Instrumentation Functions for SNMP-COMMUNITY-MIB

The module `snmp_community_mib` implements the instrumentation functions for the
SNMP-COMMUNITY-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.

# `context_name`

```erlang
-type context_name() :: snmp_framework_mib:admin_string().
```

A human readable string.

`SnmpAdminString`

# `index`

```erlang
-type index() :: snmp_framework_mib:admin_string().
```

`SnmpAdminString (SIZE(1..32))`

# `name`

```erlang
-type name() :: string().
```

`OCTET STRING (SIZE(1..64))`

# `security_name`

```erlang
-type security_name() :: snmp_framework_mib:admin_string().
```

`SnmpAdminString`

# `transport_tag`

```erlang
-type transport_tag() :: snmp_target_mib:tag_value().
```

`SnmpTagValue`

# `add_community`

```erlang
-spec add_community(Idx, CommName, SecName, CtxName, TransportTag) -> {ok, Key} | {error, Reason}
                       when
                           Idx :: index(),
                           CommName :: name(),
                           SecName :: security_name(),
                           CtxName :: context_name(),
                           TransportTag :: transport_tag(),
                           Key :: term(),
                           Reason :: term().
```

# `add_community`
*since OTP R14B03* 

```erlang
-spec add_community(Idx, CommName, SecName, EngineId, CtxName, TransportTag) ->
                       {ok, Key} | {error, Reason}
                       when
                           Idx :: index(),
                           CommName :: name(),
                           SecName :: security_name(),
                           EngineId :: snmp_framework_mib:engine_id(),
                           CtxName :: context_name(),
                           TransportTag :: transport_tag(),
                           Key :: term(),
                           Reason :: term().
```

Adds a community to the agent config. Equivalent to one line in the
`community.conf` file.

With the `EngineId` argument it is possible to override the configured engine-id
(SNMP-FRAMEWORK-MIB).

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
function `config_err/2` of the error, report module and the function fails with
reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `community.conf`.

# `delete_community`

```erlang
-spec delete_community(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a community from the agent config.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-COMMUNITY-MIB, after this function has been called,
is from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `community.conf`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
