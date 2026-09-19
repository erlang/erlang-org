# `snmp_framework_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_framework_mib.erl#L22)

Instrumentation Functions for SNMP-FRAMEWORK-MIB

The module `snmp_framework_mib` implements instrumentation functions for the
SNMP-FRAMEWORK-MIB, and functions for initializing and configuring the database.

The configuration files are described in the SNMP User's Manual.

# `admin_string`

```erlang
-type admin_string() :: string().
```

`OCTET STRING (SIZE(0..255))`

# `engine_id`

```erlang
-type engine_id() :: string().
```

`OCTET STRING (SIZE(5..32))`

# `max_message_size`

```erlang
-type max_message_size() :: 484..2147483647.
```

> #### Note {: .info }
>
> "The maximum length in octets of an SNMP message which this SNMP engine can
> send or receive and process, determined as the minimum of the maximum message
> size values supported among all of the transports available to and supported
> by the engine."

`INTEGER (484..2147483647)`

# `message_processing_model`

```erlang
-type message_processing_model() :: v1 | v2c | v3.
```

> #### Note {: .info }
>
> "As of this writing, there are several values of messageProcessingModel
> defined for use with SNMP. They are as follows: "
>
> ```text
>                         0  reserved for SNMPv1
>                         1  reserved for SNMPv2c
>                         2  reserved for SNMPv2u and SNMPv2*
>                         3  reserved for SNMPv3
> ```

`INTEGER(0 .. 2147483647)`

# `security_level`

```erlang
-type security_level() :: noAuthNoPriv | authNoPriv | authPriv.
```

> #### Note {: .info }
>
> "A Level of Security at which SNMP messages can be sent or with which
> operations are being processed; in particular, one of: "
>
> ```text
>                       noAuthNoPriv - without authentication and
>                                      without privacy,
>                       authNoPriv   - with authentication but
>                                      without privacy,
>                       authPriv     - with authentication and
>                                      with privacy.
> ```
>
> "These three values are ordered such that noAuthNoPriv is less than authNoPriv
> and authNoPriv is less than authPriv."

`INTEGER { noAuthNoPriv(1), authNoPriv(2), authPriv(3) }`

# `security_model`

```erlang
-type security_model() :: any | v1 | v2c | usm.
```

> #### Note {: .info }
>
> "As of this writing, there are several values of securityModel defined for use
> with SNMP or reserved for use with supporting MIB objects. They are as
> follows: "
>
> ```text
>                         0  reserved for 'any'
>                         1  reserved for SNMPv1
>                         2  reserved for SNMPv2c
>                         3  User-Based Security Model (USM)
> ```

`INTEGER(0 .. 2147483647)`

# `add_context`

```erlang
-spec add_context(Ctx) -> {ok, Key} | {error, Reason}
                     when Ctx :: string(), Key :: term(), Reason :: term().
```

Adds a context to the agent config. Equivalent to one line in the `context.conf`
file.

# `configure`

```erlang
-spec configure(ConfDir) -> snmp:void() when ConfDir :: string().
```

This function is called from the supervisor at system start-up.

Inserts all data in the configuration files into the database and destroys all
old data.

Thus, the data in the SNMP-FRAMEWORK-MIB, after this function has been called,
is from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `context.conf`.

# `delete_context`

```erlang
-spec delete_context(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a context from the agent config.

# `init`

```erlang
-spec init() -> snmp:void().
```

This function is called from the supervisor at system start-up.

Creates the necessary objects in the database if they do not exist. It does not
destroy any old values.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
