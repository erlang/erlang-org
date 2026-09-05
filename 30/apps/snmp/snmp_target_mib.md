# `snmp_target_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_target_mib.erl#L22)

Instrumentation Functions for SNMP-TARGET-MIB

The module `snmp_target_mib` implements the instrumentation functions for the
SNMP-TARGET-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.

Legacy API functions [`add_addr/10`](`add_addr/10`) that does not specify
transport domain, and `add_addr/11` that has got separate `IpAddr` and
`PortNumber` arguments still work as before for backwards compatibility reasons.

## DATA TYPES

See the [data types in `snmpa_conf`](`m:snmpa_conf#types`).

# `mms`

```erlang
-type mms() :: 484..65535.
```

> #### Note {: .info }
>
> "The maximum message size value associated with an entry in the
> snmpTargetAddrTable.".

`Integer32 (484..65535)`

# `name`

```erlang
-type name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The locally arbitrary, but unique identifier associated with this
> snmpTargetAddrEntry."

`SnmpAdminString (SIZE(1..32))`

# `params`

```erlang
-type params() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The value of this object identifies an entry in the snmpTargetParamsTable."

`SnmpAdminString (SIZE(1..32))`

# `retry_count`

```erlang
-type retry_count() :: 0..255.
```

> #### Note {: .info }
>
> "This object specifies a default number of retries to be attempted when a
> response is not received for a generated message."

`Integer32 (0..255)`

# `tag_list`

```erlang
-type tag_list() :: string().
```

> #### Note {: .info }
>
> "An octet string containing a list of tag values. Tag values are preferably in
> human-readable form."
>
> "To facilitate internationalization, this information is represented using the
> ISO/IEC IS 10646-1 character set, encoded as an octet string using the UTF-8
> character encoding scheme described in RFC 2279."

`OCTET STRING (SIZE (0..255))`

# `tag_value`

```erlang
-type tag_value() :: string().
```

> #### Note {: .info }
>
> "An octet string containing a tag value. Tag values are preferably in
> human-readable form."

`OCTET STRING (SIZE (0..255))`

# `tmask`

```erlang
-type tmask() :: snmpa_conf:transportAddressMask().
```

> #### Note {: .info }
>
> "The mask value associated with an entry in the snmpTargetAddrTable. The value
> of this object must have the same length as the corresponding instance of
> snmpTargetAddrTAddress, or must have length 0."

`OCTET STRING (SIZE (0..255))`

# `add_addr`

```erlang
-spec add_addr(Name, TDomain, TAddr, Timeout, Retry, TagList, Params, EngineId, TMask, MMS) ->
                  {ok, Key} | {error, Reason}
                  when
                      Name :: name(),
                      TDomain :: snmpa_conf:transportDomain(),
                      TAddr :: snmpa_conf:transportAddress(),
                      Timeout :: snmp:time_interval(),
                      Retry :: integer(),
                      TagList :: tag_list(),
                      Params :: params(),
                      EngineId :: snmp_framework_mib:engine_id(),
                      TMask :: tmask(),
                      MMS :: snmp_framework_mib:max_message_size(),
                      Key :: term(),
                      Reason :: term();
              (Name, Ip, Port, Timeout, Retry, TagList, Params, EngineId, TMask, MMS) ->
                  {ok, Key} | {error, Reason}
                  when
                      Name :: name(),
                      Ip :: snmpa_conf:transportAddressWithoutPort(),
                      Port :: inet:port_number(),
                      Timeout :: snmp:time_interval(),
                      Retry :: integer(),
                      TagList :: tag_list(),
                      Params :: params(),
                      EngineId :: snmp_framework_mib:engine_id(),
                      TMask :: tmask(),
                      MMS :: snmp_framework_mib:max_message_size(),
                      Key :: term(),
                      Reason :: term().
```

Adds a target address definition to the agent config. Equivalent to one line in
the `target_addr.conf` file.

# `add_params`

```erlang
-spec add_params(Name, MPModel, SecModel, SecName, SecLevel) -> {ok, Key} | {error, Reason}
                    when
                        Name :: name(),
                        MPModel :: snmp_framework_mib:message_processing_model(),
                        SecModel :: snmp_framework_mib:security_model(),
                        SecName :: snmp_framework_mib:admin_string(),
                        SecLevel :: snmp_framework_mib:security_level(),
                        Key :: term(),
                        Reason :: term().
```

Adds a target parameter definition to the agent config. Equivalent to one line
in the `target_params.conf` file.

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

The configuration files read are: `target_addr.conf` and `target_params.conf`.

# `delete_addr`

```erlang
-spec delete_addr(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a target address definition from the agent config.

# `delete_params`

```erlang
-spec delete_params(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a target parameter definition from the agent config.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-TARGET-MIB, after this function has been called, is
the data from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the , and the function fails with the reason
`configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration files read are: `target_addr.conf` and `target_params.conf`.

# `set_target_engine_id`

```erlang
-spec set_target_engine_id(TargetAddrName, EngineId) -> boolean()
                              when TargetAddrName :: name(), EngineId :: snmp_framework_mib:engine_id().
```

Changes the engine id for a target in the `snmpTargetAddrTable`. If
notifications are sent as Inform requests to a target, its engine id must be
set.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
