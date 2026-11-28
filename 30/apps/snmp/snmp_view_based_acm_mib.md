# `snmp_view_based_acm_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_view_based_acm_mib.erl#L22)

Instrumentation Functions for SNMP-VIEW-BASED-ACM-MIB

The module `snmp_view_based_acm_mib` implements the instrumentation functions
for the SNMP-VIEW-BASED-ACM-MIB, and functions for configuring the database.

The configuration files are described in the SNMP User's Manual.

# `access_notify_view_name`

```erlang
-type access_notify_view_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The value of an instance of this object identifies the MIB view of the SNMP
> context to which this conceptual row authorizes access for notifications."

`SnmpAdminString (SIZE(0..32))`

# `access_read_view_name`

```erlang
-type access_read_view_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The value of an instance of this object identifies the MIB view of the SNMP
> context to which this conceptual row authorizes read access."

`SnmpAdminString (SIZE(0..32))`

# `access_write_view_name`

```erlang
-type access_write_view_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The value of an instance of this object identifies the MIB view of the SNMP
> context to which this conceptual row authorizes write access."

`SnmpAdminString (SIZE(0..32))`

# `context_match`

```erlang
-type context_match() :: exact | prefix.
```

```text
	  exact  - exact match of prefix and contextName
          prefix - Only match to the prefix
```

`INTEGER { exact (1), prefix (2) }`

# `context_prefix`

```erlang
-type context_prefix() :: snmp_framework_mib:admin_string().
```

`SnmpAdminString (SIZE(0..32))`

# `group_name`

```erlang
-type group_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The name of the group to which this entry (e.g., the combination of
> securityModel and securityName) belongs."

`SnmpAdminString (SIZE(1..32))`

# `internal_view_mask`

```erlang
-type internal_view_mask() :: null | [internal_view_mask_element()].
```

# `internal_view_mask_element`

```erlang
-type internal_view_mask_element() :: 0 | 1.
```

# `internal_view_type`

```erlang
-type internal_view_type() :: 1 | 2.
```

# `mibview`

```erlang
-type mibview() :: [{SubTree :: snmp:oid(), Mask :: internal_view_mask(), Type :: internal_view_type()}].
```

# `security_name`

```erlang
-type security_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The securityName for the principal, represented in a Security Model
> independent format."

`SnmpAdminString (SIZE(1..32))`

# `view_mask`

```erlang
-type view_mask() :: [0 | 1].
```

The bit mask which, in combination with the corresponding instance of
vacmViewTreeFamilySubtree, defines a family of view subtrees.

A '1' indicates that an exact match must occur, a '0' indicates 'wild card' (any
sub-identifier value matches).

> #### Note {: .info }
>
> Note that in the "external" format, each bit of each octet is represented by a
> "bit" in this list. That is, each octet "contains" 8 bits; so at most 8\*16 =
> 128 bits in total.

`OCTET STRING (SIZE (0..16))`

# `view_name`

```erlang
-type view_name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "The human readable name for a family of view subtrees."

`SnmpAdminString (SIZE(1..32))`

# `view_type`

```erlang
-type view_type() :: included | excluded.
```

Does the corresponding instances of subtree and mask define a family of view
subtrees which are included in or excluded from the MIB view.

`INTEGER { included(1), excluded(2) }`

# `add_access`

```erlang
-spec add_access(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) ->
                    {ok, Key} | {error, Reason}
                    when
                        GroupName :: group_name(),
                        Prefix :: context_prefix(),
                        SecModel :: snmp_framework_mib:security_model(),
                        SecLevel :: snmp_framework_mib:security_level(),
                        Match :: context_match(),
                        RV :: access_read_view_name(),
                        WV :: access_write_view_name(),
                        NV :: access_notify_view_name(),
                        Key :: term(),
                        Reason :: term().
```

Adds a access definition to the agent config. Equivalent to one vacmAccess-line
in the `vacm.conf` file.

# `add_sec2group`

```erlang
-spec add_sec2group(SecModel, SecName, GroupName) -> {ok, Key} | {error, Reason}
                       when
                           SecModel :: snmp_framework_mib:security_model(),
                           SecName :: security_name(),
                           GroupName :: group_name(),
                           Key :: term(),
                           Reason :: term().
```

Adds a security to group definition to the agent config. Equivalent to one
vacmSecurityToGroup-line in the `vacm.conf` file.

# `add_view_tree_fam`

```erlang
-spec add_view_tree_fam(ViewName, SubTree, Status, Mask) -> {ok, Key} | {error, Reason}
                           when
                               ViewName :: view_name(),
                               SubTree :: snmp:oid(),
                               Status :: view_type(),
                               Mask :: null | view_mask(),
                               Key :: term(),
                               Reason :: term().
```

Adds a view tree family definition to the agent config. Equivalent to one
vacmViewTreeFamily-line in the `vacm.conf` file.

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

The configuration file read is: `vacm.conf`.

# `delete_access`

```erlang
-spec delete_access(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a access definition from the agent config.

# `delete_sec2group`

```erlang
-spec delete_sec2group(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a security to group definition from the agent config.

# `delete_view_tree_fam`

```erlang
-spec delete_view_tree_fam(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a view tree family definition from the agent config.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-VIEW-BASED-ACM-MIB, after this function has been
called, is the data from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function [config_err/2](`snmpa_error:config_err/2`) of the error report module,
and the function fails with the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `vacm.conf`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
