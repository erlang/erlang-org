# `snmp_user_based_sm_mib`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_user_based_sm_mib.erl#L22)

Instrumentation Functions for SNMP-USER-BASED-SM-MIB

The module `snmp_user_based_sm_mib` implements the instrumentation functions for
the SNMP-USER-BASED-SM-MIB, and functions for configuring the database.

Note that authentication has been extended according to RFC 7860
(SNMP-USM-HMAC-SHA2-MIB).

The configuration files are described in the SNMP User's Manual.

# `auth_key`

```erlang
-type auth_key() :: snmp:octet_string().
```

The size/length of the list depends on auth protocol:

```text
               Size any for usmNoAuthProtocol
               Size 16  for usmHMACMD5AuthProtocol
               Size 20  for usmHMACSHAAuthProtocol
               Size 28  for usmHMAC128SHA224AuthProtocol
               Size 32  for usmHMAC192SHA256AuthProtocol
               Size 48  for usmHMAC256SHA384AuthProtocol
	       Size 64  for usmHMAC384SHA512AuthProtocol
```

# `auth_protocol`

```erlang
-type auth_protocol() ::
          usmNoAuthProtocol | usmHMACMD5AuthProtocol | usmHMACSHAAuthProtocol |
          usmHMAC128SHA224AuthProtocol | usmHMAC192SH256AuthProtocol | usmHMAC256SHA384AuthProtocol |
          usmHMAC384SHA512AuthProtocol.
```

> #### Note {: .info }
>
> "An indication of whether messages sent on behalf of this user to/from the
> SNMP engine identified by usmUserEngineID, can be authenticated, and if so,
> the type of authentication protocol which is used."

> #### Note {: .info }
>
> Some of the entries of this type are actually defined by the
> SNMP-USM-HMAC-SHA2-MIB mib.

`AutonomousType`

# `clone_from`

```erlang
-type clone_from() :: zeroDotZero | snmp:row_pointer().
```

> #### Note {: .info }
>
> "A pointer to another conceptual row in this usmUserTable. The user in this
> other conceptual row is called the clone-from user."

`RowPointer`

# `key_change`

```erlang
-type key_change() :: snmp:octet_string().
```

> #### Note {: .info }
>
> "Every definition of an object with this syntax must identify a protocol P, a
> secret key K, and a hash algorithm H that produces output of L octets."

`OCTET STRING`

# `name`

```erlang
-type name() :: snmp_framework_mib:admin_string().
```

> #### Note {: .info }
>
> "A human readable string representing the name of the user. This is the
> (User-based Security) Model dependent security ID."

`SnmpAdminString (SIZE(1..32))`

# `priv_key`

```erlang
-type priv_key() :: snmp:octet_string().
```

The size/length of the list depends on priv protocol:

```text
	       Size any for usmNoPrivProtocol
               Size 16  for usmDESPrivProtocol
               Size 16  for usmAesCfb128Protocol
```

# `priv_protocol`

```erlang
-type priv_protocol() :: usmNoPrivProtocol | usmDESPrivProtocol | usmAesCfb128Protocol.
```

> #### Note {: .info }
>
> "An indication of whether messages sent on behalf of this user to/from the
> SNMP engine identified by usmUserEngineID, can be protected from disclosure,
> and if so, the type of privacy protocol which is used."

> #### Note {: .info }
>
> Some of the entries of this tyype are actually defined by the SNMP-USM-AES-MIB
> mib.

`AutonomousType`

# `public`

```erlang
-type public() :: string().
```

`OCTET STRING (SIZE(0..32))`

# `usm_entry`

```erlang
-type usm_entry() ::
          {EngineID :: snmp_framework_mib:engine_id(),
           UserName :: name(),
           SecName :: snmp_framework_mib:admin_string(),
           Clone :: clone_from(),
           AuthP :: auth_protocol(),
           AuthKeyC :: key_change(),
           OwnAuthKeyC :: key_change(),
           PrivP :: priv_protocol(),
           PrivKeyC :: key_change(),
           OwnPrivKeyC :: key_change(),
           Public :: public(),
           AuthKey :: auth_key(),
           PrivKey :: priv_key()}.
```

# `add_user`

```erlang
-spec add_user(EngineID, Name, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC, PrivP, PrivKeyC,
               OwnPrivKeyC, Public, AuthKey, PrivKey) ->
                  {ok, Key} | {error, Reason}
                  when
                      EngineID :: snmp_framework_mib:engine_id(),
                      Name :: name(),
                      SecName :: snmp_framework_mib:admin_string(),
                      Clone :: clone_from(),
                      AuthP :: auth_protocol(),
                      AuthKeyC :: key_change(),
                      OwnAuthKeyC :: key_change(),
                      PrivP :: priv_protocol(),
                      PrivKeyC :: key_change(),
                      OwnPrivKeyC :: key_change(),
                      Public :: public(),
                      AuthKey :: auth_key(),
                      PrivKey :: priv_key(),
                      Key :: term(),
                      Reason :: term().
```

Adds a USM security data (user) to the agent config. Equivalent to one line in
the `usm.conf` file.

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

The configuration file read is: `usm.conf`.

# `delete_user`

```erlang
-spec delete_user(Key) -> ok | {error, Reason} when Key :: term(), Reason :: term().
```

Delete a USM security data (user) from the agent config.

# `reconfigure`

```erlang
-spec reconfigure(ConfDir) -> snmp:void() when ConfDir :: string().
```

Inserts all data in the configuration files into the database and destroys all
old data, including the rows with StorageType `nonVolatile`. The rows created
from the configuration file will have StorageType `nonVolatile`.

Thus, the data in the SNMP-USER-BASED-SM-MIB, after this function has been
called, is the data from the configuration files.

All `snmp` counters are set to zero.

If an error is found in the configuration file, it is reported using the
function `config_err/2` of the error report module, and the function fails with
the reason `configuration_error`.

`ConfDir` is a string which points to the directory where the configuration
files are found.

The configuration file read is: `usm.conf`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
