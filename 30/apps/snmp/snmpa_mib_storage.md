# `snmpa_mib_storage`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_mib_storage.erl#L23)

Behaviour module for the SNMP agent mib storage.

This module defines the behaviour of the SNMP agent mib storage.

The mib storage is used by the agent to store internal mib- related information.
The mib storage module is used by several entities, not just the mib-server.

A `snmpa_mib_storage` compliant module must export the following functions:

- [open/5](`c:snmpa_mib_storage:open/5`)
- [close/1](`c:snmpa_mib_storage:close/1`)
- [read/2](`c:snmpa_mib_storage:read/2`)
- [write/2](`c:snmpa_mib_storage:write/2`)
- [delete/1](`c:snmpa_mib_storage:delete/1`)
- [delete/2](`c:snmpa_mib_storage:delete/2`)
- [match_object/2](`c:snmpa_mib_storage:match_object/2`)
- [match_delete/2](`c:snmpa_mib_storage:match_delete/2`)
- [tab2list/1](`c:snmpa_mib_storage:tab2list/1`)
- [info/1](`c:snmpa_mib_storage:info/1`)
- [info/2](`c:snmpa_mib_storage:info/2`)
- [sync/1](`c:snmpa_mib_storage:sync/1`)
- [backup/2](`c:snmpa_mib_storage:backup/2`)

The semantics of them and their exact signatures are explained below.

# `mib_storage_fields`
*since OTP R16B01* 

```erlang
-type mib_storage_fields() :: [atom()].
```

# `mib_storage_table_id`
*since OTP R16B01* 

```erlang
-type mib_storage_table_id() :: term().
```

# `mib_storage_table_type`
*since OTP R16B01* 

```erlang
-type mib_storage_table_type() :: set | bag.
```

# `backup`
*since OTP R16B01* 

```erlang
-callback backup(TabId :: mib_storage_table_id(), Dir :: file:filename()) -> ok | {error, Reason :: term()}.
```

Perform a backup of the mib-storage table.

What this means, if anything, is implementation dependent.

# `close`
*since OTP R16B01* 

```erlang
-callback close(TabId :: mib_storage_table_id()) -> term().
```

Close the mib-storage table.

# `delete`
*since OTP R16B01* 

```erlang
-callback delete(TabId :: mib_storage_table_id()) -> snmp:void().
```

Delete an entire mib-storage table.

# `delete`
*since OTP R16B01* 

```erlang
-callback delete(TabId :: mib_storage_table_id(), Key :: term()) -> ok | {error, Reason :: term()}.
```

Delete a record from the mib-storage table.

# `info`
*since OTP R16B01* 

```erlang
-callback info(TabId :: mib_storage_table_id()) -> Info :: term().
```

Retrieve implementation dependent mib-storage table information.

# `info`
*since OTP R16B01* 

```erlang
-callback info(TabId :: mib_storage_table_id(), Item :: atom()) -> Info :: term().
```

# `match_delete`
*since OTP R16B01* 

```erlang
-callback match_delete(TabId :: mib_storage_table_id(), Pattern :: ets:match_pattern()) ->
                          Recs :: [tuple()] | {error, Reason :: term()}.
```

Search the mib-storage table for record that match the specified pattern and
then delete them. The records deleted are also returned.

# `match_object`
*since OTP R16B01* 

```erlang
-callback match_object(TabId :: mib_storage_table_id(), Pattern :: ets:match_pattern()) ->
                          Recs :: [tuple()] | {error, Reason :: term()}.
```

Search the mib-storage table for record that match the specified pattern.

# `open`
*since OTP R16B01* 

```erlang
-callback open(Name :: atom(),
               RecName :: atom(),
               Fields :: mib_storage_fields(),
               Type :: mib_storage_table_type(),
               Options :: list()) ->
                  {ok, TabId :: mib_storage_table_id()} | {error, Reason :: term()}.
```

Create or open a mib storage table.

Note that the `RecordName` and `Fields` arguments my not be used in all
implementations (they are actually only needed for mnesia-based
implementations).

Note also that the `Options` argument comes from the `options` config option of
the mib-storage config option, and is passed on as is.

# `read`
*since OTP R16B01* 

```erlang
-callback read(TabId :: mib_storage_table_id(), Key :: term()) -> false | {value, Record :: tuple()}.
```

Read a record from the mib-storage table.

# `sync`
*since OTP R16B01* 

```erlang
-callback sync(TabId :: mib_storage_table_id()) -> snmp:void().
```

Synchronize the mib-storage table.

What this means, if anything, is implementation dependent.

# `tab2list`
*since OTP R16B01* 

```erlang
-callback tab2list(TabId :: mib_storage_table_id()) -> [tuple()].
```

Return all records in the mib-storage table in the form of a list.

# `write`
*since OTP R16B01* 

```erlang
-callback write(TabId :: mib_storage_table_id(), Record :: tuple()) -> ok | {error, Reason :: term()}.
```

Write a record to the mib-storage table.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
