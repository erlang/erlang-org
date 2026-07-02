# `mnesia`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/mnesia/src/mnesia.erl#L26)

A distributed key-value DBMS

The following are some of the most important and attractive capabilities
provided by Mnesia:

- A relational/object hybrid data model that is suitable for telecommunications
  applications.
- A DBMS query language, Query List Comprehension (QLC) as an add-on library.
- Persistence. Tables can be coherently kept on disc and in the main memory.
- Replication. Tables can be replicated at several nodes.
- Atomic transactions. A series of table manipulation operations can be grouped
  into a single atomic transaction.
- Location transparency. Programs can be written without knowledge of the actual
  data location.
- Extremely fast real-time data searches.
- Schema manipulation routines. The DBMS can be reconfigured at runtime without
  stopping the system.

This Reference Manual describes the Mnesia API. This includes functions that
define and manipulate Mnesia tables.

All functions in this Reference Manual can be used in any combination with
queries using the list comprehension notation. For information about the query
notation, see the `m:qlc` manual page in STDLIB.

Data in Mnesia is organized as a set of tables. Each table has a name that must
be an atom. Each table is made up of Erlang records. The user is responsible for
the record definitions. Each table also has a set of properties. The following
are some of the properties that are associated with each table:

- `type`. Each table can have `set`, `ordered_set`, or `bag` semantics. Notice
  that currently `ordered_set` is not supported for `disc_only_copies`.

  If a table is of type `set`, each key leads to either one or zero records.

  If a new item is inserted with the same key as an existing record, the old
  record is overwritten. However, if a table is of type `bag`, each key can map
  to several records. All records in type `bag` tables are unique, only the keys
  can be duplicated.

- `record_name`. All records stored in a table must have the same name. The
  records must be instances of the same record type.
- `ram_copies`. A table can be replicated on a number of Erlang nodes. Property
  `ram_copies` specifies a list of Erlang nodes where RAM copies are kept. These
  copies can be dumped to disc at regular intervals. However, updates to these
  copies are not written to disc on a transaction basis.
- `disc_copies`. This property specifies a list of Erlang nodes where the table
  is kept in RAM and on disc. All updates of the table are performed in the
  actual table and are also logged to disc. If a table is of type `disc_copies`
  at a certain node, the entire table is resident in RAM memory and on disc.
  Each transaction performed on the table is appended to a `LOG` file and
  written into the RAM table.
- `disc_only_copies`. Some, or all, table replicas can be kept on disc only.
  These replicas are considerably slower than the RAM-based replicas.
- `index`. This is a list of attribute names, or integers, which specify the
  tuple positions on which Mnesia is to build and maintain an extra index table.
- `local_content`. When an application requires tables whose contents are local
  to each node, `local_content` tables can be used. The table name is known to
  all Mnesia nodes, but its content is unique on each node. This means that
  access to such a table must be done locally. Set field `local_content` to
  `true` to enable the `local_content` behavior. Default is `false`.
- `majority`. This attribute is `true` or `false`; default is `false`. When
  `true`, a majority of the table replicas must be available for an update to
  succeed. Majority checking can be enabled on tables with mission-critical
  data, where it is vital to avoid inconsistencies because of network splits.
- `snmp`. Each (set-based) Mnesia table can be automatically turned into a
  Simple Network Management Protocol (SNMP) ordered table as well. This property
  specifies the types of the SNMP keys.
- `attributes`. The names of the attributes for the records that are inserted in
  the table.

For information about the complete set of table properties and their details,
see `mnesia:create_table/2`.

This Reference Manual uses a table of persons to illustrate various examples.
The following record definition is assumed:

```erlang
-record(person, {name,
                 age = 0,
                 address = unknown,
                 salary = 0,
                 children = []}),
```

The first record attribute is the primary key, or key for short.

The function descriptions are sorted in alphabetical order. It is recommended to
start to read about `mnesia:create_table/2`, `mnesia:lock/2`, and
 `mnesia:activity/4` before you continue and learn about the rest.

Writing or deleting in transaction-context creates a local copy of each modified
record during the transaction. During iteration, that is, `mnesia:foldl/4`,
`mnesia:foldr/4`, `mnesia:next/2`, `mnesia:prev/2`, and `mnesia:snmp_get_next_index/2`, Mnesia
compensates for every written or deleted record, which can reduce the
performance.

If possible, avoid writing or deleting records in the same transaction before
iterating over the table.

## Configuration Parameters

[](){: #configuration_parameters }

Mnesia reads the following application configuration parameters:

- `-mnesia access_module Module`. The name of the Mnesia activity access
  callback module. Default is `mnesia`.
- `-mnesia auto_repair true | false`. This flag controls if Mnesia automatically
  tries to repair files that have not been properly closed. Default is `true`.
- `-mnesia backup_module Module`. The name of the Mnesia backup callback module.
  Default is `mnesia_backup`.
- `-mnesia debug Level`. Controls the debug level of Mnesia. The possible values
  are as follows:

  - **`none`** - No trace outputs. This is the default.

  - **`verbose`** - Activates tracing of important debug events. These events
    generate `{mnesia_info, Format, Args}` system events. Processes can
    subscribe to these events with `mnesia:subscribe/1`. The events are always
    sent to the Mnesia event handler.

  - **`debug`** - Activates all events at the verbose level plus full trace of
    all debug events. These debug events generate `{mnesia_info, Format, Args}`
    system events. Processes can subscribe to these events with
    `mnesia:subscribe/1`. The events are always sent to the Mnesia event
    handler. On this debug level, the Mnesia event handler starts subscribing to
    updates in the schema table.

  - **`trace`** - Activates all events at the debug level. On this level, the
    Mnesia event handler starts subscribing to updates on all Mnesia tables.
    This level is intended only for debugging small toy systems, as many large
    events can be generated.

  - **`false`** - An alias for none.

  - **`true`** - An alias for debug.

- `-mnesia core_dir Directory`. The name of the directory where Mnesia core
  files is stored, or false. Setting it implies that also RAM-only nodes
  generate a core file if a crash occurs.
- `-mnesia dc_dump_limit Number`. Controls how often `disc_copies` tables are
  dumped from memory. Tables are dumped when
  `filesize(Log) > (filesize(Tab)/Dc_dump_limit)`. Lower values reduce CPU
  overhead but increase disk space and startup times. Default is 4.
- `-mnesia dir Directory`. The name of the directory where all Mnesia data is
  stored. The directory name must be unique for the current node. Two nodes must
  never share the the same Mnesia directory. The results are unpredictable.
- `-mnesia dump_disc_copies_at_startup true | false`. If set to false, this
  disables the dumping of `disc_copies` tables during startup while tables are
  being loaded. The default is true.
- `-mnesia dump_log_load_regulation true | false`. Controls if log dumps are to
  be performed as fast as possible, or if the dumper is to do its own load
  regulation. Default is `false`.

  This feature is temporary and will be removed in a future release

- `-mnesia dump_log_update_in_place true | false`. Controls if log dumps are
  performed on a copy of the original data file, or if the log dump is performed
  on the original data file. Default is `true`
- [](){: #dump_log_write_threshold } `-mnesia dump_log_write_threshold Max`.
  `Max` is an integer that specifies the maximum number of writes allowed to the
  transaction log before a new dump of the log is performed. Default is `1000`
  log writes.
- [](){: #dump_log_time_threshold } `-mnesia dump_log_time_threshold Max`. `Max`
  is an integer that specifies the dump log interval in milliseconds. Default is
  3 minutes. If a dump has not been performed within `dump_log_time_threshold`
  milliseconds, a new dump is performed regardless of the number of writes
  performed.
- `-mnesia event_module Module`. The name of the Mnesia event handler callback
  module. Default is `mnesia_event`.
- `-mnesia extra_db_nodes Nodes` specifies a list of nodes, in addition to the
  ones found in the schema, with which Mnesia is also to establish contact.
  Default is `[]` (empty list).
- `-mnesia fallback_error_function {UserModule, UserFunc}`. Specifies a
  user-supplied callback function, which is called if a fallback is installed
  and Mnesia goes down on another node. Mnesia calls the function with one
  argument, the name of the dying node, for example,
  `UserModule:UserFunc(DyingNode)`. Mnesia must be restarted, otherwise the
  database can be inconsistent. The default behavior is to terminate Mnesia.
- `-mnesia max_wait_for_decision Timeout`. Specifies how long Mnesia waits for
  other nodes to share their knowledge about the outcome of an unclear
  transaction. By default, `Timeout` is set to the atom `infinity`. This implies
  that if Mnesia upon startup detects a "heavyweight transaction" whose outcome
  is unclear, the local Mnesia waits until Mnesia is started on some (in the
  worst case all) of the other nodes that were involved in the interrupted
  transaction. This is a rare situation, but if it occurs, Mnesia does not guess
  if the transaction on the other nodes was committed or terminated. Mnesia
  waits until it knows the outcome and then acts accordingly.

  If `Timeout` is set to an integer value in milliseconds, Mnesia forces
  "heavyweight transactions" to be finished, even if the outcome of the
  transaction for the moment is unclear. After `Timeout` milliseconds, Mnesia
  commits or terminates the transaction and continues with the startup. This can
  lead to a situation where the transaction is committed on some nodes and
  terminated on other nodes. If the transaction is a schema transaction, the
  inconsistency can be fatal.

- `-mnesia no_table_loaders NUMBER`. Specifies the number of parallel table
  loaders during start. More loaders can be good if the network latency is high
  or if many tables contain few records. Default is `2`.
- `-mnesia send_compressed Level`. Specifies the level of compression to be used
  when copying a table from the local node to another one. Default is `0`.

  `Level` must be an integer in the interval `[0, 9]`, where `0` means no
  compression and `9` means maximum compression. Before setting it to a non-zero
  value, ensure that the remote nodes understand this configuration.

- `-mnesia max_transfer_size Number`. Specifies the estimated size in bytes of a
  single packet of data to be used when copying a table from the local node to
  another one. Default is `64000`.
- `-mnesia schema_location Loc`. Controls where Mnesia looks for its schema.
  Parameter `Loc` can be one of the following atoms:

  - **`disc`** - Mandatory disc. The schema is assumed to be located in the
    Mnesia directory. If the schema cannot be found, Mnesia refuses to start.
    This is the old behavior.

  - **`ram`** - Mandatory RAM. The schema resides in RAM only. At startup, a
    tiny new schema is generated. This default schema only contains the
    definition of the schema table and only resides on the local node. Since no
    other nodes are found in the default schema, configuration parameter
    `extra_db_nodes` must be used to let the node share its table definitions
    with other nodes.

    Parameter `extra_db_nodes` can also be used on disc based nodes.

  - **`opt_disc`** - Optional disc. The schema can reside on disc or in RAM. If
    the schema is found on disc, Mnesia starts as a disc-based node and the
    storage type of the schema table is `disc_copies`. If no schema is found on
    disc, Mnesia starts as a disc-less node and the storage type of the schema
    table is `ram_copies`. Default value for the application parameter is
    `opt_disc`.

First, the SASL application parameters are checked, then the command-line flags
are checked, and finally, the default value is chosen.

### See Also

`m:application`, `m:dets`, `m:disk_log`, `m:ets`, `m:qlc`

# `activity`
*not exported* 

```erlang
-type activity() ::
          ets | async_dirty | sync_dirty | transaction | sync_transaction |
          {transaction, Retries :: non_neg_integer()} |
          {sync_transaction, Retries :: non_neg_integer()}.
```

# `change_frag_prop`
*not exported* 

```erlang
-type change_frag_prop() ::
          deactivate |
          {activate, [frag_prop()]} |
          {add_frag, [node()] | [{node(), non_neg_integer()}]} |
          del_frag |
          {add_node, node()} |
          {del_node, node()}.
```

# `config_key`
*not exported* 

```erlang
-type config_key() :: extra_db_nodes | dc_dump_limit.
```

# `config_result`
*not exported* 

```erlang
-type config_result() :: {ok, config_value()} | {error, term()}.
```

# `config_value`
*not exported* 

```erlang
-type config_value() :: [node()] | number().
```

# `create_option`
*not exported* 

```erlang
-type create_option() ::
          {access_mode, read_write | read_only} |
          {attributes, [atom()]} |
          {disc_copies, [node()]} |
          {disc_only_copies, [node()]} |
          {index, [index_attr()]} |
          {load_order, non_neg_integer()} |
          {majority, boolean()} |
          {ram_copies, [node()]} |
          {record_name, atom()} |
          {snmp, SnmpStruct :: term()} |
          {storage_properties, [{Backend :: module(), [BackendProp :: _]}]} |
          {type, set | ordered_set | bag} |
          {local_content, boolean()} |
          {user_properties, proplists:proplist()} |
          {frag_properties, [frag_prop()]}.
```

# `debug_level`
*not exported* 

```erlang
-type debug_level() :: none | verbose | debug | trace.
```

# `frag_prop`
*not exported* 

```erlang
-type frag_prop() ::
          {n_fragments, pos_integer()} |
          {node_pool, [node()]} |
          {n_ram_copies, non_neg_integer()} |
          {n_disc_copies, non_neg_integer()} |
          {n_disc_only_copies, non_neg_integer()} |
          {foreign_key, undefined | {table(), atom()}} |
          {hash_module, atom()} |
          {hash_state, term()}.
```

# `index_attr`
*not exported* 

```erlang
-type index_attr() :: atom() | non_neg_integer() | {atom()}.
```

# `lock_kind`
*not exported* 

```erlang
-type lock_kind() :: write_locks() | read_locks().
```

# `read_locks`
*not exported* 

```erlang
-type read_locks() :: read.
```

# `result`
*not exported* 

```erlang
-type result() :: ok | {error, Reason :: term()}.
```

# `select_continuation`
*not exported* 

```erlang
-type select_continuation() :: term().
```

# `snmp_struct`
*not exported* 

```erlang
-type snmp_struct() :: [{atom(), snmp_type() | tuple_of(snmp_type())}].
```

# `snmp_type`
*not exported* 

```erlang
-type snmp_type() :: fix_string | string | integer.
```

# `storage_type`
*not exported* 

```erlang
-type storage_type() :: ram_copies | disc_copies | disc_only_copies.
```

# `t_result`
*not exported* 

```erlang
-type t_result(Res) :: {atomic, Res} | {aborted, Reason :: term()}.
```

# `table`
*not exported* 

```erlang
-type table() :: atom().
```

# `tuple_of`
*not exported* 

```erlang
-type tuple_of(_T) :: tuple().
```

# `write_locks`
*not exported* 

```erlang
-type write_locks() :: write | sticky_write.
```

# `abort`

```erlang
-spec abort(Reason :: term()) -> no_return().
```

Terminate the current transaction.

Makes the transaction silently return the tuple `{aborted, Reason}`. Termination
of a Mnesia transaction means that an exception is thrown to an enclosing
`catch`. Thus, the expression `catch mnesia:abort(x)` does not terminate the
transaction.

# `activate_checkpoint`

```erlang
-spec activate_checkpoint([Arg]) -> {ok, Name, [node()]} | {error, Reason :: term()}
                             when
                                 Arg ::
                                     {name, Name} |
                                     {max, [table()]} |
                                     {min, [table()]} |
                                     {allow_remote, boolean()} |
                                     {ram_overrides_dump, boolean()}.
```

Activate a checkpoint.

A checkpoint is a consistent view of the system. A checkpoint can be activated
on a set of tables. This checkpoint can then be traversed and presents a view of
the system as it existed at the time when the checkpoint was activated, even if
the tables are being or have been manipulated.

`Args` is a list of the following tuples:

- `{name,Name}`. `Name` is the checkpoint name. Each checkpoint must have a name
  that is unique to the associated nodes. The name can be reused only once the
  checkpoint has been deactivated. By default, a name that is probably unique is
  generated.
- `{max,MaxTabs}`. `MaxTabs` is a list of tables that are to be included in the
  checkpoint. Default is `[]`. For these tables, the redundancy is maximized and
  checkpoint information is retained together with all replicas. The checkpoint
  becomes more fault tolerant if the tables have several replicas. When a new
  replica is added by the schema manipulation function
  `mnesia:add_table_copy/3`, a retainer is also attached automatically.
- `{min,MinTabs}`. `MinTabs` is a list of tables that are to be included in the
  checkpoint. Default is []. For these tables, the redundancy is minimized and
  the checkpoint information is only retained with one replica, preferably on
  the local node.
- `{allow_remote,Bool}`. `false` means that all retainers must be local. The
  checkpoint cannot be activated if a table does not reside locally. `true`
  allows retainers to be allocated on any node. Default is `true`.
- `{ram_overrides_dump,Bool}`. Only applicable for `ram_copies`. `Bool` allows
  you to choose to back up the table state as it is in RAM, or as it is on disc.
  `true` means that the latest committed records in RAM are to be included in
  the checkpoint. These are the records that the application accesses. `false`
  means that the records dumped to `DAT` files are to be included in the
  checkpoint. These records are loaded at startup. Default is `false`.

Returns `{ok,Name,Nodes}` or `{error,Reason}`. `Name` is the (possibly
generated) checkpoint name. `Nodes` are the nodes that are involved in the
checkpoint. Only nodes that keep a checkpoint retainer know about the
checkpoint.

# `activity`

```erlang
-spec activity(AccessContext, Fun) -> t_result(Res) | Res
                  when AccessContext :: activity(), Fun :: fun(() -> Res).
```

Execute `Fun` in `AccessContext`.

Calls [`mnesia:activity(AccessContext, Fun, Args, AccessMod)`](`activity/4`), where `AccessMod`
is the default access callback module obtained by
`mnesia:system_info(access_module)`. `Args` defaults to `[]` (empty list).

# `activity`

```erlang
-spec activity(AccessContext, Fun, Args, AccessMod) -> t_result(Res) | Res
                  when
                      AccessContext :: activity(),
                      Args :: [Arg :: _],
                      Fun :: fun((...) -> Res),
                      AccessMod :: atom().
```

Execute `Fun` in `AccessContext`.

Executes the functional object `Fun` with argument `Args`.

The code that executes inside the activity can consist of a series of table
manipulation functions, which are performed in an `AccessContext`. Currently,
the following access contexts are supported:

- **`transaction`** - Short for `{transaction, infinity}`

- **`{transaction, Retries}`** - Calls [`mnesia:transaction(Fun, Args, Retries)`](`transaction/3`).
  Notice that the result from `Fun` is returned if the transaction is successful
  (atomic), otherwise the function exits with an abort reason.

- **`sync_transaction`** - Short for `{sync_transaction, infinity}`

- **`{sync_transaction, Retries}`** - Calls
  [`mnesia:sync_transaction(Fun, Args, Retries)`](`sync_transaction/3`). Notice that the result from
  `Fun` is returned if the transaction is successful (atomic), otherwise the
  function exits with an abort reason.

- **`async_dirty`** - Calls `mnesia:async_dirty(Fun, Args)`.

- **`sync_dirty`** - Calls `mnesia:sync_dirty(Fun, Args)`.

- **`ets`** - Calls `mnesia:ets(Fun, Args)`.

This function (`mnesia:activity/4`) differs in an important way from the
functions `mnesia:transaction/3`, `mnesia:sync_transaction/3`, `mnesia:async_dirty/2`,
`mnesia:sync_dirty/2`, and `mnesia:ets/2`. Argument `AccessMod` is the name of a
callback module, which implements the `mnesia_access` behavior.

Mnesia forwards calls to the following functions:

- mnesia:lock/2 (read_lock_table/1, write_lock_table/1)
- mnesia:write/3 (write/1, s_write/1)
- mnesia:delete/3 (delete/1, s_delete/1)
- mnesia:delete_object/3 (delete_object/1, s_delete_object/1)
- mnesia:read/3 (read/1, wread/1)
- mnesia:match_object/3 (match_object/1)
- mnesia:all_keys/1
- mnesia:first/1
- mnesia:last/1
- mnesia:prev/2
- mnesia:next/2
- mnesia:index_match_object/4 (index_match_object/2)
- mnesia:index_read/3
- mnesia:table_info/2
- mnesia:foldl/4 (foldl/3)
- mnesia:foldr/4 (foldr/3)
- mnesia:select/3 (select/2)
- mnesia:select/4
- mnesia:select/1
- mnesia:select_reverse/3 (select_reverse/2)
- mnesia:select_reverse/4
- mnesia:clear_table/1

to the corresponding:

- AccessMod:lock(ActivityId, Opaque, LockItem, LockKind)
- AccessMod:write(ActivityId, Opaque, Tab, Rec, LockKind)
- AccessMod:delete(ActivityId, Opaque, Tab, Key, LockKind)
- AccessMod:delete_object(ActivityId, Opaque, Tab, RecXS, LockKind)
- AccessMod:read(ActivityId, Opaque, Tab, Key, LockKind)
- AccessMod:match_object(ActivityId, Opaque, Tab, Pattern, LockKind)
- AccessMod:all_keys(ActivityId, Opaque, Tab, LockKind)
- AccessMod:first(ActivityId, Opaque, Tab)
- AccessMod:last(ActivityId, Opaque, Tab)
- AccessMod:prev(ActivityId, Opaque, Tab, Key)
- AccessMod:next(ActivityId, Opaque, Tab, Key)
- AccessMod:index_match_object(ActivityId, Opaque, Tab, Pattern, Attr, LockKind)
- AccessMod:index_read(ActivityId, Opaque, Tab, SecondaryKey, Attr, LockKind)
- AccessMod:table_info(ActivityId, Opaque, Tab, InfoItem)
- AccessMod:foldl(ActivityId, Opaque, Fun, Acc, Tab, LockKind)
- AccessMod:foldr(ActivityId, Opaque, Fun, Acc, Tab, LockKind)
- AccessMod:select(ActivityId, Opaque, Tab, Spec, LockKind)
- AccessMod:select(ActivityId, Opaque, Tab, Spec, NObjects, LockKind)
- AccessMod:select_cont(ActivityId, Opaque, Cont)
- AccessMod:select_reverse(ActivityId, Opaque, Tab, Spec, LockKind)
- AccessMod:select_reverse(ActivityId, Opaque, Tab, Spec, NObjects, LockKind)
- AccessMod:clear_table(ActivityId, Opaque, Tab, Obj)

`ActivityId` is a record that represents the identity of the enclosing Mnesia
activity. The first field (obtained with
[`element(1, ActivityId)`](`element/2`)) contains an atom, which can be
interpreted as the activity type: `ets`, `async_dirty`, `sync_dirty`, or `tid`.
`tid` means that the activity is a transaction. The structure of the rest of the
identity record is internal to Mnesia.

`Opaque` is an opaque data structure that is internal to Mnesia.

# `add_table_copy`

```erlang
-spec add_table_copy(Tab, Node, Type) -> t_result(ok)
                        when Tab :: table(), Node :: node(), Type :: storage_type().
```

Copy a table to a remote node.

Makes another copy of a table at the node `Node`. Argument `Type` must be either
of the atoms `ram_copies`, `disc_copies`, or `disc_only_copies`. For example,
the following call ensures that a disc replica of the `person` table also exists
at node `Node`:

```text
mnesia:add_table_copy(person, Node, disc_copies)
```

This function can also be used to add a replica of the table named `schema`.

# `add_table_index`

```erlang
-spec add_table_index(Tab, I) -> t_result(ok) when Tab :: table(), I :: index_attr().
```

Add table index.

Table indexes can be used whenever the user wants to use frequently some other
field than the key field to look up records. If this other field has an
associated index, these lookups can occur in constant time and space. For
example, if your application wishes to use field `age` to find efficiently all
persons with a specific age, it can be a good idea to have an index on field
`age`. This can be done with the following call:

```text
mnesia:add_table_index(person, age)
```

Indexes do not come for free. They occupy space that is proportional to the
table size, and they cause insertions into the table to execute slightly slower.

# `all_keys`

```erlang
-spec all_keys(Tab :: table()) -> [Key :: term()].
```

Return all keys in a table.

Returns a list of all keys in the table named `Tab`. The semantics of this
function is context-sensitive. For more information, see `mnesia:activity/4`. In
transaction-context, it acquires a read lock on the entire table.

# `async_dirty`

```erlang
-spec async_dirty(Fun) -> Res | no_return() when Fun :: fun(() -> Res).
```

# `async_dirty`

```erlang
-spec async_dirty(Fun, [Arg :: _]) -> Res | no_return() when Fun :: fun((...) -> Res).
```

Call the `Fun` in a context that is not protected by a transaction.

The Mnesia function calls performed in the `Fun` are mapped to the
corresponding dirty functions. This still involves logging,
replication, and subscriptions, but there is no locking, local
transaction storage, or commit protocols involved.  Checkpoint
retainers and indexes are updated, but they are updated dirty. As for
normal `mnesia:dirty_*` operations, the operations are performed
semi-asynchronously. For details, see `mnesia:activity/4` and the
User's Guide.

The Mnesia tables can be manipulated without using transactions. This has some
serious disadvantages, but is considerably faster, as the transaction manager is
not involved and no locks are set. A dirty operation does, however, guarantee a
certain level of consistency, and the dirty operations cannot return garbled
records. All dirty operations provide location transparency to the programmer,
and a program does not have to be aware of the whereabouts of a certain table to
function.

Notice that it is more than ten times more efficient to read records dirty than
within a transaction.

Depending on the application, it can be a good idea to use the dirty functions
for certain operations. Almost all Mnesia functions that can be called within
transactions have a dirty equivalent, which is much more efficient.

However, notice that there is a risk that the database can be left in an
inconsistent state if dirty operations are used to update it. Dirty operations
are only to be used for performance reasons when it is absolutely necessary.

Notice that calling (nesting) `mnesia:[a]sync_dirty` inside a
transaction-context inherits the transaction semantics.

# `backup`

```erlang
-spec backup(Dest :: term()) -> result().
```

# `backup`

```erlang
-spec backup(Dest :: term(), BackupMod :: module()) -> result().
```

Back up all tables in the database.

Activates a new checkpoint covering all Mnesia tables, including the schema,
with maximum degree of redundancy, and performs a backup using
`backup_checkpoint/2/3`. The default value of the backup callback module
`BackupMod` is obtained by `mnesia:system_info(backup_module)`.

# `backup_checkpoint`

```erlang
-spec backup_checkpoint(Name, Dest) -> result() when Name :: term(), Dest :: term().
```

# `backup_checkpoint`

```erlang
-spec backup_checkpoint(Name, Dest, BackupMod) -> result()
                           when Name :: term(), Dest :: term(), BackupMod :: module().
```

Back up all tables in a checkpoint.

The tables are backed up to external media using backup module `BackupMod`.
Tables with the local contents property are backed up as they exist on the
current node. `BackupMod` is the default backup callback module obtained by
`mnesia:system_info(backup_module)`. For information about the exact callback
interface (the `mnesia_backup behavior`), see the User's Guide.

# `change_config`

```erlang
-spec change_config(Config, Value) -> ReturnValue
                       when
                           Config :: config_key(),
                           Value :: config_value(),
                           ReturnValue :: config_result().
```

Change a configuration setting.

`Config` is to be an atom of the following configuration parameters:

- **`extra_db_nodes`** - `Value` is a list of nodes that Mnesia is to try to
  connect to. `ReturnValue` is those nodes in `Value` that Mnesia is connected
  to.

  Notice that this function must only be used to connect to newly started RAM
  nodes (N.D.R.S.N.) with an empty schema. If, for example, this function is
  used after the network has been partitioned, it can lead to inconsistent
  tables.

  Notice that Mnesia can be connected to other nodes than those returned in
  `ReturnValue`.

- **`dc_dump_limit`** - `Value` is a number. See the description in
  [Section Configuration Parameters](`m:mnesia#configuration_parameters`).
  `ReturnValue` is the new value. Notice that this configuration parameter is
  not persistent. It is lost when Mnesia has stopped.

# `change_table_access_mode`

```erlang
-spec change_table_access_mode(Tab :: table(), AccessMode) -> t_result(ok)
                                  when AccessMode :: read_only | read_write.
```

Change table access mode.

`AccessMode` is by default the atom `read_write` but it can also be set to the
atom `read_only`. If `AccessMode` is set to `read_only`, updates to the table
cannot be performed. At startup, Mnesia always loads `read_only` tables locally
regardless of when and if Mnesia is terminated on other nodes.

# `change_table_copy_type`

```erlang
-spec change_table_copy_type(Tab :: table(), Node :: node(), To :: storage_type()) -> t_result(ok).
```

Change the storage type of a table.

For example:

```erlang
mnesia:change_table_copy_type(person, node(), disc_copies)
```

Transforms the `person` table from a RAM table into a disc-based table at
`Node`.

This function can also be used to change the storage type of the table named
`schema`. The schema table can only have `ram_copies` or `disc_copies` as the
storage type. If the storage type of the schema is `ram_copies`, no other table
can be disc-resident on that node.

# `change_table_frag`

```erlang
-spec change_table_frag(Tab :: table(), FragProp :: change_frag_prop()) -> t_result(ok).
```

Reconfigure table fragment properties.

Argument `FragProp` should have one of the following values:

- **`{activate, FragProps}`** - Activates the fragmentation properties of an
  existing table. `FragProps` is either to contain `{node_pool, Nodes}` or be
  empty.

- **`deactivate`** - Deactivates the fragmentation properties of a table. The
  number of fragments must be `1`. No other table can refer to this table in its
  foreign key.

- **`{add_frag, NodesOrDist}`** - Adds a fragment to a fragmented table. All
  records in one of the old fragments are rehashed and about half of them are
  moved to the new (last) fragment. All other fragmented tables, which refer to
  this table in their foreign key, automatically get a new fragment. Also, their
  records are dynamically rehashed in the same manner as for the main table.

  Argument `NodesOrDist` can either be a list of nodes or the result from the
  function [mnesia:table_info(Tab, frag_dist)](`mnesia:table_info/2`). Argument
  `NodesOrDist` is assumed to be a sorted list with the best nodes to host new
  replicas first in the list. The new fragment gets the same number of replicas
  as the first fragment (see `n_ram_copies`, `n_disc_copies`, and
  `n_disc_only_copies` in [Fragmentation Properties](mnesia_chap5.md#fragmentation-properties)).
  The `NodesOrDist` list must at least contain one element for each replica that
  needs to be allocated.

- **`del_frag`** - Deletes a fragment from a fragmented table. All records in
  the last fragment are moved to one of the other fragments. All other
  fragmented tables, which refer to this table in their foreign key,
  automatically lose their last fragment. Also, their records are dynamically
  rehashed in the same manner as for the main table.

- **`{add_node, Node}`** - Adds a node to `node_pool`. The new node pool affects
  the list returned from the function
  [mnesia:table_info(Tab, frag_dist)](`mnesia:table_info/2`).

- **`{del_node, Node}`** - Deletes a node from `node_pool`. The new node pool
  affects the list returned from the function
  [mnesia:table_info(Tab, frag_dist)](`mnesia:table_info/2`).

# `change_table_load_order`

```erlang
-spec change_table_load_order(Tab :: table(), Order) -> t_result(ok) when Order :: non_neg_integer().
```

Change table load order.

The `LoadOrder` priority is by default `0` (zero) but can be set to any integer.
The tables with the highest `LoadOrder` priority are loaded first at startup.

# `change_table_majority`
*since OTP R14B03* 

```erlang
-spec change_table_majority(Tab :: table(), Majority :: boolean()) -> t_result(ok).
```

Change table majority.

`Majority` must be a boolean. Default is `false`. When `true`, a majority of the
table replicas must be available for an update to succeed. When used on
fragmented tables, `Tab` must be the base table name. Directly changing the
majority setting on individual fragments is not allowed.

# `clear_table`

```erlang
-spec clear_table(Tab :: table()) -> t_result(ok).
```

Delete all entries in the table `Tab`.

# `create_schema`

```erlang
-spec create_schema(DiscNodes :: [node()]) -> result().
```

Create a new schema on the specified nodes.

Creates a new database on disc. Various files are created in the local Mnesia
directory of each node. Notice that the directory must be unique for each node.
Two nodes must never share the same directory. If possible, use a local disc
device to improve performance.

`mnesia:create_schema/1` fails if any of the Erlang nodes given as `DiscNodes`
are not alive, if Mnesia is running on any of the nodes, or if any of the nodes
already have a schema. Use `mnesia:delete_schema/1` to get rid of old faulty
schemas.

Notice that only nodes with disc are to be included in `DiscNodes`. Disc-less
nodes, that is, nodes where all tables including the schema only resides in RAM,
must not be included.

# `create_table`

```erlang
-spec create_table(Name :: table(), Opts :: [create_option()]) -> t_result(ok).
```

Create a table.

Creates a Mnesia table called `Name` according to argument `Opts`. This list
must be a list of `{Item, Value}` tuples, where the following values are
allowed:

- `{access_mode, Atom}`. The access mode is by default the atom `read_write` but
  it can also be set to the atom `read_only`. If `AccessMode` is set to
  `read_only`, updates to the table cannot be performed.

  At startup, Mnesia always loads `read_only` table locally regardless of when
  and if Mnesia is terminated on other nodes. This argument returns the access
  mode of the table. The access mode can be `read_only` or `read_write`.

- `{attributes, AtomList}` is a list of the attribute names for the records that
  are supposed to populate the table. Default is `[key, val]`. The table must at
  least have one extra attribute in addition to the key.

  When accessing single attributes in a record, it is not necessary, or even
  recommended, to hard code any attribute names as atoms. Use construct
  `record_info(fields, RecordName)` instead. It can be used for records of type
  `RecordName`.

- `{disc_copies, Nodelist}`, where `Nodelist` is a list of the nodes where this
  table is supposed to have disc copies. If a table replica is of type
  `disc_copies`, all write operations on this particular replica of the table
  are written to disc and to the RAM copy of the table.

  It is possible to have a replicated table of type `disc_copies` on one node
  and another type on another node. Default is `[]`.

- `{disc_only_copies, Nodelist}`, where `Nodelist` is a list of the nodes where
  this table is supposed to have `disc_only_copies`. A disc only table replica
  is kept on disc only and unlike the other replica types, the contents of the
  replica do not reside in RAM. These replicas are considerably slower than
  replicas held in RAM.
- `{index, Intlist}`, where `Intlist` is a list of attribute names (atoms) or
  record fields for which Mnesia is to build and maintain an extra index table.
  The `qlc` query compiler _may_ be able to optimize queries if there are
  indexes available.
- `{load_order, Integer}`. The load order priority is by default `0` (zero) but
  can be set to any integer. The tables with the highest load order priority are
  loaded first at startup.
- `{majority, Flag}`, where `Flag` must be a boolean. If `true`, any (non-dirty)
  update to the table is aborted, unless a majority of the table replicas are
  available for the commit. When used on a fragmented table, all fragments are
  given the same majority setting.
- `{ram_copies, Nodelist}`, where `Nodelist` is a list of the nodes where this
  table is supposed to have RAM copies. A table replica of type `ram_copies` is
  not written to disc on a per transaction basis. `ram_copies` replicas can be
  dumped to disc with the function `mnesia:dump_tables(Tabs)`. Default value for
  this attribute is `[node()]`.
- `{record_name, Name}`, where `Name` must be an atom. All records stored in the
  table must have this name as the first element. It defaults to the same name
  as the table name.
- `{snmp, SnmpStruct}`. For a description of `SnmpStruct`, see
  `mnesia:snmp_open_table/2`. If this attribute is present in `ArgList` to
  `mnesia:create_table/2`, the table is immediately accessible by SNMP.
  Therefore applications that use SNMP to manipulate and control the system can
  be designed easily, since Mnesia provides a direct mapping between the logical
  tables that make up an SNMP control application and the physical data that
  makes up a Mnesia table.
- `{storage_properties, [{Backend, Properties}]` forwards more properties to the
  back end storage. `Backend` can currently be `ets` or `dets`. `Properties` is
  a list of options sent to the back end storage during table creation.
  `Properties` cannot contain properties already used by Mnesia, such as `type`
  or `named_table`.

  For example:

  ```erlang
  mnesia:create_table(table, [{ram_copies, [node()]}, {disc_only_copies, nodes()},
         {storage_properties,
          [{ets, [compressed]}, {dets, [{auto_save, 5000}]} ]}])
  ```

- `{type, Type}`, where `Type` must be either of the atoms `set`, `ordered_set`,
  or `bag`. Default is `set`. In a `set`, all records have unique keys. In a
  `bag`, several records can have the same key, but the record content is
  unique. If a non-unique record is stored, the old conflicting records are
  overwritten.

  Notice that currently `ordered_set` is not supported for `disc_only_copies`.

- `{local_content, Bool}`, where `Bool` is `true` or `false`. Default is
  `false`.
- `{user_properties, PropList}`, where `PropList` is a list of user-defined
  properties associated with the table. The property is a tuple
  where the first element is the property key. These properties can be read and modified
  using `mnesia:read_table_property/2`, `mnesia:write_table_property/2`, and
  `mnesia:delete_table_property/2`. Default is `[]`.
- `{frag_properties, FragProps}`, where `FragProps` is a list of fragmentation
  properties for the table.
  See [Fragmentation Properties](mnesia_chap5.md#fragmentation-properties)
  for details on fragmentation options.

For example, the following call creates the `person` table (defined earlier) and
replicates it on two nodes:

```erlang
mnesia:create_table(person,
    [{ram_copies, [N1, N2]},
     {attributes, record_info(fields, person)}]).
```

If it is required that Mnesia must build and maintain an extra index table on
attribute `address` of all the `person` records that are inserted in the table,
the following code would be issued:

```erlang
mnesia:create_table(person,
    [{ram_copies, [N1, N2]},
     {index, [address]},
     {attributes, record_info(fields, person)}]).
```

The specification of `index` and `attributes` can be hard-coded as
`{index, [2]}` and `{attributes, [name, age, address, salary, children]}`,
respectively.

`mnesia:create_table/2` writes records into the table `schema`. This function,
and all other schema manipulation functions, are implemented with the normal
transaction management system. This guarantees that schema updates are performed
on all nodes in an atomic manner.

# `deactivate_checkpoint`

```erlang
-spec deactivate_checkpoint(Name :: _) -> result().
```

Deactivate a checkpoint.

The checkpoint is automatically deactivated when some of the tables involved
have no retainer attached to them. This can occur when nodes go down or when a
replica is deleted. Checkpoints are also deactivated with this function. `Name`
is the name of an active checkpoint.

# `del_table_copy`

```erlang
-spec del_table_copy(Tab :: table(), Node :: node()) -> t_result(ok).
```

Delete the replica of table.

Deletes the replica of table `Tab` at node `Node`. When the last replica is
deleted with this function, the table disappears entirely.

This function can also be used to delete a replica of the table named `schema`.
The Mnesia node is then removed. Notice that Mnesia must be stopped on the node
first.

# `del_table_index`

```erlang
-spec del_table_index(Tab, I) -> t_result(ok) when Tab :: table(), I :: index_attr().
```

Delete table index.

Deletes the index on attribute with name `AttrName` in a table.

# `delete`

```erlang
-spec delete(TabKey :: {Tab :: table(), Key :: term()}) -> ok.
```

# `delete`

```erlang
-spec delete(Tab :: table(), Key :: _, LockKind :: write_locks()) -> ok.
```

Delete all records in table `Tab` with the key `Key`.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` in the record. Currently, the lock types `write` and `sticky_write`
are supported.

# `delete_object`

```erlang
-spec delete_object(Rec :: tuple()) -> ok.
```

# `delete_object`

```erlang
-spec delete_object(Tab :: table(), Rec :: tuple(), LockKind :: write_locks()) -> ok.
```

Delete a record.

If a table is of type `bag`, it can sometimes be needed to delete only some of
the records with a certain key. This can be done with the function
[`delete_object/3`](`delete_object/3`). A complete record must be supplied to
this function.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the record. Currently, the lock types `write` and `sticky_write`
are supported.

# `delete_schema`

```erlang
-spec delete_schema(DiscNodes :: [node()]) -> result().
```

Delete the schema on the given nodes.

Deletes a database created with `mnesia:create_schema/1`.
`mnesia:delete_schema/1` fails if any of the Erlang nodes given as `DiscNodes`
are not alive, or if Mnesia is running on any of the nodes.

After the database is deleted, it can still be possible to start Mnesia as a
disc-less node. This depends on how configuration parameter `schema_location` is
set.

> #### Warning {: .warning }
>
> Use this function with extreme caution, as it makes existing persistent data
> obsolete. Think twice before using it.

# `delete_table`

```erlang
-spec delete_table(Tab :: table()) -> t_result(ok).
```

Permanently delete all replicas of table `Tab`.

# `delete_table_property`
*since OTP 28.5* 

```erlang
-spec delete_table_property(Tab :: table(), PropKey :: term()) -> t_result(ok).
```

Delete a user-defined table property.

Deletes a user-defined property from a table if such property exists.
The property is identified by its key. User-defined properties can be read
with `mnesia:read_table_property/2` and written with `mnesia:write_table_property/2`.

# `dirty_all_keys`

```erlang
-spec dirty_all_keys(Tab :: table()) -> [Key :: term()].
```

Dirty equivalent to `mnesia:all_keys/1`.

# `dirty_delete`

```erlang
-spec dirty_delete({Tab :: table(), Key :: _}) -> ok.
```

# `dirty_delete`

```erlang
-spec dirty_delete(Tab :: table(), Key :: _) -> ok.
```

Dirty equivalent to `mnesia:delete/3`.

# `dirty_delete_object`

```erlang
-spec dirty_delete_object(Record :: tuple()) -> ok.
```

# `dirty_delete_object`

```erlang
-spec dirty_delete_object(Tab :: table(), Record :: tuple()) -> ok.
```

Dirty equivalent to `mnesia:delete_object/3`.

# `dirty_first`

```erlang
-spec dirty_first(Tab :: table()) -> Key :: term().
```

Return the key for the first record in a table.

Records in `set` or `bag` tables are not ordered. However, there is an ordering
of the records that is unknown to the user. Therefore, a table can be traversed
by this function with the function `mnesia:dirty_next/2`.

If there are no records in the table, this function returns the atom
`'$end_of_table'`. It is therefore highly undesirable, but not disallowed, to
use this atom as the key for any user records.

# `dirty_index_match_object`

```erlang
-spec dirty_index_match_object(Pattern, Attr) -> [Record]
                                  when Pattern :: tuple(), Attr :: index_attr(), Record :: tuple().
```

# `dirty_index_match_object`

```erlang
-spec dirty_index_match_object(Tab, Pattern, Attr) -> [Record]
                                  when
                                      Tab :: table(),
                                      Pattern :: tuple(),
                                      Attr :: index_attr(),
                                      Record :: tuple().
```

Dirty equivalent to `mnesia:index_match_object/4`.

# `dirty_index_read`

```erlang
-spec dirty_index_read(Tab, Key, Attr) -> [Record]
                          when Tab :: table(), Key :: term(), Attr :: index_attr(), Record :: tuple().
```

Dirty equivalent to `mnesia:index_read/3`.

# `dirty_last`

```erlang
-spec dirty_last(Tab :: table()) -> Key :: term().
```

Return the key for the last record in a table.

Works exactly like `mnesia:dirty_first/1` but returns the last object in Erlang
term order for the `ordered_set` table type. For all other table types,
`mnesia:dirty_first/1` and `mnesia:dirty_last/1` are synonyms.

# `dirty_match_object`

```erlang
-spec dirty_match_object(Pattern :: tuple()) -> [Record :: tuple()].
```

# `dirty_match_object`

```erlang
-spec dirty_match_object(Tab, Pattern) -> [Record]
                            when Tab :: table(), Pattern :: tuple(), Record :: tuple().
```

Dirty equivalent to `mnesia:match_object/3`.

# `dirty_next`

```erlang
-spec dirty_next(Tab :: table(), Key :: _) -> NextKey :: term().
```

Return the next key in a table.

Traverses a table and performs operations on all records in the table. When the
end of the table is reached, the special key `'$end_of_table'` is returned.
Otherwise, the function returns a key that can be used to read the actual
record. The behavior is undefined if another Erlang process performs write
operations on the table while it is being traversed with the function
`mnesia:dirty_next/2`.

# `dirty_prev`

```erlang
-spec dirty_prev(Tab :: table(), Key :: _) -> PrevKey :: term().
```

Return the previous key in a table.

Works exactly like `mnesia:dirty_next/2` but returns the previous object in
Erlang term order for the `ordered_set` table type. For all other table types,
`mnesia:dirty_next/2` and `mnesia:dirty_prev/2` are synonyms.

# `dirty_read`

```erlang
-spec dirty_read(TabKey :: {Tab :: table(), Key :: _}) -> [tuple()].
```

# `dirty_read`

```erlang
-spec dirty_read(Tab :: table(), Key :: _) -> [tuple()].
```

Dirty equivalent to `mnesia:read/3`.

# `dirty_select`

```erlang
-spec dirty_select(Tab, MatchSpec) -> [Match]
                      when Tab :: table(), MatchSpec :: ets:match_spec(), Match :: term().
```

Dirty equivalent to `mnesia:select/2`.

# `dirty_select_reverse`
*since OTP 29.0* 

```erlang
-spec dirty_select_reverse(Tab, MatchSpec) -> [Match]
                              when Tab :: table(), MatchSpec :: ets:match_spec(), Match :: term().
```

Dirty equivalent to `mnesia:select_reverse/2`.

# `dirty_update_counter`

```erlang
-spec dirty_update_counter({Tab :: table(), Key :: _}, Incr :: integer()) -> NewVal :: integer().
```

# `dirty_update_counter`

```erlang
-spec dirty_update_counter(Tab :: table(), Key :: _, Incr :: integer()) -> NewVal :: integer().
```

Dirty update of a counter record.

Mnesia has no special counter records. However, records of the form
`{Tab, Key, Integer}` can be used as (possibly disc-resident) counters when
`Tab` is a `set`. This function updates a counter with a positive or negative
number. However, counters can never become less than zero. There are two
significant differences between this function and the action of first reading
the record, performing the arithmetic, and then writing the record:

- It is much more efficient.
- `mnesia:dirty_update_counter/3` is performed as an atomic operation although
  it is not protected by a transaction.

If two processes perform `mnesia:dirty_update_counter/3` simultaneously, both
updates take effect without the risk of losing one of the updates. The new value
`NewVal` of the counter is returned.

If `Key` does not exist, a new record is created with value `Incr` if it is
larger than 0, otherwise it is set to 0.

# `dirty_write`

```erlang
-spec dirty_write(Record :: tuple()) -> ok.
```

# `dirty_write`

```erlang
-spec dirty_write(Tab :: table(), Record :: tuple()) -> ok.
```

Dirty equivalent to `mnesia:write/3`.

# `dump_log`

```erlang
-spec dump_log() -> dumped.
```

Perform a user-initiated dump of the local log file.

This is usually not necessary, as Mnesia by default manages this automatically. See configuration
parameters [dump_log_time_threshold](`m:mnesia#dump_log_time_threshold`) and
[dump_log_write_threshold](`m:mnesia#dump_log_write_threshold`).

# `dump_tables`

```erlang
-spec dump_tables([Tab :: table()]) -> t_result(ok).
```

Dump ram_copies tables to disc.

Dumps a set of `ram_copies` tables to disc. The next time the system is started,
these tables are initiated with the data found in the files that are the result
of this dump. None of the tables can have disc-resident replicas.

# `dump_to_textfile`

```erlang
-spec dump_to_textfile(File :: file:filename()) -> result() | error.
```

Dump local tables into a text file.

Dumps all local tables of a Mnesia system into a text file, which can be edited
(by a normal text editor) and then be reloaded with `mnesia:load_textfile/1`.
Only use this function for educational purposes. Use other functions to deal
with real backups.

# `error_description`

```erlang
-spec error_description(Error) -> string()
                           when Error :: {error, Reason} | {aborted, Reason} | Reason, Reason :: term().
```

Return a string describing a particular Mnesia error.

All Mnesia transactions, including all the schema update functions, either
return value `{atomic, Val}` or the tuple `{aborted, Reason}`. `Reason` can be
either of the atoms in the following list. The function
[`error_description/1`](`error_description/1`) returns a descriptive string that
describes the error.

- `nested_transaction`. Nested transactions are not allowed in this context.
- `badarg`. Bad or invalid argument, possibly bad type.
- `no_transaction`. Operation not allowed outside transactions.
- `combine_error`. Table options illegally combined.
- `bad_index`. Index already exists, or was out of bounds.
- `already_exists`. Schema option to be activated is already on.
- `index_exists`. Some operations cannot be performed on tables with an index.
- `no_exists`. Tried to perform operation on non-existing (not-alive) item.
- `system_limit`. A system limit was exhausted.
- `mnesia_down`. A transaction involves records on a remote node, which became
  unavailable before the transaction was completed. Records are no longer
  available elsewhere in the network.
- `not_a_db_node`. A node was mentioned that does not exist in the schema.
- `bad_type`. Bad type specified in argument.
- `node_not_running`. Node is not running.
- `truncated_binary_file`. Truncated binary in file.
- `active`. Some delete operations require that all active records are removed.
- `illegal`. Operation not supported on this record.

`Error` can be `Reason`, `{error, Reason}`, `{aborted, Reason}`, or `Reason`.
`Reason` can be an atom or a tuple with `Reason` as an atom in the first field.

The following examples illustrate a function that returns an error, and the
method to retrieve more detailed error information:

- The function [mnesia:create_table(bar, \[\{attributes,
  3.14\}])](`create_table/2`) returns the tuple `{aborted,Reason}`, where
  `Reason` is the tuple `{bad_type,bar,3.14000}`.
- The function [mnesia:error_description(Reason)](`error_description/1`) returns
  the term `{"Bad type on some provided arguments",bar,3.14000}`, which is an
  error description suitable for display.

# `ets`

```erlang
-spec ets(Fun) -> Res | no_return() when Fun :: fun(() -> Res).
```

# `ets`

```erlang
-spec ets(Fun, [Arg :: _]) -> Res | no_return() when Fun :: fun((...) -> Res).
```

Call `Fun` in a raw context that is not protected by a transaction.

The Mnesia function call is performed in the `Fun` and performed directly on the
local ETS tables on the assumption that the local storage type is `ram_copies`
and the tables are not replicated to other nodes. Subscriptions are not
triggered and checkpoints are not updated, but it is extremely fast. This
function can also be applied to `disc_copies` tables if all operations are read
only. For details, see `mnesia:activity/4` and the User's Guide.

Notice that calling (nesting) a `mnesia:ets` inside a transaction-context
inherits the transaction semantics.

# `first`

```erlang
-spec first(Tab :: table()) -> Key :: term().
```

Return the key for the first record in a table.

Records in `set` or `bag` tables are not ordered. However, there is an ordering
of the records that is unknown to the user. A table can therefore be traversed
by this function with the function `mnesia:next/2`.

If there are no records in the table, this function returns the atom
`'$end_of_table'`. It is therefore highly undesirable, but not disallowed, to
use this atom as the key for any user records.

# `foldl`

```erlang
-spec foldl(Fun, Acc0, Table :: table()) -> Acc when Fun :: fun((Record :: tuple(), Acc0) -> Acc).
```

# `foldl`

```erlang
-spec foldl(Fun, Acc0, Table :: table(), LockKind :: lock_kind()) -> Acc
               when Fun :: fun((Record :: tuple(), Acc0) -> Acc).
```

Call `Fun` for each record in `Table`.

Iterates over the table `Table` and calls `Fun(Record, Acc)` for each
`Record` in the table. The term returned from `Fun` is used as the second
argument in the next call to `Fun`.

`foldl` returns the same term as the last call to `Fun` returned.

# `foldr`

```erlang
-spec foldr(Fun, Acc0, Table :: table()) -> Acc when Fun :: fun((Record :: tuple(), Acc0) -> Acc).
```

# `foldr`

```erlang
-spec foldr(Fun, Acc0, Table :: table(), LockKind :: lock_kind()) -> Acc
               when Fun :: fun((Record :: tuple(), Acc0) -> Acc).
```

Call `Fun` for each record in `Table`.

Works exactly like [`foldl/3`](`foldl/3`) but iterates the table in the opposite
order for the `ordered_set` table type. For all other table types,
[`foldr/3`](`foldr/3`) and [`foldl/3`](`foldl/3`) are synonyms.

# `force_load_table`

```erlang
-spec force_load_table(Tab :: table()) -> yes | {error, Reason :: term()}.
```

Force a table to be loaded into the system.

The Mnesia algorithm for table load can lead to a situation where a table cannot
be loaded. This situation occurs when a node is started and Mnesia concludes, or
suspects, that another copy of the table was active after this local copy became
inactive because of a system crash.

If this situation is not acceptable, this function can be used to override the
strategy of the Mnesia table load algorithm. This can lead to a situation where
some transaction effects are lost with an inconsistent database as result, but
for some applications high availability is more important than consistent data.

# `index_match_object`

```erlang
-spec index_match_object(Pattern, Attr) -> [Record]
                            when Pattern :: tuple(), Attr :: index_attr(), Record :: tuple().
```

Match records and uses index information.

Starts `mnesia:index_match_object(Tab, Pattern, Attr, read)`, where `Tab` is
[`element(1, Pattern)`](`element/2`).

# `index_match_object`

```erlang
-spec index_match_object(Tab, Pattern, Attr, LockKind) -> [Record]
                            when
                                Tab :: table(),
                                Pattern :: tuple(),
                                Attr :: index_attr(),
                                LockKind :: lock_kind(),
                                Record :: tuple().
```

Match records and uses index information.

In a manner similar to the function `mnesia:index_read/3`, any index information
can be used when trying to match records. This function takes a pattern that
obeys the same rules as the function `mnesia:match_object/3`, except that this
function requires the following conditions:

- The table `Tab` must have an index on position `Attr`.
- The element in position `Attr` in `Pattern` must be bound. `Attr` is an integer
  (`#record.Field`) or an attribute name.

The two index search functions described here are automatically started when
searching tables with `qlc` list comprehensions and also when using the
low-level `mnesia:[dirty_]match_object` functions.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the entire table or on a single record. Currently, the lock type
`read` is supported.

# `index_read`

```erlang
-spec index_read(Tab, Key, Attr) -> [Record]
                    when Tab :: table(), Key :: term(), Attr :: index_attr(), Record :: tuple().
```

Read records through the index table.

Assume that there is an index on position `Attr` for a certain record type. This
function can be used to read the records without knowing the actual key for the
record. For example, with an index in position 1 of table `person`, the call
`mnesia:index_read(person, 36, #person.age)` returns a list of all persons with
age 36. `Attr` can also be an attribute name (atom), but if the notation
`mnesia:index_read(person, 36, age)` is used, the field position is searched for
in runtime, for each call.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a read lock on the
entire table.

# `info`

```erlang
-spec info() -> ok.
```

Print system information on the terminal.

Prints system information on the terminal. This function can be used even if
Mnesia is not started. However, more information is displayed if Mnesia is
started.

# `install_fallback`

```erlang
-spec install_fallback(Source :: term()) -> result().
```

# `install_fallback`

```erlang
-spec install_fallback(Source :: term(), BackupMod | Args) -> result()
                          when
                              Args :: [Arg],
                              Arg :: Module | Scope | Dir,
                              Module :: {module, BackupMod},
                              Scope :: {scope, global | local},
                              Dir :: {mnesia_dir, Dir :: string()},
                              BackupMod :: module().
```

Install a backup as fallback.

The fallback is used to restore the database at
the next startup. Installation of fallbacks requires Erlang to be operational on
all the involved nodes, but it does not matter if Mnesia is running or not. The
installation of the fallback fails if the local node is not one of the
disc-resident nodes in the backup.

`Args` is a list of the following tuples:

- `{module, BackupMod}`. All accesses of the backup media are performed through
  a callback module named `BackupMod`. Argument `Source` is forwarded to the
  callback module, which can interpret it as it wishes. The default callback
  module is called `mnesia_backup` and it interprets argument `Source` as a
  local filename. The default for this module is also configurable through
  configuration parameter `-mnesia mnesia_backup`.
- `{scope, Scope}`. The `Scope` of a fallback is either `global` for the entire
  database or `local` for one node. By default, the installation of a fallback
  is a global operation, which either is performed on all nodes with a
  disc-resident schema or none. Which nodes that are disc-resident is determined
  from the schema information in the backup.

  If `Scope` of the operation is `local`, the fallback is only installed on the
  local node.

- `{mnesia_dir, AlternateDir}`. This argument is only valid if the scope of the
  installation is `local`. Normally the installation of a fallback is targeted
  to the Mnesia directory, as configured with configuration parameter
  `-mnesia dir`. But by explicitly supplying an `AlternateDir`, the fallback is
  installed there regardless of the Mnesia directory configuration parameter
  setting. After installation of a fallback on an alternative Mnesia directory,
  that directory is fully prepared for use as an active Mnesia directory.

  This is a dangerous feature that must be used with care. By unintentional
  mixing of directories, you can easily end up with an inconsistent database, if
  the same backup is installed on more than one directory.

# `is_transaction`

```erlang
-spec is_transaction() -> boolean().
```

Return true if inside a transaction context.

When this function is executed inside a transaction-context, it returns `true`,
otherwise `false`.

# `last`

```erlang
-spec last(Tab :: table()) -> Key :: term().
```

Return the key for the last record in a table.

Works exactly like `mnesia:first/1`, but returns the last object in Erlang term
order for the `ordered_set` table type. For all other table types,
`mnesia:first/1` and `mnesia:last/1` are synonyms.

# `load_textfile`

```erlang
-spec load_textfile(File :: file:filename()) -> t_result(ok) | {error, term()}.
```

Load tables from a text file.

Loads a series of definitions and data found in the text file (generated with
`mnesia:dump_to_textfile/1`) into Mnesia. This function also starts Mnesia and
possibly creates a new schema. This function is intended for educational
purposes only. It is recommended to use other functions to deal with real
backups.

# `lock`

```erlang
-spec lock(LockItem, LockKind) -> list() | tuple() | no_return()
              when
                  LockItem ::
                      {record, table(), Key :: term()} |
                      {table, table()} |
                      {global, Key :: term(), MnesiaNodes :: [node()]},
                  LockKind :: lock_kind() | load.
```

Explicitly grab lock.

Write locks are normally acquired on all nodes where a replica of the table
resides (and is active). Read locks are acquired on one node (the local node if
a local replica exists). Most of the context-sensitive access functions acquire
an implicit lock if they are started in a transaction-context. The granularity
of a lock can either be a single record or an entire table.

The normal use is to call the function without checking the return value, as it
exits if it fails and the transaction is restarted by the transaction manager.
It returns all the locked nodes if a write lock is acquired and `ok` if it was a
read lock.

The function `mnesia:lock/2` is intended to support explicit locking on tables,
but is also intended for situations when locks need to be acquired regardless of
how tables are replicated. Currently, two kinds of `LockKind` are supported:

- **`write`** - Write locks are exclusive. This means that if one transaction
  manages to acquire a write lock on an item, no other transaction can acquire
  any kind of lock on the same item.

- **`read`** - Read locks can be shared. This means that if one transaction
  manages to acquire a read lock on an item, other transactions can also acquire
  a read lock on the same item. However, if someone has a read lock, no one can
  acquire a write lock at the same item. If someone has a write lock, no one can
  acquire either a read lock or a write lock at the same item.

Conflicting lock requests are automatically queued if there is no risk of a
deadlock. Otherwise the transaction must be terminated and executed again.
Mnesia does this automatically as long as the upper limit of the maximum
`retries` is not reached. For details, see `mnesia:transaction/3`.

For the sake of completeness, sticky write locks are also described here even if
a sticky write lock is not supported by this function:

- **`sticky_write`** - Sticky write locks are a mechanism that can be used to
  optimize write lock acquisition. If your application uses replicated tables
  mainly for fault tolerance (as opposed to read access optimization purpose),
  sticky locks can be the best option available.

  When a sticky write lock is acquired, all nodes are informed which node is
  locked. Then, sticky lock requests from the same node are performed as a local
  operation without any communication with other nodes. The sticky lock lingers
  on the node even after the transaction ends. For details, see the User's
  Guide.

Currently, this function supports two kinds of `LockItem`:

- **`{table, Tab}`** - This acquires a lock of type `LockKind` on the entire
  table `Tab`.

- **`{global, GlobalKey, Nodes}`** - This acquires a lock of type `LockKind` on
  the global resource `GlobalKey`. The lock is acquired on all active nodes in
  the `Nodes` list.

Locks are released when the outermost transaction ends.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires locks, otherwise it
ignores the request.

# `match_object`

```erlang
-spec match_object(Pattern :: tuple()) -> [Record :: tuple()].
```

# `match_object`

```erlang
-spec match_object(Tab, Pattern, LockKind) -> [Record]
                      when
                          Tab :: table(), Pattern :: tuple(), LockKind :: lock_kind(), Record :: tuple().
```

Match `Pattern` for records.

Takes a pattern with "don't care" variables denoted as a `'_'` parameter. This
function returns a list of records that matched the pattern. Since the second
element of a record in a table is considered to be the key for the record, the
performance of this function depends on whether this key is bound or not.

For example, the call
`mnesia:match_object(person, {person, '_', 36, '_', '_'}, read)` returns a list
of all person records with an `age` field of 36.

The function `mnesia:match_object/3` automatically uses indexes if these exist.
However, no heuristics are performed to select the best index.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind` on the entire table or a single record. Currently, the lock type
`read` is supported.

# `move_table_copy`

```erlang
-spec move_table_copy(Tab :: table(), From :: node(), To :: node()) -> t_result(ok).
```

Move a table copy.

Moves the copy of table `Tab` from node `From` to node `To`.

The storage type is preserved. For example, a RAM table moved from one node
remains a RAM on the new node. Other transactions can still read and write in
the table while it is being moved.

This function cannot be used on `local_content` tables.

# `next`

```erlang
-spec next(Tab :: table(), Key :: term()) -> NextKey :: term().
```

Return the next key in a table.

Traverses a table and performs operations on all records in the table. When the
end of the table is reached, the special key `'$end_of_table'` is returned.
Otherwise the function returns a key that can be used to read the actual record.

# `prev`

```erlang
-spec prev(Tab :: table(), Key :: term()) -> PrevKey :: term().
```

Return the previous key in a table.

Works exactly like `mnesia:next/2`, but returns the previous object in Erlang
term order for the `ordered_set` table type. For all other table types,
`mnesia:next/2` and `mnesia:prev/2` are synonyms.

# `read`

```erlang
-spec read(TabKey :: {Tab :: table(), Key :: _}) -> [tuple()].
```

# `read`

```erlang
-spec read(Tab :: table(), Key :: _) -> [tuple()].
```

# `read`

```erlang
-spec read(Tab :: table(), Key :: _, LockKind :: lock_kind()) -> [tuple()].
```

Read records(s) with a given key.

Reads all records from table `Tab` with key `Key`. This function has the same
semantics regardless of the location of `Tab`. If the table is of type `bag`,
the function `mnesia:read(Tab, Key)` can return an arbitrarily long list. If the
table is of type `set`, the list is either of length 1, or `[]`.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind`. Currently, the lock types `read`, `write`, and `sticky_write` are
supported.

If the user wants to update the record, it is more efficient to use
`write/sticky_write` as the `LockKind`. If majority checking is active on the
table, it is checked as soon as a write lock is attempted. This can be used to
end quickly if the majority condition is not met.

# `read_lock_table`

```erlang
-spec read_lock_table(Tab :: table()) -> ok.
```

# `read_table_property`
*since OTP 28.5* 

```erlang
-spec read_table_property(Tab :: table(), PropKey :: term()) -> Res :: tuple().
```

Read a user-defined table property.

Reads a user-defined property associated with a table. User-defined properties
are set when creating a table with the `user_properties` option in
`mnesia:create_table/2`, or can be added later with
`mnesia:write_table_property/2`.

Returns the property tuple if it exists, otherwise raises an exception.

# `report_event`

```erlang
-spec report_event(Event :: _) -> ok.
```

Report a user event to the Mnesia event handler.

When tracing a system of Mnesia applications it is useful to be able to
interleave Mnesia own events with application-related events that give
information about the application context.

Whenever the application begins a new and demanding Mnesia task, or if it enters
a new interesting phase in its execution, it can be a good idea to use
`mnesia:report_event/1`. `Event` can be any term and generates a
`{mnesia_user, Event}` event for any processes that subscribe to Mnesia system
events.

# `restore`

```erlang
-spec restore(Source :: _, [Arg]) -> t_result([table()])
                 when
                     Op :: skip_tables | clear_tables | keep_tables | restore_tables,
                     Arg :: {module, module()} | {Op, [table()]} | {default_op, Op}.
```

Restore a backup.

With this function, tables can be restored online from a backup without
restarting Mnesia. `Source` is forwarded to the backup module. `Args` is a list
of the following tuples:

- `{module,BackupMod}`. The backup module `BackupMod` is used to access the
  backup media. If omitted, the default backup module is used.
- `{skip_tables, TabList}`, where `TabList` is a list of tables that is not to
  be read from the backup.
- `{clear_tables, TabList}`, where `TabList` is a list of tables that is to be
  cleared before the records from the backup are inserted. That is, all records
  in the tables are deleted before the tables are restored. Schema information
  about the tables is not cleared or read from the backup.
- `{keep_tables, TabList}`, where `TabList` is a list of tables that is not to
  be cleared before the records from the backup are inserted. That is, the
  records in the backup are added to the records in the table. Schema
  information about the tables is not cleared or read from the backup.
- `{recreate_tables, TabList}`, where `TabList` is a list of tables that is to
  be recreated before the records from the backup are inserted. The tables are
  first deleted and then created with the schema information from the backup.
  All the nodes in the backup need to be operational.
- `{default_op, Operation}`, where `Operation` is either of the operations
  `skip_tables`, `clear_tables`, `keep_tables`, or `recreate_tables`. The
  default operation specifies which operation that is to be used on tables from
  the backup that is not specified in any of the mentioned lists. If omitted,
  operation `clear_tables` is used.

The affected tables are write-locked during the restoration. However, regardless
of the lock conflicts caused by this, the applications can continue to do their
work while the restoration is being performed. The restoration is performed as
one single transaction.

If the database is huge, it it not always possible to restore it online. In such
cases, restore the old database by installing a fallback and then restart.

# `s_delete`

```erlang
-spec s_delete(TabKey :: {Tab :: table(), Key :: _}) -> ok.
```

Call the function `mnesia:delete(Tab, Key, sticky_write)`

# `s_delete_object`

```erlang
-spec s_delete_object(Rec :: tuple()) -> ok.
```

# `s_write`

```erlang
-spec s_write(Record :: tuple()) -> ok.
```

# `schema`

```erlang
-spec schema() -> ok.
```

Print information about all table definitions on the terminal.

# `schema`

```erlang
-spec schema(Tab :: table()) -> ok.
```

Print information about one table definition on the terminal.

# `select`

```erlang
-spec select(Cont) -> {[Match], Cont} | '$end_of_table'
                when Match :: term(), Cont :: select_continuation().
```

Continue selecting objects.

Selects more objects with the match specification initiated by
`mnesia:select/4`.

Notice that any modifying operations, that is, `mnesia:write` or
`mnesia:delete`, that are done between the `mnesia:select/4` and
`mnesia:select/1` calls are not visible in the result.

# `select`

```erlang
-spec select(Tab, MatchSpec) -> [Match]
                when Tab :: table(), MatchSpec :: ets:match_spec(), Match :: term().
```

# `select`

```erlang
-spec select(Tab, MatchSpec, LockKind) -> [Match]
                when
                    Tab :: table(),
                    MatchSpec :: ets:match_spec(),
                    Match :: term(),
                    LockKind :: lock_kind().
```

Select the objects in `Tab` against `MatchSpec`.

Matches the objects in table `Tab` using a `match_spec` as described in the
`ets:select/3`. Optionally a lock `read` or `write` can be given as the third
argument. Default is `read`. The return value depends on `MatchSpec`.

Notice that for best performance, `select` is to be used before any modifying
operations are done on that table in the same transaction. That is, do not use
`write` or `delete` before a `select`.

In its simplest forms, the `match_spec` look as follows:

- `MatchSpec = [MatchFunction]`
- `MatchFunction = {MatchHead, [Guard], [Result]}`
- `MatchHead = tuple() | record()`
- `Guard = {"Guardtest name", ...}`
- `Result = "Term construct"`

For a complete description of `select`, see the [ERTS](`e:erts:index.html`)
User's Guide and the `m:ets` manual page in STDLIB.

For example, to find the names of all male persons older than 30 in table `Tab`:

```erlang
MatchHead = #person{name='$1', sex=male, age='$2', _='_'},
Guard = {'>', '$2', 30},
Result = '$1',
mnesia:select(Tab,[{MatchHead, [Guard], [Result]}]),
```

# `select`

```erlang
-spec select(Tab, MatchSpec, NObjects, LockKind) -> {[Match], Cont} | '$end_of_table'
                when
                    Tab :: table(),
                    MatchSpec :: ets:match_spec(),
                    Match :: term(),
                    NObjects :: non_neg_integer(),
                    LockKind :: lock_kind(),
                    Cont :: select_continuation().
```

Select the objects in `Tab` against `MatchSpec`.

Matches the objects in table `Tab` using a `match_spec` as described in the
[ERTS](`e:erts:index.html`) User's Guide, and returns a chunk of terms and a
continuation. The wanted number of returned terms is specified by argument
`NObjects`. The lock argument can be `read` or `write`. The continuation is to
be used as argument to `mnesia:select/1`, if more or all answers are needed.

Notice that for best performance, `select` is to be used before any modifying
operations are done on that table in the same transaction. That is, do not use
`mnesia:write` or `mnesia:delete` before a `mnesia:select`. For efficiency,
`NObjects` is a recommendation only and the result can contain anything from an
empty list to all available results.

# `select_reverse`
*since OTP 29.0* 

```erlang
-spec select_reverse(Cont) -> {[Match], Cont} | '$end_of_table'
                        when Match :: term(), Cont :: select_continuation().
```

Continue selecting objects.

Selects more objects with the match specification initiated by
`mnesia:select_reverse/4`.

Notice that any modifying operations, that is, `mnesia:write` or
`mnesia:delete`, that are done between the `mnesia:select_reverse/4` and
`mnesia:select_reverse/1` calls are not visible in the result.

# `select_reverse`
*since OTP 29.0* 

```erlang
-spec select_reverse(Tab, MatchSpec) -> [Match]
                        when Tab :: table(), MatchSpec :: ets:match_spec(), Match :: term().
```

# `select_reverse`
*since OTP 29.0* 

```erlang
-spec select_reverse(Tab, MatchSpec, LockKind) -> [Match]
                        when
                            Tab :: table(),
                            MatchSpec :: ets:match_spec(),
                            Match :: term(),
                            LockKind :: lock_kind().
```

Works like `select/3`, but for table type `ordered_set`, traversing is done
starting at the last object in Erlang term order, and moves to the first. For
all other table types, the return value is identical to that of `select/3`.

See `select/3` for more information.

# `select_reverse`
*since OTP 29.0* 

```erlang
-spec select_reverse(Tab, MatchSpec, NObjects, LockKind) -> {[Match], Cont} | '$end_of_table'
                        when
                            Tab :: table(),
                            MatchSpec :: ets:match_spec(),
                            Match :: term(),
                            NObjects :: non_neg_integer(),
                            LockKind :: lock_kind(),
                            Cont :: select_continuation().
```

Select the objects in `Tab` against `MatchSpec` in reverse order.

Matches the objects in table `Tab` using a `match_spec` as described in the
[ERTS](`e:erts:index.html`) User's Guide, and returns a chunk of terms and a
continuation. The wanted number of returned terms is specified by argument
`NObjects`. The lock argument can be `read` or `write`. The continuation is to
be used as argument to `mnesia:select_reverse/1`, if more or all answers are needed.

Notice that for best performance, `select_reverse` is to be used before any modifying
operations are done on that table in the same transaction. That is, do not use
`mnesia:write` or `mnesia:delete` before a `mnesia:select_reverse`. For efficiency,
`NObjects` is a recommendation only and the result can contain anything from an
empty list to all available results.

# `set_debug_level`

```erlang
-spec set_debug_level(Level :: debug_level()) -> OldLevel :: debug_level().
```

Change the internal debug level of Mnesia.

For details, see Section Configuration Parameters](`m:mnesia#configuration_parameters`).

# `set_master_nodes`

```erlang
-spec set_master_nodes(MasterNodes :: [node()]) -> result().
```

Set the master nodes for all tables.

For each table Mnesia determines its replica nodes (`TabNodes`) and starts
`mnesia:set_master_nodes(Tab, TabMasterNodes)`. where `TabMasterNodes` is the
intersection of `MasterNodes` and `TabNodes`. For semantics, see
`mnesia:set_master_nodes/2`.

# `set_master_nodes`

```erlang
-spec set_master_nodes(Tab :: table(), MasterNodes :: [node()]) -> result().
```

Set the master nodes for a table.

If the application detects a communication failure (in a potentially partitioned
network) that can have caused an inconsistent database, it can use the function
`mnesia:set_master_nodes(Tab, MasterNodes)` to define from which nodes each
table is to be loaded. At startup, the Mnesia normal table load algorithm is
bypassed and the table is loaded from one of the master nodes defined for the
table, regardless of when and if Mnesia terminated on other nodes. `MasterNodes`
can only contain nodes where the table has a replica. If the `MasterNodes` list
is empty, the master node recovery mechanism for the particular table is reset,
and the normal load mechanism is used at the next restart.

The master node setting is always local. It can be changed regardless if Mnesia
is started or not.

The database can also become inconsistent if configuration parameter
`max_wait_for_decision` is used or if `mnesia:force_load_table/1` is used.

# `snmp_close_table`

```erlang
-spec snmp_close_table(Tab :: table()) -> ok.
```

Remove the possibility for SNMP to manipulate the table.

# `snmp_get_mnesia_key`

```erlang
-spec snmp_get_mnesia_key(Tab :: table(), RowIndex :: [integer()]) -> {ok, Key :: term()} | undefined.
```

Get the corresponding Mnesia key from an SNMP index.

Transforms an SNMP index to the corresponding Mnesia key. If the SNMP table has
multiple keys, the key is a tuple of the key columns.

# `snmp_get_next_index`

```erlang
-spec snmp_get_next_index(Tab :: table(), RowIndex :: [integer()]) -> {ok, [integer()]} | endOfTable.
```

Get the index of the next lexicographical row.

`RowIndex` can specify a non-existing row. Specifically, it can be the empty
list. Returns the index of the next lexicographical row. If `RowIndex` is the
empty list, this function returns the index of the first row in the table.

# `snmp_get_row`

```erlang
-spec snmp_get_row(Tab :: table(), RowIndex :: [integer()]) -> {ok, Row :: tuple()} | undefined.
```

Retrieve a row indexed by an SNMP index.

Reads a row by its SNMP index. This index is specified as an SNMP Object
Identifier, a list of integers.

# `snmp_open_table`

```erlang
-spec snmp_open_table(Tab :: table(), SnmpStruct :: snmp_struct()) -> ok.
```

Organize a Mnesia table as an SNMP table.

A direct one-to-one mapping can be established between Mnesia tables and SNMP
tables. Many telecommunication applications are controlled and monitored by the
SNMP protocol. This connection between Mnesia and SNMP makes it simple and
convenient to achieve this mapping.

Argument `SnmpStruct` is a list of SNMP information. Currently, the only
information needed is information about the key types in the table. Multiple
keys cannot be handled in Mnesia, but many SNMP tables have multiple keys.
Therefore, the following convention is used: if a table has multiple keys, these
must always be stored as a tuple of the keys. Information about the key types is
specified as a tuple of atoms describing the types. The only significant type is
`fix_string`. This means that a string has a fixed size.

For example, the following causes table `person` to be ordered as an SNMP table:

```text
mnesia:snmp_open_table(person, [{key, string}])
```

Consider the following schema for a table of company employees. Each employee is
identified by department number and name. The other table column stores the
telephone number:

```erlang
mnesia:create_table(employee,
    [{snmp, [{key, {integer, string}}]},
     {attributes, record_info(fields, employees)}]),
```

The corresponding SNMP table would have three columns: `department`, `name`, and
`telno`.

An option is to have table columns that are not visible through the SNMP
protocol. These columns must be the last columns of the table. In the previous
example, the SNMP table could have columns `department` and `name` only. The
application could then use column `telno` internally, but it would not be
visible to the SNMP managers.

In a table monitored by SNMP, all elements must be integers, strings, or lists
of integers.

When a table is SNMP ordered, modifications are more expensive than usual,
O(logN). Also, more memory is used.

Notice that only the lexicographical SNMP ordering is implemented in Mnesia, not
the actual SNMP monitoring.

# `start`

```erlang
-spec start() -> result().
```

Start a local Mnesia system.

Mnesia startup is asynchronous. The function call `mnesia:start()` returns the
atom `ok` and then starts to initialize the different tables. Depending on the
size of the database, this can take some time, and the application programmer
must wait for the tables that the application needs before they can be used.
This is achieved by using the function `mnesia:wait_for_tables/2`.

The startup procedure for a set of Mnesia nodes is a fairly complicated
operation. A Mnesia system consists of a set of nodes, with Mnesia started
locally on all participating nodes. Normally, each node has a directory where
all the Mnesia files are written. This directory is referred to as the Mnesia
directory. Mnesia can also be started on disc-less nodes. For more information
about disc-less nodes, see `mnesia:create_schema/1` and the User's Guide.

The set of nodes that makes up a Mnesia system is kept in a schema. Mnesia nodes
can be added to or removed from the schema. The initial schema is normally
created on disc with the function `mnesia:create_schema/1`. On disc-less nodes,
a tiny default schema is generated each time Mnesia is started. During the
startup procedure, Mnesia exchanges schema information between the nodes to
verify that the table definitions are compatible.

Each schema has a unique cookie, which can be regarded as a unique schema
identifier. The cookie must be the same on all nodes where Mnesia is supposed to
run. For details, see the User's Guide.

The schema file and all other files that Mnesia needs are kept in the Mnesia
directory. The command-line option `-mnesia dir Dir` can be used to specify the
location of this directory to the Mnesia system. If no such command-line option
is found, the name of the directory defaults to `Mnesia.Node`.

`application:start(mnesia)` can also be used.

# `stop`

```erlang
-spec stop() -> stopped | {error, term()}.
```

Stop Mnesia locally on the current node.

`application:stop(mnesia)` can also be used.

# `subscribe`

```erlang
-spec subscribe(EventCategory) -> {ok, node()} | {error, Reason :: term()}
                   when EventCategory :: system | activity | {table, table(), simple | detailed}.
```

Subscribe to events of type `EventCategory`.

Ensures that a copy of all events of type `EventCategory` is sent to the caller.
The available event types are described in the
[User's Guide](mnesia_chap5.md#event_handling).

# `sync_dirty`

```erlang
-spec sync_dirty(Fun) -> Res | no_return() when Fun :: fun(() -> Res).
```

# `sync_dirty`

```erlang
-spec sync_dirty(Fun, [Arg :: _]) -> Res | no_return() when Fun :: fun((...) -> Res).
```

Call the `Fun` in a context that is not protected by a transaction.

The Mnesia function calls performed in the `Fun` are mapped to the corresponding dirty
functions. It is performed in almost the same context as
`mnesia:async_dirty/1,2`. The difference is that the operations are performed
synchronously. The caller waits for the updates to be performed on all active
replicas before the `Fun` returns. For details, see `mnesia:activity/4` and the
User's Guide.

# `sync_log`
*since OTP 17.0* 

```erlang
-spec sync_log() -> result().
```

Perform a file sync of the local log file.

Ensures that the local transaction log file is synced to disk. On a single node
system, data written to disk tables since the last dump can be lost if there is
a power outage. See `dump_log/0`.

# `sync_transaction`

```erlang
-spec sync_transaction(Fun) -> t_result(Res) when Fun :: fun(() -> Res).
```

# `sync_transaction`

```erlang
-spec sync_transaction(Fun, Retries) -> t_result(Res)
                          when
                              Fun :: fun(() -> Res) | fun((...) -> Res),
                              Retries :: non_neg_integer() | infinity;
                      (Fun, Args :: [Arg :: _]) -> t_result(Res) when Fun :: fun((...) -> Res).
```

# `sync_transaction`

```erlang
-spec sync_transaction(Fun, [Arg :: _], Retries) -> t_result(Res)
                          when Fun :: fun((...) -> Res), Retries :: non_neg_integer() | infinity.
```

Synchronously execute a transaction.

Waits until data have been committed and logged to disk (if disk is used) on
every involved node before it returns, otherwise it behaves as
`mnesia:transaction/[1,2,3]`.

This functionality can be used to avoid that one process overloads a database on
another node.

# `system_info`

```erlang
-spec system_info(Item :: term()) -> ItemVal :: term().
```

Return information about the Mnesia system.

Such as transaction statistics, `db_nodes`, and configuration
parameters.

The valid keys are as follows:

- `all`. Returns a list of all local system information. Each element is a
  `{InfoItem, ItemVal}` tuple.

  New `InfoItem`s can be added and old undocumented `InfoItem`s can be removed
  without notice.

- `access_module`. Returns the name of module that is configured to be the
  activity access callback module.
- `auto_repair`. Returns `true` or `false` to indicate if Mnesia is configured
  to start the auto-repair facility on corrupted disc files.
- `backup_module`. Returns the name of the module that is configured to be the
  backup callback module.
- `checkpoints`. Returns a list of the names of the checkpoints currently active
  on this node.
- `event_module`. Returns the name of the module that is the event handler
  callback module.
- `db_nodes`. Returns the nodes that make up the persistent database. Disc-less
  nodes are only included in the list of nodes if they explicitly have been
  added to the schema, for example, with `mnesia:add_table_copy/3`. The function
  can be started even if Mnesia is not yet running.
- `debug`. Returns the current debug level of Mnesia.
- `directory`. Returns the name of the Mnesia directory. It can be called even
  if Mnesia is not yet running.
- `dump_log_load_regulation`. Returns a boolean that tells if Mnesia is
  configured to regulate the dumper process load.

  This feature is temporary and will be removed in future releases.

- `dump_log_time_threshold`. Returns the time threshold for transaction log
  dumps in milliseconds.
- `dump_log_update_in_place`. Returns a boolean that tells if Mnesia is
  configured to perform the updates in the Dets files directly, or if the
  updates are to be performed in a copy of the Dets files.
- `dump_log_write_threshold`. Returns the write threshold for transaction log
  dumps as the number of writes to the transaction log.
- `extra_db_nodes`. Returns a list of extra `db_nodes` to be contacted at
  startup.
- `fallback_activated`. Returns `true` if a fallback is activated, otherwise
  `false`.
- `held_locks`. Returns a list of all locks held by the local Mnesia lock
  manager.
- `is_running`. Returns `yes` or `no` to indicate if Mnesia is running. It can
  also return `starting` or `stopping`. Can be called even if Mnesia is not yet
  running.
- `local_tables`. Returns a list of all tables that are configured to reside
  locally.
- `lock_queue`. Returns a list of all transactions that are queued for execution
  by the local lock manager.
- `log_version`. Returns the version number of the Mnesia transaction log
  format.
- `master_node_tables`. Returns a list of all tables with at least one master
  node.
- `protocol_version`. Returns the version number of the Mnesia inter-process
  communication protocol.
- `running_db_nodes`. Returns a list of nodes where Mnesia currently is running.
  This function can be called even if Mnesia is not yet running, but it then has
  slightly different semantics.

  If Mnesia is down on the local node, the function returns those other
  `db_nodes` and `extra_db_nodes` that for the moment are operational.

  If Mnesia is started, the function returns those nodes that Mnesia on the
  local node is fully connected to. Only those nodes that Mnesia has exchanged
  schema information with are included as `running_db_nodes`. After the merge of
  schemas, the local Mnesia system is fully operable and applications can
  perform access of remote replicas. Before the schema merge, Mnesia only
  operates locally. Sometimes there are more nodes included in the
  `running_db_nodes` list than all `db_nodes` and `extra_db_nodes` together.

- `schema_location`. Returns the initial schema location.
- `subscribers`. Returns a list of local processes currently subscribing to
  system events.
- `tables`. Returns a list of all locally known tables.
- `transactions`. Returns a list of all currently active local transactions.
- `transaction_failures`. Returns a number that indicates how many transactions
  have failed since Mnesia was started.
- `transaction_commits`. Returns a number that indicates how many transactions
  have terminated successfully since Mnesia was started.
- `transaction_restarts`. Returns a number that indicates how many transactions
  have been restarted since Mnesia was started.
- `transaction_log_writes`. Returns a number that indicates how many write
  operations that have been performed to the transaction log since startup.
- `use_dir`. Returns a boolean that indicates if the Mnesia directory is used or
  not. Can be started even if Mnesia is not yet running.
- `version`. Returns the current version number of Mnesia.

# `table`

```erlang
-spec table(Tab :: table()) -> qlc:query_handle().
```

# `table`

```erlang
-spec table(Tab :: table(), Options) -> qlc:query_handle()
               when
                   Options :: Option | [Option],
                   Option :: MnesiaOpt | QlcOption,
                   MnesiaOpt ::
                       {traverse, SelectOp} | {lock, lock_kind()} | {n_objects, non_neg_integer()},
                   SelectOp :: select | {select, ets:match_spec()},
                   QlcOption :: {key_equality, '==' | '=:='}.
```

Return a QLC query handle.

Returns a Query List Comprehension (QLC) query handle, see the `m:qlc` manual
page in STDLIB. The module `qlc` implements a query language that can use Mnesia
tables as sources of data. Calling `mnesia:table/1,2` is the means to make the
`mnesia` table `Tab` usable to QLC.

`Option` can contain Mnesia options or QLC options. Mnesia recognizes the
following options (any other option is forwarded to QLC).

- `{lock, Lock}`, where `lock` can be `read` or `write`. Default is `read`.
- `{n_objects,Number}`, where `n_objects` specifies (roughly) the number of
  objects returned from Mnesia to QLC. Queries to remote tables can need a
  larger chunk to reduce network overhead. By default, `100` objects at a time
  are returned.
- `{traverse, SelectMethod}`, where `traverse` determines the method to traverse
  the whole table (if needed). The default method is `select`.

There are two alternatives for `select`:

- `select`. The table is traversed by calling `mnesia:select/4` and
  `mnesia:select/1`. The match specification (the second argument of
  [`select/3`](`select/3`)) is assembled by QLC: simple filters are translated
  into equivalent match specifications. More complicated filters need to be
  applied to all objects returned by [`select/3`](`select/3`) given a match
  specification that matches all objects.
- `{select, MatchSpec}`. As for `select`, the table is traversed by calling
  `mnesia:select/3` and `mnesia:select/1`. The difference is that the match
  specification is explicitly given. This is how to state match specifications
  that cannot easily be expressed within the syntax provided by QLC.

# `table_info`

```erlang
-spec table_info(Tab :: table(), InfoItem :: term()) -> ItemVal :: term().
```

Return local information about table.

The [`table_info/2`](`table_info/2`) function takes two arguments. The first is
the name of a Mnesia table. The second is one of the following keys:

- `all`. Returns a list of all local table information. Each element is a
  `{InfoItem, ItemVal}` tuple.

  New `InfoItem`s can be added and old undocumented `InfoItem`s can be removed
  without notice.

- `access_mode`. Returns the access mode of the table. The access mode can be
  `read_only` or `read_write`.
- `arity`. Returns the arity of records in the table as specified in the schema.
- `attributes`. Returns the table attribute names that are specified in the
  schema.
- `checkpoints`. Returns the names of the currently active checkpoints, which
  involve this table on this node.
- `cookie`. Returns a table cookie, which is a unique system-generated
  identifier for the table. The cookie is used internally to ensure that two
  different table definitions using the same table name cannot accidentally be
  intermixed. The cookie is generated when the table is created initially.
- `disc_copies`. Returns the nodes where a `disc_copy` of the table resides
  according to the schema.
- `disc_only_copies`. Returns the nodes where a `disc_only_copy` of the table
  resides according to the schema.
- `index`. Returns the list of index position integers for the table.
- `load_node`. Returns the name of the node that Mnesia loaded the table from.
  The structure of the returned value is unspecified, but can be useful for
  debugging purposes.
- `load_order`. Returns the load order priority of the table. It is an integer
  and defaults to `0` (zero).
- `load_reason`. Returns the reason of why Mnesia decided to load the table. The
  structure of the returned value is unspecified, but can be useful for
  debugging purposes.
- `local_content`. Returns `true` or `false` to indicate if the table is
  configured to have locally unique content on each node.
- `master_nodes`. Returns the master nodes of a table.
- `memory`. Returns for `ram_copies` and `disc_copies` tables the number of
  words allocated in memory to the table on this node. For `disc_only_copies`
  tables the number of bytes stored on disc is returned.
- `ram_copies`. Returns the nodes where a `ram_copy` of the table resides
  according to the schema.
- `record_name`. Returns the record name, common for all records in the table.
- `size`. Returns the number of records inserted in the table.
- `snmp`. Returns the SNMP struct. `[]` means that the table currently has no
  SNMP properties.
- `storage_type`. Returns the local storage type of the table. It can be
  `disc_copies`, `ram_copies`, `disc_only_copies`, or the atom `unknown`.
  `unknown` is returned for all tables that only reside remotely.
- `subscribers`. Returns a list of local processes currently subscribing to
  local table events that involve this table on this node.
- `type`. Returns the table type, which is `bag`, `set`, or `ordered_set`.
- `user_properties`. Returns the user-associated table properties of the table.
  It is a list of the stored property records.
- `version`. Returns the current version of the table definition. The table
  version is incremented when the table definition is changed. The table
  definition can be incremented directly when it has been changed in a schema
  transaction, or when a committed table definition is merged with table
  definitions from other nodes during startup.
- `where_to_read`. Returns the node where the table can be read. If value
  `nowhere` is returned, either the table is not loaded or it resides at a
  remote node that is not running.
- `where_to_write`. Returns a list of the nodes that currently hold an active
  replica of the table.
- `wild_pattern`. Returns a structure that can be given to the various match
  functions for a certain table. A record tuple is where all record fields have
  value `'_'`.

# `transaction`

```erlang
-spec transaction(Fun) -> t_result(Res) when Fun :: fun(() -> Res).
```

# `transaction`

```erlang
-spec transaction(Fun, Retries) -> t_result(Res)
                     when Fun :: fun(() -> Res), Retries :: non_neg_integer() | infinity;
                 (Fun, Args :: [Arg :: _]) -> t_result(Res) when Fun :: fun((...) -> Res).
```

# `transaction`

```erlang
-spec transaction(Fun, Args, Retries) -> t_result(Res)
                     when
                         Fun :: fun((...) -> Res),
                         Args :: [Arg :: _],
                         Retries :: non_neg_integer() | infinity.
```

Execute `Fun` with arguments `Args` as a transaction.

The code that executes inside the transaction can consist of a series of table
manipulation functions. If something goes wrong inside the transaction as a
result of a user error or a certain table not being available, the entire
transaction is terminated and the function [`transaction/1`](`transaction/1`)
returns the tuple `{aborted, Reason}`.

If all is going well, `{atomic, Res}` is returned, where `Res`
is the value of the last expression in `Fun`.

A function that adds a family to the database can be written as follows if there
is a structure `{family, Father, Mother, ChildrenList}`:

```erlang
add_family({family, Father, Mother, ChildrenList}) ->
    ChildOids = lists:map(fun oid/1, ChildrenList),
    Trans = fun() ->
        mnesia:write(Father#person{children = ChildOids}),
        mnesia:write(Mother#person{children = ChildOids}),
        Write = fun(Child) -> mnesia:write(Child) end,
        lists:foreach(Write, ChildrenList)
    end,
    mnesia:transaction(Trans).

oid(Rec) -> {element(1, Rec), element(2, Rec)}.
```

This code adds a set of people to the database. Running this code within one
transaction ensures that either the whole family is added to the database, or
the whole transaction terminates. For example, if the last child is badly
formatted, or the executing process terminates because of an `'EXIT'` signal
while executing the family code, the transaction terminates. Thus, the situation
where half a family is added can never occur.

It is also useful to update the database within a transaction if several
processes concurrently update the same records. For example, the function
`raise(Name, Amount)`, which adds `Amount` to the salary field of a person, is
to be implemented as follows:

```erlang
raise(Name, Amount) ->
    mnesia:transaction(fun() ->
        case mnesia:wread({person, Name}) of
            [P] ->
                Salary = Amount + P#person.salary,
                P2 = P#person{salary = Salary},
                mnesia:write(P2);
            _ ->
                mnesia:abort("No such person")
        end
    end).
```

When this function executes within a transaction, several processes running on
different nodes can concurrently execute the function `raise/2` without
interfering with each other.

Since Mnesia detects deadlocks, a transaction can be restarted any number of
times and therefore the `Fun` shall not have any side effects such as waiting
for specific messages. This function attempts a restart as many times as
specified in `Retries`. `Retries` must be an integer greater than 0 or the atom
`infinity`, default is `infinity`. Mnesia uses `exit` exceptions to signal that
a transaction needs to be restarted, thus a `Fun` must not catch `exit`
exceptions with reason `{aborted, term()}`.

# `transform_table`

```erlang
-spec transform_table(Tab :: table(), Fun, NewAttributeList) -> t_result(ok)
                         when
                             NewAttributeList :: [atom()],
                             Fun :: fun((Record :: tuple()) -> Transformed :: tuple()) | ignore.
```

# `transform_table`

```erlang
-spec transform_table(Tab :: table(), Fun, NewAttributeList, NewRecordName) -> t_result(ok)
                         when
                             NewRecordName :: atom(),
                             NewAttributeList :: [atom()],
                             Fun :: fun((Record :: tuple()) -> Transformed :: tuple()) | ignore.
```

Change format on all records in table.

Applies argument `Fun` to all records in the table. `Fun` is a function that
takes a record of the old type and returns a transformed record of the new type.
Argument `Fun` can also be the atom `ignore`, which indicates that only the
metadata about the table is updated. Use of `ignore` is not recommended, but
included as a possibility for the user do to an own transformation.

`NewAttributeList` and `NewRecordName` specify the attributes and the new record
type of the converted table. Table name always remains unchanged. If
`record_name` is changed, only the Mnesia functions that use table identifiers
work, for example, `mnesia:write/3` works, but not `mnesia:write/1`.

# `traverse_backup`

```erlang
-spec traverse_backup(Source :: term(), Target :: term(), Fun, Acc) ->
                         {ok, Acc} | {error, Reason :: term()}
                         when Fun :: fun((Items, Acc) -> {Items, Acc}).
```

# `traverse_backup`

```erlang
-spec traverse_backup(Source :: term(),
                      SourceMod :: module(),
                      Target :: term(),
                      TargetMod :: module(),
                      Fun, Acc) ->
                         {ok, LastAcc} | {error, Reason :: term()}
                         when Fun :: fun((BackupItems, Acc) -> {BackupItems, NewAcc}), LastAcc :: NewAcc.
```

Traverse a backup.

Iterates over a backup, either to transform it into a new backup, or read it.
The arguments are explained briefly here. For details, see the User's Guide.

- `SourceMod` and `TargetMod` are the names of the modules that actually access
  the backup media.
- `Source` and `Target` are opaque data used exclusively by modules `SourceMod`
  and `TargetMod` to initialize the backup media.
- `Acc` is an initial accumulator value.
- `Fun(BackupItems, Acc)` is applied to each item in the backup. The `Fun` must
  return a tuple `{BackupItems,NewAcc}`, where `BackupItems` is a list of valid
  backup items, and `NewAcc` is a new accumulator value. The returned backup
  items are written in the target backup.
- `LastAcc` is the last accumulator value. This is the last `NewAcc` value that
  was returned by `Fun`.

# `uninstall_fallback`

```erlang
-spec uninstall_fallback() -> result().
```

# `uninstall_fallback`

```erlang
-spec uninstall_fallback(Args) -> result() when Args :: [{mnesia_dir, Dir :: string()}].
```

Uninstall a fallback.

Deinstalls a fallback before it has been used to restore the database. This is
normally a distributed operation that is either performed on all nodes with disc
resident schema, or none. Uninstallation of fallbacks requires Erlang to be
operational on all involved nodes, but it does not matter if Mnesia is running
or not. Which nodes that are considered as disc-resident nodes is determined
from the schema information in the local fallback.

`Args` is a list of the following tuples:

- `{module, BackupMod}`. For semantics, see `mnesia:install_fallback/2`.
- `{scope, Scope}`. For semantics, see `mnesia:install_fallback/2`.
- `{mnesia_dir, AlternateDir}`. For semantics, see `mnesia:install_fallback/2`.

# `unsubscribe`

```erlang
-spec unsubscribe(EventCategory) -> {ok, node()} | {error, Reason :: term()}
                     when EventCategory :: system | activity | {table, table(), simple | detailed}.
```

Stop sending events of type `EventCategory` to the caller.

`Node` is the local node.

# `wait_for_tables`

```erlang
-spec wait_for_tables([Tab :: table()], TMO :: timeout()) -> result() | {timeout, [table()]}.
```

Wait for tables to be accessible.

Some applications need to wait for certain tables to be accessible to do useful
work. `mnesia:wait_for_tables/2` either hangs until all tables in `TabList` are
accessible, or until `timeout` is reached.

# `wread`

```erlang
-spec wread(TabKey :: {Tab :: table(), Key :: _}) -> [tuple()].
```

# `write`

```erlang
-spec write(Record :: tuple()) -> ok.
```

Write a record into the database.

Calls the function `mnesia:write(Tab, Record, write)`, where `Tab` is
[`element(1, Record)`](`element/2`).

# `write`

```erlang
-spec write(Tab :: table(), Record :: tuple(), LockKind :: write_locks()) -> ok.
```

Write `Record` to table `Tab`.

The function returns `ok`, or terminates if an error occurs. For example, the
transaction terminates if no `Tab` table exists.

The semantics of this function is context-sensitive. For details, see
`mnesia:activity/4`. In transaction-context, it acquires a lock of type
`LockKind`. The lock types `write` and `sticky_write` are supported.

# `write_lock_table`

```erlang
-spec write_lock_table(Tab :: table()) -> ok.
```

# `write_table_property`
*since OTP 28.5* 

```erlang
-spec write_table_property(Tab :: table(), Prop :: tuple()) -> t_result(ok).
```

Write a user-defined table property.

Writes or updates a user-defined property for a table. The property is a tuple
where the first element is the property key. User-defined properties can be read
with `mnesia:read_table_property/2` and deleted with
`mnesia:delete_table_property/2`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
