# `snmpa`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/snmp/src/agent/snmpa.erl#L22)

Interface Functions to the SNMP toolkit agent

The module `snmpa` contains interface functions to the SNMP agent.

# `db`

```erlang
-type db() :: volatile | persistent | mnesia.
```

# `discovery_handler`

```erlang
-type discovery_handler() :: module().
```

Module implementing the [snmpa_discovery_handler](`m:snmpa_discovery_handler#`)
behaviour.

# `me`

```erlang
-type me() :: snmp:me().
```

# `mib_storage`

```erlang
-type mib_storage() :: [mib_storage_opt()].
```

# `mib_storage_module`

```erlang
-type mib_storage_module() :: atom().
```

# `mib_storage_opt`

```erlang
-type mib_storage_opt() :: {module, mib_storage_module()} | {options, mib_storage_options()}.
```

# `mib_storage_options`

```erlang
-type mib_storage_options() :: list().
```

# `name`

```erlang
-type name() :: table_name() | variable_name().
```

# `name_db`

```erlang
-type name_db() :: {name(), db()}.
```

# `nfilter_id`

```erlang
-type nfilter_id() :: term().
```

# `nfilter_position`

```erlang
-type nfilter_position() :: first | last | {insert_before, nfilter_id()} | {insert_after, nfilter_id()}.
```

# `notification_delivery_info`

```erlang
-type notification_delivery_info() ::
          snmpa_notification_delivery_info_receiver:notification_delivery_info().
```

How shall (notification) delivery info be reported.

# `pdu_type`

```erlang
-type pdu_type() :: snmp:pdu_type().
```

# `table_name`

```erlang
-type table_name() :: atom().
```

# `transport_kind`

```erlang
-type transport_kind() :: req_responder | trap_sender.
```

# `variable_name`

```erlang
-type variable_name() :: atom().
```

# `add_agent_caps`

```erlang
-spec add_agent_caps(Oid, Descr) -> Index when Oid :: snmp:oid(), Descr :: string(), Index :: integer().
```

This function can be used to add an AGENT-CAPABILITY statement to the sysORTable
in the agent. The table is defined in the SNMPv2-MIB.

# `backup`

```erlang
-spec backup(BackupDir) -> ok | {error, Reason}
                when BackupDir :: string(), Reason :: backup_in_progress | term().
```

# `backup`

```erlang
-spec backup(Agent, BackupDir) -> ok | {error, Reason}
                when
                    Agent :: pid() | AgentName,
                    AgentName :: atom(),
                    BackupDir :: string(),
                    Reason :: backup_in_progress | term().
```

Backup persistent/permanent data handled by the agent (such as local-db,
mib-data and vacm).

Data stored by mnesia is not handled.

`BackupDir` cannot be identical to DbDir.

Simultaneous backup calls are _not_ allowed. That is, two different processes
cannot simultaneously successfully call this function. One of them will be
first, and succeed. The second will fail with the error reason
`backup_in_progress`.

# `change_log_size`

```erlang
-spec change_log_size(NewSize) -> ok | {error, Reason} when NewSize :: snmp:log_size(), Reason :: term().
```

Changes the log size of the Audit Trail Log. The application must be configured
to use the audit trail log function. Please refer to disk_log(3) in Kernel
Reference Manual for a description of how to change the log size.

The change is permanent, as long as the log is not deleted. That means, the log
size is remembered across reboots.

# `convert_config`

```erlang
-spec convert_config(OldConfig) -> NewConfig when OldConfig :: list(), NewConfig :: list().
```

This off-line utility function can be used to convert the old snmp application
config (pre snmp-4.0) to the new snmp agent config (as of snmp-4.0).

For information about the old config (`OldConfig`) see the OTP R9C
documentation.

For information about the current agent config (`AgentConfig`), see the
[Configuring the application](snmp_config.md#configuration_params) chapter of
the SNMP user's guide.

# `current_address`

```erlang
-spec current_address() -> {value, Address} | false when Address :: term().
```

Get the address of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).

# `current_community`

```erlang
-spec current_community() -> {value, Community} | false when Community :: snmp_community_mib:name().
```

Get the community of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).

# `current_context`

```erlang
-spec current_context() -> {value, Context} | false when Context :: snmp_community_mib:context_name().
```

Get the context of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).

# `current_request_id`

```erlang
-spec current_request_id() -> {value, RequestId} | false when RequestId :: integer().
```

Get the request-id of the request currently being processed by the agent.

Note that this function is intended to be called by the instrumentation
functions and _only_ if it is executed in the context of the agent process (e.g.
it _does not work_ if called from a spawned process).

# `del_agent_caps`

```erlang
-spec del_agent_caps(Index) -> snmp:void() when Index :: integer().
```

This function can be used to delete an AGENT-CAPABILITY statement to the
sysORTable in the agent. This table is defined in the SNMPv2-MIB.

# `disable_mibs_cache`

```erlang
-spec disable_mibs_cache() -> snmp:void().
```

# `disable_mibs_cache`

```erlang
-spec disable_mibs_cache(Agent) -> snmp:void() when Agent :: pid() | AgentName, AgentName :: atom().
```

Disable the mib server cache.

# `disable_mibs_cache_autogc`

```erlang
-spec disable_mibs_cache_autogc() -> snmp:void().
```

# `disable_mibs_cache_autogc`

```erlang
-spec disable_mibs_cache_autogc(Agent) -> snmp:void()
                                   when Agent :: pid() | AgentName, AgentName :: atom().
```

Disable automatic gc of the mib server cache.

# `discovery`

```erlang
-spec discovery(TargetName, Notification) -> {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term().
```

# `discovery`

```erlang
-spec discovery(TargetName, Notification, Varbinds) -> {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       Varbinds :: [Varbind],
                       Varbind ::
                           {Variable :: atom(), Value} |
                           {OID :: snmp:oid(), Value} |
                           {Column :: atom(), RowIndex :: snmp:row_index(), Value},
                       Value :: term(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term();
               (TargetName, Notification, ContextName) -> {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       ContextName :: snmp_community_mib:context_name(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term().
```

# `discovery`

```erlang
-spec discovery(TargetName, Notification, ContextName, Varbinds) ->
                   {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       ContextName :: snmp_community_mib:context_name(),
                       Varbinds :: [Varbind],
                       Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {OID, Value},
                       Variable :: atom(),
                       Column :: atom(),
                       RowIndex :: snmp:row_index(),
                       OID :: snmp:oid(),
                       Value :: term(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term();
               (TargetName, Notification, Varbinds, DiscoHandler) ->
                   {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       Varbinds :: [Varbind],
                       Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {OID, Value},
                       Variable :: atom(),
                       Column :: atom(),
                       RowIndex :: snmp:row_index(),
                       OID :: snmp:oid(),
                       Value :: term(),
                       DiscoHandler :: discovery_handler(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term().
```

# `discovery`

```erlang
-spec discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler) ->
                   {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       ContextName :: snmp_community_mib:context_name(),
                       Varbinds :: [Varbind],
                       Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {OID, Value},
                       Variable :: atom(),
                       Column :: atom(),
                       RowIndex :: snmp:row_index(),
                       OID :: snmp:oid(),
                       Value :: term(),
                       DiscoHandler :: discovery_handler(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term().
```

# `discovery`

```erlang
-spec discovery(TargetName, Notification, ContextName, Varbinds, DiscoHandler, ExtraInfo) ->
                   {ok, ManagerEngineID} | {error, Reason}
                   when
                       TargetName :: string(),
                       Notification :: atom(),
                       ContextName :: snmp_community_mib:context_name(),
                       Varbinds :: [Varbind],
                       Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {OID, Value},
                       Variable :: atom(),
                       Column :: atom(),
                       RowIndex :: snmp:row_index(),
                       OID :: snmp:oid(),
                       Value :: term(),
                       DiscoHandler :: discovery_handler(),
                       ExtraInfo :: term(),
                       ManagerEngineID :: snmp_framework_mib:engine_id(),
                       Reason :: term().
```

Initiate the discovery process with the manager identified by `TargetName` using
the notification `Notification`.

This function is synchronous, which means that it will return when the discovery
process has been completed or failed.

The `DiscoHandler` module is used during the discovery process. See
[discovery handler](`m:snmpa_discovery_handler`) for more info.

The `ExtraInfo` argument is passed on to the callback functions of the
`DiscoHandler`.

> #### Note {: .info }
>
> If we are not at security-level `noAuthNoPriv`, this could be complicated,
> since the agent will then continue with stage 2, before which the usm-related
> updates must be done.

> #### Note {: .info }
>
> The default discovery handler will require additional actions by the caller
> and the discovery will not work if the security-level is higher then
> `noAuthNoPriv`.

# `enable_mibs_cache`

```erlang
-spec enable_mibs_cache() -> snmp:void().
```

# `enable_mibs_cache`

```erlang
-spec enable_mibs_cache(Agent) -> snmp:void() when Agent :: pid() | AgentName, AgentName :: atom().
```

Enable the mib server cache.

# `enable_mibs_cache_autogc`

```erlang
-spec enable_mibs_cache_autogc() -> snmp:void().
```

# `enable_mibs_cache_autogc`

```erlang
-spec enable_mibs_cache_autogc(Agent) -> snmp:void()
                                  when Agent :: pid() | AgentName, AgentName :: atom().
```

Enable automatic gc of the mib server cache.

# `enum_to_int`

```erlang
-spec enum_to_int(Name, Enum) -> {value, Int} | false
                     when Name :: atom(), Enum :: atom(), Int :: integer().
```

# `enum_to_int`

```erlang
-spec enum_to_int(Db, Name, Enum) -> {value, Int} | false
                     when Db :: term(), Name :: atom(), Enum :: atom(), Int :: integer().
```

Converts the symbolic value `Enum` to the corresponding integer of the
enumerated object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

# `gc_mibs_cache`

```erlang
-spec gc_mibs_cache() -> {ok, NumElementsGCed} | {error, Reason}
                       when NumElementsGCed :: non_neg_integer(), Reason :: term().
```

# `gc_mibs_cache`

```erlang
-spec gc_mibs_cache(Agent) -> {ok, NumElementsGCed} | {error, Reason}
                       when
                           Agent :: pid() | AgentName,
                           AgentName :: atom(),
                           NumElementsGCed :: non_neg_integer(),
                           Reason :: term();
                   (Age) -> {ok, NumElementsGCed} | {error, Reason}
                       when Age :: pos_integer(), NumElementsGCed :: non_neg_integer(), Reason :: term().
```

# `gc_mibs_cache`

```erlang
-spec gc_mibs_cache(Agent, Age) -> {ok, NumElementsGCed} | {error, Reason}
                       when
                           Agent :: pid() | AgentName,
                           AgentName :: atom(),
                           Age :: pos_integer(),
                           NumElementsGCed :: non_neg_integer(),
                           Reason :: term();
                   (Age, GcLimit) -> {ok, NumElementsGCed} | {error, Reason}
                       when
                           Age :: pos_integer(),
                           GcLimit :: pos_integer() | infinity,
                           NumElementsGCed :: non_neg_integer(),
                           Reason :: term().
```

# `gc_mibs_cache`

```erlang
-spec gc_mibs_cache(Agent, Age, GcLimit) -> {ok, NumElementsGCed} | {error, Reason}
                       when
                           Agent :: pid() | AgentName,
                           AgentName :: atom(),
                           Age :: pos_integer(),
                           GcLimit :: pos_integer() | infinity,
                           NumElementsGCed :: non_neg_integer(),
                           Reason :: term().
```

Perform mib server cache gc.

Manually performs a mib server cache gc. This can be done regardless of the
value of the `autogc` option. The `NumElementsGCed` value indicates how many
elements where actually removed from the cache.

# `get`

```erlang
-spec get(Agent, Vars) -> Values | {error, Reason}
             when
                 Agent :: pid() | AgentName,
                 AgentName :: atom(),
                 Vars :: [snmp:oid()],
                 Values :: [term()],
                 Reason :: term().
```

# `get`

```erlang
-spec get(Agent, Vars, Context) -> Values | {error, Reason}
             when
                 Agent :: pid() | AgentName,
                 AgentName :: atom(),
                 Vars :: [snmp:oid()],
                 Context :: snmp_community_mib:context_name(),
                 Values :: [term()],
                 Reason :: term().
```

Performs a GET operation on the agent. All loaded MIB objects are visible in
this operation. The agent calls the corresponding instrumentation functions just
as if it was a GET request coming from a manager.

Note that the request specific parameters (such as
`current_request_id/0`) are not accessible for the
instrumentation functions if this function is used.

# `get_agent_caps`

```erlang
-spec get_agent_caps() -> Caps
                        when
                            Caps :: [[Cap]],
                            Cap :: SysORIndex | SysORID | SysORDescr | SysORUpTime,
                            SysORIndex :: integer(),
                            SysORID :: snmp:oid(),
                            SysORDescr :: string(),
                            SysORUpTime :: integer().
```

Returns all AGENT-CAPABILITY statements in the sysORTable in the agent. This
table is defined in the SNMPv2-MIB.

Note that the Erlang type language do not permit us to properly describe what
this function returns. The _exact_ return is:

`[[SysORIndex, SysORID, SysORDescr, SysORUpTime]]`

# `get_next`

```erlang
-spec get_next(Agent, Vars) -> Values | {error, Reason}
                  when
                      Agent :: pid() | AgentName,
                      AgentName :: atom(),
                      Vars :: [snmp:oid()],
                      Values :: [{snmp:oid(), term()}],
                      Reason :: term().
```

# `get_next`

```erlang
-spec get_next(Agent, Vars, Context) -> Values | {error, Reason}
                  when
                      Agent :: pid() | AgentName,
                      AgentName :: atom(),
                      Vars :: [snmp:oid()],
                      Context :: snmp_community_mib:context_name(),
                      Values :: [{snmp:oid(), term()}],
                      Reason :: {atom(), snmp:oid()}.
```

Performs a GET-NEXT operation on the agent. All loaded MIB objects are visible
in this operation. The agent calls the corresponding instrumentation functions
just as if it was a GET request coming from a manager.

Note that the request specific parameters (such as `snmpa:current_request_id/0`
are not accessible for the instrumentation functions if this function is used.

# `info`

```erlang
-spec info() -> Info when Info :: [{Key, Value}], Key :: term(), Value :: term().
```

# `info`

```erlang
-spec info(Agent) -> Info
              when
                  Agent :: pid() | AgentName,
                  AgentName :: atom(),
                  Info :: [{Key, Value}],
                  Key :: term(),
                  Value :: term().
```

Returns a list (a dictionary) containing information about the agent.
Information includes loaded MIBs, registered sub-agents, some information about
the memory allocation.

# `int_to_enum`

```erlang
-spec int_to_enum(Name, Int) -> {value, Enum} | false
                     when Name :: atom(), Int :: integer(), Enum :: atom().
```

# `int_to_enum`

```erlang
-spec int_to_enum(Db, Name, Int) -> {value, Enum} | false
                     when Db :: term(), Name :: atom(), Int :: integer(), Enum :: atom().
```

Converts the integer `Int` to the corresponding symbolic value of the enumerated
object or type `Name` in a MIB. The MIB must be loaded.

`false` is returned if the object or type is not defined in any loaded MIB, or
if it does not define the symbolic value as enumerated.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

# `invalidate_mibs_cache`

```erlang
-spec invalidate_mibs_cache() -> snmp:void().
```

# `invalidate_mibs_cache`

```erlang
-spec invalidate_mibs_cache(Agent) -> snmp:void() when Agent :: pid() | AgentName, AgentName :: atom().
```

Invalidate the mib server cache.

The entire contents of the cache will be deleted.

# `load_mib`
*since OTP R16B02* 

```erlang
-spec load_mib(Mib) -> ok | {error, Reason} when Mib :: string(), Reason :: already_loaded | term().
```

# `load_mib`
*since OTP R16B02* 

```erlang
-spec load_mib(Agent, Mib) -> ok | {error, Reason}
                  when
                      Agent :: pid() | AgentName,
                      AgentName :: atom(),
                      Mib :: string(),
                      Reason :: already_loaded | term().
```

Load a single `Mib` into an agent. The `MibName` is the name of the Mib,
including the path to where the compiled mib is found. For example:

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpa:load_mib(snmp_master_agent, Dir ++ "MY-MIB").
```

# `load_mibs`

```erlang
-spec load_mibs(Mibs) -> ok | {error, Reason}
                   when
                       Mibs :: [MibName],
                       MibName :: string(),
                       Reason :: {'load aborted at', MibName, InternalReason},
                       InternalReason :: already_loaded | term().
```

# `load_mibs`

```erlang
-spec load_mibs(Agent, Mibs) -> ok | {error, Reason}
                   when
                       Agent :: pid() | AgentName,
                       AgentName :: atom(),
                       Mibs :: [MibName],
                       MibName :: string(),
                       Reason :: {'load aborted at', MibName, InternalReason},
                       InternalReason :: already_loaded | term();
               (Mibs, Force) -> ok | {error, Reason}
                   when
                       Mibs :: [MibName],
                       MibName :: string(),
                       Force :: boolean(),
                       Reason :: {'load aborted at', MibName, InternalReason},
                       InternalReason :: already_loaded | term().
```

# `load_mibs`
*since OTP R16B02* 

```erlang
-spec load_mibs(Agent, Mibs, Force) -> ok | {error, Reason}
                   when
                       Agent :: pid() | AgentName,
                       AgentName :: atom(),
                       Mibs :: [MibName],
                       MibName :: string(),
                       Force :: boolean(),
                       Reason :: {'load aborted at', MibName, InternalReason},
                       InternalReason :: already_loaded | term().
```

Load `Mibs` into an agent. If the agent cannot load all MIBs (the default value
of the `Force` argument is `false`), it will indicate where loading was aborted.
The `MibName` is the name of the Mib, including the path to where the compiled
mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpa:load_mibs(snmp_master_agent, [Dir ++ "MY-MIB"]).
```

If `Force = true` then the agent will continue attempting to load each mib even
after failing to load a previous mib. Use with care.

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile, Start) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Start :: null | snmp:log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Start :: null | snmp:log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile, Start, Stop) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Start :: null | snmp:log_time(),
                       Stop :: null | snmp:log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R16B03* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Start :: null | snmp:log_time(),
                       Stop :: null | snmp:log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

Converts an Audit Trail Log to a readable format and prints it on stdio.
`LogName` defaults to "snmpa_log". `LogFile` defaults to "snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [`snmp:log_to_io/7`](`snmp:log_to_io/7`) for more info.

# `log_to_txt`
*since OTP R15B01* 

```erlang
-spec log_to_txt(LogDir) -> snmp:void() when LogDir :: snmp:dir().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Block) -> snmp:void() when LogDir :: snmp:dir(), Block :: boolean();
                (LogDir, Mibs) -> snmp:void() when LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()].
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, Block) -> snmp:void()
                    when LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()], Block :: boolean();
                (LogDir, Mibs, OutFile) -> snmp:void()
                    when LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()], OutFile :: file:filename().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, Block) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        Block :: boolean();
                (LogDir, Mibs, OutFile, LogName) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, Block) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        Block :: boolean();
                (LogDir, Mibs, OutFile, LogName, LogFile) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        LogFile :: string().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block | Start) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Start :: null | snmp:log_time().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Start :: null | snmp:log_time();
                (LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        LogFile :: string(),
                        Start :: null | snmp:log_time(),
                        Stop :: null | snmp:log_time().
```

# `log_to_txt`
*since OTP R16B03* 

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) -> snmp:void()
                    when
                        LogDir :: snmp:dir(),
                        Mibs :: [snmp:mib_name()],
                        OutFile :: file:filename(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Start :: null | snmp:log_time(),
                        Stop :: null | snmp:log_time().
```

Converts an Audit Trail Log to a readable text file. `OutFile` defaults to
"./snmpa_log.txt". `LogName` defaults to "snmpa_log". `LogFile` defaults to
"snmpa.log".

The `Block` option indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

See [`snmp:log_to_txt/8`](`snmp:log_to_txt/8`) for more info.

# `me_of`

```erlang
-spec me_of(Oid) -> {ok, Me} | {error, Reason} when Oid :: snmp:oid(), Me :: snmp:me(), Reason :: term().
```

# `me_of`

```erlang
-spec me_of(Agent, Oid) -> {ok, Me} | {error, Reason}
               when
                   Agent :: pid() | AgentName,
                   AgentName :: atom(),
                   Oid :: snmp:oid(),
                   Me :: snmp:me(),
                   Reason :: term().
```

Finds the mib entry corresponding to the `Oid`.

If it is a variable, the Oid must be <Oid for var>.0 and if it is a table, Oid
must be `<table>.<entry>.<col>.<any>`

# `mib_of`

```erlang
-spec mib_of(Oid) -> {ok, MibName} | {error, Reason}
                when Oid :: snmp:oid(), MibName :: atom(), Reason :: term().
```

# `mib_of`

```erlang
-spec mib_of(Agent, Oid) -> {ok, MibName} | {error, Reason}
                when
                    Agent :: pid() | AgentName,
                    AgentName :: atom(),
                    Oid :: snmp:oid(),
                    MibName :: atom(),
                    Reason :: term().
```

Finds the mib corresponding to the `Oid`.

If it is a variable, the Oid must be `<Oid for var>.0` and if it is a table, Oid
must be `<table>.<entry>.<col>.<any>`.

# `name_to_oid`

```erlang
-spec name_to_oid(Name) -> {value, Oid} | false when Name :: atom(), Oid :: snmp:oid().
```

# `name_to_oid`

```erlang
-spec name_to_oid(Db, Name) -> {value, Oid} | false when Db :: term(), Name :: atom(), Oid :: snmp:oid().
```

Looks up the OBJECT IDENTIFIER of a MIB object, given the symbolic name. Note,
the OBJECT IDENTIFIER is given for the object, not for an instance.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

# `oid_to_name`

```erlang
-spec oid_to_name(Oid) -> {value, Name} | false when Oid :: snmp:oid(), Name :: atom().
```

# `oid_to_name`

```erlang
-spec oid_to_name(Db, Oid) -> {value, Name} | false when Db :: term(), Oid :: snmp:oid(), Name :: atom().
```

Looks up the symbolic name of a MIB object, given OBJECT IDENTIFIER.

`false` is returned if the object is not defined in any loaded MIB.

`Db` is a reference to the symbolic store database (retrieved by a call to
`get_symbolic_store_db/0`).

# `print_mib_info`
*since OTP R14B02* 

```erlang
-spec print_mib_info() -> snmp:void().
```

Prints the content of all the (snmp) tables and variables for all mibs handled
by the snmp agent.

# `print_mib_tables`
*since OTP R14B02* 

```erlang
-spec print_mib_tables() -> snmp:void().
```

Prints the content of all the (snmp) tables for all mibs handled by the snmp
agent.

# `print_mib_variables`
*since OTP R14B02* 

```erlang
-spec print_mib_variables() -> snmp:void().
```

Prints the content of all the (snmp) variables for all mibs handled by the snmp
agent.

# `register_notification_filter`

```erlang
-spec register_notification_filter(Id, Mod, Data) -> ok | {error, Reason}
                                      when
                                          Id :: nfilter_id(),
                                          Mod :: module(),
                                          Data :: term(),
                                          Reason :: term().
```

# `register_notification_filter`

```erlang
-spec register_notification_filter(Agent | Id, Id | Mod, Mod | Data, Data | Where) ->
                                      ok | {error, Reason}
                                      when
                                          Agent :: pid() | AgentName,
                                          AgentName :: atom(),
                                          Id :: nfilter_id(),
                                          Mod :: module(),
                                          Data :: term(),
                                          Where :: nfilter_position(),
                                          Reason :: term().
```

Accepted type specifications are:
```
-spec register_notification_filter(Agent, Id, Mod, Data) -> ok | {error, Reason}.
-spec register_notification_filter(Id, Mod, Data, Where) -> ok | {error, Reason}.
```

# `register_notification_filter`

```erlang
-spec register_notification_filter(Agent, Id, Mod, Data, Where) -> ok | {error, Reason}
                                      when
                                          Agent :: pid() | AgentName,
                                          AgentName :: atom(),
                                          Id :: nfilter_id(),
                                          Mod :: module(),
                                          Data :: term(),
                                          Where :: nfilter_position(),
                                          Reason :: term().
```

Registers a notification filter.

`Mod` is a module implementing the `snmpa_notification_filter` behaviour.

`Data` will be passed on to the filter when calling the functions of the
behaviour.

# `register_subagent`

```erlang
-spec register_subagent(Agent, SubTree, SubAgent) -> ok | {error, Reason}
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               SubTree :: snmp:oid(),
                               SubAgent :: pid(),
                               Reason :: term().
```

Registers a sub-agent under a sub-tree of another agent.

It is easy to make mistakes when registering sub-agents and this activity should
be done carefully. For example, a strange behaviour would result from the
following configuration:

```erlang
snmp_agent:register_subagent(MAPid,[1,2,3,4],SA1),
snmp_agent:register_subagent(SA1,[1,2,3], SA2).
```

`SA2` will not get requests starting with object identifier `[1,2,3]` since
`SA1` does not.

# `restart_set_worker`

```erlang
-spec restart_set_worker() -> snmp:void().
```

# `restart_set_worker`

```erlang
-spec restart_set_worker(Agent) -> snmp:void() when Agent :: pid | AgentName, AgentName :: atom().
```

Restart the set worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.

# `restart_worker`

```erlang
-spec restart_worker() -> snmp:void().
```

# `restart_worker`

```erlang
-spec restart_worker(Agent) -> snmp:void() when Agent :: pid | AgentName, AgentName :: atom().
```

Restart the worker process of a multi-threaded agent.

This is a utility function, that can be useful when e.g. debugging
instrumentation functions.

# `send_notification2`
*since OTP R14B03* 

```erlang
-spec send_notification2(Agent, Notification, SendOpts) -> snmp:void()
                            when
                                Agent :: pid() | AgentName,
                                AgentName :: atom(),
                                Notification :: atom(),
                                SendOpts :: [SendOpt],
                                SendOpt ::
                                    {receiver, Receiver} |
                                    {name, snmp_notification_mib:notify_name()} |
                                    {context, snmp_community_mib:context_name()} |
                                    {varbinds, [Varbind]} |
                                    {local_engine_id, snmp_framework_mib:engine_id()} |
                                    {extra, term()},
                                Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                                Tag :: term(),
                                Recv :: pid() | atom() | MFA,
                                MFA :: {Mod, Func, Args},
                                Mod :: module(),
                                Func :: atom(),
                                Args :: list(),
                                Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {Oid, Value},
                                Variable :: atom(),
                                Column :: atom(),
                                RowIndex :: snmp:row_index(),
                                Oid :: snmp:oid(),
                                Value :: term().
```

Send the notification `Notification` to the management targets defined for
notify-name (`name`) in the `snmpNotifyTable` in SNMP-NOTIFICATION-MIB from the
specified `context`.

If no `name` is specified (or if it is `""`), the notification is sent to all
management targets.

If no `context` is specified, the default context, `""`, is used.

The send option `receiver` specifies where information about delivery of
Inform-Requests should be sent. The agent sends Inform-Requests and waits for
acknowledgments from the management targets. The `receiver` can have three
values:

- `no_receiver` \- No information is delivered.
- `t:notification_delivery_info/0` \- The information is delivered via a
  function call according to this data.
- `{tag(), tag_receiver()}` \- The information is delivered either via messages
  or via a function call according to the value of `tag_receiver()`.

  Delivery is done differently depending on the value of `tag_receiver()`:

  - `pid() | registered_name()` \- The info will be delivered in the following
    messages:

    - `{snmp_targets, tag(), Addresses}`

      This informs the user which target addresses the notification was sent to.

    - `{snmp_notification, tag(), {got_response, Address}}`

      This informs the user that this target address acknowledged the
      notification.

    - `{snmp_notification, tag(), {no_response, Address}}`

      This informs the user that this target address did not acknowledge the
      notification.

    The notification is sent as an Inform-Request to each target address in
    `Addresses` and if there are no targets for which an Inform-Request is sent,
    `Addresses` is the empty list `[]`.

    The `tag_receiver()` will first be sent the `snmp_targets` message, and then
    for each address in `Addresses` list, one of the two `snmp_notification`
    messages.

  - `{Mod, Func, Args}` \- The info will be delivered via the function call:

    `Mod:Func([Msg | Args])`

    where `Msg` has the same content and purpose as the messages descrived
    above.

The 'process oid' "tag" that can be provided with the variable name / oids is
intended to be used for oid post processing. The value '`keep`', which is the
default, leaves the oid as is. The value '`truncate`', will cause the oid to be
"truncated". That is, any trailing ".0" will be removed.

> #### Note {: .info }
>
> There is a way to exclude a varbind from the notification. In the normal
> `varbinds` list, providing the special value `'$ignore-oid'` (instead of a
> normal value) will exclude this varbind from the notification.
>
> A define for this has been added to the `snmp_types.hrl` include file,
> `NOTIFICATION_IGNORE_VB_VALUE`.

> #### Note {: .info }
>
> The `extra` info is not normally interpreted by the agent, instead it is
> passed through to the [net-if](snmp_agent_netif.md) process. It is up to the
> implementor of that process to make use of this data.
>
> The version of net-if provided by this application makes no use of this data,
> with one exception: Any tuple containing the atom
> `snmpa_default_notification_extra_info` may be used by the agent and is
> therefore _reserved_.
>
> See the net-if incoming messages for sending a
> [trap](snmp_agent_netif.md#im_send_pdu) and
> [notification](snmp_agent_netif.md#im_send_pdu_req) for more info.

# `send_notification`

```erlang
-spec send_notification(Agent, Notification, Receiver) -> snmp:void()
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               Notification :: atom(),
                               Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                               Tag :: term(),
                               Recv :: pid() | atom() | MFA,
                               MFA :: {Mod, Func, Args},
                               Mod :: module(),
                               Func :: atom(),
                               Args :: list().
```

# `send_notification`

```erlang
-spec send_notification(Agent, Notification, Receiver, Varbinds) -> snmp:void()
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               Notification :: atom(),
                               Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                               Tag :: term(),
                               Recv :: pid() | atom() | MFA,
                               MFA :: {Mod, Func, Args},
                               Mod :: module(),
                               Func :: atom(),
                               Args :: list(),
                               Varbinds :: [Varbind],
                               Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {Oid, Value},
                               Variable :: atom(),
                               Column :: atom(),
                               RowIndex :: snmp:row_index(),
                               Oid :: snmp:oid(),
                               Value :: term().
```

# `send_notification`

```erlang
-spec send_notification(Agent, Notification, Receiver, NotifyName, Varbinds) -> snmp:void()
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               Notification :: atom(),
                               Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                               Tag :: term(),
                               Recv :: pid() | atom() | MFA,
                               MFA :: {Mod, Func, Args},
                               Mod :: module(),
                               Func :: atom(),
                               Args :: list(),
                               NotifyName :: snmp_notification_mib:notify_name(),
                               Varbinds :: [Varbind],
                               Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {Oid, Value},
                               Variable :: atom(),
                               Column :: atom(),
                               RowIndex :: snmp:row_index(),
                               Oid :: snmp:oid(),
                               Value :: term().
```

# `send_notification`

```erlang
-spec send_notification(Agent, Notification, Receiver, NotifyName, ContextName, Varbinds) -> snmp:void()
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               Notification :: atom(),
                               Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                               Tag :: term(),
                               Recv :: pid() | atom() | MFA,
                               MFA :: {Mod, Func, Args},
                               Mod :: module(),
                               Func :: atom(),
                               Args :: list(),
                               NotifyName :: snmp_notification_mib:notify_name(),
                               ContextName :: snmp_community_mib:context_name(),
                               Varbinds :: [Varbind],
                               Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {Oid, Value},
                               Variable :: atom(),
                               Column :: atom(),
                               RowIndex :: snmp:row_index(),
                               Oid :: snmp:oid(),
                               Value :: term().
```

# `send_notification`
*since OTP R14B* 

```erlang
-spec send_notification(Agent, Notification, Receiver, NotifyName, ContextName, Varbinds, LocalEngineID) ->
                           snmp:void()
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               Notification :: atom(),
                               Receiver :: no_receiver | {Tag, Recv} | notification_delivery_info(),
                               Tag :: term(),
                               Recv :: pid() | atom() | MFA,
                               MFA :: {Mod, Func, Args},
                               Mod :: module(),
                               Func :: atom(),
                               Args :: list(),
                               NotifyName :: snmp_notification_mib:notify_name(),
                               ContextName :: snmp_community_mib:context_name(),
                               Varbinds :: [Varbind],
                               Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {Oid, Value},
                               Variable :: atom(),
                               Column :: atom(),
                               RowIndex :: snmp:row_index(),
                               Oid :: snmp:oid(),
                               Value :: term(),
                               LocalEngineID :: snmp_framework_mib:engine_id().
```

Sends the notification `Notification` to the management targets defined for
`NotifyName` in the `snmpNotifyTable` in SNMP-NOTIFICATION-MIB from the
specified context.

If no `NotifyName` is specified (or if it is `""`), the notification is sent to
all management targets (`Addresses` below).

If no `ContextName` is specified, the default `""` context is used.

The parameter `Receiver` specifies where information about delivery of
Inform-Requests should be sent. The agent sends Inform-Requests and waits for
acknowledgments from the managers. `Receiver` can have three values:

- `no_receiver` \- No information is delivered.
- `t:notification_delivery_info/0` \- The information is delivered via a
  function call according to this data.
- `{Tag, Recv}` \- The information is delivered either via messages or via a
  function call according to the value of `Recv`.

If `Receiver` has the value `{Tag, Recv}`, the delivery is done according to
`Recv`:

- `pid() | atom()` \- The info will be delivered in the following messages:

  - `{snmp_targets, Tag, Addresses}`

    This inform the user which target addresses the notification was sent to.

  - `{snmp_notification, Tag, {got_response, Address}}`

    This informs the user that this target address acknowledged the
    notification.

  - `{snmp_notification, Tag, {no_response, Address}}`

    This informs the user that this target address did not acknowledge
    notification.

  The notification is sent as an Inform-Request to each target address in
  `Addresses` and if there are no targets for which an Inform-Request is sent,
  `Addresses` is the empty list `[]`.

  The `receiver` will first be sent the `snmp_targets` message, and then for
  each address in `Addresses` list, one of the two `snmp_notification` messages.

- `{Mod, Func, Args}` \- The info will be delivered via the function call:

  `Mod:Func([Msg | Args])`

  where `Msg` has the same content and purpose as the messages descrived above.

`Address` is a management target address and `Addresses` is a list of management
target addresses. They are defined as followes:

```erlang
        Addresses  = [address()]
        Address    = address()
        address()  = v1_address() | v3_address()
        v1_address() = {TDomain, TAddress}
        v3_address() = {{TDomain, TAddress}, V3MsgData}
        TDomain    = tdoamin()
        TAddress   = taddress()
        tdomain()  = The oid of snmpUDPDomain
                     This is the only supported transport domain.
        taddress() = [A1, A2, A3, A4, P1, P3]
                     The 4 first bytes makes up the IP-address and the last 2,
                     the UDP-port number.
        V3MsgData  = v3_msg_data()
        v3_msg_data() = term()
```

If `Receiver` is a `t:notification_delivery_info/0` record, then the information
about the notification delivery will be delivered to the `receiver` via the
callback functions defined by the `m:snmpa_notification_delivery_info_receiver`
behaviour according to the content of the `t:notification_delivery_info/0`
record.

The optional argument `Varbinds` defines values for the objects in the
notification. If no value is given for an object, the `Agent` performs a
get-operation to retrieve the value.

`Varbinds` is a list of `Varbind`, where each `Varbind` is one of:

- `{Variable, Value}`, where `Variable` is the symbolic name of a scalar
  variable referred to in the notification specification.
- `{Column, RowIndex, Value}`, where `Column` is the symbolic name of a column
  variable. `RowIndex` is a list of indices for the specified element. If this
  is the case, the OBJECT IDENTIFIER sent in the notification is the `RowIndex`
  appended to the OBJECT IDENTIFIER for the table column. This is the OBJECT
  IDENTIFIER which specifies the element.
- `{OID, Value}`, where `OID` is the OBJECT IDENTIFIER for an instance of an
  object, scalar variable, or column variable.

For example, to specify that `sysLocation` should have the value `"upstairs"` in
the notification, we could use one of:

- `{sysLocation, "upstairs"}` or
- `{[1,3,6,1,2,1,1,6,0], "upstairs"}` or
- `{?sysLocation_instance, "upstairs"}` (provided that the generated `.hrl` file
  is included)

If a variable in the notification is a table element, the `RowIndex` for the
element must be given in the `Varbinds` list. In this case, the OBJECT
IDENTIFIER sent in the notification is the OBJECT IDENTIFIER that identifies
this element. This OBJECT IDENTIFIER could be used in a get operation later.

This function is asynchronous, and does not return any information. If an error
occurs, `user_err/2` of the error report module is called and the notification
is discarded.

> #### Note {: .info }
>
> Note that the use of the LocalEngineID argument is only intended for special
> cases, if the agent is to "emulate" multiple EngineIDs\! By default, the agent
> uses the value of `SnmpEngineID` (see SNMP-FRAMEWORK-MIB).

`ExtraInfo` is not normally used in any way by the agent. It is intended to be
passed along to the net-if process, which is a component that a user can
implement themself. The users own net-if may then make use of ExtraInfo. The
net-if provided with this application does not process ExtraInfo.

There is one exception. _Any_ tuple containing the atom
`snmpa_default_notification_extra_info` will, in this context, be considered
belonging to this application, and may be processed by the agent.

# `set_log_type`

```erlang
-spec set_log_type(NewType) -> {ok, OldType} | {error, Reason}
                      when NewType :: snmp:atl_type(), OldType :: snmp:atl_type(), Reason :: term().
```

# `set_log_type`

```erlang
-spec set_log_type(Agent, NewType) -> {ok, OldType} | {error, Reason}
                      when
                          Agent :: pid() | AgentName,
                          AgentName :: atom(),
                          NewType :: snmp:atl_type(),
                          OldType :: snmp:atl_type(),
                          Reason :: term().
```

Changes the run-time Audit Trail log type.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in testing/debugging scenarios.

# `set_request_limit`

```erlang
-spec set_request_limit(NewLimit) -> {ok, OldLimit} | {error, Reason}
                           when
                               NewLimit :: infinity | non_neg_integer(),
                               OldLimit :: infinity | non_neg_integer(),
                               Reason :: term().
```

# `set_request_limit`

```erlang
-spec set_request_limit(Agent, NewLimit) -> {ok, OldLimit} | {error, Reason}
                           when
                               Agent :: pid() | AgentName,
                               AgentName :: atom(),
                               NewLimit :: infinity | non_neg_integer(),
                               OldLimit :: infinity | non_neg_integer(),
                               Reason :: term().
```

Changes the request limit.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in load regulation scenarios.

# `unload_mib`
*since OTP R16B02* 

```erlang
-spec unload_mib(Mib) -> ok | {error, Reason} when Mib :: string(), Reason :: not_loaded | term().
```

# `unload_mib`
*since OTP R16B02* 

```erlang
-spec unload_mib(Agent, Mib) -> ok | {error, Reason}
                    when
                        Agent :: pid() | AgentName,
                        AgentName :: atom(),
                        Mib :: string(),
                        Reason :: not_loaded | term().
```

Unload a single `Mib` from an agent.

# `unload_mibs`

```erlang
-spec unload_mibs(Mibs) -> ok | {error, Reason}
                     when
                         Mibs :: [MibName],
                         MibName :: string(),
                         Reason :: {'unload aborted at', MibName, InternalReason},
                         InternalReason :: not_loaded | term().
```

# `unload_mibs`

```erlang
-spec unload_mibs(Agent, Mibs) -> ok | {error, Reason}
                     when
                         Agent :: pid() | AgentName,
                         AgentName :: atom(),
                         Mibs :: [MibName],
                         MibName :: string(),
                         Reason :: {'unload aborted at', MibName, InternalReason},
                         InternalReason :: not_loaded | term();
                 (Mibs, Force) -> ok | {error, Reason}
                     when
                         Mibs :: [MibName],
                         MibName :: string(),
                         Force :: boolean(),
                         Reason :: {'unload aborted at', MibName, InternalReason},
                         InternalReason :: not_loaded | term().
```

# `unload_mibs`
*since OTP R16B02* 

```erlang
-spec unload_mibs(Agent, Mibs, Force) -> ok | {error, Reason}
                     when
                         Agent :: pid() | AgentName,
                         AgentName :: atom(),
                         Mibs :: [MibName],
                         MibName :: string(),
                         Force :: boolean(),
                         Reason :: {'unload aborted at', MibName, InternalReason},
                         InternalReason :: not_loaded | term().
```

Unload `Mibs` from an agent. If it cannot unload all MIBs (the default value of
the `Force` argument is `false`), it will indicate where unloading was aborted.

If `Force = true` then the agent will continue attempting to unload each mib
even after failing to unload a previous mib. Use with care.

# `unregister_notification_filter`

```erlang
-spec unregister_notification_filter(Id) -> ok | {error, Reason}
                                        when Id :: nfilter_id(), Reason :: term().
```

# `unregister_notification_filter`

```erlang
-spec unregister_notification_filter(Agent, Id) -> ok | {error, Reason}
                                        when
                                            Agent :: pid() | AgentName,
                                            AgentName :: atom(),
                                            Id :: nfilter_id(),
                                            Reason :: term().
```

Unregister a notification filter.

# `unregister_subagent`

```erlang
-spec unregister_subagent(Agent, SubAgentOidOrPid) -> ok | {error, Reason}
                             when
                                 Agent :: pid() | AgentName,
                                 AgentName :: atom(),
                                 SubAgentOidOrPid :: snmp:oid() | pid(),
                                 Reason :: term().
```

Unregister a sub-agent. If the second argument is a pid, then that sub-agent
will be unregistered from all trees in `Agent`.

# `update_mibs_cache_age`

```erlang
-spec update_mibs_cache_age(Age) -> ok | {error, Reason} when Age :: pos_integer(), Reason :: term().
```

# `update_mibs_cache_age`

```erlang
-spec update_mibs_cache_age(Agent, Age) -> ok | {error, Reason}
                               when
                                   Agent :: pid() | AgentName,
                                   AgentName :: atom(),
                                   Age :: pos_integer(),
                                   Reason :: term().
```

Change the mib server cache `age` property.

# `update_mibs_cache_gclimit`

```erlang
-spec update_mibs_cache_gclimit(GcLimit) -> ok | {error, Reason}
                                   when GcLimit :: pos_integer(), Reason :: term().
```

# `update_mibs_cache_gclimit`

```erlang
-spec update_mibs_cache_gclimit(Agent, GcLimit) -> ok | {error, Reason}
                                   when
                                       Agent :: pid() | AgentName,
                                       AgentName :: atom(),
                                       GcLimit :: pos_integer(),
                                       Reason :: term().
```

Change the mib server cache `gclimit` property.

# `verbosity`

```erlang
-spec verbosity(Target, Verbosity) -> snmp:void()
                   when
                       Target :: all | LogicalName | PidOrAgentName,
                       LogicalName :: net_if | note_store | mib_server | symbolic_store | local_db,
                       PidOrAgentName :: pid() | master_agent | atom(),
                       Verbosity :: SNMPVerb | SubAgent,
                       SNMPVerb :: snmp:verbosity(),
                       SubAgent :: {subagents, snmp:verbosity()}.
```

Sets 'verbosity' for the indicated process(s):

- **`all`** -
  Sets verbosity for all the agent processes; net_if, note_store, mib_server,
symbolic_store, local_db and master_agent (and sub-agents).

- **`net_if`** - Sets verbosity for the net-if process.

- **`note_store`** - Sets verbosity for the note store process.

- **`mib_server`** - Sets verbosity for the mib server process.

- **`symbolic_store`** - Sets verbosity for the symbolic store process.

- **`local_db`** - Sets verbosity for the local-db process.

- **`master_agent | pid()`** when `Verbosity = {subagents,` [`snmp:verbosity()`](`t:snmp:verbosity/0`)`}` -
  Sets verbosity for all sub-agent(s) controlled by this (master) agent.

- **`master_agent | pid() | atom()`** - Sets verbosity for the agent process.

The following text documents expected input-output relations

- If `Target :: all | net_if | note_store | mib_server | symbolic_store | local_db`,
  then `Verbosity :: snmp:verbosity()`.

- If `Target :: master_agent`,
  then `Verbosity :: {subagents, snmp:verbosity()}`

- If `Target :: pid() | atom()`,
  then `Verbosity :: snmp:verbosity() | {subagents, snmp:verbosity()}`.

# `whereis_mib`

```erlang
-spec whereis_mib(MibName) -> {ok, MibFile} | {error, Reason}
                     when MibName :: atom(), MibFile :: string(), Reason :: term().
```

# `whereis_mib`

```erlang
-spec whereis_mib(Agent, MibName) -> {ok, MibFile} | {error, Reason}
                     when
                         Agent :: pid() | AgentName,
                         AgentName :: atom(),
                         MibName :: atom(),
                         MibFile :: string(),
                         Reason :: term().
```

Get the full path to the (compiled) mib-file.

# `which_aliasnames`

```erlang
-spec which_aliasnames() -> AliasNames when AliasNames :: [AliasName], AliasName :: atom().
```

Retrieve all alias-names known to the agent.

# `which_mibs`

```erlang
-spec which_mibs() -> Mibs when Mibs :: [{MibName, MibFile}], MibName :: atom(), MibFile :: string().
```

# `which_mibs`

```erlang
-spec which_mibs(Agent) -> Mibs
                    when
                        Agent :: pid() | AgentName,
                        AgentName :: atom(),
                        Mibs :: [{MibName, MibFile}],
                        MibName :: atom(),
                        MibFile :: string().
```

Retrieve the list of all the mibs loaded into this agent. Default is the master
agent.

# `which_mibs_cache_size`
*since OTP R14B* 

```erlang
-spec which_mibs_cache_size() -> {ok, Size} | {error, Reason}
                               when Size :: non_neg_integer(), Reason :: term().
```

# `which_mibs_cache_size`
*since OTP R14B* 

```erlang
-spec which_mibs_cache_size(Agent) -> {ok, Size} | {error, Reason}
                               when
                                   Agent :: pid() | AgentName,
                                   AgentName :: atom(),
                                   Size :: non_neg_integer(),
                                   Reason :: term().
```

Retrieve the size of the mib server cache.

# `which_notification_filter`

```erlang
-spec which_notification_filter() -> Filters when Filters :: [FilterId], FilterId :: nfilter_id().
```

# `which_notification_filter`

```erlang
-spec which_notification_filter(Agent) -> Filters
                                   when
                                       Agent :: pid() | AgentName,
                                       AgentName :: atom(),
                                       Filters :: [FilterId],
                                       FilterId :: nfilter_id().
```

List all notification filters in an agent.

# `which_notifications`

```erlang
-spec which_notifications() -> Notifications
                             when
                                 Notifications :: [{Name, MibName, Info}],
                                 Name :: atom(),
                                 MibName :: atom(),
                                 Info :: term().
```

Retrieve all notifications (and traps) known to the agent.

# `which_tables`

```erlang
-spec which_tables() -> Tables when Tables :: [Table], Table :: atom().
```

Retrieve all tables known to the agent.

# `which_transports`
*since OTP 23.3* 

```erlang
-spec which_transports() -> Transports
                          when
                              Transports :: [Transport],
                              Transport :: {TDomain, TAddress} | {TDomain, TAddress, Kind},
                              TDomain :: snmp:tdomain(),
                              TAddress :: {IpAddr, IpPort},
                              IpAddr :: inet:ip_address(),
                              IpPort :: inet:port_number(),
                              Kind :: transport_kind().
```

Retrieve all configured transports.

# `which_variables`

```erlang
-spec which_variables() -> Variables when Variables :: [Variable], Variable :: atom().
```

Retrieve all variables known to the agent.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
