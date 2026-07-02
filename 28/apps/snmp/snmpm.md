# `snmpm`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/snmp/src/manager/snmpm.erl#L23)

Interface functions to the SNMP toolkit manager

The module `snmpm` contains interface functions to the SNMP manager.

# `agent_config`

```elixir
-type agent_config() ::
          {engine_id, snmp:engine_id()} |
          {address, inet:ip_address()} |
          {port, inet:port_number()} |
          {tdomain, snmp:tdomain()} |
          {community, snmp:community()} |
          {timeout, register_timeout()} |
          {max_message_size, snmp:mms()} |
          {version, snmp:version()} |
          {sec_model, snmp:sec_model()} |
          {sec_name, snmp:sec_name()} |
          {sec_level, snmp:sec_level()}.
```

# `agent_config_item`

```elixir
-type agent_config_item() ::
          engine_id | address | port | tdomain | community | timeout | max_message_size | version |
          sec_model | sec_name | sec_level.
```

Value type depend on the item according to:

- **`engine_id`** - Engine ID of the agent.

  Value type: [engine_id()](`t:snmp:engine_id/0`)

- **`address`** - The IP address of the agent.

  Value type: [ip_address()](`t:inet:ip_address/0`)

- **`port`** - Port number of the agent.

  Value type: [port_number()](`t:inet:port_number/0`)

- **`tdomain`** - Transport domain.

  Value type: [tdomain()](`t:snmp:tdomain/0`)

- **`community`** - Community.

  Value type: [community()](`t:snmp:community/0`)

- **`timeout`** - Registration timeout.

  Value type: `t:register_timeout/0`

- **`max_message_size`** - Max Message Size of a message.

  Value type: [mms()](`t:snmp:mms/0`)

- **`version`** - What SNMP version is used when communicating with this agent.

  Value type: [version()](`t:snmp:version/0`)

- **`sec_model`** - Security Model.

  Value type: [sec_model()](`t:snmp:sec_model/0`)

- **`sec_name`** - Security Name.

  Value type: [sec_name()](`t:snmp:sec_name/0`)

- **`sec_level`** - Security Level.

  Value type: [sec_level()](`t:snmp:sec_level/0`)

# `pdu_type`

```elixir
-type pdu_type() :: snmp:pdu_type() | trappdu.
```

# `register_timeout`

```elixir
-type register_timeout() :: pos_integer() | snmp:snmp_timer().
```

The time to complete a (agent) registration.

# `request_id`

```elixir
-opaque request_id() :: term().
```

Is a unique term that identifies a request.

# `snmp_reply`

```elixir
-type snmp_reply() :: {snmp:error_status(), snmp:error_index(), [snmp:varbind()]}.
```

# `snmpm_user`

```elixir
-type snmpm_user() :: module().
```

Module implementing the [snmpm_user](`m:snmpm_user#`) behaviour.

# `target_name`

```elixir
-type target_name() :: string().
```

Is a unique _non-empty_ string.

# `user_id`

```elixir
-type user_id() :: term().
```

Is a unique term that identifies a user.

# `usm_config_item`

```elixir
-type usm_config_item() :: sec_name | auth | auth_key | priv | priv_key.
```

Value type depend on the item according to:

- **`sec_name`** - Security Name.

  Value type: [`snmp:sec_name()`](`t:snmp:sec_name/0`)

- **`auth`** - Authentication protocol.

  Value type: [`snmp:usm_auth_protocol()`](`t:snmp:usm_auth_protocol/0`)

- **`auth_key`** - Authentication key.

  Value type: [`snmp:usm_auth_key()`](`t:snmp:usm_auth_key/0`)

- **`priv`** - Privacy protocol.

  Value type: [`snmp:usm_priv_protocol()`](`t:snmp:usm_priv_protocol/0`)

- **`priv_key`** - Privacy key.

  Value type: [`snmp:usm_priv_key()`](`t:snmp:usm_priv_key/0`)

# `value_type`

```elixir
-type value_type() :: o | i | u | g | s | s | b | ip | op | c32 | c64 | tt.
```

- **`o - 'OBJECT IDENTIFIER'`**

- **`i - 'INTEGER'`**

- **`u - 'Unsigned32`**

- **`g - 'Unsigned32'`**

- **`s - 'OCTET STRING'`**

- **`b - 'BITS'`**

- **`ip - 'IpAddress'`**

- **`op - 'Opaque'`**

- **`c32 - 'Counter32'`**

- **`c64 - 'Counter64'`**

- **`tt - 'TimeTicks'`**

# `var_and_val`

```elixir
-type var_and_val() ::
          {OID :: snmp:oid(), ValueType :: value_type(), Value :: term()} |
          {OID :: snmp:oid(), Value :: term()}.
```

# `agent_info`

```elixir
-spec agent_info(TargetName, Item) -> {ok, Value} | {error, Reason}
                    when
                        TargetName :: target_name(),
                        Item :: agent_config_item(),
                        Value :: term(),
                        Reason :: term().
```

Retrieve agent config.

# `async_get2`
*since OTP R14B03* 

```elixir
-spec async_get2(UserId, TargetName, Oids) -> {ok, ReqId} | {error, Reason}
                    when
                        UserId :: user_id(),
                        TargetName :: target_name(),
                        Oids :: [snmp:oid()],
                        ReqId :: request_id(),
                        Reason :: term().
```

# `async_get2`
*since OTP R14B03* 

```elixir
-spec async_get2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error, Reason}
                    when
                        UserId :: user_id(),
                        TargetName :: target_name(),
                        Oids :: [snmp:oid()],
                        SendOpts :: [SendOpt],
                        SendOpt ::
                            {context, snmp:context_name()} |
                            {timeout, pos_integer()} |
                            {community, snmp:community()} |
                            {sec_model, snmp:sec_model()} |
                            {sec_name, snmp:sec_name()} |
                            {sec_level, snmp:sec_level()} |
                            {max_message_size, snmp:mms()} |
                            {extra, term()},
                        ReqId :: request_id(),
                        Reason :: term().
```

Asynchronous `get-request`.

The reply, if it arrives, will be delivered to the user through a call to the
`m:snmpm_user` callback function [`handle_pdu`](`c:snmpm_user:handle_pdu/4`).

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
net-if) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

# `async_get_bulk2`
*since OTP R14B03* 

```elixir
-spec async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) -> {ok, ReqId} | {error, Reason}
                         when
                             UserId :: user_id(),
                             TargetName :: target_name(),
                             NonRep :: non_neg_integer(),
                             MaxRep :: non_neg_integer(),
                             Oids :: [snmp:oid()],
                             ReqId :: request_id(),
                             Reason :: term().
```

# `async_get_bulk2`
*since OTP R14B03* 

```elixir
-spec async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) ->
                         {ok, ReqId} | {error, Reason}
                         when
                             UserId :: user_id(),
                             TargetName :: target_name(),
                             NonRep :: non_neg_integer(),
                             MaxRep :: non_neg_integer(),
                             Oids :: [snmp:oid()],
                             SendOpts :: [SendOpt],
                             SendOpt ::
                                 {context, snmp:context_name()} |
                                 {timeout, pos_integer()} |
                                 {community, snmp:community()} |
                                 {sec_model, snmp:sec_model()} |
                                 {sec_name, snmp:sec_name()} |
                                 {sec_level, snmp:sec_level()} |
                                 {max_message_size, snmp:mms()} |
                                 {extra, term()},
                             ReqId :: request_id(),
                             Reason :: term().
```

Asynchronous `get-bulk-request` (See RFC1905).

The reply, if it arrives, will be delivered to the user through a call to the
`m:snmpm_user` callback function [`handle_pdu`](`c:snmpm_user:handle_pdu/4`).

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes no use of
this info, so the only use for it in such a configuration (when using the built
in `net-if`) would be tracing.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

# `async_get_next2`
*since OTP R14B03* 

```elixir
-spec async_get_next2(UserId, TargetName, Oids) -> {ok, ReqId} | {error, Reason}
                         when
                             UserId :: user_id(),
                             TargetName :: target_name(),
                             Oids :: [snmp:oid()],
                             ReqId :: request_id(),
                             Reason :: term().
```

# `async_get_next2`
*since OTP R14B03* 

```elixir
-spec async_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error, Reason}
                         when
                             UserId :: user_id(),
                             TargetName :: target_name(),
                             Oids :: [snmp:oid()],
                             SendOpts :: [SendOpt],
                             SendOpt ::
                                 {context, snmp:context_name()} |
                                 {timeout, pos_integer()} |
                                 {community, snmp:community()} |
                                 {sec_model, snmp:sec_model()} |
                                 {sec_name, snmp:sec_name()} |
                                 {sec_level, snmp:sec_level()} |
                                 {max_message_size, snmp:mms()} |
                                 {extra, term()},
                             ReqId :: request_id(),
                             Reason :: term().
```

Asynchronous `get-next-request`.

The reply, if it arrives, will be delivered to the user through a call to the
`m:snmpm_user` callback function [`handle_pdu`](`c:snmpm_user:handle_pdu/4`).

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
`net-if`) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

# `async_set2`
*since OTP R14B03* 

```elixir
-spec async_set2(UserId, TargetName, VarsAndVals) -> {ok, ReqId} | {error, Reason}
                    when
                        UserId :: user_id(),
                        TargetName :: target_name(),
                        VarsAndVals :: [var_and_val()],
                        ReqId :: request_id(),
                        Reason :: term().
```

# `async_set2`
*since OTP R14B03* 

```elixir
-spec async_set2(UserId, TargetName, VarsAndVals, SendOpts) -> {ok, ReqId} | {error, Reason}
                    when
                        UserId :: user_id(),
                        TargetName :: target_name(),
                        VarsAndVals :: [var_and_val()],
                        SendOpts :: [SendOpt],
                        SendOpt ::
                            {context, snmp:context_name()} |
                            {timeout, pos_integer()} |
                            {community, snmp:community()} |
                            {sec_model, snmp:sec_model()} |
                            {sec_name, snmp:sec_name()} |
                            {sec_level, snmp:sec_level()} |
                            {max_message_size, snmp:mms()} |
                            {extra, term()},
                        ReqId :: request_id(),
                        Reason :: term().
```

Asynchronous `set-request`.

The reply will be delivered to the user through a call to the
`m:snmpm_user` callback function [`handle_pdu`](`c:snmpm_user:handle_pdu/4`).

The send option `timeout` specifies for how long the request is valid (after
which the manager is free to delete it).

When _var_and_val()_ is _\{oid(), value()\}_, the manager makes an educated
guess based on the loaded mibs.

The send option `extra` specifies an opaque data structure passed on to the
net-if process. The net-if process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
net-if) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

# `backup`

```elixir
-spec backup(BackupDir) -> ok | {error, Reason} when BackupDir :: snmp:dir(), Reason :: term().
```

Backup persistent data handled by the manager.

BackupDir cannot be identical to DbDir.

# `cancel_async_request`

```elixir
-spec cancel_async_request(UserId, ReqId) -> ok | {error, Reason}
                              when UserId :: user_id(), ReqId :: request_id(), Reason :: term().
```

Cancel a previous asynchronous request.

# `cancel_notify_started`

```elixir
-spec cancel_notify_started(Pid) -> snmp:void() when Pid :: pid().
```

Cancel a previous request to be notified of SNMP manager start.

# `change_log_size`

```elixir
-spec change_log_size(NewSize) -> ok | {error, Reason} when NewSize :: snmp:log_size(), Reason :: term().
```

Changes the log size of the Audit Trail Log. The application must be configured
to use the audit trail log function. Please refer to disk_log(3) in Kernel
Reference Manual for a description of how to change the log size.

The change is permanent, as long as the log is not deleted. That means, the log
size is remembered across reboots.

# `demonitor`

```elixir
-spec demonitor(Ref) -> true when Ref :: reference().
```

Turn off monitoring of the SNMP manager.

# `format_reason`

```elixir
-spec format_reason(Reason) -> FReason when Reason :: term(), FReason :: string().
```

# `format_reason`

```elixir
-spec format_reason(Prefix, Reason) -> FReason
                       when
                           Prefix :: non_neg_integer() | string(), Reason :: term(), FReason :: string().
```

This utility function is used to create a formatted (pretty printable) string of
the error reason received from either:

- The `Reason` returned value if any of the sync/async get/get-next/set/get-bulk
  functions returns `{error, Reason}`
- The `Reason` parameter in the [handle_error](`c:snmpm_user:handle_error/3`) user
  callback function.

`Prefix` should either be an indentation string (e.g. a list of spaces) or a
positive integer (which will be used to create the indentation string of that
length).

# `info`

```elixir
-spec info() -> [{Key, Value}] when Key :: atom(), Value :: term().
```

Returns a list (a dictionary) containing information about the manager.
Information includes statistics counters, miscellaneous info about each process
(e.g. memory allocation), and so on.

# `load_mib`

```elixir
-spec load_mib(MibName) -> ok | {error, Reason} when MibName :: snmp:mib_name(), Reason :: term().
```

Load a `Mib` into the manager. The `MibName` is the name of the Mib, including
the path to where the compiled mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpm:load_mib(Dir ++ "MY-MIB").
```

# `log_to_io`
*since OTP R15B01* 

```elixir
-spec log_to_io(LogDir) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```elixir
-spec log_to_io(LogDir, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Mibs :: [snmp:mib_name()],
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```elixir
-spec log_to_io(LogDir, Mibs, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Mibs :: [snmp:mib_name()],
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```elixir
-spec log_to_io(LogDir, Mibs, LogName, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
                       Mibs :: [snmp:mib_name()],
                       LogName :: string(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
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

```elixir
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
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
                       LogDir :: snmp:dir(),
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

```elixir
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
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
                       LogDir :: snmp:dir(),
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

```elixir
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: snmp:dir(),
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
`LogName` defaults to "snmpm_log". `LogFile` defaults to "snmpm.log".

The `Block` argument indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

`Start` and `Stop` indicates which log entries should be converted,
from when (`Start`) to when (`Stop`). `Start = null` => Start from the
beginning of the log. `Stop = null` => Stop the conversion at the end
of the log. Defaults to `Start = null` and `Stop = null` (the entire log).

See [`snmp:log_to_io/7`](`snmp:log_to_io/7`) for more info.

# `log_to_txt`
*since OTP R16B03* 

```elixir
-spec log_to_txt(LogDir :: snmp:dir()) -> snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(), Block :: boolean()) -> snmp:void();
                (LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()]) -> snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()], Block :: boolean()) -> snmp:void();
                (LogDir :: snmp:dir(), Mibs :: [snmp:mib_name()], OutFile :: file:filename()) ->
                    snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 Block :: boolean()) ->
                    snmp:void();
                (LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string()) ->
                    snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 Block :: boolean()) ->
                    snmp:void();
                (LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string()) ->
                    snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string(),
                 Block :: boolean()) ->
                    snmp:void();
                (LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string(),
                 Start :: null | snmp:log_time()) ->
                    snmp:void().
```

# `log_to_txt`

```elixir
-spec log_to_txt(LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string(),
                 Block :: boolean(),
                 Start :: null | snmp:log_time()) ->
                    snmp:void();
                (LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string(),
                 Start :: null | snmp:log_time(),
                 Stop :: null | snmp:log_time()) ->
                    snmp:void().
```

# `log_to_txt`
*since OTP R16B03* 

```elixir
-spec log_to_txt(LogDir :: snmp:dir(),
                 Mibs :: [snmp:mib_name()],
                 OutFile :: file:filename(),
                 LogName :: string(),
                 LogFile :: string(),
                 Block :: boolean(),
                 Start :: snmp:log_time(),
                 Stop :: snmp:log_time()) ->
                    snmp:void().
```

Converts an Audit Trail Log to a readable text file. `OutFile` defaults to
"./snmpm_log.txt". `LogName` defaults to "snmpm_log". `LogFile` defaults to
"snmpm.log".

The `Block` argument indicates if the log should be blocked during conversion.
This could be useful when converting large logs (when otherwise the log could
wrap during conversion). Defaults to `true`.

`Start` and `Stop` indicates which log entries should be converted,
from when (`Start`) to when (`Stop`). `Start = null` => Start from the
beginning of the log. `Stop = null` => Stop the conversion at the end
of the log. Defaults to `Start = null` and `Stop = null` (the entire log).

See [`snmp:log_to_txt/8`](`snmp:log_to_txt/8`) for more info.

# `monitor`

```elixir
-spec monitor() -> MRef when MRef :: reference().
```

Monitor the SNMP manager. In case of a crash, the calling (monitoring) process
will get a 'DOWN' message (see the erlang module for more info).

# `name_to_oid`

```elixir
-spec name_to_oid(AliasName) -> {ok, OIDs} | {error, Reason}
                     when AliasName :: atom(), OIDs :: [snmp:oid()], Reason :: term().
```

Transform a alias-name to its oid.

Note that an alias-name is only unique within the mib, so when loading several
mib's into a manager, there might be several instances of the same aliasname.

# `notify_started`

```elixir
-spec notify_started(Timeout) -> Pid when Timeout :: non_neg_integer(), Pid :: pid();
                    (Args) -> Pid when Args :: map(), Pid :: pid().
```

Request a notification (message) when the SNMP manager has started.

The `Timeout` is the time the request is valid. The value has to be greater then
zero.

The `Pid` is the process handling the supervision of the SNMP manager start.
When the manager has started a completion message will be sent to the client
from this process: `{snmpm_started, Pid}`. If the SNMP manager was not started
in time, a timeout message will be sent to the client:
`{snmpm_start_timeout, Pid}`.

A client application that is dependent on the SNMP manager will use this
function in order to be notified of when the manager has started. There are two
situations when this is useful:

- During the start of a system, when a client application _could_ start prior to
  the SNMP manager but is dependent upon it, and therefore has to wait for it to
  start.
- When the SNMP manager has crashed, the dependent client application has to
  wait for the SNMP manager to be restarted before it can _reconnect_.

The function returns the pid() of a handler process, that does the supervision
on behalf of the client application. Note that the client application is linked
to this handler.

This function is used in conjunction with the monitor function.

# `oid_to_name`

```elixir
-spec oid_to_name(OID) -> {ok, AliasName} | {error, Reason}
                     when OID :: snmp:oid(), AliasName :: atom(), Reason :: term().
```

Transform a oid to its aliasname.

# `oid_to_type`

```elixir
-spec oid_to_type(OID) -> {ok, Type} | {error, Reason}
                     when OID :: snmp:oid(), Type :: atom(), Reason :: term().
```

Retrieve the type (asn1 bertype) of an oid.

# `register_agent`

```elixir
-spec register_agent(UserId, TargetName, Config) -> ok | {error, Reason}
                        when
                            UserId :: user_id(),
                            TargetName :: target_name(),
                            Config :: [ConfigEntry],
                            ConfigEntry :: {Item, Value},
                            Item :: agent_config_item(),
                            Value :: term(),
                            Reason :: term().
```

Explicitly instruct the manager to handle this agent, with `UserId` as the
responsible user.

Called to instruct the manager that this agent shall be handled. This function
is used when the user knows in advance which agents the manager shall handle.
Note that there is an alternate way to do the same thing: Add the agent to the
manager config files (see [agents.conf](snmp_manager_config_files.md#agents)).

`TargetName` is a non-empty string, uniquely identifying the agent.

The type of `Val` depends on `Item`:

```text
[mandatory] engine_id = engine_id()
[mandatory] address = inet:ip_address()  % Depends on tdomain
[optional]  port = inet:port_number()
[optional]  tdomain = snmp:tdomain()
[optional]  community = snmp:community()
[optional]  timeout = register_timeout()
[optional]  max_message_size = snmp:mms()
[optional]  version = snmp:version()
[optional]  sec_model = snmp:sec_model()
[optional]  sec_name = snmp:sec_name()
[optional]  sec_level = snmp:sec_level()
```

Note that if no `tdomain` is given, the default value, `transportDomainUdpIpv4`,
is used.

Note that if no `port` is given and if `taddress` does not contain a port
number, the default value is used.

# `register_user`

```elixir
-spec register_user(UserId, Module, Data) -> ok | {error, Reason}
                       when
                           UserId :: user_id(), Module :: snmpm_user(), Data :: term(), Reason :: term().
```

# `register_user`

```elixir
-spec register_user(UserId, Module, Data, DefaultAgentConfig) -> ok | {error, Reason}
                       when
                           UserId :: user_id(),
                           Module :: snmpm_user(),
                           Data :: term(),
                           DefaultAgentConfig :: [DefaultConfigEntry],
                           DefaultConfigEntry :: {Item, Value},
                           Item :: agent_config_item(),
                           Value :: term(),
                           Reason :: term().
```

Register the manager entity (=user) responsible for specific agent(s).

`Module` is the callback module (`m:snmpm_user` behaviour) which will be called
whenever something happens (detected agent, incoming reply or incoming
trap/notification).

`Data` is an opaque data structure, not inspected by the manager, that will be
included in all callback calls to the `Module` callback module (`m:snmpm_user`
behaviour).

The argument `DefaultAgentConfig` is used as default values when this user
register agents.

Note that this operation (register user) could have already been done as a
consequence of the node config. (see users.conf).

# `register_user_monitor`

```elixir
-spec register_user_monitor(UserId, Module, Data) -> ok | {error, Reason}
                               when
                                   UserId :: user_id(),
                                   Module :: snmpm_user(),
                                   Data :: term(),
                                   Reason :: term().
```

# `register_user_monitor`

```elixir
-spec register_user_monitor(UserId, Module, Data, DefaultAgentConfig) -> ok | {error, Reason}
                               when
                                   UserId :: user_id(),
                                   Module :: snmpm_user(),
                                   Data :: term(),
                                   DefaultAgentConfig :: [DefaultConfigEntry],
                                   DefaultConfigEntry :: {Item, Value},
                                   Item :: agent_config_item(),
                                   Value :: term(),
                                   Reason :: term().
```

Register the monitored manager entity (=user) responsible for specific agent(s).

The process performing the registration will be monitored. Which means that if
that process should die, all agents registered by that user process will be
unregistered. All outstanding requests will be canceled.

`Module` is the callback module (`m:snmpm_user` behaviour) which will be called
whenever something happens (detected agent, incoming reply or incoming
trap/notification).

`Data` is an opaque data structure, not inspected by the manager, that will be
included in all callback calls to the `Module` callback module (`m:snmpm_user` 
behaviour).

The argument `DefaultAgentConfig` is used as default values when this user
register agents.

# `register_usm_user`

```elixir
-spec register_usm_user(EngineID, UserName, Config) -> ok | {error, Reason}
                           when
                               EngineID :: snmp:engine_id(),
                               UserName :: snmp:usm_name(),
                               Config :: [ConfigEntry],
                               ConfigEntry :: {Item, Value},
                               Item :: usm_config_item(),
                               Value :: term(),
                               Reason :: term().
```

Explicitly instruct the manager to handle this USM user. Note that there is an
alternate way to do the same thing: Add the usm user to the manager config files
(see [usm.conf](snmp_manager_config_files.md#security-data-for-usm)).

# `restart`
*since OTP 22.3* 

```elixir
-spec restart(What) -> snmp:void() when What :: net_if.
```

Restart the indicated process (`What`). Note that its not without risk to
restart a process, and should therefore be used with care.

# `set_log_type`

```elixir
-spec set_log_type(NewType) -> {ok, OldType} | {error, Reason}
                      when NewType :: snmp:atl_type(), OldType :: snmp:atl_type(), Reason :: term().
```

Changes the run-time Audit Trail log type.

Note that this has no effect on the application configuration as defined by
configuration files, so a node restart will revert the config to whatever is in
those files.

This function is primarily useful in testing/debugging scenarios.

# `sync_get2`
*since OTP R14B03* 

```elixir
-spec sync_get2(UserId, TargetName, Oids) -> {ok, SnmpReply, Remaining} | {error, Reason}
                   when
                       UserId :: user_id(),
                       TargetName :: target_name(),
                       Oids :: [snmp:oid()],
                       SnmpReply :: snmp_reply(),
                       Remaining :: non_neg_integer(),
                       Reason ::
                           {send_failed, ReqId, ActualReason} |
                           {invalid_sec_info, SecInfo, SnmpInfo} |
                           term(),
                       ReqId :: request_id(),
                       ActualReason :: term(),
                       SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                       SecTag :: atom(),
                       ExpectedValue :: term(),
                       ReceivedValue :: term(),
                       SnmpInfo :: term().
```

# `sync_get2`
*since OTP R14B03* 

```elixir
-spec sync_get2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason}
                   when
                       UserId :: user_id(),
                       TargetName :: target_name(),
                       Oids :: [snmp:oid()],
                       SendOpts :: [SendOpt],
                       SendOpt ::
                           {context, snmp:context_name()} |
                           {timeout, pos_integer()} |
                           {community, snmp:community()} |
                           {sec_model, snmp:sec_model()} |
                           {sec_name, snmp:sec_name()} |
                           {sec_level, snmp:sec_level()} |
                           {max_message_size, snmp:mms()} |
                           {extra, term()},
                       SnmpReply :: snmp_reply(),
                       Remaining :: non_neg_integer(),
                       Reason ::
                           {send_failed, ReqId, ActualReason} |
                           {invalid_sec_info, SecInfo, SnmpInfo} |
                           term(),
                       ReqId :: request_id(),
                       ActualReason :: term(),
                       SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                       SecTag :: atom(),
                       ExpectedValue :: term(),
                       ReceivedValue :: term(),
                       SnmpInfo :: term().
```

Synchronous `get-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the `net-if` process
failed to send the (`get-request` ) message.
This could happen because of any number of reasons, i.e. encoding error.
_ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
`net-if`) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`c:snmpm_user:handle_report/3`).

# `sync_get_bulk2`
*since OTP R14B03* 

```elixir
-spec sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) ->
                        {ok, SnmpReply, Remaining} | {error, Reason}
                        when
                            UserId :: user_id(),
                            TargetName :: target_name(),
                            NonRep :: non_neg_integer(),
                            MaxRep :: non_neg_integer(),
                            Oids :: [snmp:oid()],
                            SnmpReply :: snmp_reply(),
                            Remaining :: non_neg_integer(),
                            Reason ::
                                {send_failed, ReqId, ActualReason} |
                                {invalid_sec_info, SecInfo, SnmpInfo} |
                                term(),
                            ReqId :: request_id(),
                            ActualReason :: term(),
                            SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                            SecTag :: atom(),
                            ExpectedValue :: term(),
                            ReceivedValue :: term(),
                            SnmpInfo :: term().
```

# `sync_get_bulk2`
*since OTP R14B03* 

```elixir
-spec sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) ->
                        {ok, SnmpReply, Remaining} | {error, Reason}
                        when
                            UserId :: user_id(),
                            TargetName :: target_name(),
                            NonRep :: non_neg_integer(),
                            MaxRep :: non_neg_integer(),
                            Oids :: [snmp:oid()],
                            SendOpts :: [SendOpt],
                            SendOpt ::
                                {context, snmp:context_name()} |
                                {timeout, pos_integer()} |
                                {community, snmp:community()} |
                                {sec_model, snmp:sec_model()} |
                                {sec_name, snmp:sec_name()} |
                                {sec_level, snmp:sec_level()} |
                                {max_message_size, snmp:mms()} |
                                {extra, term()},
                            ReqId :: request_id(),
                            SnmpReply :: snmp_reply(),
                            Remaining :: non_neg_integer(),
                            Reason ::
                                {send_failed, ReqId, ActualReason} |
                                {invalid_sec_info, SecInfo, SnmpInfo} |
                                term(),
                            ReqId :: request_id(),
                            ActualReason :: term(),
                            SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                            SecTag :: atom(),
                            ExpectedValue :: term(),
                            ReceivedValue :: term(),
                            SnmpInfo :: term().
```

Synchronous `get-bulk-request` (See RFC1905).

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the `net-if` process
failed to send the message. This could happen because of any number of reasons,
i.e. encoding error. _ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
`net-if`) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[`snmpm_user:handle_report/3`](`c:snmpm_user:handle_report/3`).

# `sync_get_next2`
*since OTP R14B03* 

```elixir
-spec sync_get_next2(UserId, TargetName, Oids) -> {ok, SnmpReply, Remaining} | {error, Reason}
                        when
                            UserId :: user_id(),
                            TargetName :: target_name(),
                            Oids :: [snmp:oid()],
                            SnmpReply :: snmp_reply(),
                            Remaining :: non_neg_integer(),
                            Reason ::
                                {send_failed, ReqId, ActualReason} |
                                {invalid_sec_info, SecInfo, SnmpInfo} |
                                term(),
                            ReqId :: request_id(),
                            ActualReason :: term(),
                            SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                            SecTag :: atom(),
                            ExpectedValue :: term(),
                            ReceivedValue :: term(),
                            SnmpInfo :: term().
```

# `sync_get_next2`
*since OTP R14B03* 

```elixir
-spec sync_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason}
                        when
                            UserId :: user_id(),
                            TargetName :: target_name(),
                            Oids :: [snmp:oid()],
                            SendOpts :: [SendOpt],
                            SendOpt ::
                                {context, snmp:context_name()} |
                                {timeout, pos_integer()} |
                                {community, snmp:community()} |
                                {sec_model, snmp:sec_model()} |
                                {sec_name, snmp:sec_name()} |
                                {sec_level, snmp:sec_level()} |
                                {max_message_size, snmp:mms()} |
                                {extra, term()},
                            SnmpReply :: snmp_reply(),
                            Remaining :: non_neg_integer(),
                            Reason ::
                                {send_failed, ReqId, ActualReason} |
                                {invalid_sec_info, SecInfo, SnmpInfo} |
                                term(),
                            ReqId :: request_id(),
                            ActualReason :: term(),
                            SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                            SecTag :: atom(),
                            ExpectedValue :: term(),
                            ReceivedValue :: term(),
                            SnmpInfo :: term().
```

Synchronous `get-next-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the `net-if` process
failed to send the message.
This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
`net-if`) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[handle_report](`c:snmpm_user:handle_report/3`).

# `sync_set2`
*since OTP R14B03* 

```elixir
-spec sync_set2(UserId, TargetName, VarsAndVals) -> {ok, SnmpReply, Remaining} | {error, Reason}
                   when
                       UserId :: user_id(),
                       TargetName :: target_name(),
                       VarsAndVals :: [var_and_val()],
                       SnmpReply :: snmp_reply(),
                       Remaining :: non_neg_integer(),
                       Reason ::
                           {send_failed, ReqId, ActualReason} |
                           {invalid_sec_info, SecInfo, SnmpInfo} |
                           term(),
                       ReqId :: request_id(),
                       ActualReason :: term(),
                       SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                       SecTag :: atom(),
                       ExpectedValue :: term(),
                       ReceivedValue :: term(),
                       SnmpInfo :: term().
```

# `sync_set2`
*since OTP R14B03* 

```elixir
-spec sync_set2(UserId, TargetName, VarsAndVals, SendOpts) ->
                   {ok, SnmpReply, Remaining} | {error, Reason}
                   when
                       UserId :: user_id(),
                       TargetName :: target_name(),
                       VarsAndVals :: [var_and_val()],
                       SendOpts :: [SendOpt],
                       SendOpt ::
                           {context, snmp:context_name()} |
                           {timeout, pos_integer()} |
                           {community, snmp:community()} |
                           {sec_model, snmp:sec_model()} |
                           {sec_name, snmp:sec_name()} |
                           {sec_level, snmp:sec_level()} |
                           {max_message_size, snmp:mms()} |
                           {extra, term()},
                       SnmpReply :: snmp_reply(),
                       Remaining :: non_neg_integer(),
                       Reason ::
                           {send_failed, ReqId, ActualReason} |
                           {invalid_sec_info, SecInfo, SnmpInfo} |
                           term(),
                       ReqId :: request_id(),
                       ActualReason :: term(),
                       SecInfo :: {SecTag, ExpectedValue, ReceivedValue},
                       SecTag :: atom(),
                       ExpectedValue :: term(),
                       ReceivedValue :: term(),
                       SnmpInfo :: term().
```

Synchronous `set-request`.

`Remaining` is the remaining time of the given (or default) timeout time.

When _Reason_ is _\{send_failed, ...\}_ it means that the `net-if` process
failed to send the message.
This could happen because of any number of reasons, i.e.
encoding error. _ActualReason_ is the actual reason in this case.

When _var_and_val()_ is _\{oid(), value()\}_, the manager makes an educated
guess based on the loaded mibs.

The send option `extra` specifies an opaque data structure passed on to the
`net-if` process.
The `net-if` process included in this application makes, with one
exception, no use of this info, so the only use for it (when using the built in
`net-if`) would be tracing. The one usage exception is: _Any_ tuple with
`snmpm_extra_info_tag` as its first element is reserved for internal use.

Some of the send options (`community`, `sec_model`, `sec_name`, `sec_level` and
`max_message_size`) are `override options`. That is, for _this_ request, they
override any configuration done when the agent was registered.

For `SnmpInfo`, see the user callback function
[`snmpm_user:handle_report/3`](`c:snmpm_user:handle_report/3`).

# `unload_mib`

```elixir
-spec unload_mib(MibName) -> ok | {error, Reason} when MibName :: snmp:mib_name(), Reason :: term().
```

Unload a `Mib` from the manager. The `MibName` is the name of the Mib, including
the path to where the compiled mib is found. For example,

```erlang
          Dir = code:priv_dir(my_app) ++ "/mibs/",
          snmpm:unload_mib(Dir ++ "MY-MIB").
```

# `unregister_agent`

```elixir
-spec unregister_agent(UserId, TargetName) -> ok | {error, Reason}
                          when UserId :: user_id(), TargetName :: target_name(), Reason :: term().
```

Unregister the agent.

# `unregister_user`

```elixir
-spec unregister_user(UserId) -> ok | {error, Reason} when UserId :: user_id(), Reason :: term().
```

Unregister the user.

# `unregister_usm_user`

```elixir
-spec unregister_usm_user(EngineID, UserName) -> ok | {error, Reason}
                             when
                                 EngineID :: snmp:engine_id(),
                                 UserName :: snmp:usm_name(),
                                 Reason :: term().
```

Unregister this USM user.

# `update_agent_info`
*since OTP R14B04* 

```elixir
-spec update_agent_info(UserId, TargetName, Info) -> ok | {error, Reason}
                           when
                               UserId :: user_id(),
                               TargetName :: target_name(),
                               Info :: [{Item, Value}],
                               Item :: agent_config_item(),
                               Value :: term(),
                               Reason :: term().
```

Update agent config.

This function, [`update_agent_info/3`](`update_agent_info/3`), should be used when several
values needs to be updated atomically.

See function `register_agent/3` for more info about what kind of items are allowed.

# `update_agent_info`

```elixir
-spec update_agent_info(UserId, TargetName, Item, Value) -> ok | {error, Reason}
                           when
                               UserId :: user_id(),
                               TargetName :: target_name(),
                               Item :: agent_config_item(),
                               Value :: term(),
                               Reason :: term().
```

Update agent config.

See function `register_agent/3` for more info about what
kind of items are allowed.

# `update_usm_user_info`

```elixir
-spec update_usm_user_info(EngineID, UserName, Item, Value) -> ok | {error, Reason}
                              when
                                  EngineID :: snmp:engine_id(),
                                  UserName :: snmp:usm_name(),
                                  Item :: usm_config_item(),
                                  Value :: term(),
                                  Reason :: term().
```

Update usm user config.

# `usm_user_info`

```elixir
-spec usm_user_info(EngineID, UserName, Item) -> {ok, Value} | {error, Reason}
                       when
                           EngineID :: snmp:engine_id(),
                           UserName :: snmp:usm_name(),
                           Item :: usm_config_item(),
                           Value :: term(),
                           Reason :: term().
```

Retrieve usm user config.

# `verbosity`

```elixir
-spec verbosity(Target, Verbosity) -> snmp:void()
                   when
                       Target :: config | server | net_if | note_store | all,
                       Verbosity :: snmp:verbosity().
```

Sets verbosity for the designated process. For the lowest verbosity `silence`,
nothing is printed. The higher the verbosity, the more is printed.

# `which_agents`

```elixir
-spec which_agents() -> Agents when Agents :: [target_name()].
```

# `which_agents`

```elixir
-spec which_agents(UserId) -> Agents when UserId :: user_id(), Agents :: [target_name()].
```

Get a list of all registered agents or all agents registered by a specific user.

# `which_mibs`

```elixir
-spec which_mibs() -> Mibs
                    when Mibs :: [{MibName, MibFile}], MibName :: snmp:mib_name(), MibFile :: string().
```

Get a list of all the mib's loaded into the manager.

# `which_users`

```elixir
-spec which_users() -> Users when Users :: [user_id()].
```

Get a list of the identities of all registered users.

# `which_usm_users`

```elixir
-spec which_usm_users() -> UsmUsers
                         when
                             UsmUsers :: [{EngineID, UserName}],
                             EngineID :: snmp:engine_id(),
                             UserName :: snmp:usm_name().
```

Get a list of all registered usm users.

# `which_usm_users`

```elixir
-spec which_usm_users(EngineID) -> UsmUsers
                         when
                             EngineID :: snmp:engine_id(),
                             UsmUsers :: [UserName],
                             UserName :: snmp:usm_name().
```

Get a list of all registered usm users with engine-id `EngineID`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
