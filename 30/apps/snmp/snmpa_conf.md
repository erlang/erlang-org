# `snmpa_conf`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_conf.erl#L23)

Utility functions for handling the agent config files.

The module `snmpa_conf` contains various utility functions to use for
manipulating (write/read/append) the config files of the SNMP agent.

# `agent_entry`

```erlang
-opaque agent_entry()
```

An opaque term that represents an entry in the 'agent' config.

# `community_entry`

```erlang
-opaque community_entry()
```

An opaque term that represents an entry in the 'community' (agent) config.

# `context_entry`

```erlang
-opaque context_entry()
```

An opaque term that represents an entry in the 'context' (agent) config.

# `extended_transport_address`

```erlang
-type extended_transport_address() :: {inet:ip_address(), port_info()}.
```

# `intAgentTransport`

```erlang
-type intAgentTransport() ::
          {transportDomain(), transport_address()} |
          {transportDomain(), extended_transport_address(), snmpa:transport_kind()} |
          {transportDomain(), extended_transport_address(), transport_opts()} |
          {transportDomain(), extended_transport_address(), snmpa:transport_kind(), transport_opts()}.
```

# `ip_address`
*not exported* 

```erlang
-type ip_address() :: inet:ip_address() | snmp_ip_address().
```

# `notify_entry`

```erlang
-opaque notify_entry()
```

An opaque term that represents an entry in the 'notify' (agent) config.

# `port_info`

```erlang
-type port_info() :: inet:port_number() | system | range() | ranges().
```

Port number `0` (zero) cannot be specified directly (it is used internally).
Instead the atom `'system'` should be used.

# `range`

```erlang
-type range() :: {Min :: inet:port_number(), Max :: inet:port_number()}.
```

`Min < Max`

# `ranges`

```erlang
-type ranges() :: [inet:port_number() | range()].
```

# `snmp_ip_address`
*not exported* 

```erlang
-type snmp_ip_address() :: [non_neg_integer()].
```

# `standard_entry`

```erlang
-opaque standard_entry()
```

An opaque term that represents an entry in the 'standard' (agent) config.

# `target_addr_entry`

```erlang
-opaque target_addr_entry()
```

An opaque term that represents an entry in the 'target address' (agent) config.

# `target_params_entry`

```erlang
-opaque target_params_entry()
```

An opaque term that represents an entry in the 'target parameters' (agent)
config.

# `transport_address`

```erlang
-type transport_address() :: {ip_address(), inet:port_number()} | ip_address().
```

# `transport_opts`
*not exported* 

```erlang
-type transport_opts() :: list().
```

# `transportAddress`

```erlang
-type transportAddress() :: transportAddressIPv4() | transportAddressIPv6().
```

# `transportAddressIPv4`
*not exported* 

```erlang
-type transportAddressIPv4() :: transportAddressIPv4WithPort() | transportAddressIPv4WithoutPort().
```

# `transportAddressIPv4WithoutPort`
*not exported* 

```erlang
-type transportAddressIPv4WithoutPort() :: inet:ip4_address() | [byte()].
```

Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

The list variant, 4 bytes for address:

`[byte() x 4]`

# `transportAddressIPv4WithPort`
*not exported* 

```erlang
-type transportAddressIPv4WithPort() ::
          {transportAddressIPv4WithoutPort(), inet:port_number()} | [byte()].
```

Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

The list variant, 4 bytes for address + 2 bytes for port:

`[byte() x 4, byte() x 2]`

# `transportAddressIPv6`
*not exported* 

```erlang
-type transportAddressIPv6() :: transportAddressIPv6WithPort() | transportAddressIPv6WithoutPort().
```

# `transportAddressIPv6WithoutPort`
*not exported* 

```erlang
-type transportAddressIPv6WithoutPort() :: inet:ip6_address() | [word()] | [byte()].
```

Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

First list variant, 8 words for address:

`[word() x 8]`

Second list variant, 16 bytes for address:

`[byte() x 16]`

# `transportAddressIPv6WithPort`
*not exported* 

```erlang
-type transportAddressIPv6WithPort() ::
          {transportAddressIPv6WithoutPort(), inet:port_number()} |
          [word() | inet:port_number()] |
          [word() | byte()] |
          [byte()].
```

Because of limitations of the Erlang type language we cannot define this type in
detail. Instead, we describe it here.

First list variant, 8 words for address + 1 word for port:

`[word() x 8, inet:port_number()]`

Second list variant, 8 words for address + 2 bytes for port:

`[word() x 8, byte() x 2]`

Third list variant, 16 bytes for address + 2 bytes for port:

`[byte() x 16, byte() x 2]`

# `transportAddressMask`

```erlang
-type transportAddressMask() :: [] | transportAddressWithPort().
```

# `transportAddressWithoutPort`

```erlang
-type transportAddressWithoutPort() ::
          transportAddressIPv4WithoutPort() | transportAddressIPv6WithoutPort().
```

# `transportAddressWithPort`

```erlang
-type transportAddressWithPort() :: transportAddressIPv4WithPort() | transportAddressIPv6WithPort().
```

# `transportDomain`

```erlang
-type transportDomain() :: snmp:tdomain().
```

# `usm_entry`

```erlang
-opaque usm_entry()
```

An opaque term that represents an entry in the 'user based sm' (agent) config.

# `vacm_acc_entry`

```erlang
-opaque vacm_acc_entry()
```

An opaque term that represents an (access) entry in the 'vacm access' (agent)
config.

# `vacm_entry`

```erlang
-type vacm_entry() :: vacm_s2g_entry() | vacm_acc_entry() | vacm_vtf_entry().
```

An basically opaque term that represents an entry in the 'view based acm'
(agent) config.

# `vacm_s2g_entry`

```erlang
-opaque vacm_s2g_entry()
```

An opaque term that represents an (security to group) entry in the 'vacm
security to group' (agent) config.

# `vacm_vtf_entry`

```erlang
-opaque vacm_vtf_entry()
```

An opaque term that represents an (tree family) entry in the 'vacm tree family'
(agent) config.

# `word`
*not exported* 

```erlang
-type word() :: 0..65535.
```

# `agent_entry`

```erlang
-spec agent_entry(Tag, Val) -> AgentEntry
                     when
                         Tag ::
                             intAgentTransports | intAgentUDPPort | snmpEngineMaxMessageSize |
                             snmpEngineID,
                         Val :: term(),
                         AgentEntry :: agent_entry().
```

Create an entry for the agent config file, `agent.conf`.

The type of `Val` depends on the value of `Tag`:

- **`intAgentTransports: [`[`snmpa_conf:intAgentTransport()`](`t:intAgentTransport/0`) ] `<mandatory>`**{: #intAgentTransports }

- **`intAgentUDPPort: `[`inet:port_number()`](`t:inet:port_number/0`) `<optional>`**{: #intAgentUDPPort }

- **`snmpEngineMaxMessageSize: `[`snmp_framework_mib:max_message_size()`](`t:snmp_framework_mib:max_message_size/0`) `<mandatory>`**{: #snmpEngineMaxMessageSize }

- **`snmpEngineID: `[`snmp_framework_mib:engine_id()`](`t:snmp_framework_mib:engine_id/0`) `<mandatory>`**{: #snmpEngineID }

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

# `append_agent_config`

```erlang
-spec append_agent_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [agent_entry()].
```

Append the config to the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

# `append_community_config`

```erlang
-spec append_community_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [community_entry()].
```

Append the community config to the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Community](snmp_agent_config_files.md#community) for more info.

# `append_context_config`

```erlang
-spec append_context_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [context_entry()].
```

Append the context config to the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.

# `append_notify_config`

```erlang
-spec append_notify_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [notify_entry()].
```

Append the notify config to the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

# `append_standard_config`

```erlang
-spec append_standard_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [standard_entry()].
```

Append the standard config to the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

# `append_target_addr_config`

```erlang
-spec append_target_addr_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [target_addr_entry()].
```

Append the target_addr config to the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `append_target_params_config`

```erlang
-spec append_target_params_config(Dir, Conf) -> ok
                                     when Dir :: snmp:dir(), Conf :: [target_params_entry()].
```

Append the target_params config to the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `append_usm_config`

```erlang
-spec append_usm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [usm_entry()].
```

Append the usm config to the current agent usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

# `append_vacm_config`

```erlang
-spec append_vacm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [vacm_entry()].
```

Append the vacm config to the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `community_entry`

```erlang
-spec community_entry(CommIndex) -> CommunityEntry
                         when
                             CommIndex :: snmp_framework_mib:admin_string(),
                             CommunityEntry :: community_entry().
```

Create an entry for the agent community config file, `community.conf`.

`CommunityIndex` must be a _non-empty_ string.

This function only accepts the following values of `CommIndex`:

- **`"public"`{: #community_index_public }** - Translates to the following call:

  [`community_entry(CommunityIndex, CommunityIndex, "initial", "", "")`](`community_entry/5`).

- **`"all-rights"`{: #community_index_all_rights }** - Translates to the
  following call:

  [`community_entry(CommunityIndex, CommunityIndex, CommunityIndex, "", "")`](`community_entry/5`).

See [Community](snmp_agent_config_files.md#community) for more info.

# `community_entry`

```erlang
-spec community_entry(CommIndex, CommName, SecName, CtxName, TransportTag) -> CommunityEntry
                         when
                             CommIndex :: snmp_community_mib:index(),
                             CommName :: snmp_community_mib:name(),
                             SecName :: snmp_community_mib:security_name(),
                             CtxName :: snmp_community_mib:context_name(),
                             TransportTag :: snmp_community_mib:transport_tag(),
                             CommunityEntry :: community_entry().
```

Create an entry for the agent community config file, `community.conf`.

`CommunityIndex` must be a _non-empty_ string.

See [Community](snmp_agent_config_files.md#community) for more info.

# `context_entry`

```erlang
-spec context_entry(Ctx) -> ContextEntry
                       when Ctx :: snmp_community_mib:context_name(), ContextEntry :: context_entry().
```

Create an entry for the agent context config file, `context.conf`.

See [Contexts](snmp_agent_config_files.md#context) for more info.

# `notify_entry`

```erlang
-spec notify_entry(Name, Tag, Type) -> NotifyEntry
                      when
                          Name :: snmp_notification_mib:notify_name(),
                          Tag :: snmp_notification_mib:notify_tag(),
                          Type :: snmp_notification_mib:notify_type(),
                          NotifyEntry :: notify_entry().
```

Create an entry for the agent notify config file, `notify.conf`.

`Name` must be a _non-empty_ string.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

# `read_agent_config`

```erlang
-spec read_agent_config(Dir) -> {ok, Conf} | {error, Reason}
                           when Dir :: snmp:dir(), Conf :: [agent_entry()], Reason :: term().
```

Read the current agent config file.

`Dir` is the path to the directory where to store the config file.

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

# `read_community_config`

```erlang
-spec read_community_config(Dir) -> {ok, Conf} | {error, Reason}
                               when Dir :: snmp:dir(), Conf :: [community_entry()], Reason :: term().
```

Read the current agent community config file.

`Dir` is the path to the directory where to store the config file.

See [Communities](snmp_agent_config_files.md#community) for more info.

# `read_context_config`

```erlang
-spec read_context_config(Dir) -> {ok, Conf} | {error, Reason}
                             when Dir :: snmp:dir(), Conf :: [context_entry()], Reason :: term().
```

Read the current agent context config file.

`Dir` is the path to the directory where to store the config file.

See [Contexts](snmp_agent_config_files.md#context) for more info.

# `read_notify_config`

```erlang
-spec read_notify_config(Dir) -> {ok, Conf} | {error, Reason}
                            when Dir :: snmp:dir(), Conf :: [notify_entry()], Reason :: term().
```

Read the current agent notify config file.

`Dir` is the path to the directory where to store the config file.

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

# `read_standard_config`

```erlang
-spec read_standard_config(Dir) -> {ok, Conf} | {error, Reason}
                              when Dir :: snmp:dir(), Conf :: [standard_entry()], Reason :: term().
```

Read the current agent standard config file.

`Dir` is the path to the directory where to store the config file.

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

# `read_target_addr_config`

```erlang
-spec read_target_addr_config(Dir) -> {ok, Conf} | {error, Reason}
                                 when Dir :: snmp:dir(), Conf :: [target_addr_entry()], Reason :: term().
```

Read the current agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `read_target_params_config`

```erlang
-spec read_target_params_config(Dir) -> {ok, Conf} | {error, Reason}
                                   when
                                       Dir :: snmp:dir(),
                                       Conf :: [target_params_entry()],
                                       Reason :: term().
```

Read the current agent target_params config file.

`Dir` is the path to the directory where to store the config file.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `read_usm_config`

```erlang
-spec read_usm_config(Dir) -> {ok, Conf} | {error, Reason}
                         when Dir :: snmp:dir(), Conf :: [usm_entry()], Reason :: term().
```

Read the current agent usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

# `read_vacm_config`

```erlang
-spec read_vacm_config(Dir) -> {ok, Conf} | {error, Reason}
                          when Dir :: snmp:dir(), Conf :: [vacm_entry()], Reason :: term().
```

Read the current agent vacm config file.

`Dir` is the path to the directory where to store the config file.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `standard_entry`

```erlang
-spec standard_entry(Tag, Val) -> StandardEntry
                        when
                            Tag ::
                                sysDescr | sysObjectID | sysContact | sysName | sysLocation |
                                sysServices | snmpEnableAuthenTraps,
                            Val :: term(),
                            StandardEntry :: standard_entry().
```

Create an entry for the agent standard config file, `standard.conf`.

The type of `Val` depends on the value of `Tag`:

- **`sysDescr: `{: #sysDescr }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysObjectID: `{: #sysObjectID }[`snmp:oid()`](`t:snmp:oid/0`) `<mandatory>`** - `OBJECT IDENTIFIER`

- **`sysContact: `{: #sysContact }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysName: `{: #sysName }[`string()`](`t:erlang:string/0`) `<mandatory>`** -
  `DisplayString (SIZE(0..255))`

- **`sysLocation: `{: #sysLocation }[`string()`](`t:erlang:string/0`)
  `<mandatory>`** - `DisplayString (SIZE(0..255))`

- **`sysLocation: `[`non_neg_integer()`](`t:erlang:non_neg_integer/0`) `<mandatory>`** - "A
  value which indicates the set of services that this entity primarily offers."

  `INTEGER (0..127)`

- **`snmpEnableAuthenTraps: `{: #snmpEnableAuthenTraps }`enabled | disabled`
  `<mandatory>`** - `INTEGER { enabled(1), disabled(2) }`

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

# `target_addr_entry`

```erlang
-spec target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId) -> TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               Domain :: transportDomain(),
                               Addr :: transportAddress(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TargetAddrEntry :: target_addr_entry();
                       (Name, IP, TagList, ParamsName, EngineId, TMask) -> TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               IP :: inet:ip_address(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               TargetAddrEntry :: target_addr_entry().
```

Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/6` (1)](`target_addr_entry/6`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, [])`](`target_addr_entry/7`).

[`target_addr_entry/6` (2)](`target_addr_entry/6`) translates to the following call (with `Domain` and `Addr` built from `IP` and the default port number):
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `target_addr_entry`
*since OTP 17.3* 

```erlang
-spec target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask) -> TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               Domain :: transportDomain(),
                               Addr :: transportAddress(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               TargetAddrEntry :: target_addr_entry();
                       (Name, IP, Port, TagList, ParamsName, EngineId, TMask) -> TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               IP :: inet:ip_address(),
                               Port :: inet:port_number(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               TargetAddrEntry :: target_addr_entry().
```

Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/7` (1)](`target_addr_entry/7`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

[`target_addr_entry/7` (2)](`target_addr_entry/7`) translates to the following call (with `Domain` and `Addr` built from `IP` and `Port`):
[`target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, 2048)`](`target_addr_entry/8`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `target_addr_entry`

```erlang
-spec target_addr_entry(Name, Domain, Addr, TagList, ParamsName, EngineId, TMask, MaxMessageSize) ->
                           TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               Domain :: transportDomain(),
                               Addr :: transportAddress(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               MaxMessageSize :: snmp_target_mib:mms(),
                               TargetAddrEntry :: target_addr_entry();
                       (Name, IP, Port, TagList, ParamsName, EngineId, TMask, MaxMessageSize) ->
                           TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               IP :: inet:ip_address(),
                               Port :: inet:port_number(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_target_mib:params(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               MaxMessageSize :: snmp_target_mib:mms(),
                               TargetAddrEntry :: target_addr_entry().
```

Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

[`target_addr_entry/8` (1)](`target_addr_entry/8`) translates to the following call:
[`target_addr_entry(Name, Domain, Addr, 1500, 3, TagList, ParamsName, EngineId, TMask, MaxMessageSize)`](`target_addr_entry/10`).

[`target_addr_entry/8` (2)](`target_addr_entry/8`) translates to the following call (with `Domain` and `Addr` built from `IP` and `Port`):
[`target_addr_entry(Name, Domain, Addr, 1500, 3, TagList, ParamsName, EngineId, TMask, MaxMessageSize)`](`target_addr_entry/10`).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `target_addr_entry`

```erlang
-spec target_addr_entry(Name, Domain, Addr, Timeout, RetryCount, TagList, ParamsName, EngineId, TMask,
                        MaxMessageSize) ->
                           TargetAddrEntry
                           when
                               Name :: snmp_target_mib:name(),
                               Domain :: transportDomain(),
                               Addr :: transportAddress(),
                               Timeout :: snmp:time_interval(),
                               RetryCount :: snmp_target_mib:retry_count(),
                               TagList :: snmp_target_mib:tag_list(),
                               ParamsName :: snmp_framework_mib:admin_string(),
                               EngineId :: snmp_framework_mib:engine_id(),
                               TMask :: snmp_target_mib:tmask(),
                               MaxMessageSize :: snmp_target_mib:mms(),
                               TargetAddrEntry :: target_addr_entry().
```

Create an entry for the agent target_addr config file, `target_addr.conf`.

`Name` must be a _non-empty_ string.

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `target_params_entry`

```erlang
-spec target_params_entry(Name, Vsn) -> TargetParamsEntry
                             when
                                 Name :: snmp_target_mib:name(),
                                 Vsn :: snmp:version(),
                                 TargetParamsEntry :: target_params_entry().
```

Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

[`target_params_entry/2`](`target_params_entry/2`) translates to the following
call:

```text
	  target_params_entry(Name, Vsn, "initial", noAuthNoPriv)
```

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `target_params_entry`

```erlang
-spec target_params_entry(Name, Vsn, SecName, SecLevel) -> TargetParamsEntry
                             when
                                 Name :: snmp_target_mib:name(),
                                 Vsn :: snmp:version(),
                                 SecName :: snmp_framework_mib:admin_string(),
                                 SecLevel :: snmp_framework_mib:security_level(),
                                 TargetParamsEntry :: target_params_entry().
```

Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

`Vsn` translates into `MPModel` and `SecModel` as follows:

```text
	  Vsn = v1 => MPModel = v1,  SecModel = v1
	  Vsn = v2 => MPModel = v2c, SecModel = v2c
	  Vsn = v3 => MPModel = v3,  SecModel = usm
```

[`target_params_entry/4`](`target_params_entry/4`) translates to the following
call:

```text
	  target_params_entry(Name, MPModel, SecModel, SecName, SecLevel)
```

Where `MPModel` and `SecModel` is mapped from `Vsn`, see above.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `target_params_entry`

```erlang
-spec target_params_entry(Name, MPModel, SecModel, SecName, SecLevel) -> TargetParamsEntry
                             when
                                 Name :: snmp_target_mib:name(),
                                 MPModel :: snmp_framework_mib:message_processing_model(),
                                 SecModel :: snmp_framework_mib:security_model(),
                                 SecName :: snmp_framework_mib:admin_string(),
                                 SecLevel :: snmp_framework_mib:security_level(),
                                 TargetParamsEntry :: target_params_entry().
```

Create an entry for the agent target_params config file, `target_params.conf`.

`Name` must be a _non-empty_ string.

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `usm_entry`

```erlang
-spec usm_entry(EngineID) -> UsmEntry
                   when EngineID :: snmp_framework_mib:engine_id(), UsmEntry :: usm_entry().
```

Create an entry for the agent usm config file, `usm.conf`.

[`usm_entry/1`](`usm_entry/1`) translates to the following call:

```text
	  usm_entry(EngineID,
	            "initial", "initial", zeroDotZero,
		    usmNoAuthProtocol, "", "",
		    usmNoPrivProtocol, "", "",
		    "", "", "").
```

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

# `usm_entry`

```erlang
-spec usm_entry(EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC, PrivP, PrivKeyC,
                OwnPrivKeyC, Public, AuthKey, PrivKey) ->
                   UsmEntry
                   when
                       EngineID :: snmp_framework_mib:engine_id(),
                       UserName :: snmp_user_based_sm_mib:name(),
                       SecName :: snmp_framework_mib:admin_string(),
                       Clone :: snmp_user_based_sm_mib:clone_from(),
                       AuthP :: snmp_user_based_sm_mib:auth_protocol(),
                       AuthKeyC :: snmp_user_based_sm_mib:key_change(),
                       OwnAuthKeyC :: snmp_user_based_sm_mib:key_change(),
                       PrivP :: snmp_user_based_sm_mib:priv_protocol(),
                       PrivKeyC :: snmp_user_based_sm_mib:key_change(),
                       OwnPrivKeyC :: snmp_user_based_sm_mib:key_change(),
                       Public :: snmp_user_based_sm_mib:public(),
                       AuthKey :: snmp_user_based_sm_mib:auth_key(),
                       PrivKey :: snmp_user_based_sm_mib:priv_key(),
                       UsmEntry :: usm_entry().
```

Create an entry for the agent usm config file, `usm.conf`.

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

# `vacm_acc_entry`

```erlang
-spec vacm_acc_entry(GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV) -> VacmAccEntry
                        when
                            GroupName :: snmp_framework_mib:admin_string(),
                            Prefix :: snmp_view_based_acm_mib:context_prefix(),
                            SecModel :: snmp_framework_mib:security_model(),
                            SecLevel :: snmp_framework_mib:security_level(),
                            Match :: snmp_view_based_acm_mib:context_match(),
                            RV :: snmp_framework_mib:admin_string(),
                            WV :: snmp_framework_mib:admin_string(),
                            NV :: snmp_framework_mib:admin_string(),
                            VacmAccEntry :: vacm_acc_entry().
```

Create an (access) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `vacm_s2g_entry`

```erlang
-spec vacm_s2g_entry(SecModel, SecName, GroupName) -> VacmS2GEntry
                        when
                            SecModel :: snmp_framework_mib:security_model(),
                            SecName :: snmp_view_based_acm_mib:security_name(),
                            GroupName :: snmp_framework_mib:admin_string(),
                            VacmS2GEntry :: vacm_s2g_entry().
```

Create an (security to group) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `vacm_vtf_entry`

```erlang
-spec vacm_vtf_entry(ViewName, ViewSubtree) -> VacmVtfEntry
                        when
                            ViewName :: snmp_framework_mib:admin_string(),
                            ViewSubtree :: snmp:oid(),
                            VacmVtfEntry :: VacmVtfEntry.
```

Create an (view tree family) entry for the agent vacm config file, `vacm.conf`.

[`vacm_vtf_entry/2`](`vacm_vtf_entry/2`) translates to the following call:

```text
	  vacm_vtf_entry(ViewIndex, ViewSubtree, included, null).
```

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `vacm_vtf_entry`

```erlang
-spec vacm_vtf_entry(ViewName, ViewSubtree, ViewType, ViewMask) -> VacmVtfEntry
                        when
                            ViewName :: snmp_framework_mib:admin_string(),
                            ViewSubtree :: snmp:oid(),
                            ViewType :: snmp_view_based_acm_mib:view_type(),
                            ViewMask :: null | snmp_view_based_acm_mib:view_mask(),
                            VacmVtfEntry :: VacmVtfEntry.
```

Create an (view tree family) entry for the agent vacm config file, `vacm.conf`.

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

# `write_agent_config`

```erlang
-spec write_agent_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [agent_entry()].
```

# `write_agent_config`

```erlang
-spec write_agent_config(Dir, Hdr, Conf) -> ok
                            when Dir :: snmp:dir(), Hdr :: string(), Conf :: [agent_entry()].
```

Write the agent config to the agent config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Agent Information](snmp_agent_config_files.md#agent_information) for more
info.

# `write_community_config`

```erlang
-spec write_community_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [community_entry()].
```

# `write_community_config`

```erlang
-spec write_community_config(Dir, Hdr, Conf) -> ok
                                when Dir :: snmp:dir(), Hdr :: string(), Conf :: [community_entry()].
```

Write the agent community config to the agent community config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Community](snmp_agent_config_files.md#community) for more info.

# `write_context_config`

```erlang
-spec write_context_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [context_entry()].
```

# `write_context_config`

```erlang
-spec write_context_config(Dir, Hdr, Conf) -> ok
                              when Dir :: snmp:dir(), Hdr :: string(), Conf :: [context_entry()].
```

Write the agent context config to the agent context config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Contexts](snmp_agent_config_files.md#context) for more info.

# `write_notify_config`

```erlang
-spec write_notify_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [notify_entry()].
```

# `write_notify_config`

```erlang
-spec write_notify_config(Dir, Hdr, Conf) -> ok
                             when Dir :: snmp:dir(), Hdr :: string(), Conf :: [notify_entry()].
```

Write the agent notify config to the agent notify config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Notify Definitions](snmp_agent_config_files.md#notify) for more info.

# `write_standard_config`

```erlang
-spec write_standard_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [standard_entry()].
```

# `write_standard_config`

```erlang
-spec write_standard_config(Dir, Hdr, Conf) -> ok
                               when Dir :: snmp:dir(), Hdr :: string(), Conf :: [standard_entry()].
```

Write the agent standard config to the agent standard config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [System Information](snmp_agent_config_files.md#system_information) for more
info.

# `write_target_addr_config`

```erlang
-spec write_target_addr_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [target_addr_entry()].
```

# `write_target_addr_config`

```erlang
-spec write_target_addr_config(Dir, Hdr, Conf) -> ok
                                  when Dir :: snmp:dir(), Hdr :: string(), Conf :: [target_addr_entry()].
```

Write the agent target_addr config to the agent target_addr config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Address Definitions](snmp_agent_config_files.md#target_addr) for
more info.

# `write_target_params_config`

```erlang
-spec write_target_params_config(Dir, Conf) -> ok
                                    when Dir :: snmp:dir(), Conf :: [target_params_entry()].
```

# `write_target_params_config`

```erlang
-spec write_target_params_config(Dir, Hdr, Conf) -> ok
                                    when
                                        Dir :: snmp:dir(),
                                        Hdr :: string(),
                                        Conf :: [target_params_entry()].
```

Write the agent target_params config to the agent target_params config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Target Parameters Definitions](snmp_agent_config_files.md#target_params)
for more info.

# `write_usm_config`

```erlang
-spec write_usm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [usm_entry()].
```

# `write_usm_config`

```erlang
-spec write_usm_config(Dir, Hdr, Conf) -> ok
                          when Dir :: snmp:dir(), Hdr :: string(), Conf :: [usm_entry()].
```

Write the agent usm config to the agent usm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Security data for USM](snmp_agent_config_files.md#usm) for more info.

# `write_vacm_config`

```erlang
-spec write_vacm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [vacm_entry()].
```

# `write_vacm_config`

```erlang
-spec write_vacm_config(Dir, Hdr, Conf) -> ok
                           when Dir :: snmp:dir(), Hdr :: string(), Conf :: [vacm_entry()].
```

Write the agent vacm config to the agent vacm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [MIB Views for VACM](snmp_agent_config_files.md#vacm) for more info.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
