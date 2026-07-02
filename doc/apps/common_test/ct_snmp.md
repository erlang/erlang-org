# `ct_snmp`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/common_test/src/ct_snmp.erl#L23)

`Common Test` user interface module for the `SNMP` application.

The purpose of this module is to simplify SNMP configuration for the test case
writer. Many test cases can use default values for common operations and then no
SNMP configuration files need to be supplied. When it is necessary to change
particular configuration parameters, a subset of the relevant SNMP configuration
files can be passed to `ct_snmp` by `Common Test` configuration files. For more
specialized configuration parameters, a simple SNMP configuration file can be
placed in the test suite data directory. To simplify the test suite,
`Common Test` keeps track of some of the SNMP manager information. This way the
test suite does not have to handle as many input parameters as if it had to
interface wthe OTP SNMP manager directly.

_Configurable SNMP Manager and Agent Parameters:_

Manager configuration:

- **`[{start_manager, boolean()}`** - Optional. Default is `true`.

- **`{users, [{user_name(), [call_back_module(), user_data()]}]}`** - Optional.

- **`{usm_users, [{usm_user_name(), [usm_config()]}]}`** - Optional. SNMPv3
  only.

- **`{managed_agents,[{agent_name(), [user_name(), agent_ip(), agent_port(), [agent_config()]]}]}`** -
  `managed_agents` is optional.

- **`{max_msg_size, integer()}`** - Optional. Default is `484`.

- **`{mgr_port, integer()}`** - Optional. Default is `5000`.

- **`{engine _id, string()}`** - Optional. Default is `"mgrEngine"`.

Agent configuration:

- **`{start_agent, boolean()}`** - Optional. Default is `false`.

- **`{agent_sysname, string()}`** - Optional. Default is `"ct_test"`.

- **`{agent_manager_ip, manager_ip()}`** - Optional. Default is `localhost`.

- **`{agent_vsns, list()}`** - Optional. Default is `[v2]`.

- **`{agent_trap_udp, integer()}`** - Optional. Default is `5000`.

- **`{agent_udp, integer()}`** - Optional. Default is `4000`.

- **`{agent_notify_type, atom()}`** - Optional. Default is `trap`.

- **`{agent_sec_type, sec_type()}`** - Optional. Default is `none`.

- **`{agent_passwd, string()}`** - Optional. Default is `""`.

- **`{agent_engine_id, string()}`** - Optional. Default is `"agentEngine"`.

- **`{agent_max_msg_size, string()}`** - Optional. Default is `484`.

The following parameters represents the SNMP configuration files `context.conf`,
`standard.conf`, `community.conf`, `vacm.conf`, `usm.conf`, `notify.conf`,
`target_addr.conf`, and `target_params.conf`. Notice that all values in
`agent.conf` can be modified by the parameters listed above. All these
configuration files have default values set by the `SNMP` application. These
values can be overridden by suppling a list of valid configuration values or a
file located in the test suites data directory, which can produce a list of
valid configuration values if you apply function `file:consult/1` to the file.

- **`{agent_contexts, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_community, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_sysinfo, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_vacm, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_usm, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_notify_def, [term()] | {data_dir_file, rel_path()}}`** - Optional.

- **`{agent_target_address_def, [term()] | {data_dir_file, rel_path()}}`** -
  Optional.

- **`{agent_target_param_def, [term()] | {data_dir_file, rel_path()}}`** -
  Optional.

Parameter `MgrAgentConfName` in the functions is to be a name you allocate in
your test suite using a `require` statement. Example (where
`MgrAgentConfName = snmp_mgr_agent`):

```erlang
suite() -> [{require, snmp_mgr_agent, snmp}].
```

or

```erlang
ct:require(snmp_mgr_agent, snmp).
```

Notice that USM users are needed for SNMPv3 configuration and are not to be
confused with users.

SNMP traps, inform, and report messages are handled by the user callback module.
For details, see the [`SNMP`](`e:snmp:index.html`) application.

It is recommended to use the `.hrl` files created by the Erlang/OTP MIB compiler
to define the Object Identifiers (OIDs). For example, to get the Erlang node
name from `erlNodeTable` in the OTP-MIB:

```erlang
Oid = ?erlNodeEntry ++ [?erlNodeName, 1]
```

Furthermore, values can be set for `SNMP` application configuration parameters,
`config`, `server`, `net_if`, and so on (for a list of valid parameters and
types, see the [`User's Guide for the SNMP application`](`e:snmp:index.html`)).
This is done by defining a configuration data variable on the following form:

```erlang
{snmp_app, [{manager, [snmp_app_manager_params()]},
            {agent, [snmp_app_agent_params()]}]}.
```

A name for the data must be allocated in the suite using `require` (see the
example above). Pass this name as argument `SnmpAppConfName` to
[`ct_snmp:start/3`](`start/3`). `ct_snmp` specifies default values for some
`SNMP` application configuration parameters (such as `{verbosity,trace}` for
parameter `config`). This set of defaults is merged with the parameters
specified by the user. The user values override `ct_snmp` defaults.

# `agent_config`

```erlang
-type agent_config() :: {Item :: term(), Value :: term()}.
```

# `agent_ip`

```erlang
-type agent_ip() :: ip().
```

# `agent_name`

```erlang
-type agent_name() :: atom().
```

# `agent_port`

```erlang
-type agent_port() :: integer().
```

# `call_back_module`

```erlang
-type call_back_module() :: atom().
```

# `error_index`

```erlang
-type error_index() :: integer().
```

# `error_status`

```erlang
-type error_status() :: noError | atom().
```

# `ip`

```erlang
-type ip() :: string() | {integer(), integer(), integer(), integer()}.
```

# `manager_ip`

```erlang
-type manager_ip() :: ip().
```

# `oid`

```erlang
-type oid() :: [byte()].
```

# `oids`

```erlang
-type oids() :: [oid()].
```

# `rel_path`

```erlang
-type rel_path() :: string().
```

# `sec_type`

```erlang
-type sec_type() :: none | minimum | semi.
```

# `snmp_app_agent_params`

```erlang
-type snmp_app_agent_params() :: term().
```

# `snmp_app_manager_params`

```erlang
-type snmp_app_manager_params() :: term().
```

# `snmpreply`

```erlang
-type snmpreply() :: {error_status(), error_index(), varbinds()}.
```

# `user_data`

```erlang
-type user_data() :: term().
```

# `user_name`

```erlang
-type user_name() :: atom().
```

# `usm_config`

```erlang
-type usm_config() :: {Item :: term(), Value :: term()}.
```

# `usm_user_name`

```erlang
-type usm_user_name() :: string().
```

# `value_type`

```erlang
-type value_type() :: o | i | u | g | s.
```

# `var_and_val`

```erlang
-type var_and_val() :: {oid(), value_type(), term()}.
```

# `varbind`

```erlang
-type varbind() :: term().
```

# `varbinds`

```erlang
-type varbinds() :: [varbind()].
```

# `varsandvals`

```erlang
-type varsandvals() :: [var_and_val()].
```

These data types are described in the documentation for the
[`SNMP`](`e:snmp:index.html`) application.

# `get_next_values`

```erlang
-spec get_next_values(Agent, Oids, MgrAgentConfName) -> SnmpReply
                         when
                             Agent :: agent_name(),
                             Oids :: oids(),
                             MgrAgentConfName :: atom(),
                             SnmpReply :: snmpreply().
```

Issues a synchronous SNMP `get next` request.

# `get_values`

```erlang
-spec get_values(Agent, Oids, MgrAgentConfName) -> SnmpReply
                    when
                        Agent :: agent_name(),
                        Oids :: oids(),
                        MgrAgentConfName :: atom(),
                        SnmpReply :: snmpreply().
```

Issues a synchronous SNMP `get` request.

# `load_mibs`

```erlang
-spec load_mibs(Mibs) -> ok | {error, Reason}
                   when
                       Mibs :: [MibName],
                       MibName :: string(),
                       Reason :: {'load aborted at', MibName, InternalReason},
                       InternalReason :: already_loaded | term().
```

Loads the MIBs into agent `snmp_master_agent`.

# `register_agents`

```erlang
-spec register_agents(MgrAgentConfName, ManagedAgents) -> ok | {error, Reason}
                         when
                             MgrAgentConfName :: atom(),
                             ManagedAgents :: [{AgentName, [Uid | AgentIp | AgentUdpPort | AgentConf]}],
                             AgentName :: agent_name(),
                             Uid :: snmpm:user_id(),
                             AgentIp :: agent_ip(),
                             AgentUdpPort :: inet:port_number(),
                             AgentConf :: [agent_config()],
                             Reason :: term().
```

Explicitly instructs the manager to handle this agent. Corresponds to making an
entry in `agents.conf`.

This function tries to register the specified managed agents, without checking
if any of them exist. To change a registered managed agent, the agent must first
be unregistered.

# `register_users`

```erlang
-spec register_users(MgrAgentConfName, Users) -> ok | {error, Reason}
                        when
                            MgrAgentConfName :: atom(),
                            Users :: [{Id, [Module | Data]}],
                            Id :: snmpm:user_id(),
                            Module :: user_name(),
                            Data :: user_data(),
                            Reason :: term().
```

Registers the manager entity (=user) responsible for specific agent(s).
Corresponds to making an entry in `users.conf`.

This function tries to register the specified users, without checking if any of
them exist. To change a registered user, the user must first be unregistered.

# `register_usm_users`

```erlang
-spec register_usm_users(MgrAgentConfName, UsmUsers) -> ok | {error, Reason}
                            when
                                MgrAgentConfName :: atom(),
                                UsmUsers :: [{UsmUserName, UsmConfig}],
                                UsmUserName :: usm_user_name(),
                                UsmConfig :: [usm_config()],
                                Reason :: term().
```

Explicitly instructs the manager to handle this USM user. Corresponds to making
an entry in `usm.conf`.

This function tries to register the specified users, without checking if any of
them exist. To change a registered user, the user must first be unregistered.

# `set_info`

```erlang
-spec set_info(Config) -> [{Agent, OldVarsAndVals, NewVarsAndVals}]
                  when
                      Config :: proplists:proplist(),
                      Agent :: agent_name(),
                      OldVarsAndVals :: varsandvals(),
                      NewVarsAndVals :: varsandvals().
```

Returns a list of all successful `set` requests performed in the test case in
reverse order. The list contains the involved user and agent, the value before
`set`, and the new value. This is intended to simplify the cleanup in function
`end_per_testcase`, that is, the undoing of the `set` requests and their
possible side-effects.

# `set_values`

```erlang
-spec set_values(Agent, VarsAndVals, MgrAgentConfName, Config) -> SnmpReply
                    when
                        Agent :: agent_name(),
                        VarsAndVals :: varsandvals(),
                        MgrAgentConfName :: atom(),
                        Config :: proplists:proplist(),
                        SnmpReply :: snmpreply().
```

Issues a synchronous SNMP `set` request.

# `start`

```erlang
-spec start(Config, MgrAgentConfName) -> ok | {error, Reason}
               when Config :: proplists:proplist(), MgrAgentConfName :: atom(), Reason :: term().
```

# `start`

```erlang
-spec start(Config, MgrAgentConfName, SnmpAppConfName) -> ok | {error, Reason}
               when
                   Config :: proplists:proplist(),
                   MgrAgentConfName :: atom(),
                   SnmpAppConfName :: atom(),
                   Reason :: term().
```

Starts an SNMP manager and/or agent. In the manager case, registrations of users
and agents, as specified by the configuration `MgrAgentConfName`, are performed.
When using SNMPv3, called USM users are also registered. Users, `usm_users`, and
managed agents can also be registered later using
[`ct_snmp:register_users/2`](`register_users/2`),
[`ct_snmp:register_agents/2`](`register_agents/2`), and
[`ct_snmp:register_usm_users/2`](`register_usm_users/2`).

The agent started is called `snmp_master_agent`. Use
[`ct_snmp:load_mibs/1`](`load_mibs/1`) to load MIBs into the agent.

With `SnmpAppConfName` SNMP applications can be configured with parameters
`config`, `mibs`, `net_if`, and so on. The values are merged with (and possibly
override) default values set by `ct_snmp`.

# `stop`

```erlang
-spec stop(Config) -> ok when Config :: proplists:proplist().
```

Stops the SNMP manager and/or agent, and removes all files created.

# `unload_mibs`
*since OTP R16B* 

```erlang
-spec unload_mibs(Mibs) -> ok | {error, Reason}
                     when
                         Mibs :: [MibName],
                         MibName :: string(),
                         Reason :: {'unload aborted at', MibName, InternalReason},
                         InternalReason :: not_loaded | term().
```

Unloads the MIBs from agent `snmp_master_agent`.

# `unregister_agents`

```erlang
-spec unregister_agents(MgrAgentConfName) -> ok when MgrAgentConfName :: atom().
```

Unregisters all managed agents.

# `unregister_agents`
*since OTP R16B* 

```erlang
-spec unregister_agents(MgrAgentConfName, ManagedAgents) -> ok
                           when MgrAgentConfName :: atom(), ManagedAgents :: [agent_name()].
```

Unregisters the specified managed agents.

# `unregister_users`

```erlang
-spec unregister_users(MgrAgentConfName) -> ok when MgrAgentConfName :: atom().
```

Unregisters all users.

# `unregister_users`
*since OTP R16B* 

```erlang
-spec unregister_users(MgrAgentConfName, Users) -> ok
                          when MgrAgentConfName :: atom(), Users :: [snmpm:user_id()].
```

Unregisters the specified users.

# `unregister_usm_users`
*since OTP R16B* 

```erlang
-spec unregister_usm_users(MgrAgentConfName) -> ok when MgrAgentConfName :: atom().
```

Unregisters all USM users.

# `unregister_usm_users`
*since OTP R16B* 

```erlang
-spec unregister_usm_users(MgrAgentConfName, UsmUsers) -> ok
                              when MgrAgentConfName :: atom(), UsmUsers :: [usm_user_name()].
```

Unregisters the specified USM users.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
