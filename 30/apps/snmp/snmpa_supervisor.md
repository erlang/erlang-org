# `snmpa_supervisor`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_supervisor.erl#L22)

A supervisor for the SNMP agent Processes

This is the top supervisor for the agent part of the SNMP application. There is
always one supervisor at each node with an SNMP agent (master agent or
sub-agent).

# `start_master_sup`

```erlang
-spec start_master_sup(Opts) -> {ok, Pid} | {error, Reason}
                          when
                              Opts :: [Opt],
                              Opt :: {db_dir, string()} | {config, ConfOpts} | {atom(), term()},
                              ConfOpts :: [ConfOpt],
                              ConfOpt :: {dir, string()} | {atom(), term()},
                              Pid :: pid(),
                              Reason :: {already_started, Pid} | term().
```

Starts a supervisor for the SNMP agent system. The supervisor starts all
involved SNMP processes, including the master agent. Sub-agents should be
started by calling `start_subagent/3`.

`db_dir` is mandatory.

`dir` in config is mandatory.

See [snmp config](snmp_config.md) for a description of the options.

# `start_sub_agent`

```erlang
-spec start_sub_agent(ParentAgent, Subtree, Mibs) -> {ok, Pid} | {error, Reason}
                         when
                             ParentAgent :: pid(),
                             Subtree :: snmp:oid(),
                             Mibs :: [MibName],
                             MibName :: string(),
                             Pid :: pid(),
                             Reason :: term().
```

Starts a sub-agent on the node where the function is called. The
`snmpa_supervisor` must be running.

If the supervisor is not running, the function fails with the reason `badarg`.

# `start_sub_sup`

```erlang
-spec start_sub_sup(Opts) -> {ok, Pid} | {error, Reason}
                       when
                           Opts :: [Opt],
                           Opt :: {db_dir, snmp:dir()} | {atom(), term()},
                           Pid :: pid(),
                           Reason :: {already_started, Pid} | term().
```

Starts a supervisor for the SNMP agent system without a master agent. The
supervisor starts all involved SNMP agent processes, but no agent processes.
Sub-agents should be started by calling
[`start_sub_agent/3`](`start_sub_agent/3`).

`db_dir` is mandatory.

See [configuration parameters](snmp_config.md#configuration_params) for a
description of the options.

# `stop_sub_agent`

```erlang
-spec stop_sub_agent(SubAgentPid) -> ok | no_such_child when SubAgentPid :: pid().
```

Stops the sub-agent on the node where the function is called. The
`snmpa_supervisor` must be running.

If the supervisor is not running, the function fails with the reason `badarg`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
