# `snmpa_discovery_handler`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_discovery_handler.erl#L22)

Behaviour module for the SNMP agent discovery handler.

This module defines the behaviour of the agent discovery handler. A
`snmpa_discovery_handler` compliant module must export the following functions:

- `c:stage1_finish/3`

The semantics of them and their exact signatures are explained below.

# `stage1_finish`

```erlang
-callback stage1_finish(TargetName, ManagerEngineID, ExtraInfo) ->
                           ignore | {ok, UsmEntry | [UsmEntry]} | {ok, UsmEntry | [UsmEntry], NewExtraInfo}
                           when
                               TargetName :: snmp_target_mib:name(),
                               ManagerEngineID :: snmp_framework_mib:engine_id(),
                               ExtraInfo :: term(),
                               UsmEntry :: snmp_user_based_sm_mib:usm_entry(),
                               NewExtraInfo :: term().
```

This function is called at the end of stage 1 of the discovery process. It
should return either the atom `ignore` or `{ok, usm_entry() | [usm_entry()]}`.
See [usm_entry()](snmp_agent_config_files.md#usm) and
[usm_entry/1,3](`snmpa_conf:usm_entry/1`) for more info.

If the function returns `ignore`, then it is assumed that either:

- The caller (of the discovery function) will make the needed updates later.
- The callback function itself did the updates.

In either case, the agent will do nothing, but return the retrieved
ManagerEngineID (see `snmpa:discovery/6` for more info) and
possible continue with stage 2 of the discovery process.

The `ExtraInfo` argument is passed on from the `snmpa:discovery/6`
function.

This function may return an updated `NewExtraInfo` that will be used in
subsequent calls to the callback functions. Intended for future use.

The purpose of this function is to generate the usm- related security data
needed for usm processing in the agent. Specifically, updating the usmUserTable.

When an `usm_entry()` tuple (or a list of such tuples) is returned, this data is
then added to the `usmUserTable` by the (master-) agent.

When an `usm_entry()` tuple (or a list of such tuples) is returned, this data is
then added to the `usmUserTable` by the (master-) agent.

> #### Note {: .info }
>
> Note that the function does not check if this entry already exists.

> #### Note {: .info }
>
> Note that this function is executed in the context of the master-agent
> process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
