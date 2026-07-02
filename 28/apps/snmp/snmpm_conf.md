# `snmpm_conf`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/snmp/src/manager/snmpm_conf.erl#L23)

Utility functions for handling the manager config files.

The module `snmpm_conf` contains various utility functions to used for
manipulating (write/append/read) the config files of the SNMP manager.

# `agent_entry`

```elixir
-opaque agent_entry() ::
            {UserId :: snmpm:user_id(),
             TargetName :: snmpm:target_name(),
             Community :: snmp:community(),
             Domain :: snmp:tdomain(),
             Address :: snmp:taddress(),
             EngineID :: snmp:engine_id(),
             Timeout :: snmpm:register_timeout(),
             MaxMessageSize :: snmp:mms(),
             Version :: snmp:version(),
             SecModel :: snmp:sec_model(),
             SecName :: snmp:sec_name(),
             SecLevel :: snmp:sec_level()} |
            {UserId :: snmpm:user_id(),
             TargetName :: snmpm:target_name(),
             Community :: snmp:community(),
             Address :: inet:ip_address() | [non_neg_integer()],
             Port :: inet:port_number(),
             EngineID :: snmp:engine_id(),
             Timeout :: snmpm:register_timeout(),
             MaxMessageSize :: snmp:mms(),
             Version :: snmp:version(),
             SecModel :: snmp:sec_model(),
             SecName :: snmp:sec_name(),
             SecLevel :: snmp:sec_level()}.
```

An opaque data structure containg all configuration for one agent for the
manager.

# `manager_entry`

```elixir
-opaque manager_entry() :: {Tag :: atom(), Value :: term()}.
```

An opaque data structure that represents one configuration entry for the
manager.

# `user_entry`

```elixir
-opaque user_entry() ::
            {UserId :: snmpm:user_id(),
             Mod :: snmpm:snmpm_user(),
             Data :: term(),
             DefaultAgentConfig :: [snmpm:agent_config()]}.
```

An opaque data structure containg all configuration for one user for the
manager.

# `usm_entry`

```elixir
-opaque usm_entry() ::
            {EngineID :: snmp:engine_id(),
             UserName :: snmp:usm_name(),
             AuthP :: snmp:usm_auth_protocol(),
             AuthKey :: snmp:usm_auth_key(),
             PrivP :: snmp:usm_priv_protocol(),
             PrivKey :: snmp:usm_priv_key()} |
            {EngineID :: snmp:engine_id(),
             UserName :: snmp:usm_name(),
             SecName :: snmp:sec_name(),
             AuthP :: snmp:usm_auth_protocol(),
             AuthKey :: snmp:usm_auth_key(),
             PrivP :: snmp:usm_priv_protocol(),
             PrivKey :: snmp:usm_priv_key()}.
```

An opaque data structure containg information about security data for usm for
the manager.

# `agents_entry`

```elixir
-spec agents_entry(UserId, TargetName, Comm, TDomain, TAddr, EngineID, Timeout, MaxMessageSize, Version,
                   SecModel, SecName, SecLevel) ->
                      Entry
                      when
                          UserId :: snmpm:user_id(),
                          TargetName :: snmpm:target_name(),
                          Comm :: snmp:community(),
                          TDomain :: snmp:tdomain(),
                          TAddr :: snmp:taddress(),
                          EngineID :: snmp:engine_id(),
                          Timeout :: snmpm:register_timeout(),
                          MaxMessageSize :: snmp:mms(),
                          Version :: snmp:version(),
                          SecModel :: snmp:sec_model(),
                          SecName :: snmp:sec_name(),
                          SecLevel :: snmp:sec_level(),
                          Entry :: agent_entry();
                  (UserId, TargetName, Comm, Ip, Port, EngineID, Timeout, MaxMessageSize, Version,
                   SecModel, SecName, SecLevel) ->
                      Entry
                      when
                          UserId :: snmpm:user_id(),
                          TargetName :: snmpm:target_name(),
                          Comm :: snmp:community(),
                          Ip :: inet:ip_address(),
                          Port :: inet:port_number(),
                          EngineID :: snmp:engine_id(),
                          Timeout :: snmpm:register_timeout(),
                          MaxMessageSize :: snmp:mms(),
                          Version :: snmp:version(),
                          SecModel :: snmp:sec_model(),
                          SecName :: snmp:sec_name(),
                          SecLevel :: snmp:sec_level(),
                          Entry :: agent_entry().
```

Create an entry for the manager agents config file, `agents.conf`.

See [Agents](snmp_manager_config_files.md#agents) for more info.

# `append_agents_config`

```elixir
-spec append_agents_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [agent_entry()].
```

Append the agents config to the current manager agents config file.

`Dir` is the path to the directory where to store the config file.

See [Agents](snmp_manager_config_files.md#agents) for more info.

# `append_manager_config`

```elixir
-spec append_manager_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [manager_entry()].
```

Append the config to the current manager config file.

`Dir` is the path to the directory where to store the config file.

See [Manager Information](snmp_manager_config_files.md#manager-information) for
more info.

# `append_users_config`

```elixir
-spec append_users_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [user_entry()].
```

Append the users config to the current manager users config file.

`Dir` is the path to the directory where to store the config file.

See [Users](snmp_manager_config_files.md#users) for more info.

# `append_usm_config`

```elixir
-spec append_usm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [usm_entry()].
```

Append the usm config to the current manager usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_manager_config_files.md#security-data-for-usm) for more info.

# `manager_entry`

```elixir
-spec manager_entry(Tag, Val) -> ManagerEntry
                       when
                           Tag :: transports | port | engine_id | max_message_size,
                           Val :: term(),
                           ManagerEntry :: manager_entry();
                   (Tag, Val) -> ManagerEntry
                       when Tag :: address, Val :: term(), ManagerEntry :: manager_entry().
```

Create an entry for the manager config file, `manager.conf`.

The type of `Val` depends on the value of `Tag`, see
[Manager Information](snmp_manager_config_files.md#manager-information) for more
info.

# `read_agents_config`

```elixir
-spec read_agents_config(Dir) -> {ok, Conf} | {error, Reason}
                            when Dir :: snmp:dir(), Conf :: [agent_entry()], Reason :: term().
```

Read the current manager agents config file.

`Dir` is the path to the directory where to store the config file.

See [Agents](snmp_manager_config_files.md#agents) for more info.

# `read_manager_config`

```elixir
-spec read_manager_config(Dir) -> {ok, Conf} | {error, Reason}
                             when Dir :: snmp:dir(), Conf :: [manager_entry()], Reason :: term().
```

Read the current manager config file.

`Dir` is the path to the directory where to store the config file.

See [Manager Information](snmp_manager_config_files.md#manager-information) for
more info.

# `read_users_config`

```elixir
-spec read_users_config(Dir) -> {ok, Conf} | {error, Reason}
                           when Dir :: snmp:dir(), Conf :: [user_entry()], Reason :: term().
```

Read the current manager users config file.

`Dir` is the path to the directory where to store the config file.

See [Users](snmp_manager_config_files.md#users) for more info.

# `read_usm_config`

```elixir
-spec read_usm_config(Dir) -> {ok, Conf} | {error, Reason}
                         when Dir :: snmp:dir(), Conf :: [usm_entry()], Reason :: term().
```

Read the current manager usm config file.

`Dir` is the path to the directory where to store the config file.

See [Security data for USM](snmp_manager_config_files.md#security-data-for-usm) for more info.

# `users_entry`

```elixir
-spec users_entry(UserId) -> UserEntry when UserId :: snmpm:user_id(), UserEntry :: user_entry().
```

# `users_entry`

```elixir
-spec users_entry(UserId, UserMod) -> UserEntry
                     when
                         UserId :: snmpm:user_id(),
                         UserMod :: snmpm:snmpm_user(),
                         UserEntry :: user_entry().
```

# `users_entry`

```elixir
-spec users_entry(UserId, UserMod, UserData) -> UserEntry
                     when
                         UserId :: snmpm:user_id(),
                         UserMod :: snmpm:snmpm_user(),
                         UserData :: term(),
                         UserEntry :: user_entry().
```

# `users_entry`
*since OTP 27.0* 

```elixir
-spec users_entry(UserId, UserMod, UserData, DefaultAgentConfig) -> UserEntry
                     when
                         UserId :: snmpm:user_id(),
                         UserMod :: snmpm:snmpm_user(),
                         UserData :: term(),
                         DefaultAgentConfig :: [snmpm:agent_config()],
                         UserEntry :: user_entry().
```

Create an entry for the manager users config file, `users.conf`.

See the [`Users`](snmp_manager_config_files.md#users) chapter of the
(SNMP) `Manager Configuration` User Guide for more info.

# `usm_entry`

```elixir
-spec usm_entry(EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey) -> UsmEntry
                   when
                       EngineID :: snmp:engine_id(),
                       UserName :: snmp:usm_name(),
                       AuthP :: snmp:usm_auth_protocol(),
                       AuthKey :: snmp:usm_auth_key(),
                       PrivP :: snmp:usm_priv_protocol(),
                       PrivKey :: snmp:usm_priv_key(),
                       UsmEntry :: usm_entry().
```

# `usm_entry`

```elixir
-spec usm_entry(EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey) -> UsmEntry
                   when
                       EngineID :: snmp:engine_id(),
                       UserName :: snmp:usm_name(),
                       SecName :: snmp:sec_name(),
                       AuthP :: snmp:usm_auth_protocol(),
                       AuthKey :: snmp:usm_auth_key(),
                       PrivP :: snmp:usm_priv_protocol(),
                       PrivKey :: snmp:usm_priv_key(),
                       UsmEntry :: usm_entry().
```

Create an entry for the manager usm config file, `usm.conf`.

See
[`Security data for USM`](snmp_manager_config_files.md#security-data-for-usm)
for more info.

# `write_agents_config`

```elixir
-spec write_agents_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [agent_entry()].
```

# `write_agents_config`

```elixir
-spec write_agents_config(Dir, Hdr, Conf) -> ok
                             when Dir :: snmp:dir(), Hdr :: string(), Conf :: [agent_entry()].
```

Write the manager agents config to the manager agents config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Agents](snmp_manager_config_files.md#agents) for more info.

# `write_manager_config`

```elixir
-spec write_manager_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [manager_entry()].
```

# `write_manager_config`

```elixir
-spec write_manager_config(Dir, Hdr, Conf) -> ok
                              when Dir :: snmp:dir(), Hdr :: string(), Conf :: [manager_entry()].
```

Write the manager config to the manager config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Manager Information](snmp_manager_config_files.md#manager-information) for
more info.

# `write_users_config`

```elixir
-spec write_users_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [user_entry()].
```

# `write_users_config`

```elixir
-spec write_users_config(Dir, Hdr, Conf) -> ok
                            when Dir :: snmp:dir(), Hdr :: string(), Conf :: [user_entry()].
```

Write the manager users config to the manager users config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Users](snmp_manager_config_files.md#users) for more info.

# `write_usm_config`

```elixir
-spec write_usm_config(Dir, Conf) -> ok when Dir :: snmp:dir(), Conf :: [user_entry()].
```

# `write_usm_config`

```elixir
-spec write_usm_config(Dir, Hdr, Conf) -> ok
                          when Dir :: snmp:dir(), Hdr :: string(), Conf :: [user_entry()].
```

Write the manager usm config to the manager usm config file.

`Dir` is the path to the directory where to store the config file.

`Hdr` is an optional file header (note that this text is written to the file as
is).

See [Security data for USM](snmp_manager_config_files.md#security-data-for-usm) for more info.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
