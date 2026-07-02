# `mod_auth`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/inets/src/http_server/mod_auth.erl#L23)

User authentication using text files, Dets, or Mnesia database.

This module provides for basic user authentication using textual files, Dets
databases, or Mnesia databases.

### See also

`m:httpd`, `m:mod_alias`

# `httpd_group`
*not exported* 

```erlang
-type httpd_group() :: #httpd_group{name :: term(), userlist :: term()}.
```

# `httpd_user`
*not exported* 

```erlang
-type httpd_user() :: #httpd_user{username :: term(), password :: term(), user_data :: term()}.
```

# `add_group_member`

```erlang
-spec add_group_member(GroupName, UserName, Options) -> true | {error, Reason}
                          when
                              GroupName :: string(),
                              UserName :: string(),
                              Options ::
                                  [{port, Port} |
                                   {addr, Address} |
                                   {dir, Directory} |
                                   {authPassword, AuthPassword}],
                              Port :: inet:port_number(),
                              Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                              Directory :: string(),
                              AuthPassword :: string(),
                              Reason :: term().
```

# `add_group_member`

```erlang
-spec add_group_member(GroupName, UserName, Port, Directory) -> true | {error, Reason}
                          when
                              GroupName :: string(),
                              UserName :: string(),
                              Port :: inet:port_number(),
                              Directory :: string(),
                              Reason :: term().
```

# `add_group_member`

```erlang
-spec add_group_member(GroupName, UserName, Address, Port, Directory) -> true | {error, Reason}
                          when
                              GroupName :: string(),
                              UserName :: string(),
                              Port :: inet:port_number(),
                              Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                              Directory :: string(),
                              Reason :: term().
```

`add_group_member/3`, `add_group_member/4`, and `add_group_member/5` each adds
a user to a group. If the group does not exist, it is created and the user is
added to the group. Upon successful operation, this function returns `true`.
When `add_group_members/3` is called, options `Port` and `Dir` are mandatory.

# `add_user`

```erlang
-spec add_user(UserName, Options) -> true | {error, Reason}
                  when
                      UserName :: string(),
                      Options ::
                          [{password, Password} |
                           {userData, UserData} |
                           {port, Port} |
                           {addr, Address} |
                           {dir, Directory} |
                           {authPassword, AuthPassword}],
                      Password :: string(),
                      UserData :: term(),
                      Port :: inet:port_number(),
                      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                      Directory :: string(),
                      AuthPassword :: string(),
                      Reason :: term().
```

# `add_user`

```erlang
-spec add_user(UserName, Password, UserData, Port, Directory) -> true | {error, Reason}
                  when
                      UserName :: string(),
                      Password :: string(),
                      UserData :: term(),
                      Port :: inet:port_number(),
                      Directory :: string(),
                      Reason :: term().
```

# `add_user`

```erlang
-spec add_user(UserName, Password, UserData, Address, Port, Directory) -> true | {error, Reason}
                  when
                      UserName :: string(),
                      Password :: string(),
                      UserData :: term(),
                      Port :: inet:port_number(),
                      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                      Directory :: string(),
                      Reason :: term().
```

add_user(UserName, Password, UserData, Address, Port, Dir) -> true | {error,
Reason}

`add_user/2, add_user/5`, and [`add_user/6`](`add_user/6`) each adds a user to
the user database. If the operation is successful, this function returns `true`.
If an error occurs, `{error, Reason}` is returned. When
[`add_user/2`](`add_user/2`) is called, options `Password`, `UserData`, `Port`,
and `Dir` are mandatory.

# `delete_group`

```erlang
-spec delete_group(GroupName, Options) -> true | {error, Reason}
                      when
                          GroupName :: string(),
                          Options ::
                              [{port, Port} |
                               {addr, Address} |
                               {dir, Directory} |
                               {authPassword, AuthPassword}],
                          Port :: inet:port_number(),
                          Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                          Directory :: string(),
                          AuthPassword :: string(),
                          Reason :: term().
```

# `delete_group`

```erlang
-spec delete_group(GroupName, Port, Directory) -> true | {error, Reason}
                      when
                          GroupName :: string(),
                          Port :: inet:port_number(),
                          Directory :: string(),
                          Reason :: term().
```

# `delete_group`

```erlang
-spec delete_group(GroupName, Address, Port, Directory) -> true | {error, Reason}
                      when
                          GroupName :: string(),
                          Port :: inet:port_number(),
                          Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                          Directory :: string(),
                          Reason :: term().
```

`delete_group/2`, `delete_group/3`, and `delete_group/4` each deletes the group
specified and returns `true`. If there is an error, `{error, Reason}` is
returned. When `delete_group/2` is called, option `Port` and `Dir` are
mandatory.

# `delete_group_member`

```erlang
-spec delete_group_member(GroupName, UserName, Options) -> true | {error, Reason}
                             when
                                 GroupName :: string(),
                                 UserName :: string(),
                                 Options ::
                                     [{port, Port} |
                                      {addr, Address} |
                                      {dir, Directory} |
                                      {authPassword, AuthPassword}],
                                 Port :: inet:port_number(),
                                 Address ::
                                     inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                 Directory :: string(),
                                 AuthPassword :: string(),
                                 Reason :: term().
```

# `delete_group_member`

```erlang
-spec delete_group_member(GroupName, UserName, Port, Directory) -> true | {error, Reason}
                             when
                                 GroupName :: string(),
                                 UserName :: string(),
                                 Port :: inet:port_number(),
                                 Directory :: string(),
                                 Reason :: term().
```

# `delete_group_member`

```erlang
-spec delete_group_member(GroupName, UserName, Address, Port, Directory) -> true | {error, Reason}
                             when
                                 GroupName :: string(),
                                 UserName :: string(),
                                 Port :: inet:port_number(),
                                 Address ::
                                     inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                 Directory :: string(),
                                 Reason :: term().
```

`delete_group_member/3`, `delete_group_member/4`, and `delete_group_member/5`
each deletes a user from a group. If the group or the user does not exist, this
function returns an error, otherwise `true`. When `delete_group_member/3` is
called, the options `Port` and `Dir` are mandatory.

# `delete_user`

```erlang
-spec delete_user(UserName, Options) -> true | {error, Reason}
                     when
                         UserName :: string(),
                         Options ::
                             [{port, Port} |
                              {addr, Address} |
                              {dir, Directory} |
                              {authPassword, AuthPassword}],
                         Port :: inet:port_number(),
                         Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                         Directory :: string(),
                         AuthPassword :: string(),
                         Reason :: term().
```

# `delete_user`

```erlang
-spec delete_user(UserName, Port, Directory) -> true | {error, Reason}
                     when
                         UserName :: string(),
                         Port :: inet:port_number(),
                         Directory :: string(),
                         Reason :: term().
```

# `delete_user`

```erlang
-spec delete_user(UserName, Address, Port, Directory) -> true | {error, Reason}
                     when
                         UserName :: string(),
                         Port :: inet:port_number(),
                         Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                         Directory :: string(),
                         Reason :: term().
```

`delete_user/2`, `delete_user/3`, and `delete_user/4` each
deletes a user from the user database. If the operation is successful, this
function returns `true`. If an error occurs, `{error, Reason}` is returned. When
`delete_user/2` is called, options `Port` and `Dir` are
mandatory.

# `get_user`

```erlang
-spec get_user(UserName, Options) -> {ok, User} | {error, Reason}
                  when
                      UserName :: string(),
                      Options ::
                          [{port, Port} |
                           {addr, Address} |
                           {dir, Directory} |
                           {authPassword, AuthPassword}],
                      User :: httpd_user(),
                      Port :: inet:port_number(),
                      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                      Directory :: string(),
                      AuthPassword :: string(),
                      Reason :: term().
```

# `get_user`

```erlang
-spec get_user(UserName, Port, Directory) -> {ok, User} | {error, Reason}
                  when
                      UserName :: string(),
                      User :: httpd_user(),
                      Port :: inet:port_number(),
                      Directory :: string(),
                      Reason :: term().
```

# `get_user`

```erlang
-spec get_user(UserName, Address, Port, Directory) -> {ok, User} | {error, Reason}
                  when
                      UserName :: string(),
                      User :: httpd_user(),
                      Port :: inet:port_number(),
                      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                      Directory :: string(),
                      Reason :: term().
```

`get_user/2`, `get_user/3`, and `get_user/4` each returns an `t:httpd_user/0`
record containing the userdata for a specific user. If the user cannot be
found, `{error, Reason}` is returned. When `get_user/2` is called, options
`Port` and `Dir` are mandatory.

# `list_group_members`

```erlang
-spec list_group_members(GroupName, Options) -> {ok, Users} | {error, Reason}
                            when
                                GroupName :: string(),
                                Options ::
                                    [{port, Port} |
                                     {addr, Address} |
                                     {dir, Directory} |
                                     {authPassword, AuthPassword}],
                                Port :: inet:port_number(),
                                Address ::
                                    inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                Directory :: string(),
                                Users :: [httpd_user()],
                                AuthPassword :: string(),
                                Reason :: term().
```

# `list_group_members`

```erlang
-spec list_group_members(GroupName, Port, Directory) -> {ok, Users} | {error, Reason}
                            when
                                GroupName :: string(),
                                Port :: inet:port_number(),
                                Directory :: string(),
                                Users :: [httpd_user()],
                                Reason :: term().
```

# `list_group_members`

```erlang
-spec list_group_members(GroupName, Address, Port, Directory) -> {ok, Users} | {error, Reason}
                            when
                                GroupName :: string(),
                                Port :: inet:port_number(),
                                Address ::
                                    inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                Directory :: string(),
                                Users :: [httpd_user()],
                                Reason :: term().
```

`list_group_members/2`, `list_group_members/3`, and `list_group_members/4` each
lists the members of a specified group. If the group does not exist or there is
an error, `{error, Reason}` is returned. When `list_group_members/2` is called,
options `Port` and `Dir` are mandatory.

# `list_groups`

```erlang
-spec list_groups(Options) -> {ok, Groups} | {error, Reason}
                     when
                         Options ::
                             [{port, Port} |
                              {addr, Address} |
                              {dir, Directory} |
                              {authPassword, AuthPassword}],
                         Port :: inet:port_number(),
                         Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                         Directory :: string(),
                         Groups :: [httpd_group()],
                         AuthPassword :: string(),
                         Reason :: term().
```

# `list_groups`

```erlang
-spec list_groups(Port, Directory) -> {ok, Groups} | {error, Reason}
                     when
                         Port :: inet:port_number(),
                         Directory :: string(),
                         Groups :: [httpd_group()],
                         Reason :: term().
```

# `list_groups`

```erlang
-spec list_groups(Address, Port, Directory) -> {ok, Groups} | {error, Reason}
                     when
                         Port :: inet:port_number(),
                         Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                         Directory :: string(),
                         Groups :: [httpd_group()],
                         Reason :: term().
```

`list_groups/1`, `list_groups/2`, and `list_groups/3` each lists all the groups
available. If there is an error, `{error, Reason}` is returned. When
`list_groups/1` is called, options `Port` and `Dir` are mandatory.

# `list_users`

```erlang
-spec list_users(Options) -> {ok, Users} | {error, Reason}
                    when
                        Options ::
                            [{port, Port} |
                             {addr, Address} |
                             {dir, Directory} |
                             {authPassword, AuthPassword}],
                        Port :: inet:port_number(),
                        Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                        Directory :: string(),
                        Users :: [httpd_user()],
                        AuthPassword :: string(),
                        Reason :: atom().
```

# `list_users`
*since OTP R14B01* 

```erlang
-spec list_users(Port, Directory) -> {ok, Users} | {error, Reason}
                    when
                        Port :: inet:port_number(),
                        Directory :: string(),
                        Users :: [httpd_user()],
                        Reason :: atom().
```

# `list_users`

```erlang
-spec list_users(Address, Port, Directory) -> {ok, Users} | {error, Reason}
                    when
                        Port :: inet:port_number(),
                        Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                        Directory :: string(),
                        Users :: [httpd_user()],
                        Reason :: atom().
```

`list_users/1`, `list_users/2`, and `list_users/3` each returns a list of users
in the user database for a specific `Port/Dir`. When `list_users/1` is called,
options `Port` and `Dir` are mandatory.

# `update_password`

```erlang
-spec update_password(Port, Dir, OldPassword, NewPassword, NewPassword) -> ok | {error, Reason}
                         when
                             Port :: inet:port_number(),
                             Dir :: string(),
                             OldPassword :: string(),
                             NewPassword :: string(),
                             Reason :: term().
```

# `update_password`

```erlang
-spec update_password(Address, Port, Dir, OldPassword, NewPassword, NewPassword) -> ok | {error, Reason}
                         when
                             Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                             Port :: inet:port_number(),
                             Dir :: string(),
                             OldPassword :: string(),
                             NewPassword :: string(),
                             Reason :: term().
```

`update_password/5` and `update_password/6` each updates `AuthAccessPassword`
for the specified directory. If `NewPassword` is equal to "NoPassword", no
password is required to change authorisation data. If `NewPassword` is equal to
"DummyPassword", no changes can be done without changing the password first.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
