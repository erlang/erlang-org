# `mod_security`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/inets/src/http_server/mod_security.erl#L23)

Security Audit and Trailing Functionality

# `event`
*since OTP 18.1* 

```elixir
-callback event(What, Port, Dir, Data) -> term()
                   when
                       What :: auth_fail | user_block | user_unblock,
                       Port :: integer(),
                       Dir :: string(),
                       Data :: [Info],
                       Info :: {Name :: term(), Value :: term()}.
```

# `event`
*since OTP 18.1* 

```elixir
-callback event(What, Address, Port, Dir, Data) -> term()
                   when
                       What :: auth_fail | user_block | user_unblock,
                       Port :: integer(),
                       Address :: inet:ip4_address() | inet:ip6_address() | string(),
                       Dir :: string(),
                       Data :: [Info],
                       Info :: {Name :: term(), Value :: term()}.
```

[`event/4`](`c:event/4`) or [`event/5`](`c:event/5`) is called whenever an event
occurs in the `mod_security` Erlang web server API module.
([`event/4`](`c:event/4`) is called if `Address` is undefined, otherwise
[`event/5`](`c:event/5`). Argument `What` specifies the type of event that has
occurred and is one of the following reasons:

- **`auth_fail`** - A failed user authentication.

- **`user_block`** - A user is being blocked from access.

- **`user_unblock`** - A user is being removed from the block list.

> #### Note {: .info }
>
> The event `user_unblock` is not triggered when a user is removed from the
> block list explicitly using the `unblock_user` function.

# `block_user`

```elixir
-spec block_user(User, Port, Dir, Seconds) -> true | {error, Reason}
                    when
                        User :: string(),
                        Port :: inet:port_number(),
                        Dir :: string(),
                        Seconds :: non_neg_integer() | infinity,
                        Reason :: no_such_directory.
```

# `block_user`

```elixir
-spec block_user(User, Address, Port, Dir, Seconds) -> true | {error, Reason}
                    when
                        User :: string(),
                        Port :: inet:port_number(),
                        Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                        Dir :: string(),
                        Seconds :: non_neg_integer() | infinity,
                        Reason :: no_such_directory.
```

[`block_user/4`](`block_user/4`) and [`block_user/5`](`block_user/5`) each
blocks the user `User` from directory `Dir` for a specified amount of time.

# `list_auth_users`

```elixir
-spec list_auth_users(Port) -> Users | [] when Port :: inet:port_number(), Users :: [string()].
```

# `list_auth_users`

```elixir
-spec list_auth_users(Port, Directory) -> Users | []
                         when Port :: inet:port_number(), Directory :: string(), Users :: [string()];
                     (Address, Port) -> Users | []
                         when
                             Port :: inet:port_number(),
                             Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                             Users :: [string()].
```

# `list_auth_users`

```elixir
-spec list_auth_users(Address, Port, Dir) -> Users | []
                         when
                             Port :: inet:port_number(),
                             Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                             Dir :: string(),
                             Users :: [string()].
```

[`list_auth_users/1`](`list_auth_users/1`),
[`list_auth_users/2`](`list_auth_users/2`), and
[`list_auth_users/3`](`list_auth_users/3`) each returns a list of users that are
currently authenticated. Authentications are stored for `SecurityAuthTimeout`
seconds, and then discarded.

# `list_blocked_users`

```elixir
-spec list_blocked_users(Port) -> Users | []
                            when
                                Port :: integer(),
                                Users :: [{blocked_user, term(), term(), term(), term()}].
```

# `list_blocked_users`

```elixir
-spec list_blocked_users(Port, Directory) -> Users | []
                            when
                                Port :: integer(),
                                Directory :: string(),
                                Users :: [{blocked_user, term(), term(), term(), term()}];
                        (Address, Port) -> Users | []
                            when
                                Port :: integer(),
                                Address ::
                                    inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                Users :: [{blocked_user, term(), term(), term(), term()}].
```

# `list_blocked_users`

```elixir
-spec list_blocked_users(Address, Port, Dir) -> Users | []
                            when
                                Port :: integer(),
                                Address ::
                                    inet:ip4_address() | inet:ip6_address() | string() | undefined,
                                Dir :: string(),
                                Users :: [{blocked_user, term(), term(), term(), term()}].
```

[`list_blocked_users/1`](`list_blocked_users/1`),
[`list_blocked_users/2`](`list_blocked_users/2`), and
[`list_blocked_users/3`](`list_blocked_users/3`) each returns a list of users
that are currently blocked from access.

# `unblock_user`

```elixir
-spec unblock_user(User, Port) -> true | {error, Reason}
                      when User :: string(), Port :: integer(), Reason :: term().
```

# `unblock_user`

```elixir
-spec unblock_user(User, Port, Directory) -> true | {error, Reason}
                      when User :: string(), Port :: integer(), Directory :: string(), Reason :: term();
                  (User, Address, Port) -> true | {error, Reason}
                      when
                          User :: string(),
                          Port :: integer(),
                          Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                          Reason :: term().
```

# `unblock_user`

```elixir
-spec unblock_user(User, Address, Port, Dir) -> true | {error, Reason}
                      when
                          User :: string(),
                          Port :: integer(),
                          Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
                          Dir :: string(),
                          Reason :: term().
```

[`unblock_user/2`](`unblock_user/2`), [`unblock_user/3`](`unblock_user/3`), and
[`unblock_user/4`](`unblock_user/4`) each removes the user `User` from the list
of blocked users for `Port` (and `Dir`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
