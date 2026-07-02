# `ssh_agent`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/ssh/src/ssh_agent.erl#L25)

Callback module for using an SSH agent instead of the default ssh_file callback.

This module defines a callback handler for the communication with an
[SSH Agent](https://tools.ietf.org/html/draft-miller-ssh-agent-02) and can be
used to replace the [default callback](`m:ssh_file`). This allows to issue
signing requests to an agent that stores SSH private keys to perform
authentication.

Ssh_agent implements the `m:ssh_client_key_api`, to allow it to be used by
setting the option [`key_cb`](`t:ssh:key_cb_common_option/0`) when starting a
client (with for example [ssh:connect](`ssh:connect/3`),
[ssh:shell](`ssh:shell/1`) ).

```erlang
      {key_cb, {ssh_agent, []}}
```

The agent communication is established through a UNIX domain socket. By default,
the socket path will be fetched from the `SSH_AUTH_SOCK` environment variable,
which is the default socket path in the agent implementation of
[OpenSSH](http://www.openssh.com).

[](){: #SOCKET_PATH } In order to set a different socket path the `socket_path`
option can be set.

```erlang
      {key_cb, {ssh_agent, [{socket_path, SocketPath}]}}
```

> #### Note {: .info }
>
> The functions are _Callbacks_ for the SSH application. They are not intended to be
> called from the user's code\!

# `call_ssh_file_option`
*since OTP 23.0* 

```erlang
-type call_ssh_file_option() :: {call_ssh_file, atom()}.
```

The module which the `add_host_key` and `is_host_key` callbacks are delegated
to. Defaults to the `m:ssh_file` module.

# `socket_path_option`
*since OTP 23.0* 

```erlang
-type socket_path_option() :: {socket_path, string()}.
```

Sets the [socket path](`m:ssh_agent#SOCKET_PATH`) for the communication with the
agent.

# `timeout_option`
*since OTP 23.0* 

```erlang
-type timeout_option() :: {timeout, timeout()}.
```

Sets the time-out in milliseconds when communicating with the agent via the
socket. The default value is `1000`.

# `add_host_key`
*since OTP 23.0* 

```erlang
-spec add_host_key(string(), public_key:public_key(), Options) -> ok | {error, Error :: term()}
                      when Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()).
```

# `add_host_key`
*since OTP 23.0* 

```erlang
-spec add_host_key(Host, inet:port_number(), public_key:public_key(), Options) -> Result
                      when
                          Host ::
                              inet:ip_address() |
                              inet:hostname() |
                              [inet:ip_address() | inet:hostname()],
                          Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()),
                          Result :: ok | {error, Error :: term()}.
```

This callback is delegated to the [ssh_file](`ssh_file:add_host_key/4`) module.

# `is_host_key`
*since OTP 23.0* 

```erlang
-spec is_host_key(Key :: public_key:public_key(),
                  Host :: string(),
                  Algorithm :: ssh:pubkey_alg(),
                  Options) ->
                     boolean()
                     when Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()).
```

# `is_host_key`
*since OTP 23.0* 

```erlang
-spec is_host_key(public_key:public_key(), Host, inet:port_number(), ssh:pubkey_alg(), Options) ->
                     boolean()
                     when
                         Host ::
                             inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
                         Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()).
```

This callback is delegated to the [ssh_file](`ssh_file:is_host_key/5`) module.

# `user_key`
*since OTP 23.0* 

```erlang
-spec user_key(Algorithm :: ssh:pubkey_alg(), Options) -> Result
                  when
                      Result ::
                          {ok, public_key:private_key()} |
                          {ok, {ssh2_pubkey, PubKeyBlob :: binary()}} |
                          {error, string()},
                      Options ::
                          ssh_client_key_api:client_key_cb_options(socket_path_option() |
                                                                   timeout_option()).
```

**Types and description**

See the api description in
[ssh_client_key_api, Module:user_key/2](`c:ssh_client_key_api:user_key/2`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
