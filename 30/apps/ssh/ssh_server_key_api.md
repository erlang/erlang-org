# `ssh_server_key_api`
[🔗](https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh_server_key_api.erl#L23)

\-behaviour(ssh_server_key_api).

Behaviour describing the API for public key handling of an SSH server. By
implementing the callbacks defined in this behavior, the public key handling of
an SSH server can be customized. By default the SSH application implements this
behavior with help of the standard OpenSSH files, see the [ssh](ssh_app.md)
application manual.

# `daemon_key_cb_options`
*since OTP R16B* 

```erlang
-type daemon_key_cb_options(T) :: [{key_cb_private, [T]} | ssh:daemon_option()].
```

Options provided to [ssh:daemon/2,3](`ssh:daemon/2`).

The option list given in the [`key_cb`](`t:ssh:key_cb_common_option/0`) option
is available with the key `key_cb_private`.

# `host_key`
*since OTP R16B* 

```erlang
-callback host_key(Algorithm :: ssh:pubkey_alg(), DaemonOptions :: daemon_key_cb_options(any())) ->
                      {ok, PrivateKey :: public_key:private_key()} | {error, term()}.
```

Fetches the private key of the host.

# `is_auth_key`
*since OTP R16B* 

```erlang
-callback is_auth_key(PublicKey :: public_key:public_key(),
                      User :: string(),
                      DaemonOptions :: daemon_key_cb_options(any())) ->
                         boolean().
```

Checks if the user key is authorized.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
