# `ssh_client_key_api`
[🔗](https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh_client_key_api.erl#L23)

\-behaviour(ssh_client_key_api).

Behavior describing the API for public key handling of an SSH client. By
implementing the callbacks defined in this behavior, the public key handling of
an SSH client can be customized. By default the `ssh` application implements
this behavior with help of the standard OpenSSH files, see the
[ssh](ssh_app.md) application manual.

# `client_key_cb_options`
*since OTP R16B* 

```erlang
-type client_key_cb_options(T) :: [{key_cb_private, [T]} | ssh:client_option()].
```

Options provided to [ssh:connect/3,4](`ssh:connect/3`).

The option list given in the [`key_cb`](`t:ssh:key_cb_common_option/0`) option
is available with the key `key_cb_private`.

# `add_host_key`
*since OTP R16B* *optional* 

```erlang
-callback add_host_key(Host :: string(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())) ->
                          ok | {error, term()}.
```

This function is retired in favour for
[`Module:add_host_key/4`](`c:add_host_key/4`) which is the preferred API
function. The calling SSH application will still try the
[`add_host_key/3`](`c:add_host_key/3`) if the call to
[`add_host_key/4`](`c:add_host_key/4`) failed.

Adds a host key to the set of trusted host keys.

# `add_host_key`
*since OTP R16B* *optional* 

```erlang
-callback add_host_key(Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
                       Port :: inet:port_number(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())) ->
                          ok | {error, term()}.
```

Adds a host key to the set of trusted host keys.

This function is preferred to the old
[`Module:add_host_key/3`](`c:add_host_key/3`) since it also uses the peer host
port number and may return an error message.

The OTP/SSH application first calls this function in the callback module, and
then the old [`Module:add_host_key/3`](`c:add_host_key/3`) for compatibility.

# `is_host_key`
*since OTP R16B* *optional* 

```erlang
-callback is_host_key(Key :: public_key:public_key(),
                      Host :: string(),
                      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())) ->
                         boolean().
```

This function is retired in favour for
[`Module:is_host_key/5`](`c:is_host_key/5`) which is the preferred API function.
The calling SSH application will still try the
[`is_host_key/4`](`c:is_host_key/4`) if the call to
[`is_host_key/5`](`c:is_host_key/5`) failed.

Checks if a host key is trusted.

# `is_host_key`
*since OTP 23.0* *optional* 

```erlang
-callback is_host_key(Key :: public_key:public_key(),
                      Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
                      Port :: inet:port_number(),
                      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())) ->
                         boolean() | {error, term()}.
```

Checks if a host key is trusted.

This function is preferred to the old
[`Module:is_host_key/4`](`c:is_host_key/4`) since it also uses the peer host
port number and may return an error message.

The OTP/SSH application first calls this function in the callback module, and
then the old [`Module:is_host_key/4`](`c:is_host_key/4`) for compatibility.

# `sign`
*since OTP 23.0* *optional* 

```erlang
-callback sign(PubKeyBlob :: binary(), SigData :: binary(), Options :: client_key_cb_options(any())) ->
                  Blob :: binary().
```

Sign the SigData with the _private_ key corresponding to PubKeyBlob.

# `user_key`
*since OTP R16B* 

```erlang
-callback user_key(Algorithm :: ssh:pubkey_alg(), Options :: client_key_cb_options(any())) ->
                      {ok, public_key:private_key()} |
                      {ok, {ssh2_pubkey, PubKeyBlob :: binary()}} |
                      {error, string()}.
```

Fetches the users _public key_ matching the `Algorithm`. Some key callback
modules may return `{ssh2_pubkey, PubKeyBlob :: binary()}`.

> #### Note {: .info }
>
> The private key contains the public key.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
