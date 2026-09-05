# `ssh_file`
[🔗](https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh_file.erl#L27)

Default callback module for the client's and server's database operations in the
ssh application

This module is the default callback handler for the client's and the server's
user and host "database" operations. All data, for instance key pairs, are
stored in files in the normal file system. This page documents the files, where
they are stored and configuration options for this callback module.

The intention is to be compatible with the [OpenSSH](http://www.openssh.com)
storage in files. Therefore it mimics directories and filenames of
[OpenSSH](http://www.openssh.com).

Ssh_file implements the `m:ssh_server_key_api` and the `m:ssh_client_key_api`.
This enables the user to make an own interface using for example a database
handler.

Such another callback module could be used by setting the option
[`key_cb`](`t:ssh:key_cb_common_option/0`) when starting a client or a server
(with for example [ssh:connect](`ssh:connect/3`), [ssh:daemon](`ssh:daemon/2`)
or [ssh:shell](`ssh:shell/1`) ).

> #### Callbacks {: .info }
>
> The callback functions (`host_key/2`, `is_auth_key/3`, `user_key/2`,
> `is_host_key/5`, `add_host_key/4`) are called by the SSH application
> internally. They are documented here to show which files and options
> the default implementation uses. See `m:ssh_server_key_api` and
> `m:ssh_client_key_api` for the callback specifications.
>
> The public API functions are `decode/2`, `encode/2`, and
> `extract_public_key/1`.

## Files, directories and who uses them

### Daemons

Daemons uses all files stored in the [SYSDIR](`m:ssh_file#SYSDIR`) directory.

Optionally, in case of `publickey` authorization, one or more of the remote
user's public keys in the [USERDIR](`m:ssh_file#USERDIR`) directory are used.
See the files [`USERDIR/authorized_keys`](`m:ssh_file#FILE-authorized_keys`) and
[`USERDIR/authorized_keys2`](`m:ssh_file#FILE-authorized_keys2`).

### Clients

Clients uses all files stored in the [USERDIR](`m:ssh_file#USERDIR`) directory.

### Directory contents

- **[](){: #LOCALUSER } LOCALUSER**  
  The user name of the OS process running the Erlang virtual machine (emulator).

- **[](){: #SYSDIR } SYSDIR**  
  This is the directory holding the server's files:

  - [](){: #FILE-ssh_host_STAR_key } `ssh_host_dsa_key`{: #FILE-ssh_host_dsa_key
    } \- private dss host key (optional)
  - `ssh_host_rsa_key`{: #FILE-ssh_host_rsa_key } \- private rsa host key
    (optional)
  - `ssh_host_ecdsa_key`{: #FILE-ssh_host_ecdsa_key } \- private ecdsa host key
    (optional)
  - `ssh_host_ed25519_key`{: #FILE-ssh_host_ed25519_key } \- private eddsa host
    key for curve 25519 (optional)
  - `ssh_host_ed448_key`{: #FILE-ssh_host_ed448_key } \- private eddsa host key
    for curve 448 (optional)

  The key files could be generated with OpenSSH's ssh-keygen command.

  At least one host key must be defined. The default value of SYSDIR is
  `/etc/ssh`{: ##/etc/ssh }.

  For security reasons, this directory is normally accessible only to the root
  user.

  To change the SYSDIR, see the [system_dir](`t:system_dir_daemon_option/0`)
  option.

- **[](){: #USERDIR } USERDIR**  
  This is the directory holding the files:

  - `authorized_keys`{: #FILE-authorized_keys } and, as second alternative
    `authorized_keys2`{: #FILE-authorized_keys2 } \- the user's public keys are
    stored concatenated in one of those files.

    It is composed of lines as for
    [OpenSSH](https://man.openbsd.org/sshd#AUTHORIZED_KEYS_FILE_FORMAT):

    ```text
    (options)? keytype base64-encoded-key comment
    ```

    where

    ```text
    options :: option(,option)*
    option :: % All options are skipped
    keytype :: 'ssh-dsa'
             | 'ssh-rsa'
             | 'ssh-ecdsa-nistp256'
    	 | 'ssh-ecdsa-nistp384'
             | 'ssh-ecdsa-nistp521'
             | 'ssh-ed25519'
    	 | 'ssh-ed448'
    base64-encoded-key :: % The user's public key
    comment :: % Comments are skipped
    ```

  - `known_hosts`{: #FILE-known_hosts } \- host keys from hosts visited
    concatenated. The file is created and used by the client.

    It is composed of lines as for
    [OpenSSH](https://man.openbsd.org/sshd#SSH_KNOWN_HOSTS_FILE_FORMAT):

    ```text
    (option)? pattern(,pattern)* keytype key (comment)?
    ```

    where

    ```text
    option :: '@revoked'
    pattern :: host | '[' host ']:' port
    host :: ip-address | hostname | '*'
    port :: portnumber | '*'
    keytype :: 'ssh-dsa'
             | 'ssh-rsa'
             | 'ssh-ecdsa-nistp256'
    	 | 'ssh-ecdsa-nistp384'
             | 'ssh-ecdsa-nistp521'
             | 'ssh-ed25519'
    	 | 'ssh-ed448'
    key :: % encoded key from eg ssh_host_*.pub
    ```

  - [](){: #FILE-id_STAR } `id_dsa`{: #FILE-id_dsa } \- private dss user key
    (optional)
  - `id_rsa`{: #FILE-id_rsa } \- private rsa user key (optional)
  - `id_ecdsa`{: #FILE-id_ecdsa } \- private ecdsa user key (optional)
  - `id_ed25519`{: #FILE-id_ed25519 } \- private eddsa user key for curve 25519
    (optional)
  - `id_ed448`{: #FILE-id_ed448 } \- private eddsa user key for curve 448
    (optional)

  The key files could be generated with OpenSSH's ssh-keygen command.

  The default value of USERDIR is
  `/home/`[`LOCALUSER`](`m:ssh_file#LOCALUSER`)`/.ssh`.

  To change the USERDIR, see the [user_dir](`t:user_dir_common_option/0`) option

# `experimental_openssh_key_v1_decode`
*since OTP 21.2* 

```erlang
-type experimental_openssh_key_v1_decode() :: [{key(), openssh_key_v1_attributes()}].
```

# `experimental_openssh_key_v1_encode`
*since OTP 21.2* 

```erlang
-type experimental_openssh_key_v1_encode() ::
          [{[{public_key:public_key(), public_key:private_key(), Comment :: binary()} |
             {public_key:private_key(), Comment :: binary()}],
            openssh_key_v1_attributes()}].
```

# `key`
*since OTP 21.2* 

```erlang
-type key() :: public_key:public_key() | public_key:private_key().
```

The key representation

# `openssh_key_v1_attributes`
*since OTP 21.2* 

```erlang
-type openssh_key_v1_attributes() :: [{atom(), term()}].
```

Types for the experimental implementation of the `openssh_key_v1` format.

# `optimize_key_lookup`
*since OTP 21.2* 

```erlang
-type optimize_key_lookup() :: {optimize, time | space}.
```

Make the handling of large files fast by setting `time`, but this will use more
memory. The `space` variant shrinks the memory requirements, but with a higher
time consumption.

To set it, set the option `{key_cb, {ssh_file, [{optimize,TimeOrSpace}]}` in the
call of [ssh:connect/3](`ssh:connect/3`), `ssh:daemon/2` or similar function
call that initiates an ssh connection.

# `pubkey_passphrase_client_options`
*since OTP 21.2* 

```erlang
-type pubkey_passphrase_client_options() ::
          {dsa_pass_phrase, string()} | {rsa_pass_phrase, string()} | {ecdsa_pass_phrase, string()}.
```

If the user's DSA, RSA or ECDSA key is protected by a passphrase, it can be
supplied with those options.

Note that EdDSA passhrases (Curves 25519 and 448) are not implemented.

# `system_dir_daemon_option`
*since OTP 21.2* 

```erlang
-type system_dir_daemon_option() :: {system_dir, string()}.
```

Sets the [system directory](`m:ssh_file#SYSDIR`).

# `user2dir`
*since OTP 21.2* 

```erlang
-type user2dir() :: fun((RemoteUserName :: string()) -> UserDir :: string()).
```

Sets the [user directory](`m:ssh_file#USERDIR`) dynamically by evaluating the
`user2dir` function.

# `user_dir_common_option`
*since OTP 21.2* 

```erlang
-type user_dir_common_option() :: {user_dir, string()}.
```

Sets the [user directory](`m:ssh_file#USERDIR`).

# `user_dir_fun_common_option`
*since OTP 21.2* 

```erlang
-type user_dir_fun_common_option() :: {user_dir_fun, user2dir()}.
```

# `decode`
*since OTP 24.0* 

```erlang
-spec decode(SshBin, Type) -> Decoded | {error, term()}
                when
                    SshBin :: binary(),
                    Type ::
                        ssh2_pubkey | public_key | openssh_key | rfc4716_key | openssh_key_v1 |
                        known_hosts | auth_keys,
                    Decoded ::
                        Decoded_ssh2_pubkey | Decoded_public | Decoded_openssh | Decoded_rfc4716 |
                        Decoded_openssh_key_v1 | Decoded_known_hosts | Decoded_auth_keys,
                    Decoded_ssh2_pubkey :: public_key:public_key(),
                    Decoded_public :: Decoded_rfc4716 | Decoded_openssh_key_v1 | Decoded_openssh,
                    Decoded_openssh :: [{public_key:public_key(), [{comment, string()}]}],
                    Decoded_rfc4716 :: [{key(), [{headers, Attrs}]}],
                    Decoded_openssh_key_v1 :: experimental_openssh_key_v1_decode(),
                    Decoded_known_hosts ::
                        [{public_key:public_key(), [{comment, string()} | {hostnames, [string()]}]}],
                    Decoded_auth_keys ::
                        [{public_key:public_key(), [{comment, string()} | {options, [string()]}]}],
                    Attrs :: {Key :: string(), Value :: string()}.
```

Decodes an SSH file-binary.

If `Type` is `public_key` the binary can be either an RFC4716 public key or an
OpenSSH public key.

> #### Note {: .info }
>
> The implementation of the `openssh_key_v1` format is still experimental.

# `encode`
*since OTP 24.0* 

```erlang
-spec encode(InData, Type) -> binary() | {error, term()}
                when
                    Type ::
                        ssh2_pubkey | openssh_key | rfc4716_key | openssh_key_v1 | known_hosts |
                        auth_keys,
                    InData ::
                        InData_ssh2_pubkey | InData_openssh | InData_rfc4716 | InData_openssh_key_v1 |
                        InData_known_hosts | InData_auth_keys,
                    InData_ssh2_pubkey :: public_key:public_key(),
                    InData_openssh :: [{public_key:public_key(), [{comment, string()}]}],
                    InData_rfc4716 :: [{key(), [{headers, Attrs}]}],
                    InData_openssh_key_v1 :: experimental_openssh_key_v1_encode(),
                    InData_known_hosts ::
                        [{public_key:public_key(), [{comment, string()} | {hostnames, [string()]}]}],
                    InData_auth_keys ::
                        [{public_key:public_key(), [{comment, string()} | {options, [string()]}]}],
                    Attrs :: {Key :: string(), Value :: string()}.
```

Encodes a list of SSH file entries (public keys and attributes) to a binary.

> #### Note {: .info }
>
> The implementation of the `openssh_key_v1` format is still experimental.

# `extract_public_key`
*since OTP 25.0* 

```erlang
-spec extract_public_key(PrivKey) -> PubKey
                            when PrivKey :: public_key:private_key(), PubKey :: public_key:public_key().
```

Fetches the public key from a private key.

# `add_host_key`
*since OTP 23.0* 

```erlang
-spec add_host_key(Host, Port, Key, Options) -> Result
                      when
                          Host ::
                              inet:ip_address() |
                              inet:hostname() |
                              [inet:ip_address() | inet:hostname()],
                          Port :: inet:port_number(),
                          Key :: public_key:public_key(),
                          Options :: ssh_client_key_api:client_key_cb_options(none()),
                          Result :: ok | {error, term()}.
```

Implements `c:ssh_client_key_api:add_host_key/4`.

[](){: #add_host_key-3 }

**Option**

- [user_dir](`t:user_dir_common_option/0`)

**File**

- [`USERDIR/known_hosts`](`m:ssh_file#FILE-known_hosts`)

# `host_key`
*since OTP 21.2* 

```erlang
-spec host_key(Algorithm, Options) -> Result
                  when
                      Algorithm :: ssh:pubkey_alg(),
                      Result :: {ok, public_key:private_key()} | {error, term()},
                      Options :: ssh_server_key_api:daemon_key_cb_options(none()).
```

Implements `c:ssh_server_key_api:host_key/2`.

**Options**

- [system_dir](`t:system_dir_daemon_option/0`)

**Files**

- [`SYSDIR/ssh_host_rsa_key`](`m:ssh_file#FILE-ssh_host_rsa_key`)
- [`SYSDIR/ssh_host_dsa_key`](`m:ssh_file#FILE-ssh_host_dsa_key`)
- [`SYSDIR/ssh_host_ecdsa_key`](`m:ssh_file#FILE-ssh_host_ecdsa_key`)
- [`SYSDIR/ssh_host_ed25519_key`](`m:ssh_file#FILE-ssh_host_ed25519_key`)
- [`SYSDIR/ssh_host_ed448_key`](`m:ssh_file#FILE-ssh_host_ed448_key`)

# `is_auth_key`
*since OTP 21.2* 

```erlang
-spec is_auth_key(Key, User, Options) -> boolean()
                     when
                         Key :: public_key:public_key(),
                         User :: string(),
                         Options :: ssh_server_key_api:daemon_key_cb_options(optimize_key_lookup()).
```

Implements `c:ssh_server_key_api:is_auth_key/3`.

**Options**

- [user_dir_fun](`t:user_dir_fun_common_option/0`)
- [user_dir](`t:user_dir_common_option/0`)

**Files**

- [`USERDIR/authorized_keys`](`m:ssh_file#FILE-authorized_keys`)
- [`USERDIR/authorized_keys2`](`m:ssh_file#FILE-authorized_keys2`)

This functions discards all options in the beginning of the lines of thoose
files when reading them.

# `is_host_key`
*since OTP 23.0* 

```erlang
-spec is_host_key(Key, Host, Port, Algorithm, Options) -> Result
                     when
                         Key :: public_key:public_key(),
                         Host ::
                             inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
                         Port :: inet:port_number(),
                         Algorithm :: ssh:pubkey_alg(),
                         Options :: ssh_client_key_api:client_key_cb_options(optimize_key_lookup()),
                         Result :: boolean() | {error, term()}.
```

Implements `c:ssh_client_key_api:is_host_key/5`.

[](){: #is_host_key-4 }

**Option**

- [user_dir](`t:user_dir_common_option/0`)

**File**

- [`USERDIR/known_hosts`](`m:ssh_file#FILE-known_hosts`)

# `user_key`
*since OTP 21.2* 

```erlang
-spec user_key(Algorithm, Options) -> Result
                  when
                      Algorithm :: ssh:pubkey_alg(),
                      Result :: {ok, public_key:private_key()} | {error, string()},
                      Options :: ssh_client_key_api:client_key_cb_options(none()).
```

Implements `c:ssh_client_key_api:user_key/2`.

**Options**

- [user_dir](`t:user_dir_common_option/0`)
- [dsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)
- [rsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)
- [ecdsa_pass_phrase](`t:pubkey_passphrase_client_options/0`)

Note that EdDSA passhrases (Curves 25519 and 448) are not implemented.

**Files**

- [`USERDIR/id_dsa`](`m:ssh_file#FILE-id_dsa`)
- [`USERDIR/id_rsa`](`m:ssh_file#FILE-id_rsa`)
- [`USERDIR/id_ecdsa`](`m:ssh_file#FILE-id_ecdsa`)
- [`USERDIR/id_ed25519`](`m:ssh_file#FILE-id_ed25519`)
- [`USERDIR/id_ed448`](`m:ssh_file#FILE-id_ed448`)

---

*Consult [api-reference.md](api-reference.md) for complete listing*
