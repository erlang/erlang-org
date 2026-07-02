# `ssh`
[🔗](https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh.erl#L25)

Main API of the ssh application

This is the interface module for the `SSH` application. The Secure Shell (SSH)
Protocol is a protocol for secure remote login and other secure network services
over an insecure network. See [ssh](ssh_app.md#supported) for details of
supported RFCs, versions, algorithms and unicode handling.

With the SSH application it is possible to start _clients_ and to start
_daemons_ (servers).

Clients are started with `connect/2`, `connect/3` or `connect/4`. They open an
encrypted connection on top of TCP/IP. In that encrypted connection one or more
channels could be opened with
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

Each channel is an isolated "pipe" between a client-side process and a
server-side process. Those process pairs could handle for example file transfers
(sftp) or remote command execution (shell, exec and/or cli). If a custom shell
is implemented, the user of the client could execute the special commands
remotely. Note that the user is not necessarily a human but probably a system
interfacing the SSH application.

A server-side subssystem (channel) server is requested by the client with
`ssh_connection:subsystem/4`.

A server (daemon) is started with [daemon/1](`daemon/2`), `daemon/2` or
[daemon/3](`daemon/2`). Possible channel handlers (subsystems) are declared with
the [subsystem](`t:subsystem_daemon_option/0`) option when the daemon is
started.

To just run a shell on a remote machine, there are functions that bundles the
needed three steps needed into one: [shell/1,2,3](`shell/1`). Similarly, to just
open an sftp (file transfer) connection to a remote machine, the simplest way is
to use [ssh_sftp:start_channel/1,2,3](`ssh_sftp:start_channel/1`).

To write your own client channel handler, use the behaviour
`m:ssh_client_channel`. For server channel handlers use `m:ssh_server_channel`
behaviour (replaces ssh_daemon_channel).

Both clients and daemons accept options that control the exact behaviour. Some
options are common to both. The three sets are called
[Client Options](`t:client_options/0`), [Daemon Options](`t:daemon_options/0`)
and [Common Options](`t:common_options/0`).

The descriptions of the options uses the
[Erlang Type Language](`e:system:typespec.md`) with explaining text.

> #### Note {: .info }
>
> See also [SSH Application Reference](index.html) and [Examples](using_ssh.md) section.

## Keys and files

A number of objects must be present for the SSH application to work. Those
objects are per default stored in files. The default names, paths and file
formats are the same as for [OpenSSH](http://www.openssh.com). Keys could be
generated with the `ssh-keygen` program from OpenSSH. See the
[User's Guide](using_ssh.md#running-an-erlang-ssh-daemon).

The paths could easily be changed by options:
[`user_dir`](`t:ssh_file:user_dir_common_option/0`) and
[`system_dir`](`t:ssh_file:system_dir_daemon_option/0`).

A completely different storage could be interfaced by writing callback modules
using the behaviours `m:ssh_client_key_api` and/or `m:ssh_server_key_api`. A
callback module is installed with the option
[`key_cb`](`t:key_cb_common_option/0`) to the client and/or the daemon.

### Daemons

The keys are by default stored in files:

- Mandatory: one or more _Host key(s)_, both private and public. Default is to
  store them in the directory `/etc/ssh` in the files

  - `ssh_host_dsa_key` and `ssh_host_dsa_key.pub`
  - `ssh_host_rsa_key` and `ssh_host_rsa_key.pub`
  - `ssh_host_ecdsa_key` and `ssh_host_ecdsa_key.pub`

  The host keys directory could be changed with the option
  [`system_dir`](`t:ssh_file:system_dir_daemon_option/0`).

- Optional: one or more _User's public key_ in case of `publickey`
  authorization. Default is to store them concatenated in the file
  `.ssh/authorized_keys` in the user's home directory.

  The user keys directory could be changed with the option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`).

### Clients

The keys and some other data are by default stored in files in the directory
`.ssh` in the user's home directory.

The directory could be changed with the option
[`user_dir`](`t:ssh_file:user_dir_common_option/0`).

- Optional: a list of _Host public key(s)_ for previously connected hosts. This
  list is handled by the SSH application without any need of user assistance.
  The default is to store them in the file `known_hosts`.

  The `t:host_accepting_client_options/0` are associated with this list of keys.

- Optional: one or more _User's private key(s)_ in case of `publickey`
  authorization. The default files are
  - `id_dsa` and `id_dsa.pub`
  - `id_rsa` and `id_rsa.pub`
  - `id_ecdsa` and `id_ecdsa.pub`

# `accept_callback`

```erlang
-type accept_callback() ::
          fun((PeerName :: string(), fingerprint()) -> boolean()) |
          fun((PeerName :: string(), Port :: inet:port_number(), fingerprint()) -> boolean()).
```

# `accept_hosts`

```erlang
-type accept_hosts() ::
          boolean() | accept_callback() | {HashAlgoSpec :: fp_digest_alg(), accept_callback()}.
```

# `authentication_client_options`

```erlang
-type authentication_client_options() :: {user, string()} | {password, string()}.
```

- **`user`** - Provides the username. If this option is not given, `ssh` reads
  from the environment (`LOGNAME` or `USER` on UNIX, `USERNAME` on Windows).

- **`password`** - Provides a password for password authentication. If this
  option is not given, the user is asked for a password, if the password
  authentication method is attempted.

# `client_option`

```erlang
-type client_option() ::
          ssh_file:pubkey_passphrase_client_options() |
          host_accepting_client_options() |
          authentication_client_options() |
          diffie_hellman_group_exchange_client_option() |
          connect_timeout_client_option() |
          recv_ext_info_client_option() |
          gen_tcp:connect_option() |
          common_option() |
          experimental_client_options().
```

Options for [clients](`connect/3`). The individual options are further explained
below or by following the hyperlinks.

Note that not every `t:gen_tcp:connect_option/0` is accepted. See
`set_sock_opts/2` for a list of prohibited options.

Also note that setting a `t:gen_tcp:connect_option/0` could change the socket in
a way that impacts the ssh client's behaviour negatively. You use it on your own
risk.

# `client_options`

```erlang
-type client_options() :: [client_option()].
```

# `connect_timeout_client_option`

```erlang
-type connect_timeout_client_option() :: {connect_timeout, timeout()}.
```

Sets a timeout on the transport layer connect time. For `m:gen_tcp` the time is
in milli-seconds and the default value is `infinity`.

See the parameter `Timeout` in `connect/4` for a timeout of the negotiation
phase.

# `diffie_hellman_group_exchange_client_option`

```erlang
-type diffie_hellman_group_exchange_client_option() ::
          {dh_gex_limits, {Min :: pos_integer(), I :: pos_integer(), Max :: pos_integer()}}.
```

Sets the three diffie-hellman-group-exchange parameters that guides the
connected server in choosing a group. See
[RFC 4419](https://tools.ietf.org/html/rfc4419) for the details. The default
value is `{1024, 6144, 8192}`.

# `fingerprint`

```erlang
-type fingerprint() :: string() | [string()].
```

# `fp_digest_alg`

```erlang
-type fp_digest_alg() :: md5 | crypto:sha1() | crypto:sha2().
```

# `host_accepting_client_options`

```erlang
-type host_accepting_client_options() ::
          {silently_accept_hosts, accept_hosts()} |
          {user_interaction, boolean()} |
          {save_accepted_host, boolean()} |
          {quiet_mode, boolean()}.
```

- **`silently_accept_hosts`{: #hardening_client_options-silently_accept_hosts
  }** - This option guides the `connect` function on how to act when the
  connected server presents a Host Key that the client has not seen before. The
  default is to ask the user with a question on stdio of whether to accept or
  reject the new Host Key. See the option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for specifying the path to
  the file `known_hosts` where previously accepted Host Keys are recorded. See
  also the option [key_cb](`t:key_cb_common_option/0`) for the general way to
  handle keys.

  The option can be given in three different forms as seen
  [above](`t:accept_hosts/0`):

  - The value is a `t:boolean/0`. The value `true` will make the client accept
    any unknown Host Key without any user interaction. The value `false`
    preserves the default behaviour of asking the user on stdio.
  - An `t:accept_callback/0` will be called and the boolean return value `true`
    will make the client accept the Host Key. A return value of `false` will
    make the client to reject the Host Key and as a result the connection will
    be closed. The arguments to the fun are:
    - `PeerName` \- a string with the name or address of the remote host.
    - `FingerPrint` \- the fingerprint of the Host Key as
      `hostkey_fingerprint/1` calculates it.
  - A tuple `{HashAlgoSpec, accept_callback}`. The `HashAlgoSpec` specifies
    which hash algorithm shall be used to calculate the fingerprint used in the
    call of the `t:accept_callback/0`. The `HashALgoSpec` is either an atom or a
    list of atoms as the first argument in `hostkey_fingerprint/2`. If it is a
    list of hash algorithm names, the `FingerPrint` argument in the
    `t:accept_callback/0` will be a list of fingerprints in the same order as
    the corresponding name in the `HashAlgoSpec` list.

- **`user_interaction`** - If `false`, disables the client to connect to the
  server if any user interaction is needed, such as accepting the server to be
  added to the `known_hosts` file, or supplying a password.

  Even if user interaction is allowed it can be suppressed by other options,
  such as `silently_accept_hosts` and `password`. However, those options are not
  always desirable to use from a security point of view.

  Defaults to `true`.

- **`save_accepted_host`** - If `true`, the client saves an accepted host key to
  avoid the accept question the next time the same host is connected. If the
  option [`key_cb`](`t:key_cb_common_option/0`) is not present, the key is saved
  in the file "known_hosts". See option
  [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for the location of that
  file.

  If `false`, the key is not saved and the key will still be unknown at the next
  access of the same host.

  Defaults to `true`

- **`quiet_mode`** - If `true`, the client does not print anything on
  authorization.

  Defaults to `false`

# `recv_ext_info_client_option`

```erlang
-type recv_ext_info_client_option() :: {recv_ext_info, boolean()}.
```

Make the client tell the server that the client accepts extension negotiation,
that is, include `ext-info-c` in the kexinit message sent. See
[RFC 8308](https://tools.ietf.org/html/rfc8308) for details and
[ssh](ssh_app.md#supported-ext-info) for a list of currently implemented
extensions.

Default value is `true` which is compatible with other implementations not
supporting ext-info.

# `alg_entry`

```erlang
-type alg_entry() ::
          {kex, [kex_alg()]} |
          {public_key, [pubkey_alg()]} |
          {cipher, double_algs(cipher_alg())} |
          {mac, double_algs(mac_alg())} |
          {compression, double_algs(compression_alg())}.
```

# `algs_list`

```erlang
-type algs_list() :: [alg_entry()].
```

# `alive_common_option`

```erlang
-type alive_common_option() ::
          {alive, #{count_max := CountMax :: pos_integer(), interval := Interval :: timeout()}}.
```

This option is used to configure the alive messages. Alive messages are sent
through the encrypted channel and are typically used to detect that a
connection became unresponsive.

`count_max` sets the maximum number
of alive messages which may be sent without receiving any messages back
from the peer. If this threshold is reached the connection will be terminated.
`interval` sets a timeout interval, in milliseconds, after which, if no data
has been received from the peer, a message to request a response from the peer is sent.

The default is `#{count_max => 3, interval => infinity}`, which means that alive
messages will not be sent to the peer, since the `interval` is set to `infinity`.

No alive messages are sent during renegotiation, however, a timeout derived from
the alive parameters is set to ensure that unresponsive connections are terminated.

# `auth_methods_common_option`

```erlang
-type auth_methods_common_option() :: {auth_methods, string()}.
```

Comma-separated string that determines which authentication methods that the
client shall support and in which order they are tried. Defaults to
`"publickey,keyboard-interactive,password"`

Note that the client is free to use any order and to exclude methods.

# `cipher_alg`

```erlang
-type cipher_alg() ::
          'aes128-ctr' | 'aes128-gcm@openssh.com' | 'aes192-ctr' | 'aes256-ctr' |
          'aes256-gcm@openssh.com' | 'chacha20-poly1305@openssh.com' |
          disabled_cipher_alg() |
          legacy_cipher_alg().
```

# `common_option`

```erlang
-type common_option() ::
          ssh_file:user_dir_common_option() |
          profile_common_option() |
          max_idle_time_common_option() |
          max_log_item_len_common_option() |
          key_cb_common_option() |
          disconnectfun_common_option() |
          unexpectedfun_common_option() |
          ssh_msg_debug_fun_common_option() |
          rekey_limit_common_option() |
          id_string_common_option() |
          pref_public_key_algs_common_option() |
          preferred_algorithms_common_option() |
          modify_algorithms_common_option() |
          auth_methods_common_option() |
          inet_common_option() |
          fd_common_option() |
          alive_common_option().
```

The options above can be used both in clients and in daemons (servers). They are
further explained below.

# `common_options`

```erlang
-type common_options() :: [common_option()].
```

# `compression_alg`

```erlang
-type compression_alg() :: none | 'zlib@openssh.com' | legacy_compression_alg().
```

# `disabled_cipher_alg`

```erlang
-type disabled_cipher_alg() :: 'AEAD_AES_128_GCM' | 'AEAD_AES_256_GCM'.
```

# `disabled_mac_alg`

```erlang
-type disabled_mac_alg() :: 'AEAD_AES_128_GCM' | 'AEAD_AES_256_GCM'.
```

# `disconnectfun_common_option`

```erlang
-type disconnectfun_common_option() :: {disconnectfun, fun((Reason :: term()) -> void | any())}.
```

Provides a fun to implement your own logging or other handling at disconnects.

# `double_algs`

```erlang
-type double_algs(AlgType) :: [{client2server, [AlgType]} | {server2client, [AlgType]}] | [AlgType].
```

List of algorithms to use in the algorithm negotiation. The default
`t:algs_list/0` can be obtained from `default_algorithms/0`.

If an alg_entry() is missing in the algs_list(), the default value is used for
that entry.

Here is an example of this option:

```erlang
	  {preferred_algorithms,
	  [{public_key,['ssh-rsa','ssh-dss']},
	  {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes128-cbc','3des-cbc']}]},
	  {mac,['hmac-sha2-256','hmac-sha1']},
	  {compression,[none,zlib]}
	  ]
	  }
```

The example specifies different algorithms in the two directions (client2server
and server2client), for cipher but specifies the same algorithms for mac and
compression in both directions. The kex (key exchange) is implicit but
public_key is set explicitly.

For background and more examples see the
[User's Guide](configure_algos.md#algorithm-negotiation-in-ssh).

If an algorithm name occurs more than once in a list, the behaviour is
undefined. The tags in the property lists are also assumed to occur at most one
time.

> #### Warning {: .warning }
>
> Changing the values can make a connection less secure. Do not change unless
> you know exactly what you are doing. If you do not understand the values then
> you are not supposed to change them.

# `fd_common_option`

```erlang
-type fd_common_option() :: {fd, gen_tcp:socket()}.
```

Allows an existing file-descriptor to be used (passed on to the transport
protocol).

# `id_string_common_option`

```erlang
-type id_string_common_option() ::
          {id_string, string() | random | {random, Nmin :: pos_integer(), Nmax :: pos_integer()}}.
```

The string the daemon will present to a connecting peer initially. The default
value is "Erlang/VSN" where VSN is the ssh application version number.

The value `random` will cause a random string to be created at each connection
attempt. This is to make it a bit more difficult for a malicious peer to find
the ssh software brand and version.

The value `{random, Nmin, Nmax}` will make a random string with at least `Nmin`
characters and at most `Nmax` characters.

# `inet_common_option`

```erlang
-type inet_common_option() :: {inet, inet | inet6}.
```

IP version to use when the host address is specified as `any`.

# `kex_alg`

```erlang
-type kex_alg() ::
          'diffie-hellman-group-exchange-sha256' | 'diffie-hellman-group14-sha256' |
          'diffie-hellman-group16-sha512' | 'diffie-hellman-group18-sha512' | 'curve25519-sha256' |
          'curve25519-sha256@libssh.org' | 'curve448-sha512' | 'ecdh-sha2-nistp256' |
          'ecdh-sha2-nistp384' | 'ecdh-sha2-nistp521' | 'mlkem768x25519-sha256' |
          legacy_kex_alg().
```

# `key_cb_common_option`

```erlang
-type key_cb_common_option() :: {key_cb, Module :: atom() | {Module :: atom(), Opts :: [term()]}}.
```

Module implementing the behaviour `m:ssh_client_key_api` and/or
`m:ssh_server_key_api`. Can be used to customize the handling of public keys. If
callback options are provided along with the module name, they are made
available to the callback module via the options passed to it under the key
'key_cb_private'.

The `Opts` defaults to `[]` when only the `Module` is specified.

The default value of this option is `{ssh_file, []}`. See also the manpage of
`m:ssh_file`.

A call to the call-back function `F` will be

```text
	  Module:F(..., [{key_cb_private,Opts}|UserOptions])
```

where `...` are arguments to `F` as in `m:ssh_client_key_api` and/or
`m:ssh_server_key_api`. The `UserOptions` are the options given to
[ssh:connect](`connect/3`), [ssh:shell](`shell/1`) or [ssh:daemon](`daemon/2`).

# `limit_bytes`

```erlang
-type limit_bytes() :: non_neg_integer() | infinity.
```

# `limit_time`

```erlang
-type limit_time() :: pos_integer() | infinity.
```

Sets the limit when rekeying is to be initiated. Both the max time and max
amount of data could be configured:

- `{Minutes, Bytes}` initiate rekeying when any of the limits are reached.
- `Bytes` initiate rekeying when `Bytes` number of bytes are transferred, or at
  latest after one hour.

When a rekeying is done, both the timer and the byte counter are restarted.
Defaults to one hour and one GByte.

If `Minutes` is set to `infinity`, no rekeying will ever occur due to that max
time has passed. Setting `Bytes` to `infinity` will inhibit rekeying after a
certain amount of data has been transferred. If the option value is set to
`{infinity, infinity}`, no rekeying will be initiated. Note that rekeying
initiated by the peer will still be performed.

# `mac_alg`

```erlang
-type mac_alg() ::
          'hmac-sha1' | 'hmac-sha1-etm@openssh.com' | 'hmac-sha2-256' | 'hmac-sha2-512' |
          'hmac-sha2-256-etm@openssh.com' | 'hmac-sha2-512-etm@openssh.com' |
          disabled_mac_alg() |
          legacy_mac_alg().
```

# `max_idle_time_common_option`

```erlang
-type max_idle_time_common_option() :: {idle_time, timeout()}.
```

Sets a time-out on a connection when no channels are open. Defaults to
`infinity`. The unit is milliseconds.

The timeout is not active until channels are started, so it does not limit the
time from the connection creation to the first channel opening.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.

# `max_log_item_len_common_option`

```erlang
-type max_log_item_len_common_option() :: {max_log_item_len, limit_bytes()}.
```

Sets a limit for the size of a logged item excluding a header. The unit is bytes
and the value defaults to 500.

# `modify_algorithms_common_option`

```erlang
-type modify_algorithms_common_option() :: {modify_algorithms, modify_algs_list()}.
```

# `modify_algs_list`

```erlang
-type modify_algs_list() :: [{append, algs_list()} | {prepend, algs_list()} | {rm, algs_list()}].
```

Modifies the list of algorithms to use in the algorithm negotiation. The
modifications are applied after the option `preferred_algorithms` (if existing)
is applied.

The algorithm for modifications works like this:

- Input is the `t:modify_algs_list/0` and a set of algorithms `A` obtained from
  the `preferred_algorithms` option if existing, or else from the
  [ssh:default_algorithms/0](`default_algorithms/0`).
- The head of the `t:modify_algs_list/0` modifies `A` giving the result `A'`.

  The possible modifications are:

  - Append or prepend supported but not enabled algorithm(s) to the list of
    algorithms. If the wanted algorithms already are in `A` they will first be
    removed and then appended or prepended,
  - Remove (rm) one or more algorithms from `A`.

- Repeat the modification step with the tail of `t:modify_algs_list/0` and the
  resulting `A'`.

If an unsupported algorithm is in the `t:modify_algs_list/0`, it will be
silently ignored

If there are more than one modify_algorithms options, the result is undefined.

Here is an example of this option:

```text
	  {modify_algorithms,
	  [{prepend, [{kex, ['diffie-hellman-group1-sha1']}],
	  {rm,      [{compression, [none]}]}
	  ]
	  }
```

The example specifies that:

- the old key exchange algorithm 'diffie-hellman-group1-sha1' should be the main
  alternative. It will be the main alternative since it is prepened to the list
- The compression algorithm none (= no compression) is removed so compression is
  enforced

For background and more examples see the
[User's Guide](configure_algos.md#algorithm-negotiation-in-ssh).

# `pref_public_key_algs_common_option`

```erlang
-type pref_public_key_algs_common_option() :: {pref_public_key_algs, [pubkey_alg()]}.
```

List of user (client) public key algorithms to try to use.

The default value is the `public_key` entry in the list returned by
[ssh:default_algorithms/0](`default_algorithms/0`).

If there is no public key of a specified type available, the corresponding entry
is ignored. Note that the available set is dependent on the underlying cryptolib
and current user's public keys.

See also the option [`user_dir`](`t:ssh_file:user_dir_common_option/0`) for
specifying the path to the user's keys.

# `preferred_algorithms_common_option`

```erlang
-type preferred_algorithms_common_option() :: {preferred_algorithms, algs_list()}.
```

# `profile_common_option`

```erlang
-type profile_common_option() :: {profile, atom()}.
```

Used together with `ip-address` and `port` to uniquely identify a ssh daemon.
This can be useful in a virtualized environment, where there can be more that
one server that has the same `ip-address` and `port`. If this property is not
explicitly set, it is assumed that the the `ip-address` and `port` uniquely
identifies the SSH daemon.

# `pubkey_alg`

```erlang
-type pubkey_alg() ::
          'ecdsa-sha2-nistp256' | 'ecdsa-sha2-nistp384' | 'ecdsa-sha2-nistp521' | 'ssh-ed25519' |
          'ssh-ed448' | 'rsa-sha2-256' | 'rsa-sha2-512' |
          legacy_pubkey_alg().
```

# `rekey_limit_common_option`

```erlang
-type rekey_limit_common_option() ::
          {rekey_limit, Bytes :: limit_bytes() | {Minutes :: limit_time(), Bytes :: limit_bytes()}}.
```

# `ssh_msg_debug_fun_common_option`

```erlang
-type ssh_msg_debug_fun_common_option() ::
          {ssh_msg_debug_fun,
           fun((ssh:connection_ref(),
                AlwaysDisplay :: boolean(),
                Msg :: binary(),
                LanguageTag :: binary()) ->
                   any())}.
```

Provide a fun to implement your own logging of the SSH message SSH_MSG_DEBUG.
The last three parameters are from the message, see
[RFC 4253, section 11.3](https://tools.ietf.org/html/rfc4253#section-11.3). The
`t:connection_ref/0` is the reference to the connection on which the message
arrived. The return value from the fun is not checked.

The default behaviour is ignore the message. To get a printout for each message
with `AlwaysDisplay = true`, use for example
`{ssh_msg_debug_fun, fun(_,true,M,_)-> io:format("DEBUG: ~p~n", [M]) end}`

# `unexpectedfun_common_option`

```erlang
-type unexpectedfun_common_option() ::
          {unexpectedfun, fun((Message :: term(), {Host :: term(), Port :: term()}) -> report | skip)}.
```

Provides a fun to implement your own logging or other action when an unexpected
message arrives. If the fun returns `report` the usual info report is issued but
if `skip` is returned no report is generated.

# `authentication_daemon_options`

```erlang
-type authentication_daemon_options() ::
          ssh_file:system_dir_daemon_option() |
          {auth_method_kb_interactive_data, prompt_texts()} |
          {user_passwords, [{UserName :: string(), Pwd :: string()}]} |
          {pk_check_user, boolean()} |
          {password, string()} |
          {pwdfun, pwdfun_2() | pwdfun_4()} |
          {no_auth_needed, boolean()}.
```

# `callbacks_daemon_options`

```erlang
-type callbacks_daemon_options() ::
          {failfun,
           fun((User :: string(), Peer :: {inet:ip_address(), inet:port_number()}, Reason :: term()) ->
                   _)} |
          {connectfun,
           fun((User :: string(), Peer :: {inet:ip_address(), inet:port_number()}, Method :: string()) ->
                   _)} |
          {bannerfun, fun((User :: string()) -> binary())}.
```

- **`connectfun`** - Provides a fun to implement your own logging when a user
  authenticates to the server.

- **`failfun`** - Provides a fun to implement your own logging when a user fails
  to authenticate.

- **`bannerfun`** - Provides a fun to implement the construction of a banner
  text that is sent at the beginning of the user authentication. The banner will
  not be sent if the function does not return a binary.

# `daemon_option`

```erlang
-type daemon_option() ::
          subsystem_daemon_option() |
          shell_daemon_option() |
          exec_daemon_option() |
          ssh_cli_daemon_option() |
          tcpip_tunnel_out_daemon_option() |
          tcpip_tunnel_in_daemon_option() |
          authentication_daemon_options() |
          diffie_hellman_group_exchange_daemon_option() |
          max_initial_idle_time_daemon_option() |
          negotiation_timeout_daemon_option() |
          hello_timeout_daemon_option() |
          hardening_daemon_options() |
          callbacks_daemon_options() |
          send_ext_info_daemon_option() |
          gen_tcp:listen_option() |
          common_option() |
          experimental_daemon_options().
```

Options for [daemons](`daemon/1`). The individual options are further explained
below or by following the hyperlinks.

Note that not every `t:gen_tcp:listen_option/0` is accepted. See
`set_sock_opts/2` for a list of prohibited options.

Also note that setting a `t:gen_tcp:listen_option/0` could change the socket in
a way that impacts the ssh deamon's behaviour negatively. You use it on your own
risk.

# `daemon_options`

```erlang
-type daemon_options() :: [daemon_option()].
```

# `deprecated_exec_opt`

```erlang
-type deprecated_exec_opt() :: fun() | mod_fun_args().
```

Old-style exec specification that are kept for compatibility, but should not be
used in new programs

# `diffie_hellman_group_exchange_daemon_option`

```erlang
-type diffie_hellman_group_exchange_daemon_option() ::
          {dh_gex_groups, [explicit_group()] | explicit_group_file() | ssh_moduli_file()} |
          {dh_gex_limits, {Min :: pos_integer(), Max :: pos_integer()}}.
```

# `exec_daemon_option`

```erlang
-type exec_daemon_option() :: {exec, exec_spec()}.
```

# `exec_fun`

```erlang
-type exec_fun() :: 'exec_fun/1'() | 'exec_fun/2'() | 'exec_fun/3'().
```

The default is `disabled`.

Value `erlang_eval` enables evaluation of Erlang terms via exec requests.
This works when the shell option is either `disabled` (no shell) or
`{shell, start, []}` (Erlang shell). It does not work with custom shells.

To restore the behavior from OTP versions prior to OTP @OTP-19969@, configure:
```
ssh:daemon(Port, [{shell, {shell, start, []}},
                  {exec, erlang_eval}
                  | Options])
```

For new code, consider using `{direct, Fun}` for more controlled exec handling.

# `exec_fun/1`

```erlang
-type 'exec_fun/1'() :: fun((Cmd :: string()) -> exec_result()).
```

# `exec_fun/2`

```erlang
-type 'exec_fun/2'() :: fun((Cmd :: string(), User :: string()) -> exec_result()).
```

# `exec_fun/3`

```erlang
-type 'exec_fun/3'() ::
          fun((Cmd :: string(), User :: string(), ClientAddr :: ip_port()) -> exec_result()).
```

# `exec_result`

```erlang
-type exec_result() :: {ok, Result :: term()} | {error, Reason :: term()}.
```

This option changes how the daemon executes exec-requests from clients. The term
in the return value is formatted to a string if it is a non-string type. No
trailing newline is added in the ok-case.

See the User's Guide section on
[One-Time Execution](using_ssh.md#one-time-execution) for examples.

Error texts are returned on channel-type 1 which usually is piped to `stderr` on
e.g Linux systems. Texts from a successful execution are returned on
channel-type 0 and will in similar manner be piped to `stdout`. The exit-status
code is set to 0 for success and 255 for errors. The exact results presented on
the client side depends on the client and the client's operating system.

In case of the `{direct, exec_fun()}` variant or `erlang_eval`, all
reads from `standard_input` will be from the received data-events of type 0.
Those are sent by the client. Similarly all writes to `standard_output` will be
sent as data-events to the client. An OS shell client like the command 'ssh'
will usually use stdin and stdout for the user interface.

The option cooperates with the daemon-option
[`shell`](`t:shell_daemon_option/0`) in the following way:

- **1\. If neither the [`exec-option`](`t:exec_daemon_option/0`) nor the
  [`shell-option`](`t:shell_daemon_option/0`) is present:** - Both default to
  `disabled`. No exec-requests or shell-requests are executed. This is the
  default behavior since @OTP-19969@. To restore the previous
  behavior, set `{shell, {shell, start, []}}` and `{exec, erlang_eval}`.

- **2\. If the [`exec_spec`](`t:exec_daemon_option/0`)'s value is `disabled`
  (the [`shell-option`](`t:shell_daemon_option/0`) may or may not be
  present):** - No exec-requests are executed but shell-requests are not
  affected, they follow the [`shell_spec`](`t:shell_daemon_option/0`)'s value.

- **3\. If the [`exec-option`](`t:exec_daemon_option/0`) is present and the
  [`exec_spec`](`t:exec_daemon_option/0`) value =/= `disabled` (the
  [`shell-option`](`t:shell_daemon_option/0`) may or may not be present):** -
  The [`exec_spec`](`t:exec_daemon_option/0`) `fun()` is called with the same
  number of parameters as the arity of the fun, and the result is returned to
  the client. Shell-requests are not affected, they follow the
  [`shell_spec`](`t:shell_daemon_option/0`)'s value.

- **4\. If the [`exec_spec`](`t:exec_daemon_option/0`)'s value is
  `erlang_eval`, and the [`shell-option`](`t:shell_daemon_option/0`) is
  `disabled` or set to the default Erlang shell `{shell, start, []}`:** - The
  default Erlang evaluator is used for exec requests. The result is returned to
  the client. Shell-requests follow the
  [`shell_spec`](`t:shell_daemon_option/0`)'s value.

- **5\. If the [`exec_spec`](`t:exec_daemon_option/0`)'s value is
  `erlang_eval`, and the [`shell-option`](`t:shell_daemon_option/0`) is present
  with a value that is neither the default Erlang shell nor `disabled`:** - The
  exec-request is not evaluated and an error message is returned to the client.
  Shell-requests are executed according to the value of the
  [`shell_spec`](`t:shell_daemon_option/0`).

- **6\. If the [`exec-option`](`t:exec_daemon_option/0`) is absent (defaults to
  `disabled`), and the [`shell_spec`](`t:shell_daemon_option/0`)'s value is
  `disabled`:** - Neither exec-requests nor shell-requests are executed.

If a custom CLI is installed (see the option
[`ssh_cli`](`t:ssh_cli_daemon_option/0`)) the rules above are replaced by thoose
implied by the custom CLI.

> #### Note {: .info }
>
> The [`exec-option`](`t:exec_daemon_option/0`) has existed for a long time but
> has not previously been documented. The old definition and behaviour are
> retained but obey the rules 1-6 above if conflicting. The old and undocumented
> style should not be used in new programs.

# `exec_spec`

```erlang
-type exec_spec() :: {direct, exec_fun()} | disabled | deprecated_exec_opt() | erlang_eval.
```

# `explicit_group`

```erlang
-type explicit_group() :: {Size :: pos_integer(), G :: pos_integer(), P :: pos_integer()}.
```

# `explicit_group_file`

```erlang
-type explicit_group_file() :: {file, string()}.
```

# `hardening_daemon_options`

```erlang
-type hardening_daemon_options() ::
          {max_sessions, pos_integer()} |
          {max_channels, pos_integer()} |
          {parallel_login, boolean()} |
          {minimal_remote_max_packet_size, pos_integer()} |
          {max_auth_request_size, pos_integer()}.
```

For more information about hardening, see the [Hardening](hardening.md) section
in the User's Guide chapter.

- **`max_sessions`{: #hardening_daemon_options-max_sessions }** - The maximum
  number of simultaneous sessions that are accepted at any time for this daemon.
  This includes sessions that are being authorized. Thus, if set to `N`, and `N`
  clients have connected but not started the login process, connection attempt
  `N+1` is aborted. If `N` connections are authenticated and still logged in, no
  more logins are accepted until one of the existing ones log out.

  The counter is per listening port. Thus, if two daemons are started, one with
  `{max_sessions,N}` and the other with `{max_sessions,M}`, in total `N+M`
  connections are accepted for the whole `ssh` application.

  Notice that if `parallel_login` is `false`, only one client at a time can be
  in the authentication phase.

  By default, this option is not set. This means that the number is not limited.

- **`max_channels`{: #hardening_daemon_options-max_channels }** - The maximum
  number of channels with active remote subsystem that are accepted for each
  connection to this daemon

  By default, this option is not set. This means that the number is not limited.

- **`parallel_login`{: #hardening_daemon_options-parallel_login }** - If set to
  false (the default value), only one login is handled at a time. If set to
  true, an unlimited number of login attempts are allowed simultaneously.

  If the `max_sessions` option is set to `N` and `parallel_login` is set to
  `true`, the maximum number of simultaneous login attempts at any time is
  limited to `N-K`, where `K` is the number of authenticated connections present
  at this daemon.

  > #### Warning {: .warning }
  >
  > Do not enable `parallel_logins` without protecting the server by other
  > means, for example, by the `max_sessions` option or a firewall
  > configuration. If set to `true`, there is no protection against DOS attacks.

- **`minimal_remote_max_packet_size`{:
  #hardening_daemon_options-minimal_remote_max_packet_size }** - The least
  maximum packet size that the daemon will accept in channel open requests from
  the client. The default value is 0.

- **`max_auth_request_size`{:
  #hardening_daemon_options-max_auth_request_size }** - The maximum size allowed
  in bytes for the SSH_MSG_USERAUTH_REQUEST messages. The default value
  is the maximum allowed packet size, 262144 bytes,
  which is the same as no check being made,
  since maximum allowed packet size check is performed earlier.

# `hello_timeout_daemon_option`

```erlang
-type hello_timeout_daemon_option() :: {hello_timeout, timeout()}.
```

Maximum time in milliseconds for the first part of the ssh session setup, the
hello message exchange. Defaults to 30000 ms (30 seconds). If the client fails
to send the first message within this time, the connection is closed.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.

# `kb_int_fun_3`

```erlang
-type kb_int_fun_3() ::
          fun((Peer :: ip_port(), User :: string(), Service :: string()) -> kb_int_tuple()).
```

# `kb_int_fun_4`

```erlang
-type kb_int_fun_4() ::
          fun((Peer :: ip_port(), User :: string(), Service :: string(), State :: any()) ->
                  kb_int_tuple()).
```

# `kb_int_tuple`

```erlang
-type kb_int_tuple() ::
          {Name :: string(), Instruction :: string(), Prompt :: string(), Echo :: boolean()}.
```

# `max_initial_idle_time_daemon_option`

```erlang
-type max_initial_idle_time_daemon_option() :: {max_initial_idle_time, timeout()}.
```

Maximum time in milliseconds for the first channel start after completion of the
authentication negotiation. Defaults to `infinity`.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.

# `negotiation_timeout_daemon_option`

```erlang
-type negotiation_timeout_daemon_option() :: {negotiation_timeout, timeout()}.
```

Maximum time in milliseconds for the authentication negotiation. Defaults to
120000 ms (2 minutes). If the client fails to log in within this time, the
connection is closed.

For more information about timeouts, see the
[Timeouts section ](hardening.md#timeouts)in the User's Guide
[Hardening](hardening.md) chapter.

# `prompt_texts`

```erlang
-type prompt_texts() :: kb_int_tuple() | kb_int_fun_3() | kb_int_fun_4().
```

# `pwdfun_2`

```erlang
-type pwdfun_2() :: fun((User :: string(), Password :: string() | pubkey) -> boolean()).
```

# `pwdfun_4`

```erlang
-type pwdfun_4() ::
          fun((User :: string(),
               Password :: string() | pubkey,
               PeerAddress :: ip_port(),
               State :: any()) ->
                  boolean() | disconnect | {boolean(), NewState :: any()}).
```

- **`auth_method_kb_interactive_data`{: #option-auth_method_kb_interactive_data }** - Sets the text strings that the daemon
  sends to the client for presentation to the user when using
  `keyboard-interactive` authentication.

  If the fun/3 or fun/4 is used, it is called when the actual authentication
  occurs and may therefore return dynamic data like time, remote ip etc.

  The parameter `Echo` guides the client about need to hide the password.

  The default value is:
  `{auth_method_kb_interactive_data, {"SSH server", "Enter password for \""++User++"\"", "password: ", false}>`

- **`user_passwords`{: #option-user_passwords }** - Provides passwords for
  password authentication. The passwords are used when someone tries to connect
  to the server and public key user-authentication fails. The option provides a
  list of valid usernames and the corresponding passwords.

  > #### Warning {: .warning }
  >
  > Note that this is very insecure due to the plain-text passwords; it is
  > intended for test purposes. Use the [`pwdfun`](`m:ssh#option-pwdfun`) option
  > to handle the password checking instead.

- **`pk_check_user`{: #option-pk_check_user }** - Enables checking of the
  [client's user name](`t:authentication_client_options/0`) in the server when
  doing public key authentication. It is disabled by default.

  The term "user" is used differently in OpenSSH and SSH in Erlang/OTP: see more
  in the [User's Guide](terminology.md#the-term-user-in-openssh).

  If the option is enabled, and no [`pwdfun`](`m:ssh#option-pwdfun`) is present,
  the user name must present in the
  [user_passwords](`m:ssh#option-user_passwords`) for the check to succeed but
  the value of the password is not checked.

  In case of a [`pwdfun`](`m:ssh#option-pwdfun`) checking the user, the atom
  `pubkey` is put in the password argument.

- **`password`{: #option-password }** - Provides a global password that
  authenticates any user.

  > #### Warning {: .warning }
  >
  > Intended to facilitate testing.
  >
  > From a security perspective this option makes the server very vulnerable.

- **`pwdfun`{: #option-pwdfun } with `t:pwdfun_4/0`** - Provides a function for
  password validation. This could used for calling an external system or
  handling passwords stored as hash values.

  This fun can also be used to make delays in authentication tries for example
  by calling `timer:sleep/1`.

  To facilitate for instance counting of failed tries, the `State` variable
  could be used. This state is per connection only. The first time the pwdfun is
  called for a connection, the `State` variable has the value `undefined`.

  The fun should return:

  - `true` if the user and password is valid
  - `false` if the user or password is invalid
  - `disconnect` if a SSH_MSG_DISCONNECT message should be sent immediately. It
    will be followed by a close of the underlying tcp connection.
  - `{true, NewState:any()}` if the user and password is valid
  - `{false, NewState:any()}` if the user or password is invalid

  A third usage is to block login attempts from a missbehaving peer. The `State`
  described above can be used for this. The return value `disconnect` is useful
  for this.

  In case of the [`pk_check_user`](`m:ssh#option-pk_check_user`) is set, the
  atom `pubkey` is put in the password argument when validating a public key
  login. The pwdfun is then responsible to check that the user name is valid.

- **`pwdfun` with `t:pwdfun_2/0`** - Provides a function for password
  validation. This function is called with user and password as strings, and
  returns:

  - `true` if the user and password is valid
  - `false` if the user or password is invalid

  In case of the [`pk_check_user`](`m:ssh#option-pk_check_user`) is set, the
  atom `pubkey` is put in the password argument when validating a public key
  login. The pwdfun is then responsible to check that the user name is valid.

  This variant is kept for compatibility.

- **`no_auth_needed`{: #option-no_auth_needed }** - If `true`, a client is
  authenticated without any need of providing any password or key.

  This option is only intended for very special applications due to the high
  risk of accepting any connecting client.

  The default value is `false`.

# `send_ext_info_daemon_option`

```erlang
-type send_ext_info_daemon_option() :: {send_ext_info, boolean()}.
```

Make the server (daemon) tell the client that the server accepts extension
negotiation, that is, include `ext-info-s` in the kexinit message sent. See
[RFC 8308](https://tools.ietf.org/html/rfc8308) for details and
[ssh](ssh_app.md#supported-ext-info) for a list of currently implemented
extensions.

Default value is `true` which is compatible with other implementations not
supporting ext-info.

# `shell_daemon_option`

```erlang
-type shell_daemon_option() :: {shell, shell_spec()}.
```

# `shell_fun`

```erlang
-type shell_fun() :: 'shell_fun/1'() | 'shell_fun/2'().
```

The default is `disabled`.

To enable the Erlang shell:
```
ssh:daemon(Port, [{shell, {shell, start, []}} | Options])
```

# `shell_fun/1`

```erlang
-type 'shell_fun/1'() :: fun((User :: string()) -> pid()).
```

# `shell_fun/2`

```erlang
-type 'shell_fun/2'() :: fun((User :: string(), PeerAddr :: inet:ip_address()) -> pid()).
```

Defines the read-eval-print loop used in a daemon when a shell is requested by
the client.

See the option [`exec-option`](`t:exec_daemon_option/0`) for a description of
how the daemon executes shell-requests and exec-requests depending on the shell-
and exec-options.

# `shell_spec`

```erlang
-type shell_spec() :: mod_fun_args() | shell_fun() | disabled.
```

# `ssh_cli_daemon_option`

```erlang
-type ssh_cli_daemon_option() :: {ssh_cli, mod_args() | no_cli}.
```

Provides your own CLI implementation in a daemon.

It is a channel callback module that implements a shell and command execution.
The shell's read-eval-print loop can be customized, using the option
[`shell`](`t:shell_daemon_option/0`). This means less work than implementing an
own CLI channel. If `ssh_cli` is set to `no_cli`, the CLI channels like
[`shell`](`t:shell_daemon_option/0`) and [`exec`](`t:exec_daemon_option/0`) are
disabled and only subsystem channels are allowed.

# `ssh_moduli_file`

```erlang
-type ssh_moduli_file() :: {ssh_moduli_file, string()}.
```

- **`dh_gex_groups`** - Defines the groups the server may choose among when
  diffie-hellman-group-exchange is negotiated. See
  [RFC 4419](https://tools.ietf.org/html/rfc4419) for details. The three
  variants of this option are:

  - **`{Size=integer(),G=integer(),P=integer()}`** - The groups are given
    explicitly in this list. There may be several elements with the same `Size`.
    In such a case, the server will choose one randomly in the negotiated Size.

  - **`{file,filename()}`** - The file must have one or more three-tuples
    `{Size=integer(),G=integer(),P=integer()}` terminated by a dot. The file is
    read when the daemon starts.

  - **`{ssh_moduli_file,filename()}`** - The file must be in
    [ssh-keygen moduli file format](`public_key:dh_gex_group/4`). The file is
    read when the daemon starts.

  The default list is fetched from the [public_key](`public_key:dh_gex_group/4`)
  application.

- **`dh_gex_limits`** - Limits what a client can ask for in
  diffie-hellman-group-exchange. The limits will be
  `{MaxUsed = min(MaxClient,Max), MinUsed = max(MinClient,Min)}` where
  `MaxClient` and `MinClient` are the values proposed by a connecting client.

  The default value is `{0,infinity}`.

  If `MaxUsed < MinUsed` in a key exchange, it will fail with a disconnect.

  See [RFC 4419](https://tools.ietf.org/html/rfc4419) for the function of the
  Max and Min values.

# `subsystem_daemon_option`

```erlang
-type subsystem_daemon_option() :: {subsystems, subsystem_specs()}.
```

# `subsystem_spec`

```erlang
-type subsystem_spec() :: {Name :: string(), mod_args()}.
```

Defines a subsystem in the daemon.

The `subsystem_name` is the name that a client requests to start with for
example `ssh_connection:subsystem/4`.

The `channel_callback` is the module that implements the `m:ssh_server_channel`
(replaces ssh_daemon_channel) behaviour in the daemon. See the section
[Creating a Subsystem](using_ssh.md#usersguide_creating_a_subsystem) in the
User's Guide for more information and an example.

If the subsystems option is not present, the default is an empty list
and no subsystems are enabled.

To enable the SFTP subsystem:
```
ssh:daemon(Port, [{subsystems, [ssh_sftpd:subsystem_spec([])]} | Options])
```

# `subsystem_specs`

```erlang
-type subsystem_specs() :: [subsystem_spec()].
```

# `tcpip_tunnel_in_daemon_option`

```erlang
-type tcpip_tunnel_in_daemon_option() ::
          {tcpip_tunnel_in,
           boolean() |
           (Callback :: fun((HostName :: string(), inet:port_number()) -> boolean() | denied))}.
```

Enables (`true`) or disables (`false`) the possibility to tunnel a TCP/IP
connection in to a [server](`daemon/2`). Disabled per default.

Set `Callback` function to allow/deny/log tunnel connections.

# `tcpip_tunnel_out_daemon_option`

```erlang
-type tcpip_tunnel_out_daemon_option() :: {tcpip_tunnel_out, boolean()}.
```

Enables (`true`) or disables (`false`) the possibility to tunnel a TCP/IP
connection out of a [server](`daemon/2`). Disabled per default.

# `ssh_channel_id`

```erlang
-opaque ssh_channel_id()
```

# `ssh_connection_ref`

```erlang
-opaque ssh_connection_ref()
```

# `ssh_daemon_ref`

```erlang
-opaque ssh_daemon_ref()
```

# `legacy_cipher_alg`

```erlang
-type legacy_cipher_alg() :: 'aes128-cbc' | 'aes192-cbc' | 'aes256-cbc' | '3des-cbc'.
```

# `legacy_compression_alg`

> This type is deprecated. use 'none' or 'zlib@openssh.com' instead.

```erlang
-type legacy_compression_alg() :: zlib.
```

Deprecated: the use of `zlib` compression in SSH will be
removed in OTP 30.0. Use `none` or `zlib@openssh.com` instead.

# `legacy_kex_alg`

```erlang
-type legacy_kex_alg() ::
          'diffie-hellman-group1-sha1' | 'diffie-hellman-group14-sha1' |
          'diffie-hellman-group-exchange-sha1'.
```

# `legacy_mac_alg`

```erlang
-type legacy_mac_alg() :: 'hmac-sha1-96'.
```

# `legacy_pubkey_alg`

```erlang
-type legacy_pubkey_alg() :: 'ssh-rsa' | 'ssh-dss'.
```

# `channel_id`

```erlang
-opaque channel_id()
```

Opaque data type representing a channel inside a connection.

Returned by the functions
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

# `conn_info_algs`

```erlang
-type conn_info_algs() ::
          [{kex, kex_alg()} |
           {hkey, pubkey_alg()} |
           {encrypt, cipher_alg()} |
           {decrypt, cipher_alg()} |
           {send_mac, mac_alg()} |
           {recv_mac, mac_alg()} |
           {compress, compression_alg()} |
           {decompress, compression_alg()} |
           {send_ext_info, boolean()} |
           {recv_ext_info, boolean()}].
```

# `conn_info_channels`

```erlang
-type conn_info_channels() :: [proplists:proplist()].
```

# `connection_info_tuple`

```erlang
-type connection_info_tuple() ::
          {client_version, version()} |
          {server_version, version()} |
          {user, string()} |
          {peer, {inet:hostname(), ip_port()}} |
          {sockname, ip_port()} |
          {options, client_options()} |
          {algorithms, conn_info_algs()} |
          {channels, conn_info_channels()}.
```

Return values from the `connection_info/1` and `connection_info/2` functions.

> #### Note {: .info }
>
> Note that `options` info tuple contains only items with nondefault values.

# `connection_ref`

```erlang
-type connection_ref() :: pid().
```

Opaque data type representing a connection between a client and a server
(daemon).

Returned by the functions [`connect/2,3,4`](`connect/3`) and
[`ssh_sftp:start_channel/2,3`](`ssh_sftp:start_channel/2`).

# `daemon_info_tuple`

```erlang
-type daemon_info_tuple() ::
          {port, inet:port_number()} |
          {ip, inet:ip_address()} |
          {profile, atom()} |
          {options, daemon_options()}.
```

Return values from the `daemon_info/1` and `daemon_info/2` functions.

> #### Note {: .info }
>
> Note that `options` info tuple contains only items with nondefault values.

# `daemon_ref`

```erlang
-opaque daemon_ref()
```

Opaque data type representing a daemon.

Returned by the functions [`daemon/1,2,3`](`daemon/1`).

# `experimental_client_options`

```erlang
-type experimental_client_options() ::
          {keyboard_interact_fun,
           fun((Name :: iodata(),
                Instruction :: iodata(),
                Prompts :: [{Prompt :: iodata(), Echo :: boolean()}]) ->
                   [Response :: iodata()])} |
          experimental_common_options().
```

Experimental options that should not to be used in products.

# `experimental_common_options`

```erlang
-type experimental_common_options() ::
          {transport, {atom(), atom(), atom()}} |
          {vsn, {non_neg_integer(), non_neg_integer()}} |
          {tstflg, [term()]} |
          ssh_file:user_dir_fun_common_option() |
          {max_random_length_padding, non_neg_integer()}.
```

Experimental options that should not to be used in products.

# `experimental_daemon_options`

```erlang
-type experimental_daemon_options() :: {infofun, fun()} | experimental_common_options().
```

Experimental options that should not to be used in products.

# `host`

```erlang
-type host() :: string() | inet:ip_address() | loopback.
```

# `ip_port`

```erlang
-type ip_port() :: {inet:ip_address(), inet:port_number()}.
```

# `mod_args`

```erlang
-type mod_args() :: {Module :: atom(), Args :: list()}.
```

# `mod_fun_args`

```erlang
-type mod_fun_args() :: {Module :: atom(), Function :: atom(), Args :: list()}.
```

# `open_socket`

```erlang
-type open_socket() :: gen_tcp:socket().
```

The socket is supposed to be result of a [gen_tcp:connect](`gen_tcp:connect/3`)
or a [gen_tcp:accept](`gen_tcp:accept/1`). The socket must be in passive mode
(that is, opened with the option `{active,false})`.

# `protocol_version`

```erlang
-type protocol_version() :: {Major :: pos_integer(), Minor :: non_neg_integer()}.
```

# `role`

```erlang
-type role() :: client | server.
```

# `software_version`

```erlang
-type software_version() :: string().
```

# `version`

```erlang
-type version() :: {protocol_version(), software_version()}.
```

# `close`

```erlang
-spec close(ConnectionRef) -> ok | {error, term()} when ConnectionRef :: connection_ref().
```

Closes an SSH connection.

# `connect`
*since OTP 19.0* 

```erlang
-spec connect(OpenTcpSocket, Options) -> {ok, connection_ref()} | {error, term()}
                 when OpenTcpSocket :: open_socket(), Options :: client_options().
```

# `connect`

```erlang
-spec connect(open_socket(), client_options(), timeout()) -> {ok, connection_ref()} | {error, term()};
             (host(), inet:port_number(), client_options()) -> {ok, connection_ref()} | {error, term()}.
```

# `connect`

```erlang
-spec connect(Host, Port, Options, NegotiationTimeout) -> {ok, connection_ref()} | {error, term()}
                 when
                     Host :: host(),
                     Port :: inet:port_number(),
                     Options :: client_options(),
                     NegotiationTimeout :: timeout().
```

Connects to an SSH server at the `Host` on `Port`.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
with the SSH that should be at the other end.

No channel is started. This is done by calling
[ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

The `NegotiationTimeout` is in milli-seconds. The default value is `infinity` or
the value of the [`connect_timeout`](`t:connect_timeout_client_option/0`)
option, if present. For connection timeout, use the option
[`connect_timeout`](`t:connect_timeout_client_option/0`).

# `connection_info`
*since OTP 22.1* 

```erlang
-spec connection_info(ConnectionRef) -> InfoTupleList | {error, term()}
                         when
                             ConnectionRef :: connection_ref(),
                             InfoTupleList :: [InfoTuple],
                             InfoTuple :: connection_info_tuple().
```

# `connection_info`

```erlang
-spec connection_info(ConnectionRef, ItemList | Item) -> InfoTupleList | InfoTuple | {error, term()}
                         when
                             ConnectionRef :: connection_ref(),
                             ItemList :: [Item],
                             Item ::
                                 client_version | server_version | user | peer | sockname | options |
                                 algorithms | sockname,
                             InfoTupleList :: [InfoTuple],
                             InfoTuple :: connection_info_tuple().
```

Returns information about a connection intended for e.g debugging or logging.

When the `Key` is a single `Item`, the result is a single `InfoTuple`

# `daemon`

```erlang
-spec daemon(inet:port_number()) -> {ok, daemon_ref()} | {error, term()}.
```

# `daemon`

```erlang
-spec daemon(inet:port_number() | open_socket(), daemon_options()) ->
                {ok, daemon_ref()} | {error, term()}.
```

# `daemon`

```erlang
-spec daemon(any | inet:ip_address(), inet:port_number(), daemon_options()) ->
                {ok, daemon_ref()} | {error, term()};
            (socket, open_socket(), daemon_options()) -> {ok, daemon_ref()} | {error, term()}.
```

Starts a server listening for SSH connections on the given port. If the `Port`
is 0, a random free port is selected. See `daemon_info/1` about how to find the
selected port number.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
when an SSH starts at the other end of the TCP socket.

For a description of the options, see [Daemon Options](`t:daemon_options/0`).

Please note that by historical reasons both the `HostAddress` argument and the
[gen_tcp connect_option() `{ip,Address}`](`t:gen_tcp:connect_option/0`) set the
listening address. This is a source of possible inconsistent settings.

The rules for handling the two address passing options are:

- if `HostAddress` is an IP-address, that IP-address is the listening address.
  An 'ip'-option will be discarded if present.
- if `HostAddress` is the atom `loopback`, the listening address is `loopback`
  and an loopback address will be chosen by the underlying layers. An
  'ip'-option will be discarded if present.
- if `HostAddress` is the atom `any` and no 'ip'-option is present, the
  listening address is `any` and the socket will listen to all addresses
- if `HostAddress` is `any` and an 'ip'-option is present, the listening address
  is set to the value of the 'ip'-option

# `daemon_info`
*since OTP 19.0* 

```erlang
-spec daemon_info(DaemonRef) -> {ok, InfoTupleList} | {error, bad_daemon_ref}
                     when
                         DaemonRef :: daemon_ref(),
                         InfoTupleList :: [InfoTuple],
                         InfoTuple :: daemon_info_tuple().
```

# `daemon_info`
*since OTP 22.1* 

```erlang
-spec daemon_info(DaemonRef, ItemList | Item) -> InfoTupleList | InfoTuple | {error, bad_daemon_ref}
                     when
                         DaemonRef :: daemon_ref(),
                         ItemList :: [Item],
                         Item :: ip | port | profile | options,
                         InfoTupleList :: [InfoTuple],
                         InfoTuple :: daemon_info_tuple().
```

Returns information about a daemon intended for e.g debugging or logging.

When the `Key` is a single `Item`, the result is a single `InfoTuple`

Note that [`daemon_info/1`](`daemon_info/1`) and
[`daemon_info/2`](`daemon_info/2`) returns different types due to compatibility
reasons.

# `daemon_replace_options`
*since OTP 25.1* 

```erlang
-spec daemon_replace_options(DaemonRef, NewUserOptions) -> {ok, daemon_ref()} | {error, term()}
                                when DaemonRef :: daemon_ref(), NewUserOptions :: daemon_options().
```

Replaces the options in a running daemon with the options in `NewUserOptions`.
Only connections established after this call are affected, already established
connections are not.

> #### Note {: .info }
>
> In the final phase of this function, the listening process is restarted.
> Therfore a connection attempt to the daemon in this final phase could fail.

The handling of Erlang configurations is described in the User's Guide; see
chapters [Configuration in SSH](configurations.md) and
[Configuring algorithms in SSH](configure_algos.md).

# `default_algorithms`
*since OTP 18.0* 

```erlang
-spec default_algorithms() -> algs_list().
```

Returns a key-value list, where the keys are the different types of algorithms
and the values are the algorithms themselves.

See the [User's Guide](configure_algos.md#example_default_algorithms) for an
example.

# `get_sock_opts`
*since OTP 22.3* 

```erlang
-spec get_sock_opts(ConnectionRef, SocketGetOptions) -> ok | {error, inet:posix()}
                       when
                           ConnectionRef :: connection_ref(),
                           SocketGetOptions :: [gen_tcp:option_name()].
```

Get tcp socket option values of the tcp-socket below an ssh connection.

This function calls the `inet:getopts/2`, read that documentation.

# `hostkey_fingerprint`
*since OTP 24.0* 

```erlang
-spec hostkey_fingerprint(public_key:public_key()) -> string().
```

# `hostkey_fingerprint`
*since OTP 24.0* 

```erlang
-spec hostkey_fingerprint(TypeOrTypes, Key) -> StringOrString
                             when
                                 TypeOrTypes :: public_key:digest_type() | [public_key:digest_type()],
                                 Key :: public_key:public_key(),
                                 StringOrString :: string() | [string()].
```

hostkey_fingerprint([DigestType], HostKey) ->
[string()]hostkey_fingerprint(DigestType, HostKey) -> string()

Calculates a ssh fingerprint from a public host key as openssh does.

The algorithm in [`hostkey_fingerprint/1`](`hostkey_fingerprint/1`) is md5 to be
compatible with older ssh-keygen commands. The string from the second variant is
prepended by the algorithm name in uppercase as in newer ssh-keygen commands.

Examples:

```erlang
 2> ssh:hostkey_fingerprint(Key).
 "f5:64:a6:c1:5a:cb:9f:0a:10:46:a2:5c:3e:2f:57:84"

 3> ssh:hostkey_fingerprint(md5,Key).
 "MD5:f5:64:a6:c1:5a:cb:9f:0a:10:46:a2:5c:3e:2f:57:84"

 4> ssh:hostkey_fingerprint(sha,Key).
 "SHA1:bSLY/C4QXLDL/Iwmhyg0PGW9UbY"

 5> ssh:hostkey_fingerprint(sha256,Key).
 "SHA256:aZGXhabfbf4oxglxltItWeHU7ub3Dc31NcNw2cMJePQ"

 6> ssh:hostkey_fingerprint([sha,sha256],Key).
 ["SHA1:bSLY/C4QXLDL/Iwmhyg0PGW9UbY",
  "SHA256:aZGXhabfbf4oxglxltItWeHU7ub3Dc31NcNw2cMJePQ"]
```

# `set_sock_opts`
*since OTP 22.3* 

```erlang
-spec set_sock_opts(ConnectionRef, SocketOptions) -> ok | {error, inet:posix()}
                       when ConnectionRef :: connection_ref(), SocketOptions :: [gen_tcp:option()].
```

Sets tcp socket options on the tcp-socket below an ssh connection.

This function calls the `inet:setopts/2`, read that documentation and for
`t:gen_tcp:option/0`.

All gen_tcp socket options except

- `active`
- `deliver`
- `mode` and
- `packet`

are allowed. The excluded options are reserved by the SSH application.

> #### Warning {: .warning }
>
> This is an extremely dangerous function. You use it on your own risk.
>
> Some options are OS and OS version dependent. Do not use it unless you know
> what effect your option values will have on an TCP stream.
>
> Some values may destroy the functionality of the SSH protocol.

# `shell`

```erlang
-spec shell(open_socket() | host() | connection_ref()) -> _.
```

# `shell`

```erlang
-spec shell(open_socket() | host(), client_options()) -> _.
```

# `shell`

```erlang
-spec shell(Host, Port, Options) -> _
               when Host :: host(), Port :: inet:port_number(), Options :: client_options().
```

Connects to an SSH server at `Host` and `Port` (defaults to 22) and starts an
interactive shell on that remote host.

As an alternative, an already open TCP socket could be passed to the function in
`TcpSocket`. The SSH initiation and negotiation will be initiated on that one
and finally a shell will be started on the host at the other end of the TCP
socket.

For a description of the options, see [Client Options](`t:client_options/0`).

The function waits for user input, and does not return until the remote shell is
ended (that is, exit from the shell).

# `start`

```erlang
-spec start() -> ok | {error, term()}.
```

# `start`

```erlang
-spec start(Type) -> ok | {error, term()} when Type :: permanent | transient | temporary.
```

Utility function that starts the applications `crypto`, `public_key`, and `ssh`.
Default type is `temporary`. For more information, see the `m:application`
manual page in Kernel.

# `stop`

```erlang
-spec stop() -> ok | {error, term()}.
```

Stops the `ssh` application. For more information, see the `m:application`
manual page in Kernel.

# `stop_daemon`

```erlang
-spec stop_daemon(DaemonRef :: daemon_ref()) -> ok.
```

# `stop_daemon`

```erlang
-spec stop_daemon(inet:ip_address(), inet:port_number()) -> ok.
```

# `stop_daemon`
*since OTP 21.0* 

```erlang
-spec stop_daemon(any | inet:ip_address(), inet:port_number(), atom()) -> ok.
```

Stops the listener and all connections started by the listener.

If the daemon process does not exist, the call exits the calling process
with reason `noproc`.

# `stop_listener`

```erlang
-spec stop_listener(daemon_ref()) -> ok.
```

# `stop_listener`

```erlang
-spec stop_listener(inet:ip_address(), inet:port_number()) -> ok.
```

# `stop_listener`
*since OTP 21.0* 

```erlang
-spec stop_listener(any | inet:ip_address(), inet:port_number(), term()) -> ok.
```

Stops the listener, but leaves existing connections started by the listener
operational.

# `tcpip_tunnel_from_server`
*since OTP 23.0* 

```erlang
-spec tcpip_tunnel_from_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort) ->
                                  {ok, TrueListenPort} | {error, term()}
                                  when
                                      ConnectionRef :: connection_ref(),
                                      ListenHost :: host(),
                                      ListenPort :: inet:port_number(),
                                      ConnectToHost :: host(),
                                      ConnectToPort :: inet:port_number(),
                                      TrueListenPort :: inet:port_number().
```

# `tcpip_tunnel_from_server`
*since OTP 23.0* 

```erlang
-spec tcpip_tunnel_from_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort,
                               Timeout) ->
                                  {ok, TrueListenPort} | {error, term()}
                                  when
                                      ConnectionRef :: connection_ref(),
                                      ListenHost :: host(),
                                      ListenPort :: inet:port_number(),
                                      ConnectToHost :: host(),
                                      ConnectToPort :: inet:port_number(),
                                      Timeout :: timeout(),
                                      TrueListenPort :: inet:port_number().
```

Asks the remote server of `ConnectionRef` to listen to `ListenHost:ListenPort`.
When someone connects that address, the connection is forwarded in an encrypted
channel from the server to the client. The client (that is, at the node that
calls this function) then connects to `ConnectToHost:ConnectToPort`.

The returned `TrueListenPort` is the port that is listened to. It is the same as
`ListenPort`, except when `ListenPort = 0`. In that case a free port is selected
by the underlying OS.

Note that in case of an Erlang/OTP SSH server (daemon) as peer, that server must
have been started with the option
[tcpip_tunnel_out](`t:tcpip_tunnel_out_daemon_option/0`) to allow the
connection.

# `tcpip_tunnel_to_server`
*since OTP 23.0* 

```erlang
-spec tcpip_tunnel_to_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort) ->
                                {ok, TrueListenPort} | {error, term()}
                                when
                                    ConnectionRef :: connection_ref(),
                                    ListenHost :: host(),
                                    ListenPort :: inet:port_number(),
                                    ConnectToHost :: host(),
                                    ConnectToPort :: inet:port_number(),
                                    TrueListenPort :: inet:port_number().
```

# `tcpip_tunnel_to_server`
*since OTP 23.0* 

```erlang
-spec tcpip_tunnel_to_server(ConnectionRef, ListenHost, ListenPort, ConnectToHost, ConnectToPort,
                             Timeout) ->
                                {ok, TrueListenPort} | {error, term()}
                                when
                                    ConnectionRef :: connection_ref(),
                                    ListenHost :: host(),
                                    ListenPort :: inet:port_number(),
                                    ConnectToHost :: host(),
                                    ConnectToPort :: inet:port_number(),
                                    Timeout :: timeout(),
                                    TrueListenPort :: inet:port_number().
```

Tells the local client to listen to `ListenHost:ListenPort`. When someone
connects to that address, the connection is forwarded in an encrypted channel to
the peer server of `ConnectionRef`. That server then connects to
`ConnectToHost:ConnectToPort`.

The returned `TrueListenPort` is the port that is listened to. It is the same as
`ListenPort`, except when `ListenPort = 0`. In that case a free port is selected
by the underlying OS.

Note that in case of an Erlang/OTP SSH server (daemon) as peer, that server must
have been started with the option
[tcpip_tunnel_in](`t:tcpip_tunnel_in_daemon_option/0`) to allow the connection.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
