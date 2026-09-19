# `ct_ftp`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_ftp.erl#L23)

FTP client module (based on the `ftp` application).

# `connection`

```erlang
-type connection() :: handle() | ct:target_name().
```

Reference to opened FTP connection associated to either a `handle` or `target_name`.

# `handle`

```erlang
-type handle() :: ct:handle().
```

Handle for a specific FTP connection, see module `m:ct`.

# `cd`

> This function is deprecated. ct_ftp:cd/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec cd(Connection, Dir) -> ok | {error, Reason}
            when Connection :: connection(), Dir :: file:filename(), Reason :: term().
```

Changes directory on remote host.

# `close`

> This function is deprecated. ct_ftp:close/1 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec close(Connection) -> ok | {error, Reason} when Connection :: connection(), Reason :: term().
```

Closes the FTP connection.

# `delete`

> This function is deprecated. ct_ftp:delete/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec delete(Connection, File) -> ok | {error, Reason}
                when Connection :: connection(), File :: file:filename(), Reason :: term().
```

Deletes a file on remote host.

# `get`

> This function is deprecated. ct_ftp:get/3 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec get(KeyOrName, RemoteFile, LocalFile) -> ok | {error, Reason}
             when
                 KeyOrName :: ct:key_or_name(),
                 RemoteFile :: file:filename(),
                 LocalFile :: file:filename(),
                 Reason :: term().
```

Opens an FTP connection and fetches a file from the remote host.

`RemoteFile` and `LocalFile` must be absolute paths.

The configuration file must be as for [`ct_ftp:put/3`](`put/3`).

See also `ct:require/2`.

# `ls`

> This function is deprecated. ct_ftp:ls/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec ls(Connection, Dir) -> {ok, Listing} | {error, Reason}
            when
                Connection :: connection(),
                Dir :: file:filename(),
                Listing :: string(),
                Reason :: term().
```

Lists directory `Dir`.

# `open`

> This function is deprecated. ct_ftp:open/1 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec open(KeyOrName) -> {ok, Handle} | {error, Reason}
              when KeyOrName :: ct:key_or_name(), Handle :: handle(), Reason :: term().
```

Opens an FTP connection to the specified node.

You can open a connection for a particular `Name` and use the same name as
reference for all following subsequent operations. If you want the connection to
be associated with `Handle` instead (if you, for example, need to open multiple
connections to a host), use `Key`, the configuration variable name, to specify
the target. A connection without an associated target name can only be closed
with the handle value.

For information on how to create a new `Name`, see `ct:require/2`.

# `put`

> This function is deprecated. ct_ftp:put/3 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec put(KeyOrName, LocalFile, RemoteFile) -> ok | {error, Reason}
             when
                 KeyOrName :: ct:key_or_name(),
                 LocalFile :: file:filename(),
                 RemoteFile :: file:filename(),
                 Reason :: term().
```

Opens an FTP connection and sends a file to the remote host.

`LocalFile` and `RemoteFile` must be absolute paths.

If the target host is a "special" node, the FTP address must be specified in the
configuration file as follows:

```erlang
{node,[{ftp,IpAddr}]}.
```

If the target host is something else, for example, a UNIX host, the
configuration file must also include the username and password (both strings):

```erlang
{unix,[{ftp,IpAddr},
       {username,Username},
       {password,Password}]}.
```

See also `ct:require/2`.

# `recv`

> This function is deprecated. ct_ftp:recv/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec recv(Connection, RemoteFile) -> ok | {error, Reason}
              when Connection :: connection(), RemoteFile :: file:filename(), Reason :: term().
```

Fetches a file over FTP.

The file gets the same name on the local host.

See also [`ct_ftp:recv/3`](`recv/3`).

# `recv`

> This function is deprecated. ct_ftp:recv/3 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec recv(Connection, RemoteFile, LocalFile) -> ok | {error, Reason}
              when
                  Connection :: connection(),
                  RemoteFile :: file:filename(),
                  LocalFile :: file:filename(),
                  Reason :: term().
```

Fetches a file over FTP.

The file is named `LocalFile` on the local host.

# `send`

> This function is deprecated. ct_ftp:send/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec send(Connection, LocalFile) -> ok | {error, Reason}
              when Connection :: connection(), LocalFile :: file:filename(), Reason :: term().
```

Sends a file over FTP.

The file gets the same name on the remote host.

See also [`ct_ftp:send/3`](`send/3`).

# `send`

> This function is deprecated. ct_ftp:send/3 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec send(Connection, LocalFile, RemoteFile) -> ok | {error, Reason}
              when
                  Connection :: connection(),
                  LocalFile :: file:filename(),
                  RemoteFile :: file:filename(),
                  Reason :: term().
```

Sends a file over FTP.

The file is named `RemoteFile` on the remote host.

# `type`

> This function is deprecated. ct_ftp:type/2 is deprecated; Legacy protocol support will be dropped in OTP-30.

```erlang
-spec type(Connection, Type) -> ok | {error, Reason}
              when Connection :: connection(), Type :: ascii | binary, Reason :: term().
```

Changes the file transfer type.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
