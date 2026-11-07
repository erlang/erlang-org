# `ftp`
[🔗](https://github.com/erlang/otp/blob/master/lib/ftp/src/ftp.erl#L24)

A File Transfer Protocol client.

This module implements a client for file transfer according to a subset of the
File Transfer Protocol (FTP), see [RFC 959](http://www.ietf.org/rfc/rfc959.txt).

The FTP client always tries to use passive FTP mode and only resort to active
FTP mode if this fails. This default behavior can be changed by start option
[mode](`m:ftp#mode`).

For a simple example of an FTP session, see [FTP User's Guide](ftp_client.md).

The return values of the following functions depend much on the implementation
of the FTP server at the remote host. In particular, the results from `ls` and
`nlist` varies. Often real errors are not reported as errors by `ls`, even if,
for example, a file or directory does not exist. `nlist` is usually more strict,
but some implementations have the peculiar behaviour of responding with an error
if the request is a listing of the contents of a directory that exists but is
empty.

## Errors
The possible error reasons and the corresponding diagnostic strings returned by
[`formaterror/1`](`formaterror/1`) are as follows:

- **`echunk`** - Synchronization error during chunk sending according to one of
  the following:

  - A call is made to [`send_chunk/2`](`send_chunk/2`) or
    [`send_chunk_end/1`](`send_chunk_end/1`) before a call to
    [`send_chunk_start/2`](`send_chunk_start/2`).
  - A call has been made to another transfer function during chunk sending, that
    is, before a call to [`send_chunk_end/1`](`send_chunk_end/1`).

- **`eclosed`** - The session is closed.

- **`econn`** - Connection to the remote server is prematurely closed.

- **`ehost`** - Host is not found, FTP server is not found, or connection is
  rejected by FTP server.

- **`elogin`** - User is not logged in.

- **`enotbinary`** - Term is not a binary.

- **`epath`** - No such file or directory, or directory already exists, or
  permission denied.

- **`etype`** - No such type.

- **`euser`** - Invalid username or password.

- **`etnospc`** - Insufficient storage space in system \[452].

- **`epnospc`** - Exceeded storage allocation (for current directory or dataset)
  \[552].

- **`efnamena`** - Filename not allowed \[553].

# `client`

```erlang
-type client() :: pid().
```

# `append_chunk`

> This function is deprecated. ftp:append_chunk/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append_chunk(Client :: client(), Bin :: binary()) -> ok | {error, Reason :: term()}.
```

Transfers the chunk `Bin` to the remote server, which appends it to the file
specified in the call to [`append_chunk_start/2`](`append_chunk_start/2`).

For some errors, for example, file system full, it is necessary to call
`append_chunk_end` to get the proper reason.

# `append_chunk_end`

> This function is deprecated. ftp:append_chunk_end/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append_chunk_end(Client :: client()) -> ok | {error, Reason :: term()}.
```

Stops transfer of chunks for appending to the remote server. The file at the
remote server, specified in the call to
[`append_chunk_start/2`](`append_chunk_start/2`), is closed by the server.

# `append_chunk_start`

> This function is deprecated. ftp:append_chunk_start/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append_chunk_start(Client :: client(), RemoteFile :: string()) -> ok | {error, Reason :: term()}.
```

Starts the transfer of chunks for appending to the file `RemoteFile` at the
remote server. If the file does not exist, it is created.

# `recv_chunk`

> This function is deprecated. ftp:recv_chunk/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec recv_chunk(Client :: client()) -> ok | {ok, Bin :: binary()} | {error, Reason :: term()}.
```

Receives a chunk of the remote file (`RemoteFile` of `recv_chunk_start`). The
return values have the following meaning:

- `ok` = the transfer is complete.
- `{ok, Bin}` = just another chunk of the file.
- `{error, Reason}` = transfer failed.

# `recv_chunk_start`

> This function is deprecated. ftp:recv_chunk_start/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec recv_chunk_start(Pid :: pid(), RemoteFile :: string()) -> ok | {error, Reason :: term()}.
```

Starts transfer of the file `RemoteFile` from the remote server.

# `send_chunk`

> This function is deprecated. ftp:send_chunk/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send_chunk(Client :: client(), Bin :: binary()) -> ok | {error, Reason :: term()}.
```

Transfers the chunk `Bin` to the remote server, which writes it into the file
specified in the call to [`send_chunk_start/2`](`send_chunk_start/2`).

For some errors, for example, file system full, it is necessary to to call
`send_chunk_end` to get the proper reason.

# `send_chunk_end`

> This function is deprecated. ftp:send_chunk_end/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send_chunk_end(Client :: client()) -> ok | {error, Reason :: term()}.
```

Stops transfer of chunks to the remote server. The file at the remote server,
specified in the call to [`send_chunk_start/2`](`send_chunk_start/2`) is closed
by the server.

# `send_chunk_start`

> This function is deprecated. ftp:send_chunk_start/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send_chunk_start(Client :: client(), RemoteFile :: string()) -> ok | {error, Reason :: term()}.
```

Starts transfer of chunks into the file `RemoteFile` at the remote server.

# `account`

> This function is deprecated. ftp:account/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec account(Client :: client(), Acc :: string()) -> ok | {error, Reason :: term()}.
```

Sets the account for an operation, if needed.

# `close`

> This function is deprecated. ftp:close/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec close(Client :: client()) -> ok.
```

Ends an FTP session, created using function [open](`open/2`).

# `open`

> This function is deprecated. ftp:open/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec open(Host :: inet:hostname() | inet:ip_address()) ->
              {ok, Client :: client()} | {error, Reason :: term()}.
```

# `open`

> This function is deprecated. ftp:open/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec open(Host :: string() | inet:ip_address(), Opts) ->
              {ok, Client :: client()} | {error, Reason :: term()}
              when
                  Opts :: [Opt],
                  Opt :: StartOption | OpenOption,
                  StartOption :: {verbose, Verbose} | {debug, Debug},
                  Verbose :: boolean(),
                  Debug :: disable | debug | trace,
                  OpenOption ::
                      {ipfamily, IpFamily} |
                      {port, Port :: port()} |
                      {mode, Mode} |
                      {tls, TLSOptions :: [ssl:tls_option()]} |
                      {tls_sec_method, TLSSecMethod :: ftps | ftpes} |
                      {tls_ctrl_session_reuse, TLSSessionReuse :: boolean()} |
                      {timeout, Timeout :: timeout()} |
                      {dtimeout, DTimeout :: timeout()} |
                      {progress, Progress} |
                      {sock_ctrl, SocketCtrls} |
                      {sock_data_act, [SocketControl]} |
                      {sock_data_pass, [SocketControl]},
                  SocketCtrls :: [SocketControl],
                  IpFamily :: inet | inet6 | inet6fb4,
                  Mode :: active | passive,
                  Module :: atom(),
                  Function :: atom(),
                  InitialData :: term(),
                  Progress :: ignore | {Module, Function, InitialData},
                  SocketControl :: gen_tcp:option().
```

Starts a FTP client process and opens a session with the FTP server at `Host`.

A session opened in this way is closed using function `close/1`.

The available configuration options are as follows:

- **\{host, Host\}** - [](){: #host } Host = `string() | ip_address()`

- **\{port, Port\}** - [](){: #port } Default is `0` which aliases to `21` or
  `990` when used with [`{tls_sec_method,ftps}`](`open/2`)).

- **\{mode, Mode\}** - [](){: #mode } Default is `passive`.

- **\{verbose, Verbose\}** - [](){: #verbose } Determines if the FTP
  communication is to be verbose or not.

  Default is `false`.

- **\{debug, Debug\}** - [](){: #debug } Debugging using the dbg toolkit.

  Default is `disable`.

- **\{ipfamily, IpFamily\}** - [](){: #ipfamily } With `inet6fb4` the client
  behaves as before, that is, tries to use IPv6, and only if that does not work
  it uses IPv4).

  Default is `inet` (IPv4).

- **\{timeout, Timeout\}** - [](){: #timeout } Connection time-out.

  Default is `60000` (milliseconds).

- **\{dtimeout, DTimeout\}** - [](){: #dtimeout } Data connect time-out. The
  time the client waits for the server to connect to the data socket.

  Default is `infinity`.

- **\{tls, TLSOptions\}** - [](){: #tls_options } The FTP session is transported
  over `tls` (`ftps`, see [RFC 4217](http://www.ietf.org/rfc/rfc4217.txt)). The
  list `TLSOptions` can be empty. The function `ssl:connect/3` is used for
  securing both the control connection and the data sessions.

- **\{tls_sec_method, TLSSecMethod\}** - [](){: #tls_sec_method } When set to
  `ftps` will connect immediately with SSL instead of upgrading with STARTTLS.
  This suboption is ignored unless the suboption `tls` is also set.

  Default is `ftpes`

- **\{tls_ctrl_session_reuse, boolean()\}** - [](){: #tls_ctrl_session_reuse }
  When set to `true` the client will re-use the TLS session from the control
  channel on the data channel as enforced by many FTP servers as
  ([proposed and implemented first by vsftpd](https://scarybeastsecurity.blogspot.com/2009/02/vsftpd-210-released.html)).

  Default is `false`.

- **\{sock_ctrl, SocketCtrls :: \[SocketControl :: gen_tcp:option()]\}** -
  Passes options from `SocketCtrls` down to the underlying transport layer
  (tcp).

  `t:gen_tcp:option/0` except for `ipv6_v6only`, `active`, `packet`, `mode`,
  `packet_size` and `header`.

  Default value is `SocketCtrls = []`.

- **\{sock_data_act, \[SocketControl]\}** - Passes options from
  `[SocketControl]` down to the underlying transport layer (tcp).

  `sock_data_act` uses the value of `sock_ctrl` as default value.

- **\{sock_data_pass, \[SocketControl]\}** - Passes options from
  `[SocketControl]` down to the underlying transport layer (tcp).

  `sock_data_pass` uses the value of `sock_ctrl` as default value.

- **\{progress, Progress\}** - [](){: #progress } Progress =
  `ignore | {Module, Function, InitialData}`

  `Module = atom()`, `Function = atom()`

  `InitialData = term()`

  Default is `ignore`.

  Option `progress` is intended to be used by applications that want to create
  some type of progress report, such as a progress bar in a GUI. Default for the
  progress option is `ignore`, that is, the option is not used. When the
  progress option is specified, the following happens when `ftp:send/[3,4]` or
  `ftp:recv/[3,4]` are called:

  - Before a file is transferred, the following call is made to indicate the
    start of the file transfer and how large the file is. The return value of
    the callback function is to be a new value for the `UserProgressTerm` that
    will be used as input the next time the callback function is called.

    `Module:Function(InitialData, File, {file_size, FileSize})`

  - Every time a chunk of bytes is transferred the following call is made:

    `Module:Function(UserProgressTerm, File, {transfer_size, TransferSize})`

  - At the end of the file the following call is made to indicate the end of the
    transfer:

    `Module:Function(UserProgressTerm, File, {transfer_size, 0})`

  The callback function is to be defined as follows:

  `Module:Function(UserProgressTerm, File, Size) -> UserProgressTerm`

  `UserProgressTerm = term()`

  `File = string()`

  `Size = {transfer_size, integer()} | {file_size, integer()} | {file_size, unknown}`

  For remote files, `ftp` cannot determine the file size in a platform
  independent way. In this case the size becomes `unknown` and it is left to the
  application to determine the size.

  > #### Note {: .info }
  >
  > The callback is made by a middleman process, hence the file transfer is not
  > affected by the code in the progress callback function. If the callback
  > crashes, this is detected by the FTP connection process, which then prints
  > an info-report and goes on as if the progress option was set to `ignore`.

  The file transfer type is set to the default of the FTP server when the
  session is opened. This is usually ASCII mode.

  The current local working directory (compare [`lpwd/1`](`lpwd/1`)) is set to
  the value reported by `file:get_cwd/1`, the wanted local directory.

  The return value `Pid` is used as a reference to the newly created FTP client
  in all other functions, and they are to be called by the process that created
  the connection. The FTP client process monitors the process that created it
  and terminates if that process terminates.

# `user`

> This function is deprecated. ftp:user/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec user(Pid :: pid(), User :: string(), Pass :: string()) -> ok | {error, Reason :: term()}.
```

Performs login of `User` with `Pass`.

# `user`

> This function is deprecated. ftp:user/4 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec user(Pid :: pid(), User :: string(), Pass :: string(), Account :: string()) ->
              ok | {error, Reason :: term()}.
```

Performs login of `User` with `Pass` to the account specified by `Account`.

# `recv`

> This function is deprecated. ftp:recv/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec recv(Client :: client(), RemoteFileName :: file:filename()) -> ok | {error, Reason :: term()}.
```

# `recv`

> This function is deprecated. ftp:recv/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec recv(Pid :: pid(), RemoteFileName :: file:filename(), LocalFileName :: file:filename()) ->
              ok | {error, Reason :: term()}.
```

Transfers the file `RemoteFileName` from the remote server to the file system of
the local client. If `LocalFileName` is specified, the local file will be
`LocalFileName`, otherwise `RemoteFileName`.

If the file write fails, the command is aborted and `{error, term()}` is
returned. However, the file is _not_ removed.

# `recv_bin`

> This function is deprecated. ftp:recv_bin/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec recv_bin(Pid :: pid(), RemoteFile :: string()) ->
                  {ok, Bin :: binary()} | {error, Reason :: term()}.
```

Transfers the file `RemoteFile` from the remote server and receives it as a
binary.

# `send`

> This function is deprecated. ftp:send/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send(Client :: client(), LocalFileName :: file:filename()) -> ok | {error, Reason :: term()}.
```

# `send`

> This function is deprecated. ftp:send/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send(Pid :: pid(), LocalFileName :: file:filename(), RemoteFileName :: file:filename()) ->
              ok | {error, Reason :: term()}.
```

Transfers the file `LocalFileName` to the remote server. If `RemoteFileName` is
specified, the name of the remote file is set to `RemoteFileName`, otherwise to
`LocalFileName`.

# `send_bin`

> This function is deprecated. ftp:send_bin/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec send_bin(Client :: client(), Bin :: binary(), RemoteFile :: string()) ->
                  ok | {error, Reason :: term()}.
```

Transfers the binary `Bin` into the file `RemoteFile` at the remote server.

# `formaterror`

> This function is deprecated. ftp:formaterror/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec formaterror(Tag :: atom() | {error, atom()}) -> string().
```

Given an error return value `{error, AtomReason}`, this function returns a
readable string describing the error.

# `lpwd`

> This function is deprecated. ftp:lpwd/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec lpwd(Client :: client()) -> {ok, Dir :: string()}.
```

Returns the current working directory at the local client.

# `ls`

> This function is deprecated. ftp:ls/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec ls(Client :: client()) -> {ok, Listing :: string()} | {error, Reason :: term()}.
```

# `ls`

> This function is deprecated. ftp:ls/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec ls(Client :: client(), Dir :: string()) -> {ok, Listing :: string()} | {error, Reason :: term()}.
```

Returns a list of files in long format.

`Dir` can be a directory or a file. The `Dir` string can contain wildcards.

[`ls/1`](`ls/1`) implies the current remote directory of the user.

The format of `Listing` depends on the operating system. On UNIX, it is
typically produced from the output of the `ls -l` shell command.

# `nlist`

> This function is deprecated. ftp:nlist/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec nlist(Client :: client()) -> {ok, Listing :: string()} | {error, Reason :: term()}.
```

# `nlist`

> This function is deprecated. ftp:nlist/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec nlist(Client :: client(), Pathname :: string()) ->
               {ok, Listing :: string()} | {error, Reason :: term()}.
```

Returns a list of files in short format.

`Pathname` can be a directory or a file. The `Pathname` string can contain
wildcards.

[`nlist/1`](`nlist/1`) implies the current remote directory of the user.

The format of `Listing` is a stream of filenames where each filename is
separated by <CRLF> or <NL>. Contrary to function `ls`, the purpose of `nlist`
is to enable a program to process filename information automatically.

# `pwd`

> This function is deprecated. ftp:pwd/1 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec pwd(Client :: client()) -> {ok, Dir :: string()} | {error, Reason :: term()}.
```

Returns the current working directory at the remote server.

# `append`

> This function is deprecated. ftp:append/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append(Client :: client(), LocalFileName :: file:filename()) -> ok | {error, Reason :: term()}.
```

# `append`

> This function is deprecated. ftp:append/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append(Pid :: pid(), LocalFileName :: file:filename(), RemoteFileName :: file:filename()) ->
                ok | {error, Reason :: term()}.
```

Transfers the file `LocalFile` to the remote server. If `RemoteFile` is
specified, the name of the remote file that the file is appended to is set to
`RemoteFile`, otherwise to `LocalFile`. If the file does not exists, it is
created.

# `append_bin`

> This function is deprecated. ftp:append_bin/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec append_bin(Pid :: pid(), Bin :: binary(), RemoteFile :: string()) ->
                    ok | {error, Reason :: term()}.
```

Transfers the binary `Bin` to the remote server and appends it to the file
`RemoteFile`. If the file does not exist, it is created.

# `cd`

> This function is deprecated. ftp:cd/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec cd(Client :: client(), Dir :: string()) -> ok | {error, Reason :: term()}.
```

Changes the working directory at the remote server to `Dir`.

# `delete`

> This function is deprecated. ftp:delete/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec delete(Client :: client(), File :: string()) -> ok | {error, Reason :: term()}.
```

Deletes the file `File` at the remote server.

# `lcd`

> This function is deprecated. ftp:lcd/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec lcd(Client :: client(), Dir :: string()) -> ok | {error, Reason :: term()}.
```

Changes the working directory to `Dir` for the local client.

# `mkdir`

> This function is deprecated. ftp:mkdir/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec mkdir(Client :: client(), Dir :: string()) -> ok | {error, Reason :: term()}.
```

Creates the directory `Dir` at the remote server.

# `rename`

> This function is deprecated. ftp:rename/3 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec rename(Client :: client(), Old :: string(), New :: string()) -> ok | {error, Reason :: term()}.
```

Renames `Old` to `New` at the remote server.

# `rmdir`

> This function is deprecated. ftp:rmdir/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec rmdir(Client :: client(), Dir :: string()) -> ok | {error, Reason :: term()}.
```

Removes directory `Dir` at the remote server.

# `type`

> This function is deprecated. ftp:type/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec type(Client :: client(), Type :: ascii | binary) -> ok | {error, Reason :: term()}.
```

Sets the file transfer type to `ascii` or `binary`. When an FTP session is
opened, the default transfer type of the server is used, most often `ascii`,
which is default according to [RFC 959](http://www.ietf.org/rfc/rfc959.txt).

# `quote`

> This function is deprecated. ftp:quote/2 is deprecated; Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol)..

```erlang
-spec quote(Client :: client(), Cmd :: string()) -> [FTPLine :: string()].
```

> #### Note {: .info }
>
> The telnet end of line characters, from the FTP protocol definition, CRLF, for
> example, "\\\\r\\\\n" has been removed.

Sends an arbitrary FTP command and returns verbatim a list of the lines sent
back by the FTP server. This function is intended to give application accesses
to FTP commands that are server-specific or that cannot be provided by this FTP
client.

> #### Note {: .info }
>
> FTP commands requiring a data connection cannot be successfully issued with
> this function.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
