# `ssh_sftp`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/ssh/src/ssh_sftp.erl#L27)

SFTP client.

This module implements an SSH FTP (SFTP) client. SFTP is a secure, encrypted
file transfer service available for SSH.

# `chunk_size`

```erlang
-type chunk_size() :: undefined | pos_integer().
```

# `crypto_fun`

```erlang
-type crypto_fun() :: fun((TextIn :: binary(), crypto_state()) -> crypto_result()).
```

# `crypto_result`

```erlang
-type crypto_result() ::
          {ok, TextOut :: binary(), crypto_state()} |
          {ok, TextOut :: binary(), crypto_state(), chunk_size()}.
```

The initial `t:crypto_state/0` returned from the `t:init_fun/0` is folded into
repeated applications of the `t:crypto_fun/0` in the
[tar_crypto_spec](`t:tar_crypto_spec/0`). The binary returned from that fun is
sent to the remote SFTP server and the new `t:crypto_state/0` is used in the
next call of the `t:crypto_fun/0`.

If the `t:crypto_fun/0` reurns a `t:chunk_size/0`, that value is as block size
for further blocks in calls to `t:crypto_fun/0`.

# `crypto_state`

```erlang
-type crypto_state() :: any().
```

The `t:init_fun/0` in the [tar_crypto_spec](`t:tar_crypto_spec/0`) is applied
once prior to any other `crypto` operation. The intention is that this function
initiates the encryption or decryption for example by calling
`crypto:crypto_init/4` or similar. The `t:crypto_state/0` is the state such a
function may return.

If the selected cipher needs to have the input data partitioned into blocks of a
certain size, the `t:init_fun/0` should return the second form of return value
with the `t:chunk_size/0` set to the block size. If the `t:chunk_size/0` is
`undefined`, the size of the `PlainBin`s varies, because this is intended for
stream crypto, whereas a fixed `t:chunk_size/0` is intended for block crypto. A
`t:chunk_size/0` can be changed in the return from the `t:crypto_fun/0`. The
value can be changed between `t:pos_integer/0` and `undefined`.

# `decrypt_spec`

```erlang
-type decrypt_spec() :: {init_fun(), crypto_fun()}.
```

Specifies the encryption or decryption applied to tar files when using
`open_tar/3` or `open_tar/4`.

The encryption or decryption is applied to the generated stream of bytes prior
to sending the resulting stream to the SFTP server.

For code examples see Section
[Example with encryption](using_ssh.md#example-with-encryption) in the ssh Users
Guide.

# `encrypt_spec`

```erlang
-type encrypt_spec() :: {init_fun(), crypto_fun(), final_fun()}.
```

# `final_fun`

```erlang
-type final_fun() :: fun((FinalTextIn :: binary(), crypto_state()) -> {ok, FinalTextOut :: binary()}).
```

If doing encryption, the `t:final_fun/0` in the
[tar_crypto_spec](`t:tar_crypto_spec/0`) is applied to the last piece of data.
The `t:final_fun/0` is responsible for padding (if needed) and encryption of
that last piece.

# `init_fun`

```erlang
-type init_fun() :: fun(() -> {ok, crypto_state()}) | fun(() -> {ok, crypto_state(), chunk_size()}).
```

# `tar_crypto_spec`

```erlang
-type tar_crypto_spec() :: encrypt_spec() | decrypt_spec().
```

# `reason`

```erlang
-type reason() :: atom() | string() | tuple().
```

A description of the reason why an operation failed.

The `t:atom/0` value is formed from the sftp error codes in the protocol-level
responses as defined in
[draft-ietf-secsh-filexfer-13](https://tools.ietf.org/html/draft-ietf-secsh-filexfer-13#page-49)
section 9.1. The codes are named as `SSH_FX_*` which are transformed into
lowercase of the star-part. E.g. the error code `SSH_FX_NO_SUCH_FILE` will cause
the `t:reason/0` to be `no_such_file`.

The `t:string/0` reason is the error information from the server in case of an
exit-signal. If that information is empty, the reason is the exit signal name.

The `t:tuple/0` reason are other errors like for example `{exit_status,1}`.

# `sftp_option`

```erlang
-type sftp_option() ::
          {timeout, timeout()} |
          {sftp_vsn, pos_integer()} |
          {window_size, pos_integer()} |
          {packet_size, pos_integer()}.
```

Specifies available SFTP options.

# `apread`

```erlang
-spec apread(ChannelPid, Handle, Position, Len) -> {async, N} | Error
                when
                    ChannelPid :: pid(),
                    Handle :: term(),
                    Position :: integer(),
                    Len :: integer(),
                    Error :: {error, reason()},
                    N :: term().
```

The [`apread/4`](`apread/4`) function reads from a specified position, combining
the `position/3` and `aread/3` functions.

# `apwrite`

```erlang
-spec apwrite(ChannelPid, Handle, Position, Data) -> {async, N} | Error
                 when
                     ChannelPid :: pid(),
                     Handle :: term(),
                     Position :: integer(),
                     Data :: binary(),
                     Error :: {error, reason()},
                     N :: term().
```

The [`apwrite/4`](`apwrite/4`) function writes to a specified position,
combining the `position/3` and `awrite/3` functions.

# `aread`

```erlang
-spec aread(ChannelPid, Handle, Len) -> {async, N} | Error
               when
                   ChannelPid :: pid(),
                   Handle :: term(),
                   Len :: integer(),
                   Error :: {error, reason()},
                   N :: term().
```

Reads from an open file, without waiting for the result. If the handle is valid,
the function returns `{async, N}`, where `N` is a term guaranteed to be unique
between calls of `aread`. The actual data is sent as a message to the calling
process. This message has the form `{async_reply, N, Result}`, where `Result` is
the result from the read, either `{ok, Data}`, `eof`, or `{error, reason()}`.

# `awrite`

```erlang
-spec awrite(ChannelPid, Handle, Data) -> {async, N} | Error
                when
                    ChannelPid :: pid(),
                    Handle :: term(),
                    Data :: binary(),
                    Error :: {error, reason()},
                    N :: term().
```

Writes to an open file, without waiting for the result. If the handle is valid,
the function returns `{async, N}`, where `N` is a term guaranteed to be unique
between calls of `awrite`. The result of the `write` operation is sent as a
message to the calling process. This message has the form
`{async_reply, N, Result}`, where `Result` is the result from the write, either
`ok`, or `{error, reason()}`.

# `close`

```erlang
-spec close(ChannelPid, Handle) -> ok | Error
               when ChannelPid :: pid(), Handle :: term(), Error :: {error, reason()}.
```

# `close`

```erlang
-spec close(ChannelPid, Handle, Timeout) -> ok | Error
               when
                   ChannelPid :: pid(),
                   Handle :: term(),
                   Timeout :: timeout(),
                   Error :: {error, reason()}.
```

Closes a handle to an open file or directory on the server.

# `del_dir`

```erlang
-spec del_dir(ChannelPid, Name) -> ok | Error
                 when ChannelPid :: pid(), Name :: string(), Error :: {error, reason()}.
```

# `del_dir`

```erlang
-spec del_dir(ChannelPid, Name, Timeout) -> ok | Error
                 when
                     ChannelPid :: pid(),
                     Name :: string(),
                     Timeout :: timeout(),
                     Error :: {error, reason()}.
```

Deletes a directory specified by `Name`. The directory must be empty before it
can be successfully deleted.

# `delete`

```erlang
-spec delete(ChannelPid, Name) -> ok | Error
                when ChannelPid :: pid(), Name :: string(), Error :: {error, reason()}.
```

# `delete`

```erlang
-spec delete(ChannelPid, Name, Timeout) -> ok | Error
                when
                    ChannelPid :: pid(),
                    Name :: string(),
                    Timeout :: timeout(),
                    Error :: {error, reason()}.
```

Deletes the file specified by `Name`.

# `list_dir`

```erlang
-spec list_dir(ChannelPid, Path) -> {ok, FileNames} | Error
                  when
                      ChannelPid :: pid(),
                      Path :: string(),
                      FileNames :: [FileName],
                      FileName :: string(),
                      Error :: {error, reason()}.
```

# `list_dir`

```erlang
-spec list_dir(ChannelPid, Path, Timeout) -> {ok, FileNames} | Error
                  when
                      ChannelPid :: pid(),
                      Path :: string(),
                      Timeout :: timeout(),
                      FileNames :: [FileName],
                      FileName :: string(),
                      Error :: {error, reason()}.
```

Lists the given directory on the server, returning the filenames as a list of
strings.

# `make_dir`

```erlang
-spec make_dir(ChannelPid, Name) -> ok | Error
                  when ChannelPid :: pid(), Name :: string(), Error :: {error, reason()}.
```

# `make_dir`

```erlang
-spec make_dir(ChannelPid, Name, Timeout) -> ok | Error
                  when
                      ChannelPid :: pid(),
                      Name :: string(),
                      Timeout :: timeout(),
                      Error :: {error, reason()}.
```

Creates a directory specified by `Name`. `Name` must be a full path to a new
directory. The directory can only be created in an existing directory.

# `make_symlink`

```erlang
-spec make_symlink(ChannelPid, Name, Target) -> ok | Error
                      when
                          ChannelPid :: pid(),
                          Name :: string(),
                          Target :: string(),
                          Error :: {error, reason()}.
```

# `make_symlink`

```erlang
-spec make_symlink(ChannelPid, Name, Target, Timeout) -> ok | Error
                      when
                          ChannelPid :: pid(),
                          Name :: string(),
                          Target :: string(),
                          Timeout :: timeout(),
                          Error :: {error, reason()}.
```

Creates a symbolic link pointing to `Target` with the name `Name`.

# `open`

```erlang
-spec open(ChannelPid, Name, Mode) -> {ok, Handle} | Error
              when
                  ChannelPid :: pid(),
                  Name :: string(),
                  Mode :: [read | write | append | binary | raw],
                  Handle :: term(),
                  Error :: {error, reason()}.
```

# `open`

```erlang
-spec open(ChannelPid, Name, Mode, Timeout) -> {ok, Handle} | Error
              when
                  ChannelPid :: pid(),
                  Name :: string(),
                  Mode :: [read | write | append | binary | raw],
                  Timeout :: timeout(),
                  Handle :: term(),
                  Error :: {error, reason()}.
```

Opens a file on the server and returns a handle, which can be used for reading
or writing.

# `open_tar`
*since OTP 17.4* 

```erlang
-spec open_tar(ChannelPid, Path, Mode) -> {ok, Handle} | Error
                  when
                      ChannelPid :: pid(),
                      Path :: string(),
                      Mode :: [read | write | {crypto, tar_crypto_spec()}],
                      Handle :: term(),
                      Error :: {error, reason()}.
```

# `open_tar`
*since OTP 17.4* 

```erlang
-spec open_tar(ChannelPid, Path, Mode, Timeout) -> {ok, Handle} | Error
                  when
                      ChannelPid :: pid(),
                      Path :: string(),
                      Mode :: [read | write | {crypto, tar_crypto_spec()}],
                      Timeout :: timeout(),
                      Handle :: term(),
                      Error :: {error, reason()}.
```

Opens a handle to a tar file on the server, associated with `ChannelPid`. The
handle can be used for remote tar creation and extraction. The actual writing
and reading is performed by calls to [erl_tar:add/3,4](`erl_tar:add/3`) and
`erl_tar:extract/2`. Note: The `erl_tar:init/3` function should not be called,
that one is called by this open_tar function.

For code examples see Section
[SFTP Client with TAR Compression](using_ssh.md#sftp-client-with-tar-compression)
in the ssh Users Guide.

The `crypto` mode option is explained in the data types section above, see
[Crypto operations for open_tar](`m:ssh_sftp#types-crypto-open_tar`).
Encryption is assumed if the `Mode` contains `write`, and decryption if the
`Mode` contains `read`.

# `opendir`

```erlang
-spec opendir(ChannelPid, Path) -> {ok, Handle} | Error
                 when
                     ChannelPid :: pid(), Path :: string(), Handle :: term(), Error :: {error, reason()}.
```

# `opendir`

```erlang
-spec opendir(ChannelPid, Path, Timeout) -> {ok, Handle} | Error
                 when
                     ChannelPid :: pid(),
                     Path :: string(),
                     Timeout :: timeout(),
                     Handle :: term(),
                     Error :: {error, reason()}.
```

Opens a handle to a directory on the server. The handle can be used for reading
directory contents.

# `position`

```erlang
-spec position(ChannelPid, Handle, Location) -> {ok, NewPosition} | Error
                  when
                      ChannelPid :: pid(),
                      Handle :: term(),
                      Location ::
                          Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
                      Offset :: integer(),
                      NewPosition :: integer(),
                      Error :: {error, reason()}.
```

# `position`

```erlang
-spec position(ChannelPid, Handle, Location, Timeout) -> {ok, NewPosition} | Error
                  when
                      ChannelPid :: pid(),
                      Handle :: term(),
                      Location ::
                          Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
                      Timeout :: timeout(),
                      Offset :: integer(),
                      NewPosition :: integer(),
                      Error :: {error, reason()}.
```

Sets the file position of the file referenced by `Handle`. Returns
`{ok, NewPosition}` (as an absolute offset) if successful, otherwise
`{error, reason()}`. `Location` is one of the following:

- **`Offset`** - The same as `{bof, Offset}`.

- **`{bof, Offset}`** - Absolute offset.

- **`{cur, Offset}`** - Offset from the current position.

- **`{eof, Offset}`** - Offset from the end of file.

- **`bof | cur | eof`** - The same as eariler with `Offset` 0, that is, `{bof, 0} | {cur, 0} | {eof, 0}`.

# `pread`

```erlang
-spec pread(ChannelPid, Handle, Position, Len) -> {ok, Data} | eof | Error
               when
                   ChannelPid :: pid(),
                   Handle :: term(),
                   Position :: integer(),
                   Len :: integer(),
                   Data :: string() | binary(),
                   Error :: {error, reason()}.
```

# `pread`

```erlang
-spec pread(ChannelPid, Handle, Position, Len, Timeout) -> {ok, Data} | eof | Error
               when
                   ChannelPid :: pid(),
                   Handle :: term(),
                   Position :: integer(),
                   Len :: integer(),
                   Timeout :: timeout(),
                   Data :: string() | binary(),
                   Error :: {error, reason()}.
```

The `pread/3,4` function reads from a specified position, combining the
`position/3` and [`read/3,4`](`read/3`) functions.

# `pwrite`

```erlang
-spec pwrite(ChannelPid, Handle, Position, Data) -> ok | Error
                when
                    ChannelPid :: pid(),
                    Handle :: term(),
                    Position :: integer(),
                    Data :: iolist(),
                    Error :: {error, reason()}.
```

# `pwrite`

```erlang
-spec pwrite(ChannelPid, Handle, Position, Data, Timeout) -> ok | Error
                when
                    ChannelPid :: pid(),
                    Handle :: term(),
                    Position :: integer(),
                    Data :: iolist(),
                    Timeout :: timeout(),
                    Error :: {error, reason()}.
```

The `pwrite/3,4` function writes to a specified position, combining the
`position/3` and [`write/3,4`](`write/3`) functions.

# `read`

```erlang
-spec read(ChannelPid, Handle, Len) -> {ok, Data} | eof | Error
              when
                  ChannelPid :: pid(),
                  Handle :: term(),
                  Len :: integer(),
                  Data :: string() | binary(),
                  Error :: {error, reason()}.
```

# `read`

```erlang
-spec read(ChannelPid, Handle, Len, Timeout) -> {ok, Data} | eof | Error
              when
                  ChannelPid :: pid(),
                  Handle :: term(),
                  Len :: integer(),
                  Timeout :: timeout(),
                  Data :: string() | binary(),
                  Error :: {error, reason()}.
```

Reads `Len` bytes from the file referenced by `Handle`. Returns `{ok, Data}`,
`eof`, or `{error, reason()}`. If the file is opened with `binary`, `Data` is a
binary, otherwise it is a string.

If the file is read past `eof`, only the remaining bytes are read and returned.
If no bytes are read, `eof` is returned.

# `read_file`

```erlang
-spec read_file(ChannelPid, File) -> {ok, Data} | Error
                   when
                       ChannelPid :: pid(),
                       File :: string(),
                       Data :: binary(),
                       Error :: {error, reason()}.
```

# `read_file`

```erlang
-spec read_file(ChannelPid, File, Timeout) -> {ok, Data} | Error
                   when
                       ChannelPid :: pid(),
                       File :: string(),
                       Data :: binary(),
                       Timeout :: timeout(),
                       Error :: {error, reason()}.
```

Reads a file from the server, and returns the data in a binary.

# `read_file_info`

```erlang
-spec read_file_info(ChannelPid, Name) -> {ok, FileInfo} | Error
                        when
                            ChannelPid :: pid(),
                            Name :: string(),
                            FileInfo :: file:file_info(),
                            Error :: {error, reason()}.
```

# `read_file_info`

```erlang
-spec read_file_info(ChannelPid, Name, Timeout) -> {ok, FileInfo} | Error
                        when
                            ChannelPid :: pid(),
                            Name :: string(),
                            Timeout :: timeout(),
                            FileInfo :: file:file_info(),
                            Error :: {error, reason()}.
```

Returns a `file_info` record from the file system object specified by `Name` or
`Handle`. See `file:read_file_info/2` for information about the record.

Depending on the underlying OS:es links might be followed and info on the final
file, directory etc is returned. See `read_link_info/2` on how to get
information on links instead.

# `read_link`

```erlang
-spec read_link(ChannelPid, Name) -> {ok, Target} | Error
                   when
                       ChannelPid :: pid(),
                       Name :: string(),
                       Target :: string(),
                       Error :: {error, reason()}.
```

# `read_link`

```erlang
-spec read_link(ChannelPid, Name, Timeout) -> {ok, Target} | Error
                   when
                       ChannelPid :: pid(),
                       Name :: string(),
                       Target :: string(),
                       Timeout :: timeout(),
                       Error :: {error, reason()}.
```

Reads the link target from the symbolic link specified by `name`.

# `read_link_info`

```erlang
-spec read_link_info(ChannelPid, Name) -> {ok, FileInfo} | Error
                        when
                            ChannelPid :: pid(),
                            Name :: string(),
                            FileInfo :: file:file_info(),
                            Error :: {error, reason()}.
```

# `read_link_info`

```erlang
-spec read_link_info(ChannelPid, Name, Timeout) -> {ok, FileInfo} | Error
                        when
                            ChannelPid :: pid(),
                            Name :: string(),
                            FileInfo :: file:file_info(),
                            Timeout :: timeout(),
                            Error :: {error, reason()}.
```

Returns a `file_info` record from the symbolic link specified by `Name` or
`Handle`. See `file:read_link_info/2` for information about the record.

# `rename`

```erlang
-spec rename(ChannelPid, OldName, NewName) -> ok | Error
                when
                    ChannelPid :: pid(),
                    OldName :: string(),
                    NewName :: string(),
                    Error :: {error, reason()}.
```

# `rename`

```erlang
-spec rename(ChannelPid, OldName, NewName, Timeout) -> ok | Error
                when
                    ChannelPid :: pid(),
                    OldName :: string(),
                    NewName :: string(),
                    Timeout :: timeout(),
                    Error :: {error, reason()}.
```

Renames a file named `OldName` and gives it the name `NewName`.

# `start_channel`

```erlang
-spec start_channel(ssh:open_socket() | ssh:connection_ref() | ssh:host()) ->
                       {ok, pid()} | {ok, pid(), ssh:connection_ref()} | {error, reason()}.
```

# `start_channel`

```erlang
-spec start_channel(ssh:open_socket(), [ssh:client_option() | sftp_option()]) ->
                       {ok, pid(), ssh:connection_ref()} | {error, reason()};
                   (ssh:connection_ref(), [ssh:client_option() | sftp_option()]) ->
                       {ok, pid()} | {ok, pid(), ssh:connection_ref()} | {error, reason()};
                   (ssh:host(), [ssh:client_option() | sftp_option()]) ->
                       {ok, pid(), ssh:connection_ref()} | {error, reason()}.
```

Starts new ssh channel for communicating with the SFTP server.

Starts an ssh channel when first argument is a connection reference.

Equivalent to [start_channel(Host, 22, UserOptions)](`start_channel/3`) when
first argument is recognized as network host.

Otherwise, first argument is treated as a network socket which will be used for
establishing new SSH connection. New connection reference will be used for
starting an SSH channel.

The returned `pid` for this process is to be used as input to all other API
functions in this module.

See also (`start_channel/3`).

# `start_channel`

```erlang
-spec start_channel(ssh:host(), inet:port_number(), [ssh:client_option() | sftp_option()]) ->
                       {ok, pid(), ssh:connection_ref()} | {error, reason()}.
```

Starts new ssh connection and channel for communicating with the SFTP server.

The returned `pid` for this process is to be
used as input to all other API functions in this module.

Options:

- **`{timeout, timeout()}`** - There are two ways to set a timeout for the
  underlying ssh connection:

  - If the connection timeout option `connect_timeout` is set, that value is
    used also for the negotiation timeout and this option (`timeout`) is
    ignored.
  - Otherwise, this option (`timeout`) is used as the negotiation timeout only
    and there is no connection timeout set

  The value defaults to `infinity`.

- **`{sftp_vsn, integer()}`** - Desired SFTP protocol version. The actual
  version is the minimum of the desired version and the maximum supported
  versions by the SFTP server.

All other options are directly passed to [ssh:connect/3](`m:ssh`) or ignored if
a connection is already provided.

# `stop_channel`

```erlang
-spec stop_channel(ChannelPid) -> ok when ChannelPid :: pid().
```

Stops an SFTP channel. Does not close the SSH connection. Use `ssh:close/1` to
close it.

# `write`

```erlang
-spec write(ChannelPid, Handle, Data) -> ok | Error
               when ChannelPid :: pid(), Handle :: term(), Data :: iodata(), Error :: {error, reason()}.
```

# `write`

```erlang
-spec write(ChannelPid, Handle, Data, Timeout) -> ok | Error
               when
                   ChannelPid :: pid(),
                   Handle :: term(),
                   Data :: iodata(),
                   Timeout :: timeout(),
                   Error :: {error, reason()}.
```

Writes `data` to the file referenced by `Handle`. The file is to be opened with
`write` or `append` flag. Returns `ok` if successful or `{error, reason()}`
otherwise.

# `write_file`

```erlang
-spec write_file(ChannelPid, File, Data) -> ok | Error
                    when
                        ChannelPid :: pid(),
                        File :: string(),
                        Data :: iodata(),
                        Error :: {error, reason()}.
```

# `write_file`

```erlang
-spec write_file(ChannelPid, File, Data, Timeout) -> ok | Error
                    when
                        ChannelPid :: pid(),
                        File :: string(),
                        Data :: iodata(),
                        Timeout :: timeout(),
                        Error :: {error, reason()}.
```

Writes a file to the server. The file is created if it does not exist but
overwritten if it exists.

# `write_file_info`

```erlang
-spec write_file_info(ChannelPid, Name, FileInfo) -> ok | Error
                         when
                             ChannelPid :: pid(),
                             Name :: string(),
                             FileInfo :: file:file_info(),
                             Error :: {error, reason()}.
```

# `write_file_info`

```erlang
-spec write_file_info(ChannelPid, Name, FileInfo, Timeout) -> ok | Error
                         when
                             ChannelPid :: pid(),
                             Name :: string(),
                             FileInfo :: file:file_info(),
                             Timeout :: timeout(),
                             Error :: {error, reason()}.
```

Writes file information from a `file_info` record to the file specified by
`Name`. See [file:write_file_info/2,3](`file:write_file_info/2`) for
information about the record.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
