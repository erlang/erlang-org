# `ct_ssh`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/common_test/src/ct_ssh.erl#L23)

SSH/SFTP client module.

This module uses application `SSH`, which provides detailed information about,
for example, functions, types, and options.

Argument `Server` in the SFTP functions is only to be used for SFTP sessions
that have been started on existing SSH connections (that is, when the original
connection type is `ssh`). Whenever the connection type is `sftp`, use the SSH
connection reference only.

The following options are valid for specifying an SSH/SFTP connection (that is,
can be used as configuration elements):

```erlang
[{ConnType, Addr},
 {port, Port},
 {user, UserName}
 {password, Pwd}
 {user_dir, String}
 {public_key_alg, PubKeyAlg}
 {connect_timeout, Timeout}
 {key_cb, KeyCallbackMod}]
```

`ConnType = ssh | sftp`.

For other types, see `m:ssh`.

All time-out parameters in `ct_ssh` functions are values in milliseconds.

# `connection`
*not exported* 

```elixir
-type connection() :: handle() | ct:target_name().
```

Reference to opened SSH/SFTP connection associated to either a `handle` or `target_name`.

# `connection_type`
*not exported* 

```elixir
-type connection_type() :: host | ssh | sftp.
```

Connection type used for connect.

# `handle`
*not exported* 

```elixir
-type handle() :: pid().
```

Handle for a specific SSH/SFTP connection, see module `m:ct`.

# `ssh_channel_id`
*not exported* 

```elixir
-type ssh_channel_id() :: non_neg_integer().
```

Data type representing a channel inside a connection.

"For `ssh_channel_id`, see module `m:ssh`.".

# `ssh_data_type_code`
*not exported* 

```elixir
-type ssh_data_type_code() :: non_neg_integer().
```

The valid values are `0` ("normal") and `1` ("stderr"), see
[RFC 4254, Section 5.2](https://tools.ietf.org/html/rfc4254#page-8).

# `apread`

```elixir
-spec apread(SSH, Handle, Position, Length) -> Result
                when
                    SSH :: connection(),
                    Handle :: term(),
                    Position :: integer(),
                    Length :: integer(),
                    Result :: {async, N} | {error, Reason},
                    N :: term(),
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `apread`

```elixir
-spec apread(SSH, Server, Handle, Position, Length) -> Result
                when
                    SSH :: connection(),
                    Server :: pid(),
                    Handle :: term(),
                    Position :: integer(),
                    Length :: integer(),
                    Result :: {async, N} | {error, Reason},
                    N :: term(),
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `apwrite`

```elixir
-spec apwrite(SSH, Handle, Position, Data) -> Result
                 when
                     SSH :: connection(),
                     Handle :: term(),
                     Position :: integer(),
                     Data :: binary(),
                     Result :: {async, N} | {error, Reason},
                     N :: term(),
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `apwrite`

```elixir
-spec apwrite(SSH, Server, Handle, Position, Data) -> Result
                 when
                     SSH :: connection(),
                     Server :: pid(),
                     Handle :: term(),
                     Position :: integer(),
                     Data :: binary(),
                     Result :: {async, N} | {error, Reason},
                     N :: term(),
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `aread`

```elixir
-spec aread(SSH, Handle, Len) -> Result
               when
                   SSH :: connection(),
                   Handle :: term(),
                   Len :: integer(),
                   Result :: {async, N} | {error, Reason},
                   N :: term(),
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `aread`

```elixir
-spec aread(SSH, Server, Handle, Len) -> Result
               when
                   SSH :: connection(),
                   Server :: pid(),
                   Handle :: term(),
                   Len :: integer(),
                   Result :: {async, N} | {error, Reason},
                   N :: term(),
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `awrite`

```elixir
-spec awrite(SSH, Handle, Data) -> Result
                when
                    SSH :: connection(),
                    Handle :: term(),
                    Data :: binary(),
                    Result :: {async, N} | {error, Reason},
                    N :: term(),
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `awrite`

```elixir
-spec awrite(SSH, Server, Handle, Data) -> Result
                when
                    SSH :: connection(),
                    Server :: pid(),
                    Handle :: term(),
                    Data :: binary(),
                    Result :: {async, N} | {error, Reason},
                    N :: term(),
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `close`

```elixir
-spec close(SSH, Handle) -> Result
               when
                   SSH :: connection(),
                   Handle :: term(),
                   Result :: ok | {error, Reason},
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `close`

```elixir
-spec close(SSH, Server, Handle) -> Result
               when
                   SSH :: connection(),
                   Server :: pid(),
                   Handle :: term(),
                   Result :: ok | {error, Reason},
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `connect`

```elixir
-spec connect(KeyOrName) -> {ok, Handle} | {error, Reason}
                 when KeyOrName :: ct:key_or_name(), Handle :: handle(), Reason :: term().
```

# `connect`

```elixir
-spec connect(KeyOrName, ConnType) -> {ok, Handle} | {error, Reason}
                 when
                     KeyOrName :: ct:key_or_name(),
                     ConnType :: connection_type(),
                     Handle :: handle(),
                     Reason :: term();
             (KeyOrName, ExtraOpts) -> {ok, Handle} | {error, Reason}
                 when
                     KeyOrName :: ct:key_or_name(),
                     ExtraOpts :: [ExtraOption],
                     ExtraOption ::
                         {ssh, Address} | {sftp, Address} | ssh:client_option() | ssh_sftp:sftp_option(),
                     Address :: ssh:host(),
                     Handle :: handle(),
                     Reason :: term().
```

Opens an SSH or SFTP connection using the information associated with `KeyOrName`
(see `connect/3`).

Equivalent to [`connect(KeyOrName, ConnType, [])`](`connect/3`) if
called with ConnType being atom.

Equivalent to [`connect(KeyOrName, host, ExtraOpts)`](`connect/3`) if
called with ExtraOpts being list.

# `connect`

```elixir
-spec connect(KeyOrName, ConnType, ExtraOpts) -> {ok, Handle} | {error, Reason}
                 when
                     KeyOrName :: ct:key_or_name(),
                     ConnType :: connection_type(),
                     ExtraOpts :: [ExtraOption],
                     ExtraOption ::
                         {ssh, Address} | {sftp, Address} | ssh:client_option() | ssh_sftp:sftp_option(),
                     Address :: ssh:host(),
                     Handle :: handle(),
                     Reason :: term().
```

Opens an SSH or SFTP connection using the information associated with
`KeyOrName`.

If `Name` (an alias name for `Key`) is used to identify the connection, this
name can be used as connection reference for subsequent calls. Only one open
connection at a time associated with `Name` is possible. If `Key` is used, the
returned handle must be used for subsequent calls (multiple connections can be
opened using the configuration data specified by `Key`).

For information on how to create a new `Name`, see `ct:require/2`.

`ConnType` always overrides the type specified in the address tuple in the
configuration data (and in `ExtraOpts`). So it is possible to, for example, open
an SFTP connection directly using data originally specifying an SSH connection.
Value `host` means that the connection type specified by the host option (either
in the configuration data or in `ExtraOpts`) is used.

`ExtraOpts` (optional) are extra SSH options to be added to the configuration
data for `KeyOrName`. The extra options override any existing options with the
same key in the configuration data. For details on valid SSH options, see
application [`SSH`](`e:ssh:index.html`).

# `del_dir`

```elixir
-spec del_dir(SSH, Name) -> Result
                 when
                     SSH :: connection(),
                     Name :: file:filename(),
                     Result :: ok | {error, Reason},
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `del_dir`

```elixir
-spec del_dir(SSH, Server, Name) -> Result
                 when
                     SSH :: connection(),
                     Server :: pid(),
                     Name :: file:filename(),
                     Result :: ok | {error, Reason},
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `delete`

```elixir
-spec delete(SSH, Name) -> Result
                when
                    SSH :: connection(),
                    Name :: file:filename(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `delete`

```elixir
-spec delete(SSH, Server, Name) -> Result
                when
                    SSH :: connection(),
                    Server :: pid(),
                    Name :: file:filename(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `disconnect`

```elixir
-spec disconnect(SSH) -> ok | {error, Reason} when SSH :: connection(), Reason :: term().
```

Closes an SSH/SFTP connection.

# `exec`

```elixir
-spec exec(SSH, Command) -> {ok, Data} | {timeout, Data} | {error, Reason}
              when SSH :: connection(), Command :: string(), Data :: string(), Reason :: term().
```

# `exec`

```elixir
-spec exec(SSH, Command, Timeout) -> {ok, Data} | {timeout, Data} | {error, Reason}
              when
                  SSH :: connection(),
                  Command :: string(),
                  Timeout :: timeout(),
                  Data :: string(),
                  Reason :: term();
          (SSH, ChannelId, Command) -> {ok, Data} | {timeout, Data} | {error, Reason}
              when
                  SSH :: connection(),
                  ChannelId :: ssh_channel_id(),
                  Command :: string(),
                  Data :: string(),
                  Reason :: term().
```

Requests server to perform `Command`, (see `exec/4`).

Equivalent to [`exec(SSH, undefined, Command, Timeout)`](`exec/4`) if
called with Command being string.

Equivalent to [`exec(SSH, ChannelId, Command, DefaultTimeout)`](`exec/4`) if
called with ChannelId being integer.

# `exec`

```elixir
-spec exec(SSH, ChannelId, Command, Timeout) -> {ok, Data} | {timeout, Data} | {error, Reason}
              when
                  SSH :: connection(),
                  ChannelId :: ssh_channel_id() | undefined,
                  Command :: string(),
                  Timeout :: timeout(),
                  Data :: string(),
                  Reason :: term().
```

Requests server to perform `Command`. A previously opened session channel is
used for the request. `Data` is received from the server as a result of the
command.

# `get_file_info`

```elixir
-spec get_file_info(SSH, Handle) -> Result
                       when
                           SSH :: connection(),
                           Handle :: term(),
                           Result :: {ok, FileInfo} | {error, Reason},
                           FileInfo :: file:file_info(),
                           Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `get_file_info`

```elixir
-spec get_file_info(SSH, Server, Handle) -> Result
                       when
                           SSH :: connection(),
                           Server :: pid(),
                           Handle :: term(),
                           Result :: {ok, FileInfo} | {error, Reason},
                           FileInfo :: file:file_info(),
                           Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `list_dir`

```elixir
-spec list_dir(SSH, Path) -> Result
                  when
                      SSH :: connection(),
                      Path :: file:filename(),
                      Result :: {ok, FileNames} | {error, Reason},
                      FileNames :: [FileName],
                      FileName :: file:filename(),
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `list_dir`

```elixir
-spec list_dir(SSH, Server, Path) -> Result
                  when
                      SSH :: connection(),
                      Server :: pid(),
                      Path :: file:filename(),
                      Result :: {ok, FileNames} | {error, Reason},
                      FileNames :: [FileName],
                      FileName :: file:filename(),
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `make_dir`

```elixir
-spec make_dir(SSH, Name) -> Result
                  when
                      SSH :: connection(),
                      Name :: file:filename(),
                      Result :: ok | {error, Reason},
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `make_dir`

```elixir
-spec make_dir(SSH, Server, Name) -> Result
                  when
                      SSH :: connection(),
                      Server :: pid(),
                      Name :: file:filename(),
                      Result :: ok | {error, Reason},
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `make_symlink`

```elixir
-spec make_symlink(SSH, Name, Target) -> Result
                      when
                          SSH :: connection(),
                          Name :: file:filename(),
                          Target :: file:filename(),
                          Result :: ok | {error, Reason},
                          Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `make_symlink`

```elixir
-spec make_symlink(SSH, Server, Name, Target) -> Result
                      when
                          SSH :: connection(),
                          Server :: pid(),
                          Name :: file:filename(),
                          Target :: file:filename(),
                          Result :: ok | {error, Reason},
                          Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `open`

```elixir
-spec open(SSH, File, Mode) -> Result
              when
                  SSH :: connection(),
                  File :: file:filename(),
                  Mode :: [read | write | append | binary | raw],
                  Result :: {ok, Handle} | {error, Reason},
                  Handle :: term(),
                  Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `open`

```elixir
-spec open(SSH, Server, File, Mode) -> Result
              when
                  SSH :: connection(),
                  Server :: pid(),
                  File :: file:filename(),
                  Mode :: [read | write | append | binary | raw],
                  Result :: {ok, Handle} | {error, Reason},
                  Handle :: term(),
                  Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `opendir`

```elixir
-spec opendir(SSH, Path) -> Result
                 when
                     SSH :: connection(),
                     Path :: file:filename(),
                     Result :: {ok, Handle} | {error, Reason},
                     Handle :: term(),
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `opendir`

```elixir
-spec opendir(SSH, Server, Path) -> Result
                 when
                     SSH :: connection(),
                     Server :: pid(),
                     Path :: file:filename(),
                     Result :: {ok, Handle} | {error, Reason},
                     Handle :: term(),
                     Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `position`

```elixir
-spec position(SSH, Handle, Location) -> Result
                  when
                      SSH :: connection(),
                      Handle :: term(),
                      Location ::
                          Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
                      Offset :: integer(),
                      Result :: {ok, NewPosition} | {error, Reason},
                      NewPosition :: integer(),
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `position`

```elixir
-spec position(SSH, Server, Handle, Location) -> Result
                  when
                      SSH :: connection(),
                      Server :: pid(),
                      Handle :: term(),
                      Location ::
                          Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
                      Offset :: integer(),
                      Result :: {ok, NewPosition} | {error, Reason},
                      NewPosition :: integer(),
                      Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `pread`

```elixir
-spec pread(SSH, Handle, Position, Length) -> Result
               when
                   SSH :: connection(),
                   Handle :: term(),
                   Position :: integer(),
                   Length :: integer(),
                   Result :: {ok, Data} | eof | {error, Reason},
                   Data :: string() | binary(),
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `pread`

```elixir
-spec pread(SSH, Server, Handle, Position, Length) -> Result
               when
                   SSH :: connection(),
                   Server :: pid(),
                   Handle :: term(),
                   Position :: integer(),
                   Length :: integer(),
                   Result :: {ok, Data} | eof | {error, Reason},
                   Data :: string() | binary(),
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `pwrite`

```elixir
-spec pwrite(SSH, Handle, Position, Data) -> Result
                when
                    SSH :: connection(),
                    Handle :: term(),
                    Position :: integer(),
                    Data :: iolist(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `pwrite`

```elixir
-spec pwrite(SSH, Server, Handle, Position, Data) -> Result
                when
                    SSH :: connection(),
                    Server :: pid(),
                    Handle :: term(),
                    Position :: integer(),
                    Data :: iolist(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read`

```elixir
-spec read(SSH, Handle, Len) -> Result
              when
                  SSH :: connection(),
                  Handle :: term(),
                  Len :: integer(),
                  Result :: {ok, Data} | eof | {error, Reason},
                  Data :: string() | binary(),
                  Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read`

```elixir
-spec read(SSH, Server, Handle, Len) -> Result
              when
                  SSH :: connection(),
                  Server :: pid(),
                  Handle :: term(),
                  Len :: integer(),
                  Result :: {ok, Data} | eof | {error, Reason},
                  Data :: string() | binary(),
                  Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_file`

```elixir
-spec read_file(SSH, File) -> Result
                   when
                       SSH :: connection(),
                       File :: file:filename(),
                       Result :: {ok, Data} | {error, Reason},
                       Data :: binary(),
                       Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_file`

```elixir
-spec read_file(SSH, Server, File) -> Result
                   when
                       SSH :: connection(),
                       Server :: pid(),
                       File :: file:filename(),
                       Result :: {ok, Data} | {error, Reason},
                       Data :: binary(),
                       Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_file_info`

```elixir
-spec read_file_info(SSH, Name) -> Result
                        when
                            SSH :: connection(),
                            Name :: file:filename(),
                            Result :: {ok, FileInfo} | {error, Reason},
                            FileInfo :: file:file_info(),
                            Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_file_info`

```elixir
-spec read_file_info(SSH, Server, Name) -> Result
                        when
                            SSH :: connection(),
                            Server :: pid(),
                            Name :: file:filename(),
                            Result :: {ok, FileInfo} | {error, Reason},
                            FileInfo :: file:file_info(),
                            Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_link`

```elixir
-spec read_link(SSH, Name) -> Result
                   when
                       SSH :: connection(),
                       Name :: file:filename(),
                       Result :: {ok, Target} | {error, Reason},
                       Target :: file:filename(),
                       Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_link`

```elixir
-spec read_link(SSH, Server, Name) -> Result
                   when
                       SSH :: connection(),
                       Server :: pid(),
                       Name :: file:filename(),
                       Result :: {ok, Target} | {error, Reason},
                       Target :: file:filename(),
                       Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_link_info`

```elixir
-spec read_link_info(SSH, Name) -> Result
                        when
                            SSH :: connection(),
                            Name :: string(),
                            Result :: {ok, FileInfo} | {error, Reason},
                            FileInfo :: file:file_info(),
                            Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `read_link_info`

```elixir
-spec read_link_info(SSH, Server, Name) -> Result
                        when
                            SSH :: connection(),
                            Server :: pid(),
                            Name :: file:filename(),
                            Result :: {ok, FileInfo} | {error, Reason},
                            FileInfo :: file:file_info(),
                            Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `receive_response`

```elixir
-spec receive_response(SSH, ChannelId) -> {ok, Data} | {timeout, Data} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Data :: string(),
                              Reason :: term().
```

# `receive_response`

```elixir
-spec receive_response(SSH, ChannelId, End) -> {ok, Data} | {timeout, Data} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              End :: fun((string()) -> boolean()),
                              Data :: string(),
                              Reason :: term();
                      (SSH, ChannelId, Timeout) -> {ok, Data} | {timeout, Data} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Timeout :: timeout(),
                              Data :: string(),
                              Reason :: term().
```

Receives expected data from server on the specified session channel
(see `receive_response/4`).

Equivalent to [`receive_response(SSH, ChannelId, End, DefaultTimeout)`](`receive_response/4`) if
called with End being function.

Equivalent to [`receive_response(SSH, ChannelId, close, Timeout)`](`receive_response/4`) if
called with Timeout being integer.

# `receive_response`

```elixir
-spec receive_response(SSH, ChannelId, End, Timeout) -> {ok, Data} | {timeout, Data} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              End :: close | timeout | fun((string()) -> boolean()),
                              Timeout :: timeout(),
                              Data :: string(),
                              Reason :: term().
```

Receives expected data from server on the specified session channel.

If `End == close`, data is returned to the caller when the channel is closed by
the server. If a time-out occurs before this happens, the function returns
`{timeout,Data}` (where `Data` is the data received so far).

If `End == timeout`, a time-out is expected and `{ok,Data}` is returned both in
the case of a time-out and when the channel is closed.

If `End` is a fun, this fun is called with one argument, the data value in a
received `ssh_cm` message (see `m:ssh_connection`. The fun is to return either
`true` to end the receiving operation (and have the so far collected data
returned) or `false` to wait for more data from the server. Even if a fun is
supplied, the function returns immediately if the server closes the channel).

# `rename`

```elixir
-spec rename(SSH, OldName, NewName) -> Result
                when
                    SSH :: connection(),
                    OldName :: file:filename(),
                    NewName :: file:filename(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `rename`

```elixir
-spec rename(SSH, Server, OldName, NewName) -> Result
                when
                    SSH :: connection(),
                    Server :: pid(),
                    OldName :: file:filename(),
                    NewName :: file:filename(),
                    Result :: ok | {error, Reason},
                    Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `send`

```elixir
-spec send(SSH, ChannelId, Data) -> ok | {error, Reason}
              when
                  SSH :: connection(), ChannelId :: ssh_channel_id(), Data :: iodata(), Reason :: term().
```

# `send`

```elixir
-spec send(SSH, ChannelId, Data, Timeout) -> ok | {error, Reason}
              when
                  SSH :: connection(),
                  ChannelId :: ssh_channel_id(),
                  Data :: iodata(),
                  Timeout :: timeout(),
                  Reason :: term();
          (SSH, ChannelId, Type, Data) -> ok | {error, Reason}
              when
                  SSH :: connection(),
                  ChannelId :: ssh_channel_id(),
                  Type :: ssh_data_type_code(),
                  Data :: iodata(),
                  Reason :: term().
```

Sends data to server on specified session channel (see `send/5`).

Equivalent to [`send(SSH, ChannelId, 0, Data, Timeout)`](`send/5`) if
called with Timeout being integer.

Equivalent to [`send(SSH, ChannelId, Type, Data, DefaultTimeout)`](`send/5`) if
called with Type being integer.

# `send`

```elixir
-spec send(SSH, ChannelId, Type, Data, Timeout) -> ok | {error, Reason}
              when
                  SSH :: connection(),
                  ChannelId :: ssh_channel_id(),
                  Type :: ssh_data_type_code(),
                  Data :: iodata(),
                  Timeout :: timeout(),
                  Reason :: term().
```

Sends data to server on specified session channel.

# `send_and_receive`

```elixir
-spec send_and_receive(SSH, ChannelId, Data) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Data :: iodata(),
                              ReceivedData :: string(),
                              Reason :: term().
```

# `send_and_receive`

```elixir
-spec send_and_receive(SSH, ChannelId, Data, End) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Data :: iodata(),
                              End :: close | timeout | fun((string()) -> boolean()),
                              ReceivedData :: string(),
                              Reason :: term();
                      (SSH, ChannelId, Data, Timeout) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Data :: iodata(),
                              Timeout :: timeout(),
                              ReceivedData :: string(),
                              Reason :: term();
                      (SSH, ChannelId, Type, Data) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Type :: ssh_data_type_code(),
                              Data :: iodata(),
                              ReceivedData :: string(),
                              Reason :: term().
```

Sends data to server on specified session channel and waits to receive the
server response (see `send_and_receive/6`).

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, End, DefaultTimeout)`](`send_and_receive/6`)
if called with End being function.

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, close, Timeout)`](`send_and_receive/6`)
if called with Timeout being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, close, DefaultTimeout)`](`send_and_receive/6`)
if called with Type being integer.

# `send_and_receive`

```elixir
-spec send_and_receive(SSH, ChannelId, Data, End, Timeout) ->
                          {ok, ReceivedData} | {timeout, Data} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Data :: iodata(),
                              End :: close | timeout | fun((string()) -> boolean()),
                              Timeout :: timeout(),
                              ReceivedData :: string(),
                              Reason :: term();
                      (SSH, ChannelId, Type, Data, Timeout) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Type :: ssh_data_type_code(),
                              Data :: iodata(),
                              Timeout :: timeout(),
                              ReceivedData :: string(),
                              Reason :: term();
                      (SSH, ChannelId, Type, Data, End) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Type :: ssh_data_type_code(),
                              Data :: iodata(),
                              End :: close | timeout | fun((string()) -> boolean()),
                              ReceivedData :: string(),
                              Reason :: term().
```

Sends data to server on specified session channel and waits to receive the
server response (see `send_and_receive/6`).

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, End, Timeout)`](`send_and_receive/6`) if
called with Timeout being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, close, Timeout)`](`send_and_receive/6`) if
called with Type being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, End, DefaultTimeout)`](`send_and_receive/6`) if
called with End being function.

# `send_and_receive`

```elixir
-spec send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) ->
                          {ok, ReceivedData} | {timeout, ReceivedData} | {error, Reason}
                          when
                              SSH :: connection(),
                              ChannelId :: ssh_channel_id(),
                              Type :: ssh_data_type_code(),
                              Data :: iodata(),
                              End :: close | timeout | fun((string()) -> boolean()),
                              Timeout :: timeout(),
                              ReceivedData :: string(),
                              Reason :: term().
```

Sends data to server on specified session channel and waits to receive the
server response.

For details on argument `End`, see
[`ct_ssh:receive_response/4`](`receive_response/4`).

# `session_close`

```elixir
-spec session_close(SSH, ChannelId) -> ok | {error, Reason}
                       when SSH :: connection(), ChannelId :: ssh_channel_id(), Reason :: term().
```

Closes an SSH session channel.

# `session_open`

```elixir
-spec session_open(SSH) -> {ok, ChannelId} | {error, Reason}
                      when SSH :: connection(), ChannelId :: ssh_channel_id(), Reason :: term().
```

# `session_open`

```elixir
-spec session_open(SSH, Timeout) -> {ok, ChannelId} | {error, Reason}
                      when
                          SSH :: connection(),
                          Timeout :: timeout(),
                          ChannelId :: ssh_channel_id(),
                          Reason :: term().
```

Opens a channel for an SSH session.

# `sftp_connect`

```elixir
-spec sftp_connect(SSH) -> {ok, Server} | {error, Reason}
                      when SSH :: connection(), Server :: pid(), Reason :: term().
```

Starts an SFTP session on an already existing SSH connection. `Server`
identifies the new session and must be specified whenever SFTP requests are to
be sent.

# `shell`
*since OTP 20.0* 

```elixir
-spec shell(SSH, ChannelId) -> Result
               when
                   SSH :: connection(),
                   ChannelId :: ssh:ssh_channel_id(),
                   Result :: ok | {error, term()}.
```

# `shell`
*since OTP 20.0* 

```elixir
-spec shell(SSH, ChannelId, Timeout) -> Result
               when
                   SSH :: connection(),
                   ChannelId :: ssh:ssh_channel_id(),
                   Timeout :: timeout(),
                   Result :: ok | {error, term()}.
```

Requests that the user's default shell (typically defined in `/etc/passwd` in Unix
systems) is executed at the server end.

# `subsystem`

```elixir
-spec subsystem(SSH, ChannelId, Subsystem) -> Status | {error, Reason}
                   when
                       SSH :: connection(),
                       ChannelId :: ssh_channel_id(),
                       Subsystem :: string(),
                       Status :: success | failure,
                       Reason :: term().
```

# `subsystem`

```elixir
-spec subsystem(SSH, ChannelId, Subsystem, Timeout) -> Status | {error, Reason}
                   when
                       SSH :: connection(),
                       ChannelId :: ssh_channel_id(),
                       Subsystem :: string(),
                       Timeout :: timeout(),
                       Status :: success | failure,
                       Reason :: term().
```

Sends a request to execute a predefined subsystem.

# `write`

```elixir
-spec write(SSH, Handle, Data) -> Result
               when
                   SSH :: connection(),
                   Handle :: term(),
                   Data :: iodata(),
                   Result :: ok | {error, Reason},
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `write`

```elixir
-spec write(SSH, Server, Handle, Data) -> Result
               when
                   SSH :: connection(),
                   Server :: pid(),
                   Handle :: term(),
                   Data :: iodata(),
                   Result :: ok | {error, Reason},
                   Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `write_file`

```elixir
-spec write_file(SSH, File, Iolist) -> Result
                    when
                        SSH :: connection(),
                        File :: file:filename(),
                        Iolist :: iodata(),
                        Result :: ok | {error, Reason},
                        Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `write_file`

```elixir
-spec write_file(SSH, Server, File, Iolist) -> Result
                    when
                        SSH :: connection(),
                        Server :: pid(),
                        File :: file:filename(),
                        Iolist :: iodata(),
                        Result :: ok | {error, Reason},
                        Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `write_file_info`

```elixir
-spec write_file_info(SSH, Name, Info) -> Result
                         when
                             SSH :: connection(),
                             Name :: file:filename(),
                             Info :: file:file_info(),
                             Result :: ok | {error, Reason},
                             Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

# `write_file_info`

```elixir
-spec write_file_info(SSH, Server, Name, Info) -> Result
                         when
                             SSH :: connection(),
                             Server :: pid(),
                             Name :: file:filename(),
                             Info :: file:file_info(),
                             Result :: ok | {error, Reason},
                             Reason :: term().
```

For information and other types, see `m:ssh_sftp`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
