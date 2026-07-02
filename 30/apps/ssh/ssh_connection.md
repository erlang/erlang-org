# `ssh_connection`
[🔗](https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh_connection.erl#L29)

This module provides API functions to send SSH Connection Protocol events to the
other side of an SSH channel.

The [SSH Connection Protocol (RFC 4254)](http://www.ietf.org/rfc/rfc4254.txt) is used by
clients and servers, that is, SSH channels, to communicate over the SSH
connection. The API functions in this module send SSH Connection Protocol
events, which are received as messages by the remote channel handling the remote
channel. The Erlang format of thoose messages is (see also
[below](`t:event/0`)):

`{ssh_cm,` `t:ssh:connection_ref/0` `,` `t:channel_msg/0` `}`

If the `m:ssh_client_channel` behavior is used to implement the channel process,
these messages are handled by
[handle_ssh_msg/2](`c:ssh_client_channel:handle_ssh_msg/2`).

# `closed_ch_msg`
*RFC 4254, section 5.3* 

```erlang
-type closed_ch_msg() :: {closed, ssh:channel_id()}.
```

This event is sent as a result of calling [ssh_connection:close/2](`close/2`).
Both the handling of this event and sending it are taken care of by the
`m:ssh_client_channel` behavior.

# `eof_ch_msg`
*RFC 4254, section 5.3* 

```erlang
-type eof_ch_msg() :: {eof, ssh:channel_id()}.
```

Indicates that the other side sends no more data. This event is sent as a result
of calling [ssh_connection:send_eof/2](`send_eof/2`).

# `data_ch_msg`
*RFC 4254, section 5.2* 

```erlang
-type data_ch_msg() :: {data, ssh:channel_id(), ssh_data_type_code(), Data :: binary()}.
```

Data has arrived on the channel. This event is sent as a result of calling
[ssh_connection:send/3,4,5](`send/3`).

# `env_ch_msg`
*RFC 4254, section 6.4* 

```erlang
-type env_ch_msg() :: {env, ssh:channel_id(), want_reply(), Var :: string(), Value :: string()}.
```

Environment variables can be passed to the shell/command to be started later.
This event is sent as a result of calling [ssh_connection:setenv/5](`setenv/5`).

# `exit_signal_ch_msg`
*RFC 4254, section 6.10* 

```erlang
-type exit_signal_ch_msg() ::
          {exit_signal,
           ssh:channel_id(),
           ExitSignal :: string(),
           ErrorMsg :: string(),
           LanguageString :: string()}.
```

A remote execution can terminate violently because of a signal. Then this
message can be received. For details on valid string values, see
[RFC 4254](https://tools.ietf.org/html/rfc4254#section-6.10) Section 6.10, which
shows a special case of these signals.

# `exit_status_ch_msg`
*RFC 4254, section 6.10* 

```erlang
-type exit_status_ch_msg() :: {exit_status, ssh:channel_id(), ExitStatus :: non_neg_integer()}.
```

When the command running at the other end terminates, the following message can
be sent to return the exit status of the command. A zero `exit_status` usually
means that the command terminated successfully. This event is sent as a result
of calling [ssh_connection:exit_status/3](`exit_status/3`).

# `channel_msg`

```erlang
-type channel_msg() ::
          data_ch_msg() |
          eof_ch_msg() |
          closed_ch_msg() |
          pty_ch_msg() |
          env_ch_msg() |
          shell_ch_msg() |
          exec_ch_msg() |
          signal_ch_msg() |
          window_change_ch_msg() |
          exit_status_ch_msg() |
          exit_signal_ch_msg().
```

As mentioned in the introduction, the
[SSH Connection Protocol](https://tools.ietf.org/html/rfc4254) events are
handled as messages. When writing a channel handling process without using the
support by the `m:ssh_client_channel` behavior the process must handle thoose
messages.

# `event`

```erlang
-type event() :: {ssh_cm, ssh:connection_ref(), channel_msg()}.
```

# `want_reply`

```erlang
-type want_reply() :: boolean().
```

Messages that include a `WantReply` expect the channel handling process to call
[ssh_connection:reply_request/4](`reply_request/4`) with the boolean value of
`WantReply` as the second argument.

# `pty_ch_msg`
*RFC 4254, section 6.2* 

```erlang
-type pty_ch_msg() ::
          {pty,
           ssh:channel_id(),
           want_reply(),
           {Terminal :: string(),
            CharWidth :: non_neg_integer(),
            RowHeight :: non_neg_integer(),
            PixelWidth :: non_neg_integer(),
            PixelHeight :: non_neg_integer(),
            TerminalModes :: [term_mode()]}}.
```

# `term_mode`
*RFC 4254, section 6.2* 

```erlang
-type term_mode() :: {Opcode :: atom() | byte(), Value :: non_neg_integer()}.
```

A pseudo-terminal has been requested for the session. `Terminal` is the value of
the TERM environment variable value, that is, `vt100`. Zero dimension parameters
must be ignored. The character/row dimensions override the pixel dimensions
(when non-zero). Pixel dimensions refer to the drawable area of the window.
`Opcode` in the `TerminalModes` list is the mnemonic name, represented as a
lowercase Erlang atom, defined in
[RFC 4254](https://tools.ietf.org/html/rfc4254#section/8), Section 8. It can
also be an `Opcode` if the mnemonic name is not listed in the RFC. Example:
`OP code: 53, mnemonic name ECHO erlang atom: echo`. This event is sent as a
result of calling [ssh_connection:ptty_alloc/4](`ptty_alloc/4`).

# `exec_ch_msg`
*RFC 4254, section 6.5* 

```erlang
-type exec_ch_msg() :: {exec, ssh:channel_id(), want_reply(), Command :: string()}.
```

This message requests that the server starts execution of the given command.
This event is sent as a result of calling [ssh_connection:exec/4 ](`exec/4`).

# `shell_ch_msg`
*RFC 4254, section 6.5* 

```erlang
-type shell_ch_msg() :: {shell, ssh:channel_id(), want_reply()}.
```

This message requests that the user default shell is started at the other end.
This event is sent as a result of calling [ssh_connection:shell/2](`shell/2`).

# `signal_ch_msg`
*RFC 4254, section 6.9* 

```erlang
-type signal_ch_msg() :: {signal, ssh:channel_id(), SignalName :: string()}.
```

A signal can be delivered to the remote process/service using the following
message. Some systems do not support signals, in which case they are to ignore
this message. There is currently no function to generate this event as the
signals referred to are on OS-level and not something generated by an Erlang
program.

# `window_change_ch_msg`
*RFC 4254, section 6.7* 

```erlang
-type window_change_ch_msg() ::
          {window_change,
           ssh:channel_id(),
           CharWidth :: non_neg_integer(),
           RowHeight :: non_neg_integer(),
           PixelWidth :: non_neg_integer(),
           PixelHeight :: non_neg_integer()}.
```

When the window (terminal) size changes on the client side, it _can_ send a
message to the server side to inform it of the new dimensions. No API function
generates this event.

# `channel_id`

```erlang
-type channel_id() :: ssh:channel_id().
```

# `connection_ref`

```erlang
-type connection_ref() :: ssh:connection_ref().
```

# `reason`

```erlang
-type reason() :: closed | timeout.
```

The result of a call.

If the request reached the peer, was handled and the response reached the
requesting node the `t:req_status/0` is the status reported from the peer.

If not, the `t:reason/0` indicates what went wrong:

- **`closed`** - indicates that the channel or connection was closed when trying
  to send the request

- **`timeout`** - indicates that the operation exceeded a time limit

# `req_status`

```erlang
-type req_status() :: success | failure.
```

The status of a request. Corresponds to the `SSH_MSG_CHANNEL_SUCCESS` and
`SSH_MSG_CHANNEL_FAILURE` values in
[RFC 4254, Section 5.4](https://tools.ietf.org/html/rfc4254#section-5.4).

# `result`

```erlang
-type result() :: req_status() | {error, reason()}.
```

# `ssh_data_type_code`

```erlang
-type ssh_data_type_code() :: non_neg_integer().
```

The valid values are `0` ("normal") and `1` ("stderr"), see
[RFC 4254, Section 5.2](https://tools.ietf.org/html/rfc4254#page-8).

# `adjust_window`

```erlang
-spec adjust_window(ConnectionRef, ChannelId, NumOfBytes) -> ok
                       when
                           ConnectionRef :: ssh:connection_ref(),
                           ChannelId :: ssh:channel_id(),
                           NumOfBytes :: integer().
```

Adjusts the SSH flow control window. This is to be done by both the client- and
server-side channel processes.

> #### Note {: .info }
>
> Channels implemented with the `m:ssh_client_channel` behavior do not normally
> need to call this function as flow control is handled by the behavior. The
> behavior adjusts the window every time the callback
> [handle_ssh_msg/2](`c:ssh_client_channel:handle_ssh_msg/2`) returns after
> processing channel data.

# `close`

```erlang
-spec close(ConnectionRef, ChannelId) -> ok
               when ConnectionRef :: ssh:connection_ref(), ChannelId :: ssh:channel_id().
```

A server- or client-channel process can choose to close their session by sending
a close event.

> #### Note {: .info }
>
> This function is called by the `ssh_client_channel` behavior when the channel
> is terminated, see `m:ssh_client_channel`. Thus, channels implemented with the
> behavior are not to call this function explicitly.

# `exec`

```erlang
-spec exec(ConnectionRef, ChannelId, Command, Timeout) -> result()
              when
                  ConnectionRef :: ssh:connection_ref(),
                  ChannelId :: ssh:channel_id(),
                  Command :: string(),
                  Timeout :: timeout().
```

Is to be called by a client-channel process to request that the server starts
executing the given command. The result is several messages according to the
following pattern. The last message is a channel close message, as the `exec`
request is a one-time execution that closes the channel when it is done.

- **N x [data message(s)](`t:data_ch_msg/0`)** - The result of executing the
  command can be only one line or thousands of lines depending on the command.

- **0 or 1 x [eof message](`t:eof_ch_msg/0`)** - Indicates that no more data is
  to be sent.

- **0 or 1 x [exit signal message](`t:exit_signal_ch_msg/0`)** - Not all systems
  send signals. For details on valid string values, see RFC 4254, Section 6.10

- **0 or 1 x [exit status message](`t:exit_status_ch_msg/0`)** - It is
  recommended by the SSH Connection Protocol to send this message, but that is
  not always the case.

- **1 x [closed status message](`t:closed_ch_msg/0`)** - Indicates that the
  `ssh_client_channel` started for the execution of the command has now been
  shut down.

See the User's Guide section on
[One-Time Execution](using_ssh.md#one-time-execution) for examples.

> #### Note {: .info }
>
> In case when command generates large amount of output data, manual
> window adjustment might be necessary in order to receive it.
> see [`ssh_connectino:adjust_window/3`](`adjust_window/3`)

# `exit_status`

```erlang
-spec exit_status(ConnectionRef, ChannelId, Status) -> ok
                     when
                         ConnectionRef :: ssh:connection_ref(),
                         ChannelId :: ssh:channel_id(),
                         Status :: integer().
```

Is to be called by a server-channel process to send the exit status of a command
to the client.

# `ptty_alloc`
*since OTP 17.5* 

```erlang
-spec ptty_alloc(ConnectionRef, ChannelId, Options) -> result()
                    when
                        ConnectionRef :: ssh:connection_ref(),
                        ChannelId :: ssh:channel_id(),
                        Options :: proplists:proplist().
```

# `ptty_alloc`
*since OTP 17.4* 

```erlang
-spec ptty_alloc(ConnectionRef, ChannelId, Options, Timeout) -> result()
                    when
                        ConnectionRef :: ssh:connection_ref(),
                        ChannelId :: ssh:channel_id(),
                        Options :: proplists:proplist(),
                        Timeout :: timeout().
```

Sends an SSH Connection Protocol `pty_req`, to allocate a pseudo-terminal. Is to
be called by an SSH client process.

Options:

- **\{term, string()\}** - Defaults to _os:getenv("TERM")_ or _vt100_ if it is
  undefined.

- **\{width, integer()\}** - Defaults to 80 if `pixel_width` is not defined.

- **\{height, integer()\}** - Defaults to 24 if `pixel_height` is not defined.

- **\{pixel_width, integer()\}** - Is disregarded if `width` is defined.

- **\{pixel_height, integer()\}** - Is disregarded if `height` is defined.

- **\{pty_opts, \[\{posix_atom(), integer()\}]\}** - Option can be an empty
  list. Otherwise, see possible _POSIX_ names in Section 8 in
  [RFC 4254](http://www.ietf.org/rfc/rfc4254.txt).

# `reply_request`

```erlang
-spec reply_request(ConnectionRef, WantReply, Status, ChannelId) -> ok
                       when
                           ConnectionRef :: ssh:connection_ref(),
                           WantReply :: boolean(),
                           Status :: req_status(),
                           ChannelId :: ssh:channel_id().
```

Sends status replies to requests where the requester has stated that it wants a
status report, that is, `WantReply = true`. If `WantReply` is `false`, calling
this function becomes a "noop". Is to be called while handling an SSH Connection
Protocol message containing a `WantReply` boolean value.

# `send`

```erlang
-spec send(connection_ref(), channel_id(), iodata()) -> ok | {error, reason()}.
```

# `send`

```erlang
-spec send(connection_ref(), channel_id(), iodata(), timeout()) -> ok | {error, reason()};
          (connection_ref(), channel_id(), ssh_data_type_code(), iodata()) -> ok | {error, reason()}.
```

Depending on input arguments equivalent to one of `send/5` calls specified below.

Equivalent to [send(ConnectionRef, ChannelId, 0, Data, TimeOut)](`send/5`) if
called with TimeOut being integer.

Equivalent to [send(ConnectionRef, ChannelId, 0, Data, infinity)](`send/5`) if
called with TimeOut being infinity atom.

Equivalent to [send(ConnectionRef, ChannelId, Type, Data, infinity)](`send/5`) if
called with last argument which is not integer or infinity atom.

# `send`

```erlang
-spec send(connection_ref(), channel_id(), ssh_data_type_code(), iodata(), timeout()) ->
              ok | {error, reason()}.
```

Is to be called by client- and server-channel processes to send data to each
other.

The function `subsystem/4` and subsequent calls of `send/3,4,5` must be executed
in the same process.

# `send_eof`

```erlang
-spec send_eof(ConnectionRef, ChannelId) -> ok | {error, closed}
                  when ConnectionRef :: ssh:connection_ref(), ChannelId :: ssh:channel_id().
```

Sends EOF on channel `ChannelId`.

# `session_channel`

```erlang
-spec session_channel(ConnectionRef, Timeout) -> Result
                         when
                             ConnectionRef :: ssh:connection_ref(),
                             Timeout :: timeout(),
                             Result :: {ok, ssh:channel_id()} | {error, reason()}.
```

# `session_channel`

```erlang
-spec session_channel(ConnectionRef, InitialWindowSize, MaxPacketSize, Timeout) -> Result
                         when
                             ConnectionRef :: ssh:connection_ref(),
                             InitialWindowSize :: pos_integer() | undefined,
                             MaxPacketSize :: pos_integer() | undefined,
                             Timeout :: timeout(),
                             Result :: {ok, ssh:channel_id()} | {error, reason()}.
```

Opens a channel for an SSH session. The channel id returned from this function
is the id used as input to the other functions in this module.

# `setenv`

```erlang
-spec setenv(ConnectionRef, ChannelId, Var, Value, Timeout) -> success
                when
                    ConnectionRef :: ssh:connection_ref(),
                    ChannelId :: ssh:channel_id(),
                    Var :: string(),
                    Value :: string(),
                    Timeout :: timeout().
```

Environment variables can be passed before starting the shell/command. Is to be
called by a client channel processes.

# `shell`

```erlang
-spec shell(ConnectionRef, ChannelId) -> Result
               when
                   ConnectionRef :: ssh:connection_ref(),
                   ChannelId :: ssh:channel_id(),
                   Result :: ok | success | failure | {error, timeout}.
```

Is to be called by a client channel process to request that the user default
shell (typically defined in /etc/passwd in Unix systems) is executed at the
server end.

Note: the return value is `ok` instead of `success` unlike in other functions in
this module. This is a fault that was introduced so long ago that any change
would break a large number of existing software.

# `subsystem`

```erlang
-spec subsystem(ConnectionRef, ChannelId, Subsystem, Timeout) -> result()
                   when
                       ConnectionRef :: ssh:connection_ref(),
                       ChannelId :: ssh:channel_id(),
                       Subsystem :: string(),
                       Timeout :: timeout().
```

Is to be called by a client-channel process for requesting to execute a
predefined subsystem on the server.

The function [`subsystem/4`](`subsystem/4`) and subsequent calls of
[send/3,4,5](`send/3`) must be executed in the same process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
