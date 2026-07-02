# `ssh_client_channel`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/ssh/src/ssh_client_channel.erl#L25)

SSH services (clients and servers) are implemented as channels that are
multiplexed over an SSH connection and communicates over the
[SSH Connection Protocol](http://www.ietf.org/rfc/rfc4254.txt). This module
provides a callback API that takes care of generic channel aspects for clients,
such as flow control and close messages. It lets the callback functions take
care of the service (application) specific parts. This behavior also ensures
that the channel process honors the principal of an OTP-process so that it can
be part of a supervisor tree. This is a requirement of channel processes
implementing a subsystem that will be added to the `ssh` applications supervisor
tree.

> #### Note {: .info }
>
> This module replaces ssh_channel.
>
> The old module is still available for compatibility, but should not be used
> for new programs. The old module will not be maintained except for some error
> corrections

> #### Note {: .info }
>
> When implementing a `ssh` subsystem for daemons, use
> [\-behaviour(ssh_server_channel)](`m:ssh_server_channel`) (Replaces
> ssh_daemon_channel) instead.

> #### Dont {: .error }
>
> Functions in this module are not supposed to be called outside a module
> implementing this behaviour\!

## Callback timeouts

The timeout values that can be returned by the callback functions have the same
semantics as in a `m:gen_server`. If the time-out occurs, `c:handle_msg/2` is called as
handle_msg(timeout, State).

# `client`
*since OTP 21.0* 

```elixir
-opaque client() :: term().
```

# `code_change`
*since OTP 21.0* 

```elixir
-callback code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) ->
                         {ok, NewState :: term()} | {error, Reason :: term()}.
```

Converts process state when code is changed.

This function is called by a client-side channel when it is to update its
internal state during a release upgrade or downgrade, that is, when the
instruction `{update,Module,Change,...}`, where `Change={advanced,Extra}`, is
given in the `appup` file. For more information, refer to Section 9.11.6 Release
Handling Instructions in the
[System Documentation](`e:system:release_handling.md#instr`).

> #### Note {: .info }
>
> Soft upgrade according to the OTP release concept is not straight forward for
> the server side, as subsystem channel processes are spawned by the `ssh`
> application and hence added to its supervisor tree. The subsystem channels can
> be upgraded when upgrading the user application, if the callback functions can
> handle two versions of the state, but this function cannot be used in the
> normal way.

# `handle_call`
*since OTP 21.0* 

```elixir
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
```

Handles messages sent by calling [call/2,3](`call/2`)

For more detailed information on time-outs,, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).

# `handle_cast`
*since OTP 21.0* 

```elixir
-callback handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
```

Handles messages sent by calling [`cast/2`](`cast/2`).

For more detailed information on time-outs, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).

# `handle_msg`
*since OTP 21.0* 

```elixir
-callback handle_msg(Msg :: term(), State :: term()) ->
                        {ok, State :: term()} | {stop, ChannelId :: ssh:channel_id(), State :: term()}.
```

Handles other messages than SSH Connection Protocol, call, or cast messages sent
to the channel.

Possible Erlang 'EXIT' messages is to be handled by this function and all
channels are to handle the following message.

- **`{ssh_channel_up,` `t:ssh:channel_id/0` `,` `t:ssh:connection_ref/0` `}`** -
  This is the first message that the channel receives. It is sent just before
  the `init/1` function returns successfully. This is especially useful if the
  server wants to send a message to the client without first receiving a message
  from it. If the message is not useful for your particular scenario, ignore it
  by immediately returning `{ok, State}`.

# `handle_ssh_msg`
*since OTP 21.0* 

```elixir
-callback handle_ssh_msg(ssh_connection:event(), State :: term()) ->
                            {ok, State :: term()} | {stop, ChannelId :: ssh:channel_id(), State :: term()}.
```

Handles SSH Connection Protocol messages that may need service-specific
attention. For details, see `t:ssh_connection:event/0`.

The following message is taken care of by the `ssh_client_channel` behavior.

- **`{closed,` `t:ssh:channel_id/0` `}`** - The channel behavior sends a close
  message to the other side, if such a message has not already been sent. Then
  it terminates the channel with reason `normal`.

# `init`
*since OTP 21.0* 

```elixir
-callback init(Args :: term()) ->
                  {ok, State :: term()} |
                  {ok, State :: term(), timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
```

Makes necessary initializations and returns the initial channel state if the
initializations succeed.

For more detailed information on time-outs, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).

# `terminate`
*since OTP 21.0* 

```elixir
-callback terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: term()) -> term().
```

This function is called by a channel process when it is about to terminate.
Before this function is called,
[ssh_connection:close/2 ](`ssh_connection:close/2`)is called, if it has not been
called earlier. This function does any necessary cleaning up. When it returns,
the channel process terminates with reason `Reason`. The return value is
ignored.

# `call`
*since OTP 21.0* 

```elixir
-spec call(ChannelRef, Msg) -> Reply | {error, Reason}
              when ChannelRef :: pid(), Msg :: term(), Reply :: term(), Reason :: closed | timeout.
```

# `call`
*since OTP 21.0* 

```elixir
-spec call(ChannelRef, Msg, Timeout) -> Reply | {error, Reason}
              when
                  ChannelRef :: pid(),
                  Msg :: term(),
                  Timeout :: timeout(),
                  Reply :: term(),
                  Reason :: closed | timeout.
```

Makes a synchronous call to the channel process by sending a message and waiting
until a reply arrives, or a time-out occurs. The channel calls
[Module:handle_call/3](`c:handle_call/3`) to handle the message. If the channel
process does not exist, `{error, closed}` is returned.

# `cast`
*since OTP 21.0* 

```elixir
-spec cast(ChannelRef, Msg) -> ok when ChannelRef :: pid(), Msg :: term().
```

Sends an asynchronous message to the channel process and returns ok immediately,
ignoring if the destination node or channel process does not exist. The channel
calls [Module:handle_cast/2](`c:handle_cast/2`) to handle the message.

# `enter_loop`
*since OTP 21.0* 

```elixir
-spec enter_loop(State) -> no_return() when State :: term().
```

Makes an existing process an `ssh_client_channel` (replaces ssh_channel)
process.

Does not return, instead the calling process enters the
`ssh_client_channel` (replaces ssh_channel) process receive loop and become an
`ssh_client_channel` process. The process must have been started using one of
the start functions in `proc_lib`, see the `m:proc_lib` manual page in STDLIB.
The user is responsible for any initialization of the process and must call
`init/1`.

# `init`
*since OTP 21.0* 

```elixir
-spec init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
              when
                  Args :: [[{Option :: term(), Value :: term()}]],
                  State :: term(),
                  Timeout :: timeout(),
                  Reason :: term().
```

Initiates a client channel.

The following options must be present:

- **`{channel_cb, atom()}`** - The module that implements the channel behaviour.

- **`{init_args(), term()}`** - The arguments to the `init` function of
  the callback module.

- **`{cm,` `t:ssh:connection_ref/0` `}`** - Reference to the `ssh` connection as
  returned by `ssh:connect/3`.

- **`{channel_id,` `t:ssh:channel_id/0` `}`** - Id of the `ssh` channel as
  returned by
  [ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

> #### Note {: .info }
>
> This function is normally not called by the user. The user only needs to call
> if the channel process needs to be started with help of `proc_lib` instead of
> calling [`start/4`](`start/4`) or [`start_link/4`](`start_link/4`).

# `reply`
*since OTP 21.0* 

```elixir
-spec reply(Client, Reply) -> _ when Client :: client(), Reply :: term().
```

This function can be used by a channel to send a reply to a client that called
`call/[2,3]` when the reply cannot be defined in the return value of
[Module:handle_call/3](`c:handle_call/3`).

`Client` must be the `From` argument provided to the callback function
[`handle_call/3`](`c:handle_call/3`). `Reply` is an arbitrary term, which is
given back to the client as the return value of [call/\[2,3].](`call/2`)

# `start`
*since OTP 21.0* 

```elixir
-spec start(SshConnection, ChannelId, ChannelCb, CbInitArgs) ->
               {ok, ChannelRef} | {error, Reason :: term()}
               when
                   SshConnection :: ssh:connection_ref(),
                   ChannelId :: ssh:channel_id(),
                   ChannelCb :: atom(),
                   CbInitArgs :: term(),
                   ChannelRef :: pid().
```

# `start_link`
*since OTP 21.0* 

```elixir
-spec start_link(SshConnection, ChannelId, ChannelCb, CbInitArgs) ->
                    {ok, ChannelRef} | {error, Reason :: term()}
                    when
                        SshConnection :: ssh:connection_ref(),
                        ChannelId :: ssh:channel_id(),
                        ChannelCb :: atom(),
                        CbInitArgs :: term(),
                        ChannelRef :: pid().
```

Starts a process that handles an SSH channel. It is called internally, by the
`ssh` daemon, or explicitly by the `ssh` client implementations. The behavior
sets the `trap_exit` flag to `true`.

The `CbInitArgs` parameter can be any Erlang term and will be passed as-is to
the callback module's `init/1` function. Common patterns include:
- A list of parameters: `[Param1, Param2, ...]`
- A map: `#{key => value}`
- A single value: an atom, tuple, or other term

---

*Consult [api-reference.md](api-reference.md) for complete listing*
