# `ct_telnet`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/common_test/src/ct_telnet.erl#L23)

`Common Test` specific layer on top of Telnet client `ct_telnet_client.erl`.

Use this module to set up Telnet connections, send commands, and perform string
matching on the result. For information about how to use `ct_telnet` and
configure connections, specifically for UNIX hosts, see the `m:unix_telnet`
manual page.

Default values defined in `ct_telnet`:

[](){: #Default_values }

- Connection timeout (time to wait for connection) = 10 seconds
- Command timeout (time to wait for a command to return) = 10 seconds
- Max number of reconnection attempts = 3
- Reconnection interval (time to wait in between reconnection attempts) = 5
  seconds
- Keep alive (sends NOP to the server every 8 sec if connection is idle) =
  `true`
- Polling limit (max number of times to poll to get a remaining string
  terminated) = 0
- Polling interval (sleep time between polls) = 1 second
- The TCP_NODELAY option for the telnet socket is disabled (set to `false`) per
  default

These parameters can be modified by the user with the following configuration
term:

```erlang
{telnet_settings, [{connect_timeout,Millisec},
                   {command_timeout,Millisec},
                   {reconnection_attempts,N},
                   {reconnection_interval,Millisec},
                   {keep_alive,Bool},
                   {poll_limit,N},
                   {poll_interval,Millisec},
                   {tcp_nodelay,Bool}]}.
```

`Millisec = timeout(), N = integer()`

Enter the `telnet_settings` term in a configuration file included in the test
and `ct_telnet` retrieves the information automatically.

`keep_alive` can be specified per connection, if necessary. For details, see
`m:unix_telnet`.

## Logging

[](){: #Logging }

The default logging behavior of `ct_telnet` is to print information about
performed operations, commands, and their corresponding results to the test case
HTML log. The following is not printed to the HTML log: text strings sent from
the Telnet server that are not explicitly received by a `ct_telnet` function,
such as [`expect/3`](`expect/3`). However, `ct_telnet` can be configured to use
a special purpose event handler, implemented in `ct_conn_log_h`, for logging
_all_ Telnet traffic. To use this handler, install a `Common Test` hook named
`cth_conn_log`. Example (using the test suite information function):

```erlang
suite() ->
    [{ct_hooks, [{cth_conn_log, [{conn_mod(),hook_options()}]}]}].
```

`conn_mod()` is the name of the `Common Test` module implementing the connection
protocol, that is, `ct_telnet`.

The `cth_conn_log` hook performs unformatted logging of Telnet data to a
separate text file. All Telnet communication is captured and printed, including
any data sent from the server. The link to this text file is located at the top
of the test case HTML log.

By default, data for all Telnet connections is logged in one common file (named
`default`), which can get messy, for example, if multiple Telnet sessions are
running in parallel. Therefore a separate log file can be created for each
connection. To configure this, use hook option `hosts` and list the names of the
servers/connections to be used in the suite. The connections must be named for
this to work (see [`ct_telnet:open/1,2,3,4`](`open/1`)).

Hook option `log_type` can be used to change the `cth_conn_log` behavior. The
default value of this option is `raw`, which results in the behavior described
above. If the value is set to `html`, all Telnet communication is printed to the
test case HTML log instead.

For raw logs, `prefix` option can be used for adjusting prefix data
added to connection log. The default value of this option is
`disabled`, which results with no prefix data. If the value is set to
`full` prefix contains timestamp and additonal information. If the
value is set to `short` prefix includes only human readable timestamp.

All `cth_conn_log` hook options described can also be specified in a
configuration file with configuration variable `ct_conn_log`.

_Example:_

```erlang
{ct_conn_log, [{ct_telnet,[{log_type,raw},
                           {hosts,[key_or_name()]}]}]}
```

> #### Note {: .info }
>
> Hook options specified in a configuration file overwrite any hard-coded hook
> options in the test suite.

[](){: #Logging_example }

_Logging Example:_

The following `ct_hooks` statement causes printing of Telnet traffic to separate
logs for the connections `server1` and `server2`. Traffic for any other
connections is logged in the default Telnet log.

```erlang
suite() ->
    [{ct_hooks,
      [{cth_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}]}].
```

As previously explained, this specification can also be provided by an entry
like the following in a configuration file:

```erlang
{ct_conn_log, [{ct_telnet,[{hosts,[server1,server2]}]}]}.
```

In this case the `ct_hooks` statement in the test suite can look as follows:

```erlang
suite() ->
    [{ct_hooks, [{cth_conn_log, []}]}].
```

### See Also

`m:unix_telnet`

# `connection`

```erlang
-type connection() :: handle() | {ct:target_name(), connection_type()} | ct:target_name().
```

Reference to opened Telnet connection associated to either a `handle` or `target_name`.

# `connection_type`

```erlang
-type connection_type() :: telnet | ts1 | ts2.
```

Telnet connection_type, valid values: 'telnet' | 'ts1' | 'ts2'.

# `handle`

```erlang
-type handle() :: ct:handle().
```

Handle for a specific Telnet connection, see module `m:ct`.

# `newline_option`
*not exported* 

```erlang
-type newline_option() :: {newline, boolean() | string()}.
```

See `cmd/3` for explanation.

# `prompt_regexp`

```erlang
-type prompt_regexp() :: string().
```

Regular expression matching all possible prompts for a specific target type.
`regexp` must not have any groups, that is, when matching, `re:run/3` (in
STDLIB) must return a list with one single element.

# `close`

```erlang
-spec close(Connection) -> ok | {error, Reason}
               when Connection :: connection(), Reason :: already_closed | term().
```

Closes the Telnet connection and stops the process managing it.

A connection can be associated with a target name and/or a handle. If
`Connection` has no associated target name, it can only be closed with the
handle value (see [`ct_telnet:open/4`](`open/4`)).

# `cmd`

```erlang
-spec cmd(Connection, Cmd) -> {ok, Data} | {error, Reason}
             when Connection :: connection(), Cmd :: iodata(), Data :: string(), Reason :: term().
```

# `cmd`

```erlang
-spec cmd(Connection, Cmd, Opts) -> {ok, Data} | {error, Reason}
             when
                 Connection :: connection(),
                 Cmd :: iodata(),
                 Opts :: [{timeout, Timeout} | newline_option()] | Timeout,
                 Timeout :: integer(),
                 Data :: string(),
                 Reason :: term().
```

Sends a command through Telnet and waits for prompt.

By default, this function adds "\\n" to the end of the specified command. If
this is not desired, use option `{newline,false}`. This is necessary, for
example, when sending Telnet command sequences prefixed with character Interpret
As Command (IAC). Option `{newline,string()}` can also be used if a different
line end than "\\n" is required, for instance `{newline,"\r\n"}`, to add both
carriage return and newline characters.

Option `timeout` specifies how long the client must wait for prompt. If the time
expires, the function returns `{error,timeout}`. For information about the
default value for the command timeout, see the
[list of default values](`m:ct_telnet#Default_values`) in the beginning of this
module.

# `cmdf`

```erlang
-spec cmdf(Connection, CmdFormat, Args) -> {ok, Data} | {error, Reason}
              when
                  Connection :: connection(),
                  CmdFormat :: io:format(),
                  Args :: [term()],
                  Data :: string(),
                  Reason :: term().
```

# `cmdf`

```erlang
-spec cmdf(Connection, CmdFormat, Args, Opts) -> {ok, Data} | {error, Reason}
              when
                  Connection :: connection(),
                  CmdFormat :: io:format(),
                  Args :: [term()],
                  Opts :: [{timeout, Timeout} | newline_option()] | Timeout,
                  Timeout :: integer(),
                  Data :: string(),
                  Reason :: term().
```

Sends a Telnet command and waits for prompt (uses a format string and a list of
arguments to build the command).

For details, see [`ct_telnet:cmd/3`](`cmd/3`).

# `expect`

```erlang
-spec expect(Connection, Patterns) ->
                {ok, Match} | {ok, Match, HaltReason} | {error, Reason} | no_return()
                when
                    Connection :: connection(),
                    Patterns :: Pattern | [Pattern],
                    Pattern :: PatternString | {Tag, PatternString},
                    Tag :: term(),
                    PatternString :: unicode:charlist(),
                    Match :: MatchResult | [MatchResult],
                    MatchResult ::
                        [CaptureData | {Tag, CaptureData}] | [[CaptureData | {Tag, CaptureData}]],
                    CaptureData ::
                        string() | {error, string(), binary()} | {incomplete, string(), binary()},
                    HaltReason :: MatchResult,
                    Reason :: term().
```

# `expect`

```erlang
-spec expect(Connection, Patterns, Opts) ->
                {ok, Match} | {ok, Match, HaltReason} | {error, Reason} | no_return()
                when
                    Connection :: connection(),
                    Patterns :: Pattern | [Pattern],
                    Pattern :: PatternString | {Tag, PatternString},
                    Tag :: term(),
                    PatternString :: unicode:charlist(),
                    Opts :: [Option],
                    Option ::
                        {idle_timeout, non_neg_integer()} |
                        {total_timeout, non_neg_integer()} |
                        ignore_prompt | no_prompt_check | wait_for_prompt | repeat |
                        {repeat, non_neg_integer()} |
                        sequence |
                        {halt, Patterns},
                    Match :: MatchResult | [MatchResult],
                    MatchResult ::
                        [CaptureData | {Tag, CaptureData}] | [[CaptureData | {Tag, CaptureData}]],
                    CaptureData ::
                        string() | {error, string(), binary()} | {incomplete, string(), binary()},
                    HaltReason :: MatchResult,
                    Reason :: term().
```

Gets data from Telnet and waits for the expected pattern.

`Pattern` can be a POSIX regular expression. The function returns when a pattern
is successfully matched (at least one, in the case of multiple patterns).

`RxMatch` is a list of matched strings. It looks as follows
`[FullMatch, SubMatch1, SubMatch2, ...]`, where `FullMatch` is the string
matched by the whole regular expression, and `SubMatchN` is the string that
matched subexpression number `N`. Subexpressions are denoted with `'(' ')'` in
the regular expression.

If a `Tag` is specified, the returned `Match` also includes the matched `Tag`.
Otherwise, only `RxMatch` is returned.

_Options:_

- **`idle_timeout`** - Indicates that the function must return if the Telnet
  client is idle (that is, if no data is received) for more than `IdleTimeout`
  milliseconds. Default time-out is 10 seconds.

- **`total_timeout`** - Sets a time limit for the complete `expect` operation.
  After `TotalTimeout` milliseconds, `{error,timeout}` is returned. Default is
  `infinity` (that is, no time limit).

- **`ignore_prompt | no_prompt_check`** - >The function returns when a prompt is
  received, even if no pattern has yet been matched, and
  `{error,{prompt,Prompt}}` is returned. However, this behavior can be modified
  with option `ignore_prompt` or option `no_prompt_check`, which tells `expect`
  to return only when a match is found or after a time-out.

- **`ignore_prompt`** - `ct_telnet` ignores any prompt found. This option is
  useful if data sent by the server can include a pattern matching prompt
  `regexp` (as returned by `TargedMod:get_prompt_regexp/0`), but is not to not
  cause the function to return.

- **`no_prompt_check`** - `ct_telnet` does not search for a prompt at all. This
  is useful if, for example, `Pattern` itself matches the prompt.

- **`wait_for_prompt`** - Forces `ct_telnet` to wait until the prompt string is
  received before returning (even if a pattern has already been matched). This
  is equal to calling
  [`expect(Conn, Patterns++[{prompt,Prompt}], [sequence|Opts])`](`expect/3`).
  Notice that option `idle_timeout` and `total_timeout` can abort the operation
  of waiting for prompt.

- **`repeat | repeat, N`** - The pattern(s) must be matched multiple times. If
  `N` is specified, the pattern(s) are matched `N` times, and the function
  returns `HaltReason = done`. This option can be interrupted by one or more
  `HaltPatterns`. `MatchList` is always returned, that is, a list of `Match`
  instead of only one `Match`. Also `HaltReason` is returned.

- **`sequence`** - All patterns must be matched in a sequence. A match is not
  concluded until all patterns are matched. This option can be interrupted by
  one or more `HaltPatterns`. `MatchList` is always returned, that is, a list of
  `Match` instead of only one `Match`. Also `HaltReason` is returned.

_Example 1:_

```erlang
expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],[sequence,{halt,[{nnn,"NNN"}]}])
```

First this tries to match `"ABC"`, and then `"XYZ"`, but if `"NNN"` appears, the
function returns `{error,{nnn,["NNN"]}}`. If both `"ABC"` and `"XYZ"` are
matched, the function returns `{ok,[AbcMatch,XyzMatch]}`.

_Example 2:_

```erlang
expect(Connection,[{abc,"ABC"},{xyz,"XYZ"}],[{repeat,2},{halt,[{nnn,"NNN"}]}])
```

This tries to match `"ABC"` or `"XYZ"` twice. If `"NNN"` appears, the function
returns `HaltReason = {nnn,["NNN"]}`.

Options `repeat` and `sequence` can be combined to match a sequence multiple
times.

# `get_data`

```erlang
-spec get_data(Connection) -> {ok, Data} | {error, Reason}
                  when Connection :: connection(), Data :: string(), Reason :: term().
```

Gets all data received by the Telnet client since the last command was sent.
Only newline-terminated strings are returned. If the last received string has
not yet been terminated, the connection can be polled automatically until the
string is complete.

The polling feature is controlled by the configuration values `poll_limit` and
`poll_interval` and is by default disabled. This means that the function
immediately returns all complete strings received and saves a remaining
non-terminated string for a later `get_data` call.

# `open`

```erlang
-spec open(Name) -> {ok, Handle} | {error, Reason}
              when Name :: atom(), Handle :: handle(), Reason :: term().
```

# `open`

```erlang
-spec open(Name, ConnType) -> {ok, Handle} | {error, Reason}
              when Name :: atom(), ConnType :: connection_type(), Handle :: handle(), Reason :: term().
```

Opens a Telnet connection to the specified target host.

# `open`

```erlang
-spec open(KeyOrName, ConnType, TargetMod) -> {ok, Handle} | {error, Reason}
              when
                  KeyOrName :: ct:key_or_name(),
                  ConnType :: connection_type(),
                  TargetMod :: module(),
                  Handle :: handle(),
                  Reason :: term().
```

# `open`

```erlang
-spec open(KeyOrName, ConnType, TargetMod, Extra) -> {ok, Handle} | {error, Reason}
              when
                  KeyOrName :: ct:key_or_name(),
                  ConnType :: connection_type(),
                  TargetMod :: module(),
                  Extra :: term(),
                  Handle :: handle(),
                  Reason :: term().
```

Opens a Telnet connection to the specified target host.

The target data must exist in a configuration file. The connection can be
associated with `Name` and/or the returned `Handle`. To allocate a name for the
target, use one of the following alternatives:

- `ct:require/2` in a test case
- A `require` statement in the suite information function (`suite/0`)
- A `require` statement in a test case information function

If you want the connection to be associated with `Handle` only (if you, for
example, need to open multiple connections to a host), use `Key`, the
configuration variable name, to specify the target. Notice that a connection
without an associated target name can only be closed with the `Handle` value.

`TargetMod` is a module that exports the functions
`connect(Ip, Port, KeepAlive, Extra)` and `get_prompt_regexp()` for the
specified `TargetType` (for example, `unix_telnet`).

See also `ct:require/2`.

# `send`

```erlang
-spec send(Connection, Cmd) -> ok | {error, Reason}
              when Connection :: connection(), Cmd :: iodata(), Reason :: term().
```

# `send`
*since OTP 17.4* 

```erlang
-spec send(Connection, Cmd, Opts) -> ok | {error, Reason}
              when
                  Connection :: connection(),
                  Cmd :: iodata(),
                  Opts :: [newline_option()],
                  Reason :: term().
```

Sends a Telnet command and returns immediately.

By default, this function adds "\\n" to the end of the specified command. If
this is not desired, option `{newline,false}` can be used. This is necessary,
for example, when sending Telnet command sequences prefixed with character
Interpret As Command (IAC). Option `{newline,string()}` can also be used if a
different line end than "\\n" is required, for instance `{newline,"\r\n"}`, to
add both carriage return and newline characters.

The resulting output from the command can be read with
[`ct_telnet:get_data/2`](`get_data/1`) or [`ct_telnet:expect/2,3`](`expect/2`).

# `sendf`

```erlang
-spec sendf(Connection, CmdFormat, Args) -> ok | {error, Reason}
               when
                   Connection :: connection(),
                   CmdFormat :: io:format(),
                   Args :: [term()],
                   Reason :: term().
```

# `sendf`
*since OTP 17.4* 

```erlang
-spec sendf(Connection, CmdFormat, Args, Opts) -> ok | {error, Reason}
               when
                   Connection :: connection(),
                   CmdFormat :: io:format(),
                   Args :: [term()],
                   Opts :: [newline_option()],
                   Reason :: term().
```

Sends a Telnet command and returns immediately (uses a format string and a list
of arguments to build the command).

For details, see [`ct_telnet:send/3`](`send/3`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
