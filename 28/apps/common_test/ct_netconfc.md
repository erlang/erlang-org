# `ct_netconfc`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/common_test/src/ct_netconfc.erl#L64)

NETCONF client module.

NETCONF client module compliant with RFC 6241, NETCONF Configuration Protocol,
and RFC 6242, Using the NETCONF Configuration Protocol over Secure SHell (SSH),
and with support for RFC 5277, NETCONF Event Notifications.

[](){: #Connecting }

_Connecting to a NETCONF server_

Call [`connect/1,2`](`connect/1`) to establish a connection to a server, then
pass the returned handle to [`session/1-3`](`session/1`) to establish a NETCONF
session on a new SSH channel. Each call to [`session/1-3`](`session/1`)
establishes a new session on the same connection, and results in a hello message
to the server.

Alternately, [`open/1,2`](`open/1`) can be used to establish a single session on
a dedicated connection. (Or, equivalently, [`only_open/1,2`](`only_open/1`)
followed by [`hello/1-3`](`hello/1`).)

Connect/session options can be specified in a configuration file with entries
like the following.

```erlang
{server_id(), [option()]}.
```

The `t:server_id/0` or an associated `t:ct:target_name/0` can then be passed to
the aforementioned functions to use the referenced configuration.

[](){: #Signaling }

_Signaling_

Protocol operations in the NETCONF protocol are realized as remote procedure
calls (RPCs) from client to server and a corresponding reply from server to
client. RPCs are sent using like-named functions (eg.
[`edit_config/3-5`](`edit_config/3`) to send an edit-config RPC), with the
server reply as return value. There are functions for each RPC defined in RFC
6241 and the create-subscription RPC from RFC 5277, all of which are wrappers on
[`send_rpc/2,3`](`send_rpc/2`), that can be used to send an arbitrary RPC not
defined in RFC 6241 or RFC 5277.

All of the signaling functions have one variant with a `Timeout` argument and
one without, corresponding to an infinite timeout. The latter is inappropriate
in most cases since a non-response by the server or a missing message-id causes
the call to hang indefinitely.

[](){: #Logging }

_Logging_

The NETCONF server uses `error_logger` for logging of NETCONF traffic. A special
purpose error handler is implemented in `ct_conn_log_h`. To use this error
handler, add the `cth_conn_log` hook in the test suite, for example:

```erlang
suite() ->
    [{ct_hooks, [{cth_conn_log, [{ct:conn_log_mod(), ct:conn_log_options()}]}]}].
```

`conn_log_mod()` is the name of the `Common Test` module implementing the
connection protocol, for example, `ct_netconfc`.

Hook option `log_type` specifies the type of logging:

- **`raw`** - The sent and received NETCONF data is logged to a separate text
  file "as is" without any formatting. A link to the file is added to the test
  case HTML log.

- **`pretty`** - The sent and received NETCONF data is logged to a separate text
  file with XML data nicely indented. A link to the file is added to the test
  case HTML log.

- **`html (default)`** - The sent and received NETCONF traffic is pretty printed
  directly in the test case HTML log.

- **`silent`** - NETCONF traffic is not logged.

By default, all NETCONF traffic is logged in one single log file. However,
different connections can be logged in separate files. To do this, use hook
option `hosts` and list the names of the servers/connections to be used in the
suite. The connections must be named for this to work, that is, they must be
opened with `open/2`.

Option `hosts` has no effect if `log_type` is set to `html` or `silent`.

The hook options can also be specified in a configuration file with
configuration variable `ct_conn_log`:

```erlang
{ct_conn_log,[{ct:conn_log_mod(), ct:conn_log_options()}]}.
```

For example:

```erlang
{ct_conn_log,[{ct_netconfc,[{log_type,pretty},
                            {hosts,[ct:key_or_name()]}]}]}
```

> #### Note {: .info }
>
> Hook options specified in a configuration file overwrite the hard-coded hook
> options in the test suite.

_Logging Example 1:_

[](){: #Logging_example_1 }

The following `ct_hooks` statement causes pretty printing of NETCONF traffic to
separate logs for the connections named `nc_server1` and `nc_server2`. Any other
connections are logged to default NETCONF log.

```erlang
suite() ->
   [{ct_hooks, [{cth_conn_log, [{ct_netconfc,[{log_type,pretty}},
                                              {hosts,[nc_server1,nc_server2]}]}
                               ]}]}].
```

Connections must be opened as follows:

```erlang
open(nc_server1,[...]),
open(nc_server2,[...]).
```

_Logging Example 2:_

[](){: #Logging_example_2 }

The following configuration file causes raw logging of all NETCONF traffic in to
one single text file:

```erlang
{ct_conn_log,[{ct_netconfc,[{log_type,raw}]}]}.
```

The `ct_hooks` statement must look as follows:

```erlang
suite() ->
    [{ct_hooks, [{cth_conn_log, []}]}].
```

The same `ct_hooks` statement without the configuration file would cause HTML
logging of all NETCONF connections in to the test case HTML log.

# `client`
*since OTP R15B02* 

```elixir
-type client() :: handle() | server_id() | ct:target_name().
```

Handle to a NETCONF session, as required by signaling functions.

# `error_reason`
*not exported* *since OTP R15B02* 

```elixir
-type error_reason() :: term().
```

# `handle`
*since OTP R15B02* 

```elixir
-opaque handle() :: pid().
```

Handle to a connection to a NETCONF server as returned by
[`connect/1,2`](`connect/1`), or to a session as returned by
[`session/1-3`](`session/1`), [`open/1,2`](`open/1`), or
[`only_open/1,2`](`only_open/1`).

# `host`
*not exported* *since OTP R15B02* 

```elixir
-type host() :: inet:hostname() | inet:ip_address().
```

# `netconf_db`
*not exported* *since OTP R15B02* 

```elixir
-type netconf_db() :: running | startup | candidate.
```

# `notification`
*since OTP R15B02* 

```elixir
-type notification() :: {notification, xml_attributes(), [simple_xml()]}.
```

Event notification messages sent as a result of calls to
[`create_subscription/2,3`](`create_subscription/2`).

# `option`
*not exported* *since OTP R15B02* 

```elixir
-type option() ::
          {host | ssh, host()} |
          {port, inet:port_number()} |
          {timeout, timeout()} |
          {capability, string() | [string()]} |
          {receiver, term()} |
          ssh:client_option().
```

Options `host` and `port` specify the server endpoint to which to connect, and
are passed directly to [`ssh:connect/4`](`ssh:connect/3`), as are arbitrary ssh
options. Common options are `user`, `password` and `user_dir`.

Option `timeout` specifies the number of milliseconds to allow for connection
establishment and, if the function in question results in an outgoing hello
message, reception of the server hello. The timeout applies to connection and
hello independently; one timeout for connection establishment, another for hello
reception.

Option `receiver` specifies a destination for incoming notification messages; a
left operand of the send operator (`!`). If not specified then a process calling
[`create_subscription/2,3`](`create_subscription/2`) becomes the receiver, but
explicitly setting a receiver makes it possible to receive notifications that
are not ordered by calling this function. Multiple receiver options can be
specified.

Receiver options are ignored by connect/1-3.

Option `capability` specifies the content of a corresponding element in an
outgoing hello message, each option specifying the content of a single element.
If no base NETCONF capability is configured then the RFC 4741 1.0 capability,
"urn:ietf:params:netconf:base:1.0", is added, otherwise not. In particular, the
RFC 6241 1.1 capability must be explicitly configured. NETCONF capabilities can
be specified using the shorthand notation defined in RFC 6241, any capability
string starting with a colon being prefixed by either "urn:ietf:params:netconf"
or "urn:ietf:params:netconf:capability", as appropriate.

Capability options are ignored by connect/1-3 and only_open/1-2, which don't
result in an outgoing hello message.

# `server_id`
*not exported* *since OTP R15B02* 

```elixir
-type server_id() :: atom().
```

Identity of connection or session configuration in a configuration file.

# `session_option`
*not exported* *since OTP R15B02* 

```elixir
-type session_option() ::
          {timeout, timeout()} | {receiver, term()} | {capability, string() | [string()]}.
```

# `simple_xml`
*not exported* *since OTP R15B02* 

```elixir
-type simple_xml() ::
          {xml_tag(), xml_attributes(), xml_content()} | {xml_tag(), xml_content()} | xml_tag().
```

Representation of XML, as described in application
[`xmerl`](`e:xmerl:index.html`).

# `stream_data`
*not exported* *since OTP R15B02* 

```elixir
-type stream_data() ::
          {description, string()} |
          {replaySupport, string()} |
          {replayLogCreationTime, string()} |
          {replayLogAgedTime, string()}.
```

# `stream_name`
*not exported* *since OTP R15B02* 

```elixir
-type stream_name() :: string().
```

# `streams`
*not exported* *since OTP R15B02* 

```elixir
-type streams() :: [{stream_name(), [stream_data()]}].
```

Stream information as returned by
[`get_event_streams/1-3`](`get_event_streams/1`). See RFC 5277, "XML Schema for
Event Notifications", for detail on the format of the string values.

# `xml_attribute_tag`
*not exported* *since OTP R15B02* 

```elixir
-type xml_attribute_tag() :: atom().
```

# `xml_attribute_value`
*not exported* *since OTP R15B02* 

```elixir
-type xml_attribute_value() :: string().
```

# `xml_attributes`
*not exported* *since OTP R15B02* 

```elixir
-type xml_attributes() :: [{xml_attribute_tag(), xml_attribute_value()}].
```

# `xml_content`
*not exported* *since OTP R15B02* 

```elixir
-type xml_content() :: [simple_xml() | iolist()].
```

# `xml_tag`
*not exported* *since OTP R15B02* 

```elixir
-type xml_tag() :: atom().
```

# `xpath`
*not exported* *since OTP R15B02* 

```elixir
-type xpath() :: {xpath, string()}.
```

# `xs_datetime`
*not exported* *since OTP R15B02* 

```elixir
-type xs_datetime() :: string().
```

Date and time of a startTime/stopTime element in an RFC 5277 create-subscription
request. Of XML primitive type `dateTime`, which has the (informal) form

```text
[-]YYYY-MM-DDThh:mm:ss[.s][Z|(+|-)hh:mm]
```

where `T` and `Z` are literal and `.s` is one or more fractional seconds.

# `action`
*since OTP R15B02* 

```elixir
-spec action(Client, Action) -> Result
                when
                    Client :: client(),
                    Action :: simple_xml(),
                    Result :: ok | {ok, [simple_xml()]} | {error, error_reason()}.
```

# `action`
*since OTP R15B02* 

```elixir
-spec action(Client, Action, Timeout) -> Result
                when
                    Client :: client(),
                    Action :: simple_xml(),
                    Timeout :: timeout(),
                    Result :: ok | {ok, [simple_xml()]} | {error, error_reason()}.
```

Executes an action. If the return type is void, `ok` is returned instead of
`{ok,[simple_xml()]}`.

# `close_session`
*since OTP R15B02* 

```elixir
-spec close_session(Client) -> Result when Client :: client(), Result :: ok | {error, error_reason()}.
```

# `close_session`
*since OTP R15B02* 

```elixir
-spec close_session(Client, Timeout) -> Result
                       when
                           Client :: client(),
                           Timeout :: timeout(),
                           Result :: ok | {error, error_reason()}.
```

Requests graceful termination of the session associated with the client.

When a NETCONF server receives a `close-session` request, it gracefully closes
the session. The server releases any locks and resources associated with the
session and gracefully closes any associated connections. Any NETCONF requests
received after a `close-session` request are ignored.

# `connect`
*since OTP 20.0* 

```elixir
-spec connect(Options) -> Result
                 when Options :: [option()], Result :: {ok, handle()} | {error, error_reason()}.
```

Opens an SSH connection to a NETCONF server.

If the server options are specified in a configuration file, use `connect/2`
instead.

The opaque `t:handle/0` reference returned from this function is required as
connection identifier when opening sessions over this connection, see
[`session/1-3`](`session/1`).

# `connect`
*since OTP 20.0* 

```elixir
-spec connect(KeyOrName, ExtraOptions) -> Result
                 when
                     KeyOrName :: ct:key_or_name(),
                     ExtraOptions :: [option()],
                     Result :: {ok, handle()} | {error, error_reason()}.
```

Open an SSH connection to a named NETCONF server.

If `KeyOrName` is a configured `t:server_id/0` or a `target_name()` associated
with such an Id, then the options for this server are fetched from the
configuration file.

The options list is added to those of the configuration file. If an option is
specified in both lists, the configuration file takes precedence.

If the server is not specified in a configuration file, use `connect/1` instead.

The opaque `t:handle/0` reference returned from this function can be used as
connection identifier when opening sessions over this connection, see
[`session/1-3`](`session/1`). However, if `KeyOrName` is a `target_name()`, that
is, if the server is named through a call to `ct:require/2` or a `require`
statement in the test suite, then this name can be used instead of `t:handle/0`.

# `copy_config`
*since OTP R15B02* 

```elixir
-spec copy_config(Client, Target, Source) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Source :: netconf_db(),
                         Result :: ok | {error, error_reason()}.
```

# `copy_config`
*since OTP R15B02* 

```elixir
-spec copy_config(Client, Target, Source, Timeout) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Source :: netconf_db(),
                         Timeout :: timeout(),
                         Result :: ok | {error, error_reason()}.
```

Copies configuration data.

Which source and target options that can be issued depends on the capabilities
supported by the server. That is, `:candidate` and/or `:startup` are required.

# `create_subscription`
*since OTP 22.1* 

```elixir
-spec create_subscription(Client, Values) -> Result
                             when
                                 Client :: client(),
                                 Values ::
                                     #{stream => Stream,
                                       filter => Filter,
                                       start => StartTime,
                                       stop => StopTime},
                                 Stream :: stream_name(),
                                 Filter :: simple_xml() | [simple_xml()],
                                 StartTime :: xs_datetime(),
                                 StopTime :: xs_datetime(),
                                 Result :: ok | {error, error_reason()};
                         (Client, list() | timeout()) -> Result
                             when Client :: client(), Result :: ok | {error, error_reason()}.
```

# `create_subscription`
*since OTP 22.1* 

```elixir
-spec create_subscription(Client, Values, Timeout) -> Result
                             when
                                 Client :: client(),
                                 Values ::
                                     #{stream => Stream,
                                       filter => Filter,
                                       start => StartTime,
                                       stop => StopTime},
                                 Stream :: stream_name(),
                                 Filter :: simple_xml() | [simple_xml()],
                                 StartTime :: xs_datetime(),
                                 StopTime :: xs_datetime(),
                                 Timeout :: timeout(),
                                 Result :: ok | {error, error_reason()};
                         (Client, list(), list() | timeout()) -> Result
                             when Client :: client(), Result :: ok | {error, error_reason()}.
```

Creates a subscription for event notifications by sending an RFC 5277
create-subscription RPC to the server. The calling process receives events as
messages of type `t:notification/0`.

From RFC 5722, 2.1 Subscribing to Receive Event Notifications:

- **`Stream`** - Indicates which stream of event is of interest. If not present,
  events in the default NETCONF stream are sent.

- **`Filter`** - Indicates which subset of all possible events is of interest.
  The parameter format is the same as that of the filter parameter in the
  NETCONF protocol operations. If not present, all events not precluded by other
  parameters are sent.

- **`StartTime`** - Used to trigger the replay feature and indicate that the
  replay is to start at the time specified. If `StartTime` is not present, this
  is not a replay subscription. It is not valid to specify start times that are
  later than the current time. If `StartTime` is specified earlier than the log
  can support, the replay begins with the earliest available notification. This
  parameter is of type `dateTime` and compliant to RFC 3339. Implementations
  must support time zones.

- **`StopTime`** - Used with the optional replay feature to indicate the newest
  notifications of interest. If `StopTime` is not present, the notifications
  continues until the subscription is terminated. Must be used with and be later
  than `StartTime`. Values of `StopTime` in the future are valid. This parameter
  is of type `dateTime` and compliant to RFC 3339. Implementations must support
  time zones.

See RFC 5277 for more details. The requirement that `StopTime` must only be used
with `StartTime` is not enforced, to allow an invalid request to be sent to the
server.

Prior to OTP 22.1, this function was documented as having 15 variants in 6
arities. These are still exported for backwards compatibility, but no longer
documented. The map-based variants documented above provide the same
functionality with simpler arguments.

> #### Note {: .info }
>
> create-subscription is no longer the only RPC with which NETCONF notifications
> can be ordered: RFC 8639 adds establish-subscription and future RFCs may add
> other methods. Specify a `receiver` option at session creation to provide a
> destination for incoming notifications independently of a call to
> [`create_subscription/2,3`](`create_subscription/2`), and use
> [`send_rpc/2,3`](`send_rpc/2`) to send establish-subscription and other
> arbitrary RPCs.

# `delete_config`
*since OTP R15B02* 

```elixir
-spec delete_config(Client, Target) -> Result
                       when
                           Client :: client(),
                           Target :: startup | candidate,
                           Result :: ok | {error, error_reason()}.
```

# `delete_config`
*since OTP R15B02* 

```elixir
-spec delete_config(Client, Target, Timeout) -> Result
                       when
                           Client :: client(),
                           Target :: startup | candidate,
                           Timeout :: timeout(),
                           Result :: ok | {error, error_reason()}.
```

Deletes configuration data.

The running configuration cannot be deleted and `:candidate` or `:startup` must
be advertised by the server.

# `disconnect`
*since OTP 20.0* 

```elixir
-spec disconnect(Conn) -> ok | {error, error_reason()} when Conn :: handle().
```

Closes the given SSH connection.

If there are open NETCONF sessions on the connection, these will be brutally
aborted. To avoid this, close each session with
[`close_session/1,2`](`close_session/1`)

# `edit_config`
*since OTP R15B02* 

```elixir
-spec edit_config(Client, Target, Config) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Config :: simple_xml() | [simple_xml()],
                         Result :: ok | {error, error_reason()}.
```

# `edit_config`
*since OTP R15B02* 

```elixir
-spec edit_config(Client, Target, Config, OptParams) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Config :: simple_xml() | [simple_xml()],
                         OptParams :: [simple_xml()],
                         Result :: ok | {error, error_reason()};
                 (Client, Target, Config, Timeout) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Config :: simple_xml(),
                         Timeout :: timeout(),
                         Result :: ok | {error, error_reason()}.
```

# `edit_config`
*since OTP 18.0* 

```elixir
-spec edit_config(Client, Target, Config, OptParams, Timeout) -> Result
                     when
                         Client :: client(),
                         Target :: netconf_db(),
                         Config :: simple_xml() | [simple_xml()],
                         OptParams :: [simple_xml()],
                         Timeout :: timeout(),
                         Result :: ok | {error, error_reason()}.
```

Edits configuration data.

By default only the running target is available, unless the server includes
`:candidate` or `:startup` in its list of capabilities.

`OptParams` can be used for specifying optional parameters (`default-operation`,
`test-option`, or `error-option`) to be added to the `edit-config` request. The
value must be a list containing valid simple XML, for example:

```erlang
[{'default-operation', ["none"]},
 {'error-option', ["rollback-on-error"]}]
```

If `OptParams` is not given, the default value `[]` is used.

# `get`
*since OTP R15B02* 

```elixir
-spec get(Client, Filter) -> Result
             when
                 Client :: client(),
                 Filter :: simple_xml() | xpath(),
                 Result :: {ok, [simple_xml()]} | {error, error_reason()}.
```

# `get`
*since OTP R15B02* 

```elixir
-spec get(Client, Filter, Timeout) -> Result
             when
                 Client :: client(),
                 Filter :: simple_xml() | xpath(),
                 Timeout :: timeout(),
                 Result :: {ok, [simple_xml()]} | {error, error_reason()}.
```

Gets data.

This operation returns both configuration and state data from the server.

Filter type `xpath` can be used only if the server supports `:xpath`.

# `get_capabilities`
*since OTP R15B02* 

```elixir
-spec get_capabilities(Client) -> Result
                          when Client :: client(), Result :: [string()] | {error, error_reason()}.
```

# `get_capabilities`
*since OTP R15B02* 

```elixir
-spec get_capabilities(Client, Timeout) -> Result
                          when
                              Client :: client(),
                              Timeout :: timeout(),
                              Result :: [string()] | {error, error_reason()}.
```

Returns the server capabilities as received in its hello message.

# `get_config`
*since OTP R15B02* 

```elixir
-spec get_config(Client, Source, Filter) -> Result
                    when
                        Client :: client(),
                        Source :: netconf_db(),
                        Filter :: simple_xml() | xpath(),
                        Result :: {ok, [simple_xml()]} | {error, error_reason()}.
```

# `get_config`
*since OTP R15B02* 

```elixir
-spec get_config(Client, Source, Filter, Timeout) -> Result
                    when
                        Client :: client(),
                        Source :: netconf_db(),
                        Filter :: simple_xml() | xpath(),
                        Timeout :: timeout(),
                        Result :: {ok, [simple_xml()]} | {error, error_reason()}.
```

Gets configuration data.

To be able to access another source than `running`, the server must advertise
`:candidate` and/or `:startup`.

Filter type `xpath` can be used only if the server supports `:xpath`.

# `get_event_streams`
*since OTP 20.0* 

```elixir
-spec get_event_streams(Client) -> Result
                           when Client :: client(), Result :: {ok, streams()} | {error, error_reason()}.
```

# `get_event_streams`
*since OTP R15B02* 

```elixir
-spec get_event_streams(Client, Timeout) -> Result
                           when
                               Client :: client(),
                               Timeout :: timeout(),
                               Result :: {ok, streams()} | {error, error_reason()};
                       (Client, Streams) -> Result
                           when
                               Client :: client(),
                               Streams :: [stream_name()],
                               Result :: {ok, streams()} | {error, error_reason()}.
```

# `get_event_streams`
*since OTP R15B02* 

```elixir
-spec get_event_streams(Client, Streams, Timeout) -> Result
                           when
                               Client :: client(),
                               Streams :: [stream_name()],
                               Timeout :: timeout(),
                               Result :: {ok, streams()} | {error, error_reason()}.
```

Sends a request to get the specified event streams.

`Streams` is a list of stream names. The following filter is sent to the NETCONF
server in a `get` request:

```text
<netconf xmlns="urn:ietf:params:xml:ns:netmod:notification">
  <streams>
    <stream>
      <name>StreamName1</name>
    </stream>
    <stream>
      <name>StreamName2</name>
    </stream>
    ...
  </streams>
</netconf>
```

If `Streams` is an empty list, _all_ streams are requested by sending the
following filter:

```text
<netconf xmlns="urn:ietf:params:xml:ns:netmod:notification">
  <streams/>
</netconf>
```

If more complex filtering is needed, use [`ct_netconfc:get/2,3`](`get/2`) and
specify the exact filter according to "XML Schema for Event Notifications" in
RFC 5277.

# `get_session_id`
*since OTP R15B02* 

```elixir
-spec get_session_id(Client) -> Result
                        when Client :: client(), Result :: pos_integer() | {error, error_reason()}.
```

# `get_session_id`
*since OTP R15B02* 

```elixir
-spec get_session_id(Client, Timeout) -> Result
                        when
                            Client :: client(),
                            Timeout :: timeout(),
                            Result :: pos_integer() | {error, error_reason()}.
```

Returns the session Id associated with the specified client.

# `hello`
*since OTP R15B02* 

```elixir
-spec hello(Client) -> Result when Client :: handle(), Result :: ok | {error, error_reason()}.
```

# `hello`
*since OTP R15B02* 

```elixir
-spec hello(Client, Timeout) -> Result
               when Client :: handle(), Timeout :: timeout(), Result :: ok | {error, error_reason()}.
```

# `hello`
*since OTP 17.5.3* 

```elixir
-spec hello(Client, Options, Timeout) -> Result
               when
                   Client :: handle(),
                   Options :: [{capability, [string()]}],
                   Timeout :: timeout(),
                   Result :: ok | {error, error_reason()}.
```

Exchanges `hello` messages with the server. Returns when the server hello has
been received or after the specified timeout.

Note that capabilities for an outgoing hello can be passed directly to `open/2`.

# `kill_session`
*since OTP R15B02* 

```elixir
-spec kill_session(Client, SessionId) -> Result
                      when
                          Client :: client(),
                          SessionId :: pos_integer(),
                          Result :: ok | {error, error_reason()}.
```

# `kill_session`
*since OTP R15B02* 

```elixir
-spec kill_session(Client, SessionId, Timeout) -> Result
                      when
                          Client :: client(),
                          SessionId :: pos_integer(),
                          Timeout :: timeout(),
                          Result :: ok | {error, error_reason()}.
```

Forces termination of the session associated with the supplied session Id.

The server side must abort any ongoing operations, release any locks and
resources associated with the session, and close any associated connections.

Only if the server is in the confirmed commit phase, the configuration is
restored to its state before entering the confirmed commit phase. Otherwise, no
configuration rollback is performed.

If the specified `SessionId` is equal to the current session Id, an error is
returned.

# `lock`
*since OTP R15B02* 

```elixir
-spec lock(Client, Target) -> Result
              when Client :: client(), Target :: netconf_db(), Result :: ok | {error, error_reason()}.
```

# `lock`
*since OTP R15B02* 

```elixir
-spec lock(Client, Target, Timeout) -> Result
              when
                  Client :: client(),
                  Target :: netconf_db(),
                  Timeout :: timeout(),
                  Result :: ok | {error, error_reason()}.
```

Locks the configuration target.

Which target parameters that can be used depends on if `:candidate` and/or
`:startup` are supported by the server. If successful, the configuration system
of the device is unavailable to other clients (NETCONF, CORBA, SNMP, and so on).
Locks are intended to be short-lived.

Operation [`kill_session/2,3`](`kill_session/2`) can be used to force the
release of a lock owned by another NETCONF session. How this is achieved by the
server side is implementation-specific.

# `only_open`
*since OTP R15B02* 

```elixir
-spec only_open(Options) -> Result
                   when Options :: [option()], Result :: {ok, handle()} | {error, error_reason()}.
```

Opens a NETCONF session, but does not send `hello`.

As `open/1`, but does not send a `hello` message.

# `only_open`
*since OTP R15B02* 

```elixir
-spec only_open(KeyOrName, ExtraOptions) -> Result
                   when
                       KeyOrName :: ct:key_or_name(),
                       ExtraOptions :: [option()],
                       Result :: {ok, handle()} | {error, error_reason()}.
```

Opens a named NETCONF session, but does not send `hello`.

As `open/2`, but does not send a `hello` message.

# `open`
*since OTP R15B02* 

```elixir
-spec open(Options) -> Result
              when Options :: [option()], Result :: {ok, handle()} | {error, error_reason()}.
```

Opens a NETCONF session and exchanges `hello` messages.

If the server options are specified in a configuration file, or if a named
client is needed for logging purposes (see section
[Logging](`m:ct_netconfc#Logging`) in this module), use `open/2` instead.

The opaque `t:handle/0` reference returned from this function is required as
client identifier when calling any other function in this module.

# `open`
*since OTP R15B02* 

```elixir
-spec open(KeyOrName, ExtraOption) -> Result
              when
                  KeyOrName :: ct:key_or_name(),
                  ExtraOption :: [option()],
                  Result :: {ok, handle()} | {error, error_reason()}.
```

Opens a named NETCONF session and exchanges `hello` messages.

If `KeyOrName` is a configured `t:server_id/0` or a `target_name()` associated
with such an Id, then the options for this server are fetched from the
configuration file.

The options list is added to those of the configuration file. If an option is
specified in both lists, the configuration file take precedence.

If the server is not specified in a configuration file, use `open/1` instead.

The opaque `t:handle/0` reference returned from this function can be used as
client identifier when calling any other function in this module. However, if
`KeyOrName` is a `target_name()`, that is, if the server is named through a call
to `ct:require/2` or a `require` statement in the test suite, then this name can
be used instead of `t:handle/0`.

See also `ct:require/2`.

# `send`
*since OTP R16B02* 

```elixir
-spec send(Client, SimpleXml) -> Result
              when
                  Client :: client(),
                  SimpleXml :: simple_xml(),
                  Result :: simple_xml() | {error, error_reason()}.
```

# `send`
*since OTP R16B02* 

```elixir
-spec send(Client, SimpleXml, Timeout) -> Result
              when
                  Client :: client(),
                  SimpleXml :: simple_xml(),
                  Timeout :: timeout(),
                  Result :: simple_xml() | {error, error_reason()}.
```

Sends an XML document to the server.

The specified XML document is sent "as is" to the server. This function can be
used for sending XML documents that cannot be expressed by other interface
functions in this module.

# `send_rpc`
*since OTP R16B02* 

```elixir
-spec send_rpc(Client, SimpleXml) -> Result
                  when
                      Client :: client(),
                      SimpleXml :: simple_xml(),
                      Result :: [simple_xml()] | {error, error_reason()}.
```

# `send_rpc`
*since OTP R16B02* 

```elixir
-spec send_rpc(Client, SimpleXml, Timeout) -> Result
                  when
                      Client :: client(),
                      SimpleXml :: simple_xml(),
                      Timeout :: timeout(),
                      Result :: [simple_xml()] | {error, error_reason()}.
```

Sends a NETCONF `rpc` request to the server.

The specified XML document is wrapped in a valid NETCONF `rpc` request and sent
to the server. The `message-id` and namespace attributes are added to element
`rpc`.

This function can be used for sending `rpc` requests that cannot be expressed by
other interface functions in this module.

# `session`
*since OTP 20.0* 

```elixir
-spec session(Conn) -> Result when Conn :: handle(), Result :: {ok, handle()} | {error, error_reason()}.
```

# `session`
*since OTP 20.0* 

```elixir
-spec session(Conn, Options) -> Result
                 when
                     Conn :: handle(),
                     Options :: [session_option()],
                     Result :: {ok, handle()} | {error, error_reason()};
             (KeyOrName, Conn) -> Result
                 when
                     KeyOrName :: ct:key_or_name(),
                     Conn :: handle(),
                     Result :: {ok, handle()} | {error, error_reason()}.
```

# `session`
*since OTP 20.0* 

```elixir
-spec session(KeyOrName, Conn, Options) -> Result
                 when
                     Conn :: handle(),
                     Options :: [session_option()],
                     KeyOrName :: ct:key_or_name(),
                     Result :: {ok, handle()} | {error, error_reason()}.
```

Opens a NETCONF session as a channel on the given SSH connection, and exchanges
hello messages with the server.

The opaque `t:handle/0` reference returned from this function can be used as
client identifier when calling any other function in this module. However, if
`KeyOrName` is used and it is a `target_name()`, that is, if the server is named
through a call to `ct:require/2` or a `require` statement in the test suite,
then this name can be used instead of `t:handle/0`.

# `unlock`
*since OTP R15B02* 

```elixir
-spec unlock(Client, Target) -> Result
                when Client :: client(), Target :: netconf_db(), Result :: ok | {error, error_reason()}.
```

# `unlock`
*since OTP R15B02* 

```elixir
-spec unlock(Client, Target, Timeout) -> Result
                when
                    Client :: client(),
                    Target :: netconf_db(),
                    Timeout :: timeout(),
                    Result :: ok | {error, error_reason()}.
```

Unlocks the configuration target.

If the client earlier has acquired a lock through [`lock/2,3`](`lock/2`), this
operation releases the associated lock. To access another target than `running`,
the server must support `:candidate` and/or `:startup`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
