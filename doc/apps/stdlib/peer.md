# `peer`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/peer.erl#L48)

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2022-2026. All Rights Reserved.
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
Start and control linked Erlang nodes.

This module provides functions for starting linked Erlang nodes. The node
spawning new nodes is called _origin_, and newly started nodes are _peer_ nodes,
or peers. A peer node automatically terminates when it loses the _control
connection_ to the origin. This connection could be an Erlang distribution
connection, or an alternative - TCP or standard I/O. The alternative connection
provides a way to execute remote procedure calls even when Erlang Distribution
is not available, allowing to test the distribution itself.

Peer node terminal input/output is relayed through the origin. If a standard I/O
alternative connection is requested, console output also goes via the origin,
allowing debugging of node startup and boot script execution (see
[`-init_debug`](`e:erts:erl_cmd.md#init_debug`)). File I/O is not redirected,
contrary to `m:slave` behaviour.

The peer node can start on the same or a different host (via `ssh`) or in a
separate container (for example Docker). When the peer starts on the same host
as the origin, it inherits the current directory and environment variables from
the origin.

> #### Note {: .info }
>
> This module is designed to facilitate multi-node testing with Common Test. Use
> the `?CT_PEER()` macro to start a linked peer node according to Common Test
> conventions: crash dumps written to specific location, node name prefixed with
> module name, calling function, and origin OS process ID). Use `random_name/1`
> to create sufficiently unique node names if you need more control.
>
> A peer node started without alternative connection behaves similarly to
> `m:slave`. When an alternative connection is requested, the behaviour is
> similar to `test_server:start_node(Name, peer, Args).`

## Example

The following example implements a test suite starting extra Erlang nodes. It
employs a number of techniques to speed up testing and reliably shut down peer
nodes:

- peers start linked to test runner process. If the test case fails, the peer
  node is stopped automatically, leaving no rogue nodes running in the
  background
- arguments used to start the peer are saved in the control process state for
  manual analysis. If the test case fails, the CRASH REPORT contains these
  arguments
- multiple test cases can run concurrently speeding up overall testing process,
  peer node names are unique even when there are multiple instances of the same
  test suite running in parallel

```erlang
-module(my_SUITE).
-behaviour(ct_suite).
-export([all/0, groups/0]).
-export([basic/1, args/1, named/1, restart_node/1, multi_node/1]).

-include_lib("common_test/include/ct.hrl").

groups() ->
    [{quick, [parallel],
        [basic, args, named, restart_node, multi_node]}].

all() ->
    [{group, quick}].

basic(Config) when is_list(Config) ->
    {ok, Peer, _Node} = ?CT_PEER(),
    peer:stop(Peer).

args(Config) when is_list(Config) ->
    %% specify additional arguments to the new node
    {ok, Peer, _Node} = ?CT_PEER(["-emu_flavor", "smp"]),
    peer:stop(Peer).

named(Config) when is_list(Config) ->
    %% pass test case name down to function starting nodes
    Peer = start_node_impl(named_test),
    peer:stop(Peer).

start_node_impl(ActualTestCase) ->
    {ok, Peer, Node} = ?CT_PEER(#{name => ?CT_PEER_NAME(ActualTestCase)}),
    %% extra setup needed for multiple test cases
    ok = rpc:call(Node, application, set_env, [kernel, key, value]),
    Peer.

restart_node(Config) when is_list(Config) ->
    Name = ?CT_PEER_NAME(),
    {ok, Peer, Node} = ?CT_PEER(#{name => Name}),
    peer:stop(Peer),
    %% restart the node with the same name as before
    {ok, Peer2, Node} = ?CT_PEER(#{name => Name, args => ["+fnl"]}),
    peer:stop(Peer2).
```

The next example demonstrates how to start multiple nodes concurrently:

```erlang
multi_node(Config) when is_list(Config) ->
    Peers = [?CT_PEER(#{wait_boot => {self(), tag}})
        || _ <- lists:seq(1, 4)],
    %% wait for all nodes to complete boot process, get their names:
    _Nodes = [receive {tag, {started, Node, Peer}} -> Node end
        || {ok, Peer} <- Peers],
    [peer:stop(Peer) || {ok, Peer} <- Peers].
```

Start a peer on a different host. Requires `ssh` key-based authentication set
up, allowing "another_host" connection without password prompt.

```erlang
Ssh = os:find_executable("ssh"),
peer:start_link(#{exec => {Ssh, ["another_host", "erl"]},
    connection => standard_io}),
```

The following Common Test case demonstrates Docker integration, starting two
containers with hostnames "one" and "two". In this example Erlang nodes running
inside containers form an Erlang cluster.

```erlang
docker(Config) when is_list(Config) ->
    Docker = os:find_executable("docker"),
    PrivDir = proplists:get_value(priv_dir, Config),
    build_release(PrivDir),
    build_image(PrivDir),

    %% start two Docker containers
    {ok, Peer, Node} = peer:start_link(#{name => lambda,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "one", "-i", "lambda"]}}),
    {ok, Peer2, Node2} = peer:start_link(#{name => lambda,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "two", "-i", "lambda"]}}),

    %% find IP address of the second node using alternative connection RPC
    {ok, Ips} = peer:call(Peer2, inet, getifaddrs, []),
    {"eth0", Eth0} = lists:keyfind("eth0", 1, Ips),
    {addr, Ip} = lists:keyfind(addr, 1, Eth0),

    %% make first node to discover second one
    ok = peer:call(Peer, inet_db, set_lookup, [[file]]),
    ok = peer:call(Peer, inet_db, add_host, [Ip, ["two"]]),

    %% join a cluster
    true = peer:call(Peer, net_kernel, connect_node, [Node2]),
    %% verify that second peer node has only the first node visible
    [Node] = peer:call(Peer2, erlang, nodes, []),

    %% stop peers, causing containers to also stop
    peer:stop(Peer2),
    peer:stop(Peer).

build_release(Dir) ->
    %% load sasl.app file, otherwise application:get_key will fail
    application:load(sasl),
    %% create *.rel - release file
    RelFile = filename:join(Dir, "lambda.rel"),
    Release = {release, {"lambda", "1.0.0"},
        {erts, erlang:system_info(version)},
        [{App, begin {ok, Vsn} = application:get_key(App, vsn), Vsn end}
            || App <- [kernel, stdlib, sasl]]},
    ok = file:write_file(RelFile, list_to_binary(lists:flatten(
        io_lib:format("~tp.", [Release])))),
    RelFileNoExt = filename:join(Dir, "lambda"),

    %% create boot script
    {ok, systools_make, []} = systools:make_script(RelFileNoExt,
        [silent, {outdir, Dir}]),
    %% package release into *.tar.gz
    ok = systools:make_tar(RelFileNoExt, [{erts, code:root_dir()}]).

build_image(Dir) ->
    %% Create Dockerfile example, working only for Ubuntu 20.04
    %% Expose port 4445, and make Erlang distribution to listen
    %%  on this port, and connect to it without EPMD
    %% Set cookie on both nodes to be the same.
    BuildScript = filename:join(Dir, "Dockerfile"),
    Dockerfile =
      "FROM ubuntu:20.04 as runner\n"
      "EXPOSE 4445\n"
      "WORKDIR /opt/lambda\n"
      "COPY lambda.tar.gz /tmp\n"
      "RUN tar -zxvf /tmp/lambda.tar.gz -C /opt/lambda\n"
      "ENTRYPOINT [\"/opt/lambda/erts-" ++ erlang:system_info(version) ++
      "/bin/erl\", \"-boot\", \"/opt/lambda/releases/1.0.0/start\","
      " \"-kernel\", \"inet_dist_listen_min\", \"4445\","
      " \"-erl_epmd_port\", \"4445\","
      " \"-setcookie\", \"secret\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    os:cmd("docker build -t lambda " ++ Dir).
```

# `connection`
*not exported* *since OTP 25.0* 

```erlang
-type connection() :: Port :: 0..65535 | {inet:ip_address(), 0..65535} | standard_io.
```

Alternative connection between the origin and the peer. When the connection
closes, the peer node terminates automatically.

If the `peer_down` startup flag is set to `crash`, the controlling process on
the origin node exits with corresponding reason, effectively providing a two-way link.

When `connection` is set to a port number, the origin starts listening on the
requested TCP port, and the peer node connects to the port. When it is set to an
`{IP, Port}` tuple, the origin listens only on the specified IP. The port number
can be set to 0 for automatic selection.

Using the `standard_io` alternative connection starts the peer attached to the
origin (other connections use `-detached` flag to erl). In this mode peer and
origin communicate via stdin/stdout.

# `disconnect_timeout`
*since OTP 25.0* 

```erlang
-type disconnect_timeout() :: 1000..4294967295 | infinity.
```

Disconnect timeout. See [`stop()`](`stop/1`).

# `exec`
*since OTP 25.0* 

```erlang
-type exec() :: file:name() | {file:name(), [string()]}.
```

Overrides executable to start peer nodes with.

By default it is the path to "erl", taken from `init:get_argument(progname)`.
If `progname` is not known, `peer` makes best guess given the current ERTS version.

When a tuple is passed, the first element is the path to executable, and the
second element is prepended to the final command line. This can be used to start
peers on a remote host or in a Docker container. See the examples above.

This option is useful for testing backwards compatibility with previous
releases, installed at specific paths, or when the Erlang installation location
is missing from the `PATH`.

# `peer_state`
*since OTP 25.0* 

```erlang
-type peer_state() :: booting | running | {down, Reason :: term()}.
```

Peer node state.

# `server_ref`
*since OTP 25.0* 

```erlang
-type server_ref() :: pid().
```

Identifies the controlling process of a peer node.

# `start_options`
*since OTP 25.0* 

```erlang
-type start_options() ::
          #{name => atom() | string(),
            longnames => boolean(),
            host => string(),
            peer_down => stop | continue | crash,
            connection => connection(),
            exec => exec(),
            detached => boolean(),
            args => [string()],
            post_process_args => fun(([string()]) -> [string()]),
            env => [{string(), string()}],
            wait_boot => wait_boot(),
            shutdown => close | halt | {halt, disconnect_timeout()} | disconnect_timeout()}.
```

Options that can be used when starting a `peer` node through `start/1` and
[`start_link/0,1`](`start_link/0`).

- **`name`** - Node name (the part before "@"). When `name` is not specified,
  but `host` is, `peer` follows compatibility behaviour and uses the origin node
  name.

- **`longnames`** - Use long names to start a node. Default is taken from the
  origin using `net_kernel:longnames()`. If the origin is not distributed, short
  names is the default.

- **`host`** - Enforces a specific host name. Can be used to override the
  default behaviour and start "node@localhost" instead of "node@realhostname".

- **`peer_down`** - Defines the peer control process behaviour when the control
  connection is closed from the peer node side (for example when the peer
  crashes or dumps core). When set to `stop` (default), a lost control
  connection causes the control process to exit normally. Setting `peer_down` to
  `continue` keeps the control process running, and `crash` will cause the
  controlling process to exit abnormally.

- **`connection`** - Alternative connection specification. See the
  [`connection` datatype](`t:connection/0`).

- **`exec`** - Alternative mechanism to start peer nodes with, for example, ssh
  instead of the default bash.

- **`detached`** - Defines whether to pass the `-detached` flag to the started
  peer. This option cannot be set to `false` using the `standard_io` alternative
  connection type. Default is `true`.

- **`args`** - Extra command line arguments to append to the "erl" command.
  Arguments are passed as is, no escaping or quoting is needed or accepted.

- **`post_process_args`** - Allows the user to change the arguments passed to
  `exec` before the peer is started. This can for example be useful when the
  `exec` program wants the arguments to "erl" as a single argument. Example:

  ```erlang
  peer:start(#{ name => peer:random_name(),
    exec => {os:find_executable("bash"),["-c","erl"]},
    post_process_args =>
       fun(["-c"|Args]) -> ["-c", lists:flatten(lists:join($\s, Args))] end
    }).
  ```

- **`env`** - List of environment variables with their values. This list is
  applied to a locally started executable. If you need to change the environment
  of the remote peer, adjust `args` to contain `-env ENV_KEY ENV_VALUE`.

- **`wait_boot`** - Specifies the start/start_link timeout. See
  [`wait_boot` datatype](`t:wait_boot/0`).

- **`shutdown`** - Specifies the peer node stopping behaviour. See
  [`stop()`](`stop/1`).

# `wait_boot`
*not exported* *since OTP 25.0* 

```erlang
-type wait_boot() :: timeout() | {pid(), Tag :: term()} | false.
```

Specifies start/start_link timeout in milliseconds. Can be set to `false`,
allowing the peer to start asynchronously. If `{Pid, Tag}` is specified instead
of a timeout, the peer will send `Tag` to the requested process.

The default is `15_000` ms.

# `call`
*since OTP 25.0* 

```erlang
-spec call(Dest :: server_ref(), Module :: module(), Function :: atom(), Args :: [term()]) ->
              Result :: term().
```

# `call`
*since OTP 25.0* 

```erlang
-spec call(Dest :: server_ref(),
           Module :: module(),
           Function :: atom(),
           Args :: [term()],
           Timeout :: timeout()) ->
              Result :: term().
```

Uses the alternative connection to evaluate
[`apply(Module, Function, Args)`](`apply/3`) on the peer node and returns the
corresponding value `Result`.

`Timeout` is an integer representing the timeout in milliseconds or the atom
`infinity` which prevents the operation from ever timing out.

When an alternative connection is not requested, this function will raise `exit`
signal with the `noconnection` reason. Use `m:erpc` module to communicate over
Erlang distribution.

# `cast`
*since OTP 25.0* 

```erlang
-spec cast(Dest :: server_ref(), Module :: module(), Function :: atom(), Args :: [term()]) -> ok.
```

Uses the alternative connection to evaluate
[`apply(Module, Function, Args)`](`apply/3`) on the peer node. No response is
delivered to the calling process.

`peer:cast/4` fails silently when the alternative connection is not configured.
Use `m:erpc` module to communicate over Erlang distribution.

# `get_state`
*since OTP 25.0* 

```erlang
-spec get_state(Dest :: server_ref()) -> peer_state().
```

Returns the peer node state.

The initial state is `booting`; the node stays in that state until then boot
script is complete, and then the node progresses to `running`. If the node stops
(gracefully or not), the state changes to `down`.

# `random_name`
*since OTP 25.0* 

```erlang
-spec random_name() -> string().
```

# `random_name`
*since OTP 25.0* 

```erlang
-spec random_name(Prefix :: string() | atom()) -> string().
```

Creates a sufficiently unique node name for the current host, combining a
prefix, a unique number, and the current OS process ID.

> #### Note {: .info }
>
> Use the `?CT_PEER(["erl_arg1"])` macro provided by Common Test
> `-include_lib("common_test/include/ct.hrl")` for convenience. It starts a new
> peer using Erlang distribution as the control channel, supplies thes calling
> module's code path to the peer, and uses the calling function name for the
> name prefix.

# `send`
*since OTP 25.0* 

```erlang
-spec send(Dest :: server_ref(), To :: pid() | atom(), Message :: term()) -> ok.
```

Uses the alternative connection to send Message to a process on the the peer node.

Silently fails if no alternative connection is configured. The process can
be referenced by process ID or registered name.

# `start`
*since OTP 25.0* 

```erlang
-spec start(start_options()) -> {ok, pid()} | {ok, pid(), node()} | {error, Reason}
               when Reason :: term().
```

Starts a peer node with the specified `t:start_options/0`. Returns the
controlling process and the full peer node name, unless `wait_boot` is not
requested and the host name is not known in advance.

# `start_link`
*since OTP 25.0* 

```erlang
-spec start_link() -> {ok, pid(), node()} | {error, Reason :: term()}.
```

The same as [`start_link(#{name => random_name()})`](`start_link/1`).

# `start_link`
*since OTP 25.0* 

```erlang
-spec start_link(start_options()) -> {ok, pid()} | {ok, pid(), node()} | {error, Reason}
                    when Reason :: term().
```

Starts a peer node in the same way as `start/1`, except that the peer node is
linked to the currently executing process. If that process terminates, the peer
node also terminates.

Accepts `t:start_options/0`. Returns the controlling process and the full peer
node name, unless `wait_boot` is not requested and host name is not known in
advance.

When the `standard_io` alternative connection is requested, and `wait_boot` is
not set to `false`, a failed peer boot sequence causes the caller to exit with
the `{boot_failed, {exit_status, ExitCode}}` reason.

# `stop`
*since OTP 25.0* 

```erlang
-spec stop(Dest :: server_ref()) -> ok.
```

Stops a peer node. How the node is stopped depends on the
[`shutdown`](`t:start_options/0`) option passed when starting the peer node.
Currently the following `shutdown` options are supported:

- **`halt`** - This is the default shutdown behavior. It behaves as `shutdown`
  option `{halt, DefaultTimeout}` where `DefaultTimeout` currently equals
  `5000`.

- **`{halt, Timeout :: disconnect_timeout()}`** - Triggers a call to
  [`erlang:halt()`](`erlang:halt/0`) on the peer node and then waits for the
  Erlang distribution connection to the peer node to be taken down. If this
  connection has not been taken down after `Timeout` milliseconds, it will
  forcefully be taken down by `peer:stop/1`. See the
  [warning](`m:peer#dist_connection_close`) below for more info about this.

- **`Timeout :: disconnect_timeout()`** - Triggers a call to
  [`init:stop()`](`init:stop/0`) on the peer node and then waits for the Erlang
  distribution connection to the peer node to be taken down. If this connection
  has not been taken down after `Timeout` milliseconds, it will forcefully be
  taken down by `peer:stop/1`. See the [warning](`m:peer#dist_connection_close`)
  below for more info about this.

- **`close`** - Close the _control connection_ to the peer node and return. This
  is the fastest way for the caller of `peer:stop/1` to stop a peer node.

  Note that if the Erlang distribution connection is not used as control
  connection it might not have been taken down when `peer:stop/1` returns. Also
  note that the [warning](`m:peer#dist_connection_close`) below applies when the
  Erlang distribution connection is used as control connection.

[](){: #dist_connection_close }

> #### Warning {: .warning }
>
> In the cases where the Erlang distribution connection is taken down by
> `peer:stop/1`, other code independent of the peer code might react to the
> connection loss before the peer node is stopped which might cause undesirable
> effects. For example, [`global`](`m:global#prevent_overlapping_partitions`)
> might trigger even more Erlang distribution connections to other nodes to be
> taken down. The potential undesirable effects are, however, not limited to
> this. It is hard to say what the effects will be since these effects can be
> caused by any code with links or monitors to something on the origin node, or
> code monitoring the connection to the origin node.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
