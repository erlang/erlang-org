# `rpc`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/rpc.erl#L22)

Remote Procedure Call services.

This module contains services similar to Remote Procedure Calls. It also
contains broadcast facilities and parallel evaluators. A remote procedure call
is a method to call a function on a remote node and collect the answer. It is
used for collecting information on a remote node, or for running a function with
some specific side effects on the remote node.

> #### Note {: .info }
>
 `rpc:call/4` and related functions make it difficult to distinguish
> between successful results, raised exceptions, and other errors. This
> behavior cannot be changed for compatibility reasons.
>
> The `m:erpc` module was introduced in Erlang/OTP 23 to provide an API
> that allows clear distinction between these different outcomes. The
> `m:erpc` module offers the core subset of the functionality provided by
> the `m:rpc` module. It also features a more scalable and
> higher-performance implementation compared to the original `m:rpc` module.
>
> Since the introduction of `m:erpc`, the `m:rpc` module has been
> updated to use `m:erpc` internally for most of its core
> functionality. As a result, the `m:rpc` module does not fall short in
> scalability or performance compared to `m:erpc`.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [Blocking Signaling Over Distribution](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_. Blocking
> signaling can, for example, cause timeouts in `rpc` to be significantly
> delayed.

# `key`

```erlang
-opaque key()
```

Opaque value returned by `async_call/4`.

# `abcast`

```erlang
-spec abcast(Name, Msg) -> abcast when Name :: atom(), Msg :: term().
```

Equivalent to [`abcast([node()|nodes()], Name, Msg)`](`abcast/3`).

# `abcast`

```erlang
-spec abcast(Nodes, Name, Msg) -> abcast when Nodes :: [node()], Name :: atom(), Msg :: term().
```

Broadcasts the message `Msg` asynchronously to the registered process `Name` on
the specified nodes.

# `async_call`

```erlang
-spec async_call(Node, Module, Function, Args) -> Key
                    when
                        Node :: node(),
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Key :: key().
```

Implements _call streams with promises_, a type of RPC that does not suspend the
caller until the result is finished. Instead, a key is returned, which can be
used later to collect the value. The key can be viewed as a promise to deliver
the answer.

In this case, the key `Key` is returned, which can be used in a subsequent call
to `yield/1` or [`nb_yield/1,2`](`nb_yield/1`) to retrieve the value of
evaluating [`apply(Module, Function, Args)`](`apply/3`) on node `Node`.

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:send_request()`](`erpc:send_request/4`) function
> from the `erpc` module instead. This also gives you the ability retrieve the
> results in other useful ways.

> #### Note {: .info }
>
> `yield/1` and [`nb_yield/1,2`](`nb_yield/1`) must be called by the same
> process from which this function was made otherwise they will never yield
> correctly.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be an `rpc` server, another server, or a freshly spawned
> process.

# `block_call`

```erlang
-spec block_call(Node, Module, Function, Args) -> Res | {badrpc, Reason}
                    when
                        Node :: node(),
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Res :: term(),
                        Reason :: term().
```

The same as calling
[`rpc:block_call(Node, Module, Function, Args, infinity)`](`block_call/5`).

# `block_call`

```erlang
-spec block_call(Node, Module, Function, Args, Timeout) -> Res | {badrpc, Reason}
                    when
                        Node :: node(),
                        Module :: module(),
                        Function :: atom(),
                        Args :: [term()],
                        Res :: term(),
                        Reason :: term(),
                        Timeout :: 0..4294967295 | infinity.
```

The same as calling
[`rpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) with the exception
that it also blocks other `rpc:block_call/5` operations from executing
concurrently on the node `Node`.

> #### Warning {: .warning }
>
> Note that it also blocks other operations than just `rpc:block_call/5`
> operations, so use it with care.

# `call`

```erlang
-spec call(Node, Module, Function, Args) -> Res | {badrpc, Reason}
              when
                  Node :: node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Res :: term(),
                  Reason :: term().
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Res`, or `{badrpc, Reason}` if the call fails.
The same as calling
[`rpc:call(Node, Module, Function, Args, infinity)`](`call/5`).

# `call`

```erlang
-spec call(Node, Module, Function, Args, Timeout) -> Res | {badrpc, Reason}
              when
                  Node :: node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: [term()],
                  Res :: term(),
                  Reason :: term(),
                  Timeout :: 0..4294967295 | infinity.
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Res`, or `{badrpc, Reason}` if the call fails.
`Timeout` is a time-out value in milliseconds. If the call times out, `Reason`
is `timeout`.

If the reply arrives after the call times out, no message contaminates the
caller's message queue.

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:call()`](`erpc:call/4`) function from the `erpc`
> module instead.

> #### Note {: .info }
>
> Here follows the details of what exactly is returned.
>
> `{badrpc, Reason}` will be returned in the following circumstances:
>
> - The called function fails with an `exit` exception.
> - The called function fails with an `error` exception.
> - The called function returns a term that matches `{'EXIT', _}`.
> - The called function `throws` a term that matches `{'EXIT', _}`.
>
> `Res` is returned in the following circumstances:
>
> - The called function returns normally with a term that does **not** match
>   `{'EXIT',_}`.
> - The called function `throw`s a term that does **not** match `{'EXIT',_}`.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, an `rpc` server, another
> server, or a freshly spawned process.

# `cast`

```erlang
-spec cast(Node, Module, Function, Args) -> true
              when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node`. No
response is delivered and the calling process is not suspended until the
evaluation is complete, as is the case with [`call/4,5`](`call/4`).

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be an `rpc` server, another server, or a freshly spawned
> process.

# `eval_everywhere`

```erlang
-spec eval_everywhere(Module, Function, Args) -> abcast
                         when Module :: module(), Function :: atom(), Args :: [term()].
```

Equivalent to
[`eval_everywhere([node()|nodes()], Module, Function, Args)`](`eval_everywhere/4`).

# `eval_everywhere`

```erlang
-spec eval_everywhere(Nodes, Module, Function, Args) -> abcast
                         when
                             Nodes :: [node()], Module :: module(), Function :: atom(), Args :: [term()].
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the specified nodes.
No answers are collected.

# `multi_server_call`

```erlang
-spec multi_server_call(Name, Msg) -> {Replies, BadNodes}
                           when
                               Name :: atom(),
                               Msg :: term(),
                               Replies :: [Reply :: term()],
                               BadNodes :: [node()].
```

Equivalent to
[`multi_server_call([node()|nodes()], Name, Msg)`](`multi_server_call/3`).

# `multi_server_call`

```erlang
-spec multi_server_call(Nodes, Name, Msg) -> {Replies, BadNodes}
                           when
                               Nodes :: [node()],
                               Name :: atom(),
                               Msg :: term(),
                               Replies :: [Reply :: term()],
                               BadNodes :: [node()].
```

Can be used when interacting with servers called `Name` on the specified nodes.
It is assumed that the servers receive messages in the format `{From, Msg}` and
reply using `From ! {Name, Node, Reply}`, where `Node` is the name of the node
where the server is located. The function returns `{Replies, BadNodes}`, where
`Replies` is a list of all `Reply` values, and `BadNodes` is one of the
following:

- A list of the nodes that do not exist
- A list of the nodes where the server does not exist
- A list of the nodes where the server terminated before sending any reply.

# `multicall`

```erlang
-spec multicall(Module, Function, Args) -> {ResL, BadNodes}
                   when
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       ResL :: [Res :: term() | {badrpc, Reason :: term()}],
                       BadNodes :: [node()].
```

Equivalent to
[`multicall([node()|nodes()], Module, Function, Args, infinity)`](`multicall/5`).

# `multicall`

```erlang
-spec multicall(Nodes, Module, Function, Args) -> {ResL, BadNodes}
                   when
                       Nodes :: [node()],
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       ResL :: [Res :: term() | {badrpc, Reason :: term()}],
                       BadNodes :: [node()];
               (Module, Function, Args, Timeout) -> {ResL, BadNodes}
                   when
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       Timeout :: 0..4294967295 | infinity,
                       ResL :: [Res :: term() | {badrpc, Reason :: term()}],
                       BadNodes :: [node()].
```

Equivalent to
[`multicall(Nodes, Module, Function, Args, infinity)`](`multicall/5`).

Equivalent to
[`multicall([node()|nodes()], Module, Function, Args, Timeout)`](`multicall/5`).

# `multicall`

```erlang
-spec multicall(Nodes, Module, Function, Args, Timeout) -> {ResL, BadNodes}
                   when
                       Nodes :: [node()],
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       Timeout :: 0..4294967295 | infinity,
                       ResL :: [Res :: term() | {badrpc, Reason :: term()}],
                       BadNodes :: [node()].
```

In contrast to an RPC, a multicall is an RPC that is sent concurrently from one
client to multiple servers. This is useful for collecting information from a set
of nodes, or for calling a function on a set of nodes to achieve some side
effects. It is semantically the same as iteratively making a series of RPCs on
all the nodes, but the multicall is faster, as all the requests are sent at the
same time and are collected one by one as they come back.

The function evaluates [`apply(Module, Function, Args)`](`apply/3`) on the
specified nodes and collects the answers. It returns `{ResL, BadNodes}`, where
`BadNodes` is a list of the nodes that do not exist, and `ResL` is a list of the
return values, or `{badrpc, Reason}` for failing calls. `Timeout` is a time
(integer) in milliseconds, or `infinity`.

The following example is useful when new object code is to be loaded on all
nodes in the network, and indicates some side effects that RPCs can produce:

```erlang
%% Find object code for module Mod
{Mod, Bin, File} = code:get_object_code(Mod),

%% and load it on all nodes including this one
{ResL, _} = rpc:multicall(code, load_binary, [Mod, File, Bin]),

%% and then maybe check the ResL list.
```

> #### Note {: .info }
>
> If you want the ability to distinguish between results, you may want to
> consider using the [`erpc:multicall()`](`erpc:multicall/4`) function from the
> `erpc` module instead.

> #### Note {: .info }
>
> You cannot make _any_ assumptions about the process that will perform the
> `apply()`. It may be the calling process itself, an `rpc` server, another
> server, or a freshly spawned process.

# `nb_yield`

```erlang
-spec nb_yield(Key) -> {value, Val} | timeout
                  when Key :: key(), Val :: (Res :: term()) | {badrpc, Reason :: term()}.
```

Equivalent to [`nb_yield(Key, 0)`](`nb_yield/2`).

# `nb_yield`

```erlang
-spec nb_yield(Key, Timeout) -> {value, Val} | timeout
                  when
                      Key :: key(),
                      Timeout :: 0..4294967295 | infinity,
                      Val :: (Res :: term()) | {badrpc, Reason :: term()}.
```

Non-blocking version of `yield/1`. It returns the tuple `{value, Val}` when the
computation is finished, or `timeout` when `Timeout` milliseconds has elapsed.

See the note in `call/4` for more details of Val.

> #### Note {: .info }
>
> This function must be called by the same process from which `async_call/4` was
> made otherwise it will only return `timeout`.

# `parallel_eval`

```erlang
-spec parallel_eval(FuncCalls) -> ResL
                       when
                           FuncCalls :: [{Module, Function, Args}],
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           ResL :: [term()].
```

Evaluates, for every tuple in `FuncCalls`,
[`apply(Module, Function, Args)`](`apply/3`) on some node in the network.
Returns the list of return values, in the same order as in `FuncCalls`.

# `pinfo`

```erlang
-spec pinfo(Pid) -> [{Item, Info}] | undefined when Pid :: pid(), Item :: atom(), Info :: term().
```

Location transparent version of the BIF `erlang:process_info/1` in ERTS.

# `pinfo`

```erlang
-spec pinfo(Pid, Item) -> {Item, Info} | undefined | []
               when Pid :: pid(), Item :: atom(), Info :: term();
           (Pid, ItemList) -> [{Item, Info}] | undefined | []
               when Pid :: pid(), Item :: atom(), ItemList :: [Item], Info :: term().
```

Location transparent version of the BIF `erlang:process_info/2` in ERTS.

# `pmap`

```erlang
-spec pmap(FuncSpec, ExtraArgs, List1) -> List2
              when
                  FuncSpec :: {Module, Function},
                  Module :: module(),
                  Function :: atom(),
                  ExtraArgs :: [term()],
                  List1 :: [Elem :: term()],
                  List2 :: [term()].
```

Evaluates [`apply(Module, Function, [Elem|ExtraArgs])`](`apply/3`) for every
element `Elem` in `List1`, in parallel. Returns the list of return values, in
the same order as in `List1`.

# `sbcast`

```erlang
-spec sbcast(Name, Msg) -> {GoodNodes, BadNodes}
                when Name :: atom(), Msg :: term(), GoodNodes :: [node()], BadNodes :: [node()].
```

Equivalent to [`sbcast([node()|nodes()], Name, Msg)`](`sbcast/3`).

# `sbcast`

```erlang
-spec sbcast(Nodes, Name, Msg) -> {GoodNodes, BadNodes}
                when
                    Name :: atom(),
                    Msg :: term(),
                    Nodes :: [node()],
                    GoodNodes :: [node()],
                    BadNodes :: [node()].
```

Broadcasts the message `Msg` synchronously to the registered process `Name` on
the specified nodes.

Returns `{GoodNodes, BadNodes}`, where `GoodNodes` is the list of nodes that
have `Name` as a registered process.

The function is synchronous in the sense that it is known that all servers have
received the message when the call returns. It is not possible to know that the
servers have processed the message.

Any further messages sent to the servers, after this function has returned, are
received by all servers after this message.

# `server_call`

```erlang
-spec server_call(Node, Name, ReplyWrapper, Msg) -> Reply | {error, Reason}
                     when
                         Node :: node(),
                         Name :: atom(),
                         ReplyWrapper :: term(),
                         Msg :: term(),
                         Reply :: term(),
                         Reason :: nodedown.
```

Can be used when interacting with a server called `Name` on node `Node`. It is
assumed that the server receives messages in the format `{From, Msg}` and
replies using `From ! {ReplyWrapper, Node, Reply}`. This function makes such a
server call and ensures that the entire call is packed into an atomic
transaction, which either succeeds or fails. It never hangs, unless the server
itself hangs.

The function returns the answer `Reply` as produced by the server `Name`, or
`{error, Reason}`.

# `yield`

```erlang
-spec yield(Key) -> Res | {badrpc, Reason} when Key :: key(), Res :: term(), Reason :: term().
```

Returns the promised answer from a previous `async_call/4`. If the answer is
available, it is returned immediately. Otherwise, the calling process is
suspended until the answer arrives from `Node`.

> #### Note {: .info }
>
> This function must be called by the same process from which `async_call/4` was
> made otherwise it will never return.

See the note in `call/4` for more details of the return value.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
