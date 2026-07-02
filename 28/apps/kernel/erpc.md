# `erpc`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/kernel/src/erpc.erl#L25)

Enhanced Remote Procedure Call

This module provide services similar to Remote Procedure Calls. A remote
procedure call is a method to call a function on a remote node and collect the
answer. It is used for collecting information on a remote node, or for running a
function with some specific side effects on the remote node.

This is an enhanced subset of the operations provided by the `m:rpc` module.
Enhanced in the sense that it makes it possible to distinguish between returned
value, raised exceptions, and other errors. `erpc` also has better performance
and scalability than the original `rpc` implementation. However, current `rpc`
module will utilize `erpc` in order to also provide these properties when
possible.

In order for an `erpc` operation to succeed, the remote node also needs to
support `erpc`. Typically only ordinary Erlang nodes as of OTP 23 have `erpc`
support.

Note that it is up to the user to ensure that correct code to execute via `erpc`
is available on the involved nodes.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [Blocking Signaling Over Distribution](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_. Blocking
> signaling can, for example, cause timeouts in `erpc` to be significantly
> delayed.

# `call_options`
*since OTP 28.0* 

```elixir
-type call_options() ::
          #{timeout => Timeout :: timeout_time(), always_spawn => AlwaysSpawn :: boolean()}.
```

Options to be used in [`call/3,5`](`call/5`) and
[`multicall/3,5`](`multicall/5`) functions.

- **`timeout`** - Upper time limit for call operations to complete, see
  `t:timeout_time/0`. Default: `infinity`.

- **`always_spawn`** - If `true`, the `apply()` will _always_ be performed
  in a freshly spawned process. If `false`, the calling process _may_ be
  used instead, if possible. Default: `false`.

# `caught_call_exception`
*not exported* *since OTP 23.0* 

```elixir
-type caught_call_exception() ::
          {throw, Throw :: term()} |
          {exit, {exception, Reason :: term()}} |
          {error, {exception, Reason :: term(), StackTrace :: [stack_item()]}} |
          {exit, {signal, Reason :: term()}} |
          {error, {erpc, Reason :: term()}}.
```

# `request_id`
*since OTP 23.0* 

```elixir
-opaque request_id() :: nonempty_improper_list(reference(), reference()).
```

An opaque request identifier. For more information see `send_request/4`.

# `request_id_collection`
*since OTP 23.0* 

```elixir
-opaque request_id_collection() :: #{reference() => [reference() | term()]}.
```

An opaque collection of request identifiers (`t:request_id/0`) where each
request identifier can be associated with a label chosen by the user. For more
information see `reqids_new/0`.

# `stack_item`
*not exported* *since OTP 23.0* 

```elixir
-type stack_item() ::
          {Module :: atom(),
           Function :: atom(),
           Arity :: arity() | (Args :: [term()]),
           Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]}.
```

# `timeout_time`
*since OTP 23.0* 

```elixir
-type timeout_time() :: 0..4294967295 | infinity | {abs, integer()}.
```

The timeout time used by erpc functions.

The value can be:

- **`0..4294967295`** - Timeout relative to current time in milliseconds.

- **`infinity`** - Infinite timeout. That is, the operation will never time out.

- **`{abs, Timeout}`** - An absolute
  [Erlang monotonic time](`erlang:monotonic_time/1`) timeout in milliseconds.
  That is, the operation will time out when
  [`erlang:monotonic_time(millisecond)`](`erlang:monotonic_time/1`) returns a
  value larger than or equal to `Timeout`. `Timeout` is not allowed to identify
  a time further into the future than `4294967295` milliseconds. Identifying the
  timeout using an absolute timeout value is especially handy when you have a
  deadline for responses corresponding to a complete collection of requests
  (`t:request_id_collection/0`), since you do not have to recalculate the
  relative time until the deadline over and over again.

# `call`
*since OTP 23.0* 

```elixir
-spec call(Node, Fun) -> Result when Node :: node(), Fun :: function(), Result :: term().
```

# `call`
*since OTP 23.0* 

```elixir
-spec call(Node, Fun, TimeoutOrOptions) -> Result
              when
                  Node :: node(),
                  Fun :: function(),
                  TimeoutOrOptions :: timeout_time() | call_options(),
                  Result :: term().
```

Equivalent to
[`erpc:call(Node, erlang, apply, [Fun,[]], #{timeout => Timeout})`](`call/5`).

May raise all the same exceptions as [`call/5`](`call/5`) plus an `{erpc, badarg}`
`error` exception if `Fun` is not a fun of zero arity.

# `call`
*since OTP 23.0* 

```elixir
-spec call(Node, Module, Function, Args) -> Result
              when
                  Node :: node(),
                  Module :: atom(),
                  Function :: atom(),
                  Args :: [term()],
                  Result :: term().
```

# `call`
*since OTP 23.0* 

```elixir
-spec call(Node, Module, Function, Args, TimeoutOrOptions) -> Result
              when
                  Node :: node(),
                  Module :: atom(),
                  Function :: atom(),
                  Args :: [term()],
                  TimeoutOrOptions :: timeout_time() | call_options(),
                  Result :: term().
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node` and
returns the corresponding value `Result`.

`TimeoutOrOptions` can be either a [`timeout time`](`t:timeout_time/0`) or a
[`call options`](`t:call_options/0`) map (since OTP 28.0).

The `call()` function only returns if the applied function successfully returned
without raising any uncaught exceptions, the operation did not time out, and no
failures occurred. In all other cases an exception is raised. The following
exceptions, listed by exception class, can currently be raised by `call()`:

- **`throw`** - The applied function called [`throw(Value)`](`throw/1`) and did
  not catch this exception. The exception reason `Value` equals the argument
  passed to [`throw/1`](`throw/1`).

- **`exit`** - Exception reason:

  - **`{exception, ExitReason}`** - The applied function called
    [`exit(ExitReason)`](`exit/1`) and did not catch this exception. The exit
    reason `ExitReason` equals the argument passed to [`exit/1`](`exit/1`).

  - **`{signal, ExitReason}`** - The process that applied the function received
    an exit signal and terminated due to this signal. The process terminated
    with exit reason `ExitReason`.

- **`error`** - Exception reason:

  - **`{exception, ErrorReason, StackTrace}`** - A runtime error occurred which
    raised an error exception while applying the function, and the applied
    function did not catch the exception. The error reason `ErrorReason`
    indicates the type of error that occurred. `StackTrace` is formatted as when
    caught in a `try/catch` construct. The `StackTrace` is limited to the
    applied function and functions called by it.

  - **`{erpc, ERpcErrorReason}`** - The `erpc` operation failed. The following
    `ERpcErrorReason`s are the most common ones:

    - **`badarg`** - If any one of these are true:

      - `Node` is not an atom.
      - `Module` is not an atom.
      - `Function` is not an atom.
      - `Args` is not a list. Note that the list is not verified to be a proper
        list at the client side.
      - `Timeout` is invalid.

    - **`noconnection`** - The connection to `Node` was lost or could not be
      established. The function may or may not be applied.

    - **`system_limit`** - The `erpc` operation failed due to some system limit
      being reached. This typically due to failure to create a process on the
      remote node `Node`, but can be other things as well.

    - **`timeout`** - The `erpc` operation timed out. The function may or may
      not be applied.

    - **`notsup`** - The remote node `Node` does not support this `erpc`
      operation.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout or a connection loss), the caller will not receive
any further information about the result if/when the applied function completes.
If the applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.

> #### Note {: .info }
>
> If the `always_spawn` option is `false` (which is the default), you cannot make
> _any_ assumptions about the process that will perform the `apply()`. It may be
> the calling process itself, or a freshly spawned process.

# `cast`
*since OTP 23.0* 

```elixir
-spec cast(Node, Fun) -> ok when Node :: node(), Fun :: function().
```

Equivalent to [`erpc:cast(Node,erlang,apply,[Fun,[]])`](`cast/4`).

[`cast/2`](`cast/2`) fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a a fun of zero arity.

# `cast`
*since OTP 23.0* 

```elixir
-spec cast(Node, Module, Function, Args) -> ok
              when Node :: node(), Module :: atom(), Function :: atom(), Args :: [term()].
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on node `Node`. No
response is delivered to the calling process. `cast()` returns immediately after
the cast request has been sent. Any failures beside bad arguments are silently
ignored.

[`cast/4`](`cast/4`) fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

# `check_response`
*since OTP 23.0* 

```elixir
-spec check_response(Message, RequestId) -> {response, Result} | no_response
                        when Message :: term(), RequestId :: request_id(), Result :: term().
```

Check if a message is a response to a `call` request previously made by the
calling process using `send_request/4`.

`RequestId` should be the value returned from the previously made
[`send_request/4`](`send_request/4`) call, and the
corresponding response should not already have been received and handled to
completion by [`check_response/2`](`check_response/2`), `receive_response/2`, or
`wait_response/2`. `Message` is the message to check.

If `Message` does not correspond to the response, the atom `no_response` is
returned. If `Message` corresponds to the response, the `call` operation is
completed and either the result is returned as `{response, Result}` where
`Result` corresponds to the value returned from the applied function or an
exception is raised. The exceptions that can be raised corresponds to the same
exceptions as can be raised by `call/4`. That is, no `{erpc, timeout}` `error`
exception can be raised. `check_response()` will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a connection loss), the caller will not receive any further
information about the result if/when the applied function completes. If the
applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.

# `check_response`
*since OTP 25.0* 

```elixir
-spec check_response(Message, RequestIdCollection, Delete) ->
                        {{response, Result}, Label, NewRequestIdCollection} | no_response | no_request
                        when
                            Message :: term(),
                            RequestIdCollection :: request_id_collection(),
                            Delete :: boolean(),
                            Result :: term(),
                            Label :: term(),
                            NewRequestIdCollection :: request_id_collection().
```

Check if a message is a response to a `call` request corresponding to a request
identifier saved in `RequestIdCollection`. All request identifiers of
`RequestIdCollection` must correspond to requests that have been made using
`send_request/4` or `send_request/6`, and all requests must have been made by
the process calling this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `check_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`check_response/2`](`check_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exception `{erpc, badarg}`
is not associated with any specific request identifier, and will hence not be
wrapped.

If `RequestIdCollection` is empty, the atom `no_request` will be returned. If
`Message` does not correspond to any of the request identifiers in
`RequestIdCollection`, the atom `no_response` is returned.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`check_response/3`](`check_response/3`),
`receive_response/3`, and `wait_response/3`. However, without deleting handled
associations, the above calls will not be able to detect when there are no more
outstanding requests to handle, so you will have to keep track of this some
other way than relying on a `no_request` return. Note that if you pass a
collection only containing associations of already handled or abandoned requests
to [`check_response/3`](`check_response/3`), it will always return
`no_response`.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.

# `multicall`
*since OTP 23.0* 

```elixir
-spec multicall(Nodes, Fun) -> Result when Nodes :: [atom()], Fun :: function(), Result :: term().
```

# `multicall`
*since OTP 23.0* 

```elixir
-spec multicall(Nodes, Fun, TimeoutOrOptions) -> Result
                   when
                       Nodes :: [atom()],
                       Fun :: function(),
                       TimeoutOrOptions :: timeout_time() | call_options(),
                       Result :: term().
```

Equivalent to
[`erpc:multicall(Nodes, erlang, apply, [Fun,[]], #{timeout => Timeout})`](`multicall/5`).

May raise all the same exceptions as [`multicall/5`](`multicall/5`) plus an
`{erpc, badarg}` `error` exception if `Fun` is not a fun of zero arity.

# `multicall`
*since OTP 23.0* 

```elixir
-spec multicall(Nodes, Module, Function, Args) -> Result
                   when
                       Nodes :: [atom()],
                       Module :: atom(),
                       Function :: atom(),
                       Args :: [term()],
                       Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].
```

# `multicall`
*since OTP 23.0* 

```elixir
-spec multicall(Nodes, Module, Function, Args, TimeoutOrOptions) -> Result
                   when
                       Nodes :: [atom()],
                       Module :: atom(),
                       Function :: atom(),
                       Args :: [term()],
                       TimeoutOrOptions :: timeout_time() | call_options(),
                       Result :: [{ok, ReturnValue :: term()} | caught_call_exception()].
```

Performs multiple `call` operations in parallel on multiple nodes.

That is, evaluates [`apply(Module, Function, Args)`](`apply/3`) on the nodes `Nodes` in
parallel.

`TimeoutOrOptions` can be either a [`timeout time`](`t:timeout_time/0`) or a
[`call options`](`t:call_options/0`) map (since OTP 28.0).

The result is returned as a list where the result from each node is
placed at the same position as the node name is placed in `Nodes`. Each item in
the resulting list is formatted as either:

- **`{ok, Result}`** - The `call` operation for this specific node returned
  `Result`.

- **`{Class, ExceptionReason}`** - The `call` operation for this specific node
  raised an exception of class `Class` with exception reason `ExceptionReason`.
  These correspond to the exceptions that `call/5` can raise.

[`multicall/5`](`multicall/5`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms. Note that some requests may already
  have been sent when the failure occurs. That is, the function may or may not
  be applied on some nodes.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

The call `erpc:multicall(Nodes, Module, Function, Args)` is equivalent to the
call `erpc:multicall(Nodes, Module, Function, Args, infinity)`. These calls are
also equivalent to calling `my_multicall(Nodes, Module, Function, Args)` below
if one disregard performance and failure behavior. `multicall()` can utilize a
selective receive optimization which removes the need to scan the message queue
from the beginning in order to find a matching message. The
`send_request()/receive_response()` combination can, however, not utilize this
optimization.

```erlang
my_multicall(Nodes, Module, Function, Args) ->
  ReqIds = lists:map(fun (Node) ->
                       erpc:send_request(Node, Module, Function, Args)
                     end,
                     Nodes),
  lists:map(fun (ReqId) ->
              try
                {ok, erpc:receive_response(ReqId, infinity)}
              catch
                Class:Reason ->
                  {Class, Reason}
              end
            end,
            ReqIds).
```

If an `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout, connection loss, or an improper `Nodes` list), the
caller will not receive any further information about the result if/when the
applied function completes. If the applied function communicates with the
calling process, such communication may, of course, reach the calling process.

> #### Note {: .info }
>
> If the `always_spawn` option is `false` (which is the default), you cannot make
> _any_ assumptions about the processes that will perform the `apply()`s. It may be
> the calling process itself, or freshly spawned processes, or a mix of both.

# `multicast`
*since OTP 23.0* 

```elixir
-spec multicast(Nodes, Fun) -> ok when Nodes :: [node()], Fun :: function().
```

Equivalent to
[`erpc:multicast(Nodes,erlang,apply,[Fun,[]])`](`multicast/4`).

[`multicast/2`](`multicast/2`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms.
- `Fun` is not a a fun of zero arity.

# `multicast`
*since OTP 23.0* 

```elixir
-spec multicast(Nodes, Module, Function, Args) -> ok
                   when Nodes :: [node()], Module :: atom(), Function :: atom(), Args :: [term()].
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the nodes `Nodes`. No
response is delivered to the calling process. `multicast()` returns immediately
after the cast requests have been sent. Any failures beside bad arguments are
silently ignored.

[`multicast/4`](`multicast/4`) fails with an `{erpc, badarg}` `error` exception
if:

- `Nodes` is not a proper list of atoms. Note that some requests may already
  have been sent when the failure occurs. That is, the function may or may not
  be applied on some nodes.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

# `receive_response`
*since OTP 23.0* 

```elixir
-spec receive_response(RequestId) -> Result when RequestId :: request_id(), Result :: term().
```

# `receive_response`
*since OTP 23.0* 

```elixir
-spec receive_response(RequestId, Timeout) -> Result
                          when RequestId :: request_id(), Timeout :: timeout_time(), Result :: term().
```

Receive a response to a `call` request previously made by the calling process
using `send_request/4`.

`RequestId` should be the value returned from the
previously made [`send_request/4`](`send_request/4`) call, and the corresponding
response should not already have been received and handled to completion by
`receive_response()`, [`check_response/4`](`check_response/2`), or
[`wait_response/4`](`wait_response/2`).

`Timeout` sets an upper time limit on how long to wait for a response. If the
operation times out, the request identified by `RequestId` will be abandoned,
then an `{erpc, timeout}` `error` exception will be raised. That is, no response
corresponding to the request will ever be received after a timeout. If a
response is received, the `call` operation is completed and either the result is
returned or an exception is raised. The exceptions that can be raised
corresponds to the same exceptions as can be raised by `call/5`.
[`receive_response/2`](`receive_response/2`) will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected or if an invalid `Timeout`
is passed.

A call to the function `my_call(Node, Module, Function, Args, Timeout)` below is
equivalent to the call
[`erpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) if one disregards
performance. `call()` can utilize a selective receive optimization which removes
the need to scan the message queue from the beginning in order to find a
matching message. The `send_request()/receive_response()` combination can,
however, not utilize this optimization.

```erlang
my_call(Node, Module, Function, Args, Timeout) ->
  RequestId = erpc:send_request(Node, Module, Function, Args),
  erpc:receive_response(RequestId, Timeout).
```

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a timeout, or a connection loss), the caller will not receive
any further information about the result if/when the applied function completes.
If the applied function explicitly communicates with the calling process, such
communication may, of course, reach the calling process.

# `receive_response`
*since OTP 25.0* 

```elixir
-spec receive_response(RequestIdCollection, Timeout, Delete) ->
                          {Result, Label, NewRequestIdCollection} | no_request
                          when
                              RequestIdCollection :: request_id_collection(),
                              Timeout :: timeout_time(),
                              Delete :: boolean(),
                              Result :: term(),
                              Label :: term(),
                              NewRequestIdCollection :: request_id_collection().
```

Receive a response to a `call` request corresponding to a request identifier
saved in `RequestIdCollection`. All request identifiers of `RequestIdCollection`
must correspond to requests that have been made using `send_request/4` or
`send_request/6`, and all requests must have been made by the process calling
this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `receive_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`receive_response/2`](`receive_response/2`),
the second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exceptions `{erpc, badarg}`
and `{erpc, timeout}` are not associated with any specific request identifiers,
and will hence not be wrapped.

If `RequestIdCollection` is empty, the atom `no_request` will be returned.

If the operation times out, all requests identified by `RequestIdCollection`
will be abandoned, then an `{erpc, timeout}` `error` exception will be raised.
That is, no responses corresponding to any of the request identifiers in
`RequestIdCollection` will ever be received after a timeout. The difference
between [`receive_response/3`](`receive_response/3`) and `wait_response/3` is
that [`receive_response/3`](`receive_response/3`) abandons the requests at
timeout so that any potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`receive_response/3`](`receive_response/3`),
`check_response/3`, and `wait_response/3`. However, without deleting handled
associations, the above calls will not be able to detect when there are no more
outstanding requests to handle, so you will have to keep track of this some
other way than relying on a `no_request` return. Note that if you pass a
collection only containing associations of already handled or abandoned requests
to [`receive_response/3`](`receive_response/3`), it will always block until a
timeout determined by `Timeout` is triggered.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.

# `reqids_add`
*since OTP 25.0* 

```elixir
-spec reqids_add(RequestId :: request_id(),
                 Label :: term(),
                 RequestIdCollection :: request_id_collection()) ->
                    NewRequestIdCollection :: request_id_collection().
```

Saves `RequestId` and associates a `Label` with the request identifier by adding
this information to `RequestIdCollection` and returning the resulting request
identifier collection.

# `reqids_new`
*since OTP 25.0* 

```elixir
-spec reqids_new() -> NewRequestIdCollection :: request_id_collection().
```

Returns a new empty request identifier collection. A request identifier
collection can be utilized in order the handle multiple outstanding requests.

Request identifiers of requests made by `send_request/4` can be saved in a
request identifier collection using `reqids_add/3`. Such a collection of request
identifiers can later be used in order to get one response corresponding to a
request in the collection by passing the collection as argument to
`check_response/3`, `receive_response/3`, and `wait_response/3`.

`reqids_size/1` can be used to determine the amount of request identifiers in a
request identifier collection.

# `reqids_size`
*since OTP 25.0* 

```elixir
-spec reqids_size(RequestIdCollection :: request_id_collection()) -> non_neg_integer().
```

Returns the amount of request identifiers saved in `RequestIdCollection`.

# `reqids_to_list`
*since OTP 25.0* 

```elixir
-spec reqids_to_list(RequestIdCollection :: request_id_collection()) ->
                        [{RequestId :: request_id(), Label :: term()}].
```

Returns a list of `{RequestId, Label}` tuples which corresponds to all request
identifiers with their associated labels present in the `RequestIdCollection`
collection.

# `send_request`
*since OTP 23.0* 

```elixir
-spec send_request(Node, Fun) -> RequestId
                      when Node :: node(), Fun :: function(), RequestId :: request_id().
```

Equivalent to
[`erpc:send_request(Node, erlang, apply, [Fun, []])`](`send_request/4`).

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of zero arity.

# `send_request`
*since OTP 23.0* 

```elixir
-spec send_request(Node, Module, Function, Args) -> RequestId
                      when
                          Node :: node(),
                          Module :: atom(),
                          Function :: atom(),
                          Args :: [term()],
                          RequestId :: request_id();
                  (Node, Fun, Label, RequestIdCollection) -> NewRequestIdCollection
                      when
                          Node :: node(),
                          Fun :: function(),
                          Label :: term(),
                          RequestIdCollection :: request_id_collection(),
                          NewRequestIdCollection :: request_id_collection().
```

Send an asynchronous `call` request to the node `Node`.

[`send_request/4`](`send_request/4`) returns a request identifier that later is
to be passed to either `receive_response/2`, `wait_response/2`, or,
`check_response/2` in order to get the response of the call request. Besides
passing the request identifier directly to these functions, it can also be added
in a request identifier collection using `reqids_add/3`. Such a collection of
request identifiers can later be used in order to get one response corresponding
to a request in the collection by passing the collection as argument to
`receive_response/3`, `wait_response/3`, or, `check_response/3`. If you are
about to save the request identifier in a request identifier collection, you may
want to consider using `send_request/6` instead.

A call to the function `my_call(Node, Module, Function, Args, Timeout)` below is
equivalent to the call
[`erpc:call(Node, Module, Function, Args, Timeout)`](`call/5`) if one disregards
performance. `call()` can utilize a selective receive optimization which removes
the need to scan the message queue from the beginning in order to find a
matching message. The `send_request()/receive_response()` combination can,
however, not utilize this optimization.

```erlang
my_call(Node, Module, Function, Args, Timeout) ->
  RequestId = erpc:send_request(Node, Module, Function, Args),
  erpc:receive_response(RequestId, Timeout).
```

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.

Equivalent to
[`erpc:send_request(Node, erlang, apply, [Fun,[]]), Label, RequestIdCollection)`](`send_request/6`).

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of zero arity.
- `RequestIdCollection` is detected not to be request identifier collection.

# `send_request`
*since OTP 25.0* 

```elixir
-spec send_request(Node, Module, Function, Args, Label, RequestIdCollection) -> NewRequestIdCollection
                      when
                          Node :: node(),
                          Module :: atom(),
                          Function :: atom(),
                          Args :: [term()],
                          Label :: term(),
                          RequestIdCollection :: request_id_collection(),
                          NewRequestIdCollection :: request_id_collection().
```

Send an asynchronous `call` request to the node `Node`. The `Label` will be
associated with the request identifier of the operation and added to the
returned request identifier collection `NewRequestIdCollection`. The collection
can later be used in order to get one response corresponding to a request in the
collection by passing the collection as argument to `receive_response/3`,
`wait_response/3`, or, `check_response/3`.

Equivalent to
[`erpc:reqids_add`](`reqids_add/3`)([`erpc:send_request`](`send_request/4`)`(Node, Module, Function, Args), Label, RequestIdCollection)`,
but calling [`send_request/6`](`send_request/6`) is slightly more efficient.

Fails with an `{erpc, badarg}` `error` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a list. Note that the list is not verified to be a proper list
  at the client side.
- `RequestIdCollection` is detected not to be request identifier collection.

# `wait_response`
*since OTP 23.0* 

```elixir
-spec wait_response(RequestId) -> {response, Result} | no_response
                       when RequestId :: request_id(), Result :: term().
```

Equivalent to [`erpc:wait_response(RequestId, 0)`](`wait_response/2`).
That is, poll for a response message to a `call` request previously made by the
calling process.

# `wait_response`
*since OTP 23.0* 

```elixir
-spec wait_response(RequestId, WaitTime) -> {response, Result} | no_response
                       when RequestId :: request_id(), WaitTime :: timeout_time(), Result :: term().
```

Wait or poll for a response message to a `call` request previously made by the
calling process using `send_request/4`.

`RequestId` should be the value returned from the previously made `send_request()`
call, and the corresponding response should not already have been received and
handled to completion by `check_response/2`, `receive_response/2`, or `wait_response()`.

`WaitTime` sets an upper time limit on how long to wait for a response. If no
response is received before the `WaitTime` timeout has triggered, the atom
`no_response` is returned. It is valid to continue waiting for a response as
many times as needed up until a response has been received and completed by
`check_response()`, `receive_response()`, or `wait_response()`. If a response is
received, the `call` operation is completed and either the result is returned as
`{response, Result}` where `Result` corresponds to the value returned from the
applied function or an exception is raised. The exceptions that can be raised
corresponds to the same exceptions as can be raised by `call/4`. That is, no
`{erpc, timeout}` `error` exception can be raised.
[`wait_response/2`](`wait_response/2`) will fail with an `{erpc, badarg}`
exception if/when an invalid `RequestId` is detected or if an invalid `WaitTime`
is passed.

If the `erpc` operation fails, but it is unknown if the function is/will be
applied (that is, a too large wait time value, or a connection loss), the caller
will not receive any further information about the result if/when the applied
function completes. If the applied function explicitly communicates with the
calling process, such communication may, of course, reach the calling process.

# `wait_response`
*since OTP 25.0* 

```elixir
-spec wait_response(RequestIdCollection, WaitTime, Delete) ->
                       {{response, Result}, Label, NewRequestIdCollection} | no_response | no_request
                       when
                           RequestIdCollection :: request_id_collection(),
                           WaitTime :: timeout_time(),
                           Delete :: boolean(),
                           Label :: term(),
                           NewRequestIdCollection :: request_id_collection(),
                           Result :: term().
```

Wait or poll for a response to a `call` request corresponding to a request
identifier saved in `RequestIdCollection`. All request identifiers of
`RequestIdCollection` must correspond to requests that have been made using
`send_request/4` or `send_request/6`, and all requests must have been made by
the process calling this function.

`Label` is the label associated with the request identifier of the request that
the response corresponds to. A request identifier is associated with a label
when [adding a request identifier](`reqids_add/3`) in a
[request identifier collection](`t:request_id_collection/0`), or when sending
the request using `send_request/6`.

Compared to `wait_response/2`, the returned result associated with a specific
request identifier or an exception associated with a specific request identifier
will be wrapped in a 3-tuple. The first element of this tuple equals the value
that would have been produced by [`wait_response/2`](`wait_response/2`), the
second element equals the `Label` associated with the specific request
identifier, and the third element `NewRequestIdCollection` is a possibly
modified request identifier collection. The `error` exception `{erpc, badarg}`
is not associated with any specific request identifier, and will hence not be
wrapped.

If `RequestIdCollection` is empty, `no_request` will be returned. If no response
is received before the `WaitTime` timeout has triggered, the atom `no_response`
is returned. It is valid to continue waiting for a response as many times as
needed up until a response has been received and completed by
`check_response()`, `receive_response()`, or `wait_response()`. The difference
between `receive_response/3` and [`wait_response/3`](`wait_response/3`) is that
[`receive_response/3`](`receive_response/3`) abandons requests at timeout so
that any potential future responses are ignored, while
[`wait_response/3`](`wait_response/3`) does not.

If `Delete` equals `true`, the association with `Label` will have been deleted
from `RequestIdCollection` in the resulting `NewRequestIdCollection`. If
`Delete` equals `false`, `NewRequestIdCollection` will equal
`RequestIdCollection`. Note that deleting an association is not for free and
that a collection containing already handled requests can still be used by
subsequent calls to [`wait_response/3`](`wait_response/3`), `check_response/3`,
and `receive_response/3`. However, without deleting handled associations, the
above calls will not be able to detect when there are no more outstanding
requests to handle, so you will have to keep track of this some other way than
relying on a `no_request` return. Note that if you pass a collection only
containing associations of already handled or abandoned requests to
[`wait_response/3`](`wait_response/3`), it will always block until a timeout
determined by `WaitTime` is triggered and then return `no_response`.

Note that a response might have been consumed uppon an `{erpc, badarg}`
exception and if so, will be lost for ever.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
