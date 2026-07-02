# `ct_rpc`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/common_test/src/ct_rpc.erl#L23)

`Common Test` specific layer on Erlang/OTP `rpc`.

# `app_node`

```erlang
-spec app_node(App, Candidates) -> CandidateNode
                  when App :: atom(), Candidates :: [node()], CandidateNode :: node().
```

From a set of candidate nodes determines which of them is running the
application `App`. If none of the candidate nodes is running `App`, the function
makes the test case calling this function to fail. This function is the same as
calling [`app_node(App, Candidates, true)`](`app_node/3`).

# `app_node`

```erlang
-spec app_node(App, Candidates, FailOnBadRPC) -> CandidateNode
                  when
                      App :: atom(),
                      Candidates :: [node()],
                      FailOnBadRPC :: boolean(),
                      CandidateNode :: node().
```

Same as [`ct_rpc:app_node/2`](`app_node/2`), except that argument `FailOnBadRPC`
determines if the search for a candidate node is to stop if `badrpc` is received
at some point.

# `app_node`

```erlang
-spec app_node(App, Candidates, FailOnBadRPC, Cookie) -> CandidateNode
                  when
                      App :: atom(),
                      Candidates :: [node()],
                      FailOnBadRPC :: boolean(),
                      Cookie :: atom() | [],
                      CandidateNode :: node().
```

Same as [`ct_rpc:app_node/2`](`app_node/2`), except that argument `FailOnBadRPC`
determines if the search for a candidate node is to stop if `badrpc` is received
at some point.

The cookie on the client node is set to `Cookie` for this `rpc` operation (used
to match the server node cookie).

# `call`

```erlang
-spec call(Node, Module, Function, Args) -> term() | {badrpc, Reason}
              when
                  Node :: {Function, Args} | node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: list(),
                  Reason :: term().
```

# `call`

```erlang
-spec call(Node, Module, Function, Args, TimeOut) -> term() | {badrpc, Reason}
              when
                  Node :: {Function, Args} | node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: list(),
                  TimeOut :: timeout(),
                  Reason :: term().
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the node `Node`.
Returns either whatever `Function` returns, or `{badrpc, Reason}` if the remote
procedure call fails. If `Node` is `{Fun, FunArgs}`, applying `Fun` to `FunArgs`
is to return a node name.

# `call`

```erlang
-spec call(Node, Module, Function, Args, TimeOut, Cookie) -> term() | {badrpc, Reason}
              when
                  Node :: {Function, Args} | node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: list(),
                  TimeOut :: timeout(),
                  Cookie :: atom() | [],
                  Reason :: term().
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the node `Node`.
Returns either whatever `Function` returns, or `{badrpc, Reason}` if the remote
procedure call fails. If `Node` is `{Fun, FunArgs}`, applying `Fun` to `FunArgs`
is to return a node name.

The cookie on the client node is set to `Cookie` for this `rpc` operation (used
to match the server node cookie).

# `cast`

```erlang
-spec cast(Node, Module, Function, Args) -> ok
              when
                  Node :: {Function, Args} | node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: list().
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the node `Node`. No
response is delivered and the process that makes the call is not suspended until
the evaluation is completed as in the case of `call/3,4`. If `Node` is
`{Fun, FunArgs}`, applying `Fun` to `FunArgs` is to return a node name.

# `cast`

```erlang
-spec cast(Node, Module, Function, Args, Cookie) -> ok
              when
                  Node :: {Function, Args} | node(),
                  Module :: module(),
                  Function :: atom(),
                  Args :: list(),
                  Cookie :: atom() | [].
```

Evaluates [`apply(Module, Function, Args)`](`apply/3`) on the node `Node`. No
response is delivered and the process that makes the call is not suspended until
the evaluation is completed as in the case of `call/3,4`. If `Node` is
`{Fun, FunArgs}`, applying `Fun` to `FunArgs` is to return a node name.

The cookie on the client node is set to `Cookie` for this `rpc` operation (used
to match the server node cookie).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
