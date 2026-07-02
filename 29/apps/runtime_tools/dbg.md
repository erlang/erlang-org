# `dbg`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/runtime_tools/src/dbg.erl#L22)

The Text Based Trace Facility

This module implements a text based interface to the
`trace:process/4`, `trace:port/4`, and `trace:function/4` BIFs,
simplifying tracing of functions, processes, ports, and messages.

The [**Tracing in Erlang with dbg Users guide**](`e:runtime_tools:dbg_guide.md`)
explains how to quickly get started on tracing function calls, complex systems
and more.

# `built_in_alias`
*not exported* 

```erlang
-type built_in_alias() :: x | c | cx.
```

# `handler_spec`
*not exported* 

```erlang
-type handler_spec() ::
          {HandlerFun :: fun((Event :: term(), Data :: term()) -> NewData :: term()),
           InitialData :: term()}.
```

# `match_desc`
*not exported* 

```erlang
-type match_desc() :: [match_info()].
```

# `match_info`
*not exported* 

```erlang
-type match_info() :: {saved, tp_id()} | match_num().
```

# `match_num`
*not exported* 

```erlang
-type match_num() :: {matched, node(), integer()} | {matched, node(), 0, RPCError :: term()}.
```

# `match_pattern`
*not exported* 

```erlang
-type match_pattern() :: atom() | list().
```

# `match_spec`

```erlang
-type match_spec() :: [{match_pattern(), [_], [_]}].
```

# `session`

```erlang
-opaque session()
```

A `m:dbg` session that can be used by `session/2` to
create isolated debugging sessions.

# `tp_arity`
*not exported* 

```erlang
-type tp_arity() :: arity() | '_'.
```

# `tp_function`
*not exported* 

```erlang
-type tp_function() :: atom() | '_'.
```

# `tp_id`
*not exported* 

```erlang
-type tp_id() :: pos_integer().
```

# `tp_match_spec`
*not exported* 

```erlang
-type tp_match_spec() :: tp_id() | built_in_alias() | [] | match_spec().
```

# `tp_module`
*not exported* 

```erlang
-type tp_module() :: module() | '_'.
```

# `trace_wrap_file_size`
*not exported* 

```erlang
-type trace_wrap_file_size() :: non_neg_integer() | {time, WrapTime :: pos_integer()}.
```

# `trace_wrap_files_spec`
*not exported* 

```erlang
-type trace_wrap_files_spec() ::
          {file:name_all(), wrap, Suffix :: string()} |
          {file:name_all(), wrap, Suffix :: string(), WrapSize :: trace_wrap_file_size()} |
          {file:name_all(),
           wrap,
           Suffix :: string(),
           WrapSize :: trace_wrap_file_size(),
           WrapCnt :: pos_integer()}.
```

# `c`

```erlang
-spec c(Mod :: module(), Fun :: atom(), Args :: [term()]) -> term().
```

# `c`

```erlang
-spec c(Mod :: module(), Fun :: atom(), Args :: [term()], Flags :: term()) -> term().
```

Evaluates the expression [`apply(Mod, Fun, Args)`](`apply/3`) with the
trace flags in `Flags` set.

`c` stands for **c**all.

This is a convenient way to trace processes from the Erlang shell.

# `cn`

```erlang
-spec cn(Nodename) -> ok when Nodename :: node().
```

Clears a node from the list of traced nodes.

`cn` stands for **c**lear **n**ode.

Subsequent calls to `tp/2` and `p/2` will not consider that node, but tracing
already activated on the node will continue to be in effect.

Returns `ok`. This call cannot fail.

# `ctp`

```erlang
-spec ctp() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctp`

```erlang
-spec ctp(Module | {Module, Function, Arity}) -> {ok, MatchDesc :: match_desc()} | {error, term()}
             when Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity().
```

Disables call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctp({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctp` stands for **c**lear **t**race **p**attern.

The semantics of `ModuleOrMFA` is the same as for the corresponding function
specification in `tp/2` or `tpl/2`. Both local and global call trace
is disabled.

The return value reflects how many functions that matched, and is constructed as
described in `tp/2`, except that no `{saved, N}` tuple is returned.

# `ctp`

```erlang
-spec ctp(Module :: tp_module(), Function :: tp_function()) ->
             {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctp`

```erlang
-spec ctp(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
             {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpe`
*since OTP 19.0* 

```erlang
-spec ctpe(Event) -> {ok, MatchDesc} | {error, term()}
              when
                  Event :: send | 'receive',
                  MatchDesc :: [MatchNum],
                  MatchNum :: {matched, node(), 1} | {matched, node(), 0, RPCError :: term()}.
```

Clears match specifications for the specified trace event (`send` or
`'receive'`), reverting to the default of tracing all triggered events.

`ctpe` stands for **c**lear **t**race **p**attern **e**vent.

# `ctpg`

```erlang
-spec ctpg() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpg`

```erlang
-spec ctpg(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
              {ok, MatchDesc :: term()} | {error, term()}
              when Module :: tp_module().
```

Disables global call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctpg({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctpg` stands for **c**lear **t**race **p**attern **g**lobal.

This function works as `ctp/1`, but only disables tracing set up with
`tp/2` (not with `tpl/2`).

# `ctpg`

```erlang
-spec ctpg(Module :: tp_module(), Function :: tp_function()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpg`

```erlang
-spec ctpg(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```erlang
-spec ctpl() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```erlang
-spec ctpl(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
              {ok, MatchDesc :: term()} | {error, term()}
              when Module :: tp_module().
```

Disables local call tracing for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is
equivalent to `ctpl({ModuleOrMFA, '_', '_'})`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`ctpl` stands for **c**lear **t**race **p**attern **l**ocal.

This function works as `ctp/1`, but only disables tracing set up with
`tpl/2` (not with `tp/2`).

# `ctpl`

```erlang
-spec ctpl(Module :: tp_module(), Function :: tp_function()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```erlang
-spec ctpl(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `dtp`

```erlang
-spec dtp() -> ok.
```

Forgets all match specifications saved during calls to `tp/2`.

`dtp` stands for **d**elete **t**race **p**atterns.

Removing all saved match specifications is useful before restoring
other match specifications from a file with `rtp/1`. Use `dtp/1` to
delete specific saved match specifications.

# `dtp`

```erlang
-spec dtp(N) -> ok when N :: tp_id().
```

Forgets a specific match specification saved during calls to `tp/2`.

`dtp` stands for **d**elete **t**race **p**attern.

# `flush_trace_port`

```erlang
-spec flush_trace_port() -> term().
```

# `flush_trace_port`

```erlang
-spec flush_trace_port(Nodename :: node()) -> ok | {error, Reason :: term()}.
```

# `fun2ms`

```erlang
-spec fun2ms(LiteralFun) -> MatchSpec
                when LiteralFun :: fun((term()) -> term()), MatchSpec :: match_spec().
```

Pseudo function that by means of a parse transform translates the
_literal_ fun typed as parameter in the function call to a [match
specification](`e:erts:match_spec.md`).

The meaning of "literal" is that the fun needs to textually be written
as the argument of the function call; it cannot be held in a variable
which in turn is passed to the function. Furthermore, the parse
transform module `m:ms_transform` must be enabled. The easiest way to
enable it is by adding the following line to the source file:

```
-include_lib("stdlib/include/ms_transform.hrl").
```

Failing to include `ms_transform.hrl` in the source will result in a runtime
error, not a compile-time error.

This function can also be invoked directly from the Erlang shell, as shown in
the examples that follow.

The head of the fun must be a single pattern that matches a list. That pattern
will be used to match the arguments for the call:

_Examples_:

```erlang
1> dbg:fun2ms(fun([_,_]) -> true end).
[{['_','_'],[],[true]}]
2> dbg:fun2ms(fun(Args) when length(Args) > 6 -> true end).
[{'$1',[{'>',{length,'$1'},6}],[true]}]
```

The first match specification matches when a function having two
arguments is called. The second matches when a function with more than
6 arguments is called.

_Examples_:

```erlang
1> dbg:fun2ms(fun(42) -> true end).
Error: dbg:fun2ms requires fun with single variable or list parameter
{error,transform_error}
2> dbg:fun2ms(fun([<<H,T/binary>>]) -> true end).
Error: fun head contains bit syntax matching of variable 'H', which cannot be translated into match_spec
{error,transform_error}
```

The preceding two examples show what happens when a fun cannot be
translated into a match specification. In the first example, the fun
head connot possibly match a list. In the second example, an attempt is made
to take apart a binary using the bit syntax, which is currently not
supported in match specifications.

However, note that literal binaries *can* be matched:

```erlang
1> dbg:fun2ms(fun([<<"abc">>]) -> true end).
[{[<<"abc">>],[],[true]}]
```

Match specifications support a large subset of the
[guard expressions](`e:system:expressions.md#guard-expressions`) supported
by Erlang, but not all. For example, updating a map is currently not supported:

```erlang
1> dbg:fun2ms(fun([M]) when map_size(M#{a => b}) > 2 -> true end).
Error: the language element map (in guard) cannot be translated into match_spec
{error,transform_error}
```

However, creating a map in a guard is allowed:

```erlang
1> dbg:fun2ms(fun([M]) when map_size(#{a => b}) > 2 -> true end).
[{['$1'],[{'>',{map_size,#{a => b}},2}],[true]}]
```

Variables from the environment can be imported, so this works:

```erlang
1> X = 3.
3
2> dbg:fun2ms(fun([M,N]) when N > X  -> return_trace() end).
[{['$1','$2'],[{'>','$2',{const,3}}],[{return_trace}]}]
```
The imported variables will be replaced by `const` expressions, which
is consistent with the static scoping for Erlang funs.

In the body of the fun, only guard expressions and calls to the
[special functions for tracing](`e:erts:match_spec.md#functions-allowed-only-for-tracing`)
are allowed.

_Examples_:

```erlang
1> dbg:fun2ms(fun([A]) when is_atom(A) -> return_trace() end).
[{['$1'],[{is_atom,'$1'}],[{return_trace}]}]
2> dbg:fun2ms(fun(_) -> erlang:garbage_collect() end).
Error: fun containing the remote function call 'erlang:garbage_collect/0' (called in body) cannot be translated into match_spec
{error,transform_error}
```

> #### Warning {: .warning }
>
> If the parse transform is not applied to a module which calls `dbg:fun2ms/1`,
> the call will fail in runtime with a `badarg` exception.

More information is available in the documentation for module `m:ms_transform`
in STDLIB.

# `get_tracer`

```erlang
-spec get_tracer() -> term().
```

# `get_tracer`

```erlang
-spec get_tracer(Nodename) -> {ok, Tracer}
                    when Nodename :: atom(), Tracer :: port() | pid() | {module(), term()}.
```

Returns the process, port, or tracer module to which all trace messages are sent.

# `h`

```erlang
-spec h() -> ok.
```

Gives a list of items for brief online help.

`h` stands for **h**elp.

# `h`

```erlang
-spec h(Item) -> ok when Item :: atom().
```

Gives a brief help text for functions in the `dbg` module.

`h` stands for **h**elp.

The available items can be listed by calling `dbg:h/0`.

# `i`

```erlang
-spec i() -> ok.
```

Displays information about all traced processes and ports.

`i` stands for **i**nformation.

# `ln`

```erlang
-spec ln() -> ok.
```

Shows the list of traced nodes on the console.

`ln` stands for **l**ist **n**odes.

# `ltp`

```erlang
-spec ltp() -> ok.
```

Lists all match specifications previously used in the session.

`ltp` stands for **l**ist **t**race **p**atterns.

This function lists all match specifications previously saved during
calls to `tp/2` and `tpl/2`, as well as all built-in match
specifications. This avoids having to re-type complicated match
specifications. Note that the match specifications are lost if
`stop/0` is called.

Match specifications can be saved in a file (if a read-write file system is
present) for use in later debugging sessions; see `wtp/1` and `rtp/1`.

There are three built-in trace patterns:

- `exception_trace`, `x` - sets a trace which will show function
  names, parameters, return values, and exceptions raised from
  functions

- `caller_trace`, `c` - sets a trace that displays function names,
  parameters, and information about which function called it

- `caller_exception_trace`, `cx` - combines `exception_trace` and
  `caller_trace`

Here is an example that shows how to use a built-in match specification:

```erlang
1> dbg:tracer().
{ok,<0.90.0>}
2> dbg:tp(lists, seq, 2, cx).
{ok,[{matched,nonode@nohost,1},{saved,cx}]}
3> dbg:p(self(), call).
{ok,[{matched,nonode@nohost,1}]}
4> lists:seq(1, 5).
(<0.88.0>) call lists:seq(1,5) ({erl_eval,do_apply,7,{"erl_eval.erl",904}})
[1,2,3,4,5]
(<0.88.0>) returned from lists:seq/2 -> [1,2,3,4,5]
```

# `n`

```erlang
-spec n(Nodename) -> {ok, Nodename} | {error, Reason} when Nodename :: node(), Reason :: term().
```

Adds a remote node (`Nodename`) to the list of nodes where tracing is
performed.

`n` stands for **n**ode.

The `dbg` server keeps a list of nodes where tracing should be
performed. Whenever a `tp/2` call or a `p/2` call is made, it is
executed for all nodes in this list including the local node (except
for `p/2` with a specific `t:pid/0` or `t:port/0` as first argument,
in which case the command is executed only on the node where the
designated process or port resides).

When this function is called, it starts a tracer process on the remote
node, which will send all trace messages to the tracer process on the
local node (via the Erlang distribution). If no tracer process is
running on the local node, the error reason `no_local_tracer` is
returned. The tracer process on the local node must be started with
the [`tracer/0,2`](`tracer/2`) function.

If `Nodename` is the local node, the error reason `cant_add_local_node` is
returned.

If a trace port (see `trace_port/2`) is running on the local node, remote nodes
cannot be traced with a tracer process. The error reason
`cant_trace_remote_pid_to_local_port` is returned. However, a trace port can be
started on the remote node with the `tracer/3` function.

The function will also return an error if the node `Nodename` is not reachable.

# `p`

```erlang
-spec p(Item :: term()) -> {ok, MatchDesc :: term()} | {error, term()}.
```

# `p`

```erlang
-spec p(Item :: term(), Flags :: term()) -> {ok, MatchDesc} | {error, term()}
           when
               MatchDesc :: [MatchNum],
               MatchNum :: {matched, node(), integer()} | {matched, node(), 0, RPCError},
               RPCError :: term().
```

Traces `Item` in accordance to the value specified by `Flags`.

`p` stands for **p**rocess.

The following kind of values are allowed for `Item`:

- **`t:pid/0` or `t:port/0`** - The corresponding process or port is traced. The
  process or port can be a remote process or port (on another Erlang node). The
  node must be in the list of traced nodes (see `n/1` and `tracer/3`).

- **`all`** - All processes and ports in the system as well as all processes and
  ports created hereafter are to be traced.

- **`processes`** - All processes in the system as well as all processes created
  hereafter are to be traced.

- **`ports`** - All ports in the system as well as all ports created hereafter
  are to be traced.

- **`new`** - All processes and ports created after the call are to be
  traced.

- **`new_processes`** - All processes created after the call are to be
  traced.

- **`new_ports`** - All ports created after the call are to be traced.

- **`existing`** - All existing processes and ports are traced.

- **`existing_processes`** - All existing processes are traced.

- **`existing_ports`** - All existing ports are traced.

- **`t:atom/0`** - The process or port with the corresponding registered name is
  traced. The process or port can on another Erlang node.
  The node must be in the list of traced nodes (see `n/1` and `tracer/3`).

- **`t:integer/0`** - The process `<0.Item.0>` is traced.

- **`{X, Y, Z}`** - The process `<X.Y.Z>` is traced.

- **`t:string/0`** - If the `Item` is a string "<X.Y.Z>" as returned from
  [`pid_to_list/1`](`erlang:pid_to_list/1`), the process `<X.Y.Z>` is traced.

When enabling an `Item` that represents a group of processes, the `Item` is
enabled on all nodes added with the `n/1` or `tracer/3` function.

`Flags` can be a single atom or a list of flags. The available flags are:

- **`s (send)`** - Traces the messages the process or port sends.

- **`r (receive)`** - Traces the messages the process or port receives.

- **`m (messages)`** - Traces the messages the process or port receives and
  sends.

- **`c (call)`** - Traces global function calls for the process according to the
  trace patterns set in the system (see `tp/2`).

- **`p (procs)`** - Traces process related events to the process.

- **`ports`** - Traces port related events to the port.

- **`sos (set on spawn)`** - Lets all processes created by the traced process
  inherit the trace flags of the traced process.

- **`sol (set on link)`** - Lets another process, `P2`, inherit the trace flags
  of the traced process whenever the traced process links to `P2`.

- **`sofs (set on first spawn)`** - This is the same as `sos`, but only for the
  first process spawned by the traced process.

- **`sofl (set on first link)`** - This is the same as `sol`, but only for the
  first call to [`link/1`](`erlang:link/1`) by the traced process.

- **`all`** - Sets all flags except `silent`.

- **`clear`** - Clears all flags.

The list can also include any of the flags allowed in `trace:process/4` and
`trace:port/4`.

This function returns either an error tuple or an `{ok, List}` tuple. The `List`
consists of specifications of how many processes and ports that matched (in the
case of a single pid exactly 1). The specification of matched processes is
`{matched, Node, N}`. If the remote processor call (using `m:rpc`) to a remote node
fails, the `rpc` error message is returned as the fourth element in the tuple
and the number of matched processes is 0.

# `rtp`

```erlang
-spec rtp(Name) -> ok | {error, Error} when Name :: string(), Error :: term().
```

Reads match specifications from a text file (possibly) generated by
the `wtp/1` function.

`rtp` stands for **r**ead **t**race **p**atterns.

The function verifies that the syntax of all match specifications are correct.
If any error in any match specification is found, none of the match specifications
are added to the list of saved match specifications for the running system.

The match specifications in the file are _merged_ with the current match
specifications, so that no duplicates are generated. Use `ltp/0` to see what
numbers were assigned to the specifications from the file.

The function will return an error tuple, either due to I/O problems
(like a non-existing or non-readable file) or due to file format
problems. In the latter case, `Reason` is in a more or less textual
format, giving a hint to what is causing the problem.

# `session`
*since OTP 27.0* 

```erlang
-spec session(atom(), fun(() -> term())) -> term();
             (session(), fun(() -> term())) -> term().
```

Runs `m:dbg` commands using the provides session, or
creates a session for the duration of the call if a session name
is provided.

Any `m:dbg` function that is called with in the provided fun
will use the `t:session/0` provided instead of the default
`dbg` session. This means that the tracing will be isolated
from other tracing users on the system.

The function returns the term that the fun returns.

*Example*:

```erlang
1> S = dbg:session_create(my_session).
<0.91.0>
2> dbg:session(S, fun() -> dbg:tracer(), dbg:p(all,c), dbg:tp(lists,seq,x) end).
{ok,[{matched,nonode@nohost,2},{saved,x}]}
3> lists:seq(1, 10).
(<0.89.0>) call lists:seq(1,10)
(<0.89.0>) returned from lists:seq/2 -> [1,2,3,4,5,6,7,8,9,10]
[1,2,3,4,5,6,7,8,9,10]
4> dbg:session_destroy(S).
ok
```

The state of the `t:session/0` is preserved in between `session/2` calls, so
you can call `session/2` multiple when debugging you application.

*Example*:

```erlang
1> S = dbg:session_create(my_session).
<0.91.0>
%% Setup the initial traces
2> dbg:session(S, fun() -> dbg:tracer(), dbg:p(self(),c), dbg:tp(lists,seq,x) end).
{ok,[{matched,nonode@nohost,2},{saved,x}]}
3> lists:seq(1, 3).
(<0.89.0>) call lists:seq(1,3)
(<0.89.0>) returned from lists:seq/2 -> [1,2,3]
[1,2,3]
%% Add an additional trace pattern
4> dbg:session(S, fun() -> dbg:tpl(lists,seq_loop,x) end).
ok
5> lists:seq(1, 3).
(<0.89.0>) call lists:seq(1,3)
(<0.89.0>) call lists:seq_loop(3,3,[])
(<0.89.0>) call lists:seq_loop(1,1,[2,3])
(<0.89.0>) returned from lists:seq_loop/3 -> [1,2,3]
(<0.89.0>) returned from lists:seq_loop/3 -> [1,2,3]
(<0.89.0>) returned from lists:seq/2 -> [1,2,3]
[1,2,3]
6> dbg:session_destroy(S).
ok
```

> #### Note {: .info }
>
> The session functionality is experimental in Erlang/OTP 27
> and may change in future releases without notice.

# `session_create`
*since OTP 27.0* 

```erlang
-spec session_create(atom()) -> session().
```

Create a new `m:dbg` session with the given `Name`.

The session is linked with the calling process and will be

Multiple sessions can have the same name.

> #### Note {: .info }
>
> The session functionality is experimental in Erlang/OTP 27
> and may change in future releases without notice.

# `session_destroy`
*since OTP 27.0* 

```erlang
-spec session_destroy(Session :: session()) -> ok.
```

Destroys a dbg `t:session/0`.

This will terminate all started processes and destroy the `t:trace:session/0`.

# `stop`

```erlang
-spec stop() -> ok.
```

Stops the `dbg` server, clears all trace flags for all processes, clears all
trace patterns for all functions, clears trace patterns for send/receive, shuts
down all trace clients, and closes all trace ports.

# `stop_trace_client`

```erlang
-spec stop_trace_client(Pid) -> ok when Pid :: pid().
```

Shuts down a previously started trace client.

The `Pid` argument is the process id returned from the
`trace_client/2` or `trace_client/3` call.

# `tp`

```erlang
-spec tp(Module | {Module, Function, Arity}, MatchSpec) -> {ok, match_desc()} | {error, term()}
            when
                Module :: tp_module(),
                Function :: tp_function(),
                Arity :: tp_arity(),
                MatchSpec :: tp_match_spec().
```

Enables call trace for one or more exported functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is equivalent to
`tp({ModuleOrMFA, '_', '_'}, MatchSpec)`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`tp` stands for **t**race **p**attern.

All exported functions matching the `{Module, Function, Arity}`
argument will be concerned, but the match specification may further
narrow down the set of function calls generating trace messages.

For a description of the format for the `MatchSpec` argument, see
[_Match Specifications in Erlang_](`e:erts:match_spec.md`), which explains the
general match specification language. The most common generic match
specifications used can be found as built-in aliases; see `ltp/0` below for
details.

The Module, Function and/or Arity parts of the tuple may be specified
as the atom `'_'` which is a wildcard matching all modules, functions,
or arities. Note that if the `Module` is specified as `'_'`, the
`Function` and `Arity` parts must be specified as `'_'` as well. The
same holds for the `Function` in relation to `Arity`.

All nodes added with `n/1` or `tracer/3` will be affected by this call, and if
`Module` is not `'_'` the module will be loaded on all nodes.

The function returns either an error tuple or an `{ok, List}` tuple. The `List`
consists of specifications of how many functions that matched, in the same way
as the processes and ports are presented in the return value of `p/2`.

There may be a tuple `{saved, N}` in the return value, if the `MatchSpec` is not
`[]`. The integer `N` can then be used in subsequent calls to this function
and will stand as an "alias" for the given expression.

If the match specification is invalid, an `{error, Errors}` tuple is
returned.  `Errors` is as a list of tuples `{error, string()}`, where
the string is a textual explanation of the compilation error. For
example:

```erlang
1> dbg:tp({dbg,ltp,0},[{[],[],[{message, two, arguments}, {noexist}]}]).
{error,
 [{error,"Special form 'message' called with wrong number of
          arguments in {message,two,arguments}."},
  {error,"Function noexist/1 does_not_exist."}]}
```

# `tp`

```erlang
-spec tp(Module :: tp_module(), Function :: tp_function(), MatchSpec :: tp_match_spec()) ->
            {ok, match_desc()} | {error, term()}.
```

# `tp`

```erlang
-spec tp(Module :: tp_module(),
         Function :: tp_function(),
         Arity :: tp_arity(),
         MatchSpec :: tp_match_spec()) ->
            {ok, match_desc()} | {error, term()}.
```

# `tpe`
*since OTP 19.0* 

```erlang
-spec tpe(Event, MatchSpec) -> {ok, MatchDesc :: match_desc()} | {error, term()}
             when Event :: send | 'receive', MatchSpec :: tp_match_spec().
```

Associates a match specification with trace event `send` or
`'receive'`.

`tpe` stands for **t**race **p**attern **e**vent.

By default all executed
`send` and `'receive'` events are traced if enabled for a process. A match
specification can be used to filter traced events based on sender, receiver,
and/or message content.

For a description of the format for the `MatchSpec` argument, see
[_Match Specifications in Erlang_](`e:erts:match_spec.md`), which explains the
general match specification language.

For `send`, the matching is done on the list `[Receiver, Msg]`. `Receiver` is
the process or port identity of the receiver and `Msg` is the message term. The
pid of the sending process can be accessed with the guard function `self/0`.

For `'receive'`, the matching is done on the list `[Node, Sender, Msg]`. `Node`
is the node name of the sender. `Sender` is the process or port identity of the
sender, or the atom `undefined` if the sender is not known (which may be the
case for remote senders). `Msg` is the message term. The pid of the receiving
process can be accessed by calling `self/0`.

All nodes added with `n/1` or `tracer/3` will be affected by this call.

The return value is the same as for `tp/2`. The number of matched events is
always 1 as [`tpe/2`](`tpe/2`) does not accept any form of wildcards
for argument `Event`.

# `tpl`

```erlang
-spec tpl(Module | {Module, Function :: tp_function(), Arity :: tp_arity()},
          MatchSpec :: tp_match_spec()) ->
             {ok, MatchDesc :: term()} | {error, term()}
             when Module :: tp_module().
```

Enables call trace for one or more functions specified by `ModuleOrMFA`.

If `ModuleOrMFA` is an atom (a module name), this function call is equivalent to
`tpl({ModuleOrMFA, '_', '_'}, MatchSpec)`.

Otherwise, `ModuleOrMFA` should be `{Module, Function, Arity}`.

`tpl` stands for **t**race **p**attern **l**ocal.

This function works as `tp/2`, but enables tracing for local or remote calls
to both local and exported functions.

# `tpl`

```erlang
-spec tpl(Module :: tp_module(), Function :: tp_function(), MatchSpec :: tp_match_spec()) ->
             {ok, match_desc()} | {error, term()}.
```

# `tpl`

```erlang
-spec tpl(Module :: tp_module(),
          Function :: tp_function(),
          Arity :: tp_arity(),
          MatchSpec :: tp_match_spec()) ->
             {ok, match_desc()} | {error, term()}.
```

# `trace_client`

```erlang
-spec trace_client(ip, IPClientPortSpec) -> pid()
                      when
                          IPClientPortSpec :: PortNumber | {Hostname, PortNumber},
                          PortNumber :: integer(),
                          Hostname :: string();
                  (Type, Parameters) -> pid()
                      when
                          Type :: file | follow_file,
                          Parameters :: Filename | WrapFilesSpec,
                          Filename :: file:name_all(),
                          WrapFilesSpec :: trace_wrap_files_spec().
```

Starts a trace client that reads the output created by a trace port
driver (see `trace_port/2`) and handles it in mostly the same way as a
tracer process created by the `tracer/0` function.

If `Type` is `file`, the client reads all trace messages stored in the
file named `Filename` or specified by `WrapFilesSpec` (must be the
same as used when creating the trace) and lets the default handler
function format the messages on the console. This is one way to
interpret the data stored in a file by the file trace port driver.

If `Type` is `follow_file`, the client behaves as in the `file` case, but keeps
trying to read (and process) more data from the file until stopped by
`stop_trace_client/1`. `WrapFilesSpec` is not allowed as second argument for
this `Type`.

If `Type` is `ip`, the client connects to the TCP/IP port `PortNumber` on the
host `Hostname`, from where it reads trace messages until the TCP/IP connection
is closed. If no `Hostname` is specified, the local host is assumed.

As an example, one can let trace messages be sent over the network to another
Erlang node (preferably _not_ distributed), where the formatting occurs.

On the node `stack` there exists an Erlang node `ant@stack`. In the
shell, type the following:

```erlang
ant@stack> dbg:tracer(port, dbg:trace_port(ip, 4711)).
<0.17.0>
ant@stack> dbg:p(self(), send).
{ok,1}
```

All trace messages are now sent to the trace port driver, which in turn listens
for connections on the TCP/IP port 4711. If we want to see the messages on
another node, preferably on another host, we do like this:

```erlang
1> dbg:trace_client(ip, {"stack", 4711}).
<0.42.0>
```

If we now send a message from the shell on the node `ant@stack`, where all sends
from the shell are traced:

```erlang
ant@stack> self() ! hello.
hello
```

The following will appear at the console on the node that started the trace
client:

```erlang
(<0.23.0>) <0.23.0> ! hello
(<0.23.0>) <0.22.0> ! {shell_rep,<0.23.0>,{value,hello,[],[]}}
```

The last line is generated due to internal message passing in the Erlang shell.
The pids will vary.

# `trace_client`

```erlang
-spec trace_client(ip, IPClientPortSpec, HandlerSpec) -> pid()
                      when
                          IPClientPortSpec :: PortNumber | {Hostname, PortNumber},
                          PortNumber :: integer(),
                          Hostname :: string(),
                          HandlerSpec :: handler_spec();
                  (Type, Parameters, HandlerSpec) -> pid()
                      when
                          Type :: file | follow_file,
                          Parameters :: Filename | WrapFilesSpec,
                          Filename :: string() | [string()] | atom(),
                          WrapFilesSpec :: trace_wrap_files_spec(),
                          HandlerSpec :: handler_spec().
```

This function works exactly as `trace_client/2`, but allows you to write your
own handler function.

The handler function works mostly as the one described in `tracer/2`,
but must also be prepared to handle trace messages of the form `{drop,
N}`, where `N` is the number of dropped messages. This pseudo trace
message will only occur if the `ip` trace driver is used.

For trace type `file`, the pseudo trace message `end_of_trace` will appear at
the end of the trace. The return value from the handler function is in this case
ignored.

# `trace_port`

```erlang
-spec trace_port(ip, IpPortSpec) -> fun(() -> port())
                    when
                        IpPortSpec :: PortNumber | {PortNumber, QueSize},
                        PortNumber :: integer(),
                        QueSize :: integer();
                (file, Parameters) -> fun(() -> port())
                    when
                        Parameters :: Filename | WrapFilesSpec,
                        Filename :: file:name_all(),
                        WrapFilesSpec :: trace_wrap_files_spec().
```

Creates a trace-port-generating fun that is suitable as the
second argument to `tracer/2`.

_Example:_

```erlang
dbg:tracer(port, dbg:trace_port(ip, 4711)).
```

A trace port is an Erlang port to a dynamically linked-in driver that
handles trace messages directly, without the overhead of sending them
as messages to an Erlang process. Using a trace port significantly
lowers the overhead imposed by tracing.

Two trace drivers are currently implemented: the `file` and the `ip`
trace drivers.

The `file` driver sends all trace messages into one or
several binary files, from where they later can be fetched and
processed with the `trace_client/2` function.

The `ip` driver opens a TCP/IP port listening port. When a client
(preferably started by calling `trace_client/2` on another Erlang
node) connects, all trace messages are sent over the IP network for
further processing by the remote client.

The `file` trace driver expects a filename or a wrap files
specification as parameter. A file is written with a high degree of
buffering, which is why there is no guarantee that all are saved in the
file in case of a system crash.

A wrap files specification is used to limit the disk space consumed by the
trace. The trace is written to a limited number of files each with a limited
size. The actual filenames are `Filename ++ SeqCnt ++ Suffix`, where `SeqCnt`
counts as a decimal string from `0` to `WrapCnt` and then around again from `0`.
When a trace term written to the current file makes it longer than `WrapSize`,
that file is closed, and if the number of files in this wrap trace is as many as
`WrapCnt` the oldest file is deleted, and a new file is opened to become the
current. Thus, when a wrap trace has been stopped, there are at most `WrapCnt`
trace files saved with a size of at least `WrapSize` (but not much larger),
except for the last file that might even be empty. The default values are
`WrapSize = 128*1024` and `WrapCnt = 8`.

The `SeqCnt` values in the filenames are all in the range `0` through `WrapCnt`
with a gap in the circular sequence. The gap is needed to find the end of the
trace.

If the `WrapSize` is specified as `{time, WrapTime}`, the current file is closed
when it has been open more than `WrapTime` milliseconds, regardless of it being
empty or not.

The `ip` trace driver has a queue of `QueSize` messages waiting to be delivered.
If the driver cannot deliver messages as fast as they are produced by the
runtime system, a special message is sent, which indicates how many messages
that are dropped. That message will arrive at the handler function specified in
`trace_client/3` as the tuple `{drop, N}` where `N` is the number of consecutive
messages dropped. In case of heavy tracing, drops are likely to occur, and they
surely occur if no client is reading the trace messages. The default value of
`QueSize` is 200.

# `trace_port_control`

```erlang
-spec trace_port_control(Operation :: term()) -> term().
```

# `trace_port_control`

```erlang
-spec trace_port_control(Nodename :: node(), Operation :: term()) ->
                            ok | {ok, Result :: term()} | {error, Reason :: term()}.
```

This function is used to do a control operation on the active trace port driver
on the given node (`Nodename`).

Which operations are allowed as well as their return values depend on
which trace driver is used.

Returns either `ok` or `{ok, Result}` if the operation was successful, or
`{error, Reason}` if the current tracer is a process, or if it is a port not
supporting the operation.

The allowed values for `Operation` are:

- **`flush`** - This function is used to flush the internal buffers held by a
  trace port driver. Currently only the `file` trace driver supports this
  operation. Returns `ok`.

- **`get_listen_port`** - Returns `{ok, IpPort}` where `IpPort` is the IP port
  number used by the driver listen socket. Only the `ip` trace driver supports
  this operation.

# `tracer`

```erlang
-spec tracer() -> {ok, pid()} | {error, already_started}.
```

Starts a server on the local node that will be the recipient of
all trace messages.

All subsequent calls to `p/2` will result in messages sent
to the newly started trace server.

A trace server started in this way will simply display the formatted
trace messages the Erlang shell (that is, using `io:format/2`). See `tracer/2`
for a description of how the trace message handler can be customized.

To start a similar tracer on a remote node, use `n/1`.

# `tracer`

```erlang
-spec tracer(port, PortGenerator) -> {ok, pid()} | {error, Error :: term()}
                when PortGenerator :: fun(() -> port());
            (process, HandlerSpec) -> {ok, pid()} | {error, Error :: term()}
                when
                    HandlerSpec :: {HandlerFun, InitialData :: term()},
                    HandlerFun :: fun((Event :: term(), Data :: term()) -> NewData :: term());
            (module, ModuleSpec) -> {ok, pid()} | {error, Error :: term()}
                when
                    ModuleSpec :: fun(() -> {TracerModule, TracerState}) | {TracerModule, TracerState},
                    TracerModule :: atom(),
                    TracerState :: term();
            (file, Filename) -> {ok, pid()} | {error, Error :: term()} when Filename :: file:name_all().
```

Starts a tracer server with additional parameters on the local
node.

`Type` indicates how trace messages should be handled:

- `process` - by a receiving process
- `port` - by a port; see `trace_port/2`
- `module` - by a tracer module; see `m:erl_tracer`
- `file` - by printing them to a file

If `Type` is `process`, `Data` should be a message handler function
(`HandlerSpec`). The handler function, which should be a `fun` taking two
arguments, will be called for each trace message, with the first argument
containing the message as it is and the second argument containing the return
value from the last invocation of the fun. The initial value of the second
parameter is specified in the `InitialData` part of the `HandlerSpec`. The
`HandlerFun` can choose any appropriate action to take when invoked, and can
save a state for the next invocation by returning it.

If `Type` is `port`, then the second parameter should be a fun which takes no
arguments and returns a newly opened trace port when called. Such a fun is
preferably generated by calling `trace_port/2`.

If `Type` is `module`, `Data` should be either a tuple describing the
`m:erl_tracer` module to be used for tracing and the state to be used
for that tracer module, or a fun returning that kind of tuple.

if `Type` is `file`, `Data` should be a filename specifying a file
where all the traces are to be printed.

If an error is returned, it can either be because a tracer server is
already running (`{error,already_started}`), or because
`HandlerFun` raised an exception.

To start a similar tracer on a remote node, use `tracer/3`.

# `tracer`

```erlang
-spec tracer(Nodename :: node(), Type :: term(), Data :: term()) ->
                {ok, Nodename :: node()} | {error, Reason :: term()}.
```

This function is equivalent to `tracer/2`, but acts on the given node.

A tracer is started on the node (`Nodename`) and the node is added to
the list of traced nodes.

> #### Note {: .info }
>
> This function is not equivalent to `n/1`. While `n/1` starts a process tracer
> which redirects all trace information to a process tracer on the local node
> (that is, the trace control node), `tracer/3` starts any type of tracer,
> independent of the type of tracer on the trace control node.

For details, see `tracer/2`.

# `wtp`

```erlang
-spec wtp(Name) -> ok | {error, IOError} when Name :: string(), IOError :: term().
```

Saves all match specifications saved during the session (by calls to
`tp/2` or `tpl/2`), as well as built-in match specifications, in a text
file with the name designated by `Name`.

`wtp` stands for **w**rite **t**race **p**atterns.

The format of the file is textual, which means that it can be edited
with a text editor, and then restored with `rtp/1`.

Each match specification in the file ends with a period (`.`) and
new (syntactically correct) match specifications can be added to the
file manually.

The function returns `ok`, or an error tuple where the second element
indicates the reason that writing the file failed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
