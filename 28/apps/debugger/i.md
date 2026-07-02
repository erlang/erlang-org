# `i`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/debugger/src/i.erl#L24)

Debugger/Interpreter Interface.

The `i` module provides short forms for some of the functions used by the
graphical Debugger and some of the functions in module `m:int`, the Erlang
interpreter.

This module also provides facilities for displaying status information about
interpreted processes and break points.

It is possible to attach to interpreted processes by only giving the corresponding
process identity. By default, an attachment window is displayed. Processes
at other Erlang nodes can be attached manually or automatically.

The functions in this module are defined in the Erlang shell. That is,
they can be called without the `i:` prefix. For example:

```erlang
1> ii(t).
{module,t}
2> iaa([init]).
true
```

# `help`

```elixir
-spec help() -> ok.
```

Prints help for using the functions in this module.

# `ia`

```elixir
-spec ia(Pid) -> ok | no_proc when Pid :: pid().
```

Attaches to the debugged process `Pid`.

An "Attach Process" window is opened for the process.

# `ia`

```elixir
-spec ia(Pid, Function) -> ok | no_proc
            when Pid :: pid(), Function :: {Module, Name}, Module :: module(), Name :: atom().
```

Attaches to the debugged process `Pid`.

The interpreter calls [`spawn(Module, Name, [Pid])`](`spawn/3`) (and
ignores the result).

# `ia`

```elixir
-spec ia(X, Y, Z) -> ok | no_proc when X :: integer(), Y :: integer(), Z :: integer().
```

Equivalent to [`ia(Pid)`](`ia/1`), where `Pid` is the result of calling the shell
function `pid(X, Y, Z)`.

# `ia`

```elixir
-spec ia(X, Y, Z, Function) -> ok | no_proc
            when
                X :: integer(),
                Y :: integer(),
                Z :: integer(),
                Function :: {Module, Name},
                Module :: module(),
                Name :: atom().
```

Equivalent to [`ia(Pid, Function)`](`ia/2`), where `Pid` is the result of calling the
shell function `pid(X, Y, Z)`.

An attached process is expected to call the unofficial function
`int:attached(Pid)` and to be able to handle messages from the
interpreter. For an example, see `dbg_wx_trace.erl`.

# `iaa`

```elixir
-spec iaa(Flags) -> true when Flags :: [init | break | exit].
```

Sets when to attach to a debugged process automatically.

Debugger supplies a function that opens "Attach Process" window for
the process.

See [int:auto_attach/2](`int:auto_attach/2`) for more information.

# `iaa`

```elixir
-spec iaa(Flags, Function) -> true
             when
                 Flags :: [init | break | exit],
                 Function :: {Module, Name, Args},
                 Module :: module(),
                 Name :: atom(),
                 Args :: [term()].
```

Sets when and how to attach to a debugged process automatically.

See [int:auto_attach/2](`int:auto_attach/2`) for more information.

# `ib`

```elixir
-spec ib(Module, Line) -> ok | {error, break_exists} when Module :: module(), Line :: integer().
```

Creates a breakpoint at `Line` in `Module`.

# `ib`

```elixir
-spec ib(Module, Name, Arity) -> ok | {error, function_not_found}
            when Module :: module(), Name :: atom(), Arity :: integer().
```

Creates breakpoints at the first line of every clause of function
`Module:Name/Arity`.

# `iba`

```elixir
-spec iba(Module, Line, Action) -> ok
             when Module :: module(), Line :: integer(), Action :: enable | disable | delete.
```

Sets the trigger action of the breakpoint at `Line` in `Module` to `Action`.

# `ibc`

```elixir
-spec ibc(Module, Line, Function) -> ok
             when Module :: module(), Line :: integer(), Function :: {Module, Name}, Name :: atom().
```

Sets the conditional test of the breakpoint at `Line` in `Module` to `Function`.

The conditional test is performed by calling `Module:Name(Bindings)`, where
`Bindings` is the current variable bindings. The function must return `true`
(break) or `false` (do not break). To retrieve the value of a variable `Var`
use [int:get_binding(Var, Bindings)](`int:get_binding/2`).

# `ibd`

```elixir
-spec ibd(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Makes the breakpoint at `Line` in `Module` inactive.

# `ibe`

```elixir
-spec ibe(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Makes the breakpoint at `Line` in `Module` active.

# `ic`

```elixir
-spec ic() -> ok.
```

Clears information about processes executing interpreted code by removing all
information about terminated processes.

# `ii`

```elixir
-spec ii(AbsModules | AbsModule) -> Result
            when
                AbsModules :: [AbsModule, ...],
                AbsModule :: Module | File,
                Module :: module(),
                File :: file:name_all(),
                Result :: AbsModuleResult | AbsModulesResult,
                AbsModuleResult :: {module, Module} | error,
                AbsModulesResult :: ok.
```

Interprets the specified module(s) on the local node.

- If `AbsModule :: Module | File`, then `Result :: {module, Module} | error`.
- If `AbsModules :: [AbsModule]`, then `Result :: ok`.

See `int:i/1` for more information.

# `il`

```elixir
-spec il() -> ok.
```

Makes a printout of all interpreted modules.

Modules are printed together with the full path name of the
corresponding source code file.

# `im`

```elixir
-spec im() -> pid().
```

Starts a new graphical Monitor window.

This is the Monitor window, the main window of Debugger. All the
Debugger and interpreter functionality is accessed from the Monitor
window. This window displays the status of all processes that have
been or are executing interpreted modules.

# `ini`

```elixir
-spec ini(AbsModules | AbsModule) -> Result
             when
                 AbsModules :: [AbsModule],
                 AbsModule :: Module | File,
                 Module :: module(),
                 File :: file:name_all(),
                 Result :: AbsModuleResult | AbsModulesResult,
                 AbsModuleResult :: {module, Module} | error,
                 AbsModulesResult :: ok.
```

Interprets the specified module(s) on all known nodes.

- If `AbsModule :: Module | File`, then `Result :: {module, Module} | error`.
- If `AbsModules :: [AbsModule]`, then `Result :: ok`.

See `int:ni/1` for more information.

# `inq`

```elixir
-spec inq(AbsModule) -> ok when AbsModule :: Module | File, Module :: module(), File :: file:name_all().
```

Stops interpreting the specified module on all known nodes.

# `ip`

```elixir
-spec ip() -> ok.
```

Prints the current status of all interpreted processes.

# `ipb`

```elixir
-spec ipb() -> ok.
```

Prints all existing breakpoints.

# `ipb`

```elixir
-spec ipb(Module) -> ok when Module :: module().
```

Prints all existing breakpoints in `Module`.

# `iq`

```elixir
-spec iq(AbsModule) -> ok when AbsModule :: Module | File, Module :: module(), File :: file:name_all().
```

Stops interpreting the specified module on the local node.

# `ir`

```elixir
-spec ir() -> ok.
```

Deletes all breakpoints in all interpreted modules.

# `ir`

```elixir
-spec ir(Module) -> ok when Module :: module().
```

Deletes all breakpoints in `Module`.

# `ir`

```elixir
-spec ir(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Deletes the breakpoint at `Line` in `Module`.

# `ir`

```elixir
-spec ir(Module, Name, Arity) -> ok | {error, function_not_found}
            when Module :: module(), Name :: atom(), Arity :: integer().
```

Deletes the breakpoints at the first line of every clause of function
`Module:Name/Arity`.

# `ist`

```elixir
-spec ist(Flag) -> true when Flag :: all | no_tail | false.
```

Sets how to save call frames in the stack.

See [int:stack_trace/1](`int:stack_trace/0`) for more information.

# `iv`

```elixir
-spec iv() -> atom().
```

Returns the current version of the interpreter (Debugger).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
