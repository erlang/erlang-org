# `int`
[🔗](https://github.com/erlang/otp/blob/master/lib/debugger/src/int.erl#L22)

Interpreter Interface.

The Erlang interpreter provides mechanisms for breakpoints and stepwise
execution of code. It is primarily intended to be used by Debugger; see the
[Users's Guide for Debugger](debugger_chapter.md) and module `m:debugger`.

The following can be done from the shell:

- Specify the modules to be interpreted.
- Specify breakpoints.
- Monitor the current status of all processes executing code in interpreted
  modules, also processes at other Erlang nodes.

By _attaching to_ a process executing interpreted code, it is possible to
examine variable bindings and order stepwise execution. This is done by sending
and receiving information to/from the process through a third process, called
the meta process. You can implement your own attached process. See `int.erl` for
available functions and `dbg_wx_trace.erl` for possible messages.

The interpreter depends on the Kernel, STDLIB, and WX applications. This means
that modules belonging to any of these applications are not allowed to be
interpreted, as it could lead to a deadlock or emulator crash. This also applies
to modules belonging to the Debugger application.

[](){: #int_breakpoints }

## Breakpoints

Breakpoints are specified on a line basis. When a process executing code in an
interpreted module reaches a breakpoint, it stops. This means that a breakpoint
must be set at an executable line, that is, a code line containing an executable
expression.

A breakpoint has the following:

- A status, which is _active_ or _inactive_. An inactive breakpoint is ignored.
- A trigger action. When a breakpoint is reached, the trigger action specifies
  if the breakpoint is to continue as active (_enable_), or to become inactive
  (_disable_), or to be removed (_delete_).
- Optionally an associated condition. A condition is a tuple `{Module,Name}`.
  When the breakpoint is reached, `Module:Name(Bindings)` is called. If it
  evaluates to `true`, execution stops. If it evaluates to `false`, the
  breakpoint is ignored. `Bindings` contains the current variable bindings. To
  retrieve the value for a specified variable use `get_binding/2`.

By default, a breakpoint is active, has trigger action `enable`, and has no
associated condition. For details about breakpoints, see
[Breakpoints and Break Dialog
Windows](debugger_chapter.md#breakpoints-and-break-dialog-windows)
in the User's Guide for Debugger.

# `action_at_break`

```erlang
-spec action_at_break(Module, Line, Action) -> ok
                         when Module :: module(), Line :: integer(), Action :: enable | disable | delete.
```

Sets the trigger action of the breakpoint at `Line` in `Module` to `Action`.

# `all_breaks`

```erlang
-spec all_breaks() -> [Break]
                    when
                        Break :: {Point, Options},
                        Point :: {Module, Line},
                        Module :: module(),
                        Line :: integer(),
                        Options :: [Status | Trigger | null | Cond],
                        Status :: active | inactive,
                        Trigger :: enable | disable | delete,
                        Cond :: null | Function,
                        Function :: {Module, Name},
                        Name :: atom().
```

Gets all breakpoints.

# `all_breaks`

```erlang
-spec all_breaks(Module) -> [Break]
                    when
                        Break :: {Point, Options},
                        Point :: {Module, Line},
                        Module :: module(),
                        Line :: integer(),
                        Options :: [Status | Trigger | null | Cond],
                        Status :: active | inactive,
                        Trigger :: enable | disable | delete,
                        Cond :: null | Function,
                        Function :: {Module, Name},
                        Name :: atom().
```

Gets all breakpoints in module `Module`.

# `auto_attach`

```erlang
-spec auto_attach() -> false | {Flags, Function}
                     when
                         Flags :: [init | break | exit],
                         Function :: {Module, Name, Args},
                         Module :: module(),
                         Name :: atom(),
                         Args :: [term()].
```

Gets how to attach automatically to a process executing code in
interpreted modules.

See `auto_attach/2` for the meaning of the possible values in `Flags`.

# `auto_attach`

```erlang
-spec auto_attach(false) -> term().
```

Disables auto attach.

# `auto_attach`

```erlang
-spec auto_attach(Flags, Function) -> term()
                     when
                         Flags :: [init | break | exit],
                         Function :: {Module, Name, Args},
                         Module :: module(),
                         Name :: atom(),
                         Args :: [term()].
```

Sets when and how to attach automatically to a process executing code
in interpreted modules.

By default when the interpreter is started, automatic attach is disabled.

If `Flags` is an empty list, automatic attach is disabled.

Otherwise `Flags` should be a list containing  at least one of the following
flags:

- `init` - Attach when a process for the first time calls an interpreted
  function.
- `break` - Attach whenever a process reaches a breakpoint.
- `exit` - Attach when a process terminates.

When the specified event occurs, the function `Function` is called as:

```erlang
spawn(Module, Name, [Pid | Args])
```

`Pid` is the pid of the process executing interpreted code.

# `break`

```erlang
-spec break(Module, Line) -> ok | {error, break_exists} when Module :: module(), Line :: integer().
```

Creates a breakpoint at `Line` in `Module`.

# `break_in`

```erlang
-spec break_in(Module, Name, Arity) -> ok | {error, function_not_found}
                  when Module :: module(), Name :: atom(), Arity :: integer().
```

Creates a breakpoint at the first line of every clause of function
`Module:Name/Arity`.

# `clear`

```erlang
-spec clear() -> ok.
```

Clears information about processes executing interpreted code by removing all
information about terminated processes.

# `continue`

```erlang
-spec continue(Pid :: pid()) -> ok | {error, not_interpreted}.
```

Resumes process execution for `Pid`.

# `continue`

```erlang
-spec continue(X, Y, Z) -> ok | {error, not_interpreted}
                  when X :: integer(), Y :: integer(), Z :: integer().
```

Resumes process execution for `c:pid(X, Y, Z)`.

# `del_break_in`

```erlang
-spec del_break_in(Module, Name, Arity) -> ok | {error, function_not_found}
                      when Module :: module(), Name :: atom(), Arity :: integer().
```

Deletes the breakpoints at the first line of every clause of function
`Module:Name/Arity`.

# `delete_break`

```erlang
-spec delete_break(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Deletes the breakpoint at `Line` in `Module`.

# `disable_break`

```erlang
-spec disable_break(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Makes the breakpoint at `Line` in `Module` inactive.

# `enable_break`

```erlang
-spec enable_break(Module, Line) -> ok when Module :: module(), Line :: integer().
```

Makes the breakpoint at `Line` in `Module` active.

# `file`

```erlang
-spec file(Module) -> File | {error, not_loaded} when Module :: module(), File :: file:filename_all().
```

Returns the source code filename `File` for an interpreted module `Module`.

# `get_binding`

```erlang
-spec get_binding(Var, Bindings) -> {value, Value} | unbound
                     when Var :: atom(), Bindings :: term(), Value :: term().
```

Retrieves the binding of `Var` from `Bindings`.

This function is intended to be used by the conditional function of a breakpoint.

# `i`

```erlang
-spec i(AbsModules | AbsModule) -> Result
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

A module can be specified by its module name (atom) or filename.

If specified by its module name, the object code `Module.beam` is searched for
in the current path. The source code `Module.erl` is searched for first in the
same directory as the object code, then in an `src` directory next to it.

If specified by its filename, the filename can include a path and the `.erl`
extension can be omitted. The object code `Module.beam` is searched for first in
the same directory as the source code, then in an `ebin` directory next to it,
and then in the current path.

> #### Note {: .info }
>
> The interpreter requires both the source code and the object code. The object
> code _must_ include debug information, that is, only modules compiled with
> option `debug_info` can be interpreted.

The functions returns `{module,Module}` if the module was interpreted, otherwise
`error` is returned.

The argument can also be a list of modules or filenames, in which case the
function tries to interpret each module as specified earlier. The function then
always returns `ok`, but prints some information to `stdout` if a module cannot
be interpreted.

# `interpretable`

```erlang
-spec interpretable(AbsModule) -> true | {error, Reason}
                       when
                           AbsModule :: Module | File,
                           Module :: module(),
                           File :: file:name_all(),
                           Reason :: no_src | no_beam | no_debug_info | badarg | {app, App},
                           App :: atom().
```

Checks if a module can be interpreted.

The module can be specified by its module name `Module` or its source
filename `File`. If specified by a module name, the module is searched
for in the code path.

The function returns `true` if all of the following apply:

- Both source code and object code for the module is found.
- The module has been compiled with option `debug_info` set.
- The module does not belong to any of the applications Kernel, STDLIB, WX, or
  Debugger.

The function returns `{error,Reason}` if the module cannot be interpreted.
`Reason` can have the following values:

- **`no_src`** - No source code is found. It is assumed that the source code and
  object code are located either in the same directory, or in `src` and `ebin`
  directories next to each other.

- **`no_beam`** - No object code is found. It is assumed that the source code
  and object code are located either in the same directory, or in `src` and
  `ebin` directories next to each other.

- **`no_debug_info`** - The module has not been compiled with option
  `debug_info` set.

- **`badarg`** - `AbsModule` is not found. This could be because the specified
  file does not exist, or because `code:which/1` does not return a BEAM
  filename, which is the case not only for non-existing modules but also for
  modules that are preloaded or cover-compiled.

- **`{app,App}`** - `App` is `kernel`, `stdlib`, `gs`, or `debugger` if
  `AbsModule` belongs to one of these applications.

Notice that the function can return `true` for a module that is not
interpretable the module is marked as sticky or resides in a directory
marked as sticky. The reason is that this is not discovered until the
interpreter tries to load the module.

# `interpreted`

```erlang
-spec interpreted() -> [Module] when Module :: module().
```

Returns a list with all interpreted modules.

# `n`

```erlang
-spec n(AbsModule) -> ok
           when
               AbsModule :: Module | File | [Module | File], Module :: module(), File :: file:name_all().
```

Stops interpreting the specified module on the local node.

Similar to [`i/1`](`i/1`) and [`ni/1`](`ni/1`), a module can be specified by its
module name or filename.

# `ni`

```erlang
-spec ni(AbsModules | AbsModule) -> Result
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

A module can be specified by its module name (atom) or filename.

If specified by its module name, the object code `Module.beam` is searched for
in the current path. The source code `Module.erl` is searched for first in the
same directory as the object code, then in an `src` directory next to it.

If specified by its filename, the filename can include a path and the `.erl`
extension can be omitted. The object code `Module.beam` is searched for first in
the same directory as the source code, then in an `ebin` directory next to it,
and then in the current path.

> #### Note {: .info }
>
> The interpreter requires both the source code and the object code. The object
> code _must_ include debug information, that is, only modules compiled with
> option `debug_info` can be interpreted.

The functions returns `{module,Module}` if the module was interpreted, otherwise
`error` is returned.

The argument can also be a list of modules or filenames, in which case the
function tries to interpret each module as specified earlier. The function then
always returns `ok`, but prints some information to `stdout` if a module cannot
be interpreted.

# `nn`

```erlang
-spec nn(AbsModule) -> ok
            when
                AbsModule :: Module | File | [Module | File],
                Module :: module(),
                File :: file:name_all().
```

Stops interpreting the specified module on all known nodes.

Similar to [`i/1`](`i/1`) and [`ni/1`](`ni/1`), a module can be specified by its
module name or filename.

# `no_break`

```erlang
-spec no_break() -> ok.
```

Deletes all breakpoints.

# `no_break`

```erlang
-spec no_break(Module :: term()) -> ok.
```

Deletes all breakpoints in `Module`.

# `snapshot`

```erlang
-spec snapshot() -> [Snapshot]
                  when
                      Snapshot :: {Pid, Function, Status, Info},
                      Pid :: pid(),
                      Function :: {Module, Name, Args},
                      Module :: module(),
                      Name :: atom(),
                      Args :: [term()],
                      Status :: idle | running | waiting | break | exit | no_conn,
                      Info :: {} | {Module, Line} | ExitReason,
                      Line :: integer(),
                      ExitReason :: term().
```

Gets information about all processes executing interpreted code.

- `Pid` - Process identifier.
- `Function` - First interpreted function called by the process.
- `Status` - Current status of the process.
- `Info` - More information.

`Status` is one of the following:

- `idle` - The process is no longer executing interpreted code.
  `Info` is `{}`.
- `running` - The process is running. `Info` is `{}`.
- `waiting` - The process is waiting at a `receive`. `Info` is `{}`.
- `break` - Process execution is stopped, normally at a breakpoint.
  `Info` is `{Module,Line}`.
- `exit` - The process is terminated. `Info` is `ExitReason`.
- `no_conn` - The connection is down to the node where the process is running.
  `Info` is `{}`.

# `stack_trace`

```erlang
-spec stack_trace() -> Flag when Flag :: all | no_tail | false.
```

Gets how to save call frames in the stack.

See `stack_trace/1` for the meaning of `Flag`.

# `stack_trace`

```erlang
-spec stack_trace(Flag) -> term() when Flag :: all | no_tail | false.
```

Sets how to save call frames in the stack.

Saving call frames makes it possible to inspect the call chain of a
process, and is also used to emulate the stack trace if an error (an
exception of class error) occurs. The following flags can be
specified:

- **`all`** - Save information about all current calls, that is, function calls
  that have not yet returned a value.

- **`no_tail`** - Save information about current calls, but discard previous
  information when a tail-recursive call is made. This option consumes less
  memory and can be necessary to use for processes with long lifetimes and many
  tail-recursive calls. This is the default.

- **`false`** - Save no information about current calls.

# `test_at_break`

```erlang
-spec test_at_break(Module, Line, Function) -> ok
                       when
                           Module :: module(),
                           Line :: integer(),
                           Function :: {Module, Name},
                           Name :: atom().
```

Sets the conditional test of the breakpoint at `Line` in `Module` to `Function`.

Function `Function` must fulfill the requirements specified in section
[Breakpoints](`m:int#int_breakpoints`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
