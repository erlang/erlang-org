# `c`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/c.erl#L22)

Command line interface module.

This module enables users to enter the short form of some commonly used
commands.

> #### Note {: .info }
>
> These functions are intended for interactive use in the Erlang shell only. The
> module prefix can be omitted.

## See Also

`m:filename`, `m:compile`, `m:erlang`, `m:yecc`, `m:xref`

# `cmd_line_arg`
*not exported* 

```elixir
-type cmd_line_arg() :: atom() | string().
```

# `h_return`
*not exported* 

```elixir
-type h_return() :: ok | {error, missing | {unknown_format, unicode:chardata()}}.
```

# `hcb_return`
*not exported* 

```elixir
-type hcb_return() :: h_return() | {error, callback_missing}.
```

# `hf_return`
*not exported* 

```elixir
-type hf_return() :: h_return() | {error, function_missing}.
```

# `ht_return`
*not exported* 

```elixir
-type ht_return() :: h_return() | {error, type_missing}.
```

# `bt`

```elixir
-spec bt(Pid) -> ok | undefined when Pid :: pid().
```

Stack backtrace for a process. Equivalent to
`erlang:process_display(Pid, backtrace)`.

# `c`

```elixir
-spec c(Module) -> {ok, ModuleName} | error when Module :: file:name(), ModuleName :: module().
```

Works like [`c(Module, [])`](`c/2`).

# `c`

```elixir
-spec c(Module, Options) -> {ok, ModuleName} | error
           when
               Module :: file:name(),
               Options :: [compile:option()] | compile:option(),
               ModuleName :: module().
```

Compiles and then purges and loads the code for a module. `Module` can be either
a module name or a source file path, with or without `.erl` extension.

If `Module` is a string, it is assumed to be a source file path, and the
compiler will attempt to compile the source file with the options `Options`. If
compilation fails, the old object file (if any) is deleted.

If `Module` is an atom, a source file with that exact name or with `.erl`
extension will be looked for. If found, the source file is compiled with the
options `Options`. If compilation fails, the old object file (if any) is
deleted.

If `Module` is an atom and is not the path of a source file, then the code path
is searched to locate the object file for the module and extract its original
compiler options and source path. If the source file is not found in the
original location, `filelib:find_source/1` is used to search for it relative to
the directory of the object file.

The source file is compiled with the the original options appended to the given
`Options`, the output replacing the old object file if and only if compilation
succeeds.

Notice that purging the code means that any processes lingering in old code for
the module are killed without warning. For more information, see the `m:code`
module.

# `c`
*since OTP 20.0* 

```elixir
-spec c(Module, Options, Filter) -> {ok, ModuleName} | error
           when
               Module :: atom(),
               Options :: [compile:option()],
               Filter :: fun((compile:option()) -> boolean()),
               ModuleName :: module().
```

Compiles and then purges and loads the code for module `Module`, which must be
an atom.

The code path is searched to locate the object file for module `Module` and
extract its original compiler options and source path. If the source file is not
found in the original location, `filelib:find_source/1` is used to search for it
relative to the directory of the object file.

The source file is compiled with the the original options appended to the given
`Options`, the output replacing the old object file if and only if compilation
succeeds. The function `Filter` specifies which elements to remove from the
original compiler options before the new options are added. The `Filter` fun
should return `true` for options to keep, and `false` for options to remove.

Notice that purging the code means that any processes lingering in old code for
the module are killed without warning. For more information, see the `m:code`
module.

# `cd`

```elixir
-spec cd(Dir) -> ok when Dir :: file:name().
```

Changes working directory to `Dir`, which can be a relative name, and then
prints the name of the new working directory.

_Example:_

```text
2> cd("../erlang").
/home/ron/erlang
```

# `erlangrc`
*since OTP 21.0* 

```elixir
-spec erlangrc(PathList) -> {ok, file:filename()} | {error, term()}
                  when PathList :: [Dir :: file:name()].
```

Search `PathList` and load `.erlang` resource file if found.

# `flush`

```elixir
-spec flush() -> ok.
```

Flushes any messages sent to the shell.

# `h`
*since OTP 23.0* 

```elixir
-spec h(module()) -> h_return().
```

Print the documentation for `Module`

# `h`
*since OTP 23.0* 

```elixir
-spec h(module(), function()) -> hf_return().
```

Print the documentation for all `Module:Function`s (regardless of arity).

# `h`
*since OTP 23.0* 

```elixir
-spec h(module(), function(), arity()) -> hf_return().
```

Print the documentation for `Module:Function/Arity`.

# `hcb`
*since OTP 23.0* 

```elixir
-spec hcb(module()) -> h_return().
```

Print the callback documentation for `Module`

# `hcb`
*since OTP 23.0* 

```elixir
-spec hcb(module(), Callback :: atom()) -> hcb_return().
```

Print the callback documentation for all `Module:Callback`s (regardless of
arity).

# `hcb`
*since OTP 23.0* 

```elixir
-spec hcb(module(), Callback :: atom(), arity()) -> hcb_return().
```

Print the callback documentation for `Module:Callback/Arity`.

# `help`

```elixir
-spec help() -> ok.
```

Displays help information: all valid shell internal commands, and commands in
this module.

# `ht`
*since OTP 23.0* 

```elixir
-spec ht(module()) -> h_return().
```

Print the type documentation for `Module`

# `ht`
*since OTP 23.0* 

```elixir
-spec ht(module(), Type :: atom()) -> ht_return().
```

Print the type documentation for `Type` in `Module` regardless of arity.

# `ht`
*since OTP 23.0* 

```elixir
-spec ht(module(), Type :: atom(), arity()) -> ht_return().
```

Print the type documentation for `Type/Arity` in `Module`.

# `i`

```elixir
-spec i() -> ok.
```

# `i`

```elixir
-spec i(X, Y, Z) -> [{atom(), term()}]
           when X :: non_neg_integer(), Y :: non_neg_integer(), Z :: non_neg_integer().
```

Displays information about a process, Equivalent to
[`process_info(pid(X, Y, Z))`](`process_info/1`), but location transparent.

# `l`

```elixir
-spec l(Module) -> code:load_ret() when Module :: module().
```

Purges and loads, or reloads, a module by calling `code:purge(Module)` followed
by `code:load_file(Module)`.

Notice that purging the code means that any processes lingering in old code for
the module are killed without warning. For more information, see `code/3`.

# `lc`

```elixir
-spec lc(Files) -> ok | error when Files :: [File :: cmd_line_arg()].
```

Compiles a list of files by calling
`compile:file(File, [report_errors, report_warnings])` for each `File` in
`Files`.

For information about `File`, see `t:file:filename/0`.

# `lm`
*since OTP 20.0* 

```elixir
-spec lm() -> [code:load_ret()].
```

Reloads all currently loaded modules that have changed on disk (see `mm/0`).
Returns the list of results from calling [`l(M)`](`l/1`) for each such `M`.

# `ls`

```elixir
-spec ls() -> ok.
```

Lists files in the current directory.

# `ls`

```elixir
-spec ls(Dir) -> ok when Dir :: file:name().
```

Lists files in directory `Dir` or, if `Dir` is a file, only lists it.

# `m`

```elixir
-spec m() -> ok.
```

Displays information about the loaded modules, including the files from which
they have been loaded.

# `m`

```elixir
-spec m(Module) -> ok when Module :: module().
```

Displays information about `Module`.

# `memory`

```elixir
-spec memory() -> [{Type, Size}] when Type :: atom(), Size :: non_neg_integer().
```

Memory allocation information. Equivalent to `erlang:memory/0`.

# `memory`

```elixir
-spec memory(Type) -> Size when Type :: atom(), Size :: non_neg_integer();
            (Types) -> [{Type, Size}] when Types :: [Type], Type :: atom(), Size :: non_neg_integer().
```

Memory allocation information. Equivalent to `erlang:memory/1`.

# `mm`
*since OTP 20.0* 

```elixir
-spec mm() -> [module()].
```

Lists all modified modules. Shorthand for `code:modified_modules/0`.

# `nc`

```elixir
-spec nc(File) -> {ok, Module} | error when File :: file:name(), Module :: module().
```

# `nc`

```elixir
-spec nc(File, Options) -> {ok, Module} | error
            when
                File :: file:name(),
                Options :: [Option] | Option,
                Option :: compile:option(),
                Module :: module().
```

Compiles and then loads the code for a file on all nodes. `Options` defaults to
`[]`. Compilation is equivalent to:

```erlang
compile:file(File, Options ++ [report_errors, report_warnings])
```

# `ni`

```elixir
-spec ni() -> ok.
```

`i/0` displays system information, listing information about all processes.
`ni/0` does the same, but for all nodes in the network.

# `nl`

```elixir
-spec nl(Module) -> abcast | error when Module :: module().
```

Loads `Module` on all nodes.

# `nregs`

```elixir
-spec nregs() -> ok.
```

# `pid`

```elixir
-spec pid(X, Y, Z) -> pid() when X :: non_neg_integer(), Y :: non_neg_integer(), Z :: non_neg_integer().
```

Converts `X`, `Y`, `Z` to pid `<X.Y.Z>`. This function is only to be used when
debugging.

# `pwd`

```elixir
-spec pwd() -> ok.
```

Prints the name of the working directory.

# `q`

```elixir
-spec q() -> no_return().
```

This function is shorthand for `init:stop()`, that is, it causes the node to
stop in a controlled fashion.

# `regs`

```elixir
-spec regs() -> ok.
```

`regs/0` displays information about all registered processes. `nregs/0` does the
same, but for all nodes in the network.

# `uptime`
*since OTP 18.0* 

```elixir
-spec uptime() -> ok.
```

Prints the node uptime (as specified by `erlang:statistics(wall_clock)`) in
human-readable form.

# `xm`

```elixir
-spec xm(module() | file:filename()) -> XRefMRet :: term().
```

Finds undefined functions, unused functions, and calls to deprecated functions
in a module by calling `xref:m/1`.

# `y`

```elixir
-spec y(file:name()) -> YeccFileRet :: term().
```

Generates an LALR-1 parser. Equivalent to:

```text
yecc:file(File)
```

For information about `File = name()`, see `m:filename`. For information about
`YeccRet`, see [`yecc:file/2`](`yecc:file/1`).

# `y`

```elixir
-spec y(file:name(), [yecc:option()]) -> YeccFileRet :: yecc:yecc_ret().
```

Generates an LALR-1 parser. Equivalent to:

```text
yecc:file(File, Options)
```

For information about `File = name()`, see `m:filename`. For information about
`Options` and `YeccRet`, see [`yecc:file/2`](`yecc:file/1`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
