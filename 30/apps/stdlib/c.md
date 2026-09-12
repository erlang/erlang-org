# `c`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/c.erl#L22)

Command line interface module.

This module enables users to enter the short form of some commonly used
commands.

> #### Note {: .info }
>
> These functions are intended for interactive use in the Erlang shell only. The
> module prefix can be omitted.

### See Also

`m:filename`, `m:compile`, `m:erlang`, `m:yecc`, `m:xref`

# `cmd_line_arg`
*not exported* 

```erlang
-type cmd_line_arg() :: atom() | string().
```

# `h_return`
*not exported* 

```erlang
-type h_return() :: ok | {error, missing | {unknown_format, unicode:chardata()}}.
```

# `hcb_return`
*not exported* 

```erlang
-type hcb_return() :: h_return() | {error, callback_missing}.
```

# `hf_return`
*not exported* 

```erlang
-type hf_return() :: h_return() | {error, function_missing}.
```

# `ht_return`
*not exported* 

```erlang
-type ht_return() :: h_return() | {error, type_missing}.
```

# `bt`

```erlang
-spec bt(Pid) -> ok | undefined when Pid :: pid().
```

Stack backtrace for a process. Equivalent to
`erlang:process_display(Pid, backtrace)`.

# `c`

```erlang
-spec c(Module) -> {ok, ModuleName} | error when Module :: file:name(), ModuleName :: module().
```

Works like [`c(Module, [])`](`c/2`).

# `c`

```erlang
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

```erlang
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

```erlang
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

```erlang
-spec erlangrc(PathList) -> {ok, file:filename()} | {error, term()}
                  when PathList :: [Dir :: file:name()].
```

Search `PathList` and load `.erlang` resource file if found.

# `flush`

```erlang
-spec flush() -> ok.
```

Flushes any messages sent to the shell.

# `h`
*since OTP 23.0* 

```erlang
-spec h(module()) -> h_return().
```

Print the documentation for `Module`

# `h`
*since OTP 23.0* 

```erlang
-spec h(module(), function()) -> hf_return().
```

Print the documentation for all `Module:Function`s (regardless of arity).

# `h`
*since OTP 23.0* 

```erlang
-spec h(module(), function(), arity()) -> hf_return().
```

Print the documentation for `Module:Function/Arity`.

# `hcb`
*since OTP 23.0* 

```erlang
-spec hcb(module()) -> h_return().
```

Print the callback documentation for `Module`

# `hcb`
*since OTP 23.0* 

```erlang
-spec hcb(module(), Callback :: atom()) -> hcb_return().
```

Print the callback documentation for all `Module:Callback`s (regardless of
arity).

# `hcb`
*since OTP 23.0* 

```erlang
-spec hcb(module(), Callback :: atom(), arity()) -> hcb_return().
```

Print the callback documentation for `Module:Callback/Arity`.

# `help`

```erlang
-spec help() -> ok.
```

Displays help information: all valid shell internal commands, and commands in
this module.

# `ht`
*since OTP 23.0* 

```erlang
-spec ht(module()) -> h_return().
```

Print the type documentation for `Module`

# `ht`
*since OTP 23.0* 

```erlang
-spec ht(module(), Type :: atom()) -> ht_return().
```

Print the type documentation for `Type` in `Module` regardless of arity.

# `ht`
*since OTP 23.0* 

```erlang
-spec ht(module(), Type :: atom(), arity()) -> ht_return().
```

Print the type documentation for `Type/Arity` in `Module`.

# `i`

```erlang
-spec i() -> ok.
```

# `i`

```erlang
-spec i(X, Y, Z) -> [{atom(), term()}]
           when X :: non_neg_integer(), Y :: non_neg_integer(), Z :: non_neg_integer().
```

Old alias for `pi(X, Y, Z)`. Note that the output of `i(X, Y, Z)` is
very different from that of `i()`, so the new name is preferred.

# `l`

```erlang
-spec l(Module) -> code:load_ret() when Module :: module().
```

Purges and loads, or reloads, a module by calling `code:purge(Module)` followed
by `code:load_file(Module)`.

Notice that purging the code means that any processes lingering in old code for
the module are killed without warning. For more information, see `code/3`.

# `lc`

```erlang
-spec lc(Files) -> ok | error when Files :: [File :: cmd_line_arg()].
```

Compiles a list of files by calling
`compile:file(File, [report_errors, report_warnings])` for each `File` in
`Files`.

For information about `File`, see `t:file:filename/0`.

# `lm`
*since OTP 20.0* 

```erlang
-spec lm() -> [code:load_ret()].
```

Reloads all currently loaded modules that have changed on disk (see `mm/0`).
Returns the list of results from calling [`l(M)`](`l/1`) for each such `M`.

# `ls`

```erlang
-spec ls() -> ok.
```

Lists files in the current directory.

# `ls`

```erlang
-spec ls(Dir) -> ok when Dir :: file:name().
```

Lists files in directory `Dir` or, if `Dir` is a file, only lists it.

# `m`

```erlang
-spec m() -> ok.
```

Displays information about the loaded modules, including the files from which
they have been loaded.

# `m`

```erlang
-spec m(Module) -> ok when Module :: module().
```

Displays information about `Module`.

# `memory`

```erlang
-spec memory() -> [{Type, Size}] when Type :: atom(), Size :: non_neg_integer().
```

Memory allocation information. Equivalent to `erlang:memory/0`.

# `memory`

```erlang
-spec memory(Type) -> Size when Type :: atom(), Size :: non_neg_integer();
            (Types) -> [{Type, Size}] when Types :: [Type], Type :: atom(), Size :: non_neg_integer().
```

Memory allocation information. Equivalent to `erlang:memory/1`.

# `mm`
*since OTP 20.0* 

```erlang
-spec mm() -> [module()].
```

Lists all modified modules. Shorthand for `code:modified_modules/0`.

# `nc`

```erlang
-spec nc(File) -> {ok, Module} | error when File :: file:name(), Module :: module().
```

# `nc`

```erlang
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

```erlang
-spec ni() -> ok.
```

`i/0` displays system information, listing information about all processes.
`ni/0` does the same, but for all nodes in the network.

# `nl`

```erlang
-spec nl(Module) -> abcast | error when Module :: module().
```

Loads `Module` on all nodes.

# `nregs`

```erlang
-spec nregs() -> ok.
```

# `pi`
*since OTP 29.0* 

```erlang
-spec pi(Pid) -> [{atom(), term()}] when Pid :: pid().
```

Displays information about a process, Equivalent to
[`process_info(Pid)`](`process_info/1`), but location transparent.

# `pi`
*since OTP 29.0* 

```erlang
-spec pi(X, Y, Z) -> [{atom(), term()}]
            when X :: non_neg_integer(), Y :: non_neg_integer(), Z :: non_neg_integer().
```

Equivalent to `pi(pid(X, Y, Z))`.

# `pid`

```erlang
-spec pid(X, Y, Z) -> pid() when X :: non_neg_integer(), Y :: non_neg_integer(), Z :: non_neg_integer().
```

Converts `X`, `Y`, `Z` to pid `<X.Y.Z>`. This function is only to be used when
debugging.

# `pwd`

```erlang
-spec pwd() -> ok.
```

Prints the name of the working directory.

# `q`

```erlang
-spec q() -> no_return().
```

This function is shorthand for `init:stop()`, that is, it causes the node to
stop in a controlled fashion.

# `regs`

```erlang
-spec regs() -> ok.
```

`regs/0` displays information about all registered processes. `nregs/0` does the
same, but for all nodes in the network.

# `uptime`
*since OTP 18.0* 

```erlang
-spec uptime() -> ok.
```

Prints the node uptime (as specified by `erlang:statistics(wall_clock)`) in
human-readable form.

# `xm`

```erlang
-spec xm(module() | file:filename()) -> XRefMRet :: term().
```

Finds undefined functions, unused functions, and calls to deprecated functions
in a module by calling `xref:m/1`.

# `y`

```erlang
-spec y(file:name()) -> YeccFileRet :: term().
```

Generates an LALR-1 parser. Equivalent to:

```text
yecc:file(File)
```

For information about `File = name()`, see `m:filename`. For information about
`YeccRet`, see [`yecc:file/2`](`yecc:file/1`).

# `y`

```erlang
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
