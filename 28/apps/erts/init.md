# `init`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/erts/preloaded/src/init.erl#L52)

Coordination of system startup.

This module is preloaded and contains the code for the `init` system process
that coordinates the startup of the system. The first function evaluated at
startup is [`boot(BootArgs)`](`boot/1`), where `BootArgs` is a list of
command-line arguments supplied to the Erlang runtime system from the local
operating system; see [`erl(1)`](erl_cmd.md).

`init` reads the boot script, which contains instructions on how to initiate the
system. For more information about boot scripts, see
[`script(4)`](`e:sasl:script.md`).

`init` also contains functions to restart, reboot, and stop the system.

[](){: #flags }

## Command-Line Flags

> #### Warning {: .warning }
>
> The support for loading of code from archive files is experimental. The only
> purpose of releasing it before it is ready is to obtain early feedback. The
> file format, semantics, interfaces, and so on, can be changed in a future
> release.

The `init` module interprets the following command-line flags:

- **`--`** - Everything following `--` up to the next flag is considered plain
  arguments and can be retrieved using `get_plain_arguments/0`.

- **`-code_path_choice Choice`** - Can be set to `strict` or `relaxed`. It
  controls how each directory in the code path is to be interpreted:

  - Strictly as it appears in the `boot script`, or
  - `init` is to be more relaxed and try to find a suitable directory if it can
    choose from a regular `ebin` directory and an `ebin` directory in an archive
    file.

  It defaults to `strict` from OTP 27 and this option is scheduled for removal
  in OTP 28.

- **`-epmd_module Module`** - This flag is deprecated and has been replaced by
  the `kernel` application parameter [`epmd_module`](`e:kernel:kernel_app.md#epmd_module`).

- **`-eval Expr`** - Scans, parses, and evaluates an arbitrary expression `Expr`
  during system initialization. If any of these steps fail (syntax error, parse
  error, or exception during evaluation), Erlang stops with an error message. In
  the following example Erlang is used as a hexadecimal calculator:

  ```text
  % erl -noshell -eval 'R = 16#1F+16#A0, io:format("~.16B~n", [R])' \\
  -s erlang halt
  BF
  ```

  If multiple `-eval` expressions are specified, they are evaluated sequentially
  in the order specified. `-eval` expressions are evaluated sequentially with
  `-s` and `-run` function calls (this also in the order specified). As with
  `-s` and `-run`, an evaluation that does not terminate blocks the system
  initialization process.

- **`-extra`** - Everything following `-extra` is considered plain arguments and
  can be retrieved using `get_plain_arguments/0`.

  Example:

  ```erlang
  % erl -extra +A 1 --
  ...
  1> init:get_plain_arguments().
  ["+A","1","--"]
  ```

  The `-extra` flag can be passed on the command line, through `ERL_*FLAGS` or
  `-args_file`. It only effects the remaining command-line flags in the entity
  in which it is passed. If multiple `-extra` flags are passed they are
  concatenated using the same order rules as `ERL_*FLAGS` or `-args_file` in
  which they are given.

  Example:

  ```text
  % ERL_AFLAGS="-extra a" ERL_ZFLAGS="-extra d" erl -extra b -extra c
  ...
  1> init:get_plain_arguments().
  ["a","b","-extra","c","d"]
  ```

- **`-S Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function call
  during system initialization. `Func` defaults to `start`. If no arguments are
  provided, the function is assumed to be of arity 0. Otherwise it is assumed to
  be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments
  are passed as strings. If an exception is raised, Erlang stops with an error
  message.

  Example:

  ```text
  % erl -S httpd serve --port 8080 /var/www/html
  ```

  This starts the Erlang runtime system and evaluates the function
  `httpd:serve(["--port", "8080", "/var/www/html"])`. All arguments up to the
  end of the command line will be passed to the called function.

  The function is executed sequentially in an initialization process, which then
  terminates normally and passes control to the user. This means that a `-S`
  call that does not return blocks further processing; to avoid this, use some
  variant of `spawn` in such cases.

  The `-S` flag is only allowed on the command line. If passed through
  `ERL_*FLAGS` or `-args_file` it will be parsed as a normal command line flag.

- **`-run Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function
  call during system initialization. `Func` defaults to `start`. If no arguments
  are provided, the function is assumed to be of arity 0. Otherwise it is
  assumed to be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All
  arguments are passed as strings. If an exception is raised, Erlang stops with
  an error message.

  Example:

  ```text
  % erl -run foo -run foo bar -run foo bar baz 1 2
  ```

  This starts the Erlang runtime system and evaluates the following functions:

  ```text
  foo:start()
  foo:bar()
  foo:bar(["baz", "1", "2"]).
  ```

  The functions are executed sequentially in an initialization process, which
  then terminates normally and passes control to the user. This means that a
  `-run` call that does not return blocks further processing; to avoid this, use
  some variant of `spawn` in such cases.

  > #### Note {: .info }
  >
  > This flag will not forward arguments beginning with a hyphen (-) to the
  > specified function, as these will be interpreted as flags to the runtime. If
  > the function uses flags in this form, it is advised to use `-S` instead.

- **`-s Mod [Func [Arg1, Arg2, ...]]`** - Evaluates the specified function call
  during system initialization. `Func` defaults to `start`. If no arguments are
  provided, the function is assumed to be of arity 0. Otherwise it is assumed to
  be of arity 1, taking the list `[Arg1,Arg2,...]` as argument. All arguments
  are passed as atoms. If an exception is raised, Erlang stops with an error
  message.

  Example:

  ```text
  % erl -s foo -s foo bar -s foo bar baz 1 2
  ```

  This starts the Erlang runtime system and evaluates the following functions:

  ```text
  foo:start()
  foo:bar()
  foo:bar([baz, '1', '2']).
  ```

  The functions are executed sequentially in an initialization process, which
  then terminates normally and passes control to the user. This means that a
  `-s` call that does not return blocks further processing; to avoid this, use
  some variant of `spawn` in such cases.

  Because of the limited length of atoms, it is recommended to use `-run`
  instead.

  > #### Note {: .info }
  >
  > This flag will not forward arguments beginning with a hyphen (-) to the
  > specified function, as these will be interpreted as flags to the runtime. If
  > the function uses flags in this form, it is advised to use `-S` instead,
  > with the additional caveat that arguments are passed as strings instead of
  > atoms.

## Example

```erlang
% erl -- a b -children thomas claire -ages 7 3 -- x y
...

1> init:get_plain_arguments().
["a","b","x","y"]
2> init:get_argument(children).
{ok,[["thomas","claire"]]}
3> init:get_argument(ages).
{ok, [["7","3"]]}
4> init:get_argument(silly).
error
```

## See Also

`m:erl_prim_loader`, `m:heart`

# `internal_status`

```elixir
-type internal_status() :: starting | started | stopping.
```

Current status of init.

# `mode`

```elixir
-type mode() :: embedded | interactive.
```

Code loading mode.

# `boot`

```elixir
-spec boot(BootArgs) -> no_return() when BootArgs :: [binary()].
```

Starts the Erlang runtime system.

This function is called when the emulator is started and coordinates system startup.

`BootArgs` are all command-line arguments except the emulator flags, that is,
flags and plain arguments; see [`erl(1)`](erl_cmd.md).

`init` interprets some of the flags, see section
[Command-Line Flags](`m:init#flags`) below. The remaining flags ("user flags")
and plain arguments are passed to the `init` loop and can be retrieved by
calling `get_arguments/0` and `get_plain_arguments/0`, respectively.

# `get_argument`

```elixir
-spec get_argument(Flag) -> {ok, Arg} | error when Flag :: atom(), Arg :: [Values :: [string()]].
```

Returns all values associated with the command-line user flag `Flag`.

If `Flag` is provided several times, each `Values` is returned in preserved order.
Example:

```erlang
% erl -a b c -a d
...
1> init:get_argument(a).
{ok,[["b","c"],["d"]]}
```

The following flags are defined automatically and can be retrieved using this
function:

- **`root`** - The installation directory of Erlang/OTP, `$ROOT`:

  ```text
  2> init:get_argument(root).
  {ok,[["/usr/local/otp/releases/otp_beam_solaris8_r10b_patched"]]}
  ```

- **`progname`** - The name of the program which started Erlang:

  ```erlang
  3> init:get_argument(progname).
  {ok,[["erl"]]}
  ```

- **`home`{: #home }** - The home directory (on Unix, the value of $HOME):

  ```erlang
  4> init:get_argument(home).
  {ok,[["/home/harry"]]}
  ```

Returns `error` if no value is associated with `Flag`.

# `get_arguments`

```elixir
-spec get_arguments() -> Flags when Flags :: [{Flag :: atom(), Values :: [string()]}].
```

Returns all command-line flags and the system-defined flags, see
`get_argument/1`.

# `get_plain_arguments`

```elixir
-spec get_plain_arguments() -> [Arg] when Arg :: string().
```

Returns any plain command-line arguments as a list of strings (possibly empty).

# `get_status`

```elixir
-spec get_status() -> {InternalStatus, ProvidedStatus}
                    when InternalStatus :: internal_status(), ProvidedStatus :: term().
```

The current status of the `init` process can be inspected.

During system startup (initialization), `InternalStatus` is `starting`, and
`ProvidedStatus` indicates how far the boot script has been interpreted. Each
`{progress, Info}` term interpreted in the boot script affects `ProvidedStatus`,
that is, `ProvidedStatus` gets the value of `Info`.

# `reboot`

```elixir
-spec reboot() -> ok.
```

Reboot the Erlang node.

All applications are taken down smoothly, all code is unloaded, and all ports
are closed before the system terminates.

If command-line flag `-heart` was specified, the `heart` program tries to reboot
 the system. For more information, see `m:heart`.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.

# `restart`

```elixir
-spec restart() -> ok.
```

The same as [`restart([])`](`restart/1`).

# `restart`
*since OTP 23.0* 

```elixir
-spec restart([{mode, mode()}]) -> ok.
```

Restart all Erlang applications.

The system is restarted _inside_ the running Erlang node, which means that the
emulator is not restarted. All applications are taken down smoothly, all code is
unloaded, and all ports are closed before the system is booted again in the same
way as initially started.

The same `BootArgs` are used when restarting the system unless the `mode` option
is given, allowing the code loading mode to be set to either `embedded` or
`interactive`. All other `BootArgs` remain the same.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.

# `script_id`

```elixir
-spec script_id() -> Id when Id :: term().
```

Gets the identity of the boot script used to boot the system.

`Id` can be any Erlang term. In the delivered boot scripts, `Id` is `{Name, Vsn}`.
`Name` and `Vsn` are strings.

# `stop`

```elixir
-spec stop() -> ok.
```

The same as [`stop(0)`](`stop/1`).

# `stop`

```elixir
-spec stop(Status) -> ok when Status :: non_neg_integer() | string().
```

Stop the Erlang node.

All applications are taken down smoothly, all code is unloaded, and all ports
are closed before the system terminates by calling [`halt(Status)`](`halt/1`).
If command-line flag `-heart` was specified, the `heart` program is terminated
before the Erlang node terminates. For more information, see `m:heart`.

To limit the shutdown time, the time `init` is allowed to spend taking down
applications, command-line flag `-shutdown_time` is to be used.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
