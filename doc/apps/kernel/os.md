# `os`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/os.erl#L22)

Operating system-specific functions.

The functions in this module are operating system-specific. Careless use of
these functions results in programs that will only run on a specific platform.
On the other hand, with careful use, these functions can be of help in enabling
a program to run on most platforms.

> #### Note {: .info }
>
> The functions in this module will raise a `badarg` exception if their
> arguments contain invalid characters according to the description in the "Data
> Types" section.

# `env_var_name`

```erlang
-type env_var_name() :: nonempty_string().
```

A string containing valid characters on the specific OS for environment variable
names using [`file:native_name_encoding()`](`file:native_name_encoding/0`)
encoding.

Null characters (integer value zero) are not allowed. On Unix, `=`
characters are not allowed. On Windows, a `=` character is only allowed as the
very first character in the string.

# `env_var_name_value`

```erlang
-type env_var_name_value() :: nonempty_string().
```

Assuming that environment variables has been correctly set, a strings containing
valid characters on the specific OS for environment variable names and values
using [`file:native_name_encoding()`](`file:native_name_encoding/0`) encoding.

The first `=` characters appearing in the string separates environment variable
name (on the left) from environment variable value (on the right).

# `env_var_value`

```erlang
-type env_var_value() :: string().
```

A string containing valid characters on the specific OS for environment variable
values using [`file:native_name_encoding()`](`file:native_name_encoding/0`)
encoding.

Null characters (integer value zero) are not allowed.

# `os_command`

```erlang
-type os_command() :: atom() | io_lib:chars().
```

All characters needs to be valid characters on the specific OS using
[`file:native_name_encoding()`](`file:native_name_encoding/0`) encoding. Null
characters (integer value zero) are not allowed.

# `os_command_opts`

```erlang
-type os_command_opts() ::
          #{max_size => non_neg_integer() | infinity, exception_on_failure => boolean()}.
```

Options for [`os:cmd/2`](`cmd/2`).

- **`max_size`** - The maximum size of the data returned by the `os:cmd/2` call.
  See the [`os:cmd/2`](`cmd/2`) documentation for more details.

  Since OTP 20.2.3
- **`exception_on_failure`** - If set to true, `cmd/2` will throw an error exception if
  the command exits with a non-zero exit code.

  Since OTP 28.0

# `cmd`

```erlang
-spec cmd(Command) -> string() when Command :: os_command().
```

# `cmd`
*since OTP 20.2.3* 

```erlang
-spec cmd(Command, Options) -> string() when Command :: os_command(), Options :: os_command_opts().
```

Executes `Command` in a command shell of the target OS, captures the standard
output and standard error of the command, and returns this result as a string.

_Examples:_

```erlang
LsOut = os:cmd("ls"), % on unix platform
DirOut = os:cmd("dir"), % on Win32 platform
```

Notice that in some cases, standard output of a command when called from another
program can differ, compared with the standard output of the command when called
directly from an OS command shell.

The possible options are:

- **`max_size`** - The maximum size of the data returned by the `os:cmd/2` call.
  This option is a safety feature that should be used when the command executed
  can return a very large, possibly infinite, result.

  _Example_:

  ```erlang
  > os:cmd("cat /dev/zero", #{ max_size => 20 }).
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ```

  Since OTP 20.2.3
- **`exception_on_failure`** - If set to true, `os:cmd/2` will throw an error
  exception if the command exits with a non-zero exit code. The exception reason
  looks like this: `{command_failed, ResultBeforeFailure, ExitCode}` where
  `ResultBeforeFailure` is the result written to stdout by the command before
  the error happened and `ExitCode` is the exit code from the command.

  _Example_:

  ```erlang
  > catch os:cmd("echo hello && exit 123", #{ exception_on_failure => true }).
  {'EXIT',{{command_failed,"hello\n",123},
           [{os,cmd,2,[{file,"os.erl"},{line,579}]},
  ...
  ```

  Since OTP 28.0

The command shell can be set using the
[kernel configuration parameter](kernel_app.md#os_cmd_shell), by default the
shell is detected upon system startup.

# `env`
*since OTP 24.0* 

```erlang
-spec env() -> [{env_var_name(), env_var_value()}].
```

Returns a list of all environment variables. Each environment variable is
expressed as a tuple `{VarName,Value}`, where `VarName` is the name of the
variable and `Value` its value.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the strings can
contain characters with codepoints > 255.

# `find_executable`

```erlang
-spec find_executable(Name) -> Filename | false when Name :: string(), Filename :: string().
```

Equivalent to [`find_executable(Name, Path)`](`find_executable/2`) where
`Path` is the current execution path (that is, the environment variable `PATH`
on Unix and Windows).

# `find_executable`

```erlang
-spec find_executable(Name, Path) -> Filename | false
                         when Name :: string(), Path :: string(), Filename :: string().
```

Look up an executable program, with the specified name and a search path, in the
same way as the underlying OS.

`Path` is to conform to the syntax of execution paths on the OS.
Returns the absolute filename of the executable program `Name`, or `false` if
the program is not found.

# `getenv`

```erlang
-spec getenv() -> [env_var_name_value()].
```

Returns a list of all environment variables. Each environment variable is
expressed as a single string on the format `"VarName=Value"`, where `VarName` is
the name of the variable and `Value` its value.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the strings can
contain characters with codepoints > 255.

Consider using `env/0` for a nicer 2-tuple format.

# `getenv`

```erlang
-spec getenv(VarName) -> Value | false when VarName :: env_var_name(), Value :: env_var_value().
```

Returns the `Value` of the environment variable `VarName`, or `false` if the
environment variable is undefined.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the strings
`VarName` and `Value` can contain characters with codepoints > 255.

# `getenv`
*since OTP 18.0* 

```erlang
-spec getenv(VarName, DefaultValue) -> Value
                when
                    VarName :: env_var_name(), DefaultValue :: env_var_value(), Value :: env_var_value().
```

Returns the `Value` of the environment variable `VarName`, or `DefaultValue` if
the environment variable is undefined.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the strings
`VarName` and `Value` can contain characters with codepoints > 255.

# `getpid`

```erlang
-spec getpid() -> Value when Value :: string().
```

Returns the process identifier of the current Erlang emulator in the format most
commonly used by the OS environment.

Returns `Value` as a string containing the (usually) numerical identifier for a process.

- On Unix, this is typically the return value of the `getpid/0` system call.
- On Windows, the process id as returned by the `GetCurrentProcessId()` system call
  is used.

# `perf_counter`
*since OTP 19.0* 

```erlang
-spec perf_counter() -> Counter when Counter :: integer().
```

Returns the current performance counter value in `perf_counter`
[time unit](`t:erlang:time_unit/0`). This is a highly optimized call that
might not be traceable.

# `perf_counter`
*since OTP 19.0* 

```erlang
-spec perf_counter(Unit) -> integer() when Unit :: erlang:time_unit().
```

Returns a performance counter that can be used as a very fast and high
resolution timestamp.

This counter is read directly from the hardware or operating system with the
same guarantees. This means that two consecutive calls to the function are not
guaranteed to be monotonic, though it most likely will be. The performance
counter will be converted to the resolution passed as an argument.

```erlang
1> T1 = os:perf_counter(1000),receive after 10000 -> ok end,T2 = os:perf_counter(1000).
176525861
2> T2 - T1.
10004
```

# `putenv`

```erlang
-spec putenv(VarName, Value) -> true when VarName :: env_var_name(), Value :: env_var_value().
```

Sets a new `Value` for environment variable `VarName`.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the strings
`VarName` and `Value` can contain characters with codepoints > 255.

On Unix platforms, the environment is set using UTF-8 encoding if Unicode
filename translation is in effect. On Windows, the environment is set using wide
character interfaces.

# `set_signal`
*since OTP 20.0* 

```erlang
-spec set_signal(Signal, Option) -> ok
                    when
                        Signal ::
                            sighup | sigquit | sigabrt | sigalrm | sigterm | sigusr1 | sigusr2 |
                            sigchld | sigstop | sigtstp | sigcont | sigwinch | siginfo,
                        Option :: default | handle | ignore.
```

Enables or disables OS signals.

Each signal my be set to one of the following options:

- **`ignore`** - This signal will be ignored.

- **`default`** - This signal will use the default signal handler for the
  operating system.

- **`handle`** - This signal will notify
  [`erl_signal_server`](kernel_app.md#erl_signal_server) when it is received by
  the Erlang runtime system.

# `system_time`
*since OTP 18.0* 

```erlang
-spec system_time() -> integer().
```

Returns the current [OS system time](`e:erts:time_correction.md#os-system-time`)
in `native` [time unit](`t:erlang:time_unit/0`).

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time.

# `system_time`
*since OTP 18.0* 

```erlang
-spec system_time(Unit) -> integer() when Unit :: erlang:time_unit().
```

Returns the current [OS system time](`e:erts:time_correction.md#os-system-time`)
converted into the `Unit` passed as argument.

Calling `os:system_time(Unit)` is equivalent to
[`erlang:convert_time_unit`](`erlang:convert_time_unit/3`)([`os:system_time()`](`system_time/0`)`, native, Unit)`.

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time.

# `timestamp`

```erlang
-spec timestamp() -> Timestamp when Timestamp :: erlang:timestamp().
```

Returns the current [OS system time](`e:erts:time_correction.md#os-system-time`)
in the same format as `erlang:timestamp/0`.

The tuple can be used together with function `calendar:now_to_universal_time/1`
or `calendar:now_to_local_time/1` to get calendar time. Using the calendar time,
together with the `MicroSecs` part of the return tuple from this function,
allows you to log time stamps in high resolution and consistent with the time in
 the rest of the OS.

Example of code formatting a string in format "DD Mon YYYY HH:MM:SS.mmmmmm",
where DD is the day of month, Mon is the textual month name, YYYY is the year,
HH:MM:SS is the time, and mmmmmm is the microseconds in six positions:

```erlang
-module(print_time).
-export([format_utc_timestamp/0]).
format_utc_timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}} =
calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
    "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w",
    [Day,Mstr,Year,Hour,Minute,Second,Micro]).
```

This module can be used as follows:

```erlang
1> io:format("~s~n",[print_time:format_utc_timestamp()]).
29 Apr 2009  9:55:30.051711
```

OS system time can also be retrieved by `system_time/0` and `system_time/1`.

# `type`

```erlang
-spec type() -> {Osfamily, Osname} when Osfamily :: unix | win32, Osname :: atom().
```

Returns the `Osfamily` and, in some cases, the `Osname` of the current OS.

On Unix, `Osname` has the same value as `uname -s` returns, but in lower case.
For example, on Solaris 1 and 2, it is `sunos`.

On Windows, `Osname` is `nt`.

> #### Note {: .info }
>
> Think twice before using this function. Use module `m:filename` if you want to
> inspect or build filenames in a portable way. Avoid matching on atom `Osname`.

# `unsetenv`
*since OTP R16B03* 

```erlang
-spec unsetenv(VarName) -> true when VarName :: env_var_name().
```

Deletes the environment variable `VarName`.

If Unicode filename encoding is in effect (see the
[`erl` manual page](`e:erts:erl_cmd.md#file_name_encoding`)), the string
`VarName` can contain characters with codepoints > 255.

# `version`

```erlang
-spec version() -> VersionString | {Major, Minor, Release}
                 when
                     VersionString :: string(),
                     Major :: non_neg_integer(),
                     Minor :: non_neg_integer(),
                     Release :: non_neg_integer().
```

Returns the OS version. On most systems, this function returns a tuple, but a
string is returned instead if the system has versions that cannot be expressed
as three numbers.

> #### Note {: .info }
>
> Think twice before using this function. If you still need to use it, always
> `call os:type()` first.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
