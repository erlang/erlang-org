# `heart`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/heart.erl#L22)

Heartbeat monitoring of an Erlang runtime system.

This modules contains the interface to the `heart` process. `heart` sends
periodic heartbeats to an external port program, which is also named `heart`.
The purpose of the `heart` port program is to check that the Erlang runtime
system it is supervising is still running. If the port program has not received
any heartbeats within `HEART_BEAT_TIMEOUT` seconds (defaults to 60 seconds), the
system can be rebooted.

An Erlang runtime system to be monitored by a heart program is to be started
with command-line flag `-heart` (see also [`erl(1)`](`e:erts:erl_cmd.md`)). The
`heart` process is then started automatically:

```text
% erl -heart ...
```

If the system is to be rebooted because of missing heartbeats, or a terminated
Erlang runtime system, environment variable `HEART_COMMAND` must be set before
the system is started. If this variable is not set, a warning text is printed
but the system does not reboot.

To reboot on Windows, `HEART_COMMAND` can be set to `heart -shutdown` (included
in the Erlang delivery) or to any other suitable program that can activate a
reboot.

The environment variable `HEART_BEAT_TIMEOUT` can be used to configure the heart
time-outs; it can be set in the operating system shell before Erlang is started
or be specified at the command line:

```text
% erl -heart -env HEART_BEAT_TIMEOUT 30 ...
```

The value (in seconds) must be in the range `10 < X <= 65535`.

When running on OSs lacking support for monotonic time, `heart` is susceptible
to system clock adjustments of more than `HEART_BEAT_TIMEOUT` seconds. When this
happens, `heart` times out and tries to reboot the system. This can occur, for
example, if the system clock is adjusted automatically by use of the Network
Time Protocol (NTP).

If a crash occurs, an `erl_crash.dump` is _not_ written unless environment
variable `ERL_CRASH_DUMP_SECONDS` is set:

```text
% erl -heart -env ERL_CRASH_DUMP_SECONDS 10 ...
```

If a regular core dump is wanted, let `heart` know by setting the kill signal to
abort using environment variable `HEART_KILL_SIGNAL=SIGABRT`. If unset, or not
set to `SIGABRT`, the default behavior is a kill signal using `SIGKILL`:

```text
% erl -heart -env HEART_KILL_SIGNAL SIGABRT ...
```

If heart should _not_ kill the Erlang runtime system, this can be indicated
using the environment variable `HEART_NO_KILL=TRUE`. This can be useful if the
command executed by heart takes care of this, for example as part of a specific
cleanup sequence. If unset, or not set to `TRUE`, the default behaviour will be
to kill as described above.

```text
% erl -heart -env HEART_NO_KILL 1 ...
```

Furthermore, `ERL_CRASH_DUMP_SECONDS` has the following behavior on `heart`:

- **`ERL_CRASH_DUMP_SECONDS=0`** - Suppresses the writing of a crash dump file
  entirely, thus rebooting the runtime system immediately. This is the same as
  not setting the environment variable.

- **`ERL_CRASH_DUMP_SECONDS=-1`** - Setting the environment variable to a
  negative value does not reboot the runtime system until the crash dump file is
  completely written.

- **`ERL_CRASH_DUMP_SECONDS=S`** - `heart` waits for `S` seconds to let the
  crash dump file be written. After `S` seconds, `heart` reboots the runtime
  system, whether the crash dump file is written or not.

In the following descriptions, all functions fail with reason `badarg` if
`heart` is not started.

# `heart_option`
*not exported* 

```erlang
-type heart_option() :: check_schedulers.
```

# `clear_callback`
*since OTP 18.3* 

```erlang
-spec clear_callback() -> ok.
```

Removes the validation callback call before heartbeats.

# `clear_cmd`

```erlang
-spec clear_cmd() -> ok.
```

Clears the temporary boot command. If the system terminates, the normal
`HEART_COMMAND` is used to reboot.

# `get_callback`
*since OTP 18.3* 

```erlang
-spec get_callback() -> {ok, {Module, Function}} | none when Module :: atom(), Function :: atom().
```

Get the validation callback. If the callback is cleared, `none` will be
returned.

# `get_cmd`

```erlang
-spec get_cmd() -> {ok, Cmd} when Cmd :: string().
```

Gets the temporary reboot command. If the command is cleared, the empty string
is returned.

# `get_options`
*since OTP 18.3* 

```erlang
-spec get_options() -> {ok, Options} | none when Options :: [atom()].
```

Returns `{ok, Options}` where `Options` is a list of current options enabled for
heart. If the callback is cleared, `none` will be returned.

# `set_callback`
*since OTP 18.3* 

```erlang
-spec set_callback(Module, Function) -> ok | {error, {bad_callback, {Module, Function}}}
                      when Module :: atom(), Function :: atom().
```

This validation callback will be executed before any heartbeat is sent to the
port program. For the validation to succeed it needs to return with the value
`ok`.

An exception within the callback will be treated as a validation failure.

The callback will be removed if the system reboots.

# `set_cmd`

```erlang
-spec set_cmd(Cmd) -> ok | {error, {bad_cmd, Cmd}} when Cmd :: string().
```

Sets a temporary reboot command. This command is used if a `HEART_COMMAND` other
than the one specified with the environment variable is to be used to reboot the
system. The new Erlang runtime system uses (if it misbehaves) environment
variable `HEART_COMMAND` to reboot.

Limitations: Command string `Cmd` is sent to the `heart` program as an ISO
Latin-1 or UTF-8 encoded binary, depending on the filename encoding mode of the
emulator (see `file:native_name_encoding/0`). The size of the encoded binary
must be less than 2047 bytes.

# `set_options`
*since OTP 18.3* 

```erlang
-spec set_options(Options) -> ok | {error, {bad_options, Options}} when Options :: [heart_option()].
```

Valid options `set_options` are:

- **`check_schedulers`** - If enabled, a signal will be sent to each scheduler
  to check its responsiveness. The system check occurs before any heartbeat sent
  to the port program. If any scheduler is not responsive enough the heart
  program will not receive its heartbeat and thus eventually terminate the node.

Returns with the value `ok` if the options are valid.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
