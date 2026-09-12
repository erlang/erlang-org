# `nteventlog`
[🔗](https://github.com/erlang/otp/blob/master/lib/os_mon/src/nteventlog.erl#L22)

Interface to Windows Event Log

`nteventlog` provides a generic interface to the Windows event log. It is part
of the OS_Mon application, see [os_mon](os_mon_app.md).

This module is used as the Windows backend for `os_sup`. See `m:os_sup`.

To retain backwards compatibility, this module can also be used to start a
standalone `nteventlog` process which is not part of the OS_Mon supervision
tree. When starting such a process, the user has to supply an identifier as well
as a callback function to handle the messages.

The identifier, an arbitrary string, should be reused whenever the same
application (or node) wants to start the process. `nteventlog` is informed about
all events that have arrived to the eventlog since the last accepted message for
the current identifier. As long as the same identifier is used, the same
eventlog record will not be sent to `nteventlog` more than once (with the
exception of when graved system failures arise, in which case the last records
written before the failure may be sent to Erlang again after reboot).

If the event log is configured to wrap around automatically, records that have
arrived to the log and been overwritten when `nteventlog` was not running are
lost. However, it detects this state and loses no records that are not
overwritten.

The callback function works as described in `m:os_sup`.

### See Also

[os_mon](os_mon_app.md), `m:os_sup`

Windows NT documentation

# `start`

```erlang
-spec start(Identifier, MFA) -> Result
               when
                   Identifier :: string() | atom(),
                   MFA :: {Mod, Func, Args},
                   Mod :: atom(),
                   Func :: atom(),
                   Args :: [term()],
                   Result :: {ok, Pid} | {error, {already_started, Pid}},
                   Pid :: pid().
```

Equivalent to [`start_link(Identifier, MFA)`](`start_link/2`) except that no
link is created between `nteventlog` and the calling process.

# `start_link`

```erlang
-spec start_link(Identifier, MFA) -> Result
                    when
                        Identifier :: string() | atom(),
                        MFA :: {Mod, Func, Args},
                        Mod :: atom(),
                        Func :: atom(),
                        Args :: [term()],
                        Result :: {ok, Pid} | {error, {already_started, Pid}},
                        Pid :: pid().
```

This function starts the standalone `nteventlog` process and, if
[`start_link/2`](`start_link/2`) is used, links to it.

`Identifier` is an identifier as described above.

`MFA` is the supplied callback function. When `nteventlog` receives information
about a new event, this function will be called as
[`apply(Mod, Func, [Event|Args])`](`apply/3`) where `Event` is a tuple

# `stop`

```erlang
-spec stop() -> stopped.
```

Stops `nteventlog`. Usually only used during development. The server does not
have to be shut down gracefully to maintain its state.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
