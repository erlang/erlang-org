# `os_sup`
[🔗](https://github.com/erlang/otp/blob/master/lib/os_mon/src/os_sup.erl#L22)

Interface to OS System Messages

`os_sup` is a process providing a message passing service from the operating
system to the error logger in the Erlang runtime system. It is part of the
OS_Mon application, see [os_mon](os_mon_app.md). Available for Solaris and
Windows.

Messages received from the operating system results in an user defined callback
function being called. This function can do whatever filtering and formatting is
necessary and then deploy any type of logging suitable for the user's
application.

## Solaris Operation

The Solaris (SunOS 5.x) messages are retrieved from the syslog daemon,
`syslogd`.

Enabling the service includes actions which require root privileges, such as
change of ownership and file privileges of an executable binary file, and
creating a modified copy of the configuration file for `syslogd`. When `os_sup`
is terminated, the service must be disabled, meaning the original configuration
must be restored. Enabling/disabling can be done either outside or inside
`os_sup`. See [Configuration](`m:os_sup#config`) below.

> #### Warning {: .warning }
>
> This process cannot run in multiple instances on the same hardware. OS_Mon
> must be configured to start `os_sup` on one node only if two or more Erlang
> nodes execute on the same machine.

The format of received events is not defined.

## Windows Operation

The Windows messages are retrieved from the eventlog file.

The `nteventlog` module is used to implement `os_sup`. See `m:nteventlog`. Note
that the start functions of `nteventlog` does not need to be used, as in this
case the process is started automatically as part of the OS_Mon supervision
tree.

OS messages are formatted as a tuple
`{Time, Category, Facility, Severity, Message}`:

- **`Time = {MegaSecs, Secs, MicroSecs}`** - A time stamp as returned by the BIF
  `now/0`.

- **`Category = string()`** - Usually one of `"System"`, `"Application"` or
  `"Security"`. Note that the NT eventlog viewer has another notion of category,
  which in most cases is totally meaningless and therefore not imported into
  Erlang. What is called a category here is one of the main three types of
  events occurring in a normal NT system.

- **`Facility = string()`** - The source of the message, usually the name of the
  application that generated it. This could be almost any string. When matching
  messages from certain applications, the version number of the application may
  have to be accounted for. This is what the NT event viewer calls "source".

- **`Severity = string()`** - One of `"Error"`, `"Warning"`, `"Informational"`,
  `"Audit_Success"`, `"Audit_Faulure"` or, in case of a currently unknown
  Windows NT version `"Severity_Unknown"`.

- **`Message = string()`** - Formatted exactly as it would be in the NT eventlog
  viewer. Binary data is not imported into Erlang.

[](){: #config }

## Configuration

- **`os_sup_mfa = {Module, Function, Args}`** - The callback function to use.
  `Module` and `Function` are atoms and `Args` is a list of terms. When an OS
  message `Msg` is received, this function is called as
  [`apply(Module, Function, [Msg | Args])`](`apply/3`).

  Default is `{os_sup, error_report, [Tag]}` which will send the event to the
  error logger using
  [error_logger:error_report(Tag, Msg)](`error_logger:error_report/2`). `Tag` is
  the value of `os_sup_errortag`, see below.

- **`os_sup_errortag = atom()`** - This parameter defines the error report type
  used when messages are sent to error logger using the default callback
  function. Default is `std_error`, which means the events are handled by the
  standard event handler.

- **`os_sup_enable = bool()`** - Solaris only. Defines if the service should be
  enabled (and disabled) inside (`true`) or outside (`false`) `os_sup`. For
  backwards compatibility reasons, the default is `true`. The recommended value
  is `false`, as the Erlang emulator should normally not be run with `root`
  privileges, as is required for enabling the service.

- **`os_sup_own = string()`** - Solaris only. Defines the directory which
  contains the backup copy and the Erlang specific configuration files for
  `syslogd`, and a named pipe to receive the messages from `syslogd`. Default is
  `"/etc"`.

- **`os_sup_syslogconf = string()`** - Solaris only. Defines the full name of
  the configuration file for `syslogd`. Default is `"/etc/syslog.conf"`.

## See also

`m:error_logger`, [os_mon](os_mon_app.md)

`syslogd(1M)`, `syslog.conf(4)` in the Solaris documentation.

# `disable`

```erlang
-spec disable() -> ok | {error, Res} when Res :: string().
```

# `disable`

```erlang
-spec disable(Dir, Conf) -> ok | {error, Res} when Dir :: string(), Conf :: string(), Res :: string().
```

Disables the `os_sup` service. Needed on Solaris only.

If the configuration parameter `os_sup_enable` is `false`, this function is
called automatically by `os_sup`, using the same arguments as when
[`enable/2`](`enable/2`) was called.

If `os_sup_enable` is `true`, this function must be called _after_
OS_Mon/`os_sup` is stopped. `Dir` defines the directory which contains the
backup copy and the Erlang specific configuration files for `syslogd`, and a
named pipe to receive the messages from `syslogd`. Defaults to `"/etc"`. `Conf`
defines the full name of the configuration file for `syslogd`. Default is
`"/etc/syslog.conf"`.

Results in a OS call to:

```text
<PRIVDIR>/bin/mod_syslog nootp Dir Conf
```

where `<PRIVDIR>` is the `priv` directory of OS_Mon, `code:priv_dir(os_mon)`.

Returns `ok` if this yields the expected result `"0"`, and `{error, Res}` if it
yields anything else.

> #### Note {: .info }
>
> This function requires root privileges to succeed.

# `enable`

```erlang
-spec enable() -> ok | {error, Res} when Res :: string().
```

# `enable`

```erlang
-spec enable(Dir, Conf) -> ok | {error, Res} when Dir :: string(), Conf :: string(), Res :: string().
```

Enables the `os_sup` service. Needed on Solaris only.

If the configuration parameter `os_sup_enable` is `false`, this function is
called automatically by `os_sup`, using the values of `os_sup_own` and
`os_sup_syslogconf` as arguments.

If `os_sup_enable` is `true`, this function must be called _before_
OS_Mon/`os_sup` is started. `Dir` defines the directory which contains the
backup copy and the Erlang specific configuration files for `syslogd`, and a
named pipe to receive the messages from `syslogd`. Defaults to `"/etc"`. `Conf`
defines the full name of the configuration file for `syslogd`. Default is
`"/etc/syslog.conf"`.

Results in a OS call to:

```text
<PRIVDIR>/bin/mod_syslog otp Dir Conf
```

where `<PRIVDIR>` is the `priv` directory of OS_Mon, `code:priv_dir(os_mon)`.

Returns `ok` if this yields the expected result `"0"`, and `{error, Res}` if it
yields anything else.

> #### Note {: .info }
>
> This function requires root privileges to succeed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
