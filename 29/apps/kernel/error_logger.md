# `error_logger`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/error_logger.erl#L22)

Erlang error logger.

> #### Note {: .info }
>
> In Erlang/OTP 21.0, a new API for logging was added. The old `m:error_logger`
> module can still be used by legacy code, but log events are redirected to the
> new Logger API. New code should use the Logger API directly.
>
> `m:error_logger` is no longer started by default, but is automatically started
> when an event handler is added with [`error_logger:add_report_handler/1,2`](`error_logger:add_report_handler/2`). The
> `m:error_logger` module is then also added as a handler to the new logger.
>
> See `m:logger` and the [Logging](logger_chapter.md) chapter in the User's
> Guide for more information.

The Erlang _error logger_ is an event manager (see
[OTP Design Principles](`e:system:design_principles.md`) and `m:gen_event`),
registered as `m:error_logger`.

Error logger is no longer started by default, but is automatically started when
an event handler is added with
[`add_report_handler/1,2`](`add_report_handler/1`). The `m:error_logger` module is
then also added as a handler to the new logger, causing log events to be
forwarded from logger to error logger, and consequently to all installed error
logger event handlers.

User-defined event handlers can be added to handle application-specific events.

Existing event handlers provided by STDLIB and SASL are still available, but are
no longer used by OTP.

Warning events were introduced in Erlang/OTP R9C and are enabled by default as
from Erlang/OTP 18.0. To retain backwards compatibility with existing
user-defined event handlers, the warning events can be tagged as `errors` or
`info` using command-line flag `+W <e | i | w>`, thus showing up as
`ERROR REPORT` or `INFO REPORT` in the logs.

[](){: #events }

## Events

All event handlers added to the error logger must handle the following events.
`Gleader` is the group leader pid of the process that sent the event, and `Pid`
is the process that sent the event.

- **`{error, Gleader, {Pid, Format, Data}}`** -
  Generated when [`error_msg/1,2`](`error_msg/2`) or `format/2` is called.

- **`{error_report, Gleader, {Pid, std_error, Report}}`** -
  Generated when [`error_report/1`](`error_report/1`) is called.

- **`{error_report, Gleader, {Pid, Type, Report}}`** -
  Generated when [`error_report/2`](`error_report/2`) is called.

- **`{warning_msg, Gleader, {Pid, Format, Data}}`** -
  Generated when [`warning_msg/1,2`](`warning_msg/2`) is called if warnings are set to
  be tagged as warnings.

- **`{warning_report, Gleader, {Pid, std_warning, Report}}`** -
  Generated when [`warning_report/1`](`warning_report/1`) is called if warnings are
  set to be tagged as warnings.

- **`{warning_report, Gleader, {Pid, Type, Report}}`** - Generated when
  [`warning_report/2`](`warning_report/2`) is called if warnings are set to be
  tagged as warnings.

- **`{info_msg, Gleader, {Pid, Format, Data}}`** -
  Generated when [`info_msg/1,2`](`info_msg/2`) is called.

- **`{info_report, Gleader, {Pid, std_info, Report}}`** -
  Generated when [`info_report/1`](`info_report/1`) is called.

- **`{info_report, Gleader, {Pid, Type, Report}}`** -
  Generated when [`info_report/2`](`info_report/2`) is called.

Notice that some system-internal events can also be received. Therefore a
catch-all clause last in the definition of the event handler callback function
`c:gen_event:handle_event/2` is necessary. This also applies for
`c:gen_event:handle_info/2`, as the event handler must also take care of some
system-internal messages.

### See Also

`m:gen_event`, `m:logger`, `m:log_mf_h`, [`kernel`](kernel_app.md),
[`sasl`](`e:sasl:sasl_app.md`)

# `open_error`
*not exported* 

```erlang
-type open_error() :: file:posix() | badarg | system_limit.
```

# `report`
*not exported* 

```erlang
-type report() :: [{Tag :: term(), Data :: term()} | term()] | string() | term().
```

# `add_report_handler`

```erlang
-spec add_report_handler(Handler) -> any() when Handler :: module().
```

# `add_report_handler`

```erlang
-spec add_report_handler(Handler, Args) -> Result
                            when
                                Handler :: module(),
                                Args :: gen_event:handler_args(),
                                Result :: gen_event:add_handler_ret().
```

Adds a new event handler to the error logger. The event handler must be
implemented as a `m:gen_event` callback module.

`Handler` is typically the name of the callback module and `Args` is an optional
term (defaults to []) passed to the initialization callback function
`c:gen_event:init/1`. The function returns `ok` if successful.

The event handler must be able to handle the events in this module, see section
[Events](`m:error_logger#module-events`).

The first time this function is called, `m:error_logger` is added as a Logger
handler, and the `m:error_logger` process is started.

# `delete_report_handler`

```erlang
-spec delete_report_handler(Handler) -> Result
                               when Handler :: module(), Result :: gen_event:del_handler_ret().
```

Deletes an event handler from the error logger by calling
[`gen_event:delete_handler(error_logger, Handler, [])`](`gen_event:delete_handler/3`).

If no more event handlers exist after the deletion, `m:error_logger` is removed as
a Logger handler, and the `m:error_logger` process is stopped.

# `error_msg`

```erlang
-spec error_msg(Format) -> ok when Format :: string().
```

# `error_msg`

```erlang
-spec error_msg(Format, Data) -> ok when Format :: string(), Data :: list().
```

Log a standard error event. The `Format` and `Data` arguments are the same as
the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler.

This function is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.

_Example:_

```text
1> error_logger:error_msg("An error occurred in ~p", [a_module]).
=ERROR REPORT==== 22-May-2018::11:18:43.376917 ===
An error occurred in a_module
ok
```

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.

# `error_report`

```erlang
-spec error_report(Report) -> ok when Report :: report().
```

Log a standard error event. Error logger forwards the event to Logger, including
metadata that allows backwards compatibility with legacy error logger event
handlers.

The event is handled by the default Logger handler.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.

_Example:_

```text
2> error_logger:error_report([{tag1,data1},a_term,{tag2,data}]).
=ERROR REPORT==== 22-May-2018::11:24:23.699306 ===
    tag1: data1
    a_term
    tag2: data
ok
3> error_logger:error_report("Serious error in my module").
=ERROR REPORT==== 22-May-2018::11:24:45.972445 ===
Serious error in my module
ok
```

# `error_report`

```erlang
-spec error_report(Type, Report) -> ok when Type :: term(), Report :: report().
```

Log a user-defined error event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

It is recommended that `Report` follows the same structure as for
`error_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.

# `format`

```erlang
-spec format(Format, Data) -> ok when Format :: string(), Data :: list().
```

# `get_format_depth`
*since OTP 20.0* 

```erlang
-spec get_format_depth() -> unlimited | pos_integer().
```

Returns [`max(10, Depth)`](`max/2`), where `Depth` is the value of
[`error_logger_format_depth`](kernel_app.md#error_logger_format_depth) in the
Kernel application, if Depth is an integer. Otherwise, `unlimited` is returned.

> #### Note {: .info }
>
> The [`error_logger_format_depth`](kernel_app.md#error_logger_format_depth) variable is
> [deprecated](kernel_app.md#deprecated-configuration-parameters) since the
> [Logger API](`m:logger`) was introduced in Erlang/OTP 21.0. The variable, and
> this function, are kept for backwards compatibility since they still might be
> used by legacy report handlers.

# `info_msg`

```erlang
-spec info_msg(Format) -> ok when Format :: string().
```

# `info_msg`

```erlang
-spec info_msg(Format, Data) -> ok when Format :: string(), Data :: list().
```

Log a standard information event. The `Format` and `Data` arguments are the same
as the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler.

These functions are kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.

_Example:_

```text
1> error_logger:info_msg("Something happened in ~p", [a_module]).
=INFO REPORT==== 22-May-2018::12:03:32.612462 ===
Something happened in a_module
ok
```

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.

# `info_report`

```erlang
-spec info_report(Report) -> ok when Report :: report().
```

Log a standard information event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

The event is handled by the default Logger handler.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.

_Example:_

```text
2> error_logger:info_report([{tag1,data1},a_term,{tag2,data}]).
=INFO REPORT==== 22-May-2018::12:06:35.994440 ===
    tag1: data1
    a_term
    tag2: data
ok
3> error_logger:info_report("Something strange happened").
=INFO REPORT==== 22-May-2018::12:06:49.066872 ===
Something strange happened
ok
```

# `info_report`

```erlang
-spec info_report(Type, Report) -> ok when Type :: any(), Report :: report().
```

Log a user-defined information event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

It is recommended that `Report` follows the same structure as for
`info_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.

# `logfile`

```erlang
-spec logfile(Request :: {open, Filename}) -> ok | {error, OpenReason}
                 when Filename :: file:name(), OpenReason :: allready_have_logfile | open_error();
             (Request :: close) -> ok | {error, CloseReason} when CloseReason :: module_not_found;
             (Request :: filename) -> Filename | {error, FilenameReason}
                 when Filename :: file:name(), FilenameReason :: no_log_file.
```

Enables or disables printout of standard events to a file.

This is done by adding or deleting the `error_logger_file_h` event handler, and
thus indirectly adding `m:error_logger` as a Logger handler.

Notice that this function does not manipulate the Logger configuration directly,
meaning that if the default Logger handler is already logging to a file, this
function can potentially cause logging to a second file.

This function is useful as a shortcut during development and testing, but must
not be used in a production system. See section [Logging](logger_chapter.md) in
the Kernel User's Guide, and the `m:logger` manual page for information about
how to configure Logger for live systems.

`Request` is one of the following:

- **`{open, Filename}`** - Opens log file `Filename`. Returns `ok` if
  successful, or `{error, allready_have_logfile}` if logging to file is already
  enabled, or an error tuple if another error occurred (for example, if
  `Filename` cannot be opened). The file is opened with encoding UTF-8.

- **`close`** - Closes the current log file. Returns `ok`, or
  `{error, module_not_found}`.

- **`filename`** - Returns the name of the log file `Filename`, or
  `{error, no_log_file}` if logging to file is not enabled.

# `tty`

```erlang
-spec tty(Flag) -> ok when Flag :: boolean().
```

Enables (`Flag == true`) or disables (`Flag == false`) printout of standard
events to the terminal.

This is done by manipulating the Logger configuration. The function is useful as
a shortcut during development and testing, but must not be used in a production
system. See section [Logging](logger_chapter.md) in the Kernel User's Guide, and
the `m:logger` manual page for information about how to configure Logger for
live systems.

# `warning_map`

```erlang
-spec warning_map() -> Tag when Tag :: error | warning | info.
```

Returns the current mapping for warning events.

Events sent using [`warning_msg/1,2`](`warning_msg/2`) or
[`warning_report/1,2`](`warning_report/2`) are tagged as errors, warnings
(default), or info, depending on the value of command-line flag `+W`.

_Example:_

```text
os$ erl
Erlang (BEAM) emulator version 5.4.8 [hipe] [threads:0] [kernel-poll]

Eshell V5.4.8  (abort with ^G)
1> error_logger:warning_map().
warning
2> error_logger:warning_msg("Warnings tagged as: ~p~n", [warning]).

=WARNING REPORT==== 11-Aug-2005::15:31:55 ===
Warnings tagged as: warning
ok
3>
User switch command
 --> q
os$ erl +W e
Erlang (BEAM) emulator version 5.4.8 [hipe] [threads:0] [kernel-poll]

Eshell V5.4.8  (abort with ^G)
1> error_logger:warning_map().
error
2> error_logger:warning_msg("Warnings tagged as: ~p~n", [error]).

=ERROR REPORT==== 11-Aug-2005::15:31:23 ===
Warnings tagged as: error
ok
```

# `warning_msg`

```erlang
-spec warning_msg(Format) -> ok when Format :: string().
```

# `warning_msg`

```erlang
-spec warning_msg(Format, Data) -> ok when Format :: string(), Data :: list().
```

Log a standard warning event. The `Format` and `Data` arguments are the same as
the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler. The log level can be changed
to error or info, see `warning_map/0`.

These functions are kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.

# `warning_report`

```erlang
-spec warning_report(Report) -> ok when Report :: report().
```

Log a standard warning event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

The event is handled by the default Logger handler. The log level can be changed
to error or info, see `warning_map/0`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.

# `warning_report`

```erlang
-spec warning_report(Type, Report) -> ok when Type :: any(), Report :: report().
```

Log a user-defined warning event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

The log level can be changed to error or info, see `warning_map/0`.

It is recommended that `Report` follows the same structure as for
`warning_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
