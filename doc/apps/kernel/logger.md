# `logger`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/logger.erl#L22)

API module for Logger, the standard logging facility in Erlang/OTP.

This module implements the main API for logging in Erlang/OTP. To create a log
event, use the [API functions](#logging-api-functions) or the log
[macros](#module-macros), for example:

```erlang
?LOG_ERROR("error happened because: ~p", [Reason]).   % With macro
logger:error("error happened because: ~p", [Reason]). % Without macro
```

To configure the Logger backend, use
[Kernel configuration parameters](kernel_app.md#logger) or
configuration functions in the `m:logger` API.

By default, the Kernel application installs one log handler at system start.
This handler is named `default`. It receives and processes standard log events
produced by the Erlang runtime system, standard behaviours and different
Erlang/OTP applications. The log events are by default printed to the terminal.

If you want your systems logs to be printed to a file instead, you must
configure the default handler to do so. The simplest way is to include the
following in your [`sys.config`](config.md):

```erlang
[{kernel,
  [{logger,
    [{handler, default, logger_std_h,
      #{config => #{file => "path/to/file.log"}}}]}]}].
```

For more information about:

- the Logger facility in general, see the [User's Guide](logger_chapter.md).
- how to configure Logger, see the
  [Configuration](logger_chapter.md#configuration) section in the User's Guide.
- the built-in handlers, see `m:logger_std_h` and `m:logger_disk_log_h`.
- the built-in formatter, see `m:logger_formatter`.
- built-in filters, see `m:logger_filters`.

## Macros

The following macros are defined in `logger.hrl`, which is included in a module
with the directive

```erlang
    -include_lib("kernel/include/logger.hrl").
```

- `?LOG_EMERGENCY(StringOrReport[,Metadata])`
- `?LOG_EMERGENCY(FunOrFormat,Args[,Metadata])`
- `?LOG_ALERT(StringOrReport[,Metadata])`
- `?LOG_ALERT(FunOrFormat,Args[,Metadata])`
- `?LOG_CRITICAL(StringOrReport[,Metadata])`
- `?LOG_CRITICAL(FunOrFormat,Args[,Metadata])`
- `?LOG_ERROR(StringOrReport[,Metadata])`
- `?LOG_ERROR(FunOrFormat,Args[,Metadata])`
- `?LOG_WARNING(StringOrReport[,Metadata])`
- `?LOG_WARNING(FunOrFormat,Args[,Metadata])`
- `?LOG_NOTICE(StringOrReport[,Metadata])`
- `?LOG_NOTICE(FunOrFormat,Args[,Metadata])`
- `?LOG_INFO(StringOrReport[,Metadata])`
- `?LOG_INFO(FunOrFormat,Args[,Metadata])`
- `?LOG_DEBUG(StringOrReport[,Metadata])`
- `?LOG_DEBUG(FunOrFormat,Args[,Metadata])`
- `?LOG(Level,StringOrReport[,Metadata])`
- `?LOG(Level,FunOrFormat,Args[,Metadata])`

All macros expand to a call to Logger, where `Level` is taken from the macro
name, or from the first argument in the case of the `?LOG` macro. Location data
is added to the metadata as described under the `t:metadata/0` type definition.

The call is wrapped in a case statement and will be evaluated only if `Level` is
equal to or below the configured log level.

### See Also

[`config`](config.md), `m:erlang`, `m:io`, `m:logger_disk_log_h`,
`m:logger_filters`, `m:logger_handler`, `m:logger_formatter`, `m:logger_std_h`,
`m:unicode`

# `config_handler`
*since OTP 21.0* 

```erlang
-type config_handler() :: {handler, logger_handler:id(), module(), logger_handler:config()}.
```

Configuration used when adding or updating a handler.

# `filter`
*since OTP 21.0* 

```erlang
-type filter() :: {fun((log_event(), filter_arg()) -> filter_return()), filter_arg()}.
```

A filter which can be installed as a handler filter, or as a primary filter in
Logger.

# `filter_arg`
*since OTP 21.0* 

```erlang
-type filter_arg() :: term().
```

The second argument to the filter fun.

# `filter_id`
*since OTP 21.0* 

```erlang
-type filter_id() :: atom().
```

A unique identifier for a filter.

# `filter_return`
*since OTP 21.0* 

```erlang
-type filter_return() :: stop | ignore | log_event().
```

The return value from the filter fun.

# `formatter_config`
*since OTP 21.0* 

```erlang
-type formatter_config() :: #{atom() => term()}.
```

Configuration data for the formatter. See `m:logger_formatter` for an example of
a formatter implementation.

# `handler_config`
*since OTP 21.0* 

```erlang
-type handler_config() :: logger_handler:config().
```

Handler configuration data for Logger.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:config/0` instead.

# `handler_id`
*since OTP 21.0* 

```erlang
-type handler_id() :: logger_handler:id().
```

A unique identifier for a handler instance.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:id/0` instead.

# `level`
*since OTP 21.0* 

```erlang
-type level() :: emergency | alert | critical | error | warning | notice | info | debug.
```

The severity level for the message to be logged.

# `log_event`
*since OTP 21.0* 

```erlang
-type log_event() ::
          #{level := level(),
            msg := {io:format(), [term()]} | {report, report()} | {string, unicode:chardata()},
            meta := metadata()}.
```

A log event passed to filters and handlers

# `metadata`
*since OTP 21.0* 

```erlang
-type metadata() ::
          #{pid => pid(),
            gl => pid(),
            time => timestamp(),
            mfa => {module(), atom(), non_neg_integer()},
            file => file:filename(),
            line => non_neg_integer(),
            domain => [atom()],
            report_cb => report_cb(),
            atom() => term()}.
```

Metadata for the log event.

Logger adds the following metadata to each log event:

- `pid => self()`
- `gl => group_leader()`
- `time => logger:timestamp()`

When a log macro is used, Logger also inserts location information:

- `mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}`
- `file => ?FILE`
- `line => ?LINE`

You can add custom metadata, either by:

- specifying a map as the last parameter to any of the log macros or the logger
  API functions.
- setting process metadata with `set_process_metadata/1` or
  `update_process_metadata/1`.
- setting primary metadata with `set_primary_config/1` or through the kernel
  configuration parameter [logger_metadata](kernel_app.md#logger_metadata)

> #### Note {: .info }
>
> When adding custom metadata, make sure not to use any of the keys mentioned
> above as that may cause a lot of confusion about the log events.

Logger merges all the metadata maps before forwarding the log event to the
handlers. If the same keys occur, values from the log call overwrite process
metadata, which overwrites the primary metadata, which in turn overwrite values
set by Logger.

The following custom metadata keys have special meaning:

- **`domain`** - The value associated with this key is used by filters for
  grouping log events originating from, for example, specific functional areas.
  See `logger_filters:domain/2` for a description of how this field can be used.

- **`report_cb`** - If the log message is specified as a `t:report/0`, the
  `report_cb` key can be associated with a fun (report callback) that converts
  the report to a format string and arguments, or directly to a string. See the
  type definition of `t:report_cb/0`, and section
  [Log Message](logger_chapter.md#log-message) in the User's Guide for more
  information about report callbacks.

# `msg_fun`
*since OTP 21.0* 

```erlang
-type msg_fun() :: fun((term()) -> msg_fun_return() | {msg_fun_return(), metadata()}).
```

# `msg_fun_return`
*not exported* *since OTP 21.0* 

```erlang
-type msg_fun_return() :: {io:format(), [term()]} | report() | unicode:chardata() | ignore.
```

# `olp_config`
*since OTP 21.0* 

```erlang
-type olp_config() :: logger_handler:olp_config().
```

Overload protection configuration.

> #### Note {: .info }
>
> DEPRECATED: Use `t:logger_handler:olp_config/0` instead.

# `primary_config`
*since OTP 21.0* 

```erlang
-type primary_config() ::
          #{level => level() | all | none,
            metadata => metadata(),
            filter_default => log | stop,
            filters => [{filter_id(), filter()}]}.
```

Primary configuration data for Logger. The following default values apply:

- `level => info`
- `filter_default => log`
- `filters => []`

# `report`
*since OTP 21.0* 

```erlang
-type report() :: map() | [{atom(), term()}, ...].
```

A log report.

# `report_cb`
*since OTP 21.0* 

```erlang
-type report_cb() ::
          fun((report()) -> {io:format(), [term()]}) |
          fun((report(), report_cb_config()) -> unicode:chardata()).
```

A fun which converts a [`report()`](`t:report/0`) to a format string and
arguments, or directly to a string.

See section [Log Message](logger_chapter.md#log-message) in the User's Guide
for more information.

# `report_cb_config`
*since OTP 21.0* 

```erlang
-type report_cb_config() ::
          #{depth := pos_integer() | unlimited,
            chars_limit := pos_integer() | unlimited,
            single_line := boolean()}.
```

# `timestamp`
*since OTP 21.0* 

```erlang
-type timestamp() :: integer().
```

A timestamp produced with [`logger:timestamp()`](`timestamp/0`).

# `alert`
*since OTP 21.0* 

```erlang
-spec alert(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
```

# `alert`
*since OTP 21.0* 

```erlang
-spec alert(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a alert log event.

Equivalent to [`log(alert, StringOrReport, Metadata)`](`log/3`) if called
as [`alert(StringOrReport, Metadata)`](`alert/2`).

Equivalent to [`alert(FormatOrFun, Args, #{})`](`alert/3`) if called as
[`alert(FormatOrFun, Args)`](`alert/2`).

# `alert`
*since OTP 21.0* 

```erlang
-spec alert(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `critical`
*since OTP 21.0* 

```erlang
-spec critical(String :: unicode:chardata()) -> ok;
              (Report :: report()) -> ok.
```

# `critical`
*since OTP 21.0* 

```erlang
-spec critical(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
              (Report :: report(), Metadata :: metadata()) -> ok;
              (Format :: io:format(), Args :: [term()]) -> ok;
              (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a critical log event.

Equivalent to [`log(critical, StringOrReport, Metadata)`](`log/3`) if called
as [`critical(StringOrReport, Metadata)`](`critical/2`).

Equivalent to [`critical(FormatOrFun, Args, #{})`](`critical/3`) if called as
[`critical(FormatOrFun, Args)`](`critical/2`).

# `critical`
*since OTP 21.0* 

```erlang
-spec critical(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
              (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `debug`
*since OTP 21.0* 

```erlang
-spec debug(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
```

# `debug`
*since OTP 21.0* 

```erlang
-spec debug(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a debug log event.

Equivalent to [`log(debug, StringOrReport, Metadata)`](`log/3`) if called
as [`debug(StringOrReport, Metadata)`](`debug/2`).

Equivalent to [`debug(FormatOrFun, Args, #{})`](`debug/3`) if called as
[`debug(FormatOrFun, Args)`](`debug/2`).

# `debug`
*since OTP 21.0* 

```erlang
-spec debug(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `emergency`
*since OTP 21.0* 

```erlang
-spec emergency(String :: unicode:chardata()) -> ok;
               (Report :: report()) -> ok.
```

# `emergency`
*since OTP 21.0* 

```erlang
-spec emergency(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
               (Report :: report(), Metadata :: metadata()) -> ok;
               (Format :: io:format(), Args :: [term()]) -> ok;
               (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a emergency log event.

Equivalent to [`log(emergency, StringOrReport, Metadata)`](`log/3`) if called
as [`emergency(StringOrReport, Metadata)`](`emergency/2`).

Equivalent to [`emergency(FormatOrFun, Args, #{})`](`emergency/3`) if called as
[`emergency(FormatOrFun, Args)`](`emergency/2`).

# `emergency`
*since OTP 21.0* 

```erlang
-spec emergency(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
               (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `error`
*since OTP 21.0* 

```erlang
-spec error(String :: unicode:chardata()) -> ok;
           (Report :: report()) -> ok.
```

# `error`
*since OTP 21.0* 

```erlang
-spec error(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
           (Report :: report(), Metadata :: metadata()) -> ok;
           (Format :: io:format(), Args :: [term()]) -> ok;
           (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a error log event.

Equivalent to [`log(error, StringOrReport, Metadata)`](`log/3`) if called
as [`error(StringOrReport, Metadata)`](`error/2`).

Equivalent to [`error(FormatOrFun, Args, #{})`](`error/3`) if called as
[`error(FormatOrFun, Args)`](`error/2`).

# `error`
*since OTP 21.0* 

```erlang
-spec error(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
           (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `info`
*since OTP 21.0* 

```erlang
-spec info(String :: unicode:chardata()) -> ok;
          (Report :: report()) -> ok.
```

# `info`
*since OTP 21.0* 

```erlang
-spec info(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
          (Report :: report(), Metadata :: metadata()) -> ok;
          (Format :: io:format(), Args :: [term()]) -> ok;
          (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a info log event.

Equivalent to [`log(info, StringOrReport, Metadata)`](`log/3`) if called
as [`info(StringOrReport, Metadata)`](`info/2`).

Equivalent to [`info(FormatOrFun, Args, #{})`](`info/3`) if called as
[`info(FormatOrFun, Args)`](`info/2`).

# `info`
*since OTP 21.0* 

```erlang
-spec info(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
          (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `log`
*since OTP 21.0* 

```erlang
-spec log(Level :: level(), String :: unicode:chardata()) -> ok;
         (Level :: level(), Report :: report()) -> ok.
```

# `log`
*since OTP 21.0* 

```erlang
-spec log(Level :: level(), String :: unicode:chardata(), Metadata :: metadata()) -> ok;
         (Level :: level(), Report :: report(), Metadata :: metadata()) -> ok;
         (Level :: level(), Format :: io:format(), Args :: [term()]) -> ok;
         (Level :: level(), Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a log event at the given [log level](logger_chapter.md#log-level), with
the given [message](logger_chapter.md#log-message) to be logged and
[_metadata_](logger_chapter.md#metadata).

*Example*:

```erlang
%% A plain string
1> logger:log(info, "Hello World").
%% A plain string with metadata
2> logger:log(debug, "Hello World", #{ meta => data }).
%% A format string with arguments
3> logger:log(warning, "The roof is on ~ts",[Cause]).
%% A report
4> logger:log(warning, #{ what => roof, cause => Cause }).
```

Equivalent to [`log(Level, FormatOrFun, Args, #{})`](`log/4`) if called as
`log(Level, FormatOrFun, Args)`.

# `log`
*since OTP 21.0* 

```erlang
-spec log(Level :: level(), Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
         (Level :: level(), Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

Create a log event at the given [log level](logger_chapter.md#log-level), with
the given [message](logger_chapter.md#log-message) to be logged and
[_metadata_](logger_chapter.md#metadata).

The message and metadata can either be given directly in the arguments, or
returned from a fun. Passing a fun instead of the message/metadata directly is
useful in scenarios when the message/metadata is very expensive to compute. This
is because the fun is only evaluated when the message/metadata is actually
needed, which may be not at all if the log event is not to be logged. Examples:

```erlang
%% A plain string with expensive metadata
1> logger:info(fun([]) -> {"Hello World", #{ meta => expensive() }} end,[]).
%% An expensive report
2> logger:debug(fun(What) -> #{ what => What, cause => expensive() } end,roof).
%% A plain string with expensive metadata and normal metadata
3> logger:debug(fun([]) -> {"Hello World", #{ meta => expensive() }} end,[],
               #{ meta => data }).
```

When metadata is given both as an argument and returned from the fun they are
merged. If equal keys exists the values are taken from the metadata returned by
the fun.

# `notice`
*since OTP 21.0* 

```erlang
-spec notice(String :: unicode:chardata()) -> ok;
            (Report :: report()) -> ok.
```

# `notice`
*since OTP 21.0* 

```erlang
-spec notice(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
            (Report :: report(), Metadata :: metadata()) -> ok;
            (Format :: io:format(), Args :: [term()]) -> ok;
            (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a notice log event.

Equivalent to [`log(notice, StringOrReport, Metadata)`](`log/3`) if called
as [`notice(StringOrReport, Metadata)`](`notice/2`).

Equivalent to [`notice(FormatOrFun, Args, #{})`](`notice/3`) if called as
[`notice(FormatOrFun, Args)`](`notice/2`).

# `notice`
*since OTP 21.0* 

```erlang
-spec notice(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
            (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `warning`
*since OTP 21.0* 

```erlang
-spec warning(String :: unicode:chardata()) -> ok;
             (Report :: report()) -> ok.
```

# `warning`
*since OTP 21.0* 

```erlang
-spec warning(String :: unicode:chardata(), Metadata :: metadata()) -> ok;
             (Report :: report(), Metadata :: metadata()) -> ok;
             (Format :: io:format(), Args :: [term()]) -> ok;
             (Fun :: msg_fun(), FunArgs :: term()) -> ok.
```

Create a warning log event.

Equivalent to [`log(warning, StringOrReport, Metadata)`](`log/3`) if called
as [`warning(StringOrReport, Metadata)`](`warning/2`).

Equivalent to [`warning(FormatOrFun, Args, #{})`](`warning/3`) if called as
[`warning(FormatOrFun, Args)`](`warning/2`).

# `warning`
*since OTP 21.0* 

```erlang
-spec warning(Format :: io:format(), Args :: [term()], Metadata :: metadata()) -> ok;
             (Fun :: msg_fun(), FunArgs :: term(), Metadata :: metadata()) -> ok.
```

# `add_handler`
*since OTP 21.0* 

```erlang
-spec add_handler(HandlerId, Module, Config) -> ok | {error, term()}
                     when
                         HandlerId :: logger_handler:id(),
                         Module :: module(),
                         Config :: logger_handler:config().
```

Add a handler with the given configuration.

`HandlerId` is a unique identifier which must be used in all subsequent calls
referring to this handler.

# `add_handler_filter`
*since OTP 21.0* 

```erlang
-spec add_handler_filter(HandlerId, FilterId, Filter) -> ok | {error, term()}
                            when
                                HandlerId :: logger_handler:id(),
                                FilterId :: filter_id(),
                                Filter :: filter().
```

Add a filter to the specified handler.

The filter fun is called with the log event as the first parameter, and the
specified `filter_args()` as the second parameter.

The return value of the fun specifies if a log event is to be discarded or
forwarded to the handler callback:

- **`t:log_event/0`** - The filter _passed_. The next handler filter, if any, is
  applied. If no more filters exist for this handler, the log event is forwarded
  to the handler callback.

- **`stop`** - The filter _did not pass_, and the log event is immediately
  discarded.

- **`ignore`** - The filter has no knowledge of the log event. The next handler
  filter, if any, is applied. If no more filters exist for this handler, the
  value of the `filter_default` configuration parameter for the handler
  specifies if the log event shall be discarded or forwarded to the handler
  callback.

See section [Filters](logger_chapter.md#filters) in the User's Guide for more
information about filters.

Some built-in filters exist. These are defined in `m:logger_filters`.

# `add_handlers`
*since OTP 21.0* 

```erlang
-spec add_handlers(Application) -> ok | {error, term()} when Application :: atom();
                  (HandlerConfig) -> ok | {error, term()} when HandlerConfig :: [config_handler()].
```

Reads the application configuration parameter `logger` and calls
[`add_handlers/1`](`add_handlers/1`) with its contents.

This function should be used by custom Logger handlers to make configuration
consistent no matter which handler the system uses. Normal usage is to add a
call to `logger:add_handlers/1` just after the processes that the handler needs
are started, and pass the application's `logger` configuration as the argument.
For example:

```erlang
-behaviour(application).
start(_, []) ->
    case supervisor:start_link({local, my_sup}, my_sup, []) of
        {ok, Pid} ->
            ok = logger:add_handlers(my_app),
            {ok, Pid, []};
        Error -> Error
     end.
```

This reads the `logger` configuration parameter from the `my_app` application
and starts the configured handlers. The contents of the configuration use the
same rules as the
[logger handler configuration](logger_chapter.md#handler-configuration).

If the handler is meant to replace the default handler, the Kernel's default
handler have to be disabled before the new handler is added. A `sys.config` file
that disables the Kernel handler and adds a custom handler could look like this:

```erlang
[{kernel,
  [{logger,
    %% Disable the default Kernel handler
    [{handler, default, undefined}]}]},
 {my_app,
  [{logger,
    %% Enable this handler as the default
    [{handler, default, my_handler, #{}}]}]}].
```

# `add_primary_filter`
*since OTP 21.0* 

```erlang
-spec add_primary_filter(FilterId, Filter) -> ok | {error, term()}
                            when FilterId :: filter_id(), Filter :: filter().
```

Add a primary filter to Logger.

The filter fun is called with the log event as the first parameter, and the
specified `filter_args()` as the second parameter.

The return value of the fun specifies if a log event is to be discarded or
forwarded to the handlers:

- **`t:log_event/0`** - The filter _passed_. The next primary filter, if any, is
  applied. If no more primary filters exist, the log event is forwarded to the
  handler part of Logger, where handler filters are applied.

- **`stop`** - The filter _did not pass_, and the log event is immediately
  discarded.

- **`ignore`** - The filter has no knowledge of the log event. The next primary
  filter, if any, is applied. If no more primary filters exist, the value of the
  primary `filter_default` configuration parameter specifies if the log event
  shall be discarded or forwarded to the handler part.

See section [Filters](logger_chapter.md#filters) in the User's Guide for more
information about filters.

Some built-in filters exist. These are defined in `m:logger_filters`.

# `compare_levels`
*since OTP 21.0* 

```erlang
-spec compare_levels(Level1, Level2) -> eq | gt | lt
                        when Level1 :: level() | all | none, Level2 :: level() | all | none.
```

Compare the severity of two log levels. Returns `gt` if `Level1` is more severe
than `Level2`, `lt` if `Level1` is less severe, and `eq` if the levels are
equal.

# `format_report`
*since OTP 21.0* 

```erlang
-spec format_report(Report) -> FormatArgs when Report :: report(), FormatArgs :: {io:format(), [term()]}.
```

Convert a log message on report form to `{Format, Args}`. This is the default
report callback used by `m:logger_formatter` when no custom report callback is
found. See section [Log Message](logger_chapter.md#log-message) in the Kernel
User's Guide for information about report callbacks and valid forms of log
messages.

The function produces lines of `Key: Value` from key-value lists. Strings are
printed with `~ts` and other terms with `~tp`.

If `Report` is a map, it is converted to a key-value list before formatting as
such.

# `get_config`
*since OTP 21.0* 

```erlang
-spec get_config() ->
                    #{primary => primary_config(),
                      handlers => [logger_handler:config()],
                      proxy => olp_config(),
                      module_levels => [{module(), level() | all | none}]}.
```

Look up all current Logger configuration, including primary, handler, and proxy
configuration, and module level settings.

# `get_handler_config`
*since OTP 21.0* 

```erlang
-spec get_handler_config() -> [Config] when Config :: logger_handler:config().
```

Look up the current configuration for all handlers.

# `get_handler_config`
*since OTP 21.0* 

```erlang
-spec get_handler_config(HandlerId) -> {ok, Config} | {error, term()}
                            when HandlerId :: logger_handler:id(), Config :: logger_handler:config().
```

Look up the current configuration for the given handler.

# `get_handler_ids`
*since OTP 21.0* 

```erlang
-spec get_handler_ids() -> [HandlerId] when HandlerId :: logger_handler:id().
```

Look up the identities for all installed handlers.

# `get_module_level`
*since OTP 21.0* 

```erlang
-spec get_module_level() -> [{Module, Level}] when Module :: module(), Level :: level() | all | none.
```

Look up all current module levels. Returns a list containing one
`{Module,Level}` element for each module for which the module level was
previously set with `set_module_level/2`.

# `get_module_level`
*since OTP 21.0* 

```erlang
-spec get_module_level(Modules) -> [{Module, Level}]
                          when
                              Modules :: [Module] | Module,
                              Module :: module(),
                              Level :: level() | all | none.
```

Look up the current level for the given modules. Returns a list containing one
`{Module,Level}` element for each of the given modules for which the module
level was previously set with `set_module_level/2`.

# `get_primary_config`
*since OTP 21.0* 

```erlang
-spec get_primary_config() -> Config when Config :: primary_config().
```

Look up the current primary configuration for Logger.

# `get_process_metadata`
*since OTP 21.0* 

```erlang
-spec get_process_metadata() -> Meta | undefined when Meta :: metadata().
```

Retrieve data set with `set_process_metadata/1` or `update_process_metadata/1`.

# `get_proxy_config`
*since OTP 21.3* 

```erlang
-spec get_proxy_config() -> Config when Config :: olp_config().
```

Look up the current configuration for the Logger proxy.

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.

# `i`
*since OTP 21.3* 

```erlang
-spec i() -> ok.
```

Pretty print all Logger configuration.

# `i`
*since OTP 21.3* 

```erlang
-spec i(What) -> ok when What :: primary | handlers | proxy | modules | logger_handler:id().
```

Pretty print the Logger configuration.

# `reconfigure`
*since OTP 24.2* 

```erlang
-spec reconfigure() -> ok | {error, term()}.
```

Reconfigure Logger using updated `kernel` configuration that was set after
`kernel` application was loaded.

Beware, that this is meant to be run only by the build tools, not manually
during application lifetime, as this may cause missing log entries.

Before reconfiguration, `simple` logger handler is added to capture log events
before the logging infrastructure is started and prints them to standard
output. After the `default` handler is added again, all log events captured by
the `simple` handler are replayed to the `default` handler, and the `simple`
handler is removed. Notice that if you don't add the `default` handler,
`simple` handler will persist.

# `remove_handler`
*since OTP 21.0* 

```erlang
-spec remove_handler(HandlerId) -> ok | {error, term()} when HandlerId :: logger_handler:id().
```

Remove the handler identified by `HandlerId`.

# `remove_handler_filter`
*since OTP 21.0* 

```erlang
-spec remove_handler_filter(HandlerId, FilterId) -> ok | {error, term()}
                               when HandlerId :: logger_handler:id(), FilterId :: filter_id().
```

Remove the filter identified by `FilterId` from the handler identified by
`HandlerId`.

# `remove_primary_filter`
*since OTP 21.0* 

```erlang
-spec remove_primary_filter(FilterId) -> ok | {error, term()} when FilterId :: filter_id().
```

Remove the primary filter identified by `FilterId` from Logger.

# `set_application_level`
*since OTP 21.1* 

```erlang
-spec set_application_level(Application, Level) -> ok | {error, not_loaded}
                               when Application :: atom(), Level :: level() | all | none.
```

Set the log level for all the modules of the specified application.

This function is a convenience function that calls
[logger:set_module_level/2](`set_module_level/2`) for each module associated
with an application.

# `set_handler_config`
*since OTP 21.0* 

```erlang
-spec set_handler_config(HandlerId, Config) -> ok | {error, term()}
                            when HandlerId :: logger_handler:id(), Config :: logger_handler:config().
```

Set configuration data for the specified handler. This overwrites the current
handler configuration.

To modify the existing configuration, use `update_handler_config/2`, or, if a
more complex merge is needed, read the current configuration with
[`get_handler_config/1` ](`get_handler_config/1`), then do the merge before
writing the new configuration back with this function.

If a key is removed compared to the current configuration, and the key is known
by Logger, the default value is used. If it is a custom key, then it is up to
the handler implementation if the value is removed or a default value is
inserted.

# `set_handler_config`
*since OTP 21.0* 

```erlang
-spec set_handler_config(HandlerId, level, Level) -> Return
                            when
                                HandlerId :: logger_handler:id(),
                                Level :: level() | all | none,
                                Return :: ok | {error, term()};
                        (HandlerId, filter_default, FilterDefault) -> Return
                            when
                                HandlerId :: logger_handler:id(),
                                FilterDefault :: log | stop,
                                Return :: ok | {error, term()};
                        (HandlerId, filters, Filters) -> Return
                            when
                                HandlerId :: logger_handler:id(),
                                Filters :: [{filter_id(), filter()}],
                                Return :: ok | {error, term()};
                        (HandlerId, formatter, Formatter) -> Return
                            when
                                HandlerId :: logger_handler:id(),
                                Formatter :: {module(), formatter_config()},
                                Return :: ok | {error, term()};
                        (HandlerId, config, Config) -> Return
                            when
                                HandlerId :: logger_handler:id(),
                                Config :: term(),
                                Return :: ok | {error, term()}.
```

Add or update configuration data for the specified handler. If the given `Key`
already exists, its associated value will be changed to the given value. If it
does not exist, it will be added.

If the value is incomplete, which for example can be the case for the `config`
key, it is up to the handler implementation how the unspecified parts are set.
For all handlers in the Kernel application, unspecified data for the `config`
key is set to default values. To update only specified data, and keep the
existing configuration for the rest, use `update_handler_config/3`.

See the definition of the `t:logger_handler:config/0` type for more information
about the different parameters.

# `set_module_level`
*since OTP 21.0* 

```erlang
-spec set_module_level(Modules, Level) -> ok | {error, term()}
                          when Modules :: [module()] | module(), Level :: level() | all | none.
```

Set the log level for the specified modules.

The log level for a module overrides the primary log level of Logger for log
events originating from the module in question. Notice, however, that it does
not override the level configuration for any handler.

For example: Assume that the primary log level for Logger is `info`, and there
is one handler, `h1`, with level `info` and one handler, `h2`, with level
`debug`.

With this configuration, no debug messages will be logged, since they are all
stopped by the primary log level.

If the level for `mymodule` is now set to `debug`, then debug events from this
module will be logged by the handler `h2`, but not by handler `h1`.

Debug events from other modules are still not logged.

To change the primary log level for Logger, use
[`set_primary_config(level, Level)`](`set_primary_config/2`).

To change the log level for a handler, use
[`set_handler_config(HandlerId, level, Level)` ](`set_handler_config/3`).

> #### Note {: .info }
>
> The originating module for a log event is only detected if the key `mfa`
> exists in the metadata, and is associated with `{Module, Function, Arity}`.
> When log macros are used, this association is automatically added to all log
> events. If an API function is called directly, without using a macro, the
> logging client must explicitly add this information if module levels shall
> have any effect.

# `set_primary_config`
*since OTP 21.0* 

```erlang
-spec set_primary_config(Config) -> ok | {error, term()} when Config :: primary_config().
```

Set primary configuration data for Logger. This overwrites the current
configuration.

To modify the existing configuration, use `update_primary_config/1`, or, if a
more complex merge is needed, read the current configuration with
[`get_primary_config/0` ](`get_primary_config/0`), then do the merge before
writing the new configuration back with this function.

If a key is removed compared to the current configuration, the default value is
used.

# `set_primary_config`
*since OTP 21.0* 

```erlang
-spec set_primary_config(level, Level) -> ok | {error, term()} when Level :: level() | all | none;
                        (filter_default, FilterDefault) -> ok | {error, term()}
                            when FilterDefault :: log | stop;
                        (filters, Filters) -> ok | {error, term()}
                            when Filters :: [{filter_id(), filter()}];
                        (metadata, Meta) -> ok | {error, term()} when Meta :: metadata().
```

Add or update primary configuration data for Logger. If the given `Key` already
exists, its associated value will be changed to the given value. If it does not
exist, it will be added.

The `metadata` key was added in OTP 24.0.

# `set_process_metadata`
*since OTP 21.0* 

```erlang
-spec set_process_metadata(Meta) -> ok when Meta :: metadata().
```

Set metadata which Logger shall automatically insert in all log events produced
on the current process.

Location data produced by the log macros, and/or metadata given as argument to
the log call (API function or macro), are merged with the process metadata. If
the same keys occur, values from the metadata argument to the log call overwrite
values from the process metadata, which in turn overwrite values from the
location data.

Subsequent calls to this function overwrites previous data set. To update
existing data instead of overwriting it, see `update_process_metadata/1`.

# `set_proxy_config`
*since OTP 21.3* 

```erlang
-spec set_proxy_config(Config) -> ok | {error, term()} when Config :: olp_config().
```

Set configuration data for the Logger proxy. This overwrites the current proxy
configuration. Keys that are not specified in the `Config` map gets default
values.

To modify the existing configuration, use `update_proxy_config/1`, or, if a more
complex merge is needed, read the current configuration with
[`get_proxy_config/0` ](`get_proxy_config/0`), then do the merge before writing
the new configuration back with this function.

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.

# `timestamp`
*since OTP 21.3* 

```erlang
-spec timestamp() -> timestamp().
```

Return a timestamp that can be inserted as the `time` field in the meta data for
a log event. It is produced with
[`os:system_time(microsecond)`](`os:system_time/1`).

Notice that Logger automatically inserts a timestamp in the meta data unless it
already exists. This function is exported for the rare case when the timestamp
must be taken at a different point in time than when the log event is issued.

# `unset_application_level`
*since OTP 21.1* 

```erlang
-spec unset_application_level(Application) -> ok | {error, {not_loaded, Application}}
                                 when Application :: atom().
```

Unset the log level for all the modules of the specified application.

This function is a utility function that calls
[logger:unset_module_level/2](`unset_module_level/1`) for each module associated
with an application.

# `unset_module_level`
*since OTP 21.0* 

```erlang
-spec unset_module_level() -> ok.
```

Remove module specific log settings. After this, the primary log level is used
for all modules.

# `unset_module_level`
*since OTP 21.0* 

```erlang
-spec unset_module_level(Modules) -> ok when Modules :: [module()] | module().
```

Remove module specific log settings. After this, the primary log level is used
for the specified modules.

# `unset_process_metadata`
*since OTP 21.0* 

```erlang
-spec unset_process_metadata() -> ok.
```

Delete data set with `set_process_metadata/1` or `update_process_metadata/1`.

# `update_formatter_config`
*since OTP 21.0* 

```erlang
-spec update_formatter_config(HandlerId, FormatterConfig) -> ok | {error, term()}
                                 when
                                     HandlerId :: logger_handler:id(),
                                     FormatterConfig :: formatter_config().
```

Update the formatter configuration for the specified handler.

The new configuration is merged with the existing formatter configuration.

To overwrite the existing configuration without any merge, use

```erlang
set_handler_config(HandlerId, formatter,
	      {FormatterModule, FormatterConfig}).
```

# `update_formatter_config`
*since OTP 21.0* 

```erlang
-spec update_formatter_config(HandlerId, Key, Value) -> ok | {error, term()}
                                 when HandlerId :: logger_handler:id(), Key :: atom(), Value :: term().
```

# `update_handler_config`
*since OTP 21.0* 

```erlang
-spec update_handler_config(HandlerId, Config) -> ok | {error, term()}
                               when HandlerId :: logger_handler:id(), Config :: logger_handler:config().
```

Update configuration data for the specified handler. This function behaves as if
it was implemented as follows:

```erlang
{ok, {_, Old}} = logger:get_handler_config(HandlerId),
logger:set_handler_config(HandlerId, maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_handler_config/2` ](`set_handler_config/2`).

# `update_handler_config`
*since OTP 21.2* 

```erlang
-spec update_handler_config(HandlerId, level, Level) -> Return
                               when
                                   HandlerId :: logger_handler:id(),
                                   Level :: level() | all | none,
                                   Return :: ok | {error, term()};
                           (HandlerId, filter_default, FilterDefault) -> Return
                               when
                                   HandlerId :: logger_handler:id(),
                                   FilterDefault :: log | stop,
                                   Return :: ok | {error, term()};
                           (HandlerId, filters, Filters) -> Return
                               when
                                   HandlerId :: logger_handler:id(),
                                   Filters :: [{filter_id(), filter()}],
                                   Return :: ok | {error, term()};
                           (HandlerId, formatter, Formatter) -> Return
                               when
                                   HandlerId :: logger_handler:id(),
                                   Formatter :: {module(), formatter_config()},
                                   Return :: ok | {error, term()};
                           (HandlerId, config, Config) -> Return
                               when
                                   HandlerId :: logger_handler:id(),
                                   Config :: term(),
                                   Return :: ok | {error, term()}.
```

Add or update configuration data for the specified handler. If the given `Key`
already exists, its associated value will be changed to the given value. If it
does not exist, it will be added.

If the value is incomplete, which for example can be the case for the `config`
key, it is up to the handler implementation how the unspecified parts are set.
For all handlers in the Kernel application, unspecified data for the `config`
key is not changed. To reset unspecified data to default values, use
`set_handler_config/3`.

See the definition of the `t:logger_handler:config/0` type for more information
about the different parameters.

# `update_primary_config`
*since OTP 21.0* 

```erlang
-spec update_primary_config(Config) -> ok | {error, term()} when Config :: primary_config().
```

Update primary configuration data for Logger. This function behaves as if it was
implemented as follows:

```erlang
Old = logger:get_primary_config(),
logger:set_primary_config(maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_primary_config/1` ](`set_primary_config/1`).

# `update_process_metadata`
*since OTP 21.0* 

```erlang
-spec update_process_metadata(Meta) -> ok when Meta :: metadata().
```

Set or update metadata to use when logging from current process

If process metadata exists for the current process, this function behaves as if
it was implemented as follows:

```erlang
logger:set_process_metadata(maps:merge(logger:get_process_metadata(), Meta)).
```

If no process metadata exists, the function behaves as
[`set_process_metadata/1` ](`set_process_metadata/1`).

# `update_proxy_config`
*since OTP 21.3* 

```erlang
-spec update_proxy_config(Config) -> ok | {error, term()} when Config :: olp_config().
```

Update configuration data for the Logger proxy. This function behaves as if it
was implemented as follows:

```erlang
Old = logger:get_proxy_config(),
logger:set_proxy_config(maps:merge(Old, Config)).
```

To overwrite the existing configuration without any merge, use
[`set_proxy_config/1` ](`set_proxy_config/1`).

For more information about the proxy, see section
[Logger Proxy](logger_chapter.md#logger-proxy) in the Kernel User's Guide.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
