# `rb`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/sasl/src/rb.erl#L22)

The Report Browser Tool

The Report Browser (RB) tool is used to browse and format error reports written
by the error logger handler `m:log_mf_h` in STDLIB.

# `filter`
*not exported* 

```elixir
-type filter() ::
          {Key :: term(), Value :: term()} |
          {Key :: term(), Value :: term(), no} |
          {Key :: term(), RegExp :: regexp(), re} |
          {Key :: term(), RegExp :: regexp(), re, no}.
```

# `option`
*not exported* 

```elixir
-type option() ::
          {start_log, FileName :: string() | atom() | pid()} |
          {max, MaxNoOfReports :: integer() | all} |
          {report_dir, DirString :: string()} |
          {type, ReportType :: type() | [type()] | all} |
          {abort_on_error, boolean()}.
```

# `regexp`
*not exported* 

```elixir
-type regexp() ::
          string() |
          {string(), Options :: [re:options()]} |
          re:mp() |
          {re:mp(), Options :: [re:compile_options()]}.
```

# `type`
*not exported* 

```elixir
-type type() ::
          error | error_report | info_msg | info_report | warning_msg | warning_report | crash_report |
          supervisor_report | progress | all.
```

# `filter`
*since OTP R13B04* 

```elixir
-spec filter(Filters) -> term() when Filters :: [filter()].
```

# `filter`
*since OTP R13B04* 

```elixir
-spec filter(Filters, Dates) -> term()
                when
                    Filters :: [filter()],
                    Dates :: {DateFrom, DateTo} | {DateFrom, from} | {DateTo, to},
                    DateFrom :: calendar:datetime(),
                    DateTo :: calendar:datetime().
```

Displays the reports that match the provided filters.

When a filter includes the `no` atom, it excludes the reports that match that
filter.

The reports are matched using the `m:proplists` module in STDLIB. The report
must be a proplist to be matched against any of the filters.

If the filter has the form `{Key, RegExp, re}`, the report must contain an
element with key equal to `Key` and the value must match the regular expression
`RegExp`.

If parameter `Dates` is specified, the reports are filtered according to the
date when they occurred. If `Dates` has the form `{DateFrom, from}`, reports
that occurred after `DateFrom` are displayed.

If `Dates` has the form `{DateTo, to}`, reports that occurred before `DateTo`
are displayed.

If two `Dates` are specified, reports that occurred between those dates are
returned.

To filter only by dates, specify the empty list as the `Filters` parameter.

For details about parameter `RegExp`, see `rb:grep/1`.

For details about data type `mp()`, see `t:re:mp/0`.

For details about data type `datetime()`, see `t:calendar:datetime/0`.

# `grep`

```elixir
-spec grep(RegExp :: regexp()) -> term().
```

All reports matching the regular expression `RegExp` are displayed. `RegExp` can
be any of the following:

- A string containing the regular expression
- A tuple with the string and the options for compilation
- A compiled regular expression
- A compiled regular expression and the options for running it

For a definition of valid regular expressions and options, see the `m:re` module
in STDLIB and in particular function `re:run/3`.

For details about data type `mp()`, see `t:re:mp/0`.

# `h`

```elixir
-spec h() -> term().
```

# `help`

```elixir
-spec help() -> term().
```

Displays online help information.

# `list`

```elixir
-spec list() -> term().
```

# `list`

```elixir
-spec list(Type :: type()) -> term().
```

Lists all reports loaded in `rb_server`. Each report is given a unique number
that can be used as a reference to the report in function `show/1`.

If no `Type` is specified, all reports are listed.

# `log_list`
*since OTP R16B02* 

```elixir
-spec log_list() -> term().
```

# `log_list`
*since OTP R16B02* 

```elixir
-spec log_list(Type :: type()) -> term().
```

Same as functions `list/0` or `list/1`, but the result is printed to a log file,
if set; otherwise to `standard_io`.

If no `Type` is specified, all reports are listed.

# `rescan`

```elixir
-spec rescan() -> term().
```

# `rescan`

```elixir
-spec rescan(Options) -> term() when Options :: [option()].
```

Rescans the report directory. `Options` is the same as for function `start/1`.

# `show`

```elixir
-spec show() -> term().
```

All reports are displayed.

# `show`

```elixir
-spec show(Report) -> term() when Report :: integer() | type().
```

If argument `Report` is specified as one of the values of
[`type()`](`t:type/0`), all loaded reports of that type are
displayed. If `Report` is specified as an integer, the report with
this reference number is displayed.

# `start`

```elixir
-spec start() -> term().
```

# `start`

```elixir
-spec start(Options) -> term() when Options :: [option()].
```

Function [`start/1`](`start/1`) starts `rb_server` with the specified options,
whereas function `start/0` starts with default options.

`rb_server` must be started before reports can be browsed. When
`rb_server` is started, the files in the specified directory are
scanned. The other functions assume that the server has started.

_Options:_

- **`{start_log, FileName}`** - Starts logging to file, registered name, or
  `io_device`. All reports are printed to the specified destination. Default is
  `standard_io`. Option `{start_log, standard_error}` is not allowed and will be
  replaced by default `standard_io`.

- **`{max, MaxNoOfReports}`** - Controls how many reports `rb_server` is to read
  at startup. This option is useful, as the directory can contain a large amount
  of reports. If this option is specified, the `MaxNoOfReports` latest reports
  are read. Default is `all`.

- **`{report_dir, DirString}`** - Defines the directory where the error log
  files are located. Default is the directory specified by application
  environment variable `error_logger_mf_dir`, see [sasl(6)](sasl_app.md).

- **`{type, ReportType}`** - Controls what kind of reports `rb_server` is to
  read at startup. `ReportType` is a supported type, `all`, or a list of
  supported types. Default is `all`.

- **`{abort_on_error, Bool}`** - Specifies if logging is to be ended if `rb`
  encounters an unprintable report. (You can get a report with an incorrect form
  if function `error_logger`, `error_msg`, or `info_msg` has been called with an
  invalid format string)

  - If `Bool` is `true`, `rb` stops logging (and prints an error message to
    `stdout`) if it encounters a badly formatted report. If logging to file is
    enabled, an error message is appended to the log file as well.
  - If `Bool` is `false` (the default value), `rb` prints an error message to
    `stdout` for every bad report it encounters, but the logging process is
    never ended. All printable reports are written. If logging to file is
    enabled, `rb` prints `* UNPRINTABLE REPORT *` in the log file at the
    location of an unprintable report.

# `start_log`

```elixir
-spec start_log(FileName) -> term() when FileName :: string() | atom() | pid().
```

Redirects all report output from the RB tool to the specified file, registered
name, or `io_device`.

# `stop`

```elixir
-spec stop() -> term().
```

Stops `rb_server`.

# `stop_log`

```elixir
-spec stop_log() -> term().
```

Closes the log file. The output from the RB tool is directed to `standard_io`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
