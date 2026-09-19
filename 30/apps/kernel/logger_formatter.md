# `logger_formatter`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_formatter.erl#L22)

Default formatter for Logger.

Each Logger handler has a configured formatter specified as a module and a
configuration term. The purpose of the formatter is to translate the log events
to a final printable string ([`unicode:chardata()`](`t:unicode:chardata/0`))
which can be written to the output device of the handler. See sections
[Handlers](logger_chapter.md#handlers) and
[Formatters](logger_chapter.md#formatters) in the Kernel User's Guide for more
information.

`m:logger_formatter` is the default formatter used by Logger.

### See Also

`m:calendar`, `m:error_logger`, `m:io`, `m:io_lib`, `m:logger`, `m:maps`,
[`sasl(6)`](`e:sasl:sasl_app.md`), `m:unicode`

# `config`
*not exported* *since OTP 21.0* 

```erlang
-type config() ::
          #{chars_limit => pos_integer() | unlimited,
            depth => pos_integer() | unlimited,
            legacy_header => boolean(),
            max_size => pos_integer() | unlimited,
            report_cb => logger:report_cb(),
            single_line => boolean(),
            template => template(),
            time_designator => byte(),
            time_offset => integer() | [byte()]}.
```

The configuration term for `logger_formatter` is a [map](`m:maps`), and the
following keys can be set as configuration parameters:

- **`chars_limit = integer() > 0 | unlimited`{: #chars_limit }** - A positive
  integer representing the value of the option with the same name to be used
  when calling `io_lib:format/3`. This value limits the total number of
  characters printed for each log event. Notice that this is a soft limit. For a
  hard truncation limit, see option `max_size`.

  Defaults to `unlimited`.

- **`depth = integer() > 0 | unlimited`{: #depth }** - A positive integer
  representing the maximum depth to which terms shall be printed by this
  formatter. Format strings passed to this formatter are rewritten. The format
  controls ~p and ~w are replaced with ~P and ~W, respectively, and the value is
  used as the depth parameter. For details, see [`io:format/2,3`](`io:format/2`)
  in STDLIB.

  Defaults to `unlimited`.

- **`legacy_header = boolean()`** - If set to `true` a header field is added to
  logger_formatter's part of `Metadata`. The value of this field is a string
  similar to the header created by the old `m:error_logger` event handlers. It
  can be included in the log event by adding the list
  `[logger_formatter,header]` to the template. See the description of the
  `t:template/0` type for more information.

  Defaults to `false`.

- **`max_size = integer() > 0 | unlimited`{: #max_size }** - A positive integer
  representing the absolute maximum size a string returned from this formatter
  can have. If the formatted string is longer, after possibly being limited by
  `chars_limit` or `depth`, it is truncated.

  Defaults to `unlimited`.

- **`report_cb = ` `t:logger:report_cb/0`** - A report callback is used by the
  formatter to transform log messages on report form to a format string and
  arguments. The report callback can be specified in the metadata for the log
  event. If no report callback exists in metadata, `logger_formatter` will use
  `logger:format_report/1` as default callback.

  If this configuration parameter is set, it replaces both the default report
  callback, and any report callback found in metadata. That is, all reports are
  converted by this configured function.

- **`single_line = boolean()`** - If set to `true`, each log event is printed as
  a single line. To achieve this, `logger_formatter` sets the field width to `0`
  for all `~p` and `~P` control sequences in the format a string (see
  `io:format/2`), and replaces all newlines in the message with `", "`. White
  spaces following directly after newlines are removed. Notice that newlines
  added by the `template` parameter are not replaced.

  Defaults to `true`.

- **`template = `{: #template }`t:template/0`** - The template describes how the
  formatted string is composed by combining different data values from the log
  event. See the description of the `t:template/0` type for more information
  about this.

- **`time_designator = byte()`** - Timestamps are formatted according to
  RFC3339, and the time designator is the character used as date and time
  separator.

  Defaults to `$T`.

  The value of this parameter is used as the `time_designator` option to
  `calendar:system_time_to_rfc3339/2`.

- **`time_offset = integer() | [byte()]`** - The time offset, either a string or
  an integer, to be used when formatting the timestamp.

  An empty string is interpreted as local time. The values `"Z"`, `"z"` or `0`
  are interpreted as Universal Coordinated Time (UTC).

  Strings, other than `"Z"`, `"z"`, or `""`, must be of the form `±[hh]:[mm]`,
  for example `"-02:00"` or `"+00:00"`.

  Integers must be in microseconds, meaning that the offset `7200000000` is
  equivalent to `"+02:00"`.

  Defaults to an empty string, meaning that timestamps are displayed in local
  time. However, for backwards compatibility, if the SASL configuration
  parameter [`utc_log`](`e:sasl:sasl_app.md#utc_log`)`=true`, the default is
  changed to `"Z"`, meaning that timestamps are displayed in UTC.

  The value of this parameter is used as the `offset` option to
  `calendar:system_time_to_rfc3339/2`.

# `metakey`
*not exported* *since OTP 21.0* 

```erlang
-type metakey() :: atom() | [atom()].
```

# `template`
*not exported* *since OTP 21.0* 

```erlang
-type template() :: [metakey() | {metakey(), template(), template()} | unicode:chardata()].
```

The template to be used by a logger formatter.

The template is a list of atoms, atom lists, tuples and strings. The atoms
`level` or `msg`, are treated as placeholders for the severity level and the log
message, respectively. Other atoms or atom lists are interpreted as placeholders
for metadata, where atoms are expected to match top level keys, and atom lists
represent paths to sub keys when the metadata is a nested map. For example the
list `[key1,key2]` is replaced by the value of the `key2` field in the nested
map below. The atom `key1` on its own is replaced by the complete value of the
`key1` field. The values are converted to strings.

```text
#{key1 => #{key2 => my_value,
            ...}
  ...}
```

Tuples in the template express if-exist tests for metadata keys. For example,
the following tuple says that if `key1` exists in the metadata map, print
`"key1=Value"`, where `Value` is the value that `key1` is associated with in the
metadata map. If `key1` does not exist, print nothing.

```text
{key1, ["key1=",key1], []}
```

Strings in the template are printed literally.

The default value for the `template` configuration parameter depends on the
value of the `single_line` and `legacy_header` configuration parameters as
follows.

The log event used in the examples is:

```text
?LOG_ERROR("name: ~p~nexit_reason: ~p", [my_name, "It crashed"])
```

- **`legacy_header = true, single_line = false`** - Default template:
  `[[logger_formatter,header],"\n",msg,"\n"]`

  Example log entry:

  ```text
  =ERROR REPORT==== 17-May-2018::18:30:19.453447 ===
  name: my_name
  exit_reason: "It crashed"
  ```

  Notice that all eight levels can occur in the heading, not only `ERROR`,
  `WARNING` or `INFO` as `m:error_logger` produces. And microseconds are added
  at the end of the timestamp.

- **`legacy_header = true, single_line = true`** - Default template:
  `[[logger_formatter,header],"\n",msg,"\n"]`

  Notice that the template is here the same as for `single_line=false`, but the
  resulting log entry differs in that there is only one line after the heading:

  ```text
  =ERROR REPORT==== 17-May-2018::18:31:06.952665 ===
  name: my_name, exit_reason: "It crashed"
  ```

- **`legacy_header = false, single_line = true`** - Default template:
  `[time," ",level,": ",msg,"\n"]`

  Example log entry:

  ```text
  2018-05-17T18:31:31.152864+02:00 error: name: my_name, exit_reason: "It crashed"
  ```

- **`legacy_header = false, single_line = false`** - Default template:
  `[time," ",level,":\n",msg,"\n"]`

  Example log entry:

  ```text
  2018-05-17T18:32:20.105422+02:00 error:
  name: my_name
  exit_reason: "It crashed"
  ```

# `check_config`
*since OTP 21.0* 

```erlang
-callback check_config(FConfig) -> ok | {error, Reason}
                          when FConfig :: logger:formatter_config(), Reason :: term().
```

The function is called by a Logger when formatter configuration is set or
modified. The formatter must validate the given configuration and return `ok` if
it is correct, and `{error,Reason}` if it is faulty.

The following Logger API functions can trigger this callback:

- `logger:add_handler/3`
- [`logger:set_handler_config/2,3`](`logger:set_handler_config/2`)
- [`logger:update_handler_config/2,3`](`logger:update_handler_config/2`)
- `logger:update_formatter_config/2`

See `m:logger_formatter` for an example implementation. `m:logger_formatter` is
the default formatter used by Logger.

# `format`
*since OTP 21.0* 

```erlang
-callback format(LogEvent, FConfig) -> FormattedLogEntry
                    when
                        LogEvent :: logger:log_event(),
                        FConfig :: logger:formatter_config(),
                        FormattedLogEntry :: unicode:chardata().
```

The function can be called by a log handler to convert a log event term to a
printable string. The returned value can, for example, be printed as a log entry
to the console or a file using [`io:put_chars/1,2`](`io:put_chars/1`).

See `m:logger_formatter` for an example implementation. `m:logger_formatter` is
the default formatter used by Logger.

# `check_config`
*since OTP 21.0* 

```erlang
-spec check_config(Config) -> ok | {error, term()} when Config :: config().
```

The function is called by Logger when the formatter configuration for a handler
is set or modified. It returns `ok` if the configuration is valid, and
`{error,term()}` if it is faulty.

The following Logger API functions can trigger this callback:

- `logger:add_handler/3`
- [`logger:set_handler_config/2,3`](`logger:set_handler_config/2`)
- `logger:update_handler_config/2`
- `logger:update_formatter_config/2`

# `format`
*since OTP 21.0* 

```erlang
-spec format(LogEvent, Config) -> unicode:chardata()
                when LogEvent :: logger:log_event(), Config :: config().
```

This the formatter callback function to be called from handlers.

The log event is processed as follows:

- If the message is on report form, it is converted to `{Format,Args}` by
  calling the report callback. See section
  [Log Message](logger_chapter.md#log-message) in the Kernel User's Guide for
  more information about report callbacks and valid forms of log messages.
- The message size is limited according to the values of configuration
  parameters [`chars_limit`](`m:logger_formatter#chars_limit`) and
  [`depth`](`m:logger_formatter#depth`).
- The full log entry is composed according to the
  [`template`](`m:logger_formatter#template`).
- If the final string is too long, it is truncated according to the value of
  configuration parameter [`max_size`](`m:logger_formatter#max_size`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
