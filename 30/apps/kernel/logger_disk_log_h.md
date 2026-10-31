# `logger_disk_log_h`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_disk_log_h.erl#L22)

A disk_log based handler for Logger

This is a handler for Logger that offers circular (wrapped) logs by using
`m:disk_log`. Multiple instances of this handler can be added to Logger, and
each instance prints to its own disk log file, created with the name and
settings specified in the handler configuration.

The default standard handler, `m:logger_std_h`, can be replaced by a disk_log
handler at startup of the Kernel application. See an example of this below.

The handler has an overload protection mechanism that keeps the handler process
and the Kernel application alive during high loads of log events. How overload
protection works, and how to configure it, is described in the
[`User's Guide`](logger_chapter.md#overload_protection).

To add a new instance of the disk_log handler, use
[`logger:add_handler/3`](`logger:add_handler/3`). The handler configuration
argument is a map which can contain general configuration parameters, as
documented in the [`User's Guide`](logger_chapter.md#handler-configuration),
and handler specific parameters. The specific data is stored in a sub map with
the key `config`, and can contain the following parameters:

- **`file`** - This is the full name of the disk log file. The option
  corresponds to the `name` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to the same name as the handler identity, in the current directory.

- **`type`** - This is the disk log type, `wrap` or `halt`. The option
  corresponds to the `type` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `wrap`.

- **`max_no_files`** - This is the maximum number of files that disk_log uses
  for its circular logging. The option corresponds to the `MaxNoFiles` element
  in the `size` property in the [`dlog_option()`](`disk_log:open/1`) datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `10`.

  The setting has no effect on a halt log.

- **`max_no_bytes`** - This is the maximum number of bytes that is written to a
  log file before disk_log proceeds with the next file in order, or generates an
  error in case of a full halt log. The option corresponds to the `MaxNoBytes`
  element in the `size` property in the [`dlog_option()`](`disk_log:open/1`)
  datatype.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `1048576` bytes for a wrap log, and `infinity` for a halt log.

- **`filesync_repeat_interval`** - This value, in milliseconds, specifies how
  often the handler does a disk_log sync operation to write buffered data to
  disk. The handler attempts the operation repeatedly, but only performs a new
  sync if something has actually been logged.

  Defaults to `5000` milliseconds.

  If `no_repeat` is set as value, the repeated sync operation is disabled. The
  user can also call the [`filesync/1`](`filesync/1`) function to perform a
  disk_log sync.

Other configuration parameters exist, to be used for customizing the overload
protection behaviour. The same parameters are used both in the standard handler
and the disk_log handler, and are documented in the
[`User's Guide`](logger_chapter.md#overload_protection).

Notice that when changing the configuration of the handler in runtime, the
disk_log options (`file`, `type`, `max_no_files`, `max_no_bytes`) must not be
modified.

Example of adding a disk_log handler:

```erlang
logger:add_handler(my_disk_log_h, logger_disk_log_h,
                   #{config => #{file => "./my_disk_log",
                                 type => wrap,
                                 max_no_files => 4,
                                 max_no_bytes => 10000,
                                 filesync_repeat_interval => 1000}}).
```

To use the disk_log handler instead of the default standard handler when
starting an Erlang node, change the Kernel default logger to use
`logger_disk_log_h`. Example:

```text
erl -kernel logger '[{handler,default,logger_disk_log_h,
                      #{config => #{file => "./system_disk_log"}}}]'
```

### See Also

`m:logger`, `m:logger_std_h`, `m:disk_log`

# `filesync`
*since OTP 21.0* 

```erlang
-spec filesync(Name) -> ok | {error, Reason}
                  when Name :: atom(), Reason :: handler_busy | {badarg, term()}.
```

Write buffered data to disk.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
