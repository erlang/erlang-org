# `logger_std_h`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_std_h.erl#L22)

Standard handler for Logger.

This is the standard handler for Logger. Multiple instances of this handler can
be added to Logger, and each instance prints logs to
[`standard_io`](`t:io:standard_io/0`),
[`standard_error`](`t:io:standard_error/0`), or to file.

The handler has an overload protection mechanism that keeps the handler process
and the Kernel application alive during high loads of log events. How overload
protection works, and how to configure it, is described in the
[`User's Guide`](logger_chapter.md#overload_protection).

To add a new instance of the standard handler, use
[`logger:add_handler/3`](`logger:add_handler/3`). The handler configuration
argument is a map which can contain general configuration parameters, as
documented in the [`User's Guide`](logger_chapter.md#handler-configuration),
and handler specific parameters. The specific data is stored in a sub map with
the key `config`, and can contain the following parameters:

- **`type = ` `t:io:standard_io/0` ` | ` `t:io:standard_error/0` ` | file | {device, ` `t:io:device/0` `}`**{: #type } -
  Specifies the log destination.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to [`standard_io`](`t:io:standard_io/0`), unless parameter
  [`file`](`m:logger_std_h#file`) is given, in which case it defaults to `file`.

- **`file = ` `t:file:filename/0`**{: #file } - This specifies the name of the
  log file when the handler is of type `file`.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to the same name as the handler identity, in the current directory.

- **`modes = [` `t:file:mode/0` `]`**{: #modes } - This specifies the file modes
  to use when opening the log file, see `file:open/2`. If `modes` are not
  specified, the default list used is `[raw,append,delayed_write]`. If `modes`
  are specified, the list replaces the default modes list with the following
  adjustments:

  - If `raw` is not found in the list, it is added.
  - If none of `write`, `append` or `exclusive` is found in the list, `append`
    is added.
  - If none of `delayed_write` or `{delayed_write,Size,Delay}` is found in the
    list, `delayed_write` is added.

  Log files are always UTF-8 encoded. The encoding cannot be changed by setting
  the mode `{encoding,Encoding}`.

  The value is set when the handler is added, and it cannot be changed in
  runtime.

  Defaults to `[raw,append,delayed_write]`.

- **`max_no_bytes = ` `t:pos_integer/0` ` | infinity`{: #max_no_bytes }** - This
  parameter specifies if the log file should be rotated or not. The value
  `infinity` means the log file will grow indefinitely, while an integer value
  specifies at which file size (bytes) the file is rotated.

  Defaults to `infinity`.

- **`max_no_files = ` `t:non_neg_integer/0`**{: #max_no_files } - This parameter
  specifies the number of rotated log file archives to keep. This has meaning
  only if [`max_no_bytes`](`m:logger_std_h#max_no_bytes`) is set to an integer
  value.

  The log archives are named `FileName.0`, `FileName.1`, ... `FileName.N`, where
  `FileName` is the name of the current log file. `FileName.0` is the newest of
  the archives. The maximum value for `N` is the value of `max_no_files`
  minus 1.

  Notice that setting this value to `0` does not turn off rotation. It only
  specifies that no archives are kept.

  Defaults to `0`.

- **`compress_on_rotate = ` `t:boolean/0`**{: #compress_on_rotate } - This parameter
  specifies if the rotated log file archives shall be compressed or not. If set
  to `true`, all archives are compressed with `gzip`, and renamed to
  `FileName.N.gz`

  `compress_on_rotate` has no meaning if
  [`max_no_bytes`](`m:logger_std_h#max_no_bytes`) has the value `infinity`.

  Defaults to `false`.

- **`file_check = ` `t:non_neg_integer/0`**{: #file_check } - When `logger_std_h`
  logs to a file, it reads the file information of the log file prior to each
  write operation. This is to make sure the file still exists and has the same
  inode as when it was opened. This implies some performance loss, but ensures
  that no log events are lost in the case when the file has been removed or
  renamed by an external actor.

  In order to allow minimizing the performance loss, the `file_check` parameter
  can be set to a positive integer value, `N`. The handler will then skip
  reading the file information prior to writing, as long as no more than `N`
  milliseconds have passed since it was last read.

  Notice that the risk of losing log events grows when the `file_check` value
  grows.

  Defaults to 0.

- **`filesync_repeat_interval = ` `t:pos_integer/0` ` | no_repeat`** - This value, in
  milliseconds, specifies how often the handler does a file sync operation to
  write buffered data to disk. The handler attempts the operation repeatedly,
  but only performs a new sync if something has actually been logged.

  If `no_repeat` is set as value, the repeated file sync operation is disabled,
  and it is the operating system settings that determine how quickly or slowly
  data is written to disk. The user can also call the `filesync/1` function to
  perform a file sync.

  Defaults to `5000` milliseconds.

Other configuration parameters exist, to be used for customizing the overload
protection behaviour. The same parameters are used both in the standard handler
and the disk_log handler, and are documented in the
[`User's Guide`](logger_chapter.md#overload_protection).

Notice that if changing the configuration of the handler in runtime, the `type`,
`file`, or `modes` parameters must not be modified.

Example of adding a standard handler:

```erlang
logger:add_handler(my_standard_h, logger_std_h,
                   #{config => #{file => "./system_info.log",
                                 filesync_repeat_interval => 1000}}).
```

To set the default handler, that starts initially with the Kernel application,
to log to file instead of [`standard_io`](`t:io:standard_io/0`), change the
Kernel default logger configuration. Example:

```text
erl -kernel logger '[{handler,default,logger_std_h,
                      #{config => #{file => "./log.log"}}}]'
```

An example of how to replace the standard handler with a disk_log handler at
startup is found in the `m:logger_disk_log_h` manual.

### See Also

`m:logger`, `m:logger_disk_log_h`

# `filesync`
*since OTP 21.0* 

```erlang
-spec filesync(Name) -> ok | {error, Reason}
                  when Name :: atom(), Reason :: handler_busy | {badarg, term()}.
```

Write buffered data to disk.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
