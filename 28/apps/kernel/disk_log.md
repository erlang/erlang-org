# `disk_log`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/kernel/src/disk_log.erl#L22)

`disk_log` is a disk-based term logger that enables efficient logging of items
on files.

Three types of logs are supported:

- **halt logs** - Appends items to a single file, which size can be limited by
  the `disk_log` module.

- **wrap logs** - Uses a sequence of wrap log files of limited size. As a wrap
  log file is filled up, further items are logged on to the next file in the
  sequence, starting all over with the first file when the last file is filled
  up.

- **rotate logs** - Uses a sequence of rotate log files of limited size. As a
  log file is filled up, it is rotated and then compressed. There is one active
  log file and upto the configured number of compressed log files. Only
  externally formatted logs are supported. It follows the same naming convention
  as the handler logger_std_h for Logger. For more details about the naming
  convention check the file parameter for `open/1`.

  It follows the same naming convention as that for the compressed files for
  Linux's logrotate and BSD's newsyslog.

For efficiency reasons, items are always written to files as binaries.

Two formats of the log files are supported:

- **internal format** - Supports automatic repair of log files that are not
  properly closed and enables efficient reading of logged items in _chunks_
  using a set of functions defined in this module. This is the only way to read
  internally formatted logs. An item logged to an internally formatted log must
  not occupy more than 4 GB of disk space (the size must fit in 4 bytes).

- **external format** - Leaves it up to the user to read and interpret the
  logged data. The `disk_log` module cannot repair externally formatted logs.

For each open disk log, one process handles requests made to the disk log. This
process is created when `open/1` is called, provided there exists no process
handling the disk log. A process that opens a disk log can be an _owner_ or an
anonymous _user_ of the disk log. Each owner is linked to the disk log process,
and an owner can close the disk log either explicitly (by calling
[`close/1`](`close/1`)) or by terminating.

Owners can subscribe to _notifications_, messages of the form
`{disk_log, Node, Log, Info}`, which are sent from the disk log process when
certain events occur, see the functions and in particular the
[`open/1`](`open/1`) option [`notify`](`m:disk_log#notify`). A log can have many
owners, but a process cannot own a log more than once. However, the same process
can open the log as a user more than once.

For a disk log process to close its file properly and terminate, it must be
closed by its owners and once by some non-owner process for each time the log
was used anonymously. The users are counted and there must not be any users left
when the disk log process terminates.

Items can be logged _synchronously_ by using functions `log/2`, `blog/2`,
`log_terms/2`, and `blog_terms/2`. For each of these functions, the caller is
put on hold until the items are logged (but not necessarily written, use
[`sync/1`](`sync/1`) to ensure that). By adding an `a` to each of the mentioned
function names, we get functions that log items _asynchronously_. Asynchronous
functions do not wait for the disk log process to write the items to the file,
but return the control to the caller more or less immediately.

When using the internal format for logs, use functions `log/2`, `log_terms/2`,
`alog/2`, and `alog_terms/2`. These functions log one or more Erlang terms. By
prefixing each of the functions with a `b` (for "binary"), we get the
corresponding `blog()` functions for the external format. These functions log
one or more chunks of bytes. For example, to log the string `"hello"` in ASCII
format, you can use [`disk_log:blog(Log, "hello")`](`blog/2`), or
[`disk_log:blog(Log, list_to_binary("hello"))`](`blog/2`). The two alternatives
are equally efficient.

The `blog()` functions can also be used for internally formatted logs, but in
this case they must be called with binaries constructed with calls to
[`term_to_binary/1`](`erlang:term_to_binary/1`). There is no check to ensure
this, it is entirely the responsibility of the caller. If these functions are
called with binaries that do not correspond to Erlang terms, the
[`chunk/2,3`](`chunk/3`) and automatic repair functions fail. The corresponding
terms (not the binaries) are returned when [`chunk/2,3`](`chunk/3`) is called.

An open disk log is only accessible from the node where the disk log process
runs. All processes on the node where the disk log process runs can log items or
otherwise change, inspect, or close the log.

Errors are reported differently for asynchronous log attempts and other uses of
the `m:disk_log` module. When used synchronously, this module replies with an
error message, but when called asynchronously, this module does not know where
to send the error message. Instead, owners subscribing to notifications receive
an `error_status` message.

The `m:disk_log` module does not report errors to the `m:error_logger` module. It
is up to the caller to decide whether to employ the error logger. Function
`format_error/1` can be used to produce readable messages from error replies.
However, information events are sent to the error logger in two situations,
namely when a log is repaired, or when a file is missing while reading chunks.

Error message `no_such_log` means that the specified disk log is not open.
Nothing is said about whether the disk log files exist or not.

> #### Note {: .info }
>
> If an attempt to reopen or truncate a log fails (see
> [`reopen/2,3`](`reopen/3`) and [`truncate/1,2`](`truncate/2`)) the disk log
> process terminates immediately. Before the process terminates, links to owners
> and blocking processes (see [`block/1,2`](`block/2`)) are removed. The effect
> is that the links work in one direction only. Any process using a disk log
> must check for error message `no_such_log` if some other process truncates or
> reopens the log simultaneously.

## See Also

`m:file`, `m:wrap_log_reader`

# `bchunk_ret`
*not exported* 

```elixir
-type bchunk_ret() ::
          {Continuation2 :: continuation(), Binaries :: [binary()]} |
          {Continuation2 :: continuation(), Binaries :: [binary()], Badbytes :: non_neg_integer()} |
          eof |
          {error, Reason :: chunk_error_rsn()}.
```

# `block_error_rsn`
*not exported* 

```elixir
-type block_error_rsn() :: no_such_log | nonode | {blocked_log, log()}.
```

# `chunk_error_rsn`
*not exported* 

```elixir
-type chunk_error_rsn() ::
          no_such_log |
          {format_external, log()} |
          {blocked_log, log()} |
          {badarg, continuation} |
          {not_internal_wrap, log()} |
          {corrupt_log_file, FileName :: file:filename()} |
          {file_error, file:filename(), file_error()}.
```

# `chunk_ret`
*not exported* 

```elixir
-type chunk_ret() ::
          {Continuation2 :: continuation(), Terms :: [term()]} |
          {Continuation2 :: continuation(), Terms :: [term()], Badbytes :: non_neg_integer()} |
          eof |
          {error, Reason :: chunk_error_rsn()}.
```

# `close_error_rsn`
*not exported* 

```elixir
-type close_error_rsn() :: no_such_log | nonode | {file_error, file:filename(), file_error()}.
```

# `continuation`

```elixir
-opaque continuation() ::
            #continuation{pid :: pid(),
                          pos :: non_neg_integer() | {integer(), non_neg_integer()},
                          b :: binary() | [] | pos_integer()}.
```

Chunk continuation returned by [`chunk/2,3`](`chunk/3`),
[`bchunk/2,3`](`bchunk/3`), or [`chunk_step/3`](`chunk_step/3`).

# `dlog_format`
*not exported* 

```elixir
-type dlog_format() :: external | internal.
```

# `dlog_head_opt`
*not exported* 

```elixir
-type dlog_head_opt() :: none | term() | iodata().
```

# `dlog_info`
*not exported* 

```elixir
-type dlog_info() ::
          {name, Log :: log()} |
          {file, File :: file:filename()} |
          {type, Type :: dlog_type()} |
          {format, Format :: dlog_format()} |
          {size, Size :: dlog_size()} |
          {mode, Mode :: dlog_mode()} |
          {owners, [{pid(), Notify :: boolean()}]} |
          {users, Users :: non_neg_integer()} |
          {status, Status :: ok | {blocked, QueueLogRecords :: boolean()}} |
          {node, Node :: node()} |
          {head, Head :: none | {head, binary()} | (MFA :: {atom(), atom(), list()})} |
          {no_written_items, NoWrittenItems :: non_neg_integer()} |
          {full, Full :: boolean} |
          {no_current_bytes, non_neg_integer()} |
          {no_current_items, non_neg_integer()} |
          {no_items, non_neg_integer()} |
          {current_file, pos_integer()} |
          {no_overflows, {SinceLogWasOpened :: non_neg_integer(), SinceLastInfo :: non_neg_integer()}}.
```

# `dlog_mode`
*not exported* 

```elixir
-type dlog_mode() :: read_only | read_write.
```

# `dlog_optattr`
*not exported* 

```elixir
-type dlog_optattr() ::
          name | file | linkto | repair | type | format | size | notify | head | head_func | mode.
```

# `dlog_option`
*not exported* 

```elixir
-type dlog_option() ::
          {name, Log :: log()} |
          {file, FileName :: file:filename()} |
          {linkto, LinkTo :: none | pid()} |
          {repair, Repair :: true | false | truncate} |
          {type, Type :: dlog_type()} |
          {format, Format :: dlog_format()} |
          {size, Size :: dlog_size()} |
          {notify, boolean()} |
          {head, Head :: dlog_head_opt()} |
          {head_func, MFA :: {atom(), atom(), list()}} |
          {quiet, boolean()} |
          {mode, Mode :: dlog_mode()}.
```

# `dlog_options`
*not exported* 

```elixir
-type dlog_options() :: [dlog_option()].
```

# `dlog_size`
*not exported* 

```elixir
-type dlog_size() ::
          infinity | pos_integer() | {MaxNoBytes :: pos_integer(), MaxNoFiles :: pos_integer()}.
```

# `dlog_type`
*not exported* 

```elixir
-type dlog_type() :: halt | wrap | rotate.
```

# `file_error`
*not exported* 

```elixir
-type file_error() :: term().
```

# `inc_wrap_error_rsn`
*not exported* 

```elixir
-type inc_wrap_error_rsn() :: next_file_error_rsn().
```

# `invalid_header`
*not exported* 

```elixir
-type invalid_header() :: term().
```

# `log`
*not exported* 

```elixir
-type log() :: term().
```

# `log_error_rsn`
*not exported* 

```elixir
-type log_error_rsn() ::
          no_such_log | nonode |
          {read_only_mode, log()} |
          {format_external, log()} |
          {blocked_log, log()} |
          {full, log()} |
          {invalid_header, invalid_header()} |
          {file_error, file:filename(), file_error()}.
```

# `next_file_error_rsn`
*not exported* 

```elixir
-type next_file_error_rsn() ::
          no_such_log | nonode |
          {read_only_mode, log()} |
          {blocked_log, log()} |
          {halt_log, log()} |
          {rotate_log, log()} |
          {invalid_header, invalid_header()} |
          {file_error, file:filename(), file_error()}.
```

# `notify_ret`
*not exported* 

```elixir
-type notify_ret() :: ok | {error, no_such_log}.
```

# `open_error_rsn`
*not exported* 

```elixir
-type open_error_rsn() ::
          no_such_log |
          {badarg, term()} |
          {size_mismatch, CurrentSize :: dlog_size(), NewSize :: dlog_size()} |
          {arg_mismatch, OptionName :: dlog_optattr(), CurrentValue :: term(), Value :: term()} |
          {name_already_open, Log :: log()} |
          {open_read_write, Log :: log()} |
          {open_read_only, Log :: log()} |
          {need_repair, Log :: log()} |
          {not_a_log_file, FileName :: file:filename()} |
          {invalid_index_file, FileName :: file:filename()} |
          {invalid_header, invalid_header()} |
          {file_error, file:filename(), file_error()} |
          {node_already_open, Log :: log()}.
```

# `open_ret`
*not exported* 

```elixir
-type open_ret() ::
          {ok, Log :: log()} |
          {repaired,
           Log :: log(),
           {recovered, Rec :: non_neg_integer()},
           {badbytes, Bad :: non_neg_integer()}} |
          {error, open_error_rsn()}.
```

# `reopen_error_rsn`
*not exported* 

```elixir
-type reopen_error_rsn() ::
          no_such_log | nonode |
          {read_only_mode, log()} |
          {blocked_log, log()} |
          {same_file_name, log()} |
          {invalid_index_file, file:filename()} |
          {invalid_header, invalid_header()} |
          {file_error, file:filename(), file_error()}.
```

# `sync_error_rsn`
*not exported* 

```elixir
-type sync_error_rsn() ::
          no_such_log | nonode |
          {read_only_mode, log()} |
          {blocked_log, log()} |
          {file_error, file:filename(), file_error()}.
```

# `trunc_error_rsn`
*not exported* 

```elixir
-type trunc_error_rsn() ::
          no_such_log | nonode |
          {read_only_mode, log()} |
          {blocked_log, log()} |
          {invalid_header, invalid_header()} |
          {file_error, file:filename(), file_error()}.
```

# `unblock_error_rsn`
*not exported* 

```elixir
-type unblock_error_rsn() :: no_such_log | nonode | {not_blocked, log()} | {not_blocked_by_pid, log()}.
```

# `all`
*since OTP 24.0* 

```elixir
-spec all() -> [Log] when Log :: log().
```

Returns the names of the disk logs accessible on the current node.

# `alog`

```elixir
-spec alog(Log, Term) -> notify_ret() when Log :: log(), Term :: term().
```

Asynchronously version of `log/2`.

Owners subscribing to notifications receive message `read_only`, `blocked_log`,
or `format_external` if the item cannot be written on the log, and possibly one
of the messages `wrap`, `full`, or `error_status` if an item is written on the
log. Message `error_status` is sent if something is wrong with the header
function or if a file error occurs.

# `alog_terms`

```elixir
-spec alog_terms(Log, TermList) -> notify_ret() when Log :: log(), TermList :: [term()].
```

Asynchronously version of `log_terms/2`.

Owners subscribing to notifications receive message `read_only`, `blocked_log`,
or `format_external` if the items cannot be written on the log, and possibly one
or more of the messages `wrap`, `full`, and `error_status` if items are written
on the log. Message `error_status` is sent if something is wrong with the header
function or if a file error occurs.

# `balog`

```elixir
-spec balog(Log, Bytes) -> notify_ret() when Log :: log(), Bytes :: iodata().
```

Asynchronously version of `blog/2`.

Owners subscribing to notifications receive message `read_only`, `blocked_log`,
or `format_external` if the item cannot be written on the log, and possibly one
of the messages `wrap`, `full`, or `error_status` if an item is written on the
log. Message `error_status` is sent if something is wrong with the header
function or if a file error occurs.

# `balog_terms`

```elixir
-spec balog_terms(Log, ByteList) -> notify_ret() when Log :: log(), ByteList :: [iodata()].
```

Asynchronously version of `blog_terms/2`.

Owners subscribing to notifications receive message `read_only`, `blocked_log`,
or `format_external` if the items cannot be written on the log, and possibly one
or more of the messages `wrap`, `full`, and `error_status` if items are written
on the log. Message `error_status` is sent if something is wrong with the header
function or if a file error occurs.

# `bchunk`

```elixir
-spec bchunk(Log, Continuation) -> bchunk_ret()
                when Log :: log(), Continuation :: start | continuation().
```

# `bchunk`

```elixir
-spec bchunk(Log, Continuation, N) -> bchunk_ret()
                when Log :: log(), Continuation :: start | continuation(), N :: pos_integer() | infinity.
```

Equivalent to [`chunk(Log, Continuation, N)`](`chunk/3`) except that
it returns the binaries read from the file, that is it does not call
`binary_to_term/1`.

# `block`

```elixir
-spec block(Log) -> ok | {error, block_error_rsn()} when Log :: log().
```

# `block`

```elixir
-spec block(Log, QueueLogRecords) -> ok | {error, block_error_rsn()}
               when Log :: log(), QueueLogRecords :: boolean().
```

With a call to `block/2` a process can block a log.

If the blocking process is not an owner of the log, a temporary link is created
between the disk log process and the blocking process. The link ensures that the disk log is
unblocked if the blocking process terminates without first closing or unblocking
the log.

Any process can probe a blocked log with [`info/1`](`info/1`) or close it with
[`close/1`](`close/1`). The blocking process can also use functions [`chunk/2,3`](`chunk/3`),
[`bchunk/2,3`](`chunk/3`), [`chunk_step/3`](`chunk_step/3`), and [`unblock/1`](`unblock/1`)
without being affected by the block. Any other attempt than those mentioned so
far to update or read a blocked log suspends the calling process until the log
is unblocked or returns error message `{blocked_log, Log}`, depending on whether
the value of `QueueLogRecords` is `true` or `false`.

# `blog`

```elixir
-spec blog(Log, Bytes) -> ok | {error, Reason :: log_error_rsn()} when Log :: log(), Bytes :: iodata().
```

Equivalent to `log/2` except that it is used for externally formatted logs.

`blog/2` can also be used for internally formatted logs
if the binaries are constructed with calls to `term_to_binary/1`.

# `blog_terms`

```elixir
-spec blog_terms(Log, BytesList) -> ok | {error, Reason :: log_error_rsn()}
                    when Log :: log(), BytesList :: [iodata()].
```

Equivalent to `log_terms/2` except that it is used for externally formatted logs.

`blog_terms/2` can also be used for internally formatted logs
if the binaries are constructed with calls to `term_to_binary/1`.

# `breopen`

```elixir
-spec breopen(Log, File, BHead) -> ok | {error, reopen_error_rsn()}
                 when Log :: log(), File :: file:filename(), BHead :: iodata().
```

Equivalent to `reopen` except that it is used for externally formatted logs.

# `btruncate`

```elixir
-spec btruncate(Log, BHead) -> ok | {error, trunc_error_rsn()} when Log :: log(), BHead :: iodata().
```

Equivalent to `truncate/2` for externally formatted logs.

# `change_header`

```elixir
-spec change_header(Log, Header) -> ok | {error, Reason}
                       when
                           Log :: log(),
                           Header ::
                               {head, dlog_head_opt()} | {head_func, MFA :: {atom(), atom(), list()}},
                           Reason ::
                               no_such_log | nonode |
                               {read_only_mode, Log} |
                               {blocked_log, Log} |
                               {badarg, head}.
```

Changes the value of option `head` or `head_func` for an owner of a disk log.

# `change_notify`

```elixir
-spec change_notify(Log, Owner, Notify) -> ok | {error, Reason}
                       when
                           Log :: log(),
                           Owner :: pid(),
                           Notify :: boolean(),
                           Reason ::
                               no_such_log | nonode |
                               {blocked_log, Log} |
                               {badarg, notify} |
                               {not_owner, Owner}.
```

Changes the value of option `notify` for an owner of a disk log.

# `change_size`

```elixir
-spec change_size(Log, Size) -> ok | {error, Reason}
                     when
                         Log :: log(),
                         Size :: dlog_size(),
                         Reason ::
                             no_such_log | nonode |
                             {read_only_mode, Log} |
                             {blocked_log, Log} |
                             {new_size_too_small, Log, CurrentSize :: pos_integer()} |
                             {badarg, size} |
                             {file_error, file:filename(), file_error()}.
```

Changes the size of an open log. For a halt log, the size can always be
increased, but it cannot be decreased to something less than the current file
size.

For a wrap or rotate log, both the size and the number of files can always be
increased, as long as the number of files does not exceed 65000. For wrap logs,
if the maximum number of files is decreased, the change is not valid until the
current file is full and the log wraps to the next file. The redundant files are
removed the next time the log wraps around, that is, starts to log to file
number 1.

As an example, assume that the old maximum number of files is 10 and that the
new maximum number of files is 6. If the current file number is not greater than
the new maximum number of files, files 7-10 are removed when file 6 is full and
the log starts to write to file number 1 again. Otherwise, the files greater
than the current file are removed when the current file is full (for example, if
the current file is 8, files 9 and 10 are removed). The files between the new
maximum number of files and the current file (that is, files 7 and 8) are
removed the next time file 6 is full.

For rotate logs, if the maximum number of files is decreased, the redundant
files are deleted instantly.

If the size of the files is decreased, the change immediately affects the
current log. It does not change the size of log files already full until the
next time they are used.

If the log size is decreased, for example, to save space, function
`next_file/1`, can be used to force the log to wrap.

# `chunk`

```elixir
-spec chunk(Log, Continuation) -> chunk_ret() when Log :: log(), Continuation :: start | continuation().
```

# `chunk`

```elixir
-spec chunk(Log, Continuation, N) -> chunk_ret()
               when Log :: log(), Continuation :: start | continuation(), N :: pos_integer() | infinity.
```

Efficiently reads the terms that are appended to an internally formatted log.

It minimizes disk I/O by reading 64 kilobyte chunks from the file.

The first time `chunk()` is called, an initial continuation, the
atom `start`, must be provided.

When [`chunk/3`](`chunk/3`) is called, `N` controls the maximum number of terms
that are read from the log in each chunk. `infinity` means
that all the terms contained in the 64 kilobyte chunk are read. If less than `N`
terms are returned, this does not necessarily mean that the end of the file is
reached.

`chunk/3` returns a tuple `{Continuation2, Terms}`, where `Terms` is a list of
terms found in the log. `Continuation2` is yet another continuation, which must
be passed on to any subsequent calls to `chunk()`. With a series of calls to
`chunk()`, all terms from a log can be extracted.

`chunk/3` returns a tuple `{Continuation2, Terms, Badbytes}` if the log is
opened in read-only mode and the read chunk is corrupt. `Badbytes` is the number
of bytes in the file found not to be Erlang terms in the chunk. Notice that the
log is not repaired. When trying to read chunks from a log opened in read-write
mode, tuple `{corrupt_log_file, FileName}` is returned if the read chunk is
corrupt.

`chunk/3` returns `eof` when the end of the log is reached, or `{error, Reason}`
if an error occurs. If a wrap log file is missing, a message is output on the
error log.

When [`chunk/2,3`](`chunk/3`) is used with wrap logs, the returned continuation might not be
valid in the next call to `chunk/3`. This is because the log can wrap and delete
the file into which the continuation points. To prevent this, the log can be
blocked during the search.

# `chunk_info`

```elixir
-spec chunk_info(Continuation) -> InfoList | {error, Reason}
                    when
                        Continuation :: continuation(),
                        InfoList :: [{node, Node :: node()}, ...],
                        Reason :: {no_continuation, Continuation}.
```

Returns the pair `{node, Node}`, describing the chunk continuation returned by
`chunk/2,3`, [`bchunk/2,3`](`bchunk/3`), or [`chunk_step/3`](`chunk_step/3`).

Terms are read from the disk log running on `Node`.

# `chunk_step`

```elixir
-spec chunk_step(Log, Continuation, Step) -> {ok, any()} | {error, Reason}
                    when
                        Log :: log(),
                        Continuation :: start | continuation(),
                        Step :: integer(),
                        Reason ::
                            no_such_log | end_of_log |
                            {format_external, Log} |
                            {blocked_log, Log} |
                            {badarg, continuation} |
                            {file_error, file:filename(), file_error()}.
```

Can be used with [`chunk/2,3`](`chunk/3`) and [`bchunk/2,3`](`chunk/3`) to
search through an internally formatted wrap log.

It takes as argument a continuation as returned by [`chunk/2,3`](`chunk/3`),
[`bchunk/2,3`](`bchunk/3`), or [`chunk_step/3`](`chunk_step/3`), and steps
forward (or backward) `Step` files in the wrap log. The continuation returned,
points to the first log item in the new current file.

If atom `start` is specified as continuation, the first file of the wrap log is
chosen as the new current file.

If the wrap log is not full because all files are not yet used,
`{error, end_of_log}` is returned if trying to step outside the log.

# `close`

```elixir
-spec close(Log) -> ok | {error, close_error_rsn()} when Log :: log().
```

Closes a disk log properly.

An internally formatted log must be closed before the Erlang system is stopped.
Otherwise, the log is regarded as unclosed and the automatic repair procedure is
activated next time the log is opened.

The disk log process is not terminated as long as there are owners or users of
the log. All owners must close the log, possibly by terminating. Also, any other
process, not only the processes that have opened the log anonymously, can
decrement the `users` counter by closing the log. Attempts to close a log by a
process that is not an owner are ignored if there are no users.

If the log is blocked by the closing process, the log is also unblocked.

# `format_error`

```elixir
-spec format_error(Error) -> io_lib:chars() when Error :: term().
```

Given the error returned by any function in this module, this function returns a
descriptive string of the error in English.

For file errors, function [`format_error/1`](`format_error/1`) in module
[`file`](`file:format_error/1`) is called.

# `inc_wrap_file`

> This function is deprecated. disk_log:inc_wrap_file/1 is deprecated; use disk_log:next_file/1 instead.

```elixir
-spec inc_wrap_file(Log) -> ok | {error, inc_wrap_error_rsn()} when Log :: log().
```

Forces the internally formatted disk log to start logging to the next log file.
It can be used, for example, with [`change_size/2`](`change_size/2`) to reduce
the amount of disk space allocated by the disk log.

Owners subscribing to notifications normally receive a `wrap` message, but if an
error occurs with a reason tag of `invalid_header` or `file_error`, an
`error_status` message is sent.

# `info`

```elixir
-spec info(Log) -> InfoList | {error, no_such_log} when Log :: log(), InfoList :: [dlog_info()].
```

Returns a list of `{Tag, Value}` pairs describing a log running on the node.

The following pairs are returned for all logs:

- **`{name, Log}`** - `Log` is the log name as specified by the
  [`open/1`](`open/1`) option `name`.

- **`{file, File}`** - For halt logs `File` is the filename, and for wrap logs
  `File` is the base name.

- **`{type, Type}`** - `Type` is the log type as specified by the
  [`open/1`](`open/1`) option `type`.

- **`{format, Format}`** - `Format` is the log format as specified by the
  [`open/1`](`open/1`) option `format`.

- **`{size, Size}`** - `Size` is the log size as specified by the
  [`open/1`](`open/1`) option `size`, or the size set by
  [`change_size/2`](`change_size/2`). The value set by
  [`change_size/2`](`change_size/2`) is reflected immediately.

- **`{mode, Mode}`** - `Mode` is the log mode as specified by the
  [`open/1`](`open/1`) option `mode`.

- **`{owners, [{pid(), Notify}]}`** - `Notify` is the value set by the
  [`open/1`](`open/1`) option `notify` or function
  [`change_notify/3`](`change_notify/3`) for the owners of the log.

- **`{users, Users}`** - `Users` is the number of anonymous users of the log,
  see the [`open/1`](`open/1`) option [`linkto`](`m:disk_log#linkto`).

- **`{status, Status}`** - `Status` is `ok` or `{blocked, QueueLogRecords}` as
  set by functions `block/1,2` and [`unblock/1`](`unblock/1`).

- **`{node, Node}`** - The information returned by the current invocation of
  function [`info/1`](`info/1`) is gathered from the disk log process running on
  `Node`.

The following pairs are returned for all logs opened in `read_write` mode:

- **`{head, Head}`** - Depending on the value of the [`open/1`](`open/1`)
  options `head` and `head_func`, or set by function
  [`change_header/2`](`change_header/2`), the value of `Head` is `none`
  (default), `{head, H}` (`head` option), or `{M,F,A}` (`head_func` option).

- **`{no_written_items, NoWrittenItems}`** - `NoWrittenItems` is the number of
  items written to the log since the disk log process was created.

The following pair is returned for halt logs opened in `read_write` mode:

- **`{full, Full}`** - `Full` is `true` or `false` depending on whether the halt
  log is full or not.

The following pairs are returned for wrap logs opened in `read_write` mode:

- **`{no_current_bytes, integer() >= 0}`** - The number of bytes written to the
  current wrap log file.

- **`{no_current_items, integer() >= 0}`** - The number of items written to the
  current wrap log file, header inclusive.

- **`{no_items, integer() >= 0}`** - The total number of items in all wrap log
  files.

- **`{current_file, integer()}`** - The ordinal for the current wrap log file in
  the range `1..MaxNoFiles`, where `MaxNoFiles` is specified by the
  [`open/1`](`open/1`) option `size` or set by
  [`change_size/2`](`change_size/2`).

- **`{no_overflows, {SinceLogWasOpened, SinceLastInfo}}`** - `SinceLogWasOpened`
  (`SinceLastInfo`) is the number of times a wrap log file has been filled up
  and a new one is opened or [`inc_wrap_file/1`](`inc_wrap_file/1`) has been
  called since the disk log was last opened ([`info/1`](`info/1`) was last
  called). The first time `info/2` is called after a log was (re)opened or
  truncated, the two values are equal.

Notice that functions [`chunk/2,3`](`chunk/3`), [`bchunk/2,3`](`bchunk/3`), and
[`chunk_step/3`](`chunk_step/3`) do not affect any value returned by
[`info/1`](`info/1`).

# `log`

```elixir
-spec log(Log, Term) -> ok | {error, Reason :: log_error_rsn()} when Log :: log(), Term :: term().
```

Synchronously appends a term to a internally formatted disk log. Returns `ok`
or `{error, Reason}` when the term is written to disk.

Terms are written by the ordinary `write()` function of the operating system.
Hence, it is not guaranteed that the term is written to disk, it can linger in
the operating system kernel for a while. To ensure that the item is written to disk,
function `sync/1` must be called.

Owners subscribing to notifications are notified of an error with an
`error_status` message if the error reason tag is `invalid_header` or
`file_error`.

# `log_terms`

```elixir
-spec log_terms(Log, TermList) -> ok | {error, Reason :: log_error_rsn()}
                   when Log :: log(), TermList :: [term()].
```

Synchronously appends a list of items to an internally formatted log.

It is more efficient to use this functions instead of [`log/2`](`log/2`). The specified
list is split into as large sublists as possible (limited by the size of wrap log files),
and each sublist is logged as one single item, which reduces the overhead.

Owners subscribing to notifications are notified of an error with an
`error_status` message if the error reason tag is `invalid_header` or
`file_error`.

# `next_file`
*since OTP 26.0* 

```elixir
-spec next_file(Log) -> ok | {error, next_file_error_rsn()} when Log :: log().
```

For wrap logs, it forces the disk log to start logging to the next log file. It
can be used, for example, with [`change_size/2`](`change_size/2`) to reduce the
amount of disk space allocated by the disk log.

Owners subscribing to notifications normally receive a `wrap` message, but if an
error occurs with a reason tag of `invalid_header` or `file_error`, an
`error_status` message is sent.

For rotate logs, it forces rotation of the currently active log file, compresses
it and opens a new active file for logging.

# `open`

```elixir
-spec open(ArgL) -> open_ret() when ArgL :: dlog_options().
```

Open a new disk_log file for reading or writing.

Parameter `ArgL` is a list of the following options:

- **`{name, Log}`** - Specifies the log name. This name must be passed on as a
  parameter in all subsequent logging operations. A name must always be
  supplied.

- **`{file, FileName}`** - Specifies the name of the file to be used for logged
  terms. If this value is omitted and the log name is an atom or a string, the
  filename defaults to `lists:concat([Log, ".LOG"])` for halt logs.

  For wrap logs, this is the base name of the files. Each file in a wrap log is
  called `<FileName>.N`, where `N` is an integer. Each wrap log also has two
  files called `<FileName>.idx` and `<FileName>.siz`.

  For rotate logs, this is the name of the active log file. The compressed files
  are named as `<FileName>.N.gz`, where `N` is an integer and `<FileName>.0.gz`
  is the latest compressed log file. All the compressed files are renamed at
  each rotation so that the latest files have the smallest index. The maximum
  value for N is the value of `MaxNoFiles` minus 1.

- **`{linkto, LinkTo}`[](){: #linkto } **  
   If `LinkTo` is a pid, it becomes an owner of the log. If `LinkTo` is `none`, the
  log records that it is used anonymously by some process by incrementing the `users`
  counter. By default, the process that calls [`open/1`](`open/1`) owns the log.

- **`{repair, Repair}`** - If `Repair` is `true`, the current log file is
  repaired, if needed. As the restoration is initiated, a message is output on
  the error log. If `false` is specified, no automatic repair is attempted.
  Instead, the tuple `{error, {need_repair, Log}}` is returned if an attempt is
  made to open a corrupt log file. If `truncate` is specified, the log file
  becomes truncated, creating an empty log, regardless of previous content.
  Defaults to `true`, which has no effect on logs opened in read-only mode.

- **`{type, Type}`** - The log type. Defaults to `halt`.

- **`{format, Format}`** - Disk log format. Defaults to `internal`.

- **`{size, Size}`** - Log size.

  When a halt log has reached its maximum size, all attempts to log more items
  are rejected. Defaults to `infinity`, which for halt implies that there is no
  maximum size.

  For wrap and rotate logs, parameter `Size` can be a pair
  `{MaxNoBytes, MaxNoFiles}`. For wrap logs it can also be `infinity`. In the
  latter case, if the files of an existing wrap log with the same name can be
  found, the size is read from the existing wrap log, otherwise an error is
  returned.

  Wrap logs write at most `MaxNoBytes` bytes on each file and use `MaxNoFiles`
  files before starting all over with the first wrap log file. Regardless of
  `MaxNoBytes`, at least the header (if there is one) and one item are written
  on each wrap log file before wrapping to the next file.

  The first time an existing wrap log is opened, that is, when the disk log
  process is created, the value of the option `size` is allowed to differ from
  the current log size, and the size of the disk log is changed as per
  `change_size/2`.

  When opening an existing wrap log, it is not necessary to supply a value for
  option `size`, but if the log is already open, that is, the disk log process
  exists, the supplied value must equal the current log size, otherwise the
  tuple `{error, {size_mismatch, CurrentSize, NewSize}}` is returned.

  > #### Note {: .info }
  >
  > Before Erlang/OTP 24.0, the supplied value of option `size` was to be equal
  > to the current log size when opening an existing wrap log for the first
  > time, that is, when creating the disk log process.

  Rotate logs write at most `MaxNoBytes` bytes on the active log file and keep
  the latest `MaxNoFiles` compressed files. Regardless of `MaxNoBytes`, at least
  the header (if there is one) and one item are written on each rotate log file
  before rotation.

  When opening an already open halt log, option `size` is ignored.

- **`{notify, boolean()}`**{: #notify } - If `true`, the log owners are notified
  when certain log events occur. Defaults to `false`. The owners are sent one of the
  following messages when an event occurs:

  - **`{disk_log, Node, Log, {wrap, NoLostItems}}`** - Sent when a wrap log has
    filled up one of its files and a new file is opened. `NoLostItems` is the
    number of previously logged items that were lost when truncating existing
    files.

  - **`{disk_log, Node, Log, {truncated, NoLostItems}}`** - Sent when a log is
    truncated or reopened. For halt logs `NoLostItems` is the number of items
    written on the log since the disk log process was created. For wrap logs
    `NoLostItems` is the number of items on all wrap log files.

  - **`{disk_log, Node, Log, {read_only, Items}}`** - Sent when an asynchronous
    log attempt is made to a log file opened in read-only mode. `Items` is the
    items from the log attempt.

  - **`{disk_log, Node, Log, {blocked_log, Items}}`** - Sent when an
    asynchronous log attempt is made to a blocked log that does not queue log
    attempts. `Items` is the items from the log attempt.

  - **`{disk_log, Node, Log, {format_external, Items}}`** - Sent when function
    [`alog/2`](`alog/2`) or [`alog_terms/2`](`alog_terms/2`) is used for
    internally formatted logs. `Items` is the items from the log attempt.

  - **`{disk_log, Node, Log, full}`** - Sent when an attempt to log items to a
    wrap log would write more bytes than the limit set by option `size`.

  - **`{disk_log, Node, Log, {error_status, Status}}`** - Sent when the error
    status changes. The error status is defined by the outcome of the last
    attempt to log items to the log, or to truncate the log, or the last use of
    function [`sync/1`](`sync/1`), [`inc_wrap_file/1`](`inc_wrap_file/1`), or
    [`change_size/2`](`change_size/2`). `Status` is either `ok` or
    `{error, Error}`, the former is the initial value.

- **`{head, Head}`** - Specifies a header to be written first on the log file.
  If the log is a wrap or rotate log, the item `Head` is written first in each
  new file. `Head` is to be a term if the format is `internal`, otherwise an
  `t:iodata/0`. Defaults to `none`, which means that no header is written first
  on the file.

- **`{head_func, {M,F,A}}`** - Specifies a function to be called each time a new
  log file is opened. The call `M:F(A)` is assumed to return `{ok, Head}`. The
  item `Head` is written first in each file. `Head` is to be a term if the
  format is `internal`, otherwise an `t:iodata/0`.

- **`{mode, Mode}`** - Specifies if the log is to be opened in read-only or
  read-write mode. Defaults to `read_write`.

- **`{quiet, Boolean}`** - Specifies if messages will be sent to `error_logger`
  on recoverable errors with the log files. Defaults to `false`.

[`open/1`](`open/1`) returns `{ok, Log}` if the log file is successfully opened.
If the file is successfully repaired, the tuple
`{repaired, Log, {recovered, Rec}, {badbytes, Bad}}` is returned, where `Rec` is
the number of whole Erlang terms found in the file and `Bad` is the number of
bytes in the file that are non-Erlang terms.

When a disk log is opened in read-write mode, any existing log file is checked
for. If there is none, a new empty log is created, otherwise the existing file
is opened at the position after the last logged item, and the logging of items
starts from there. If the format is `internal` and the existing file is not
recognized as an internally formatted log, a tuple
`{error, {not_a_log_file, FileName}}` is returned.

[`open/1`](`open/1`) cannot be used for changing the values of options of an
open log. When there are prior owners or users of a log, all option values
except `name`, `linkto`, and `notify` are only checked against the values
supplied before as option values to function [`open/1`](`open/1`),
[`change_header/2`](`change_header/2`), [`change_notify/3`](`change_notify/3`),
or [`change_size/2`](`change_size/2`). Thus, none of the options except `name`
is mandatory. If some specified value differs from the current value, a tuple
`{error, {arg_mismatch, OptionName, CurrentValue, Value}}` is returned.

> #### Note {: .info }
>
> If an owner attempts to open a log as owner once again, it is acknowledged
> with the return value `{ok, Log}`, but the state of the disk log is not
> affected.

A log file can be opened more than once by giving different values to option
`name` or by using the same file when opening a log on different nodes. It is up
to the user of module `disk_log` to ensure that not more than one disk log
process has write access to any file, otherwise the file can be corrupted.

If an attempt to open a log file for the first time fails, the disk log process
terminates with the EXIT message `{{failed,Reason},[{disk_log,open,1}]}`. The
function returns `{error, Reason}` for all other errors.

# `pid2name`

```elixir
-spec pid2name(Pid) -> {ok, Log} | undefined when Pid :: pid(), Log :: log().
```

Returns the log name given the pid of a disk log process on the current node, or
`undefined` if the specified pid is not a disk log process.

This function is meant to be used for debugging only.

# `reopen`

```elixir
-spec reopen(Log, File) -> ok | {error, reopen_error_rsn()} when Log :: log(), File :: file:filename().
```

Equivalent to [`reopen(Log, File, Head)`](`reopen/3`) where `Head` is
the `Head` specified in `open/1`.

# `reopen`

```elixir
-spec reopen(Log, File, Head) -> ok | {error, reopen_error_rsn()}
                when Log :: log(), File :: file:filename(), Head :: term().
```

Renames an internally formatted log file to `File` and then recreates a new log file. If a
wrap/rotate log exists, `File` is used as the base name of the renamed files.

Writes the value of `Head` first in the newly opened log file. The header argument
is used only once. Next time a wrap/rotate log file is opened, the header given to
[`open/1`](`open/1`) is used.

Owners subscribing to notifications receive a `truncate` message.

Upon failure to reopen the log, the disk log process terminates with the EXIT
message `{{failed,Error},[{disk_log,Fun,Arity}]}`. Other processes having
requests queued receive the message
`{disk_log, Node, {error, disk_log_stopped}}`.

# `sync`

```elixir
-spec sync(Log) -> ok | {error, sync_error_rsn()} when Log :: log().
```

Ensures that the contents of the log are written to the disk. This is usually a
rather expensive operation.

# `truncate`

```elixir
-spec truncate(Log) -> ok | {error, trunc_error_rsn()} when Log :: log().
```

Equivalent to [`truncate(Log, Head)`](`truncate/2`) where `Head` is
the `Head` specified in `open/1`.

This function can be used for both internally and externally
formatted logs.

# `truncate`

```elixir
-spec truncate(Log, Head) -> ok | {error, trunc_error_rsn()} when Log :: log(), Head :: term().
```

Removes all items from an internally formatted disk log. The argument `Head` or
is written first in the newly truncated log.

The header argument is used only once. Next time a wrap/rotate log file is opened,
the header given to [`open/1`](`open/1`) is used.

Owners subscribing to notifications receive a `truncate` message.

If the attempt to truncate the log fails, the disk log process terminates with
the EXIT message `{{failed,Reason},[{disk_log,Fun,Arity}]}`. Other processes
having requests queued receive the message
`{disk_log, Node, {error, disk_log_stopped}}`.

# `unblock`

```elixir
-spec unblock(Log) -> ok | {error, unblock_error_rsn()} when Log :: log().
```

Unblocks a log. A log can only be unblocked by the blocking process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
