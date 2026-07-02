# `wrap_log_reader`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/wrap_log_reader.erl#L25)

A service to read internally formatted wrap disk logs.

This module makes it possible to read internally formatted wrap disk logs, see
`m:disk_log`. `m:wrap_log_reader` does not interfere with `m:disk_log` activities;
there is however a bug in this version of the `m:wrap_log_reader`, see section
[Known Limitations](`m:wrap_log_reader#module-known-limitations`).

A wrap disk log file consists of many files, called index files. A log file can
be opened and closed. Also, a single index file can be opened separately. If a
non-existent or non-internally formatted file is opened, an error message is
returned. If the file is corrupt, no attempt is made to repair it, but an error
message is returned.

If a log is configured to be distributed, it is possible that all items are not
logged on all nodes. `m:wrap_log_reader` only reads the log on the called node; it
is up to the user to be sure that all items are read.

## Known Limitations

This version of `m:wrap_log_reader` does not detect if `m:disk_log` wraps to a new
index file between a call to `wrap_log_reader:open/1` and the first call to
`wrap_log_reader:chunk/1`. If this occurs, the call to `chunk/1` reads the last
logged items in the log file, as the opened index file was truncated by
`m:disk_log`.

# `chunk_ret`
*not exported* 

```erlang
-type chunk_ret() ::
          {Continuation2 :: term(), Terms :: [term()]} |
          {Continuation2 :: term(), Terms :: [term()], Badbytes :: non_neg_integer()} |
          {Continuation2 :: term(), eof} |
          {error, Reason :: term()}.
```

# `continuation`

```erlang
-opaque continuation()
```

Continuation returned by `open/1,2` or `chunk/1,2`.

# `open_ret`
*not exported* 

```erlang
-type open_ret() :: {ok, Continuation :: continuation()} | {error, Reason :: tuple()}.
```

# `chunk`

```erlang
-spec chunk(Continuation) -> chunk_ret() when Continuation :: continuation().
```

# `chunk`

```erlang
-spec chunk(Continuation, N) -> chunk_ret()
               when Continuation :: continuation(), N :: infinity | pos_integer().
```

Enables to efficiently read the terms that are appended to a log. Minimises disk
I/O by reading 64 kilobyte chunks from the file.

The first time `chunk/2` is called, an initial continuation returned from
[`open/1`](`open/1`) or [`open/2`](`open/2`) must be provided.

When `chunk/2` is called, `N` controls the maximum number of terms that are read
from the log in each chunk. `infinity` means that all the
terms contained in the 8K chunk are read. If less than `N` terms are returned,
this does not necessarily mean that end of file is reached.

Returns a tuple `{Continuation2, Terms}`, where `Terms` is a list of terms found
in the log. `Continuation2` is yet another continuation that must be passed on
to any subsequent calls to `chunk()`. With a series of calls to `chunk()`, it is
then possible to extract all terms from a log.

Returns a tuple `{Continuation2, Terms, Badbytes}` if the log is opened in read
only mode and the read chunk is corrupt. `Badbytes` indicates the number of
non-Erlang terms found in the chunk. Notice that the log is not repaired.

Returns `{Continuation2, eof}` when the end of the log is reached, and
`{error, Reason}` if an error occurs.

The returned continuation either is or is not valid in the next call to this
function. This is because the log can wrap and delete the file into which the
continuation points. To ensure this does not occur, the log can be blocked
during the search.

# `close`

```erlang
-spec close(Continuation) -> ok | {error, Reason}
               when Continuation :: continuation(), Reason :: file:posix().
```

Closes a log file properly.

# `open`

```erlang
-spec open(Filename) -> open_ret() when Filename :: string() | atom().
```

Equivalent to [`open(Filename, ...)`](`open/2`) except that the whole
wrap log file is read.

# `open`

```erlang
-spec open(Filename, N) -> open_ret() when Filename :: string() | atom(), N :: integer().
```

`Filename` specifies the name of the file to be read.

`N` specifies the index of the file to be read. Use `open/1` to read the entire
wrap log.

Returns `{ok, Continuation}` if the log/index file is opened successfully.
`Continuation` is to be used when chunking or closing the file.

Returns `{error, Reason}` for all errors.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
