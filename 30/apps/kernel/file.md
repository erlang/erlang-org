# `file`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/file.erl#L22)

File interface module.

This module provides an interface to the file system.

> #### Warning {: .warning }
>
> File operations are only guaranteed to appear atomic when going through the
> same file server. A NIF or other OS process may observe intermediate steps on
> certain operations on some operating systems, eg. renaming an existing file on
> Windows, or [`write_file_info/2`](`write_file_info/2`) on any OS at the time
> of writing.

Regarding filename encoding, the Erlang VM can operate in two modes. The current
mode can be queried using function `native_name_encoding/0`. It returns `latin1`
or `utf8`.

In `latin1` mode, the Erlang VM does not change the encoding of filenames. In
`utf8` mode, filenames can contain Unicode characters greater than 255 and the
VM converts filenames back and forth to the native filename encoding (usually
UTF-8, but UTF-16 on Windows).

The default mode depends on the operating system. Windows, MacOS X and Android
enforce consistent filename encoding and therefore the VM uses `utf8` mode.

On operating systems with transparent naming (for example, all Unix systems
except MacOS X), default is `utf8` if the terminal supports UTF-8, otherwise
`latin1`. The default can be overridden using `+fnl` (to force `latin1` mode) or
`+fnu` (to force `utf8` mode) when starting [`erl`](`e:erts:erl_cmd.md`).

On operating systems with transparent naming, files can be inconsistently named,
for example, some files are encoded in UTF-8 while others are encoded in ISO
Latin-1. The concept of _raw filenames_ is introduced to handle file systems
with inconsistent naming when running in `utf8` mode.

A _raw filename_ is a filename specified as a binary. The Erlang VM does not
translate a filename specified as a binary on systems with transparent naming.

When running in `utf8` mode, functions `list_dir/1` and `read_link/1` never
return raw filenames. To return all filenames including raw filenames, use
functions `list_dir_all/1` and `read_link_all/1`.

See also section
[Notes About Raw Filenames](`e:stdlib:unicode_usage.md#notes-about-raw-filenames`)
in the STDLIB User's Guide.

> #### Note {: .info }
>
> File operations used to accept filenames containing null characters (integer
> value zero). This caused the name to be truncated and in some cases arguments
> to primitive operations to be mixed up. Filenames containing null characters
> inside the filename are now _rejected_ and will cause primitive file
> operations fail.

## POSIX Error Codes

- `eacces` \- Permission denied
- `eagain` \- Resource temporarily unavailable
- `ebadf` \- Bad file number
- `ebusy` \- File busy
- `edquot` \- Disk quota exceeded
- `eexist` \- File already exists
- `efault` \- Bad address in system call argument
- `efbig` \- File too large
- `eintr` \- Interrupted system call
- `einval` \- Invalid argument
- `eio` \- I/O error
- `eisdir` \- Illegal operation on a directory
- `eloop` \- Too many levels of symbolic links
- `emfile` \- Too many open files
- `emlink` \- Too many links
- `enametoolong` \- Filename too long
- `enfile` \- File table overflow
- `enodev` \- No such device
- `enoent` \- No such file or directory
- `enomem` \- Not enough memory
- `enospc` \- No space left on device
- `enotblk` \- Block device required
- `enotdir` \- Not a directory
- `enotsup` \- Operation not supported
- `enxio` \- No such device or address
- `eperm` \- Not owner
- `epipe` \- Broken pipe
- `erofs` \- Read-only file system
- `espipe` \- Invalid seek
- `esrch` \- No such process
- `estale` \- Stale remote file handle
- `exdev` \- Cross-device link

## Performance

For increased performance, raw files are recommended.

A normal file is really a process so it can be used as an I/O device (see
`m:io`). Therefore, when data is written to a normal file, the sending of the
data to the file process, copies all data that are not binaries. Opening the
file in binary mode and writing binaries is therefore recommended. If the file
is opened on another node, or if the file server runs as slave to the file
server of another node, also binaries are copied.

> #### Note {: .info }
>
> Raw files use the file system of the host machine of the node. For normal
> files (non-raw), the file server is used to find the files, and if the node is
> running its file server as slave to the file server of another node, and the
> other node runs on some other host machine, they can have different file
> systems. However, this is seldom a problem.

`open/2` can be given the options `delayed_write` and `read_ahead` to turn on
caching, which will reduce the number of operating system calls and greatly
improve performance for small reads and writes. However, the overhead won't
disappear completely and it's best to keep the number of file operations to a
minimum. As a contrived example, the following function writes 4MB in 2.5
seconds when tested:

```erlang
create_file_slow(Name) ->
    {ok, Fd} = file:open(Name, [raw, write, delayed_write, binary]),
    create_file_slow_1(Fd, 4 bsl 20),
    file:close(Fd).

create_file_slow_1(_Fd, 0) ->
    ok;
create_file_slow_1(Fd, M) ->
    ok = file:write(Fd, <<0>>),
    create_file_slow_1(Fd, M - 1).
```

The following functionally equivalent code writes 128 bytes per call to
`write/2` and so does the same work in 0.08 seconds, which is roughly 30 times
faster:

```erlang
create_file(Name) ->
    {ok, Fd} = file:open(Name, [raw, write, delayed_write, binary]),
    create_file_1(Fd, 4 bsl 20),
    file:close(Fd),
    ok.

create_file_1(_Fd, 0) ->
    ok;
create_file_1(Fd, M) when M >= 128 ->
    ok = file:write(Fd, <<0:(128)/unit:8>>),
    create_file_1(Fd, M - 128);
create_file_1(Fd, M) ->
    ok = file:write(Fd, <<0:(M)/unit:8>>),
    create_file_1(Fd, M - 1).
```

When writing data it's generally more efficient to write a list of binaries
rather than a list of integers. It is not needed to flatten a deep list before
writing. On Unix hosts, scatter output, which writes a set of buffers in one
operation, is used when possible. In this way
[`write(FD, [Bin1, Bin2 | Bin3])`](`write/2`) writes the contents of the
binaries without copying the data at all, except for perhaps deep down in the
operating system kernel.

> #### Warning {: .warning }
>
> If an error occurs when accessing an open file with module `m:io`, the process
> handling the file exits. The dead file process can hang if a process tries to
> access it later. This will be fixed in a future release.

### See Also

`m:filename`

# `date_time`

```erlang
-type date_time() :: calendar:datetime().
```

Must denote a valid date and time.

# `deep_list`
*not exported* 

```erlang
-type deep_list() :: [char() | atom() | deep_list()].
```

# `delete_option`
*not exported* 

```erlang
-type delete_option() :: raw.
```

# `fd`

```erlang
-type fd() :: file_descriptor().
```

A file descriptor representing a file opened in [`raw`](`m:file#raw`) mode.

# `file_info`

```erlang
-type file_info() ::
          #file_info{size :: non_neg_integer() | undefined,
                     type :: device | directory | other | regular | symlink | undefined,
                     access :: read | write | read_write | none | undefined,
                     atime :: file:date_time() | non_neg_integer() | undefined,
                     mtime :: file:date_time() | non_neg_integer() | undefined,
                     ctime :: file:date_time() | non_neg_integer() | undefined,
                     mode :: non_neg_integer() | undefined,
                     links :: non_neg_integer() | undefined,
                     major_device :: non_neg_integer() | undefined,
                     minor_device :: non_neg_integer() | undefined,
                     inode :: non_neg_integer() | undefined,
                     uid :: non_neg_integer() | undefined,
                     gid :: non_neg_integer() | undefined}.
```

# `file_info_option`
*not exported* 

```erlang
-type file_info_option() :: {time, local} | {time, universal} | {time, posix} | raw.
```

# `filename`

```erlang
-type filename() :: string().
```

A file name as returned from `m:file` API functions.

See the documentation of the `t:name_all/0` type.

# `filename_all`

```erlang
-type filename_all() :: string() | (RawFilename :: binary()).
```

A file name as returned from `m:file` API functions.

See the documentation of the `t:name_all/0` type.

# `io_device`

```erlang
-type io_device() :: io_server() | fd().
```

An IO device as returned by `open/2`.

`t:io_server/0` is returned by default and `t:fd/0` is returned if the `raw` option is given.

# `io_server`

```erlang
-type io_server() :: pid().
```

A process handling the I/O protocol.

# `location`

```erlang
-type location() ::
          integer() |
          {bof, Offset :: integer()} |
          {cur, Offset :: integer()} |
          {eof, Offset :: integer()} |
          bof | cur | eof.
```

# `mode`

```erlang
-type mode() ::
          read | write | append | exclusive | raw | binary |
          {delayed_write, Size :: non_neg_integer(), Delay :: non_neg_integer()} |
          delayed_write |
          {read_ahead, Size :: pos_integer()} |
          read_ahead | compressed | compressed_one |
          {zstd, zstd:compress_parameters() | zstd:decompress_parameters()} |
          {encoding, unicode:encoding()} |
          sync.
```

# `name`

```erlang
-type name() :: string() | atom() | deep_list().
```

A restricted file name used as input into `m:file` API functions.

If VM is in Unicode filename mode, `t:string/0` and `t:char/0` are allowed to
be > 255. See also the documentation of the `t:name_all/0` type.

# `name_all`

```erlang
-type name_all() :: string() | atom() | deep_list() | (RawFilename :: binary()).
```

A file name used as input into `m:file` API functions.

If VM is in Unicode filename mode, characters are allowed to be > 255.
`RawFilename` is a filename not subject to Unicode translation, meaning that it
can contain characters not conforming to the Unicode encoding expected from the
file system (that is, non-UTF-8 characters although the VM is started in Unicode
filename mode). Null characters (integer value zero) are _not_ allowed in
filenames (not even at the end).

# `posix`

```erlang
-type posix() ::
          eacces | eagain | ebadf | ebadmsg | ebusy | edeadlk | edeadlock | edquot | eexist | efault |
          efbig | eftype | eintr | einval | eio | eisdir | eloop | emfile | emlink | emultihop |
          enametoolong | enfile | enobufs | enodev | enolck | enolink | enoent | enomem | enospc |
          enosr | enostr | enosys | enotblk | enotdir | enotsup | enxio | eopnotsupp | eoverflow |
          eperm | epipe | erange | erofs | espipe | esrch | estale | etxtbsy | exdev.
```

An atom that is named from the POSIX error codes used in Unix, and in the
runtime libraries of most C compilers.

# `posix_file_advise`
*not exported* 

```erlang
-type posix_file_advise() :: normal | sequential | random | no_reuse | will_need | dont_need.
```

# `read_file_option`
*not exported* 

```erlang
-type read_file_option() :: raw.
```

# `sendfile_option`
*not exported* 

```erlang
-type sendfile_option() :: {chunk_size, non_neg_integer()} | {use_threads, boolean()}.
```

# `advise`
*since OTP R14B* 

```erlang
-spec advise(IoDevice, Offset, Length, Advise) -> ok | {error, Reason}
                when
                    IoDevice :: io_device(),
                    Offset :: integer(),
                    Length :: integer(),
                    Advise :: posix_file_advise(),
                    Reason :: posix() | badarg.
```

[`advise/4`](`advise/4`) can be used to announce an intention to access file
data in a specific pattern in the future, thus allowing the operating system to
perform appropriate optimizations.

On some platforms, this function might have no effect.

# `allocate`
*since OTP R16B* 

```erlang
-spec allocate(File, Offset, Length) -> ok | {error, posix()}
                  when File :: io_device(), Offset :: non_neg_integer(), Length :: non_neg_integer().
```

[`allocate/3`](`allocate/3`) can be used to preallocate space for a file.

This function only succeeds in platforms that provide this feature.

# `change_group`

```erlang
-spec change_group(Filename, Gid) -> ok | {error, Reason}
                      when Filename :: name_all(), Gid :: integer(), Reason :: posix() | badarg.
```

Changes group of a file. See `write_file_info/2`.

# `change_mode`
*since OTP R14B* 

```erlang
-spec change_mode(Filename, Mode) -> ok | {error, Reason}
                     when Filename :: name_all(), Mode :: integer(), Reason :: posix() | badarg.
```

Changes permissions of a file. See `write_file_info/2`.

# `change_owner`

```erlang
-spec change_owner(Filename, Uid) -> ok | {error, Reason}
                      when Filename :: name_all(), Uid :: integer(), Reason :: posix() | badarg.
```

Changes owner of a file. See `write_file_info/2`.

# `change_owner`

```erlang
-spec change_owner(Filename, Uid, Gid) -> ok | {error, Reason}
                      when
                          Filename :: name_all(),
                          Uid :: integer(),
                          Gid :: integer(),
                          Reason :: posix() | badarg.
```

Changes owner and group of a file. See `write_file_info/2`.

# `change_time`

```erlang
-spec change_time(Filename, Mtime) -> ok | {error, Reason}
                     when Filename :: name_all(), Mtime :: date_time(), Reason :: posix() | badarg.
```

Changes the modification and access times of a file. See `write_file_info/2`.

# `change_time`

```erlang
-spec change_time(Filename, Atime, Mtime) -> ok | {error, Reason}
                     when
                         Filename :: name_all(),
                         Atime :: date_time(),
                         Mtime :: date_time(),
                         Reason :: posix() | badarg.
```

Changes the modification and last access times of a file. See
`write_file_info/2`.

# `close`

```erlang
-spec close(IoDevice) -> ok | {error, Reason}
               when IoDevice :: io_device(), Reason :: posix() | badarg | terminated.
```

Closes the file referenced by `IoDevice`. It mostly returns `ok`, except for
some severe errors such as out of memory.

Notice that if option `delayed_write` was used when opening the file,
[`close/1`](`close/1`) can return an old write error and not even try to close
the file. See `open/2`.

# `consult`

```erlang
-spec consult(Filename) -> {ok, Terms} | {error, Reason}
                 when
                     Filename :: name_all(),
                     Terms :: [term()],
                     Reason ::
                         posix() |
                         badarg | terminated | system_limit |
                         {Line :: integer(), Mod :: module(), Term :: term()}.
```

Reads Erlang terms, separated by `.`, from `Filename`. Returns one of the
following:

- **`{ok, Terms}`** - The file was successfully read.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang terms in the file. To convert the three-element tuple to an English
  description of the error, use `format_error/1`.

_Example:_

```text
f.txt:  {person, "kalle", 25}.
        {person, "pelle", 30}.
```

```erlang
1> file:consult("f.txt").
{ok,[{person,"kalle",25},{person,"pelle",30}]}
```

The encoding of `Filename` can be set by a comment, as described in
[`epp`](`m:epp#encoding`).

# `copy`

```erlang
-spec copy(Source, Destination) -> {ok, BytesCopied} | {error, Reason}
              when
                  Source :: io_device() | Filename | {Filename, Modes},
                  Destination :: io_device() | Filename | {Filename, Modes},
                  Filename :: name_all(),
                  Modes :: [mode()],
                  BytesCopied :: non_neg_integer(),
                  Reason :: posix() | badarg | terminated.
```

# `copy`

```erlang
-spec copy(Source, Destination, ByteCount) -> {ok, BytesCopied} | {error, Reason}
              when
                  Source :: io_device() | Filename | {Filename, Modes},
                  Destination :: io_device() | Filename | {Filename, Modes},
                  Filename :: name_all(),
                  Modes :: [mode()],
                  ByteCount :: non_neg_integer() | infinity,
                  BytesCopied :: non_neg_integer(),
                  Reason :: posix() | badarg | terminated.
```

Copies `ByteCount` bytes from `Source` to `Destination`. `Source` and
`Destination` refer to either filenames or IO devices from, for example,
[`open/2`](`open/2`).

Argument `Modes` is a list of possible modes, see `open/2`, and defaults to
`[]`.

If both `Source` and `Destination` refer to filenames, the files are opened with
`[read, binary]` and `[write, binary]` prepended to their mode lists,
respectively, to optimize the copy.

If `Source` refers to a filename, it is opened with `read` mode prepended to the
mode list before the copy, and closed when done.

If `Destination` refers to a filename, it is opened with `write` mode prepended
to the mode list before the copy, and closed when done.

Returns `{ok, BytesCopied}`, where `BytesCopied` is the number of bytes that was
copied, which can be less than `ByteCount` if end of file was encountered on the
source. If the operation fails, `{error, Reason}` is returned.

Typical error reasons: as for `open/2` if a file had to be opened, and as for
`read/2` and `write/2`.

# `datasync`
*since OTP R14B* 

```erlang
-spec datasync(IoDevice) -> ok | {error, Reason}
                  when IoDevice :: io_device(), Reason :: posix() | badarg | terminated.
```

Ensures that any buffers kept by the operating system (not by the Erlang runtime
system) are written to disk. In many ways it resembles `fsync` but it does not
update some of the metadata of the file, such as the access time. On some
platforms this function has no effect.

Applications that access databases or log files often write a tiny data fragment
(for example, one line in a log file) and then call `fsync()` immediately to
ensure that the written data is physically stored on the hard disk.
Unfortunately, `fsync()` always initiates two write operations: one for the
newly written data and another one to update the modification time stored in the
`inode`. If the modification time is not a part of the transaction concept,
`fdatasync()` can be used to avoid unnecessary `inode` disk write operations.

Available only in some POSIX systems, this call results in a call to `fsync()`,
or has no effect in systems not providing the `fdatasync()` syscall.

# `del_dir`

```erlang
-spec del_dir(Dir) -> ok | {error, Reason} when Dir :: name_all(), Reason :: posix() | badarg.
```

Tries to delete directory `Dir`. The directory must be empty before it can be
deleted. Returns `ok` if successful.

Typical error reasons:

- **`eacces`** - Missing search or write permissions for the parent directories
  of `Dir`.

- **`eexist`** - The directory is not empty.

- **`enoent`** - The directory does not exist.

- **`enotdir`** - A component of `Dir` is not a directory. On some platforms,
  `enoent` is returned instead.

- **`einval`** - Attempt to delete the current directory. On some platforms,
  `eacces` is returned instead.

# `del_dir_r`
*since OTP 23.0* 

```erlang
-spec del_dir_r(File) -> ok | {error, Reason} when File :: name_all(), Reason :: posix() | badarg.
```

Deletes file or directory `File`. If `File` is a directory, its contents is
first recursively deleted. Returns:

- **`ok`** - The operation completed without errors.

- **`{error, posix()}`** - An error occurred when accessing or deleting `File`.
  If some file or directory under `File` could not be deleted, `File` cannot be
  deleted as it is non-empty, and `{error, eexist}` is returned.

# `delete`

```erlang
-spec delete(Filename) -> ok | {error, Reason} when Filename :: name_all(), Reason :: posix() | badarg.
```

# `delete`
*since OTP 24.0* 

```erlang
-spec delete(Filename, Opts) -> ok | {error, Reason}
                when Filename :: name_all(), Opts :: [delete_option()], Reason :: posix() | badarg.
```

Tries to delete file `Filename`. Returns `ok` if successful.

If the option `raw` is set, the file server is not called. This can be useful in
particular during the early boot stage when the file server is not yet
registered, to still be able to delete local files.

Typical error reasons:

- **`enoent`** - The file does not exist.

- **`eacces`** - Missing permission for the file or one of its parents.

- **`eperm`** - The file is a directory and the user is not superuser.

- **`enotdir`** - A component of the filename is not a directory. On some
  platforms, `enoent` is returned instead.

- **`einval`** - `Filename` has an improper type, such as tuple.

> #### Warning {: .warning }
>
> In a future release, a bad type for argument `Filename` will probably generate
> an exception.

# `eval`

```erlang
-spec eval(Filename) -> ok | {error, Reason}
              when
                  Filename :: name_all(),
                  Reason ::
                      posix() |
                      badarg | terminated | system_limit |
                      {Line :: integer(), Mod :: module(), Term :: term()}.
```

Reads and evaluates Erlang expressions, separated by `.` (or `,`, a sequence of
expressions is also an expression) from `Filename`. The result of the evaluation
is not returned; any expression sequence in the file must be there for its side
effect.

Returns one of the following:

- **`ok`** - The file was read and evaluated.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang expressions in the file. To convert the three-element tuple to an
  English description of the error, use `format_error/1`.

The encoding of `Filename` can be set by a comment, as described in
[`epp`](`m:epp#encoding`).

# `eval`

```erlang
-spec eval(Filename, Bindings) -> ok | {error, Reason}
              when
                  Filename :: name_all(),
                  Bindings :: erl_eval:binding_struct(),
                  Reason ::
                      posix() |
                      badarg | terminated | system_limit |
                      {Line :: integer(), Mod :: module(), Term :: term()}.
```

The same as [`eval/1`](`eval/1`), but the variable bindings `Bindings` are used
in the evaluation. For information about the variable bindings, see
`m:erl_eval`.

# `format_error`

```erlang
-spec format_error(Reason) -> Chars
                      when
                          Reason ::
                              posix() |
                              badarg | terminated | system_limit |
                              {Line :: integer(), Mod :: module(), Term :: term()},
                          Chars :: string().
```

Given the error reason returned by any function in this module, returns a
descriptive string of the error in English.

# `get_cwd`

```erlang
-spec get_cwd() -> {ok, Dir} | {error, Reason} when Dir :: filename(), Reason :: posix().
```

Returns `{ok, Dir}`, where `Dir` is the current working directory of the file
server.

> #### Note {: .info }
>
> In rare circumstances, this function can fail on Unix. It can occur if read
> permission does not exist for the parent directories of the current directory.

A typical error reason:

- **`eacces`** - Missing read permission for one of the parents of the current
  directory.

# `get_cwd`

```erlang
-spec get_cwd(Drive) -> {ok, Dir} | {error, Reason}
                 when Drive :: string(), Dir :: filename(), Reason :: posix() | badarg.
```

Returns `{ok, Dir}` or `{error, Reason}`, where `Dir` is the current working
directory of the specified drive.

`Drive` is to be of the form `Letter:`, for example, `c:`.

Returns `{error, enotsup}` on platforms that have no concept of current drive
(Unix, for example).

Typical error reasons:

- **`enotsup`** - The operating system has no concept of drives.

- **`eacces`** - The drive does not exist.

- **`einval`** - The format of `Drive` is invalid.

# `list_dir`

```erlang
-spec list_dir(Dir) -> {ok, Filenames} | {error, Reason}
                  when
                      Dir :: name_all(),
                      Filenames :: [filename()],
                      Reason :: posix() | badarg | {no_translation, Filename :: unicode:latin1_binary()}.
```

Lists all files in a directory, _except_ files with raw filenames. Returns
`{ok, Filenames}` if successful, otherwise `{error, Reason}`. `Filenames` is a
list of the names of all the files in the directory. The names are not sorted.

Typical error reasons:

- **`eacces`** - Missing search or write permissions for `Dir` or one of its
  parent directories.

- **`enoent`** - The directory does not exist.

- **`{no_translation, Filename}`** - `Filename` is a `t:binary/0` with
  characters coded in ISO Latin-1 and the VM was started with parameter `+fnue`.

# `list_dir_all`
*since OTP R16B* 

```erlang
-spec list_dir_all(Dir) -> {ok, Filenames} | {error, Reason}
                      when Dir :: name_all(), Filenames :: [filename_all()], Reason :: posix() | badarg.
```

Lists all the files in a directory, including files with
raw filenames. Returns `{ok, Filenames}` if successful, otherwise
`{error, Reason}`. `Filenames` is a list of the names of all the files in the
directory. The names are not sorted.

Typical error reasons:

- **`eacces`** - Missing search or write permissions for `Dir` or one of its
  parent directories.

- **`enoent`** - The directory does not exist.

# `make_dir`

```erlang
-spec make_dir(Dir) -> ok | {error, Reason} when Dir :: name_all(), Reason :: posix() | badarg.
```

Tries to create directory `Dir`. Missing parent directories are _not_ created.
Returns `ok` if successful.

Typical error reasons:

- **`eacces`** - Missing search or write permissions for the parent directories
  of `Dir`.

- **`eexist`** - A file or directory named `Dir` exists already.

- **`enoent`** - A component of `Dir` does not exist.

- **`enospc`** - No space is left on the device.

- **`enotdir`** - A component of `Dir` is not a directory. On some platforms,
  `enoent` is returned instead.

# `make_link`

```erlang
-spec make_link(Existing, New) -> ok | {error, Reason}
                   when Existing :: name_all(), New :: name_all(), Reason :: posix() | badarg.
```

Makes a hard link from `Existing` to `New` on platforms supporting links (Unix
and Windows). This function returns `ok` if the link was successfully created,
otherwise `{error, Reason}`. On platforms not supporting links,
`{error,enotsup}` is returned.

Typical error reasons:

- **`eacces`** - Missing read or write permissions for the parent directories of
  `Existing` or `New`.

- **`eexist`** - `New` already exists.

- **`enotsup`** - Hard links are not supported on this platform.

# `make_symlink`

```erlang
-spec make_symlink(Existing, New) -> ok | {error, Reason}
                      when Existing :: name_all(), New :: name_all(), Reason :: posix() | badarg.
```

Creates a symbolic link `New` to the file or directory `Existing` on platforms
supporting symbolic links (most Unix systems and Windows, beginning with Vista).
`Existing` does not need to exist. Returns `ok` if the link is successfully
created, otherwise `{error, Reason}`. On platforms not supporting symbolic
links, `{error, enotsup}` is returned.

Typical error reasons:

- **`eacces`** - Missing read or write permissions for the parent directories of
  `Existing` or `New`.

- **`eexist`** - `New` already exists.

- **`enotsup`** - Symbolic links are not supported on this platform.

- **`eperm`** - User does not have privileges to create symbolic links
  (`SeCreateSymbolicLinkPrivilege` on Windows).

# `native_name_encoding`
*since OTP R14B01* 

```erlang
-spec native_name_encoding() -> latin1 | utf8.
```

Returns the filename encoding mode. If it is `latin1`, the system translates no
filenames. If it is `utf8`, filenames are converted back and forth to the native
filename encoding (usually UTF-8, but UTF-16 on Windows).

# `open`

```erlang
-spec open(File, Modes) -> {ok, IoDevice} | {error, Reason}
              when
                  File :: Filename | iodata(),
                  Filename :: name_all(),
                  Modes :: [mode() | ram | directory],
                  IoDevice :: io_device(),
                  Reason :: posix() | badarg | system_limit.
```

Opens file `File` in the mode determined by `Modes`, which can contain one or
more of the following options:

- **`read`** - The file, which must exist, is opened for reading.

- **`write`** - The file is opened for writing. It is created if it does not
  exist. If the file exists and `write` is not combined with `read`, the file is
  truncated.

- **`append`** - The file is opened for writing. It is created if it does not
  exist. Every write operation to a file opened with `append` takes place at the
  end of the file.

- **`exclusive`** - The file is opened for writing. It is created if it does not
  exist. If the file exists, `{error, eexist}` is returned.

  > #### Warning {: .warning }
  >
  > This option does not guarantee exclusiveness on file systems not supporting
  > `O_EXCL` properly, such as NFS. Do not depend on this option unless you know
  > that the file system supports it (in general, local file systems are safe).

- **`raw`** - [](){: #raw } Allows faster access to a file, as no Erlang process
  is needed to handle the file. However, a file opened in this way has the
  following limitations:

  - The functions in the `io` module cannot be used, as they can only talk to an
    Erlang process. Instead, use functions `read/2`, `read_line/1`, and
    `write/2`.
  - Especially if [`read_line/1`](`read_line/1`) is to be used on a `raw` file,
    it is recommended to combine this option with option `{read_ahead, Size}` as
    line-oriented I/O is inefficient without buffering.
  - Only the Erlang process that opened the file can use it.
  - A remote Erlang file server cannot be used. The computer on which the Erlang
    node is running must have access to the file system (directly or through
    NFS).

- **`binary`** - Read operations on the file return binaries rather than lists.

- **`{delayed_write, Size, Delay}`** - Data in subsequent [`write/2`](`write/2`)
  calls is buffered until at least `Size` bytes are buffered, or until the
  oldest buffered data is `Delay` milliseconds old. Then all buffered data is
  written in one operating system call. The buffered data is also flushed before
  some other file operation than [`write/2`](`write/2`) is executed.

  The purpose of this option is to increase performance by reducing the number
  of operating system calls. Thus, the [`write/2`](`write/2`) calls must be for
  sizes significantly less than `Size`, and not interspersed by too many other
  file operations.

  When this option is used, the result of [`write/2`](`write/2`) calls can
  prematurely be reported as successful, and if a write error occurs, the error
  is reported as the result of the next file operation, which is not executed.

  For example, when `delayed_write` is used, after a number of
  [`write/2`](`write/2`) calls, [`close/1`](`close/1`) can return
  `{error, enospc}`, as there is not enough space on the disc for previously
  written data. [`close/1`](`close/1`) must probably be called again, as the
  file is still open.

- **`delayed_write`** - The same as `{delayed_write, Size, Delay}` with
  reasonable default values for `Size` and `Delay` (roughly some 64 KB, 2
  seconds).

- **`{read_ahead, Size}`** - Activates read data buffering. If
  [`read/2`](`read/2`) calls are for significantly less than `Size` bytes, read
  operations to the operating system are still performed for blocks of `Size`
  bytes. The extra data is buffered and returned in subsequent
  [`read/2`](`read/2`) calls, giving a performance gain as the number of
  operating system calls is reduced.

  The `read_ahead` buffer is also highly used by function
  [`read_line/1`](`read_line/1`) in `raw` mode, therefore this option is
  recommended (for performance reasons) when accessing raw files using that
  function.

  If [`read/2`](`read/2`) calls are for sizes not significantly less than, or
  even greater than `Size` bytes, no performance gain can be expected.

- **`read_ahead`** - The same as `{read_ahead, Size}` with a reasonable default
  value for `Size` (roughly some 64 KB).

- **`compressed`** - Makes it possible to read or write gzip compressed files.
  Option `compressed` must be combined with `read` or `write`, but not both.
  Notice that the file size obtained with `read_file_info/1` does probably not
  match the number of bytes that can be read from a compressed file.

- **`{zstd, Opts}`** - Makes it possible to read or write zstd compressed
  files. Option `compressed` must be combined with `read` or `write`, but not
  both. Notice that the file size obtained with `read_file_info/1` does
  probably not match the number of bytes that can be read from a compressed
  file.

- **`compressed_one`** - Read one member of a gzip compressed file. Option
  `compressed_one` can only be combined with `read`.

- **`{encoding, Encoding}`** - Makes the file perform automatic translation of
  characters to and from a specific (Unicode) encoding. Notice that the data
  supplied to `write/2` or returned by `read/2` still is byte-oriented; this
  option denotes only how data is stored in the disk file.

  Depending on the encoding, different methods of reading and writing data is
  preferred. The default encoding of `latin1` implies using this module (`file`)
  for reading and writing data as the interfaces provided here work with
  byte-oriented data. Using other (Unicode) encodings makes the `m:io` functions
  `get_chars`, `get_line`, and `put_chars` more suitable, as they can work with
  the full Unicode range.

  If data is sent to an `t:io_device/0` in a format that cannot be converted to
  the specified encoding, or if data is read by a function that returns data in
  a format that cannot cope with the character range of the data, an error
  occurs and the file is closed.

  Allowed values for `Encoding`:

  - **`latin1`** - The default encoding. Bytes supplied to the file, that is,
    `write/2` are written "as is" on the file. Likewise, bytes read from the
    file, that is, `read/2` are returned "as is". If module `m:io` is used for
    writing, the file can only cope with Unicode characters up to code point 255
    (the ISO Latin-1 range).

  - **`unicode or utf8`** - Characters are translated to and from UTF-8 encoding
    before they are written to or read from the file. A file opened in this way
    can be readable using function `read/2`, as long as no data stored on the
    file lies beyond the ISO Latin-1 range (0..255), but failure occurs if the
    data contains Unicode code points beyond that range. The file is best read
    with the functions in the Unicode aware module `m:io`.

    Bytes written to the file by any means are translated to UTF-8 encoding
    before being stored on the disk file.

  - **`utf16 or {utf16,big}`** - Works like `unicode`, but translation is done
    to and from big endian UTF-16 instead of UTF-8.

  - **`{utf16,little}`** - Works like `unicode`, but translation is done to and
    from little endian UTF-16 instead of UTF-8.

  - **`utf32 or {utf32,big}`** - Works like `unicode`, but translation is done
    to and from big endian UTF-32 instead of UTF-8.

  - **`{utf32,little}`** - Works like `unicode`, but translation is done to and
    from little endian UTF-32 instead of UTF-8.

  The Encoding can be changed for a file "on the fly" by using function
  `io:setopts/2`. So a file can be analyzed in latin1 encoding for, for example,
  a BOM, positioned beyond the BOM and then be set for the right encoding before
  further reading. For functions identifying BOMs, see module `m:unicode`.

  This option is not allowed on `raw` files.

- **`ram`** - `File` must be `t:iodata/0`. Returns an `t:fd/0`, which lets
  module `file` operate on the data in-memory as if it is a file.

- **`sync`** - On platforms supporting it, enables the POSIX `O_SYNC`
  synchronous I/O flag or its platform-dependent equivalent (for example,
  `FILE_FLAG_WRITE_THROUGH` on Windows) so that writes to the file block until
  the data is physically written to disk. However, be aware that the exact
  semantics of this flag differ from platform to platform. For example, none of
  Linux or Windows guarantees that all file metadata are also written before the
  call returns. For precise semantics, check the details of your platform
  documentation. On platforms with no support for POSIX `O_SYNC` or equivalent,
  use of the `sync` flag causes `open` to return `{error, enotsup}`.

- **`directory`** - Allows `open` to work on directories.

Returns:

- **`{ok, IoDevice}`** - The file is opened in the requested mode. `IoDevice` is
  a reference to the file.

- **`{error, Reason}`** - The file cannot be opened.

`IoDevice` is really the pid of the process that handles the file. This process
monitors the process that originally opened the file (the owner process). If the
owner process terminates, the file is closed and the process itself terminates
too. An `IoDevice` returned from this call can be used as an argument to the I/O
functions (see `m:io`).

> #### Warning {: .warning }
>
> While this function can be used to open any file, we recommend against using
> it for NFS-mounted files, FIFOs, devices, or similar since they can cause IO
> threads to hang forever.
>
> If your application needs to interact with these kinds of files we recommend
> breaking out those parts to a port program instead.

> #### Note {: .info }
>
> In previous versions of `file`, modes were specified as one of the atoms
> `read`, `write`, or `read_write` instead of a list. This is still allowed for
> reasons of backwards compatibility, but is not to be used for new code. Also
> note that `read_write` is not allowed in a mode list.

Typical error reasons:

- **`enoent`** - The file does not exist.

- **`eacces`** - Missing permission for reading the file or searching one of the
  parent directories.

- **`eisdir`** - The named file is a directory.

- **`enotdir`** - A component of the filename is not a directory, or the
  filename itself is not a directory if `directory` mode was specified. On some
  platforms, `enoent` is returned instead.

- **`enospc`** - There is no space left on the device (if `write` access was
  specified).

# `path_consult`

```erlang
-spec path_consult(Path, Filename) -> {ok, Terms, FullName} | {error, Reason}
                      when
                          Path :: [Dir],
                          Dir :: name_all(),
                          Filename :: name_all(),
                          Terms :: [term()],
                          FullName :: filename_all(),
                          Reason ::
                              posix() |
                              badarg | terminated | system_limit |
                              {Line :: integer(), Mod :: module(), Term :: term()}.
```

Searches the path `Path` (a list of directory names) until the file `Filename`
is found. If `Filename` is an absolute filename, `Path` is ignored. Then reads
Erlang terms, separated by `.`, from the file.

Returns one of the following:

- **`{ok, Terms, FullName}`** - The file is successfully read. `FullName` is the
  full name of the file.

- **`{error, enoent}`** - The file cannot be found in any of the directories in
  `Path`.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang terms in the file. Use `format_error/1` to convert the three-element
  tuple to an English description of the error.

The encoding of `Filename` can be set by a comment as described in
[`epp`](`m:epp#encoding`).

# `path_eval`

```erlang
-spec path_eval(Path, Filename) -> {ok, FullName} | {error, Reason}
                   when
                       Path :: [Dir :: name_all()],
                       Filename :: name_all(),
                       FullName :: filename_all(),
                       Reason ::
                           posix() |
                           badarg | terminated | system_limit |
                           {Line :: integer(), Mod :: module(), Term :: term()}.
```

Searches the path `Path` (a list of directory names) until the file `Filename`
is found. If `Filename` is an absolute filename, `Path` is ignored. Then reads
and evaluates Erlang expressions, separated by `.` (or `,`, a sequence of
expressions is also an expression), from the file. The result of evaluation is
not returned; any expression sequence in the file must be there for its side
effect.

Returns one of the following:

- **`{ok, FullName}`** - The file is read and evaluated. `FullName` is the full
  name of the file.

- **`{error, enoent}`** - The file cannot be found in any of the directories in
  `Path`.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang expressions in the file. Use `format_error/1` to convert the
  three-element tuple to an English description of the error.

The encoding of `Filename` can be set by a comment as described in
[`epp`](`m:epp#encoding`).

# `path_open`

```erlang
-spec path_open(Path, Filename, Modes) -> {ok, IoDevice, FullName} | {error, Reason}
                   when
                       Path :: [Dir :: name_all()],
                       Filename :: name_all(),
                       Modes :: [mode() | directory],
                       IoDevice :: io_device(),
                       FullName :: filename_all(),
                       Reason :: posix() | badarg | system_limit.
```

Searches the path `Path` (a list of directory names) until the file `Filename`
is found. If `Filename` is an absolute filename, `Path` is ignored. Then opens
the file in the mode determined by `Modes`.

Returns one of the following:

- **`{ok, IoDevice, FullName}`** - The file is opened in the requested mode.
  `IoDevice` is a reference to the file and `FullName` is the full name of the
  file.

- **`{error, enoent}`** - The file cannot be found in any of the directories in
  `Path`.

- **`{error, atom()}`** - The file cannot be opened.

# `path_script`

```erlang
-spec path_script(Path, Filename) -> {ok, Value, FullName} | {error, Reason}
                     when
                         Path :: [Dir :: name_all()],
                         Filename :: name_all(),
                         Value :: term(),
                         FullName :: filename_all(),
                         Reason ::
                             posix() |
                             badarg | terminated | system_limit |
                             {Line :: integer(), Mod :: module(), Term :: term()}.
```

Searches the path `Path` (a list of directory names) until the file `Filename`
is found. If `Filename` is an absolute filename, `Path` is ignored. Then reads
and evaluates Erlang expressions, separated by `.` (or `,`, a sequence of
expressions is also an expression), from the file.

Returns one of the following:

- **`{ok, Value, FullName}`** - The file is read and evaluated. `FullName` is
  the full name of the file and `Value` the value of the last expression.

- **`{error, enoent}`** - The file cannot be found in any of the directories in
  `Path`.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang expressions in the file. Use `format_error/1` to convert the
  three-element tuple to an English description of the error.

The encoding of `Filename` can be set by a comment as described in
[`epp`](`m:epp#encoding`).

# `path_script`

```erlang
-spec path_script(Path, Filename, Bindings) -> {ok, Value, FullName} | {error, Reason}
                     when
                         Path :: [Dir :: name_all()],
                         Filename :: name_all(),
                         Bindings :: erl_eval:binding_struct(),
                         Value :: term(),
                         FullName :: filename_all(),
                         Reason ::
                             posix() |
                             badarg | terminated | system_limit |
                             {Line :: integer(), Mod :: module(), Term :: term()}.
```

The same as [`path_script/2`](`path_script/2`) but the variable bindings
`Bindings` are used in the evaluation. See `m:erl_eval` about variable bindings.

# `position`

```erlang
-spec position(IoDevice, Location) -> {ok, NewPosition} | {error, Reason}
                  when
                      IoDevice :: io_device(),
                      Location :: location(),
                      NewPosition :: integer(),
                      Reason :: posix() | badarg | terminated.
```

Sets the position of the file referenced by `IoDevice` to `Location`. Returns
`{ok, NewPosition}` (as absolute offset) if successful, otherwise
`{error, Reason}`. `Location` is one of the following:

- **`Offset`** - The same as `{bof, Offset}`.

- **`{bof, Offset}`** - Absolute offset.

- **`{cur, Offset}`** - Offset from the current position.

- **`{eof, Offset}`** - Offset from the end of file.

- **`bof | cur | eof`** - The same as above with `Offset` 0.

Notice that offsets are counted in bytes, not in characters. If the file is
opened using some other `encoding` than `latin1`, one byte does not correspond
to one character. Positioning in such a file can only be done to known character
boundaries. That is, to a position earlier retrieved by getting a current
position, to the beginning/end of the file or to some other position _known_ to
be on a correct character boundary by some other means (typically beyond a byte
order mark in the file, which has a known byte-size).

A typical error reason is:

- **`einval`** - Either `Location` is illegal, or it is evaluated to a negative
  offset in the file. Notice that if the resulting position is a negative value,
  the result is an error, and after the call the file position is undefined.

# `pread`

```erlang
-spec pread(IoDevice, LocNums) -> {ok, DataL} | eof | {error, Reason}
               when
                   IoDevice :: io_device(),
                   LocNums :: [{Location :: location(), Number :: non_neg_integer()}],
                   DataL :: [Data],
                   Data :: string() | binary() | eof,
                   Reason :: posix() | badarg | terminated.
```

Performs a sequence of [`pread/3`](`pread/3`) in one operation, which is more
efficient than calling them one at a time. Returns `{ok, [Data, ...]}` or
`{error, Reason}`, where each `Data`, the result of the corresponding `pread`,
is either a list or a binary depending on the mode of the file, or `eof` if the
requested position is beyond end of file.

As the position is specified as a byte-offset, take special caution when working
with files where `encoding` is set to something else than `latin1`, as not every
byte position is a valid character boundary on such a file.

# `pread`

```erlang
-spec pread(IoDevice, Location, Number) -> {ok, Data} | eof | {error, Reason}
               when
                   IoDevice :: io_device(),
                   Location :: location(),
                   Number :: non_neg_integer(),
                   Data :: string() | binary(),
                   Reason :: posix() | badarg | terminated.
```

Combines [`position/2`](`position/2`) and [`read/2`](`read/2`) in one operation,
which is more efficient than calling them one at a time.

`Location` is only allowed to be an integer for `raw` and `ram` modes.

The current position of the file after the operation is undefined for `raw` mode
and unchanged for `ram` mode.

As the position is specified as a byte-offset, take special caution when working
with files where `encoding` is set to something else than `latin1`, as not every
byte position is a valid character boundary on such a file.

# `pwrite`

```erlang
-spec pwrite(IoDevice, LocBytes) -> ok | {error, {N, Reason}}
                when
                    IoDevice :: io_device(),
                    LocBytes :: [{Location :: location(), Bytes :: iodata()}],
                    N :: non_neg_integer(),
                    Reason :: posix() | badarg | terminated.
```

Performs a sequence of [`pwrite/3`](`pwrite/3`) in one operation, which is more
efficient than calling them one at a time. Returns `ok` or
`{error, {N, Reason}}`, where `N` is the number of successful writes done before
the failure.

When positioning in a file with other `encoding` than `latin1`, caution must be
taken to set the position on a correct character boundary. For details, see
`position/2`.

# `pwrite`

```erlang
-spec pwrite(IoDevice, Location, Bytes) -> ok | {error, Reason}
                when
                    IoDevice :: io_device(),
                    Location :: location(),
                    Bytes :: iodata(),
                    Reason :: posix() | badarg | terminated.
```

Combines [`position/2`](`position/2`) and [`write/2`](`write/2`) in one
operation, which is more efficient than calling them one at a time.

`Location` is only allowed to be an integer for `raw` and `ram` modes.

The current position of the file after the operation is undefined for `raw` mode
and unchanged for `ram` mode.

When positioning in a file with other `encoding` than `latin1`, caution must be
taken to set the position on a correct character boundary. For details, see
`position/2`.

# `read`

```erlang
-spec read(IoDevice, Number) -> {ok, Data} | eof | {error, Reason}
              when
                  IoDevice :: io_device() | io:device(),
                  Number :: non_neg_integer(),
                  Data :: string() | binary(),
                  Reason :: posix() | badarg | terminated | {no_translation, unicode, latin1}.
```

Reads `Number` bytes/characters from the file referenced by `IoDevice`.

The functions `read/2`, `pread/3`, and `read_line/1` are the only ways to read from
a file opened in `raw` mode (although they work for normally opened files, too).

For files where `encoding` is set to something else than `latin1`, one character
can be represented by more than one byte on the file. The parameter `Number`
always denotes the number of _characters_ read from the file, while the position
in the file can be moved much more than this number when reading a Unicode file.

Also, if `encoding` is set to something else than `latin1`, the
[`read/2`](`read/2`) call fails if the data contains characters larger than 255,
which is why `io:get_chars/3` is to be preferred when reading such a file.

The function returns:

- **`{ok, Data}`** - If the file was opened in binary mode, the read bytes are
  returned in a binary, otherwise in a list. The list or binary is shorter than
  the number of bytes requested if end of file was reached.

- **`eof`** - Returned if `Number>0` and end of file was reached before anything
  at all could be read.

- **`{error, Reason}`** - An error occurred.

Typical error reasons:

- **`ebadf`** - The file is not opened for reading.

- **`{no_translation, unicode, latin1}`** - The file is opened with another
  `encoding` than `latin1` and the data in the file cannot be translated to the
  byte-oriented data that this function returns.

# `read_file`

```erlang
-spec read_file(Filename) -> {ok, Binary} | {error, Reason}
                   when
                       Filename :: name_all(),
                       Binary :: binary(),
                       Reason :: posix() | badarg | terminated | system_limit.
```

# `read_file`
*since OTP 27.0* 

```erlang
-spec read_file(Filename, Opts) -> {ok, Binary} | {error, Reason}
                   when
                       Filename :: name_all(),
                       Opts :: [read_file_option()],
                       Binary :: binary(),
                       Reason :: posix() | badarg | terminated | system_limit.
```

Returns `{ok, Binary}`, where `Binary` is a binary data object that contains the
contents of `Filename`, or `{error, Reason}` if an error occurs.

If the option `raw` is set, the file server is not called.

Typical error reasons:

- **`enoent`** - The file does not exist.

- **`eacces`** - Missing permission for reading the file, or for searching one
  of the parent directories.

- **`eisdir`** - The named file is a directory.

- **`enotdir`** - A component of the filename is not a directory. On some
  platforms, `enoent` is returned instead.

- **`enomem`** - There is not enough memory for the contents of the file.

# `read_file_info`

```erlang
-spec read_file_info(File) -> {ok, FileInfo} | {error, Reason}
                        when
                            File :: name_all() | io_device(),
                            FileInfo :: file_info(),
                            Reason :: posix() | badarg.
```

# `read_file_info`
*since OTP R15B* 

```erlang
-spec read_file_info(File, Opts) -> {ok, FileInfo} | {error, Reason}
                        when
                            File :: name_all() | io_device(),
                            Opts :: [file_info_option()],
                            FileInfo :: file_info(),
                            Reason :: posix() | badarg.
```

Retrieves information about a file. Returns `{ok, FileInfo}` if successful,
otherwise `{error, Reason}`.

`FileInfo` is a record `file_info`, defined in the Kernel include file `file.hrl`.
Include the following directive in the module from which the function is called:

```erlang
-include_lib("kernel/include/file.hrl").
```

The time type returned in `atime`, `mtime`, and `ctime` is dependent on the time
type set in `Opts :: {time, Type}` as follows:

- **`local`** - Returns local time.

- **`universal`** - Returns universal time.

- **`posix`** - Returns seconds since or before Unix time epoch, which is
  1970-01-01 00:00 UTC.

Default is `{time, local}`.

If the option `raw` is set, the file server is not called and only information
about local files is returned. Note that this will break this module's atomicity
guarantees as it can race with a concurrent call to
[`write_file_info/1,2` ](`write_file_info/2`).

This option has no effect when the function is given an I/O device instead of a
file name. Use `open/2` with the `raw` mode to obtain a file descriptor first.

> #### Note {: .info }
>
> As file times are stored in POSIX time on most OS, it is faster to query file
> information with option `posix`.

The record `file_info` contains the following fields:

- **`size = ` `t:non_neg_integer/0`** - Size of file in bytes.

- **`type = device | directory | other | regular`** - The type of the file. Can
  also contain `symlink` when returned from
  [read_link_info/1,2](`read_link_info/1`).

- **`access = read | write | read_write | none`** - The current system access to
  the file.

- **`atime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - The last time the file was
  read.

- **`mtime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - The last time the file was
  written.

- **`ctime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - The interpretation of this
  time field depends on the operating system. On Unix, it is the last time the
  file or the `inode` was changed. In Windows, it is the create time.

- **`mode = ` `t:non_neg_integer/0`** - The file permissions as the sum of the following
  bit values:

  - **`8#00400`** - read permission: owner

  - **`8#00200`** - write permission: owner

  - **`8#00100`** - execute permission: owner

  - **`8#00040`** - read permission: group

  - **`8#00020`** - write permission: group

  - **`8#00010`** - execute permission: group

  - **`8#00004`** - read permission: other

  - **`8#00002`** - write permission: other

  - **`8#00001`** - execute permission: other

  - **`16#800`** - set user id on execution

  - **`16#400`** - set group id on execution

  On Unix platforms, other bits than those listed above may be set.

- **`links = ` `t:non_neg_integer/0`** - Number of links to the file (this is always 1
  for file systems that have no concept of links).

- **`major_device = ` `t:non_neg_integer/0`** - Identifies the file system where the
  file is located. In Windows, the number indicates a drive as follows: 0 means
  A:, 1 means B:, and so on.

- **`minor_device = ` `t:non_neg_integer/0`** - Only valid for character devices on
  Unix. In all other cases, this field is zero.

- **`inode = ` `t:non_neg_integer/0`** - Gives the `inode` number. On non-Unix file
  systems, this field is zero.

- **`uid = ` `t:non_neg_integer/0`** - Indicates the owner of the file. On non-Unix file
  systems, this field is zero.

- **`gid = ` `t:non_neg_integer/0`** - Gives the group that the owner of the file
  belongs to. On non-Unix file systems, this field is zero.

Typical error reasons:

- **`eacces`** - Missing search permission for one of the parent directories of
  the file.

- **`enoent`** - The file does not exist.

- **`enotdir`** - A component of the filename is not a directory. On some
  platforms, `enoent` is returned instead.

# `read_line`

```erlang
-spec read_line(IoDevice) -> {ok, Data} | eof | {error, Reason}
                   when
                       IoDevice :: io_device() | io:device(),
                       Data :: string() | binary(),
                       Reason :: posix() | badarg | terminated | {no_translation, unicode, latin1}.
```

Reads a line of bytes/characters from the file referenced by `IoDevice`.

Lines are defined to be delimited by the linefeed (LF, `\n`) character, but any
carriage return (CR, `\r`) followed by a newline is also treated as a single LF
character (the carriage return is silently ignored). The line is returned
_including_ the LF, but excluding any CR immediately followed by an LF. This
behaviour is consistent with the behaviour of `io:get_line/2`. If end of file is
reached without any LF ending the last line, a line with no trailing LF is
returned.

The function can be used on files opened in `raw` mode. However, it is
inefficient to use it on `raw` files if the file is not opened with option
`{read_ahead, Size}` specified. Thus, combining `raw` and `{read_ahead, Size}`
is highly recommended when opening a text file for raw line-oriented reading.

If `encoding` is set to something else than `latin1`, the
[`read_line/1`](`read_line/1`) call fails if the data contains characters larger
than 255, why `io:get_line/2` is to be preferred when reading such a file.

The function returns:

- **`{ok, Data}`** - One line from the file is returned, including the trailing
  LF, but with CRLF sequences replaced by a single LF (see above).

  If the file is opened in binary mode, the read bytes are returned in a binary,
  otherwise in a list.

- **`eof`** - Returned if end of file was reached before anything at all could
  be read.

- **`{error, Reason}`** - An error occurred.

Typical error reasons:

- **`ebadf`** - The file is not opened for reading.

- **`{no_translation, unicode, latin1}`** - The file is opened with another
  `encoding` than `latin1` and the data on the file cannot be translated to the
  byte-oriented data that this function returns.

# `read_link`

```erlang
-spec read_link(Name) -> {ok, Filename} | {error, Reason}
                   when Name :: name_all(), Filename :: filename(), Reason :: posix() | badarg.
```

Returns `{ok, Filename}` if `Name` refers to a symbolic
link that is not a raw filename, or `{error, Reason}` otherwise. On platforms
that do not support symbolic links, the return value is `{error,enotsup}`.

Typical error reasons:

- **`einval`** - `Name` does not refer to a symbolic link or the name of the
  file that it refers to does not conform to the expected encoding.

- **`enoent`** - The file does not exist.

- **`enotsup`** - Symbolic links are not supported on this platform.

# `read_link_all`
*since OTP R16B* 

```erlang
-spec read_link_all(Name) -> {ok, Filename} | {error, Reason}
                       when Name :: name_all(), Filename :: filename_all(), Reason :: posix() | badarg.
```

Returns `{ok, Filename}` if `Name` refers to a symbolic link or
`{error, Reason}` otherwise. On platforms that do not support symbolic links,
the return value is `{error, enotsup}`.

Notice that `Filename` can be either a list or a binary.

Typical error reasons:

- **`einval`** - `Name` does not refer to a symbolic link.

- **`enoent`** - The file does not exist.

- **`enotsup`** - Symbolic links are not supported on this platform.

# `read_link_info`

```erlang
-spec read_link_info(Name) -> {ok, FileInfo} | {error, Reason}
                        when Name :: name_all(), FileInfo :: file_info(), Reason :: posix() | badarg.
```

# `read_link_info`
*since OTP R15B* 

```erlang
-spec read_link_info(Name, Opts) -> {ok, FileInfo} | {error, Reason}
                        when
                            Name :: name_all(),
                            Opts :: [file_info_option()],
                            FileInfo :: file_info(),
                            Reason :: posix() | badarg.
```

Works like [`read_file_info/1,2`](`read_file_info/2`) except that if `Name` is a
symbolic link, information about the link is returned in the `file_info` record
and the `type` field of the record is set to `symlink`.

If the option `raw` is set, the file server is not called and only information
about local files is returned. Note that this will break this module's atomicity
guarantees as it can race with a concurrent call to
[`write_file_info/1,2`](`write_file_info/2`)

If `Name` is not a symbolic link, this function returns the same result as
[`read_file_info/1`](`read_file_info/1`). On platforms that do not support
symbolic links, this function is always equivalent to
[`read_file_info/1`](`read_file_info/1`).

# `rename`

```erlang
-spec rename(Source, Destination) -> ok | {error, Reason}
                when Source :: name_all(), Destination :: name_all(), Reason :: posix() | badarg.
```

Tries to rename the file `Source` to `Destination`. It can be used to move files
(and directories) between directories, but it is not sufficient to specify the
destination only. The destination filename must also be specified. For example,
if `bar` is a normal file and `foo` and `baz` are directories,
[`rename("foo/bar", "baz")`](`rename/2`) returns an error, but
[`rename("foo/bar", "baz/bar")`](`rename/2`) succeeds. Returns `ok` if it is
successful.

> #### Note {: .info }
>
> Renaming of open files is not allowed on most platforms (see `eacces` below).

Typical error reasons:

- **`eacces`** - Missing read or write permissions for the parent directories of
  `Source` or `Destination`. On some platforms, this error is given if either
  `Source` or `Destination` is open.

- **`eexist`** - `Destination` is not an empty directory. On some platforms,
  also given when `Source` and `Destination` are not of the same type.

- **`einval`** - `Source` is a root directory, or `Destination` is a
  subdirectory of `Source`.

- **`eisdir`** - `Destination` is a directory, but `Source` is not.

- **`enoent`** - `Source` does not exist.

- **`enotdir`** - `Source` is a directory, but `Destination` is not.

- **`exdev`** - `Source` and `Destination` are on different file systems.

# `script`

```erlang
-spec script(Filename) -> {ok, Value} | {error, Reason}
                when
                    Filename :: name_all(),
                    Value :: term(),
                    Reason ::
                        posix() |
                        badarg | terminated | system_limit |
                        {Line :: integer(), Mod :: module(), Term :: term()}.
```

Reads and evaluates Erlang expressions, separated by `.` (or `,`, a sequence of
expressions is also an expression), from the file.

Returns one of the following:

- **`{ok, Value}`** - The file is read and evaluated. `Value` is the value of
  the last expression.

- **`{error, atom()}`** - An error occurred when opening the file or reading it.
  For a list of typical error codes, see `open/2`.

- **`{error, {Line, Mod, Term}}`** - An error occurred when interpreting the
  Erlang expressions in the file. Use `format_error/1` to convert the
  three-element tuple to an English description of the error.

The encoding of `Filename` can be set by a comment as described in
[`epp`](`m:epp#encoding`).

# `script`

```erlang
-spec script(Filename, Bindings) -> {ok, Value} | {error, Reason}
                when
                    Filename :: name_all(),
                    Bindings :: erl_eval:binding_struct(),
                    Value :: term(),
                    Reason ::
                        posix() |
                        badarg | terminated | system_limit |
                        {Line :: integer(), Mod :: module(), Term :: term()}.
```

The same as [`script/1`](`script/1`) but the variable bindings `Bindings` are
used in the evaluation. See `m:erl_eval` about variable bindings.

# `sendfile`
*since OTP R15B* 

```erlang
-spec sendfile(Filename, Socket) ->
                  {ok, non_neg_integer()} | {error, inet:posix() | closed | badarg | not_owner}
                  when
                      Filename :: name_all(),
                      Socket ::
                          inet:socket() |
                          socket:socket() |
                          fun((iolist()) -> ok | {error, inet:posix() | closed}).
```

Sends the file `Filename` to `Socket`. Returns `{ok, BytesSent}` if successful,
otherwise `{error, Reason}`.

# `sendfile`
*since OTP R15B* 

```erlang
-spec sendfile(RawFile, Socket, Offset, Bytes, Opts) ->
                  {ok, non_neg_integer()} | {error, inet:posix() | closed | badarg | not_owner}
                  when
                      RawFile :: fd(),
                      Socket ::
                          inet:socket() |
                          socket:socket() |
                          fun((iolist()) -> ok | {error, inet:posix() | closed}),
                      Offset :: non_neg_integer(),
                      Bytes :: non_neg_integer(),
                      Opts :: [sendfile_option()].
```

Sends `Bytes` from the file referenced by `RawFile` beginning at `Offset` to
`Socket`. Returns `{ok, BytesSent}` if successful, otherwise `{error, Reason}`.
If `Bytes` is set to `0` all data after the specified `Offset` is sent.

The file used must be opened using the `raw` flag, and the process calling
`sendfile` must be the controlling process of the socket. See
`gen_tcp:controlling_process/2` or module [`socket`'s](`socket:setopt/3`)
[level `otp` socket option ](`t:socket:otp_socket_option/0`)`controlling_process`.

If the OS used does not support non-blocking `sendfile`, an Erlang fallback
using `read/2` and `gen_tcp:send/2` is used.

The option list can contain the following options:

- **`chunk_size`** - The chunk size used by the Erlang fallback to send data. If
  using the fallback, set this to a value that comfortably fits in the systems
  memory. Default is 20 MB.

# `set_cwd`

```erlang
-spec set_cwd(Dir) -> ok | {error, Reason}
                 when
                     Dir :: name() | EncodedBinary,
                     EncodedBinary :: binary(),
                     Reason :: posix() | badarg | no_translation.
```

Sets the current working directory of the file server to `Dir`. Returns `ok` if
successful.

The functions in the module `file` usually treat binaries as raw filenames, that
is, they are passed "as is" even when the encoding of the binary does not agree
with [`native_name_encoding()`](`native_name_encoding/0`). However, this
function expects binaries to be encoded according to the value returned by
`native_name_encoding/0`.

Typical error reasons are:

- **`enoent`** - The directory does not exist.

- **`enotdir`** - A component of `Dir` is not a directory. On some platforms,
  `enoent` is returned.

- **`eacces`** - Missing permission for the directory or one of its parents.

- **`badarg`** - `Dir` has an improper type, such as tuple.

- **`no_translation`** - `Dir` is a `t:binary/0` with characters coded in
  ISO-latin-1 and the VM is operating with unicode filename encoding.

> #### Warning {: .warning }
>
> In a future release, a bad type for argument `Dir` will probably generate an
> exception.

# `sync`

```erlang
-spec sync(IoDevice) -> ok | {error, Reason}
              when IoDevice :: io_device(), Reason :: posix() | badarg | terminated.
```

Ensures that any buffers kept by the operating system (not by the Erlang runtime
system) are written to disk. On some platforms, this function might have no
effect.

A typical error reason is:

- **`enospc`** - Not enough space left to write the file.

# `truncate`

```erlang
-spec truncate(IoDevice) -> ok | {error, Reason}
                  when IoDevice :: io_device(), Reason :: posix() | badarg | terminated.
```

Truncates the file referenced by `IoDevice` at the current position. Returns
`ok` if successful, otherwise `{error, Reason}`.

# `write`

```erlang
-spec write(IoDevice, Bytes) -> ok | {error, Reason}
               when
                   IoDevice :: io_device() | io:device(),
                   Bytes :: iodata(),
                   Reason :: posix() | badarg | terminated.
```

Writes `Bytes` to the file referenced by `IoDevice`. This function is the only
way to write to a file opened in `raw` mode (although it works for normally
opened files too). Returns `ok` if successful, and `{error, Reason}` otherwise.

If the file is opened with `encoding` set to something else than `latin1`, each
byte written can result in many bytes being written to the file, as the byte
range 0..255 can represent anything between one and four bytes depending on
value and UTF encoding type. If you want to write `t:unicode:chardata/0` to the
`IoDevice` you should use `io:put_chars/2` instead.

Typical error reasons:

- **`ebadf`** - The file is not opened for writing.

- **`enospc`** - No space is left on the device.

# `write_file`

```erlang
-spec write_file(Filename, Bytes) -> ok | {error, Reason}
                    when
                        Filename :: name_all(),
                        Bytes :: iodata(),
                        Reason :: posix() | badarg | terminated | system_limit.
```

Writes the contents of the `iodata` term `Bytes` to file `Filename`. The file is
created if it does not exist. If it exists, the previous contents are
overwritten. Returns `ok` if successful, otherwise `{error, Reason}`.

Typical error reasons:

- **`enoent`** - A component of the filename does not exist.

- **`enotdir`** - A component of the filename is not a directory. On some
  platforms, `enoent` is returned instead.

- **`enospc`** - No space is left on the device.

- **`eacces`** - Missing permission for writing the file or searching one of the
  parent directories.

- **`eisdir`** - The named file is a directory.

# `write_file`

```erlang
-spec write_file(Filename, Bytes, Modes) -> ok | {error, Reason}
                    when
                        Filename :: name_all(),
                        Bytes :: iodata(),
                        Modes :: [mode()],
                        Reason :: posix() | badarg | terminated | system_limit.
```

Same as [`write_file/2`](`write_file/2`), but takes a third argument `Modes`, a
list of possible modes, see `open/2`. The mode flags `binary` and `write` are
implicit, so they are not to be used.

# `write_file_info`

```erlang
-spec write_file_info(Filename, FileInfo) -> ok | {error, Reason}
                         when
                             Filename :: name_all(), FileInfo :: file_info(), Reason :: posix() | badarg.
```

# `write_file_info`
*since OTP R15B* 

```erlang
-spec write_file_info(Filename, FileInfo, Opts) -> ok | {error, Reason}
                         when
                             Filename :: name_all(),
                             Opts :: [file_info_option()],
                             FileInfo :: file_info(),
                             Reason :: posix() | badarg.
```

Changes file information. Returns `ok` if successful, otherwise
`{error, Reason}`.

`FileInfo` is a record `file_info`, defined in the Kernel
include file `file.hrl`. Include the following directive in the module from
which the function is called:

```erlang
-include_lib("kernel/include/file.hrl").
```

The time type set in `atime`, `mtime`, and `ctime` depends on the time type set
in `Opts :: {time, Type}` as follows:

- **`local`** - Interprets the time set as local.

- **`universal`** - Interprets it as universal time.

- **`posix`** - Must be seconds since or before Unix time epoch, which is
  1970-01-01 00:00 UTC.

Default is `{time, local}`.

If the option `raw` is set, the file server is not called and only information
about local files is returned.

The following fields are used from the record, if they are specified:

- **`atime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - The last time the file was
  read.

- **`mtime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - The last time the file was
  written.

- **`ctime = ` `t:date_time/0` ` | ` `t:non_neg_integer/0`** - On Unix, any value
  specified for this field is ignored (the "ctime" for the file is set to the
  current time). On Windows, this field is the new creation time to set for the
  file.

- **`mode = ` `t:non_neg_integer/0`** - The file permissions as the sum of the following
  bit values:

  - **`8#00400`** - Read permission: owner

  - **`8#00200`** - Write permission: owner

  - **`8#00100`** - Execute permission: owner

  - **`8#00040`** - Read permission: group

  - **`8#00020`** - Write permission: group

  - **`8#00010`** - Execute permission: group

  - **`8#00004`** - Read permission: other

  - **`8#00002`** - Write permission: other

  - **`8#00001`** - Execute permission: other

  - **`16#800`** - Set user id on execution

  - **`16#400`** - Set group id on execution

  On Unix platforms, other bits than those listed above may be set.

- **`uid = ` `t:non_neg_integer/0`** - Indicates the file owner. Ignored for non-Unix
  file systems.

- **`gid = ` `t:non_neg_integer/0`** - Gives the group that the file owner belongs to.
  Ignored for non-Unix file systems.

Typical error reasons:

- **`eacces`** - Missing search permission for one of the parent directories of
  the file.

- **`enoent`** - The file does not exist.

- **`enotdir`** - A component of the filename is not a directory. On some
  platforms, `enoent` is returned instead.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
