# `erl_tar`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/erl_tar.erl#L32)

Unix 'tar' utility for reading and writing tar archives.

This module archives and extract files to and from a tar file. This module
supports reading most common tar formats, namely v7, STAR, USTAR, and PAX, as
well as some of GNU tar's extensions to the USTAR format (sparse files most
notably). It produces tar archives in USTAR format, unless the files being
archived require PAX format due to restrictions in USTAR (such as unicode
metadata, filename length, and more). As such, `erl_tar` supports tar archives
produced by most all modern tar utilities, and produces tarballs which should be
similarly portable.

By convention, the name of a tar file is to end in "`.tar`". To abide to the
convention, add "`.tar`" to the name.

Tar files can be created in one operation using function `create/2` or
`create/3`.

Alternatively, for more control, use functions `open/2`, [`add/3,4`](`add/3`),
and `close/1`.

To extract all files from a tar file, use function `extract/1`. To extract only
some files or to be able to specify some more options, use function `extract/2`.

To return a list of the files in a tar file, use function `table/1` or
`table/2`. To print a list of files to the Erlang shell, use function `t/1` or
`tt/1`.

To convert an error term returned from one of the functions above to a readable
message, use function `format_error/1`.

## Unicode Support

If `file:native_name_encoding/0` returns `utf8`, path names are encoded in UTF-8
when creating tar files, and path names are assumed to be encoded in UTF-8 when
extracting tar files.

If `file:native_name_encoding/0` returns `latin1`, no translation of path names
is done.

Unicode metadata stored in PAX headers is preserved

## Other Storage Media

The `m:ftp` module normally accesses the tar file on disk using the `m:file`
module. When other needs arise, you can define your own low-level Erlang
functions to perform the writing and reading on the storage media; use function
`init/3`.

An example of this is the SFTP support in `ssh_sftp:open_tar/3`. This function
opens a tar file on a remote machine using an SFTP channel.

## Limitations

- If you must remain compatible with the USTAR tar format, you must ensure file
  paths being stored are less than 255 bytes in total, with a maximum filename
  component length of 100 bytes. USTAR uses a header field (prefix) in addition
  to the name field, and splits file paths longer than 100 bytes into two parts.
  This split is done on a directory boundary, and is done in such a way to make
  the best use of the space available in those two fields, but in practice this
  will often mean that you have less than 255 bytes for a path. `erl_tar` will
  automatically upgrade the format to PAX to handle longer filenames, so this is
  only an issue if you need to extract the archive with an older implementation
  of `erl_tar` or `tar` which does not support PAX. In this case, the PAX
  headers will be extracted as regular files, and you will need to apply them
  manually.
- Like the above, if you must remain USTAR compatible, you must also ensure that
  paths for symbolic/hard links are no more than 100 bytes, otherwise PAX
  headers will be used.

# `add_opt`
*not exported* 

```erlang
-type add_opt() ::
          dereference | verbose |
          {chunks, pos_integer()} |
          {atime, non_neg_integer()} |
          {mtime, non_neg_integer()} |
          {ctime, non_neg_integer()} |
          {mode, non_neg_integer()} |
          {uid, non_neg_integer()} |
          {gid, non_neg_integer()}.
```

# `create_opt`
*not exported* 

```erlang
-type create_opt() :: compressed | cooked | dereference | verbose.
```

# `extract_opt`
*not exported* 

```erlang
-type extract_opt() ::
          {cwd, string()} |
          {files, [name_in_archive()]} |
          {chunks, pos_integer()} |
          {max_size, pos_integer() | infinity} |
          compressed | cooked | memory | keep_old_files | verbose.
```

# `file_op`
*not exported* 

```erlang
-type file_op() ::
          fun((close, file:io_device()) -> ok | {error, term()}) |
          fun((position, {file:io_device(), file:location()}) -> {ok, integer()} | {error, term()}) |
          fun((read2, {file:io_device(), non_neg_integer()}) ->
                  {ok, string() | binary()} | eof | {error, term()}) |
          fun((write, {file:io_device(), iodata()}) -> ok | {error, term()}).
```

# `filelist`
*not exported* 

```erlang
-type filelist() :: [file:filename() | {name_in_archive(), file:filename_all()}].
```

# `gid`
*not exported* 

```erlang
-type gid() :: non_neg_integer().
```

# `mode`
*not exported* 

```erlang
-type mode() :: non_neg_integer().
```

# `name_in_archive`
*not exported* 

```erlang
-type name_in_archive() :: string().
```

# `open_type`
*not exported* 

```erlang
-type open_type() :: file:filename_all() | {binary, binary()} | {file, file:io_device()}.
```

# `tar_descriptor`

```erlang
-opaque tar_descriptor()
```

# `tar_entry`
*not exported* 

```erlang
-type tar_entry() ::
          {Name :: name_in_archive(),
           Type :: typeflag(),
           Size :: non_neg_integer(),
           MTime :: tar_time(),
           Mode :: mode(),
           Uid :: uid(),
           Gid :: gid()}.
```

# `tar_time`
*not exported* 

```erlang
-type tar_time() :: non_neg_integer().
```

# `typeflag`
*not exported* 

```erlang
-type typeflag() :: regular | link | symlink | char | block | directory | fifo | reserved | unknown.
```

# `uid`
*not exported* 

```erlang
-type uid() :: non_neg_integer().
```

# `user_data`
*not exported* 

```erlang
-type user_data() :: term().
```

# `add`

```erlang
-spec add(TarDescriptor, Name, Options) -> ok | {error, term()}
             when
                 TarDescriptor :: tar_descriptor(),
                 Name :: name_in_archive() | {name_in_archive(), file:filename_all()},
                 Options :: [add_opt()].
```

Equivalent to `add/4`.

If `Name` is `t:name_in_archive/0`, then [`add(TarDescriptor, Name, Name, Options)`](`add/4`) is called.

If `Name` is a two tuple then [`add(TarDescriptor, NameInArchive, Name, Options)`](`add/4`) is called.

# `add`

```erlang
-spec add(TarDescriptor, Filename, NameInArchive, Options) -> ok | {error, term()}
             when
                 TarDescriptor :: tar_descriptor(),
                 Filename :: file:filename_all(),
                 NameInArchive :: name_in_archive(),
                 Options :: [add_opt()].
```

Adds a file to a tar file that has been opened for writing by
[`open/1`](`open/2`).

`NameInArchive` is the name under which the file becomes stored in the tar file.
The file gets this name when it is extracted from the tar file.

Options:

- **`dereference`** - By default, symbolic links are stored as symbolic links in
  the tar file. To override the default and store the file that the symbolic
  link points to into the tar file, use option `dereference`.

- **`verbose`** - Prints an informational message about the added file.

- **`{chunks,ChunkSize}`** - Sets the chunk size, in bytes, for reading data
  from the file. Defaults to 65536 bytes.
- **`{atime,non_neg_integer()}`** - Sets the last time, as
  [POSIX time](`e:erts:time_correction.md#posix-time`), when the file was read.
  See also `file:read_file_info/1`.

- **`{mtime,non_neg_integer()}`** - Sets the last time, as
  [POSIX time](`e:erts:time_correction.md#posix-time`), when the file was
  written. See also `file:read_file_info/1`.

- **`{ctime,non_neg_integer()}`** - Sets the time, as
  [POSIX time](`e:erts:time_correction.md#posix-time`), when the file was
  created. See also `file:read_file_info/1`.

- **`{uid,non_neg_integer()}`** - Sets the file owner. `file:read_file_info/1`.

- **`{gid,non_neg_integer()}`** - Sets the group that the file owner belongs to.
  `file:read_file_info/1`.

- **`{mode,non_neg_integer()}`** - Sets the file permissions.
  See also `file:read_file_info/1`.

# `close`

```erlang
-spec close(TarDescriptor :: tar_descriptor()) -> ok | {error, term()}.
```

Closes a tar file opened by `open/2`.

# `create`

```erlang
-spec create(file:filename_all(), filelist()) -> ok | {error, {string(), term()}}.
```

Creates a tar file and archives the files whose names are specified in
`FileList` into it. The files can either be read from disk or be specified as
binaries.

# `create`

```erlang
-spec create(file:filename_all(), filelist(), [create_opt()]) ->
                ok | {error, term()} | {error, {string(), term()}}.
```

Creates a tar file and archives the files whose names are specified in
`FileList` into it. The files can either be read from disk or be specified as
binaries.

The options in `OptionList` modify the defaults as follows:

- **`compressed`** - The entire tar file is compressed, as if it has been run
  through the `gzip` program. To abide to the convention that a compressed tar
  file is to end in "`.tar.gz`" or "`.tgz`", add the appropriate extension.

- **`cooked`** - By default, function [`open/2`](`open/2`) opens the tar file in
  `raw` mode, which is faster but does not allow a remote (Erlang) file server
  to be used. Adding `cooked` to the mode list overrides the default and opens
  the tar file without option `raw`.

- **`dereference`** - By default, symbolic links are stored as symbolic links in
  the tar file. To override the default and store the file that the symbolic
  link points to into the tar file, use option `dereference`.

- **`verbose`** - Prints an informational message about each added file.

# `extract`

```erlang
-spec extract(Open :: open_type()) -> ok | {error, term()}.
```

Extracts all files from a tar archive.

If argument `Name` is specified as `{binary,Binary}`, the contents of the binary
is assumed to be a tar archive.

If argument `Name` is specified as `{file,Fd}`, `Fd` is assumed to be a file
descriptor returned from function `file:open/2`.

Otherwise, `Name` is to be a filename.

> #### Note {: .info }
>
> Leading slashes in tar member names will be removed before writing the file.
> That is, absolute paths will be turned into relative paths. There will be an
> info message written to the error logger when paths are changed in this way.

> #### Warning {: .warning }
>
> The `compressed` and `cooked` flags are invalid when passing a file descriptor
> with `{file,Fd}`. The file is assumed to have been opened with the appropriate
> flags.

# `extract`

```erlang
-spec extract(Open :: open_type(), []) -> ok | {error, term()};
             (Open :: open_type(), [extract_opt(), ...]) ->
                 ok | {ok, [{string(), binary()}]} | {error, term()}.
```

Extracts files from a tar archive.

If argument `Name` is specified as `{binary,Binary}`, the contents of the binary
is assumed to be a tar archive.

If argument `Name` is specified as `{file,Fd}`, `Fd` is assumed to be a file
descriptor returned from function `file:open/2`.

Otherwise, `Name` is to be a filename.

The following options modify the defaults for the extraction as follows:

- **`{cwd,Cwd}`** - Files with relative filenames are by default extracted to
  the current working directory. With this option, files are instead extracted
  into directory `Cwd`.

- **`{files,FileList}`** - By default, all files are extracted from the tar
  file. With this option, only those files are extracted whose names are
  included in `FileList`.

- **`compressed`** - With this option, the file is uncompressed while
  extracting. If the tar file is not compressed, this option is ignored.

- **`cooked`** - By default, function [`open/2`](`open/2`) function opens the
  tar file in `raw` mode, which is faster but does not allow a remote (Erlang)
  file server to be used. Adding `cooked` to the mode list overrides the default
  and opens the tar file without option `raw`.

- **`memory`** - Instead of extracting to a directory, this option gives the
  result as a list of tuples `{Filename, Binary}`, where `Binary` is a binary
  containing the extracted data of the file named `Filename` in the tar file.

- **`keep_old_files`** - By default, all existing files with the same name as
  files in the tar file are overwritten. With this option, existing files are
  not overwriten.

- **`verbose`** - Prints an informational message for each extracted file.

- **`{chunks,ChunkSize}`** - Sets the chunk size, in bytes, for writing extracted
  file data to disk. Defaults to 65536 bytes.

- **`{max_size,Size}`** - Sets a limit on the total size of extracted data. If
  the cumulative size of all extracted files exceeds `Size` bytes, extraction
  fails with `{error, too_big}`. When extracting a compressed binary archive,
  the decompressed binary is also subject to this limit. Defaults to `infinity`
  (no limit).

> #### Warning {: .warning }
>
> The `compressed` and `cooked` flags are invalid when passing a file descriptor
> with `{file,Fd}`. The file is assumed to have been opened with the appropriate
> flags.

# `format_error`

```erlang
-spec format_error(term()) -> string().
```

Converts an error reason term to a human-readable error message string.

# `init`
*since OTP 17.4* 

```erlang
-spec init(UserData :: user_data(), write | read, file_op()) -> {ok, tar_descriptor()} | {error, badarg}.
```

The `Fun` is the definition of what to do when the different storage operations
functions are to be called from the higher tar handling functions (such as
[`add/3`](`add/3`), [`add/4`](`add/4`), and [`close/1`](`close/1`)).

The `Fun` is called when the tar function wants to do a low-level operation,
like writing a block to a file. The `Fun` is called as
`Fun(Op, {UserData,Parameters...})`, where `Op` is the operation name,
`UserData` is the term passed as the first argument to `init/1` and
`Parameters...` are the data added by the tar function to be passed down to the
storage handling function.

Parameter `UserData` is typically the result of opening a low-level structure
like a file descriptor or an SFTP channel id. The different `Fun` clauses
operate on that very term.

The following are the fun clauses parameter lists:

- **`(write, {UserData,DataToWrite})`** - Writes term `DataToWrite` using
  `UserData`.

- **`(close, UserData)`** - Closes the access.

- **`(read2, {UserData,Size})`** - Reads using `UserData` but only `Size` bytes.
  Notice that there is only an arity-2 read function, not an arity-1 function.

- **`(position,{UserData,Position})`** - Sets the position of `UserData` as
  defined for files in `file:position/2`

_Example:_

The following is a complete `Fun` parameter for reading and writing on files
using the `m:file` module:

```erlang
ExampleFun =
   fun(write, {Fd,Data}) ->  file:write(Fd, Data);
      (position, {Fd,Pos}) -> file:position(Fd, Pos);
      (read2, {Fd,Size}) -> file:read(Fd, Size);
      (close, Fd) -> file:close(Fd)
   end
```

Here `Fd` was specified to function [`init/3`](`init/3`) as:

```erlang
{ok,Fd} = file:open(Name, ...).
{ok,TarDesc} = erl_tar:init(Fd, write, ExampleFun),
```

`TarDesc` is then used:

```erlang
erl_tar:add(TarDesc, SomeValueIwantToAdd, FileNameInTarFile),
...,
erl_tar:close(TarDesc)
```

When the `erl_tar` core wants to, for example, write a piece of `Data`, it would
call `ExampleFun(write, {UserData,Data})`.

> #### Note {: .info }
>
> This example with the `file` module operations is not necessary to use
> directly, as that is what function `open/2` in principle does.

> #### Warning {: .warning }
>
> The `TarDescriptor` term is not a file descriptor. You are advised not to rely
> on the specific contents of this term, as it can change in future Erlang/OTP
> releases when more features are added to this module.

# `open`

```erlang
-spec open(Open :: open_type(), [write | compressed | cooked]) ->
              {ok, tar_descriptor()} | {error, term()}.
```

Creates a tar file for writing (any existing file with the same name is
truncated).

By convention, the name of a tar file is to end in "`.tar`". To abide to the
convention, add "`.tar`" to the name.

Except for the `write` atom, the following atoms can be added to `OpenModeList`:

- **`compressed`** - The entire tar file is compressed, as if it has been run
  through the `gzip` program. To abide to the convention that a compressed tar
  file is to end in "`.tar.gz`" or "`.tgz`", add the appropriate extension.

- **`cooked`** - By default, the tar file is opened in `raw` mode, which is
  faster but does not allow a remote (Erlang) file server to be used. Adding
  `cooked` to the mode list overrides the default and opens the tar file without
  option `raw`.

To add one file at the time into an opened tar file, use function
[`add/3,4`](`add/3`). When you are finished adding files, use function `close/1`
to close the tar file.

> #### Warning {: .warning }
>
> The `compressed` and `cooked` flags are invalid when passing a file descriptor
> with `{file,Fd}`. The file must already be opened with the appropriate flags.

> #### Warning {: .warning }
>
> The `TarDescriptor` term is not a file descriptor. You are advised not to rely
> on the specific contents of this term, as it can change in future Erlang/OTP
> releases when more features are added to this module.

# `t`

```erlang
-spec t(file:filename()) -> ok | {error, term()}.
```

Prints the names of all files in the tar file `Name` to the Erlang shell
(similar to "`tar t`").

# `table`

```erlang
-spec table(Open :: open_type()) -> {ok, [name_in_archive()]} | {error, term()}.
```

# `table`

```erlang
-spec table(Open :: open_type(), [compressed | verbose | cooked]) ->
               {ok, [name_in_archive() | tar_entry()]} | {error, term()}.
```

Retrieves the names of all files in the tar file `Name`.

# `tt`

```erlang
-spec tt(open_type()) -> ok | {error, term()}.
```

Prints names and information about all files in the tar file `Name` to the
Erlang shell (similar to "`tar tv`").

---

*Consult [api-reference.md](api-reference.md) for complete listing*
