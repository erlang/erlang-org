# `zip`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/zip.erl#L22)

Utility for reading and creating 'zip' archives.

This module archives and extracts files to and from a zip archive. The zip
format is specified by the "ZIP Appnote.txt" file, available on the PKWARE web
site [www.pkware.com](http://www.pkware.com).

The zip module supports zip archive versions up to 6.1. However,
password-protection is not supported.

By convention, the name of a zip file is to end with `.zip`. To abide to the
convention, add `.zip` to the filename.

- To create zip archives, use function `zip/2` or `zip/3`. They are
  also available as [`create/2,3`](`create/3`), to resemble the `m:erl_tar` module.
- To extract files from a zip archive, use function `unzip/1` or `unzip/2`. They
  are also available as [`extract/1,2`](`extract/1`), to resemble the `m:erl_tar` module.
- To fold a function over all files in a zip archive, use function `foldl/3`.
- To return a list of the files in a zip archive, use function `list_dir/1` or
  `list_dir/2`. They are also available as [`table/1,2`](`table/1`), to resemble the
  `m:erl_tar` module.
- To print a list of files to the Erlang shell, use function `t/1` or `tt/1`.
- Sometimes it is desirable to open a zip archive, and to unzip files from it
  file by file, without having to reopen the archive. This can be done by
  functions [`zip_open/1,2`](`zip_open/1`), [`zip_get/1,2`](`zip_get/1`),
  `zip_list_dir/1`, and `zip_close/1`.
- The ZIP extensions 0x5355 "extended timestamps" and 0x7875 "UID+GID handling"
  are supported. Both extensions are by default enabled when creating an archive,
  but only "extended timestamps" are enabled when extracting. Use the `t:extra/0`
  option to change how these extensions are used.

## Limitations

- Password-protected and encrypted archives are not supported.
- Only the DEFLATE (zlib-compression) and the STORE (uncompressed data) zip
  methods are supported.
- Comments for individual files are not supported when creating zip archives.
  The zip archive comment for the whole zip archive is supported.
- Changing a zip archive is not supported. To add or remove a file from an
  archive, the whole archive must be recreated.

# `create_option`

```erlang
-type create_option() ::
          memory | cooked | verbose |
          {comment, Comment :: string()} |
          {cwd, CWD :: file:filename()} |
          {compress, What :: extension_spec()} |
          {uncompress, What :: extension_spec()} |
          {extra, extra()}.
```

These options are described in [`create/3`](`m:zip#zip_options`).

# `extension`
*not exported* 

```erlang
-type extension() :: string().
```

A filename extension, for example ".txt".

# `extension_spec`
*not exported* 

```erlang
-type extension_spec() ::
          all |
          [Extension :: extension()] |
          {add, [Extension :: extension()]} |
          {del, [Extension :: extension()]}.
```

# `extra`
*not exported* 

```erlang
-type extra() :: [extended_timestamp | uid_gid].
```

The possible extra extension that can be used.

- **`extended_timestamp`** - enables the 0x5455 "extended timestamps" zip extension
  that embeds POSIX timestamps for access and modification times for each file in the
  archive. This makes the timestamps to be in UTC instead of local time and also increases
  the time resolution from 2 seconds to 1 second.
- **`uid_gid`** - enables 0x7875 "UNIX 3rd generation" zip extension that embeds the
  UID and GID for each file into the archive.

# `filename`

```erlang
-type filename() :: file:filename().
```

The name of a zip file.

# `handle`

```erlang
-opaque handle()
```

As returned by `zip_open/2`.

# `zip_comment`
*not exported* 

```erlang
-type zip_comment() :: #zip_comment{comment :: string()}.
```

The record `zip_comment` only contains the archive comment for a zip archive.

# `zip_file`
*not exported* 

```erlang
-type zip_file() ::
          #zip_file{name :: string(),
                    info :: file:file_info(),
                    comment :: string(),
                    offset :: non_neg_integer(),
                    comp_size :: non_neg_integer()}.
```

The record `zip_file` contains the following fields:

- **`name`** - The filename

- **`info`** - File information as in `file:read_file_info/1` in Kernel.
  `mtime`, `atime` and `ctime` are expected to be
  in [`local time`](`erlang:localtime/0`) if represented using `t:calendar:datetime/0`,
  or in [OS system time](`e:erts:time_correction.md#os-system-time`) if represented by an integer.

- **`comment`** - The comment for the file in the zip archive

- **`offset`** - The file offset in the zip archive (used internally)

- **`comp_size`** - The size of the compressed file (the size of the
  uncompressed file is found in `info`)

# `foldl`
*since OTP R14B* 

```erlang
-spec foldl(Fun, Acc0, Archive) -> {ok, Acc1} | {error, Reason}
               when
                   Fun :: fun((FileInArchive, GetInfo, GetBin, AccIn) -> AccOut),
                   FileInArchive :: file:name(),
                   GetInfo :: fun(() -> file:file_info()),
                   GetBin :: fun(() -> binary()),
                   Acc0 :: term(),
                   Acc1 :: term(),
                   AccIn :: term(),
                   AccOut :: term(),
                   Archive :: file:name() | {file:name(), binary()},
                   Reason :: term().
```

Calls `Fun(FileInArchive, GetInfo, GetBin, AccIn)` on successive files in the
`Archive`, starting with `AccIn == Acc0`.

`FileInArchive` is the name that the file has in the archive.

`GetInfo` is a fun that returns information about the file.

`GetBin` returns the file contents.

Both `GetInfo` and `GetBin` must be called within the `Fun`. Their behavior is
undefined if they are called outside the context of `Fun`.

The `Fun` must return a new accumulator, which is passed to the next call.
[`foldl/3`](`foldl/3`) returns the final accumulator value. `Acc0` is returned
if the archive is empty. It is not necessary to iterate over all files in the
archive. The iteration can be ended prematurely in a controlled manner by
throwing an exception.

_Example:_

```erlang
> Name = "dummy.zip".
"dummy.zip"
> {ok, {Name, Bin}} = zip:create(Name, [{"foo", <<"FOO">>}, {"bar", <<"BAR">>}], [memory]).
{ok,{"dummy.zip",
     <<80,75,3,4,20,0,0,0,0,0,74,152,97,60,171,39,212,26,3,0,
       0,0,3,0,0,...>>}}
> {ok, FileSpec} = zip:foldl(fun(N, I, B, Acc) -> [{N, B(), I()} | Acc] end, [], {Name, Bin}).
{ok,[{"bar",<<"BAR">>,
      {file_info,3,regular,read_write,
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 54,1,0,0,0,0,0}},
     {"foo",<<"FOO">>,
      {file_info,3,regular,read_write,
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 54,1,0,0,0,0,0}}]}
> {ok, {Name, Bin}} = zip:create(Name, lists:reverse(FileSpec), [memory]).
{ok,{"dummy.zip",
     <<80,75,3,4,20,0,0,0,0,0,74,152,97,60,171,39,212,26,3,0,
       0,0,3,0,0,...>>}}
> catch zip:foldl(fun("foo", _, B, _) -> throw(B()); (_,_,_,Acc) -> Acc end, [], {Name, Bin}).
<<"FOO">>
```

# `list_dir`

```erlang
-spec list_dir(Archive) -> RetValue
                  when
                      Archive :: file:name() | binary(),
                      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
                      CommentAndFiles :: [zip_comment() | zip_file()].
```

# `list_dir`

```erlang
-spec list_dir(Archive, Options) -> RetValue
                  when
                      Archive :: file:name() | binary(),
                      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
                      CommentAndFiles :: [zip_comment() | zip_file()],
                      Options :: [Option],
                      Option :: cooked | skip_directories | {extra, extra()}.
```

Retrieves all filenames in the zip archive `Archive`.

The result value is the tuple `{ok, List}`, where `List` contains the zip
archive comment as the first element.

One option is available:

- **`cooked`** - By default, this function opens the zip file in `raw` mode,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without option `raw`.

- **`skip_directories`** - By default empty directories within zip archives are
  listed. With option `skip_directories` set, empty directories are no longer
  listed.

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default only "extended timestamps" is enabled when listing files.
  See `t:extra/0` for more details.

# `t`

```erlang
-spec t(Archive) -> ok when Archive :: file:name() | binary() | ZipHandle, ZipHandle :: handle().
```

Prints all filenames in the zip archive `Archive` to the Erlang shell. (Similar
to `tar t`.)

# `tt`

```erlang
-spec tt(Archive) -> ok when Archive :: file:name() | binary() | ZipHandle, ZipHandle :: handle().
```

Prints filenames and information about all files in the zip archive `Archive` to
the Erlang shell. (Similar to `tar tv`.)

# `unzip`

```erlang
-spec unzip(Archive) -> RetValue
               when
                   Archive :: file:name() | binary(),
                   RetValue ::
                       {ok, FileList} |
                       {ok, FileBinList} |
                       {error, Reason :: term()} |
                       {error, {Name :: file:name(), Reason :: term()}},
                   FileList :: [file:name()],
                   FileBinList :: [{file:name(), binary()}].
```

# `unzip`

```erlang
-spec unzip(Archive, Options) -> RetValue
               when
                   Archive :: file:name() | binary(),
                   Options :: [Option],
                   Option ::
                       {file_list, FileList} |
                       cooked | keep_old_files | verbose | memory | skip_directories |
                       {file_filter, FileFilter} |
                       {cwd, CWD} |
                       {extra, extra()},
                   FileList :: [file:name()],
                   FileBinList :: [{file:name(), binary()}],
                   FileFilter :: fun((ZipFile) -> boolean()),
                   CWD :: file:filename(),
                   ZipFile :: zip_file(),
                   RetValue ::
                       {ok, FileList} |
                       {ok, FileBinList} |
                       {error, Reason :: term()} |
                       {error, {Name :: file:name(), Reason :: term()}}.
```

Extracts all files from a zip archive.

If argument `Archive` is specified as a `t:binary/0`, the contents of the binary is
assumed to be a zip archive, otherwise a filename.

Options:

- **`{file_list, FileList}`** - By default, all files are extracted from the zip
  archive. With option `{file_list, FileList}`, function [`unzip/2`](`unzip/2`)
  only extracts the files whose names are included in `FileList`. The full
  paths, including the names of all subdirectories within the zip archive, must
  be specified.

- **`cooked`** - By default, this function opens the zip file in `raw` mode,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without option `raw`. The same applies for the files extracted.

- **`keep_old_files`** - By default, all files with the same name as files in
  the zip archive are overwritten. With option `keep_old_files` set, function
  [`unzip/2`](`unzip/2`) does not overwrite existing files. Notice that even
  with option `memory` specified, which means that no files are overwritten,
  existing files are excluded from the result.

- **`skip_directories`** - By default empty directories within zip archives are
  extracted. With option `skip_directories` set, empty directories are no longer
  created.

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default only "extended timestamps" is enabled when unzipping.
  See `t:extra/0` for more details.

- **`verbose`** - Prints an informational message for each extracted file.

- **`memory`** - Instead of extracting to the current directory, the result is
  given as a list of tuples `{Filename, Binary}`, where `Binary` is a binary
  containing the extracted data of file `Filename` in the zip archive.

- **`{cwd, CWD}`** - Uses the specified directory as current directory. It is
  prepended to filenames when extracting them from the zip archive. (Acting like
  `file:set_cwd/1` in Kernel, but without changing the global `cwd` property.)

# `zip`

```erlang
-spec zip(Name, FileList) -> RetValue
             when
                 Name :: file:name(),
                 FileList :: [FileSpec],
                 FileSpec ::
                     file:name() | {file:name(), binary()} | {file:name(), binary(), file:file_info()},
                 RetValue ::
                     {ok, FileName :: file:name()} |
                     {ok, {FileName :: file:name(), binary()}} |
                     {error, Reason :: term()}.
```

# `zip`

```erlang
-spec zip(Name, FileList, Options) -> RetValue
             when
                 Name :: file:name(),
                 FileList :: [FileSpec],
                 FileSpec ::
                     file:name() | {file:name(), binary()} | {file:name(), binary(), file:file_info()},
                 Options :: [Option],
                 Option :: create_option(),
                 RetValue ::
                     {ok, FileName :: file:name()} |
                     {ok, {FileName :: file:name(), binary()}} |
                     {error, Reason :: term()}.
```

Creates a zip archive containing the files specified in `FileList`.

`FileList` is a list of files, with paths relative to the current directory,
which are stored with this path in the archive. File system operations are
performed to read the file metadata and, when compression is enabled, to stream
the file contents without loading whole files into memory. Files can also be
specified as binaries to create an archive directly from data. In such cases, no
metadata or file system reads are performed.

Files are compressed using the DEFLATE compression, as described in the
"Appnote.txt" file. However, files are stored without compression if they are
already compressed. [`zip/2`](`zip/2`) and [`zip/3`](`zip/3`) check the file
extension to determine if the file is to be stored without compression. Files
with the following extensions are not compressed: `.Z`, `.zip`, `.zoo`, `.arc`,
`.lzh`, `.arj`.

It is possible to override the default behavior and control what types of files
that are to be compressed by using options `{compress, What}` and
`{uncompress, What}`. It is also possible to use many `compress` and
`uncompress` options.

To trigger file compression, its extension must match with the `compress`
condition and must not match the `uncompress` condition. For example, if
`compress` is set to `["gif", "jpg"]` and `uncompress` is set to `["jpg"]`, only
files with extension `"gif"` are compressed.

[](){: #zip_options }

Options:

- **`cooked`** - By default, this function opens the zip file in mode `raw`,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without the `raw` option. The same applies for the files added.

- **`verbose`** - Prints an informational message about each added file.

- **`memory`** - The output is not to a file, but instead as a tuple
  `{FileName, binary()}`. The binary is a full zip archive with header and can
  be extracted with, for example, `unzip/2`.

- **`{comment, Comment}`** - Adds a comment to the zip archive.

- **`{cwd, CWD}`** - Uses the specified directory as current work directory
  (`cwd`). This is prepended to filenames when adding them, although not in the
  zip archive (acting like `file:set_cwd/1` in Kernel, but without changing the
  global `cwd` property.).

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default both these "extra" features are enabled.
  See `t:extra/0` for more details.

- **`{compress, What}`** - Controls what types of files to be compressed.
  Defaults to `all`. The following values of `What` are allowed:

  - **`all`** - All files are compressed (as long as they pass the `uncompress`
    condition).

  - **`[Extension]`** - Only files with exactly these extensions are compressed.

  - **`{add,[Extension]}`** - Adds these extensions to the list of compress
    extensions.

  - **`{del,[Extension]}`** - Deletes these extensions from the list of compress
    extensions.

- **`{uncompress, What}`** - Controls what types of files to be uncompressed.
  Defaults to `[".Z", ".zip", ".zoo", ".arc", ".lzh", ".arj"]`. The following
  values of `What` are allowed:

  - **`all`** - No files are compressed.

  - **`[Extension]`** - Files with these extensions are uncompressed.

  - **`{add,[Extension]}`** - Adds these extensions to the list of uncompress
    extensions.

  - **`{del,[Extension]}`** - Deletes these extensions from the list of
    uncompress extensions.

# `zip_close`

```erlang
-spec zip_close(ZipHandle) -> ok | {error, einval} when ZipHandle :: handle().
```

Closes a zip archive, previously opened with [`zip_open/1,2`](`zip_open/1`). All
resources are closed, and the handle is not to be used after closing.

# `zip_get`

```erlang
-spec zip_get(ZipHandle) -> {ok, [Result]} | {error, Reason}
                 when
                     ZipHandle :: handle(),
                     Result :: file:name() | {file:name(), binary()},
                     Reason :: term().
```

# `zip_get`

```erlang
-spec zip_get(FileName, ZipHandle) -> {ok, Result} | {error, Reason}
                 when
                     FileName :: file:name(),
                     ZipHandle :: handle(),
                     Result :: file:name() | {file:name(), binary()},
                     Reason :: term().
```

Extracts one or all files from an open archive.

The files are unzipped to memory or to file, depending on the options specified
to function [`zip_open/1,2`](`zip_open/1`) when opening the archive.

# `zip_get_crc32`
*since OTP 26.0* 

```erlang
-spec zip_get_crc32(FileName, ZipHandle) -> {ok, CRC} | {error, Reason}
                       when
                           FileName :: file:name(),
                           ZipHandle :: handle(),
                           CRC :: non_neg_integer(),
                           Reason :: term().
```

Extracts one crc32 checksum from an open archive.

# `zip_list_dir`

```erlang
-spec zip_list_dir(ZipHandle) -> {ok, Result} | {error, Reason}
                      when
                          Result :: [zip_comment() | zip_file()],
                          ZipHandle :: handle(),
                          Reason :: term().
```

Returns the file list of an open zip archive. The first returned element is the
zip archive comment.

# `zip_open`

```erlang
-spec zip_open(Archive) -> {ok, ZipHandle} | {error, Reason}
                  when Archive :: file:name() | binary(), ZipHandle :: handle(), Reason :: term().
```

# `zip_open`

```erlang
-spec zip_open(Archive, Options) -> {ok, ZipHandle} | {error, Reason}
                  when
                      Archive :: file:name() | binary(),
                      ZipHandle :: handle(),
                      Options :: [Option],
                      Option :: cooked | memory | {cwd, CWD :: file:filename()} | {extra, extra()},
                      Reason :: term().
```

Opens a zip archive, and reads and saves its directory. This means that later
reading files from the archive is faster than unzipping files one at a time with
[`unzip/1,2`](`unzip/1`).

The options are equivalent to those in `unzip/2`.

The archive must be closed with `zip_close/1`.

The `ZipHandle` is closed if the process that originally opened the archive
dies.

# `create`

```erlang
-spec create(Name, FileList) -> RetValue
                when
                    Name :: file:name(),
                    FileList :: [FileSpec],
                    FileSpec ::
                        file:name() |
                        {file:name(), binary()} |
                        {file:name(), binary(), file:file_info()},
                    RetValue ::
                        {ok, FileName :: filename()} |
                        {ok, {FileName :: filename(), binary()}} |
                        {error, Reason :: term()}.
```

# `create`

```erlang
-spec create(Name, FileList, Options) -> RetValue
                when
                    Name :: file:name(),
                    FileList :: [FileSpec],
                    FileSpec ::
                        file:name() |
                        {file:name(), binary()} |
                        {file:name(), binary(), file:file_info()},
                    Options :: [Option],
                    Option :: create_option(),
                    RetValue ::
                        {ok, FileName :: filename()} |
                        {ok, {FileName :: filename(), binary()}} |
                        {error, Reason :: term()}.
```

# `extract`

```erlang
-spec extract(Archive) -> RetValue
                 when
                     Archive :: file:name() | binary(),
                     RetValue ::
                         {ok, FileList} |
                         {ok, FileBinList} |
                         {error, Reason :: term()} |
                         {error, {Name :: file:name(), Reason :: term()}},
                     FileList :: [file:name()],
                     FileBinList :: [{file:name(), binary()}].
```

# `extract`

```erlang
-spec extract(Archive, Options) -> RetValue
                 when
                     Archive :: file:name() | binary(),
                     Options :: [Option],
                     Option ::
                         {file_list, FileList} |
                         keep_old_files | verbose | memory |
                         {file_filter, FileFilter} |
                         {cwd, CWD},
                     FileList :: [file:name()],
                     FileBinList :: [{file:name(), binary()}],
                     FileFilter :: fun((ZipFile) -> boolean()),
                     CWD :: file:filename(),
                     ZipFile :: zip_file(),
                     RetValue ::
                         {ok, FileList} |
                         {ok, FileBinList} |
                         {error, Reason :: term()} |
                         {error, {Name :: file:name(), Reason :: term()}}.
```

# `table`

```erlang
-spec table(Archive) -> RetValue
               when
                   Archive :: file:name() | binary(),
                   RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
                   CommentAndFiles :: [zip_comment() | zip_file()].
```

# `table`

```erlang
-spec table(Archive, Options) -> RetValue
               when
                   Archive :: file:name() | binary(),
                   RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
                   CommentAndFiles :: [zip_comment() | zip_file()],
                   Options :: [Option],
                   Option :: cooked.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
