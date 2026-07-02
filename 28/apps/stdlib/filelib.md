# `filelib`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/filelib.erl#L22)

File utilities, such as wildcard matching of filenames.

This module contains utilities on a higher level than the `m:file` module.

This module does not support "raw" filenames (that is, files whose names do not
comply with the expected encoding). Such files are ignored by the functions in
this module.

For more information about raw filenames, see the `m:file` module.

> #### Note {: .info }
>
> Functionality in this module generally assumes valid input and does not
> necessarily fail on input that does not use a valid encoding, but may instead
> very likely produce invalid output.
>
> File operations used to accept filenames containing null characters (integer
> value zero). This caused the name to be truncated and in some cases arguments
> to primitive operations to be mixed up. Filenames containing null characters
> inside the filename are now _rejected_ and will cause primitive file
> operations to fail.

> #### Warning {: .warning }
>
> Currently null characters at the end of the filename will be accepted by
> primitive file operations. Such filenames are however still documented as
> invalid. The implementation will also change in the future and reject such
> filenames.

# `dirname`
*not exported* 

```elixir
-type dirname() :: filename().
```

# `dirname_all`
*not exported* 

```elixir
-type dirname_all() :: filename_all().
```

# `filename`
*not exported* 

```elixir
-type filename() :: file:name().
```

# `filename_all`
*not exported* 

```elixir
-type filename_all() :: file:name_all().
```

# `find_file_rule`
*not exported* 

```elixir
-type find_file_rule() :: {ObjDirSuffix :: string(), SrcDirSuffix :: string()}.
```

# `find_source_rule`
*not exported* 

```elixir
-type find_source_rule() :: {ObjExtension :: string(), SrcExtension :: string(), [find_file_rule()]}.
```

# `ensure_dir`

```elixir
-spec ensure_dir(Name) -> ok | {error, Reason}
                    when Name :: filename_all() | dirname_all(), Reason :: file:posix().
```

Ensures that all parent directories for the specified file or directory name
`Name` exist, trying to create them if necessary.

Returns `ok` if all parent directories already exist or can be created. Returns
`{error, Reason}` if some parent directory does not exist and cannot be created.

# `ensure_path`
*since OTP 25.0* 

```elixir
-spec ensure_path(Path) -> ok | {error, Reason} when Path :: dirname_all(), Reason :: file:posix().
```

Ensures that all parent directories for the specified path `Path` exist, trying
to create them if necessary.

Unlike `ensure_dir/1`, this function will attempt to create all path segments as
a directory, including the last segment.

Returns `ok` if all parent directories already exist or can be created. Returns
`{error, Reason}` if some parent directory does not exist and cannot be created.

# `file_size`

```elixir
-spec file_size(Filename) -> non_neg_integer() when Filename :: filename_all().
```

Returns the size of the specified file.

# `find_file`
*since OTP 20.0* 

```elixir
-spec find_file(Filename :: filename(), Dir :: filename()) -> {ok, filename()} | {error, not_found}.
```

# `find_file`
*since OTP 20.0* 

```elixir
-spec find_file(filename(), filename(), [find_file_rule()]) -> {ok, filename()} | {error, not_found}.
```

Looks for a file of the given name by applying suffix rules to the given
directory path.

For example, a rule `{"ebin", "src"}` means that if the directory path ends with
 `"ebin"`, the corresponding path ending in `"src"` should be searched.

If `Rules` is left out or is an empty list, the default system rules are used.
See also the Kernel application parameter
[`source_search_rules`](`e:kernel:kernel_app.md#source_search_rules`).

# `find_source`
*since OTP 20.0* 

```elixir
-spec find_source(filename()) -> {ok, filename()} | {error, not_found}.
```

Equivalent to [`find_source(Base, Dir)`](`find_source/2`), where `Dir` is
`filename:dirname(FilePath)` and `Base` is `filename:basename(FilePath)`.

# `find_source`
*since OTP 20.0* 

```elixir
-spec find_source(filename(), filename()) -> {ok, filename()} | {error, not_found}.
```

# `find_source`
*since OTP 20.0* 

```elixir
-spec find_source(filename(), filename(), [find_source_rule()]) -> {ok, filename()} | {error, not_found}.
```

Applies file extension specific rules to find the source file for a given object
file relative to the object directory.

For example, for a file with the extension `.beam`, the default rule is to look
for a file with a corresponding extension `.erl` by replacing the suffix `"ebin"`
of the object directory path with `"src"` or `"src/*"`. The file search is done
through `find_file/3`. The directory of the object file is always tried before
any other directory specified by the rules.

If `Rules` is left out or is an empty list, the default system rules are used.
See also the Kernel application parameter
[`source_search_rules`](`e:kernel:kernel_app.md#source_search_rules`).

# `fold_files`

```elixir
-spec fold_files(Dir, RegExp, Recursive, Fun, AccIn) -> AccOut
                    when
                        Dir :: dirname(),
                        RegExp :: string(),
                        Recursive :: boolean(),
                        Fun :: fun((F :: file:filename(), AccIn) -> AccOut),
                        AccIn :: term(),
                        AccOut :: term().
```

Folds function `Fun` over all (regular) files `F` in directory `Dir` whose
basename (for example, just `"baz.erl"` in `"foo/bar/baz.erl"`) matches the
regular expression `RegExp` (for a description of the allowed regular
expressions, see the `m:re` module).

If `Recursive` is `true`, all subdirectories to `Dir` are processed.
The regular expression matching is only done on the filename without the directory part.

If Unicode filename translation is in effect and the file system is transparent,
filenames that cannot be interpreted as Unicode can be encountered, in which
case the `fun()` must be prepared to handle raw filenames (that is, binaries).
If the regular expression contains codepoints > 255, it does not match filenames
that do not conform to the expected character encoding (that is, are not encoded
in valid UTF-8).

For more information about raw filenames, see the `m:file` module.

# `is_dir`

```elixir
-spec is_dir(Name) -> boolean() when Name :: filename_all() | dirname_all().
```

Returns `true` if `Name` refers to a directory, otherwise `false`.

# `is_file`

```elixir
-spec is_file(Name) -> boolean() when Name :: filename_all() | dirname_all().
```

Returns `true` if `Name` refers to a file or a directory, otherwise `false`.

# `is_regular`

```elixir
-spec is_regular(Name) -> boolean() when Name :: filename_all().
```

Returns `true` if `Name` refers to a (regular) file, otherwise `false`.

# `last_modified`

```elixir
-spec last_modified(Name) -> file:date_time() | 0 when Name :: filename_all() | dirname_all().
```

Returns the date and time the specified file or directory was last modified, or
`0` if the file does not exist.

# `safe_relative_path`
*since OTP 23.0* 

```elixir
-spec safe_relative_path(Filename, Cwd) -> unsafe | SafeFilename
                            when
                                Filename :: filename_all(),
                                Cwd :: filename_all(),
                                SafeFilename :: filename_all().
```

Sanitizes the relative path by eliminating ".." and "." components to protect
against directory traversal attacks.

Either returns the sanitized path name, or the atom `unsafe` if the path is unsafe.
The path is considered unsafe in the following circumstances:

- The path is not relative.
- A ".." component would climb up above the root of the relative path.
- A symbolic link in the path points above the root of the relative path.

_Examples:_

```erlang
1> {ok, Cwd} = file:get_cwd().
...
2> filelib:safe_relative_path("dir/sub_dir/..", Cwd).
"dir"
3> filelib:safe_relative_path("dir/..", Cwd).
[]
4> filelib:safe_relative_path("dir/../..", Cwd).
unsafe
5> filelib:safe_relative_path("/abs/path", Cwd).
unsafe
```

# `wildcard`

```elixir
-spec wildcard(Wildcard) -> [file:filename()] when Wildcard :: filename() | dirname().
```

Returns a list of all files that match Unix-style wildcard string `Wildcard`.

The wildcard string looks like an ordinary filename, except that the following
"wildcard characters" are interpreted in a special way:

- **?** - Matches one character.

- **\*** - Matches any number of characters up to the end of the filename, the
  next dot, or the next slash.

- **\*\*** - Two adjacent `*` used as a single pattern match all files and zero
  or more directories and subdirectories.

- **\[Character1,Character2,...]** - Matches any of the characters listed. Two
  characters separated by a hyphen match a range of characters. Example: `[A-Z]`
  matches any uppercase letter.

- **\{Item,...\}** - Alternation. Matches one of the alternatives.

Other characters represent themselves. Only filenames that have exactly the same
character in the same position match. Matching is case-sensitive, for example,
"a" does not match "A".

Directory separators must always be written as `/`, even on Windows.

A character preceded by `\` loses its special meaning. Note that `\` must be
written as `\\` in a string literal. For example, "\\\\?\*" will match any
filename starting with `?`.

Notice that multiple "\*" characters are allowed (as in Unix wildcards, but
opposed to Windows/DOS wildcards).

_Examples:_

The following examples assume that the current directory is the top of an
Erlang/OTP installation.

To find all `.beam` files in all applications, use the following line:

```text
filelib:wildcard("lib/*/ebin/*.beam").
```

To find `.erl` or `.hrl` in all applications `src` directories, use either of
the following lines:

```text
filelib:wildcard("lib/*/src/*.?rl")
```

```text
filelib:wildcard("lib/*/src/*.{erl,hrl}")
```

To find all `.hrl` files in `src` or `include` directories:

```text
filelib:wildcard("lib/*/{src,include}/*.hrl").
```

To find all `.erl` or `.hrl` files in either `src` or `include` directories:

```text
filelib:wildcard("lib/*/{src,include}/*.{erl,hrl}")
```

To find all `.erl` or `.hrl` files in any subdirectory:

```text
filelib:wildcard("lib/**/*.{erl,hrl}")
```

# `wildcard`

```elixir
-spec wildcard(Wildcard, Cwd) -> [file:filename()]
                  when Wildcard :: filename() | dirname(), Cwd :: dirname().
```

Same as `wildcard/1`, except that `Cwd` is used instead of the working
directory.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
