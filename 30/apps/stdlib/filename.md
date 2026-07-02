# `filename`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/filename.erl#L22)

Filename manipulation functions.

This module provides functions for analyzing and manipulating filenames. These
functions are designed so that the Erlang code can work on many different
platforms with different filename formats. With filename is meant all strings
that can be used to denote a file. The filename can be a short relative name
like `foo.erl`, a long absolute name including a drive designator, a directory
name like `D:\usr/local\bin\erl/lib\tools\foo.erl`, or any variations in
between.

In Windows, all functions return filenames with forward slashes only, even if
the arguments contain backslashes. To normalize a filename by removing redundant
directory separators, use `join/1`.

The module supports [raw filenames](unicode_usage.md#notes-about-raw-filenames)
in the way that if a binary is present, or the filename cannot be interpreted
according to the return value of `file:native_name_encoding/0`, a raw filename
is also returned. For example, [`join/1`](`join/1`) provided with a path
component that is a binary (and cannot be interpreted under the current native
filename encoding) results in a raw filename that is returned (the join
operation is performed of course). For more information about raw filenames, see
the `m:file` module.

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

# `basedir_opts`
*not exported* 

```erlang
-type basedir_opts() ::
          #{author => string() | binary(),
            os => windows | darwin | linux,
            version => string() | binary()}.
```

# `basedir_path_type`
*not exported* 

```erlang
-type basedir_path_type() :: user_cache | user_config | user_data | user_log.
```

# `basedir_paths_type`
*not exported* 

```erlang
-type basedir_paths_type() :: site_config | site_data.
```

# `absname`

```erlang
-spec absname(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Converts a relative `Filename` and returns an absolute name. No attempt is made
to create the shortest absolute name, as this can give incorrect results on file
systems that allow links.

_Unix examples:_

```erlang
1> pwd().
"/usr/local"
2> filename:absname("foo").
"/usr/local/foo"
3> filename:absname("../x").
"/usr/local/../x"
4> filename:absname("/").
"/"
```

_Windows examples:_

```erlang
1> pwd().
"D:/usr/local"
2> filename:absname("foo").
"D:/usr/local/foo"
3> filename:absname("../x").
"D:/usr/local/../x"
4> filename:absname("/").
"D:/"
```

# `absname`

```erlang
-spec absname(Filename, Dir) -> file:filename_all()
                 when Filename :: file:name_all(), Dir :: file:name_all().
```

Same as `absname/1`, except that the directory to which the filename is to be
made relative is specified in argument `Dir`.

# `absname_join`

```erlang
-spec absname_join(Dir, Filename) -> file:filename_all()
                      when Dir :: file:name_all(), Filename :: file:name_all().
```

Joins an absolute directory with a relative filename.

Similar to `join/2`, but on platforms with tight restrictions on raw filename length
and no support for symbolic links, leading parent directory components in `Filename` are matched
against trailing directory components in `Dir` so they can be removed from the
result - minimizing its length.

# `basedir`
*since OTP 19.0* 

```erlang
-spec basedir(PathType, Application) -> file:filename_all()
                 when PathType :: basedir_path_type(), Application :: string() | binary();
             (PathsType, Application) -> [file:filename_all()]
                 when PathsType :: basedir_paths_type(), Application :: string() | binary().
```

Equivalent to [basedir(PathType, Application, #\{\})](`basedir/3`)
or [basedir(PathsType, Application, #\{\})](`basedir/3`).

# `basedir`
*since OTP 19.0* 

```erlang
-spec basedir(PathType, Application, Opts) -> file:filename_all()
                 when
                     PathType :: basedir_path_type(),
                     Application :: string() | binary(),
                     Opts :: basedir_opts();
             (PathsType, Application, Opts) -> [file:filename_all()]
                 when
                     PathsType :: basedir_paths_type(),
                     Application :: string() | binary(),
                     Opts :: basedir_opts().
```

Returns a suitable path, or paths, for a given type.

If `os` is not set in `Opts` the function will default to the native option, that
is `'linux'`, `'darwin'` or `'windows'`, as understood by `os:type/0`.
Anything not recognized as `'darwin'` or `'windows'` is interpreted as `'linux'`.

The options `'author'` and `'version'` are only used with `'windows'` option
mode.

- `user_cache`{: #user_cache }

  The path location is intended for transient data files on a local machine.

  On Linux: Respects the os environment variable `XDG_CACHE_HOME`.

  ```erlang
  1> filename:basedir(user_cache, "my_application", #{os=>linux}).
  "/home/otptest/.cache/my_application"
  ```

  On Darwin:

  ```erlang
  1> filename:basedir(user_cache, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Caches/my_application"
  ```

  On Windows:

  ```erlang
  1> filename:basedir(user_cache, "My App").
  "c:/Users/otptest/AppData/Local/My App/Cache"
  2> filename:basedir(user_cache, "My App").
  "c:/Users/otptest/AppData/Local/My App/Cache"
  3> filename:basedir(user_cache, "My App", #{author=>"Erlang"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/Cache"
  4> filename:basedir(user_cache, "My App", #{version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/My App/1.2/Cache"
  5> filename:basedir(user_cache, "My App", #{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2/Cache"
  ```

- `user_config`{: #user_config }

  The path location is intended for persistent configuration files.

  On Linux: Respects the os environment variable `XDG_CONFIG_HOME`.

  ```erlang
  2> filename:basedir(user_config, "my_application", #{os=>linux}).
  "/home/otptest/.config/my_application"
  ```

  On Darwin:

  ```erlang
  2> filename:basedir(user_config, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Application Support/my_application"
  ```

  On Windows:

  ```erlang
  1> filename:basedir(user_config, "My App").
  "c:/Users/otptest/AppData/Roaming/My App"
  2> filename:basedir(user_config, "My App", #{author=>"Erlang", version=>"1.2"}).
  "c:/Users/otptest/AppData/Roaming/Erlang/My App/1.2"
  ```

- `user_data`{: #user_data }

  The path location is intended for persistent data files.

  On Linux: Respects the os environment variable `XDG_DATA_HOME`.

  ```erlang
  3> filename:basedir(user_data, "my_application", #{os=>linux}).
  "/home/otptest/.local/my_application"
  ```

  On Darwin:

  ```erlang
  3> filename:basedir(user_data, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Application Support/my_application"
  ```

  On Windows:

  ```erlang
  8> filename:basedir(user_data, "My App").
  "c:/Users/otptest/AppData/Local/My App"
  9> filename:basedir(user_data, "My App",#{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2"
  ```

- `user_log`{: #user_log }

  The path location is intended for transient log files on a local machine.

  On Linux: Respects the os environment variable `XDG_CACHE_HOME`.

  ```erlang
  4> filename:basedir(user_log, "my_application", #{os=>linux}).
  "/home/otptest/.cache/my_application/log"
  ```

  On Darwin:

  ```erlang
  4> filename:basedir(user_log, "my_application", #{os=>darwin}).
  "/home/otptest/Library/Logs/my_application"
  ```

  On Windows:

  ```erlang
  12> filename:basedir(user_log, "My App").
  "c:/Users/otptest/AppData/Local/My App/Logs"
  13> filename:basedir(user_log, "My App",#{author=>"Erlang",version=>"1.2"}).
  "c:/Users/otptest/AppData/Local/Erlang/My App/1.2/Logs"
  ```

- `site_config`{: #site_config }

  On Linux: Respects the os environment variable `XDG_CONFIG_DIRS`.

  ```erlang
  5> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/usr/local/share/my_application",
   "/usr/share/my_application"]
  6> os:getenv("XDG_CONFIG_DIRS").
  "/etc/xdg/xdg-ubuntu:/usr/share/upstart/xdg:/etc/xdg"
  7> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/etc/xdg/xdg-ubuntu/my_application",
   "/usr/share/upstart/xdg/my_application",
   "/etc/xdg/my_application"]
  8> os:unsetenv("XDG_CONFIG_DIRS").
  true
  9> filename:basedir(site_config, "my_application", #{os=>linux}).
  ["/etc/xdg/my_application"]
  ```

  On Darwin:

  ```erlang
  5> filename:basedir(site_config, "my_application", #{os=>darwin}).
  ["/Library/Application Support/my_application"]
  ```

- `site_data`{: #site_data }

  On Linux: Respects the os environment variable `XDG_DATA_DIRS`.

  ```erlang
  10> os:getenv("XDG_DATA_DIRS").
  "/usr/share/ubuntu:/usr/share/gnome:/usr/local/share/:/usr/share/"
  11> filename:basedir(site_data, "my_application", #{os=>linux}).
  ["/usr/share/ubuntu/my_application",
   "/usr/share/gnome/my_application",
   "/usr/local/share/my_application",
   "/usr/share/my_application"]
  12> os:unsetenv("XDG_DATA_DIRS").
  true
  13> filename:basedir(site_data, "my_application", #{os=>linux}).
  ["/usr/local/share/my_application",
   "/usr/share/my_application"]
  ```

  On Darwin:

  ```erlang
  5> filename:basedir(site_data, "my_application", #{os=>darwin}).
  ["/Library/Application Support/my_application"]
  ```

# `basename`

```erlang
-spec basename(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Returns the last component of `Filename`, or `Filename` itself if it does not
contain any directory separators.

_Examples:_

```erlang
5> filename:basename("foo").
"foo"
6> filename:basename("/usr/foo").
"foo"
7> filename:basename("/").
[]
```

# `basename`

```erlang
-spec basename(Filename, Ext) -> file:filename_all()
                  when Filename :: file:name_all(), Ext :: file:name_all().
```

Returns the last component of `Filename` with extension `Ext` stripped.

This function is to be used to remove a (possible) specific extension.
To remove an existing extension when you are unsure which one it is, use
[`rootname(basename(Filename))`](`rootname/1`).

_Examples:_

```erlang
8> filename:basename("~/src/kalle.erl", ".erl").
"kalle"
9> filename:basename("~/src/kalle.beam", ".erl").
"kalle.beam"
10> filename:basename("~/src/kalle.old.erl", ".erl").
"kalle.old"
11> filename:rootname(filename:basename("~/src/kalle.erl")).
"kalle"
12> filename:rootname(filename:basename("~/src/kalle.beam")).
"kalle"
```

# `dirname`

```erlang
-spec dirname(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Returns the directory part of `Filename`.

_Examples:_

```erlang
13> filename:dirname("/usr/src/kalle.erl").
"/usr/src"
14> filename:dirname("kalle.erl").
"."
```

```erlang
5> filename:dirname("\\usr\\src/kalle.erl"). % Windows
"/usr/src"
```

# `extension`

```erlang
-spec extension(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Returns the file extension of `Filename`, including the period. Returns an empty
string if no extension exists.

_Examples:_

```erlang
15> filename:extension("foo.erl").
".erl"
16> filename:extension("bork.src/kalle").
[]
```

# `flatten`

```erlang
-spec flatten(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Converts a possibly deep list filename consisting of characters and atoms into
the corresponding flat string filename.

# `join`

```erlang
-spec join(Components) -> file:filename_all() when Components :: [file:name_all()].
```

Joins a list of filename `Components` with directory separators. If one of the
elements of `Components` includes an absolute path, such as `"/xxx"`, the
preceding elements, if any, are removed from the result.

The result is "normalized":

- Redundant directory separators are removed.
- In Windows, all directory separators are forward slashes and the drive letter
  is in lower case.

_Examples:_

```erlang
17> filename:join(["/usr", "local", "bin"]).
"/usr/local/bin"
18> filename:join(["a/b///c/"]).
"a/b/c"
```

```erlang
6> filename:join(["B:a\\b///c/"]). % Windows
"b:a/b/c"
```

# `join`

```erlang
-spec join(Name1, Name2) -> file:filename_all() when Name1 :: file:name_all(), Name2 :: file:name_all().
```

Joins two filename components with directory separators. Equivalent to
[`join([Name1, Name2])`](`join/1`).

# `nativename`

```erlang
-spec nativename(Path) -> file:filename_all() when Path :: file:name_all().
```

Converts `Path` to a form accepted by the command shell and native applications
on the current platform. On Windows, forward slashes are converted to backward
slashes. On all platforms, the name is normalized as done by `join/1`.

_Examples:_

```erlang
19> filename:nativename("/usr/local/bin/"). % Unix
"/usr/local/bin"
```

```erlang
7> filename:nativename("/usr/local/bin/"). % Windows
"\\usr\\local\\bin"
```

# `pathtype`

```erlang
-spec pathtype(Path) -> absolute | relative | volumerelative when Path :: file:name_all().
```

Returns the path type, which is one of the following:

- **`absolute`** - The path name refers to a specific file on a specific volume.

  Unix example: `/usr/local/bin`

  Windows example: `D:/usr/local/bin`

- **`relative`** - The path name is relative to the current working directory on
  the current volume.

  Example: `foo/bar, ../src`

- **`volumerelative`** - The path name is relative to the current working
  directory on a specified volume, or it is a specific file on the current
  working volume.

  Windows example: `D:bar.erl, /bar/foo.erl`

# `rootname`

```erlang
-spec rootname(Filename) -> file:filename_all() when Filename :: file:name_all().
```

Removes the filename extension.

_Examples:_

```erlang
1> filename:rootname("/bork.src/kalle").
"/bork.src/kalle"
2> filename:rootname("/bork.src/foo.erl").
"/bork.src/foo"
```

# `rootname`

```erlang
-spec rootname(Filename, Ext) -> file:filename_all()
                  when Filename :: file:name_all(), Ext :: file:name_all().
```

Removes the filename extension `Ext` from `Filename`.

_Examples:_

```erlang
1> filename:rootname("/bork.src/foo.erl", ".erl").
"/bork.src/foo"
2> filename:rootname("/bork.src/foo.beam", ".erl").
"/bork.src/foo.beam"
```

# `split`

```erlang
-spec split(Filename) -> Components when Filename :: file:name_all(), Components :: [file:name_all()].
```

Returns a list whose elements are the path components of `Filename`.

_Examples:_

```erlang
24> filename:split("/usr/local/bin").
["/","usr","local","bin"]
25> filename:split("foo/bar").
["foo","bar"]
26> filename:split("a:\\msdev\\include").
["a:/","msdev","include"]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
