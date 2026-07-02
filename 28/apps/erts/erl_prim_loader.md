# `erl_prim_loader`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/erts/preloaded/src/erl_prim_loader.erl#L34)

The low-level Erlang loader. This module is used to load all Erlang modules into
the system. The start script is also fetched with this low-level loader.

`erl_prim_loader` knows about the environment and how to fetch modules.

Command-line flag `-loader Loader` can be used to choose the method used by
`erl_prim_loader`. Two `Loader` methods are supported by the Erlang runtime
system: `efile` and `inet`.

## Command-Line Flags

The `erl_prim_loader` module interprets the following command-line flags:

- **`-loader Loader`** - Specifies the name of the loader used by
  `erl_prim_loader`. `Loader` can be `efile` (use the local file system) or
  `inet` (load using the `boot_server` on another Erlang node).

  If flag `-loader` is omitted, it defaults to `efile`.

- **`-loader_debug`** - Makes the `efile` loader write some debug information,
  such as the reason for failures, while it handles files.

- **`-hosts Hosts`** - Specifies which other Erlang nodes the `inet` loader can
  use. This flag is mandatory if flag `-loader inet` is present. On each host,
  there must be on Erlang node with the `m:erl_boot_server`, which handles the
  load requests. `Hosts` is a list of IP addresses (hostnames are not
  acceptable).

- **`-setcookie Cookie`** - Specifies the cookie of the Erlang runtime system.
  This flag is mandatory if flag `-loader inet` is present.

## See Also

`m:init`, `m:erl_boot_server`

# `get_file`

```elixir
-spec get_file(Filename) -> {ok, Bin, FullName} | error
                  when Filename :: atom() | string(), Bin :: binary(), FullName :: string().
```

_Use of this function is deprecated in favor of [`read_file/1`](`read_file/1`)._

Fetches a file using the low-level loader. `Filename` is either an absolute
filename or only the name of the file, for example, `"lists.beam"`. If an
internal path is set to the loader, this path is used to find the file.
`FullName` is the complete name of the fetched file. `Bin` is the contents of
the file as a binary.

`Filename` can also be a file in an archive, for example,
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin/mnesia.beam`. For
information about archive files, see `m:code`.

# `get_path`

```elixir
-spec get_path() -> {ok, Path} when Path :: [Dir :: string()].
```

_Use of this function is deprecated in favor of `code:get_path/0`._

Gets the path set in the loader. The path is set by the `m:init` process
according to information found in the start script.

# `list_dir`

```elixir
-spec list_dir(Dir) -> {ok, Filenames} | error when Dir :: string(), Filenames :: [Filename :: string()].
```

Lists all the files in a directory.

Returns `{ok, Filenames}` if successful, otherwise `error`. `Filenames`
is a list of the names of all the files in the directory. The names are
not sorted.

`Dir` can also be a directory in an archive, for example,
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin`. For information about
archive files, see `m:code`.

# `read_file`
*since OTP 27.0* 

```elixir
-spec read_file(Filename) -> {ok, Bin} | error when Filename :: string(), Bin :: binary().
```

Reads a file using the low-level loader.

Returns `{ok, Bin}` if successful, otherwise `error`. `Bin` is the contents
of the file as a binary.

`Filename` can also be a file in an archive, for example,
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin/mnesia.beam`. For
information about archive files, see `m:code`.

# `read_file_info`

```elixir
-spec read_file_info(Filename) -> {ok, FileInfo} | error
                        when Filename :: string(), FileInfo :: file:file_info().
```

Retrieves information about a file.

Returns `{ok, FileInfo}` if successful, otherwise `error`. `FileInfo` is a
record [`file_info`](`t:file:file_info/0`), defined in the Kernel include file
 `file.hrl`. Include the following directive in the module from which the
function is called:

```erlang
-include_lib("kernel/include/file.hrl").
```

For more information about the record see `file:read_file_info/2`.

`Filename` can also be a file in an archive, for example,
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin/mnesia`. For information
about archive files, see `m:code`.

# `read_link_info`
*since OTP 17.1.2* 

```elixir
-spec read_link_info(Filename) -> {ok, FileInfo} | error
                        when Filename :: string(), FileInfo :: file:file_info().
```

Works like `read_file_info/1` except that if `Filename` is a symbolic link,
information about the link is returned in the [`file_info`](`t:file:file_info/0`)
record and the `type` field of the record is set to `symlink`.

If `Filename` is not a symbolic link, this function returns exactly the same
result as [`read_file_info/1`](`read_file_info/1`). On platforms that do not
support symbolic links, this function is always equivalent to
[`read_file_info/1`](`read_file_info/1`).

# `set_path`

```elixir
-spec set_path(Path) -> ok when Path :: [Dir :: string()].
```

Sets the path of the loader if `m:init` interprets a `path` command in the start
script.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
