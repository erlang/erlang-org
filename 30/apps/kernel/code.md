# `code`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/code.erl#L22)

Interface to the Erlang code server process.

This module contains the interface to the Erlang _code server_, which deals with
the loading of compiled code into a running Erlang runtime system.

The runtime system can be started in _interactive_ or _embedded_ mode. Which one
is decided by the command-line flag `-mode`:

```bash
% erl -mode embedded
```

The modes are as follows:

- In _interactive_ mode, which is default, only the modules needed by
  the runtime system are loaded during system startup. Other code is
  dynamically loaded when first referenced. When a call to a function
  in a certain module is made, and that module is not loaded, the code
  server searches for and tries to load that module.

- In _embedded_ mode, modules are not auto-loaded. Trying to use a
  module that has not been loaded results in an error. This mode is
  recommended when the boot script loads all modules, as it is
  typically done in OTP releases. (Code can still be loaded later by
  explicitly ordering the code server to do so).

To prevent accidentally reloading of modules affecting the Erlang runtime
system, directories `kernel`, `stdlib`, and `compiler` are considered _sticky_.
This means that the system issues a warning and rejects the request if a user
tries to reload a module residing in any of them. The feature can be disabled by
using command-line flag `-nostick`.

## Code Path

In interactive mode, the code server maintains a _code path_,
consisting of a list of directories, which it searches sequentially
when trying to load a module.

Initially, the code path consists of the current working directory and all
Erlang object code directories under library directory `$OTPROOT/lib`, where
`$OTPROOT` is the installation directory of Erlang/OTP, `code:root_dir()`.
Directories can be named `Name[-Vsn]` and the code server, by default, chooses
the directory with the highest version number among those having the same
`Name`. Suffix `-Vsn` is optional. If an `ebin` directory exists under
`Name[-Vsn]`, this directory is added to the code path.

Environment variable `ERL_LIBS` (defined in the operating system) can be used to
define more library directories to be handled in the same way as the standard
OTP library directory described above, except that directories without an `ebin`
directory are ignored.

All application directories found in the additional directories appear before
the standard OTP applications, except for the Kernel and STDLIB applications,
which are placed before any additional applications. In other words, modules
found in any of the additional library directories override modules with the
same name in OTP, except for modules in Kernel and STDLIB.

Environment variable `ERL_LIBS` (if defined) is to contain a colon-separated
(for Unix-like systems) or semicolon-separated (for Windows) list of additional
libraries.

_Example:_

On a Unix-like system, `ERL_LIBS` can be set to the following:

```text
/usr/local/jungerl:/home/some_user/my_erlang_lib
```

The code paths specified by `$OTPROOT`, `ERL_LIBS`, and boot scripts have their
listings cached by default (except for `"."`) The code server will
lookup the contents in their directories once and avoid future file system
traversals. Therefore, modules added to such directories after the Erlang VM
boots will not be picked up. This behaviour can be disabled by setting
`-cache_boot_paths false` or by calling `code:set_path(code:get_path())`.

> #### Change {: .info }
>
> The support for caching directories in the code path was added
> in Erlang/OTP 26.

Directories given by the command line options `-pa` and `-pz` are not
cached by default. Many of the functions that manipulate the code path
accept the `cache` atom as an optional argument to enable caching
selectively.

## Loading of Code From Archive Files

> #### Change {: .info }
>
> The existing experimental support for archive files will be changed
> in a future release. As of Erlang/OTP 27, the function `code:lib_dir/2`,
> the `-code_path_choice` flag, and using `m:erl_prim_loader` for
> reading files from an archive are deprecated.
>
> `escript` scripts that use archive files should use
> `escript:extract/2` to read data files from its archive instead of using
> `code:lib_dir/2` and `m:erl_prim_loader`.

The Erlang archives are `ZIP` files with extension `.ez`. Erlang archives can
also be [enclosed in `escript`](`m:escript`) files whose file extension is arbitrary.

Erlang archive files can contain entire Erlang applications or parts of
applications. The structure in an archive file is the same as the directory
structure for an application. If you, for example, create an archive of
`mnesia-4.4.7`, the archive file must be named `mnesia-4.4.7.ez` and it must
contain a top directory named `mnesia-4.4.7`. If the version part of the name is
omitted, it must also be omitted in the archive. That is, a `mnesia.ez` archive
must contain a `mnesia` top directory.

An archive file for an application can, for example, be created like this:

```erlang
zip:create("mnesia-4.4.7.ez",
	["mnesia-4.4.7"],
	[{cwd, code:lib_dir()},
	 {compress, all},
	 {uncompress,[".beam",".app"]}]).
```

Any file in the archive can be compressed, but to speed up the access of
frequently read files, it can be a good idea to store `beam` and `app` files
uncompressed in the archive.

Normally the top directory of an application is located in library directory
`$OTPROOT/lib` or in a directory referred to by environment variable `ERL_LIBS`.
At startup, when the initial code path is computed, the code server also looks
for archive files in these directories and possibly adds `ebin` directories in
archives to the code path. The code path then contains paths to directories that
look like `$OTPROOT/lib/mnesia.ez/mnesia/ebin` or
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin`.

The code server uses module `erl_prim_loader` in ERTS (possibly through
`erl_boot_server`) to read code files from archives. However, the functions in
`erl_prim_loader` can also be used by other applications to read files from
archives. For example, the call
`erl_prim_loader:list_dir( "/otp/root/lib/mnesia-4.4.7.ez/mnesia-4.4.7/examples/bench)"`
would list the contents of a directory inside an archive. See
`m:erl_prim_loader`.

An application archive file and a regular application directory can coexist.
This can be useful when it is needed to have parts of the application as regular
files. A typical case is the `priv` directory, which must reside as a regular
directory to link in drivers dynamically and start port programs. For other
applications that do not need this, directory `priv` can reside in the archive
and the files under the directory `priv` can be read through `erl_prim_loader`.

When a directory is added to the code path and when the entire code path is
(re)set, the code server decides which subdirectories in an application that are
to be read from the archive and which that are to be read as regular files. If
directories are added or removed afterwards, the file access can fail if the
code path is not updated (possibly to the same path as before, to trigger the
directory resolution update).

For each directory on the second level in the application archive (`ebin`,
`priv`, `src`, and so on), the code server first chooses the regular directory
if it exists and second from the archive. Function `code:lib_dir/2` returns the
path to the subdirectory. For example, `code:lib_dir(megaco, ebin)` can return
`/otp/root/lib/megaco-3.9.1.1.ez/megaco-3.9.1.1/ebin` while
`code:lib_dir(megaco, priv)` can return `/otp/root/lib/megaco-3.9.1.1/priv`.

When an `escript` file contains an archive, there are no restrictions on the
name of the `escript` and no restrictions on how many applications that can be
stored in the embedded archive. Single Beam files can also reside on the top
level in the archive. At startup, the top directory in the embedded archive and
all (second level) `ebin` directories in the embedded archive are added to the
code path. See [`escript`](`e:erts:escript_cmd.md`).

A future-proof way for `escript` scripts to read data files from the archive is
to use the `escript:extract/2` function.

When the choice of directories in the code path is `strict` (which is
the default as of Erlang/OTP 27), the directory that ends up in the
code path is exactly the stated one. This means that if, for example,
the directory `$OTPROOT/lib/mnesia-4.4.7/ebin` is explicitly added to
the code path, the code server does not load files from
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin`.

This behavior can be controlled through command-line flag
`-code_path_choice Choice`. If the flag is set to `relaxed`, the code server
instead chooses a suitable directory depending on the actual file structure. If
a regular application `ebin` directory exists, it is chosen. Otherwise, the
directory `ebin` in the archive is chosen if it exists. If neither of them
exists, the original directory is chosen.

Command-line flag `-code_path_choice Choice` also affects how module `init`
interprets the `boot script`. The interpretation of the explicit code paths in
the `boot script` can be `strict` or `relaxed`. It is particularly useful to set
the flag to `relaxed` when elaborating with code loading from archives without
editing the `boot script`. The default has changed to `strict` in OTP 27 and the
option is scheduled for removal in OTP 28. See module `m:init` in the
Erts application.

## Current and Old Code

The code for a module can exist in two variants in a system: _current code_ and
_old code_. When a module is loaded into the system for the first time, the
module code becomes *current* and the global _export table_ is updated with
references to all functions exported from the module.

When a new instance of the module is loaded, the code of the previous
instance becomes *old*, and all export entries referring to the
previous instance are removed. After that, the new instance is loaded
as for the first time, and becomes current.

Both old and current code for a module are valid, and can even be executed
concurrently. The difference is that exported functions in old code are
unavailable. Hence, a global call cannot be made to an exported function in old
code, but old code can still be executed because of processes lingering in it.

If a third instance of the module is loaded, the code server removes (purges)
the old code and any processes lingering in it are terminated. Then the third
instance becomes current and the previously current code becomes old.

For more information about old and current code, and how to make a process
switch from old to current code, see section Compilation and Code Loading in the
[Erlang Reference Manual](`e:system:code_loading.md`).

## Native Coverage Support

In runtime systems that use the JIT, native coverage is a light-weight
way to find out which functions or lines that have been executed, or
how many times each function or line has been executed.

> #### Change {: .info }
>
> The support for native coverage was added in Erlang/OTP 27.

Native coverage works by instrumenting code at load-time. When a
module has been instrumented for native coverage collection it is not
possible to later disable the coverage collection, except by reloading
the module. However, the overhead for keeping coverage collection
running is often neligible, especially for [coverage
mode](`t:coverage_mode/0`) `function` that only keeps track of which
functions that have been executed.

The `m:cover` tool in the Tools application will automatically use the
native coverage support if the runtime system supports it.

It is only necessary to use the functionality described next if
`m:cover` is not sufficient, for example:

* If one wants to collect coverage information for the code that runs
  when the runtime system is starting (module `m:init` and so on).
  `m:cover` can only be used when the Erlang system has started, and
  it will reload every module that is to be analyzed.

* If it is necessary to collect coverage information with the absolute
  minimum disturbance of the test system. `m:cover` always counts how
  many times each line is executed (coverage mode `line_counters`),
  but by using native coverage one can use a less expensive coverage
  mode such as `function`, which has almost negligible overhead.

### Short summary of using native coverage

If the `line` or `line_counters` coverage mode is to be used,
the code to be tested must be compiled with option
[`line_coverage`](`e:compiler:compile#line_coverage`).

Use [set_coverage_mode(Mode)](`set_coverage_mode/1`) to set a
[coverage mode](`t:coverage_mode/0`) for all code subsequently
loaded, or set it with option [\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`)
for `erl`.

Optionally reset coverage information for all
modules that are to be tested by calling
[reset_coverage(Module)](`reset_coverage/1`).

Run the code whose coverage information is to be collected.

Read out the counters for all interesting modules by calling
[get_coverage(Level, Module)](`get_coverage/2`), where `Level`
is either `function` or `line`.

### The other native coverage BIFs

The following BIFs are sometimes useful, for example to fail gracefully
if the runtime system does not support native coverage:

* [coverage_support()](`coverage_support/0`) - check whether
  the runtime system supports native coverage

* [get_coverage_mode()](`get_coverage_mode/0`) - get the current
  coverage mode

* [get_coverage_mode(Module)](`get_coverage_mode/1`) - get the coverage
  mode for module `Module`

## Argument Types and Invalid Arguments

Module and application names are atoms, while file and directory names are
strings. For backward compatibility reasons, some functions accept both strings
and atoms, but a future release will probably only allow the arguments that are
documented.

Functions in this module generally fail with an exception if they are passed an
incorrect type (for example, an integer or a tuple where an atom is expected).
An error tuple is returned if the argument type is correct, but there are some
other errors (for example, a non-existing directory is specified to
[`set_path/1`](`set_path/1`)).

[](){: #error_reasons }

## Error Reasons for Code-Loading Functions

Functions that load code (such as [`load_file/1`](`load_file/1`)) will return
`{error,Reason}` if the load operation fails. Here follows a description of the
common reasons.

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code was found.

- **`not_purged`** - The object code could not be loaded because an old version
  of the code already existed.

- **`on_load_failure`** - The module has an
  [\-on_load function](`e:system:code_loading.md#on_load`) that failed when it
  was called.

- **`sticky_directory`** - The object code resides in a sticky directory.

# `add_path_ret`
*not exported* 

```erlang
-type add_path_ret() :: true | {error, bad_directory}.
```

# `cache`
*not exported* 

```erlang
-type cache() :: cache | nocache.
```

# `coverage_mode`

```erlang
-type coverage_mode() :: none | function | function_counters | line | line_counters.
```

# `debug_atom_or_var`

```erlang
-nominal debug_atom_or_var() :: atom() | debug_var().
```

# `debug_call`

```erlang
-nominal debug_call() ::
             MFA ::
                 {debug_atom_or_var(), debug_atom_or_var(), arity()} |
                 (FA :: {atom(), arity()} | debug_var()).
```

# `debug_frame`

```erlang
-nominal debug_frame() :: non_neg_integer() | entry | none.
```

# `debug_info`

```erlang
-nominal debug_info() ::
             [{debug_line(),
               #{frame_size => debug_frame(), vars => [debug_value()], calls => [debug_call()]}}].
```

# `debug_line`

```erlang
-nominal debug_line() :: pos_integer().
```

# `debug_name`

```erlang
-nominal debug_name() :: debug_var() | 1..255.
```

# `debug_source`

```erlang
-nominal debug_source() :: {x, non_neg_integer()} | {y, non_neg_integer()} | {value, _}.
```

# `debug_value`

```erlang
-nominal debug_value() :: {debug_name(), debug_source()}.
```

# `debug_var`

```erlang
-nominal debug_var() :: binary().
```

# `load_error_rsn`

```erlang
-type load_error_rsn() :: badfile | nofile | not_purged | on_load_failure | sticky_directory.
```

# `load_ret`

```erlang
-type load_ret() :: {error, What :: load_error_rsn()} | {module, Module :: module()}.
```

# `loaded_filename`
*not exported* 

```erlang
-type loaded_filename() :: (Filename :: file:filename()) | loaded_ret_atoms().
```

# `loaded_ret_atoms`
*not exported* 

```erlang
-type loaded_ret_atoms() :: cover_compiled | preloaded.
```

# `module_status`

```erlang
-type module_status() :: not_loaded | loaded | modified | removed.
```

# `prepared_code`

```erlang
-opaque prepared_code()
```

An opaque term holding prepared code.

# `replace_path_ret`
*not exported* 

```erlang
-type replace_path_ret() :: true | {error, bad_directory | bad_name | {badarg, _}}.
```

# `set_path_ret`
*not exported* 

```erlang
-type set_path_ret() :: true | {error, bad_directory}.
```

# `add_path`

```erlang
-spec add_path(Dir) -> add_path_ret() when Dir :: file:filename().
```

# `add_path`
*since OTP 26.0* 

```erlang
-spec add_path(Dir, cache()) -> add_path_ret() when Dir :: file:filename().
```

# `add_patha`

```erlang
-spec add_patha(Dir) -> add_path_ret() when Dir :: file:filename().
```

# `add_patha`
*since OTP 26.0* 

```erlang
-spec add_patha(Dir, cache()) -> add_path_ret() when Dir :: file:filename().
```

Adds `Dir` to the beginning of the code path.

If `Dir` exists, it is removed from the old position in the code path.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns `true` if successful, or `{error, bad_directory}` if `Dir` is
not the name of a directory.

# `add_paths`

```erlang
-spec add_paths(Dirs) -> ok when Dirs :: [Dir :: file:filename()].
```

# `add_paths`
*since OTP 26.0* 

```erlang
-spec add_paths(Dirs, cache()) -> ok when Dirs :: [Dir :: file:filename()].
```

# `add_pathsa`

```erlang
-spec add_pathsa(Dirs) -> ok when Dirs :: [Dir :: file:filename()].
```

# `add_pathsa`
*since OTP 26.0* 

```erlang
-spec add_pathsa(Dirs, cache()) -> ok when Dirs :: [Dir :: file:filename()].
```

Traverses `Dirs` and adds each `Dir` to the beginning of the code path.

This means that the order of `Dirs` is reversed in the resulting code
path. For example, if `Dirs` is `[Dir1,Dir2]`, the resulting path will
be `[Dir2,Dir1|OldCodePath]`.

If a `Dir` already exists in the code path, it is removed from the old position.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Always returns `ok`, regardless of the validity of each individual `Dir`.

# `add_pathsz`

```erlang
-spec add_pathsz(Dirs) -> ok when Dirs :: [Dir :: file:filename()].
```

# `add_pathsz`
*since OTP 26.0* 

```erlang
-spec add_pathsz(Dirs, cache()) -> ok when Dirs :: [Dir :: file:filename()].
```

Adds the directories in `Dirs` to the end of the code path.

Directories that are already present in the path will not be added.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Always returns `ok`, regardless of the validity of each individual `Dir`.

# `add_pathz`

```erlang
-spec add_pathz(Dir) -> add_path_ret() when Dir :: file:filename().
```

# `add_pathz`
*since OTP 26.0* 

```erlang
-spec add_pathz(Dir, cache()) -> add_path_ret() when Dir :: file:filename().
```

Adds `Dir` as the directory last in the code path.

If `Dir` already exists in the path, it is not added.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns `true` if successful, or `{error, bad_directory}` if `Dir` is
not the name of a directory.

# `all_available`
*since OTP 23.0* 

```erlang
-spec all_available() -> [{Module, Filename, Loaded}]
                       when Module :: string(), Filename :: loaded_filename(), Loaded :: boolean().
```

Returns a list of tuples `{Module, Filename, Loaded}` for all available modules.

A module is considered to be available if it either is loaded or would be loaded
if called. `Filename` is normally the absolute filename, as described for
`is_loaded/1`.

# `all_loaded`

```erlang
-spec all_loaded() -> [{Module, Loaded}] when Module :: module(), Loaded :: loaded_filename().
```

Returns a list of tuples `{Module, Loaded}` for all loaded modules.

`Loaded` is normally the absolute filename, as described for `is_loaded/1`.

# `atomic_load`
*since OTP 19.0* 

```erlang
-spec atomic_load(Modules) -> ok | {error, [{Module, What}]}
                     when
                         Modules :: [Module | {Module, Filename, Binary}],
                         Module :: module(),
                         Filename :: file:filename(),
                         Binary :: binary(),
                         What ::
                             badfile | nofile | on_load_not_allowed | duplicated | not_purged |
                             sticky_directory | pending_on_load.
```

Tries to load all of the modules in the list `Modules` atomically.

That means that either all modules are loaded at the same time, or
none of the modules are loaded if there is a problem with any of the
modules.

Loading can fail for one the following reasons:

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code exists.

- **`on_load_not_allowed`** - A module contains an
  [\-on_load function](`e:system:code_loading.md#on_load`).

- **`duplicated`** - A module is included more than once in `Modules`.

- **`not_purged`** - The object code cannot be loaded because an old version of
  the code already exists.

- **`sticky_directory`** - The object code resides in a sticky directory.

- **`pending_on_load`** - A previously loaded module contains an `-on_load`
  function that never finished.

If it is important to minimize the time that an application is inactive while
changing code, use `prepare_loading/1` and `finish_loading/1` instead of
[`atomic_load/1`](`atomic_load/1`). Here is an example:

```erlang
{ok,Prepared} = code:prepare_loading(Modules),
%% Put the application into an inactive state or do any
%% other preparation needed before changing the code.
ok = code:finish_loading(Prepared),
%% Resume the application.
```

# `clash`

```erlang
-spec clash() -> ok.
```

Searches all directories in the code path for module names with identical names
and writes a report to `stdout`.

# `clear_cache`
*since OTP 26.0* 

```erlang
-spec clear_cache() -> ok.
```

Clears the code path cache.

If a directory is cached, its cache is cleared once and then it will
be recalculated and cached once more in a future traversal.

To clear the cache for a single path, either re-add it to the code
path (with [`add_path/2`](`add_path/2`)) or replace it (with
[`replace_path/3`](`replace_path/3`)). To disable all caching, reset
the code path with `code:set_path(code:get_path())`.

Always returns `ok`.

# `compiler_dir`

```erlang
-spec compiler_dir() -> file:filename().
```

Returns the compiler library directory.

Equivalent to [`code:lib_dir(compiler)`](`code:lib_dir/1`).

# `coverage_support`
*since OTP 27.0* 

```erlang
-spec coverage_support() -> Supported when Supported :: boolean().
```

Returns `true` if the system supports coverage and `false` otherwise.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `del_path`

```erlang
-spec del_path(NameOrDir) -> boolean() | {error, What}
                  when NameOrDir :: Name | Dir, Name :: atom(), Dir :: file:filename(), What :: bad_name.
```

Deletes a directory from the code path.

The argument can be an atom `Name`, in which case the directory with
the name `.../Name[-Vsn][/ebin]` is deleted from the code path. Also,
the complete directory name `Dir` can be specified as argument.

Returns:

- **`true`** - If successful

- **`false`** - If the directory is not found

- **`{error, bad_name}`** - If the argument is invalid

# `del_paths`
*since OTP 26.0* 

```erlang
-spec del_paths(NamesOrDirs) -> ok
                   when NamesOrDirs :: [Name | Dir], Name :: atom(), Dir :: file:filename().
```

Deletes directories from the code path.

The argument is a list of either atoms or complete directory names. If
`Name` is an atom, the directory with the name `.../Name[-Vsn][/ebin]` is
deleted from the code path.

Always returns `ok`, regardless of the validity of each individual
`NamesOrDirs`.

# `delete`

```erlang
-spec delete(Module) -> boolean() when Module :: module().
```

Removes the current code for `Module`, that is, the current code for `Module` is
made old.

This means that processes can continue to execute the code in the
module, but no external function calls can be made to it.

Returns `true` if successful, or `false` if there is old code for `Module` that
must be purged first, or if `Module` is not a (loaded) module.

# `ensure_loaded`

```erlang
-spec ensure_loaded(Module) -> {module, Module} | {error, What}
                       when Module :: module(), What :: embedded | badfile | nofile | on_load_failure.
```

Tries to load a module in the same way as `load_file/1`, unless the module is
already loaded.

If called concurrently, this function ensures that only one process
attempts to load said module at a given time.

In embedded mode, it does not load a module that is not already loaded, but
returns `{error, embedded}` instead. See
[Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of other possible error reasons.

# `ensure_modules_loaded`
*since OTP 19.0* 

```erlang
-spec ensure_modules_loaded([Module]) -> ok | {error, [{Module, What}]}
                               when Module :: module(), What :: badfile | nofile | on_load_failure.
```

Tries to load any modules not already loaded in the list `Modules` in the same
way as `load_file/1`.

Unlike `ensure_loaded/1`, modules are loaded even in `embedded` mode.

Returns `ok` if successful, or `{error,[{Module,Reason}]}` if loading of some
modules fails. See
[Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of other possible error reasons.

# `finish_loading`
*since OTP 19.0* 

```erlang
-spec finish_loading(Prepared) -> ok | {error, [{Module, What}]}
                        when
                            Prepared :: prepared_code(),
                            Module :: module(),
                            What :: not_purged | sticky_directory | pending_on_load.
```

Tries to load code for all modules that have been previously prepared by
`prepare_loading/1`.

The loading occurs atomically, meaning that either all modules are
loaded at the same time, or none of the modules are loaded.

This function can fail with one of the following error reasons:

- **`not_purged`** - The object code cannot be loaded because an old version of
  the code already exists.

- **`sticky_directory`** - The object code resides in a sticky directory.

- **`pending_on_load`** - A previously loaded module contains an `-on_load`
  function that never finished.

# `get_coverage`
*since OTP 27.0* 

```erlang
-spec get_coverage(Level, module()) -> Result
                      when
                          Level :: function | line | cover_id_line,
                          Result :: [{Entity, CoverageInfo}],
                          Entity :: {Function, Arity} | Line | CoverId,
                          CoverageInfo :: Covered | Counter,
                          Function :: atom(),
                          Arity :: arity(),
                          Line :: non_neg_integer(),
                          CoverId :: pos_integer(),
                          Covered :: boolean(),
                          Counter :: non_neg_integer().
```

Return either `function` or `line` coverage data for module `Module`.

If Level is `function`, returns function coverage for the given module
according to its [coverage mode](`t:coverage_mode/0`):

- **`function`** - For each function in module Module, a boolean indicating
  whether that function has been executed at least once is returned.

- **`function_counters`** - For each function in module Module, an integer
  giving the number of times that line has been executed is returned.

- **`line`** - For each function in module Module, a boolean indicating whether
  that function has been executed at least once is returned.

- **`line_counters`** - For each function in module Module, a boolean indicating
  whether that function has been executed at least once is returned (note that
  in this mode, counters for the number of times each function has been executed
  **cannot** be retrieved).

If Level is `line`, returns line coverage for the given module according to its
coverage mode:

- **`line`** - For each executable line in the module, a boolean indicating
  whether that line has been executed at least once is returned.

- **`line_counters`** - For each executable line in the module, an integer
  giving the number of times that line was executed is returned.

Level `cover_id_line` is used by the `m:cover` tool.

Failures:

- **`badarg`** - If `Level` is not `function` or `line`.

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If `Module` was not loaded in another coverage mode than
  `none`.

- **`badarg`** - If Level is `line` and `Module` has not been loaded with either
  `line` or `line_counters` enabled.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `get_coverage_mode`
*since OTP 27.0* 

```erlang
-spec get_coverage_mode() -> Mode when Mode :: coverage_mode().
```

Returns the coverage mode as set by option
[\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`) for `erl` or `set_coverage_mode/1`.

Failure:

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `get_coverage_mode`
*since OTP 27.0* 

```erlang
-spec get_coverage_mode(Module) -> Mode when Module :: module(), Mode :: coverage_mode().
```

Get coverage mode for the given module.

Failures:

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `get_debug_info`
*since OTP 28.0* 

```erlang
-spec get_debug_info(Module) -> DebugInfo when Module :: module(), DebugInfo :: debug_info().
```

# `get_doc`
*since OTP 23.0* 

```erlang
-spec get_doc(Mod) -> {ok, Res} | {error, Reason}
                 when
                     Mod :: module(),
                     Res ::
                         #docs_v1{anno :: term(),
                                  beam_language :: term(),
                                  format :: term(),
                                  module_doc :: term(),
                                  metadata :: term(),
                                  docs :: term()},
                     Reason :: non_existing | missing | file:posix().
```

Returns [EEP 48](https://www.erlang.org/eeps/eep-0048.html) style
documentation for `Module` if available.

If `Module` is not found in the code path, this function returns
`{error,non_existing}`.

If no documentation can be found this function attempts to generate
documentation from the debug information in the module. If no debug
information is available, this function returns `{error,missing}`.

For more information about the documentation chunk see
[Documentation Storage and Format](eep48_chapter.md) in
Kernel's User's Guide.

# `get_mode`
*since OTP R16B* 

```erlang
-spec get_mode() -> embedded | interactive.
```

Returns an atom describing the mode of the code server: `interactive` or
`embedded`.

This information is useful when an external entity (for example, an IDE)
provides additional code for a running node. If the code server is in
interactive mode, it only has to add the path to the code. If the code server is
in embedded mode, the code must be loaded with `load_binary/3`.

# `get_object_code`

```erlang
-spec get_object_code(Module) -> {Module, Binary, Filename} | error
                         when Module :: module(), Binary :: binary(), Filename :: file:filename().
```

Returns the object code for module `Module` if found in the code path.

Returns `{Module, Binary, Filename}` if successful, otherwise
`error`. `Binary` is a binary data object, which contains the object
code for the module. This is useful if code is to be loaded on a
remote node in a distributed system. For example, loading module
`Module` on a node `Node` is done as follows:

```erlang
...
{_Module, Binary, Filename} = code:get_object_code(Module),
erpc:call(Node, code, load_binary, [Module, Filename, Binary]),
...
```

# `get_path`

```erlang
-spec get_path() -> Path when Path :: [Dir :: file:filename()].
```

Returns the code path.

# `is_loaded`

```erlang
-spec is_loaded(Module) -> {file, Loaded} | false when Module :: module(), Loaded :: loaded_filename().
```

Checks whether `Module` is loaded.

If it is, `{file, Loaded}` is returned, otherwise `false`.

Normally, `Loaded` is the absolute filename `Filename` from which the code is
obtained. If the module is preloaded (see [`script(4)`](`e:sasl:script.md`)),
`Loaded =:= preloaded`. If the module is Cover-compiled (see `m:cover`),
`Loaded =:= cover_compiled`.

# `is_sticky`

```erlang
-spec is_sticky(Module) -> boolean() when Module :: module().
```

Returns `true` if `Module` is the name of a module that has been loaded from a
sticky directory (in other words: an attempt to reload the module will fail), or
`false` if `Module` is not a loaded module or is not sticky.

# `lib_dir`

```erlang
-spec lib_dir() -> file:filename().
```

Returns the library directory, `$OTPROOT/lib`, where `$OTPROOT` is the root
directory of Erlang/OTP.

_Example:_

```erlang
1> code:lib_dir().
"/usr/local/otp/lib"
```

# `lib_dir`

```erlang
-spec lib_dir(Name) -> file:filename() | {error, bad_name} when Name :: atom().
```

Returns the path for the *library directory*, the top directory, for an
application `Name` located under `$OTPROOT/lib` or in a directory referred to
with environment variable `ERL_LIBS`.

If a regular directory called `Name` or `Name-Vsn` exists in the code path with
an `ebin` subdirectory, the path to this directory is returned (not the `ebin`
directory).

If the directory refers to a directory in an archive, the archive name is
stripped away before the path is returned. For example, if directory
`/usr/local/otp/lib/mnesia-4.2.2.ez/mnesia-4.2.2/ebin` is in the path,
`/usr/local/otp/lib/mnesia-4.2.2/ebin` is returned. This means that the library
directory for an application is the same, regardless if the application resides
in an archive or not.

> #### Warning {: .info }
>
> Archives are experimental. In a future release, they can be removed or
> their behavior can change.

_Example:_

```erlang
> code:lib_dir(mnesia).
"/usr/local/otp/lib/mnesia-4.23"
```

Returns `{error, bad_name}` if `Name` is not the name of an application under
`$OTPROOT/lib` or on a directory referred to through environment variable
`ERL_LIBS`. Fails with an exception if `Name` has the wrong type.

> #### Warning {: .warning }
>
> For backward compatibility, `Name` is also allowed to be a string. That will
> probably change in a future release.

# `lib_dir`

> This function is deprecated. code:lib_dir/2 is deprecated; this functionality will be removed in a future release.

```erlang
-spec lib_dir(Name, SubDir) -> file:filename() | {error, bad_name} when Name :: atom(), SubDir :: atom().
```

Returns the path to a subdirectory directly under the top directory of an
application.

> #### Change {: .info }
>
> This function is part of the archive support, which is an experimental
> feature that will be changed or removed in a future release.

Normally the subdirectories reside under the top directory for the
application, but when applications at least partly reside in an archive, the
situation is different. Some of the subdirectories can reside as regular
directories while others reside in an archive file. It is not checked whether
this directory exists.

Instead of using this function, use [`code:lib_dir/1`](`code:lib_dir/1`)
and `filename:join/2`.

_Example:_

```erlang
1> filename:join(code:lib_dir(megaco), "priv").
"/usr/local/otp/lib/megaco-3.9.1.1/priv"
```

Fails with an exception if `Name` or `SubDir` has the wrong type.

# `load_abs`

```erlang
-spec load_abs(Filename) -> load_ret() when Filename :: file:filename().
```

Equivalent to [`load_file(Module)`](`load_file/1`), except that `Filename` is
an absolute or relative filename.

The code path is not searched. It returns a value in the same way as
`load_file/1`. Notice that `Filename` must not contain the extension
(for example, `.beam`) because [`load_abs/1`](`load_abs/1`) adds the
correct extension.

# `load_binary`

```erlang
-spec load_binary(Module, Filename, Binary) -> {module, Module} | {error, What}
                     when
                         Module :: module(),
                         Filename :: loaded_filename(),
                         Binary :: binary(),
                         What :: badarg | load_error_rsn().
```

Loads object code from a binary.

This function can be used to load object code on remote Erlang nodes. Argument
`Binary` must contain object code for `Module`. `Filename` is only used by the
code server to keep a record of from which file the object code for `Module`
originates. Thus, `Filename` is not opened and read by the code server.

Returns `{module, Module}` if successful, or `{error, Reason}` if loading fails.
See [Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of the possible error reasons.

# `load_file`

```erlang
-spec load_file(Module) -> load_ret() when Module :: module().
```

Tries to load the Erlang module `Module` using the code path.

It looks for the object code file with an extension corresponding to
the Erlang machine used, for example, `Module.beam`. The loading fails
if the module name found in the object code differs from the name
`Module`. Use `load_binary/3` to load object code with a module name
that is different from the file name.

Returns `{module, Module}` if successful, or `{error, Reason}` if loading fails.
See [Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of the possible error reasons.

# `modified_modules`
*since OTP 20.0* 

```erlang
-spec modified_modules() -> [module()].
```

Returns the list of all currently loaded modules for which `module_status/1`
returns `modified`.

See also `all_loaded/0`.

# `module_status`
*since OTP 23.0* 

```erlang
-spec module_status() -> [{module(), module_status()}].
```

See `module_status/1` and `all_loaded/0` for details.

# `module_status`
*since OTP 20.0* 

```erlang
-spec module_status(Module :: module() | [module()]) -> module_status() | [{module(), module_status()}].
```

Returns the status of `Module` in relation to object file on disk.

The status of a module can be one of:

- **`not_loaded`** - If `Module` is not currently loaded.

- **`loaded`** - If `Module` is loaded, and the object file exists and contains
  the same code.

- **`removed`** - If `Module` is loaded, but no corresponding object file can be
  found in the code path.

- **`modified`** - If `Module` is loaded, but the object file contains code with
  a different MD5 checksum.

Preloaded modules are always reported as `loaded`, without inspecting the
contents on disk. Cover-compiled modules will always be reported as `modified`
if an object file exists, or as `removed` otherwise. Modules whose load path is
an empty string (which is the convention for auto-generated code) will only be
reported as `loaded` or `not_loaded`.

See also `modified_modules/0`.

# `objfile_extension`

```erlang
-spec objfile_extension() -> nonempty_string().
```

Returns the object code file extension corresponding to the Erlang machine used.

For the official Erlang/OTP release, the return value is always `.beam`.

# `prepare_loading`
*since OTP 19.0* 

```erlang
-spec prepare_loading(Modules) -> {ok, Prepared} | {error, [{Module, What}]}
                         when
                             Modules :: [Module | {Module, Filename, Binary}],
                             Module :: module(),
                             Filename :: file:filename(),
                             Binary :: binary(),
                             Prepared :: prepared_code(),
                             What :: badfile | nofile | on_load_not_allowed | duplicated.
```

Prepares to load the modules in the list `Modules`.

Finish the loading by calling
[finish_loading(Prepared)](`finish_loading/1`).

This function can fail with one of the following error reasons:

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code exists.

- **`on_load_not_allowed`** - A module contains an
  [\-on_load function](`e:system:code_loading.md#on_load`).

- **`duplicated`** - A module is included more than once in `Modules`.

# `priv_dir`

```erlang
-spec priv_dir(Name) -> file:filename() | {error, bad_name} when Name :: atom().
```

Returns the path to the `priv` directory in an application.

> #### Warning {: .warning }
>
> For backward compatibility, `Name` is also allowed to be a string. That will
> probably change in a future release.

# `purge`

```erlang
-spec purge(Module) -> boolean() when Module :: module().
```

Purges the code for `Module`, that is, removes code marked as old.

If some processes still linger in the old code, these processes are
killed before the code is removed.

> #### Change {: .info }
>
> As of Erlang/OTP 20.0, a process is only considered to be lingering in the
> code if it has direct references to the code. For more information see
> documentation of `erlang:check_process_code/3`, which is used in order to
> determine whether a process is lingering.

Returns `true` if successful and any process is needed to be killed, otherwise
`false`.

# `replace_path`

```erlang
-spec replace_path(Name, Dir) -> replace_path_ret() when Name :: atom(), Dir :: file:filename().
```

# `replace_path`
*since OTP 26.0* 

```erlang
-spec replace_path(Name, Dir, cache()) -> replace_path_ret() when Name :: atom(), Dir :: file:filename().
```

Replaces an old occurrence of a directory named `.../Name[-Vsn][/ebin]` in the
code path, with `Dir`.

If `Name` does not exist, it adds the new directory `Dir` last in the
code path. The new directory must also be named
`.../Name[-Vsn][/ebin]`. This function is to be used if a new version
of the directory (library) is added to a running system.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns:

- **`true`** - If successful

- **`{error, bad_name}`** - If `Name` is not found

- **`{error, bad_directory}`** - If `Dir` does not exist

- **`{error, {badarg, [Name, Dir]}}`** - If `Name` or `Dir` is invalid

# `reset_coverage`
*since OTP 27.0* 

```erlang
-spec reset_coverage(Module) -> ok when Module :: module().
```

Resets coverage information for module `Module`.

If the [coverage mode](`t:coverage_mode/0`) is either `function` or
`line`, all booleans for `Module` keeping track of executed functions
or lines are set to `false`.

If the coverage mode is either `function_counters` or
`line_counters`, all counters for `Module` are reset to zero.

Failures:

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If `Module` was not loaded with coverage enabled.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `root_dir`

```erlang
-spec root_dir() -> file:filename().
```

Returns the root directory of Erlang/OTP, which is the directory where it is
installed.

_Example:_

```erlang
1> code:root_dir().
"/usr/local/otp"
```

# `set_coverage_mode`
*since OTP 27.0* 

```erlang
-spec set_coverage_mode(Mode) -> OldMode when Mode :: coverage_mode(), OldMode :: coverage_mode().
```

Sets the coverage mode for modules that are subsequently loaded, similar to
option [\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`) for `erl`.

The coverage mode will have the following effect on code that is
loaded following this call:

- **`function`** - All modules that are loaded will be instrumented to keep
  track of which functions are executed. Information about which functions that
  have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`function_counters`** - All modules that are loaded will be instrumented to
  count how many times each function is executed. Information about how many
  times each function has been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`line`** - When modules that have been compiled with the
  [`line_coverage`](`m:compile#line_coverage`) option are loaded, they will be
  instrumented to keep track of which lines have been executed. Information
  about which lines have been executed can be retrieved by calling
  [`get_coverage(line, Module)`](`get_coverage/2`), and information about which
  functions that have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`line_counters`** - When modules that have been compiled with the
  [`line_coverage`](`m:compile#line_coverage`) option are loaded, they will be
  instrumented to count the number of times each line is executed. Information
  about how many times each line has been executed can be retrieved by calling
  [`get_coverage(line, Module)`](`get_coverage/2`), and information about which
  functions that have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`) (note that in this mode,
  counters for the number of times each function has been executed **cannot** be
  retrieved).

- **`none`** - Modules will be loaded without coverage instrumentation.

Returns the previous coverage mode.

Failures:

- **`badarg`** - If `Mode` is not a valid coverage mode.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)

# `set_path`

```erlang
-spec set_path(Path) -> set_path_ret() when Path :: [Dir :: file:filename()].
```

# `set_path`
*since OTP 26.0* 

```erlang
-spec set_path(Path, cache()) -> set_path_ret() when Path :: [Dir :: file:filename()].
```

Sets the code path to the list of directories `Path`.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns:

- **`true`** - If successful

- **`{error, bad_directory}`** - If any `Dir` is not a directory name

# `soft_purge`

```erlang
-spec soft_purge(Module) -> boolean() when Module :: module().
```

Purges the code for `Module`, that is, removes code marked as old, but only if
no processes linger in it.

> #### Change {: .info }
>
> As of Erlang/OTP 20.0, a process is only considered to be lingering in the
> code if it has direct references to the code. For more information see
> documentation of `erlang:check_process_code/3`, which is used in order to
> determine whether a process is lingering.

Returns `false` if the module cannot be purged because of processes lingering in
old code, otherwise `true`.

# `stick_dir`

```erlang
-spec stick_dir(Dir) -> ok | error when Dir :: file:filename().
```

Marks `Dir` as sticky.

Returns `ok` if successful, otherwise `error`.

# `unstick_dir`

```erlang
-spec unstick_dir(Dir) -> ok | error when Dir :: file:filename().
```

Unsticks a directory that is marked as sticky.

Returns `ok` if successful, otherwise `error`.

# `where_is_file`

```erlang
-spec where_is_file(Filename) -> non_existing | Absname
                       when Filename :: file:filename(), Absname :: file:filename().
```

Searches the code path for `Filename`, which is a file of arbitrary type.

If found, the full name is returned. `non_existing` is returned if the
file cannot be found.  The function can be useful, for example, to
locate application resource files.

# `which`

```erlang
-spec which(Module) -> Which when Module :: module(), Which :: loaded_filename() | non_existing.
```

If the module is not loaded, this function searches the code path for the first
file containing object code for `Module` and returns the absolute filename.

- If the module is loaded, it returns the name of the file containing the loaded
  object code.

- If the module is preloaded, `preloaded` is returned.

- If the module is Cover-compiled, `cover_compiled` is returned.

- If the module cannot be found, `non_existing` is returned.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
