# `release_handler`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/sasl/src/release_handler.erl#L22)

Unpacking and Installation of Release Packages

The _release handler_ process belongs to the SASL application, which is
responsible for _release handling_, that is, unpacking, installation, and
removal of release packages.

An introduction to release handling and an example is provided by
[Release Handling section in OTP Design Principles](`e:system:release_handling.md`).

A _release package_ is a compressed tar file containing code for a certain
version of a release, created by calling
[`systools:make_tar/1,2`](`systools:make_tar/1`). The release package is to be
located in the `$ROOT/releases` directory of the previous version of the
release, where `$ROOT` is the installation root directory,
[`code:root_dir()`](`code:root_dir/0`). Another `releases` directory can be
specified using the SASL configuration parameter `releases_dir` or the OS
environment variable `RELDIR`. The release handler must have write access to
this directory to install the new release. The persistent state of the release
handler is stored there in a file called `RELEASES`.

A release package is always to contain:

- A release resource file, `Name.rel`
- A boot script, `Name.boot`

The `.rel` file contains information about the release: its name, version, and
which ERTS and application versions it uses.

A release package can also contain:

- A release upgrade file, `relup`
- A system configuration file, `sys.config`
- A system configuration source file, `sys.config.src`

The `relup` file contains instructions for how to upgrade to, or downgrade from,
this version of the release.

The release package can be _unpacked_, which extracts the files. An unpacked
release can be _installed_. The currently used version of the release is then
upgraded or downgraded to the specified version by evaluating the instructions
in the `relup` file. An installed release can be made _permanent_. Only one
permanent release can exist in the system, and this release is used if the
system is restarted. An installed release, except the permanent one, can be
_removed_. When a release is removed, all files belonging to that release only
are deleted.

Each release version has a status, which can be `unpacked`, `current`,
`permanent`, or `old`. There is always one latest release, which either has
status `permanent` (normal case) or `current` (installed, but not yet made
permanent). The meaning of the status values are illustrated in the following
table:

```text
        Status     Action                NextStatus
        -------------------------------------------
        -          unpack                unpacked
        unpacked   install               current
                   remove                -
        current    make_permanent        permanent
                   install other         old
                   remove                -
        permanent  make other permanent  old
                   install               permanent
        old        reboot_old            permanent
                   install               current
                   remove                -
```

The release handler process is a locally registered process on each node. When a
release is installed in a distributed system, the release handler on each node
must be called. The release installation can be synchronized between nodes. From
an operator view, it can be unsatisfactory to specify each node. The aim is to
install one release package in the system, no matter how many nodes there are.
It is recommended that software management functions are written that take care
of this problem. Such a function can have knowledge of the system architecture,
so it can contact each individual release handler to install the package.

For release handling to work properly, the runtime system must know which
release it is running. It must also be able to change (in runtime) which boot
script and system configuration file are to be used if the system is restarted.
This is taken care of automatically if Erlang is started as an embedded system.
Read about this in [Embedded System](`e:system:index.html`) in _System
Documentation_. In this case, the system configuration file `sys.config` is
mandatory.

The installation of a new release can restart the system. Which program to use
is specified by the SASL configuration parameter `start_prg`, which defaults to
`$ROOT/bin/start`.

The emulator restart on Windows NT expects that the system is started using the
`erlsrv` program (as a service). Furthermore, the release handler expects that
the service is named `NodeName`\_`Release`, where `NodeName` is the first part
of the Erlang node name (up to, but not including the "@") and `Release` is the
current release version. The release handler furthermore expects that a program
like `start_erl.exe` is specified as "machine" to `erlsrv`. During upgrading
with restart, a new service is registered and started. The new service is set to
automatic and the old service is removed when the new release is made permanent.

The release handler at a node running on a diskless machine, or with a read-only
file system, must be configured accordingly using the following SASL
configuration parameters (for details, see [sasl(6)](sasl_app.md)):

- **`masters`** - This node uses some master nodes to store and fetch release
  information. All master nodes must be operational whenever release information
  is written by this node.

- **`client_directory`** - The `client_directory` in the directory structure of
  the master nodes must be specified.

- **`static_emulator`** - This parameter specifies if the Erlang emulator is
  statically installed at the client node. A node with a static emulator cannot
  dynamically switch to a new emulator, as the executable files are statically
  written into memory.

The release handler can also be used to unpack and install release packages when
not running Erlang as an embedded system. However, in this case the user must
somehow ensure that correct boot scripts and configuration files are used if the
system must be restarted.

Functions are provided for using another file structure than the structure
defined in OTP. These functions can be used to test a release upgrade locally.

## Typical Error Reasons

- **`{bad_masters, Masters}`** - The master nodes `Masters` are not alive.

- **`{bad_rel_file, File}`** - Specified `.rel` file `File` cannot be read or
  does not contain a single term.

- **`{bad_rel_data, Data}`** - Specified `.rel` file does not contain a
  recognized release specification, but another term `Data`.

- **`{bad_relup_file, File}`** - Specified `relup` file `Relup` contains bad
  data.

- **`{cannot_extract_file, Name, Reason}`** - Problems when extracting from a
  tar file, `erl_tar:extract/2` returned `{error, {Name, Reason}}`.

- **`{existing_release, Vsn}`** - Specified release version `Vsn` is already in
  use.

- **`{Master, Reason, When}`** - Some operation, indicated by the term `When`,
  failed on the master node `Master` with the specified error reason `Reason`.

- **`{no_matching_relup, Vsn, CurrentVsn}`** - Cannot find a script for
  upgrading/downgrading between `CurrentVsn` and `Vsn`.

- **`{no_such_directory, Path}`** - The directory `Path`does not exist.

- **`{no_such_file, Path}`** - The path `Path` (file or directory) does not
  exist.

- **`{no_such_file, {Master, Path}}`** - The path `Path` (file or directory)
  does not exist at the master node `Master`.

- **`{no_such_release, Vsn}`** - The specified release version `Vsn` does not
  exist.

- **`{not_a_directory, Path}`** - `Path` exists but is not a directory.

- **`{Posix, File}`** - Some file operation failed for `File`. `Posix` is an
  atom named from the Posix error codes, such as `enoent`, `eacces`, or
  `eisdir`. See `m:file` in Kernel.

- **`Posix`** - Some file operation failed, as for the previous item in the
  list.

## Application Upgrade/Downgrade

The functions in the [Application Upgrade/Downgrade](#application-upgrade-downgrade)
section can be used to test upgrade and downgrade of single applications
(instead of upgrading/downgrading an entire release). A script corresponding to
the instructions in the relup file is created on-the-fly, based on the .appup
file for the application, and evaluated exactly in the same way as
release_handler does.

> #### Warning {: .warning }
>
> These functions are primarily intended for simplified testing of .appup files.
> They are not run within the context of the release_handler process.
> They must therefore not be used together with calls to install_release/1,2,
> as this causes the release_handler to end up in an inconsistent state.
>
> No persistent information is updated, so these functions can be used on ay
> Erlang node, embedded or not. Also, using these functions does not affect which
> code is loaded if there is a reboot.
>
> If the upgrade or downgrade fails, the application can end up in an
> inconsistent state.

### See Also

[OTP Design Principles](`e:system:index.html`),
[`config`](`e:kernel:config.md`), [`rel`](rel.md), [`relup`](relup.md),
[`script`](script.md), `m:sys`, `m:systools`

# `downgrade_app`

```erlang
-spec downgrade_app(App, Dir) -> {ok, Unpurged} | restart_emulator | {error, Reason}
                       when
                           App :: atom(),
                           Dir :: string(),
                           Unpurged :: [Module],
                           Module :: atom(),
                           Reason :: term().
```

# `downgrade_app`

```erlang
-spec downgrade_app(App, OldVsn, Dir) -> {ok, Unpurged} | restart_emulator | {error, Reason}
                       when
                           App :: atom(),
                           Dir :: string(),
                           OldVsn :: string(),
                           Unpurged :: [Module],
                           Module :: atom(),
                           Reason :: term().
```

Downgrades an application `App` from the current version to a previous version
`OldVsn` located in `Dir` according to the `.appup` file.

`App` is the name of the application, which must be started. `OldVsn` is the
previous application version and can be omitted if `Dir` is of the format
`"App-OldVsn"`. `Dir` is the library directory of the previous version of `App`.
The corresponding modules and the old `.app` file are to be located under
`Dir/ebin`. The `.appup` file is to be located in the `ebin` directory of the
_current_ library directory of the application
([`code:lib_dir(App)`](`code:lib_dir/1`)).

The function looks in the `.appup` file and tries to find a downgrade script to
the previous version of the application using `downgrade_script/3`. This script
is evaluated using `eval_appup_script/4`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script

# `downgrade_script`

```erlang
-spec downgrade_script(App, OldVsn, Dir) -> {ok, Script}
                          when
                              App :: atom(),
                              OldVsn :: string(),
                              Dir :: string(),
                              Script :: Instructions :: term().
```

Tries to find an application downgrade script for `App` from the current version
to a previous version `OldVsn` located in `Dir`.

The downgrade script can then be evaluated using `eval_appup_script/4`. It is
recommended to use [`downgrade_app/2,3`](`downgrade_app/3`) instead, but this
function (`downgrade_script`) is useful to inspect the contents of the script.

`App` is the name of the application, which must be started. `Dir` is the
previous library directory of `App`. The corresponding modules and the old
`.app` file are to be located under `Dir/ebin`. The `.appup` file is to be
located in the `ebin` directory of the _current_ library directory of the
application ([`code:lib_dir(App)`)](`code:lib_dir/1`).

The function looks in the `.appup` file and tries to find a downgrade script
from the current application version. High-level instructions are translated to
low-level instructions. The instructions are sorted in the same manner as when
generating a `relup` file.

Returns `{ok, Script}` if successful. For details about `Script`, see
[`appup(4)`](appup.md).

Failure: If a script cannot be found, the function fails with an appropriate
error reason.

# `eval_appup_script`

```erlang
-spec eval_appup_script(App, ToVsn, ToDir, Script :: term()) ->
                           {ok, Unpurged} | restart_emulator | {error, Reason}
                           when
                               App :: atom(),
                               ToVsn :: string(),
                               ToDir :: string(),
                               Unpurged :: [{Module, PurgeMethod}],
                               Module :: atom(),
                               PurgeMethod :: term(),
                               Reason :: term().
```

Evaluates an application upgrade or downgrade script `Script`, the result from
calling `upgrade_script/2` or `downgrade_script/3`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

`App` is the name of the application, which must be started. `ToVsn` is the
version to be upgraded/downgraded to, and `ToDir` is the library directory of
this version. The corresponding modules as well as the `.app` and `.appup` files
are to be located under `Dir/ebin`.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script

If the `restart_new_emulator` instruction is found in the script,
`eval_appup_script/4` returns `{error,restart_new_emulator}`. This because
`restart_new_emulator` requires a new version of the emulator to be started
before the rest of the upgrade instructions can be executed, and this can only
be done by [`install_release/1,2`](`install_release/1`).

# `upgrade_app`

```erlang
-spec upgrade_app(App, Dir) -> {ok, Unpurged} | restart_emulator | {error, Reason}
                     when
                         App :: atom(),
                         Dir :: string(),
                         Unpurged :: [Module],
                         Module :: atom(),
                         Reason :: term().
```

Upgrades an application `App` from the current version to a new version located
in `Dir` according to the `.appup` file.

`App` is the name of the application, which must be started. `Dir` is the new
library directory of `App`. The corresponding modules as well as the `.app` and
`.appup` files are to be located under `Dir/ebin`.

The function looks in the `.appup` file and tries to find an upgrade script from
the current version of the application using `upgrade_script/2`. This script is
evaluated using `eval_appup_script/4`, exactly in the same way as
[`install_release/1,2`](`install_release/1`) does.

Returns one of the following:

- `{ok, Unpurged}` if evaluating the script is successful, where `Unpurged` is a
  list of unpurged modules
- `restart_emulator` if this instruction is encountered in the script
- `{error, Reason}` if an error occurred when finding or evaluating the script

If the `restart_new_emulator` instruction is found in the script,
`upgrade_app/2` returns `{error,restart_new_emulator}`. This because
`restart_new_emulator` requires a new version of the emulator to be started
before the rest of the upgrade instructions can be executed, and this can only
be done by [`install_release/1,2`](`install_release/1`).

# `upgrade_script`

```erlang
-spec upgrade_script(App, Dir) -> {ok, NewVsn, Script}
                        when
                            App :: atom(),
                            Dir :: string(),
                            NewVsn :: string(),
                            Script :: Instructions :: term().
```

Tries to find an application upgrade script for `App` from the current version
to a new version located in `Dir`.

The upgrade script can then be evaluated using `eval_appup_script/4`. It is
recommended to use `upgrade_app/2` instead, but this function (`upgrade_script`)
is useful to inspect the contents of the script.

`App` is the name of the application, which must be started. `Dir` is the new
library directory of `App`. The corresponding modules as well as the `.app` and
`.appup` files are to be located under `Dir/ebin`.

The function looks in the `.appup` file and tries to find an upgrade script from
the current application version. High-level instructions are translated to
low-level instructions. The instructions are sorted in the same manner as when
generating a `relup` file.

Returns `{ok, NewVsn, Script}` if successful, where `NewVsn` is the new
application version. For details about `Script`, see [`appup(4)`](appup.md).

Failure: If a script cannot be found, the function fails with an appropriate
error reason.

# `check_install_release`

```erlang
-spec check_install_release(Vsn) -> {ok, OtherVsn, Descr} | {error, Reason}
                               when
                                   Vsn :: string(),
                                   OtherVsn :: string(),
                                   Descr :: term(),
                                   Reason :: term().
```

# `check_install_release`
*since OTP R14B04* 

```erlang
-spec check_install_release(Vsn, Opts) -> {ok, OtherVsn, Descr} | {error, Reason}
                               when
                                   Vsn :: string(),
                                   OtherVsn :: string(),
                                   Opts :: [Opt],
                                   Opt :: purge,
                                   Descr :: term(),
                                   Reason :: term().
```

Checks if the specified version `Vsn` of the release can be installed.

The release must not have status `current`. Issues warnings if `relup` file or
`sys.config` is not present. If `relup` file is present, its contents are
checked and `{error,Reason}` is returned if an error is found. Also checks that
all required applications are present and that all new code can be loaded;
`{error,Reason}` is returned if an error is found.

Evaluates all instructions that occur before the `point_of_no_return`
instruction in the release upgrade script.

Returns the same as `install_release/1`. `Descr` defaults to "" if no `relup`
file is found.

If option `purge` is specified, all old code that can be soft-purged is purged
after all other checks are successfully completed. This can be useful to reduce
the time needed by `install_release/1`.

# `create_RELEASES`
*since OTP 25.0* 

```erlang
-spec create_RELEASES(RelDir, RelFile, AppDirs) -> ok | {error, Reason}
                         when
                             RelDir :: string(),
                             RelFile :: string(),
                             AppDirs :: [{App, Vsn, Dir}],
                             App :: atom(),
                             Vsn :: string(),
                             Dir :: string(),
                             Reason :: term().
```

# `create_RELEASES`

```erlang
-spec create_RELEASES(Root, RelDir, RelFile, AppDirs) -> ok | {error, Reason}
                         when
                             Root :: string(),
                             RelDir :: string(),
                             RelFile :: string(),
                             AppDirs :: [{App, Vsn, Dir}],
                             App :: atom(),
                             Vsn :: string(),
                             Dir :: string(),
                             Reason :: term().
```

Creates an initial `RELEASES` file to be used by the release handler.

This file must exist to install new releases.

`Root` is the root of the installation (`$ROOT`) as described earlier. `RelDir`
is the directory where the `RELEASES` file is to be created (normally
`$ROOT/releases`). `RelFile` is the name of the `.rel` file that describes the
initial release, including the extension `.rel`. If `Root` is not given, the
`RELEASES` file will be location independent (i.e, it will not contain absolute
paths unless there are absolute paths in `AppDirs`). A `RELEASES` file should be
made location independent if the installation's `$ROOT` is unknown. The
`release_handler` module will interpret relative paths in a running system's
`RELEASES` file as being relative to `$ROOT`.

`AppDirs` can be used to specify from where the modules for the specified
applications are to be loaded. `App` is the name of an application, `Vsn` is the
version, and `Dir` is the name of the directory where `App-Vsn` is located. The
corresponding modules are to be located under `Dir/App-Vsn/ebin`. The
directories for applications not specified in `AppDirs` are assumed to be
located in `$ROOT/lib`.

# `install_file`

```erlang
-spec install_file(Vsn, File) -> ok | {error, Reason}
                      when Vsn :: string(), File :: string(), Reason :: term().
```

Installs a release-dependent file in the release structure.

The release-dependent file must be in the release structure when a new release
is installed: `start.boot`, `relup`, and `sys.config`.

The function can be called, for example, when these files are generated at the
target. The function is to be called after `set_unpacked/2` has been called.

# `install_release`

```erlang
-spec install_release(Vsn) -> {ok, OtherVsn, Descr} | {error, Reason}
                         when
                             Vsn :: string(),
                             OtherVsn :: string(),
                             Descr :: term(),
                             Reason ::
                                 {already_installed, Vsn} |
                                 {change_appl_data, term()} |
                                 {missing_base_app, OtherVsn, App} |
                                 {could_not_create_hybrid_boot, term()} |
                                 term(),
                             App :: atom().
```

# `install_release`

```erlang
-spec install_release(Vsn, [Opt]) ->
                         {ok, OtherVsn, Descr} |
                         {continue_after_restart, OtherVsn, Descr} |
                         {error, Reason}
                         when
                             Vsn :: string(),
                             OtherVsn :: string(),
                             Opt ::
                                 {error_action, Action} |
                                 {code_change_timeout, Timeout} |
                                 {suspend_timeout, Timeout} |
                                 {update_paths, Bool},
                             Action :: restart | reboot,
                             Timeout :: default | infinity | pos_integer(),
                             Bool :: boolean(),
                             Descr :: term(),
                             Reason ::
                                 {illegal_option, Opt} |
                                 {already_installed, Vsn} |
                                 {change_appl_data, term()} |
                                 {missing_base_app, OtherVsn, App} |
                                 {could_not_create_hybrid_boot, term()} |
                                 term(),
                             App :: atom().
```

Installs the specified version `Vsn` of the release.

Looks first for a `relup` file for `Vsn` and a script
`{UpFromVsn,Descr1,Instructions1}` in this file for upgrading from the
current version. If not found, the function looks for a `relup` file
for the current version and a script `{Vsn,Descr2,Instructions2}` in
this file for downgrading to `Vsn`.

If a script is found, the first thing that happens is that the application
specifications are updated according to the `.app` files and `sys.config`
belonging to the release version `Vsn`.

After the application specifications have been updated, the instructions in the
script are evaluated and the function returns `{ok,OtherVsn,Descr}` if
successful. `OtherVsn` and `Descr` are the version (`UpFromVsn` or `Vsn`) and
description (`Descr1` or `Descr2`) as specified in the script.

If `{continue_after_restart,OtherVsn,Descr}` is returned, the emulator is
restarted before the upgrade instructions are executed. This occurs if the
emulator or any of the applications Kernel, STDLIB, or SASL are updated. The new
emulator version and these core applications execute after the restart. For all
other applications the old versions are started and the upgrade is performed as
normal by executing the upgrade instructions.

If a recoverable error occurs, the function returns `{error,Reason}` and the
original application specifications are restored. If a non-recoverable error
occurs, the system is restarted.

_Options_:

- **`error_action`** - Defines if the node is to be restarted
  ([`init:restart()`](`init:restart/0`)) or rebooted
  ([`init:reboot()`](`init:reboot/0`)) if there is an error during the
  installation. Default is `restart`.

- **`code_change_timeout`** - Defines the time-out for all calls to
  [`sys:change_code`](`sys:change_code/4`). If no value is specified or
  `default` is specified, the default value defined in `sys` is used.

- **`suspend_timeout`** - Defines the time-out for all calls to
  [`sys:suspend`](`sys:suspend/1`). If no value is specified, the values defined
  by the `Timeout` parameter of the `upgrade` or `suspend` instructions are
  used. If `default` is specified, the default value defined in `sys` is used.

- **`{update_paths,Bool}`** - Indicates if all application code paths are to be
  updated (`Bool==true`) or if only code paths for modified applications are to
  be updated (`Bool==false`, default). This option has only effect for other
  application directories than the default `$ROOT/lib/App-Vsn`, that is,
  application directories specified in argument `AppDirs` in a call to
  `create_RELEASES/4` or `set_unpacked/2`.

  _Example:_

  In the current version `CurVsn` of a release, the application directory of
  `myapp` is `$ROOT/lib/myapp-1.0`. A new version `NewVsn` is unpacked outside
  the release handler and the release handler is informed about this with a call
  as follows:

  ```erlang
  release_handler:set_unpacked(RelFile, [{myapp,"1.0","/home/user"},...]).
  => {ok,NewVsn}
  ```

  If `NewVsn` is installed with option `{update_paths,true}`, then
  [`code:lib_dir(myapp)`](`code:lib_dir/1`) returns `/home/user/myapp-1.0`.

> #### Note {: .info }
>
> Installing a new release can be time consuming if there are many processes in
> the system. The reason is that each process must be checked for references to
> old code before a module can be purged. This check can lead to garbage
> collections and copying of data.
>
> To speed up the execution of [`install_release`](`install_release/1`), first
> call [`check_install_release`](`check_install_release/1`), using option
> `purge`. This does the same check for old code. Then purges all modules that
> can be soft-purged. The purged modules do then no longer have any old code,
> and [`install_release`](`install_release/1`) does not need to do the checks.
>
> This does not reduce the overall time for the upgrade, but it allows checks
> and purge to be executed in the background before the real upgrade is started.

> #### Note {: .info }
>
> When upgrading the emulator from a version older than OTP R15, an attempt is
> made to load new application beam code into the old emulator. Sometimes the
> new beam format cannot be read by the old emulator, so the code loading fails
> and the complete upgrade is terminated. To overcome this problem, the new
> application code is to be compiled with the old emulator. For more information
> about emulator upgrade from pre OTP R15 versions, see
> [Design Principles](`e:system:appup_cookbook.md`) in _System Documentation_.

# `make_permanent`

```erlang
-spec make_permanent(Vsn) -> ok | {error, Reason}
                        when Vsn :: string(), Reason :: {bad_status, Status :: term()} | term().
```

Makes the specified release version `Vsn` permanent.

# `reboot_old_release`

```erlang
-spec reboot_old_release(Vsn) -> ok | {error, Reason}
                            when Vsn :: string(), Reason :: {bad_status, Status :: term()} | term().
```

Reboots the system by making the old release permanent, and calls
[`init:reboot()`](`init:reboot/0`) directly.

The release must have status `old`.

# `remove_release`

```erlang
-spec remove_release(Vsn) -> ok | {error, Reason}
                        when Vsn :: string(), Reason :: {permanent, Vsn} | client_node | term().
```

Removes a release and its files from the system.

The release must not be the permanent release. Removes only the files
and directories not in use by another release.

# `set_removed`

```erlang
-spec set_removed(Vsn) -> ok | {error, Reason} when Vsn :: string(), Reason :: {permanent, Vsn} | term().
```

Makes it possible to handle removal of releases outside the release handler.

Tells the release handler that the release is removed from the system. This
function does not delete any files.

# `set_unpacked`

```erlang
-spec set_unpacked(RelFile, AppDirs) -> {ok, Vsn} | {error, Reason}
                      when
                          RelFile :: string(),
                          AppDirs :: [{App, Vsn, Dir}],
                          App :: atom(),
                          Vsn :: string(),
                          Dir :: string(),
                          Reason :: term().
```

Makes it possible to handle unpacking of releases outside the release handler.

Tells the release handler that the release is unpacked. `Vsn` is extracted from
the release resource file `RelFile`.

`AppDirs` can be used to specify from where the modules for the specified
applications are to be loaded. `App` is the name of an application, `Vsn` is the
version, and `Dir` is the name of the directory where `App-Vsn` is located. The
corresponding modules are to be located under `Dir/App-Vsn/ebin`. The
directories for applications not specified in `AppDirs` are assumed to be
located in `$ROOT/lib`.

# `unpack_release`

```erlang
-spec unpack_release(Name) -> {ok, Vsn} | {error, Reason}
                        when Name :: string(), Vsn :: string(), Reason :: client_node | term().
```

Unpacks a release package `Name.tar.gz` located in the `releases` directory.

Performs some checks on the package, for example, checks that all mandatory
files are present, and extracts its contents.

# `which_releases`

```erlang
-spec which_releases() -> [{Name, Vsn, Apps, Status}]
                        when
                            Name :: string(),
                            Vsn :: string(),
                            Apps :: [AppVsn :: string()],
                            Status :: unpacked | current | permanent | old.
```

Returns all releases known to the release handler.

# `which_releases`
*since OTP R15B* 

```erlang
-spec which_releases(Status) -> [{Name, Vsn, Apps, Status}]
                        when
                            Name :: string(),
                            Vsn :: string(),
                            Apps :: [AppVsn :: string()],
                            Status :: unpacked | current | permanent | old.
```

Returns all releases, known to the release handler, of a specific status.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
