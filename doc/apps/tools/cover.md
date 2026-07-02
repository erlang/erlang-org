# `cover`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/tools/src/cover.erl#L22)

A Coverage Analysis Tool for Erlang

The module `cover` provides a set of functions for coverage analysis
of Erlang programs, counting how many times each _executable line_ of
code is executed when a program is run. Executable lines are
lines in the body of a clause in a function, `case`,
`receive`, or `try`. Lines in clause heads, blank lines, and lines
containing only comments are not executable.

Coverage analysis can be used to verify that test cases covers all
relevant line in the code being test. It can also be helpful when
looking for bottlenecks in the code.

Before any analysis can take place, the involved modules has to be
_cover-compiled_.  This means that some extra information is added to
the module before it is compiled into a binary which then is
loaded. The source file of the module is not affected and no `.beam`
file is created. If the runtime system supports coverage natively,
Cover will automatically use that functionality to lower the execution
overhead for cover-compiled code.

> #### Change {: .info }
>
> Native coverage support was added in Erlang/OTP 27.

Each time a function in a cover-compiled module is called, information about the
call is added to an internal database of Cover. The coverage analysis is
performed by examining the contents of the Cover database. The output `Answer`
is determined by two parameters: `Level` and `Analysis`.

- `Level = module`

  `Answer = {Module,Value}`, where `Module` is the module name.

- `Level = function`

  `Answer = [{Function,Value}]`, one tuple for each function in the module. A
  function is specified by its module name `M`, function name `F` and arity `A`
  as a tuple `{M,F,A}`.

- `Level = clause`

  `Answer = [{Clause,Value}]`, one tuple for each clause in the module. A clause
  is specified by its module name `M`, function name `F`, arity `A` and position
  in the function definition `C` as a tuple `{M,F,A,C}`.

- `Level = line`

  `Answer = [{Line,Value}]`, one tuple for each executable line in the module. A
  line is specified by its module name `M` and line number in the source file
  `N` as a tuple `{M,N}`.

- `Analysis = coverage`

  `Value = {Cov,NotCov}` where `Cov` is the number of executable lines in the
  module, function, clause or line that have been executed at least once and
  `NotCov` is the number of executable lines that have not been executed.

- `Analysis = calls`

  `Value = Calls` which is the number of times the module, function, or clause
  has been called. In the case of line level analysis, `Calls` is the number of
  times the line has been executed.

### Distribution

Cover can be used in a distributed Erlang system. One of the nodes in the system
has to be selected as the _main node_, and all Cover commands must be
executed from that node. The error reason `not_main_node` is returned if an
interface function is called on one of the remote nodes.

Use `cover:start/1` and `cover:stop/1` to add or remove nodes. The
same cover-compiled code will be loaded on each node, and analysis
will collect and sum up coverage data results from all nodes.

To only collect data from remote nodes without stopping `cover` on those nodes,
use `cover:flush/1`

If the connection to a remote node goes down, the main node will mark it as
lost. If the node comes back it will be added again. If the remote node was
alive during the disconnected period, cover data from before and during this
period will be included in the analysis.

# `analyse_answer`
*not exported* 

```erlang
-type analyse_answer() :: {ok, OutFile :: file:filename()} | {error, analyse_rsn()}.
```

# `analyse_fail`
*not exported* 

```erlang
-type analyse_fail() :: [{not_cover_compiled, module()}].
```

# `analyse_file_fail`
*not exported* 

```erlang
-type analyse_file_fail() :: [analyse_rsn()].
```

# `analyse_file_ok`
*not exported* 

```erlang
-type analyse_file_ok() :: [OutFile :: file:filename()].
```

# `analyse_item`
*not exported* 

```erlang
-type analyse_item() ::
          (Line :: {M :: module(), N :: non_neg_integer()}) |
          (Clause :: {M :: module(), F :: atom(), A :: arity(), C :: non_neg_integer()}) |
          (Function :: {M :: module(), F :: atom(), A :: arity()}).
```

# `analyse_ok`
*not exported* 

```erlang
-type analyse_ok() ::
          [{Module :: module(), Value :: analyse_value()}] |
          [{Item :: analyse_item(), Value :: analyse_value()}].
```

# `analyse_option`
*not exported* 

```erlang
-type analyse_option() ::
          html | {outfile, OutFile :: file:filename()} | {outdir, OutDir :: file:filename()}.
```

# `analyse_rsn`
*not exported* 

```erlang
-type analyse_rsn() ::
          {not_cover_compiled, Module :: module()} |
          {file, File :: file:filename(), Reason :: term()} |
          {no_source_code_found, Module :: module()}.
```

# `analyse_value`
*not exported* 

```erlang
-type analyse_value() ::
          {Cov :: non_neg_integer(), NotCov :: non_neg_integer()} | (Calls :: non_neg_integer()).
```

# `analysis`
*not exported* 

```erlang
-type analysis() :: coverage | calls.
```

# `beam_mod_file`
*not exported* 

```erlang
-type beam_mod_file() :: (Module :: module()) | (BeamFile :: file:filename()).
```

# `beam_mod_files`
*not exported* 

```erlang
-type beam_mod_files() :: beam_mod_file() | [beam_mod_file()].
```

# `compile_beam_result`
*not exported* 

```erlang
-type compile_beam_result() ::
          {ok, module()} | {error, BeamFile :: file:filename()} | {error, Reason :: compile_beam_rsn()}.
```

# `compile_beam_rsn`
*not exported* 

```erlang
-type compile_beam_rsn() ::
          non_existing |
          {no_abstract_code, BeamFile :: file:filename()} |
          {encrypted_abstract_code, BeamFile :: file:filename()} |
          {already_cover_compiled, no_beam_found, module()} |
          {{missing_backend, module()}, BeamFile :: file:filename()} |
          {no_file_attribute, BeamFile :: file:filename()} |
          not_main_node.
```

# `compile_result`
*not exported* 

```erlang
-type compile_result() :: {ok, Module :: module()} | {error, file:filename()} | {error, not_main_node}.
```

# `export_reason`
*not exported* 

```erlang
-type export_reason() ::
          {not_cover_compiled, Module :: module()} |
          {cant_open_file, ExportFile :: file:filename(), FileReason :: term()} |
          not_main_node.
```

# `file_error`
*not exported* 

```erlang
-type file_error() :: eacces | enoent.
```

# `level`
*not exported* 

```erlang
-type level() :: line | clause | function | module.
```

# `mod_file`
*not exported* 

```erlang
-type mod_file() :: (Module :: module()) | (File :: file:filename()).
```

# `mod_files`
*not exported* 

```erlang
-type mod_files() :: mod_file() | [mod_file()].
```

# `modules`
*not exported* 

```erlang
-type modules() :: module() | [module()].
```

# `one_result`
*not exported* 

```erlang
-type one_result() ::
          {ok, {Module :: module(), Value :: analyse_value()}} |
          {ok, [{Item :: analyse_item(), Value :: analyse_value()}]} |
          {error, {not_cover_compiled, module()}}.
```

# `option`
*not exported* 

```erlang
-type option() ::
          {i, Dir :: file:filename()} |
          {d, Macro :: atom()} |
          {d, Macro :: atom(), Value :: term()} |
          export_all.
```

# `analyse`
*since OTP 18.0* 

```erlang
-spec analyse() -> {result, analyse_ok(), analyse_fail()} | {error, not_main_node}.
```

# `analyse`

```erlang
-spec analyse(Analysis) -> {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Analysis :: analysis();
             (Level) -> {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Level :: level();
             (Modules) -> OneResult | {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Modules :: modules(), OneResult :: one_result().
```

Analyzes one or more modules as specified by `Arg`.

If `Arg` is one of the values in [`analysis()`](`t:analysis/0`), this
call is equivalent to [`analyse('_', Arg, function)`](`analyse/3`).

If `Arg` is one of the values in [`level()`](`t:level/0`), this
call is equivalent to [`analyse('_', coverage, Arg)`](`analyse/3`).

Otherwise `Arg` is assumed to be a module name, and this call is equivalent
to [`analyse(Arg, coverage, function)`](`analyse/3`).

> #### Note {: .info }
>
> To analyze a module whose name overlaps with one the values in
> [`analysis()`](`t:analysis/0`) or [`level()`](`t:level/0`), the module
> name has to be in a list. For example, to analyze a module named `calls`:
>
> ```
> cover:analyse([calls]).
> ```

# `analyse`

```erlang
-spec analyse(Analysis, Level) -> {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Analysis :: analysis(), Level :: level();
             (Modules, Analysis) ->
                 OneResult | {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Analysis :: analysis(), Modules :: modules(), OneResult :: one_result();
             (Modules, Level) ->
                 OneResult | {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when Level :: level(), Modules :: modules(), OneResult :: one_result().
```

Analyzes one or more modules as specified by `Arg1` and `Arg2`.

If `Arg1` is one of the values in [`analysis()`](`t:analysis/0`) and
`Arg2` is one of the values in [`level()`](`t:level/0`), this
call is equivalent to [`analyse('_', Arg1, Arg2)`](`analyse/3`).

If `Arg2` is one of the values in [`analysis()`](`t:analysis/0`),
`Arg1` is assumed to be a module and this call is equivalent to
[`analyse(Arg1, Arg2, function)`](`analyse/3`).

If `Arg2` is one of the values in [`level()`](`t:level/0`), `Arg1` is
assumed to be a module and this call is equivalent to [`analyse(Arg1,
coverage, Arg2)`](`analyse/3`).

> #### Note {: .info }
>
> To analyze a module whose name overlaps with one of the values in
> [`analysis()`](`t:analysis/0`), the module name needs to be in a
> list. For example, to analyze a module named `calls`:
>
> ```
> cover:analyse([calls], function).
> ```

# `analyse`

```erlang
-spec analyse(Modules, Analysis, Level) ->
                 OneResult | {result, analyse_ok(), analyse_fail()} | {error, not_main_node}
                 when
                     Analysis :: analysis(),
                     Level :: level(),
                     Modules :: modules(),
                     OneResult :: one_result().
```

Perform analysis of one or more cover-compiled modules, as specified by
`Analysis` and `Level`, by examining the contents of the internal
database.

If `Modules` is an atom (a single module), the return is `OneResult`,
otherwise the return is `{result, Ok, Fail}`.

If `Modules` is atom `'_'`, all modules that have data in the cover data table
are analysed. Note that this includes both cover-compiled modules and imported
modules.

If a given module is not cover-compiled, this is indicated by the error reason
`{not_cover_compiled, Module}`.

# `analyse_to_file`
*since OTP 18.0* 

```erlang
-spec analyse_to_file() -> {result, analyse_file_ok(), analyse_file_fail()} | {error, not_main_node}.
```

# `analyse_to_file`

```erlang
-spec analyse_to_file(Modules) ->
                         Answer |
                         {result, analyse_file_ok(), analyse_file_fail()} |
                         {error, not_main_node}
                         when Modules :: modules(), Answer :: analyse_answer();
                     (Options) ->
                         {result, analyse_file_ok(), analyse_file_fail()} | {error, not_main_node}
                         when Options :: [analyse_option()].
```

If `Arg` is a list of [`analyse_option()`](`t:analyse_option/0`)
options, this call is equivalent to [`analyse_to_file('_',
Arg)`](`analyse_to_file/2`).

Otherwise `Arg` is assumed to be a module, and this call is equivalent to
[`analyse_to_file(Arg, [])`](`analyse_to_file/2`).

> #### Note {: .info }
>
> To analyze a module of the name `html` (which overlaps with an option
> in [`analyse_option()`](`t:analyse_option/0`)), it is necessary to
> use `cover:analyse_to_file/2`:
>
> ```
> cover:analyse_to_file([html], []).
> ```

# `analyse_to_file`

```erlang
-spec analyse_to_file(Modules, Options) ->
                         Answer |
                         {result, analyse_file_ok(), analyse_file_fail()} |
                         {error, not_main_node}
                         when
                             Modules :: modules(),
                             Options :: [analyse_option()],
                             Answer :: analyse_answer().
```

Outputs copies of the source code for the given modules annotated with
execution counts for each executable line.

The output file `OutFile` defaults to `Module.COVER.out`, and to `Module.COVER.html`
if option `html` is used.

If `Modules` is an atom (one module), the return will be `Answer`, otherwise the
return will be a list, `{result, Ok, Fail}`.

If `Modules` is '_', all modules that have data in the Cover data table
are analysed. Note that this includes both cover-compiled modules and imported
modules.

If a module is not cover-compiled, this is indicated by the error reason
`{not_cover_compiled, Module}`.

If the source file and/or the output file cannot be opened using `file:open/2`,
the function returns `{error, {file, File, Reason}}`, where `File` is the file
name and `Reason` is the error reason.

If a module was cover compiled from the `.beam` file, that is, using
`compile_beam/1` or
[`compile_beam_directory/0,1` ](`compile_beam_directory/0`), it is assumed that
the source code can be found in one of the following locations:

- the same directory as the `.beam` file
- `../src` relative to the directory with `.beam` file
- the source path in `Module:module_info(compile)`, in which case two paths
  are examined:
  * first the one constructed by joining `../src` and the tail of the compiled path
    below a trailing `src` component
  * the compiled path itself

If no source code is found, this is indicated by the error reason
`{no_source_code_found, Module}`.

# `async_analyse_to_file`
*since OTP R14B02* 

```erlang
-spec async_analyse_to_file(Module) -> pid() when Module :: module().
```

# `async_analyse_to_file`
*since OTP R14B02* 

```erlang
-spec async_analyse_to_file(Module, OutFile) -> pid()
                               when Module :: module(), OutFile :: file:filename();
                           (Module, Options) -> pid()
                               when Module :: module(), Options :: [Option], Option :: html.
```

# `async_analyse_to_file`
*since OTP R14B02* 

```erlang
-spec async_analyse_to_file(Module, OutFile, Options) -> pid()
                               when
                                   Module :: module(),
                                   OutFile :: file:filename(),
                                   Options :: [Option],
                                   Option :: html.
```

This function works the same way as
[`analyse_to_file/2`](`analyse_to_file/2`) except that it is asynchronous instead
of synchronous.

The spawned process will link with the caller when created. If an
error of type [`analyse_rsn()`](`t:analyse_rsn/0`) occurs while doing
the cover analysis the process will crash with the same error reason
as [`analyse_to_file`](`analyse_to_file/1`) would return.

# `compile`

```erlang
-spec compile(ModFiles) -> Result | [Result] when ModFiles :: mod_files(), Result :: compile_result().
```

# `compile`

```erlang
-spec compile(ModFiles, Options) -> Result | [Result]
                 when ModFiles :: mod_files(), Options :: [option()], Result :: compile_result().
```

# `compile_beam`

```erlang
-spec compile_beam(ModFiles) -> Result | [Result]
                      when ModFiles :: beam_mod_files(), Result :: compile_beam_result().
```

Cover-compiles one or more modules based `.beam` files containing
abstract code (option `debug_info`).

Cover-compiling from `.beam` files is faster than compiling from
source and less hassle, because there is no need to supply options for
include paths or macros. However, the existing `.beam` files must have
been compiled with option
[`debug_info`](`e:compiler:compile.md#debug_info`) so that they contain
[*abstract code*](`e:erts:absform`).

If abstract code is missing, the error reason `{no_abstract_code,
BeamFile}` is returned. If the abstract code is encrypted, and no key
is available for decrypting it, the error reason
`{encrypted_abstract_code, BeamFile}` is returned.

If only the module name (that is, not the full name of the `.beam`
file) is given to this function, the `.beam` file is found by calling
[`code:which(Module)`](`code:which/1`). If no `.beam` file is found,
the error reason `non_existing` is returned. If the module is already
cover compiled with [`compile_beam/1`](`compile_beam/1`), the `.beam`
file will be picked from the same location as the first time it was
compiled. If the module is already cover-compiled with
`compile_module/2`, there is no way to find the correct `.beam` file,
so the error reason `{already_cover_compiled, no_beam_found, Module}`
is returned.

`{error, BeamFile}` is returned if the compiled code cannot be loaded on the
node.

If a list of `ModFiles` is given as input, a list of `Result` will be returned.
The order of the returned list is undefined.

# `compile_beam_directory`

```erlang
-spec compile_beam_directory() -> [Result] | {error, Reason}
                                when Reason :: file_error(), Result :: compile_beam_result().
```

# `compile_beam_directory`

```erlang
-spec compile_beam_directory(Dir) -> [Result] | {error, Reason}
                                when
                                    Dir :: file:filename(),
                                    Reason :: file_error(),
                                    Result :: compile_beam_result().
```

Cover-compiles all `.beam` files in directory `Dir` in the same way
as `compile_beam/1`.

This function returns a list of [`compile_beam_result()`](`t:compile_beam_result/0`)
if successful. Otherwise, it returns  `{error, eacces}` if the directory is not
readable, and `{error, enoent}` if the directory does not exist.

# `compile_directory`

```erlang
-spec compile_directory() -> [Result] | {error, Reason}
                           when Reason :: file_error(), Result :: compile_result().
```

# `compile_directory`

```erlang
-spec compile_directory(Dir) -> [Result] | {error, Reason}
                           when
                               Dir :: file:filename(),
                               Reason :: file_error(),
                               Result :: compile_result().
```

# `compile_directory`

```erlang
-spec compile_directory(Dir, Options) -> [Result] | {error, Reason}
                           when
                               Dir :: file:filename(),
                               Options :: [option()],
                               Reason :: file_error(),
                               Result :: compile_result().
```

Compiles all modules (`.erl` files) in a directory `Dir` for Cover analysis the
same way as [`compile_module/1,2`](`compile_module/1`) and returns a list of
[`Result`](`t:compile_result/0`).

This function returns `{error, eacces}` if the directory is not readable or
`{error, enoent}` if the directory does not exist.

# `compile_module`

```erlang
-spec compile_module(ModFiles) -> Result | [Result]
                        when ModFiles :: mod_files(), Result :: compile_result().
```

# `compile_module`

```erlang
-spec compile_module(ModFiles, Options) -> Result | [Result]
                        when ModFiles :: mod_files(), Options :: [option()], Result :: compile_result().
```

Cover-compiles one or more modules.

The module is given by its module name `Module` or by its file name
`File`.

The `.erl` extension can be omitted. If the module is not located in
the current directory, the full path to it must be specified.

`Options` is a list of compiler options. Only options defining include
file directories and macros are passed to `compile:file/2`;
everything else is ignored.

If the module is successfully cover-compiled, the function returns
`{ok, Module}`. Otherwise the function returns `{error, File}`. Errors and
warnings are printed as they occur.

If a list of `ModFiles` is given as input, a list of [`Result`](`t:compile_result/0`)
will be returned. The order of the returned results in the list is undefined.

Note that the internal database is initialized during the compilation,
which means that any previously collected coverage data for the module
is lost.

# `export`

```erlang
-spec export(File) -> ok | {error, Reason} when File :: file:filename(), Reason :: export_reason().
```

# `export`

```erlang
-spec export(File, Module) -> ok | {error, Reason}
                when File :: file:filename(), Module :: module(), Reason :: export_reason().
```

Exports the current coverage data for `Module` to the file `ExportFile`.

It is recommended to name the `ExportFile` with the extension `.coverdata`.

If `Module` is '_', data for all cover-compiled or earlier imported
modules is exported.

This function is useful if coverage data from different systems is to be merged.

See also `import/1`.

# `flush`
*since OTP R16B* 

```erlang
-spec flush(Nodes) -> ok | {error, not_main_node} when Nodes :: node() | [node()].
```

Fetches data from the Cover database on the remote nodes and stores it on the main
node.

# `import`

```erlang
-spec import(ExportFile) -> ok | {error, Reason}
                when
                    ExportFile :: file:filename(),
                    Reason :: {cant_open_file, ExportFile, FileReason :: term()} | not_main_node.
```

Imports coverage data from the file `ExportFile` created with
[`export/1,2`](`export/2`).

Any analysis performed after this call will include the imported data.

Note that when compiling a module _all existing coverage data is removed_,
including imported data. If a module is already compiled when data is imported,
the imported data is _added_ to the existing coverage data.

Coverage data from several export files can be imported into one system. The
coverage data is then added up when analysing.

Coverage data for a module cannot be imported from the same file twice unless
the module is first reset or compiled. The check is based on the filename, so
you can easily fool the system by renaming your export file.

# `imported`

```erlang
-spec imported() -> [file:filename()] | {error, not_main_node}.
```

Returns a list of all imported files.

# `imported_modules`

```erlang
-spec imported_modules() -> [module()] | {error, not_main_node}.
```

Returns a list of all modules for which there are imported data.

# `is_compiled`

```erlang
-spec is_compiled(Module) -> {file, File :: file:filename()} | false | {error, not_main_node}
                     when Module :: module().
```

Returns `{file, File}` if the module `Module` is cover-compiled, or `false`
otherwise.

`File` is the `.erl` file used by [`compile_module/1,2`](`compile_module/2`)
or the `.beam` file used by `compile_beam/1`.

# `local_only`
*since OTP 22.0* 

```erlang
-spec local_only() -> ok | {error, too_late}.
```

Only support running Cover on the local node.

This function has to be called before any modules have been compiled or
any nodes added. When running in this mode, modules will be
cover-compiled in a more efficient way, but the resulting code will
only work on the same node they were compiled on.

# `modules`

```erlang
-spec modules() -> [module()] | {error, not_main_node}.
```

Returns a list with all modules that are currently cover-compiled.

# `reset`

```erlang
-spec reset() -> ok | {error, not_main_node}.
```

Resets all coverage data for all cover-compiled modules in the Cover
database on all nodes.

# `reset`

```erlang
-spec reset(Module) -> ok | {error, not_main_node} | {error, {not_cover_compiled, Module}}
               when Module :: module().
```

Resets all coverage data for the cover-compiled module `Module` in the Cover
database on all nodes.

If `Module` is not cover-compiled, the function returns
`{error, {not_cover_compiled, Module}}`.

# `start`

```erlang
-spec start() -> {ok, pid()} | {error, Reason} when Reason :: {already_started, pid()} | term().
```

Starts the Cover server which owns the Cover internal database. This function is
called automatically by the other functions in the module.

# `start`

```erlang
-spec start(Nodes) -> {ok, StartedNodes} | {error, not_main_node} | {error, local_only}
               when Nodes :: node() | [node()], StartedNodes :: [node()].
```

Starts a Cover server on the each of given nodes, and loads all cover compiled
modules.

This call will fail if `cover:local_only/0` has been called.

# `stop`

```erlang
-spec stop() -> ok | {error, not_main_node}.
```

Stops the Cover server and unloads all cover-compiled code.

# `stop`

```erlang
-spec stop(Nodes) -> ok | {error, not_main_node} when Nodes :: node() | [node()].
```

Stops the Cover server and unloads all cover-compiled code on the given nodes.

Data stored in the Cover database on the remote nodes is fetched and stored on
the main node.

# `which_nodes`

```erlang
-spec which_nodes() -> [node()].
```

Returns a list with all nodes that are part of the coverage analysis.

Note that the current node is not included, because it is always part
of the analysis.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
