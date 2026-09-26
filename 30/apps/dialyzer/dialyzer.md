# `dialyzer`
[🔗](https://github.com/erlang/otp/blob/master/lib/dialyzer/src/dialyzer.erl#L33)

Dialyzer is a **DI**screpancy **A**na**LYZ**er for **ER**lang programs.

Dialyzer is a static analysis tool that identifies software
discrepancies, such as definite type errors, code that is unreachable
because of programming errors, and unnecessary tests in single Erlang
modules or an entire codebase.

Dialyzer starts its analysis from either debug-compiled BEAM code or
from Erlang source code. The file and line number of a discrepancy is
reported along with an indication of the nature of the discrepancy.
Dialyzer bases its analysis on the concept of success typings,
ensuring sound warnings without false positives.

[](){: #command_line }

## Using Dialyzer from the Command Line

This section provides a brief description of the options available
when running Dialyzer from the command line. The same information can
be obtained by writing the following in a shell:

```text
dialyzer --help
```

_Exit status of the command-line version:_

- **`0`** - No problems were found during the analysis and no warnings were
  emitted.

- **`1`** - Problems were found during the analysis.

- **`2`** - No problems were found during the analysis, but warnings were
  emitted.

_Usage:_

```text
dialyzer [--add_to_plt] [--apps applications] [--build_plt]
         [--check_plt] [-Ddefine]* [-Dname]* [--dump_callgraph file]
         [--error_location flag] [files_or_dirs] [--fullpath]
         [--get_warnings] [--help] [-I include_dir]*
         [--incremental] [--metrics_file] [--no_check_plt] [--no_indentation]
         [--no_spec] [-o outfile] [--output_plt file] [-pa dir]* [--plt plt]
         [--plt_info] [--plts plt*] [--quiet] [-r dirs] [--raw]
         [--remove_from_plt] [--shell] [--src] [--statistics] [--verbose]
         [--version] [--warning_apps applications] [-Wwarn]*
```

> #### Note {: .info }
>
> \* denotes that multiple occurrences of the option are possible.

_Options of the command-line version:_

- **`--add_to_plt`** - The PLT is extended to also include the files specified
  with `-c` and `-r`. Use `--plt` to specify which PLT to start from, and
  `--output_plt` to specify where to put the PLT. Note that files already
  included in the PLT will be reanalyzed if they depend on the new files.
  This option only works for BEAM files, not source files.

- **`--apps applications`** - By default, warnings will be reported to all
  applications given by `--apps`. However, if `--warning_apps` is used, only
  those applications given to `--warning_apps` will have warnings reported. All
  applications given by `--apps`, but not `--warning_apps`, will be analysed to
  provide context to the analysis, but warnings will not be reported for them.
  For example, you may want to include libraries you depend on in the analysis
  with `--apps` so discrepancies in their usage can be found, but only include
  your own code with `--warning_apps` so that discrepancies are only reported in
  code that you own.

- **`--warning_apps applications`** - This option is typically used when
  building or modifying a PLT as in:

  ```text
  dialyzer --build_plt --apps erts kernel stdlib mnesia ...
  ```

  to refer conveniently to library applications corresponding to the
  Erlang/OTP installation. This option can also be used during
  analysis to refer to Erlang/OTP applications. File or directory
  names can also be included, as in:

  ```text
  dialyzer --apps inets ssl ./ebin ../other_lib/ebin/my_module.beam
  ```

- **`--build_plt`** - The analysis starts from an empty PLT and creates a new
  one from the files specified with `-c` and `-r`. This option only works for
  BEAM files. To override the default PLT location, use `--plt` or
  `--output_plt`.

- **`--check_plt`** - Check the PLT for consistency and rebuild it if it is not
  up-to-date.

- **`-Dname` (or `-Dname=value`)** - When analyzing from source, pass the define
  to Dialyzer. (\*\*)

- **`--dump_callgraph file`** - Dump the call graph into the specified file
  whose format is determined by the filename extension. Supported extensions
  are: `raw`, `dot`, and `ps`. If something else is used as filename extension,
  the default `.raw` format is used.

- **`--error_location column | line`{: #error_location }** - Use a pair
  `{Line, Column}` or an integer `Line` to pinpoint the location of warnings.
  The default is to use a pair `{Line, Column}`. When formatted, the line and
  the column are separated by a colon.

- **`files_or_dirs` (for backward compatibility also as `-c files_or_dirs`)** -
  Use Dialyzer from the command line to detect defects in the specified files or
  directories containing `.erl` or `.beam` files, depending on the type of the
  analysis.

- **`--fullpath`** - Display the full path names of files for which warnings are
  emitted.

- **`--get_warnings`** - Make Dialyzer emit warnings even when manipulating the
  PLT. Warnings are only emitted for files that are analyzed.

- **`--help` (or `-h`)** - Print a help message and exit.

- **`-I include_dir`** - When analyzing from source, pass the `include_dir` to
  Dialyzer. (\*\*)

- **`--input_list_file file`** - Analyze the file names that are listed in the
  specified file (one file name per line).

- **`--no_check_plt`** - Skip the PLT check when running Dialyzer. This is
  useful when working with installed PLTs that never change.

- **`--incremental`** - The analysis starts from an existing incremental PLT, or
  builds one from scratch if one does not exist, and runs the minimal amount of
  additional analysis to report all issues in the given set of apps. Notably,
  incremental PLT files are not compatible with "classic" PLT files, and vice
  versa. The initial incremental PLT will be updated unless an alternative
  output incremental PLT is given.

- **`--no_indentation`** - Do not insert line breaks in types, contracts, and
  Erlang Code when formatting warnings.

- **`--no_spec`** - Ignore functions specs. This is useful for debugging when
  one suspects that some specs are incorrect.

- **`-o outfile` (or `--output outfile`)** - When using Dialyzer from the
  command line, send the analysis results to the specified outfile rather than
  to `stdout`.

- **`--metrics_file file`** - Write metrics about Dialyzer's incrementality (for
  example, total number of modules considered, how many modules were changed
  since the PLT was last updated, how many modules needed to be analyzed) to a
  file. This can be useful for tracking and debugging Dialyzer's incrementality.

- **`--output_plt file`** - Store the PLT at the specified file after building
  it.

- **`-pa dir`** - Include `dir` in the path for Erlang. This is useful when
  analyzing files that have `-include_lib()` directives.

- **`--plt plt`** - Use the specified PLT as the initial PLT. If the PLT was
  built during setup, the files are checked for consistency.

- **`--plt_info`** - Make Dialyzer print information about the PLT and then
  quit. The PLT can be specified with `--plt(s)`.

- **`--plts plt*`** - Merge the specified PLTs to create the initial PLT. This
  requires that the PLTs are disjoint (that is, do not have any module appearing
  in more than one PLT). The PLTs are created in the usual way:

  ```text
  dialyzer --build_plt --output_plt plt_1 files_to_include
  ...
  dialyzer --build_plt --output_plt plt_n files_to_include
  ```

  They can then be used in either of the following ways:

  ```text
  dialyzer files_to_analyze --plts plt_1 ... plt_n
  ```

  or

  ```text
  dialyzer --plts plt_1 ... plt_n -- files_to_analyze
  ```

  Notice the `--` delimiter in the second case.

- **`--quiet` (or `-q`)** - Make Dialyzer a bit more quiet.

- **`-r dirs`** - Same as `files_or_dirs`, but the specified directories are
  searched recursively for subdirectories containing `.erl` or `.beam` files in
  them, depending on the type of analysis.

- **`--raw`** - When using Dialyzer from the command line, output the raw
  analysis results (Erlang terms) instead of the formatted result. The raw
  format is easier to post-process (for example, to filter warnings or to output
  HTML pages).

- **`--remove_from_plt`** - The information from the files specified with `-c`
  and `-r` is removed from the PLT. Notice that this can cause a reanalysis of
  the remaining dependent files.

- **`--src`** - Override the default, which is to analyze BEAM files, and
  analyze starting from Erlang source code instead.

- **`--statistics`** - Print information about the progress of execution
  (analysis phases, time spent in each, and size of the relative input).

- **`--verbose`** - Make Dialyzer a bit more verbose.

- **`--version` (or `-v`)** - Print the Dialyzer version and some more
  information and exit.

- **`-Wwarn`** - A family of options that selectively turn on/off warnings. (For
  help on the names of warnings, use `dialyzer -Whelp`.) Notice that the options
  can also be specified in the file with a `-dialyzer()` attribute. For details,
  see section
  [Requesting or Suppressing Warnings in Source Files](`m:dialyzer#suppression`).

> #### Note {: .info }
>
> \*\* the syntax of defines and includes is the same as that used by
> [erlc](`e:erts:erlc_cmd.md`).

[](){: #warning_options }

_Warning options:_

- **`-Werror_handling` (\*\*\*)** - Include warnings for functions that only
  return by an exception.

- **`-Wextra_return` (\*\*\*)** - Warn about functions whose specification
  includes types that the function cannot return.

- **`-Wmissing_return` (\*\*\*)** - Warn about functions that return values that
  are not part of the specification.

- **`-Wno_behaviours`** - Suppress warnings about behavior callbacks that drift
  from the published recommended interfaces.

- **`-Wno_contracts`** - Suppress warnings about invalid contracts.

- **`-Wno_fail_call`** - Suppress warnings for failing calls.

- **`-Wno_fun_app`** - Suppress warnings for fun applications that will fail.

- **`-Wno_improper_lists`** - Suppress warnings for construction of improper
  lists.

- **`-Wno_match`** - Suppress warnings for patterns that are unused or cannot
  match.

- **`-Wno_missing_calls`** - Suppress warnings about calls to missing functions.

- **`-Wno_opaque`** - Suppress warnings for violations of opacity of data types.

- **`-Wno_return`** - Suppress warnings for functions that will never return a
  value.

- **`-Wno_undefined_callbacks`** - Suppress warnings about behaviors that have
  no `-callback` attributes for their callbacks.

- **`-Wno_unused`** - Suppress warnings for unused functions.

- **`-Wno_unknown`** - Suppress warnings about unknown functions and types. The
  default is to warn about unknown functions and types when setting the exit
  status. When using Dialyzer from Erlang, warnings about unknown functions and
  types are returned.

- **`-Wunderspecs` (\*\*\*)** - Warn about underspecified functions (the
  specification is strictly more allowing than the success typing).

- **`-Wunmatched_returns` (\*\*\*)** - Include warnings for function calls that
  ignore a structured return value or do not match against one of many possible
  return values. However, no warnings are included if the possible return values
  are a union of atoms or a union of numbers.

The following options are also available, but their use is not recommended (they
are mostly for Dialyzer developers and internal debugging):

- **`-Woverspecs` (\*\*\*)** - Warn about overspecified functions (the
  specification is strictly less allowing than the success typing).

- **`-Wspecdiffs` (\*\*\*)** - Warn when the specification is different than the
  success typing.

> #### Note {: .info }
>
> \*\*\* denotes options that turn on warnings rather than turning them off.

The following options are not strictly needed as they specify the
default. They are primarily intended to be used with the `-dialyzer`
attribute. For an example see section [Requesting or Suppressing
Warnings in Source Files](`m:dialyzer#suppression`).

- **`-Wno_underspecs`** - Suppress warnings about underspecified functions (the
  specification is strictly more allowing than the success typing).

- **`-Wno_extra_return`** - Suppress warnings about functions whose
  specification includes types that the function cannot return.

- **`-Wno_missing_return`** - Suppress warnings about functions that return
  values that are not part of the specification.

## Using Dialyzer from Erlang

Dialyzer can be used directly from Erlang. The options are similar to the ones
given from the command line. See section
[Using Dialyzer from the Command Line](`m:dialyzer#command_line`).

## Default Dialyzer Options

The (host operating system) environment variable `ERL_COMPILER_OPTIONS` can be
used to give default Dialyzer options. Its value must be a valid Erlang term. If
the value is a list, it is used as is. If it is not a list, it is put into a
list.

The list is appended to any options given to `run/1` or on the command line.

The list can be retrieved with `compile:env_compiler_options/0`.

Currently the only option used is the
[`error_location`](`m:dialyzer#error_location`) option.

_Dialyzer configuration file:_

Dialyzer's configuration file may also be used to augment the default options
and those given directly to the Dialyzer command. It is commonly used to avoid
repeating options which would otherwise need to be given explicitly to Dialyzer
on every invocation.

The location of the configuration file can be set via the `DIALYZER_CONFIG`
environment variable, and defaults to within the `user_config` from
`filename:basedir/3`.

An example configuration file's contents might be:

```erlang
      {incremental,
        {default_apps,[stdlib,kernel,erts]},
        {default_warning_apps,[stdlib]}
      }.
      {warnings, [no_improper_lists]}.
      {add_pathsa,["/users/samwise/potatoes/ebin"]}.
      {add_pathsz,["/users/smeagol/fish/ebin"]}.
```

[](){: #suppression }

## Requesting or Suppressing Warnings in Source Files

Attribute `-dialyzer()` can be used for turning off warnings in a module by
specifying functions or warning options. For example, to turn off all warnings
for the function `f/0`, include the following line:

```erlang
-dialyzer({nowarn_function, f/0}).
```

To turn off warnings for improper lists, add the following line to the source
file:

```text
-dialyzer(no_improper_lists).
```

Attribute `-dialyzer()` is allowed after function declarations. Lists of warning
options or functions are allowed:

```erlang
-dialyzer([{nowarn_function, [f/0]}, no_improper_lists]).
```

Warning options can be restricted to functions:

```erlang
-dialyzer({no_improper_lists, g/0}).
```

```erlang
-dialyzer({[no_return, no_match], [g/0, h/0]}).
```

The warning option for underspecified functions, `-Wunderspecs`, can result in
useful warnings, but often functions with specifications that are strictly more
allowing than the success typing cannot easily be modified to be less allowing.
To turn off the warning for underspecified function `f/0`, include the following
line:

```erlang
-dialyzer({no_underspecs, f/0}).
```

For help on the warning options, use `dialyzer -Whelp`. The options are also
enumerated, see type `t:warn_option/0`.

Attribute `-dialyzer()` can also be used for turning on warnings. For example,
if a module has been fixed regarding unmatched returns, adding the following
line can help in assuring that no new unmatched return warnings are introduced:

```text
-dialyzer(unmatched_returns).
```

# `dial_option`
*not exported* 

```erlang
-type dial_option() ::
          {files, [FileName :: file:filename()]} |
          {files_rec, [DirName :: file:filename()]} |
          {defines, [{Macro :: atom(), Value :: term()}]} |
          {from, src_code | byte_code} |
          {init_plt, FileName :: file:filename()} |
          {plts, [FileName :: file:filename()]} |
          {include_dirs, [DirName :: file:filename()]} |
          {output_file, FileName :: file:filename()} |
          {metrics_file, FileName :: file:filename()} |
          {module_lookup_file, FileName :: file:filename()} |
          {output_plt, FileName :: file:filename()} |
          {check_plt, boolean()} |
          {analysis_type, succ_typings | plt_add | plt_build | plt_check | plt_remove | incremental} |
          {warnings, [warn_option()]} |
          {get_warnings, boolean()} |
          {use_spec, boolean()} |
          {filename_opt, filename_opt()} |
          {callgraph_file, file:filename()} |
          {mod_deps_file, file:filename()} |
          {warning_files_rec, [DirName :: file:filename()]} |
          {error_location, error_location()}.
```

Option `from` defaults to `byte_code`. Options `init_plt` and `plts` change the
default.

# `dial_warn_tag`
*not exported* 

```erlang
-type dial_warn_tag() ::
          warn_behaviour | warn_bin_construction | warn_callgraph | warn_contract_extra_return |
          warn_contract_missing_return | warn_contract_not_equal | warn_contract_opaque |
          warn_contract_range | warn_contract_subtype | warn_contract_supertype | warn_contract_syntax |
          warn_contract_types | warn_failing_call | warn_fun_app | warn_map_construction |
          warn_matching | warn_non_proper_list | warn_not_called | warn_opaque |
          warn_overlapping_contract | warn_return_no_exit | warn_return_only_exit |
          warn_undefined_callbacks | warn_unknown | warn_umatched_return.
```

# `dial_warning`
*not exported* 

```erlang
-type dial_warning() :: {Tag :: dial_warn_tag(), Id :: file_location(), Msg :: {atom(), [term()]}}.
```

# `error_location`
*not exported* 

```erlang
-type error_location() :: column | line.
```

If the value of this option is `line`, an integer `Line` is used as `Location`
in messages. If the value is `column`, a pair `{Line, Column}` is used as
`Location`. The default is `column`.

# `file_location`
*not exported* 

```erlang
-type file_location() :: {File :: file:filename(), Location :: erl_anno:location()}.
```

# `filename_opt`
*not exported* 

```erlang
-type filename_opt() :: basename | fullpath.
```

# `format_option`
*not exported* 

```erlang
-type format_option() ::
          {indent_opt, boolean()} | {filename_opt, filename_opt()} | {error_location, error_location()}.
```

# `warn_option`
*not exported* 

```erlang
-type warn_option() ::
          error_handling | no_behaviours | no_contracts | no_fail_call | no_fun_app |
          no_improper_lists | no_match | no_missing_calls | no_opaque | no_return |
          no_undefined_callbacks | no_underspecs | no_unknown | no_unused | underspecs | unknown |
          unmatched_returns | overspecs | specdiffs | overlapping_contract | extra_return |
          no_extra_return | missing_return | no_missing_return | opaque_union.
```

See section [Warning options](`m:dialyzer#warning_options`) for a description of
the warning options.

# `format_warning`

```erlang
-spec format_warning(Warnings) -> string() when Warnings :: dial_warning().
```

Get a string from warnings as returned by `run/1`.

# `format_warning`
*since R14B02* 

```erlang
-spec format_warning(Warnings, Options) -> string()
                        when Warnings :: dial_warning(), Options :: filename_opt() | [format_option()].
```

Get a string from warnings as returned by `run/1`.

If `indent_opt` is set to `true` (default), line breaks are inserted in types,
contracts, and Erlang code to improve readability.

If `error_location` is set to `column` (default), locations are formatted as
`Line:Column` if the column number is available, otherwise locations are
formatted as `Line` even if the column number is available.

# `plt_info`

```erlang
-spec plt_info(Plt) -> {ok, ClassicResult | IncrementalResult} | {error, Reason}
                  when
                      Plt :: file:filename(),
                      ClassicResult :: [{files, [file:filename()]}],
                      IncrementalResult :: {incremental, [{modules, [module()]}]},
                      Reason :: not_valid | no_such_file | read_error.
```

Returns information about the specified PLT.

# `run`

```erlang
-spec run(Options) -> Warnings when Options :: [dial_option()], Warnings :: [dial_warning()].
```

Run Dialyzer and return warnings.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
