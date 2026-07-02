# `compile`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/compiler/src/compile.erl#L24)

Erlang Compiler

This module provides an interface to the standard Erlang compiler. It can
generate either a file containing the object code or return a binary
that can be loaded directly.

## Default Compiler Options

The (host operating system) environment variable `ERL_COMPILER_OPTIONS` can be
used to give default compiler options. Its value must be a valid Erlang term. If
the value is a list, it is used as is. If it is not a list, it is put into a
list.

The list is appended to any options given to `file/2`, `forms/2`, and
[output_generated/2](`output_generated/1`). Use the alternative functions
`noenv_file/2`, `noenv_forms/2`, or
[noenv_output_generated/2](`noenv_output_generated/1`) if you do not want the
environment variable to be consulted, for example, if you are calling the
compiler recursively from inside a parse transform.

The list can be retrieved with `env_compiler_options/0`.

## Order of Compiler Options

Options given in the `compile()` attribute in the source code take
precedence over options given to the compiler, which in turn take
precedence over options given in the environment.

A later compiler option takes precedence over an earlier one in the
option list. Example:

```
compile:file(something, [nowarn_missing_spec,warn_missing_spec]).
```

Warnings will be emitted for functions without specifications, unless
the source code for module `something` contains a `compile(nowarn_missing_spec)`
attribute.

> #### Change {: .info }
>
> In Erlang/OTP 26 and earlier, the option order was the opposite of what
> is described here.

## Inlining

The compiler can do function inlining within an Erlang
module. Inlining means that a call to a function is replaced with the
function body with the arguments replaced with the actual values. The
semantics are preserved, except if exceptions are generated in the
inlined code, in which case exceptions are reported as occurring in
the function the body was inlined into. Also, `function_clause`
exceptions are converted to similar `case_clause` exceptions.

When a function is inlined, the original function is kept if it is exported
(either by an explicit export or if the option `export_all` was given) or if not
all calls to the function are inlined.

Inlining does not necessarily improve running time. For example, inlining can
increase Beam stack use, which probably is detrimental to performance for
recursive functions.

Inlining is never default. It must be explicitly enabled with a compiler option
or a `-compile()` attribute in the source module.

To enable inlining, either use the option `inline` to let the compiler decide
which functions to inline, or `{inline,[{Name,Arity},...]}` to have the compiler
inline all calls to the given functions. If the option is given inside a
`compile` directive in an Erlang module, `{Name,Arity}` can be written as
`Name/Arity`.

Example of explicit inlining:

```erlang
-compile({inline,[pi/0]}).

pi() -> 3.1416.
```

Example of implicit inlining:

```text
-compile(inline).
```

The option `{inline_size,Size}` controls how large functions that are allowed to
be inlined. Default is `24`, which keeps the size of the inlined code roughly
the same as the un-inlined version (only relatively small functions are
inlined).

Example:

```erlang
%% Aggressive inlining - will increase code size.
-compile(inline).
-compile({inline_size,100}).
```

## Inlining of List Functions

The compiler can also inline various list manipulation functions from the module
`list` in STDLIB.

This feature must be explicitly enabled with a compiler option or a `-compile()`
attribute in the source module.

To enable inlining of list functions, use option `inline_list_funcs`.

The following functions are inlined:

- `lists:all/2`
- `lists:any/2`
- `lists:foreach/2`
- `lists:map/2`
- `lists:flatmap/2`
- `lists:filter/2`
- `lists:foldl/3`
- `lists:foldr/3`
- `lists:mapfoldl/3`
- `lists:mapfoldr/3`

## Parse Transformations

Parse transformations are used when a programmer wants to use Erlang syntax but
with different semantics. The original Erlang code is then transformed into
other Erlang code.

See `m:erl_id_trans` for an example and an explanation of the function
`parse_transform_info/0`.

## See Also

`m:epp`, `m:erl_expand_records`, `m:erl_id_trans`, `m:erl_lint`, `m:beam_lib`

# `abstract_code`
*not exported* 

```elixir
-type abstract_code() :: [erl_parse:abstract_form()].
```

# `bin_ret`
*not exported* 

```elixir
-type bin_ret() :: {ok, module(), binary()} | {ok, module(), binary(), warnings()}.
```

# `comp_ret`

```elixir
-type comp_ret() :: mod_ret() | bin_ret() | err_ret().
```

# `err_ret`
*not exported* 

```elixir
-type err_ret() :: error | {error, errors(), warnings()}.
```

# `error_description`
*not exported* 

```elixir
-type error_description() :: erl_lint:error_description().
```

# `error_info`
*not exported* 

```elixir
-type error_info() :: erl_lint:error_info().
```

# `errors`
*not exported* 

```elixir
-type errors() :: [{file:filename(), [error_info()]}].
```

# `forms`

```elixir
-type forms() :: abstract_code() | cerl:c_module().
```

List of Erlang abstract or Core Erlang format representations, as used by
`forms/2`.

# `mod_ret`
*not exported* 

```elixir
-type mod_ret() ::
          {ok, module()} |
          {ok, module(), cerl:c_module()} |
          {ok, module() | [], abstract_code()} |
          {ok, module(), warnings()}.
```

# `option`

```elixir
-type option() :: atom() | {atom(), term()} | {d, atom(), term()}.
```

See `file/2` for detailed description.

# `warnings`
*not exported* 

```elixir
-type warnings() :: [{file:filename(), [error_info()]}].
```

# `env_compiler_options`
*since OTP 19.0* 

```elixir
-spec env_compiler_options() -> [term()].
```

Return compiler options given via the environment variable
`ERL_COMPILER_OPTIONS`. If the value is a list, it is returned as is. If it is
not a list, it is put into a list.

# `file`

```elixir
-spec file(module() | file:filename()) -> CompRet :: comp_ret().
```

Is the same as
[`file(File, [verbose,report_errors,report_warnings])`](`file/2`).

# `file`

```elixir
-spec file(File :: module() | file:filename(), Options :: [option()] | option()) ->
              CompRet :: comp_ret().
```

Compiles the code in the file `File`, which is an Erlang source code file
without the `.erl` extension.

`Options` determine the behavior of the compiler.

Returns `{ok,ModuleName}` if successful, or `error` if there are errors. An
object code file is created if the compilation succeeds without errors. It is
considered to be an error if the module name in the source code is not the same
as the basename of the output file.

Available options:

- **`brief`** - Restricts error and warning messages to a single line
  of output.  As of Erlang/OTP 24, the compiler will by default also
  display the part of the source code that the message refers to.

- **`basic_validation`** - This option is a fast way to test whether a module
  will compile successfully. This is useful for code generators that want to
  verify the code that they emit. No code is generated. If warnings are enabled,
  warnings generated by the `erl_lint` module (such as warnings for unused
  variables and functions) are also returned.

  Use option `strong_validation` to generate all warnings that the compiler
  would generate.

- **`strong_validation`** - Similar to option `basic_validation`. No code is
  generated, but more compiler passes are run to ensure that warnings generated
  by the optimization passes are generated (such as clauses that will not match,
  or expressions that are guaranteed to fail with an exception at runtime).

- **`no_docs`**{: #no_docs } - The compiler by default extracts
  [documentation](`e:system:documentation.md`) from
  [`-doc` attributes](`e:system:modules.md#documentation-attributes`) and places
  them in the [`Docs` chunk](`t:beam_lib:chunkid/0`) according to
  [EEP-48](`e:kernel:eep48_chapter.md`).

  This option switches off the placement of
  [`-doc` attributes](`e:system:modules.md#documentation-attributes`) in the
  [`Docs` chunk](`t:beam_lib:chunkid/0`).

- **`binary`** - The compiler returns the object code in a binary instead of
  creating an object file. If successful, the compiler returns
  `{ok,ModuleName,Binary}`.

- **`bin_opt_info`** - The compiler will emit informational warnings about
  binary matching optimizations (both successful and unsuccessful). For more
  information, see the section about
  [bin_opt_info](`e:system:binaryhandling.md#bin_opt_info`) in the Efficiency
  Guide.

- **`{compile_info, [{atom(), term()}]}`** - Allows compilers built on top of
  `compile` to attach extra compilation metadata to the `compile_info` chunk in
  the generated BEAM file.

  It is advised for compilers to remove all non-deterministic information if the
  `deterministic` option is supported and it was supplied by the user.

- **`compressed`** - The compiler will compress the generated object code, which
  can be useful for embedded systems.

- **`debug_info`** - [](){: #debug_info } Includes debug information in the form
  of [Erlang Abstract Format](`e:erts:absform.md`) in the `debug_info` chunk of
  the compiled beam module. Tools such as Debugger, Xref, and Cover require the
  debug information to be included.

  _Warning_: Source code can be reconstructed from the debug information. Use
  encrypted debug information (`encrypt_debug_info`) to prevent this.

  For details, see [beam_lib(3)](`m:beam_lib#debug_info`).

- **`{debug_info, {Backend, Data}}`** - [](){: #debug_info_backend } Includes
  custom debug information in the form of a `Backend` module with custom `Data`
  in the compiled beam module. The given module must implement a `debug_info/4`
  function and is responsible for generating different code representations, as
  described in the `debug_info` under [beam_lib(3)](`m:beam_lib#debug_info`).

  _Warning_: Source code can be reconstructed from the debug information. Use
  encrypted debug information (`encrypt_debug_info`) to prevent this.

- **`{debug_info_key,KeyString}`**

- **`{debug_info_key,{Mode,KeyString}}`** - [](){: #debug_info_key } Includes
  debug information, but encrypts it so that it cannot be accessed without
  supplying the key. (To give option `debug_info` as well is allowed, but not
  necessary.) Using this option is a good way to always have the debug
  information available during testing, yet protecting the source code.

  `Mode` is the type of crypto algorithm to be used for encrypting the debug
  information. The default (and currently the only) type is `des3_cbc`.

  For details, see [beam_lib(3)](`m:beam_lib#debug_info`).

- **`encrypt_debug_info`** - [](){: #encrypt_debug_info } Similar to the
  `debug_info_key` option, but the key is read from an `.erlang.crypt` file.

  For details, see [beam_lib(3)](`m:beam_lib#debug_info`).

- **`deterministic`** - Omit the `options` and `source` tuples in the list
  returned by `Module:module_info(compile)`, and reduce the paths in stack
  traces to the module name alone. This option will make it easier to achieve
  reproducible builds.

- **`{feature, Feature, enable | disable}`** - [](){: #feature-option } Enable
  (disable) the [feature](`e:system:features.md#features`) `Feature` during
  compilation. The special feature `all` can be used to enable (disable) all
  features.

  > #### Note {: .info }
  >
  > This option has no effect when used in a `-compile(..)` attribute. Instead,
  > the `-feature(..)` directive (described next) should be used.
  >
  > [](){: #feature-directive } A feature can also be enabled (disabled) using
  > the `-feature(Feature, enable | disable).` module directive. Note that this
  > directive can only be present in a prefix of the file, before exports and
  > function definitions. This is the preferred method of enabling and disabling
  > features, since it is a local property of a module.

- **`makedep`** - Produces a Makefile rule to track headers dependencies. No
  object file is produced.

  By default, this rule is written to `<File>.Pbeam`. However, if option
  `binary` is set, nothing is written and the rule is returned in `Binary`.

  The output will be encoded in UTF-8.

  For example, if you have the following module:

  ```erlang
  -module(module).

  -include_lib("eunit/include/eunit.hrl").
  -include("header.hrl").
  ```

  The Makefile rule generated by this option looks as follows:

  ```text
  module.beam: module.erl \
    /usr/local/lib/erlang/lib/eunit/include/eunit.hrl \
    header.hrl
  ```

- **`makedep_side_effect`** - The dependencies are created as a side effect to
  the normal compilation process. This means that the object file will also be
  produced. This option override the `makedep` option.

- **`{makedep_output, Output}`** - Writes generated rules to `Output` instead of
  the default `<File>.Pbeam`. `Output` can be a filename or an `io_device()`. To
  write to stdout, use `standard_io`. However, if `binary` is set, nothing is
  written to `Output` and the result is returned to the caller with
  `{ok, ModuleName, Binary}`.

- **`{makedep_target, Target}`** - Changes the name of the rule emitted to
  `Target`.

- **`makedep_quote_target`** - Characters in `Target` special to make(1) are
  quoted.

- **`makedep_add_missing`** - Considers missing headers as generated files and
  adds them to the dependencies.

- **`makedep_phony`** - Adds a phony target for each dependency.

- **`'P'`** - Produces a listing of the parsed code, after preprocessing and
  parse transforms, in the file `<File>.P`. No object file is produced.

- **`'E'`** - Produces a listing of the code, after all source code
  transformations have been performed, in the file `<File>.E`. No object file is
  produced.

- **`'S'`** - Produces a listing of the assembler code in the file `<File>.S`.
  No object file is produced.

- **`recv_opt_info`** - The compiler will emit informational warnings about
  selective `receive` optimizations (both successful and unsuccessful). For more
  information, see the section about
  [selective receive optimization](`e:system:eff_guide_processes.md#receiving-messages`)
  in the Efficiency Guide.

- **`report_errors/report_warnings`** - Causes errors/warnings to be printed as
  they occur.

- **`report`** - A short form for both `report_errors` and `report_warnings`.

- **`return_errors`** - If this flag is set, `{error,ErrorList,WarningList}` is
  returned when there are errors.

- **`return_warnings`** - If this flag is set, an extra field, containing
  `WarningList`, is added to the tuples returned on success.

- **`warnings_as_errors`** - Causes warnings to be treated as errors.

- **`{error_location,line | column}`** - If the value of this flag is `line`,
  the location [`ErrorLocation`](`m:compile#error_information`) of warnings and
  errors is a line number. If the value is `column`, `ErrorLocation` includes
  both a line number and a column number. Default is `column`. This option is
  supported since Erlang/OTP 24.0.

  If the value of this flag is `column`,
  [debug information](`m:compile#debug_info`) includes column information.

- **`return`** - A short form for both `return_errors` and `return_warnings`.

- **`verbose`** - Causes more verbose information from the compiler, describing
  what it is doing.

- **`{source,FileName}`** - Overrides the source file name as presented in
  `module_info(compile)` and stack traces.

- **`absolute_source`** - Turns the source file name (as presented in
  `module_info(compile)` and stack traces) into an absolute path, which helps
  external tools like `perf` and `gdb` find Erlang source code.

- **`{outdir,Dir}`** - Sets a new directory for the object code. The current
  directory is used for output, except when a directory has been specified with
  this option.

- **`export_all`** - Causes all functions in the module to be exported.

- **`{i,Dir}`** - Adds `Dir` to the list of directories to be searched when
  including a file. When encountering an `-include` or `-include_lib` directive,
  the compiler searches for header files in the following directories:

  1. `"."`, the current working directory of the file server
  1. The base name of the compiled file
  1. The directories specified using option `i`; the directory specified last is
     searched first

- **`{d,Macro}`**

- **`{d,Macro,Value}`** - Defines a macro `Macro` to have the value `Value`.
  `Macro` is of type atom, and `Value` can be any term. The default `Value` is
  `true`.

- **`{parse_transform,Module}`** - Causes the parse transformation function
  `Module:parse_transform/2` to be applied to the parsed code before the code is
  checked for errors.

- **`from_abstr`** - The input file is expected to contain Erlang terms
  representing forms in abstract format (default file suffix ".abstr"). Note
  that the format of such terms can change between releases.

  See also the `no_lint` option.

- **`from_asm`** - The input file is expected to be assembler code (default file
  suffix ".S"). Notice that the format of assembler files is not documented, and
  can change between releases.

- **`from_core`** - The input file is expected to be core code (default file
  suffix ".core"). Notice that the format of core files is not documented, and
  can change between releases.

- **`no_spawn_compiler_process`** - By default, all code is compiled in a
  separate process which is terminated at the end of compilation. However, some
  tools, like Dialyzer or compilers for other BEAM languages, may already manage
  their own worker processes and spawning an extra process may slow the
  compilation down. In such scenarios, you can pass this option to stop the
  compiler from spawning an additional process.

- **`no_strict_record_tests`** - This option is not recommended.

  By default, the generated code for operation `Record#record_tag.field`
  verifies that the tuple `Record` has the correct size for the record, and that
  the first element is the tag `record_tag`. Use this option to omit the
  verification code.

- **`no_error_module_mismatch`** - Normally the compiler verifies that
  the module name given in the source code is the same as the base
  name of the output file and refuses to generate an output file if
  there is a mismatch. If there is a good reason for having a module
  name unrelated to the name of the output file, this option disables
  that verification (there will not even be a warning if there is a
  mismatch).

- **`{no_auto_import,[{F,A}, ...]}`** - Makes the function `F/A` no longer being
  auto-imported from the `erlang` module, which resolves BIF name clashes. This
  option must be used to resolve name clashes with auto-imported BIFs that existed
  before Erlang/OTP R14A  when calling a local function with the same name
  as an auto-imported BIF without module prefix.

  If the BIF is to be called, use the `erlang` module prefix
  in the call, not `{no_auto_import,[{F,A}, ...]}`.

  If this option is written in the source code, as a `-compile` directive, the
  syntax `F/A` can be used instead of `{F,A}`. For example:

  ```erlang
  -compile({no_auto_import,[error/1]}).
  ```

- **`no_auto_import`** - Do not auto-import any functions from `erlang` module.

- **`no_line_info`** - Omits line number information to produce a slightly
  smaller output file.

- **`no_lint`** - Skips the pass that checks for errors and warnings. Only
  applicable together with the `from_abstr` option. This is mainly for
  implementations of other languages on top of Erlang, which have already done
  their own checks to guarantee correctness of the code.

  Caveat: When this option is used, there are no guarantees that the code output
  by the compiler is correct and safe to use. The responsibility for correctness
  lies on the code or person generating the abstract format. If the code
  contains errors, the compiler may crash or produce unsafe code.

- **`{extra_chunks, [{binary(), binary()}]}`** - Pass extra chunks to be stored
  in the `.beam` file. The extra chunks must be a list of tuples with a four
  byte binary as chunk name followed by a binary with the chunk contents. See
  `m:beam_lib` for more information.

- **`{check_ssa, Tag :: atom()}`** - Parse and check assertions on the structure
  and content of the BEAM SSA code produced by the compiler. The `Tag` indicates
  the set of assertions to check and after which compiler pass the check is
  performed. This option is internal to the compiler and can be changed or
  removed at any time without prior warning.

- **`line_coverage`** - [](){: #line_coverage } Instrument the compiled code for
  line coverage by inserting an `executable_line` instruction for each
  executable line in the source code. By default, this instruction will be
  ignored when loading the code.

  To activate the `executable_line` instructions, the runtime system must be
  started with the option [\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`) to enable
  a coverage mode. Alternatively, `code:set_coverage_mode/1` can be used to set
  a coverage mode before loading the code.

  The coverage information gathered by the instrumented code can be retrieved by
  calling [code:get_coverage(line, Module)](`code:get_coverage/2`).

- **`force_line_counters`** - [](){: #force_line_counters } When combined with
  option `line_coverage`, this module will be loaded in the `line_counter`
  coverage mode, regardless of the current
  [coverage mode](`code:get_coverage_mode/0`) in the runtime system. This option
  is used by `m:cover` to load cover-compiled code.

If warnings are turned on (option `report_warnings` described earlier), the
following options control what type of warnings that are generated. [](){:
#erl_lint_options } Except from `{warn_format,Verbosity}`, the following options
have two forms:

- A `warn_xxx` form, to turn on the warning.
- A `nowarn_xxx` form, to turn off the warning.

In the descriptions that follow, the form that is used to change the default
value are listed.

- **`{warn_format, Verbosity}`** - Causes warnings to be emitted for malformed
  format strings as arguments to `io:format` and similar functions.

  `Verbosity` selects the number of warnings:

  - `0` = No warnings
  - `1` = Warnings for invalid format strings and incorrect number of arguments
  - `2` = Warnings also when the validity cannot be checked, for example, when
    the format string argument is a variable.

  The default verbosity is `1`. Verbosity `0` can also be selected by option
  `nowarn_format`.

- **`nowarn_bif_clash`** - This option is removed; it generates a fatal error if
  used.

  To resolve BIF clashes, use explicit module names or the
  `{no_auto_import,[F/A]}` compiler directive.

- **`{nowarn_bif_clash, FAs}`** - This option is removed; it generates a fatal
  error if used.

  To resolve BIF clashes, use explicit module names or the
  `{no_auto_import,[F/A]}` compiler directive.

- **`nowarn_export_all`** - Turns off warnings for uses of the `export_all`
  option. Default is to emit a warning if option `export_all` is also given.

- **`warn_export_vars`** - Emits warnings for all implicitly exported variables
  referred to after the primitives where they were first defined. By default,
  the compiler only emits warnings for exported variables referred to in a
  pattern.

- **`nowarn_shadow_vars`** - Turns off warnings for "fresh" variables in
  functional objects or list comprehensions with the same name as some already
  defined variable. Default is to emit warnings for such variables.

- **`warn_keywords`** - [](){: #warn-keywords } Emits warnings when the code
  contains atoms that are used as keywords in some
  [feature](`e:system:features.md#features`). When the feature is enabled, any
  occurrences will lead to a syntax error. To prevent this, the atom has to be
  renamed or quoted.

- **`nowarn_unused_function`** - Turns off warnings for unused local functions.
  Default is to emit warnings for all local functions that are not called
  directly or indirectly by an exported function. The compiler does not include
  unused local functions in the generated BEAM file, but the warning is still
  useful to keep the source code cleaner.

- **`{nowarn_unused_function, FAs}`** - Turns off warnings for unused local
  functions like `nowarn_unused_function` does, but only for the mentioned local
  functions. `FAs` is a tuple `{Name,Arity}` or a list of such tuples.

- **`nowarn_deprecated_function`** - Turns off warnings for calls to deprecated
  functions. Default is to emit warnings for every call to a function known by
  the compiler to be deprecated. Notice that the compiler does not know about
  attribute `-deprecated()`, but uses an assembled list of deprecated functions
  in Erlang/OTP. To do a more general check, the Xref tool can be used. See also
  [xref(3)](`m:xref#deprecated_function`) and the function `xref:m/1`, also
  accessible through the function `\c:xm/1`.

- **`{nowarn_deprecated_function, MFAs}`** - Turns off warnings for calls to
  deprecated functions like `nowarn_deprecated_function` does, but only for the
  mentioned functions. `MFAs` is a tuple `{Module,Name,Arity}` or a list of such
  tuples.

- **`nowarn_deprecated_type`** - Turns off warnings for use of deprecated types.
  Default is to emit warnings for every use of a type known by the compiler to
  be deprecated.

- **`nowarn_deprecated_callback`** - Turns off warnings for use of deprecated callbacks.
  Default is to emit warnings for every use of a callback known by the compiler to
  be deprecated.

- **`warn_deprecated_catch`** - Enables warnings for use of old style catch
  expressions of the form `catch Expr` instead of the modern `try ... catch
  ... end`. You may enable this compiler option on the project level and
  add `-compile(nowarn_deprecated_catch).` to individual files which still
  contain old catches in order to prevent new uses from getting added.

- **`nowarn_removed`** - Turns off warnings for calls to functions that have
  been removed. Default is to emit warnings for every call to a function known
  by the compiler to have been recently removed from Erlang/OTP.

- **`{nowarn_removed, ModulesOrMFAs}`** - Turns off warnings for calls to
  modules or functions that have been removed. Default is to emit warnings for
  every call to a function known by the compiler to have been recently removed
  from Erlang/OTP.

- **`nowarn_obsolete_guard`** - Turns off warnings for calls to old type testing
  BIFs, such as `pid/1` and [`list/1`](`t:list/1`). See the
  [Erlang Reference Manual](`e:system:expressions.md#guards`) for a complete
  list of type testing BIFs and their old equivalents. Default is to emit
  warnings for calls to old type testing BIFs.

- **`warn_unused_import`** - Emits warnings for unused imported functions.
  Default is to emit no warnings for unused imported functions.

- **`nowarn_underscore_match`** - By default, warnings are emitted when a
  variable that begins with an underscore is matched after being bound. Use this
  option to turn off this kind of warning.

- **`nowarn_unused_vars`** - By default, warnings are emitted for unused
  variables, except for variables beginning with an underscore ("Prolog style
  warnings"). Use this option to turn off this kind of warning.

- **`nowarn_unused_record`** - Turns off warnings for unused record definitions.
  Default is to emit warnings for unused locally defined records.

- **`{nowarn_unused_record, RecordNames}`** - Turns off warnings for unused
  record definitions. Default is to emit warnings for unused locally defined
  records.

- **`nowarn_unused_type`** - Turns off warnings for unused type declarations.
  Default is to emit warnings for unused local type declarations.

- **`nowarn_nif_inline`** - By default, warnings are emitted when inlining is
  enabled in a module that may load NIFs, as the compiler may inline NIF
  fallbacks by accident. Use this option to turn off this kind of warnings.

- **`warn_missing_doc` | `warn_missing_doc_functions` | `warn_missing_doc_types` | `warn_missing_doc_callbacks` **{: #warn_missing_doc }  
  By default, warnings are not emitted when `-doc` attribute for an exported function,
  callback or type is not given. Use these option to turn on this kind of warning.
  `warn_missing_doc` is equivalent to setting all of `warn_missing_doc_functions`,
  `warn_missing_doc_types` and `warn_missing_doc_callbacks`.

- **`nowarn_missing_doc` | `nowarn_missing_doc_functions` | `nowarn_missing_doc_types` | `nowarn_missing_doc_callbacks` **  
  If warnings are enabled by [`warn_missing_doc`](#warn_missing_doc), then you can use
  these options turn those warnings off again.
  `nowarn_missing_doc` is equivalent to setting all of `nowarn_missing_doc_functions`,
  `nowarn_missing_doc_types` and `nowarn_missing_doc_callbacks`.

- **`nowarn_hidden_doc` | `{nowarn_hidden_doc,NAs}`**{: #nowarn_hidden_doc }  
  By default, warnings are emitted when `-doc false` attribute is set on a
  [callback or referenced type](`e:system:documentation.md#what-is-visible-versus-hidden`).
  You can set `nowarn_hidden_doc` to suppress all those warnings, or `{nowarn_hidden_doc, NAs}`
  to suppress specific callbacks or types. `NAs` is a tuple `{Name, Arity}` or a
  list of such tuples.

- **`warn_missing_spec`** - By default, warnings are not emitted when a
  specification (or contract) for an exported function is not given. Use this
  option to turn on this kind of warning.

- **`warn_missing_spec_documented`** - By default, warnings are not emitted when a
  specification (or contract) for a documented function is not given. Use this
  option to turn on this kind of warning.

- **`warn_missing_spec_all`** - By default, warnings are not emitted when a
  specification (or contract) for an exported or unexported function is not
  given. Use this option to turn on this kind of warning.

- **`nowarn_redefined_builtin_type`** - By default, a warning is emitted when a
  built-in type is locally redefined. Use this option to turn off this kind of
  warning.

- **`{nowarn_redefined_builtin_type, Types}`** - By default, a warning is
  emitted when a built-in type is locally redefined. Use this option to turn off
  this kind of warning for the types in `Types`, where `Types` is a tuple
  `{TypeName,Arity}` or a list of such tuples.

- **`nowarn_behaviours`** - By default, warnings are emitted for issues
  with behaviours. Use this option to turn off all warnings of this kind.

- **`nowarn_conflicting_behaviours`** - By default, warnings are emitted when
  a module opts in to multiple behaviours that share the names of one or more
  callback functions. Use this option to turn off this kind of warning.

- **`nowarn_undefined_behaviour_func`** - By default, a warning is
  emitted when a module that uses a behaviour does not export a
  mandatory callback function required by that behaviour. Use this
  option to turn off this kind of warning.

- **`nowarn_undefined_behaviour`** - By default, a warning is emitted
  when a module attempts to us an unknown behaviour. Use this option
  to turn off this kind of warning.

- **`nowarn_undefined_behaviour_callbacks`** - By default, a warning
  is emitted when `behaviour_info(callbacks)` in the behaviour module
  returns `undefined` instead of a list of callback functions. Use this
  option to turn off this kind of warning.

- **`nowarn_ill_defined_behaviour_callbacks`** - By default, a warning
  is emitted when `behaviour_info(callbacks)` in the behaviour module
  returns a badly formed list of functions. Use this option to turn
  off this kind of warning.

- **`nowarn_ill_defined_optional_callbacks`** - By default, a warning
  is emitted when `behaviour_info(optional_callbacks)` in the
  behaviour module returns a badly formed list of functions. Use this
  option to turn off this kind of warning.

Other kinds of warnings are _opportunistic warnings_. They are generated when
the compiler happens to notice potential issues during optimization and code
generation.

> #### Note {: .info }
>
> The compiler does not warn for expressions that it does not attempt to
> optimize. For example, the compiler will emit a warning for `1/0` but not for
> `X/0`, because `1/0` is a constant expression that the compiler will attempt
> to evaluate.
>
> The absence of warnings does not mean that there are no remaining errors in
> the code.

Opportunistic warnings can be disabled using the following options:

- **`nowarn_opportunistic`** - Disable all opportunistic warnings.

- **`nowarn_failed`** - Disable warnings for expressions that will always fail
  (such as `atom+42`).

- **`nowarn_ignored`** - Disable warnings for expressions whose values are
  ignored.

- **`nowarn_nomatch`** - Disable warnings for patterns that will never match
  (such as `a=b`) and for guards that always evaluate to `false`.

> #### Note {: .info }
>
> All options, except the include path (`{i,Dir}`), can also be given in the
> file with attribute `-compile([Option,...])`. Attribute `-compile()` is
> allowed after the function definitions.

> #### Note {: .info }
>
> Before Erlang/OTP 22, the option `{nowarn_deprecated_function, MFAs}` was only
> recognized when given in the file with attribute `-compile()`. (The option
> `{nowarn_unused_function,FAs}` was incorrectly documented to only work in a
> file, but it also worked when given in the option list.) Starting from
> Erlang/OTP 22, all options that can be given in the file can also be given
> in the option list.

For debugging of the compiler, or for pure curiosity, the intermediate code
generated by each compiler pass can be inspected. To print a complete list of
the options to produce list files, type `compile:options()` at the Erlang shell
prompt. The options are printed in the order that the passes are executed. If
more than one listing option is used, the one representing the earliest pass
takes effect.

Unrecognized options are ignored.

Both `WarningList` and `ErrorList` have the following format:

```text
[{FileName,[ErrorInfo]}].
```

The filename is included here, as the compiler uses the Erlang
pre-processor `epp`, which allows the code to be included in other
files. It is therefore important to know to _which_ file the location
of an error or a warning refers.

[](){: #error_information }

The `ErrorInfo` structure has the following format:

```text
{ErrorLocation, Module, ErrorDescriptor}
```

`ErrorLocation` is usually the tuple `{Line, Column}`. If option
`{error_location,line}` has been given, `ErrorLocation` is only the
line number.  If the error does not correspond to a specific location
(for example, if the source file does not exist), `ErrorLocation` is
the atom `none`.

A string describing the error is obtained with the following call:

```text
Module:format_error(ErrorDescriptor)
```

# `format_error`

```elixir
-spec format_error(ErrorDescription :: error_description()) -> string().
```

Uses an `ErrorDescriptor` and returns a deep list of characters that describes
the error.

This function is usually called implicitly when an `ErrorInfo`
structure is processed.

# `forms`

```elixir
-spec forms(forms()) -> CompRet :: comp_ret().
```

Is the same as
[`forms(Forms, [verbose,report_errors,report_warnings])`](`forms/2`).

# `forms`

```elixir
-spec forms(Forms :: forms(), Options :: [option()] | option()) -> CompRet :: comp_ret().
```

Analogous to [`file/1`](`file/1`), but takes a list of forms (in either Erlang
abstract or Core Erlang format representation) as first argument.

Option `binary` is implicit, that is, no object code file is
produced. For options that normally produce a listing file, such as
'E', the internal format for that compiler pass (an Erlang term,
usually not a binary) is returned instead of a binary.

# `noenv_file`

```elixir
-spec noenv_file(File :: module() | file:filename(), Options :: [option()] | option()) -> comp_ret().
```

Works like `file/2`, except that the environment variable `ERL_COMPILER_OPTIONS`
is not consulted.

# `noenv_forms`

```elixir
-spec noenv_forms(Forms :: forms(), Options :: [option()] | option()) -> comp_ret().
```

Works like `forms/2`, except that the environment variable
`ERL_COMPILER_OPTIONS` is not consulted.

# `noenv_output_generated`

```elixir
-spec noenv_output_generated(Options :: [option()]) -> boolean().
```

Works like `output_generated/1`, except that the environment variable
`ERL_COMPILER_OPTIONS` is not consulted.

# `output_generated`

```elixir
-spec output_generated(Options :: [option()]) -> boolean().
```

Determines whether the compiler generates a BEAM file with the given options.

`true` means that a BEAM file is generated. `false` means that the compiler
generates some listing file, returns a binary, or merely checks the syntax of
the source code.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
