# `snmpc`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/snmp/src/compile/snmpc.erl#L22)

Interface Functions to the SNMP toolkit MIB compiler

The module `snmpc` contains interface functions to the SNMP toolkit MIB
compiler.

## See Also

erlc(1)

snmpc(command)

# `compile`

```elixir
-spec compile(AtomFileNames | FileName) -> {ok, BinFileName} | {error, Reason}
                 when
                     AtomFileNames :: [atom()],
                     FileName :: string(),
                     BinFileName :: string(),
                     Reason :: term().
```

# `compile`

```elixir
-spec compile(FileName, Options) -> {ok, BinFileName} | {error, Reason}
                 when
                     FileName :: string(),
                     Options :: [Option],
                     Option ::
                         agent_capabilities |
                         {db, volatile | persistent | mnesia} |
                         {deprecated, boolean()} |
                         description |
                         {group_check, boolean()} |
                         {i, [snmp:dir()]} |
                         {il, [snmp:dir()]} |
                         imports |
                         {module, module()} |
                         module_identity | module_compliance | no_defs |
                         {outdir, snmp:dir()} |
                         reference | relaxed_row_name_assign_check |
                         {verbosity, snmp:verbosity()} |
                         {warnings, boolean()} |
                         {warnings_as_errors, boolean()},
                     BinFileName :: string(),
                     Reason :: term().
```

Compiles the specified MIB file `<FileName>.mib`. The compiled file `BinFileName`
is called `<FileName>.bin`.

- The option `agent_capabilities`, if present, specifies that the
  AGENT-CAPABILITIES statement of the MIB shall be included (with a mib-entry
  record) in the compiled mib. The mib-entry record of the agent-capabilitie
  will contain `reference` and `modules` part(s) this info in the `assocList`
  field).
- The option `db` specifies which database should be used for the default
  instrumentation.

  Default is `volatile`.

- The option `deprecated` specifies if a deprecated definition should be kept or
  not. If the option is false the MIB compiler will ignore all deprecated
  definitions.

  Default is `true`.

- The option `description` specifies if the text of the DESCRIPTION field will
  be included or not.

  By default it is not included, but if this option is present it will be.

- The option `group_check` specifies whether the mib compiler should check the
  OBJECT-GROUP macro and the NOTIFICATION-GROUP macro for correctness or not.

  Default is `true`.

- The option `i` specifies the path to search for imported (compiled) MIB files.
  The directories should be strings with a trailing directory delimiter.

  Default is `["./"]`.

- The option `il` (include_lib) also specifies a list of directories to search
  for imported MIBs. It assumes that the first element in the directory name
  corresponds to an OTP application. The compiler will find the current
  installed version. For example, the value \["snmp/mibs/"] will be replaced by
  \["snmp-3.1.1/mibs/"] (or what the current version may be in the system). The
  current directory and the `<snmp-home>/priv/mibs/` are always listed last in
  the include path.
- The option `imports`, if present, specifies that the IMPORT statement of the
  MIB shall be included in the compiled mib.
- The option `module`, if present, specifies the name of a module which
  implements all instrumentation functions for the MIB.

  The name of all instrumentation functions must be the same as the
  corresponding managed object it implements.

- The option `module_identity`, if present, specifies that the info part of the
  MODULE-IDENTITY statement of the MIB shall be included in the compiled mib.
- The option `module_compliance`, if present, specifies that the
  MODULE-COMPLIANCE statement of the MIB shall be included (with a mib-entry
  record) in the compiled mib. The mib-entry record of the module-compliance
  will contain `reference` and `module` part(s) this info in the `assocList`
  field).
- The option `no_defs`, if present, specifies that if a managed object does not
  have an instrumentation function, the default instrumentation function should
  NOT be used, instead this is reported as an error, and the compilation aborts.
- The option `reference` specifies if the text of the REFERENCE field, when
  found in a table definition, will be included or not.

  By default it is not included, but if this option is present it will be. The
  reference text will be placed in the allocList field of the mib-entry record
  (#me\{\}) for the table.

- The option `relaxed_row_name_assign_check`, if present, specifies that the row
  name assign check shall not be done strictly according to the SMI (which
  allows only the value 1). With this option, all values greater than zero is
  allowed (>= 1). This means that the error will be converted to a warning.

  By default it is not included, but if this option is present it will be.

- The option `verbosity` specifies the verbosity of the SNMP mib compiler. I.e.
  if warning, info, log, debug and trace messages shall be shown.

  Default is `silence`.

  Note that if the option `warnings` is `true` and the option `verbosity` is
  `silence`, warning messages will still be shown.

- The option `warnings` specifies whether warning messages should be shown.

  Default is `true`.

- The option `warnings_as_errors`, if present, specifies whether warnings should
  be treated as errors.

The MIB compiler understands both SMIv1 and SMIv2 MIBs. It uses the
`MODULE-IDENTITY` statement to determine if the MIB is version 1 or 2.

The MIB compiler can also be invoked from the OS command line by these two
commands; `erlc` and `snmpc`.

- **`erlc`** - `erlc` recognizes the extension `.mib`, and invokes the SNMP MIB
  compiler for files with that extension. The options `db`, `group_check`,
  `deprecated`, `description`, `verbosity`, `imports` and `module_identity` have
  to be specified to `erlc` using the syntax `+term`.

  See [`erlc(1)`](`e:erts:erlc_cmd.md`) for details.

- **`snmpc`** - `snmpc` is an escript that provides a more traditional interface
  to the MIB compiler.

  See [snmpc(command)](snmpc_cmd.md) for details.

# `is_consistent`

```elixir
-spec is_consistent(FileNames) -> ok | {error, Reason}
                       when FileNames :: [MibName], MibName :: string(), Reason :: term().
```

Checks for multiple usage of object identifiers and traps between MIBs.

# `mib_to_hrl`

```elixir
-spec mib_to_hrl(MibName) -> Result
                    when MibName :: string(), Result :: ok | {error, Reason}, Reason :: term().
```

Generates a `.hrl` file with definitions of Erlang constants for the objects in
the MIB. The `.hrl` file is called `<MibName>.hrl`. The MIB must be compiled,
and present in the current directory.

The `mib_to_hrl` generator can be invoked from the OS command line by using the
command `erlc`. `erlc` recognizes the extension `.bin`, and invokes this
function for files with that extension.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
