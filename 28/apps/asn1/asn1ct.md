# `asn1ct`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/asn1/src/asn1ct.erl#L24)

ASN.1 compiler and compile-time support functions

The ASN.1 compiler takes an ASN.1 module as input and generates a corresponding
Erlang module, which can encode and decode the specified data types.
Alternatively, the compiler takes a specification module specifying all input
modules, and generates a module with encode/decode functions. In addition, some
generic functions can be used during development of applications that handles
ASN.1 data (encoded as `BER` or `PER`).

> #### Note {: .info }
>
> By default in Erlang/OTP 17, the representation of the `BIT STRING` and
> `OCTET STRING` types as Erlang terms were changed. `BIT STRING` values are now
> Erlang bit strings and `OCTET STRING` values are binaries. Also, an undecoded
> open type is now wrapped in an `asn1_OPENTYPE` tuple. For details, see
> [BIT STRING](asn1_getting_started.md#bit-string),
> [OCTET STRING](asn1_getting_started.md#octet-string), and
> [ASN.1 Information Objects](asn1_getting_started.md#Information-Object) in the
> User's Guide.
>
> To revert to the old representation of the types, use option
> `legacy_erlang_types`.

# `compile`

```elixir
-spec compile(Asn1Module) -> ok | {error, Reason} when Asn1Module :: atom() | string(), Reason :: term().
```

# `compile`

```elixir
-spec compile(Asn1Module, Options) -> ok | {error, Reason}
                 when
                     Asn1Module :: atom() | string(),
                     Options :: [Option | OldOption],
                     Option ::
                         ber | per | uper | jer | der | compact_bit_string | legacy_bit_string |
                         legacy_erlang_types | noobj |
                         {n2n, EnumTypeName :: term()} |
                         {outdir, Dir :: term()} |
                         {i, IncludeDir :: term()} |
                         asn1config | undec_rest | no_ok_wrapper |
                         {macro_name_prefix, Prefix} |
                         {record_name_prefix, Prefix} |
                         verbose | warnings_as_errors | deterministic,
                     OldOption :: ber | per,
                     Reason :: term(),
                     Prefix :: string().
```

Compiles the ASN.1 module `Asn1Module` and generates an Erlang module
`Asn1Module.erl` with encode and decode functions for all types defined in
the ASN.1 module.

For each ASN.1 value defined in the module, an Erlang function that
returns the value in Erlang representation is generated.

If `Asn1Module` is a filename without extension, first `".asn1"` is assumed,
then `".asn"`, and finally `".py"` (to be compatible with the old ASN.1
compiler). `Asn1Module` can be a full pathname (relative or absolute) including
filename with (or without) extension.[](){: #asn1set }

If it is needed to compile a set of `ASN.1` modules into an Erlang
file with encode/decode functions, list all involved files in a
configuration file, one line per file. This configuration file must
have a double extension `".set.asn1"` (`".asn1"` can alternatively be
`".asn"` or `".py"`). If the input files are `File1.asn1`,
`File2.asn1`, and `File3.asn1`, the configuration file should look as
follows:

```text
File1.asn1
File2.asn1
File3.asn1
```

The output files in this case get their names from the configuration file. If
the configuration file is named `SetOfFiles.set.asn1`, the names of the output
files are `SetOfFiles.hrl, SetOfFiles.erl, and SetOfFiles.asn1db`.

Sometimes in a system of `ASN.1` modules, different modules can have
different default tag modes, for example, one uses `AUTOMATIC` and
another `IMPLICIT`.  The multi-file compilation resolves the default
tagging as if the modules were compiled separately.

Name collisions is an unwanted effect that can occur in multi-file
compilation. The compiler solves this problem in one of two ways:

- If the definitions are identical, the output module keeps only one definition
  with the original name.
- If the definitions have the same name and differs in the definition, they are
  renamed. The new names are the definition name and the original module name
  concatenated.

If a name collision occurs, the compiler reports a `"NOTICE: ..."` message that
tells if a definition was renamed, and the new name that must be used to
encode/decode data.

`Options` is a list with options specific for the ASN.1 compiler and options
that are applied to the Erlang compiler. The ASN.1 compiler passes on any
unrecognized options to the Erlang compiler. The available options are as follows:

- **`ber | per | uper | jer`** - The encoding rule to be used. The
  supported encoding rules are Basic Encoding Rules (`ber`), Packed
  Encoding Rules (`per`) aligned, PER unaligned (`uper`), and JSON
  Encoding Rules (`jer`). The `jer` option can be used by itself to
  generate a module that only supports encoding/decoding of JER, or it
  can be used as a supplementary option to `ber`, `per`, and `uper`,
  in which case a module that handles both the main encoding rules and
  JER will be generated. In that case, the exported functions for JER
  will be `jer_encode(Type, Value)` and `jer_decode(Type, Bytes)`.

  JER (ITU-T X.697) are experimental in OTP 22. There is support for a
  subset of the X.697 standard, for example there is no support for:

  - JER encoding instructions
  - the REAL type

  > #### Change {: .info }
  >
  > In Erlang/OTP 27 and later, module `m:json` in STDLIB is used for
  > encoding and decoding JSON. Before Erlang/OTP 27, it was necessary
  > to provide an external JSON library.

  If the encoding rule option is omitted, `ber` is the default.

  The generated Erlang module always gets the same name as the ASN.1 module.
  Therefore, only one encoding rule per ASN.1 module can be used at runtime.

- **`der`** - With this option the Distinguished Encoding Rules (`der`) is
  chosen. DER is regarded as a specialized variant of the BER encoding rule.
  Therefore, this option only makes sense together with option `ber`. This
  option sometimes adds sorting and value checks when encoding, which implies
  slower encoding. The decoding routines are the same as for `ber`.

- **`maps`** - This option changes the representation of the types `SEQUENCE`
  and `SET` to use maps (instead of records). This option also suppresses the
  generation of `.hrl` files.

  For details, see section
  [Map representation for SEQUENCE and SET](asn1_getting_started.md#MAP_SEQ_SET)
  in the User's Guide.

- **`compact_bit_string`** - The `BIT STRING` type is decoded to "compact
  notation".

  **This option is not recommended for new code.**

  For details, see section [BIT STRING](asn1_getting_started.md#bit-string) in
  the User's Guide.

  This option implies option `legacy_erlang_types`, and it cannot be combined
  with option `maps`.

- **`legacy_bit_string`** - The `BIT STRING` type is decoded to the legacy
  format, that is, a list of zeroes and ones.

  **This option is not recommended for new code.**

  For details, see section [BIT STRING](asn1_getting_started.md#bit-string) in
  the User's Guide

  This option implies option `legacy_erlang_types`, and it cannot be combined
  with option `maps`.

- **`legacy_erlang_types`** - Use the same Erlang types to represent
  `BIT STRING` and `OCTET STRING` as in Erlang/OTP R16.

  **This option is not recommended for new code.**

  For details, see section [BIT STRING](asn1_getting_started.md#bit-string) and
  section [OCTET STRING](asn1_getting_started.md#octet-string) in the User's
  Guide.

  This option cannot be combined with option `maps`.

- **`{n2n, EnumTypeName}`** - Tells the compiler to generate functions for
  conversion between names (as atoms) and numbers and conversely for the
  specified `EnumTypeName`. There can be multiple occurrences of this option to
  specify several type names. The type names must be declared as `ENUMERATIONS`
  in the ASN.1 specification.

  If `EnumTypeName` does not exist in the ASN.1 specification, the compilation
  stops with an error code.

  The generated conversion functions are named `name2num_EnumTypeName/1` and
  `num2name_EnumTypeName/1`.

- **`noobj`** - Do not compile (that is, do not produce object code) the
  generated `.erl` file. If this option is omitted, the generated Erlang module
  is compiled.

- **`{i, IncludeDir}`** - Adds `IncludeDir` to the search-path for `.asn1db` and
  ASN.1 source files. The compiler tries to open an `.asn1db` file when a
  module imports definitions from another ASN.1 module. If no `.asn1db` file
  is found, the ASN.1 source file is parsed. Several `{i, IncludeDir}` can be
  given.

- **`{outdir, Dir}`** - Specifies directory `Dir` where all generated files are
  to be placed. If this option is omitted, the files are placed in the current
  directory.

- **`asn1config`** - When using one of the specialized decodes, exclusive or
  selective decode, instructions must be given in a configuration file. Option
  `asn1config` enables specialized decodes and takes the configuration file in
  concern. The configuration file has the same name as the ASN.1 specification,
  but with extension `.asn1config`.

  For instructions for exclusive decode, see section
  [Exclusive Decode](asn1_spec.md#Exclusive-Instruction) in the User's Guide.

  For instructions for selective decode, see section
  [Selective Decode](asn1_spec.md#Selective-Instruction) in the User's Guide.

- **`undec_rest`** - By default when decoding, any bytes following the
  end of an ASN.1 data structure are discarded. If an ASN.1 module is
  compiled with option `undec_rest`, the decode function returns a
  tuple `{ok, Value, Rest}`, where `Rest` is the bytes following the ASN.1
  data structure. `Rest` can be a list or a binary.

- **`no_ok_wrapper`** - With this option, the generated `encode/2` and
  `decode/2` functions do not wrap a successful return value in an `{ok,...}`
  tuple. If any error occurs, an exception will be raised.

- **`{macro_name_prefix, Prefix}`** - All macro names generated by the compiler
  are prefixed with `Prefix`. This is useful when multiple protocols that
  contain macros with identical names are included in a single module.

- **`{record_name_prefix, Prefix}`** - All record names generated by the
  compiler are prefixed with `Prefix`. This is useful when multiple protocols
  that contain records with identical names are included in a single module.

- **`verbose`** - Causes more verbose information from the compiler describing
  what it is doing.

- **`warnings_as_errors`** - Causes warnings to be treated as errors.

- **`deterministic`** - Causes all non-deterministic options to be stripped from
  the `-asn1_info()` attribute.

Unrecognized options are passed on to the Erlang compiler when the generated
`.erl` file is compiled.

The compiler generates the following files:

- `Asn1Module.hrl` (if any `SET` or `SEQUENCE` is defined)
- `Asn1Module.erl` \- Erlang module with encode, decode, and value functions
- `Asn1Module.asn1db` \- Intermediate format used by the compiler when modules
  `IMPORT` definitions from each other.

# `test`

```elixir
-spec test(Module) -> ok | {error, Reason} when Module :: module(), Reason :: term().
```

Tests encoding and decoding of all types in `Module`.

For more details, see `test/3`.

# `test`

```elixir
-spec test(Module, Type | Options) -> ok | {error, Reason}
              when
                  Module :: module(),
                  Type :: atom(),
                  Options :: [{i, IncludeDir :: term()}],
                  Reason :: term().
```

Tests encoding and decoding of `Module`.

If the second argument is given as atom `Type`, that type is tested.

If the second argument is given as list `Options`, that are the options
that are used for testing all types in the module.

For more details, see `test/3`.

# `test`

```elixir
-spec test(Module, Type, Value | Options) -> ok | {error, Reason}
              when
                  Module :: module(),
                  Type :: atom(),
                  Value :: term(),
                  Options :: [{i, IncludeDir :: term()}],
                  Reason :: term().
```

Performs a test of encode and decode of types in `Module`.

The generated functions are called by this function. This function is
useful for testing to ensure that the generated encode and decode
functions as well as the general runtime support work as expected.

> #### Note {: .info }
>
> Currently, the `test` functions have many limitations. Essentially, they will
> mostly work for old specifications based on the 1997 standard for ASN.1, but
> not for most modern-style applications. Another limitation is that the `test`
> functions may not work if options that change code generations strategies such
> as the options `macro_name_prefix` and `record_name_prefix` have been used.

- [`test/1`](`test/1`) iterates over all types in `Module`.
- [`test/2`](`test/2`) tests type `Type` with a random value.
- [`test/3`](`test/3`) tests type `Type` with `Value`.

Schematically, the following occurs for each type in the module:

```erlang
{ok, Value} = asn1ct:value(Module, Type),
{ok, Bytes} = Module:encode(Type, Value),
{ok, Value} = Module:decode(Type, Bytes).
```

The `test` functions use the `*.asn1db` files for all included modules. If they
are located in a different directory than the current working directory, use the
`include` option to add paths. This is only needed when automatically generating
values. For static values using `Value` no options are needed.

# `value`

```elixir
-spec value(Module, Type) -> {ok, Value} | {error, Reason}
               when Module :: module(), Type :: atom(), Value :: term(), Reason :: term().
```

Returns an Erlang term that is an example of a valid Erlang representation of a
value of the ASN.1 type `Type`.

The value is a random value and subsequent calls to this function will
for most types return different values.

> #### Note {: .info }
>
> Currently, the `value` function has many limitations. Essentially, it will
> mostly work for old specifications based on the 1997 standard for ASN.1, but
> not for most modern-style applications. Another limitation is that the `value`
> function may not work if options that change code generations strategies such
> as the options `macro_name_prefix` and `record_name_prefix` have been used.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
