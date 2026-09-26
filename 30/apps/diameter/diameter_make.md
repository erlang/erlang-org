# `diameter_make`
[🔗](https://github.com/erlang/otp/blob/master/lib/diameter/src/compiler/diameter_make.erl#L34)

Diameter dictionary compilation.

The function `codec/2` is used to compile a diameter
[dictionary file](diameter_dict.md) into Erlang source. The resulting source
implements the interface diameter requires to encode and decode the dictionary's
messages and AVPs.

The utility [diameterc(1)](diameterc_cmd.md) provides an alternate compilation
interface.

## BUGS

Unrecognized options are silently ignored.

## SEE ALSO

[diameterc(1)](diameterc_cmd.md), [diameter_dict(4)](diameter_dict.md)

# `dict`
*not exported* *since OTP R14B03* 

```erlang
-type dict() :: iolist() | binary() | parsed().
```

# `opt`
*since OTP R14B03* 

```erlang
-type opt() ::
          {include | outdir | name | prefix | inherits, string()} |
          return | verbose | parse | forms | erl | hrl.
```

# `parsed`
*not exported* *since OTP R14B03* 

```erlang
-type parsed() :: list().
```

# `codec`
*since OTP R15B* 

```erlang
-spec codec(File, [opt()]) -> ok | {ok, list()} | {error, Reason}
               when File :: dict() | {path, file:name_all()}, Reason :: string().
```

Compile a single dictionary file.

The input `File` can be either a path or a literal dictionary, the occurrence
of newline (ascii NL) or carriage return (ascii CR) identifying the latter.
`Opt` determines the format of the results and whether they are written to
file or returned, and can have the following types.

- **`parse | forms | erl | hrl`** - Specifies an output format. Whether the
  output is returned or written to file depends on whether or not option
  `return` is specified. When written to file, the resulting file(s) will have
  extensions `.D`, `.F`, `.erl`, and `.hrl` respectively, basenames defaulting
  to `dictionary` if the input dictionary is literal and does not specify
  [`@name`](diameter_dict.md#name). When returned, results are in the order of
  the corresponding format options. Format options default to `erl` and `hrl`
  (in this order) if unspecified.

  The `parse` format is an internal representation that can be passed to
  `flatten/1` and `format/1`, while the `forms` format can be passed to
  `compile:forms/2`. The `erl` and `hrl` formats are returned as iolists.

- **`{include, string()}`** - Prepend the specified directory to the code path.
  Use to point at beam files compiled from inherited dictionaries,
  [`@inherits`](diameter_dict.md#inherits) in a dictionary file creating a beam
  dependency, not an erl/hrl dependency.

  Multiple `include` options can be specified.

- **`{outdir, string()}`** - Write generated source to the specified directory.
  Defaults to the current working directory. Has no effect if option `return` is
  specified.

- **`return`** - Return results in a `{ok, [Out]}` tuple instead of writing to
  file and returning `ok`.

- **`{name|prefix, string()}`** - Transform the input dictionary before
  compilation, setting [`@name`](diameter_dict.md#name) or
  [`@prefix`](diameter_dict.md#prefix) to the specified string.

- **`{inherits, string()}`** - Transform the input dictionary before
  compilation, appending [`@inherits`](diameter_dict.md#inherits) of the
  specified string.

  Two forms have special meaning:

  ```text
  {inherits, "-"}
  {inherits, "Prev/Mod"}
  ```

  The first has the effect of clearing any previous inherits, the second of
  replacing a previous inherits of `Prev` to one of `Mod`. This allows the
  semantics of the input dictionary to be changed without modifying the file
  itself.

  Multiple `inherits` options can be specified.

- **`indirect_inherits`** - This option makes compiler support automatic
  recursive inheritance. When a dictionary file inherits another `.dia`, all
  ancestors of that `.dia` will also be considered in code generation. This
  enhancement removes the requirement to explicitly list all parent dictionaries
  via `@inherits`, preventing missing AVP/message encodings and runtime errors.

  **Example** `C.dia` inherits `B.dia` and `B.dia` inherits `A.dia`.

  **Before (without indirect_inherits):**

  If `C.dia` references AVPs from `A.dia` without directly inheriting it,
  the generated code will lack necessary definitions, causing encoding failures.

  **After (with indirect_inherits):**

  `C.dia` can reference AVPs from `A.dia` without directly inheriting it,
  compiler will resolve the entire inheritance chain automatically, ensuring
  all relevant descendant definitions from `A.dia` are available without
  additional user declarations.  

Note that a dictionary's [`@name`](diameter_dict.md#name), together with the
`outdir` option, determine the output paths when the `return` option is not
specified. The [`@name`](diameter_dict.md#name) of a literal input dictionary
defaults to `dictionary`.

A returned error reason can be converted into a readable string using
`format_error/1`.

# `flatten`
*since OTP R16B03* 

```erlang
-spec flatten(parsed()) -> parsed().
```

Reconstitute a parsed dictionary, as returned by `codec/2`, without using
[`@inherits`](diameter_dict.md#inherits). That is, construct an equivalent
dictionary in which all AVP's are definined in the dictionary itself. The return
value is also a parsed dictionary.

# `format`
*since OTP R16B03* 

```erlang
-spec format(parsed()) -> iolist().
```

Turns a parsed dictionary, as returned by `codec/2`, back into the dictionary
format.

# `format_error`
*since OTP 17.0* 

```erlang
-spec format_error(Reason) -> FormattedReason when Reason :: term(), FormattedReason :: string().
```

Turn an error reason returned by `codec/2` into a readable string.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
