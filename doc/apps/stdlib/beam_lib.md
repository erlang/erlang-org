# `beam_lib`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/beam_lib.erl#L22)

This module provides an interface to files created by the BEAM Compiler ("BEAM
files").

The format used, a variant of "EA IFF 1985" Standard for Interchange Format Files,
divides data into chunks.

Chunk data can be returned as binaries or as compound terms. Compound terms are
returned when chunks are referenced by names (atoms) rather than identifiers
(strings). The recognized names and the corresponding identifiers are as
follows:

- `atoms ("Atom")`
- `attributes ("Attr")`
- `compile_info ("CInf")`
- `debug_info ("Dbgi")`
- `exports ("ExpT")`
- `imports ("ImpT")`
- `indexed_imports ("ImpT")`
- `labeled_exports ("ExpT")`
- `labeled_locals ("LocT")`
- `literals ("LitT")`
- `locals ("LocT")`
- `documentation ("Docs")`

[](){: #debug_info }

## Debug Information/Abstract Code

Option `debug_info` can be specified to the Compiler (see
[`compile`](`m:compile#debug_info`)) to have debug information, such as
[Erlang Abstract Format](`e:erts:absform.md`), stored in the `debug_info` chunk.
Tools such as Debugger and Xref require the debug information to be included.

> #### Warning {: .warning }
>
> Source code can be reconstructed from the debug information. To prevent this,
> use encrypted debug information (see below).

The debug information can also be removed from BEAM files using `strip/1`,
`strip_files/1`, and/or `strip_release/1`.

## Reconstruct Source Code

The following example shows how to reconstruct Erlang source code from the debug
information in a BEAM file `Beam`:

```erlang
{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Beam,[abstract_code]).
io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).
```

## Encrypted Debug Information

The debug information can be encrypted to keep the source code secret, but still
be able to use tools such as Debugger or Xref.

To use encrypted debug information, a key must be provided to the compiler and
`beam_lib`. The key is specified as a string. It is recommended that the string
contains at least 32 characters and that both upper and lower case letters as
well as digits and special characters are used.

The default type (and currently the only type) of crypto algorithm is
`des3_cbc`, three rounds of DES. The key string is scrambled using
`erlang:md5/1` to generate the keys used for `des3_cbc`.

> #### Note {: .info }
>
> As far as we know by the time of writing, it is infeasible to break `des3_cbc`
> encryption without any knowledge of the key. Therefore, as long as the key is
> kept safe and is unguessable, the encrypted debug information _should_ be safe
> from intruders.

The key can be provided in the following two ways:

1. Use Compiler option `{debug_info_key,Key}`, see
   [`compile`](`m:compile#debug_info_key`) and function `crypto_key_fun/1` to
   register a fun that returns the key whenever `beam_lib` must decrypt the
   debug information.
   If no such fun is registered, `beam_lib` instead searches for an `.erlang.crypt`
   file, see the next section.
1. Store the key in a text file named `.erlang.crypt`.
   In this case, Compiler option `encrypt_debug_info` can be used, see
   [`compile`](`m:compile#encrypt_debug_info`).

## .erlang.crypt

`beam_lib` searches for `.erlang.crypt` in the current directory, then the
[user's home directory](`m:init#home`) and then
[`filename:basedir(user_config, "erlang")`](`m:filename#user_config`). If the
file is found and contains a key, `beam_lib` implicitly creates a crypto key fun
and registers it.

File `.erlang.crypt` is to contain a single list of tuples:

```erlang
{debug_info, Mode, Module, Key}
```

`Mode` is the type of crypto algorithm; currently, the only allowed value is
`des3_cbc`. `Module` is either an atom, in which case `Key` is only used for the
module `Module`, or `[]`, in which case `Key` is used for all modules. `Key` is
the non-empty key string.

`Key` in the first tuple where both `Mode` and `Module` match is used.

The following is an example of an `.erlang.crypt` file that returns the same key
for all modules:

```erlang
[{debug_info, des3_cbc, [], "%>7}|pc/DM6Cga*68$Mw]L#&_Gejr]G^"}].
```

The following is a slightly more complicated example of an `.erlang.crypt`
providing one key for module `t` and another key for all other modules:

```erlang
[{debug_info, des3_cbc, t, "My KEY"},
 {debug_info, des3_cbc, [], "%>7}|pc/DM6Cga*68$Mw]L#&_Gejr]G^"}].
```

> #### Note {: .info }
>
> Do not use any of the keys in these examples. Use your own keys.

# `abst_code`
*not exported* 

```erlang
-type abst_code() :: {AbstVersion :: atom(), forms()} | no_abstract_code.
```

It is not checked that the forms conform to the abstract format indicated by
`AbstVersion`. `no_abstract_code` means that chunk `"Abst"` is present, but
empty.

For modules compiled with OTP 20 onwards, the `abst_code` chunk is automatically
computed from the `debug_info` chunk.

# `attrib_entry`

```erlang
-type attrib_entry() :: {Attribute :: atom(), [AttributeValue :: term()]}.
```

# `beam`

```erlang
-type beam() :: file:filename() | binary().
```

Each of the functions described below accept either the filename (as a string)
or a binary containing the BEAM module.

# `chnk_rsn`

```erlang
-type chnk_rsn() ::
          {unknown_chunk, file:filename(), atom()} |
          {key_missing_or_invalid, file:filename(), abstract_code | debug_info} |
          {missing_backend, file:filename(), module()} |
          info_rsn().
```

# `chunkdata`
*not exported* 

```erlang
-type chunkdata() ::
          {chunkid(), dataB()} |
          {abstract_code, abst_code()} |
          {debug_info, debug_info()} |
          {attributes, [attrib_entry()]} |
          {compile_info, [compinfo_entry()]} |
          {exports, [{atom(), arity()}]} |
          {labeled_exports, [labeled_entry()]} |
          {imports, [mfa()]} |
          {indexed_imports, [{index(), module(), Function :: atom(), arity()}]} |
          {locals, [{atom(), arity()}]} |
          {labeled_locals, [labeled_entry()]} |
          {atoms, [{integer(), atom()}]} |
          {documentation, docs()} |
          {literals, literals()}.
```

The list of attributes is sorted on `Attribute` (in `t:attrib_entry/0`) and each
attribute name occurs once in the list. The attribute values occur in the same
order as in the file. The lists of functions are also sorted.

# `chunkid`

```erlang
-type chunkid() :: nonempty_string().
```

`"Attr" | "CInf" | "Dbgi" | "ExpT" | "ImpT" | "LocT" | "AtU8" | "Docs"`

# `chunkname`
*not exported* 

```erlang
-type chunkname() ::
          abstract_code | debug_info | attributes | compile_info | exports | labeled_exports | imports |
          indexed_imports | locals | labeled_locals | atoms | documentation | literals.
```

# `chunkref`
*not exported* 

```erlang
-type chunkref() :: chunkname() | chunkid().
```

# `cmp_rsn`
*not exported* 

```erlang
-type cmp_rsn() ::
          {modules_different, module(), module()} |
          {chunks_different, chunkid()} |
          different_chunks |
          info_rsn().
```

# `compinfo_entry`

```erlang
-type compinfo_entry() :: {InfoKey :: atom(), term()}.
```

# `crypto_fun`
*not exported* 

```erlang
-type crypto_fun() :: fun((crypto_fun_arg()) -> term()).
```

# `crypto_fun_arg`
*not exported* 

```erlang
-type crypto_fun_arg() :: init | clear | {debug_info, mode(), module(), file:filename()}.
```

# `dataB`
*not exported* 

```erlang
-type dataB() :: binary().
```

# `debug_info`
*not exported* 

```erlang
-type debug_info() :: {DbgiVersion :: atom(), Backend :: module(), Data :: term()} | no_debug_info.
```

The format stored in the `debug_info` chunk.

To retrieve particular code representation from the backend,
`Backend:debug_info(Format, Module, Data, Opts)` must be invoked. `Format` is an
atom, such as `erlang_v1` for the Erlang Abstract Format or `core_v1` for Core
Erlang. `Module` is the module represented by the beam file and `Data` is the
value stored in the debug info chunk. `Opts` is any list of values supported by
the `Backend`. `Backend:debug_info/4` must return `{ok, Code}` or
`{error, Term}`.

Developers must always invoke the `debug_info/4` function and never rely on the
`Data` stored in the `debug_info` chunk, as it is opaque and may change at any
moment. `no_debug_info` means that chunk `"Dbgi"` is present, but empty.

# `docs`
*not exported* 

```erlang
-type docs() ::
          #docs_v1{anno :: term(),
                   beam_language :: term(),
                   format :: term(),
                   module_doc :: term(),
                   metadata :: term(),
                   docs :: term()}.
```

[EEP-48 documentation format](`e:kernel:eep48_chapter.md#the-docs-format`)

# `forms`
*not exported* 

```erlang
-type forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].
```

# `index`
*not exported* 

```erlang
-type index() :: non_neg_integer().
```

# `info_rsn`
*not exported* 

```erlang
-type info_rsn() ::
          {chunk_too_big,
           file:filename(),
           chunkid(),
           ChunkSize :: non_neg_integer(),
           FileSize :: non_neg_integer()} |
          {invalid_beam_file, file:filename(), Position :: non_neg_integer()} |
          {invalid_chunk, file:filename(), chunkid()} |
          {missing_chunk, file:filename(), chunkid()} |
          {not_a_beam_file, file:filename()} |
          {file_error, file:filename(), file:posix()}.
```

# `label`

```erlang
-type label() :: integer().
```

# `labeled_entry`

```erlang
-type labeled_entry() :: {Function :: atom(), arity(), label()}.
```

# `literals`
*not exported* 

```erlang
-type literals() :: {index(), term()}.
```

# `mode`
*not exported* 

```erlang
-type mode() :: des3_cbc.
```

# `all_chunks`
*since OTP 18.2* 

```erlang
-spec all_chunks(beam()) -> {ok, module(), [{chunkid(), dataB()}]} | {error, beam_lib, info_rsn()}.
```

Reads chunk data for all chunks.

# `build_module`
*since OTP 18.2* 

```erlang
-spec build_module(Chunks) -> {ok, Binary} when Chunks :: [{chunkid(), dataB()}], Binary :: binary().
```

Builds a BEAM module (as a binary) from a list of chunks.

# `chunks`

```erlang
-spec chunks(Beam, ChunkRefs) -> {ok, {module(), [chunkdata()]}} | {error, beam_lib, chnk_rsn()}
                when Beam :: beam(), ChunkRefs :: [chunkref()].
```

Reads chunk data for selected chunks references. The order of the returned list
of chunk data is determined by the order of the list of chunks references.

# `chunks`

```erlang
-spec chunks(Beam, ChunkRefs, Options) ->
                {ok, {module(), [ChunkResult]}} | {error, beam_lib, chnk_rsn()}
                when
                    Beam :: beam(),
                    ChunkRefs :: [chunkref()],
                    Options :: [allow_missing_chunks],
                    ChunkResult :: chunkdata() | {ChunkRef :: chunkref(), missing_chunk}.
```

Reads chunk data for selected chunks references. The order of the returned list
of chunk data is determined by the order of the list of chunks references.

By default, if any requested chunk is missing in `Beam`, an `error` tuple is
returned. However, if option `allow_missing_chunks` is specified, a result is
returned even if chunks are missing. In the result list, any missing chunks are
represented as `{ChunkRef,missing_chunk}`. Notice however that if chunk `"Atom"`
is missing, that is considered a fatal error and the return value is an `error`
tuple.

# `clear_crypto_key_fun`

```erlang
-spec clear_crypto_key_fun() -> undefined | {ok, Result} when Result :: undefined | term().
```

Unregisters the crypto key fun and terminates the process holding it, started by
`crypto_key_fun/1`.

Returns either `{ok, undefined}` if no crypto key fun is registered, or
`{ok, Term}`, where `Term` is the return value from `CryptoKeyFun(clear)`, see
[`crypto_key_fun/1`](`crypto_key_fun/1`).

# `cmp`

```erlang
-spec cmp(Beam1, Beam2) -> ok | {error, beam_lib, cmp_rsn()} when Beam1 :: beam(), Beam2 :: beam().
```

Compares the contents of two BEAM files.

If the module names are the same, and all chunks except for chunk `"CInf"`
(the chunk containing the compilation information that is returned by
`Module:module_info(compile)`) have the same contents in both files, `ok` is
returned. Otherwise an error message is returned.

# `cmp_dirs`

```erlang
-spec cmp_dirs(Dir1, Dir2) -> {Only1, Only2, Different} | {error, beam_lib, Reason}
                  when
                      Dir1 :: atom() | file:filename(),
                      Dir2 :: atom() | file:filename(),
                      Only1 :: [file:filename()],
                      Only2 :: [file:filename()],
                      Different :: [{Filename1 :: file:filename(), Filename2 :: file:filename()}],
                      Reason :: {not_a_directory, term()} | info_rsn().
```

Compares the BEAM files in two directories.

Only files with extension `".beam"` are compared. BEAM files that exist only in
directory `Dir1` (`Dir2`) are returned in `Only1` (`Only2`). BEAM files that
exist in both directories but are considered different by [`cmp/2`](`cmp/2`) are
 returned as pairs \{`Filename1`, `Filename2`\}, where `Filename1` (`Filename2`)
exists in directory `Dir1` (`Dir2`).

# `crypto_key_fun`

```erlang
-spec crypto_key_fun(CryptoKeyFun) -> ok | {error, Reason}
                        when CryptoKeyFun :: crypto_fun(), Reason :: badfun | exists | term().
```

Registers an unary fun that is called if `beam_lib` must read an `debug_info`
chunk that has been encrypted. The fun is held in a process that is started by
the function.

If a fun is already registered when attempting to register a fun,
`{error, exists}` is returned.

The fun must handle the following arguments:

```erlang
CryptoKeyFun(init) -> ok | {ok, NewCryptoKeyFun} | {error, Term}
```

Called when the fun is registered, in the process that holds the fun. Here the
crypto key fun can do any necessary initializations. If `{ok, NewCryptoKeyFun}`
is returned, `NewCryptoKeyFun` is registered instead of `CryptoKeyFun`. If
`{error, Term}` is returned, the registration is aborted and
[`crypto_key_fun/1`](`crypto_key_fun/1`) also returns `{error, Term}`.

```erlang
CryptoKeyFun({debug_info, Mode, Module, Filename}) -> Key
```

Called when the key is needed for module `Module` in the file named `Filename`.
`Mode` is the type of crypto algorithm; currently, the only possible value is
`des3_cbc`. The call is to fail (raise an exception) if no key is available.

```text
CryptoKeyFun(clear) -> term()
```

Called before the fun is unregistered. Here any cleaning up can be done. The
return value is not important, but is passed back to the caller of
`clear_crypto_key_fun/0` as part of its return value.

# `diff_dirs`

```erlang
-spec diff_dirs(Dir1, Dir2) -> ok | {error, beam_lib, Reason}
                   when
                       Dir1 :: atom() | file:filename(),
                       Dir2 :: atom() | file:filename(),
                       Reason :: {not_a_directory, term()} | info_rsn().
```

Compares the BEAM files in two directories as `cmp_dirs/2`, but the names of
files that exist in only one directory or are different are presented on
standard output.

# `format_error`

```erlang
-spec format_error(Reason) -> io_lib:chars() when Reason :: term().
```

For a specified error returned by any function in this module, this function
returns a descriptive string of the error in English. For file errors, function
[`file:format_error(Posix)`](`file:format_error/1`) is to be called.

# `info`

```erlang
-spec info(Beam) -> [InfoPair] | {error, beam_lib, info_rsn()}
              when
                  Beam :: beam(),
                  InfoPair ::
                      {file, Filename :: file:filename()} |
                      {binary, Binary :: binary()} |
                      {module, Module :: module()} |
                      {chunks,
                       [{ChunkId :: chunkid(), Pos :: non_neg_integer(), Size :: non_neg_integer()}]}.
```

Returns a list containing some information about a BEAM file as tuples
`{Item, Info}`:

- **`{file, Filename} | {binary, Binary}`** - The name (string) of the BEAM
  file, or the binary from which the information was extracted.

- **`{module, Module}`** - The name (atom) of the module.

- **`{chunks, [{ChunkId, Pos, Size}]}`** - For each chunk, the identifier
  (string) and the position and size of the chunk data, in bytes.

# `md5`

```erlang
-spec md5(Beam) -> {ok, {module(), MD5}} | {error, beam_lib, chnk_rsn()}
             when Beam :: beam(), MD5 :: binary().
```

Calculates an MD5 redundancy check for the code of the module (compilation date
and other attributes are not included).

# `strip`

```erlang
-spec strip(Beam1) -> {ok, {module(), Beam2}} | {error, beam_lib, info_rsn()}
               when Beam1 :: beam(), Beam2 :: beam().
```

Removes all chunks from a BEAM file except those used by the loader.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed.

# `strip`
*since OTP 22.0* 

```erlang
-spec strip(Beam1, AdditionalChunks) -> {ok, {module(), Beam2}} | {error, beam_lib, info_rsn()}
               when Beam1 :: beam(), AdditionalChunks :: [chunkid()], Beam2 :: beam().
```

Removes all chunks from a BEAM file except those used by the loader or mentioned
in `AdditionalChunks`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is removed.

# `strip_files`

```erlang
-spec strip_files(Files) -> {ok, [{module(), Beam}]} | {error, beam_lib, info_rsn()}
                     when Files :: [beam()], Beam :: beam().
```

Removes all chunks except those used by the loader from `Files`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed. The returned list contains one element for each specified filename, in
the same order as in `Files`.

# `strip_files`
*since OTP 22.0* 

```erlang
-spec strip_files(Files, AdditionalChunks) -> {ok, [{module(), Beam}]} | {error, beam_lib, info_rsn()}
                     when Files :: [beam()], AdditionalChunks :: [chunkid()], Beam :: beam().
```

Removes all chunks except those used by the loader or mentioned in
`AdditionalChunks` from `Files`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed. The returned list contains one element for each specified filename,
in the same order as in `Files`.

# `strip_release`

```erlang
-spec strip_release(Dir) -> {ok, [{module(), file:filename()}]} | {error, beam_lib, Reason}
                       when
                           Dir :: atom() | file:filename(),
                           Reason :: {not_a_directory, term()} | info_rsn().
```

Removes all chunks except those used by the loader from the BEAM files of a
release.

`Dir` is to be the installation root directory. For example, the current OTP
release can be stripped with the call `beam_lib:strip_release(code:root_dir())`.

# `strip_release`
*since OTP 22.0* 

```erlang
-spec strip_release(Dir, AdditionalChunks) ->
                       {ok, [{module(), file:filename()}]} | {error, beam_lib, Reason}
                       when
                           Dir :: atom() | file:filename(),
                           AdditionalChunks :: [chunkid()],
                           Reason :: {not_a_directory, term()} | info_rsn().
```

Removes all chunks except those used by the loader or mentioned in
`AdditionalChunks`.

`Dir` is to be the installation root directory. For example, the current OTP
release can be stripped with the call `beam_lib:strip_release(code:root_dir(),[documentation])`.

# `version`

```erlang
-spec version(Beam) -> {ok, {module(), [Version :: term()]}} | {error, beam_lib, chnk_rsn()}
                 when Beam :: beam().
```

Returns the module version or versions. A version is defined by module attribute
`-vsn(Vsn)`.

If this attribute is not specified, the version defaults to the
checksum of the module. Notice that if version `Vsn` is not a list, it is made
into one, that is `{ok,{Module,[Vsn]}}` is returned. If there are many `-vsn`
module attributes, the result is the concatenated list of versions.

_Examples:_

```erlang
1> beam_lib:version(a). % -vsn(1).
{ok,{a,[1]}}
2> beam_lib:version(b). % -vsn([1]).
{ok,{b,[1]}}
3> beam_lib:version(c). % -vsn([1]). -vsn(2).
{ok,{c,[1,2]}}
4> beam_lib:version(d). % no -vsn attribute
{ok,{d,[275613208176997377698094100858909383631]}}
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
