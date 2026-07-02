# `epp`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/epp.erl#L22)

An Erlang code preprocessor.

The Erlang code preprocessor includes functions that are used by the `m:compile`
module to preprocess macros and include files before the parsing takes place.

The Erlang source file _encoding_{: #encoding } is selected by a comment in one
of the first two lines of the source file. The first string matching the regular
expression `coding\s*[:=]\s*([-a-zA-Z0-9])+` selects the encoding. If the
matching string is not a valid encoding, it is ignored. The valid encodings are
`Latin-1` and `UTF-8`, where the case of the characters can be chosen freely.

_Examples:_

```erlang
%% coding: utf-8
```

```erlang
%% For this file we have chosen encoding = Latin-1
```

```erlang
%% -*- coding: latin-1 -*-
```

## Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLine, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

### See Also

`m:erl_parse`

# `epp_handle`
*not exported* 

```erlang
-type epp_handle() :: pid().
```

Handle to the `epp` server.

# `macros`
*not exported* 

```erlang
-type macros() :: [atom() | {atom(), term()} | {atom(), term(), redefine}].
```

# `source_encoding`

```erlang
-type source_encoding() :: latin1 | utf8.
```

# `warning_info`
*not exported* 

```erlang
-type warning_info() :: {erl_anno:location(), module(), term()}.
```

# `close`

```erlang
-spec close(Epp) -> ok when Epp :: epp_handle().
```

Closes the preprocessing of a file.

# `default_encoding`
*since OTP R16B* 

```erlang
-spec default_encoding() -> source_encoding().
```

Returns the default encoding of Erlang source files.

# `encoding_to_string`
*since OTP R16B* 

```erlang
-spec encoding_to_string(Encoding) -> string() when Encoding :: source_encoding().
```

Returns a string representation of an encoding. The string is recognized by
[`read_encoding/1,2`](`read_encoding/1`),
[`read_encoding_from_binary/1,2`](`read_encoding_from_binary/1`), and
[`set_encoding/1,2`](`set_encoding/1`) as a valid encoding.

# `format_error`
*since OTP R14B03* 

```erlang
-spec format_error(ErrorDescriptor) -> io_lib:chars() when ErrorDescriptor :: term().
```

Takes an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when processing an
`ErrorInfo` structure (see section [Error Information](`m:epp#module-error-information`)).

# `open`
*since OTP 17.0* 

```erlang
-spec open(Options) -> {ok, Epp} | {ok, Epp, Extra} | {error, ErrorDescriptor}
              when
                  Options ::
                      [{default_encoding, DefEncoding :: source_encoding()} |
                       {includes, IncludePath :: [DirectoryName :: file:name()]} |
                       {source_name, SourceName :: file:name()} |
                       {deterministic, Enabled :: boolean()} |
                       {macros, PredefMacros :: macros()} |
                       {name, FileName :: file:name()} |
                       {location, StartLocation :: erl_anno:location()} |
                       {fd, FileDescriptor :: file:io_device()} |
                       extra |
                       {compiler_internal, [term()]}],
                  Epp :: epp_handle(),
                  Extra :: [{encoding, source_encoding() | none}],
                  ErrorDescriptor :: term().
```

Opens a file for preprocessing.

If you want to change the file name of the implicit -file() attributes inserted
during preprocessing, you can do with `{source_name, SourceName}`. If unset it
will default to the name of the opened file.

Setting `{deterministic, Enabled}` will additionally reduce the file name of the
implicit -file() attributes inserted during preprocessing to only the basename
of the path.

If `extra` is specified in `Options`, the return value is `{ok, Epp, Extra}`
instead of `{ok, Epp}`.

The option `location` is forwarded to the Erlang token scanner, see
[`erl_scan:tokens/3,4`](`erl_scan:tokens/3`).

The `{compiler_internal,term()}` option is forwarded to the Erlang token
scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`).

# `open`

```erlang
-spec open(FileName, IncludePath) -> {ok, Epp} | {error, ErrorDescriptor}
              when
                  FileName :: file:name(),
                  IncludePath :: [DirectoryName :: file:name()],
                  Epp :: epp_handle(),
                  ErrorDescriptor :: term().
```

Equivalent to `epp:open([{name, FileName}, {includes, IncludePath}])`.

# `open`

```erlang
-spec open(FileName, IncludePath, PredefMacros) -> {ok, Epp} | {error, ErrorDescriptor}
              when
                  FileName :: file:name(),
                  IncludePath :: [DirectoryName :: file:name()],
                  PredefMacros :: macros(),
                  Epp :: epp_handle(),
                  ErrorDescriptor :: term().
```

Equivalent to
`epp:open([{name, FileName}, {includes, IncludePath}, {macros, PredefMacros}])`.

# `parse_erl_form`

```erlang
-spec parse_erl_form(Epp) ->
                        {ok, AbsForm} | {error, ErrorInfo} | {warning, WarningInfo} | {eof, Location}
                        when
                            Epp :: epp_handle(),
                            AbsForm :: erl_parse:abstract_form(),
                            Location :: erl_anno:location(),
                            ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
                            WarningInfo :: warning_info().
```

Returns the next Erlang form from the opened Erlang source file. Tuple
`{eof, Location}` is returned at the end of the file. The first form corresponds
to an implicit attribute `-file(File,1).`, where `File` is the file name.

# `parse_file`
*since OTP 17.0* 

```erlang
-spec parse_file(FileName, Options) -> {ok, [Form]} | {ok, [Form], Extra} | {error, OpenError}
                    when
                        FileName :: file:name(),
                        Options ::
                            [{includes, IncludePath :: [DirectoryName :: file:name()]} |
                             {source_name, SourceName :: file:name()} |
                             {macros, PredefMacros :: macros()} |
                             {default_encoding, DefEncoding :: source_encoding()} |
                             {location, StartLocation :: erl_anno:location()} |
                             {reserved_word_fun, Fun :: fun((atom()) -> boolean())} |
                             {features, [Feature :: atom()]} |
                             {deterministic, boolean()} |
                             extra |
                             {compiler_internal, [term()]}],
                        Form :: erl_parse:abstract_form() | {error, ErrorInfo} | {eof, Location},
                        Location :: erl_anno:location(),
                        ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
                        Extra :: [{encoding, source_encoding() | none}],
                        OpenError :: file:posix() | badarg | system_limit.
```

Preprocesses and parses an Erlang source file. Notice that tuple
`{eof, Location}` returned at the end of the file is included as a "form".

If you want to change the file name of the implicit -file() attributes inserted
during preprocessing, you can do with `{source_name, SourceName}`. If unset it
will default to the name of the opened file.

If `extra` is specified in `Options`, the return value is `{ok, [Form], Extra}`
instead of `{ok, [Form]}`.

The option `location` is forwarded to the Erlang token scanner, see
[`erl_scan:tokens/3,4`](`erl_scan:tokens/3`).

The `{compiler_internal,term()}` option is forwarded to the Erlang token
scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`).

# `parse_file`

```erlang
-spec parse_file(FileName, IncludePath, PredefMacros) -> {ok, [Form]} | {error, OpenError}
                    when
                        FileName :: file:name(),
                        IncludePath :: [DirectoryName :: file:name()],
                        Form :: erl_parse:abstract_form() | {error, ErrorInfo} | {eof, Location},
                        PredefMacros :: macros(),
                        Location :: erl_anno:location(),
                        ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
                        OpenError :: file:posix() | badarg | system_limit.
```

Equivalent to
`epp:parse_file(FileName, [{includes, IncludePath}, {macros, PredefMacros}])`.

# `read_encoding`
*since OTP R16B* 

```erlang
-spec read_encoding(FileName) -> source_encoding() | none when FileName :: file:name().
```

# `read_encoding`
*since OTP R16B* 

```erlang
-spec read_encoding(FileName, Options) -> source_encoding() | none
                       when
                           FileName :: file:name(),
                           Options :: [Option],
                           Option :: {in_comment_only, boolean()}.
```

Read the [encoding](`m:epp#encoding`) from a file. Returns the read encoding, or
`none` if no valid encoding is found.

Option `in_comment_only` is `true` by default, which is correct for Erlang
source files. If set to `false`, the encoding string does not necessarily have
to occur in a comment.

# `read_encoding_from_binary`
*since OTP R16B* 

```erlang
-spec read_encoding_from_binary(Binary) -> source_encoding() | none when Binary :: binary().
```

# `read_encoding_from_binary`
*since OTP R16B* 

```erlang
-spec read_encoding_from_binary(Binary, Options) -> source_encoding() | none
                                   when
                                       Binary :: binary(),
                                       Options :: [Option],
                                       Option :: {in_comment_only, boolean()}.
```

Read the [encoding](`m:epp#encoding`) from a binary. Returns the read encoding,
or `none` if no valid encoding is found.

Option `in_comment_only` is `true` by default, which is correct for Erlang
source files. If set to `false`, the encoding string does not necessarily have
to occur in a comment.

# `scan_erl_form`
*since OTP R13B03* 

```erlang
-spec scan_erl_form(Epp) -> {ok, Tokens} | {error, ErrorInfo} | {warning, WarningInfo} | {eof, Line}
                       when
                           Epp :: epp_handle(),
                           Tokens :: erl_scan:tokens(),
                           Line :: erl_anno:line(),
                           ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
                           WarningInfo :: warning_info().
```

Returns the raw tokens of the next Erlang form from the opened Erlang source
file. A tuple `{eof, Line}` is returned at the end of the file. The first form
corresponds to an implicit attribute `-file(File,1).`, where `File` is the file
name.

# `scan_file`
*since OTP 24.0* 

```erlang
-spec scan_file(FileName, Options) -> {ok, [Form], Extra} | {error, OpenError}
                   when
                       FileName :: file:name(),
                       Options ::
                           [{includes, IncludePath :: [DirectoryName :: file:name()]} |
                            {source_name, SourceName :: file:name()} |
                            {macros, PredefMacros :: macros()} |
                            {default_encoding, DefEncoding :: source_encoding()}],
                       Form :: erl_scan:tokens() | {error, ErrorInfo} | {eof, Loc},
                       Loc :: erl_anno:location(),
                       ErrorInfo :: erl_scan:error_info(),
                       Extra :: [{encoding, source_encoding() | none}],
                       OpenError :: file:posix() | badarg | system_limit.
```

Preprocesses an Erlang source file returning a list of the lists of raw tokens
of each form. Notice that the tuple `{eof, Line}` returned at the end of the
file is included as a "form", and any failures to scan a form are included in
the list as tuples `{error, ErrorInfo}`.

# `set_encoding`
*since OTP R16B* 

```erlang
-spec set_encoding(File) -> source_encoding() | none when File :: io:device().
```

Reads the [encoding](`m:epp#encoding`) from an I/O device and sets the encoding
of the device accordingly. The position of the I/O device referenced by `File`
is not affected. If no valid encoding can be read from the I/O device, the
encoding of the I/O device is set to the default encoding.

Returns the read encoding, or `none` if no valid encoding is found.

# `set_encoding`
*since OTP 17.0* 

```erlang
-spec set_encoding(File, Default) -> source_encoding() | none
                      when Default :: source_encoding(), File :: io:device().
```

Reads the [encoding](`m:epp#encoding`) from an I/O device and sets the encoding
of the device accordingly. The position of the I/O device referenced by `File`
is not affected. If no valid encoding can be read from the I/O device, the
encoding of the I/O device is set to the [encoding](`m:epp#encoding`) specified
by `Default`.

Returns the read encoding, or `none` if no valid encoding is found.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
