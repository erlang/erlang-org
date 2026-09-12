# `epp`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/epp.erl#L22)

An Erlang code preprocessor.

The Erlang code preprocessor includes functions that are used by the `m:compile`
module to preprocess macros and include files before the parsing takes place.
For example:

```erlang
1> file:read_file("example.erl").
{ok, ~"""
      -module(example).

      -export([foo/0]).

      foo() -> ?MODULE.
      """}
2> epp:parse_file("example.erl", []).
{ok,[{attribute,1,file,{"example.erl",1}},
     {attribute,1,module,example},
     {attribute,3,export,[{foo,0}]},
     {function,5,foo,0,[{clause,5,[],[],[{atom,5,example}]}]},
     {eof,5}]}
```

## Encoding

The Erlang source file _encoding_{: #encoding } is selected by a comment in one
of the first two lines of the source file. The first string matching the regular
expression `coding\s*[:=]\s*([-a-zA-Z0-9])+` selects the encoding. If the
matching string is not a valid encoding, it is ignored. The valid encodings are
`Latin-1` and `UTF-8`, where the case of the characters can be chosen freely.

### Examples

```erlang
%% coding: utf-8
```

```erlang
%% For this file we have chosen encoding = Latin-1
```

```erlang
%% -*- coding: latin-1 -*-
```

Scan a ram file using the encoding specified in the file:

```erlang
1> {ok, IoDevice} = file:open(
       ~"""
        %% coding: utf-8
        -module(foo).
        -export([bar/0]).
        bar() ->
            ?MODULE.
        """, [read, binary, ram, cooked]).
2> epp:parse_file("foo.erl", [{fd, IoDevice}, extra]).
{ok,[{attribute,1,file,{"foo.erl",1}},
     {attribute,2,module,foo},
     {attribute,3,export,[{bar,0}]},
     {function,4,bar,0,[{clause,4,[],[],[{atom,5,foo}]}]},
     {eof,5}],
    [{features,[]},{encoding,utf8}]}
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

### Examples

```erlang
1> {ok, IoDevice} = file:open(
       ~"""
        -module("foo").
        -export([bar/0]).
        bar() ->
            ?MODULE.
        """, [read, binary, ram, cooked]).
2> {ok, Forms} = epp:parse_file("foo.erl", [{fd, IoDevice}]).
{ok,[{attribute,1,file,{"foo.erl",1}},
     {error,{1,erl_parse,[98,97,100,32,"module",32,100,101,99,108,97,114,97,116,105,111,110]}},
     {attribute,2,export,[{bar,0}]},
     {error,{4,epp,{undefined,'MODULE',none}}},
     {eof,4}]}
3> [io:format("%% ~ts~n", [M:format_error(E)]) || {error,{Loc,M,E}} <- Forms].
%% bad module declaration
%% undefined macro 'MODULE'
[ok,ok]
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

# `option`
*not exported* 

```erlang
-type option() ::
          {default_encoding, DefEncoding :: source_encoding()} |
          {includes, IncludePath :: [DirectoryName :: file:name()]} |
          {macros, PredefMacros :: macros()} |
          {source_name, SourceName :: file:name()} |
          {deterministic, boolean()} |
          {location, StartLocation :: erl_anno:location()} |
          {reserved_word_fun, Fun :: fun((atom()) -> boolean())} |
          {features, [Feature :: atom()]} |
          {fd, OpenedFile :: file:io_server()} |
          {include_path_open,
           fun((Path :: [file:name_all()], FileName :: file:name_all(), Modes :: [file:mode()]) ->
                   {ok, IoDevice :: file:io_device(), FullName :: file:filename_all()} | {error, term()})} |
          extra |
          {compiler_internal, [term()]}.
```

The `t:option/0` are the options that can be used to customize the preprocessing.

* **`{default_encoding, DefEncoding}`** - sets the default encoding of the file. The default encoding is
  used if no valid encoding is found in the file. The valid encodings are `latin1` and `utf8`,
  where the case of the characters can be chosen freely. If unset, it defaults to `utf8`.

* **`{includes, IncludePath}`** - sets the include path for the file. The include path is used to resolve
  `-include`, `-include_lib` directives.

* **`{source_name, SourceName}`** - sets the file name of the implicit -file() attributes inserted
  during preprocessing. If unset it will default to the name of the opened file.

* **`{macros, PredefMacros}`** - sets the predefined `t:macros/0` for the file. `PredefMacros` is a list of
  macros that are defined before preprocessing starts.

* **`{deterministic, Enabled}`** - if set to `true`, will reduce the file name of the
  implicit -file() attributes inserted during preprocessing to only the basename of the path. 

* **`{extra, Enabled}`** - if set to `true`, the return value is `{ok, Epp, Extra}` instead of `{ok, Epp}`,
  where `Extra` contains which encoding was detected from the file.

* **`{location, StartLocation}`** - sets the initial location of the file. The option `location` is forwarded
  to the Erlang token scanner, see [`erl_scan:tokens/3,4`](`erl_scan:tokens/3`). For example:

  ```erlang
  1> file:read_file("example.erl").
  {ok, ~"""
        -module(example).

        -export([foo/0]).

        foo() -> ?MODULE.
        """}
  2> epp:parse_file("example.erl", [{location, {1, 1}}]).
  {ok,[{attribute,{1,1},file,{"example.erl",1}},
       {attribute,{1,2},module,example},
       {attribute,{3,2},export,[{foo,0}]},
       {function,{5,1},
                 foo,0,
                 [{clause,{5,1},[],[],[{atom,{5,11},example}]}]},
       {eof,{5,18}}]}
  ```

* **`{fd, FileDescriptor}`** - use an already opened file descriptor to read from instead of a file name.
  The file descriptor is expected to be an `t:file:io_server/0`. This enables in-memory preprocessing
  using [ram files](`m:file#ram`), where the main source file can be served from memory without touching disk.

  See `parse_file/2` for an example of how to use this option.

* **`{include_path_open, Fun}`** - provide a custom function for opening include files. The function has the
  same signature as `file:path_open/3` and is used when resolving `-include`, `-include_lib`, and
  `-doc {file, ...}` directives. This enables fully in-memory preprocessing using [ram files](`m:file#ram`).
  For example:

  ```erlang
  1> {ok, IoDevice} = file:open(<<"-module(foo).\n-include(\"bar.hrl\").">>,
         [read, binary, ram, cooked]).
  2> IncludeFileFun = fun
         (_Path, "bar.hrl", Modes) ->
             {ok, RamFD} = file:open(<<"-export([bar/0]).\nbar() ->\n?MODULE.">>,
                  [ram, cooked | Modes]),
             {ok, RamFD, "bar.hrl"};
         (Path, Name, Modes) ->
             file:path_open(Path, Name, Modes)
     end.
  3> {ok, EPP} = epp:parse_file("example.erl",
         [{fd, IoDevice}, {include_path_open, IncludeFileFun}]).
  {ok,[{attribute,1,file,{"example.erl",1}},
       {attribute,1,module,foo},
       {attribute,1,file,{"bar.hrl",1}},
       {attribute,1,export,[{bar,0}]},
       {function,2,bar,0,[{clause,2,[],[],[{atom,3,foo}]}]},
       {attribute,2,file,{"example.erl",2}},
       {eof,2}]}
  ```

  Since OTP @OTP-20341@
* **`{compiler_internal,term()}`** - forwarded to the Erlang token
  scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`) in `erl_scan:string/3`.

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
-spec close(Epp :: epp_handle()) -> ok.
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
                  Options :: [option() | {name, FileName :: file:name()}],
                  Epp :: epp_handle(),
                  Extra :: [{encoding, source_encoding() | none}],
                  ErrorDescriptor :: term().
```

Opens a file for preprocessing.

The function is used to start parsing of an Erlang source file. To get the forms from the file
use `scan_erl_form/1` or `parse_erl_form/1`. When finished, the `m:epp` server should be closed with
`close/1`. Use this function if you want to scan or parse a file incrementally. To scan or parse
a whole file at once, use `scan_file/2` and `parse_file/3` respectively.

When using `open/1` the option `name` must always be specified and is the name of the file to
preprocess. This name is used in error messages and in the implicit `-file()` attributes
generated during preprocessing.

See `t:option/0` for the other available options and what they do.

## Examples

```erlang
1> file:read_file("example.erl").
{ok, ~"""
      -module(example).

      -export([foo/0]).

      foo() -> ?MODULE.
      """}
2> {ok, EPP} = epp:open("example.erl", []).
3> epp:scan_erl_form(EPP).
{ok,[{'-',1},{atom,1,file},{'(',1},{string,1,"example.erl"},
     {',',1},{integer,1,1},{')',1},{dot,1}]}
4> epp:parse_erl_form(EPP).
{ok,{attribute,1,module,example}}
5> epp:close(EPP).
ok
```

# `open`

```erlang
-spec open(FileName, IncludePath) -> {ok, Epp} | {error, ErrorDescriptor}
              when
                  FileName :: file:name(),
                  IncludePath :: [DirectoryName :: file:name()],
                  Epp :: epp_handle(),
                  ErrorDescriptor :: term().
```

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

Returns the next Erlang form from the opened Erlang source file.

Tuple `{eof, Location}` is returned at the end of the file. The first form corresponds
to an implicit attribute `-file(File,1).`, where `File` is the file name.

# `parse_file`
*since OTP 17.0* 

```erlang
-spec parse_file(FileName, Options) -> {ok, [Form]} | {ok, [Form], Extra} | {error, OpenError}
                    when
                        FileName :: file:name(),
                        Options :: [option()],
                        Form :: erl_parse:abstract_form() | {error, ErrorInfo} | {eof, Location},
                        Location :: erl_anno:location(),
                        ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
                        Extra :: [{encoding, source_encoding() | none}],
                        OpenError :: file:posix() | badarg | system_limit.
```

Preprocesses and parses an Erlang source file.

Notice that tuple `{eof, Location}` returned at the end of the file is included as
a "form".

See `t:option/0` for the available options and what they do.

## Examples

Parse a file:

```erlang
1> file:read_file("example.erl").
{ok, <<"-module(example).\n\n-export([foo/0]).\n\nfoo() -> ?MODULE.", ...>>}
2> epp:parse_file("example.erl", []).
{ok,[{attribute,1,file,{"example.erl",1}},
     {attribute,1,module,example},
     {attribute,3,export,[{foo,0}]},
     {function,5,foo,0,[{clause,5,[],[],[{atom,5,example}]}]},
     {eof,5}]}
```

Parse a module from a ram file:

```erlang
1> {ok, IoDevice} = file:open(<<"-module(foo).\n-export([bar/0]).\nbar() ->\n?MODULE.">>,
       [read, binary, ram, cooked]).
2> epp:parse_file("foo.erl", [{fd, IoDevice}]).
{ok,[{attribute,1,file,{"foo.erl",1}},
     {attribute,1,module,foo},
     {attribute,2,export,[{bar,0}]},
     {function,3,bar,0,[{clause,3,[],[],[{atom,4,foo}]}]},
     {eof,4}]}
```

Parse a broken ram file:

```erlang
1> {ok, IoDevice} = file:open(<<"-module(Foo).\n-export([bar/0]).\nbar() ->\n?MODULE.">>,
       [read, binary, ram, cooked]).
2> {ok, Forms} = epp:parse_file("foo.erl", [{fd, IoDevice}]).
{ok,[{attribute,1,file,{"foo.erl",1}},
     {error,{1,erl_parse,
             [98,97,100,32,"module",32,100,101,99,108,97,114,97,116,105,
              111,110]}},
     {attribute,2,export,[{bar,0}]},
     {error,{4,epp,{undefined,'MODULE',none}}},
     {eof,4}]}
3> [io:format("%% ~ts~n", [M:format_error(E)]) || {error,{Loc,M,E}} <- Forms].
%% bad module declaration
%% undefined macro 'MODULE'
[ok,ok]
```

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
file.

A tuple `{eof, Line}` is returned at the end of the file. The first form
corresponds to an implicit attribute `-file(File,1).`, where `File` is the file
name.

# `scan_file`
*since OTP 24.0* 

```erlang
-spec scan_file(FileName, Options) -> {ok, [Form], Extra} | {error, OpenError}
                   when
                       FileName :: file:name(),
                       Options :: [option()],
                       Form :: erl_scan:tokens() | {error, ErrorInfo} | {eof, Loc},
                       Loc :: erl_anno:location(),
                       ErrorInfo :: erl_scan:error_info(),
                       Extra :: [{encoding, source_encoding() | none}],
                       OpenError :: file:posix() | badarg | system_limit.
```

Preprocesses an Erlang source file returning a list of the lists of raw tokens
of each form.

Notice that the tuple `{eof, Line}` returned at the end of the file is included
as a "form", and any failures to scan a form are included in the list as tuples
`{error, ErrorInfo}`.

For details on what each `Option` does, see `t:option/0`.

## Examples

```
1> file:read_file("example.erl").
{ok, <<"-module(example).\n\n-export([foo/0]).\n\nfoo() -> ?MODULE.", ...>>}
2> epp:scan_file("example.erl", []).
{ok,[[{'-',1},{atom,1,file},{'(',1},{string,1,"example.erl"},
      {',',1},{integer,1,1},{')',1},{dot,1}], ...],
    [{encoding,none}]}
```

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
