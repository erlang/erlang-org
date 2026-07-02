# `escript`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/escript.erl#L22)

This module provides functions to create and inspect escripts.

See the [escript](`e:erts:escript_cmd.md`) program documentation
for more details on how to use escripts.

# `comment`
*not exported* 

```erlang
-type comment() :: string().
```

# `emu_args`
*not exported* 

```erlang
-type emu_args() :: string().
```

Any arguments that should be passed to [erl](`e:erts:erl_cmd.md`) when starting.

# `extract_option`
*not exported* 

```erlang
-type extract_option() :: compile_source | {section, [section_name()]}.
```

# `section`
*not exported* 

```erlang
-type section() ::
          shebang |
          {shebang, shebang() | default | undefined} |
          comment |
          {comment, comment() | default | undefined} |
          {emu_args, emu_args() | undefined} |
          {source, file:filename() | binary()} |
          {beam, file:filename() | binary()} |
          {archive, zip:filename() | binary()} |
          {archive, [zip_file()], [zip:create_option()]}.
```

# `section_name`
*not exported* 

```erlang
-type section_name() :: shebang | comment | emu_args | body.
```

# `shebang`
*not exported* 

```erlang
-type shebang() :: string().
```

The initial `#!` line.

For example:

```text
#!/usr/bin/env escript
```

# `zip_file`
*not exported* 

```erlang
-type zip_file() ::
          zip:filename() | {zip:filename(), binary()} | {zip:filename(), binary(), file:file_info()}.
```

# `create`

```erlang
-spec create(file:filename() | binary(), [section()]) -> ok | {ok, binary()} | {error, term()}.
```

Creates an escript from a list of sections.

The sections can be specified in any order. An escript begins with an optional
`Header` followed by a mandatory `Body`. If the header is present, it does always
 begin with a `shebang`, possibly followed by a `comment` and `emu_args`. The
`shebang` defaults to `"/usr/bin/env escript"`. The `comment` defaults to
`"This is an -*- erlang -*- file"`. The created escript can either be returned
as a binary or written to file.

As an example of how the function can be used, we create an interpreted escript
that uses `emu_args` to set some emulator flag. In this case, it happens to set
number of schedulers with `+S3`. We also extract the different sections from the
newly created script:

```erlang
> Source = "%% Demo\nmain(_Args) ->\n    io:format(\"~p\",[erlang:system_info(schedulers)]).\n".
"%% Demo\nmain(_Args) ->\n    io:format(erlang:system_info(schedulers)).\n"
> io:format("~s\n", [Source]).
%% Demo
main(_Args) ->
    io:format(erlang:system_info(schedulers)).

ok
> {ok, Bin} = escript:create(binary, [shebang, comment, {emu_args, "+S3"},
                                      {source, list_to_binary(Source)}]).
{ok,<<"#!/usr/bin/env escript\n%% This is an -*- erlang -*- file\n%%!+S3"...>>}
> file:write_file("demo.escript", Bin).
ok
> os:cmd("escript demo.escript").
"3"
> escript:extract("demo.escript", []).
{ok,[{shebang,default}, {comment,default}, {emu_args,"+S3"},
     {source,<<"%% Demo\nmain(_Args) ->\n    io:format(erlang:system_info(schedu"...>>}]}
```

An escript without header can be created as follows:

```erlang
> file:write_file("demo.erl",
                  ["%% demo.erl\n-module(demo).\n-export([main/1]).\n\n", Source]).
ok
> {ok, _, BeamCode} = compile:file("demo.erl", [binary, debug_info]).
{ok,demo,
    <<70,79,82,49,0,0,2,208,66,69,65,77,65,116,111,109,0,0,0,
      79,0,0,0,9,4,100,...>>}
> escript:create("demo.beam", [{beam, BeamCode}]).
ok
> escript:extract("demo.beam", []).
{ok,[{shebang,undefined}, {comment,undefined}, {emu_args,undefined},
     {beam,<<70,79,82,49,0,0,3,68,66,69,65,77,65,116,
             111,109,0,0,0,83,0,0,0,9,...>>}]}
> os:cmd("escript demo.beam").
"true"
```

Here we create an archive script containing both Erlang code and Beam code, then
we iterate over all files in the archive and collect their contents and some
information about them:

```erlang
> {ok, SourceCode} = file:read_file("demo.erl").
{ok,<<"%% demo.erl\n-module(demo).\n-export([main/1]).\n\n%% Demo\nmain(_Arg"...>>}
> escript:create("demo.escript",
                 [shebang,
                  {archive, [{"demo.erl", SourceCode},
                             {"demo.beam", BeamCode}], []}]).
ok
> {ok, [{shebang,default}, {comment,undefined}, {emu_args,undefined},
     {archive, ArchiveBin}]} = escript:extract("demo.escript", []).
{ok,[{shebang,default}, {comment,undefined}, {emu_args,undefined},
     {{archive,<<80,75,3,4,20,0,0,0,8,0,118,7,98,60,105,
                152,61,93,107,0,0,0,118,0,...>>}]}
> file:write_file("demo.zip", ArchiveBin).
ok
> zip:foldl(fun(N, I, B, A) -> [{N, I(), B()} | A] end, [], "demo.zip").
{ok,[{"demo.beam",
      {file_info,748,regular,read_write,
                 {{2010,3,2},{0,59,22}},
                 {{2010,3,2},{0,59,22}},
                 {{2010,3,2},{0,59,22}},
                 54,1,0,0,0,0,0},
      <<70,79,82,49,0,0,2,228,66,69,65,77,65,116,111,109,0,0,0,
        83,0,0,...>>},
     {"demo.erl",
      {file_info,118,regular,read_write,
                 {{2010,3,2},{0,59,22}},
                 {{2010,3,2},{0,59,22}},
                 {{2010,3,2},{0,59,22}},
                 54,1,0,0,0,0,0},
      <<"%% demo.erl\n-module(demo).\n-export([main/1]).\n\n%% Demo\nmain(_Arg"...>>}]}
```

# `extract`

```erlang
-spec extract(file:filename(), [extract_option()]) -> {ok, [section()]} | {error, term()}.
```

Parses an escript and extracts its sections. This is the reverse of `create/2`.

All sections are returned even if they do not exist in the escript. If a
particular section happens to have the same value as the default value, the
extracted value is set to the atom `default`. If a section is missing, the
extracted value is set to the atom `undefined`.

Option `compile_source` only affects the result if the escript contains `source`
code. In this case the Erlang code is automatically compiled and
`{source, BeamCode}` is returned instead of `{source, SourceCode}`.

Example:

```erlang
> escript:create("demo.escript",
                 [shebang, {archive, [{"demo.erl", SourceCode},
                                      {"demo.beam", BeamCode}], []}]).
ok
> {ok, [{shebang,default}, {comment,undefined}, {emu_args,undefined},
     {archive, ArchiveBin}]} =
              escript:extract("demo.escript", []).
{ok,[{{archive,<<80,75,3,4,20,0,0,0,8,0,118,7,98,60,105,
                152,61,93,107,0,0,0,118,0,...>>}
     {emu_args,undefined}]}
```

# `script_name`

```erlang
-spec script_name() -> string().
```

Returns the name of the escript that is executed.

If the function is invoked outside the context of an escript,
the behavior is undefined.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
