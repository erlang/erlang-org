# `mod_alias`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_server/mod_alias.erl#L23)

URL aliasing.

Erlang web server internal API for handling of, for example, interaction data
exported by module `mod_alias`.

# `default_index`

```erlang
-spec default_index(ConfigDB, Path) -> NewPath
                       when ConfigDB :: ets:tid(), Path :: string(), NewPath :: string().
```

If `Path` is a directory, `default_index/2`, it starts
searching for resources or files that are specified in the config directive
`DirectoryIndex`. If an appropriate resource or file is found, it is appended to
the end of `Path` and then returned. `Path` is returned unaltered if no
appropriate file is found or if `Path` is not a directory. `config_db()` is the
server config file in ETS table format as described in
[Inets User's Guide](http_server.md).

# `path`

```erlang
-spec path(Data, ConfigDB, RequestURI) -> Path
              when
                  Data :: [{real_name, {Path, AfterPath}}],
                  ConfigDB :: ets:tid(),
                  RequestURI :: string(),
                  AfterPath :: string(),
                  Path :: string().
```

`path/3` returns the file `Path` in the `RequestURI` (see
[RFC 1945](https://www.ietf.org/rfc/rfc1945.txt)). If the interaction data
`{real_name, {Path, AfterPath}}` has been exported by `mod_alias`, `Path` is
returned. If no interaction data has been exported, `ServerRoot` is used to
generate a file `Path`. `config_db()` and `interaction_data()` are as defined in
[Inets User's Guide](http_server.md).

# `real_name`

```erlang
-spec real_name(ConfigDB, RequestURI, Aliases) -> ReturnPath
                   when
                       ConfigDB :: ets:tid(),
                       RequestURI :: string(),
                       Aliases :: [{FakeName, RealName}],
                       ReturnPath :: {ShortPath, Path, AfterPath},
                       FakeName :: re:mp() | iodata() | unicode:charlist() | string(),
                       RealName :: string(),
                       ShortPath :: string(),
                       Path :: string(),
                       AfterPath :: string().
```

`real_name/3` traverses `Aliases`, typically extracted from
`ConfigDB`, and matches each `FakeName` with `RequestURI`. If a match is found,
`FakeName` is replaced with `RealName` in the match. The resulting path is split
into two parts, `ShortPath` and `AfterPath`, as defined in
`httpd_util:split_path/1`. `Path` is generated from `ShortPath`, that is, the
result from `default_index/2` with `ShortPath` as
an argument. `config_db()` is the server config file in ETS table format as
described in [Inets User's Guide](http_server.md).

# `real_script_name`

```erlang
-spec real_script_name(ConfigDB, RequestURI, ScriptAliases) -> ReturnPath | not_a_script
                          when
                              ConfigDB :: ets:tid(),
                              RequestURI :: string(),
                              ScriptAliases :: list() | [{FakeName, RealName}],
                              ReturnPath :: {ShortPath, AfterPath},
                              FakeName :: re:mp() | iodata() | unicode:charlist() | string(),
                              RealName :: string(),
                              ShortPath :: string(),
                              AfterPath :: term().
```

`real_script_name/3` traverses `ScriptAliases`,
typically extracted from `ConfigDB`, and matches each `FakeName` with
`RequestURI`. If a match is found, `FakeName` is replaced with `RealName` in the
match. If the resulting match is not an executable script, `not_a_script` is
returned. If it is a script, the resulting script path is in two parts,
`ShortPath` and `AfterPath`, as defined in `httpd_util:split_script_path/1`.
`config_db()` is the server config file in ETS table format as described in
[Inets User's Guide](http_server.md).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
