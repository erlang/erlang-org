-module(erlorg_versions_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {erlorg_base_handler,
         [
          init/3,
          content_types_provided/2,
          rest_init/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_get/2
  ]).

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  Versions = versions(),
  {ok, Body} = versions_dtl:render(#{versions => Versions}),
  {Body, Req, State}.

versions() ->
  {ok, DocDir} = application:get_env(erlorg, documentation_dir),
  {ok, DocPrefix} = application:get_env(erlorg, documentation_prefix),
  DocBase = filename:join(DocDir, DocPrefix),
  Paths = filelib:wildcard(DocBase ++ "*"),

  {ok, Regex} = re:compile(DocBase ++ "(?<VER>.*)"),

  Versions = [version(DocBase, Regex, FilePath) || FilePath <- Paths],
  lists:reverse(Versions).

version(DocBase, Regex, Path) ->
  {match, [{A, B}]} = re:run(Path, Regex, [{capture, ['VER']}]),
  Ver = lists:sublist(Path, A + 1, B),
  #{doc  => filelib:is_dir(filename:join([Path, "doc", "pdf"])),
    html => filelib:is_dir(filename:join(Path, "doc")),
    pdf  => filelib:is_dir(filename:join(Path, "pdf")),
    release => ver2rel(DocBase, Ver),
    version => Ver}.

%% Pre-R13 hardcoded version mapping
ver2rel(_, "4.7.3") -> "R4B";
ver2rel(_, "4.8.1") -> "R5B";
ver2rel(_, "4.8.2") -> "R5B01";
ver2rel(_, "4.9.1") -> "R6B";
ver2rel(_, "5.0.1") -> "R7B";
ver2rel(_, "5.0.2") -> "R7B01";
ver2rel(_, "5.1") -> "R8B";
ver2rel(_, "5.2") -> "R9B";
ver2rel(_, "5.3") -> "R9C";
ver2rel(_, "5.3.6.13") -> "R9C patched";
ver2rel(_, "5.4") -> "R10B";
ver2rel(_, "5.4.2.1") -> "R10B-1a";
ver2rel(_, "5.4.3") -> "R10B-2";
ver2rel(_, "5.4.5") -> "R10B-4";
ver2rel(_, "5.4.6") -> "R10B-5";
ver2rel(_, "5.4.8") -> "R10B-6";
ver2rel(_, "5.4.9") -> "R10B-7";
ver2rel(_, "5.4.10") -> "R10B-8";
ver2rel(_, "5.4.12") -> "R10B-9";
ver2rel(_, "5.4.13") -> "R10B-10";
ver2rel(_, "5.4.14") -> "R10B patched";
ver2rel(_, "5.5") -> "R11B";
ver2rel(_, "5.5.1") -> "R11B-1";
ver2rel(_, "5.5.2") -> "R11B-2";
ver2rel(_, "5.5.3") -> "R11B-3";
ver2rel(_, "5.5.4") -> "R11B-4";
ver2rel(_, "5.5.5") -> "R11B-5";
ver2rel(_, "5.6") -> "R12B";
ver2rel(_, "5.6.1") -> "R12B-1";
ver2rel(_, "5.6.2") -> "R12B-2";
ver2rel(_, "5.6.3") -> "R12B-3";
ver2rel(_, "5.6.4") -> "R12B-4";
ver2rel(_, "5.6.5") -> "R12B-5";
ver2rel(DocBase, Ver) ->
  %% Since R13 the version can be found in doc-*/doc/index.html:
  %% <title>Erlang/OTP ...</title>
  RegexStr = "[<]title[>]Erlang/OTP\\s+([^<]+)[<]/title[>]",
  {ok, Regex} = re:compile(RegexStr, [multiline]),

  Path = filename:join([DocBase ++ Ver, "doc", "index.html"]),
  case file:read_file(Path) of
    {ok, Content} ->
      case re:run(Content, Regex, [{capture, all_but_first, list}]) of
        {match, [Rel]} -> Rel;
        _ -> "??"
      end;
    _ -> "??"
  end.
