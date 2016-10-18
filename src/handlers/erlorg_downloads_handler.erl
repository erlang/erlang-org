-module(erlorg_downloads_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {erlorg_base_handler,
         [
          init/3,
          rest_init/2,
          content_types_provided/2,
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
  {Bindings, Req1} = erlorg_req:bindings_map(Req),
  Releases = erlorg_releases_repo:list(),
  Release = process_req(Bindings, Releases),
  DownloadsArticle = erlorg_articles_repo:fetch_by_type(downloads),
  Vars = maps:merge(#{releases => Releases,
                      article => DownloadsArticle},
                    Release),
  case Vars of
    #{release := not_found} ->
      {ok, Body} = '404_dtl':render([]),
      {ok, Req2} = cowboy_req:reply(404, [], Body, Req1),
      {halt, Req2, State};
    _ ->
      {ok, Body} = downloads_dtl:render(Vars),
      {Body, Req1, State}
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_req(map(), [erlorg_releases:release()]) -> map().
process_req(#{version := Version}, _) ->
  Release = erlorg_releases_repo:by_version(Version),
  #{release => Release};
process_req(_, Releases) ->
  case Releases of
    [] -> #{};
    [Latest | _] -> #{release => Latest}
  end.
