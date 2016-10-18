%%% @doc Releases repository
-module(erlorg_releases_repo).
-author('jfacorro@inakanetworks.com').

-export([create/6, list/0, list/1, by_version/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create (binary(), binary(), binary(), binary(),
              erlorg_datetime:datetime(),
              erlorg_releases:files()) -> erlorg_releases:release().
create(Title, Version, Note, Content, Date, Files) ->
  Release = erlorg_releases:new(Title, Version, Note, Content, Date, Files),
  sumo:persist(erlorg_releases, Release).

-spec list() -> [erlorg_releases:release()].
list() ->
  sumo:find_all(erlorg_releases, {date, desc}, 0, 0).

-spec list(integer()) -> [erlorg_releases:release()].
list(Limit) ->
  sumo:find_all(erlorg_releases, {date, desc}, Limit, 0).

-spec by_version(binary()) -> erlorg_releases:release().
by_version(Version) ->
  case sumo:find_by(erlorg_releases, {version, Version}, 1, 0) of
    [] -> not_found;
    [Release] -> Release
  end.
