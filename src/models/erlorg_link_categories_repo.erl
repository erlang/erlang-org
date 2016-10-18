%%% @doc Link Categories repository
-module(erlorg_link_categories_repo).
-author('juan@inaka.net').

-export([create/3, list/1]).

-type link_group() :: community | documentation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(binary(), integer(), integer()) ->
  erlorg_link_categories:link_category().
create(Name, GroupId, Order) ->
  LinkCategory = erlorg_link_categories:new(Name, GroupId, Order),
  sumo:persist(erlorg_link_categories, LinkCategory).

-spec list(link_group()) -> [erlorg_link_categories:link_category()].
list(LinkGroup) ->
  sumo:call(erlorg_link_categories, link_categories_with_links, [LinkGroup]).
