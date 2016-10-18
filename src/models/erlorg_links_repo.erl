%%% @doc Links repository
-module(erlorg_links_repo).
-author('juan@inaka.net').

-export([create/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(binary(), binary(), binary(), integer(), integer()) ->
  erlorg_links:link().
create(Title, Url, Description, Order, CategoryId) ->
  Link = erlorg_links:new(Title, Url, Description, Order, CategoryId),
  sumo:persist(erlorg_links, Link).
