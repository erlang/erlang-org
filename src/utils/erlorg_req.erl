-module(erlorg_req).

-export([bindings_map/1]).

-spec bindings_map(cowboy_req:req()) -> {map(), cowboy_req:req()}.
bindings_map(Req) ->
  {Bindings, Req1} = cowboy_req:bindings(Req),
  {maps:from_list(Bindings), Req1}.
