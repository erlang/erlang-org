%%% @doc Links model
-module(erlorg_links).
-author('juan@inaka.net').

-behaviour(sumo_doc).

-opaque 'link'() ::
          #{
             id            => integer(),
             title         => binary(),
             url           => binary(),
             description   => binary(),
             category_id   => integer(),
             created_at    => erlorg_datetime:datetime(),
             updated_at    => erlorg_datetime:datetime()
           }.
-export_type([link/0]).

-export([new/5]).
-export([title/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id,            integer,  [id, auto_increment, not_null])
    , sumo:new_field(title,         string,   [{length, 255}, not_null])
    , sumo:new_field(url,           string,   [{length, 255}, not_null])
    , sumo:new_field(description,   string,   [{length, 255}, not_null])
    , sumo:new_field(order,         integer,  [not_null])
    , sumo:new_field(category_id,   integer,  [not_null])
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ]).

-spec sumo_sleep(link()) -> sumo:doc().
sumo_sleep(Link) -> Link.

-spec sumo_wakeup(sumo:doc()) -> link().
sumo_wakeup(Doc) -> Doc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (binary(), binary(), binary(), integer(), integer()) -> link().
new(Title, Url, Description, Order, CategoryId) ->
  Now = erlorg_datetime:now(),
  #{ id            => undefined
   , title         => Title
   , url           => Url
   , description   => Description
   , order         => Order
   , category_id   => CategoryId
   , created_at    => Now
   , updated_at    => Now
   }.

-spec title(link()) -> binary().
title(#{title := Title}) -> Title.
