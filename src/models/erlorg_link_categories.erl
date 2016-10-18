%%% @doc Link Categories model
-module(erlorg_link_categories).
-author('juan@inaka.net').

-behaviour(sumo_doc).

-opaque link_category() ::
          #{
             id            => integer(),
             name          => binary(),
             group_id      => integer(),
             order         => integer(),
             created_at    => erlorg_datetime:datetime(),
             updated_at    => erlorg_datetime:datetime()
           }.
-export_type([link_category/0]).

-export([new/3]).
-export([id/1, name/1, order/1, links/1]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id,            integer,  [id, auto_increment, not_null])
    , sumo:new_field(name,          string,   [{length, 255}, not_null])
    , sumo:new_field(group_id,      integer,  [not_null])
    , sumo:new_field(order,         integer,  [not_null])
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ]).

-spec sumo_sleep(link_category()) -> sumo:doc().
sumo_sleep(LinkCategory) -> LinkCategory.

-spec sumo_wakeup(sumo:doc()) -> link_category().
sumo_wakeup(Doc) -> Doc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (binary(), integer(), integer()) -> link_category().
new(Name, GroupId, Order) ->
  Now = erlorg_datetime:now(),
  #{ id         => undefined
   , name       => Name
   , group_id   => GroupId
   , order      => Order
   , created_at => Now
   , updated_at => Now
   }.

-spec id(link_category()) -> integer().
id(#{id := Id}) -> Id.

-spec name(link_category()) -> binary().
name(#{name := Name}) -> Name.

-spec order(link_category()) -> integer().
order(#{order := Order}) -> Order.

-spec links(link_category()) -> [erlorg_links:link()].
links(#{links := Links}) -> Links.
