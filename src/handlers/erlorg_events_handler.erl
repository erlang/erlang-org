-module(erlorg_events_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {erlorg_base_handler,
         [
          init/3,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_get/2,
   rest_init/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% cowboy

rest_init(Req, [Opts]) ->
  {ok, Req, Opts};
rest_init(Req, []) ->
  {ok, Req, #{}}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  {Bindings, Req1} = erlorg_req:bindings_map(Req),
  Vars = process_req(maps:merge(State, Bindings)),
  Vars1 = printable_event_dates(Vars),
  Template = case maps:is_key(filters, Vars1) of
               true -> events_filter_dtl;
               false -> events_dtl
             end,
  {ok, Body} = Template:render(Vars1),
  {Body, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_req(map()) -> map().
process_req(#{filter := tag, value := Tag}) ->
  Events = erlorg_articles_repo:by_tag(event, Tag),
  Filters = erlorg_articles_repo:filters(event, tag),
  #{events => Events, filters => Filters};
process_req(#{filter := year, value := Year}) ->
  YearBin = binary_to_integer(Year),
  Events = erlorg_articles_repo:by_year(event, YearBin),
  Filters = erlorg_articles_repo:filters(event, year),
  #{events => Events, filters => Filters};
process_req(#{filter := user, value := User}) ->
  Events = erlorg_articles_repo:by_user(event, User),
  Filters = erlorg_articles_repo:filters(event, user),
  #{events => Events, filters => Filters};
process_req(#{filter := Filter}) ->
  #{filters => erlorg_articles_repo:filters(event, Filter)};
process_req(#{id := Id}) ->
  IdInt = binary_to_integer(Id),
  Event = erlorg_articles_repo:by_id(IdInt),
  #{event => Event};
process_req(_) ->
  Events = erlorg_articles_repo:list(event),
  #{events => Events}.

-spec printable_event_dates(map()) -> map().
printable_event_dates(#{events := Events} = Vars) ->
  Vars#{events => erlorg_datetime:printable_date(Events)};
printable_event_dates(#{event := Event} = Vars) ->
  Vars#{event => erlorg_datetime:printable_date(Event)};
printable_event_dates(Vars) ->
  Vars.
