-module(erlorg_news_handler).

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
  {Bindings, Req1} = bindings_map(Req),
  Vars = process_req(maps:merge(State, Bindings)),
  {ok, Body} =
    case printable_news_dates(Vars) of
      #{filters := _Filter} = Vars1 -> news_filter_dtl:render(Vars1);
      Vars1 -> news_dtl:render(Vars1)
    end,
  {Body, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_req(map()) -> map().
process_req(#{filter := tag, value := Tag}) ->
  News = erlorg_articles_repo:by_tag(news, Tag),
  Filters = erlorg_articles_repo:filters(news, tag),
  #{news => News, filters => Filters};
process_req(#{filter := user, value := User}) ->
  News = erlorg_articles_repo:by_user(news, User),
  Filters = erlorg_articles_repo:filters(new, user),
  #{news => News, filters => Filters};
process_req(#{filter := Filter}) ->
  #{filters => erlorg_articles_repo:filters(news, Filter)};
process_req(#{id := Id}) ->
  IdInt = binary_to_integer(Id),
  News = erlorg_articles_repo:by_id(IdInt),
  #{current_news => News};
process_req(_) ->
  News = erlorg_articles_repo:list(news),
  #{news => News}.

-spec bindings_map(cowboy_req:req()) -> {map(), cowboy_req:req()}.
bindings_map(Req) ->
  {Bindings, Req1} = cowboy_req:bindings(Req),
  {maps:from_list(Bindings), Req1}.

-spec printable_news_dates(map()) -> map().
printable_news_dates(#{news := News} = Vars) ->
  Vars#{news => erlorg_datetime:printable_date(News)};
printable_news_dates(#{current_news := CurrentNews} = Vars) ->
  Vars#{current_news => erlorg_datetime:printable_date(CurrentNews)};
printable_news_dates(Vars) ->
  Vars.
