-module(erlorg_rss_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {erlorg_base_handler,
         [
          init/3,
          resource_exists/2
         ]}
       ]).

-export(
  [
   content_types_provided/2,
   rest_init/2,
   allowed_methods/2,
   handle_get/2
  ]).

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"xml">>, []}, handle_get}], Req, State}.

rest_init(Req, []) ->
  {ok, Req, #{}}.

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

handle_get(Req, State) ->
  {Bindings, Req1} = bindings_map(Req),
  #{dtl := Dtl, content := Vars} = process_req(Bindings),
  {ok, Body} = Dtl:render(Vars),
  {Body, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_req(map()) -> map().
process_req(#{value := <<"news">>}) ->
  {ok, Limit} = application:get_env(erlorg, rss_news_limit),
  News = erlorg_articles_repo:list(news, Limit),
  PDateNews = erlorg_datetime:full_printable_date(News, article),
  PNews = erlorg_articles:article_type(<<"News">>, PDateNews),
  #{dtl => rss_articles_dtl, content => #{values => PNews}};
process_req(#{value := <<"downloads">>}) ->
  {ok, Limit} = application:get_env(erlorg, rss_download_limit),
  Releases = erlorg_releases_repo:list(Limit),
  PReleases = erlorg_datetime:full_printable_date(Releases, release),
  #{dtl => rss_downloads_dtl, content => #{values => PReleases}};
process_req(#{value := <<"events">>}) ->
  {ok, Limit} = application:get_env(erlorg, rss_event_limit),
  Events = erlorg_articles_repo:list(event, Limit),
  PDateEvents = erlorg_datetime:full_printable_date(Events, article),
  PEvents = erlorg_articles:article_type(<<"Event">>, PDateEvents),
  #{dtl => rss_articles_dtl, content => #{values => PEvents}};
process_req(#{}) ->
  {ok, Limit} = application:get_env(erlorg, rss_overall_limit),
  News = erlorg_articles_repo:list(news, Limit),
  PNews = erlorg_articles:article_type(<<"News">>, News),
  Events = erlorg_articles_repo:list(event, Limit),
  PEvents = erlorg_articles:article_type(<<"Event">>, Events),
  Releases = erlorg_releases_repo:list(Limit),
  Sorted = lists:sort(fun cmp/2, PNews ++ PEvents ++ Releases),
  Values = lists:map(fun to_print/1, lists:sublist(Sorted, Limit)),
  #{dtl => rss_dtl, content => #{values => Values}}.

to_print(Value) ->
  case Value of
    #{date := _} -> erlorg_datetime:full_printable_date(Value, release);
    #{article_date := _} -> erlorg_datetime:full_printable_date(Value, article)
  end.

cmp(#{date := E1}, #{date := E2}) ->
  E1 > E2;
cmp(#{article_date := E1}, #{date := E2}) ->
  E1 > E2;
cmp(#{article_date := E1}, #{article_date := E2}) ->
  E1 > E2.

-spec bindings_map(cowboy_req:req()) -> {map(), cowboy_req:req()}.
bindings_map(Req) ->
  {Bindings, Req1} = cowboy_req:bindings(Req),
  {maps:from_list(Bindings), Req1}.