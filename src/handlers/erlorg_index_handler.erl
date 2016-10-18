-module(erlorg_index_handler).

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
  News = erlorg_articles_repo:list(news, 3),
  Events = erlorg_articles_repo:list(event, 1),
  %MailingList = erlorg_mailinglists:list("erlang-questions", 5),
  HeaderArticle = erlorg_articles_repo:fetch_by_type('home header'),
  GettingStartedArticle =
    erlorg_articles_repo:fetch_by_type('home getting started'),
  FooterArticle = erlorg_articles_repo:fetch_by_type('home footer'),

  Vars = #{index => true,
           news => erlorg_datetime:printable_date(News),
           events => erlorg_datetime:printable_date(Events),
           %mailinglist => MailingList,
           header => HeaderArticle,
           getting_started => GettingStartedArticle,
           footer => FooterArticle},
  {ok, Body} = index_dtl:render(Vars),
  {Body, Req, State}.
