-module(erlorg_models_SUITE).
-include_lib("common_test/include/ct.hrl").

-type config() :: proplists:proplist().

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export(
   [
    events/1,
    link_categories/1,
    links/1,
    mailinglists/1,
    news/1,
    releases/1
   ]
  ).

-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> ok.
init_per_suite(Config) ->
  erlorg:start(),
  application:ensure_all_started(shotgun),
  sumo:create_schema(),
  sumo:delete_all(erlorg_articles),
  ok = erlorg_test_utils:delete_all_users(),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  erlorg:stop(),
  application:stop(shotgun).

-spec events(config()) -> {comment, string()}.
events(_Config) ->
  {ok, EventTypeId} = erlorg_test_utils:article_type_id(event),
  sumo:delete_by(erlorg_articles, {article_type_id, EventTypeId}),

  Date1 = {{2015, 5, 1}, {12, 0, 0}},
  Date2 = {{2015, 12, 1}, {12, 0, 0}},
  Date3 = {{2014, 5, 1}, {12, 0, 0}},

  {ok, UserId1} = erlorg_test_utils:create_user(<<"John Bitdiddle Event">>),
  {ok, UserId2} = erlorg_test_utils:create_user(<<"Alice P. Hacker Event">>),

  E1 = erlorg_articles_repo:create(EventTypeId, <<"Event 1">>, <<"Lead 1">>,
                                   <<"Twitter 1">>,
                                   <<"Content 1">>, <<"Attachment 1">>,
                                   [<<"Tag 1">>],
                                   UserId1, true, Date1),

  _ = erlorg_articles_repo:create(EventTypeId, <<"Event 1">>, <<"Lead 1">>,
                                  <<"Twitter 1">>,
                                  <<"Content 1">>, <<"Attachment 1">>,
                                  [<<"Tag 1">>],
                                  UserId1, false, Date1),

  E2 = erlorg_articles_repo:create(EventTypeId, <<"Event 2">>, <<"Lead 2">>,
                                   <<"Twitter 2">>,
                                   <<"Content 2">>, <<"Attachment 2">>,
                                   [<<"Tag 2">>],
                                   UserId2, true, Date2),

  E3 = erlorg_articles_repo:create(EventTypeId, <<"Event 3">>, <<"Lead 3">>,
                                   <<"Twitter 3">>,
                                   <<"Content 3">>, <<"Attachment 3">>,
                                   [<<"Tag 1">>, <<"Tag 2">>,  <<"Tag 3">>],
                                   UserId2, true, Date3),

  ct:comment("Events were stored in the DB"),
  [_, _, _] = erlorg_articles_repo:list(event),

  ct:comment("Filter events by tag"),
  [E1, E3] = erlorg_articles_repo:by_tag(event, <<"Tag 1">>),
  [E2, E3] = erlorg_articles_repo:by_tag(event, <<"Tag 2">>),
  [E3] = erlorg_articles_repo:by_tag(event, <<"Tag 3">>),

  ct:comment("Filter events by author"),
  [E1] = erlorg_articles_repo:by_user(event, <<"John Bitdiddle Event">>),
  [E2, E3] = erlorg_articles_repo:by_user(event, <<"Alice P. Hacker Event">>),

  ct:comment("Filter events by year"),
  [E2, E1] = erlorg_articles_repo:by_year(event, 2015),
  [E3] = erlorg_articles_repo:by_year(event, 2014),

  ct:comment("Get event by id"),
  E1 = erlorg_articles_repo:by_id(erlorg_articles:id(E1)),
  E2 = erlorg_articles_repo:by_id(erlorg_articles:id(E2)),
  E3 = erlorg_articles_repo:by_id(erlorg_articles:id(E3)),
  not_found = erlorg_articles_repo:by_id(-1),

  [#{value := <<"Tag 1">>, count := 2},
   #{value := <<"Tag 2">>, count := 2},
   #{value := <<"Tag 3">>, count := 1}] =
    erlorg_articles_repo:filters(event, tag),

  [#{value := 2015, count := 2},
   #{value := 2014, count := 1}] = erlorg_articles_repo:filters(event, year),

  [#{value := <<"Alice P. Hacker Event">>,
     count := 2},
   #{value := <<"John Bitdiddle Event">>,
     count := 1}] = erlorg_articles_repo:filters(event, user),

  {comment, ""}.

-spec link_categories(config()) -> {comment, string()}.
link_categories(_Config) ->
  sumo:delete_all(erlorg_links),
  sumo:delete_all(erlorg_link_categories),

  {ok, CommunityId} = erlorg_test_utils:link_group_id(community),
  {ok, DocsId} = erlorg_test_utils:link_group_id(documentation),

  CodeRepos =
    erlorg_link_categories_repo:create(<<"Code Repositories">>, CommunityId, 2),
  CodeReposId = erlorg_link_categories:id(CodeRepos),

  NewsSites =
    erlorg_link_categories_repo:create(<<"News Sites">>, CommunityId, 1),
  NewsSitesId = erlorg_link_categories:id(NewsSites),

  Conferences =
    erlorg_link_categories_repo:create(<<"Conferences">>, CommunityId, 3),
  ConferencesId = erlorg_link_categories:id(Conferences),

  erlorg_link_categories_repo:create(<<"Documentation">>, DocsId, 1),

  ct:comment("Categories were stored in the DB"),

  [Category1, Category2, Category3] = erlorg_link_categories_repo:list(community),

  ct:comment("Categories are in the right order"),
  NewsSitesId = erlorg_link_categories:id(Category1),
  CodeReposId = erlorg_link_categories:id(Category2),
  ConferencesId = erlorg_link_categories:id(Category3),

  <<"News Sites">> = erlorg_link_categories:name(Category1),
  <<"Code Repositories">> = erlorg_link_categories:name(Category2),
  <<"Conferences">> = erlorg_link_categories:name(Category3),

  1 = erlorg_link_categories:order(Category1),
  2 = erlorg_link_categories:order(Category2),
  3 = erlorg_link_categories:order(Category3),

  {comment, ""}.

-spec links(config()) -> {comment, string()}.
links(_Config) ->
  sumo:delete_all(erlorg_links),
  sumo:delete_all(erlorg_link_categories),

  {ok, CommunityId} = erlorg_test_utils:link_group_id(community),

  NewsSites =
    erlorg_link_categories_repo:create(<<"News Sites">>, CommunityId, 1),
  NewsSitesId = erlorg_link_categories:id(NewsSites),
  CodeRepos =
    erlorg_link_categories_repo:create(<<"Code Repositories">>, CommunityId, 2),
  CodeReposId = erlorg_link_categories:id(CodeRepos),
  Conferences =
    erlorg_link_categories_repo:create(<<"Conferences">>, CommunityId, 3),
  ConferencesId = erlorg_link_categories:id(Conferences),

  erlorg_links_repo:create(<<"Link 1">>, <<"Url 1">>,
                           <<"Description 1">>, 1, NewsSitesId),
  erlorg_links_repo:create(<<"Link 2">>, <<"Url 2">>,
                           <<"Description 2">>, 2, NewsSitesId),

  erlorg_links_repo:create(<<"Link 3">>, <<"Url 3">>,
                           <<"Description 3">>, 2, CodeReposId),
  erlorg_links_repo:create(<<"Link 4">>, <<"Url 4">>,
                           <<"Description 4">>, 1, CodeReposId),

  erlorg_links_repo:create(<<"Link 5">>, <<"Url 5">>,
                           <<"Description 5">>, 1, ConferencesId),

  ct:comment("Links were stored in the DB"),
  Links = sumo:find_all(erlorg_links),
  5 = length(Links),

  ct:comment("Links are in the right order"),
  [Category1, Category2, Category3] = erlorg_link_categories_repo:list(community),

  [L1, L2] = erlorg_link_categories:links(Category1),
  [L4, L3] = erlorg_link_categories:links(Category2),
  [L5] = erlorg_link_categories:links(Category3),

  <<"Link 1">> = erlorg_links:title(L1),
  <<"Link 2">> = erlorg_links:title(L2),
  <<"Link 4">> = erlorg_links:title(L4),
  <<"Link 3">> = erlorg_links:title(L3),
  <<"Link 5">> = erlorg_links:title(L5),

  {comment, ""}.


-spec mailinglists(config()) -> {comment, string()}.
mailinglists(_Config) ->
  try
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun request_mock/4),

    erlorg_mailinglists ! timeout,
    timer:sleep(1000),
    Result = erlorg_mailinglists:list("erlang-questions", 10),
    10 = length(Result)
  after
    meck:unload(httpc)
  end,
  {comment, ""}.

-spec news(config()) -> {comment, string()}.
news(_Config) ->
  {ok, NewsTypeId} = erlorg_test_utils:article_type_id(news),
  sumo:delete_by(erlorg_articles, {article_type_id, NewsTypeId}),

  Date1 = {{2015, 5, 1}, {12, 0, 0}},
  Date2 = {{2015, 12, 1}, {12, 0, 0}},
  Date3 = {{2014, 5, 1}, {12, 0, 0}},

  {ok, UserId1} = erlorg_test_utils:create_user(<<"John Bitdiddle News">>),
  {ok, UserId2} = erlorg_test_utils:create_user(<<"Alice P. Hacker News">>),

  N1 = erlorg_articles_repo:create(NewsTypeId, <<"Title 1">>, <<"Lead 1">>,
                                   <<"Twitter 1">>,
                                   <<"Content 1">>, <<"Attachment 1">>,
                                   [<<"Tag 1">>], UserId1, true, Date1),

  _ = erlorg_articles_repo:create(NewsTypeId, <<"Title 1">>, <<"Lead 1">>,
                                  <<"Twitter 1">>, <<"Content 1">>,
                                  <<"Attachment 1">>, [<<"Tag 1">>],
                                  UserId1, false, Date1),

  N2 = erlorg_articles_repo:create(NewsTypeId, <<"Title 2">>, <<"Lead 2">>,
                                   <<"Twitter 2">>, <<"Content 2">>,
                                   <<"Attachment 2">>, [<<"Tag 2">>],
                                   UserId2, true, Date2),

  N3 = erlorg_articles_repo:create(NewsTypeId, <<"Title 3">>, <<"Lead 3">>,
                                   <<"Twitter 3">>, <<"Content 3">>,
                                   <<"Attachment 3">>,
                                   [<<"Tag 1">>, <<"Tag 2">>, <<"Tag 3">>],
                                   UserId2, true, Date3),

  ct:comment("News were stored in the DB"),
  [N2, N1, N3] = erlorg_articles_repo:list(news),

  ct:comment("Filter news by tag"),
  [N1, N3] = erlorg_articles_repo:by_tag(news, <<"Tag 1">>),
  [N2, N3] = erlorg_articles_repo:by_tag(news, <<"Tag 2">>),
  [N3] = erlorg_articles_repo:by_tag(news, <<"Tag 3">>),

  ct:comment("Filter news by author"),
  [N1] = erlorg_articles_repo:by_user(news, <<"John Bitdiddle News">>),
  [N2, N3] = erlorg_articles_repo:by_user(news, <<"Alice P. Hacker News">>),

  ct:comment("Filter events by year"),
  [N2, N1] = erlorg_articles_repo:by_year(news, 2015),
  [N3] = erlorg_articles_repo:by_year(news, 2014),

  ct:comment("Get news by id"),
  N1 = erlorg_articles_repo:by_id(erlorg_articles:id(N1)),
  N2 = erlorg_articles_repo:by_id(erlorg_articles:id(N2)),
  N3 = erlorg_articles_repo:by_id(erlorg_articles:id(N3)),
  not_found = erlorg_articles_repo:by_id(-1),

  [#{value := <<"Tag 1">>, count := 2},
   #{value := <<"Tag 2">>, count := 2},
   #{value := <<"Tag 3">>, count := 1}] =
    erlorg_articles_repo:filters(news, tag),

  [#{value := <<"Alice P. Hacker News">>,
     count := 2},
   #{value := <<"John Bitdiddle News">>,
     count := 1}] = erlorg_articles_repo:filters(news, user),

  ct:comment("Create news with not tags"),
  _N4 = erlorg_articles_repo:create(NewsTypeId, <<"Title 3">>, <<"Lead 3">>,
                                    <<"Twitter 3">>, <<"Content 3">>,
                                    <<"Attachment 3">>, [],
                                    UserId2, true, Date3),

  {comment, ""}.

-spec releases(config()) -> {comment, string()}.
releases(_Config) ->
  sumo:delete_all(erlorg_releases),

  Date1 = {{2014, 6, 1}, {10, 0, 0}},
  Date2 = {{2014, 12, 1}, {10, 0, 0}},
  Date3 = {{2015, 6, 1}, {10, 0, 0}},
  Date4 = {{2015, 12, 1}, {10, 0, 0}},

  erlorg_test_utils:create_release(<<"17.3">>, Date1),
  erlorg_test_utils:create_release(<<"17.4">>, Date2),
  erlorg_test_utils:create_release(<<"17.5">>, Date3, true),

  [Release175, Release174, Release173] = erlorg_releases_repo:list(),

  <<"17.5">> = erlorg_releases:version(Release175),
  <<"17.4">> = erlorg_releases:version(Release174),
  <<"17.3">> = erlorg_releases:version(Release173),

  NewRelease173 = erlorg_test_utils:update_release_updateat(Date4, Release173),

  Date4 = erlorg_releases:updated_at(NewRelease173),

  {comment, ""}.

-spec request_mock(atom(), string(), any(), any()) -> string().
request_mock(get, {Url, []}, _, _) ->
  Page = case string:str(Url, "index.html") of
           0 -> undefined;
           _ -> index
         end,
  Page1 = case string:str(Url, "2015-June") of
            0 -> Page;
            _ -> june
          end,
  Page2 = case string:str(Url, "2015-May") of
            0 -> Page1;
            _ -> may
          end,

  File = case Page2 of
           index -> "index.html";
           june -> "2015-June.html";
           _ -> "2015-May.html"
         end,

  Path = "../../test/mailinglists/" ++ File,
  {ok, Content} = file:read_file(Path),
  {ok, {undefined, undefined, binary_to_list(Content)}}.
