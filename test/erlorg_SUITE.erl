-module(erlorg_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-type config() :: proplists:proplist().

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export(
   [
    about/1,
    community/1,
    docs/1,
    versions/1,
    course/1,
    downloads/1,
    eep/1,
    eplicense/1,
    euc/1,
    events/1,
    getting_started_quickly/1,
    index/1,
    news/1,
    mailinglists/1,
    workshop/1,
    rss/1
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

  {ok, Conn} = shotgun:open("localhost", 8080),
  [{conn, Conn} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  shotgun:close(get_conn(Config)),

  erlorg:stop(),
  application:stop(shotgun).

-spec about(config()) -> {comment, string()}.
about(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body1}} = shotgun:get(Conn, "/about"),
  {match, _} = re:run(Body1, <<"<!-- Default -->">>),

  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle About">>),

  Now = erlorg_datetime:now(),
  {ok, AboutTypeId} = erlorg_test_utils:article_type_id(about),

  erlorg_articles_repo:create(AboutTypeId, <<"About">>,
                              <<"About Lead">>, <<"Twitter About">>,
                              <<"Content About">>, undefined, [],
                              UserId, true, Now),

  {ok, #{status_code := 200,
         body := Body2}} = shotgun:get(Conn, "/about"),
  false = is_empty(Body2),
  nomatch = re:run(Body2, <<"<!-- Default -->">>),

  {comment, ""}.

-spec community(config()) -> {comment, string()}.
community(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body1}} = shotgun:get(Conn, "/community"),
  false = is_empty(Body1),
  {match, _} = re:run(Body1, <<"<!-- Default -->">>),

  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle Community">>),

  Now = erlorg_datetime:now(),
  {ok, CommunityTypeId} = erlorg_test_utils:article_type_id(community),

  erlorg_articles_repo:create(CommunityTypeId, <<"Community">>,
                              <<"Community Lead">>, <<"Twitter Community">>,
                              <<"Content Community">>, undefined, [],
                              UserId, true, Now),

  {ok, #{status_code := 200,
         body := Body2}} = shotgun:get(Conn, "/community"),
  false = is_empty(Body2),
  nomatch = re:run(Body2, <<"<!-- Default -->">>),

  {comment, ""}.

-spec docs(config()) -> {comment, string()}.
docs(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/docs"),

  false = is_empty(Body),

  {comment, ""}.

-spec versions(config()) -> {comment, string()}.
versions(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/docs/versions"),

  false = is_empty(Body),

  {comment, ""}.

-spec course(config()) -> {comment, string()}.
course(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/course"),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/course/course"),

  false = is_empty(Body),

  {ok, #{status_code := 200,
         body := BodyExercises}} = shotgun:get(Conn, "/course/exercises"),
  false = is_empty(BodyExercises),

  {ok, #{status_code := 200,
         body := BodyHistory}} = shotgun:get(Conn, "/course/history"),
  false = is_empty(BodyHistory),

  {ok, #{status_code := 200,
         body := BodySeqProg}} = shotgun:get(Conn, "/course/sequential-programming"),
  false = is_empty(BodySeqProg),

  {ok, #{status_code := 200,
         body := BodyConcurrentProg}} = shotgun:get(Conn, "/course/concurrent-programming"),
  false = is_empty(BodyConcurrentProg),

  {ok, #{status_code := 200,
         body := BodyError}} = shotgun:get(Conn, "/course/error-handling"),
  false = is_empty(BodyError),

  {ok, #{status_code := 200,
         body := BodyAdvanced}} = shotgun:get(Conn, "/course/advanced"),
  false = is_empty(BodyAdvanced),

  {comment, ""}.

-spec downloads(config()) -> {comment, string()}.
downloads(Config) ->
  Conn = get_conn(Config),

  sumo:delete_all(erlorg_releases),

  ct:comment("Downloads no releases returns 200 OK"),
  {ok, #{status_code := 200,
         body := BodyNoReleases}} = shotgun:get(Conn, "/downloads"),
  false = is_empty(BodyNoReleases),

  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle Downloads">>),

  Now = erlorg_datetime:now(),
  {ok, DownloadsTypeId} = erlorg_test_utils:article_type_id(downloads),

  erlorg_articles_repo:create(DownloadsTypeId, <<"Downloads">>,
                              <<"Downloads Lead">>, <<"Twitter Downloads">>,
                              <<"Content Downloads">>, undefined, [],
                              UserId, true, Now),

  {ok, #{status_code := 200,
         body := BodyNoReleases2}} = shotgun:get(Conn, "/downloads"),
  false = is_empty(BodyNoReleases2),
  false = BodyNoReleases == BodyNoReleases2,

  ct:comment("Downloads with releases returns 200 OK"),
  erlorg_test_utils:create_release(<<"17.5">>),
  erlorg_test_utils:create_release(<<"17.4">>),
  erlorg_test_utils:create_release(<<"17.3">>),
  {ok, #{status_code := 200,
         body := BodyWithReleases}} = shotgun:get(Conn, "/downloads"),
  false = is_empty(BodyWithReleases),

  ct:comment("Release page returns 200 OK"),
  {ok, #{status_code := 200,
         body := BodyRelease}} = shotgun:get(Conn, "/downloads/17.4"),
  false = is_empty(BodyRelease),

  ct:comment("Release page returns 404 Not Found"),
  {ok, #{status_code := 404,
         body := BodyNotFound}} = shotgun:get(Conn, "/downloads/0.0"),
  false = is_empty(BodyNotFound),

  {comment, ""}.

-spec eep(config()) -> {comment, string()}.
eep(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/erlang-enhancement-proposals"),
  false = is_empty(Body),

  {ok, #{status_code := 200,
         body := BodyEEPS}} = shotgun:get(Conn, "/erlang-enhancement-proposals/home"),
  {ok, #{status_code := 200,
         body := BodyEEPS}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0000"),
  {ok, #{status_code := 200,
         body := BodyEEPS}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0000.html"),
  false = is_empty(BodyEEPS),

  {ok, #{status_code := 200,
         body := BodyEEPSMD}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0000.md"),
  false = is_empty(BodyEEPSMD),

  {ok, #{status_code := 200,
         body := BodyEEPS0001Img}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0001-1.png"),
  false = is_empty(BodyEEPS0001Img),

  {ok, #{status_code := 200,
         body := BodyEEPS0001}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0001"),
  {ok, #{status_code := 200,
         body := BodyEEPS0001}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-0001.html"),
  false = is_empty(BodyEEPS0001),

  {ok, #{status_code := 404}} = shotgun:get(Conn, "/erlang-enhancement-proposals/eep-XXXX.html"),

  {comment, ""}.

-spec eplicense(config()) -> {comment, string()}.
eplicense(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/EPLICENSE"),

  false = is_empty(Body),

  {comment, ""}.

-spec euc(config()) -> {comment, string()}.
euc(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 404}} =
    shotgun:get(Conn, "/community/erlang-user-conference"),

  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle EUC">>),

  Now = erlorg_datetime:now(),
  {ok, EucTypeId} = erlorg_test_utils:article_type_id(euc),
  _ = erlorg_articles_repo:create(EucTypeId, <<"EUC">>, <<"EUC Lead">>,
                                  <<"Twitter EUC">>, <<"Content EUC">>,
                                  undefined, [],
                                  UserId, true, Now),

  {ok, #{status_code := 200, body := Body}} =
    shotgun:get(Conn, "/community/erlang-user-conference"),

  false = is_empty(Body),

  {comment, ""}.

-spec events(config()) -> {comment, string()}.
events(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/events"),
  false = is_empty(Body),

  %% Tag filter

  {ok, #{status_code := 200,
         body := BodyTagFilter}} = shotgun:get(Conn, "/events/tag"),
  false = is_empty(BodyTagFilter),

  {ok, #{status_code := 200,
         body := BodyTagValue}} = shotgun:get(Conn, "/events/tag/whatever"),
  false = is_empty(BodyTagValue),

  %% User filter

  {ok, #{status_code := 200,
         body := BodyUserFilter}} = shotgun:get(Conn, "/events/user"),
  false = is_empty(BodyUserFilter),

  {ok, #{status_code := 200,
         body := BodyUserValue}} =
    shotgun:get(Conn, "/events/user/" ++ http_uri:encode("John Doe")),
  false = is_empty(BodyUserValue),

  %% Year filter

  {ok, #{status_code := 200,
         body := BodyYearFilter}} = shotgun:get(Conn, "/events/year"),
  false = is_empty(BodyYearFilter),

  {ok, #{status_code := 200,
         body := BodyYearValue}} = shotgun:get(Conn, "/events/year/2015"),
  false = is_empty(BodyYearValue),

  %% Event by id
  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle 1">>),

  Now = erlorg_datetime:now(),
  {ok, EventTypeId} = erlorg_test_utils:article_type_id(event),
  E1 = erlorg_articles_repo:create(EventTypeId, <<"Event 1">>, <<"Lead 1">>,
                                   <<"Twitter 1">>, <<"Content 1">>,
                                   <<"Attachment 1">>, [<<"Tag 1">>],
                                   UserId, true, Now),

  EventId = erlorg_articles:id(E1),

  {ok, #{status_code := 200,
         body := BodyById}} =
    shotgun:get(Conn, "/events/" ++ integer_to_list(EventId)),
  false = is_empty(BodyById),

  {comment, ""}.

-spec getting_started_quickly(config()) -> {comment, string()}.
getting_started_quickly(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/static/getting_started_quickly"),

  false = is_empty(Body),

  {comment, ""}.

-spec index(config()) -> {comment, string()}.
index(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/"),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/index.html"),

  false = is_empty(Body),

  {comment, ""}.

-spec news(config()) -> {comment, string()}.
news(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/news"),
  false = is_empty(Body),

  %% Tag filter

  {ok, #{status_code := 200,
         body := BodyTagFilter}} = shotgun:get(Conn, "/news/tag"),
  false = is_empty(BodyTagFilter),

  {ok, #{status_code := 200,
         body := BodyTagValue}} = shotgun:get(Conn, "/news/tag/whatever"),
  false = is_empty(BodyTagValue),

  %% User filter

  {ok, #{status_code := 200,
         body := BodyUserFilter}} = shotgun:get(Conn, "/news/user"),
  false = is_empty(BodyUserFilter),

  {ok, #{status_code := 200,
         body := BodyUserValue}} =
    shotgun:get(Conn, "/news/user/" ++ http_uri:encode("John Doe")),
  false = is_empty(BodyUserValue),

  %% Year filter

  {ok, #{status_code := 200,
         body := BodyYearFilter}} = shotgun:get(Conn, "/news/year"),
  false = is_empty(BodyYearFilter),

  {ok, #{status_code := 200,
         body := BodyYearValue}} = shotgun:get(Conn, "/news/year/2015"),
  false = is_empty(BodyYearValue),

  %% Event by id

  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle 2">>),

  Now = erlorg_datetime:now(),
  {ok, NewsTypeId} = erlorg_test_utils:article_type_id(news),
  N1 = erlorg_articles_repo:create(NewsTypeId, <<"Title 1">>, <<"Lead 1">>,
                                   <<"Twitter 1">>, <<"Content 1">>,
                                   <<"Attachment 1">>, [<<"Tag 1">>],
                                   UserId, true, Now),

  NewsId = erlorg_articles:id(N1),

  {ok, #{status_code := 200,
         body := BodyById}} =
    shotgun:get(Conn, "/news/" ++ integer_to_list(NewsId)),
  false = is_empty(BodyById),

  {comment, ""}.

-spec mailinglists(config()) -> {comment, string()}.
mailinglists(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/community/mailinglists"),

  false = is_empty(Body),

  {comment, ""}.

-spec workshop(config()) -> {comment, string()}.
workshop(Config) ->
  Conn = get_conn(Config),

  {ok, #{status_code := 404}} = shotgun:get(Conn, "/community/workshops"),

  %% Event by id
  erlorg_test_utils:delete_all_users(),
  {ok, UserId} = erlorg_test_utils:create_user(<<"John Bitdiddle Workshop">>),

  Now = erlorg_datetime:now(),
  {ok, WorkshopTypeId} = erlorg_test_utils:article_type_id(workshop),
  erlorg_articles_repo:create(WorkshopTypeId, <<"Workshop">>,
                              <<"Workshop Lead">>, <<"Twitter Workshop">>,
                              <<"Content Workshop">>, undefined, [],
                              UserId, true, Now),

  {ok, #{status_code := 200,
         body := Body}} = shotgun:get(Conn, "/community/workshops"),

  false = is_empty(Body),

  {comment, ""}.

-spec rss(config()) -> {comment, string()}.
rss(Config) ->
  Conn = get_conn(Config),

  erlorg_test_utils:delete_all_users(),
  sumo:delete_all(erlorg_releases),
  sumo:delete_all(erlorg_articles),

  {ok, UserId} = erlorg_test_utils:create_user(<<"RSS User">>),

  Date1 = {{2014, 12, 29}, {10, 0, 0}},
  Date2 = {{2014, 12, 30}, {10, 0, 0}},
  Date3 = {{2014, 12, 31}, {10, 0, 0}},
  Date4 = {{2015, 1, 1}, {10, 0, 0}},
  Date5 = {{2015, 1, 2}, {10, 0, 0}},
  Date6 = {{2015, 1, 3}, {10, 0, 0}},
  Date7 = {{2015, 1, 4}, {10, 0, 0}},
  Date8 = {{2015, 1, 5}, {10, 0, 0}},
  Date9 = {{2015, 1, 6}, {10, 0, 0}},
  Date10 = {{2015, 1, 7}, {10, 0, 0}},

  % Now = erlorg_datetime:now(),
  {ok, NewsTypeId} = erlorg_test_utils:article_type_id(news),
  {ok, EventTypeId} = erlorg_test_utils:article_type_id(event),

  erlorg_articles_repo:create(EventTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date10),

  erlorg_test_utils:create_release(<<"17.6">>, Date9),

  erlorg_articles_repo:create(NewsTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date8),

  erlorg_test_utils:create_release(<<"17.7">>, Date7),

  erlorg_articles_repo:create(EventTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date6),

  erlorg_test_utils:create_release(<<"17.8">>, Date5),

  erlorg_test_utils:create_release(<<"17.9">>, Date4, true),

  erlorg_articles_repo:create(NewsTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date3),

  erlorg_articles_repo:create(NewsTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date2),

  erlorg_articles_repo:create(EventTypeId, <<"Title">>, <<"Lead">>,
                                   <<"Twitter">>, <<"Content">>,
                                   <<"Attachment">>, [<<"Tag">>],
                                   UserId, true, Date1),

  {ok, #{status_code := 200, headers := NewsHeader,
         body := NewsBody}} = shotgun:get(Conn, "/rss/news"),
  <<"text/xml">> = proplists:get_value(<<"content-type">>, NewsHeader),
  false = is_empty(NewsBody),

  {ok, #{status_code := 200, headers := EventHeader,
         body := EventBody}} = shotgun:get(Conn, "/rss/events"),
  <<"text/xml">> = proplists:get_value(<<"content-type">>, EventHeader),
  false = is_empty(EventBody),

  {ok, #{status_code := 200, headers := DownloadsHeader,
         body := DownloadsBody}} = shotgun:get(Conn, "/rss/downloads"),
  <<"text/xml">> = proplists:get_value(<<"content-type">>, DownloadsHeader),
  false = is_empty(DownloadsBody),

  {ok, #{status_code := 200, headers := OverallHeader,
         body := OverallBody}} = shotgun:get(Conn, "/rss/"),
  <<"text/xml">> = proplists:get_value(<<"content-type">>, OverallHeader),
  false = is_empty(OverallBody),

  {ParsResult, _} = xmerl_scan:string(erlang:binary_to_list(OverallBody)),

  SimplifyResult = xmerl_lib:simplify_element(ParsResult),

  {_, _, [_, {_, _, Elements}, _]} = SimplifyResult,

  Items = [I || {item, _, I} <- Elements],

  CategoryFun =
    fun(Item) ->
      {_, _, [C]} = lists:keyfind(category, 1, Item),
      C
    end,

  [ "Event", "Downloads", "News", "Downloads", "Event",
    "Downloads", "Downloads", "News", "News", "Event"
  ] = [CategoryFun(Item) || Item <- Items],

  {comment, ""}.

-spec get_conn(config()) -> pid().
get_conn(Config) ->
  proplists:get_value(conn, Config).

-spec is_empty(binary()) -> boolean().
is_empty(Body) ->
  <<>> == Body.
