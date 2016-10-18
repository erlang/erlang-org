-module(erlorg).
-export([start/0, start/2, stop/0, stop/1]).

%% application
%% @doc Starts the application
start() ->
  {ok, _Started} = application:ensure_all_started(erlorg).

%% @doc Stops the application
stop() ->
  application:stop(erlorg).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  Cwd = file:get_cwd(),
  lager:info("CWD ~p", [Cwd]),
  {ok, Pid} = erlorg_sup:start_link(),
  start_listeners(),
  {ok, Pid}.

%% @private
stop(_State) ->
  cowboy:stop_listener(erlorg_http),
  ok.

start_listeners() ->
  {ok, Port} = application:get_env(erlorg, http_port),
  {ok, ListenerCount} = application:get_env(erlorg, http_listener_count),

  Dispatch =
    cowboy_router:compile(
      [{'_',
        [
         %% Home
         {<<"/">>, erlorg_index_handler, []},
         {<<"/index.html">>, erlorg_index_handler, []},
         %% Other
         {<<"/about">>,
          erlorg_plain_handler,
          #{dtl => about_dtl,
            type => about,
            not_found => false}},
         {<<"/community">>, erlorg_community_handler, []},
         {<<"/community/mailinglists">>,
          erlorg_plain_handler,
          #{dtl => mailinglists_dtl}},
         {<<"/community/workshops">>,
          erlorg_plain_handler,
          #{dtl => article_dtl,
            type => workshop}},
         {<<"/docs">>, erlorg_docs_handler, []},
         {<<"/docs/versions">>, erlorg_versions_handler, []},
         {<<"/course/">>, erlorg_plain_handler, #{dtl => course_dtl}},
         {<<"/course/course">>, erlorg_plain_handler, #{dtl => course_dtl}},
         {<<"/course/exercises">>,
          erlorg_plain_handler,
          #{dtl => exercises_dtl}},
         {<<"/course/history">>, erlorg_plain_handler, #{dtl => history_dtl}},
         {<<"/course/sequential-programming">>,
          erlorg_plain_handler,
          #{dtl => sequential_prog_dtl}},
         {<<"/course/concurrent-programming">>,
          erlorg_plain_handler,
          #{dtl => concurrent_prog_dtl}},
         {<<"/course/error-handling">>,
          erlorg_plain_handler,
          #{dtl => error_handling_dtl}},
         {<<"/course/advanced">>, erlorg_plain_handler, #{dtl => advanced_dtl}},
         {<<"/EPLICENSE">>,
          erlorg_plain_handler,
          #{dtl => eplicense_dtl,
            format => text}},
         {<<"/downloads">>, erlorg_downloads_handler, []},
         {<<"/downloads/[:version]">>, erlorg_downloads_handler, []},
         {<<"/erlang-enhancement-proposals">>,
          erlorg_plain_handler,
          #{dtl => eep_dtl}},
         {<<"/erlang-enhancement-proposals/:eep">>, erlorg_eeps_handler, []},
         {<<"/community/erlang-user-conference">>,
          erlorg_plain_handler,
          #{dtl => article_dtl,
            type => euc}},
         {<<"/events">>, erlorg_events_handler, []},
         {<<"/events/tag/[:value]">>,
          erlorg_events_handler,
          [#{filter => tag}]},
         {<<"/events/user/[:value]">>,
          erlorg_events_handler,
          [#{filter => user}]},
         {<<"/events/year/[:value]">>,
          erlorg_events_handler,
          [#{filter => year}]},
         {<<"/events/:id">>, erlorg_events_handler, []},
         {<<"/static/getting_started_quickly">>,
          erlorg_plain_handler,
          #{dtl => getting_started_quickly_dtl}},
         {<<"/news">>, erlorg_news_handler, []},
         {<<"/news/year/[:value]">>, erlorg_news_handler, []},
         {<<"/news/tag/[:value]">>, erlorg_news_handler, [#{filter => tag}]},
         {<<"/news/user/[:value]">>, erlorg_news_handler, [#{filter => user}]},
         {<<"/news/:id">>, erlorg_news_handler, []},
         {<<"/rss/[:value]">>, erlorg_rss_handler, []},
         %% Assets
         {"/favicon.ico", cowboy_static, {file, "assets/img/favicon.ico"}},
         {"/img/[...]", cowboy_static, {dir, "assets/img"}},
         {"/js/[...]", cowboy_static, {dir, "assets/js"}},
         {"/css/[...]", cowboy_static, {dir, "assets/css"}}
        ]
       }
      ]),

  RanchOptions =
    [
     {port, Port}
    ],
  CowboyOptions =
    [
     {env,
      [
       {dispatch, Dispatch}
      ]},
     {compress, true},
     {timeout, 12000}
    ],

  cowboy:start_http(erlorg_http, ListenerCount, RanchOptions, CowboyOptions).
