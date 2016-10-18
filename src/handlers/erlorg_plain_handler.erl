-module(erlorg_plain_handler).

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
   rest_init/2,
   allowed_methods/2,
   content_types_provided/2,
   handle_get/2
  ]).

%% @doc
%% dtl: Template to use.
%% type: Article type to dislplay
%% not_found: If true return 404 when article doesn't exist.
-type options() ::#{dtl => atom(),
                    type => erlorg_articles:type(),
                    not_found => boolean()}.

-type state() :: #{vars => map(),
                   dtl => atom(),
                   type => erlorg_articles:type(),
                   not_found => boolean()}.

%% cowboy
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Default = #{vars => #{}},
  {ok, Req, maps:merge(Default, Opts)}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, #{format := text} = State) ->
  ContentType = {<<"text">>, <<"plain">>, []},
  {[{ContentType, handle_get}], Req, State};
content_types_provided(Req, State) ->
  ContentType = {<<"text">>, <<"html">>, []},
  {[{ContentType, handle_get}], Req, State}.

%% internal
handle_get(Req, #{dtl := Dtl, vars := Vars, type := Type} = State) ->
  NotFound = maps:get(not_found, State, true),
  case erlorg_articles_repo:fetch_by_type(Type) of
    undefined when NotFound ->
      {ok, Body} = '404_dtl':render([]),
      {ok, Req1} = cowboy_req:reply(404, [], Body, Req),
      {halt, Req1, State};
    undefined ->
      {ok, Body} = Dtl:render(Vars),
      {Body, Req, State};
    Article ->
      ArticlePrintable = erlorg_datetime:printable_date(Article),
      {ok, Body} = Dtl:render(Vars#{article => ArticlePrintable}),
      {Body, Req, State}
  end;
handle_get(Req, #{dtl := Dtl, vars := Vars} = State) ->
  {ok, Body} = Dtl:render(Vars),
  {Body, Req, State}.
