-module(erlorg_eeps_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {erlorg_base_handler,
         [
          init/3,
          rest_init/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_get/2,
   content_types_provided/2
  ]).

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {Path, Req1} = path_to_file(Req),
  ContentType = case filename:extension(Path) of
                  <<".md">> -> {<<"text">>, <<"plain">>, []};
                  <<".png">> -> {<<"image">>, <<"png">>, []};
                  _ -> {<<"text">>, <<"html">>, []}
                end,

  {[{ContentType, handle_get}], Req1, State#{path => Path}}.

%% internal
handle_get(Req, #{path := PathEEP} = State) ->
  case filelib:is_file(PathEEP) of
    true ->
      Body = dispatch(filename:extension(PathEEP), PathEEP),
      {Body, Req, State};
    false ->
      {ok, Body} = '404_dtl':render([]),
      {ok, Req1} = cowboy_req:reply(404, [], Body, Req),
      {halt, Req1, State}
  end.

dispatch(<<".html">>, Path) ->
  {ok, Content} = file:read_file(Path),
  {ok, Body} = eeps_dtl:render(#{eep_content => Content}),
  Body;
dispatch(_, Path) ->
  {ok, Content} = file:read_file(Path),
  Content.

path_to_file(Req) ->
  {Bindings, Req1} = erlorg_req:bindings_map(Req),

  {ok, DirEEP} = application:get_env(erlorg, eeps_dir),
  {ok, DefaultEEP} = application:get_env(erlorg, eeps_default),

  EEP = case maps:get(eep, Bindings) of
          <<"home">> -> DefaultEEP;
          EEPName ->
            case filename:extension(EEPName) of
              <<>> -> <<EEPName/binary, ".html">>;
              _ -> EEPName
            end
        end,
  PathEEP = iolist_to_binary([DirEEP,  EEP]),

  {PathEEP, Req1}.
