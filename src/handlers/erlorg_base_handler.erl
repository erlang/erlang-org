-module(erlorg_base_handler).

-export(
   [
    init/3,
    rest_init/2,
    content_types_provided/2,
    resource_exists/2
   ]
  ).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, []}, handle_get}], Req, State}.

resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.
