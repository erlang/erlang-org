-module(erlorg_binary).

-export([
         join/2
        ]).

%% @doc Joins and returns a list of binaries
-spec join([binary()], binary()) -> binary().
join([], _) ->
    <<>>;
join([S], _) when is_binary(S) ->
    S;
join([H | T], Sep) ->
    B = << <<Sep/binary, X/binary>> || X <- T >>,
  <<H/binary, B/binary>>.
