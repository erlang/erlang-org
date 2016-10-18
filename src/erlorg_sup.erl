-module(erlorg_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% admin api
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% behaviour callbacks
init({}) ->
  {ok, {{one_for_one, 5, 10},
%        [worker(erlorg_mailinglists)]}
	[]}
  }.

%worker(I) ->
%  {I, {I, start_link, []}, permanent, infinity, worker, [I]}.
