-module(insert_disjoint_ranges).
-mode(compile).
-export([bench/3, main/1]).

inserter(_Table, RangeEnd, RangeEnd, P) ->
    P ! done;
inserter(Table, RangeStart, RangeEnd, P) ->
    ets:insert(Table, {RangeStart}),
    inserter(Table, RangeStart + 1, RangeEnd, P).

bench(T, NrOfProcs, NrOfItems) ->
    N = NrOfItems div NrOfProcs,
    Parent = self(),
    {Time, _} =
        timer:tc(
          fun() ->
                  % Spawn inserters
                  [ spawn(
                      fun()->
                              End = case (P+1) =:= NrOfProcs of
                                        true -> NrOfItems;
                                        false -> (P+1)*N
                                    end,
                              inserter(T, P*N, End, Parent)
                      end)
                    || P <- lists:seq(0, NrOfProcs-1) ],
                  % Wait for inserters to finish'
                  [ receive done -> ok end || _ <- lists:seq(1, NrOfProcs)]
          end),
    io:format("Time: ~p seconds ~p~n", [Time / 1000000, ets:info(T, size)]).

main([Type, NrOfProcs, Size])
  when is_integer(Size) andalso is_integer(Size) andalso Size > 0 andalso NrOfProcs > 0 ->
    Settings =
        case Type of
            "old" -> [public, ordered_set];
            "new" -> [public, ordered_set, {write_concurrency, true}]
        end,
    bench(ets:new(t, Settings), NrOfProcs, Size);
main([V, NrOfProcsStr, SizeStr]) when not is_integer(SizeStr) ->
    main([V, catch list_to_integer(NrOfProcsStr), catch list_to_integer(SizeStr)]);
main(_) ->
    io:format("usage: escript ~s (new|old) NrOfProcesses Size~n", [escript:script_name()]).

