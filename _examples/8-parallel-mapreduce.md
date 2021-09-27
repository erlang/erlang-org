---
description: Parallel map-reduce to find even numbers
display: large
---
<!-- This file has to be 15 lines long -->
```erlang
-spec even(list(integer())) -> list(integer()).
even(Numbers) ->
  mapreduce(Numbers, fun(Number) -> Number rem 2 == 0 end).
mapreduce(Numbers, Function) ->
  Parent = self(),
  [spawn(fun() -> Parent ! {Number, Function(Number)} end) || Number <- Numbers],
  lists:flatten(
    [receive {Number, true} -> Number; _ -> [] end || Number <- Numbers]).
```