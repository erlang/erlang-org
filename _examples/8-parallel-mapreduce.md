---
description: Parallel map-reduce to find even numbers
display: large
---
```erlang
-spec even(list(integer())) -> list(integer()).
even(Numbers) ->
  mapreduce(Numbers, fun(Number) -> Number rem 2 == 0 end).
mapreduce(Numbers, Function) ->
  Parent = self(),
  [spawn(fun() -> Parent ! {Number, Function(Number)} end) || Number <- Numbers],
  [receive {Number, Even} -> Number end || Number <- Numbers, Even == true].
```