---
description: Find even numbers
display: small
---
```erlang
-spec even(In) -> Out
  when In :: list(integer()),
       Out :: list(integer()).
even(Numbers) ->
  [Number || Number <- Numbers,
   Number rem 2 == 0].
```