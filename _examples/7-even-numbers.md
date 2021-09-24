---
description: Find even numbers
display: small
---
```erlang
-spec even(list(integer())) ->
  list(integer()).
even(Numbers) ->
  [Number || Number <- Numbers,
   Number rem 2 == 0].
```