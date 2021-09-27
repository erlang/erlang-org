---
description: Find even numbers
display: small
---
```erlang
-spec even(In) -> even(Out)
  when In :: Out :: list(integer()).
even(Numbers) ->
  %% Use list comprehensions to map
  [Number || Number <- Numbers,
   Number rem 2 == 0].
```