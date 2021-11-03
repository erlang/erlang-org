---
description: Higher-order functions
display: large
---
<!-- This file has to be 15 lines long -->
```erlang
> Fruits = ["banana","monkey","jungle"].     %% Immutable variables
["banana","monkey","jungle"]
> lists:map(fun string:uppercase/1, Fruits). %% Map values using stdlib functions
["BANANA","MONKEY","JUNGLE"]
%% Fold over lists using custom functions
> lists:foldl(fun(Str, Cnt) -> string:length(Str) + Cnt end, 0, Fruits).
18

```
