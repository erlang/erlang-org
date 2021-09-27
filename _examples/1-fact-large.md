---
description: Functional programming
display: large
---
<!-- This file has to be 15 lines long -->
```erlang
fact(1) -> 1;              %% Pattern matching for control-flow
fact(N) -> N * fact(N-1).  %% Recursion to create loops

> example:fact(10).        %% Interactive shell for fast iterations
3628800
> [{I, example:fact(I)} || I <- lists:seq(1,10)].
[{1, 1}, {2, 2}, {3, 6}, {4, 24}, {5, 120}, {6, 720},
 {7, 5040}, {8, 40320}, {9, 362880}, {10, 3628800}]
```