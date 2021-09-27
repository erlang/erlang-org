---
description: Functional programming
display: small
---
<!-- This file has to be 13 lines long -->
```erlang
%% Return factorial for N
fact(1) -> 1;
fact(N) -> N * fact(N-1).

> example:fact(10).
3628800
```