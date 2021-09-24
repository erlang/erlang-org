---
description: Functional programming
---
```erlang
> Fact = fun Fact(1) -> 1;
             Fact(N) -> N * Fact(N-1)
         end.
#Fun<erl_eval.20.100337348>
> Fact(10).
3628800
```