---
description: Light-weigth processes
display: large
---
```erlang
> Parent = self().
<0.376.0>
> Child = spawn(fun() -> receive Function -> Parent ! Function() end end).
<0.930.0>
> Child ! fun() -> lists:seq(1,100) end.
#Fun<erl_eval.45.100337348>
> receive Reply -> Reply end.
[1,2,3,4,5,6,7,8,9,10,11|...]
```