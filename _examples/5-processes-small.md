---
description: Light-weigth processes
display: small
---
<!-- This file has to be 13 lines long -->
```erlang
> Me = self().
<0.376.0>        %% Send msg using !
> spawn(fun() -> Me!lists:seq(1,10) end).
<0.930.0>
> receive Reply -> Reply end.
[1,2,3,4,5,6,7,8,9,10]
```