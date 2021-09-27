---
description: Light-weigth processes
display: large
---
<!-- This file has to be 15 lines long -->
```erlang
> Parent = self().                         %% Get own process id
<0.376.0>
> Child = spawn(fun() -> receive go -> Parent ! lists:seq(1,100) end end).
<0.930.0>
> Child ! go.                              %% Send message to child
go
> receive Reply -> Reply end.              %% Receive response from child
[1,2,3,4,5,6,7,8,9,10,11|...]
```