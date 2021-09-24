---
description: Higher-order functions
---
```erlang
> Fruits = ["apple","banana","jungle"].
["apple","banana","jungle"]
> lists:map(fun string:uppercase/1, Fruits).
["APPLE","BANANA","JUNGLE"]
```