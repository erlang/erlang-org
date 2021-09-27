---
description: Higher-order functions
display: small
---
<!-- This file has to be 13 lines long -->
```erlang
> Fruits = ["banana","monkey"].
["banana","monkey"]
> lists:map(
    fun string:uppercase/1,
    Fruits).
["BANANA","MONKEY"]
```