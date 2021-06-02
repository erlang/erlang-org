---
layout: post
title: ETS oddity
tags: ets write_concurrency first next
author: Lukas Larsson
---

When working with the implementation of the new [scalable ordered_set](https://github.com/erlang/otp/pull/1952)
we came across a strangeness with the guarantees when iterating over a table
while inserting elements in parallel.

### Scenario:

```
> Tab = ets:new(test_table,
                [set, public, {write_concurrency, true}]).
#Ref<0.1705802953.985792516.98626>
> P1 = spawn(fun() ->
               ets:insert(Tab, {fir, 1}),
               ets:insert(Tab, {sec, 2})
             end).
> K1 = ets:first(Tab), K2 = ets:next(Tab, K1).
```

What are the theoretical possible values of `K1` and `K2`? Let us first list the obvious:

* `K1 = fir`, `K2 = sec` - both values inserted and found in term order
* `K1 = sec`, `K2 = fir` - since this is a `set`, the hash algorithm may put `sec` before `fir`
* `K1 = fir`, `K2 = '$end_of_table'` - only `fir` had time to be inserted
* `K1 = '$end_of_table'`, `K2 = badarg` - no elements were inserted

However it is also possible to get:

* `K1 = sec`, `K2 = '$end_of_table'`

This was, at first, very counter-intuitive to me. How can the `ets:first/1` find the
second value inserted, but then when iterating not find the value inserted before it?

The answer can be found in the way that the `write_concurrency` functionality is
implemented. Imagine we have a [hash table](https://en.wikipedia.org/wiki/Hash_table)
where each bucket is protected by a mutex. When inserting a new element the mutex for
the current bucket has to be taken and when iterating over the hash table we take
each mutex in turn for the buckets we iterate through.

### Initial Table:

| Bucket #      | Values        |
| ------------- | ------------- |
| 1             | `[]`          |
| 2             | `[]`          |
| 3             | `[]`          |
| 4             | `[]`          |

### Finished Table:

| Bucket #      | Values        |
| ------------- | ------------- |
| 1             | `[{fir,1}]`   |
| 2             | `[]`          |
| 3             | `[]`          |
| 4             | `[{sec,2}]`   |

So, in the scenario that leads to the strange behaviour the following will happen:

* `ets:first/1` is called when the table is empty and iterates to Bucket #2.

| Bucket #      | Values        |
| ------------- | ------------- |
| 1             | `[]`          |
| 2 (first)     | `[]`          |
| 3             | `[]`          |
| 4             | `[]`          |


* The OS does a context switch and P1 is allowed to run.
* P1 inserts both `{fir,1}` and `{sec,2}` and then exits.

| Bucket #      | Values        |
| ------------- | ------------- |
| 1             | `[{fir,a}]`   |
| 2 (first)     | `[]`          |
| 3             | `[]`          |
| 4             | `[{sec,b}]`   |

* The `ets:first/1` call resumes and will only see `sec` and then `'$end_of_table'`.

When spelled out like this it becomes more logical that it is possible to get only
the element inserted as the second element. This is not normally a problem for
tables of type `set` which have an arbitrary iteration order that you can't depend on anyway.

However, for `ordered_set` you may very well depend on the defined iteration order
and expect `ets:first/1` to return a key that has at least been first in the table
at some point in time. But for the same reasons as with `set`, that is not guaranteed
if you need that guarantee you have to either not use `write_concurrency`,
find some other way to synchronize or rely on luck... these races are very rare, but in heavily
used tables they will eventually happen.

The same oddity applies to all kinds of table iterations; `ets:next/1`,
`ets:select/1-3`, `ets:match/1-3` and friends. They may all miss concurrently
inserted keys and return a key that has never existed in the table ordered
directly after the previously returned key.
