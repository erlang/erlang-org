---
layout: post
title: Decentralized ETS Counters for Better Scalability
tags: ETS, erlang term storage, scalability, multicore
author: Kjell Winblad
---

A shared [Erlang Term Storage
(ETS)](https://erlang.org/doc/man/ets.html) table is often an
excellent place to store data that is updated and read from
multiple Erlang processes frequently. ETS provides key-value stores to
Erlang processes. When the
[write_concurrency](https://erlang.org/doc/man/ets.html#new-2) option
is activated, ETS tables use fine-grained locking
internally. Therefore, a scenario where multiple processes insert and
remove different items in an ETS table should scale well with the
number of utilized cores. However, in practice the scalability
for such scenarios is not yet perfect. This blog post will explore
how the `decentralized_counters` option brings us one step closer to
perfect scalability.

The ETS table option
[`decentralized_counters`](https://erlang.org/doc/man/ets.html#new-2)
(introduced in Erlang/OTP 22 for `ordered_set` tables and in
Erlang/OTP 23 for the other table types) has made the scalability much
better. A table with `decentralized_counters` activated uses
decentralized counters instead of centralized counters to track the
number of items in the table and the memory
consumption. Unfortunately, tables with `decentralized_counters`
activated will have slow operations to get the table size and
memory usage ([`ets:info(Table,
size)`](https://erlang.org/doc/man/ets.html#info-2) and
[`ets:info(Table,
memory)`](https://erlang.org/doc/man/ets.html#info-2)), so whether it
is beneficial to turn `decentralized_counters` on or off depends on
your use case. This blog post will give you a better understanding of
when one should activate the `decentralized_counters` option and how
the decentralized counters work.

## Scalability with Decentralized ETS Counters

The following figure shows the throughput (operations/second) achieved
when processes are doing inserts (`ets:insert/2`) and deletes
(`ets:delete/2`) to an ETS table of the `set` type on a machine with
64 hardware threads both when `decentralized_counters` option is
activated and when it is deactivated. The table types `bag` and
`duplicate_bag` have similar scalability behavior as their
implementation is based on the same hash table.

![alt text]({% link blog/images/ets_scalable_counters/bench_set_50_ins_50_del_nospread.png %} "Throughput of inserts and deletes on a table of type set with and without the decentralized_counters activated")

The following figure shows the results for the same benchmark but with
a table of type `ordered_set`:

![alt text]({% link blog/images/ets_scalable_counters/bench_ordset_50_ins_50_del_nospread.png %} "Throughput of inserts and deletes on a table of type ordered_set with and without the decentralized_counters activated")

The interested reader can find more information about the benchmark at
the [benchmark website for
`decentralized_counters`](http://winsh.me/ets_catree_benchmark/decent_ctrs_hash.html). The
benchmark results above show that both `set` and `ordered_set` tables
get a significant scalability boost when the `decentralized_counter`
option is activated. The `ordered_set` type receives a more
substantial scalability improvement than the `set` type. Tables of the
set type have a fixed number of locks for the hash table buckets. The
`ordered_set` table type is implemented with a [contention adapting
search tree](https://doi.org/10.1016/j.jpdc.2017.11.007) that
dynamically changes the locking granularity based on how much
contention is detected. This implementation difference explains the
difference in scalability between `set` and `ordered_set`. The
interested reader can find details about the `ordered_set`
implementation in an [earlier blog
post]({% link _posts/2020-8-19-the-new-scalable-ets-ordered_set.md %}).

Worth noting is also that the Erlang VM that ran the benchmarks has
been compiled with the configure option "`./configure
--with-ets-write-concurrency-locks=256`". The configure option
`--with-ets-write-concurrency-locks=256` changes the number of locks
for hash-based ETS tables from the current default of 64 to 256 (256
is currently the max value one can set this configuration option
to). Changing the implementation of the hash-based tables so that one
can set the number of locks per table instance or so that the lock
granularity is adjusted automatically seems like an excellent future
improvement, but this is not what this blog post is about.

A centralized counter consists of a single memory word that is
incremented and decremented with atomic instructions. The problem with
a centralized counter is that modifications of the counter
by multiple cores are serialized. This problem is amplified because
frequent modifications of a single memory word by multiple cores cause
a lot of expensive traffic in the [cache
coherence](https://en.wikipedia.org/wiki/Cache_coherence)
system. However, reading from a centralized counter is quite efficient
as the reader only has to read a single memory word.

When designing the decentralized counters for ETS, we have tried to
optimize for update performance and scalability as most applications
need to get the size of an ETS table relatively rarely. However, since
there may be applications out in the wild that frequently call
[`ets:info(Table, size)`](https://erlang.org/doc/man/ets.html#info-2)
and [`ets:info(Table,
memory)`](https://erlang.org/doc/man/ets.html#info-2), we have chosen
to make decentralized counters optional.

Another thing that might be worth keeping in mind is that the
hash-based tables that use decentralized counters tend to use slightly
more hash table buckets than the corresponding tables without
decentralized counters. The reason for this is that, with
decentralized counters activated, the resizing decision is based on an
estimate of the number of items in the table rather than an exact
count, and the resizing heuristics trigger an increase of the number
of buckets more eagerly than a decrease.

## Implementation

You will now learn how the decentralized counters in ETS works. The
[decentralized counter implementation exports an
API](https://github.com/erlang/otp/blob/ce7dbe8742e66f4632b5d39a9b4d7aa461e4f164/erts/emulator/beam/erl_flxctr.h)
that makes it easy to swap between a decentralized counter and a
centralized one. ETS uses this to support the usage of both
centralized and decentralized counters. The data structure for the
decentralized counter is illustrated in the following picture. When
`is_decentralized = false`, the counter field represents the current
count instead of a pointer to an array of cache line padded counters.

![alt text]({% link blog/images/ets_scalable_counters/structure.png %} "An image
showing the structure of a decentralized counter")

When `is_decentralized = true`, processes that update (increment or
decrement) the counter follow the pointer to the array of counters and
increments the counter at the slot in the array that the current
scheduler maps to (one takes the scheduler identifier modulo the
number of slots in the array to get the appropriate slot). Updates do
not need to do anything else, so they are very efficient and can scale
perfectly with the number of cores as long as there are as many slots
as schedulers. One can configure the maximum number of slots in the
array of counters with the
[`+dcg`](https://erlang.org/doc/man/erl.html) option.

To implement the `ets:info(Table, size)` and `ets:info(Table, memory)`
operations, one also needs to read the current counter value. Reading
the current counter value can be implemented by taking the sum of the
values in the counter array. However, if this summation is done
concurrently with updates to the array of counters, we could get
strange results. For example, we could end up in a situation where
`ets:info(Table, size)` returns a negative number, which is not
exactly what we want. On the other hand, we want to make counter
updates as fast as possible so having locks to protect the counters in
the counter array is not a good solution. We opted for a solution that
lets readers swap out the entire counter array and wait (using the
[Erlang VM's thread progress
system](https://github.com/erlang/otp/blob/7c06ca6231b812965305522284dd9f2653ced98d/erts/emulator/internal_doc/ThreadProgress.md))
until no updates can occur in the swapped-out array before the sum is
calculated. The following example illustrates this approach:


* **[Step 1]**
   
   A thread is going to read the counter value.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_1.png %} "Step 1")

* **[Step 2]**
   
   The reader starts by creating a new counter array.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_1_b.png %} "Step 2")

* **[Step 3]**
   
   The pointer to the old counter array is changed to point to the new
   one with the `snapshot_ongoing` field set to `true`. This
   change can only be done when the `snapshot_onging` field is set to
   `false` in the old counter array.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_2.png %} "Step 3")

* **[Step 4]**
   
   Now, the reader has to wait until all other threads that will
   update a counter in the old array have completed their updates. As
   mentioned, this can be done using the [Erlang VM's thread progress
   system](https://github.com/erlang/otp/blob/7c06ca6231b812965305522284dd9f2653ced98d/erts/emulator/internal_doc/ThreadProgress.md). After
   that, the reader can safely calculate the sum of counters in the
   old counter array (the sum is 1406). The calculated sum is also
   given to the process that requested the count so that it can
   continue execution.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_3.png %} "Step 4")

* **[Step 5]**
   
   The read operation is not done, even though we have successfully
   calculated a count. One must add the calculated sum from the old
   array to the new array to avoid losing something.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_4.png %} "Step 5")

* **[Step 6]**
   
   Finally, the `snapshot_ongoing` field in the new counter array is
   set to `false` so that other read operations can swap out the new
   counter array.
   
   ![alt text]({% link blog/images/ets_scalable_counters/snap_ani_5.png %} "Step 6")

Now, you should have got a basic understanding of how ETS'
decentralized counters work. You are also welcome to look at the
source code in
[erl_flxctr.c](https://github.com/erlang/otp/blob/ce7dbe8742e66f4632b5d39a9b4d7aa461e4f164/erts/emulator/beam/erl_flxctr.c)
and
[erl_flxctr.h](https://github.com/erlang/otp/blob/ce7dbe8742e66f4632b5d39a9b4d7aa461e4f164/erts/emulator/beam/erl_flxctr.h)
if you are interested in details of the implementation.

As you can imagine, reading the value of a decentralized counter with,
for example, `ets:info(Table, size)` is extremely slow compared to a
centralized counter. Fortunately, most time that is spent reading the
value of a decentralized counter is spent waiting for the thread
progress system to report that it is safe to read the swapped-out array,
and the read operation does not block any scheduler and does not
consume any CPU time during this time. On the other hand, the
decentralized counter can be updated in a very efficient and scalable
way, so using decentralized counters is most likely to prefer, if you
seldom need to get the size and the memory consumed by your shared
ETS table.


## Concluding Remarks


This blog post has described the implementation of the decentralized
counter option for ETS tables. ETS tables with decentralized counters
scale much better with the number of cores than ETS tables with
centralized counters. However, as decentralized counters make
`ets:info(Table, size)` and `ets:info(Table, memory)` very slow, one
should not use them if any of these two operations need to be
performed frequently.
