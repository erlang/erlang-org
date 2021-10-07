---
layout: post
title: The Many-to-One Parallel Signal Sending Optimization
tags: message, signal, signal queue, message queue, parallel
author: Kjell Winblad
---

This blog post discusses [the parallel signal sending
optimization][parallel_sig_pr] that recently got merged into the
master branch (scheduled to be included in Erlang/OTP 25). The
optimization improves signal sending throughput when several processes
send signals to a single process simultaneously on multicore
machines. At the moment, the optimization is only active when one
configures the receiving process with the `{message_queue_data,
off_heap}` [setting][off_heap_setting]. The following figure gives an
idea of what type of scalability improvement the optimization can give
in extreme scenarios (number of Erlang processes sending signals on
the x-axis and throughput on the y-axis):

![alt text]({% link blog/images/parallel_siq_q/benchmark_peek.png %} "Send Benchmark Result Peek")

This blog post aims to give you an understanding of how signal sending
on a single node is implemented in Erlang and how the new optimization
can yield the impressive scalability improvement illustrated in the
figure above. Let us begin with a brief introduction to what Erlang
signals are.

Erlang Signals
--------------

All concurrently executing entities (processes, ports, etc.)  in an
Erlang system [communicate using asynchronous signals][erl_com]. The
most common signal is normal messages that are typically sent between
processes with the bang (!) operator. As Erlang takes pride in being a
concurrent programming language, it is, of course, essential that
signals are sent efficiently between different entities. Let us now
discuss what guarantees Erlang programmers get about signal sending
ordering, as this will help when learning how the new optimization works.


### The Signal Ordering Guarantee


The signal ordering guarantee is described in the [Erlang
documentation like this][sig_ord]:

> "The only signal ordering guarantee given is the following: if an
> entity sends multiple signals to the same destination entity, the
> order is preserved; that is, if `A` sends a signal `S1` to `B`, and later
> sends signal `S2` to `B`, `S1` is guaranteed not to arrive after `S2`."

This guarantee means that if multiple processes send signals to a
single process, all signals from the same process are received in the
send order in the receiving process. Still, there is no ordering
guarantee for two signals coming from two distinct processes. One
should not think about signal sending as instantaneous. There can be
an arbitrary delay after a signal has been sent until it has reached
its destination, but all signals from `A` to `B` travel on the same path
and cannot pass each other.

The guarantee has deliberately been designed to allow for efficient
implementations and allow for future optimizations. However, as we
will see in the next section, before the optimization presented in
this blog post, the implementation did not take advantage of the
permissive ordering guarantee for signals sent between processes
running on the same node.


### Single-Node Process-to-Process Implementation before the Optimization


Conceptually, the Erlang VM organized the data structure for an Erlang
process as in the following figure before the optimization:

![alt text]({% link blog/images/parallel_siq_q/before_process_struct.png %} "Process struct before optimization")

Of course, this is an extreme simplification of the Erlang process
structure, but it is enough for our explanation. When a process has
the `{message_queue_data, off_heap}` setting activated, the following
algorithm is executed to send a signal:

1. Allocate a new linked list node containing the signal data
2. Acquire the `OuterSignalQueueLock` in the receiving process
3. Insert the new node at the end of the `OuterSignalQueue`
4. Release the `OuterSignalQueueLock`

When a receiving process has run out of signals in its
`InnerSignalQueue` and/or wants to check if there are more signals in
the outer queue, the following algorithm is executed:

1. Acquire the `OuterSignalQueueLock`
2. Append the `OuterSignalQueue` at the end of the `InnerSignalQueue`
4. Release the `OuterSignalQueueLock`

How signal sending works when the receiving process is configured with
`{message_queue_data, on_heap}` is not so relevant for the main topic
of this blog post. Still, understanding how `{message_queue_data,
on_heap}` works will also give you an understaning of why the parallel
signal queue optimization is not enabled when a process is configured
with `{message_queue_data, on_heap}` (which is the default setting),
so here is the algorithm for sending a signal to such a process:


1. Try to acquire the `MainProcessLock` with a `try_lock` call
   * If the `try_lock` call succeeded:
     1. Allocate space for the signal data on the process' main heap
        area and copy the signal data there
     2. Allocate a linked list node containing a pointer to the
        process heap-allocated signal data
     3. Acquire the `OuterSignalQueueLock`
     4. Insert the linked list node at the end of the
        `OuterSignalQueue`
     5. Release the `OuterSignalQueueLock`
     6. Release the `MainProcessLock`
   * Else:
     1. Allocate a new linked list node containing the signal data
     2. Acquire the `OuterSignalQueueLock`
     3. Insert the new node at the end of the `OuterSignalQueue`
     4. Release the `OuterSignalQueueLock`


The advantage of `{message_queue_data, on_heap}` compared to
`{message_queue_data, off_heap}` is that the signal data is copied
directly to the receiving process main heap (when the `try_lock` call
for the `MainProcessLock` succeeds). The disadvantage of
`{message_queue_data, on_heap}` is that the sender creates extra
contention on the receiver's `MainProcessLock`. Notice that we cannot
simply release the `MainProcessLock` directly after allocating the
data on the receiver's process heap. If a garbage collection happen
before the signal have been inserted into the process' heap, the
signal data would be lost (holding the `MainProcessLock` prevents a
garbage collection from happening). Therefore, `{message_queue_data,
off_heap}` provides much better scalability than `{message_queue_data,
on_heap}` when multiple processes send signals to the same process
concurrently on a multicore system.

However, even though `{message_queue_data, off_heap}` scales better
than `{message_queue_data, on_heap}` with the old implementation,
signal senders still had to acquire the `OuterSignalQueueLock` for a
short time. This lock can become a scalability bottleneck and a
contended hot-spot when there are enough parallel senders. This is why
we saw very poor scalability and even a slowdown for the old
implementation in the benchmark figure above. Now, we are ready to
look at the new optimization.

The Parallel Signal Sending Optimization
----------------------------------------

The optimization takes advantage of Erlang's permissive signal
ordering guarantee discussed above. It is enough to keep the order of
signals coming from the same entity to ensure that the signal ordering
guarantee holds. So there is no need for different senders to
synchronize with each other! In theory, signal sending could therefore
be parallelized perfectly. In practice, however, there is only one
thread of execution that handles incoming signals, so we also have to
keep in mind that we don't want to slow down the receiver and ideally
make receiving signals faster. As signal queue data is stored outside
the process main heap area when the `{message_queue_data, off_heap}`
setting is enabled, the garbage collector does not need to go through
the whole signal queue, giving better performance for processes with a
lot of signals in their signal queue. Therefore, it is also important
for the optimization not to add unnecessary overhead when the
`OuterSignalQueueLock` is uncontended, so that we do not slow down
existing use cases for `{message_queue_data, off_heap}` too much.

### Data Structure and Birds-Eye-View of Optimized Implementation

We decided to go for a design that enables the parallel signal sending
optimization on demand when the contention on the `OuterSignalQueueLock`
seems to be high to avoid as much overhead as possible when the
optimization is unnecessary. Here is a conceptual view of the process
structure when the optimization is not active (which is the initial
state when creating a process with `{message_queue_data, off_heap}`):

![alt text]({% link blog/images/parallel_siq_q/after_opt_not_active_process_struct.png %} "Process struct after optimization but when the optimization is inactive")

The following figure shows a conceptual view of the process structure
when the parallel signal sending optimization is turned on. The only
difference between this and the previous figure is that the
`OuterSignalQueueBufferArray` field now points to a structure
containing an array with buffers.

![alt text]({% link blog/images/parallel_siq_q/after_opt_active_process_sturct.png %} "Process struct after optimization when the optimization is active")


When the parallel signal sending optimization is active, senders do
not need to acquire the `OuterSignalQueueLock` anymore. Senders are
mapped to a slot in the `OuterSignalQueueBufferArray` by a simple hash
function that is applied to the process ID (senders without a process
ID are currently mapped to the same slot). Before a sender takes the
`OuterSignalQueueLock` in the receiving process' structure, the sender
tries to enqueue in its slot in the `OuterSignalQueueBufferArray` (if
it exists). If the enqueue attempt succeeds, the sender can continue
without even touching the `OuterSignalQueueLock`! The order of signals
coming from the same sender is maintained because the same sender is
always mapped to the same slot in the buffer array. Now, you have
probably got an idea of why the signal sending throughput can increase
so much with the new optimization, as we saw in the benchmark figure
presented earlier. Essentially, the contention on the
`OuterSignalQueueLock` gets distributed among the slots in the
`OuterSignalQueueBufferArray`. The rest of the subsections in this
section cover details of the implementation, so you can skip those
if you do not want to dig deeper.

### Adaptively Activating the Outer Signal Queue Buffers

As the figure above tries to illustrate, the `OuterSignalQueueLock` carries
a statistics counter. When that statistics counter reaches a certain
threshold, the new parallel signal sending optimization is activated
by installing the `OuterSignalQueueBufferArray` in the process
structure. The statistics counter for the lock is updated in a simple
way. When a thread tries to acquire the `OuterSignalQueueLock` and the lock
is already taken, the counter is increased, and otherwise, it is
decreased, as the following code snippet illustrates:

```c
void erts_proc_sig_queue_lock(Process* proc)
{
    if (EBUSY == erts_proc_trylock(proc, ERTS_PROC_LOCK_MSGQ)) {
        erts_proc_lock(proc, ERTS_PROC_LOCK_MSGQ);
        proc->sig_inq_contention_counter += 1;
    } else if(proc->sig_inq_contention_counter > 0) {
        proc->sig_inq_contention_counter -= 1;
    }
}

```

### The Outer Signal Queue Buffer Array Structure

Currently, the number of slots in the `OuterSignalQueueBufferArray` is
fixed to 64. Sixty-four slots should go a long way to reduce signal
queue contention in most practical application that exists today. Few
servers have more than 100 cores, and typical applications spend a lot
of time doing other things than sending signals. Using 64 slots also
allows us to implement a very efficient atomically updatable bitset
containing information about which slots are currently non-empty (the
`NonEmptySlots` field in the figure above). This bitset makes flushing
the buffer array into the `OuterSignalQueue` more efficient
since only the non-empty slots in the buffer array need to be visited
and updated to perform the flush.

### Sending Signals with the Optimization Activated

Pseudo-code for the algorithm that is executed when a process is
sending a signal to another process that has the
`OuterSignalQueueBufferArray` installed can be seen below:


1. Allocate a new linked list node containing the signal data
2. Map the process ID of the sender to the right slot `I` with the hash function
3. Acquire the `SlotLock` for the slot `I`
4. Check the `IsAlive` field for slot `I`
   * If the `IsAlive` field's value is `true`:
     1. Set the appropriate bit in the `NonEmptySlots` field, if the buffer is empty
     2. Insert the allocated signal node at the end of the `BufferQueue` for slot `I`
     3. Increase the `NumberOfEnqueues` in slot `I` by 1
     4. Release `SlotLock` for slot `I`
     5. The signal is enqueued, and the thread can continue with the next task
   * Else (the `OuterSignalQueueBufferArray` has been deactivated):
     1. Release the lock for slot `I`
     2. Do the insert into the `OuterSignalQueue` in the same way as
        the signal sending algorithm did it prior to the optimization

### Fetching Signals from the Outer Signal Queue Buffer Array and Deactivation of the Optimization

The algorithm for fetching signals from the outer signal queue uses
the `NonEmptySlots` field in the `OuterSignalQueueBufferArray`, so it
only needs to check slots that are guaranteed to be non-empty. At a
high level, the routine works according to the following pseudo-code:

1. Acquire the `OuterSignalQueueLock`
2. For each non-empty slot in the buffer array:
   1. Lock the slot
   2. Append the signals in the slot to the end of `OuterSignalQueue`
   3. Add the value of the slot's `NumberOfEnqueues` field to the
      `TotNumberOfEnqueues` field in the `OuterSignalQueueBufferArray`
   4. Reset the slot's `BufferQueue` and `NumberOfEnqueues` fields
   5. Unlock the slot
3. Increase the value of the `NumberOfFlushes` field in the
   `OuterSignalQueueBufferArray` by one
4. If the value of the `NumberOfFlushes` field has reached a certain
   threshold `T`:
   * Calculate the average number of enqueues per flush
     (`EnqPerFlush`) during the last `T` flushes
     (`TotNumberOfEnqueues` / `T`).
      * If `EnqPerFlush` is below a certain threshold `Q`:
         * Deactivate the parallel signal sending optimization:
           1. For each slot in the `OuterSignalQueueBufferArray`:
              1. Acquire the `SlotLock`
              2. Append the signals in the slot (if any) to the end of `OuterSignalQueue`
              3. Set the slot's `IsAlive` field to `false`
              4. Release the `SlotLock`
           2. Set the `OuterSignalQueueBufferArray` field in the process
              structure to `NULL`
           3. Schedule deallocation of the buffer array structure
      * Else if the average is equal to or above the threshold `Q`:
        * Set the `NumberOfFlushes` and the `TotNumberOfEnqueues`
          fields in the buffer array struct to 0
5. Append the `OuterSignalQueue` to the end of the `InnerSignalQueue`
6. Reset the `OuterSignalQueue`
7. Release the `OuterSignalQueueLock`

For simplicity, many details have been left out from the pseudo-code
snippets above. However, if you have understood them, you have an
excellent understanding of how signal sending in Erlang works, how the
new optimization is implemented, and how it automatically activates
and deactivates itself. Let us now dive a little bit deeper into
benchmark results for the new implementation.

Benchmark
---------

A configurable benchmark to measure the performance of both signal
sending processes and receiving processes has been created. The
benchmark lets `N` Erlang processes send signals (of configurable types
and sizes) to a single process during a period of `T` seconds. Both `N`
and `T` are configurable variables. A signal with size `S` has a payload
consisting of a list of length `S` with word-sized (64 bits) items. The
send throughput is calculated by dividing the number of signals that
are sent by `T`. The receive throughput is calculated by waiting until
all sent signals have been received and then dividing the total number
of signals sent by the time between when the first signal was sent and
when the last signal was received. The benchmark machine has 32 cores
and two hardware threads per core (giving 64 hardware threads). You
can find a detailed benchmark description on the [signal queue
benchmark page][sig_q_bench_page].

First, let us look at the results for very small messages (a list
containing a single integer) below. The graph for the receive
throughput is the same as we saw at the beginning of this blog post. Not
surprisingly, the scalability for sending messages is much better
after the optimization. More surprising is that the performance of
receiving messages is also substantially improved. For example, with
16 processes, the receive throughput is 520 times better with the
optimization! The improved receive throughput can be explained by the
fact that in this scenario, the receiver has to fetch messages from
the outer signal queue much more seldom. Sending is much faster
after the optimization, so the receiver will bring more messages from
the outer signal queue to the inner every time it runs out of
messages. The sender can thus process messages from the inner queue
for a longer time before it needs to fetch messages from the outer
queue again. We cannot expect any improvement for the receiver beyond
a certain point as there is only a single hardware thread that can
work on processing messages at the same time.


![alt text]({% link blog/images/parallel_siq_q/small_msg_send_receive_throughput.png %} "Small Messages Benchmark Result")


Below are the results for larger messages (a list containing 100
integers). We do not get as good improvement in this scenario with a
larger message size. With larger messages, the benchmark spends more
time doing other work than sending and receiving messages. Things like
the speed of the memory system and memory allocation might become
limiting factors. Still, we get decent improvement both in the send
throughput and receive throughput, as seen below.

![alt text]({% link blog/images/parallel_siq_q/large_msg_send_receive_throughput.png %} "Large Messages Benchmark Result")

You can find results for even larger messages as well as for
non-message signals on the [benchmark page][sig_q_bench_page]. Real
Erlang applications do much more than message and signal sending, so
this benchmark is, of course, not representative of what kind of
improvements real applications will get. However, the benchmarks show
that we have pushed the threshold for when parallel message sending to
a single process becomes a problem. Perhaps the new optimization opens
up new interesting ways of writing software that was impractical due
to previous performance reasons.


Possible Future Work
--------------------

Users can configure processes with `{message_queue_data, off_heap}` or
`{message_queue_data, on_heap}`. This configurability increases the
burden for Erlang programmers as it can be difficult to figure out
which one is better for a particular process. It would therefore make
sense also to have a `{message_queue_data, auto}` option that would
automatically detect lock contention even in `on_heap` mode and
seamlessly switch between `on_heap` and `off_heap` based on how much
contention is detected.

As discussed previously, 64 slots in the signal queue buffer array is
a good start but might not be enough when servers have thousands of
cores. A possible way to make the implementation even more scalable
would be to make the signal queue buffer array expandable. For
example, one could have contention detecting locks for each slot in
the array. If the contention is high in a particular slot, one could
expand this slot by creating a link to a subarray with buffers where
senders can use another hash function (similar to how the [HAMT data
structure][hamt] works).



Conclusion
----------

The new parallel signal queue optimization that affects processes
configured with `{message_queue_data, off_heap}` yields much better
scalability when multiple processes send signals to the same process
in parallel. The optimization has a very low overhead when the
contention is low as it is only activated when its contention
detection mechanism indicates that the contention is high.


[sig_ord]: https://erlang.org/doc/reference_manual/processes.html#signal-delivery
[parallel_sig_pr]: https://github.com/erlang/otp/pull/5020
[sig_q_bench_page]: http://winsh.me/bench/erlang_sig_q/sigq_bench_result.html
[erl_com]: https://erlang.org/doc/apps/erts/communication.html
[hamt]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[off_heap_setting]: https://erlang.org/doc/man/erlang.html#spawn_opt-4
