---
layout: post
title: Memory instrumentation in OTP 21
tags: erts memory instrumentation
author: John Högberg
---

The memory instrumentation module was rewritten for Erlang/OTP 21 to make it
easier to use. In this post I'll describe the rationale behind the new features
and how to make use of them.

One of the most important features that a diagnostic tool can have is the
ability to work on the fly. If it requires a restart then the condition you're
trying to diagnose might vanish, and you can't use it to troubleshoot issues on
"production" systems.

The previous implementation had a few major issues to this effect; you had to
start the Erlang VM with a certain flag, accept considerable overhead, and
worst of all suspend the VM while it collected all its data.

The amount of data it collected was also quite problematic; with one entry for
every single allocation it was difficult to tell what was hiding in all that
information, and since there was no way to tell whether a gap between two
allocations was mapped or not it was needlessly difficult to use when trying
to troubleshoot memory fragmentation.

The new implementation tackles these problems by scanning existing data
structures to lower its overhead to the point it can be turned on by default,
and tries to collect information in a manner that doesn't harm the
responsiveness of the system.

## Carriers and memory fragmentation

The VM allocates memory in large segments we call "carriers" and then allocates
blocks within those. This has many benefits; since each carrier is completely
separate from the others it's easy to determine when they can be returned to
the operating system, and they scale very well since we can guarantee that
they're only modified by per-thread instances which makes allocation and
deallocation wait-free in most cases.

There are two types of carriers; single-block which always contain one large
block, and multi-block that can contain several smaller blocks. While both of
these rely on the operating system to minimize address space fragmentation, the
latter kind can also become internally fragmented which will result in new
carriers being created if no existing multi-block carrier can satisfy an
allocation, even if the amount of unused memory exceeds the request.

While you can glean some information about average carrier utilization from
`erlang:system_info({allocator, Alloc})` and use `pmap` (or similar) to get an
idea of how fragmented the address space is, it has always been a pain to get
information about the individual carriers. Starting in OTP 21 you can ask the
system for a list of all carriers without having to start the VM with any
particular flags. The list contains information about each carrier's total
size, combined allocation size, allocation count, whether it's in the migration
pool, and a histogram over free block sizes.

We chose to represent the free blocks with histograms (log2, starting at 512 by
default) as they make it easy to tell at a glance whether a carrier has
fragmentation issues; if there's a lot of free blocks clustered along the left
side then it's pretty safe to say there's a problem.

In the example below, the `ll_alloc` carrier has no free blocks at all, the
`binary_alloc` and `eheap_alloc` ones look healthy with a few very large
blocks, and the `fix_alloc` carrier is somewhat fragmented with ~3KB free split
into 22 blocks smaller than 512 bytes (although this is not a problem for this
allocator type).

```erlang
1> instrument:carriers().
{ok,{512,
     [{ll_alloc,1048576,0,1048344,71,false,{0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
      {binary_alloc,1048576,0,324640,13,false,{3,0,0,1,0,0,0,2,0,0,0,0,0,0}},
      {eheap_alloc,2097152,0,1037200,45,false,{2,1,1,3,4,3,2,2,0,0,0,0,0,0}},
      {fix_alloc,32768,0,29544,82,false,{22,0,0,0,0,0,0,0,0,0,0,0,0,0}},
      {...}|...]}}
```

(`instrument:carriers/1` can be used to tweak the histograms and which
allocators to look in.)

## Allocations and memory utilization

Those who have used `erlang:memory()` are probably familiar with how annoyingly
general the `system` category can be. It's possible to get a bit more
information by using `erlang:system_info({allocator, Alloc})` but the most it
will do is tell you that it's (say) `driver_alloc` that eats all that memory
and leave you with no clue which one.

While it's often easy to tell which driver or NIF is causing problems while
you're developing, it's not as easy when it's used in anger alongside half a
dozen others. The new "allocation tagging" feature will help you figure out
where the memory went at the cost of one word per allocation. The allocations
are presented as block size histograms (similar to carrier information)
grouped by their origin and type:

```erlang
2> instrument:allocations()
{ok,{128,0,
     #{udp_inet =>
           #{driver_event_state => {0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0}},
       tty_sl =>
           #{io_queue => {0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             drv_internal => {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0}},
       system =>
           #{db_segment => {0,0,0,0,0,18,0,0,1,0,0,0,0,0,0,0,0,0},
             heap => {0,0,0,0,20,4,2,2,2,3,0,1,0,0,1,0,0,0},
             thr_prgr_data => {38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             db_term => {271,3,1,52,80,1,0,0,0,0,0,0,0,0,0,0,0,0},
             code => {0,0,0,5,3,6,11,22,19,20,10,2,1,0,0,0,0,0},
             binary => {18,0,0,0,7,0,0,1,0,0,0,0,0,0,0,0,0,0},
             atom_entry => {8681,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             message => {0,40,78,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0},
             ... }
       spawn_forker =>
           #{driver_select_data_state =>
                 {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
       ram_file_drv => #{drv_binary => {0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0}},
       prim_file =>
           #{process_specific_data => {2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             nif_trap_export_entry => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             monitor_extended => {0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             drv_binary => {0,0,0,0,0,0,1,0,3,5,0,0,0,1,0,0,0,0},
             binary => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}},
       prim_buffer =>
           #{nif_internal => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
             binary => {0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}}}}
```

The above example was taken with allocation tagging turned on for all
allocators (`+Muatags true` command line argument) to give you a better idea of
what it can do. By default it will only be turned on for driver/NIF allocations
and binaries since those are the most common culprits, and their allocations
are generally so large that the overhead of a single word is a drop in the
bucket.

(As with carriers, `instrument:allocations/1` can be used to tweak the
histograms and which allocators to look in.)

## Further reading

For those who'd like to know more about how our memory allocators work, Lukas
Larsson's [talk at EUC 2014](https://erlangcentral.org/videos/euc-2014-lukas-larsson-memory-allocators-in-the-vm-memory-management-battle-stories/) is a good primer. Our internal
documentation on [carrier migration](https://github.com/erlang/otp/blob/master/erts/emulator/internal_doc/CarrierMigration.md) and [inter-thread deallocation](https://github.com/erlang/otp/blob/master/erts/emulator/internal_doc/DelayedDealloc.md) may also be of interest.

The PR implementing this change can be found [here](https://github.com/erlang/otp/pull/1790), and the documentation for the old instrumentation module can be found [here](http://erlang.org/documentation/doc-9.3/lib/tools-2.11.2/doc/html/instrument.html).
