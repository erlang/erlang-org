---
layout: post
title: I/O polling options in OTP 21
tags: erts polling tcp
author: Lukas Larsson
---

Erlang/OTP 21 will introduce a completely new IO polling implementation.
This new implementation comes with a new set of tuneable parameters that
can be used to get the most out of your system. This blog post describes
the parameters and attempts to describe what they should be used for.

The I/O polling framework in erts is responsible for delivering events to
ports and processes that have subscribed to events on file descriptors.
Before OTP 21 it was the job of an Erlang scheduler thread to deliver these
events. In OTP 21 dedicated threads are used to deliver the events.

For information about how the new implementation works under the hood you can
look at Kenneth Lundin's presentation [Erlang VM News Regarding Dirty Schedulers and I/O](http://www.erlang-factory.com/euc2017/kenneth-lundin)
from the EUC 2017.

## Kernel-space vs User-space polling

In OTP 21 the `+K` option has been removed as it is not longer possible to
choose whether to use kernel-space poll or not at run-time. Instead the decision
is made at compile time where kernel-space poll will be used by default. If you
want to use user-space poll instead you have to pass the `--disable-kernel-poll`
flag to configure when compiling Erlang/OTP.

Before OTP 21 it made sense to run using user-space polling if the file
descriptors that was subscribed to tended to be removed quickly. For example
if a HTTP server managed short-lived connection from only a handful other
machines, it could be beneficial to use user-space poll. However if the
connection start being long-lived, or the number of concurrent connection
go up, kernel-space poll becomes better.

In OTP 21, this is no longer true. Because the polling has been moved to another
thread, it is almost always better to use kernel-space polling. The user-space
polling implementation is left in place for platforms that do not support
parallel update of the kernel-space pollset. Also user-space polling is used
for individual file descriptors when they cannot be put in a kernel-space pollset
for some reason.

## Poll-threads and Poll-sets

OTP 21 introduces two new configuration parameters: +IOt and +IOp.

### Configure +IOt

+IOt controls the number of threads that are used to deliver events. The default
is 1 and it should be enough for most applications. However on very busy
systems with many concurrent connection it could be beneficial to increase this.
One way to get an indication of whether your system could benefit from it is
by using [msacc](http://erlang.org/doc/man/msacc.html). If you turn it on briefly
and when examining the `msacc:print()` output notice that sleep time
of the the thread type `poll` is low, the system may benefit from increasing the
number of polling threads.

```
Eshell V9.3  (abort with ^G)
1> msacc:start(10000),msacc:print().
Average thread real-time    : 10000410 us
Accumulated system run-time :      937 us
Average scheduler run-time  :      897 us

        Thread      aux check_io emulator       gc    other     port    sleep

Stats per thread:
     async( 0)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
       aux( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
dirty_cpu_( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
dirty_io_s( 1)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
      poll( 0)    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
 scheduler( 1)    0.00%    0.00%    0.00%    0.00%    0.01%    0.00%   99.99%

Stats per type:
         async    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
           aux    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
dirty_cpu_sche    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
dirty_io_sched    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
          poll    0.00%    0.00%    0.00%    0.00%    0.00%    0.00%  100.00%
     scheduler    0.00%    0.00%    0.00%    0.00%    0.01%    0.00%   99.99%
```

In the example above the poll thread is sleeping for 100% of the time so no need to
increase the number of poll threads.

### Configure +IOp

+IOp controls the number of pollsets used to put the file descriptors in. This
options defaults to 1, and it should be very rare for any system to benefit
from changing this. The only time so far that I have seen it to be beneficial is when the
kernel-space poll implementation does not scale well when accessed in parallel
by multiple threads. So if you run [perf top](http://man7.org/linux/man-pages/man1/perf-top.1.html)
(or something similar) on your system and notice that a lot of time is spent
locking the kernel-space pollset, it would be a good idea to increase the
number of pollsets used.
