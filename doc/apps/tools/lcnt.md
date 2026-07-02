# `lcnt`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/tools/src/lcnt.erl#L23)

A runtime system Lock Profiling tool.

The `lcnt` module is used to profile the internal ethread locks in the Erlang
Runtime System. With `lcnt` enabled, internal counters in the runtime system are
updated each time a lock is taken. The counters stores information about the
number of acquisition tries and the number of collisions that has occurred
during the acquisition tries. The counters also record the waiting time a lock
has caused for a blocked thread when a collision has occurred.

The data produced by the lock counters will give an estimate on how well the
runtime system will behave from a parallelizable view point for the scenarios
tested. This tool was mainly developed to help Erlang runtime developers iron
out potential and generic bottlenecks.

Locks in the emulator are named after what type of resource they protect and
where in the emulator they are initialized, those are lock 'classes'. Most of
those locks are also instantiated several times, and given unique identifiers,
to increase locking granularity. Typically an instantiated lock protects a
disjunct set of the resource, for example ets tables, processes or ports. In
other cases it protects a specific range of a resource, for example `pix_lock`
which protects index to process mappings, and is given a unique number within
the class. A unique lock in `lcnt` is referenced by a name (class) and an
identifier: `{Name, Id}`.

Some locks in the system are static and protects global resources, for example
`bif_timers` and the `run_queue` locks. Other locks are dynamic and not
necessarily long lived, for example process locks and ets-table locks. The
statistics data from short lived locks can be stored separately when the locks
are deleted. This behavior is by default turned off to save memory but can be
turned on via `lcnt:rt_opt({copy_save, true})`. The `lcnt:apply/1,2,3` functions
enables this behavior during profiling.

### See Also

[LCNT User's Guide](lcnt_chapter.md)

# `category_atom`
*not exported* *since OTP R13B04* 

```erlang
-type category_atom() :: atom().
```

# `lock_counter_data`
*not exported* *since OTP R13B04* 

```erlang
-type lock_counter_data() :: term().
```

# `option`
*not exported* *since OTP R13B04* 

```erlang
-type option() ::
          {sort, Sort :: sort()} |
          {reverse, boolean()} |
          {locations, boolean()} |
          {thresholds, Thresholds :: [threshold()]} |
          {print, PrintOptions :: [print() | {print(), non_neg_integer()}]} |
          {max_locks, MaxLocks :: non_neg_integer() | none} |
          {combine, boolean()}.
```

# `print`
*not exported* *since OTP R13B04* 

```erlang
-type print() :: colls | duration | entry | id | name | ratio | time | tries | type.
```

# `sort`
*not exported* *since OTP R13B04* 

```erlang
-type sort() :: colls | entry | id | name | ratio | time | tries | type.
```

# `threshold`
*not exported* *since OTP R13B04* 

```erlang
-type threshold() :: {colls, non_neg_integer()} | {time, non_neg_integer()} | {tries, non_neg_integer()}.
```

# `apply`
*since OTP R13B04* 

```erlang
-spec apply(Fun) -> term() when Fun :: fun().
```

# `apply`
*since OTP R13B04* 

```erlang
-spec apply(Fun, Args) -> term() when Fun :: fun(), Args :: [term()].
```

Sets up lock counters, applies `Fun` with `Args`, and cleans up.

Clears the lock counters and then setups the instrumentation to save all
destroyed locks. After setup the function is called, passing the elements in
`Args` as arguments. When the function returns the statistics are immediately
collected to the server. After the collection the instrumentation is returned to
its previous behavior. The result of the applied function is returned.

> #### Warning {: .warning }
>
> This function should only be used for micro-benchmarks; it sets `copy_save` to
> `true` for the duration of the call, which can quickly lead to running out of
> memory.

# `apply`
*since OTP R13B04* 

```erlang
-spec apply(Module, Function, Args) -> term()
               when Module :: module(), Function :: atom(), Args :: [term()].
```

# `clear`
*since OTP R13B04* 

```erlang
-spec clear() -> ok.
```

# `clear`
*since OTP R13B04* 

```erlang
-spec clear(Node) -> ok when Node :: node().
```

Clears the internal lock statistics from the runtime system.

This clears the data in the runtime system but not in server.  All
counters for static locks are zeroed, all dynamic locks currently
alive are zeroed and all saved locks now destroyed are removed. It
also resets the duration timer.

# `collect`
*since OTP R13B04* 

```erlang
-spec collect() -> ok.
```

# `collect`
*since OTP R13B04* 

```erlang
-spec collect(Node) -> ok when Node :: node().
```

Collects lock statistics from the runtime system.

The function starts a server if it is not already started. It then
populates the server with lock statistics.  If the server held any
lock statistics data before the collect then that data is lost.

# `conflicts`
*since OTP R13B04* 

```erlang
-spec conflicts() -> ok.
```

# `conflicts`
*since OTP R13B04* 

```erlang
-spec conflicts(Options) -> ok when Options :: [option()].
```

Prints a list of internal locks and its statistics.

For option description, see [`lcnt:inspect/2`](`inspect/2`).

# `information`
*since OTP R13B04* 

```erlang
-spec information() -> ok.
```

Prints `lcnt` server state and generic information about collected lock
statistics.

# `inspect`
*since OTP R13B04* 

```erlang
-spec inspect(Lock) -> ok
                 when
                     Lock :: Name | {Name, Id | [Id]},
                     Name :: atom() | pid() | port(),
                     Id :: atom() | integer() | pid() | port().
```

# `inspect`
*since OTP R13B04* 

```erlang
-spec inspect(Lock, Options) -> ok
                 when
                     Lock :: Name | {Name, Id | [Id]},
                     Name :: atom() | pid() | port(),
                     Id :: atom() | integer() | pid() | port(),
                     Options :: [option()].
```

Prints a list of internal lock counters for a specific lock.

Lock `Name` and `Id` for ports and processes are interchangeable with the use of
[`lcnt:swap_pid_keys/0`](`swap_pid_keys/0`) and is the reason why `t:pid/0` and
`t:port/0` options can be used in both `Name` and `Id` space. Both pids and
ports are special identifiers with stripped creation and can be recreated with
[`lcnt:pid/2,3`](`pid/3`) and [`lcnt:port/1,2`](`port/2`).

Option description:

- **`{combine, boolean()}`** - Combine the statistics from different instances
  of a lock class.  
  Default: `true`

- **`{locations, boolean()}`** - Print the statistics by source file and line
  numbers.  
  Default: `false`

- **`{max_locks, MaxLocks}`** - Maximum number of locks printed or no limit with
  `none`.  
  Default: `20`

- **`{print, PrintOptions}`** - Printing options:

  - **`name`** - Named lock or named set of locks (classes). The same name used
    for initializing the lock in the VM.

  - **`id`** - Internal id for set of locks, not always unique. This could be
    table name for ets tables (db_tab), port id for ports, integer identifiers
    for allocators, etc.

  - **`type`** - Type of lock: `rw_mutex`, `mutex`, `spinlock`, `rw_spinlock` or
    `proclock`.

  - **`entry`** - In combination with `{locations, true}` this option prints the
    lock operations source file and line number entry-points along with
    statistics for each entry.

  - **`tries`** - Number of acquisitions of this lock.

  - **`colls`** - Number of collisions when a thread tried to acquire this lock.
    This is when a trylock is EBUSY, a write try on read held rw_lock, a try
    read on write held rw_lock, a thread tries to lock an already locked lock.
    (Internal states supervises this.)

  - **`ratio`** - The ratio between the number of collisions and the number of
    tries (acquisitions) in percentage.

  - **`time`** - Accumulated waiting time for this lock. This could be greater
    than actual wall clock time, it is accumulated for all threads. Trylock
    conflicts does not accumulate time.

  - **`duration`** - Percentage of accumulated waiting time of wall clock time.
    This percentage can be higher than 100% since accumulated time is from all
    threads.

  Default: `[name,id,tries,colls,ratio,time,duration]`

- **`{reverse, boolean()}`** - Reverses the order of sorting.  
  Default: `false`

- **`{sort, Sort}`** - Column sorting orders.  
  Default: `time`

- **`{thresholds, Thresholds}`** - Filtering thresholds. Anything values above
  the threshold value are passed through.  
  Default: `[{tries, 0}, {colls, 0}, {time, 0}]`

# `load`
*since OTP R13B04* 

```erlang
-spec load(Filename) -> ok when Filename :: file:filename().
```

Restores previously saved data to the server.

# `locations`
*since OTP R13B04* 

```erlang
-spec locations() -> ok.
```

# `locations`
*since OTP R13B04* 

```erlang
-spec locations(Options) -> ok when Options :: [option()].
```

Prints a list of internal lock counters by source code locations.

For option description, see [`lcnt:inspect/2`](`inspect/2`).

# `pid`
*since OTP R13B04* 

```erlang
-spec pid(Id, Serial) -> pid() when Id :: integer(), Serial :: integer().
```

# `pid`
*since OTP R13B04* 

```erlang
-spec pid(Node, Id, Serial) -> pid() when Node :: node(), Id :: integer(), Serial :: integer().
```

Creates a process id with creation 0.

# `port`
*since OTP R13B04* 

```erlang
-spec port(Id) -> port() when Id :: integer().
```

# `port`
*since OTP R13B04* 

```erlang
-spec port(Node, Id) -> port() when Node :: node(), Id :: integer().
```

Creates a port id with creation 0.

# `rt_clear`
*since OTP R13B04* 

```erlang
-spec rt_clear() -> ok.
```

# `rt_clear`
*since OTP R13B04* 

```erlang
-spec rt_clear(Node) -> ok when Node :: node().
```

Clear the internal counters.

Equivalent to [`lcnt:clear(Node)`](`clear/1`).

# `rt_collect`
*since OTP R13B04* 

```erlang
-spec rt_collect() -> [lock_counter_data()].
```

# `rt_collect`
*since OTP R13B04* 

```erlang
-spec rt_collect(Node) -> [lock_counter_data()] when Node :: node().
```

Returns a list of raw lock counter data.

# `rt_mask`
*since OTP 20.1* 

```erlang
-spec rt_mask() -> [category_atom()].
```

Return the current category mask for the current node.

# `rt_mask`
*since OTP 20.1* 

```erlang
-spec rt_mask(Node) -> [category_atom()] when Node :: node();
             (Categories) -> ok | {error, copy_save_enabled} when Categories :: [category_atom()].
```

Sets the current lock category mask for the current node or
retrieves the current mask for a remote node.

If `Arg` is an atom, it is assumed to be a node, and this
call returns the current lock category mask for node `Arg`.

If `Arg` is a list, this call is equivalent to
[`rt_mask(node(), Arg)`](`rt_mask/2`).

# `rt_mask`
*since OTP 20.1* 

```erlang
-spec rt_mask(Node, Categories) -> ok | {error, copy_save_enabled}
                 when Node :: node(), Categories :: [category_atom()].
```

Sets the lock category mask according to `Categories` on node `Node`.

This call will fail if the `copy_save` option is enabled; see
[`lcnt:rt_opt/2`](`rt_opt/2`).

Valid categories are:

- `allocator`
- `db` (ETS tables)
- `debug`
- `distribution`
- `generic`
- `io`
- `process`
- `scheduler`

This list is subject to change at any time, as is the category any given lock
belongs to.

# `rt_opt`
*since OTP R13B04* 

```erlang
-spec rt_opt(Option) -> boolean()
                when Option :: {Type, Value :: boolean()}, Type :: copy_save | process_locks.
```

# `rt_opt`
*since OTP R13B04* 

```erlang
-spec rt_opt(Node, Option) -> boolean()
                when
                    Node :: node(),
                    Option :: {Type, Value :: boolean()},
                    Type :: copy_save | process_locks.
```

Sets a single option on node `Node`.

Option description:

- **`{copy_save, boolean()}`** - Retains the statistics of destroyed locks.  
  Default: `false`

  > #### Warning {: .warning }
  >
  > This option will use a lot of memory when enabled, which must be reclaimed
  > with [`lcnt:rt_clear/0,1`](`lcnt:rt_clear/1`). Note that it makes no
  > distinction between locks that  were destroyed and locks for which counting
  > was disabled, so enabling this option will disable changes to the lock
  > category mask.

- **`{process_locks, boolean()}`** - Profile process locks, equal to adding
  `process` to the lock category mask; see `lcnt:rt_mask/2`.  
  Default: `true`

# `save`
*since OTP R13B04* 

```erlang
-spec save(Filename) -> ok when Filename :: file:filename().
```

Saves the collected data to file.

# `start`
*since OTP R13B04* 

```erlang
-spec start() -> {ok, Pid} | {error, {already_started, Pid}} when Pid :: pid().
```

Starts the lock profiler server.

The server only act as a medium for the user and performs filtering
and printing of data collected by `lcnt:collect/1`.

# `stop`
*since OTP R13B04* 

```erlang
-spec stop() -> ok.
```

Stops the lock profiler server.

# `swap_pid_keys`
*since OTP R13B04* 

```erlang
-spec swap_pid_keys() -> ok.
```

Swaps places on `Name` and `Id` space for ports and processes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
