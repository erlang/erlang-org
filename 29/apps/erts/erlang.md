# `erlang`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/erts/preloaded/src/erlang.erl#L22)

The Erlang BIFs and predefined types.

By convention, most [Built-In Functions](`e:system:ref_man_functions.md#built-in-functions-bifs`)
(BIFs) and all [predefined types](`e:system:typespec.md#predefined`) are included
in this module. Some of the BIFs and all of the predefined types are viewed more
or less as part of the Erlang programming language and are _auto-imported_.
Thus, it is not necessary to specify the module name. For example, the calls
[`atom_to_list(erlang)`](`atom_to_list/1`) and [`erlang:atom_to_list(erlang)`](`atom_to_list/1`)
are identical.

Auto-imported BIFs are annotated with `auto-imported` and predefined types are
annotated with `predefined`.

Some auto-imported BIFs are also allowed in [guard expression](`e:system:expressions.md#guard-expressions`).
Such BIFs are annoted with both `auto-imported` and `guard-bif`.

BIFs can fail for various reasons. All BIFs fail with reason `badarg` if they
are called with arguments of an incorrect type. The other reasons are described
in the description of each individual BIF.

# ``
*predefined* 

```erlang
-type nil() :: [].
```

The empty `t:list/0`.

# `any`
*predefined* 

```erlang
-type any() :: any().
```

All possible Erlang terms. Synonym for `t:term/0`.

# `arity`
*predefined* 

```erlang
-type arity() :: arity().
```

The arity of a function, type or callback.

# `atom`
*predefined* 

```erlang
-type atom() :: atom().
```

An Erlang [atom](`e:system:data_types.md#atom`).

# `binary`
*predefined* 

```erlang
-type binary() :: <<_:_*8>>.
```

An Erlang [binary](`e:system:data_types.md#bit-strings-and-binaries`), that is,
a bitstring with a size divisible by 8.

# `bitstring`
*predefined* 

```erlang
-type bitstring() :: <<_:_*1>>.
```

An Erlang [bitstring](`e:system:data_types.md#bit-strings-and-binaries`).

# `boolean`
*predefined* 

```erlang
-type boolean() :: true | false.
```

A [boolean](`e:system:data_types.md#boolean`) value.

# `byte`
*predefined* 

```erlang
-type byte() :: 0..255.
```

A byte of data represented by an integer.

# `char`
*predefined* 

```erlang
-type char() :: 0..1114111.
```

An ASCII character or a `m:unicode` codepoint presented by an integer.

# `dynamic`
*predefined* 

```erlang
-type dynamic() :: dynamic().
```

The [dynamic](`e:system:typespec.md#dynamic`) type, which represents a statically unknown type

# `float`
*predefined* 

```erlang
-type float() :: float().
```

An Erlang [float](`e:system:data_types.md#number`).

# `function`
*predefined* 

```erlang
-type function() :: fun().
```

An Erlang [fun](`e:system:data_types.md#fun`).

# `identifier`
*predefined* 

```erlang
-type identifier() :: pid() | port() | reference().
```

An unique identifier for some entity, for example a
[process](`e:system:ref_man_processes.md`), [port](`e:system:ports.md#ports`) or
[monitor](`monitor/2`).

# `integer`
*predefined* 

```erlang
-type integer() :: integer().
```

An Erlang [integer](`e:system:data_types.md#number`).

# `iodata`
*predefined* 

```erlang
-type iodata() :: iolist() | binary().
```

A binary or list containing bytes and/or iodata.

This datatype is used to represent data that is meant to be output using
any I/O module. For example: `file:write/2` or `gen_tcp:send/2`.

To convert an `t:iodata/0` term to `t:binary/0` you can use
[iolist_to_binary/2](`iolist_to_binary/1`). To transcode a `t:string/0` or
`t:unicode:chardata/0` to `t:iodata/0` you can use `unicode:characters_to_binary/1`.

# `iolist`
*predefined* 

```erlang
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).
```

A list containing bytes and/or iodata.

This datatype is used to represent data that is meant to be output using any
I/O module. For example: `file:write/2` or `gen_tcp:send/2`.

In most use cases you want to use `t:iodata/0` instead of this type.

# `list`
*predefined* 

```erlang
-type list() :: [any()].
```

An Erlang [list](`e:system:data_types.md#list`) containing terms of any type.

# `list`
*predefined* 

```erlang
-type list(ContentType) :: [ContentType].
```

An Erlang [list](`e:system:data_types.md#list`) containing terms of the type
`ContentType`.

# `map`
*predefined* 

```erlang
-type map() :: #{any() => any()}.
```

An Erlang [map](`e:system:data_types.md#map`) containing any number of key and
value associations.

# `maybe_improper_list`
*predefined* 

```erlang
-type maybe_improper_list() :: maybe_improper_list(any(), any()).
```

An Erlang [list](`e:system:data_types.md#list`) that is not guaranteed to end
with a [`[]`](`t:nil/0`), and where the list elements can be of any type.

# `maybe_improper_list`
*predefined* 

```erlang
-type maybe_improper_list(ContentType, TerminationType) ::
          maybe_improper_list(ContentType, TerminationType).
```

An Erlang [list](`e:system:data_types.md#list`), that is not guaranteed to end
with a [`[]`](`t:nil/0`), and where the list elements are of the type
`ContentType`.

# `mfa`
*predefined* 

```erlang
-type mfa() :: {module(), atom(), arity()}.
```

A three-tuple representing a `Module:Function/Arity` function signature.

# `module`
*predefined* 

```erlang
-type module() :: atom().
```

An Erlang module represented by an atom.

# `neg_integer`
*predefined* 

```erlang
-type neg_integer() :: neg_integer().
```

A negative integer.

# `no_return`
*predefined* 

```erlang
-type no_return() :: none().
```

The type used to show that a function will _never_ return a value, that is it
will _always_ throw an exception.

# `node`
*predefined* 

```erlang
-type node() :: atom().
```

An Erlang [node](`e:system:distributed.md#nodes`) represented by an atom.

# `non_neg_integer`
*predefined* 

```erlang
-type non_neg_integer() :: non_neg_integer().
```

A non-negative integer, that is any positive integer or 0.

# `none`
*predefined* 

```erlang
-type none() :: none().
```

This type is used to show that a function will _never_ return a value; that is
it will _always_ throw an exception.

In a spec, use `t:no_return/0` for the sake of clarity.

# `nonempty_binary`
*predefined* 

```erlang
-type nonempty_binary() :: <<_:8, _:_*8>>.
```

A `t:binary/0` that contains some data.

# `nonempty_bitstring`
*predefined* 

```erlang
-type nonempty_bitstring() :: <<_:1, _:_*1>>.
```

A `t:bitstring/0` that contains some data.

# `nonempty_improper_list`
*predefined* 

```erlang
-type nonempty_improper_list(ContentType, TerminationType) ::
          nonempty_improper_list(ContentType, TerminationType).
```

A [maybe_improper_list/2](`t:maybe_improper_list/0`) that contains some items.

# `nonempty_list`
*predefined* 

```erlang
-type nonempty_list() :: [any(), ...].
```

A `t:list/0` that contains some items.

# `nonempty_list`
*predefined* 

```erlang
-type nonempty_list(ContentType) :: [ContentType, ...].
```

A [list(ContentType)](`t:list/0`) that contains some items.

# `nonempty_maybe_improper_list`
*predefined* 

```erlang
-type nonempty_maybe_improper_list() :: nonempty_maybe_improper_list(any(), any()).
```

A `t:maybe_improper_list/0` that contains some items.

# `nonempty_maybe_improper_list`
*predefined* 

```erlang
-type nonempty_maybe_improper_list(ContentType, TerminationType) ::
          nonempty_maybe_improper_list(ContentType, TerminationType).
```

A [maybe_improper_list(ContentType, TerminationType)](`t:maybe_improper_list/0`)
that contains some items.

# `nonempty_string`
*predefined* 

```erlang
-type nonempty_string() :: [char(), ...].
```

A `t:string/0` that contains some characters.

# `number`
*predefined* 

```erlang
-type number() :: integer() | float().
```

An Erlang [number](`e:system:data_types.md#number`).

# `pid`
*predefined* 

```erlang
-type pid() :: pid().
```

An Erlang [process identifier](`e:system:data_types.md#pid`).

# `port`
*predefined* 

```erlang
-type port() :: port().
```

An Erlang [port identifier](`e:system:data_types.md#port-identifier`).

# `pos_integer`
*predefined* 

```erlang
-type pos_integer() :: pos_integer().
```

An integer greater than zero.

# `record`
*predefined* 

```erlang
-type record() :: record().
```

A [native record](`e:system:data_types.md#native-record`).

# `reference`
*predefined* 

```erlang
-type reference() :: reference().
```

An Erlang [reference](`e:system:data_types.md#reference`).

# `string`
*predefined* 

```erlang
-type string() :: [char()].
```

A character string represented by a list of ASCII characters or unicode
codepoints.

# `term`
*predefined* 

```erlang
-type term() :: any().
```

All possible Erlang terms. Synonym for `t:any/0`.

# `timeout`
*predefined* 

```erlang
-type timeout() :: infinity | non_neg_integer().
```

A timeout value that can be passed to a
[receive expression](`e:system:expressions.md#receive`).

# `tuple`
*predefined* 

```erlang
-type tuple() :: tuple().
```

An Erlang [tuple](`e:system:data_types.md#tuple`).

# `bitstring_list`

```erlang
-type bitstring_list() :: maybe_improper_list(byte() | bitstring() | bitstring_list(), bitstring() | []).
```

# `cpu_topology`

```erlang
-type cpu_topology() :: [LevelEntry :: level_entry()] | undefined.
```

The current cpu topology.

`node` refers to Non-Uniform Memory Access (NUMA) nodes. `thread` refers
to hardware threads (for example, Intel hyper-threads).

A level in term `CpuTopology` can be omitted if only one entry exists and
`InfoList` is empty.

`thread` can only be a sublevel to `core`. `core` can be a sublevel to
`processor` or `node`. `processor` can be on the top level or a sublevel to
`node`. `node` can be on the top level or a sublevel to `processor`. That
is, NUMA nodes can be processor internal or processor external. A CPU
topology can consist of a mix of processor internal and external NUMA nodes,
as long as each logical CPU belongs to _one_ NUMA node. Cache hierarchy is
not part of the `CpuTopology` type, but will be in a future release. Other
things can also make it into the CPU topology in a future release. So, expect
the `CpuTopology` type to change.

# `deprecated_time_unit`

```erlang
-type deprecated_time_unit() :: seconds | milli_seconds | micro_seconds | nano_seconds.
```

The `t:time_unit/0` type also consist of the following _deprecated_ symbolic
time units:

- **`seconds`** - Same as [`second`](`t:time_unit/0`).

- **`milli_seconds`** - Same as [`millisecond`](`t:time_unit/0`).

- **`micro_seconds`** - Same as [`microsecond`](`t:time_unit/0`).

- **`nano_seconds`** - Same as [`nanosecond`](`t:time_unit/0`).

# `dist_handle`

```erlang
-opaque dist_handle()
```

An opaque handle identifying a distribution channel.

# `ext_binary`

```erlang
-type ext_binary() :: binary().
```

A binary data object, structured according to the Erlang external term format.

# `ext_iovec`

```erlang
-type ext_iovec() :: iovec().
```

A term of type `t:iovec/0`, structured according to the Erlang external term
format.

# `fun_info_item`

```erlang
-type fun_info_item() :: arity | env | index | name | module | new_index | new_uniq | pid | type | uniq.
```

# `garbage_collection_defaults`

```erlang
-type garbage_collection_defaults() ::
          [{max_heap_size, non_neg_integer()} |
           {min_bin_vheap_size, non_neg_integer()} |
           {min_heap_size, non_neg_integer()} |
           {fullsweep_after, non_neg_integer()}].
```

A list with the system wide garbage collection defaults.

# `halt_options`

```erlang
-type halt_options() :: [{flush, boolean()} | {flush_timeout, Timeout :: 0..2147483647 | infinity}].
```

# `info_list`

```erlang
-type info_list() :: [].
```

# `iovec`

```erlang
-type iovec() :: [binary()].
```

A list of binaries. This datatype is useful to use together with
[`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec).

# `level_entry`

```erlang
-type level_entry() ::
          {LevelTag :: level_tag(), SubLevel :: sub_level()} |
          {LevelTag :: level_tag(), InfoList :: info_list(), SubLevel :: sub_level()}.
```

# `level_tag`

```erlang
-type level_tag() :: core | node | processor | thread.
```

# `link_option`
*since OTP 28.0* 

```erlang
-type link_option() :: priority.
```

See `link/2`.

# `match_variable`

```erlang
-type match_variable() :: atom().
```

# `max_heap_size`

```erlang
-type max_heap_size() ::
          Size ::
              non_neg_integer() |
              #{size => non_neg_integer(),
                kill => boolean(),
                error_logger => boolean(),
                include_shared_binaries => boolean()}.
```

Process max heap size configuration. For more info see
[`process_flag(max_heap_size, MaxHeapSize)`](#process_flag_max_heap_size)

# `memory_type`

```erlang
-type memory_type() ::
          total | processes | processes_used | system | atom | atom_used | binary | code | ets.
```

# `message_queue_data`

```erlang
-type message_queue_data() :: off_heap | on_heap.
```

See
[`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

Process message queue data configuration. For more information, see
[`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data)

# `monitor_option`

```erlang
-type monitor_option() ::
          {alias, explicit_unalias | demonitor | reply_demonitor} | {tag, term()} | priority.
```

See `monitor/3`.

# `monitor_port_identifier`

```erlang
-type monitor_port_identifier() :: port() | registered_name().
```

# `monitor_process_identifier`

```erlang
-type monitor_process_identifier() :: pid() | registered_process_identifier().
```

# `nif_resource`

```erlang
-opaque nif_resource()
```

An opaque handle identifying a
[NIF resource object ](erl_nif.md#resource_objects).

# `prepared_code`

```erlang
-opaque prepared_code()
```

# `priority_level`

```erlang
-type priority_level() :: low | normal | high | max.
```

Process priority level. For more info see
[`process_flag(priority, Level)`](#process_flag_priority)

# `process_info_item`

```erlang
-type process_info_item() ::
          async_dist | backtrace | binary | catchlevel | current_function | current_location |
          current_stacktrace | dictionary |
          {dictionary, Key :: term()} |
          error_handler | garbage_collection | garbage_collection_info | group_leader | heap_size |
          initial_call | links | label | last_calls | memory | message_queue_len | messages |
          min_heap_size | min_bin_vheap_size | monitored_by | monitors | message_queue_data | parent |
          priority | priority_messages | reductions | registered_name | sequential_trace_token |
          stack_size | status | suspending | total_heap_size | trace | trap_exit.
```

# `process_info_result_item`

```erlang
-type process_info_result_item() ::
          {async_dist, Enabled :: boolean()} |
          {backtrace, Bin :: binary()} |
          {binary, BinInfo :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}]} |
          {catchlevel, CatchLevel :: non_neg_integer()} |
          {current_function, {Module :: module(), Function :: atom(), Arity :: arity()} | undefined} |
          {current_location,
           {Module :: module(),
            Function :: atom(),
            Arity :: arity(),
            Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]}} |
          {current_stacktrace, Stack :: [stack_item()]} |
          {dictionary, Dictionary :: [{Key :: term(), Value :: term()}]} |
          {{dictionary, Key :: term()}, Value :: term()} |
          {error_handler, Module :: module()} |
          {garbage_collection, GCInfo :: [{atom(), non_neg_integer()}]} |
          {garbage_collection_info, GCInfo :: [{atom(), non_neg_integer()}]} |
          {group_leader, GroupLeader :: pid()} |
          {heap_size, Size :: non_neg_integer()} |
          {initial_call, mfa()} |
          {links, PidsAndPorts :: [pid() | port()]} |
          {label, term()} |
          {last_calls, false | (Calls :: [mfa()])} |
          {memory, Size :: non_neg_integer()} |
          {message_queue_len, MessageQueueLen :: non_neg_integer()} |
          {messages, MessageQueue :: [term()]} |
          {min_heap_size, MinHeapSize :: non_neg_integer()} |
          {min_bin_vheap_size, MinBinVHeapSize :: non_neg_integer()} |
          {max_heap_size, MaxHeapSize :: max_heap_size()} |
          {monitored_by, MonitoredBy :: [pid() | port() | nif_resource()]} |
          {monitors,
           Monitors :: [{process | port, Pid :: pid() | port() | {RegName :: atom(), Node :: node()}}]} |
          {message_queue_data, MQD :: message_queue_data()} |
          {parent, pid() | undefined} |
          {priority, Level :: priority_level()} |
          {priority_messages, Enabled :: boolean()} |
          {reductions, Number :: non_neg_integer()} |
          {registered_name, [] | (Atom :: atom())} |
          {sequential_trace_token, [] | (SequentialTraceToken :: term())} |
          {stack_size, Size :: non_neg_integer()} |
          {status, Status :: exiting | garbage_collecting | waiting | running | runnable | suspended} |
          {suspending,
           SuspendeeList ::
               [{Suspendee :: pid(),
                 ActiveSuspendCount :: non_neg_integer(),
                 OutstandingSuspendCount :: non_neg_integer()}]} |
          {total_heap_size, Size :: non_neg_integer()} |
          {trace, InternalTraceFlags :: non_neg_integer()} |
          {trap_exit, Boolean :: boolean()}.
```

# `processes_iter_ref`

```erlang
-opaque processes_iter_ref()
```

# `raise_stacktrace`

```erlang
-type raise_stacktrace() ::
          [{module(), atom(), arity() | [term()]} | {function(), arity() | [term()]}] | stacktrace().
```

A extended `t:stacktrace/0` that can be passed to `raise/3`.

# `registered_name`

```erlang
-type registered_name() :: atom().
```

# `registered_process_identifier`

```erlang
-type registered_process_identifier() :: registered_name() | {registered_name(), node()}.
```

# `scheduler_bind_type`

```erlang
-type scheduler_bind_type() ::
          no_node_processor_spread | no_node_thread_spread | no_spread | processor_spread | spread |
          thread_spread | thread_no_node_processor_spread | unbound.
```

The requested scheduler bind type.

# `send_destination`

```erlang
-type send_destination() ::
          pid() | reference() | port() | (RegName :: atom()) | {RegName :: atom(), Node :: node()}.
```

The destination for a send operation.

This can be a remote or local process identifier, a (local) port, a reference
denoting a process alias, a locally registered name, or a tuple `{RegName, Node}`
for a registered name at another node.

# `spawn_opt_option`

```erlang
-type spawn_opt_option() ::
          link |
          {link, LinkOpts :: [link_option()]} |
          monitor |
          {monitor, MonitorOpts :: [monitor_option()]} |
          {priority, Level :: priority_level()} |
          {fullsweep_after, Number :: non_neg_integer()} |
          {min_heap_size, Size :: non_neg_integer()} |
          {min_bin_vheap_size, VSize :: non_neg_integer()} |
          {max_heap_size, Size :: max_heap_size()} |
          {message_queue_data, MQD :: message_queue_data()} |
          {async_dist, Enabled :: boolean()}.
```

Options for [`spawn_opt()`](`spawn_opt/4`).

# `stack_item`

```erlang
-type stack_item() ::
          {Module :: module(),
           Function :: atom(),
           Arity :: arity() | (Args :: [term()]),
           Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]}.
```

# `stacktrace`

```erlang
-type stacktrace() ::
          [{module(), atom(), arity() | [term()], [stacktrace_extrainfo()]} |
           {function(), arity() | [term()], [stacktrace_extrainfo()]}].
```

An Erlang stacktrace as described by
[Errors and Error Handling](`e:system:errors.md#stacktrace`) section in the
Erlang Reference Manual.

# `stacktrace_extrainfo`

```erlang
-type stacktrace_extrainfo() ::
          {line, pos_integer()} |
          {file, unicode:chardata()} |
          {error_info, #{module => module(), function => atom(), cause => term()}} |
          {atom(), term()}.
```

# `sub_level`

```erlang
-type sub_level() :: [LevelEntry :: level_entry()] | (LogicalCpuId :: {logical, non_neg_integer()}).
```

# `system_monitor_option`

```erlang
-type system_monitor_option() ::
          busy_port | busy_dist_port |
          {long_gc, non_neg_integer()} |
          {long_message_queue, {Disable :: non_neg_integer(), Enable :: pos_integer()}} |
          {long_schedule, non_neg_integer()} |
          {large_heap, non_neg_integer()}.
```

# `system_profile_option`

```erlang
-type system_profile_option() ::
          exclusive | runnable_ports | runnable_procs | scheduler | timestamp | monotonic_timestamp |
          strict_monotonic_timestamp.
```

# `time_unit`

```erlang
-type time_unit() ::
          pos_integer() |
          second | millisecond | microsecond | nanosecond | native | perf_counter |
          deprecated_time_unit().
```

The time unit used by erlang time APIs.

Supported time unit representations:

- **`PartsPerSecond :: integer() >= 1`** - Time unit expressed in parts per
  second. That is, the time unit equals `1/PartsPerSecond` second.

- **`second`** - Symbolic representation of the time unit represented by the
  integer `1`.

- **`millisecond`** - Symbolic representation of the time unit represented by
  the integer `1000`.

- **`microsecond`** - Symbolic representation of the time unit represented by
  the integer `1000_000`.

- **`nanosecond`** - Symbolic representation of the time unit represented by the
  integer `1000_000_000`.

- **`native`** - Symbolic representation of the native time unit used by the
  Erlang runtime system.

  The `native` time unit is determined at runtime system start, and remains the
  same until the runtime system terminates. If a runtime system is stopped and
  then started again (even on the same machine), the `native` time unit of the
  new runtime system instance can differ from the `native` time unit of the old
  runtime system instance.

  One can get an approximation of the `native` time unit by calling
  [`erlang:convert_time_unit(1, second, native)`](`convert_time_unit/3`). The
  result equals the number of whole `native` time units per second. If the
  number of `native` time units per second does not add up to a whole number,
  the result is rounded downwards.

  > #### Note {: .info }
  >
  > The value of the `native` time unit gives you more or less no information
  > about the quality of time values. It sets a limit for the
  > [resolution](time_correction.md#time-resolution) and for the
  > [precision](time_correction.md#time-precision) of time values, but it gives
  > no information about the [accuracy](time_correction.md#time-accuracy) of
  > time values. The resolution of the `native` time unit and the resolution of
  > time values can differ significantly.

- **`perf_counter`** - Symbolic representation of the performance counter time
  unit used by the Erlang runtime system.

  The `perf_counter` time unit behaves much in the same way as the `native` time
  unit. That is, it can differ between runtime restarts. To get values of this
  type, call `os:perf_counter/0`.

- **`t:deprecated_time_unit/0`** -
  Deprecated symbolic representations kept for backwards-compatibility.

The `t:time_unit/0` type can be extended. To convert time values between time
units, use [`erlang:convert_time_unit/3`](`convert_time_unit/3`).

# `timestamp`

```erlang
-type timestamp() ::
          {MegaSecs :: non_neg_integer(), Secs :: non_neg_integer(), MicroSecs :: non_neg_integer()}.
```

See [`erlang:timestamp/0`](`timestamp/0`).

# `trace_flag`

```erlang
-type trace_flag() ::
          all | send | 'receive' | procs | ports | call | arity | return_to | silent | running |
          exiting | running_procs | running_ports | garbage_collection | timestamp | cpu_timestamp |
          monotonic_timestamp | strict_monotonic_timestamp | set_on_spawn | set_on_first_spawn |
          set_on_link | set_on_first_link |
          {tracer, pid() | port()} |
          {tracer, module(), term()}.
```

# `trace_info_flag`

```erlang
-type trace_info_flag() ::
          send | 'receive' | set_on_spawn | call | return_to | procs | set_on_first_spawn |
          set_on_link | running | garbage_collection | timestamp | monotonic_timestamp |
          strict_monotonic_timestamp | arity.
```

# `trace_info_item_result`

```erlang
-type trace_info_item_result() ::
          {traced, global | local | false | undefined} |
          {match_spec, trace_match_spec() | false | undefined} |
          {meta, pid() | port() | false | undefined | []} |
          {meta, module(), term()} |
          {meta_match_spec, trace_match_spec() | false | undefined} |
          {call_count, non_neg_integer() | boolean() | undefined} |
          {call_time | call_memory,
           [{pid(), non_neg_integer(), non_neg_integer(), non_neg_integer()}] | boolean() | undefined}.
```

# `trace_info_return`

```erlang
-type trace_info_return() ::
          undefined |
          {flags, [trace_info_flag()]} |
          {tracer, pid() | port() | []} |
          {tracer, module(), term()} |
          trace_info_item_result() |
          {all, [trace_info_item_result()] | false | undefined}.
```

# `trace_match_spec`

```erlang
-type trace_match_spec() :: [{[term()] | '_' | match_variable(), [term()], [term()]}].
```

# `trace_pattern_flag`

```erlang
-type trace_pattern_flag() ::
          global | local | meta |
          {meta, Pid :: pid()} |
          {meta, TracerModule :: module(), TracerState :: term()} |
          call_count | call_time | call_memory.
```

# `trace_pattern_mfa`

```erlang
-type trace_pattern_mfa() :: {atom(), atom(), arity() | '_'} | on_load.
```

# `abs`
*auto-imported* *allowed in guard tests* 

```erlang
-spec abs(Float) -> float() when Float :: float();
         (Int) -> non_neg_integer() when Int :: integer().
```

Returns an integer or float representing the absolute value of `Float`
or `Int`.

## Examples

```erlang
1> abs(-3.33).
3.33
2> abs(-3).
3
3> abs(5).
5
```

# `adler32`

```erlang
-spec adler32(Data) -> non_neg_integer() when Data :: iodata().
```

Computes and returns the adler32 checksum for `Data`.

## Examples

```erlang
1> Data = ~"abc".
2> erlang:adler32(Data).
38600999
```

# `adler32`

```erlang
-spec adler32(OldAdler, Data) -> non_neg_integer() when OldAdler :: non_neg_integer(), Data :: iodata().
```

Continues computing the adler32 checksum by combining the previous checksum,
`OldAdler`, with the checksum of `Data`.

The following code:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:adler32(Data1).
38600999
3> Y = erlang:adler32(X,Data2).
136184406
```

assigns the same value to `Y` as this:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> Y = erlang:adler32([Data1,Data2]).
136184406
```

# `adler32_combine`

```erlang
-spec adler32_combine(FirstAdler, SecondAdler, SecondSize) -> non_neg_integer()
                         when
                             FirstAdler :: non_neg_integer(),
                             SecondAdler :: non_neg_integer(),
                             SecondSize :: non_neg_integer().
```

Combines two previously computed adler32 checksums.

This computation requires the size of the data object for the second checksum
to be known.

The following code:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:adler32(Data1).
38600999
3> Z = erlang:adler32(X,Data2).
136184406
```

assigns the same value to `Z` as this:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:adler32(Data1).
38600999
3> Y = erlang:adler32(Data2).
39780656
4> Z = erlang:adler32_combine(X,Y,iolist_size(Data2)).
136184406
```

# `alias`
*auto-imported* *since OTP 24.0* 

```erlang
-spec alias() -> Alias when Alias :: reference().
```

# `alias`
*auto-imported* *since OTP 24.0* 

```erlang
-spec alias(Opts) -> Alias when Alias :: reference(), Opts :: [explicit_unalias | reply | priority].
```

Create an alias which can be used when sending messages to the process that
created the alias. When the alias has been deactivated, messages sent using the
alias will be dropped. An alias can be deactivated using `unalias/1`.

Currently available options for [`alias/1`](`alias/1`):

- **`explicit_unalias`** - The alias can only be deactivated via a call to
  [`unalias/1`](`unalias/1`). This is also the default behaviour if no options
  are passed or if `alias/0` is called.

- **`reply`** - The alias will be automatically deactivated when a reply message
  sent via the alias is received. The alias can also still be deactivated via a
  call to [`unalias/1`](`unalias/1`).

- **`priority`** - [](){: #priority_alias } Since OTP 28.0

  The alias can be used for sending
  [priority messages](`e:system:ref_man_processes.md#priority-messages`) to the
  process that created this alias. An alias created with this option is also
  known as a *priority process alias* or shorter *priority alias*.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  section of the _Erlang Reference Manual_.

Example:

```erlang
server() ->
    receive
        {request, AliasReqId, Request} ->
            Result = perform_request(Request),
            AliasReqId ! {reply, AliasReqId, Result}
    end,
    server().

client(ServerPid, Request) ->
    AliasReqId = alias([reply]),
    ServerPid ! {request, AliasReqId, Request},
    %% Alias will be automatically deactivated if we receive a reply
    %% since we used the 'reply' option...
    receive
        {reply, AliasReqId, Result} -> Result
    after 5000 ->
            unalias(AliasReqId),
            %% Flush message queue in case the reply arrived
            %% just before the alias was deactivated...
            receive {reply, AliasReqId, Result} -> Result
            after 0 -> exit(timeout)
            end
    end.
```

Note that both the server and the client in this example must be executing on at
least OTP 24 systems in order for this to work.

For more information on process aliases see the
[_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section of
the _Erlang Reference Manual_.

# `append_element`

```erlang
-spec append_element(Tuple1, Term) -> Tuple2 when Tuple1 :: tuple(), Tuple2 :: tuple(), Term :: term().
```

Returns a new tuple that has one element more than `Tuple1`, and contains the
elements in `Tuple1` followed by `Term` as the last element.

Semantically equivalent to
[`list_to_tuple(tuple_to_list(Tuple1) ++ [Term])`](`list_to_tuple/1`), but
faster.

## Examples

```erlang
1> erlang:append_element({one, two}, three).
{one,two,three}
```

# `apply`
*auto-imported* 

```erlang
-spec apply(Fun, Args) -> term() when Fun :: function(), Args :: [term()].
```

Calls a fun, passing the elements in `Args` as arguments.

If the number of elements in the arguments are known at compile time, the call
is better written as `Fun(Arg1, Arg2, ... ArgN)`.

> #### Warning {: .warning }
>
> Earlier, `Fun` could also be specified as `{Module, Function}`, equivalent to
> [`apply(Module, Function, Args)`](`apply/3`). _This use is deprecated and will
> stop working in a future release._

# `apply`
*auto-imported* 

```erlang
-spec apply(Module, Function, Args) -> term()
               when Module :: module(), Function :: atom(), Args :: [term()].
```

Returns the result of applying `Function` in `Module` to `Args`. The applied
function must be exported from `Module`. The arity of the function is the length
of `Args`.

For example:

```erlang
1> apply(lists, reverse, [[a, b, c]]).
[c,b,a]
2> apply(erlang, atom_to_list, ['Erlang']).
"Erlang"
```

If the number of arguments are known at compile time, the call is better written
as `Module:Function(Arg1, Arg2, ..., ArgN)`.

Failure: `error_handler:undefined_function/3` is called if the applied function
is not exported. The error handler can be redefined (see `process_flag/2`). If
`error_handler` is undefined, or if the user has redefined the default
`error_handler` so the replacement module is undefined, an error with reason
`undef` is generated.

# `atom_to_binary`
*auto-imported* *since OTP 23.0* 

```erlang
-spec atom_to_binary(Atom) -> binary() when Atom :: atom().
```

# `atom_to_binary`
*auto-imported* 

```erlang
-spec atom_to_binary(Atom, Encoding) -> binary()
                        when Atom :: atom(), Encoding :: latin1 | unicode | utf8.
```

Returns a binary corresponding to the text representation of `Atom`.

If `Encoding` is `latin1`, each character in the text representation
is stored as a single byte.  If `Encoding` is `utf8` or `unicode`, the
characters are encoded using UTF-8, where some characters may require
multiple bytes.

> #### Change {: .info }
>
> As from Erlang/OTP 20, atoms can contain any Unicode character and
> [`atom_to_binary(Atom, latin1)`](`atom_to_binary/2`) may fail if the text
> representation for `Atom` contains a Unicode character > 255.

## Examples

```erlang
1> atom_to_binary('Erlang', latin1).
<<"Erlang">>
2> atom_to_binary('π', unicode).
<<207,128>>
3> atom_to_binary('π', latin1).
** exception error: bad argument
     in function  atom_to_binary/2
        called as atom_to_binary('π',latin1)
        *** argument 1: contains a character not expressible in latin1
```

# `atom_to_list`
*auto-imported* 

```erlang
-spec atom_to_list(Atom) -> string() when Atom :: atom().
```

Returns a list of unicode code points corresponding to the text representation
of `Atom`.

See the `m:unicode` module for instructions on converting the resulting list into
different formats.

## Examples

```erlang
1> atom_to_list('Erlang').
"Erlang"
2> atom_to_list('π').
[960]
3> atom_to_list('你好').
[20320,22909]
```

# `binary_part`
*auto-imported* *allowed in guard tests* *since OTP R14B* 

```erlang
-spec binary_part(Subject, PosLen) -> binary()
                     when
                         Subject :: binary(),
                         PosLen :: {Start :: non_neg_integer(), Length :: integer()}.
```

# `binary_part`
*auto-imported* *allowed in guard tests* *since OTP R14B* 

```erlang
-spec binary_part(Subject, Start, Length) -> binary()
                     when Subject :: binary(), Start :: non_neg_integer(), Length :: integer().
```

Extracts the part of the binary described by `Start` and `Length`.

A negative length can be used to extract bytes at the end of a binary.

`Start` is zero-based.

Failure: `badarg` if `Start` and `Length` in any way reference
outside the binary.

For details about the semantics of `Start` and `Length`, see
`binary:part/3`.

## Examples

```erlang
1> Bin = <<1,2,3,4,5,6,7,8,9,10>>.
2> binary_part(Bin, 0, 2).
<<1,2>>
3> binary_part(Bin, 2, 3).
<<3,4,5>>
4> binary_part(Bin, byte_size(Bin), -5).
<<6,7,8,9,10>>
```

# `binary_to_atom`
*auto-imported* *since OTP 23.0* 

```erlang
-spec binary_to_atom(Binary) -> atom() when Binary :: binary().
```

# `binary_to_atom`
*auto-imported* 

```erlang
-spec binary_to_atom(Binary, Encoding) -> atom()
                        when Binary :: binary(), Encoding :: latin1 | unicode | utf8.
```

Returns the atom whose text representation is `Binary`, creating a new
atom if necessary.

If `Encoding` is `utf8` or `unicode`, the binary must contain valid
UTF-8 sequences.

> #### Note {: .info }
>
> Note that once an atom is created, it cannot be deleted.
> The Erlang system has a
> [configurable limit](`e:system:system_limits.md#atoms`)
> on the number of atoms that can exist.
> To avoid reaching this limit, consider whether
> [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) is a better choice
> than [`binary_to_atom/2`](`binary_to_atom/2`).
>
> The number of characters that are permitted in an atom name is
> [limited](`e:system:system_limits.md#atom_name_limit`).

> #### Change {: .info }
>
> As from Erlang/OTP 20, [`binary_to_atom(Binary, utf8)`](`binary_to_atom/2`) is
> capable of decoding any Unicode character. Earlier versions would fail if the
> binary contained Unicode characters > 255.

## Examples

```erlang
1> binary_to_atom(<<"Erlang">>, latin1).
'Erlang'
2> binary_to_atom(<<960/utf8>>, utf8).
'π'
```

# `binary_to_existing_atom`
*auto-imported* *since OTP 23.0* 

```erlang
-spec binary_to_existing_atom(Binary) -> atom() when Binary :: binary().
```

# `binary_to_existing_atom`
*auto-imported* 

```erlang
-spec binary_to_existing_atom(Binary, Encoding) -> atom()
                                 when Binary :: binary(), Encoding :: latin1 | unicode | utf8.
```

Returns the atom whose text representation is `Binary` provided that such
atom already exists.

The Erlang system has a [configurable
limit](`e:system:system_limits.md#atoms`) for the total number of
atoms that can exist. Once an atom is created, it cannot be
deleted. Therefore, it is not safe to create many atoms from binaries
that come from an untrusted source (for example, a file fetched from
the Internet), for example, using `binary_to_atom/2`. This function is
thus the appropriate option when the input binary comes from an
untrusted source.

An atom exists in an Erlang system when included in a loaded Erlang module or
when created programmatically (for example, by
[`binary_to_atom/2`](`binary_to_atom/2`)). See the next note for an example of
when an atom exists in the source code for an Erlang module but not in the
compiled version of the same module.

Failure: `badarg` if the atom does not exist.

> #### Note {: .info }
>
> Note that the compiler may optimize away atoms. For example, the compiler will
> rewrite [`atom_to_list(some_atom)`](`atom_to_list/1`) to `"some_atom"`. If
> that expression is the only mention of the atom `some_atom` in the containing
> module, the atom will not be created when the module is loaded, and a
> subsequent call to
> [`binary_to_existing_atom(<<"some_atom">>, utf8)`](`binary_to_existing_atom/2`)
> will fail.

> #### Note {: .info }
>
> The number of characters that are permitted in an atom name is limited. The
> default limits can be found in the
> [Efficiency Guide (section System Limits)](`e:system:system_limits.md`).

## Examples

```erlang
1> binary_to_existing_atom(~"definitely_not_existing_at_all", utf8).
** exception error: bad argument
     in function  binary_to_existing_atom/2
        called as binary_to_existing_atom(<<"definitely_not_existing_at_all">>,utf8)
        *** argument 1: not an already existing atom
2> hello.
hello
3> binary_to_existing_atom(~"hello", utf8).
hello
```

# `binary_to_float`
*auto-imported* *since OTP R16B* 

```erlang
-spec binary_to_float(Binary) -> float() when Binary :: binary().
```

Returns the float whose text representation is `Binary`.

The float string format is the same as the format for
[Erlang float literals](`e:system:data_types.md`), except that underscores
are not permitted.

Failure: `badarg` if `Binary` contains an invalid representation of a float.

## Examples

```erlang
1> binary_to_float(~"10.5").
10.5
2> binary_to_float(~"17.0").
17.0
3> binary_to_float(<<"2.2017764e+1">>).
22.017764
```

# `binary_to_integer`
*auto-imported* *since OTP R16B* 

```erlang
-spec binary_to_integer(Binary) -> integer() when Binary :: binary().
```

Returns an integer whose text representation is `Binary`.

[`binary_to_integer/1`](`binary_to_integer/1`) accepts the same string formats
as `list_to_integer/1`.

Failure: `badarg` if `Binary` contains an invalid representation of an integer.

## Examples

```erlang
1> binary_to_integer(<<"123">>).
123
2> binary_to_integer(<<"-99">>).
-99
3> binary_to_integer(<<"+33">>).
33
```

# `binary_to_integer`
*auto-imported* *since OTP R16B* 

```erlang
-spec binary_to_integer(Binary, Base) -> integer() when Binary :: binary(), Base :: 2..36.
```

Returns an integer whose text representation in base `Base` is `Binary`.

## Example

```erlang
1> binary_to_integer(<<"3FF">>, 16).
1023
2> binary_to_integer(<<"101">>, 2).
5
```

[`binary_to_integer/2`](`binary_to_integer/2`) accepts the same string formats
as `list_to_integer/2`.

Failure: `badarg` if `Binary` contains a invalid representation of an integer.

# `binary_to_list`
*auto-imported* 

```erlang
-spec binary_to_list(Binary) -> [byte()] when Binary :: binary().
```

Returns a list of integers corresponding to the bytes of `Binary`.

## Examples

```erlang
1> binary_to_list(<<1,2,3>>).
[1,2,3]
```

# `binary_to_list`
*auto-imported* 

```erlang
-spec binary_to_list(Binary, Start, Stop) -> [byte()]
                        when Binary :: binary(), Start :: pos_integer(), Stop :: pos_integer().
```

As [`binary_to_list/1`](`binary_to_list/1`), but returns a list of integers
corresponding to the bytes from position `Start` to position `Stop` in `Binary`.

The positions in the binary are numbered starting from 1.

> #### Note {: .info }
>
> **The one-based indexing for binaries used by this function is deprecated.** New
> code should use `binary:bin_to_list/3`. All functions in
> module `binary` consistently use zero-based indexing.

## Examples

```erlang
1> binary_to_list(~"abcdef", 2, 3).
"bc"
```

# `binary_to_term`
*auto-imported* 

```erlang
-spec binary_to_term(Binary) -> term() when Binary :: ext_binary().
```

Returns an Erlang term that is the result of decoding binary object `Binary`,
which must be encoded according to the
[Erlang external term format](erl_ext_dist.md).

> #### Warning {: .warning }
>
> When decoding binaries from untrusted sources, the untrusted source may submit
> data in a way to create resources, such as atoms and remote references, that
> cannot be garbage collected and lead to a Denial of Service (DoS) attack.
> In such cases, use [`binary_to_term/2`](`binary_to_term/2`) with the `safe`
> option.

## Examples

```erlang
1> Bin = term_to_binary(hello).
<<131,119,5,104,101,108,108,111>>
2> hello = binary_to_term(Bin).
hello
```

See also `term_to_binary/1` and `binary_to_term/2`.

# `binary_to_term`
*auto-imported* *since OTP R13B04* 

```erlang
-spec binary_to_term(Binary, Opts) -> term() | {term(), Used}
                        when
                            Binary :: ext_binary(),
                            Opt :: safe | used,
                            Opts :: [Opt],
                            Used :: pos_integer().
```

Equivalent to [`binary_to_term(Binary)`](`binary_to_term/1`), but can be configured to
fit special purposes.

The allowed options are:

- **`safe`** - Use this option when receiving binaries from an untrusted source.

  When enabled, it prevents decoding data that can be used to attack the Erlang
  runtime. In the event of receiving unsafe data, decoding fails with a `badarg`
  error.

  The `safe` option prevents direct and indirect creation of new atoms
  (such as those embedded in certain structures like process
  identifiers) and creation of new external function references. None
  of these resources are garbage collected, so unchecked creation can
  exhaust available memory.

  > #### Warning {: .warning }
  >
  > The `safe` option ensures data is safely processed by the Erlang runtime,
  > but it does not guarantee that the data is safe for your application.
  > Always validate data from untrusted sources. If a binary is stored or
  > transmitted through untrusted sources, consider cryptographically signing it.

  ## Examples

  ```erlang
  1> Bin = <<131,119,8,"tjenixen">>.
  2> binary_to_term(Bin, [safe]).
  ** exception error: bad argument
       in function  binary_to_term/2
          called as binary_to_term(<<131,119,8,116,106,101,110,105,120,101,110>>,[safe])
          *** argument 1: invalid or unsafe external representation of a term
  3> tjenixen.
  tjenixen
  4> binary_to_term(Bin, [safe]).
  tjenixen
  ```

- **`used`** - Changes the return value to `{Term, Used}` where `Used` is the
  number of bytes actually read from `Binary`.

  ## Examples

  ```erlang
  1> Input = <<(term_to_binary(hello))/binary, "world">>.
  <<131,119,5,104,101,108,108,111,119,111,114,108,100>>
  2> {Term, Used} = binary_to_term(Input, [used]).
  {hello, 8}
  3> split_binary(Input, Used).
  {<<131,119,5,104,101,108,108,111>>, <<"world">>}
  ```

Failure: `badarg` if `safe` is specified and unsafe data is decoded.

See also `term_to_binary/1`, `binary_to_term/1`, and `list_to_existing_atom/1`.

# `bit_size`
*auto-imported* *allowed in guard tests* 

```erlang
-spec bit_size(Bitstring) -> non_neg_integer() when Bitstring :: bitstring().
```

Returns an integer that is the size in bits of `Bitstring`.

## Examples

```erlang
1> bit_size(<<433:16,3:3>>).
19
2> bit_size(<<1,2,3>>).
24
```

# `bitstring_to_list`
*auto-imported* 

```erlang
-spec bitstring_to_list(Bitstring) -> [byte() | bitstring()] when Bitstring :: bitstring().
```

Returns a list of integers corresponding to the bytes of `Bitstring`.

If the number of bits in the binary is not a multiple of 8, the last element of
the list is a bitstring containing the remaining 1 to 7 bits.

## Examples

```erlang
1> bitstring_to_list(<<433:16>>).
[1,177]
2> bitstring_to_list(<<433:16,3:3>>).
[1,177,<<3:3>>]
```

# `bump_reductions`

```erlang
-spec bump_reductions(Reductions) -> true when Reductions :: pos_integer().
```

This implementation-dependent function increments the reduction counter for the
calling process.

In the Beam emulator, the reduction counter is normally incremented by one for
each function and BIF call. A context switch is forced when the counter reaches
the maximum number of reductions for a process (4000 reductions in Erlang/OTP 19.2 and later).

> #### Warning {: .warning }
>
> This BIF can be removed in a future version of the Beam machine without prior
> warning. It is unlikely to be implemented in other Erlang implementations.

# `byte_size`
*auto-imported* *allowed in guard tests* 

```erlang
-spec byte_size(Bitstring) -> non_neg_integer() when Bitstring :: bitstring().
```

Returns an integer that is the number of bytes needed to contain `Bitstring`.

If the number of bits in `Bitstring` is not a multiple of 8, the
result is rounded **up**.

## Examples

```erlang
1> byte_size(<<433:16,3:3>>).
3
2> byte_size(<<1,2,3,4>>).
4
```

# `cancel_timer`

```erlang
-spec cancel_timer(TimerRef) -> Result
                      when TimerRef :: reference(), Time :: non_neg_integer(), Result :: Time | false.
```

# `cancel_timer`
*since OTP 18.0* 

```erlang
-spec cancel_timer(TimerRef, Options) -> Result | ok
                      when
                          TimerRef :: reference(),
                          Async :: boolean(),
                          Info :: boolean(),
                          Option :: {async, Async} | {info, Info},
                          Options :: [Option],
                          Time :: non_neg_integer(),
                          Result :: Time | false.
```

Cancels a timer that has been created by [`erlang:start_timer`](`start_timer/4`)
or [`erlang:send_after`](`send_after/4`). `TimerRef` identifies the timer, and
was returned by the BIF that created the timer.

`Option`s:

- **`{async, Async}`** - Asynchronous request for cancellation. `Async` defaults
  to `false`, which causes the cancellation to be performed synchronously. When
  `Async` is set to `true`, the cancel operation is performed asynchronously.
  That is, `cancel_timer()` sends an asynchronous request for cancellation to
  the timer service that manages the timer, and then returns `ok`.

- **`{info, Info}`** - Requests information about the `Result` of the
  cancellation. `Info` defaults to `true`, which means the `Result` is given.
  When `Info` is set to `false`, no information about the result of the
  cancellation is given.

  - When `Async` is `false`: if `Info` is `true`, the `Result` is returned by
    `erlang:cancel_timer()`. otherwise `ok` is returned.
  - When `Async` is `true`: if `Info` is `true`, a message on the form
    `{cancel_timer, TimerRef, Result}` is sent to the caller of
    `erlang:cancel_timer()` when the cancellation operation has been performed,
    otherwise no message is sent.

More `Option`s may be added in the future.

If `Result` is an integer, it represents the time in milliseconds left until the
canceled timer would have expired.

If `Result` is `false`, a timer corresponding to `TimerRef` could not be found.
This can be either because the timer had expired, already had been canceled, or
because `TimerRef` never corresponded to a timer. Even if the timer had expired,
it does not tell you if the time-out message has arrived at its destination yet.

> #### Note {: .info }
>
> The timer service that manages the timer can be co-located with another
> scheduler than the scheduler that the calling process is executing on. If so,
> communication with the timer service takes much longer time than if it is
> located locally. If the calling process is in critical path, and can do other
> things while waiting for the result of this operation, or is not interested in
> the result of the operation, you want to use option `{async, true}`. If using
> option `{async, false}`, the calling process blocks until the operation has
> been performed.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:start_timer/4`](`start_timer/4`), and
[`erlang:read_timer/2`](`read_timer/2`).

# `ceil`
*auto-imported* *allowed in guard tests* *since OTP 20.0* 

```erlang
-spec ceil(Number) -> integer() when Number :: number().
```

Returns the smallest integer not less than `Number`.

See also `trunc/1`.

## Examples

```erlang
1> ceil(5.5).
6
2> ceil(-2.3).
-2
3> ceil(10.0).
10
```

# `check_old_code`
*auto-imported* *since OTP R14B04* 

```erlang
-spec check_old_code(Module) -> boolean() when Module :: module().
```

Returns `true` if `Module` has
[old code](`e:system:code_loading.md#code-replacement`), otherwise `false`.

See also `m:code`.

# `check_process_code`
*auto-imported* 

```erlang
-spec check_process_code(Pid, Module) -> CheckResult
                            when Pid :: pid(), Module :: module(), CheckResult :: boolean().
```

# `check_process_code`
*auto-imported* *since OTP 17.0* 

```erlang
-spec check_process_code(Pid, Module, OptionList) -> CheckResult | async
                            when
                                Pid :: pid(),
                                Module :: module(),
                                RequestId :: term(),
                                Option :: {async, RequestId} | {allow_gc, boolean()},
                                OptionList :: [Option],
                                CheckResult :: boolean() | aborted.
```

Checks if the node local process identified by `Pid` executes old code for
`Module`.

`Option`s:

- **`{allow_gc, boolean()}`** - Determines if garbage collection is allowed when
  performing the operation. If `{allow_gc, false}` is passed, and a garbage
  collection is needed to determine the result of the operation, the operation
  is aborted (see information on `CheckResult` below). The default is to allow
  garbage collection, that is, `{allow_gc, true}`.

- **`{async, RequestId}`** - The function
  [`check_process_code/3`](`check_process_code/3`) returns the value `async`
  immediately after the request has been sent. When the request has been
  processed, the process that called this function is passed a message on the
  form `{check_process_code, RequestId, CheckResult}`.

If `Pid` equals `self/0`, and no `async` option has been passed, the operation
is performed at once. Otherwise a request for the operation is sent to the
process identified by `Pid`, and is handled when appropriate. If no `async`
option has been passed, the caller blocks until `CheckResult` is available and
can be returned.

`CheckResult` informs about the result of the request as follows:

- **`true`** - The process identified by `Pid` executes old code for `Module`.
  That is, the current call of the process executes old code for this module, or
  the process has references to old code for this module, or the process
  contains funs that references old code for this module.

- **`false`** - The process identified by `Pid` does not execute old code for
  `Module`.

- **`aborted`** - The operation was aborted, as the process needed to be garbage
  collected to determine the operation result, and the operation was requested
  by passing option `{allow_gc, false}`.

> #### Change {: .info }
>
> Up until ERTS version 8.\*, the check process code operation checks for all
> types of references to the old code. That is, direct references (e.g. return
> addresses on the process stack), indirect references (`fun`s in process
> context), and references to literals in the code.
>
> As of ERTS version 9.0, the check process code operation only checks for
> direct references to the code. Indirect references via `fun`s will be ignored.
> If such `fun`s exist and are used after a purge of the old code, an exception
> will be raised upon usage (same as the case when the `fun` is received by the
> process after the purge). Literals will be taken care of (copied) at a later
> stage. This behavior can as of ERTS version 8.1 be enabled when
> [building OTP](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`),
> and will automatically be enabled if dirty scheduler support is enabled.

See also `m:code`.

Failures:

- **`badarg`** - If `Pid` is not a node local process identifier.

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `OptionList` is an invalid list of options.

# `convert_time_unit`
*since OTP 18.0* 

```erlang
-spec convert_time_unit(Time, FromUnit, ToUnit) -> ConvertedTime
                           when
                               Time :: integer(),
                               ConvertedTime :: integer(),
                               FromUnit :: time_unit(),
                               ToUnit :: time_unit().
```

Converts the `Time` value of time unit `FromUnit` to the corresponding
`ConvertedTime` value of time unit `ToUnit`. The result is rounded using the
`floor/1` function.

> #### Warning {: .warning }
>
> You can lose accuracy and precision when converting between time units. To
> minimize such loss, collect all data at `native` time unit and do the
> conversion on the end result.

# `crc32`

```erlang
-spec crc32(Data) -> non_neg_integer() when Data :: iodata().
```

Computes and returns the crc32 (IEEE 802.3 style) checksum for `Data`.

## Examples:

```erlang
1> Data = ~"abc".
2> erlang:crc32(Data).
891568578
```

# `crc32`

```erlang
-spec crc32(OldCrc, Data) -> non_neg_integer() when OldCrc :: non_neg_integer(), Data :: iodata().
```

Continues computing the crc32 checksum by combining the previous checksum,
`OldCrc`, with the checksum of `Data`.

The following code:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:crc32(Data1).
891568578
3> Y = erlang:crc32(X,Data2).
1267612143
```

assigns the same value to `Y` as this:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> Y = erlang:crc32([Data1,Data2]).
1267612143
```

# `crc32_combine`

```erlang
-spec crc32_combine(FirstCrc, SecondCrc, SecondSize) -> non_neg_integer()
                       when
                           FirstCrc :: non_neg_integer(),
                           SecondCrc :: non_neg_integer(),
                           SecondSize :: non_neg_integer().
```

Combines two previously computed crc32 checksums.

This computation requires the size of the data object for the second checksum
to be known.

The following code:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:crc32(Data1).
891568578
3> Y = erlang:crc32(X,Data2).
1267612143
```

assigns the same value to `Z` as this:

```erlang
1> Data1 = ~"abc", Data2 = ~"def".
2> X = erlang:crc32(Data1).
891568578
3> Y = erlang:crc32(Data2).
214229345
4> Z = erlang:crc32_combine(X,Y,iolist_size(Data2)).
1267612143
```

# `date`
*auto-imported* 

```erlang
-spec date() -> Date when Date :: calendar:date().
```

Returns the current date as `{Year, Month, Day}`.

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> date().
{1995,2,19}
```

# `decode_packet`

```erlang
-spec decode_packet(Type, Bin, Options) -> {ok, Packet, Rest} | {more, Length} | {error, Reason}
                       when
                           Type ::
                               raw | 0 | 1 | 2 | 4 | asn1 | cdr | sunrm | fcgi | tpkt | line | http |
                               http_bin | httph | httph_bin,
                           Bin :: binary(),
                           Options :: [Opt],
                           Opt :: {packet_size, non_neg_integer()} | {line_length, non_neg_integer()},
                           Packet :: binary() | HttpPacket,
                           Rest :: binary(),
                           Length :: non_neg_integer() | undefined,
                           Reason :: term(),
                           HttpPacket :: HttpRequest | HttpResponse | HttpHeader | http_eoh | HttpError,
                           HttpRequest :: {http_request, HttpMethod, HttpUri, HttpVersion},
                           HttpResponse :: {http_response, HttpVersion, integer(), HttpString},
                           HttpHeader ::
                               {http_header,
                                integer(),
                                HttpField,
                                UnmodifiedField :: HttpString,
                                Value :: HttpString},
                           HttpError :: {http_error, HttpString},
                           HttpMethod ::
                               'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'TRACE' |
                               HttpString,
                           HttpUri ::
                               '*' |
                               {absoluteURI,
                                http | https,
                                Host :: HttpString,
                                Port :: inet:port_number() | undefined,
                                Path :: HttpString} |
                               {scheme, Scheme :: HttpString, HttpString} |
                               {abs_path, HttpString} |
                               HttpString,
                           HttpVersion :: {Major :: non_neg_integer(), Minor :: non_neg_integer()},
                           HttpField ::
                               'Cache-Control' | 'Connection' | 'Date' | 'Pragma' |
                               'Transfer-Encoding' | 'Upgrade' | 'Via' | 'Accept' | 'Accept-Charset' |
                               'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 'From' |
                               'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' |
                               'If-Range' | 'If-Unmodified-Since' | 'Max-Forwards' |
                               'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' |
                               'Location' | 'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' |
                               'Vary' | 'Warning' | 'Www-Authenticate' | 'Allow' | 'Content-Base' |
                               'Content-Encoding' | 'Content-Language' | 'Content-Length' |
                               'Content-Location' | 'Content-Md5' | 'Content-Range' | 'Content-Type' |
                               'Etag' | 'Expires' | 'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie' |
                               'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 'Keep-Alive' |
                               'Proxy-Connection' | HttpString,
                           HttpString :: string() | binary().
```

Decodes the binary `Bin` according to the packet protocol specified by `Type`,
similar to the packet handling done by sockets with the `{packet,Type}` option.

If `Bin` contains an entire packet, it is returned along with the
remainder of the binary as `{ok,Packet,Rest}`.

If `Bin` does not contain the entire packet, `{more,Length}` is returned.
`Length` is either the expected _total size_ of the packet or `undefined` if
the expected packet size is unknown. `decode_packet` can then be called again
with additional data.

If the packet does not conform to the protocol format, `{error,Reason}` is
returned.

`Type`s:

- **`raw | 0`** - No packet handling is done. The entire binary is returned
  unless it is empty.

- **`1 | 2 | 4`** - Packets consist of a header specifying the number of bytes
  in the packet, followed by that number of bytes. The length of the header can
  be one, two, or four bytes; the order of the bytes is big-endian. The header
  is stripped off when the packet is returned.

- **`line`** - A packet is a line-terminated by a delimiter byte, default is the
  latin-1 newline character. The delimiter byte is included in the returned
  packet unless the line was truncated according to option `line_length`.

- **`asn1 | cdr | sunrm | fcgi | tpkt`** - The header is _not_ stripped off.

  The meanings of the packet types are as follows:

  - **`asn1` \- ASN.1 BER**

  - **`sunrm` \- Sun's RPC encoding**

  - **`cdr` \- CORBA (GIOP 1.1)**

  - **`fcgi` \- Fast CGI**

  - **`tpkt` \- TPKT format \[RFC1006]**

- **`http | httph | http_bin | httph_bin`** - The Hypertext Transfer Protocol.
  The packets are returned with the format according to `HttpPacket` described
  earlier. A packet is either a request, a response, a header, or an end of
  header mark. Invalid lines are returned as `HttpError`.

  Recognized request methods and header fields are returned as atoms. Others are
  returned as strings. Strings of unrecognized header fields are formatted with
  only capital letters first and after hyphen characters, for example,
  `"Sec-Websocket-Key"`. Header field names are also returned in
  `UnmodifiedField` as strings, without any conversion or formatting.

  The protocol type `http` is only to be used for the first line when an
  `HttpRequest` or an `HttpResponse` is expected. The following calls are to use
  `httph` to get `HttpHeader`s until `http_eoh` is returned, which marks the end
  of the headers and the beginning of any following message body.

  The variants `http_bin` and `httph_bin` return strings (`HttpString`) as
  binaries instead of lists.

  Since OTP 26.0, `Host` may be an IPv6 address enclosed in `[]`, as defined in
  [RFC2732 ](https://www.ietf.org/rfc/rfc2732.txt).

Options:

- **`{packet_size, integer() >= 0}`** - Sets the maximum allowed size of the
  packet body. If the packet header indicates that the length of the packet is
  longer than the maximum allowed length, the packet is considered invalid.
  Defaults to 0, which means no size limit.

- **`{line_length, integer() >= 0}`** - For packet type `line`, lines longer
  than the indicated length are truncated.

  Option `line_length` also applies to `http*` packet types as an alias for
  option `packet_size` if `packet_size` itself is not set. This use is only
  intended for backward compatibility.

- **`{line_delimiter, 0 =< byte() =< 255}`** - For packet type `line`, sets the
  delimiting byte. Default is the latin-1 character `$\n`.

## Examples

```erlang
1> erlang:decode_packet(1, <<3,"abcd">>, []).
{ok,<<"abc">>,<<"d">>}
2> erlang:decode_packet(1, <<5,"abcd">>, []).
{more,6}
```

# `delete_element`
*since OTP R16B* 

```erlang
-spec delete_element(Index, Tuple1) -> Tuple2
                        when Index :: pos_integer(), Tuple1 :: tuple(), Tuple2 :: tuple().
```

Returns a new tuple with element at `Index` removed from tuple `Tuple1`.

## Examples

```erlang
1> erlang:delete_element(2, {one, two, three}).
{one,three}
```

# `delete_module`
*auto-imported* 

```erlang
-spec delete_module(Module) -> true | undefined when Module :: module().
```

Makes the current code for `Module` become old code and deletes all references
for this module from the export table. Returns `undefined` if the module does
not exist, otherwise `true`.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.

Failure: `badarg` if there already is an old version of `Module`.

# `demonitor`
*auto-imported* 

```erlang
-spec demonitor(MonitorRef) -> true when MonitorRef :: reference().
```

If `MonitorRef` is a reference that the calling process obtained by calling
`monitor/2`, this monitoring is turned off. If the monitoring is already turned
off, nothing happens.

Once [`demonitor(MonitorRef)`](`demonitor/1`) has returned, it is guaranteed
that no `{'DOWN', MonitorRef, _, _, _}` message, because of the monitor, will be
placed in the caller message queue in the future. However, a
`{'DOWN', MonitorRef, _, _, _}` message can have been placed in the caller
message queue before the call. It is therefore usually advisable to remove such
a `'DOWN'` message from the message queue after monitoring has been stopped.
[`demonitor(MonitorRef, [flush])`](`demonitor/2`) can be used instead of
[`demonitor(MonitorRef)`](`demonitor/1`) if this cleanup is wanted.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

> #### Change {: .info }
>
> Before Erlang/OTP R11B (ERTS 5.5) [`demonitor/1`](`demonitor/1`) behaved
> completely asynchronously, that is, the monitor was active until the
> "demonitor signal" reached the monitored entity. This had one undesirable
> effect. You could never know when you were guaranteed _not_ to receive a
> `DOWN` message because of the monitor.
>
> The current behavior can be viewed as two combined operations: asynchronously
> send a "demonitor signal" to the monitored entity and ignore any future
> results of the monitor.

Failure: It is an error if `MonitorRef` refers to a monitoring started by
another process. Not all such cases are cheap to check. If checking is cheap,
the call fails with `badarg`, for example if `MonitorRef` is a remote reference.

# `demonitor`
*auto-imported* 

```erlang
-spec demonitor(MonitorRef, OptionList) -> boolean()
                   when MonitorRef :: reference(), OptionList :: [Option], Option :: flush | info.
```

The returned value is `true` unless `info` is part of `OptionList`.

[`demonitor(MonitorRef, [])`](`demonitor/2`) is equivalent to
[`demonitor(MonitorRef)`](`demonitor/1`).

`Option`s:

- **`flush`** - Removes (one) `{_, MonitorRef, _, _, _}` message, if there is
  one, from the caller message queue after monitoring has been stopped.

  Calling [`demonitor(MonitorRef, [flush])`](`demonitor/2`) is equivalent to the
  following, but more efficient:

  ```erlang
  demonitor(MonitorRef),
  receive
      {_, MonitorRef, _, _, _} ->
          true
  after 0 ->
          true
  end
  ```

- **`info`** - The returned value is one of the following:

  - **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
    message corresponding to this monitor has been delivered and will not be
    delivered.

  - **`false`** - The monitor was not found and could not be removed. This
    probably because someone already has placed a `'DOWN'` message corresponding
    to this monitor in the caller message queue.

  If option `info` is combined with option `flush`, `false` is returned if a
  flush was needed, otherwise `true`.

> #### Change {: .info }
>
> More options can be added in a future release.

Failures:

- **`badarg`** - If `OptionList` is not a list.

- **`badarg`** - If `Option` is an invalid option.

- **`badarg`** - The same failure as for `demonitor/1`.

# `disconnect_node`
*auto-imported* 

```erlang
-spec disconnect_node(Node) -> boolean() | ignored when Node :: node().
```

Forces the disconnection of a node.

Doing this makes it appears to the node `Node` as if the local node has crashed.
This BIF is mainly used in the Erlang network authentication protocols.

Returns `true` if disconnection succeeds, otherwise `false`. If the local node
is not alive, `ignored` is returned.

> #### Note {: .info }
>
> This function may return before [`nodedown` messages](`monitor_node/2`) have
> been delivered.

# `display`

```erlang
-spec display(Term) -> true when Term :: term().
```

Prints a text representation of `Term` on the standard output.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only. The printed representation may
> contain internal details that do not match the high-level representation of
> the term in Erlang.

# `dist_ctrl_get_data`
*since OTP 21.0* 

```erlang
-spec dist_ctrl_get_data(DHandle) -> {Size, Data} | Data | none
                            when Size :: non_neg_integer(), DHandle :: dist_handle(), Data :: iovec().
```

Get distribution channel data from the local node that is to be passed to the
remote node.

The distribution channel is identified by `DHandle`. If no data is
available, the atom `none` is returned. One can request to be informed by a
message when more data is available by calling
[`erlang:dist_ctrl_get_data_notification(DHandle)`](`dist_ctrl_get_data_notification/1`).

The returned value when there are data available depends on the value of the
`get_size` option configured on the distribution channel identified by
`DHandle`. For more information see the documentation of the `get_size` option
for the [`erlang:dist_ctrl_set_opt/3`](`dist_ctrl_set_opt/3`) function.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `dist_ctrl_get_data_notification`
*since OTP 21.0* 

```erlang
-spec dist_ctrl_get_data_notification(DHandle) -> ok when DHandle :: dist_handle().
```

Request notification when more data is available to fetch using
[`erlang:dist_ctrl_get_data(DHandle)`](`dist_ctrl_get_data/1`) for the
distribution channel identified by `DHandle`.

When more data is present, the caller will be sent the message `dist_data`.
Once a `dist_data` messages has been sent, no more `dist_data` messages will
be sent until the [`dist_ctrl_get_data_notification/1`](`dist_ctrl_get_data_notification/1`)
function has been called again.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `dist_ctrl_get_opt`
*since OTP 22.0* 

```erlang
-spec dist_ctrl_get_opt(DHandle, get_size) -> Value when DHandle :: dist_handle(), Value :: boolean().
```

Returns the value of the `get_size` option on the distribution channel
identified by `DHandle`. For more information see the documentation of the
`get_size` option for the [`erlang:dist_ctrl_set_opt/3`](`dist_ctrl_set_opt/3`)
function.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `dist_ctrl_input_handler`
*since OTP 21.0* 

```erlang
-spec dist_ctrl_input_handler(DHandle, InputHandler) -> ok
                                 when DHandle :: dist_handle(), InputHandler :: pid().
```

Register an alternate input handler process for the distribution channel
identified by `DHandle`.

Once this function has been called, `InputHandler` is the only process allowed to call
[`erlang:dist_ctrl_put_data(DHandle, Data)`](`dist_ctrl_put_data/2`) with the
`DHandle` identifying this distribution channel.

> #### Note {: .info }
>
> When the distribution controller for the distribution channel identified by
> `DHandle` is a process, it is the only process allowed to call this function.
> This function is also allowed to be called when the distribution controller
> for the distribution channel identified by `DHandle` is a port. The data
> received by the port should in this case be delivered to the process
> identified by `InputHandler` which in turn should call
> [`erlang:dist_ctrl_put_data/2`](`dist_ctrl_put_data/2`).

This function is used when implementing an alternative distribution carrier.
`DHandle` is retrieved via the callback
[`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete). More
information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `dist_ctrl_put_data`
*since OTP 21.0* 

```erlang
-spec dist_ctrl_put_data(DHandle, Data) -> ok when DHandle :: dist_handle(), Data :: iodata().
```

Deliver distribution channel data from a remote node to the local node.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function unless an
> alternate input handler process has been registered using
> [`erlang:dist_ctrl_input_handler(DHandle, InputHandler)`](`dist_ctrl_input_handler/2`).
> If an alternate input handler has been registered, only the registered input
> handler process is allowed to call this function.

This function is used when implementing an alternative distribution carrier.
`DHandle` is retrieved via the callback
[`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete). More
information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `dist_ctrl_set_opt`
*since OTP 22.0* 

```erlang
-spec dist_ctrl_set_opt(DHandle, get_size, Value) -> OldValue
                           when DHandle :: dist_handle(), Value :: boolean(), OldValue :: boolean().
```

Sets the value of the `get_size` option on the distribution channel identified
by `DHandle`.

This option controls the return value of calls to
[erlang:dist_ctrl_get_data(DHandle)](`dist_ctrl_get_data/1`) where `DHandle`
equals `DHandle` used when setting this option. When the `get_size` option is:

- **`false`** - and there are distribution data available, a call to
  `erlang:dist_ctrl_get_data(DHandle)` will just return `Data` to pass over the
  channel. This is the default value of the `get_size` option.

- **`true`** - and there are distribution data available, a call to
  `erlang:dist_ctrl_get_data(DHandle)` will return `Data` to pass over the
  channel as well as the `Size` of `Data` in bytes. This is returned as a tuple
  of the form `{Size, Data}`.

All options are set to default when a channel is closed.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

# `element`
*auto-imported* *allowed in guard tests* 

```erlang
-spec element(N, Tuple) -> term() when N :: pos_integer(), Tuple :: tuple().
```

Returns the `N`th element (numbering from 1) of `Tuple`.

## Examples

```erlang
1> element(2, {a, b, c}).
b
```

# `erase`
*auto-imported* 

```erlang
-spec erase() -> [{Key, Val}] when Key :: term(), Val :: term().
```

Returns the process dictionary and deletes it.

For example:

```erlang
1> put(key1, {1, 2, 3}).
2> put(key2, [a, b, c]).
3> lists:sort(erase()).
[{key1,{1,2,3}},{key2,[a,b,c]}]
```

# `erase`
*auto-imported* 

```erlang
-spec erase(Key) -> Val | undefined when Key :: term(), Val :: term().
```

Returns the value `Val` associated with `Key` and deletes it from the process
dictionary. Returns `undefined` if no value is associated with `Key`.

The average time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
1> put(key1, {merry, lambs, are, playing}).
2> X = erase(key1).
{merry,lambs,are,playing}
3> erase(key1).
undefined
```

# `error`
*auto-imported* 

```erlang
-spec error(Reason) -> no_return() when Reason :: term().
```

Raises an exception of class `error` with the reason `Reason`.

As evaluating this function causes an exception to be thrown, it has no return value.

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.
Example:

```erlang
1> catch error(foobar).
{'EXIT',{foobar, _StackTrace}}
```

# `error`
*auto-imported* 

```erlang
-spec error(Reason, Args) -> no_return() when Reason :: term(), Args :: [term()] | none.
```

Raises an exception of class `error` with the reason `Reason`. `Args` is
expected to be the list of arguments for the current function or the atom
`none`.

If `Args` is a list, it is used to provide the arguments for the current
function in the stack back-trace. If it is `none`, the arity of the calling
function is used in the stacktrace. As evaluating this function causes an
exception to be raised, it has no return value.

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.

## Examples

`test.erl`:

```erlang
-module(test).
-export([example_fun/2]).

example_fun(A1, A2) ->
    erlang:error(my_error, [A1, A2]).
```

Erlang shell:

```erlang
1> test:example_fun(arg1, "this is the second argument").
** exception error: my_error
     in function  test:example_fun/2
        called as test:example_fun(arg1,"this is the second argument")
```

# `error`
*auto-imported* *since OTP 24.0* 

```erlang
-spec error(Reason, Args, Options) -> no_return()
               when
                   Reason :: term(),
                   Args :: [term()] | none,
                   Options :: [Option],
                   Option :: {error_info, ErrorInfoMap},
                   ErrorInfoMap :: #{cause => term(), module => module(), function => atom()}.
```

Raises an exception of class `error` with the reason `Reason`. `Args` is
expected to be the list of arguments for the current function or the atom
`none`.

If `Args` is a list, it is used to provide the arguments for the current
function in the stack back-trace. If it is `none`, the arity of the calling
function is used in the stacktrace. As evaluating this function causes an
exception to be raised, it has no return value.

If the `error_info` option is given, the `ErrorInfoMap` will be inserted into
the stacktrace. The information given in the `ErrorInfoMap` is to be used by
error formatters such as [`erl_error`](`erl_error:format_exception/4`) to
provide more context around an error.

The default `module` of the `ErrorInfoMap` is the module that the call to
`error/3` is made. The default `function` is `format_error`. See
[`format_error/2`](`c:erl_error:format_error/2`) for more details on how this
Module:Function/2 is to be used

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.

# `exit`
*auto-imported* 

```erlang
-spec exit(Reason) -> no_return() when Reason :: term().
```

Raises an exception of class `exit` with exit reason `Reason`.

As evaluating this function causes an exception to be raised, it has no return value.

The intent of the exception class `exit` is that the current process should be
stopped (for example when a message telling a process to stop is received).

This function differ from [`error/1,2,3`](`error/1`) by causing an exception of
a different class and by having a reason that does not include the list of
functions from the call stack.

See the guide about [errors and error handling](`e:system:errors.md`) for
additional information.

Example:

```erlang
1> exit(foobar).
** exception exit: foobar
2> catch exit(foobar).
{'EXIT',foobar}
```

> #### Note {: .info }
>
> If a process calls [`exit(kill)`](`exit/1`) and does not catch the exception,
> it will terminate with exit reason `kill` and also emit exit signals with exit
> reason `kill` (not `killed`) to all linked processes. Such exit signals with
> exit reason `kill` can be trapped by the linked processes. Note that this
> means that signals with exit reason `kill` behave differently depending on how
> they are sent because the signal will be untrappable if a process sends such a
> signal to another process with [`erlang:exit_signal/2`](`exit_signal/2`).

# `exit_signal`
*auto-imported* *since OTP 29.0* 

```erlang
-spec exit_signal(Pid, Reason) -> true when Pid :: pid() | port() | reference(), Reason :: term().
```

Sends an exit signal with exit reason `Reason` to the process or port identified
by `Dest`. If `Dest` is a reference, the exit signal will *only* affect the
identified process if the reference is an active
[process alias](`e:system:ref_man_processes.md#process-aliases`) of a process
executing on an OTP 28.0 node or newer.

Let `P` be the process or port identified by `Dest`. The following behavior
applies if `Reason` is any term except `normal` or `kill`:

- If `P` is not [trapping exits](`process_flag/2`), `P` exits with exit reason
  `Reason`.
- If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
  into a message `{'EXIT', From, Reason}`, where `From` is the process
  identifier of the process that sent the exit signal, and delivered to the
  message queue of `P`.

The following behavior applies if `Reason` is the term `normal`:

- The signal has no effect if `P` is not trapping exits.
- If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
  into a message `{'EXIT', From, normal}`, where `From` is the process
  identifier of the process that sent the exit signal, and delivered to `P`'s
  message queue.

If `Reason` is the atom `kill`, that is, if [`exit(Dest, kill)`](`exit_signal/2`) is
called, an untrappable exit signal is sent to the process that is identified by
`Dest`, which unconditionally exits with exit reason `killed`. The exit reason is
changed from `kill` to `killed` to hint to linked processes that the killed
process got killed by a call to [`exit(Dest, kill)`](`exit_signal/2`).

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

# `exit_signal`
*auto-imported* *since OTP 29.0* 

```erlang
-spec exit_signal(Dest, Reason, OptList) -> true
                     when Dest :: pid() | port() | reference(), Reason :: term(), OptList :: [priority].
```

Provides an option list for modification of the functionality provided by the
`exit_signal/2` BIF. The `Dest` and `Reason` arguments has the same meaning as when
passed to `exit_signal/2`.

Currently available options:

- **`priority`** -- Since OTP 28.0

  Send this exit signal as a priority exit signal. In order for
  the signal to be handled as a
  [priority `EXIT` message](`e:system:ref_man_processes.md#priority-messages`)
  by the receiver, this option *must* be passed, `Dest` *must* be an active
  [*priority alias*](#priority_alias) and the receiver *must* be
  [trapping exits](#process_flag_trap_exit).

  If `Dest` is an active priority alias, but this option is not passed, the exit
  signal will be handled as on ordinary exit signal. The same is true, if this
  option is passed, but `Dest` is not an active priority alias.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  and the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  sections of the _Erlang Reference Manual_.

# `external_size`
*since OTP R14B04* 

```erlang
-spec external_size(Term) -> non_neg_integer() when Term :: term().
```

Calculates, without doing the encoding, the maximum byte size for a term encoded
in the Erlang external term format.

The following condition applies always:

```erlang
Size1 = byte_size(term_to_binary(Term)),
Size2 = erlang:external_size(Term),
true = Size1 =< Size2.
```

## Examples

```erlang
1> Term = {ok,"abc"}.
2> erlang:external_size(Term).
13
3> byte_size(term_to_binary(Term)).
13
```

# `external_size`
*since OTP R14B04* 

```erlang
-spec external_size(Term, Options) -> non_neg_integer()
                       when
                           Term :: term(),
                           Options ::
                               [compressed |
                                {compressed, Level :: 0..9} |
                                deterministic |
                                {minor_version, Version :: 0..2} |
                                local].
```

Calculates, without doing the encoding, the maximum byte size for a term encoded
in the Erlang external term format.

The following condition applies always:

```erlang
Size1 = byte_size(term_to_binary(Term, Options)),
Size2 = erlang:external_size(Term, Options),
true = Size1 =< Size2.
```

See `term_to_binary/2` for a description of the options.

## Examples

```erlang
1> Term = {ok,lists:duplicate(50, $A)}.
{ok,"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
2> erlang:external_size(Term, [compressed]).
60
3> byte_size(term_to_binary(Term, [compressed])).
26
```

# `float`
*auto-imported* *allowed in guard tests* 

```erlang
-spec float(Number) -> float() when Number :: number().
```

Returns a float by converting `Number` to a float.

## Examples

```erlang
1> float(55).
55.0
```

> #### Note {: .info }
>
> If used on the top level in a guard, it tests whether the argument is a
> floating point number; for clarity, use `is_float/1` instead.
>
> When [`float/1`](`float/1`) is used in an expression in a guard, such as
> '`float(A) == 4.0`', it converts a number as described earlier.

# `float_to_binary`
*auto-imported* *since OTP R16B* 

```erlang
-spec float_to_binary(Float) -> binary() when Float :: float().
```

# `float_to_binary`
*auto-imported* *since OTP R16B* 

```erlang
-spec float_to_binary(Float, Options) -> binary()
                         when
                             Float :: float(),
                             Options :: [Option],
                             Option ::
                                 {decimals, Decimals :: 0..253} |
                                 {scientific, Decimals :: 0..249} |
                                 compact | short.
```

Returns a binary corresponding to the text representation of `Float` using fixed
decimal point formatting.

`Options` behaves in the same way as `float_to_list/2`.

## Examples

```erlang
1> float_to_binary(7.12, [{decimals, 4}]).
<<"7.1200">>
2> float_to_binary(7.12, [{decimals, 4}, compact]).
<<"7.12">>
3> float_to_binary(7.12, [{scientific, 3}]).
<<"7.120e+00">>
4> float_to_binary(7.12, [short]).
<<"7.12">>
5> float_to_binary(0.1+0.2, [short]).
<<"0.30000000000000004">>
6> float_to_binary(0.1+0.2).
<<"3.00000000000000044409e-01">>
```

# `float_to_list`
*auto-imported* 

```erlang
-spec float_to_list(Float) -> string() when Float :: float().
```

# `float_to_list`
*auto-imported* *since OTP R16B* 

```erlang
-spec float_to_list(Float, Options) -> string()
                       when
                           Float :: float(),
                           Options :: [Option],
                           Option ::
                               {decimals, Decimals :: 0..253} |
                               {scientific, Decimals :: 0..249} |
                               compact | short.
```

Returns a string corresponding to the text representation of `Float` using fixed
decimal point formatting.

Available options:

- If option `decimals` is specified, the returned value contains at most
  `Decimals` number of digits past the decimal point. If the number does not fit
  in the internal static buffer of 256 bytes, the function throws `badarg`.
- If option `compact` is specified, the trailing zeros at the end of the list
  are truncated. This option is only meaningful together with option `decimals`.
- If option `scientific` is specified, the float is formatted using scientific
  notation with `Decimals` digits of precision.
- If option `short` is specified, the float is formatted with the smallest
  number of digits that still guarantees that
  `F =:= list_to_float(float_to_list(F, [short]))`. When the float is inside the
  range (-2⁵³, 2⁵³), the notation that yields the smallest number of characters
  is used (scientific notation or normal decimal notation). Floats outside the
  range (-2⁵³, 2⁵³) are always formatted using scientific notation to avoid
  confusing results when doing arithmetic operations.
- If `Options` is `[]`, the function behaves as `float_to_list/1`.

## Examples

```erlang
1> float_to_list(7.12, [{decimals, 4}]).
"7.1200"
2> float_to_list(7.12, [{decimals, 4}, compact]).
"7.12"
3> float_to_list(7.12, [{scientific, 3}]).
"7.120e+00"
4> float_to_list(7.12, [short]).
"7.12"
5> float_to_list(0.1+0.2, [short]).
"0.30000000000000004"
6> float_to_list(0.1+0.2).
"3.00000000000000044409e-01"
```

In the last example, [`float_to_list(0.1+0.2)`](`float_to_list/1`) evaluates to
`"3.00000000000000044409e-01"`. The reason for this is explained in
[Representation of Floating Point Numbers](`e:system:data_types.md#float_representation_problem`).

# `floor`
*auto-imported* *allowed in guard tests* *since OTP 20.0* 

```erlang
-spec floor(Number) -> integer() when Number :: number().
```

Returns the largest integer not greater than `Number`.

See also `trunc/1`.

## Examples

```erlang
1> floor(-10.5).
-11
2> floor(5.5).
5
3> floor(10.0).
10
```

# `fun_info`

```erlang
-spec fun_info(Fun) -> [{Item, Info}]
                  when
                      Fun :: function(),
                      Item ::
                          arity | env | index | name | module | new_index | new_uniq | pid | type | uniq,
                      Info :: term().
```

Returns a list with information about the fun `Fun`.

Each list element is a tuple. The order of the tuples is undefined,
and more tuples can be added in a future release.

> #### Warning {: .warning }
>
> This BIF is intended for debugging. Library functions that
> need to check some property of a fun should use `fun_info/2`.

Two types of funs have slightly different semantics:

- A fun created by `fun M:F/A` is called an _external_ fun. Calling it will
  always call the function `F` with arity `A` in the latest code for module `M`.
  Notice that module `M` does not even need to be loaded when the fun
  `fun M:F/A` is created.
- All other funs are called _local_. When a local fun is called, the same
  version of the code that created the fun is called (even if a newer version of
  the module has been loaded).

The following elements are always present in the list for both local and
external funs:

- **`{type, Type}`** - `Type` is `local` or `external`.

- **`{module, Module}`** - `Module` (an atom) is the module name.

  If `Fun` is a local fun, `Module` is the module in which the fun is defined.

  If `Fun` is an external fun, `Module` is the module that the fun refers to.

- **`{name, Name}`** - `Name` (an atom) is a function name.

  If `Fun` is a local fun, `Name` is the name of the local function that
  implements the fun. (This name was generated by the compiler, and is only of
  informational use. As it is a local function, it cannot be called directly.)
  If no code is currently loaded for the fun, `[]` is returned instead of an
  atom.

  If `Fun` is an external fun, `Name` is the name of the exported function that
  the fun refers to.

- **`{arity, Arity}`** - `Arity` is the number of arguments that the fun is to
  be called with.

- **`{env, Env}`** - `Env` (a list) is the environment or free variables for the
  fun. For external funs, the returned list is always empty.

The following elements are only present in the list if `Fun` is local:

- **`{pid, Pid}`** - `Pid` is the process identifier of `init` process on
  the local node.

  > #### Change {: .info }
  >
  > Starting in Erlang/OTP 27, `Pid` always points to the local `init` process,
  > regardless of which process or node the fun was originally created on.
  >
  > See
  > [Upcoming Potential Incompatibilities ](`e:general_info:upcoming_incompatibilities.md#fun-creator-pid-will-always-be-local-init-process`).

- **`{index, Index}`** - `Index` (an integer) is an index into the module fun
  table.

- **`{new_index, Index}`** - `Index` (an integer) is an index into the module
  fun table.

- **`{new_uniq, Uniq}`** - `Uniq` (a binary) is a unique value for this fun. It
  is calculated from the compiled code for the entire module.

- **`{uniq, Uniq}`** - `Uniq` (an integer) is a unique value for this fun. As
  from Erlang/OTP R15, this integer is calculated from the compiled code for the
  entire module. Before Erlang/OTP R15, this integer was based on only the body
  of the fun.

See also `fun_info/2` and `is_function/2`.

# `fun_info`

```erlang
-spec fun_info(Fun, Item) -> {Item, Info}
                  when Fun :: function(), Item :: fun_info_item(), Info :: term().
```

Returns information about `Fun` as specified by `Item`, in the form
`{Item,Info}`.

For any fun, `Item` can be any of the atoms `module`, `name`, `arity`, `env`, or
`type`.

For a local fun, `Item` can also be any of the atoms `index`, `new_index`,
`new_uniq`, `uniq`, and `pid`. For an external fun, the value of any of these
items is always the atom `undefined`.

See [`erlang:fun_info/1`](`fun_info/1`) for a description of the items.

## Examples

```erlang
1> erlang:fun_info(fun() -> ok end, type).
{type,local}
2> erlang:fun_info(fun lists:sum/1, type).
{type,external}
```

# `fun_to_list`

```erlang
-spec fun_to_list(Fun) -> String :: string() when Fun :: function().
```

Returns `String` that represents the code that created `Fun`.

`String` has the following form, if `Fun` was created by a
[fun expression](`e:system:expressions.md#fun-expressions`) of the form
`fun ModuleName:FuncName/Arity`:

`"fun ModuleName:FuncName/Arity"`

The form of `String` when `Fun` is created from other types of
[fun expressions](`e:system:expressions.md#fun-expressions`) differs depending
on if the fun expression was executed while executing compiled code or if the
fun expression was executed while executing uncompiled code (uncompiled
escripts, the Erlang shell, and other code executed by the erl_eval module):

- **compiled code** - `"#Fun<M.I.U>"`, where M, I and U correspond to the values
  named `module`, `index` and `uniq` in the result of
  [`erlang:fun_info(Fun)`](`fun_info/1`).

- **uncompiled code** - All funs created from fun expressions in uncompiled code
  with the same arity are mapped to the same list by
  [`fun_to_list/1`](`fun_to_list/1`).

> #### Note {: .info }
>
> Generally, one can not use [`fun_to_list/1`](`fun_to_list/1`) to check if two
> funs are equal as [`fun_to_list/1`](`fun_to_list/1`) does not take the fun's
> environment into account. See [`erlang:fun_info/1`](`fun_info/1`) for how to
> get the environment of a fun.

> #### Change {: .info }
>
> The output of [`fun_to_list/1`](`fun_to_list/1`) can differ between Erlang
> implementations and may change in future versions.

## Examples

```erlang
-module(test).
-export([add/1, add2/0, fun_tuple/0]).
add(A) -> fun(B) -> A + B end.
add2() -> fun add/1.
fun_tuple() -> {fun() -> 1 end, fun() -> 1 end}.
```

```erlang
> {fun test:add/1, test:add2()}.
{fun test:add/1,#Fun<test.1.107738983>}
```

Explanation: `fun test:add/1` is upgradable but `test:add2()` is not upgradable.

```erlang
> {test:add(1), test:add(42)}.
{#Fun<test.0.107738983>,#Fun<test.0.107738983>}
```

Explanation: `test:add(1)` and `test:add(42)` has the same string representation
as the environment is not taken into account.

```erlang
> test:fun_tuple().
{#Fun<test.2.107738983>,#Fun<test.3.107738983>}
```

Explanation: The string representations differ because the funs come from
different fun expressions.

```erlang
> {fun() -> 1 end, fun() -> 1 end}. >
{#Fun<erl_eval.45.97283095>,#Fun<erl_eval.45.97283095>}
```

Explanation: All funs created from fun expressions of this form in uncompiled
code with the same arity are mapped to the same list by
[`fun_to_list/1`](`fun_to_list/1`).

# `function_exported`

```erlang
-spec function_exported(Module, Function, Arity) -> boolean()
                           when Module :: module(), Function :: atom(), Arity :: arity().
```

Returns `true` if the module `Module` is
[current](`e:system:code_loading.md#code-replacement`) and contains an exported
function `Function/Arity`, or if there is a BIF (a built-in function implemented
in C) with the specified name, otherwise returns `false`.

# `garbage_collect`
*auto-imported* 

```erlang
-spec garbage_collect() -> true.
```

Forces an immediate garbage collection of the executing process.

The function is not to be used unless it has been noticed (or there are good
reasons to suspect) that the spontaneous garbage collection will occur too late
or not at all.

> #### Warning {: .warning }
>
> Improper use can seriously degrade system performance.

# `garbage_collect`
*auto-imported* 

```erlang
-spec garbage_collect(Pid) -> GCResult when Pid :: pid(), GCResult :: boolean().
```

# `garbage_collect`
*auto-imported* *since OTP 17.0* 

```erlang
-spec garbage_collect(Pid, OptionList) -> GCResult | async
                         when
                             Pid :: pid(),
                             RequestId :: term(),
                             Option :: {async, RequestId} | {type, major | minor},
                             OptionList :: [Option],
                             GCResult :: boolean().
```

Garbage collects the node local process identified by `Pid`.

`Option`:

- **`{async, RequestId}`** - The function
  [`garbage_collect/2`](`garbage_collect/2`) returns the value `async`
  immediately after the request has been sent. When the request has been
  processed, the process that called this function is passed a message on the
  form `{garbage_collect, RequestId, GCResult}`.

- **`{type, 'major' | 'minor'}`** - Triggers garbage collection of requested
  type. Default value is `'major'`, which would trigger a fullsweep GC. The
  option `'minor'` is considered a hint and may lead to either minor or major GC
  run.

If `Pid` equals `self/0`, and no `async` option has been passed, the garbage
collection is performed at once, that is, the same as calling
`garbage_collect/0`. Otherwise a request for garbage collection is sent to the
process identified by `Pid`, and will be handled when appropriate. If no `async`
option has been passed, the caller blocks until `GCResult` is available and can
be returned.

`GCResult` informs about the result of the garbage collection request as
follows:

- **`true`** - The process identified by `Pid` has been garbage collected.

- **`false`** - No garbage collection was performed, as the process identified
  by `Pid` terminated before the request could be satisfied.

Notice that the same caveats apply as for `garbage_collect/0`.

Failures:

- **`badarg`** - If `Pid` is not a node local process identifier.

- **`badarg`** - If `OptionList` is an invalid list of options.

# `get`
*auto-imported* 

```erlang
-spec get() -> [{Key, Val}] when Key :: term(), Val :: term().
```

Returns the process dictionary as a list of `{Key, Val}` tuples. The items in
the returned list can be in any order.

For example:

```erlang
1> put(key1, merry).
2> put(key2, lambs).
3> put(key3, {are, playing}).
4> lists:sort(get()).
[{key1,merry},{key2,lambs},{key3,{are,playing}}]
5> erase().
6> get().
[]
```

# `get`
*auto-imported* 

```erlang
-spec get(Key) -> Val | undefined when Key :: term(), Val :: term().
```

Returns the value `Val` associated with `Key` in the process dictionary, or
`undefined` if `Key` does not exist.

The expected time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
1> put(key1, merry).
2> put(key2, lambs).
3> put({any, [valid, term]}, {are, playing}).
4> get({any, [valid, term]}).
{are,playing}
5> erase().
6> get({any, [valid, term]}).
undefined
```

# `get_cookie`

```erlang
-spec get_cookie() -> Cookie | nocookie when Cookie :: atom().
```

Returns the magic cookie of the local node if the node is alive, otherwise the
atom `nocookie`. This value is set by `set_cookie/1`.

# `get_cookie`
*since OTP 24.1* 

```erlang
-spec get_cookie(Node) -> Cookie | nocookie when Node :: node(), Cookie :: atom().
```

Returns the magic cookie for node `Node` if the local node is alive, otherwise
the atom `nocookie`. This value is set by `set_cookie/2`.

# `get_keys`
*auto-imported* *since OTP 18.0* 

```erlang
-spec get_keys() -> [Key] when Key :: term().
```

Returns a list of all keys present in the process dictionary. The items in the
returned list can be in any order.

For example:

```erlang
1> put(dog, '🐶').
2> put(cow, '🐄').
3> put(lamb, '🐑').
4> lists:sort(get_keys()).
[cow,dog,lamb]
5> erase().
6> get_keys().
[]
```

# `get_keys`
*auto-imported* 

```erlang
-spec get_keys(Val) -> [Key] when Val :: term(), Key :: term().
```

Returns a list of keys that are associated with the value `Val` in the process
dictionary. The items in the returned list can be in any order.

For example:

```erlang
1> put(allosaurus, '🦕').
2> put(brachiosaurus, '🦕').
3> put(carnotaurus, '🦕').
4> put(diplodocus, '🦕').
5> put(euoplocephalus, '🦕').
6> put(fox, '🦊').
7> lists:sort(get_keys('🦕')).
[allosaurus,brachiosaurus,carnotaurus,diplodocus,euoplocephalus]
8> erase().
9> get_keys('🦕').
[]
```

# `group_leader`
*auto-imported* 

```erlang
-spec group_leader() -> pid().
```

Returns the process identifier of the group leader for the process evaluating
the function.

Every process is a member of some process group and all groups have a _group
leader_. All I/O from the group is channeled to the group leader. When a new
process is spawned, it gets the same group leader as the spawning process.

Initially, at system startup, `init` is both its own group leader and the group
leader of all processes. During the boot of a system the group leader for
processes will be changed depending on the need of the system. Some examples
where this is done are:

- When an application is started, the top supervisor of that application will
  have its group leader set to the application master. See `application:start/2`
  for more details.
- When running tests, both [`common_test`](`e:common_test:index.html`) and
  `m:eunit` set the group leader in order to capture any I/O from the testcase.
- The [interactive shell](`m:shell`) sets the group leader to intercept I/O.

# `group_leader`
*auto-imported* 

```erlang
-spec group_leader(GroupLeader, Pid) -> true when GroupLeader :: pid(), Pid :: pid().
```

Sets the group leader of `Pid` to `GroupLeader`. Typically, this is used when a
process started from a certain shell is to have another group leader than
`init`.

The group leader should be rarely changed in applications with a supervision
tree, because OTP assumes the group leader of their processes is their
application master.

Setting the group leader follows the signal ordering guarantees described in the
[Processes Chapter](`e:system:ref_man_processes.md#signals`) in the _Erlang
Reference Manual_.

See also `group_leader/0` and
[OTP design principles](`e:system:applications.md#stopping`) related to starting
and stopping applications.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

# `halt`
*auto-imported* 

```erlang
-spec halt() -> no_return().
```

Equivalent to calling [`halt(0, [])`](`halt/2`).

For example:

```erlang
> halt().
os_prompt%
```

# `halt`
*auto-imported* 

```erlang
-spec halt(Status :: non_neg_integer()) -> no_return();
          (Abort :: abort) -> no_return();
          (CrashDumpSlogan :: string()) -> no_return().
```

Equivalent to calling [`halt(HaltType, [])`](`halt/2`).

For example:

```erlang
> halt(17).
os_prompt% echo $?
17
os_prompt%
```

# `halt`
*auto-imported* *since OTP R15B01* 

```erlang
-spec halt(Status :: non_neg_integer(), Options :: halt_options()) -> no_return();
          (Abort :: abort, Options :: halt_options()) -> no_return();
          (CrashDumpSlogan :: string(), Options :: halt_options()) -> no_return().
```

Halt the runtime system.

- ```erlang
  halt(Status :: non_neg_integer(), Options :: halt_options())
  ```
  {: #halt_status_2 }

  Halt the runtime system with status code `Status`.

  > #### Note {: .info }
  >
  > On many platforms, the OS supports only status codes 0-255. A too large
  > status code is truncated by clearing the high bits.

  Currently the following options are valid:

  - **`{flush, EnableFlushing}`{: #halt_flush }** - If `EnableFlushing` equals
    `true`, which also is the default behavior, the runtime system will perform
    the following operations before terminating:

    - Flush all outstanding output.
    - Send all Erlang ports exit signals and wait for them to exit.
    - Wait for all async threads to complete all outstanding async jobs.
    - Call all installed [NIF _on halt_ callbacks](erl_nif.md#on_halt).
    - Wait for all ongoing
      [NIF calls with the _delay halt_ setting](erl_nif.md#delay_halt) enabled
      to return.
    - Call all installed `atexit`/`on_exit` callbacks.

    If `EnableFlushing` equals `false`, the runtime system will terminate
    immediately without performing any of the above listed operations.

    > #### Change {: .info }
    >
    > Runtime systems prior to OTP 26.0 called all installed `atexit`/`on_exit`
    > callbacks also when `flush` was disabled, but as of OTP 26.0 this is no
    > longer the case.

  - **`{flush_timeout, Timeout :: 0..2147483647 | infinity}`{: #halt_flush_timeout }** -
    Sets a limit on the time allowed for [flushing](#halt_flush) prior to
    termination of the runtime system. `Timeout` is in milliseconds. The default
    value is determined by the the `erl` [`+zhft <Timeout>`](erl_cmd.md#+zhft)
    command line flag.

    If flushing has been ongoing for `Timeout` milliseconds, flushing operations
    will be interrupted and the runtime system will immediately be terminated
    with the exit code `255`. If flushing is not enabled, the timeout will have
    no effect on the system.

    See also the `erl` [`+zhft <Timeout>`](erl_cmd.md#+zhft) command line flag.
    Note that the shortest timeout set by the command line flag and the
    `flush_timeout` option will be the actual timeout value in effect.

    Since: OTP 27.0

- ```erlang
  halt(Abort :: abort, Options :: halt_options())
  ```
  {: #halt_abort_2 }

  Halt the Erlang runtime system by aborting and produce a core dump if core
  dumping has been enabled in the environment that the runtime system is
  executing in.

  > #### Note {: .info }
  >
  > The [`{flush, boolean()}`](#halt_flush) option will be ignored, and
  > flushing will be disabled.

- ```erlang
  halt(CrashDumpSlogan :: string(), Options :: halt_options())
  ```
  {: #halt_crash_dump_2 }

  Halt the Erlang runtime system and generate an
  [Erlang crash dump](crash_dump.md). The string `CrashDumpSlogan` will be used
  as slogan in the Erlang crash dump created. The slogan will be trunkated if
  `CrashDumpSlogan` is longer than 1023 characters.

  > #### Note {: .info }
  >
  > The [`{flush, boolean()}`](#halt_flush) option will be ignored, and
  > flushing will be disabled.

  > #### Change {: .info }
  >
  > Behavior changes compared to earlier versions:
  >
  > - Before OTP 24.2, the slogan was truncated if `CrashDumpSlogan` was longer
  >   than 200 characters. Now it will be truncated if longer than 1023
  >   characters.
  > - Before OTP 20.1, only code points in the range 0-255 were accepted in the
  >   slogan. Now any Unicode string is valid.

# `hd`
*auto-imported* *allowed in guard tests* 

```erlang
-spec hd(List) -> Head when List :: nonempty_maybe_improper_list(), Head :: term().
```

Returns the first element of `List`.

It works with improper lists.

Failure: `badarg` if `List` is the empty list `[]`.

## Examples

```erlang
1> hd([1,2,3,4,5]).
1
2> hd([first, second, third, so_on | improper_end]).
first
3> hd([]).
** exception error: bad argument
     in function  hd/1
        called as hd([])
        *** argument 1: not a nonempty list
```

# `hibernate`
*since OTP 28.0* 

```erlang
-spec hibernate() -> ok.
```

Puts the calling process into a wait state where its memory allocation has been
reduced as much as possible. This is useful if the process does not expect to
receive any messages soon.

The process is awakened when a message is sent to it, and control resumes
normally to the caller. Unlike `erlang:hibernate/3`, it does not discard the
call stack.

# `hibernate`

```erlang
-spec hibernate(Module, Function, Args) -> no_return()
                   when Module :: module(), Function :: atom(), Args :: [term()].
```

Puts the calling process into a wait state where its memory allocation has been
reduced as much as possible. This is useful if the process does not expect to
receive any messages soon.

The process is awakened when a message is sent to it, and control resumes in
`Module:Function` with the arguments specified by `Args` with the call stack
emptied, meaning that the process terminates when that function returns. Thus
`erlang:hibernate/3` never returns to its caller. The resume function
`Module:Function/Arity` must be exported (`Arity` =:=
[`length(Args)`](`length/1`)).

If the process has any message in its message queue, the process is awakened
immediately in the same way as described earlier.

In more technical terms, `erlang:hibernate/3` discards the call stack for the
process, and then garbage collects the process. After this, all live data is in
one continuous heap. The heap is then shrunken to the exact same size as the
live data that it holds (even if that size is less than the minimum heap size
for the process).

If the size of the live data in the process is less than the minimum heap size,
the first garbage collection occurring after the process is awakened ensures
that the heap size is changed to a size not smaller than the minimum heap size.

Notice that emptying the call stack means that any surrounding `catch` is
removed and must be re-inserted after hibernation. One effect of this is that
processes started using `proc_lib` (also indirectly, such as `gen_server`
processes), are to use `proc_lib:hibernate/3` instead, to ensure that the
exception handler continues to work when the process wakes up.

# `insert_element`
*since OTP R16B* 

```erlang
-spec insert_element(Index, Tuple1, Term) -> Tuple2
                        when
                            Index :: pos_integer(), Tuple1 :: tuple(), Tuple2 :: tuple(), Term :: term().
```

Returns a new tuple with element `Term` inserted at position `Index` in tuple
`Tuple1`.

All elements from position `Index` and upwards are pushed one step
higher in the new tuple `Tuple2`.

## Examples

```erlang
1> erlang:insert_element(2, {one, two, three}, new).
{one,new,two,three}
```

# `integer_to_binary`
*auto-imported* *since OTP R16B* 

```erlang
-spec integer_to_binary(Integer) -> binary() when Integer :: integer().
```

Returns a binary corresponding to the text representation of `Integer`.

## Examples

```erlang
1> integer_to_binary(77).
<<"77">>
```

# `integer_to_binary`
*auto-imported* *since OTP R16B* 

```erlang
-spec integer_to_binary(Integer, Base) -> binary() when Integer :: integer(), Base :: 2..36.
```

Returns a binary corresponding to the text representation of `Integer` in base
`Base`.

## Examples

```erlang
1> integer_to_binary(1023, 16).
<<"3FF">>
```

# `integer_to_list`
*auto-imported* 

```erlang
-spec integer_to_list(Integer) -> string() when Integer :: integer().
```

Returns a string corresponding to the text representation of `Integer`.

## Examples

```erlang
1> integer_to_list(77).
"77"
```

# `integer_to_list`
*auto-imported* 

```erlang
-spec integer_to_list(Integer, Base) -> string() when Integer :: integer(), Base :: 2..36.
```

Returns a string corresponding to the text representation of `Integer` in base
`Base`.

## Examples

```erlang
1> integer_to_list(1023, 16).
"3FF"
```

# `iolist_size`
*auto-imported* 

```erlang
-spec iolist_size(Item) -> non_neg_integer() when Item :: iolist() | binary().
```

Returns the size in bytes of the binary that would result from
[`iolist_to_binary(Item)`](`iolist_to_binary/1`).

## Examples

```erlang
1> iolist_size([1,2|<<3,4>>]).
4
```

# `iolist_to_binary`
*auto-imported* 

```erlang
-spec iolist_to_binary(IoListOrBinary) -> binary() when IoListOrBinary :: iolist() | binary().
```

Returns a binary constructed from the integers and binaries in
`IoListOrBinary`.

## Examples

```erlang
1> Bin1 = <<1,2,3>>.
<<1,2,3>>
2> Bin2 = <<4,5>>.
<<4,5>>
3> Bin3 = <<6>>.
<<6>>
4> iolist_to_binary([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6>>
```

# `iolist_to_iovec`
*since OTP 20.1* 

```erlang
-spec iolist_to_iovec(IoListOrBinary) -> iovec() when IoListOrBinary :: iolist() | binary().
```

Returns an [iovec](`t:iovec/0`) that is made from the integers and binaries in
`IoListOrBinary`.

This function is useful when you need to flatten an iolist but do not
require a single binary. It can be beneficial for passing data to NIF
functions such as
[`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec) or for more
efficient message passing. The advantage of using this function over
`iolist_to_binary/1` is that it does not need to copy off-heap binaries.

## Examples

If you pass small binaries and integers, it works like `iolist_to_binary/1`.

```erlang
1> Bin1 = <<1,2,3>>.
<<1,2,3>>
2> Bin2 = <<4,5>>.
<<4,5>>
3> Bin3 = <<6>>.
<<6>>
4> erlang:iolist_to_iovec([Bin1,1,[2,3,Bin2],4|Bin3]).
[<<1,2,3,1,2,3,4,5,4,6>>]
```

If you pass larger binaries, they are split and returned in a form
optimized for calling the C function `writev()`.

```erlang
1> erlang:iolist_to_iovec([<<1>>,<<2:8096>>,<<3:8096>>]).
[<<1>>,
 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   ...>>,
 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>]
```

# `is_alive`
*auto-imported* 

```erlang
-spec is_alive() -> boolean().
```

Returns `true` if the local node is alive (that is, if the node can be part of a
distributed system), otherwise `false`. A node is alive if it is started with:

1. [`"erl -name LONGNAME"`](erl_cmd.md#name) or,
1. [`"erl -sname SHORTNAME"`](erl_cmd.md#sname).

A node can also be alive if it has got a name from a call to
`net_kernel:start/2` and has not been stopped by a call to `net_kernel:stop/0`.

# `is_atom`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_atom(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is an atom; otherwise, returns `false`.

## Examples

```erlang
1> is_atom(42).
false
2> is_atom(ok).
true
```

# `is_binary`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_binary(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a binary; otherwise, returns `false`.

A binary always contains a complete number of bytes.

## Examples

```erlang
1> is_binary(42).
false
2> is_binary(<<1,2,3>>).
true
3> is_binary(<<7:12>>).
false
```

# `is_bitstring`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_bitstring(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a bitstring (including a binary); otherwise, returns `false`.

## Examples

```erlang
1> is_bitstring(42).
false
2> is_bitstring(<<1,2,3>>).
true
3> is_bitstring(<<7:12>>).
true
```

# `is_boolean`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_boolean(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is the atom `true` or `false`; otherwise, returns `false`.

## Examples

```erlang
1> is_boolean(true).
true
2> is_boolean(false).
true
3> is_boolean(ok).
false
4> is_boolean(42).
false
```

# `is_builtin`

```erlang
-spec is_builtin(Module, Function, Arity) -> boolean()
                    when Module :: module(), Function :: atom(), Arity :: arity().
```

This BIF is useful for builders of cross-reference tools.

Returns `true` if `Module:Function/Arity` is a BIF implemented in C, otherwise
`false`.

## Examples:

```erlang
1> erlang:is_builtin(lists, keyfind, 3).
true
2> erlang:is_builtin(lists, reverse, 1).
false
```

# `is_float`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_float(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a floating point number; otherwise, returns `false`.

## Examples

```erlang
1> is_float(42).
false
2> is_float(42.0).
true
3> is_float(zero).
false
```

# `is_function`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_function(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a fun; otherwise, returns `false`.

## Examples

```erlang
1> is_function(fun() -> ok end).
true
2> is_function(fun lists:sum/1).
true
3> is_function({lists,sum}).
false
```

# `is_function`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_function(Term, Arity) -> boolean() when Term :: term(), Arity :: arity().
```

Returns `true` if `Term` is a fun that can be applied with `Arity` number of
arguments; otherwise, returns `false`.

## Examples

```erlang
1> is_function(fun() -> ok end, 0).
true
2> is_function(fun lists:sum/1, 1).
true
3> is_function({lists,sum}, 1).
false
4> is_function(fun lists:sum/1, -1).
** exception error: bad argument
     in function  is_function/2
        called as is_function(fun lists:sum/1,-1)
        *** argument 2: out of range
5> is_function(fun lists:sum/1, bad_arity).
** exception error: bad argument
     in function  is_function/2
        called as is_function(fun lists:sum/1,bad_arity)
        *** argument 2: not an integer
```

# `is_integer`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_integer(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is an integer; otherwise, returns `false`.

## Examples

```erlang
1> is_integer(1).
true
2> is_integer(-1234567890123456789012345678901234567890).
true
3> is_integer(1.0).
false
4> is_integer(zero).
false
```

# `is_integer`
*auto-imported* *allowed in guard tests* *since OTP 29.0* 

```erlang
-spec is_integer(Term, LB, UB) -> boolean() when Term :: integer(), LB :: integer(), UB :: integer().
```

Returns `true` if `Term`, `LB`, and `UB` all evaluate to integers, and `Term`
is between `LB` and `UB` inclusive; otherwise, returns `false`.

## Examples

```erlang
1> is_integer(15, 0, 1024).
true
2> is_integer(-1, 0, 1).
false
```

Failure: `badarg` if `LB` or `UB` does not evaluate to an integer.

# `is_list`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_list(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a list with zero or more elements; otherwise,
returns `false`.

## Examples

```erlang
1> is_list({a,b,c}).
false
2> is_list([]).
true
3> is_list([1]).
true
4> is_list([1,2]).
true
5> is_list([1,2|3]).
true
```

# `is_map`
*auto-imported* *allowed in guard tests* *since OTP 17.0* 

```erlang
-spec is_map(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a map; otherwise, returns `false`.

## Examples

```erlang
1> is_map(#{}).
true
2> is_map(#{key => value}).
true
3> is_map([]).
false
```

# `is_map_key`
*auto-imported* *allowed in guard tests* *since OTP 21.0* 

```erlang
-spec is_map_key(Key, Map) -> boolean() when Key :: term(), Map :: map().
```

Returns `true` if map `Map` contains `Key` and returns `false` if it does not
contain the `Key`.

Failure: A `{badmap,Map}` exception is raised if `Map` is not a map.

## Examples

```erlang
1> Map = #{"42" => value}.
#{"42" => value}
2> is_map_key("42", Map).
true
3> is_map_key(value, Map).
false
4> is_map_key(value, no_map).
** exception error: bad map: no_map
     in function  is_map_key/2
        called as is_map_key(value,no_map)
        *** argument 2: not a map
```

# `is_number`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_number(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is an integer or a floating point number; otherwise,
returns `false`.

## Examples

```erlang
1> is_number(10.0).
true
2> is_number(7).
true
3> is_number(zero).
false
```

# `is_pid`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_pid(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a process identifier; otherwise, returns `false`.

## Examples

```erlang
1> is_pid(self()).
true
2> is_pid(ok).
false
```

# `is_port`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_port(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a port identifier; otherwise, returns `false`.

## Examples

```erlang
1> APort = hd(erlang:ports()).
2> is_port(APort).
true
3> is_port(self()).
false
```

# `is_process_alive`
*auto-imported* 

```erlang
-spec is_process_alive(Pid) -> boolean() when Pid :: pid().
```

`Pid` must refer to a process at the local node.

Returns `true` if the process exists and is alive, that is, is not exiting and
has not exited. Otherwise returns `false`.

If process `P1` calls [`is_process_alive(P2Pid)`](`is_process_alive/1`) it is
guaranteed that all signals, sent from `P1` to `P2` (`P2` is the process with
identifier `P2Pid`) before the call, will be delivered to `P2` before the
aliveness of `P2` is checked. This guarantee means that one can use
[`is_process_alive/1`](`is_process_alive/1`) to let a process `P1` wait until a
process `P2`, which has got an exit signal with reason `kill` from P1, is
killed.

For example:

```erlang
1> P2Pid = spawn(fun() -> receive after infinity -> ok end end).
2> exit(P2Pid, kill).
true
% P2 might not be killed
3> is_process_alive(P2Pid).
false
% P2 is not alive (the call above always return false)
```

See the documentation about [signals](`e:system:ref_man_processes.md#signals`)
and [erlang:exit_signal/2](`exit_signal/2`) for more information about signals and
exit signals.

# `is_record`
*auto-imported* *allowed in guard tests* *since OTP 29.0* 

```erlang
-spec is_record(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a native record; otherwise, returns `false`.

## Examples

```erlang
1> NR = records:create(test, native, [], #{is_exported => true}).
2> is_record(NR).
true
3> is_record(an_atom).
false
4> is_record({a,b,c}).
false
```

# `is_record`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_record(Term, Name) -> boolean() when Term :: term(), Name :: atom().
```

Returns `true` if `Term` is either a tuple record or a native record named `Name`;
otherwise, returns `false`.

This is allowed in guard tests if `Name` is a literal atom.

The compilation will fail with an error message if `Name` does not
refer to either a record defined in the same module as the call to
`is_record/2` or a native record imported into it.

If `Name` is a variable and `Term` is a native record, this function
returns `true` if `Name` refers to a native record named `Name` in any
module; otherwise, it returns `false`.

If `Name` is a variable and `Term` is tuple, this function returns
`true` if the first element of the tuple is `Name`; otherwise, it
returns `false`.

> #### Change {: .change }
>
> Before OTP 29, this function would only work for tuple records.

# `is_record`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_record(Term :: dynamic(), Module :: module(), Name :: atom()) -> boolean();
               (Term :: dynamic(), Name :: atom(), Arity :: non_neg_integer()) -> boolean().
```

Returns `true` either if `Term` is a native record defined in module `Module`
with name `Name`, or if `Term` is a tuple, its first element is `Name`, and its
size is `Arity`; otherwise, returns `false`.

This is allowed in guard tests if either `Module` and `Name` are literal atoms, or
`Name` is a literal atom and `Size` is a literal integer.

> #### Note {: .info }
>
> This BIF only checks if `Term` is a native record created from the given
> module. It does not check whether `Term` is still defined in the given
> module, nor whether it is exported.

> #### Change {: .change }
>
> Before Erlang/OTP 29, this function only worked for tuple records.

# `is_reference`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_reference(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a reference; otherwise, returns `false`.

## Examples

```erlang
1> is_reference(make_ref()).
true
2> is_reference(self()).
false
```

# `is_tuple`
*auto-imported* *allowed in guard tests* 

```erlang
-spec is_tuple(Term) -> boolean() when Term :: term().
```

Returns `true` if `Term` is a tuple; otherwise, returns `false`.

## Examples

```erlang
1> is_tuple({a, b, c}).
true
2> is_tuple([a, b, c]).
false
```

# `length`
*auto-imported* *allowed in guard tests* 

```erlang
-spec length(List) -> non_neg_integer() when List :: [term()].
```

Returns the length of `List`.

## Examples

```erlang
1> length([1,2,3,4,5,6,7,8,9]).
9
2> length([a,b|c]).
** exception error: bad argument
     in function  length/1
        called as length([a,b|c])
        *** argument 1: not a list
```

# `link`
*auto-imported* 

```erlang
-spec link(PidOrPort) -> true when PidOrPort :: pid() | port().
```

Sets up and activates a link between the calling process and another process or
a port identified by `PidOrPort`.

We will from here on call the identified process or port linkee. If the linkee
is a port, it must reside on the same node as the caller.

If one of the participants of a link terminates, it will
[send an exit signal](`e:system:ref_man_processes.md#sending_exit_signals`) to
the other participant. The exit signal will contain the
[exit reason](`e:system:ref_man_processes.md#link_exit_signal_reason`) of the
terminated participant. Other cases when exit signals are triggered due to a
link are when no linkee exist (`noproc` exit reason) and when the connection
between linked processes on different nodes is lost or cannot be established
(`noconnection` exit reason).

An existing link can be removed by calling `unlink/1`. For more information on
links and exit signals due to links, see the _Processes_ chapter in the _Erlang
Reference Manual_:

- [Links](`e:system:ref_man_processes.md#links`)
- [Sending Exit Signals](`e:system:ref_man_processes.md#sending_exit_signals`)
- [Receiving Exit Signals](`e:system:ref_man_processes.md#receiving_exit_signals`)

For historical reasons, [`link/1`](`link/1`) has a strange semi-synchronous
behavior when it is "cheap" to check if the linkee exists or not, and the caller
does not [trap exits](#process_flag_trap_exit). If the above is true
and the linkee does not exist, [`link/1`](`link/1`) will raise a `noproc` error
_exception_. The expected behavior would instead have been that
[`link/1`](`link/1`) returned `true`, and the caller later was sent an exit
signal with `noproc` exit reason, but this is unfortunately not the case. The
`noproc` [exception](`e:system:errors.md#exceptions`) is not to be confused with
an [exit signal](`e:system:ref_man_processes.md#sending_exit_signals`) with exit
reason `noproc`. Currently it is "cheap" to check if the linkee exists when it
is supposed to reside on the same node as the calling process.

The link setup and activation is performed asynchronously. If the link already
exists, or if the caller attempts to create a link to itself, nothing is done. A
detailed description of the [link protocol](erl_dist_protocol.md#link_protocol)
can be found in the _Distribution Protocol_ chapter of the _ERTS User's Guide_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

Failure:

- `badarg` if `PidOrPort` does not identify a process or a node local port.
- `noproc` linkee does not exist and it is "cheap" to check if it exists as
  described above.

# `link`
*auto-imported* *since OTP 28.0* 

```erlang
-spec link(PidOrPort, [link_option()]) -> true when PidOrPort :: pid() | port().
```

Provides an option list for modification of the link functionality provided by
`link/1`. The `PidOrPort` argument has the same meaning as when passed to
`link/1`.

Currently available options:

- **`priority`** - Since OTP 28.0

  [Enables priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  of `EXIT` messages due to the link for the calling process. If the link
  already exists without priority message reception enabled for the link,
  priority message reception will be enabled on the existing link. If the link
  already exists with priority message reception enabled and this option is not
  passed or `link/1` is called, priority message reception for this link will be
  disabled.

  Note that priority message reception due to the link is *only* enabled for the
  process that passed this option. If the linked process also wants to enable
  priority message reception, it needs to call `link/2` passing the `priority`
  option itself.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.

# `list_to_atom`
*auto-imported* 

```erlang
-spec list_to_atom(String) -> atom() when String :: string().
```

Returns the atom whose text representation is `String`, creating a new
atom if necessary.

As from Erlang/OTP 20, `String` may contain any Unicode character. Earlier
versions allowed only ISO-latin-1 characters as the implementation did not allow
Unicode characters above 255.

> #### Note {: .info }
>
> Note that once an atom is created, it cannot be deleted.
> The Erlang system has a
> [configurable limit](`e:system:system_limits.md#atoms`)
> on the number of atoms that can exist.
> To avoid reaching this limit, consider whether
> [`list_to_existing_atom/1`](`list_to_existing_atom/1`) is a better choice
> than [`list_to_atom/1`](`list_to_atom/1`).
>
> The number of characters that are permitted in an atom name is
> [limited](`e:system:system_limits.md#atom_name_limit`).

## Examples

```erlang
1> list_to_atom("Erlang").
'Erlang'
2> list_to_atom([960]).
'π'
```

# `list_to_binary`
*auto-imported* 

```erlang
-spec list_to_binary(IoList) -> binary() when IoList :: iolist().
```

Returns a binary made from the integers and binaries in `IoList`.

## Examples

```erlang
1> Bin1 = <<1,2,3>>.
<<1,2,3>>
2> Bin2 = <<4,5>>.
<<4,5>>
3> Bin3 = <<6>>.
<<6>>
4> list_to_binary([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6>>
```

# `list_to_bitstring`
*auto-imported* 

```erlang
-spec list_to_bitstring(BitstringList) -> bitstring() when BitstringList :: bitstring_list().
```

Returns a bitstring made from the integers and bitstrings in
`BitstringList`.

The last tail in `BitstringList` is allowed to be a bitstring.

## Examples

```erlang
1> Bin1 = <<1,2,3>>.
<<1,2,3>>
2> Bin2 = <<4,5>>.
<<4,5>>
3> Bin3 = <<6,7:4>>.
<<6,7:4>>
4> list_to_bitstring([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6,7:4>>
```

# `list_to_existing_atom`
*auto-imported* 

```erlang
-spec list_to_existing_atom(String) -> atom() when String :: string().
```

Returns the atom whose text representation is `String`, but only if there
already exists such atom.

An atom exists if it has been created by the run-time system by either
loading code or creating a term that the atom is a part of.

Failure: `badarg` if there does not already exist an atom whose text
representation is `String`.

> #### Note {: .info }
>
> Note that the compiler may optimize away atoms. For example, the compiler will
> rewrite [`atom_to_list(some_atom)`](`atom_to_list/1`) to `"some_atom"`. If
> that expression is the only mention of the atom `some_atom` in the containing
> module, the atom will not be created when the module is loaded, and a
> subsequent call to
> [`list_to_existing_atom("some_atom")`](`list_to_existing_atom/1`) will fail.

## Examples

```erlang
1> list_to_existing_atom("a_blatal_DOS_attack").
** exception error: bad argument
     in function  list_to_existing_atom/1
        called as list_to_existing_atom("a_blatal_DOS_attack")
        *** argument 1: not an already existing atom
2> hello.
hello
3> list_to_existing_atom("hello").
hello
```

# `list_to_float`
*auto-imported* 

```erlang
-spec list_to_float(String) -> float() when String :: string().
```

Returns the float whose text representation is `String`.

## Examples

```erlang
1> list_to_float("2.2017764e+0").
2.2017764
```

The float string format is the same as the format for
[Erlang float literals](`e:system:data_types.md`) except for that underscores
are not permitted.

Failure: `badarg` if `String` contains a invalid representation of a float.

# `list_to_integer`
*auto-imported* 

```erlang
-spec list_to_integer(String) -> integer() when String :: string().
```

Returns an integer whose text representation is `String`.

`String` must contain at least one digit character and can have an optional
prefix consisting of a single "`+`" or "`-`" character (that is, `String` must
match the regular expression `"^[+-]?[0-9]+$"`).

Failure: `badarg` if `String` contains a invalid representation of an integer.

## Examples

```erlang
1> list_to_integer("123").
123
2> list_to_integer("-123").
-123
3> list_to_integer("+123234982304982309482093833234234").
123234982304982309482093833234234
```

# `list_to_integer`
*auto-imported* 

```erlang
-spec list_to_integer(String, Base) -> integer() when String :: string(), Base :: 2..36.
```

Returns an integer whose text representation in base `Base` is `String`.

`String` must contain at least one digit character and can have an optional
prefix consisting of a single "`+`" or "`-`" character.

Failure: `badarg` if `String` contains an invalid integer representation.

## Examples

```erlang
1> list_to_integer("3FF", 16).
1023
2> list_to_integer("+3FF", 16).
1023
3> list_to_integer("3ff", 16).
1023
4> list_to_integer("-3FF", 16).
-1023
5> list_to_integer("Base36IsFun", 36).
41313437507787071
6> list_to_integer("102", 2).
** exception error: bad argument
     in function  list_to_integer/2
        called as list_to_integer("102",2)
        *** argument 1: not a textual representation of an integer
```

# `list_to_pid`
*auto-imported* 

```erlang
-spec list_to_pid(String) -> pid() when String :: string().
```

Returns a process identifier whose text representation is a `String`.

Failure: `badarg` if `String` contains an invalid representation of a process
identifier.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.

## Examples

```erlang
1> list_to_pid("<0.1.0>").
<0.1.0>
```

# `list_to_port`
*auto-imported* *since OTP 20.0* 

```erlang
-spec list_to_port(String) -> port() when String :: string().
```

Returns a port identifier whose text representation is a `String`.

Failure: `badarg` if `String` contains a bad representation of a port
identifier.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.

## Examples

```erlang
1> list_to_port("#Port<0.0>").
#Port<0.0>
```

# `list_to_ref`
*auto-imported* *since OTP 20.0* 

```erlang
-spec list_to_ref(String) -> reference() when String :: string().
```

Returns a reference whose text representation is a `String`.

Failure: `badarg` if `String` contains a bad representation of a reference.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.

## Examples

```erlang
1> list_to_ref("#Ref<0.0.0.0>").
#Ref<0.0.0.0>
```

# `list_to_tuple`
*auto-imported* 

```erlang
-spec list_to_tuple(List) -> tuple() when List :: [term()].
```

Returns a tuple whose elements are the elements of `List`.

## Examples

```erlang
1> list_to_tuple([share, ['Ericsson_B', 163]]).
{share, ['Ericsson_B', 163]}
```

# `load_module`
*auto-imported* 

```erlang
-spec load_module(Module, Binary) -> {module, Module} | {error, Reason}
                     when
                         Module :: module(),
                         Binary :: binary(),
                         Reason :: badfile | not_purged | on_load | {features_not_allowed, [atom()]}.
```

Loads `Module` described by the object code contained within `Binary`.

If the code for module `Module` already exists, all export
references are replaced so they point to the newly loaded code. The previously
loaded code is kept in the system as old code, as there can still be processes
executing that code.

Returns either `{module, Module}`, or `{error, Reason}` if loading fails.
`Reason` is one of the following:

- **`badfile`** - The object code in `Binary` has an incorrect format _or_ the
  object code contains code for another module than `Module`.

- **`not_purged`** - `Binary` contains a module that cannot be loaded because
  old code for this module already exists.

- **`on_load`** - The code in `Binary` contains an `on_load` declaration that
  must be executed before `Binary` can become the current code. Any previous
  current code for `Module` will remain until the `on_load` call has finished.

- **not_allowed** - The code in `Binary` has been compiled with features that
  are currently not enabled in the runtime system.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.

# `load_nif`

```erlang
-spec load_nif(Path, LoadInfo) -> ok | Error
                  when
                      Path :: string(),
                      LoadInfo :: term(),
                      Error :: {error, {Reason, Text :: string()}},
                      Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
```

Loads and links a dynamic library containing native implemented functions (NIFs)
for a module.

`Path` is a file path to the shareable object/dynamic library file
minus the OS-dependent file extension (`.so` for Unix and `.dll` for Windows).
Notice that on most OSs the library has to have a different name on disc when an
upgrade of the nif is done. If the name is the same, but the contents differ,
the old library may be loaded instead. For information on how to implement a NIF
library, see [`erl_nif(3)`](erl_nif.md).

`LoadInfo` can be any term. It is passed on to the library as part of the
initialization. A good practice is to include a module version number to support
future code upgrade scenarios.

The call to [`load_nif/2`](`load_nif/2`) must be made _directly_ from the Erlang
code of the module that the NIF library belongs to. It returns either `ok`, or
`{error,{Reason,Text}}` if loading fails. `Reason` is one of the following atoms
while `Text` is a human readable string that can give more information about the
failure:

- **`load_failed`** - The OS failed to load the NIF library.

- **`bad_lib`** - The library did not fulfill the requirements as a NIF library
  of the calling module.

- **`load | upgrade`** - The corresponding library callback was unsuccessful.

- **`reload`** - A NIF library is already loaded for this module instance. The
  previously deprecated `reload` feature was removed in OTP 20.

- **`old_code`** - The call to [`load_nif/2`](`load_nif/2`) was made from the
  old code of a module that has been upgraded; this is not allowed.

If the [`-nifs()`](`e:system:modules.md#nifs_attribute`) attribute is used
(which is recommended), all NIFs in the dynamic library must be declared as such
for [`load_nif/2`](`load_nif/2`) to succeed. On the other hand, all functions
declared with the `-nifs()` attribute do not have to be implemented by the
dynamic library. This allows a target independent Erlang file to contain
fallback implementations for functions that may lack NIF support depending on
target OS/hardware platform.

# `loaded`

```erlang
-spec loaded() -> [Module] when Module :: module().
```

Returns a list of all loaded Erlang modules (current and old code), including
preloaded modules.

See also `m:code`.

# `localtime`

```erlang
-spec localtime() -> DateTime when DateTime :: calendar:datetime().
```

Returns the current local date and time,
`{{Year, Month, Day}, {Hour, Minute, Second}}`.

For example:

```erlang
> erlang:localtime().
{{1996,11,6},{14,45,17}}
```

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).

# `localtime_to_universaltime`

```erlang
-spec localtime_to_universaltime(Localtime) -> Universaltime
                                    when
                                        Localtime :: calendar:datetime(),
                                        Universaltime :: calendar:datetime().
```

Converts local date and time to Universal Time Coordinated (UTC), if supported
by the underlying OS. Otherwise no conversion is done and `Localtime` is
returned.

For example:

```erlang
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}).
{{1996,11,6},{13,45,17}}
```

Failure: `badarg` if `Localtime` denotes an invalid date and time.

# `localtime_to_universaltime`

```erlang
-spec localtime_to_universaltime(Localtime, IsDst) -> Universaltime
                                    when
                                        Localtime :: calendar:datetime(),
                                        Universaltime :: calendar:datetime(),
                                        IsDst :: true | false | undefined.
```

Converts local date and time to Universal Time Coordinated (UTC) as
`erlang:localtime_to_universaltime/1`, but the caller decides if Daylight Saving
Time is active.

If `IsDst == true`, `Localtime` is during Daylight Saving Time, if
`IsDst == false` it is not. If `IsDst == undefined`, the underlying OS can
guess, which is the same as calling
`erlang:localtime_to_universaltime(Localtime)`.

Examples:

```erlang
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, true).
{{1996,11,6},{12,45,17}}
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, false).
{{1996,11,6},{13,45,17}}
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, undefined).
{{1996,11,6},{13,45,17}}
```

Failure: `badarg` if `Localtime` denotes an invalid date and time.

# `make_ref`
*auto-imported* 

```erlang
-spec make_ref() -> reference().
```

Returns a [unique reference](`e:system:system_limits.md#unique_references`).

The reference is unique among connected nodes.

> #### Warning {: .warning }
>
> Before OTP 23, if a node was restarted multiple times with the same
> node name, references created on a newer instance could be mistaken
> for those created on an older instance with the same name.

## Examples

```erlang
1> is_reference(make_ref()).
true
```

# `make_tuple`

```erlang
-spec make_tuple(Arity, InitialValue) -> tuple() when Arity :: arity(), InitialValue :: term().
```

Creates a new tuple of the specified `Arity`, where all elements are
`InitialValue`.

## Examples

```erlang
1> erlang:make_tuple(4, []).
{[],[],[],[]}
```

# `make_tuple`

```erlang
-spec make_tuple(Arity, DefaultValue, InitList) -> tuple()
                    when
                        Arity :: arity(),
                        DefaultValue :: term(),
                        InitList :: [{Position :: pos_integer(), term()}].
```

Creates a tuple of size `Arity`, where each element has value `DefaultValue`,
and then fills in values from `InitList`.

Each list element in `InitList` must be a two-tuple, where the first element is
a position in the newly created tuple and the second element is any term. If a
position occurs more than once in the list, the term corresponding to the last
occurrence is used.

## Examples

```erlang
1> erlang:make_tuple(5, [], [{2,ignored},{5,zz},{2,aa}]).
{[],aa,[],[],zz}
```

# `map_get`
*auto-imported* *allowed in guard tests* *since OTP 21.0* 

```erlang
-spec map_get(Key, Map) -> Value when Map :: map(), Key :: any(), Value :: any().
```

Returns value `Value` associated with `Key` if `Map` contains `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

## Examples

```erlang
1> Key = 1337.
2> Map = #{42 => value_two,Key => "value one","a" => 1}.
3> map_get(Key, Map).
"value one"
4> map_get(unknown_key, Map).
** exception error: bad key: unknown_key
     in function  map_get/2
        called as map_get(unknown_key,#{42 => value_two,1337 => "value one","a" => 1})
        *** argument 1: not present in map
5> map_get(key, no_map).
** exception error: bad map: no_map
     in function  map_get/2
        called as map_get(key,no_map)
        *** argument 2: not a map
```

# `map_size`
*auto-imported* *allowed in guard tests* *since OTP 17.0* 

```erlang
-spec map_size(Map) -> non_neg_integer() when Map :: map().
```

Returns the number of key-value pairs in `Map`.

## Examples

```erlang
1> map_size(#{a=>1, b=>2, c=>3}).
3
```

# `match_spec_test`
*since OTP 19.0* 

```erlang
-spec match_spec_test(MatchAgainst, MatchSpec, Type) -> TestResult
                         when
                             MatchAgainst :: [term()] | tuple(),
                             MatchSpec :: term(),
                             Type :: table | trace,
                             TestResult ::
                                 {ok, term(), [return_trace], [{error | warning, string()}]} |
                                 {error, [{error | warning, string()}]}.
```

Tests a match specification used in calls to `ets:select/2` and
`trace:function/4`.

The function tests both a match specification for "syntactic" correctness and
runs the match specification against the object.
If the match specification contains errors, the tuple
`{error, Errors}` is returned, where `Errors` is a list of natural language
descriptions of what was wrong with the match specification.

If `Type` is `table`, the object to match against is to be a tuple. The function
then returns `{ok,Result,[],Warnings}`, where `Result` is what would have been
the result in a real `ets:select/2` call, or `false` if the match specification
does not match the object tuple.

If `Type` is `trace`, the object to match against is to be a list. The function
returns `{ok, Result, Flags, Warnings}`, where `Result` is one of the following:

- `true` if a trace message is to be emitted
- `false` if a trace message is not to be emitted
- The message term to be appended to the trace message

`Flags` is a list of trace flags to be enabled; currently, the only
available flag is `return_trace`.

See also `ets:test_ms/2`.

## Examples

```erlang
1> Ms = [{{'$1','$2'}, [], [{{'$2','$1'}}]}].
2> erlang:match_spec_test({a,b}, Ms, table).
{ok,{b,a},[],[]}
3> erlang:match_spec_test({a,b,c}, Ms, table).
{ok,false,[],[]}
```

# `max`
*auto-imported* *allowed in guard tests* 

```erlang
-spec max(Term1, Term2) -> Maximum when Term1 :: term(), Term2 :: term(), Maximum :: term().
```

Returns the largest of `Term1` and `Term2`.

If the terms compare equal with the `==` operator, `Term1` is returned.

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of the `==` operator and how terms are ordered.

> #### Change {: .info }
>
> Allowed in guards tests from Erlang/OTP 26.

## Examples:

```erlang
1> max(1, 2).
2
2> max(1.0, 1).
1.0
3> max(1, 1.0).
1
4> max("abc", "b").
"b"
```

# `md5`

```erlang
-spec md5(Data) -> Digest when Data :: iodata(), Digest :: binary().
```

Computes an MD5 message digest from `Data`, where the length of the digest is
128 bits (16 bytes). `Data` is a binary or a list of small integers and
binaries.

For more information about MD5, see
[RFC 1321 - The MD5 Message-Digest Algorithm](https://www.ietf.org/rfc/rfc1321.txt).

> #### Warning {: .warning }
>
> The MD5 Message-Digest Algorithm is _not_ considered safe for code-signing or
> software-integrity purposes.

# `md5_final`

```erlang
-spec md5_final(Context) -> Digest when Context :: binary(), Digest :: binary().
```

Finishes the update of an MD5 `Context` and returns the computed `MD5` message
digest.

# `md5_init`

```erlang
-spec md5_init() -> Context when Context :: binary().
```

Creates an MD5 context, to be used in the following calls to
[`md5_update/2`](`md5_update/2`).

# `md5_update`

```erlang
-spec md5_update(Context, Data) -> NewContext
                    when Context :: binary(), Data :: iodata(), NewContext :: binary().
```

Update an MD5 `Context` with `Data` and returns a `NewContext`.

# `memory`

```erlang
-spec memory() -> [{Type, Size}] when Type :: memory_type(), Size :: non_neg_integer().
```

Returns a list with information about memory dynamically allocated by the Erlang
emulator.

Each list element is a tuple `{Type, Size}`. The first element `Type`
is an atom describing memory type. The second element `Size` is the memory size
in bytes.

Memory types:

- **`total`** - The total amount of memory currently allocated. This is the same
  as the sum of the memory size for `processes` and `system`.

- **`processes`** - The total amount of memory currently allocated for the
  Erlang processes.

- **`processes_used`** - The total amount of memory currently used by the Erlang
  processes. This is part of the memory presented as `processes` memory.

- **`system`** - The total amount of memory currently allocated for the emulator
  that is not directly related to any Erlang process. Memory presented as
  `processes` is not included in this memory. `m:instrument` can be used to get
  a more detailed breakdown of what memory is part of this type.

- **`atom`** - The total amount of memory currently allocated for atoms. This
  memory is part of the memory presented as `system` memory.

- **`atom_used`** - The total amount of memory currently used for atoms. This
  memory is part of the memory presented as `atom` memory.

- **`binary`** - The total amount of memory currently allocated for binaries.
  This memory is part of the memory presented as `system` memory.

- **`code`** - The total amount of memory currently allocated for Erlang code.
  This memory is part of the memory presented as `system` memory.

- **`ets`** - The total amount of memory currently allocated for ETS tables.
  This memory is part of the memory presented as `system` memory.

- **`maximum`** - The maximum total amount of memory allocated since the
  emulator was started. This tuple is only present when the emulator is run with
  instrumentation.

  For information on how to run the emulator with instrumentation, see
  `m:instrument` and/or [`erl(1)`](erl_cmd.md).

> #### Note {: .info }
>
> The `system` value is not complete. Some allocated memory that is to be part
> of this value is not.
>
> When the emulator is run with instrumentation, the `system` value is more
> accurate, but memory directly allocated for `malloc` (and friends) is still
> not part of the `system` value. Direct calls to `malloc` are only done from
> OS-specific runtime libraries and perhaps from user-implemented Erlang drivers
> that do not use the memory allocation functions in the driver interface.
>
> As the `total` value is the sum of `processes` and `system`, the error in
> `system` propagates to the `total` value.
>
> The different amounts of memory that are summed are _not_ gathered atomically,
> which introduces an error in the result.

The different values have the following relation to each other. Values beginning
with an uppercase letter is not part of the result.

```text
total      = processes + system
processes  = processes_used + ProcessesNotUsed
system     = atom + binary + code + ets + OtherSystem
atom       = atom_used + AtomNotUsed
RealTotal  = processes + RealSystem
RealSystem = system + MissedSystem
```

More tuples in the returned list can be added in a future release.

> #### Note {: .info }
>
> The `total` value is supposed to be the total amount of memory dynamically
> allocated by the emulator. Shared libraries, the code of the emulator itself,
> and the emulator stacks are not supposed to be included. That is, the `total`
> value is _not_ supposed to be equal to the total size of all pages mapped to
> the emulator.
>
> Also, because of fragmentation and prereservation of memory areas, the size of
> the memory segments containing the dynamically allocated memory blocks can be
> much larger than the total size of the dynamically allocated memory blocks.

> #### Change {: .info }
>
> As from ERTS 5.6.4, `erlang:memory/0` requires that all
> [`erts_alloc(3)`](erts_alloc.md) allocators are enabled (default behavior).

Failure: `notsup` if an [`erts_alloc(3)`](erts_alloc.md) allocator has been
disabled.

# `memory`

```erlang
-spec memory(Type :: memory_type()) -> non_neg_integer();
            (TypeList :: [memory_type()]) -> [{memory_type(), non_neg_integer()}].
```

Returns the memory size in bytes allocated for memory of type `Type`. The
argument can also be specified as a list of `t:memory_type/0` atoms, in which case
a corresponding list of `{memory_type(), Size :: integer >= 0}` tuples is
returned.

> #### Change {: .info }
>
> As from ERTS 5.6.4, `erlang:memory/1` requires that all
> [`erts_alloc(3)`](erts_alloc.md) allocators are enabled (default behavior).

Failures:

- **`badarg`** - If `Type` is not one of the memory types listed in the
  description of [`erlang:memory/0`](`memory/0`).

- **`badarg`** - If `maximum` is passed as `Type` and the emulator is not run in
  instrumented mode.

- **`notsup`** - If an [`erts_alloc(3)`](erts_alloc.md) allocator has been
  disabled.

See also [`erlang:memory/0`](`memory/0`).

# `min`
*auto-imported* *allowed in guard tests* 

```erlang
-spec min(Term1, Term2) -> Minimum when Term1 :: term(), Term2 :: term(), Minimum :: term().
```

Returns the smallest of `Term1` and `Term2`.

If the terms compare equal with the `==` operator, `Term1` is returned.

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of the `==` operator and how terms are ordered.

> #### Change {: .info }
>
> Allowed in guards tests from Erlang/OTP 26.

## Examples

```erlang
1> min(1, 2).
1
2> min(1.0, 1).
1.0
3> min(1, 1.0).
1
4> min("abc", "b").
"abc"
```

# `module_loaded`
*auto-imported* 

```erlang
-spec module_loaded(Module) -> boolean() when Module :: module().
```

Returns `true` if the module `Module` is loaded as
[_current code_](`e:system:code_loading.md#code-replacement`); otherwise,
`false`. It does not attempt to load the module.

# `monitor`
*auto-imported* 

```erlang
-spec monitor(process, monitor_process_identifier()) -> MonitorRef when MonitorRef :: reference();
             (port, monitor_port_identifier()) -> MonitorRef when MonitorRef :: reference();
             (time_offset, clock_service) -> MonitorRef when MonitorRef :: reference().
```

Sends a monitor request of type `Type` to the entity identified by `Item`.

If the monitored entity does not exist or it changes monitored state, the caller
of `monitor/2` is notified by a message on the following format:
{: #monitor_message}

```erlang
{Tag, MonitorRef, Type, Object, Info}
```

> #### Note {: .info }
>
> The monitor request is an asynchronous signal. That is, it takes time before
> the signal reaches its destination.

`Type` can be one of the following atoms: `process`, `port` or `time_offset`.

A `process` or `port` monitor is triggered only once, after that it is removed
from both monitoring process and the monitored entity. Monitors are fired when
the monitored process or port terminates, does not exist at the moment of
creation, or if the connection to it is lost. If the connection to it is lost,
we do not know if it still exists. The monitoring is also turned off when
`demonitor/1` is called.

A `process` or `port` monitor by name resolves the `RegisteredName` to `t:pid/0`
or `t:port/0` only once at the moment of monitor instantiation, later changes to
the name registration will not affect the existing monitor.

When a `process` or `port` monitor is triggered, a `'DOWN'` message is sent that
has the following pattern:

```erlang
{'DOWN', MonitorRef, Type, Object, Info}
```

In the monitor message `MonitorRef` and `Type` are the same as described
earlier, and:

- **`Object`** - The monitored entity, which triggered the event. When
  monitoring a process or a local port, `Object` will be equal to the `t:pid/0`
  or `t:port/0` that was being monitored. When monitoring process or port by
  name, `Object` will have format `{RegisteredName, Node}` where
  `RegisteredName` is the name which has been used with
  `monitor/2` call and `Node` is local or remote node name (for
  ports monitored by name, `Node` is always local node name).

- **`Info`** - Either the exit reason of the process, `noproc` (process or port
  did not exist at the time of monitor creation), or `noconnection` (no
  connection to the node where the monitored process resides).

- **Monitoring a `process`{: #monitor_process }** - Creates monitor between the
  current process and another process identified by `Item`, which can be a
  `t:pid/0` (local or remote), an atom `RegisteredName` or a tuple
  `{RegisteredName, Node}` for a registered process, located elsewhere.

  > #### Change {: .info }
  >
  > Before ERTS 10.0 (OTP 21.0), monitoring a process could fail with `badarg`
  > if the monitored process resided on a primitive node (such as erl_interface
  > or jinterface), where remote process monitoring is not implemented.
  >
  > Now, such a call to `monitor` will instead succeed and a monitor is created.
  > But the monitor will only supervise the connection. That is, a
  > `{'DOWN', _, process, _, noconnection}` is the only message that may be
  > received, as the primitive node has no way of reporting the status of the
  > monitored process.

- **Monitoring a `port`{: #monitor_port }** - Creates monitor between the
  current process and a port identified by `Item`, which can be a `t:port/0`
  (only local), an atom `RegisteredName` or a tuple `{RegisteredName, Node}` for
  a registered port, located on this node. Note, that attempt to monitor a
  remote port will result in `badarg`.

  Available since OTP 19.0.

- **Monitoring a `time_offset`{: #monitor_time_offset }** - Monitors changes in
  `time_offset/0` between
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
  [Erlang system time](time_correction.md#erlang-system-time). One valid `Item`
  exists in combination with the `time_offset Type`, namely the atom
  `clock_service`. Notice that the atom `clock_service` is _not_ the registered
  name of a process. In this case it serves as an identifier of the runtime
  system internal clock service at current runtime system instance.

  The monitor is triggered when the time offset is changed. This either if the
  time offset value is changed, or if the offset is changed from preliminary to
  final during
  [finalization of the time offset](#system_flag_time_offset) when the
  [single time warp mode](time_correction.md#single-time-warp-mode) is used.
  When a change from preliminary to final time offset is made, the monitor is
  triggered once regardless of whether the time offset value was changed or not.

  If the runtime system is in
  [multi time warp mode](time_correction.md#multi-time-warp-mode), the time
  offset is changed when the runtime system detects that the
  [OS system time](time_correction.md#os-system-time) has changed. The runtime
  system does, however, not detect this immediately when it occurs. A task
  checking the time offset is scheduled to execute at least once a minute, so
  under normal operation this is to be detected within a minute, but during
  heavy load it can take longer time.

  The monitor is _not_ automatically removed after it has been triggered. That
  is, repeated changes of the time offset trigger the monitor repeatedly.

  When the monitor is triggered a `'CHANGE'` message is sent to the monitoring
  process. A `'CHANGE'` message has the following pattern:

  ```erlang
  {'CHANGE', MonitorRef, Type, Item, NewTimeOffset}
  ```

  where `MonitorRef`, `Type`, and `Item` are the same as described above, and
  `NewTimeOffset` is the new time offset.

  When the `'CHANGE'` message has been received you are guaranteed not to
  retrieve the old time offset when calling
  `erlang:time_offset/0`. Notice that you can observe the
  change of the time offset when calling `erlang:time_offset/0` before you get
  the `'CHANGE'` message.

  Available since OTP 18.0.

Making several calls to `monitor/2` for the same `Item` and/or
`Type` is not an error; it results in as many independent monitoring instances.

The monitor functionality is expected to be extended. That is, other `Type`s and
`Item`s are expected to be supported in a future release.

> #### Note {: .info }
>
> If or when `monitor/2` is extended, other possible values for
> `Tag`, `Object`, and `Info` in the monitor message will be introduced.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

# `monitor`
*auto-imported* *since OTP 24.0* 

```erlang
-spec monitor(process, monitor_process_identifier(), [monitor_option()]) -> MonitorRef
                 when MonitorRef :: reference();
             (port, monitor_port_identifier(), [monitor_option()]) -> MonitorRef
                 when MonitorRef :: reference();
             (time_offset, clock_service, [monitor_option()]) -> MonitorRef
                 when MonitorRef :: reference().
```

Provides an option list for modification of monitoring functionality provided by
`monitor/2`. The `Type` and `Item` arguments have the same meaning as when
passed to [`monitor/2`](`monitor/2`).

Currently available options:

- **`{alias, UnaliasOpt}`** - The returned monitor reference will also become an
  alias for the calling process. That is, the returned reference can be used for
  sending messages to the calling process. See also `alias/0`. The `UnaliasOpt`
  determines how the alias should be deactivated.

  - **`explicit_unalias`** - Only an explicit call to `unalias/1` will
    deactivate the alias.

  - **`demonitor`** - The alias will be automatically deactivated when the
    monitor is removed. This either via an explicit call to `demonitor/1` or
    when it is automatically removed at the same time as a `'DOWN'` message is
    delivered due to the monitor. The alias can also still be deactivated via a
    call to [`unalias/1`](`unalias/1`).

  - **`reply_demonitor`** - The alias will be automatically deactivated when the
    monitor is removed (see `demonitor` option above) or a reply message sent
    via the alias is received. When a reply message is received via the alias
    the monitor will also be automatically removed. This is useful in
    client/server scenarios when a client monitors the server and will get the
    reply via the alias. Once the response is received both the alias and the
    monitor will be automatically removed regardless of whether the response is
    a reply or a `'DOWN'` message. The alias can also still be deactivated via a
    call to [`unalias/1`](`unalias/1`). Note that if the alias is removed using
    the [`unalias/1`](`unalias/1`) BIF, the monitor will still be left active.

  Example:

  ```erlang
  server() ->
      receive
          {request, AliasReqId, Request} ->
              Result = perform_request(Request),
              AliasReqId ! {reply, AliasReqId, Result}
      end,
      server().

  client(ServerPid, Request) ->
      AliasMonReqId = monitor(process, ServerPid, [{alias, reply_demonitor}]),
      ServerPid ! {request, AliasMonReqId, Request},
      %% Alias as well as monitor will be automatically deactivated if we
      %% receive a reply or a 'DOWN' message since we used 'reply_demonitor'
      %% as unalias option...
      receive
          {reply, AliasMonReqId, Result} ->
              Result;
          {'DOWN', AliasMonReqId, process, ServerPid, ExitReason} ->
              error(ExitReason)
      end.
  ```

  Note that both the server and the client in this example must be executing on
  at least OTP 24 systems in order for this to work.

  For more information on process aliases see the
  [_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section
  of the _Erlang Reference Manual_.

- **`{tag, UserDefinedTag}`** - Replace the default `Tag` with `UserDefinedTag`
  in the [monitor message](#monitor_message) delivered when the
  monitor is triggered. For example, when monitoring a process, the `'DOWN'` tag
  in the down message will be replaced by `UserDefinedTag`.

  An example of how the `{tag, UserDefinedTag}` option can be used in order to
  enable the new
  [selective receive optimization](`e:system:eff_guide_processes.md#receiving-messages`),
  introduced in OTP 24, when making multiple requests to different servers:

  ```erlang
  server() ->
      receive
          {request, From, ReqId, Request} ->
              Result = perform_request(Request),
              From ! {reply, self(), ReqId, Result}
      end,
      server().

  client(ServerPids, Request) when is_list(ServerPids) ->
      ReqId = make_ref(),
      lists:foreach(fun (ServerPid) ->
                            _ = monitor(process, ServerPid,
                                        [{tag, {'DOWN', ReqId}}]),
                            ServerPid ! {request, self(), ReqId, Request}
                    end,
                    ServerPids),
      receive_replies(ReqId, length(ServerPids), []).

  receive_replies(_ReqId, 0, Acc) ->
      Acc;
  receive_replies(ReqId, N, Acc) ->
      %% The compiler will detect that we match on the 'ReqId'
      %% reference in all clauses, and will enable the selective
      %% receive optimization which makes the receive able to
      %% skip past all messages present in the message queue at
      %% the time when the 'ReqId' reference was created...
      Res = receive
                {reply, ServerPid, ReqId, Result} ->
                    %% Here we typically would have deactivated the
                    %% monitor by a call to demonitor(Mon, [flush]) but
                    %% we ignore this in this example for simplicity...
                    {ok, ServerPid, Result};
                {{'DOWN', ReqId}, _Mon, process, ServerPid, ExitReason} ->
                    {error, ServerPid, ExitReason}
            end,
      receive_replies(ReqId, N-1, [Res | Acc]).
  ```

  In order for this example to work as intended, the client must be executing on
  at least an OTP 24 system, but the servers may execute on older systems.

- **`priority`** - Since OTP 28.0

  [Enables priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  of the monitor message(s) sent when this monitor is triggered for the calling
  process.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.

# `monitor_node`
*auto-imported* 

```erlang
-spec monitor_node(Node, Flag) -> true when Node :: node(), Flag :: boolean().
```

Monitor the status of the node `Node`. If `Flag` is `true`, monitoring is turned
on. If `Flag` is `false`, monitoring is turned off.

Making several calls to [`monitor_node(Node, true)`](`monitor_node/2`) for the
same `Node` is not an error; it results in as many independent monitoring
instances.

If `Node` fails or does not exist, the message `{nodedown, Node}` is delivered
to the process. If a process has made two calls to
[`monitor_node(Node, true)`](`monitor_node/2`) and `Node` terminates, two
`nodedown` messages are delivered to the process. If there is no connection to
`Node`, an attempt is made to create one. If this fails, a `nodedown` message is
delivered.

The delivery of the `nodedown` signal is not ordered with respect to other link
or monitor signals from the node that goes down. If you need a guarantee that
all signals from the remote node has been delivered before the `nodedown` signal
is sent, you should use `net_kernel:monitor_nodes/1`.

Nodes connected through hidden connections can be monitored as any other nodes.

Failure: `notalive` if the local node is not alive.

# `monitor_node`

```erlang
-spec monitor_node(Node, Flag, Options) -> true
                      when
                          Node :: node(),
                          Flag :: boolean(),
                          Options :: [Option],
                          Option :: allow_passive_connect.
```

Behaves as `monitor_node/2` except that it allows an extra option to be
specified, namely `allow_passive_connect`.

This option allows the BIF to wait the normal network connection time-out
for the _monitored node_ to connect itself, even if it cannot be actively
connected from this node (that is, it is blocked). The state where this can
be useful can only be achieved by using the Kernel option `dist_auto_connect once`.
If that option is not used, option `allow_passive_connect` has no effect.

> #### Note {: .info }
>
> Option `allow_passive_connect` is used internally and is seldom needed in
> applications where the network topology and the Kernel options in effect are
> known in advance.

Failure: `badarg` if the local node is not alive or the option list is
malformed.

# `monotonic_time`
*since OTP 18.0* 

```erlang
-spec monotonic_time() -> integer().
```

Returns the current
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) in `native`
[time unit](`t:time_unit/0`). This is a monotonically increasing time
since some unspecified point in time.

> #### Note {: .info }
>
> This is a
> [monotonically increasing](time_correction.md#monotonically-increasing) time,
> but _not_ a
> [strictly monotonically increasing](time_correction.md#strictly-monotonically-increasing)
> time. That is, consecutive calls to `erlang:monotonic_time/0` can produce the
> same result.
>
> Different runtime system instances will use different unspecified points in
> time as base for their Erlang monotonic clocks. That is, it is _pointless_
> comparing monotonic times from different runtime system instances. Different
> runtime system instances can also place this unspecified point in time
> different relative runtime system start. It can be placed in the future (time
> at start is a negative value), the past (time at start is a positive value),
> or the runtime system start (time at start is zero). The monotonic time at
> runtime system start can be retrieved by calling
> [`erlang:system_info(start_time)`](#system_info_start_time).

# `monotonic_time`
*since OTP 18.0* 

```erlang
-spec monotonic_time(Unit) -> integer() when Unit :: time_unit().
```

Returns the current
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) converted into
the `Unit` passed as argument.

Same as calling
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[ `erlang:monotonic_time()`](`monotonic_time/0`)`, native, Unit)`,
however optimized for commonly used `Unit`s.

# `nif_error`
*since OTP R14B* 

```erlang
-spec nif_error(Reason) -> no_return() when Reason :: term().
```

Works exactly like `error/1`, but Dialyzer thinks that this BIF will return an
arbitrary term. When used in a stub function for a NIF to generate an exception
when the NIF library is not loaded, Dialyzer does not generate false warnings.

# `nif_error`
*since OTP R14B* 

```erlang
-spec nif_error(Reason, Args) -> no_return() when Reason :: term(), Args :: [term()].
```

Works exactly like `error/2`, but Dialyzer thinks that this BIF will return an
arbitrary term. When used in a stub function for a NIF to generate an exception
when the NIF library is not loaded, Dialyzer does not generate false warnings.

# `node`
*auto-imported* *allowed in guard tests* 

```erlang
-spec node() -> Node when Node :: node().
```

Returns the name of the local node.

If the node is not alive, `nonode@nohost` is returned.

## Examples

```erlang
> node().
nonode@nohost
```

# `node`
*auto-imported* *allowed in guard tests* 

```erlang
-spec node(Arg) -> Node when Arg :: pid() | port() | reference(), Node :: node().
```

Returns the node where `Arg` originates.

If `Arg` originates from the local node and the local node is not
alive, `nonode@nohost` is returned.

## Examples

```erlang
1> node(self()) =:= node().
true
```

# `nodes`
*auto-imported* 

```erlang
-spec nodes() -> Nodes when Nodes :: [node()].
```

Returns a list of all nodes connected to this node through normal connections
(that is, [hidden nodes](`e:system:distributed.md#hidden-nodes`) are not
listed). Same as [nodes(visible)](#nodes_visible).

# `nodes`
*auto-imported* 

```erlang
-spec nodes(Arg) -> Nodes
               when
                   Arg :: NodeType | [NodeType],
                   NodeType :: visible | hidden | connected | this | known,
                   Nodes :: [node()].
```

Returns a list of nodes according to the argument specified. The returned
result, when the argument is a list, is the list of nodes satisfying the
disjunction(s) of the list elements.

`NodeType`s:

- **`visible`{: #nodes_visible }** - Nodes connected to this node through normal
  connections.

- **`hidden`** - Nodes connected to this node through hidden connections.

- **`connected`** - All nodes connected to this node.

- **`this`** - This node.

- **`known`** - Nodes that are known to this node. That is, connected nodes and
  nodes referred to by process identifiers, port identifiers, and references
  located on this node. The set of known nodes is garbage collected. Notice that
  this garbage collection can be delayed. For more information, see
  [`erlang:system_info(delayed_node_table_gc)`](#system_info_delayed_node_table_gc).

Some equalities: `[node()] = nodes(this)`,
`nodes(connected) = nodes([visible, hidden])`, and `nodes() = nodes(visible)`.

# `nodes`
*auto-imported* *since OTP 25.1* 

```erlang
-spec nodes(Arg, InfoOpts) -> [NodeInfo]
               when
                   NodeType :: visible | hidden | connected | this | known,
                   Arg :: NodeType | [NodeType],
                   InfoOpts :: #{connection_id => boolean(), node_type => boolean()},
                   NodeTypeInfo :: visible | hidden | this | known,
                   ConnectionId :: undefined | integer(),
                   Info :: #{connection_id => ConnectionId, node_type => NodeTypeInfo},
                   NodeInfo :: {node(), Info}.
```

Returns a list of `NodeInfo` tuples.

The first element is the node name. Nodes to be included in the list are determined
by the first argument `Arg` in the same way as for [`nodes(Arg)`](`nodes/1`).
The second element of `NodeInfo` tuples is a map containing further information
about the node identified by the first element.
The information present in this map is determined by the
`InfoOpts` map passed as the second argument. Currently the following
associations are allowed in the `InfoOpts` map:

- **`connection_id => boolean()`** - If the value of the association equals
  `true`, the `Info` map in the returned result will contain the key
  `connection_id` associated with the value `ConnectionId`. If `ConnectionId`
  equals `undefined`, the node is not connected to the node which the caller is
  executing on, or is the node which the caller is executing on. If
  `ConnectionId` is an integer, the node is currently connected to the node
  which the caller is executing on.

  [](){: #connection_id } The integer connection identifier value together with
  a node name identifies a specific connection instance to the node with that
  node name. The connection identifier value is node local. That is, on the
  other node the connection identifier will _not_ be the same value. If a
  connection is taken down and then taken up again, the connection identifier
  value will change for the connection to that node. The amount of values for
  connection identifiers are limited, so it is possible to see the same value
  for different instances, but quite unlikely. It is undefined how the value
  change between two consecutive connection instances.

- **`node_type => boolean()`** - If the value of the association equals `true`,
  the `Info` map in the returned result will contain the key `node_type`
  associated with the value `NodeTypeInfo`. Currently the following node types
  exist:

  - **`visible`** - The node is connected to the node of the calling process
    through an ordinary visible connection. That is, the node name would appear
    in the result returned by `nodes/0`.

  - **`hidden`** - The node is connected to the node of the calling process
    through a hidden connection. That is, the node name would _not_ appear in
    the result returned by `nodes/0`.

  - **`this`** - This is the node of the calling process.

  - **`known`** - The node is not connected but known to the node of the calling
    process.

Example:

```erlang
(a@localhost)1> nodes([this, connected], #{connection_id=>true, node_type=>true}).
[{c@localhost,#{connection_id => 13892108,node_type => hidden}},
 {b@localhost,#{connection_id => 3067553,node_type => visible}},
 {a@localhost,#{connection_id => undefined,node_type => this}}]
(a@localhost)2>
```

# `open_port`
*auto-imported* 

```erlang
-spec open_port(PortName, PortSettings) -> port()
                   when
                       PortName ::
                           {spawn, Command :: string() | binary()} |
                           {spawn_driver, Command :: string() | binary()} |
                           {spawn_executable, FileName :: file:name_all()} |
                           {fd, In :: non_neg_integer(), Out :: non_neg_integer()},
                       PortSettings :: [Opt],
                       Opt ::
                           {packet, N :: 1 | 2 | 4} |
                           stream |
                           {line, L :: non_neg_integer()} |
                           {cd, Dir :: string() | binary()} |
                           {env,
                            Env :: [{Name :: os:env_var_name(), Val :: os:env_var_value() | [] | false}]} |
                           {args, [string() | binary()]} |
                           {arg0, string() | binary()} |
                           exit_status | use_stdio | nouse_stdio | stderr_to_stdout | in | out |
                           binary | eof |
                           {parallelism, Boolean :: boolean()} |
                           hide |
                           {busy_limits_port, {non_neg_integer(), non_neg_integer()} | disabled} |
                           {busy_limits_msgq, {non_neg_integer(), non_neg_integer()} | disabled}.
```

Returns a port identifier as the result of opening a new Erlang port. A port can
be seen as an external Erlang process.

The name of the executable as well as the arguments specified in `cd`, `env`,
`args`, and `arg0` are subject to Unicode filename translation if the system is
running in Unicode filename mode. To avoid translation or to force, for example
UTF-8, supply the executable and/or arguments as a binary in the correct
encoding. For details, see the module `m:file`, the function
`file:native_name_encoding/0` in Kernel, and the
[`Using Unicode in Erlang`](`e:stdlib:unicode_usage.md`) User's Guide.

> #### Note {: .info }
>
> The characters in the name (if specified as a list) can only be > 255 if the
> Erlang virtual machine is started in Unicode filename translation mode.
> Otherwise the name of the executable is limited to the ISO Latin-1 character
> set.

`PortName`s:

- **`{spawn, Command}`** - Starts an external program. `Command` is the name of
  the external program to be run. `Command` runs outside the Erlang work space
  unless an Erlang driver with the name `Command` is found. If found, that
  driver is started. A driver runs in the Erlang work space, which means that it
  is linked with the Erlang runtime system.

  For external programs, `PATH` is searched (or an equivalent method is used to
  find programs, depending on the OS). This is done by invoking the shell on
  certain platforms. The first space-separated token of the command is
  considered as the name of the executable (or driver). This (among other
  things) makes this option unsuitable for running programs with spaces in
  filenames or directory names. If spaces in executable filenames are desired,
  use `{spawn_executable, Command}` instead.

  > #### Warning {: .warning }
  >
  > On Unix systems, arguments are passed to a new operating system process as
  > an array of strings but on Windows it is up to the child process to parse
  > them and some Windows programs may apply their own rules, which are
  > inconsistent with the standard C runtime `argv` parsing.
  >
  > This is particularly troublesome when invoking `.bat`, `.cmd`, or `.com`
  > files as these run implicitly through `cmd.exe`, whose argument parsing is
  > vulnerable to malicious input and can be used to run arbitrary shell
  > commands.
  >
  > Therefore, if you are running on Windows and you execute batch files or
  > `.com` applications, you must not pass untrusted input as arguments to the
  > program. This affects both `spawn` and `spawn_executable`.

- **`{spawn_executable, FileName}`** - Works like `{spawn, FileName}`, but only
  runs external executables. `FileName` in its whole is used as the name of the
  executable, including any spaces. If arguments are to be passed, the
  `PortSettings` `args` and `arg0` can be used.

  The shell is usually not invoked to start the program, it is executed
  directly. `PATH` (or equivalent) is not searched. To find a program in `PATH`
  to execute, use `os:find_executable/1`.

  Only if a shell script or `.bat` file is executed, the appropriate command
  interpreter is invoked implicitly, but there is still no command-argument
  expansion or implicit `PATH` search.

  If `FileName` cannot be run, an error exception is raised, with the POSIX
  error code as the reason. The error reason can differ between OSs. Typically
  the error `enoent` is raised when an attempt is made to run a program that is
  not found and `eacces` is raised when the specified file is not executable.

- **`{spawn_driver, Command}`** - Works like `{spawn, Command}`, but demands the
  first (space-separated) token of the command to be the name of a loaded
  driver. If no driver with that name is loaded, a `badarg` error is raised.

- **`{fd, In, Out}`** - Allows an Erlang process to access any currently opened
  file descriptors used by Erlang. The file descriptor `In` can be used for
  standard input, and the file descriptor `Out` for standard output. It is only
  used for various servers in the Erlang OS (`shell` and `user`). Hence, its use
  is limited.

`PortSettings` is a list of settings for the port. The valid settings are as
follows:

- **`{packet, N}`** - Messages are preceded by their length, sent in `N` bytes,
  with the most significant byte first. The valid values for `N` are 1, 2,
  and 4.

- **`stream`** - Output messages are sent without packet lengths. A user-defined
  protocol must be used between the Erlang process and the external object.

- **`{line, L}`** - Messages are delivered on a per line basis. Each line
  (delimited by the OS-dependent newline sequence) is delivered in a single
  message. The message data format is `{Flag, Line}`, where `Flag` is `eol` or
  `noeol`, and `Line` is the data delivered (without the newline sequence).

  `L` specifies the maximum line length in bytes. Lines longer than this are
  delivered in more than one message, with `Flag` set to `noeol` for all but the
  last message. If end of file is encountered anywhere else than immediately
  following a newline sequence, the last line is also delivered with `Flag` set
  to `noeol`. Otherwise lines are delivered with `Flag` set to `eol`.

  The `{packet, N}` and `{line, L}` settings are mutually exclusive.

- **`{cd, Dir}`** - Only valid for `{spawn, Command}` and
  `{spawn_executable, FileName}`. The external program starts using `Dir` as its
  working directory. `Dir` must be a string.

- **`{env, Env}`** - Only valid for `{spawn, Command}`, and `{spawn_executable, FileName}`.
  The environment of the started process is extended using the environment
  specifications in `Env`.

  `Env` is to be a list of tuples `{Name, Val}`, where `Name` is a `t:os:env_var_name/0`
  representing the name of an environment variable, and `Val` is a `t:os:env_var_name/0`
  representing the value it is to have in the spawned port process. Both `Name` and `Val` must
  be strings.

  If `Val` is set to the atom `false` or the empty string (that is `""` or `[]`), open_port
  will consider those variables unset just as if `os:unsetenv/1` had been called.

  For information about encoding requirements, see documentation of the types
  for `Name` and `Val`.

- **`{args, [ string() | binary() ]}`** - Only valid for
  `{spawn_executable, FileName}` and specifies arguments to the executable. Each
  argument is specified as a separate string and (on Unix) eventually ends up as
  one element each in the argument vector. On other platforms, a similar
  behavior is mimicked.

  The arguments are not expanded by the shell before they are supplied to the
  executable. Most notably this means that file wildcard expansion does not
  occur. To expand wildcards for the arguments, use `filelib:wildcard/1`. Notice
  that even if the program is a Unix shell script, meaning that the shell
  ultimately is invoked, wildcard expansion does not occur, and the script is
  provided with the untouched arguments. On Windows, wildcard expansion is
  always up to the program itself, therefore this is not an issue.

  The executable name (also known as `argv[0]`) is not to be specified in this
  list. The proper executable name is automatically used as `argv[0]`, where
  applicable.

  If you explicitly want to set the program name in the argument vector, option
  `arg0` can be used.

- **`{arg0, string() | binary()}`** - Only valid for
  `{spawn_executable, FileName}` and explicitly specifies the program name
  argument when running an executable. This can in some circumstances, on some
  OSs, be desirable. How the program responds to this is highly system-dependent
  and no specific effect is guaranteed.

- **`exit_status`** - Only valid for `{spawn, Command}`, where `Command` refers
  to an external program, and for `{spawn_executable, FileName}`.

  When the external process connected to the port exits, a message of the form
  `{Port,{exit_status,Status}}` is sent to the connected process, where `Status`
  is the exit status of the external process. If the program aborts on Unix, the
  same convention is used as the shells do (that is, 128+signal).

  If option `eof` is specified also, the messages `eof` and `exit_status` appear
  in an unspecified order.

- **`use_stdio`** - Only valid for `{spawn, Command}` and
  `{spawn_executable, FileName}`. It allows the standard input and output (file
  descriptors 0 and 1) of the spawned (Unix) process for communication with
  Erlang.

- **`nouse_stdio`** - The opposite of `use_stdio`. It uses file descriptors 3
  and 4 for communication with Erlang.

- **`stderr_to_stdout`** - Affects ports to external programs. The executed
  program gets its standard error file redirected to its standard output file.
  `stderr_to_stdout` and `nouse_stdio` are mutually exclusive.

- **`overlapped_io`** - Affects ports to external programs on Windows only. The
  standard input and standard output handles of the port program are, if this
  option is supplied, opened with flag `FILE_FLAG_OVERLAPPED`, so that the port
  program can (and must) do overlapped I/O on its standard handles. This is not
  normally the case for simple port programs, but an option of value for the
  experienced Windows programmer. _On all other platforms, this option is
  silently discarded._

- **`in`** - The port can only be used for input.

- **`out`** - The port can only be used for output.

- **`binary`** - All I/O from the port is binary data objects as opposed to
  lists of bytes.

- **`eof`** - The port is not closed at the end of the file and does not produce
  an exit signal. Instead, it remains open and a `{Port, eof}` message is sent
  to the process holding the port.

- **`hide`** - When running on Windows, suppresses creation of a new console
  window when spawning the port program. (This option has no effect on other
  platforms.)

- **`{parallelism, Boolean}`** - [](){: #open_port_parallelism } Sets scheduler
  hint for port parallelism. If set to `true`, the virtual machine schedules
  port tasks; when doing so, it improves parallelism in the system. If set to
  `false`, the virtual machine tries to perform port tasks immediately,
  improving latency at the expense of parallelism. The default can be set at
  system startup by passing command-line argument [`+spp`](erl_cmd.md#%2Bspp) to
  [erl](erl_cmd.md).

- **`{busy_limits_port, {Low, High} | disabled}`** - Sets limits that will be
  used for controlling the busy state of the port.

  When the ports internal output queue size becomes larger than or equal to
  `High` bytes, it enters the busy state. When it becomes less than `Low` bytes
  it leaves the busy state. When the port is in the busy state, processes
  sending commands to it will be suspended until the port leaves the busy state.
  Commands are in this context either `Port ! {Owner, {command, Data}}` or
  `port_command/[2,3]`.

  The `Low` limit is automatically adjusted to the same as `High` if it is set
  larger then `High`. Valid range of values for `Low` and `High` is
  `[1, (1 bsl (8*erlang:system_info(wordsize)))-2]`. If the atom `disabled` is
  passed, the port will never enter the busy state.

  The defaults are `Low = 4096` and `High = 8192`.

  _Note_ that this option is only valid when spawning an executable (port
  program) by opening the spawn driver and when opening the `fd` driver. This
  option will cause a failure with a `badarg` exception when opening other
  drivers.

- **`{busy_limits_msgq, {Low, High} | disabled}`** - Sets limits that will be
  used for controlling the busy state of the port message queue.

  When the ports message queue size becomes larger than or equal to `High` bytes
  it enters the busy state. When it becomes less than `Low` bytes it leaves the
  busy state. When the port message queue is in the busy state, processes
  sending commands to it will be suspended until the port message queue leaves
  the busy state. Commands are in this context either
  `Port ! {Owner, {command, Data}}` or `port_command/[2,3]`.

  The `Low` limit is automatically adjusted to the same as `High` if it is set
  larger then `High`. Valid range of values for `Low` and `High` is
  `[1, (1 bsl (8*erlang:system_info(wordsize)))-2]`. If the atom `disabled` is
  passed, the port message queue will never enter the busy state.

  _Note_ that if the driver statically has disabled the use of this feature, a
  failure with a `badarg` exception will be raised unless this option also is
  set to `disable` or not passed at all.

  The defaults are `Low = 4096` and `High = 8192` unless the driver itself does
  modifications of these values.

  _Note_ that the driver might fail if it also adjust these limits by itself and
  you have disabled this feature.

  The spawn driver (used when spawning an executable) and the `fd` driver do not
  disable this feature and do not adjust these limits by themselves.

  For more information see the documentation
  [`erl_drv_busy_msgq_limits()`](erl_driver.md#erl_drv_busy_msgq_limits).

Default is `stream` for all port types and `use_stdio` for spawned ports.

Failure: if the port cannot be opened, the exit reason is `badarg`,
`system_limit`, or the POSIX error code that most closely describes the error,
or `einval` if no POSIX code is appropriate:

- **`badarg`** - Bad input arguments to `open_port`.

- **`system_limit`** - All available ports in the Erlang emulator are in use.

- **`enomem`** - Not enough memory to create the port.

- **`eagain`** - No more available OS processes.

- **`enametoolong`** - Too long external command.

- **`emfile`** - No more available file descriptors (for the OS process that the
  Erlang emulator runs in).

- **`enfile`** - Full file table (for the entire OS).

- **`eacces`** - `Command` specified in `{spawn_executable, Command}` does not
  point out an executable file.

- **`enoent`** - `FileName` specified in `{spawn_executable, FileName}` does not
  point out an existing file.

During use of a port opened using `{spawn, Name}`, `{spawn_driver, Name}`, or
`{spawn_executable, Name}`, errors arising when sending messages to it are
reported to the owning process using signals of the form
`{'EXIT', Port, PosixCode}`. For the possible values of `PosixCode`, see
`m:file`.

The maximum number of ports that can be open at the same time can be configured
by passing command-line flag [`+Q`](erl_cmd.md#max_ports) to [erl](erl_cmd.md).

# `phash2`

```erlang
-spec phash2(Term) -> Hash when Term :: term(), Hash :: non_neg_integer().
```

# `phash2`

```erlang
-spec phash2(Term, Range) -> Hash when Term :: term(), Range :: pos_integer(), Hash :: non_neg_integer().
```

Returns a hash value for `Term`.

The hash value for the same Erlang term is guaranteed to be the same
regardless of machine architecture and ERTS version.

The function returns a hash value for `Term` within the range
`0..Range-1`. The maximum value for `Range` is 2^32. When without argument
`Range`, a value in the range 0..2^27-1 is returned.

This BIF is always to be used for hashing terms. It distributes small
integers better than [`phash/2`](`phash/2`), and it is faster for
large integers and binaries.

Notice that the range `0..Range-1` is different from the range of
[`phash/2`](`phash/2`), which is `1..Range`.

## Examples

```erlang
1> erlang:phash2({a,b,c}, 1_000).
870
2> erlang:phash2(41, 1_000).
297
3> erlang:phash2(42, 1_000).
368
4> erlang:phash2(43, 1_000).
725
```

# `pid_to_list`
*auto-imported* 

```erlang
-spec pid_to_list(Pid) -> string() when Pid :: pid().
```

Returns a string corresponding to the text representation of `Pid`.

> #### Note {: .info }
>
> The [creation](erl_dist_protocol.md) for the node is not included in the list
> representation of `Pid`. This means that processes in different incarnations
> of a node with a specific name can get the same list representation.

## Examples

```erlang
1> erlang:pid_to_list(<0.1.0>).
"<0.1.0>"
```

# `port_call`

```erlang
-spec port_call(Port, Operation, Data) -> term()
                   when Port :: port() | atom(), Operation :: integer(), Data :: term().
```

Performs a synchronous call to a port. The meaning of `Operation` and `Data`
depends on the port, that is, on the port driver. Not all port drivers support
this feature.

`Port` is a port identifier, referring to a driver.

`Operation` is an integer, which is passed on to the driver.

`Data` is any Erlang term. This data is converted to binary term format and sent
to the port.

Returns a term from the driver. The meaning of the returned data also depends on
the port driver.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Operation` does not fit in a 32-bit integer.

- **`badarg`** - If the port driver does not support synchronous control
  operations.

- **`badarg`** - If the port driver so decides for any reason (probably
  something wrong with `Operation` or `Data`).

  > #### Warning {: .warning }
  >
  > Do not call `port_call` with an unknown `Port` identifier and expect
  > `badarg` exception. Any undefined behavior is possible (including node
  > crash) depending on how the port driver interprets the supplied arguments.

# `port_close`
*auto-imported* 

```erlang
-spec port_close(Port) -> true when Port :: port() | atom().
```

Closes an open port. Roughly the same as `Port ! {self(), close}` except for the
error behavior (see below), being synchronous, and that the port does _not_
reply with `{Port, closed}`.

Any process can close a port with [`port_close/1`](`port_close/1`), not only the
port owner (the connected process). If the calling process is linked to the port
identified by `Port`, the exit signal from the port is guaranteed to be delivered before
[`port_close/1`](`port_close/1`) returns.

For comparison: `Port ! {self(), close}` only fails with `badarg` if `Port` does
not refer to a port or a process. If `Port` is a closed port, nothing happens.
If `Port` is an open port and the calling process is the port owner, the port
replies with `{Port, closed}` when all buffers have been flushed and the port
really closes. If the calling process is not the port owner, the _port owner_
fails with `badsig`.

Notice that any process can close a port using `Port ! {PortOwner, close}` as if
it itself was the port owner, but the reply always goes to the port owner.

As from Erlang/OTP R16, `Port ! {PortOwner, close}` is truly asynchronous.
Notice that this operation has always been documented as an asynchronous
operation, while the underlying implementation has been synchronous.
[`port_close/1`](`port_close/1`) is however still fully synchronous because of
its error behavior.

Failure: `badarg` if `Port` is not an identifier of an open port, or the
registered name of an open port. If the calling process was previously linked to
the closed port, identified by `Port`, the exit signal from the port is
guaranteed to be delivered before this `badarg` exception occurs.

# `port_command`
*auto-imported* 

```erlang
-spec port_command(Port, Data) -> true when Port :: port() | atom(), Data :: iodata().
```

Sends data to a port. Same as `Port ! {PortOwner, {command, Data}}` except for
the error behavior and being synchronous (see below).

Any process can send data to a port with [`port_command/2`](`port_command/2`),
not only the port owner (the connected process).

For comparison: `Port ! {PortOwner, {command, Data}}` only fails with `badarg`
if `Port` does not refer to a port or a process. If `Port` is a closed port, the
data message disappears without a sound. If `Port` is open and the calling
process is not the port owner, the _port owner_ fails with `badsig`. The port
owner fails with `badsig` also if `Data` is an invalid I/O list.

Notice that any process can send to a port using
`Port ! {PortOwner, {command, Data}}` as if it itself was the port owner.

If the port is busy, the calling process is suspended until the port is not busy
any more.

As from Erlang/OTP R16, `Port ! {PortOwner, {command, Data}}` is truly
asynchronous. Notice that this operation has always been documented as an
asynchronous operation, while the underlying implementation has been
synchronous. [`port_command/2`](`port_command/2`) is however still fully
synchronous because of its error behavior.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Data` is an invalid I/O list.

> #### Warning {: .warning }
>
> Do not send data to an unknown port. Any undefined behavior is possible
> (including node crash) depending on how the port driver interprets the data.

# `port_command`
*auto-imported* 

```erlang
-spec port_command(Port, Data, OptionList) -> boolean()
                      when
                          Port :: port() | atom(),
                          Data :: iodata(),
                          Option :: force | nosuspend,
                          OptionList :: [Option].
```

Sends data to a port. [`port_command(Port, Data, [])`](`port_command/3`) equals
[`port_command(Port, Data)`](`port_command/2`).

If the port command is aborted, `false` is returned, otherwise `true`.

If the port is busy, the calling process is suspended until the port is not busy
anymore.

`Option`s:

- **`force`** - The calling process is not suspended if the port is busy,
  instead the port command is forced through. The call fails with a `notsup`
  exception if the driver of the port does not support this. For more
  information, see driver flag
  [`ERL_DRV_FLAG_SOFT_BUSY`](driver_entry.md#driver_flags).

- **`nosuspend`** - The calling process is not suspended if the port is busy,
  instead the port command is aborted and `false` is returned.

> #### Change {: .info }
>
> More options can be added in a future release.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Data` is an invalid I/O list.

- **`badarg`** - If `OptionList` is an invalid option list.

- **`notsup`** - If option `force` has been passed, but the driver of the port
  does not allow forcing through a busy port.

> #### Warning {: .warning }
>
> Do not send data to an unknown port. Any undefined behavior is possible
> (including node crash) depending on how the port driver interprets the data.

# `port_connect`
*auto-imported* 

```erlang
-spec port_connect(Port, Pid) -> true when Port :: port() | atom(), Pid :: pid().
```

Sets the port owner (the connected port) to `Pid`. Roughly the same as
`Port ! {Owner, {connect, Pid}}` except for the following:

- The error behavior differs, see below.
- The port does _not_ reply with `{Port,connected}`.
- `port_connect/1` is synchronous, see below.
- The new port owner gets linked to the port.

The old port owner stays linked to the port and must call
[`unlink(Port)`](`unlink/1`) if this is not desired. Any process can set the
port owner to be any process with [`port_connect/2`](`port_connect/2`).

For comparison: `Port ! {self(), {connect, Pid}}` only fails with `badarg` if
`Port` does not refer to a port or a process. If `Port` is a closed port,
nothing happens. If `Port` is an open port and the calling process is the port
owner, the port replies with `{Port, connected}` to the old port owner. Notice
that the old port owner is still linked to the port, while the new is not. If
`Port` is an open port and the calling process is not the port owner, the _port
owner_ fails with `badsig`. The port owner fails with `badsig` also if `Pid` is
not an existing local process identifier.

Notice that any process can set the port owner using
`Port ! {PortOwner, {connect, Pid}}` as if it itself was the port owner, but the
reply always goes to the port owner.

As from Erlang/OTP R16, `Port ! {PortOwner, {connect, Pid}}` is truly
asynchronous. Notice that this operation has always been documented as an
asynchronous operation, while the underlying implementation has been
synchronous. [`port_connect/2`](`port_connect/2`) is however still fully
synchronous because of its error behavior.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If the process identified by `Pid` is not an existing local
  process.

# `port_control`
*auto-imported* 

```erlang
-spec port_control(Port, Operation, Data) -> iodata() | binary()
                      when Port :: port() | atom(), Operation :: integer(), Data :: iodata().
```

Performs a synchronous control operation on a port. The meaning of `Operation`
and `Data` depends on the port, that is, on the port driver. Not all port
drivers support this control feature.

Returns a list of integers in the range 0..255, or a binary, depending on the
port driver. The meaning of the returned data also depends on the port driver.

Failures:

- **`badarg`** - If `Port` is not an open port or the registered name of an open
  port.

- **`badarg`** - If `Operation` cannot fit in a 32-bit integer.

- **`badarg`** - If the port driver does not support synchronous control
  operations.

- **`badarg`** - If the port driver so decides for any reason (probably
  something wrong with `Operation` or `Data`).
  > #### Warning {: .warning }
  >
  > Do not call [`port_control/3`](`port_control/3`) with an unknown `Port`
  > identifier and expect `badarg` exception. Any undefined behavior is possible
  > (including node crash) depending on how the port driver interprets the
  > supplied arguments.

# `port_info`

```erlang
-spec port_info(Port) -> Result
                   when
                       Port :: port() | atom(),
                       ResultItem ::
                           {registered_name, RegisteredName :: atom()} |
                           {id, Index :: non_neg_integer()} |
                           {connected, Pid :: pid()} |
                           {links, Pids :: [pid()]} |
                           {name, String :: string()} |
                           {input, Bytes :: non_neg_integer()} |
                           {output, Bytes :: non_neg_integer()} |
                           {os_pid, OsPid :: non_neg_integer() | undefined},
                       Result :: [ResultItem] | undefined.
```

Returns a list containing tuples with information about `Port`, or `undefined`
if the port is not open.

The order of the tuples is undefined, and all the tuples are not mandatory.
If the port is closed and the calling process was
previously linked to the port, the exit signal from the port is guaranteed to be
delivered before [`port_info/1`](`port_info/1`) returns `undefined`.

The result contains information about the following `Item`s:

- `registered_name` (if the port has a registered name)
- `id`
- `connected`
- `links`
- `name`
- `input`
- `output`

For more information about the different `Item`s, see `port_info/2`.

Failure: `badarg` if `Port` is not a local port identifier, or an atom.

# `port_info`

```erlang
-spec port_info(Port, Item :: connected) -> {connected, Pid} | undefined
                   when Port :: port() | atom(), Pid :: pid();
               (Port, Item :: id) -> {id, Index} | undefined
                   when Port :: port() | atom(), Index :: non_neg_integer();
               (Port, Item :: input) -> {input, Bytes} | undefined
                   when Port :: port() | atom(), Bytes :: non_neg_integer();
               (Port, Item :: links) -> {links, Pids} | undefined
                   when Port :: port() | atom(), Pids :: [pid()];
               (Port, Item :: locking) -> {locking, Locking} | undefined
                   when Port :: port() | atom(), Locking :: false | port_level | driver_level;
               (Port, Item :: memory) -> {memory, Bytes} | undefined
                   when Port :: port() | atom(), Bytes :: non_neg_integer();
               (Port, Item :: monitors) -> {monitors, Monitors} | undefined
                   when Port :: port() | atom(), Monitors :: [{process, pid()}];
               (Port, Item :: monitored_by) -> {monitored_by, MonitoredBy} | undefined
                   when Port :: port() | atom(), MonitoredBy :: [pid()];
               (Port, Item :: name) -> {name, Name} | undefined
                   when Port :: port() | atom(), Name :: string();
               (Port, Item :: os_pid) -> {os_pid, OsPid} | undefined
                   when Port :: port() | atom(), OsPid :: non_neg_integer() | undefined;
               (Port, Item :: output) -> {output, Bytes} | undefined
                   when Port :: port() | atom(), Bytes :: non_neg_integer();
               (Port, Item :: parallelism) -> {parallelism, Boolean} | undefined
                   when Port :: port() | atom(), Boolean :: boolean();
               (Port, Item :: queue_size) -> {queue_size, Bytes} | undefined
                   when Port :: port() | atom(), Bytes :: non_neg_integer();
               (Port, Item :: registered_name) -> {registered_name, RegisteredName} | [] | undefined
                   when Port :: port() | atom(), RegisteredName :: atom().
```

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
Returns information about `Port`.

If the port identified by `Port` is not open, `undefined` is returned. If the port is closed and the calling process was previously linked to the port, the exit signal from the port is guaranteed to be delivered before `port_info/2` returns `undefined`.

`Item` is one of the following and can be used to get various information about the `Port`.

- `connected` - returns `{connected, Pid}` where `Pid` is the process identifier of the process connected to the port.
- `id` - returns `{id, Index}` where `Index` is the internal index of the port. This index can be used to separate ports.
- `input` - returns `{input, Bytes}` where `Bytes` is the total number of bytes read from the port.
- `links` - returns `{links, Pids}` where `Pids` is a list of the process identifiers of the processes that the port is linked to.
- `locking` - returns `{locking, Locking}` where `Locking` is one of the following:
  * `port_level` (port-specific locking)
  * `driver_level` (driver-specific locking)
  Notice that these results are highly implementation-specific and can change in a future release.
  
  Since: OTP R16B
- `memory` {: #port_info_memory } - returns `{memory, Bytes}` where `Bytes` is the total number of bytes allocated for this port by the runtime system. The port itself can have allocated memory that is not included in `Bytes`.
  
  Since: OTP R16B
- `monitors` - returns `{monitors, Monitors}` where `Monitors` represent processes monitored by this port.
  
  Since: OTP R16B
- `monitored_by` - returns `{monitored_by, MonitoredBy}` where `MonitoredBy` is a list of pids that are monitoring given port at the moment.
  
  Since: OTP 19.0
- `name` - returns `{name, Name}` where `Name` is the command name set by `open_port/2`.
- `os_pid` - returns `{os_pid, OsPid}` where `OsPid` is the process identifier (or equivalent) of an OS process created with [`open_port({spawn | spawn_executable, Command}, Options)`](`open_port/2`). If the port is not the result of spawning an OS process, the value is `undefined`.
  
  Since: OTP R16B
- `output` - returns `{output, Bytes}` where `Bytes` is the total number of bytes written to the port from Erlang processes using `port_command/2`, `port_command/3`, or `Port ! {Owner, {command, Data}`.
- `parallelism` - returns `{parallelism, Boolean}` where `Boolean` corresponds to the port parallelism hint used by this port. For more information, see option [`parallelism`](`m:erlang#open_port_parallelism`) of `open_port/2`.
  
  Since: OTP R16B
- `queue_size` - returns `{queue_size, Bytes}` where `Bytes` is the total number of bytes queued by the port using the ERTS driver queue implementation.
  
  Since: OTP R16B
- `registered_name` - returns `{registered_name, RegisteredName}` where `RegisteredName` is the registered name of the port. If the port has no registered name, `[]` is returned.

Failure: `badarg` if `Port` is not a local port identifier, or an atom.

# `port_to_list`
*auto-imported* 

```erlang
-spec port_to_list(Port) -> string() when Port :: port().
```

Returns a string corresponding to the text representation of the port identifier
`Port`.

## Examples

```erlang
1> erlang:port_to_list(#Port<0.0>).
"#Port<0.0>"
```

# `ports`

```erlang
-spec ports() -> [port()].
```

Returns a list of port identifiers corresponding to all the ports existing on
the local node.

Notice that an exiting port exists, but is not open.

# `pre_loaded`
*auto-imported* 

```erlang
-spec pre_loaded() -> [module()].
```

Returns a list of Erlang modules that are preloaded in the run-time system.

Pre-loaded modules are Erlang modules that are needed to bootstrap the system to
load the first Erlang modules from either disk or by using `m:erl_boot_server`.

# `process_display`

```erlang
-spec process_display(Pid, Type) -> true when Pid :: pid(), Type :: backtrace.
```

Writes information about the local process `Pid` on [standard error](`t:io:standard_error/0`).

The only allowed value for the atom `Type` is `backtrace`, which shows the contents of
the call stack, including information about the call chain, with the current
function printed first. The format of the output is not further defined.

# `process_flag`
*auto-imported* 

```erlang
-spec process_flag(async_dist, Boolean) -> OldBoolean when Boolean :: boolean(), OldBoolean :: boolean();
                  (trap_exit, Boolean) -> OldBoolean when Boolean :: boolean(), OldBoolean :: boolean();
                  (error_handler, Module) -> OldModule when Module :: atom(), OldModule :: atom();
                  (fullsweep_after, FullsweepAfter) -> OldFullsweepAfter
                      when FullsweepAfter :: non_neg_integer(), OldFullsweepAfter :: non_neg_integer();
                  (min_heap_size, MinHeapSize) -> OldMinHeapSize
                      when MinHeapSize :: non_neg_integer(), OldMinHeapSize :: non_neg_integer();
                  (min_bin_vheap_size, MinBinVHeapSize) -> OldMinBinVHeapSize
                      when MinBinVHeapSize :: non_neg_integer(), OldMinBinVHeapSize :: non_neg_integer();
                  (max_heap_size, MaxHeapSize) -> OldMaxHeapSize
                      when MaxHeapSize :: max_heap_size(), OldMaxHeapSize :: max_heap_size();
                  (message_queue_data, MQD) -> OldMQD
                      when MQD :: message_queue_data(), OldMQD :: message_queue_data();
                  (priority, Level) -> OldLevel
                      when Level :: priority_level(), OldLevel :: priority_level();
                  (save_calls, N) -> OldN when N :: 0..10000, OldN :: 0..10000;
                  (sensitive, Boolean) -> OldBoolean when Boolean :: boolean(), OldBoolean :: boolean();
                  ({monitor_nodes, term()}, term()) -> term();
                  (monitor_nodes, term()) -> term().
```

Sets the process flag indicated to the specified value. Returns the previous value
of the flag.

`Flag` is one of the following:

- ```erlang
  process_flag(async_dist, boolean())
  ```
  {: #process_flag_async_dist }
  
  Enable or disable _fully asynchronous distributed signaling_ for the calling
  process. When disabled, which is the default, the process sending a distributed
  signal will block in the send operation if the buffer for the distribution
  channel reach the [distribution buffer busy limit](erl_cmd.md#%2Bzdbbl). The
  process will remain blocked until the buffer shrinks enough. This might in some
  cases take a substantial amount of time. When `async_dist` is enabled, send
  operations of distributed signals will always buffer the signal on the outgoing
  distribution channel and then immediately return. That is, these send operations
  will _never_ block the sending process.
  
  > #### Note {: .info }
  >
  > Since no flow control is enforced by the runtime system when `async_dist`
  > process flag is enabled, you need to make sure that flow control for such data
  > is implemented, or that the amount of such data is known to always be limited.
  > Unlimited signaling with `async_dist` enabled in the absence of flow control
  > will typically cause the sending runtime system to crash on an out of memory
  > condition.
  
  Blocking due to disabled `async_dist` can be monitored by
  [`trace:system()`](`trace:system/3`) using the
  [`busy_dist_port`](`m:trace#busy_dist_port`) option. Only data buffered by
  processes which (at the time of sending a signal) have disabled `async_dist`
  will be counted when determining whether or not an operation should block the
  caller.
  
  The `async_dist` flag can also be set on a new process when spawning it using
  the [`spawn_opt()`](`spawn_opt/4`) BIF with the option
  [`{async_dist, Enable}`](#spawn_opt_async_dist). The default
  `async_dist` flag to use on newly spawned processes can be set by passing the
  command line argument [`+pad <boolean>`](erl_cmd.md#%2Bpad) when starting the
  runtime system. If the `+pad <boolean>` command line argument is not passed, the
  default value of the `async_dist` flag will be `false`.
  
  You can inspect the state of the `async_dist` process flag of a process by
  calling [`process_info(Pid, async_dist)`](#process_info_async_dist).
  
- ```erlang
  process_flag(trap_exit, boolean())
  ```
  {: #process_flag_trap_exit }
  
  When `trap_exit` is set to `true`, exit signals arriving to a process are
  converted to `{'EXIT', From, Reason}` messages, which can be received as
  ordinary messages. If `trap_exit` is set to `false`, the process exits if it
  receives an exit signal other than `normal` and the exit signal is propagated to
  its linked processes. Application processes are normally not to trap exits.
  
  See also `exit_signal/2`.
  
- ```erlang
  process_flag(error_handler, module())
  ```
  {: #process_flag_error_handler }
  
  Used by a process to redefine the `m:error_handler` for undefined function calls and
  undefined registered processes. Use this flag with substantial caution, as code
  auto-loading depends on the correct operation of the error handling module.
  
- ```erlang
  process_flag(fullsweep_after,  non_neg_integer())
  ```
  
  Changes the maximum number of generational collections before forcing a
  fullsweep for the calling process.
  
- ```erlang
  process_flag(min_heap_size, non_neg_integer())
  ```
  {: #process_flag_min_heap_size }
  
  Changes the minimum heap size for the calling process.
  
- ```erlang
  process_flag(min_bin_vheap_size, non_neg_integer())
  ```
  
  Changes the minimum binary virtual heap size for the calling process.
  
- ```erlang
  process_flag(max_heap_size, max_heap_size())
  ```
  {: #process_flag_max_heap_size }
  
  This flag sets the maximum heap size for the calling process. If `MaxHeapSize`
  is an integer, the system default values for `kill` and `error_logger` are used.
  
  For details on how the heap grows, see
  [Sizing the heap](GarbageCollection.md#sizing-the-heap) in the ERTS internal
  documentation.
  
  - **`size`** - The maximum size in words of the process. If set to zero, the
    heap size limit is disabled. `badarg` is be thrown if the value is smaller
    than [`min_heap_size`](#process_flag_min_heap_size). The size check
    is only done when a garbage collection is triggered.
  
    `size` is the entire heap of the process when garbage collection is triggered.
    This includes all generational heaps, the process stack, any
    [messages that are considered to be part of the heap](#process_flag_message_queue_data),
    and any extra memory that the garbage collector needs during collection.
  
    `size` is the same as can be retrieved using
    [`erlang:process_info(Pid, total_heap_size)`](#process_info_total_heap_size),
    or by adding `heap_block_size`, `old_heap_block_size` and `mbuf_size` from
    [`erlang:process_info(Pid, garbage_collection_info)`](#process_info_garbage_collection_info).
  
  - **`kill`** - When set to `true`, the runtime system sends an untrappable exit
    signal with reason `kill` to the process if the maximum heap size is reached.
    The garbage collection that triggered the `kill` is not completed, instead the
    process exits as soon as possible. When set to `false`, no exit signal is sent
    to the process, instead it continues executing.
  
    If `kill` is not defined in the map, the system default will be used. The
    default system default is `true`. It can be changed by either option
    [\+hmaxk](erl_cmd.md#%2Bhmaxk) in [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  - **`error_logger`** - When set to `true`, the runtime system logs an error
    event via `m:logger`, containing details about the process when the maximum
    heap size is reached. One log event is sent each time the limit is reached.
  
    If `error_logger` is not defined in the map, the system default is used. The
    default system default is `true`. It can be changed by either the option
    [\+hmaxel](erl_cmd.md#%2Bhmaxel) int [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  - **`include_shared_binaries`** - When set to `true`, off-heap binaries are
    included in the total sum compared against the `size` limit. Off-heap binaries
    are typically larger binaries that may be shared between processes. The size
    of a shared binary is included by all processes that are referring it. Also,
    the entire size of a large binary may be included even if only a smaller part
    of it is referred by the process.
  
    If `include_shared_binaries` is not defined in the map, the system default is
    used. The default system default is `false`. It can be changed by either the
    option [\+hmaxib](erl_cmd.md#%2Bhmaxib) in [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  The heap size of a process is quite hard to predict, especially the amount of
  memory that is used during the garbage collection. When contemplating using this
  option, it is recommended to first run it in production with `kill` set to
  `false` and inspect the log events to see what the normal peak sizes of the
  processes in the system is and then tune the value accordingly.
  
- ```erlang
  process_flag(message_queue_data, message_queue_data())
  ```
  {: #process_flag_message_queue_data }
  
  Determines how messages in the message queue are stored, as follows:
  
  - **`off_heap`** - _All_ messages in the message queue will be stored outside
    the process heap. This implies that _no_ messages in the message queue will be
    part of a garbage collection of the process.
  
  - **`on_heap`** - All messages in the message queue will eventually be placed on
    the process heap. They can, however, be temporarily stored off the heap. This
    is how messages have always been stored up until ERTS 8.0.
  
  The default value of the `message_queue_data` process flag is determined by the
  command-line argument [`+hmqd`](erl_cmd.md#%2Bhmqd) in [erl](erl_cmd.md).
  
  If the process may potentially accumulate a large number of messages in its
  queue it is recommended to set the flag value to `off_heap`. This is due to the
  fact that the garbage collection of a process that has a large number of
  messages stored on the heap can become extremely expensive and the process can
  consume large amounts of memory. The performance of the actual message passing
  is, however, generally better when the flag value is `on_heap`.
  
  Changing the flag value causes any existing messages to be moved. The move
  operation is initiated, but not necessarily completed, by the time the function
  returns.
  
- ```erlang
  process_flag(priority, priority_level())
  ```
  {: #process_flag_priority }
  
  Sets the process priority. `Level` is an atom. Four priority levels exist:
  `low`, `normal`, `high`, and `max`. Default is `normal`.
  
  > #### Note {: .info }
  >
  > Priority level `max` is reserved for internal use in the Erlang runtime
  > system, and is _not_ to be used by others.
  
  Internally in each priority level, processes are scheduled in a round robin
  fashion.
  
  Execution of processes on priority `normal` and `low` are interleaved. Processes
  on priority `low` are selected for execution less frequently than processes on
  priority `normal`.
  
  When runnable processes on priority `high` exist, no processes on priority `low`
  or `normal` are selected for execution. Notice however that this does _not_ mean
  that no processes on priority `low` or `normal` can run when processes are
  running on priority `high`. When using multiple schedulers, more processes can
  be running in parallel than processes on priority `high`. That is, a `low` and a
  `high` priority process can execute at the same time.
  
  When runnable processes on priority `max` exist, no processes on priority `low`,
  `normal`, or `high` are selected for execution. As with priority `high`,
  processes on lower priorities can execute in parallel with processes on priority
  `max`.
  
  Scheduling is pre-emptive. Regardless of priority, a process is pre-empted when
  it has consumed more than a certain number of reductions since the last time it
  was selected for execution.
  
  > #### Note {: .info }
  >
  > Do not depend on the scheduling to remain exactly as it is today. Scheduling
  > is likely to be changed in a future release to use available processor cores
  > better.
  
  There is _no_ automatic mechanism for avoiding priority inversion, such as
  priority inheritance or priority ceilings. When using priorities, take this into
  account and handle such scenarios by yourself.
  
  Making calls from a `high` priority process into code that you has no control
  over can cause the `high` priority process to wait for a process with lower
  priority. That is, effectively decreasing the priority of the `high` priority
  process during the call. Even if this is not the case with one version of the
  code that you have no control over, it can be the case in a future version of
  it. This can, for example, occur if a `high` priority process triggers code
  loading, as the code server runs on priority `normal`.
  
  Other priorities than `normal` are normally not needed. When other priorities
  are used, use them with care, _especially_ priority `high`. A process on
  priority `high` is only to perform work for short periods. Busy looping for long
  periods in a `high` priority process causes most likely problems, as important
  OTP servers run on priority `normal`.
  
- ```erlang
  process_flag(save_calls, 0..10000)
  ```
  
  `N` must be an integer in the interval 0..10000. If `N` > 0, call saving is made
  active for the process. This means that information about the `N` most recent
  global function calls, BIF calls, sends, and receives made by the process are
  saved in a list, which can be retrieved with
  [`process_info(Pid, last_calls)`](`process_info/2`). A global function call is
  one in which the module of the function is explicitly mentioned. Only a fixed
  amount of information is saved, as follows:
  
  - A tuple `{Module, Function, Arity}` for function calls
  - The atoms `send`, `'receive'`, and `timeout` for sends and receives
    (`'receive'` when a message is received and `timeout` when a receive times
    out)
  
  If `N` = 0, call saving is disabled for the process, which is the default.
  Whenever the size of the call saving list is set, its contents are reset.
  
- ```erlang
  process_flag(sensitive, boolean())
  ```
  
  Sets or clears flag `sensitive` for the current process. When a process has been
  marked as sensitive by calling
  [`process_flag(sensitive, true)`](`process_flag/2`), features in the runtime
  system that can be used for examining the data or inner working of the process
  are silently disabled.
  
  Features that are disabled include (but are not limited to) the following:
  
  - Tracing. Trace flags can still be set for the process, but no trace messages
    of any kind are generated. (If flag `sensitive` is turned off, trace messages
    are again generated if any trace flags are set.)
  - Sequential tracing. The sequential trace token is propagated as usual, but no
    sequential trace messages are generated.
  
  `process_info/1,2` cannot be used to read out the message queue or the process
  dictionary (both are returned as empty lists).
  
  Stack back-traces cannot be displayed for the process.
  
  In crash dumps, the stack, messages, and the process dictionary are omitted.
  
  If `{save_calls,N}` has been set for the process, no function calls are saved to
  the call saving list. (The call saving list is not cleared. Also, send, receive,
  and time-out events are still added to the list.)

# `process_flag`
*auto-imported* 

```erlang
-spec process_flag(Pid, Flag, Value) -> OldValue
                      when
                          Pid :: pid(),
                          Flag :: save_calls,
                          Value :: non_neg_integer(),
                          OldValue :: non_neg_integer().
```

Sets certain flags for the process `Pid`, in the same manner as
`process_flag/2`. Returns the old value of the flag. The valid values for `Flag`
are only a subset of those allowed in [`process_flag/2`](`process_flag/2`),
namely `save_calls`.

Failure: `badarg` if `Pid` is not a local process.

# `process_info`
*auto-imported* 

```erlang
-spec process_info(Pid) -> Info
                      when
                          Pid :: pid(),
                          Info :: [InfoTuple] | undefined,
                          InfoTuple :: process_info_result_item().
```

Returns a list containing `InfoTuple`s with miscellaneous information about the
process identified by `Pid`, or `undefined` if the process is not alive.

The order of the `InfoTuple`s is undefined and all `InfoTuple`s are not
mandatory. The `InfoTuple`s part of the result can be changed without prior
notice.

The `InfoTuple`s with the following items are part of the result:

- `current_function`
- `initial_call`
- `status`
- `message_queue_len`
- `links`
- `dictionary`
- `trap_exit`
- `error_handler`
- `priority`
- `group_leader`
- `total_heap_size`
- `heap_size`
- `stack_size`
- `reductions`
- `garbage_collection`

If the process identified by `Pid` has a registered name, also an `InfoTuple`
with item `registered_name` is included.

For information about specific `InfoTuple`s, see `process_info/2`.

> #### Warning {: .warning }
>
> This BIF is intended for _debugging only_. For all other purposes, use
> `process_info/2`.

Failure: `badarg` if `Pid` is not a local process.

# `process_info`
*auto-imported* 

```erlang
-spec process_info(Pid, Item) -> InfoTuple | [] | undefined
                      when
                          Pid :: pid(),
                          Item :: process_info_item(),
                          InfoTuple :: process_info_result_item();
                  (Pid, ItemList) -> InfoTupleList | [] | undefined
                      when
                          Pid :: pid(),
                          ItemList :: [Item],
                          Item :: process_info_item(),
                          InfoTupleList :: [InfoTuple],
                          InfoTuple :: process_info_result_item().
```

Returns information about the process identified by `Pid`, as specified by
`Item` or `ItemList`. Returns `undefined` if the process is not alive.

If the process is alive and a single `Item` is specified, the returned value is
the corresponding `InfoTuple`, unless `Item =:= registered_name` and the process
has no registered name. In this case, `[]` is returned. This strange behavior is
because of historical reasons, and is kept for backward compatibility.

If `ItemList` is specified, the result is `InfoTupleList`. The `InfoTuple`s in
`InfoTupleList` are included with the corresponding `Item`s in the same order as
the `Item`s were included in `ItemList`. Valid `Item`s can be included multiple
times in `ItemList`.

Getting process information follows the signal ordering guarantees described in
the [Processes Chapter](`e:system:ref_man_processes.md#signals`) in the _Erlang
Reference Manual_.

> #### Note {: .info }
>
> If `registered_name` is part of `ItemList` and the process has no name
> registered, a `{registered_name, []}`, `InfoTuple` _will_ be included in the
> resulting `InfoTupleList`. This behavior is different when a single
> `Item =:= registered_name` is specified, and when
> [`process_info/1`](`process_info/1`) is used.

Valid `InfoTuple`s with corresponding `Item`s:

- **`{async_dist, Enabled}`{: #process_info_async_dist }** - Current value of the
  [`async_dist`](#process_flag_async_dist) process flag.

  Since: OTP 25.3

- **`{backtrace, Bin}`** - Binary `Bin` contains the same information as the
  output from `erlang:process_display(Pid, backtrace)`. Use
  [`binary_to_list/1`](`binary_to_list/1`) to obtain the string of characters
  from the binary.

- **`{binary, BinInfo}`** - `BinInfo` is a list containing miscellaneous
  information about binaries on the heap of this process. This `InfoTuple` can
  be changed or removed without prior notice. In the current implementation
  `BinInfo` is a list of tuples. The tuples contain; `BinaryId`, `BinarySize`,
  `BinaryRefcCount`.

  Depending on the value of the
  [`message_queue_data`](#process_flag_message_queue_data) process
  flag the message queue may be stored on the heap.

- **`{catchlevel, CatchLevel}`** - `CatchLevel` is the number of currently
  active catches in this process. This `InfoTuple` can be changed or removed
  without prior notice.

- **`{current_function, {Module, Function, Arity} | undefined}`** - `Module`,
  `Function`, `Arity` is the current function call of the process. The value
  `undefined` can be returned if the process is currently executing native
  compiled code.

- **`{current_location, {Module, Function, Arity, Location}}`** - `Module`,
  `Function`, `Arity` is the current function call of the process. `Location` is
  a list of two-tuples describing the location in the source code.

- **`{current_stacktrace, Stack}`**{: #process_info_current_stacktrace } -
  Returns the current call stack back-trace
  (_stacktrace_) of the process. The stack has the same format as in the `catch`
  part of a `try`. See
  [The call-stack back trace (stacktrace)](`e:system:errors.md#stacktrace`). The
  depth of the stacktrace is truncated according to the `backtrace_depth` system
  flag setting.

- **`{dictionary, Dictionary}`** - `Dictionary` is the process dictionary.

- **`{{dictionary, Key}, Value}`** - `Value` associated with `Key` in the
  process dictionary.

- **`{error_handler, Module}`** - `Module` is the `m:error_handler` module used by
  the process (for undefined function calls, for example).

- **`{garbage_collection, GCInfo}`** - `GCInfo` is a list containing
  miscellaneous information about garbage collection for this process. The
  content of `GCInfo` can be changed without prior notice.

- **`{garbage_collection_info, GCInfo}`{: #process_info_garbage_collection_info }** -
  `GCInfo` is a list containing miscellaneous detailed information about
  garbage collection for this process. The content of `GCInfo` can be changed
  without prior notice. For details about the meaning of each item, see
  [`gc_minor_start`](`m:trace#gc_minor_start`) in `trace:process/4`.

- **`{group_leader, GroupLeader}`** - `GroupLeader` is the group leader for the
  I/O of the process.

- **`{heap_size, Size}`** - `Size` is the size in words of the youngest heap
  generation of the process. This generation includes the process stack. This
  information is highly implementation-dependent, and can change if the
  implementation changes.

- **`{initial_call, {Module, Function, Arity}}`** - `Module`, `Function`,
  `Arity` is the initial function call with which the process was spawned.

- **`{links, PidsAndPorts}`** - `PidsAndPorts` is a list of process identifiers
  and port identifiers, with processes or ports to which the process has a link.

- **`{label, Label}`** -
  `Label` is the label for the process. See `proc_lib:get_label/1`.

   Since: OTP 27.2

- **`{last_calls, false|Calls}`** - The value is `false` if call saving is not
  active for the process (see `process_flag/3`). If call saving is active, a
  list is returned, in which the last element is the most recent called.

- **`{memory, Size}`** - [](){: #process_info_memory } `Size` is the size in
  bytes of the process. This includes call stack, heap, and internal structures.

- **`{message_queue_len, MessageQueueLen}`** - `MessageQueueLen` is the number
  of messages currently in the message queue of the process. This is the length
  of the list `MessageQueue` returned as the information item `messages` (see
  below).

- **`{messages, MessageQueue}`** - `MessageQueue` is a list of the messages to
  the process, which have not yet been processed.

- **`{min_heap_size, MinHeapSize}`** - `MinHeapSize` is the minimum heap size
  for the process.

- **`{min_bin_vheap_size, MinBinVHeapSize}`** - `MinBinVHeapSize` is the minimum
  binary virtual heap size for the process.

- **`{monitored_by, MonitoredBy}`** - A list of identifiers for all the
  processes, ports and NIF resources, that are monitoring the process.

- **`{monitors, Monitors}`** - A list of monitors (started by
  [`monitor/2`](`monitor/2`)) that are active for the process. For a local
  process monitor or a remote process monitor by a process identifier, the list
  consists of:

  - **`{process, Pid}`** - Process is monitored by pid.

  - **`{process, {RegName, Node}}`** - Local or remote process is monitored by
    name.

  - **`{port, PortId}`** - Local port is monitored by port id.

  - **`{port, {RegName, Node}}`** - Local port is monitored by name. Please
    note, that remote port monitors are not supported, so `Node` will always be
    the local node name.

- **`{message_queue_data, MQD}`** - `MQD` is the current value of the
  `message_queue_data` process flag, which can be either `off_heap` or
  `on_heap`. For more information, see the documentation of
  [`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

- **`{parent, Pid}`** - `Pid` is the identifier of the parent process, the one
  that spawned current process. When the process does not have a parent
  `undefined` is returned. Only the initial process (`init`) on a node lacks a
  parent, though.

  Since: OTP 25.0

- **`{priority, Level}`** - `Level` is the current priority level for the
  process. For more information on priorities, see
  [`process_flag(priority, Level)`](#process_flag_priority).

- **`{priority_messages, Enabled}`** - Since OTP 28.0

  If `Enabled` equals `true`, the process has
  [enabled priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  enabled priority message reception for at least one type of messages.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.

- **`{reductions, Number}`** - `Number` is the number of reductions executed by
  the process.

- **`{registered_name, Atom}`** - `Atom` is the registered process name. If the
  process has no registered name, this tuple is not present in the list.

- **`{sequential_trace_token, [] | SequentialTraceToken}`** -
  `SequentialTraceToken` is the sequential trace token for the process. This
  `InfoTuple` can be changed or removed without prior notice.

- **`{stack_size, Size}`** - `Size` is the stack size, in words, of the process.

- **`{status, Status}`** - `Status` is the status of the process and is one of
  the following:

  - `exiting`
  - `garbage_collecting`
  - `waiting` (for a message)
  - `running`
  - `runnable` (ready to run, but another process is running)
  - `suspended` (suspended on a "busy" port or by the BIF
    `erlang:suspend_process/1,2`)

- **`{suspending, SuspendeeList}`** - `SuspendeeList` is a list of
  `{Suspendee, ActiveSuspendCount, OutstandingSuspendCount}` tuples. `Suspendee`
  is the process identifier of a process that has been, or is to be, suspended
  by the process identified by `Pid` through the BIF
  [`erlang:suspend_process/2`](`suspend_process/2`) or
  [`erlang:suspend_process/1`](`suspend_process/1`).

  `ActiveSuspendCount` is the number of times `Suspendee` has been suspended by
  `Pid`. `OutstandingSuspendCount` is the number of not yet completed suspend
  requests sent by `Pid`, that is:

  - If `ActiveSuspendCount =/= 0`, `Suspendee` is currently in the suspended
    state.
  - If `OutstandingSuspendCount =/= 0`, option `asynchronous` of
    `erlang:suspend_process/2` has been used and the suspendee has not yet been
    suspended by `Pid`.

  Notice that `ActiveSuspendCount` and `OutstandingSuspendCount` are not the
  total suspend count on `Suspendee`, only the parts contributed by `Pid`.

- **`{total_heap_size, Size}`{: #process_info_total_heap_size }** - `Size` is
  the total size, in words, of all heap fragments of the process. This includes
  the process stack and any unreceived messages that are considered to be part
  of the heap.

- **`{trace, InternalTraceFlags}`** - `InternalTraceFlags` is an integer
  representing the internal trace flag for this process. This `InfoTuple` can be
  changed or removed without prior notice.

- **`{trap_exit, Boolean}`** - `Boolean` is `true` if the process is trapping
  exits, otherwise `false`.

Notice that not all implementations support all these `Item`s.

Failures:

- **`badarg`** - If `Pid` is not a local process.

- **`badarg`** - If `Item` is an invalid item.

# `processes`
*auto-imported* 

```erlang
-spec processes() -> [pid()].
```

Returns a list of process identifiers corresponding to all the processes
currently existing on the local node.

Notice that an exiting process exists, but is not alive. That is,
[`is_process_alive/1`](`is_process_alive/1`) returns `false` for an exiting
process, but its process identifier is part of the result returned from
`processes/0`.

Example:

```erlang
> processes().
[<0.0.0>,<0.2.0>,<0.4.0>,<0.5.0>,<0.7.0>,<0.8.0>]
```

# `processes_iterator`
*since OTP 28.0* 

```erlang
-spec processes_iterator() -> processes_iter_ref().
```

Returns a processes iterator that can be used in
[`processes_next/1`](`processes_next/1`).

# `processes_next`
*since OTP 28.0* 

```erlang
-spec processes_next(Iter) -> {Pid, NewIter} | none
                        when Iter :: processes_iter_ref(), NewIter :: processes_iter_ref(), Pid :: pid().
```

Returns a 2-tuple, consisting of one process identifier and a new processes
iterator. If the process iterator has run out of processes in the process table,
`none` will be returned.

The two major benefits of using the `processes_iterator/0`/`processes_next/1`
BIFs instead of using the `processes/0` BIF are that they scale better since
no locking is needed, and you do not risk getting a huge list allocated on the
heap if there are a huge amount of processes alive in the system.

Example:

```erlang
> I0 = erlang:processes_iterator(), ok.
ok
> {Pid1, I1} = erlang:processes_next(I0), Pid1.
<0.0.0>,
> {Pid2, I2} = erlang:processes_next(I1), Pid2.
<0.1.0>
```

> #### Note {: .info }
>
> This BIF has less consistency guarantee than [`processes/0`](`processes/0`).
> Process identifiers returned from consecutive calls of this BIF may not be a
> consistent snapshot of all elements existing in the table during any of the
> calls. The process identifier of a process that is alive before
> `processes_iterator/0` is called and continues to be alive until
> `processes_next/1` returns `none` is guaranteed to be part of the result
> returned from one of the calls to `processes_next/1`.

# `purge_module`
*auto-imported* 

```erlang
-spec purge_module(Module) -> true when Module :: atom().
```

Removes old code for `Module`. Before this BIF is used, `check_process_code/2`
is to be called to check that no processes execute old code in the module.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.

> #### Change {: .info }
>
> As from ERTS 8.0 (Erlang/OTP 19), any lingering processes that still execute
> the old code is killed by this function. In earlier versions, such incorrect
> use could cause much more fatal failures, like emulator crash.

Failure: `badarg` if there is no old code for `Module`.

# `put`
*auto-imported* 

```erlang
-spec put(Key, Val) -> term() when Key :: term(), Val :: term().
```

Adds a new `Key` to the process dictionary, associated with the value `Val`, and
returns `undefined`. If `Key` exists, the old value is deleted and replaced by
`Val`, and the function returns the old value.

The average time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
1> X = put(name, walrus).
undefined
2> Y = put(name, carpenter).
walrus
3> Z = get(name).
carpenter
4> erase(name).
5> get(name).
undefined
```

> #### Note {: .info }
>
> The values stored when `put` is evaluated within the scope of a `catch` are
> not retracted if a `throw` is evaluated, or if an error occurs.

# `raise`

```erlang
-spec raise(Class, Reason, Stacktrace) -> badarg
               when Class :: error | exit | throw, Reason :: term(), Stacktrace :: raise_stacktrace().
```

Raises an exception of the specified class, reason, and call stack backtrace
(_stacktrace_).

`Class` is `error`, `exit`, or `throw`. So, if it were not for the stacktrace,
`erlang:raise(Class, Reason, Stacktrace)` is equivalent to
`erlang:Class(Reason)` (given that `Class` is a valid class).

`Reason` can be any term.

`Stacktrace` is a list as provided in a try-catch clause.

```erlang
try
    ...
catch Class:Reason:Stacktrace ->
    ...
end
```

That is, a list of four-tuples `{Module, Function, Arity | Args, ExtraInfo}`,
where `Module` and `Function` are atoms, and the third element is an integer
arity or an argument list. The stacktrace can also contain
`{Fun, Args, ExtraInfo}` tuples, where `Fun` is a local fun and `Args` is an
argument list.

Element `ExtraInfo` at the end is optional. Omitting it is equivalent to
specifying an empty list.

The stacktrace is used as the exception stacktrace for the calling process; it
is truncated to the current maximum stacktrace depth.

As evaluating this function causes the process to terminate, it has no return
value unless the arguments are invalid, in which case the function _returns the
error reason_ `badarg`. If you want to be sure not to return, you can call
[`error(erlang:raise(Class, Reason, Stacktrace))`](`error/1`) and hope to
distinguish exceptions later.

See the reference manual about [errors and error handling](`e:system:errors.md`)
for more information about exception classes and how to catch exceptions.

# `read_timer`

```erlang
-spec read_timer(TimerRef) -> Result
                    when TimerRef :: reference(), Time :: non_neg_integer(), Result :: Time | false.
```

# `read_timer`
*since OTP 18.0* 

```erlang
-spec read_timer(TimerRef, Options) -> Result | ok
                    when
                        TimerRef :: reference(),
                        Async :: boolean(),
                        Option :: {async, Async},
                        Options :: [Option],
                        Time :: non_neg_integer(),
                        Result :: Time | false.
```

Reads the state of a timer that has been created by either
[`erlang:start_timer`](`start_timer/4`) or
[`erlang:send_after`](`send_after/4`). `TimerRef` identifies the timer, and was
returned by the BIF that created the timer.

`Options`:

- **`{async, Async}`** - Asynchronous request for state information. `Async`
  defaults to `false`, which causes the operation to be performed synchronously.
  In this case, the `Result` is returned by `erlang:read_timer`. When `Async` is
  `true`, `erlang:read_timer` sends an asynchronous request for the state
  information to the timer service that manages the timer, and then returns
  `ok`. A message on the format `{read_timer, TimerRef, Result}` is sent to the
  caller of `erlang:read_timer` when the operation has been processed.

More `Option`s can be added in the future.

If `Result` is an integer, it represents the time in milliseconds left until the
timer expires.

If `Result` is `false`, a timer corresponding to `TimerRef` could not be found.
This because the timer had expired, or been canceled, or because `TimerRef`
never has corresponded to a timer. Even if the timer has expired, it does not
tell you whether or not the time-out message has arrived at its destination yet.

> #### Note {: .info }
>
> The timer service that manages the timer can be co-located with another
> scheduler than the scheduler that the calling process is executing on. If so,
> communication with the timer service takes much longer time than if it is
> located locally. If the calling process is in a critical path, and can do
> other things while waiting for the result of this operation, you want to use
> option `{async, true}`. If using option `{async, false}`, the calling process
> is blocked until the operation has been performed.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:start_timer/4`](`start_timer/4`), and
[`erlang:cancel_timer/2`](`cancel_timer/2`).

# `ref_to_list`
*auto-imported* 

```erlang
-spec ref_to_list(Ref) -> string() when Ref :: reference().
```

Returns a string corresponding to the text representation of `Ref`.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.

## Examples

```erlang
1> ref_to_list(#Ref<0.0.0.0>).
"#Ref<0.0.0.0>"
```

# `register`
*auto-imported* 

```erlang
-spec register(RegName, PidOrPort) -> true when RegName :: atom(), PidOrPort :: port() | pid().
```

Registers the name `RegName` with a process identifier (pid) or a port
identifier in the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`).
`RegName`, which must be an atom, can be used instead of the pid or port
identifier in send operator (`RegName ! Message`) and most other BIFs that take
a pid or port identifies as an argument.

For example:

```erlang
1> Pid = spawn(fun() -> receive after infinity -> ok end end).
2> register(db, Pid).
true
```

The registered name is considered a
[Directly Visible Erlang Resource](`e:system:ref_man_processes.md#visible-resources`)
and is automatically unregistered when the process terminates.

Failures:

- **`badarg`** - If `PidOrPort` is not an existing local process or port.

- **`badarg`** - If `RegName` is already in use.

- **`badarg`** - If the process or port is already registered (already has a
  name).

- **`badarg`** - If `RegName` is the atom `undefined`.

# `registered`
*auto-imported* 

```erlang
-spec registered() -> [RegName] when RegName :: atom().
```

Returns a list of names that have been registered using `register/2`.

For example:

```erlang
> registered().
[code_server, file_server, init, user, my_db]
```

# `resume_process`

```erlang
-spec resume_process(Suspendee) -> true when Suspendee :: pid().
```

Decreases the suspend count on the process identified by `Suspendee`.

`Suspendee` is previously to have been suspended through
[`erlang:suspend_process/2`](`suspend_process/2`) or
[`erlang:suspend_process/1`](`suspend_process/1`) by the process calling
`erlang:resume_process(Suspendee)`. When the suspend count on `Suspendee`
reaches zero, `Suspendee` is resumed, that is, its state is changed from
suspended into the state it had before it was suspended.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.

Failures:

- **`badarg`** - If `Suspendee` is not a process identifier.

- **`badarg`** - If the process calling `erlang:resume_process/1` had not
  previously increased the suspend count on the process identified by
  `Suspendee`.

- **`badarg`** - If the process identified by `Suspendee` is not alive.

# `round`
*auto-imported* *allowed in guard tests* 

```erlang
-spec round(Number) -> integer() when Number :: number().
```

Returns an integer by rounding `Number` to the nearest integer.

Example:

```erlang
1> round(42.1).
42
2> round(5.5).
6
3> round(-5.5).
-6
4> round(36028797018963969.0).
36028797018963968
```

In the last example, [`round(36028797018963969.0)`](`round/1`) evaluates to
`36028797018963968`. The reason for this is that the number
`36028797018963969.0` cannot be represented exactly as a float value. Instead,
the float literal is represented as `36028797018963968.0`, which is the closest
number that can be represented exactly as a float value. See
[Representation of Floating Point Numbers](`e:system:data_types.md#float_representation_problem`)
for additional information.

# `self`
*auto-imported* *allowed in guard tests* 

```erlang
-spec self() -> pid().
```

Returns the process identifier of the calling process.

For example:

```erlang
> self().
<0.26.0>
```

# `send`

```erlang
-spec send(Dest, Msg) -> Msg when Dest :: send_destination(), Msg :: term().
```

Sends a message and returns `Msg`. This is the same as using the
[send operator](`e:system:expressions.md#send`): `Dest ! Msg`.

`Dest` can be a remote or local process identifier, an alias, a (local) port, a
locally registered name, or a tuple `{RegName, Node}` for a registered name at
another node.

The function fails with a `badarg` run-time error if `Dest` is an atom name, but
this name is not registered. This is the only case when `send` fails for an
unreachable destination `Dest` (of correct type).

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

# `send`

```erlang
-spec send(Dest, Msg, Options) -> Res
              when
                  Dest :: send_destination(),
                  Msg :: term(),
                  Options :: [nosuspend | noconnect | priority],
                  Res :: ok | nosuspend | noconnect.
```

Either sends a message and returns `ok`, or does not send the message but
returns something else (see below). Otherwise the same as
[`erlang:send/2`](`send/2`).

For more detailed explanation and warnings, see [`erlang:send_nosuspend/2,3`](`send_nosuspend/2`).

Options:

- **`nosuspend`** - If the sender would have to be suspended to do the send,
  `nosuspend` is returned instead.

- **`noconnect`** - If the destination node would have to be auto-connected to
  do the send, `noconnect` is returned instead.

- **`priority`** - Since OTP 28.0

  Send this message as a priority message. In order for the message to be
  handled as a
  [priority message](`e:system:ref_man_processes.md#priority-messages`) by the
  receiver, this option *must* be passed, and `Dest` *must* be an active
  [*priority alias*](#priority_alias).

  If `Dest` is an active priority alias, but this option is not passed, the
  message will be handled as on ordinary message. The same is true, if this
  option is passed, but `Dest` is not an active priority alias.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  and the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  sections of the _Erlang Reference Manual_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

> #### Warning {: .warning }
>
> As with `erlang:send_nosuspend/2,3`: use with extreme care.

# `send_after`

```erlang
-spec send_after(Time, Dest, Msg) -> TimerRef
                    when
                        Time :: non_neg_integer(),
                        Dest :: pid() | atom(),
                        Msg :: term(),
                        TimerRef :: reference().
```

# `send_after`
*since OTP 18.0* 

```erlang
-spec send_after(Time, Dest, Msg, Options) -> TimerRef
                    when
                        Time :: integer(),
                        Dest :: pid() | atom(),
                        Msg :: term(),
                        Options :: [Option],
                        Abs :: boolean(),
                        Option :: {abs, Abs},
                        TimerRef :: reference().
```

Starts a timer. When the timer expires, the message `Msg` is sent to the process
identified by `Dest`. Apart from the format of the time-out message, this
function works exactly as [`erlang:start_timer/4`](`start_timer/4`).

# `send_nosuspend`

```erlang
-spec send_nosuspend(Dest, Msg) -> boolean() when Dest :: send_destination(), Msg :: term().
```

Send a message without suspending the caller.

Equivalent to [`erlang:send(Dest, Msg, [nosuspend])`](`send/3`), but returns
`true` if the message was sent and `false` if the message was not sent because
the sender would have had to be suspended.

This function is intended for send operations to an unreliable remote node
without ever blocking the sending (Erlang) process. If the connection to the
remote node (usually not a real Erlang node, but a node written in C or Java) is
overloaded, this function _does not send the message_ and returns `false`.

The same occurs if `Dest` refers to a local port that is busy. For all other
destinations (allowed for the ordinary send operator `'!'`), this function sends
the message and returns `true`.

This function is only to be used in rare circumstances where a process
communicates with Erlang nodes that can disappear without any trace, causing the
TCP buffers and the drivers queue to be over-full before the node is shut down
(because of tick time-outs) by `net_kernel`. The normal reaction to take when
this occurs is some kind of premature shutdown of the other node.

Notice that ignoring the return value from this function would result in an
_unreliable_ message passing, which is contradictory to the Erlang programming
model. The message is _not_ sent if this function returns `false`.

In many systems, transient states of overloaded queues are normal. Although this
function returns `false` does not mean that the other node is guaranteed to be
non-responsive, it could be a temporary overload. Also, a return value of `true`
does only mean that the message can be sent on the (TCP) channel without
blocking; the message is not guaranteed to arrive at the remote node. For a
disconnected non-responsive node, the return value is `true` (mimics the
behavior of operator `!`). The expected behavior and the actions to take when
the function returns `false` are application- and hardware-specific.

> #### Warning {: .warning }
>
> Use with extreme care.

# `send_nosuspend`

```erlang
-spec send_nosuspend(Dest, Msg, Options) -> boolean()
                        when Dest :: send_destination(), Msg :: term(), Options :: [noconnect].
```

Equivalent to [`erlang:send(Dest, Msg, [nosuspend | Options])`](`send/3`), but
with a Boolean return value.

This function behaves like [`erlang:send_nosuspend/2`](`send_nosuspend/2`), but
takes a third parameter, a list of options. The only option is `noconnect`,
which makes the function return `false` if the remote node is not currently
reachable by the local node. The normal behavior is to try to connect to the
node, which can stall the process during a short period. The use of option
`noconnect` makes it possible to be sure not to get the slightest delay when
sending to a remote process. This is especially useful when communicating with
nodes that expect to always be the connecting part (that is, nodes written in C
or Java).

Whenever the function returns `false` (either when a suspend would occur or when
`noconnect` was specified and the node was not already connected), the message
is guaranteed _not_ to have been sent.

> #### Warning {: .warning }
>
> Use with extreme care.

# `set_cookie`
*since OTP 24.1* 

```erlang
-spec set_cookie(Cookie) -> true when Cookie :: atom().
```

Sets the magic cookie of the local node to the atom `Cookie`, which is also the
cookie for all nodes that have no explicit cookie set with `set_cookie/2`
`Cookie`.

See section [Distributed Erlang](`e:system:distributed.md`) in the
Erlang Reference Manual in System Documentation for more information.

You can get this value using `get_cookie/0`.

Failure: `function_clause` if the local node is not alive.

# `set_cookie`

```erlang
-spec set_cookie(Node, Cookie) -> true when Node :: node(), Cookie :: atom().
```

Sets the magic cookie for `Node` to the atom `Cookie`. If `Node` is the local
node, the function sets the cookie of all other nodes (that have no explicit
cookie set with this function) to `Cookie`.

See section [Distributed Erlang](`e:system:distributed.md`) in the
Erlang Reference Manual in System Documentation for more information.

You can get this value using `get_cookie/1`.

Failure: `function_clause` if the local node is not alive.

# `setelement`
*auto-imported* 

```erlang
-spec setelement(Index, Tuple1, Value) -> Tuple2
                    when Index :: pos_integer(), Tuple1 :: tuple(), Tuple2 :: tuple(), Value :: term().
```

Returns a tuple that is a copy of argument `Tuple1` with the element specified
by integer argument `Index` (the first element is the element with index 1)
replaced by argument `Value`.

## Examples

```erlang
1> setelement(2, {10, green, bottles}, red).
{10,red,bottles}
```

# `size`
*auto-imported* *allowed in guard tests* 

```erlang
-spec size(Item) -> non_neg_integer() when Item :: tuple() | binary().
```

Returns the number of elements in a tuple or the number of bytes in a binary or
bitstring.

For bitstrings, the number of whole bytes is returned. That is, if the number of
bits in the bitstring is not divisible by 8, the resulting number of bytes is
rounded _down_.

See also `tuple_size/1`, `byte_size/1`, and `bit_size/1`.

> #### Note {: .info }
>
> It is recommended to avoid `size/1` in new code.

## Examples

```erlang
1> size({a, b, c}).
3
2> tuple_size({a, b, c}).
3
3> size(<<11, 22, 33>>).
3
4> byte_size(<<11, 22, 33>>).
3
5> size(<<11, 7:4>>).
1
6> byte_size(<<11, 7:4>>).
2
7> bit_size(<<11, 7:4>>).
12
```

# `spawn`
*auto-imported* 

```erlang
-spec spawn(Fun) -> pid() when Fun :: function().
```

Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]`. Otherwise works like `spawn/3`.

# `spawn`
*auto-imported* 

```erlang
-spec spawn(Node, Fun) -> pid() when Node :: node(), Fun :: function().
```

Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]` on `Node`. If `Node` does not exist, a useless pid
is returned. Otherwise works like `spawn/3`.

# `spawn`
*auto-imported* 

```erlang
-spec spawn(Module, Function, Args) -> pid()
               when Module :: module(), Function :: atom(), Args :: [term()].
```

Returns the process identifier of a new process started by the application of
`Module:Function` to `Args`.

[`error_handler:undefined_function(Module, Function, Args)`](`error_handler`) is
 evaluated by the new process if `Module:Function/Arity` does not exist
(where `Arity` is the length of `Args`). The error handler can be redefined
(see `process_flag/2`). If
`error_handler` is undefined, or the user has redefined the default
`error_handler` and its replacement is undefined, a failure with reason `undef`
occurs.

Example:

```erlang
> spawn(speed, regulator, [high_speed, thin_cut]).
<0.13.1>
```

# `spawn`
*auto-imported* 

```erlang
-spec spawn(Node, Module, Function, Args) -> pid()
               when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn/3`.

# `spawn_link`
*auto-imported* 

```erlang
-spec spawn_link(Fun) -> pid() when Fun :: fun(() -> term()).
```

Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]`. A link is created between the calling process and
the new process, atomically. Otherwise works like `spawn/3`.

# `spawn_link`
*auto-imported* 

```erlang
-spec spawn_link(Node, Fun) -> pid() when Node :: node(), Fun :: fun(() -> term()).
```

Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]` on `Node`. A link is created between the calling
process and the new process, atomically. If `Node` does not exist, a useless pid
is returned and an exit signal with reason `noconnection` is sent to the calling
process. Otherwise works like `spawn/3`.

# `spawn_link`
*auto-imported* 

```erlang
-spec spawn_link(Module, Function, Args) -> pid()
                    when Module :: module(), Function :: atom(), Args :: [term()].
```

Returns the process identifier of a new process started by the application of
`Module:Function` to `Args`. A link is created between the calling process and
the new process, atomically. Otherwise works like `spawn/3`.

# `spawn_link`
*auto-imported* 

```erlang
-spec spawn_link(Node, Module, Function, Args) -> pid()
                    when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. A link is created between the calling
process and the new process, atomically. If `Node` does not exist, a useless pid
is returned and an exit signal with reason `noconnection` is sent to the calling
process. Otherwise works like `spawn/3`.

# `spawn_monitor`
*auto-imported* 

```erlang
-spec spawn_monitor(Fun) -> {pid(), reference()} when Fun :: function().
```

Returns the process identifier of a new process, started by the application of
`Fun` to the empty list `[]`, and a reference for a monitor created to the new
process. Otherwise works like `spawn/3`.

# `spawn_monitor`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_monitor(Node, Fun) -> {pid(), reference()} when Node :: node(), Fun :: function().
```

Returns the process identifier of a new process, started by the application of
`Fun` to the empty list `[]` on the node `Node`, and a reference for a monitor
created to the new process. Otherwise works like `spawn/3`.

If the node identified by `Node` does not support distributed `spawn_monitor()`,
the call will fail with a `notsup` exception.

# `spawn_monitor`
*auto-imported* 

```erlang
-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()}
                       when Module :: module(), Function :: atom(), Args :: [term()].
```

A new process is started by the application of `Module:Function` to `Args`. The
process is monitored at the same time. Returns the process identifier and a
reference for the monitor. Otherwise works like `spawn/3`.

# `spawn_monitor`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_monitor(Node, Module, Function, Args) -> {pid(), reference()}
                       when Node :: node(), Module :: module(), Function :: atom(), Args :: [term()].
```

A new process is started by the application of `Module:Function` to `Args` on
the node `Node`. The process is monitored at the same time. Returns the process
identifier and a reference for the monitor. Otherwise works like `spawn/3`.

If the node identified by `Node` does not support distributed `spawn_monitor()`,
the call will fail with a `notsup` exception.

# `spawn_opt`
*auto-imported* 

```erlang
-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()}
                   when Fun :: function(), Options :: [spawn_opt_option()].
```

Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]`. Otherwise works like `spawn_opt/4`.

If option `monitor` is specified, the newly created process is monitored, and
both the pid and reference for the monitor are returned.

# `spawn_opt`
*auto-imported* 

```erlang
-spec spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()}
                   when
                       Node :: node(),
                       Fun :: function(),
                       Options :: [monitor | {monitor, [monitor_option()]} | link | OtherOption],
                       OtherOption :: term().
```

Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn_opt/4`.

Valid options depends on what options are supported by the node identified by
`Node`. A description of valid `Option`s for the local node of current OTP
version can be found in the documentation of `spawn_opt/4`.

# `spawn_opt`
*auto-imported* 

```erlang
-spec spawn_opt(Module, Function, Args, Options) -> Pid | {Pid, MonitorRef}
                   when
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       Options :: [spawn_opt_option()],
                       Pid :: pid(),
                       MonitorRef :: reference().
```

Works as `spawn/3`, except that an extra option list is specified when creating
the process.

If option `monitor` is specified, the newly created process is monitored, and
both the pid and reference for the monitor are returned.

Options:

- **`link`** - Sets a link to the parent process (like `spawn_link/3` does).

- **`monitor`** - Monitors the new process (like
  [`monitor(process, Pid)`](`monitor/2`) does). A `{Pid, MonitorRef}` tuple will
  be returned instead of just a `Pid`.

- **`{monitor, MonitorOpts}`** - Monitors the new process with options (like
  [`monitor(process, Pid, MonitorOpts)`](`monitor/3`) does). A
  `{Pid, MonitorRef}` tuple will be returned instead of just a `Pid`.

- **`{priority, Level}`** - Sets the priority of the new process. Equivalent to
  executing [`process_flag(priority, Level)`](#process_flag_priority)
  in the start function of the new process, except that the priority is set
  before the process is selected for execution for the first time. For more
  information on priorities, see
  [`process_flag(priority, Level)`](#process_flag_priority).

- **`{fullsweep_after, Number}`** - Useful only for performance tuning. Do not
  use this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  The Erlang runtime system uses a generational garbage collection scheme, using
  an "old heap" for data that has survived at least one garbage collection. When
  there is no more room on the old heap, a fullsweep garbage collection is done.

  Option `fullsweep_after` makes it possible to specify the maximum number of
  generational collections before forcing a fullsweep, even if there is room on
  the old heap. Setting the number to zero disables the general collection
  algorithm, that is, all live data is copied at every garbage collection.

  A few cases when it can be useful to change `fullsweep_after`:

  - If binaries that are no longer used are to be thrown away as soon as
    possible. (Set `Number` to zero.)
  - A process that mostly have short-lived data is fullsweeped seldom or never,
    that is, the old heap contains mostly garbage. To ensure a fullsweep
    occasionally, set `Number` to a suitable value, such as 10 or 20.
  - In embedded systems with a limited amount of RAM and no virtual memory, you
    might want to preserve memory by setting `Number` to zero. (The value can be
    set globally, see [`erlang:system_flag/2`](`system_flag/2`).)

- **`{min_heap_size, Size}`** - Useful only for performance tuning. Do not use
  this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  Gives a minimum heap size, in words. Setting this value higher than the system
  default can speed up some processes because less garbage collection is done.
  However, setting a too high value can waste memory and slow down the system
  because of worse data locality. Therefore, use this option only for
  fine-tuning an application and to measure the execution time with various
  `Size` values.

- **`{min_bin_vheap_size, VSize}`** - Useful only for performance tuning. Do not
  use this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  Gives a minimum binary virtual heap size, in words. Setting this value higher
  than the system default can speed up some processes because less garbage
  collection is done. However, setting a too high value can waste memory.
  Therefore, use this option only for fine-tuning an application and to measure
  the execution time with various `VSize` values.

- **`{max_heap_size, Size}`** - Sets the `max_heap_size` process flag. The
  default `max_heap_size` is determined by command-line argument
  [`+hmax`](erl_cmd.md#%2Bhmax) in [erl](erl_cmd.md). For more information, see
  the documentation of
  [`process_flag(max_heap_size, Size)`](#process_flag_max_heap_size).

- **`{message_queue_data, MQD}`** - Sets the value of the `message_queue_data`
  process flag. `MQD` can be either `off_heap` or `on_heap`. The default value
  of the `message_queue_data` process flag is determined by the command-line
  argument [`+hmqd`](erl_cmd.md#%2Bhmqd) in [erl](erl_cmd.md). For more
  information, see the documentation of
  [`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

- **`{async_dist, Enabled}`{: #spawn_opt_async_dist }** - Sets the
  [`async_dist`](#process_flag_async_dist) process flag of the spawned process.
  This option will override the default value set by the command line argument
  [`+pad <boolean>`](erl_cmd.md#%2Bpad).

  Since: OTP 25.3

# `spawn_opt`
*auto-imported* 

```erlang
-spec spawn_opt(Node, Module, Function, Args, Options) -> pid() | {pid(), reference()}
                   when
                       Node :: node(),
                       Module :: module(),
                       Function :: atom(),
                       Args :: [term()],
                       Options :: [monitor | {monitor, [monitor_option()]} | link | OtherOption],
                       OtherOption :: term().
```

Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn_opt/4`.

Valid options depends on what options are supported by the node identified by
`Node`. A description of valid `Option`s for the local node of current OTP
version can be found in the documentation of `spawn_opt/4`.

# `spawn_request`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request(Fun) -> ReqId when Fun :: function(), ReqId :: reference().
```

Equivalent to the call [`spawn_request(node(),Fun,[])`](`spawn_request/3`). That
is, a spawn request on the local node with no options.

# `spawn_request`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request(Fun, Options) -> ReqId
                       when
                           Fun :: function(),
                           Option :: {reply_tag, ReplyTag} | {reply, Reply} | spawn_opt_option(),
                           ReplyTag :: term(),
                           Reply :: yes | no | error_only | success_only,
                           Options :: [Option],
                           ReqId :: reference();
                   (Node, Fun) -> ReqId when Node :: node(), Fun :: function(), ReqId :: reference().
```

Equivalent to [`spawn_request(node(),Fun,Options)`](`spawn_request/3`) or
[`spawn_request(Node,Fun,[])`](`spawn_request/3`) depending on the arguments.

That is either:
- a spawn request on the local node.
- a spawn request with no options.

# `spawn_request`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request(Node, Fun, Options) -> ReqId
                       when
                           Node :: node(),
                           Fun :: function(),
                           Options :: [Option],
                           Option ::
                               monitor |
                               {monitor, [monitor_option()]} |
                               link |
                               {reply_tag, ReplyTag} |
                               {reply, Reply} |
                               OtherOption,
                           ReplyTag :: term(),
                           Reply :: yes | no | error_only | success_only,
                           OtherOption :: term(),
                           ReqId :: reference();
                   (Module, Function, Args) -> ReqId
                       when
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           ReqId :: reference().
```

Equivalent to
[`spawn_request(Node,erlang,apply,[Fun,[]],Options)`](`spawn_request/5`) or
[`spawn_request(node(),Module,Function,Args,[])`](`spawn_request/5`) depending
on the arguments.

That is either:

- a spawn request using the fun `Fun` of arity zero as entry point
- a spawn request on the local node with no options.

This function will fail with a `badarg` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of arity zero.
- `Options` is not a proper list of terms.

# `spawn_request`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request(Node, Module, Function, Args) -> ReqId
                       when
                           Node :: node(),
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           ReqId :: reference();
                   (Module, Function, Args, Options) -> ReqId
                       when
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           Option :: {reply_tag, ReplyTag} | {reply, Reply} | spawn_opt_option(),
                           ReplyTag :: term(),
                           Reply :: yes | no | error_only | success_only,
                           Options :: [Option],
                           ReqId :: reference().
```

Equivalent to
[`spawn_request(Node,Module,Function,Args,[])`](`spawn_request/5`) or
[`spawn_request(node(),Module,Function,Args,Options)`](`spawn_request/5`)
depending on the arguments.

That is either:
- a spawn request with no options.
- a spawn request on the local node.

# `spawn_request`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request(Node, Module, Function, Args, Options) -> ReqId
                       when
                           Node :: node(),
                           Module :: module(),
                           Function :: atom(),
                           Args :: [term()],
                           Options :: [Option],
                           Option ::
                               monitor |
                               {monitor, [monitor_option()]} |
                               link |
                               {reply_tag, ReplyTag} |
                               {reply, Reply} |
                               OtherOption,
                           ReplyTag :: term(),
                           Reply :: yes | no | error_only | success_only,
                           OtherOption :: term(),
                           ReqId :: reference().
```

Asynchronously send a spawn request. Returns a request identifier `ReqId`.

[](){: #spawn_request_success_message }

If the spawn operation succeeds, a new process is created on the node identified
by `Node`. When a spawn operation succeeds, the caller will by default be sent a
message of the form `{ReplyTag, ReqId, ok, Pid}` where `Pid` is the process
identifier of the newly created process. Such a message is referred to as a
_success message_ below in the text. `ReplyTag` is by default the atom
`spawn_reply` unless modified by the `{reply_tag, ReplyTag}` option. The new
process is started by the application of `Module:Function` to `Args`.

[](){: #spawn_request_error_message }

The spawn operation fails either if creation of a new process failed or if the
spawn operation was interrupted by a connection failure. When a spawn operation
fails, the caller will by default be sent a message on the form
`{ReplyTag, ReqId, error, Reason}` where `Reason` is the error reason. Such a
message is referred to as an _error message_ below in the text. Currently the
following spawn error `Reason`s are defined, but other reasons can appear at any
time without prior notice:

- **`badopt`** - An invalid `Option` was passed as argument. Note that different
  runtime systems may support different options.

- **`notsup`** - The node identified by `Node` does not support spawn operations
  issued by `spawn_request()`.

- **`noconnection`** - Failure to set up a connection to the node identified by
  `Node` or the connection to that node was lost during the spawn operation. In
  the case the connection was lost, a process may or may not have been created.

- **`system_limit`** - Could not create a new process due to that some system
  limit was reached. Typically the process table was full.

Valid `Option`s:

- **`monitor`** - In the absence of spawn operation failures, atomically sets up
  a monitor to the newly created process. That is, as if the calling process had
  called [`monitor(process, Pid)`](`monitor/2`) where `Pid` is the process
  identifier of the newly created process. The `ReqId` returned by
  `spawn_request()` is also used as monitor reference as if it was returned from
  [`monitor(process, Pid)`](`monitor/2`).

  The monitor will not be activated for the calling process until the spawn
  operation has succeeded. The monitor can not be [demonitored](`demonitor/1`)
  before the operation has succeeded. A `'DOWN'` message for the corresponding
  monitor is guaranteed not to be delivered before a
  [_success message_](#spawn_request_success_message) that corresponds
  to the spawn operation. If the spawn operation fails, no `'DOWN'` message will
  be delivered.

  If the connection between the nodes involved in the spawn operation is lost
  during the spawn operation, the spawn operation will fail with an error reason
  of `noconnection`. A new process may or may not have been created.

- **`{monitor, MonitorOpts}`** - In the absence of spawn operation failures,
  atomically sets up a monitor to the newly created process. That is, as if the
  calling process had called [`monitor(process, Pid, MonitorOpts)`](`monitor/2`)
  where `Pid` is the process identifier of the newly created process. See the
  `monitor` option above for more information.

  Note that the monitor will not be activated for the calling process until the
  spawn operation has succeeded. For example, in the case that an alias is
  created using the monitor option, the alias will not be active until the
  monitor is activated.

- **`link`** - In absence of spawn operation failures, atomically sets up a link
  between the calling process and the newly created process. That is, as if the
  calling process had called [`link(Pid)`](`link/1`) where `Pid` is the process
  identifier of the newly created process.

  The link will not be activated for the calling process until the spawn
  operation has succeeded. The link can not be removed before the operation has
  succeeded. An exit signal due to the link is guaranteed not to be delivered
  before a [_success message_](#spawn_request_success_message) that
  corresponds to the spawn operation. If the spawn operation fails, no exit
  signal due to the link will be delivered to the caller of `spawn_request()`.

  If the connection between the nodes involved in the spawn operation is lost
  during the spawn operation, the spawn operation will fail with an error reason
  of `noconnection`. A new process may or may not have been created. If it has
  been created, it will be delivered an exit signal with an exit reason of
  `noconnection`.

- **`{reply, Reply}`** - Valid `Reply` values:

  - **`yes`** - A spawn reply message will be sent to the caller regardless of
    whether the operation succeeds or not. If the call to `spawn_request()`
    returns without raising an exception and the `reply` option is set to `yes`,
    the caller is guaranteed to be delivered either a
    [_success message_](#spawn_request_success_message) or an
    [_error message_](#spawn_request_error_message). The `reply`
    option is by default set to `yes`.

  - **`no`** - No spawn reply message will be sent to the caller when the spawn
    operation completes. This regardless of whether the operation succeeds or
    not.

  - **`error_only`** - No spawn reply message will be sent to the caller if the
    spawn operation succeeds, but an
    [_error message_](#spawn_request_error_message) will be sent to
    the caller if the operation fails.

  - **`success_only`** - No spawn reply message will be sent to the caller if
    the spawn operation fails, but a
    [_success message_](#spawn_request_success_message) will be sent
    to the caller if the operation succeeds.

- **`{reply_tag, ReplyTag}`** - Sets the reply tag to `ReplyTag` in the reply
  message. That is, in the [_success_](#spawn_request_success_message)
  or [_error_](#spawn_request_error_message) message that is sent to
  the caller due to the spawn operation. The default reply tag is the atom
  `spawn_reply`.

- **`OtherOption`** - Other valid options depends on what options are supported
  by the node identified by `Node`. A description of other valid `Option`s for
  the local node of current OTP version can be found in the documentation of
  `spawn_opt/4`.

If a spawn reply message is delivered, it is guaranteed to be delivered before
any other signals from the newly spawned process are delivered to the process
issuing the spawn request.

This function will fail with a `badarg` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a proper list of terms.
- `Options` is not a proper list of terms.

Note that not all individual `Option`s are checked when the spawn request is
sent. Some `Option`s can only be checked on reception of the request. Therefore
an invalid option does _not_ cause a `badarg` exception, but will cause the
spawn operation to fail with an error reason of `badopt`.

A spawn request can be abandoned by calling `spawn_request_abandon/1`.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

# `spawn_request_abandon`
*auto-imported* *since OTP 23.0* 

```erlang
-spec spawn_request_abandon(ReqId :: reference()) -> boolean().
```

Abandon a previously issued spawn request. `ReqId` corresponds to a request
identifier previously returned by [`spawn_request()`](`spawn_request/5`) in a
call from current process. That is, only the process that has made the request
can abandon the request.

A spawn request can only be successfully abandoned until the spawn request has
completed. When a spawn request has been successfully abandoned, the caller will
not be effected by future direct effects of the spawn request itself. For
example, it will not receive a spawn reply message. The request is however not
withdrawn, so a new process may or may not be created due to the request. If a
new process is created after the spawn request was abandoned, no monitors nor
links will be set up to the caller of
[`spawn_request_abandon/1`](`spawn_request_abandon/1`) due to the spawn request.
If the spawn request included the `link` option, the process created due to this
request will be sent an exit signal from its parent with the exit reason
`abandoned` when it is detected that the spawn operation has succeeded.

> #### Note {: .info }
>
> A process created due to a spawn request that has been abandoned may
> communicate with its parent as any other process. It is _only_ the direct
> effects on the parent of the actual spawn request, that will be canceled by
> abandoning a spawn request.

Return values:

- **`true`** - The spawn request was successfully abandoned.

- **`false`** - No spawn request was abandoned. The `ReqId` request identifier
  did not correspond to an outstanding spawn request issued by the calling
  process. The reason for this is either:

  - `ReqId` corresponds to a spawn request previoulsy made by the calling
    process. The spawn operation has completed and a spawn reply has already
    been delivered to the calling process unless the spawn reply was disabled in
    the request.
  - `ReqId` does not correspond to a spawn request that has been made by the
    calling process.

This function fail with a `badarg` exception if `ReqId` is not a reference.

# `split_binary`
*auto-imported* 

```erlang
-spec split_binary(Bin, Pos) -> {binary(), binary()} when Bin :: binary(), Pos :: non_neg_integer().
```

Returns a tuple containing the binaries that are the result of splitting `Bin`
into two parts at position `Pos`.

This operation is non-destructive. After the operation, there are
three binaries altogether.

## Examples

```erlang
1> B = list_to_binary("0123456789").
<<"0123456789">>
2> byte_size(B).
10
3> {B1, B2} = split_binary(B, 3).
{<<"012">>,<<"3456789">>}
4> byte_size(B1).
3
5> byte_size(B2).
7
```

# `start_timer`

```erlang
-spec start_timer(Time, Dest, Msg) -> TimerRef
                     when
                         Time :: non_neg_integer(),
                         Dest :: pid() | atom(),
                         Msg :: term(),
                         TimerRef :: reference().
```

# `start_timer`
*since OTP 18.0* 

```erlang
-spec start_timer(Time, Dest, Msg, Options) -> TimerRef
                     when
                         Time :: integer(),
                         Dest :: pid() | atom(),
                         Msg :: term(),
                         Options :: [Option],
                         Abs :: boolean(),
                         Option :: {abs, Abs},
                         TimerRef :: reference().
```

Starts a timer. When the timer expires, the message `{timeout, TimerRef, Msg}`
is sent to the process identified by `Dest`.

`Option`s:

- **`{abs, false}`** - This is the default. It means the `Time` value is
  interpreted as a time in milliseconds _relative_ current
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time).

- **`{abs, true}`** - Absolute `Time` value. The `Time` value is interpreted as
  an absolute Erlang monotonic time in milliseconds.

More `Option`s can be added in the future.

The absolute point in time, the timer is set to expire on, must be in the
interval
`[ `[erlang:convert_time_unit](`convert_time_unit/3`)`(`[erlang:system_info](#system_info_start_time)`(start_time), native, millisecond), `[erlang:convert_time_unit](`convert_time_unit/3`)`(`[erlang:system_info](#system_info_end_time)`(end_time), native, millisecond) ]`.
If a relative time is specified, the `Time` value is not allowed to be negative.

If `Dest` is a `t:pid/0`, it must be a `t:pid/0` of a process created on the
current runtime system instance. This process has either terminated or not. If
`Dest` is an `t:atom/0`, it is interpreted as the name of a locally registered
process. The process referred to by the name is looked up at the time of timer
expiration. No error is returned if the name does not refer to a process.

If `Dest` is a `t:pid/0`, the timer is automatically canceled if the process
referred to by the `t:pid/0` is not alive, or if the process exits. This feature
was introduced in ERTS 5.4.11. Notice that timers are not automatically canceled
when `Dest` is an `t:atom/0`.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:cancel_timer/2`](`cancel_timer/2`), and
[`erlang:read_timer/2`](`read_timer/2`).

For more information on timers in Erlang in general, see the
[*Timers*](`e:erts:time_correction.md#timers`) section of the
[*Time and Time Correction in Erlang*](`e:erts:time_correction.md`)
ERTS User's guide.

Failure: `badarg` if the arguments do not satisfy the requirements specified
here.

# `statistics`
*auto-imported* 

```erlang
-spec statistics(active_tasks) -> [ActiveTasks] when ActiveTasks :: non_neg_integer();
                (active_tasks_all) -> [ActiveTasks] when ActiveTasks :: non_neg_integer();
                (context_switches) -> {ContextSwitches, 0} when ContextSwitches :: non_neg_integer();
                (exact_reductions) -> {Total_Exact_Reductions, Exact_Reductions_Since_Last_Call}
                    when
                        Total_Exact_Reductions :: non_neg_integer(),
                        Exact_Reductions_Since_Last_Call :: non_neg_integer();
                (garbage_collection) -> {Number_of_GCs, Words_Reclaimed, 0}
                    when Number_of_GCs :: non_neg_integer(), Words_Reclaimed :: non_neg_integer();
                (io) -> {{input, Input}, {output, Output}}
                    when Input :: non_neg_integer(), Output :: non_neg_integer();
                (microstate_accounting) -> [MSAcc_Thread] | undefined
                    when
                        MSAcc_Thread ::
                            #{type := MSAcc_Thread_Type,
                              id := MSAcc_Thread_Id,
                              counters := MSAcc_Counters},
                        MSAcc_Thread_Type ::
                            async | aux | dirty_io_scheduler | dirty_cpu_scheduler | poll | scheduler,
                        MSAcc_Thread_Id :: non_neg_integer(),
                        MSAcc_Counters :: #{MSAcc_Thread_State => non_neg_integer()},
                        MSAcc_Thread_State ::
                            alloc | aux | bif | busy_wait | check_io | emulator | ets | gc |
                            gc_fullsweep | nif | other | port | send | sleep | timers;
                (reductions) -> {Total_Reductions, Reductions_Since_Last_Call}
                    when
                        Total_Reductions :: non_neg_integer(),
                        Reductions_Since_Last_Call :: non_neg_integer();
                (run_queue) -> non_neg_integer();
                (run_queue_lengths) -> [RunQueueLength] when RunQueueLength :: non_neg_integer();
                (run_queue_lengths_all) -> [RunQueueLength] when RunQueueLength :: non_neg_integer();
                (runtime) -> {Total_Run_Time, Time_Since_Last_Call}
                    when Total_Run_Time :: non_neg_integer(), Time_Since_Last_Call :: non_neg_integer();
                (scheduler_wall_time) -> [{SchedulerId, ActiveTime, TotalTime}] | undefined
                    when
                        SchedulerId :: pos_integer(),
                        ActiveTime :: non_neg_integer(),
                        TotalTime :: non_neg_integer();
                (scheduler_wall_time_all) -> [{SchedulerId, ActiveTime, TotalTime}] | undefined
                    when
                        SchedulerId :: pos_integer(),
                        ActiveTime :: non_neg_integer(),
                        TotalTime :: non_neg_integer();
                (total_active_tasks) -> ActiveTasks when ActiveTasks :: non_neg_integer();
                (total_active_tasks_all) -> ActiveTasks when ActiveTasks :: non_neg_integer();
                (total_run_queue_lengths) -> TotalRunQueueLengths
                    when TotalRunQueueLengths :: non_neg_integer();
                (total_run_queue_lengths_all) -> TotalRunQueueLengths
                    when TotalRunQueueLengths :: non_neg_integer();
                (wall_clock) -> {Total_Wallclock_Time, Wallclock_Time_Since_Last_Call}
                    when
                        Total_Wallclock_Time :: non_neg_integer(),
                        Wallclock_Time_Since_Last_Call :: non_neg_integer().
```

Returns statistics about the current system.

The possible flags are:

- ```erlang
  statistics(active_tasks) -> [non_neg_integer()]
  ```
  {: #statistics_active_tasks }

  Returns the same as
  [`statistics(active_tasks_all)`](#statistics_active_tasks_all) with
  the exception that no information about the dirty IO run queue and its
  associated schedulers is part of the result. That is, only tasks that are
  expected to be CPU bound are part of the result.

  Available since OTP 18.3

- ```erlang
  statistics(active_tasks_all) -> [non_neg_integer()]
  ```
  {: #statistics_active_tasks_all }

  Returns a list where each element represents the amount of active processes and
  ports on each run queue and its associated schedulers. That is, the number of
  processes and ports that are ready to run, or are currently running. Values for
  normal run queues and their associated schedulers are located first in the
  resulting list. The first element corresponds to scheduler number 1 and so on.
  If support for dirty schedulers exist, an element with the value for the dirty
  CPU run queue and its associated dirty CPU schedulers follow and then as last
  element the value for the dirty IO run queue and its associated dirty IO
  schedulers follow. The information is _not_ gathered atomically. That is, the
  result is not necessarily a consistent snapshot of the state, but instead quite
  efficiently gathered.

  > #### Note {: .info }
  >
  > Each normal scheduler has one run queue that it manages. If dirty schedulers
  > are supported, all dirty CPU schedulers share one run queue, and all dirty IO
  > schedulers share one run queue. That is, we have multiple normal run queues,
  > one dirty CPU run queue and one dirty IO run queue. Work can _not_ migrate
  > between the different types of run queues. Only work in normal run queues can
  > migrate to other normal run queues. This has to be taken into account when
  > evaluating the result.

  See also
  [`statistics(total_active_tasks)`](#statistics_total_active_tasks),
  [`statistics(run_queue_lengths)`](#statistics_run_queue_lengths),
  [`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all),
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  and
  [`statistics(total_run_queue_lengths_all)`](#statistics_total_run_queue_lengths_all).

  Available since OTP 20.0

- ```erlang
  statistics(context_switches) -> {non_neg_integer(), 0}
  ```
  {: #statistics_context_switches }

  Returns the total number of context switches since the system started.

- ```erlang
  statistics(exact_reductions) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_exact_reductions }

  Returns the number of exact reductions.

  > #### Note {: .info }
  >
  > [`statistics(exact_reductions)`](`statistics/1`) is a more expensive operation
  > than [statistics(reductions)](#statistics_reductions).

- ```erlang
  statistics(garbage_collection) ->
    { NumerOfGCs :: non_neg_integer(), WordsReclaimed :: non_neg_integer(), 0}
  ```

  Returns information about garbage collection, for example:

  ```erlang
  > statistics(garbage_collection).
  {85,23961,0}
  ```

  This information can be invalid for some implementations.

- ```erlang
  statistics(io) -> {{input, non_neg_integer()}, {output, non_neg_integer()}}
  ```

  Returns `Input`, which is the total number of bytes received through ports, and
  `Output`, which is the total number of bytes output to ports.

- ```erlang
  statistics(microstate_accounting) -> [MSAcc_Thread]
  ```
  {: #statistics_microstate_accounting }

  Microstate accounting can be used to measure how much time the Erlang runtime
  system spends doing various tasks. It is designed to be as lightweight as
  possible, but some overhead exists when this is enabled. Microstate accounting
  is meant to be a profiling tool to help finding performance bottlenecks. To
  `start`/`stop`/`reset` microstate accounting, use system flag
  [`microstate_accounting`](#system_flag_microstate_accounting).

  [`statistics(microstate_accounting)`](`statistics/1`) returns a list of maps
  representing some of the OS threads within ERTS. Each map contains `type` and
  `id` fields that can be used to identify what thread it is, and also a counters
  field that contains data about how much time has been spent in the various
  states.

  Example:

  ```erlang
  > erlang:statistics(microstate_accounting).
  [#{counters => #{aux => 1899182914,
                   check_io => 2605863602,
                   emulator => 45731880463,
                   gc => 1512206910,
                   other => 5421338456,
                   port => 221631,
                   sleep => 5150294100},
     id => 1,
     type => scheduler}, ...]
  ```

  The time unit is the same as returned by `os:perf_counter/0`. So, to convert it
  to milliseconds, you can do something like this:

  ```erlang
  lists:map(
    fun(#{ counters := Cnt } = M) ->
           MsCnt = maps:map(fun(_K, PerfCount) ->
                                      erlang:convert_time_unit(PerfCount, perf_counter, 1000)
                             end, Cnt),
           M#{ counters := MsCnt }
    end, erlang:statistics(microstate_accounting)).
  ```

  Notice that these values are not guaranteed to be the exact time spent in each
  state. This is because of various optimisation done to keep the overhead as
  small as possible.

  `MSAcc_Thread_Type`s:

  - **`scheduler`** - The main execution threads that do most of the work. See
    [erl +S](erl_cmd.md#%2BS) for more details.

  - **`dirty_cpu_scheduler`** - The threads for long running cpu intensive work.
    See [erl +SDcpu](erl_cmd.md#%2BSDcpu) for more details.

  - **`dirty_io_scheduler`** - The threads for long running I/O work. See
    [erl +SDio](erl_cmd.md#%2BSDio) for more details.

  - **`async`** - Async threads are used by various linked-in drivers (mainly the
    file drivers) do offload non-CPU intensive work. See
    [erl +A](erl_cmd.md#async_thread_pool_size) for more details.

  - **`aux`** - Takes care of any work that is not specifically assigned to a
    scheduler.

  - **`poll`** - Does the IO polling for the emulator. See
    [erl +IOt](erl_cmd.md#%2BIOt) for more details.

  The following `MSAcc_Thread_State`s are available. All states are exclusive,
  meaning that a thread cannot be in two states at once. So, if you add the
  numbers of all counters in a thread, you get the total runtime for that thread.

  - **`aux`** - Time spent handling auxiliary jobs.

  - **`check_io`** - Time spent checking for new I/O events.

  - **`emulator`** - Time spent executing Erlang processes.

  - **`gc`** - Time spent doing garbage collection. When extra states are enabled
    this is the time spent doing non-fullsweep garbage collections.

  - **`other`** - Time spent doing unaccounted things.

  - **`port`** - Time spent executing ports.

  - **`sleep`** - Time spent sleeping.

  More fine-grained `MSAcc_Thread_State`s can be added through configure (such as
  `./configure --with-microstate-accounting=extra`). Enabling these states causes
  performance degradation when microstate accounting is turned off and increases
  the overhead when it is turned on.

  - **`alloc`** - Time spent managing memory. Without extra states this time is
    spread out over all other states.

  - **`bif`** - Time spent in BIFs. Without extra states this time is part of the
    `emulator` state.

  - **`busy_wait`** - Time spent busy waiting. This is also the state where a
    scheduler no longer reports that it is active when using
    [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time).
    So, if you add all other states but this and sleep, and then divide that by
    all time in the thread, you should get something very similar to the
    `scheduler_wall_time` fraction. Without extra states this time is part of the
    `other` state.

  - **`ets`** - Time spent executing ETS BIFs. Without extra states this time is
    part of the `emulator` state.

  - **`gc_full`** - Time spent doing fullsweep garbage collection. Without extra
    states this time is part of the `gc` state.

  - **`nif`** - Time spent in NIFs. Without extra states this time is part of the
    `emulator` state.

  - **`send`** - Time spent sending messages (processes only). Without extra
    states this time is part of the `emulator` state.

  - **`timers`** - Time spent managing timers. Without extra states this time is
    part of the `other` state.

  The utility module `m:msacc` can be used to more easily analyse these
  statistics.

  Returns `undefined` if system flag
  [`microstate_accounting`](#system_flag_microstate_accounting) is
  turned off.

  The list of thread information is unsorted and can appear in different order
  between calls.

  > #### Note {: .info }
  >
  > The threads and states are subject to change without any prior notice.

  Available since OTP 19.0

- ```erlang
  statistics(reductions) -> {Reductions :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_reductions }

  Returns information about reductions, for example:

  ```erlang
  > statistics(reductions).
  {2046,11}
  ```

  > #### Change {: .info }
  >
  > As from ERTS 5.5 (Erlang/OTP R11B), this value does not include reductions
  > performed in current time slices of currently scheduled processes. If an exact
  > value is wanted, use
  > [`statistics(exact_reductions)`](#statistics_exact_reductions).

- ```erlang
  statistics(run_queue) -> non_neg_integer()
  ```
  {: #statistics_run_queue }

  Returns the total length of all normal and dirty CPU run queues. That is, queued
  work that is expected to be CPU bound. The information is gathered atomically.
  That is, the result is a consistent snapshot of the state, but this operation is
  much more expensive compared to
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  especially when a large amount of schedulers is used.

- ```erlang
  statistics(run_queue_lengths) -> [non_neg_integer()]
  ```
  {: #statistics_run_queue_lengths }

  Returns the same as
  [`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all)
  with the exception that no information about the dirty IO run queue is part of
  the result. That is, only run queues with work that is expected to be CPU bound
  is part of the result.

  Available since OTP 18.3

- ```erlang
  statistics(run_queue_lengths_all) -> [non_neg_integer()]
  ```
  {: #statistics_run_queue_lengths_all }

  Returns a list where each element represents the amount of processes and ports
  ready to run for each run queue. Values for normal run queues are located first
  in the resulting list. The first element corresponds to the normal run queue of
  scheduler number 1 and so on. If support for dirty schedulers exist, values for
  the dirty CPU run queue and the dirty IO run queue follow (in that order) at the
  end. The information is _not_ gathered atomically. That is, the result is not
  necessarily a consistent snapshot of the state, but instead quite efficiently
  gathered.

  > #### Note {: .info }
  >
  > Each normal scheduler has one run queue that it manages. If dirty schedulers
  > are supported, all dirty CPU schedulers share one run queue, and all dirty IO
  > schedulers share one run queue. That is, we have multiple normal run queues,
  > one dirty CPU run queue and one dirty IO run queue. Work can _not_ migrate
  > between the different types of run queues. Only work in normal run queues can
  > migrate to other normal run queues. This has to be taken into account when
  > evaluating the result.

  See also
  [`statistics(run_queue_lengths)`](#statistics_run_queue_lengths),
  [`statistics(total_run_queue_lengths_all)`](#statistics_total_run_queue_lengths_all),
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  [`statistics(active_tasks)`](#statistics_active_tasks),
  [`statistics(active_tasks_all)`](#statistics_active_tasks_all), and
  [`statistics(total_active_tasks)`](#statistics_total_active_tasks),
  [`statistics(total_active_tasks_all)`](#statistics_total_active_tasks_all).

  Available since OTP 20.0

- ```erlang
  statistics(runtime) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```

  Returns information about runtime, in milliseconds.

  This is the sum of the runtime for all threads in the Erlang runtime system and
  can therefore be greater than the wall clock time.

  > #### Warning {: .warning }
  >
  > This value might wrap due to limitations in the underlying functionality
  > provided by the operating system that is used.

  Example:

  ```erlang
  > statistics(runtime).
  {1690,1620}
  ```

- ```erlang
  statistics(scheduler_wall_time) ->
    [{Id :: pos_integer,
      ActiveTime :: non_neg_integer(),
      TotalTime :: non_neg_integer()}] |
    undefined
  ```
  {: #statistics_scheduler_wall_time }

  Returns information describing how much time
  [normal](#system_info_schedulers) and
  [dirty CPU](#system_info_dirty_cpu_schedulers) schedulers in the
  system have been busy. This value is normally a better indicator of how much
  load an Erlang node is under instead of looking at the CPU utilization provided
  by tools such as `top` or `sysstat`. This is because `scheduler_wall_time` also
  includes time where the scheduler is waiting for some other resource (such as
  an internal mutex) to be available but does not use the CPU. In order to better
  understand what a scheduler is busy doing you can use
  [microstate accounting](#statistics_microstate_accounting).

  The definition of a busy scheduler is when it is not idle and not
  [busy waiting](erl_cmd.md#%2Bsbwt) for new work, that is:

  - Executing process code
  - Executing linked-in driver or NIF code
  - Executing BIFs, or any other runtime handling
  - Garbage collecting
  - Handling any other memory management

  Notice that a scheduler can also be busy even if the OS has scheduled out the
  scheduler thread.

  > #### Note {: .info }
  >
  > It is recommended to use the module `m:scheduler` instead of this function
  > directly as it provides an easier way to get the information that you usually
  > want.

  If [enabled](#system_flag_scheduler_wall_time) this function returns a
  list of tuples with `{SchedulerId, ActiveTime, TotalTime}`, where `SchedulerId`
  is an integer ID of the scheduler, `ActiveTime` is the duration the scheduler
  has been busy, and `TotalTime` is the total time duration since
  [`scheduler_wall_time`](#system_flag_scheduler_wall_time) activation
  for the specific scheduler. The time unit returned is undefined and can be
  subject to change between releases, OSs, and system restarts.
  `scheduler_wall_time` is only to be used to calculate relative values for
  scheduler utilization. The `ActiveTime` can never exceed `TotalTime`. The list
  of scheduler information is unsorted and can appear in different order between
  calls.

  The [disabled](#system_flag_scheduler_wall_time) this function returns
  `undefined`.

  The activation time can differ significantly between schedulers. Currently dirty
  schedulers are activated at system start while normal schedulers are activated
  some time after the `scheduler_wall_time` functionality is enabled.

  Only information about schedulers that are expected to handle CPU bound work is
  included in the return values from this function. If you also want information
  about [dirty I/O schedulers](#system_info_dirty_io_schedulers), use
  [`statistics(scheduler_wall_time_all)`](#statistics_scheduler_wall_time_all)
  instead.

  Normal schedulers will have scheduler identifiers in the range
  `1 =< SchedulerId =< `[`erlang:system_info(schedulers)`](#system_info_schedulers).
  Dirty CPU schedulers will have scheduler identifiers in the range
  `erlang:system_info(schedulers) < SchedulerId =< erlang:system_info(schedulers) + `[`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers).

  > #### Note {: .info }
  >
  > The different types of schedulers handle specific types of jobs. Every job is
  > assigned to a specific scheduler type. Jobs can migrate between different
  > schedulers of the same type, but never between schedulers of different types.
  > This fact has to be taken under consideration when evaluating the result
  > returned.

  You can use `scheduler_wall_time` to calculate scheduler utilization. First you
  take a sample of the values returned by
  `erlang:statistics(scheduler_wall_time)`.

  ```erlang
  > erlang:system_flag(scheduler_wall_time, true).
  false
  > Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)), ok.
  ok
  ```

  Some time later the user takes another snapshot and calculates scheduler
  utilization per scheduler, for example:

  ```erlang
  > Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)), ok.
  ok
  > lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
          {I, (A1 - A0)/(T1 - T0)} end, lists:zip(Ts0,Ts1)).
  [{1,0.9743474730177548},
   {2,0.9744843782751444},
   {3,0.9995902361669045},
   {4,0.9738012596572161},
   {5,0.9717956667018103},
   {6,0.9739235846420741},
   {7,0.973237033077876},
   {8,0.9741297293248656}]
  ```

  Using the same snapshots to calculate a total scheduler utilization:

  ```erlang
  > {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
          {Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0,Ts1)),
    TotalSchedulerUtilization = A/T.
  0.9769136803764825
  ```

  Total scheduler utilization will equal `1.0` when all schedulers have been
  active all the time between the two measurements.

  Another (probably more) useful value is to calculate total scheduler utilization
  weighted against maximum amount of available CPU time:

  ```erlang
  > WeightedSchedulerUtilization = (TotalSchedulerUtilization
                                    * (erlang:system_info(schedulers)
                                       + erlang:system_info(dirty_cpu_schedulers)))
                                   / erlang:system_info(logical_processors_available).
  0.9769136803764825
  ```

  This weighted scheduler utilization will reach `1.0` when schedulers are active
  the same amount of time as maximum available CPU time. If more schedulers exist
  than available logical processors, this value may be greater than `1.0`.

  As of ERTS version 9.0, the Erlang runtime system will as default have more
  schedulers than logical processors. This due to the dirty schedulers.

  > #### Note {: .info }
  >
  > `scheduler_wall_time` is by default disabled. To enable it, use
  > [`erlang:system_flag(scheduler_wall_time, true)`](#system_flag_scheduler_wall_time).

  Available since OTP R15B01

- ```erlang
  statistics(scheduler_wall_time_all) ->
    [{Id :: pos_integer,
      ActiveTime :: non_neg_integer(),
      TotalTime :: non_neg_integer()}] |
    undefined
  ```
  {: #statistics_scheduler_wall_time_all }

  Equivalent to
  [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time),
  except that it also include information about all dirty I/O schedulers.

  Dirty IO schedulers will have scheduler identifiers in the range
  [`erlang:system_info(schedulers)`](#system_info_schedulers)`+`[`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers)`< SchedulerId =< erlang:system_info(schedulers) + erlang:system_info(dirty_cpu_schedulers) +`[`erlang:system_info(dirty_io_schedulers)`](#system_info_dirty_io_schedulers).

  > #### Note {: .info }
  >
  > Note that work executing on dirty I/O schedulers are expected to mainly wait
  > for I/O. That is, when you get high scheduler utilization on dirty I/O
  > schedulers, CPU utilization is _not_ expected to be high due to this work.

  Available since OTP 20.0

- ```erlang
  statistics(total_active_tasks) -> non_neg_integer()
  ```
  {: #statistics_total_active_tasks }

  Equivalent to calling
  `lists:sum(`[`statistics(active_tasks)`](#statistics_active_tasks)`)`,
  but more efficient.

  Available since OTP 18.3

- ```erlang
  statistics(total_active_tasks_all) -> non_neg_integer()
  ```
  {: #statistics_total_active_tasks_all }

  Equivalent to calling
  `lists:sum(`[`statistics(active_tasks_all)`](#statistics_active_tasks_all)`)`,
  but more efficient.

  Available since OTP 20.0

- ```erlang
  statistics(total_run_queue_lengths) -> non_neg_integer()
  ```
  {: #statistics_total_run_queue_lengths }

  Equivalent to calling
  `lists:sum(`[`statistics(run_queue_lengths)`](#statistics_run_queue_lengths)`)`,
  but more efficient.

  Available since OTP 18.3

- ```erlang
  statistics(total_run_queue_lengths_all) -> non_neg_integer()
  ```
  {: #statistics_total_run_queue_lengths_all }

  Equivalent to calling
  `lists:sum(`[`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all)`)`,
  but more efficient.

  Available since OTP 20.0

- ```erlang
  statistics(wall_clock) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_wall_clock }

  Returns information about wall clock. `wall_clock` can be used in the same
  manner as `runtime`, except that real time is measured as opposed to runtime or
  CPU time.

# `suspend_process`

```erlang
-spec suspend_process(Suspendee) -> true when Suspendee :: pid().
```

Suspends the process identified by `Suspendee`. Equivalent to calling
[`erlang:suspend_process(Suspendee, [])`](`suspend_process/2`).

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.

# `suspend_process`

```erlang
-spec suspend_process(Suspendee, OptList) -> boolean()
                         when
                             Suspendee :: pid(),
                             OptList :: [Opt],
                             Opt :: unless_suspending | asynchronous | {asynchronous, term()}.
```

Increases the suspend count on the process identified by `Suspendee` and puts it
in the suspended state if it is not already in that state. A suspended process
is not scheduled for execution until the process has been resumed. If the
suspended process currently is waiting in a `receive ... after` expression, the
timer for the timeout will, as of OTP 28.0, also be suspended until the process
is resumed. BIF timers (see [`erlang:send_after/3`](`send_after/3`) and
[`erlang:start_timer/3`](`start_timer/3`)) created using the PID of `Suspendee`
will, as of OTP 29.0, also be suspended until the process is resumed. BIF timers
created using a registered name are not affected.

A process can be suspended by multiple processes and can be suspended multiple
times by a single process. A suspended process does not leave the suspended
state until its suspend count reaches zero. The suspend count of `Suspendee` is
decreased when [`erlang:resume_process(Suspendee)`](`resume_process/1`) is
called by the same process that called `erlang:suspend_process(Suspendee)`. All
increased suspend counts on other processes acquired by a process are
automatically decreased when the process terminates.

Options (`Opt`s):

- **`asynchronous`** - A suspend request is sent to the process identified by
  `Suspendee`. `Suspendee` eventually suspends unless it is resumed before it
  could suspend. The caller of `erlang:suspend_process/2` returns immediately,
  regardless of whether `Suspendee` has suspended yet or not. The point in time
  when `Suspendee` suspends cannot be deduced from other events in the system.
  It is only guaranteed that `Suspendee` _eventually_ suspends (unless it is
  resumed). If no `asynchronous` options has been passed, the caller of
  `erlang:suspend_process/2` is blocked until `Suspendee` has suspended.

- **`{asynchronous, ReplyTag}`** - A suspend request is sent to the process
  identified by `Suspendee`. When the suspend request has been processed, a
  reply message is sent to the caller of this function. The reply is on the form
  `{ReplyTag, State}` where `State` is either:

  - **`exited`** - `Suspendee` has exited.

  - **`suspended`** - `Suspendee` is now suspended.

  - **`not_suspended`** - `Suspendee` is not suspended. This can only happen
    when the process that issued this request, have called
    [`resume_process(Suspendee)`](`resume_process/1`) before getting the reply.

  Apart from the reply message, the `{asynchronous, ReplyTag}` option behaves
  exactly the same as the `asynchronous` option without reply tag.

- **`unless_suspending`** - The process identified by `Suspendee` is suspended
  unless the calling process already is suspending `Suspendee`. If
  `unless_suspending` is combined with option `asynchronous`, a suspend request
  is sent unless the calling process already is suspending `Suspendee` or if a
  suspend request already has been sent and is in transit. If the calling
  process already is suspending `Suspendee`, or if combined with option
  `asynchronous` and a send request already is in transit, `false` is returned
  and the suspend count on `Suspendee` remains unchanged.

If the suspend count on the process identified by `Suspendee` is increased,
`true` is returned, otherwise `false`.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.

> #### Warning {: .warning }
>
> You can easily create deadlocks if processes suspends each other (directly or
> in circles). In ERTS versions prior to ERTS version 10.0, the runtime system
> prevented such deadlocks, but this prevention has now been removed due to
> performance reasons.

Failures:

- **`badarg`** - If `Suspendee` is not a process identifier.

- **`badarg`** - If the process identified by `Suspendee` is the same process as
  the process calling `erlang:suspend_process/2`.

- **`badarg`** - If the process identified by `Suspendee` is not alive.

- **`badarg`** - If the process identified by `Suspendee` resides on another
  node.

- **`badarg`** - If `OptList` is not a proper list of valid `Opt`s.

- **`system_limit`** - If the process identified by `Suspendee` has been
  suspended more times by the calling process than can be represented by the
  currently used internal data structures. The system limit is greater than
  2,000,000,000 suspends and will never be lower.

# `system_flag`

```erlang
-spec system_flag(backtrace_depth, Depth) -> OldDepth
                     when Depth :: non_neg_integer(), OldDepth :: non_neg_integer();
                 (cpu_topology, CpuTopology) -> OldCpuTopology
                     when CpuTopology :: cpu_topology(), OldCpuTopology :: cpu_topology();
                 (dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline) -> OldDirtyCPUSchedulersOnline
                     when
                         DirtyCPUSchedulersOnline :: pos_integer(),
                         OldDirtyCPUSchedulersOnline :: pos_integer();
                 (erts_alloc, {Alloc, F, V}) -> ok | notsup
                     when Alloc :: atom(), F :: atom(), V :: integer();
                 (fullsweep_after, Number) -> OldNumber
                     when Number :: non_neg_integer(), OldNumber :: non_neg_integer();
                 (microstate_accounting, Action) -> OldState
                     when Action :: true | false | reset, OldState :: true | false;
                 (min_heap_size, MinHeapSize) -> OldMinHeapSize
                     when MinHeapSize :: non_neg_integer(), OldMinHeapSize :: non_neg_integer();
                 (min_bin_vheap_size, MinBinVHeapSize) -> OldMinBinVHeapSize
                     when MinBinVHeapSize :: non_neg_integer(), OldMinBinVHeapSize :: non_neg_integer();
                 (max_heap_size, MaxHeapSize) -> OldMaxHeapSize
                     when MaxHeapSize :: max_heap_size(), OldMaxHeapSize :: max_heap_size();
                 (multi_scheduling, BlockState) -> OldBlockState
                     when
                         BlockState :: block | unblock | block_normal | unblock_normal,
                         OldBlockState :: blocked | disabled | enabled;
                 (outstanding_system_requests_limit, NewLimit) -> OldLimit
                     when NewLimit :: 1..134217727, OldLimit :: 1..134217727;
                 (scheduler_bind_type, How) -> OldBindType
                     when
                         How :: scheduler_bind_type() | default_bind,
                         OldBindType :: scheduler_bind_type();
                 (scheduler_wall_time, Boolean) -> OldBoolean
                     when Boolean :: boolean(), OldBoolean :: boolean();
                 (schedulers_online, SchedulersOnline) -> OldSchedulersOnline
                     when SchedulersOnline :: pos_integer(), OldSchedulersOnline :: pos_integer();
                 (system_logger, Logger) -> PrevLogger
                     when Logger :: logger | undefined | pid(), PrevLogger :: logger | undefined | pid();
                 (trace_control_word, TCW) -> OldTCW
                     when TCW :: non_neg_integer(), OldTCW :: non_neg_integer();
                 (time_offset, finalize) -> OldState when OldState :: preliminary | final | volatile;
                 (internal_cpu_topology, term()) -> term();
                 (sequential_tracer, Tracer) -> PrevTracer | false
                     when
                         Tracer :: pid() | port() | {module(), term()} | false,
                         PrevTracer :: pid() | port() | {module(), term()} | false;
                 (reset_seq_trace, true) -> true.
```

Sets a system flag to the given value.

The possible flags to set are:

- ```erlang
  system_flag(backtrace_depth, non_neg_integer()) -> non_neg_integer()
  ```

   Sets the maximum depth of call stack back-traces in the exit reason element of
  `'EXIT'` tuples. The flag also limits the stacktrace depth returned by
  `process_info/2` item [`current_stacktrace`](#process_info_current_stacktrace).

  Returns the old value of the flag.

- ```erlang
  system_flag(cpu_topology, cpu_topology()) -> cpu_topology()
  ```
  {: #system_flag_cpu_topology }

  > #### Warning {: .warning }
  >
  > _This argument is deprecated._ Instead of using this argument, use
  > command-line argument [`+sct`](erl_cmd.md#%2Bsct) in [erl](erl_cmd.md).
  >
  > When this argument is removed, a final CPU topology to use is determined at
  > emulator boot time.

  Sets the user-defined `CpuTopology`. The user-defined CPU topology overrides any
  automatically detected CPU topology. By passing `undefined` as `CpuTopology`,
  the system reverts to the CPU topology automatically detected. The returned
  value equals the value returned from `erlang:system_info(cpu_topology)` before
  the change was made.

  Returns the old value of the flag.

  The CPU topology is used when binding schedulers to logical processors. If
  schedulers are already bound when the CPU topology is changed, the schedulers
  are sent a request to rebind according to the new CPU topology.

  The user-defined CPU topology can also be set by passing command-line argument
  [`+sct`](erl_cmd.md#%2Bsct) to [erl](erl_cmd.md).

  For information on type `CpuTopology` and more, see
  [`erlang:system_info(cpu_topology)`](#system_info_cpu_topology) as
  well as command-line flags [`+sct`](erl_cmd.md#%2Bsct) and
  [`+sbt`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

- ```erlang
  system_flag(dirty_cpu_schedulers_online, pos_integer()) -> pos_integer()
  ```
  {: #system_flag_dirty_cpu_schedulers_online }

  Sets the number of dirty CPU schedulers online. Range is
  `1 <= DirtyCPUSchedulersOnline <= N`, where `N` is the smallest of the return
  values of `erlang:system_info(dirty_cpu_schedulers)` and
  `erlang:system_info(schedulers_online)`.

  Returns the old value of the flag.

  The number of dirty CPU schedulers online can change if the number of schedulers
  online changes. For example, if 12 schedulers and 6 dirty CPU schedulers are
  online, and [`system_flag/2`](`system_flag/2`) is used to set the number of
  schedulers online to 6, then the number of dirty CPU schedulers online is
  automatically decreased by half as well, down to 3. Similarly, the number of
  dirty CPU schedulers online increases proportionally to increases in the number
  of schedulers online.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers)
  and
  [`erlang:system_info(dirty_cpu_schedulers_online)`](#system_info_dirty_cpu_schedulers_online).

  Available since OTP 17.0

- ```erlang
  system_flag(erts_alloc, {Alloc :: atom(), F :: atom(), V :: integer()}) ->
    ok | notsup
  ```

  Sets system flags for [`erts_alloc(3)`](erts_alloc.md). `Alloc` is the allocator
  to affect, for example `binary_alloc`. `F` is the flag to change and `V` is the
  new value.

  Only a subset of all `erts_alloc` flags can be changed at run time. This subset
  is currently only the flag [`sbct`](erts_alloc.md#M_sbct).

  Returns `ok` if the flag was set or `notsup` if not supported by `erts_alloc`.

  Available since OTP 20.2.3

- ```erlang
  system_flag(fullsweep_after, non_neg_integer()) -> non_neg_integer()
  ```

  Sets system flag `fullsweep_after`. `Number` is a non-negative integer
  indicating how many times generational garbage collections can be done without
  forcing a fullsweep collection. The value applies to new processes, while
  processes already running are not affected.

  Returns the old value of the flag.

  In low-memory systems (especially without virtual memory), setting the value to
  `0` can help to conserve memory.

  This value can also be set through (OS) environment variable
  `ERL_FULLSWEEP_AFTER`.

- ```erlang
  system_flag(microstate_accounting, true | false | reset) -> boolean()
  ```
  {: #system_flag_microstate_accounting }

  Turns on/off microstate accounting measurements. When passing reset, all
  counters are reset to 0.

  For more information see
  [`statistics(microstate_accounting)`](#statistics_microstate_accounting).

  Available since OTP 19.0

- ```erlang
  system_flag(min_heap_size, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the default minimum heap size for processes. The size is specified in
  words. The new `min_heap_size` effects only processes spawned after the change
  of `min_heap_size` has been made. `min_heap_size` can be set for individual
  processes by using `spawn_opt/4` or `process_flag/2`.

  Returns the old value of the flag.

- ```erlang
  system_flag(min_bin_vheap_size, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the default minimum binary virtual heap size for processes. The size is
  specified in words. The new `min_bin_vhheap_size` effects only processes spawned
  after the change of `min_bin_vheap_size` has been made. `min_bin_vheap_size` can
  be set for individual processes by using [`spawn_opt/2,3,4`](`spawn_opt/4`) or
  `process_flag/2`.

  Returns the old value of the flag.

  Available since OTP R13B04

- ```erlang
  system_flag(max_heap_size, max_heap_size()) -> max_heap_size()
  ```
  {: #system_flag_max_heap_size }

  Sets the default maximum heap size settings for processes. The size is specified
  in words. The new `max_heap_size` effects only processes spawned after the
  change has been made. `max_heap_size` can be set for individual processes using
  [`spawn_opt/2,3,4`](`spawn_opt/4`) or
  [`process_flag/2`](#process_flag_max_heap_size).

  Returns the old value of the flag.

  For details on how the heap grows, see
  [Sizing the heap](GarbageCollection.md#sizing-the-heap) in the ERTS internal
  documentation.

  Available since OTP 19.0

- ```erlang
  system_flag(multi_scheduling, BlockState) -> OldBlockState when
    BlockState :: block | unblock | block_normal | unblock_normal,
    OldBlockState :: blocked | disabled | enabled
  ```
  {: #system_flag_multi_scheduling }

  If multi-scheduling is enabled, more than one scheduler thread is used by the
  emulator. Multi-scheduling can be blocked in two different ways. Either all
  schedulers but one is blocked, or all _normal_ schedulers but one is blocked.
  When only normal schedulers are blocked, dirty schedulers are free to continue
  to schedule processes.

  If `BlockState =:= block`, multi-scheduling is blocked. That is, one and only
  one scheduler thread will execute. If `BlockState =:= unblock` and no one else
  blocks multi-scheduling, and this process has blocked only once,
  multi-scheduling is unblocked.

  If `BlockState =:= block_normal`, normal multi-scheduling is blocked. That is,
  only one normal scheduler thread will execute, but multiple dirty schedulers can
  execute. If `BlockState =:= unblock_normal` and no one else blocks normal
  multi-scheduling, and this process has blocked only once, normal
  multi-scheduling is unblocked.

  One process can block multi-scheduling and normal multi-scheduling multiple
  times. If a process has blocked multiple times, it must unblock exactly as many
  times as it has blocked before it has released its multi-scheduling block. If a
  process that has blocked multi-scheduling or normal multi-scheduling exits, it
  automatically releases its blocking of multi-scheduling and normal
  multi-scheduling.

  The return values are `disabled`, `blocked`, `blocked_normal`, or `enabled`. The
  returned value describes the state just after the call to
  `erlang:system_flag(multi_scheduling, BlockState)` has been made. For
  information about the return values, see
  [`erlang:system_info(multi_scheduling)`](#system_info_multi_scheduling).

  > #### Note {: .info }
  >
  > Blocking of multi-scheduling and normal multi-scheduling is normally not
  > needed. If you feel that you need to use these features, consider it a few
  > more times again. Blocking multi-scheduling is only to be used as a last
  > resort, as it is most likely a _very inefficient_ way to solve the problem.

  See also
  [`erlang:system_info(multi_scheduling)`](#system_info_multi_scheduling),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](#system_info_normal_multi_scheduling_blockers),
  [`erlang:system_info(multi_scheduling_blockers)`](#system_info_multi_scheduling_blockers),
  and [`erlang:system_info(schedulers)`](#system_info_schedulers).

- ```erlang
  system_flag(outstanding_system_requests_limit, 1..134217727) -> 1..134217727
  ```
  {: #system_flag_outstanding_system_requests_limit }

  Sets a limit on the amount of outstanding requests made by a system process
  orchestrating system wide changes. Currently there are two such processes:

  - **The Code Purger** - The code purger orchestrates checking of references to
    old code before old code is removed from the system.

  - **The Literal Area Collector** - The literal area collector orchestrates
    copying of references from old literal areas before removal of such areas from
    the system.

  Each of these processes are allowed to have as many outstanding requests as this
  limit is set to. By default this limit is set to twice the amount of
  [schedulers](#system_info_schedulers) on the system. This will ensure
  that schedulers will have enough work scheduled to perform these operations as
  quickly as possible at the same time as other work will be interleaved with this
  work. Currently used limit can be checked by calling
  [`erlang:system_info(outstanding_system_requests_limit)`](#system_info_outstanding_system_requests_limit).

  This limit can also be set by passing the command line argument
  [`+zosrl <Limit>`](erl_cmd.md#%2Bzosrl) to `erl`.

  Available since OTP 24.2

- ```erlang
  system_flag(scheduler_bind_type, scheduler_bind_type() | default_bind) ->
    scheduler_bind_type()
  ```
  {: #system_flag_scheduler_bind_type }

  > #### Warning {: .warning }
  >
  > _This argument is deprecated._ Instead of using this argument, use
  > command-line argument [`+sbt`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md). When
  > this argument is removed, a final scheduler bind type to use is determined at
  > emulator boot time.

  Controls if and how schedulers are bound to logical processors.

  When `erlang:system_flag(scheduler_bind_type, How)` is called, an asynchronous
  signal is sent to all schedulers online, causing them to try to bind or unbind
  as requested.

  > #### Note {: .info }
  >
  > If a scheduler fails to bind, this is often silently ignored, as it is not
  > always possible to verify valid logical processor identifiers. If an error is
  > reported, an error event is logged. To verify that the schedulers have bound
  > as requested, call
  > [`erlang:system_info(scheduler_bindings)`](#system_info_scheduler_bindings).

  Schedulers can be bound on newer Linux, Solaris, FreeBSD, and Windows systems,
  but more systems will be supported in future releases.

  In order for the runtime system to be able to bind schedulers, the CPU topology
  must be known. If the runtime system fails to detect the CPU topology
  automatically, it can be defined. For more information on how to define the CPU
  topology, see command-line flag [`+sct`](erl_cmd.md#%2Bsct) in
  [erl](erl_cmd.md).

  The runtime system does by default _not_ bind schedulers to logical processors.

  > #### Note {: .info }
  >
  > If the Erlang runtime system is the only OS process binding threads to logical
  > processors, this improves the performance of the runtime system. However, if
  > other OS processes (for example, another Erlang runtime system) also bind
  > threads to logical processors, there can be a performance penalty instead.
  > Sometimes this performance penalty can be severe. If so, it is recommended to
  > not bind the schedulers.

  Schedulers can be bound in different ways. Argument `How` determines how
  schedulers are bound and can be any of the following:

  - **`unbound`** - Same as command-line argument [`+sbt u`](erl_cmd.md#%2Bsbt) in
    [erl](erl_cmd.md).

  - **`no_spread`** - Same as command-line argument [`+sbt ns`](erl_cmd.md#%2Bsbt)
    in [erl](erl_cmd.md).

  - **`thread_spread`** - Same as command-line argument
    [`+sbt ts`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`processor_spread`** - Same as command-line argument
    [`+sbt ps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`spread`** - Same as command-line argument [`+sbt s`](erl_cmd.md#%2Bsbt) in
    [erl](erl_cmd.md).

  - **`no_node_thread_spread`** - Same as command-line argument
    [`+sbt nnts`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`no_node_processor_spread`** - Same as command-line argument
    [`+sbt nnps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`thread_no_node_processor_spread`** - Same as command-line argument
    [`+sbt tnnps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`default_bind`** - Same as command-line argument
    [`+sbt db`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  The returned value equals `How` before flag `scheduler_bind_type` was changed.

  Failures:

  - **`notsup`** - If binding of schedulers is not supported.

  - **`badarg`** - If `How` is not one of the documented alternatives.

  - **`badarg`** - If CPU topology information is unavailable.

  The scheduler bind type can also be set by passing command-line argument
  [`+sbt`](erl_cmd.md#%2Bsbt) to [erl](erl_cmd.md).

  For more information, see
  [`erlang:system_info(scheduler_bind_type)`](#system_info_scheduler_bind_type),
  [`erlang:system_info(scheduler_bindings)`](#system_info_scheduler_bindings),
  as well as command-line flags [`+sbt`](erl_cmd.md#%2Bsbt) and
  [`+sct`](erl_cmd.md#%2Bsct) in [erl](erl_cmd.md).

- ```erlang
  system_flag(scheduler_wall_time, boolean()) -> boolean()
  ```
  {: #system_flag_scheduler_wall_time }

  Try enable or disable scheduler wall time measurements by passing `Boolean` as
  either `true` or `false`.

  For more information about how to use scheduler wall time measurements, see
  [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time).

  Scheduler wall time measurements has a node global state. It is either enabled
  for all processes on the node or disabled for all processes. Each process has a
  logical counter initialized as zero. A call with `Boolean` as `true` will
  increase that counter one step for the calling process. A call with `false` will
  decrease it one step unless it already is zero. The node global state for
  `scheduler_wall_time` will be enabled as long as there is at least one process
  alive with a counter value larger than zero. When a process terminates, its
  counter will also disappear. To ensure `scheduler_wall_time` is kept enabled,
  the process that enabled it must therefore be kept alive.

  Returns the old value of the node global state, `true` if scheduler wall time
  measurements were enabled, `false` if it were disabled.

  Scheduler wall time measurements do consume some cpu overhead and should not be
  left turned on unless used.

  Available since OTP R15B01

- ```erlang
  system_flag(schedulers_online, pos_integer()) -> pos_integer()
  ```
  {: #system_flag_schedulers_online }

  Sets the number of schedulers online. Range is
  `1 <= SchedulersOnline <= erlang:system_info(schedulers)`.

  Returns the old value of the flag.

  If the emulator was built with support for
  [dirty schedulers](#system_flag_dirty_cpu_schedulers_online), changing
  the number of schedulers online can also change the number of dirty CPU
  schedulers online. For example, if 12 schedulers and 6 dirty CPU schedulers are
  online, and [`system_flag/2`](`system_flag/2`) is used to set the number of
  schedulers online to 6, then the number of dirty CPU schedulers online is
  automatically decreased by half as well, down to 3. Similarly, the number of
  dirty CPU schedulers online increases proportionally to increases in the number
  of schedulers online.

  For more information, see
  [`erlang:system_info(schedulers)`](#system_info_schedulers) and
  [`erlang:system_info(schedulers_online)`](#system_info_schedulers_online).

- ```erlang
  system_flag(system_logger, logger | undefined | pid()) -> logger | undefined | pid()
  ```

  Sets the process that will receive the logging messages generated by ERTS. If
  set to `undefined`, all logging messages generated by ERTS will be dropped. The
  messages will be in the format:

  ```erlang
  {log,Level,Format,ArgList,Metadata} where

  Level = atom(),
  Format = string(),
  ArgList = list(term()),
  Metadata = #{ pid => pid(),
     group_leader => pid(),
     time := logger:timestamp(),
     error_logger := #{ emulator := true, tag := atom() }
  ```

  If the `system_logger` process dies, this flag will be reset to `logger`.

  The default is the process named `logger`.

  Returns the old value of the flag.

  > #### Note {: .info }
  >
  > This function is designed to be used by the KERNEL `m:logger`. Be careful if
  > you change it to something else as log messages may be lost. If you want to
  > intercept emulator log messages, do it by adding a specialized handler to the
  > KERNEL logger.

  Available since OTP 21.2

- ```erlang
  system_flag(trace_control_word, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the value of the node trace control word to `TCW`, which is to be an
  unsigned integer. For more information, see function
  [`set_tcw`](match_spec.md#set_tcw) in section "Match Specifications in Erlang"
  in the User's Guide.

  Returns the old value of the flag.

- ```erlang
  system_flag(time_offset, finalize) -> preliminary | final | volatile
  ```
  {: #system_flag_time_offset }

  Finalizes the [time offset](`time_offset/0`) when
  [single time warp mode](time_correction.md#single-time-warp-mode) is used. If
  another time warp mode is used, the time offset state is left unchanged.

  Returns the old state identifier, that is:

  - If `preliminary` is returned, finalization was performed and the time offset
    is now final.
  - If `final` is returned, the time offset was already in the final state. This
    either because another `erlang:system_flag(time_offset, finalize)` call or
    because [no time warp mode](time_correction.md#no-time-warp-mode) is used.
  - If `volatile` is returned, the time offset cannot be finalized because
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.

  Available since OTP 18.0

# `system_info`

```erlang
-spec system_info(allocated_areas) -> [tuple()];
                 (allocator) -> {Allocator, Version, Features, Settings}
                     when
                         Allocator :: undefined | glibc,
                         Version :: [non_neg_integer()],
                         Features :: [atom()],
                         Settings :: [{Subsystem :: atom(), [{Parameter :: atom(), Value :: term()}]}];
                 ({allocator, Alloc}) -> [_] when Alloc :: atom();
                 (alloc_util_allocators) -> [Alloc] when Alloc :: atom();
                 ({allocator_sizes, Alloc}) -> [_] when Alloc :: atom();
                 (atom_count) -> pos_integer();
                 (atom_limit) -> pos_integer();
                 (build_type) -> opt | debug | gcov | valgrind | gprof | lcnt | frmptr;
                 (c_compiler_used) -> {atom(), term()};
                 (check_io) -> [_];
                 (cpu_topology) -> CpuTopology when CpuTopology :: cpu_topology();
                 ({cpu_topology, defined | detected | used}) -> CpuTopology
                     when CpuTopology :: cpu_topology();
                 (cpu_quota) -> pos_integer() | unknown;
                 (creation) -> integer();
                 (debug_compiled) -> boolean();
                 (delayed_node_table_gc) -> infinity | non_neg_integer();
                 (dirty_cpu_schedulers) -> non_neg_integer();
                 (dirty_cpu_schedulers_online) -> non_neg_integer();
                 (dirty_io_schedulers) -> non_neg_integer();
                 (dist) -> binary();
                 (dist_buf_busy_limit) -> non_neg_integer();
                 (dist_ctrl) -> [{Node :: node(), ControllingEntity :: port() | pid()}];
                 (driver_version) -> string();
                 (dynamic_trace) -> none | dtrace | systemtap;
                 (dynamic_trace_probes) -> boolean();
                 (eager_check_io) -> boolean();
                 (embedded_3pps) -> #{included := [atom()], excluded := [atom()]};
                 (emu_flavor) -> emu | jit;
                 (emu_type) -> opt | debug | gcov | valgrind | gprof | lcnt | frmptr;
                 (end_time) -> non_neg_integer();
                 (ets_count) -> pos_integer();
                 (ets_limit) -> pos_integer();
                 (fullsweep_after) -> {fullsweep_after, non_neg_integer()};
                 (garbage_collection) -> garbage_collection_defaults();
                 (heap_sizes) -> [non_neg_integer()];
                 (heap_type) -> private;
                 (info) -> binary();
                 (kernel_poll) -> boolean();
                 (loaded) -> binary();
                 (logical_processors | logical_processors_available | logical_processors_online) ->
                     unknown | pos_integer();
                 (machine) -> string();
                 (max_heap_size) -> {max_heap_size, MaxHeapSize :: max_heap_size()};
                 (message_queue_data) -> message_queue_data();
                 (min_heap_size) -> {min_heap_size, MinHeapSize :: pos_integer()};
                 (min_bin_vheap_size) -> {min_bin_vheap_size, MinBinVHeapSize :: pos_integer()};
                 (modified_timing_level) -> integer() | undefined;
                 (multi_scheduling) -> disabled | blocked | blocked_normal | enabled;
                 (multi_scheduling_blockers) -> [Pid :: pid()];
                 (nif_version) -> string();
                 (normal_multi_scheduling_blockers) -> [Pid :: pid()];
                 (otp_release) -> string();
                 (os_monotonic_time_source) -> [{atom(), term()}];
                 (os_system_time_source) -> [{atom(), term()}];
                 (outstanding_system_requests_limit) -> 1..134217727;
                 (port_parallelism) -> boolean();
                 (port_count) -> non_neg_integer();
                 (port_limit) -> pos_integer();
                 (process_count) -> pos_integer();
                 (process_limit) -> pos_integer();
                 (procs) -> binary();
                 (scheduler_bind_type) -> scheduler_bind_type();
                 (scheduler_bindings) -> tuple();
                 (scheduler_id) -> SchedulerId :: pos_integer();
                 (schedulers | schedulers_online) -> pos_integer();
                 (smp_support) -> boolean();
                 (start_time) -> integer();
                 (system_architecture) -> string();
                 (system_logger) -> logger | undefined | pid();
                 (system_version) -> string();
                 (threads) -> boolean();
                 (thread_pool_size) -> non_neg_integer();
                 (time_correction) -> true | false;
                 (time_offset) -> preliminary | final | volatile;
                 (time_warp_mode) -> no_time_warp | single_time_warp | multi_time_warp;
                 (tolerant_timeofday) -> enabled | disabled;
                 (trace_control_word) -> non_neg_integer();
                 (update_cpu_info) -> changed | unchanged;
                 (version) -> string();
                 (wordsize | {wordsize, internal} | {wordsize, external}) -> 4 | 8;
                 (async_dist) -> boolean();
                 (halt_flush_timeout) -> non_neg_integer() | infinity.
```

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
Returns information about the current system.

The documentation of this function is broken into the following sections in
order to make it easier to navigate.

- [`Memory Allocation`](`m:erlang#system_info/1-memory-allocation`) -
  [`allocated_areas`](`m:erlang#system_info_allocated_areas`),
  [`allocator`](`m:erlang#system_info_allocator`),
  [`alloc_util_allocators`](`m:erlang#system_info_alloc_util_allocators`),
  [`allocator_sizes`](`m:erlang#system_info_allocator_sizes`)

- [`CPU Topology`](`m:erlang#system_info/1-cpu-topology`) -
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`),
  [`logical_processors`](`m:erlang#system_info_logical_processors`),
  [`update_cpu_info`](`m:erlang#system_info_update_cpu_info`)

- [`Process Information`](`m:erlang#system_info/1-process-information`) -
  [`fullsweep_after`](`m:erlang#system_info_fullsweep_after`),
  [`garbage_collection`](`m:erlang#system_info_garbage_collection`),
  [`heap_sizes`](`m:erlang#system_info_heap_sizes`),
  [`heap_type`](`m:erlang#system_info_heap_type`),
  [`max_heap_size`](`m:erlang#system_info_max_heap_size`),
  [`message_queue_data`](`m:erlang#system_info_message_queue_data`),
  [`min_heap_size`](`m:erlang#system_info_min_heap_size`),
  [`min_bin_vheap_size`](`m:erlang#system_info_min_bin_vheap_size`),
  [`procs`](`m:erlang#system_info_procs`)

- [`System Limits`](`m:erlang#system_info/1-system-limits`) -
  [`atom_count`](`m:erlang#system_info_atom_count`),
  [`atom_limit`](`m:erlang#system_info_atom_limit`),
  [`ets_count`](`m:erlang#system_info_ets_count`),
  [`ets_limit`](`m:erlang#system_info_ets_limit`),
  [`port_count`](`m:erlang#system_info_port_count`),
  [`port_limit`](`m:erlang#system_info_port_limit`),
  [`process_count`](`m:erlang#system_info_process_count`),
  [`process_limit`](`m:erlang#system_info_process_limit`)

- [`System Time`](`m:erlang#system_info/1-system-time`) -
  [`end_time`](`m:erlang#system_info_end_time`),
  [`os_monotonic_time_source`](`m:erlang#system_info_os_monotonic_time_source`),
  [`os_system_time_source`](`m:erlang#system_info_os_system_time_source`),
  [`start_time`](`m:erlang#system_info_start_time`),
  [`time_correction`](`m:erlang#system_info_time_correction`),
  [`time_offset`](`m:erlang#system_info_time_offset`),
  [`time_warp_mode`](`m:erlang#system_info_time_warp_mode`),
  [`tolerant_timeofday`](`m:erlang#system_info_tolerant_timeofday`)

- [`Scheduler Information`](`m:erlang#system_info/1-scheduler-information`) -
  [`dirty_cpu_schedulers`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`dirty_cpu_schedulers_online`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  [`dirty_io_schedulers`](`m:erlang#system_info_dirty_io_schedulers`),
  [`multi_scheduling`](`m:erlang#system_info_multi_scheduling`),
  [`multi_scheduling_blockers`](`m:erlang#system_info_multi_scheduling_blockers`),
  [`normal_multi_scheduling_blockers`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  [`scheduler_bind_type`](`m:erlang#system_info_scheduler_bind_type`),
  [`scheduler_bindings`](`m:erlang#system_info_scheduler_bindings`),
  [`scheduler_id`](`m:erlang#system_info_scheduler_id`),
  [`schedulers`](`m:erlang#system_info_schedulers`),
  [`smp_support`](`m:erlang#system_info_smp_support`),
  [`threads`](`m:erlang#system_info_threads`),
  [`thread_pool_size`](`m:erlang#system_info_thread_pool_size`)

- [`Distribution Information`](`m:erlang#system_info/1-distribution-information`) -
  [`creation`](`m:erlang#system_info_creation`),
  [`delayed_node_table_gc`](`m:erlang#system_info_delayed_node_table_gc`),
  [`dist`](`m:erlang#system_info_dist`),
  [`dist_buf_busy_limit`](`m:erlang#system_info_dist_buf_busy_limit`),
  [`dist_ctrl`](`m:erlang#system_info_dist_ctrl`)

- [`System Information`](`m:erlang#system_info/1-system-information`) -
  [`c_compiler_used`](`m:erlang#system_info_c_compiler_used`),
  [`check_io`](`m:erlang#system_info_check_io`),
  [`debug_compiled`](`m:erlang#system_info_debug_compiled`),
  [`driver_version`](`m:erlang#system_info_driver_version`),
  [`dynamic_trace`](`m:erlang#system_info_dynamic_trace`),
  [`dynamic_trace_probes`](`m:erlang#system_info_dynamic_trace_probes`),
  [`embedded_3pps`](`m:erlang#system_info_embedded_3pps`),
  [`emu_flavor`](`m:erlang#system_info_emu_flavor`),
  [`emu_type`](`m:erlang#system_info_emu_type`),
  [`info`](`m:erlang#system_info_info`),
  [`kernel_poll`](`m:erlang#system_info_kernel_poll`),
  [`loaded`](`m:erlang#system_info_loaded`),
  [`machine`](`m:erlang#system_info_machine`),
  [`modified_timing_level`](`m:erlang#system_info_modified_timing_level`),
  [`nif_version`](`m:erlang#system_info_nif_version`),
  [`otp_release`](`m:erlang#system_info_otp_release`),
  [`outstanding_system_requests_limit`](`m:erlang#system_info_outstanding_system_requests_limit`),
  [`port_parallelism`](`m:erlang#system_info_port_parallelism`),
  [`system_architecture`](`m:erlang#system_info_system_architecture`),
  [`system_logger`](`m:erlang#system_info_system_logger`),
  [`system_version`](`m:erlang#system_info_system_version`),
  [`trace_control_word`](`m:erlang#system_info_trace_control_word`),
  [`version`](`m:erlang#system_info_version`),
  [`wordsize`](`m:erlang#system_info_wordsize`)

## Memory Allocation

Returns various information about the memory allocators of the current system (emulator) as specified by `Item`:

* __`allocated_areas`__{: #system_info_allocated_areas } - Returns `[tuple()]` with
  information about miscellaneous allocated memory areas.

  Each tuple contains an atom describing the type of memory as first element and
  the amount of allocated memory in bytes as second element. When information
  about allocated and used memory is present, also a third element is present,
  containing the amount of used memory in bytes.

  `erlang:system_info(allocated_areas)` is intended for debugging, and the content
  is highly implementation-dependent. The content of the results therefore
  changes when needed without prior notice.

  Notice that the sum of these values is _not_ the total amount of memory
  allocated by the emulator. Some values are part of other values, and some
  memory areas are not part of the result. For information about the total amount
  of memory allocated by the emulator, see
  [`erlang:memory/0,1`](`erlang:memory/0`).

- `allocator`{: #system_info_allocator } - Returns
  ```
  {Allocator :: undefined | glibc,
   Version :: [non_neg_integer()],
   Features :: [atom()],
   Settings :: [{Subsystem :: atom(),
                 [{Parameter :: atom(),
                   Value :: term()}]
                 }]
  }
  ```

  where

  - `Allocator` corresponds to the `malloc()` implementation used. If
    `Allocator` equals `undefined`, the `malloc()` implementation used cannot be
    identified. `glibc` can be identified.
  - `Version` is a list of integers (but not a string) representing the
    version of the `malloc()` implementation used.
  - `Features` is a list of atoms representing the allocation features used.
  - `Settings` is a list of subsystems, their configurable parameters, and used
    values. Settings can differ between different combinations of platforms,
    allocators, and allocation features. Memory sizes are given in bytes.

  See also "System Flags Effecting erts_alloc" in
  [`erts_alloc(3)`](erts_alloc.md#flags).

- `{allocator, Alloc}`{: #system_info_allocator_tuple } - Returns
  information about the specified allocator. As from ERTS 5.6.1, the return
  value is a list of `{instance, InstanceNo, InstanceInfo}` tuples, where
  `InstanceInfo` contains information about a specific instance of the
  allocator. If `Alloc` is not a recognized allocator, `undefined` is
  returned. If `Alloc` is disabled, `false` is returned.

  Notice that the information returned is highly implementation-dependent and
  can be changed or removed at any time without prior notice. It was initially
  intended as a tool when developing new allocators, but as it can be of
  interest for others it has been briefly documented.

  The recognized allocators are listed in [`erts_alloc(3)`](erts_alloc.md).
  Information about super carriers can be obtained from ERTS 8.0 with
  `{allocator, erts_mmap}` or from ERTS 5.10.4; the returned list when calling
  with `{allocator, mseg_alloc}` also includes an `{erts_mmap, _}` tuple as one
  element in the list.

  After reading the `erts_alloc(3)` documentation, the returned information more
  or less speaks for itself, but it can be worth explaining some things. Call
  counts are presented by two values, the first value is giga calls, and the
  second value is calls. `mbcs` and `sbcs` denote multi-block carriers, and
  single-block carriers, respectively. Sizes are presented in bytes. When a
  size is not presented, it is the amount of something. Sizes and amounts are
  often presented by three values:

  - The first is the current value.
  - The second is the maximum value since the last call to
    `erlang:system_info({allocator, Alloc})`.
  - The third is the maximum value since the emulator was started.

  If only one value is present, it is the current value. `fix_alloc` memory
  block types are presented by two values. The first value is the memory pool
  size and the second value is the used memory size.

- `alloc_util_allocators`{: #system_info_alloc_util_allocators } - Returns a
  list of the names of all allocators using the ERTS internal `alloc_util`
  framework as atoms. For more information, see section
  [The alloc_util framework](erts_alloc.md#alloc_util) in `erts_alloc(3)`.

- `{allocator_sizes, Alloc}`{: #system_info_allocator_sizes } - Returns
  various size information for the specified allocator. The information
  returned is a subset of the information returned by
  [`erlang:system_info({allocator, Alloc})`](`m:erlang#system_info_allocator_tuple`).

## CPU Topology

Returns various information about the CPU topology of the current system (emulator) as specified by `Item`:

- `cpu_topology`{: #system_info_cpu_topology } - Returns the `t:cpu_topology()`
  currently used by the emulator. The CPU topology is used when binding
  schedulers to logical processors. The CPU topology used is the
  [user-defined CPU topology](`m:erlang#system_info_cpu_topology_defined`), if
  such exists, otherwise the
  [automatically detected CPU topology](`m:erlang#system_info_cpu_topology_detected`),
  if such exists. If no CPU topology exists, `undefined` is returned.

- `{cpu_topology, defined}`{: #system_info_cpu_topology_defined } - Returns
  the user-defined `t:cpu_topology()`. For more information, see command-line flag
  [`+sct`](erl_cmd.md#+sct) in `erl(1)` and argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `{cpu_topology, detected}`{: #system_info_cpu_topology_detected } -
  Returns the automatically detected `t:cpu_topology()`. The emulator detects the
  CPU topology on some newer Linux, Solaris, FreeBSD, and Windows systems. On
  Windows system with more than 32 logical processors, the CPU topology is not
  detected.

  For more information, see argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `{cpu_topology, used}` - Returns `CpuTopology` used by the emulator. For
  more information, see argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `logical_processors`{: #system_info_logical_processors } - Returns the
  detected number of logical processors configured in the system. The return
  value is either an integer, or the atom `unknown` if the emulator cannot
  detect the configured logical processors.

- `logical_processors_available`{: #system_info_logical_processors_available
  } - Returns the detected number of logical processors available to the Erlang
  runtime system. The return value is either an integer, or the atom `unknown`
  if the emulator cannot detect the available logical processors. The number of
  available logical processors is less than or equal to the number of
  [logical processors online](`m:erlang#system_info_logical_processors_online`).

- `logical_processors_online`{: #system_info_logical_processors_online } -
  Returns the detected number of logical processors online on the system. The
  return value is either an integer, or the atom `unknown` if the emulator
  cannot detect logical processors online. The number of logical processors
  online is less than or equal to the number of
  [logical processors configured](`m:erlang#system_info_logical_processors`).

- `cpu_quota`{: #system_info_cpu_quota } - Returns the detected CPU quota
  the emulator is limited by. The return value is an integer saying how many
  processors' worth of runtime we get (between 1 and the number of logical
  processors), or the atom `unknown` if the emulator cannot detect a quota.

- `update_cpu_info`{: #system_info_update_cpu_info } - The runtime system
  rereads the CPU information available and updates its internally stored
  information about the
  [detected CPU topology](`m:erlang#system_info_cpu_topology_detected`) and the
  number of logical processors
  [configured](`m:erlang#system_info_logical_processors`),
  [online](`m:erlang#system_info_logical_processors_online`),
  [available](`m:erlang#system_info_logical_processors_available`), and
  [cpu quota](`m:erlang#system_info_cpu_quota`).

  If the CPU information has changed since the last time it was read, the atom
  `changed` is returned, otherwise the atom `unchanged`. If the CPU information
  has changed, you probably want to
  [adjust the number of schedulers online](`m:erlang#system_flag_schedulers_online`).
  You typically want to have as many schedulers online as
  [logical processors available](`m:erlang#system_info_logical_processors_available`).
  
  Since: OTP R14B

## Process Information

Returns information about the default process heap settings:

- `fullsweep_after`{: #system_info_fullsweep_after } - Returns
  `{fullsweep_after, integer() >= 0}`, which is the `fullsweep_after` garbage
  collection setting used by default. For more information, see
  `garbage_collection` described below.

- `garbage_collection`{: #system_info_garbage_collection } - Returns
  `t:garbage_collection_defaults/0` describing the default garbage collection settings.
  A process spawned on the local node by a `spawn` or `spawn_link` uses these
  garbage collection settings. The default settings can be changed by using
  [`erlang:system_flag/2`](`erlang:system_flag/2`).
  [`spawn_opt/2,3,4`](`erlang:spawn_opt/4`) can spawn a process that does not
  use the default settings.

- `heap_sizes`{: #system_info_heap_sizes } - Returns a list of integers
  representing valid heap sizes in words. All Erlang heaps are sized from sizes
  in this list.

- `heap_type`{: #system_info_heap_type } - Returns the heap type used by the
  current emulator. One heap type exists:

  - `private` - Each process has a heap reserved for its use and no
    references between heaps of different processes are allowed. Messages
    passed between processes are copied between heaps.

- `max_heap_size`{: #system_info_max_heap_size } - Returns
  `{max_heap_size, MaxHeapSize}`, where `MaxHeapSize` is the current
  system-wide maximum heap size settings for spawned processes. This setting
  can be set using the command-line flags [`+hmax`](erl_cmd.md#+hmax),
  [`+hmaxk`](erl_cmd.md#+hmaxk), [`+hmaxel`](erl_cmd.md#+hmaxel) and
  [`+hmaxibl`](erl_cmd.md#+hmaxib) in `erl(1)`. It can also be changed at runtime
  using
  [`erlang:system_flag(max_heap_size, MaxHeapSize)`](`m:erlang#system_flag_max_heap_size`).
  For more details about the `max_heap_size` process flag, see
  [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).
  
  Since: OTP 19.0

- `message_queue_data`{: #system_info_message_queue_data } - Returns the
  default value of the `message_queue_data` process flag, which can be either
  `off_heap` or `on_heap`. The default value is set by the command-line
  argument [`+hmqd`](erl_cmd.md#+hmqd) in `erl(1)`. For more information, see the
  documentation of
  [`process_flag(message_queue_data, MQD)`](`m:erlang#process_flag_message_queue_data`).
  
  Since: OTP 19.0

- `min_heap_size`{: #system_info_min_heap_size } - Returns
  `{min_heap_size, MinHeapSize}`, where `MinHeapSize` is the current
  system-wide minimum heap size for spawned processes.
  
  Since: OTP R13B04

- `min_bin_vheap_size`{: #system_info_min_bin_vheap_size } - Returns
  `{min_bin_vheap_size, MinBinVHeapSize}`, where `MinBinVHeapSize` is the
  current system-wide minimum binary virtual heap size for spawned processes.
  
  Since: OTP R13B04

- `procs`{: #system_info_procs } - Returns a binary containing a string of
  process and port information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

## System Limits

Returns information about the current system (emulator) limits as specified by `Item`:

- `atom_count`{: #system_info_atom_count } - Returns the number of atoms
  currently existing at the local node. The value is given as an integer.
  
  Since: OTP 20.0

- `atom_limit`{: #system_info_atom_limit } - Returns the maximum number of
  atoms allowed. This limit can be increased at startup by passing
  command-line flag [`+t`](erl_cmd.md#+t_size) to `erl(1)`.
  
  Since: OTP 20.0

- `ets_count`{: #system_info_ets_count } - Returns the number of ETS tables
  currently existing at the local node.
  
  Since: OTP 21.1

- `ets_limit`{: #system_info_ets_limit } - Returns the limit for number of
  ETS tables. This limit is [partially obsolete](`m:ets#max_ets_tables`) and
  number of tables are only limited by available memory.
  
  Since: OTP R16B03

- `port_count`{: #system_info_port_count } - Returns the number of ports
  currently existing at the local node. The value is given as an integer. This
  is the same value as returned by `length(erlang:ports())`, but more
  efficient.
  
  Since: OTP R16B

- `port_limit`{: #system_info_port_limit } - Returns the maximum number of
  simultaneously existing ports at the local node as an integer. This limit can
  be configured at startup by using command-line flag [`+Q`](erl_cmd.md#+Q) in
  `erl(1)`.
  
  Since OTP R16B

- `process_count`{: #system_info_process_count } - Returns the number of
  processes currently existing at the local node. The value is given as an
  integer. This is the same value as returned by `length(processes())`, but
  more efficient.

- `process_limit`{: #system_info_process_limit } - Returns the maximum
  number of simultaneously existing processes at the local node. The value is
  given as an integer. This limit can be configured at startup by using
  command-line flag [`+P`](erl_cmd.md#+P) in `erl(1)`.

## System Time

Returns information about the current system (emulator) time as specified by `Item`:

- `end_time`{: #system_info_end_time } - The last
  [Erlang monotonic time](`erlang:monotonic_time/0`) in `native`
  [time unit](`t:time_unit/0`) that can be represented internally in
  the current Erlang runtime system instance. The time between the
  [start time](`m:erlang#system_info_start_time`) and the end time is at least a
  quarter of a millennium.
  
  Since: OTP 18.0

- `os_monotonic_time_source`{: #system_info_os_monotonic_time_source } -
  Returns a list containing information about the source of
  [OS monotonic time](time_correction.md#os-monotonic-time) that is used by the
  runtime system.

  If `[]` is returned, no OS monotonic time is available. The list contains
  two-tuples with `Key`s as first element, and `Value`s as second element. The
  order of these tuples is undefined. The following tuples can be part of the
  list, but more tuples can be introduced in the future:

  - `{function, Function}` - `Function` is the name of the function used.
    This tuple always exists if OS monotonic time is available to the runtime
    system.

  - `{clock_id, ClockId}` - This tuple only exists if `Function` can be used
    with different clocks. `ClockId` corresponds to the clock identifier used
    when calling `Function`.

  - `{resolution, OsMonotonicTimeResolution}` - Highest possible
    [resolution](time_correction.md#time-resolution) of current OS monotonic
    time source as parts per second. If no resolution information can be
    retrieved from the OS, `OsMonotonicTimeResolution` is set to the resolution
    of the time unit of `Function`s return value. That is, the actual
    resolution can be lower than `OsMonotonicTimeResolution`. Notice that the
    resolution does not say anything about the
    [accuracy](time_correction.md#time-accuracy) or whether the
    [precision](time_correction.md#time-precision) aligns with the resolution.
    You do, however, know that the precision is not better than
    `OsMonotonicTimeResolution`.

  - `{used_resolution, UsedOsMonotonicTimeResolution}` - The OS monotonic time
    resolution used by the runtime system. This is very often the same as
    `OsMonotonicTimeResolution`. However, on some systems the resolution has to
    be reduced in order to reliably produce monotonic timestamps. An example of
    this is when `QueryPerformanceCounter()` is used as OS monotonic time
    source on Windows. If such a reduction of the resolution has been done,
    `UsedOsMonotonicTimeResolution` will be smaller than
    `OsMonotonicTimeResolution`.

  - `{extended, Extended}` - `Extended` equals `yes` if the range of time
    values has been extended; otherwise `Extended` equals `no`. The range must
    be extended if `Function` returns values that wrap fast. This typically is
    the case when the return value is a 32-bit value.

  - `{parallel, Parallel}` - `Parallel` equals `yes` if `Function` is called
    in parallel from multiple threads. If it is not called in parallel, because
    calls must be serialized, `Parallel` equals `no`.

  - `{time, OsMonotonicTime}` - `OsMonotonicTime` equals current OS
    monotonic time in `native` [time unit](`t:time_unit/0`).
  
  Since: OTP 18.0

- `os_system_time_source`{: #system_info_os_system_time_source } - Returns a
  list containing information about the source of
  [OS system time](time_correction.md#os-system-time) that is used by the
  runtime system.

  The list contains two-tuples with `Key`s as first element, and `Value`s as
  second element. The order of these tuples is undefined. The following tuples
  can be part of the list, but more tuples can be introduced in the future:

  - `{function, Function}` - `Function` is the name of the function used.

  - `{clock_id, ClockId}` - Exists only if `Function` can be used with
    different clocks. `ClockId` corresponds to the clock identifier used when
    calling `Function`.

  - `{resolution, OsSystemTimeResolution}` - Highest possible
    [resolution](time_correction.md#time-resolution) of current OS system time
    source as parts per second. If no resolution information can be retrieved
    from the OS, `OsSystemTimeResolution` is set to the resolution of the time
    unit of `Function`s return value. That is, the actual resolution can be
    lower than `OsSystemTimeResolution`. Notice that the resolution does not
    say anything about the [accuracy](time_correction.md#time-accuracy) or
    whether the [precision](time_correction.md#time-precision) do align with the
    resolution. You do, however, know that the precision is not better than
    `OsSystemTimeResolution`.

  - `{parallel, Parallel}` - `Parallel` equals `yes` if `Function` is called
    in parallel from multiple threads. If it is not called in parallel, because
    calls needs to be serialized, `Parallel` equals `no`.

  - `{time, OsSystemTime}` - `OsSystemTime` equals current OS system time in
    `native` [time unit](`t:time_unit/0`).
  
  Since: OTP 18.0

- `start_time`{: #system_info_start_time } - The
  [Erlang monotonic time](`erlang:monotonic_time/0`) in `native`
  [time unit](`t:time_unit/0`) at the time when current Erlang runtime
  system instance started.

  See also [`erlang:system_info(end_time)`](`m:erlang#system_info_end_time`).
  
  Since: OTP 18.0

- `time_correction`{: #system_info_time_correction } - Returns a `t:boolean()`
  value indicating whether [time correction](time_correction.md#time-correction)
  is enabled or not.
  
  Since: OTP 18.0

- `time_offset`{: #system_info_time_offset } - Returns the state of the time
  offset:

  - `preliminary` - The time offset is preliminary, and will be changed and
    finalized later. The preliminary time offset is used during the preliminary
    phase of the
    [single time warp mode](time_correction.md#single-time-warp-mode).

  - `final` - The time offset is final. This either because
    [no time warp mode](time_correction.md#no-time-warp-mode) is used, or
    because the time offset have been finalized when
    [single time warp mode](time_correction.md#single-time-warp-mode) is used.

  - `volatile` - The time offset is volatile. That is, it can change at any
    time. This is because
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.
  
  Since: OTP 18.0

- `time_warp_mode`{: #system_info_time_warp_mode } - Returns a value
  identifying the [time warp mode](time_correction.md#time-warp-modes) that is
  used:

  - `no_time_warp` - The
    [no time warp mode](time_correction.md#no-time-warp-mode) is used.

  - `single_time_warp` - The
    [single time warp mode](time_correction.md#single-time-warp-mode) is used.

  - `multi_time_warp` - The
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.
  
  Since: OTP 18.0

- `tolerant_timeofday`{: #system_info_tolerant_timeofday } - Returns whether
  a pre ERTS 7.0 backwards compatible compensation for sudden changes of system
  time is `enabled` or `disabled`. Such compensation is `enabled` when the
  [time offset](`m:erlang#system_info_time_offset`) is `final`, and
  [time correction](`m:erlang#system_info_time_correction`) is enabled.
  
  Since: OTP 17.1

## Scheduler Information

Returns information about schedulers, scheduling and threads in the current system as specified by `Item`:

- `dirty_cpu_schedulers`{: #system_info_dirty_cpu_schedulers } - Returns the
  number of dirty CPU scheduler threads used by the emulator. Dirty CPU
  schedulers execute CPU-bound native functions, such as NIFs, linked-in
  driver code, and BIFs that cannot be managed cleanly by the normal emulator
  schedulers.

  The number of dirty CPU scheduler threads is determined at emulator boot time
  and cannot be changed after that. However, the number of dirty CPU scheduler
  threads online can be changed at any time. The number of dirty CPU schedulers
  can be set at startup by passing command-line flag [`+SDcpu`](erl_cmd.md#+SDcpu)
  or [`+SDPcpu`](erl_cmd.md#+SDPcpu) in `erl(1)`.

  See also
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`),
  [`erlang:system_info(dirty_cpu_schedulers_online)`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  [`erlang:system_info(dirty_io_schedulers)`](`m:erlang#system_info_dirty_io_schedulers`),
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  and
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).
  
  Since: OTP 17.0

- `dirty_cpu_schedulers_online`{: #system_info_dirty_cpu_schedulers_online
  } - Returns the number of dirty CPU schedulers online. The return value
  satisfies `1 <= DirtyCPUSchedulersOnline <= N`, where `N` is the smallest of
  the return values of `erlang:system_info(dirty_cpu_schedulers)` and
  `erlang:system_info(schedulers_online)`.

  The number of dirty CPU schedulers online can be set at startup by passing
  command-line flag [`+SDcpu`](erl_cmd.md#+SDcpu) in `erl(1)`.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`erlang:system_info(dirty_io_schedulers)`](`m:erlang#system_info_dirty_io_schedulers`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  and
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).
  
  Since: OTP 17.0

- `dirty_io_schedulers`{: #system_info_dirty_io_schedulers } - Returns the
  number of dirty I/O schedulers as an integer. Dirty I/O schedulers execute
  I/O-bound native functions, such as NIFs and linked-in driver code, which
  cannot be managed cleanly by the normal emulator schedulers.

  This value can be set at startup by passing command-line argument
  [`+SDio`](erl_cmd.md#+SDio) in `erl(1)`.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`erlang:system_info(dirty_cpu_schedulers_online)`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  and
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).
  
  Since: OTP 17.0

- `multi_scheduling`{: #system_info_multi_scheduling } - Returns one of the
  following:

  - `disabled` - The emulator has been started with only one scheduler
    thread.

  - `blocked` - The emulator has more than one scheduler thread, but all
    scheduler threads except one are blocked. That is, only one scheduler
    thread schedules Erlang processes and executes Erlang code.

  - `blocked_normal` - The emulator has more than one scheduler thread, but
    all normal scheduler threads except one are blocked. Notice that dirty
    schedulers are not blocked, and can schedule Erlang processes and execute
    native code.

  - `enabled` - The emulator has more than one scheduler thread, and no
    scheduler threads are blocked. That is, all available scheduler threads
    schedule Erlang processes and execute Erlang code.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `multi_scheduling_blockers`{: #system_info_multi_scheduling_blockers } -
  Returns a list of `Pid`s when multi-scheduling is blocked, otherwise the
  empty list is returned. The `Pid`s in the list represent all the processes
  currently blocking multi-scheduling. A `Pid` occurs only once in the list,
  even if the corresponding process has blocked multiple times.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `normal_multi_scheduling_blockers`{:
  #system_info_normal_multi_scheduling_blockers } - Returns a list of `Pid`s
  when normal multi-scheduling is blocked (that is, all normal schedulers but
  one is blocked), otherwise the empty list is returned. The `Pid`s in the
  list represent all the processes currently blocking normal multi-scheduling.
  A `Pid` occurs only once in the list, even if the corresponding process has
  blocked multiple times.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).
  
  Since: OTP 19.0

- `scheduler_bind_type`{: #system_info_scheduler_bind_type } - Returns
  `t:scheduler_bind_type()`, information about how the user has requested
  schedulers to be bound or not bound.

  Notice that although a user has requested schedulers to be bound, they can
  silently have failed to bind. To inspect the scheduler bindings, call
  [`erlang:system_info(scheduler_bindings)`](`m:erlang#system_info_scheduler_bindings`).

  For more information, see command-line argument [`+sbt`](erl_cmd.md#+sbt) in
  `erl(1)` and
  [`erlang:system_info(scheduler_bindings)`](`m:erlang#system_info_scheduler_bindings`).

- `scheduler_bindings`{: #system_info_scheduler_bindings } - Returns
  information about the currently used scheduler bindings.

  A tuple of a size equal to
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`) is
  returned. The tuple elements are integers or the atom `unbound`. Logical
  processor identifiers are represented as integers. The `N`th element of the
  tuple equals the current binding for the scheduler with the scheduler
  identifier equal to `N`. For example, if the schedulers are bound,
  `element(erlang:system_info(scheduler_id), erlang:system_info(scheduler_bindings))`
  returns the identifier of the logical processor that the calling process is
  executing on.

  Notice that only schedulers online can be bound to logical processors.

  For more information, see command-line argument [`+sbt`](erl_cmd.md#+sbt) in
  `erl(1)` and
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`).

- `scheduler_id`{: #system_info_scheduler_id } - Returns the scheduler ID
  (`SchedulerId`) of the scheduler thread that the calling process is
  executing on. `SchedulerId` is a positive integer, where
  `1 <= SchedulerId <= erlang:system_info(schedulers)`.

  See also
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `schedulers`{: #system_info_schedulers } - Returns the number of scheduler
  threads used by the emulator. Scheduler threads online schedules Erlang
  processes and Erlang ports, and execute Erlang code and Erlang linked-in
  driver code.

  The number of scheduler threads is determined at emulator boot time and cannot
  be changed later. However, the number of schedulers online can be changed at
  any time.

  See also
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  [`erlang:system_info(scheduler_id)`](`m:erlang#system_info_scheduler_id`),
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`)
  and
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`).

- `schedulers_online`{: #system_info_schedulers_online } - Returns the
  number of schedulers online. The scheduler identifiers of schedulers online
  satisfy the relationship
  `1 <= SchedulerId <= erlang:system_info(schedulers_online)`.

  For more information, see
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`) and
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).

- `smp_support`{: #system_info_smp_support } - Returns `true`.

- `threads`{: #system_info_threads } - Returns `true`.

- `thread_pool_size`{: #system_info_thread_pool_size } - Returns the number of async threads in the
  async thread pool used for asynchronous driver calls
  ([ `erl_driver:driver_async()`](erl_driver.md#driver_async)). The value is
  given as an integer.

## Distribution Information

Returns information about Erlang Distribution in the current system as specified by `Item`:

- `async_dist`{: #system_info_async_dist } - Returns the value of the command line argument
  [+pad <boolean>](erl_cmd.md#+pad) which the runtime system use. This value
  determines the default [`async_dist`](`m:erlang#process_flag_async_dist`)
  value for newly spawned processes.
  
  Since: OTP 25.3

- `creation`{: #system_info_creation } - Returns the "creation" value of the
  local node as an integer. The creation is changed when a node is restarted.
  The creation of a node is stored in process identifiers, port identifiers, and
  references. This makes it possible to distinguish between identifiers from
  different incarnations of a node. Creation values are currently 32-bit
  positive integers, but this may change in future releases. If the node is not
  alive, `0` is returned.

- `delayed_node_table_gc`{: #system_info_delayed_node_table_gc } - Returns
  the amount of time in seconds garbage collection of an entry in a node table
  is delayed. This limit can be set on startup by passing command-line flag
  [`+zdntgc`](erl_cmd.md#+zdntgc) to `erl(1)`. For more information, see the
  documentation of the command-line flag.
  
  Since: OTP 18.0

- `dist`{: #system_info_dist } - Returns a binary containing a string of
  distribution information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `dist_buf_busy_limit`{: #system_info_dist_buf_busy_limit } - Returns the
  value of the distribution buffer busy limit in bytes. This limit can be set
  at startup by passing command-line flag [`+zdbbl`](erl_cmd.md#+zdbbl) to
  `erl(1)`.
  
  Since: OTP R14B01

- `dist_ctrl`{: #system_info_dist_ctrl } - Returns a list of tuples
  `{Node :: node(), ControllingEntity :: port() | pid()}`, one entry for each
  connected remote node. `Node` is the node name and `ControllingEntity` is the
  port or process identifier responsible for the communication to that node.
  More specifically, `ControllingEntity` for nodes connected through TCP/IP (the
  normal case) is the socket used in communication with the specific node.

## System Information

Returns various information about the current system (emulator) as specified by `Item`:

- `c_compiler_used`{: #system_info_c_compiler_used } - Returns a two-tuple
  describing the C compiler used when compiling the runtime system. The first
  element is an atom describing the name of the compiler, or `undefined` if
  unknown. The second element is a term describing the version of the compiler,
  or `undefined` if unknown.

- `check_io`{: #system_info_check_io } - Returns a list containing
  miscellaneous information about the emulators internal I/O checking. Notice
  that the content of the returned list can vary between platforms and over
  time. It is only guaranteed that a list is returned.

- `debug_compiled`{: #system_info_debug_compiled } - Returns `true` if the
  emulator has been debug-compiled, otherwise `false`.

- `driver_version`{: #system_info_driver_version } - Returns a string
  containing the Erlang driver version used by the runtime system. It has the
  form ["<major ver>.<minor ver>"](erl_driver.md#version_management).

- `dynamic_trace`{: #system_info_dynamic_trace } - Returns an atom
  describing the dynamic trace framework compiled into the virtual machine. It
  can be `dtrace`, `systemtap`, or `none`. For a commercial or standard build,
  it is always `none`. The other return values indicate a custom configuration
  (for example, `./configure --with-dynamic-trace=dtrace`). For more
  information about dynamic tracing, see [`dyntrace(3)`](`m:dyntrace`) manual
  page and the `README.dtrace`/`README.systemtap` files in the Erlang source
  code top directory.
  
  Since: OTP R15B01

- `dynamic_trace_probes`{: #system_info_dynamic_trace_probes } - Returns a
  `t:boolean()` indicating if dynamic trace probes (`dtrace` or `systemtap`) are
  built into the emulator. This can only be `true` if the virtual machine was
  built for dynamic tracing (that is, `system_info(dynamic_trace)` returns
  `dtrace` or `systemtap`).
  
  Since: OTP R15B01

- `embedded_3pps`{: #system_info_embedded_3pps } - Returns a map with
  information about third party products embedded in the source code of
  the Erlang/OTP runtime system. Note that often only parts (in some cases
  minuscule parts) of these third party products have been embedded in the
  source code. Currently the returned map contains the following keys:

  - `included` - The value of this key is a list of atoms where each atom
    represents a third party product from which code has been embedded in
    the source code and also has been included into the built runtime system.

  - `excluded` - The value of this key is a list of atoms where each atom
    represents a third party product from which code has been embedded in
    the source code, but which has been completely excluded in the built
    runtime system. The exclusion might be due to the functionality being
    disabled, due to the functionality being implemented using other
    primitives on the specific system, or due to the runtime system being
    linked against the same or another third party product installed on the
    system.

  Note that the returned map may be extended with new key/value pairs at
  any time.

  Since: OTP 28.5

- `emu_flavor`{: #system_info_emu_flavor } - Returns an atom describing the
  flavor of the runtime system. This will be either `emu` or `jit`. Possible
  return values can be added or removed at any time without prior notice.
  
  Since: OTP 24.0

- `emu_type`{: #system_info_emu_type } - Returns an atom describing the
  build type of the runtime system. This is normally the atom `opt` for
  optimized. Other possible return values are `debug`, `gcov`, `valgrind`,
  `gprof`, and `lcnt`. Possible return values can be added or removed at any
  time without prior notice.
  
  Since: OTP 24.0

- `halt_flush_timeout`{: #system_info_halt_flush_timeout } - Returns the
  default *halt flush timeout* set by the `erl`
  [`+zhft <Timeout>`](erl_cmd.md#+zhft) command line flag.

  Since: OTP 27.0

- `info`{: #system_info_info } - Returns a binary containing a string of
  miscellaneous system information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `kernel_poll`{: #system_info_kernel_poll } - Returns `true` if the
  emulator uses some kind of kernel-poll implementation, otherwise `false`.

- `loaded`{: #system_info_loaded } - Returns a binary containing a string of
  loaded module information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `machine`{: #system_info_machine } - Returns a string containing the
  Erlang machine name.

- `modified_timing_level`{: #system_info_modified_timing_level } - Returns
  the modified timing-level (an `t:integer()`) if modified timing is enabled,
  otherwise `undefined`. For more information about modified timing, see
  command-line flag [`+T`](erl_cmd.md#+T_level) in `erl(1)`

- `nif_version`{: #system_info_nif_version } - Returns a string containing
  the version of the Erlang NIF interface used by the runtime system. It is on
  the form "<major ver>.<minor ver>".
  
  Since: OTP 17.4

- `otp_release`{: #system_info_otp_release } -
  Returns a string containing the OTP release number of the OTP release that the
  currently executing ERTS application is part of.

  As from Erlang/OTP 17, the OTP release number corresponds to the major OTP
  version number. No `erlang:system_info()` argument gives the exact OTP
  version. This is because the exact OTP version in the general case is
  difficult to determine. For more information, see the description of versions
  in [System principles](`e:system:versions.md`) in System Documentation.

- `outstanding_system_requests_limit`{:
  #system_info_outstanding_system_requests_limit } - Returns the limit on the
  amount of outstanding requests made by a system process orchestrating system
  wide changes. See
  [`erlang:system_flag(outstanding_system_requests_limit, Limit)`](`m:erlang#system_flag_outstanding_system_requests_limit`)
  for more information.
  
  Since: OTP 24.2

- `port_parallelism`{: #system_info_port_parallelism } - Returns the default
  port parallelism scheduling hint used. For more information, see
  command-line argument [`+spp`](erl_cmd.md#+spp) in `erl(1)`.
  
  Since: OTP R16B

- `system_architecture`{: #system_info_system_architecture } - Returns a
  string containing the processor and OS architecture the emulator is built
  for.

- `system_logger`{: #system_info_system_logger } - Returns the current
  `system_logger` as set by [`erlang:system_flag(system_logger,
  *)`](`erlang:system_flag/2`).
  
  Since: OTP 21.3

- `system_version`{: #system_info_system_version } - Returns a string
  containing version number and some important properties, such as the number of
  schedulers.

- `trace_control_word`{: #system_info_trace_control_word } - Returns the
  value of the node trace control word. For more information, see function
  `get_tcw` in section [Match Specifications in Erlang](match_spec.md#get_tcw)
  in the User's Guide.

- `version`{: #system_info_version } - Returns a string containing the
  version number of the emulator.

- `wordsize`{: #system_info_wordsize } - Same as `{wordsize, internal}`.

- `{wordsize, internal}` - Returns the size of Erlang term words in bytes as
  an integer, that is, 4 is returned on a 32-bit architecture, and 8 is
  returned on a 64-bit architecture.

- `{wordsize, external}` - Returns the true word size of the emulator, that
  is, the size of a pointer. The value is given in bytes as an integer. On a
  pure 32-bit architecture, 4 is returned. On a 64-bit architecture, 8 is
  returned.

# `system_monitor`

```erlang
-spec system_monitor() -> MonSettings
                        when
                            MonSettings :: undefined | {MonitorPid, Options},
                            MonitorPid :: pid(),
                            Options :: [system_monitor_option()].
```

Returns the current system monitoring settings set by
[`erlang:system_monitor/2`](`system_monitor/2`) as `{MonitorPid, Options}`, or
`undefined` if no settings exist.

The order of the options can be different from the one that was set.

# `system_monitor`

```erlang
-spec system_monitor(Arg) -> MonSettings
                        when
                            Arg :: undefined | {MonitorPid, Options},
                            MonSettings :: undefined | {MonitorPid, Options},
                            MonitorPid :: pid(),
                            Options :: [system_monitor_option()].
```

When called with argument `undefined`, all system performance monitoring
settings are cleared.

Calling the function with `{MonitorPid, Options}` as argument is the same as
calling [`erlang:system_monitor(MonitorPid, Options)`](`system_monitor/2`).

Returns the previous system monitor settings just like
[`erlang:system_monitor/0`](`system_monitor/0`).

# `system_monitor`

```erlang
-spec system_monitor(MonitorPid, Options) -> MonSettings
                        when
                            MonitorPid :: pid(),
                            Options :: [system_monitor_option()],
                            MonSettings :: undefined | {OldMonitorPid, OldOptions},
                            OldMonitorPid :: pid(),
                            OldOptions :: [system_monitor_option()].
```

Sets the system event monitoring options. `MonitorPid` is a local process
identifier (pid) receiving system monitor messages.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:system/3` that operate on
  > dynamic trace sessions.

The second argument is a list of monitoring options to enable:

- **`{long_gc, Time}`**
- **`{long_message_queue, {Disable, Enable}}`**
- **`{long_schedule, Time}`**
- **`{large_heap, Size}`**
- **`busy_port`**
- **`busy_dist_port`**

For more detailed descriptions about the monitoring options, see
`trace:system/3`.

Unlike `trace:system/3`, the arguments to
[`system_monitor/2`](`system_monitor/2`) specify how all system monitoring
should be set, not how it should be changed. This means only one process at a
time (`MonitorPid`) can be the receiver of messages from system monitoring set
with this function. Also, the way to clear a specific monitor option is to not
include it in the list `Options`. All system monitoring will, however, be
cleared if the process identified by `MonitorPid` terminates.

There are no special option values (like zero) to clear an option. Some of the
options have a unspecified minimum value. Lower values will be adjusted to the
minimum value. For example, it is currently not possible to monitor all garbage
collections with `{long_gc, 0}`.

Returns the previous system monitor settings just like
[`erlang:system_monitor/0`](`system_monitor/0`).

> #### Note {: .info }
>
> If a monitoring process gets so large that it itself starts to cause system
> monitor messages when garbage collecting, the messages enlarge the process
> message queue and probably make the problem worse.
>
> Keep the monitoring process neat and do not set the system monitor limits too
> tight.

Failures:

- **`badarg`** - If `MonitorPid` does not exist.

- **`badarg`** - If `MonitorPid` is not a local process.

# `system_profile`

```erlang
-spec system_profile() -> ProfilerSettings
                        when
                            ProfilerSettings :: undefined | {ProfilerPid, Options},
                            ProfilerPid :: pid() | port(),
                            Options :: [system_profile_option()].
```

Returns the current system profiling settings set by
[`erlang:system_profile/2`](`system_profile/2`) as `{ProfilerPid, Options}`, or
`undefined` if there are no settings. The order of the options can be different
from the one that was set.

# `system_profile`

```erlang
-spec system_profile(ProfilerPid, Options) -> ProfilerSettings
                        when
                            ProfilerPid :: pid() | port() | undefined,
                            Options :: [system_profile_option()],
                            ProfilerSettings :: undefined | {pid() | port(), [system_profile_option()]}.
```

Sets system profiler options. `ProfilerPid` is a local process identifier (pid)
or port receiving profiling messages. The receiver is excluded from all
profiling. The second argument is a list of profiling options:

- **`exclusive`** - If a synchronous call to a port from a process is done, the
  calling process is considered not runnable during the call runtime to the
  port. The calling process is notified as `inactive`, and later `active` when
  the port callback returns.

- **`monotonic_timestamp`** - Time stamps in profile messages use
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time). The time
  stamp (Ts) has the same format and value as produced by
  `erlang:monotonic_time(nanosecond)`.

- **`runnable_procs`** - If a process is put into or removed from the run queue,
  a message, `{profile, Pid, State, Mfa, Ts}`, is sent to `ProfilerPid`. Running
  processes that are reinserted into the run queue after having been pre-empted
  do not trigger this message.

- **`runnable_ports`** - If a port is put into or removed from the run queue, a
  message, `{profile, Port, State, 0, Ts}`, is sent to `ProfilerPid`.

- **`scheduler`** - If a scheduler is put to sleep or awoken, a message,
  `{profile, scheduler, Id, State, NoScheds, Ts}`, is sent to `ProfilerPid`.

- **`strict_monotonic_timestamp`** - Time stamps in profile messages consist of
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time) and a
  monotonically increasing integer. The time stamp (Ts) has the same format and
  value as produced by
  `{erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])}`.

- **`timestamp`** - Time stamps in profile messages include a time stamp (Ts)
  that has the same form as returned by `erlang:now()`. This is also the default
  if no time stamp flag is specified. If `cpu_timestamp` has been enabled
  through `trace:process/4`, this also effects the time stamp
  produced in profiling messages when flag `timestamp` is enabled.

> #### Note {: .info }
>
> `erlang:system_profile` behavior can change in a future release.

# `system_time`
*since OTP 18.0* 

```erlang
-spec system_time() -> integer().
```

Returns current [Erlang system time](time_correction.md#erlang-system-time) in
`native` [time unit](`t:time_unit/0`).

Calling `erlang:system_time()` is equivalent to
[`erlang:monotonic_time()`](`monotonic_time/0`)`+`[`erlang:time_offset()`](`time_offset/0`).

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.

# `system_time`
*since OTP 18.0* 

```erlang
-spec system_time(Unit) -> integer() when Unit :: time_unit().
```

Returns current [Erlang system time](time_correction.md#erlang-system-time)
converted into the `Unit` passed as argument.

Calling `erlang:system_time(Unit)` is equivalent to
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[`erlang:system_time()`](`system_time/0`)`, native, Unit)`.

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.

# `term_to_binary`
*auto-imported* 

```erlang
-spec term_to_binary(Term) -> ext_binary() when Term :: term().
```

Returns a binary data object that is the result of encoding `Term` according to
the [Erlang external term format](erl_ext_dist.md).

This can be used for various purposes, such as efficiently writing a
term to a file or sending an Erlang term through a communication
channel not supported by distributed Erlang.

See also `binary_to_term/1`.

> #### Note {: .info }
>
> There is no guarantee that this function will always return the same encoded
> representation for the same term.

## Examples

```erlang
1> Bin = term_to_binary(hello).
<<131,119,5,104,101,108,108,111>>
2> hello = binary_to_term(Bin).
hello
```

# `term_to_binary`
*auto-imported* 

```erlang
-spec term_to_binary(Term, Options) -> ext_binary()
                        when
                            Term :: term(),
                            Options ::
                                [compressed |
                                 {compressed, Level :: 0..9} |
                                 deterministic |
                                 {minor_version, Version :: 0..2} |
                                 local].
```

Returns a binary data object that is the result of encoding `Term` according to
the Erlang external term format.

Supported options:

- **`compressed`** - Compress the external term format. The compressed format is
  automatically recognized by [`binary_to_term/1`](`binary_to_term/1`) as from
  Erlang/OTP R7B.

- **`{compressed, Level}`** - Compress the external term format to a given
  level. The compression level is specified by `Level` which is an integer in
  the range 0 to 9, where:

  - **`0`** - No compression is applied (equivalent to omitting the `compressed`
    option).

  - **`1`** - Fastest compression but may not compress as well as the higher
    levels.

  - **`6`** - Default level when the `compressed` option is provided.

  - **`9`** - Highest compression level, taking the longest time while
    attempting to produce the smallest result. However, depending on
    the input term, level 9 compression may not always produce a smaller
    result than level 1 compression.

- **`{minor_version, Version}`**(Since R11B-4)  
  The option can be used to control some encoding details. Valid values for
  `Version` are:

  - **`0`** - Floats are encoded using a textual representation.

    Atoms that can be represented by a latin1 string are encoded using latin1
    while only atoms that cannot be represented by latin1 are encoded using
    utf8.

  - **`1`** - Floats are encoded in a more space-efficient and exact way (namely
    in the 64-bit IEEE format, rather than converted to a textual
    representation). As from Erlang/OTP R11B-4,
    [`binary_to_term/1`](`binary_to_term/1`) can decode this representation.

    Atoms that can be represented by a latin1 string are encoded using latin1
    while only atoms that cannot be represented by latin1 are encoded using
    utf8.

  - **`2`** - This is as of Erlang/OTP 26.0 the _default_. Atoms are
    unconditionally encoded using utf8. Erlang/OTP systems as of R16B can decode
    this representation.

- **`deterministic`**(Since OTP 24.1)  
  This option can be used to ensure that, within the same major release of
  Erlang/OTP, the same encoded representation is returned for the same term.
  There is still no guarantee that the encoded representation remains the same
  between major releases of Erlang/OTP.

  This option cannot be combined with the `local` option.

- **`local`[](){: #term_to_binary_local } **(Since OTP 26.0)  
  This option encodes `Term` in an alternative local version of the
  external term format. When decoded by the same runtime system
  instance, it produces a term identical to original term, even if the
  node name and/or [creation](#system_info_creation) of the runtime
  system instance have changed between encoding and decoding.

  When encoding without the `local` option, local identifiers such as
  [pids](`t:pid/0`), [ports](`t:port/0`), and
  [references](`t:reference/0`) will not remain the same if node name
  and/or creation of the runtime system instance changed
  between encoding and decoding. This is because such identifiers refer to
  a specific node by node name and creation.

  The node name and creation change when the distribution is started
  or stopped. The distribution starts when the runtime system is
  started with the [`-name`](erl_cmd.md#name) or
  [`-sname`](erl_cmd.md#sname) command-line arguments. Note that the
  actual start of the distribution occurs after other code in the
  startup phase has already begun executing. The distribution can also
  be started by calling `net_kernel:start/2` and stopped by calling
  [`net_kernel:stop/1`](`net_kernel:stop/0`), provided it was not
  started via the command line.

  When decoding a term encoded with the `local` option using, for
  example, `binary_to_term/1`, the runtime system attempts to verify
  that the term was encoded by the same runtime system instance. In
  most cases, decoding will fail if it was encoded by a different
  instance. However, this verification is not foolproof. You _should_
  ensure that terms encoded with the local option are _only_ decoded by
  the same Erlang runtime system instance that encoded them.

  Since only the runtime system that encoded a term using the `local`
  option can decode it, the local encoding is typically pieced
  together with something else to produce a reply to where the local
  encoding originates. If a term encoded with the local option has its
  leading version number stripped, it can be embedded as part of a
  larger term (for example as an element in a tuple) when encoding in
  the external term format using, for example,
  [EI](`e:erl_interface:ei.md`).  In this case, you would strip the
  version number using `ei_decode_version()` and append the remaining
  local encoding using, for example, `ei_x_append_buf()`.

  A common use case for the `local` option is when making a request
  from a process to a port driver [driver](erl_driver.md) while
  leveraging the [selective receive
  optimization](`e:system:eff_guide_processes.md#receiving-messages`)
  for handling the reply.

  In this scenario:

  1. A reference is created.

  2. The reference is serialized using the external term format with
  the `local` option.

  3. This serialized reference is passed to the driver in the request.

  4. The process then waits for a reply message in a selective
  receive, matching on the reference.

  The driver should send the reply using either
  [`erl_drv_output_term()`](erl_driver.md#erl_drv_output_term) or
  [`erl_drv_send_term()`](erl_driver.md#erl_drv_send_term) using the
  term type [`ERL_DRV_EXT2TERM`](erl_driver.md#ERL_DRV_EXT2TERM)
  for the reference previously received in the request.

  Note that you should not strip the leading version number from the
  local encoding when using the term type `ERL_DRV_EXT2TERM`. If the
  reference is not encoded with the `local` option, and distribution is
  started or stopped while the request is ongoing, the requesting
  process will hang indefinitely because the reference in the reply
  message will never match.

  This option cannot be combined with the `deterministic` option.

  For more details, see [`LOCAL_EXT`](erl_ext_dist.md#local_ext).

  See also `binary_to_term/1`.

## Examples

```erlang
1> List = lists:duplicate(20, $=).
"===================="
2> term_to_binary(List, []).
<<131,107,0,20,61,61,61,61,61,61,61,61,61,61,61,61,61,61,
  61,61,61,61,61,61>>
3> term_to_binary(List, [compressed]).
<<131,80,0,0,0,23,120,156,203,102,16,177,197,2,0,61,98,5,
  68>>
```

# `term_to_iovec`
*auto-imported* *since OTP 23.0* 

```erlang
-spec term_to_iovec(Term) -> ext_iovec() when Term :: term().
```

Returns the encoding of `Term` according to the Erlang external term format as
`t:ext_iovec/0`.

This function produce the same encoding as `term_to_binary/1`, but with another
return type. The call
[`iolist_to_binary(term_to_iovec(Term))`](`iolist_to_binary/1`) will produce
exactly the same result as the call
[`term_to_binary(Term)`](`term_to_binary/1`).

`term_to_iovec/1` is a pure optimization of the functionality provided
by `term_to_binary/1`. For example, it can reference off-heap binaries
directly instead of copying their contents into the result.

See also `term_to_binary/1`.

## Examples

```erlang
1> term_to_iovec({binary:copy(~"a", 65), binary:copy(~"b", 65)}).
[<<131,104,2,109,0,0,0,65>>,
 <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
 <<109,0,0,0,65>>,
 <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>]
```

# `term_to_iovec`
*auto-imported* *since OTP 23.0* 

```erlang
-spec term_to_iovec(Term, Options) -> ext_iovec()
                       when
                           Term :: term(),
                           Options ::
                               [compressed |
                                {compressed, Level :: 0..9} |
                                deterministic |
                                {minor_version, Version :: 0..2} |
                                local].
```

Returns the encoding of `Term` according to the Erlang external term format as
`t:ext_iovec/0`.

This function produce the same encoding as `term_to_binary/2`, but with another
return type. The call
[`iolist_to_binary(term_to_iovec(Term, Opts))`](`iolist_to_binary/1`) will
produce exactly the same result as
[`term_to_binary(Term, Opts)`](`term_to_binary/2`).

This function supports all options supported by `term_to_binary/2`.

`term_to_iovec/2` is a pure optimization of the functionality provided
by `term_to_binary/2`. For example, it can reference off-heap binaries
directly instead of copying their contents into the result.

See also `term_to_binary/2`.

## Examples

```erlang
1> term_to_iovec({binary:copy(~"a", 65), binary:copy(~"b", 65)}, [deterministic]).
[<<131,104,2,109,0,0,0,65>>,
 <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
 <<109,0,0,0,65>>,
 <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>]
```

# `throw`
*auto-imported* 

```erlang
-spec throw(Any) -> no_return() when Any :: term().
```

Raises an exception of class `throw`. Intended to be used to do non-local
returns from functions.

If evaluated within a [catch expression](`e:system:expressions.md#catch-and-throw`), the
catch expression returns value `Any`.

For example:

```erlang
1> catch throw({hello, there}).
{hello,there}
```

If evaluated within a `try`\-block of a
[try expression](`e:system:expressions.md#try`), the value `Any` can be caught
within the catch block.

For example:

```erlang
1>  try
        throw({my_exception, "Something happened"})
    catch
        throw:{my_exception, Desc} ->
            io:format(standard_error, "Error: ~s~n", [Desc])
    end.
%% Output to standard error: Error: Something happened
```

If `throw/1` is not evaluated within a catch, a `nocatch` run-time error occurs.

See the guide about [errors and error handling](`e:system:errors.md`) for
additional information.

# `time`
*auto-imported* 

```erlang
-spec time() -> Time when Time :: calendar:time().
```

Returns the current time as `{Hour, Minute, Second}`.

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> time().
{9,42,44}
```

# `time_offset`
*since OTP 18.0* 

```erlang
-spec time_offset() -> integer().
```

Returns the current time offset between
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
[Erlang system time](time_correction.md#erlang-system-time) in `native`
[time unit](`t:time_unit/0`). Current time offset added to an Erlang
monotonic time gives corresponding Erlang system time.

The time offset may or may not change during operation depending on the
[time warp mode](time_correction.md#time-warp-modes) used.

> #### Note {: .info }
>
> A change in time offset can be observed at slightly different points in time
> by different processes.
>
> If the runtime system is in
> [multi-time warp mode](time_correction.md#multi-time-warp-mode), the time
> offset is changed when the runtime system detects that the
> [OS system time](time_correction.md#os-system-time) has changed. The runtime
> system will, however, not detect this immediately when it occurs. A task
> checking the time offset is scheduled to execute at least once a minute; so,
> under normal operation this is to be detected within a minute, but during
> heavy load it can take longer time.

# `time_offset`
*since OTP 18.0* 

```erlang
-spec time_offset(Unit) -> integer() when Unit :: time_unit().
```

Returns the current time offset between
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
[Erlang system time](time_correction.md#erlang-system-time) converted into the
`Unit` passed as argument.

Same as calling
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[ `erlang:time_offset()`](`time_offset/0`)`, native, Unit)`
however optimized for commonly used `Unit`s.

# `timestamp`
*since OTP 18.0* 

```erlang
-spec timestamp() -> Timestamp when Timestamp :: timestamp().
```

Returns current [Erlang system time](time_correction.md#erlang-system-time) on
the format `{MegaSecs, Secs, MicroSecs}`.

This format is the same as `os:timestamp/0` and the deprecated [`erlang:now/0`](`now/0`) use.
The reason for the existence of `erlang:timestamp()` is purely to simplify use for existing
code that assumes this time stamp format. Current Erlang system time can more
efficiently be retrieved in the time unit of your choice using
[`erlang:system_time/1`](`system_time/1`).

The `erlang:timestamp()` BIF is equivalent to:

```c
timestamp() ->
    ErlangSystemTime = erlang:system_time(microsecond),
    MegaSecs = ErlangSystemTime div 1000_000_000_000,
    Secs = ErlangSystemTime div 1000_000 - MegaSecs*1000_000,
    MicroSecs = ErlangSystemTime rem 1000_000,
    {MegaSecs, Secs, MicroSecs}.
```

It, however, uses a native implementation that does not build garbage on the
heap and with slightly better performance.

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.

# `tl`
*auto-imported* *allowed in guard tests* 

```erlang
-spec tl(List) -> Tail when List :: nonempty_maybe_improper_list(), Tail :: term().
```

Returns the tail of `List`, which is the list without its first element.

It works with improper lists.

## Examples

```erlang
1> tl([geesties, guilies, beasties]).
[guilies, beasties]
2> tl([geesties]).
[]
3> tl([geesties, guilies, beasties | improper_end]).
[guilies, beasties | improper_end]
4> tl([geesties | improper_end]).
improper_end
5> tl([]).
** exception error: bad argument
     in function  tl/1
        called as tl([])
        *** argument 1: not a nonempty list
```

Failure: `badarg` if `List` is an empty list `[]`.

# `trace`

```erlang
-spec trace(PidPortSpec, How, FlagList) -> integer()
               when
                   PidPortSpec ::
                       pid() |
                       port() |
                       all | processes | ports | existing | existing_processes | existing_ports | new |
                       new_processes | new_ports,
                   How :: boolean(),
                   FlagList :: [trace_flag()].
```

Turn on or off trace flags on processes or ports for the static legacy trace session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:process/4` and `trace:port/4` that
  > operate on dynamic trace sessions.

Argument `FlagList` can contain two additional options:

- **`{tracer, Tracer}`** - Specifies where to send the trace messages. `Tracer`
  must be the process identifier of a local process or the port identifier of a
  local port.

- **`{tracer, TracerModule, TracerState}`** - Specifies that a tracer module is
  to be called instead of sending a trace message. The tracer module can then
  ignore or change the trace message. For more details on how to write a tracer
  module, see `m:erl_tracer`.

If no `tracer` is specified, the calling process receives all the trace
messages. The legacy trace session has no specified tracer.

For further documentation see `trace:process/4` and `trace:port/4`.

# `trace_delivered`

```erlang
-spec trace_delivered(Tracee) -> Ref when Tracee :: pid() | all, Ref :: reference().
```

Calling this function makes sure all trace messages have been delivered.

The delivery of trace messages (generated by [`erlang:trace/3`](`trace/3`),
`m:seq_trace`, or [`erlang:system_profile/2`](`system_profile/2`)) is dislocated
on the time-line compared to other events in the system. If you know that
`Tracee` has passed some specific point in its execution, and you want to know
when at least all trace messages corresponding to events up to this point have
reached the tracer, use `erlang:trace_delivered(Tracee)`.

When it is guaranteed that all trace messages are delivered to the tracer up to
the point that `Tracee` reached at the time of the call to
`erlang:trace_delivered(Tracee)`, then a `{trace_delivered, Tracee, Ref}`
message is sent to the caller of `erlang:trace_delivered(Tracee)` .

Notice that message `trace_delivered` does _not_ imply that trace messages have
been delivered. Instead it implies that all trace messages that _are to be
delivered_ have been delivered. It is not an error if `Tracee` is not, and has
not been traced by someone, but if this is the case, _no_ trace messages have
been delivered when the `trace_delivered` message arrives.

Notice that `Tracee` must refer to a process currently or previously existing on
the same node as the caller of `erlang:trace_delivered(Tracee)` resides on. The
special `Tracee` atom `all` denotes all processes that currently are traced in
the node.

When used together with a [Tracer Module](`m:erl_tracer`), any message sent in
the trace callback is guaranteed to have reached its recipient before the
`trace_delivered` message is sent.

Example: Process `A` is `Tracee`, port `B` is tracer, and process `C` is the
port owner of `B`. `C` wants to close `B` when `A` exits. To ensure that the
trace is not truncated, `C` can call `erlang:trace_delivered(A)` when `A` exits,
and wait for message `{trace_delivered, A, Ref}` before closing `B`.

Failure: `badarg` if `Tracee` does not refer to a process (dead or alive) on the
same node as the caller of `erlang:trace_delivered(Tracee)` resides on.

# `trace_info`

```erlang
-spec trace_info(PidPortFuncEvent, Item) -> Res
                    when
                        PidPortFuncEvent ::
                            pid() |
                            port() |
                            new | new_processes | new_ports |
                            {Module, Function, Arity} |
                            on_load | send | 'receive',
                        Module :: module(),
                        Function :: atom(),
                        Arity :: arity(),
                        Item ::
                            flags | tracer | traced | match_spec | meta | meta_match_spec | call_count |
                            call_time | call_memory | all,
                        Res :: trace_info_return().
```

Returns trace information about a port, process, function, or event for the
static legacy trace session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:info/3` that operates on dynamic trace
  > sessions.

# `trace_pattern`

```erlang
-spec trace_pattern(MFA, MatchSpec) -> non_neg_integer()
                       when
                           MFA :: trace_pattern_mfa() | send | 'receive',
                           MatchSpec ::
                               (MatchSpecList :: trace_match_spec()) | boolean() | restart | pause.
```

Equivalent to [`erlang:trace_pattern(Event, MatchSpec, [])`](`trace_pattern/3`),
retained for backward compatibility.

# `trace_pattern`

```erlang
-spec trace_pattern(send, MatchSpec, []) -> non_neg_integer()
                       when MatchSpec :: (MatchSpecList :: trace_match_spec()) | boolean();
                   ('receive', MatchSpec, []) -> non_neg_integer()
                       when MatchSpec :: (MatchSpecList :: trace_match_spec()) | boolean();
                   (MFA, MatchSpec, FlagList) -> non_neg_integer()
                       when
                           MFA :: trace_pattern_mfa(),
                           MatchSpec ::
                               (MatchSpecList :: trace_match_spec()) | boolean() | restart | pause,
                           FlagList :: [trace_pattern_flag()].
```

Set trace pattern for call, send and receive tracing on the static legacy trace
session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:function/4`, `trace:send/3` and
  > `trace:recv/3` that operate on dynamic trace sessions.

Argument `FlagList` can contain two additional options for call tracing:

- **`{meta, Pid} | {meta, TracerModule, TracerState}`** - Turns on or off
  meta-tracing for all types of function calls. Trace messages are sent to the
  tracer whenever any of the specified functions are called. If no tracer is
  specified, `self/0` is used as a default tracer process.

For further documentation see `trace:function/4` , `trace:send/3` and
`trace:recv/3`.

# `trunc`
*auto-imported* *allowed in guard tests* 

```erlang
-spec trunc(Number) -> integer() when Number :: number().
```

Truncates the decimals of `Number`.

See also `round/1`, `floor/1`, and `ceil/1`.

## Examples

```erlang
1> trunc(5.7).
5
2> trunc(-5.7).
-5
3> trunc(5).
5
4> trunc(36028797018963969.0).
36028797018963968
```

In the last example, [`trunc(36028797018963969.0)`](`trunc/1`)
evaluates to `36028797018963968`. This happens because the number
`36028797018963969.0` cannot be represented exactly as a
floating-point value. Instead, it is represented as
`36028797018963968.0`, which is the closest representable
floating-point value. See [Representation of Floating Point
Numbers](`e:system:data_types.md#float_representation_problem`) for
additional information.

# `tuple_size`
*auto-imported* *allowed in guard tests* 

```erlang
-spec tuple_size(Tuple) -> non_neg_integer() when Tuple :: tuple().
```

Returns the number of elements in `Tuple`.

## Examples

```erlang
1> tuple_size({a, b, c}).
3
```

# `tuple_to_list`
*auto-imported* 

```erlang
-spec tuple_to_list(Tuple) -> [term()] when Tuple :: tuple().
```

Returns a list corresponding to `Tuple`.

## Examples

```erlang
1> tuple_to_list({share, {'Ericsson_B', 163}}).
[share,{'Ericsson_B',163}]
```

# `unalias`
*auto-imported* *since OTP 24.0* 

```erlang
-spec unalias(Alias) -> boolean() when Alias :: reference().
```

Deactivate the alias `Alias` previously created by the calling process.

An alias can, for example, be created via `alias/0` or `monitor/3`.
[`unalias/1`](`unalias/1`) will always deactivate the alias regardless of
options used when creating the alias.

Returns true if `Alias` was a currently active alias for current processes;
otherwise, false.

For more information on process aliases see the
[_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section of
the _Erlang Reference Manual_.

# `unique_integer`
*since OTP 18.0* 

```erlang
-spec unique_integer() -> integer().
```

Generates and returns an
[integer unique on current runtime system instance](`e:system:system_limits.md#unique_integers`).

Equivalent to calling [`erlang:unique_integer([])`](`unique_integer/1`).

## Examples

```erlang
> erlang:unique_integer().
-576460752303422335
```

# `unique_integer`
*since OTP 18.0* 

```erlang
-spec unique_integer(ModifierList) -> integer()
                        when ModifierList :: [Modifier], Modifier :: positive | monotonic.
```

Generates and returns an
[integer unique on current runtime system instance](`e:system:system_limits.md#unique_integers`).

The integer is unique in the sense that this BIF, using the same set of
modifiers, does not return the same integer more than once on the current
runtime system instance. Each integer value can of course be constructed by
other means.

By default, when `[]` is passed as `ModifierList`, both negative and positive
integers can be returned. This to use the range of integers that do not need
heap memory allocation as much as possible. By default the returned integers are
also only guaranteed to be unique, that is, any returned integer can be smaller
or larger than previously returned integers.

`Modifier`s:

- **positive** - Returns only positive integers.

  Notice that by passing the `positive` modifier you will get heap allocated
  integers (bignums) quicker.

- **monotonic** - Returns
  [strictly monotonically increasing](time_correction.md#strictly-monotonically-increasing)
  integers corresponding to creation time. That is, the integer returned is
  always larger than previously returned integers on the current runtime system
  instance.

  These values can be used to determine order between events on the runtime
  system instance. That is, if both `X = erlang:unique_integer([monotonic])` and
  `Y = erlang:unique_integer([monotonic])` are executed by different processes
  (or the same process) on the same runtime system instance and `X < Y`, we know
  that `X` was created before `Y`.

  > #### Warning {: .warning }
  >
  > Strictly monotonically increasing values are inherently quite expensive to
  > generate and scales poorly. This is because the values need to be
  > synchronized between CPU cores. That is, do not pass the `monotonic`
  > modifier unless you really need strictly monotonically increasing values.

All valid `Modifier`s can be combined. Repeated (valid) `Modifier`s in the
`ModifierList` are ignored.

> #### Note {: .info }
>
> The set of integers returned by `erlang:unique_integer/1` using different sets
> of `Modifier`s _will overlap_. For example, by calling
> [`unique_integer([monotonic])`](`unique_integer/1`), and
> [`unique_integer([positive, monotonic])`](`unique_integer/1`) repeatedly, you
> will eventually see some integers that are returned by both calls.

Failures:

- **`badarg`** - if `ModifierList` is not a proper list.

- **`badarg`** - if `Modifier` is not a valid modifier.

## Examples

```erlang
> erlang:unique_integer([positive]).
1186
```

# `universaltime`

```erlang
-spec universaltime() -> DateTime when DateTime :: calendar:datetime().
```

Returns the current date and time according to Universal Time Coordinated (UTC)
in the form `{{Year, Month, Day}, {Hour, Minute, Second}}` if supported by the
underlying OS. Otherwise `erlang:universaltime()` is equivalent to
`erlang:localtime()`. The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> erlang:universaltime().
{{1996,11,6},{14,18,43}}
```

# `universaltime_to_localtime`

```erlang
-spec universaltime_to_localtime(Universaltime) -> Localtime
                                    when
                                        Localtime :: calendar:datetime(),
                                        Universaltime :: calendar:datetime().
```

Converts Universal Time Coordinated (UTC) date and time to local date and time
in the form `{{Year, Month, Day}, {Hour, Minute, Second}}` if supported by the
underlying OS. Otherwise no conversion is done, and `Universaltime` is returned.

For example:

```erlang
> erlang:universaltime_to_localtime({{1996,11,6},{14,18,43}}).
{{1996,11,7},{15,18,43}}
```

Failure: `badarg` if `Universaltime` denotes an invalid date and time.

# `unlink`
*auto-imported* 

```erlang
-spec unlink(Id) -> true when Id :: pid() | port().
```

Removes a link between the calling process and another process or a port
identified by `Id`.

We will from here on call the identified process or port unlinkee.

A link can be set up using the `link/1` BIF. For more information on links and
exit signals due to links, see the _Processes_ chapter in the _Erlang Reference
Manual_:

- [Links](`e:system:ref_man_processes.md#links`)
- [Sending Exit Signals](`e:system:ref_man_processes.md#sending_exit_signals`)
- [Receiving Exit Signals](`e:system:ref_man_processes.md#receiving_exit_signals`)

Once [`unlink(Id)`](`unlink/1`) has returned, it is guaranteed that the link
between the caller and the unlinkee has no effect on the caller in the future
(unless the link is setup again). Note that if the caller is
[trapping exits](#process_flag_trap_exit), an
`{'EXIT', Id, ExitReason}` message due to the link may have been placed in the
message queue of the caller before the [`unlink(Id)`](`unlink/1`) call
completed. Also note that the `{'EXIT', Id, ExitReason}` message may be the
result of the link, but may also be the result of the unlikee sending the caller
an exit signal by calling the `exit_signal/2` BIF. Therefore, it may or may not be
appropriate to clean up the message queue after a call to
[`unlink(Id)`](`unlink/1`) as follows, when trapping exits:

```erlang
unlink(Id),
receive
    {'EXIT', Id, _} ->
        true
after 0 ->
        true
end
```

The link removal is performed asynchronously. If such a link does not exist,
nothing is done. A detailed description of the
[link protocol](erl_dist_protocol.md#link_protocol) can be found in the
_Distribution Protocol_ chapter of the _ERTS User's Guide_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

Failure: `badarg` if `Id` does not identify a process or a node local port.

# `unregister`
*auto-imported* 

```erlang
-spec unregister(RegName) -> true when RegName :: atom().
```

Removes the [`registered name`](`register/2`) `RegName` associated with a
process identifier or a port identifier from the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`).

For example:

```erlang
> unregister(db).
true
```

Keep in mind that you can still receive signals associated with the registered
name after it has been unregistered as the sender may have looked up the name
before sending to it.

Users are advised not to unregister system processes.

Failure: `badarg` if `RegName` is not a registered name.

# `whereis`
*auto-imported* 

```erlang
-spec whereis(RegName) -> pid() | port() | undefined when RegName :: atom().
```

Returns the process identifier or port identifier with the
[`registered name`](`register/2`) `RegName` from the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`). Returns
`undefined` if the name is not registered.

For example:

```erlang
> whereis(db).
<0.43.0>
```

# `yield`

```erlang
-spec yield() -> true.
```

Tries to give other processes with the same or higher priority (if any) a chance
to execute before returning. There is no guarantee that any other process runs
between the invocation and return of `erlang:yield/0`.

See the documentation for
[`receive-after` expressions](`e:system:expressions.md#receive`) for how to make
the current process sleep for a specific number of milliseconds.

> #### Warning {: .warning }
>
> There is seldom or never any need to use this BIF. Using this BIF without a
> thorough grasp of how the scheduler works can cause performance degradation.
> The current implementation of this function puts the current process last in
> the current scheduler's queue for processes of the same priority as the
> current process.

# `exit`
*auto-imported* 

```erlang
-spec exit(Dest, Reason) -> true when Dest :: pid() | port() | reference(), Reason :: term().
```

Old form of `exit_signal/2`, with a quirk when sender and receiver are the same.

> #### Note {: .info }
>
> The function [`erlang:exit/2`](`exit/2`) is named similarly to [`erlang:exit/1`](`exit/1`)
> but provides very different functionality. The `erlang:exit/1` function should be used
> when the intent is to stop the current process by raising an exception of class `exit`.

> The `erlang:exit_signal/2` function, or the old form `erlang:exit/2`, should be used
> when the intent is to send an exit signal to another process. Note also that
> `erlang:exit/1` raises an exception that can be caught, while `erlang:exit_signal/2`
> does not cause any exception to be raised.

> #### Warning {: .warning }
>
> This function has a quirk: When a process `P` sends an exit signal with reason `normal`
> to itself using this function, that is, `erlang:exit(self(), normal)`, the behavior is
> as follows:
>
> - `P` exits with reason `normal` if `P` is not trapping exits.
> - If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
>   into a message `{'EXIT', From, normal}`, where `From` is `P`'s process
>   identifier, and delivered to `P`'s message queue.
>
> Note that this differs from when a process sends an exit signal with reason `normal`
> to another process than itself (see `exit_signal/2` for details). This behavior is kept
> for backward compatibility reasons. Use `exit_signal/2` for new code.

# `exit`
*auto-imported* *since OTP 28.0* 

```erlang
-spec exit(Dest, Reason, OptList) -> true
              when Dest :: pid() | port() | reference(), Reason :: term(), OptList :: [priority].
```

Deprecated form of `exit_signal/3`, with a quirk when sender and receiver are the same.

This allows passing options to `exit/2`, which preserves the behavior when a process `P`
sends an exit signal with reason `normal` to itself; see `exit/2` for details. For new
code, use `exit_signal/3`.

# `now`
*auto-imported* 

> This function is deprecated. erlang:now/0 is deprecated; see the &quot;Time and Time Correction in Erlang&quot; chapter of the ERTS User's Guide for more information.

```erlang
-spec now() -> Timestamp when Timestamp :: timestamp().
```

> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>
> For more information, see section
> [Time and Time Correction](time_correction.md) in the User's Guide.
> Specifically, section [Dos and Dont's](time_correction.md#Dos_and_Donts)
> describes what to use instead of `erlang:now/0`.

Returns the tuple `{MegaSecs, Secs, MicroSecs}`, which is the elapsed time since
00:00 GMT, January 1, 1970 (zero hour), if provided by the underlying OS.
Otherwise some other point in time is chosen. It is also guaranteed that the
following calls to this BIF return continuously increasing values. Hence, the
return value from `erlang:now/0` can be used to generate unique time stamps. If
it is called in a tight loop on a fast machine, the time of the node can become
skewed.

Can only be used to check the local time of day if the time-zone information of
the underlying OS is properly configured.

# `phash`

> This function is deprecated. erlang:phash/2 is deprecated; use erlang:phash2/2 instead.

```erlang
-spec phash(Term, Range) -> Hash when Term :: term(), Range :: pos_integer(), Hash :: pos_integer().
```

> #### Warning {: .warning }
>
> This function is deprecated as [`erlang:phash2/2`](`phash2/2`) should be used
> for new code. Note that `erlang:phash(X,N)` is not necessary equal to
> `erlang:phash2(X,N)`

Portable hash function that gives the same hash for the same Erlang term
regardless of machine architecture and ERTS version (the BIF was introduced in
ERTS 4.9.1.1). The function returns a hash value for `Term` within the range
`1..Range`. The maximum value for `Range` is 2^32.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
