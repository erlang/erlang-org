# `et_collector`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/et/src/et_collector.erl#L27)

Collect trace events and provide a backing storage appropriate for iteration

Interface module for the Event Trace (ET) application

# `actor`
*not exported* 

```erlang
-type actor() :: term().
```

# `collector_fun`
*not exported* 

```erlang
-type collector_fun() :: trace_filter_fun() | event_filter_fun().
```

# `dbg_match_spec`
*not exported* 

```erlang
-type dbg_match_spec() :: [tuple()].
```

# `dbg_trace_parameters`
*not exported* 

```erlang
-type dbg_trace_parameters() :: file:filename() | integer() | {string(), integer()}.
```

# `dbg_trace_type`
*not exported* 

```erlang
-type dbg_trace_type() :: ip | file | follow_file.
```

# `ets_match_object_pattern`
*not exported* 

```erlang
-type ets_match_object_pattern() :: term().
```

# `event`
*not exported* 

```erlang
-type event() ::
          #event{detail_level :: term(),
                 trace_ts :: term(),
                 event_ts :: term(),
                 from :: term(),
                 to :: term(),
                 label :: term(),
                 contents :: term()}.
```

# `event_filter_fun`
*not exported* 

```erlang
-type event_filter_fun() :: fun((Event :: event()) -> boolean() | {true, event()}).
```

# `event_ts`
*not exported* 

```erlang
-type event_ts() :: #event_ts{event_ts :: term(), trace_ts :: term()}.
```

# `level`
*not exported* 

```erlang
-type level() :: 0..100.
```

# `option`

```erlang
-type option() ::
          {parent_pid, pid() | undefined} |
          {event_order, trace_ts | event_ts} |
          {dict_insert, {filter, all}, collector_fun()} |
          {dict_insert, {filter, EventFilterName :: atom()}, event_filter_fun()} |
          {dict_insert, {subscriber, pid()}, Val :: term()} |
          {dict_insert, Key :: term(), Val :: term()} |
          {dict_delete, Key :: term()} |
          {trace_client, {event_file, file:filename()} | {dbg_trace_type(), dbg_trace_parameters()}} |
          {trace_global, boolean()} |
          {trace_pattern, {module() | undefined, Level :: level() | dbg_match_spec()} | undefined} |
          {trace_port, integer()} |
          {trace_max_queue, integer()}.
```

# `table_handle`
*not exported* 

```erlang
-type table_handle() ::
          #table_handle{collector_pid :: term(),
                        event_tab :: term(),
                        event_order :: term(),
                        filter :: term()}.
```

# `trace_filter_fun`
*not exported* 

```erlang
-type trace_filter_fun() :: fun((TraceData :: tuple()) -> boolean() | {true, event()}).
```

# `trace_ts`
*not exported* 

```erlang
-type trace_ts() :: #trace_ts{trace_ts :: term(), event_ts :: term()}.
```

# `change_pattern`

```erlang
-spec change_pattern(CollectorPid, RawPattern) -> {old_pattern, TracePattern}
                        when
                            CollectorPid :: pid(),
                            RawPattern :: {module(), min | max | level()},
                            TracePattern :: [{[term()] | '_' | atom(), [term()], [term()]}].
```

Change active trace pattern globally on all trace nodes.

# `clear_table`

```erlang
-spec clear_table(Handle) -> ok when Handle :: CollectorPid | table_handle(), CollectorPid :: pid().
```

Clear the event table.

# `dict_delete`

```erlang
-spec dict_delete(CollectorPid :: pid(), Key :: term()) -> ok.
```

Delete a dictionary entry and send a \{et, \{dict_delete, Key\}\} tuple to all
registered subscribers.

If the deleted entry is a registered subscriber, it will imply that the
subscriber process gets is unregistered as subscriber as well as it gets it
final message.

# `dict_insert`

```erlang
-spec dict_insert(CollectorPid, What, Value) -> ok
                     when
                         CollectorPid :: pid(),
                         What :: {filter, atom()} | {subscriber, pid()} | Key,
                         Key :: term(),
                         Value :: collector_fun() | term().
```

Insert a dictionary entry and send a `{et, {dict_insert, Key, Val}}` tuple to
all registered subscribers.

If the entry is a new subscriber, it will imply that the new subscriber process
first will get one message for each already stored dictionary entry, before it
and all old subscribers will get this particular entry. The collector process
links to and then supervises the subscriber process. If the subscriber process
dies it will imply that it gets unregistered as with a normal dict_delete/2.

# `dict_lookup`

```erlang
-spec dict_lookup(CollectorPid :: pid(), Key :: term()) -> [Val :: term()].
```

Lookup a dictionary entry and return zero or one value.

# `dict_match`

```erlang
-spec dict_match(CollectorPid :: pid(), {KeyPattern, ValPattern}) -> [Match]
                    when
                        KeyPattern :: ets_match_object_pattern(),
                        ValPattern :: ets_match_object_pattern(),
                        Match :: {Key :: term(), Val :: term()}.
```

Match some dictionary entries

# `get_global_pid`

```erlang
-spec get_global_pid() -> CollectorPid :: pid().
```

Return a the identity of the globally registered collector if there is any.

# `iterate`

```erlang
-spec iterate(Handle, Prev, Limit) -> NewAcc
                 when
                     Handle :: CollectorPid | table_handle(),
                     CollectorPid :: pid(),
                     Prev :: first | last | term(),
                     Limit :: Done | Forward | Backward,
                     Done :: 0,
                     Forward :: pos_integer(),
                     Backward :: neg_integer(),
                     NewAcc :: term().
```

# `iterate`

```erlang
-spec iterate(Handle, Prev, Limit, Fun, Acc) -> NewAcc
                 when
                     Handle :: CollectorPid | table_handle(),
                     CollectorPid :: pid(),
                     Prev :: first | last | term(),
                     Limit :: Done | Forward | Backward,
                     Done :: 0,
                     Forward :: pos_integer() | infinity,
                     Backward :: neg_integer() | '-infinity',
                     Fun :: fun((event(), Acc) -> NewAcc) | undefined,
                     Acc :: term(),
                     NewAcc :: term().
```

Iterate over the currently stored events.

Iterates over the currently stored events and applies a function for each event.
The iteration may be performed forwards or backwards and may be limited to a
maximum number of events (abs(Limit)).

# `make_key`

```erlang
-spec make_key(Handle, Stuff) -> Key
                  when
                      Handle :: table_handle() | trace_ts | event_ts,
                      Stuff :: event() | Key,
                      Key :: event_ts() | trace_ts().
```

Make a key out of an event record or an old key.

# `multicast`

```erlang
-spec multicast(CollectorPid :: pid(), Msg :: term()) -> ok.
```

Sends a message to all registered subscribers.

# `report`

```erlang
-spec report(Handle, TraceOrEvent) -> {ok, Continuation}
                when
                    Handle :: table_handle() | (CollectorPid :: pid()),
                    TraceOrEvent :: event() | TraceData | end_of_trace,
                    TraceData :: tuple(),
                    Continuation :: table_handle().
```

# `report_event`

```erlang
-spec report_event(Handle, DetailLevel, FromTo, Label, Contents) -> {ok, Continuation}
                      when
                          Handle :: CollectorPid :: pid() | table_handle(),
                          DetailLevel :: level(),
                          FromTo :: actor(),
                          Label :: term(),
                          Contents :: [{Key :: term(), Value :: term()}] | term(),
                          Continuation :: table_handle().
```

# `report_event`

```erlang
-spec report_event(Handle, DetailLevel, From, To, Label, Contents) -> {ok, Continuation}
                      when
                          Handle :: CollectorPid :: pid() | table_handle(),
                          DetailLevel :: level(),
                          From :: actor(),
                          To :: actor(),
                          Label :: term(),
                          Contents :: [{Key :: term(), Value :: term()}] | term(),
                          Continuation :: table_handle().
```

Report an event to the collector.

All events are filtered thru the collector filter, which optionally may
transform or discard the event. The first call should use the pid of the
collector process as report handle, while subsequent calls should use the table
handle.

# `save_event_file`

```erlang
-spec save_event_file(CollectorPid, FileName, [Option]) -> ok | {error, term()}
                         when
                             CollectorPid :: pid(),
                             FileName :: file:filename(),
                             Option :: existing | write | append | keep | clear.
```

Save the events to a file.

By default the currently stored events (existing) are written to a brand new
file (write) and the events are kept (keep) after they have been written to the
file.

Instead of keeping the events after writing them to file, it is possible to
remove all stored events after they have successfully written to file (clear).

The options defaults to existing, write and keep.

# `start_link`

```erlang
-spec start_link(Options) -> {ok, Pid :: pid()} | {error, term()} when Options :: [option()].
```

Start a collector process.

The collector collects trace events and keeps them ordered by their timestamp.
The timestamp may either reflect the time when the actual trace data was
generated (trace_ts) or when the trace data was transformed into an event record
(event_ts). If the time stamp is missing in the trace data (missing timestamp
option to erlang:trace/4) the trace_ts will be set to the event_ts.

Events are reported to the collector directly with the report function or
indirectly via one or more trace clients. All reported events are first filtered
thru the collector filter before they are stored by the collector. By replacing
the default collector filter with a customized dito it is possible to allow any
trace data as input. The collector filter is a dictionary entry with the
predefined key \{filter, collector\} and the value is a fun of arity 1. See
et_selector:make_event/1 for interface details, such as which erlang:trace/1
tuples that are accepted.

The collector has a built-in dictionary service. Any term may be stored as value
in the dictionary and bound to a unique key. When new values are inserted with
an existing key, the new values will overwrite the existing ones. Processes may
subscribe on dictionary updates by using \{subscriber, pid()\} as dictionary
key. All dictionary updates will be propagated to the subscriber processes
matching the pattern \{\{subscriber, '_'\}, '_'\} where the first '\_' is
interpreted as a pid().

In global trace mode, the collector will automatically start tracing on all
connected Erlang nodes. When a node connects, a port tracer will be started on
that node and a corresponding trace client on the collector node.

Default values:

- parent_pid - self().
- event_order - trace_ts.
- trace_global - false.
- trace_pattern - undefined.
- trace_port - 4711.
- trace_max_queue - 50.

# `start_trace_client`

```erlang
-spec start_trace_client(CollectorPid, Type, Parameter) -> file_loaded | {trace_client_pid, pid()}
                            when
                                CollectorPid :: pid(),
                                Type :: event_file | dbg_trace_type(),
                                Parameter :: dbg_trace_parameters().
```

Load raw Erlang trace from a file, port or process.

# `stop`

```erlang
-spec stop(pid()) -> ok.
```

Stop a collector process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
