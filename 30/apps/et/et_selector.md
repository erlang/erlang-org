# `et_selector`
[🔗](https://github.com/erlang/otp/blob/master/lib/et/src/et_selector.erl#L26)

Define event transforms and trace patterns

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

# `level`
*not exported* 

```erlang
-type level() :: 0..100.
```

# `change_pattern`

```erlang
-spec change_pattern({Mod :: module(), Pattern}) -> ok
                        when
                            Pattern :: DetailLevel | TracePattern | EmptyTracePattern,
                            DetailLevel :: level(),
                            TracePattern :: [{[term()] | '_' | atom(), [term()], [term()]}],
                            EmptyTracePattern :: [].
```

Activates/deactivates tracing by changing the current trace pattern.

`min` detail level deactivates tracing of calls to `et:trace_me/4,5`

`max` detail level activates tracing of all calls to `et:trace_me/4,5`

`integer(X)` detail level activates tracing of all calls to `et:trace_me/4,5`
whose detail level argument is lesser than `X`.

An empty match spec deactivates tracing of calls to `et:trace_me/4,5`

Other match specs activates tracing of calls to `et:trace_me/4,5` accordingly
with `erlang:trace_pattern/2`.

# `make_pattern`

```erlang
-spec make_pattern({Mod :: module(), RawPattern}) -> {Mod :: module(), TracePattern}
                      when
                          RawPattern :: level(),
                          TracePattern :: [{[term()] | '_' | atom(), [term()], [term()]}].
```

Makes a trace pattern suitable to feed change_pattern/1

Min detail level deactivates tracing of calls to `et:trace_me/4,5`

Max detail level activates tracing of all calls to `et:trace_me/4,5`

integer(X) detail level activates tracing of all calls to `et:trace_me/4,5`
whose detail level argument is lesser than X.

See also `erlang:trace_pattern/2` for more info about its `match_spec()`

# `parse_event`

```erlang
-spec parse_event(Mod, ValidTraceData) -> boolean | {true, event()}
                     when
                         Mod :: module(),
                         ValidTraceData :: ErlangTraceData | event(),
                         ErlangTraceData ::
                             {trace, pid(), atom(), term()} |
                             {trace, pid(), atom(), term(), term()} |
                             {trace_ts, pid(), atom(), term(), TS :: {integer(), integer(), integer()}} |
                             {trace_ts,
                              pid(),
                              atom(),
                              term(),
                              term(),
                              TS :: {integer(), integer(), integer()}} |
                             {seq_trace, atom(), term()} |
                             {seq_trace, atom(), term(), TS :: {integer(), integer(), integer()}} |
                             {drop, integer()}.
```

Transforms trace data and makes an event record out of it.

See `erlang:trace/3` for more info about the semantics of the trace data.

An event record consists of the following fields:

- **_detail_level_** - Noise has a high level as opposed to essentials.

- **_trace_ts_** - Time when the trace was generated. Same as event_ts if
  omitted in trace data.

- **_event_ts_** - Time when the event record was created.

- **_from_** - From actor, such as sender of a message.

- **_to_** - To actor, such as receiver of message.

- **_label_** - Label intended to provide a brief event summary.

- **_contents_** - All nitty gritty details of the event.

See `et:trace_me/4`and `et:trace_me/5` for details.

Returns:

- **_\{true, Event\}_** - where Event is an #event\{\} record representing the
  trace data

- **_true_** - means that the trace data already is an event record and that it
  is valid as it is. No transformation is needed.

- **_false_** - means that the trace data is uninteresting and should be dropped

---

*Consult [api-reference.md](api-reference.md) for complete listing*
