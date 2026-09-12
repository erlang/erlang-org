# `et_viewer`
[🔗](https://github.com/erlang/otp/blob/master/lib/et/src/et_viewer.erl#L26)

Displays a sequence chart for trace events (messages/actions)

# `actors`
*not exported* 

```erlang
-type actors() :: [term()].
```

# `first_key`
*not exported* 

```erlang
-type first_key() :: term().
```

# `option`
*not exported* 

```erlang
-type option() ::
          {title, string()} |
          {detail_level, 0..100} |
          {is_suspended, boolean()} |
          {scale, integer()} |
          {width, integer()} |
          {height, integer()} |
          {collector_pid, pid() | undefined} |
          {active_filter, atom()} |
          {max_events, integer() | undefined} |
          {max_actors, integer() | undefined} |
          {actors, actors()} |
          {first_event, first_key()} |
          {hide_unknown, boolean()} |
          {hide_actions, boolean()} |
          {display_mode, all | {search_actors, forward | reverse, first_key(), actors()}}.
```

# `file`

```erlang
-spec file(FileName :: file:filename()) -> {ok, pid()} | {error, term()}.
```

Start a new event viewer and a corresponding collector and load them with trace
events from a trace file.

# `get_collector_pid`

```erlang
-spec get_collector_pid(ViewerPid :: pid()) -> pid().
```

Returns the identifier of the collector process.

# `start`

```erlang
-spec start() -> {ok, pid()} | {error, term()}.
```

Simplified start of a sequence chart viewer with global tracing activated.

Convenient to be used from the command line (erl -s et_viewer).

# `start`

```erlang
-spec start(GUIorOptions) -> {ok, Viewer :: pid()} | {error, term()}
               when
                   GUIorOptions :: wx | default | Options, Options :: [option() | et_collector:option()].
```

Start of a sequence chart viewer without linking to the parent process.

# `start_link`

```erlang
-spec start_link(GUIorOptions) -> {ok, Viewer :: pid()} | {error, term()}
                    when
                        GUIorOptions :: wx | default | Options,
                        Options :: [option() | et_collector:option()].
```

Start a sequence chart viewer for trace events (messages/actions)

A filter_fun() takes an event record as sole argument and returns
`false | true | {true, NewEvent}`.

If the `collector_pid` is `undefined` a new `et_collector` will be started with
the following parameter settings: `parent_pid`, `event_order`, `trace_global`,
`trace_pattern`, `trace_port`, `trace_max_queue`, `trace_client`, `dict_insert`
and `dict_delete`. The new `et_viewer` will register itself as an `et_collector`
subscriber.

Default values:

- parent_pid - self().
- title - "et_viewer".
- detail_level - max.
- is_suspended - false.
- scale - 2.
- width - 800.
- height - 600.
- collector_pid - undefined.
- event_order - trace_ts.
- active_filter - collector.
- max_actors - 5.
- actors - \["UNKNOWN"].
- first_event - first.
- hide_unknown - false.
- hide_actions - false.
- display_mode - all.

# `stop`

```erlang
-spec stop(ViewerPid :: pid()) -> ok.
```

Stops a viewer process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
