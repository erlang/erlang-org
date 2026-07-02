# `ttb`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/observer/src/ttb.erl#L22)

A base for building trace tools for distributed systems.

The Trace Tool Builder, `ttb`, is a base for building trace tools for
distributed systems.

When using `ttb`, do not use module `dbg` in application Runtime_Tools in
parallel.

# `format_fun`
*not exported* 

```elixir
-type format_fun() ::
          fun((Fd :: standard_io | file:fd(),
               Trace :: tuple(),
               TraceInfo :: [{atom(), list()}],
               State :: term()) ->
                  NewState :: term()).
```

# `format_handler`
*not exported* 

```elixir
-type format_handler() :: {format_fun(), InitialState :: term()}.
```

# `format_opt`
*not exported* 

```elixir
-type format_opt() :: {out, standard_io | file:filename()} | {handler, format_handler()} | disable_sort.
```

# `format_opts`
*not exported* 

```elixir
-type format_opts() :: format_opt() | [format_opt()].
```

# `item`
*not exported* 

```elixir
-type item() ::
          pid() |
          port() |
          atom() |
          {global, term()} |
          all | processes | ports | existing | existing_processes | existing_ports | new |
          new_processes | new_ports.
```

# `match_desc`
*not exported* 

```elixir
-type match_desc() :: [{matched, node(), integer()} | {matched, node(), 0, term()} | {saved, integer()}].
```

# `match_spec`
*not exported* 

```elixir
-type match_spec() :: pos_integer() | x | c | cx | [] | dbg:match_spec().
```

# `mfas`
*not exported* 

```elixir
-type mfas() :: {Module :: atom(), Function :: atom(), [term()]}.
```

# `nodes`
*not exported* 

```elixir
-type nodes() :: node() | [node()] | all.
```

# `stop_opt`
*not exported* 

```elixir
-type stop_opt() ::
          nofetch | {fetch_dir, file:filename()} | format | {format, format_opts()} | return_fetch_dir.
```

# `stop_opts`
*not exported* 

```elixir
-type stop_opts() :: stop_opt() | [stop_opt()].
```

# `tp_arity`
*not exported* 

```elixir
-type tp_arity() :: arity() | '_'.
```

# `tp_function`
*not exported* 

```elixir
-type tp_function() :: atom() | '_'.
```

# `tp_module`
*not exported* 

```elixir
-type tp_module() :: module() | '_'.
```

# `trace_flag`
*not exported* 

```elixir
-type trace_flag() ::
          s | r | m | c | p | sos | sol | sofs | all | clear | send | 'receive' | procs | ports | call |
          arity | return_to | silent | running | exiting | running_procs | running_ports |
          garbage_collection | timestamp | cpu_timestamp | monotonic_timestamp |
          strict_monotonic_timestamp | set_on_spawn | set_on_first_spawn | set_on_link |
          set_on_first_link |
          {tracer, pid() | port()} |
          {tracer, module(), term()}.
```

# `ctp`

```elixir
-spec ctp() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctp`

```elixir
-spec ctp(Module | {Module, Function, Arity}) -> {ok, MatchDesc :: match_desc()} | {error, term()}
             when Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity().
```

# `ctp`

```elixir
-spec ctp(Module :: tp_module(), Function :: tp_function()) ->
             {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctp`

```elixir
-spec ctp(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
             {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpe`
*since OTP 19.0* 

```elixir
-spec ctpe(Event) -> {ok, MatchDesc} | {error, term()}
              when
                  Event :: send | 'receive',
                  MatchDesc :: [MatchNum],
                  MatchNum :: {matched, node(), 1} | {matched, node(), 0, RPCError :: term()}.
```

# `ctpg`

```elixir
-spec ctpg() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpg`

```elixir
-spec ctpg(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
              {ok, MatchDesc :: term()} | {error, term()}
              when Module :: tp_module().
```

# `ctpg`

```elixir
-spec ctpg(Module :: tp_module(), Function :: tp_function()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpg`

```elixir
-spec ctpg(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```elixir
-spec ctpl() -> {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```elixir
-spec ctpl(Module | {Module, Function :: tp_function(), Arity :: tp_arity()}) ->
              {ok, MatchDesc :: term()} | {error, term()}
              when Module :: tp_module().
```

# `ctpl`

```elixir
-spec ctpl(Module :: tp_module(), Function :: tp_function()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `ctpl`

```elixir
-spec ctpl(Module :: tp_module(), Function :: tp_function(), Arity :: tp_arity()) ->
              {ok, MatchDesc :: match_desc()} | {error, term()}.
```

# `format`

```elixir
-spec format(Files) -> ok | {error, term()} when Files :: [file:filename()] | file:filename().
```

# `format`

```elixir
-spec format(Files, Options) -> ok | {error, term()}
                when Files :: [file:filename()] | file:filename(), Options :: format_opts().
```

Reads the specified binary trace log(s). The logs are processed in the order of
their time stamps as long as option `disable_sort` is not specified.

If `FormatHandler = {Function,InitialState}`, `Function` is called for each
trace message.

If `FormatHandler = get_et_handler()`, `et_viewer` in application ET is used for
presenting the trace log graphically. `ttb` provides a few different filters
that can be selected from menu _Filters and scaling_ in the `et_viewer`.

If `FormatHandler` is not specified, a default handler is used presenting each
trace message as a text line.

The state returned from each call of `Function` is passed to the next call, even
if the next call is to format a message from another log file.

If `Out` is specified, `FormatHandler` gets the file descriptor to `Out` as the
first parameter.

`Out` is ignored if the `et` format handler is used.

Wrap logs can be formatted one by one or all at once. To format one of the wrap
logs in a set, specify the exact file name. To format the whole set of wrap
logs, specify the name with `*` instead of the wrap count. For examples, see the
[`User's Guide`](ttb_ug.md#format).

# `get_et_handler`
*since OTP R15B* 

```elixir
-spec get_et_handler() -> {Fun, InitState} when Fun :: fun(), InitState :: term().
```

Returns the `et` handler, which can be used with [`format/2`](`format/2`) or
[`tracer/2`](`tracer/2`).

Example: `ttb:format(Dir, [{handler, ttb:get_et_handler()}])`.

# `list_config`

```elixir
-spec list_config(ConfigFile) -> Result
                     when
                         ConfigFile :: file:filename(),
                         Result :: Config | {error, term()},
                         Config :: [{integer(), mfas()}].
```

Lists all entries in the specified configuration file.

# `list_history`

```elixir
-spec list_history() -> History | {error, term()}
                      when History :: [{N :: integer(), Func :: function(), Args :: integer()}].
```

All calls to `ttb` is stored in the history. This function returns the current
content of the history. Any entry can be reexecuted with
[`run_history/1`](`run_history/1`) or stored in a configuration file with
`write_config/2,3`.

# `p`

```elixir
-spec p(Item, Flags) -> Result
           when
               Item :: item(),
               Flags :: trace_flag() | [trace_flag()],
               Result :: {ok, [{item(), match_desc()}]}.
```

Sets the specified trace flags on the specified processes or ports. Flag
`timestamp` is always turned on.

See the Reference Manual for module `m:dbg` for the possible trace flags.
Parameter `MatchDesc` is the same as returned from `dbg:p/2`.

Processes can be specified as registered names, globally registered names, or
process identifiers. Ports can be specified as registered names or port
identifiers. If a registered name is specified, the flags are set on
processes/ports with this name on all active nodes.

Issuing this command starts the timer for this trace if option `timer` is
specified with [`tracer/2`](`tracer/2`).

# `run_config`

```elixir
-spec run_config(ConfigFile) -> Result
                    when ConfigFile :: file:filename(), Result :: ok | {error, term()}.
```

Executes all entries in the specified configuration file. Notice that the
history of the last trace is always available in file `ttb_last_config`.

# `run_config`

```elixir
-spec run_config(ConfigFile, NumList) -> Result
                    when
                        ConfigFile :: file:filename(),
                        NumList :: [integer()],
                        Result :: ok | {error, term()}.
```

Executes selected entries from the specified configuration file. `NumList` is a
list of integers pointing out the entries to be executed.

To list the contents of a configuration file, use
[`list_config/1`](`list_config/1`).

Notice that the history of the last trace is always available in file
`ttb_last_config`.

# `run_history`

```elixir
-spec run_history(Entries) -> ok | {error, term()}
                     when Entries :: [Entry] | Entry | all | all_silent, Entry :: integer().
```

Executes the specified entry or entries from the history list. To list history,
use `list_history/0`.

# `seq_trigger_ms`

```elixir
-spec seq_trigger_ms() -> match_spec().
```

# `seq_trigger_ms`

```elixir
-spec seq_trigger_ms(Flags) -> match_spec()
                        when Flags :: all | SeqTraceFlag | [SeqTraceFlag], SeqTraceFlag :: atom().
```

A match specification can turn on or off sequential tracing. This function
returns a match specification, which turns on sequential tracing with the
specified `Flags`.

This match specification can be specified as the last argument to `tp` or `tpl`.
The activated `Item` then becomes a _trigger_ for sequential tracing. This means
that if the item is called on a process with trace flag `call` set, the process
is "contaminated" with token `seq_trace`.

If `Flags = all`, all possible flags are set.

The possible values for `SeqTraceFlag` are available in `m:seq_trace`.

For a description of the `match_spec()` syntax, see section
[`Match Specifications in Erlang`](`e:erts:match_spec.md`) in ERTS, which
explains the general match specification "language".

> #### Note {: .info }
>
> The _system tracer_ for sequential tracing is automatically initiated by `ttb`
> when a trace port is started with `ttb:tracer/0,1,2`.

An example of how to use function `seq_trigger_ms/0,1` follows:

```erlang
(tiger@durin)5> ttb:tracer().
{ok,[tiger@durin]}
(tiger@durin)6> ttb:p(all,call).
{ok,{[all],[call]}}
(tiger@durin)7> ttb:tp(mod,func,ttb:seq_trigger_ms()).
{ok,[{matched,1},{saved,1}]}
(tiger@durin)8>
```

Whenever `mod:func(...)` is called after this, token `seq_trace` is set on the
executing process.

# `start_trace`
*since OTP R15B* 

```elixir
-spec start_trace(Nodes, Patterns, FlagSpec, TracerOpts) -> Result
                     when
                         Nodes :: nodes(),
                         Patterns :: [tuple()],
                         FlagSpec :: {item(), trace_flag() | [trace_flag()]},
                         TracerOpts :: term(),
                         Result :: {ok, [{item(), match_desc()}]}.
```

This function is a shortcut allowing to start a trace with one command. Each
tuple in `Patterns` is converted to a list, which in turn is passed to
`ttb:tpl/2,3,4`.

The call:

```erlang
> ttb:start_trace([Node, OtherNode],
                  [{mod, foo, []}, {mod, bar, 2}],
                  {all, call},
                  [{file, File}, {handler,{fun myhandler/4, S}}]).
```

is equivalent to:

```erlang
> ttb:start_trace([Node, OtherNode],
                  [{file, File}, {handler,{fun myhandler/4, S}}]),
ttb:tpl(mod, foo, []),
ttb:tpl(mod, bar, 2, []),
ttb:p(all, call).
```

# `stop`

```elixir
-spec stop() -> stopped | {stopped, Dir :: file:filename()}.
```

Equivalent to [`stop([])`](`stop/1`).

# `stop`

```elixir
-spec stop(Opts :: stop_opts()) -> stopped | {stopped, Dir :: file:filename()}.
```

Stops tracing on all nodes. Logs and trace information files are sent to the
trace control node and stored in a directory named
`ttb_upload_FileName-Timestamp`, where `Filename` is the one provided with
`{file, File}` during trace setup and `Timestamp` is of the form
`yyyymmdd-hhmmss`. Even logs from nodes on the same machine as the trace control
node are moved to this directory. The history list is saved to a file named
`ttb_last_config` for further reference (as it is no longer accessible through
history and configuration management functions, like `ttb:list_history/0`).

_Options:_

- **`nofetch`** - Indicates that trace logs are not to be collected after
  tracing is stopped.

- **`{fetch, Dir}`** - Allows specification of the directory to fetch the data
  to. If the directory already exists, an error is thrown.

- **`format`** - Indicates the trace logs to be formatted after tracing is
  stopped. All logs in the fetch directory are merged.

- **`return_fetch_dir`** - Indicates the return value to be `{stopped, Dir}` and
  not just `stopped`. This implies `fetch`.

# `tp`

```elixir
-spec tp(tp_module(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

# `tp`

```elixir
-spec tp(tp_module(), tp_function(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

# `tp`

```elixir
-spec tp(tp_module(), tp_function(), tp_arity(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

# `tpe`
*since OTP 19.0* 

```elixir
-spec tpe(Event, MatchSpec) -> {ok, MatchDesc :: match_desc()} | {error, term()}
             when Event :: send | 'receive', MatchSpec :: match_spec().
```

# `tpl`

```elixir
-spec tpl(tp_module(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

# `tpl`

```elixir
-spec tpl(tp_module(), tp_function(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

# `tpl`

```elixir
-spec tpl(tp_module(), tp_function(), tp_arity(), match_spec()) -> {ok, match_desc()} | {error, term()}.
```

These functions are to be used with trace flag `call`, `send`, and `'receive'`
for setting and clearing trace patterns.

When trace flag `call` is set on a process, function calls are traced on that
process if a trace pattern is set for the called function.

The `send` and `'receive'` flags enable tracing of all messages sent and
received by the process/port. Trace patterns set with `tpe` may limit traced
messages based on the message content, the sender, and/or the receiver.

Trace patterns specify how to trace a function or a message by using match
specifications. Match specifications are described in the
[`ERTS User's Guide`](`e:erts:match_spec.md`).

These functions are equivalent to the corresponding functions in module `m:dbg`,
but all calls are stored in the history. The history buffer makes it easy to
create configuration files; the same trace environment can be set up many times,
for example, to compare two test runs. It also reduces the amount of typing when
using `ttb` from the Erlang shell.

- **`tp`** - Sets trace patterns on global function calls.

- **`tpl`** - Sets trace patterns on local and global function calls.

- **`tpe`** - Sets trace patterns on messages.

- **`ctp`** - Clears trace patterns on local and global function calls.

- **`ctpl`** - Clears trace patterns on local function calls.

- **`ctpg`** - Clears trace patterns on global function calls.

- **`ctpe`** - Clears trace patterns on messages.

With `tp` and `tpl`, one of the match specification shortcuts can be used (for
example, `ttb:tp(foo_module, caller)`).

The shortcuts are as follows:

- `return` \- for `[{'_',[],[{return_trace}]}]` (report the return value from a
  traced function)
- `caller` \- for `[{'_',[],[{message,{caller}}]}]` (report the calling
  function)
- `{codestr, Str}` \- for `dbg:fun2ms/1` arguments passed as strings (example:
  `"fun(_) -> return_trace() end"`)

# `tracer`

```elixir
-spec tracer() -> {ok, [node()]} | {error, term()}.
```

# `tracer`

```elixir
-spec tracer(shell | dbg | nodes()) -> {ok, [node()]} | {error, term()}.
```

Handy shortcuts for common tracing settings.

`shell` is equivalent to
[`tracer(node(),[{file, {local, "ttb"}}, shell])`](`tracer/2`).

`dbg` is equivalent to [`tracer(node(),[{shell, only}])`](`tracer/2`).

`Nodes` is equivalent to [`tracer(Nodes,[])`](`tracer/2`).

# `tracer`

```elixir
-spec tracer(Nodes, Opts) -> Result
                when
                    Nodes :: nodes(),
                    Opts :: Opt | [Opt],
                    Opt ::
                        {file, Client} |
                        {handler, format_handler()} |
                        {process_info, boolean()} |
                        shell |
                        {shell, ShellSpec} |
                        {timer, TimerSpec} |
                        {overload_check, {MSec, Module, Function}} |
                        {flush, MSec} |
                        resume |
                        {resume, MSec} |
                        {queue_size, non_neg_integer()},
                    TimerSpec :: MSec | {MSec, stop_opts()},
                    MSec :: timer:time(),
                    Module :: atom(),
                    Function :: atom(),
                    Client :: File | {local, File},
                    File :: file:filename() | Wrap,
                    Wrap ::
                        {wrap, file:filename()} |
                        {wrap, file:filename(), Size :: integer(), Count :: integer()},
                    ShellSpec :: true | false | only,
                    Result :: {ok, [node()]} | {error, term()}.
```

Starts a file trace port on all specified nodes and points the system tracer for
sequential tracing to the same port.

_Options:_

- **`Filename`** - The specified `Filename` is prefixed with the node name.
  Default `Filename` is `ttb`.

- **`File={wrap,Filename,Size,Count}`** - Can be used if the size of the trace
  logs must be limited. Default values are `Size=128*1024` and `Count=8`.

- **`Client`** - When tracing diskless nodes, `ttb` must be started from an
  external "trace control node" with disk access, and `Client` must be
  `{local, File}`. All trace information is then sent to the trace control node
  where it is written to file.

- **`queue_size`** - When tracing to shell or `{local,File}`, an ip trace driver
  is used internally. The ip trace driver has a queue of maximum `QueueSize`
  messages waiting to be delivered. If the driver cannot deliver messages as
  fast as they are produced, the queue size might be exceeded and messages are
  dropped. This parameter is optional, and is only useful if many `{drop,N}`
  trace messages are received by the trace handler. It has no meaning if shell
  or `{local,File}` is not used. See `dbg:trace_port/2` for more information
  about the ip trace driver.

- **`process_info`** - Indicates if process information is to be collected. If
  `PI = true` (which is default), each process identifier `Pid` is replaced by a
  tuple `{Pid,ProcessInfo,Node}`, where `ProcessInfo` is the registered process
  name, its globally registered name, or its initial function. To turn off this
  functionality, set `PI = false`.

- **`{shell, ShellSpec}`** - Indicates that trace messages are to be printed on
  the console as they are received by the tracing process. This implies trace
  client `{local, File}`. If `ShellSpec` is `only` (instead of `true`), no trace
  logs are stored.

- **`shell`** - Shortcut for `{shell, true}`.

- **`timer`** - Indicates that the trace is to be automatically stopped after
  `MSec` milliseconds. `StopOpts` are passed to command `ttb:stop/1` if
  specified (default is `[]`). Notice that the timing is approximate, as delays
  related to network communication are always present. The timer starts after
  `ttb:p/2` is issued, so you can set up your trace patterns before.

- **`overload_check`** - Allows to enable overload checking on the nodes under
  trace. `Module:Function(check)` is performed each `MSec` millisecond. If the
  check returns `true`, the tracing is disabled on a specified node.

  `Module:Function` must be able to handle at least three atoms: `init`,
  `check`, and `stop`. `init` and `stop` allows you to initialize and clean up
  the check environment.

  When a node gets overloaded, it is not possible to issue `ttb:p/2` or any
  command from the `ttb:tp/2,3,4` family, as it would lead to inconsistent
  tracing state (different trace specifications on different nodes).

- **`flush`** - Periodically flushes all file trace port clients (see
  `dbg:flush_trace_port/1`). When enabled, the buffers are freed each `MSec`
  millisecond. This option is not allowed with `{file, {local, File}}` tracing.

- **`{resume, FetchTimeout}`** - Enables the autoresume feature. When enabled,
  remote nodes try to reconnect to the controlling node if they are restarted.
  The feature requires application Runtime_Tools to be started (so it has to be
  present in the `.boot` scripts if the traced nodes run with embedded Erlang).
  If this is not possible, resume can be performed manually by starting
  `Runtime_Tools` remotely using `rpc:call/4`.

  `ttb` tries to fetch all logs from a reconnecting node before reinitializing
  the trace. This must finish within `FetchTimeout` milliseconds or is aborted.

  By default, autostart information is stored in a file named
  `ttb_autostart.bin` on each node. If this is not desired (for example, on
  diskless nodes), a custom module handling autostart information storage and
  retrieval can be provided by specifying environment variable
  `ttb_autostart_module` for the application Runtime_Tools. The module must
  respond to the following API:

  - **`write_config(Data) -> ok`** - Stores the provided data for further
    retrieval. It is important to realize that the data storage used must not be
    affected by the node crash.

  - **`read_config() -> {ok, Data} | {error, Error}`** - Retrieves configuration
    stored with `write_config(Data)`.

  - **`delete_config() -> ok`** - Deletes configuration stored with
    `write_config(Data)`. Notice that after this call any subsequent calls to
    `read_config` must return `{error, Error}`.

  `resume` implies the default `FetchTimeout`, which is 10 seconds

# `write_config`

```elixir
-spec write_config(ConfigFile, Config) -> Result
                      when
                          ConfigFile :: file:filename(),
                          Config :: all | [integer()] | [mfas()],
                          Result :: ok | {error, term()}.
```

# `write_config`

```elixir
-spec write_config(ConfigFile, Config, Opts) -> Result
                      when
                          ConfigFile :: file:filename(),
                          Config :: all | [integer()] | [mfas()],
                          Opts :: Opt | [Opt],
                          Opt :: append,
                          Result :: ok | {error, term()}.
```

Creates or extends a configuration file, which can be used for restoring a
specific configuration later.

The contents of the configuration file can either be fetched from the history or
specified directly as a list of `{Mod,Func,Args}`.

If the complete history is to be stored in the configuration file, `Config` must
be `all`. If only a selected number of entries from the history are to be
stored, `Config` must be a list of integers pointing out the entries to be
stored.

If `Opts` is not specified or if it is `[]`, `ConfigFile` is deleted and a new
file is created. If `Opts = [append]`, `ConfigFile` is not deleted. The new
information is appended at the end of the file.

# `write_trace_info`

```elixir
-spec write_trace_info(Key :: term(), Info) -> ok
                          when Info :: Data :: term() | fun(() -> Data :: term()).
```

File `.ti` contains `{Key,ValueList}` tuples. This function adds `Data` to the
`ValueList` associated with `Key`. All information written with this function is
included in the call to the format handler.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
