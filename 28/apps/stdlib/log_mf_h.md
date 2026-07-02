# `log_mf_h`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/log_mf_h.erl#L22)

An event handler that logs events to disk.

This module is a `gen_event` handler module that can be installed in any
`gen_event` process. It logs onto disk all events that are sent to an event
manager. Each event is written as a binary, which makes the logging very fast.
However, a tool such as the Report Browser (`m:rb`) must be used to read the
files. The events are written to multiple files. When all files have been used,
the first one is reused and overwritten. The directory location, the number of
files, and the size of each file are configurable. The directory will include
one file called `index`, and report files `1, 2, ...`.

## See Also

`m:gen_event`, `m:rb`

# `args`

```elixir
-opaque args() :: {file:filename(), b(), f(), pred()}.
```

Term to be sent to `gen_event:add_handler/3`.

# `b`
*not exported* 

```elixir
-type b() :: non_neg_integer().
```

# `f`
*not exported* 

```elixir
-type f() :: 1..255.
```

# `pred`
*not exported* 

```elixir
-type pred() :: fun((term()) -> boolean()).
```

# `init`

```elixir
-spec init(Dir, MaxBytes, MaxFiles) -> Args
              when
                  Dir :: file:filename(),
                  MaxBytes :: non_neg_integer(),
                  MaxFiles :: 1..255,
                  Args :: args().
```

# `init`

```elixir
-spec init(Dir, MaxBytes, MaxFiles, Pred) -> Args
              when
                  Dir :: file:filename(),
                  MaxBytes :: non_neg_integer(),
                  MaxFiles :: 1..255,
                  Pred :: fun((Event :: term()) -> boolean()),
                  Args :: args().
```

Initiates the event handler. Returns `Args`, which is to be used in a call to
[`gen_event:add_handler(EventMgr, log_mf_h, Args)`](`gen_event:add_handler/3`).

`Dir` specifies which directory to use for the log files. `MaxBytes` specifies
the size of each individual file. `MaxFiles` specifies how many files are used.
`Pred` is a predicate function used to filter the events. If no predicate
function is specified, all events are logged.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
