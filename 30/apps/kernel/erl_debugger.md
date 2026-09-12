# `erl_debugger`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/erl_debugger.erl#L24)

Erlang debugger support (EXPERIMENTAL).

This module exposes low-level functionality for the implementation
of a debugger for Erlang.

Any local process can register itself as the debugger for a node, but
there can be at most one such process registered at any given time.
Using the BIFs in this module, a debugger can:

  - set breakpoints;
  - inspect internal process state, such registers, stack-frames;
  - get notified on debugger events such as a process hitting a breakpoint;
  - resume processes paused on breakpoints

At the moment, the API is highly experimental; so don't depend on it,
or otherwise expect frequent incompatible changes.

# `event`
*since OTP 28.0* 

```erlang
-type event() :: {breakpoint, pid(), mfa(), Line :: pos_integer(), Resume :: fun(() -> ok)}.
```

Debugger events.

Here are the possible events:

  * `{breakpoint, Pid, {M,F,A}, Line, Resume}`: process Pid hit a breakpoint
    on module `M`, at the given `Line`. The debugger can resume the process
    by executing `Resume()`.

# `event_message`
*since OTP 28.0* 

```erlang
-type event_message() :: {debugger_event, session(), event()}.
```

The debugger process will receive debugger-event messages, wrapped in
an envelope of this type.

# `instrumentation`
*since OTP 28.0* 

```erlang
-type instrumentation() :: line_breakpoint.
```

Debugging instrumentations that can be applied on module loading.

  - `line_breakpoint`: Allows setting breakpoints at the beginning
     of executable lines

# `reg_val`
*since OTP 28.0* 

```erlang
-type reg_val() :: {value, term()} | {too_large, Size :: pos_integer()}.
```

The value of an X or a Y register, provided it fits within the requested
size.

If it is too large, then size of the term.

# `session`
*since OTP 28.0* 

```erlang
-opaque session()
```

Debugger session identifier.

It is attached to all debugger events.

# `stack_frame`
*since OTP 28.0* 

```erlang
-type stack_frame() :: {FrameNo :: non_neg_integer(), stack_frame_fun(), stack_frame_info()}.
```

A stack-frame, including the value of each slot.

# `stack_frame_fun`
*since OTP 28.0* 

```erlang
-type stack_frame_fun() ::
          #{function := mfa(), line := pos_integer() | undefined} |
          '<terminate process>' | '<continue terminate process>' | '<terminate process normally>' |
          '<breakpoint>' | 'unknown function'.
```

What is running in each stack frame, including special VM frames.

# `stack_frame_info`
*since OTP 28.0* 

```erlang
-type stack_frame_info() :: #{slots := [stack_frame_slot()], code := pos_integer()}.
```

Extra information about a stack-frame.

  - `slots`: Y-registers (in order `[Y0,...Yk])`, followed by exception-handlers.
  - `code`: Memory address of the next instruction to execute in this frame.

# `stack_frame_slot`
*since OTP 28.0* 

```erlang
-type stack_frame_slot() :: reg_val() | {'catch', stack_frame_fun()}.
```

The contents of a stack frame slot can be a Y register
or an exception handler.

# `breakpoint`
*since OTP 28.0* 

```erlang
-spec breakpoint(Module, Line, Flag) -> ok | {error, Reason}
                    when
                        Module :: module(),
                        Line :: pos_integer(),
                        Flag :: boolean(),
                        Reason :: {unsupported, Module | Line} | {badkey, Module | Line}.
```

Sets or clear a breakpoint on the given Module/Line.

When a process hits a breakpoint, it will pause and a `breakpoint`
message is sent to the registered debugger.

Returns `ok` on success. It can fail with the following reasons:
  - `{badkey, Module}`: The given module is not loaded.
  - `{unsupported, Module}`: The module was loaded without support
    for line breakpoints.
  - `{badkey, Line}`: The line is not relevant; it could refer to a comment,
     not existing in the module source, and so on.
  - `{unsupported, Line}`: It is not possible to set a breakpoint in
    in the given line; for example, if it refers to a function head.

# `breakpoints`
*since OTP 28.0* 

```erlang
-spec breakpoints(Module) -> {ok, Result} | {error, Reason}
                     when
                         Module :: module(),
                         Result :: #{Fun => #{Line => boolean()}},
                         Fun :: {atom(), arity()},
                         Line :: pos_integer(),
                         Reason :: badkey.
```

Returns information on available breakpoints for a module.

For each function in the module, returns a map `#{Line => boolean()}`,
where the keys are lines where breakpoints can be set, and the value
represents whether be breakpoint is enabled (`true`) or not (`false`).

# `breakpoints`
*since OTP 28.0* 

```erlang
-spec breakpoints(Module, FunName, Arity) -> {ok, Result} | {error, Reason}
                     when
                         Module :: module(),
                         FunName :: atom(),
                         Arity :: arity(),
                         Result :: #{Line => boolean()},
                         Line :: pos_integer(),
                         Reason :: {badkey, module() | {FunName, Arity}}.
```

Returns information on available breakpoints for a given function. .

The function need not be exported.

Returns a map `#{Line => boolean()}`, where the keys are lines where
breakpoints can be set, and the value represents whether be breakpoint
is enabled (`true`) or not (`false`).

# `instrumentations`
*since OTP 28.0* 

```erlang
-spec instrumentations() -> #{instrumentation() => boolean()}.
```

Returns the instrumentations that will be applied on module loading.

Modules that are already loaded may have had a different set of
instrumentations applied.

# `peek_stack_frame_slot`
*since OTP 28.0* 

```erlang
-spec peek_stack_frame_slot(Pid, FrameNo, Slot, MaxSize) -> running | undefined | stack_frame_slot()
                               when
                                   Pid :: pid(),
                                   FrameNo :: pos_integer(),
                                   Slot :: non_neg_integer(),
                                   MaxSize :: non_neg_integer().
```

Gets the value of a slot in a suspended process stack-frame.

Returns `running` if the process is not suspended, and `undefined`
if the frame or the slot does not exist for that process.
Otherwise, returns the slot, that can be a term, if its size is less
than `MaxTermSize`, or an exeption handler.

# `peek_xreg`
*since OTP 28.0* 

```erlang
-spec peek_xreg(Pid, Reg, MaxSize) -> running | undefined | reg_val()
                   when Pid :: pid(), Reg :: non_neg_integer(), MaxSize :: non_neg_integer().
```

Get the value of an X register for a suspended process.

# `register`
*since OTP 28.0* 

```erlang
-spec register(Pid) -> {ok, session()} | {error, already_exists} when Pid :: pid().
```

Register the given process as the debugger.

If the registration succeeds, it returns `{ok, Session}`, where `Session`
is a token that will be included in every message sent to the process.

Returns `{error, already_exists}` if some process is currently
registered as debugger.

# `stack_frames`
*since OTP 28.0* 

```erlang
-spec stack_frames(Pid, MaxTermSize) -> running | [stack_frame()]
                      when Pid :: pid(), MaxTermSize :: non_neg_integer().
```

Get the all the stack-frames for a suspended process.

If the given process is not in a suspended state, returns `running`.
Otherwise, a list of [stack frames](`t:stack_frame/0`) including the
content of each slot is returned. For slots containing terms,
`MaxTermSize` controls the maximum size of values that are allowed to
be returned (to avoid accidentally blowing the heap of the caller).

# `supported`
*since OTP 28.0* 

```erlang
-spec supported() -> boolean().
```

Returns `true` if the emulator supports debugging.

The debugger can only be used if the `+D` argument was passed
to the emulator on start-up.

# `toggle_instrumentations`
*since OTP 28.0* 

```erlang
-spec toggle_instrumentations(Toggle) -> ok when Toggle :: #{instrumentation() => boolean()}.
```

Updates the instrumentations that will be applied on module loading.

Modules that are already loaded will keep the instrumentation they
had at their time of loading.

# `unregister`
*since OTP 28.0* 

```erlang
-spec unregister(Pid, Session) -> ok when Pid :: pid(), Session :: session().
```

Unregisters the given process.

The session given on registration needs to be provided.

# `whereis`
*since OTP 28.0* 

```erlang
-spec whereis() -> undefined | pid().
```

Returns the pid of the registered debugger.

# `xregs_count`
*since OTP 28.0* 

```erlang
-spec xregs_count(Pid) -> running | non_neg_integer() when Pid :: pid().
```

Get the number of X registers currently in use by a suspended process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
