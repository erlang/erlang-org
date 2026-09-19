# `cprof`
[🔗](https://github.com/erlang/otp/blob/master/lib/tools/src/cprof.erl#L22)

A simple Call Count Profiling Tool using breakpoints for minimal runtime
performance impact.

The `cprof` module is used to profile a program to find out how many times
different functions are called. To minimize runtime performance impact,
breakpoints containing counters are used.

Since breakpoints are used there is no need for special compilation of the
modules to be profiled. These breakpoints can only be set on BEAM code, so
BIFs cannot be call-count traced.

The size of the call counters is the host machine word size. One bit is used
when pausing the counter, so the maximum counter value for a 32-bit host
is 2,147,483,647.

The profiling result is delivered as a term containing a sorted list of entries,
one per module. Each module entry contains a sorted list of functions. The
sorting order in both cases is of decreasing call count.

Call count tracing is lightweight compared to other forms of tracing,
such as `m:eprof` or `m:fprof`, since no trace messages have to be
generated. Some measurements indicates that the performance degradation is
about 10 percent.

For more information and some examples, see the
[User's Guide for `cprof`](cprof_chapter.md).

# `func_analysis_list`
*not exported* 

```erlang
-type func_analysis_list() :: [{mfa(), FuncCallCount :: non_neg_integer()}].
```

# `mod_analysis`
*not exported* 

```erlang
-type mod_analysis() ::
          {Mod :: module(), ModCallCount :: non_neg_integer(), FuncAnalysisList :: func_analysis_list()}.
```

# `mod_analysis_list`
*not exported* 

```erlang
-type mod_analysis_list() :: [mod_analysis()].
```

# `analyse`

```erlang
-spec analyse() -> {AllCallCount :: non_neg_integer(), ModAnalysisList :: mod_analysis_list()}.
```

# `analyse`

```erlang
-spec analyse(Limit) -> {AllCallCount :: non_neg_integer(), ModAnalysisList :: mod_analysis_list()}
                 when Limit :: non_neg_integer();
             (Mod) -> ModAnalysis :: mod_analysis() when Mod :: module().
```

Collect call counters for one or more modules.

If `ModLimit` is a module name (an atom), this call is equivalent to
[`analyse(ModLimit, 1)`](`analyse/2`).

If `ModLimit` is an integer, this function calls
[`analyse(Module, ModLimit)`](`analyse/2`) for each `Module` that is
currently loaded (except the `cprof` module itself).
The result from those calls are returned in a list.

# `analyse`

```erlang
-spec analyse(Mod, Limit) -> ModAnalysis :: mod_analysis()
                 when Mod :: module(), Limit :: non_neg_integer().
```

Collects and analyses all call counters for module `Module`.

This function returns:

```
{Module, ModuleCount, FuncAnalysisList}
```

where `FuncAnalysisList` is a list of tuples, one for each function:

```
{{Module, FunctionName, Arity}, FuncCallCount}
```

If call counters are still running while `analyse/0,1,2` is executing, the result
could be inconsistent. This happens if the process executing `analyse/0,1,2`
is scheduled out so some other process can increment the counters that are
being analysed. Calling [`pause()`](`pause/0`) before analysing takes care of
that problem.

All functions with a `FuncCallCount` lower than `Limit` are excluded from
`FuncAnalysisList`. They are still included in `ModCallCount`, though.

# `pause`

```erlang
-spec pause() -> non_neg_integer().
```

Pause call count tracing for all functions in all modules and stop it for all
functions in modules to be loaded.

This call is equivalent to
[`pause('_', '_', '_') + stop({on_load})`](`pause/3`).

# `pause`

```erlang
-spec pause(FuncSpec) -> non_neg_integer() when FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.
```

If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`pause(FuncSpec, '_', '_')`](`pause/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`pause(Module, Name, Arity)`](`pause/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be paused for all functions in modules to be loaded.

# `pause`

```erlang
-spec pause(Mod, Func) -> non_neg_integer() when Mod :: module(), Func :: atom().
```

# `pause`

```erlang
-spec pause(Mod, Func, Arity) -> non_neg_integer()
               when Mod :: module(), Func :: atom(), Arity :: arity().
```

Pause call counters for matching functions in matching modules.

The call counters for all matching functions that have call count breakpoints
are paused at their current count.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/*`](`start/3`) with the same arguments would have
returned.

# `restart`

```erlang
-spec restart() -> non_neg_integer().
```

# `restart`

```erlang
-spec restart(FuncSpec) -> non_neg_integer() when FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.
```

If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`restart(FuncSpec, '_', '_')`](`restart/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`restart(Module, Name, Arity)`](`restart/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be set to zero and running for all functions in
modules to be loaded.

# `restart`

```erlang
-spec restart(Mod, Func) -> non_neg_integer() when Mod :: module(), Func :: atom().
```

# `restart`

```erlang
-spec restart(Mod, Func, Arity) -> non_neg_integer()
                 when Mod :: module(), Func :: atom(), Arity :: arity().
```

Restart call counters for the matching functions in matching modules that are
call-count traced.

The call counters for all matching functions that has call count breakpoints
are set to zero and running.

Return the number of matching functions that can have call count breakpoints,
the same as [`start/*`](`start/3`) with the same arguments would have
returned.

# `start`

```erlang
-spec start() -> non_neg_integer().
```

Start call count tracing for all functions in all modules, and also for all
functions in modules to be loaded.

This is equivalent to
[`start('_', '_', '_') + start({on_load})`](`start/3`).

# `start`

```erlang
-spec start(FuncSpec) -> non_neg_integer() when FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.
```

If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`start(FuncSpec, '_', '_')`](`start/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`start(Module, Name, Arity)`](`start/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters will be set to zero and running for all functions in
modules to be loaded.

# `start`

```erlang
-spec start(Mod, Func) -> non_neg_integer() when Mod :: module(), Func :: atom().
```

# `start`

```erlang
-spec start(Mod, Func, Arity) -> non_neg_integer()
               when Mod :: module(), Func :: atom(), Arity :: arity().
```

Start call count tracing for matching functions in matching modules.

Set call count breakpoints on the matching functions that has no call count
breakpoints. Call counters are set to zero and running for all matching
functions.

Return the number of matching functions that has call count breakpoints.

# `stop`

```erlang
-spec stop() -> non_neg_integer().
```

Stop call count tracing for all functions in all modules, and also for all
functions in modules to be loaded.

This is equivalent to
[`stop('_', '_', '_') + stop({on_load})`](`stop/3`).

# `stop`

```erlang
-spec stop(FuncSpec) -> non_neg_integer() when FuncSpec :: (Mod :: module()) | mfa() | {FS :: term()}.
```

If `FuncSpec` is an atom, it is assumed to be a module name, and
this call is equivalent to [`stop(FuncSpec, '_', '_')`](`stop/3`).

If `FuncSpec` is an MFA tuple, `{Module, Name, Arity`}, this call
is equivalent to [`stop(Module, Name, Arity)`](`stop/3`).

If `FuncSpec` is tuple `{FS}`, `FS` is the first argument to
`erlang:trace_pattern/3`. For example, if `FuncSpec` is `{on_load}`,
call counters be disabled for all functions in modules to be loaded.

# `stop`

```erlang
-spec stop(Mod, Func) -> non_neg_integer() when Mod :: module(), Func :: atom().
```

# `stop`

```erlang
-spec stop(Mod, Func, Arity) -> non_neg_integer() when Mod :: module(), Func :: atom(), Arity :: arity().
```

Stop call count tracing for matching functions in matching modules.

Remove call count breakpoints from the matching functions that has call count
breakpoints.

Return the number of matching functions that can have call count breakpoints,
which is the same as [`start/*`](`start/3`) with the same arguments would have
returned.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
