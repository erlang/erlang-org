# `ct_master`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/common_test/src/ct_master.erl#L23)

Distributed test execution control for `Common Test`.

This module exports functions for running `Common Test` nodes on multiple hosts
in parallel.

# `test_spec`

```erlang
-type test_spec() :: file:name_all().
```

Filename of test spec to be executed.

# `abort`

```erlang
-spec abort() -> ok.
```

Stops all running tests.

# `abort`

```erlang
-spec abort(Nodes) -> ok when Nodes :: Node | [Node], Node :: node().
```

Stops tests on specified nodes.

# `basic_html`
*since OTP R15B01* 

```erlang
-spec basic_html(Bool) -> ok when Bool :: boolean().
```

If set to `true`, the `ct_master logs` are written on a primitive HTML format,
not using the `Common Test` CSS style sheet.

# `get_event_mgr_ref`
*since OTP 17.5* 

```erlang
-spec get_event_mgr_ref() -> atom().
```

Gets a reference to the `Common Test` master event manager. The reference can be
used to, for example, add a user-specific event handler while tests are running.

_Example:_

```erlang
gen_event:add_handler(ct_master:get_event_mgr_ref(), my_ev_h, [])
```

# `progress`

```erlang
-spec progress() -> [{Node, Status}] when Node :: node(), Status :: atom().
```

Returns test progress. If `Status` is `ongoing`, tests are running on the node
and are not yet finished.

# `run`

```erlang
-spec run(TestSpecs) -> [{Specs, ok} | {error, Reason}]
             when
                 TestSpecs :: TestSpec | [TestSpec] | [[TestSpec]],
                 TestSpec :: test_spec(),
                 Specs :: [file:filename_all()],
                 Reason :: term().
```

Run tests on spawned nodes as specified in `TestSpecs` (see `run/4`).

Equivalent to [`run(TestSpecs, false, [], [])`](`run/4`) if
called with TestSpecs being list of strings;

Equivalent to [`run([TS], false, [], [])`](`run/4`) if
called with TS being string.

# `run`

```erlang
-spec run(TestSpecs, InclNodes, ExclNodes) -> [{Specs, ok} | {error, Reason}]
             when
                 TestSpecs :: TestSpec | [TestSpec] | [[TestSpec]],
                 TestSpec :: test_spec(),
                 InclNodes :: [node()],
                 ExclNodes :: [node()],
                 Specs :: [file:filename_all()],
                 Reason :: term().
```

# `run`

```erlang
-spec run(TestSpecs, AllowUserTerms, InclNodes, ExclNodes) -> [{Specs, ok} | {error, Reason}]
             when
                 TestSpecs :: TestSpec | [TestSpec] | [[TestSpec]],
                 TestSpec :: test_spec(),
                 AllowUserTerms :: boolean(),
                 InclNodes :: [node()],
                 ExclNodes :: [node()],
                 Specs :: [file:filename_all()],
                 Reason :: term().
```

Tests are spawned on the nodes as specified in `TestSpecs`. Each specification
in `TestSpec` is handled separately. However, it is also possible to specify a
list of specifications to be merged into one specification before the tests are
executed. Any test without a particular node specification is also executed on
the nodes in `InclNodes`. Nodes in the `ExclNodes` list are excluded from the
test.

# `run_on_node`

```erlang
-spec run_on_node(TestSpecs, Node) -> [{Specs, ok} | {error, Reason}]
                     when
                         TestSpecs :: TestSpec | [TestSpec] | [[TestSpec]],
                         TestSpec :: test_spec(),
                         Node :: node(),
                         Specs :: [file:filename_all()],
                         Reason :: term().
```

# `run_on_node`

```erlang
-spec run_on_node(TestSpecs, AllowUserTerms, Node) -> [{Specs, ok} | {error, Reason}]
                     when
                         TestSpecs :: TestSpec | [TestSpec] | [[TestSpec]],
                         TestSpec :: test_spec(),
                         AllowUserTerms :: boolean(),
                         Node :: node(),
                         Specs :: [file:filename_all()],
                         Reason :: term().
```

Tests are spawned on `Node` according to `TestSpecs`.

# `run_test`

```erlang
-spec run_test(Node, Opts) -> ok
                  when
                      Node :: node(),
                      Opts :: [OptTuples],
                      OptTuples ::
                          {dir, TestDirs} |
                          {suite, Suites} |
                          {group, Groups} |
                          {testcase, Cases} |
                          {spec, TestSpecs} |
                          {join_specs, boolean()} |
                          {label, Label} |
                          {config, CfgFiles} |
                          {userconfig, UserConfig} |
                          {allow_user_terms, boolean()} |
                          {logdir, LogDir} |
                          {silent_connections, Conns} |
                          {stylesheet, CSSFile} |
                          {cover, CoverSpecFile} |
                          {cover_stop, boolean()} |
                          {step, StepOpts} |
                          {event_handler, EventHandlers} |
                          {include, InclDirs} |
                          {auto_compile, boolean()} |
                          {abort_if_missing_suites, boolean()} |
                          {create_priv_dir, CreatePrivDir} |
                          {multiply_timetraps, M} |
                          {scale_timetraps, boolean()} |
                          {repeat, N} |
                          {duration, DurTime} |
                          {until, StopTime} |
                          {force_stop, ForceStop} |
                          {decrypt, DecryptKeyOrFile} |
                          {refresh_logs, LogDir} |
                          {logopts, LogOpts} |
                          {verbosity, VLevels} |
                          {basic_html, boolean()} |
                          {esc_chars, boolean()} |
                          {keep_logs, KeepSpec} |
                          {ct_hooks, CTHs} |
                          {ct_hooks_order, CTHsOrder} |
                          {enable_builtin_hooks, boolean()} |
                          {release_shell, boolean()},
                      TestDirs :: [string()] | string(),
                      Suites :: [string()] | [atom()] | string() | atom(),
                      Cases :: [atom()] | atom(),
                      Groups :: GroupNameOrPath | [GroupNameOrPath],
                      GroupNameOrPath :: [atom()] | atom() | all,
                      TestSpecs :: [string()] | string(),
                      Label :: string() | atom(),
                      CfgFiles :: [string()] | string(),
                      UserConfig :: [{CallbackMod, CfgStrings}] | {CallbackMod, CfgStrings},
                      CallbackMod :: atom(),
                      CfgStrings :: [string()] | string(),
                      LogDir :: string(),
                      Conns :: all | [atom()],
                      CSSFile :: string(),
                      CoverSpecFile :: string(),
                      StepOpts :: [StepOpt],
                      StepOpt :: config | keep_inactive,
                      EventHandlers :: EH | [EH],
                      EH :: atom() | {atom(), InitArgs} | {[atom()], InitArgs},
                      InitArgs :: [term()],
                      InclDirs :: [string()] | string(),
                      CreatePrivDir :: auto_per_run | auto_per_tc | manual_per_tc,
                      M :: integer(),
                      N :: integer(),
                      DurTime :: HHMMSS,
                      HHMMSS :: string(),
                      StopTime :: YYMoMoDDHHMMSS | HHMMSS,
                      YYMoMoDDHHMMSS :: string(),
                      ForceStop :: skip_rest | boolean(),
                      DecryptKeyOrFile :: {key, DecryptKey} | {file, DecryptFile},
                      DecryptKey :: string(),
                      DecryptFile :: string(),
                      LogOpts :: [LogOpt],
                      LogOpt :: no_nl | no_src,
                      VLevels :: VLevel | [{Category, VLevel}],
                      VLevel :: integer(),
                      Category :: atom(),
                      KeepSpec :: all | pos_integer(),
                      CTHs :: [CTHModule | {CTHModule, CTHInitArgs}],
                      CTHsOrder :: atom(),
                      CTHModule :: atom(),
                      CTHInitArgs :: term().
```

Tests are spawned on `Node` using `ct:run_test/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
