# `ct_hooks`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_hooks.erl#L23)

A callback interface on top of Common Test.

The _Common Test Hook (CTH)_ framework allows extensions of the default behavior
of `Common Test` by callbacks before and after all test suite calls. It is
intended for advanced users of `Common Test` who want to abstract out behavior
that is common to multiple test suites.

In brief, CTH allows you to:

- Manipulate the runtime configuration before each suite configuration call.
- Manipulate the return of all suite configuration calls and by extension the
  result of the test themselves.

The following sections describe the mandatory and optional CTH functions that
`Common Test` calls during test execution. For more details, see section
[Common Test Hooks](ct_hooks_chapter.md) in the User's Guide.

For information about how to add a CTH to your suite, see section
[Installing a CTH](ct_hooks_chapter.md#installing) in the User's Guide.

> #### Note {: .info }
>
> For a minimal example of a CTH, see section
> [Example CTH](ct_hooks_chapter.md#example) in the User's Guide.

# `id`
*since OTP R14B02* *optional* 

```erlang
-callback id(Opts) -> Id when Opts :: term(), Id :: term().
```

The `Id` identifies a CTH instance uniquely. If two CTHs return the same `Id`,
the second CTH is ignored and subsequent calls to the CTH are only made to the
first instance. For details, see section
[Installing a CTH](ct_hooks_chapter.md#installing) in the User's Guide.

This function is _not_ to have any side effects, as it can be called multiple
times by `Common Test`.

If not implemented, the CTH acts as if this function returned a call to
`make_ref/0`.

# `init`
*since OTP R14B02* 

```erlang
-callback init(Id, Opts) -> {ok, State} | {ok, State, Priority}
                  when Id :: reference() | term(), Opts :: term(), State :: term(), Priority :: integer().
```

This function is always called before any other callback function. Use it to
initiate any common state. It is to return a state for this CTH.

`Id` is either the return value of [`ct_hooks:id/1`](`c:id/1`), or a `reference`
(created using `erlang:make_ref/0` in ERTS) if [`ct_hooks:id/1`](`c:id/1`) is
not implemented.

`Priority` is the relative priority of this hook. Hooks with a lower priority
are executed first. If no priority is specified, it is set to `0`.

For details about hook execution order, see section
[CTH Execution Order](ct_hooks_chapter.md#cth_execution_order) in the User's
Guide.

For details about when `init` is called, see section
[CTH Scope](ct_hooks_chapter.md#scope) in the User's Guide.

# `on_tc_fail`
*since OTP 19.3* *optional* 

```erlang
-callback on_tc_fail(SuiteName, TestName, Reason, CTHState) -> NewCTHState
                        when
                            SuiteName :: atom(),
                            TestName ::
                                init_per_suite | end_per_suite |
                                {init_per_group, GroupName} |
                                {end_per_group, GroupName} |
                                {FuncName, GroupName} |
                                FuncName,
                            FuncName :: atom(),
                            GroupName :: atom(),
                            Reason :: term(),
                            CTHState :: term(),
                            NewCTHState :: term().
```

This function is called whenever a test case (or configuration function) fails.
It is called after the post function is called for the failed test case.

That is:

- If `init_per_suite` fails, this function is called after
  [`post_init_per_suite`](`c:post_init_per_suite/4`).
- If a test case fails, this function is called after
  [`post_end_per_testcase`](`c:post_end_per_testcase/5`).

If the failed test case belongs to a test case group, the second argument is a
tuple `{FuncName,GroupName}`, otherwise only the function name.

The data that comes with `Reason` follows the same format as
[`FailReason`](event_handler_chapter.md#failreason) in event
[`tc_done`](event_handler_chapter.md#tc_done). For details, see section
[Event Handling](event_handler_chapter.md#events) in the User's Guide.

If [`Module:on_tc_fail/4`](`c:on_tc_fail/4`) is not exported, common_test will
attempt to call `Module:on_tc_fail(TestName, Reason, CTHState)` instead. This is
for backwards compatibility.

# `on_tc_skip`
*since OTP 19.3* *optional* 

```erlang
-callback on_tc_skip(SuiteName, TestName, Reason, CTHState) -> NewCTHState
                        when
                            SuiteName :: atom(),
                            TestName ::
                                init_per_suite | end_per_suite |
                                {init_per_group, GroupName} |
                                {end_per_group, GroupName} |
                                {FuncName, GroupName} |
                                FuncName,
                            FuncName :: atom(),
                            GroupName :: atom(),
                            Reason :: {tc_auto_skip | tc_user_skip, term()},
                            CTHState :: term(),
                            NewCTHState :: term().
```

This function is called whenever a test case (or configuration function) is
skipped. It is called after the post function is called for the skipped test
case.

That is:

- If `init_per_group` is skipped, this function is called after
  [`post_init_per_group`](`c:post_init_per_group/5`).
- If a test case is skipped, this function is called after
  [`post_end_per_testcase`](`c:post_end_per_testcase/5`).

If the skipped test case belongs to a test case group, the second argument is a
tuple `{FuncName,GroupName}`, otherwise only the function name.

The data that comes with `Reason` follows the same format as events
[`tc_auto_skip`](event_handler_chapter.md#tc_auto_skip) and
[`tc_user_skip`](event_handler_chapter.md#tc_user_skip) For details, see section
[Event Handling](event_handler_chapter.md#events) in the User's Guide.

If [`Module:on_tc_skip/4`](`c:on_tc_skip/4`) is not exported, common_test will
attempt to call `Module:on_tc_skip(TestName, Reason, CTHState)` instead. This is
for backwards compatibility.

# `post_all`
*since OTP 21.3.8* *optional* 

```erlang
-callback post_all(SuiteName, Return, GroupDefs) -> NewReturn
                      when
                          SuiteName :: atom(),
                          Return :: Tests | {skip, Reason},
                          NewReturn :: Tests | {skip, Reason},
                          Tests ::
                              [TestCase |
                               {testcase, TestCase, TCRepeatProps} |
                               {group, GroupName} |
                               {group, GroupName, Properties} |
                               {group, GroupName, Properties, SubGroups}],
                          TestCase :: atom(),
                          TCRepeatProps :: [{repeat, N} | {repeat_until_ok, N} | {repeat_until_fail, N}],
                          GroupName :: atom(),
                          Properties :: GroupProperties | default,
                          SubGroups :: [{GroupName, Properties} | {GroupName, Properties, SubGroups}],
                          Shuffle :: shuffle | {shuffle, Seed},
                          Seed :: {integer(), integer(), integer()},
                          GroupRepeatType ::
                              repeat | repeat_until_all_ok | repeat_until_all_fail | repeat_until_any_ok |
                              repeat_until_any_fail,
                          N :: integer() | forever,
                          GroupDefs :: [Group],
                          Group :: {GroupName, GroupProperties, GroupsAndTestCases},
                          GroupProperties :: [parallel | sequence | Shuffle | {GroupRepeatType, N}],
                          GroupsAndTestCases :: [Group | {group, GroupName} | TestCase],
                          Reason :: term().
```

This function is called after [`all/0`](`c:ct_suite:all/0`). It is used to
modify the set of test cases and test group to be executed, for instance to add
or remove test cases and groups, change group properties, or even skip all tests
in the suite.

`Return` is what [`all/0`](`c:ct_suite:all/0`) returned, that is, a list of test
cases and groups to be executed, or a tuple `{skip,Reason}`.

`GroupDefs` is what [`groups/0`](`c:ct_suite:groups/0`) or the
[`post_groups/2`](`c:post_groups/2`) hook returned, that is, a list of group
definitions.

`NewReturn` is the possibly modified version of `Return`.

This function is called only if the CTH is added before `init_per_suite` is run.
For details, see section [CTH Scope](ct_hooks_chapter.md#scope) in the User's
Guide.

Notice that for CTHs that are installed by means of the
[`suite/0`](`c:ct_suite:suite/0`) function, `post_all/2` is called before the
`c:init/2` hook function. However, for CTHs that are installed by means of the
CT start flag, the `c:init/2` function is called first.

> #### Note {: .info }
>
> Prior to each test execution, Common Test does a simulated test run in order
> to count test suites, groups and cases for logging purposes. This causes the
> [`post_all/3`](`c:post_all/3`) hook function to always be called twice. For
> this reason, side effects are best avoided in this callback.

# `post_end_per_group`
*since OTP 19.3* *optional* 

```erlang
-callback post_end_per_group(SuiteName, GroupName, Config, Return, CTHState) -> Result
                                when
                                    SuiteName :: atom(),
                                    GroupName :: atom(),
                                    Config :: [{Key, Value}],
                                    Return :: Config | SkipOrFail | term(),
                                    NewReturn :: Config | SkipOrFail | term(),
                                    SkipOrFail :: {fail, Reason} | {skip, Reason},
                                    CTHState :: term(),
                                    NewCTHState :: term(),
                                    Result :: {NewReturn, NewCTHState},
                                    Key :: atom(),
                                    Value :: term(),
                                    Reason :: term().
```

This function is called after [`end_per_group`](`c:ct_suite:end_per_group/2`) if
it exists. It behaves the same way as
[`post_init_per_suite`](`c:post_init_per_suite/4`), but for function
[end_per_group](`c:ct_suite:end_per_group/2`) instead.

If [`Module:post_end_per_group/5`](`c:post_end_per_group/5`) is not exported,
common_test will attempt to call
`Module:post_end_per_group(GroupName, Config, Return, CTHState)` instead. This
is for backwards compatibility.

# `post_end_per_suite`
*since OTP R14B02* *optional* 

```erlang
-callback post_end_per_suite(SuiteName, Config, Return, CTHState) -> Result
                                when
                                    SuiteName :: atom(),
                                    Config :: [{Key, Value}],
                                    Return :: Config | SkipOrFail | term(),
                                    NewReturn :: Config | SkipOrFail | term(),
                                    SkipOrFail :: {fail, Reason} | {skip, Reason},
                                    CTHState :: term(),
                                    NewCTHState :: term(),
                                    Result :: {NewReturn, NewCTHState},
                                    Key :: atom(),
                                    Value :: term(),
                                    Reason :: term().
```

This function is called after [`end_per_suite`](`c:ct_suite:end_per_suite/1`) if
it exists. It behaves the same way as
[`post_init_per_suite`](`c:post_init_per_suite/4`), but for function
[`end_per_suite`](`c:ct_suite:end_per_suite/1`) instead.

# `post_end_per_testcase`
*since OTP 19.3* *optional* 

```erlang
-callback post_end_per_testcase(SuiteName, TestcaseName, Config, Return, CTHState) -> Result
                                   when
                                       SuiteName :: atom(),
                                       TestcaseName :: atom(),
                                       Config :: [{Key, Value}],
                                       Return :: Config | SkipOrFail | term(),
                                       NewReturn :: Config | SkipOrFail | term(),
                                       SkipOrFail :: {fail, Reason} | {skip, Reason},
                                       CTHState :: term(),
                                       NewCTHState :: term(),
                                       Result :: {NewReturn, NewCTHState},
                                       Key :: atom(),
                                       Value :: term(),
                                       Reason :: term().
```

This function is called after
[`end_per_testcase`](`c:ct_suite:end_per_testcase/2`) if it exists. It behaves
the same way as [`post_end_per_suite`](`c:post_end_per_suite/4`), but for
function [`end_per_testcase`](`c:ct_suite:end_per_testcase/2`) instead.

If [`Module:post_end_per_testcase/5`](`c:post_end_per_testcase/5`) is not
exported, common_test will attempt to call
`Module:post_end_per_testcase(TestcaseName, Config, Return, CTHState)` instead.
This is for backwards compatibility.

# `post_groups`
*since OTP 21.3.8* *optional* 

```erlang
-callback post_groups(SuiteName, GroupDefs) -> NewGroupDefs
                         when
                             SuiteName :: atom(),
                             GroupDefs :: [Group],
                             NewGroupDefs :: [Group],
                             Group :: {GroupName, Properties, GroupsAndTestCases},
                             GroupName :: atom(),
                             Properties :: [parallel | sequence | Shuffle | {GroupRepeatType, N}],
                             GroupsAndTestCases ::
                                 [Group |
                                  {group, GroupName} |
                                  TestCase |
                                  {testcase, TestCase, TCRepeatProps}],
                             TestCase :: atom(),
                             TCRepeatProps :: [{repeat, N} | {repeat_until_ok, N} | {repeat_until_fail, N}],
                             Shuffle :: shuffle | {shuffle, Seed},
                             Seed :: {integer(), integer(), integer()},
                             GroupRepeatType ::
                                 repeat | repeat_until_all_ok | repeat_until_all_fail |
                                 repeat_until_any_ok | repeat_until_any_fail,
                             N :: integer() | forever.
```

This function is called after [`groups/0`](`c:ct_suite:groups/0`). It is used to
modify the test group definitions, for instance to add or remove groups or
change group properties.

`GroupDefs` is what [`groups/0`](`c:ct_suite:groups/0`) returned, that is, a
list of group definitions.

`NewGroupDefs` is the possibly modified version of this list.

This function is called only if the CTH is added before `init_per_suite` is run.
For details, see section [CTH Scope](ct_hooks_chapter.md#scope) in the User's
Guide.

Notice that for CTHs that are installed by means of the
[`suite/0`](`c:ct_suite:suite/0`) function, [`post_groups/2`](`c:post_groups/2`)
is called before the `c:init/2` hook function. However, for CTHs that are
installed by means of the CT start flag, the `c:init/2` function is called
first.

> #### Note {: .info }
>
> Prior to each test execution, Common Test does a simulated test run in order
> to count test suites, groups and cases for logging purposes. This causes the
> [`post_groups/2`](`c:post_groups/2`) hook function to always be called twice.
> For this reason, side effects are best avoided in this callback.

# `post_init_per_group`
*since OTP 19.3* *optional* 

```erlang
-callback post_init_per_group(SuiteName, GroupName, Config, Return, CTHState) -> Result
                                 when
                                     SuiteName :: atom(),
                                     GroupName :: atom(),
                                     Config :: [{Key, Value}],
                                     Return :: Config | SkipOrFail | term(),
                                     NewReturn :: Config | SkipOrFail | term(),
                                     SkipOrFail :: {fail, Reason} | {skip, Reason},
                                     CTHState :: term(),
                                     NewCTHState :: term(),
                                     Result :: {NewReturn, NewCTHState},
                                     Key :: atom(),
                                     Value :: term(),
                                     Reason :: term().
```

This function is called after [`init_per_group`](`c:ct_suite:init_per_group/2`)
if it exists. It behaves the same way as
[`post_init_per_suite`](`c:post_init_per_suite/4`), but for function
[`init_per_group`](`c:ct_suite:init_per_group/2`) instead.

If [`Module:post_init_per_group/5`](`c:post_init_per_group/5`) is not exported,
common_test will attempt to call
`Module:post_init_per_group(GroupName, Config, Return, CTHState)` instead. This
is for backwards compatibility.

# `post_init_per_suite`
*since OTP R14B02* *optional* 

```erlang
-callback post_init_per_suite(SuiteName, Config, Return, CTHState) -> Result
                                 when
                                     SuiteName :: atom(),
                                     Config :: [{Key, Value}],
                                     Return :: Config | SkipOrFail | term(),
                                     NewReturn :: Config | SkipOrFail | term(),
                                     SkipOrFail :: {fail, Reason} | {skip, Reason} | term(),
                                     CTHState :: term(),
                                     NewCTHState :: term(),
                                     Result :: {NewReturn, NewCTHState},
                                     Key :: atom(),
                                     Value :: term(),
                                     Reason :: term().
```

This function is called after [`init_per_suite`](`c:ct_suite:init_per_suite/1`)
if it exists. It typically contains extra checks to ensure that all the correct
dependencies are started correctly.

`Return` is what [`init_per_suite`](`c:ct_suite:init_per_suite/1`) returned,
that is, `{fail,Reason}`, `{skip,Reason}`, a `Config` list, or a term describing
how [`init_per_suite`](`c:ct_suite:init_per_suite/1`) failed.

`NewReturn` is the possibly modified return value of
[`init_per_suite`](`c:ct_suite:init_per_suite/1`). To recover from a failure in
[`init_per_suite`](`c:ct_suite:init_per_suite/1`), return `ConfigList` with the
`tc_status` element removed. For more details, see
[Post Hooks](ct_hooks_chapter.md#post) in section "Manipulating Tests" in the
User's Guide.

`CTHState` is the current internal state of the CTH.

This function is called only if the CTH is added before or in `init_per_suite`.
For details, see section [CTH Scope](ct_hooks_chapter.md#scope) in the User's
Guide.

# `post_init_per_testcase`
*since OTP 19.3* *optional* 

```erlang
-callback post_init_per_testcase(SuiteName, TestcaseName, Config, Return, CTHState) -> Result
                                    when
                                        SuiteName :: atom(),
                                        TestcaseName :: atom(),
                                        Config :: [{Key, Value}],
                                        Return :: Config | SkipOrFail | term(),
                                        NewReturn :: Config | SkipOrFail | term(),
                                        SkipOrFail :: {fail, Reason} | {skip, Reason},
                                        CTHState :: term(),
                                        NewCTHState :: term(),
                                        Result :: {NewReturn, NewCTHState},
                                        Key :: atom(),
                                        Value :: term(),
                                        Reason :: term().
```

This function is called after
[`init_per_testcase`](`c:ct_suite:init_per_testcase/2`) if it exists. It behaves
the same way as [`post_init_per_suite`](`c:post_init_per_suite/4`), but for
function [`init_per_testcase`](`c:ct_suite:init_per_testcase/2`) instead.

If [`Module:post_init_per_testcase/5`](`c:post_init_per_testcase/5`) is not
exported, common_test will attempt to call
`Module:post_init_per_testcase(TestcaseName, Config, Return, CTHState)` instead.
This is for backwards compatibility.

# `pre_end_per_group`
*since OTP 19.3* *optional* 

```erlang
-callback pre_end_per_group(SuiteName, GroupName, EndData, CTHState) -> Result
                               when
                                   SuiteName :: atom(),
                                   GroupName :: atom(),
                                   EndData :: Config | SkipOrFail,
                                   Config :: [{Key, Value}],
                                   NewConfig :: [{Key, Value}],
                                   CTHState :: term(),
                                   NewCTHState :: term(),
                                   Result :: {NewConfig | SkipOrFail, NewCTHState},
                                   SkipOrFail :: {fail, Reason} | {skip, Reason},
                                   Key :: atom(),
                                   Value :: term(),
                                   Reason :: term().
```

This function is called before [`end_per_group`](`c:ct_suite:end_per_group/2`)
if it exists. It behaves the same way as
[`pre_init_per_suite`](`c:pre_init_per_suite/3`), but for function
[`end_per_group`](`c:ct_suite:end_per_group/2`) instead.

If [`Module:pre_end_per_group/4`](`c:pre_end_per_group/4`) is not exported,
common_test will attempt to call
`Module:pre_end_per_group(GroupName, EndData, CTHState)` instead. This is for
backwards compatibility.

# `pre_end_per_suite`
*since OTP R14B02* *optional* 

```erlang
-callback pre_end_per_suite(SuiteName, EndData, CTHState) -> Result
                               when
                                   SuiteName :: atom(),
                                   EndData :: Config | SkipOrFail,
                                   Config :: [{Key, Value}],
                                   NewConfig :: [{Key, Value}],
                                   CTHState :: term(),
                                   NewCTHState :: term(),
                                   Result :: {NewConfig | SkipOrFail, NewCTHState},
                                   SkipOrFail :: {fail, Reason} | {skip, Reason},
                                   Key :: atom(),
                                   Value :: term(),
                                   Reason :: term().
```

This function is called before [`end_per_suite`](`c:ct_suite:end_per_suite/1`)
if it exists. It behaves the same way as
[`pre_init_per_suite`](`c:pre_init_per_suite/3`), but for function
[`end_per_suite`](`c:ct_suite:end_per_suite/1`) instead.

# `pre_end_per_testcase`
*since OTP 19.3* *optional* 

```erlang
-callback pre_end_per_testcase(SuiteName, TestcaseName, EndData, CTHState) -> Result
                                  when
                                      SuiteName :: atom(),
                                      TestcaseName :: atom(),
                                      EndData :: Config,
                                      Config :: [{Key, Value}],
                                      NewConfig :: [{Key, Value}],
                                      CTHState :: term(),
                                      NewCTHState :: term(),
                                      Result :: {NewConfig, NewCTHState},
                                      Key :: atom(),
                                      Value :: term().
```

This function is called before
[`end_per_testcase`](`c:ct_suite:end_per_testcase/2`) if it exists. It behaves
the same way as [`pre_end_per_suite`](`c:pre_end_per_suite/3`), but for function
[`end_per_testcase`](`c:ct_suite:end_per_testcase/2`) instead.

This function cannot change the result of the test case by returning skip or
fail tuples, but it may insert items in `Config` that can be read in
`end_per_testcase/2` or in
[`post_end_per_testcase/5`](`c:post_end_per_testcase/5`).

If [`Module:pre_end_per_testcase/4`](`c:pre_end_per_testcase/4`) is not
exported, common_test will attempt to call
`Module:pre_end_per_testcase(TestcaseName, EndData, CTHState)` instead. This is
for backwards compatibility.

# `pre_init_per_group`
*since OTP 19.3* *optional* 

```erlang
-callback pre_init_per_group(SuiteName, GroupName, InitData, CTHState) -> Result
                                when
                                    SuiteName :: atom(),
                                    GroupName :: atom(),
                                    InitData :: Config | SkipOrFail,
                                    Config :: [{Key, Value}],
                                    NewConfig :: [{Key, Value}],
                                    CTHState :: term(),
                                    NewCTHState :: term(),
                                    Result :: {NewConfig | SkipOrFail, NewCTHState},
                                    SkipOrFail :: {fail, Reason} | {skip, Reason},
                                    Key :: atom(),
                                    Value :: term(),
                                    Reason :: term().
```

This function is called before [`init_per_group`](`c:ct_suite:init_per_group/2`)
if it exists. It behaves the same way as
[`pre_init_per_suite`](`c:pre_init_per_suite/3`), but for function
[`init_per_group`](`c:ct_suite:init_per_group/2`) instead.

If [`Module:pre_init_per_group/4`](`c:pre_init_per_group/4`) is not exported,
common_test will attempt to call
`Module:pre_init_per_group(GroupName, InitData, CTHState)` instead. This is for
backwards compatibility.

# `pre_init_per_suite`
*since OTP R14B02* *optional* 

```erlang
-callback pre_init_per_suite(SuiteName, InitData, CTHState) -> Result
                                when
                                    SuiteName :: atom(),
                                    InitData :: Config | SkipOrFail,
                                    Config :: [{Key, Value}],
                                    NewConfig :: [{Key, Value}],
                                    CTHState :: term(),
                                    NewCTHState :: term(),
                                    Result :: {Return, NewCTHState},
                                    Return :: NewConfig | SkipOrFail,
                                    SkipOrFail :: {fail, Reason} | {skip, Reason},
                                    Key :: atom(),
                                    Value :: term(),
                                    Reason :: term().
```

This function is called before [`init_per_suite`](`c:ct_suite:init_per_suite/1`)
if it exists. It typically contains initialization/logging that must be done
before `init_per_suite` is called. If `{skip,Reason}` or `{fail,Reason}` is
returned, `init_per_suite` and all test cases of the suite are skipped and
`Reason` printed in the overview log of the suite.

`SuiteName` is the name of the suite to be run.

`InitData` is the original configuration list of the test suite, or a
`SkipOrFail` tuple if a previous CTH has returned this.

`CTHState` is the current internal state of the CTH.

`Return` is the result of the `init_per_suite` function. If it is
`{skip,Reason}` or `{fail,Reason}`,
[`init_per_suite`](`c:ct_suite:init_per_suite/1`) is never called, instead the
initiation is considered to be skipped or failed, respectively. If a `NewConfig`
list is returned, [`init_per_suite`](`c:ct_suite:init_per_suite/1`) is called
with that `NewConfig` list. For more details, see section
[Pre Hooks](ct_hooks_chapter.md#pre) in the User's Guide.

This function is called only if the CTH is added before `init_per_suite is run`.
For details, see section [CTH Scope](ct_hooks_chapter.md#scope) in the User's
Guide.

# `pre_init_per_testcase`
*since OTP 19.3* *optional* 

```erlang
-callback pre_init_per_testcase(SuiteName, TestcaseName, InitData, CTHState) -> Result
                                   when
                                       SuiteName :: atom(),
                                       TestcaseName :: atom(),
                                       InitData :: Config | SkipOrFail,
                                       Config :: [{Key, Value}],
                                       NewConfig :: [{Key, Value}],
                                       CTHState :: term(),
                                       NewCTHState :: term(),
                                       Result :: {NewConfig | SkipOrFail, NewCTHState},
                                       SkipOrFail :: {fail, Reason} | {skip, Reason},
                                       Key :: atom(),
                                       Value :: term(),
                                       Reason :: term().
```

This function is called before
[`init_per_testcase`](`c:ct_suite:init_per_testcase/2`) if it exists. It behaves
the same way as [`pre_init_per_suite`](`c:pre_init_per_suite/3`), but for
function [`init_per_testcase`](`c:ct_suite:init_per_testcase/2`) instead.

If [`Module:pre_init_per_testcase/4`](`c:pre_init_per_testcase/4`) is not
exported, common_test will attempt to call
`Module:pre_init_per_testcase(TestcaseName, InitData, CTHState)` instead. This
is for backwards compatibility.

CTHs cannot be added here right now. That feature may be added in a later
release, but it would right now break backwards compatibility.

# `terminate`
*since OTP R14B02* *optional* 

```erlang
-callback terminate(CTHState) -> term() when CTHState :: term().
```

This function is called at the end of a CTH [scope](ct_hooks_chapter.md#scope).
The returned term is ignored.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
