# `ct_testspec`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_testspec.erl#L23)

Parsing of test specifications for `Common Test`.

This module exports help functions for parsing of test specifications.

# `get_tests`
*since OTP 19.3* 

```erlang
-spec get_tests(Specs) -> {ok, [{Specs, Tests}]} | {error, Reason}
                   when
                       Specs :: [string()] | [[string()]],
                       Tests :: {Node, Run, Skip},
                       Node :: atom(),
                       Run :: {Dir, Suites, Cases},
                       Skip :: {Dir, Suites, Comment} | {Dir, Suites, Cases, Comment},
                       Dir :: string(),
                       Suites :: atom | [atom()] | all,
                       Cases :: atom | [atom()] | all,
                       Comment :: string(),
                       Reason :: term().
```

Parse the given test specification files and return the tests to run and skip.

[](){: #add_nodes-1 }

If `SpecsIn=[Spec1,Spec2,...]`, separate tests will be created per
specification. If `SpecsIn=[[Spec1,Spec2,...]]`, all specifications will be
merge into one test.

For each test, a `{Specs,Tests}` element is returned, where `Specs` is a list of
all included test specifications, and `Tests` specifies actual tests to run/skip
per node.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
