# `eunit`
[🔗](https://github.com/erlang/otp/blob/master/lib/eunit/src/eunit.erl#L32)

This module is the main EUnit user interface.

# `start`

```erlang
-spec start() -> term().
```

Starts the EUnit server. Normally, you don't need to call this function; it is
started automatically.

# `stop`

```erlang
-spec stop() -> term().
```

Stops the EUnit server. Normally, you don't need to call this function.

# `test`

```erlang
-spec test(Tests :: term()) -> ok | {error, term()} | error.
```

# `test`

```erlang
-spec test(Tests :: term(), Options :: [term()]) -> ok | error | {error, term()}.
```

Runs a set of tests. The format of `Tests` is described in the section
[EUnit test representation](chapter.md#EUnit_test_representation) of the
overview.

Example:

```text
  eunit:test(fred)
```

runs all tests in the module `fred` and also any tests in the module
`fred_tests`, if that module exists.

Options:

- **`verbose`** - Displays more details about the running tests.

- **`print_depth`** - Maximum depth to which terms are printed in case of error.

- **`exact_execution`** - If this boolean flag is set to `true` framework will
  not automatically execute tests found in related module suffixed with
  "\_tests". This behaviour might be unwanted if execution of modules found in a
  folder is ordered while it contains both source and test modules.

- **`scale_timeouts`** - If this numeric value is set, timeouts will get scaled
  accordingly. It may be useful when running a set of tests on a slower host.
  Examples: `{scale_timeouts,10}` make the timeouts 10 times longer, while
  `{scale_timeouts,0.1}` would shorten them by a factor of 10.

Options in the environment variable EUNIT are also included last in the option
list, i.e., have lower precedence than those in `Options`.

_See also: _`test/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
