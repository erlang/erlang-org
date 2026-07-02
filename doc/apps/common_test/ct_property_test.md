# `ct_property_test`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/common_test/src/ct_property_test.erl#L23)

Support in Common Test for running property-based tests.

This module helps running property-based tests in the `Common Test` framework.
One (or more) of the property testing tools

- [QuickCheck](http://www.quviq.com),
- [PropEr](https://proper-testing.github.io) or
- [Triq](https://github.com/krestenkrab/triq)

is assumed to be installed.

The idea with this module is to have a `Common Test` test suite calling a
property testing tool with special property test suites as defined by that tool.
The tests are collected in the `test` directory of the application. The `test`
directory has a subdirectory `property_test`, where everything needed for the
property tests are collected. The usual Erlang application directory structure
is assumed.

A typical `Common Test` test suite using `ct_property_test` is organized as
follows:

```erlang
-module(my_prop_test_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [prop_ftp_case].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

%%%---- test case
prop_ftp_case(Config) ->
    ct_property_test:quickcheck(
      ftp_simple_client_server:prop_ftp(),
      Config
     ).
```

and the the property test module (in this example
`ftp_simple_client_server.erl`) as almost a usual property testing module (More
examples are in [the User's Guide](ct_property_test_chapter.md)):

```erlang
-module(ftp_simple_client_server).
-export([prop_ftp/0...]).

-include_lib("common_test/include/ct_property_test.hrl").

prop_ftp() ->
    ?FORALL( ....
```

# `arguments`
*not exported* *since OTP 17.3* 

```erlang
-type arguments() :: [term()].
```

# `command`
*not exported* *since OTP 17.3* 

```erlang
-type command() :: set_command() | init_command().
```

# `command_list`
*not exported* *since OTP 17.3* 

```erlang
-type command_list() :: [command()].
```

# `dynamic_state`
*not exported* *since OTP 17.3* 

```erlang
-type dynamic_state() :: term().
```

# `function_name`
*not exported* *since OTP 17.3* 

```erlang
-type function_name() :: atom().
```

# `history`
*not exported* *since OTP 17.3* 

```erlang
-type history() :: [term()].
```

# `init_command`
*not exported* *since OTP 17.3* 

```erlang
-type init_command() :: {init, symbolic_state()}.
```

# `parallel_testcase`
*not exported* *since OTP 17.3* 

```erlang
-type parallel_testcase() :: {command_list(), [command_list()]}.
```

# `set_command`
*not exported* *since OTP 17.3* 

```erlang
-type set_command() :: {set, symbolic_var(), symbolic_call()}.
```

# `statem_result`
*not exported* *since OTP 17.3* 

```erlang
-type statem_result() :: ok | term().
```

# `symbolic_call`
*not exported* *since OTP 17.3* 

```erlang
-type symbolic_call() :: {call, module(), function_name(), arguments()}.
```

# `symbolic_state`
*not exported* *since OTP 17.3* 

```erlang
-type symbolic_state() :: term().
```

# `symbolic_var`
*not exported* *since OTP 17.3* 

```erlang
-type symbolic_var() :: {var, pos_integer()}.
```

# `cmnd_names`
*since OTP 27.1* 

```erlang
-spec cmnd_names(Cs) -> Result
                    when Cs :: command_list() | parallel_testcase(), Result :: [function_name()].
```

Returns a list of commands (function calls) generated in the `Cmnd` sequence,
without Module, Arguments and other details.

For more information see: `present_result/5`.

# `init_per_suite`
*since OTP 17.3* 

```erlang
-spec init_per_suite(Config) -> Config | {skip, Reason} | {fail, Reason}
                        when Config :: proplists:proplist(), Reason :: string().
```

Initializes and extends `Config` for property based testing.

This function investigates if support is available for either
[QuickCheck](http://www.quviq.com), [PropEr](https://proper-testing.github.io)
or [Triq](https://github.com/krestenkrab/triq) and compiles the properties with
the first tool found. It is supposed to be called in the
[`init_per_suite/1`](`init_per_suite/1`) function in a CommonTest test suite.

Which tools to check for, and in which order could be set with the option
`{prop_tools, list(eqc|proper|triq)}` in the CommonTest configuration `Config`.
The default value is `[eqc, proper, triq]` with `eqc` being the first one
searched for.

If no support is found for any tool, this function returns
`{skip, Explanation}`.

In case of other errors, this function returns
`{fail, Explanation}`.

If support is found, the option `{property_test_tool,ToolModule}` with the
selected tool main module name (`eqc`, `proper` or `triq`) is added to the list
`Config` which then is returned.

The property tests are assumed to be in a subdirectory named `property_test`.
All found Erlang files in that directory are compiled with one of the macros
`'EQC'`, `'PROPER'` or `'TRIQ'` set, depending on which tool that is first
found. This could make parts of the Erlang property tests code to be included or
excluded with the macro directives `-ifdef(Macro).` or `-ifndef(Macro).`.

The file(s) in the `property_test` subdirectory could, or should, include the
ct_property_test include file:

```erlang
-include_lib("common_test/include/ct_property_test.hrl").
```

This included file will:

- Include the correct tool's include file
- Set the macro `'MOD_eqc'` to the correct module name for the selected tool.
  That is, the macro `'MOD_eqc'` is set to either `eqc`, `proper` or `triq`.

# `num_calls`
*since OTP 27.1* 

```erlang
-spec num_calls(Cs) -> Result
                   when Cs :: command_list() | parallel_testcase(), Result :: [non_neg_integer()].
```

Returns number of command calls in a test case.

For more information see: `present_result/5`.

# `present_result`
*since OTP 22.3* 

```erlang
-spec present_result(Module, Cmds, Triple, Config) -> boolean()
                        when
                            Module :: module(),
                            Cmds :: command_list() | parallel_testcase(),
                            Triple :: {H, Sf, Result},
                            H :: history(),
                            Sf :: dynamic_state(),
                            Result :: statem_result(),
                            Config :: proplists:proplist().
```

# `present_result`
*since OTP 22.3* 

```erlang
-spec present_result(Module, Cmds, Triple, Config, Options0) -> boolean()
                        when
                            Module :: module(),
                            Cmds :: command_list() | parallel_testcase(),
                            Triple :: {H, Sf, Result},
                            H :: history(),
                            Sf :: dynamic_state(),
                            Result :: statem_result(),
                            Config :: proplists:proplist(),
                            Options0 :: proplists:proplist().
```

Presents the result of _stateful (statem) property testing_ using the aggregate
function in PropEr, QuickCheck or other similar property testing tool.

It is assumed to be called inside the property called by `quickcheck/2`:

```erlang
...
RunResult = run_parallel_commands(?MODULE, Cmds),
ct_property_test:present_result(?MODULE, Cmds, RunResult, Config)
...
```

See the [User's Guide](ct_property_test_chapter.md#stateful1) for an example of
the usage and of the default printout.

The `StatisticsSpec` is a list of the tuples:

- `{Title::string(), CollectFun::fun/1}`
- `{Title::string(), FrequencyFun::/0, CollectFun::fun/1}`

Each tuple will produce one table in the order of their places in the list.

- `Title` will be the title of one result table
- `CollectFun` is called with one argument: the `Cmds`. It should return a list
  of the values to be counted. The following pre-defined functions exist:
  - `ct_property_test:cmnd_names/1` returns a list of commands (function calls)
    generated in the `Cmnd` sequence, without Module, Arguments and other
    details.
  - `ct_property_test:num_calls/1` returns a list of the length of commands
    lists
  - `ct_property_test:sequential_parallel/1` returns a list with information
    about sequential and parallel parts from `Tool:parallel_commands/1,2`
- `FrequencyFun/0` returns a fun/1 which is supposed to take a list of items as
  input, and return an iolist which will be printed as the table. Per default,
  the number of each item is counted and the percentage is printed for each. The
  list \[a,b,a,a,c] could for example return

  ```erlang
  ["a 60%\n","b 20%\n","c 20%\n"]
  ```

  which will be printed by the `print_fun`. The default `print_fun` will print
  it as:

  ```text
  a 60%
  b 20%
  c 20%
  ```

The default `StatisticsSpec` is:

- For sequential commands:

  ```erlang
  [{"Function calls", fun cmnd_names/1},
   {"Length of command sequences", fun print_frequency_ranges/0,
                                                    fun num_calls/1}]
  ```

- For parallel commands:

  ```erlang
  [{"Distribution sequential/parallel", fun sequential_parallel/1},
   {"Function calls", fun cmnd_names/1},
   {"Length of command sequences", fun print_frequency_ranges/0,
                                                    fun num_calls/1}]
  ```

# `quickcheck`
*since OTP 17.3* 

```erlang
-spec quickcheck(Property, Config) -> true | {fail, Reason}
                    when Property :: term(), Config :: proplists:proplist(), Reason :: term().
```

Calls the selected tool's function for running the `Property`. It is usually and
by historical reasons called quickcheck, and that is why that name is used in
this module (`ct_property_test`).

The result is returned in a form suitable for `Common Test` test suites.

This function is intended to be called in test cases in test suites.

# `sequential_parallel`
*since OTP 27.1* 

```erlang
-spec sequential_parallel(Cs) -> Result
                             when Cs :: command_list() | parallel_testcase(), Result :: [atom()].
```

Returns a list with information about sequential and parallel parts.

For more information see: `present_result/5`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
