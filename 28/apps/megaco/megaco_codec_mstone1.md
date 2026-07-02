# `megaco_codec_mstone1`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/megaco/examples/meas/megaco_codec_mstone1.erl#L29)

This module implements a simple megaco codec-based performance tool.

This module implements the _mstone1_ tool, a simple megaco codec-based
performance tool.

The results, the mstone value(s), are written to stdout.

_Note_ that this module is _not_ included in the runtime part of the
application.

# `start`

# `start`

This function is intended to be called from the _mstone1_ script, which
uses the '-s' arguments to run the function (argument order; message package,
run time (in minutes in the example) and factor):

```text
erl -s megaco_codec_mstone1 start time_test 1 1
```

# `start`

```elixir
-spec start(RunTime, Factor) -> ok when RunTime :: pos_integer(), Factor :: default | pos_integer();
           (MessagePackage, Factor) -> ok when MessagePackage :: atom(), Factor :: pos_integer().
```

This function starts the _mstone1_ performance test with all codec configs.
`Factor` (defaults to `1`) processes are started for every supported codec
config.

Each process encodes and decodes their messages. The number of messages
processed in total (for all processes) is the mstone value.

# `start_flex`

# `start_flex`

This function is intended to be called from the _mstone1_ script, which
uses the '-s' arguments to run the function (argument order; message package,
run time (in minutes in the example) and factor):

```text
erl -s megaco_codec_mstone1 start_flex time_test 1 1
```

# `start_flex`

```elixir
-spec start_flex(MessagePackage, Factor) -> ok when MessagePackage :: atom(), Factor :: pos_integer().
```

This function starts the _mstone1_ performance test with only the flex codec
configs (i.e. `pretty` and `compact` with `flex`). The same number of processes
are started as when running the standard test (using the `start/0,1` function).
Each process encodes and decodes their messages. The number of messages
processed in total (for all processes) is the mstone value.

# `start_no_drv`

# `start_no_drv`

This function is intended to be called from the _mstone1_ script, which
uses the '-s' arguments to run the function (argument order; message package,
run time (in minutes in the example) and factor):

```text
erl -s megaco_codec_mstone1 start_no_drv time_test 1 1
```

# `start_no_drv`

```elixir
-spec start_no_drv(MessagePackage, Factor) -> ok when MessagePackage :: atom(), Factor :: pos_integer().
```

This function starts the _mstone1_ performance test with codec configs not using
any drivers (i.e. `pretty` and `compact` without `flex`, `ber` and `per` without
`driver` and `erlang` without `compressed`). The same number of processes are
started as when running the standard test (using the `start/0,1` function). Each
process encodes and decodes their messages. The number of messages processed in
total (for all processes) is the mstone value.

# `start_only_drv`

# `start_only_drv`

start_no_drv([MessagePackage, RunTime, Factor])

This function is intended to be called from the _mstone1_ script, which
uses the '-s' arguments to run the function (argument order; message package,
run time (in minutes in the example) and factor):

```text
erl -s megaco_codec_mstone1 start_no_drv time_test 1 1
```

# `start_only_drv`

```elixir
-spec start_only_drv(MessagePackage, Factor) -> ok
                        when MessagePackage :: atom(), Factor :: pos_integer().
```

This function starts the _mstone1_ performance test with only the driver using
codec configs (i.e. `pretty` and `compact` with `flex`, and `ber` and `per` with
`driver` and `erlang` with `compressed`). The same number of processes are
started as when running the standard test (using the `start/0,1` function). Each
process encodes and decodes their messages. The number of messages processed in
total (for all processes) is the mstone value.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
