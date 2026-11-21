# `etop`
[🔗](https://github.com/erlang/otp/blob/master/lib/observer/src/etop.erl#L22)

Erlang Top is a tool for presenting information about Erlang processes similar
to the information presented by "top" in UNIX.

Start Erlang Top with the provided scripts `etop`. This starts a hidden Erlang
node that connects to the node to be measured. The measured node is specified
with option `-node`. If the measured node has a different cookie than the
default cookie for the user who invokes the script, the cookie must be
explicitly specified with option `-setcookie`.

Under Windows, batch file `etop.bat` can be used.

When executing the `etop` script, configuration parameters can be specified as
command-line options, for example,
`etop -node testnode@myhost -setcookie MyCookie`. The following configuration
parameters exist for the tool:

- **`node`** - The measured node.

  Value: `t:atom/0`

  Mandatory

- **`setcookie`** - Cookie to use for the `etop` node. Must be same as the
  cookie on the measured node.

  Value: `t:atom/0`

- **`lines`** - Number of lines (processes) to display.

  Value: `t:integer/0`

  Default: `10`

- **`interval`** - Time interval (in seconds) between each update of the
  display.

  Value: `t:integer/0`

  Default: `5`

- **`accumulate`** - If `true`, the execution time and reductions are
  accumulated.

  Value: `t:boolean/0`

  Default: `false`

- **`sort`** - Identifies what information to sort by.

  Value: `runtime | reductions | memory | msg_q`

  Default: `runtime` (`reductions` if `tracing=off`)

- **`tracing`** - `etop` uses the Erlang trace facility, and thus no other
  tracing is possible on the measured node while `etop` is running, unless this
  option is set to `off`. Also helpful if the `etop` tracing causes too high
  load on the measured node. With tracing off, runtime is not measured.

  Value: `on | off`

  Default: `on`

For details about Erlang Top, see the [User's Guide](etop_ug.md).

# `config`

```erlang
-spec config(Key, Value) -> ok | {error, Reason}
                when Key :: lines | interval | accumulate | sort, Value :: term(), Reason :: term().
```

Changes the configuration parameters of the tool during runtime. Allowed
parameters are `lines`, `interval`, `accumulate`, and `sort`.

# `dump`

```erlang
-spec dump(File) -> ok | {error, Reason} when File :: file:filename_all(), Reason :: term().
```

Dumps the current display to a text file.

# `help`
*since OTP R15B01* 

```erlang
-spec help() -> ok.
```

Displays the help of `etop` and its options.

# `start`
*since OTP R15B01* 

```erlang
-spec start() -> ok.
```

Starts `etop`. Notice that `etop` is preferably started with the `etop` script.

# `start`
*since OTP R15B01* 

```erlang
-spec start(Options) -> ok when Options :: [{Key, Value}], Key :: atom(), Value :: term().
```

Starts `etop`. To view the possible options, use `help/0`.

# `stop`

```erlang
-spec stop() -> stop | not_started.
```

Terminates `etop`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
