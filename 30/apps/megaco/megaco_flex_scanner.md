# `megaco_flex_scanner`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/flex/megaco_flex_scanner.erl#L27)

Interface module to the flex scanner linked in driver.

This module contains the public interface to the flex scanner linked in driver.
The flex scanner performs the scanning phase of text message decoding.

The flex scanner is written using a tool called _flex_. In order to be able to
compile the flex scanner driver, this tool has to be available.

By default the flex scanner reports line-number of an error. But it can be built
without line-number reporting. Instead token number is used. This will speed up
the scanning some 5-10%. Use `--disable-megaco-flex-scanner-lineno` when
configuring the application.

The scanner will, by default, be built as a reentrant scanner _if_ the flex
utility supports this (it depends on the version of flex). It is possible to
explicitly disable this even when flex support this. Use
`--disable-megaco-reentrant-flex-scanner` when configuring the application.

# `megaco_ports`

```erlang
-type megaco_ports() :: port() | tuple().
```

Return value of a successful (flex) scanner start.

# `is_reentrant_enabled`

```erlang
-spec is_reentrant_enabled() -> boolean().
```

Is the flex scanner reentrant or not.

# `is_scanner_port`

```erlang
-spec is_scanner_port(Port, PortOrPorts) -> boolean() when Port :: port(), PortOrPorts :: megaco_ports().
```

Checks if a port is a flex scanner port or not (useful when if a port exits).

# `scan`

```erlang
-spec scan(Binary, PortOrPorts) -> {ok, Tokens, Version, LatestLine} | {error, Reason, LatestLine}
              when
                  Binary :: binary(),
                  PortOrPorts :: megaco_ports(),
                  Tokens :: list(),
                  Version :: megaco_encoder:protocol_version(),
                  LatestLine :: non_neg_integer(),
                  Reason :: term().
```

Scans a megaco message and generates a token list to be passed on the parser.

# `start`

```erlang
-spec start() -> {ok, PortOrPorts} | {error, Reason}
               when PortOrPorts :: megaco_ports(), Reason :: term().
```

This function is used to start the flex scanner. It locates the library and
loads the linked in driver.

On a single core system or if it's a non-reentrant scanner, a single port is
created. On a multi-core system with a reentrant scanner, several ports will be
created (one for each scheduler).

Note that the process that calls this function _must_ be permanent. If it dies,
the port(s) will exit and the driver unload.

# `stop`

```erlang
-spec stop(PortOrPorts) -> stopped when PortOrPorts :: megaco_ports().
```

This function is used to stop the flex scanner. It also unloads the driver.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
