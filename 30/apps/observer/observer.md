# `observer`
[🔗](https://github.com/erlang/otp/blob/master/lib/observer/src/observer.erl#L22)

A GUI tool for observing an Erlang system.

Observer is a graphical tool for observing the characteristics of Erlang
systems. The tool Observer displays system information, application supervisor
trees, process information, ETS tables, Mnesia tables, and contains a front end
for Erlang tracing with module `m:ttb`.

For details about how to get started, see the [`User's Guide`](observer_ug.md).

# `start`
*since OTP R15B* 

```erlang
-spec start() -> ok | {error, term()}.
```

Starts the Observer GUI. To stop the tool, close the window or call `stop/0`.

# `start`
*since OTP 26.0* 

```erlang
-spec start(Node :: node() | [node()]) -> ok | {error, term()}.
```

Starts the Observer GUI and tries to connect it to `Node`.

# `start_and_wait`
*since OTP 26.0* 

```erlang
-spec start_and_wait() -> ok.
```

Starts the Observer GUI and only return when it is either stopped or the window
is closed

# `start_and_wait`
*since OTP 26.0* 

```erlang
-spec start_and_wait(Node :: node() | [node()]) -> ok.
```

Starts the Observer GUI and only return when it is either stopped or the window
is closed, connects it directly to `Node` like `start/1`.

# `stop`
*since OTP 26.0* 

```erlang
-spec stop() -> ok.
```

Stops the Observer GUI.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
