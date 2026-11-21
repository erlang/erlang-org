# `debugger`
[🔗](https://github.com/erlang/otp/blob/master/lib/debugger/src/debugger.erl#L22)

The Erlang Debugger for debugging and testing of Erlang programs.

# `quick`

```erlang
-spec quick(Module, Name, Args) -> term() when Module :: atom(), Name :: atom(), Args :: [term()].
```

Debugs a single process.

The module `Module` is interpreted and
[`apply(Module, Name, Args)`](`apply/3`) is called. This opens an "Attach
Process" window. For details, see the
[User's Guide](debugger_chapter.md).

# `start`

```erlang
-spec start() -> term().
```

Starts Debugger.

Started by this function, Debugger interprets code on all known nodes.

# `start`

```erlang
-spec start(Mode) -> term() when Mode :: local | global | wx;
           (File) -> term() when File :: string().
```

Starts Debugger.

If `ModeOrFile` is a string, it is assumed to be the name of a file,
and Debugger tries to load its settings from this file. For details
about settings, see the [User's Guide](debugger_chapter.md).

If `ModeOrFile` is atom `local`, Debugger interprets code only at the
current node. If `ModeOrFile` is `global`, Debugger interprets code on
all known nodes.

# `start`

```erlang
-spec start(Mode, File) -> term() when Mode :: local | global, File :: string().
```

Starts Debugger.

Debugger tries to load its settings from the file named by `File`.
For details about settings, see the [User's Guide](debugger_chapter.md).

If `Mode` is `local`, Debugger interprets code only on the current
node. If `Mode` is `global`, Debugger interprets code on all known
nodes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
