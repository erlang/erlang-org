# `crashdump_viewer`
[🔗](https://github.com/erlang/otp/blob/master/lib/observer/src/crashdump_viewer.erl#L22)

A WxWidgets based tool for browsing Erlang crashdumps.

For details about how to get started with the Crashdump Viewer, see the
[User's Guide](crashdump_ug.md).

# `start`

```erlang
-spec start() -> ok | {error, term()}.
```

Starts the Crashdump Viewer GUI and opens a file dialog where the
crashdump can be selected.

# `start`
*since OTP 17.0* 

```erlang
-spec start(File :: string()) -> ok | {error, term()}.
```

Starts the Crashdump Viewer GUI and loads the specified crashdump.

# `stop`

```erlang
-spec stop() -> ok.
```

Terminates the Crashdump Viewer and closes all GUI windows.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
