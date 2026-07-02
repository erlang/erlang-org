# `wxAcceleratorEntry`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxAcceleratorEntry.erl#L58)

An object used by an application wishing to create an accelerator table (see `m:wxAcceleratorTable`).

See:
* `m:wxAcceleratorTable`

* `wxWindow:setAcceleratorTable/2`

wxWidgets docs: [wxAcceleratorEntry](https://docs.wxwidgets.org/3.2/classwx_accelerator_entry.html)

# `wxAcceleratorEntry`

```erlang
-type wxAcceleratorEntry() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxAcceleratorEntry()) -> ok.
```

Destroys the object

# `getCommand`

```erlang
-spec getCommand(This) -> integer() when This :: wxAcceleratorEntry().
```

Returns the command identifier for the accelerator table entry.

# `getFlags`

```erlang
-spec getFlags(This) -> integer() when This :: wxAcceleratorEntry().
```

Returns the flags for the accelerator table entry.

# `getKeyCode`

```erlang
-spec getKeyCode(This) -> integer() when This :: wxAcceleratorEntry().
```

Returns the keycode for the accelerator table entry.

# `new`

```erlang
-spec new() -> wxAcceleratorEntry().
```

# `new`

```erlang
-spec new([Option]) -> wxAcceleratorEntry()
             when
                 Option ::
                     {flags, integer()} |
                     {keyCode, integer()} |
                     {cmd, integer()} |
                     {item, wxMenuItem:wxMenuItem()};
         (Entry) -> wxAcceleratorEntry() when Entry :: wxAcceleratorEntry().
```

Copy ctor.

# `set`

```erlang
-spec set(This, Flags, KeyCode, Cmd) -> ok
             when
                 This :: wxAcceleratorEntry(),
                 Flags :: integer(),
                 KeyCode :: integer(),
                 Cmd :: integer().
```

# `set`

```erlang
-spec set(This, Flags, KeyCode, Cmd, [Option]) -> ok
             when
                 This :: wxAcceleratorEntry(),
                 Flags :: integer(),
                 KeyCode :: integer(),
                 Cmd :: integer(),
                 Option :: {item, wxMenuItem:wxMenuItem()}.
```

Sets the accelerator entry parameters.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
