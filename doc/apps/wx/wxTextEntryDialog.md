# `wxTextEntryDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxTextEntryDialog.erl#L58)

This class represents a dialog that requests a one-line text string from the user.

It is implemented as a generic wxWidgets dialog.

See: [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_textentry)

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTextEntryDialog](https://docs.wxwidgets.org/3.2/classwx_text_entry_dialog.html)

# `wxTextEntryDialog`

```erlang
-type wxTextEntryDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxTextEntryDialog()) -> ok.
```

Destroys the object

# `getValue`

```erlang
-spec getValue(This) -> unicode:charlist() when This :: wxTextEntryDialog().
```

Returns the text that the user has entered if the user has pressed OK, or the original
value if the user has pressed Cancel.

# `new`

```erlang
-spec new() -> wxTextEntryDialog().
```

Default constructor.

Call `Create()` (not implemented in wx) to really create the dialog later.

Since: 2.9.5

# `new`

```erlang
-spec new(Parent, Message) -> wxTextEntryDialog()
             when Parent :: wxWindow:wxWindow(), Message :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Message, [Option]) -> wxTextEntryDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Option ::
                     {caption, unicode:chardata()} |
                     {value, unicode:chardata()} |
                     {style, integer()} |
                     {pos, {X :: integer(), Y :: integer()}}.
```

Constructor.

Use `wxDialog:showModal/1` to show the dialog.

See `Create()` (not implemented in wx) method for parameter description.

# `setValue`

```erlang
-spec setValue(This, Value) -> ok when This :: wxTextEntryDialog(), Value :: unicode:chardata().
```

Sets the default text value.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
