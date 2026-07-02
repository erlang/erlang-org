# `wxPasswordEntryDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxPasswordEntryDialog.erl#L58)

This class represents a dialog that requests a one-line password string from the user.

It is implemented as a generic wxWidgets dialog.

See: [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_password)

This class is derived, and can use functions, from:

* `m:wxTextEntryDialog`

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPasswordEntryDialog](https://docs.wxwidgets.org/3.2/classwx_password_entry_dialog.html)

# `wxPasswordEntryDialog`

```erlang
-type wxPasswordEntryDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPasswordEntryDialog()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Parent, Message) -> wxPasswordEntryDialog()
             when Parent :: wxWindow:wxWindow(), Message :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Message, [Option]) -> wxPasswordEntryDialog()
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

---

*Consult [api-reference.md](api-reference.md) for complete listing*
