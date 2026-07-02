# `wxSingleChoiceDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSingleChoiceDialog.erl#L58)

This class represents a dialog that shows a list of strings, and allows the user to
select one.

Double-clicking on a list item is equivalent to single-clicking and then pressing OK.

## Styles

This class supports the following styles:

* wxOK: Show an OK button.

* wxCANCEL: Show a Cancel button.

* wxCENTRE: Centre the message.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_singlechoice)

* `m:wxMultiChoiceDialog`

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSingleChoiceDialog](https://docs.wxwidgets.org/3.2/classwx_single_choice_dialog.html)

# `wxSingleChoiceDialog`

```erlang
-type wxSingleChoiceDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxSingleChoiceDialog()) -> ok.
```

Destroys the object

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxSingleChoiceDialog().
```

Returns the index of selected item.

# `getStringSelection`

```erlang
-spec getStringSelection(This) -> unicode:charlist() when This :: wxSingleChoiceDialog().
```

Returns the selected string.

# `new`

```erlang
-spec new(Parent, Message, Caption, Choices) -> wxSingleChoiceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Caption :: unicode:chardata(),
                 Choices :: [unicode:chardata()].
```

# `new`

```erlang
-spec new(Parent, Message, Caption, Choices, [Option]) -> wxSingleChoiceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Caption :: unicode:chardata(),
                 Choices :: [unicode:chardata()],
                 Option :: {style, integer()} | {pos, {X :: integer(), Y :: integer()}}.
```

Constructor, taking an array of `wxString` (not implemented in wx) choices and optional
client data.

Remark: Use `wxDialog:showModal/1` to show the dialog.

# `setSelection`

```erlang
-spec setSelection(This, Selection) -> ok when This :: wxSingleChoiceDialog(), Selection :: integer().
```

Sets the index of the initially selected item.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
