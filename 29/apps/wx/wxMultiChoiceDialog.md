# `wxMultiChoiceDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxMultiChoiceDialog.erl#L58)

This class represents a dialog that shows a list of strings, and allows the user to
select one or more.

## Styles

This class supports the following styles:

* wxOK: Show an OK button.

* wxCANCEL: Show a Cancel button.

* wxCENTRE: Centre the message.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_multichoice)

* `m:wxSingleChoiceDialog`

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMultiChoiceDialog](https://docs.wxwidgets.org/3.2/classwx_multi_choice_dialog.html)

# `wxMultiChoiceDialog`

```erlang
-type wxMultiChoiceDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxMultiChoiceDialog()) -> ok.
```

Destroys the object

# `getSelections`

```erlang
-spec getSelections(This) -> [integer()] when This :: wxMultiChoiceDialog().
```

Returns array with indexes of selected items.

# `new`

```erlang
-spec new(Parent, Message, Caption, Choices) -> wxMultiChoiceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Caption :: unicode:chardata(),
                 Choices :: [unicode:chardata()].
```

# `new`

```erlang
-spec new(Parent, Message, Caption, Choices, [Option]) -> wxMultiChoiceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Caption :: unicode:chardata(),
                 Choices :: [unicode:chardata()],
                 Option :: {style, integer()} | {pos, {X :: integer(), Y :: integer()}}.
```

Constructor taking an array of `wxString` (not implemented in wx) choices.

Remark: Use `wxDialog:showModal/1` to show the dialog.

# `setSelections`

```erlang
-spec setSelections(This, Selections) -> ok
                       when This :: wxMultiChoiceDialog(), Selections :: [integer()].
```

Sets selected items from the array of selected items' indexes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
