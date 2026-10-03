# `wxControl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxControl.erl#L58)

This is the base class for a control or "widget".

A control is generally a small window which processes user input and/or displays one or
more item of data.

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxControl](https://docs.wxwidgets.org/3.2/classwx_control.html)

## Events

Event types emitted from this class:

* [`command_text_copy`](`m:wxClipboardTextEvent`)

* [`command_text_cut`](`m:wxClipboardTextEvent`)

* [`command_text_paste`](`m:wxClipboardTextEvent`)

# `wxControl`

```erlang
-type wxControl() :: wx:wx_object().
```

# `getLabel`

```erlang
-spec getLabel(This) -> unicode:charlist() when This :: wxControl().
```

Returns the control's label, as it was passed to `setLabel/2`.

Note that the returned string may contains mnemonics ("&" characters) if they were passed
to the `setLabel/2` function; use `GetLabelText()` (not implemented in wx) if they are undesired.

Also note that the returned string is always the string which was passed to `setLabel/2` but may be
different from the string passed to `SetLabelText()` (not implemented in wx) (since this
last one escapes mnemonic characters).

# `setLabel`

```erlang
-spec setLabel(This, Label) -> ok when This :: wxControl(), Label :: unicode:chardata().
```

Sets the control's label.

All "&" characters in the `label` are special and indicate that the following character
is a `mnemonic` for this control and can be used to activate it from the keyboard
(typically by using `Alt` key in combination with it). To insert a literal ampersand
character, you need to double it, i.e. use "&&". If this behaviour is undesirable, use `SetLabelText()`
(not implemented in wx) instead.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
