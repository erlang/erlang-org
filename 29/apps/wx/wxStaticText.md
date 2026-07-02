# `wxStaticText`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxStaticText.erl#L58)

A static text control displays one or more lines of read-only text.

`m:wxStaticText` supports the three classic text alignments, label ellipsization i.e.
replacing parts of the text with the ellipsis ("...") if the label doesn't fit into the
provided space and also formatting markup with `wxControl::SetLabelMarkup()` (not
implemented in wx).

## Styles

This class supports the following styles:

* wxALIGN_LEFT: Align the text to the left.

* wxALIGN_RIGHT: Align the text to the right.

* wxALIGN_CENTRE_HORIZONTAL: Center the text (horizontally).

* wxST_NO_AUTORESIZE: By default, the control will adjust its size to exactly fit to the
size of the text when `setLabel/2` is called. If this style flag is given, the control will not change
its size (this style is especially useful with controls which also have the `wxALIGN_RIGHT`
or the `wxALIGN_CENTRE_HORIZONTAL` style because otherwise they won't make sense any
longer after a call to `setLabel/2`).

* wxST_ELLIPSIZE_START: If the labeltext width exceeds the control width, replace the
beginning of the label with an ellipsis; uses `wxControl::Ellipsize` (not implemented in
wx).

* wxST_ELLIPSIZE_MIDDLE: If the label text width exceeds the control width, replace the
middle of the label with an ellipsis; uses `wxControl::Ellipsize` (not implemented in wx).

* wxST_ELLIPSIZE_END: If the label text width exceeds the control width, replace the end of
the label with an ellipsis; uses `wxControl::Ellipsize` (not implemented in wx).

See:
* `m:wxStaticBitmap`

* `m:wxStaticBox`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStaticText](https://docs.wxwidgets.org/3.2/classwx_static_text.html)

# `wxStaticText`

```erlang
-type wxStaticText() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxStaticText(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxStaticText(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creation function, for two-step construction.

For details see `new/4`.

# `destroy`

```erlang
-spec destroy(This :: wxStaticText()) -> ok.
```

Destroys the object

# `getLabel`

```erlang
-spec getLabel(This) -> unicode:charlist() when This :: wxStaticText().
```

Returns the control's label, as it was passed to `wxControl:setLabel/2`.

Note that the returned string may contains mnemonics ("&" characters) if they were passed
to the `wxControl:setLabel/2` function; use `GetLabelText()` (not implemented in wx) if they are undesired.

Also note that the returned string is always the string which was passed to `wxControl:setLabel/2` but may be
different from the string passed to `SetLabelText()` (not implemented in wx) (since this
last one escapes mnemonic characters).

# `new`

```erlang
-spec new() -> wxStaticText().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id, Label) -> wxStaticText()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Id, Label, [Option]) -> wxStaticText()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating and showing a text control.

See: `create/5`

# `setLabel`

```erlang
-spec setLabel(This, Label) -> ok when This :: wxStaticText(), Label :: unicode:chardata().
```

Change the label shown in the control.

Notice that since wxWidgets 3.1.1 this function is guaranteed not to do anything if the
label didn't really change, so there is no benefit to checking if the new label is
different from the current one in the application code.

See: `wxControl:setLabel/2`

# `wrap`

```erlang
-spec wrap(This, Width) -> ok when This :: wxStaticText(), Width :: integer().
```

This functions wraps the controls label so that each of its lines becomes at most `width`
pixels wide if possible (the lines are broken at words boundaries so it might not be the
case if words are too long).

If `width` is negative, no wrapping is done. Note that this width is not necessarily the
total width of the control, since a few pixels for the border (depending on the controls
border style) may be added.

Since: 2.6.2

---

*Consult [api-reference.md](api-reference.md) for complete listing*
