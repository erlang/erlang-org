# `wxFontDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFontDialog.erl#L58)

This class represents the font chooser dialog.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_font)

* `m:wxFontData`

* ?wxGetFontFromUser()

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFontDialog](https://docs.wxwidgets.org/3.2/classwx_font_dialog.html)

# `wxFontDialog`

```erlang
-type wxFontDialog() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Data) -> boolean()
                when
                    This :: wxFontDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Data :: wxFontData:wxFontData().
```

Creates the dialog if the `m:wxFontDialog` object had been initialized using the default
constructor.

Return: true on success and false if an error occurred.

# `destroy`

```erlang
-spec destroy(This :: wxFontDialog()) -> ok.
```

Destroys the object

# `getFontData`

```erlang
-spec getFontData(This) -> wxFontData:wxFontData() when This :: wxFontDialog().
```

Returns the `m:wxFontData` associated with the font dialog.

# `new`

```erlang
-spec new() -> wxFontDialog().
```

Default ctor.

`create/3` must be called before the dialog can be shown.

# `new`

```erlang
-spec new(Parent, Data) -> wxFontDialog()
             when Parent :: wxWindow:wxWindow(), Data :: wxFontData:wxFontData().
```

Constructor.

Pass a parent window, and the `m:wxFontData` object to be used to initialize the dialog
controls.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
