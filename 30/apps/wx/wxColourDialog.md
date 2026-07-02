# `wxColourDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxColourDialog.erl#L58)

This class represents the colour chooser dialog.

Starting from wxWidgets 3.1.3 and currently in the MSW port only, this dialog generates
wxEVT_COLOUR_CHANGED events while it is being shown, i.e. from inside its `wxDialog:showModal/1` method, that
notify the program about the change of the currently selected colour and allow it to e.g.
preview the effect of selecting this colour. Note that if you react to this event, you
should also correctly revert to the previously selected colour if the dialog is cancelled
by the user.

Example of using this class with dynamic feedback for the selected colour:

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_colour)

* `wx_color()`

* `m:wxColourData`

* ?wxGetColourFromUser()

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxColourDialog](https://docs.wxwidgets.org/3.2/classwx_colour_dialog.html)

# `wxColourDialog`

```erlang
-type wxColourDialog() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxColourDialog(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxColourDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Option :: {data, wxColourData:wxColourData()}.
```

Same as `new/2`.

# `destroy`

```erlang
-spec destroy(This :: wxColourDialog()) -> ok.
```

Destroys the object

# `getColourData`

```erlang
-spec getColourData(This) -> wxColourData:wxColourData() when This :: wxColourDialog().
```

Returns the colour data associated with the colour dialog.

# `new`

```erlang
-spec new() -> wxColourDialog().
```

# `new`

```erlang
-spec new(Parent) -> wxColourDialog() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxColourDialog()
             when Parent :: wxWindow:wxWindow(), Option :: {data, wxColourData:wxColourData()}.
```

Constructor.

Pass a parent window, and optionally a pointer to a block of colour data, which will be
copied to the colour dialog's colour data.

Custom colours from colour data object will be used in the dialog's colour palette.
Invalid entries in custom colours list will be ignored on some platforms(GTK) or replaced
with white colour on platforms where custom colours palette has fixed size (MSW).

See: `m:wxColourData`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
