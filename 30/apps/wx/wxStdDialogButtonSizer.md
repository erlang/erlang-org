# `wxStdDialogButtonSizer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStdDialogButtonSizer.erl#L58)

This class creates button layouts which conform to the standard button spacing and
ordering defined by the platform or toolkit's user interface guidelines (if such things
exist).

By using this class, you can ensure that all your standard dialogs look correct on all
major platforms. Currently it conforms to the Windows, GTK+ and macOS human interface guidelines.

When there aren't interface guidelines defined for a particular platform or toolkit, `m:wxStdDialogButtonSizer`
reverts to the Windows implementation.

To use this class, first add buttons to the sizer by calling `addButton/2` (or `setAffirmativeButton/2`, `setNegativeButton/2` or `setCancelButton/2`) and then call
Realize in order to create the actual button layout used. Other than these special
operations, this sizer works like any other sizer.

If you add a button with wxID_SAVE, on macOS the button will be renamed to "Save" and the
wxID_NO button will be renamed to "Don't Save" in accordance with the macOS Human
Interface Guidelines.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

* `wxDialog:createButtonSizer/2`

This class is derived, and can use functions, from:

* `m:wxBoxSizer`

* `m:wxSizer`

wxWidgets docs: [wxStdDialogButtonSizer](https://docs.wxwidgets.org/3.2/classwx_std_dialog_button_sizer.html)

# `wxStdDialogButtonSizer`

```erlang
-type wxStdDialogButtonSizer() :: wx:wx_object().
```

# `addButton`

```erlang
-spec addButton(This, Button) -> ok when This :: wxStdDialogButtonSizer(), Button :: wxButton:wxButton().
```

Adds a button to the `m:wxStdDialogButtonSizer`.

The `button` must have one of the following identifiers:

* wxID_OK

* wxID_YES

* wxID_SAVE

* wxID_APPLY

* wxID_CLOSE

* wxID_NO

* wxID_CANCEL

* wxID_HELP

* wxID_CONTEXT_HELP

# `destroy`

```erlang
-spec destroy(This :: wxStdDialogButtonSizer()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxStdDialogButtonSizer().
```

Constructor for a `m:wxStdDialogButtonSizer`.

# `realize`

```erlang
-spec realize(This) -> ok when This :: wxStdDialogButtonSizer().
```

Rearranges the buttons and applies proper spacing between buttons to make them match the
platform or toolkit's interface guidelines.

# `setAffirmativeButton`

```erlang
-spec setAffirmativeButton(This, Button) -> ok
                              when This :: wxStdDialogButtonSizer(), Button :: wxButton:wxButton().
```

Sets the affirmative button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined above.

# `setCancelButton`

```erlang
-spec setCancelButton(This, Button) -> ok
                         when This :: wxStdDialogButtonSizer(), Button :: wxButton:wxButton().
```

Sets the cancel button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined above.

# `setNegativeButton`

```erlang
-spec setNegativeButton(This, Button) -> ok
                           when This :: wxStdDialogButtonSizer(), Button :: wxButton:wxButton().
```

Sets the negative button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined above.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
