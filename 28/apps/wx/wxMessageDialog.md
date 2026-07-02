# `wxMessageDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMessageDialog.erl#L58)

This class represents a dialog that shows a single or multi-line message, with a choice
of OK, Yes, No and Cancel buttons.

## Styles

This class supports the following styles:

* wxOK: Puts an Ok button in the message box. May be combined with `wxCANCEL`.

* wxCANCEL: Puts a Cancel button in the message box. Must be combined with either `wxOK` or `wxYES_NO`.

* wxYES_NO: Puts Yes and No buttons in the message box. It is recommended to always use `wxCANCEL`
with this style as otherwise the message box won't have a close button under wxMSW and
the user will be forced to answer it.

* wxHELP: Puts a Help button to the message box. This button can have special appearance or
be specially positioned if its label is not changed from the default one. Notice that
using this button is not supported when showing a message box from non-main thread in
wxOSX/Cocoa. Available since wxWidgets 2.9.3.

* wxNO_DEFAULT: Makes the "No" button default, can only be used with `wxYES_NO`.

* wxCANCEL_DEFAULT: Makes the "Cancel" button default, can only be used with `wxCANCEL`.
This style is currently not supported (and ignored) in wxOSX.

* wxYES_DEFAULT: Makes the "Yes" button default, this is the default behaviour and this
flag exists solely for symmetry with `wxNO_DEFAULT`.

* wxOK_DEFAULT: Makes the "OK" button default, this is the default behaviour and this flag
exists solely for symmetry with `wxCANCEL_DEFAULT`.

* wxICON_NONE: Displays no icon in the dialog if possible (an icon might still be displayed
if the current platform mandates its use). This style may be used to prevent the dialog
from using the default icon based on `wxYES_NO` presence as explained in `wxICON_QUESTION`
and `wxICON_INFORMATION` documentation below.

* wxICON_ERROR: Displays an error icon in the dialog.

* wxICON_WARNING: Displays a warning icon in the dialog. This style should be used for
informative warnings or, in combination with `wxYES_NO` or `wxCANCEL`, for questions that
have potentially serious consequences (caution icon is used on macOS in this case).

* wxICON_QUESTION: Displays a question mark symbol. This icon is automatically used with `wxYES_NO`
so it's usually unnecessary to specify it explicitly. This style is not supported for
message dialogs under wxMSW when a task dialog is used to implement them (i.e. when
running under Windows Vista or later) because [Microsoft guidelines](https://docs.microsoft.com/en-us/windows/desktop/uxguide/mess-confirm)
indicate that no icon should be used for routine confirmations. If it is specified, no
icon will be displayed.

* wxICON_INFORMATION: Displays an information symbol. This icon is used by default if `wxYES_NO`
is not given so it is usually unnecessary to specify it explicitly.

* wxICON_EXCLAMATION: Alias for `wxICON_WARNING`.

* wxICON_HAND: Alias for `wxICON_ERROR`.

* wxICON_AUTH_NEEDED: Displays an authentication needed symbol. This style is only
supported for message dialogs under wxMSW when a task dialog is used to implement them
(i.e. when running under Windows Vista or later). In other cases the default icon
selection logic will be used. Note this can be combined with other styles to provide a
fallback. For instance, using wxICON_AUTH_NEEDED | wxICON_QUESTION will show a shield
symbol on Windows Vista or above and a question symbol on other platforms. Available since
wxWidgets 2.9.5

* wxSTAY_ON_TOP: Makes the message box stay on top of all other windows and not only just
its parent (currently implemented only under MSW and GTK).

* wxCENTRE: Centre the message box on its parent or on the screen if parent is not
specified. Setting this style under MSW makes no differences as the dialog is always
centered on the parent.

See: [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_msg)

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMessageDialog](https://docs.wxwidgets.org/3.2/classwx_message_dialog.html)

# `wxMessageDialog`

```elixir
-type wxMessageDialog() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxMessageDialog()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new(Parent, Message) -> wxMessageDialog()
             when Parent :: wxWindow:wxWindow(), Message :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Message, [Option]) -> wxMessageDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Message :: unicode:chardata(),
                 Option ::
                     {caption, unicode:chardata()} |
                     {style, integer()} |
                     {pos, {X :: integer(), Y :: integer()}}.
```

Constructor specifying the message box properties.

Use `wxDialog:showModal/1` to show the dialog.

`style` may be a bit list of the identifiers described above.

Notice that not all styles are compatible: only one of `wxOK` and `wxYES_NO` may be
specified (and one of them must be specified) and at most one default button style can be
used and it is only valid if the corresponding button is shown in the message box.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
