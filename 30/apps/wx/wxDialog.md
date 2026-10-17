# `wxDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDialog.erl#L58)

A dialog box is a window with a title bar and sometimes a system menu, which can be moved
around the screen.

It can contain controls and other windows and is often used to allow the user to make
some choice or to answer a question.

Dialogs can be made scrollable, automatically, for computers with low resolution screens:
please see overview_dialog_autoscrolling for further details.

Dialogs usually contain either a single button allowing to close the dialog or two
buttons, one accepting the changes and the other one discarding them (such button, if
present, is automatically activated if the user presses the "Esc" key). By default,
buttons with the standard wxID_OK and wxID_CANCEL identifiers behave as expected. Starting
with wxWidgets 2.7 it is also possible to use a button with a different identifier
instead, see `setAffirmativeId/2` and `SetEscapeId()` (not implemented in wx).

Also notice that the `createButtonSizer/2` should be used to create the buttons appropriate for the current
platform and positioned correctly (including their order which is platform-dependent).

Modal and Modeless

There are two kinds of dialog, modal and modeless. A modal dialog blocks program flow and
user input on other windows until it is dismissed, whereas a modeless dialog behaves more
like a frame in that program flow continues, and input in other windows is still possible.
To show a modal dialog you should use the `showModal/1` method while to show a dialog modelessly you
simply use `show/2`, just as with frames.

Note that the modal dialog is one of the very few examples of wxWindow-derived objects
which may be created on the stack and not on the heap. In other words, while most windows
would be created like this:

You can achieve the same result with dialogs by using simpler code:

An application can define a `m:wxCloseEvent` handler for the dialog to respond to system
close events.

## Styles

This class supports the following styles:

* wxCAPTION: Puts a caption on the dialog box.

* wxDEFAULT_DIALOG_STYLE: Equivalent to a combination of wxCAPTION, wxCLOSE_BOX and
wxSYSTEM_MENU (the last one is not used under Unix).

* wxRESIZE_BORDER: Display a resizable frame around the window.

* wxSYSTEM_MENU: Display a system menu.

* wxCLOSE_BOX: Displays a close box on the frame.

* wxMAXIMIZE_BOX: Displays a maximize box on the dialog.

* wxMINIMIZE_BOX: Displays a minimize box on the dialog.

* wxTHICK_FRAME: Display a thick frame around the window.

* wxSTAY_ON_TOP: The dialog stays on top of all other windows.

* wxNO_3D: This style is obsolete and doesn't do anything any more, don't use it in any new
code.

* wxDIALOG_NO_PARENT: By default, a dialog created with a NULL parent window will be given
the `application's top level window` (not implemented in wx) as parent. Use this style to
prevent this from happening and create an orphan dialog. This is not recommended for modal
dialogs.

* wxDIALOG_EX_CONTEXTHELP: Under Windows, puts a query button on the caption. When pressed,
Windows will go into a context-sensitive help mode and wxWidgets will send a `wxEVT_HELP`
event if the user clicked on an application window. Note that this is an extended style
and must be set by calling `wxWindow:setExtraStyle/2` before Create is called (two-step construction).

* wxDIALOG_EX_METAL: On macOS, frames with this style will be shown with a metallic look.
This is an extra style. Under Unix or Linux, MWM (the Motif Window Manager) or other
window managers recognizing the MHM hints should be running for any of these styles to
have an effect.

See:
* [Overview dialog](https://docs.wxwidgets.org/3.2/overview_dialog.html#overview_dialog)

* `m:wxFrame`

* [Overview validator](https://docs.wxwidgets.org/3.2/overview_validator.html#overview_validator)

This class is derived, and can use functions, from:

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxDialog](https://docs.wxwidgets.org/3.2/classwx_dialog.html)

## Events

Event types emitted from this class:

* [`close_window`](`m:wxCloseEvent`)

* [`init_dialog`](`m:wxInitDialogEvent`)

# `wxDialog`

```erlang
-type wxDialog() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Title) -> boolean()
                when
                    This :: wxDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata().
```

# `create`

```erlang
-spec create(This, Parent, Id, Title, [Option]) -> boolean()
                when
                    This :: wxDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Used for two-step dialog box construction.

See: `new/4`

# `createButtonSizer`

```erlang
-spec createButtonSizer(This, Flags) -> wxSizer:wxSizer() when This :: wxDialog(), Flags :: integer().
```

Creates a sizer with standard buttons.

`flags` is a bit list of the following flags: wxOK, wxCANCEL, wxYES, wxNO, wxAPPLY,
wxCLOSE, wxHELP, wxNO_DEFAULT.

The sizer lays out the buttons in a manner appropriate to the platform.

This function uses `createStdDialogButtonSizer/2` internally for most platforms but doesn't create the sizer at all for
the platforms with hardware buttons (such as smartphones) for which it sets up the
hardware buttons appropriately and returns NULL, so don't forget to test that the return
value is valid before using it.

# `createStdDialogButtonSizer`

```erlang
-spec createStdDialogButtonSizer(This, Flags) -> wxStdDialogButtonSizer:wxStdDialogButtonSizer()
                                    when This :: wxDialog(), Flags :: integer().
```

Creates a `m:wxStdDialogButtonSizer` with standard buttons.

`flags` is a bit list of the following flags: wxOK, wxCANCEL, wxYES, wxNO, wxAPPLY,
wxCLOSE, wxHELP, wxNO_DEFAULT.

The sizer lays out the buttons in a manner appropriate to the platform.

# `destroy`

```erlang
-spec destroy(This :: wxDialog()) -> ok.
```

Destroys the object

# `endModal`

```erlang
-spec endModal(This, RetCode) -> ok when This :: wxDialog(), RetCode :: integer().
```

Ends a modal dialog, passing a value to be returned from the `showModal/1` invocation.

See:
* `showModal/1`

* `getReturnCode/1`

* `setReturnCode/2`

# `getAffirmativeId`

```erlang
-spec getAffirmativeId(This) -> integer() when This :: wxDialog().
```

Gets the identifier of the button which works like standard OK button in this dialog.

See: `setAffirmativeId/2`

# `getReturnCode`

```erlang
-spec getReturnCode(This) -> integer() when This :: wxDialog().
```

Gets the return code for this window.

Remark: A return code is normally associated with a modal dialog, where `showModal/1` returns a code
to the application.

See:
* `setReturnCode/2`

* `showModal/1`

* `endModal/2`

# `isModal`

```erlang
-spec isModal(This) -> boolean() when This :: wxDialog().
```

Returns true if the dialog box is modal, false otherwise.

# `new`

```erlang
-spec new() -> wxDialog().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id, Title) -> wxDialog()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Title :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Id, Title, [Option]) -> wxDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Title :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

See: `create/5`

# `setAffirmativeId`

```erlang
-spec setAffirmativeId(This, Id) -> ok when This :: wxDialog(), Id :: integer().
```

Sets the identifier to be used as OK button.

When the button with this identifier is pressed, the dialog calls `wxWindow:validate/1` and `wxWindow:transferDataFromWindow/1` and, if they both
return true, closes the dialog with the affirmative id return code.

Also, when the user presses a hardware OK button on the devices having one or the special
OK button in the PocketPC title bar, an event with this id is generated.

By default, the affirmative id is wxID_OK.

See: `getAffirmativeId/1`

# `setReturnCode`

```erlang
-spec setReturnCode(This, RetCode) -> ok when This :: wxDialog(), RetCode :: integer().
```

Sets the return code for this window.

A return code is normally associated with a modal dialog, where `showModal/1` returns a code to the
application. The function `endModal/2` calls `setReturnCode/2`.

See:
* `getReturnCode/1`

* `showModal/1`

* `endModal/2`

# `show`

```erlang
-spec show(This) -> boolean() when This :: wxDialog().
```

# `show`

```erlang
-spec show(This, [Option]) -> boolean() when This :: wxDialog(), Option :: {show, boolean()}.
```

Hides or shows the dialog.

The preferred way of dismissing a modal dialog is to use `endModal/2`.

# `showModal`

```erlang
-spec showModal(This) -> integer() when This :: wxDialog().
```

Shows an application-modal dialog.

Program flow does not return until the dialog has been dismissed with `endModal/2`.

Notice that it is possible to call `showModal/1` for a dialog which had been previously shown with `show/2`,
this allows making an existing modeless dialog modal. However `showModal/1` can't be called twice
without intervening `endModal/2` calls.

Note that this function creates a temporary event loop which takes precedence over the
application's main event loop (see `wxEventLoopBase` (not implemented in wx)) and which is
destroyed when the dialog is dismissed. This also results in a call to `wxApp::ProcessPendingEvents()`
(not implemented in wx).

Return: The value set with `setReturnCode/2`.

See:
* `endModal/2`

* `getReturnCode/1`

* `setReturnCode/2`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
