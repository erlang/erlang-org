# `wxDirPickerCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDirPickerCtrl.erl#L58)

This control allows the user to select a directory.

The generic implementation is a button which brings up a `m:wxDirDialog` when clicked.
Native implementation may differ but this is usually a (small) widget which give access to
the dir-chooser dialog. It is only available if `wxUSE_DIRPICKERCTRL` is set to 1 (the default).

## Styles

This class supports the following styles:

* wxDIRP_DEFAULT_STYLE: The default style: includes wxDIRP_DIR_MUST_EXIST and, under wxMSW
only, wxDIRP_USE_TEXTCTRL.

* wxDIRP_USE_TEXTCTRL: Creates a text control to the left of the picker button which is
completely managed by the `m:wxDirPickerCtrl` and which can be used by the user to specify
a path (see SetPath). The text control is automatically synchronized with button's value.
Use functions defined in `m:wxPickerBase` to modify the text control.

* wxDIRP_DIR_MUST_EXIST: Creates a picker which allows selecting only existing directories
in the popup `m:wxDirDialog`. Notice that, as with `wxFLP_FILE_MUST_EXIST`, it is still
possible to enter a non-existent directory even when this file is specified if `wxDIRP_USE_TEXTCTRL`
style is also used. Also note that if `wxDIRP_USE_TEXTCTRL` is not used, the native wxGTK
implementation always uses this style as it doesn't support selecting non-existent
directories.

* wxDIRP_CHANGE_DIR: Change current working directory on each user directory selection
change.

* wxDIRP_SMALL: Use smaller version of the control with a small "..." button instead of the
normal "Browse" one. This flag is new since wxWidgets 2.9.3.

See:
* `m:wxDirDialog`

* `m:wxFileDirPickerEvent`

This class is derived, and can use functions, from:

* `m:wxPickerBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxDirPickerCtrl](https://docs.wxwidgets.org/3.2/classwx_dir_picker_ctrl.html)

## Events

Event types emitted from this class:

* [`command_dirpicker_changed`](`m:wxFileDirPickerEvent`)

# `wxDirPickerCtrl`

```erlang
-type wxDirPickerCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxDirPickerCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxDirPickerCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {path, unicode:chardata()} |
                        {message, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the widgets with the given parameters.

Return: true if the control was successfully created or false if creation failed.

# `destroy`

```erlang
-spec destroy(This :: wxDirPickerCtrl()) -> ok.
```

Destroys the object

# `getPath`

```erlang
-spec getPath(This) -> unicode:charlist() when This :: wxDirPickerCtrl().
```

Returns the absolute path of the currently selected directory.

# `new`

```erlang
-spec new() -> wxDirPickerCtrl().
```

# `new`

```erlang
-spec new(Parent, Id) -> wxDirPickerCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxDirPickerCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {path, unicode:chardata()} |
                     {message, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Initializes the object and calls `create/4` with all the parameters.

# `setPath`

```erlang
-spec setPath(This, Dirname) -> ok when This :: wxDirPickerCtrl(), Dirname :: unicode:chardata().
```

Sets the absolute path of the currently selected directory.

If the control uses `wxDIRP_DIR_MUST_EXIST` and does not use `wxDIRP_USE_TEXTCTRL` style,
the `dirname` must be a name of an existing directory and will be simply ignored by the
native wxGTK implementation if this is not the case.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
