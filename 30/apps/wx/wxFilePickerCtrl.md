# `wxFilePickerCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFilePickerCtrl.erl#L58)

This control allows the user to select a file.

The generic implementation is a button which brings up a `m:wxFileDialog` when clicked.
Native implementation may differ but this is usually a (small) widget which give access to
the file-chooser dialog. It is only available if `wxUSE_FILEPICKERCTRL` is set to 1 (the default).

## Styles

This class supports the following styles:

* wxFLP_DEFAULT_STYLE: The default style: includes wxFLP_OPEN | wxFLP_FILE_MUST_EXIST and,
under wxMSW and wxOSX, wxFLP_USE_TEXTCTRL.

* wxFLP_USE_TEXTCTRL: Creates a text control to the left of the picker button which is
completely managed by the `m:wxFilePickerCtrl` and which can be used by the user to
specify a path (see SetPath). The text control is automatically synchronized with button's
value. Use functions defined in `m:wxPickerBase` to modify the text control.

* wxFLP_OPEN: Creates a picker which allows the user to select a file to open.

* wxFLP_SAVE: Creates a picker which allows the user to select a file to save.

* wxFLP_OVERWRITE_PROMPT: Can be combined with wxFLP_SAVE only: ask confirmation to the
user before selecting a file.

* wxFLP_FILE_MUST_EXIST: Can be combined with wxFLP_OPEN only: the file selected in the
popup `m:wxFileDialog` must be an existing file. Notice that it still remains possible for
the user to enter a non-existent file name in the text control if `wxFLP_USE_TEXTCTRL` is
also used, this flag is a hint for the user rather than a guarantee that the selected file
does exist for the program.

* wxFLP_CHANGE_DIR: Change current working directory on each user file selection change.

* wxFLP_SMALL: Use smaller version of the control with a small "..." button instead of the
normal "Browse" one. This flag is new since wxWidgets 2.9.3.

See:
* `m:wxFileDialog`

* `m:wxFileDirPickerEvent`

This class is derived, and can use functions, from:

* `m:wxPickerBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFilePickerCtrl](https://docs.wxwidgets.org/3.2/classwx_file_picker_ctrl.html)

## Events

Event types emitted from this class:

* [`command_filepicker_changed`](`m:wxFileDirPickerEvent`)

# `wxFilePickerCtrl`

```erlang
-type wxFilePickerCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxFilePickerCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxFilePickerCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {path, unicode:chardata()} |
                        {message, unicode:chardata()} |
                        {wildcard, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates this widget with the given parameters.

Return: true if the control was successfully created or false if creation failed.

# `destroy`

```erlang
-spec destroy(This :: wxFilePickerCtrl()) -> ok.
```

Destroys the object

# `getPath`

```erlang
-spec getPath(This) -> unicode:charlist() when This :: wxFilePickerCtrl().
```

Returns the absolute path of the currently selected file.

# `new`

```erlang
-spec new() -> wxFilePickerCtrl().
```

# `new`

```erlang
-spec new(Parent, Id) -> wxFilePickerCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxFilePickerCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {path, unicode:chardata()} |
                     {message, unicode:chardata()} |
                     {wildcard, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Initializes the object and calls `create/4` with all the parameters.

# `setPath`

```erlang
-spec setPath(This, Filename) -> ok when This :: wxFilePickerCtrl(), Filename :: unicode:chardata().
```

Sets the absolute path of the currently selected file.

If the control uses `wxFLP_FILE_MUST_EXIST` and does not use `wxFLP_USE_TEXTCTRL` style,
the `filename` must be a name of an existing file and will be simply ignored by the native
wxGTK implementation if this is not the case (the generic implementation used under the
other platforms accepts even invalid file names currently, but this is subject to change
in the future, don't rely on being able to use non-existent paths with it).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
