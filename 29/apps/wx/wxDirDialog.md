# `wxDirDialog`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxDirDialog.erl#L58)

This class represents the directory chooser dialog.

## Styles

This class supports the following styles:

* wxDD_DEFAULT_STYLE: Equivalent to a combination of wxDEFAULT_DIALOG_STYLE and
wxRESIZE_BORDER.

* wxDD_DIR_MUST_EXIST: The dialog will allow the user to choose only an existing folder.
When this style is not given, a "Create new directory" button is added to the dialog (on
Windows) or some other way is provided to the user to type the name of a new folder.

* wxDD_CHANGE_DIR: Change the current working directory to the directory chosen by the
user.

Note: This flag cannot be used with the `wxDD_MULTIPLE` style.

* wxDD_MULTIPLE: Allow the user to select multiple directories. This flag is only
available since wxWidgets 3.1.4

* wxDD_SHOW_HIDDEN: Show hidden and system folders. This flag is only available since
wxWidgets 3.1.4 Notice that `wxRESIZE_BORDER` has special side effect under Windows where
two different directory selection dialogs are available and this style also implicitly
selects the new version as the old one always has fixed size. As the new version is almost
always preferable, it is recommended that `wxRESIZE_BORDER` style be always used. This is
the case if the dialog is created with the default style value but if you need to use any
additional styles you should still specify `wxDD_DEFAULT_STYLE` unless you explicitly need
to use the old dialog version under Windows. E.g. do instead of just using `wxDD_DIR_MUST_EXIST`
style alone.

Remark: MacOS 10.11+ does not display a title bar on the dialog. Use `setMessage/2` to change the
string displayed to the user at the top of the dialog after creation. The `wxTopLevelWindow:setTitle/2` method is
provided for compatibility with pre-10.11 MacOS versions that do still support displaying
the title bar.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_dir)

* `m:wxFileDialog`

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxDirDialog](https://docs.wxwidgets.org/3.2/classwx_dir_dialog.html)

# `wxDirDialog`

```erlang
-type wxDirDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxDirDialog()) -> ok.
```

Destroys the object

# `getMessage`

```erlang
-spec getMessage(This) -> unicode:charlist() when This :: wxDirDialog().
```

Returns the message that will be displayed on the dialog.

# `getPath`

```erlang
-spec getPath(This) -> unicode:charlist() when This :: wxDirDialog().
```

Returns the default or user-selected path.

Note: This function can't be used with dialogs which have the `wxDD_MULTIPLE` style, use `GetPaths()`
(not implemented in wx) instead.

# `new`

```erlang
-spec new(Parent) -> wxDirDialog() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxDirDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {title, unicode:chardata()} |
                     {defaultPath, unicode:chardata()} |
                     {style, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {sz, {W :: integer(), H :: integer()}}.
```

Constructor.

Use `wxDialog:showModal/1` to show the dialog.

# `setMessage`

```erlang
-spec setMessage(This, Message) -> ok when This :: wxDirDialog(), Message :: unicode:chardata().
```

Sets the message that will be displayed on the dialog.

# `setPath`

```erlang
-spec setPath(This, Path) -> ok when This :: wxDirDialog(), Path :: unicode:chardata().
```

Sets the default path.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
