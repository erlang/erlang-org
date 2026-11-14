# `wxFileDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFileDialog.erl#L58)

This class represents the file chooser dialog.

The path and filename are distinct elements of a full file pathname. If path is
?wxEmptyString, the current directory will be used. If filename is ?wxEmptyString, no
default filename will be supplied. The wildcard determines what files are displayed in the
file selector, and file extension supplies a type extension for the required filename.

The typical usage for the open file dialog is:

The typical usage for the save file dialog is instead somewhat simpler:

Remark: All implementations of the `m:wxFileDialog` provide a wildcard filter. Typing a
filename containing wildcards (*, ?) in the filename text item, and clicking on Ok, will
result in only those files matching the pattern being displayed. The wildcard may be a
specification for multiple types of file with a description for each, such as: It must be
noted that wildcard support in the native Motif file dialog is quite limited: only one
file type is supported, and it is displayed without the descriptive test; "BMP files
(*.bmp)|*.bmp" is displayed as "*.bmp", and both "BMP files (*.bmp)|*.bmp|GIF files
(*.gif)|*.gif" and "Image files|*.bmp;*.gif" are errors. On Mac macOS in the open file
dialog the filter choice box is not shown by default. Instead all given wildcards are
appplied at the same time: So in the above example all bmp, gif and png files are
displayed. To enforce the display of the filter choice set the corresponding `m:wxSystemOptions`
before calling the file open dialog: But in contrast to Windows and Unix, where the file
type choice filters only the selected files, on Mac macOS even in this case the dialog
shows all files matching all file types. The files which does not match the currently
selected file type are greyed out and are not selectable.

## Styles

This class supports the following styles:

* wxFD_DEFAULT_STYLE: Equivalent to `wxFD_OPEN`.

* wxFD_OPEN: This is an open dialog; usually this means that the default button's label of
the dialog is "Open". Cannot be combined with `wxFD_SAVE`.

* wxFD_SAVE: This is a save dialog; usually this means that the default button's label of
the dialog is "Save". Cannot be combined with `wxFD_OPEN`.

* wxFD_OVERWRITE_PROMPT: For save dialog only: prompt for a confirmation if a file will be
overwritten.

* wxFD_NO_FOLLOW: Directs the dialog to return the path and file name of the selected
shortcut file, not its target as it does by default. Currently this flag is only
implemented in wxMSW and wxOSX (where it prevents aliases from being resolved). The
non-dereferenced link path is always returned, even without this flag, under Unix and so
using it there doesn't do anything. This flag was added in wxWidgets 3.1.0.

* wxFD_FILE_MUST_EXIST: For open dialog only: the user may only select files that actually
exist. Notice that under macOS the file dialog with `wxFD_OPEN` style always behaves as if
this style was specified, because it is impossible to choose a file that doesn't exist
from a standard macOS file dialog.

* wxFD_MULTIPLE: For open dialog only: allows selecting multiple files.

* wxFD_CHANGE_DIR: Change the current working directory (when the dialog is dismissed) to
the directory where the file(s) chosen by the user are.

* wxFD_PREVIEW: Show the preview of the selected files (currently only supported by wxGTK).

* wxFD_SHOW_HIDDEN: Show hidden files. This flag was added in wxWidgets 3.1.3

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_file)

* ?wxFileSelector()

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFileDialog](https://docs.wxwidgets.org/3.2/classwx_file_dialog.html)

# `wxFileDialog`

```erlang
-type wxFileDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxFileDialog()) -> ok.
```

Destroys the object

# `getDirectory`

```erlang
-spec getDirectory(This) -> unicode:charlist() when This :: wxFileDialog().
```

Returns the default directory.

# `getFilename`

```erlang
-spec getFilename(This) -> unicode:charlist() when This :: wxFileDialog().
```

Returns the default filename.

Note: This function can't be used with dialogs which have the `wxFD_MULTIPLE` style, use `getFilenames/1`
instead.

# `getFilenames`

```erlang
-spec getFilenames(This) -> [unicode:charlist()] when This :: wxFileDialog().
```

Fills the array `filenames` with the names of the files chosen.

This function should only be used with the dialogs which have `wxFD_MULTIPLE` style, use `getFilename/1`
for the others.

Note that under Windows, if the user selects shortcuts, the filenames include paths,
since the application cannot determine the full path of each referenced file by appending
the directory containing the shortcuts to the filename.

# `getFilterIndex`

```erlang
-spec getFilterIndex(This) -> integer() when This :: wxFileDialog().
```

Returns the index into the list of filters supplied, optionally, in the wildcard
parameter.

Before the dialog is shown, this is the index which will be used when the dialog is first displayed.

After the dialog is shown, this is the index selected by the user.

# `getMessage`

```erlang
-spec getMessage(This) -> unicode:charlist() when This :: wxFileDialog().
```

Returns the message that will be displayed on the dialog.

# `getPath`

```erlang
-spec getPath(This) -> unicode:charlist() when This :: wxFileDialog().
```

Returns the full path (directory and filename) of the selected file.

Note: This function can't be used with dialogs which have the `wxFD_MULTIPLE` style, use `getPaths/1`
instead.

# `getPaths`

```erlang
-spec getPaths(This) -> [unicode:charlist()] when This :: wxFileDialog().
```

Fills the array `paths` with the full paths of the files chosen.

This function should only be used with the dialogs which have `wxFD_MULTIPLE` style, use `getPath/1`
for the others.

# `getWildcard`

```erlang
-spec getWildcard(This) -> unicode:charlist() when This :: wxFileDialog().
```

Returns the file dialog wildcard.

# `new`

```erlang
-spec new(Parent) -> wxFileDialog() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxFileDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {message, unicode:chardata()} |
                     {defaultDir, unicode:chardata()} |
                     {defaultFile, unicode:chardata()} |
                     {wildCard, unicode:chardata()} |
                     {style, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {sz, {W :: integer(), H :: integer()}}.
```

Constructor.

Use `wxDialog:showModal/1` to show the dialog.

# `setDirectory`

```erlang
-spec setDirectory(This, Directory) -> ok when This :: wxFileDialog(), Directory :: unicode:chardata().
```

Sets the default directory.

# `setFilename`

```erlang
-spec setFilename(This, Setfilename) -> ok
                     when This :: wxFileDialog(), Setfilename :: unicode:chardata().
```

Sets the default filename.

In wxGTK this will have little effect unless a default directory has previously been set.

# `setFilterIndex`

```erlang
-spec setFilterIndex(This, FilterIndex) -> ok when This :: wxFileDialog(), FilterIndex :: integer().
```

Sets the default filter index, starting from zero.

# `setMessage`

```erlang
-spec setMessage(This, Message) -> ok when This :: wxFileDialog(), Message :: unicode:chardata().
```

Sets the message that will be displayed on the dialog.

# `setPath`

```erlang
-spec setPath(This, Path) -> ok when This :: wxFileDialog(), Path :: unicode:chardata().
```

Sets the path (the combined directory and filename that will be returned when the dialog
is dismissed).

# `setWildcard`

```erlang
-spec setWildcard(This, WildCard) -> ok when This :: wxFileDialog(), WildCard :: unicode:chardata().
```

Sets the wildcard, which can contain multiple file types, for example: "BMP files
(\*.bmp)|\*.bmp|GIF files (\*.gif)|\*.gif".

Note that the native Motif dialog has some limitations with respect to wildcards; see the
Remarks section above.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
