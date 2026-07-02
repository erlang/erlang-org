# `wxGenericDirCtrl`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGenericDirCtrl.erl#L58)

This control can be used to place a directory listing (with optional files) on an
arbitrary window.

The control contains a `m:wxTreeCtrl` window representing the directory hierarchy, and
optionally, a `m:wxChoice` window containing a list of filters.

## Styles

This class supports the following styles:

* wxDIRCTRL_DIR_ONLY: Only show directories, and not files.

* wxDIRCTRL_3D_INTERNAL: Use 3D borders for internal controls. This is the default.

* wxDIRCTRL_SELECT_FIRST: When setting the default path, select the first file in the
directory.

* wxDIRCTRL_SHOW_FILTERS: Show the drop-down filter list.

* wxDIRCTRL_EDIT_LABELS: Allow the folder and file labels to be editable.

* wxDIRCTRL_MULTIPLE: Allows multiple files and folders to be selected.

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxGenericDirCtrl](https://docs.wxwidgets.org/3.2/classwx_generic_dir_ctrl.html)

## Events

Event types emitted from this class:

* [`dirctrl_selectionchanged`](`m:wxTreeEvent`)

* [`dirctrl_fileactivated`](`m:wxTreeEvent`)

# `wxGenericDirCtrl`

```erlang
-type wxGenericDirCtrl() :: wx:wx_object().
```

# `collapseTree`

```erlang
-spec collapseTree(This) -> ok when This :: wxGenericDirCtrl().
```

Collapses the entire tree.

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxGenericDirCtrl(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxGenericDirCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {dir, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {filter, unicode:chardata()} |
                        {defaultFilter, integer()}.
```

Create function for two-step construction.

See `new/2` for details.

# `destroy`

```erlang
-spec destroy(This :: wxGenericDirCtrl()) -> ok.
```

Destroys the object

# `expandPath`

```erlang
-spec expandPath(This, Path) -> boolean() when This :: wxGenericDirCtrl(), Path :: unicode:chardata().
```

Tries to expand as much of the given `path` as possible, so that the filename or
directory is visible in the tree control.

# `getDefaultPath`

```erlang
-spec getDefaultPath(This) -> unicode:charlist() when This :: wxGenericDirCtrl().
```

Gets the default path.

# `getFilePath`

```erlang
-spec getFilePath(This) -> unicode:charlist() when This :: wxGenericDirCtrl().
```

Gets selected filename path only (else empty string).

This function doesn't count a directory as a selection.

# `getFilter`

```erlang
-spec getFilter(This) -> unicode:charlist() when This :: wxGenericDirCtrl().
```

Returns the filter string.

# `getFilterIndex`

```erlang
-spec getFilterIndex(This) -> integer() when This :: wxGenericDirCtrl().
```

Returns the current filter index (zero-based).

# `getPath`

```erlang
-spec getPath(This) -> unicode:charlist() when This :: wxGenericDirCtrl().
```

Gets the currently-selected directory or filename.

# `getPath`

```erlang
-spec getPath(This, ItemId) -> unicode:charlist() when This :: wxGenericDirCtrl(), ItemId :: integer().
```

Gets the path corresponding to the given tree control item.

Since: 2.9.5

# `getRootId`

```erlang
-spec getRootId(This) -> integer() when This :: wxGenericDirCtrl().
```

Returns the root id for the tree control.

# `getTreeCtrl`

```erlang
-spec getTreeCtrl(This) -> wxTreeCtrl:wxTreeCtrl() when This :: wxGenericDirCtrl().
```

Returns a pointer to the tree control.

# `init`

```erlang
-spec init(This) -> ok when This :: wxGenericDirCtrl().
```

Initializes variables.

# `new`

```erlang
-spec new() -> wxGenericDirCtrl().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxGenericDirCtrl() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxGenericDirCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {dir, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {filter, unicode:chardata()} |
                     {defaultFilter, integer()}.
```

Main constructor.

# `reCreateTree`

```erlang
-spec reCreateTree(This) -> ok when This :: wxGenericDirCtrl().
```

Collapse and expand the tree, thus re-creating it from scratch.

May be used to update the displayed directory content.

# `setDefaultPath`

```erlang
-spec setDefaultPath(This, Path) -> ok when This :: wxGenericDirCtrl(), Path :: unicode:chardata().
```

Sets the default path.

# `setFilter`

```erlang
-spec setFilter(This, Filter) -> ok when This :: wxGenericDirCtrl(), Filter :: unicode:chardata().
```

Sets the filter string.

# `setFilterIndex`

```erlang
-spec setFilterIndex(This, N) -> ok when This :: wxGenericDirCtrl(), N :: integer().
```

Sets the current filter index (zero-based).

# `setPath`

```erlang
-spec setPath(This, Path) -> ok when This :: wxGenericDirCtrl(), Path :: unicode:chardata().
```

Sets the current path.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
