# `wxXmlResource`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxXmlResource.erl#L58)

This is the main class for interacting with the XML-based resource system.

The class holds XML resources from one or more .xml files, binary files or zip archive files.

Note that this is a singleton class and you'll never allocate/deallocate it. Just use the
static `get/0` getter.

See:
* [Overview xrc](https://docs.wxwidgets.org/3.2/overview_xrc.html#overview_xrc)

* [Overview xrcformat](https://docs.wxwidgets.org/3.2/overview_xrcformat.html#overview_xrcformat)

wxWidgets docs: [wxXmlResource](https://docs.wxwidgets.org/3.2/classwxXml_resource.html)

# `wxXmlResource`

```erlang
-type wxXmlResource() :: wx:wx_object().
```

# `attachUnknownControl`

```erlang
-spec attachUnknownControl(This, Name, Control) -> boolean()
                              when
                                  This :: wxXmlResource(),
                                  Name :: unicode:chardata(),
                                  Control :: wxWindow:wxWindow().
```

# `attachUnknownControl`

```erlang
-spec attachUnknownControl(This, Name, Control, [Option]) -> boolean()
                              when
                                  This :: wxXmlResource(),
                                  Name :: unicode:chardata(),
                                  Control :: wxWindow:wxWindow(),
                                  Option :: {parent, wxWindow:wxWindow()}.
```

Attaches an unknown control to the given panel/window/dialog.

Unknown controls are used in conjunction with <object class="unknown">.

# `clearHandlers`

```erlang
-spec clearHandlers(This) -> ok when This :: wxXmlResource().
```

Removes all handlers and deletes them (this means that any handlers added using `AddHandler()`
(not implemented in wx) must be allocated on the heap).

# `compareVersion`

```erlang
-spec compareVersion(This, Major, Minor, Release, Revision) -> integer()
                        when
                            This :: wxXmlResource(),
                            Major :: integer(),
                            Minor :: integer(),
                            Release :: integer(),
                            Revision :: integer().
```

Compares the XRC version to the argument.

Returns -1 if the XRC version is less than the argument, +1 if greater, and 0 if they are
equal.

# `destroy`

```erlang
-spec destroy(This :: wxXmlResource()) -> ok.
```

Destroys the object

# `get`

```erlang
-spec get() -> wxXmlResource().
```

Gets the global resources object or creates one if none exists.

# `getFlags`

```erlang
-spec getFlags(This) -> integer() when This :: wxXmlResource().
```

Returns flags, which may be a bitlist of ?wxXmlResourceFlags enumeration values.

# `getVersion`

```erlang
-spec getVersion(This) -> integer() when This :: wxXmlResource().
```

Returns version information (a.b.c.d = d + 256*c + 2562*b + 2563*a).

# `getXRCID`

```erlang
-spec getXRCID(Str_id) -> integer() when Str_id :: unicode:chardata().
```

# `getXRCID`

```erlang
-spec getXRCID(Str_id, [Option]) -> integer()
                  when Str_id :: unicode:chardata(), Option :: {value_if_not_found, integer()}.
```

Returns a numeric ID that is equivalent to the string ID used in an XML resource.

If an unknown `str_id` is requested (i.e. other than wxID_XXX or integer), a new record
is created which associates the given string with a number.

If `value_if_not_found` is `wxID_NONE`, the number is obtained via `wx_misc:newId/0`. Otherwise `value_if_not_found`
is used.

Macro `XRCID(name)` is provided for convenient use in event tables.

Note: IDs returned by XRCID() cannot be used with the `EVT_*_RANGE` macros, because the
order in which they are assigned to symbolic `name` values is not guaranteed.

# `initAllHandlers`

```erlang
-spec initAllHandlers(This) -> ok when This :: wxXmlResource().
```

Initializes handlers for all supported controls/windows.

This will make the executable quite big because it forces linking against most of the
wxWidgets library.

# `load`

```erlang
-spec load(This, Filemask) -> boolean() when This :: wxXmlResource(), Filemask :: unicode:chardata().
```

Loads resources from XML files that match given filemask.

Example:

Note: If wxUSE_FILESYS is enabled, this method understands `wxFileSystem` (not
implemented in wx) URLs (see `wxFileSystem::FindFirst()` (not implemented in wx)).

Note: If you are sure that the argument is name of single XRC file (rather than an URL or
a wildcard), use `LoadFile()` (not implemented in wx) instead.

# `loadBitmap`

```erlang
-spec loadBitmap(This, Name) -> wxBitmap:wxBitmap()
                    when This :: wxXmlResource(), Name :: unicode:chardata().
```

Loads a bitmap resource from a file.

# `loadDialog`

```erlang
-spec loadDialog(This, Parent, Name) -> wxDialog:wxDialog()
                    when
                        This :: wxXmlResource(),
                        Parent :: wxWindow:wxWindow(),
                        Name :: unicode:chardata().
```

Loads a dialog.

`parent` points to parent window (if any).

# `loadDialog`

```erlang
-spec loadDialog(This, Dlg, Parent, Name) -> boolean()
                    when
                        This :: wxXmlResource(),
                        Dlg :: wxDialog:wxDialog(),
                        Parent :: wxWindow:wxWindow(),
                        Name :: unicode:chardata().
```

Loads a dialog.

`parent` points to parent window (if any).

This form is used to finish creation of an already existing instance (the main reason for
this is that you may want to use derived class with a new event table). Example:

# `loadFrame`

```erlang
-spec loadFrame(This, Parent, Name) -> wxFrame:wxFrame()
                   when
                       This :: wxXmlResource(),
                       Parent :: wxWindow:wxWindow(),
                       Name :: unicode:chardata().
```

Loads a frame from the resource.

`parent` points to parent window (if any).

# `loadFrame`

```erlang
-spec loadFrame(This, Frame, Parent, Name) -> boolean()
                   when
                       This :: wxXmlResource(),
                       Frame :: wxFrame:wxFrame(),
                       Parent :: wxWindow:wxWindow(),
                       Name :: unicode:chardata().
```

Loads the contents of a frame onto an existing `m:wxFrame`.

This form is used to finish creation of an already existing instance (the main reason for
this is that you may want to use derived class with a new event table).

# `loadIcon`

```erlang
-spec loadIcon(This, Name) -> wxIcon:wxIcon() when This :: wxXmlResource(), Name :: unicode:chardata().
```

Loads an icon resource from a file.

# `loadMenu`

```erlang
-spec loadMenu(This, Name) -> wxMenu:wxMenu() when This :: wxXmlResource(), Name :: unicode:chardata().
```

Loads menu from resource.

Returns NULL on failure.

# `loadMenuBar`

```erlang
-spec loadMenuBar(This, Name) -> wxMenuBar:wxMenuBar()
                     when This :: wxXmlResource(), Name :: unicode:chardata().
```

# `loadMenuBar`

```erlang
-spec loadMenuBar(This, Parent, Name) -> wxMenuBar:wxMenuBar()
                     when
                         This :: wxXmlResource(),
                         Parent :: wxWindow:wxWindow(),
                         Name :: unicode:chardata().
```

Loads a menubar from resource.

Returns NULL on failure.

# `loadPanel`

```erlang
-spec loadPanel(This, Parent, Name) -> wxPanel:wxPanel()
                   when
                       This :: wxXmlResource(),
                       Parent :: wxWindow:wxWindow(),
                       Name :: unicode:chardata().
```

Loads a panel.

`parent` points to the parent window.

# `loadPanel`

```erlang
-spec loadPanel(This, Panel, Parent, Name) -> boolean()
                   when
                       This :: wxXmlResource(),
                       Panel :: wxPanel:wxPanel(),
                       Parent :: wxWindow:wxWindow(),
                       Name :: unicode:chardata().
```

Loads a panel.

`parent` points to the parent window. This form is used to finish creation of an already
existing instance.

# `loadToolBar`

```erlang
-spec loadToolBar(This, Parent, Name) -> wxToolBar:wxToolBar()
                     when
                         This :: wxXmlResource(),
                         Parent :: wxWindow:wxWindow(),
                         Name :: unicode:chardata().
```

Loads a toolbar.

# `new`

```erlang
-spec new() -> wxXmlResource().
```

# `new`

```erlang
-spec new([Option]) -> wxXmlResource() when Option :: {flags, integer()} | {domain, unicode:chardata()}.
```

Constructor.

# `new`

```erlang
-spec new(Filemask, [Option]) -> wxXmlResource()
             when
                 Filemask :: unicode:chardata(),
                 Option :: {flags, integer()} | {domain, unicode:chardata()}.
```

Constructor.

# `set`

```erlang
-spec set(Res) -> wxXmlResource() when Res :: wxXmlResource().
```

Sets the global resources object and returns a pointer to the previous one (may be NULL).

# `setFlags`

```erlang
-spec setFlags(This, Flags) -> ok when This :: wxXmlResource(), Flags :: integer().
```

Sets flags (bitlist of ?wxXmlResourceFlags enumeration values).

# `unload`

```erlang
-spec unload(This, Filename) -> boolean() when This :: wxXmlResource(), Filename :: unicode:chardata().
```

This function unloads a resource previously loaded by `load/2`.

Returns true if the resource was successfully unloaded and false if it hasn't been found
in the list of loaded resources.

# `xrcctrl`

```erlang
-spec xrcctrl(Window, Name, Type) -> wx:wx_object()
                 when Window :: wxWindow:wxWindow(), Name :: string(), Type :: atom().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
