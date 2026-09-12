# `wxAuiNotebook`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxAuiNotebook.erl#L58)

`m:wxAuiNotebook` is part of the wxAUI class framework, which represents a notebook
control, managing multiple windows with associated tabs.

See also overview_aui.

`m:wxAuiNotebook` is a notebook control which implements many features common in
applications with dockable panes. Specifically, `m:wxAuiNotebook` implements functionality
which allows the user to rearrange tab order via drag-and-drop, split the tab window into
many different splitter configurations, and toggle through different themes to customize
the control's look and feel.

The default theme that is used is `wxAuiDefaultTabArt` (not implemented in wx), which
provides a modern, glossy look and feel. The theme can be changed by calling `setArtProvider/2`.

## Styles

This class supports the following styles:

* wxAUI_NB_DEFAULT_STYLE: Defined as wxAUI_NB_TOP | wxAUI_NB_TAB_SPLIT | wxAUI_NB_TAB_MOVE
| wxAUI_NB_SCROLL_BUTTONS | wxAUI_NB_CLOSE_ON_ACTIVE_TAB | wxAUI_NB_MIDDLE_CLICK_CLOSE.

* wxAUI_NB_TAB_SPLIT: Allows the tab control to be split by dragging a tab.

* wxAUI_NB_TAB_MOVE: Allows a tab to be moved horizontally by dragging.

* wxAUI_NB_TAB_EXTERNAL_MOVE: Allows a tab to be moved to another tab control.

* wxAUI_NB_TAB_FIXED_WIDTH: With this style, all tabs have the same width.

* wxAUI_NB_SCROLL_BUTTONS: With this style, left and right scroll buttons are displayed.

* wxAUI_NB_WINDOWLIST_BUTTON: With this style, a drop-down list of windows is available.

* wxAUI_NB_CLOSE_BUTTON: With this style, a close button is available on the tab bar.

* wxAUI_NB_CLOSE_ON_ACTIVE_TAB: With this style, the close button is visible on the active
tab.

* wxAUI_NB_CLOSE_ON_ALL_TABS: With this style, the close button is visible on all tabs.

* wxAUI_NB_MIDDLE_CLICK_CLOSE: With this style, middle click on a tab closes the tab.

* wxAUI_NB_TOP: With this style, tabs are drawn along the top of the notebook.

* wxAUI_NB_BOTTOM: With this style, tabs are drawn along the bottom of the notebook.

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxAuiNotebook](https://docs.wxwidgets.org/3.2/classwx_aui_notebook.html)

## Events

Event types emitted from this class:

* [`command_auinotebook_page_close`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_page_closed`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_page_changed`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_page_changing`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_button`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_begin_drag`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_end_drag`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_drag_motion`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_allow_dnd`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_drag_done`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_tab_middle_down`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_tab_middle_up`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_tab_right_down`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_tab_right_up`](`m:wxAuiNotebookEvent`)

* [`command_auinotebook_bg_dclick`](`m:wxAuiNotebookEvent`)

# `wxAuiNotebook`

```erlang
-type wxAuiNotebook() :: wx:wx_object().
```

# `addPage`

```erlang
-spec addPage(This, Page, Caption) -> boolean()
                 when
                     This :: wxAuiNotebook(), Page :: wxWindow:wxWindow(), Caption :: unicode:chardata().
```

# `addPage`

```erlang
-spec addPage(This, Page, Caption, [Option]) -> boolean()
                 when
                     This :: wxAuiNotebook(),
                     Page :: wxWindow:wxWindow(),
                     Caption :: unicode:chardata(),
                     Option :: {select, boolean()} | {bitmap, wxBitmap:wxBitmap()}.
```

Adds a page.

If the `select` parameter is true, calling this will generate a page change event.

# `addPage`

```erlang
-spec addPage(This, Page, Text, Select, ImageId) -> boolean()
                 when
                     This :: wxAuiNotebook(),
                     Page :: wxWindow:wxWindow(),
                     Text :: unicode:chardata(),
                     Select :: boolean(),
                     ImageId :: integer().
```

Adds a new page.

The page must have the book control itself as the parent and must not have been added to
this control previously.

The call to this function may generate the page changing events.

Return: true if successful, false otherwise.

Remark: Do not delete the page, it will be deleted by the book control.

See: `insertPage/6`

Since: 2.9.3

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxAuiNotebook(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, Winid) -> boolean()
                when This :: wxAuiNotebook(), Parent :: wxWindow:wxWindow(), Winid :: integer();
            (This, Parent, [Option]) -> boolean()
                when
                    This :: wxAuiNotebook(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates the notebook window.

# `create`

```erlang
-spec create(This, Parent, Winid, [Option]) -> boolean()
                when
                    This :: wxAuiNotebook(),
                    Parent :: wxWindow:wxWindow(),
                    Winid :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Constructs the book control with the given parameters.

# `deletePage`

```erlang
-spec deletePage(This, Page) -> boolean() when This :: wxAuiNotebook(), Page :: integer().
```

Deletes a page at the given index.

Calling this method will generate a page change event.

# `destroy`

```erlang
-spec destroy(This :: wxAuiNotebook()) -> ok.
```

Destroys the object

# `getArtProvider`

```erlang
-spec getArtProvider(This) -> wxAuiTabArt:wxAuiTabArt() when This :: wxAuiNotebook().
```

Returns the associated art provider.

# `getPage`

```erlang
-spec getPage(This, Page_idx) -> wxWindow:wxWindow() when This :: wxAuiNotebook(), Page_idx :: integer().
```

Returns the page specified by the given index.

# `getPageBitmap`

```erlang
-spec getPageBitmap(This, Page) -> wxBitmap:wxBitmap() when This :: wxAuiNotebook(), Page :: integer().
```

Returns the tab bitmap for the page.

# `getPageCount`

```erlang
-spec getPageCount(This) -> integer() when This :: wxAuiNotebook().
```

Returns the number of pages in the notebook.

# `getPageIndex`

```erlang
-spec getPageIndex(This, Page_wnd) -> integer()
                      when This :: wxAuiNotebook(), Page_wnd :: wxWindow:wxWindow().
```

Returns the page index for the specified window.

If the window is not found in the notebook, wxNOT_FOUND is returned.

# `getPageText`

```erlang
-spec getPageText(This, Page) -> unicode:charlist() when This :: wxAuiNotebook(), Page :: integer().
```

Returns the tab label for the page.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxAuiNotebook().
```

Returns the currently selected page.

# `insertPage`

```erlang
-spec insertPage(This, Page_idx, Page, Caption) -> boolean()
                    when
                        This :: wxAuiNotebook(),
                        Page_idx :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Caption :: unicode:chardata().
```

# `insertPage`

```erlang
-spec insertPage(This, Page_idx, Page, Caption, [Option]) -> boolean()
                    when
                        This :: wxAuiNotebook(),
                        Page_idx :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Caption :: unicode:chardata(),
                        Option :: {select, boolean()} | {bitmap, wxBitmap:wxBitmap()}.
```

`insertPage/6` is similar to AddPage, but allows the ability to specify the insert
location.

If the `select` parameter is true, calling this will generate a page change event.

# `insertPage`

```erlang
-spec insertPage(This, Index, Page, Text, Select, ImageId) -> boolean()
                    when
                        This :: wxAuiNotebook(),
                        Index :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata(),
                        Select :: boolean(),
                        ImageId :: integer().
```

Inserts a new page at the specified position.

Return: true if successful, false otherwise.

Remark: Do not delete the page, it will be deleted by the book control.

See: `addPage/5`

Since: 2.9.3

# `new`

```erlang
-spec new() -> wxAuiNotebook().
```

Default ctor.

# `new`

```erlang
-spec new(Parent) -> wxAuiNotebook() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxAuiNotebook()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

Creates a wxAuiNotebok control.

# `removePage`

```erlang
-spec removePage(This, Page) -> boolean() when This :: wxAuiNotebook(), Page :: integer().
```

Removes a page, without deleting the window pointer.

# `setArtProvider`

```erlang
-spec setArtProvider(This, Art) -> ok when This :: wxAuiNotebook(), Art :: wxAuiTabArt:wxAuiTabArt().
```

Sets the art provider to be used by the notebook.

# `setFont`

```erlang
-spec setFont(This, Font) -> boolean() when This :: wxAuiNotebook(), Font :: wxFont:wxFont().
```

Sets the font for drawing the tab labels, using a bold version of the font for selected
tab labels.

# `setPageBitmap`

```erlang
-spec setPageBitmap(This, Page, Bitmap) -> boolean()
                       when This :: wxAuiNotebook(), Page :: integer(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the bitmap for the page.

To remove a bitmap from the tab caption, pass wxNullBitmap.

# `setPageText`

```erlang
-spec setPageText(This, Page, Text) -> boolean()
                     when This :: wxAuiNotebook(), Page :: integer(), Text :: unicode:chardata().
```

Sets the tab label for the page.

# `setSelection`

```erlang
-spec setSelection(This, New_page) -> integer() when This :: wxAuiNotebook(), New_page :: integer().
```

Sets the page selection.

Calling this method will generate a page change event.

# `setTabCtrlHeight`

```erlang
-spec setTabCtrlHeight(This, Height) -> ok when This :: wxAuiNotebook(), Height :: integer().
```

Sets the tab height.

By default, the tab control height is calculated by measuring the text height and bitmap
sizes on the tab captions. Calling this method will override that calculation and set the
tab control to the specified height parameter. A call to this method will override any
call to `setUniformBitmapSize/2`.

Specifying -1 as the height will return the control to its default auto-sizing behaviour.

# `setUniformBitmapSize`

```erlang
-spec setUniformBitmapSize(This, Size) -> ok
                              when This :: wxAuiNotebook(), Size :: {W :: integer(), H :: integer()}.
```

Ensure that all tabs have the same height, even if some of them don't have bitmaps.

Passing ?wxDefaultSize as `size` undoes the effect of a previous call to this function
and instructs the control to use dynamic tab height.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
