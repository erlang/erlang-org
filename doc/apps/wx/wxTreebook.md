# `wxTreebook`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxTreebook.erl#L58)

This class is an extension of the `m:wxNotebook` class that allows a tree structured set
of pages to be shown in a control.

A classic example is a netscape preferences dialog that shows a tree of preference
sections on the left and select section page on the right.

To use the class simply create it and populate with pages using `insertPage/5`, `insertSubPage/5`, `addPage/4`, `AddSubPage()` (not
implemented in wx).

If your tree is no more than 1 level in depth then you could simply use `addPage/4` and `AddSubPage()`
(not implemented in wx) to sequentially populate your tree by adding at every step a page
or a subpage to the end of the tree.

See:
* ?wxBookCtrl

* `m:wxBookCtrlEvent`

* `m:wxNotebook`

* `m:wxTreeCtrl`

* `m:wxImageList`

* [Overview bookctrl](https://docs.wxwidgets.org/3.2/overview_bookctrl.html#overview_bookctrl)

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_notebook)

This class is derived, and can use functions, from:

* `m:wxBookCtrlBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTreebook](https://docs.wxwidgets.org/3.2/classwx_treebook.html)

## Events

Event types emitted from this class:

* [`treebook_page_changed`](`m:wxBookCtrlEvent`)

* [`treebook_page_changing`](`m:wxBookCtrlEvent`)

# `wxTreebook`

```erlang
-type wxTreebook() :: wx:wx_object().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text) -> boolean()
                 when This :: wxTreebook(), Page :: wxWindow:wxWindow(), Text :: unicode:chardata().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text, [Option]) -> boolean()
                 when
                     This :: wxTreebook(),
                     Page :: wxWindow:wxWindow(),
                     Text :: unicode:chardata(),
                     Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Adds a new page.

The page is placed at the topmost level after all other pages. NULL could be specified
for page to create an empty page.

# `advanceSelection`

```erlang
-spec advanceSelection(This) -> ok when This :: wxTreebook().
```

# `advanceSelection`

```erlang
-spec advanceSelection(This, [Option]) -> ok when This :: wxTreebook(), Option :: {forward, boolean()}.
```

Cycles through the tabs.

The call to this function generates the page changing events.

# `assignImageList`

```erlang
-spec assignImageList(This, ImageList) -> ok
                         when This :: wxTreebook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list for the page control and takes ownership of the list.

See:
* `m:wxImageList`

* `setImageList/2`

# `changeSelection`

```erlang
-spec changeSelection(This, Page) -> integer() when This :: wxTreebook(), Page :: integer().
```

Changes the selection to the given page, returning the previous selection.

This function behaves as `setSelection/2` but does `not` generate the page changing events.

See overview_events_prog for more information.

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxTreebook(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxTreebook(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates a treebook control.

See `new/3` for the description of the parameters.

# `deleteAllPages`

```erlang
-spec deleteAllPages(This) -> boolean() when This :: wxTreebook().
```

Deletes all pages.

# `destroy`

```erlang
-spec destroy(This :: wxTreebook()) -> ok.
```

Destroys the object

# `expandNode`

```erlang
-spec expandNode(This, PageId) -> boolean() when This :: wxTreebook(), PageId :: integer().
```

# `expandNode`

```erlang
-spec expandNode(This, PageId, [Option]) -> boolean()
                    when This :: wxTreebook(), PageId :: integer(), Option :: {expand, boolean()}.
```

Expands (collapses) the `pageId` node.

Returns the previous state. May generate page changing events (if selected page is under
the collapsed branch, then its parent is autoselected).

# `getCurrentPage`

```erlang
-spec getCurrentPage(This) -> wxWindow:wxWindow() when This :: wxTreebook().
```

Returns the currently selected page or NULL.

# `getImageList`

```erlang
-spec getImageList(This) -> wxImageList:wxImageList() when This :: wxTreebook().
```

Returns the associated image list, may be NULL.

See:
* `m:wxImageList`

* `setImageList/2`

# `getPage`

```erlang
-spec getPage(This, Page) -> wxWindow:wxWindow() when This :: wxTreebook(), Page :: integer().
```

Returns the window at the given page position.

# `getPageCount`

```erlang
-spec getPageCount(This) -> integer() when This :: wxTreebook().
```

Returns the number of pages in the control.

# `getPageImage`

```erlang
-spec getPageImage(This, NPage) -> integer() when This :: wxTreebook(), NPage :: integer().
```

Returns the image index for the given page.

# `getPageText`

```erlang
-spec getPageText(This, NPage) -> unicode:charlist() when This :: wxTreebook(), NPage :: integer().
```

Returns the string for the given page.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxTreebook().
```

Returns the currently selected page, or `wxNOT\_FOUND` if none was selected.

Note: This method may return either the previously or newly selected page when called
from the EVT_TREEBOOK_PAGE_CHANGED() handler depending on the platform and so `wxBookCtrlEvent:getSelection/1` should be
used instead in this case.

# `hitTest`

```erlang
-spec hitTest(This, Pt) -> Result
                 when
                     Result :: {Res :: integer(), Flags :: integer()},
                     This :: wxTreebook(),
                     Pt :: {X :: integer(), Y :: integer()}.
```

Returns the index of the tab at the specified position or `wxNOT\_FOUND` if none.

If `flags` parameter is non-NULL, the position of the point inside the tab is returned as well.

Return: Returns the zero-based tab index or `wxNOT_FOUND` if there is no tab at the
specified position.

# `insertPage`

```erlang
-spec insertPage(This, PagePos, Page, Text) -> boolean()
                    when
                        This :: wxTreebook(),
                        PagePos :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata().
```

# `insertPage`

```erlang
-spec insertPage(This, PagePos, Page, Text, [Option]) -> boolean()
                    when
                        This :: wxTreebook(),
                        PagePos :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata(),
                        Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Inserts a new page just before the page indicated by `pagePos`.

The new page is placed before `pagePos` page and on the same level. NULL could be
specified for page to create an empty page.

# `insertSubPage`

```erlang
-spec insertSubPage(This, PagePos, Page, Text) -> boolean()
                       when
                           This :: wxTreebook(),
                           PagePos :: integer(),
                           Page :: wxWindow:wxWindow(),
                           Text :: unicode:chardata().
```

# `insertSubPage`

```erlang
-spec insertSubPage(This, PagePos, Page, Text, [Option]) -> boolean()
                       when
                           This :: wxTreebook(),
                           PagePos :: integer(),
                           Page :: wxWindow:wxWindow(),
                           Text :: unicode:chardata(),
                           Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Inserts a sub page under the specified page.

NULL could be specified for page to create an empty page.

# `isNodeExpanded`

```erlang
-spec isNodeExpanded(This, PageId) -> boolean() when This :: wxTreebook(), PageId :: integer().
```

Returns true if the page represented by `pageId` is expanded.

# `new`

```erlang
-spec new() -> wxTreebook().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxTreebook() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxTreebook()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Creates an empty `m:wxTreebook`.

# `setImageList`

```erlang
-spec setImageList(This, ImageList) -> ok
                      when This :: wxTreebook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list to use.

It does not take ownership of the image list, you must delete it yourself.

See:
* `m:wxImageList`

* `assignImageList/2`

# `setPageImage`

```erlang
-spec setPageImage(This, Page, Image) -> boolean()
                      when This :: wxTreebook(), Page :: integer(), Image :: integer().
```

Sets the image index for the given page.

`image` is an index into the image list which was set with `setImageList/2`.

# `setPageSize`

```erlang
-spec setPageSize(This, Size) -> ok when This :: wxTreebook(), Size :: {W :: integer(), H :: integer()}.
```

Sets the width and height of the pages.

Note: This method is currently not implemented for wxGTK.

# `setPageText`

```erlang
-spec setPageText(This, Page, Text) -> boolean()
                     when This :: wxTreebook(), Page :: integer(), Text :: unicode:chardata().
```

Sets the text for the given page.

# `setSelection`

```erlang
-spec setSelection(This, Page) -> integer() when This :: wxTreebook(), Page :: integer().
```

Sets the selection to the given page, returning the previous selection.

Notice that the call to this function generates the page changing events, use the `changeSelection/2`
function if you don't want these events to be generated.

See: `wxBookCtrlBase:getSelection/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
