# `wxListbook`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxListbook.erl#L58)

`m:wxListbook` is a class similar to `m:wxNotebook` but which uses a `m:wxListCtrl` to
show the labels instead of the tabs.

The underlying `m:wxListCtrl` displays page labels in a one-column report view by
default. Calling wxBookCtrl::SetImageList will implicitly switch the control to use an
icon view.

For usage documentation of this class, please refer to the base abstract class
wxBookCtrl. You can also use the page_samples_notebook to see `m:wxListbook` in action.

## Styles

This class supports the following styles:

* wxLB_DEFAULT: Choose the default location for the labels depending on the current
platform (left everywhere except Mac where it is top).

* wxLB_TOP: Place labels above the page area.

* wxLB_LEFT: Place labels on the left side.

* wxLB_RIGHT: Place labels on the right side.

* wxLB_BOTTOM: Place labels below the page area.

See:
* ?wxBookCtrl

* `m:wxNotebook`

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_notebook)

This class is derived, and can use functions, from:

* `m:wxBookCtrlBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxListbook](https://docs.wxwidgets.org/3.2/classwx_listbook.html)

## Events

Event types emitted from this class:

* [`listbook_page_changed`](`m:wxBookCtrlEvent`)

* [`listbook_page_changing`](`m:wxBookCtrlEvent`)

# `wxListbook`

```erlang
-type wxListbook() :: wx:wx_object().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text) -> boolean()
                 when This :: wxListbook(), Page :: wxWindow:wxWindow(), Text :: unicode:chardata().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text, [Option]) -> boolean()
                 when
                     This :: wxListbook(),
                     Page :: wxWindow:wxWindow(),
                     Text :: unicode:chardata(),
                     Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Adds a new page.

The page must have the book control itself as the parent and must not have been added to
this control previously.

The call to this function will generate the page changing and page changed events if `select`
is true, but not when inserting the very first page (as there is no previous page
selection to switch from in this case and so it wouldn't make sense to e.g. veto such event).

Return: true if successful, false otherwise.

Remark: Do not delete the page, it will be deleted by the book control.

See: `insertPage/5`

# `advanceSelection`

```erlang
-spec advanceSelection(This) -> ok when This :: wxListbook().
```

# `advanceSelection`

```erlang
-spec advanceSelection(This, [Option]) -> ok when This :: wxListbook(), Option :: {forward, boolean()}.
```

Cycles through the tabs.

The call to this function generates the page changing events.

# `assignImageList`

```erlang
-spec assignImageList(This, ImageList) -> ok
                         when This :: wxListbook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list for the page control and takes ownership of the list.

See:
* `m:wxImageList`

* `setImageList/2`

# `changeSelection`

```erlang
-spec changeSelection(This, Page) -> integer() when This :: wxListbook(), Page :: integer().
```

Changes the selection to the given page, returning the previous selection.

This function behaves as `setSelection/2` but does `not` generate the page changing events.

See overview_events_prog for more information.

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxListbook(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxListbook(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Create the list book control that has already been constructed with the default
constructor.

# `deleteAllPages`

```erlang
-spec deleteAllPages(This) -> boolean() when This :: wxListbook().
```

Deletes all pages.

# `destroy`

```erlang
-spec destroy(This :: wxListbook()) -> ok.
```

Destroys the object

# `getCurrentPage`

```erlang
-spec getCurrentPage(This) -> wxWindow:wxWindow() when This :: wxListbook().
```

Returns the currently selected page or NULL.

# `getImageList`

```erlang
-spec getImageList(This) -> wxImageList:wxImageList() when This :: wxListbook().
```

Returns the associated image list, may be NULL.

See:
* `m:wxImageList`

* `setImageList/2`

# `getPage`

```erlang
-spec getPage(This, Page) -> wxWindow:wxWindow() when This :: wxListbook(), Page :: integer().
```

Returns the window at the given page position.

# `getPageCount`

```erlang
-spec getPageCount(This) -> integer() when This :: wxListbook().
```

Returns the number of pages in the control.

# `getPageImage`

```erlang
-spec getPageImage(This, NPage) -> integer() when This :: wxListbook(), NPage :: integer().
```

Returns the image index for the given page.

# `getPageText`

```erlang
-spec getPageText(This, NPage) -> unicode:charlist() when This :: wxListbook(), NPage :: integer().
```

Returns the string for the given page.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxListbook().
```

Returns the currently selected page, or `wxNOT\_FOUND` if none was selected.

Note that this method may return either the previously or newly selected page when called
from the `EVT_BOOKCTRL_PAGE_CHANGED` handler depending on the platform and so `wxBookCtrlEvent:getSelection/1` should be
used instead in this case.

# `hitTest`

```erlang
-spec hitTest(This, Pt) -> Result
                 when
                     Result :: {Res :: integer(), Flags :: integer()},
                     This :: wxListbook(),
                     Pt :: {X :: integer(), Y :: integer()}.
```

Returns the index of the tab at the specified position or `wxNOT\_FOUND` if none.

If `flags` parameter is non-NULL, the position of the point inside the tab is returned as well.

Return: Returns the zero-based tab index or `wxNOT_FOUND` if there is no tab at the
specified position.

# `insertPage`

```erlang
-spec insertPage(This, Index, Page, Text) -> boolean()
                    when
                        This :: wxListbook(),
                        Index :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata().
```

# `insertPage`

```erlang
-spec insertPage(This, Index, Page, Text, [Option]) -> boolean()
                    when
                        This :: wxListbook(),
                        Index :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata(),
                        Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Inserts a new page at the specified position.

Return: true if successful, false otherwise.

Remark: Do not delete the page, it will be deleted by the book control.

See: `addPage/4`

# `new`

```erlang
-spec new() -> wxListbook().
```

Default ctor.

# `new`

```erlang
-spec new(Parent, Id) -> wxListbook() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxListbook()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructs a listbook control.

# `setImageList`

```erlang
-spec setImageList(This, ImageList) -> ok
                      when This :: wxListbook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list to use.

It does not take ownership of the image list, you must delete it yourself.

See:
* `m:wxImageList`

* `assignImageList/2`

# `setPageImage`

```erlang
-spec setPageImage(This, Page, Image) -> boolean()
                      when This :: wxListbook(), Page :: integer(), Image :: integer().
```

Sets the image index for the given page.

`image` is an index into the image list which was set with `setImageList/2`.

# `setPageSize`

```erlang
-spec setPageSize(This, Size) -> ok when This :: wxListbook(), Size :: {W :: integer(), H :: integer()}.
```

Sets the width and height of the pages.

Note: This method is currently not implemented for wxGTK.

# `setPageText`

```erlang
-spec setPageText(This, Page, Text) -> boolean()
                     when This :: wxListbook(), Page :: integer(), Text :: unicode:chardata().
```

Sets the text for the given page.

# `setSelection`

```erlang
-spec setSelection(This, Page) -> integer() when This :: wxListbook(), Page :: integer().
```

Sets the selection to the given page, returning the previous selection.

Notice that the call to this function generates the page changing events, use the `changeSelection/2`
function if you don't want these events to be generated.

See: `getSelection/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
