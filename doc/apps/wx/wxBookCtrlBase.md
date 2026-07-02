# `wxBookCtrlBase`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxBookCtrlBase.erl#L58)

A book control is a convenient way of displaying multiple pages of information, displayed
one page at a time.

wxWidgets has five variants of this control:

* `m:wxChoicebook`: controlled by a `m:wxChoice`

* `m:wxListbook`: controlled by a `m:wxListCtrl`

* `m:wxNotebook`: uses a row of tabs

* `m:wxTreebook`: controlled by a `m:wxTreeCtrl`

* `m:wxToolbook`: controlled by a `m:wxToolBar`

This abstract class is the parent of all these book controls, and provides their basic
interface. This is a pure virtual class so you cannot allocate it directly.

See: [Overview bookctrl](https://docs.wxwidgets.org/3.2/overview_bookctrl.html#overview_bookctrl)

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxBookCtrlBase](https://docs.wxwidgets.org/3.2/classwx_book_ctrl_base.html)

# `wxBookCtrlBase`

```erlang
-type wxBookCtrlBase() :: wx:wx_object().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text) -> boolean()
                 when This :: wxBookCtrlBase(), Page :: wxWindow:wxWindow(), Text :: unicode:chardata().
```

# `addPage`

```erlang
-spec addPage(This, Page, Text, [Option]) -> boolean()
                 when
                     This :: wxBookCtrlBase(),
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
-spec advanceSelection(This) -> ok when This :: wxBookCtrlBase().
```

# `advanceSelection`

```erlang
-spec advanceSelection(This, [Option]) -> ok
                          when This :: wxBookCtrlBase(), Option :: {forward, boolean()}.
```

Cycles through the tabs.

The call to this function generates the page changing events.

# `changeSelection`

```erlang
-spec changeSelection(This, Page) -> integer() when This :: wxBookCtrlBase(), Page :: integer().
```

Changes the selection to the given page, returning the previous selection.

This function behaves as `setSelection/2` but does `not` generate the page changing events.

See overview_events_prog for more information.

# `deleteAllPages`

```erlang
-spec deleteAllPages(This) -> boolean() when This :: wxBookCtrlBase().
```

Deletes all pages.

# `deletePage`

```erlang
-spec deletePage(This, Page) -> boolean() when This :: wxBookCtrlBase(), Page :: integer().
```

Deletes the specified page, and the associated window.

The call to this function generates the page changing events when deleting the currently
selected page or a page preceding it in the index order, but it does `not` send any events
when deleting the last page: while in this case the selection also changes, it becomes
invalid and for compatibility reasons the control never generates events with the invalid
selection index.

# `getCurrentPage`

```erlang
-spec getCurrentPage(This) -> wxWindow:wxWindow() when This :: wxBookCtrlBase().
```

Returns the currently selected page or NULL.

# `getPage`

```erlang
-spec getPage(This, Page) -> wxWindow:wxWindow() when This :: wxBookCtrlBase(), Page :: integer().
```

Returns the window at the given page position.

# `getPageCount`

```erlang
-spec getPageCount(This) -> integer() when This :: wxBookCtrlBase().
```

Returns the number of pages in the control.

# `getPageText`

```erlang
-spec getPageText(This, NPage) -> unicode:charlist() when This :: wxBookCtrlBase(), NPage :: integer().
```

Returns the string for the given page.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxBookCtrlBase().
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
                     This :: wxBookCtrlBase(),
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
                        This :: wxBookCtrlBase(),
                        Index :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata().
```

# `insertPage`

```erlang
-spec insertPage(This, Index, Page, Text, [Option]) -> boolean()
                    when
                        This :: wxBookCtrlBase(),
                        Index :: integer(),
                        Page :: wxWindow:wxWindow(),
                        Text :: unicode:chardata(),
                        Option :: {bSelect, boolean()} | {imageId, integer()}.
```

Inserts a new page at the specified position.

Return: true if successful, false otherwise.

Remark: Do not delete the page, it will be deleted by the book control.

See: `addPage/4`

# `removePage`

```erlang
-spec removePage(This, Page) -> boolean() when This :: wxBookCtrlBase(), Page :: integer().
```

Deletes the specified page, without deleting the associated window.

See `deletePage/2` for a note about the events generated by this function.

# `setPageText`

```erlang
-spec setPageText(This, Page, Text) -> boolean()
                     when This :: wxBookCtrlBase(), Page :: integer(), Text :: unicode:chardata().
```

Sets the text for the given page.

# `setSelection`

```erlang
-spec setSelection(This, Page) -> integer() when This :: wxBookCtrlBase(), Page :: integer().
```

Sets the selection to the given page, returning the previous selection.

Notice that the call to this function generates the page changing events, use the `changeSelection/2`
function if you don't want these events to be generated.

See: `getSelection/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
