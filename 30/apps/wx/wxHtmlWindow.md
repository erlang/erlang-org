# `wxHtmlWindow`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxHtmlWindow.erl#L58)

`m:wxHtmlWindow` is probably the only class you will directly use unless you want to do
something special (like adding new tag handlers or MIME filters).

The purpose of this class is to display rich content pages (either local file or
downloaded via HTTP protocol) in a window based on a subset of the HTML standard. The
width of the window is constant, given in the constructor and virtual height is changed
dynamically depending on page size. Once the window is created you can set its content by
calling `setPage/2` with raw HTML, `loadPage/2` with a `wxFileSystem` (not implemented in wx) location or `loadFile/2` with a filename.

Note: If you want complete HTML/CSS support as well as a Javascript engine, consider
using `m:wxWebView` instead.

`m:wxHtmlWindow` uses the `m:wxImage` class for displaying images, so you need to
initialize the handlers for any image formats you use before loading a page. See
?wxInitAllImageHandlers and `wxImage::AddHandler` (not implemented in wx).

## Styles

This class supports the following styles:

* wxHW_SCROLLBAR_NEVER: Never display scrollbars, not even when the page is larger than the
window.

* wxHW_SCROLLBAR_AUTO: Display scrollbars only if page's size exceeds window's size.

* wxHW_NO_SELECTION: Don't allow the user to select text.

See: `m:wxHtmlLinkEvent`

This class is derived, and can use functions, from:

* `m:wxScrolledWindow`

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxHtmlWindow](https://docs.wxwidgets.org/3.2/classwx_html_window.html)

## Events

Event types emitted from this class:

* [`html_cell_clicked`](`m:wxHtmlLinkEvent`)

* [`html_cell_hover`](`m:wxHtmlLinkEvent`)

* [`command_html_link_clicked`](`m:wxHtmlLinkEvent`)

# `wxHtmlWindow`

```erlang
-type wxHtmlWindow() :: wx:wx_object().
```

# `appendToPage`

```erlang
-spec appendToPage(This, Source) -> boolean() when This :: wxHtmlWindow(), Source :: unicode:chardata().
```

Appends HTML fragment to currently displayed text and refreshes the window.

Return: false if an error occurred, true otherwise.

# `destroy`

```erlang
-spec destroy(This :: wxHtmlWindow()) -> ok.
```

Destroys the object

# `getOpenedAnchor`

```erlang
-spec getOpenedAnchor(This) -> unicode:charlist() when This :: wxHtmlWindow().
```

Returns anchor within currently opened page (see `getOpenedPage/1`).

If no page is opened or if the displayed page wasn't produced by call to `loadPage/2`, empty string
is returned.

# `getOpenedPage`

```erlang
-spec getOpenedPage(This) -> unicode:charlist() when This :: wxHtmlWindow().
```

Returns full location of the opened page.

If no page is opened or if the displayed page wasn't produced by call to `loadPage/2`, empty string
is returned.

# `getOpenedPageTitle`

```erlang
-spec getOpenedPageTitle(This) -> unicode:charlist() when This :: wxHtmlWindow().
```

Returns title of the opened page or wxEmptyString if the current page does not contain
*<TITLE>* tag.

# `getRelatedFrame`

```erlang
-spec getRelatedFrame(This) -> wxFrame:wxFrame() when This :: wxHtmlWindow().
```

Returns the related frame.

# `historyBack`

```erlang
-spec historyBack(This) -> boolean() when This :: wxHtmlWindow().
```

Moves back to the previous page.

Only pages displayed using `loadPage/2` are stored in history list.

# `historyCanBack`

```erlang
-spec historyCanBack(This) -> boolean() when This :: wxHtmlWindow().
```

Returns true if it is possible to go back in the history i.e.

`historyBack/1` won't fail.

# `historyCanForward`

```erlang
-spec historyCanForward(This) -> boolean() when This :: wxHtmlWindow().
```

Returns true if it is possible to go forward in the history i.e.

`historyForward/1` won't fail.

# `historyClear`

```erlang
-spec historyClear(This) -> ok when This :: wxHtmlWindow().
```

Clears history.

# `historyForward`

```erlang
-spec historyForward(This) -> boolean() when This :: wxHtmlWindow().
```

Moves to next page in history.

Only pages displayed using `loadPage/2` are stored in history list.

# `loadFile`

```erlang
-spec loadFile(This, Filename) -> boolean() when This :: wxHtmlWindow(), Filename :: unicode:chardata().
```

Loads an HTML page from a file and displays it.

Return: false if an error occurred, true otherwise

See: `loadPage/2`

# `loadPage`

```erlang
-spec loadPage(This, Location) -> boolean() when This :: wxHtmlWindow(), Location :: unicode:chardata().
```

Unlike `setPage/2` this function first loads the HTML page from `location` and then
displays it.

Return: false if an error occurred, true otherwise

See: `loadFile/2`

# `new`

```erlang
-spec new() -> wxHtmlWindow().
```

Default ctor.

# `new`

```erlang
-spec new(Parent) -> wxHtmlWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxHtmlWindow()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

The parameters are the same as `wxScrolled::wxScrolled()` (not implemented in wx)
constructor.

# `selectAll`

```erlang
-spec selectAll(This) -> ok when This :: wxHtmlWindow().
```

Selects all text in the window.

See:
* `selectLine/2`

* `selectWord/2`

# `selectionToText`

```erlang
-spec selectionToText(This) -> unicode:charlist() when This :: wxHtmlWindow().
```

Returns the current selection as plain text.

Returns an empty string if no text is currently selected.

# `selectLine`

```erlang
-spec selectLine(This, Pos) -> ok when This :: wxHtmlWindow(), Pos :: {X :: integer(), Y :: integer()}.
```

Selects the line of text that `pos` points at.

Note that `pos` is relative to the top of displayed page, not to window's origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to
convert physical coordinate.

See:
* `selectAll/1`

* `selectWord/2`

# `selectWord`

```erlang
-spec selectWord(This, Pos) -> ok when This :: wxHtmlWindow(), Pos :: {X :: integer(), Y :: integer()}.
```

Selects the word at position `pos`.

Note that `pos` is relative to the top of displayed page, not to window's origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to
convert physical coordinate.

See:
* `selectAll/1`

* `selectLine/2`

# `setBorders`

```erlang
-spec setBorders(This, B) -> ok when This :: wxHtmlWindow(), B :: integer().
```

This function sets the space between border of window and HTML contents.

See image:

# `setFonts`

```erlang
-spec setFonts(This, Normal_face, Fixed_face) -> ok
                  when
                      This :: wxHtmlWindow(),
                      Normal_face :: unicode:chardata(),
                      Fixed_face :: unicode:chardata().
```

# `setFonts`

```erlang
-spec setFonts(This, Normal_face, Fixed_face, [Option]) -> ok
                  when
                      This :: wxHtmlWindow(),
                      Normal_face :: unicode:chardata(),
                      Fixed_face :: unicode:chardata(),
                      Option :: {sizes, [integer()]}.
```

This function sets font sizes and faces.

See `wxHtmlDCRenderer::SetFonts` (not implemented in wx) for detailed description.

# `setPage`

```erlang
-spec setPage(This, Source) -> boolean() when This :: wxHtmlWindow(), Source :: unicode:chardata().
```

Sets the source of a page and displays it, for example:

If you want to load a document from some location use `loadPage/2` instead.

Return: false if an error occurred, true otherwise.

# `setRelatedFrame`

```erlang
-spec setRelatedFrame(This, Frame, Format) -> ok
                         when
                             This :: wxHtmlWindow(),
                             Frame :: wxFrame:wxFrame(),
                             Format :: unicode:chardata().
```

Sets the frame in which page title will be displayed.

`format` is the format of the frame title, e.g. "HtmlHelp : %s". It must contain exactly
one s. This s is substituted with HTML page title.

# `setRelatedStatusBar`

```erlang
-spec setRelatedStatusBar(This, Statusbar) -> ok
                             when This :: wxHtmlWindow(), Statusbar :: wxStatusBar:wxStatusBar();
                         (This, Index) -> ok when This :: wxHtmlWindow(), Index :: integer().
```

`After` calling `setRelatedFrame/3`, this sets statusbar slot where messages will be
displayed.

(Default is -1 = no messages.)

# `setRelatedStatusBar`

```erlang
-spec setRelatedStatusBar(This, Statusbar, [Option]) -> ok
                             when
                                 This :: wxHtmlWindow(),
                                 Statusbar :: wxStatusBar:wxStatusBar(),
                                 Option :: {index, integer()}.
```

`Sets` the associated statusbar where messages will be displayed.

Call this instead of `setRelatedFrame/3` if you want statusbar updates only, no changing of the frame title.

Since: 2.9.0

# `toText`

```erlang
-spec toText(This) -> unicode:charlist() when This :: wxHtmlWindow().
```

Returns content of currently displayed page as plain text.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
