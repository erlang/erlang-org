# `wxWebView`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxWebView.erl#L58)

This control may be used to render web (HTML / CSS / javascript) documents.

It is designed to allow the creation of multiple backends for each port, although
currently just one is available. It differs from `m:wxHtmlWindow` in that each backend is
actually a full rendering engine, Trident on MSW and Webkit on macOS and GTK. This allows
the correct viewing of complex pages with javascript and css.

Backend Descriptions

Par:

The IE backend uses Microsoft's Trident rendering engine, specifically the version used
by the locally installed copy of Internet Explorer. As such it is only available for the
MSW port. By default recent versions of the [WebBrowser](http://msdn.microsoft.com/en-us/library/aa752085%28v=VS.85%29.aspx)
control, which this backend uses, emulate Internet Explorer 7. This can be changed with a
registry setting by wxWebView::MSWSetEmulationLevel() see [this](http://msdn.microsoft.com/en-us/library/ee330730%28v=vs.85%29.aspx#browser_emulation)
article for more information. This backend has full support for custom schemes and
virtual file systems.

Par:

The Edge (Chromium) backend uses Microsoft's [Edge WebView2](https://docs.microsoft.com/en-us/microsoft-edge/hosting/webview2).
It is available for Windows 7 and newer. The following features are currently unsupported
with this backend: virtual filesystems, custom urls, find.

This backend is not enabled by default, to build it follow these steps:

* Visual Studio 2015, or newer, is required

* Download the [WebView2 SDK](https://aka.ms/webviewnuget) nuget package (Version 0.9.488
or newer)

* Extract the package (it's a zip archive) to `wxWidgets/3rdparty/webview2` (you should
have `3rdparty/webview2/build/native/include/WebView2.h` file after unpacking it)

* Enable `wxUSE_WEBVIEW_EDGE` in CMake or `setup.h`

* Build wxWidgets webview library

* Copy `WebView2Loader.dll` from the subdirectory corresponding to the architecture used
(x86 or x64) of `wxWidgets/3rdparty/webview2/build/` to your applications executable

* At runtime you can use `isBackendAvailable/1` to check if the backend can be used (it will be available if `WebView2Loader.dll`
can be loaded and Edge (Chromium) is installed)

* Make sure to add a note about using the WebView2 SDK to your application documentation,
as required by its licence

Par:

Under GTK the WebKit backend uses [WebKitGTK+](http://webkitgtk.org/). The current
minimum version required is 1.3.1 which ships by default with Ubuntu Natty and Debian
Wheezy and has the package name libwebkitgtk-dev. Custom schemes and virtual files systems
are supported under this backend, however embedded resources such as images and
stylesheets are currently loaded using the data:// scheme.

Par:

Under GTK3 the WebKit2 version of [WebKitGTK+](http://webkitgtk.org/) is used. In Ubuntu
the required package name is libwebkit2gtk-4.0-dev and under Fedora it is
webkitgtk4-devel. All wxWEBVIEW_WEBKIT features are supported except for clearing and
enabling / disabling the history.

Par:

The macOS WebKit backend uses Apple's [WebView](http://developer.apple.com/library/mac/#documentation/Cocoa/Reference/WebKit/Classes/WebView_Class/Reference/Reference.html#//apple_ref/doc/uid/20001903)
class. This backend has full support for custom schemes and virtual file systems.

Asynchronous Notifications

Many of the methods in `m:wxWebView` are asynchronous, i.e. they return immediately and
perform their work in the background. This includes functions such as `loadURL/2` and `reload/2`. To receive
notification of the progress and completion of these functions you need to handle the
events that are provided. Specifically `wxEVT_WEBVIEW_LOADED` notifies when the page or a
sub-frame has finished loading and `wxEVT_WEBVIEW_ERROR` notifies that an error has occurred.

Virtual File Systems and Custom Schemes

`m:wxWebView` supports the registering of custom scheme handlers, for example `file` or `http`.
To do this create a new class which inherits from `wxWebViewHandler` (not implemented in
wx), where wxWebHandler::GetFile() returns a pointer to a `wxFSFile` (not implemented in
wx) which represents the given url. You can then register your handler with `RegisterHandler()`
(not implemented in wx) it will be called for all pages and resources.

`wxWebViewFSHandler` (not implemented in wx) is provided to access the virtual file
system encapsulated by `wxFileSystem` (not implemented in wx). The `wxMemoryFSHandler`
(not implemented in wx) documentation gives an example of how this may be used.

`wxWebViewArchiveHandler` (not implemented in wx) is provided to allow the navigation of
pages inside a zip archive. It supports paths of the form: `scheme:///C`:/example/docs.zip;protocol=zip/main.htm

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxWebView](https://docs.wxwidgets.org/3.2/classwx_web_view.html)

## Events

Event types emitted from this class:

* [`webview_navigating`](`m:wxWebViewEvent`)

* [`webview_navigated`](`m:wxWebViewEvent`)

* [`webview_loaded`](`m:wxWebViewEvent`)

* [`webview_error`](`m:wxWebViewEvent`)

* [`webview_newwindow`](`m:wxWebViewEvent`)

* [`webview_title_changed`](`m:wxWebViewEvent`)

# `wxWebView`

```erlang
-type wxWebView() :: wx:wx_object().
```

# `canCopy`

```erlang
-spec canCopy(This) -> boolean() when This :: wxWebView().
```

Returns true if the current selection can be copied.

Note: This always returns `true` on the macOS WebKit backend.

# `canCut`

```erlang
-spec canCut(This) -> boolean() when This :: wxWebView().
```

Returns true if the current selection can be cut.

Note: This always returns `true` on the macOS WebKit backend.

# `canGoBack`

```erlang
-spec canGoBack(This) -> boolean() when This :: wxWebView().
```

Returns true if it is possible to navigate backward in the history of visited pages.

# `canGoForward`

```erlang
-spec canGoForward(This) -> boolean() when This :: wxWebView().
```

Returns true if it is possible to navigate forward in the history of visited pages.

# `canPaste`

```erlang
-spec canPaste(This) -> boolean() when This :: wxWebView().
```

Returns true if data can be pasted.

Note: This always returns `true` on the macOS WebKit backend.

# `canRedo`

```erlang
-spec canRedo(This) -> boolean() when This :: wxWebView().
```

Returns true if there is an action to redo.

# `canSetZoomType`

```erlang
-spec canSetZoomType(This, Type) -> boolean() when This :: wxWebView(), Type :: wx:wx_enum().
```

Retrieve whether the current HTML engine supports a zoom type.

Return: Whether this type of zoom is supported by this HTML engine (and thus can be set
through `setZoomType/2`).

# `canUndo`

```erlang
-spec canUndo(This) -> boolean() when This :: wxWebView().
```

Returns true if there is an action to undo.

# `clearHistory`

```erlang
-spec clearHistory(This) -> ok when This :: wxWebView().
```

Clear the history, this will also remove the visible page.

Note: This is not implemented on the WebKit2GTK+ backend.

# `clearSelection`

```erlang
-spec clearSelection(This) -> ok when This :: wxWebView().
```

Clears the current selection.

# `copy`

```erlang
-spec copy(This) -> ok when This :: wxWebView().
```

Copies the current selection.

# `cut`

```erlang
-spec cut(This) -> ok when This :: wxWebView().
```

Cuts the current selection.

# `deleteSelection`

```erlang
-spec deleteSelection(This) -> ok when This :: wxWebView().
```

Deletes the current selection.

Note that for `wxWEBVIEW_BACKEND_WEBKIT` the selection must be editable, either through
SetEditable or the correct HTML attribute.

# `enableContextMenu`

```erlang
-spec enableContextMenu(This) -> ok when This :: wxWebView().
```

# `enableContextMenu`

```erlang
-spec enableContextMenu(This, [Option]) -> ok when This :: wxWebView(), Option :: {enable, boolean()}.
```

Enable or disable the right click context menu.

By default the standard context menu is enabled, this method can be used to disable it or
re-enable it later.

Since: 2.9.5

# `enableHistory`

```erlang
-spec enableHistory(This) -> ok when This :: wxWebView().
```

# `enableHistory`

```erlang
-spec enableHistory(This, [Option]) -> ok when This :: wxWebView(), Option :: {enable, boolean()}.
```

Enable or disable the history.

This will also clear the history.

Note: This is not implemented on the WebKit2GTK+ backend.

# `find`

```erlang
-spec find(This, Text) -> integer() when This :: wxWebView(), Text :: unicode:chardata().
```

# `find`

```erlang
-spec find(This, Text, [Option]) -> integer()
              when This :: wxWebView(), Text :: unicode:chardata(), Option :: {flags, wx:wx_enum()}.
```

Finds a phrase on the current page and if found, the control will scroll the phrase into
view and select it.

Return: If search phrase was not found in combination with the flags then `wxNOT_FOUND`
is returned. If called for the first time with search phrase then the total number of
results will be returned. Then for every time its called with the same search phrase it
will return the number of the current match.

Note: This function will restart the search if the flags `wxWEBVIEW_FIND_ENTIRE_WORD` or `wxWEBVIEW_FIND_MATCH_CASE`
are changed, since this will require a new search. To reset the search, for example
resetting the highlights call the function with an empty search phrase. This always
returns `wxNOT_FOUND` on the macOS WebKit backend.

Since: 2.9.5

# `getCurrentTitle`

```erlang
-spec getCurrentTitle(This) -> unicode:charlist() when This :: wxWebView().
```

Get the title of the current web page, or its URL/path if title is not available.

# `getCurrentURL`

```erlang
-spec getCurrentURL(This) -> unicode:charlist() when This :: wxWebView().
```

Get the URL of the currently displayed document.

# `getPageSource`

```erlang
-spec getPageSource(This) -> unicode:charlist() when This :: wxWebView().
```

Get the HTML source code of the currently displayed document.

Return: The HTML source code, or an empty string if no page is currently shown.

# `getPageText`

```erlang
-spec getPageText(This) -> unicode:charlist() when This :: wxWebView().
```

Get the text of the current page.

# `getSelectedSource`

```erlang
-spec getSelectedSource(This) -> unicode:charlist() when This :: wxWebView().
```

Returns the currently selected source, if any.

# `getSelectedText`

```erlang
-spec getSelectedText(This) -> unicode:charlist() when This :: wxWebView().
```

Returns the currently selected text, if any.

# `getZoom`

```erlang
-spec getZoom(This) -> wx:wx_enum() when This :: wxWebView().
```

Get the zoom level of the page.

See `getZoomFactor/1` to get more precise zoom scale value other than as provided by `wxWebViewZoom`.

Return: The current level of zoom.

# `getZoomFactor`

```erlang
-spec getZoomFactor(This) -> number() when This :: wxWebView().
```

Get the zoom factor of the page.

Return: The current factor of zoom.

Since: 3.1.4

# `getZoomType`

```erlang
-spec getZoomType(This) -> wx:wx_enum() when This :: wxWebView().
```

Get how the zoom factor is currently interpreted.

Return: How the zoom factor is currently interpreted by the HTML engine.

# `goBack`

```erlang
-spec goBack(This) -> ok when This :: wxWebView().
```

Navigate back in the history of visited pages.

Only valid if `canGoBack/1` returns true.

# `goForward`

```erlang
-spec goForward(This) -> ok when This :: wxWebView().
```

Navigate forward in the history of visited pages.

Only valid if `canGoForward/1` returns true.

# `hasSelection`

```erlang
-spec hasSelection(This) -> boolean() when This :: wxWebView().
```

Returns true if there is a current selection.

# `isBackendAvailable`

```erlang
-spec isBackendAvailable(Backend) -> boolean() when Backend :: unicode:chardata().
```

Allows to check if a specific backend is currently available.

Since: 3.1.4

# `isBusy`

```erlang
-spec isBusy(This) -> boolean() when This :: wxWebView().
```

Returns whether the web control is currently busy (e.g. loading a page).

# `isContextMenuEnabled`

```erlang
-spec isContextMenuEnabled(This) -> boolean() when This :: wxWebView().
```

Returns true if a context menu will be shown on right click.

Since: 2.9.5

# `isEditable`

```erlang
-spec isEditable(This) -> boolean() when This :: wxWebView().
```

Returns whether the web control is currently editable.

# `loadURL`

```erlang
-spec loadURL(This, Url) -> ok when This :: wxWebView(), Url :: unicode:chardata().
```

Load a web page from a URL.

Note: Web engines generally report errors asynchronously, so if you wish to know whether
loading the URL was successful, register to receive navigation error events.

# `new`

```erlang
-spec new(Parent, Id) -> wxWebView() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxWebView()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {url, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {backend, unicode:chardata()} |
                     {style, integer()}.
```

Factory function to create a new `m:wxWebView` using a `wxWebViewFactory` (not
implemented in wx).

Return: The created `m:wxWebView`, or `NULL` if the requested backend is not available

Since: 2.9.5

# `paste`

```erlang
-spec paste(This) -> ok when This :: wxWebView().
```

Pastes the current data.

# `print`

```erlang
-spec print(This) -> ok when This :: wxWebView().
```

Opens a print dialog so that the user may print the currently displayed page.

# `redo`

```erlang
-spec redo(This) -> ok when This :: wxWebView().
```

Redos the last action.

# `reload`

```erlang
-spec reload(This) -> ok when This :: wxWebView().
```

# `reload`

```erlang
-spec reload(This, [Option]) -> ok when This :: wxWebView(), Option :: {flags, wx:wx_enum()}.
```

Reload the currently displayed URL.

Note: The flags are ignored by the edge backend.

# `runScript`

```erlang
-spec runScript(This, Javascript) -> Result
                   when
                       Result :: {Res :: boolean(), Output :: unicode:charlist()},
                       This :: wxWebView(),
                       Javascript :: unicode:chardata().
```

Runs the given JavaScript code.

JavaScript code is executed inside the browser control and has full access to DOM and
other browser-provided functionality. For example, this code will replace the current
page contents with the provided string.

If `output` is non-null, it is filled with the result of executing this code on success,
e.g. a JavaScript value such as a string, a number (integer or floating point), a boolean
or JSON representation for non-primitive types such as arrays and objects. For example:

This function has a few platform-specific limitations:

* When using WebKit v1 in wxGTK2, retrieving the result of JavaScript execution is
unsupported and this function will always return false if `output` is non-null to indicate
this. This functionality is fully supported when using WebKit v2 or later in wxGTK3.

* When using WebKit under macOS, code execution is limited to at most 10MiB of memory and
10 seconds of execution time.

* When using IE backend under MSW, scripts can only be executed when the current page is
fully loaded (i.e. `wxEVT_WEBVIEW_LOADED` event was received). A script tag inside the
page HTML is required in order to run JavaScript.

Also notice that under MSW converting JavaScript objects to JSON is not supported in the
default emulation mode. `m:wxWebView` implements its own object-to-JSON conversion as a
fallback for this case, however it is not as full-featured, well-tested or performing as
the implementation of this functionality in the browser control itself, so it is
recommended to use MSWSetEmulationLevel() to change emulation level to a more modern one
in which JSON conversion is done by the control itself.

Return: true if there is a result, false if there is an error.

# `selectAll`

```erlang
-spec selectAll(This) -> ok when This :: wxWebView().
```

Selects the entire page.

# `setEditable`

```erlang
-spec setEditable(This) -> ok when This :: wxWebView().
```

# `setEditable`

```erlang
-spec setEditable(This, [Option]) -> ok when This :: wxWebView(), Option :: {enable, boolean()}.
```

Set the editable property of the web control.

Enabling allows the user to edit the page even if the `contenteditable` attribute is not
set. The exact capabilities vary with the backend being used.

# `setPage`

```erlang
-spec setPage(This, Html, BaseUrl) -> ok
                 when This :: wxWebView(), Html :: unicode:chardata(), BaseUrl :: unicode:chardata().
```

Set the displayed page source to the contents of the given string.

Note: When using `wxWEBVIEW_BACKEND_IE` you must wait for the current page to finish
loading before calling `setPage/3`. The baseURL parameter is not used in this backend and the edge
backend.

# `setZoom`

```erlang
-spec setZoom(This, Zoom) -> ok when This :: wxWebView(), Zoom :: wx:wx_enum().
```

Set the zoom level of the page.

See `setZoomFactor/2` for more precise scaling other than the measured steps provided by `wxWebViewZoom`.

# `setZoomFactor`

```erlang
-spec setZoomFactor(This, Zoom) -> ok when This :: wxWebView(), Zoom :: number().
```

Set the zoom factor of the page.

Note: zoom scale in IE will be converted into `wxWebViewZoom` levels for `wxWebViewZoomType`
of `wxWEBVIEW_ZOOM_TYPE_TEXT`.

Since: 3.1.4

# `setZoomType`

```erlang
-spec setZoomType(This, ZoomType) -> ok when This :: wxWebView(), ZoomType :: wx:wx_enum().
```

Set how to interpret the zoom factor.

Note: invoke `canSetZoomType/2` first, some HTML renderers may not support all zoom types.

# `stop`

```erlang
-spec stop(This) -> ok when This :: wxWebView().
```

Stop the current page loading process, if any.

May trigger an error event of type `wxWEBVIEW_NAV_ERR_USER_CANCELLED`. TODO: make `wxWEBVIEW_NAV_ERR_USER_CANCELLED`
errors uniform across ports.

# `undo`

```erlang
-spec undo(This) -> ok when This :: wxWebView().
```

Undos the last action.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
