# `wxSashWindow`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSashWindow.erl#L58)

`m:wxSashWindow` allows any of its edges to have a sash which can be dragged to resize
the window.

The actual content window will be created by the application as a child of `m:wxSashWindow`.

The window (or an ancestor) will be notified of a drag via a `m:wxSashEvent` notification.

## Styles

This class supports the following styles:

* wxSW_3D: Draws a 3D effect sash and border.

* wxSW_3DSASH: Draws a 3D effect sash.

* wxSW_3DBORDER: Draws a 3D effect border.

* wxSW_BORDER: Draws a thin black border.

See:
* `m:wxSashEvent`

* `m:wxSashLayoutWindow`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSashWindow](https://docs.wxwidgets.org/3.2/classwx_sash_window.html)

## Events

Event types emitted from this class:

* [`sash_dragged`](`m:wxSashEvent`)

# `wxSashWindow`

```erlang
-type wxSashWindow() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxSashWindow()) -> ok.
```

Destroys the object

# `getMaximumSizeX`

```erlang
-spec getMaximumSizeX(This) -> integer() when This :: wxSashWindow().
```

Gets the maximum window size in the x direction.

# `getMaximumSizeY`

```erlang
-spec getMaximumSizeY(This) -> integer() when This :: wxSashWindow().
```

Gets the maximum window size in the y direction.

# `getMinimumSizeX`

```erlang
-spec getMinimumSizeX(This) -> integer() when This :: wxSashWindow().
```

Gets the minimum window size in the x direction.

# `getMinimumSizeY`

```erlang
-spec getMinimumSizeY(This) -> integer() when This :: wxSashWindow().
```

Gets the minimum window size in the y direction.

# `getSashVisible`

```erlang
-spec getSashVisible(This, Edge) -> boolean() when This :: wxSashWindow(), Edge :: wx:wx_enum().
```

Returns true if a sash is visible on the given edge, false otherwise.

See: `setSashVisible/3`

# `new`

```erlang
-spec new() -> wxSashWindow().
```

Default ctor.

# `new`

```erlang
-spec new(Parent) -> wxSashWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxSashWindow()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructs a sash window, which can be a child of a frame, dialog or any other
non-control window.

# `setMaximumSizeX`

```erlang
-spec setMaximumSizeX(This, Min) -> ok when This :: wxSashWindow(), Min :: integer().
```

Sets the maximum window size in the x direction.

# `setMaximumSizeY`

```erlang
-spec setMaximumSizeY(This, Min) -> ok when This :: wxSashWindow(), Min :: integer().
```

Sets the maximum window size in the y direction.

# `setMinimumSizeX`

```erlang
-spec setMinimumSizeX(This, Min) -> ok when This :: wxSashWindow(), Min :: integer().
```

Sets the minimum window size in the x direction.

# `setMinimumSizeY`

```erlang
-spec setMinimumSizeY(This, Min) -> ok when This :: wxSashWindow(), Min :: integer().
```

Sets the minimum window size in the y direction.

# `setSashVisible`

```erlang
-spec setSashVisible(This, Edge, Visible) -> ok
                        when This :: wxSashWindow(), Edge :: wx:wx_enum(), Visible :: boolean().
```

Call this function to make a sash visible or invisible on a particular edge.

See: `getSashVisible/2`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
