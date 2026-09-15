# `wxDisplay`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDisplay.erl#L58)

Determines the sizes and locations of displays connected to the system.

wxWidgets docs: [wxDisplay](https://docs.wxwidgets.org/3.2/classwx_display.html)

# `wxDisplay`

```erlang
-type wxDisplay() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxDisplay()) -> ok.
```

Destroys the object

# `getClientArea`

```erlang
-spec getClientArea(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                       when This :: wxDisplay().
```

Returns the client area of the display.

The client area is the part of the display available for the normal (non full screen)
windows, usually it is the same as `getGeometry/1` but it could be less if there is a taskbar (or
equivalent) on this display.

# `getCount`

```erlang
-spec getCount() -> integer().
```

Returns the number of connected displays.

# `getFromPoint`

```erlang
-spec getFromPoint(Pt) -> integer() when Pt :: {X :: integer(), Y :: integer()}.
```

Returns the index of the display on which the given point lies, or `wxNOT\_FOUND` if the
point is not on any connected display.

# `getFromWindow`

```erlang
-spec getFromWindow(Win) -> integer() when Win :: wxWindow:wxWindow().
```

Returns the index of the display on which the given window lies.

If the window is on more than one display it gets the display that overlaps the window
the most.

Returns `wxNOT_FOUND` if the window is not on any connected display.

# `getGeometry`

```erlang
-spec getGeometry(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                     when This :: wxDisplay().
```

Returns the bounding rectangle of the display whose index was passed to the constructor.

See:
* `getClientArea/1`

* `wx_misc:displaySize/0`

# `getName`

```erlang
-spec getName(This) -> unicode:charlist() when This :: wxDisplay().
```

Returns the display's name.

The returned value is currently an empty string under all platforms except MSW.

# `getPPI`

```erlang
-spec getPPI(This) -> {W :: integer(), H :: integer()} when This :: wxDisplay().
```

Returns display resolution in pixels per inch.

Horizontal and vertical resolution are returned in `x` and `y` components of the
{Width,Height} object respectively.

If the resolution information is not available, returns.

Since: 3.1.2

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxDisplay().
```

Returns true if the object was initialized successfully.

# `isPrimary`

```erlang
-spec isPrimary(This) -> boolean() when This :: wxDisplay().
```

Returns true if the display is the primary display.

The primary display is the one whose index is 0.

# `new`

```erlang
-spec new() -> wxDisplay().
```

Default constructor creating `m:wxDisplay` object representing the primary display.

# `new`

```erlang
-spec new(Index) -> wxDisplay() when Index :: integer();
         (Window) -> wxDisplay() when Window :: wxWindow:wxWindow().
```

Constructor creating the display object associated with the given window.

This is the most convenient way of finding the display on which the given window is shown
while falling back to the default display if it is not shown at all or positioned outside
of any display.

See: `getFromWindow/1`

Since: 3.1.2

---

*Consult [api-reference.md](api-reference.md) for complete listing*
