# `wxCaret`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxCaret.erl#L58)

A caret is a blinking cursor showing the position where the typed text will appear.

Text controls usually have their own caret but `m:wxCaret` provides a way to use a caret
in other windows.

Currently, the caret appears as a rectangle of the given size. In the future, it will be
possible to specify a bitmap to be used for the caret shape.

A caret is always associated with a window and the current caret can be retrieved using `wxWindow:getCaret/1`.
The same caret can't be reused in two different windows.

wxWidgets docs: [wxCaret](https://docs.wxwidgets.org/3.2/classwx_caret.html)

# `wxCaret`

```erlang
-type wxCaret() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Window, Size) -> boolean()
                when
                    This :: wxCaret(),
                    Window :: wxWindow:wxWindow(),
                    Size :: {W :: integer(), H :: integer()}.
```

# `create`

```erlang
-spec create(This, Window, Width, Height) -> boolean()
                when
                    This :: wxCaret(),
                    Window :: wxWindow:wxWindow(),
                    Width :: integer(),
                    Height :: integer().
```

Creates a caret with the given size (in pixels) and associates it with the `window` (same
as the equivalent constructors).

# `destroy`

```erlang
-spec destroy(This :: wxCaret()) -> ok.
```

Destroys the object

# `getBlinkTime`

```erlang
-spec getBlinkTime() -> integer().
```

Returns the blink time which is measured in milliseconds and is the time elapsed between
2 inversions of the caret (blink time of the caret is the same for all carets, so this
functions is static).

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxCaret().
```

# `getSize`

```erlang
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxCaret().
```

# `getWindow`

```erlang
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxCaret().
```

Get the window the caret is associated with.

# `hide`

```erlang
-spec hide(This) -> ok when This :: wxCaret().
```

Hides the caret, same as Show(false).

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxCaret().
```

Returns true if the caret was created successfully.

# `isVisible`

```erlang
-spec isVisible(This) -> boolean() when This :: wxCaret().
```

Returns true if the caret is visible and false if it is permanently hidden (if it is
blinking and not shown currently but will be after the next blink, this method still
returns true).

# `move`

```erlang
-spec move(This, Pt) -> ok when This :: wxCaret(), Pt :: {X :: integer(), Y :: integer()}.
```

# `move`

```erlang
-spec move(This, X, Y) -> ok when This :: wxCaret(), X :: integer(), Y :: integer().
```

Move the caret to given position (in logical coordinates).

# `new`

```erlang
-spec new(Window, Size) -> wxCaret()
             when Window :: wxWindow:wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

# `new`

```erlang
-spec new(Window, Width, Height) -> wxCaret()
             when Window :: wxWindow:wxWindow(), Width :: integer(), Height :: integer().
```

Creates a caret with the given size (in pixels) and associates it with the `window`.

# `setBlinkTime`

```erlang
-spec setBlinkTime(Milliseconds) -> ok when Milliseconds :: integer().
```

Sets the blink time for all the carets.

Warning:

Under Windows, this function will change the blink time for all carets permanently (until
the next time it is called), even for carets in other applications.

See: `getBlinkTime/0`

# `setSize`

```erlang
-spec setSize(This, Size) -> ok when This :: wxCaret(), Size :: {W :: integer(), H :: integer()}.
```

# `setSize`

```erlang
-spec setSize(This, Width, Height) -> ok when This :: wxCaret(), Width :: integer(), Height :: integer().
```

Changes the size of the caret.

# `show`

```erlang
-spec show(This) -> ok when This :: wxCaret().
```

# `show`

```erlang
-spec show(This, [Option]) -> ok when This :: wxCaret(), Option :: {show, boolean()}.
```

Shows or hides the caret.

Notice that if the caret was hidden N times, it must be shown N times as well to reappear
on the screen.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
