# `wxPanel`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPanel.erl#L58)

A panel is a window on which controls are placed.

It is usually placed within a frame. Its main feature over its parent class `m:wxWindow`
is code for handling child windows and TAB traversal, which is implemented natively if
possible (e.g. in wxGTK) or by wxWidgets itself otherwise.

Note: Tab traversal is implemented through an otherwise undocumented intermediate
wxControlContainer class from which any class can derive in addition to the normal `m:wxWindow`
base class. Please see and to find out how this is achieved.

Note: if not all characters are being intercepted by your OnKeyDown or OnChar handler, it
may be because you are using the `wxTAB_TRAVERSAL` style, which grabs some keypresses for
use by child controls.

Remark: By default, a panel has the same colouring as a dialog.

See: `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPanel](https://docs.wxwidgets.org/3.2/classwx_panel.html)

## Events

Event types emitted from this class:

* [`navigation_key`](`m:wxNavigationKeyEvent`)

# `wxPanel`

```elixir
-type wxPanel() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPanel()) -> ok.
```

Destroys the object

# `initDialog`

```elixir
-spec initDialog(This) -> ok when This :: wxPanel().
```

Sends a `m:wxInitDialogEvent`, which in turn transfers data to the dialog via validators.

See: `m:wxInitDialogEvent`

# `new`

```elixir
-spec new() -> wxPanel().
```

Default constructor.

# `new`

```elixir
-spec new(Parent) -> wxPanel() when Parent :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Parent, [Option]) -> wxPanel()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {winid, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

# `setFocusIgnoringChildren`

```elixir
-spec setFocusIgnoringChildren(This) -> ok when This :: wxPanel().
```

In contrast to `wxWindow:setFocus/1` (see above) this will set the focus to the panel
even if there are child windows in the panel.

This is only rarely needed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
