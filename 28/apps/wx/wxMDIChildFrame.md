# `wxMDIChildFrame`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMDIChildFrame.erl#L58)

An MDI child frame is a frame that can only exist inside a `m:wxMDIClientWindow`, which
is itself a child of `m:wxMDIParentFrame`.

## Styles

This class supports the following styles:

All of the standard `m:wxFrame` styles can be used but most of them are ignored by
TDI-based MDI implementations.

Remark: Although internally an MDI child frame is a child of the MDI client window, in
wxWidgets you create it as a child of `m:wxMDIParentFrame`. In fact, you can usually
forget that the client window exists. MDI child frames are clipped to the area of the MDI
client window, and may be iconized on the client window. You can associate a menubar with
a child frame as usual, although an MDI child doesn't display its menubar under its own
title bar. The MDI parent frame's menubar will be changed to reflect the currently active
child frame. If there are currently no children, the parent frame's own menubar will be displayed.

See:
* `m:wxMDIClientWindow`

* `m:wxMDIParentFrame`

* `m:wxFrame`

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMDIChildFrame](https://docs.wxwidgets.org/3.2/classwx_m_d_i_child_frame.html)

# `wxMDIChildFrame`

```elixir
-type wxMDIChildFrame() :: wx:wx_object().
```

# `activate`

```elixir
-spec activate(This) -> ok when This :: wxMDIChildFrame().
```

Activates this MDI child frame.

See:
* `maximize/2`

* `restore/1`

# `create`

```elixir
-spec create(This, Parent, Id, Title) -> boolean()
                when
                    This :: wxMDIChildFrame(),
                    Parent :: wxMDIParentFrame:wxMDIParentFrame(),
                    Id :: integer(),
                    Title :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Title, [Option]) -> boolean()
                when
                    This :: wxMDIChildFrame(),
                    Parent :: wxMDIParentFrame:wxMDIParentFrame(),
                    Id :: integer(),
                    Title :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Used in two-step frame construction.

See `new/4` for further details.

# `destroy`

```elixir
-spec destroy(This :: wxMDIChildFrame()) -> ok.
```

Destroys the object

# `maximize`

```elixir
-spec maximize(This) -> ok when This :: wxMDIChildFrame().
```

# `maximize`

```elixir
-spec maximize(This, [Option]) -> ok when This :: wxMDIChildFrame(), Option :: {maximize, boolean()}.
```

Maximizes this MDI child frame.

This function doesn't do anything if `IsAlwaysMaximized()` (not implemented in wx)
returns true.

See:
* `activate/1`

* `restore/1`

# `new`

```elixir
-spec new() -> wxMDIChildFrame().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id, Title) -> wxMDIChildFrame()
             when
                 Parent :: wxMDIParentFrame:wxMDIParentFrame(),
                 Id :: integer(),
                 Title :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Title, [Option]) -> wxMDIChildFrame()
             when
                 Parent :: wxMDIParentFrame:wxMDIParentFrame(),
                 Id :: integer(),
                 Title :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating the window.

See: `create/5`

# `restore`

```elixir
-spec restore(This) -> ok when This :: wxMDIChildFrame().
```

Restores this MDI child frame (unmaximizes).

This function doesn't do anything if `IsAlwaysMaximized()` (not implemented in wx)
returns true.

See:
* `activate/1`

* `maximize/2`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
