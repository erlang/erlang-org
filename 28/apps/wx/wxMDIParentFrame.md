# `wxMDIParentFrame`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMDIParentFrame.erl#L58)

An MDI (Multiple Document Interface) parent frame is a window which can contain MDI child
frames in its client area which emulates the full desktop.

MDI is a user-interface model in which all the window reside inside the single parent
window as opposed to being separate from each other. It remains popular despite dire
warnings from Microsoft itself (which popularized this model in the first model) that MDI
is obsolete.

An MDI parent frame always has a `m:wxMDIClientWindow` associated with it, which is the
parent for MDI child frames. In the simplest case, the client window takes up the entire
parent frame area but it is also possible to resize it to be smaller in order to have
other windows in the frame, a typical example is using a sidebar along one of the window edges.

The appearance of MDI applications differs between different ports. The classic MDI
model, with child windows which can be independently moved, resized etc, is only available
under MSW, which provides native support for it. In Mac ports, multiple top level windows
are used for the MDI children too and the MDI parent frame itself is invisible, to
accommodate the native look and feel requirements. In all the other ports, a tab-based MDI
implementation (sometimes called TDI) is used and so at most one MDI child is visible at
any moment (child frames are always maximized).

Although it is possible to have multiple MDI parent frames, a typical MDI application
has a single MDI parent frame window inside which multiple MDI child frames, i.e. objects
of class `m:wxMDIChildFrame`, can be created.

## Styles

This class supports the following styles:

There are no special styles for this class, all `m:wxFrame` styles apply to it in the
usual way. The only exception is that wxHSCROLL and wxVSCROLL styles apply not to the
frame itself but to the client window, so that using them enables horizontal and vertical
scrollbars for this window and not the frame.

See:
* `m:wxMDIChildFrame`

* `m:wxMDIClientWindow`

* `m:wxFrame`

* `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMDIParentFrame](https://docs.wxwidgets.org/3.2/classwx_m_d_i_parent_frame.html)

# `wxMDIParentFrame`

```elixir
-type wxMDIParentFrame() :: wx:wx_object().
```

# `activateNext`

```elixir
-spec activateNext(This) -> ok when This :: wxMDIParentFrame().
```

Activates the MDI child following the currently active one.

The MDI children are maintained in an ordered list and this function switches to the next
element in this list, wrapping around the end of it if the currently active child is the
last one.

See: `activatePrevious/1`

# `activatePrevious`

```elixir
-spec activatePrevious(This) -> ok when This :: wxMDIParentFrame().
```

Activates the MDI child preceding the currently active one.

See: `activateNext/1`

# `arrangeIcons`

```elixir
-spec arrangeIcons(This) -> ok when This :: wxMDIParentFrame().
```

Arranges any iconized (minimized) MDI child windows.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.

See:
* `cascade/1`

* `tile/2`

# `cascade`

```elixir
-spec cascade(This) -> ok when This :: wxMDIParentFrame().
```

Arranges the MDI child windows in a cascade.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.

See:
* `tile/2`

* `arrangeIcons/1`

# `create`

```elixir
-spec create(This, Parent, Id, Title) -> boolean()
                when
                    This :: wxMDIParentFrame(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Title, [Option]) -> boolean()
                when
                    This :: wxMDIParentFrame(),
                    Parent :: wxWindow:wxWindow(),
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
-spec destroy(This :: wxMDIParentFrame()) -> ok.
```

Destroys the object

# `getActiveChild`

```elixir
-spec getActiveChild(This) -> wxMDIChildFrame:wxMDIChildFrame() when This :: wxMDIParentFrame().
```

Returns a pointer to the active MDI child, if there is one.

If there are any children at all this function returns a non-NULL pointer.

# `getClientWindow`

```elixir
-spec getClientWindow(This) -> wxMDIClientWindow:wxMDIClientWindow() when This :: wxMDIParentFrame().
```

Returns a pointer to the client window.

# `new`

```elixir
-spec new() -> wxMDIParentFrame().
```

Default constructor.

Use `create/5` for the objects created using this constructor.

# `new`

```elixir
-spec new(Parent, Id, Title) -> wxMDIParentFrame()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Title :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Title, [Option]) -> wxMDIParentFrame()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Title :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating the window.

Notice that if you override virtual `OnCreateClient()` (not implemented in wx) method you
shouldn't be using this constructor but the default constructor and `create/5` as otherwise your
overridden method is never going to be called because of the usual C++ virtual call
resolution rules.

Under wxMSW, the client window will automatically have a sunken border style when the
active child is not maximized, and no border style when a child is maximized.

See: `create/5`

# `tile`

```elixir
-spec tile(This) -> ok when This :: wxMDIParentFrame().
```

# `tile`

```elixir
-spec tile(This, [Option]) -> ok when This :: wxMDIParentFrame(), Option :: {orient, wx:wx_enum()}.
```

Tiles the MDI child windows either horizontally or vertically depending on whether `orient`
is `wxHORIZONTAL` or `wxVERTICAL`.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
