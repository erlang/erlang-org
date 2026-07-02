# `wxSizer`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxSizer.erl#L58)

`m:wxSizer` is the abstract base class used for laying out subwindows in a window.

You cannot use `m:wxSizer` directly; instead, you will have to use one of the sizer
classes derived from it. Currently there are `m:wxBoxSizer`, `m:wxStaticBoxSizer`, `m:wxGridSizer`, `m:wxFlexGridSizer`, `wxWrapSizer`
(not implemented in wx) and `m:wxGridBagSizer`.

The layout algorithm used by sizers in wxWidgets is closely related to layout in other
GUI toolkits, such as Java's AWT, the GTK toolkit or the Qt toolkit. It is based upon the
idea of the individual subwindows reporting their minimal required size and their ability
to get stretched if the size of the parent window has changed.

This will most often mean that the programmer does not set the original size of a dialog
in the beginning, rather the dialog will be assigned a sizer and this sizer will be
queried about the recommended size. The sizer in turn will query its children, which can
be normal windows, empty space or other sizers, so that a hierarchy of sizers can be
constructed. Note that `m:wxSizer` does not derive from `m:wxWindow` and thus does not
interfere with tab ordering and requires very little resources compared to a real window
on screen.

What makes sizers so well fitted for use in wxWidgets is the fact that every control
reports its own minimal size and the algorithm can handle differences in font sizes or
different window (dialog item) sizes on different platforms without problems. If e.g. the
standard font as well as the overall design of Motif widgets requires more space than on
Windows, the initial dialog size will automatically be bigger on Motif than on Windows.

Sizers may also be used to control the layout of custom drawn items on the window. The `add/4`, `insert/5`,
and `prepend/4` functions return a pointer to the newly added `m:wxSizerItem`. Just add empty space
of the desired size and attributes, and then use the `wxSizerItem:getRect/1` method to determine where the
drawing operations should take place.

Please notice that sizers, like child windows, are owned by the library and will be
deleted by it which implies that they must be allocated on the heap. However if you create
a sizer and do not add it to another sizer or window, the library wouldn't be able to
delete such an orphan sizer and in this, and only this, case it should be deleted explicitly.

wxSizer flags

The "flag" argument accepted by `m:wxSizerItem` constructors and other functions, e.g. `add/4`,
is an OR-combination of the following flags. Two main behaviours are defined using these
flags. One is the border around a window: the border parameter determines the border width
whereas the flags given here determine which side(s) of the item that the border will be
added. The other flags determine how the sizer item behaves when the space allotted to the
sizer changes, and is somewhat dependent on the specific kind of sizer used.

See: [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

wxWidgets docs: [wxSizer](https://docs.wxwidgets.org/3.2/classwx_sizer.html)

# `wxSizer`

```elixir
-type wxSizer() :: wx:wx_object().
```

# `add`

```elixir
-spec add(This, Window) -> wxSizerItem:wxSizerItem()
             when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer().
```

# `add`

```elixir
-spec add(This, Width, Height) -> wxSizerItem:wxSizerItem()
             when This :: wxSizer(), Width :: integer(), Height :: integer();
         (This, Window, Flags) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxSizer(),
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                 Flags :: wxSizerFlags:wxSizerFlags();
         (This, Window, [Option]) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxSizer(),
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                 Option ::
                     {proportion, integer()} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()}.
```

Appends a child to the sizer.

`m:wxSizer` itself is an abstract class, but the parameters are equivalent in the derived
classes that you will instantiate to use it so they are described here:

# `add`

```elixir
-spec add(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxSizer(),
                 Width :: integer(),
                 Height :: integer(),
                 Option ::
                     {proportion, integer()} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()};
         (This, Width, Height, Flags) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxSizer(),
                 Width :: integer(),
                 Height :: integer(),
                 Flags :: wxSizerFlags:wxSizerFlags().
```

Appends a spacer child to the sizer.

# `addSpacer`

```elixir
-spec addSpacer(This, Size) -> wxSizerItem:wxSizerItem() when This :: wxSizer(), Size :: integer().
```

This base function adds non-stretchable space to both the horizontal and vertical
orientation of the sizer.

More readable way of calling:

See: `addSpacer/2`

# `addStretchSpacer`

```elixir
-spec addStretchSpacer(This) -> wxSizerItem:wxSizerItem() when This :: wxSizer().
```

# `addStretchSpacer`

```elixir
-spec addStretchSpacer(This, [Option]) -> wxSizerItem:wxSizerItem()
                          when This :: wxSizer(), Option :: {prop, integer()}.
```

Adds stretchable space to the sizer.

More readable way of calling:

# `calcMin`

```elixir
-spec calcMin(This) -> {W :: integer(), H :: integer()} when This :: wxSizer().
```

This method is abstract and has to be overwritten by any derived class.

Here, the sizer will do the actual calculation of its children's minimal sizes.

# `clear`

```elixir
-spec clear(This) -> ok when This :: wxSizer().
```

# `clear`

```elixir
-spec clear(This, [Option]) -> ok when This :: wxSizer(), Option :: {delete_windows, boolean()}.
```

Detaches all children from the sizer.

If `delete_windows` is true then child windows will also be deleted.

Notice that child sizers are always deleted, as a general consequence of the principle
that sizers own their sizer children, but don't own their window children (because they
are already owned by their parent windows).

# `detach`

```elixir
-spec detach(This, Window) -> boolean()
                when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
            (This, Index) -> boolean() when This :: wxSizer(), Index :: integer().
```

Detach a item at position `index` from the sizer without destroying it.

This method does not cause any layout or resizing to take place, call `layout/1` to update the
layout "on screen" after detaching a child from the sizer. Returns true if the child item
was found and detached, false otherwise.

See: `remove/2`

# `fit`

```elixir
-spec fit(This, Window) -> {W :: integer(), H :: integer()}
             when This :: wxSizer(), Window :: wxWindow:wxWindow().
```

Tell the sizer to resize the `window` so that its client area matches the sizer's minimal
size (`ComputeFittingClientSize()` (not implemented in wx) is called to determine it).

This is commonly done in the constructor of the window itself, see sample in the
description of `m:wxBoxSizer`.

Return: The new window size.

# `fitInside`

```elixir
-spec fitInside(This, Window) -> ok when This :: wxSizer(), Window :: wxWindow:wxWindow().
```

Tell the sizer to resize the virtual size of the `window` to match the sizer's minimal
size.

This will not alter the on screen size of the window, but may cause the
addition/removal/alteration of scrollbars required to view the virtual area in windows
which manage it.

See:
* `wxScrolledWindow:setScrollbars/6`

* `setVirtualSizeHints/2`

# `getChildren`

```elixir
-spec getChildren(This) -> [wxSizerItem:wxSizerItem()] when This :: wxSizer().
```

# `getItem`

```elixir
-spec getItem(This, Window) -> wxSizerItem:wxSizerItem()
                 when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
             (This, Index) -> wxSizerItem:wxSizerItem() when This :: wxSizer(), Index :: integer().
```

Finds `m:wxSizerItem` which is located in the sizer at position `index`.

Use parameter `recursive` to search in subsizers too. Returns pointer to item or NULL.

# `getItem`

```elixir
-spec getItem(This, Window, [Option]) -> wxSizerItem:wxSizerItem()
                 when
                     This :: wxSizer(),
                     Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Option :: {recursive, boolean()}.
```

Finds `m:wxSizerItem` which holds the given `window`.

Use parameter `recursive` to search in subsizers too. Returns pointer to item or NULL.

# `getMinSize`

```elixir
-spec getMinSize(This) -> {W :: integer(), H :: integer()} when This :: wxSizer().
```

Returns the minimal size of the sizer.

This is either the combined minimal size of all the children and their borders or the
minimal size set by `setMinSize/3`, depending on which is bigger. Note that the returned value is client
size, not window size. In particular, if you use the value to set toplevel window's
minimal or actual size, use `wxWindow::SetMinClientSize()` (not implemented in wx) or `wxWindow:setClientSize/3`,
not `wxWindow:setMinSize/2` or `wxWindow:setSize/6`.

# `getPosition`

```elixir
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxSizer().
```

Returns the current position of the sizer.

# `getSize`

```elixir
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxSizer().
```

Returns the current size of the sizer.

# `hide`

```elixir
-spec hide(This, Window) -> boolean()
              when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
          (This, Index) -> boolean() when This :: wxSizer(), Index :: integer().
```

Hides the item at position `index`.

To make a sizer item disappear, use `hide/3` followed by `layout/1`.

Use parameter `recursive` to hide elements found in subsizers. Returns true if the child
item was found, false otherwise.

See:
* `isShown/2`

* `show/3`

# `hide`

```elixir
-spec hide(This, Window, [Option]) -> boolean()
              when
                  This :: wxSizer(),
                  Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                  Option :: {recursive, boolean()}.
```

Hides the child `window`.

To make a sizer item disappear, use `hide/3` followed by `layout/1`.

Use parameter `recursive` to hide elements found in subsizers. Returns true if the child
item was found, false otherwise.

See:
* `isShown/2`

* `show/3`

# `insert`

```elixir
-spec insert(This, Index, Item) -> wxSizerItem:wxSizerItem()
                when This :: wxSizer(), Index :: integer(), Item :: wxSizerItem:wxSizerItem().
```

# `insert`

```elixir
-spec insert(This, Index, Width, Height) -> wxSizerItem:wxSizerItem()
                when This :: wxSizer(), Index :: integer(), Width :: integer(), Height :: integer();
            (This, Index, Window, Flags) -> wxSizerItem:wxSizerItem()
                when
                    This :: wxSizer(),
                    Index :: integer(),
                    Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                    Flags :: wxSizerFlags:wxSizerFlags();
            (This, Index, Window, [Option]) -> wxSizerItem:wxSizerItem()
                when
                    This :: wxSizer(),
                    Index :: integer(),
                    Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                    Option ::
                        {proportion, integer()} |
                        {flag, integer()} |
                        {border, integer()} |
                        {userData, wx:wx_object()}.
```

Insert a child into the sizer before any existing item at `index`.

See `add/4` for the meaning of the other parameters.

# `insert`

```elixir
-spec insert(This, Index, Width, Height, [Option]) -> wxSizerItem:wxSizerItem()
                when
                    This :: wxSizer(),
                    Index :: integer(),
                    Width :: integer(),
                    Height :: integer(),
                    Option ::
                        {proportion, integer()} |
                        {flag, integer()} |
                        {border, integer()} |
                        {userData, wx:wx_object()};
            (This, Index, Width, Height, Flags) -> wxSizerItem:wxSizerItem()
                when
                    This :: wxSizer(),
                    Index :: integer(),
                    Width :: integer(),
                    Height :: integer(),
                    Flags :: wxSizerFlags:wxSizerFlags().
```

Insert a child into the sizer before any existing item at `index`.

See `add/4` for the meaning of the other parameters.

# `insertSpacer`

```elixir
-spec insertSpacer(This, Index, Size) -> wxSizerItem:wxSizerItem()
                      when This :: wxSizer(), Index :: integer(), Size :: integer().
```

Inserts non-stretchable space to the sizer.

More readable way of calling wxSizer::Insert(index, size, size).

# `insertStretchSpacer`

```elixir
-spec insertStretchSpacer(This, Index) -> wxSizerItem:wxSizerItem()
                             when This :: wxSizer(), Index :: integer().
```

# `insertStretchSpacer`

```elixir
-spec insertStretchSpacer(This, Index, [Option]) -> wxSizerItem:wxSizerItem()
                             when This :: wxSizer(), Index :: integer(), Option :: {prop, integer()}.
```

Inserts stretchable space to the sizer.

More readable way of calling wxSizer::Insert(0, 0, prop).

# `isShown`

```elixir
-spec isShown(This, Window) -> boolean()
                 when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
             (This, Index) -> boolean() when This :: wxSizer(), Index :: integer().
```

Returns true if the item at `index` is shown.

See:
* `hide/3`

* `show/3`

* `wxSizerItem:isShown/1`

# `layout`

```elixir
-spec layout(This) -> ok when This :: wxSizer().
```

Call this to force layout of the children anew, e.g. after having added a child to or
removed a child (window, other sizer or space) from the sizer while keeping the current
dimension.

# `prepend`

```elixir
-spec prepend(This, Item) -> wxSizerItem:wxSizerItem()
                 when This :: wxSizer(), Item :: wxSizerItem:wxSizerItem().
```

# `prepend`

```elixir
-spec prepend(This, Width, Height) -> wxSizerItem:wxSizerItem()
                 when This :: wxSizer(), Width :: integer(), Height :: integer();
             (This, Window, Flags) -> wxSizerItem:wxSizerItem()
                 when
                     This :: wxSizer(),
                     Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Flags :: wxSizerFlags:wxSizerFlags();
             (This, Window, [Option]) -> wxSizerItem:wxSizerItem()
                 when
                     This :: wxSizer(),
                     Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Option ::
                         {proportion, integer()} |
                         {flag, integer()} |
                         {border, integer()} |
                         {userData, wx:wx_object()}.
```

Same as `add/4`, but prepends the items to the beginning of the list of items (windows,
subsizers or spaces) owned by this sizer.

# `prepend`

```elixir
-spec prepend(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem()
                 when
                     This :: wxSizer(),
                     Width :: integer(),
                     Height :: integer(),
                     Option ::
                         {proportion, integer()} |
                         {flag, integer()} |
                         {border, integer()} |
                         {userData, wx:wx_object()};
             (This, Width, Height, Flags) -> wxSizerItem:wxSizerItem()
                 when
                     This :: wxSizer(),
                     Width :: integer(),
                     Height :: integer(),
                     Flags :: wxSizerFlags:wxSizerFlags().
```

Same as `add/4`, but prepends the items to the beginning of the list of items (windows,
subsizers or spaces) owned by this sizer.

# `prependSpacer`

```elixir
-spec prependSpacer(This, Size) -> wxSizerItem:wxSizerItem() when This :: wxSizer(), Size :: integer().
```

Prepends non-stretchable space to the sizer.

More readable way of calling wxSizer::Prepend(size, size, 0).

# `prependStretchSpacer`

```elixir
-spec prependStretchSpacer(This) -> wxSizerItem:wxSizerItem() when This :: wxSizer().
```

# `prependStretchSpacer`

```elixir
-spec prependStretchSpacer(This, [Option]) -> wxSizerItem:wxSizerItem()
                              when This :: wxSizer(), Option :: {prop, integer()}.
```

Prepends stretchable space to the sizer.

More readable way of calling wxSizer::Prepend(0, 0, prop).

# `recalcSizes`

```elixir
-spec recalcSizes(This) -> ok when This :: wxSizer().
```

Equivalent to: `layout/1`

# `remove`

```elixir
-spec remove(This, Index) -> boolean() when This :: wxSizer(), Index :: integer();
            (This, Sizer) -> boolean() when This :: wxSizer(), Sizer :: wxSizer().
```

Removes a sizer child from the sizer and destroys it.

Note: This method does not cause any layout or resizing to take place, call `layout/1` to update
the layout "on screen" after removing a child from the sizer.

Return: true if the child item was found and removed, false otherwise.

# `replace`

```elixir
-spec replace(This, Oldwin, Newwin) -> boolean()
                 when
                     This :: wxSizer(),
                     Oldwin :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Newwin :: wxWindow:wxWindow() | wxSizer:wxSizer();
             (This, Index, Newitem) -> boolean()
                 when This :: wxSizer(), Index :: integer(), Newitem :: wxSizerItem:wxSizerItem().
```

Detaches the given item at position `index` from the sizer and replaces it with the given `m:wxSizerItem`
`newitem`.

The detached child is deleted `only` if it is a sizer or a spacer (but not if it is a `m:wxWindow`
because windows are owned by their parent window, not the sizer).

This method does not cause any layout or resizing to take place, call `layout/1` to update the
layout "on screen" after replacing a child from the sizer.

Returns true if the child item was found and removed, false otherwise.

# `replace`

```elixir
-spec replace(This, Oldwin, Newwin, [Option]) -> boolean()
                 when
                     This :: wxSizer(),
                     Oldwin :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Newwin :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                     Option :: {recursive, boolean()}.
```

Detaches the given `oldwin` from the sizer and replaces it with the given `newwin`.

The detached child window is `not` deleted (because windows are owned by their parent
window, not the sizer).

Use parameter `recursive` to search the given element recursively in subsizers.

This method does not cause any layout or resizing to take place, call `layout/1` to update the
layout "on screen" after replacing a child from the sizer.

Returns true if the child item was found and removed, false otherwise.

# `setDimension`

```elixir
-spec setDimension(This, Pos, Size) -> ok
                      when
                          This :: wxSizer(),
                          Pos :: {X :: integer(), Y :: integer()},
                          Size :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setDimension`

```elixir
-spec setDimension(This, X, Y, Width, Height) -> ok
                      when
                          This :: wxSizer(),
                          X :: integer(),
                          Y :: integer(),
                          Width :: integer(),
                          Height :: integer().
```

Call this to force the sizer to take the given dimension and thus force the items owned
by the sizer to resize themselves according to the rules defined by the parameter in the `add/4`
and `prepend/4` methods.

# `setItemMinSize`

```elixir
-spec setItemMinSize(This, Window, Size) -> boolean()
                        when
                            This :: wxSizer(),
                            Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                            Size :: {W :: integer(), H :: integer()};
                    (This, Index, Size) -> boolean()
                        when
                            This :: wxSizer(),
                            Index :: integer(),
                            Size :: {W :: integer(), H :: integer()}.
```

# `setItemMinSize`

```elixir
-spec setItemMinSize(This, Window, Width, Height) -> boolean()
                        when
                            This :: wxSizer(),
                            Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                            Width :: integer(),
                            Height :: integer();
                    (This, Index, Width, Height) -> boolean()
                        when
                            This :: wxSizer(),
                            Index :: integer(),
                            Width :: integer(),
                            Height :: integer().
```

# `setMinSize`

```elixir
-spec setMinSize(This, Size) -> ok when This :: wxSizer(), Size :: {W :: integer(), H :: integer()}.
```

Call this to give the sizer a minimal size.

Normally, the sizer will calculate its minimal size based purely on how much space its
children need. After calling this method `getMinSize/1` will return either the minimal size as requested
by its children or the minimal size set here, depending on which is bigger.

# `setMinSize`

```elixir
-spec setMinSize(This, Width, Height) -> ok
                    when This :: wxSizer(), Width :: integer(), Height :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setSizeHints`

```elixir
-spec setSizeHints(This, Window) -> ok when This :: wxSizer(), Window :: wxWindow:wxWindow().
```

This method first calls `fit/2` and then `setSizeHints/2` on the `window` passed to it.

This only makes sense when `window` is actually a `m:wxTopLevelWindow` such as a `m:wxFrame`
or a `m:wxDialog`, since SetSizeHints only has any effect in these classes. It does
nothing in normal windows or controls.

This method is implicitly used by `wxWindow:setSizerAndFit/3` which is commonly invoked in the constructor of a
toplevel window itself (see the sample in the description of `m:wxBoxSizer`) if the
toplevel window is resizable.

# `setVirtualSizeHints`

```elixir
-spec setVirtualSizeHints(This, Window) -> ok when This :: wxSizer(), Window :: wxWindow:wxWindow().
```

Equivalent to: `fitInside/2`

# `show`

```elixir
-spec show(This, Window) -> boolean()
              when This :: wxSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
          (This, Index) -> boolean() when This :: wxSizer(), Index :: integer();
          (This, Show) -> ok when This :: wxSizer(), Show :: boolean().
```

# `show`

```elixir
-spec show(This, Window, [Option]) -> boolean()
              when
                  This :: wxSizer(),
                  Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                  Option :: {show, boolean()} | {recursive, boolean()};
          (This, Index, [Option]) -> boolean()
              when This :: wxSizer(), Index :: integer(), Option :: {show, boolean()}.
```

Shows the item at `index`.

To make a sizer item disappear or reappear, use `show/3` followed by `layout/1`.

Returns true if the child item was found, false otherwise.

See:
* `hide/3`

* `isShown/2`

# `showItems`

```elixir
-spec showItems(This, Show) -> ok when This :: wxSizer(), Show :: boolean().
```

Show or hide all items managed by the sizer.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
