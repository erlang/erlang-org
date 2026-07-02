# `wxSizerItem`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSizerItem.erl#L58)

The `m:wxSizerItem` class is used to track the position, size and other attributes of
each item managed by a `m:wxSizer`.

It is not usually necessary to use this class because the sizer elements can also be
identified by their positions or window or sizer pointers but sometimes it may be more
convenient to use it directly.

wxWidgets docs: [wxSizerItem](https://docs.wxwidgets.org/3.2/classwx_sizer_item.html)

# `wxSizerItem`

```erlang
-type wxSizerItem() :: wx:wx_object().
```

# `assignSizer`

```erlang
-spec assignSizer(This, Sizer) -> ok when This :: wxSizerItem(), Sizer :: wxSizer:wxSizer().
```

Set the sizer tracked by this item.

Old sizer, if any, is deleted.

# `assignSpacer`

```erlang
-spec assignSpacer(This, Size) -> ok
                      when This :: wxSizerItem(), Size :: {W :: integer(), H :: integer()}.
```

Set the size of the spacer tracked by this item.

Old spacer, if any, is deleted.

# `assignSpacer`

```erlang
-spec assignSpacer(This, W, H) -> ok when This :: wxSizerItem(), W :: integer(), H :: integer().
```

# `assignWindow`

```erlang
-spec assignWindow(This, Window) -> ok when This :: wxSizerItem(), Window :: wxWindow:wxWindow().
```

Set the window to be tracked by this item.

Note: This is a low-level method which is dangerous if used incorrectly, avoid using it
if possible, i.e. if higher level methods such as `wxSizer:replace/4` can be used instead.

If the sizer item previously contained a window, it is dissociated from the sizer
containing this sizer item (if any), but this object doesn't have the pointer to the
containing sizer and so it's the caller's responsibility to call `wxWindow:setContainingSizer/2` on `window`. Failure to
do this can result in memory corruption when the window is destroyed later, so it is
crucial to not forget to do it.

Also note that the previously contained window is `not` deleted, so it's also the callers
responsibility to do it, if necessary.

# `calcMin`

```erlang
-spec calcMin(This) -> {W :: integer(), H :: integer()} when This :: wxSizerItem().
```

Calculates the minimum desired size for the item, including any space needed by borders.

# `deleteWindows`

```erlang
-spec deleteWindows(This) -> ok when This :: wxSizerItem().
```

Destroy the window or the windows in a subsizer, depending on the type of item.

# `destroy`

```erlang
-spec destroy(This :: wxSizerItem()) -> ok.
```

Destroys the object

# `detachSizer`

```erlang
-spec detachSizer(This) -> ok when This :: wxSizerItem().
```

Enable deleting the SizerItem without destroying the contained sizer.

# `getBorder`

```erlang
-spec getBorder(This) -> integer() when This :: wxSizerItem().
```

Return the border attribute.

# `getFlag`

```erlang
-spec getFlag(This) -> integer() when This :: wxSizerItem().
```

Return the flags attribute.

See `wxSizer flags list` (not implemented in wx) for details.

# `getMinSize`

```erlang
-spec getMinSize(This) -> {W :: integer(), H :: integer()} when This :: wxSizerItem().
```

Get the minimum size needed for the item.

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxSizerItem().
```

What is the current position of the item, as set in the last Layout.

# `getProportion`

```erlang
-spec getProportion(This) -> integer() when This :: wxSizerItem().
```

Get the proportion item attribute.

# `getRatio`

```erlang
-spec getRatio(This) -> number() when This :: wxSizerItem().
```

Get the ratio item attribute.

# `getRect`

```erlang
-spec getRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                 when This :: wxSizerItem().
```

Get the rectangle of the item on the parent window, excluding borders.

# `getSize`

```erlang
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxSizerItem().
```

Get the current size of the item, as set in the last Layout.

# `getSizer`

```erlang
-spec getSizer(This) -> wxSizer:wxSizer() when This :: wxSizerItem().
```

If this item is tracking a sizer, return it.

NULL otherwise.

# `getSpacer`

```erlang
-spec getSpacer(This) -> {W :: integer(), H :: integer()} when This :: wxSizerItem().
```

If this item is tracking a spacer, return its size.

# `getUserData`

```erlang
-spec getUserData(This) -> wx:wx_object() when This :: wxSizerItem().
```

Get the userData item attribute.

# `getWindow`

```erlang
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxSizerItem().
```

If this item is tracking a window then return it.

NULL otherwise.

# `isShown`

```erlang
-spec isShown(This) -> boolean() when This :: wxSizerItem().
```

Returns true if this item is a window or a spacer and it is shown or if this item is a
sizer and not all of its elements are hidden.

In other words, for sizer items, all of the child elements must be hidden for the sizer
itself to be considered hidden.

As an exception, if the `wxRESERVE_SPACE_EVEN_IF_HIDDEN` flag was used for this sizer
item, then `isShown/1` always returns true for it (see `wxSizerFlags::ReserveSpaceEvenIfHidden()`
(not implemented in wx)).

# `isSizer`

```erlang
-spec isSizer(This) -> boolean() when This :: wxSizerItem().
```

Is this item a sizer?

# `isSpacer`

```erlang
-spec isSpacer(This) -> boolean() when This :: wxSizerItem().
```

Is this item a spacer?

# `isWindow`

```erlang
-spec isWindow(This) -> boolean() when This :: wxSizerItem().
```

Is this item a window?

# `new`

```erlang
-spec new(Window) -> wxSizerItem() when Window :: wxWindow:wxWindow() | wxSizer:wxSizer().
```

# `new`

```erlang
-spec new(Width, Height) -> wxSizerItem() when Width :: integer(), Height :: integer();
         (Window, Flags) -> wxSizerItem()
             when
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(), Flags :: wxSizerFlags:wxSizerFlags();
         (Window, [Option]) -> wxSizerItem()
             when
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                 Option ::
                     {proportion, integer()} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()}.
```

# `new`

```erlang
-spec new(Width, Height, [Option]) -> wxSizerItem()
             when
                 Width :: integer(),
                 Height :: integer(),
                 Option ::
                     {proportion, integer()} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()}.
```

Construct a sizer item for tracking a spacer.

# `setBorder`

```erlang
-spec setBorder(This, Border) -> ok when This :: wxSizerItem(), Border :: integer().
```

Set the border item attribute.

# `setDimension`

```erlang
-spec setDimension(This, Pos, Size) -> ok
                      when
                          This :: wxSizerItem(),
                          Pos :: {X :: integer(), Y :: integer()},
                          Size :: {W :: integer(), H :: integer()}.
```

Set the position and size of the space allocated to the sizer, and adjust the position
and size of the item to be within that space taking alignment and borders into account.

# `setFlag`

```erlang
-spec setFlag(This, Flag) -> ok when This :: wxSizerItem(), Flag :: integer().
```

Set the flag item attribute.

# `setInitSize`

```erlang
-spec setInitSize(This, X, Y) -> ok when This :: wxSizerItem(), X :: integer(), Y :: integer().
```

Sets the minimum size to be allocated for this item.

This is identical to `setMinSize/3`, prefer to use the other function, as its name is more clear.

# `setMinSize`

```erlang
-spec setMinSize(This, Size) -> ok when This :: wxSizerItem(), Size :: {W :: integer(), H :: integer()}.
```

Sets the minimum size to be allocated for this item.

If this item is a window, the `size` is also passed to `wxWindow:setMinSize/2`.

# `setMinSize`

```erlang
-spec setMinSize(This, X, Y) -> ok when This :: wxSizerItem(), X :: integer(), Y :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setProportion`

```erlang
-spec setProportion(This, Proportion) -> ok when This :: wxSizerItem(), Proportion :: integer().
```

Set the proportion item attribute.

# `setRatio`

```erlang
-spec setRatio(This, Ratio) -> ok when This :: wxSizerItem(), Ratio :: number();
              (This, Size) -> ok when This :: wxSizerItem(), Size :: {W :: integer(), H :: integer()}.
```

# `setRatio`

```erlang
-spec setRatio(This, Width, Height) -> ok
                  when This :: wxSizerItem(), Width :: integer(), Height :: integer().
```

Set the ratio item attribute.

# `show`

```erlang
-spec show(This, Show) -> ok when This :: wxSizerItem(), Show :: boolean().
```

Set the show item attribute, which sizers use to determine if the item is to be made part
of the layout or not.

If the item is tracking a window then it is shown or hidden as needed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
