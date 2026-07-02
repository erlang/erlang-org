# `wxToolTip`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxToolTip.erl#L58)

This class holds information about a tooltip associated with a window (see `wxWindow:setToolTip/2`).

The four static methods, `enable/1`, `setDelay/1` `wxToolTip::SetAutoPop()` (not implemented in wx) and `wxToolTip::SetReshow()`
(not implemented in wx) can be used to globally alter tooltips behaviour.

wxWidgets docs: [wxToolTip](https://docs.wxwidgets.org/3.2/classwx_tool_tip.html)

# `wxToolTip`

```erlang
-type wxToolTip() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxToolTip()) -> ok.
```

Destroys the object

# `enable`

```erlang
-spec enable(Flag) -> ok when Flag :: boolean().
```

Enable or disable tooltips globally.

Note: May not be supported on all platforms (eg. wxCocoa).

# `getTip`

```erlang
-spec getTip(This) -> unicode:charlist() when This :: wxToolTip().
```

Get the tooltip text.

# `getWindow`

```erlang
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxToolTip().
```

Get the associated window.

# `new`

```erlang
-spec new(Tip) -> wxToolTip() when Tip :: unicode:chardata().
```

Constructor.

# `setDelay`

```erlang
-spec setDelay(Msecs) -> ok when Msecs :: integer().
```

Set the delay after which the tooltip appears.

Note: May not be supported on all platforms.

# `setTip`

```erlang
-spec setTip(This, Tip) -> ok when This :: wxToolTip(), Tip :: unicode:chardata().
```

Set the tooltip text.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
