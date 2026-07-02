# `wxWebViewEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxWebViewEvent.erl#L58)

A navigation event holds information about events associated with `m:wxWebView` objects.

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxWebViewEvent](https://docs.wxwidgets.org/3.2/classwx_web_view_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxWebViewEventType` to subscribe to events of this type.

# `wxWebView`

```elixir
-type wxWebView() ::
          #wxWebView{type :: wxWebViewEvent:wxWebViewEventType(),
                     string :: unicode:chardata(),
                     int :: integer(),
                     target :: unicode:chardata(),
                     url :: unicode:chardata()}.
```

# `wxWebViewEvent`

```elixir
-type wxWebViewEvent() :: wx:wx_object().
```

# `wxWebViewEventType`

```elixir
-type wxWebViewEventType() ::
          webview_navigating | webview_navigated | webview_loaded | webview_error | webview_newwindow |
          webview_title_changed.
```

# `getInt`

```elixir
-spec getInt(This) -> integer() when This :: wxWebViewEvent().
```

Returns the integer identifier corresponding to a listbox, choice or radiobox selection
(only if the event was a selection, not a deselection), or a boolean value representing
the value of a checkbox.

For a menu item, this method returns -1 if the item is not checkable or a boolean value
(true or false) for checkable items indicating the new state of the item.

# `getString`

```elixir
-spec getString(This) -> unicode:charlist() when This :: wxWebViewEvent().
```

Returns item string for a listbox or choice selection event.

If one or several items have been deselected, returns the index of the first deselected
item. If some items have been selected and others deselected at the same time, it will
return the index of the first selected item.

# `getTarget`

```elixir
-spec getTarget(This) -> unicode:charlist() when This :: wxWebViewEvent().
```

Get the name of the target frame which the url of this event has been or will be loaded
into.

This may return an empty string if the frame is not available.

# `getURL`

```elixir
-spec getURL(This) -> unicode:charlist() when This :: wxWebViewEvent().
```

Get the URL being visited.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
