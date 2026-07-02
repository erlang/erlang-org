# `wxContextMenuEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxContextMenuEvent.erl#L58)

This class is used for context menu events, sent to give the application a chance to show
a context (popup) menu for a `m:wxWindow`.

Note that if `getPosition/1` returns wxDefaultPosition, this means that the event originated from a
keyboard context button event, and you should compute a suitable position yourself, for
example by calling `wx_misc:getMousePosition/0`.

Notice that the exact sequence of mouse events is different across the platforms. For
example, under MSW the context menu event is generated after `EVT_RIGHT_UP` event and only
if it was not handled but under GTK the context menu event is generated after `EVT_RIGHT_DOWN`
event. This is correct in the sense that it ensures that the context menu is shown
according to the current platform UI conventions and also means that you must not handle
(or call `wxEvent:skip/2` in your handler if you do have one) neither right mouse down nor right mouse up
event if you plan on handling `EVT_CONTEXT_MENU` event.

See:
* `m:wxCommandEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxContextMenuEvent](https://docs.wxwidgets.org/3.2/classwx_context_menu_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxContextMenuEventType` to subscribe to events of this type.

# `wxContextMenu`

```erlang
-type wxContextMenu() ::
          #wxContextMenu{type :: wxContextMenuEvent:wxContextMenuEventType(),
                         pos :: {X :: integer(), Y :: integer()}}.
```

# `wxContextMenuEvent`

```erlang
-type wxContextMenuEvent() :: wx:wx_object().
```

# `wxContextMenuEventType`

```erlang
-type wxContextMenuEventType() :: context_menu.
```

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxContextMenuEvent().
```

Returns the position in screen coordinates at which the menu should be shown.

Use `wxWindow:screenToClient/2` to convert to client coordinates.

You can also omit a position from `wxWindow:popupMenu/4` in order to use the current mouse pointer position.

If the event originated from a keyboard event, the value returned from this function will
be wxDefaultPosition.

# `setPosition`

```erlang
-spec setPosition(This, Point) -> ok
                     when This :: wxContextMenuEvent(), Point :: {X :: integer(), Y :: integer()}.
```

Sets the position at which the menu should be shown.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
