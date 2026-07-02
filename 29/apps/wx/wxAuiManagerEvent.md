# `wxAuiManagerEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxAuiManagerEvent.erl#L58)

Event used to indicate various actions taken with `m:wxAuiManager`.

See `m:wxAuiManager` for available event types.

See:
* `m:wxAuiManager`

* `m:wxAuiPaneInfo`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxAuiManagerEvent](https://docs.wxwidgets.org/3.2/classwx_aui_manager_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxAuiManagerEventType` to subscribe to events of this type.

# `wxAuiManager`

```erlang
-type wxAuiManager() ::
          #wxAuiManager{type :: wxAuiManagerEvent:wxAuiManagerEventType(),
                        manager :: wxAuiManager:wxAuiManager(),
                        pane :: wxAuiPaneInfo:wxAuiPaneInfo(),
                        button :: integer(),
                        veto_flag :: boolean(),
                        canveto_flag :: boolean(),
                        dc :: wxDC:wxDC()}.
```

# `wxAuiManagerEvent`

```erlang
-type wxAuiManagerEvent() :: wx:wx_object().
```

# `wxAuiManagerEventType`

```erlang
-type wxAuiManagerEventType() ::
          aui_pane_button | aui_pane_close | aui_pane_maximize | aui_pane_restore | aui_pane_activated |
          aui_render | aui_find_manager.
```

# `canVeto`

```erlang
-spec canVeto(This) -> boolean() when This :: wxAuiManagerEvent().
```

Return: true if this event can be vetoed.

See: `veto/2`

# `getButton`

```erlang
-spec getButton(This) -> integer() when This :: wxAuiManagerEvent().
```

Return: The ID of the button that was clicked.

# `getDC`

```erlang
-spec getDC(This) -> wxDC:wxDC() when This :: wxAuiManagerEvent().
```

# `getManager`

```erlang
-spec getManager(This) -> wxAuiManager:wxAuiManager() when This :: wxAuiManagerEvent().
```

Return: The `m:wxAuiManager` this event is associated with.

# `getPane`

```erlang
-spec getPane(This) -> wxAuiPaneInfo:wxAuiPaneInfo() when This :: wxAuiManagerEvent().
```

Return: The pane this event is associated with.

# `getVeto`

```erlang
-spec getVeto(This) -> boolean() when This :: wxAuiManagerEvent().
```

Return: true if this event was vetoed.

See: `veto/2`

# `setButton`

```erlang
-spec setButton(This, Button) -> ok when This :: wxAuiManagerEvent(), Button :: integer().
```

Sets the ID of the button clicked that triggered this event.

# `setCanVeto`

```erlang
-spec setCanVeto(This, Can_veto) -> ok when This :: wxAuiManagerEvent(), Can_veto :: boolean().
```

Sets whether or not this event can be vetoed.

# `setDC`

```erlang
-spec setDC(This, Pdc) -> ok when This :: wxAuiManagerEvent(), Pdc :: wxDC:wxDC().
```

# `setManager`

```erlang
-spec setManager(This, Manager) -> ok
                    when This :: wxAuiManagerEvent(), Manager :: wxAuiManager:wxAuiManager().
```

Sets the `m:wxAuiManager` this event is associated with.

# `setPane`

```erlang
-spec setPane(This, Pane) -> ok when This :: wxAuiManagerEvent(), Pane :: wxAuiPaneInfo:wxAuiPaneInfo().
```

Sets the pane this event is associated with.

# `veto`

```erlang
-spec veto(This) -> ok when This :: wxAuiManagerEvent().
```

# `veto`

```erlang
-spec veto(This, [Option]) -> ok when This :: wxAuiManagerEvent(), Option :: {veto, boolean()}.
```

Cancels the action indicated by this event if `canVeto/1` is true.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
