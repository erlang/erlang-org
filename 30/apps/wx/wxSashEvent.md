# `wxSashEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxSashEvent.erl#L58)

A sash event is sent when the sash of a `m:wxSashWindow` has been dragged by the user.

Remark: When a sash belonging to a sash window is dragged by the user, and then released,
this event is sent to the window, where it may be processed by an event table entry in a
derived class, a plug-in event handler or an ancestor class. Note that the `m:wxSashWindow`
doesn't change the window's size itself. It relies on the application's event handler to
do that. This is because the application may have to handle other consequences of the
resize, or it may wish to veto it altogether. The event handler should look at the drag
rectangle: see `getDragRect/1` to see what the new size of the window would be if the resize were to be
applied. It should also call `getDragStatus/1` to see whether the drag was OK or out of the current allowed range.

See:
* `m:wxSashWindow`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxSashEvent](https://docs.wxwidgets.org/3.2/classwx_sash_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxSashEventType` to subscribe to events of this type.

# `wxSash`

```erlang
-type wxSash() ::
          #wxSash{type :: wxSashEvent:wxSashEventType(),
                  edge :: wx:wx_enum(),
                  dragRect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                  dragStatus :: wx:wx_enum()}.
```

# `wxSashEvent`

```erlang
-type wxSashEvent() :: wx:wx_object().
```

# `wxSashEventType`

```erlang
-type wxSashEventType() :: sash_dragged.
```

# `getDragRect`

```erlang
-spec getDragRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                     when This :: wxSashEvent().
```

Returns the rectangle representing the new size the window would be if the resize was
applied.

It is up to the application to set the window size if required.

# `getDragStatus`

```erlang
-spec getDragStatus(This) -> wx:wx_enum() when This :: wxSashEvent().
```

Returns the status of the sash: one of wxSASH\_STATUS\_OK,
wxSASH\_STATUS\_OUT\_OF\_RANGE.

If the drag caused the notional bounding box of the window to flip over, for example, the
drag will be out of rage.

# `getEdge`

```erlang
-spec getEdge(This) -> wx:wx_enum() when This :: wxSashEvent().
```

Returns the dragged edge.

The return value is one of wxSASH_TOP, wxSASH_RIGHT, wxSASH_BOTTOM, wxSASH_LEFT.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
