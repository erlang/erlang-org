# `wxSizeEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxSizeEvent.erl#L58)

A size event holds information about size change events of `m:wxWindow`.

The EVT_SIZE handler function will be called when the window has been resized.

You may wish to use this for frames to resize their child windows as appropriate.

Note that the size passed is of the whole window: call `wxWindow:getClientSize/1` for the area which may be used by
the application.

When a window is resized, usually only a small part of the window is damaged and you may
only need to repaint that area. However, if your drawing depends on the size of the
window, you may need to clear the DC explicitly and repaint the whole window. In which
case, you may need to call `wxWindow:refresh/2` to invalidate the entire window.

`Important` : Sizers (see overview_sizer ) rely on size events to function correctly.
Therefore, in a sizer-based layout, do not forget to call Skip on all size events you
catch (and don't catch size events at all when you don't need to).

See:
* {Width,Height}

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxSizeEvent](https://docs.wxwidgets.org/3.2/classwx_size_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxSizeEventType` to subscribe to events of this type.

# `wxSize`

```erlang
-type wxSize() ::
          #wxSize{type :: wxSizeEvent:wxSizeEventType(),
                  size :: {W :: integer(), H :: integer()},
                  rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}}.
```

# `wxSizeEvent`

```erlang
-type wxSizeEvent() :: wx:wx_object().
```

# `wxSizeEventType`

```erlang
-type wxSizeEventType() :: size.
```

# `getRect`

```erlang
-spec getRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                 when This :: wxSizeEvent().
```

# `getSize`

```erlang
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxSizeEvent().
```

Returns the entire size of the window generating the size change event.

This is the new total size of the window, i.e. the same size as would be returned by `wxWindow:getSize/1` if
it were called now. Use `wxWindow:getClientSize/1` if you catch this event in a top level window such as `m:wxFrame`
to find the size available for the window contents.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
