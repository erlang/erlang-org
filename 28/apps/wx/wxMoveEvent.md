# `wxMoveEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMoveEvent.erl#L58)

A move event holds information about window position change.

These events are currently generated for top level (see `m:wxTopLevelWindow`) windows in
all ports, but are not generated for the child windows in wxGTK.

See:
* {X,Y}

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMoveEvent](https://docs.wxwidgets.org/3.2/classwx_move_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMoveEventType` to subscribe to events of this type.

# `wxMove`

```elixir
-type wxMove() ::
          #wxMove{type :: wxMoveEvent:wxMoveEventType(),
                  pos :: {X :: integer(), Y :: integer()},
                  rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}}.
```

# `wxMoveEvent`

```elixir
-type wxMoveEvent() :: wx:wx_object().
```

# `wxMoveEventType`

```elixir
-type wxMoveEventType() :: move.
```

# `getPosition`

```elixir
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxMoveEvent().
```

Returns the position of the window generating the move change event.

# `getRect`

```elixir
-spec getRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                 when This :: wxMoveEvent().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
