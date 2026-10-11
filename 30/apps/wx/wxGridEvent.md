# `wxGridEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridEvent.erl#L58)

This event class contains information about various grid events.

Notice that all grid event table macros are available in two versions: `EVT_GRID_XXX` and `EVT_GRID_CMD_XXX`.
The only difference between the two is that the former doesn't allow to specify the grid
window identifier and so takes a single parameter, the event handler, but is not suitable
if there is more than one grid control in the window where the event table is used (as it
would catch the events from all the grids). The version with `CMD` takes the id as first
argument and the event handler as the second one and so can be used with multiple grids as
well. Otherwise there are no difference between the two and only the versions without the
id are documented below for brevity.

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxGridEvent](https://docs.wxwidgets.org/3.2/classwx_grid_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxGridEventType` to subscribe to events of this type.

# `wxGrid`

```erlang
-type wxGrid() ::
          #wxGrid{type :: wxGridEvent:wxGridEventType(),
                  row :: integer(),
                  col :: integer(),
                  pos :: {X :: integer(), Y :: integer()},
                  selecting :: boolean(),
                  control :: boolean(),
                  meta :: boolean(),
                  shift :: boolean(),
                  alt :: boolean()}.
```

# `wxGridEvent`

```erlang
-type wxGridEvent() :: wx:wx_object().
```

# `wxGridEventType`

```erlang
-type wxGridEventType() ::
          grid_cell_left_click | grid_cell_right_click | grid_cell_left_dclick |
          grid_cell_right_dclick | grid_label_left_click | grid_label_right_click |
          grid_label_left_dclick | grid_label_right_dclick | grid_cell_changed | grid_select_cell |
          grid_cell_begin_drag | grid_editor_shown | grid_editor_hidden | grid_col_move |
          grid_col_sort | grid_tabbing.
```

# `altDown`

```erlang
-spec altDown(This) -> boolean() when This :: wxGridEvent().
```

Returns true if the Alt key was down at the time of the event.

# `controlDown`

```erlang
-spec controlDown(This) -> boolean() when This :: wxGridEvent().
```

Returns true if the Control key was down at the time of the event.

# `getCol`

```erlang
-spec getCol(This) -> integer() when This :: wxGridEvent().
```

Column at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this column is the column of the newly
selected cell while the previously selected cell can be retrieved using `wxGrid:getGridCursorCol/1`.

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxGridEvent().
```

Position in pixels at which the event occurred.

# `getRow`

```erlang
-spec getRow(This) -> integer() when This :: wxGridEvent().
```

Row at which the event occurred.

Notice that for a `wxEVT_GRID_SELECT_CELL` event this row is the row of the newly
selected cell while the previously selected cell can be retrieved using `wxGrid:getGridCursorRow/1`.

# `metaDown`

```erlang
-spec metaDown(This) -> boolean() when This :: wxGridEvent().
```

Returns true if the Meta key was down at the time of the event.

# `selecting`

```erlang
-spec selecting(This) -> boolean() when This :: wxGridEvent().
```

Returns true if the user is selecting grid cells, or false if deselecting.

# `shiftDown`

```erlang
-spec shiftDown(This) -> boolean() when This :: wxGridEvent().
```

Returns true if the Shift key was down at the time of the event.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
