# `wxGridCellEditor`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridCellEditor.erl#L58)

This class is responsible for providing and manipulating the in-place edit controls for
the grid.

Instances of `m:wxGridCellEditor` (actually, instances of derived classes since it is an
abstract class) can be associated with the cell attributes for individual cells, rows,
columns, or even for the entire grid.

Normally `m:wxGridCellEditor` shows some UI control allowing the user to edit the cell,
but starting with wxWidgets 3.1.4 it's also possible to define "activatable" cell editors,
that change the value of the cell directly when it's activated (typically by pressing
Space key or clicking on it), see `TryActivate()` (not implemented in wx) method. Note
that when implementing an editor which is always activatable, i.e. never shows any
in-place editor, it is more convenient to derive its class from `wxGridCellActivatableEditor`
(not implemented in wx) than from `m:wxGridCellEditor` itself.

See:
* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

wxWidgets docs: [wxGridCellEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_editor.html)

# `wxGridCellEditor`

```erlang
-type wxGridCellEditor() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, EvtHandler) -> ok
                when
                    This :: wxGridCellEditor(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    EvtHandler :: wxEvtHandler:wxEvtHandler().
```

Creates the actual edit control.

# `handleReturn`

```erlang
-spec handleReturn(This, Event) -> ok when This :: wxGridCellEditor(), Event :: wxKeyEvent:wxKeyEvent().
```

Some types of controls on some platforms may need some help with the Return key.

# `isCreated`

```erlang
-spec isCreated(This) -> boolean() when This :: wxGridCellEditor().
```

Returns true if the edit control has been created.

# `reset`

```erlang
-spec reset(This) -> ok when This :: wxGridCellEditor().
```

Reset the value in the control back to its starting value.

# `setSize`

```erlang
-spec setSize(This, Rect) -> ok
                 when
                     This :: wxGridCellEditor(),
                     Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

Size and position the edit control.

# `show`

```erlang
-spec show(This, Show) -> ok when This :: wxGridCellEditor(), Show :: boolean().
```

# `show`

```erlang
-spec show(This, Show, [Option]) -> ok
              when
                  This :: wxGridCellEditor(),
                  Show :: boolean(),
                  Option :: {attr, wxGridCellAttr:wxGridCellAttr()}.
```

Show or hide the edit control, use the specified attributes to set colours/fonts for it.

# `startingClick`

```erlang
-spec startingClick(This) -> ok when This :: wxGridCellEditor().
```

If the editor is enabled by clicking on the cell, this method will be called.

# `startingKey`

```erlang
-spec startingKey(This, Event) -> ok when This :: wxGridCellEditor(), Event :: wxKeyEvent:wxKeyEvent().
```

If the editor is enabled by pressing keys on the grid, this will be called to let the
editor do something about that first key if desired.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
