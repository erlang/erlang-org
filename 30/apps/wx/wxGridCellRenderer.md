# `wxGridCellRenderer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridCellRenderer.erl#L58)

This class is responsible for actually drawing the cell in the grid.

You may pass it to the `m:wxGridCellAttr` (below) to change the format of one given cell
or to `wxGrid:setDefaultRenderer/2` to change the view of all cells. This is an abstract class, and you will normally
use one of the predefined derived classes or derive your own class from it.

See:
* `m:wxGridCellBoolRenderer`

* `m:wxGridCellFloatRenderer`

* `m:wxGridCellNumberRenderer`

* `m:wxGridCellStringRenderer`

wxWidgets docs: [wxGridCellRenderer](https://docs.wxwidgets.org/3.2/classwx_grid_cell_renderer.html)

# `wxGridCellRenderer`

```erlang
-type wxGridCellRenderer() :: wx:wx_object().
```

# `draw`

```erlang
-spec draw(This, Grid, Attr, Dc, Rect, Row, Col, IsSelected) -> ok
              when
                  This :: wxGridCellRenderer(),
                  Grid :: wxGrid:wxGrid(),
                  Attr :: wxGridCellAttr:wxGridCellAttr(),
                  Dc :: wxDC:wxDC(),
                  Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                  Row :: integer(),
                  Col :: integer(),
                  IsSelected :: boolean().
```

Draw the given cell on the provided DC inside the given rectangle using the style
specified by the attribute and the default or selected state corresponding to the
isSelected value.

This pure virtual function has a default implementation which will prepare the DC using
the given attribute: it will draw the rectangle with the background colour from attr and
set the text colour and font.

# `getBestSize`

```erlang
-spec getBestSize(This, Grid, Attr, Dc, Row, Col) -> {W :: integer(), H :: integer()}
                     when
                         This :: wxGridCellRenderer(),
                         Grid :: wxGrid:wxGrid(),
                         Attr :: wxGridCellAttr:wxGridCellAttr(),
                         Dc :: wxDC:wxDC(),
                         Row :: integer(),
                         Col :: integer().
```

Get the preferred size of the cell for its contents.

This method must be overridden in the derived classes to return the minimal fitting size
for displaying the content of the given grid cell.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
