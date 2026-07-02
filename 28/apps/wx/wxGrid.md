# `wxGrid`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGrid.erl#L58)

`m:wxGrid` and its related classes are used for displaying and editing tabular data.

They provide a rich set of features for display, editing, and interacting with a variety
of data sources. For simple applications, and to help you get started, `m:wxGrid` is the
only class you need to refer to directly. It will set up default instances of the other
classes and manage them for you. For more complex applications you can derive your own
classes for custom grid views, grid data tables, cell editors and renderers. The
overview_grid has examples of simple and more complex applications, explains the
relationship between the various grid classes and has a summary of the keyboard shortcuts
and mouse functions provided by `m:wxGrid`.

A `wxGridTableBase` (not implemented in wx) class holds the actual data to be displayed
by a `m:wxGrid` class. One or more `m:wxGrid` classes may act as a view for one table
class. The default table class is called `wxGridStringTable` (not implemented in wx) and
holds an array of strings. An instance of such a class is created by `createGrid/4`.

`m:wxGridCellRenderer` is the abstract base class for rendering contents in a cell. The
following renderers are predefined:

* `m:wxGridCellBoolRenderer`

* `m:wxGridCellFloatRenderer`

* `m:wxGridCellNumberRenderer`

* `m:wxGridCellStringRenderer`

* `wxGridCellDateRenderer` (not implemented in wx)

* `wxGridCellDateTimeRenderer` (not implemented in wx)

The look of a cell can be further defined using `m:wxGridCellAttr`. An object of this
type may be returned by `wxGridTableBase::GetAttr()` (not implemented in wx).

`m:wxGridCellEditor` is the abstract base class for editing the value of a cell. The
following editors are predefined:

* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

* `wxGridCellDateEditor` (not implemented in wx)

Please see `m:wxGridEvent`, `wxGridSizeEvent` (not implemented in wx), `wxGridRangeSelectEvent`
(not implemented in wx), and `wxGridEditorCreatedEvent` (not implemented in wx) for the
documentation of all event types you can use with `m:wxGrid`.

See: [Overview grid](https://docs.wxwidgets.org/3.2/overview_grid.html#overview_grid)

This class is derived, and can use functions, from:

* `m:wxScrolledWindow`

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxGrid](https://docs.wxwidgets.org/3.2/classwx_grid.html)

# `wxGrid`

```elixir
-type wxGrid() :: wx:wx_object().
```

# `appendCols`

```elixir
-spec appendCols(This) -> boolean() when This :: wxGrid().
```

# `appendCols`

```elixir
-spec appendCols(This, [Option]) -> boolean()
                    when This :: wxGrid(), Option :: {numCols, integer()} | {updateLabels, boolean()}.
```

Appends one or more new columns to the right of the grid.

The `updateLabels` argument is not used at present. If you are using a derived grid table
class you will need to override `wxGridTableBase::AppendCols()` (not implemented in wx).
See `insertCols/2` for further information.

Return: true on success or false if appending columns failed.

# `appendRows`

```elixir
-spec appendRows(This) -> boolean() when This :: wxGrid().
```

# `appendRows`

```elixir
-spec appendRows(This, [Option]) -> boolean()
                    when This :: wxGrid(), Option :: {numRows, integer()} | {updateLabels, boolean()}.
```

Appends one or more new rows to the bottom of the grid.

The `updateLabels` argument is not used at present. If you are using a derived grid table
class you will need to override `wxGridTableBase::AppendRows()` (not implemented in wx).
See `insertRows/2` for further information.

Return: true on success or false if appending rows failed.

# `autoSize`

```elixir
-spec autoSize(This) -> ok when This :: wxGrid().
```

Automatically sets the height and width of all rows and columns to fit their contents.

# `autoSizeColumn`

```elixir
-spec autoSizeColumn(This, Col) -> ok when This :: wxGrid(), Col :: integer().
```

# `autoSizeColumn`

```elixir
-spec autoSizeColumn(This, Col, [Option]) -> ok
                        when This :: wxGrid(), Col :: integer(), Option :: {setAsMin, boolean()}.
```

Automatically sizes the column to fit its contents.

If `setAsMin` is true the calculated width will also be set as the minimal width for the
column.

# `autoSizeColumns`

```elixir
-spec autoSizeColumns(This) -> ok when This :: wxGrid().
```

# `autoSizeColumns`

```elixir
-spec autoSizeColumns(This, [Option]) -> ok when This :: wxGrid(), Option :: {setAsMin, boolean()}.
```

Automatically sizes all columns to fit their contents.

If `setAsMin` is true the calculated widths will also be set as the minimal widths for
the columns.

# `autoSizeRow`

```elixir
-spec autoSizeRow(This, Row) -> ok when This :: wxGrid(), Row :: integer().
```

# `autoSizeRow`

```elixir
-spec autoSizeRow(This, Row, [Option]) -> ok
                     when This :: wxGrid(), Row :: integer(), Option :: {setAsMin, boolean()}.
```

Automatically sizes the row to fit its contents.

If `setAsMin` is true the calculated height will also be set as the minimal height for
the row.

# `autoSizeRows`

```elixir
-spec autoSizeRows(This) -> ok when This :: wxGrid().
```

# `autoSizeRows`

```elixir
-spec autoSizeRows(This, [Option]) -> ok when This :: wxGrid(), Option :: {setAsMin, boolean()}.
```

Automatically sizes all rows to fit their contents.

If `setAsMin` is true the calculated heights will also be set as the minimal heights for
the rows.

# `beginBatch`

```elixir
-spec beginBatch(This) -> ok when This :: wxGrid().
```

Increments the grid's batch count.

When the count is greater than zero repainting of the grid is suppressed. Each call to
BeginBatch must be matched by a later call to `endBatch/1`. Code that does a lot of grid modification
can be enclosed between `beginBatch/1` and `endBatch/1` calls to avoid screen flicker. The final `endBatch/1` call will cause
the grid to be repainted.

Notice that you should use `wxGridUpdateLocker` (not implemented in wx) which ensures
that there is always a matching `endBatch/1` call for this `beginBatch/1` if possible instead of calling this method
directly.

# `blockToDeviceRect`

```elixir
-spec blockToDeviceRect(This, TopLeft, BottomRight) ->
                           {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                           when
                               This :: wxGrid(),
                               TopLeft :: {R :: integer(), C :: integer()},
                               BottomRight :: {R :: integer(), C :: integer()}.
```

Convert grid cell coordinates to grid window pixel coordinates.

This function returns the rectangle that encloses the block of cells limited by `topLeft`
and `bottomRight` cell in device coords and clipped to the client size of the grid window.

Since: 3.1.3 Parameter `gridWindow` has been added.

See: `cellToRect/3`

# `canDragCell`

```elixir
-spec canDragCell(This) -> boolean() when This :: wxGrid().
```

Return true if the dragging of cells is enabled or false otherwise.

# `canDragColMove`

```elixir
-spec canDragColMove(This) -> boolean() when This :: wxGrid().
```

Returns true if columns can be moved by dragging with the mouse.

Columns can be moved by dragging on their labels.

# `canDragColSize`

```elixir
-spec canDragColSize(This, Col) -> boolean() when This :: wxGrid(), Col :: integer().
```

Returns true if the given column can be resized by dragging with the mouse.

This function returns true if resizing the columns interactively is globally enabled,
i.e. if `disableDragColSize/1` hadn't been called, and if this column wasn't explicitly marked as non-resizable
with `DisableColResize()` (not implemented in wx).

# `canDragGridRowEdges`

```elixir
-spec canDragGridRowEdges(This) -> boolean() when This :: wxGrid().
```

Return true if row edges inside the grid can be dragged to resize the rows.

See:
* `canDragGridSize/1`

* `canDragRowSize/2`

Since: 3.1.4

# `canDragGridSize`

```elixir
-spec canDragGridSize(This) -> boolean() when This :: wxGrid().
```

Return true if the dragging of grid lines to resize rows and columns is enabled or false
otherwise.

# `canDragRowSize`

```elixir
-spec canDragRowSize(This, Row) -> boolean() when This :: wxGrid(), Row :: integer().
```

Returns true if the given row can be resized by dragging with the mouse.

This is the same as `canDragColSize/2` but for rows.

# `canEnableCellControl`

```elixir
-spec canEnableCellControl(This) -> boolean() when This :: wxGrid().
```

Returns true if the in-place edit control for the current grid cell can be used and false
otherwise.

This function always returns false for the read-only cells.

# `cellToRect`

```elixir
-spec cellToRect(This, Coords) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                    when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

Return the rectangle corresponding to the grid cell's size and position in logical
coordinates.

See: `blockToDeviceRect/3`

# `cellToRect`

```elixir
-spec cellToRect(This, Row, Col) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                    when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Return the rectangle corresponding to the grid cell's size and position in logical
coordinates.

See: `blockToDeviceRect/3`

# `clearGrid`

```elixir
-spec clearGrid(This) -> ok when This :: wxGrid().
```

Clears all data in the underlying grid table and repaints the grid.

The table is not deleted by this function. If you are using a derived table class then
you need to override `wxGridTableBase::Clear()` (not implemented in wx) for this function
to have any effect.

# `clearSelection`

```elixir
-spec clearSelection(This) -> ok when This :: wxGrid().
```

Deselects all cells that are currently selected.

# `createGrid`

```elixir
-spec createGrid(This, NumRows, NumCols) -> boolean()
                    when This :: wxGrid(), NumRows :: integer(), NumCols :: integer().
```

# `createGrid`

```elixir
-spec createGrid(This, NumRows, NumCols, [Option]) -> boolean()
                    when
                        This :: wxGrid(),
                        NumRows :: integer(),
                        NumCols :: integer(),
                        Option :: {selmode, wx:wx_enum()}.
```

Creates a grid with the specified initial number of rows and columns.

Call this directly after the grid constructor. When you use this function `m:wxGrid` will
create and manage a simple table of string values for you. All of the grid data will be
stored in memory.

For applications with more complex data types or relationships, or for dealing with very
large datasets, you should derive your own grid table class and pass a table object to the
grid with `SetTable()` (not implemented in wx) or `AssignTable()` (not implemented in wx).

# `deleteCols`

```elixir
-spec deleteCols(This) -> boolean() when This :: wxGrid().
```

# `deleteCols`

```elixir
-spec deleteCols(This, [Option]) -> boolean()
                    when
                        This :: wxGrid(),
                        Option :: {pos, integer()} | {numCols, integer()} | {updateLabels, boolean()}.
```

Deletes one or more columns from a grid starting at the specified position.

The `updateLabels` argument is not used at present. If you are using a derived grid table
class you will need to override `wxGridTableBase::DeleteCols()` (not implemented in wx).
See `insertCols/2` for further information.

Return: true on success or false if deleting columns failed.

# `deleteRows`

```elixir
-spec deleteRows(This) -> boolean() when This :: wxGrid().
```

# `deleteRows`

```elixir
-spec deleteRows(This, [Option]) -> boolean()
                    when
                        This :: wxGrid(),
                        Option :: {pos, integer()} | {numRows, integer()} | {updateLabels, boolean()}.
```

Deletes one or more rows from a grid starting at the specified position.

The `updateLabels` argument is not used at present. If you are using a derived grid table
class you will need to override `wxGridTableBase::DeleteRows()` (not implemented in wx).
See `insertRows/2` for further information.

Return: true on success or false if deleting rows failed.

# `destroy`

```elixir
-spec destroy(This :: wxGrid()) -> ok.
```

Destroys the object

# `disableCellEditControl`

```elixir
-spec disableCellEditControl(This) -> ok when This :: wxGrid().
```

Disables in-place editing of grid cells.

Equivalent to calling EnableCellEditControl(false).

# `disableDragColSize`

```elixir
-spec disableDragColSize(This) -> ok when This :: wxGrid().
```

Disables column sizing by dragging with the mouse.

Equivalent to passing false to `enableDragColSize/2`.

# `disableDragGridSize`

```elixir
-spec disableDragGridSize(This) -> ok when This :: wxGrid().
```

Disable mouse dragging of grid lines to resize rows and columns.

Equivalent to passing false to `enableDragGridSize/2`

# `disableDragRowSize`

```elixir
-spec disableDragRowSize(This) -> ok when This :: wxGrid().
```

Disables row sizing by dragging with the mouse.

Equivalent to passing false to `enableDragRowSize/2`.

# `enableCellEditControl`

```elixir
-spec enableCellEditControl(This) -> ok when This :: wxGrid().
```

# `enableCellEditControl`

```elixir
-spec enableCellEditControl(This, [Option]) -> ok when This :: wxGrid(), Option :: {enable, boolean()}.
```

Enables or disables in-place editing of grid cell data.

Enabling in-place editing generates `wxEVT_GRID_EDITOR_SHOWN` and, if it isn't vetoed by
the application, shows the in-place editor which allows the user to change the cell value.

Disabling in-place editing does nothing if the in-place editor isn't currently shown,
otherwise the `wxEVT_GRID_EDITOR_HIDDEN` event is generated but, unlike the "shown" event,
it can't be vetoed and the in-place editor is dismissed unconditionally.

Note that it is an error to call this function if the current cell is read-only, use `canEnableCellControl/1` to
check for this precondition.

# `enableDragColSize`

```elixir
-spec enableDragColSize(This) -> ok when This :: wxGrid().
```

# `enableDragColSize`

```elixir
-spec enableDragColSize(This, [Option]) -> ok when This :: wxGrid(), Option :: {enable, boolean()}.
```

Enables or disables column sizing by dragging with the mouse.

# `enableDragGridSize`

```elixir
-spec enableDragGridSize(This) -> ok when This :: wxGrid().
```

# `enableDragGridSize`

```elixir
-spec enableDragGridSize(This, [Option]) -> ok when This :: wxGrid(), Option :: {enable, boolean()}.
```

Enables or disables row and column resizing by dragging gridlines with the mouse.

# `enableDragRowSize`

```elixir
-spec enableDragRowSize(This) -> ok when This :: wxGrid().
```

# `enableDragRowSize`

```elixir
-spec enableDragRowSize(This, [Option]) -> ok when This :: wxGrid(), Option :: {enable, boolean()}.
```

Enables or disables row sizing by dragging with the mouse.

# `enableEditing`

```elixir
-spec enableEditing(This, Edit) -> ok when This :: wxGrid(), Edit :: boolean().
```

Makes the grid globally editable or read-only.

If the edit argument is false this function sets the whole grid as read-only. If the
argument is true the grid is set to the default state where cells may be editable. In the
default state you can set single grid cells and whole rows and columns to be editable or
read-only via `wxGridCellAttr:setReadOnly/2`. For single cells you can also use the shortcut function `setReadOnly/4`.

For more information about controlling grid cell attributes see the `m:wxGridCellAttr`
class and the overview_grid.

# `enableGridLines`

```elixir
-spec enableGridLines(This) -> ok when This :: wxGrid().
```

# `enableGridLines`

```elixir
-spec enableGridLines(This, [Option]) -> ok when This :: wxGrid(), Option :: {enable, boolean()}.
```

Turns the drawing of grid lines on or off.

# `endBatch`

```elixir
-spec endBatch(This) -> ok when This :: wxGrid().
```

Decrements the grid's batch count.

When the count is greater than zero repainting of the grid is suppressed. Each previous
call to `beginBatch/1` must be matched by a later call to `endBatch/1`. Code that does a lot of grid modification
can be enclosed between `beginBatch/1` and `endBatch/1` calls to avoid screen flicker. The final `endBatch/1` will cause the
grid to be repainted.

# `fit`

```elixir
-spec fit(This) -> ok when This :: wxGrid().
```

Overridden `m:wxWindow` method.

# `forceRefresh`

```elixir
-spec forceRefresh(This) -> ok when This :: wxGrid().
```

Causes immediate repainting of the grid.

Use this instead of the usual `wxWindow:refresh/2`.

# `getBatchCount`

```elixir
-spec getBatchCount(This) -> integer() when This :: wxGrid().
```

Returns the number of times that `beginBatch/1` has been called without (yet) matching
calls to `endBatch/1`.

While the grid's batch count is greater than zero the display will not be updated.

# `getCellAlignment`

```elixir
-spec getCellAlignment(This, Row, Col) -> {Horiz :: integer(), Vert :: integer()}
                          when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Sets the arguments to the horizontal and vertical text alignment values for the grid cell
at the specified location.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `getCellBackgroundColour`

```elixir
-spec getCellBackgroundColour(This, Row, Col) -> wx:wx_colour4()
                                 when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the background colour of the cell at the specified location.

# `getCellEditor`

```elixir
-spec getCellEditor(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor()
                       when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns a pointer to the editor for the cell at the specified location.

See `m:wxGridCellEditor` and the overview_grid for more information about cell editors
and renderers.

The caller must call DecRef() on the returned pointer.

# `getCellFont`

```elixir
-spec getCellFont(This, Row, Col) -> wxFont:wxFont()
                     when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the font for text in the grid cell at the specified location.

# `getCellRenderer`

```elixir
-spec getCellRenderer(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer()
                         when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns a pointer to the renderer for the grid cell at the specified location.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell editors
and renderers.

The caller must call DecRef() on the returned pointer.

# `getCellTextColour`

```elixir
-spec getCellTextColour(This, Row, Col) -> wx:wx_colour4()
                           when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the text colour for the grid cell at the specified location.

# `getCellValue`

```elixir
-spec getCellValue(This, Coords) -> unicode:charlist()
                      when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

Returns the string contained in the cell at the specified location.

For simple applications where a grid object automatically uses a default grid table of
string values you use this function together with `setCellValue/4` to access cell values. For more complex
applications where you have derived your own grid table class that contains various data
types (e.g. numeric, boolean or user-defined custom types) then you only use this function
for those cells that contain string values.

See `wxGridTableBase::CanGetValueAs()` (not implemented in wx) and the overview_grid for
more information.

# `getCellValue`

```elixir
-spec getCellValue(This, Row, Col) -> unicode:charlist()
                      when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the string contained in the cell at the specified location.

For simple applications where a grid object automatically uses a default grid table of
string values you use this function together with `setCellValue/4` to access cell values. For more complex
applications where you have derived your own grid table class that contains various data
types (e.g. numeric, boolean or user-defined custom types) then you only use this function
for those cells that contain string values.

See `wxGridTableBase::CanGetValueAs()` (not implemented in wx) and the overview_grid for
more information.

# `getColLabelAlignment`

```elixir
-spec getColLabelAlignment(This) -> {Horiz :: integer(), Vert :: integer()} when This :: wxGrid().
```

Sets the arguments to the current column label alignment values.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `getColLabelSize`

```elixir
-spec getColLabelSize(This) -> integer() when This :: wxGrid().
```

Returns the current height of the column labels.

# `getColLabelValue`

```elixir
-spec getColLabelValue(This, Col) -> unicode:charlist() when This :: wxGrid(), Col :: integer().
```

Returns the specified column label.

The default grid table class provides column labels of the form A,B...Z,AA,AB...ZZ,AAA...
If you are using a custom grid table you can override `wxGridTableBase::GetColLabelValue()`
(not implemented in wx) to provide your own labels.

# `getColMinimalAcceptableWidth`

```elixir
-spec getColMinimalAcceptableWidth(This) -> integer() when This :: wxGrid().
```

Returns the minimal width to which a column may be resized.

Use `setColMinimalAcceptableWidth/2` to change this value globally or `setColMinimalWidth/3` to do it for individual columns.

See: `getRowMinimalAcceptableHeight/1`

# `getDefaultCellAlignment`

```elixir
-spec getDefaultCellAlignment(This) -> {Horiz :: integer(), Vert :: integer()} when This :: wxGrid().
```

Returns the default cell alignment.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

See: `setDefaultCellAlignment/3`

# `getDefaultCellBackgroundColour`

```elixir
-spec getDefaultCellBackgroundColour(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the current default background colour for grid cells.

# `getDefaultCellFont`

```elixir
-spec getDefaultCellFont(This) -> wxFont:wxFont() when This :: wxGrid().
```

Returns the current default font for grid cell text.

# `getDefaultCellTextColour`

```elixir
-spec getDefaultCellTextColour(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the current default colour for grid cell text.

# `getDefaultColLabelSize`

```elixir
-spec getDefaultColLabelSize(This) -> integer() when This :: wxGrid().
```

Returns the default height for column labels.

# `getDefaultColSize`

```elixir
-spec getDefaultColSize(This) -> integer() when This :: wxGrid().
```

Returns the current default width for grid columns.

# `getDefaultEditor`

```elixir
-spec getDefaultEditor(This) -> wxGridCellEditor:wxGridCellEditor() when This :: wxGrid().
```

Returns a pointer to the current default grid cell editor.

See `m:wxGridCellEditor` and the overview_grid for more information about cell editors
and renderers.

# `getDefaultEditorForCell`

```elixir
-spec getDefaultEditorForCell(This, C) -> wxGridCellEditor:wxGridCellEditor()
                                 when This :: wxGrid(), C :: {R :: integer(), C :: integer()}.
```

Returns the default editor for the specified cell.

The base class version returns the editor appropriate for the current cell type but this
method may be overridden in the derived classes to use custom editors for some cells by default.

Notice that the same may be achieved in a usually simpler way by associating a custom
editor with the given cell or cells.

The caller must call DecRef() on the returned pointer.

# `getDefaultEditorForCell`

```elixir
-spec getDefaultEditorForCell(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor()
                                 when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the default editor for the specified cell.

The base class version returns the editor appropriate for the current cell type but this
method may be overridden in the derived classes to use custom editors for some cells by default.

Notice that the same may be achieved in a usually simpler way by associating a custom
editor with the given cell or cells.

The caller must call DecRef() on the returned pointer.

# `getDefaultEditorForType`

```elixir
-spec getDefaultEditorForType(This, TypeName) -> wxGridCellEditor:wxGridCellEditor()
                                 when This :: wxGrid(), TypeName :: unicode:chardata().
```

Returns the default editor for the cells containing values of the given type.

The base class version returns the editor which was associated with the specified `typeName`
when it was registered `registerDataType/4` but this function may be overridden to return something
different. This allows overriding an editor used for one of the standard types.

The caller must call DecRef() on the returned pointer.

# `getDefaultRenderer`

```elixir
-spec getDefaultRenderer(This) -> wxGridCellRenderer:wxGridCellRenderer() when This :: wxGrid().
```

Returns a pointer to the current default grid cell renderer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell editors
and renderers.

The caller must call DecRef() on the returned pointer.

# `getDefaultRendererForCell`

```elixir
-spec getDefaultRendererForCell(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer()
                                   when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the default renderer for the given cell.

The base class version returns the renderer appropriate for the current cell type but
this method may be overridden in the derived classes to use custom renderers for some
cells by default.

The caller must call DecRef() on the returned pointer.

# `getDefaultRendererForType`

```elixir
-spec getDefaultRendererForType(This, TypeName) -> wxGridCellRenderer:wxGridCellRenderer()
                                   when This :: wxGrid(), TypeName :: unicode:chardata().
```

Returns the default renderer for the cell containing values of the given type.

See: `getDefaultEditorForType/2`

# `getDefaultRowLabelSize`

```elixir
-spec getDefaultRowLabelSize(This) -> integer() when This :: wxGrid().
```

Returns the default width for the row labels.

# `getDefaultRowSize`

```elixir
-spec getDefaultRowSize(This) -> integer() when This :: wxGrid().
```

Returns the current default height for grid rows.

# `getGridColLabelWindow`

```elixir
-spec getGridColLabelWindow(This) -> wxWindow:wxWindow() when This :: wxGrid().
```

Return the column labels window.

This window is not shown if the columns labels were hidden using `HideColLabels()` (not
implemented in wx).

Depending on whether `UseNativeColHeader()` (not implemented in wx) was called or not
this can be either a `wxHeaderCtrl` (not implemented in wx) or a plain `m:wxWindow`. This
function returns a valid window pointer in either case but in the former case you can also
use `GetGridColHeader()` (not implemented in wx) to access it if you need
wxHeaderCtrl-specific functionality.

# `getGridCornerLabelWindow`

```elixir
-spec getGridCornerLabelWindow(This) -> wxWindow:wxWindow() when This :: wxGrid().
```

Return the window in the top left grid corner.

This window is shown only of both columns and row labels are shown and normally doesn't
contain anything. Clicking on it is handled by `m:wxGrid` however and can be used to
select the entire grid.

# `getGridCursorCol`

```elixir
-spec getGridCursorCol(This) -> integer() when This :: wxGrid().
```

Returns the current grid cell column position.

# `getGridCursorRow`

```elixir
-spec getGridCursorRow(This) -> integer() when This :: wxGrid().
```

Returns the current grid cell row position.

# `getGridLineColour`

```elixir
-spec getGridLineColour(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the colour used for grid lines.

# `getGridRowLabelWindow`

```elixir
-spec getGridRowLabelWindow(This) -> wxWindow:wxWindow() when This :: wxGrid().
```

Return the row labels window.

This window is not shown if the row labels were hidden using `HideRowLabels()` (not
implemented in wx).

# `getGridWindow`

```elixir
-spec getGridWindow(This) -> wxWindow:wxWindow() when This :: wxGrid().
```

Return the main grid window containing the grid cells.

This window is always shown.

# `getLabelBackgroundColour`

```elixir
-spec getLabelBackgroundColour(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the colour used for the background of row and column labels.

# `getLabelFont`

```elixir
-spec getLabelFont(This) -> wxFont:wxFont() when This :: wxGrid().
```

Returns the font used for row and column labels.

# `getLabelTextColour`

```elixir
-spec getLabelTextColour(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the colour used for row and column label text.

# `getNumberCols`

```elixir
-spec getNumberCols(This) -> integer() when This :: wxGrid().
```

Returns the total number of grid columns.

This is the same as the number of columns in the underlying grid table.

# `getNumberRows`

```elixir
-spec getNumberRows(This) -> integer() when This :: wxGrid().
```

Returns the total number of grid rows.

This is the same as the number of rows in the underlying grid table.

# `getOrCreateCellAttr`

```elixir
-spec getOrCreateCellAttr(This, Row, Col) -> wxGridCellAttr:wxGridCellAttr()
                             when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns the attribute for the given cell creating one if necessary.

If the cell already has an attribute, it is returned. Otherwise a new attribute is
created, associated with the cell and returned. In any case the caller must call DecRef()
on the returned pointer.

Prefer to use `GetOrCreateCellAttrPtr()` (not implemented in wx) to avoid the need to
call DecRef() on the returned pointer.

This function may only be called if `CanHaveAttributes()` (not implemented in wx) returns
true.

# `getRowLabelAlignment`

```elixir
-spec getRowLabelAlignment(This) -> {Horiz :: integer(), Vert :: integer()} when This :: wxGrid().
```

Returns the alignment used for row labels.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `getRowLabelSize`

```elixir
-spec getRowLabelSize(This) -> integer() when This :: wxGrid().
```

Returns the current width of the row labels.

# `getRowLabelValue`

```elixir
-spec getRowLabelValue(This, Row) -> unicode:charlist() when This :: wxGrid(), Row :: integer().
```

Returns the specified row label.

The default grid table class provides numeric row labels. If you are using a custom grid
table you can override `wxGridTableBase::GetRowLabelValue()` (not implemented in wx) to
provide your own labels.

# `getRowMinimalAcceptableHeight`

```elixir
-spec getRowMinimalAcceptableHeight(This) -> integer() when This :: wxGrid().
```

Returns the minimal size to which rows can be resized.

Use `setRowMinimalAcceptableHeight/2` to change this value globally or `setRowMinimalHeight/3` to do it for individual cells.

See: `getColMinimalAcceptableWidth/1`

# `getRowSize`

```elixir
-spec getRowSize(This, Row) -> integer() when This :: wxGrid(), Row :: integer().
```

Returns the height of the specified row.

# `getScrollLineX`

```elixir
-spec getScrollLineX(This) -> integer() when This :: wxGrid().
```

Returns the number of pixels per horizontal scroll increment.

The default is 15.

See:
* `getScrollLineY/1`

* `setScrollLineX/2`

* `setScrollLineY/2`

# `getScrollLineY`

```elixir
-spec getScrollLineY(This) -> integer() when This :: wxGrid().
```

Returns the number of pixels per vertical scroll increment.

The default is 15.

See:
* `getScrollLineX/1`

* `setScrollLineX/2`

* `setScrollLineY/2`

# `getSelectedCells`

```elixir
-spec getSelectedCells(This) -> [{R :: integer(), C :: integer()}] when This :: wxGrid().
```

Returns an array of individually selected cells.

Notice that this array does `not` contain all the selected cells in general as it doesn't
include the cells selected as part of column, row or block selection. You must use this
method, `getSelectedCols/1`, `getSelectedRows/1` and `getSelectionBlockTopLeft/1` and `getSelectionBlockBottomRight/1` methods to obtain the entire selection in general.

Please notice this behaviour is by design and is needed in order to support grids of
arbitrary size (when an entire column is selected in a grid with a million of columns, we
don't want to create an array with a million of entries in this function, instead it
returns an empty array and `getSelectedCols/1` returns an array containing one element).

The function can be slow for the big grids, use `GetSelectedBlocks()` (not implemented in
wx) in the new code.

# `getSelectedCols`

```elixir
-spec getSelectedCols(This) -> [integer()] when This :: wxGrid().
```

Returns an array of selected columns.

Please notice that this method alone is not sufficient to find all the selected columns
as it contains only the columns which were individually selected but not those being part
of the block selection or being selected in virtue of all of their cells being selected
individually, please see `getSelectedCells/1` for more details.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not implemented in
wx) in the new code.

# `getSelectedRows`

```elixir
-spec getSelectedRows(This) -> [integer()] when This :: wxGrid().
```

Returns an array of selected rows.

Please notice that this method alone is not sufficient to find all the selected rows as
it contains only the rows which were individually selected but not those being part of the
block selection or being selected in virtue of all of their cells being selected
individually, please see `getSelectedCells/1` for more details.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not implemented in
wx) in the new code.

# `getSelectionBackground`

```elixir
-spec getSelectionBackground(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the colour used for drawing the selection background.

# `getSelectionBlockBottomRight`

```elixir
-spec getSelectionBlockBottomRight(This) -> [{R :: integer(), C :: integer()}] when This :: wxGrid().
```

Returns an array of the bottom right corners of blocks of selected cells.

Please see `getSelectedCells/1` for more information about the selection representation in `m:wxGrid`.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not implemented in
wx) in the new code.

See: `getSelectionBlockTopLeft/1`

# `getSelectionBlockTopLeft`

```elixir
-spec getSelectionBlockTopLeft(This) -> [{R :: integer(), C :: integer()}] when This :: wxGrid().
```

Returns an array of the top left corners of blocks of selected cells.

Please see `getSelectedCells/1` for more information about the selection representation in `m:wxGrid`.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not implemented in
wx) in the new code.

See: `getSelectionBlockBottomRight/1`

# `getSelectionForeground`

```elixir
-spec getSelectionForeground(This) -> wx:wx_colour4() when This :: wxGrid().
```

Returns the colour used for drawing the selection foreground.

# `gridLinesEnabled`

```elixir
-spec gridLinesEnabled(This) -> boolean() when This :: wxGrid().
```

Returns true if drawing of grid lines is turned on, false otherwise.

# `hideCellEditControl`

```elixir
-spec hideCellEditControl(This) -> ok when This :: wxGrid().
```

Hides the in-place cell edit control.

# `insertCols`

```elixir
-spec insertCols(This) -> boolean() when This :: wxGrid().
```

# `insertCols`

```elixir
-spec insertCols(This, [Option]) -> boolean()
                    when
                        This :: wxGrid(),
                        Option :: {pos, integer()} | {numCols, integer()} | {updateLabels, boolean()}.
```

Inserts one or more new columns into a grid with the first new column at the specified
position.

Notice that inserting the columns in the grid requires grid table cooperation: when this
method is called, grid object begins by requesting the underlying grid table to insert new
columns. If this is successful the table notifies the grid and the grid updates the
display. For a default grid (one where you have called `createGrid/4`) this process is automatic. If you
are using a custom grid table (specified with `SetTable()` (not implemented in wx) or `AssignTable()`
(not implemented in wx)) then you must override `wxGridTableBase::InsertCols()` (not
implemented in wx) in your derived table class.

Return: true if the columns were successfully inserted, false if an error occurred (most
likely the table couldn't be updated).

# `insertRows`

```elixir
-spec insertRows(This) -> boolean() when This :: wxGrid().
```

# `insertRows`

```elixir
-spec insertRows(This, [Option]) -> boolean()
                    when
                        This :: wxGrid(),
                        Option :: {pos, integer()} | {numRows, integer()} | {updateLabels, boolean()}.
```

Inserts one or more new rows into a grid with the first new row at the specified
position.

Notice that you must implement `wxGridTableBase::InsertRows()` (not implemented in wx) if
you use a grid with a custom table, please see `insertCols/2` for more information.

Return: true if the rows were successfully inserted, false if an error occurred (most
likely the table couldn't be updated).

# `isCellEditControlEnabled`

```elixir
-spec isCellEditControlEnabled(This) -> boolean() when This :: wxGrid().
```

Returns true if the in-place edit control is currently enabled.

# `isCurrentCellReadOnly`

```elixir
-spec isCurrentCellReadOnly(This) -> boolean() when This :: wxGrid().
```

Returns true if the current cell is read-only.

See:
* `setReadOnly/4`

* `isReadOnly/3`

# `isEditable`

```elixir
-spec isEditable(This) -> boolean() when This :: wxGrid().
```

Returns false if the whole grid has been set as read-only or true otherwise.

See `enableEditing/2` for more information about controlling the editing status of grid cells.

# `isInSelection`

```elixir
-spec isInSelection(This, Coords) -> boolean()
                       when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

Returns true if the given cell is selected.

# `isInSelection`

```elixir
-spec isInSelection(This, Row, Col) -> boolean()
                       when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns true if the given cell is selected.

# `isReadOnly`

```elixir
-spec isReadOnly(This, Row, Col) -> boolean() when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Returns true if the cell at the specified location can't be edited.

See:
* `setReadOnly/4`

* `isCurrentCellReadOnly/1`

# `isSelection`

```elixir
-spec isSelection(This) -> boolean() when This :: wxGrid().
```

Returns true if there are currently any selected cells, rows, columns or blocks.

# `isVisible`

```elixir
-spec isVisible(This, Coords) -> boolean()
                   when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

# `isVisible`

```elixir
-spec isVisible(This, Row, Col) -> boolean() when This :: wxGrid(), Row :: integer(), Col :: integer();
               (This, Coords, [Option]) -> boolean()
                   when
                       This :: wxGrid(),
                       Coords :: {R :: integer(), C :: integer()},
                       Option :: {wholeCellVisible, boolean()}.
```

Returns true if a cell is either entirely or at least partially visible in the grid
window.

By default, the cell must be entirely visible for this function to return true but if `wholeCellVisible`
is false, the function returns true even if the cell is only partially visible.

# `isVisible`

```elixir
-spec isVisible(This, Row, Col, [Option]) -> boolean()
                   when
                       This :: wxGrid(),
                       Row :: integer(),
                       Col :: integer(),
                       Option :: {wholeCellVisible, boolean()}.
```

Returns true if a cell is either entirely or at least partially visible in the grid
window.

By default, the cell must be entirely visible for this function to return true but if `wholeCellVisible`
is false, the function returns true even if the cell is only partially visible.

# `makeCellVisible`

```elixir
-spec makeCellVisible(This, Coords) -> ok
                         when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

Brings the specified cell into the visible grid cell area with minimal scrolling.

Does nothing if the cell is already visible.

# `makeCellVisible`

```elixir
-spec makeCellVisible(This, Row, Col) -> ok when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Brings the specified cell into the visible grid cell area with minimal scrolling.

Does nothing if the cell is already visible.

# `moveCursorDown`

```elixir
-spec moveCursorDown(This, ExpandSelection) -> boolean()
                        when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor down by one row.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorDownBlock`

```elixir
-spec moveCursorDownBlock(This, ExpandSelection) -> boolean()
                             when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor down in the current column such that it skips to the beginning or
end of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorLeft`

```elixir
-spec moveCursorLeft(This, ExpandSelection) -> boolean()
                        when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor left by one column.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorLeftBlock`

```elixir
-spec moveCursorLeftBlock(This, ExpandSelection) -> boolean()
                             when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor left in the current row such that it skips to the beginning or end
of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorRight`

```elixir
-spec moveCursorRight(This, ExpandSelection) -> boolean()
                         when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor right by one column.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorRightBlock`

```elixir
-spec moveCursorRightBlock(This, ExpandSelection) -> boolean()
                              when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor right in the current row such that it skips to the beginning or end
of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorUp`

```elixir
-spec moveCursorUp(This, ExpandSelection) -> boolean()
                      when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor up by one row.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `moveCursorUpBlock`

```elixir
-spec moveCursorUpBlock(This, ExpandSelection) -> boolean()
                           when This :: wxGrid(), ExpandSelection :: boolean().
```

Moves the grid cursor up in the current column such that it skips to the beginning or end
of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is true or be
cleared if the argument is false.

# `movePageDown`

```elixir
-spec movePageDown(This) -> boolean() when This :: wxGrid().
```

Moves the grid cursor down by some number of rows so that the previous bottom visible row
becomes the top visible row.

# `movePageUp`

```elixir
-spec movePageUp(This) -> boolean() when This :: wxGrid().
```

Moves the grid cursor up by some number of rows so that the previous top visible row
becomes the bottom visible row.

# `new`

```elixir
-spec new() -> wxGrid().
```

Default constructor.

You must call `Create()` (not implemented in wx) to really create the grid window and
also call `createGrid/4` or `SetTable()` (not implemented in wx) or `AssignTable()` (not implemented in
wx) to initialize its contents.

# `new`

```elixir
-spec new(Parent, Id) -> wxGrid() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```elixir
-spec new(Parent, Id, [Option]) -> wxGrid()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor creating the grid window.

You must call either `createGrid/4` or `SetTable()` (not implemented in wx) or `AssignTable()` (not
implemented in wx) to initialize the grid contents before using it.

# `registerDataType`

```elixir
-spec registerDataType(This, TypeName, Renderer, Editor) -> ok
                          when
                              This :: wxGrid(),
                              TypeName :: unicode:chardata(),
                              Renderer :: wxGridCellRenderer:wxGridCellRenderer(),
                              Editor :: wxGridCellEditor:wxGridCellEditor().
```

Register a new data type.

The data types allow to naturally associate specific renderers and editors to the cells
containing values of the given type. For example, the grid automatically registers a data
type with the name `wxGRID_VALUE_STRING` which uses `m:wxGridCellStringRenderer` and `m:wxGridCellTextEditor`
as its renderer and editor respectively - this is the data type used by all the cells of
the default `wxGridStringTable` (not implemented in wx), so this renderer and editor are
used by default for all grid cells.

However if a custom table returns `wxGRID_VALUE_BOOL` from its `wxGridTableBase::GetTypeName()`
(not implemented in wx) method, then `m:wxGridCellBoolRenderer` and `m:wxGridCellBoolEditor`
are used for it because the grid also registers a boolean data type with this name.

And as this mechanism is completely generic, you may register your own data types using
your own custom renderers and editors. Just remember that the table must identify a cell
as being of the given type for them to be used for this cell.

# `saveEditControlValue`

```elixir
-spec saveEditControlValue(This) -> ok when This :: wxGrid().
```

Sets the value of the current grid cell to the current in-place edit control value.

This is called automatically when the grid cursor moves from the current cell to a new
cell. It is also a good idea to call this function when closing a grid since any edits to
the final cell location will not be saved otherwise.

# `selectAll`

```elixir
-spec selectAll(This) -> ok when This :: wxGrid().
```

Selects all cells in the grid.

# `selectBlock`

```elixir
-spec selectBlock(This, TopLeft, BottomRight) -> ok
                     when
                         This :: wxGrid(),
                         TopLeft :: {R :: integer(), C :: integer()},
                         BottomRight :: {R :: integer(), C :: integer()}.
```

# `selectBlock`

```elixir
-spec selectBlock(This, TopLeft, BottomRight, [Option]) -> ok
                     when
                         This :: wxGrid(),
                         TopLeft :: {R :: integer(), C :: integer()},
                         BottomRight :: {R :: integer(), C :: integer()},
                         Option :: {addToSelected, boolean()}.
```

Selects a rectangular block of cells.

If `addToSelected` is false then any existing selection will be deselected; if true the
column will be added to the existing selection.

# `selectBlock`

```elixir
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol) -> ok
                     when
                         This :: wxGrid(),
                         TopRow :: integer(),
                         LeftCol :: integer(),
                         BottomRow :: integer(),
                         RightCol :: integer().
```

# `selectBlock`

```elixir
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol, [Option]) -> ok
                     when
                         This :: wxGrid(),
                         TopRow :: integer(),
                         LeftCol :: integer(),
                         BottomRow :: integer(),
                         RightCol :: integer(),
                         Option :: {addToSelected, boolean()}.
```

Selects a rectangular block of cells.

If `addToSelected` is false then any existing selection will be deselected; if true the
column will be added to the existing selection.

# `selectCol`

```elixir
-spec selectCol(This, Col) -> ok when This :: wxGrid(), Col :: integer().
```

# `selectCol`

```elixir
-spec selectCol(This, Col, [Option]) -> ok
                   when This :: wxGrid(), Col :: integer(), Option :: {addToSelected, boolean()}.
```

Selects the specified column.

If `addToSelected` is false then any existing selection will be deselected; if true the
column will be added to the existing selection.

This method won't select anything if the current selection mode is wxGridSelectRows.

# `selectRow`

```elixir
-spec selectRow(This, Row) -> ok when This :: wxGrid(), Row :: integer().
```

# `selectRow`

```elixir
-spec selectRow(This, Row, [Option]) -> ok
                   when This :: wxGrid(), Row :: integer(), Option :: {addToSelected, boolean()}.
```

Selects the specified row.

If `addToSelected` is false then any existing selection will be deselected; if true the
row will be added to the existing selection.

This method won't select anything if the current selection mode is wxGridSelectColumns.

# `setCellAlignment`

```elixir
-spec setCellAlignment(This, Row, Col, Horiz, Vert) -> ok
                          when
                              This :: wxGrid(),
                              Row :: integer(),
                              Col :: integer(),
                              Horiz :: integer(),
                              Vert :: integer().
```

Sets the horizontal and vertical alignment for grid cell text at the specified location.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.

Vertical alignment should be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `setCellBackgroundColour`

```elixir
-spec setCellBackgroundColour(This, Row, Col, Colour) -> ok
                                 when
                                     This :: wxGrid(),
                                     Row :: integer(),
                                     Col :: integer(),
                                     Colour :: wx:wx_colour().
```

Set the background colour for the given cell or all cells by default.

# `setCellEditor`

```elixir
-spec setCellEditor(This, Row, Col, Editor) -> ok
                       when
                           This :: wxGrid(),
                           Row :: integer(),
                           Col :: integer(),
                           Editor :: wxGridCellEditor:wxGridCellEditor().
```

Sets the editor for the grid cell at the specified location.

The grid will take ownership of the pointer.

See `m:wxGridCellEditor` and the overview_grid for more information about cell editors
and renderers.

# `setCellFont`

```elixir
-spec setCellFont(This, Row, Col, Font) -> ok
                     when This :: wxGrid(), Row :: integer(), Col :: integer(), Font :: wxFont:wxFont().
```

Sets the font for text in the grid cell at the specified location.

# `setCellRenderer`

```elixir
-spec setCellRenderer(This, Row, Col, Renderer) -> ok
                         when
                             This :: wxGrid(),
                             Row :: integer(),
                             Col :: integer(),
                             Renderer :: wxGridCellRenderer:wxGridCellRenderer().
```

Sets the renderer for the grid cell at the specified location.

The grid will take ownership of the pointer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell editors
and renderers.

# `setCellTextColour`

```elixir
-spec setCellTextColour(This, Row, Col, Colour) -> ok
                           when
                               This :: wxGrid(),
                               Row :: integer(),
                               Col :: integer(),
                               Colour :: wx:wx_colour().
```

Sets the text colour for the given cell.

# `setCellValue`

```elixir
-spec setCellValue(This, Coords, S) -> ok
                      when
                          This :: wxGrid(),
                          Coords :: {R :: integer(), C :: integer()},
                          S :: unicode:chardata().
```

Sets the string value for the cell at the specified location.

For simple applications where a grid object automatically uses a default grid table of
string values you use this function together with `getCellValue/3` to access cell values. For more complex
applications where you have derived your own grid table class that contains various data
types (e.g. numeric, boolean or user-defined custom types) then you only use this function
for those cells that contain string values.

See `wxGridTableBase::CanSetValueAs()` (not implemented in wx) and the overview_grid for
more information.

# `setCellValue`

```elixir
-spec setCellValue(This, Row, Col, S) -> ok
                      when This :: wxGrid(), Row :: integer(), Col :: integer(), S :: unicode:chardata().
```

Sets the string value for the cell at the specified location.

For simple applications where a grid object automatically uses a default grid table of
string values you use this function together with `getCellValue/3` to access cell values. For more complex
applications where you have derived your own grid table class that contains various data
types (e.g. numeric, boolean or user-defined custom types) then you only use this function
for those cells that contain string values.

See `wxGridTableBase::CanSetValueAs()` (not implemented in wx) and the overview_grid for
more information.

# `setColAttr`

```elixir
-spec setColAttr(This, Col, Attr) -> ok
                    when This :: wxGrid(), Col :: integer(), Attr :: wxGridCellAttr:wxGridCellAttr().
```

Sets the cell attributes for all cells in the specified column.

For more information about controlling grid cell attributes see the `m:wxGridCellAttr`
cell attribute class and the overview_grid.

# `setColFormatBool`

```elixir
-spec setColFormatBool(This, Col) -> ok when This :: wxGrid(), Col :: integer().
```

Sets the specified column to display boolean values.

See: `setColFormatCustom/3`

# `setColFormatCustom`

```elixir
-spec setColFormatCustom(This, Col, TypeName) -> ok
                            when This :: wxGrid(), Col :: integer(), TypeName :: unicode:chardata().
```

Sets the specified column to display data in a custom format.

This method provides an alternative to defining a custom grid table which would return `typeName`
from its GetTypeName() method for the cells in this column: while it doesn't really
change the type of the cells in this column, it does associate the renderer and editor
used for the cells of the specified type with them.

See the overview_grid for more information on working with custom data types.

# `setColFormatFloat`

```elixir
-spec setColFormatFloat(This, Col) -> ok when This :: wxGrid(), Col :: integer().
```

# `setColFormatFloat`

```elixir
-spec setColFormatFloat(This, Col, [Option]) -> ok
                           when
                               This :: wxGrid(),
                               Col :: integer(),
                               Option :: {width, integer()} | {precision, integer()}.
```

Sets the specified column to display floating point values with the given width and
precision.

See: `setColFormatCustom/3`

# `setColFormatNumber`

```elixir
-spec setColFormatNumber(This, Col) -> ok when This :: wxGrid(), Col :: integer().
```

Sets the specified column to display integer values.

See: `setColFormatCustom/3`

# `setColLabelAlignment`

```elixir
-spec setColLabelAlignment(This, Horiz, Vert) -> ok
                              when This :: wxGrid(), Horiz :: integer(), Vert :: integer().
```

Sets the horizontal and vertical alignment of column label text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.
Vertical alignment should be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `setColLabelSize`

```elixir
-spec setColLabelSize(This, Height) -> ok when This :: wxGrid(), Height :: integer().
```

Sets the height of the column labels.

If `height` equals to `wxGRID_AUTOSIZE` then height is calculated automatically so that
no label is truncated. Note that this could be slow for a large table.

# `setColLabelValue`

```elixir
-spec setColLabelValue(This, Col, Value) -> ok
                          when This :: wxGrid(), Col :: integer(), Value :: unicode:chardata().
```

Set the value for the given column label.

If you are using a custom grid table you must override `wxGridTableBase::SetColLabelValue()`
(not implemented in wx) for this to have any effect.

# `setColMinimalAcceptableWidth`

```elixir
-spec setColMinimalAcceptableWidth(This, Width) -> ok when This :: wxGrid(), Width :: integer().
```

Sets the minimal `width` to which the user can resize columns.

See: `getColMinimalAcceptableWidth/1`

# `setColMinimalWidth`

```elixir
-spec setColMinimalWidth(This, Col, Width) -> ok
                            when This :: wxGrid(), Col :: integer(), Width :: integer().
```

Sets the minimal `width` for the specified column `col`.

It is usually best to call this method during grid creation as calling it later will not
resize the column to the given minimal width even if it is currently narrower than it.

`width` must be greater than the minimal acceptable column width as returned by `getColMinimalAcceptableWidth/1`.

# `setColSize`

```elixir
-spec setColSize(This, Col, Width) -> ok when This :: wxGrid(), Col :: integer(), Width :: integer().
```

Sets the width of the specified column.

# `setDefaultCellAlignment`

```elixir
-spec setDefaultCellAlignment(This, Horiz, Vert) -> ok
                                 when This :: wxGrid(), Horiz :: integer(), Vert :: integer().
```

Sets the default horizontal and vertical alignment for grid cell text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.
Vertical alignment should be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `setDefaultCellBackgroundColour`

```elixir
-spec setDefaultCellBackgroundColour(This, Colour) -> ok when This :: wxGrid(), Colour :: wx:wx_colour().
```

Sets the default background colour for grid cells.

# `setDefaultCellFont`

```elixir
-spec setDefaultCellFont(This, Font) -> ok when This :: wxGrid(), Font :: wxFont:wxFont().
```

Sets the default font to be used for grid cell text.

# `setDefaultCellTextColour`

```elixir
-spec setDefaultCellTextColour(This, Colour) -> ok when This :: wxGrid(), Colour :: wx:wx_colour().
```

Sets the current default colour for grid cell text.

# `setDefaultColSize`

```elixir
-spec setDefaultColSize(This, Width) -> ok when This :: wxGrid(), Width :: integer().
```

# `setDefaultColSize`

```elixir
-spec setDefaultColSize(This, Width, [Option]) -> ok
                           when
                               This :: wxGrid(),
                               Width :: integer(),
                               Option :: {resizeExistingCols, boolean()}.
```

Sets the default width for columns in the grid.

This will only affect columns subsequently added to the grid unless `resizeExistingCols`
is true.

If `width` is less than `getColMinimalAcceptableWidth/1`, then the minimal acceptable width is used instead of it.

# `setDefaultEditor`

```elixir
-spec setDefaultEditor(This, Editor) -> ok
                          when This :: wxGrid(), Editor :: wxGridCellEditor:wxGridCellEditor().
```

Sets the default editor for grid cells.

The grid will take ownership of the pointer.

See `m:wxGridCellEditor` and the overview_grid for more information about cell editors
and renderers.

# `setDefaultRenderer`

```elixir
-spec setDefaultRenderer(This, Renderer) -> ok
                            when This :: wxGrid(), Renderer :: wxGridCellRenderer:wxGridCellRenderer().
```

Sets the default renderer for grid cells.

The grid will take ownership of the pointer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell editors
and renderers.

# `setDefaultRowSize`

```elixir
-spec setDefaultRowSize(This, Height) -> ok when This :: wxGrid(), Height :: integer().
```

# `setDefaultRowSize`

```elixir
-spec setDefaultRowSize(This, Height, [Option]) -> ok
                           when
                               This :: wxGrid(),
                               Height :: integer(),
                               Option :: {resizeExistingRows, boolean()}.
```

Sets the default height for rows in the grid.

This will only affect rows subsequently added to the grid unless `resizeExistingRows` is true.

If `height` is less than `getRowMinimalAcceptableHeight/1`, then the minimal acceptable height is used instead of it.

# `setGridCursor`

```elixir
-spec setGridCursor(This, Coords) -> ok
                       when This :: wxGrid(), Coords :: {R :: integer(), C :: integer()}.
```

Set the grid cursor to the specified cell.

The grid cursor indicates the current cell and can be moved by the user using the arrow
keys or the mouse.

Calling this function generates a `wxEVT_GRID_SELECT_CELL` event and if the event handler
vetoes this event, the cursor is not moved.

This function doesn't make the target call visible, use `GoToCell()` (not implemented in
wx) to do this.

# `setGridCursor`

```elixir
-spec setGridCursor(This, Row, Col) -> ok when This :: wxGrid(), Row :: integer(), Col :: integer().
```

Set the grid cursor to the specified cell.

The grid cursor indicates the current cell and can be moved by the user using the arrow
keys or the mouse.

Calling this function generates a `wxEVT_GRID_SELECT_CELL` event and if the event handler
vetoes this event, the cursor is not moved.

This function doesn't make the target call visible, use `GoToCell()` (not implemented in
wx) to do this.

# `setGridLineColour`

```elixir
-spec setGridLineColour(This, Colour) -> ok when This :: wxGrid(), Colour :: wx:wx_colour().
```

Sets the colour used to draw grid lines.

# `setLabelBackgroundColour`

```elixir
-spec setLabelBackgroundColour(This, Colour) -> ok when This :: wxGrid(), Colour :: wx:wx_colour().
```

Sets the background colour for row and column labels.

# `setLabelFont`

```elixir
-spec setLabelFont(This, Font) -> ok when This :: wxGrid(), Font :: wxFont:wxFont().
```

Sets the font for row and column labels.

# `setLabelTextColour`

```elixir
-spec setLabelTextColour(This, Colour) -> ok when This :: wxGrid(), Colour :: wx:wx_colour().
```

Sets the colour for row and column label text.

# `setMargins`

```elixir
-spec setMargins(This, ExtraWidth, ExtraHeight) -> ok
                    when This :: wxGrid(), ExtraWidth :: integer(), ExtraHeight :: integer().
```

Sets the extra margins used around the grid area.

A grid may occupy more space than needed for its data display and this function allows
setting how big this extra space is

# `setReadOnly`

```elixir
-spec setReadOnly(This, Row, Col) -> ok when This :: wxGrid(), Row :: integer(), Col :: integer().
```

# `setReadOnly`

```elixir
-spec setReadOnly(This, Row, Col, [Option]) -> ok
                     when
                         This :: wxGrid(),
                         Row :: integer(),
                         Col :: integer(),
                         Option :: {isReadOnly, boolean()}.
```

Makes the cell at the specified location read-only or editable.

See: `isReadOnly/3`

# `setRowAttr`

```elixir
-spec setRowAttr(This, Row, Attr) -> ok
                    when This :: wxGrid(), Row :: integer(), Attr :: wxGridCellAttr:wxGridCellAttr().
```

Sets the cell attributes for all cells in the specified row.

The grid takes ownership of the attribute pointer.

See the `m:wxGridCellAttr` class for more information about controlling cell attributes.

# `setRowLabelAlignment`

```elixir
-spec setRowLabelAlignment(This, Horiz, Vert) -> ok
                              when This :: wxGrid(), Horiz :: integer(), Vert :: integer().
```

Sets the horizontal and vertical alignment of row label text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT`.
Vertical alignment should be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `setRowLabelSize`

```elixir
-spec setRowLabelSize(This, Width) -> ok when This :: wxGrid(), Width :: integer().
```

Sets the width of the row labels.

If `width` equals `wxGRID_AUTOSIZE` then width is calculated automatically so that no
label is truncated. Note that this could be slow for a large table.

# `setRowLabelValue`

```elixir
-spec setRowLabelValue(This, Row, Value) -> ok
                          when This :: wxGrid(), Row :: integer(), Value :: unicode:chardata().
```

Sets the value for the given row label.

If you are using a derived grid table you must override `wxGridTableBase::SetRowLabelValue()`
(not implemented in wx) for this to have any effect.

# `setRowMinimalAcceptableHeight`

```elixir
-spec setRowMinimalAcceptableHeight(This, Height) -> ok when This :: wxGrid(), Height :: integer().
```

Sets the minimal row `height` used by default.

See `setColMinimalAcceptableWidth/2` for more information.

# `setRowMinimalHeight`

```elixir
-spec setRowMinimalHeight(This, Row, Height) -> ok
                             when This :: wxGrid(), Row :: integer(), Height :: integer().
```

Sets the minimal `height` for the specified `row`.

See `setColMinimalWidth/3` for more information.

# `setRowSize`

```elixir
-spec setRowSize(This, Row, Height) -> ok when This :: wxGrid(), Row :: integer(), Height :: integer().
```

Sets the height of the specified row.

See `setColSize/3` for more information.

# `setScrollLineX`

```elixir
-spec setScrollLineX(This, X) -> ok when This :: wxGrid(), X :: integer().
```

Sets the number of pixels per horizontal scroll increment.

The default is 15.

See:
* `getScrollLineX/1`

* `getScrollLineY/1`

* `setScrollLineY/2`

# `setScrollLineY`

```elixir
-spec setScrollLineY(This, Y) -> ok when This :: wxGrid(), Y :: integer().
```

Sets the number of pixels per vertical scroll increment.

The default is 15.

See:
* `getScrollLineX/1`

* `getScrollLineY/1`

* `setScrollLineX/2`

# `setSelectionBackground`

```elixir
-spec setSelectionBackground(This, C) -> ok when This :: wxGrid(), C :: wx:wx_colour().
```

Set the colour to be used for drawing the selection background.

# `setSelectionForeground`

```elixir
-spec setSelectionForeground(This, C) -> ok when This :: wxGrid(), C :: wx:wx_colour().
```

Set the colour to be used for drawing the selection foreground.

# `setSelectionMode`

```elixir
-spec setSelectionMode(This, Selmode) -> ok when This :: wxGrid(), Selmode :: wx:wx_enum().
```

Set the selection behaviour of the grid.

The existing selection is converted to conform to the new mode if possible and discarded
otherwise (e.g. any individual selected cells are deselected if the new mode allows only
the selection of the entire rows or columns).

# `showCellEditControl`

```elixir
-spec showCellEditControl(This) -> ok when This :: wxGrid().
```

Displays the active in-place cell edit control for the current cell after it was hidden.

This method should only be called after calling `hideCellEditControl/1`, to start editing the current grid cell
use `enableCellEditControl/2` instead.

# `xToCol`

```elixir
-spec xToCol(This, X) -> integer() when This :: wxGrid(), X :: integer().
```

# `xToCol`

```elixir
-spec xToCol(This, X, [Option]) -> integer()
                when This :: wxGrid(), X :: integer(), Option :: {clipToMinMax, boolean()}.
```

Returns the column at the given pixel position depending on the window.

Return: The column index or `wxNOT_FOUND`.

# `xToEdgeOfCol`

```elixir
-spec xToEdgeOfCol(This, X) -> integer() when This :: wxGrid(), X :: integer().
```

Returns the column whose right hand edge is close to the given logical `x` position.

If no column edge is near to this position `wxNOT_FOUND` is returned.

# `yToEdgeOfRow`

```elixir
-spec yToEdgeOfRow(This, Y) -> integer() when This :: wxGrid(), Y :: integer().
```

Returns the row whose bottom edge is close to the given logical `y` position.

If no row edge is near to this position `wxNOT_FOUND` is returned.

# `yToRow`

```elixir
-spec yToRow(This, Y) -> integer() when This :: wxGrid(), Y :: integer().
```

# `yToRow`

```elixir
-spec yToRow(This, Y, [Option]) -> integer()
                when This :: wxGrid(), Y :: integer(), Option :: {clipToMinMax, boolean()}.
```

Returns the grid row that corresponds to the logical `y` coordinate.

The parameter `gridWindow` is new since wxWidgets 3.1.3. If it is specified, i.e.
non-NULL, only the cells of this window are considered, i.e. the function returns `wxNOT_FOUND`
if `y` is out of bounds.

If `gridWindow` is NULL, the function returns `wxNOT_FOUND` only if there is no row at
all at the `y` position.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
