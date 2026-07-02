# `wxPageSetupDialogData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPageSetupDialogData.erl#L58)

This class holds a variety of information related to `m:wxPageSetupDialog`.

It contains a `m:wxPrintData` member which is used to hold basic printer configuration
data (as opposed to the user-interface configuration settings stored by `m:wxPageSetupDialogData`).

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPageSetupDialog`

wxWidgets docs: [wxPageSetupDialogData](https://docs.wxwidgets.org/3.2/classwx_page_setup_dialog_data.html)

# `wxPageSetupDialogData`

```elixir
-type wxPageSetupDialogData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPageSetupDialogData()) -> ok.
```

Destroys the object

# `enableHelp`

```elixir
-spec enableHelp(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the "Help" button (Windows only).

# `enableMargins`

```elixir
-spec enableMargins(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the margin controls (Windows only).

# `enableOrientation`

```elixir
-spec enableOrientation(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the orientation control (Windows only).

# `enablePaper`

```elixir
-spec enablePaper(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the paper size control (Windows only).

# `enablePrinter`

```elixir
-spec enablePrinter(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the "Printer" button, which invokes a printer setup dialog.

# `getDefaultInfo`

```elixir
-spec getDefaultInfo(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).

# `getDefaultMinMargins`

```elixir
-spec getDefaultMinMargins(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the page setup dialog will take its minimum margin values from the
currently selected printer properties (Windows only).

# `getEnableHelp`

```elixir
-spec getEnableHelp(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the printer setup button is enabled.

# `getEnableMargins`

```elixir
-spec getEnableMargins(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the margin controls are enabled (Windows only).

# `getEnableOrientation`

```elixir
-spec getEnableOrientation(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the orientation control is enabled (Windows only).

# `getEnablePaper`

```elixir
-spec getEnablePaper(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the paper size control is enabled (Windows only).

# `getEnablePrinter`

```elixir
-spec getEnablePrinter(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the printer setup button is enabled.

# `getMarginBottomRight`

```elixir
-spec getMarginBottomRight(This) -> {X :: integer(), Y :: integer()}
                              when This :: wxPageSetupDialogData().
```

Returns the right (x) and bottom (y) margins in millimetres.

# `getMarginTopLeft`

```elixir
-spec getMarginTopLeft(This) -> {X :: integer(), Y :: integer()} when This :: wxPageSetupDialogData().
```

Returns the left (x) and top (y) margins in millimetres.

# `getMinMarginBottomRight`

```elixir
-spec getMinMarginBottomRight(This) -> {X :: integer(), Y :: integer()}
                                 when This :: wxPageSetupDialogData().
```

Returns the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `getMinMarginTopLeft`

```elixir
-spec getMinMarginTopLeft(This) -> {X :: integer(), Y :: integer()} when This :: wxPageSetupDialogData().
```

Returns the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `getPaperId`

```elixir
-spec getPaperId(This) -> wx:wx_enum() when This :: wxPageSetupDialogData().
```

Returns the paper id (stored in the internal `m:wxPrintData` object).

See: `wxPrintData:setPaperId/2`

# `getPaperSize`

```elixir
-spec getPaperSize(This) -> {W :: integer(), H :: integer()} when This :: wxPageSetupDialogData().
```

Returns the paper size in millimetres.

# `getPrintData`

```elixir
-spec getPrintData(This) -> wxPrintData:wxPrintData() when This :: wxPageSetupDialogData().
```

# `isOk`

```elixir
-spec isOk(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the print data associated with the dialog data is valid.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.

# `new`

```elixir
-spec new() -> wxPageSetupDialogData().
```

Default constructor.

# `new`

```elixir
-spec new(PrintData) -> wxPageSetupDialogData()
             when PrintData :: wxPrintData:wxPrintData() | wxPageSetupDialogData:wxPageSetupDialogData().
```

Construct an object from a print data object.

# `setDefaultInfo`

```elixir
-spec setDefaultInfo(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Pass true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).

# `setDefaultMinMargins`

```elixir
-spec setDefaultMinMargins(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Pass true if the page setup dialog will take its minimum margin values from the currently
selected printer properties (Windows only).

Units are in millimetres.

# `setMarginBottomRight`

```elixir
-spec setMarginBottomRight(This, Pt) -> ok
                              when
                                  This :: wxPageSetupDialogData(),
                                  Pt :: {X :: integer(), Y :: integer()}.
```

Sets the right (x) and bottom (y) margins in millimetres.

# `setMarginTopLeft`

```elixir
-spec setMarginTopLeft(This, Pt) -> ok
                          when This :: wxPageSetupDialogData(), Pt :: {X :: integer(), Y :: integer()}.
```

Sets the left (x) and top (y) margins in millimetres.

# `setMinMarginBottomRight`

```elixir
-spec setMinMarginBottomRight(This, Pt) -> ok
                                 when
                                     This :: wxPageSetupDialogData(),
                                     Pt :: {X :: integer(), Y :: integer()}.
```

Sets the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `setMinMarginTopLeft`

```elixir
-spec setMinMarginTopLeft(This, Pt) -> ok
                             when
                                 This :: wxPageSetupDialogData(), Pt :: {X :: integer(), Y :: integer()}.
```

Sets the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `setPaperId`

```elixir
-spec setPaperId(This, Id) -> ok when This :: wxPageSetupDialogData(), Id :: wx:wx_enum().
```

Sets the paper size id.

Calling this function overrides the explicit paper dimensions passed in `setPaperSize/2`.

See: `wxPrintData:setPaperId/2`

# `setPaperSize`

```elixir
-spec setPaperSize(This, Size) -> ok
                      when This :: wxPageSetupDialogData(), Size :: {W :: integer(), H :: integer()}.
```

Sets the paper size in millimetres.

If a corresponding paper id is found, it will be set in the internal `m:wxPrintData`
object, otherwise the paper size overrides the paper id.

# `setPrintData`

```elixir
-spec setPrintData(This, PrintData) -> ok
                      when This :: wxPageSetupDialogData(), PrintData :: wxPrintData:wxPrintData().
```

Sets the print data associated with this object.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
