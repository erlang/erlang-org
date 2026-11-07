# `wxPageSetupDialogData`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPageSetupDialogData.erl#L58)

This class holds a variety of information related to `m:wxPageSetupDialog`.

It contains a `m:wxPrintData` member which is used to hold basic printer configuration
data (as opposed to the user-interface configuration settings stored by `m:wxPageSetupDialogData`).

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPageSetupDialog`

wxWidgets docs: [wxPageSetupDialogData](https://docs.wxwidgets.org/3.2/classwx_page_setup_dialog_data.html)

# `wxPageSetupDialogData`

```erlang
-type wxPageSetupDialogData() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPageSetupDialogData()) -> ok.
```

Destroys the object

# `enableHelp`

```erlang
-spec enableHelp(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the "Help" button (Windows only).

# `enableMargins`

```erlang
-spec enableMargins(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the margin controls (Windows only).

# `enableOrientation`

```erlang
-spec enableOrientation(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the orientation control (Windows only).

# `enablePaper`

```erlang
-spec enablePaper(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the paper size control (Windows only).

# `enablePrinter`

```erlang
-spec enablePrinter(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Enables or disables the "Printer" button, which invokes a printer setup dialog.

# `getDefaultInfo`

```erlang
-spec getDefaultInfo(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).

# `getDefaultMinMargins`

```erlang
-spec getDefaultMinMargins(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the page setup dialog will take its minimum margin values from the
currently selected printer properties (Windows only).

# `getEnableHelp`

```erlang
-spec getEnableHelp(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the printer setup button is enabled.

# `getEnableMargins`

```erlang
-spec getEnableMargins(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the margin controls are enabled (Windows only).

# `getEnableOrientation`

```erlang
-spec getEnableOrientation(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the orientation control is enabled (Windows only).

# `getEnablePaper`

```erlang
-spec getEnablePaper(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the paper size control is enabled (Windows only).

# `getEnablePrinter`

```erlang
-spec getEnablePrinter(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the printer setup button is enabled.

# `getMarginBottomRight`

```erlang
-spec getMarginBottomRight(This) -> {X :: integer(), Y :: integer()}
                              when This :: wxPageSetupDialogData().
```

Returns the right (x) and bottom (y) margins in millimetres.

# `getMarginTopLeft`

```erlang
-spec getMarginTopLeft(This) -> {X :: integer(), Y :: integer()} when This :: wxPageSetupDialogData().
```

Returns the left (x) and top (y) margins in millimetres.

# `getMinMarginBottomRight`

```erlang
-spec getMinMarginBottomRight(This) -> {X :: integer(), Y :: integer()}
                                 when This :: wxPageSetupDialogData().
```

Returns the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `getMinMarginTopLeft`

```erlang
-spec getMinMarginTopLeft(This) -> {X :: integer(), Y :: integer()} when This :: wxPageSetupDialogData().
```

Returns the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `getPaperId`

```erlang
-spec getPaperId(This) -> wx:wx_enum() when This :: wxPageSetupDialogData().
```

Returns the paper id (stored in the internal `m:wxPrintData` object).

See: `wxPrintData:setPaperId/2`

# `getPaperSize`

```erlang
-spec getPaperSize(This) -> {W :: integer(), H :: integer()} when This :: wxPageSetupDialogData().
```

Returns the paper size in millimetres.

# `getPrintData`

```erlang
-spec getPrintData(This) -> wxPrintData:wxPrintData() when This :: wxPageSetupDialogData().
```

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxPageSetupDialogData().
```

Returns true if the print data associated with the dialog data is valid.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.

# `new`

```erlang
-spec new() -> wxPageSetupDialogData().
```

Default constructor.

# `new`

```erlang
-spec new(PrintData) -> wxPageSetupDialogData()
             when PrintData :: wxPrintData:wxPrintData() | wxPageSetupDialogData:wxPageSetupDialogData().
```

Construct an object from a print data object.

# `setDefaultInfo`

```erlang
-spec setDefaultInfo(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Pass true if the dialog will simply return default printer information (such as
orientation) instead of showing a dialog (Windows only).

# `setDefaultMinMargins`

```erlang
-spec setDefaultMinMargins(This, Flag) -> ok when This :: wxPageSetupDialogData(), Flag :: boolean().
```

Pass true if the page setup dialog will take its minimum margin values from the currently
selected printer properties (Windows only).

Units are in millimetres.

# `setMarginBottomRight`

```erlang
-spec setMarginBottomRight(This, Pt) -> ok
                              when
                                  This :: wxPageSetupDialogData(),
                                  Pt :: {X :: integer(), Y :: integer()}.
```

Sets the right (x) and bottom (y) margins in millimetres.

# `setMarginTopLeft`

```erlang
-spec setMarginTopLeft(This, Pt) -> ok
                          when This :: wxPageSetupDialogData(), Pt :: {X :: integer(), Y :: integer()}.
```

Sets the left (x) and top (y) margins in millimetres.

# `setMinMarginBottomRight`

```erlang
-spec setMinMarginBottomRight(This, Pt) -> ok
                                 when
                                     This :: wxPageSetupDialogData(),
                                     Pt :: {X :: integer(), Y :: integer()}.
```

Sets the right (x) and bottom (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `setMinMarginTopLeft`

```erlang
-spec setMinMarginTopLeft(This, Pt) -> ok
                             when
                                 This :: wxPageSetupDialogData(), Pt :: {X :: integer(), Y :: integer()}.
```

Sets the left (x) and top (y) minimum margins the user can enter (Windows only).

Units are in millimetres.

# `setPaperId`

```erlang
-spec setPaperId(This, Id) -> ok when This :: wxPageSetupDialogData(), Id :: wx:wx_enum().
```

Sets the paper size id.

Calling this function overrides the explicit paper dimensions passed in `setPaperSize/2`.

See: `wxPrintData:setPaperId/2`

# `setPaperSize`

```erlang
-spec setPaperSize(This, Size) -> ok
                      when This :: wxPageSetupDialogData(), Size :: {W :: integer(), H :: integer()}.
```

Sets the paper size in millimetres.

If a corresponding paper id is found, it will be set in the internal `m:wxPrintData`
object, otherwise the paper size overrides the paper id.

# `setPrintData`

```erlang
-spec setPrintData(This, PrintData) -> ok
                      when This :: wxPageSetupDialogData(), PrintData :: wxPrintData:wxPrintData().
```

Sets the print data associated with this object.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
