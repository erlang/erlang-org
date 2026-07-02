# `wxPrintData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPrintData.erl#L58)

This class holds a variety of information related to printers and printer device
contexts.

This class is used to create a `wxPrinterDC` (not implemented in wx) and a `m:wxPostScriptDC`.
It is also used as a data member of `m:wxPrintDialogData` and `m:wxPageSetupDialogData`,
as part of the mechanism for transferring data between the print dialogs and the application.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPageSetupDialog`

* `m:wxPrintDialogData`

* `m:wxPageSetupDialogData`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_print)

* `m:wxPostScriptDC`

wxWidgets docs: [wxPrintData](https://docs.wxwidgets.org/3.2/classwx_print_data.html)

# `wxPrintData`

```elixir
-type wxPrintData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPrintData()) -> ok.
```

Destroys the object

# `getBin`

```elixir
-spec getBin(This) -> wx:wx_enum() when This :: wxPrintData().
```

Returns the current bin (papersource).

By default, the system is left to select the bin (`wxPRINTBIN_DEFAULT` is returned).

See `setBin/2` for the full list of bin values.

# `getCollate`

```elixir
-spec getCollate(This) -> boolean() when This :: wxPrintData().
```

Returns true if collation is on.

# `getColour`

```elixir
-spec getColour(This) -> boolean() when This :: wxPrintData().
```

Returns true if colour printing is on.

# `getDuplex`

```elixir
-spec getDuplex(This) -> wx:wx_enum() when This :: wxPrintData().
```

Returns the duplex mode.

One of wxDUPLEX_SIMPLEX, wxDUPLEX_HORIZONTAL, wxDUPLEX_VERTICAL.

# `getNoCopies`

```elixir
-spec getNoCopies(This) -> integer() when This :: wxPrintData().
```

Returns the number of copies requested by the user.

# `getOrientation`

```elixir
-spec getOrientation(This) -> wx:wx_enum() when This :: wxPrintData().
```

Gets the orientation.

This can be wxLANDSCAPE or wxPORTRAIT.

# `getPaperId`

```elixir
-spec getPaperId(This) -> wx:wx_enum() when This :: wxPrintData().
```

Returns the paper size id.

See: `setPaperId/2`

# `getPrinterName`

```elixir
-spec getPrinterName(This) -> unicode:charlist() when This :: wxPrintData().
```

Returns the printer name.

If the printer name is the empty string, it indicates that the default printer should be
used.

# `getQuality`

```elixir
-spec getQuality(This) -> integer() when This :: wxPrintData().
```

Returns the current print quality.

This can be a positive integer, denoting the number of dots per inch, or one of the
following identifiers:

* wxPRINT_QUALITY_HIGH

* wxPRINT_QUALITY_MEDIUM

* wxPRINT_QUALITY_LOW

* wxPRINT_QUALITY_DRAFT

On input you should pass one of these identifiers, but on return you may get back a
positive integer indicating the current resolution setting.

# `isOk`

```elixir
-spec isOk(This) -> boolean() when This :: wxPrintData().
```

Returns true if the print data is valid for using in print dialogs.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.

# `new`

```elixir
-spec new() -> wxPrintData().
```

Default constructor.

# `new`

```elixir
-spec new(Data) -> wxPrintData() when Data :: wxPrintData().
```

Copy constructor.

# `setBin`

```elixir
-spec setBin(This, Flag) -> ok when This :: wxPrintData(), Flag :: wx:wx_enum().
```

Sets the current bin.

# `setCollate`

```elixir
-spec setCollate(This, Flag) -> ok when This :: wxPrintData(), Flag :: boolean().
```

Sets collation to on or off.

# `setColour`

```elixir
-spec setColour(This, Flag) -> ok when This :: wxPrintData(), Flag :: boolean().
```

Sets colour printing on or off.

# `setDuplex`

```elixir
-spec setDuplex(This, Mode) -> ok when This :: wxPrintData(), Mode :: wx:wx_enum().
```

Returns the duplex mode.

One of wxDUPLEX_SIMPLEX, wxDUPLEX_HORIZONTAL, wxDUPLEX_VERTICAL.

# `setNoCopies`

```elixir
-spec setNoCopies(This, N) -> ok when This :: wxPrintData(), N :: integer().
```

Sets the default number of copies to be printed out.

# `setOrientation`

```elixir
-spec setOrientation(This, Orientation) -> ok when This :: wxPrintData(), Orientation :: wx:wx_enum().
```

Sets the orientation.

This can be wxLANDSCAPE or wxPORTRAIT.

# `setPaperId`

```elixir
-spec setPaperId(This, PaperId) -> ok when This :: wxPrintData(), PaperId :: wx:wx_enum().
```

Sets the paper id.

This indicates the type of paper to be used. For a mapping between paper id, paper size
and string name, see wxPrintPaperDatabase in `"paper.h"` (not yet documented).

# `setPrinterName`

```elixir
-spec setPrinterName(This, PrinterName) -> ok
                        when This :: wxPrintData(), PrinterName :: unicode:chardata().
```

Sets the printer name.

This can be the empty string to indicate that the default printer should be used.

# `setQuality`

```elixir
-spec setQuality(This, Quality) -> ok when This :: wxPrintData(), Quality :: integer().
```

Sets the desired print quality.

This can be a positive integer, denoting the number of dots per inch, or one of the
following identifiers:

* wxPRINT_QUALITY_HIGH

* wxPRINT_QUALITY_MEDIUM

* wxPRINT_QUALITY_LOW

* wxPRINT_QUALITY_DRAFT

On input you should pass one of these identifiers, but on return you may get back a
positive integer indicating the current resolution setting.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
