# `wxPrinter`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPrinter.erl#L58)

This class represents the Windows or PostScript printer, and is the vehicle through which
printing may be launched by an application.

Printing can also be achieved through using of lower functions and classes, but this and
associated classes provide a more convenient and general method of printing.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPrintout`

* `m:wxPrintPreview`

wxWidgets docs: [wxPrinter](https://docs.wxwidgets.org/3.2/classwx_printer.html)

# `wxPrinter`

```elixir
-type wxPrinter() :: wx:wx_object().
```

# `createAbortWindow`

```elixir
-spec createAbortWindow(This, Parent, Printout) -> wxDialog:wxDialog()
                           when
                               This :: wxPrinter(),
                               Parent :: wxWindow:wxWindow(),
                               Printout :: wxPrintout:wxPrintout().
```

Creates the default printing abort window, with a cancel button.

# `destroy`

```elixir
-spec destroy(This :: wxPrinter()) -> ok.
```

Destroys the object

# `getAbort`

```elixir
-spec getAbort(This) -> boolean() when This :: wxPrinter().
```

Returns true if the user has aborted the print job.

# `getLastError`

```elixir
-spec getLastError() -> wx:wx_enum().
```

Return last error.

Valid after calling `print/4`, `printDialog/2` or `wxPrintPreview:print/2`.

These functions set last error to `wxPRINTER_NO_ERROR` if no error happened.

Returned value is one of the following:

# `getPrintDialogData`

```elixir
-spec getPrintDialogData(This) -> wxPrintDialogData:wxPrintDialogData() when This :: wxPrinter().
```

Returns the print data associated with the printer object.

# `new`

```elixir
-spec new() -> wxPrinter().
```

# `new`

```elixir
-spec new([Option]) -> wxPrinter() when Option :: {data, wxPrintDialogData:wxPrintDialogData()}.
```

Constructor.

Pass an optional pointer to a block of print dialog data, which will be copied to the
printer object's local data.

See:
* `m:wxPrintDialogData`

* `m:wxPrintData`

# `print`

```elixir
-spec print(This, Parent, Printout) -> boolean()
               when
                   This :: wxPrinter(),
                   Parent :: wxWindow:wxWindow(),
                   Printout :: wxPrintout:wxPrintout().
```

# `print`

```elixir
-spec print(This, Parent, Printout, [Option]) -> boolean()
               when
                   This :: wxPrinter(),
                   Parent :: wxWindow:wxWindow(),
                   Printout :: wxPrintout:wxPrintout(),
                   Option :: {prompt, boolean()}.
```

Starts the printing process.

Provide a parent window, a user-defined `m:wxPrintout` object which controls the printing
of a document, and whether the print dialog should be invoked first.

`print/4` could return false if there was a problem initializing the printer device context
(current printer not set, for example) or the user cancelled printing. Call `getLastError/0` to get
detailed information about the kind of the error.

# `printDialog`

```elixir
-spec printDialog(This, Parent) -> wxDC:wxDC() when This :: wxPrinter(), Parent :: wxWindow:wxWindow().
```

Invokes the print dialog.

If successful (the user did not press Cancel and no error occurred), a suitable device
context will be returned; otherwise NULL is returned; call `getLastError/0` to get detailed information
about the kind of the error.

Remark: The application must delete this device context to avoid a memory leak.

# `reportError`

```elixir
-spec reportError(This, Parent, Printout, Message) -> ok
                     when
                         This :: wxPrinter(),
                         Parent :: wxWindow:wxWindow(),
                         Printout :: wxPrintout:wxPrintout(),
                         Message :: unicode:chardata().
```

Default error-reporting function.

# `setup`

```elixir
-spec setup(This, Parent) -> boolean() when This :: wxPrinter(), Parent :: wxWindow:wxWindow().
```

Invokes the print setup dialog.

Deprecated:

The setup dialog is obsolete, though retained for backward compatibility.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
