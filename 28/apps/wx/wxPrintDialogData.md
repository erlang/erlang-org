# `wxPrintDialogData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPrintDialogData.erl#L58)

This class holds information related to the visual characteristics of `m:wxPrintDialog`.

It contains a `m:wxPrintData` object with underlying printing settings.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_print)

wxWidgets docs: [wxPrintDialogData](https://docs.wxwidgets.org/3.2/classwx_print_dialog_data.html)

# `wxPrintDialogData`

```elixir
-type wxPrintDialogData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPrintDialogData()) -> ok.
```

Destroys the object

# `enableHelp`

```elixir
-spec enableHelp(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Help" button.

# `enablePageNumbers`

```elixir
-spec enablePageNumbers(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Page numbers" controls.

# `enablePrintToFile`

```elixir
-spec enablePrintToFile(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Print to file" checkbox.

# `enableSelection`

```elixir
-spec enableSelection(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Selection" radio button.

# `getAllPages`

```elixir
-spec getAllPages(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that all pages be printed.

# `getCollate`

```elixir
-spec getCollate(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that the document(s) be collated.

# `getFromPage`

```elixir
-spec getFromPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `from` page number, as entered by the user.

# `getMaxPage`

```elixir
-spec getMaxPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `maximum` page number.

# `getMinPage`

```elixir
-spec getMinPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `minimum` page number.

# `getNoCopies`

```elixir
-spec getNoCopies(This) -> integer() when This :: wxPrintDialogData().
```

Returns the number of copies requested by the user.

# `getPrintData`

```elixir
-spec getPrintData(This) -> wxPrintData:wxPrintData() when This :: wxPrintDialogData().
```

Returns a reference to the internal `m:wxPrintData` object.

# `getPrintToFile`

```elixir
-spec getPrintToFile(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user has selected printing to a file.

# `getSelection`

```elixir
-spec getSelection(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that the selection be printed (where "selection" is a
concept specific to the application).

# `getToPage`

```elixir
-spec getToPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `"print to"` page number, as entered by the user.

# `isOk`

```elixir
-spec isOk(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the print data is valid for using in print dialogs.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.

# `new`

```elixir
-spec new() -> wxPrintDialogData().
```

Default constructor.

# `new`

```elixir
-spec new(DialogData) -> wxPrintDialogData()
             when DialogData :: wxPrintDialogData:wxPrintDialogData() | wxPrintData:wxPrintData().
```

Copy constructor.

# `setCollate`

```elixir
-spec setCollate(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Sets the "Collate" checkbox to true or false.

# `setFromPage`

```elixir
-spec setFromPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `from` page number.

# `setMaxPage`

```elixir
-spec setMaxPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `maximum` page number.

# `setMinPage`

```elixir
-spec setMinPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `minimum` page number.

# `setNoCopies`

```elixir
-spec setNoCopies(This, N) -> ok when This :: wxPrintDialogData(), N :: integer().
```

Sets the default number of copies the user has requested to be printed out.

# `setPrintData`

```elixir
-spec setPrintData(This, PrintData) -> ok
                      when This :: wxPrintDialogData(), PrintData :: wxPrintData:wxPrintData().
```

Sets the internal `m:wxPrintData`.

# `setPrintToFile`

```elixir
-spec setPrintToFile(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Sets the "Print to file" checkbox to true or false.

# `setSelection`

```elixir
-spec setSelection(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Selects the "Selection" radio button.

The effect of printing the selection depends on how the application implements this
command, if at all.

# `setToPage`

```elixir
-spec setToPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `"print to"` page number.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
