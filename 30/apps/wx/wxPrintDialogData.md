# `wxPrintDialogData`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPrintDialogData.erl#L58)

This class holds information related to the visual characteristics of `m:wxPrintDialog`.

It contains a `m:wxPrintData` object with underlying printing settings.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_print)

wxWidgets docs: [wxPrintDialogData](https://docs.wxwidgets.org/3.2/classwx_print_dialog_data.html)

# `wxPrintDialogData`

```erlang
-type wxPrintDialogData() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPrintDialogData()) -> ok.
```

Destroys the object

# `enableHelp`

```erlang
-spec enableHelp(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Help" button.

# `enablePageNumbers`

```erlang
-spec enablePageNumbers(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Page numbers" controls.

# `enablePrintToFile`

```erlang
-spec enablePrintToFile(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Print to file" checkbox.

# `enableSelection`

```erlang
-spec enableSelection(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Enables or disables the "Selection" radio button.

# `getAllPages`

```erlang
-spec getAllPages(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that all pages be printed.

# `getCollate`

```erlang
-spec getCollate(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that the document(s) be collated.

# `getFromPage`

```erlang
-spec getFromPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `from` page number, as entered by the user.

# `getMaxPage`

```erlang
-spec getMaxPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `maximum` page number.

# `getMinPage`

```erlang
-spec getMinPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `minimum` page number.

# `getNoCopies`

```erlang
-spec getNoCopies(This) -> integer() when This :: wxPrintDialogData().
```

Returns the number of copies requested by the user.

# `getPrintData`

```erlang
-spec getPrintData(This) -> wxPrintData:wxPrintData() when This :: wxPrintDialogData().
```

Returns a reference to the internal `m:wxPrintData` object.

# `getPrintToFile`

```erlang
-spec getPrintToFile(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user has selected printing to a file.

# `getSelection`

```erlang
-spec getSelection(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the user requested that the selection be printed (where "selection" is a
concept specific to the application).

# `getToPage`

```erlang
-spec getToPage(This) -> integer() when This :: wxPrintDialogData().
```

Returns the `"print to"` page number, as entered by the user.

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxPrintDialogData().
```

Returns true if the print data is valid for using in print dialogs.

This can return false on Windows if the current printer is not set, for example. On all
other platforms, it returns true.

# `new`

```erlang
-spec new() -> wxPrintDialogData().
```

Default constructor.

# `new`

```erlang
-spec new(DialogData) -> wxPrintDialogData()
             when DialogData :: wxPrintDialogData:wxPrintDialogData() | wxPrintData:wxPrintData().
```

Copy constructor.

# `setCollate`

```erlang
-spec setCollate(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Sets the "Collate" checkbox to true or false.

# `setFromPage`

```erlang
-spec setFromPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `from` page number.

# `setMaxPage`

```erlang
-spec setMaxPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `maximum` page number.

# `setMinPage`

```erlang
-spec setMinPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `minimum` page number.

# `setNoCopies`

```erlang
-spec setNoCopies(This, N) -> ok when This :: wxPrintDialogData(), N :: integer().
```

Sets the default number of copies the user has requested to be printed out.

# `setPrintData`

```erlang
-spec setPrintData(This, PrintData) -> ok
                      when This :: wxPrintDialogData(), PrintData :: wxPrintData:wxPrintData().
```

Sets the internal `m:wxPrintData`.

# `setPrintToFile`

```erlang
-spec setPrintToFile(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Sets the "Print to file" checkbox to true or false.

# `setSelection`

```erlang
-spec setSelection(This, Flag) -> ok when This :: wxPrintDialogData(), Flag :: boolean().
```

Selects the "Selection" radio button.

The effect of printing the selection depends on how the application implements this
command, if at all.

# `setToPage`

```erlang
-spec setToPage(This, Page) -> ok when This :: wxPrintDialogData(), Page :: integer().
```

Sets the `"print to"` page number.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
