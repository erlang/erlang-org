# `wxPrintDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPrintDialog.erl#L58)

This class represents the print and print setup common dialogs.

You may obtain a `wxPrinterDC` (not implemented in wx) device context from a successfully
dismissed print dialog.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_print)

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPrintDialog](https://docs.wxwidgets.org/3.2/classwx_print_dialog.html)

# `wxPrintDialog`

```erlang
-type wxPrintDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPrintDialog()) -> ok.
```

Destroys the object

# `getPrintDC`

```erlang
-spec getPrintDC(This) -> wxDC:wxDC() when This :: wxPrintDialog().
```

Returns the device context created by the print dialog, if any.

When this function has been called, the ownership of the device context is transferred to
the application, so it must then be deleted explicitly.

# `getPrintDialogData`

```erlang
-spec getPrintDialogData(This) -> wxPrintDialogData:wxPrintDialogData() when This :: wxPrintDialog().
```

Returns the print dialog data associated with the print dialog.

# `new`

```erlang
-spec new(Parent) -> wxPrintDialog() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxPrintDialog()
             when Parent :: wxWindow:wxWindow(), Option :: {data, wxPrintDialogData:wxPrintDialogData()};
         (Parent, Data) -> wxPrintDialog()
             when Parent :: wxWindow:wxWindow(), Data :: wxPrintData:wxPrintData().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
