# `wxPageSetupDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPageSetupDialog.erl#L58)

This class represents the page setup common dialog.

The page setup dialog contains controls for paper size (letter, A4, A5 etc.), orientation
(landscape or portrait), and, only under Windows currently, controls for setting left,
top, right and bottom margin sizes in millimetres.

The exact appearance of this dialog varies among the platforms as a native dialog is used
when available (currently the case for all major platforms).

When the dialog has been closed, you need to query the `m:wxPageSetupDialogData` object
associated with the dialog.

Note that the OK and Cancel buttons do not destroy the dialog; this must be done by the application.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPageSetupDialogData`

wxWidgets docs: [wxPageSetupDialog](https://docs.wxwidgets.org/3.2/classwx_page_setup_dialog.html)

# `wxPageSetupDialog`

```erlang
-type wxPageSetupDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPageSetupDialog()) -> ok.
```

Destroys the object

# `getPageSetupData`

```erlang
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData()
                          when This :: wxPageSetupDialog().
```

Returns the `m:wxPageSetupDialogData` object associated with the dialog.

# `new`

```erlang
-spec new(Parent) -> wxPageSetupDialog() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxPageSetupDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option :: {data, wxPageSetupDialogData:wxPageSetupDialogData()}.
```

Constructor.

Pass a parent window, and optionally a pointer to a block of page setup data, which will
be copied to the print dialog's internal data.

# `showModal`

```erlang
-spec showModal(This) -> integer() when This :: wxPageSetupDialog().
```

Shows the dialog, returning `wxID\_OK` if the user pressed OK, and `wxID\_CANCEL`
otherwise.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
