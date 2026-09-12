# `wxPreviewControlBar`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPreviewControlBar.erl#L58)

This is the default implementation of the preview control bar, a panel with buttons and a
zoom control.

You can derive a new class from this and override some or all member functions to change
the behaviour and appearance; or you can leave it as it is.

See:
* `m:wxPreviewFrame`

* `m:wxPreviewCanvas`

* `m:wxPrintPreview`

This class is derived, and can use functions, from:

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPreviewControlBar](https://docs.wxwidgets.org/3.2/classwx_preview_control_bar.html)

# `wxPreviewControlBar`

```erlang
-type wxPreviewControlBar() :: wx:wx_object().
```

# `createButtons`

```erlang
-spec createButtons(This) -> ok when This :: wxPreviewControlBar().
```

Creates buttons, according to value of the button style flags.

# `destroy`

```erlang
-spec destroy(This :: wxPreviewControlBar()) -> ok.
```

Destroys the object

# `getPrintPreview`

```erlang
-spec getPrintPreview(This) -> wxPrintPreview:wxPrintPreview() when This :: wxPreviewControlBar().
```

Gets the print preview object associated with the control bar.

# `getZoomControl`

```erlang
-spec getZoomControl(This) -> integer() when This :: wxPreviewControlBar().
```

Gets the current zoom setting in percent.

# `new`

```erlang
-spec new(Preview, Buttons, Parent) -> wxPreviewControlBar()
             when
                 Preview :: wxPrintPreview:wxPrintPreview(),
                 Buttons :: integer(),
                 Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Preview, Buttons, Parent, [Option]) -> wxPreviewControlBar()
             when
                 Preview :: wxPrintPreview:wxPrintPreview(),
                 Buttons :: integer(),
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

The `buttons` parameter may be a combination of the following, using the bitwise 'or' operator:

* wxPREVIEW_PRINT: Create a print button.

* wxPREVIEW_NEXT: Create a next page button.

* wxPREVIEW_PREVIOUS: Create a previous page button.

* wxPREVIEW_ZOOM: Create a zoom control.

* wxPREVIEW_DEFAULT: Equivalent to a combination of `wxPREVIEW_PREVIOUS`, `wxPREVIEW_NEXT`
and `wxPREVIEW_ZOOM`.

# `setZoomControl`

```erlang
-spec setZoomControl(This, Percent) -> ok when This :: wxPreviewControlBar(), Percent :: integer().
```

Sets the zoom control.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
