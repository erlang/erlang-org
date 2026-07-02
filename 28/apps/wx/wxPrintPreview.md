# `wxPrintPreview`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPrintPreview.erl#L58)

Objects of this class manage the print preview process.

The object is passed a `m:wxPrintout` object, and the `m:wxPrintPreview` object itself is
passed to a `m:wxPreviewFrame` object. Previewing is started by initializing and showing
the preview frame. Unlike `wxPrinter:print/4`, flow of control returns to the application immediately after
the frame is shown.

Note: The preview shown is only exact on Windows. On other platforms, the `m:wxDC` used
for preview is different from what is used for printing and the results may be
significantly different, depending on how is the output created. In particular, printing
code relying on `wxDC:getTextExtent/3` heavily (for example, `m:wxHtmlEasyPrinting` and other wxHTML classes do)
is affected. It is recommended to use native preview functionality on platforms that offer
it (macOS, GTK+).

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPrintout`

* `m:wxPrinter`

* `m:wxPreviewCanvas`

* `m:wxPreviewControlBar`

* `m:wxPreviewFrame`

wxWidgets docs: [wxPrintPreview](https://docs.wxwidgets.org/3.2/classwx_print_preview.html)

# `wxPrintPreview`

```elixir
-type wxPrintPreview() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPrintPreview()) -> ok.
```

Destroys the object

# `getCanvas`

```elixir
-spec getCanvas(This) -> wxPreviewCanvas:wxPreviewCanvas() when This :: wxPrintPreview().
```

Gets the preview window used for displaying the print preview image.

# `getCurrentPage`

```elixir
-spec getCurrentPage(This) -> integer() when This :: wxPrintPreview().
```

Gets the page currently being previewed.

# `getFrame`

```elixir
-spec getFrame(This) -> wxFrame:wxFrame() when This :: wxPrintPreview().
```

Gets the frame used for displaying the print preview canvas and control bar.

# `getMaxPage`

```elixir
-spec getMaxPage(This) -> integer() when This :: wxPrintPreview().
```

Returns the maximum page number.

# `getMinPage`

```elixir
-spec getMinPage(This) -> integer() when This :: wxPrintPreview().
```

Returns the minimum page number.

# `getPrintout`

```elixir
-spec getPrintout(This) -> wxPrintout:wxPrintout() when This :: wxPrintPreview().
```

Gets the preview printout object associated with the `m:wxPrintPreview` object.

# `getPrintoutForPrinting`

```elixir
-spec getPrintoutForPrinting(This) -> wxPrintout:wxPrintout() when This :: wxPrintPreview().
```

Gets the printout object to be used for printing from within the preview interface, or
NULL if none exists.

# `isOk`

```elixir
-spec isOk(This) -> boolean() when This :: wxPrintPreview().
```

Returns true if the `m:wxPrintPreview` is valid, false otherwise.

It could return false if there was a problem initializing the printer device context
(current printer not set, for example).

# `new`

```elixir
-spec new(Printout) -> wxPrintPreview() when Printout :: wxPrintout:wxPrintout().
```

# `new`

```elixir
-spec new(Printout, [Option]) -> wxPrintPreview()
             when
                 Printout :: wxPrintout:wxPrintout(),
                 Option ::
                     {printoutForPrinting, wxPrintout:wxPrintout()} |
                     {data, wxPrintDialogData:wxPrintDialogData()}.
```

Constructor.

Pass a printout object, an optional printout object to be used for actual printing, and
the address of an optional block of printer data, which will be copied to the print
preview object's print data.

If `printoutForPrinting` is non-NULL, a `"Print..."` button will be placed on the preview
frame so that the user can print directly from the preview interface.

Remark: Do not explicitly delete the printout objects once this constructor has been
called, since they will be deleted in the `m:wxPrintPreview` destructor. The same does not
apply to the `data` argument.

Use `isOk/1` to check whether the `m:wxPrintPreview` object was created correctly.

# `new`

```elixir
-spec new(Printout, PrintoutForPrinting, Data) -> wxPrintPreview()
             when
                 Printout :: wxPrintout:wxPrintout(),
                 PrintoutForPrinting :: wxPrintout:wxPrintout(),
                 Data :: wxPrintData:wxPrintData().
```

# `paintPage`

```elixir
-spec paintPage(This, Canvas, Dc) -> boolean()
                   when
                       This :: wxPrintPreview(),
                       Canvas :: wxPreviewCanvas:wxPreviewCanvas(),
                       Dc :: wxDC:wxDC().
```

This refreshes the preview window with the preview image.

It must be called from the preview window's OnPaint member.

The implementation simply blits the preview bitmap onto the canvas, creating a new
preview bitmap if none exists.

# `print`

```elixir
-spec print(This, Prompt) -> boolean() when This :: wxPrintPreview(), Prompt :: boolean().
```

Invokes the print process using the second `m:wxPrintout` object supplied in the `m:wxPrintPreview`
constructor.

Will normally be called by the `Print`... panel item on the preview frame's control bar.

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed information about the kind of the
error.

# `renderPage`

```elixir
-spec renderPage(This, PageNum) -> boolean() when This :: wxPrintPreview(), PageNum :: integer().
```

Renders a page into a `m:wxMemoryDC`.

Used internally by `m:wxPrintPreview`.

# `setCanvas`

```elixir
-spec setCanvas(This, Window) -> ok
                   when This :: wxPrintPreview(), Window :: wxPreviewCanvas:wxPreviewCanvas().
```

Sets the window to be used for displaying the print preview image.

# `setCurrentPage`

```elixir
-spec setCurrentPage(This, PageNum) -> boolean() when This :: wxPrintPreview(), PageNum :: integer().
```

Sets the current page to be previewed.

# `setFrame`

```elixir
-spec setFrame(This, Frame) -> ok when This :: wxPrintPreview(), Frame :: wxFrame:wxFrame().
```

Sets the frame to be used for displaying the print preview canvas and control bar.

# `setPrintout`

```elixir
-spec setPrintout(This, Printout) -> ok
                     when This :: wxPrintPreview(), Printout :: wxPrintout:wxPrintout().
```

Associates a printout object with the `m:wxPrintPreview` object.

# `setZoom`

```elixir
-spec setZoom(This, Percent) -> ok when This :: wxPrintPreview(), Percent :: integer().
```

Sets the percentage preview zoom, and refreshes the preview canvas accordingly.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
