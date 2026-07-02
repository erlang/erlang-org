# `wxPrintout`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPrintout.erl#L58)

This class encapsulates the functionality of printing out an application document.

A new class must be derived and members overridden to respond to calls such as `OnPrintPage()`
(not implemented in wx) and `HasPage()` (not implemented in wx) and to render the print
image onto an associated `m:wxDC`. Instances of this class are passed to `wxPrinter:print/4` or to a `m:wxPrintPreview`
object to initiate printing or previewing.

Your derived `m:wxPrintout` is responsible for drawing both the preview image and the
printed page. If your windows' drawing routines accept an arbitrary DC as an argument, you
can re-use those routines within your `m:wxPrintout` subclass to draw the printout image.
You may also add additional drawing elements within your `m:wxPrintout` subclass, like
headers, footers, and/or page numbers. However, the image on the printed page will often
differ from the image drawn on the screen, as will the print preview image - not just in
the presence of headers and footers, but typically in scale. A high-resolution printer
presents a much larger drawing surface (i.e., a higher-resolution DC); a zoomed-out
preview image presents a much smaller drawing surface (lower-resolution DC). By using the
routines FitThisSizeToXXX() and/or MapScreenSizeToXXX() within your `m:wxPrintout`
subclass to set the user scale and origin of the associated DC, you can easily use a
single drawing routine to draw on your application's windows, to create the print preview
image, and to create the printed paper image, and achieve a common appearance to the
preview image and the printed page.

See:
* [Overview printing](https://docs.wxwidgets.org/3.2/overview_printing.html#overview_printing)

* `m:wxPrintDialog`

* `m:wxPageSetupDialog`

* `m:wxPrinter`

* `m:wxPrintPreview`

wxWidgets docs: [wxPrintout](https://docs.wxwidgets.org/3.2/classwx_printout.html)

# `wxPrintout`

```elixir
-type wxPrintout() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPrintout()) -> ok.
```

Destroys the object

# `fitThisSizeToPage`

```elixir
-spec fitThisSizeToPage(This, ImageSize) -> ok
                           when This :: wxPrintout(), ImageSize :: {W :: integer(), H :: integer()}.
```

Set the user scale and device origin of the `m:wxDC` associated with this `m:wxPrintout`
so that the given image size fits entirely within the page rectangle and the origin is at
the top left corner of the page rectangle.

On MSW and Mac, the page rectangle is the printable area of the page. On other platforms
and PostScript printing, the page rectangle is the entire paper.

Use this if you want your printed image as large as possible, but with the caveat that on
some platforms, portions of the image might be cut off at the edges.

# `fitThisSizeToPageMargins`

```elixir
-spec fitThisSizeToPageMargins(This, ImageSize, PageSetupData) -> ok
                                  when
                                      This :: wxPrintout(),
                                      ImageSize :: {W :: integer(), H :: integer()},
                                      PageSetupData :: wxPageSetupDialogData:wxPageSetupDialogData().
```

Set the user scale and device origin of the `m:wxDC` associated with this `m:wxPrintout`
so that the given image size fits entirely within the page margins set in the given `m:wxPageSetupDialogData`
object.

This function provides the greatest consistency across all platforms because it does not
depend on having access to the printable area of the paper.

Remark: On Mac, the native `m:wxPageSetupDialog` does not let you set the page margins;
you'll have to provide your own mechanism, or you can use the Mac-only class
wxMacPageMarginsDialog.

# `fitThisSizeToPaper`

```elixir
-spec fitThisSizeToPaper(This, ImageSize) -> ok
                            when This :: wxPrintout(), ImageSize :: {W :: integer(), H :: integer()}.
```

Set the user scale and device origin of the `m:wxDC` associated with this `m:wxPrintout`
so that the given image size fits entirely within the paper and the origin is at the top
left corner of the paper.

Use this if you're managing your own page margins.

Note: With most printers, the region around the edges of the paper are not printable so
that the edges of the image could be cut off.

# `getDC`

```elixir
-spec getDC(This) -> wxDC:wxDC() when This :: wxPrintout().
```

Returns the device context associated with the printout (given to the printout at start
of printing or previewing).

The application can use `getDC/1` to obtain a device context to draw on.

This will be a `wxPrinterDC` (not implemented in wx) if printing under Windows or Mac, a `m:wxPostScriptDC`
if printing on other platforms, and a `m:wxMemoryDC` if previewing.

# `getLogicalPageMarginsRect`

```elixir
-spec getLogicalPageMarginsRect(This, PageSetupData) ->
                                   {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                                   when
                                       This :: wxPrintout(),
                                       PageSetupData :: wxPageSetupDialogData:wxPageSetupDialogData().
```

Return the rectangle corresponding to the page margins specified by the given `m:wxPageSetupDialogData`
object in the associated `m:wxDC`'s logical coordinates for the current user scale and
device origin.

The page margins are specified with respect to the edges of the paper on all platforms.

# `getLogicalPageRect`

```elixir
-spec getLogicalPageRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                            when This :: wxPrintout().
```

Return the rectangle corresponding to the page in the associated `m:wxDC` 's logical
coordinates for the current user scale and device origin.

On MSW and Mac, this will be the printable area of the paper. On other platforms and
PostScript printing, this will be the full paper rectangle.

# `getLogicalPaperRect`

```elixir
-spec getLogicalPaperRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                             when This :: wxPrintout().
```

Return the rectangle corresponding to the paper in the associated `m:wxDC` 's logical
coordinates for the current user scale and device origin.

# `getPageSizeMM`

```elixir
-spec getPageSizeMM(This) -> {W :: integer(), H :: integer()} when This :: wxPrintout().
```

Returns the size of the printer page in millimetres.

# `getPageSizePixels`

```elixir
-spec getPageSizePixels(This) -> {W :: integer(), H :: integer()} when This :: wxPrintout().
```

Returns the size of the printer page in pixels, called the page rectangle.

The page rectangle has a top left corner at (0,0) and a bottom right corner at (w,h).
These values may not be the same as the values returned from `wxDC:getSize/1`;if the printout is being
used for previewing, a memory device context is used, which uses a bitmap size reflecting
the current preview zoom. The application must take this discrepancy into account if
previewing is to be supported.

# `getPaperRectPixels`

```elixir
-spec getPaperRectPixels(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                            when This :: wxPrintout().
```

Returns the rectangle that corresponds to the entire paper in pixels, called the paper
rectangle.

This distinction between paper rectangle and page rectangle reflects the fact that most
printers cannot print all the way to the edge of the paper. The page rectangle is a
rectangle whose top left corner is at (0,0) and whose width and height are given by wxDC::GetPageSizePixels().

On MSW and Mac, the page rectangle gives the printable area of the paper, while the paper
rectangle represents the entire paper, including non-printable borders. Thus, the
rectangle returned by wxDC::GetPaperRectPixels() will have a top left corner whose
coordinates are small negative numbers and the bottom right corner will have values
somewhat larger than the width and height given by wxDC::GetPageSizePixels().

On other platforms and for PostScript printing, the paper is treated as if its entire
area were printable, so this function will return the same rectangle as the page
rectangle.

# `getPPIPrinter`

```elixir
-spec getPPIPrinter(This) -> {W :: integer(), H :: integer()} when This :: wxPrintout().
```

Returns the number of pixels per logical inch of the printer device context.

Dividing the printer PPI by the screen PPI can give a suitable scaling factor for drawing
text onto the printer.

Remember to multiply this by a scaling factor to take the preview DC size into account.
Or you can just use the FitThisSizeToXXX() and MapScreenSizeToXXX routines below, which do
most of the scaling calculations for you.

# `getPPIScreen`

```elixir
-spec getPPIScreen(This) -> {W :: integer(), H :: integer()} when This :: wxPrintout().
```

Returns the number of pixels per logical inch of the screen device context.

Dividing the printer PPI by the screen PPI can give a suitable scaling factor for drawing
text onto the printer.

If you are doing your own scaling, remember to multiply this by a scaling factor to take
the preview DC size into account.

# `getTitle`

```elixir
-spec getTitle(This) -> unicode:charlist() when This :: wxPrintout().
```

Returns the title of the printout.

# `isPreview`

```elixir
-spec isPreview(This) -> boolean() when This :: wxPrintout().
```

Returns true if the printout is currently being used for previewing.

# `mapScreenSizeToDevice`

```elixir
-spec mapScreenSizeToDevice(This) -> ok when This :: wxPrintout().
```

Set the user scale and device origin of the `m:wxDC` associated with this `m:wxPrintout`
so that one screen pixel maps to one device pixel on the DC.

That is, the user scale is set to (1,1) and the device origin is set to (0,0).

Use this if you want to do your own scaling prior to calling `m:wxDC` drawing calls, for
example, if your underlying model is floating-point and you want to achieve maximum
drawing precision on high-resolution printers.

You can use the GetLogicalXXXRect() routines below to obtain the paper rectangle, page
rectangle, or page margins rectangle to perform your own scaling.

Note: While the underlying drawing model of macOS is floating-point, wxWidgets's drawing
model scales from integer coordinates.

# `mapScreenSizeToPage`

```elixir
-spec mapScreenSizeToPage(This) -> ok when This :: wxPrintout().
```

This sets the user scale of the `m:wxDC` associated with this `m:wxPrintout` to the same
scale as `mapScreenSizeToPaper/1` but sets the logical origin to the top left corner of
the page rectangle.

# `mapScreenSizeToPageMargins`

```elixir
-spec mapScreenSizeToPageMargins(This, PageSetupData) -> ok
                                    when
                                        This :: wxPrintout(),
                                        PageSetupData :: wxPageSetupDialogData:wxPageSetupDialogData().
```

This sets the user scale of the `m:wxDC` associated with this `m:wxPrintout` to the same
scale as `mapScreenSizeToPageMargins/2` but sets the logical origin to the top left corner
of the page margins specified by the given `m:wxPageSetupDialogData` object.

# `mapScreenSizeToPaper`

```elixir
-spec mapScreenSizeToPaper(This) -> ok when This :: wxPrintout().
```

Set the user scale and device origin of the `m:wxDC` associated with this `m:wxPrintout`
so that the printed page matches the screen size as closely as possible and the logical
origin is in the top left corner of the paper rectangle.

That is, a 100-pixel object on screen should appear at the same size on the printed page.
(It will, of course, be larger or smaller in the preview image, depending on the zoom factor.)

Use this if you want WYSIWYG behaviour, e.g., in a text editor.

# `new`

```elixir
-spec new(Title :: string(), OnPrintPage :: function()) -> wxPrintout:wxPrintout().
```

# `new`

```elixir
-spec new(Title :: string(), OnPrintPage, [Option]) -> wxPrintout:wxPrintout()
             when
                 OnPrintPage :: fun((wxPrintout(), Page :: integer()) -> boolean()),
                 Option ::
                     {onPreparePrinting, fun((wxPrintout()) -> ok)} |
                     {onBeginPrinting, fun((wxPrintout()) -> ok)} |
                     {onEndPrinting, fun((wxPrintout()) -> ok)} |
                     {onBeginDocument,
                      fun((wxPrintout(), StartPage :: integer(), EndPage :: integer()) -> boolean())} |
                     {onEndDocument, fun((wxPrintout()) -> ok)} |
                     {hasPage, fun((wxPrintout(), Page :: integer()) -> ok)} |
                     {getPageInfo,
                      fun((wxPrintout()) ->
                              {MinPage :: integer(),
                               MaxPage :: integer(),
                               PageFrom :: integer(),
                               PageTo :: integer()})}.
```

Constructor.

Creates a `m:wxPrintout` object with a callback fun and optionally other
callback funs. The `This` argument is the `m:wxPrintout` object reference to
this object

Notice: The callbacks may not call other processes.

# `offsetLogicalOrigin`

```elixir
-spec offsetLogicalOrigin(This, Xoff, Yoff) -> ok
                             when This :: wxPrintout(), Xoff :: integer(), Yoff :: integer().
```

Shift the device origin by an amount specified in logical coordinates.

# `setLogicalOrigin`

```elixir
-spec setLogicalOrigin(This, X, Y) -> ok when This :: wxPrintout(), X :: integer(), Y :: integer().
```

Set the device origin of the associated `m:wxDC` so that the current logical point
becomes the new logical origin.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
