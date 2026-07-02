# `wxHtmlEasyPrinting`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxHtmlEasyPrinting.erl#L58)

This class provides very simple interface to printing architecture.

It allows you to print HTML documents using only a few commands.

Note: Do not create this class on the stack only. You should create an instance on app
startup and use this instance for all printing operations. The reason is that this class
stores various settings in it.

wxWidgets docs: [wxHtmlEasyPrinting](https://docs.wxwidgets.org/3.2/classwx_html_easy_printing.html)

# `wxHtmlEasyPrinting`

```erlang
-type wxHtmlEasyPrinting() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxHtmlEasyPrinting()) -> ok.
```

Destroys the object

# `getPageSetupData`

```erlang
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData()
                          when This :: wxHtmlEasyPrinting().
```

Returns a pointer to `m:wxPageSetupDialogData` instance used by this class.

You can set its parameters (via SetXXXX methods).

# `getPrintData`

```erlang
-spec getPrintData(This) -> wxPrintData:wxPrintData() when This :: wxHtmlEasyPrinting().
```

Returns pointer to `m:wxPrintData` instance used by this class.

You can set its parameters (via SetXXXX methods).

# `new`

```erlang
-spec new() -> wxHtmlEasyPrinting().
```

# `new`

```erlang
-spec new([Option]) -> wxHtmlEasyPrinting()
             when Option :: {name, unicode:chardata()} | {parentWindow, wxWindow:wxWindow()}.
```

Constructor.

# `pageSetup`

```erlang
-spec pageSetup(This) -> ok when This :: wxHtmlEasyPrinting().
```

Display page setup dialog and allows the user to modify settings.

# `previewFile`

```erlang
-spec previewFile(This, Htmlfile) -> boolean()
                     when This :: wxHtmlEasyPrinting(), Htmlfile :: unicode:chardata().
```

Preview HTML file.

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed information about the kind of the
error.

# `previewText`

```erlang
-spec previewText(This, Htmltext) -> boolean()
                     when This :: wxHtmlEasyPrinting(), Htmltext :: unicode:chardata().
```

# `previewText`

```erlang
-spec previewText(This, Htmltext, [Option]) -> boolean()
                     when
                         This :: wxHtmlEasyPrinting(),
                         Htmltext :: unicode:chardata(),
                         Option :: {basepath, unicode:chardata()}.
```

Preview HTML text (not file!).

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed information about the kind of the error.

# `printFile`

```erlang
-spec printFile(This, Htmlfile) -> boolean()
                   when This :: wxHtmlEasyPrinting(), Htmlfile :: unicode:chardata().
```

Print HTML file.

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed information about the kind of the
error.

# `printText`

```erlang
-spec printText(This, Htmltext) -> boolean()
                   when This :: wxHtmlEasyPrinting(), Htmltext :: unicode:chardata().
```

# `printText`

```erlang
-spec printText(This, Htmltext, [Option]) -> boolean()
                   when
                       This :: wxHtmlEasyPrinting(),
                       Htmltext :: unicode:chardata(),
                       Option :: {basepath, unicode:chardata()}.
```

Print HTML text (not file!).

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed information about the kind of the error.

# `setFonts`

```erlang
-spec setFonts(This, Normal_face, Fixed_face) -> ok
                  when
                      This :: wxHtmlEasyPrinting(),
                      Normal_face :: unicode:chardata(),
                      Fixed_face :: unicode:chardata().
```

# `setFonts`

```erlang
-spec setFonts(This, Normal_face, Fixed_face, [Option]) -> ok
                  when
                      This :: wxHtmlEasyPrinting(),
                      Normal_face :: unicode:chardata(),
                      Fixed_face :: unicode:chardata(),
                      Option :: {sizes, [integer()]}.
```

Sets fonts.

See `wxHtmlDCRenderer::SetFonts` (not implemented in wx) for detailed description.

# `setFooter`

```erlang
-spec setFooter(This, Footer) -> ok when This :: wxHtmlEasyPrinting(), Footer :: unicode:chardata().
```

# `setFooter`

```erlang
-spec setFooter(This, Footer, [Option]) -> ok
                   when
                       This :: wxHtmlEasyPrinting(),
                       Footer :: unicode:chardata(),
                       Option :: {pg, integer()}.
```

Set page footer.

The following macros can be used inside it: @DATE@ is replaced by the current date in
default format @PAGENUM@ is replaced by page number @PAGESCNT@ is replaced by total number
of pages @TIME@ is replaced by the current time in default format @TITLE@ is replaced with
the title of the document

# `setHeader`

```erlang
-spec setHeader(This, Header) -> ok when This :: wxHtmlEasyPrinting(), Header :: unicode:chardata().
```

# `setHeader`

```erlang
-spec setHeader(This, Header, [Option]) -> ok
                   when
                       This :: wxHtmlEasyPrinting(),
                       Header :: unicode:chardata(),
                       Option :: {pg, integer()}.
```

Set page header.

The following macros can be used inside it:

* @DATE@ is replaced by the current date in default format

* @PAGENUM@ is replaced by page number

* @PAGESCNT@ is replaced by total number of pages

* @TIME@ is replaced by the current time in default format

* @TITLE@ is replaced with the title of the document

---

*Consult [api-reference.md](api-reference.md) for complete listing*
