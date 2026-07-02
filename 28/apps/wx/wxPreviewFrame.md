# `wxPreviewFrame`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPreviewFrame.erl#L58)

This class provides the default method of managing the print preview interface.

Member functions may be overridden to replace functionality, or the class may be used
without derivation.

See:
* `m:wxPreviewCanvas`

* `m:wxPreviewControlBar`

* `m:wxPrintPreview`

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPreviewFrame](https://docs.wxwidgets.org/3.2/classwx_preview_frame.html)

# `wxPreviewFrame`

```elixir
-type wxPreviewFrame() :: wx:wx_object().
```

# `createCanvas`

```elixir
-spec createCanvas(This) -> ok when This :: wxPreviewFrame().
```

Creates a `m:wxPreviewCanvas`.

Override this function to allow a user-defined preview canvas object to be created.

# `createControlBar`

```elixir
-spec createControlBar(This) -> ok when This :: wxPreviewFrame().
```

Creates a `m:wxPreviewControlBar`.

Override this function to allow a user-defined preview control bar object to be created.

# `destroy`

```elixir
-spec destroy(This :: wxPreviewFrame()) -> ok.
```

Destroys the object

# `initialize`

```elixir
-spec initialize(This) -> ok when This :: wxPreviewFrame().
```

Initializes the frame elements and prepares for showing it.

Calling this method is equivalent to calling `InitializeWithModality()` (not implemented
in wx) with wxPreviewFrame_AppModal argument, please see its documentation for more details.

Please notice that this function is virtual mostly for backwards compatibility only,
there is no real need to override it as it's never called by wxWidgets itself.

# `new`

```elixir
-spec new(Preview, Parent) -> wxPreviewFrame()
             when Preview :: wxPrintPreview:wxPrintPreview(), Parent :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Preview, Parent, [Option]) -> wxPreviewFrame()
             when
                 Preview :: wxPrintPreview:wxPrintPreview(),
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {title, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

Pass a print preview object plus other normal frame arguments. The print preview object
will be destroyed by the frame when it closes.

# `onCloseWindow`

```elixir
-spec onCloseWindow(This, Event) -> ok
                       when This :: wxPreviewFrame(), Event :: wxCloseEvent:wxCloseEvent().
```

Enables any disabled frames in the application, and deletes the print preview object,
implicitly deleting any printout objects associated with the print preview object.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
