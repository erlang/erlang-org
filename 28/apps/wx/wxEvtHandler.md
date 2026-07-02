# `wxEvtHandler`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxEvtHandler.erl#L60)

The Event handler

A class that can handle events from the windowing system. `m:wxWindow` is (and
therefore all window classes are) derived from this class.

To get events from wxwidgets objects you subscribe to them by calling `connect/3`.

If the `callback` option is not supplied events are sent as messages.

These messages will be `#wx{}` where `EventRecord` is a record that depends on
the `wxEventType`. The records are defined in: `wx/include/wx.hrl`.

If a callback was supplied to connect, the callback will be invoked (in another
process) to handle the event. The callback should be of arity 2.

`fun Callback (EventRecord::wx(), EventObject::wxObject()).`

Note: The callback will be in executed in new process each time.

See:
[Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events_processing)

wxWidgets docs:
[wxEvtHandler](https://docs.wxwidgets.org/3.2/classwx_evt_handler.html)

# `event`

```elixir
-type event() ::
          wxActivateEvent:wxActivate() |
          wxAuiManagerEvent:wxAuiManager() |
          wxAuiNotebookEvent:wxAuiNotebook() |
          wxBookCtrlEvent:wxBookCtrl() |
          wxCalendarEvent:wxCalendar() |
          wxChildFocusEvent:wxChildFocus() |
          wxClipboardTextEvent:wxClipboardText() |
          wxCloseEvent:wxClose() |
          wxColourPickerEvent:wxColourPicker() |
          wxCommandEvent:wxCommand() |
          wxContextMenuEvent:wxContextMenu() |
          wxDateEvent:wxDate() |
          wxDisplayChangedEvent:wxDisplayChanged() |
          wxDropFilesEvent:wxDropFiles() |
          wxEraseEvent:wxErase() |
          wxFileDirPickerEvent:wxFileDirPicker() |
          wxFocusEvent:wxFocus() |
          wxFontPickerEvent:wxFontPicker() |
          wxGridEvent:wxGrid() |
          wxHelpEvent:wxHelp() |
          wxHtmlLinkEvent:wxHtmlLink() |
          wxIconizeEvent:wxIconize() |
          wxIdleEvent:wxIdle() |
          wxInitDialogEvent:wxInitDialog() |
          wxJoystickEvent:wxJoystick() |
          wxKeyEvent:wxKey() |
          wxListEvent:wxList() |
          wxMaximizeEvent:wxMaximize() |
          wxMenuEvent:wxMenu() |
          wxMouseCaptureChangedEvent:wxMouseCaptureChanged() |
          wxMouseCaptureLostEvent:wxMouseCaptureLost() |
          wxMouseEvent:wxMouse() |
          wxMoveEvent:wxMove() |
          wxNavigationKeyEvent:wxNavigationKey() |
          wxPaintEvent:wxPaint() |
          wxPaletteChangedEvent:wxPaletteChanged() |
          wxQueryNewPaletteEvent:wxQueryNewPalette() |
          wxSashEvent:wxSash() |
          wxScrollEvent:wxScroll() |
          wxScrollWinEvent:wxScrollWin() |
          wxSetCursorEvent:wxSetCursor() |
          wxShowEvent:wxShow() |
          wxSizeEvent:wxSize() |
          wxSpinEvent:wxSpin() |
          wxSplitterEvent:wxSplitter() |
          wxStyledTextEvent:wxStyledText() |
          wxSysColourChangedEvent:wxSysColourChanged() |
          wxTaskBarIconEvent:wxTaskBarIcon() |
          wxTreeEvent:wxTree() |
          wxUpdateUIEvent:wxUpdateUI() |
          wxWebViewEvent:wxWebView() |
          wxWindowCreateEvent:wxWindowCreate() |
          wxWindowDestroyEvent:wxWindowDestroy().
```

# `wx`

```elixir
-type wx() :: #wx{id :: integer(), obj :: wx:wx_object(), userData :: term(), event :: event()}.
```

# `wxEventType`
*not exported* 

```elixir
-type wxEventType() ::
          wxActivateEvent:wxActivateEventType() |
          wxAuiManagerEvent:wxAuiManagerEventType() |
          wxAuiNotebookEvent:wxAuiNotebookEventType() |
          wxBookCtrlEvent:wxBookCtrlEventType() |
          wxCalendarEvent:wxCalendarEventType() |
          wxChildFocusEvent:wxChildFocusEventType() |
          wxClipboardTextEvent:wxClipboardTextEventType() |
          wxCloseEvent:wxCloseEventType() |
          wxColourPickerEvent:wxColourPickerEventType() |
          wxCommandEvent:wxCommandEventType() |
          wxContextMenuEvent:wxContextMenuEventType() |
          wxDateEvent:wxDateEventType() |
          wxDisplayChangedEvent:wxDisplayChangedEventType() |
          wxDropFilesEvent:wxDropFilesEventType() |
          wxEraseEvent:wxEraseEventType() |
          wxFileDirPickerEvent:wxFileDirPickerEventType() |
          wxFocusEvent:wxFocusEventType() |
          wxFontPickerEvent:wxFontPickerEventType() |
          wxGridEvent:wxGridEventType() |
          wxHelpEvent:wxHelpEventType() |
          wxHtmlLinkEvent:wxHtmlLinkEventType() |
          wxIconizeEvent:wxIconizeEventType() |
          wxIdleEvent:wxIdleEventType() |
          wxInitDialogEvent:wxInitDialogEventType() |
          wxJoystickEvent:wxJoystickEventType() |
          wxKeyEvent:wxKeyEventType() |
          wxListEvent:wxListEventType() |
          wxMaximizeEvent:wxMaximizeEventType() |
          wxMenuEvent:wxMenuEventType() |
          wxMouseCaptureChangedEvent:wxMouseCaptureChangedEventType() |
          wxMouseCaptureLostEvent:wxMouseCaptureLostEventType() |
          wxMouseEvent:wxMouseEventType() |
          wxMoveEvent:wxMoveEventType() |
          wxNavigationKeyEvent:wxNavigationKeyEventType() |
          wxPaintEvent:wxPaintEventType() |
          wxPaletteChangedEvent:wxPaletteChangedEventType() |
          wxQueryNewPaletteEvent:wxQueryNewPaletteEventType() |
          wxSashEvent:wxSashEventType() |
          wxScrollEvent:wxScrollEventType() |
          wxScrollWinEvent:wxScrollWinEventType() |
          wxSetCursorEvent:wxSetCursorEventType() |
          wxShowEvent:wxShowEventType() |
          wxSizeEvent:wxSizeEventType() |
          wxSpinEvent:wxSpinEventType() |
          wxSplitterEvent:wxSplitterEventType() |
          wxStyledTextEvent:wxStyledTextEventType() |
          wxSysColourChangedEvent:wxSysColourChangedEventType() |
          wxTaskBarIconEvent:wxTaskBarIconEventType() |
          wxTreeEvent:wxTreeEventType() |
          wxUpdateUIEvent:wxUpdateUIEventType() |
          wxWebViewEvent:wxWebViewEventType() |
          wxWindowCreateEvent:wxWindowCreateEventType() |
          wxWindowDestroyEvent:wxWindowDestroyEventType().
```

# `wxEvtHandler`

```elixir
-type wxEvtHandler() :: wx:wx_object().
```

# `connect`

```elixir
-spec connect(This :: wxEvtHandler(), EventType :: wxEventType()) -> ok.
```

# `connect`

```elixir
-spec connect(This :: wxEvtHandler(), EventType :: wxEventType(), [Option]) -> ok
                 when
                     Option ::
                         {id, integer()} |
                         {lastId, integer()} |
                         {skip, boolean()} |
                         callback |
                         {callback, function()} |
                         {userData, term()}.
```

This function subscribes to events.

Subscribes to events of type `EventType`, in the range `id`, `lastId`.

The events will be received as messages if no callback is supplied.

Options

id:`{id, integer()} `The identifier (or first of the identifier range) to be
associated with this event handler. Default is ?wxID_ANY

lastid:`{lastId,integer()} `The second part of the identifier range. If used
'id' must be set as the starting identifier range. Default is ?wxID_ANY

skip:`{skip,boolean()} `If skip is true further event_handlers will be called.
This is not used if the 'callback' option is used. Default is `false`.

callback:`{callback,function()} `Use a
callback`fun(EventRecord::wx(),EventObject::wxObject()) `to process the event.
Default not specified i.e. a message will be delivered to the process calling
this function.

userData:`{userData,term()} `An erlang term that will be sent with the event.
Default: [].

# `disconnect`

```elixir
-spec disconnect(This :: wxEvtHandler()) -> boolean().
```

# `disconnect`

```elixir
-spec disconnect(This :: wxEvtHandler(), EventType :: wxEventType()) -> boolean().
```

# `disconnect`

```elixir
-spec disconnect(This :: wxEvtHandler(), EventType :: wxEventType(), [Option]) -> boolean()
                    when Option :: {id, integer()} | {lastId, integer()} | {callback, function()}.
```

This function unsubscribes the process or callback fun from the event handler.

EventType may be the atom 'null' to match any eventtype. Notice that the options
skip and userdata is not used to match the eventhandler.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
