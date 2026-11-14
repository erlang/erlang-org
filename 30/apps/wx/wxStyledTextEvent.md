# `wxStyledTextEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStyledTextEvent.erl#L58)

The type of events sent from `m:wxStyledTextCtrl`.

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxStyledTextEvent](https://docs.wxwidgets.org/3.2/classwx_styled_text_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxStyledTextEventType` to subscribe to events of this type.

# `wxStyledText`

```erlang
-type wxStyledText() ::
          #wxStyledText{type :: wxStyledTextEvent:wxStyledTextEventType(),
                        position :: integer(),
                        key :: integer(),
                        modifiers :: integer(),
                        modificationType :: integer(),
                        text :: unicode:chardata(),
                        length :: integer(),
                        linesAdded :: integer(),
                        line :: integer(),
                        foldLevelNow :: integer(),
                        foldLevelPrev :: integer(),
                        margin :: integer(),
                        message :: integer(),
                        wParam :: integer(),
                        lParam :: integer(),
                        listType :: integer(),
                        x :: integer(),
                        y :: integer(),
                        dragText :: unicode:chardata(),
                        dragAllowMove :: boolean(),
                        dragResult :: wx:wx_enum()}.
```

# `wxStyledTextEvent`

```erlang
-type wxStyledTextEvent() :: wx:wx_object().
```

# `wxStyledTextEventType`

```erlang
-type wxStyledTextEventType() ::
          stc_autocomp_cancelled | stc_autocomp_char_deleted | stc_autocomp_selection |
          stc_calltip_click | stc_change | stc_charadded | stc_do_drop | stc_doubleclick |
          stc_drag_over | stc_dwellend | stc_dwellstart | stc_hotspot_click | stc_hotspot_dclick |
          stc_hotspot_release_click | stc_indicator_click | stc_indicator_release | stc_macrorecord |
          stc_marginclick | stc_modified | stc_needshown | stc_painted | stc_romodifyattempt |
          stc_savepointleft | stc_savepointreached | stc_start_drag | stc_styleneeded | stc_updateui |
          stc_userlistselection | stc_zoom.
```

# `getAlt`

```erlang
-spec getAlt(This) -> boolean() when This :: wxStyledTextEvent().
```

Returns true if the Alt key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`

# `getControl`

```erlang
-spec getControl(This) -> boolean() when This :: wxStyledTextEvent().
```

Returns true if the Control key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`

# `getDragAllowMove`

```erlang
-spec getDragAllowMove(This) -> boolean() when This :: wxStyledTextEvent().
```

# `getDragResult`

```erlang
-spec getDragResult(This) -> wx:wx_enum() when This :: wxStyledTextEvent().
```

Returns drag result for this event.

This method is valid for `wxEVT_STC_DRAG_OVER` and `wxEVT_STC_DO_DROP` events.

# `getDragText`

```erlang
-spec getDragText(This) -> unicode:charlist() when This :: wxStyledTextEvent().
```

Deprecated:

Use `wxCommandEvent:getString/1` instead.

# `getFoldLevelNow`

```erlang
-spec getFoldLevelNow(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the current fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_CHANGEFOLD.

# `getFoldLevelPrev`

```erlang
-spec getFoldLevelPrev(This) -> integer() when This :: wxStyledTextEvent().
```

Returns previous fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_CHANGEFOLD.

# `getKey`

```erlang
-spec getKey(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the key code of the key that generated this event.

This method is valid for the following event types:

* `wxEVT_STC_CHARADDED`

* `wxEVT_STC_USERLISTSELECTION`

* `wxEVT_STC_AUTOCOMP_SELECTION`

* `wxEVT_STC_AUTOCOMP_COMPLETED`

# `getLength`

```erlang
-spec getLength(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the length (number of characters) of this event.

This method is valid for `wxEVT_STC_MODIFIED` and `wxEVT_STC_NEEDSHOWN` events.

# `getLine`

```erlang
-spec getLine(This) -> integer() when This :: wxStyledTextEvent().
```

Returns zero-based line number for this event.

This method is valid for `wxEVT_STC_DOUBLECLICK` and `wxEVT_STC_MODIFIED` events.

# `getLinesAdded`

```erlang
-spec getLinesAdded(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the number of lines added or deleted with this event.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_INSERTTEXT or ?wxSTC\_MOD\_DELETETEXT.

# `getListType`

```erlang
-spec getListType(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the list type for this event.

The list type is an integer passed to a list when it is created with the `wxStyledTextCtrl:userListShow/3` method and can
be used to distinguish lists if more than one is used.

This method is valid for `wxEVT_STC_AUTOCOMP_SELECTION_CHANGE` and `wxEVT_STC_USERLISTSELECTION`
events.

# `getLParam`

```erlang
-spec getLParam(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the value of the LParam field for this event.

This method is valid for `wxEVT_STC_MACRORECORD` events.

# `getMargin`

```erlang
-spec getMargin(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the zero-based index of the margin that generated this event.

This method is valid for `wxEVT_STC_MARGINCLICK` and `wxEVT_STC_MARGIN_RIGHT_CLICK`
events.

# `getMessage`

```erlang
-spec getMessage(This) -> integer() when This :: wxStyledTextEvent().
```

Returns a message number while a macro is being recorded.

Many of the `m:wxStyledTextCtrl` methods such as `wxStyledTextCtrl:insertText/3` and `wxStyledTextCtrl:paste/1` have an event number associated
with them. This method returns that number while a macro is being recorded so that the
macro can be played back later.

This method is valid for `wxEVT_STC_MACRORECORD` events.

# `getModificationType`

```erlang
-spec getModificationType(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the modification type for this event.

The modification type is a bit list that describes the change that generated this event.
It may contain one or more of the following values:

* ?wxSTC\_MOD\_INSERTTEXT

* ?wxSTC\_MOD\_DELETETEXT

* ?wxSTC\_MOD\_CHANGESTYLE

* ?wxSTC\_MOD\_CHANGEFOLD

* ?wxSTC\_PERFORMED\_USER

* ?wxSTC\_PERFORMED\_UNDO

* ?wxSTC\_PERFORMED\_REDO

* ?wxSTC\_MULTISTEPUNDOREDO

* ?wxSTC\_LASTSTEPINUNDOREDO

* ?wxSTC\_MOD\_CHANGEMARKER

* ?wxSTC\_MOD\_BEFOREINSERT

* ?wxSTC\_MOD\_BEFOREDELETE

* ?wxSTC\_MULTILINEUNDOREDO

* ?wxSTC\_STARTACTION

* ?wxSTC\_MOD\_CHANGEINDICATOR

* ?wxSTC\_MOD\_CHANGELINESTATE

* ?wxSTC\_MOD\_CHANGEMARGIN

* ?wxSTC\_MOD\_CHANGEANNOTATION

* ?wxSTC\_MOD\_CONTAINER

* ?wxSTC\_MOD\_LEXERSTATE

* ?wxSTC\_MOD\_INSERTCHECK

* ?wxSTC\_MOD\_CHANGETABSTOPS

This method is valid for `wxEVT_STC_MODIFIED` events.

# `getModifiers`

```erlang
-spec getModifiers(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the modifiers of the key press or mouse click for this event.

The returned value is a bit list that may contain one or more of the following values:

* ?wxSTC\_KEYMOD\_SHIFT

* ?wxSTC\_KEYMOD\_CTRL

* ?wxSTC\_KEYMOD\_ALT

* ?wxSTC\_KEYMOD\_SUPER

* ?wxSTC\_KEYMOD\_META

In addition, the value can be checked for equality with ?wxSTC\_KEYMOD\_NORM to test if
no modifiers are present.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`

# `getPosition`

```erlang
-spec getPosition(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the zero-based text position associated this event.

This method is valid for the following event types:

* `wxEVT_STC_STYLENEEDED`

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MODIFIED`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_NEEDSHOWN`

* `wxEVT_STC_USERLISTSELECTION`

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_CALLTIP_CLICK`

* `wxEVT_STC_AUTOCOMP_SELECTION`

* `wxEVT_STC_AUTOCOMP_SELECTION_CHANGE`

* `wxEVT_STC_AUTOCOMP_COMPLETED`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`

# `getShift`

```erlang
-spec getShift(This) -> boolean() when This :: wxStyledTextEvent().
```

Returns true if the Shift key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`

# `getText`

```erlang
-spec getText(This) -> unicode:charlist() when This :: wxStyledTextEvent().
```

Deprecated:

Use `wxCommandEvent:getString/1` instead.

# `getWParam`

```erlang
-spec getWParam(This) -> integer() when This :: wxStyledTextEvent().
```

Returns value of the WParam field for this event.

This method is valid for `wxEVT_STC_MACRORECORD` events.

# `getX`

```erlang
-spec getX(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the X coordinate of the mouse for this event.

This method is valid for the following event types:

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_START_DRAG`

* `wxEVT_STC_DRAG_OVER`

* `wxEVT_STC_DO_DROP`

# `getY`

```erlang
-spec getY(This) -> integer() when This :: wxStyledTextEvent().
```

Returns the Y coordinate of the mouse for this event.

This method is valid for the following event types:

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_START_DRAG`

* `wxEVT_STC_DRAG_OVER`

* `wxEVT_STC_DO_DROP`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
