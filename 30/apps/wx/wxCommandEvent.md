# `wxCommandEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCommandEvent.erl#L58)

This event class contains information about command events, which originate from a
variety of simple controls.

Note that wxCommandEvents and wxCommandEvent-derived event classes by default and unlike
other wxEvent-derived classes propagate upward from the source window (the window which
emits the event) up to the first parent which processes the event. Be sure to read overview_events_propagation.

More complex controls, such as `m:wxTreeCtrl`, have separate command event classes.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxCommandEvent](https://docs.wxwidgets.org/3.2/classwx_command_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxCommandEventType` to subscribe to events of this type.

# `wxCommand`

```erlang
-type wxCommand() ::
          #wxCommand{type :: wxCommandEvent:wxCommandEventType(),
                     cmdString :: unicode:chardata(),
                     commandInt :: integer(),
                     extraLong :: integer()}.
```

# `wxCommandEvent`

```erlang
-type wxCommandEvent() :: wx:wx_object().
```

# `wxCommandEventType`

```erlang
-type wxCommandEventType() ::
          command_button_clicked | command_checkbox_clicked | command_choice_selected |
          command_listbox_selected | command_listbox_doubleclicked | command_text_updated |
          command_text_enter | text_maxlen | command_menu_selected | command_slider_updated |
          command_radiobox_selected | command_radiobutton_selected | command_scrollbar_updated |
          command_vlbox_selected | command_combobox_selected | combobox_dropdown | combobox_closeup |
          command_tool_rclicked | command_tool_enter | tool_dropdown | command_checklistbox_toggled |
          command_togglebutton_clicked | command_left_click | command_left_dclick |
          command_right_click | command_set_focus | command_kill_focus | command_enter |
          notification_message_click | notification_message_dismissed | notification_message_action.
```

# `getClientData`

```erlang
-spec getClientData(This) -> term() when This :: wxCommandEvent().
```

Returns client object pointer for a listbox or choice selection event (not valid for a
deselection).

# `getExtraLong`

```erlang
-spec getExtraLong(This) -> integer() when This :: wxCommandEvent().
```

Returns extra information dependent on the event objects type.

If the event comes from a listbox selection, it is a boolean determining whether the
event was a selection (true) or a deselection (false). A listbox deselection only occurs
for multiple-selection boxes, and in this case the index and string values are
indeterminate and the listbox must be examined by the application.

# `getInt`

```erlang
-spec getInt(This) -> integer() when This :: wxCommandEvent().
```

Returns the integer identifier corresponding to a listbox, choice or radiobox selection
(only if the event was a selection, not a deselection), or a boolean value representing
the value of a checkbox.

For a menu item, this method returns -1 if the item is not checkable or a boolean value
(true or false) for checkable items indicating the new state of the item.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxCommandEvent().
```

Returns item index for a listbox or choice selection event (not valid for a deselection).

# `getString`

```erlang
-spec getString(This) -> unicode:charlist() when This :: wxCommandEvent().
```

Returns item string for a listbox or choice selection event.

If one or several items have been deselected, returns the index of the first deselected
item. If some items have been selected and others deselected at the same time, it will
return the index of the first selected item.

# `isChecked`

```erlang
-spec isChecked(This) -> boolean() when This :: wxCommandEvent().
```

This method can be used with checkbox and menu events: for the checkboxes, the method
returns true for a selection event and false for a deselection one.

For the menu events, this method indicates if the menu item just has become checked or
unchecked (and thus only makes sense for checkable menu items).

Notice that this method cannot be used with `m:wxCheckListBox` currently.

# `isSelection`

```erlang
-spec isSelection(This) -> boolean() when This :: wxCommandEvent().
```

For a listbox or similar event, returns true if it is a selection, false if it is a
deselection.

If some items have been selected and others deselected at the same time, it will return
true.

# `setInt`

```erlang
-spec setInt(This, IntCommand) -> ok when This :: wxCommandEvent(), IntCommand :: integer().
```

Sets the `m_commandInt` member.

# `setString`

```erlang
-spec setString(This, String) -> ok when This :: wxCommandEvent(), String :: unicode:chardata().
```

Sets the `m_commandString` member.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
