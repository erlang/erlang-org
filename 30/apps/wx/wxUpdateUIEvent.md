# `wxUpdateUIEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxUpdateUIEvent.erl#L58)

This class is used for pseudo-events which are called by wxWidgets to give an application
the chance to update various user interface elements.

Without update UI events, an application has to work hard to check/uncheck,
enable/disable, show/hide, and set the text for elements such as menu items and toolbar
buttons. The code for doing this has to be mixed up with the code that is invoked when an
action is invoked for a menu item or button.

With update UI events, you define an event handler to look at the state of the
application and change UI elements accordingly. wxWidgets will call your member functions
in idle time, so you don't have to worry where to call this code.

In addition to being a clearer and more declarative method, it also means you don't have
to worry whether you're updating a toolbar or menubar identifier. The same handler can
update a menu item and toolbar button, if the identifier is the same. Instead of directly
manipulating the menu or button, you call functions in the event object, such as `check/2`.
wxWidgets will determine whether such a call has been made, and which UI element to update.

These events will work for popup menus as well as menubars. Just before a menu is popped
up, `wxMenu::UpdateUI` (not implemented in wx) is called to process any UI events for the
window that owns the menu.

If you find that the overhead of UI update processing is affecting your application, you
can do one or both of the following:

* Call `setMode/1` with a value of wxUPDATE_UI_PROCESS_SPECIFIED, and set the extra style
wxWS_EX_PROCESS_UI_UPDATES for every window that should receive update events. No other
windows will receive update events.

* Call `setUpdateInterval/1` with a millisecond value to set the delay between updates. You may need to call `wxWindow:updateWindowUI/2` at
critical points, for example when a dialog is about to be shown, in case the user sees a
slight delay before windows are updated.

Note that although events are sent in idle time, defining a `m:wxIdleEvent` handler for
a window does not affect this because the events are sent from `wxWindow::OnInternalIdle`
(not implemented in wx) which is always called in idle time.

wxWidgets tries to optimize update events on some platforms. On Windows and GTK+, events
for menubar items are only sent when the menu is about to be shown, and not in idle time.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxUpdateUIEvent](https://docs.wxwidgets.org/3.2/classwx_update_u_i_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxUpdateUIEventType` to subscribe to events of this type.

# `wxUpdateUI`

```erlang
-type wxUpdateUI() :: #wxUpdateUI{type :: wxUpdateUIEvent:wxUpdateUIEventType()}.
```

# `wxUpdateUIEvent`

```erlang
-type wxUpdateUIEvent() :: wx:wx_object().
```

# `wxUpdateUIEventType`

```erlang
-type wxUpdateUIEventType() :: update_ui.
```

# `canUpdate`

```erlang
-spec canUpdate(Window) -> boolean() when Window :: wxWindow:wxWindow().
```

Returns true if it is appropriate to update (send UI update events to) this window.

This function looks at the mode used (see `setMode/1`), the wxWS_EX_PROCESS_UI_UPDATES flag in `window`,
the time update events were last sent in idle time, and the update interval, to determine
whether events should be sent to this window now. By default this will always return true
because the update mode is initially wxUPDATE_UI_PROCESS_ALL and the interval is set to 0;
so update events will be sent as often as possible. You can reduce the frequency that
events are sent by changing the mode and/or setting an update interval.

See:
* `resetUpdateTime/0`

* `setUpdateInterval/1`

* `setMode/1`

# `check`

```erlang
-spec check(This, Check) -> ok when This :: wxUpdateUIEvent(), Check :: boolean().
```

Check or uncheck the UI element.

# `enable`

```erlang
-spec enable(This, Enable) -> ok when This :: wxUpdateUIEvent(), Enable :: boolean().
```

Enable or disable the UI element.

# `getChecked`

```erlang
-spec getChecked(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the UI element should be checked.

# `getEnabled`

```erlang
-spec getEnabled(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the UI element should be enabled.

# `getMode`

```erlang
-spec getMode() -> wx:wx_enum().
```

Static function returning a value specifying how wxWidgets will send update events: to
all windows, or only to those which specify that they will process the events.

See: `setMode/1`

# `getSetChecked`

```erlang
-spec getSetChecked(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the application has called `check/2`.

For wxWidgets internal use only.

# `getSetEnabled`

```erlang
-spec getSetEnabled(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the application has called `enable/2`.

For wxWidgets internal use only.

# `getSetShown`

```erlang
-spec getSetShown(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the application has called `show/2`.

For wxWidgets internal use only.

# `getSetText`

```erlang
-spec getSetText(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the application has called `setText/2`.

For wxWidgets internal use only.

# `getShown`

```erlang
-spec getShown(This) -> boolean() when This :: wxUpdateUIEvent().
```

Returns true if the UI element should be shown.

# `getText`

```erlang
-spec getText(This) -> unicode:charlist() when This :: wxUpdateUIEvent().
```

Returns the text that should be set for the UI element.

# `getUpdateInterval`

```erlang
-spec getUpdateInterval() -> integer().
```

Returns the current interval between updates in milliseconds.

The value -1 disables updates, 0 updates as frequently as possible.

See: `setUpdateInterval/1`

# `resetUpdateTime`

```erlang
-spec resetUpdateTime() -> ok.
```

Used internally to reset the last-updated time to the current time.

It is assumed that update events are normally sent in idle time, so this is called at the
end of idle processing.

See:
* `canUpdate/1`

* `setUpdateInterval/1`

* `setMode/1`

# `setMode`

```erlang
-spec setMode(Mode) -> ok when Mode :: wx:wx_enum().
```

Specify how wxWidgets will send update events: to all windows, or only to those which
specify that they will process the events.

# `setText`

```erlang
-spec setText(This, Text) -> ok when This :: wxUpdateUIEvent(), Text :: unicode:chardata().
```

Sets the text for this UI element.

# `setUpdateInterval`

```erlang
-spec setUpdateInterval(UpdateInterval) -> ok when UpdateInterval :: integer().
```

Sets the interval between updates in milliseconds.

Set to -1 to disable updates, or to 0 to update as frequently as possible. The default is 0.

Use this to reduce the overhead of UI update events if your application has a lot of
windows. If you set the value to -1 or greater than 0, you may also need to call `wxWindow:updateWindowUI/2` at
appropriate points in your application, such as when a dialog is about to be shown.

# `show`

```erlang
-spec show(This, Show) -> ok when This :: wxUpdateUIEvent(), Show :: boolean().
```

Show or hide the UI element.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
