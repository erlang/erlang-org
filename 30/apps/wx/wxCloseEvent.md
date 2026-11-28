# `wxCloseEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCloseEvent.erl#L58)

This event class contains information about window and session close events.

The handler function for EVT_CLOSE is called when the user has tried to close a a frame
or dialog box using the window manager (X) or system menu (Windows). It can also be
invoked by the application itself programmatically, for example by calling the `wxWindow:close/2` function.

You should check whether the application is forcing the deletion of the window using `canVeto/1`. If
this is false, you `must` destroy the window using `wxWindow:'Destroy'/1`.

If the return value is true, it is up to you whether you respond by destroying the window.

If you don't destroy the window, you should call `veto/2` to let the calling code know that you
did not destroy the window. This allows the `wxWindow:close/2` function to return true or false depending on
whether the close instruction was honoured or not.

Example of a `m:wxCloseEvent` handler:

The EVT_END_SESSION event is slightly different as it is sent by the system when the user
session is ending (e.g. because of log out or shutdown) and so all windows are being
forcefully closed. At least under MSW, after the handler for this event is executed the
program is simply killed by the system. Because of this, the default handler for this
event provided by wxWidgets calls all the usual cleanup code (including `wxApp::OnExit()`
(not implemented in wx)) so that it could still be executed and exit()s the process
itself, without waiting for being killed. If this behaviour is for some reason
undesirable, make sure that you define a handler for this event in your wxApp-derived
class and do not call `event.Skip()` in it (but be aware that the system will still kill
your application).

See:
* `wxWindow:close/2`

* [Overview windowdeletion](https://docs.wxwidgets.org/3.2/overview_windowdeletion.html#overview_windowdeletion)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxCloseEvent](https://docs.wxwidgets.org/3.2/classwx_close_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxCloseEventType` to subscribe to events of this type.

# `wxClose`

```erlang
-type wxClose() :: #wxClose{type :: wxCloseEvent:wxCloseEventType()}.
```

# `wxCloseEvent`

```erlang
-type wxCloseEvent() :: wx:wx_object().
```

# `wxCloseEventType`

```erlang
-type wxCloseEventType() :: close_window | end_session | query_end_session.
```

# `canVeto`

```erlang
-spec canVeto(This) -> boolean() when This :: wxCloseEvent().
```

Returns true if you can veto a system shutdown or a window close event.

Vetoing a window close event is not possible if the calling code wishes to force the
application to exit, and so this function must be called to check this.

# `getLoggingOff`

```erlang
-spec getLoggingOff(This) -> boolean() when This :: wxCloseEvent().
```

Returns true if the user is just logging off or false if the system is shutting down.

This method can only be called for end session and query end session events, it doesn't
make sense for close window event.

# `setCanVeto`

```erlang
-spec setCanVeto(This, CanVeto) -> ok when This :: wxCloseEvent(), CanVeto :: boolean().
```

Sets the 'can veto' flag.

# `setLoggingOff`

```erlang
-spec setLoggingOff(This, LoggingOff) -> ok when This :: wxCloseEvent(), LoggingOff :: boolean().
```

Sets the 'logging off' flag.

# `veto`

```erlang
-spec veto(This) -> ok when This :: wxCloseEvent().
```

# `veto`

```erlang
-spec veto(This, [Option]) -> ok when This :: wxCloseEvent(), Option :: {veto, boolean()}.
```

Call this from your event handler to veto a system shutdown or to signal to the calling
application that a window close did not happen.

You can only veto a shutdown if `canVeto/1` returns true.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
