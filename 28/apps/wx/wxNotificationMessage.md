# `wxNotificationMessage`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxNotificationMessage.erl#L58)

This class allows showing the user a message non intrusively.

Currently it is implemented natively for Windows, macOS, GTK and uses generic toast
notifications under the other platforms. It's not recommended but `wxGenericNotificationMessage`
can be used instead of the native ones. This might make sense if your application
requires features not available in the native implementation.

Notice that this class is not a window and so doesn't derive from `m:wxWindow`.

Platform Notes

Par:

Up to Windows 8 balloon notifications are displayed from an icon in the notification area
of the taskbar. If your application uses a `m:wxTaskBarIcon` you should call `useTaskBarIcon/1` to ensure
that only one icon is shown in the notification area. Windows 10 displays all
notifications as popup toasts. To suppress the additional icon in the notification area on
Windows 10 and for toast notification support on Windows 8 it is recommended to call `mSWUseToasts/1`
before showing the first notification message.

Par:

The macOS implementation uses Notification Center to display native notifications. In
order to use actions your notifications must use the alert style. This can be enabled by
the user in system settings or by setting the `NSUserNotificationAlertStyle` value in
Info.plist to `alert`. Please note that the user always has the option to change the
notification style.

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxNotificationMessage](https://docs.wxwidgets.org/3.2/classwx_notification_message.html)

## Events

Event types emitted from this class:

* [`notification_message_click`](`m:wxCommandEvent`)

* [`notification_message_dismissed`](`m:wxCommandEvent`)

* [`notification_message_action`](`m:wxCommandEvent`)

# `wxNotificationMessage`

```elixir
-type wxNotificationMessage() :: wx:wx_object().
```

# `addAction`

```elixir
-spec addAction(This, Actionid) -> boolean() when This :: wxNotificationMessage(), Actionid :: integer().
```

# `addAction`

```elixir
-spec addAction(This, Actionid, [Option]) -> boolean()
                   when
                       This :: wxNotificationMessage(),
                       Actionid :: integer(),
                       Option :: {label, unicode:chardata()}.
```

Add an action to the notification.

If supported by the implementation this are usually buttons in the notification
selectable by the user.

Return: false if the current implementation or OS version does not support actions in notifications.

Since: 3.1.0

# `close`

```elixir
-spec close(This) -> boolean() when This :: wxNotificationMessage().
```

Hides the notification.

Returns true if it was hidden or false if it couldn't be done (e.g. on some systems
automatically hidden notifications can't be hidden manually).

# `destroy`

```elixir
-spec destroy(This :: wxNotificationMessage()) -> ok.
```

Destroys the object

# `mSWUseToasts`

```elixir
-spec mSWUseToasts() -> boolean().
```

# `mSWUseToasts`

```elixir
-spec mSWUseToasts([Option]) -> boolean()
                      when Option :: {shortcutPath, unicode:chardata()} | {appId, unicode:chardata()}.
```

Enables toast notifications available since Windows 8 and suppresses the additional icon
in the notification area on Windows 10.

Toast notifications `require` a shortcut to the application in the start menu. The start
menu shortcut needs to contain an Application User Model ID. It is recommended that the
applications setup creates the shortcut and the application specifies the setup created
shortcut in `shortcutPath`. A call to this method will verify (and if necessary modify)
the shortcut before enabling toast notifications.

Return: false if toast notifications could not be enabled.

Only for:wxmsw

Since: 3.1.0

# `new`

```elixir
-spec new() -> wxNotificationMessage().
```

Default constructor, use `setParent/2`, `setTitle/2` and `setMessage/2` to initialize the
object before showing it.

# `new`

```elixir
-spec new(Title) -> wxNotificationMessage() when Title :: unicode:chardata().
```

# `new`

```elixir
-spec new(Title, [Option]) -> wxNotificationMessage()
             when
                 Title :: unicode:chardata(),
                 Option ::
                     {message, unicode:chardata()} | {parent, wxWindow:wxWindow()} | {flags, integer()}.
```

Create a notification object with the given attributes.

See `setTitle/2`, `setMessage/2`, `setParent/2` and `setFlags/2` for the description of the corresponding parameters.

# `setFlags`

```elixir
-spec setFlags(This, Flags) -> ok when This :: wxNotificationMessage(), Flags :: integer().
```

This parameter can be currently used to specify the icon to show in the notification.

Valid values are `wxICON_INFORMATION`, `wxICON_WARNING` and `wxICON_ERROR` (notice that `wxICON_QUESTION`
is not allowed here). Some implementations of this class may not support the icons.

See: `setIcon/2`

# `setIcon`

```elixir
-spec setIcon(This, Icon) -> ok when This :: wxNotificationMessage(), Icon :: wxIcon:wxIcon().
```

Specify a custom icon to be displayed in the notification.

Some implementations of this class may not support custom icons.

See: `setFlags/2`

Since: 3.1.0

# `setMessage`

```elixir
-spec setMessage(This, Message) -> ok
                    when This :: wxNotificationMessage(), Message :: unicode:chardata().
```

Set the main text of the notification.

This should be a more detailed description than the title but still limited to reasonable
length (not more than 256 characters).

# `setParent`

```elixir
-spec setParent(This, Parent) -> ok when This :: wxNotificationMessage(), Parent :: wxWindow:wxWindow().
```

Set the parent for this notification: the notification will be associated with the top
level parent of this window or, if this method is not called, with the main application
window by default.

# `setTitle`

```elixir
-spec setTitle(This, Title) -> ok when This :: wxNotificationMessage(), Title :: unicode:chardata().
```

Set the title, it must be a concise string (not more than 64 characters), use `setMessage/2`
to give the user more details.

# `show`

```elixir
-spec show(This) -> boolean() when This :: wxNotificationMessage().
```

# `show`

```elixir
-spec show(This, [Option]) -> boolean()
              when This :: wxNotificationMessage(), Option :: {timeout, integer()}.
```

Show the notification to the user and hides it after `timeout` seconds are elapsed.

Special values `Timeout_Auto` and `Timeout_Never` can be used here, notice that you
shouldn't rely on `timeout` being exactly respected because the current platform may only
support default timeout value and also because the user may be able to close the notification.

Note: When using native notifications in wxGTK, the timeout is ignored for the
notifications with `wxICON_WARNING` or `wxICON_ERROR` flags, they always remain shown
unless they're explicitly hidden by the user, i.e. behave as if Timeout_Auto were given.

Return: false if an error occurred.

# `useTaskBarIcon`

```elixir
-spec useTaskBarIcon(Icon) -> wxTaskBarIcon:wxTaskBarIcon() when Icon :: wxTaskBarIcon:wxTaskBarIcon().
```

If the application already uses a `m:wxTaskBarIcon`, it should be connected to
notifications by using this method.

This has no effect if toast notifications are used.

Return: the task bar icon which was used previously (may be `NULL`)

Only for:wxmsw

---

*Consult [api-reference.md](api-reference.md) for complete listing*
