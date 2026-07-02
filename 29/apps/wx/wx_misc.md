# `wx_misc`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wx_misc.erl#L62)

Miscellaneous functions.

# `beginBusyCursor`

```erlang
-spec beginBusyCursor() -> ok.
```

# `beginBusyCursor`

```erlang
-spec beginBusyCursor([Option]) -> ok when Option :: {cursor, wxCursor:wxCursor()}.
```

Changes the cursor to the given cursor for all windows in the application.

Use `wx_misc:endBusyCursor/0` to revert the cursor back to its previous state. These two calls can be nested, and
a counter ensures that only the outer calls take effect.

See: `wx_misc:isBusy/0`

# `bell`

```erlang
-spec bell() -> ok.
```

Ring the system bell.

Note: This function is categorized as a GUI one and so is not thread-safe.

# `displaySize`

```erlang
-spec displaySize() -> {Width :: integer(), Height :: integer()}.
```

Returns the display size in pixels.

Note: Use of this function is not recommended in the new code as it only works for the
primary display. Use `wxDisplay:getGeometry/1` to retrieve the size of the appropriate display instead.

Either of output pointers can be NULL if the caller is not interested in the
corresponding value.

See: `m:wxDisplay`

# `endBusyCursor`

```erlang
-spec endBusyCursor() -> ok.
```

Changes the cursor back to the original cursor, for all windows in the application.

Use with `wx_misc:beginBusyCursor/1`.

See: `wx_misc:isBusy/0`

# `findMenuItemId`

```erlang
-spec findMenuItemId(Frame, MenuString, ItemString) -> integer()
                        when
                            Frame :: wxFrame:wxFrame(),
                            MenuString :: unicode:chardata(),
                            ItemString :: unicode:chardata().
```

Find a menu item identifier associated with the given frame's menu bar.

# `findWindowAtPoint`

```erlang
-spec findWindowAtPoint(Pt) -> wxWindow:wxWindow() when Pt :: {X :: integer(), Y :: integer()}.
```

Find the deepest window at the given mouse position in screen coordinates, returning the
window if found, or NULL if not.

This function takes child windows at the given position into account even if they are
disabled. The hidden children are however skipped by it.

# `getCurrentId`

```erlang
-spec getCurrentId() -> integer().
```

Returns the current id.

# `getEmailAddress`

```erlang
-spec getEmailAddress() -> unicode:charlist().
```

Copies the user's email address into the supplied buffer, by concatenating the values
returned by `wxGetFullHostName()` (not implemented in wx) and `wx_misc:getUserId/0`.

Return: true if successful, false otherwise.

# `getHomeDir`

```erlang
-spec getHomeDir() -> unicode:charlist().
```

Return the (current) user's home directory.

# `getKeyState`

```erlang
-spec getKeyState(Key) -> boolean() when Key :: wx:wx_enum().
```

For normal keys, returns true if the specified key is currently down.

For togglable keys (Caps Lock, Num Lock and Scroll Lock), returns true if the key is
toggled such that its LED indicator is lit. There is currently no way to test whether
togglable keys are up or down.

Even though there are virtual key codes defined for mouse buttons, they cannot be used
with this function currently.

In wxGTK, this function can be only used with modifier keys (`WXK_ALT`, `WXK_CONTROL` and `WXK_SHIFT`)
when not using X11 backend currently.

# `getMousePosition`

```erlang
-spec getMousePosition() -> {X :: integer(), Y :: integer()}.
```

Returns the mouse position in screen coordinates.

# `getMouseState`

```erlang
-spec getMouseState() -> wx:wx_wxMouseState().
```

Returns the current state of the mouse.

Returns a `wx_wxMouseState()` instance that contains the current position of the mouse pointer in screen
coordinates, as well as boolean values indicating the up/down status of the mouse buttons
and the modifier keys.

# `getOsDescription`

```erlang
-spec getOsDescription() -> unicode:charlist().
```

Returns the string containing the description of the current platform in a user-readable
form.

For example, this function may return strings like "Windows 10 (build 10240), 64-bit
edition" or "Linux 4.1.4 i386".

# `getUserId`

```erlang
-spec getUserId() -> unicode:charlist().
```

This function returns the "user id" also known as "login name" under Unix (i.e.

something like "jsmith"). It uniquely identifies the current user (on this system). Under
Windows or NT, this function first looks in the environment variables USER and LOGNAME; if
neither of these is found, the entry `UserId` in the `wxWidgets` section of the WIN.INI
file is tried.

Return: The login name if successful or an empty string otherwise.

# `isBusy`

```erlang
-spec isBusy() -> boolean().
```

Returns true if between two `wx_misc:beginBusyCursor/1` and `wx_misc:endBusyCursor/0`
calls.

# `isPlatform64Bit`

```erlang
-spec isPlatform64Bit() -> boolean().
```

Returns true if the operating system the program is running under is 64 bit.

The check is performed at run-time and may differ from the value available at
compile-time (at compile-time you can just check if `sizeof(void*) == 8`) since the
program could be running in emulation mode or in a mixed 32/64 bit system (bi-architecture
operating system).

Note: This function is not 100% reliable on some systems given the fact that there isn't
always a standard way to do a reliable check on the OS architecture.

# `isPlatformLittleEndian`

```erlang
-spec isPlatformLittleEndian() -> boolean().
```

Returns true if the current platform is little endian (instead of big endian).

The check is performed at run-time.

# `launchDefaultBrowser`

```erlang
-spec launchDefaultBrowser(Url) -> boolean() when Url :: unicode:chardata().
```

# `launchDefaultBrowser`

```erlang
-spec launchDefaultBrowser(Url, [Option]) -> boolean()
                              when Url :: unicode:chardata(), Option :: {flags, integer()}.
```

Opens the `url` in user's default browser.

If the `flags` parameter contains `wxBROWSER_NEW_WINDOW` flag, a new window is opened for
the URL (currently this is only supported under Windows).

And unless the `flags` parameter contains `wxBROWSER_NOBUSYCURSOR` flag, a busy cursor is
shown while the browser is being launched (using `wxBusyCursor` (not implemented in wx)).

The parameter `url` is interpreted as follows:

* if it has a valid scheme (e.g. `"file:"`, `"http:"` or `"mailto:"`) it is passed to the
appropriate browser configured in the user system.

* if it has no valid scheme (e.g. it's a local file path without the `"file:"` prefix),
then ?wxFileExists and ?wxDirExists are used to test if it's a local file/directory; if it
is, then the browser is called with the `url` parameter eventually prefixed by `"file:"`.

* if it has no valid scheme and it's not a local file/directory, then `"http:"` is
prepended and the browser is called.

Returns true if the application was successfully launched.

Note: For some configurations of the running user, the application which is launched to
open the given URL may be URL-dependent (e.g. a browser may be used for local URLs while
another one may be used for remote URLs).

# `mSWSetEmulationLevel`

```erlang
-spec mSWSetEmulationLevel(Level) -> boolean() when Level :: wx:wx_enum().
```

# `mSWSetEmulationLevel`

```erlang
-spec mSWSetEmulationLevel(Level, Executable) -> boolean()
                              when Level :: wx:wx_enum(), Executable :: string().
```

# `newId`

```erlang
-spec newId() -> integer().
```

Deprecated:

Ids generated by it can conflict with the Ids defined by the user code, use `wxID_ANY` to
assign ids which are guaranteed to not conflict with the user-defined ids for the controls
and menu items you create instead of using this function.

Generates an integer identifier unique to this run of the program.

# `registerId`

```erlang
-spec registerId(Id) -> ok when Id :: integer().
```

Ensures that Ids subsequently generated by `wx_misc:newId/0` do not clash with the given `id`.

# `setCursor`

```erlang
-spec setCursor(Cursor) -> ok when Cursor :: wxCursor:wxCursor().
```

Globally sets the cursor; only has an effect on Windows, Mac and GTK+.

You should call this function with wxNullCursor to restore the system cursor.

See:
* `m:wxCursor`

* `wxWindow:setCursor/2`

# `setDetectableAutoRepeat`

```erlang
-spec setDetectableAutoRepeat(Flag) -> boolean() when Flag :: boolean().
```

Don't synthesize KeyUp events holding down a key and producing KeyDown events with
autorepeat.

On by default and always on in wxMSW.

# `shell`

```erlang
-spec shell() -> boolean().
```

# `shell`

```erlang
-spec shell([Option]) -> boolean() when Option :: {command, unicode:chardata()}.
```

Executes a command in an interactive shell window.

If no command is specified, then just the shell is spawned.

See: [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_exec)

# `shutdown`

```erlang
-spec shutdown() -> boolean().
```

# `shutdown`

```erlang
-spec shutdown([Option]) -> boolean() when Option :: {flags, integer()}.
```

This function shuts down or reboots the computer depending on the value of the `flags`.

Note: Note that performing the shutdown requires the corresponding access rights
(superuser under Unix, SE_SHUTDOWN privilege under Windows) and that this function is only
implemented under Unix and MSW.

Return: true on success, false if an error occurred.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
