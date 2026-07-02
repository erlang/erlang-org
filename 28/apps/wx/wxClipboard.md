# `wxClipboard`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxClipboard.erl#L58)

A class for manipulating the clipboard.

To use the clipboard, you call member functions of the global ?wxTheClipboard object.

See the overview_dataobject for further information.

Call `open/1` to get ownership of the clipboard. If this operation returns true, you now own the
clipboard. Call `setData/2` to put data on the clipboard, or `getData/2` to retrieve data from the clipboard.
Call `close/1` to close the clipboard and relinquish ownership. You should keep the clipboard open
only momentarily.

For example:

Note: On GTK, the clipboard behavior can vary depending on the configuration of the
end-user's machine. In order for the clipboard data to persist after the window closes, a
clipboard manager must be installed. Some clipboard managers will automatically flush the
clipboard after each new piece of data is added, while others will not. The @Flush()
function will force the clipboard manager to flush the data.

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* [Overview dataobject](https://docs.wxwidgets.org/3.2/overview_dataobject.html#overview_dataobject)

* `m:wxDataObject`

wxWidgets docs: [wxClipboard](https://docs.wxwidgets.org/3.2/classwx_clipboard.html)

# `wxClipboard`

```elixir
-type wxClipboard() :: wx:wx_object().
```

# `addData`

```elixir
-spec addData(This, Data) -> boolean() when This :: wxClipboard(), Data :: wxDataObject:wxDataObject().
```

Call this function to add the data object to the clipboard.

This is an obsolete synonym for `setData/2`.

# `clear`

```elixir
-spec clear(This) -> ok when This :: wxClipboard().
```

Clears the global clipboard object and the system's clipboard if possible.

# `close`

```elixir
-spec close(This) -> ok when This :: wxClipboard().
```

Call this function to close the clipboard, having opened it with `open/1`.

# `destroy`

```elixir
-spec destroy(This :: wxClipboard()) -> ok.
```

Destroys the object

# `flush`

```elixir
-spec flush(This) -> boolean() when This :: wxClipboard().
```

Flushes the clipboard: this means that the data which is currently on clipboard will stay
available even after the application exits (possibly eating memory), otherwise the
clipboard will be emptied on exit.

Currently this method is implemented in MSW and GTK and always returns false otherwise.

Note: On GTK, only the non-primary selection can be flushed. Calling this function when
the clipboard is using the primary selection will return false and not make any data
available after the program exits.

Return: false if the operation is unsuccessful for any reason.

# `get`

```elixir
-spec get() -> wxClipboard().
```

Returns the global instance (wxTheClipboard) of the clipboard object.

# `getData`

```elixir
-spec getData(This, Data) -> boolean() when This :: wxClipboard(), Data :: wxDataObject:wxDataObject().
```

Call this function to fill `data` with data on the clipboard, if available in the
required format.

Returns true on success.

# `isOpened`

```elixir
-spec isOpened(This) -> boolean() when This :: wxClipboard().
```

Returns true if the clipboard has been opened.

# `isSupported`

```elixir
-spec isSupported(This, Format) -> boolean() when This :: wxClipboard(), Format :: wx:wx_enum().
```

Returns true if there is data which matches the data format of the given data object
currently `available` on the clipboard.

# `new`

```elixir
-spec new() -> wxClipboard().
```

Default constructor.

# `open`

```elixir
-spec open(This) -> boolean() when This :: wxClipboard().
```

Call this function to open the clipboard before calling `setData/2` and `getData/2`.

Call `close/1` when you have finished with the clipboard. You should keep the clipboard open for
only a very short time.

Return: true on success. This should be tested (as in the sample shown above).

# `setData`

```elixir
-spec setData(This, Data) -> boolean() when This :: wxClipboard(), Data :: wxDataObject:wxDataObject().
```

Call this function to set the data object to the clipboard.

The new data object replaces any previously set one, so if the application wants to
provide clipboard data in several different formats, it must use a composite data object
supporting all of the formats instead of calling this function several times with
different data objects as this would only leave data from the last one in the clipboard.

After this function has been called, the clipboard owns the data, so do not delete the
data explicitly.

# `usePrimarySelection`

```elixir
-spec usePrimarySelection(This) -> ok when This :: wxClipboard().
```

# `usePrimarySelection`

```elixir
-spec usePrimarySelection(This, [Option]) -> ok
                             when This :: wxClipboard(), Option :: {primary, boolean()}.
```

On platforms supporting it (all X11-based ports), `m:wxClipboard` uses the CLIPBOARD X11
selection by default.

When this function is called with true, all subsequent clipboard operations will use
PRIMARY selection until this function is called again with false.

On the other platforms, there is no PRIMARY selection and so all clipboard operations
will fail. This allows implementing the standard X11 handling of the clipboard which
consists in copying data to the CLIPBOARD selection only when the user explicitly requests
it (i.e. by selecting the "Copy" menu command) but putting the currently selected text
into the PRIMARY selection automatically, without overwriting the normal clipboard
contents with the currently selected text on the other platforms.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
