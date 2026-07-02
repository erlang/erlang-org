# `wxFindReplaceData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxFindReplaceData.erl#L58)

`m:wxFindReplaceData` holds the data for `m:wxFindReplaceDialog`.

It is used to initialize the dialog with the default values and will keep the last values
from the dialog when it is closed. It is also updated each time a `wxFindDialogEvent` (not
implemented in wx) is generated so instead of using the `wxFindDialogEvent` (not
implemented in wx) methods you can also directly query this object.

Note that all `SetXXX()` methods may only be called before showing the dialog and calling
them has no effect later.

wxWidgets docs: [wxFindReplaceData](https://docs.wxwidgets.org/3.2/classwx_find_replace_data.html)

# `wxFindReplaceData`

```elixir
-type wxFindReplaceData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxFindReplaceData()) -> ok.
```

Destroys the object

# `getFindString`

```elixir
-spec getFindString(This) -> unicode:charlist() when This :: wxFindReplaceData().
```

Get the string to find.

# `getFlags`

```elixir
-spec getFlags(This) -> integer() when This :: wxFindReplaceData().
```

Get the combination of `wxFindReplaceFlags` values.

# `getReplaceString`

```elixir
-spec getReplaceString(This) -> unicode:charlist() when This :: wxFindReplaceData().
```

Get the replacement string.

# `new`

```elixir
-spec new() -> wxFindReplaceData().
```

# `new`

```elixir
-spec new([Option]) -> wxFindReplaceData() when Option :: {flags, integer()}.
```

Constructor initializes the flags to default value (0).

# `setFindString`

```elixir
-spec setFindString(This, Str) -> ok when This :: wxFindReplaceData(), Str :: unicode:chardata().
```

Set the string to find (used as initial value by the dialog).

# `setFlags`

```elixir
-spec setFlags(This, Flags) -> ok when This :: wxFindReplaceData(), Flags :: integer().
```

Set the flags to use to initialize the controls of the dialog.

# `setReplaceString`

```elixir
-spec setReplaceString(This, Str) -> ok when This :: wxFindReplaceData(), Str :: unicode:chardata().
```

Set the replacement string (used as initial value by the dialog).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
