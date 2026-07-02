# `wxTextDataObject`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxTextDataObject.erl#L58)

`m:wxTextDataObject` is a specialization of `wxDataObjectSimple` (not implemented in wx)
for text data.

It can be used without change to paste data into the `m:wxClipboard` or a `wxDropSource`
(not implemented in wx). A user may wish to derive a new class from this class for
providing text on-demand in order to minimize memory consumption when offering data in
several formats, such as plain text and RTF because by default the text is stored in a
string in this class, but it might as well be generated when requested. For this, `getTextLength/1` and `getText/1`
will have to be overridden.

Note that if you already have the text inside a string, you will not achieve any
efficiency gain by overriding these functions because copying wxStrings is already a very
efficient operation (data is not actually copied because wxStrings are reference counted).

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* `m:wxDataObject`

* `m:wxFileDataObject`

* `m:wxBitmapDataObject`

This class is derived, and can use functions, from:

* `m:wxDataObject`

wxWidgets docs: [wxTextDataObject](https://docs.wxwidgets.org/3.2/classwx_text_data_object.html)

# `wxTextDataObject`

```elixir
-type wxTextDataObject() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxTextDataObject()) -> ok.
```

Destroys the object

# `getText`

```elixir
-spec getText(This) -> unicode:charlist() when This :: wxTextDataObject().
```

Returns the text associated with the data object.

You may wish to override this method when offering data on-demand, but this is not
required by wxWidgets' internals. Use this method to get data in text form from the `m:wxClipboard`.

# `getTextLength`

```elixir
-spec getTextLength(This) -> integer() when This :: wxTextDataObject().
```

Returns the data size.

By default, returns the size of the text data set in the constructor or using `setText/2`. This can
be overridden to provide text size data on-demand. It is recommended to return the text
length plus 1 for a trailing zero, but this is not strictly required.

# `new`

```elixir
-spec new() -> wxTextDataObject().
```

# `new`

```elixir
-spec new([Option]) -> wxTextDataObject() when Option :: {text, unicode:chardata()}.
```

Constructor, may be used to initialise the text (otherwise `setText/2` should be used
later).

# `setText`

```elixir
-spec setText(This, StrText) -> ok when This :: wxTextDataObject(), StrText :: unicode:chardata().
```

Sets the text associated with the data object.

This method is called when the data object receives the data and, by default, copies the
text into the member variable. If you want to process the text on the fly you may wish to
override this function.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
