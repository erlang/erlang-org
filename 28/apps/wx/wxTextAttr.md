# `wxTextAttr`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxTextAttr.erl#L58)

`m:wxTextAttr` represents the character and paragraph attributes, or style, for a range
of text in a `m:wxTextCtrl` or `wxRichTextCtrl` (not implemented in wx).

When setting up a `m:wxTextAttr` object, pass a bitlist mask to `setFlags/2` to indicate which style
elements should be changed. As a convenience, when you call a setter such as SetFont, the
relevant bit will be set.

See: `m:wxTextCtrl`

wxWidgets docs: [wxTextAttr](https://docs.wxwidgets.org/3.2/classwx_text_attr.html)

# `wxTextAttr`

```elixir
-type wxTextAttr() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxTextAttr()) -> ok.
```

Destroys the object

# `getAlignment`

```elixir
-spec getAlignment(This) -> wx:wx_enum() when This :: wxTextAttr().
```

Returns the alignment flags.

See ?wxTextAttrAlignment for a list of available styles.

# `getBackgroundColour`

```elixir
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxTextAttr().
```

Returns the background colour.

# `getFlags`

```elixir
-spec getFlags(This) -> integer() when This :: wxTextAttr().
```

Returns flags indicating which attributes are applicable.

See `setFlags/2` for a list of available flags.

# `getFont`

```elixir
-spec getFont(This) -> wxFont:wxFont() when This :: wxTextAttr().
```

Creates and returns a font specified by the font attributes in the `m:wxTextAttr` object.

Note that `m:wxTextAttr` does not store a `m:wxFont` object, so this is only a temporary font.

For greater efficiency, access the font attributes directly.

# `getFontEncoding`

```elixir
-spec getFontEncoding(This) -> wx:wx_enum() when This :: wxTextAttr().
```

Returns the font encoding.

# `getFontFaceName`

```elixir
-spec getFontFaceName(This) -> unicode:charlist() when This :: wxTextAttr().
```

Returns the font face name.

# `getFontSize`

```elixir
-spec getFontSize(This) -> integer() when This :: wxTextAttr().
```

Returns the font size in points.

# `getFontStyle`

```elixir
-spec getFontStyle(This) -> wx:wx_enum() when This :: wxTextAttr().
```

Returns the font style.

# `getFontUnderlined`

```elixir
-spec getFontUnderlined(This) -> boolean() when This :: wxTextAttr().
```

Returns true if the font is underlined.

# `getFontWeight`

```elixir
-spec getFontWeight(This) -> wx:wx_enum() when This :: wxTextAttr().
```

Returns the font weight.

# `getLeftIndent`

```elixir
-spec getLeftIndent(This) -> integer() when This :: wxTextAttr().
```

Returns the left indent in tenths of a millimetre.

# `getLeftSubIndent`

```elixir
-spec getLeftSubIndent(This) -> integer() when This :: wxTextAttr().
```

Returns the left sub-indent in tenths of a millimetre.

# `getRightIndent`

```elixir
-spec getRightIndent(This) -> integer() when This :: wxTextAttr().
```

Returns the right indent in tenths of a millimeter.

# `getTabs`

```elixir
-spec getTabs(This) -> [integer()] when This :: wxTextAttr().
```

Returns an array of tab stops, each expressed in tenths of a millimeter.

Each stop is measured from the left margin and therefore each value must be larger than
the last.

# `getTextColour`

```elixir
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxTextAttr().
```

Returns the text foreground colour.

# `hasBackgroundColour`

```elixir
-spec hasBackgroundColour(This) -> boolean() when This :: wxTextAttr().
```

Returns true if the attribute object specifies a background colour.

# `hasFont`

```elixir
-spec hasFont(This) -> boolean() when This :: wxTextAttr().
```

Returns true if the attribute object specifies any font attributes.

# `hasTextColour`

```elixir
-spec hasTextColour(This) -> boolean() when This :: wxTextAttr().
```

Returns true if the attribute object specifies a text foreground colour.

# `isDefault`

```elixir
-spec isDefault(This) -> boolean() when This :: wxTextAttr().
```

Returns false if we have any attributes set, true otherwise.

# `new`

```elixir
-spec new() -> wxTextAttr().
```

Constructors.

# `new`

```elixir
-spec new(ColText) -> wxTextAttr() when ColText :: wx:wx_colour();
         (Attr) -> wxTextAttr() when Attr :: wxTextAttr().
```

# `new`

```elixir
-spec new(ColText, [Option]) -> wxTextAttr()
             when
                 ColText :: wx:wx_colour(),
                 Option ::
                     {colBack, wx:wx_colour()} | {font, wxFont:wxFont()} | {alignment, wx:wx_enum()}.
```

# `setAlignment`

```elixir
-spec setAlignment(This, Alignment) -> ok when This :: wxTextAttr(), Alignment :: wx:wx_enum().
```

Sets the paragraph alignment.

See ?wxTextAttrAlignment enumeration values.

Of these, wxTEXT_ALIGNMENT_JUSTIFIED is unimplemented. In future justification may be
supported when printing or previewing, only.

# `setBackgroundColour`

```elixir
-spec setBackgroundColour(This, ColBack) -> ok when This :: wxTextAttr(), ColBack :: wx:wx_colour().
```

Sets the background colour.

# `setFlags`

```elixir
-spec setFlags(This, Flags) -> ok when This :: wxTextAttr(), Flags :: integer().
```

Sets the flags determining which styles are being specified.

The ?wxTextAttrFlags values can be passed in a bitlist.

# `setFont`

```elixir
-spec setFont(This, Font) -> ok when This :: wxTextAttr(), Font :: wxFont:wxFont().
```

# `setFont`

```elixir
-spec setFont(This, Font, [Option]) -> ok
                 when This :: wxTextAttr(), Font :: wxFont:wxFont(), Option :: {flags, integer()}.
```

Sets the attributes for the given font.

Note that `m:wxTextAttr` does not store an actual `m:wxFont` object.

# `setFontEncoding`

```elixir
-spec setFontEncoding(This, Encoding) -> ok when This :: wxTextAttr(), Encoding :: wx:wx_enum().
```

Sets the font encoding.

# `setFontFaceName`

```elixir
-spec setFontFaceName(This, FaceName) -> ok when This :: wxTextAttr(), FaceName :: unicode:chardata().
```

Sets the font face name.

# `setFontFamily`

```elixir
-spec setFontFamily(This, Family) -> ok when This :: wxTextAttr(), Family :: wx:wx_enum().
```

Sets the font family.

# `setFontPixelSize`

```elixir
-spec setFontPixelSize(This, PixelSize) -> ok when This :: wxTextAttr(), PixelSize :: integer().
```

Sets the font size in pixels.

# `setFontPointSize`

```elixir
-spec setFontPointSize(This, PointSize) -> ok when This :: wxTextAttr(), PointSize :: integer().
```

Sets the font size in points.

# `setFontSize`

```elixir
-spec setFontSize(This, PointSize) -> ok when This :: wxTextAttr(), PointSize :: integer().
```

Sets the font size in points.

# `setFontStyle`

```elixir
-spec setFontStyle(This, FontStyle) -> ok when This :: wxTextAttr(), FontStyle :: wx:wx_enum().
```

Sets the font style (normal, italic or slanted).

# `setFontUnderlined`

```elixir
-spec setFontUnderlined(This, Underlined) -> ok when This :: wxTextAttr(), Underlined :: boolean().
```

Sets the font underlining (solid line, text colour).

# `setFontWeight`

```elixir
-spec setFontWeight(This, FontWeight) -> ok when This :: wxTextAttr(), FontWeight :: wx:wx_enum().
```

Sets the font weight.

# `setLeftIndent`

```elixir
-spec setLeftIndent(This, Indent) -> ok when This :: wxTextAttr(), Indent :: integer().
```

# `setLeftIndent`

```elixir
-spec setLeftIndent(This, Indent, [Option]) -> ok
                       when This :: wxTextAttr(), Indent :: integer(), Option :: {subIndent, integer()}.
```

Sets the left indent and left subindent in tenths of a millimetre.

The sub-indent is an offset from the left of the paragraph, and is used for all but the
first line in a paragraph.

A positive value will cause the first line to appear to the left of the subsequent lines,
and a negative value will cause the first line to be indented relative to the subsequent lines.

`wxRichTextBuffer` (not implemented in wx) uses indentation to render a bulleted item.
The left indent is the distance between the margin and the bullet. The content of the
paragraph, including the first line, starts at leftMargin + leftSubIndent. So the distance
between the left edge of the bullet and the left of the actual paragraph is leftSubIndent.

# `setRightIndent`

```elixir
-spec setRightIndent(This, Indent) -> ok when This :: wxTextAttr(), Indent :: integer().
```

Sets the right indent in tenths of a millimetre.

# `setTabs`

```elixir
-spec setTabs(This, Tabs) -> ok when This :: wxTextAttr(), Tabs :: [integer()].
```

Sets the tab stops, expressed in tenths of a millimetre.

Each stop is measured from the left margin and therefore each value must be larger than
the last.

# `setTextColour`

```elixir
-spec setTextColour(This, ColText) -> ok when This :: wxTextAttr(), ColText :: wx:wx_colour().
```

Sets the text foreground colour.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
