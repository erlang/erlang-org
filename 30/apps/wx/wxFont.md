# `wxFont`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFont.erl#L58)

A font is an object which determines the appearance of text.

Fonts are used for drawing text to a device context, and setting the appearance of a
window's text, see `wxDC:setFont/2` and `wxWindow:setFont/2`.

The easiest way to create a custom font is to use `wxFontInfo` (not implemented in wx)
object to specify the font attributes and then use `new/5` constructor. Alternatively, you could
start with one of the pre-defined fonts or use `wxWindow:getFont/1` and modify the font, e.g. by increasing
its size using `MakeLarger()` (not implemented in wx) or changing its weight using `MakeBold()`
(not implemented in wx).

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

You can retrieve the current system font settings with `m:wxSystemSettings`.

Predefined objects (include wx.hrl): ?wxNullFont, ?wxNORMAL\_FONT, ?wxSMALL\_FONT,
?wxITALIC\_FONT, ?wxSWISS\_FONT

See:
* [Overview font](https://docs.wxwidgets.org/3.2/overview_font.html#overview_font)

* `wxDC:setFont/2`

* `wxDC:drawText/3`

* `wxDC:getTextExtent/3`

* `m:wxFontDialog`

* `m:wxSystemSettings`

wxWidgets docs: [wxFont](https://docs.wxwidgets.org/3.2/classwx_font.html)

# `wxFont`

```erlang
-type wxFont() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxFont()) -> ok.
```

Destroys the object

# `getDefaultEncoding`

```erlang
-spec getDefaultEncoding() -> wx:wx_enum().
```

Returns the current application's default encoding.

See:
* [Overview fontencoding](https://docs.wxwidgets.org/3.2/overview_fontencoding.html#overview_fontencoding)

* `setDefaultEncoding/1`

# `getFaceName`

```erlang
-spec getFaceName(This) -> unicode:charlist() when This :: wxFont().
```

Returns the face name associated with the font, or the empty string if there is no face
information.

See: `setFaceName/2`

# `getFamily`

```erlang
-spec getFamily(This) -> wx:wx_enum() when This :: wxFont().
```

Gets the font family if possible.

As described in ?wxFontFamily docs the returned value acts as a rough, basic
classification of the main font properties (look, spacing).

If the current font face name is not recognized by `m:wxFont` or by the underlying
system, `wxFONTFAMILY_DEFAULT` is returned.

Note that currently this function is not very precise and so not particularly useful.
Font families mostly make sense only for font creation, see `setFamily/2`.

See: `setFamily/2`

# `getNativeFontInfoDesc`

```erlang
-spec getNativeFontInfoDesc(This) -> unicode:charlist() when This :: wxFont().
```

Returns the platform-dependent string completely describing this font.

Returned string is always non-empty unless the font is invalid (in which case an assert
is triggered).

Note that the returned string is not meant to be shown or edited by the user: a typical
use of this function is for serializing in string-form a `m:wxFont` object.

See: `getNativeFontInfoUserDesc/1`

# `getNativeFontInfoUserDesc`

```erlang
-spec getNativeFontInfoUserDesc(This) -> unicode:charlist() when This :: wxFont().
```

Returns a user-friendly string for this font object.

Returned string is always non-empty unless the font is invalid (in which case an assert
is triggered).

The string does not encode all `m:wxFont` infos under all platforms; e.g. under wxMSW the
font family is not present in the returned string.

Some examples of the formats of returned strings (which are platform-dependent) are in `SetNativeFontInfoUserDesc()`
(not implemented in wx).

See: `getNativeFontInfoDesc/1`

# `getPointSize`

```erlang
-spec getPointSize(This) -> integer() when This :: wxFont().
```

Gets the point size as an integer number.

This function is kept for compatibility reasons. New code should use `GetFractionalPointSize()`
(not implemented in wx) and support fractional point sizes.

See: `setPointSize/2`

# `getStyle`

```erlang
-spec getStyle(This) -> wx:wx_enum() when This :: wxFont().
```

Gets the font style.

See ?wxFontStyle for a list of valid styles.

See: `setStyle/2`

# `getUnderlined`

```erlang
-spec getUnderlined(This) -> boolean() when This :: wxFont().
```

Returns true if the font is underlined, false otherwise.

See: `setUnderlined/2`

# `getWeight`

```erlang
-spec getWeight(This) -> wx:wx_enum() when This :: wxFont().
```

Gets the font weight.

See ?wxFontWeight for a list of valid weight identifiers.

See: `setWeight/2`

# `isFixedWidth`

```erlang
-spec isFixedWidth(This) -> boolean() when This :: wxFont().
```

Returns true if the font is a fixed width (or monospaced) font, false if it is a
proportional one or font is invalid.

Note that this function under some platforms is different from just testing for the font
family being equal to `wxFONTFAMILY_TELETYPE` because native platform-specific functions
are used for the check (resulting in a more accurate return value).

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxFont().
```

Returns true if this object is a valid font, false otherwise.

# `new`

```erlang
-spec new() -> wxFont().
```

Default ctor.

# `new`

```erlang
-spec new(NativeInfoString) -> wxFont() when NativeInfoString :: unicode:chardata();
         (Font) -> wxFont() when Font :: wxFont().
```

Copy constructor, uses reference counting.

# `new`

```erlang
-spec new(PointSize, Family, Style, Weight) -> wxFont()
             when
                 PointSize :: integer(),
                 Family :: wx:wx_enum(),
                 Style :: wx:wx_enum(),
                 Weight :: wx:wx_enum();
         (PixelSize, Family, Style, Weight) -> wxFont()
             when
                 PixelSize :: {W :: integer(), H :: integer()},
                 Family :: wx:wx_enum(),
                 Style :: wx:wx_enum(),
                 Weight :: wx:wx_enum().
```

Equivalent to: `new/5`

# `new`

```erlang
-spec new(PointSize, Family, Style, Weight, [Option]) -> wxFont()
             when
                 PointSize :: integer(),
                 Family :: wx:wx_enum(),
                 Style :: wx:wx_enum(),
                 Weight :: wx:wx_enum(),
                 Option ::
                     {underlined, boolean()} | {face, unicode:chardata()} | {encoding, wx:wx_enum()};
         (PixelSize, Family, Style, Weight, [Option]) -> wxFont()
             when
                 PixelSize :: {W :: integer(), H :: integer()},
                 Family :: wx:wx_enum(),
                 Style :: wx:wx_enum(),
                 Weight :: wx:wx_enum(),
                 Option ::
                     {underline, boolean()} | {faceName, unicode:chardata()} | {encoding, wx:wx_enum()}.
```

Creates a font object with the specified attributes and size in pixels.

Notice that the use of this constructor is often more verbose and less readable than the
use of constructor from `wxFontInfo` (not implemented in wx), consider using that
constructor instead.

Remark: If the desired font does not exist, the closest match will be chosen. Under
Windows, only scalable TrueType fonts are used.

# `ok`

```erlang
-spec ok(This) -> boolean() when This :: wxFont().
```

Equivalent to: `isOk/1`

# `setDefaultEncoding`

```erlang
-spec setDefaultEncoding(Encoding) -> ok when Encoding :: wx:wx_enum().
```

Sets the default font encoding.

See:
* [Overview fontencoding](https://docs.wxwidgets.org/3.2/overview_fontencoding.html#overview_fontencoding)

* `getDefaultEncoding/0`

# `setFaceName`

```erlang
-spec setFaceName(This, FaceName) -> boolean() when This :: wxFont(), FaceName :: unicode:chardata().
```

Sets the facename for the font.

Remark: To avoid portability problems, don't rely on a specific face, but specify the
font family instead (see ?wxFontFamily and `setFamily/2`).

Return: true if the given face name exists; if the face name doesn't exist in the user's
system then the font is invalidated (so that `isOk/1` will return false) and false is returned.

See:
* `getFaceName/1`

* `setFamily/2`

# `setFamily`

```erlang
-spec setFamily(This, Family) -> ok when This :: wxFont(), Family :: wx:wx_enum().
```

Sets the font family.

As described in ?wxFontFamily docs the given `family` value acts as a rough, basic
indication of the main font properties (look, spacing).

Note that changing the font family results in changing the font face name.

See:
* `getFamily/1`

* `setFaceName/2`

# `setPointSize`

```erlang
-spec setPointSize(This, PointSize) -> ok when This :: wxFont(), PointSize :: integer().
```

Sets the font size in points to an integer value.

This is a legacy version of the function only supporting integer point sizes. It can
still be used, but to avoid unnecessarily restricting the font size in points to integer
values, consider using the new (added in wxWidgets 3.1.2) `SetFractionalPointSize()` (not
implemented in wx) function instead.

# `setStyle`

```erlang
-spec setStyle(This, Style) -> ok when This :: wxFont(), Style :: wx:wx_enum().
```

Sets the font style.

See: `getStyle/1`

# `setUnderlined`

```erlang
-spec setUnderlined(This, Underlined) -> ok when This :: wxFont(), Underlined :: boolean().
```

Sets underlining.

See: `getUnderlined/1`

# `setWeight`

```erlang
-spec setWeight(This, Weight) -> ok when This :: wxFont(), Weight :: wx:wx_enum().
```

Sets the font weight.

See: `getWeight/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
