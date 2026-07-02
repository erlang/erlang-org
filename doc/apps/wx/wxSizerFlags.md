# `wxSizerFlags`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSizerFlags.erl#L58)

Container for sizer items flags providing readable names for them.

Normally, when you add an item to a sizer via `wxSizer:add/4`, you have to specify a lot of flags and
parameters which can be unwieldy. This is where `m:wxSizerFlags` comes in: it allows you
to specify all parameters using the named methods instead. For example, instead of

you can now write

This is more readable and also allows you to create `m:wxSizerFlags` objects which can be
reused for several sizer items.

Note that by specification, all methods of `m:wxSizerFlags` return the `m:wxSizerFlags`
object itself to allowing chaining multiple methods calls like in the examples above.

See: `m:wxSizer`

wxWidgets docs: [wxSizerFlags](https://docs.wxwidgets.org/3.2/classwx_sizer_flags.html)

# `wxSizerFlags`

```erlang
-type wxSizerFlags() :: wx:wx_object().
```

# `align`

```erlang
-spec align(This, Alignment) -> wxSizerFlags() when This :: wxSizerFlags(), Alignment :: integer().
```

Sets the alignment of this `m:wxSizerFlags` to `align`.

This method replaces the previously set alignment with the specified one.

See:
* `left/1`

* `right/1`

* `centre/1`

# `border`

```erlang
-spec border(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

# `border`

```erlang
-spec border(This, [Option]) -> wxSizerFlags()
                when This :: wxSizerFlags(), Option :: {direction, integer()}.
```

Sets the `m:wxSizerFlags` to have a border with size as returned by `GetDefaultBorder()`
(not implemented in wx).

# `border`

```erlang
-spec border(This, Direction, Borderinpixels) -> wxSizerFlags()
                when This :: wxSizerFlags(), Direction :: integer(), Borderinpixels :: integer().
```

Sets the `m:wxSizerFlags` to have a border of a number of pixels specified by `borderinpixels`
with the directions specified by `direction`.

Prefer to use the overload below or `DoubleBorder()` (not implemented in wx) or `TripleBorder()`
(not implemented in wx) versions instead of hard-coding the border value in pixels to
avoid too small borders on devices with high DPI displays.

# `center`

```erlang
-spec center(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

Sets the object of the `m:wxSizerFlags` to center itself in the area it is given.

# `centre`

```erlang
-spec centre(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

Equivalent to: `center/1`

# `destroy`

```erlang
-spec destroy(This :: wxSizerFlags()) -> ok.
```

Destroys the object

# `expand`

```erlang
-spec expand(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

Sets the object of the `m:wxSizerFlags` to expand to fill as much area as it can.

# `left`

```erlang
-spec left(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

Aligns the object to the left, similar for `Align(wxALIGN\_LEFT)`.

Unlike `align/2`, this method doesn't change the vertical alignment of the item.

# `new`

```erlang
-spec new() -> wxSizerFlags().
```

# `new`

```erlang
-spec new([Option]) -> wxSizerFlags() when Option :: {proportion, integer()}.
```

Creates the `m:wxSizer` with the proportion specified by `proportion`.

# `proportion`

```erlang
-spec proportion(This, Proportion) -> wxSizerFlags()
                    when This :: wxSizerFlags(), Proportion :: integer().
```

Sets the proportion of this `m:wxSizerFlags` to `proportion`.

# `right`

```erlang
-spec right(This) -> wxSizerFlags() when This :: wxSizerFlags().
```

Aligns the object to the right, similar for `Align(wxALIGN\_RIGHT)`.

Unlike `align/2`, this method doesn't change the vertical alignment of the item.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
