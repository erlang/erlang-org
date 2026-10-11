# `wxStatusBar`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStatusBar.erl#L58)

A status bar is a narrow window that can be placed along the bottom of a frame to give
small amounts of status information.

It can contain one or more fields, one or more of which can be variable length according
to the size of the window.

`m:wxStatusBar` also maintains an independent stack of status texts for each field (see `pushStatusText/3`
and `popStatusText/2`).

Note that in `m:wxStatusBar` context, the terms `pane` and `field` are synonyms.

## Styles

This class supports the following styles:

* wxSTB_SIZEGRIP: Displays a gripper at the right-hand side of the status bar which can be
used to resize the parent window.

* wxSTB_SHOW_TIPS: Displays tooltips for those panes whose status text has been
ellipsized/truncated because the status text doesn't fit the pane width. Note that this
style has effect only on wxGTK (with GTK+ >= 2.12) currently.

* wxSTB_ELLIPSIZE_START: Replace the beginning of the status texts with an ellipsis when
the status text widths exceed the status bar pane's widths (uses `wxControl::Ellipsize`
(not implemented in wx)).

* wxSTB_ELLIPSIZE_MIDDLE: Replace the middle of the status texts with an ellipsis when the
status text widths exceed the status bar pane's widths (uses `wxControl::Ellipsize` (not
implemented in wx)).

* wxSTB_ELLIPSIZE_END: Replace the end of the status texts with an ellipsis when the status
text widths exceed the status bar pane's widths (uses `wxControl::Ellipsize` (not
implemented in wx)).

* wxSTB_DEFAULT_STYLE: The default style: includes `wxSTB_SIZEGRIP|wxSTB_SHOW_TIPS|wxSTB_ELLIPSIZE_END|wxFULL_REPAINT_ON_RESIZE`.

Remark: It is possible to create controls and other windows on the status bar. Position
these windows from an OnSize() event handler.

Remark: Notice that only the first 127 characters of a string will be shown in status bar
fields under Windows if a proper manifest indicating that the program uses version 6 of
common controls library is not used. This is a limitation of the native control on these platforms.

See:
* `m:wxFrame`

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_statbar)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStatusBar](https://docs.wxwidgets.org/3.2/classwx_status_bar.html)

# `wxStatusBar`

```erlang
-type wxStatusBar() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxStatusBar(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxStatusBar(),
                    Parent :: wxWindow:wxWindow(),
                    Option :: {winid, integer()} | {style, integer()}.
```

Creates the window, for two-step construction.

See `new/2` for details.

# `destroy`

```erlang
-spec destroy(This :: wxStatusBar()) -> ok.
```

Destroys the object

# `getFieldRect`

```erlang
-spec getFieldRect(This, I) -> Result
                      when
                          Result ::
                              {Res :: boolean(),
                               Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}},
                          This :: wxStatusBar(),
                          I :: integer().
```

Returns the size and position of a field's internal bounding rectangle.

Return: true if the field index is valid, false otherwise.

See: {X,Y,W,H}

# `getFieldsCount`

```erlang
-spec getFieldsCount(This) -> integer() when This :: wxStatusBar().
```

Returns the number of fields in the status bar.

# `getStatusText`

```erlang
-spec getStatusText(This) -> unicode:charlist() when This :: wxStatusBar().
```

# `getStatusText`

```erlang
-spec getStatusText(This, [Option]) -> unicode:charlist()
                       when This :: wxStatusBar(), Option :: {number, integer()}.
```

Returns the string associated with a status bar field.

Return: The status field string if the field is valid, otherwise the empty string.

See: `setStatusText/3`

# `new`

```erlang
-spec new() -> wxStatusBar().
```

Default ctor.

# `new`

```erlang
-spec new(Parent) -> wxStatusBar() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxStatusBar()
             when Parent :: wxWindow:wxWindow(), Option :: {winid, integer()} | {style, integer()}.
```

Constructor, creating the window.

See: `create/3`

# `popStatusText`

```erlang
-spec popStatusText(This) -> ok when This :: wxStatusBar().
```

# `popStatusText`

```erlang
-spec popStatusText(This, [Option]) -> ok when This :: wxStatusBar(), Option :: {number, integer()}.
```

Restores the text to the value it had before the last call to `pushStatusText/3`.

Notice that if `setStatusText/3` had been called in the meanwhile, `popStatusText/2` will not change the text, i.e. it does
not override explicit changes to status text but only restores the saved text if it hadn't
been changed since.

See: `pushStatusText/3`

# `pushStatusText`

```erlang
-spec pushStatusText(This, String) -> ok when This :: wxStatusBar(), String :: unicode:chardata().
```

# `pushStatusText`

```erlang
-spec pushStatusText(This, String, [Option]) -> ok
                        when
                            This :: wxStatusBar(),
                            String :: unicode:chardata(),
                            Option :: {number, integer()}.
```

Saves the current field text in a per-field stack, and sets the field text to the string
passed as argument.

See: `popStatusText/2`

# `setFieldsCount`

```erlang
-spec setFieldsCount(This, Number) -> ok when This :: wxStatusBar(), Number :: integer().
```

# `setFieldsCount`

```erlang
-spec setFieldsCount(This, Number, [Option]) -> ok
                        when This :: wxStatusBar(), Number :: integer(), Option :: {widths, [integer()]}.
```

Sets the number of fields, and optionally the field widths.

# `setMinHeight`

```erlang
-spec setMinHeight(This, Height) -> ok when This :: wxStatusBar(), Height :: integer().
```

Sets the minimal possible height for the status bar.

The real height may be bigger than the height specified here depending on the size of the
font used by the status bar.

# `setStatusStyles`

```erlang
-spec setStatusStyles(This, Styles) -> ok when This :: wxStatusBar(), Styles :: [integer()].
```

Sets the styles of the fields in the status line which can make fields appear flat or
raised instead of the standard sunken 3D border.

# `setStatusText`

```erlang
-spec setStatusText(This, Text) -> ok when This :: wxStatusBar(), Text :: unicode:chardata().
```

# `setStatusText`

```erlang
-spec setStatusText(This, Text, [Option]) -> ok
                       when
                           This :: wxStatusBar(),
                           Text :: unicode:chardata(),
                           Option :: {number, integer()}.
```

Sets the status text for the `i-th` field.

The given text will replace the current text. The display of the status bar is updated
immediately, so there is no need to call `wxWindow:update/1` after calling this function.

Note that if `pushStatusText/3` had been called before the new text will also replace the last saved value
to make sure that the next call to `popStatusText/2` doesn't restore the old value, which was overwritten
by the call to this function.

See:
* `getStatusText/2`

* `wxFrame:setStatusText/3`

# `setStatusWidths`

```erlang
-spec setStatusWidths(This, Widths_field) -> ok when This :: wxStatusBar(), Widths_field :: [integer()].
```

Sets the widths of the fields in the status line.

There are two types of fields: `fixed` widths and `variable` width fields. For the fixed
width fields you should specify their (constant) width in pixels. For the variable width
fields, specify a negative number which indicates how the field should expand: the space
left for all variable width fields is divided between them according to the absolute value
of this number. A variable width field with width of -2 gets twice as much of it as a
field with width -1 and so on.

For example, to create one fixed width field of width 100 in the right part of the status
bar and two more fields which get 66% and 33% of the remaining space correspondingly, you
should use an array containing -2, -1 and 100.

Remark: The widths of the variable fields are calculated from the total width of all
fields, minus the sum of widths of the non-variable fields, divided by the number of
variable fields.

See:
* `setFieldsCount/3`

* `wxFrame:setStatusWidths/2`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
