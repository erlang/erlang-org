# `wxPen`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPen.erl#L58)

A pen is a drawing tool for drawing outlines.

It is used for drawing lines and painting the outline of rectangles, ellipses, etc. It
has a colour, a width and a style.

Note: On a monochrome display, wxWidgets shows all non-white pens as black.

Do not initialize objects on the stack before the program commences, since other
required structures may not have been set up yet. Instead, define global pointers to
objects and create them in `wxApp::OnInit()` (not implemented in wx) or when required.

An application may wish to dynamically create pens with different characteristics, and
there is the consequent danger that a large number of duplicate pens will be created.
Therefore an application may wish to get a pointer to a pen by using the global list of
pens ?wxThePenList, and calling the member function `wxPenList::FindOrCreatePen()` (not
implemented in wx). See `wxPenList` (not implemented in wx) for more info.

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

Predefined objects (include wx.hrl):

* ?wxNullPen

* ?wxBLACK\_DASHED\_PEN

* ?wxBLACK\_PEN

* ?wxBLUE\_PEN

* ?wxCYAN\_PEN

* ?wxGREEN\_PEN

* ?wxYELLOW\_PEN

* ?wxGREY\_PEN

* ?wxLIGHT\_GREY\_PEN

* ?wxMEDIUM\_GREY\_PEN

* ?wxRED\_PEN

* ?wxTRANSPARENT\_PEN

* ?wxWHITE\_PEN

See:
* `m:wxDC`

* `wxDC:setPen/2`

wxWidgets docs: [wxPen](https://docs.wxwidgets.org/3.2/classwx_pen.html)

# `wxPen`

```erlang
-type wxPen() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPen()) -> ok.
```

Destroys the object

# `getCap`

```erlang
-spec getCap(This) -> wx:wx_enum() when This :: wxPen().
```

Returns the pen cap style, which may be one of `wxCAP\_ROUND`, `wxCAP\_PROJECTING` and `wxCAP\_BUTT`.

The default is `wxCAP_ROUND`.

See: `setCap/2`

# `getColour`

```erlang
-spec getColour(This) -> wx:wx_colour4() when This :: wxPen().
```

Returns a reference to the pen colour.

See: `setColour/4`

# `getJoin`

```erlang
-spec getJoin(This) -> wx:wx_enum() when This :: wxPen().
```

Returns the pen join style, which may be one of `wxJOIN\_BEVEL`, `wxJOIN\_ROUND` and `wxJOIN\_MITER`.

The default is `wxJOIN_ROUND`.

See: `setJoin/2`

# `getStyle`

```erlang
-spec getStyle(This) -> wx:wx_enum() when This :: wxPen().
```

Returns the pen style.

See:
* `new/2`

* `setStyle/2`

# `getWidth`

```erlang
-spec getWidth(This) -> integer() when This :: wxPen().
```

Returns the pen width.

See: `setWidth/2`

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxPen().
```

Returns true if the pen is initialised.

Notice that an uninitialized pen object can't be queried for any pen properties and all
calls to the accessor methods on it will result in an assert failure.

# `new`

```erlang
-spec new() -> wxPen().
```

Default constructor.

The pen will be uninitialised, and `isOk/1` will return false.

# `new`

```erlang
-spec new(Colour) -> wxPen() when Colour :: wx:wx_colour();
         (Pen) -> wxPen() when Pen :: wxPen().
```

Copy constructor, uses overview_refcount.

# `new`

```erlang
-spec new(Colour, [Option]) -> wxPen()
             when Colour :: wx:wx_colour(), Option :: {width, integer()} | {style, wx:wx_enum()}.
```

Constructs a pen from a colour object, pen width and style.

Remark: Different versions of Windows and different versions of other platforms support
very different subsets of the styles above so handle with care.

See:
* `setStyle/2`

* `setColour/4`

* `setWidth/2`

# `setCap`

```erlang
-spec setCap(This, CapStyle) -> ok when This :: wxPen(), CapStyle :: wx:wx_enum().
```

Sets the pen cap style, which may be one of `wxCAP\_ROUND`, `wxCAP\_PROJECTING` and `wxCAP\_BUTT`.

The default is `wxCAP_ROUND`.

See: `getCap/1`

# `setColour`

```erlang
-spec setColour(This, Colour) -> ok when This :: wxPen(), Colour :: wx:wx_colour().
```

The pen's colour is changed to the given colour.

See: `getColour/1`

# `setColour`

```erlang
-spec setColour(This, Red, Green, Blue) -> ok
                   when This :: wxPen(), Red :: integer(), Green :: integer(), Blue :: integer().
```

# `setJoin`

```erlang
-spec setJoin(This, Join_style) -> ok when This :: wxPen(), Join_style :: wx:wx_enum().
```

Sets the pen join style, which may be one of `wxJOIN\_BEVEL`, `wxJOIN\_ROUND` and `wxJOIN\_MITER`.

The default is `wxJOIN_ROUND`.

See: `getJoin/1`

# `setStyle`

```erlang
-spec setStyle(This, Style) -> ok when This :: wxPen(), Style :: wx:wx_enum().
```

Set the pen style.

See: `new/2`

# `setWidth`

```erlang
-spec setWidth(This, Width) -> ok when This :: wxPen(), Width :: integer().
```

Sets the pen width.

See: `getWidth/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
