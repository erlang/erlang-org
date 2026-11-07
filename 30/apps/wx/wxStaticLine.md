# `wxStaticLine`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStaticLine.erl#L58)

A static line is just a line which may be used in a dialog to separate the groups of
controls.

The line may be only vertical or horizontal. Moreover, not all ports (notably not wxGTK)
support specifying the transversal direction of the line (e.g. height for a horizontal
line) so for maximal portability you should specify it as wxDefaultCoord.

## Styles

This class supports the following styles:

* wxLI_HORIZONTAL: Creates a horizontal line.

* wxLI_VERTICAL: Creates a vertical line.

See: `m:wxStaticBox`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStaticLine](https://docs.wxwidgets.org/3.2/classwx_static_line.html)

# `wxStaticLine`

```erlang
-type wxStaticLine() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxStaticLine(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxStaticLine(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates the static line for two-step construction.

See `new/2` for further details.

# `destroy`

```erlang
-spec destroy(This :: wxStaticLine()) -> ok.
```

Destroys the object

# `getDefaultSize`

```erlang
-spec getDefaultSize() -> integer().
```

This static function returns the size which will be given to the smaller dimension of the
static line, i.e.

its height for a horizontal line or its width for a vertical one.

# `isVertical`

```erlang
-spec isVertical(This) -> boolean() when This :: wxStaticLine().
```

Returns true if the line is vertical, false if horizontal.

# `new`

```erlang
-spec new() -> wxStaticLine().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxStaticLine() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxStaticLine()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating and showing a static line.

See: `create/3`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
