# `wxStaticBox`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxStaticBox.erl#L58)

A static box is a rectangle drawn around other windows to denote a logical grouping of
items.

Note that while the previous versions required that windows appearing inside a static box
be created as its siblings (i.e. use the same parent as the static box itself), since
wxWidgets 2.9.1 it is also possible to create them as children of `m:wxStaticBox` itself
and you are actually encouraged to do it like this if compatibility with the previous
versions is not important.

So the new recommended way to create static box is:

While the compatible - and now deprecated - way is

Also note that there is a specialized `m:wxSizer` class (`m:wxStaticBoxSizer`) which can
be used as an easier way to pack items into a static box.

See:
* `m:wxStaticText`

* `m:wxStaticBoxSizer`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStaticBox](https://docs.wxwidgets.org/3.2/classwx_static_box.html)

# `wxStaticBox`

```elixir
-type wxStaticBox() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxStaticBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxStaticBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates the static box for two-step construction.

See `new/4` for further details.

# `destroy`

```elixir
-spec destroy(This :: wxStaticBox()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new() -> wxStaticBox().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id, Label) -> wxStaticBox()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Label, [Option]) -> wxStaticBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating and showing a static box.

See: `create/5`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
