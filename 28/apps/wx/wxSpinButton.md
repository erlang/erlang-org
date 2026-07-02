# `wxSpinButton`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxSpinButton.erl#L58)

A `m:wxSpinButton` has two small up and down (or left and right) arrow buttons.

It is often used next to a text control for increment and decrementing a value. Portable
programs should try to use `m:wxSpinCtrl` instead as `m:wxSpinButton` is not implemented
for all platforms but `m:wxSpinCtrl` is as it degenerates to a simple `m:wxTextCtrl` on
such platforms.

Note: the range supported by this control (and `m:wxSpinCtrl`) depends on the platform
but is at least `-0x8000` to `0x7fff`. Under GTK and Win32 with sufficiently new version
of `comctrl32.dll` (at least 4.71 is required, 5.80 is recommended) the full 32 bit range
is supported.

## Styles

This class supports the following styles:

* wxSP_HORIZONTAL: Specifies a horizontal spin button (note that this style is not
supported in wxGTK).

* wxSP_VERTICAL: Specifies a vertical spin button.

* wxSP_ARROW_KEYS: The user can use arrow keys to change the value.

* wxSP_WRAP: The value wraps at the minimum and maximum.

See: `m:wxSpinCtrl`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSpinButton](https://docs.wxwidgets.org/3.2/classwx_spin_button.html)

## Events

Event types emitted from this class:

* [`spin`](`m:wxSpinEvent`)

* [`spin_up`](`m:wxSpinEvent`)

* [`spin_down`](`m:wxSpinEvent`)

# `wxSpinButton`

```elixir
-type wxSpinButton() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent) -> boolean() when This :: wxSpinButton(), Parent :: wxWindow:wxWindow().
```

# `create`

```elixir
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxSpinButton(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Scrollbar creation function called by the spin button constructor.

See `new/2` for details.

# `destroy`

```elixir
-spec destroy(This :: wxSpinButton()) -> ok.
```

Destroys the object

# `getMax`

```elixir
-spec getMax(This) -> integer() when This :: wxSpinButton().
```

Returns the maximum permissible value.

See: `setRange/3`

# `getMin`

```elixir
-spec getMin(This) -> integer() when This :: wxSpinButton().
```

Returns the minimum permissible value.

See: `setRange/3`

# `getValue`

```elixir
-spec getValue(This) -> integer() when This :: wxSpinButton().
```

Returns the current spin button value.

See: `setValue/2`

# `new`

```elixir
-spec new() -> wxSpinButton().
```

Default constructor.

# `new`

```elixir
-spec new(Parent) -> wxSpinButton() when Parent :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Parent, [Option]) -> wxSpinButton()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating and showing a spin button.

See: `create/3`

# `setRange`

```elixir
-spec setRange(This, Min, Max) -> ok when This :: wxSpinButton(), Min :: integer(), Max :: integer().
```

Sets the range of the spin button.

In portable code, `min` should be less than or equal to `max`. In wxMSW it is possible to
specify minimum greater than maximum and the native control supports the same range as if
they were reversed, but swaps the meaning of up and down arrows, however this dubious
feature is not supported on other platforms.

See:
* `getMin/1`

* `getMax/1`

# `setValue`

```elixir
-spec setValue(This, Value) -> ok when This :: wxSpinButton(), Value :: integer().
```

Sets the value of the spin button.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
