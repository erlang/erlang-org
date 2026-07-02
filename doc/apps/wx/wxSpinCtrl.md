# `wxSpinCtrl`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSpinCtrl.erl#L58)

`m:wxSpinCtrl` combines `m:wxTextCtrl` and `m:wxSpinButton` in one control.

## Styles

This class supports the following styles:

* wxSP_ARROW_KEYS: The user can use arrow keys to change the value.

* wxSP_WRAP: The value wraps at the minimum and maximum.

* wxTE_PROCESS_ENTER: Indicates that the control should generate `wxEVT_TEXT_ENTER` events.
Using this style will prevent the user from using the Enter key for dialog navigation
(e.g. activating the default button in the dialog) under MSW.

* wxALIGN_LEFT: Same as wxTE_LEFT for `m:wxTextCtrl`: the text is left aligned (this is the
default).

* wxALIGN_CENTRE_HORIZONTAL: Same as wxTE_CENTRE for `m:wxTextCtrl`: the text is centered.

* wxALIGN_RIGHT: Same as wxTE_RIGHT for `m:wxTextCtrl`: the text is right aligned.

See:
* `m:wxSpinButton`

* `m:wxControl`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSpinCtrl](https://docs.wxwidgets.org/3.2/classwx_spin_ctrl.html)

## Events

Event types emitted from this class:

* [`command_spinctrl_updated`](`m:wxSpinEvent`)

# `wxSpinCtrl`

```erlang
-type wxSpinCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxSpinCtrl(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxSpinCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {value, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {min, integer()} |
                        {max, integer()} |
                        {initial, integer()}.
```

Creation function called by the spin control constructor.

See `new/2` for details.

# `destroy`

```erlang
-spec destroy(This :: wxSpinCtrl()) -> ok.
```

Destroys the object

# `getMax`

```erlang
-spec getMax(This) -> integer() when This :: wxSpinCtrl().
```

Gets maximal allowable value.

# `getMin`

```erlang
-spec getMin(This) -> integer() when This :: wxSpinCtrl().
```

Gets minimal allowable value.

# `getValue`

```erlang
-spec getValue(This) -> integer() when This :: wxSpinCtrl().
```

Gets the value of the spin control.

# `new`

```erlang
-spec new() -> wxSpinCtrl().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxSpinCtrl() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxSpinCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {value, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {min, integer()} |
                     {max, integer()} |
                     {initial, integer()}.
```

Constructor, creating and showing a spin control.

If `value` is non-empty, it will be shown in the text entry part of the control and if it
has numeric value, the initial numeric value of the control, as returned by `getValue/1` will also be
determined by it instead of by `initial`. Hence, it only makes sense to specify `initial`
if `value` is an empty string or is not convertible to a number, otherwise `initial` is
simply ignored and the number specified by `value` is used.

See: `create/3`

# `setRange`

```erlang
-spec setRange(This, MinVal, MaxVal) -> ok
                  when This :: wxSpinCtrl(), MinVal :: integer(), MaxVal :: integer().
```

Sets range of allowable values.

Notice that calling this method may change the value of the control if it's not inside
the new valid range, e.g. it will become `minVal` if it is less than it now. However no `wxEVT_SPINCTRL`
event is generated, even if it the value does change.

Note: Setting a range including negative values is silently ignored if current base is
set to 16.

# `setSelection`

```erlang
-spec setSelection(This, From, To) -> ok when This :: wxSpinCtrl(), From :: integer(), To :: integer().
```

Select the text in the text part of the control between positions `from` (inclusive) and `to`
(exclusive).

This is similar to `wxTextCtrl:setSelection/3`.

Note: this is currently only implemented for Windows and generic versions of the control.

# `setValue`

```erlang
-spec setValue(This, Value) -> ok when This :: wxSpinCtrl(), Value :: integer();
              (This, Text) -> ok when This :: wxSpinCtrl(), Text :: unicode:chardata().
```

Sets the value of the spin control.

It is recommended to use the overload taking an integer value instead.

Notice that, unlike `wxTextCtrl:setValue/2`, but like most of the other setter methods in wxWidgets, calling
this method does not generate any events as events are only generated for the user
actions.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
