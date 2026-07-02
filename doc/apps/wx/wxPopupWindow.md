# `wxPopupWindow`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxPopupWindow.erl#L58)

A special kind of top level window used for popup menus, combobox popups and such.

## Styles

This class supports the following styles:

* wxPU_CONTAINS_CONTROLS: By default in wxMSW, a popup window will not take focus from its
parent window. However many standard controls, including common ones such as `m:wxTextCtrl`,
need focus to function correctly and will not work when placed on a default popup. This
flag can be used to make the popup take focus and let all controls work but at the price
of not allowing the parent window to keep focus while the popup is shown, which can also
be sometimes desirable. This style is currently only implemented in MSW and simply does
nothing under the other platforms (it's new since wxWidgets 3.1.3).

See:
* `m:wxDialog`

* `m:wxFrame`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPopupWindow](https://docs.wxwidgets.org/3.2/classwx_popup_window.html)

# `wxPopupWindow`

```erlang
-type wxPopupWindow() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxPopupWindow(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxPopupWindow(), Parent :: wxWindow:wxWindow(), Option :: {flags, integer()}.
```

Create method for two-step creation.

# `destroy`

```erlang
-spec destroy(This :: wxPopupWindow()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxPopupWindow().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxPopupWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxPopupWindow()
             when Parent :: wxWindow:wxWindow(), Option :: {flags, integer()}.
```

Constructor.

# `position`

```erlang
-spec position(This, PtOrigin, SizePopup) -> ok
                  when
                      This :: wxPopupWindow(),
                      PtOrigin :: {X :: integer(), Y :: integer()},
                      SizePopup :: {W :: integer(), H :: integer()}.
```

Move the popup window to the right position, i.e. such that it is entirely visible.

The popup is positioned at ptOrigin + size if it opens below and to the right (default),
at ptOrigin - sizePopup if it opens above and to the left etc.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
