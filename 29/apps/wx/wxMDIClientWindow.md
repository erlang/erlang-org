# `wxMDIClientWindow`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxMDIClientWindow.erl#L58)

An MDI client window is a child of `m:wxMDIParentFrame`, and manages zero or more `m:wxMDIChildFrame`
objects.

The client window is the area where MDI child windows exist. It doesn't have to cover
the whole parent frame; other windows such as toolbars and a help window might coexist
with it. There can be scrollbars on a client window, which are controlled by the parent
window style.

The `m:wxMDIClientWindow` class is usually adequate without further derivation, and it is
created automatically when the MDI parent frame is created. If the application needs to
derive a new class, the function `wxMDIParentFrame::OnCreateClient()` (not implemented in
wx) must be overridden in order to give an opportunity to use a different class of client window.

Under wxMSW, the client window will automatically have a sunken border style when the
active child is not maximized, and no border style when a child is maximized.

See:
* `m:wxMDIChildFrame`

* `m:wxMDIParentFrame`

* `m:wxFrame`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMDIClientWindow](https://docs.wxwidgets.org/3.2/classwx_m_d_i_client_window.html)

# `wxMDIClientWindow`

```erlang
-type wxMDIClientWindow() :: wx:wx_object().
```

# `createClient`

```erlang
-spec createClient(This, Parent) -> boolean()
                      when This :: wxMDIClientWindow(), Parent :: wxMDIParentFrame:wxMDIParentFrame().
```

# `createClient`

```erlang
-spec createClient(This, Parent, [Option]) -> boolean()
                      when
                          This :: wxMDIClientWindow(),
                          Parent :: wxMDIParentFrame:wxMDIParentFrame(),
                          Option :: {style, integer()}.
```

Called by `m:wxMDIParentFrame` immediately after creating the client window.

This function may be overridden in the derived class but the base class version must
usually be called first to really create the window.

# `destroy`

```erlang
-spec destroy(This :: wxMDIClientWindow()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxMDIClientWindow().
```

Default constructor.

Objects of this class are only created by `m:wxMDIParentFrame` which uses the default
constructor and calls `createClient/3` immediately afterwards.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
