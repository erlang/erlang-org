# `wxMiniFrame`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxMiniFrame.erl#L58)

A miniframe is a frame with a small title bar.

It is suitable for floating toolbars that must not take up too much screen area.

An example of mini frame can be seen in the page_samples_dialogs using the "Mini frame"
command of the "Generic dialogs" submenu.

## Styles

This class supports the following styles:

* wxICONIZE: Display the frame iconized (minimized) (Windows only).

* wxCAPTION: Puts a caption on the frame.

* wxMINIMIZE: Identical to wxICONIZE.

* wxMINIMIZE_BOX: Displays a minimize box on the frame (Windows and Motif only).

* wxMAXIMIZE: Displays the frame maximized (Windows only).

* wxMAXIMIZE_BOX: Displays a maximize box on the frame (Windows and Motif only).

* wxCLOSE_BOX: Displays a close box on the frame.

* wxSTAY_ON_TOP: Stay on top of other windows (Windows only).

* wxSYSTEM_MENU: Displays a system menu (Windows and Motif only).

* wxRESIZE_BORDER: Displays a resizable border around the window.

Remark: This class has miniframe functionality under Windows and GTK, i.e. the presence
of mini frame will not be noted in the task bar and focus behaviour is different. On other
platforms, it behaves like a normal frame.

See:
* `m:wxMDIParentFrame`

* `m:wxMDIChildFrame`

* `m:wxFrame`

* `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMiniFrame](https://docs.wxwidgets.org/3.2/classwx_mini_frame.html)

# `wxMiniFrame`

```erlang
-type wxMiniFrame() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Title) -> boolean()
                when
                    This :: wxMiniFrame(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata().
```

# `create`

```erlang
-spec create(This, Parent, Id, Title, [Option]) -> boolean()
                when
                    This :: wxMiniFrame(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Used in two-step frame construction.

See `new/4` for further details.

# `destroy`

```erlang
-spec destroy(This :: wxMiniFrame()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxMiniFrame().
```

Default ctor.

# `new`

```erlang
-spec new(Parent, Id, Title) -> wxMiniFrame()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Title :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Id, Title, [Option]) -> wxMiniFrame()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Title :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating the window.

Remark: The frame behaves like a normal frame on non-Windows platforms.

See: `create/5`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
