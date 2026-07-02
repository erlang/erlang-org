# `wxSplashScreen`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxSplashScreen.erl#L58)

`m:wxSplashScreen` shows a window with a thin border, displaying a bitmap describing your
application.

Show it in application initialisation, and then either explicitly destroy it or let it time-out.

Example usage:

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSplashScreen](https://docs.wxwidgets.org/3.2/classwx_splash_screen.html)

# `wxSplashScreen`

```elixir
-type wxSplashScreen() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxSplashScreen()) -> ok.
```

Destroys the object

# `getSplashStyle`

```elixir
-spec getSplashStyle(This) -> integer() when This :: wxSplashScreen().
```

Returns the splash style (see `new/6` for details).

# `getTimeout`

```elixir
-spec getTimeout(This) -> integer() when This :: wxSplashScreen().
```

Returns the timeout in milliseconds.

# `new`

```elixir
-spec new(Bitmap, SplashStyle, Milliseconds, Parent, Id) -> wxSplashScreen()
             when
                 Bitmap :: wxBitmap:wxBitmap(),
                 SplashStyle :: integer(),
                 Milliseconds :: integer(),
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer().
```

# `new`

```elixir
-spec new(Bitmap, SplashStyle, Milliseconds, Parent, Id, [Option]) -> wxSplashScreen()
             when
                 Bitmap :: wxBitmap:wxBitmap(),
                 SplashStyle :: integer(),
                 Milliseconds :: integer(),
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Construct the splash screen passing a bitmap, a style, a timeout, a window id, optional
position and size, and a window style.

`splashStyle` is a bitlist of some of the following:

* wxSPLASH_CENTRE_ON_PARENT

* wxSPLASH_CENTRE_ON_SCREEN

* wxSPLASH_NO_CENTRE

* wxSPLASH_TIMEOUT

* wxSPLASH_NO_TIMEOUT

`milliseconds` is the timeout in milliseconds.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
