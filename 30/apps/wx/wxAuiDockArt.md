# `wxAuiDockArt`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxAuiDockArt.erl#L58)

`m:wxAuiDockArt` is part of the wxAUI class framework.

See also overview_aui.

`m:wxAuiDockArt` is the art provider: provides all drawing functionality to the wxAui
dock manager. This allows the dock manager to have a pluggable look-and-feel.

By default, a `m:wxAuiManager` uses an instance of this class called `wxAuiDefaultDockArt`
(not implemented in wx) which provides bitmap art and a colour scheme that is adapted to
the major platforms' look. You can either derive from that class to alter its behaviour or
write a completely new dock art class. Call `wxAuiManager:setArtProvider/2` to force wxAUI to use your new dock art provider.

See:
* `m:wxAuiManager`

* `m:wxAuiPaneInfo`

wxWidgets docs: [wxAuiDockArt](https://docs.wxwidgets.org/3.2/classwx_aui_dock_art.html)

# `wxAuiDockArt`

```erlang
-type wxAuiDockArt() :: wx:wx_object().
```

# `getColour`

```erlang
-spec getColour(This, Id) -> wx:wx_colour4() when This :: wxAuiDockArt(), Id :: integer().
```

Get the colour of a certain setting.

`id` can be one of the colour values of `wxAuiPaneDockArtSetting`.

# `getFont`

```erlang
-spec getFont(This, Id) -> wxFont:wxFont() when This :: wxAuiDockArt(), Id :: integer().
```

Get a font setting.

# `getMetric`

```erlang
-spec getMetric(This, Id) -> integer() when This :: wxAuiDockArt(), Id :: integer().
```

Get the value of a certain setting.

`id` can be one of the size values of `wxAuiPaneDockArtSetting`.

# `setColour`

```erlang
-spec setColour(This, Id, Colour) -> ok
                   when This :: wxAuiDockArt(), Id :: integer(), Colour :: wx:wx_colour().
```

Set a certain setting with the value `colour`.

`id` can be one of the colour values of `wxAuiPaneDockArtSetting`.

# `setFont`

```erlang
-spec setFont(This, Id, Font) -> ok
                 when This :: wxAuiDockArt(), Id :: integer(), Font :: wxFont:wxFont().
```

Set a font setting.

# `setMetric`

```erlang
-spec setMetric(This, Id, New_val) -> ok
                   when This :: wxAuiDockArt(), Id :: integer(), New_val :: integer().
```

Set a certain setting with the value `new\_val`.

`id` can be one of the size values of `wxAuiPaneDockArtSetting`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
