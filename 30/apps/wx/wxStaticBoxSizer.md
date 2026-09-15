# `wxStaticBoxSizer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStaticBoxSizer.erl#L58)

`m:wxStaticBoxSizer` is a sizer derived from `m:wxBoxSizer` but adds a static box around
the sizer.

The static box may be either created independently or the sizer may create it itself as a
convenience. In any case, the sizer owns the `m:wxStaticBox` control and will delete it in
the `m:wxStaticBoxSizer` destructor.

Note that since wxWidgets 2.9.1 you are encouraged to create the windows which are added
to `m:wxStaticBoxSizer` as children of `m:wxStaticBox` itself, see this class
documentation for more details.

Example of use of this class:

See:
* `m:wxSizer`

* `m:wxStaticBox`

* `m:wxBoxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxBoxSizer`

* `m:wxSizer`

wxWidgets docs: [wxStaticBoxSizer](https://docs.wxwidgets.org/3.2/classwx_static_box_sizer.html)

# `wxStaticBoxSizer`

```erlang
-type wxStaticBoxSizer() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxStaticBoxSizer()) -> ok.
```

Destroys the object

# `getStaticBox`

```erlang
-spec getStaticBox(This) -> wxStaticBox:wxStaticBox() when This :: wxStaticBoxSizer().
```

Returns the static box associated with the sizer.

# `new`

```erlang
-spec new(Orient, Parent) -> wxStaticBoxSizer() when Orient :: integer(), Parent :: wxWindow:wxWindow();
         (Box, Orient) -> wxStaticBoxSizer() when Box :: wxStaticBox:wxStaticBox(), Orient :: integer().
```

This constructor uses an already existing static box.

# `new`

```erlang
-spec new(Orient, Parent, [Option]) -> wxStaticBoxSizer()
             when
                 Orient :: integer(),
                 Parent :: wxWindow:wxWindow(),
                 Option :: {label, unicode:chardata()}.
```

This constructor creates a new static box with the given label and parent window.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
