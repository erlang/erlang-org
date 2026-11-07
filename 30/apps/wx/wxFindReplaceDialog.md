# `wxFindReplaceDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFindReplaceDialog.erl#L58)

`m:wxFindReplaceDialog` is a standard modeless dialog which is used to allow the user to
search for some text (and possibly replace it with something else).

The actual searching is supposed to be done in the owner window which is the parent of
this dialog. Note that it means that unlike for the other standard dialogs this one `must`
have a parent window. Also note that there is no way to use this dialog in a modal way; it
is always, by design and implementation, modeless.

Please see the page_samples_dialogs sample for an example of using it.

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFindReplaceDialog](https://docs.wxwidgets.org/3.2/classwx_find_replace_dialog.html)

# `wxFindReplaceDialog`

```erlang
-type wxFindReplaceDialog() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Data, Title) -> boolean()
                when
                    This :: wxFindReplaceDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Data :: wxFindReplaceData:wxFindReplaceData(),
                    Title :: unicode:chardata().
```

# `create`

```erlang
-spec create(This, Parent, Data, Title, [Option]) -> boolean()
                when
                    This :: wxFindReplaceDialog(),
                    Parent :: wxWindow:wxWindow(),
                    Data :: wxFindReplaceData:wxFindReplaceData(),
                    Title :: unicode:chardata(),
                    Option :: {style, integer()}.
```

Creates the dialog; use `wxWindow:show/2` to show it on screen.

The `parent` and `data` parameters must be non-NULL.

# `destroy`

```erlang
-spec destroy(This :: wxFindReplaceDialog()) -> ok.
```

Destroys the object

# `getData`

```erlang
-spec getData(This) -> wxFindReplaceData:wxFindReplaceData() when This :: wxFindReplaceDialog().
```

Get the `m:wxFindReplaceData` object used by this dialog.

# `new`

```erlang
-spec new() -> wxFindReplaceDialog().
```

# `new`

```erlang
-spec new(Parent, Data, Title) -> wxFindReplaceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Data :: wxFindReplaceData:wxFindReplaceData(),
                 Title :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Data, Title, [Option]) -> wxFindReplaceDialog()
             when
                 Parent :: wxWindow:wxWindow(),
                 Data :: wxFindReplaceData:wxFindReplaceData(),
                 Title :: unicode:chardata(),
                 Option :: {style, integer()}.
```

After using default constructor `create/5` must be called.

The `parent` and `data` parameters must be non-NULL.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
