# `wxProgressDialog`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxProgressDialog.erl#L58)

If supported by the platform this class will provide the platform's native progress
dialog, else it will simply be the `wxGenericProgressDialog` (not implemented in wx).

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxProgressDialog](https://docs.wxwidgets.org/3.2/classwx_progress_dialog.html)

# `wxProgressDialog`

```erlang
-type wxProgressDialog() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxProgressDialog()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Title, Message) -> wxProgressDialog()
             when Title :: unicode:chardata(), Message :: unicode:chardata().
```

# `new`

```erlang
-spec new(Title, Message, [Option]) -> wxProgressDialog()
             when
                 Title :: unicode:chardata(),
                 Message :: unicode:chardata(),
                 Option :: {maximum, integer()} | {parent, wxWindow:wxWindow()} | {style, integer()}.
```

# `resume`

```erlang
-spec resume(This) -> ok when This :: wxProgressDialog().
```

Can be used to continue with the dialog, after the user had clicked the "Abort" button.

# `update`

```erlang
-spec update(This, Value) -> boolean() when This :: wxProgressDialog(), Value :: integer().
```

# `update`

```erlang
-spec update(This, Value, [Option]) -> boolean()
                when
                    This :: wxProgressDialog(),
                    Value :: integer(),
                    Option :: {newmsg, unicode:chardata()}.
```

Updates the dialog, setting the progress bar to the new value and updating the message if
new one is specified.

Returns true unless the "Cancel" button has been pressed.

If false is returned, the application can either immediately destroy the dialog or ask
the user for the confirmation and if the abort is not confirmed the dialog may be resumed
with `resume/1` function.

If `value` is the maximum value for the dialog, the behaviour of the function depends on
whether `wxPD_AUTO_HIDE` was used when the dialog was created. If it was, the dialog is
hidden and the function returns immediately. If it was not, the dialog becomes a modal
dialog and waits for the user to dismiss it, meaning that this function does not return
until this happens.

Notice that if `newmsg` is longer than the currently shown message, the dialog will be
automatically made wider to account for it. However if the new message is shorter than the
previous one, the dialog doesn't shrink back to avoid constant resizes if the message is
changed often. To do this and fit the dialog to its current contents you may call `wxWindow:fit/1`
explicitly. However the native MSW implementation of this class does make the dialog
shorter if the new text has fewer lines of text than the old one, so it is recommended to
keep the number of lines of text constant in order to avoid jarring dialog size changes.
You may also want to make the initial message, specified when creating the dialog, wide
enough to avoid having to resize the dialog later, e.g. by appending a long string of
unbreakable spaces (`wxString` (not implemented in wx)(L'\u00a0', 100)) to it.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
