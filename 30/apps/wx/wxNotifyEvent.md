# `wxNotifyEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxNotifyEvent.erl#L58)

This class is not used by the event handlers by itself, but is a base class for other
event classes (such as `m:wxBookCtrlEvent`).

It (or an object of a derived class) is sent when the controls state is being changed and
allows the program to `veto/1` this change if it wants to prevent it from happening.

See: `m:wxBookCtrlEvent`

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxNotifyEvent](https://docs.wxwidgets.org/3.2/classwx_notify_event.html)

# `wxNotifyEvent`

```erlang
-type wxNotifyEvent() :: wx:wx_object().
```

# `allow`

```erlang
-spec allow(This) -> ok when This :: wxNotifyEvent().
```

This is the opposite of `veto/1`: it explicitly allows the event to be processed.

For most events it is not necessary to call this method as the events are allowed anyhow
but some are forbidden by default (this will be mentioned in the corresponding event
description).

# `isAllowed`

```erlang
-spec isAllowed(This) -> boolean() when This :: wxNotifyEvent().
```

Returns true if the change is allowed (`veto/1` hasn't been called) or false otherwise
(if it was).

# `veto`

```erlang
-spec veto(This) -> ok when This :: wxNotifyEvent().
```

Prevents the change announced by this event from happening.

It is in general a good idea to notify the user about the reasons for vetoing the change
because otherwise the applications behaviour (which just refuses to do what the user
wants) might be quite surprising.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
