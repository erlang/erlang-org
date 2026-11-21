# `wxDateEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDateEvent.erl#L58)

This event class holds information about a date change and is used together with `m:wxDatePickerCtrl`.

It also serves as a base class for `m:wxCalendarEvent`.

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxDateEvent](https://docs.wxwidgets.org/3.2/classwx_date_event.html)

# `wxDate`

```erlang
-type wxDate() :: #wxDate{type :: wxDateEvent:wxDateEventType(), date :: wx:wx_datetime()}.
```

# `wxDateEvent`

```erlang
-type wxDateEvent() :: wx:wx_object().
```

# `wxDateEventType`

```erlang
-type wxDateEventType() :: date_changed.
```

# `getDate`

```erlang
-spec getDate(This) -> wx:wx_datetime() when This :: wxDateEvent().
```

Returns the date.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
