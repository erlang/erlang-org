# `wxCalendarEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCalendarEvent.erl#L58)

The `m:wxCalendarEvent` class is used together with `m:wxCalendarCtrl`.

See: `m:wxCalendarCtrl`

This class is derived, and can use functions, from:

* `m:wxDateEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxCalendarEvent](https://docs.wxwidgets.org/3.2/classwx_calendar_event.html)

# `wxCalendar`

```erlang
-type wxCalendar() ::
          #wxCalendar{type :: wxCalendarEvent:wxCalendarEventType(),
                      wday :: wx:wx_enum(),
                      date :: wx:wx_datetime()}.
```

# `wxCalendarEvent`

```erlang
-type wxCalendarEvent() :: wx:wx_object().
```

# `wxCalendarEventType`

```erlang
-type wxCalendarEventType() ::
          calendar_sel_changed | calendar_day_changed | calendar_month_changed | calendar_year_changed |
          calendar_doubleclicked | calendar_weekday_clicked.
```

# `getDate`

```erlang
-spec getDate(This) -> wx:wx_datetime() when This :: wxCalendarEvent().
```

Returns the date.

# `getWeekDay`

```erlang
-spec getWeekDay(This) -> wx:wx_enum() when This :: wxCalendarEvent().
```

Returns the week day on which the user clicked in `EVT\_CALENDAR\_WEEKDAY\_CLICKED`
handler.

It doesn't make sense to call this function in other handlers.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
