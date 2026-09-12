# `calendar`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/calendar.erl#L22)

Local and universal time, day of the week, date and time conversions.

This module provides computation of local and universal time, day of the week,
and many time conversion functions.

Time is local when it is adjusted in accordance with the current time zone and
daylight saving. Time is universal when it reflects the time at longitude zero,
without any adjustment for daylight saving. Universal Coordinated Time (UTC)
time is also called Greenwich Mean Time (GMT).

The time functions `local_time/0` and `universal_time/0` in this module both
return date and time. This is because separate functions for date and time can
result in a date/time combination that is displaced by 24 hours. This occurs if
one of the functions is called before midnight, and the other after midnight.
This problem also applies to the Erlang BIFs `date/0` and `time/0`, and their
use is strongly discouraged if a reliable date/time stamp is required.

All dates conform to the Gregorian calendar. This calendar was introduced by
Pope Gregory XIII in 1582 and was used in all Catholic countries from this year.
Protestant parts of Germany and the Netherlands adopted it in 1698, England
followed in 1752, and Russia in 1918 (the October revolution of 1917 took place
in November according to the Gregorian calendar).

The Gregorian calendar in this module is extended back (proleptic
Gregorian calendar) beyond year 0 to negative years. For a given date,
the _gregorian days_ is the number of days from Jan 1 year 0 up to
the date specified. Similarly, the _gregorian seconds_ for a specified
date and time is the number of seconds from 0:00 Jan 1 year 0 up to
the specified date and time.

Non-positive years use astronomical year numbering where year 0 = 1 BCE/BC,
year -1 = 2 BCE/BC, and so on, because in the CE+BCE and AD+BC year numberings
there is no year 0. Instead 1 CE (AD 1) is preceded by 1 BCE (1 BC).

For computing differences between epochs in time, use the functions counting
gregorian days or seconds. If epochs are specified as local time, they must be
converted to universal time to get the correct value of the elapsed time between
epochs. Use of function [`time_difference/2`](`time_difference/2`) is
discouraged.

Different definitions exist for the week of the year. This module contains a
week of the year implementation conforming to the ISO 8601 standard. As the week
number for a specified date can fall on the previous, the current, or on the
next year, it is important to specify both the year and the week number.
Functions `iso_week_number/0` and [`iso_week_number/1`](`iso_week_number/1`)
return a tuple of the year and the week number.

## Leap Years

The notion that every fourth year is a leap year is not completely true. By the
Gregorian rule, a year Y is a leap year if one of the following rules is valid:

- Y is divisible by 4, but not by 100.
- Y is divisible by 400.

Hence, 1996 is a leap year, 1900 is not, but 2000 is.

## Date and Time Source

Local time is obtained from the Erlang BIF `localtime/0`. Universal time is
computed from the BIF `universaltime/0`.

The following apply:

- There are 86400 seconds in a day.
- There are 365 days in an ordinary year.
- There are 366 days in a leap year.
- There are 1461 days in a 4 year period.
- There are 36524 days in a 100 year period.
- There are 146097 days in a 400 year period.
- There are 719528 days between Jan 1, 0 and Jan 1, 1970.

# `date`

```erlang
-type date() :: {year(), month(), day()}.
```

A date using the Gregorian calendar.

All APIs expect this to be a valid date. If the source of the date
is unknown, then verify that it is valid by calling `valid_date/1`
before using it.

# `datetime1970`

```erlang
-type datetime1970() :: {{year1970(), month(), day()}, time()}.
```

# `datetime`

```erlang
-type datetime() :: {date(), time()}.
```

# `day`
*not exported* 

```erlang
-type day() :: 1..31.
```

# `daynum`
*not exported* 

```erlang
-type daynum() :: 1..7.
```

# `hour`
*not exported* 

```erlang
-type hour() :: 0..23.
```

# `ldom`
*not exported* 

```erlang
-type ldom() :: 28 | 29 | 30 | 31.
```

The last day of the month.

# `minute`
*not exported* 

```erlang
-type minute() :: 0..59.
```

# `month`
*not exported* 

```erlang
-type month() :: 1..12.
```

# `offset`
*not exported* 

```erlang
-type offset() :: [byte()] | (Time :: integer()).
```

# `rfc3339_string`
*not exported* 

```erlang
-type rfc3339_string() :: [byte(), ...] | binary().
```

# `rfc3339_time_unit`
*not exported* 

```erlang
-type rfc3339_time_unit() :: microsecond | millisecond | nanosecond | second | native.
```

The time unit used by the rfc3339 conversion functions.

> #### Note {: .info }
>
> The `native` time unit was added to `t:rfc3339_time_unit/0` in OTP 25.0.

# `second`
*not exported* 

```erlang
-type second() :: 0..59.
```

# `secs_per_day`
*not exported* 

```erlang
-type secs_per_day() :: 0..86399.
```

# `time`

```erlang
-type time() :: {hour(), minute(), second()}.
```

# `weeknum`
*not exported* 

```erlang
-type weeknum() :: 1..53.
```

# `year1970`
*not exported* 

```erlang
-type year1970() :: 1970..10000.
```

# `year`
*not exported* 

```erlang
-type year() :: integer().
```

The year using the Gregorian calendar.

Year cannot be abbreviated. For example, 93 denotes year 93, not 1993. The valid
range depends on the underlying operating system.

# `yearweeknum`
*not exported* 

```erlang
-type yearweeknum() :: {year(), weeknum()}.
```

# `date_to_gregorian_days`

```erlang
-spec date_to_gregorian_days(Date) -> Days when Date :: date(), Days :: integer().
```

Computes the number of gregorian days starting with year 0 and ending at the
specified date.

# `date_to_gregorian_days`

```erlang
-spec date_to_gregorian_days(Year, Month, Day) -> Days
                                when Year :: year(), Month :: month(), Day :: day(), Days :: integer().
```

# `datetime_to_gregorian_seconds`

```erlang
-spec datetime_to_gregorian_seconds(DateTime) -> Seconds
                                       when DateTime :: datetime(), Seconds :: integer().
```

Computes the number of gregorian seconds starting with year 0 and ending at the
specified date and time.

# `day_of_the_week`

```erlang
-spec day_of_the_week(Date) -> daynum() when Date :: date().
```

Computes the day of the week from the specified `Year`, `Month`, and `Day`.
Returns the day of the week as `1`: Monday, `2`: Tuesday, and so on.

# `day_of_the_week`

```erlang
-spec day_of_the_week(Year, Month, Day) -> daynum() when Year :: year(), Month :: month(), Day :: day().
```

# `gregorian_days_to_date`

```erlang
-spec gregorian_days_to_date(Days) -> date() when Days :: integer().
```

Computes the date from the specified number of gregorian days.

# `gregorian_seconds_to_datetime`

```erlang
-spec gregorian_seconds_to_datetime(Seconds) -> datetime() when Seconds :: integer().
```

Computes the date and time from the specified number of gregorian seconds.

# `is_leap_year`

```erlang
-spec is_leap_year(Year) -> boolean() when Year :: year().
```

Checks if the specified year is a leap year.

# `iso_week_number`
*since OTP R14B02* 

```erlang
-spec iso_week_number() -> yearweeknum().
```

Returns tuple `{Year, WeekNum}` representing the ISO week number for the actual
date. To determine the actual date, use function `local_time/0`.

# `iso_week_number`
*since OTP R14B02* 

```erlang
-spec iso_week_number(Date) -> yearweeknum() when Date :: date().
```

Returns tuple `{Year, WeekNum}` representing the ISO week number for the
specified date.

# `last_day_of_the_month`

```erlang
-spec last_day_of_the_month(Year, Month) -> LastDay
                               when Year :: year(), Month :: month(), LastDay :: ldom().
```

Computes the number of days in a month.

# `local_time`

```erlang
-spec local_time() -> datetime().
```

Returns the local time reported by the underlying operating system.

# `local_time_to_system_time`
*since OTP 28.0* 

```erlang
-spec local_time_to_system_time(datetime1970()) -> pos_integer().
```

# `local_time_to_system_time`
*since OTP 28.0* 

```erlang
-spec local_time_to_system_time(datetime1970(), Options) -> pos_integer()
                                   when Options :: [Option], Option :: {unit, erlang:time_unit()}.
```

Converts local time into system time.
Error will occur if the local time is non existing or ambiguous due to DST,
see [`calendar:local_time_to_universal_time_dst/1`](`local_time_to_universal_time_dst/1`).

# `local_time_to_universal_time`

> This function is deprecated. calendar:local_time_to_universal_time/1 is deprecated; use calendar:local_time_to_universal_time_dst/1 instead.

```erlang
-spec local_time_to_universal_time(DateTime1) -> DateTime2
                                      when DateTime1 :: datetime1970(), DateTime2 :: datetime1970().
```

Converts from local time to Universal Coordinated Time (UTC). `DateTime1` must
refer to a local date after Jan 1, 1970.

> #### Warning {: .warning }
>
> This function is deprecated. Use `local_time_to_universal_time_dst/1` instead,
> as it gives a more correct and complete result. Especially for the period that
> does not exist, as it is skipped during the switch _to_ daylight saving time,
> this function still returns a result.

# `local_time_to_universal_time_dst`

```erlang
-spec local_time_to_universal_time_dst(DateTime1) -> [DateTime]
                                          when DateTime1 :: datetime1970(), DateTime :: datetime1970().
```

Converts from local time to Universal Coordinated Time (UTC). `DateTime1` must
refer to a local date after Jan 1, 1970.

The return value is a list of 0, 1, or 2 possible UTC times:

- **`[]`** - For a local `{Date1, Time1}` during the period that is skipped when
  switching _to_ daylight saving time, there is no corresponding UTC, as the
  local time is illegal (it has never occured).

- **`[DstDateTimeUTC, DateTimeUTC]`** - For a local `{Date1, Time1}` during the
  period that is repeated when switching _from_ daylight saving time, two
  corresponding UTCs exist; one for the first instance of the period when
  daylight saving time is still active, and one for the second instance.

- **`[DateTimeUTC]`** - For all other local times only one corresponding UTC
  exists.

# `now_to_datetime`

```erlang
-spec now_to_datetime(Now) -> datetime1970() when Now :: erlang:timestamp().
```

Returns Universal Coordinated Time (UTC) converted from the return value from
`erlang:timestamp/0`.

# `now_to_local_time`

```erlang
-spec now_to_local_time(Now) -> datetime1970() when Now :: erlang:timestamp().
```

Returns local date and time converted from the return value from
`erlang:timestamp/0`.

# `now_to_universal_time`

```erlang
-spec now_to_universal_time(Now) -> datetime1970() when Now :: erlang:timestamp().
```

Returns Universal Coordinated Time (UTC) converted from the return value from
`erlang:timestamp/0`.

# `rfc3339_to_system_time`
*since OTP 21.0* 

```erlang
-spec rfc3339_to_system_time(DateTimeString) -> integer() when DateTimeString :: rfc3339_string().
```

# `rfc3339_to_system_time`
*since OTP 21.0* 

```erlang
-spec rfc3339_to_system_time(DateTimeString, Options) -> integer()
                                when
                                    DateTimeString :: rfc3339_string(),
                                    Options :: [Option],
                                    Option :: {unit, rfc3339_time_unit()}.
```

Converts an RFC 3339 timestamp into system time. The data format of RFC 3339
timestamps is described by [RFC 3339](https://www.ietf.org/rfc/rfc3339.txt).
Starting from OTP 25.1, the minutes part of the time zone is optional.

Valid option:

- **`{unit, Unit}`** - The time unit of the return value. The default is
  `second`.

```erlang
1> calendar:rfc3339_to_system_time("2018-02-01T16:17:58+01:00").
1517498278
2> calendar:rfc3339_to_system_time("2018-02-01 15:18:02.088Z",
   [{unit, nanosecond}]).
1517498282088000000
3> calendar:rfc3339_to_system_time(<<"2018-02-01 15:18:02.088Z">>,
   [{unit, nanosecond}]).
1517498282088000000
```

# `seconds_to_daystime`

```erlang
-spec seconds_to_daystime(Seconds) -> {Days, Time}
                             when Seconds :: integer(), Days :: integer(), Time :: time().
```

Converts a specified number of seconds into days, hours, minutes, and seconds.
`Time` is always non-negative, but `Days` is negative if argument `Seconds` is.

# `seconds_to_time`

```erlang
-spec seconds_to_time(Seconds) -> time() when Seconds :: secs_per_day().
```

Computes the time from the specified number of seconds. `Seconds` must be less
than the number of seconds per day (86400).

# `system_time_to_local_time`
*since OTP 21.0* 

```erlang
-spec system_time_to_local_time(Time, TimeUnit) -> datetime()
                                   when Time :: integer(), TimeUnit :: erlang:time_unit().
```

Converts a specified system time into local date and time.

# `system_time_to_rfc3339`
*since OTP 21.0* 

```erlang
-spec system_time_to_rfc3339(Time) -> DateTimeString
                                when Time :: integer(), DateTimeString :: rfc3339_string().
```

# `system_time_to_rfc3339`
*since OTP 21.0* 

```erlang
-spec system_time_to_rfc3339(Time, Options) -> DateTimeString
                                when
                                    Time :: integer(),
                                    Options :: [Option],
                                    Option ::
                                        {offset, offset()} |
                                        {time_designator, byte()} |
                                        {unit, rfc3339_time_unit()} |
                                        {return, string | binary},
                                    DateTimeString :: rfc3339_string().
```

Converts a system time into an RFC 3339 timestamp.

The data format of RFC 3339 timestamps is described by [RFC 3339].
The data format of offsets is also described by [RFC 3339].

Valid options:

- **`{offset, Offset}`** - The offset, either a string or an integer, to be
  included in the formatted string. An empty string, which is the default, is
  interpreted as local time. A non-empty string is included as is. The time unit
  of the integer is the same as the one of `Time`.

- **`{time_designator, Character}`** - The character used as time designator,
  that is, the date and time separator. The default is `$T`.

- **`{unit, Unit}`** - The time unit of `Time`. The default is `second`. If some
  other unit is given (`millisecond`, `microsecond`, `nanosecond`, or `native`),
  the formatted string includes a fraction of a second. The number of fractional
  second digits is three, six, or nine depending on what time unit is chosen.
  For `native` three fractional digits are included. Notice that trailing zeros
  are not removed from the fraction.

- **`{return, Return}`** - The desired encoding type for the output,
  whether a string or a binary is desired. Defaults to string.

```erlang
1> calendar:system_time_to_rfc3339(erlang:system_time(second)).
"2018-04-23T14:56:28+02:00"
2> calendar:system_time_to_rfc3339(erlang:system_time(second),
   [{offset, "-02:00"}]).
"2018-04-23T10:56:52-02:00"
3> calendar:system_time_to_rfc3339(erlang:system_time(second),
   [{offset, -7200}]).
"2018-04-23T10:57:05-02:00"
4> calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
   [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}]).
"2018-04-23 12:57:20.482Z"
5> calendar:system_time_to_rfc3339(erlang:system_time(millisecond),
   [{unit, millisecond}, {time_designator, $\s}, {offset, "Z"}, {return, binary}]).
<<"2018-04-23 12:57:20.482Z">>
```
[RFC 3339]: https://www.ietf.org/rfc/rfc3339.txt

# `system_time_to_universal_time`
*since OTP 21.0* 

```erlang
-spec system_time_to_universal_time(Time, TimeUnit) -> datetime()
                                       when Time :: integer(), TimeUnit :: erlang:time_unit().
```

Converts a specified system time into universal date and time.

# `time_difference`

```erlang
-spec time_difference(T1, T2) -> {Days, Time}
                         when T1 :: datetime(), T2 :: datetime(), Days :: integer(), Time :: time().
```

Returns the difference between two `{Date, Time}` tuples. `T2` is to refer to an
epoch later than `T1`.

> #### Warning {: .warning }
>
> This function is obsolete. Use the conversion functions for gregorian days and
> seconds instead.

# `time_to_seconds`

```erlang
-spec time_to_seconds(Time) -> secs_per_day() when Time :: time().
```

Returns the number of seconds since midnight up to the specified time.

# `universal_time`

```erlang
-spec universal_time() -> datetime().
```

Returns the Universal Coordinated Time (UTC) reported by the underlying
operating system. Returns local time if universal time is unavailable.

# `universal_time_to_local_time`

```erlang
-spec universal_time_to_local_time(DateTime) -> datetime() when DateTime :: datetime1970().
```

Converts from Universal Coordinated Time (UTC) to local time. `DateTime` must
refer to a date after Jan 1, 1970.

# `universal_time_to_system_time`
*since OTP 28.0* 

```erlang
-spec universal_time_to_system_time(datetime()) -> integer().
```

# `universal_time_to_system_time`
*since OTP 28.0* 

```erlang
-spec universal_time_to_system_time(datetime(), Options) -> integer()
                                       when Options :: [Option], Option :: {unit, erlang:time_unit()}.
```

Converts universal time into system time.

# `valid_date`

```erlang
-spec valid_date(Date) -> boolean() when Date :: date().
```

This function checks if a date is a valid.

# `valid_date`

```erlang
-spec valid_date(Year, Month, Day) -> boolean()
                    when Year :: integer(), Month :: integer(), Day :: integer().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
