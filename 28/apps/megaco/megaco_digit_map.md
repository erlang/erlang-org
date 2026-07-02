# `megaco_digit_map`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/megaco/src/engine/megaco_digit_map.erl#L60)

Digit Map utility module.

This is a Digit Map utility module (types).

## Version note

This module has existed in the megaco app for long time,
but as of 27.0, its also documented.

# `cancel`
*since OTP 27.0* 

```elixir
-type cancel() :: $z | $Z | cancel.
```

`$z | $Z | cancel`

# `event`
*since OTP 27.0* 

```elixir
-type event() :: letter() | pause() | cancel().
```

# `kind`
*since OTP 27.0* 

```elixir
-type kind() :: full | unambiguous.
```

# `letter`
*since OTP 27.0* 

```elixir
-type letter() :: $0..$9 | $a..$k | $A..$K.
```

`$0..$9 | $a..$k | $A..$K`

# `one_second`
*since OTP 27.0* 

```elixir
-type one_second() :: $s | $S.
```

`$s | $S`

# `pause`
*since OTP 27.0* 

```elixir
-type pause() :: one_second() | ten_seconds().
```

# `ten_seconds`
*since OTP 27.0* 

```elixir
-type ten_seconds() :: $l | $L.
```

`$l | $L`

# `value`
*since OTP 27.0* 

```elixir
-type value() ::
          #'DigitMapValue'{startTimer :: term(),
                           shortTimer :: term(),
                           longTimer :: term(),
                           digitMapBody :: term(),
                           durationTimer :: term()}.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
