# `megaco_sdp`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/engine/megaco_sdp.erl#L28)

SDP utility module.

This module contains various things related to SDP.

## Version note

This module has existed in the megaco app for long time,
but as of 27.0 its also documented.

# `property_group`
*since OTP 27.0* 

```erlang
-type property_group() :: [property_parm()].
```

# `property_groups`
*since OTP 27.0* 

```erlang
-type property_groups() :: [property_group()].
```

# `property_parm`
*since OTP 27.0* 

```erlang
-type property_parm() :: #'PropertyParm'{name :: term(), value :: term(), extraInfo :: term()}.
```

# `sdp`
*since OTP 27.0* 

```erlang
-type sdp() ::
          sdp_o() |
          sdp_s() |
          sdp_i() |
          sdp_u() |
          sdp_e() |
          sdp_p() |
          sdp_c() |
          sdp_b() |
          sdp_k() |
          sdp_a() |
          sdp_a_rtpmap() |
          sdp_a_ptime() |
          sdp_z() |
          sdp_t() |
          sdp_r() |
          sdp_m().
```

# `sdp_a`
*since OTP 27.0* 

```erlang
-type sdp_a() :: #megaco_sdp_a{attribute :: term(), value :: term()}.
```

Session attribute.

# `sdp_a_fmtp`
*since OTP 27.0* 

```erlang
-type sdp_a_fmtp() :: #megaco_sdp_a_fmtp{format :: term(), param :: term()}.
```

# `sdp_a_ptime`
*since OTP 27.0* 

```erlang
-type sdp_a_ptime() :: #megaco_sdp_a_ptime{packet_time :: term()}.
```

# `sdp_a_quality`
*since OTP 27.0* 

```erlang
-type sdp_a_quality() :: #megaco_sdp_a_quality{quality :: term()}.
```

# `sdp_a_rtpmap`
*since OTP 27.0* 

```erlang
-type sdp_a_rtpmap() ::
          #megaco_sdp_a_rtpmap{payload_type :: term(),
                               encoding_name :: term(),
                               clock_rate :: term(),
                               encoding_parms :: term()}.
```

# `sdp_b`
*since OTP 27.0* 

```erlang
-type sdp_b() :: #megaco_sdp_b{bwtype :: term(), bandwidth :: term()}.
```

Bandwidth information.

# `sdp_c`
*since OTP 27.0* 

```erlang
-type sdp_c() ::
          #megaco_sdp_c{network_type :: term(), address_type :: term(), connection_addr :: term()}.
```

Connection information.

# `sdp_e`
*since OTP 27.0* 

```erlang
-type sdp_e() :: #megaco_sdp_e{email :: term()}.
```

Email address.

# `sdp_i`
*since OTP 27.0* 

```erlang
-type sdp_i() :: #megaco_sdp_i{session_descriptor :: term()}.
```

Session information.

# `sdp_k`
*since OTP 27.0* 

```erlang
-type sdp_k() :: #megaco_sdp_k{method :: term(), encryption_key :: term()}.
```

Encryption key.

# `sdp_m`
*since OTP 27.0* 

```erlang
-type sdp_m() ::
          #megaco_sdp_m{media :: term(),
                        port :: term(),
                        num_ports :: term(),
                        transport :: term(),
                        fmt_list :: term()}.
```

Media name and transport address.

# `sdp_o`
*since OTP 27.0* 

```erlang
-type sdp_o() ::
          #megaco_sdp_o{user_name :: term(),
                        session_id :: term(),
                        version :: term(),
                        network_type :: term(),
                        address_type :: term(),
                        address :: term()}.
```

Owner/creator and session identifier.

# `sdp_p`
*since OTP 27.0* 

```erlang
-type sdp_p() :: #megaco_sdp_p{phone_number :: term()}.
```

Phone number.

# `sdp_property_group`
*since OTP 27.0* 

```erlang
-type sdp_property_group() :: [sdp_property_parm()].
```

# `sdp_property_groups`
*since OTP 27.0* 

```erlang
-type sdp_property_groups() :: [sdp_property_group()].
```

# `sdp_property_parm`
*since OTP 27.0* 

```erlang
-type sdp_property_parm() :: sdp() | property_parm().
```

# `sdp_r`
*since OTP 27.0* 

```erlang
-type sdp_r() ::
          #megaco_sdp_r{repeat_interval :: term(), active_duration :: term(), list_of_offsets :: term()}.
```

Repeat times.

# `sdp_s`
*since OTP 27.0* 

```erlang
-type sdp_s() :: #megaco_sdp_s{name :: term()}.
```

Session name.

# `sdp_t`
*since OTP 27.0* 

```erlang
-type sdp_t() :: #megaco_sdp_t{start :: term(), stop :: term()}.
```

# `sdp_u`
*since OTP 27.0* 

```erlang
-type sdp_u() :: #megaco_sdp_u{uri :: term()}.
```

URI of description.

# `sdp_v`
*since OTP 27.0* 

```erlang
-type sdp_v() :: #megaco_sdp_v{version :: term()}.
```

Protocol version.

# `sdp_z`
*since OTP 27.0* 

```erlang
-type sdp_z() :: #megaco_sdp_z{list_of_adjustments :: term()}.
```

Time zone adjustment.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
