# `et`
[🔗](https://github.com/erlang/otp/blob/master/lib/et/src/et.erl#L87)

Main API of the Event Trace (ET) application

Interface module for the Event Trace (ET) application

# `actor`
*not exported* 

```erlang
-type actor() :: term().
```

# `level`
*not exported* 

```erlang
-type level() :: 0..100.
```

# `phone_home`

```erlang
-spec phone_home(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
                    when
                        DetailLevel :: level(),
                        FromTo :: actor(),
                        Label :: atom() | string() | term(),
                        Contents :: [{Key :: term(), Value :: term()}] | term().
```

# `phone_home`

```erlang
-spec phone_home(DetailLevel, From, To, Label, Contents) -> hopefully_traced
                    when
                        DetailLevel :: level(),
                        From :: actor(),
                        To :: actor(),
                        Label :: atom() | string() | term(),
                        Contents :: [{Key :: term(), Value :: term()}] | term().
```

These functions sends a signal to the outer space and the caller hopes that
someone is listening. In other words, they invoke `et:trace_me/4` and
`et:trace_me/5` respectively.

# `report_event`

```erlang
-spec report_event(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
                      when
                          DetailLevel :: level(),
                          FromTo :: actor(),
                          Label :: atom() | string() | term(),
                          Contents :: [{Key :: term(), Value :: term()}] | term().
```

# `report_event`

```erlang
-spec report_event(DetailLevel, From, To, Label, Contents) -> hopefully_traced
                      when
                          DetailLevel :: level(),
                          From :: actor(),
                          To :: actor(),
                          Label :: atom() | string() | term(),
                          Contents :: [{Key :: term(), Value :: term()}] | term().
```

Deprecated functions which for the time being are kept for backwards
compatibility. Invokes `et:trace_me/4` and `et:trace_me/5` respectively.

# `trace_me`
*since OTP R13B04* 

```erlang
-spec trace_me(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
                  when
                      DetailLevel :: level(),
                      FromTo :: actor(),
                      Label :: atom() | string() | term(),
                      Contents :: [{Key :: term(), Value :: term()}] | term().
```

Invokes `et:trace_me/5` with both `From` and `To` set to `FromTo`.

# `trace_me`
*since OTP R13B04* 

```erlang
-spec trace_me(DetailLevel, From, To, Label, Contents) -> hopefully_traced
                  when
                      DetailLevel :: level(),
                      From :: actor(),
                      To :: actor(),
                      Label :: atom() | string() | term(),
                      Contents :: [{Key :: term(), Value :: term()}] | term().
```

A function that is intended to be traced.

This function is intended to be invoked at strategic places in user applications
in order to enable simplified tracing. The functions are extremely light weight
as they do nothing besides returning an atom. The functions are designed for
being traced. The global tracing mechanism in `et_collector` defaults to set its
trace pattern to these functions.

The label is intended to provide a brief summary of the event. It is preferred
to use an atom but a string would also do.

The contents can be any term but in order to simplify post processing of the
traced events, a plain list of \{Key, Value\} tuples is preferred.

Some events, such as messages, are directed from some actor to another. Other
events (termed actions) may be undirected and only have one actor.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
