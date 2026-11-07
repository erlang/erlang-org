# `megaco_edist_compress`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/engine/megaco_edist_compress.erl#L28)

Megaco erlang dist compress behaviour.

The following functions should be exported from a `megaco_edist_compress`
callback module:

# `decode`

```erlang
-callback decode(T, Version) -> R
                    when
                        T :: term(),
                        Version :: megaco_encoder:protocol_version() | dynamic,
                        R ::
                            megaco_encoder:megaco_message() |
                            megaco_encoder:transaction() |
                            megaco_encoder:action_reply() |
                            megaco_encoder:action_request() |
                            megaco_encoder:command_request().
```

Decompress a megaco component.

# `encode`

```erlang
-callback encode(R, Version) -> T
                    when
                        R ::
                            megaco_encoder:megaco_message() |
                            megaco_encoder:transaction() |
                            megaco_encoder:action_reply() |
                            megaco_encoder:action_request() |
                            megaco_encoder:command_request(),
                        Version :: megaco_encoder:protocol_version(),
                        T :: term().
```

Compress a megaco component. The erlang dist encoder makes no assumption on the
how or even if the component is compressed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
