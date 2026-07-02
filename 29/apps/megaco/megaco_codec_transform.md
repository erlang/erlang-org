# `megaco_codec_transform`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/megaco/examples/meas/megaco_codec_transform.erl#L59)

Megaco message transformation utility.

This module implements a simple megaco message transformation utility.

_Note_ that this module is _not_ included in the runtime part of the
application.

[](){: #export_messages }

# `export_messages`

# `export_messages`

```erlang
-spec export_messages(MessagePackage) -> ok | {error, Reason}
                         when MessagePackage :: atom(), Reason :: term().
```

Export the messages in the `MessagePackage` (default is `time_test`).

The output produced by this function is a directory structure with the following
structure:

```text
<message package>/pretty/<message-files>
                  compact/<message-files>
                  per/<message-files>
                  ber/<message-files>
                  erlang/<message-files>
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
