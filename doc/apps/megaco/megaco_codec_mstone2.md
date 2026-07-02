# `megaco_codec_mstone2`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/megaco/examples/meas/megaco_codec_mstone2.erl#L52)

This module implements a simple megaco codec-based performance tool.

This module implements the _mstone2_ tool, a simple megaco codec-based
performance tool.

The results, the mstone value(s), are written to stdout.

_Note_ that this module is _not_ included in the runtime part of the
application.

# `start`

# `start`

```erlang
-spec start(RunTime) -> ok when RunTime :: pos_integer();
           (MessagePackage) -> ok when MessagePackage :: atom().
```

start(RunTime | MessagePackage)

This function starts the _mstone2_ performance test with all codec configs.
Processes are created dynamically. Each process make _one_ run through their
messages (decoding and encoding messages) and then exits. When one process
exits, a new is created with the same codec config and set of messages.

The number of messages processed in total (for all processes) is the mstone
value.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
