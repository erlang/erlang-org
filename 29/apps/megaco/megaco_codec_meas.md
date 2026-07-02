# `megaco_codec_meas`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/megaco/examples/meas/megaco_codec_meas.erl#L43)

This module implements a simple megaco codec measurement tool.

Results are written to file (excel compatible text files) and on stdout.

_Note_ that this module is _not_ included in the runtime part of the
application.

# `start`

# `start`

```erlang
-spec start([MessagePackage]) -> ok when MessagePackage :: atom();
           (MessagePackage) -> ok when MessagePackage :: atom();
           (Factor) -> ok when Factor :: pos_integer().
```

This function runs the measurement on all the _official_ codecs; pretty,
compact, ber, per and erlang.

This function is intended to be called from the _meas_ script, which
uses the '-s' arguments to run the function:

```text
erl -s megaco_codec_meas start time_test
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
