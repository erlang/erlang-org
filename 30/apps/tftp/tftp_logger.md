# `tftp_logger`
[🔗](https://github.com/erlang/otp/blob/master/lib/tftp/src/tftp_logger.erl#L23)

Trivial FTP logger.

A `tftp_logger` callback module is to be implemented as a `tftp_logger` behavior
and export the following functions:

# `error_msg`
*since OTP 18.1* *optional* 

```erlang
-callback error_msg(Format :: io:format(), Args :: [term()]) -> ok.
```

Logs an error message. See `logger:error/2` for details.

# `info_msg`
*since OTP 18.1* *optional* 

```erlang
-callback info_msg(Format :: io:format(), Args :: [term()]) -> ok.
```

Logs an info message. See `logger:info/2` for details.

# `warning_msg`
*since OTP 18.1* *optional* 

```erlang
-callback warning_msg(Format :: io:format(), Args :: [term()]) -> ok.
```

Logs a warning message. See `logger:warning/2` for details.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
