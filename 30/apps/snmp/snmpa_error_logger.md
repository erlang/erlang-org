# `snmpa_error_logger`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_error_logger.erl#L22)

Functions for Reporting SNMP Errors through the error_logger

The module `snmpa_error_logger` implements the `snmpa_error_report` behaviour
(see `m:snmpa_error_report`) containing two callback functions which are called
in order to report SNMP errors.

This module provides a simple mechanism for reporting SNMP errors. Errors are
sent to the `error_logger` after a size check. Messages are truncated after 1024
chars. It is provided as an example.

This module is the default error report module, but can be explicitly
configured, see [snmpa_error](`m:snmpa_error#desc`) and
[configuration parameters](snmp_config.md#configuration_params).

### See Also

error_logger(3)

# `config_err`

```erlang
-spec config_err(Format, Args) -> snmp:void() when Format :: string(), Args :: list().
```

The function is called if an error occurs during the configuration phase, for
example if a syntax error is found in a configuration file.

`Format` and `Args` are as in `io:format(Format, Args)`.

# `user_err`

```erlang
-spec user_err(Format, Args) -> snmp:void() when Format :: string(), Args :: list().
```

The function is called if a user related error occurs at run-time, for example
if a user defined instrumentation function returns erroneous.

`Format` and `Args` are as in `io:format(Format, Args)`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
