# `snmpa_error`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_error.erl#L22)

Functions for Reporting SNMP Errors

[](){: #desc }

The module `snmpa_error` contains two callback functions which are called if an
error occurs at different times during agent operation. These functions in turn
calls the corresponding function in the configured error report module, which
implements the actual report functionality.

Two simple implementation(s) is provided with the toolkit; the modules
`m:snmpa_error_logger` which is the default and `m:snmpa_error_io`.

The error report module is configured using the directive `error_report_mod`,
see [configuration parameters](snmp_config.md#configuration_params).

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
