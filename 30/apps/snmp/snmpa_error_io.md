# `snmpa_error_io`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_error_io.erl#L22)

Functions for Reporting SNMP Errors on stdio

The module `snmpa_error_io` implements the `snmp_error_report` behaviour (see
`m:snmpa_error_report`) containing two callback functions which are called in
order to report SNMP errors.

This module provides a simple mechanism for reporting SNMP errors. Errors are
written to stdout using the `io` module. It is provided as an simple example.

This module needs to be explicitly configured, see
[snmpa_error](`m:snmpa_error#desc`) and
[configuration parameters](snmp_config.md#configuration_params).

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
