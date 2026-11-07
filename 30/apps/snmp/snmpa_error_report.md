# `snmpa_error_report`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_error_report.erl#L22)

Behaviour module for reporting SNMP agent errors

[](){: #desc } This module defines the behaviour of the agent error reporting. A
`snmpa_error_report` compliant module must export the following functions:

- config_err/2
- user_err/2

The semantics of them and their exact signatures are explained below.

# `config_err`

```erlang
-callback config_err(Format, Args) -> snmp:void() when Format :: string(), Args :: [term()].
```

The function is called if an error occurs during the configuration phase, for
example if a syntax error is found in a configuration file.

`Format` and `Args` are as in `io:format(Format, Args)`.

# `user_err`

```erlang
-callback user_err(Format, Args) -> snmp:void() when Format :: string(), Args :: [term()].
```

The function is called if a user related error occurs at run-time, for example
if a user defined instrumentation function returns erroneous.

`Format` and `Args` are as in `io:format(Format, Args)`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
