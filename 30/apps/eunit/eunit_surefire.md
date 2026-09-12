# `eunit_surefire`
[🔗](https://github.com/erlang/otp/blob/master/lib/eunit/src/eunit_surefire.erl#L32)

Surefire reports for EUnit (Format used by Maven and Atlassian Bamboo for
example to integrate test results). Based on initial code from Paul Guyot.

Example: Generate XML result file in the current directory:

```text
     eunit:test([fib, eunit_examples],
                [{report,{eunit_surefire,[{dir,"."}]}}]).
```

_See also: _`m:eunit`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
