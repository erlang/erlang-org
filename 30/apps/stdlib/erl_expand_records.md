# `erl_expand_records`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_expand_records.erl#L25)

This module expands records in a module.

### See Also

Section [The Abstract Format](`e:erts:absform.md`) in ERTS User's Guide.

# `module`

```erlang
-spec module(AbsForms, CompileOptions) -> AbsForms2
                when
                    AbsForms :: [erl_parse:abstract_form()],
                    AbsForms2 :: [erl_parse:abstract_form()],
                    CompileOptions :: [compile:option()].
```

Expands all records in a module to use explicit tuple operations and adds
explicit module names to calls to auto-imported BIFs and imported functions.

The code in the returned module has no references to records, imported functions,
or auto-imported BIFs.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
