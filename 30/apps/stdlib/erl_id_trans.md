# `erl_id_trans`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/examples/erl_id_trans.erl#L20)

This module performs an identity parse transformation of Erlang code.

It is included as an example for users who wants to write their own
parse transformers. If option `{parse_transform,Module}` is passed
to the compiler, a user-written function `parse_transform/2`
is called by the compiler before the code is checked for errors.

Before the function `parse_transform/2` is called, the Erlang
Compiler checks if the parse transformation can handle abstract code
with column numbers: If the function `parse_transform_info/0`
is implemented and returns a map where the key `error_location` is
associated with the value `line`, the compiler removes
column numbers from the abstract code before calling the parse
transform. Otherwise, the compiler passes the abstract code on
without modification.

## Parse Transformations

Parse transformations are used if a programmer wants to use
Erlang syntax, but with different semantics. The original Erlang
code is then transformed into other Erlang code.

> #### Note {: .info }
>
> Programmers are strongly advised not to engage in parse
> transformations. No support is offered for problems encountered.
>

## See Also

`m:erl_parse` and `m:compile`.

# `parse_transform`

```erlang
-spec parse_transform(Forms, Options) -> NewForms
                         when
                             Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
                             NewForms :: Forms,
                             Options :: [compile:option()].
```

Performs an identity transformation on Erlang forms, as an example.

# `parse_transform_info`
*since OTP 24.0* 

```erlang
-spec parse_transform_info() -> #{error_location => column | line}.
```

Returns information about the parse transform itself.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
