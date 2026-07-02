# `erl_internal`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/erl_internal.erl#L22)

Internal Erlang definitions.

This module defines Erlang BIFs, guard tests, and operators. This module is only
of interest to programmers who manipulate Erlang code.

# `add_predefined_functions`
*since OTP 20.0* 

```elixir
-spec add_predefined_functions(Forms) -> UpdatedForms
                                  when
                                      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
                                      UpdatedForms ::
                                          [erl_parse:abstract_form() | erl_parse:form_info()].
```

Adds to `Forms` the code for the standard pre-defined functions (such as
`module_info/0`) that are to be included in every module.

# `arith_op`

```elixir
-spec arith_op(OpName, Arity) -> boolean() when OpName :: atom(), Arity :: arity().
```

Returns `true` if `OpName/Arity` is an arithmetic operator, otherwise `false`.

# `bif`

```elixir
-spec bif(Name, Arity) -> boolean() when Name :: atom(), Arity :: arity().
```

Returns `true` if `Name/Arity` is an Erlang BIF that is automatically recognized
by the compiler, otherwise `false`.

# `bool_op`

```elixir
-spec bool_op(OpName, Arity) -> boolean() when OpName :: atom(), Arity :: arity().
```

Returns `true` if `OpName/Arity` is a Boolean operator, otherwise `false`.

# `comp_op`

```elixir
-spec comp_op(OpName, Arity) -> boolean() when OpName :: atom(), Arity :: arity().
```

Returns `true` if `OpName/Arity` is a comparison operator, otherwise `false`.

# `guard_bif`

```elixir
-spec guard_bif(Name, Arity) -> boolean() when Name :: atom(), Arity :: arity().
```

Returns `true` if `Name/Arity` is an Erlang BIF that is allowed in guards,
otherwise `false`.

# `list_op`

```elixir
-spec list_op(OpName, Arity) -> boolean() when OpName :: atom(), Arity :: arity().
```

Returns `true` if `OpName/Arity` is a list operator, otherwise `false`.

# `op_type`

```elixir
-spec op_type(OpName, Arity) -> Type
                 when OpName :: atom(), Arity :: arity(), Type :: arith | bool | comp | list | send.
```

Returns the `Type` of operator that `OpName/Arity` belongs to, or generates a
`function_clause` error if it is not an operator.

# `send_op`

```elixir
-spec send_op(OpName, Arity) -> boolean() when OpName :: atom(), Arity :: arity().
```

Returns `true` if `OpName/Arity` is a send operator, otherwise `false`.

# `type_test`

```elixir
-spec type_test(Name, Arity) -> boolean() when Name :: atom(), Arity :: arity().
```

Returns `true` if `Name/Arity` is a valid Erlang type test, otherwise `false`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
