# `mnesia_registry`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/mnesia/src/mnesia_registry.erl#L24)

This module is deprecated and the functions should not be used.

This module was intended for internal use within OTP by the `erl_interface` application,
but it has two functions that are exported for public use.

Since the `erl_interface` have removed the registry functionality a long time ago,
these functions are deprecated.

## See Also
 `m:mnesia`

# `create_table`

> This function is deprecated. mnesia_registry:create_table/1 is deprecated; use mnesia:create_table/2 instead.

```elixir
-spec create_table(Tab :: atom()) -> ok.
```

> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>

A wrapper function for `mnesia:create_table/2`, which creates a table (if there
is no existing table) with an appropriate set of `attributes`. The table only
resides on the local node and its storage type is the same as the `schema` table
on the local node, that is, `{ram_copies,[node()]}` or `{disc_copies,[node()]}`.

This function is used by `erl_interface` to create the Mnesia table if it does
not already exist.

# `create_table`

> This function is deprecated. mnesia_registry:create_table/2 is deprecated; use mnesia:create_table/2 instead.

```elixir
-spec create_table(Tab :: atom(), Opt :: [{atom(), term()}]) -> ok.
```

> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>

A wrapper function for `mnesia:create_table/2`, which creates a table (if there
is no existing table) with an appropriate set of `attributes`. The attributes
and `TabDef` are forwarded to `mnesia:create_table/2`. For example, if the table
is to reside as `disc_only_copies` on all nodes, a call looks as follows:

```erlang
          TabDef = [{{disc_only_copies, node()|nodes()]}],
          mnesia_registry:create_table(my_reg, TabDef)
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
