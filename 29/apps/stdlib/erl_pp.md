# `erl_pp`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/erl_pp.erl#L22)

The Erlang pretty printer.

The functions in this module are used to generate aesthetically attractive
representations of abstract forms, which are suitable for printing. All
functions return (possibly deep) lists of characters and generate an error if
the form is wrong.

All functions can have an optional argument, which specifies a hook that is
called if an attempt is made to print an unknown form.

Note that if the functions in this module are used to convert abstract code back
to Erlang source code, the enclosing function should first be processed by
`legalize_vars/1` in order to ensure that the output is semantically equivalent
to the abstract code.

## Known Limitations

It is not possible to have hook functions for unknown forms at other places than
expressions.

### See Also

`m:erl_eval`, `m:erl_parse`, `m:io`

# `hook_function`
*not exported* 

```erlang
-type hook_function() ::
          none |
          fun((Expr :: erl_parse:abstract_expr(),
               CurrentIndentation :: integer(),
               CurrentPrecedence :: non_neg_integer(),
               Options :: options()) ->
                  io_lib:chars()).
```

Optional argument `HookFunction`, shown in the functions
described in this module, defines a function that is called when an unknown form
occurs where there is to be a valid expression. If `HookFunction` is equal to
`none`, there is no hook function.

The called hook function is to return a (possibly deep) list of characters.
Function `expr/4` is useful in a hook.

If `CurrentIndentation` is negative, there are no line breaks and only a space
is used as a separator.

# `option`
*not exported* 

```erlang
-type option() ::
          {hook, hook_function()} |
          {encoding, latin1 | unicode | utf8} |
          {quote_singleton_atom_types, boolean()} |
          {linewidth, pos_integer()} |
          {indent, pos_integer()}.
```

The option `quote_singleton_atom_types` is used to add quotes to all singleton
atom types.

The option `linewidth` controls the maximum line width for formatted lines
(defaults to 72 characters).

The option `indent` controls the indention for formatted lines (defaults to 4
spaces).

# `options`
*not exported* 

```erlang
-type options() :: hook_function() | [option()].
```

# `attribute`

```erlang
-spec attribute(Attribute) -> io_lib:chars() when Attribute :: erl_parse:abstract_form().
```

# `attribute`

```erlang
-spec attribute(Attribute, Options) -> io_lib:chars()
                   when Attribute :: erl_parse:abstract_form(), Options :: options().
```

Same as [`form/1,2`](`form/1`), but only for attribute `Attribute`.

# `expr`

```erlang
-spec expr(Expression) -> io_lib:chars() when Expression :: erl_parse:abstract_expr().
```

# `expr`

```erlang
-spec expr(Expression, Options) -> io_lib:chars()
              when Expression :: erl_parse:abstract_expr(), Options :: options().
```

# `expr`

```erlang
-spec expr(Expression, Indent, Options) -> io_lib:chars()
              when Expression :: erl_parse:abstract_expr(), Indent :: integer(), Options :: options().
```

# `expr`

```erlang
-spec expr(Expression, Indent, Precedence, Options) -> io_lib:chars()
              when
                  Expression :: erl_parse:abstract_expr(),
                  Indent :: integer(),
                  Precedence :: non_neg_integer(),
                  Options :: options().
```

Prints one expression.

It is useful for implementing hooks (see section
[Known Limitations](`m:erl_pp#module-known-limitations`)).

# `exprs`

```erlang
-spec exprs(Expressions) -> io_lib:chars() when Expressions :: [erl_parse:abstract_expr()].
```

# `exprs`

```erlang
-spec exprs(Expressions, Options) -> io_lib:chars()
               when Expressions :: [erl_parse:abstract_expr()], Options :: options().
```

# `exprs`

```erlang
-spec exprs(Expressions, Indent, Options) -> io_lib:chars()
               when
                   Expressions :: [erl_parse:abstract_expr()], Indent :: integer(), Options :: options().
```

Same as [`form/1,2`](`form/1`), but only for the sequence of expressions in
`Expressions`.

# `form`

```erlang
-spec form(Form) -> io_lib:chars() when Form :: erl_parse:abstract_form() | erl_parse:form_info().
```

# `form`

```erlang
-spec form(Form, Options) -> io_lib:chars()
              when Form :: erl_parse:abstract_form() | erl_parse:form_info(), Options :: options().
```

Pretty prints a `Form`, which is an abstract form of a type that is returned by
`erl_parse:parse_form/1`.

# `function`

```erlang
-spec function(Function) -> io_lib:chars() when Function :: erl_parse:abstract_form().
```

# `function`

```erlang
-spec function(Function, Options) -> io_lib:chars()
                  when Function :: erl_parse:abstract_form(), Options :: options().
```

Same as [`form/1,2`](`form/1`), but only for function `Function`.

# `guard`

```erlang
-spec guard(Guard) -> io_lib:chars() when Guard :: [erl_parse:abstract_expr()].
```

# `guard`

```erlang
-spec guard(Guard, Options) -> io_lib:chars()
               when Guard :: [erl_parse:abstract_expr()], Options :: options().
```

Same as [`form/1,2`](`form/1`), but only for the guard test `Guard`.

# `legalize_vars`
*since OTP 25.0* 

```erlang
-spec legalize_vars(Function) -> erl_parse:abstract_form() when Function :: erl_parse:abstract_form().
```

The Erlang compiler will, when expanding records to tuples, introduce new
variables in the abstract representation. As the expansion is done on the
abstract representation, the compiler can safely name the new variables with
names that are not syntactically valid in Erlang source code (the name starts
with a lowercase letter), thus ensuring the uniqueness of the new names.

The above strategy leads to problems if a user wants to convert the abstract
representation, using the functions of this module back to Erlang source code.
Typically, pattern variables are output as atoms thus changing the sematics of
the program. To solve this problem [`legalize_vars/1`](`legalize_vars/1`), when
run on the abstract representation of a function, will return an equivalent
function where all variables will have syntactically valid names.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
