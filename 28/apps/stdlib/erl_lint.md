# `erl_lint`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/erl_lint.erl#L25)

The Erlang code linter.

This module is used to check Erlang code for illegal syntax and other bugs. It
also warns against coding practices that are not recommended.

The errors detected include:

- Redefined and undefined functions
- Unbound and unsafe variables
- Illegal record use

The warnings detected include:

- Unused functions and imports
- Unused variables
- Variables imported into matches
- Variables exported from `if`/`case`/`receive`
- Variables shadowed in funs and list comprehensions

Some of the warnings are optional, and can be turned on by specifying the
appropriate option, described below.

The functions in this module are invoked automatically by the Erlang compiler.
There is no reason to invoke these functions separately unless you have written
your own Erlang compiler.

## Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLine, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

## See Also

`m:epp`, `m:erl_parse`

# `error_description`

```elixir
-type error_description() :: term().
```

# `error_info`

```elixir
-type error_info() :: {erl_anno:location() | none, module(), error_description()}.
```

# `fa`
*not exported* 

```elixir
-type fa() :: {atom(), arity()}.
```

# `fun_used_vars`

```elixir
-type fun_used_vars() :: #{erl_parse:abstract_expr() => {[atom()], fun_used_vars()}}.
```

# `format_error`

```elixir
-spec format_error(ErrorDescriptor) -> io_lib:chars() when ErrorDescriptor :: error_description().
```

Takes an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when processing an
`ErrorInfo` structure (see section [Error Information](`m:erl_lint#module-error-information`)).

# `is_guard_test`

```elixir
-spec is_guard_test(Expr) -> boolean() when Expr :: erl_parse:abstract_expr().
```

Tests if `Expr` is a legal guard test. `Expr` is an Erlang term representing the
abstract form for the expression.
[`erl_parse:parse_exprs(Tokens)`](`erl_parse:parse_exprs/1`) can be used to
generate a list of `Expr`.

# `module`

```elixir
-spec module(AbsForms) -> {ok, Warnings} | {error, Errors, Warnings}
                when
                    AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
                    Warnings :: [{SourceFile, [ErrorInfo]}],
                    Errors :: [{SourceFile, [ErrorInfo]}],
                    SourceFile :: file:filename(),
                    ErrorInfo :: error_info().
```

# `module`

```elixir
-spec module(AbsForms, FileName) -> {ok, Warnings} | {error, Errors, Warnings}
                when
                    AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
                    FileName :: atom() | string(),
                    Warnings :: [{SourceFile, [ErrorInfo]}],
                    Errors :: [{SourceFile, [ErrorInfo]}],
                    SourceFile :: file:filename(),
                    ErrorInfo :: error_info().
```

# `module`

```elixir
-spec module(AbsForms, FileName, CompileOptions) -> {ok, Warnings} | {error, Errors, Warnings}
                when
                    AbsForms :: [erl_parse:abstract_form() | erl_parse:form_info()],
                    FileName :: atom() | string(),
                    CompileOptions :: [compile:option()],
                    Warnings :: [{SourceFile, [ErrorInfo]}],
                    Errors :: [{SourceFile, [ErrorInfo]}],
                    SourceFile :: file:filename(),
                    ErrorInfo :: error_info().
```

Checks all the forms in a module for errors. It returns:

- **`{ok,Warnings}`** - There are no errors in the module.

- **`{error,Errors,Warnings}`** - There are errors in the module.

As this module is of interest only to the maintainers of the compiler, and to
avoid the same description in two places, the elements of `Options` that control
the warnings are only described in the [`compile`](`m:compile#erl_lint_options`)
module.

`AbsForms` of a module, which comes from a file that is read through `epp`, the
Erlang preprocessor, can come from many files. This means that any references to
errors must include the filename, see the `m:epp` module or parser (see the
`m:erl_parse` module). The returned errors and warnings have the following
format:

```text
[{SourceFile,[ErrorInfo]}]
```

The errors and warnings are listed in the order in which they are encountered in
the forms. The errors from one file can therefore be split into different
entries in the list of errors.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
