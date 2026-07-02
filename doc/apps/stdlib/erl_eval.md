# `erl_eval`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/erl_eval.erl#L22)

The Erlang meta interpreter.

This module provides an interpreter for Erlang expressions. The expressions are
in the abstract syntax as returned by `m:erl_parse`, the Erlang parser, or
`m:io`.

## Local Function Handler

During evaluation of a function, no calls can be made to local functions. An
undefined function error would be generated. However, the optional argument
`LocalFunctionHandler` can be used to define a function that is called when
there is a call to a local function. The argument can have the following
formats:

- **`{value,Func}`** - This defines a local function handler that is called
  with:

  ```erlang
  Func(Name, Arguments)
  ```

  `Name` is the name of the local function (an atom) and `Arguments` is a list
  of the _evaluated_ arguments. The function handler returns the value of the
  local function. In this case, the current bindings cannot be accessed. To
  signal an error, the function handler calls [`exit/1`](`exit/1`) with a
  suitable exit value.

- **`{eval,Func}`** - This defines a local function handler that is called with:

  ```erlang
  Func(Name, Arguments, Bindings)
  ```

  `Name` is the name of the local function (an atom), `Arguments` is a list of
  the _unevaluated_ arguments, and `Bindings` are the current variable bindings.
  The function handler returns:

  ```erlang
  {value,Value,NewBindings}
  ```

  `Value` is the value of the local function and `NewBindings` are the updated
  variable bindings. In this case, the function handler must itself evaluate all
  the function arguments and manage the bindings. To signal an error, the
  function handler calls [`exit/1`](`exit/1`) with a suitable exit value.

- **`none`** - There is no local function handler.

## Non-Local Function Handler

The optional argument `NonLocalFunctionHandler` can be used to define a function
that is called in the following cases:

- A functional object (fun) is called.
- A built-in function is called.
- A function is called using the `M:F` syntax, where `M` and `F` are atoms or
  expressions.
- An operator `Op/A` is called (this is handled as a call to function
  `erlang:Op/A`).

Exceptions are calls to `erlang:apply/2,3`; neither of the function handlers are
called for such calls. The argument can have the following formats:

- **`{value,Func}`** - This defines a non-local function handler. The function
  may be called with two arguments:

  ```erlang
  Func(FuncSpec, Arguments)
  ```

  or three arguments:

  ```erlang
  Func(Anno, FuncSpec, Arguments)
  ```

  `Anno` is the [`erl_anno:anno()`](`t:erl_anno:anno/0`) of the node, `FuncSpec`
  is the name of the function of the form `{Module,Function}` or a fun, and
  `Arguments` is a list of the _evaluated_ arguments. The function handler
  returns the value of the function. To signal an error, the function handler
  calls [`exit/1`](`exit/1`) with a suitable exit value.

- **`none`** - There is no non-local function handler.

> #### Note {: .info }
>
> For calls such as `erlang:apply(Fun, Args)` or
> `erlang:apply(Module, Function, Args)`, the call of the non-local function
> handler corresponding to the call to `erlang:apply/2,3` itself
> (`Func({erlang, apply}, [Fun, Args])` or
> `Func({erlang, apply}, [Module, Function, Args])`) never takes place.
>
> The non-local function handler _is_ however called with the evaluated
> arguments of the call to `erlang:apply/2,3`: `Func(Fun, Args)` or
> `Func({Module, Function}, Args)` (assuming that `{Module, Function}` is not
> `{erlang, apply}`).
>
> Calls to functions defined by evaluating fun expressions `"fun ... end"` are
> also hidden from non-local function handlers.

The non-local function handler argument is probably not used as frequently as
the local function handler argument. A possible use is to call
[`exit/1`](`exit/1`) on calls to functions that for some reason are not allowed
to be called.

# `binding_struct`

```erlang
-type binding_struct() :: orddict:orddict() | map().
```

A binding structure. It is either a `map` or an `orddict`. `erl_eval` will
always return the same type as the one given.

# `bindings`
*not exported* 

```erlang
-type bindings() :: [{name(), value()}].
```

# `expression`
*not exported* 

```erlang
-type expression() :: erl_parse:abstract_expr().
```

# `expression_list`
*not exported* 

```erlang
-type expression_list() :: [expression()].
```

# `expressions`
*not exported* 

```erlang
-type expressions() :: [erl_parse:abstract_expr()].
```

As returned by `erl_parse:parse_exprs/1` or `io:parse_erl_exprs/2`.

# `func_spec`
*not exported* 

```erlang
-type func_spec() :: {Module :: module(), Function :: atom()} | function().
```

# `lfun_eval_handler`
*not exported* 

```erlang
-type lfun_eval_handler() ::
          fun((Name :: atom(), Arguments :: expression_list(), Bindings :: binding_struct()) ->
                  {value, Value :: value(), NewBindings :: binding_struct()}).
```

# `lfun_value_handler`
*not exported* 

```erlang
-type lfun_value_handler() :: fun((Name :: atom(), Arguments :: [term()]) -> Value :: value()).
```

# `local_function_handler`
*not exported* 

```erlang
-type local_function_handler() :: {value, lfun_value_handler()} | {eval, lfun_eval_handler()} | none.
```

Further described in section
[Local Function Handler](`m:erl_eval#module-local-function-handler`) in this module

# `name`
*not exported* 

```erlang
-type name() :: term().
```

# `nlfun_handler`
*not exported* 

```erlang
-type nlfun_handler() ::
          fun((FuncSpec :: func_spec(), Arguments :: [term()]) -> term()) |
          fun((Anno :: erl_anno:anno(), FuncSpec :: func_spec(), Arguments :: [term()]) -> term()).
```

# `non_local_function_handler`
*not exported* 

```erlang
-type non_local_function_handler() :: {value, nlfun_handler()} | none.
```

Further described in section
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.

# `value`
*not exported* 

```erlang
-type value() :: term().
```

# `add_binding`

```erlang
-spec add_binding(Name, Value, BindingStruct) -> binding_struct()
                     when Name :: name(), Value :: value(), BindingStruct :: binding_struct().
```

Adds binding `Name=Value` to `BindingStruct`. Returns an updated binding
structure.

# `binding`

```erlang
-spec binding(Name, BindingStruct) -> {value, value()} | unbound
                 when Name :: name(), BindingStruct :: binding_struct().
```

Returns the binding of `Name` in `BindingStruct`.

# `bindings`

```erlang
-spec bindings(BindingStruct :: binding_struct()) -> bindings().
```

Returns the list of bindings contained in the binding structure.

# `del_binding`

```erlang
-spec del_binding(Name, BindingStruct) -> binding_struct()
                     when Name :: name(), BindingStruct :: binding_struct().
```

Removes the binding of `Name` in `BindingStruct`. Returns an updated binding
structure.

# `expr`

```erlang
-spec expr(Expression, Bindings) -> {value, Value, NewBindings}
              when
                  Expression :: expression(),
                  Bindings :: binding_struct(),
                  Value :: value(),
                  NewBindings :: binding_struct().
```

# `expr`

```erlang
-spec expr(Expression, Bindings, LocalFunctionHandler) -> {value, Value, NewBindings}
              when
                  Expression :: expression(),
                  Bindings :: binding_struct(),
                  LocalFunctionHandler :: local_function_handler(),
                  Value :: value(),
                  NewBindings :: binding_struct().
```

# `expr`

```erlang
-spec expr(Expression, Bindings, LocalFunctionHandler, NonLocalFunctionHandler) ->
              {value, Value, NewBindings}
              when
                  Expression :: expression(),
                  Bindings :: binding_struct(),
                  LocalFunctionHandler :: local_function_handler(),
                  NonLocalFunctionHandler :: non_local_function_handler(),
                  Value :: value(),
                  NewBindings :: binding_struct().
```

# `expr`

```erlang
-spec expr(Expression, Bindings, LocalFunctionHandler, NonLocalFunctionHandler, ReturnFormat) ->
              {value, Value, NewBindings} | Value
              when
                  Expression :: expression(),
                  Bindings :: binding_struct(),
                  LocalFunctionHandler :: local_function_handler(),
                  NonLocalFunctionHandler :: non_local_function_handler(),
                  ReturnFormat :: none | value,
                  Value :: value(),
                  NewBindings :: binding_struct().
```

Evaluates `Expression` with the set of bindings `Bindings`. `Expression` is an
expression in abstract syntax.

For an explanation of when and how to use arguments `LocalFunctionHandler` and
`NonLocalFunctionHandler`, see sections
[Local Function Handler](`m:erl_eval#module-local-function-handler`) and
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.

Returns `{value, Value, NewBindings}` by default. If `ReturnFormat` is `value`,
only `Value` is returned.

# `expr_list`

```erlang
-spec expr_list(ExpressionList, Bindings) -> {ValueList, NewBindings}
                   when
                       ExpressionList :: expression_list(),
                       Bindings :: binding_struct(),
                       ValueList :: [value()],
                       NewBindings :: binding_struct().
```

# `expr_list`

```erlang
-spec expr_list(ExpressionList, Bindings, LocalFunctionHandler) -> {ValueList, NewBindings}
                   when
                       ExpressionList :: expression_list(),
                       Bindings :: binding_struct(),
                       LocalFunctionHandler :: local_function_handler(),
                       ValueList :: [value()],
                       NewBindings :: binding_struct().
```

# `expr_list`

```erlang
-spec expr_list(ExpressionList, Bindings, LocalFunctionHandler, NonLocalFunctionHandler) ->
                   {ValueList, NewBindings}
                   when
                       ExpressionList :: expression_list(),
                       Bindings :: binding_struct(),
                       LocalFunctionHandler :: local_function_handler(),
                       NonLocalFunctionHandler :: non_local_function_handler(),
                       ValueList :: [value()],
                       NewBindings :: binding_struct().
```

Evaluates a list of expressions in parallel, using the same initial bindings for
each expression. Attempts are made to merge the bindings returned from each
evaluation.

This function is useful in `LocalFunctionHandler`, see section
[Local Function Handler](`m:erl_eval#module-local-function-handler`) in this module.

Returns `{ValueList, NewBindings}`.

# `exprs`

```erlang
-spec exprs(Expressions, Bindings) -> {value, Value, NewBindings}
               when
                   Expressions :: expressions(),
                   Bindings :: binding_struct(),
                   Value :: value(),
                   NewBindings :: binding_struct().
```

# `exprs`

```erlang
-spec exprs(Expressions, Bindings, LocalFunctionHandler) -> {value, Value, NewBindings}
               when
                   Expressions :: expressions(),
                   Bindings :: binding_struct(),
                   LocalFunctionHandler :: local_function_handler(),
                   Value :: value(),
                   NewBindings :: binding_struct().
```

# `exprs`

```erlang
-spec exprs(Expressions, Bindings, LocalFunctionHandler, NonLocalFunctionHandler) ->
               {value, Value, NewBindings}
               when
                   Expressions :: expressions(),
                   Bindings :: binding_struct(),
                   LocalFunctionHandler :: local_function_handler(),
                   NonLocalFunctionHandler :: non_local_function_handler(),
                   Value :: value(),
                   NewBindings :: binding_struct().
```

Evaluates `Expressions` with the set of bindings `Bindings`, where `Expressions`
is a sequence of expressions (in abstract syntax) of a type that can be returned
by `io:parse_erl_exprs/2`.

For an explanation of when and how to use arguments
`LocalFunctionHandler` and `NonLocalFunctionHandler`, see sections
[Local Function Handler](`m:erl_eval#module-local-function-handler`) and
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.

Returns `{value, Value, NewBindings}`

# `new_bindings`

```erlang
-spec new_bindings() -> binding_struct().
```

Returns an empty binding structure.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
