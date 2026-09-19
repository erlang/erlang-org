# `error_handler`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/error_handler.erl#L22)

Default system error handler.

This module defines what happens when certain types of errors occur.

You can change the error handler of a process by calling
[`erlang:process_flag(error_handler, NewErrorHandler)`](`erlang#process_flag_error_handler`).

## Notes

The code in `error_handler` is complex. Do not change it without fully
understanding the interaction between the error handler, the `init` process of
the code server, and the I/O mechanism of the code.

Code changes that seem small can cause a deadlock, as unforeseen consequences
can occur. The use of `input` is dangerous in this type of code.

# `raise_undef_exception`
*since OTP R16B* 

```erlang
-spec raise_undef_exception(Module, Function, Args) -> no_return()
                               when Module :: atom(), Function :: atom(), Args :: list().
```

Raises an `undef` exception with a stacktrace, indicating that
`Module:Function/N` is undefined.

# `undefined_function`

```erlang
-spec undefined_function(Module, Function, Args) -> any()
                            when Module :: atom(), Function :: atom(), Args :: list().
```

This function is called by the runtime system if a call is made to
`Module:Function(Arg1,.., ArgN)` and `Module:Function/N` is undefined. Notice
that this function is evaluated inside the process making the original call.

This function first attempts to autoload `Module`. If that is not possible, an
`undef` exception is raised.

If it is possible to load `Module` and function `Function/N` is exported, it is
called.

Otherwise, if function `'$handle_undefined_function'/2` is exported, it is
called as `'$handle_undefined_function'(`Function, Args).

> #### Warning {: .warning }
>
> Defining `'$handle_undefined_function'/2` in ordinary application code is
> highly discouraged. It is very easy to make subtle errors that can take a long
> time to debug. Furthermore, none of the tools for static code analysis (such
> as Dialyzer and Xref) supports the use of `'$handle_undefined_function'/2` and
> no such support will be added. Only use this function after having carefully
> considered other, less dangerous, solutions. One example of potential
> legitimate use is creating stubs for other sub-systems during testing and
> debugging.

Otherwise an `undef` exception is raised.

# `undefined_lambda`

```erlang
-spec undefined_lambda(Module, Fun, Args) -> term() when Module :: atom(), Fun :: fun(), Args :: list().
```

This function is evaluated if a call is made to `Fun(Arg1,.., ArgN)` when the
module defining the fun is not loaded. The function is evaluated inside the
process making the original call.

If `Module` is interpreted, the interpreter is invoked and the return value of
the interpreted `Fun(Arg1,.., ArgN)` call is returned.

Otherwise, it returns, if possible, the value of [`apply(Fun, Args)`](`apply/2`)
after an attempt is made to autoload `Module`. If this is not possible, the call
fails with exit reason `undef`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
