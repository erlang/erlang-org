# `erl_error`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_error.erl#L22)

This module provides functions for pretty-printing errors and exceptions. It is
used by both the `m:shell` and by `m:proc_lib` to print exceptions.

It is possible for the module raising an error to provide additional information
by calling [`error/3`](`erlang:error/3`) with extra error information. More
details about this mechanism is described in
[EEP-54](https://www.erlang.org/erlang-enhancement-proposals/eep-0054.html).

# `column`
*not exported* *since OTP 24.0* 

```erlang
-type column() :: pos_integer().
```

Start column number. Default is 1.

# `format_fun`
*since OTP 24.0* 

```erlang
-type format_fun() :: fun((term(), column()) -> iolist()).
```

A fun used to format function arguments for BIF and function calls. By default
the following fun will be used:

```erlang
fun(Term, I) -> io_lib:print(Term, I, 80, 30) end
```

# `format_options`
*since OTP 24.0* 

```erlang
-type format_options() ::
          #{column => column(), stack_trim_fun => stack_trim_fun(), format_fun => format_fun()}.
```

A map with formatting options.

# `stack_trim_fun`
*since OTP 24.0* 

```erlang
-type stack_trim_fun() :: fun((module(), atom(), arity()) -> boolean()).
```

A fun used to trim the end of the stacktrace. It is called with module,
function, and arity from an entry from the stacktrace. The fun is to return
`true` if the entry should be trimmed, and `false` otherwise. The default value
is:

```text
fun(_, _, _) -> false end
```

# `format_error`
*since OTP 24.0* 

```erlang
-callback format_error(Reason, StackTrace) -> ErrorDescription
                          when
                              Reason :: term(),
                              StackTrace :: erlang:stacktrace(),
                              ArgumentPosition :: pos_integer(),
                              ErrorDescription ::
                                  #{ArgumentPosition => unicode:chardata(),
                                    general => unicode:chardata(),
                                    reason => unicode:chardata()}.
```

This callback is called when `format_exception/4` or similar functionality wants
to provide extra information about an error. The `Module`:`Function` called is
the one specificed by the `error_info` map.

The function should return a map with additional information about what have
caused the exception. The possible keys of the map are:

- **`ArgumentPosition = pos_integer()`** - The position of the argument that
  caused the error starting at 1.

- **`general`** - An error that is not associated with any argument caused the
  error.

- **`reason`** - If the `Reason` should be printed differently than the default
  way.

If the text returned includes new-lines, `format_exception/4` will indent the
text correctly.

Example:

```erlang
-module(my_error_module).
-export([atom_to_string/1, format_error/2]).

atom_to_string(Arg) when is_atom(Arg) ->
  atom_to_list(Arg);
atom_to_string(Arg) ->
  erlang:error(badarg,[Arg],
               [{error_info,#{ module => ?MODULE,
                               cause => #{ 1 => "should be an atom" }}}]).

format_error(Reason, [{_M,_F,_As,Info}|_]) ->
  ErrorInfo = proplists:get_value(error_info, Info, #{}),
  ErrorMap = maps:get(cause, ErrorInfo),
  ErrorMap#{ general => "optional general information",
             reason => io_lib:format("~p: ~p",[?MODULE, Reason]) }.
```

```erlang
1> c(my_error_module).
{ok,my_error_module}
2> my_error_module:atom_to_string(1).
** exception error: my_error_module: badarg
     in function  my_error_module:atom_to_string/1
        called as my_error_module:atom_to_string(1)
        *** argument 1: should be an atom
        *** optional general information
```

# `format_exception`
*since OTP 24.0* 

```erlang
-spec format_exception(Class, Reason, StackTrace) -> unicode:chardata()
                          when
                              Class :: error | exit | throw,
                              Reason :: term(),
                              StackTrace :: erlang:stacktrace().
```

# `format_exception`
*since OTP 24.0* 

```erlang
-spec format_exception(Class, Reason, StackTrace, Options) -> unicode:chardata()
                          when
                              Class :: error | exit | throw,
                              Reason :: term(),
                              StackTrace :: erlang:stacktrace(),
                              Options :: format_options().
```

Format the error reason and stack back-trace caught using `try` ... `catch` in
the same style as the shell formats them.

Example:

```erlang
try
    do_something()
catch
    C:R:Stk ->
        Message = erl_error:format_exception(C, R, Stk),
        io:format(LogFile, "~ts\n", [Message])
end
```

If `error_info` is provided with the exception, `format_exception` will use that
information to provide additional information about the exception.

Example:

```erlang
try
  erlang:raise(badarg,[],[{error_info,#{}}])
catch
    C:R:Stk ->
        Message = erl_error:format_exception(C, R, Stk),
        io:format(LogFile, "~ts\n", [Message])
end
```

See `erlang:error/3` for details on how to raise an exception with `error_info`
included.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
