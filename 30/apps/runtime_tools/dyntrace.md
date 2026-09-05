# `dyntrace`
[🔗](https://github.com/erlang/otp/blob/master/lib/runtime_tools/src/dyntrace.erl#L23)

Interface to dynamic tracing

This module implements interfaces to dynamic tracing, should such be compiled
into the virtual machine. For a standard and/or commercial build, no dynamic
tracing is available, in which case none of the functions in this module is
usable or give any effect.

Should dynamic tracing be enabled in the current build, either by configuring
with `./configure --with-dynamic-trace=dtrace` or with
`./configure --with-dynamic-trace=systemtap`, the module can be used for two
things:

- Trigger the user-probe `user_trace_i4s4` in the NIF library `dyntrace.so` by
  calling `dyntrace:p/{1,2,3,4,5,6,7,8}`.
- Set a user specified tag that will be present in the trace messages of both
  the `efile_drv` and the user-probe mentioned above.

Both building with dynamic trace probes and using them is experimental and
unsupported by Erlang/OTP. It is included as an option for the developer to
trace and debug performance issues in their systems.

The original implementation is mostly done by Scott Lystiger Fritchie as an Open
Source Contribution and it should be viewed as such even though the source for
dynamic tracing as well as this module is included in the main distribution.
However, the ability to use dynamic tracing of the virtual machine is a very
valuable contribution which OTP has every intention to maintain as a tool for
the developer.

How to write `d` programs or `systemtap` scripts can be learned from books and
from a lot of pages on the Internet. This manual page does not include any
documentation about using the dynamic trace tools of respective platform.
However, the `examples` directory of the `runtime_tools` application contains
comprehensive examples of both `d` and `systemtap` programs that will help you
get started. Another source of information is the [dtrace](dtrace.md) and
[systemtap](systemtap.md) chapters in the Runtime Tools Users' Guide.

# `probe_arg`
*not exported* *since OTP R15B01* 

```erlang
-type probe_arg() :: integer() | iolist().
```

# `available`
*since OTP R15B01* 

```erlang
-spec available() -> true | false.
```

This function uses the NIF library to determine if dynamic tracing is available.

This function will throw an exception if the `dyntrace` NIF library could not be
loaded by the `on_load` function in this module.

Use [`erlang:system_info(dynamic_trace)`](`e:erts:erlang.md#system_info_dynamic_trace`)
to determine whether the run-time system supports dynamic tracing.

# `get_tag`
*since OTP R15B01* 

```erlang
-spec get_tag() -> binary() | undefined.
```

This function returns the user tag set in the current process. If no tag is set
or dynamic tracing is not available, it returns `undefined`.

This function returns the user tag set in the current process or, if no user tag
is present, the last user tag sent to the process together with a message (in
the same way as [sequential trace tokens](`m:seq_trace`) are spread to other
processes together with messages. For an explanation of how user tags can be
spread together with messages, see `spread_tag/1`. If no tag is found or dynamic
tracing is not available, it returns `undefined`

# `p`
*since OTP R15B01* 

```erlang
-spec p() -> true | false | error | badarg.
```

Calling this function triggers the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing only the user tag and
zeroes/empty strings in all other fields.

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg()) -> true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
`dyntrace` NIF module, sending a trace message containing the user tag and the
integer or string parameter in the first integer/string field.

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg()) -> true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

That is, the following calls work:

- [`dyntrace:p(1, "Hello")`](`p/2`)
- [`dyntrace:p(1, 1)`](`p/2`)
- [`dyntrace:p("Hello", "Again")`](`p/2`)

The following call is invalid because the string argument comes before the
integer argument:

- [`dyntrace:p("Hello", 1)`](`p/2`)

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
           true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first parameter must be of type [`integer()`](`t:integer/0`) and
the last parameter of type [`string()`](`t:string/0`).

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
           true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first two parameters must be of type [`integer()`](`t:integer/0`) and
the last two of type [`string()`](`t:string/0`).

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
           true | false | error | badarg.
```

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first three parameters must be of type [`integer()`](`t:integer/0`) and
the last three of type [`string()`](`t:string/0`).

# `p`
*since OTP R15B01* 

```erlang
-spec p(probe_arg(),
        probe_arg(),
        probe_arg(),
        probe_arg(),
        probe_arg(),
        probe_arg(),
        probe_arg(),
        probe_arg()) ->
           true | false | error | badarg.
```

Calling this function will trigger the "user" trace `probe user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing all the
[`integer()`](`t:integer/0`) and [`string()`](`t:string/0`) parameters
provided, as well as any user tag set in the current process.

# `put_tag`
*since OTP R15B01* 

```erlang
-spec put_tag(undefined | iodata()) -> binary() | undefined.
```

This function sets the user tag of the current process.

The user tag is a [`binary()`](`t:binary/0`), but can be specified as
any [`iodata()`](`t:iodata/0`), which is automatically converted to a
binary by this function.

The user tag is provided to the user probes triggered by calls top
`dyntrace:p/{1,2,3,4,5,6,7,8}` as well as probes in the `efile` driver. In the
future, user tags might be added to more probes.

The old user tag (if any) is returned, or `undefined` if no user tag was present,
or dynamic tracing is not enabled.

# `restore_tag`
*since OTP R15B01* 

```erlang
-spec restore_tag(true | {non_neg_integer(), binary() | []}) -> true.
```

Restores the previous state of user tags and their spreading as it was before a
call to `spread_tag/1`.

Note that the restoring is not limited to the same process; one can
utilize this to turn off spreding in one process and restore it in a
newly created process that is is actually going to send messages:

```erlang
f() ->
    TagData = dyntrace:spread_tag(false),
    spawn(fun() ->
             dyntrace:restore_tag(TagData),
             do_something()
          end),
    do_something_else(),
    dyntrace:restore_tag(TagData).
```

Correctly handling user tags and their spreading might take some effort, as
Erlang programs tend to send and receive messages so that sometimes the user tag
gets lost due to various things, like double receives or communication with a
port (ports do not handle user tags, in the same way as they do not handle
regular sequential trace tokens).

# `spread_tag`
*since OTP R15B01* 

```erlang
-spec spread_tag(boolean()) -> true | {non_neg_integer(), binary() | []}.
```

This function controls if user tags are to be spread to other processes with the
next message.

Spreading of user tags work like spreading of sequential trace
tokens, so that a received user tag will be active in the process until the next
message arrives (if that message does not also contain the user tag).

This functionality is used when a client process communicates with a file
i/o-server to spread the user tag to the I/O-server and then down to the
`efile` driver. By using [`spread_tag/1`](`spread_tag/1`) and
[`restore_tag/1`](`restore_tag/1`), one can enable or disable spreading of user
tags to other processes and then restore the previous state of the user tag. The
TagData returned from this call contains all previous information so the state
(including any previously spread user tags) will be completely restored by a
later call to [`restore_tag/1`](`restore_tag/1`).

The `m:file` module already spreads tags, so there is no need to manually call
this function to get user tags spread to the `efile` driver through that module.

The most use of this function would be if one, for example, uses the `m:io` module
to communicate with an I/O-server for a regular file, such as in the following
example:

```erlang
f() ->
   {ok, F} = file:open("test.tst", [write]),
   Saved = dyntrace:spread_tag(true),
   io:format(F, "Hello world!", []),
   dyntrace:restore_tag(Saved),
   file:close(F).
```

In this example, any user tag set in the calling process will be spread to the
I/O-server when the `io:format/3` call is done.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
