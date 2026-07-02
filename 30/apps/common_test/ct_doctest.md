# `ct_doctest`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_doctest.erl#L22)

`ct_doctest` runs doctests on documentation examples. Using `ct_doctest` ensures that the examples
in the documentation are correct, up to date, and stylistically consistent.

The tested examples can be either in a module (normally written using
[documentation attributes](`e:system:documentation.md`)) or in files.
By default `ct_doctest` looks for markdown code blocks and runs any Erlang
code block found that looks like a shell session.

The doctest parser looks for examples that are formatted as if they were run in the
Erlang shell, using prompts of the form `N>`, where `N` starts at `1` for each block.
The expected output is written on the lines following the prompt. For example:

    -doc """
    This is an example of a doctest:
    
    ```
    1> 1+2.
    3
    ```
    """.

`ct_doctest` can be used in Common Test suites to validate documentation examples as part of your
test runs. Normal usage is to call `module/1` with a module name. For example:

```
all() ->
    [doctests].
doctests(_Config) ->
    ct_doctest:module(my_module).
```

## Prompt format rules

For a code block to run as a doctest:

- prompts must start at `1>` for each block
- each subsequent prompt must increment (`2>`, `3>`, ...)
- continuation lines must be indented
- `%` style comment lines are allowed in prompt blocks
- mismatched prompt numbering causes a doctest parse error

## Troubleshooting

If a doctest fails unexpectedly:

- use `verbose` to print per-block execution details
- verify that expected output matches the shell output exactly
- verify prompt numbering and continuation-line indentation

## Examples

Below are examples of supported formats for the code blocks in the documentation. The parser
is quite flexible and supports various styles, including multi-line expressions, comments,
and even prebound variables.

### Basic example

```
1> 1+2.
3
```

### Basic example using Erlang code

This example uses an explicit Erlang code block. That is,

    ```erlang
    1> 1+2.
    3
    ```

instead of the previous one which is a generic code block. Both formats are supported.

```
1> 1+2.
3
```

### Multi-line prompt

Use multiline prompts for expressions that span multiple lines by starting the prompt with `>` and indenting the continuation lines. For example:

```
1> 1
  +
  2
  .
3
```

### Multi-line with comma

It is possible to have multiple expressions in the same prompt, separated by commas. For example:

```
1> A = 1,
  A + 2.
3
```

### Multi-line match

The expected output can span multiple lines. For example:

```
1> [1, 2].
[
 1
 ,
 2
 ]
```

### Multiple prompts

Examples can have multiple prompts. For example:

```
1> 1 + 2.
3
2> 3 + 4.
7
```

### Defining variables

Any variable defined in the examples will be available in the following prompts. For example:

```
1> A = 1 + 2.
3
2> A + 3.
6
```

### Prebound variables

If the documentation examples rely on certain variables being prebound, you can provide these
bindings when calling `module/3`. For example, if you have a module
doc that uses a variable `Prebound`, you can set it up like this:

```
1> Prebound.
hello
```

and then in your test suite:

```
binding_test(_Config) ->
    Bindings = [{moduledoc, #{'Prebound' => hello}}],
    ct_doctest:module(my_module, Bindings, []).
```

### Ignore result

To ignore the results of a prompt, just skip writing the expected output. For example:

```
1> 1 + 2.
2> 3 + 4.
7
```

### Matching exceptions

Examples of failures can be tested by writing the expected exception after the prompt. For example:

```
1> hello + 1.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as hello + 1
2> lists:last([]).
** exception error: no function clause matching lists:last([])
```

The simplest way to know what output to write is to run the example in the shell and copy the output,
including the `** exception` line.

If you don't want to include the entire exception message, use only the start of the message.

```
1> hello + 1.
** exception error
```

### Comments

Comments can be inserted anywhere in the code block. For example:

```erlang
%% A comment before the first prompt
1> [1,
%% A comment between prompts
  2].
[1,
%% A comment in a match
 2]
2> [1,
  %% Indented comment between prompts
  2].
[1,
 %% Indented comment in a match
 2]
3> """
  %% A comment in a string is not a comment
  
  """.
"""
%% A comment in a string is not a comment

"""
4> 1 + a.
** exception error: an error occurred when evaluating an arithmetic expression
%% Comments
     in operator  +/2
     %% in exceptions
        called as 1 + a
%% are ignored
```

### Matching of maps

When matching on maps, it is possible to use shell syntax, that is, `=>` and not `:=`, as in
normal Erlang code. For example:

```
1> #{ a => b }.
#{ a => b }
```

### Matching of ...

It is possible to use `...` in the expected output to indicate that the rest of the output
should be ignored. This is useful for outputs that are large or contain non-deterministic elements.

```
1> lists:seq(1,100).
[1, 2, 3, ...]
2> #{ a => b }.
#{ a => ... }
3> <<1, 0:1024>>.
<<1, 0, 0, 0, ...>>
```

### Compiling modules

ct_doctest can also compile full module code examples. It then looks for a
`-module` declaration to determine the module name and compiles the code as
if it were in a file. For example:

```
-module(my_module).
-export([foo/0]).
foo() ->
    ok.
```

The module is then available for use in following prompts. For example:

```
1> my_module:foo().
```

### Edge cases

The following are examples that are not supported by the parser and will be ignored.

```
a> should not be tested
```

```
 1> should not be tested
```

```
> should not be tested
```

```
should not be tested
1> 
```

# `options`
*not exported* *since OTP 29.0* 

```erlang
-type options() ::
          [{parser, fun((unicode:unicode_binary()) -> [unicode:unicode_binary()] | {error, term()})} |
           {skipped_blocks, non_neg_integer() | false} |
           {missing_tests, [{atom(), arity()}]} |
           {skip_tests, [moduledoc | {function | type | callback, atom(), arity()}]} |
           {verbose, boolean()}].
```

Options for doctest execution.

* `parser` - Use this option to plug in an external documentation parser. The
  parser callback must be a `fun/1` and return a list of Erlang code block binaries.
  The code blocks are then checked to determine whether they should be run as doctests.
  If no parser is provided, a built-in markdown parser will be used.

* `skipped_blocks` - Sets the exact number of Erlang code blocks that are allowed
  to be skipped because no runnable shell prompts were found. It does not count blocks
  in any function listed in `missing_tests`. It defaults to `false`.

* `missing_tests` - A list of `{Function, Arity}` pairs that are expected to have
  documentation but no doctests. When this option is set, `ct_doctest` will fail
  if any documented function lacks doctests and is not in this list (i.e., a new
  function was added without doctests), and also fail if a function in this list
  now has doctests (i.e., the list is stale and should be updated). Defaults to
  not checking.

* `skip_tests` - A list of doc entries whose doctests should be skipped. Each entry
  is either `moduledoc` or a `{Kind, Name, Arity}` tuple where `Kind` is
  `function`, `type`, or `callback`. For example,
  `[moduledoc, {function, foo, 1}]` skips the moduledoc and the `foo/1` function.

* `verbose` - Print detailed information while running doctests, including each
  block run and skipped block details.

# `file`
*since OTP 29.0* 

```erlang
-spec file(file:filename()) -> ok | {error, term()} | no_return().
```

# `file`
*since OTP 29.0* 

```erlang
-spec file(file:filename(), options()) -> ok | {comment, string()} | {error, term()} | no_return().
```

# `file`
*since OTP 29.0* 

```erlang
-spec file(File :: file:filename(), Bindings :: [{atom(), term()}], Options :: options()) ->
              ok | {comment, string()} | {error, term()} | no_return().
```

Run doctests for a markdown file.

The function returns `ok` if all tests pass. If any test fails, an exception in the form of
`error({N, errors})` is raised, where `N` is the number of failed tests. The details of each
failure are printed to the console.

Use `Bindings` to provide prebound variables. Bindings are global for all files, so take
care to avoid any naming conflicts.

You can run doctests on non-markdown files by providing a custom parser that extracts the
code blocks to be tested.

See `t:options/0` for available options.

# `module`
*since OTP 29.0* 

```erlang
-spec module(module()) -> ok | {comment, string()} | {error, term()} | no_return().
```

# `module`
*since OTP 29.0* 

```erlang
-spec module(module(), options()) -> ok | {comment, string()} | {error, term()} | no_return().
```

# `module`
*since OTP 29.0* 

```erlang
-spec module(Module :: module(), Bindings, Options :: options()) ->
                ok | {comment, string()} | {error, term()} | no_return()
                when
                    KFA :: {Kind :: function | type | callback, atom(), arity()},
                    Bindings :: [{KFA | moduledoc, erl_eval:binding_struct()}].
```

Run tests for the documentation in a module with EEP-48 docs.

When calling `module/3`, `ct_doctest` looks for documentation in the specified module and
runs any examples found there. The module, function, type, and callback documentation
are all checked for examples.

The function returns `ok` if all tests pass, or `{comment, Comment}` if all tests pass but one or more
functions lack tests. If any test fails, an exception in the form of `error({N, errors})` is raised,
where `N` is the number of failed tests. The details of each failure are printed to the console.

Use `Bindings` to provide prebound variables for a specific doc entry. Use
`moduledoc` for module docs and `{function, Name, Arity}` (or corresponding
`type`/`callback` keys) for entry-specific bindings.

See `t:options/0` for available options.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
