# `shell_default`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/shell_default.erl#L26)

Customizing the Erlang environment.

The functions in this module are called when no module name is specified in a
shell command.

Consider the following shell dialog:

```erlang
1> lists:reverse("abc").
"cba"
2> c(foo).
{ok, foo}
```

In command one, module `m:lists` is called. In command two, no module name is
specified. The shell searches module `user_default` followed by module
`shell_default` for function `c/1`.

`shell_default` is intended for "system wide" customizations to the shell.
`user_default` is intended for "local" or individual user customizations.

## Hint

To add your own commands to the shell, create a module called `user_default` and
add the commands you want. Then add the following line as the _first_ line in
your `.erlang` file in your home directory.

```text
code:load_abs("$PATH/user_default").
```

`$PATH` is the directory where your `user_default` module can be found.

# `help`

```erlang
-spec help() -> true.
```

Print the help for all shell commands.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
