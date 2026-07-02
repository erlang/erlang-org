# `merl`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/syntax_tools/src/merl.erl#L36)

Metaprogramming in Erlang.

Merl is a user-friendly interface to the `erl_syntax` module,
making it easy both to build new ASTs from scratch and to match and
decompose existing ASTs. For details that are outside the scope of
Merl itself, see the documentation of `m:erl_syntax`.

### Quick start

To enable the full power of Merl, your module needs to include the Merl header
file:

```erlang
-include_lib("syntax_tools/include/merl.hrl").
```

Then, you can use the `?Q(Text)` macros in your code to create ASTs or match on
existing ASTs. For example:

```erlang
Tuple = ?Q("{foo, 42}"),
?Q("{foo, _@Number}") = Tuple,
Call = ?Q("foo:bar(_@Number)")
```

Calling `merl:print(Call)` will then print the following code:

```erlang
foo:bar(42)
```

The `?Q` macros turn the quoted code fragments into ASTs, and lifts
metavariables such as `_@Tuple` and `_@Number` to the level of your Erlang code,
so you can use the corresponding Erlang variables `Tuple` and `Number` directly.
This is the most straightforward way to use Merl, and in many cases it's all you
need.

You can even write case switches using `?Q` macros as patterns. For example:

```erlang
case AST of
    ?Q("{foo, _@Foo}") -> handle(Foo);
    ?Q("{bar, _@Bar}") when erl_syntax:is_integer(Bar) -> handle(Bar);
    _ -> handle_default()
end
```

These case switches only allow `?Q(...)` or `_` as clause patterns, and the
guards may contain any expressions, not just Erlang guard expressions.

If the macro `MERL_NO_TRANSFORM` is defined before the `merl.hrl` header file is
included, the parse transform used by Merl will be disabled, and in that case,
the match expressions `?Q(...) = ...`, case switches using `?Q(...)` patterns,
and automatic metavariables like `_@Tuple` cannot be used in your code, but the
Merl macros and functions still work. To do metavariable substitution, you need
to use the `?Q(Text, Map)` macro. For example:

```erlang
Tuple = ?Q("{foo, _@bar, _@baz}", [{bar, Bar}, {baz,Baz}])
```

The text given to a `?Q(Text)` macro can be either a single string or a list of
strings. The latter is useful when you need to split a long expression over
multiple lines. For example:

```erlang
?Q(["case _@Expr of",
    "  {foo, X} -> f(X);",
    "  {bar, X} -> g(X)",
    "  _ -> h(X)"
"end"])
```

If there is a syntax error somewhere in the text (like the missing semicolon in
the second clause above) this allows Merl to generate an error message pointing
to the exact line in your source code. (Just remember to comma-separate the
strings in the list, otherwise Erlang will concatenate the string fragments as
if they were a single string.)

### Metavariable syntax

There are several ways to write a metavariable in your quoted code:

- Atoms starting with `@`, for example `'@foo'` or `'@Foo'`
- Variables starting with `_@`, for example `_@bar` or `_@Bar`
- Strings starting with `"'@`, for example `"'@File"`
- Integers starting with 909, for example `9091` or `909123`

Following the prefix, one or more `_` or `0` characters may be used to indicate
"lifting" of the variable one or more levels, and after that, a `@` or `9`
character indicates a glob metavariable (matching zero or more elements in a
sequence) rather than a normal metavariable. For example:

- `'@_foo'` is lifted one level, and `_@__foo` is lifted two levels
- `_@@bar` is a glob variable, and `_@_@bar` is a lifted glob variable
- `90901` is a lifted variable,`90991` is a glob variable, and `9090091` is a
  glob variable lifted two levels

(Note that the last character in the name is never considered to be a lift or
glob marker, hence, `_@__` and `90900` are only lifted one level, not two. Also
note that globs only matter for matching; when doing substitutions, a non-glob
variable can be used to inject a sequence of elements, and vice versa.)

If the name after the prefix and any lift and glob markers is `_` or `0`, the
variable is treated as an anonymous catch-all pattern in matches. For example,
`_@_`, `_@@_`, `_@__`, or even `_@__@_`.

Finally, if the name without any prefixes or lift/glob markers begins with an
uppercase character, as in `_@Foo` or `_@_@Foo`, it will become a variable on
the Erlang level, and can be used to easily deconstruct and construct syntax
trees:

```erlang
case Input of
    ?Q("{foo, _@Number}") -> ?Q("foo:bar(_@Number)");
    ...
```

We refer to these as "automatic metavariables". If in addition the name ends
with `@`, as in `_@Foo@`, the value of the variable as an Erlang term will be
automatically converted to the corresponding abstract syntax tree when used to
construct a larger tree. For example, in:

```erlang
Bar = {bar, 42},
Foo = ?Q("{foo, _@Bar@}")
```

(where Bar is just some term, not a syntax tree) the result `Foo` will be a
syntax tree representing `{foo, {bar, 42}}`. This avoids the need for temporary
variables in order to inject data, as in

```erlang
TmpBar = erl_syntax:abstract(Bar),
Foo = ?Q("{foo, _@TmpBar}")
```

If the context requires an integer rather than a variable, an atom, or a string,
you cannot use the uppercase convention to mark an automatic metavariable.
Instead, if the integer (without the `909`\-prefix and lift/glob markers) ends
in a `9`, the integer will become an Erlang-level variable prefixed with `Q`,
and if it ends with `99` it will also be automatically abstracted. For example,
the following will increment the arity of the exported function f:

```erlang
case Form of
    ?Q("-export([f/90919]).") ->
        Q2 = erl_syntax:concrete(Q1) + 1,
        ?Q("-export([f/909299]).");
    ...
```

### When to use the various forms of metavariables

Merl can only parse a fragment of text if it follows the basic syntactical rules
of Erlang. In most places, a normal Erlang variable can be used as metavariable,
for example:

```erlang
?Q("f(_@Arg)") = Expr
```

but if you want to match on something like the name of a function, you have to
use an atom as metavariable:

```erlang
?Q("'@Name'() -> _@@_." = Function
```

(note the anonymous glob variable `_@@_` to ignore the function body).

In some contexts, only a string or an integer is allowed. For example, the
directive `-file(Name, Line)` requires that `Name` is a string literal and
`Line` an integer literal:

```erlang
?Q("-file(\"'@File\", 9090).") = ?Q("-file(\"foo.erl\", 42).")).
```

This will extract the string literal `"foo.erl"` into the variable `Foo`. Note
the use of the anonymous variable `9090` to ignore the line number. To match and
also bind a metavariable that must be an integer literal, we can use the
convention of ending the integer with a 9, turning it into a Q-prefixed variable
on the Erlang level (see the previous section).

#### Globs

Whenever you want to match out a number of elements in a sequence (zero or more)
rather than a fixed set of elements, you need to use a glob. For example:

```erlang
?Q("{_@@Elements}") = ?Q({a, b, c})
```

will bind Elements to the list of individual syntax trees representing the atoms
`a`, `b`, and `c`. This can also be used with static prefix and suffix elements
in the sequence. For example:

```erlang
?Q("{a, b, _@@Elements}") = ?Q({a, b, c, d})
```

will bind Elements to the list of the `c` and `d` subtrees, and

```erlang
?Q("{_@@Elements, c, d}") = ?Q({a, b, c, d})
```

will bind Elements to the list of the `a` and `b` subtrees. You can even use
plain metavariables in the prefix or suffix:

```erlang
?Q("{_@First, _@@Rest}") = ?Q({a, b, c})
```

or

```erlang
?Q("{_@@_, _@Last}") = ?Q({a, b, c})
```

(ignoring all but the last element). However, you cannot have two globs as part
of the same sequence.

#### Lifted metavariables

In some cases, the Erlang syntax rules make it impossible to place a
metavariable directly where you would like it. For example, you cannot write:

```erlang
?Q("-export([_@@Name]).")
```

to match out all name/arity pairs in the export list, or to insert a list of
exports in a declaration, because the Erlang parser only allows elements on the
form `A/I` (where `A` is an atom and `I` an integer) in the export list. A
variable like the above is not allowed, but neither is a single atom or integer,
so `'@@Name'` or `909919` would not work either.

What you have to do in such cases is to write your metavariable in a
syntactically valid position, and use lifting markers to denote where it should
really apply, as in:

```erlang
?Q("-export(['@_@Name'/0]).")
```

This causes the variable to be lifted (after parsing) to the next higher level
in the syntax tree, replacing that entire subtree. In this case, the
`'@_@Name'/0` will be replaced with `'@@Name'`, and the `/0` part was just used
as dummy notation and will be discarded.

You may even need to apply lifting more than once. To match the entire export
list as a single syntax tree, you can write:

```erlang
?Q("-export(['@__Name'/0]).")
```

using two underscores, but with no glob marker this time. This will make the
entire `['@__Name'/0]` part be replaced with `'@Name'`.

Sometimes, the tree structure of a code fragment is not very obvious, and parts
of the structure may be invisible when printed as source code. For instance, a
simple function definition like the following:

```erlang
zero() -> 0.
```

consists of the name (the atom `zero`), and a list of clauses containing the
single clause `() -> 0`. The clause consists of an argument list (empty), a
guard (empty), and a body (which is always a list of expressions) containing the
single expression `0`. This means that to match out the name and the list of
clauses of any function, you'll need to use a pattern like
`?Q("'@Name'() -> _@_@Body.")`, using a dummy clause whose body is a glob lifted
one level.

To visualize the structure of a syntax tree, you can use the function
`merl:show(T)`, which prints a summary. For example, entering

```erlang
merl:show(merl:quote("inc(X, Y) when Y > 0 -> X + Y."))
```

in the Erlang shell will print the following (where the `+` signs separate
groups of subtrees on the same level):

```text
function: inc(X, Y) when ... -> X + Y.
  atom: inc
  +
  clause: (X, Y) when ... -> X + Y
    variable: X
    variable: Y
    +
    disjunction: Y > 0
      conjunction: Y > 0
        infix_expr: Y > 0
          variable: Y
          +
          operator: >
          +
          integer: 0
    +
    infix_expr: X + Y
      variable: X
      +
      operator: +
      +
      variable: Y
```

This shows another important non-obvious case: a clause guard, even if it's as
simple as `Y > 0`, always consists of a single disjunction of one or more
conjunctions of tests, much like a tuple of tuples. Thus:

- `"when _@Guard ->"` will only match a guard with exactly one test
- `"when _@@Guard ->"` will match a guard with one or more comma-separated tests
  (but no semicolons), binding `Guard` to the list of tests
- `"when _@_Guard ->"` will match just like the previous pattern, but binds
  `Guard` to the conjunction subtree
- `"when _@_@Guard ->"` will match an arbitrary nonempty guard, binding `Guard`
  to the list of conjunction subtrees
- `"when _@__Guard ->"` will match like the previous pattern, but binds `Guard`
  to the whole disjunction subtree
- and finally, `"when _@__@Guard ->"` will match any clause, binding `Guard` to
  `[]` if the guard is empty and to `[Disjunction]` otherwise

Thus, the following pattern matches all possible clauses:

```erlang
     "(_@Args) when _@__@Guard -> _@Body"
```

# `default_action`
*not exported* 

```elixir
-type default_action() :: fun(() -> any()).
```

# `env`
*not exported* 

```elixir
-type env() :: [{Key :: id(), pattern_or_patterns()}].
```

# `guard_test`
*not exported* 

```elixir
-type guard_test() :: fun((env()) -> boolean()).
```

# `guarded_action`
*not exported* 

```elixir
-type guarded_action() :: switch_action() | {guard_test(), switch_action()}.
```

# `guarded_actions`
*not exported* 

```elixir
-type guarded_actions() :: guarded_action() | [guarded_action()].
```

# `id`
*not exported* 

```elixir
-type id() :: atom() | integer().
```

# `location`
*not exported* 

```elixir
-type location() :: erl_anno:location().
```

# `pattern`
*not exported* 

```elixir
-type pattern() :: tree() | template().
```

# `pattern_or_patterns`
*not exported* 

```elixir
-type pattern_or_patterns() :: pattern() | [pattern()].
```

# `switch_action`
*not exported* 

```elixir
-type switch_action() :: fun((env()) -> any()).
```

# `switch_clause`
*not exported* 

```elixir
-type switch_clause() ::
          {pattern_or_patterns(), guarded_actions()} |
          {pattern_or_patterns(), guard_test(), switch_action()} |
          default_action().
```

# `template`
*not exported* 

```elixir
-type template() :: tree() | {id()} | {'*', id()} | {template, atom(), term(), [[template()]]}.
```

# `template_or_templates`
*not exported* 

```elixir
-type template_or_templates() :: template() | [template()].
```

# `text`
*not exported* 

```elixir
-type text() :: string() | binary() | [string()] | [binary()].
```

# `tree`
*not exported* 

```elixir
-type tree() :: erl_syntax:syntaxTree().
```

# `tree_or_trees`
*not exported* 

```elixir
-type tree_or_trees() :: tree() | [tree()].
```

# `alpha`

```elixir
-spec alpha(pattern_or_patterns(), [{id(), id()}]) -> template_or_templates().
```

Alpha converts a pattern (renames variables).

Similar to tsubst/1, but only renames variables (including globs).

_See also: _`tsubst/2`.

# `compile`

```elixir
-spec compile(tree_or_trees()) -> compile:comp_ret().
```

# `compile`

```elixir
-spec compile(tree_or_trees(), [compile:option()]) -> compile:comp_ret().
```

Compile a syntax tree or list of syntax trees representing a module into a
binary BEAM object.

_See also: _`compile/1`, `compile_and_load/2`.

# `compile_and_load`

```elixir
-spec compile_and_load(tree_or_trees()) ->
                          {ok, binary()} | error | {error, Errors :: list(), Warnings :: list()}.
```

# `compile_and_load`

```elixir
-spec compile_and_load(tree_or_trees(), [compile:option()]) ->
                          {ok, binary()} | error | {error, Errors :: list(), Warnings :: list()}.
```

Compile a syntax tree or list of syntax trees representing a module and load the
resulting module into memory.

_See also: _`compile/2`, `compile_and_load/1`.

# `match`

```elixir
-spec match(pattern_or_patterns(), tree_or_trees()) -> {ok, env()} | error.
```

Match a pattern against a syntax tree (or patterns against syntax trees)
returning an environment mapping variable names to subtrees; the environment is
always sorted on keys.

> #### Note {: .info }
>
> Multiple occurrences of metavariables in the pattern is not
> allowed, but is not checked.

_See also: _`switch/2`, `template/1`.

# `meta_template`

```elixir
-spec meta_template(template_or_templates()) -> tree_or_trees().
```

Turn a template into a syntax tree representing the template.

Meta-variables in the template are turned into normal Erlang variables
if their names (after the metavariable prefix characters) begin with
an uppercase character. For example, `_@Foo` in the template becomes the
variable `Foo` in the meta-template. Furthermore, variables ending
with `@` are automatically wrapped in a call to merl:term/1, so
`_@Foo@` in the template becomes `merl:term(Foo)` in the
meta-template.

# `print`

```elixir
-spec print(tree_or_trees()) -> ok.
```

Pretty-print a syntax tree or template to the standard output.

This is a utility function for development and debugging.

# `qquote`

```elixir
-spec qquote(Text :: text(), Env :: env()) -> tree_or_trees().
```

# `qquote`

```elixir
-spec qquote(StartPos :: location(), Text :: text(), Env :: env()) -> tree_or_trees().
```

Parse text and substitute meta-variables.

Takes an initial scanner starting position as first argument.

The macro `?Q(Text, Env)` expands to `merl:qquote(?LINE, Text, Env)`.

_See also: _`quote/2`.

# `quote`

```elixir
-spec quote(Text :: text()) -> tree_or_trees().
```

# `quote`

```elixir
-spec quote(StartPos :: location(), Text :: text()) -> tree_or_trees().
```

Parse text.

Takes an initial scanner starting position as first argument.

The macro `?Q(Text)` expands to `merl:quote(?LINE, Text)`.

_See also: _`quote/1`.

# `show`

```elixir
-spec show(tree_or_trees()) -> ok.
```

Print the structure of a syntax tree or template to the standard output.

This is a utility function for development and debugging.

# `subst`

```elixir
-spec subst(pattern_or_patterns(), env()) -> tree_or_trees().
```

Substitute metavariables in a pattern or list of patterns, yielding a syntax
tree or list of trees as result.

Both for normal metavariables and glob metavariables, the substituted
value may be a single element or a list of elements. For example, if a
list representing `1, 2, 3` is substituted for `var` in either of
`[foo, _@var, bar]` or `[foo, _@var, bar]`, the result represents
`[foo, 1, 2, 3, bar]`.

# `switch`

```elixir
-spec switch(tree_or_trees(), [switch_clause()]) -> any().
```

Match against one or more clauses with patterns and optional guards.

Note that clauses following a default action will be ignored.

_See also: _`match/2`.

# `template`

```elixir
-spec template(pattern_or_patterns()) -> template_or_templates().
```

Turn a syntax tree or list of trees into a template or templates.

Templates can be instantiated or matched against, and reverted back to
normal syntax trees using `tree/1`. If the input is already a
template, it is not modified further.

_See also: _`match/2`, `subst/2`, `tree/1`.

# `template_vars`

```elixir
-spec template_vars(template_or_templates()) -> [id()].
```

Return an ordered list of the metavariables in the template.

# `term`

```elixir
-spec term(term()) -> tree().
```

Create a syntax tree for a constant term.

# `tree`

```elixir
-spec tree(template_or_templates()) -> tree_or_trees().
```

Revert a template to a normal syntax tree.

Any remaining metavariables are turned into `@`\-prefixed atoms or
`909`\-prefixed integers.

_See also: _`template/1`.

# `tsubst`

```elixir
-spec tsubst(pattern_or_patterns(), env()) -> template_or_templates().
```

Like `subst/2`, but does not convert the result from a template back to a tree.

Useful if you want to do multiple separate substitutions.

_See also: _`subst/2`, `tree/1`.

# `var`

```elixir
-spec var(atom()) -> tree().
```

Create a variable.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
