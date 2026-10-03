# `edlin_expand`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/edlin_expand.erl#L22)

Shell expansion and formatting of expansion suggestions.

This module provides an expand_fun for the erlang shell
[`expand/1,2`](`expand/1`). It is possible to override this expand_fun
[`io:setopts/1,2`](`io:setopts/1`).

# `expand`
*since OTP 26.0* 

```erlang
-spec expand(Bef0) -> {Res, Completion, Matches}
                when
                    Bef0 :: string(),
                    Res :: yes | no,
                    Completion :: string(),
                    Matches :: [Element] | [Section],
                    Element :: {string(), [ElementOption]},
                    ElementOption :: {ending, string()},
                    Section :: #{title := string(), elems := Matches, options := SectionOption},
                    SectionOption ::
                        {highlight_all} |
                        {highlight, string()} |
                        {highlight_param, integer()} |
                        {hide, title} |
                        {hide, result} |
                        {separator, string()}.
```

# `expand`
*since OTP 26.0* 

```erlang
-spec expand(Bef0, Opts) -> {Res, Completion, Matches}
                when
                    Bef0 :: string(),
                    Opts :: [Option],
                    Option :: {legacy_output, boolean()},
                    Res :: yes | no,
                    Completion :: string(),
                    Matches :: [Element] | [Section],
                    Element :: {string(), [ElementOption]},
                    ElementOption :: {ending, string()},
                    Section :: #{title := string(), elems := Matches, options := SectionOption},
                    SectionOption ::
                        {highlight_all} |
                        {highlight, string()} |
                        {highlight_param, integer()} |
                        {hide, title} |
                        {hide, result} |
                        {separator, string()}.
```

The standard expansion function is able to expand strings to valid erlang terms.
This includes module names:

```text
1> erla
modules
erlang:
```

function names:

```text
1> is_ato
functions
is_atom(
2> erlang:is_ato
functions
is_atom(
```

function types:

```text
1> erlang:is_atom(
typespecs
erlang:is_atom(Term)
any()
```

and automatically add , or closing parenthesis when no other valid expansion is
possible. The expand function also completes: shell bindings, record names,
record fields and map keys.

As seen below, function headers are grouped together if they've got the same
expansion suggestion, in this case all had the same suggestions, that is '\}'.
There is also limited support for filtering out function typespecs that that
does not match the types on the terms on the prompt. Only 4 suggestions are
shown below but there exists plenty more typespecs for `erlang:system_info`.

```text
1> erlang:system_info({allocator, my_allocator
typespecs
erlang:system_info(wordsize | {wordsize, ...} | {wordsize, ...})
erlang:system_info({allocator, ...})
erlang:system_info({allocator_sizes, ...})
erlang:system_info({cpu_topology, ...})
}
```

The return type of `expand` function specifies either a list of `Element` tuples
or a list of `Section` maps. The section concept was introduced to enable more
formatting options for the expansion results. For example, the shell expansion
has support to highlight text and hide suggestions. There are also a
`{highlight, Text}` that highlights all occurances of `Text` in the title, and a
`highlight_all` for simplicity which highlights the whole title, as can be seen
above for `functions` and `typespecs`.

By setting the `{hide, result}` or `{hide, title}` options you may hide
suggestions. Sometimes the title isn't useful and just produces text noise, in
the example above the `t:any/0` result is part of a section with title `Types`.
Hiding results is currently not in use, but the idea is that a section can be
selected in the expand area and all the other section entries should be
collapsed.

Its possible to set a custom separator between the title and the results. This
can be done with `{separator, Separator}`. By default its set to be `\n`, some
results display a `type_name() :: `followed by all types that define
`type_name()`.

The `{ending, Text}` ElementOption just appends Text to the `Element`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
