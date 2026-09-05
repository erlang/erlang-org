# `merl_transform`
[🔗](https://github.com/erlang/otp/blob/master/lib/syntax_tools/src/merl_transform.erl#L33)

Parse transform for merl.

Enables the use of automatic metavariables and using quasi-quotes in
matches and case switches. Also optimizes calls to functions in `merl`
by partially evaluating them, turning strings to templates, and so on,
at compile-time.

Using `-include_lib("syntax_tools/include/merl.hrl").` enables this transform,
unless the macro `MERL_NO_TRANSFORM` is defined first.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
