# `erl_syntax`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/syntax_tools/src/erl_syntax.erl#L33)

Abstract Erlang syntax trees.

This module defines an abstract data type for representing Erlang source code as
syntax trees, in a way that is backwards compatible with the data structures
created by the Erlang standard library parser module `m:erl_parse` (often referred
to as "parse trees", which is a bit of a misnomer). This means that all
`erl_parse` trees are valid abstract syntax trees, but the reverse is not true:
abstract syntax trees can in general not be used as input to functions expecting
an `erl_parse` tree. However, as long as an abstract syntax tree represents a
correct Erlang program, the function `revert/1` should be able to transform it
to the corresponding `erl_parse` representation.

A recommended starting point for the first-time user is the documentation of the
[`syntaxTree()`](`t:syntaxTree/0`) data type, and the function `type/1`.

> #### Note {: .info }
>
> This module deals with the composition and decomposition of _syntactic_ entities
> (as opposed to semantic ones); its purpose is to hide all direct references to
> the data structures used to represent these entities. With few exceptions, the
> functions in this module perform no semantic interpretation of their inputs, and
> in general, the user is assumed to pass type-correct arguments — if this is not
> done, the effects are not defined.

With the exception of the [`erl_parse()`](`t:erl_parse/0`) data structures, the
internal representations of abstract syntax trees are subject to change without
notice, and should not be documented outside this module. Furthermore, we do not
give any guarantees on how an abstract syntax tree may or may not be
represented, _with the following exceptions_: no syntax tree is represented by a
single atom, such as `none`, by a list constructor `[X | Y]`, or by the empty
list `[]`. This can be relied on when writing functions that operate on syntax
trees.

# `annotation_or_location`

```erlang
-type annotation_or_location() :: erl_anno:anno() | erl_anno:location().
```

# `encoding`
*not exported* 

```erlang
-type encoding() :: utf8 | unicode | latin1.
```

# `erl_parse`
*not exported* 

```erlang
-type erl_parse() ::
          erl_parse:abstract_clause() |
          erl_parse:abstract_expr() |
          erl_parse:abstract_form() |
          erl_parse:abstract_type() |
          erl_parse:form_info() |
          erl_parse:af_binelement(term()) |
          erl_parse:af_generator() |
          [erl_parse:af_generator()] |
          erl_parse:af_remote_function().
```

# `forms`

```erlang
-type forms() :: syntaxTree() | [syntaxTree()].
```

# `guard`
*not exported* 

```erlang
-type guard() :: none | syntaxTree() | [syntaxTree()] | [[syntaxTree()]].
```

# `padding`

```erlang
-type padding() :: none | integer().
```

# `syntaxTree`

```erlang
-type syntaxTree() :: tree() | wrapper() | erl_parse().
```

# `syntaxTreeAttributes`

```erlang
-type syntaxTreeAttributes() ::
          #attr{pos :: term(),
                ann :: [term()],
                com :: none | #com{pre :: [syntaxTree()], post :: [syntaxTree()]}}.
```

# `tree`
*not exported* 

```erlang
-type tree() ::
          #tree{type :: atom(),
                attr ::
                    #attr{pos :: term(),
                          ann :: [term()],
                          com :: none | #com{pre :: [syntaxTree()], post :: [syntaxTree()]}},
                data :: term()}.
```

# `wrapper`
*not exported* 

```erlang
-type wrapper() ::
          #wrapper{type :: atom(),
                   attr ::
                       #attr{pos :: term(),
                             ann :: [term()],
                             com :: none | #com{pre :: [syntaxTree()], post :: [syntaxTree()]}},
                   tree :: erl_parse()}.
```

# ``

```erlang
-spec nil() -> syntaxTree().
```

Creates an abstract empty list.

The result represents "`[]`". The empty list is traditionally called
"nil".

_See also: _`is_list_skeleton/1`, `list/2`.

# `abstract`

```erlang
-spec abstract(term()) -> syntaxTree().
```

Returns the syntax tree corresponding to an Erlang term.

`Term` must be a literal term, meaning one that can be represented as a
source code literal. Thus, it must not contain a process identifier,
port, reference, or function value as a subterm. The function
recognises printable strings, in order to get a compact and readable
representation. Evaluation fails with reason `badarg` if `Term` is not
a literal term.

_See also: _`concrete/1`, `is_literal/1`.

# `add_ann`

```erlang
-spec add_ann(term(), syntaxTree()) -> syntaxTree().
```

Appends the term `Annotation` to the list of user annotations of `Node`.

Note: this is equivalent to
[`set_ann(Node, [Annotation | get_ann(Node)])`](`set_ann/2`), but potentially
more efficient.

_See also: _`get_ann/1`, `set_ann/2`.

# `add_postcomments`

```erlang
-spec add_postcomments([syntaxTree()], syntaxTree()) -> syntaxTree().
```

Appends `Comments` to the post-comments of `Node`.

Note: This is equivalent to
[`set_postcomments(Node, get_postcomments(Node) ++ Comments)`](`set_postcomments/2`),
but potentially more efficient.

_See also: _`add_precomments/2`, `comment/2`, `get_postcomments/1`,
`join_comments/2`, `set_postcomments/2`.

# `add_precomments`

```erlang
-spec add_precomments([syntaxTree()], syntaxTree()) -> syntaxTree().
```

Appends `Comments` to the pre-comments of `Node`.

Note: This is equivalent to
[`set_precomments(Node, get_precomments(Node) ++ Comments)`](`set_precomments/2`),
but potentially more efficient.

_See also: _`add_postcomments/2`, `comment/2`, `get_precomments/1`,
`join_comments/2`, `set_precomments/2`.

# `annotated_type`

```erlang
-spec annotated_type(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract annotated type expression.

The result represents "`Name :: Type`".

_See also: _`annotated_type_body/1`, `annotated_type_name/1`.

# `annotated_type_body`

```erlang
-spec annotated_type_body(syntaxTree()) -> syntaxTree().
```

Returns the type subtrees of an `annotated_type` node.

_See also: _`annotated_type/2`.

# `annotated_type_name`

```erlang
-spec annotated_type_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of an `annotated_type` node.

_See also: _`annotated_type/2`.

# `application`

```erlang
-spec application(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract function application expression.

If `Arguments` is `[A1, ..., An]`, the result represents
"`Operator(A1, ..., An)`".

_See also: _`application/3`, `application_arguments/1`,
`application_operator/1`.

# `application`

```erlang
-spec application(none | syntaxTree(), syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract function application expression.

If `Module` is `none`, this is call is equivalent to
[`application(Function, Arguments)`](`application/2`), otherwise it is
equivalent to [`application(module_qualifier(Module, Function),
Arguments)`](`application/2`).

(This is a utility function.)

_See also: _`application/2`, `module_qualifier/2`.

# `application_arguments`

```erlang
-spec application_arguments(syntaxTree()) -> [syntaxTree()].
```

Returns the list of argument subtrees of an `application` node.

_See also: _`application/2`.

# `application_operator`

```erlang
-spec application_operator(syntaxTree()) -> syntaxTree().
```

Returns the operator subtree of an `application` node.

If `Node` represents "`M:F(...)`", then the result is the subtree
representing "`M:F`".

_See also: _`application/2`, `module_qualifier/2`.

# `arity_qualifier`

```erlang
-spec arity_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract arity qualifier.

The result represents "`Body/Arity`".

_See also: _`arity_qualifier_argument/1`, `arity_qualifier_body/1`.

# `arity_qualifier_argument`

```erlang
-spec arity_qualifier_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument (the arity) subtree of an `arity_qualifier` node.

_See also: _`arity_qualifier/2`.

# `arity_qualifier_body`

```erlang
-spec arity_qualifier_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of an `arity_qualifier` node.

_See also: _`arity_qualifier/2`.

# `atom`

```erlang
-spec atom(atom() | string()) -> syntaxTree().
```

Creates an abstract atom literal.

The print name of the atom is the character sequence represented by
`Name`.

_See also: _`atom_literal/1`, `atom_literal/2`, `atom_name/1`, `atom_value/1`,
`is_atom/2`.

# `atom_literal`

```erlang
-spec atom_literal(syntaxTree()) -> string().
```

Returns the literal string represented by an `atom` node.

This includes surrounding single-quote characters if
necessary. Characters beyond 255 will be escaped.

Note that, for example, the result of [`atom("x\ny")`](`atom/1`)
represents any and all of `'x\ny'`, `'x\12y'`, `'x\012y'`, and
`'x\^Jy'`; see `string/1`.

_See also: _`atom/1`, `string/1`.

# `atom_literal`

```erlang
-spec atom_literal(syntaxTree(), utf8 | unicode | latin1) -> string().
```

Returns the literal string represented by an `atom` node.

This includes surrounding single-quote characters if
necessary. Depending on the encoding a character beyond 255 will be
escaped (`latin1`) or copied as is (`utf8`).

_See also: _`atom/1`, `atom_literal/1`, `string/1`.

# `atom_name`

```erlang
-spec atom_name(syntaxTree()) -> string().
```

Returns the printname of an `atom` node.

_See also: _`atom/1`.

# `atom_value`

```erlang
-spec atom_value(syntaxTree()) -> atom().
```

Returns the value represented by an `atom` node.

_See also: _`atom/1`.

# `attribute`

```erlang
-spec attribute(syntaxTree()) -> syntaxTree().
```

# `attribute`

```erlang
-spec attribute(syntaxTree(), none | [syntaxTree()]) -> syntaxTree().
```

Creates an abstract program attribute.

If `Arguments` is `[A1, ..., An]`, the result represents
"`-Name(A1, ..., An).`". Otherwise, if `Arguments` is `none`,
the result represents "`-Name.`". The latter form makes it possible
to represent preprocessor directives such as "`-endif.`". Attributes
are source code forms.

> #### Note {: .info }
>
> The preprocessor macro definition directive "`-define(Name, Body).`"
> has relatively few requirements on the syntactical form of `Body`
> (viewed as a sequence of tokens). The `text` node type can be used for
> a `Body` that is not a normal Erlang construct.

_See also: _`attribute/1`, `attribute_arguments/1`, `attribute_name/1`,
`is_form/1`, `text/1`.

# `attribute_arguments`

```erlang
-spec attribute_arguments(syntaxTree()) -> none | [syntaxTree()].
```

Returns the list of argument subtrees of an `attribute` node, if any.

If `Node` represents "`-Name.`", the result is `none`. Otherwise, if
`Node` represents "`-Name(E1, ..., En).`", `[E1, ..., E1]` is
returned.

_See also: _`attribute/1`.

# `attribute_name`

```erlang
-spec attribute_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of an `attribute` node.

_See also: _`attribute/1`.

# `binary`

```erlang
-spec binary([syntaxTree()]) -> syntaxTree().
```

Creates an abstract binary-object template.

If `Fields` is `[F1, ..., Fn]`, the result represents "`<<F1, ...,
Fn>>`".

_See also: _`binary_field/2`, `binary_fields/1`.

# `binary_comp`

```erlang
-spec binary_comp(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract binary comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`<<Template ||
E1, ..., En>>`".

_See also: _`binary_comp_body/1`, `binary_comp_template/1`, `generator/2`.

# `binary_comp_body`

```erlang
-spec binary_comp_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `binary_comp` node.

_See also: _`binary_comp/2`.

# `binary_comp_template`

```erlang
-spec binary_comp_template(syntaxTree()) -> syntaxTree().
```

Returns the template subtree of a `binary_comp` node.

_See also: _`binary_comp/2`.

# `binary_field`

```erlang
-spec binary_field(syntaxTree()) -> syntaxTree().
```

# `binary_field`

```erlang
-spec binary_field(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract binary template field.

If `Types` is the empty list, the result simply represents
"`Body`", otherwise, if `Types` is `[T1, ..., Tn]`, the result
represents "`Body/T1-...-Tn`".

_See also: _`binary/1`, `binary_field/1`, `binary_field/3`,
`binary_field_body/1`, `binary_field_size/1`, `binary_field_types/1`.

# `binary_field`

```erlang
-spec binary_field(syntaxTree(), none | syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract binary template field.

If `Size` is `none`, this is equivalent to "[`binary_field(Body,
Types)`](`binary_field/2`)", otherwise it is equivalent to
"[`binary_field(size_qualifier(Body, Size),
Types)`](`binary_field/2`)".

(This is a utility function.)

_See also: _`binary/1`, `binary_field/2`, `size_qualifier/2`.

# `binary_field_body`

```erlang
-spec binary_field_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `binary_field`.

_See also: _`binary_field/2`.

# `binary_field_size`

```erlang
-spec binary_field_size(syntaxTree()) -> none | syntaxTree().
```

Returns the size specifier subtree of a `binary_field` node, if any.

If `Node` represents "`Body:Size`" or "`Body:Size/T1, ...,
Tn`", the result is `Size`, otherwise `none` is returned.

(This is a utility function.)

_See also: _`binary_field/2`, `binary_field/3`.

# `binary_field_types`

```erlang
-spec binary_field_types(syntaxTree()) -> [syntaxTree()].
```

Returns the list of type-specifier subtrees of a `binary_field` node.

If `Node` represents "`.../T1, ..., Tn`", the result is `[T1,
..., Tn]`, otherwise the result is the empty list.

_See also: _`binary_field/2`.

# `binary_fields`

```erlang
-spec binary_fields(syntaxTree()) -> [syntaxTree()].
```

Returns the list of field subtrees of a `binary` node.

_See also: _`binary/1`, `binary_field/2`.

# `binary_generator`

```erlang
-spec binary_generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract binary_generator.

The result represents "`Pattern <= Body`".

_See also: _`binary_comp/2`, `binary_generator_body/1`,
`binary_generator_pattern/1`, `list_comp/2`, `map_comp/2`.

# `binary_generator_body`

```erlang
-spec binary_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `generator` node.

_See also: _`binary_generator/2`.

# `binary_generator_pattern`

```erlang
-spec binary_generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `generator` node.

_See also: _`binary_generator/2`.

# `bitstring_type`

```erlang
-spec bitstring_type(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract bitstring type.

The result represents "`<<_:M, _:_N>>`".

_See also: _`bitstring_type_m/1`, `bitstring_type_n/1`.

# `bitstring_type_m`

```erlang
-spec bitstring_type_m(syntaxTree()) -> syntaxTree().
```

Returns the number of start bits, `M`, of a `bitstring_type` node.

_See also: _`bitstring_type/2`.

# `bitstring_type_n`

```erlang
-spec bitstring_type_n(syntaxTree()) -> syntaxTree().
```

Returns the segment size, `N`, of a `bitstring_type` node.

_See also: _`bitstring_type/2`.

# `block_expr`

```erlang
-spec block_expr([syntaxTree()]) -> syntaxTree().
```

Creates an abstract block expression.

If `Body` is `[B1, ..., Bn]`, the result represents "`begin B1, ...,
Bn end`".

_See also: _`block_expr_body/1`.

# `block_expr_body`

```erlang
-spec block_expr_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `block_expr` node.

_See also: _`block_expr/1`.

# `case_expr`

```erlang
-spec case_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract case-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`case
Argument of C1; ...; Cn end`". More exactly, if each `Ci`
represents "`(Pi) Gi -> Bi`", then the result represents "`case
Argument of P1G1 -> B1; ...; PnGn -> Bn end`".

_See also: _`case_expr_argument/1`, `case_expr_clauses/1`, `clause/3`,
`if_expr/1`.

# `case_expr_argument`

```erlang
-spec case_expr_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument subtree of a `case_expr` node.

_See also: _`case_expr/2`.

# `case_expr_clauses`

```erlang
-spec case_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of a `case_expr` node.

_See also: _`case_expr/2`.

# `catch_expr`

```erlang
-spec catch_expr(syntaxTree()) -> syntaxTree().
```

Creates an abstract catch-expression.

The result represents "`catch Expr`".

_See also: _`catch_expr_body/1`.

# `catch_expr_body`

```erlang
-spec catch_expr_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `catch_expr` node.

_See also: _`catch_expr/1`.

# `char`

```erlang
-spec char(char()) -> syntaxTree().
```

Creates an abstract character literal.

The result represents "`$Name`", where `Name` corresponds to `Value`.

> #### Note {: .info }

The literal corresponding to a particular character value is not
uniquely defined. For example, the character "`a`" can be written both
as "`$a`" and "`$\141`", and a Tab character can be written as
"`$\11`", "`$\011`", or "`$\t`".

_See also: _`char_literal/1`, `char_literal/2`, `char_value/1`, `is_char/2`.

# `char_literal`

```erlang
-spec char_literal(syntaxTree()) -> nonempty_string().
```

Returns the literal string represented by a `char` node.

This includes the leading "`$`" character. Characters beyond 255 will
be escaped.

_See also: _`char/1`.

# `char_literal`

```erlang
-spec char_literal(syntaxTree(), encoding()) -> nonempty_string().
```

Returns the literal string represented by a `char` node.

This includes the leading "`$`" character. Depending on the encoding a
character beyond 255 will be escaped (`latin1`) or copied as is
(`utf8`).

_See also: _`char/1`.

# `char_value`

```erlang
-spec char_value(syntaxTree()) -> char().
```

Returns the value represented by a `char` node.

_See also: _`char/1`.

# `class_qualifier`

```erlang
-spec class_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract class qualifier.

The result represents "`Class:Body`".

_See also: _`class_qualifier_argument/1`, `class_qualifier_body/1`,
`class_qualifier_stacktrace/1`, `try_expr/4`.

# `class_qualifier`

```erlang
-spec class_qualifier(syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract class qualifier.

The result represents "`Class:Body:Stacktrace`".

_See also: _`class_qualifier_argument/1`, `class_qualifier_body/1`,
`try_expr/4`.

# `class_qualifier_argument`

```erlang
-spec class_qualifier_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument (the class) subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.

# `class_qualifier_body`

```erlang
-spec class_qualifier_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.

# `class_qualifier_stacktrace`

```erlang
-spec class_qualifier_stacktrace(syntaxTree()) -> syntaxTree().
```

Returns the stacktrace subtree of a `class_qualifier` node.

_See also: _`class_qualifier/2`.

# `clause`

```erlang
-spec clause(guard(), [syntaxTree()]) -> syntaxTree().
```

# `clause`

```erlang
-spec clause([syntaxTree()], guard(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract clause.

If `Patterns` is `[P1, ..., Pn]` and `Body` is `[B1, ..., Bm]`, then
if `Guard` is `none`, the result represents "`(P1, ..., Pn) ->
B1, ..., Bm`", otherwise, unless `Guard` is a list, the result
represents "`(P1, ..., Pn) when Guard -> B1, ..., Bm`".

For simplicity, the `Guard` argument may also be any of the following:

- An empty list `[]`. This is equivalent to passing `none`.
- A nonempty list `[E1, ..., Ej]` of syntax trees. This is equivalent to passing
  `conjunction([E1, ..., Ej])`.
- A nonempty list of lists of syntax trees
  `[[E1_1, ..., E1_k1], ..., [Ej_1, ..., Ej_kj]]`, which is equivalent to
  passing
  `disjunction([conjunction([E1_1, ..., E1_k1]), ..., conjunction([Ej_1, ..., Ej_kj])])`.

_See also: _`clause/2`, `clause_body/1`, `clause_guard/1`, `clause_patterns/1`.

# `clause_body`

```erlang
-spec clause_body(syntaxTree()) -> [syntaxTree()].
```

Return the list of body subtrees of a `clause` node.

_See also: _`clause/3`.

# `clause_guard`

```erlang
-spec clause_guard(syntaxTree()) -> none | syntaxTree().
```

Returns the guard subtree of a `clause` node, if any.

If `Node` represents "`(P1, ..., Pn) when Guard -> B1, ...,
Bm`", `Guard` is returned.  Otherwise, the result is `none`.

_See also: _`clause/3`.

# `clause_patterns`

```erlang
-spec clause_patterns(syntaxTree()) -> [syntaxTree()].
```

Returns the list of pattern subtrees of a `clause` node.

_See also: _`clause/3`.

# `comment`

```erlang
-spec comment([string()]) -> syntaxTree().
```

# `comment`

```erlang
-spec comment(padding(), [string()]) -> syntaxTree().
```

Creates an abstract comment with the given padding and text.

If `Strings` is a (possibly empty) list `["Txt1", ..., "TxtN"]`,
the result represents the source code text

```text
     Txt1
     ...
     TxtN
```

`Padding` states the number of empty character positions to the left of the
comment separating it horizontally from source code on the same line (if any).
If `Padding` is `none`, a default positive number is used. If `Padding` is an
integer less than 1, there should be no separating space. Comments are in
themselves regarded as source program forms.

_See also: _`comment/1`, `is_form/1`.

# `comment_padding`

```erlang
-spec comment_padding(syntaxTree()) -> padding().
```

Returns the amount of padding before the comment, or `none`.

`none` means that a default padding may be used.

_See also: _`comment/2`.

# `comment_text`

```erlang
-spec comment_text(syntaxTree()) -> [string()].
```

Returns the lines of text of the abstract comment.

_See also: _`comment/2`.

# `compact_list`

```erlang
-spec compact_list(syntaxTree()) -> syntaxTree().
```

Yields the most compact form for an abstract list skeleton.

The result either represents "`[E1, ..., En | Tail]`", where
`Tail` is not a list skeleton, or otherwise simply "`[E1, ...,
En]`". Annotations on subtrees of `Node` that represent list
skeletons may be lost, but comments will be propagated to the
result. Returns `Node` itself if `Node` does not represent a list
skeleton.

_See also: _`list/2`, `normalize_list/1`.

# `concrete`

```erlang
-spec concrete(syntaxTree()) -> term().
```

Returns the Erlang term represented by a syntax tree.

Evaluation fails with reason `badarg` if `Node` does not represent a
literal term.

> #### Note {: .info }
>
> The set of syntax trees which have a concrete representation is larger
> than the set of trees which can be built using the function
> `abstract/1`.  An abstract character will be concretised as an
> integer, while `abstract/1` does not at present yield an abstract
> character for any input. (Use the `char/1` function to explicitly
> create an abstract character.)

> #### Note {: .info }
>
> `arity_qualifier` nodes are recognized. This is to follow the Erlang
> Parser when it comes to wild attributes: both `{F, A}` and `F/A` are
> recognized, which makes it possible to turn wild attributes into
> recognized attributes without at the same time making it impossible to
> compile files using the new syntax with the old version of the Erlang
> Compiler.

_See also: _`abstract/1`, `char/1`, `is_literal/1`.

# `conjunction`

```erlang
-spec conjunction([syntaxTree()]) -> syntaxTree().
```

Creates an abstract conjunction.

If `List` is `[E1, ..., En]`, the result represents "`E1, ..., En`".

_See also: _`conjunction_body/1`, `disjunction/1`.

# `conjunction_body`

```erlang
-spec conjunction_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `conjunction` node.

_See also: _`conjunction/1`.

# `cons`

```erlang
-spec cons(syntaxTree(), syntaxTree()) -> syntaxTree().
```

"Optimizing" list skeleton cons operation.

Creates an abstract list skeleton whose first element is `Head` and
whose tail corresponds to `Tail`. This is similar to [`list([Head],
Tail)`](`list/2`), except that `Tail` must not be `none`,
and the result does not necessarily represent exactly "`[Head | Tail]`", but
may depend on the `Tail` subtree.

For example, if `Tail` represents `[X, Y]`, the result may represent
"`[Head, X, Y]`", rather than "`[Head | [X, Y]]`". Annotations on
`Tail` itself may be lost if `Tail` represents a list skeleton, but
comments on `Tail` are propagated to the result.

_See also: _`list/2`, `list_head/1`, `list_tail/1`.

# `constrained_function_type`

```erlang
-spec constrained_function_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract constrained function type.

If `FunctionConstraint` is `[C1, ..., Cn]`, the result represents
"`FunctionType when C1, ...Cn`".

_See also: _`constrained_function_type_argument/1`,
`constrained_function_type_body/1`.

# `constrained_function_type_argument`

```erlang
-spec constrained_function_type_argument(syntaxTree()) -> syntaxTree().
```

Returns the function constraint subtree of a `constrained_function_type` node.

_See also: _`constrained_function_type/2`.

# `constrained_function_type_body`

```erlang
-spec constrained_function_type_body(syntaxTree()) -> syntaxTree().
```

Returns the function type subtree of a `constrained_function_type` node.

_See also: _`constrained_function_type/2`.

# `constraint`

```erlang
-spec constraint(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract (subtype) constraint.

The result represents "`Name :: Type`".

_See also: _`constraint_argument/1`, `constraint_body/1`.

# `constraint_argument`

```erlang
-spec constraint_argument(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `constraint` node.

_See also: _`constraint/2`.

# `constraint_body`

```erlang
-spec constraint_body(syntaxTree()) -> [syntaxTree()].
```

Returns the type subtree of a `constraint` node.

_See also: _`constraint/2`.

# `copy_ann`

```erlang
-spec copy_ann(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Copies the list of user annotations from `Source` to `Target`.

Note: this is equivalent to [`set_ann(Target, get_ann(Source))`](`set_ann/2`),
but potentially more efficient.

_See also: _`get_ann/1`, `set_ann/2`.

# `copy_attrs`

```erlang
-spec copy_attrs(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Copies the attributes from `Source` to `Target`.

Note: this is equivalent to
[`set_attrs(Target, get_attrs(Source))`](`set_attrs/2`), but potentially more
efficient.

_See also: _`get_attrs/1`, `set_attrs/2`.

# `copy_comments`

```erlang
-spec copy_comments(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Copies the pre- and postcomments from `Source` to `Target`.

Note: This is equivalent to
[`set_postcomments(set_precomments(Target, get_precomments(Source)), get_postcomments(Source))`](`set_postcomments/2`),
but potentially more efficient.

_See also: _`comment/2`, `get_postcomments/1`, `get_precomments/1`,
`set_postcomments/2`, `set_precomments/2`.

# `copy_pos`

```erlang
-spec copy_pos(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Copies the annotation from `Source` to `Target`.

This is equivalent to [`set_pos(Target, get_pos(Source))`](`set_pos/2`), but
potentially more efficient.

_See also: _`get_pos/1`, `set_pos/2`.

# `data`

```erlang
-spec data(syntaxTree()) -> term().
```

**For special purposes only**. Returns the associated data of a syntax tree node.

Evaluation fails with reason `badarg` if [`is_tree(Node)`](`is_tree/1`) does not
yield `true`.

_See also: _`tree/2`.

# `disjunction`

```erlang
-spec disjunction([syntaxTree()]) -> syntaxTree().
```

Creates an abstract disjunction.

If `List` is `[E1, ..., En]`, the result represents "`E1; ...; En`".

_See also: _`conjunction/1`, `disjunction_body/1`.

# `disjunction_body`

```erlang
-spec disjunction_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `disjunction` node.

_See also: _`disjunction/1`.

# `else_expr`

```erlang
-spec else_expr([syntaxTree()]) -> syntaxTree().
```

Creates an abstract else-expression.

If `Clauses` is `[C1, ..., Cn]`, the result
represents "`else C1; ...; Cn end`". More exactly, if each `Ci` represents
"`(Pi) Gi -> Bi`", then the result represents
"`else (P1) G1 -> B1; ...; (Pn) Gn -> Bn end`".

_See also: _`clause/3`, `else_expr_clauses/1`, `maybe_expr/2`.

# `else_expr_clauses`

```erlang
-spec else_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of an `else_expr` node.

_See also: _`else_expr/1`.

# `eof_marker`

```erlang
-spec eof_marker() -> syntaxTree().
```

Creates an abstract end-of-file marker.

This represents the end of input when reading a sequence of source
code forms. An end-of-file marker is itself regarded as a source code
form (namely, the last in any sequence in which it occurs). It has no
defined lexical form.

> #### Note {: .info }
>
> This is retained only for backwards compatibility with existing parsers
> and tools.

_See also: _`error_marker/1`, `is_form/1`, `warning_marker/1`.

# `error_marker`

```erlang
-spec error_marker(term()) -> syntaxTree().
```

Creates an abstract error marker.

The result represents an occurrence of an error in the source code,
with an associated Erlang I/O ErrorInfo structure given by `Error`
(see module [`//stdlib/io`](`m:io`) for details). Error markers are
regarded as source code forms, but have no defined lexical form.

> #### Note {: .info }
>
> This is supported only for backwards compatibility with existing parsers
> and tools.

_See also: _`eof_marker/0`, `error_marker_info/1`, `is_form/1`,
`warning_marker/1`.

# `error_marker_info`

```erlang
-spec error_marker_info(syntaxTree()) -> term().
```

Returns the ErrorInfo structure of an `error_marker` node.

_See also: _`error_marker/1`.

# `flatten_form_list`

```erlang
-spec flatten_form_list(syntaxTree()) -> syntaxTree().
```

Flattens sublists of a `form_list` node.

Returns `Node` with all subtrees of type `form_list` recursively
expanded, yielding a single "flat" abstract form sequence.

_See also: _`form_list/1`.

# `float`

```erlang
-spec float(float()) -> syntaxTree().
```

Creates an abstract floating-point literal.

The lexical representation is the decimal floating-point numeral of
`Value`.

_See also: _`float_literal/1`, `float_value/1`.

# `float_literal`

```erlang
-spec float_literal(syntaxTree()) -> string().
```

Returns the numeral string represented by a `float` node.

_See also: _`float/1`.

# `float_value`

```erlang
-spec float_value(syntaxTree()) -> float().
```

Returns the value represented by a `float` node.

Note that floating-point values should usually not be compared for
equality.

_See also: _`float/1`.

# `form_list`

```erlang
-spec form_list([syntaxTree()]) -> syntaxTree().
```

Creates an abstract sequence of "source code forms".

If `Forms` is `[F1, ..., Fn]`, where each `Fi` is a form (see
`is_form/1`), the result represents:

```text
F1
...
Fn
```

where the `Fi` are separated by one or more line breaks. A node of type
`form_list` is itself regarded as a source code form; see `flatten_form_list/1`.

> #### Note {: .info }
>
> This is simply a way of grouping source code forms into a single syntax
tree, usually to form an Erlang module definition.

_See also: _`flatten_form_list/1`, `form_list_elements/1`, `is_form/1`.

# `form_list_elements`

```erlang
-spec form_list_elements(syntaxTree()) -> [syntaxTree()].
```

Returns the list of subnodes of a `form_list` node.

_See also: _`form_list/1`.

# `fun_expr`

```erlang
-spec fun_expr([syntaxTree()]) -> syntaxTree().
```

Creates an abstract fun-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`fun C1;
...; Cn end`". More exactly, if each `Ci` represents "`(Pi1, ...,
Pim) Gi -> Bi`", then the result represents "`fun (P11, ...,
P1m) G1 -> B1; ...; (Pn1, ..., Pnm) Gn -> Bn end`".

_See also: _`fun_expr_arity/1`, `fun_expr_clauses/1`.

# `fun_expr_arity`

```erlang
-spec fun_expr_arity(syntaxTree()) -> arity().
```

Returns the arity of a `fun_expr` node.

The result is the number of parameter patterns in the first clause of
the fun-expression; subsequent clauses are ignored.

An exception is thrown if [`fun_expr_clauses(Node)`](`fun_expr_clauses/1`)
returns an empty list, or if the first element of that list is not a syntax tree
`C` of type `clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a
nonempty list.

_See also: _`clause/3`, `clause_patterns/1`, `fun_expr/1`, `fun_expr_clauses/1`.

# `fun_expr_clauses`

```erlang
-spec fun_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of a `fun_expr` node.

_See also: _`fun_expr/1`.

# `fun_type`

```erlang
-spec fun_type() -> syntaxTree().
```

Creates an abstract fun of any type.

The result represents "`fun()`".

# `function`

```erlang
-spec function(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract function definition.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`Name C1;
...; Name Cn.`". More exactly, if each `Ci` represents "`(Pi1, ...,
Pim) Gi -> Bi`", then the result represents "`Name(P11, ...,
P1m) G1 -> B1; ...; Name(Pn1, ..., Pnm) Gn -> Bn.`".
Function definitions are source code forms.

_See also: _`function_arity/1`, `function_clauses/1`, `function_name/1`,
`is_form/1`.

# `function_arity`

```erlang
-spec function_arity(syntaxTree()) -> arity().
```

Returns the arity of a `function` node.

The result is the number of parameter patterns in the first clause of
the function; subsequent clauses are ignored.

An exception is thrown if [`function_clauses(Node)`](`function_clauses/1`)
returns an empty list, or if the first element of that list is not a syntax tree
`C` of type `clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a
nonempty list.

_See also: _`clause/3`, `clause_patterns/1`, `function/2`, `function_clauses/1`.

# `function_clauses`

```erlang
-spec function_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of a `function` node.

_See also: _`function/2`.

# `function_name`

```erlang
-spec function_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `function` node.

_See also: _`function/2`.

# `function_type`

```erlang
-spec function_type(syntaxTree()) -> syntaxTree().
```

# `function_type`

```erlang
-spec function_type(any_arity | [syntaxTree()], syntaxTree()) -> syntaxTree().
```

Creates an abstract function type.

If `Arguments` is `[T1, ..., Tn]` *and* it occurs within a function
specification, the result represents "`(T1, ...Tn) -> Return`";
otherwise it represents "`fun((T1, ...Tn) -> Return)`". If
`Arguments` is `any_arity`, it represents "`fun((...) -> Return)`".

Note that the `m:erl_parse` representation is identical for
"`FunctionType`" and "`fun(FunctionType)`".

_See also: _`function_type_arguments/1`, `function_type_return/1`.

# `function_type_arguments`

```erlang
-spec function_type_arguments(syntaxTree()) -> any_arity | [syntaxTree()].
```

Returns the argument types subtrees of a `function_type` node.

If `Node` represents "`fun((...) -> Return)`", `any_arity` is
returned; otherwise, if `Node` represents "`(T1, ...Tn) ->
Return`" or "`fun((T1, ...Tn) -> Return)`", `[T1, ..., Tn]` is
returned.

_See also: _`function_type/1`, `function_type/2`.

# `function_type_return`

```erlang
-spec function_type_return(syntaxTree()) -> syntaxTree().
```

Returns the return type subtrees of a `function_type` node.

_See also: _`function_type/1`, `function_type/2`.

# `generator`

```erlang
-spec generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract list generator.

The result represents "`Pattern <- Body`".

_See also: _`binary_comp/2`, `generator_body/1`, `generator_pattern/1`,
`map_comp/2`, `list_comp/2`.

# `generator_body`

```erlang
-spec generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `generator` node.

_See also: _`generator/2`.

# `generator_pattern`

```erlang
-spec generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `generator` node.

_See also: _`generator/2`.

# `get_ann`

```erlang
-spec get_ann(syntaxTree()) -> [term()].
```

Returns the list of user annotations associated with a syntax tree.

For a newly created node, this is the empty list. The annotations may
be any terms.

_See also: _`get_attrs/1`, `set_ann/2`.

# `get_attrs`

```erlang
-spec get_attrs(syntaxTree()) -> syntaxTreeAttributes().
```

Returns a representation of the attributes associated with a syntax tree node.

The attributes are all the extra information that can be attached to a node.
Currently, this includes position information, source code comments, and user
annotations. The result of this function cannot be inspected directly; only
attached to another node (see `set_attrs/2`).

For accessing individual attributes, see `get_pos/1`, `get_ann/1`,
`get_precomments/1` and `get_postcomments/1`.

_See also: _`get_ann/1`, `get_pos/1`, `get_postcomments/1`, `get_precomments/1`,
`set_attrs/2`.

# `get_pos`

```erlang
-spec get_pos(syntaxTree()) -> annotation_or_location().
```

Returns the annotation (see [`//stdlib/erl_anno`](`m:erl_anno`)) associated with
`Node`.

By default, all new tree nodes have their associated position
information set to the integer zero. Use
[`//stdlib/erl_anno:location/1`](`erl_anno:location/1`) or
[`//stdlib/erl_anno:line/1`](`erl_anno:line/1`) to get the position information.

_See also: _`get_attrs/1`, `set_pos/2`.

# `get_postcomments`

```erlang
-spec get_postcomments(syntaxTree()) -> [syntaxTree()].
```

Returns the associated post-comments of a node.

This is a possibly empty list of abstract comments, in top-down
textual order. When the code is formatted, post-comments are typically
displayed to the right of and/or below the node. For example:

```erlang
{foo, X, Y}     % Post-comment of tuple
```

If possible, the comment should be moved past any following separator characters
on the same line, rather than placing the separators on the following line.
For example:

```erlang
foo([X | Xs], Y) ->
    foo(Xs, bar(X));     % Post-comment of 'bar(X)' node
 ...
```

(where the comment is moved past the rightmost "`)`" and the "`;`").

_See also: _`comment/2`, `get_attrs/1`, `get_precomments/1`,
`set_postcomments/2`.

# `get_precomments`

```erlang
-spec get_precomments(syntaxTree()) -> [syntaxTree()].
```

Returns the associated pre-comments of a node.

This is a possibly empty list of abstract comments, in top-down
textual order. When the code is formatted, pre-comments are typically
displayed directly above the node. For example:

```erlang
% Pre-comment of function
foo(X) -> {bar, X}.
```

If possible, the comment should be moved before any preceding separator
characters on the same line. For example:

```erlang
foo([X | Xs]) ->
    % Pre-comment of 'bar(X)' node
    [bar(X) | foo(Xs)];
...
```

(where the comment is moved before the "`[`").

_See also: _`comment/2`, `get_attrs/1`, `get_postcomments/1`,
`set_precomments/2`.

# `has_comments`

```erlang
-spec has_comments(syntaxTree()) -> boolean().
```

Yields `false` if the node has no associated comments, and `true` otherwise.

Note: This is equivalent to
`(get_precomments(Node) == []) and (get_postcomments(Node) == [])`, but
potentially more efficient.

_See also: _`get_postcomments/1`, `get_precomments/1`, `remove_comments/1`.

# `if_expr`

```erlang
-spec if_expr([syntaxTree()]) -> syntaxTree().
```

Creates an abstract if-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`if C1; ...;
Cn end`". More exactly, if each `Ci` represents "`() Gi -> Bi`",
then the result represents "`if G1 -> B1; ...; Gn -> Bn end`".

_See also: _`case_expr/2`, `clause/3`, `if_expr_clauses/1`.

# `if_expr_clauses`

```erlang
-spec if_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of an `if_expr` node.

_See also: _`if_expr/1`.

# `implicit_fun`

```erlang
-spec implicit_fun(syntaxTree()) -> syntaxTree().
```

Creates an abstract "implicit fun" expression.

The result represents "`fun Name`". `Name` should represent either
`F/A` or `M:F/A`

_See also: _`arity_qualifier/2`, `implicit_fun/2`, `implicit_fun/3`,
`implicit_fun_name/1`, `module_qualifier/2`.

# `implicit_fun`

```erlang
-spec implicit_fun(syntaxTree(), none | syntaxTree()) -> syntaxTree().
```

Creates an abstract "implicit fun" expression.

If `Arity` is `none`, this is equivalent to
[`implicit_fun(Name)`](`implicit_fun/1`), otherwise it is equivalent
to [`implicit_fun(arity_qualifier(Name, Arity))`](`implicit_fun/1`).

(This is a utility function.)

_See also: _`implicit_fun/1`, `implicit_fun/3`.

# `implicit_fun`

```erlang
-spec implicit_fun(none | syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract module-qualified "implicit fun" expression.

If `Module` is `none`, this is equivalent to [`implicit_fun(Name,
Arity)`](`implicit_fun/2`), otherwise it is equivalent to
`implicit_fun(module_qualifier(Module, arity_qualifier(Name, Arity))`.

(This is a utility function.)

_See also: _`implicit_fun/1`, `implicit_fun/2`.

# `implicit_fun_name`

```erlang
-spec implicit_fun_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of an `implicit_fun` node.

If `Node` represents "`fun N/A`" or "`fun M:N/A`", then the
result is the subtree representing "`N/A`" or "`M:N/A`", respectively.

_See also: _`arity_qualifier/2`, `implicit_fun/1`, `module_qualifier/2`.

# `infix_expr`

```erlang
-spec infix_expr(syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract infix operator expression.

The result represents "`Left Operator Right`".

_See also: _`infix_expr_left/1`, `infix_expr_operator/1`, `infix_expr_right/1`,
`prefix_expr/2`.

# `infix_expr_left`

```erlang
-spec infix_expr_left(syntaxTree()) -> syntaxTree().
```

Returns the left argument subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.

# `infix_expr_operator`

```erlang
-spec infix_expr_operator(syntaxTree()) -> syntaxTree().
```

Returns the operator subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.

# `infix_expr_right`

```erlang
-spec infix_expr_right(syntaxTree()) -> syntaxTree().
```

Returns the right argument subtree of an `infix_expr` node.

_See also: _`infix_expr/3`.

# `integer`

```erlang
-spec integer(integer()) -> syntaxTree().
```

Creates an abstract integer literal.

The lexical representation is the canonical decimal numeral of `Value`.

_See also: _`integer_literal/1`, `integer_value/1`, `is_integer/2`.

# `integer_literal`

```erlang
-spec integer_literal(syntaxTree()) -> string().
```

Returns the numeral string represented by an `integer` node.

_See also: _`integer/1`.

# `integer_range_type`

```erlang
-spec integer_range_type(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract range type.

The result represents "`Low .. High`".

_See also: _`integer_range_type_high/1`, `integer_range_type_low/1`.

# `integer_range_type_high`

```erlang
-spec integer_range_type_high(syntaxTree()) -> syntaxTree().
```

Returns the high limit of an `integer_range_type` node.

_See also: _`integer_range_type/2`.

# `integer_range_type_low`

```erlang
-spec integer_range_type_low(syntaxTree()) -> syntaxTree().
```

Returns the low limit of an `integer_range_type` node.

_See also: _`integer_range_type/2`.

# `integer_value`

```erlang
-spec integer_value(syntaxTree()) -> integer().
```

Returns the value represented by an `integer` node.

_See also: _`integer/1`.

# `is_atom`

```erlang
-spec is_atom(syntaxTree(), atom()) -> boolean().
```

Returns `true` if `Node` has type `atom` and represents `Value`, otherwise
`false`.

_See also: _`atom/1`.

# `is_char`

```erlang
-spec is_char(syntaxTree(), char()) -> boolean().
```

Returns `true` if `Node` has type `char` and represents `Value`, otherwise
`false`.

_See also: _`char/1`.

# `is_form`

```erlang
-spec is_form(syntaxTree()) -> boolean().
```

Returns `true` if `Node` is a syntax tree representing a so-called "source code
form", otherwise `false`.

Forms are the Erlang source code units which, placed in sequence,
constitute an Erlang program. Current form types are:

* `attribute`
* `comment`
* `error_marker`
* `eof_marker`
* `form_list`
* `function`
* `warning_marker`
* `text`

_See also: _`attribute/2`, `comment/2`, `eof_marker/0`, `error_marker/1`,
`form_list/1`, `function/2`, `type/1`, `warning_marker/1`.

# `is_integer`

```erlang
-spec is_integer(syntaxTree(), integer()) -> boolean().
```

Returns `true` if `Node` has type `integer` and represents `Value`, otherwise
`false`.

_See also: _`integer/1`.

# `is_leaf`

```erlang
-spec is_leaf(syntaxTree()) -> boolean().
```

Returns `true` if `Node` is a leaf node, otherwise `false`.

The currently recognised leaf node types are:

* `atom`
* `char`
* `comment`
* `eof_marker`
* `error_marker`
* `float`
* `fun_type`
* `integer`
* `nil`
* `operator`
* `string`
* `text`
* `underscore`
* `variable`
* `warning_marker`

A node of type `map_expr` is a leaf node if and only if it has no argument and
no fields. A node of type `map_type` is a leaf node if and only if it has no
fields (`any_size`). A node of type `tuple` is a leaf node if and only if its
arity is zero. A node of type `tuple_type` is a leaf node if and only if it has
no elements (`any_size`).

Note: not all literals are leaf nodes, and vice versa. For example,
tuples with nonzero arity and nonempty lists may be literals, but are
not leaf nodes. Variables, on the other hand, are leaf nodes but not
literals.

_See also: _`is_literal/1`, `type/1`.

# `is_list_skeleton`

```erlang
-spec is_list_skeleton(syntaxTree()) -> boolean().
```

Returns `true` if `Node` has type `list` or `nil`, otherwise `false`.

_See also: _`list/2`, `nil/0`.

# `is_literal`

```erlang
-spec is_literal(syntaxTree()) -> boolean().
```

Returns `true` if `Node` represents a literal term, otherwise `false`.

This function returns `true` if and only if the value of
[`concrete(Node)`](`concrete/1`) is defined.

_See also: _`abstract/1`, `concrete/1`.

# `is_proper_list`

```erlang
-spec is_proper_list(syntaxTree()) -> boolean().
```

Returns `true` if `Node` represents a proper list, and `false` otherwise.

A proper list is a list skeleton either of the form "`[]`" or "`[E1,
..., En]`", or "`[... | Tail]`" where recursively `Tail` also
represents a proper list.

> #### Note {: .info }
>
> Since `Node` is a syntax tree, the actual run-time values
> corresponding to its subtrees can often be partially or completely
> unknown. For example, if `Node` represents "`[... | Ns]`"
> (where `Ns` is a variable), the function will return `false`
> because it is not known whether `Ns` will be bound to a list at
> run-time. Conversely, if `Node` represents, for example, "`[1, 2, 3]`" or
> "`[A | []]`", the function will return `true`.

_See also: _`list/2`.

# `is_string`

```erlang
-spec is_string(syntaxTree(), string()) -> boolean().
```

Returns `true` if `Node` has type `string` and represents `Value`, otherwise
`false`.

_See also: _`string/1`.

# `is_tree`

```erlang
-spec is_tree(syntaxTree()) -> boolean().
```

**For special purposes only**. Returns `true` if `Tree` is an abstract syntax tree
and `false` otherwise.

> #### Note {: .info }
>
> This function yields `false` for all "old-style" `m:erl_parse`-compatible
> "parse trees".

_See also: _`tree/2`.

# `join_comments`

```erlang
-spec join_comments(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Appends the comments of `Source` to the current comments of `Target`.

Note: This is equivalent to
[`add_postcomments(get_postcomments(Source), add_precomments(get_precomments(Source), Target))`](`add_postcomments/2`),
but potentially more efficient.

_See also: _`add_postcomments/2`, `add_precomments/2`, `comment/2`,
`get_postcomments/1`, `get_precomments/1`.

# `list`

```erlang
-spec list([syntaxTree()]) -> syntaxTree().
```

# `list`

```erlang
-spec list([syntaxTree()], none | syntaxTree()) -> syntaxTree().
```

Constructs an abstract list skeleton.

The result has type `list` or `nil`. If `List` is a nonempty list
`[E1, ..., En]`, the result has type `list` and represents either
"`[E1, ..., En]`" if `Tail` is `none`, or otherwise "`[E1, ...,
En | Tail]`". If `List` is the empty list, `Tail` _must_ be `none`,
and in that case the result has type `nil` and represents "`[]`" (see
`nil/0`).

The difference between lists as semantic objects (built up of individual "cons"
and "nil" terms) and the various syntactic forms for denoting lists may be
bewildering at first. This module provides functions both for exact control of
the syntactic representation as well as for the simple composition and
deconstruction in terms of cons and head/tail operations.

> #### Note {: .info }
>
> In [`list(Elements, none)`](`list/2`), the "nil" list terminator is
> implicit and has no associated information (see `get_attrs/1`). However,
> in the seemingly equivalent [`list(Elements, Tail)`](`list/2`) where
> `Tail` has the type `nil`, the list terminator subtree `Tail` may have
> attached attributes such as position, comments, and annotations, which
> will be preserved in the result.

_See also: _`compact_list/1`, `cons/2`, `get_attrs/1`, `is_list_skeleton/1`,
`is_proper_list/1`, `list/1`, `list_elements/1`, `list_head/1`, `list_length/1`,
`list_prefix/1`, `list_suffix/1`, `list_tail/1`, `nil/0`, `normalize_list/1`.

# `list_comp`

```erlang
-spec list_comp(syntaxTree() | [syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

Creates an abstract list comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`[Template ||
E1, ..., En]`".

Supports comprehensions with multiple emitted elements per iteration,
from EEP 78 - in such cases, `Template` is a list of expressions.

_See also: _`generator/2`, `list_comp_body/1`, `list_comp_template/1`.

# `list_comp_body`

```erlang
-spec list_comp_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `list_comp` node.

_See also: _`list_comp/2`.

# `list_comp_template`

```erlang
-spec list_comp_template(syntaxTree()) -> syntaxTree() | [syntaxTree()].
```

Returns the template subtree of a `list_comp` node.

Supports comprehensions with multiple emitted elements per iteration,
from EEP 78 - in such cases, template will be a list of expressions.

_See also: _`list_comp/2`.

# `list_elements`

```erlang
-spec list_elements(syntaxTree()) -> [syntaxTree()].
```

Returns the list of element subtrees of a list skeleton.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1, X2 | [X3, X4 | []]`", then
[`list_elements(Node)`](`list_elements/1`) yields the list `[X1, X2,
X3, X4]`.

_See also: _`is_proper_list/1`, `list/2`.

# `list_head`

```erlang
-spec list_head(syntaxTree()) -> syntaxTree().
```

Returns the head element subtree of a `list` node.

If `Node` represents "`[Head ...]`", the result will represent "`Head`".

_See also: _`cons/2`, `list/2`, `list_tail/1`.

# `list_length`

```erlang
-spec list_length(syntaxTree()) -> non_neg_integer().
```

Returns the number of element subtrees of a list skeleton.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1 | [X2, X3 | [X4, X5, X6]]]`", then
[`list_length(Node)`](`list_length/1`) returns the integer 6.

> #### Note {: .info }
>
> This is equivalent to [`length(list_elements(Node))`](`length/1`), but
> potentially more efficient.

_See also: _`is_proper_list/1`, `list/2`, `list_elements/1`.

# `list_prefix`

```erlang
-spec list_prefix(syntaxTree()) -> [syntaxTree()].
```

Returns the prefix element subtrees of a `list` node.

If `Node` represents "`[E1, ..., En]`" or "`[E1, ..., En |
Tail]`", the returned value is `[E1, ..., En]`.

_See also: _`list/2`.

# `list_suffix`

```erlang
-spec list_suffix(syntaxTree()) -> none | syntaxTree().
```

Returns the suffix subtree of a `list` node, if one exists.

If `Node` represents "`[E1, ..., En | Tail]`", the returned value is
`Tail`. Otherwise, if `Node` represents "`[E1, ..., En]`", `none` is
returned.

> #### Note {: .info }
>
> Even if this function returns a `Tail` that is not `none`, the type
> of `Tail` can be `nil` if the tail has been given explicitly and the
> list skeleton has not been compacted (see `compact_list/1`).

_See also: _`compact_list/1`, `list/2`, `nil/0`.

# `list_tail`

```erlang
-spec list_tail(syntaxTree()) -> syntaxTree().
```

Returns the tail of a `list` node.

If `Node` represents a single-element list "`[E]`", then the result
has type `nil`, representing "`[]`". If `Node` represents "`[E1,
E2 ...]`", the result will represent "`[E2 ...]`", and if `Node`
represents "`[Head | Tail]`", the result will represent
"`Tail`".

_See also: _`cons/2`, `list/2`, `list_head/1`.

# `macro`

```erlang
-spec macro(syntaxTree()) -> syntaxTree().
```

# `macro`

```erlang
-spec macro(syntaxTree(), none | [syntaxTree()]) -> syntaxTree().
```

Creates an abstract macro application.

If `Arguments` is `none`, the result represents "`?Name`",
otherwise, if `Arguments` is `[A1, ..., An]`, the result represents
"`?Name(A1, ..., An)`".

Notes: if `Arguments` is the empty list, the result will thus represent
"`?Name()`", including a pair of matching parentheses.

The only syntactical limitation imposed by the preprocessor on the arguments to
a macro application (viewed as sequences of tokens) is that they must be
balanced with respect to parentheses, brackets, `begin ... end`, `case ... end`,
and so on. The `text` node type can be used to represent arguments which are not
regular Erlang constructs.

_See also: _`macro/1`, `macro_arguments/1`, `macro_name/1`, `text/1`.

# `macro_arguments`

```erlang
-spec macro_arguments(syntaxTree()) -> none | [syntaxTree()].
```

Returns the list of argument subtrees of a `macro` node, if any.

If `Node` represents "`?Name`", `none` is returned. Otherwise, if
`Node` represents "`?Name(A1, ..., An)`", `[A1, ..., An]` is
returned.

_See also: _`macro/2`.

# `macro_name`

```erlang
-spec macro_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `macro` node.

_See also: _`macro/2`.

# `make_tree`

```erlang
-spec make_tree(atom(), [[syntaxTree()]]) -> syntaxTree().
```

Creates a syntax tree with the given type and subtrees.

`Type` must be a node type name (see `type/1`) that does not denote a
leaf node type (see `is_leaf/1`). `Groups` must be a _nonempty_ list
of groups of syntax trees, representing the subtrees of a node of the
given type, in left-to-right order as they would occur in the printed
program text, grouped by category as done by `subtrees/1`.

The result of
[`copy_attrs(Node, make_tree(type(Node), subtrees(Node)))`](`copy_attrs/2`) (see
`update_tree/2`) represents the same source code text as the original `Node`,
assuming that [`subtrees(Node)`](`subtrees/1`) yields a nonempty list. However,
it does not necessarily have the same data representation as `Node`.

_See also: _`copy_attrs/2`, `is_leaf/1`, `subtrees/1`, `type/1`,
`update_tree/2`.

# `map_comp`

```erlang
-spec map_comp(syntaxTree() | [syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

Creates an abstract map comprehension.

If `Body` is `[E1, ..., En]`, the result represents "`#{Template ||
E1, ..., En}`".

Supports comprehensions with multiple emitted elements per iteration,
from EEP 78 - in such cases, `Template` is a list of key-value associations.

_See also: _`generator/2`, `map_comp_body/1`, `map_comp_template/1`.

# `map_comp_body`

```erlang
-spec map_comp_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `map_comp` node.

_See also: _`map_comp/2`.

# `map_comp_template`

```erlang
-spec map_comp_template(syntaxTree()) -> syntaxTree() | [syntaxTree()].
```

Returns the template subtree of a `map_comp` node.

Supports comprehensions with multiple emitted elements per iteration,
from EEP 78 - in such cases, template will be list of key-value associations.

_See also: _`map_comp/2`.

# `map_expr`

```erlang
-spec map_expr([syntaxTree()]) -> syntaxTree().
```

# `map_expr`

```erlang
-spec map_expr(none | syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract map expression.

If `Fields` is `[F1, ..., Fn]`, then if `Argument` is `none`, the
result represents "`#{F1, ..., Fn}`", otherwise it represents
"`Argument#{F1, ..., Fn}`".

_See also: _`map_expr/1`, `map_expr_argument/1`, `map_expr_fields/1`,
`map_field_assoc/2`, `map_field_exact/2`.

# `map_expr_argument`

```erlang
-spec map_expr_argument(syntaxTree()) -> none | syntaxTree().
```

Returns the argument subtree of a `map_expr` node, if any.

If `Node` represents "`#{...}`", `none` is returned. Otherwise, if
`Node` represents "`Argument#{...}`", `Argument` is returned.

_See also: _`map_expr/2`.

# `map_expr_fields`

```erlang
-spec map_expr_fields(syntaxTree()) -> [syntaxTree()].
```

Returns the list of field subtrees of a `map_expr` node.

_See also: _`map_expr/2`.

# `map_field_assoc`

```erlang
-spec map_field_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract map assoc field.

The result represents "`Name => Value`".

_See also: _`map_expr/2`, `map_field_assoc_name/1`, `map_field_assoc_value/1`.

# `map_field_assoc_name`

```erlang
-spec map_field_assoc_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `map_field_assoc` node.

_See also: _`map_field_assoc/2`.

# `map_field_assoc_value`

```erlang
-spec map_field_assoc_value(syntaxTree()) -> syntaxTree().
```

Returns the value subtree of a `map_field_assoc` node.

_See also: _`map_field_assoc/2`.

# `map_field_exact`

```erlang
-spec map_field_exact(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract map exact field.

The result represents "`Name := Value`".

_See also: _`map_expr/2`, `map_field_exact_name/1`, `map_field_exact_value/1`.

# `map_field_exact_name`

```erlang
-spec map_field_exact_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `map_field_exact` node.

_See also: _`map_field_exact/2`.

# `map_field_exact_value`

```erlang
-spec map_field_exact_value(syntaxTree()) -> syntaxTree().
```

Returns the value subtree of a `map_field_exact` node.

_See also: _`map_field_exact/2`.

# `map_generator`

```erlang
-spec map_generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract map_generator.

The result represents "`Pattern <- Body`".

_See also: _`binary_comp/2`, `list_comp/2`, `map_comp/2`, `map_generator_body/1`,
`map_generator_pattern/1`.

# `map_generator_body`

```erlang
-spec map_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `map_generator` node.

_See also: _`map_generator/2`.

# `map_generator_pattern`

```erlang
-spec map_generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `map_generator` node.

_See also: _`map_generator/2`.

# `map_type`

```erlang
-spec map_type() -> syntaxTree().
```

# `map_type`

```erlang
-spec map_type(any_size | [syntaxTree()]) -> syntaxTree().
```

Creates an abstract type map.

If `Fields` is `[F1, ..., Fn]`, the result represents "`#{F1, ...,
Fn}`"; otherwise, if `Fields` is `any_size`, it represents
"`t:map/0`".

_See also: _`map_type_fields/1`.

# `map_type_assoc`

```erlang
-spec map_type_assoc(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract map type assoc field.

The result represents "`Name => Value`".

_See also: _`map_type/1`, `map_type_assoc_name/1`, `map_type_assoc_value/1`.

# `map_type_assoc_name`

```erlang
-spec map_type_assoc_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `map_type_assoc` node.

_See also: _`map_type_assoc/2`.

# `map_type_assoc_value`

```erlang
-spec map_type_assoc_value(syntaxTree()) -> syntaxTree().
```

Returns the value subtree of a `map_type_assoc` node.

_See also: _`map_type_assoc/2`.

# `map_type_exact`

```erlang
-spec map_type_exact(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract map type exact field.

The result represents "`Name := Value`".

_See also: _`map_type/1`, `map_type_exact_name/1`, `map_type_exact_value/1`.

# `map_type_exact_name`

```erlang
-spec map_type_exact_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `map_type_exact` node.

_See also: _`map_type_exact/2`.

# `map_type_exact_value`

```erlang
-spec map_type_exact_value(syntaxTree()) -> syntaxTree().
```

Returns the value subtree of a `map_type_exact` node.

_See also: _`map_type_exact/2`.

# `map_type_fields`

```erlang
-spec map_type_fields(syntaxTree()) -> any_size | [syntaxTree()].
```

Returns the list of field subtrees of a `map_type` node.

If `Node` represents "`t:map/0`", `any_size` is returned; otherwise,
if `Node` represents "`#{F1, ..., Fn}`", `[F1, ..., Fn]` is
returned.

_See also: _`map_type/0`, `map_type/1`.

# `match_expr`

```erlang
-spec match_expr(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract match-expression.

The result represents "`Pattern = Body`".

_See also: _`match_expr_body/1`, `match_expr_pattern/1`.

# `match_expr_body`

```erlang
-spec match_expr_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `match_expr` node.

_See also: _`match_expr/2`.

# `match_expr_pattern`

```erlang
-spec match_expr_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `match_expr` node.

_See also: _`match_expr/2`.

# `maybe_expr`

```erlang
-spec maybe_expr([syntaxTree()]) -> syntaxTree().
```

# `maybe_expr`

```erlang
-spec maybe_expr([syntaxTree()], none | syntaxTree()) -> syntaxTree().
```

Creates an abstract maybe-expression.

If `Body` is `[B1, ..., Bn]`, and `OptionalElse` is `none`, the result
represents "`maybe B1, ..., Bn end`".  If `Body` is `[B1, ...,
Bn]`, and `OptionalElse` reprsents an `else_expr` node with clauses
`[C1, ..., Cn]`, the result represents "`maybe B1, ..., Bn else
C1; ..., Cn end`".

See `clause` for documentation on `m:erl_parse` clauses.

_See also: _`maybe_expr_body/1`, `maybe_expr_else/1`.

# `maybe_expr_body`

```erlang
-spec maybe_expr_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `maybe_expr` node.

_See also: _`maybe_expr/2`.

# `maybe_expr_else`

```erlang
-spec maybe_expr_else(syntaxTree()) -> none | syntaxTree().
```

Returns the else subtree of a `maybe_expr` node.

_See also: _`maybe_expr/2`.

# `maybe_match_expr`

```erlang
-spec maybe_match_expr(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract maybe-expression, as used in `maybe` blocks.

The result represents "`Pattern ?= Body`".

_See also: _`maybe_expr/2`, `maybe_match_expr_body/1`,
`maybe_match_expr_pattern/1`.

# `maybe_match_expr_body`

```erlang
-spec maybe_match_expr_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `maybe_expr` node.

_See also: _`maybe_match_expr/2`.

# `maybe_match_expr_pattern`

```erlang
-spec maybe_match_expr_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `maybe_expr` node.

_See also: _`maybe_match_expr/2`.

# `meta`

```erlang
-spec meta(syntaxTree()) -> syntaxTree().
```

Creates a meta-representation of a syntax tree.

The result represents an Erlang expression "`MetaTree`" which, if
evaluated, will yield a new syntax tree representing the same source
code text as `Tree` (although the actual data representation may be
different). The expression represented by `MetaTree` is
_implementation independent_ with regard to the data structures used
by the abstract syntax tree implementation. Comments attached to nodes
of `Tree` will be preserved, but other attributes are lost.

Any node in `Tree` whose node type is `variable` (see `type/1`), and whose list
of annotations (see `get_ann/1`) contains the atom `meta_var`, will remain
unchanged in the resulting tree, except that exactly one occurrence of
`meta_var` is removed from its annotation list.

The main use of the function [`meta/1`](`meta/1`) is to transform a
data structure `Tree`, which represents a piece of program code, into
a form that is _representation independent when printed_. For example,
suppose `Tree` represents a variable named "V". Then (assuming a
function `print/1` for printing syntax trees), evaluating
`print(abstract(Tree))` — simply using `abstract/1` to map the actual
data structure onto a syntax tree representation — would output a
string that might look something like "`{tree, variable, ..., "V",
...}`", which is obviously dependent on the implementation of the
abstract syntax trees. This could, for example, be useful for caching
a syntax tree in a file. However, in some situations like in a program
generator generator (with two "generator"), it may be
unacceptable. Using `print(meta(Tree))` instead would output a
_representation independent_ syntax tree generating expression; in the
above case, something like "`erl_syntax:variable("V")`".

_See also: _`abstract/1`, `get_ann/1`, `type/1`.

# `module_qualifier`

```erlang
-spec module_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract module qualifier.

The result represents "`Module:Body`".

_See also: _`module_qualifier_argument/1`, `module_qualifier_body/1`.

# `module_qualifier_argument`

```erlang
-spec module_qualifier_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument (the module) subtree of a `module_qualifier` node.

_See also: _`module_qualifier/2`.

# `module_qualifier_body`

```erlang
-spec module_qualifier_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `module_qualifier` node.

_See also: _`module_qualifier/2`.

# `named_fun_expr`

```erlang
-spec named_fun_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract named fun-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`fun
Name C1; ...; Name Cn end`". More exactly, if each `Ci`
represents "`(Pi1, ..., Pim) Gi -> Bi`", then the result
represents "`fun Name(P11, ..., P1m) G1 -> B1; ...;
Name(Pn1, ..., Pnm) Gn -> Bn end`".

_See also: _`named_fun_expr_arity/1`, `named_fun_expr_clauses/1`,
`named_fun_expr_name/1`.

# `named_fun_expr_arity`

```erlang
-spec named_fun_expr_arity(syntaxTree()) -> arity().
```

Returns the arity of a `named_fun_expr` node.

The result is the number of parameter patterns in the first clause of
the named fun-expression; subsequent clauses are ignored.

An exception is thrown if
[`named_fun_expr_clauses(Node)`](`named_fun_expr_clauses/1`) returns an empty
list, or if the first element of that list is not a syntax tree `C` of type
`clause` such that [`clause_patterns(C)`](`clause_patterns/1`) is a nonempty
list.

_See also: _`clause/3`, `clause_patterns/1`, `named_fun_expr/2`,
`named_fun_expr_clauses/1`.

# `named_fun_expr_clauses`

```erlang
-spec named_fun_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of a `named_fun_expr` node.

_See also: _`named_fun_expr/2`.

# `named_fun_expr_name`

```erlang
-spec named_fun_expr_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `named_fun_expr` node.

_See also: _`named_fun_expr/2`.

# `normalize_list`

```erlang
-spec normalize_list(syntaxTree()) -> syntaxTree().
```

Expands an abstract list skeleton to its most explicit form.

If `Node` represents "`[E1, ..., En | Tail]`", the result
represents "`[E1 | ... [En | Tail1] ... ]`", where `Tail1` is
the result of [`normalize_list(Tail)`](`normalize_list/1`). If `Node`
represents "`[E1, ..., En]`", the result simply represents "`[E1
| ... [En | []] ... ]`". If `Node` does not represent a list
skeleton, `Node` itself is returned.

_See also: _`compact_list/1`, `list/2`.

# `operator`

```erlang
-spec operator(atom() | string()) -> syntaxTree().
```

Creates an abstract operator.

The name of the operator is the character sequence represented by
`Name`. This is analogous to the print name of an atom, but an
operator is never written within single-quotes; for example, the
result of [`operator('++')`](`operator/1`) represents "`++`" rather
than "`'++'`".

_See also: _`atom/1`, `operator_literal/1`, `operator_name/1`.

# `operator_literal`

```erlang
-spec operator_literal(syntaxTree()) -> string().
```

Returns the literal string represented by an `operator` node.

This is simply the operator name as a string.

_See also: _`operator/1`.

# `operator_name`

```erlang
-spec operator_name(syntaxTree()) -> atom().
```

Returns the name of an `operator` node.

Note that the name is returned as an atom.

_See also: _`operator/1`.

# `parentheses`

```erlang
-spec parentheses(syntaxTree()) -> syntaxTree().
```

Creates an abstract parenthesised expression.

The result represents "`(Body)`", independently of the context.

_See also: _`parentheses_body/1`.

# `parentheses_body`

```erlang
-spec parentheses_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `parentheses` node.

_See also: _`parentheses/1`.

# `prefix_expr`

```erlang
-spec prefix_expr(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract prefix operator expression.

The result represents "`Operator Argument`".

_See also: _`infix_expr/3`, `prefix_expr_argument/1`, `prefix_expr_operator/1`.

# `prefix_expr_argument`

```erlang
-spec prefix_expr_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument subtree of a `prefix_expr` node.

_See also: _`prefix_expr/2`.

# `prefix_expr_operator`

```erlang
-spec prefix_expr_operator(syntaxTree()) -> syntaxTree().
```

Returns the operator subtree of a `prefix_expr` node.

_See also: _`prefix_expr/2`.

# `receive_expr`

```erlang
-spec receive_expr([syntaxTree()]) -> syntaxTree().
```

# `receive_expr`

```erlang
-spec receive_expr([syntaxTree()], none | syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract receive-expression.

If `Timeout` is `none`, the result represents "`receive C1; ...;
Cn end`" (the `Action` argument is ignored).  Otherwise, if
`Clauses` is `[C1, ..., Cn]` and `Action` is `[A1, ..., Am]`, the
result represents "`receive C1; ...; Cn after Timeout -> A1,
..., Am end`". More exactly, if each `Ci` represents "`(Pi) Gi
-> Bi`", then the result represents "`receive P1 G1 -> B1; ...;
Pn Gn -> Bn ... end`".

Note that in Erlang, a receive-expression must have at least one clause if no
timeout part is specified.

_See also: _`case_expr/2`, `clause/3`, `receive_expr/1`,
`receive_expr_action/1`, `receive_expr_clauses/1`, `receive_expr_timeout/1`.

# `receive_expr_action`

```erlang
-spec receive_expr_action(syntaxTree()) -> [syntaxTree()].
```

Returns the list of action body subtrees of a `receive_expr` node.

If `Node` represents "`receive C1; ...; Cn end`", this is the
empty list.

_See also: _`receive_expr/3`.

# `receive_expr_clauses`

```erlang
-spec receive_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of clause subtrees of a `receive_expr` node.

_See also: _`receive_expr/3`.

# `receive_expr_timeout`

```erlang
-spec receive_expr_timeout(syntaxTree()) -> none | syntaxTree().
```

Returns the timeout subtree of a `receive_expr` node, if any.

If `Node` represents "`receive C1; ...; Cn end`", `none` is
returned. Otherwise, if `Node` represents "`receive C1; ...; Cn
after Timeout -> ... end`", `Timeout` is returned.

_See also: _`receive_expr/3`.

# `record_access`

```erlang
-spec record_access(syntaxTree(), syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract record field access expression.

The result represents "`Argument#Type.Field`".

_See also: _`record_access_argument/1`, `record_access_field/1`,
`record_access_type/1`, `record_expr/3`.

# `record_access_argument`

```erlang
-spec record_access_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument subtree of a `record_access` node.

_See also: _`record_access/3`.

# `record_access_field`

```erlang
-spec record_access_field(syntaxTree()) -> syntaxTree().
```

Returns the field subtree of a `record_access` node.

_See also: _`record_access/3`.

# `record_access_type`

```erlang
-spec record_access_type(syntaxTree()) -> syntaxTree().
```

Returns the type subtree of a `record_access` node.

_See also: _`record_access/3`.

# `record_expr`

```erlang
-spec record_expr(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

# `record_expr`

```erlang
-spec record_expr(none | syntaxTree(), syntaxTree() | [syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

Creates an abstract record expression.

If `Fields` is `[F1, ..., Fn]`, then if `Argument` is `none`, the
result represents "`#Type{F1, ..., Fn}`", otherwise it
represents "`Argument#Type{F1, ..., Fn}`".

_See also: _`record_access/3`, `record_expr/2`, `record_expr_argument/1`,
`record_expr_fields/1`, `record_expr_type/1`, `record_field/2`,
`record_index_expr/2`.

# `record_expr_argument`

```erlang
-spec record_expr_argument(syntaxTree()) -> none | syntaxTree().
```

Returns the argument subtree of a `record_expr` node, if any.

If `Node` represents "`#Type{...}`", `none` is returned. Otherwise,
if `Node` represents "`Argument#Type{...}`", `Argument` is
returned.

_See also: _`record_expr/3`.

# `record_expr_fields`

```erlang
-spec record_expr_fields(syntaxTree()) -> [syntaxTree()].
```

Returns the list of field subtrees of a `record_expr` node.

_See also: _`record_expr/3`.

# `record_expr_type`

```erlang
-spec record_expr_type(syntaxTree()) -> syntaxTree().
```

Returns the type subtree of a `record_expr` node.

_See also: _`record_expr/3`.

# `record_field`

```erlang
-spec record_field(syntaxTree()) -> syntaxTree().
```

# `record_field`

```erlang
-spec record_field(syntaxTree(), none | syntaxTree()) -> syntaxTree().
```

Creates an abstract record field specification.

If `Value` is `none`, the result represents simply "`Name`",
otherwise it represents "`Name = Value`".

_See also: _`record_expr/3`, `record_field_name/1`, `record_field_value/1`.

# `record_field_name`

```erlang
-spec record_field_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `record_field` node.

_See also: _`record_field/2`.

# `record_field_value`

```erlang
-spec record_field_value(syntaxTree()) -> none | syntaxTree().
```

Returns the value subtree of a `record_field` node, if any.

If `Node` represents "`Name`", `none` is returned. Otherwise, if
`Node` represents "`Name = Value`", `Value` is returned.

_See also: _`record_field/2`.

# `record_index_expr`

```erlang
-spec record_index_expr(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract record field index expression. The result represents
"`#Type.Field`".

> #### Note {: .info }
>
> The function name `record_index/2` is reserved by the Erlang compiler,
> which is why that name could not be used for this constructor.

_See also: _`record_expr/3`, `record_index_expr_field/1`,
`record_index_expr_type/1`.

# `record_index_expr_field`

```erlang
-spec record_index_expr_field(syntaxTree()) -> syntaxTree().
```

Returns the field subtree of a `record_index_expr` node.

_See also: _`record_index_expr/2`.

# `record_index_expr_type`

```erlang
-spec record_index_expr_type(syntaxTree()) -> syntaxTree().
```

Returns the type subtree of a `record_index_expr` node.

_See also: _`record_index_expr/2`.

# `record_type`

```erlang
-spec record_type(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract record type.

If `Fields` is `[F1, ..., Fn]`, the result represents "`#Name{F1,
..., Fn}`".

_See also: _`record_type_fields/1`, `record_type_name/1`.

# `record_type_field`

```erlang
-spec record_type_field(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract record type field.

The result represents "`Name :: Type`".

_See also: _`record_type_field_name/1`, `record_type_field_type/1`.

# `record_type_field_name`

```erlang
-spec record_type_field_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `record_type_field` node.

_See also: _`record_type_field/2`.

# `record_type_field_type`

```erlang
-spec record_type_field_type(syntaxTree()) -> syntaxTree().
```

Returns the type subtree of a `record_type_field` node.

_See also: _`record_type_field/2`.

# `record_type_fields`

```erlang
-spec record_type_fields(syntaxTree()) -> [syntaxTree()].
```

Returns the fields subtree of a `record_type` node.

_See also: _`record_type/2`.

# `record_type_name`

```erlang
-spec record_type_name(syntaxTree()) -> syntaxTree().
```

Returns the name subtree of a `record_type` node.

_See also: _`record_type/2`.

# `remove_comments`

```erlang
-spec remove_comments(syntaxTree()) -> syntaxTree().
```

Clears the associated comments of `Node`.

Note: This is equivalent to
[`set_precomments(set_postcomments(Node, []), [])`](`set_precomments/2`), but
potentially more efficient.

_See also: _`set_postcomments/2`, `set_precomments/2`.

# `revert`

```erlang
-spec revert(syntaxTree()) -> syntaxTree().
```

Returns an `m:erl_parse`-compatible representation of a syntax tree, if possible.

If `Tree` represents a well-formed Erlang program or expression, the conversion
should work without problems. Typically, `is_tree/1` yields `true` if conversion
failed (that is, the result is still an abstract syntax tree), and `false`
otherwise.

The `is_tree/1` test is not completely foolproof. For a few special
node types (for example `arity_qualifier`), if such a node occurs in a
context where it is not expected, it will be left unchanged as a
non-reverted subtree of the result.  This can only happen if `Tree`
does not actually represent legal Erlang code.

_See also: _[//stdlib/erl_parse](`m:erl_parse`), `revert_forms/1`.

# `revert_forms`

```erlang
-spec revert_forms(forms()) -> [erl_parse()].
```

Reverts a sequence of Erlang source code forms.

The sequence can be given either as a `form_list` syntax tree
(possibly nested), or as a list of "program form" syntax trees. If
successful, the corresponding flat list of `m:erl_parse`-compatible
syntax trees is returned (see `revert/1`). If some program form could
not be reverted, `{error, Form}` is thrown. Standalone comments in the
form sequence are discarded.

_See also: _`form_list/1`, `is_form/1`, `revert/1`.

# `set_ann`

```erlang
-spec set_ann(syntaxTree(), [term()]) -> syntaxTree().
```

Sets the list of user annotations of `Node` to `Annotations`.

_See also: _`add_ann/2`, `copy_ann/2`, `get_ann/1`.

# `set_attrs`

```erlang
-spec set_attrs(syntaxTree(), syntaxTreeAttributes()) -> syntaxTree().
```

Sets the attributes of `Node` to `Attributes`.

_See also: _`copy_attrs/2`, `get_attrs/1`.

# `set_pos`

```erlang
-spec set_pos(syntaxTree(), annotation_or_location()) -> syntaxTree().
```

Sets the position information of `Node` to `Pos`.

_See also: _`copy_pos/2`, `get_pos/1`.

# `set_postcomments`

```erlang
-spec set_postcomments(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Sets the post-comments of `Node` to `Comments`.

`Comments` should be a possibly empty list of abstract comments, in
top-down textual order

_See also: _`add_postcomments/2`, `comment/2`, `copy_comments/2`,
`get_postcomments/1`, `join_comments/2`, `remove_comments/1`,
`set_precomments/2`.

# `set_precomments`

```erlang
-spec set_precomments(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Sets the pre-comments of `Node` to `Comments`.

`Comments` should be a possibly empty list of abstract comments, in
top-down textual order.

_See also: _`add_precomments/2`, `comment/2`, `copy_comments/2`,
`get_precomments/1`, `join_comments/2`, `remove_comments/1`,
`set_postcomments/2`.

# `size_qualifier`

```erlang
-spec size_qualifier(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract size qualifier.

The result represents "`Body:Size`".

_See also: _`size_qualifier_argument/1`, `size_qualifier_body/1`.

# `size_qualifier_argument`

```erlang
-spec size_qualifier_argument(syntaxTree()) -> syntaxTree().
```

Returns the argument subtree (the size) of a `size_qualifier` node.

_See also: _`size_qualifier/2`.

# `size_qualifier_body`

```erlang
-spec size_qualifier_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `size_qualifier` node.

_See also: _`size_qualifier/2`.

# `strict_binary_generator`

```erlang
-spec strict_binary_generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract strict binary_generator.

The result represents "`*Pattern*<:- *Body*`".

_See also: _`binary_comp/2`, `strict_binary_generator_body/1`,
`strict_binary_generator_pattern/1`, `list_comp/2`.

# `strict_binary_generator_body`

```erlang
-spec strict_binary_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `generator` node.

_See also: _`strict_binary_generator/2`.

# `strict_binary_generator_pattern`

```erlang
-spec strict_binary_generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `generator` node.

_See also: _`strict_binary_generator/2`.

# `strict_generator`

```erlang
-spec strict_generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract strict list generator.

The result represents "`*Pattern*<:- *Body*`".

_See also: _`binary_comp/2`, `strict_generator_body/1`,
`strict_generator_pattern/1`, `list_comp/2`.

# `strict_generator_body`

```erlang
-spec strict_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `generator` node.

_See also: _`strict_generator/2`.

# `strict_generator_pattern`

```erlang
-spec strict_generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `generator` node.

_See also: _`strict_generator/2`.

# `strict_map_generator`

```erlang
-spec strict_map_generator(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract strict map_generator. The result represents
"`*Pattern*<- *Body*`".

_See also: _`list_comp/2`, `map_comp/2`,
`strict_map_generator_body/1`,
`strict_map_generator_pattern/1`.

# `strict_map_generator_body`

```erlang
-spec strict_map_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `generator` node.

_See also: _`strict_map_generator/2`.

# `strict_map_generator_pattern`

```erlang
-spec strict_map_generator_pattern(syntaxTree()) -> syntaxTree().
```

Returns the pattern subtree of a `generator` node.

_See also: _`strict_map_generator/2`.

# `string`

```erlang
-spec string(string()) -> syntaxTree().
```

Creates an abstract string literal.

The result represents `"Text"` (including the surrounding
double-quotes), where `Text` corresponds to the sequence of characters
in `Value`, but not representing a _specific_ string literal.

For example, the result of [`string("x\ny")`](`string/1`) represents any and all
of `"x\ny"`, `"x\12y"`, `"x\012y"` and `"x\^Jy"`; see `char/1`.

_See also: _`char/1`, `is_string/2`, `string_literal/1`, `string_literal/2`,
`string_value/1`.

# `string_literal`

```erlang
-spec string_literal(syntaxTree()) -> nonempty_string().
```

Returns the literal string represented by a `string` node.

This includes surrounding double-quote characters. Characters beyond
255 will be escaped.

_See also: _`string/1`.

# `string_literal`

```erlang
-spec string_literal(syntaxTree(), encoding()) -> nonempty_string().
```

Returns the literal string represented by a `string` node.

This includes surrounding double-quote characters. Depending on the
encoding characters beyond 255 will be escaped (`latin1`) or copied as
is (`utf8`).

_See also: _`string/1`.

# `string_value`

```erlang
-spec string_value(syntaxTree()) -> string().
```

Returns the value represented by a `string` node.

_See also: _`string/1`.

# `subtrees`

```erlang
-spec subtrees(syntaxTree()) -> [[syntaxTree()]].
```

Returns the grouped list of all subtrees of a syntax tree.

If `Node` is a leaf node (see `is_leaf/1`), this is the empty list,
otherwise the result is always a nonempty list, containing the lists
of subtrees of `Node`, in left-to-right order as they occur in the
printed program text, and grouped by category. Often, each group
contains only a single subtree.

Depending on the type of `Node`, the size of some groups may be
variable (for example, the group consisting of all the elements of a
tuple), while others always contain the same number of elements —
usually exactly one (for example, the group containing the argument
expression of a case-expression). Note, however, that the exact
structure of the returned list (for a given node type) should in
general not be depended upon, since it might be subject to change
without notice.

The function `subtrees/1` and the constructor functions `make_tree/2` and
`update_tree/2` can be a great help if one wants to traverse a syntax tree,
visiting all its subtrees, but treat nodes of the tree in a uniform way in most
or all cases. Using these functions makes this simple, and also assures that
your code is not overly sensitive to extensions of the syntax tree data type,
because any node types not explicitly handled by your code can be left to a
default case.

For example:

```text
     postorder(F, Tree) ->
        F(case subtrees(Tree) of
            [] -> Tree;
            List -> update_tree(Tree,
                                [[postorder(F, Subtree)
                                  || Subtree &lt;- Group]
                                 || Group &lt;- List])
          end).
```

maps the function `F` on `Tree` and all its subtrees, doing a post-order
traversal of the syntax tree. (Note the use of `update_tree/2` to preserve node
attributes.) For a simple function like:

```text
     f(Node) ->
        case type(Node) of
            atom -> atom("a_" ++ atom_name(Node));
            _ -> Node
        end.
```

the call `postorder(fun f/1, Tree)` will yield a new representation of `Tree` in
which all atom names have been extended with the prefix "a_", but nothing else
(including comments, annotations, and line numbers) has been changed.

_See also: _`copy_attrs/2`, `is_leaf/1`, `make_tree/2`, `type/1`.

# `text`

```erlang
-spec text(string()) -> syntaxTree().
```

Creates an abstract piece of source code text.

The result represents exactly the sequence of characters in
`String`. This is useful in cases where one wants full control of the
resulting output, such as the appearance of floating-point numbers or
macro definitions.

_See also: _`text_string/1`.

# `text_string`

```erlang
-spec text_string(syntaxTree()) -> string().
```

Returns the character sequence represented by a `text` node.

_See also: _`text/1`.

# `tree`

```erlang
-spec tree(atom()) -> tree().
```

# `tree`

```erlang
-spec tree(atom(), term()) -> tree().
```

**For special purposes only**. Creates an abstract syntax tree node with type tag
`Type` and associated data `Data`.

This function and the related `is_tree/1` and `data/1` provide a uniform way to
extend the set of `erl_parse` node types. The associated data is any term, whose
format may depend on the type tag.

> #### Notes {: .info }
>
> - Any nodes created outside of this module must have type tags distinct from
>   those currently defined by this module; see `type/1` for a complete list.
>
> - The type tag of a syntax tree node may also be used as a primary tag by the
>   `erl_parse` representation; in that case, the selector functions for that node
>   type _must_ handle both the abstract syntax tree and the `m:erl_parse` form. The
>   function [`type(T)`](`type/1`) should return the correct type tag regardless
>   of the representation of `T`, so that the user sees no difference between
>   `erl_syntax` and `erl_parse` nodes.

_See also: _`data/1`, `is_tree/1`, `type/1`.

# `try_after_expr`

```erlang
-spec try_after_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

# `try_expr`

```erlang
-spec try_expr([syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

# `try_expr`

```erlang
-spec try_expr([syntaxTree()], [syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

# `try_expr`

```erlang
-spec try_expr([syntaxTree()], [syntaxTree()], [syntaxTree()], [syntaxTree()]) -> syntaxTree().
```

Creates an abstract try-expression.

If `Body` is `[B1, ..., Bn]`, `Clauses` is `[C1, ..., Cj]`, `Handlers`
is `[H1, ..., Hk]`, and `After` is `[A1, ..., Am]`, the result
represents "`try B1, ..., Bn of C1; ...; Cj catch H1; ...; Hk after
A1, ..., Am end`".  More exactly, if each `Ci` represents "`(CPi) CGi
-> CBi`", and each `Hi` represents "`(HPi) HGi -> HBi`", then the
result represents "`try B1, ..., Bn of CP1 CG1 -> CB1; ...; CPj CGj ->
CBj catch HP1 HG1 -> HB1; ...; HPk HGk -> HBk after A1, ..., Am end`";
see `case_expr/2`. If `Clauses` is the empty list, the `of ...`
section is left out. If `After` is the empty list, the `after ...`
section is left out. If `Handlers` is the empty list, and `After` is
nonempty, the `catch ...` section is left out.

_See also: _`case_expr/2`, `class_qualifier/2`, `clause/3`, `try_after_expr/2`,
`try_expr/2`, `try_expr/3`, `try_expr_after/1`, `try_expr_body/1`,
`try_expr_clauses/1`, `try_expr_handlers/1`.

# `try_expr_after`

```erlang
-spec try_expr_after(syntaxTree()) -> [syntaxTree()].
```

Returns the list of "after" subtrees of a `try_expr` node.

_See also: _`try_expr/4`.

# `try_expr_body`

```erlang
-spec try_expr_body(syntaxTree()) -> [syntaxTree()].
```

Returns the list of body subtrees of a `try_expr` node.

_See also: _`try_expr/4`.

# `try_expr_clauses`

```erlang
-spec try_expr_clauses(syntaxTree()) -> [syntaxTree()].
```

Returns the list of case-clause subtrees of a `try_expr` node. If `Node`
represents "`try Body catch H1; ...; Hn end`", the result is the empty
list.

_See also: _`try_expr/4`.

# `try_expr_handlers`

```erlang
-spec try_expr_handlers(syntaxTree()) -> [syntaxTree()].
```

Returns the list of handler-clause subtrees of a `try_expr` node.

_See also: _`try_expr/4`.

# `tuple`

```erlang
-spec tuple([syntaxTree()]) -> syntaxTree().
```

Creates an abstract tuple.

If `Elements` is `[X1, ..., Xn]`, the result represents "`{X1, ...,
Xn}`".

> #### Note {: .info }
>
> The Erlang language has distinct 1-tuples, meaning `{X}` is always distinct
> from `X` itself.

_See also: _`tuple_elements/1`, `tuple_size/1`.

# `tuple_elements`

```erlang
-spec tuple_elements(syntaxTree()) -> [syntaxTree()].
```

Returns the list of element subtrees of a `tuple` node.

_See also: _`tuple/1`.

# `tuple_size`

```erlang
-spec tuple_size(syntaxTree()) -> non_neg_integer().
```

Returns the number of elements of a `tuple` node.

> #### Note {: .info }
>
> This is equivalent to [`length(tuple_elements(Node))`](`length/1`),
> but potentially more efficient.

_See also: _`tuple/1`, `tuple_elements/1`.

# `tuple_type`

```erlang
-spec tuple_type() -> syntaxTree().
```

# `tuple_type`

```erlang
-spec tuple_type(any_size | [syntaxTree()]) -> syntaxTree().
```

Creates an abstract type tuple.

If `Elements` is `[T1, ..., Tn]`, the result represents "`{T1, ...,
Tn}`"; otherwise, if `Elements` is `any_size`, it represents
"`t:tuple/0`".

_See also: _`tuple_type_elements/1`.

# `tuple_type_elements`

```erlang
-spec tuple_type_elements(syntaxTree()) -> any_size | [syntaxTree()].
```

Returns the list of type element subtrees of a `tuple_type` node.

If `Node` represents "`t:tuple/0`", `any_size` is returned; otherwise,
if `Node` represents "`{T1, ..., Tn}`", `[T1, ..., Tn]` is returned.

_See also: _`tuple_type/0`, `tuple_type/1`.

# `type`

```erlang
-spec type(syntaxTree()) -> atom().
```

Returns the type tag of `Node`.

If `Node` does not represent a syntax tree, evaluation fails with
reason `badarg`. Node types currently defined by this module are:

* `application`
* `annotated_type`
* `arity_qualifier`
* `atom`
* `attribute`
* `binary`
* `binary_field`
* `bitstring_type`
* `block_expr`
* `case_expr`
* `catch_expr`
* `char`
* `class_qualifier`
* `clause`
* `comment`
* `conjunction`
* `constrained_function_type`
* `constraint`
* `disjunction`
* `else_expr`
* `eof_marker`
* `error_marker`
* `float`
* `form_list`
* `fun_expr`
* `fun_type`
* `function`
* `function_type`
* `generator`
* `if_expr`
* `implicit_fun`
* `infix_expr`
* `integer`
* `integer_range_type`
* `list`
* `list_comp`
* `macro`
* `map_expr`
* `map_field_assoc`
* `map_field_exact`
* `map_type`
* `map_type_assoc`
* `map_type_exact`
* `match_expr`
* `maybe_expr`
* `maybe_match_expr`
* `module_qualifier`
* `named_fun_expr`
* `nil`
* `operator`
* `parentheses`
* `prefix_expr`
* `receive_expr`
* `record_access`
* `record_expr`
* `record_field`
* `record_index_expr`
* `record_type`
* `record_type_field`
* `size_qualifier`
* `string`
* `text`
* `try_expr`
* `tuple`
* `tuple_type`
* `typed_record_field`
* `type_application`
* `type_union`
* `underscore`
* `user_type_application`
* `variable`
* `warning_marker`
* `zip_generator`

The user may (for special purposes) create additional nodes with other type
tags, using the `tree/2` function.

Note: The primary constructor functions for a node type should always have the
same name as the node type itself.

_See also: _`annotated_type/2`, `application/3`, `arity_qualifier/2`, `atom/1`,
`attribute/2`, `binary/1`, `binary_field/2`, `bitstring_type/2`, `block_expr/1`,
`case_expr/2`, `catch_expr/1`, `char/1`, `class_qualifier/2`, `clause/3`,
`comment/2`, `conjunction/1`, `constrained_function_type/2`, `constraint/2`,
`disjunction/1`, `else_expr/1`, `eof_marker/0`, `error_marker/1`, `float/1`,
`form_list/1`, `fun_expr/1`, `fun_type/0`, `function/2`, `function_type/1`,
`function_type/2`, `generator/2`, `if_expr/1`, `implicit_fun/2`, `infix_expr/3`,
`integer/1`, `integer_range_type/2`, `list/2`, `list_comp/2`, `macro/2`,
`map_expr/2`, `map_field_assoc/2`, `map_field_exact/2`, `map_type/0`,
`map_type/1`, `map_type_assoc/2`, `map_type_exact/2`, `match_expr/2`,
`maybe_expr/1`, `maybe_expr/2`, `maybe_match_expr/2`, `module_qualifier/2`,
`named_fun_expr/2`, `nil/0`, `operator/1`, `parentheses/1`, `prefix_expr/2`,
`receive_expr/3`, `record_access/3`, `record_expr/2`, `record_field/2`,
`record_index_expr/2`, `record_type/2`, `record_type_field/2`,
`size_qualifier/2`, `string/1`, `text/1`, `tree/2`, `try_expr/3`, `tuple/1`,
`tuple_type/0`, `tuple_type/1`, `type_application/2`, `type_union/1`,
`typed_record_field/2`, `underscore/0`, `user_type_application/2`, `variable/1`,
`warning_marker/1`,`zip_generator/1`.

# `type_application`

```erlang
-spec type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract type application expression.

If `Arguments` is `[T1, ..., Tn]`, the result represents
"`TypeName(T1, ...Tn)`".

_See also: _`type_application/3`, `type_application_arguments/1`,
`type_application_name/1`, `user_type_application/2`.

# `type_application`

```erlang
-spec type_application(none | syntaxTree(), syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract type application expression.

If `Module` is `none`, this is call is equivalent to
[`type_application(TypeName, Arguments)`](`type_application/2`),
otherwise it is equivalent to
[`type_application(module_qualifier(Module, TypeName),
Arguments)`](`type_application/2`).

(This is a utility function.)

_See also: _`module_qualifier/2`, `type_application/2`.

# `type_application_arguments`

```erlang
-spec type_application_arguments(syntaxTree()) -> [syntaxTree()].
```

Returns the arguments subtrees of a `type_application` node.

_See also: _`type_application/2`.

# `type_application_name`

```erlang
-spec type_application_name(syntaxTree()) -> syntaxTree().
```

Returns the type name subtree of a `type_application` node.

_See also: _`type_application/2`.

# `type_union`

```erlang
-spec type_union([syntaxTree()]) -> syntaxTree().
```

Creates an abstract type union.

If `Types` is `[T1, ..., Tn]`, the result represents "`T1 | ... |
Tn`".

_See also: _`type_union_types/1`.

# `type_union_types`

```erlang
-spec type_union_types(syntaxTree()) -> [syntaxTree()].
```

Returns the list of type subtrees of a `type_union` node.

_See also: _`type_union/1`.

# `typed_record_field`

```erlang
-spec typed_record_field(syntaxTree(), syntaxTree()) -> syntaxTree().
```

Creates an abstract typed record field specification.

The result represents "`Field :: Type`".

_See also: _`typed_record_field_body/1`, `typed_record_field_type/1`.

# `typed_record_field_body`

```erlang
-spec typed_record_field_body(syntaxTree()) -> syntaxTree().
```

Returns the field subtree of a `typed_record_field` node.

_See also: _`typed_record_field/2`.

# `typed_record_field_type`

```erlang
-spec typed_record_field_type(syntaxTree()) -> syntaxTree().
```

Returns the type subtree of a `typed_record_field` node.

_See also: _`typed_record_field/2`.

# `underscore`

```erlang
-spec underscore() -> syntaxTree().
```

Creates an abstract universal pattern ("`_`").

The lexical representation is a single underscore character. Note that
this is _not_ a variable, lexically speaking.

_See also: _`variable/1`.

# `update_tree`

```erlang
-spec update_tree(syntaxTree(), [[syntaxTree()]]) -> syntaxTree().
```

Creates a syntax tree with the same type and attributes as the given tree.

This is equivalent to [`copy_attrs(Node, make_tree(type(Node),
Groups))`](`copy_attrs/2`).

_See also: _`copy_attrs/2`, `make_tree/2`, `type/1`.

# `user_type_application`

```erlang
-spec user_type_application(syntaxTree(), [syntaxTree()]) -> syntaxTree().
```

Creates an abstract user type.

If `Arguments` is `[T1, ..., Tn]`, the result represents
"`TypeName(T1, ...Tn)`".

_See also: _`type_application/2`, `user_type_application_arguments/1`,
`user_type_application_name/1`.

# `user_type_application_arguments`

```erlang
-spec user_type_application_arguments(syntaxTree()) -> [syntaxTree()].
```

Returns the arguments subtrees of a `user_type_application` node.

_See also: _`user_type_application/2`.

# `user_type_application_name`

```erlang
-spec user_type_application_name(syntaxTree()) -> syntaxTree().
```

Returns the type name subtree of a `user_type_application` node.

_See also: _`user_type_application/2`.

# `variable`

```erlang
-spec variable(atom() | string()) -> syntaxTree().
```

Creates an abstract variable with the given name.

`Name` may be any atom or string that represents a lexically valid
variable name, but _not_ a single underscore character; see
`underscore/0`.

> #### Note {: .info }
>
> No check is performed to verify whether the character sequence
> represents a proper variable name, that is, whether its first character
> is an uppercase Erlang character, or whether it contains illegal characters
> such as control characters or whitespace.

_See also: _`underscore/0`, `variable_literal/1`, `variable_name/1`.

# `variable_literal`

```erlang
-spec variable_literal(syntaxTree()) -> string().
```

Returns the name of a `variable` node as a string.

_See also: _`variable/1`.

# `variable_name`

```erlang
-spec variable_name(syntaxTree()) -> atom().
```

Returns the name of a `variable` node as an atom.

_See also: _`variable/1`.

# `warning_marker`

```erlang
-spec warning_marker(term()) -> syntaxTree().
```

Creates an abstract warning marker.

The result represents an occurrence of a possible problem in the
source code, with an associated Erlang I/O ErrorInfo structure given
by `Error` (see module [`//stdlib/io`](`m:io`) for details). Warning
markers are regarded as source code forms, but have no defined lexical
form.

> #### Note {: .info }
>
> This is supported only for backwards compatibility with existing parsers
> and tools.

_See also: _`eof_marker/0`, `error_marker/1`, `is_form/1`,
`warning_marker_info/1`.

# `warning_marker_info`

```erlang
-spec warning_marker_info(syntaxTree()) -> term().
```

Returns the ErrorInfo structure of a `warning_marker` node.

_See also: _`warning_marker/1`.

# `zip_generator`

```erlang
-spec zip_generator([syntaxTree()]) -> syntaxTree().
```

Creates an abstract zip_generator.

The result represents `G1 && ... Gn`, where each `G` is a generator.

_See also: _`binary_comp/2`, `list_comp/2`, `map_comp/2`, `map_generator_body/1`,
`map_generator_pattern/1`.

# `zip_generator_body`

```erlang
-spec zip_generator_body(syntaxTree()) -> syntaxTree().
```

Returns the body subtree of a `zip_generator` node.

_See also: _`zip_generator/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
