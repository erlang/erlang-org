# `cerl`
[🔗](https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl.erl#L26)

Core Erlang abstract syntax trees.

> #### Note {: .info }
>
> The public interface of the Erlang compiler can be found in
> module `m:compile`.
>
> This module is an internal part of the compiler. Its API is not guaranteed
> to remain compatible between releases.

This module defines an abstract data type for representing Core Erlang source
code as syntax trees.

A recommended starting point for the first-time user is the documentation of the
function `type/1`.

> #### Note {: .info }
>
> This module deals with the composition and decomposition of _syntactic_ entities
> (as opposed to semantic ones); its purpose is to hide all direct references to
> the data structures used to represent these entities. With few exceptions, the
> functions in this module perform no semantic interpretation of their inputs, and
> in general, the user is assumed to pass type-correct arguments - if this is not
> done, the effects are not defined.
>
> Currently, the internal data structure used is the same as the record-based data
> structures used traditionally in the Beam compiler.
>
> The internal representations of abstract syntax trees are subject to change
> without notice, and should not be documented outside this module. Furthermore,
> we do not give any guarantees on how an abstract syntax tree may or may not be
> represented, _with the following exceptions_: no syntax tree is represented by a
> single atom, such as `none`, by a list constructor `[X | Y]`, or by the empty
> list `[]`. This can be relied on when writing functions that operate on syntax
> trees.

# `c_alias`
*not exported* 

```erlang
-type c_alias() :: #c_alias{}.
```

# `c_apply`
*not exported* 

```erlang
-type c_apply() :: #c_apply{}.
```

# `c_binary`

```erlang
-type c_binary() :: #c_binary{}.
```

# `c_bitstr`

```erlang
-type c_bitstr() :: #c_bitstr{}.
```

# `c_call`

```erlang
-type c_call() :: #c_call{}.
```

# `c_case`
*not exported* 

```erlang
-type c_case() :: #c_case{}.
```

# `c_catch`
*not exported* 

```erlang
-type c_catch() :: #c_catch{}.
```

# `c_clause`

```erlang
-type c_clause() :: #c_clause{}.
```

# `c_cons`

```erlang
-type c_cons() :: #c_cons{}.
```

# `c_fun`

```erlang
-type c_fun() :: #c_fun{}.
```

# `c_lct`
*not exported* 

```erlang
-type c_lct() :: c_literal() | c_cons() | c_tuple().
```

# `c_let`

```erlang
-type c_let() :: #c_let{}.
```

# `c_letrec`
*not exported* 

```erlang
-type c_letrec() :: #c_letrec{}.
```

# `c_literal`

```erlang
-type c_literal() :: #c_literal{}.
```

# `c_map`

```erlang
-type c_map() :: #c_map{}.
```

# `c_map_pair`

```erlang
-type c_map_pair() :: #c_map_pair{}.
```

# `c_module`

```erlang
-type c_module() :: #c_module{}.
```

# `c_opaque`
*not exported* 

```erlang
-type c_opaque() :: #c_opaque{}.
```

# `c_primop`
*not exported* 

```erlang
-type c_primop() :: #c_primop{}.
```

# `c_receive`
*not exported* 

```erlang
-type c_receive() :: #c_receive{}.
```

# `c_record`

```erlang
-type c_record() :: #c_record{}.
```

# `c_record_pair`

```erlang
-type c_record_pair() :: #c_record_pair{}.
```

# `c_seq`
*not exported* 

```erlang
-type c_seq() :: #c_seq{}.
```

# `c_try`
*not exported* 

```erlang
-type c_try() :: #c_try{}.
```

# `c_tuple`

```erlang
-type c_tuple() :: #c_tuple{}.
```

# `c_values`

```erlang
-type c_values() :: #c_values{}.
```

# `c_var`

```erlang
-type c_var() :: #c_var{}.
```

# `cerl`

```erlang
-type cerl() ::
          c_alias() |
          c_apply() |
          c_binary() |
          c_bitstr() |
          c_call() |
          c_case() |
          c_catch() |
          c_clause() |
          c_cons() |
          c_fun() |
          c_let() |
          c_letrec() |
          c_literal() |
          c_map() |
          c_map_pair() |
          c_record() |
          c_record_pair() |
          c_module() |
          c_opaque() |
          c_primop() |
          c_receive() |
          c_seq() |
          c_try() |
          c_tuple() |
          c_values() |
          c_var().
```

# `ctype`
*not exported* 

```erlang
-type ctype() ::
          alias | apply | binary | bitstr | call | 'case' | 'catch' | clause | cons | 'fun' | 'let' |
          letrec | literal | map | map_pair | module | opaque | primop | 'receive' | seq | record |
          record_pair | 'try' | tuple | values | var.
```

# `dtype`
*not exported* 

```erlang
-type dtype() :: cons | tuple | {atomic, value()}.
```

# `map_op`
*not exported* 

```erlang
-type map_op() :: #c_literal{val :: assoc} | #c_literal{val :: exact}.
```

# `record_id`
*not exported* 

```erlang
-type record_id() :: c_literal().
```

# `value`
*not exported* 

```erlang
-type value() :: integer() | float() | atom() | [].
```

# `var_name`

```erlang
-type var_name() :: integer() | atom() | {atom(), integer()}.
```

# `abstract`

```erlang
-spec abstract(Term :: term()) -> c_literal().
```

Creates a syntax tree corresponding to an Erlang term.

`Term` must be a literal term, that is, one that can be represented as
a source code literal. Thus, it may not contain a process identifier,
port, reference, binary or function value as a subterm.

Note: This is a constant time operation.

_See also: _`ann_abstract/2`, `concrete/1`, `is_literal/1`, `is_literal_term/1`.

# `add_ann`

```erlang
-spec add_ann(Annotations :: [term()], Node :: cerl()) -> cerl().
```

Appends `Annotations` to the list of user annotations of `Node`.

Note: this is equivalent to
[`set_ann(Node, Annotations ++ get_ann(Node))`](`set_ann/2`), but potentially
more efficient.

_See also: _`get_ann/1`, `set_ann/2`.

# `alias_pat`

```erlang
-spec alias_pat(Node :: c_alias()) -> cerl().
```

Returns the pattern subtree of an abstract pattern alias.

_See also: _`c_alias/2`.

# `alias_var`

```erlang
-spec alias_var(Node :: c_alias()) -> c_var().
```

Returns the variable subtree of an abstract pattern alias.

_See also: _`c_alias/2`.

# `ann_abstract`

```erlang
-spec ann_abstract(Annotations :: [term()], Term :: term()) -> c_literal().
```

_See also: _`abstract/1`.

# `ann_c_alias`

```erlang
-spec ann_c_alias(Annotations :: [term()], Variable :: c_var(), Pattern :: cerl()) -> c_alias().
```

_See also: _`c_alias/2`.

# `ann_c_apply`

```erlang
-spec ann_c_apply(Annotations :: [term()], Operator :: cerl(), Arguments :: [cerl()]) -> c_apply().
```

_See also: _`c_apply/2`.

# `ann_c_atom`

```erlang
-spec ann_c_atom(Annotations :: [term()], Name :: atom() | string()) -> c_literal().
```

_See also: _`c_atom/1`.

# `ann_c_binary`

```erlang
-spec ann_c_binary(Annotations :: [term()], Segments :: [cerl()]) -> c_binary().
```

_See also: _`c_binary/1`.

# `ann_c_bitstr`

```erlang
-spec ann_c_bitstr(Annotations :: [term()],
                   Value :: cerl(),
                   Size :: cerl(),
                   Type :: cerl(),
                   Flags :: cerl()) ->
                      c_bitstr().
```

Equivalent to
[ann_c_bitstr(As, Value, Size, abstract(1), Type, Flags)](`ann_c_bitstr/6`).

# `ann_c_bitstr`

```erlang
-spec ann_c_bitstr(Annotations :: [term()],
                   Value :: cerl(),
                   Size :: cerl(),
                   Unit :: cerl(),
                   Type :: cerl(),
                   Flags :: cerl()) ->
                      c_bitstr().
```

_See also: _`ann_c_bitstr/5`, `c_bitstr/5`.

# `ann_c_call`

```erlang
-spec ann_c_call(Annotations :: [term()], Module :: cerl(), Name :: cerl(), Arguments :: [cerl()]) ->
                    c_call().
```

_See also: _`c_call/3`.

# `ann_c_case`

```erlang
-spec ann_c_case(Annotations :: [term()], Argument :: cerl(), Clauses :: [cerl()]) -> c_case().
```

_See also: _`c_case/2`.

# `ann_c_catch`

```erlang
-spec ann_c_catch(Annotations :: [term()], Body :: cerl()) -> c_catch().
```

_See also: _`c_catch/1`.

# `ann_c_char`

```erlang
-spec ann_c_char(Annotations :: [term()], Value :: char()) -> c_literal().
```

_See also: _`c_char/1`.

# `ann_c_clause`

```erlang
-spec ann_c_clause(Annotations :: [term()], Patterns :: [cerl()], Body :: cerl()) -> c_clause().
```

Equivalent to
[ann_c_clause(As, Patterns, c_atom(true), Body)](`ann_c_clause/4`).

_See also: _`c_clause/3`.

# `ann_c_clause`

```erlang
-spec ann_c_clause(Annotations :: [term()], Patterns :: [cerl()], Guard :: cerl(), Body :: cerl()) ->
                      c_clause().
```

_See also: _`ann_c_clause/3`, `c_clause/3`.

# `ann_c_cons`

```erlang
-spec ann_c_cons(Annotations :: [term()], Head :: cerl(), Tail :: cerl()) -> c_literal() | c_cons().
```

_See also: _`c_cons/2`.

# `ann_c_cons_skel`

```erlang
-spec ann_c_cons_skel(Annotations :: [term()], Head :: cerl(), Tail :: cerl()) -> c_cons().
```

_See also: _`c_cons_skel/2`.

# `ann_c_float`

```erlang
-spec ann_c_float(Annotations :: [term()], Value :: float()) -> c_literal().
```

_See also: _`c_float/1`.

# `ann_c_fname`

```erlang
-spec ann_c_fname(Annotations :: [term()], Name :: atom(), Arity :: arity()) -> c_var().
```

Equivalent to [ann_c_var(As, \{Atom, Arity\})](`ann_c_var/2`).

_See also: _`c_fname/2`.

# `ann_c_fun`

```erlang
-spec ann_c_fun(Annotations :: [term()], Variables :: [cerl()], Body :: cerl()) -> c_fun().
```

_See also: _`c_fun/2`.

# `ann_c_int`

```erlang
-spec ann_c_int(Annotations :: [term()], Value :: integer()) -> c_literal().
```

_See also: _`c_int/1`.

# `ann_c_let`

```erlang
-spec ann_c_let(Annotations :: [term()], Variables :: [cerl()], Argument :: cerl(), Body :: cerl()) ->
                   c_let().
```

_See also: _`c_let/3`.

# `ann_c_letrec`

```erlang
-spec ann_c_letrec(Annotations :: [term()], Definitions :: [{cerl(), cerl()}], Body :: cerl()) ->
                      c_letrec().
```

_See also: _`c_letrec/2`.

# `ann_c_map`
*since OTP 17.0* 

```erlang
-spec ann_c_map(Annotations :: [term()], Pairs :: [c_map_pair()]) -> c_map() | c_literal().
```

_See also: _`c_map/1`.

# `ann_c_map`
*since OTP 17.0* 

```erlang
-spec ann_c_map(Annotations :: [term()], Argument :: c_map() | c_literal(), Pairs :: [c_map_pair()]) ->
                   c_map() | c_literal().
```

_See also: _`c_map/2`

# `ann_c_map_pair`
*since OTP 17.0* 

```erlang
-spec ann_c_map_pair(Annotations :: [term()], Operation :: cerl(), Key :: cerl(), Value :: cerl()) ->
                        c_map_pair().
```

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.

# `ann_c_map_pattern`
*since OTP 17.0* 

```erlang
-spec ann_c_map_pattern(Annotations :: [term()], Pairs :: [c_map_pair()]) -> c_map().
```

_See also: _`c_map_pattern/2`

# `ann_c_module`

```erlang
-spec ann_c_module(Annotations :: [term()],
                   Name :: cerl(),
                   Exports :: [cerl()],
                   Definitions :: [{cerl(), cerl()}]) ->
                      c_module().
```

_See also: _`ann_c_module/5`, `c_module/3`.

# `ann_c_module`

```erlang
-spec ann_c_module(Annotations :: [term()],
                   Name :: cerl(),
                   Exports :: [cerl()],
                   Attributes :: [{cerl(), cerl()}],
                   Definitions :: [{cerl(), cerl()}]) ->
                      c_module().
```

_See also: _`ann_c_module/4`, `c_module/4`.

# `ann_c_nil`

```erlang
-spec ann_c_nil(Annotations :: [term()]) -> c_literal().
```

_See also: _`c_nil/0`.

# `ann_c_primop`

```erlang
-spec ann_c_primop(Annotations :: [term()], Name :: cerl(), Arguments :: [cerl()]) -> c_primop().
```

_See also: _`c_primop/2`.

# `ann_c_receive`

```erlang
-spec ann_c_receive(Annotations :: [term()], Clauses :: [cerl()]) -> c_receive().
```

Equivalent to
[ann_c_receive(As, Clauses, c_atom(infinity), c_atom(true))](`ann_c_receive/4`).

_See also: _`c_atom/1`, `c_receive/3`.

# `ann_c_receive`

```erlang
-spec ann_c_receive(Annotations :: [term()], Clauses :: [cerl()], Timeout :: cerl(), Actions :: cerl()) ->
                       c_receive().
```

_See also: _`ann_c_receive/2`, `c_receive/3`.

# `ann_c_record`
*since OTP 29.0* 

```erlang
-spec ann_c_record(Annotations :: [term()],
                   Argument :: c_var() | c_record() | c_literal(),
                   Id :: term(),
                   Pairs :: [c_record_pair()]) ->
                      #c_record{}.
```

_See also: _`c_record/2`.

# `ann_c_record_pair`
*since OTP 29.0* 

```erlang
-spec ann_c_record_pair(Annotations :: [term()], Key :: cerl(), Value :: cerl()) -> c_record_pair().
```

_See also: _`c_record/2`.

# `ann_c_seq`

```erlang
-spec ann_c_seq(Annotations :: [term()], Argument :: cerl(), Body :: cerl()) -> c_seq().
```

_See also: _`c_seq/2`.

# `ann_c_string`

```erlang
-spec ann_c_string(Annotations :: [term()], Value :: string()) -> c_literal().
```

_See also: _`c_string/1`.

# `ann_c_try`

```erlang
-spec ann_c_try(Annotations :: [term()],
                Argument :: cerl(),
                Variables :: [cerl()],
                Body :: cerl(),
                ExceptionVars :: [cerl()],
                Handler :: cerl()) ->
                   c_try().
```

_See also: _`c_try/5`.

# `ann_c_tuple`

```erlang
-spec ann_c_tuple(Annotations :: [term()], Elements :: [cerl()]) -> c_tuple() | c_literal().
```

_See also: _`c_tuple/1`.

# `ann_c_tuple_skel`

```erlang
-spec ann_c_tuple_skel(Annotations :: [term()], Elements :: [cerl()]) -> c_tuple().
```

_See also: _`c_tuple_skel/1`.

# `ann_c_values`

```erlang
-spec ann_c_values(Annotations :: [term()], Values :: [cerl()]) -> c_values().
```

_See also: _`c_values/1`.

# `ann_c_var`

```erlang
-spec ann_c_var(Annotations :: [term()], Name :: var_name()) -> c_var().
```

_See also: _`c_var/1`.

# `ann_make_data`

```erlang
-spec ann_make_data(Annotations :: [term()], Type :: dtype(), Elementes :: [cerl()]) -> c_lct().
```

_See also: _`make_data/2`.

# `ann_make_data_skel`

```erlang
-spec ann_make_data_skel(Annotations :: [term()], Type :: dtype(), Elements :: [cerl()]) -> c_lct().
```

_See also: _`make_data_skel/2`.

# `ann_make_list`

```erlang
-spec ann_make_list(Annotations :: [term()], List :: [cerl()]) -> cerl().
```

# `ann_make_list`

```erlang
-spec ann_make_list(Annotations :: [term()], List :: [cerl()], Tail :: cerl() | none) -> cerl().
```

_See also: _`ann_make_list/2`, `make_list/2`.

# `ann_make_tree`

```erlang
-spec ann_make_tree(Annotations :: [term()], Type :: ctype(), Groups :: [[cerl()], ...]) -> cerl().
```

Creates a syntax tree with the given annotations, type and subtrees.

See [`make_tree/2`](`make_tree/2`) for details.

_See also: _`make_tree/2`.

# `apply_args`

```erlang
-spec apply_args(Node :: c_apply()) -> [cerl()].
```

Returns the list of argument subtrees of an abstract function application.

_See also: _`apply_arity/1`, `c_apply/2`.

# `apply_arity`

```erlang
-spec apply_arity(Node :: c_apply()) -> arity().
```

Returns the number of argument subtrees of an abstract function application.

Note: this is equivalent to [`length(apply_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`apply_args/1`, `c_apply/2`.

# `apply_op`

```erlang
-spec apply_op(Node :: c_apply()) -> cerl().
```

Returns the operator subtree of an abstract function application.

_See also: _`c_apply/2`.

# `atom_lit`

```erlang
-spec atom_lit(Node :: cerl()) -> nonempty_string().
```

Returns the literal string represented by an abstract atom. This always includes
surrounding single-quote characters.

Note that an abstract atom may have several literal representations, and that
the representation yielded by this function is not fixed; for example,
[`atom_lit(c_atom("a\012b"))`](`atom_lit/1`) could yield the string
`"\'a\\nb\'"`.

_See also: _`c_atom/1`.

# `atom_name`

```erlang
-spec atom_name(Node :: c_literal()) -> string().
```

Returns the printname of an abstract atom.

_See also: _`c_atom/1`.

# `atom_val`

```erlang
-spec atom_val(Node :: c_literal()) -> atom().
```

Returns the value represented by an abstract atom.

_See also: _`c_atom/1`.

# `binary_segments`

```erlang
-spec binary_segments(Node :: c_binary()) -> [cerl()].
```

Returns the list of segment subtrees of an abstract binary-template.

_See also: _`c_binary/1`, `c_bitstr/5`.

# `bitstr_bitsize`

```erlang
-spec bitstr_bitsize(Node :: c_bitstr()) -> all | any | utf | non_neg_integer().
```

Returns the total size in bits of an abstract bit-string template.

If the size field is an integer literal, the result is the product of
the size and unit values; if the size field is the atom literal `all`,
the atom `all` is returned.  If the size is not a literal, the atom
`any` is returned.  If the type of the bit-string segment is one of
`utf8`, `utf16` or `utf32`, the atom `utf` is returned.

_See also: _`c_bitstr/5`.

# `bitstr_flags`

```erlang
-spec bitstr_flags(Node :: c_bitstr()) -> cerl().
```

Returns the flags subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.

# `bitstr_size`

```erlang
-spec bitstr_size(Node :: c_bitstr()) -> cerl().
```

Returns the size subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.

# `bitstr_type`

```erlang
-spec bitstr_type(Node :: c_bitstr()) -> cerl().
```

Returns the type subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.

# `bitstr_unit`

```erlang
-spec bitstr_unit(Node :: c_bitstr()) -> cerl().
```

Returns the unit subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.

# `bitstr_val`

```erlang
-spec bitstr_val(Node :: c_bitstr()) -> cerl().
```

Returns the value subtree of an abstract bit-string template.

_See also: _`c_bitstr/5`.

# `c_alias`

```erlang
-spec c_alias(Variable :: c_var(), Pattern :: cerl()) -> c_alias().
```

Creates an abstract pattern alias.

The result represents "`Variable = Pattern`".

_See also: _`alias_pat/1`, `alias_var/1`, `ann_c_alias/3`, `c_clause/3`,
`is_c_alias/1`, `update_c_alias/3`.

# `c_apply`

```erlang
-spec c_apply(Operator :: cerl(), Arguments :: [cerl()]) -> c_apply().
```

Creates an abstract function application.

If `Arguments` is `[A1, ..., An]`, the result represents "`apply
Operator(A1, ..., An)`".

_See also: _`ann_c_apply/3`, `apply_args/1`, `apply_arity/1`, `apply_op/1`,
`c_call/3`, `c_primop/2`, `is_c_apply/1`, `update_c_apply/3`.

# `c_atom`

```erlang
-spec c_atom(Name :: atom() | string()) -> c_literal().
```

Creates an abstract atom literal.

The print name of the atom is the character sequence represented by
`Name`.

Note: passing a string as argument to this function causes a corresponding atom
to be created for the internal representation.

_See also: _`ann_c_atom/2`, `atom_lit/1`, `atom_name/1`, `atom_val/1`,
`is_c_atom/1`.

# `c_binary`

```erlang
-spec c_binary(Segments :: [cerl()]) -> c_binary().
```

Creates an abstract binary-template.

A binary object is in this context is a sequence of an arbitrary
number of bits. (The number of bits used to be evenly divisible by 8,
but after the introduction of bit strings in the Erlang language, the
choice was made to use the binary template for all bit strings.)  It
is specified by zero or more bit-string template _segments_ of
arbitrary lengths (in number of bits).

If `Segments` is `[S1, ..., Sn]`, the result represents "`#{S1, ...,
Sn}#`". All the `Si` must have type `bitstr`.

_See also: _`ann_c_binary/2`, `binary_segments/1`, `c_bitstr/5`,
`is_c_binary/1`, `update_c_binary/2`.

# `c_bitstr`

```erlang
-spec c_bitstr(Value :: cerl(), Type :: cerl(), Flags :: cerl()) -> c_bitstr().
```

Equivalent to
[c_bitstr(Value, abstract(all), abstract(1), Type, Flags)](`c_bitstr/5`).

# `c_bitstr`

```erlang
-spec c_bitstr(Value :: cerl(), Size :: cerl(), Type :: cerl(), Flags :: cerl()) -> c_bitstr().
```

# `c_bitstr`

```erlang
-spec c_bitstr(Value :: cerl(), Size :: cerl(), Unit :: cerl(), Type :: cerl(), Flags :: cerl()) ->
                  c_bitstr().
```

Creates an abstract bit-string template.

These can only occur as components of an abstract binary-template (see
`c_binary/1`). The result represents "`#<Value>(Size, Unit, Type,
Flags)`", where `Unit` must represent a positive integer constant,
`Type` must represent a constant atom (one of `'integer'`, `'float'`,
`'binary'`, `'utf8'`, `'utf16'` or `'utf32'`), and `Flags` must
represent a constant list `"[F1, ..., Fn]"` where all the `Fi` are
atoms.

_See also: _`ann_c_bitstr/6`, `bitstr_flags/1`, `bitstr_size/1`,
`bitstr_type/1`, `bitstr_unit/1`, `bitstr_val/1`, `c_binary/1`, `is_c_bitstr/1`,
`update_c_bitstr/6`.

# `c_call`

```erlang
-spec c_call(Module :: cerl(), Name :: cerl(), Arguments :: [cerl()]) -> c_call().
```

Creates an abstract inter-module call.

If `Arguments` is `[A1, ..., An]`, the result represents "`call
Module:Name(A1, ..., An)`".

_See also: _`ann_c_call/4`, `c_apply/2`, `c_primop/2`, `call_args/1`,
`call_arity/1`, `call_module/1`, `call_name/1`, `is_c_call/1`,
`update_c_call/4`.

# `c_case`

```erlang
-spec c_case(Argument :: cerl(), Clauses :: [cerl()]) -> c_case().
```

Creates an abstract case-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`case Argument
of C1 ... Cn end`". `Clauses` must not be empty.

_See also: _`ann_c_case/3`, `c_clause/3`, `case_arg/1`, `case_arity/1`,
`case_clauses/1`, `is_c_case/1`, `update_c_case/3`.

# `c_catch`

```erlang
-spec c_catch(Body :: cerl()) -> c_catch().
```

Creates an abstract catch-expression.

The result represents "`catch Body`".

Note: catch-expressions can be rewritten as try-expressions, and will eventually
be removed from Core Erlang.

_See also: _`ann_c_catch/2`, `c_try/5`, `catch_body/1`, `is_c_catch/1`,
`update_c_catch/2`.

# `c_char`

```erlang
-spec c_char(Value :: non_neg_integer()) -> c_literal().
```

Creates an abstract character literal.

If the local implementation of Erlang defines `t:char/0` as a subset
of `t:integer/0`, this function is equivalent to
[`c_int/1`](`c_int/1`). Otherwise, if the given value is an integer,
it will be converted to the character with the corresponding code. The
lexical representation of a character is "`$Char`", where `Char` is a
single printing character or an escape sequence.

_See also: _`ann_c_char/2`, `c_int/1`, `c_string/1`, `char_lit/1`, `char_val/1`,
`is_c_char/1`, `is_print_char/1`.

# `c_clause`

```erlang
-spec c_clause(Patterns :: [cerl()], Body :: cerl()) -> c_clause().
```

Equivalent to [c_clause(Patterns, c_atom(true), Body)](`c_clause/3`).

_See also: _`c_atom/1`.

# `c_clause`

```erlang
-spec c_clause(Patterns :: [cerl()], Guard :: cerl(), Body :: cerl()) -> c_clause().
```

Creates an an abstract clause.

If `Patterns` is `[P1, ..., Pn]`, the result represents "`<P1, ...,
Pn> when Guard -> Body`".

_See also: _`ann_c_clause/4`, `c_case/2`, `c_clause/2`, `c_receive/3`,
`clause_arity/1`, `clause_body/1`, `clause_guard/1`, `clause_pats/1`,
`clause_vars/1`, `is_c_clause/1`, `update_c_clause/4`.

# `c_cons`

```erlang
-spec c_cons(Head :: cerl(), Tail :: cerl()) -> c_literal() | c_cons().
```

Creates an abstract list constructor.

The result represents "`[Head | Tail]`". Note that if both `Head` and
`Tail` have type `literal`, then the result will also have type
`literal`, and annotations on `Head` and `Tail` are lost.

Recall that in Erlang, the tail element of a list constructor is not necessarily
a list.

_See also: _`ann_c_cons/3`, `c_cons_skel/2`, `c_nil/0`, `cons_hd/1`,
`cons_tl/1`, `is_c_cons/1`, `is_c_list/1`, `list_elements/1`, `list_length/1`,
`make_list/2`, `update_c_cons/3`.

# `c_cons_skel`

```erlang
-spec c_cons_skel(Head :: cerl(), Tail :: cerl()) -> c_cons().
```

Creates an abstract list constructor skeleton.

Does not fold constant literals, that is, the result always has type
`cons`, representing "`[Head | Tail]`".

This function is occasionally useful when it is necessary to have annotations on
the subnodes of a list constructor node, even when the subnodes are constant
literals. However, note that [`is_literal/1`](`is_literal/1`) will yield `false`
and [`concrete/1`](`concrete/1`) will fail if passed the result from this
function.

[`fold_literal/1`](`fold_literal/1`) can be used to revert a node to the
normal-form representation.

_See also: _`ann_c_cons_skel/3`, `c_cons/2`, `c_nil/0`, `concrete/1`,
`fold_literal/1`, `is_c_cons/1`, `is_c_list/1`, `is_literal/1`,
`update_c_cons_skel/3`.

# `c_float`

```erlang
-spec c_float(Value :: float()) -> c_literal().
```

Creates an abstract floating-point literal.

The lexical representation is the decimal floating-point numeral of
`Value`.

_See also: _`ann_c_float/2`, `float_lit/1`, `float_val/1`, `is_c_float/1`.

# `c_fname`

```erlang
-spec c_fname(Name :: atom(), Arity :: arity()) -> c_var().
```

Equivalent to [c_var(\{Name, Arity\})](`c_var/1`).

_See also: _`ann_c_fname/3`, `fname_arity/1`, `fname_id/1`, `is_c_fname/1`,
`update_c_fname/3`.

# `c_fun`

```erlang
-spec c_fun(Variables :: [cerl()], Body :: cerl()) -> c_fun().
```

Creates an abstract fun-expression.

If `Variables` is `[V1, ..., Vn]`, the result represents "`fun (V1,
..., Vn) -> Body`". All the `Vi` must have type `var`.

_See also: _`ann_c_fun/3`, `fun_arity/1`, `fun_body/1`, `fun_vars/1`,
`is_c_fun/1`, `update_c_fun/3`.

# `c_int`

```erlang
-spec c_int(Value :: integer()) -> c_literal().
```

Creates an abstract integer literal.

The lexical representation is the canonical decimal numeral of `Value`.

_See also: _`ann_c_int/2`, `c_char/1`, `int_lit/1`, `int_val/1`, `is_c_int/1`.

# `c_let`

```erlang
-spec c_let(Variables :: [cerl()], Argument :: cerl(), Body :: cerl()) -> c_let().
```

Creates an abstract let-expression.

If `Variables` is `[V1, ..., Vn]`, the result represents "`let <V1,
..., Vn> = Argument in Body`". All the `Vi` must have type `var`.

_See also: _`ann_c_let/4`, `is_c_let/1`, `let_arg/1`, `let_arity/1`,
`let_body/1`, `let_vars/1`, `update_c_let/4`.

# `c_letrec`

```erlang
-spec c_letrec(Definitions :: [{cerl(), cerl()}], Body :: cerl()) -> c_letrec().
```

Creates an abstract letrec-expression.

If `Definitions` is `[{V1, F1}, ..., {Vn, Fn}]`, the result represents
"`letrec V1 = F1 ... Vn = Fn in Body`". All the `Vi` must have type
`var` and represent function names. All the `Fi` must have type
`'fun'`.

_See also: _`ann_c_letrec/3`, `is_c_letrec/1`, `letrec_body/1`, `letrec_defs/1`,
`letrec_vars/1`, `update_c_letrec/3`.

# `c_map`
*since OTP 17.0* 

```erlang
-spec c_map(Pairs :: [c_map_pair()]) -> c_map().
```

Creates an abstract map constructor.

If `Pairs` is `[E1, ..., EN]`, the result represents "`~{E1, ...,
EN}~`" (creating a new map). Note that if all pairs in `Pairs` have
type `literal` for both the key and the value, or if `Pairs` is empty,
then the result will also have type `literal` and annotations on nodes
in `Pairs` are lost.

All `Ei` must be abstract pairs constructed by `c_map_pair/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair/2`, `c_map_pair_exact/2`.

# `c_map`
*since OTP 27.0* 

```erlang
-spec c_map(Argument :: cerl(), Pairs :: [c_map_pair()]) -> c_map().
```

Creates an abstract map update expression.

If `Pairs` is `[E1, ..., EN]`, the result represents "`~{E1, ..., EN |
Argument}~`" (updating an existing map). Note that if `Argument` is a
literal and all pairs in `Pairs` have type `literal` for both the key
and the value, or if `Pairs` is empty, then the result will also have
type `literal` and annotations on nodes in `Pairs` are lost.

All `Ei` must be abstract pairs constructed by either `c_map_pair/2` or
`c_map_pair_exact/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair/2`, `c_map_pair_exact/2`.

# `c_map_pair`
*since OTP 17.0* 

```erlang
-spec c_map_pair(Key :: cerl(), Value :: cerl()) -> c_map_pair().
```

Creates an abstract map pair using the `assoc` operator.

These can only occur as components of an abstract map creation
expression or an abstract update expression (see `c_map/1` and
`c_map/2`).

The result represents "`Key => Value`".

_See also: _`map_pair_key/1`, `map_pair_op/1`, `map_pair_val/1`.

# `c_map_pair_exact`
*since OTP 17.0* 

```erlang
-spec c_map_pair_exact(Key :: cerl(), Value :: cerl()) -> c_map_pair().
```

Creates an abstract map pair using the `exact` operator.

These can only occur as components of an abstract map update
expression or an abstract map pattern (see `c_map/1` and
`c_map_pattern/1`).

The result represents "`Key := Value`".

_See also: _`map_pair_key/1`, `map_pair_op/1`, `map_pair_val/1`.

# `c_map_pattern`
*since OTP 17.0* 

```erlang
-spec c_map_pattern(Pairs :: [c_map_pair()]) -> c_map().
```

Creates an abstract map pattern.

If `Pairs` is `[E1, ..., EN]`, the result represents
"`~{E1, ..., EN}~`".

All `Ei` must be abstract pairs constructed by `c_map_pair_exact/2`.

_See also: _`ann_c_map/2`, `is_c_map/1`, `is_c_map_empty/1`, `is_c_map_pattern/1`,
`map_es/1`, `c_map_pair_exact/2`.

# `c_module`

```erlang
-spec c_module(Name :: cerl(), Exports :: [cerl()], Definitions :: [{cerl(), cerl()}]) -> c_module().
```

# `c_module`

```erlang
-spec c_module(Name :: cerl(),
               Exports :: [cerl()],
               Attributes :: [{cerl(), cerl()}],
               Definitions :: [{cerl(), cerl()}]) ->
                  c_module().
```

Creates an abstract module definition.

The result represents

```text
    module Name [E1, ..., Ek]
      attributes [K1 = T1, ...,
                  Km = Tm]
      V1 = F1
      ...
      Vn = Fn
    end
```

if `Exports` = `[E1, ..., Ek]`, `Attributes` = `[{K1, T1}, ..., {Km, Tm}]`, and
`Definitions` = `[{V1, F1}, ..., {Vn, Fn}]`.

`Name` and all the `Ki` must be atom literals, and all the `Ti` must be constant
literals. All the `Vi` and `Ei` must have type `var` and represent function
names. All the `Fi` must have type `'fun'`.

_See also: _`ann_c_module/4`, `ann_c_module/5`, `c_atom/1`, `c_fun/2`,
`c_module/3`, `c_var/1`, `is_literal/1`, `module_attrs/1`, `module_defs/1`,
`module_exports/1`, `module_name/1`, `module_vars/1`, `update_c_module/5`.

# `c_nil`

```erlang
-spec c_nil() -> c_literal().
```

Creates an abstract empty list.

The result represents "`[]`". The empty list is traditionally called
"nil".

_See also: _`ann_c_nil/1`, `c_cons/2`, `is_c_list/1`.

# `c_primop`

```erlang
-spec c_primop(Name :: cerl(), Arguments :: [cerl()]) -> c_primop().
```

Creates an abstract primitive operation call.

If `Arguments` is `[A1, ..., An]`, the result represents "`primop
Name(A1, ..., An)`". `Name` must be an atom literal.

_See also: _`ann_c_primop/3`, `c_apply/2`, `c_call/3`, `is_c_primop/1`,
`primop_args/1`, `primop_arity/1`, `primop_name/1`, `update_c_primop/3`.

# `c_receive`

```erlang
-spec c_receive(Clauses :: [cerl()]) -> c_receive().
```

Equivalent to
[c_receive(Clauses, c_atom(infinity), c_atom(true))](`c_receive/3`).

_See also: _`c_atom/1`.

# `c_receive`

```erlang
-spec c_receive(Clauses :: [cerl()], Timeout :: cerl(), Action :: cerl()) -> c_receive().
```

Creates an abstract receive-expression.

If `Clauses` is `[C1, ..., Cn]`, the result represents "`receive C1
... Cn after Timeout -> Action end`".

_See also: _`ann_c_receive/4`, `c_receive/1`, `is_c_receive/1`,
`receive_action/1`, `receive_clauses/1`, `receive_timeout/1`,
`update_c_receive/4`.

# `c_record`
*since OTP 29.0* 

```erlang
-spec c_record(Argument :: record_id(), Pairs :: [c_record_pair()]) -> #c_record{}.
```

Creates an abstract record constructor.

_See also: _`ann_c_record/3`, `is_c_record/1`, `record_id/1`, `record_es/1`,
`c_record_pair/2`, `update_c_record/4`.

# `c_record_pair`
*since OTP 29.0* 

```erlang
-spec c_record_pair(Key :: c_literal(), Value :: cerl()) -> c_record_pair().
```

Creates an abstract record pair.

These can only occur as components of an abstract record creation
expression or an abstract update expression (see `c_record/2`).

The result represents "`Key = Value`".

_See also: _`ann_c_record_pair/3`, `record_pair_key/1`, `record_pair_val/1`.

# `c_seq`

```erlang
-spec c_seq(Argument :: cerl(), Body :: cerl()) -> c_seq().
```

Creates an abstract sequencing expression.

The result represents "`do Argument Body`".

_See also: _`ann_c_seq/3`, `is_c_seq/1`, `seq_arg/1`, `seq_body/1`,
`update_c_seq/3`.

# `c_string`

```erlang
-spec c_string(Value :: string()) -> c_literal().
```

Creates an abstract string literal.

Equivalent to creating an abstract list of the corresponding character
literals (cf. [`is_c_string/1`](`is_c_string/1`)), but is typically
more efficient. The lexical representation of a string is "`"Chars"`",
where `Chars` is a sequence of printing characters or spaces.

_See also: _`ann_c_string/2`, `c_char/1`, `is_c_string/1`, `is_print_string/1`,
`string_lit/1`, `string_val/1`.

# `c_try`

```erlang
-spec c_try(Argument :: cerl(),
            Variables :: [cerl()],
            Body :: cerl(),
            ExceptionVars :: [cerl()],
            Handler :: cerl()) ->
               c_try().
```

Creates an abstract try-expression.

If `Variables` is `[V1, ..., Vn]` and `ExceptionVars` is `[X1, ...,
Xm]`, the result represents "`try Argument of <V1, ..., Vn> -> Body
catch <X1, ..., Xm> -> Handler`".  All the `Vi` and `Xi` must have
type `var`.

_See also: _`ann_c_try/6`, `c_catch/1`, `is_c_try/1`, `try_arg/1`, `try_body/1`,
`try_vars/1`, `update_c_try/6`.

# `c_tuple`

```erlang
-spec c_tuple(Elements :: [cerl()]) -> c_tuple() | c_literal().
```

Creates an abstract tuple.

If `Elements` is `[E1, ..., En]`, the result represents "`{E1, ...,
En}`". Note that if all nodes in `Elements` have type `literal`, or if
`Elements` is empty, then the result will also have type `literal` and
annotations on nodes in `Elements` are lost.

Recall that Erlang has distinct 1-tuples, that is, `{X}` is always
distinct from `X` itself.

_See also: _`ann_c_tuple/2`, `c_tuple_skel/1`, `is_c_tuple/1`, `tuple_arity/1`,
`tuple_es/1`, `update_c_tuple/2`.

# `c_tuple_skel`

```erlang
-spec c_tuple_skel(Elements :: [cerl()]) -> c_tuple().
```

Creates an abstract tuple skeleton.

Does not fold constant literals, that is, the result always has type
`tuple`, representing "`{E1, ..., En}`", if `Elements` is `[E1, ...,
En]`.

This function is occasionally useful when it is necessary to have annotations on
the subnodes of a tuple node, even when all the subnodes are constant literals.
However, note that [`is_literal/1`](`is_literal/1`) will yield `false` and
[`concrete/1`](`concrete/1`) will fail if passed the result from this function.

[`fold_literal/1`](`fold_literal/1`) can be used to revert a node to the
normal-form representation.

_See also: _`ann_c_tuple_skel/2`, `c_tuple/1`, `concrete/1`, `fold_literal/1`,
`is_c_tuple/1`, `is_literal/1`, `tuple_es/1`, `update_c_tuple_skel/2`.

# `c_values`

```erlang
-spec c_values(Elements :: [cerl()]) -> c_values().
```

Creates an abstract value list.

If `Elements` is `[E1, ..., En]`, the result represents "`<E1, ...,
En>`".

_See also: _`ann_c_values/2`, `is_c_values/1`, `update_c_values/2`,
`values_arity/1`, `values_es/1`.

# `c_var`

```erlang
-spec c_var(Name :: var_name()) -> c_var().
```

Creates an abstract variable.

A variable is identified by its name, given by the `Name` parameter.

If a name is given by a single atom, it should either be a "simple" atom which
does not need to be single-quoted in Erlang, or otherwise its print name should
correspond to a proper Erlang variable, that is, begin with an uppercase character
or an underscore. Names of the form `{A, N}` represent function name variables
"`A/N`"; these are special variables which may be bound only in the function
definitions of a module or a `letrec`. They may not be bound in `let`
expressions and cannot occur in clause patterns. The atom `A` in a function name
may be any atom; the integer `N` must be nonnegative. The functions
[`c_fname/2`](`c_fname/2`) etc. are utilities for handling function name
variables.

When printing variable names, they must have the form of proper Core Erlang
variables and function names. E.g., a name represented by an integer such as
`42` could be formatted as "`_42`", an atom `'Xxx'` simply as "`Xxx`", and an
atom `foo` as "`_foo`". However, one must assure that any two valid distinct
names are never mapped to the same strings. Tuples such as `{foo, 2}`
representing function names can simply by formatted as "`'foo'/2`", with no risk
of conflicts.

_See also: _`ann_c_var/2`, `c_fname/2`, `c_letrec/2`, `c_module/4`,
`is_c_var/1`, `update_c_var/2`, `var_name/1`.

# `call_args`

```erlang
-spec call_args(Node :: c_call()) -> [cerl()].
```

Returns the list of argument subtrees of an abstract inter-module call.

_See also: _`c_call/3`, `call_arity/1`.

# `call_arity`

```erlang
-spec call_arity(Node :: c_call()) -> arity().
```

Returns the number of argument subtrees of an abstract inter-module call.

Note: this is equivalent to [`length(call_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_call/3`, `call_args/1`.

# `call_module`

```erlang
-spec call_module(Node :: c_call()) -> cerl().
```

Returns the module subtree of an abstract inter-module call.

_See also: _`c_call/3`.

# `call_name`

```erlang
-spec call_name(Node :: c_call()) -> cerl().
```

Returns the name subtree of an abstract inter-module call.

_See also: _`c_call/3`.

# `case_arg`

```erlang
-spec case_arg(Node :: c_case()) -> cerl().
```

Returns the argument subtree of an abstract case-expression.

_See also: _`c_case/2`.

# `case_arity`

```erlang
-spec case_arity(Node :: c_case()) -> non_neg_integer().
```

Equivalent to [`clause_arity(hd(case_clauses(Node)))`](`clause_arity/1`), but
potentially more efficient.

_See also: _`c_case/2`, `case_clauses/1`, `clause_arity/1`.

# `case_clauses`

```erlang
-spec case_clauses(Node :: c_case()) -> [cerl()].
```

Returns the list of clause subtrees of an abstract case-expression.

_See also: _`c_case/2`, `case_arity/1`.

# `catch_body`

```erlang
-spec catch_body(Node :: c_catch()) -> cerl().
```

Returns the body subtree of an abstract catch-expression.

_See also: _`c_catch/1`.

# `char_lit`

```erlang
-spec char_lit(Node :: c_literal()) -> nonempty_string().
```

Returns the literal string represented by an abstract character. This includes a
leading `$` character.

Currently, all characters that are not in the set of ISO 8859-1
(Latin-1) "printing" characters will be escaped.

_See also: _`c_char/1`.

# `char_val`

```erlang
-spec char_val(Node :: c_literal()) -> char().
```

Returns the value represented by an abstract character literal.

_See also: _`c_char/1`.

# `clause_arity`

```erlang
-spec clause_arity(Node :: c_clause()) -> non_neg_integer().
```

Returns the number of pattern subtrees of an abstract clause.

Note: this is equivalent to [`length(clause_pats(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_clause/3`, `clause_pats/1`.

# `clause_body`

```erlang
-spec clause_body(Node :: c_clause()) -> cerl().
```

Returns the body subtree of an abstract clause.

_See also: _`c_clause/3`.

# `clause_guard`

```erlang
-spec clause_guard(Node :: c_clause()) -> cerl().
```

Returns the guard subtree of an abstract clause.

_See also: _`c_clause/3`.

# `clause_pats`

```erlang
-spec clause_pats(Node :: c_clause()) -> [cerl()].
```

Returns the list of pattern subtrees of an abstract clause.

_See also: _`c_clause/3`, `clause_arity/1`.

# `clause_vars`

```erlang
-spec clause_vars(Node :: c_clause()) -> [cerl()].
```

Returns the list of all abstract variables in the patterns of an abstract
clause.

The order of listing is not defined.

_See also: _`c_clause/3`, `pat_list_vars/1`.

# `concrete`

```erlang
-spec concrete(Node :: c_literal()) -> term().
```

Returns the Erlang term represented by a syntax tree.

An exception is thrown if `Node` does not represent a literal term.

Note: This is a constant time operation.

_See also: _`abstract/1`, `is_literal/1`.

# `cons_hd`

```erlang
-spec cons_hd(Node :: c_cons() | c_literal()) -> cerl().
```

Returns the head subtree of an abstract list constructor.

_See also: _`c_cons/2`.

# `cons_tl`

```erlang
-spec cons_tl(Node :: c_cons() | c_literal()) -> cerl().
```

Returns the tail subtree of an abstract list constructor.

Recall that the tail does not necessarily represent a proper list.

_See also: _`c_cons/2`.

# `copy_ann`

```erlang
-spec copy_ann(Source :: cerl(), Target :: cerl()) -> cerl().
```

Copies the list of user annotations from `Source` to `Target`.

Note: this is equivalent to [`set_ann(Target, get_ann(Source))`](`set_ann/2`),
but potentially more efficient.

_See also: _`get_ann/1`, `set_ann/2`.

# `data_arity`

```erlang
-spec data_arity(Node :: c_lct()) -> non_neg_integer().
```

Returns the number of subtrees of a data constructor node.

This is equivalent to [`length(data_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`data_es/1`, `is_data/1`.

# `data_es`

```erlang
-spec data_es(Node :: c_lct()) -> [cerl()].
```

Returns the list of subtrees of a data constructor node.

If the arity of the constructor is zero, the result is the empty list.

Note: if [`data_type(Node)`](`data_type/1`) is `cons`, the number of subtrees is
exactly two. If [`data_type(Node)`](`data_type/1`) is `{atomic, Value}`, the
number of subtrees is zero.

_See also: _`data_arity/1`, `data_type/1`, `is_data/1`, `make_data/2`.

# `data_type`

```erlang
-spec data_type(Node :: c_lct()) -> dtype().
```

Returns a type descriptor for a data constructor node. (Cf.
[`is_data/1`](`is_data/1`).)

This is mainly useful for comparing types and for constructing new
nodes of the same type (cf. [`make_data/2`](`make_data/2`)). If `Node`
represents an integer, floating-point number, atom or empty list, the
result is `{atomic, Value}`, where `Value` is the value of
[`concrete(Node)`](`concrete/1`), otherwise the result is either
`cons` or `tuple`.

Type descriptors can be compared for equality or order (in the Erlang term
order), but remember that floating-point values should in general never be
tested for equality.

_See also: _`concrete/1`, `is_data/1`, `make_data/2`, `type/1`.

# `float_lit`

```erlang
-spec float_lit(Node :: c_literal()) -> string().
```

Returns the numeral string represented by a floating-point literal node.

_See also: _`c_float/1`.

# `float_val`

```erlang
-spec float_val(Node :: c_literal()) -> float().
```

Returns the value represented by a floating-point literal node.

_See also: _`c_float/1`.

# `fname_arity`

```erlang
-spec fname_arity(Node :: c_var()) -> arity().
```

Returns the arity part of an abstract function name variable.

_See also: _`c_fname/2`, `fname_id/1`.

# `fname_id`

```erlang
-spec fname_id(Node :: c_var()) -> atom().
```

Returns the identifier part of an abstract function name variable.

_See also: _`c_fname/2`, `fname_arity/1`.

# `fold_literal`

```erlang
-spec fold_literal(Node :: cerl()) -> cerl().
```

Ensures that literals have a compact representation.

This is occasionally useful if
[`c_cons_skel/2`](`c_cons_skel/2`), [`c_tuple_skel/1`](`c_tuple_skel/1`) or
[`unfold_literal/1`](`unfold_literal/1`) were used in the construction of
`Node`, and you want to revert to the normal "folded" representation of
literals. If `Node` represents a tuple or list constructor, its elements are
rewritten recursively, and the node is reconstructed using
[`c_cons/2`](`c_cons/2`) or [`c_tuple/1`](`c_tuple/1`), respectively; otherwise,
`Node` is not changed.

_See also: _`c_cons/2`, `c_cons_skel/2`, `c_tuple/1`, `c_tuple_skel/1`,
`is_literal/1`, `unfold_literal/1`.

# `from_records`

```erlang
-spec from_records(Node :: cerl()) -> cerl().
```

Translates an explicit record representation to a corresponding abstract syntax
tree.

The records are defined in the file "`core_parse.hrl`".

_See also: _`to_records/1`, `type/1`.

# `fun_arity`

```erlang
-spec fun_arity(Node :: c_fun()) -> arity().
```

Returns the number of parameter subtrees of an abstract fun-expression.

Note: this is equivalent to [`length(fun_vars(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_fun/2`, `fun_vars/1`.

# `fun_body`

```erlang
-spec fun_body(Node :: c_fun()) -> cerl().
```

Returns the body subtree of an abstract fun-expression.

_See also: _`c_fun/2`.

# `fun_vars`

```erlang
-spec fun_vars(Node :: c_fun()) -> [cerl()].
```

Returns the list of parameter subtrees of an abstract fun-expression.

_See also: _`c_fun/2`, `fun_arity/1`.

# `get_ann`

```erlang
-spec get_ann(Node :: cerl()) -> [term()].
```

Returns the list of user annotations associated with a syntax tree node.

For a newly created node, this is the empty list. The annotations may
be any terms.

_See also: _`set_ann/2`.

# `int_lit`

```erlang
-spec int_lit(Node :: c_literal()) -> string().
```

Returns the numeral string represented by an integer literal node.

_See also: _`c_int/1`.

# `int_val`

```erlang
-spec int_val(Node :: c_literal()) -> integer().
```

Returns the value represented by an integer literal node.

_See also: _`c_int/1`.

# `is_c_alias`

```erlang
-spec is_c_alias(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract pattern alias, otherwise `false`.

_See also: _`c_alias/2`.

# `is_c_apply`

```erlang
-spec is_c_apply(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract function application, otherwise `false`.

_See also: _`c_apply/2`.

# `is_c_atom`

```erlang
-spec is_c_atom(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents an atom literal, otherwise `false`.

_See also: _`c_atom/1`.

# `is_c_binary`

```erlang
-spec is_c_binary(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract binary-template, otherwise `false`.

_See also: _`c_binary/1`.

# `is_c_bitstr`

```erlang
-spec is_c_bitstr(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract bit-string template, otherwise `false`.

_See also: _`c_bitstr/5`.

# `is_c_call`

```erlang
-spec is_c_call(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract inter-module call expression, otherwise
`false`.

_See also: _`c_call/3`.

# `is_c_case`

```erlang
-spec is_c_case(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract case-expression, otherwise `false`.

_See also: _`c_case/2`.

# `is_c_catch`

```erlang
-spec is_c_catch(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract catch-expression, otherwise `false`.

_See also: _`c_catch/1`.

# `is_c_char`

```erlang
-spec is_c_char(Node :: c_literal()) -> boolean().
```

Returns `true` if `Node` may represent a character literal, otherwise `false`.

If the local implementation of Erlang defines `t:char/0` as a subset of
`t:integer/0`, then `is_c_int(Node)` will also yield `true`.

_See also: _`c_char/1`, `is_print_char/1`.

# `is_c_clause`

```erlang
-spec is_c_clause(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract clause, otherwise `false`.

_See also: _`c_clause/3`.

# `is_c_cons`

```erlang
-spec is_c_cons(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract list constructor, otherwise `false`.

# `is_c_float`

```erlang
-spec is_c_float(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents a floating-point literal, otherwise `false`.

_See also: _`c_float/1`.

# `is_c_fname`

```erlang
-spec is_c_fname(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract function name variable, otherwise
`false`.

_See also: _`c_fname/2`, `c_var/1`, `var_name/1`.

# `is_c_fun`

```erlang
-spec is_c_fun(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract fun-expression, otherwise `false`.

_See also: _`c_fun/2`.

# `is_c_int`

```erlang
-spec is_c_int(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents an integer literal, otherwise `false`.

_See also: _`c_int/1`.

# `is_c_let`

```erlang
-spec is_c_let(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract let-expression, otherwise `false`.

_See also: _`c_let/3`.

# `is_c_letrec`

```erlang
-spec is_c_letrec(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract letrec-expression, otherwise `false`.

_See also: _`c_letrec/2`.

# `is_c_list`

```erlang
-spec is_c_list(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents a proper list, otherwise `false`.

A proper list is either the empty list `[]`, or a cons cell `[Head |
Tail]`, where recursively `Tail` is a proper list.

Note: Because `Node` is a syntax tree, the actual run-time values
corresponding to its subtrees may often be partially or completely
unknown. Thus, if `Node` represents for example "`[... | Ns]`" (where
`Ns` is a variable), then the function will return `false`, because it
is not known whether `Ns` will be bound to a list at run-time. If
`Node` instead represents for example "`[1, 2, 3]`" or "`[A | []]`",
then the function will return `true`.

_See also: _`c_cons/2`, `c_nil/0`, `list_elements/1`, `list_length/1`.

# `is_c_map`
*since OTP 17.0* 

```erlang
-spec is_c_map(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is any kind of abstract map (for constructing,
updating or matching), otherwise `false`.

_See also: _`ann_c_map/3`, `c_map/1`, `c_map_pattern/1`.

# `is_c_map_empty`
*since OTP 17.0* 

```erlang
-spec is_c_map_empty(Node :: c_map() | c_literal()) -> boolean().
```

Returns `true` if `Node` represents an empty abstract map, otherwise `false`.

_See also: _`c_map/1`, `c_map_pattern/1`.

# `is_c_map_pattern`
*since OTP 17.0* 

```erlang
-spec is_c_map_pattern(Node :: c_map()) -> boolean().
```

Returns `true` if `Node` is an abstract map pattern, otherwise `false`.

_See also: _`c_map/1`, `c_map_pattern/1`.

# `is_c_module`

```erlang
-spec is_c_module(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract module definition, otherwise `false`.

_See also: _`type/1`.

# `is_c_nil`

```erlang
-spec is_c_nil(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract empty list, otherwise `false`.

# `is_c_primop`

```erlang
-spec is_c_primop(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract primitive operation call, otherwise
`false`.

_See also: _`c_primop/2`.

# `is_c_receive`

```erlang
-spec is_c_receive(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract receive-expression, otherwise `false`.

_See also: _`c_receive/3`.

# `is_c_record`
*since OTP 29.0* 

```erlang
-spec is_c_record(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract record, otherwise `false`.

_See also: _`c_record/2`.

# `is_c_seq`

```erlang
-spec is_c_seq(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract sequencing expression, otherwise
`false`.

_See also: _`c_seq/2`.

# `is_c_string`

```erlang
-spec is_c_string(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` may represent a string literal, otherwise `false`.

Strings are defined as lists of characters; see [`is_c_char/1`](`is_c_char/1`)
for details.

_See also: _`c_string/1`, `is_c_char/1`, `is_print_string/1`.

# `is_c_try`

```erlang
-spec is_c_try(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract try-expression, otherwise `false`.

_See also: _`c_try/5`.

# `is_c_tuple`

```erlang
-spec is_c_tuple(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract tuple, otherwise `false`.

_See also: _`c_tuple/1`.

# `is_c_values`

```erlang
-spec is_c_values(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract value list, otherwise `false`.

_See also: _`c_values/1`.

# `is_c_var`

```erlang
-spec is_c_var(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is an abstract variable, otherwise `false`.

_See also: _`c_var/1`.

# `is_data`

```erlang
-spec is_data(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents a data constructor, otherwise `false`.

Data constructors are cons cells, tuples, and atomic literals.

_See also: _`data_arity/1`, `data_es/1`, `data_type/1`.

# `is_leaf`

```erlang
-spec is_leaf(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` is a leaf node, otherwise `false`.

The current leaf node types are `literal` and `var`.

Note: all literals (cf. [`is_literal/1`](`is_literal/1`)) are leaf nodes, even
if they represent structured (constant) values such as `{foo, [bar, baz]}`. Also
note that variables are leaf nodes but not literals.

_See also: _`is_literal/1`, `type/1`.

# `is_literal`

```erlang
-spec is_literal(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` represents a literal term, otherwise `false`.

This function returns `true` if and only if the value of
[`concrete(Node)`](`concrete/1`) is defined.

Note: This is a constant time operation.

_See also: _`abstract/1`, `concrete/1`, `fold_literal/1`.

# `is_literal_term`

```erlang
-spec is_literal_term(Term :: term()) -> boolean().
```

Returns `true` if `Term` can be represented as a literal, otherwise `false`.

This function takes time proportional to the size of `Term`.

_See also: _`abstract/1`.

# `is_print_char`

```erlang
-spec is_print_char(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` may represent a "printing" character, otherwise
`false`. (Cf. [`is_c_char/1`](`is_c_char/1`).)

A "printing" character has either a given graphical representation, or
a "named" escape sequence such as "`\n`".  Currently, only ISO 8859-1
(Latin-1) character values are recognized.

_See also: _`c_char/1`, `is_c_char/1`.

# `is_print_string`

```erlang
-spec is_print_string(Node :: cerl()) -> boolean().
```

Returns `true` if `Node` may represent a string literal containing only
"printing" characters, otherwise `false`.

See [`is_c_string/1`](`is_c_string/1`) and
[`is_print_char/1`](`is_print_char/1`) for details. Currently, only
ISO 8859-1 (Latin-1) character values are recognized.

_See also: _`c_string/1`, `is_c_string/1`, `is_print_char/1`.

# `let_arg`

```erlang
-spec let_arg(Node :: c_let()) -> cerl().
```

Returns the argument subtree of an abstract let-expression.

_See also: _`c_let/3`.

# `let_arity`

```erlang
-spec let_arity(Node :: c_let()) -> non_neg_integer().
```

Returns the number of left-hand side variables of an abstract let-expression.

Note: this is equivalent to [`length(let_vars(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_let/3`, `let_vars/1`.

# `let_body`

```erlang
-spec let_body(Node :: c_let()) -> cerl().
```

Returns the body subtree of an abstract let-expression.

_See also: _`c_let/3`.

# `let_vars`

```erlang
-spec let_vars(Node :: c_let()) -> [cerl()].
```

Returns the list of left-hand side variables of an abstract let-expression.

_See also: _`c_let/3`, `let_arity/1`.

# `letrec_body`

```erlang
-spec letrec_body(Node :: c_letrec()) -> cerl().
```

Returns the body subtree of an abstract letrec-expression.

_See also: _`c_letrec/2`.

# `letrec_defs`

```erlang
-spec letrec_defs(Node :: c_letrec()) -> [{cerl(), cerl()}].
```

Returns the list of definitions of an abstract letrec-expression.

If `Node` represents "`letrec V1 = F1 ... Vn = Fn in Body`", the
returned value is `[{V1, F1}, ..., {Vn, Fn}]`.

_See also: _`c_letrec/2`.

# `letrec_vars`

```erlang
-spec letrec_vars(Node :: c_letrec()) -> [cerl()].
```

Returns the list of left-hand side function variable subtrees of a
letrec-expression.

If `Node` represents§ "`letrec V1 = F1 ... Vn = Fn in Body`", the
returned value is `[V1, ..., Vn]`.

_See also: _`c_letrec/2`.

# `list_elements`

```erlang
-spec list_elements(Node :: c_cons() | c_literal()) -> [cerl()].
```

Returns the list of element subtrees of an abstract list.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1, X2 | [X3, X4 | []]`", then
[`list_elements(Node)`](`list_elements/1`) yields the list `[X1, X2,
X3, X4]`.

_See also: _`c_cons/2`, `c_nil/0`, `is_c_list/1`, `list_length/1`,
`make_list/2`.

# `list_length`

```erlang
-spec list_length(Node :: c_cons() | c_literal()) -> non_neg_integer().
```

Returns the number of element subtrees of an abstract list.

`Node` must represent a proper list. For example, if `Node` represents
"`[X1 | [X2, X3 | [X4, X5, X6]]]`", then
[`list_length(Node)`](`list_length/1`) returns the integer 6.

Note: this is equivalent to [`length(list_elements(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_cons/2`, `c_nil/0`, `is_c_list/1`, `list_elements/1`.

# `make_data`

```erlang
-spec make_data(Type :: dtype(), Elements :: [cerl()]) -> c_lct().
```

Creates a data constructor node with the specified type and subtrees. (Cf.
[`data_type/1`](`data_type/1`).)

An exception is thrown if the length of `Elements` is invalid for the
given `Type`; see [`data_es/1`](`data_es/1`) for arity constraints on
constructor types.

_See also: _`ann_make_data/3`, `data_es/1`, `data_type/1`, `make_data_skel/2`,
`update_data/3`.

# `make_data_skel`

```erlang
-spec make_data_skel(Type :: dtype(), Elements :: [cerl()]) -> c_lct().
```

Like [`make_data/2`](`make_data/2`), but analogous to
[`c_tuple_skel/1`](`c_tuple_skel/1`) and [`c_cons_skel/2`](`c_cons_skel/2`).

_See also: _`ann_make_data_skel/3`, `c_cons_skel/2`, `c_tuple_skel/1`,
`make_data/2`, `update_data_skel/3`.

# `make_list`

```erlang
-spec make_list(List :: [cerl()]) -> cerl().
```

# `make_list`

```erlang
-spec make_list(List :: [cerl()], Tail :: cerl() | none) -> cerl().
```

Creates an abstract list from the elements in `List` and the optional `Tail`.

If `Tail` is `none`, the result will represent a nil-terminated list,
otherwise it represents "`[... | Tail]`".

_See also: _`ann_make_list/3`, `c_cons/2`, `c_nil/0`, `list_elements/1`,
`update_list/3`.

# `make_tree`

```erlang
-spec make_tree(Type :: ctype(), Groups :: [[cerl()], ...]) -> cerl().
```

Creates a syntax tree with the given type and subtrees.

`Type` must be a node type name (cf. [`type/1`](`type/1`)) that does
not denote a leaf node type (cf.
[`is_leaf/1`](`is_leaf/1`)).

`Groups` must be a _nonempty_ list of groups of syntax trees,
representing the subtrees of a node of the given type, in
left-to-right order as they would occur in the printed program text,
grouped by category as done by [`subtrees/1`](`subtrees/1`).

The result of
[`ann_make_tree(get_ann(Node), type(Node), subtrees(Node))`](`ann_make_tree/3`)
(cf. [`update_tree/2`](`update_tree/2`)) represents the same source code text as
the original `Node`, assuming that [`subtrees(Node)`](`subtrees/1`) yields a
nonempty list. However, it does not necessarily have the exact same data
representation as `Node`.

_See also: _`ann_make_tree/3`, `is_leaf/1`, `subtrees/1`, `type/1`,
`update_tree/2`.

# `map_arg`
*since OTP 17.0* 

```erlang
-spec map_arg(Node :: c_map() | c_literal()) -> c_map() | c_literal().
```

Returns the argument subtree of an abstract map.

_See also: _`c_map/2`.

# `map_es`
*since OTP 17.0* 

```erlang
-spec map_es(Node :: c_map() | c_literal()) -> [c_map_pair()].
```

Returns the list of map pair subtrees of an abstract map.

_See also: _`c_map/1`.

# `map_pair_key`
*since OTP 17.0* 

```erlang
-spec map_pair_key(Node :: c_map_pair()) -> cerl().
```

Returns the key subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.

# `map_pair_op`
*since OTP 17.0* 

```erlang
-spec map_pair_op(Node :: c_map_pair()) -> map_op().
```

Returns the operation subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.

# `map_pair_val`
*since OTP 17.0* 

```erlang
-spec map_pair_val(Node :: c_map_pair()) -> cerl().
```

Returns the value subtree of an abstract map pair.

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.

# `meta`

```erlang
-spec meta(Tree :: cerl()) -> cerl().
```

Creates a meta-representation of a syntax tree.

The result represents an Erlang expression "`MetaTree`" which, if
evaluated, will yield a new syntax tree representing the same source
code text as `Tree` (although the actual data representation may be
different). The expression represented by `MetaTree` is
_implementation independent_ with regard to the data structures used
by the abstract syntax tree implementation.

Any node in `Tree` whose node type is `var` (cf. [`type/1`](`type/1`)), and
whose list of annotations (cf. [`get_ann/1`](`get_ann/1`)) contains the atom
`meta_var`, will remain unchanged in the resulting tree, except that exactly one
occurrence of `meta_var` is removed from its annotation list.

The main use of the function [`meta/1`](`meta/1`) is to transform a data
structure `Tree`, which represents a piece of program code, into a form that is
_representation independent when printed_. E.g., suppose `Tree` represents a
variable named "V". Then (assuming a function `print/1` for printing syntax
trees), evaluating `print(abstract(Tree))` \- simply using
[`abstract/1`](`abstract/1`) to map the actual data structure onto a syntax tree
representation - would output a string that might look something like
"`{var, ..., 'V'}`", which is obviously dependent on the implementation of the
abstract syntax trees. This could, for example, be useful for caching a syntax tree
in a file. However, in some situations like in a program generator generator (with
two "generator"), it may be unacceptable. Using `print(meta(Tree))` instead
would output a _representation independent_ syntax tree generating expression;
in the above case, something like "`cerl:c_var('V')`".

The implementation tries to generate compact code with respect to literals and
lists.

_See also: _`abstract/1`, `get_ann/1`, `type/1`.

# `module_attrs`

```erlang
-spec module_attrs(Node :: c_module()) -> [{cerl(), cerl()}].
```

Returns the list of pairs of attribute key/value subtrees of an abstract module
definition.

_See also: _`c_module/4`.

# `module_defs`

```erlang
-spec module_defs(Node :: c_module()) -> [{cerl(), cerl()}].
```

Returns the list of function definitions of an abstract module definition.

_See also: _`c_module/4`.

# `module_exports`

```erlang
-spec module_exports(Node :: c_module()) -> [cerl()].
```

Returns the list of exports subtrees of an abstract module definition.

_See also: _`c_module/4`.

# `module_name`

```erlang
-spec module_name(Node :: c_module()) -> cerl().
```

Returns the name subtree of an abstract module definition.

_See also: _`c_module/4`.

# `module_vars`

```erlang
-spec module_vars(Node :: c_module()) -> [cerl()].
```

Returns the list of left-hand side function variable subtrees of an abstract
module definition.

_See also: _`c_module/4`.

# `pat_list_vars`

```erlang
-spec pat_list_vars(Patterns :: [cerl()]) -> [cerl()].
```

Returns the list of all abstract variables in the given patterns.

An exception is thrown if some element in `Patterns` does not
represent a well-formed Core Erlang clause pattern. The order of
listing is not defined.

_See also: _`clause_vars/1`, `pat_vars/1`.

# `pat_vars`

```erlang
-spec pat_vars(Node :: cerl()) -> [cerl()].
```

Returns the list of all abstract variables in a pattern.

An exception is thrown if `Node` does not represent a well-formed Core
Erlang clause pattern. The order of listing is not defined.

_See also: _`clause_vars/1`, `pat_list_vars/1`.

# `primop_args`

```erlang
-spec primop_args(Node :: c_primop()) -> [cerl()].
```

Returns the list of argument subtrees of an abstract primitive operation call.

_See also: _`c_primop/2`, `primop_arity/1`.

# `primop_arity`

```erlang
-spec primop_arity(Node :: c_primop()) -> arity().
```

Returns the number of argument subtrees of an abstract primitive operation call.

Note: this is equivalent to [`length(primop_args(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_primop/2`, `primop_args/1`.

# `primop_name`

```erlang
-spec primop_name(Node :: c_primop()) -> cerl().
```

Returns the name subtree of an abstract primitive operation call.

_See also: _`c_primop/2`.

# `receive_action`

```erlang
-spec receive_action(Node :: c_receive()) -> cerl().
```

Returns the action subtree of an abstract receive-expression.

_See also: _`c_receive/3`.

# `receive_clauses`

```erlang
-spec receive_clauses(Node :: c_receive()) -> [cerl()].
```

Returns the list of clause subtrees of an abstract receive-expression.

_See also: _`c_receive/3`.

# `receive_timeout`

```erlang
-spec receive_timeout(Node :: c_receive()) -> cerl().
```

Returns the timeout subtree of an abstract receive-expression.

_See also: _`c_receive/3`.

# `record_arg`
*since OTP 29.0* 

```erlang
-spec record_arg(Node :: c_record()) -> c_var() | c_literal().
```

Returns the argument subtree of an abstract record.

_See also: _`c_record/2`.

# `record_es`
*since OTP 29.0* 

```erlang
-spec record_es(Node :: c_record()) -> [c_record_pair()].
```

Returns the list of native record pair subtrees of an abstract record.

_See also: _`c_record/2`.

# `record_id`
*since OTP 29.0* 

```erlang
-spec record_id(Node :: c_record()) -> record_id().
```

Returns the identifier of of an abstract record.

_See also: _`c_record/2`.

# `record_pair_key`
*since OTP 29.0* 

```erlang
-spec record_pair_key(Node :: c_record_pair()) -> c_literal().
```

Returns the key subtree of an abstract record pair.

_See also: _`c_record_pair/2`.

# `record_pair_val`
*since OTP 29.0* 

```erlang
-spec record_pair_val(Node :: c_record_pair()) -> cerl().
```

Returns the value subtree of an abstract record pair.

_See also: _`c_record_pair/2`.

# `seq_arg`

```erlang
-spec seq_arg(Node :: c_seq()) -> cerl().
```

Returns the argument subtree of an abstract sequencing expression.

_See also: _`c_seq/2`.

# `seq_body`

```erlang
-spec seq_body(Node :: c_seq()) -> cerl().
```

Returns the body subtree of an abstract sequencing expression.

_See also: _`c_seq/2`.

# `set_ann`

```erlang
-spec set_ann(Node :: cerl(), Annotations :: [term()]) -> cerl().
```

Sets the list of user annotations of `Node` to `Annotations`.

_See also: _`add_ann/2`, `copy_ann/2`, `get_ann/1`.

# `string_lit`

```erlang
-spec string_lit(Node :: c_literal()) -> nonempty_string().
```

Returns the literal string represented by an abstract string. This includes
surrounding double-quote characters `"..."`.

Currently, characters that are not in the set of ISO 8859-1 (Latin-1)
"printing" characters will be escaped, except for spaces.

_See also: _`c_string/1`.

# `string_val`

```erlang
-spec string_val(Node :: c_literal()) -> string().
```

Returns the value represented by an abstract string literal.

_See also: _`c_string/1`.

# `subtrees`

```erlang
-spec subtrees(Node :: cerl()) -> [[cerl()]].
```

Returns the grouped list of all subtrees of a node.

If `Node` is a leaf node (cf. [`is_leaf/1`](`is_leaf/1`)), this is the
empty list, otherwise the result is always a nonempty list, containing
the lists of subtrees of `Node`, in left-to-right order as they occur
in the printed program text, and grouped by category. Often, each
group contains only a single subtree.

Depending on the type of `Node`, the size of some groups may be
variable (for example, the group consisting of all the elements of a
tuple), while others always contain the same number of elements -
usually exactly one (for example, the group containing the argument
expression of a case-expression). Note, however, that the exact
structure of the returned list (for a given node type) should in
general not be depended upon, since it might be subject to change
without notice.

The function [`subtrees/1`](`subtrees/1`) and the constructor functions
[`make_tree/2`](`make_tree/2`) and [`update_tree/2`](`update_tree/2`) can be a
great help if one wants to traverse a syntax tree, visiting all its subtrees,
but treat nodes of the tree in a uniform way in most or all cases. Using these
functions makes this simple, and also assures that your code is not overly
sensitive to extensions of the syntax tree data type, because any node types not
explicitly handled by your code can be left to a default case.

For example:

```text
    postorder(F, Tree) ->
        F(case subtrees(Tree) of
            [] -> Tree;
            List -> update_tree(Tree,
                                [[postorder(F, Subtree)
                                  || Subtree <- Group]
                                 || Group <- List])
          end).

```

maps the function `F` on `Tree` and all its subtrees, doing a post-order
traversal of the syntax tree. (Note the use of
[`update_tree/2`](`update_tree/2`) to preserve annotations.) For a simple
function like:

```text
    f(Node) ->
        case type(Node) of
            atom -> atom("a_" ++ atom_name(Node));
            _ -> Node
        end.

```

the call `postorder(fun f/1, Tree)` will yield a new representation of `Tree` in
which all atom names have been extended with the prefix "a\_", but nothing else
(including annotations) has been changed.

_See also: _`is_leaf/1`, `make_tree/2`, `update_tree/2`.

# `to_records`

```erlang
-spec to_records(Node :: cerl()) -> cerl().
```

Translates an abstract syntax tree to a corresponding explicit record
representation.

The records are defined in the file "`cerl.hrl`".

_See also: _`from_records/1`, `type/1`.

# `try_arg`

```erlang
-spec try_arg(Node :: c_try()) -> cerl().
```

Returns the expression subtree of an abstract try-expression.

_See also: _`c_try/5`.

# `try_body`

```erlang
-spec try_body(Node :: c_try()) -> cerl().
```

Returns the success body subtree of an abstract try-expression.

_See also: _`c_try/5`.

# `try_evars`

```erlang
-spec try_evars(Node :: c_try()) -> [cerl()].
```

Returns the list of exception variable subtrees of an abstract try-expression.

_See also: _`c_try/5`.

# `try_handler`

```erlang
-spec try_handler(Node :: c_try()) -> cerl().
```

Returns the exception body subtree of an abstract try-expression.

_See also: _`c_try/5`.

# `try_vars`

```erlang
-spec try_vars(Node :: c_try()) -> [cerl()].
```

Returns the list of success variable subtrees of an abstract try-expression.

_See also: _`c_try/5`.

# `tuple_arity`

```erlang
-spec tuple_arity(Node :: c_tuple() | c_literal()) -> non_neg_integer().
```

Returns the number of element subtrees of an abstract tuple.

Note: this is equivalent to [`length(tuple_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_tuple/1`, `tuple_es/1`.

# `tuple_es`

```erlang
-spec tuple_es(Node :: c_tuple() | c_literal()) -> [cerl()].
```

Returns the list of element subtrees of an abstract tuple.

_See also: _`c_tuple/1`.

# `type`

```erlang
-spec type(Node :: cerl()) -> ctype().
```

Returns the type tag of `Node`.

Current node types are:

- `alias`
- `apply`
- `binary`
- `bitstr`
- `call`
- `case`
- `catch`
- `clause`
- `cons`
- `fun`
- `let`
- `letrec`
- `literal`
- `map`
- `map_pair`
- `module`
- `opaque`
- `primop`
- `receive`
- `seq`
- `record`
- `try`
- `tuple`
- `values`
- `var`

> #### Note {: .info }
> The name of the primary constructor function for a node type is always the
> name of the type itself, prefixed by "`c_`"; recognizer predicates are
> correspondingly prefixed by "`is_c_`". Furthermore, to simplify preservation of
> annotations (cf. [`get_ann/1`](`get_ann/1`)), there are analogous constructor
> functions prefixed by "`ann_c_`" and "`update_c_`", for setting the annotation
> list of the new node to either a specific value or to the annotations of an
> existing node, respectively.

The only purpose of the `opaque` type is to facilitate testing of the compiler.

_See also: _`abstract/1`, `c_alias/2`, `c_apply/2`, `c_binary/1`, `c_bitstr/5`,
`c_call/3`, `c_case/2`, `c_catch/1`, `c_clause/3`, `c_cons/2`, `c_fun/2`,
`c_let/3`, `c_letrec/2`, `c_module/3`, `c_primop/2`, `c_receive/1`, `c_seq/2`,
`c_record/2`, `c_try/5`, `c_tuple/1`, `c_values/1`, `c_var/1`, `data_type/1`,
`from_records/1`, `get_ann/1`, `meta/1`, `subtrees/1`, `to_records/1`.

# `unfold_literal`

```erlang
-spec unfold_literal(Node :: cerl()) -> cerl().
```

Ensures that literals have a fully expanded representation.

If `Node` represents a literal tuple or list constructor, its elements
are rewritten recursively, and the node is reconstructed using
[`c_cons_skel/2`](`c_cons_skel/2`) or
[`c_tuple_skel/1`](`c_tuple_skel/1`), respectively; otherwise, `Node`
is not changed. The `fold_literal/1` can be used to revert to the
normal compact representation.

_See also: _`c_cons/2`, `c_cons_skel/2`, `c_tuple/1`, `c_tuple_skel/1`,
`fold_literal/1`, `is_literal/1`.

# `update_c_alias`

```erlang
-spec update_c_alias(Node :: c_alias(), Variable :: cerl(), Pattern :: cerl()) -> c_alias().
```

_See also: _`c_alias/2`.

# `update_c_apply`

```erlang
-spec update_c_apply(Node :: c_apply(), Operator :: cerl(), Arguments :: [cerl()]) -> c_apply().
```

_See also: _`c_apply/2`.

# `update_c_binary`

```erlang
-spec update_c_binary(Node :: c_binary(), Segments :: [cerl()]) -> c_binary().
```

_See also: _`c_binary/1`.

# `update_c_bitstr`

```erlang
-spec update_c_bitstr(Node :: c_bitstr(),
                      Value :: cerl(),
                      Size :: cerl(),
                      Type :: cerl(),
                      Flags :: cerl()) ->
                         c_bitstr().
```

Equivalent to
[update_c_bitstr(Node, Value, Size, abstract(1), Type, Flags)](`update_c_bitstr/6`).

# `update_c_bitstr`

```erlang
-spec update_c_bitstr(Node :: c_bitstr(),
                      Value :: cerl(),
                      Size :: cerl(),
                      Unit :: cerl(),
                      Type :: cerl(),
                      Flags :: cerl()) ->
                         c_bitstr().
```

_See also: _`c_bitstr/5`, `update_c_bitstr/5`.

# `update_c_call`

```erlang
-spec update_c_call(Node :: cerl(), Module :: cerl(), Name :: cerl(), Arguments :: [cerl()]) -> c_call().
```

_See also: _`c_call/3`.

# `update_c_case`

```erlang
-spec update_c_case(Node :: c_case(), Argument :: cerl(), Clauses :: [cerl()]) -> c_case().
```

_See also: _`c_case/2`.

# `update_c_catch`

```erlang
-spec update_c_catch(Node :: c_catch(), Body :: cerl()) -> c_catch().
```

_See also: _`c_catch/1`.

# `update_c_clause`

```erlang
-spec update_c_clause(Node :: c_clause(), Patterns :: [cerl()], Guard :: cerl(), Body :: cerl()) ->
                         c_clause().
```

_See also: _`c_clause/3`.

# `update_c_cons`

```erlang
-spec update_c_cons(Node :: c_literal() | c_cons(), Head :: cerl(), Tail :: cerl()) ->
                       c_literal() | c_cons().
```

_See also: _`c_cons/2`.

# `update_c_cons_skel`

```erlang
-spec update_c_cons_skel(Node :: c_cons() | c_literal(), Head :: cerl(), Tail :: cerl()) -> c_cons().
```

_See also: _`c_cons_skel/2`.

# `update_c_fname`

```erlang
-spec update_c_fname(Node :: c_var(), Name :: atom()) -> c_var().
```

Like [`update_c_fname/3`](`update_c_fname/3`), but takes the arity from `Node`.

_See also: _`c_fname/2`, `update_c_fname/3`.

# `update_c_fname`

```erlang
-spec update_c_fname(Node :: c_var(), Name :: atom(), Arity :: arity()) -> c_var().
```

Equivalent to [update_c_var(Old, \{Atom, Arity\})](`update_c_var/2`).

_See also: _`c_fname/2`, `update_c_fname/2`.

# `update_c_fun`

```erlang
-spec update_c_fun(Node :: c_fun(), Variables :: [cerl()], Body :: cerl()) -> c_fun().
```

_See also: _`c_fun/2`.

# `update_c_let`

```erlang
-spec update_c_let(Node :: c_let(), Variables :: [cerl()], Argument :: cerl(), Body :: cerl()) ->
                      c_let().
```

_See also: _`c_let/3`.

# `update_c_letrec`

```erlang
-spec update_c_letrec(Node :: c_letrec(), Definitions :: [{cerl(), cerl()}], Body :: cerl()) ->
                         c_letrec().
```

_See also: _`c_letrec/2`.

# `update_c_map`
*since OTP 17.0* 

```erlang
-spec update_c_map(Node :: c_map(), Map :: cerl(), Pairs :: [c_map_pair()]) -> c_map() | c_literal().
```

_See also: _`c_map/1`, `c_map_pattern/1`.

# `update_c_map_pair`
*since OTP 17.0* 

```erlang
-spec update_c_map_pair(Node :: c_map_pair(), Operation :: map_op(), Key :: cerl(), Value :: cerl()) ->
                           c_map_pair().
```

_See also: _`c_map_pair/2`, `c_map_pair_exact/2`.

# `update_c_module`

```erlang
-spec update_c_module(Node :: c_module(),
                      Name :: cerl(),
                      Exports :: [cerl()],
                      Attributes :: [{cerl(), cerl()}],
                      Definitions :: [{cerl(), cerl()}]) ->
                         c_module().
```

_See also: _`c_module/4`.

# `update_c_primop`

```erlang
-spec update_c_primop(Node :: cerl(), Name :: cerl(), Arguments :: [cerl()]) -> c_primop().
```

_See also: _`c_primop/2`.

# `update_c_receive`

```erlang
-spec update_c_receive(Node :: c_receive(), Clauses :: [cerl()], Timeout :: cerl(), Action :: cerl()) ->
                          c_receive().
```

_See also: _`c_receive/3`.

# `update_c_record`
*since OTP 29.0* 

```erlang
-spec update_c_record(Node :: c_record() | c_literal(),
                      Arg :: c_var() | c_literal(),
                      Id :: record_id(),
                      Pairs :: [c_record_pair()]) ->
                         c_record().
```

_See also: _`c_record/2`.

# `update_c_record_pair`
*since OTP 29.0* 

```erlang
-spec update_c_record_pair(Node :: c_record_pair(), Key :: c_literal(), Value :: cerl()) ->
                              c_record_pair().
```

_See also: _`c_record_pair/2`.

# `update_c_seq`

```erlang
-spec update_c_seq(Node :: c_seq(), Argument :: cerl(), Body :: cerl()) -> c_seq().
```

_See also: _`c_seq/2`.

# `update_c_try`

```erlang
-spec update_c_try(Node :: c_try(),
                   Argument :: cerl(),
                   Variables :: [cerl()],
                   Body :: cerl(),
                   ExceptionVars :: [cerl()],
                   Handler :: cerl()) ->
                      c_try().
```

_See also: _`c_try/5`.

# `update_c_tuple`

```erlang
-spec update_c_tuple(Node :: c_tuple() | c_literal(), Elements :: [cerl()]) -> c_tuple() | c_literal().
```

_See also: _`c_tuple/1`.

# `update_c_tuple_skel`

```erlang
-spec update_c_tuple_skel(Node :: c_tuple(), Elements :: [cerl()]) -> c_tuple().
```

_See also: _`c_tuple_skel/1`.

# `update_c_values`

```erlang
-spec update_c_values(Node :: c_values(), Elements :: [cerl()]) -> c_values().
```

_See also: _`c_values/1`.

# `update_c_var`

```erlang
-spec update_c_var(Node :: c_var(), Name :: var_name()) -> c_var().
```

_See also: _`c_var/1`.

# `update_data`

```erlang
-spec update_data(Node :: cerl(), Type :: dtype(), Elements :: [cerl()]) -> c_lct().
```

_See also: _`make_data/2`.

# `update_data_skel`

```erlang
-spec update_data_skel(Node :: cerl(), Type :: dtype(), Elements :: [cerl()]) -> c_lct().
```

_See also: _`make_data_skel/2`.

# `update_list`

```erlang
-spec update_list(Node :: cerl(), List :: [cerl()]) -> cerl().
```

# `update_list`

```erlang
-spec update_list(Node :: cerl(), List :: [cerl()], Tail :: cerl() | none) -> cerl().
```

_See also: _`make_list/2`, `update_list/2`.

# `update_tree`

```erlang
-spec update_tree(Node :: cerl(), Groups :: [[cerl()], ...]) -> cerl().
```

Creates a syntax tree with the given subtrees, and the same type and annotations
as the node `Node`.

This is equivalent to [`ann_make_tree(get_ann(Node), type(Node),
Groups)`](`ann_make_tree/3`), but potentially more efficient.

_See also: _`ann_make_tree/3`, `get_ann/1`, `type/1`, `update_tree/3`.

# `update_tree`

```erlang
-spec update_tree(Node :: cerl(), Type :: ctype(), Groups :: [[cerl()], ...]) -> cerl().
```

Creates a syntax tree with the given type and subtrees, and the same annotations
as the node `Node`.

This is equivalent to
[`ann_make_tree(get_ann(Node), Type, Groups)`](`ann_make_tree/3`), but
potentially more efficient.

_See also: _`ann_make_tree/3`, `get_ann/1`, `update_tree/2`.

# `values_arity`

```erlang
-spec values_arity(Node :: c_values()) -> non_neg_integer().
```

Returns the number of element subtrees of an abstract value list.

Note: This is equivalent to [`length(values_es(Node))`](`length/1`), but
potentially more efficient.

_See also: _`c_values/1`, `values_es/1`.

# `values_es`

```erlang
-spec values_es(Node :: c_values()) -> [cerl()].
```

Returns the list of element subtrees of an abstract value list.

_See also: _`c_values/1`, `values_arity/1`.

# `var_name`

```erlang
-spec var_name(Node :: c_var()) -> var_name().
```

Returns the name of an abstract variable.

_See also: _`c_var/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
