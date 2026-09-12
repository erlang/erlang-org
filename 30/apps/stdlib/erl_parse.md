# `erl_parse`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_parse.yrl#L1)

This module is the basic Erlang parser that converts tokens into the abstract
form of either forms (that is, top-level constructs), expressions, or terms.

The Abstract Format is described in the ERTS User's Guide. Notice that a token
list must end with the dot token to be acceptable to the parse functions
(see the `m:erl_scan`) module.

## Error Information

ErrorInfo is the standard ErrorInfo structure that is returned from all I/O modules.
The format is as follows:

```
{ErrorLine, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```
Module:format_error(ErrorDescriptor)
```

## See Also

`m:erl_anno`, `m:erl_scan`, `m:io`, section [The Abstract Format](`e:erts:absform`)
in the ERTS User's Guide.

# `abstract_clause`

```erlang
-type abstract_clause() :: af_clause().
```

Abstract form of an Erlang clause.

# `abstract_expr`

```erlang
-type abstract_expr() ::
          af_literal() |
          af_match(abstract_expr()) |
          af_maybe_match() |
          af_variable() |
          af_tuple(abstract_expr()) |
          af_nil() |
          af_cons(abstract_expr()) |
          af_bin(abstract_expr()) |
          af_binary_op(abstract_expr()) |
          af_unary_op(abstract_expr()) |
          af_record_creation(abstract_expr()) |
          af_record_update(abstract_expr()) |
          af_record_index() |
          af_record_field_access(abstract_expr()) |
          af_native_record_creation() |
          af_native_record_update() |
          af_map_creation(abstract_expr()) |
          af_map_update(abstract_expr()) |
          af_catch() |
          af_local_call() |
          af_remote_call() |
          af_list_comprehension() |
          af_map_comprehension() |
          af_binary_comprehension() |
          af_block() |
          af_if() |
          af_case() |
          af_try() |
          af_receive() |
          af_local_fun() |
          af_remote_fun() |
          af_fun() |
          af_named_fun() |
          af_maybe() |
          af_maybe_else().
```

Abstract form of an Erlang expression.

# `abstract_form`

```erlang
-type abstract_form() ::
          af_module() |
          af_behavior() |
          af_behaviour() |
          af_export() |
          af_import() |
          af_import_record() |
          af_export_type() |
          af_compile() |
          af_file() |
          af_record_decl() |
          af_native_record_decl() |
          af_type_decl() |
          af_function_spec() |
          af_wild_attribute() |
          af_function_decl().
```

Abstract form of an Erlang form.

# `abstract_type`

```erlang
-type abstract_type() ::
          af_annotated_type() |
          af_atom() |
          af_bitstring_type() |
          af_empty_list_type() |
          af_fun_type() |
          af_integer_range_type() |
          af_map_type() |
          af_predefined_type() |
          af_record_type() |
          af_remote_type() |
          af_singleton_integer_type() |
          af_tuple_type() |
          af_type_union() |
          af_type_variable() |
          af_user_defined_type().
```

Abstract form of an Erlang type.

# `af_anno`
*not exported* 

```erlang
-type af_anno() :: af_variable().
```

# `af_annotated_type`
*not exported* 

```erlang
-type af_annotated_type() :: {ann_type, anno(), [af_anno() | abstract_type()]}.
```

# `af_args`
*not exported* 

```erlang
-type af_args() :: [abstract_expr()].
```

# `af_assoc`
*not exported* 

```erlang
-type af_assoc(T) :: {map_field_assoc, anno(), T, T} | af_assoc_exact(T).
```

# `af_assoc_exact`
*not exported* 

```erlang
-type af_assoc_exact(T) :: {map_field_exact, anno(), T, T}.
```

# `af_assoc_type`
*not exported* 

```erlang
-type af_assoc_type() ::
          {type, anno(), map_field_assoc, [abstract_type()]} |
          {type, anno(), map_field_exact, [abstract_type()]}.
```

# `af_atom`
*not exported* 

```erlang
-type af_atom() :: af_lit_atom(atom()).
```

# `af_behavior`
*not exported* 

```erlang
-type af_behavior() :: {attribute, anno(), behavior, behaviour()}.
```

# `af_behaviour`
*not exported* 

```erlang
-type af_behaviour() :: {attribute, anno(), behaviour, behaviour()}.
```

# `af_bin`
*not exported* 

```erlang
-type af_bin(T) :: {bin, anno(), [af_binelement(T)]}.
```

# `af_binary_comprehension`
*not exported* 

```erlang
-type af_binary_comprehension() :: {bc, anno(), af_template(), af_qualifier_seq()}.
```

# `af_binary_op`
*not exported* 

```erlang
-type af_binary_op(T) :: {op, anno(), binary_op(), T, T}.
```

# `af_binelement`

```erlang
-type af_binelement(T) :: {bin_element, anno(), T, af_binelement_size(), type_specifier_list()}.
```

Abstract representation of an element of a bitstring.

# `af_binelement_size`
*not exported* 

```erlang
-type af_binelement_size() :: default | abstract_expr().
```

# `af_bitstring_type`
*not exported* 

```erlang
-type af_bitstring_type() :: {type, anno(), binary, [af_singleton_integer_type()]}.
```

# `af_block`
*not exported* 

```erlang
-type af_block() :: {block, anno(), af_body()}.
```

# `af_body`
*not exported* 

```erlang
-type af_body() :: [abstract_expr(), ...].
```

# `af_case`
*not exported* 

```erlang
-type af_case() :: {'case', anno(), abstract_expr(), af_clause_seq()}.
```

# `af_catch`
*not exported* 

```erlang
-type af_catch() :: {'catch', anno(), abstract_expr()}.
```

# `af_character`
*not exported* 

```erlang
-type af_character() :: {char, anno(), char()}.
```

# `af_clause`
*not exported* 

```erlang
-type af_clause() :: {clause, anno(), [af_pattern()], af_guard_seq(), af_body()}.
```

# `af_clause_seq`
*not exported* 

```erlang
-type af_clause_seq() :: [af_clause(), ...].
```

# `af_compile`
*not exported* 

```erlang
-type af_compile() :: {attribute, anno(), compile, any()}.
```

# `af_cons`
*not exported* 

```erlang
-type af_cons(T) :: {cons, anno(), T, T}.
```

# `af_constrained_function_type`
*not exported* 

```erlang
-type af_constrained_function_type() ::
          {type, anno(), bounded_fun, [af_function_type() | af_function_constraint()]}.
```

# `af_constraint`
*not exported* 

```erlang
-type af_constraint() ::
          {type, anno(), constraint, [af_lit_atom(is_subtype) | [af_type_variable() | abstract_type()]]}.
```

# `af_empty_list_type`
*not exported* 

```erlang
-type af_empty_list_type() :: {type, anno(), nil, []}.
```

# `af_export`
*not exported* 

```erlang
-type af_export() :: {attribute, anno(), export, af_fa_list()}.
```

# `af_export_type`
*not exported* 

```erlang
-type af_export_type() :: {attribute, anno(), export_type, af_ta_list()}.
```

# `af_fa_list`
*not exported* 

```erlang
-type af_fa_list() :: [{function_name(), arity()}].
```

# `af_field`
*not exported* 

```erlang
-type af_field() ::
          {record_field, anno(), af_field_name()} |
          {record_field, anno(), af_field_name(), abstract_expr()}.
```

# `af_field_decl`

```erlang
-type af_field_decl() :: af_typed_field() | af_field().
```

Abstract representation of a record field.

# `af_field_name`
*not exported* 

```erlang
-type af_field_name() :: af_atom().
```

# `af_file`
*not exported* 

```erlang
-type af_file() :: {attribute, anno(), file, {string(), anno()}}.
```

# `af_filter`
*not exported* 

```erlang
-type af_filter() :: abstract_expr().
```

# `af_float`
*not exported* 

```erlang
-type af_float() :: {float, anno(), float()}.
```

# `af_fun`
*not exported* 

```erlang
-type af_fun() :: {'fun', anno(), {clauses, af_clause_seq()}}.
```

# `af_fun_type`
*not exported* 

```erlang
-type af_fun_type() ::
          {type, anno(), 'fun', []} |
          {type, anno(), 'fun', [{type, anno(), any} | abstract_type()]} |
          af_function_type().
```

# `af_function_constraint`
*not exported* 

```erlang
-type af_function_constraint() :: [af_constraint(), ...].
```

# `af_function_decl`

```erlang
-type af_function_decl() :: {function, anno(), function_name(), arity(), af_clause_seq()}.
```

# `af_function_spec`
*not exported* 

```erlang
-type af_function_spec() ::
          {attribute, anno(), spec_attr(), {{function_name(), arity()}, af_function_type_list()}} |
          {attribute, anno(), spec, {{module(), function_name(), arity()}, af_function_type_list()}}.
```

# `af_function_type`
*not exported* 

```erlang
-type af_function_type() ::
          {type, anno(), 'fun', [{type, anno(), product, [abstract_type()]} | abstract_type()]}.
```

# `af_function_type_list`
*not exported* 

```erlang
-type af_function_type_list() :: [af_constrained_function_type() | af_function_type(), ...].
```

# `af_generator`

```erlang
-type af_generator() ::
          {generate, anno(), af_pattern(), abstract_expr()} |
          {generate_strict, anno(), af_pattern(), abstract_expr()} |
          {m_generate, anno(), af_assoc_exact(af_pattern()), abstract_expr()} |
          {m_generate_strict, anno(), af_assoc_exact(af_pattern()), abstract_expr()} |
          {b_generate, anno(), af_pattern(), abstract_expr()} |
          {b_generate_strict, anno(), af_pattern(), abstract_expr()} |
          af_zip_generator().
```

Abstract representation of a list, bitstring or map generator.

# `af_guard`
*not exported* 

```erlang
-type af_guard() :: [af_guard_test(), ...].
```

# `af_guard_call`
*not exported* 

```erlang
-type af_guard_call() :: {call, anno(), af_atom(), [af_guard_test()]}.
```

# `af_guard_seq`
*not exported* 

```erlang
-type af_guard_seq() :: [af_guard()].
```

# `af_guard_test`
*not exported* 

```erlang
-type af_guard_test() ::
          af_literal() |
          af_variable() |
          af_tuple(af_guard_test()) |
          af_nil() |
          af_cons(af_guard_test()) |
          af_bin(af_guard_test()) |
          af_binary_op(af_guard_test()) |
          af_unary_op(af_guard_test()) |
          af_record_creation(af_guard_test()) |
          af_record_index() |
          af_record_field_access(af_guard_test()) |
          af_map_creation(af_guard_test()) |
          af_map_update(af_guard_test()) |
          af_guard_call() |
          af_remote_guard_call().
```

# `af_if`
*not exported* 

```erlang
-type af_if() :: {'if', anno(), af_clause_seq()}.
```

# `af_import`
*not exported* 

```erlang
-type af_import() :: {attribute, anno(), import, {module(), af_fa_list()}}.
```

# `af_import_record`
*not exported* 

```erlang
-type af_import_record() :: {attribute, anno(), import_record, {module(), [atom()]}}.
```

# `af_integer`
*not exported* 

```erlang
-type af_integer() :: {integer, anno(), non_neg_integer()}.
```

# `af_integer_range_type`
*not exported* 

```erlang
-type af_integer_range_type() :: {type, anno(), range, [af_singleton_integer_type()]}.
```

# `af_list_comprehension`
*not exported* 

```erlang
-type af_list_comprehension() :: {lc, anno(), af_template() | [af_template()], af_qualifier_seq()}.
```

# `af_lit_atom`
*not exported* 

```erlang
-type af_lit_atom(A) :: {atom, anno(), A}.
```

# `af_literal`
*not exported* 

```erlang
-type af_literal() :: af_atom() | af_character() | af_float() | af_integer() | af_string().
```

# `af_local_call`
*not exported* 

```erlang
-type af_local_call() :: {call, anno(), af_local_function(), af_args()}.
```

# `af_local_fun`
*not exported* 

```erlang
-type af_local_fun() :: {'fun', anno(), {function, function_name(), arity()}}.
```

# `af_local_function`
*not exported* 

```erlang
-type af_local_function() :: abstract_expr().
```

# `af_map_comprehension`
*not exported* 

```erlang
-type af_map_comprehension() ::
          {mc, anno(), af_assoc(abstract_expr()) | [af_assoc(abstract_expr())], af_qualifier_seq()}.
```

# `af_map_creation`
*not exported* 

```erlang
-type af_map_creation(T) :: {map, anno(), [af_assoc(T)]}.
```

# `af_map_pattern`
*not exported* 

```erlang
-type af_map_pattern() :: {map, anno(), [af_assoc_exact(af_pattern())]}.
```

# `af_map_type`
*not exported* 

```erlang
-type af_map_type() :: {type, anno(), map, any} | {type, anno(), map, [af_assoc_type()]}.
```

# `af_map_update`
*not exported* 

```erlang
-type af_map_update(T) :: {map, anno(), T, [af_assoc(T)]}.
```

# `af_match`
*not exported* 

```erlang
-type af_match(T) :: {match, anno(), af_pattern(), T}.
```

# `af_maybe`
*not exported* 

```erlang
-type af_maybe() :: {'maybe', anno(), af_body()}.
```

# `af_maybe_else`
*not exported* 

```erlang
-type af_maybe_else() :: {'maybe', anno(), af_body(), {'else', anno(), af_clause_seq()}}.
```

# `af_maybe_match`
*not exported* 

```erlang
-type af_maybe_match() :: {maybe_match, anno(), af_pattern(), abstract_expr()}.
```

# `af_module`
*not exported* 

```erlang
-type af_module() :: {attribute, anno(), module, module()}.
```

# `af_named_fun`
*not exported* 

```erlang
-type af_named_fun() :: {named_fun, anno(), fun_name(), af_clause_seq()}.
```

# `af_native_record_creation`
*not exported* 

```erlang
-type af_native_record_creation() ::
          {native_record, anno(), {atom(), atom()} | {}, [af_record_field(abstract_expr())]}.
```

# `af_native_record_decl`
*not exported* 

```erlang
-type af_native_record_decl() ::
          {attribute, anno(), native_record, {NativeRecordName :: atom(), [af_field()]}}.
```

# `af_native_record_pattern`
*not exported* 

```erlang
-type af_native_record_pattern() ::
          {native_record, anno(), {atom(), atom()} | {}, [af_record_field(af_pattern())]}.
```

# `af_native_record_update`
*not exported* 

```erlang
-type af_native_record_update() ::
          {native_record_update,
           anno(),
           abstract_expr(),
           {atom(), atom()} | {},
           [af_record_field(abstract_expr())]}.
```

# `af_nil`
*not exported* 

```erlang
-type af_nil() :: {nil, anno()}.
```

# `af_pattern`

```erlang
-type af_pattern() ::
          af_literal() |
          af_match(af_pattern()) |
          af_variable() |
          af_tuple(af_pattern()) |
          af_nil() |
          af_cons(af_pattern()) |
          af_bin(af_pattern()) |
          af_binary_op(af_pattern()) |
          af_unary_op(af_pattern()) |
          af_record_creation(af_pattern()) |
          af_record_index() |
          af_native_record_pattern() |
          af_map_pattern().
```

# `af_predefined_type`
*not exported* 

```erlang
-type af_predefined_type() :: {type, anno(), type_name(), [abstract_type()]}.
```

# `af_qualifier`
*not exported* 

```erlang
-type af_qualifier() :: af_generator() | af_filter().
```

# `af_qualifier_seq`
*not exported* 

```erlang
-type af_qualifier_seq() :: [af_qualifier(), ...].
```

# `af_receive`
*not exported* 

```erlang
-type af_receive() ::
          {'receive', anno(), af_clause_seq()} |
          {'receive', anno(), af_clause_seq(), abstract_expr(), af_body()}.
```

# `af_record_creation`
*not exported* 

```erlang
-type af_record_creation(T) :: {record, anno(), record_name(), [af_record_field(T)]}.
```

# `af_record_decl`

```erlang
-type af_record_decl() :: {attribute, anno(), record, {record_name(), [af_field_decl()]}}.
```

# `af_record_field`

```erlang
-type af_record_field(T) :: {record_field, anno(), af_field_name(), T}.
```

# `af_record_field_access`

```erlang
-type af_record_field_access(T) :: {record_field, anno(), T, record_name(), af_field_name()}.
```

# `af_record_field_type`
*not exported* 

```erlang
-type af_record_field_type() :: {type, anno(), field_type, [(Name :: af_atom()) | abstract_type()]}.
```

# `af_record_index`
*not exported* 

```erlang
-type af_record_index() :: {record_index, anno(), record_name(), af_field_name()}.
```

# `af_record_type`
*not exported* 

```erlang
-type af_record_type() :: {type, anno(), record, [(Name :: af_atom()) | af_record_field_type()]}.
```

# `af_record_update`
*not exported* 

```erlang
-type af_record_update(T) :: {record, anno(), abstract_expr(), record_name(), [af_record_field(T)]}.
```

# `af_remote_call`
*not exported* 

```erlang
-type af_remote_call() :: {call, anno(), af_remote_function(), af_args()}.
```

# `af_remote_fun`
*not exported* 

```erlang
-type af_remote_fun() ::
          {'fun', anno(), {function, module(), function_name(), arity()}} |
          {'fun',
           anno(),
           {function,
            af_atom() | af_variable(),
            af_atom() | af_variable(),
            af_integer() | af_variable()}}.
```

# `af_remote_function`

```erlang
-type af_remote_function() :: {remote, anno(), abstract_expr(), abstract_expr()}.
```

Abstract representation of a remote function call.

# `af_remote_guard_call`
*not exported* 

```erlang
-type af_remote_guard_call() ::
          {call, anno(), {remote, anno(), af_lit_atom(erlang), af_atom()}, [af_guard_test()]}.
```

# `af_remote_type`
*not exported* 

```erlang
-type af_remote_type() ::
          {remote_type, anno(), [(Module :: af_atom()) | (TypeName :: af_atom()) | [abstract_type()]]}.
```

# `af_singleton_integer_type`
*not exported* 

```erlang
-type af_singleton_integer_type() ::
          af_integer() |
          af_character() |
          af_unary_op(af_singleton_integer_type()) |
          af_binary_op(af_singleton_integer_type()).
```

# `af_string`
*not exported* 

```erlang
-type af_string() :: {string, anno(), string()}.
```

# `af_ta_list`
*not exported* 

```erlang
-type af_ta_list() :: [{type_name(), arity()}].
```

# `af_template`
*not exported* 

```erlang
-type af_template() :: abstract_expr().
```

# `af_try`
*not exported* 

```erlang
-type af_try() :: {'try', anno(), af_body(), af_clause_seq() | [], af_clause_seq() | [], af_body() | []}.
```

# `af_tuple`
*not exported* 

```erlang
-type af_tuple(T) :: {tuple, anno(), [T]}.
```

# `af_tuple_type`
*not exported* 

```erlang
-type af_tuple_type() :: {type, anno(), tuple, any} | {type, anno(), tuple, [abstract_type()]}.
```

# `af_type_decl`
*not exported* 

```erlang
-type af_type_decl() ::
          {attribute, anno(), type_attr(), {type_name(), abstract_type(), [af_variable()]}}.
```

# `af_type_union`
*not exported* 

```erlang
-type af_type_union() :: {type, anno(), union, [abstract_type(), ...]}.
```

# `af_type_variable`
*not exported* 

```erlang
-type af_type_variable() :: {var, anno(), atom()}.
```

# `af_typed_field`
*not exported* 

```erlang
-type af_typed_field() :: {typed_record_field, af_field(), abstract_type()}.
```

# `af_unary_op`
*not exported* 

```erlang
-type af_unary_op(T) :: {op, anno(), unary_op(), T}.
```

# `af_user_defined_type`
*not exported* 

```erlang
-type af_user_defined_type() :: {user_type, anno(), type_name(), [abstract_type()]}.
```

# `af_variable`

```erlang
-type af_variable() :: {var, anno(), atom()}.
```

# `af_wild_attribute`
*not exported* 

```erlang
-type af_wild_attribute() :: {attribute, anno(), atom(), any()}.
```

# `af_zip_generator`

```erlang
-type af_zip_generator() :: {zip, anno(), [af_generator(), ...]}.
```

# `anno`
*not exported* 

```erlang
-type anno() :: erl_anno:anno().
```

# `behaviour`
*not exported* 

```erlang
-type behaviour() :: atom().
```

# `binary_op`
*not exported* 

```erlang
-type binary_op() ::
          '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-' | 'bor' | 'bxor' | 'bsl' | 'bsr' |
          'or' | 'xor' | '++' | '--' | '==' | '/=' | '=<' | '<' | '>=' | '>' | '=:=' | '=/=' | '!' |
          'andalso' | 'orelse'.
```

# `encoding_func`
*not exported* 

```erlang
-type encoding_func() :: fun((non_neg_integer()) -> boolean()).
```

# `endianness`
*not exported* 

```erlang
-type endianness() :: big | little | native.
```

# `erl_parse_tree`
*not exported* 

```erlang
-type erl_parse_tree() :: abstract_clause() | abstract_expr() | abstract_form() | abstract_type().
```

# `error_description`
*not exported* 

```erlang
-type error_description() :: term().
```

# `error_info`

```erlang
-type error_info() :: {erl_anno:location(), module(), error_description()}.
```

# `form_info`

```erlang
-type form_info() ::
          {eof, erl_anno:location()} |
          {error, erl_scan:error_info() | error_info()} |
          {warning, erl_scan:error_info() | error_info()}.
```

Tuples `{error, error_info()}` and `{warning, error_info()}`, denoting
syntactically incorrect forms and warnings, and `{eof, line()}`, denoting an
end-of-stream encountered before a complete form had been parsed.

# `fun_name`
*not exported* 

```erlang
-type fun_name() :: atom().
```

# `function_name`
*not exported* 

```erlang
-type function_name() :: atom().
```

# `record_name`

```erlang
-type record_name() :: atom().
```

# `signedness`
*not exported* 

```erlang
-type signedness() :: signed | unsigned.
```

# `spec_attr`
*not exported* 

```erlang
-type spec_attr() :: callback | spec.
```

# `token`
*not exported* 

```erlang
-type token() :: erl_scan:token().
```

# `type`
*not exported* 

```erlang
-type type() :: integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32.
```

# `type_attr`
*not exported* 

```erlang
-type type_attr() :: nominal | opaque | type.
```

# `type_name`
*not exported* 

```erlang
-type type_name() :: atom().
```

# `type_specifier`
*not exported* 

```erlang
-type type_specifier() :: type() | signedness() | endianness() | unit().
```

# `type_specifier_list`
*not exported* 

```erlang
-type type_specifier_list() :: default | [type_specifier(), ...].
```

# `unary_op`
*not exported* 

```erlang
-type unary_op() :: '+' | '-' | 'bnot' | 'not'.
```

# `unit`
*not exported* 

```erlang
-type unit() :: {unit, 1..256}.
```

# `abstract`

```erlang
-spec abstract(Data) -> AbsTerm when Data :: term(), AbsTerm :: abstract_expr().
```

Converts the Erlang data structure `Data` into an abstract form of type
`AbsTerm`. This function is the inverse of `normalise/1`.

`erl_parse:abstract(T)` is equivalent to `erl_parse:abstract(T, 0)`.

# `abstract`
*since OTP R16B01* 

```erlang
-spec abstract(Data, Options) -> AbsTerm
                  when
                      Data :: term(),
                      Options :: Location | [Option],
                      Option :: {encoding, Encoding} | {line, Line} | {location, Location},
                      Encoding :: latin1 | unicode | utf8 | none | encoding_func(),
                      Line :: erl_anno:line(),
                      Location :: erl_anno:location(),
                      AbsTerm :: abstract_expr().
```

Converts the Erlang data structure `Data` into an abstract form of type
`AbsTerm`.

Each node of `AbsTerm` is assigned an annotation, see `m:erl_anno`. The
annotation contains the location given by option `location` or by option `line`.
Option `location` overrides option `line`. If neither option `location` nor
option `line` is given, `0` is used as location.

Option `Encoding` is used for selecting which integer lists to be considered as
strings. The default is to use the encoding returned by function
`epp:default_encoding/0`. Value `none` means that no integer lists are
considered as strings. `encoding_func()` is called with one integer of a list at
a time; if it returns `true` for every integer, the list is considered a string.

# `anno_from_term`
*since OTP 18.0* 

```erlang
-spec anno_from_term(Term) -> erl_parse_tree() | form_info() when Term :: term().
```

Assumes that `Term` is a term with the same structure as a `erl_parse` tree, but
with terms, say `T`, where a `erl_parse` tree has collections of annotations.

Returns a `erl_parse` tree where each term `T` is replaced by the value returned
by [`erl_anno:from_term(T)`](`erl_anno:from_term/1`). The term `Term` is
traversed in a depth-first, left-to-right fashion.

# `anno_to_term`
*since OTP 18.0* 

```erlang
-spec anno_to_term(Abstr) -> term() when Abstr :: erl_parse_tree() | form_info().
```

Returns a term where each collection of annotations `Anno` of the nodes of the
`erl_parse` tree `Abstr` is replaced by the term returned by
[`erl_anno:to_term(Anno)`](`erl_anno:to_term/1`). The `erl_parse` tree is
traversed in a depth-first, left-to-right fashion.

# `fold_anno`
*since OTP 18.0* 

```erlang
-spec fold_anno(Fun, Acc0, Abstr) -> Acc1
                   when
                       Fun :: fun((Anno, AccIn) -> AccOut),
                       Anno :: erl_anno:anno(),
                       Acc0 :: term(),
                       Acc1 :: term(),
                       AccIn :: term(),
                       AccOut :: term(),
                       Abstr :: erl_parse_tree() | form_info().
```

Updates an accumulator by applying `Fun` on each collection of annotations of
the `erl_parse` tree `Abstr`.

The first call to `Fun` has `AccIn` as argument, the returned accumulator
`AccOut` is passed to the next call, and so on. The
final value of the accumulator is returned. The `erl_parse` tree is traversed in
a depth-first, left-to-right fashion.

# `format_error`

```erlang
-spec format_error(any()) -> [char() | list()].
```

Uses an ErrorDescriptor and returns a string that describes the error.

This function is usually called implicitly when an ErrorInfo structure is
processed (see section [Error Information](#module-error-information)).

# `map_anno`
*since OTP 18.0* 

```erlang
-spec map_anno(Fun, Abstr) -> NewAbstr
                  when
                      Fun :: fun((Anno) -> NewAnno),
                      Anno :: erl_anno:anno(),
                      NewAnno :: erl_anno:anno(),
                      Abstr :: erl_parse_tree() | form_info(),
                      NewAbstr :: erl_parse_tree() | form_info().
```

Modifies the `erl_parse` tree `Abstr` by applying `Fun` on each collection of
annotations of the nodes of the `erl_parse` tree. The `erl_parse` tree is
traversed in a depth-first, left-to-right fashion.

# `mapfold_anno`
*since OTP 18.0* 

```erlang
-spec mapfold_anno(Fun, Acc0, Abstr) -> {NewAbstr, Acc1}
                      when
                          Fun :: fun((Anno, AccIn) -> {NewAnno, AccOut}),
                          Anno :: erl_anno:anno(),
                          NewAnno :: erl_anno:anno(),
                          Acc0 :: term(),
                          Acc1 :: term(),
                          AccIn :: term(),
                          AccOut :: term(),
                          Abstr :: erl_parse_tree() | form_info(),
                          NewAbstr :: erl_parse_tree() | form_info().
```

Modifies the `erl_parse` tree `Abstr` by applying `Fun` on each collection of
annotations of the nodes of the `erl_parse` tree, while at the same time
updating an accumulator.

The first call to `Fun` has `AccIn` as second argument,
the returned accumulator `AccOut` is passed to the next call, and so on. The
modified `erl_parse` tree and the final value of the accumulator are returned.
The `erl_parse` tree is traversed in a depth-first, left-to-right fashion.

# `new_anno`
*since OTP 18.0* 

```erlang
-spec new_anno(Term) -> Abstr when Term :: term(), Abstr :: erl_parse_tree() | form_info().
```

Assumes that `Term` is a term with the same structure as a `erl_parse` tree, but
with [locations](`t:erl_anno:location/0`) where a `erl_parse` tree has
collections of annotations.

Returns a `erl_parse` tree where each location `L`
is replaced by the value returned by [`erl_anno:new(L)`](`erl_anno:new/1`). The
term `Term` is traversed in a depth-first, left-to-right fashion.

# `normalise`

```erlang
-spec normalise(AbsTerm) -> Data when AbsTerm :: abstract_expr(), Data :: term().
```

Converts the abstract form `AbsTerm` of a term into a conventional Erlang data
structure (that is, the term itself). This function is the inverse of
`abstract/1`.

# `parse_exprs`

```erlang
-spec parse_exprs(Tokens) -> {ok, ExprList} | {error, ErrorInfo}
                     when Tokens :: [token()], ExprList :: [abstract_expr()], ErrorInfo :: error_info().
```

Parses `Tokens` as if it was a list of expressions.

Returns one of the following:

- **`{ok, ExprList}`** - The parsing was successful. `ExprList` is a list of the
  abstract forms of the parsed expressions.

- **`{error, ErrorInfo}`** - An error occurred.

# `parse_form`

```erlang
-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo}
                    when Tokens :: [token()], AbsForm :: abstract_form(), ErrorInfo :: error_info().
```

Parses `Tokens` as if it was a form.

Returns one of the following:

- **`{ok, AbsForm}`** - The parsing was successful. `AbsForm` is the abstract
  form of the parsed form.

- **`{error, ErrorInfo}`** - An error occurred.

# `parse_term`

```erlang
-spec parse_term(Tokens) -> {ok, Term} | {error, ErrorInfo}
                    when Tokens :: [token()], Term :: term(), ErrorInfo :: error_info().
```

Parses `Tokens` as if it was a term.

Returns one of the following:

- **`{ok, Term}`** - The parsing was successful. `Term` is the Erlang term
  corresponding to the token list.

- **`{error, ErrorInfo}`** - An error occurred.

# `tokens`

```erlang
-spec tokens(AbsTerm) -> Tokens when AbsTerm :: abstract_expr(), Tokens :: [token()].
```

# `tokens`

```erlang
-spec tokens(AbsTerm, MoreTokens) -> Tokens
                when AbsTerm :: abstract_expr(), MoreTokens :: [token()], Tokens :: [token()].
```

Generates a list of tokens representing the abstract form `AbsTerm` of an
expression. Optionally, `MoreTokens` is appended.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
