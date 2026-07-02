# `erl_syntax_lib`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/syntax_tools/src/erl_syntax_lib.erl#L33)

Support library for abstract Erlang syntax trees.

This module contains utility functions for working with the abstract data type
defined in the module `m:erl_syntax`.

# `appFunName`
*not exported* 

```erlang
-type appFunName() :: {atom(), arity()} | {atom(), {atom(), arity()}}.
```

# `field`
*not exported* 

```erlang
-type field() :: {atom(), {field_default(), field_type()}}.
```

# `field_default`
*not exported* 

```erlang
-type field_default() :: none | syntaxTree().
```

# `field_type`
*not exported* 

```erlang
-type field_type() :: none | syntaxTree().
```

# `fields`
*not exported* 

```erlang
-type fields() :: [field()].
```

# `functionN`
*not exported* 

```erlang
-type functionN() :: atom() | {atom(), arity()}.
```

# `functionName`
*not exported* 

```erlang
-type functionName() :: functionN() | {atom(), functionN()}.
```

# `info`
*not exported* 

```erlang
-type info() :: {atom(), [{atom(), syntaxTree()}]} | {atom(), atom()} | atom().
```

# `info_pair`

```erlang
-type info_pair() :: {key(), term()}.
```

# `key`
*not exported* 

```erlang
-type key() :: attributes | errors | exports | functions | imports | module | records | warnings.
```

# `name`
*not exported* 

```erlang
-type name() :: shortname() | {atom(), shortname()}.
```

# `ordset`
*not exported* 

```erlang
-type ordset(T) :: ordsets:ordset(T).
```

# `set`
*not exported* 

```erlang
-type set(T) :: sets:set(T).
```

# `shortname`
*not exported* 

```erlang
-type shortname() :: atom() | {atom(), arity()}.
```

# `syntaxTree`
*not exported* 

```erlang
-type syntaxTree() :: erl_syntax:syntaxTree().
```

An abstract syntax tree. See the `m:erl_syntax` module for details.

# `typeName`
*not exported* 

```erlang
-type typeName() :: atom() | {module(), {atom(), arity()}} | {atom(), arity()}.
```

# `analyze_application`

```erlang
-spec analyze_application(syntaxTree()) -> appFunName() | arity().
```

Returns the name of a called function.

The result is a representation of the name of the applied function
`F/A`, if `Node` represents a function application "`F(X_1, ...,
X_A)`". If the function is not explicitly named (that is, `F` is given
by some expression), only the arity `A` is returned.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
application expression.

_See also: _`analyze_function_name/1`.

# `analyze_attribute`

```erlang
-spec analyze_attribute(syntaxTree()) -> preprocessor | {atom(), term()}.
```

Analyzes an attribute node.

If `Node` represents a preprocessor directive, the atom `preprocessor`
is returned. Otherwise, if `Node` represents a module attribute
"`-Name...`", a tuple `{Name, Info}` is returned, where `Info` depends
on `Name`, as follows:

- **`{module, Info}`** - where `Info = analyze_module_attribute(Node)`.

- **`{export, Info}`** - where `Info = analyze_export_attribute(Node)`.

- **`{import, Info}`** - where `Info = analyze_import_attribute(Node)`.

- **`{file, Info}`** - where `Info = analyze_file_attribute(Node)`.

- **`{record, Info}`** - where `Info = analyze_record_attribute(Node)`.

- **`{Name, Info}`** - where `{Name, Info} = analyze_wild_attribute(Node)`.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
module attribute.

_See also: _`analyze_export_attribute/1`, `analyze_file_attribute/1`,
`analyze_import_attribute/1`, `analyze_module_attribute/1`,
`analyze_record_attribute/1`, `analyze_wild_attribute/1`.

# `analyze_export_attribute`

```erlang
-spec analyze_export_attribute(syntaxTree()) -> [functionName()].
```

Returns the list of function names declared by an export attribute.

We do not guarantee that each name occurs at most once in the
list. The order of listing is not defined.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
export attribute.

_See also: _`analyze_attribute/1`.

# `analyze_file_attribute`

```erlang
-spec analyze_file_attribute(syntaxTree()) -> {string(), integer()}.
```

Returns the file name and line number of a `file` attribute.

The result is the pair `{File, Line}` if `Node` represents
"`-file(File, Line).`".

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
`file` attribute.

_See also: _`analyze_attribute/1`.

# `analyze_form`

```erlang
-spec analyze_form(syntaxTree()) -> {atom(), term()} | atom().
```

Analyzes a "source code form" node.

If `Node` is a "form" type (see `erl_syntax:is_form/1`), the returned
value is a tuple `{Type, Info}` where `Type` is the node type and
`Info` depends on `Type`, as follows:

- **`{attribute, Info}`** - where `Info = analyze_attribute(Node)`.

- **`{error_marker, Info}`** - where
  `Info = erl_syntax:error_marker_info(Node)`.

- **`{function, Info}`** - where `Info = analyze_function(Node)`.

- **`{warning_marker, Info}`** - where
  `Info = erl_syntax:warning_marker_info(Node)`.

For other types of forms, only the node type is returned.

The evaluation throws `syntax_error` if `Node` is not well-formed.

_See also: _`analyze_attribute/1`, `analyze_function/1`,
`erl_syntax:error_marker_info/1`, `erl_syntax:is_form/1`,
`erl_syntax:warning_marker_info/1`.

# `analyze_forms`

```erlang
-spec analyze_forms(erl_syntax:forms()) -> [info_pair()].
```

Analyzes a sequence of "program forms".

The given `Forms` may be a single syntax tree of type `form_list`, or
a list of "program form" syntax trees. The returned value is a list of
pairs `{Key, Info}`, where each value of `Key` occurs at most once in
the list; the absence of a particular key indicates that there is no
well-defined value for that key.

Each entry in the resulting list contains the following corresponding
information about the program forms:

- **`{attributes, Attributes}`**

  - `Attributes = [{atom(), term()}]`

  `Attributes` is a list of pairs representing the names and
  corresponding values of all so-called "wild" attributes (as, for
  example, "`-compile(...)`") occurring in `Forms` (see
  [`analyze_wild_attribute/1`](`analyze_wild_attribute/1`)). We do not
  guarantee that each name occurs at most once in the list. The order
  of listing is not defined.

- **`{errors, Errors}`**

  - `Errors = [term()]`

  `Errors` is the list of error descriptors of all `error_marker` nodes that
  occur in `Forms`. The order of listing is not defined.

- **`{exports, Exports}`**

  - `Exports = [FunctionName]`
  - `FunctionName = atom() | {atom(), integer()} | {ModuleName, FunctionName}`
  - `ModuleName = atom()`

  `Exports` is a list of representations of those function names that are listed
  by export declaration attributes in `Forms` (see
  [`analyze_export_attribute/1`](`analyze_export_attribute/1`)). We do not
  guarantee that each name occurs at most once in the list. The order of listing
  is not defined.

- **`{functions, Functions}`**

  - `Functions = [{atom(), integer()}]`

  `Functions` is a list of the names of the functions that are defined in
  `Forms` (see [`analyze_function/1`](`analyze_function/1`)). We do not
  guarantee that each name occurs at most once in the list. The order of listing
  is not defined.

- **`{imports, Imports}`**

  - `Imports = [{Module, Names}]`
  - `Module = atom()`
  - `Names = [FunctionName]`
  - `FunctionName = atom() | {atom(), integer()} | {ModuleName, FunctionName}`
  - `ModuleName = atom()`

  `Imports` is a list of pairs representing those module names and corresponding
  function names that are listed by import declaration attributes in `Forms`
  (see [`analyze_import_attribute/1`](`analyze_import_attribute/1`)), where each
  `Module` occurs at most once in `Imports`. We do not guarantee that each name
  occurs at most once in the lists of function names. The order of listing is
  not defined.

- **`{module, ModuleName}`**

  - `ModuleName = atom()`

  `ModuleName` is the name declared by a module attribute in `Forms`. If no
  module name is defined in `Forms`, the result will contain no entry for the
  `module` key. If multiple module name declarations should occur, all but the
  first will be ignored.

- **`{records, Records}`**

  - `Records = [{atom(), Fields}]`
  - `Fields = [{atom(), {Default, Type}}]`
  - `Default = none | syntaxTree()`
  - `Type = none | syntaxTree()`

  `Records` is a list of pairs representing the names and corresponding field
  declarations of all record declaration attributes occurring in `Forms`. For
  fields declared without a default value, the corresponding value for `Default`
  is the atom `none`. Similarly, for fields declared without a type, the
  corresponding value for `Type` is the atom `none` (see
  [`analyze_record_attribute/1`](`analyze_record_attribute/1`)). We do not
  guarantee that each record name occurs at most once in the list. The order of
  listing is not defined.

- **`{warnings, Warnings}`**

  - `Warnings = [term()]`

  `Warnings` is the list of error descriptors of all `warning_marker` nodes that
  occur in `Forms`. The order of listing is not defined.

The evaluation throws `syntax_error` if an ill-formed Erlang construct is
encountered.

_See also: _`analyze_export_attribute/1`, `analyze_function/1`,
`analyze_import_attribute/1`, `analyze_record_attribute/1`,
`analyze_wild_attribute/1`, `erl_syntax:error_marker_info/1`,
`erl_syntax:warning_marker_info/1`.

# `analyze_function`

```erlang
-spec analyze_function(syntaxTree()) -> {atom(), arity()}.
```

Returns the name and arity of a function definition.

The result is a pair `{Name, A}` if `Node` represents a function
definition "`Name(P_1, ..., P_A) -> ...`".

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
function definition.

# `analyze_function_name`

```erlang
-spec analyze_function_name(syntaxTree()) -> functionName().
```

Returns the function name represented by a syntax tree.

If `Node` represents a function name, such as "`foo/1`" or
"`bloggs:fred/2`", a uniform representation of that name is
returned. Different nestings of arity and module name qualifiers in
the syntax tree does not affect the result.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
function name.

# `analyze_implicit_fun`

```erlang
-spec analyze_implicit_fun(syntaxTree()) -> functionName().
```

Returns the name of an implicit fun expression "`fun F`".

The result is a representation of the function name `F`. (See
[`analyze_function_name/1`](`analyze_function_name/1`).)

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
implicit fun.

_See also: _`analyze_function_name/1`.

# `analyze_import_attribute`

```erlang
-spec analyze_import_attribute(syntaxTree()) -> {atom(), [functionName()]} | atom().
```

Returns the module name and (if present) list of function names declared by an
import attribute.

The returned value is an atom `Module` or a pair `{Module, Names}`,
where `Names` is a list of function names declared as imported from
the module named by `Module`. We do not guarantee that each name
occurs at most once in `Names`. The order of listing is not defined.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
import attribute.

_See also: _`analyze_attribute/1`.

# `analyze_module_attribute`

```erlang
-spec analyze_module_attribute(syntaxTree()) -> atom() | {atom(), [atom()]}.
```

Returns the module name and possible parameters declared by a module attribute.

If the attribute is a plain module declaration such as `-module(name)`, the
result is the module name. If the attribute is a parameterized module
declaration, the result is a tuple containing the module name and a list of the
parameter variable names.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
module attribute.

_See also: _`analyze_attribute/1`.

# `analyze_record_attribute`

```erlang
-spec analyze_record_attribute(syntaxTree()) -> {atom(), fields()}.
```

Returns the name and the list of fields of a record declaration attribute.

The result is a pair `{Name, Fields}`, if `Node` represents
"`-record(Name, {...}).`", where `Fields` is a list of pairs
`{Label, {Default, Type}}` for each field "`Label`", "`Label = Default`",
"`Label :: Type`", or "`Label = Default :: Type`" in the declaration, listed in
left-to-right order. If the field has no default-value declaration, the value
for `Default` will be the atom `none`. If the field has no type declaration, the
value for `Type` will be the atom `none`. We do not guarantee that each label
occurs at most once in the list.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
record declaration attribute.

_See also: _`analyze_attribute/1`, `analyze_record_field/1`.

# `analyze_record_expr`

```erlang
-spec analyze_record_expr(syntaxTree()) -> {atom(), info()} | atom().
```

Returns the record name and field name/names of a record expression.

If `Node` has type `record_expr`, `record_index_expr` or
`record_access`, a pair `{Type, Info}` is returned, otherwise an atom
`Type` is returned. `Type` is the node type of `Node`, and `Info`
depends on `Type`, as follows:

- **`record_expr`:** - `{atom(), [{atom(), Value}]}`

- **`record_access`:** - `{atom(), atom()}`

- **`record_index_expr`:** - `{atom(), atom()}`

For a `record_expr` node, `Info` represents the record name and the list of
descriptors for the involved fields, listed in the order they appear. A field
descriptor is a pair `{Label, Value}`, if `Node` represents "`Label = Value`".
For a `record_access` node, `Info` represents the record name and the field
name. For a `record_index_expr` node, `Info` represents the record name and the
name field name.

The evaluation throws `syntax_error` if `Node` represents a record expression
that is not well-formed.

_See also: _`analyze_record_attribute/1`, `analyze_record_field/1`.

# `analyze_record_field`

```erlang
-spec analyze_record_field(syntaxTree()) -> field().
```

Returns the label, value-expression, and type of a record field specifier.

The result is a pair `{Label, {Default, Type}}`, if `Node` represents
"`Label`", "`Label = Default`", "`Label :: Type`", or "`Label =
Default :: Type`". If the field has no value-expression, the value for
`Default` will be the atom `none`.  If the field has no type, the
value for `Type` will be the atom `none`.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
record field specifier.

_See also: _`analyze_record_attribute/1`, `analyze_record_expr/1`.

# `analyze_type_application`

```erlang
-spec analyze_type_application(syntaxTree()) -> typeName().
```

Returns the name of a used type.

The result is a representation of the name of the used pre-defined or
local type `N/A`, if `Node` represents a local (user) type application
"`N(T_1, ..., T_A)`", or a representation of the name of the used
remote type `M:N/A` if `Node` represents a remote user type
application "`M:N(T_1, ..., T_A)`".

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
(user) type application expression.

_See also: _`analyze_type_name/1`.

# `analyze_type_name`

```erlang
-spec analyze_type_name(syntaxTree()) -> typeName().
```

Returns the type name represented by a syntax tree.

If `Node` represents a type name, such as "`foo/1`" or
"`bloggs:fred/2`", a uniform representation of that name is returned.

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
type name.

# `analyze_wild_attribute`

```erlang
-spec analyze_wild_attribute(syntaxTree()) -> {atom(), term()}.
```

Returns the name and value of a "wild" attribute.

The result is the pair `{Name, Value}`, if `Node` represents
"`-Name(Value)`".

Note that no checking is done whether `Name` is a reserved attribute name such
as `module` or `export`: it is assumed that the attribute is "wild".

The evaluation throws `syntax_error` if `Node` does not represent a well-formed
wild attribute.

_See also: _`analyze_attribute/1`.

# `annotate_bindings`

```erlang
-spec annotate_bindings(syntaxTree()) -> syntaxTree().
```

Adds or updates annotations on nodes in a syntax tree.

Equivalent to [`annotate_bindings(Tree,
Bindings)`](`annotate_bindings/2`) where the top-level environment
`Bindings` is taken from the annotation `{env, Bindings}` on the root
node of `Tree`. An exception is thrown if no such annotation should
exist.

_See also: _`annotate_bindings/2`.

# `annotate_bindings`

```erlang
-spec annotate_bindings(syntaxTree(), ordset(atom())) -> syntaxTree().
```

Adds or updates annotations on nodes in a syntax tree.

`Bindings` specifies the set of bound variables in the environment of
the top level node. The following annotations are affected:

- `{env, Vars}`, representing the input environment of the subtree.
- `{bound, Vars}`, representing the variables that are bound in the subtree.
- `{free, Vars}`, representing the free variables in the subtree.

`Bindings` and `Vars` are ordered-set lists (see module `m:ordsets`) of atoms
representing variable names.

_See also: _[//stdlib/ordsets](`m:ordsets`), `annotate_bindings/1`.

# `fold`

```erlang
-spec fold(fun((syntaxTree(), term()) -> term()), term(), syntaxTree()) -> term().
```

Folds a function over all nodes of a syntax tree.

The result is the value of `Function(X1, Function(X2, ... Function(Xn,
Start) ... ))`, where `[X1, X2, ..., Xn]` are the nodes of `Tree` in a
post-order traversal.

_See also: _`fold_subtrees/3`, `foldl_listlist/3`.

# `fold_subtrees`

```erlang
-spec fold_subtrees(fun((syntaxTree(), term()) -> term()), term(), syntaxTree()) -> term().
```

Folds a function over the immediate subtrees of a syntax tree.

This is similar to [`fold/3`](`fold/3`), but only on the immediate
subtrees of `Tree`, in left-to-right order; it does not include the
root node of `Tree`.

_See also: _`fold/3`.

# `foldl_listlist`

```erlang
-spec foldl_listlist(fun((term(), term()) -> term()), term(), [[term()]]) -> term().
```

Like `lists:foldl/3`, but over a list of lists.

_See also: _[//stdlib/lists:foldl/3](`lists:foldl/3`), `fold/3`.

# `function_name_expansions`

```erlang
-spec function_name_expansions([name()]) -> [{shortname(), name()}].
```

Creates a mapping from corresponding short names to full function names.

Names are represented by nested tuples of atoms and integers (see
[`analyze_function_name/1`](`analyze_function_name/1`)). The result is
a list containing a pair `{ShortName, Name}` for each element `Name`
in the given list, where the corresponding `ShortName` is the
rightmost-innermost part of `Name`.  The list thus represents a finite
mapping from unqualified names to the corresponding qualified names.

Note that the resulting list can contain more than one tuple
`{ShortName, Name}` for the same `ShortName`, possibly with different
values for `Name`, depending on the given list.

_See also: _`analyze_function_name/1`.

# `is_fail_expr`

```erlang
-spec is_fail_expr(syntaxTree()) -> boolean().
```

Returns `true` if `Tree` represents an expression that never terminates
normally.

Note that the reverse does not apply. Currently, the detected cases
are calls to [`exit/1`](`exit/1`), [`throw/1`](`throw/1`),
`erlang:error/1` and `erlang:error/2`.

_See also: _[//erts/erlang:error/1](`erlang:error/1`),
[//erts/erlang:error/2](`erlang:error/2`),
[//erts/erlang:exit/1](`erlang:exit/1`),
[//erts/erlang:throw/1](`erlang:throw/1`).

# `limit`

```erlang
-spec limit(syntaxTree(), integer()) -> syntaxTree().
```

Equivalent to [`limit(Tree, Depth, Text)`](`limit/3`) using the text `"..."` as
default replacement.

_See also: _`limit/3`, `erl_syntax:text/1`.

# `limit`

```erlang
-spec limit(syntaxTree(), integer(), syntaxTree()) -> syntaxTree().
```

Limits a syntax tree to a specified depth.

Replaces all non-leaf subtrees in
`Tree` at the given `Depth` by `Node`. If `Depth` is negative, the result is
always `Node`, even if `Tree` has no subtrees.

When a group of subtrees (as, for example, the argument list of an
`application` node) is at the specified depth, and there are two or
more subtrees in the group, these will be collectively replaced by
`Node` even if they are leaf nodes.  Groups of subtrees that are above
the specified depth will be limited in size, as if each subsequent
tree in the group were one level deeper than the previous. For example,
if `Tree` represents a list of integers "`[1, 2, 3, 4, 5, 6, 7, 8, 9,
10]`", the result of [`limit(Tree, 5)`](`limit/2`) will represent `[1,
2, 3, 4, ...]`.

The resulting syntax tree is typically only useful for pretty-printing or
similar visual formatting.

_See also: _`limit/2`.

# `map`

```erlang
-spec map(fun((syntaxTree()) -> syntaxTree()), syntaxTree()) -> syntaxTree().
```

Applies a function to each node of a syntax tree.

The result of each application replaces the corresponding original
node. The order of traversal is bottom-up.

_See also: _`map_subtrees/2`.

# `map_subtrees`

```erlang
-spec map_subtrees(fun((syntaxTree()) -> syntaxTree()), syntaxTree()) -> syntaxTree().
```

Applies a function to each immediate subtree of a syntax tree.

The result of each application replaces the corresponding original
node.

_See also: _`map/2`.

# `mapfold`

```erlang
-spec mapfold(fun((syntaxTree(), term()) -> {syntaxTree(), term()}), term(), syntaxTree()) ->
                 {syntaxTree(), term()}.
```

Combines map and fold in a single operation.

This is similar to [`map/2`](`map/2`), but also propagates an extra
value from each application of the `Function` to the next, while doing
a post-order traversal of the tree like [`fold/3`](`fold/3`). The
value `Start` is passed to the first function application, and the
final result is the result of the last application.

_See also: _`fold/3`, `map/2`.

# `mapfold_subtrees`

```erlang
-spec mapfold_subtrees(fun((syntaxTree(), term()) -> {syntaxTree(), term()}), term(), syntaxTree()) ->
                          {syntaxTree(), term()}.
```

Does a mapfold operation over the immediate subtrees of a syntax tree.

This is similar to [`mapfold/3`](`mapfold/3`), but only on the
immediate subtrees of `Tree`, in left-to-right order; it does not
include the root node of `Tree`.

_See also: _`mapfold/3`.

# `mapfoldl_listlist`

```erlang
-spec mapfoldl_listlist(fun((term(), term()) -> {term(), term()}), term(), [[term()]]) ->
                           {[[term()]], term()}.
```

Like `lists:mapfoldl/3`, but over a list of lists.

The list of lists in the result has the same structure as the given
list of lists.

# `new_variable_name`

```erlang
-spec new_variable_name(set(atom())) -> atom().
```

Returns an atom which is not already in the set `Used`.

This is equivalent to [`new_variable_name(Function,
Used)`](`new_variable_name/2`), where `Function` maps a given integer
`N` to the atom whose name consists of "`V`" followed by the numeral
for `N`.

_See also: _`new_variable_name/2`.

# `new_variable_name`

```erlang
-spec new_variable_name(fun((integer()) -> atom()), set(atom())) -> atom().
```

Returns a user-named atom which is not already in the set `Used`.

The atom is generated by applying the given `Function` to a generated
integer. Integers are generated using an algorithm which tries to keep
the names randomly distributed within a reasonably small range
relative to the number of elements in the set.

This function uses the module `m:rand` to generate new keys. The seed it uses can
be initialized by calling `rand:seed/1` or `rand:seed/2` before this function is
first called.

_See also: _[//stdlib/rand](`m:rand`), [//stdlib/sets](`m:sets`),
`new_variable_name/1`.

# `new_variable_names`

```erlang
-spec new_variable_names(integer(), set(atom())) -> [atom()].
```

Like [`new_variable_name/1`](`new_variable_name/1`), but generates a list of `N`
new names.

_See also: _`new_variable_name/1`.

# `new_variable_names`

```erlang
-spec new_variable_names(integer(), fun((integer()) -> atom()), set(atom())) -> [atom()].
```

Like [`new_variable_name/2`](`new_variable_name/2`), but generates a list of `N`
new names.

_See also: _`new_variable_name/2`.

# `strip_comments`

```erlang
-spec strip_comments(syntaxTree()) -> syntaxTree().
```

Removes all comments from all nodes of a syntax tree.

All other attributes (such as position information) remain
unchanged. Standalone comments in form lists are removed; any other
standalone comments are changed into null-comments (no text, no
indentation).

# `to_comment`

```erlang
-spec to_comment(syntaxTree()) -> syntaxTree().
```

# `to_comment`

```erlang
-spec to_comment(syntaxTree(), string()) -> syntaxTree().
```

Equivalent to [`to_comment(Tree, Prefix, F)`](`to_comment/3`) for a default
formatting function `F`.

The default `F` simply calls `erl_prettypr:format/1`.

_See also: _`to_comment/3`, `erl_prettypr:format/1`.

# `to_comment`

```erlang
-spec to_comment(syntaxTree(), string(), fun((syntaxTree()) -> string())) -> syntaxTree().
```

Transforms a syntax tree into an abstract comment.

The lines of the comment contain the text for `Node`, as produced by
the given `Printer` function. Each line of the comment is prefixed by
the string `Prefix` (this does not include the initial "`%`" character
of the comment line).

For example, the result of
[`to_comment(erl_syntax:abstract([a,b,c]))`](`to_comment/1`) represents

```erlang
%% [a,b,c]
```

(see [`to_comment/1`](`to_comment/1`)).

> #### Note {: .info }
>
> The text returned by the formatting function will be split
> automatically into separate comment lines at each line break. No extra
> work is needed.

_See also: _`to_comment/1`, `to_comment/2`.

# `variables`

```erlang
-spec variables(syntaxTree()) -> set(atom()).
```

Returns the names of variables occurring in a syntax tree.

The result is a set of variable names represented by atoms. Macro
names are not included.

_See also: _[//stdlib/sets](`m:sets`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
