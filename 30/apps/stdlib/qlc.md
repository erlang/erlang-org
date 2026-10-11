# `qlc`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/qlc.erl#L22)

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
This module provides a query interface to [Mnesia](`m:mnesia`), [ETS](`m:ets`),
[Dets](`m:dets`), and other data structures that provide an iterator style
traversal of objects.

## Overview

This module provides a query interface to _QLC tables_. Typical QLC tables are
Mnesia, ETS, and Dets tables. Support is also provided for user-defined tables,
see section [Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`). [](){:
#query_list_comprehension } A _query_ is expressed using _Query List
Comprehensions_ (QLCs). The answers to a query are determined by data in QLC
tables that fulfill the constraints expressed by the QLCs of the query. QLCs are
similar to ordinary list comprehensions as described in
[Erlang Reference Manual](`e:system:expressions.md#lcs`) and
[Programming Examples](`e:system:list_comprehensions.md`), except that variables
introduced in patterns cannot be used in list expressions. In the absence of
optimizations and options such as `cache` and `unique` (see section
[Common Options](`m:qlc#common_options`), every QLC free of QLC tables evaluates
to the same list of answers as the identical ordinary list comprehension.

While ordinary list comprehensions evaluate to lists, calling [`q/1,2`](`q/1`)
returns a _query handle_{: #query_handle }. To obtain all the answers to a
query, [`eval/1,2`](`eval/1`) is to be called with the query handle as first
argument. Query handles are essentially functional objects (funs) created in the
module calling `q/1,2`. As the funs refer to the module code, be careful not to
keep query handles too long if the module code is to be replaced. Code
replacement is described in section
[Compilation and Code Loading](`e:system:code_loading.md`) in the Erlang
Reference Manual. The list of answers can also be traversed in chunks by use of
a _query cursor_{: #query_cursor }. Query cursors are created by calling
[`cursor/1,2`](`cursor/1`) with a query handle as first argument. Query cursors
are essentially Erlang processes. One answer at a time is sent from the query
cursor process to the process that created the cursor.

## Syntax

Syntactically QLCs have the same parts as ordinary list comprehensions:

```text
[Expression || Qualifier1, Qualifier2, ...]
```

`Expression` (the _template_) is any Erlang expression. Qualifiers are either
_filters_ or _generators_. Filters are Erlang expressions returning
`t:boolean/0`. Generators have the form `Pattern <- ListExpression`, where
`ListExpression` is an expression evaluating to a query handle or a list. Query
handles are returned from [`append/1,2`](`append/1`),
[`keysort/2,3`](`keysort/2`), [`q/1,2`](`q/1`), [`sort/1,2`](`sort/1`),
[`string_to_handle/1,2,3`](`string_to_handle/1`), and `table/2`.

## Evaluation

A query handle is evaluated in the following order:

- Inspection of options and the collection of information about tables. As a
  result, qualifiers are modified during the optimization phase.
- All list expressions are evaluated. If a cursor has been created, evaluation
  takes place in the cursor process. For list expressions that are QLCs, the
  list expressions of the generators of the QLCs are evaluated as well. Be
  careful if list expressions have side effects, as list expressions are
  evaluated in unspecified order.
- The answers are found by evaluating the qualifiers from left to right,
  backtracking when some filter returns `false`, or collecting the template when
  all filters return `true`.

Filters that do not return `t:boolean/0` but fail are handled differently
depending on their syntax: if the filter is a guard, it returns `false`,
otherwise the query evaluation fails. This behavior makes it possible for the
`qlc` module to do some optimizations without affecting the meaning of a query.
For example, when testing some position of a table and one or more constants for
equality, only the objects with equal values are candidates for further
evaluation. The other objects are guaranteed to make the filter return `false`,
but never fail. The (small) set of candidate objects can often be found by
looking up some key values of the table or by traversing the table using a match
specification. It is necessary to place the guard filters immediately after the
table generator, otherwise the candidate objects are not restricted to a small
set. The reason is that objects that could make the query evaluation fail must
not be excluded by looking up a key or running a match specification.

## Join

The `qlc` module supports fast join of two query handles. Fast join is possible
if some position `P1` of one query handler and some position `P2` of another
query handler are tested for equality. Two fast join methods are provided:

- _Lookup join_ traverses all objects of one query handle and finds objects of
  the other handle (a QLC table) such that the values at `P1` and `P2` match or
  compare equal. The `qlc` module does not create any indexes but looks up
  values using the key position and the indexed positions of the QLC table.
- _Merge join_ sorts the objects of each query handle if necessary and filters
  out objects where the values at `P1` and `P2` do not compare equal. If many
  objects with the same value of `P2` exist, a temporary file is used for the
  equivalence classes.

The `qlc` module warns at compile time if a QLC combines query handles in such a
way that more than one join is possible. That is, no query planner is provided
that can select a good order between possible join operations. It is up to the
user to order the joins by introducing query handles.

The join is to be expressed as a guard filter. The filter must be placed
immediately after the two joined generators, possibly after guard filters that
use variables from no other generators but the two joined generators. The `qlc`
module inspects the operands of `=:=/2`, `==/2`, [`is_record/2`](`is_record/2`),
[`element/2`](`element/2`), and logical operators (`and/2`, `or/2`, `andalso/2`,
`orelse/2`, `xor/2`) when determining which joins to consider.

[](){: #common_options }

## Common Options

The following options are accepted by `cursor/2`, `eval/2`, `fold/4`, and
`info/2`:

- `{cache_all, Cache}`, where `Cache` is equal to `ets` or `list` adds a
  `{cache, Cache}` option to every list expression of the query except tables
  and lists. Defaults to `{cache_all, no}`. Option `cache_all` is equivalent to
  `{cache_all, ets}`.
- `{max_list_size, MaxListSize}`{: #max_list_size }, where `MaxListSize` is the
  size in bytes of terms on the external format. If the accumulated size of
  collected objects exceeds `MaxListSize`, the objects are written onto a
  temporary file. This option is used by option `{cache, list}` and by the merge
  join method. Defaults to 512\*1024 bytes.
- `{tmpdir_usage, TmpFileUsage}` determines the action taken when `qlc` is about
  to create temporary files on the directory set by option `tmpdir`. If the
  value is `not_allowed`, an error tuple is returned, otherwise temporary files
  are created as needed. Default is `allowed`, which means that no further
  action is taken. The values `info_msg`, `warning_msg`, and `error_msg` mean
  that the function with the corresponding name in module `m:error_logger` is
  called for printing some information (currently the stacktrace).
- `{tmpdir, TempDirectory}` sets the directory used by merge join for temporary
  files and by option `{cache, list}`. The option also overrides option `tmpdir`
  of `keysort/3` and `sort/2`. Defaults to `""`, which means that the directory
  returned by `file:get_cwd()` is used.
- `{unique_all, true}` adds a `{unique, true}` option to every list expression
  of the query. Defaults to `{unique_all, false}`. Option `unique_all` is
  equivalent to `{unique_all, true}`.

[](){: #getting_started }

## Getting Started

As mentioned earlier, queries are expressed in the list comprehension syntax as
described in section [Expressions](`e:system:expressions.md`) in Erlang
Reference Manual. In the following, some familiarity with list comprehensions is
assumed. The examples in section
[List Comprehensions](`e:system:list_comprehensions.md`) in Programming Examples
can get you started. Notice that list comprehensions do not add any
computational power to the language; anything that can be done with list
comprehensions can also be done without them. But they add syntax for expressing
simple search problems, which is compact and clear once you get used to it.

Many list comprehension expressions can be evaluated by the `qlc` module.
Exceptions are expressions, such that variables introduced in patterns (or
filters) are used in some generator later in the list comprehension. As an
example, consider an implementation of `lists:append(L)`:
`[X ||Y <- L, X <- Y]`. `Y` is introduced in the first generator and used in the
second. The ordinary list comprehension is normally to be preferred when there
is a choice as to which to use. One difference is that [`eval/1,2`](`eval/1`)
collects answers in a list that is finally reversed, while list comprehensions
collect answers on the stack that is finally unwound.

What the `qlc` module primarily adds to list comprehensions is that data can be
read from QLC tables in small chunks. A QLC table is created by calling
[`qlc:table/2`](`table/2`). Usually `qlc:table/2` is not called directly from
the query but through an interface function of some data structure. Erlang/OTP
includes a few examples of such functions:
[`mnesia:table/1,2`](`mnesia:table/1`), [`ets:table/1,2`](`ets:table/1`), and
[`dets:table/1,2`](`dets:table/1`). For a given data structure, many functions
can create QLC tables, but common for these functions is that they return a
query handle created by [`qlc:table/2`](`table/2`). Using the QLC tables
provided by Erlang/OTP is usually probably sufficient, but for the more advanced
user section [Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`)
describes the implementation of a function calling `qlc:table/2`.

Besides `qlc:table/2`, other functions return query handles. They are used more
seldom than tables, but are sometimes useful. [`qlc:append/1,2`](`append/1`)
traverses objects from many tables or lists after each other. If, for example,
you want to traverse all answers to a query `QH` and then finish off by a term
`{finished}`, you can do that by calling `qlc:append(QH, [{finished}])`.
[`append/2`](`append/2`) first returns all objects of `QH`, then `{finished}`.
If a tuple `{finished}` exists among the answers to `QH`, it is returned twice
from [`append/2`](`append/2`).

As another example, consider concatenating the answers to two queries `QH1` and
`QH2` while removing all duplicates. This is accomplished by using option
`unique`:

```erlang
qlc:q([X || X <- qlc:append(QH1, QH2)], {unique, true})
```

The cost is substantial: every returned answer is stored in an ETS table. Before
returning an answer, it is looked up in the ETS table to check if it has already
been returned. Without the `unique` option, all answers to `QH1` would be
returned followed by all answers to `QH2`. The `unique` option keeps the order
between the remaining answers.

If the order of the answers is not important, there is an alternative to the
`unique` option, namely to sort the answers uniquely:

```erlang
qlc:sort(qlc:q([X || X <- qlc:append(QH1, QH2)], {unique, true})).
```

This query also removes duplicates but the answers are sorted. If there are many
answers, temporary files are used. Notice that to get the first unique answer,
all answers must be found and sorted. Both alternatives find duplicates by
comparing answers, that is, if `A1` and `A2` are answers found in that order,
then `A2` is a removed if `A1 == A2`.

To return only a few answers, cursors can be used. The following code returns no
more than five answers using an ETS table for storing the unique answers:

```erlang
C = qlc:cursor(qlc:q([X || X <- qlc:append(QH1, QH2)],{unique,true})),
R = qlc:next_answers(C, 5),
ok = qlc:delete_cursor(C),
R.
```

QLCs are convenient for stating constraints on data from two or more tables. The
following example does a natural join on two query handles on position 2:

```erlang
qlc:q([{X1,X2,X3,Y1} ||
          {X1,X2,X3} <- QH1,
          {Y1,Y2} <- QH2,
          X2 =:= Y2])
```

The `qlc` module evaluates this differently depending on the query handles `QH1`
and `QH2`. If, for example, `X2` is matched against the key of a QLC table, the
lookup join method traverses the objects of `QH2` while looking up key values in
the table. However, if not `X2` or `Y2` is matched against the key or an indexed
position of a QLC table, the merge join method ensures that `QH1` and `QH2` are
both sorted on position 2 and next do the join by traversing the objects one by
one.

Option `join` can be used to force the `qlc` module to use a certain join
method. For the rest of this section it is assumed that the excessively slow
join method called "nested loop" has been chosen:

```erlang
qlc:q([{X1,X2,X3,Y1} ||
          {X1,X2,X3} <- QH1,
          {Y1,Y2} <- QH2,
          X2 =:= Y2],
      {join, nested_loop})
```

In this case the filter is applied to every possible pair of answers to `QH1`
and `QH2`, one at a time. If there are M answers to `QH1` and N answers to
`QH2`, the filter is run M\*N times.

If `QH2` is a call to the function for `m:gb_trees`, as defined in section
[Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`), then
[`gb_table:table/1` ](`m:qlc#gb_table`), the iterator for the gb-tree is
initiated for each answer to `QH1`. The objects of the gb-tree are then returned
one by one. This is probably the most efficient way of traversing the table in
that case, as it takes minimal computational power to get the following object.
But if `QH2` is not a table but a more complicated QLC, it can be more efficient
to use some RAM memory for collecting the answers in a cache, particularly if
there are only a few answers. It must then be assumed that evaluating `QH2` has
no side effects so that the meaning of the query does not change if `QH2` is
evaluated only once. One way of caching the answers is to evaluate `QH2` first
of all and substitute the list of answers for `QH2` in the query. Another way is
to use option `cache`. It is expressed like this:

```erlang
QH2' = qlc:q([X || X <- QH2], {cache, ets})
```

or only

```text
QH2' = qlc:q([X || X <- QH2], cache)
```

The effect of option `cache` is that when generator `QH2'` is run the first
time, every answer is stored in an ETS table. When the next answer of `QH1` is
tried, answers to `QH2'` are copied from the ETS table, which is very fast. As
for option `unique` the cost is a possibly substantial amount of RAM memory.

Option `{cache, list}` offers the possibility to store the answers in a list on
the process heap. This has the potential of being faster than ETS tables, as
there is no need to copy answers from the table. However, it can often result in
slower evaluation because of more garbage collections of the process heap and
increased RAM memory consumption because of larger heaps. Another drawback with
cache lists is that if the list size exceeds a limit, a temporary file is used.
Reading the answers from a file is much slower than copying them from an ETS
table. But if the available RAM memory is scarce, setting the
[limit](`m:qlc#max_list_size`) to some low value is an alternative.

Option `cache_all` can be set to `ets` or `list` when evaluating a query. It
adds a `cache` or `{cache, list}` option to every list expression except QLC
tables and lists on all levels of the query. This can be used for testing if
caching would improve efficiency at all. If the answer is yes, further testing
is needed to pinpoint the generators that are to be cached.

[](){: #implementing_a_qlc_table }

## Implementing a QLC Table

As an example of how to use function `table/2`, the implementation of a QLC
table for the `m:gb_trees` module is given:

[](){: #gb_table }

```erlang
-module(gb_table).

-export([table/1]).

table(T) ->
    TF = fun() -> qlc_next(gb_trees:next(gb_trees:iterator(T))) end,
    InfoFun = fun(num_of_objects) -> gb_trees:size(T);
                 (keypos) -> 1;
                 (is_sorted_key) -> true;
                 (is_unique_objects) -> true;
                 (_) -> undefined
              end,
    LookupFun =
        fun(1, Ks) ->
                lists:flatmap(fun(K) ->
                                      case gb_trees:lookup(K, T) of
                                          {value, V} -> [{K,V}];
                                          none -> []
                                      end
                              end, Ks)
        end,
    FormatFun =
        fun({all, NElements, ElementFun}) ->
                ValsS = io_lib:format("gb_trees:from_orddict(~w)",
                                      [gb_nodes(T, NElements, ElementFun)]),
                io_lib:format("gb_table:table(~s)", [ValsS]);
           ({lookup, 1, KeyValues, _NElements, ElementFun}) ->
                ValsS = io_lib:format("gb_trees:from_orddict(~w)",
                                      [gb_nodes(T, infinity, ElementFun)]),
                io_lib:format("lists:flatmap(fun(K) -> "
                              "case gb_trees:lookup(K, ~s) of "
                              "{value, V} -> [{K,V}];none -> [] end "
                              "end, ~w)",
                              [ValsS, [ElementFun(KV) || KV <- KeyValues]])
        end,
    qlc:table(TF, [{info_fun, InfoFun}, {format_fun, FormatFun},
                   {lookup_fun, LookupFun},{key_equality,'=='}]).

qlc_next({X, V, S}) ->
    [{X,V} | fun() -> qlc_next(gb_trees:next(S)) end];
qlc_next(none) ->
    [].

gb_nodes(T, infinity, ElementFun) ->
    gb_nodes(T, -1, ElementFun);
gb_nodes(T, NElements, ElementFun) ->
    gb_iter(gb_trees:iterator(T), NElements, ElementFun).

gb_iter(_I, 0, _EFun) ->
    '...';
gb_iter(I0, N, EFun) ->
    case gb_trees:next(I0) of
        {X, V, I} ->
            [EFun({X,V}) | gb_iter(I, N-1, EFun)];
        none ->
            []
    end.
```

`TF` is the traversal function. The `qlc` module requires that there is a way of
traversing all objects of the data structure. `gb_trees` has an iterator
function suitable for that purpose. Notice that for each object returned, a new
fun is created. As long as the list is not terminated by `[]`, it is assumed
that the tail of the list is a nullary function and that calling the function
returns further objects (and functions).

The lookup function is optional. It is assumed that the lookup function always
finds values much faster than it would take to traverse the table. The first
argument is the position of the key. As `qlc_next/1` returns the objects as
`{Key, Value}` pairs, the position is 1. Notice that the lookup function is to
return `{Key, Value}` pairs, as the traversal function does.

The format function is also optional. It is called by [`info/1,2`](`info/1`) to
give feedback at runtime of how the query is to be evaluated. Try to give as
good feedback as possible without showing too much details. In the example, at
most seven objects of the table are shown. The format function handles two
cases: `all` means that all objects of the table are traversed;
`{lookup, 1, KeyValues}` means that the lookup function is used for looking up
key values.

Whether the whole table is traversed or only some keys looked up depends on how
the query is expressed. If the query has the form

```text
qlc:q([T || P <- LE, F])
```

and `P` is a tuple, the `qlc` module analyzes `P` and `F` in compile time to
find positions of tuple `P` that are tested for equality to constants. If such a
position at runtime turns out to be the key position, the lookup function can be
used, otherwise all objects of the table must be traversed. The info function
`InfoFun` returns the key position. There can be indexed positions as well, also
returned by the info function. An index is an extra table that makes lookup on
some position fast. Mnesia maintains indexes upon request, and introduces so
called secondary keys. The `qlc` module prefers to look up objects using the key
before secondary keys regardless of the number of constants to look up.

## Key Equality

Erlang/OTP has two operators for testing term equality: `==/2` and `=:=/2`. The
difference is all about the integers that can be represented by floats. For
example, `2 == 2.0` evaluates to `true` while `2 =:= 2.0` evaluates to `false`.
Normally this is a minor issue, but the `qlc` module cannot ignore the
difference, which affects the user's choice of operators in QLCs.

If the `qlc` module at compile time can determine that some constant is free of
integers, it does not matter which one of `==/2` or `=:=/2` is used:

```erlang
1> E1 = ets:new(t, [set]), % uses =:=/2 for key equality
Q1 = qlc:q([K ||
{K} <- ets:table(E1),
K == 2.71 orelse K == a]),
io:format("~s~n", [qlc:info(Q1)]).
ets:match_spec_run(
       lists:flatmap(fun(V) ->
			    ets:lookup(#Ref<0.3098908599.2283929601.256025>,
				       V)
		     end,
		     [a, 2.71]),
       ets:match_spec_compile([{{'$1'}, [], ['$1']}]))
```

In the example, operator `==/2` has been handled exactly as `=:=/2` would have
been handled. However, if it cannot be determined at compile time that some
constant is free of integers, and the table uses `=:=/2` when comparing keys for
equality (see option [key_equality](`m:qlc#key_equality`)), then the `qlc`
module does not try to look up the constant. The reason is that there is in the
general case no upper limit on the number of key values that can compare equal
to such a constant; every combination of integers and floats must be looked up:

```erlang
2> E2 = ets:new(t, [set]),
true = ets:insert(E2, [{{2,2},a},{{2,2.0},b},{{2.0,2},c}]),
F2 = fun(I) ->
qlc:q([V || {K,V} <- ets:table(E2), K == I])
end,
Q2 = F2({2,2}),
io:format("~s~n", [qlc:info(Q2)]).
ets:table(#Ref<0.3098908599.2283929601.256125>,
          [{traverse,
            {select,
             [{{'$1', '$2'}, [{'==', '$1', {const, {2, 2}}}], ['$2']}]}}])
3> lists:sort(qlc:e(Q2)).
[a,b,c]
```

Looking up only `{2,2}` would not return `b` and `c`.

If the table uses `==/2` when comparing keys for equality, the `qlc` module
looks up the constant regardless of which operator is used in the QLC. However,
`==/2` is to be preferred:

```erlang
4> E3 = ets:new(t, [ordered_set]), % uses ==/2 for key equality
true = ets:insert(E3, [{{2,2.0},b}]),
F3 = fun(I) ->
qlc:q([V || {K,V} <- ets:table(E3), K == I])
end,
Q3 = F3({2,2}),
io:format("~s~n", [qlc:info(Q3)]).
ets:match_spec_run(ets:lookup(#Ref<0.3098908599.2283929601.256211>,
                              {2, 2}),
                   ets:match_spec_compile([{{'$1', '$2'}, [], ['$2']}]))
5> qlc:e(Q3).
[b]
```

Lookup join is handled analogously to lookup of constants in a table: if the
join operator is `==/2`, and the table where constants are to be looked up uses
`=:=/2` when testing keys for equality, then the `qlc` module does not consider
lookup join for that table.

### See Also

`m:dets`, `m:erl_eval`, `m:erlang`, `m:error_logger`, `m:ets`, `m:file`,
`m:file_sorter`, `m:mnesia`, `m:shell`,
[Erlang Reference Manual](`e:system:index.html`),
[Programming Examples](`e:system:index.html`)

# `abstract_expr`
*not exported* 

```erlang
-type abstract_expr() :: erl_parse:abstract_expr().
```

Parse trees for Erlang expression, see section
[The Abstract Format](`e:erts:absform.md`) in the ERTS User's Guide.

# `answer`
*not exported* 

```erlang
-type answer() :: term().
```

# `answers`
*not exported* 

```erlang
-type answers() :: [answer()].
```

# `cache`
*not exported* 

```erlang
-type cache() :: ets | list | no.
```

# `key_pos`
*not exported* 

```erlang
-type key_pos() :: pos_integer() | [pos_integer()].
```

# `match_expression`
*not exported* 

```erlang
-type match_expression() :: ets:match_spec().
```

Match specification, see section
[Match Specifications in Erlang](`e:erts:match_spec.md`) in the ERTS User's
Guide and `m:ms_transform`.

# `max_list_size`
*not exported* 

```erlang
-type max_list_size() :: non_neg_integer().
```

# `no_files`
*not exported* 

```erlang
-type no_files() :: pos_integer().
```

An integer > 1.

# `order`
*not exported* 

```erlang
-type order() :: ascending | descending | order_fun().
```

# `order_fun`
*not exported* 

```erlang
-type order_fun() :: fun((term(), term()) -> boolean()).
```

# `query_cursor`

```erlang
-opaque query_cursor()
```

A [query cursor](`m:qlc#query_cursor`).

# `query_handle`

```erlang
-opaque query_handle()
```

A [query handle](`m:qlc#query_handle`).

# `query_handle_or_list`
*not exported* 

```erlang
-type query_handle_or_list() :: query_handle() | list().
```

# `query_list_comprehension`
*not exported* 

```erlang
-type query_list_comprehension() :: term().
```

A literal [query list comprehension](`m:qlc#query_list_comprehension`).

# `sort_option`
*not exported* 

```erlang
-type sort_option() ::
          {compressed, boolean()} |
          {no_files, no_files()} |
          {order, order()} |
          {size, pos_integer()} |
          {tmpdir, tmp_directory()} |
          {unique, boolean()}.
```

See `m:file_sorter` for a description of the options.

# `sort_options`
*not exported* 

```erlang
-type sort_options() :: [sort_option()] | sort_option().
```

# `spawn_options`
*not exported* 

```erlang
-type spawn_options() :: default | [proc_lib:spawn_option()].
```

# `tmp_directory`
*not exported* 

```erlang
-type tmp_directory() :: [] | file:name().
```

# `tmp_file_usage`
*not exported* 

```erlang
-type tmp_file_usage() :: allowed | not_allowed | info_msg | warning_msg | error_msg.
```

# `append`

```erlang
-spec append(QHL) -> QH when QHL :: [query_handle_or_list()], QH :: query_handle().
```

Returns a query handle. When evaluating query handle `QH`, all answers to the
first query handle in `QHL` are returned, followed by all answers to the
remaining query handles in `QHL`.

# `append`

```erlang
-spec append(QH1, QH2) -> QH3
                when QH1 :: query_handle_or_list(), QH2 :: query_handle_or_list(), QH3 :: query_handle().
```

Returns a query handle. When evaluating query handle `QH3`, all answers to `QH1`
are returned, followed by all answers to `QH2`.

[`append(QH1, QH2)`](`append/2`) is equivalent to
[`append([QH1, QH2])`](`append/1`).

# `cursor`

```erlang
-spec cursor(QH) -> Cursor when QH :: query_handle_or_list(), Cursor :: query_cursor().
```

# `cursor`

```erlang
-spec cursor(QH, Options) -> Cursor
                when
                    QH :: query_handle_or_list(),
                    Options :: [Option] | Option,
                    Option ::
                        {cache_all, cache()} |
                        cache_all |
                        {max_list_size, max_list_size()} |
                        {spawn_options, spawn_options()} |
                        {tmpdir_usage, tmp_file_usage()} |
                        {tmpdir, tmp_directory()} |
                        {unique_all, boolean()} |
                        unique_all,
                    Cursor :: query_cursor().
```

Creates a query cursor and makes the calling process the owner of the cursor.

The cursor is to be used as argument to [`next_answers/1,2`](`next_answers/1`)
and (eventually) `delete_cursor/1`. Calls `erlang:spawn_opt/2` to spawn and link
to a process that evaluates the query handle. The value of option
`spawn_options` is used as last argument when calling
[`spawn_opt/2`](`spawn_opt/2`). Defaults to `[link]`.

_Example:_

```erlang
1> QH = qlc:q([{X,Y} || X <- [a,b], Y <- [1,2]]),
QC = qlc:cursor(QH),
qlc:next_answers(QC, 1).
[{a,1}]
2> qlc:next_answers(QC, 1).
[{a,2}]
3> qlc:next_answers(QC, all_remaining).
[{b,1},{b,2}]
4> qlc:delete_cursor(QC).
ok
```

# `delete_cursor`

```erlang
-spec delete_cursor(QueryCursor) -> ok when QueryCursor :: query_cursor().
```

Deletes a query cursor. Only the owner of the cursor can delete the cursor.

# `e`

```erlang
-spec e(QH) -> Answers | Error
           when
               QH :: query_handle_or_list(),
               Answers :: answers(),
               Error :: {error, module(), Reason},
               Reason :: file_sorter:reason().
```

# `e`

```erlang
-spec e(QH, Options) -> Answers | Error
           when
               QH :: query_handle_or_list(),
               Options :: [Option] | Option,
               Option ::
                   {cache_all, cache()} |
                   cache_all |
                   {max_list_size, max_list_size()} |
                   {tmpdir_usage, tmp_file_usage()} |
                   {tmpdir, tmp_directory()} |
                   {unique_all, boolean()} |
                   unique_all,
               Answers :: answers(),
               Error :: {error, module(), Reason},
               Reason :: file_sorter:reason().
```

# `eval`

```erlang
-spec eval(QH) -> Answers | Error
              when
                  QH :: query_handle_or_list(),
                  Answers :: answers(),
                  Error :: {error, module(), Reason},
                  Reason :: file_sorter:reason().
```

# `eval`

```erlang
-spec eval(QH, Options) -> Answers | Error
              when
                  QH :: query_handle_or_list(),
                  Answers :: answers(),
                  Options :: [Option] | Option,
                  Option ::
                      {cache_all, cache()} |
                      cache_all |
                      {max_list_size, max_list_size()} |
                      {tmpdir_usage, tmp_file_usage()} |
                      {tmpdir, tmp_directory()} |
                      {unique_all, boolean()} |
                      unique_all,
                  Error :: {error, module(), Reason},
                  Reason :: file_sorter:reason().
```

Evaluates a query handle in the calling process and collects all answers in a
list.

_Example:_

```erlang
1> QH = qlc:q([{X,Y} || X <- [a,b], Y <- [1,2]]),
qlc:eval(QH).
[{a,1},{a,2},{b,1},{b,2}]
```

# `fold`

```erlang
-spec fold(Function, Acc0, QH) -> Acc1 | Error
              when
                  QH :: query_handle_or_list(),
                  Function :: fun((answer(), AccIn) -> AccOut),
                  Acc0 :: term(),
                  Acc1 :: term(),
                  AccIn :: term(),
                  AccOut :: term(),
                  Error :: {error, module(), Reason},
                  Reason :: file_sorter:reason().
```

# `fold`

```erlang
-spec fold(Function, Acc0, QH, Options) -> Acc1 | Error
              when
                  QH :: query_handle_or_list(),
                  Function :: fun((answer(), AccIn) -> AccOut),
                  Acc0 :: term(),
                  Acc1 :: term(),
                  AccIn :: term(),
                  AccOut :: term(),
                  Options :: [Option] | Option,
                  Option ::
                      {cache_all, cache()} |
                      cache_all |
                      {max_list_size, max_list_size()} |
                      {tmpdir_usage, tmp_file_usage()} |
                      {tmpdir, tmp_directory()} |
                      {unique_all, boolean()} |
                      unique_all,
                  Error :: {error, module(), Reason},
                  Reason :: file_sorter:reason().
```

Calls `Function` on successive answers to the query handle together with an
extra argument `AccIn`.

The query handle and the function are evaluated in the
calling process. `Function` must return a new accumulator, which is passed to
the next call. `Acc0` is returned if there are no answers to the query handle.

_Example:_

```erlang
1> QH = [1,2,3,4,5,6],
qlc:fold(fun(X, Sum) -> X + Sum end, 0, QH).
21
```

# `format_error`

```erlang
-spec format_error(Error) -> Chars
                      when
                          Error ::
                              {error, module(), term()} |
                              atom() |
                              {atom(), term()} |
                              {atom(), term(), term()},
                          Chars :: io_lib:chars().
```

Returns a descriptive string in English of an error tuple returned by some of
the functions of the `qlc` module or the parse transform. This function is
mainly used by the compiler invoking the parse transform.

# `info`

```erlang
-spec info(QH) -> Info when QH :: query_handle_or_list(), Info :: abstract_expr() | string().
```

# `info`

```erlang
-spec info(QH, Options) -> Info
              when
                  QH :: query_handle_or_list(),
                  Options :: [Option] | Option,
                  Option :: EvalOption | ReturnOption,
                  EvalOption ::
                      {cache_all, cache()} |
                      cache_all |
                      {max_list_size, max_list_size()} |
                      {tmpdir_usage, tmp_file_usage()} |
                      {tmpdir, tmp_directory()} |
                      {unique_all, boolean()} |
                      unique_all,
                  ReturnOption ::
                      {depth, Depth} | {flat, boolean()} | {format, Format} | {n_elements, NElements},
                  Depth :: infinity | non_neg_integer(),
                  Format :: abstract_code | string,
                  NElements :: infinity | pos_integer(),
                  Info :: abstract_expr() | string().
```

Returns information about a query handle. The information describes the
simplifications and optimizations that are the results of preparing the query
for evaluation. This function is probably mainly useful during debugging.

The information has the form of an Erlang expression where QLCs most likely
occur. Depending on the format functions of mentioned QLC tables, it is not
certain that the information is absolutely accurate.

Options:

- The default is to return a sequence of QLCs in a block, but if option
  `{flat, false}` is specified, one single QLC is returned.
- The default is to return a string, but if option `{format, abstract_code}` is
  specified, abstract code is returned instead. In the abstract code, port
  identifiers, references, and pids are represented by strings.
- The default is to return all elements in lists, but if option
  `{n_elements, NElements}` is specified, only a limited number of elements are
  returned.
- The default is to show all parts of objects and match specifications, but if
  option `{depth, Depth}` is specified, parts of terms below a certain depth are
  replaced by `'...'`.

_Examples:_

In the following example two simple QLCs are inserted only to hold option
`{unique, true}`:

```erlang
1> QH = qlc:q([{X,Y} || X <- [x,y], Y <- [a,b]]),
io:format("~s~n", [qlc:info(QH, unique_all)]).
begin
    V1 =
        qlc:q([
               SQV ||
                   SQV <- [x, y]
              ],
              [{unique, true}]),
    V2 =
        qlc:q([
               SQV ||
                   SQV <- [a, b]
              ],
              [{unique, true}]),
    qlc:q([
           {X,Y} ||
               X <- V1,
               Y <- V2
          ],
          [{unique, true}])
end
```

In the following example QLC `V2` has been inserted to show the joined
generators and the join method chosen. A convention is used for lookup join: the
first generator (`G2`) is the one traversed, the second (`G1`) is the table
where constants are looked up.

```erlang
1> E1 = ets:new(e1, []),
E2 = ets:new(e2, []),
true = ets:insert(E1, [{1,a},{2,b}]),
true = ets:insert(E2, [{a,1},{b,2}]),
Q = qlc:q([{X,Z,W} ||
{X, Z} <- ets:table(E1),
{W, Y} <- ets:table(E2),
X =:= Y]),
io:format("~s~n", [qlc:info(Q)]).
begin
    V1 =
        qlc:q([
               P0 ||
                   P0 = {W, Y} <-
                       ets:table(#Ref<0.3098908599.2283929601.256549>)
              ]),
    V2 =
        qlc:q([
               [G1 | G2] ||
                   G2 <- V1,
                   G1 <-
                       ets:table(#Ref<0.3098908599.2283929601.256548>),
                   element(2, G1) =:= element(1, G2)
              ],
              [{join, lookup}]),
    qlc:q([
           {X, Z, W} ||
               [{X, Z} | {W, Y}] <- V2
          ])
end
```

# `keysort`

```erlang
-spec keysort(KeyPos, QH1) -> QH2
                 when KeyPos :: key_pos(), QH1 :: query_handle_or_list(), QH2 :: query_handle().
```

# `keysort`

```erlang
-spec keysort(KeyPos, QH1, SortOptions) -> QH2
                 when
                     KeyPos :: key_pos(),
                     SortOptions :: sort_options(),
                     QH1 :: query_handle_or_list(),
                     QH2 :: query_handle().
```

Returns a query handle. When evaluating query handle `QH2`, the answers to query
handle `QH1` are sorted by `file_sorter:keysort/4` according to the options.

The sorter uses temporary files only if `QH1` does not evaluate to a list and
the size of the binary representation of the answers exceeds `Size` bytes, where
`Size` is the value of option `size`.

# `next_answers`

```erlang
-spec next_answers(QueryCursor) -> Answers | Error
                      when
                          QueryCursor :: query_cursor(),
                          Answers :: answers(),
                          Error :: {error, module(), Reason},
                          Reason :: file_sorter:reason().
```

# `next_answers`

```erlang
-spec next_answers(QueryCursor, NumberOfAnswers) -> Answers | Error
                      when
                          QueryCursor :: query_cursor(),
                          Answers :: answers(),
                          NumberOfAnswers :: all_remaining | pos_integer(),
                          Error :: {error, module(), Reason},
                          Reason :: file_sorter:reason().
```

Returns some or all of the remaining answers to a query cursor. Only the owner
of `QueryCursor` can retrieve answers.

Argument `NumberOfAnswers` determines the maximum number of answers
returned. If less than the requested number of answers is
returned, subsequent calls to `next_answers` return `[]`.

# `q`

```erlang
-spec q(QLC) -> QH when QLC :: query_list_comprehension(), QH :: query_handle().
```

# `q`

```erlang
-spec q(QLC, Options) -> QH
           when
               QH :: query_handle(),
               Options :: [Option] | Option,
               Option ::
                   {max_lookup, MaxLookup} |
                   {cache, cache()} |
                   cache |
                   {join, Join} |
                   {lookup, Lookup} |
                   {unique, boolean()} |
                   unique,
               MaxLookup :: non_neg_integer() | infinity,
               Join :: any | lookup | merge | nested_loop,
               Lookup :: boolean() | any,
               QLC :: query_list_comprehension().
```

Returns a query handle for a QLC. The QLC must be the first argument to this
function, otherwise it is evaluated as an ordinary list comprehension. It is
also necessary to add the following line to the source code:

```erlang
-include_lib("stdlib/include/qlc.hrl").
```

This causes a parse transform to substitute a fun for the QLC. The (compiled)
fun is called when the query handle is evaluated.

When calling `qlc:q/1,2` from the Erlang shell, the parse transform is
automatically called. When this occurs, the fun substituted for the QLC is not
compiled but is evaluated by `m:erl_eval`. This is also true when expressions
are evaluated by `file:eval/1,2` or in the debugger.

To be explicit, this does not work:

```erlang
...
A = [X || {X} <- [{1},{2}]],
QH = qlc:q(A),
...
```

Variable `A` is bound to the evaluated value of the list comprehension
(`[1,2]`). The compiler complains with an error message ("argument is not a
query list comprehension"); the shell process stops with a `badarg` reason.

Options:

- Option `{cache, ets}` can be used to cache the answers to a QLC. The answers
  are stored in one ETS table for each cached QLC. When a cached QLC is
  evaluated again, answers are fetched from the table without any further
  computations. Therefore, when all answers to a cached QLC have been found, the
  ETS tables used for caching answers to the qualifiers of the QLC can be
  emptied. Option `cache` is equivalent to `{cache, ets}`.
- Option `{cache, list}` can be used to cache the answers to a QLC like
  `{cache, ets}`. The difference is that the answers are kept in a list (on the
  process heap). If the answers would occupy more than a certain amount of RAM
  memory, a temporary file is used for storing the answers. Option
  `max_list_size` sets the limit in bytes and the temporary file is put on the
  directory set by option `tmpdir`.

  Option `cache` has no effect if it is known that the QLC is to be evaluated at
  most once. This is always true for the top-most QLC and also for the list
  expression of the first generator in a list of qualifiers. Notice that in the
  presence of side effects in filters or callback functions, the answers to QLCs
  can be affected by option `cache`.

- Option `{unique, true}` can be used to remove duplicate answers to a QLC. The
  unique answers are stored in one ETS table for each QLC. The table is emptied
  every time it is known that there are no more answers to the QLC. Option
  `unique` is equivalent to `{unique, true}`. If option `unique` is combined
  with option `{cache, ets}`, two ETS tables are used, but the full answers are
  stored in one table only. If option `unique` is combined with option
  `{cache, list}`, the answers are sorted twice using `keysort/3`; once to
  remove duplicates and once to restore the order.

Options `cache` and `unique` apply not only to the QLC itself but also to the
results of looking up constants, running match specifications, and joining
handles.

_Example:_

In the following example the cached results of the merge join are traversed for
each value of `A`. Notice that without option `cache` the join would have been
carried out three times, once for each value of `A`.

```erlang
1> Q = qlc:q([{A,X,Z,W} ||
A <- [a,b,c],
{X,Z} <- [{a,1},{b,4},{c,6}],
{W,Y} <- [{2,a},{3,b},{4,c}],
X =:= Y],
{cache, list}),
io:format("~s~n", [qlc:info(Q)]).
begin
    V1 =
        qlc:q([
               P0 ||
                   P0 = {X, Z} <-
                       qlc:keysort(1, [{a, 1}, {b, 4}, {c, 6}], [])
              ]),
    V2 =
        qlc:q([
               P0 ||
                   P0 = {W, Y} <-
                       qlc:keysort(2, [{2, a}, {3, b}, {4, c}], [])
              ]),
    V3 =
        qlc:q([
               [G1 | G2] ||
                   G1 <- V1,
                   G2 <- V2,
                   element(1, G1) == element(2, G2)
              ],
              [{join, merge}, {cache, list}]),
    qlc:q([
           {A, X, Z, W} ||
               A <- [a, b, c],
               [{X, Z} | {W, Y}] <- V3,
               X =:= Y
          ])
end
```

[`sort/1,2`](`sort/1`) and [`keysort/2,3`](`keysort/2`) can also be used for
caching answers and for removing duplicates. When sorting answers are cached in
a list, possibly stored on a temporary file, and no ETS tables are used.

Sometimes (see `table/2`) traversal of tables can be done by looking up key
values, which is assumed to be fast. Under certain (rare) circumstances there
can be too many key values to look up. [](){: #max_lookup } Option
`{max_lookup, MaxLookup}` can then be used to limit the number of lookups: if
more than `MaxLookup` lookups would be required, no lookups are done but the
table is traversed instead. Defaults to `infinity`, which means that there is no
limit on the number of keys to look up.

_Example:_

In the following example, using the `gb_table` module from section
[Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`), there are six keys
to look up: `{1,a}`, `{1,b}`, `{1,c}`, `{2,a}`, `{2,b}`, and `{2,c}`. The reason
is that the two elements of key `{X, Y}` are compared separately.

```erlang
1> T = gb_trees:empty(),
QH = qlc:q([X || {{X,Y},_} <- gb_table:table(T),
((X == 1) or (X == 2)) andalso
((Y == a) or (Y == b) or (Y == c))]),
io:format("~s~n", [qlc:info(QH)]).
ets:match_spec_run(
       lists:flatmap(fun(K) ->
                            case
                                gb_trees:lookup(K,
                                                gb_trees:from_orddict([]))
                            of
                                {value, V} ->
                                    [{K, V}];
                                none ->
                                    []
                            end
                     end,
                     [{1, a},
                      {1, b},
                      {1, c},
                      {2, a},
                      {2, b},
                      {2, c}]),
       ets:match_spec_compile([{{{'$1', '$2'}, '_'},
                                [],
                                ['$1']}]))
```

Options:

- Option `{lookup, true}` can be used to ensure that the `qlc` module looks up
  constants in some QLC table. If there are more than one QLC table among the
  list expressions of the generators, constants must be looked up in at least
  one of the tables. The evaluation of the query fails if there are no constants
  to look up. This option is useful when it would be unacceptable to traverse
  all objects in some table. Setting option `lookup` to `false` ensures that no
  constants are looked up (`{max_lookup, 0}` has the same effect). Defaults to
  `any`, which means that constants are looked up whenever possible.
- Option `{join, Join}` can be used to ensure that a certain join method is
  used:

  - `{join, lookup}` invokes the lookup join method.
  - `{join, merge}` invokes the merge join method.
  - `{join, nested_loop}` invokes the method of matching every pair of objects
    from two handles. This method is mostly very slow.

  The evaluation of the query fails if the `qlc` module cannot carry out the
  chosen join method. Defaults to `any`, which means that some fast join method
  is used if possible.

# `sort`

```erlang
-spec sort(QH1) -> QH2 when QH1 :: query_handle_or_list(), QH2 :: query_handle().
```

# `sort`

```erlang
-spec sort(QH1, SortOptions) -> QH2
              when SortOptions :: sort_options(), QH1 :: query_handle_or_list(), QH2 :: query_handle().
```

Returns a query handle. When evaluating query handle `QH2`, the answers to query
handle `QH1` are sorted by `file_sorter:sort/3` according to the options.

The sorter uses temporary files only if `QH1` does not evaluate to a list and
the size of the binary representation of the answers exceeds `Size` bytes, where
`Size` is the value of option `size`.

# `string_to_handle`

```erlang
-spec string_to_handle(QueryString) -> QH | Error
                          when
                              QueryString :: string(),
                              QH :: query_handle(),
                              Error :: {error, module(), Reason},
                              Reason :: erl_parse:error_info() | erl_scan:error_info().
```

# `string_to_handle`

```erlang
-spec string_to_handle(QueryString, Options) -> QH | Error
                          when
                              QueryString :: string(),
                              Options :: [Option] | Option,
                              Option ::
                                  {max_lookup, MaxLookup} |
                                  {cache, cache()} |
                                  cache |
                                  {join, Join} |
                                  {lookup, Lookup} |
                                  {unique, boolean()} |
                                  unique,
                              MaxLookup :: non_neg_integer() | infinity,
                              Join :: any | lookup | merge | nested_loop,
                              Lookup :: boolean() | any,
                              QH :: query_handle(),
                              Error :: {error, module(), Reason},
                              Reason :: erl_parse:error_info() | erl_scan:error_info().
```

# `string_to_handle`

```erlang
-spec string_to_handle(QueryString, Options, Bindings) -> QH | Error
                          when
                              QueryString :: string(),
                              Options :: [Option] | Option,
                              Option ::
                                  {max_lookup, MaxLookup} |
                                  {cache, cache()} |
                                  cache |
                                  {join, Join} |
                                  {lookup, Lookup} |
                                  {unique, boolean()} |
                                  unique,
                              MaxLookup :: non_neg_integer() | infinity,
                              Join :: any | lookup | merge | nested_loop,
                              Lookup :: boolean() | any,
                              Bindings :: erl_eval:binding_struct(),
                              QH :: query_handle(),
                              Error :: {error, module(), Reason},
                              Reason :: erl_parse:error_info() | erl_scan:error_info().
```

A string version of [`q/1,2`](`q/1`). When the query handle is evaluated, the
fun created by the parse transform is interpreted by `m:erl_eval`. The query
string is to be one single QLC terminated by a period.

_Example:_

```erlang
1> L = [1,2,3],
Bs = erl_eval:add_binding('L', L, erl_eval:new_bindings()),
QH = qlc:string_to_handle("[X+1 || X <- L].", [], Bs),
qlc:eval(QH).
[2,3,4]
```

This function is probably mainly useful when called from outside of Erlang, for
example from a driver written in C.

> #### Note {: .info }
>
> Query handles created this way may have worse performance than when created
> directly via [`q/1,2`](`q/1`).

# `table`

```erlang
-spec table(TraverseFun, Options) -> QH
               when
                   TraverseFun :: TraverseFun0 | TraverseFun1,
                   TraverseFun0 :: fun(() -> TraverseResult),
                   TraverseFun1 :: fun((match_expression()) -> TraverseResult),
                   TraverseResult :: Objects | term(),
                   Objects :: [] | [term() | ObjectList],
                   ObjectList :: TraverseFun0 | Objects,
                   Options :: [Option] | Option,
                   Option ::
                       {format_fun, FormatFun} |
                       {info_fun, InfoFun} |
                       {lookup_fun, LookupFun} |
                       {parent_fun, ParentFun} |
                       {post_fun, PostFun} |
                       {pre_fun, PreFun} |
                       {key_equality, KeyComparison},
                   FormatFun :: undefined | fun((SelectedObjects) -> FormatedTable),
                   SelectedObjects ::
                       all |
                       {all, NElements, DepthFun} |
                       {match_spec, match_expression()} |
                       {lookup, Position, Keys} |
                       {lookup, Position, Keys, NElements, DepthFun},
                   NElements :: infinity | pos_integer(),
                   DepthFun :: fun((term()) -> term()),
                   FormatedTable :: {Mod, Fun, Args} | abstract_expr() | string(),
                   InfoFun :: undefined | fun((InfoTag) -> InfoValue),
                   InfoTag :: indices | is_unique_objects | keypos | num_of_objects,
                   InfoValue :: undefined | term(),
                   LookupFun :: undefined | fun((Position, Keys) -> LookupResult),
                   LookupResult :: [term()] | term(),
                   ParentFun :: undefined | fun(() -> ParentFunValue),
                   PostFun :: undefined | fun(() -> term()),
                   PreFun :: undefined | fun((PreArgs) -> term()),
                   PreArgs :: [PreArg],
                   PreArg :: {parent_value, ParentFunValue} | {stop_fun, StopFun},
                   ParentFunValue :: undefined | term(),
                   StopFun :: undefined | fun(() -> term()),
                   KeyComparison :: '=:=' | '==',
                   Position :: pos_integer(),
                   Keys :: [term()],
                   Mod :: atom(),
                   Fun :: atom(),
                   Args :: [term()],
                   QH :: query_handle().
```

Returns a query handle for a QLC table. In Erlang/OTP there is support for ETS,
Dets, and Mnesia tables, but many other data structures can be turned into QLC
tables. This is accomplished by letting function(s) in the module implementing
the data structure create a query handle by calling `qlc:table/2`.

The different ways to traverse the table and properties of the table are handled
by callback functions provided as options to `qlc:table/2`.

- Callback function `TraverseFun` is used for traversing the table. It is to
  return a list of objects terminated by either `[]` or a nullary fun to be used
  for traversing the not yet traversed objects of the table. Any other return
  value is immediately returned as value of the query evaluation. Unary
  `TraverseFun`s are to accept a match specification as argument. The match
  specification is created by the parse transform by analyzing the pattern of
  the generator calling `qlc:table/2` and filters using variables introduced in
  the pattern. If the parse transform cannot find a match specification
  equivalent to the pattern and filters, `TraverseFun` is called with a match
  specification returning every object.

  - Modules that can use match specifications for optimized traversal of tables
    are to call `qlc:table/2` with an unary `TraverseFun`. An example is
    `ets:table/2`.
  - Other modules can provide a nullary `TraverseFun`. An example is
    [`gb_table:table/1`](`m:qlc#gb_table`) in section
    [Implementing a QLC Table](`m:qlc#implementing_a_qlc_table`).

- Unary callback function `PreFun` is called once before the table is read for
  the first time. If the call fails, the query evaluation fails.

  Argument `PreArgs` is a list of tagged values. There are two tags,
  `parent_value` and `stop_fun`, used by Mnesia for managing transactions.

  - The value of `parent_value` is the value returned by `ParentFun`, or
    `undefined` if there is no `ParentFun`. `ParentFun` is called once just
    before the call of `PreFun` in the context of the process calling
    [`eval/1,2`](`eval/1`), [`fold/3,4`](`fold/3`), or
    [`cursor/1,2`](`cursor/1`).
  - The value of `stop_fun` is a nullary fun that deletes the cursor if called
    from the parent, or `undefined` if there is no cursor.

- Nullary callback function `PostFun` is called once after the table was last
  read. The return value, which is caught, is ignored. If `PreFun` has been
  called for a table, `PostFun` is guaranteed to be called for that table, even
  if the evaluation of the query fails for some reason.

  The pre (post) functions for different tables are evaluated in unspecified
  order.

  Other table access than reading, such as calling `InfoFun`, is assumed to be
  OK at any time.

- [](){: #lookup_fun } Binary callback function `LookupFun` is used for looking
  up objects in the table. The first argument `Position` is the key position or
  an indexed position and the second argument `Keys` is a sorted list of unique
  values. The return value is to be a list of all objects (tuples), such that
  the element at `Position` is a member of `Keys`. Any other return value is
  immediately returned as value of the query evaluation. `LookupFun` is called
  instead of traversing the table if the parse transform at compile time can
  determine that the filters match and compare the element at `Position` in such
  a way that only `Keys` need to be looked up to find all potential answers.

  The key position is obtained by calling `InfoFun(keypos)` and the indexed
  positions by calling `InfoFun(indices)`. If the key position can be used for
  lookup, it is always chosen, otherwise the indexed position requiring the
  least number of lookups is chosen. If there is a tie between two indexed
  positions, the one occurring first in the list returned by `InfoFun` is
  chosen. Positions requiring more than [max_lookup](`m:qlc#max_lookup`) lookups
  are ignored.

- Unary callback function `InfoFun` is to return information about the table.
  `undefined` is to be returned if the value of some tag is unknown:

  - **`indices`** - Returns a list of indexed positions, a list of positive
    integers.

  - **`is_unique_objects`** - Returns `true` if the objects returned by
    `TraverseFun` are unique.

  - **`keypos`** - Returns the position of the table key, a positive integer.

  - **`is_sorted_key`** - Returns `true` if the objects returned by
    `TraverseFun` are sorted on the key.

  - **`num_of_objects`** - Returns the number of objects in the table, a
    non-negative integer.

- Unary callback function `FormatFun` is used by [`info/1,2`](`info/1`) for
  displaying the call that created the query handle of the table. Defaults to
  `undefined`, which means that `info/1,2` displays a call to `'$MOD':'$FUN'/0`.
  It is up to `FormatFun` to present the selected objects of the table in a
  suitable way. However, if a character list is chosen for presentation, it must
  be an Erlang expression that can be scanned and parsed (a trailing dot is
  added by `info/1,2` though).

  `FormatFun` is called with an argument that describes the selected objects
  based on optimizations done as a result of analyzing the filters of the QLC
  where the call to `qlc:table/2` occurs. The argument can have the following
  values:

  - **`{lookup, Position, Keys, NElements, DepthFun}`.** - `LookupFun` is used
    for looking up objects in the table.

  - **`{match_spec, MatchExpression}`** - No way of finding all possible answers
    by looking up keys was found, but the filters could be transformed into a
    match specification. All answers are found by calling
    `TraverseFun(MatchExpression)`.

  - **`{all, NElements, DepthFun}`** - No optimization was found. A match
    specification matching all objects is used if `TraverseFun` is unary.

    `NElements` is the value of the `info/1,2` option `n_elements`.

    `DepthFun` is a function that can be used for limiting the size of terms;
    calling `DepthFun(Term)` substitutes `'...'` for parts of `Term` below the
    depth specified by the `info/1,2` option `depth`.

    If calling `FormatFun` with an argument including `NElements` and `DepthFun`
    fails, `FormatFun` is called once again with an argument excluding
    `NElements` and `DepthFun` (`{lookup, Position, Keys}` or `all`).

- [](){: #key_equality } The value of option `key_equality` is to be `'=:='` if
  the table considers two keys equal if they match, and to be `'=='` if two keys
  are equal if they compare equal. Defaults to `'=:='`.

For the various options recognized by `table/1,2` in respective module, see
[`ets`](`ets:table/1`), [`dets`](`dets:table/1`), and
[`mnesia`](`mnesia:table/1`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
