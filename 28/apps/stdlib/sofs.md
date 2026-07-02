# `sofs`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/sofs.erl#L22)

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2001-2025. All Rights Reserved.
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
Functions for manipulating sets of sets.

This module provides operations on finite sets and relations represented as
sets. Intuitively, a set is a collection of elements; every element belongs to
the set, and the set contains every element.

The data representing `sofs` as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

## Getting started

A recommended starting point for the first-time user is the examples
for the following functions:

* `relation_to_family/1`
* `restriction/2` and `drestriction/2`
* `image/2` and `inverse_image/2`
* `converse/1`

## Set theory

Given a set A and a sentence S(x), where x is a free variable, a new set B whose
elements are exactly those elements of A for which S(x) holds can be formed,
this is denoted B = \{x in A : S(x)\}. Sentences are expressed using the logical
operators "for some" (or "there exists"), "for all", "and", "or", "not". If the
existence of a set containing all the specified elements is known (as is always
the case in this module), this is denoted B = \{x : S(x)\}.

- The _unordered set_ containing the elements a, b, and c is denoted
  \{a, b, c\}. This notation is not to be confused with tuples.

  The _ordered pair_ of a and b, with first _coordinate_ a and second coordinate
  b, is denoted (a, b). An ordered pair is an _ordered set_ of two elements. In
  this module, ordered sets can contain one, two, or more elements, and
  parentheses are used to enclose the elements.

  Unordered sets and ordered sets are orthogonal, again in this module; there is
  no unordered set equal to any ordered set.

- The _empty set_ contains no elements.

  Set A is _equal_{: #equal } to set B if they contain the same elements, which
  is denoted A = B. Two ordered sets are equal if they contain the same number
  of elements and have equal elements at each coordinate.

  Set B is a _subset_{: #subset } of set A if A contains all elements that B
  contains.

  The _union_{: #union } of two sets A and B is the smallest set that contains
  all elements of A and all elements of B.

  The _intersection_{: #intersection } of two sets A and B is the set that
  contains all elements of A that belong to B.

  Two sets are _disjoint_{: #disjoint } if their intersection is the empty set.

  The _difference_{: #difference } of two sets A and B is the set that contains
  all elements of A that do not belong to B.

  The _symmetric difference_{: #symmetric_difference } of two sets is the set
  that contains those element that belong to either of the two sets, but not
  both.

  The _union_{: #union_n } of a collection of sets is the smallest set that
  contains all the elements that belong to at least one set of the collection.

  The _intersection_{: #intersection_n } of a non-empty collection of sets is
  the set that contains all elements that belong to every set of the collection.

- The _Cartesian product_{: #Cartesian_product } of two sets X and Y, denoted
  X × Y, is the set \{a : a = (x, y) for some x in X and for some y in Y\}.

  A _relation_{: #relation } is a subset of X × Y. Let R be a relation. The fact
  that (x, y) belongs to R is written as x R y. As relations are sets, the
  definitions of the last item (subset, union, and so on) apply to relations as
  well.

  The _domain_{: #domain } of R is the set \{x : x R y for some y in Y\}.

  The _range_{: #range } of R is the set \{y : x R y for some x in X\}.

  The _converse_{: #converse } of R is the set \{a : a = (y, x) for some
  (x, y) in R\}.

  If A is a subset of X, the _image_{: #image } of A under R is the set \{y :
  x R y for some x in A\}. If B is a subset of Y, the _inverse image_{:
  #inverse_image } of B is the set \{x : x R y for some y in B\}.

  If R is a relation from X to Y, and S is a relation from Y to Z, the _relative
  product_{: #relative_product } of R and S is the relation T from X to Z
  defined so that x T z if and only if there exists an element y in Y such that
  x R y and y S z.

  The _restriction_{: #restriction } of R to A is the set S defined so that
  x S y if and only if there exists an element x in A such that x R y.

  If S is a restriction of R to A, then R is an _extension_{: #extension } of S
  to X.

  If X = Y, then R is called a relation _in_ X.

  The _field_{: #field } of a relation R in X is the union of the domain of R
  and the range of R.

  If R is a relation in X, and if S is defined so that x S y if x R y and not
  x = y, then S is the _strict_{: #strict_relation } relation corresponding to
  R. Conversely, if S is a relation in X, and if R is defined so that x R y if
  x S y or x = y, then R is the _weak_{: #weak_relation } relation corresponding
  to S.

  A relation R in X is _reflexive_ if x R x for every element x of X, it is
  _symmetric_ if x R y implies that y R x, and it is _transitive_ if x R y and
  y R z imply that x R z.

- A _function_{: #function } F is a relation, a subset of X × Y, such that the
  domain of F is equal to X and such that for every x in X there is a unique
  element y in Y with (x, y) in F. The latter condition can be formulated as
  follows: if x F y and x F z, then y = z. In this module, it is not required
  that the domain of F is equal to X for a relation to be considered a function.

  Instead of writing (x, y) in F or x F y, we write F(x) = y when F is a
  function, and say that F maps x onto y, or that the value of F at x is y.

  As functions are relations, the definitions of the last item (domain, range,
  and so on) apply to functions as well.

  If the converse of a function F is a function F', then F' is called the
  _inverse_{: #inverse } of F.

  The relative product of two functions F1 and F2 is called the _composite_{:
  #composite } of F1 and F2 if the range of F1 is a subset of the domain of F2.

- Sometimes, when the range of a function is more important than the function
  itself, the function is called a _family_.

  The domain of a family is called the _index set_, and the range is called the
  _indexed set_.

  If x is a family from I to X, then x\[i] denotes the value of the function at
  index i. The notation "a family in X" is used for such a family.

  When the indexed set is a set of subsets of a set X, we call x a _family of
  subsets_{: #family } of X.

  If x is a family of subsets of X, the union of the range of x is called the
  _union of the family_ x.

  If x is non-empty (the index set is non-empty), the _intersection of the
  family_ x is the intersection of the range of x.

  In this module, the only families that are considered are families of subsets
  of some set X; in the following, the word "family" is used for such families
  of subsets.

- A _partition_{: #partition } of a set X is a collection S of non-empty subsets
  of X whose union is X and whose elements are pairwise disjoint.

  A relation in a set is an _equivalence relation_ if it is reflexive,
  symmetric, and transitive.

  If R is an equivalence relation in X, and x is an element of X, the
  _equivalence class_{: #equivalence_class } of x with respect to R is the set
  of all those elements y of X for which x R y holds. The equivalence classes
  constitute a partitioning of X. Conversely, if C is a partition of X, the
  relation that holds for any two elements of X if they belong to the same
  equivalence class, is an equivalence relation induced by the partition C.

  If R is an equivalence relation in X, the _canonical map_{: #canonical_map }
  is the function that maps every element of X onto its equivalence class.

- [](){: #binary_relation } Relations as defined above (as sets of ordered
  pairs) are from now on referred to as _binary relations_.

  We call a set of ordered sets (x\[1], ..., x\[n]) an _(n-ary) relation_{:
  #n_ary_relation }, and say that the relation is a subset of the [](){:
  #Cartesian_product_tuple } Cartesian product X\[1] × ... × X\[n], where x\[i]
  is an element of X\[i], 1 <= i <= n.

  The _projection_{: #projection } of an n-ary relation R onto coordinate i is
  the set \{x\[i] : (x\[1], ..., x\[i], ..., x\[n]) in R for some
  x\[j] in X\[j], 1 <= j <= n and not i = j\}. The projections of a binary
  relation R onto the first and second coordinates are the domain and the range
  of R, respectively.

  The relative product of binary relations can be generalized to n-ary relations
  as follows. Let TR be an ordered set (R\[1], ..., R\[n]) of binary relations
  from X to Y\[i] and S a binary relation from (Y\[1] × ... × Y\[n]) to Z. The
  _relative product_{: #tuple_relative_product } of TR and S is the binary
  relation T from X to Z defined so that x T z if and only if there exists an
  element y\[i] in Y\[i] for each 1 <= i <= n such that x R\[i] y\[i] and
  (y\[1], ..., y\[n]) S z. Now let TR be a an ordered set (R\[1], ..., R\[n]) of
  binary relations from X\[i] to Y\[i] and S a subset of X\[1] × ... × X\[n].

  The _multiple relative product_{: #multiple_relative_product } of TR and S is
  defined to be the set \{z : z = ((x\[1], ..., x\[n]), (y\[1],...,y\[n])) for
  some (x\[1], ..., x\[n]) in S and for some (x\[i], y\[i]) in R\[i],
  1 <= i <= n\}.

  The _natural join_{: #natural_join } of an n-ary relation R and an m-ary
  relation S on coordinate i and j is defined to be the set \{z : z =
  (x\[1], ..., x\[n],  y\[1], ..., y\[j-1], y\[j+1], ..., y\[m]) for some
  (x\[1], ..., x\[n]) in R and for some (y\[1], ..., y\[m]) in S such that
  x\[i] = y\[j]\}.

## Sets handled by this module

The sets recognized by this module are represented
by elements of the relation Sets, which is defined as the smallest set such
that:

- For every atom T, except '\_', and for every term X, (T, X) belongs to Sets
  (_atomic sets_).
- (\['\_'], []) belongs to Sets (the _untyped empty set_).
- For every tuple T = \{T\[1], ..., T\[n]\} and for every tuple X =
  \{X\[1], ..., X\[n]\}, if (T\[i], X\[i]) belongs to Sets for every
  1 <= i <= n, then (T, X) belongs to Sets (_ordered sets_).
- For every term T, if X is the empty list or a non-empty sorted list
  \[X[1], ..., X\[n]] without duplicates such that (T, X\[i]) belongs to Sets
  for every 1 <= i <= n, then (\[T], X) belongs to Sets (_typed unordered
  sets_).

An _external set_{: #external_set } is an element of the range of Sets.

A _type_{: #type } is an element of the domain of Sets.

If S is an element (T, X) of Sets, then T is a _valid type_{: #valid_type } of
X, T is the type of S, and X is the external set of S. `from_term/2` creates a
set from a type and an Erlang term turned into an external set.

The sets represented by Sets are the elements of the range of function Set
from Sets to Erlang terms and sets of Erlang terms:

- Set(T,Term) = Term, where T is an atom
- Set(\{T\[1], ..., T\[n]\}, \{X\[1], ...,  X\[n]\}) =
  (Set(T\[1], X\[1]), ...,  Set(T\[n], X\[n]))
- Set(\[T], \[X[1], ..., X\[n]]) = \{Set(T, X\[1]), ..., Set(T, X\[n])\}
- Set(\[T], []) = \{\}

When there is no risk of confusion, elements of Sets are identified with the
sets they represent. For example, if U is the result of calling `union/2` with
S1 and S2 as arguments, then U is said to be the union of S1 and S2. A more
precise formulation is that Set(U) is the union of Set(S1) and Set(S2).

The types are used to implement the various conditions that sets must fulfill.
As an example, consider the relative product of two sets R and S, and recall
that the relative product of R and S is defined if R is a binary relation to Y
and S is a binary relation from Y. The function that implements the relative
product, `relative_product/2`, checks that the arguments represent binary
relations by matching \[\{A,B\}] against the type of the first argument (Arg1
say), and \[\{C,D\}] against the type of the second argument (Arg2 say). The
fact that \[\{A,B\}] matches the type of Arg1 is to be interpreted as Arg1
representing a binary relation from X to Y, where X is defined as all sets
Set(x) for some element x in Sets the type of which is A, and similarly for Y.
In the same way Arg2 is interpreted as representing a binary relation from W to
Z. Finally it is checked that B matches C, which is sufficient to ensure that W
is equal to Y. The untyped empty set is handled separately: its type, \['\_'],
matches the type of any unordered set.

A few functions of this module (`drestriction/3`, `family_projection/2`,
`partition/2`, `partition_family/2`, `projection/2`, `restriction/3`,
`substitution/2`) accept an Erlang function as a means to modify each element of
a given unordered set. [](){: #set_fun } Such a function, called SetFun in the
following, can be specified as a functional object (fun), a tuple
`{external, Fun}`, or an integer:

- If SetFun is specified as a fun, the fun is applied to each element of the
  given set and the return value is assumed to be a set.
- If SetFun is specified as a tuple `{external, Fun}`, Fun is applied to the
  external set of each element of the given set and the return value is assumed
  to be an external set. Selecting the elements of an unordered set as external
  sets and assembling a new unordered set from a list of external sets is in the
  present implementation more efficient than modifying each element as a set.
  However, this optimization can only be used when the elements of the unordered
  set are atomic or ordered sets. It must also be the case that the type of the
  elements matches some clause of Fun (the type of the created set is the result
  of applying Fun to the type of the given set), and that Fun does nothing but
  selecting, duplicating, or rearranging parts of the elements.
- Specifying a SetFun as an integer I is equivalent to specifying
  `{external, fun(X) -> element(I, X) end}`, but is to be preferred, as it makes
  it possible to handle this case even more efficiently.

Examples of valid SetFuns:

```erlang
fun sofs:union/1
fun(S) -> sofs:partition(1, S) end
fun(S) -> sofs:from_term(sofs:no_elements(S)) end
{external, fun(A) -> A end}
{external, fun({A,_,C}) -> {C,A} end}
{external, fun({_,{_,C}}) -> C end}
{external, fun({_,{_,{_,E}=C}}) -> {E,{E,C}} end}
2
```

Examples of invalid SetFuns:

```erlang
fun sofs:no_elements/1
{external, fun(A) -> 2 * A end}
{external, fun({A,B,C}) -> A + B + C end}
{external, fun lists:sum/1}
```

The order in which a SetFun is applied to the elements of an unordered set is
not specified, and can change in future versions of this module.

The execution time of the functions of this module is dominated by the time it
takes to sort lists. When no sorting is needed, the execution time is in the
worst case proportional to the sum of the sizes of the input arguments and the
returned value. A few functions execute in constant time: `from_external/2`,
`is_empty_set/1`, `is_set/1`, `is_sofs_set/1`, `to_external/1` `type/1`.

The functions of this module exit the process with a `badarg`, `bad_function`,
or `type_mismatch` message when given badly formed arguments or sets the types
of which are not compatible.

When comparing external sets, operator `==/2` is used.

## See Also

`m:digraph`, `m:gb_sets`, `m:gb_trees`, `m:maps`, `m:orddict`, `m:ordsets`, `m:sets`

# `a_function`

```elixir
-type a_function() :: relation().
```

A [function](`m:sofs#function`).

# `a_set`

```elixir
-opaque a_set() :: #'Set'{data :: list(), type :: term()}.
```

An [unordered set](`m:sofs#module-sets-handled-by-this-module`).

# `anyset`

```elixir
-type anyset() :: ordset() | a_set().
```

Any kind of set (also included are the atomic sets).

# `binary_relation`

```elixir
-type binary_relation() :: relation().
```

A [binary relation](`m:sofs#binary_relation`).

# `external_set`

```elixir
-type external_set() :: term().
```

An [external set](`m:sofs#external_set`).

# `family`

```elixir
-type family() :: a_function().
```

A [family](`m:sofs#family`) (of subsets).

# `ordset`

```elixir
-opaque ordset() :: #'OrdSet'{orddata :: tuple() | atom(), ordtype :: term()}.
```

An [ordered set](`m:sofs#module-sets-handled-by-this-module`).

# `relation`

```elixir
-type relation() :: a_set().
```

An [n-ary relation](`m:sofs#n_ary_relation`).

# `set_fun`

```elixir
-type set_fun() ::
          pos_integer() |
          {external, fun((external_set()) -> external_set())} |
          fun((anyset()) -> anyset()).
```

A [SetFun](`m:sofs#set_fun`).

# `set_of_sets`

```elixir
-type set_of_sets() :: a_set().
```

An [unordered set](`m:sofs#module-sets-handled-by-this-module`)
of unordered sets.

# `spec_fun`

```elixir
-type spec_fun() :: {external, fun((external_set()) -> boolean())} | fun((anyset()) -> boolean()).
```

# `tuple_of`
*not exported* 

```elixir
-type tuple_of(_T) :: tuple().
```

A tuple where the elements are of type `T`.

# `type`

```elixir
-type type() :: term().
```

A [type](`m:sofs#type`).

# `a_function`

```elixir
-spec a_function(Tuples) -> Function when Function :: a_function(), Tuples :: [tuple()].
```

# `a_function`

```elixir
-spec a_function(Tuples, Type) -> Function
                    when Function :: a_function(), Tuples :: [tuple()], Type :: type().
```

Creates a [function](`m:sofs#function`).

[`a_function(F, T)`](`a_function/2`) is equivalent to
[`from_term(F, T)`](`from_term/2`) if the result is a function.

## Examples

```erlang
1> sofs:is_a_function(sofs:a_function([{1,a},{2,b},{3,c}])).
true
2> sofs:a_function([{1,a},{1,b}]).
** exception error: bad_function
     in function  sofs:a_function/1
```

# `canonical_relation`

```elixir
-spec canonical_relation(SetOfSets) -> BinRel
                            when BinRel :: binary_relation(), SetOfSets :: set_of_sets().
```

Returns the binary relation containing the elements (E, Set) such that Set
belongs to `SetOfSets` and E belongs to Set.

If `SetOfSets` is a [partition](`m:sofs#partition`) of a set X and R is the
equivalence relation in X induced by `SetOfSets`, then the returned relation is the
[canonical map](`m:sofs#canonical_map`) from X onto the equivalence classes with
respect to R.

## Examples

```erlang
1> Ss = sofs:from_term([[a,b],[b,c]]).
2> CR = sofs:canonical_relation(Ss).
3> sofs:to_external(CR).
[{a,[a,b]},{b,[a,b]},{b,[b,c]},{c,[b,c]}]
```

# `composite`

```elixir
-spec composite(Function1, Function2) -> Function3
                   when Function1 :: a_function(), Function2 :: a_function(), Function3 :: a_function().
```

Returns the [composite](`m:sofs#composite`) of the functions `Function1` and
`Function2`.

## Examples

```erlang
1> F1 = sofs:a_function([{a,1},{b,2},{c,2}]).
2> F2 = sofs:a_function([{1,x},{2,y},{3,z}]).
3> F = sofs:composite(F1, F2).
4> sofs:to_external(F).
[{a,x},{b,y},{c,y}]
5> sofs:composite(F2, F1).
** exception error: bad_function
     in function  sofs:composite/2
```

# `constant_function`

```elixir
-spec constant_function(Set, AnySet) -> Function
                           when AnySet :: anyset(), Function :: a_function(), Set :: a_set().
```

Creates the [function](`m:sofs#function`) that maps each element of set `Set`
onto `AnySet`.

## Examples

```erlang
1> S = sofs:set([a,b]).
2> E = sofs:from_term(1).
3> R = sofs:constant_function(S, E).
4> sofs:to_external(R).
[{a,1},{b,1}]
```

# `converse`

```elixir
-spec converse(BinRel1) -> BinRel2 when BinRel1 :: binary_relation(), BinRel2 :: binary_relation().
```

Returns the [converse](`m:sofs#converse`) of the binary relation `BinRel1`.

See `inverse/1` for a similar function that applies only to invertible
functions.

## Examples

```erlang
1> R1 = sofs:relation([{1,a},{2,b},{3,a}]).
2> R2 = sofs:converse(R1).
3> sofs:to_external(R2).
[{a,1},{a,3},{b,2}]
```

# `difference`

```elixir
-spec difference(Set1, Set2) -> Set3 when Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns the [difference](`m:sofs#difference`) of the sets `Set1` and `Set2`.

## Examples

```erlang
1> S0 = sofs:set([a,b,c,d]).
2> S1 = sofs:set([c,d,e,f]).
3> sofs:to_external(sofs:difference(S0, S1)).
[a,b]
4> sofs:to_external(sofs:difference(S1, S0)).
[e,f]
```

# `digraph_to_family`

```elixir
-spec digraph_to_family(Graph) -> Family when Graph :: digraph:graph(), Family :: family().
```

# `digraph_to_family`

```elixir
-spec digraph_to_family(Graph, Type) -> Family
                           when Graph :: digraph:graph(), Family :: family(), Type :: type().
```

Creates a [family](`m:sofs#family`) from the directed graph `Graph`.

Each vertex a of `Graph` is represented by a pair
(a, \{b\[1], ..., b\[n]\}), where the b\[i]:s are the out-neighbors of
a. It is assumed that `Type` is a [valid type](`m:sofs#valid_type`) of
the external set of the family.

If G is a directed graph, it holds that the vertices and edges of G
are the same as the vertices and edges of
[`family_to_digraph(digraph_to_family(G))`](`family_to_digraph/1`).

## Examples

```erlang
1> G = digraph:new().
2> digraph:add_vertex(G, 1).
3> digraph:add_vertex(G, a).
4> digraph:add_vertex(G, b).
5> digraph:add_edge(G, 1, a).
6> digraph:add_edge(G, 1, b).
7> F = sofs:digraph_to_family(G).
8> sofs:to_external(F).
[{1,[a,b]},{a,[]},{b,[]}]
```

# `domain`

```elixir
-spec domain(BinRel) -> Set when BinRel :: binary_relation(), Set :: a_set().
```

Returns the [domain](`m:sofs#domain`) of the binary relation `BinRel`.

## Examples

```erlang
1> R = sofs:relation([{1,a},{1,b},{2,b},{2,c}]).
2> S = sofs:domain(R).
3> sofs:to_external(S).
[1,2]
```

# `drestriction`

```elixir
-spec drestriction(BinRel1, Set) -> BinRel2
                      when BinRel1 :: binary_relation(), BinRel2 :: binary_relation(), Set :: a_set().
```

Returns the difference between the binary relation `BinRel1` and the
[restriction](`m:sofs#restriction`) of `BinRel1` to `Set`.

## Examples

```erlang
1> R1 = sofs:relation([{1,a},{2,b},{3,c}]).
2> S = sofs:set([2,4,6]).
3> R2 = sofs:drestriction(R1, S).
4> sofs:to_external(R2).
[{1,a},{3,c}]
```

[`drestriction(R, S)`](`drestriction/2`) is equivalent to
[`difference(R, restriction(R, S))`](`difference/2`).

# `drestriction`

```elixir
-spec drestriction(SetFun, Set1, Set2) -> Set3
                      when SetFun :: set_fun(), Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns a subset of `Set1` containing those elements that do not give an element
in `Set2` as the result of applying `SetFun`.

## Examples

```erlang
1> SetFun = {external, fun({_A,B,C}) -> {B,C} end}.
2> R1 = sofs:relation([{a,aa,1},{b,bb,2},{c,cc,3}]).
3> R2 = sofs:relation([{bb,2},{cc,3},{dd,4}]).
4> R3 = sofs:drestriction(SetFun, R1, R2).
5> sofs:to_external(R3).
[{a,aa,1}]
```

[`drestriction(F, S1, S2)`](`drestriction/3`) is equivalent to
[`difference(S1, restriction(F, S1, S2))`](`difference/2`).

# `empty_set`

```elixir
-spec empty_set() -> Set when Set :: a_set().
```

Returns the [untyped empty set](`m:sofs#module-sets-handled-by-this-module`).

`empty_set/0` is equivalent to [`from_term([], ['_'])`](`from_term/2`).

## Examples

```erlang
1> sofs:to_external(sofs:empty_set()).
[]
2> sofs:is_empty_set(sofs:empty_set()).
true
```

# `extension`

```elixir
-spec extension(BinRel1, Set, AnySet) -> BinRel2
                   when
                       AnySet :: anyset(),
                       BinRel1 :: binary_relation(),
                       BinRel2 :: binary_relation(),
                       Set :: a_set().
```

Returns the [extension](`m:sofs#extension`) of `BinRel1` such that for each
element E in `Set` that does not belong to the [domain](`m:sofs#domain`) of
`BinRel1`, `BinRel2` contains the pair (E, `AnySet`).

## Examples

```erlang
1> S = sofs:set([b,c]).
2> A = sofs:empty_set().
3> R = sofs:family([{a,[1,2]},{b,[3]}]).
4> X = sofs:extension(R, S, A).
5> sofs:to_external(X).
[{a,[1,2]},{b,[3]},{c,[]}]
```

# `family`

```elixir
-spec family(Tuples) -> Family when Family :: family(), Tuples :: [tuple()].
```

# `family`

```elixir
-spec family(Tuples, Type) -> Family when Family :: family(), Tuples :: [tuple()], Type :: type().
```

Creates a [family of subsets](`m:sofs#family`).

[`family(F, T)`](`family/2`) is equivalent to
[`from_term(F, T)`](`from_term/2`) if the result is a family.

## Examples

```erlang
1> S = sofs:family([{1,[a,b]},{2,[c]}], [{index,[value]}]).
2> sofs:to_external(sofs:family_to_relation(S)).
[{1,a},{1,b},{2,c}]
3> S = sofs:family([{1,[a,b]},{1,[c]}], [{index,[value]}]).
** exception error: bad_function
     in function  sofs:family/2
```

# `family_difference`

```elixir
-spec family_difference(Family1, Family2) -> Family3
                           when Family1 :: family(), Family2 :: family(), Family3 :: family().
```

If `Family1` and `Family2` are [families](`m:sofs#family`), then `Family3` is
the family such that the index set is equal to the index set of `Family1`, and
`Family3`\[i] is the difference between `Family1`\[i] and `Family2`\[i] if
`Family2` maps i, otherwise `Family1[i]`.

## Examples

```erlang
1> F1 = sofs:family([{a,[1,2]},{b,[3,4]}]).
2> F2 = sofs:family([{b,[4,5]},{c,[6,7]}]).
3> F3 = sofs:family_difference(F1, F2).
4> sofs:to_external(F3).
[{a,[1,2]},{b,[3]}]
```

# `family_domain`

```elixir
-spec family_domain(Family1) -> Family2 when Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`) and `Family1`\[i] is a binary
relation for every i in the index set of `Family1`, then `Family2` is the family
with the same index set as `Family1` such that `Family2`\[i] is the
[domain](`m:sofs#domain`) of `Family1[i]`.

## Examples

```erlang
1> FR = sofs:from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]).
2> F = sofs:family_domain(FR).
3> sofs:to_external(F).
[{a,[1,2,3]},{b,[]},{c,[4,5]}]
```

# `family_field`

```elixir
-spec family_field(Family1) -> Family2 when Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`) and `Family1`\[i] is a binary
relation for every i in the index set of `Family1`, then `Family2` is the family
with the same index set as `Family1` such that `Family2`\[i] is the
[field](`m:sofs#field`) of `Family1`\[i].

## Examples

```erlang
1> FR = sofs:from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]).
2> F = sofs:family_field(FR).
3> sofs:to_external(F).
[{a,[1,2,3,a,b,c]},{b,[]},{c,[4,5,d,e]}]
```

[`family_field(Family1)`](`family_field/1`) is equivalent to
[`family_union(family_domain(Family1), family_range(Family1))`](`family_union/2`).

# `family_intersection`

```elixir
-spec family_intersection(Family1) -> Family2 when Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`) and `Family1`[i] is a set of sets
for every i in the index set of `Family1`, then `Family2` is the family with the
same index set as `Family1` such that `Family2`[i] is the
[intersection](`m:sofs#intersection_n`) of `Family1`[i].

If `Family1`[i] is an empty set for some i, a `badarg` exception is raised.

## Examples

```erlang
1> F1 = sofs:from_term([{a,[[1,2,3],[2,3,4]]},{b,[[x,y,z],[x,y]]}]).
2> F2 = sofs:family_intersection(F1).
3> sofs:to_external(F2).
[{a,[2,3]},{b,[x,y]}]
4> F3 = sofs:from_term([{a,[[1,2]]},{b,[]}]).
5> sofs:family_intersection(F3).
** exception error: bad argument
     in function  sofs:family_intersection/1
```

# `family_intersection`

```elixir
-spec family_intersection(Family1, Family2) -> Family3
                             when Family1 :: family(), Family2 :: family(), Family3 :: family().
```

If `Family1` and `Family2` are [families](`m:sofs#family`), then `Family3` is
the family such that the index set is the intersection of `Family1`:s and
`Family2`:s index sets, and `Family3`\[i] is the intersection of `Family1`\[i]
and `Family2`\[i].

## Examples

```erlang
1> F1 = sofs:family([{a,[1,2]},{b,[3,4]},{c,[5,6]}]).
2> F2 = sofs:family([{b,[4,5]},{c,[7,8]},{d,[9,10]}]).
3> F3 = sofs:family_intersection(F1, F2).
4> sofs:to_external(F3).
[{b,[4]},{c,[]}]
```

# `family_projection`

```elixir
-spec family_projection(SetFun, Family1) -> Family2
                           when SetFun :: set_fun(), Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`), then `Family2` is the family with
the same index set as `Family1` such that `Family2`\[i] is the result of calling
`SetFun` with `Family1`\[i] as argument.

## Examples

```erlang
1> F1 = sofs:from_term([{a,[[1,2],[2,3]]},{b,[[]]}]).
2> F2 = sofs:family_projection(fun sofs:union/1, F1).
3> sofs:to_external(F2).
[{a,[1,2,3]},{b,[]}]
```

# `family_range`

```elixir
-spec family_range(Family1) -> Family2 when Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`) and `Family1`\[i] is a binary
relation for every i in the index set of `Family1`, then `Family2` is the family
with the same index set as `Family1` such that `Family2`\[i] is the
[range](`m:sofs#range`) of `Family1`\[i].

## Examples

```erlang
1> FR = sofs:from_term([{a,[{1,a},{2,b},{3,c}]},{b,[]},{c,[{4,d},{5,e}]}]).
2> F = sofs:family_range(FR).
3> sofs:to_external(F).
[{a,[a,b,c]},{b,[]},{c,[d,e]}]
```

# `family_specification`

```elixir
-spec family_specification(Fun, Family1) -> Family2
                              when Fun :: spec_fun(), Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`), then `Family2` is the
[restriction](`m:sofs#restriction`) of `Family1` to those elements i of the
index set for which `Fun` applied to `Family1`\[i] returns `true`.

If `Fun` is a tuple `{external, Fun2}`, then `Fun2` is applied to the
[external set](`m:sofs#external_set`) of `Family1`\[i]; otherwise
`Fun` is applied to `Family1`\[i].

## Examples

```erlang
1> F1 = sofs:family([{a,[1,2,3]},{b,[1,2]},{c,[1]}]).
2> SpecFun = fun(S) -> sofs:no_elements(S) =:= 2 end.
3> F2 = sofs:family_specification(SpecFun, F1).
4> sofs:to_external(F2).
[{b,[1,2]}]
```

# `family_to_digraph`

```elixir
-spec family_to_digraph(Family) -> Graph when Graph :: digraph:graph(), Family :: family().
```

# `family_to_digraph`

```elixir
-spec family_to_digraph(Family, GraphType) -> Graph
                           when
                               Graph :: digraph:graph(),
                               Family :: family(),
                               GraphType :: [digraph:d_type()].
```

Creates a directed graph from [family](`m:sofs#family`) `Family`.

For each pair (a, \{b\[1], ..., b\[n]\}) of `Family`, vertex a and the
edges (a, b\[i]) for 1 <= i <= n are added to a newly created directed
graph.

`GraphType` is passed on to `digraph:new/1`.

It F is a family, it holds that F is a subset of
[`digraph_to_family(family_to_digraph(F), type(F))`](`digraph_to_family/2`).
Equality holds if [`union_of_family(F)`](`union_of_family/1`) is a subset of
[`domain(F)`](`domain/1`).

Creating a cycle in an acyclic graph exits the process with a `cyclic` message.

## Examples

```erlang
1> F1 = sofs:family([{1,[a,b]}, {2,[c,d]}, {3,[d]}, {a,[b]}]).
2> G = sofs:family_to_digraph(F1, []).
3> digraph_utils:topsort(G).
[1,a,b,2,c,3,d]
4> F2 = sofs:family([{1,[1]}]).
5> sofs:family_to_digraph(F2, [acyclic]).
** exception error: cyclic
     in function  sofs:family_to_digraph/2
```

# `family_to_relation`

```elixir
-spec family_to_relation(Family) -> BinRel when Family :: family(), BinRel :: binary_relation().
```

If `Family` is a [family](`m:sofs#family`), then `BinRel` is the binary relation
containing all pairs (i, x) such that i belongs to the index set of `Family` and
x belongs to `Family`\[i].

## Examples

```erlang
1> F = sofs:family([{a,[]}, {b,[1]}, {c,[2,3]}]).
2> R = sofs:family_to_relation(F).
3> sofs:to_external(R).
[{b,1},{c,2},{c,3}]
```

# `family_union`

```elixir
-spec family_union(Family1) -> Family2 when Family1 :: family(), Family2 :: family().
```

If `Family1` is a [family](`m:sofs#family`) and `Family1`\[i] is a set of sets
for each i in the index set of `Family1`, then `Family2` is the family with the
same index set as `Family1` such that `Family2`\[i] is the
[union](`m:sofs#union_n`) of `Family1`\[i].

## Examples

```erlang
1> F1 = sofs:from_term([{a,[[1,2],[2,3]]},{b,[[]]}]).
2> F2 = sofs:family_union(F1).
3> sofs:to_external(F2).
[{a,[1,2,3]},{b,[]}]
```

[`family_union(F)`](`family_union/1`) is equivalent to
[`family_projection(fun sofs:union/1, F)`](`family_projection/2`).

# `family_union`

```elixir
-spec family_union(Family1, Family2) -> Family3
                      when Family1 :: family(), Family2 :: family(), Family3 :: family().
```

If `Family1` and `Family2` are [families](`m:sofs#family`), then `Family3` is
the family such that the index set is the union of `Family1`:s and `Family2`:s
index sets, and `Family3`\[i] is the union of `Family1`\[i] and `Family2`\[i] if
both map i, otherwise `Family1`\[i] or `Family2`\[i].

## Examples

```erlang
1> F1 = sofs:family([{a,[1,2]},{b,[3,4]},{c,[5,6]}]).
2> F2 = sofs:family([{b,[4,5]},{c,[7,8]},{d,[9,10]}]).
3> F3 = sofs:family_union(F1, F2).
4> sofs:to_external(F3).
[{a,[1,2]},{b,[3,4,5]},{c,[5,6,7,8]},{d,[9,10]}]
```

# `field`

```elixir
-spec field(BinRel) -> Set when BinRel :: binary_relation(), Set :: a_set().
```

Returns the [field](`m:sofs#field`) of the binary relation `BinRel`.

## Examples

```erlang
1> R = sofs:relation([{1,a},{1,b},{2,b},{2,c}]).
2> S = sofs:field(R).
3> sofs:to_external(S).
[1,2,a,b,c]
```

[`field(R)`](`field/1`) is equivalent to
[`union(domain(R), range(R))`](`union/2`).

# `from_external`

```elixir
-spec from_external(ExternalSet, Type) -> AnySet
                       when ExternalSet :: external_set(), AnySet :: anyset(), Type :: type().
```

Creates a set from the [external set](`m:sofs#external_set`) `ExternalSet` and
the [type](`m:sofs#type`) `Type`.

It is assumed that `Type` is a [valid type](`m:sofs#valid_type`) of
`ExternalSet`.

## Examples

```erlang
1> S0 = sofs:from_external([{1,[a,b]},{2,[c]}], [{x,[y]}]).
2> sofs:to_external(sofs:family_to_relation(S0)).
[{1,a},{1,b},{2,c}]
3> S1 = sofs:from_external({a,b,c}, {x,x,x}).
4> sofs:no_elements(S1).
3
```

# `from_sets`

```elixir
-spec from_sets(ListOfSets) -> Set when Set :: a_set(), ListOfSets :: [anyset()];
               (TupleOfSets) -> Ordset when Ordset :: ordset(), TupleOfSets :: tuple_of(anyset()).
```

Returns the [unordered
set](`m:sofs#module-sets-handled-by-this-module`) containing the sets
of list `ListOfSets`, or returns the [ordered
set](`m:sofs#module-sets-handled-by-this-module`) containing the sets
of the non-empty tuple `TupleOfSets`.

## Examples

Creating an unordered set.

```erlang
1> S1 = sofs:relation([{a,1},{b,2}]).
2> S2 = sofs:relation([{x,3},{y,4}]).
3> S = sofs:from_sets([S1,S2]).
4> sofs:to_external(S).
[[{a,1},{b,2}],[{x,3},{y,4}]]
5> sofs:type(S).
[[{atom,atom}]]
```

Creating an ordered set.

```erlang
1> S1 = sofs:from_term(a).
2> S2 = sofs:from_term(b).
3> S = sofs:from_sets({S1,S2}).
4> sofs:to_external(S).
{a,b}
5> sofs:type(S).
{atom,atom}
```

# `from_term`

```elixir
-spec from_term(Term) -> AnySet when AnySet :: anyset(), Term :: term().
```

# `from_term`

```elixir
-spec from_term(Term, Type) -> AnySet when AnySet :: anyset(), Term :: term(), Type :: type().
```

Creates an element of [Sets](`m:sofs#module-sets-handled-by-this-module`) by
traversing term `Term`, sorting lists, removing duplicates, and deriving or
verifying a [valid type](`m:sofs#valid_type`) for the so obtained external set.

An explicitly specified [type](`m:sofs#type`) `Type` can be used to limit the
depth of the traversal; an atomic type stops the traversal, as shown by the
following example where `"foo"` and `{"foo"}` are left unmodified:

```erlang
1> S = sofs:from_term([{{"foo"},[1,1]},{"foo",[2,2]}],
                      [{atom,[atom]}]),
   sofs:to_external(S).
[{{"foo"},[1]},{"foo",[2]}]
```

`from_term/1` can be used for creating atomic or ordered sets. The only purpose of
such a set is that of later building unordered sets, as all functions in this
module that _do_ anything operate on unordered sets. Creating unordered sets
from a collection of ordered sets can be the way to go if the ordered sets are
big and one does not want to waste heap by rebuilding the elements of the
unordered set. The following example shows that a set can be built "layer by
layer":

```erlang
1> A = sofs:from_term(a).
2> S = sofs:set([1,2,3]).
3> P1 = sofs:from_sets({A,S}).
4> P2 = sofs:from_term({b,[6,5,4]}).
5> Ss = sofs:from_sets([P1,P2]).
6> sofs:to_external(Ss).
[{a,[1,2,3]},{b,[4,5,6]}]
```

Other functions that create sets are `from_external/2` and `from_sets/1`.
Special cases of [`from_term/2`](`from_term/2`) are
[`a_function/1,2`](`a_function/1`), `empty_set/0`, [`family/1,2`](`family/1`),
[`relation/1,2`](`relation/1`), and [`set/1,2`](`set/1`).

# `image`

```elixir
-spec image(BinRel, Set1) -> Set2 when BinRel :: binary_relation(), Set1 :: a_set(), Set2 :: a_set().
```

Returns the [image](`m:sofs#image`) of set `Set1` under the binary relation
`BinRel`.

## Examples

```erlang
1> R = sofs:relation([{1,a},{2,b},{2,c},{3,d}]).
2> S1 = sofs:set([1,2]).
3> S2 = sofs:image(R, S1).
4> sofs:to_external(S2).
[a,b,c]
```

# `intersection`

```elixir
-spec intersection(SetOfSets) -> Set when Set :: a_set(), SetOfSets :: set_of_sets().
```

Returns the [intersection](`m:sofs#intersection_n`) of the set of sets
`SetOfSets`.

Intersecting an empty set of sets exits the process with a `badarg` message.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([b,c,d,e]).
3> S3 = sofs:set([a,b,c,d]).
4> S4 = sofs:from_sets([S1,S2,S3]).
5> S5 = sofs:intersection(S4).
6> sofs:to_external(S5).
[b,c]
7> S6 = sofs:from_sets([]).
8> sofs:intersection(S6).
** exception error: bad argument
     in function  sofs:intersection/1
```

# `intersection`

```elixir
-spec intersection(Set1, Set2) -> Set3 when Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns the [intersection](`m:sofs#intersection`) of `Set1` and `Set2`.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([b,c,d]).
3> S3 = sofs:intersection(S1, S2).
4> sofs:to_external(S3).
[b,c]
```

# `intersection_of_family`

```elixir
-spec intersection_of_family(Family) -> Set when Family :: family(), Set :: a_set().
```

Returns the intersection of [family](`m:sofs#family`) `Family`.

Intersecting an empty family exits the process with a `badarg` message.

## Examples

```erlang
1> F = sofs:family([{a,[0,2,4]},{b,[0,1,2]},{c,[2,3]}]).
2> S = sofs:intersection_of_family(F).
3> sofs:to_external(S).
[2]
```

# `inverse`

```elixir
-spec inverse(Function1) -> Function2 when Function1 :: a_function(), Function2 :: a_function().
```

Returns the [inverse](`m:sofs#inverse`) of function `Function1`.

A `bad_function` exception is raised if `Function1` is not invertible.

See `converse/1` for a similar function that handles any binary relation.

## Examples

```erlang
1> F1 = sofs:relation([{1,a},{2,b},{3,c}]).
2> F2 = sofs:inverse(F1).
3> sofs:to_external(F2).
[{a,1},{b,2},{c,3}]
```

Trying to inverse a non-invertible function.

```erlang
1> R1 = sofs:relation([{1,a},{2,a}]).
2> sofs:inverse(R1).
** exception error: bad_function
     in function  sofs:inverse/1
3> R2 = sofs:converse(R1).
4> sofs:to_external(R2).
[{a,1},{a,2}]
```

# `inverse_image`

```elixir
-spec inverse_image(BinRel, Set1) -> Set2
                       when BinRel :: binary_relation(), Set1 :: a_set(), Set2 :: a_set().
```

Returns the [inverse image](`m:sofs#inverse_image`) of `Set1` under the binary
relation `BinRel`.

## Examples

```erlang
1> R = sofs:relation([{1,a},{2,b},{2,c},{3,d}]).
2> S1 = sofs:set([c,d,e]).
3> S2 = sofs:inverse_image(R, S1).
4> sofs:to_external(S2).
[2,3]
```

# `is_a_function`

```elixir
-spec is_a_function(BinRel) -> Bool when Bool :: boolean(), BinRel :: binary_relation().
```

Returns `true` if the binary relation `BinRel` is a
[function](`m:sofs#function`) or the untyped empty set; otherwise,
returns `false`.

## Examples

```erlang
1> sofs:is_a_function(sofs:relation([{1,a},{2,b},{3,c}])).
true
2> sofs:is_a_function(sofs:relation([{1,a},{1,b},{3,c}])).
false
3> sofs:is_a_function(sofs:set([a,b,c])).
** exception error: bad argument
     in function  sofs:is_a_function/1
```

# `is_disjoint`

```elixir
-spec is_disjoint(Set1, Set2) -> Bool when Bool :: boolean(), Set1 :: a_set(), Set2 :: a_set().
```

Returns `true` if `Set1` and `Set2` are [disjoint](`m:sofs#disjoint`); otherwise,
returns `false`.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([c,d,e]).
3> S3 = sofs:set([1,2,3]).
4> sofs:is_disjoint(S1, S2).
false
5> sofs:is_disjoint(S1, S3).
true
6> sofs:is_disjoint(sofs:set([1,2,3]), sofs:relation([{a,b}])).
** exception error: type_mismatch
     in function  sofs:is_disjoint/2
```

# `is_empty_set`

```elixir
-spec is_empty_set(AnySet) -> Bool when AnySet :: anyset(), Bool :: boolean().
```

Returns `true` if `AnySet` is an empty unordered set; otherwise, returns `false`.

## Examples

```erlang
1> sofs:is_empty_set(sofs:empty_set()).
true
2> sofs:is_empty_set(sofs:set([a,b])).
false
```

# `is_equal`

```elixir
-spec is_equal(AnySet1, AnySet2) -> Bool
                  when AnySet1 :: anyset(), AnySet2 :: anyset(), Bool :: boolean().
```

Returns `true` if `AnySet1` and `AnySet2` are [equal](`m:sofs#equal`), otherwise
`false`.

## Examples

The following example shows that `==/2` is used when comparing sets for
equality:

```erlang
1> S1 = sofs:set([1.0]).
2> S2 = sofs:set([1]).
3> sofs:is_equal(S1, S2).
true
```

# `is_set`

```elixir
-spec is_set(AnySet) -> Bool when AnySet :: anyset(), Bool :: boolean().
```

Returns `true` if `AnySet` appears to be an
[unordered set](`m:sofs#module-sets-handled-by-this-module`), and `false` if `AnySet` is an ordered
set or an atomic set or any other term.

Note that the test is shallow and this function will return `true` for any term
that coincides with the representation of an unordered set. See also note on
[data types](`e:system:data_types.md#no_user_types`).

## Examples

```erlang
1> sofs:is_set(sofs:set([1,2,3])).
true
2> sofs:is_set(sofs:from_term({a,b,c})).
false
3> sofs:is_set(42).
** exception error: no function clause matching sofs:is_set(42)
```

# `is_sofs_set`

```elixir
-spec is_sofs_set(Term) -> Bool when Bool :: boolean(), Term :: term().
```

Returns `true` if `Term` appears to be an
[unordered set](`m:sofs#module-sets-handled-by-this-module`),
an ordered set, or an atomic set; otherwise, returns `false`.

Note that this function will return `true` for any term that
coincides with the representation of a `sofs` set. See also note on
[data types](`e:system:data_types.md#no_user_types`).

## Examples

```erlang
1> sofs:is_sofs_set(sofs:set([a,b,c])).
true
2> sofs:is_sofs_set(sofs:from_term(a)).
true
3> sofs:is_sofs_set(sofs:from_term({a,b,c})).
true
4> sofs:is_sofs_set(42).
false
```

# `is_subset`

```elixir
-spec is_subset(Set1, Set2) -> Bool when Bool :: boolean(), Set1 :: a_set(), Set2 :: a_set().
```

Returns `true` if `Set1` is a [subset](`m:sofs#subset`) of `Set2`; otherwise,
returns `false`.

```erlang
1> S1 = sofs:set([2,4,6]).
2> S2 = sofs:set([1,2,3,4,5,6]).
3> sofs:is_subset(S1, S2).
true
4> sofs:is_subset(S2, S1).
false
5> sofs:is_subset(S1, S1).
true
6> S3 = sofs:relation([{1,a},{2,b}]).
7> S4 = sofs:relation([{1,a}]).
8> sofs:is_subset(S4, S3).
true
9> sofs:is_subset(S3, S1).
** exception error: type_mismatch
     in function  sofs:is_subset/2
```

# `is_type`

```elixir
-spec is_type(Term) -> Bool when Bool :: boolean(), Term :: term().
```

Returns `true` if term `Term` is a [type](`m:sofs#type`).

## Examples

```erlang
1> sofs:is_type(atom).
true
2> sofs:is_type([atom]).
true
3> sofs:is_type({a,b}).
true
4> sofs:is_type(42).
false
```

# `join`

```elixir
-spec join(Relation1, I, Relation2, J) -> Relation3
              when
                  Relation1 :: relation(),
                  Relation2 :: relation(),
                  Relation3 :: relation(),
                  I :: pos_integer(),
                  J :: pos_integer().
```

Returns the [natural join](`m:sofs#natural_join`) of the relations `Relation1`
and `Relation2` on coordinates `I` and `J`.

## Examples

```erlang
1> R1 = sofs:relation([{a,x,1},{b,y,2}]).
2> R2 = sofs:relation([{1,f,g},{1,h,i},{2,3,4}]).
3> J = sofs:join(R1, 3, R2, 1).
4> sofs:to_external(J).
[{a,x,1,f,g},{a,x,1,h,i},{b,y,2,3,4}]
```

# `multiple_relative_product`

```elixir
-spec multiple_relative_product(TupleOfBinRels, BinRel1) -> BinRel2
                                   when
                                       TupleOfBinRels :: tuple_of(BinRel),
                                       BinRel :: binary_relation(),
                                       BinRel1 :: binary_relation(),
                                       BinRel2 :: binary_relation().
```

If `TupleOfBinRels` is a non-empty tuple \{R\[1], ..., R\[n]\} of binary
relations and `BinRel1` is a binary relation, then `BinRel2` is the
[multiple relative product](`m:sofs#multiple_relative_product`) of the ordered
set (R\[i], ..., R\[n]) and `BinRel1`.

## Examples

```erlang
1> Ri = sofs:relation([{a,1},{b,2},{c,3}]).
2> R = sofs:relation([{a,b},{b,c},{c,a}]).
3> MP = sofs:multiple_relative_product({Ri, Ri}, R).
4> sofs:to_external(sofs:range(MP)).
[{1,2},{2,3},{3,1}]
```

# `no_elements`

```elixir
-spec no_elements(ASet) -> NoElements when ASet :: a_set() | ordset(), NoElements :: non_neg_integer().
```

Returns the number of elements of the ordered or unordered set `ASet`.

## Examples

```erlang
1> sofs:no_elements(sofs:set([a,b,c])).
3
2> sofs:no_elements(sofs:relation([{1,a}])).
1
3> sofs:no_elements(sofs:from_term({1,2,3,4})).
4
4> sofs:no_elements(sofs:from_term(a)).
** exception error: bad argument
     in function  sofs:no_elements/1
```

# `partition`

```elixir
-spec partition(SetOfSets) -> Partition when SetOfSets :: set_of_sets(), Partition :: a_set().
```

Returns the [partition](`m:sofs#partition`) of the union of the set of sets
`SetOfSets` such that two elements are considered equal if they belong to the
same elements of `SetOfSets`.

## Examples

```erlang
1> Sets1 = sofs:from_term([[a,b,c],[d,e,f],[g,h,i]]).
2> Sets2 = sofs:from_term([[b,c,d],[e,f,g],[h,i,j]]).
3> P = sofs:partition(sofs:union(Sets1, Sets2)).
4> sofs:to_external(P).
[[a],[b,c],[d],[e,f],[g],[h,i],[j]]
```

# `partition`

```elixir
-spec partition(SetFun, Set) -> Partition when SetFun :: set_fun(), Partition :: a_set(), Set :: a_set().
```

Returns the [partition](`m:sofs#partition`) of `Set` such that two elements are
considered equal if the results of applying `SetFun` are equal.

## Examples

```erlang
1> Ss = sofs:from_term([[a],[b],[c,d],[e,f]]).
2> SetFun = fun(S) -> sofs:from_term(sofs:no_elements(S)) end.
3> P = sofs:partition(SetFun, Ss).
4> sofs:to_external(P).
[[[a],[b]],[[c,d],[e,f]]]
```

# `partition`

```elixir
-spec partition(SetFun, Set1, Set2) -> {Set3, Set4}
                   when
                       SetFun :: set_fun(),
                       Set1 :: a_set(),
                       Set2 :: a_set(),
                       Set3 :: a_set(),
                       Set4 :: a_set().
```

Returns a pair of sets that, regarded as constituting a set, forms a
[partition](`m:sofs#partition`) of `Set1`.

If the result of applying `SetFun` to an element of `Set1` gives an
element in `Set2`, the element belongs to `Set3`, otherwise the
element belongs to `Set4`.

[`partition(F, S1, S2)`](`partition/3`) is equivalent to
`{restriction(F, S1, S2), drestriction(F, S1, S2)}`.

## Examples

```erlang
1> R1 = sofs:relation([{1,a},{2,b},{3,c}]).
2> S = sofs:set([2,4,6]).
3> {R2,R3} = sofs:partition(1, R1, S).
4> {sofs:to_external(R2),sofs:to_external(R3)}.
{[{2,b}],[{1,a},{3,c}]}
```

# `partition_family`

```elixir
-spec partition_family(SetFun, Set) -> Family
                          when Family :: family(), SetFun :: set_fun(), Set :: a_set().
```

Returns [family](`m:sofs#family`) `Family` where the indexed set is a
[partition](`m:sofs#partition`) of `Set` such that two elements are considered
equal if the results of applying `SetFun` are the same value i.

This is the index that `Family` maps onto the [equivalence
class](`m:sofs#equivalence_class`).

## Examples

```erlang
1> S = sofs:relation([{a,a,a,a},{a,a,b,b},{a,b,b,b}]).
2> SetFun = {external, fun({A,_,C,_}) -> {A,C} end}.
3> F = sofs:partition_family(SetFun, S).
4> sofs:to_external(F).
[{{a,a},[{a,a,a,a}]},{{a,b},[{a,a,b,b},{a,b,b,b}]}]
```

# `product`

```elixir
-spec product(TupleOfSets) -> Relation when Relation :: relation(), TupleOfSets :: tuple_of(a_set()).
```

Returns the [Cartesian product](`m:sofs#Cartesian_product_tuple`) of the
non-empty tuple of sets `TupleOfSets`.

If (x\[1], ..., x\[n]) is an element of the n-ary relation `Relation`,
then x\[i] is drawn from element i of `TupleOfSets`.

## Examples

```erlang
1> S1 = sofs:set([a,b]).
2> S2 = sofs:set([1,2]).
3> S3 = sofs:set([x,y]).
4> P3 = sofs:product({S1,S2,S3}).
5> sofs:to_external(P3).
[{a,1,x},{a,1,y},{a,2,x},{a,2,y},{b,1,x},{b,1,y},{b,2,x},{b,2,y}]
```

# `product`

```elixir
-spec product(Set1, Set2) -> BinRel when BinRel :: binary_relation(), Set1 :: a_set(), Set2 :: a_set().
```

Returns the [Cartesian product](`m:sofs#Cartesian_product`) of `Set1` and
`Set2`.

## Examples

```erlang
1> S1 = sofs:set([1,2]).
2> S2 = sofs:set([a,b]).
3> R = sofs:product(S1, S2).
4> sofs:to_external(R).
[{1,a},{1,b},{2,a},{2,b}]
```

[`product(S1, S2)`](`product/2`) is equivalent to
[`product({S1, S2})`](`product/1`).

# `projection`

```elixir
-spec projection(SetFun, Set1) -> Set2 when SetFun :: set_fun(), Set1 :: a_set(), Set2 :: a_set().
```

Returns the set created by substituting each element of `Set1` by the result of
applying `SetFun` to the element.

If `SetFun` is a number i >= 1 and `Set1` is a relation, then the returned set
is the [projection](`m:sofs#projection`) of `Set1` onto coordinate i.

## Examples

```erlang
1> S1 = sofs:from_term([{1,a},{2,b},{3,a}]).
2> S2 = sofs:projection(2, S1).
3> sofs:to_external(S2).
[a,b]
```

Projecting using an external SetFun.

```erlang
1> S1 = sofs:relation([{1,2,7}, {4,3,2}]).
2> SetFun = {external,fun({X,_,Z}) -> {X,Z} end}.
3> S2 = sofs:projection(SetFun, S1).
4> sofs:to_external(S2).
[{1,7},{4,2}]
```

# `range`

```elixir
-spec range(BinRel) -> Set when BinRel :: binary_relation(), Set :: a_set().
```

Returns the [range](`m:sofs#range`) of the binary relation `BinRel`.

## Examples

```erlang
1> R = sofs:relation([{1,a},{1,b},{2,b},{2,c}]).
2> S = sofs:range(R).
3> sofs:to_external(S).
[a,b,c]
```

# `relation`

```elixir
-spec relation(Tuples) -> Relation when Relation :: relation(), Tuples :: [tuple()].
```

Equivalent to [`relation(Tuples, Type)`](`relation/2`), where `Type` is the size
of the first tuple of `Tuples`, if such a tuple exists.

If tuples is `[]`, then `Type` is `2`.

## Examples

```erlang
1> S1 = sofs:relation([{1,a},{1,b},{1,a}]).
2> sofs:to_external(S1).
[{1,a},{1,b}]
3> sofs:type(S1).
[{atom,atom}]
4> sofs:type(sofs:relation([])).
[{atom,atom}]
5> sofs:type(sofs:relation([], 3)).
[{atom,atom,atom}]
6> sofs:relation([a,b,c]).
** exception error: bad argument
     in function  sofs:relation/1
```

# `relation`

```elixir
-spec relation(Tuples, Type) -> Relation
                  when N :: integer(), Type :: N | type(), Relation :: relation(), Tuples :: [tuple()].
```

Creates a [relation](`m:sofs#relation`).

[`relation(R, T)`](`relation/2`) is equivalent to
[`from_term(R, T)`](`from_term/2`), if T is a [type](`m:sofs#type`)
and the result is a relation.

If `Type` is an integer N, then `[{atom, ..., atom}])`, where the tuple size is N,
is used as type of the relation.

## Examples

```erlang
1> S1 = sofs:relation([{3,blue},{2,green},{3,blue},{1,red}], [{index,color}]).
2> sofs:to_external(S1).
[{1,red},{2,green},{3,blue}]
3> sofs:type(S1).
[{index,color}]
4> sofs:type(sofs:relation([{1,a},{1,b}], 2)).
[{atom,atom}]
5> sofs:type(sofs:relation([], 3)).
[{atom,atom,atom}]
```

# `relation_to_family`

```elixir
-spec relation_to_family(BinRel) -> Family when Family :: family(), BinRel :: binary_relation().
```

Returns [family](`m:sofs#family`) `Family` such that the index set is equal to
the [domain](`m:sofs#domain`) of the binary relation `BinRel`, and `Family`\[i]
is the [image](`m:sofs#image`) of the set of i under `BinRel`.

## Examples

```erlang
1> R = sofs:relation([{b,1},{c,2},{c,3}]).
2> F = sofs:relation_to_family(R).
3> sofs:to_external(F).
[{b,[1]},{c,[2,3]}]
```

# `relative_product1`

```elixir
-spec relative_product1(BinRel1, BinRel2) -> BinRel3
                           when
                               BinRel1 :: binary_relation(),
                               BinRel2 :: binary_relation(),
                               BinRel3 :: binary_relation().
```

Returns the [relative product](`m:sofs#relative_product`) of the
[converse](`m:sofs#converse`) of the binary relation `BinRel1` and the binary
relation `BinRel2`.

## Examples

```erlang
1> R1 = sofs:relation([{1,a},{1,aa},{2,b}]).
2> R2 = sofs:relation([{1,u},{2,v},{3,c}]).
3> R3 = sofs:relative_product1(R1, R2).
4> sofs:to_external(R3).
[{a,u},{aa,u},{b,v}]
```

[`relative_product1(R1, R2)`](`relative_product1/2`) is equivalent to
[`relative_product(converse(R1), R2)`](`relative_product/2`).

# `relative_product`

```elixir
-spec relative_product(ListOfBinRels) -> BinRel2
                          when
                              ListOfBinRels :: [BinRel, ...],
                              BinRel :: binary_relation(),
                              BinRel2 :: binary_relation().
```

Returns [relative product](`m:sofs#tuple_relative_product`) of the ordered set
(R\[i], ..., R\[n]) and the relation of equality between the elements of the
[Cartesian product](`m:sofs#Cartesian_product_tuple`) of the ranges of R\[i],
range R\[1] × ... × range R\[n].

## Examples

```erlang
1> TR = sofs:relation([{1,a},{1,aa},{2,b},{4,x}]).
2> R1 = sofs:relation([{1,u},{2,v},{3,c}]).
3> R2 = sofs:relative_product([TR, R1]).
4> sofs:to_external(R2).
[{1,{a,u}},{1,{aa,u}},{2,{b,v}}]
```

# `relative_product`

```elixir
-spec relative_product(ListOfBinRels, BinRel1) -> BinRel2
                          when
                              ListOfBinRels :: [BinRel, ...],
                              BinRel :: binary_relation(),
                              BinRel1 :: binary_relation(),
                              BinRel2 :: binary_relation();
                      (BinRel1, BinRel2) -> BinRel3
                          when
                              BinRel1 :: binary_relation(),
                              BinRel2 :: binary_relation(),
                              BinRel3 :: binary_relation().
```

Returns the [relative product](`m:sofs#tuple_relative_product`).

If `ListOrRel` is a non-empty list [R[1], ..., R[n]] of binary relations
and `BinRel1` is a binary relation, then `BinRel2` is the
[relative product](`m:sofs#tuple_relative_product`) of the ordered set
(R\[i], ..., R\[n]) and `BinRel1`.

Notice that [`relative_product([R1], R2)`](`relative_product/2`) is different
from [`relative_product(R1, R2)`](`relative_product/2`); the list of one element
is not identified with the element itself.

## Examples

```erlang
1> R1 = sofs:relation([{a,b},{c,a}]).
2> R2 = sofs:relation([{a,1},{a,2}]).
3> S = sofs:from_term([{{b,1},b1},{{b,2},b2}]).
4> R3 = sofs:relative_product([R1,R2], S).
5> sofs:to_external(R3).
[{a,b1},{a,b2}]
```

If `ListOrRel` is a binary relation, then `BinRel2` is the
[relative product](`m:sofs#relative_product`) of the binary
relations `ListOfRel` and `BinRel1`.

## Examples

```erlang
1> R1 = sofs:relation([{a,b}, {c,a}]).
2> R2 = sofs:relation([{a,1}, {a,2}]).
3> R3 = sofs:relative_product(R1, R2).
4> sofs:to_external(R3).
[{c,1},{c,2}]
```

# `restriction`

```elixir
-spec restriction(BinRel1, Set) -> BinRel2
                     when BinRel1 :: binary_relation(), BinRel2 :: binary_relation(), Set :: a_set().
```

Returns the [restriction](`m:sofs#restriction`) of the binary relation `BinRel1`
to `Set`.

## Examples

```erlang
1> R1 = sofs:relation([{1,a},{2,b},{3,c}]).
2> S = sofs:set([1,2,4]).
3> R2 = sofs:restriction(R1, S).
4> sofs:to_external(R2).
[{1,a},{2,b}]
```

# `restriction`

```elixir
-spec restriction(SetFun, Set1, Set2) -> Set3
                     when SetFun :: set_fun(), Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns a subset of `Set1` containing those elements that gives an element in
`Set2` as the result of applying `SetFun`.

## Examples

```erlang
1> S1 = sofs:relation([{1,a},{2,b},{3,c}]).
2> S2 = sofs:set([b,c,d]).
3> S3 = sofs:restriction(2, S1, S2).
4> sofs:to_external(S3).
[{2,b},{3,c}]
```

# `set`

```elixir
-spec set(Terms) -> Set when Set :: a_set(), Terms :: [term()].
```

# `set`

```elixir
-spec set(Terms, Type) -> Set when Set :: a_set(), Terms :: [term()], Type :: type().
```

Creates an [unordered set](`m:sofs#module-sets-handled-by-this-module`).

[`set(L, T)`](`set/2`) is equivalent to
[`from_term(L, T)`](`from_term/2`) if the result is an unordered set.

## Examples

```erlang
1> S1 = sofs:set([3,1,2,3,4], [digit]).
2> sofs:to_external(S1).
[1,2,3,4]
3> S2 = sofs:from_term([1,2,3,4], [digit]).
4> sofs:is_equal(S1, S2).
true
```

# `specification`

```elixir
-spec specification(Fun, Set1) -> Set2 when Fun :: spec_fun(), Set1 :: a_set(), Set2 :: a_set().
```

Returns the set containing every element of `Set1` for which `Fun` returns
`true`.

If `Fun` is a tuple `{external, Fun2}`, `Fun2` is applied to the
[external set](`m:sofs#external_set`) of each element, otherwise `Fun` is
applied to each element.

## Examples

```erlang
1> R1 = sofs:relation([{a,1},{b,2}]).
2> R2 = sofs:relation([{x,1},{x,2},{y,3}]).
3> S1 = sofs:from_sets([R1,R2]).
4> S2 = sofs:specification(fun sofs:is_a_function/1, S1).
5> sofs:to_external(S2).
[[{a,1},{b,2}]]
```

Using an external fun.

```erlang
1> S1 = sofs:set([1,2,3,4,5,6,7]).
2> SetFun = {external,fun(E) -> E rem 2 =:= 0 end}.
3> S2 = sofs:specification(SetFun, S1).
4> sofs:to_external(S2).
[2,4,6]
```

# `strict_relation`

```elixir
-spec strict_relation(BinRel1) -> BinRel2
                         when BinRel1 :: binary_relation(), BinRel2 :: binary_relation().
```

Returns the [strict relation](`m:sofs#strict_relation`) corresponding to the
binary relation `BinRel1`.

## Examples

```erlang
1> R1 = sofs:relation([{1,1},{1,2},{2,1},{2,2}]).
2> R2 = sofs:strict_relation(R1).
3> sofs:to_external(R2).
[{1,2},{2,1}]
```

# `substitution`

```elixir
-spec substitution(SetFun, Set1) -> Set2 when SetFun :: set_fun(), Set1 :: a_set(), Set2 :: a_set().
```

Returns a function with the domain `Set1`, where each element maps to
the result of applying `SetFun` to it.

## Examples

```erlang
1> R = sofs:relation([{a,1},{b,2}]).
2> sofs:to_external(sofs:projection(1, R)).
[a,b]
3> sofs:to_external(sofs:substitution(1, R)).
[{{a,1},a},{{b,2},b}]
4> SetFun = {external, fun({A,_}=E) -> {E,A} end}.
5> sofs:to_external(sofs:projection(SetFun, R)).
[{{a,1},a},{{b,2},b}]
```

The relation of equality between the elements of {a,b,c}:

```erlang
1> I = sofs:substitution(fun(A) -> A end, sofs:set([a,b,c])).
2> sofs:to_external(I).
[{a,a},{b,b},{c,c}]
```

Let `SetOfSets` be a set of sets and `BinRel` a binary relation. The function
that maps each element `Set` of `SetOfSets` onto the [image](`m:sofs#image`) of
`Set` under `BinRel` is returned by the `Images` fun in the following example.

```erlang
1> Images = fun(SetOfSets, BinRel) ->
                    Fun = fun(Set) -> sofs:image(BinRel, Set) end,
                    sofs:substitution(Fun, SetOfSets)
            end.
2> S1 = sofs:set([1,2]).
3> S2 = sofs:set([1,3,4]).
4> S3 = sofs:set([x]).
5> SetsOfSets = sofs:from_sets([S1,S2,S3]).
6> BinRel = sofs:relation([{1,a}, {2,b}, {3,c}, {4,d}]).
7> S4 = Images(SetsOfSets, BinRel).
8> sofs:to_external(S4).
[{[1,2],[a,b]},{[1,3,4],[a,c,d]},{[x],[]}]
```

External unordered sets are represented as sorted lists. So, creating
the image of a set under a relation R can traverse all elements of R
(to that comes the sorting of results, the image). In the `Image` fun,
`BinRel` is traversed once for each element of `SetOfSets`.

The following `Images2` fun is more efficient. It can can be used
under the assumption that the image of each element of `SetOfSets`
under `BinRel` is non-empty.

```erlang
1> Images2 = fun(SetOfSets, BinRel) ->
                 CR = sofs:canonical_relation(SetOfSets),
                 R = sofs:relative_product1(CR, BinRel),
                 sofs:relation_to_family(R)
   end.
2> S1 = sofs:set([1,2]).
3> S2 = sofs:set([1,3,4]).
4> S3 = sofs:set([x]).
5> SetsOfSets = sofs:from_sets([S1,S2,S3]).
6> BinRel = sofs:relation([{1,a}, {2,b}, {3,c}, {4,d}]).
7> S4 = Images2(SetsOfSets, BinRel).
8> sofs:to_external(S4).
[{[1,2],[a,b]},{[1,3,4],[a,c,d]}]
```

Note that `S3`, which has an empty image, is missing from the result.

# `symdiff`

```elixir
-spec symdiff(Set1, Set2) -> Set3 when Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns the [symmetric difference](`m:sofs#symmetric_difference`) (or the
Boolean sum) of `Set1` and `Set2`.

## Examples

```erlang
1> S1 = sofs:set([1,2,3]).
2> S2 = sofs:set([2,3,4]).
3> P = sofs:symdiff(S1, S2).
4> sofs:to_external(P).
[1,4]
```

# `symmetric_partition`

```elixir
-spec symmetric_partition(Set1, Set2) -> {Set3, Set4, Set5}
                             when
                                 Set1 :: a_set(),
                                 Set2 :: a_set(),
                                 Set3 :: a_set(),
                                 Set4 :: a_set(),
                                 Set5 :: a_set().
```

Returns the symmetric partition of `Set1` and `Set2`.

Returns a triple of sets:

- `Set3` contains the elements of `Set1` that do not belong to `Set2`.
- `Set4` contains the elements of `Set1` that belong to `Set2`.
- `Set5` contains the elements of `Set2` that do not belong to `Set1`.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([c,d,e]).
3> {S3,S4,S5} = sofs:symmetric_partition(S1, S2).
4> {sofs:to_external(S3),sofs:to_external(S4),sofs:to_external(S5)}
{[a,b],[c],[d,e]}
```

# `to_external`

```elixir
-spec to_external(AnySet) -> ExternalSet when ExternalSet :: external_set(), AnySet :: anyset().
```

Returns the [external set](`m:sofs#external_set`) of an atomic, ordered, or
unordered set.

```erlang
1> sofs:to_external(sofs:set([2,3,1])).
[1,2,3]
2> sofs:to_external(sofs:from_term({2,3,1})).
{2,3,1}
3> sofs:to_external(sofs:from_term(a)).
a
```

# `to_sets`

```elixir
-spec to_sets(ASet) -> Sets
                 when
                     ASet :: a_set() | ordset(), Sets :: tuple_of(AnySet) | [AnySet], AnySet :: anyset().
```

Returns the elements of the ordered set `ASet` as a tuple of sets, and the
elements of the unordered set `ASet` as a sorted list of sets without
duplicates.

## Examples

```erlang
1> [S1,S2,S3] = sofs:to_sets(sofs:set([3,2,1])).
2> {sofs:to_external(S1),sofs:to_external(S2),sofs:to_external(S3)}.
{1,2,3}
3> {S4,S5,S6} = sofs:to_sets(sofs:from_term({c,a,b})).
4> {sofs:to_external(S4),sofs:to_external(S5),sofs:to_external(S6)}.
{c,a,b}
```

# `type`

```elixir
-spec type(AnySet) -> Type when AnySet :: anyset(), Type :: type().
```

Returns the [type](`m:sofs#type`) of an atomic, ordered, or unordered set.

## Examples

Unordered sets.

```erlang
1> sofs:type(sofs:empty_set()).
['_']
2> sofs:type(sofs:set([], [color])).
[color]
3> sofs:type(sofs:set([red,green,blue], [color])).
[color]
4> sofs:type(sofs:set([1,2,3])).
[atom]
```

Ordered sets.

```erlang
1> sofs:type(sofs:from_term({a,b,c})).
{atom,atom,atom}
2> sofs:type(sofs:from_term({1.0,2.5,-1.0}, {x,y,z})).
{x,y,z}
```

Atomic sets.

```erlang
1> sofs:type(sofs:from_term(a)).
atom
2> sofs:type(sofs:from_term(1, index)).
index
```

# `union`

```elixir
-spec union(SetOfSets) -> Set when Set :: a_set(), SetOfSets :: set_of_sets().
```

Returns the [union](`m:sofs#union_n`) of the set of sets `SetOfSets`.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([b,1,2]).
3> S3 = sofs:set([a,d,e])
4> S4 = sofs:from_sets([S1,S2,S3]).
5> S5 = sofs:union(S4).
6> sofs:to_external(S5).
[1,2,a,b,c,d,e]
```

# `union`

```elixir
-spec union(Set1, Set2) -> Set3 when Set1 :: a_set(), Set2 :: a_set(), Set3 :: a_set().
```

Returns the [union](`m:sofs#union`) of `Set1` and `Set2`.

## Examples

```erlang
1> S1 = sofs:set([a,b,c]).
2> S2 = sofs:set([c,d,1,2,3]).
3> S3 = sofs:union(S1, S2).
4> sofs:to_external(S3).
[1,2,3,a,b,c,d]
```

# `union_of_family`

```elixir
-spec union_of_family(Family) -> Set when Family :: family(), Set :: a_set().
```

Returns the union of [family](`m:sofs#family`) `Family`.

## Examples

```erlang
1> F = sofs:family([{a,[0,2,4]},{b,[0,1,2]},{c,[2,3]}]).
2> S = sofs:union_of_family(F).
3> sofs:to_external(S).
[0,1,2,3,4]
```

# `weak_relation`

```elixir
-spec weak_relation(BinRel1) -> BinRel2 when BinRel1 :: binary_relation(), BinRel2 :: binary_relation().
```

Returns a subset S of the [weak relation](`m:sofs#weak_relation`) W
corresponding to the binary relation `BinRel1`.

Let F be the [field](`m:sofs#field`) of `BinRel1`. The subset S is
defined so that x S y if x W y for some x in F and for some y in F.

## Examples

```erlang
1> R1 = sofs:relation([{1,1},{1,2},{3,1}]).
2> R2 = sofs:weak_relation(R1).
3> sofs:to_external(R2).
[{1,1},{1,2},{2,2},{3,1},{3,3}]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
