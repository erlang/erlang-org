# `lists`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/lists.erl#L22)

List processing functions.

This module contains functions for list processing.

Unless otherwise stated, all functions assume that position numbering starts
at 1. That is, the first element of a list is at position 1.

Two terms `T1` and `T2` compare equal if `T1 == T2` evaluates to `true`. They
match if `T1 =:= T2` evaluates to `true`.

Whenever an _ordering function_{: #ordering_function } `F` is expected as
argument, it is assumed that the following properties hold of `F` for all x, y,
and z:

- If x `F` y and y `F` x, then x = y (`F` is antisymmetric).
- If x `F` y and y `F` z, then x `F` z (`F` is transitive).
- x `F` y or y `F` x (`F` is total).

An example of a typical ordering function is less than or equal to: `=</2`.

# `all`

```erlang
-spec all(Pred, List) -> boolean() when Pred :: fun((Elem :: T) -> boolean()), List :: [T], T :: term().
```

Returns `true` if `Pred(Elem)` returns `true` for all elements `Elem` in `List`;
otherwise, returns `false`.

## Examples

```erlang
1> IsEven = fun(N) -> N rem 2 =:= 0 end.
2> lists:all(IsEven, [2,4,5]).
false
3> lists:all(IsEven, [2,4,6]).
true
```

# `any`

```erlang
-spec any(Pred, List) -> boolean() when Pred :: fun((Elem :: T) -> boolean()), List :: [T], T :: term().
```

Returns `true` if `Pred(Elem)` returns `true` for at least one element `Elem` in
`List`; otherwise, returns `false`.

## Examples

```erlang
1> IsEven = fun(N) -> N rem 2 =:= 0 end.
2> lists:any(IsEven, [3,5,7]).
false
3> lists:any(IsEven, [2,3,5,7]).
true
```

# `append`

```erlang
-spec append(ListOfLists) -> List1 when ListOfLists :: [List], List :: [T], List1 :: [T], T :: term().
```

Returns a list in which all sublists of `ListOfLists` have been concatenated.

## Examples

```erlang
1> lists:append([[1, 2, 3], [a, b], [4, 5, 6]]).
[1,2,3,a,b,4,5,6]
```

# `append`

```erlang
-spec append(List1, List2) -> List3 when List1 :: [T], List2 :: [T], List3 :: [T], T :: term().
```

Returns a new list, `List3`, consisting of the elements of
`List1`, followed by the elements of `List2`.

## Examples

```erlang
1> lists:append("abc", "def").
"abcdef"
```

`lists:append(A, B)` is equivalent to `A ++ B`.

# `concat`

```erlang
-spec concat(Things) -> string()
                when Things :: [Thing], Thing :: atom() | integer() | float() | string().
```

Concatenates the text representation of the elements of `Things`.

The elements of `Things` can be atoms, integers, floats, or strings.

## Examples

```erlang
1> lists:concat([doc, '/', file, '.', 3]).
"doc/file.3"
```

# `delete`

```erlang
-spec delete(Elem, List1) -> List2 when Elem :: T, List1 :: [T], List2 :: [T], T :: term().
```

Returns a copy of `List1` where the first element matching `Elem` is removed, if
there is such an element.

## Examples

```erlang
1> lists:delete(b, [a,b,c]).
[a,c]
2> lists:delete(x, [a,b,c]).
[a,b,c]
```

# `droplast`
*since OTP 17.0* 

```erlang
-spec droplast(List) -> InitList when List :: [T, ...], InitList :: [T], T :: term().
```

Drops the last element of a `List`.

The list must be non-empty; otherwise, the function raises a
`function_clause` exception.

## Examples

```erlang
1> lists:droplast([1]).
[]
2> lists:droplast([1,2,3]).
[1,2]
3> lists:droplast([]).
** exception error: no function clause matching lists:droplast([])
```

# `dropwhile`

```erlang
-spec dropwhile(Pred, List1) -> List2
                   when Pred :: fun((Elem :: T) -> boolean()), List1 :: [T], List2 :: [T], T :: term().
```

Drops elements `Elem` from `List1` while `Pred(Elem)` returns `true`,
and then returns the remaining list.

## Examples

```erlang
1> lists:dropwhile(fun is_atom/1, [a,b,c,1,2,3,x,y,z]).
[1,2,3,x,y,z]
2> lists:dropwhile(fun is_integer/1, [a,b,c,1,2,3,x,y,z]).
[a,b,c,1,2,3,x,y,z]
```

# `duplicate`

```erlang
-spec duplicate(N, Elem) -> List when N :: non_neg_integer(), Elem :: T, List :: [T], T :: term().
```

Returns a list containing `N` copies of term `Elem`.

## Examples

```erlang
1> lists:duplicate(5, xx).
[xx,xx,xx,xx,xx]
```

# `enumerate`
*since OTP 25.0* 

```erlang
-spec enumerate(List1) -> List2
                   when List1 :: [T], List2 :: [{Index, T}], Index :: integer(), T :: term().
```

# `enumerate`
*since OTP 25.0* 

```erlang
-spec enumerate(Index, List1) -> List2
                   when List1 :: [T], List2 :: [{Index, T}], Index :: integer(), T :: term().
```

# `enumerate`
*since OTP 26.0* 

```erlang
-spec enumerate(Index, Step, List1) -> List2
                   when
                       List1 :: [T],
                       List2 :: [{Index, T}],
                       Index :: integer(),
                       Step :: integer(),
                       T :: term().
```

Returns `List1` with each element `H` replaced by a tuple of form `{I, H}`, where
`I` is the position of `H` in `List1`.

The enumeration starts with `Index` and increases by `Step` in each
step.

That is, [`enumerate/3`](`enumerate/3`) behaves as if it were defined as
follows:

```erlang
enumerate(I, S, List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc+S} end, I, List),
  List1.
```

The default values for `Index` and `Step` are both `1`.

## Examples

```erlang
1> lists:enumerate([a,b,c]).
[{1,a},{2,b},{3,c}]
2> lists:enumerate(10, [a,b,c]).
[{10,a},{11,b},{12,c}]
3> lists:enumerate(0, -2, [a,b,c]).
[{0,a},{-2,b},{-4,c}]
```

# `filter`

```erlang
-spec filter(Pred, List1) -> List2
                when Pred :: fun((Elem :: T) -> boolean()), List1 :: [T], List2 :: [T], T :: term().
```

Returns a list of elements `Elem` in `List1` for which `Pred(Elem)`
returns `true`.

## Examples

```erlang
1> IsEven = fun(N) -> N rem 2 =:= 0 end.
2> lists:filter(IsEven, [1,2,3,4,5]).
[2,4]
```

# `filtermap`
*since OTP R16B01* 

```erlang
-spec filtermap(Fun, List1) -> List2
                   when
                       Fun :: fun((Elem) -> boolean() | {true, Value}),
                       List1 :: [Elem],
                       List2 :: [Elem | Value],
                       Elem :: term(),
                       Value :: term().
```

Calls `Fun(Elem)` on successive elements `Elem` of `List1` to update or
remove elements from `List1`.

`Fun/1` must return either a Boolean or a tuple `{true, Value}`. The
function returns the list of elements for which `Fun` returns a new
value, with `true` being equivalent to `{true, Elem}`.

That is, `filtermap` behaves as if it were defined as follows:

```erlang
filtermap(Fun, List1) ->
    lists:flatmap(fun(Elem) ->
                          case Fun(Elem) of
                              false -> [];
                              true -> [Elem];
                              {true,Value} -> [Value]
                          end
                  end, List1).
```

## Examples

```erlang
1> lists:filtermap(fun(X) ->
                           case X rem 2 of
                               0 -> {true, X div 2};
                               1 -> false
                           end
                   end, [1,2,3,4,5]).
[1,2]
```

# `flatlength`

```erlang
-spec flatlength(DeepList) -> non_neg_integer() when DeepList :: [term() | DeepList].
```

Equivalent to [`length(flatten(DeepList))`](`length/1`), but more efficient.

## Examples

```erlang
1> lists:flatlength([a,[b,c,[d,e]],f,[[g,h,i]]]).
9
2> lists:flatlength([[[]]]).
0
```

# `flatmap`

```erlang
-spec flatmap(Fun, List1) -> List2
                 when Fun :: fun((A) -> [B]), List1 :: [A], List2 :: [B], A :: term(), B :: term().
```

Takes a function from `A`s to lists of `B`s, and a list of `A`s (`List1`),
producing a list of `B`s by applying the function to each element in `List1` and
appending the resulting lists.

That is, `flatmap` behaves as if it were defined as follows:

```erlang
flatmap(Fun, List1) ->
    lists:append(lists:map(Fun, List1)).
```

## Examples

```erlang
1> lists:flatmap(fun(X)-> [X,X] end, [a,b,c]).
[a,a,b,b,c,c]
2> F = fun(N) when is_integer(N) -> [10 * N];
          (_) -> []
       end, ok.
3> lists:flatmap(F, [1,2,a,b,c,3]).
[10,20,30]
```

# `flatten`

```erlang
-spec flatten(DeepList) -> List when DeepList :: [term() | DeepList], List :: [term()].
```

Returns a flattened version of `DeepList`.

## Examples

```erlang
1> lists:flatten([a,[b,c,[d,e]],f]).
[a,b,c,d,e,f]
```

# `flatten`

```erlang
-spec flatten(DeepList, Tail) -> List
                 when DeepList :: [term() | DeepList], Tail :: [term()], List :: [term()].
```

Returns a flattened version of `DeepList` with tail `Tail` appended.

## Examples

```erlang
1> lists:flatten([a,[b,c,[d,e]],f], [g,h,i]).
[a,b,c,d,e,f,g,h,i]
```

# `foldl`

```erlang
-spec foldl(Fun, Acc0, List) -> Acc1
               when
                   Fun :: fun((Elem :: T, AccIn) -> AccOut),
                   Acc0 :: term(),
                   Acc1 :: term(),
                   AccIn :: term(),
                   AccOut :: term(),
                   List :: [T],
                   T :: term().
```

Calls `Fun(Elem, AccIn)` on successive elements `A` of `List`, starting with
`AccIn` bound to `Acc0`.

`Fun/2` must return a new accumulator, which is passed to the next
call. The function returns the final value of the accumulator. `Acc0`
is returned if the list is empty.

## Examples

```erlang
1> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).
15
2> lists:foldl(fun(X, Prod) -> X * Prod end, 1, [1,2,3,4,5]).
120
```

# `foldr`

```erlang
-spec foldr(Fun, Acc0, List) -> Acc1
               when
                   Fun :: fun((Elem :: T, AccIn) -> AccOut),
                   Acc0 :: term(),
                   Acc1 :: term(),
                   AccIn :: term(),
                   AccOut :: term(),
                   List :: [T],
                   T :: term().
```

Like `foldl/3`, but the list is traversed from right to left.

## Examples

```erlang
1> P = fun(A, AccIn) -> [A|AccIn] end.
2> lists:foldl(P, [], [1,2,3]).
[3,2,1]
3> lists:foldr(P, [], [1,2,3]).
[1,2,3]
```

[`foldl/3`](`foldl/3`) is tail-recursive and is usually preferred to
[`foldr/3`](`foldr/3`).

# `foreach`

```erlang
-spec foreach(Fun, List) -> ok when Fun :: fun((Elem :: T) -> term()), List :: [T], T :: term().
```

Calls `Fun(Elem)` for each element `Elem` in `List`, ignoring the return value.

This function is used for its side effects and the evaluation order is
defined to be the same as the order of the elements in the list.

# `join`
*since OTP 19.0* 

```erlang
-spec join(Sep, List1) -> List2 when Sep :: T, List1 :: [T], List2 :: [T], T :: term().
```

Inserts `Sep` between each element in `List1`.

Has no effect on an empty list or a singleton list.

## Examples

```erlang
1> lists:join(x, [a,b,c]).
[a,x,b,x,c]
2> lists:join(x, [a]).
[a]
3> lists:join(x, []).
[]
```

# `keydelete`

```erlang
-spec keydelete(Key, N, TupleList1) -> TupleList2
                   when
                       Key :: term(),
                       N :: pos_integer(),
                       TupleList1 :: [Tuple],
                       TupleList2 :: [Tuple],
                       Tuple :: tuple().
```

Returns a copy of `TupleList1`, where the first occurrence of a tuple
whose `N`th element compares equal to `Key` is removed, if there is
such a tuple.

## Examples

```erlang
1> lists:keydelete(c, 1, [{b,1}, {c,55}, {d,75}]).
[{b,1},{d,75}]
2> lists:keydelete(unknown, 1, [{b,1}, {c,55}, {d,75}]).
[{b,1},{c,55},{d,75}]
```

# `keyfind`

```erlang
-spec keyfind(Key, N, TupleList) -> Tuple | false
                 when Key :: term(), N :: pos_integer(), TupleList :: [Tuple], Tuple :: tuple().
```

Searches the list of tuples `TupleList` for a tuple whose `N`th element compares
equal to `Key`.

Returns `Tuple` if such a tuple is found; otherwise, returns `false`.

## Examples

```erlang
1> lists:keyfind(b, 1, [{a,10}, {b,20}, {c,30}]).
{b,20}
2> lists:keyfind(unknown, 1, [{a,10}, {b,20}, {c,30}]).
false
```

# `keymap`

```erlang
-spec keymap(Fun, N, TupleList1) -> TupleList2
                when
                    Fun :: fun((Term1 :: term()) -> Term2 :: term()),
                    N :: pos_integer(),
                    TupleList1 :: [Tuple],
                    TupleList2 :: [Tuple],
                    Tuple :: tuple().
```

Returns a list of tuples where, for each tuple in `TupleList1`, the `N`th
element `Term1` of the tuple has been replaced with the result of calling
`Fun(Term1)`.

## Examples

```erlang
1> Fun = fun(Atom) -> atom_to_list(Atom) end.
2> lists:keymap(Fun, 2, [{name,jane,22},{name,lizzie,20},{name,lydia,15}]).
[{name,"jane",22},{name,"lizzie",20},{name,"lydia",15}]
```

# `keymember`

```erlang
-spec keymember(Key, N, TupleList) -> boolean()
                   when Key :: term(), N :: pos_integer(), TupleList :: [Tuple], Tuple :: tuple().
```

Returns `true` if `TupleList` contains a tuple whose `N`th element compares
equal to `Key`; otherwise, returns `false`.

## Examples

```erlang
1> lists:keymember(b, 1, [{a,10}, {b,20}, {c,30}]).
true
2> lists:keymember(unknown, 1, [{a,10}, {b,20}, {c,30}]).
false
```

# `keymerge`

```erlang
-spec keymerge(N, TupleList1, TupleList2) -> TupleList3
                  when
                      N :: pos_integer(),
                      TupleList1 :: [T1],
                      TupleList2 :: [T2],
                      TupleList3 :: [T1 | T2],
                      T1 :: Tuple,
                      T2 :: Tuple,
                      Tuple :: tuple().
```

Returns the sorted list formed by merging `TupleList1` and `TupleList2`.

The merge is performed on the `N`th element of each tuple. Both
`TupleList1` and `TupleList2` must be key-sorted before evaluating
this function. When the key elements of the two tuples compare equal,
the tuple from `TupleList1` is picked before the tuple from
`TupleList2`.

## Examples

```erlang
1> lists:keymerge(2, [{b, 50}], [{c, 20}, {a, 50}]).
[{c,20},{b,50},{a,50}]
```

# `keyreplace`

```erlang
-spec keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2
                    when
                        Key :: term(),
                        N :: pos_integer(),
                        TupleList1 :: [Tuple],
                        TupleList2 :: [Tuple],
                        NewTuple :: Tuple,
                        Tuple :: tuple().
```

Returns a copy of `TupleList1` where the first occurrence of a tuple `T` whose
`N`th element compares equal to `Key` is replaced with `NewTuple`, if there is
such a tuple `T`.

## Examples

```erlang
1> lists:keyreplace(c, 1, [{b,1}, {c,55}, {d,75}], {new,tuple}).
[{b,1},{new,tuple},{d,75}]
2> lists:keyreplace(unknown, 1, [{b,1}, {c,55}, {d,75}], {new,tuple}).
[{b,1},{c,55},{d,75}]
```

# `keysearch`

```erlang
-spec keysearch(Key, N, TupleList) -> {value, Tuple} | false
                   when Key :: term(), N :: pos_integer(), TupleList :: [Tuple], Tuple :: tuple().
```

Searches the list of tuples `TupleList` for a tuple whose `N`th element compares
equal to `Key`.

Returns `{value, Tuple}` if such a tuple is found; otherwise, returns
`false`.

> #### Note {: .info }
>
> This function is retained for backward compatibility. Function `keyfind/3` is
> easier to use and more efficient.

# `keysort`

```erlang
-spec keysort(N, TupleList1) -> TupleList2
                 when N :: pos_integer(), TupleList1 :: [Tuple], TupleList2 :: [Tuple], Tuple :: tuple().
```

Returns a list of the elements in `TupleList1`, sorted by the `N`th
element of each tuple.

The sort is stable.

## Examples

```erlang
1> lists:keysort(2, [{a, 99}, {b, 17}, {c, 50}, {d, 50}]).
[{b,17},{c,50},{d,50},{a,99}]
```

# `keystore`

```erlang
-spec keystore(Key, N, TupleList1, NewTuple) -> TupleList2
                  when
                      Key :: term(),
                      N :: pos_integer(),
                      TupleList1 :: [Tuple],
                      TupleList2 :: [Tuple, ...],
                      NewTuple :: Tuple,
                      Tuple :: tuple().
```

Returns a copy of `TupleList1` with the first tuple whose `N`th
element compares equal to `Key` replaced by `NewTuple`, or with
`[NewTuple]` appended if no such tuple exists.

## Examples

```erlang
1> lists:keystore(b, 1, [{a, 10}, {b, 23}, {c, 99}], {bb, 1}).
[{a, 10}, {bb, 1}, {c, 99}]
2> lists:keystore(z, 1, [{a, 10}, {b, 23}, {c, 99}], {z, 2}).
[{a, 10}, {b, 23}, {c, 99}, {z, 2}]
```

# `keytake`

```erlang
-spec keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false
                 when
                     Key :: term(),
                     N :: pos_integer(),
                     TupleList1 :: [tuple()],
                     TupleList2 :: [tuple()],
                     Tuple :: tuple().
```

Searches the list of tuples `TupleList1` for a tuple whose `N`th
element compares equal to `Key`, returning `{value, Tuple,
TupleList2}` if found, where `TupleList2` is a copy of `TupleList1`
with the first occurrence of `Tuple` removed.

Otherwise, returns `false` if no such tuple is found.

## Examples

```erlang
1> lists:keytake(b, 1, [{a, 10}, {b, 23}, {c, 99}]).
{value,{b,23},[{a, 10},{c, 99}]}
2> lists:keytake(z, 1, [{a, 10}, {b, 23}, {c, 99}]).
false
```

# `last`

```erlang
-spec last(List) -> Last when List :: [T, ...], Last :: T, T :: term().
```

Returns the last element in `List`.

The list must be non-empty; otherwise, the function raises a
`function_clause` exception.

## Examples

```erlang
1> lists:last([1]).
1
2> lists:last([1,2,3]).
3
3> lists:last([]).
** exception error: no function clause matching lists:last([])
```

# `map`

```erlang
-spec map(Fun, List1) -> List2
             when Fun :: fun((A) -> B), List1 :: [A], List2 :: [B], A :: term(), B :: term().
```

Takes a function from `A`s to `B`s and a list of `A`s, producing a list of
`B`s by applying the function to each element in the list.

## Examples

```erlang
1> lists:map(fun(N) -> N + 1 end, [1,2,3]).
[2,3,4]
```

# `mapfoldl`

```erlang
-spec mapfoldl(Fun, Acc0, List1) -> {List2, Acc1}
                  when
                      Fun :: fun((A, AccIn) -> {B, AccOut}),
                      Acc0 :: term(),
                      Acc1 :: term(),
                      AccIn :: term(),
                      AccOut :: term(),
                      List1 :: [A],
                      List2 :: [B],
                      A :: term(),
                      B :: term().
```

Combines the operations of `map/2` and `foldl/3` into one pass.

## Examples

Summing the elements in a list and double them at the same time:

```erlang
1> lists:mapfoldl(fun(X, Sum) -> {2*X, X+Sum} end, 0, [1,2,3,4,5]).
{[2,4,6,8,10],15}
```

# `mapfoldr`

```erlang
-spec mapfoldr(Fun, Acc0, List1) -> {List2, Acc1}
                  when
                      Fun :: fun((A, AccIn) -> {B, AccOut}),
                      Acc0 :: term(),
                      Acc1 :: term(),
                      AccIn :: term(),
                      AccOut :: term(),
                      List1 :: [A],
                      List2 :: [B],
                      A :: term(),
                      B :: term().
```

Combines the operations of `map/2` and `foldr/3` into one pass.

> #### Note {: .info }
>
> Unless the order in which the elements are accumulated is important,
> prefer [`mapfoldl/3`](`mapfoldl/3`) as it is slightly more efficient.

## Examples

Doubling the elements in list and producing a list of squares at the
same time:

```erlang
1> lists:mapfoldr(fun(X, Acc) -> {2*X, [X*X|Acc]} end, [], [1,2,3,4,5]).
{[2,4,6,8,10],[1,4,9,16,25]}
```

# `max`

```erlang
-spec max(List) -> Max when List :: [T, ...], Max :: T, T :: term().
```

Returns the first element of `List` that compares greater than or equal to all
other elements of `List`.

## Examples

```erlang
1> lists:max([17,19,7,55]).
55
2> lists:max([]).
** exception error: no function clause matching lists:max([])
```

# `member`

```erlang
-spec member(Elem, List) -> boolean() when Elem :: T, List :: [T], T :: term().
```

Returns `true` if `Elem` matches some element of `List`; otherwise, returns `false`.

## Examples

```erlang
1> lists:member(2, [1,2,3]).
true
2> lists:member(nope, [1,2,3]).
false
```

# `merge3`

```erlang
-spec merge3(List1, List2, List3) -> List4
                when
                    List1 :: [X],
                    List2 :: [Y],
                    List3 :: [Z],
                    List4 :: [X | Y | Z],
                    X :: term(),
                    Y :: term(),
                    Z :: term().
```

Returns the sorted list formed by merging `List1`, `List2`, and `List3`.

All of `List1`, `List2`, and `List3` must be sorted before evaluating
this function.

When two elements compare equal, the element from `List1`, if there is such an
element, is picked before the other element, otherwise the element from `List2`
is picked before the element from `List3`.

## Examples

```erlang
1> lists:merge3([a,o], [g,q], [j]).
[a,g,j,o,q]
```

# `merge`

```erlang
-spec merge(ListOfLists) -> List1 when ListOfLists :: [List], List :: [T], List1 :: [T], T :: term().
```

Returns the sorted list formed by merging all sublists of `ListOfLists`.

All sublists must be sorted before evaluating this function.

When two elements compare equal, the element from the sublist with the lowest
position in `ListOfLists` is picked before the other element.

## Examples

```erlang
1> lists:merge([[b,l,l], [g,k,q]]).
[b,g,k,l,l,q]
```

# `merge`

```erlang
-spec merge(List1, List2) -> List3
               when List1 :: [X], List2 :: [Y], List3 :: [X | Y], X :: term(), Y :: term().
```

Returns the sorted list formed by merging `List1` and `List2`.

Both `List1` and `List2` must be sorted before evaluating this function.

When two elements compare equal, the element from `List1` is picked before the
element from `List2`.

## Examples

```erlang
1> lists:merge([a,o], [b,x]).
[a,b,o,x]
```

# `merge`

```erlang
-spec merge(Fun, List1, List2) -> List3
               when
                   Fun :: fun((A, B) -> boolean()),
                   List1 :: [A],
                   List2 :: [B],
                   List3 :: [A | B],
                   A :: term(),
                   B :: term().
```

Returns a sorted list formed by merging `List1` and `List2` based on `Fun`.

Both `List1` and `List2` must be sorted according to the
[ordering function](`m:lists#ordering_function`) `Fun` before evaluating this
function.

`Fun(A, B)` is to return `true` if `A` compares less than or equal to
`B` in the ordering, otherwise `false`. When two elements compare equal, the
element from `List1` is picked before the element from `List2`.

## Examples

```erlang
1> F = fun(A, B) -> tuple_size(A) =< tuple_size(B) end.
2> lists:merge(F, [{x, y}, {a, b, c}], [{q, w}]).
[{x,y},{q,w},{a,b,c}]
```

# `min`

```erlang
-spec min(List) -> Min when List :: [T, ...], Min :: T, T :: term().
```

Returns the first element of `List` that compares less than or equal to all
other elements of `List`.

## Examples

```erlang
1> lists:min([17,19,7,55]).
7
2> lists:min([]).
** exception error: no function clause matching lists:min([])
```

# `nth`

```erlang
-spec nth(N, List) -> Elem when N :: pos_integer(), List :: [T, ...], Elem :: T, T :: term().
```

Returns the `N`th element of `List`.

## Examples

```erlang
1> lists:nth(3, [a, b, c, d, e]).
c
```

# `nthtail`

```erlang
-spec nthtail(N, List) -> Tail when N :: non_neg_integer(), List :: [T], Tail :: [T], T :: term().
```

Returns the `N`th tail of `List`, meaning the sublist of `List`
starting at `N+1` and continuing to the end of the list.

## Examples

```erlang
1> lists:nthtail(3, [a, b, c, d, e]).
[d,e]
2> tl(tl(tl([a, b, c, d, e]))).
[d,e]
3> lists:nthtail(0, [a, b, c, d, e]).
[a,b,c,d,e]
4> lists:nthtail(5, [a, b, c, d, e]).
[]
```

# `partition`

```erlang
-spec partition(Pred, List) -> {Satisfying, NotSatisfying}
                   when
                       Pred :: fun((Elem :: T) -> boolean()),
                       List :: [T],
                       Satisfying :: [T],
                       NotSatisfying :: [T],
                       T :: term().
```

Partitions `List` into two lists: the first containing elements for
which `Pred(Elem)` returns `true`, and the second containing elements
for which `Pred(Elem)` returns `false`.

## Examples

```erlang
1> lists:partition(fun(A) -> A rem 2 =:= 1 end, [1,2,3,4,5,6,7]).
{[1,3,5,7],[2,4,6]}
2> lists:partition(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]).
{[a,b,c,d,e],[1,2,3,4]}
```

For a different way to partition a list, see `splitwith/2`.

# `prefix`

```erlang
-spec prefix(List1, List2) -> boolean() when List1 :: [T], List2 :: [T], T :: term().
```

Returns `true` if `List1` is a prefix of `List2`; otherwise, returns `false`.

A prefix of a list is the first part of the list, starting from the
beginning and stopping at any point.

## Examples

```erlang
1> lists:prefix("abc", "abcdef").
true
2> lists:prefix("def", "abcdef").
false
3> lists:prefix([], "any list").
true
4> lists:prefix("abc", "abc").
true
```

# `reverse`

```erlang
-spec reverse(List1) -> List2 when List1 :: [T], List2 :: [T], T :: term().
```

Returns a list containing the elements in `List1` in reverse order.

## Examples

```erlang
1> lists:reverse([1,2,3]).
[3,2,1]
```

# `reverse`

```erlang
-spec reverse(List1, Tail) -> List2 when List1 :: [T], Tail :: term(), List2 :: [T], T :: term().
```

Returns a list containing the elements of `List1` in reverse order,
with tail `Tail` appended.

## Examples

```erlang
1> lists:reverse([1, 2, 3, 4], [a, b, c]).
[4,3,2,1,a,b,c]
```

# `search`
*since OTP 21.0* 

```erlang
-spec search(Pred, List) -> {value, Value} | false
                when Pred :: fun((T) -> boolean()), List :: [T], Value :: T.
```

If there is a `Value` in `List` such that `Pred(Value)` returns `true`, returns
`{value, Value}` for the first such `Value`; otherwise, returns `false`.

## Examples

```erlang
1> lists:search(fun is_atom/1, [1,2,3,a,b,c]).
{value,a}
2> lists:search(fun(#{a := V}) -> V =:= 42 end,
     [#{a => 1}, #{a => 42}, #{a => 100}]).
{value,#{a => 42}}
```

# `seq`

```erlang
-spec seq(From, To) -> Seq when From :: integer(), To :: integer(), Seq :: [integer()].
```

# `seq`

```erlang
-spec seq(From, To, Incr) -> Seq
             when From :: integer(), To :: integer(), Incr :: integer(), Seq :: [integer()].
```

Returns a sequence of integers that starts with `From` and contains the
successive results of adding `Incr` to the previous element, until `To` is
reached or passed (in the latter case, `To` is not an element of the sequence).

`Incr` defaults to 1.

Failures:

- If `To < From - Incr` and `Incr > 0`.
- If `To > From - Incr` and `Incr < 0`.
- If `Incr =:= 0` and `From =/= To`.

The following equalities hold for all sequences:

```erlang
length(lists:seq(From, To)) =:= To - From + 1
length(lists:seq(From, To, Incr)) =:= (To - From + Incr) div Incr
```

## Examples

```erlang
1> lists:seq(1, 10).
[1,2,3,4,5,6,7,8,9,10]
2> lists:seq(1, 20, 3).
[1,4,7,10,13,16,19]
3> lists:seq(1, 0, 1).
[]
4> lists:seq(10, 6, 4).
[]
5> lists:seq(1, 1, 0).
[1]
```

# `sort`

```erlang
-spec sort(List1) -> List2 when List1 :: [T], List2 :: [T], T :: term().
```

Returns a list containing the sorted elements of `List1`.

The sort is stable.

## Examples

```erlang
1> lists:sort([4,1,3,2]).
[1,2,3,4]
2> lists:sort([a,4,3,b,9]).
[3,4,9,a,b]
```
Since the sort is stable, the relative order of elements that compare
equal is not changed:

```erlang
1> lists:sort([1.0,1]).
[1.0,1]
2> lists:sort([1,1.0]).
[1,1.0]
```

# `sort`

```erlang
-spec sort(Fun, List1) -> List2
              when Fun :: fun((A :: T, B :: T) -> boolean()), List1 :: [T], List2 :: [T], T :: term().
```

Returns a list of the elements in `List1`, sorted according to the
[ordering function](`m:lists#ordering_function`) `Fun`, where `Fun(A,
B)` returns `true` if `A` compares less than or equal to `B` in the
ordering; otherwise, it returns `false`.

## Examples

```erlang
1> F = fun(A, B) -> tuple_size(A) =< tuple_size(B) end.
2> lists:sort(F, [{a, b, c}, {x, y}, {q, w}]).
[{x,y},{q,w},{a,b,c}]
```

# `split`

```erlang
-spec split(N, List1) -> {List2, List3}
               when N :: non_neg_integer(), List1 :: [T], List2 :: [T], List3 :: [T], T :: term().
```

Splits `List1` into `List2`, containing the first `N` elements, and
`List3`, containing the rest.

## Examples

```erlang
1> lists:split(3, [1,2,3,4,5,6,7]).
{[1,2,3],[4,5,6,7]}
```

# `splitwith`

```erlang
-spec splitwith(Pred, List) -> {List1, List2}
                   when
                       Pred :: fun((T) -> boolean()),
                       List :: [T],
                       List1 :: [T],
                       List2 :: [T],
                       T :: term().
```

Partitions `List` into two lists according to `Pred`.

[`splitwith/2`](`splitwith/2`) behaves as if it were defined as follows:

```erlang
splitwith(Pred, List) ->
    {takewhile(Pred, List), dropwhile(Pred, List)}.
```

## Examples

```erlang
1> lists:splitwith(fun(A) -> A rem 2 =:= 1 end, [1,2,3,4,5,6,7]).
{[1],[2,3,4,5,6,7]}
2> lists:splitwith(fun(A) -> is_atom(A) end, [a,b,1,c,d,2,3,4,e]).
{[a,b],[1,c,d,2,3,4,e]}
```

For a different way to partition a list, see `partition/2`.

# `sublist`

```erlang
-spec sublist(List1, Len) -> List2
                 when List1 :: [T], List2 :: [T], Len :: non_neg_integer(), T :: term().
```

Returns the sublist of `List1` starting at position 1 and with no more than `Len`
elements.

It is not an error for `Len` to exceed the length of the list, in which
case the whole list is returned.

## Examples

```erlang
1> lists:sublist([1,2,3,4,5], 2).
[1,2]
2> lists:sublist([1,2,3,4,5], 99).
[1,2,3,4,5]
```

# `sublist`

```erlang
-spec sublist(List1, Start, Len) -> List2
                 when
                     List1 :: [T],
                     List2 :: [T],
                     Start :: pos_integer(),
                     Len :: non_neg_integer(),
                     T :: term().
```

Returns the sublist of `List1` starting at `Start` and with no more than `Len`
elements.

It is not an error for `Start+Len` to exceed the length of the list.

## Examples

```erlang
1> lists:sublist([1,2,3,4], 2, 2).
[2,3]
2> lists:sublist([1,2,3,4], 2, 5).
[2,3,4]
3> lists:sublist([1,2,3,4], 5, 2).
[]
```

# `subtract`

```erlang
-spec subtract(List1, List2) -> List3 when List1 :: [T], List2 :: [T], List3 :: [T], T :: term().
```

Returns a new list, `List3`, which is a copy of `List1` with the
following modification: for each element in `List2`, its first
occurrence in `List1` is removed.

## Examples

```erlang
1> lists:subtract("123212", "212").
"312"
```

`lists:subtract(A, B)` is equivalent to `A -- B`.

# `suffix`

```erlang
-spec suffix(List1, List2) -> boolean() when List1 :: [T], List2 :: [T], T :: term().
```

Returns `true` if `List1` is a suffix of `List2`; otherwise, returns `false`.

A suffix of a list is the last part of the list, starting from any position
and going all the way to the end.

## Examples

```erlang
1> lists:suffix("abc", "abcdef").
false
2> lists:suffix("def", "abcdef").
true
3> lists:suffix([], "any list").
true
4> lists:suffix("abc", "abc").
true
```

# `sum`

```erlang
-spec sum(List) -> number() when List :: [number()].
```

Returns the sum of the elements in `List`.

## Examples

```erlang
1> lists:sum([]).
0
2> lists:sum([1,2,3]).
6
```

# `takewhile`

```erlang
-spec takewhile(Pred, List1) -> List2
                   when Pred :: fun((Elem :: T) -> boolean()), List1 :: [T], List2 :: [T], T :: term().
```

Takes elements `Elem` from `List1` while `Pred(Elem)` returns `true`,
returning the longest prefix in which all elements satisfy the predicate.

## Examples

```erlang
1> lists:takewhile(fun is_atom/1, [a,b,c,1,2,3,x,y,z]).
[a,b,c]
2> lists:takewhile(fun is_integer/1, [a,b,c,1,2,3,x,y,z]).
[]
```

# `ukeymerge`

```erlang
-spec ukeymerge(N, TupleList1, TupleList2) -> TupleList3
                   when
                       N :: pos_integer(),
                       TupleList1 :: [T1],
                       TupleList2 :: [T2],
                       TupleList3 :: [T1 | T2],
                       T1 :: Tuple,
                       T2 :: Tuple,
                       Tuple :: tuple().
```

Returns the sorted list formed by merging `TupleList1` and `TupleList2`
based on the `N`th element of each tuple.

Both `TupleList1` and `TupleList2` must be key-sorted without
duplicates before evaluating this function.

When the `N`th elements of two tuples compare equal, the tuple
from `TupleList1` is picked and the one from `TupleList2` is removed.

## Examples

```erlang
1> lists:ukeymerge(1, [{a, 33}, {c, 15}], [{a, 59}, {d, 39}]).
[{a,33},{c,15},{d,39}]
```

# `ukeysort`

```erlang
-spec ukeysort(N, TupleList1) -> TupleList2
                  when
                      N :: pos_integer(), TupleList1 :: [Tuple], TupleList2 :: [Tuple], Tuple :: tuple().
```

Returns a sorted list of the elements in `TupleList1`, keeping only the
first occurrence of tuples whose `N`th elements compare equal.

Sorting is performed on the `N`th element of the tuples.

## Examples

```erlang
1> lists:ukeysort(2, [{a, 27}, {d, 23}, {e, 23}]).
[{d,23}, {a, 27}]
```

# `umerge3`

```erlang
-spec umerge3(List1, List2, List3) -> List4
                 when
                     List1 :: [X],
                     List2 :: [Y],
                     List3 :: [Z],
                     List4 :: [X | Y | Z],
                     X :: term(),
                     Y :: term(),
                     Z :: term().
```

Returns the sorted list formed by merging `List1`, `List2`, and `List3`,
while removing duplicates.

All of `List1`, `List2`, and `List3` must be sorted and contain no
duplicates before evaluating this function.

When two elements compare equal, the element from
`List1` is picked if there is such an element, otherwise the element from
`List2` is picked, and the other is removed.

## Examples

```erlang
1> lists:umerge3([a,b], [a,d,e], [b,f]).
[a,b,d,e,f]
```

# `umerge`

```erlang
-spec umerge(ListOfLists) -> List1 when ListOfLists :: [List], List :: [T], List1 :: [T], T :: term().
```

Returns a sorted list formed by merging all sublists in `ListOfLists`,
while removing duplicates.

All sublists must be sorted and contain no duplicates before
evaluating this function.

When two elements compare equal, the element from the sublist with the
lowest position in `ListOfLists` is picked and the other is removed.

## Examples

```erlang
1> lists:umerge([[a,b], [a,d,e]]).
[a,b,d,e]
```

# `umerge`

```erlang
-spec umerge(List1, List2) -> List3
                when List1 :: [X], List2 :: [Y], List3 :: [X | Y], X :: term(), Y :: term().
```

Returns the sorted list formed by merging `List1` and `List2`,
while removing duplicates.

Both `List1` and `List2` must be sorted and contain no duplicates
before evaluating this function.

When two elements compare equal, the element from `List1` is picked
and the one from `List2` is removed.

## Examples

```erlang
1> lists:umerge([a,b], [a,d,e]).
[a,b,d,e]
```

# `umerge`

```erlang
-spec umerge(Fun, List1, List2) -> List3
                when
                    Fun :: fun((A, B) -> boolean()),
                    List1 :: [A],
                    List2 :: [B],
                    List3 :: [A | B],
                    A :: term(),
                    B :: term().
```

Returns a sorted list by merging `List1` and `List2` using [ordering
function](`m:lists#ordering_function`) `Fun`, assuming both lists are
pre-sorted according to `Fun` and contain no duplicates.

`Fun(A, B)` is to return `true` if `A` compares less than or equal to
`B` in the ordering; otherwise, it should return `false`. When two
elements compare equal, the element from `List1` is picked and the one
from `List2` is removed.

## Examples

```erlang
1> F = fun(A, B) -> tuple_size(A) =< tuple_size(B) end.
2> lists:umerge(F, [{x, y}, {a, b, c}], [{q, w}, {x, y, z, w}]).
[{x,y},{a,b,c},{x,y,z,w}]
```

# `uniq`
*since OTP 25.0* 

```erlang
-spec uniq(List1) -> List2 when List1 :: [T], List2 :: [T], T :: term().
```

Returns a list containing the elements of `List1` with duplicated elements
removed (preserving the order of the elements).

The first occurrence of each element is kept.

## Examples

```erlang
1> lists:uniq([3, 3, 1, 2, 1, 2, 3]).
[3,1,2]
2> lists:uniq([a, a, 1, b, 2, a, 3]).
[a, 1, b, 2, 3]
```

# `uniq`
*since OTP 25.0* 

```erlang
-spec uniq(Fun, List1) -> List2 when Fun :: fun((T) -> any()), List1 :: [T], List2 :: [T], T :: term().
```

Returns a list containing the elements of `List1` without the elements for which
`Fun` returned duplicate values (preserving the order of the elements).

The first occurrence of each element is kept.

## Examples

```erlang
1> lists:uniq(fun({X, _}) -> X end, [{b, 2}, {a, 1}, {c, 3}, {a, 2}]).
[{b, 2}, {a, 1}, {c, 3}]
```

# `unzip3`

```erlang
-spec unzip3(List1) -> {List2, List3, List4}
                when
                    List1 :: [{A, B, C}],
                    List2 :: [A],
                    List3 :: [B],
                    List4 :: [C],
                    A :: term(),
                    B :: term(),
                    C :: term().
```

"Unzips" a list of three-tuples into three lists, where the first list contains
the first element of each tuple, the second list contains the second element of
each tuple, and the third list contains the third element of each tuple.

## Examples

```erlang
1> lists:unzip3([{a, 1, 2}, {b, 777, 999}]).
{[a,b],[1,777],[2,999]}
```

# `unzip`

```erlang
-spec unzip(List1) -> {List2, List3}
               when List1 :: [{A, B}], List2 :: [A], List3 :: [B], A :: term(), B :: term().
```

"Unzips" a list of two-tuples into two lists, where the first list contains the
first element of each tuple, and the second list contains the second element of
each tuple.

## Examples

```erlang
1> lists:unzip([{1, a}, {2, b}]).
{[1,2],[a,b]}
```

# `usort`

```erlang
-spec usort(List1) -> List2 when List1 :: [T], List2 :: [T], T :: term().
```

Returns a sorted list of the elements of `List1`, keeping only the
first occurrence of elements that compare equal.

## Examples

```erlang
1> lists:usort([a,x,y,b,c,x,a]).
[a,b,c,x,y]
2> lists:usort([3,2,a,3,2,a,1,3,b,2,2,1]).
[1,2,3,a,b]
3> lists:usort([1.0,1]).
[1.0]
4> lists:usort([1,1.0]).
[1]
```

# `usort`

```erlang
-spec usort(Fun, List1) -> List2
               when Fun :: fun((T, T) -> boolean()), List1 :: [T], List2 :: [T], T :: term().
```

Returns a list containing the sorted elements of `List1` where all except the
first element of the elements comparing equal according to the
[ordering function](`m:lists#ordering_function`) `Fun` have been removed.

`Fun(A, B)` is to return `true` if `A` compares less than or equal to `B` in the
ordering, otherwise `false`.

## Examples

```erlang
1> F = fun(A, B) -> tuple_size(A) =< tuple_size(B) end.
2> lists:usort(F, [{a, b, c}, {x, y}, {q, w}]).
[{x,y},{a,b,c}]
```

# `zip3`

```erlang
-spec zip3(List1, List2, List3) -> List4
              when
                  List1 :: [A],
                  List2 :: [B],
                  List3 :: [C],
                  List4 :: [{A, B, C}],
                  A :: term(),
                  B :: term(),
                  C :: term().
```

# `zip3`
*since OTP 26.0* 

```erlang
-spec zip3(List1, List2, List3, How) -> List4
              when
                  List1 :: [A],
                  List2 :: [B],
                  List3 :: [C],
                  List4 :: [{A | DefaultA, B | DefaultB, C | DefaultC}],
                  A :: term(),
                  B :: term(),
                  C :: term(),
                  How :: fail | trim | {pad, {DefaultA, DefaultB, DefaultC}},
                  DefaultA :: term(),
                  DefaultB :: term(),
                  DefaultC :: term().
```

"Zips" three lists into one list of three-tuples, where the first element of
each tuple is taken from the first list, the second element is taken from the
corresponding element in the second list, and the third element is taken from
the corresponding element in the third list.

For a description of the `How` parameter, see `zip/3`.

## Examples

```erlang
1> lists:zip3([a], [1, 2, 3], [17, 19], trim).
[{a,1,17}]
2> lists:zip3([a], [1, 2, 3], [17, 19], {pad, {z, 0, 0}}).
[{a,1,17}, {z,2,19}, {z,3,0}]
```

# `zip`

```erlang
-spec zip(List1, List2) -> List3
             when List1 :: [A], List2 :: [B], List3 :: [{A, B}], A :: term(), B :: term().
```

# `zip`
*since OTP 26.0* 

```erlang
-spec zip(List1, List2, How) -> List3
             when
                 List1 :: [A],
                 List2 :: [B],
                 List3 :: [{A | DefaultA, B | DefaultB}],
                 A :: term(),
                 B :: term(),
                 How :: fail | trim | {pad, {DefaultA, DefaultB}},
                 DefaultA :: term(),
                 DefaultB :: term().
```

"Zips" two lists into one list of two-tuples, where the first element of each
tuple is taken from the first list and the second element is taken from the
corresponding element in the second list.

The `How` parameter specifies the behavior if the given lists are of different
lengths.

- **`fail`** - The call will fail if the given lists are not of equal length.
  This is the default.

- **`trim`** - Surplus elements from the longer list will be ignored.

  ## Examples

  ```erlang
  1> lists:zip([a, b], [1, 2, 3], trim).
  [{a,1},{b,2}]
  2> lists:zip([a, b, c], [1, 2], trim).
  [{a,1},{b,2}]
  ```

- **`{pad, Defaults}`** - The shorter list will be padded to the length of the
  longer list, using the respective elements from the given `Defaults` tuple.

  ## Examples

  ```erlang
  1> lists:zip([a, b], [1, 2, 3], {pad, {x, 0}}).
  [{a,1},{b,2},{x,3}]
  2> lists:zip([a, b, c], [1, 2], {pad, {x, 0}}).
  [{a,1},{b,2},{c,0}]
  ```

# `zipwith3`

```erlang
-spec zipwith3(Combine, List1, List2, List3) -> List4
                  when
                      Combine :: fun((X, Y, Z) -> T),
                      List1 :: [X],
                      List2 :: [Y],
                      List3 :: [Z],
                      List4 :: [T],
                      X :: term(),
                      Y :: term(),
                      Z :: term(),
                      T :: term().
```

# `zipwith3`
*since OTP 26.0* 

```erlang
-spec zipwith3(Combine, List1, List2, List3, How) -> List4
                  when
                      Combine :: fun((X | DefaultX, Y | DefaultY, Z | DefaultZ) -> T),
                      List1 :: [X],
                      List2 :: [Y],
                      List3 :: [Z],
                      List4 :: [T],
                      X :: term(),
                      Y :: term(),
                      Z :: term(),
                      How :: fail | trim | {pad, {DefaultX, DefaultY, DefaultZ}},
                      DefaultX :: term(),
                      DefaultY :: term(),
                      DefaultZ :: term(),
                      T :: term().
```

Combines the elements of three lists into a single list using the
`Combine` fun.

For each triple `X, Y, Z` of list elements from the three lists, the
element in the result list is `Combine(X, Y, Z)`.

For a description of the `How` parameter, see `zip/3`.

[`zipwith3(fun(X, Y, Z) -> {X,Y,Z} end, List1, List2, List3)`](`zipwith3/4`) is
equivalent to [`zip3(List1, List2, List3)`](`zip3/3`).

## Examples

```erlang
1> lists:zipwith3(fun(X, Y, Z) -> X+Y+Z end, [1,2,3], [4,5,6], [7,8,9], fail).
[12,15,18]
2> lists:zipwith3(fun(X, Y, Z) -> [X,Y,Z] end, [a,b,c], [x,y,z], [1,2,3], fail).
[[a,x,1],[b,y,2],[c,z,3]]
```

# `zipwith`

```erlang
-spec zipwith(Combine, List1, List2) -> List3
                 when
                     Combine :: fun((X, Y) -> T),
                     List1 :: [X],
                     List2 :: [Y],
                     List3 :: [T],
                     X :: term(),
                     Y :: term(),
                     T :: term().
```

# `zipwith`
*since OTP 26.0* 

```erlang
-spec zipwith(Combine, List1, List2, How) -> List3
                 when
                     Combine :: fun((X | DefaultX, Y | DefaultY) -> T),
                     List1 :: [X],
                     List2 :: [Y],
                     List3 :: [T],
                     X :: term(),
                     Y :: term(),
                     How :: fail | trim | {pad, {DefaultX, DefaultY}},
                     DefaultX :: term(),
                     DefaultY :: term(),
                     T :: term().
```

Combines the elements of two lists into a single list using the `Combine` fun.

For each pair `X, Y` of list elements from the two lists, the element
in the result list is `Combine(X, Y)`.

For a description of the `How` parameter, see `zip/3`.

[`zipwith(fun(X, Y) -> {X,Y} end, List1, List2)`](`zipwith/3`) is equivalent to
[`zip(List1, List2)`](`zip/2`).

## Examples

```erlang
1> lists:zipwith(fun(X, Y) -> X+Y end, [1,2,3], [4,5,6], fail).
[5,7,9]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
