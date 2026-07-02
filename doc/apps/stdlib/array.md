# `array`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/array.erl#L23)

Functional, extendible arrays.

Arrays can have fixed size, or can grow automatically as needed. A default value
is used for entries that have not been explicitly set.

Arrays uses _zero_-based indexing. This is a deliberate design choice and
differs from other Erlang data structures, for example, tuples.

Unless specified by the user when the array is created, the default value is the
atom `undefined`. There is no difference between an unset entry and an entry
that has been explicitly set to the same value as the default one (compare
`reset/2`). If you need to differentiate between unset and set entries, ensure
that the default value cannot be confused with the values of set entries.

The array never shrinks automatically. If an index `I` has been used to set an
entry successfully, all indices in the range `[0,I]` stay accessible unless the
array size is explicitly changed by calling `resize/2`.

## Examples

Create a fixed-size array with entries 0-9 set to `undefined`:

```
A0 = array:new(10).
10 = array:size(A0).
```

Create an extendible array and set entry 17 to `true`, causing the array to grow
automatically:

```
A1 = array:set(17, true, array:new()).
18 = array:size(A1).
```

Read back a stored value:

```
true = array:get(17, A1).
```

Accessing an unset entry returns the default value:

```
undefined = array:get(3, A1)
```

Accessing an entry beyond the last set entry also returns the default value, if
the array does not have fixed size:

```
undefined = array:get(18, A1).
```

"Sparse" functions ignore default-valued entries:

```
A2 = array:set(4, false, A1).
[{4, false}, {17, true}] = array:sparse_to_orddict(A2).
```

An extendible array can be made fixed-size later:

```
A3 = array:fix(A2).
```

A fixed-size array does not grow automatically and does not allow accesses
beyond the last set entry:

```
{'EXIT',{badarg,_}} = (catch array:set(18, true, A3)).
{'EXIT',{badarg,_}} = (catch array:get(18, A3)).
```

# `array`

```erlang
-type array() :: array(dynamic()).
```

# `array`

```erlang
-opaque array(Type)
```

A functional, extendible array.

The representation is not documented and is subject to change without
notice. Notice that arrays cannot be directly compared for equality.

# `array_indx`
*not exported* 

```erlang
-type array_indx() :: non_neg_integer().
```

# `array_opt`
*not exported* 

```erlang
-type array_opt() ::
          {fixed, boolean()} |
          fixed |
          {default, Type :: dynamic()} |
          {size, N :: non_neg_integer()} |
          (N :: non_neg_integer()).
```

# `array_opts`
*not exported* 

```erlang
-type array_opts() :: array_opt() | [array_opt()].
```

# `indx_pair`
*not exported* 

```erlang
-type indx_pair(Type) :: {Index :: array_indx(), Type}.
```

# `indx_pairs`
*not exported* 

```erlang
-type indx_pairs(Type) :: [indx_pair(Type)].
```

# `append`
*since OTP 29.0* 

```erlang
-spec append(Value :: any(), Array :: array(Type)) -> array(Type).
```

Append a single value to the right side of the array.

The operation is always allowed even if the array is fixed.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(array:size(A), array:append(last, A)).
last
```

See also `prepend/2`, `concat/2`.

# `concat`
*since OTP 29.0* 

```erlang
-spec concat(Arrays :: [array(Type)]) -> array(Type).
```

Concatenates a nonempty list of arrays.

## Examples

```erlang
1> A = array:from_list([a]).
2> B = array:from_list([b]).
3> array:to_list(array:concat([A,B])).
[a,b]
```

See also `concat/2`.

# `concat`
*since OTP 29.0* 

```erlang
-spec concat(A :: array(Type), B :: array(Type)) -> AB :: array(Type).
```

Concatenates two arrays.

Adds the elements of `B` onto `A`.

## Examples

```erlang
1> A = array:set(1, a, array:new([{default, xa}, {size,3}, {fixed, true}])).
2> B = array:set(2, b, array:new([{default, xb}, {size,4}, {fixed, false}])).
3> AB = array:concat(A,B).
4> array:to_list(AB).
[xa,a,xa,xb,xb,b,xb]
```

See also `concat/1`, `append/2`, `prepend/2`.

# `default`

```erlang
-spec default(Array :: array(Type)) -> Value :: Type.
```

Gets the value used for uninitialized entries.

## Examples

```erlang
1> array:default(array:new()).
undefined
2> array:get(52, array:new()).
undefined
3> array:default(array:new([{default, 0}])).
0
4> array:get(52, array:new([{default, 0}])).
0
```

See also `new/2`.

# `fix`

```erlang
-spec fix(Array :: array(Type)) -> array(Type).
```

Fixes the array size to prevent it from growing automatically upon
insertion.

Note that operations which explicitly increase the array size, such as
`append/2`, may still be used on a fixed size array.

## Examples

```erlang
1> array:get(1, array:from_list([a,b,c])).
b
2> array:get(10, array:from_list([a,b,c])).
undefined
3> array:get(10, array:fix(array:from_list([a,b,c]))).
** exception error: bad argument
     in function  array:get/2
```

See also `relax/1`, `set/3`.

# `foldl`

```erlang
-spec foldl(Function, InitialAcc :: A, Array :: array(Type)) -> A
               when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements using the specified function and initial accumulator
value.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> array:foldl(fun(_K, V, Acc) -> V+Acc end, 0, A).
6
```

See also `foldl/5`, `foldr/3`, `sparse_foldl/3`.

# `foldl`
*since OTP 29.0* 

```erlang
-spec foldl(Low, High, Function, InitialAcc :: A, Array) -> A
               when
                   Low :: array_indx(),
                   High :: array_indx(),
                   Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A),
                   Array :: array(Type).
```

Folds the array elements from `Low` to `High` using the specified function and
initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,100)).
2> array:foldl(50, 59, fun(_K, V, Acc) -> V+Acc end, 0, A).
545
```

See also `foldl/3`, `sparse_foldl/5`.

# `foldr`

```erlang
-spec foldr(Function, InitialAcc :: A, Array :: array(Type)) -> A
               when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements right-to-left using the specified function and initial
accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/5`, `foldl/3`, `sparse_foldr/3`.

# `foldr`
*since OTP 29.0* 

```erlang
-spec foldr(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> A
               when
                   Low :: array_indx(),
                   High :: array_indx(),
                   Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements from `High` to `Low` using the specified function and
initial accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `foldr/3`, `foldl/5`.

# `from`
*since OTP 29.0* 

```erlang
-spec from(Function, State :: term()) -> array(Type)
              when Function :: fun((State0 :: term()) -> {Type, State1 :: term()} | done).
```

Equivalent to [`from(Fun, State, undefined)`](`from/3`).

# `from`
*since OTP 29.0* 

```erlang
-spec from(Function, State :: term(), Default :: term()) -> array(Type)
              when Function :: fun((State0 :: term()) -> {Type, State1 :: term()} | done).
```

Creates an extendible array with values obtained with `Function(State)`.

The 'Function(State)' shall return `{Value, NewState}` or `done`, and is invoked
until `done` is returned, otherwise the call fails with reason `badarg`.

`Default` is used as the value for uninitialized entries of the array.

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

## Examples

```erlang
1> Floats = << <<N:32/float-native>> || N <- lists:seq(0, 2047)>>.
2> BinToVal = fun(I) ->
     case Floats of
         <<_:I/binary, N:32/float-native, _/binary>> ->
             {N, I+4};
         _ ->
             done
     end
   end.
3> A = array:from(BinToVal, 0).
4> array:get(10, A).
10.0
5> array:size(A).
2048
6> ValToBin = fun(_K, V, Acc) -> <<Acc/binary, V:32/float-native>> end.
7> Floats == array:foldl(ValToBin, <<>>, A).
true
```

See also `new/2`, `from_list/1`, `foldl/3`.

# `from_list`

```erlang
-spec from_list(List :: [Value :: Type]) -> array(Type).
```

Equivalent to [`from_list(List, undefined)`](`from_list/2`).

# `from_list`

```erlang
-spec from_list(List :: [Value :: Type], Default :: term()) -> array(Type).
```

Converts a list to an extendible array.

`Default` is used as the value for uninitialized entries of the array.

If `List` is not a proper list, the call fails with reason `badarg`.

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_list(array:reset(1, A)).
[0,default,2]
```

See also `new/2`, `to_list/1`.

# `from_orddict`

```erlang
-spec from_orddict(Orddict :: indx_pairs(Value :: Type)) -> array(Type).
```

Equivalent to [`from_orddict(Orddict, undefined)`](`from_orddict/2`).

# `from_orddict`

```erlang
-spec from_orddict(Orddict :: indx_pairs(Value :: Type), Default :: dynamic()) -> array(Type).
```

Converts an ordered list of pairs `{Index, Value}` to a corresponding extendible
array.

`Default` is used as the value for uninitialized entries of the array.

If `Orddict` is not a proper, ordered list of pairs whose first elements are
non-negative integers, the call fails with reason `badarg`.

Note: Use `fix/1` on the resulting array if you want to prevent accesses
outside the size range.

## Examples

```erlang
1> A = array:from_orddict([{K,V} || K <:- lists:seq(2,4) && V <- [v1,v2,v3]], vx).
2> array:to_orddict(A).
[{0,vx},{1,vx},{2,v1},{3,v2},{4,v3}]
```

See also `new/2`, `to_orddict/1`.

# `get`

```erlang
-spec get(I :: array_indx(), Array :: array(Type)) -> Value :: Type.
```

Gets the value of entry `I`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, the default value for any index `I`
greater than `size(Array)-1` is returned.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(4,A).
4
3> array:get(10, A).
undefined
```

See also `set/3`.

# `is_array`

```erlang
-spec is_array(X :: term()) -> boolean().
```

Returns `true` if `X` is an array, otherwise `false`.

Notice that the check is only shallow, as there is no guarantee that `X` is a
well-formed array representation even if this function returns `true`.

## Examples

```erlang
1> array:is_array(array:new(4, [])).
true
```

# `is_fix`

```erlang
-spec is_fix(Array :: array()) -> boolean().
```

Checks if the array has fixed size.

Returns `true` if the array is fixed, otherwise `false`.

## Examples

```erlang
1> array:is_fix(array:new()).
false
2> array:is_fix(array:new({fixed, true})).
true
```

See also `fix/1`.

# `map`

```erlang
-spec map(Function, Array :: array(Type1)) -> array(Type1 | Type2)
             when Function :: fun((Index :: array_indx(), Type1) -> Type2).
```

Maps the specified function onto each array element.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> B = array:map(fun(K, V) -> K*V end, A).
3> array:to_orddict(B).
[{0,0},{1,1},{2,4},{3,9}]
```

See also `mapfoldl/3`, `sparse_map/2`.

# `mapfoldl`
*since OTP 29.0* 

```erlang
-spec mapfoldl(Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A}
                  when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).
```

Combined map and fold over the array elements using the specified
function and initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,3)).
2> {B, Acc} = array:mapfoldl(fun(K, V, Sum) -> {K*V, V+Sum} end, 0, A).
3> Acc.
6
4> array:to_orddict(B).
[{0,0}, {1,1}, {2,4}, {3,9}]
```

See also `mapfoldl/5`, `foldl/3`, `map/2`, `sparse_mapfoldl/3`.

# `mapfoldl`
*since OTP 29.0* 

```erlang
-spec mapfoldl(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A}
                  when
                      Low :: array_indx(),
                      High :: array_indx(),
                      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).
```

Combined map and fold over the array elements from `Low` to `High` using
the specified function and initial accumulator value.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldl/3`, `sparse_mapfoldl/5`.

# `mapfoldr`
*since OTP 29.0* 

```erlang
-spec mapfoldr(Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A}
                  when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).
```

Combined map and fold over the array elements using the specified
function and initial accumulator value.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldr/5`, `foldr/3`, `map/2`, `sparse_mapfoldr/3`.

# `mapfoldr`
*since OTP 29.0* 

```erlang
-spec mapfoldr(Low, High, Function, InitialAcc :: A, Array :: array(Type)) -> {array(Type), A}
                  when
                      Low :: array_indx(),
                      High :: array_indx(),
                      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> {Type, A}).
```

Combined map and fold over the array elements from `Low` to `High` using
the specified function and initial accumulator value.

The elements are visited in order from the highest index to the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `mapfoldr/3`, `mapfoldl/5`, `sparse_mapfoldr/5`.

# `new`

```erlang
-spec new() -> array().
```

Creates a new, extendible array with initial size zero.

# `new`

```erlang
-spec new(Options :: array_opts()) -> array().
```

Creates a new array according to the specified options.

By default, the array is extendible and has initial size zero. Array
indices start at `0`.

`Options` is a single term or a list of terms, selected from the following:

- **`N::integer() >= 0` or `{size, N::integer() >= 0}`** - Specifies the initial
  array size; this also implies `{fixed, true}`. If `N` is not a non-negative
  integer, the call fails with reason `badarg`.

- **`fixed` or `{fixed, true}`** - Creates a fixed-size array. See also `fix/1`.

- **`{fixed, false}`** - Creates an extendible (non-fixed-size) array.

- **`{default, Value}`** - Sets the default value for the array to `Value`.

Options are processed in the order they occur in the list, that is, later
options have higher precedence.

The default value is used as the value of uninitialized entries, and cannot be
changed once the array has been created.

## Examples

```erlang
1> array:new(100).
```

creates a fixed-size array of size 100.

```
1> array:new({default,0}).
```

creates an empty, extendible array whose default value is `0`.

```
1> array:new([{size,10},{fixed,false},{default,-1}]).
```

creates an extendible array with initial size 10 whose default value is `-1`.

See also `fix/1`, `from_list/2`, `get/2`, `new/0`, `new/2`, `set/3`.

# `new`

```erlang
-spec new(Size :: non_neg_integer(), Options :: array_opts()) -> array().
```

Creates a new array according to the specified size and options.

If `Size` is not a non-negative integer, the call fails with reason `badarg`.
By default, the array has fixed size. Notice that any size specifications in
`Options` override parameter `Size`.

If `Options` is a list, this is equivalent to
[`new([{size, Size} | Options])`](`new/1`), otherwise it is equivalent to
[`new([{size, Size} | [Options]])`](`new/1`). However, using this function
directly is more efficient.

## Examples

```erlang
1> array:new(100, {default,0}).
```

Creates a fixed-size array of size 100, whose default value is `0`.

# `prepend`
*since OTP 29.0* 

```erlang
-spec prepend(Value :: Type, Array :: array(Type)) -> array(Type).
```

Prepend a single value to the left side of the array.

The operation is always allowed even if the array is fixed.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(0, array:prepend(first, A)).
first
```

See also `append/2`, `concat/2`.

# `relax`

```erlang
-spec relax(Array :: array(Type)) -> array(Type).
```

Makes the array extendible, reversing the effects of `fix/1`.

## Examples

```erlang
1> array:get(10, array:new({fixed, true})).
** exception error: bad argument
     in function  array:get/2
2> array:get(10, array:relax(array:new())).
undefined
```

See also `fix/1`.

# `reset`

```erlang
-spec reset(I :: array_indx(), Array :: array(Type)) -> array(Type).
```

Resets entry `I` to the default value for the array.

If the value of entry `I` is the default value, the array is returned
unchanged.

Reset never changes the array size. Shrinking can be done explicitly by calling
`resize/2`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`; compare
`set/3`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:get(5, array:reset(5, A)).
undefined
```

See also `new/2`, `set/3`.

# `resize`

```erlang
-spec resize(Array :: array(Type)) -> array(Type).
```

Changes the array size to that reported by `sparse_size/1`.

If the specified array has fixed size, the resulting array also has
fixed size.

## Examples

```erlang
1> A = array:set(1, x, array:new(4, [])).
2> array:size(A).
4
3> array:size(array:resize(A)).
2
```

See also `resize/2`, `sparse_size/1`.

# `resize`

```erlang
-spec resize(Size :: non_neg_integer(), Array :: array(Type)) -> array(Type).
```

Change the array size.

If `Size` is not a non-negative integer, the call fails with reason `badarg`. If
the specified array has fixed size, also the resulting array has fixed size.

Note: As of OTP 29, resizing ensures that entries outside the new range are
pruned so that garbage collection can recover the memory.

## Examples

```erlang
1> array:get(10, array:new({fixed, true})).
** exception error: bad argument
     in function  array:get/2
2> array:get(10, array:resize(20, array:new({fixed, true}))).
undefined
```

See also `shift/2`.

# `set`

```erlang
-spec set(I :: array_indx(), Value :: Type, Array :: array(Type)) -> array(Type).
```

Sets entry `I` of the array to `Value`.

If `I` is not a non-negative integer, or if the array has fixed size and `I` is
larger than the maximum index, the call fails with reason `badarg`.

If the array does not have fixed size, and `I` is greater than `size(Array)-1`,
the array grows to size `I+1`.

## Examples

```erlang
1> A = array:new(4, [{fixed,true}]).
2> array:set(1, x, A).
3> array:set(5, x, A).
** exception error: bad argument
     in function  array:set/3
```

See also `get/2`, `reset/2`.

# `shift`
*since OTP 29.0* 

```erlang
-spec shift(Steps :: integer(), Array :: array(Type)) -> array(Type).
```

Shift the array a number of steps to the left, or to the right if the
number is negative.

Shifting left drops elements from the left side, reducing the array
size, and shifting right adds space on the left, increasing the array
size.

The fixed option does not affect the result of shift.

Note: For efficiency, this does not prune the representation, which means
that a subsequent shift or similar operation can bring back the values that
were shifted out. Use `resize/2` or `resize/1` if you want to ensure that
values outside the range get pruned.

## Examples

```erlang
1> A = array:new(10, [{fixed, true}]).
2> array:size(A).
10
3> array:size(array:shift(-5, A)).
15
4> array:size(array:shift(5, A)).
5
```

# `size`

```erlang
-spec size(Array :: array()) -> non_neg_integer().
```

Gets the number of entries in the array.

Entries are numbered from `0` to `size(Array)-1`. Hence, this is also
the index of the first entry that is guaranteed to not have been
previously set.

## Examples

```erlang
1> array:size(array:new(4, [])).
4
2> array:size(array:set(5, value, array:new())).
6
```

# `slice`
*since OTP 29.0* 

```erlang
-spec slice(I :: array_indx(), Length :: non_neg_integer(), Array :: array(Type)) -> array(Type).
```

Extract a slice of the array.

This drops elements before `I` as with `shift/2`, and takes the following
`Length` elements starting from `I`.

If `N` is less than or equal to zero, the resulting array is empty. To extract
a slice from `Start` to `End` inclusive, use `slice(Start, End-Start+1,
Array)`.

Note: For efficiency, this does not prune the representation, which means
that a subsequent shift or similar operation can bring back the values that
were shifted out. Use `resize/2` or `resize/1` if you want to ensure that
values outside the range get pruned.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,9)).
2> array:to_list(array:slice(2,3,A)).
[2,3,4]
```

# `sparse_foldl`

```erlang
-spec sparse_foldl(Function, InitialAcc :: A, Array :: array(Type)) -> A
                      when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements using the specified function and initial accumulator
value, skipping default-valued entries.

The elements are visited in order from the lowest index to the
highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldl/5`, `foldl/3`.

# `sparse_foldl`
*since OTP 29.0* 

```erlang
-spec sparse_foldl(Low :: array_indx(),
                   High :: array_indx(),
                   Function,
                   InitialAcc :: A,
                   Array :: array(Type)) ->
                      A
                      when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements from `Low` to `High` using the specified
function and initial accumulator value, skipping default-valued entries.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldl/3`, `foldl/5`.

# `sparse_foldr`

```erlang
-spec sparse_foldr(Function, InitialAcc :: A, Array :: array(Type)) -> A
                      when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements right-to-left using the specified function and initial
accumulator value, skipping default-valued entries.

The elements are visited in order from the highest index to the
lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldr/5`, `foldr/3`, `sparse_foldl/3`.

# `sparse_foldr`
*since OTP 29.0* 

```erlang
-spec sparse_foldr(Low :: array_indx(),
                   High :: array_indx(),
                   Function,
                   InitialAcc :: A,
                   Array :: array(Type)) ->
                      A
                      when Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> A).
```

Folds the array elements from `High` to `Low` using the specified
function and initial accumulator value, skipping default-valued entries.

The elements are visited in order from the highest index to the lowest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `sparse_foldr/3`, `foldr/5`.

# `sparse_map`

```erlang
-spec sparse_map(Function, Array :: array(Type1)) -> array(Type1 | Type2)
                    when Function :: fun((Index :: array_indx(), Type1) -> Type2).
```

Maps the specified function onto each array element, skipping default-valued
entries.

The elements are visited in order from the lowest index to the highest.

If `Function` is not a function, the call fails with reason `badarg`.

See also `map/2`.

# `sparse_mapfoldl`
*since OTP 29.0* 

```erlang
-spec sparse_mapfoldl(Function, InitialAcc :: A, Array) -> {ArrayRes, A}
                         when
                             Array :: array(Type1),
                             Function ::
                                 fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
                             ArrayRes :: array(Type1 | Type2).
```

Like `mapfoldl/3` but skips default-valued entries.

See also `sparse_mapfoldl/5`, `sparse_mapfoldr/3`.

# `sparse_mapfoldl`
*since OTP 29.0* 

```erlang
-spec sparse_mapfoldl(Low, High, Function, InitialAcc :: A, Array) -> {ArrayRes, A}
                         when
                             Low :: array_indx(),
                             High :: array_indx(),
                             Function ::
                                 fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
                             Array :: array(Type1),
                             ArrayRes :: array(Type1 | Type2).
```

Like `mapfoldl/5` but skips default-valued entries.

See also `sparse_mapfoldl/3`, `sparse_mapfoldr/5`.

# `sparse_mapfoldr`
*since OTP 29.0* 

```erlang
-spec sparse_mapfoldr(Function, InitialAcc :: A, Array) -> {ArrayRes, A}
                         when
                             Array :: array(Type1),
                             Function ::
                                 fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
                             ArrayRes :: array(Type1 | Type2).
```

Like `mapfoldr/3` but skips default-valued entries.

See also `sparse_mapfoldr/5`, `sparse_mapfoldl/3`.

# `sparse_mapfoldr`
*since OTP 29.0* 

```erlang
-spec sparse_mapfoldr(Low, High, Function, InitialAcc :: A, Array) -> {ArrayRes, A}
                         when
                             Low :: array_indx(),
                             High :: array_indx(),
                             Function ::
                                 fun((Index :: array_indx(), Value :: Type1, Acc :: A) -> {Type2, A}),
                             Array :: array(Type1),
                             ArrayRes :: array(Type1 | Type2).
```

Like `mapfoldr/5` but skips default-valued entries.

See also `sparse_mapfoldr/3`, `sparse_mapfoldl/5`.

# `sparse_size`

```erlang
-spec sparse_size(Array :: array()) -> non_neg_integer().
```

Gets the number of entries in the array up until the last non-default-valued
entry.

That is, returns `I+1` if `I` is the last non-default-valued entry in
the array, or zero if no such entry exists.

## Examples

```erlang
1> A = array:set(3, 42, array:new(10)).
2> array:size(A).
10
3> array:sparse_size(A).
4
```

See also `resize/1`, `size/1`.

# `sparse_to_list`

```erlang
-spec sparse_to_list(Array :: array(Type)) -> [Value :: Type].
```

Converts the array to a list, skipping default-valued entries.

## Examples

```erlang
1> A = array:set(2, x, array:new()).
2> array:to_list(A).
[undefined,undefined,x]
3> array:sparse_to_list(A).
[x]
```

See also `to_list/1`  and `to_orddict/1`.

# `sparse_to_orddict`

```erlang
-spec sparse_to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).
```

Converts the array to an ordered list of pairs `{Index, Value}`, skipping
default-valued entries.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_orddict(array:reset(1, A)).
[{0,0},{1,default},{2,2}]
3> array:sparse_to_orddict(array:reset(1, A)).
[{0,0},{2,2}]
```

See also `to_orddict/1`.

# `to_list`

```erlang
-spec to_list(Array :: array(Type)) -> [Value :: Type].
```

Converts the array to a list.

## Examples

```erlang
1> A = array:set(2, x, array:new()).
2> array:to_list(A).
[undefined,undefined,x]
```

See also `from_list/2`, `sparse_to_list/1` and `to_orddict/1`.

# `to_orddict`

```erlang
-spec to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).
```

Converts the array to an ordered list of pairs `{Index, Value}`.

## Examples

```erlang
1> A = array:from_list(lists:seq(0,2), default).
2> array:to_orddict(array:reset(1, A)).
[{0,0},{1,default},{2,2}]
```

See also `from_orddict/2`, `sparse_to_orddict/1` and `to_list/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
