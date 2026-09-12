# `queue`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/queue.erl#L22)

Abstract data type for FIFO queues.

This module provides (double-ended) FIFO queues in an efficient manner.

All functions fail with reason `badarg` if arguments are of wrong type, for
example, queue arguments are not queues, indexes are not integers, and list
arguments are not lists. Improper lists cause internal crashes. An index out of
range for a queue also causes a failure with reason `badarg`.

Some functions, where noted, fail with reason `empty` for an empty queue.

The data representing a queue as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

All operations have an amortized O(1) running time, except `all/2`, `any/2`,
`delete/2`, `delete_r/2`, `delete_with/2`, `delete_with_r/2`, `filter/2`,
`filtermap/2`, `fold/3`, `join/2`, `len/1`, `member/2`, `split/2` that have
O(n). To minimize the size of a queue minimizing the amount of garbage built by
queue operations, the queues do not contain explicit length information, and
that is why [`len/1`](`len/1`) is O(n). If better performance for this
particular operation is essential, it is easy for the caller to keep track of
the length.

Queues are double-ended. The mental picture of a queue is a line of people
(items) waiting for their turn. The queue front is the end with the item that
has waited the longest. The queue rear is the end an item enters when it starts
to wait. If instead using the mental picture of a list, the front is called head
and the rear is called tail.

Entering at the front and exiting at the rear are reverse operations on the
queue.

This module has three sets of interface functions: the _"Original API"_, the
_"Extended API"_, and the _"Okasaki API"_.

The "Original API" and the "Extended API" both use the mental picture of a
waiting line of items. Both have reverse operations suffixed "\_r".

The "Original API" item removal functions return compound terms with both the
removed item and the resulting queue. The "Extended API" contains alternative
functions that build less garbage and functions for just inspecting the queue
ends. Also the "Okasaki API" functions build less garbage.

The "Okasaki API" is inspired by "Purely Functional Data Structures" by Chris
Okasaki. It regards queues as lists. This API is by many regarded as strange and
avoidable. For example, many reverse operations have lexically reversed names,
some with more readable but perhaps less understandable aliases.

# `queue`

```erlang
-type queue() :: queue(_).
```

# `queue`

```erlang
-opaque queue(Item)
```

As returned by `new/0`.

# `drop`

```erlang
-spec drop(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the front item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

## Examples

```erlang
1> Queue1 = queue:from_list([1,2,3,4,5]).
2> Queue2 = queue:drop(Queue1).
3> queue:to_list(Queue2).
[2,3,4,5]
4> queue:drop(queue:new()).
** exception error: empty
```

# `drop_r`

```erlang
-spec drop_r(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the rear item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

## Examples

```erlang
1> Queue1 = queue:from_list([1,2,3,4,5]).
2> Queue2 = queue:drop_r(Queue1).
3> queue:to_list(Queue2).
[1,2,3,4]
4> queue:drop(queue:new()).
** exception error: empty
```

# `get`

```erlang
-spec get(Q :: queue(Item)) -> Item.
```

Returns `Item` at the front of queue `Q`.

Fails with reason `empty` if `Q` is empty.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:get(Queue).
1
3> queue:drop(queue:new()).
** exception error: empty
```

# `get_r`

```erlang
-spec get_r(Q :: queue(Item)) -> Item.
```

Returns `Item` at the rear of queue `Q`.

Fails with reason `empty` if `Q` is empty.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:get_r(Queue).
5
3> queue:drop(queue:new()).
** exception error: empty
```

# `peek`

```erlang
-spec peek(Q :: queue(Item)) -> empty | {value, Item}.
```

Returns tuple `{value, Item}`, where `Item` is the front item of `Q`, or `empty`
if `Q` is empty.

## Examples

```erlang
1> queue:peek(queue:new()).
empty
2> Queue = queue:from_list([1,2,3,4,5]).
3> queue:peek(Queue).
{value,1}
```

# `peek_r`

```erlang
-spec peek_r(Q :: queue(Item)) -> empty | {value, Item}.
```

Returns tuple `{value, Item}`, where `Item` is the rear item of `Q`, or `empty`
if `Q` is empty.

## Examples

```erlang
1> queue:peek_r(queue:new()).
empty
2> Queue = queue:from_list([1,2,3,4,5]).
3> queue:peek_r(Queue).
{value,5}
```

# `cons`

```erlang
-spec cons(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Inserts `Item` at the head of queue `Q1` and returns the new queue `Q2`.

## Examples

```erlang
1> Queue1 = queue:from_list([1, 2, 3]).
2> Queue2 = queue:cons(0, Queue1).
3> queue:to_list(Queue2).
[0,1,2,3]
```

# `daeh`

```erlang
-spec daeh(Q :: queue(Item)) -> Item.
```

Returns the tail item of queue `Q`.

Fails with reason `empty` if `Q` is empty.

## Examples

```erlang
1> Queue = queue:from_list([1, 2, 3]).
2> queue:daeh(Queue).
3
3> queue:drop(queue:new()).
** exception error: empty
```

# `head`

```erlang
-spec head(Q :: queue(Item)) -> Item.
```

Returns `Item` from the head of queue `Q`.

Fails with reason `empty` if `Q` is empty.

## Examples

```erlang
1> Queue = queue:from_list([1, 2, 3]).
2> queue:head(Queue).
1
3> queue:drop(queue:new()).
** exception error: empty
```

# `init`

```erlang
-spec init(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

## Examples

```erlang
1> Queue1 = queue:from_list([1, 2, 3]).
2> Queue2 = queue:init(Queue1).
3> queue:to_list(Queue2).
[1,2]
4> queue:drop(queue:new()).
** exception error: empty
```

# `lait`

> This function is deprecated. queue:lait/1 is deprecated; use queue:liat/1 instead.

```erlang
-spec lait(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

The name [`lait/1`](`lait/1`) is a misspelling - do not use it anymore.

# `last`

```erlang
-spec last(Q :: queue(Item)) -> Item.
```

Returns the tail item of queue `Q`.

Fails with reason `empty` if `Q` is empty.

## Examples

```erlang
1> Queue = queue:from_list([1, 2, 3]).
2> queue:last(Queue).
3
3> queue:drop(queue:new()).
** exception error: empty
```

# `liat`

```erlang
-spec liat(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the tail item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

## Examples

```erlang
1> Queue1 = queue:from_list([1, 2, 3]).
2> Queue2 = queue:liat(Queue1).
3> queue:to_list(Queue2).
[1,2]
4> queue:drop(queue:new()).
** exception error: empty
```

# `snoc`

```erlang
-spec snoc(Q1 :: queue(Item), Item) -> Q2 :: queue(Item).
```

Inserts `Item` as the tail item of queue `Q1` and returns the new queue `Q2`.

## Examples

```erlang
1> Queue1 = queue:from_list([1, 2, 3]).
2> Queue2 = queue:snoc(Queue1, 4).
3> queue:to_list(Queue2).
[1,2,3,4]
```

# `tail`

```erlang
-spec tail(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` that is the result of removing the head item from `Q1`.

Fails with reason `empty` if `Q1` is empty.

## Examples

```erlang
1> Queue1 = queue:from_list([1,2,3,4,5]).
2> Queue2 = queue:tail(Queue1).
3> queue:to_list(Queue2).
[2,3,4,5]
4> queue:drop(queue:new()).
** exception error: empty
```

# `all`
*since OTP 24.0* 

```erlang
-spec all(Pred, Q :: queue(Item)) -> boolean() when Pred :: fun((Item) -> boolean()).
```

Returns `true` if `Pred(Item)` returns `true` for all items `Item` in `Q`,
otherwise `false`.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:all(fun (E) -> E > 3 end, Queue).
false
3> queue:all(fun (E) -> E > 0 end, Queue).
true
```

# `any`
*since OTP 24.0* 

```erlang
-spec any(Pred, Q :: queue(Item)) -> boolean() when Pred :: fun((Item) -> boolean()).
```

Returns `true` if `Pred(Item)` returns `true` for at least one item `Item` in
`Q`, otherwise `false`.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:any(fun (E) -> E > 10 end, Queue).
false
3> queue:any(fun (E) -> E > 3 end, Queue).
true
```

# `delete`
*since OTP 24.0* 

```erlang
-spec delete(Item, Q1) -> Q2 when Item :: T, Q1 :: queue(T), Q2 :: queue(T), T :: term().
```

Returns a copy of `Q1` where the first item matching `Item` is deleted, if there
is such an item.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:delete(3, Queue).
3> queue:member(3, Queue1).
false
```

# `delete_r`
*since OTP 24.0* 

```erlang
-spec delete_r(Item, Q1) -> Q2 when Item :: T, Q1 :: queue(T), Q2 :: queue(T), T :: term().
```

Returns a copy of `Q1` where the last item matching `Item` is deleted, if there
is such an item.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,3,5]).
2> Queue1 = queue:delete_r(3, Queue).
3> queue:to_list(Queue1).
[1,2,3,4,5]
```

# `delete_with`
*since OTP 24.0* 

```erlang
-spec delete_with(Pred, Q1) -> Q2
                     when
                         Pred :: fun((Item) -> boolean()),
                         Q1 :: queue(Item),
                         Q2 :: queue(Item),
                         Item :: term().
```

Returns a copy of `Q1` where the first item for which `Pred` returns `true` is
deleted, if there is such an item.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:delete_with(fun (E) -> E rem 2 =:= 0 end, Queue).
3> queue:to_list(Queue1).
[1,3,4,5]
```

# `delete_with_r`
*since OTP 24.0* 

```erlang
-spec delete_with_r(Pred, Q1) -> Q2
                       when
                           Pred :: fun((Item) -> boolean()),
                           Q1 :: queue(Item),
                           Q2 :: queue(Item),
                           Item :: term().
```

Returns a copy of `Q1` where the last item for which `Pred` returns `true` is
deleted, if there is such an item.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:delete_with_r(fun (E) -> E rem 2 =:= 0 end, Queue).
3> queue:to_list(Queue1).
[1,2,3,5]
```

# `filter`

```erlang
-spec filter(Fun, Q1 :: queue(Item)) -> Q2 :: queue(Item) when Fun :: fun((Item) -> boolean() | [Item]).
```

Returns a queue `Q2` that is the result of calling `Fun(Item)` on all items in
`Q1`.

If `Fun(Item)` returns `true`, `Item` is copied to the result queue. If it
returns `false`, `Item` is not copied. If it returns a list, the list elements
are inserted instead of `Item` in the result queue.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:filter(fun (E) -> E > 2 end, Queue).
3> queue:to_list(Queue1).
[3,4,5]
```

So, `Fun(Item)` returning `[Item]` is thereby semantically equivalent to
returning `true`, just as returning `[]` is semantically equivalent to returning
`false`. But returning a list builds more garbage than returning an atom.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:filter(fun (E) -> [E, E+1] end, Queue).
3> queue:to_list(Queue1).
[1,2,2,3,3,4,4,5,5,6]
```

# `filtermap`
*since OTP 24.0* 

```erlang
-spec filtermap(Fun, Q1) -> Q2
                   when
                       Fun :: fun((Item) -> boolean() | {true, Value}),
                       Q1 :: queue(Item),
                       Q2 :: queue(Item | Value),
                       Item :: term(),
                       Value :: term().
```

Returns a queue `Q2` that is the result of calling `Fun(Item)` on all items in
`Q1`.

If `Fun(Item)` returns `true`, `Item` is copied to the result queue. If it
returns `false`, `Item` is not copied. If it returns `{true, NewItem}`, the
queue element at this position is replaced with `NewItem` in the result queue.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:filtermap(fun (E) -> E > 2 end, Queue).
3> queue:to_list(Queue1).
[3,4,5]
4> Queue2 = queue:filtermap(fun (E) -> {true, E - 100} end, Queue).
5> queue:to_list(Queue2).
[-99,-98,-97,-96,-95]
```

# `fold`
*since OTP 24.0* 

```erlang
-spec fold(Fun, Acc0, Q :: queue(Item)) -> Acc1
              when
                  Fun :: fun((Item, AccIn) -> AccOut),
                  Acc0 :: term(),
                  Acc1 :: term(),
                  AccIn :: term(),
                  AccOut :: term().
```

Calls `Fun(Item, AccIn)` on successive items `Item` of `Queue`, starting with
`AccIn == Acc0`.

The queue is traversed in queue order, that is, from front to
rear. `Fun/2` must return a new accumulator, which is passed to the
next call.  The function returns the final value of the
accumulator. `Acc0` is returned if the queue is empty.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> queue:fold(fun(X, Sum) -> X + Sum end, 0, Queue).
15
3> queue:fold(fun(X, Prod) -> X * Prod end, 1, Queue).
120
```

# `from_list`

```erlang
-spec from_list(L :: [Item]) -> queue(Item).
```

Returns a queue containing the items in `L` in the same order; the head item of
the list becomes the front item of the queue.

## Examples

```erlang
1> Queue = queue:from_list([a,b,c,d,e]).
2> queue:get(Queue).
a
3> queue:to_list(Queue).
[a,b,c,d,e]
```

# `in`

```erlang
-spec in(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Inserts `Item` at the rear of queue `Q1` and returns the resulting queue `Q2`.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:in(100, Queue).
3> queue:to_list(Queue1).
[1,2,3,4,5,100]
```

# `in_r`

```erlang
-spec in_r(Item, Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Inserts `Item` at the front of queue `Q1` and returns the resulting queue `Q2`.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> Queue1 = queue:in_r(100, Queue).
3> queue:to_list(Queue1).
[100,1,2,3,4,5]
```

# `is_empty`

```erlang
-spec is_empty(Q :: queue()) -> boolean().
```

Returns `true` if `Q` is empty; otherwise, returns `false`.

## Examples

```erlang
1> queue:is_empty(queue:new()).
true
2> queue:is_empty(queue:from_list([a])).
false
3> queue:is_empty(42).
** exception error: bad argument
     in function  queue:is_empty/1
        called as queue:is_empty(42)
```

# `is_queue`

```erlang
-spec is_queue(Term :: term()) -> boolean().
```

Returns `true` if `Term` is queue; otherwise, returns `false`.

Note that the test will return `true` for a term coinciding with the
representation of a queue, even when not constructed by this
module. See also note on [data types](`e:system:data_types.md#no_user_types`).

## Examples

```erlang
1> queue:is_queue(queue:from_list([1,2,3,4,5])).
true
2> queue:is_queue(42).
false
```

# `join`

```erlang
-spec join(Q1 :: queue(Item), Q2 :: queue(Item)) -> Q3 :: queue(Item).
```

Returns a queue `Q3` that is the result of joining `Q1` and `Q2` with `Q1` in
front of `Q2`.

## Examples

```erlang
1> Queue1 = queue:from_list([1,3]).
2> Queue2 = queue:from_list([2,4]).
3> queue:to_list(queue:join(Queue1, Queue2)).
[1,3,2,4]
```

# `len`

```erlang
-spec len(Q :: queue()) -> non_neg_integer().
```

Calculates and returns the length of queue `Q`.

## Examples

```erlang
1> queue:len(queue:from_list([1,2,3])).
3
```

# `member`

```erlang
-spec member(Item, Q :: queue(Item)) -> boolean().
```

Returns `true` if `Item` matches some element in `Q`; otherwise, returns `false`.

## Examples

```erlang
1> Queue = queue:from_list([a,b,c,d,e]).
2> queue:member(b, Queue).
true
3> queue:member(42, Queue).
false
```

# `new`

```erlang
-spec new() -> queue(none()).
```

Returns an empty queue.

## Examples

```erlang
1> Q = queue:new().
2> queue:to_list(Q).
[]
3> queue:is_empty(Q).
true
```

# `out`

```erlang
-spec out(Q1 :: queue(Item)) -> {{value, Item}, Q2 :: queue(Item)} | {empty, Q1 :: queue(Item)}.
```

Removes the item at the front of queue `Q1`.

Returns tuple `{{value, Item}, Q2}`, where `Item` is the item removed
and `Q2` is the resulting queue. If `Q1` is empty, tuple `{empty, Q1}`
is returned.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> {{value, 1=Item}, Queue1} = queue:out(Queue).
3> queue:to_list(Queue1).
[2,3,4,5]
4> {empty, _} = queue:out(queue:new()).
```

# `out_r`

```erlang
-spec out_r(Q1 :: queue(Item)) -> {{value, Item}, Q2 :: queue(Item)} | {empty, Q1 :: queue(Item)}.
```

Removes the item at the rear of queue `Q1`.

Returns tuple `{{value, Item}, Q2}`, where `Item` is the item removed
and `Q2` is the new queue. If `Q1` is empty, tuple `{empty, Q1}` is
returned.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> {{value, 5=Item}, Queue1} = queue:out_r(Queue).
3> queue:to_list(Queue1).
[1,2,3,4]
4> {empty, _} = queue:out_r(queue:new()).
```

# `reverse`

```erlang
-spec reverse(Q1 :: queue(Item)) -> Q2 :: queue(Item).
```

Returns a queue `Q2` containing the items of `Q1` in the reverse order.

## Examples

```erlang
1> Queue = queue:from_list([1,2,3,4,5]).
2> QueueRev = queue:reverse(Queue).
3> queue:to_list(QueueRev).
[5,4,3,2,1]
```

# `split`

```erlang
-spec split(N :: non_neg_integer(), Q1 :: queue(Item)) -> {Q2 :: queue(Item), Q3 :: queue(Item)}.
```

Splits `Q1` into two queues.

The `N` front items are put in `Q2` and the rest in `Q3`.

## Examples

```erlang
1> Queue1 = queue:from_list([1,2,3,4,5]).
2> {Queue2, Queue3} = queue:split(2, Queue1).
3> queue:to_list(Queue2).
[1,2]
4> queue:to_list(Queue3).
[3,4,5]
```

# `to_list`

```erlang
-spec to_list(Q :: queue(Item)) -> [Item].
```

Returns a list of the items in the queue in the same order; the front item of
the queue becomes the head of the list.

## Examples

```erlang
1> List = [1,2,3,4,5].
2> Queue = queue:from_list(List).
3> queue:to_list(Queue).
[1,2,3,4,5]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
