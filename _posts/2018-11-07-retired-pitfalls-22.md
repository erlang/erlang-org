---
layout: post
title: Retiring old performance pitfalls
tags: erts compiler
author: John Högberg
---

Erlang/OTP 22 will bring many performance improvements to the table, but most
of them have a broad impact and don't affect the way you write efficient code.
In this post I'd like to highlight a few things that used to be surprisingly
slow but no longer need to be avoided.

### Named fun recursion

Named funs have a neat little feature that might not be obvious at a first
glance; their name is a variable like any other and you're free to pass it to
another function or even return it.

```erlang
deepfoldl(F, Acc0, L) ->
    (fun NamedFun([_|_]=Elem, Acc) -> lists:foldl(NamedFun, Acc, Elem);
         NamedFun([], Acc) -> Acc;
         NamedFun(Elem, Acc) -> F(Elem, Acc)
     end)(L, Acc0).
```

This is cool but a bit of a headache for the compiler. To create a fun we pass
its definition and free variables to a `make_fun2` instruction, but we can't
include the fun itself as a free variable because it hasn't been created yet.
Prior to OTP 22 we solved this by creating a new equivalent fun _inside_ the
fun, but this made recursion surprisingly expensive both in terms of run-time
and memory use.

As of OTP 22 we translate recursion to a direct function call instead which
avoids creating a new fun. Other cases still require recreating the fun, but
they're far less common.

[Optimize named funs and fun-wrapped macros #1973](https://github.com/erlang/otp/pull/1973)

### List subtraction with large operands (-- operator)

While the Erlang VM appears to be pre-emptively scheduled to the programmer,
it's [co-operatively scheduled](https://en.wikipedia.org/wiki/Computer_multitasking#Cooperative_multitasking)
internally. When a native function runs it monopolizes the scheduler until it
returns, so a long-running one can severely harm the responsiveness of the
system. We've therefore written nearly all such functions in a style that
breaks the work into short units that complete quickly enough, but there's
a steadily shrinking list of functions that misbehave, and list subtraction
was one of these.

It's usually pretty straightforward to rewrite functions in this style, but
the [old algorithm](https://github.com/erlang/otp/blob/d9682b02b81fa6e23e554b6e017650eb89ecebed/erts/emulator/beam/erl_bif_lists.c#L195)
processed the second list in a loop around the first list, which is problematic
since both lists can be very long and resuming work in nested loops is often
trickier than expected.

In this case it was easier to just get rid of the nested loop altogether. The
new algorithm starts out by building a red-black tree from the right-hand side
before removing elements from the left-hand side. As all operations on the tree
have `log n` complexity we know that they will finish really quickly, so all we
need to care about is yielding in the outer loops.

This also had the nice side-effect of reducing the worst-case complexity from
`O(n²)` to `O(n log n)` and let us remove some warnings from the reference
manual and [efficiency guide](http://erlang.org/documentation/doc-10.1/doc/efficiency_guide/commoncaveats.html#operator-----). It's worth noting
that the new implementation is always faster than the proposed workarounds, and
that it falls back to the old algorithm when it's faster to do so.

This change will be rolled out in OTP 21.2, big thanks to
Dmytro Lytovchenko (@kvakvs on GitHub) for writing the better half of it!

[Optimize list subtraction (A -- B) and make it yield on large inputs #1998](https://github.com/erlang/otp/pull/1998)

### Lookahead in bit-syntax matching

The optimization pass for bit-syntax matching was completely rewritten in OTP
22 to take advantage of the new SSA-based intermediate format. It applies the
same optimizations as before so already well-optimized code is unlikely to see
any benefit, but it manages to apply them in far more cases.

For those who aren't familiar, all bit-syntax matching operates on a
["match context"](http://erlang.org/doc/efficiency_guide/binaryhandling.html#matching-binaries)
internally, which is a mutable object that keeps track of the current
match position. This helps a lot when matching complicated patterns as it can
zip back and forth as required, saving us from having to match components more
than once.

This is great when matching several different patterns, but it comes in real
handy in loops like the following:

```erlang
trim_zero(<<0,Tail/binary>>) -> trim_zero(Tail);
trim_zero(B) when is_binary(B) -> B.
```

As the compiler can see that `Tail` is passed directly to `trim_zero`, which
promptly begins with a bit-match, it can skip extracting `Tail` as a sub-binary
and pass the match context instead. This is a pretty well-known optimization
called "match context reuse" which greatly improves performance when applied,
and a lot of code has been written with it in mind.

The catch of passing a match context like this is that we have to maintain the
illusion that we're dealing with an immutable _binary_. Whenever it's used in
a non-matching expression we either need to convert the context to an
equivalent binary, or admit defeat and skip the optimization.

While the compiler did a pretty good job prior to OTP 22 it gave up a bit too
easily in many cases, and the most trivial example is almost funny:

```erlang
calls_wrapper(<<"hello",Tail/binary>>) ->
    count_ones(Tail).

%% This simple wrapper prevents context reuse in the call above. :(
count_ones(Bin) -> count_ones_1(Bin, 0).

count_ones_1(<<1, Tail/binary>>, Acc) -> count_ones_1(Tail, Acc + 1);
count_ones_1(<<_, Tail/binary>>, Acc) -> count_ones_1(Tail, Acc);
count_ones_1(<<>>, Acc) -> Acc.
```

A trickier example can be found in the `string` module:

```erlang
bin_search_inv_1(<<CP1/utf8, BinRest/binary>>=Bin0, Cont, Sep) ->
    case BinRest of
        %% 1
        <<CP2/utf8, _/binary>> when ?ASCII_LIST(CP1, CP2) ->
            case CP1 of
                Sep ->
                    %% 2
                    bin_search_inv_1(BinRest, Cont, Sep);
                _ ->
                    %% 3
                    [Bin0|Cont]
            end;
        %% ... snip ...
```

What we're looking at is a fast-path for ASCII characters; when both `CP1` and
`CP2` are ASCII we know that `CP1` is not a part of a grapheme cluster and we
can thus avoid a call to `unicode_util:gc/1`. It's not a particularly expensive
function but calling it once per character adds up quickly.

At first glance it might seem safe to pass the context at `2`, but this is made
difficult by `Bin0` being returned at `3`. As contexts are mutable and change
their position whenever a match succeeds, naively converting `Bin0` back to a
binary would give you what comes after `CP2` instead.

Now, you might be wondering why we couldn't simply restore the position before
converting `Bin0` back to a binary. It's an obvious thing to do but before OTP
22 the context tracked not only the current position but also previous ones
needed when backtracking. These were saved in per-context "slots" which were
mutable and heavily reused, and the match at `1` clobbered the slot needed to
restore `Bin0`.

This also meant that a context couldn't be used again after being passed to
another function or entering a `try`/`catch`, which made it more or less
impossible to apply this optimization in code that requires looking ahead.

As of OTP 22 these positions are stored outside the context so there's no need
to worry about them becoming invalid, making it possible to optimize the above
cases.

[Rewrite BSM optimizations in the new SSA-based intermediate format #1958](https://github.com/erlang/otp/pull/1958)
