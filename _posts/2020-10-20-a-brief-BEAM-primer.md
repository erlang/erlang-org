---
layout: post
title: A brief introduction to BEAM
tags: BEAM compiler erts
author: John Högberg
---

This post is a brief primer on BEAM, the virtual machine that executes user
code in the Erlang Runtime System (ERTS). It's intended to help those new to
BEAM follow an upcoming series of posts about the JIT in OTP 24, leaving
implementation details for later.

BEAM is often confused with ERTS and it's important to distinguish between the
two; BEAM is just the virtual machine and it has no notion of processes, ports,
ETS tables, and so on. It merely executes instructions and while ERTS has
influenced their design, it doesn't affect what they do when the code is
running, so you don't need to understand ERTS to understand BEAM.

BEAM is a register machine, where all instructions operate on named registers.
Each register can contain any Erlang term such as an integer or a tuple, and it
helps to think of them as simple variables. The two most important kinds of
registers are:

- `X`: these are used for temporary data and passing data between functions.
  They don't require a stack frame and can be freely used in any function, but
  there are certain limitations which we'll expand on later.
- `Y`: these are local to each stack frame and have no special
  limitations beyond needing a stack frame.

Control flow is handled by instructions that test a certain condition and
either move on to the next instruction or branch to its _fail label_, noted by
`{f,Index}`. For example `{test,is_integer,{f,7},[{x,0}]}.` checks if `{x,0}`
contains an integer and jumps to label 7 if it doesn't.

Function arguments are passed from left to right in `X` registers, starting at
`{x,0}`, and the result is returned in `{x,0}`.

It's easier to explain how this fits together through example, so let's walk
through a few:

```erlang
sum_tail(List) ->
    sum_tail(List, 0).

sum_tail([Head | Tail], Acc) ->
    sum_tail(Tail, Head + Acc);
sum_tail([], Acc) ->
    Acc.
```

Let's use `erlc -S` to look at the instructions one by one:

<pre class="highlight">
<em>%% sum_tail/1, entry label is 2</em>
{function, sum_tail, 1, 2}.

  <em>%% Marks a jump target with the label 1.</em>
  {label,1}.

    <em>%% Special instruction that raises a function_clause
    %% exception. Unused in this function.</em>
    {func_info,{atom,primer},{atom,sum_tail},1}.

  {label,2}.
    <em>%% The meat of the function starts here.
    %%
    %% Our only argument - <b>List</b> - is in <b>{x,0}</b> and
    %% since sum_tail/2 expects it to be the first
    %% argument we can leave it be. We'll pass the
    %% integer 0 as the second argument in <b>{x,1}</b>.</em>
    {move,{integer,0},{x,1}}.

    <em>%% Tail call sum_tail/2, whose entry label is 4.</em>
    {call_only,2,{f,4}}.

<em>%% sum_tail/2, entry label is 4</em>
{function, sum_tail, 2, 4}.
  {label,3}.
    {func_info,{atom,primer},{atom,sum_tail},2}.
  {label,4}.

    <em>%% Test whether we have a non-empty list, and jump to
    %% the base case at label 5 if we don't.</em>
    {test,is_nonempty_list,{f,5},[{x,0}]}.

    <em>%% Unpack the list in the first argument, placing the
    %% head in <b>{x,2}</b> and the tail in <b>{x,0}</b>.</em>
    {get_list,{x,0},{x,2},{x,0}}.

    <em>%% Add the head and our accumulator (remember that the
    %% second function argument is in <b>{x,1}</b>), and place
    %% the result in <b>{x,1}</b>.
    %%
    %% A fail label of 0 means that we want the
    %% instruction to throw an exception on error, rather
    %% than jump to a given label.</em>
    {gc_bif,'+',{f,0},3,[{x,2},{x,1}],{x,1}}.

    <em>%% Tail-call ourselves to handle the rest of the list,
    %% the arguments are already in the right registers.</em>
    {call_only,2,{f,4}}.

  {label,5}.
    <em>%% Test whether our argument was the empty list. If
    %% not, we jump to label 3 to raise a function_clause
    %% exception.</em>
    {test,is_nil,{f,3},[{x,0}]}.

    <em>%% Return our accumulator.</em>
    {move,{x,1},{x,0}}.
    return.
</pre>

Simple enough, isn't it?

I glossed over one little detail though; the mysterious number `3` in the
addition instruction. This number tells us how many `X` registers hold live
data in case we need more memory, so they can be preserved while the rest are
discarded as garbage. As a consequence, it's unsafe to refer to higher `X`
registers after this instruction as their contents may be invalid (in this case
`{x,3}` and above).

Function calls are similar; we may schedule ourselves out whenever we call or
return from a function, and we'll only preserve the function arguments/return
value when we do so. This means that all `X` registers except for `{x,0}` are
invalid after a call even if you knew for certain that the called function
didn't touch a certain register.

This is where `Y` registers enter the picture. Let's take the previous example
and make it body-recursive instead:

```erlang
sum_body([Head | Tail]) ->
    Head + sum_body(Tail);
sum_body([]) ->
    0.
```
.
<pre class="highlight">
{function, sum_body, 1, 7}.
  {label,6}.
    {func_info,{atom,primer},{atom,sum_body},1}.
  {label,7}.
    {test,is_nonempty_list,{f,8},[{x,0}]}.

    <em>%% Allocate a stack frame with a single Y register.
    %% Since this instruction may need more memory, we
    %% tell the garbage collector that we currently have
    %% one live X register (our list argument in <b>{x,0}</b>).</em>
    {allocate,1,1}.

    <em>%% Unpack the list, placing the head in <b>{y,0}</b> and
    %% the tail in <b>{x,0}</b>.</em>
    {get_list,{x,0},{y,0},{x,0}}.

    <em>%% Body-call ourselves. Note that while this kills all
    %% X registers, it leaves Y registers alone so our
    %% head is still valid.</em>
    {call,1,{f,7}}.

    <em>%% Add the head to our return value and store the
    %% result in <b>{x,0}</b>.</em>
    {gc_bif,'+',{f,0},1,[{y,0},{x,0}],{x,0}}.

    <em>%% Deallocate our stack frame and return.</em>
    {deallocate,1}.
    return.

  {label,8}.
    {test,is_nil,{f,6},[{x,0}]}.

    <em>%% Return the integer 0.</em>
    {move,{integer,0},{x,0}}.
    return.
</pre>

Notice how the call instruction changed now that we're in a stack frame? There
are three different call instructions:

- `call`: ordinary call as in the example. Control flow will resume at
  the next instruction when the called function returns.
- `call_last`: tail call when there is a stack frame. The current frame will
  be deallocated before the call.
- `call_only`: tail call when there is no stack frame.

Each of these have a variant for calling functions in other modules (e.g.
`call_ext`), but they're otherwise identical.

So far we've only looked at using terms, but what about creating them? Let's
have a look:

```erlang
create_tuple(Term) ->
    {hello, Term}.
```
.
<pre class="highlight">
{function, create_tuple, 1, 10}.
  {label,9}.
    {func_info,{atom,primer},{atom,create_tuple},1}.
  {label,10}.
    <em>%% Allocate the three words needed for a 2-tuple, with
    %% a liveness annotation of 1 indicating that <b>{x,0}</b>
    %% is alive in case we need to GC.</em>
    {test_heap,3,1}.

    <em>%% Create the tuple and place the result in <b>{x,0}</b></em>
    {put_tuple2,{x,0},{list,[{atom,hello},{x,0}]}}.
  
    return.
</pre>

This is a bit magical in the sense that there's an unseen register for memory
allocations, but allocation is rarely far apart from use and it's usually
pretty easy to follow. The same principle applies for lists ([consing]),
floats, and funs as well following [PR 2765].

More complicated types like maps, big integers, references, and so on are
created by special instructions that may GC on their own (or allocate outside
the heap in a "heap fragment") as their size can't be statically determined in
advance.

Now let's look at something more uncommon: exceptions.

```erlang
exception() ->
    try
        external:call()
    catch
        throw:example -> hello
    end.
```
.
<pre class="highlight">
{function, exception, 0, 12}.
  {label,11}.
    {func_info,{atom,primer},{atom,exception},0}.
  {label,12}.
    {allocate,1,0}.
  
    <em>%% Place a catch tag in <b>{y,0}</b>. If an exception is
    %% raised while this tag is the most current one,
    %% the control flow will resume at <b>{f,13}</b> in this
    %% stack frame.</em>
    {'try',{y,0},{f,13}}.

    {call_ext,0,{extfunc,external,call,0}}.

    <em>%% Deactivate the catch tag before returning with the
    %% result from the call.</em>
    {try_end,{y,0}}.

    {deallocate,1}.
    return.

  {label,13}.
    <em>%% Uh oh, we've got an exception. Kill the catch tag
    %% and place the exception class in <b>{x,0}</b>, the error
    %% reason/thrown value in <b>{x,1}</b>, and the stack trace
    %% in <b>{x,2}</b>.</em>
    {try_case,{y,0}}.

    <em>%% Return 'hello' if the user threw 'example'</em>
    {test,is_eq_exact,{f,14},[{x,0},{atom,throw}]}.
    {test,is_eq_exact,{f,14},[{x,1},{atom,example}]}.
    {move,{atom,hello},{x,0}}.
    {deallocate,1}.
    return.

  {label,14}.
    <em>%% Otherwise, rethrow the exception since no catch
    %% clause matched.</em>
    {bif,raise,{f,0},[{x,2},{x,1}],{x,0}}.
</pre>

By now you've probably noticed how the control flow only moves forward; just
like Erlang itself the only way to loop is through recursion. The one exception
to this is the receive construct, which may loop until a matching message has
been received:

```erlang
selective_receive(Ref) ->
    receive
        {Ref, Result} -> Result
    end.
```
.
<pre class="highlight">
{function, selective_receive, 1, 16}.
  {label,15}.
    {func_info,{atom,primer},{atom,selective_receive},1}.
  {label,16}.
    {allocate,1,1}.

    <em>%% We may be scheduled out while waiting for a
    %% message, so we'll preserve our <b>Ref</b> in <b>{y,0}</b>.</em>
    {move,{x,0},{y,0}}.

  {label,17}.
    <em>%% Pick the next message from the process' message box
    %% and place it in <b>{x,0}</b>, jumping to label 19 if the
    %% message box is empty.</em>
    {loop_rec,{f,19},{x,0}}.
  
    <em>%% Does it match our pattern? If not, jump to label 18
    %% and try the next message.</em>
    {test,is_tuple,{f,18},[{x,0}]}.
    {test,test_arity,{f,18},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {test,is_eq_exact,{f,18},[{x,1},{y,0}]}.

    <em>%% We've got a match, extract the result and remove
    %% the message from the mailbox.</em>
    {get_tuple_element,{x,0},1,{x,0}}.
    remove_message.
    {deallocate,1}.
    return.

  {label,18}.
    <em>%% The message didn't match, loop back to handle our
    %% next message. Note that the current message remains
    %% in the inbox since a different receive may be
    %% interested in it.</em>
    {loop_rec_end,{f,17}}.

  {label,19}.
    <em>%% Wait until the next message arrives, returning to
    %% the start of the loop when it does. If there's a
    %% timeout involved, it will be handled here.</em>
    {wait,{f,17}}.
</pre>

There's not much more to it, and if you feel comfortable following the examples
above you should have no problems with the JIT series.

If you're curious about which instructions there are, you can find a brief
description of every instruction in [genop.tab].

[genop.tab]: https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab
[consing]: https://en.wikipedia.org/wiki/Cons
[PR 2765]: https://github.com/erlang/otp/pull/2765
