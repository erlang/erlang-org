---
layout: post
title: Erlang/OTP 27 Highlights
tags: erlang otp 27 release
author: Björn Gustavsson
---

Erlang/OTP 27 is finally here. This blog post will introduce the new
features that we are most excited about.

A list of all changes is found in [Erlang/OTP 27 Readme](https://erlang.org/patches/OTP-27.0).
Or, as always, look at the release notes of the application you are interested in.
For instance:
[Erlang/OTP 27 - Erts Release Notes - Version 15.0](https://www.erlang.org/doc/apps/erts/notes.html#erts-15.0).

This year's highlights mentioned in this blog post are:

* [Overhauled documentation system](#overhauled-documentation-system)
* [Triple-Quoted strings](#triple-quoted-strings)
* [Sigils](#sigils)
* [No need to enable feature `maybe`](#no-need-to-enable-feature-maybe)
* [The new `json` module](#the-new-json-module)
* [Process labels](#process-labels)
* [New functionality in STDLIB](#new-functionality-in-stdlib)
* [New SSL client-side stapling support](#new-ssl-client-side-stapling-support)
* [`tprof`: Yet another profiling tool](#tprof-yet-another-profiling-tool)
* [Multiple trace sessions](#multiple-trace-sessions)
* [Native coverage support](#native-coverage-support)
* [Deprecating archives](#deprecating-archives)

# Overhauled documentation system

The Erlang/OTP documentation before Erlang/OTP 27 was authored in
[XML](https://en.wikipedia.org/wiki/XML), from which the
[Erl_Docgen](https://www.erlang.org/docs/26/apps/erl_docgen/)
application could generate HTML web pages, PDFs, or Unix man pages.
The reason for generating PDFs is that the documentation used to be
printed as
[actual paper books](https://erlangforums.com/t/old-printed-otp-documentation-cover/1989/2).
The last time the books were printed were for the Erlang/OTP R7 released in 2000.

As an example, here is the XML code for
[`lists:duplicate/2`](https://www.erlang.org/docs/26/man/lists#duplicate-2)
from Erlang/OTP 26:

```xml
    <func>
      <name name="duplicate" arity="2" since=""/>
      <fsummary>Make <c>N</c> copies of element.</fsummary>
      <desc>
        <p>Returns a list containing <c><anno>N</anno></c> copies of term
          <c><anno>Elem</anno></c>.</p>
        <p><em>Example:</em></p>
        <pre>
> <input>lists:duplicate(5, xx).</input>
[xx,xx,xx,xx,xx]</pre>
      </desc>
    </func>
```

The XML code was stored in separate files, not in the source
code. When building the documentation, the function specs from the
source code would be combined with the text from the documentation
file. It was the responsibility of the writer to ensure that variables
mentioned in the documentation body matched the names in the function
spec.

One thing never said about Erl_Docgen and the old documentation system
was that it made writing documentation enjoyable and effortless. That
was one thing we wanted to change with the new documentation system.
We wanted to make it fun to write documentation, or at least to
require less attention to tedious details such as using XML tags
correctly.

In Erlang/OTP 27, the documentation is written in
[Markdown](https://en.wikipedia.org/wiki/Markdown) and is placed in
the source code before the function spec and implementation. Here is
the documentation and implementation of
[`lists:duplicate/2`](https://www.erlang.org/doc/man/lists#duplicate/2)
in Erlang/OTP 27:

    -doc """
    Returns a list containing `N` copies of term `Elem`.

    _Example:_

    ```erlang
    > lists:duplicate(5, xx).
    [xx,xx,xx,xx,xx]
    ```
    """.

    -spec duplicate(N, Elem) -> List when
          N :: non_neg_integer(),
          Elem :: T,
          List :: [T],
          T :: term().

    duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

    duplicate(0, _, L) -> L;
    duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).
    ```

The documentation is placed in a
[triple-quoted string](#triple-quoted-strings)
following
the [`-doc` attribute](https://www.erlang.org/eeps/eep-0059).

Having the documentation near the spec makes its easy to ensure that
the text refers to variables defined in the function spec.

Another goal we had was to replace Erl_Docgen with a tool more widely
used so that we wouldn't have to carry the entire burden for
maintaining it. We did that by using
[ExDoc](https://hexdocs.pm/ex_doc/readme.html), which is also used by
the [Elixir](https://elixir-lang.org) language and most, if not all,
Elixir projects.

An issue that arose is whether it's advisable to include user
documentation within the source code. Wouldn't this make it much harder
to maintain the code?

I don't claim to have a universal response to that concern, but in the
case of Erlang/OTP, most actively developed code exists within modules
lacking documentation. Typically, OTP applications consist of one or
a few modules containing the documented API, while the bulk of the
implementation is found in other modules.

For example, the interface to the Erlang compiler is found in the
[compile](https://www.erlang.org/doc/man/compile) module, while most
of the code being executed resides in one of the other 59 modules
of the Compiler application. Similarly, the [SSL
application](https://www.erlang.org/doc/apps/ssl) comprises 76 modules,
of which merely four contain documentation.

Another application that is frequently updated is
[ERTS](https://www.erlang.org/doc/apps/erts). However, most of ERTS is
implemented in C (and some C++), while much of the actual
Erlang code within ERTS is located in modules without documentation.

There are, of course, some exceptions to how applications are
structured, for example the STDLIB application, where most modules are
documented. However, STDLIB is a mature application that is updated
relatively infrequently.


# Triple-Quoted strings

To facilitate writing documentation attributes containing many lines
of text, triple-quoted strings as described in [EEP
64](https://www.erlang.org/eeps/eep-0064) have been
implemented. Triple-quoted strings come in handy whenever one needs
to include multiple line of text in Erlang source code. For example,
assume that we want to define a function that outputs some
quotations:

```
1> t:quotes().
"I always have a quotation for everything -
it saves original thinking." - Dorothy L. Sayers

"Real stupidity beats artificial intelligence every time."
- Terry Pratchett
ok
```

In Erlang/OTP 26, there are several different ways to do that, but of none
of them are particularly satisfying. For example, the text can be put into a
single string:

```erlang
quotes() ->
    S = "\"I always have a quotation for everything -
it saves original thinking.\" - Dorothy L. Sayers

\"Real stupidity beats artificial intelligence every time.\"
- Terry Pratchett\n",
    io:put_chars(S).
```

This works, but is ugly. We must also remember to escape every quote
character.

A cleaner way is to use multiple strings, one for each line, letting
the compiler combine them:


```erlang
quotes() ->
    S = "\"I always have a quotation for everything -\n"
        "it saves original thinking.\" - Dorothy L. Sayers\n"
        "\n"
        "\"Real stupidity beats artificial intelligence every time.\"\n"
        "- Terry Pratchett\n",
    io:put_chars(S).
```

That is a little bit nicer, but we'll need to type more quote characters
and we must not forget to add `\n` at the end of each string. To
make sure that we don't forget to insert the newlines, we could delegate
that mundane chore to the computer:

```erlang
quotes() ->
    S = ["\"I always have a quotation for everything -",
         "it saves original thinking.\" - Dorothy L. Sayers",
         "",
         "\"Real stupidity beats artificial intelligence every time.\"",
         "- Terry Pratchett"],
    io:put_chars(lists:join("\n", S)),
    io:nl().
```

In Erlang/OTP 27, we can use a triple-quoted string:

```
quotes() ->
    S = """
        "I always have a quotation for everything -
        it saves original thinking." - Dorothy L. Sayers

        "Real stupidity beats artificial intelligence every time."
        - Terry Pratchett
        """,
    io:put_chars(S),
    io:nl().
```

The ending `"""` determines how much each line in the string should be
indented. The same characters that precede `"""` are deleted from all
lines between the beginning and terminating delimiters. For this
particular example, all space characters are removed since all have
the same indentation as the terminating `"""`.  Neither quote
characters nor backslashes are special in the lines enclosed by the
triple-quotes, so there is no need to escape anything.

Here is another example to show the versatility of triple-quoted
strings:

```
effect_warning() ->
    """
    f() ->
        %% Test that the compiler warns for useless tuple building.
        {a,b,c},
        ok.
    """.
```

The function returns a string containing a short Erlang function.

Assuming that `effect_warning/0` is defined in module `t`, it can be
called like so:

```
1> io:format("~ts\n", [t:effect_warning()]).
%% Test that the compiler warns for useless tuple building.
f() ->
    {a,b,c},
    ok.
```

Note that indentation of the Erlang code for function `f/0` is retained.

For more information, see section [String](https://www.erlang.org/doc/reference_manual/data_types#string)
in the Reference Manual.


# Sigils

Sigils for string literals as described in [EEP 66](https://www.erlang.org/eeps/eep-0066)
have been implemented.

Continuing with the theme of quotes, let's explore why sigils were
introduced into Erlang, drawing inspiration from the wisdom of ancient
Greek philosophers:

```erlang
1> t:greek_quote().
"Know thyself" (Greek: Γνῶθι σαυτόν)
ok
```

In Erlang/OTP 26, this can be implemented as follows:

```erlang
greek_quote() ->
    S = "\"Know thyself\" (Greek: Γνῶθι σαυτόν)",
    io:format("~ts\n", [S]).
```

At this point, we get some customer feedback indicating that the
modules containing all the quotes are consuming an excessive amount of
memory. Each character in a string consumes 16 bytes of memory (on a
64-bit computer). That could be reduced to one byte for each character
if a binary were to be used instead of a string.  (Actually, one byte
for each US ASCII character and two bytes for each Greek letter.)

That change should be really easy. Let's try:

```erlang
greek_quote() ->
    S = <<"\"Know thyself\" (Greek: Γνῶθι σαυτόν)">>,
    io:format("~ts\n", [S]).
```

That works for the English text, but not for the Greek characters:

```erlang
2> t:greek_quote().
"Know thyself" (Greek: ½ö¸¹ Ã±ÅÄÌ½)
```

What's wrong?

Strings in binary expression are by default assumed to be a sequence
of byte-size characters. Therefore, this expression:

```erlang
1> <<"Γνῶθι">>.
<<147,189,246,184,185>>
```

is [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for:

```erlang
2> <<$Γ:8, $ν:8, $ῶ:8, $θ:8, $ι:8>>.
<<147,189,246,184,185>>
```

It is necessary to specify that the characters are to be encoded as
[UTF-8](https://en.wikipedia.org/wiki/UTF-8)
encoded characters by appending an `/utf8` suffix:

```erlang
greek_quote() ->
    S = <<"\"Know thyself\" (Greek: Γνῶθι σαυτόν)"/utf8>>,
    io:format("~ts\n", [S]).
```

That works because `<<"Γνῶθι"/utf8>>` is syntactic sugar for
`<<$Γ/utf8, $ν/utf8, $ῶ/utf8, $θ/utf8, $ι/utf8>>`.

Enter sigils.

```
greek_quote() ->
    S = ~B["Know thyself" (Greek: Γνῶθι σαυτόν)],
    io:format("~ts\n", [S]).
```

The `~` character begins a sigil. It is usually followed by a letter that
indicates how the characters in the string should be interpreted or encoded.

In this case the character `B` means that the characters should be put into a binary in UTF-8 encoding,
and also that that no escape characters are allowed.

After `B` follows the start delimiter, in this case `[`.  Since no escape characters
are allowed, it is necessary to choose delimiters that don't occur in the string
contents. After the contents follows the end delimiter, in this case `]`.

The `B` sigil is the default sigil that is used if the letter following
`~` is omitted. Thus we get the same binary and the same output if we omit the `B`:

```
greek_quote() ->
    S = ~["Know thyself" (Greek: Γνῶθι σαυτόν)],
    io:format("~ts\n", [S]).
```

Sigils can also be used to begin a triple-quoted string. Returning to
the quotations example from the previous section, a binary literal can
be created by inserting `~` before the leading `"""`:

```
quotes() ->
    S = ~"""
         "I always have a quotation for everything -
         it saves original thinking." - Dorothy L. Sayers

         "Real stupidity beats artificial intelligence every time."
         - Terry Pratchett
         """,
    io:put_chars(S),
    io:nl().
```

Here follows a few quick examples to show the other sigils.

`~b` creates a binary in the same way as `~B`, except that backslashes
will be interpreted as an escape character. This can be useful if one
want to insert control characters such as TAB (`\t`) into a string:

```
1> ~b"abc\txyz".
<<"abc\txyz">>
```

Here we used the `"` character as delimiters as it is not used within
the string.

`~s` creates a string in the usual way. The only useful way it differs
from a plain quoted string is that the delimiters can be switched. That
way, one can avoid the hassle of escaping quote characters and still
get to use control characters such as TAB:

```
2> ~s{"abc\txyz"}.
"\"abc\txyz\""
```

`~S` creates a string, but does not support escaping of characters
within the string, similar to `~B`.

For more information, see section [Sigil](https://www.erlang.org/doc/reference_manual/data_types#sigil)
in the Reference Manual.


# No need to enable feature `maybe`

The [maybe expression](https://www.erlang.org/doc/reference_manual/expressions#maybe)
was introduced as a [feature](https://www.erlang.org/doc/reference_manual/features.html)
in Erlang/OTP 25. In that release, it was necessary to enable it both in
the compiler and the runtime system.

Erlang/OTP 26 lifted the necessity to enable `maybe` in the runtime system.

Now in Erlang/OTP 27, `maybe` is enabled by default in the compiler.
In the example from
[last year's blog post](https://www.erlang.org/blog/otp-26-highlights/#no-need-to-enable-feature-maybe-in-the-runtime-system),
the line `-feature(maybe_expr, enable).` can now be removed:

{% raw %}


```
$ cat t.erl
-module(t).
-export([listen_port/2]).
listen_port(Port, Options) ->
    maybe
        {ok, ListenSocket} ?= inet_tcp:listen(Port, Options),
        {ok, Address} ?= inet:sockname(ListenSocket),
        {ok, {ListenSocket, Address}}
    end.
$ erlc t.erl
$ erl
Erlang/OTP 27 . . .

Eshell V15.0  (abort with ^G)
1> t:listen_port(50000, []).
{ok,{#Port<0.5>,{{0,0,0,0},50000}}}
```
{% endraw %}

When `maybe` is used as an atom, it need to be quoted. For example:

```erlang
will_succeed(. . .) -> yes;
will_succeed(. . .) -> no;
   .
   .
   .
will_succeed(_) -> 'maybe'.
```

Alternatively, it is still possible to disable the `maybe_expr` feature. With
the feature disabled, `maybe` can be used as an atom without quotes.

One way to disable `maybe` is to use the `-disable-feature` option when compiling.
For example:

```
erlc -disable-feature maybe_expr *.erl
```

Another way to disable `maybe` is to add the following directive to
the source code:

```
-feature(maybe_expr, disable).
```

# The new `json` module

There is a new module [`json`](https://www.erlang.org/doc/man/json) in
STDLIB for generating and parsing
[JSON (JavaScript Object Notation)](https://en.wikipedia.org/wiki/JSON).

It is implemented by [Michał
Muskała](https://github.com/michalmuskala) who has also implemented
the [`Jason`](https://github.com/michalmuskala/jason) library for
Elixir. `Jason` is known for being faster than other pure Erlang or
Elixir JSON libraries. The `json` module is not a pure translation of
the Elixir code for Jason, but a re-implementation with even better
performance than `Jason`.

As an example, imagine that we have this file `quotes.json` with
quotes from the film [Jason and the
Argonauts](https://en.wikipedia.org/wiki/Jason_and_the_Argonauts_(1963_film)):

```json
[
    {"quote": "The gods are best served by those who need their help the least.",
     "attribution": "Zeus",
     "verified": true},
    {"quote": "Now the voyage is over, I don't want any trouble to begin.",
     "attribution": "Jason",
     "verified": true}
]
```

The JSON contents of the file can be be decoded by calling
[json:decode/1](https://www.erlang.org/doc/man/json#decode/1):

```erlang
1> {ok,JSON} = file:read_file("quotes.text").
{ok,<<"[\n   {\"quote\": \"The gods are best served by those who need their help the least.\",\n    \"attribution\": \"Zeus\""...>>}
2> json:decode(JSON).
[#{<<"attribution">> => <<"Zeus">>,
   <<"quote">> =>
       <<"The gods are best served by those who need their help the least.">>,
   <<"verified">> => true},
 #{<<"attribution">> => <<"Jason">>,
   <<"quote">> =>
       <<"Now the voyage is over, I don't want any trouble to begin.">>,
   <<"verified">> => true}]
```

By default, for safety, the keys for objects are translated to binaries. Using atoms
could open up for
[denial-of-service attacks](https://en.wikipedia.org/wiki/Denial-of-service_attack)
if a malicious JSON object would define millions of unique keys.

For convenience, it is still possible to convert keys to atoms in
a safe way by using a *decoder callback*. Here is an example:

```erlang
1> Push = fun(Key, Value, Acc) -> [{binary_to_existing_atom(Key), Value} | Acc] end.
#Fun<erl_eval.40.39164016>
```

This fun converts the key for a JSON object to an **existing** atom,
or raises an exception if no such atom exists.

Since this example is run from the shell, we'll need to make sure that all possible keys
are known atoms:

```erlang
2> {quote,attribution,verified}.
{quote,attribution,verified}
```

This would normally not be necessary when JSON decoding is done in an Erlang module,
because the atoms to be used as keys would presumably be defined naturally by being used
when processing the decoded JSON objects.

With this preparation done, the JSON decoder can be called using the `Push` fun
as an `object_push` decoder callback:

```erlang
3> {Qs,_,<<>>} = json:decode(JSON, [], #{object_push => Push}), Qs.
[#{quote =>
       <<"The gods are best served by those who need their help the least.">>,
   attribution => <<"Zeus">>,verified => true},
 #{quote =>
       <<"Now the voyage is over, I don't want any trouble to begin.">>,
   attribution => <<"Jason">>,verified => true}]
```

The [json:encode/1](https://www.erlang.org/doc/man/json#encode/1) function encodes
an Erlang term to JSON:

```erlang
4> io:format("~ts\n", [json:encode(Qs)]).
[{"quote":"The gods are best served by those who need their help the least.","attribution":"Zeus","verified":true},{"quote":"Now the voyage is over, I don't want any trouble to begin.","attribution":"Jason","verified":true}]
ok
```

The encoder accepts binaries, atoms, and integer as keys for objects,
so there is no need to customize encoding for this particular example.

However, when necessary, it is possible to customize the encoding. For
example, assume that we want to store each quotation in a three-tuple
instead of in a map:

```erlang
1> Q = [{~"The gods are best served by those who need their help the least.",
~"Zeus",true},
{~"Now the voyage is over, I don't want any trouble to begin.",
~"Jason",true}].
[{<<"The gods are best served by those who need their help the least.">>,
  <<"Zeus">>,true},
 {<<"Now the voyage is over, I don't want any trouble to begin.">>,
  <<"Jason">>,true}]
```

The `json:encode/1` function does not handle that format by default, but it can be
handled by defining an *encoder function*:

```erlang
quote_encoder({Q, A, V}, Encode)
  when is_binary(Q), is_binary(A), is_boolean(V) ->
    json:encode_map(#{quote => Q,
                      attribution => A,
                      verified => V},
                    Encode);
quote_encoder(Other, Encode) ->
    json:encode_value(Other, Encode).
```

The first clause matches a tuple of size three that looks like a
quotation. If it matches, it is converted to the map representation
for a JSON object, which is then converted by the utility function
[json:encode_map/1](https://www.erlang.org/doc/man/json#encode_map/2)
to JSON.

The second clause handles all other Erlang terms by calling the
default encoding function
[json:encode_value/2](https://www.erlang.org/doc/man/json#encode_value/2)
for converting a term to JSON.

Assuming that this function is defined in module `t`, the conversion to JSON
is invoked as follows:

```erlang
2> io:format("~ts\n", [json:encode(Q, fun t:quote_encoder/2)]).
[{"quote":"The gods are best served by those who need their help the least.","attribution":"Zeus","verified":true},{"quote":"Now the voyage is over, I don't want any trouble to begin.","attribution":"Jason","verified":true}]
```

The JSON encoder will call the callback recursively for given term. That can
be clearly seen if we modify the second clause of `quote_encoder/2` to also
print the value of `Other`:

```erlang
3> json:encode(Q, fun t:quote_encoder/2), ok.
-- [{<<"The gods are best served by those who need their help the least.">>,
     <<"Zeus">>,true},
    {<<"Now the voyage is over, I don't want any trouble to begin.">>,
     <<"Jason">>,true}]
-- <<"quote">>
-- <<"The gods are best served by those who need their help the least.">>
-- <<"attribution">>
-- <<"Zeus">>
-- <<"verified">>
-- true
-- <<"quote">>
-- <<"Now the voyage is over, I don't want any trouble to begin.">>
-- <<"attribution">>
-- <<"Jason">>
-- <<"verified">>
-- true
```

# Process labels

As an help for debugging or observing in general, labels can be now
set on non-registered processes using
[`proc_lib:set_label/1`](https://www.erlang.org/doc/man/proc_lib#set_label/1).

The label is an arbitrary term. The label is shown by the the shell
command `i/0` and by [`observer`](https://www.erlang.org/doc/man/observer).
They can also be found in the dictionary section of a
[crash dump](https://www.erlang.org/doc/man/crashdump_viewer).

Here is an example where five labeled quote-handler processes are started and
inspected:

```erlang
1> F = fun(I) ->
   spawn_link(fun() ->
     proc_lib:set_label({quote_handler, I}),
     receive _ -> ok end
   end)
   end.
#Fun<erl_eval.42.39164016>
2> Ps = [F(I) || I <- lists:seq(1, 5)].
[<0.91.0>,<0.92.0>,<0.93.0>,<0.94.0>,<0.95.0>]
3> proc_lib:get_label(hd(Ps)).
{quote_handler,1}
4> i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack
<0.0.0>               erl_init:start/2                       987     5347    0
init                  init:loop/1                              2
   .
   .
   .
{quote_handler,1}     prim_eval:'receive'/2                    9
<0.92.0>              erlang:apply/2                         233     4006    0
{quote_handler,2}     prim_eval:'receive'/2                    9
<0.93.0>              erlang:apply/2                         233     4006    0
{quote_handler,3}     prim_eval:'receive'/2                    9
<0.94.0>              erlang:apply/2                         233     4006    0
{quote_handler,4}     prim_eval:'receive'/2                    9
<0.95.0>              erlang:apply/2                         233     4006    0
{quote_handler,5}     prim_eval:'receive'/2                    9
Total                                                     642876  1156835    0
                                                             438
ok
```

The SSH and and SSL applications have been updated to label the processes they
create.

# New functionality in STDLIB

## New utility functions for set modules

The three sets modules in STDLIB —
[`sets`](https://www.erlang.org/doc/man/sets),
[`gb_sets`](https://www.erlang.org/doc/man/gb_sets), and
[`ordsets`](https://www.erlang.org/doc/man/ordsets) —
have new functions `is_equal/2`, `map/2`, and `filtermap/2`.

The `is_equal/2` function is useful when one needs to find out whether two
sets contain the same elements. Comparing with `==` or `=:=` is not always
reliable. For example:


```erlang
1> Seq = lists:seq(1, 20, 2).
[1,3,5,7,9,11,13,15,17,19]
2> gb_sets:from_list(Seq) == gb_sets:delete(10, gb_sets:from_list([10|Seq])).
false
3> gb_sets:is_equal(gb_sets:from_list(Seq), gb_sets:delete(10, gb_sets:from_list([10|Seq]))).
true
```

The `map/2` maps the element of a set, producing a new set:

```erlang
4> Seq = lists:seq(1, 20, 2).
[1,3,5,7,9,11,13,15,17,19]
#Fun<erl_eval.42.39164016>
5> ordsets:to_list(ordsets:map(fun(N) -> N div 4 end, ordsets:from_list(Seq))).
[0,1,2,3,4]
```

The `filtermap/2` function can map and filter at the same time. Here is an example
showing how to multiply each integer in a set by 100 and remove non-integers:

```erlang
1> Mixed = [1,2,3,a,b,c].
[1,2,3,a,b,c]
2> F = fun(N) when is_integer(N) -> {true,N * 100};
   (_) -> false
   end.
#Fun<erl_eval.42.39164016>
3> sets:to_list(sets:filtermap(F, sets:from_list(Mixed))).
[300,200,100]
```

## New `timer` convenience functions that take funs

In Erlang/OTP 26, the functions in the
[`timer`](https://www.erlang.org/doc/man/timer) module don't accept funs.
It is certainly possibly to pass in a fun in the argument for
[`erlang:apply/2`](https://www.erlang.org/doc/man/erlang#apply/2),
but if one makes a mistake it will be only be noticed when the
timer expires:

```erlang
1> timer:apply_after(10, erlang, apply, [fun() -> io:put_chars("now!\n") end]).
{ok,{once,#Ref<0.2380540714.1485570051.86513>}}
=ERROR REPORT==== 10-Apr-2024::05:56:43.894073 ===
Error in process <0.109.0> with exit value:
{undef,[{erlang,apply,[#Fun<erl_eval.43.105768164>],[]}]}
```

Here the empty argument list for the fun was forgotten. It should have been:

```erlang
2> timer:apply_after(10, erlang, apply, [fun() -> io:put_chars("now!\n") end, []]).
{ok,{once,#Ref<0.2380540714.1485570051.86522>}}
now!
```

In Erlang/OTP 27, using a fun is much easier:

```erlang
1> timer:apply_after(10, fun() -> io:put_chars("now!\n") end).
{ok,{once,#Ref<0.3845681669.1215561736.51634>}}
now!
```

In systems that use hot code updating, using a local fun for a long-running
timer is not ideal. The code that defines the fun could have been replaced,
and when the timer finally expires the call will fail. Therefore, it is also
possible to pass a fun as well as its arguments, making it possible to use
use a remote fun that will survive hot code updating:

```erlang
2> timer:apply_after(10, fun io:put_chars/1, ["now\n"]).
{ok,{once,#Ref<0.3845681669.1215561736.51650>}}
now
```

The `apply_interval/*` and `apply_repeatedly/*` functions now also accept
funs.

## New `ets` functions

The new functions
[`ets:first_lookup/1`](https://www.erlang.org/doc/man/ets#first_lookup/1)
and
[`ets:next_lookup/2`](https://www.erlang.org/doc/man/ets#next_lookup/2)
simplifies and speeds up traversing an ETS table:

```erlang
1> T = ets:new(example, [ordered_set]).
#Ref<0.1968915180.2077884419.247786>
2> ets:insert(T, [{I,I*I} || I <- lists:seq(1, 10)]).
true
3> {K1,_} = ets:first_lookup(T).
{1,[{1,1}]}
4> {K2,_} = ets:next_lookup(T, K1).
{2,[{2,4}]}
5> {K3,_} = ets:next_lookup(T, K2).
{3,[{3,9}]}
6> {K4,_} = ets:next_lookup(T, K3).
{4,[{4,16}]}
```

Similarly,
[`ets:last_lookup/1`](https://www.erlang.org/doc/man/ets#last_lookup/1)
and
[`ets:prev_lookup/2`](https://www.erlang.org/doc/man/ets#prev_lookup/2)
can be used to traverse a table in reverse order.

The new function
[`ets:update_element/4`](https://www.erlang.org/doc/man/ets#update_element/4)
is similar to
[`ets:update_element/3`](https://www.erlang.org/doc/man/ets#update_element/3),
but makes it possible to supply a default object when there is no existing
object with the given key:

```erlang
1> T = ets:new(example, []).
#Ref<0.878413430.1983512583.205850>
2> ets:update_element(T, a, {2, true}, {a, true}).
true
3> ets:lookup(T, a).
[{a,true}]
```

# New SSL client-side stapling support

A new feature in the SSL client in Erlang/OTP 27 is support for [OCSP
stapling](https://en.wikipedia.org/wiki/OCSP_stapling) for easier and
faster verification of the revocation status of server
certificates.

With OCSP stapling, the SSL client can streamline the validation of
revocation status. Normally the client would have to query the
[CA (Certificate
Authority)](https://en.wikipedia.org/wiki/Certificate_authority) using
[OCSP (Online Certificate Status
Protocol)](https://en.wikipedia.org/wiki/Online_Certificate_Status_Protocol)
to ensure that the server's certificate has not been
[revoked](https://en.wikipedia.org/wiki/Certificate_revocation).

The basic idea behind OCSP stapling is that the server itself will
proactively query the CA regarding the revocation status for its own
certificate and "staple" the time-stamped OCSP response from the CA to
the certificate. When a client connects, the server passes along
its OCSP-stapled certificate to the client. To verify the revocation
status, the client only needs to check that the OCSP response was
signed by the CA.

Here follows an example showing how OCSP stapling can be enabled in the
SSL client:

```erlang
1> ssl:start().
ok
2> {ok, Socket} = ssl:connect("duckduckgo.com", 443,
                              [{cacerts, public_key:cacerts_get()},
                               {stapling, staple}]).
{ok,{sslsocket,{gen_tcp,#Port<0.5>,tls_connection,undefined},
               [<0.122.0>,<0.121.0>]}}
```

# `tprof`: Yet another profiling tool

In Erlang/OTP 27, the new profiling tool
[`tprof`](https://www.erlang.org/doc/man/tprof)
joins the existing profiling tools
[`cprof`](https://www.erlang.org/doc/man/cprof),
[`eprof`](https://www.erlang.org/doc/man/eprof),
and [`fprof`](https://www.erlang.org/doc/man/fprof).

Why introduce a new profiling tool?

One reason is that `cprof` and `eprof` perform similar profiling
tasks, but the naming of the API functions are different. It is quite
easy to mix up the names when running one tool after the other, and
running them after each other is not uncommon.  For example, when
trying to find a
[bottleneck](https://en.wikipedia.org/wiki/Bottleneck_(software)) in a
complex running Erlang system, one approcach is to first use
`cprof` to get a rough idea of the general part of the system where a
bottleneck could be located. After that, `eprof` is run on a limited
part of the system trying to narrow it down. Directly running `eprof`
on a large Erlang application could overload it and bring it down.

Using `tprof`, the same function is used for both counting calls and
measuring the time for each call. Here is how to count calls when
`lists:seq(1, 1000)` is called:

```
1> tprof:profile(lists, seq, [1, 1000], #{type => call_count}).
FUNCTION          CALLS  [    %]
lists:seq/2           1  [ 0.40]
lists:seq_loop/3    251  [99.60]
                         [100.0]
ok
```

Note that call counting is always done for all processes.

The bulk of the work for `lists:seq/2` is done in `lists:seq_loop/3`,
which was called 251 times. Since we asked for 1000 integers, we
reach the conclusion that each tail-recursive call to `seq_loop/3`
creates four list elements at once. That can be confirmed by
looking at the
[source code](https://github.com/erlang/otp/blob/ca50a5d73703f74e2eae1ca40bbe6c4f027f9f98/lib/stdlib/src/lists.erl#L409-L416).

To measure the time for each call, we only need to replace
`call_count` with `call_time`:

```
2> tprof:profile(lists, seq, [1, 1000], #{type => call_time}).

****** Process <0.94.0>  --  100.00% of total ***
FUNCTION          CALLS  TIME (μs)  PER CALL  [     %]
lists:seq/2           1          0      0.00  [  0.00]
lists:seq_loop/3    251         50      0.20  [100.00]
                                50            [ 100.0]
ok
```

Call time is only measured the process that called
[`tprof:profile/4`](https://erlang.org/doc/man/tprof#profile/4)
and any process spawned by that process.

By replacing `call_time` with `call_memory` the amount of memory consumed
by each call will be measured:

```
3> tprof:profile(lists, seq, [1, 1000], #{type => call_memory}).

****** Process <0.97.0>  --  100.00% of total ***
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3    251   2000      7.97  [100.00]
                          2000            [ 100.0]
ok
```

The total number of words created is 2000, which make sense since each
list element needs 2 words. The number of words consumed per call is
`2000 / 251`, which is approximately 7.97 or almost 8. That also makes
sense since each tail-recursive call creates 4 list elements, or 8
words, and there are 250 such calls. The remaining call creates the final
empty list (`[]`).

`call_memory` tracing was introduced in the runtime system in
Erlang/OTP 26, but was not exposed in any existing profiling tool
because it didn't really fit in any of them. It made more sense to enable
support for it in a new tool.

# Multiple trace sessions

Tracing makes it possible to observe, debug, analyse, and measure the
performance of a running Erlang system. Over the year, numerous tools
using tracing has been developed. In Erlang/OTP alone, several tools
leverage tracing for different purposes:

* [`dbg`](https://www.erlang.org/doc/man/dbg), [`ttb`](https://www.erlang.org/doc/man/ttb) -
  general tracing tools

* [`etop`](https://www.erlang.org/doc/man/etop) - similar to `top` in Unix

* [`eprof`](https://www.erlang.org/doc/man/eprof),
  [`cprof`](https://www.erlang.org/doc/man/cprof),
  [`fprof`](https://www.erlang.org/doc/man/fprof),
  [`tprof`](https://www.erlang.org/doc/man/tprof) - profiling tools

* [`et`](https://www.erlang.org/doc/apps/et) - event tracer

* [`debugger`](https://www.erlang.org/doc/man/debugger) - uses tracing
  internally when evaluating `receive` expressions

In Erlang/OTP 26 and earlier tracing had some limitations:

* There could only be a single tracer per traced process.

* The configuration for which processes and functions to trace were
  global within the runtime system.

Those limitations meant that different tracing tools could easily step
on each other's toes. The treacherous part was that using multiple tracing
tools at the same time would seem to work for a while... until it didn't.

In Erlang/OTP 27, multiple trace sessions can be created. Each trace
session has its own tracer process and configuration for which
processes and functions to trace.

To create a trace session and set up tracing, there is the new
[`trace`](https://www.erlang.org/doc/man/trace) module in the Kernel
application. Tools that set up tracing using that module will no longer
interfere with each other. Tools that use the
[old API](https://www.erlang.org/doc/man/erlang#trace/3)
will share a single global trace session.

In the initial Erlang/OTP 27 release, some of the tools using tracing
have been updated to use trace sessions. Other tools will be updated in
upcoming maintenance releases.

We have tried to design the new API in a way to make it relatively
easy for maintainers of external tools to migrate their code.  Apart
from the names of the functions and the first argument (the session
argument), the other arguments and their semantics are almost entirely
identical to the old API.


## Quick trace session example

Here is an example to show how the new API is used. First we'll need
a tracer process that prints all trace messages it receives:

```erlang
1> Tracer = spawn(fun F() -> receive M -> io:format("== ~p ==\n", [M]), F() end end).
<0.90.0>
```

Having a tracer process, we can create a trace session:

```erlang
2> Session = trace:session_create(my_session, Tracer, []).
{#Ref<0.179442114.3923902468.103849>,{my_session,0}}
```

Next we turn on call tracing on the current process:

```erlang
3> trace:process(Session, self(), true, [call]).
1
```

Make sure that module `array` is loaded and trace all calls in it:

```erlang
4> l(array).
{module,array}
5> trace:function(Session, {array,'_','_'}, [], [local]).
89
```

Next create a new array:

```erlang
6> array:new(10).
== {trace,<0.88.0>,call,{array,new,"\n"}} ==
{array,10,0,undefined,10}
== {trace,<0.88.0>,call,{array,new_0,[10,0,false]}} ==
== {trace,<0.88.0>,call,{array,new_1,["\n",0,false,undefined]}} ==
== {trace,<0.88.0>,call,{array,new_1,[[],10,true,undefined]}} ==
== {trace,<0.88.0>,call,{array,new,[10,true,undefined]}} ==
== {trace,<0.88.0>,call,{array,find_max,"\t\n"}} ==
```

Note that trace messages are randomly intermingled with the return value
of the call.

When we are done, we can destroy the session:

``` erlang
7> trace:session_destroy(Session).
```

If we don't destroy the session, it will be automatically destroyed when
the last reference to it goes away.

# Native coverage support

The [Cover](https://www.erlang.org/doc/man/cover) tool for determining
[code coverage](https://en.wikipedia.org/wiki/Code_coverage) has long been
part of Erlang/OTP.

Traditionally, Cover collected its coverage metrics without the
help of any specialized functionality in the runtime system. To count how
many times each line in a module was executed, Cover
[instrumented](https://en.wikipedia.org/wiki/Instrumentation_(computer_programming))
abstract code for the module by inserting calls to
[`ets:update_counter/3`](https://www.erlang.org/doc/man/ets#update_counter/3)
on each executable line.

That worked, but the cover-instrumented Erlang code would always run
slower. How much slower depended on the nature of the code being
tested.

In Erlang/OTP 27, runtime systems supporting the
[JIT (just-in-time compiler)](https://www.erlang.org/blog/a-first-look-at-the-jit/)
can now collect coverage metrics in the runtime system with minimal
performance overhead.

The Cover tool has been updated to automatically take advantage of
native coverage support if supported by the runtime system. When
running the test suites for most OTP applications, there is no
noticeable difference in execution time running with and without
Cover.

The native coverage support can also be used directly for performing
measurements that Cover cannot accomplish, such as collecting metrics
for code that is executed while the Erlang runtime system is starting.

Here is a quick example showing how we can collect coverage metrics
for `init`, which is the first module executed when starting up the
runtime system. First we need to instruct the runtime system to
instrument all functions in all modules with extra code to count the
number of times each function is called:

```
$ bin/erl +JPcover function_counters
```

The runtime system starts normally. We can now read out the counters
for the `init` module:

{% raw %}

```
1> lists:reverse(lists:keysort(2, code:get_coverage(function, init))).
[{{archive_extension,0},392},
 {{get_argument1,2},198},
 {{objfile_extension,0},101},
 {{boot_loop,2},64},
 {{request,1},55},
 {{to_strings,1},44},
 {{do_handle_msg,2},38},
 {{handle_msg,2},38},
 {{b2s,1},38},
 {{get_argument,2},33},
 {{get_argument,1},31},
 {{'-load_modules/2-lc$^0/1-0-',1},30},
 {{'-load_modules/2-lc$^1/1-2-',1},30},
 {{'-load_modules/2-lc$^2/1-3-',1},30},
 {{'-load_modules/2-lc$^3/1-4-',1},30},
 {{extract_var,2},30},
 {{'-prepare_loading_fun/0-fun-0-',3},29},
 {{eval_script,2},23},
 {{append,1},18},
 {{get_arguments,1},18},
 {{reverse,1},17},
 {{check,2},17},
 {{ensure_loaded,2},16},
 {{ensure_loaded,1},16},
 {{do_load_module,2},14},
 {{do_ensure_loaded,2},14},
 {{get_flag_args,...},12},
 {{...},...},
 {...}|...]
```

{% endraw %}

The returned list of counter values for each function is sorted in
descending order on the number of time each function was executed.

For more information, see
[Native Coverage Support](https://www.erlang.org/doc/man/code#module-native-coverage-support)
in the documentation for the `code` module.

# Deprecating archives

[Archives](https://www.erlang.org/doc/man/code#module-loading-of-code-from-archive-files)
is experimental functionality that has existed in Erlang/OTP for a
long time. Part of the support for archives is deprecated in Erlang/OTP 27.

The reason is that the performance of code loading from archives has
never been great. Even worse is that the very existence of the archive
functionality degrades the performance of code loading even when no
archives are used, and complicates or prevents optimizations aimed at
reducing startup time.

In Erlang/OTP 27, the following functionality is deprecated:

* Using archives for packaging a single application or parts of a single application
  into an archive file that is included in the code path. This functionality will
  likely be removed in Erlang/OTP 28.

* The [`code:lib_dir/2`](https://www.erlang.org/doc/man/code#lib_dir/2)
  function. This function was introduced to allow reading files
  inside archives. In Erlang/OTP 28, the function itself will not be
  removed, but it will most likely no longer support looking into
  archives.

* All functionality to handle archives in module
  [`erl_prim_loader`](https://www.erlang.org/doc/man/erl_prim_loader).
  That same functionality is likely to be removed in Erlang/OTP 28.

* The `-code_path_choice` flag for `erl`. In Erlang/OTP 27, the default
  has changed from `relaxed` to `strict`. This flag is likely to be removed
  in Erlang/OTP 28.

In order to use archives in Erlang/OTP 27, it is necessary to use the flag
`-code_path_choice relaxed`.

## Using a single archive in an Escript is **not** deprecated

An archive can still be used to hold all files needed by an
[Escript](https://www.erlang.org/doc/apps/erts/escript_cmd.html).
However, to access files in the archive (for example, to read templates or other
data files), the only supported way guaranteed to work in future
releases is to use the
[`escript:extract/2`](https://www.erlang.org/doc/man/escript#extract/2)
function.
