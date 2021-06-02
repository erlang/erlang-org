---
layout: post
title: Lost in Translation (Exploring the Compiler's Front End)
tags: compiler BEAM
author: Björn Gustavsson
---

In this blog post, we will explore the compiler passes that make up
the compiler's front end.

In the [previous blog post](http://blog.erlang.org/compiler-time-option/)
we showed how the `time` option shows information about the compiler passes
being executed:


```
$ erlc +time trivial.erl
Compiling "trivial"
 remove_file                   :      0.000 s       3.7 kB
 parse_module                  :      0.000 s       5.5 kB
 transform_module              :      0.000 s       5.5 kB
 lint_module                   :      0.002 s       5.5 kB
 expand_records                :      0.000 s       5.3 kB
     .
     .
     .
```

We explained what the `remove_file` pass does in the previous
blog post. In today's blog post, we will discuss the other passes
listed in the output above.

Those passes makes up the compiler's front end. The implementation
modules for those passes are not in the **compiler** application, but
in **STDLIB**. The reason is that the Erlang shell also uses those
modules. That means that the shell will work in an embedded system
that does not include the **compiler** application.

The front end passes operate on the **abstract format**. The abstract
format is fairly close to the original Erlang source code. In fact, by
pretty-printing the abstract format, we can reconstruct the original
source code, albeit not perfectly.

## Lost in translation ##

To see how much we will lose in translation, we will compile and
pretty-print this module:

```
-module(trivial).
-export([example/4]).
-record(rec, {mod,func,result}).

%% Example to help explore the compiler front end.
example(A, B, C, D) ->
    #rec{mod=?MODULE,func=?FUNCTION_NAME,result=A + (B*C*(D+42))}.
```

We use `-P` option to run the `parse_module` pass and produce
a listing of the result:

```
$ erlc -P +time trivial.erl
Compiling "trivial"
 parse_module                  :      0.000 s       5.5 kB
 transform_module              :      0.000 s       5.5 kB
 lint_module                   :      0.003 s       5.5 kB
 listing                       :      0.001 s       5.5 kB
```

For the moment, ignore the `transform_module` and `erl_lint` passes.
They don't change the abstract code for this module. The `listing`
pass pretty prints the abstract format, converting it back to Erlang
source code and creating the file `trivial.P`.


```
$ cat trivial.P
-file("trivial.erl", 1).

-module(trivial).

-export([example/4]).

-record(rec,{mod,func,result}).

example(A, B, C, D) ->
    #rec{mod = trivial,func = example,result = A + B * C * (D + 42)}.
```

Comparing the `trivial.P` file to the original, we can see what was
lost in translation:

* The `?MODULE` and `?FUNCTION_NAME` macro invocations have been
replaced with `trival` and `example`, respectively. That was done by
the preprocessor.

* The comment has disappeared. There are also several differences in the
amount of whitespace surrounding variables and operators. The abstract format
does not include whitepace or comments in its representation.

* Also note that a redundant pair of parentheses has been omitted in the
expression `A + (B*C*(D+42))`. The parentheses around `D+42` are still there
because otherwise the value of the expression would change. The abstract
format has no direct representation of parenheses.

## Looking closer at the parse_module pass ##

Now that we have seen what is lost in translation, we will take a
closer look at the abstract format.

We will use the expression `A+(B*C*(D+42))` as an example and
translate it to the abstract format using the same modules that the
`parse_module` pass uses to do its work.

### Tokenizing using erl_scan ###

The first step in the translation from Erlang source code is to group
the characters into logical groups called **tokens**. This process is
called **tokenization** or **scanning**, and is done by the `erl_scan`
module.

We will use `erl_scan:string/1` to tokenize our example. (The
compiler will use other functions in `erl_scan`, but the principle
is the same.)

```
1> {ok,Tokens,_} = erl_scan:string("A + (B*C*(D+42))."), Tokens.
[{var,1,'A'},
 {'+',1},
 {'(',1},
 {var,1,'B'},
 {'*',1},
 {var,1,'C'},
 {'*',1},
 {'(',1},
 {var,1,'D'},
 {'+',1},
 {integer,1,42},
 {')',1},
 {')',1},
 {dot,1}]
```

The output is a list of tokens. The second element in each tuple
is the line number. The first element is the category of the token.
If there is a third element, it is the symbol within that category.

We can see that whitespace has already been lost. Had there been
a comment, it would have been lost too.

To read more details about tokens, see [erl_scan:string/1].

[erl_scan:string/1]: http://erlang.org/doc/man/erl_scan.html#string-1

### Preprocessing the tokens ###

In the compiler, the next step would be to run the preprocessor
on the tokens. In this example, there are no macro invocations
and thus nothing to preprocess, so we will skip to the next step.

### Parsing using erl_parse ###

The next step is to **parse** the tokens to produce the abstract
format:

```
2> {ok,Abstract} = erl_parse:parse_exprs(Tokens), Abstract.
[{op,1,'+',
     {var,1,'A'},
     {op,1,'*',
         {op,1,'*',{var,1,'B'},{var,1,'C'}},
         {op,1,'+',{var,1,'D'},{integer,1,42}}}}]
```

The result is a list with one expression. The expression is not a
list, but a **parse tree**. It can be visualized like this:

![Abstract format visualized](../images/compiler-2018-04-26.svg)

The parentheses have been lost, because the structure of the tree
makes the evaluation order unambiguous.

See [The Abstract Format] for more details about the abstract format.

[The Abstract Format]: http://erlang.org/doc/apps/erts/absform.html
[parse tree]: ../images/compiler-2018-04-26.svg

### Pretty-printing using erl_pp ###

The `listing` pass uses the [erl_pp] module to pretty print the
abstract format to produce a listing file.

We can pretty print the abstract format of the example:

```
3> lists:flatten(erl_pp:exprs(Abstract)).
"A + B * C * (D + 42)"
```

Here the pretty printer has inserted one pair of parentheses, but the
redundant pair of parentheses in the original expression has been lost.
The whitespace is also different from the original.

[erl_pp]: http://erlang.org/doc/man/erl_pp.html

### A quick look at the preprocessor ###

A mentioned in passing, the preprocessor (the [epp] module) is run
after tokenization and before parsing.

The preprocessor goes through the tokens, looking for a question
mark followed by a variable or atom. For example, `?MODULE` in
a source file would be tokenized like this by `erl_scan`:

    [{'?',1},{var,1,'MODULE'}]

Assuming that the module name is `trivial`, the preprocessor will
replace those tokens with the token:

    [{atom,1,trivial}]

[epp]: http://erlang.org/doc/man/epp.html


## The other passes operating on the abstract format ##

Now that `parse_module` has been explained, let's take quick look at the
other passes in the front end.

### The transform_module pass ###

The `transform_module` pass runs parse transforms, for example
for [QLC] or [ms_transform].

[QLC]: http://erlang.org/doc/man/qlc.html
[ms_transform]: http://erlang.org/doc/man/ms_transform.html

### The lint_module pass ###

The `lint_module` pass verifies that the code is semantically
correct. That is, variables must be bound before they are used,
all clauses for a function must have the same number of arguments,
and so on.

When we compile a module with problems, [erl_lint] will print
error messages and terminate the compilation:

```
$ cat bug.erl
-module(bug).
-export([main/0]).

main() ->
    A+B.
$ erlc +time bug.erl
Compiling "bug"
 remove_file                   :      0.000 s       2.1 kB
 parse_module                  :      0.000 s       2.7 kB
 transform_module              :      0.000 s       2.7 kB
 lint_module                   :      0.004 s       2.4 kB
bug.erl:5: variable 'A' is unbound
bug.erl:5: variable 'B' is unbound
$
```

[erl_lint]: http://erlang.org/doc/man/erl_lint.html

### Translating records ###

The `expand_records` pass uses [erl_expand_records] to translate
records:

```
$ erlc -E +time trivial.erl
Compiling "trivial"
 parse_module                  :      0.000 s       5.5 kB
 transform_module              :      0.000 s       5.5 kB
 lint_module                   :      0.002 s       5.5 kB
 expand_records                :      0.000 s       5.3 kB
 listing                       :      0.001 s       5.3 kB
$ cat trivial.E
-file("trivial.erl", 1).

-module(trivial).

-export([example/4]).

-record(rec,{mod,func,result}).

example(A, B, C, D) ->
    {rec,trivial,example,A + B * C * (D + 42)}.
```

The `-E` option produces a listing of the abstract format
produced by the `expand_records` pass.

The `-record()` declaration is still there, but the construction of
the record has been replaced with construction of a tuple. Similarly,
matching of records will be translated to matching of tuples.

[erl_expand_records]: http://erlang.org/doc/man/erl_expand_records.html

## Tip: Producing a single source file using -P ##

The `-P` option can be used to package a source file that includes
multiple include files into a single self-contained source file.

Having a self-contained source file is useful if you want to report
a compiler bug, but don't have the time to minimize the source code
to a minimum example.

Here is an example. The `compile.erl` file includes two header files.
Compiling it directly like this will not work:

```
$ cd lib/compiler/src
$ erlc compile.erl
compile.erl:36: can't find include file "erl_compile.hrl"
   .
   .
   .
$
```

We must give the path to the `include` directories of both
Kernel and STDLIB:

```
$ erlc -I ../../kernel/include -I ../../stdlib/include compile.erl
$
```

To package the source from `compile.erl` as well as the contents
of the header files, use the `-P` option to generate `compile.P`:


```
$ erlc -P -I ../../kernel/include -I ../../stdlib/include compile.erl
```

`compile.P` can be renamed to `compile.erl` and successfully
compiled without any additional options:

```
$ mv compile.P $HOME/compile.erl
$ cd $HOME
$ erlc compile.erl
$
```

## Points to Ponder ##

The preprocessor is run after tokenization, before running the
parser.

So how are the `?FUNCTION_NAME` and `?FUNCTION_ARITY` macros implemented?

Here is an example of how tokens for a simple function looks like:

```
1> {ok,T,_} = erl_scan:string("foo({tag,X,Y}) -> ?FUNCTION_ARITY."), T.
[{atom,1,foo},
 {'(',1},
 {'{',1},
 {atom,1,tag},
 {',',1},
 {var,1,'X'},
 {',',1},
 {var,1,'Y'},
 {'}',1},
 {')',1},
 {'->',1},
 {'?',1},
 {var,1,'FUNCTION_ARITY'},
 {dot,1}]
```
