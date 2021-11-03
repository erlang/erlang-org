---
layout: post
title: A Brief History of the BEAM Compiler
tags: compiler BEAM
author: Björn Gustavsson
---

This blog post is a brief history lesson about the Erlang compiler for
the BEAM machine. To provide some context, there will first be a quick
look at the abstract machines for Erlang.

## A brief overview of the early Erlang implementations

### The Prolog interpreter

The first version of Erlang was implemented in
Prolog in 1986. That version of Erlang was used
to find out which features of the languages were
useful and which were not. New languages features
could be added or deleted in a matter of hours
or days.

### JAM (Joe's Abstract Machine)

It soon became clear that Erlang needed to be at
least 40 times faster to be useful in real projects.

In 1989 JAM (Joe's Abstract Machine) was first
implemented. [Mike Williams][mike] wrote the runtime system
in C, [Joe Armstrong][joe] wrote the compiler, and
[Robert Virding][robert] wrote the libraries.

[mike]: http://www.erlang-factory.com/conference/ErlangUserConference2013/speakers/MikeWilliams
[joe]: https://github.com/joearms
[robert]: https://github.com/rvirding

JAM turned out be 70 times faster than the Prolog
interpreter. Success?

### TEAM (Turbo Erlang Abstract Machine)

It soon became clear that Erlang still needed
to be faster to be useful in real projects.

Therefore Bogumil ("Bogdan") Hausman created TEAM (Turbo Erlang
Abstract Machine). It compiled the Erlang code to C code, which was
then compiled to native code using GCC.

It was significantly faster than JAM for small projects.
Unfortunately, compilation was very slow, and the code size of the
compiled code was too big to make it useful for large projects.

### BEAM (Bogdan's Erlang Abstract Machine)

Bogumil Hausman next machine was called BEAM
(Bogdan's Erlang Abstract Machine). It was a hybrid machine
that could execute both native code and [threaded code] with
an [interpreter]. That allowed customers to compile their
time-critial modules to native code and all other modules to
threaded BEAM code. The threaded BEAM in itself was faster
than JAM code.

[threaded code]: https://en.wikipedia.org/wiki/Threaded_code
[interpreter]: https://en.wikipedia.org/wiki/Interpreter_(computing)

Bogdan's original compiler for BEAM shared the compiler front end with
JAM. Essentially, the front end at that time did the same thing as the
front end in the current compiler as described in [Lost in Translation
(Exploring the Compiler's Front End)][front end].

I don't have the source code for Bodgan's original compiler,
but as far as I can determine it had three compiler passes that
translated the abstract format to threaded BEAM code.

* `beam_compile` - Translated the abstract format to BEAM instructions.

* `beam_optimize` - Optimized the BEAM instructions. This pass was mandatory,
since it did some necessary transformations of the BEAM instructions.

* `beam_asm` - Converted the symbolic BEAM assembly format to a binary
BEAM module.

[front end]: http://blog.erlang.org/compiler-lost-in-translation

### VEE (Virding's Erlang Engine)

Here we must mention VEE (Virding's Erlang Engine) for reasons that
will soon become clear.

VEE was an experimental implementation with a different memory model
compared to JAM and BEAM. Instead of JAM's and BEAM's separate heaps
for each process, VEE used a single shared heap with a real-time
garbage collector.  That made message passing blindlingly fast
compared to JAM and BEAM.

Overall, though, there was no speed gain compared to JAM. The reason
was probably that the single shared heap decreased the cache hit
rate.

## The maturation of BEAM

The OTP group and Erlang/OTP was created to industrialize Erlang and
make it suitable for huge real-world projects. The first release, OTP
R1B, was released in 1996.

This is the point where the history lesson may become a little bit
more subjective.

I joined the Erlang/OTP team at the end of 1996. My first small
code contributions to Erlang/OTP were included in OTP R1D.

I worked in the ERTS (Erlang Run-Time System) team, which at that time
was lead by Kenneth Lundin. Initially I worked with the Erlang runtime
system for Microsoft Windows. After some time (maybe a year or so),
Kenneth asked me to help stabilizing and improving BEAM. Gradually
BEAM become my main responsibility, and when Bogdan left Ericsson, I
become the main developer responsible for the BEAM [interpreter] and
compiler.

This blog post desperately tries to cover the history of the BEAM
*compiler*, but I think that some more historical context is needed
before we can approach the compiler.

The overall goal of the work on BEAM from OTP R1 up to OTP R5
was to make it stable enough and fast enough to be useful in real
projects.

There were two major obstacles to reaching that goal:

* BEAM/C, that is, native code via C code.
* The huge number of ever-changing BEAM instructions.

### BEAM/C must die!

It soon became obvious that BEAM/C, the compiler passes that
compiled Erlang code to C code, had to die. At the time that
I started working on BEAM, there were three distinct flavors of
BEAM/C: one for GCC on Sparc, one for GCC on non-sparc CPUs (such
as Intel x86), and one for other C compilers that did not support
GCC's extension for taking the address of a label. Bugs not only showed
up in the native code, but the mere existence of BEAM/C complicated and
caused bugs in the threaded BEAM interpreter.

Unfortunately, early in my career of improving BEAM, I made some
optimizations of the size of the C code generated by BEAM/C. That came
back to bite me later when I suggested that we should remove
BEAM/C. The size improvements made it possible to fit more Erlang code
compiled to native code into the system, and the native code was
faster than threaded BEAM code. Our customer at the time ([the AXD 301
project][axd301]) needed the extra speed improvements that BEAM/C gave
them and did not allow us to remove BEAM/C unless we could improve the
performance of threaded BEAM code to similar or better than BEAM/C
performance.

[axd301]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.140.9122&rep=rep1&type=pdf

### The ever-changing BEAM instructions

At that time, the BEAM interpreter had over [300
instructions][beam1997].  While JAM had a very simple loader that
essentially only loaded the JAM files into memory, the loader for BEAM
had to translate every instruction from the byte format in the BEAM
files to the threaded code format in memory.  The BEAM had
hand-written code for the loading of every single instruction.

To make it worse, the instruction set was constantly evolving. Bug
fixes and performance improvements needed new instructions, and those
instructions had to be implemented in the compiler, threaded code
interpreter (the `process_main()` function in `beam_emu.c`), and
the loader. In every minor and major release of Erlang/OTP, the
users of BEAM had to recompile all of their Erlang code
because the instruction set had changed.

There must be a better way, I thought. I started to write a simple
Perl script to a least automate the mapping from instruction name to
instruction number in the compiler, interpreter, and loader.
[Tony Rogvall][tonyrog] suggested that I could be more ambitious and
generate most of the code for for the loader using the Perl script.
He also suggested that operands for many instructions could be packed
into a single word. That would reduce load code size and also improve
the cache hit rate, improving execution speed.

So I started writing the first version of the [beam_makeops script][makeops]
and rewriting the loader. I prefer to work incrementally, making minor changes
to a code base that is always working. But I could not rewrite the loader
incrementally, so I hacked away frantically for two or three days until
I had a bare bones version of the new loader working. I could then relax
a little and somewhat more slowly add more features to `beam_makeops` and
the loader.

The new loader took over some tasks formerly done by the compiler.

For example, the BEAM machine has several specialized `move`
instructions.  There is one instruction for moving something into an X
register, another for moving an atom into an X register, and so
on. Before the new loader, the compiler knew about all those variants
of `move` instructions and selected the appropriate one. With the new
loader, there is only one `move` instruction that the compiler needs
to care about, and the loader will select the appropriate specialized
`move` instruction to use at load time.

Another minor optimization done by the compiler was combining of
common instructions sequences. For example, a `move` instruction
followed by a `call` instruction would be combined to a `move_call`
instruction. That optimization was also moved to the loader.

All those capabilities made it possible to significantly simplify and
reduce the number of instructions known to the compiler. More
importantly, that made it possible to keep the instruction set stable
(while still allowing minor optimizations and performance tuning by
tweaking only the loader and interpreter), avoiding the need to
recompile all Erlang code every time there was a new release.

If my memory doesn't fail me, the new loader was introduced in OTP R4.

[beam1997]: http://www.cs-lab.org/historical_beam_instruction_set.html
[tonyrog]: https://github.com/tonyrog
[makeops]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/internal_doc/beam_makeops.md

## OTP R5B: The "new" BEAM

Moving forward to OTP R5.

OTP R5 was the last release that supported JAM.

OTP R5 can also be said to be first release that featured the "new"
BEAM. In that release, the [modern BEAM file format][beam file format]
was introduced. The same file format is used today. At that time,
there were 78 BEAM instructions; in OTP 20, there are 159 instructions
(actually, 129 active instructions and 30 obsoleted instructions no
longer used). While new instructions have been introduced when needed
and obsolete instructions have been removed, it has always been
possible to load BEAM files compiled from at least two major releases
back.

Execution of threaded BEAM had become fast enough, so that BEAM/C
could be dropped (already in R4, I think). But strangely enough,
the customers still wanted more speed.

The BEAM compiler in R5 was still Bogdan's original compiler. While
it did more optimizations than the JAM ever did, we knew that more
optimizations were possible.

[beam file format]: http://www.erlang.se/~bjorn/beam_file_format.html

## R6B: Enter Kernel Erlang

Meanwhile, on the top floor Robert Virding was busy writing a
new compiler for his VEE machine. In that new compiler, Robert
introduced a new intermediate format that he called *Kernel Erlang*.
The idea was that more optimizations could be applied to the code
in that format before generating code for the actual machine.

At that time, there was no actual interpreter that could execute the
code emitted by his new compiler (he had not updated the VEE machine
yet). The machine he had in mind was a register machine. It was similar
to BEAM, except that it did stack trimming.

We wanted the better performance that we could get from Robert's compiler,
but the question was: should we implement a new interpreter (or adapt
BEAM) to execute the code from Robert's compiler, or should we adapt
Robert's compiler to generate BEAM code?

Because we now for the first time had a stable implementation of BEAM,
we decided not to rock the boat again; thus, we decided that I should
adapt the code generator part of Robert's compiler for BEAM.

For the most part, I used Robert's name for instructions. For example,
the instruction to load a term into a register was called `M` in the
original BEAM, while Robert's compiler used the `move`. The more major
changes was in the handling of the stack.  Robert's compiler had stack
trimming, which I had to remove and rewrite to handle BEAM's fixed
stack frame. (I reintroduced a limited form of stack trimming later.)

Since JAM was not supported in OTP R6, all customers that had previously
used JAM had to migrate to BEAM. To minimize the risk of the migration
as much as possible, one of our customers requested that we made the
battle-tested original BEAM compiler available as an option in OTP R6.

Therefore, we added options to choose which version of the compiler
to use. To use the old compiler, one would write:

```
$ erlc +v1 some_module.erl
```

Default was Robert's new compiler, which was called `v2`. There
was also an undocumented, unofficial compiler version called `v3`.

All compilers shared the front end and the `beam_asm` pass that
created the final BEAM module.

### The v1_compiler

The `v1` compiler had the following passes:

* v1_adapt
* v1_compile
* v1_optimize
* v1_cleanup

The `v1_compile` and `v1_optimize` passes were essentially
the `beam_compile` and `beam_optimize` passes from Bogdan's
compiler.

There had been some changes to the front end since R5, so
the `v1_adapt` pass was there to hide those changes for the
`v1_compile` and `v1_optimize` passes. The `v1_cleanup` pass was
an additional minor optimization pass; I think it was present
in OTP R5 as well.

### The v2_compiler

The `v2` compiler was Robert's new compiler. It had the following
passes:

* v2_kernel
* v2_kernopt
* v2_match
* v2_life
* v2_codegen

The `v2_kernel` pass translated the abstract format to Kernel Erlang.

`v2_kernopt` did very basic optimizations of the Kernel Erlang code,
essentially only [constant propagation and constant folding][folding].

`v2_match` did pattern matching compilation. JAM would match clauses
in function heads or `case` expressions sequentially. The old BEAM
compiler would do only a little bit better in that it could match
multiple integers or atoms in a single instruction. Robert's compiler
was the first Erlang compiler to properly compile pattern matching using
the algorithm described in
[The Implementation of Functional Programming Languages][peytonjones]
by Simon Peyton Jones.

`v2_life` would calculate life-time information needed by the
`v2_codegen` pass, and `v2_codegen` would generate the BEAM
assembly code.

[folding]: https://en.wikipedia.org/wiki/Constant_folding
[peytonjones]: https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/

## R7B: Enter Core Erlang

Meanwhile, [Richard Carlsson][carlsson] and the [HiPE group][hipe]
at Uppsala University come up with the idea for a new intermediate
format useful as an interchange format for different Erlang
implementations and for optimizing Erlang programs.

The new format was called [Core Erlang][core]. Robert liked the idea
and started to implement Core Erlang in the compiler.  The undocumented
implementation of `v3` compiler in OTP R6 is based on a draft version
of the Core Erlang specification.

In OTP R7B, the v1 and v2 compilers were removed, and the only
remaining compiler was the `v3` compiler that used Core Erlang.
It had the following passes:

* v3_core
* v3_core_opt
* v3_kernel
* v3_life
* v3_codegen

The `v3_core` pass translated the abstract format to Core Erlang.

The `v3_core_opt` pass essentially only called `sys_core_fold`, which
did [constant propagation and constant folding][folding]. `sys_core_fold`
still do those things, and [more][sys_core_fold].

The remaining passes do the same thing as today.

The `v3_kernel` pass translates from Core Erlang to Kernel Erlang,
and also does pattern matching compilation (in the same way as in
`v2_match`). The optimizations in `v2_kernopt` are now done in
`sys_core_fold`.

The `v3_life` pass (despite its name) no longer calculates life-time
information. The life-time information is instead calculated by
`v3_kernel` and passed on as annotations.

The reason that `v3_life` still exists is that Robert had continued
to work on his own version of `codegen` that did not have all
my changes in it to work for BEAM. While implementing the Core Erlang
passes, he also did many improvements to `codegen`.

When it was time to integrate our different versions of the compiler,
Robert looked in horror at all my changes in `codegen`. To avoid
having to reintroduce all my adapations and optimizations for BEAM
into his new version of `codegen`, Robert wrote an adapter pass
that translated from the new Kernel Erlang format to the old format
so that my `codegen` would work. The adapter pass is called
`v3_life`.

Thus, `v3_codegen` is essentially `v2_codegen` with a new name.

In the upcoming OTP 21, `v3_life` has been combined with `v3_codegen`.

[hipe]: https://www.it.uu.se/research/group/hipe/
[carlsson]: https://github.com/richcarl
[core]: https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
[sys_core_fold]: http://blog.erlang.org/core-erlang-optimizations

## Learning Erlang from Robert

In the time period that Robert and I worked together on the compiler,
I usually worked on `v3_codegen` and the passes below, while Robert
worked on all passes above `v3_codegen`.

Occasionally, I would add some optimizations to `sys_core_fold` and
give them to Robert to incorporate into his latest version of
`sys_core_fold`.

I would then look at what Robert had done with my code, and learn.

Usually Robert had subtly improved my code, made it slightly
cleaner and simpler. But one time I handed Robert an
optimization of `case` clauses. The code I got back was very different.
Robert had broken apart my optimization into several simpler
optimizations that achieved the same purpose (and more) than my
more complicated optimization.
