---
layout: post
title: The Road to the JIT
tags: BEAM JIT HiPE
author: Björn Gustavsson
---

As long as Erlang has existed, there has always been the need and the
ambition to make it faster. This blog post is a history lesson that
outlines the major Erlang implementations and attempts to improve
the performance of Erlang.

### The Prolog interpreter

The first version of Erlang was implemented in Prolog in 1986. That
version of Erlang was too slow for creating real applications, but it
was useful for finding out which features of the language were useful
and which were not. New language features could be added or deleted
in a matter of hours or days.

It soon became clear that Erlang needed to be at least 40 times faster
to be useful in real projects.

### JAM (Joe's Abstract Machine)

In 1989 JAM (Joe's Abstract Machine) was first
implemented. [Mike Williams][mike] wrote the runtime system
in C, [Joe Armstrong][joe] wrote the compiler, and
[Robert Virding][robert] wrote the libraries.

[mike]: http://www.erlang-factory.com/conference/ErlangUserConference2013/speakers/MikeWilliams
[joe]: https://github.com/joearms
[robert]: https://github.com/rvirding

JAM was 70 times faster than the Prolog interpreter, but it turned out
that this still wasn't fast enough.

### TEAM (Turbo Erlang Abstract Machine)

Bogumil ("Bogdan") Hausman created TEAM (Turbo Erlang Abstract
Machine). It compiled the Erlang code to C code, which was then
compiled to native code using GCC.

It was significantly faster than JAM for small projects.
Unfortunately, compilation was very slow, and the code size of the
compiled code was too big to make it useful for large projects.

### BEAM (Bogdan's Erlang Abstract Machine)

Bogumil Hausman's next machine was called BEAM (Bogdan's Erlang Abstract
Machine). It was a hybrid machine that could execute both native code
(translated via C) and [threaded code] with an [interpreter]. That
allowed customers to compile their time-critical modules to native code
and all other modules to threaded BEAM code. The threaded BEAM in
itself was faster than JAM code.

[threaded code]: https://en.wikipedia.org/wiki/Threaded_code
[interpreter]: https://en.wikipedia.org/wiki/Interpreter_(computing)

### Lessons Learned from BEAM/C

The modern BEAM only has the interpreter. The ability of BEAM to generate
C code was dropped in OTP R4. Why?

C is not a suitable target language for an Erlang compiler. The main
reason is that an Erlang function can't simply be translated to a C
function because of Erlang's process model. Each Erlang process must
have its own stack and that stack cannot be automatically managed by
the C compiler.

BEAM/C generated a single C function for each Erlang module. Local
calls within the module were made by explicitly pushing the return
address to the Erlang stack followed by a `goto` to the label of the
called function.  (Strictly speaking, the calling function stores the
return address to BEAM register and the called function pushes that
register to the stack.)

Calls to other modules were done similarly by using the GCC extension
that makes it possible to [take the address of a label][gcc_labels]
and later jumping to it.  Thus an external call was made by pushing
the return address to the stack followed by a `goto` to the address of
a label in another C function.

[gcc_labels]: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html

Isn't that undefined behavior?

Yes, it is undefined behavior even in GCC. It happened to work with
GCC on Sparc, but not on GCC for X86. A further complication was the
embedded systems having ANSI-C compilers without any GCC extensions.

Because of that, we had to maintain three distinct flavors of BEAM/C
to handle different C compilers and platforms. I don't remember any
benchmarks from that time, but it is unlikely that BEAM/C was faster
than interpreted BEAM on any other platform than Solaris on Sparc.

In the end, we removed BEAM/C and optimized the interpreted BEAM so that
it could beat BEAM/C in speed.

### HiPE

[HiPE][hipe] (The High-Performance Erlang Project) was a research
project at Uppsala University running for many years starting
around 1996. It was "aimed at efficiently implementing concurrent programming
systems using message-passing in general and the concurrent functional
language Erlang in particular".

[hipe]: https://www.it.uu.se/research/group/hipe/

One of the many outcomes of the project was the HiPE native code
compiler for Erlang. HiPE became a part of the OTP distribution in OTP
R8 in 2001.  The HiPE native compiler is written in Erlang and
translates the BEAM code to native code without the help of a C
compiler, therefore avoiding many of the problems that BEAM/C ran into.

The HiPE native compiler can often speed up sequential code by a
factor of two or three compared to interpreted BEAM code.  We hoped
that would speed up real-world huge application systems.
Unfortunately, projects within Ericsson that tried HiPE found that it
did not improve performance.

Why is that?

The main reason is probably that most huge Erlang applications
don't contain enough sequential code that HiPE could optimize.  The
runtime of those systems is typically dominated by some combination of
message passing, calls to the [ETS BIFs][ets], and garbage collection, none
of which HiPE can optimize.

[ets]: https://erlang.org/doc/man/ets.html

Another reason could be that big systems typically have many small
modules.  The HiPE native compiler (in common with the Erlang
compiler) cannot optimize code across module boundaries, thus being
unable to do much type-based optimizations.

Also, for most big systems, compiling all Erlang modules to native
code would lead to impractically long build times and the resulting
code would consume too much memory.  There is a small overhead of
switching from native code to interpreted BEAM and vice versa.  It is
a non-trivial task to figure out which modules that would gain from
being compiled to native code, and at the same time avoiding an
excessive amount of context switches between native and interpreted
code.

Because none of the Ericsson Erlang projects used the HiPE native
compiler, the OTP team could only afford to spend a limited amount of
time maintaining HiPE. Therefore, the documentation for HiPE includes
this note:

> HiPE and execution of HiPE compiled code only have limited support
  by the OTP team at Ericsson. The OTP team only does limited
  maintenance of HiPE and does not actively develop HiPE. HiPE is
  mainly supported by the HiPE team at Uppsala University.

### Other Outcomes from the HiPE Project

I think it is fair to say that Erlang/OTP would look very different
today if it hasn't been for the HiPE project.  Here are the major
contributions from the HiPE project to OTP:

* A new [staged tag scheme][newtags] in OTP R7.  The new tag scheme
  allowed the Erlang system to address the full 4GB address space (the
  previous tag scheme only supported addressing the lower 1 GB).
  Surprisingly, the new tag scheme also improved performance.

* The [Core Erlang][core] intermediate representation is used in the
  Erlang compiler to this day.  For more information, see
  [An introduction to Core Erlang][core_introduction] and
  [Core Erlang by Example][core_by_example].

* [Dialyzer][dialyzer] (DIscrepancy AnaLYZer for ERlang programs),
  started out as a type analysis pass for the HiPE native compiler,
  but soon become a tool for Erlang programmers to help find bugs and
  unreachable code in their applications.

* [Bit strings and binary comprehensions][bitstrings].

* Introducing [`try`...`catch`][exception] in OTP R10.

* Implementing per function counters and the [cprof][cprof]
  module. The counters were originally meant to be used for finding hot
  functions and generating native code only for these.  But the overhead
  in the context switch between interpreted and native code made this
  usage less useful.

* Repeatedly suggesting that Erlang needed a [literal pool][pool] for premade
  literal terms (instead of constructing them each time they are
  used).  At one of our meetings between the HiPE team and the OTP
  team, I remember [Richard Carlsson][richcarl] pointing out to me that it would
  be nice for [Wings3D][wings3d] to have floating-point literals.  The
  OTP team implemented literal pools in OTP R12.

[dialyzer]: https://erlang.org/doc/apps/dialyzer/index.html
[newtags]: http://www.it.uu.se/research/publications/reports/2000-029/2000-029-nc.pdf
[bitstrings]: http://user.it.uu.se/%7Epergu/papers/erlang05.pdf
[core]: https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
[core_introduction]: http://www.erlang.se/workshop/carlsson.ps
[core_by_example]: http://blog.erlang.org/core-erlang-by-example
[exception]: https://erlang.org/workshop/2004/exception.pdf
[richcarl]: https://github.com/richcarl
[wings3d]: http://www.wings3d.com
[pool]: https://en.wikipedia.org/wiki/Literal_pool
[cprof]: https://erlang.org/doc/man/cprof.html

### The Tracing JIT projects (BEAMJIT)

There have been three separate research projects that tried to develop
a tracing JIT for Erlang.  All of them have been led by Frej Drejhammar
of [RISE (formerly SICS)][sics].

[sics]: https://en.wikipedia.org/wiki/Swedish_Institute_of_Computer_Science

A tracing JIT ([Just In Time compiler][jit]) is a JIT that runs in two phases:

[jit]: https://en.wikipedia.org/wiki/Just-in-time_compilation

* First it traces execution to find sequences of hot (frequently executed)
  code.

* It then rewrites the found traces to native code.

The goals for the three JIT projects were:

* The JIT should work automatically with no need for the user to identify
  which modules to compile to native code beforehand.

* There should be total feature compatibility with the non-JIT
  BEAM. In particular, tracing, scheduling behavior, save calls, and
  hot code reloading should continue to work, and stack traces should
  be identical to the ones in the non-JIT BEAM.

* The system should at least on average never be slower than the non-JIT
  BEAM.

There were some promising results when running some benchmarks, but ultimately
it turned out to be impossible to fulfill the goal to never be slower than the
non-JIT system.  Here are the main reasons for the slowdowns:

* To do the tracing (finding hot code), the BEAM interpreter needed
  tweaking.  It was difficult to be able to do tracing without lowering the
  base speed of the BEAM interpreter.

* It was also difficult to design the mechanism for context
  switching between the interpreted code and native code in a way that
  didn't lower the base speed of the BEAM interpreter.

* When a hot sequence of code has been found, the code needed to be compiled
  to native code.  The compilation, that used LLVM, was slow.

* When a hot sequence had finally been converted to native code, it could turn
  out that it would not be executed again.  That was particularly a problem for
  the Erlang compiler that runs many passes.  Typically, when some of the code
  for one pass had been converted to native code, the compiler was already running
  the next pass.

The later projects mitigated some of the issues in the previous projects.  For example,
the compilation time was reduced by doing more optimizations before invoking LLVM.
Ultimately, though, it was decided to terminate the third and final tracing JIT project
at the end of 2019.

For more information about BEAMJIT, see:

* [BEAMJIT: a just-in-time compiling runtime for Erlang](https://dl.acm.org/doi/abs/10.1145/2633448.2633450?casa_token=tp5GAPlARNcAAAAA%3AEQbE8U2rzCpMKrkzE2eQbbS_fgKuKs7sAuvHFPWO3RjkU1p-3XsMi-Pkbc8A1DFurvvsa6t7qRuI)

* [Just-in-time in No Time? "Use the Source!"](https://www.youtube.com/watch?v=sNLeeakZppU)

* [JIT, a Maze of Twisty Little Traces](https://www.youtube.com/watch?v=JUjXbWqe5F0)

* [A Status Update of BEAMJIT, the Just-in-Time Compiling Abstract Machine](https://vimeo.com/99714181)

* [Just-in-time compiler for the Erlang VM](https://www.youtube.com/watch?v=M3_m0SI_-KM)

* [Tracing JIT Compiler](https://www.youtube.com/watch?v=PtgD5WRzcy4)



### The new JIT (also known as BeamAsm)

After the end of the third tracing JIT project, [Lukas
Larsson][lukas], having been involved in the last two tracing JIT
projects, could not stop thinking about different approaches that
might lead to a useful JIT.  The things that slowed down the previous
approaches were the tracing to find hot code and the generation of
optimized native code using LLVM.  Would it be possible to have a
simpler JIT that didn't do tracing and did no or little optimization?

In January 2020, salvaging some code from the third tracing JIT
project, Lukas quickly built a prototype BEAM system that translated
each BEAM instruction at load time to native code.  The resulting code
was less optimized than LLVM-generated code because it would still use
BEAM's stack and X registers (stored in memory), but the overhead for
[instruction unpacking and instruction dispatch][overhead] was
eliminated.

[overhead]: http://blog.erlang.org/a-closer-look-at-the-interpreter/

The initial benchmarks results were promising: about twice as fast
compared to interpreted BEAM code, so Lukas extended the prototype
so that it could handle more kinds of BEAM instructions.

[John Högberg][john] quickly became interested in the project and
started to act as a sounding board.  Some time later, probably in
March, John suggested that the new JIT should translate **all** loaded
code to native code.  That way, there would be no need to support
context switching between the BEAM interpreter and native code, which
would make the design simpler and eliminate the cost for context
switches.

That was a gamble, of course. After all, it could turn out that the
native code could be too large to be practically useful or decrease
performance because it fitted badly in the code cache.  They decided
that it was worth taking the risk and that it would probably be
possible to optimize the size of the code later.  (*Spoiler*: At the time
of writing, the native code generated by the JIT is about 10 percent
larger than interpreted BEAM code.)

Another change to the design was the tooling for generating the native
code. In Lukas's prototype, the native code template for each
instruction was contained in text files similar to the other files
used by the loader.  That was inflexible, so it was decided to use
some library that could generate native code.  While some pure C
libraries could have been used, the C++ library [AsmJIT][asmjit] was
more convenient in practical use than any of the C libraries.  Also,
some C libraries were excluded because they used a GNU license,
which we can't use in OTP. Therefore the part of the loader that
translates BEAM instructions to native code needed to be written in
C++, but the rest of the runtime system is still pure C code and will
remain so.

[asmjit]: https://asmjit.com

John joined the practical work on the rejigged JIT project at the end
of March.

On April 7, 2020, John reached the "prompt beer" milestone.

### Prompt Beer

When the Erlang system is started, a surprisingly large amount of code is
executed before the prompt appears. On the one hand, that means that
the translation of many instructions needs to be implemented
before it would be possible to even start the Erlang system, let alone run
any test suites or benchmarks.

On the other hand, when the prompt finally appears, it is a major
milestone worth celebrating with some prompt beer or other appropriate
beverage or by taking the rest of the evening off.

### Maturing the new JIT

On April 14 John got Dialyzer running with the JIT, and on April 17,
after some improvements to the code generation, Dialyzer was only
about 10 percent slower with the JIT than with HiPE. None of the
tracing JITs had had any success in speeding up Dialyzer. (At the time of
writing, Dialyzer runs roughly as fast with the JIT as it did with
HiPE, although it has become increasingly difficult to do a fair
comparison since HiPE doesn't work beyond OTP 23.)

It was probably at that point we realized that we had a JIT that could
finally be included in an OTP release.

The next major milestone was reached on May 6 when line numbers in
stack traces were implemented. That meant that many more test cases now
succeeded.

Soon after that, all test suites could be run successfully.  During the
summer and early fall [Dan](http://github.com/dgud) and 
[I](http://github.com/bjorng) joined the project part-time and the
following was done:

* A major refactoring of the BEAM loader so that as much code as possible
  could be shared between the JIT and the BEAM interpreter. (The BEAM interpreter
  is only used on platforms that don't support the JIT.)

* Implementation and polishing of important but less used features
  such as tracing, and [perf][perf] support, and save calls (see the
  flag `save_calls` for [process_flag/2][process_flag]).

* Shrinking of the code size of the generated native code.

* Porting the JIT to Windows, which turned out to be relatively easy.

* Making it possible to use the native stack pointer register and
  stack manipulation instructions. That improved perf support and
  slightly reduced the size of the native code.

The work culminated in a public [pull request][beamasm] that Lukas
created during his [presentation][jit_presentation] of the new JIT on
September 11.

The pull request was merged on September 22.

[beamasm]: https://github.com/erlang/otp/pull/2745
[jit_presentation]: https://www.youtube.com/watch?v=lM7UV95Rpyo
[perf]: https://en.wikipedia.org/wiki/Perf_(Linux)
[process_flag]: https://erlang.org/doc/man/erlang.html#process_flag-2

### The Future

Here are a few of the improvements that we have been thinking of
for future releases:

* Supporting ARM-64 (used by Raspberry Pi and Apple's new Macs with
  Apple Silicon).

* Implementing type-guided generation of native code. The new
  [SSA-based compiler passes][ssa] introduced in OTP 22 does a
  sophisticated type analysis.  Frustratingly, not all of the type
  information can be leveraged to generate better code for the
  interpreted BEAM.  We plan to modify the compiler so that some of
  the type information will be included in the BEAM files and then
  used by the JIT during code generation.

* Introducing new instructions for binary matching and/or construction
  to help the JIT generate better code.

[ssa]: /blog/ssa-history/

[john]: https://github.com/jhogberg
[lukas]: https://github.com/garazdawi
