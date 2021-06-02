---
layout: post
title: Interpreter optimization
tags: interpreter BEAM
author: Lukas Larsson
---

The BEAM [interpreter] in erts has been completely re-written in OTP 21.
Most of the instructions have remained the same, but the perl scripts used
to generate the C code have a new implementation. This blog post will look at
some of the optimizations that were possible because of those changes.

First however, let's start with some basics about how the interpreter is
built. The BEAM interpreter is built using generated C code. Dispatching
is done using [direct threading] which leverages the GCC extension [labels as values].
It is the job of the [beam_makeops] perl script to take the input files
and create the C files.

There is also a set of transformation rules generated that is used by the
[transform_engine] in the beam code loader to do several peephole optimization.
The optimization include, but are not limited to, instruction combining,
instruction specialization and dead code elimination.

There are three separate types of input files used.

* [genop.tab] contains a listing of the instructions that the compiler emits.
* [ops.tab] contains load time transformations done to the code.
* [instrs.tab] contain the implementation of each instruction.

There is a description of the syntax and semantics of the different files
in the [internal beam_makeops documentation]. The largest difference
between the new and old way of doing the code generation is that
in OTP 21 _all_ the instructions are now generated, instead of about 75%.
This allowed us to do architecture specific optimizations for all instructions
when generating the code for the instructions.

[interpreter]: https://en.wikipedia.org/wiki/Interpreter_(computing)
[direct threading]: https://en.wikipedia.org/wiki/Threaded_code#Direct_threading
[labels as values]: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
[beam_makeops]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/utils/beam_makeops
[genop.tab]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/lib/compiler/src/genop.tab
[ops.tab]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/beam/ops.tab
[instrs.tab]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/beam/instrs.tab
[internal beam_makeops documentation]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/internal_doc/beam_makeops.md
[transform_engine]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/beam/beam_load.c#L5213

## Packing the dispatch target address ##

On 64-bit machines all pointers are 8 byte large, this is also the case for
the pointer that you get when taking the address of a label. So for a small
instruction such as move_cx (move a constant to an x register), 3 words of
memory is needed.

         +--------------------+--------------------+
    I -> |                            &&lb_move_cx |
         +--------------------+--------------------+
         |                        Tagged atom 'id' |
         +--------------------+--------------------+
         |                                      40 |
         +--------------------+--------------------+

One word for the instruction, one word for the literal and then the target x
register actually only needs 2 bytes, but since all code is word aligned it
gets its own word.

However, on most machines, the linker uses what is called a small or medium code model.
This means that it will place all code in the lower 2 GB of the address space
so that more efficient machine instructions can be used. It also works in the
favor of the beam interpreter as we now know that the upper 4 bytes of the
instruction word will always be 0.

         +--------------------+--------------------+
    I -> |                 40 |       &&lb_move_cx |
         +--------------------+--------------------+
         |                        Tagged atom 'id' |
         +--------------------+--------------------+

So instead of placing the x register to use in its own word we pack it into the
instruction word, thus saving one word of memory for this instruction.

The optimization is only possible to do on 64-bit platforms that place all code in the
lower 4 GB of the address space, so that makes 32-bit and [position-independent executable]
platforms not use it.
To figure out which 64-bit platforms we can do it on we first started by writing
a configure script that looked at CFLAGS and LDFLAGS, asked the compiler what it would do
by default etc etc. After tinkering for a while we came up with a simpler and so
far stable solution:

    #include <stdlib.h>
    int main() {
      if ((unsigned long long)&main < (1ull << 32)) {
        exit(0);
      }
      exit(1);
    }

It would seem that [position-independent executable] always places code in
segments > 4GB so we can just check where it put main in a small test program.

Packing arguments into the instruction word is possible for a significant amount
of instructions which reduced loaded code size and in turn increased performance.

## Smarter packing ##

The packing engine in [beam\_makeops] uses several different heuristics to figure
out which instruction argument(s) it should put in the instruction word. Because
of alignment requirements and other things, only some types of argument are allowed
together. When choosing how to pack arguments the packer first builds all different
variant possible and then chooses the instruction that takes the least amount of
memory. The logic for this can be found in the [do_pack_one] function in
[beam_makeops]. Before OTP-21 this had to be done manually for all instructions,
which meant that the implementer had to be extra careful when deciding which
order the arguments should be placed in [ops.tab]. In addition the packing logic
before OTP-21 did not pack argument of varying size into the same machine word.

The packer is not perfect however, so in some cases we needed to make it
possible to override its decisions. That is why the [? type modifier] was introduced.
The [? type modifier] is used to determine whether an argument is likely to be used
or not by the instruction.

How do you determine if an argument is likely to be used or not? For some instructions
it is obvious, eg. the allocate instruction has an argument that is only used
if a garbage collection is triggered, so it is very unlikely to be used. In other cases
it is not so obvious, eg. the failure label of test instructions, is it likely to be
used or not? For most test instructions it will be unlikely that it will be used,
or at least more unlikely than the other arguments of the instruction as they are used
in the actual test.

Why is packing less used instructions into the instruction word better? It seems like
GCC will generate a little better code for most instructions if the arguments that
are always used are in the same word, we've seen this by both looking at the assembly
that GCC produced and the total code size of the interpreter becomes smaller. We haven't
been able to measure any differences in performance due to how the unlikely instructions
are packed, but smaller code is always a good thing.

[? type modifier]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/internal_doc/beam_makeops.md#the--type-modifier
[position-independent executable]: https://en.wikipedia.org/wiki/Position-independent_code#Position-independent_executables
[do_pack_one]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/utils/beam_makeops#L1801

## Relative jump labels ##

Many instruction have branches in them that will be taken depending on various conditions.
One of the simplest ones is the [is_eq_exact_immed] instruction. It will continue on
to the next instruction if the value is equal to the immediate literal in the argument,
or jump to the fail label if they are not equal. Before OTP-21 the instruction
is\_eq\_exact\_immed\_frc would have this layout:

         +--------------------+--------------------+
    I -> |              &&lb_is_eq_exact_immed_frc |
         +--------------------+--------------------+
         |                 Pointer To failure code |
         +--------------------+--------------------+
         |                       Tagged immed 'id' |
         +--------------------+--------------------+

The C code for the instruction would look something like this (I've removed all macros
that beam\_makeops uses):

    if (reg[0] != I[2]) {
      I = I[1];
      goto *(void**)I;
    }
    I+=3;
    goto *(void**)I;

In this example it does not help to use part of the instruction word as both arguments
are 8 bytes large. We also cannot use the same trick as the instruction word to make
the fail label smaller as Erlang code can be allocated anywhere in the 64-bit address
space. One thing that we can rely on though is that the code for a single Erlang module
will be located in a contiguous memory area. So instead of using pointers to the code
that we want to jump to we can use relative addresses for jumps within the same module.

The problem with using relative local jumps is that we put a limit on how large a module
can be. For instance, the largest module in Erlang/OTP is the 'OTP-PUB-KEY' module,
it is 61585 words large when loaded and the longest local jump is 5814 words. For this
particular module we could have used a 13-bit jump label, or 16-bits if we wanted to be
sure that all functions could call each other. 16-bits would have been a perfect size as
it can be packed with another register in the instruction word. So it would have been
possible to fit a instruction address + jump label + register in 8 bytes. However, it is
way too close for comfort, and there are sure to be larger modules than ours in other
systems, so we decided to use 32-bits for the jump labels. So in OTP-21 the max size of
a loaded module has gone from 2^32 GB to 32 GB, which should be enough for most use cases.

Using this new layout the is\_eq\_exact\_immed\_frc instruction can be re-written to use
the following layout instead:

         +--------------------+--------------------+
    I -> | Offset to failure..| &&lb_is_eq_exac... |
         +--------------------+--------------------+
         |                       Tagged immed 'id' |
         +--------------------+--------------------+

And generate this code:

    if (reg[0] != I[1]) {
      I += I[0] >> 32;
      goto *(void**)(Uint64)(Uint32)I;
    }
    I+=2;
    goto *(void**)(Uint64)(Uint32)I;

The code ends up being a little bit more complicated, but the C compiler manages to optimize
it into very efficient code. Basically for each jump label there is an extra add operation
when compared to before. When profiling this extra code is not noticeable as it drowns in
the load of the value from (hopefully) the l1-cache.

A lot of instructions benefit from this optimization as a lot of them control the control
flow. This is especially noticeable on large [select_val_bins], as they have their memory
usage reduced by 25%. Also as you may have noticed, it plays very well with the packing
of the instruction word and the [? type modifier] in [beam_makeops].

[is_eq_exact_immed]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/beam/instrs.tab#L789
[select_val_bins]: https://github.com/erlang/otp/blob/OTP-21.0-rc1/erts/emulator/beam/select_instrs.tab#L32-L76
