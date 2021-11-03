---
layout: post
title: A first look at the JIT
tags: BEAM erts jit
author: John Högberg
---

Now that we've had a look at [BEAM] and the [interpreter] we're going to
explore one of the most exciting additions in OTP 24: the just-in-time
compiler, or "JIT" for short.

If you're like me the word "JIT" probably makes you think of Hotspot (Java) or
V8 (Javascript). These are very impressive pieces of engineering but they seem
to have hijacked the term; not all JITs are that sophisticated, nor do they
have to be in order to be fast.

We've made many attempts at a JIT over the years that aimed for the stars only
to fall down. Our latest and by far most successful attempt went for simplicity
instead, trading slight inefficiencies in the generated code for ease of
implementation. If we exclude the run-time assembler library we use, [asmjit],
the entire thing is roughly as big as the interpreter.

I believe much of our success can be attributed to four ideas we had early in
the project:

1. **All modules are always compiled to machine code.**

   Previous attempts (and HiPE too) had a difficult time switching between the
   interpreter and machine code: it was either too slow, too difficult to
   maintain, or both.

   Always running machine code means we never have to switch.

2. **Data may only be kept (passed) in BEAM registers between instructions.**

   This may seem silly, aren't machine registers faster?

   Yes, but in practice not by much and it would make things more complicated.
   By always passing data in BEAM registers we can use the register allocation
   given to us by the Erlang compiler, saving us from having to do this very
   expensive step at runtime.

   More importantly, this minimizes the difference between the interpreter and
   the JIT from the runtime system's point of view.

3. **Modules are compiled one instruction at a time.**

   One of the most difficult problems in our prior attempts was to strike a
   good balance between the time it took to compile something and the eagerness
   to do so. If we're too eager, we'll spend too much time compiling, and if
   we're too lax we won't see any improvements.

   This problem was largely self-inflicted and caused by the compiler being too
   slow (we often used LLVM), which was made worse by us giving it large pieces
   of code to allow more optimizations.

   By limiting ourselves to compiling one instruction at a time, we leave some
   performance on the table but greatly improve compilation speed.

4. **Every instruction has a handwritten machine code template.**

   This makes compilation _extremely_ fast as we basically just copy-paste
   the template every time the instruction is used, only performing some minor
   tweaks depending on its arguments.

   This may seem daunting at first but it's actually not that bad once you get
   used to it. While it certainly takes a lot of code to achieve even the
   smallest of things, it's inherently simple and easy to follow as long as
   the code is kept short.

   The downside is that every instruction needs to be implemented for each
   architecture, but luckily there's not a lot of popular ones and we hope to
   support the two most common ones by the time we release OTP 24: `x86_64`
   and `AArch64`. The others will continue to use the interpreter.

When compiling a module the JIT goes through the instructions one by one,
invoking machine code templates as it goes. This has two very large benefits
over the interpreter: there's no need to jump between them because they're
emitted back-to-back and the end of each is the start of the next one, and the
arguments don't need to be resolved at runtime because they're already "burnt
in."

Now that we have some background, let's look at the machine code template for
our example in the previous post, `is_nonempty_list`:

```c++
/* Arguments are passed as `ArgVal` objects which hold a
 * type and a value, for example saying "X register 4",
 * "the atom 'hello'", "label 57" and so on. */
void BeamModuleAssembler::emit_is_nonempty_list(const ArgVal &Fail,
                                                const ArgVal &Src) {
    /* Figure out which memory address `Src` lives in. */
    x86:Mem list_ptr = getArgRef(Src);

    /* Emit a `test` instruction, which does a non-
     * destructive AND on the memory pointed at by
     * list_ptr, clearing the zero flag if the list is
     * empty. */
    a.test(list_ptr, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));

    /* Emit a `jnz` instruction, jumping to the fail label
     * if the zero flag is clear (the list is empty). */
    a.jnz(labels[Fail.getValue()]);

    /* Unlike the interpreter there's no need to jump to
     * the next instruction on success as it immediately
     * follows this one. */
}
```

This template will generate code that looks almost identical to the template
itself. Let's say our source is "`X` register 1" and our fail label is 57:

```
test qword ptr [rbx+8], _TAG_PRIMARY_MASK - TAG_PRIMARY_LIST
jnz label_57
```

This is much faster than the interpreter, and even a bit more compact than the
threaded code, but this is a trivial instruction. What about more complex
ones? Let's have a look at the `timeout` instruction in the interpreter:

```c++
timeout() {
    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
        trace_receive(c_p, am_clock_service, am_timeout, NULL);
    }
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
        save_calls(c_p, &exp_timeout);
    }
    c_p->flags &= ~F_TIMO;
    JOIN_MESSAGE(c_p);
}
```

That's bound to be a lot of code, and those macros will be really annoying to
convert by hand. How on earth are we going to do this without losing our minds?

By cheating, that's how :D

```c++
static void timeout(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
        trace_receive(c_p, am_clock_service, am_timeout, NULL);
    }
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
        save_calls(c_p, &exp_timeout);
    }
    c_p->flags &= ~F_TIMO;
    JOIN_MESSAGE(c_p);
}

void BeamModuleAssembler::emit_timeout() {
    /* Set the first C argument to our currently executing
     * process, c_p, and then call the above C function. */
    a.mov(ARG1, c_p);
    a.call(imm(timeout));
}
```

This little escape hatch saved us from having to write everything in assembler
from the start, and many instructions remain like this because there hasn't
been any point to changing them.

That's all for today. In the next post we'll walk through our conventions and
some of the techniques we've used to reduce the code size.

[asmjit]: https://asmjit.com/
[BEAM]: http://blog.erlang.org/a-brief-BEAM-primer/
[interpreter]: http://blog.erlang.org/a-closer-look-at-the-interpreter/
