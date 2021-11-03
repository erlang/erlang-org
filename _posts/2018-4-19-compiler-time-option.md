---
layout: post
title: Exploring the Compiler Using the 'time' Option
tags: compiler BEAM
author: Björn Gustavsson
---

This is the first of a series of blog posts about the compiler.  There
will be blog posts about how the compiler works now, how it might work
in the future, and some historical notes to explain why some things
are what they are. In this blog post I will talk about one of the most
useful options for exploring the compiler, namely the `time` option.

First let see `time` in action on a huge file with many functions
and many variables so that the numbers get interesting:

```
$ erlc +time NBAP-PDU-Contents.erl
Compiling "NBAP-PDU-Contents"
 remove_file                   :      0.000 s       6.5 kB
 parse_module                  :      0.709 s   25146.1 kB
 transform_module              :      0.000 s   25146.1 kB
 lint_module                   :      0.426 s   25146.1 kB
 expand_records                :      0.086 s   25993.7 kB
 core                          :      0.675 s  282518.3 kB
 sys_core_fold                 :      1.566 s  237885.4 kB
 core_transforms               :      0.000 s  237885.4 kB
 sys_core_bsm                  :      0.205 s  238982.3 kB
 sys_core_dsetel               :      0.108 s  238982.3 kB
 v3_kernel                     :      0.950 s  305320.5 kB
 v3_life                       :      0.453 s  221354.8 kB
 v3_codegen                    :      0.896 s   75801.0 kB
 beam_a                        :      0.080 s   75561.2 kB
 beam_reorder                  :      0.049 s   75561.2 kB
 beam_block                    :      0.361 s   87171.9 kB
 beam_except                   :      0.041 s   81557.7 kB
 beam_bs                       :      0.097 s   79929.2 kB
 beam_type                     :      0.502 s   77270.5 kB
 beam_split                    :      0.042 s   75004.5 kB
 beam_dead                     :      0.356 s   77566.7 kB
 beam_jump                     :      0.232 s   73347.9 kB
 beam_peep                     :      0.164 s   73346.0 kB
 beam_clean                    :      0.150 s   73081.0 kB
 beam_bsm                      :      0.092 s   75473.2 kB
 beam_receive                  :      0.020 s   75473.2 kB
 beam_record                   :      0.023 s   75471.4 kB
 beam_trim                     :      0.042 s   75471.4 kB
 beam_flatten                  :      0.071 s   66745.5 kB
 beam_z                        :      0.019 s   66442.2 kB
 beam_validator                :      0.401 s   66442.2 kB
 beam_asm                      :      0.236 s       6.5 kB
 save_binary                   :      0.000 s       6.5 kB
```

When the `time` option is given, the compiler will print a line after
executing each compiler pass.  First on each line is the name of the
compiler pass. Often, but not always, the name is the name of the
Erlang module that implements the compiler pass.

The name is followed by the time (in seconds) that the compiler
spent running that compiler pass. For smaller files, the time
is usually zero or nearly zero. For this huge file, most of the
times are non-zero. For example, the `sys_core_fold` pass needs
about one and a half second to do its work.

The time is followed by the amount of memory used by that compiler
pass.

In this blog post, I will just talk about a few of the compiler
passes. There will be more about what the compiler passes do in later
blog posts.

The `remove_file` pass is the very first pass run. It removes any
existing BEAM file so that there will not be an outdated BEAM file
in case the compilation fails. The last pass is the `save_binary`
pass. It saves the binary with the BEAM code to the BEAM file.

Now let's see how the output changes if we give the `-S` option:

```
$ erlc -S +time NBAP-PDU-Contents.erl
Compiling "NBAP-PDU-Contents"
 parse_module                  :      0.718 s   25146.1 kB
 transform_module              :      0.000 s   25146.1 kB
 lint_module                   :      0.420 s   25146.1 kB
 expand_records                :      0.088 s   25993.8 kB
 core                          :      0.671 s  282518.3 kB
 sys_core_fold                 :      1.564 s  237885.4 kB
 core_transforms               :      0.000 s  237885.4 kB
 sys_core_bsm                  :      0.203 s  238982.3 kB
 sys_core_dsetel               :      0.104 s  238982.3 kB
 v3_kernel                     :      0.964 s  305320.5 kB
 v3_life                       :      0.375 s  221354.8 kB
 v3_codegen                    :      1.044 s   75801.0 kB
 beam_a                        :      0.091 s   75561.3 kB
 beam_reorder                  :      0.044 s   75561.3 kB
 beam_block                    :      0.276 s   87171.9 kB
 beam_except                   :      0.028 s   81557.8 kB
 beam_bs                       :      0.103 s   79929.3 kB
 beam_type                     :      0.518 s   77270.5 kB
 beam_split                    :      0.049 s   75004.6 kB
 beam_dead                     :      0.379 s   77566.8 kB
 beam_jump                     :      0.195 s   73347.9 kB
 beam_peep                     :      0.156 s   73346.0 kB
 beam_clean                    :      0.168 s   73081.0 kB
 beam_bsm                      :      0.070 s   75473.2 kB
 beam_receive                  :      0.044 s   75473.2 kB
 beam_record                   :      0.021 s   75471.5 kB
 beam_trim                     :      0.041 s   75471.5 kB
 beam_flatten                  :      0.045 s   66745.5 kB
 beam_z                        :      0.016 s   66442.2 kB
 listing                       :      1.503 s   66442.2 kB
```

We can see how the list of passes has changed. The last pass run is
now `listing`, which produces a listing of the BEAM assembly code in a
`.S` file. The `remove_file` pass in the beginning is not run because
no BEAM file is being produced and any existing BEAM file should be
preserved.

Let's try one of the many undocumented debugging options:

```
$ erlc +no_postopt +time NBAP-PDU-Contents.erl
Compiling "NBAP-PDU-Contents"
 remove_file                   :      0.000 s       6.5 kB
 parse_module                  :      0.706 s   25146.1 kB
 transform_module              :      0.000 s   25146.1 kB
 lint_module                   :      0.421 s   25146.1 kB
 expand_records                :      0.090 s   25993.8 kB
 core                          :      0.684 s  282518.3 kB
 sys_core_fold                 :      1.614 s  237885.4 kB
 core_transforms               :      0.000 s  237885.4 kB
 sys_core_bsm                  :      0.210 s  238982.3 kB
 sys_core_dsetel               :      0.105 s  238982.3 kB
 v3_kernel                     :      0.967 s  305320.5 kB
 v3_life                       :      0.353 s  221354.8 kB
 v3_codegen                    :      1.028 s   75801.0 kB
 beam_a                        :      0.091 s   75561.3 kB
 beam_clean                    :      0.201 s   73513.2 kB
 beam_z                        :      0.023 s   72897.9 kB
 beam_validator                :      0.467 s   72897.9 kB
 beam_asm                      :      0.396 s       6.6 kB
 save_binary                   :      0.001 s       6.5 kB
```

We can see that far fewer passes were run. The `no_postopt` option
turns off all optimizations run on the BEAM code (i.e. all optimizations
after `v3_codegen`).

## So why is this `time` option useful?

* When compilation of a module is very slow, `time` can show if any particular
passes are bottlenecks (much slower than the other passes). In fact, a long time
ago the compiler needed several minutes to compile the `NBAP-PDU-Contents` module
that I have used an example in this blog post. The `time` option immediately pointed
out the bottlenecks that I needed to fix.

* If the compiler doesn't terminate when compiling a certain module, `time` will
show the last successfully run pass (the one before the culprit).

* The compiler ignores options it doesn't recognize, so if you
misremember or misspell an option, the compiler will not do what you
expect. Adding the `time` option can help you verify that the expected
compiler passes are run.

## Where are all those undocumented options documented?

There are many options meant for debugging that allow you skip certain optimization
passes or to produce a listing of the code after a certain pass.

Most of these options can be shown by running `compile:options/0` from the Erlang shell:

```
$ erl
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.2  (abort with ^G)
1> compile:options().
dpp - Generate .pp file
'P' - Generate .P source listing file
dabstr - Generate .abstr file
debug_info - Run save_abstract_code
dexp - Generate .expand file
'E' - Generate .E source listing file
dcore - Generate .core file
clint0 - Run core_lint_module
doldinline - Generate .oldinline file
dcorefold - Generate .corefold file
dinline - Generate .inline file
dcopt - Generate .copt file
.
.
.
```

## Points to Ponder

Why does the name of some compiler passes begin with `v3`? Follow this blog, and there might
be an answer in a future blog post.
