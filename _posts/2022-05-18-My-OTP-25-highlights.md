---
layout: post
title: Erlang/OTP 25 Highlights
tags: erlang otp 25 release
author: Kenneth Lundin
---

OTP 25 is finally here. This post will introduce the new features that I am most excited about.

Erlang/OTP 25 includes contributions from ??+ external contributors totalling
??+ commits, ??+ PRs.

You can download the readme describing the changes here: [Erlang/OTP 25 Readme].
Or, as always, look at the release notes of the application you are interested in.
For instance here: [Erlang/OTP 25 - Erts Release Notes - Version 13.0].

This years highlights are:
* New functions in the `maps`and `lists` modules
* Selectable features as of EEP-60
* The new `maybe`expression (`maybe_expr) EEP-49
* The JIT now works for 64-bit ARM processors.
* The JIT now does type-based optimizations based on type information in the BEAM files.
* Improved the JIT’s support for external tools like `perf` and `gdb`
* ETS-tables with adaptive support for write concurrency
* Compiler news
* Relocatable installation directory for Erlang
* New option `short` to the functions `erlang:float_to_list/2` and `erlang:float_to_binary/2` 
* Introduction of quote/1 and unquote/1 functions in the uri_string module
* The new module peer supersedes the slave module
* global will now by default prevent overlapping partitions
* gen_server, gen_statem and gen_event has got a new format_status/1 callback.
* The timer module has been modernized and made more efficient

    Add compile attribute -nifs() to empower compiler and loader with information about which functions may be overridden as NIFs by erlang:load_nif/2.
    Improved and more detailed error messages when binary construction with the binary syntax fails. This applies both for error messages in the shell and for erl_error:format_exception/3,4.


    Add crypto:hash_equals/2 which is a constant time comparision of hashvalues.

Dialyzer #

    Optimize operations in the erl_types module. Parallelize the Dialyzer pass remote.
    Added the missing_return and extra_return options to raise warnings when specifications differ from inferred types. These are similar to, but not quite as verbose as overspecs and underspecs.
    Dialyzer now better understands the types for min/2, max/2, and erlang:raise/3. Because of that, Dialyzer can potentially generate new warnings. In particular, functions that use erlang:raise/3 could now need a spec with a no_return() return type to avoid an unwanted warning.

Misc #

    A new DEVELOPMENT HOWTO guide has been added that describes how to build and test Erlang/OTP when fixing bugs or developing new functionality.
    Testing has been added to the Github actions run for each opened PR so that more bugs are caught earlier when bug fixes and new features are proposed.

For more details about new features and potential incompatibilities see

    https://erlang.org/download/otp_src_25.0-rc3.readme

* [BeamAsm - the JIT compiler for Erlang](#beamasm---the-jit-compiler-for-erlang)
* [Improved error messages](#improved-error-messages)
* [Improved receive optimizations](#improved-receive-optimizations)
* [EEP-53: Process aliases](#eep-53-process-aliases)
* [EEP-48: Documentation chunks for edoc](#eep-48-documentation-chunks-for-edoc)
* [socket support in gen_tcp](#socket-support-in-gen_tcp)
* [EEP-56: Supervisor automatic shutdown](#EEP-56-supervisor-automatic-shutdown)
* [Edwards-curve Digital Signature Algorithm](#edwards-curve-digital-signature-algorithm)

