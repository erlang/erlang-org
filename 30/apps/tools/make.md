# `make`
[🔗](https://github.com/erlang/otp/blob/master/lib/tools/src/make.erl#L28)

A Make Utility for Erlang

The module `make` provides a set of functions similar to the UNIX type `Make`
functions.

## Emakefile

[`make:all/0,1`](`all/1`) and [`make:files/1,2`](`files/2`) first looks for
`{emake, Emake}` in options, then in the current working directory for a file
named `Emakefile`. If present `Emake` should contain elements like this:

```text
Modules.
{Modules,Options}.
```

`Modules` is an atom or a list of atoms. It can be

- a module name, for exmaple,  `file1`
- a module name in another directory, for exmaple, `'../foo/file3'`
- a set of modules specified with a wildcards, for exmaple, `'file*'`
- a wildcard indicating all modules in current directory, that is: `'*'`
- a list of any of the above, for exmaple, `['file*','../foo/file3','File4']`

`Options` is a list of compiler options.

`Emakefile` is read from top to bottom. If a module matches more than one entry,
the first match is used. For example, the following `Emakefile` means that
`file1` should be compiled with the options `[debug_info,{i,"../foo"}]`, while
all other files in the current directory should be compiled with only the
`debug_info` flag.

```erlang
{'file1',[debug_info,{i,"../foo"}]}.
{'*',[debug_info]}.
```

### See Also

[The Compiler Application](`m:compile`)

# `all`

```erlang
-spec all() -> up_to_date | error.
```

# `all`

```erlang
-spec all(Options) -> up_to_date | error
             when
                 Options :: [Option],
                 Option :: noexec | load | netload | {emake, Emake} | compile:option(),
                 Emake :: [EmakeElement],
                 EmakeElement :: Modules | {Modules, [compile:option()]},
                 Modules :: atom() | [atom()].
```

This function determines the set of modules to compile and the compile options
to use, by first looking for the `emake` make option, if not present reads the
configuration from a file named `Emakefile` (see below). If no such file is
found, the set of modules to compile defaults to all modules in the current
working directory.

Traversing the set of modules, it then recompiles every module for which at
least one of the following conditions apply:

- there is no object file, or
- the source file has been modified since it was last compiled, or,
- an include file has been modified since the source file was last compiled.

As a side effect, the function prints the name of each module it tries to
compile. If compilation fails for a module, the make procedure stops and `error`
is returned.

`Options` is a list of options for `make` and the Erlang compiler. The following
`make` options exist:

- `noexec`
  No execution mode. Just prints the name of each module that needs to be
  compiled.
- `load`
  Load mode. Loads all recompiled modules.
- `netload`
  Net load mode. Loads all recompiled modules on all known nodes.
- `{emake, Emake}`
  Rather than reading the `Emakefile` specify configuration explicitly.

All items in `Options` that are not make options are assumed to be compiler
options and are passed as-is to `compile:file/2`.

# `files`

```erlang
-spec files(ModFiles) -> up_to_date | error
               when ModFiles :: [(Module :: module()) | (File :: file:filename())].
```

# `files`

```erlang
-spec files(ModFiles, Options) -> up_to_date | error
               when
                   ModFiles :: [(Module :: module()) | (File :: file:filename())],
                   Options :: [Option],
                   Option :: noexec | load | netload | compile:option().
```

This function does exactly the same thing as [`all/0,1`](`all/0`), but for the
specified `ModFiles`, which is a list of module or file names.

The file extension `.erl` can be omitted.

The `Emakefile` (if it exists) in the current directory is searched for compiler
options for each module. If a given module does not exist in `Emakefile` or if
`Emakefile` does not exist, the module is still compiled.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
