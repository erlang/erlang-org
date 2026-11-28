# `tags`
[🔗](https://github.com/erlang/otp/blob/master/lib/tools/src/tags.erl#L30)

Generate Emacs TAGS file from Erlang source files

A `TAGS` file is used by Emacs to find function and variable definitions in any
source file in large projects. This module can generate a `TAGS` file from
Erlang source files. It recognises functions, records, and macro definitions.

## Options

The functions in this module have an optional argument `Options`. It
is a list which can contain the following elements:

- `{outfile, NameOfTAGSFile}` Create a `TAGS` file named `NameOfTAGSFile`.
- `{outdir, NameOfDirectory}` Create a file named `TAGS` in the directory
  `NameOfDirectory`.

The default behaviour is to create a file named `TAGS` in the current directory.

## Examples

- `tags:root([{outfile, "root.TAGS"}]).`

  This command will create a file named `root.TAGS` in the current directory.
  The file will contain references to all Erlang source files in the Erlang
  distribution.

- `tags:files(["foo.erl", "bar.erl", "baz.erl"], [{outdir, "../projectdir"}]).`

  This command will create a file named `TAGS` placed it in the
  directory `../projectdir`. The file contains information about the
  functions, records, and macro definitions of the three files.

### See Also

- Richard M. Stallman. GNU Emacs Manual, chapter "Editing Programs", section
  "Tag Tables". Free Software Foundation, 1995.
- Anders Lindgren. The Erlang editing mode for Emacs. Ericsson, 1998.

# `option`
*not exported* 

```erlang
-type option() ::
          {outfile, NameOfTAGSFile :: file:filename()} | {outdir, NameOfDirectory :: file:filename()}.
```

# `dir`

```erlang
-spec dir(Dir) -> ok | error when Dir :: file:filename().
```

# `dir`

```erlang
-spec dir(Dir, Options) -> ok | error when Dir :: file:filename(), Options :: [option()].
```

Create a `TAGS` file for all files in directory `Dir`.

# `dirs`

```erlang
-spec dirs(DirList) -> ok | error when DirList :: [file:filename()].
```

# `dirs`

```erlang
-spec dirs(DirList, Options) -> ok | error when DirList :: [file:filename()], Options :: [option()].
```

Create a `TAGS` file for all files in any directory in `DirList`.

# `file`

```erlang
-spec file(File) -> ok | error when File :: file:filename().
```

# `file`

```erlang
-spec file(File, Options) -> ok | error when File :: file:filename(), Options :: [option()].
```

Create a `TAGS` file for the file `File`.

# `files`

```erlang
-spec files(FileList) -> ok | error when FileList :: [file:filename()].
```

# `files`

```erlang
-spec files(FileList, Options) -> ok | error when FileList :: [file:filename()], Options :: [option()].
```

Create a `TAGS` file for the files in the list `FileList`.

# `root`

```erlang
-spec root() -> ok | error.
```

# `root`

```erlang
-spec root(Options) -> ok | error when Options :: [option()].
```

Create a `TAGS` file covering all files in the Erlang distribution.

# `subdir`

```erlang
-spec subdir(Dir) -> ok | error when Dir :: file:filename().
```

# `subdir`

```erlang
-spec subdir(Dir, Options) -> ok | error when Dir :: file:filename(), Options :: [option()].
```

Descend recursively into the directory `Dir` and create a `TAGS` file based on
all files found.

# `subdirs`

```erlang
-spec subdirs(DirList) -> ok | error when DirList :: [file:filename()].
```

# `subdirs`

```erlang
-spec subdirs(DirList, Options) -> ok | error when DirList :: [file:filename()], Options :: [option()].
```

Descend recursively into the directories in `DirList` and create a `TAGS`
file based on all files found.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
