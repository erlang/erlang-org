# `file_sorter`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/file_sorter.erl#L22)

File sorter.

This module contains functions for sorting terms on files, merging already
sorted files, and checking files for sortedness. Chunks containing binary terms
are read from a sequence of files, sorted internally in memory and written on
temporary files, which are merged producing one sorted file as output. Merging
is provided as an optimization; it is faster when the files are already sorted,
but it always works to sort instead of merge.

On a file, a term is represented by a header and a binary. Two options define
the format of terms on files:

- **`{header, HeaderLength}`** - `HeaderLength` determines the number of bytes
  preceding each binary and containing the length of the binary in bytes.
  Defaults to 4. The order of the header bytes is defined as follows: if `B` is
  a binary containing a header only, size `Size` of the binary is calculated as
  `<<Size:HeaderLength/unit:8>> = B`.

- **`{format, Format}`** - Option `Format` determines the function that is
  applied to binaries to create the terms to be sorted. Defaults to
  `binary_term`, which is equivalent to `fun binary_to_term/1`. Value `binary`
  is equivalent to `fun(X) -> X end`, which means that the binaries are sorted
  as they are. This is the fastest format. If `Format` is `term`, `io:read/2` is
  called to read terms. In that case, only the default value of option `header`
  is allowed.

  Option `format` also determines what is written to the sorted output file: if
  `Format` is `term`, then `io:format/3` is called to write each term, otherwise
  the binary prefixed by a header is written. Notice that the binary written is
  the same binary that was read; the results of applying function `Format` are
  thrown away when the terms have been sorted. Reading and writing terms using
  the `io` module is much slower than reading and writing binaries.

Other options are:

- **`{order, Order}`** - The default is to sort terms in ascending order, but
  that can be changed by value `descending` or by specifying an ordering
  function `Fun`. An ordering function is antisymmetric, transitive, and total.
  `Fun(A, B)` is to return `true` if `A` comes before `B` in the ordering,
  otherwise `false`. An example of a typical ordering function is less than or
  equal to, `=</2`. Using an ordering function slows down the sort considerably.
  Functions `keysort`, `keymerge` and `keycheck` do not accept ordering
  functions.

- **`{unique, boolean()}`** - When sorting or merging files, only the first of a
  sequence of terms that compare equal (`==`) is output if this option is set to
  `true`. Defaults to `false`, which implies that all terms that compare equal
  are output. When checking files for sortedness, a check that no pair of
  consecutive terms compares equal is done if this option is set to `true`.

- **`{tmpdir, TempDirectory}`** - The directory where temporary files are put
  can be chosen explicitly. The default, implied by value `""`, is to put
  temporary files on the same directory as the sorted output file. If output is
  a function (see below), the directory returned by `file:get_cwd()` is used
  instead. The names of temporary files are derived from the Erlang nodename
  (`node/0`), the process identifier of the current Erlang emulator
  (`os:getpid()`), and a unique integer (`erlang:unique_integer([positive])`). A
  typical name is `fs_mynode@myhost_1763_4711.17`, where `17` is a sequence
  number. Existing files are overwritten. Temporary files are deleted unless
  some uncaught `EXIT` signal occurs.

- **`{compressed, boolean()}`** - Temporary files and the output file can be
  compressed. Defaults `false`, which implies that written files are not
  compressed. Regardless of the value of option `compressed`, compressed files
  can always be read. Notice that reading and writing compressed files are
  significantly slower than reading and writing uncompressed files.

- **`{size, Size}`** - By default about 512\*1024 bytes read from files are
  sorted internally. This option is rarely needed.

- **`{no_files, NoFiles}`** - By default 16 files are merged at a time. This
  option is rarely needed.

As an alternative to sorting files, a function of one argument can be specified
as input. When called with argument `read`, the function is assumed to return
either of the following:

- `end_of_input` or `{end_of_input, Value}}` when there is no more input
  (`Value` is explained below).
- `{Objects, Fun}`, where `Objects` is a list of binaries or terms depending on
  the format, and `Fun` is a new input function.

Any other value is immediately returned as value of the current call to `sort`
or `keysort`. Each input function is called exactly once. If an error occurs,
the last function is called with argument `close`, the reply of which is
ignored.

A function of one argument can be specified as output. The results of sorting or
merging the input is collected in a non-empty sequence of variable length lists
of binaries or terms depending on the format. The output function is called with
one list at a time, and is assumed to return a new output function. Any other
return value is immediately returned as value of the current call to the sort or
merge function. Each output function is called exactly once. When some output
function has been applied to all of the results or an error occurs, the last
function is called with argument `close`, and the reply is returned as value of
the current call to the sort or merge function.

If a function is specified as input and the last input function returns
`{end_of_input, Value}`, the function specified as output is called with
argument `{value, Value}`. This makes it easy to initiate the sequence of output
functions with a value calculated by the input functions.

As an example, consider sorting the terms on a disk log file. A function that
reads chunks from the disk log and returns a list of binaries is used as input.
The results are collected in a list of terms.

```erlang
sort(Log) ->
    {ok, _} = disk_log:open([{name,Log}, {mode,read_only}]),
    Input = input(Log, start),
    Output = output([]),
    Reply = file_sorter:sort(Input, Output, {format,term}),
    ok = disk_log:close(Log),
    Reply.

input(Log, Cont) ->
    fun(close) ->
            ok;
       (read) ->
            case disk_log:chunk(Log, Cont) of
                {error, Reason} ->
                    {error, Reason};
                {Cont2, Terms} ->
                    {Terms, input(Log, Cont2)};
                {Cont2, Terms, _Badbytes} ->
                    {Terms, input(Log, Cont2)};
                eof ->
                    end_of_input
            end
    end.

output(L) ->
    fun(close) ->
            lists:append(lists:reverse(L));
       (Terms) ->
            output([Terms | L])
    end.
```

For more examples of functions as input and output, see the end of the
`file_sorter` module; the `term` format is implemented with functions.

The possible values of `Reason` returned when an error occurs are:

- `bad_object`, `{bad_object, FileName}` \- Applying the format function failed
  for some binary, or the key(s) could not be extracted from some term.
- `{bad_term, FileName}` \- `io:read/2` failed to read some term.
- `{file_error, FileName, file:posix()}` \- For an explanation of
  [`file:posix()`](`t:file:posix/0`), see `m:file`.
- `{premature_eof, FileName}` \- End-of-file was encountered inside some binary
  term.

# `file_name`
*not exported* 

```elixir
-type file_name() :: file:name().
```

# `file_names`
*not exported* 

```elixir
-type file_names() :: [file:name()].
```

# `format`
*not exported* 

```elixir
-type format() :: binary_term | term | binary | format_fun().
```

# `format_fun`
*not exported* 

```elixir
-type format_fun() :: fun((binary()) -> term()).
```

# `header_length`
*not exported* 

```elixir
-type header_length() :: pos_integer().
```

# `i_command`
*not exported* 

```elixir
-type i_command() :: read | close.
```

# `i_reply`
*not exported* 

```elixir
-type i_reply() :: end_of_input | {end_of_input, value()} | {[object()], infun()} | input_reply().
```

# `infun`
*not exported* 

```elixir
-type infun() :: fun((i_command()) -> i_reply()).
```

# `input`
*not exported* 

```elixir
-type input() :: file_names() | infun().
```

# `input_reply`
*not exported* 

```elixir
-type input_reply() :: term().
```

# `key_pos`
*not exported* 

```elixir
-type key_pos() :: pos_integer() | [pos_integer()].
```

# `no_files`
*not exported* 

```elixir
-type no_files() :: pos_integer().
```

# `o_command`
*not exported* 

```elixir
-type o_command() :: {value, value()} | [object()] | close.
```

# `o_reply`
*not exported* 

```elixir
-type o_reply() :: outfun() | output_reply().
```

# `object`
*not exported* 

```elixir
-type object() :: term() | binary().
```

# `option`
*not exported* 

```elixir
-type option() ::
          {compressed, boolean()} |
          {header, header_length()} |
          {format, format()} |
          {no_files, no_files()} |
          {order, order()} |
          {size, size()} |
          {tmpdir, tmp_directory()} |
          {unique, boolean()}.
```

# `options`
*not exported* 

```elixir
-type options() :: [option()] | option().
```

# `order`
*not exported* 

```elixir
-type order() :: ascending | descending | order_fun().
```

# `order_fun`
*not exported* 

```elixir
-type order_fun() :: fun((term(), term()) -> boolean()).
```

# `outfun`
*not exported* 

```elixir
-type outfun() :: fun((o_command()) -> o_reply()).
```

# `output`
*not exported* 

```elixir
-type output() :: file_name() | outfun().
```

# `output_reply`
*not exported* 

```elixir
-type output_reply() :: term().
```

# `reason`

```elixir
-type reason() ::
          bad_object |
          {bad_object, file_name()} |
          {bad_term, file_name()} |
          {file_error, file_name(), file:posix() | badarg | system_limit} |
          {premature_eof, file_name()}.
```

# `size`
*not exported* 

```elixir
-type size() :: non_neg_integer().
```

# `tmp_directory`
*not exported* 

```elixir
-type tmp_directory() :: [] | file:name().
```

# `value`
*not exported* 

```elixir
-type value() :: term().
```

# `check`

```elixir
-spec check(FileName) -> Reply
               when
                   FileName :: file_name(),
                   Reply :: {ok, [Result]} | {error, reason()},
                   Result :: {FileName, TermPosition, term()},
                   TermPosition :: pos_integer().
```

# `check`

```elixir
-spec check(FileNames, Options) -> Reply
               when
                   FileNames :: file_names(),
                   Options :: options(),
                   Reply :: {ok, [Result]} | {error, reason()},
                   Result :: {FileName, TermPosition, term()},
                   FileName :: file_name(),
                   TermPosition :: pos_integer().
```

Checks files for sortedness. If a file is not sorted, the first out-of-order
element is returned. The first term on a file has position 1.

# `keycheck`

```elixir
-spec keycheck(KeyPos, FileName) -> Reply
                  when
                      KeyPos :: key_pos(),
                      FileName :: file_name(),
                      Reply :: {ok, [Result]} | {error, reason()},
                      Result :: {FileName, TermPosition, term()},
                      TermPosition :: pos_integer().
```

# `keycheck`

```elixir
-spec keycheck(KeyPos, FileNames, Options) -> Reply
                  when
                      KeyPos :: key_pos(),
                      FileNames :: file_names(),
                      Options :: options(),
                      Reply :: {ok, [Result]} | {error, reason()},
                      Result :: {FileName, TermPosition, term()},
                      FileName :: file_name(),
                      TermPosition :: pos_integer().
```

Checks files for sortedness. If a file is not sorted, the first out-of-order
element is returned. The first term on a file has position 1.

# `keymerge`

```elixir
-spec keymerge(KeyPos, FileNames, Output) -> Reply
                  when
                      KeyPos :: key_pos(),
                      FileNames :: file_names(),
                      Output :: output(),
                      Reply :: ok | {error, reason()} | output_reply().
```

# `keymerge`

```elixir
-spec keymerge(KeyPos, FileNames, Output, Options) -> Reply
                  when
                      KeyPos :: key_pos(),
                      FileNames :: file_names(),
                      Output :: output(),
                      Options :: options(),
                      Reply :: ok | {error, reason()} | output_reply().
```

Merges tuples on files. Each input file is assumed to be sorted on key(s).

# `keysort`

```elixir
-spec keysort(KeyPos, FileName) -> Reply
                 when
                     KeyPos :: key_pos(),
                     FileName :: file_name(),
                     Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

Sorts tuples on files.

# `keysort`

```elixir
-spec keysort(KeyPos, Input, Output) -> Reply
                 when
                     KeyPos :: key_pos(),
                     Input :: input(),
                     Output :: output(),
                     Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

# `keysort`

```elixir
-spec keysort(KeyPos, Input, Output, Options) -> Reply
                 when
                     KeyPos :: key_pos(),
                     Input :: input(),
                     Output :: output(),
                     Options :: options(),
                     Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

Sorts tuples on files. The sort is performed on the element(s) mentioned in
`KeyPos`. If two tuples compare equal (`==`) on one element, the next element
according to `KeyPos` is compared. The sort is stable.

# `merge`

```elixir
-spec merge(FileNames, Output) -> Reply
               when
                   FileNames :: file_names(),
                   Output :: output(),
                   Reply :: ok | {error, reason()} | output_reply().
```

# `merge`

```elixir
-spec merge(FileNames, Output, Options) -> Reply
               when
                   FileNames :: file_names(),
                   Output :: output(),
                   Options :: options(),
                   Reply :: ok | {error, reason()} | output_reply().
```

Merges terms on files. Each input file is assumed to be sorted.

# `sort`

```elixir
-spec sort(FileName) -> Reply
              when
                  FileName :: file_name(),
                  Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

Sorts terms on files.

# `sort`

```elixir
-spec sort(Input, Output) -> Reply
              when
                  Input :: input(),
                  Output :: output(),
                  Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

# `sort`

```elixir
-spec sort(Input, Output, Options) -> Reply
              when
                  Input :: input(),
                  Output :: output(),
                  Options :: options(),
                  Reply :: ok | {error, reason()} | input_reply() | output_reply().
```

Sorts terms on files.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
