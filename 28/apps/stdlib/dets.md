# `dets`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/stdlib/src/dets.erl#L22)

A disk-based term storage.

This module provides a term storage on file. The stored terms, in this module
called _objects_, are tuples such that one element is defined to be the key. A
Dets _table_ is a collection of objects with the key at the same position stored
on a file.

This module is used by the Mnesia application, and is provided "as is" for users
who are interested in efficient storage of Erlang terms on disk only. Many
applications only need to store some terms in a file. Mnesia adds transactions,
queries, and distribution. The size of Dets files cannot exceed 2 GB. If larger
tables are needed, table fragmentation in Mnesia can be used.

Three types of Dets tables exist:

- `set`. A table of this type has at most one object with a given key. If an
  object with a key already present in the table is inserted, the existing
  object is overwritten by the new object.
- `bag`. A table of this type has zero or more different objects with a given
  key.
- `duplicate_bag`. A table of this type has zero or more possibly matching
  objects with a given key.

Dets tables must be opened before they can be updated or read, and when finished
they must be properly closed. If a table is not properly closed, Dets
automatically repairs the table. This can take a substantial time if the table
is large. A Dets table is closed when the process which opened the table
terminates. If many Erlang processes (users) open the same Dets table, they
share the table. The table is properly closed when all users have either
terminated or closed the table. Dets tables are not properly closed if the
Erlang runtime system terminates abnormally.

> #### Note {: .info }
>
> A `^C` command abnormally terminates an Erlang runtime system in a Unix
> environment with a break-handler.

As all operations performed by Dets are disk operations, it is important to
realize that a single look-up operation involves a series of disk seek and read
operations. The Dets functions are therefore much slower than the corresponding
`m:ets` functions, although Dets exports a similar interface.

Dets organizes data as a linear hash list and the hash list grows gracefully as
more data is inserted into the table. Space management on the file is performed
by what is called a buddy system. The current implementation keeps the entire
buddy system in RAM, which implies that if the table gets heavily fragmented,
quite some memory can be used up. The only way to defragment a table is to close
it and then open it again with option `repair` set to `force`.

Notice that type `ordered_set` in Ets is not yet provided by Dets, neither is
the limited support for concurrent updates that makes a sequence of `first` and
`next` calls safe to use on fixed ETS tables. Both these features may be
provided by Dets in a future release of Erlang/OTP. Until then, the Mnesia
application (or some user-implemented method for locking) must be used to
implement safe concurrency. Currently, no Erlang/OTP library has support for
ordered disk-based term storage.

All Dets functions return `{error, Reason}` if an error occurs (`first/1` and
`next/2` are exceptions, they exit the process with the error tuple). If badly
formed arguments are specified, all functions exit the process with a `badarg`
message.

## See Also

`m:ets`, `m:mnesia`, `m:qlc`

# `access`
*not exported* 

```elixir
-type access() :: read | read_write.
```

# `auto_save`
*not exported* 

```elixir
-type auto_save() :: timeout().
```

# `bindings_cont`

```elixir
-opaque bindings_cont() ::
            #dets_cont{what :: undefined | bchunk | bindings | object | select,
                       no_objs :: default | pos_integer(),
                       bin :: eof | binary(),
                       alloc :: binary() | {From :: non_neg_integer(), To :: non_neg_integer, binary()},
                       tab :: tab_name(),
                       proc :: undefined | pid(),
                       match_program :: true | undefined | {match_spec, ets:compiled_match_spec()}}.
```

Opaque continuation used by `match/1` and `match/3`.

# `cont`

```elixir
-opaque cont() ::
            #dets_cont{what :: undefined | bchunk | bindings | object | select,
                       no_objs :: default | pos_integer(),
                       bin :: eof | binary(),
                       alloc :: binary() | {From :: non_neg_integer(), To :: non_neg_integer, binary()},
                       tab :: tab_name(),
                       proc :: undefined | pid(),
                       match_program :: true | undefined | {match_spec, ets:compiled_match_spec()}}.
```

Opaque continuation used by `bchunk/2`.

# `keypos`
*not exported* 

```elixir
-type keypos() :: pos_integer().
```

# `match_spec`
*not exported* 

```elixir
-type match_spec() :: ets:match_spec().
```

Match specifications, see section
[Match Specification in Erlang](`e:erts:match_spec.md`) in ERTS User's Guide and
the `m:ms_transform` module.

# `no_slots`
*not exported* 

```elixir
-type no_slots() :: default | non_neg_integer().
```

# `object`
*not exported* 

```elixir
-type object() :: tuple().
```

# `object_cont`

```elixir
-opaque object_cont() ::
            #dets_cont{what :: undefined | bchunk | bindings | object | select,
                       no_objs :: default | pos_integer(),
                       bin :: eof | binary(),
                       alloc :: binary() | {From :: non_neg_integer(), To :: non_neg_integer, binary()},
                       tab :: tab_name(),
                       proc :: undefined | pid(),
                       match_program :: true | undefined | {match_spec, ets:compiled_match_spec()}}.
```

Opaque continuation used by `match_object/1` and `match_object/3`.

# `pattern`
*not exported* 

```elixir
-type pattern() :: atom() | tuple().
```

For a description of patterns, see `ets:match/2`.

# `select_cont`

```elixir
-opaque select_cont() ::
            #dets_cont{what :: undefined | bchunk | bindings | object | select,
                       no_objs :: default | pos_integer(),
                       bin :: eof | binary(),
                       alloc :: binary() | {From :: non_neg_integer(), To :: non_neg_integer, binary()},
                       tab :: tab_name(),
                       proc :: undefined | pid(),
                       match_program :: true | undefined | {match_spec, ets:compiled_match_spec()}}.
```

Opaque continuation used by `select/1` and `select/3`.

# `tab_name`

```elixir
-type tab_name() :: term().
```

# `type`
*not exported* 

```elixir
-type type() :: bag | duplicate_bag | set.
```

# `all`

```elixir
-spec all() -> [tab_name()].
```

Returns a list of the names of all open tables on this node.

# `bchunk`

```elixir
-spec bchunk(Name, Continuation) -> {Continuation2, Data} | '$end_of_table' | {error, Reason}
                when
                    Name :: tab_name(),
                    Continuation :: start | cont(),
                    Continuation2 :: cont(),
                    Data :: binary() | tuple(),
                    Reason :: term().
```

Returns a list of objects stored in a table. The exact representation of the
returned objects is not public.

The lists of data can be used for initializing a table by specifying value
`bchunk` to option `format` of function `init_table/3`. The Mnesia application
uses this function for copying open tables.

Unless the table is protected using [`safe_fixtable/2`](`safe_fixtable/2`),
calls to [`bchunk/2`](`bchunk/2`) do possibly not work as expected if concurrent
updates are made to the table.

The first time [`bchunk/2`](`bchunk/2`) is called, an initial continuation, the
atom `start`, must be provided.

[`bchunk/2`](`bchunk/2`) returns a tuple `{Continuation2, Data}`, where `Data`
is a list of objects. `Continuation2` is another continuation that is to be
passed on to a subsequent call to [`bchunk/2`](`bchunk/2`). With a series of
calls to [`bchunk/2`](`bchunk/2`), all table objects can be extracted.

[`bchunk/2`](`bchunk/2`) returns `'$end_of_table'` when all objects are
returned, or `{error, Reason}` if an error occurs.

# `close`

```elixir
-spec close(Name) -> ok | {error, Reason} when Name :: tab_name(), Reason :: term().
```

Closes a table. Only processes that have opened a table are allowed to close it.

All open tables must be closed before the system is stopped. If an attempt is
made to open a table that is not properly closed, Dets automatically tries to
repair it.

# `delete`

```elixir
-spec delete(Name, Key) -> ok | {error, Reason} when Name :: tab_name(), Key :: term(), Reason :: term().
```

Deletes all objects with key `Key` from table `Name`.

# `delete_all_objects`

```elixir
-spec delete_all_objects(Name) -> ok | {error, Reason} when Name :: tab_name(), Reason :: term().
```

Deletes all objects from a table in almost constant time. However, if the table
if fixed, [`delete_all_objects(T)`](`delete_all_objects/1`) is equivalent to
[`match_delete(T, '_')`](`match_delete/2`).

# `delete_object`

```elixir
-spec delete_object(Name, Object) -> ok | {error, Reason}
                       when Name :: tab_name(), Object :: object(), Reason :: term().
```

Deletes all instances of a specified object from a table. If a table is of type
`bag` or `duplicate_bag`, this function can be used to delete only some of the
objects with a specified key.

# `first`

```elixir
-spec first(Name) -> Key | '$end_of_table' when Name :: tab_name(), Key :: term().
```

Returns the first key stored in table `Name` according to the internal order of
the table, or `'$end_of_table'` if the table is empty.

Unless the table is protected using [`safe_fixtable/2`](`safe_fixtable/2`),
subsequent calls to `next/2` do possibly not work as expected if concurrent
updates are made to the table.

If an error occurs, the process is exited with an error tuple `{error, Reason}`.
The error tuple is not returned, as it cannot be distinguished from a key.

There are two reasons why [`first/1`](`first/1`) and [`next/2`](`next/2`) are
not to be used: they are not efficient, and they prevent the use of key
`'$end_of_table'`, as this atom is used to indicate the end of the table. If
possible, use functions [`match`](`match/1`),
[`match_object`](`match_object/1`), and [`select`](`select/1`) for traversing
tables.

# `foldl`

```elixir
-spec foldl(Function, Acc0, Name) -> Acc | {error, Reason}
               when
                   Name :: tab_name(),
                   Function :: fun((Object :: object(), AccIn) -> AccOut),
                   Acc0 :: term(),
                   Acc :: term(),
                   AccIn :: term(),
                   AccOut :: term(),
                   Reason :: term().
```

# `foldr`

```elixir
-spec foldr(Function, Acc0, Name) -> Acc | {error, Reason}
               when
                   Name :: tab_name(),
                   Function :: fun((Object :: object(), AccIn) -> AccOut),
                   Acc0 :: term(),
                   Acc :: term(),
                   AccIn :: term(),
                   AccOut :: term(),
                   Reason :: term().
```

Calls `Function` on successive elements of table `Name` together with an extra
argument `AccIn`. The table elements are traversed in unspecified order.
`Function` must return a new accumulator that is passed to the next call. `Acc0`
is returned if the table is empty.

# `from_ets`

```elixir
-spec from_ets(Name, EtsTab) -> ok | {error, Reason}
                  when Name :: tab_name(), EtsTab :: ets:table(), Reason :: term().
```

Deletes all objects of table `Name` and then inserts all the objects of the ETS
table `EtsTab`. The objects are inserted in unspecified order. As
`ets:safe_fixtable/2` is called, the ETS table must be public or owned by the
calling process.

# `info`

```elixir
-spec info(Name) -> InfoList | undefined
              when
                  Name :: tab_name(),
                  InfoList :: [InfoTuple],
                  InfoTuple ::
                      {file_size, non_neg_integer()} |
                      {filename, file:name()} |
                      {keypos, keypos()} |
                      {size, non_neg_integer()} |
                      {type, type()}.
```

Returns information about table `Name` as a list of tuples:

- `{file_size, integer() >= 0}}` \- The file size, in bytes.
- `{filename,` `t:file:name/0` `}` \- The name of the file where objects are
  stored.
- `{keypos,` `t:keypos/0` `}` \- The key position.
- `{size, integer() >= 0}` \- The number of objects stored in the table.
- `{type,` `t:type/0` `}` \- The table type.

# `info`

```elixir
-spec info(Name, Item) -> Value | undefined
              when
                  Name :: tab_name(),
                  Item ::
                      access | auto_save | bchunk_format | hash | file_size | filename | keypos |
                      memory | no_keys | no_objects | no_slots | owner | ram_file | safe_fixed |
                      safe_fixed_monotonic_time | size | type,
                  Value :: term().
```

Returns the information associated with `Item` for table `Name`. In addition to
the `{Item, Value}` pairs defined for `info/1`, the following items are allowed:

- `{access,` `t:access/0` `}` \- The access mode.
- `{auto_save,` `t:auto_save/0` `}` \- The autosave interval.
- `{bchunk_format, binary()}` \- An opaque binary describing the format of the
  objects returned by [`bchunk/2`](`bchunk/2`). The binary can be used as
  argument to
  [`is_compatible_bchunk_format/2`](`is_compatible_bchunk_format/2`).
- `{hash, Hash}` \- Describes which BIF is used to calculate the hash values of
  the objects stored in the Dets table. Possible values of `Hash`:

  - `phash` \- Implies that the `erlang:phash/2` BIF is used.
  - `phash2` \- Implies that the `erlang:phash2/1` BIF is used.

- `{memory, integer() >= 0}` \- The file size, in bytes. The same value is
  associated with item `file_size`.
- `{no_keys, integer >= 0()}` \- The number of different keys stored in the
  table.
- `{no_objects, integer >= 0()}` \- The number of objects stored in the table.
- `{no_slots, {Min, Used, Max}}` \- The number of slots of the table. `Min` is
  the minimum number of slots, `Used` is the number of currently used slots, and
  `Max` is the maximum number of slots.
- `{owner, pid()}` \- The pid of the process that handles requests to the Dets
  table.
- `{ram_file, boolean()}` \- Whether the table is kept in RAM.
- `{safe_fixed_monotonic_time, SafeFixed}` \- If the table is fixed, `SafeFixed`
  is a tuple `{FixedAtTime, [{Pid,RefCount}]}`. `FixedAtTime` is the time when
  the table was first fixed, and `Pid` is the pid of the process that fixes the
  table `RefCount` times. There can be any number of processes in the list. If
  the table is not fixed, `SafeFixed` is the atom `false`.

  `FixedAtTime` corresponds to the result returned by `erlang:monotonic_time/0`
  at the time of fixation. The use of `safe_fixed_monotonic_time` is
  [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`).

- `{safe_fixed, SafeFixed}` \- The same as
  `{safe_fixed_monotonic_time, SafeFixed}` except the format and value of
  `FixedAtTime`.

  `FixedAtTime` corresponds to the result returned by `erlang:timestamp/0` at
  the time of fixation. Notice that when the system uses single or multi
  [time warp modes](`e:erts:time_correction.md#time-warp-modes`), this can
  produce strange results. This is because the use of `safe_fixed` is not
  [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`). Time warp
  safe code must use `safe_fixed_monotonic_time` instead.

# `init_table`

```elixir
-spec init_table(Name, InitFun) -> ok | {error, Reason}
                    when
                        Name :: tab_name(),
                        InitFun :: fun((Arg) -> Res),
                        Arg :: read | close,
                        Res :: end_of_input | {[object()], InitFun} | {Data, InitFun} | term(),
                        Reason :: term(),
                        Data :: binary() | tuple().
```

# `init_table`

```elixir
-spec init_table(Name, InitFun, Options) -> ok | {error, Reason}
                    when
                        Name :: tab_name(),
                        InitFun :: fun((Arg) -> Res),
                        Arg :: read | close,
                        Res :: end_of_input | {[object()], InitFun} | {Data, InitFun} | term(),
                        Options :: Option | [Option],
                        Option :: {min_no_slots, no_slots()} | {format, term | bchunk},
                        Reason :: term(),
                        Data :: binary() | tuple().
```

Replaces the existing objects of table `Name` with objects created by calling
the input function `InitFun`.

The reason for using this function rather than calling [`insert/2`](`insert/2`)
is that of efficiency. Notice that the input functions are called by the process
that handles requests to the Dets table, not by the calling process.

When called with argument `read`, function `InitFun` is assumed to return
`end_of_input` when there is no more input, or `{Objects, Fun}`, where `Objects`
is a list of objects and `Fun` is a new input function. Any other value `Value`
is returned as an error `{error, {init_fun, Value}}`. Each input function is
called exactly once, and if an error occurs, the last function is called with
argument `close`, the reply of which is ignored.

If the table type is `set` and more than one object exists with a given key, one
of the objects is chosen. This is not necessarily the last object with the given
key in the sequence of objects returned by the input functions. Avoid duplicate
keys, otherwise the file becomes unnecessarily fragmented. This holds also for
duplicated objects stored in tables of type `bag`.

It is important that the table has a sufficient number of slots for the objects.
If not, the hash list starts to grow when [`init_table/2`](`init_table/2`)
returns, which significantly slows down access to the table for a period of
time. The minimum number of slots is set by the [`open_file/2`](`open_file/2`)
option `min_no_slots` and returned by the [`info/2`](`info/2`) item `no_slots`.
See also option `min_no_slots` below.

Argument `Options` is a list of `{Key, Val}` tuples, where the following values
are allowed:

- `{min_no_slots, no_slots()}` \- Specifies the estimated number of different
  keys to be stored in the table. The [`open_file/2`](`open_file/2`) option with
  the same name is ignored, unless the table is created, in which case
  performance can be enhanced by supplying an estimate when initializing the
  table.
- `{format, Format}` \- Specifies the format of the objects returned by function
  `InitFun`. If `Format` is `term` (the default), `InitFun` is assumed to return
  a list of tuples. If `Format` is `bchunk`, `InitFun` is assumed to return
  `Data` as returned by `bchunk/2`. This option overrides option `min_no_slots`.

# `insert`

```elixir
-spec insert(Name, Objects) -> ok | {error, Reason}
                when Name :: tab_name(), Objects :: object() | [object()], Reason :: term().
```

Inserts one or more objects into the table `Name`. If there already exists an
object with a key matching the key of some of the given objects and the table
type is `set`, the old object will be replaced.

# `insert_new`

```elixir
-spec insert_new(Name, Objects) -> boolean() | {error, Reason}
                    when Name :: tab_name(), Objects :: object() | [object()], Reason :: term().
```

Inserts one or more objects into table `Name`. If there already exists some
object with a key matching the key of any of the specified objects, the table is
not updated and `false` is returned. Otherwise the objects are inserted and
`true` returned.

# `is_compatible_bchunk_format`

```elixir
-spec is_compatible_bchunk_format(Name, BchunkFormat) -> boolean()
                                     when Name :: tab_name(), BchunkFormat :: binary().
```

Returns `true` if it would be possible to initialize table `Name`, using
`init_table/3` with option `{format, bchunk}`, with objects read with `bchunk/2`
from some table `T`, such that calling [`info(T, bchunk_format)`](`info/2`)
returns `BchunkFormat`.

# `is_dets_file`

```elixir
-spec is_dets_file(Filename) -> boolean() | {error, Reason}
                      when Filename :: file:name(), Reason :: term().
```

Returns `true` if file `Filename` is a Dets table, otherwise `false`.

# `lookup`

```elixir
-spec lookup(Name, Key) -> Objects | {error, Reason}
                when Name :: tab_name(), Key :: term(), Objects :: [object()], Reason :: term().
```

Returns a list of all objects with key `Key` stored in table `Name`, for
example:

```erlang
2> dets:open_file(abc, [{type, bag}]).
{ok,abc}
3> dets:insert(abc, {1,2,3}).
ok
4> dets:insert(abc, {1,3,4}).
ok
5> dets:lookup(abc, 1).
[{1,2,3},{1,3,4}]
```

If the table type is `set`, the function returns either the empty list or a list
with one object, as there cannot be more than one object with a given key. If
the table type is `bag` or `duplicate_bag`, the function returns a list of
arbitrary length.

Notice that the order of objects returned is unspecified. In particular, the
order in which objects were inserted is not reflected.

# `match`

```elixir
-spec match(Continuation) -> {[Match], Continuation2} | '$end_of_table' | {error, Reason}
               when
                   Continuation :: bindings_cont(),
                   Continuation2 :: bindings_cont(),
                   Match :: [term()],
                   Reason :: term().
```

Matches some objects stored in a table and returns a non-empty list of the
bindings matching a specified pattern in some unspecified order. The table, the
pattern, and the number of objects that are matched are all defined by
`Continuation`, which has been returned by a previous call to
[`match/1`](`match/1`) or [`match/3`](`match/3`).

When all table objects are matched, `'$end_of_table'` is returned.

# `match`

```elixir
-spec match(Name, Pattern) -> [Match] | {error, Reason}
               when Name :: tab_name(), Pattern :: pattern(), Match :: [term()], Reason :: term().
```

Returns for each object of table `Name` that matches `Pattern` a list of
bindings in some unspecified order. For a description of patterns, see
`ets:match/2`. If the keypos'th element of `Pattern` is unbound, all table
objects are matched. If the keypos'th element is bound, only the objects with
the correct key are matched.

# `match`

```elixir
-spec match(Name, Pattern, N) -> {[Match], Continuation} | '$end_of_table' | {error, Reason}
               when
                   Name :: tab_name(),
                   Pattern :: pattern(),
                   N :: default | non_neg_integer(),
                   Continuation :: bindings_cont(),
                   Match :: [term()],
                   Reason :: term().
```

Matches some or all objects of table `Name` and returns a non-empty list of the
bindings that match `Pattern` in some unspecified order. For a description of
patterns, see `ets:match/2`.

A tuple of the bindings and a continuation is returned, unless the table is
empty, in which case `'$end_of_table'` is returned. The continuation is to be
used when matching further objects by calling `match/1`.

If the keypos'th element of `Pattern` is bound, all table objects are matched.
If the keypos'th element is unbound, all table objects are matched, `N` objects
at a time, until at least one object matches or the end of the table is reached.
The default, indicated by giving `N` the value `default`, is to let the number
of objects vary depending on the sizes of the objects. All objects with the same
key are always matched at the same time, which implies that more than N objects
can sometimes be matched.

The table is always to be protected using `safe_fixtable/2` before calling
[`match/3`](`match/3`), otherwise errors can occur when calling
[`match/1`](`match/1`).

# `match_delete`

```elixir
-spec match_delete(Name, Pattern) -> ok | {error, Reason}
                      when Name :: tab_name(), Pattern :: pattern(), Reason :: term().
```

Deletes all objects that match `Pattern` from table `Name`. For a description of
patterns, see `ets:match/2`.

If the keypos'th element of `Pattern` is bound, only the objects with the
correct key are matched.

# `match_object`

```elixir
-spec match_object(Continuation) -> {Objects, Continuation2} | '$end_of_table' | {error, Reason}
                      when
                          Continuation :: object_cont(),
                          Continuation2 :: object_cont(),
                          Objects :: [object()],
                          Reason :: term().
```

Returns a non-empty list of some objects stored in a table that match a given
pattern in some unspecified order. The table, the pattern, and the number of
objects that are matched are all defined by `Continuation`, which has been
returned by a previous call to [`match_object/1`](`match_object/1`) or
[`match_object/3`](`match_object/3`).

When all table objects are matched, `'$end_of_table'` is returned.

# `match_object`

```elixir
-spec match_object(Name, Pattern) -> Objects | {error, Reason}
                      when
                          Name :: tab_name(),
                          Pattern :: pattern(),
                          Objects :: [object()],
                          Reason :: term().
```

Returns a list of all objects of table `Name` that match `Pattern` in some
unspecified order. For a description of patterns, see `ets:match/2`.

If the keypos'th element of `Pattern` is unbound, all table objects are matched.
If the keypos'th element of `Pattern` is bound, only the objects with the
correct key are matched.

Using the `match_object` functions for traversing all table objects is more
efficient than calling [`first/1`](`first/1`) and [`next/2`](`next/2`) or
[`slot/2`](`slot/2`).

# `match_object`

```elixir
-spec match_object(Name, Pattern, N) -> {Objects, Continuation} | '$end_of_table' | {error, Reason}
                      when
                          Name :: tab_name(),
                          Pattern :: pattern(),
                          N :: default | non_neg_integer(),
                          Continuation :: object_cont(),
                          Objects :: [object()],
                          Reason :: term().
```

Matches some or all objects stored in table `Name` and returns a non-empty list
of the objects that match `Pattern` in some unspecified order. For a description
of patterns, see `ets:match/2`.

A list of objects and a continuation is returned, unless the table is empty, in
which case `'$end_of_table'` is returned. The continuation is to be used when
matching further objects by calling `match_object/1`.

If the keypos'th element of `Pattern` is bound, all table objects are matched.
If the keypos'th element is unbound, all table objects are matched, `N` objects
at a time, until at least one object matches or the end of the table is reached.
The default, indicated by giving `N` the value `default`, is to let the number
of objects vary depending on the sizes of the objects. All matching objects with
the same key are always returned in the same reply, which implies that more than
N objects can sometimes be returned.

The table is always to be protected using `safe_fixtable/2` before calling
[`match_object/3`](`match_object/3`), otherwise errors can occur when calling
[`match_object/1`](`match_object/1`).

# `member`

```elixir
-spec member(Name, Key) -> boolean() | {error, Reason}
                when Name :: tab_name(), Key :: term(), Reason :: term().
```

Works like `lookup/2`, but does not return the objects. Returns `true` if one or
more table elements has key `Key`, otherwise `false`.

# `next`

```elixir
-spec next(Name, Key1) -> Key2 | '$end_of_table' when Name :: tab_name(), Key1 :: term(), Key2 :: term().
```

Returns either the key following `Key1` in table `Name` according to the
internal order of the table, or `'$end_of_table'` if there is no next key.

If an error occurs, the process is exited with an error tuple `{error, Reason}`.

To find the first key in the table, use `first/1`.

# `open_file`

```elixir
-spec open_file(Filename) -> {ok, Reference} | {error, Reason}
                   when Filename :: file:name(), Reference :: reference(), Reason :: term().
```

Opens an existing table. If the table is not properly closed, it is repaired.
The returned reference is to be used as the table name. This function is most
useful for debugging purposes.

# `open_file`

```elixir
-spec open_file(Name, Args) -> {ok, Name} | {error, Reason}
                   when
                       Name :: tab_name(),
                       Args :: [OpenArg],
                       OpenArg ::
                           {access, access()} |
                           {auto_save, auto_save()} |
                           {estimated_no_objects, non_neg_integer()} |
                           {file, file:name()} |
                           {max_no_slots, no_slots()} |
                           {min_no_slots, no_slots()} |
                           {keypos, keypos()} |
                           {ram_file, boolean()} |
                           {repair, boolean() | force} |
                           {type, type()},
                       Reason :: term().
```

Opens a table. An empty Dets table is created if no file exists.

The atom `Name` is the table name. The table name must be provided in all
subsequent operations on the table. The name can be used by other processes as
well, and many processes can share one table.

If two processes open the same table by giving the same name and arguments, the
table has two users. If one user closes the table, it remains open until the
second user closes it.

Argument `Args` is a list of `{Key, Val}` tuples, where the following values are
allowed:

- `{access,` `t:access/0` `}` \- Existing tables can be opened in read-only mode.
  A table that is opened in read-only mode is not subjected to the automatic
  file reparation algorithm if it is later opened after a crash. Defaults to
  `read_write`.
- `{auto_save,` `t:auto_save/0` `}` \- The autosave interval. If the interval is
  an integer `Time`, the table is flushed to disk whenever it is not accessed
  for `Time` milliseconds. A table that has been flushed requires no reparation
  when reopened after an uncontrolled emulator halt. If the interval is the atom
  `infinity`, autosave is disabled. Defaults to 180000 (3 minutes).
- `{estimated_no_objects,` `t:no_slots/0` `}` \- Equivalent to option
  `min_no_slots`.
- `{file,` `t:file:name/0` `}` \- The name of the file to be opened. Defaults to
  the table name.
- `{max_no_slots,` `t:no_slots/0` `}` \- The maximum number of slots to be used.
  Defaults to 32 M, which is the maximal value. Notice that a higher value can
  increase the table fragmentation, and a smaller value can decrease the
  fragmentation, at the expense of execution time.
- `{min_no_slots,` `t:no_slots/0` `}` \- Application performance can be enhanced
  with this flag by specifying, when the table is created, the estimated number
  of different keys to be stored in the table. Defaults to 256, which is the
  minimum value.
- `{keypos,` `t:keypos/0` `}` \- The position of the element of each object to be
  used as key. Defaults to 1. The ability to explicitly state the key position
  is most convenient when we want to store Erlang records in which the first
  position of the record is the name of the record type.
- `{ram_file, boolean()}` \- Whether the table is to be kept in RAM. Keeping the
  table in RAM can sound like an anomaly, but can enhance the performance of
  applications that open a table, insert a set of objects, and then close the
  table. When the table is closed, its contents are written to the disk file.
  Defaults to `false`.
- `{repair, Value}` \- `Value` can be either a `t:boolean/0` or the atom
  `force`. The flag specifies if the Dets server is to invoke the automatic file
  reparation algorithm. Defaults to `true`. If `false` is specified, no attempt
  is made to repair the file, and `{error, {needs_repair, FileName}}` is
  returned if the table must be repaired.

  Value `force` means that a reparation is made even if the table is properly
  closed. This is a seldom needed option.

  Option `repair` is ignored if the table is already open.

- `{type,` `t:type/0` `}` \- The table type. Defaults to `set`.

# `pid2name`

```elixir
-spec pid2name(Pid) -> {ok, Name} | undefined when Pid :: pid(), Name :: tab_name().
```

Returns the table name given the pid of a process that handles requests to a
table, or `undefined` if there is no such table.

This function is meant to be used for debugging only.

# `repair_continuation`

```elixir
-spec repair_continuation(Continuation, MatchSpec) -> Continuation2
                             when
                                 Continuation :: select_cont(),
                                 Continuation2 :: select_cont(),
                                 MatchSpec :: match_spec().
```

This function can be used to restore an opaque continuation returned by
`select/3` or `select/1` if the continuation has passed through external term
format (been sent between nodes or stored on disk).

The reason for this function is that continuation terms contain compiled match
specifications and therefore are invalidated if converted to external term
format. Given that the original match specification is kept intact, the
continuation can be restored, meaning it can once again be used in subsequent
[`select/1`](`select/1`) calls even though it has been stored on disk or on
another node.

For more information and examples, see the `m:ets` module.

> #### Note {: .info }
>
> This function is rarely needed in application code. It is used by application
> Mnesia to provide distributed [`select/3`](`select/3`) and
> [`select/1`](`select/1`) sequences. A normal application would either use
> Mnesia or keep the continuation from being converted to external format.
>
> The reason for not having an external representation of compiled match
> specifications is performance. It can be subject to change in future releases,
> while this interface remains for backward compatibility.

# `safe_fixtable`

```elixir
-spec safe_fixtable(Name, Fix) -> ok when Name :: tab_name(), Fix :: boolean().
```

If `Fix` is `true`, table `Name` is fixed (once more) by the calling process,
otherwise the table is released. The table is also released when a fixing
process terminates.

If many processes fix a table, the table remains fixed until all processes have
released it or terminated. A reference counter is kept on a per process basis,
and N consecutive fixes require N releases to release the table.

It is not guaranteed that calls to [`first/1`](`first/1`), [`next/2`](`next/2`),
or select and match functions work as expected even if the table is fixed; the
limited support for concurrency provided by the `m:ets` module is not yet
provided by Dets. Fixing a table currently only disables resizing of the hash
list of the table.

If objects have been added while the table was fixed, the hash list starts to
grow when the table is released, which significantly slows down access to the
table for a period of time.

# `select`

```elixir
-spec select(Continuation) -> {Selection, Continuation2} | '$end_of_table' | {error, Reason}
                when
                    Continuation :: select_cont(),
                    Continuation2 :: select_cont(),
                    Selection :: [term()],
                    Reason :: term().
```

Applies a match specification to some objects stored in a table and returns a
non-empty list of the results. The table, the match specification, and the
number of objects that are matched are all defined by `Continuation`, which is
returned by a previous call to `select/1` or `select/3`.

When all objects of the table have been matched, `'$end_of_table'` is returned.

# `select`

```elixir
-spec select(Name, MatchSpec) -> Selection | {error, Reason}
                when
                    Name :: tab_name(),
                    MatchSpec :: match_spec(),
                    Selection :: [term()],
                    Reason :: term().
```

Returns the results of applying match specification `MatchSpec` to all or some
objects stored in table `Name`. The order of the objects is not specified. For a
description of match specifications, see the
[ERTS User's Guide](`e:erts:match_spec.md`).

If the keypos'th element of `MatchSpec` is unbound, the match specification is
applied to all objects of the table. If the keypos'th element is bound, the
match specification is applied to the objects with the correct key(s) only.

Using the `select` functions for traversing all objects of a table is more
efficient than calling [`first/1`](`first/1`) and [`next/2`](`next/2`) or
[`slot/2`](`slot/2`).

# `select`

```elixir
-spec select(Name, MatchSpec, N) -> {Selection, Continuation} | '$end_of_table' | {error, Reason}
                when
                    Name :: tab_name(),
                    MatchSpec :: match_spec(),
                    N :: default | non_neg_integer(),
                    Continuation :: select_cont(),
                    Selection :: [term()],
                    Reason :: term().
```

Returns the results of applying match specification `MatchSpec` to some or all
objects stored in table `Name`. The order of the objects is not specified. For a
description of match specifications, see the
[ERTS User's Guide](`e:erts:match_spec.md`).

A tuple of the results of applying the match specification and a continuation is
returned, unless the table is empty, in which case `'$end_of_table'` is
returned. The continuation is to be used when matching more objects by calling
`select/1`.

If the keypos'th element of `MatchSpec` is bound, the match specification is
applied to all objects of the table with the correct key(s). If the keypos'th
element of `MatchSpec` is unbound, the match specification is applied to all
objects of the table, `N` objects at a time, until at least one object matches
or the end of the table is reached. The default, indicated by giving `N` the
value `default`, is to let the number of objects vary depending on the sizes of
the objects. All objects with the same key are always handled at the same time,
which implies that the match specification can be applied to more than N
objects.

The table is always to be protected using `safe_fixtable/2` before calling
[`select/3`](`select/3`), otherwise errors can occur when calling
[`select/1`](`select/1`).

# `select_delete`

```elixir
-spec select_delete(Name, MatchSpec) -> N | {error, Reason}
                       when
                           Name :: tab_name(),
                           MatchSpec :: match_spec(),
                           N :: non_neg_integer(),
                           Reason :: term().
```

Deletes each object from table `Name` such that applying match specification
`MatchSpec` to the object returns value `true`. For a description of match
specifications, see the [ERTS User's Guide](`e:erts:match_spec.md`). Returns the
number of deleted objects.

If the keypos'th element of `MatchSpec` is bound, the match specification is
applied to the objects with the correct key(s) only.

# `slot`

```elixir
-spec slot(Name, I) -> '$end_of_table' | Objects | {error, Reason}
              when Name :: tab_name(), I :: non_neg_integer(), Objects :: [object()], Reason :: term().
```

The objects of a table are distributed among slots, starting with slot `0` and
ending with slot `n`. Returns the list of objects associated with slot `I`. If
`I` > `n`, `'$end_of_table'` is returned.

# `sync`

```elixir
-spec sync(Name) -> ok | {error, Reason} when Name :: tab_name(), Reason :: term().
```

Ensures that all updates made to table `Name` are written to disk. This also
applies to tables that have been opened with flag `ram_file` set to `true`. In
this case, the contents of the RAM file are flushed to disk.

Notice that the space management data structures kept in RAM, the buddy system,
is also written to the disk. This can take some time if the table is fragmented.

# `table`

```elixir
-spec table(Name) -> QueryHandle when Name :: tab_name(), QueryHandle :: qlc:query_handle().
```

# `table`

```elixir
-spec table(Name, Options) -> QueryHandle
               when
                   Name :: tab_name(),
                   Options :: Option | [Option],
                   Option :: {n_objects, Limit} | {traverse, TraverseMethod},
                   Limit :: default | pos_integer(),
                   TraverseMethod :: first_next | select | {select, match_spec()},
                   QueryHandle :: qlc:query_handle().
```

Returns a Query List Comprehension (QLC) query handle. The `m:qlc` module
provides a query language aimed mainly for Mnesia, but ETS tables, Dets tables,
and lists are also recognized by `qlc` as sources of data. Calling
[`dets:table/1,2`](`table/1`) is the means to make Dets table `Name` usable to
`qlc`.

When there are only simple restrictions on the key position, `qlc` uses
[`dets:lookup/2`](`lookup/2`) to look up the keys. When that is not possible,
the whole table is traversed. Option `traverse` determines how this is done:

- `first_next` \- The table is traversed one key at a time by calling
  `dets:first/1` and `dets:next/2`.
- `select` \- The table is traversed by calling [`dets:select/3`](`select/3`)
  and [`dets:select/1`](`select/1`). Option `n_objects` determines the number of
  objects returned (the third argument of [`select/3`](`select/3`)). The match
  specification (the second argument of [`select/3`](`select/3`)) is assembled
  by `qlc`:

  - Simple filters are translated into equivalent match specifications.
  - More complicated filters must be applied to all objects returned by
    [`select/3`](`select/3`) given a match specification that matches all
    objects.

- `{select,` `t:match_spec/0` `}` \- As for `select`, the table is traversed by
  calling `dets:select/3` and `dets:select/1`. The difference is that the match
  specification is specified explicitly. This is how to state match
  specifications that cannot easily be expressed within the syntax provided by
  `qlc`.

The following example uses an explicit match specification to traverse the
table:

```erlang
1> dets:open_file(t, []),
ok = dets:insert(t, [{1,a},{2,b},{3,c},{4,d}]),
MS = ets:fun2ms(fun({X,Y}) when (X > 1) or (X < 5) -> {Y} end),
QH1 = dets:table(t, [{traverse, {select, MS}}]).
```

An example with implicit match specification:

```erlang
2> QH2 = qlc:q([{Y} || {X,Y} <- dets:table(t), (X > 1) or (X < 5)]).
```

The latter example is equivalent to the former, which can be verified using
function `qlc:info/1`:

```erlang
3> qlc:info(QH1) =:= qlc:info(QH2).
true
```

`qlc:info/1` returns information about a query handle. In this case identical
information is returned for the two query handles.

# `to_ets`

```elixir
-spec to_ets(Name, EtsTab) -> EtsTab | {error, Reason}
                when Name :: tab_name(), EtsTab :: ets:table(), Reason :: term().
```

Inserts the objects of the Dets table `Name` into the ETS table `EtsTab`. The
order in which the objects are inserted is not specified. The existing objects
of the ETS table are kept unless overwritten.

# `traverse`

```elixir
-spec traverse(Name, Fun) -> Return | {error, Reason}
                  when
                      Name :: tab_name(),
                      Fun :: fun((Object) -> FunReturn),
                      Object :: object(),
                      FunReturn :: continue | {continue, Val} | {done, Value} | OtherValue,
                      Return :: [term()] | OtherValue,
                      Val :: term(),
                      Value :: term(),
                      OtherValue :: term(),
                      Reason :: term().
```

Applies `Fun` to each object stored in table `Name` in some unspecified order.
Different actions are taken depending on the return value of `Fun`. The
following `Fun` return values are allowed:

- **`continue`** - Continue to perform the traversal. For example, the following
  function can be used to print the contents of a table:

  ```erlang
  fun(X) -> io:format("~p~n", [X]), continue end.
  ```

- **`{continue, Val}`** - Continue the traversal and accumulate `Val`. The
  following function is supplied to collect all objects of a table in a list:

  ```text
  fun(X) -> {continue, X} end.
  ```

- **`{done, Value}`** - Terminate the traversal and return `[Value | Acc]`.

Any other value `OtherValue` returned by `Fun` terminates the traversal and is
returned immediately.

# `update_counter`

```elixir
-spec update_counter(Name, Key, Increment) -> Result
                        when
                            Name :: tab_name(),
                            Key :: term(),
                            Increment :: {Pos, Incr} | Incr,
                            Pos :: integer(),
                            Incr :: integer(),
                            Result :: integer().
```

Updates the object with key `Key` stored in table `Name` of type `set` by adding
`Incr` to the element at the `Pos`:th position. The new counter value is
returned. If no position is specified, the element directly following the key is
updated.

This functions provides a way of updating a counter, without having to look up
an object, update the object by incrementing an element, and insert the
resulting object into the table again.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
