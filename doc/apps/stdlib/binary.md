# `binary`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/binary.erl#L22)

Library for handling binary data.

This module provides functions for manipulating byte-oriented
binaries. While most of these functions could be implemented using the
bit syntax, the functions in this module are highly optimized and are
expected to either execute faster or consume less memory, or both,
compared to equivalent implementations written in pure Erlang.

The module is provided according to [EEP 31: Binary manipulation and
searching module](https://www.erlang.org/eeps/eep-0031).

> #### Note {: .info }
>
> This module handles byte-oriented data. For bitstrings that are not binaries
> (does not contain whole octets of bits) a `badarg` exception is raised from
> any of the functions in this module.

# `cp`
*since OTP R14B* 

```erlang
-opaque cp()
```

Opaque data type representing a compiled search pattern.

Guaranteed to be a `t:tuple/0` to allow programs to distinguish it from
non-precompiled search patterns.

# `part`
*since OTP R14B* 

```erlang
-type part() :: {Start :: non_neg_integer(), Length :: integer()}.
```

A representation of a part (or range) in a binary. `Start` is a zero-based
offset into a `t:binary/0` and `Length` is the length of that part.

As input to functions in this module, a reverse part specification is
allowed, constructed with a negative `Length`, so that the part of the
binary begins at `Start` \+ `Length` and is -`Length` long. This is
useful for referencing the last `N` bytes of a binary as
`{size(Binary), -N}`. The functions in this module always return
`t:part/0`s with positive `Length`.

# `at`
*since OTP R14B* 

```erlang
-spec at(Subject, Pos) -> byte() when Subject :: binary(), Pos :: non_neg_integer().
```

Returns the byte at position `Pos` (zero-based) in binary `Subject` as an
integer.

If `Pos` >= [`byte_size(Subject)`](`byte_size/1`), a `badarg` exception
is raised.

## Examples

```erlang
1> binary:at(<<5,19,72,33>>, 0).
5
2> binary:at(<<5,19,72,33>>, 1).
19
3> binary:at(<<5,19,72,33>>, 4).
** exception error: bad argument
     in function  binary:at/2
        called as binary:at(<<5,19,72,33>>,4)
```

# `bin_to_list`
*since OTP R14B* 

```erlang
-spec bin_to_list(Subject) -> [byte()] when Subject :: binary().
```

# `bin_to_list`
*since OTP R14B* 

```erlang
-spec bin_to_list(Subject, PosLen) -> [byte()] when Subject :: binary(), PosLen :: part().
```

Converts part of `Subject` to a list of `t:byte/0`s, each representing
the value of one byte.

`Pos` and `Len` denote which part of the `Subject` binary to convert.

## Examples

```erlang
1> binary:bin_to_list(<<"erlang",0>>).
[101,114,108,97,110,103,0]
2> binary:bin_to_list(<<"erlang">>, 1, 3).
"rla"
3> binary:bin_to_list(<<"erlang">>, {1, 3}).
"rla"
%% or [114,108,97] in list notation.
4> binary:bin_to_list(<<"erlang">>, 5, 3).
** exception error: bad argument
     in function  binary:bin_to_list/3
        called as binary:bin_to_list(<<"erlang">>,5,3)
        *** argument 3: out of range
```

If `Pos` and `Len` reference outside the binary in any way, a `badarg`
exception is raised.

# `bin_to_list`
*since OTP R14B* 

```erlang
-spec bin_to_list(Subject, Pos, Len) -> [byte()]
                     when Subject :: binary(), Pos :: non_neg_integer(), Len :: integer().
```

# `compile_pattern`
*since OTP R14B* 

```erlang
-spec compile_pattern(Pattern) -> cp()
                         when
                             Pattern :: PatternBinary | [PatternBinary, ...],
                             PatternBinary :: nonempty_binary().
```

Builds an internal structure representing a compilation of a search pattern,
later to be used in functions `match/3`, `matches/3`, `split/3`, or `replace/4`.

The `t:cp/0` returned is guaranteed to be a `t:tuple/0` to allow programs to
distinguish it from non-precompiled search patterns.

When a list of binaries is specified, it denotes a set of alternative binaries
to search for. For example, if `[<<"functional">>,<<"programming">>]` is
specified as `Pattern`, this means either `<<"functional">>` or
`<<"programming">>`". The pattern is a set of alternatives; when only a single
binary is specified, the set has only one element. The order of alternatives in
a pattern is not significant.

The list of binaries used for search alternatives must be flat, proper, and
non-empty.

If `Pattern` is not a binary or a flat proper non-empty list of binaries with
length greater than 0, a `badarg` exception is raised.

## Examples

```erlang
1> Pat = binary:compile_pattern(~"rain").
2> binary:match(~"the rain in spain", Pat).
{4,4}
```

# `copy`
*since OTP R14B* 

```erlang
-spec copy(Subject) -> binary() when Subject :: binary().
```

Creates a copy of `Subject`.

Using `copy/1` on a binary that references a larger binary can
potentially free up the larger binary for garbage collection.

> #### Note {: .info }
>
> Deliberately copying a single binary to avoid referencing a larger
> binary does not necessarily free up the larger binary for garbage
> collection. Instead, it can lead to the creation of significantly
> more binary data than needed. In general, sharing binary data is
> beneficial.
>
> Only in special cases — when small parts reference large binaries and the large
> binaries are no longer used in any process — can deliberate copying be
> beneficial.

## Examples

```erlang
1> HugeBinary = <<0:100_000/unit:8>>.
2> byte_size(HugeBinary).
100000
3> Part = binary:part(HugeBinary, 0, 5).
<<0,0,0,0,0>>
4> Copy = binary:copy(Part).
<<0,0,0,0,0>>
```

# `copy`
*since OTP R14B* 

```erlang
-spec copy(Subject, N) -> binary() when Subject :: binary(), N :: non_neg_integer().
```

Creates a binary with the content of `Subject` duplicated `N` times.

This function always creates a new binary, even when `N` is `1`.

## Examples

```erlang
1> binary:copy(~"-", 10).
<<"----------">>
```

# `decode_hex`
*since OTP 24.0* 

```erlang
-spec decode_hex(Bin) -> Bin2 when Bin :: <<_:_*16>>, Bin2 :: binary().
```

Decodes a hex-encoded binary into a binary.

An exception is raised if the size of the binary is not evenly divisble by two,
or if the binary contains any characters that do not represent hex digits.

## Examples

```erlang
1> binary:decode_hex(<<"666f6f">>).
<<"foo">>
2> binary:decode_hex(<<"A">>).
** exception error: bad argument
     in function  binary:decode_hex/1
        called as binary:decode_hex(<<"A">>)
        *** argument 1: must contain an even number of bytes
```

# `decode_unsigned`
*since OTP R14B* 

```erlang
-spec decode_unsigned(Subject) -> Unsigned when Subject :: binary(), Unsigned :: non_neg_integer().
```

# `decode_unsigned`
*since OTP R14B* 

```erlang
-spec decode_unsigned(Subject, Endianness) -> Unsigned
                         when
                             Subject :: binary(),
                             Endianness :: big | little,
                             Unsigned :: non_neg_integer().
```

Converts the binary digit representation, in big endian or little endian, of a
positive integer in `Subject` to an Erlang `t:integer/0`.

## Examples

```erlang
1> binary:decode_unsigned(<<7>>).
7
2> binary:decode_unsigned(<<1,0>>).
256
3> binary:decode_unsigned(<<169,138,199>>).
11111111
4> binary:decode_unsigned(<<169,138,199>>, big).
11111111
5> binary:decode_unsigned(<<169,138,199>>, little).
13077161
```

# `encode_hex`
*since OTP 24.0* 

```erlang
-spec encode_hex(Bin) -> Bin2 when Bin :: binary(), Bin2 :: <<_:_*16>>.
```

# `encode_hex`
*since OTP 26.0* 

```erlang
-spec encode_hex(Bin, Case) -> Bin2
                    when Bin :: binary(), Case :: lowercase | uppercase, Bin2 :: <<_:_*16>>.
```

Encodes a binary into a hex-encoded binary using the specified case for the
hexadecimal digits "a" to "f".

## Examples

```erlang
1> binary:encode_hex(<<"foo">>, uppercase).
<<"666F6F">>
2> binary:encode_hex(<<"/">>, uppercase).
<<"2F">>
3> binary:encode_hex(<<"/">>, lowercase).
<<"2f">>
```

# `encode_unsigned`
*since OTP R14B* 

```erlang
-spec encode_unsigned(Unsigned) -> binary() when Unsigned :: non_neg_integer().
```

# `encode_unsigned`
*since OTP R14B* 

```erlang
-spec encode_unsigned(Unsigned, Endianness) -> binary()
                         when Unsigned :: non_neg_integer(), Endianness :: big | little.
```

Converts a non-negative integer into the smallest possible unsigned binary
representation, using either big-endian or little-endian format.

If `Unsigned` is not a non-negative integer, a `badarg` exception is
raised.

## Examples

```erlang
1> binary:encode_unsigned(0, big).
<<0>>
2> binary:encode_unsigned(255, big).
<<255>>
3> binary:encode_unsigned(256, big).
<<1,0>>
4> binary:encode_unsigned(256, little).
<<0,1>>
5> binary:encode_unsigned(11111111, big).
<<169,138,199>>
6> binary:encode_unsigned(11111111, little).
<<199,138,169>>
```

# `first`
*since OTP R14B* 

```erlang
-spec first(Subject) -> byte() when Subject :: binary().
```

Returns the first byte of binary `Subject` as an integer.

If the size of `Subject` is zero, a `badarg` exception is raised.

## Examples

```erlang
1> binary:first(<<42,99,100>>).
42
2> binary:first(<<>>).
** exception error: bad argument
     in function  binary:first/1
        called as binary:first(<<>>)
        *** argument 1: a zero-sized binary is not allowed
```

# `join`
*since OTP 28.0* 

```erlang
-spec join([binary()], binary()) -> binary().
```

Joins a list of binaries together by a specified `Separator`.

Equivalent to `iolist_to_binary(lists:join(Separator, Binaries))`, but faster.

## Examples

```erlang
1> binary:join([<<"a">>, <<"b">>, <<"c">>], <<", ">>).
<<"a, b, c">>
```

# `last`
*since OTP R14B* 

```erlang
-spec last(Subject) -> byte() when Subject :: binary().
```

Returns the last byte of binary `Subject` as an integer.

If the size of `Subject` is zero, a `badarg` exception is raised.

## Examples

```erlang
1> binary:last(<<42,99,100>>).
100
2> binary:last(<<>>).
** exception error: bad argument
     in function  binary:last/1
        called as binary:last(<<>>)
        *** argument 1: a zero-sized binary is not allowed
```

# `list_to_bin`
*since OTP R14B* 

```erlang
-spec list_to_bin(ByteList) -> binary() when ByteList :: iolist().
```

Works exactly as `erlang:list_to_binary/1`.

This function is provided for completeness.

# `longest_common_prefix`
*since OTP R14B* 

```erlang
-spec longest_common_prefix(Binaries) -> non_neg_integer() when Binaries :: [binary(), ...].
```

Returns the length of the longest common prefix of the binaries in list
`Binaries`.

If `Binaries` is not a flat non-empty list of binaries, a `badarg`
exception is raised.

## Examples

```erlang
1> binary:longest_common_prefix([<<"erlang">>, <<"ergonomy">>]).
2
2> binary:longest_common_prefix([<<"erlang">>, <<"perl">>]).
0
```

# `longest_common_suffix`
*since OTP R14B* 

```erlang
-spec longest_common_suffix(Binaries) -> non_neg_integer() when Binaries :: [binary(), ...].
```

Returns the length of the longest common suffix of the binaries in list
`Binaries`.

If `Binaries` is not a flat non-empty list of binaries, a `badarg`
exception is raised.

## Examples

```erlang
1> binary:longest_common_suffix([<<"erlang">>, <<"fang">>]).
3
2> binary:longest_common_suffix([<<"erlang">>, <<"perl">>]).
0
```

# `match`
*since OTP R14B* 

```erlang
-spec match(Subject, Pattern) -> Found | nomatch
               when
                   Subject :: binary(),
                   Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                   PatternBinary :: nonempty_binary(),
                   Found :: part().
```

# `match`
*since OTP R14B* 

```erlang
-spec match(Subject, Pattern, Options) -> Found | nomatch
               when
                   Subject :: binary(),
                   Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                   PatternBinary :: nonempty_binary(),
                   Found :: part(),
                   Options :: [Option],
                   Option :: {scope, part()}.
```

Searches for the first occurrence of `Pattern` in `Subject` and returns the
position and length.

The function returns `{Pos, Length}` for the binary in `Pattern`, starting at
the lowest position in `Subject`.

Summary of the options:

- **\{scope, \{Start, Length\}\}** - Only the specified part is searched. Return
  values still have offsets from the beginning of `Subject`. A negative `Length`
  is allowed as described in [Types](`m:binary#types`).

If none of the strings in `Pattern` is found, the atom `nomatch` is returned.

For a description of `Pattern`, see `compile_pattern/1`.

If `{scope, {Start,Length}}` is specified in the options such that `Start` >
size of `Subject`, `Start` \+ `Length` < 0 or `Start` \+ `Length` > size of
`Subject`, a `badarg` exception is raised.

## Examples

```erlang
1> binary:match(<<"abcde">>, [<<"bcde">>, <<"cd">>], []).
{1,4}
```

Even though `<<"cd">>` ends before `<<"bcde">>`, `<<"bcde">>` begins first and
is therefore the first match. If two overlapping matches begin at the same
position, the longest is returned.

```erlang
1> binary:match(~"the rain in spain", ~"ain", []).
{5,3}
```

# `matches`
*since OTP R14B* 

```erlang
-spec matches(Subject, Pattern) -> Found
                 when
                     Subject :: binary(),
                     Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                     PatternBinary :: nonempty_binary(),
                     Found :: [part()].
```

# `matches`
*since OTP R14B* 

```erlang
-spec matches(Subject, Pattern, Options) -> Found
                 when
                     Subject :: binary(),
                     Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                     PatternBinary :: nonempty_binary(),
                     Found :: [part()],
                     Options :: [Option],
                     Option :: {scope, part()}.
```

As `match/2`, but `Subject` is searched until exhausted and a list of all
non-overlapping parts matching `Pattern` is returned (in order).

The first and longest match is preferred to a shorter, which is illustrated by
the following example:

```erlang
1> binary:matches(<<"abcde">>,
                  [<<"bcde">>,<<"bc">>,<<"de">>],
                  []).
[{1,4}]
```

The result shows that <<"bcde">> is selected instead of the shorter match
<<"bc">> (which would have resulted in one more match, <<"de">>). This
corresponds to the behavior of POSIX regular expressions (and programs such as
`awk`), but is not consistent with alternative matches in `m:re` (and Perl), where
lexical ordering in the search pattern determines which string matches.

If none of the strings in a pattern is found, an empty list is returned.

For a description of `Pattern`, see `compile_pattern/1`. For a description of
available options, see `match/3`.

If `{scope, {Start,Length}}` is specified in the options such that `Start` >
size of `Subject`, `Start + Length` < 0 or `Start + Length` is > size of
`Subject`, a `badarg` exception is raised.

## Examples

```erlang
1> binary:matches(~"the rain in spain", ~"ai", []).
[{5,2},{14,2}]
```

# `part`
*since OTP R14B* 

```erlang
-spec part(Subject, PosLen) -> binary() when Subject :: binary(), PosLen :: part().
```

# `part`
*since OTP R14B* 

```erlang
-spec part(Subject, Pos, Len) -> binary()
              when Subject :: binary(), Pos :: non_neg_integer(), Len :: integer().
```

Extracts the part of binary `Subject` described by `PosLen`.

A negative length can be used to extract bytes at the end of a binary.

> #### Note {: .info }
>
> `part/2` and `part/3` are also available in the `m:erlang` module under the
> names [`binary_part/2`](`binary_part/2`) and
> [`binary_part/3`](`binary_part/3`). Those BIFs are allowed in guard tests.

If `Pos` and `Len` in any way references outside the binary, a
`badarg` exception is raised.

## Examples

```erlang
1> Bin = <<1,2,3,4,5,6,7,8,9,10>>.
2> binary:part(Bin, 1, 3).
<<2,3,4>>
3> binary:part(Bin, byte_size(Bin), -5).
<<6,7,8,9,10>>
```

# `referenced_byte_size`
*since OTP R14B* 

```erlang
-spec referenced_byte_size(Binary) -> non_neg_integer() when Binary :: binary().
```

Get the size of the underlying binary referenced by `Binary`.

If a binary references a larger binary (often called a subbinary), it
can be useful to determine the size of the referenced binary. This
function can be used to decide when to trigger `copy/1`. Copying a
binary can help eliminate the reference to the original, potentially
large, binary that the smaller binary depends on.

> #### Note {: .info }
>
> Binary data is shared among processes. If another process still
> references the larger binary, copying only the part used by this
> process will consume more memory without freeing the larger binary for
> garbage collection. Use these intrusive functions with extreme care,
> and only when a real problem has been identified.

## Examples

```erlang
store(Binary, GBSet) ->
    NewBin =
        case binary:referenced_byte_size(Binary) of
            Large when Large > 2 * byte_size(Binary) ->
                binary:copy(Binary);
            _ ->
                Binary
        end,
    gb_sets:insert(NewBin, GBSet).
```

In this example, we choose to copy the binary content before inserting
it into [`gb_sets:set()`](`t:gb_sets:set/0`) if it references a binary
more than twice the size of the data we want to retain. Naturally,
different rules apply when copying in different applications.

Binary sharing occurs whenever binaries are taken apart. This is the
fundamental reason why binaries are efficient; decomposition always
has *O(1)* complexity. However, in rare circumstances this data
sharing is undesirable. In such situations, this function, along with
[`copy/1`](`copy/1`) can be useful for optimizing memory usage.

```erlang
1> A = binary:copy(<<1>>, 1000).
<<1,1,1,1,1,_/binary>>
2> byte_size(A).
1000
3> binary:referenced_byte_size(A).
1000
4> <<B:10/binary, C/binary>> = A.
5> {byte_size(B), binary:referenced_byte_size(B)}.
{10,10}
6> {byte_size(C), binary:referenced_byte_size(C)}.
{990,1000}
```

In the above example, the small binary `B` was copied, while the larger binary
`C` still references binary `A`.

# `replace`
*since OTP R14B* 

```erlang
-spec replace(Subject, Pattern, Replacement) -> Result
                 when
                     Subject :: binary(),
                     Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                     PatternBinary :: nonempty_binary(),
                     Replacement :: binary() | fun((binary()) -> binary()),
                     Result :: binary().
```

# `replace`
*since OTP R14B* 

```erlang
-spec replace(Subject, Pattern, Replacement, Options) -> Result
                 when
                     Subject :: binary(),
                     Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                     PatternBinary :: nonempty_binary(),
                     Replacement :: binary() | fun((binary()) -> binary()),
                     Options :: [Option],
                     Option :: global | {scope, part()} | {insert_replaced, InsPos},
                     InsPos :: OnePos | [OnePos],
                     OnePos :: non_neg_integer(),
                     Result :: binary().
```

Constructs a new binary by replacing the parts in `Subject` matching
`Pattern` with `Replacement` if given as a literal
`t:binary/0` or with the result of applying
`Replacement` to a matching subpart if given as a `fun`.

If `Replacement` is given as a `t:binary/0` and the matching subpart of
`Subject` giving rise to the replacement is to be inserted in the result,
option `{insert_replaced, InsPos}` inserts the matching part into `Replacement`
at the specified position (or positions) before inserting `Replacement` into
`Subject`. If `Replacement` is given as a `fun` instead, this option is ignored.

If any position specified in `InsPos` is greater than the size of the
replacement binary, a `badarg` exception is raised.

Options `global` and `{scope, part()}` work as for `split/3`. The return type is
always `t:binary/0`.

For a description of `Pattern`, see `compile_pattern/1`.

## Examples

```erlang
1> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], <<"X">>, []).
<<"aXcde">>

2> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], <<"X">>, [global]).
<<"aXcXe">>

3> binary:replace(<<"abcde">>, <<"b">>, <<"[]">>, [{insert_replaced, 1}]).
<<"a[b]cde">>

4> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], <<"[]">>, [global,{insert_replaced,1}]).
<<"a[b]c[d]e">>

5> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], <<"[]">>, [global,{insert_replaced,[1,1]}]).
<<"a[bb]c[dd]e">>

6> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], <<"[-]">>, [global,{insert_replaced,[1,2]}]).
<<"a[b-b]c[d-d]e">>

7> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], fun(M) -> <<$[, M/binary, $]>> end, []).
<<"a[b]cde">>

8> binary:replace(<<"abcde">>, [<<"b">>, <<"d">>], fun(M) -> <<$[, M/binary, $]>> end, [global]).
<<"a[b]c[d]e">>
```

# `split`
*since OTP R14B* 

```erlang
-spec split(Subject, Pattern) -> Parts
               when
                   Subject :: binary(),
                   Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                   PatternBinary :: nonempty_binary(),
                   Parts :: [binary()].
```

# `split`
*since OTP R14B* 

```erlang
-spec split(Subject, Pattern, Options) -> Parts
               when
                   Subject :: binary(),
                   Pattern :: PatternBinary | [PatternBinary, ...] | cp(),
                   PatternBinary :: nonempty_binary(),
                   Options :: [Option],
                   Option :: {scope, part()} | trim | global | trim_all,
                   Parts :: [binary()].
```

Splits `Subject` into a list of binaries based on `Pattern`.

If option `global` is not specified, only the first occurrence of `Pattern` in
`Subject` gives rise to a split.

The parts of `Pattern` found in `Subject` are not included in the result.

Summary of options:

- **\{scope, part()\}** - Works as in `match/3` and `matches/3`. Note that
  this only defines the scope of the search for matching strings; it does not
  cut the binary before splitting. The bytes before and after the scope are kept
  in the result. See the example below.

- **trim** - Removes trailing empty parts of the result (as does `trim` in
  `re:split/3`).

- **trim_all** - Removes all empty parts of the result.

- **global** - Repeats the split until `Subject` is exhausted. Conceptually
  option `global` makes split work on the positions returned by `matches/3`,
  while it normally works on the position returned by `match/3`.

Example of the difference between a scope and taking the binary apart before
splitting:

```erlang
1> binary:split(<<"banana">>, [<<"a">>], [{scope,{2,3}}]).
[<<"ban">>,<<"na">>]
2> binary:split(binary:part(<<"banana">>,{2,3}), [<<"a">>], []).
[<<"n">>,<<"n">>]
```

The return type is always a list of binaries that are all referencing `Subject`.
This means that the data in `Subject` is not copied to new binaries, and that
`Subject` cannot be garbage collected until the results of the split are no
longer referenced.

For a description of `Pattern`, see `compile_pattern/1`.

## Examples

```erlang
1> binary:split(~"the quick brown fox", ~" ", []).
[<<"the">>,<<"quick brown fox">>]
2> binary:split(~"the quick brown fox", ~" ", [global]).
[<<"the">>,<<"quick">>,<<"brown">>,<<"fox">>]
3> binary:split(<<1,255,4,0,0,0,2,3>>, [<<0,0,0>>,<<2>>], []).
[<<1,255,4>>, <<2,3>>]
4> binary:split(<<0,1,0,0,4,255,255,9>>, [<<0,0>>, <<255,255>>], [global]).
[<<0,1>>,<<4>>,<<9>>]
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
