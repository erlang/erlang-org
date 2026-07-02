# `string`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/string.erl#L50)

String processing functions.

This module provides functions for string processing.

A string in this module is represented by `t:unicode:chardata/0`, that is, a
list of codepoints, binaries with UTF-8-encoded codepoints (_UTF-8 binaries_),
or a mix of the two.

```text
"abcd"               is a valid string
<<"abcd">>           is a valid string
["abcd"]             is a valid string
<<"abc..åäö"/utf8>>  is a valid string
<<"abc..åäö">>       is NOT a valid string,
                     but a binary with Latin-1-encoded codepoints
[<<"abc">>, "..åäö"] is a valid string
[atom]               is NOT a valid string
```

This module operates on grapheme clusters. A _grapheme cluster_ is a
user-perceived character, which can be represented by several codepoints.

```text
"å"  [229] or [97, 778]
"e̊"  [101, 778]
```

The string length of "ß↑e̊" is 3, even though it is represented by the codepoints
`[223,8593,101,778]` or the UTF-8 binary `<<195,159,226,134,145,101,204,138>>`.

Grapheme clusters for codepoints of class `prepend` and non-modern (or
decomposed) Hangul is not handled for performance reasons in `find/3`,
`replace/3`, `split/2`, `split/3` and `trim/3`.

Splitting and appending strings is to be done on grapheme clusters borders.
There is no verification that the results of appending strings are valid or
normalized.

Most of the functions expect all input to be normalized to one form, see for
example `unicode:characters_to_nfc_list/1`.

Language or locale specific handling of input is not considered in any function.

The functions can crash for non-valid input strings. For example, the functions
expect UTF-8 binaries but not all functions verify that all binaries are encoded
correctly.

Unless otherwise specified the return value type is the same as the input type.
That is, binary input returns binary output, list input returns a list output,
and mixed input can return a mixed output.

```erlang
1> string:trim("  sarah  ").
"sarah"
2> string:trim(<<"  sarah  ">>).
<<"sarah">>
3> string:lexemes("foo bar", " ").
["foo","bar"]
4> string:lexemes(<<"foo bar">>, " ").
[<<"foo">>,<<"bar">>]
```

This module has been reworked in Erlang/OTP 20 to handle `t:unicode:chardata/0`
and operate on grapheme clusters. The
[`old functions`](`m:string#obsolete-api-functions`) that only work on Latin-1
lists as input are still available but should not be used, they will be
deprecated in a future release.

## Overlapping Functions

Some of the general string functions can seem to overlap each other. The reason
is that this string package is the combination of two earlier packages and all
functions of both packages have been retained.

# `direction`
*not exported* 

```erlang
-type direction() :: leading | trailing.
```

# `grapheme_cluster`

```erlang
-type grapheme_cluster() :: char() | [char()].
```

A user-perceived character, consisting of one or more codepoints.

# `casefold`
*since OTP 20.0* 

```erlang
-spec casefold(String :: unicode:chardata()) -> unicode:chardata().
```

Converts `String` to a case-agnostic comparable string. Function
[`casefold/1`](`casefold/1`) is preferred over [`lowercase/1`](`lowercase/1`)
when two strings are to be compared for equality. See also `equal/4`.

_Example:_

```erlang
1> string:casefold("Ω and ẞ SHARP S").
"ω and ss sharp s"
```

# `chomp`
*since OTP 20.0* 

```erlang
-spec chomp(String :: unicode:chardata()) -> unicode:chardata().
```

Returns a string where any trailing `\n` or `\r\n` have been removed from
`String`.

_Example:_

```erlang
182> string:chomp(<<"\nHello\n\n">>).
<<"\nHello">>
183> string:chomp("\nHello\r\r\n").
"\nHello\r"
```

# `equal`

```erlang
-spec equal(A, B) -> boolean() when A :: unicode:chardata(), B :: unicode:chardata().
```

# `equal`
*since OTP 20.0* 

```erlang
-spec equal(A, B, IgnoreCase) -> boolean()
               when A :: unicode:chardata(), B :: unicode:chardata(), IgnoreCase :: boolean().
```

# `equal`
*since OTP 20.0* 

```erlang
-spec equal(A, B, IgnoreCase, Norm) -> boolean()
               when
                   A :: unicode:chardata(),
                   B :: unicode:chardata(),
                   IgnoreCase :: boolean(),
                   Norm :: none | nfc | nfd | nfkc | nfkd.
```

Returns `true` if `A` and `B` are equal, otherwise `false`.

If `IgnoreCase` is `true` the function does [`casefold`ing](`casefold/1`) on the
fly before the equality test.

If `Norm` is not `none` the function applies normalization on the fly before the
equality test. There are four available normalization forms:
[`nfc`](`unicode:characters_to_nfc_list/1`),
[`nfd`](`unicode:characters_to_nfd_list/1`),
[`nfkc`](`unicode:characters_to_nfkc_list/1`), and
[`nfkd`](`unicode:characters_to_nfkd_list/1`).

_Example:_

```erlang
1> string:equal("åäö", <<"åäö"/utf8>>).
true
2> string:equal("åäö", unicode:characters_to_nfd_binary("åäö")).
false
3> string:equal("åäö", unicode:characters_to_nfd_binary("ÅÄÖ"), true, nfc).
true
```

# `find`
*since OTP 20.0* 

```erlang
-spec find(String, SearchPattern) -> unicode:chardata() | nomatch
              when String :: unicode:chardata(), SearchPattern :: unicode:chardata().
```

# `find`
*since OTP 20.0* 

```erlang
-spec find(String, SearchPattern, Dir) -> unicode:chardata() | nomatch
              when String :: unicode:chardata(), SearchPattern :: unicode:chardata(), Dir :: direction().
```

Removes anything before `SearchPattern` in `String` and returns the remainder of
the string or `nomatch` if `SearchPattern` is not found. `Dir`, which can be
`leading` or `trailing`, indicates from which direction characters are to be
searched.

_Example:_

```erlang
1> string:find("ab..cd..ef", ".").
"..cd..ef"
2> string:find(<<"ab..cd..ef">>, "..", trailing).
<<"..ef">>
3> string:find(<<"ab..cd..ef">>, "x", leading).
nomatch
4> string:find("ab..cd..ef", "x", trailing).
nomatch
```

# `is_empty`
*since OTP 20.0* 

```erlang
-spec is_empty(String :: unicode:chardata()) -> boolean().
```

Returns `true` if `String` is the empty string, otherwise `false`.

_Example:_

```erlang
1> string:is_empty("foo").
false
2> string:is_empty(["",<<>>]).
true
```

# `jaro_similarity`
*since OTP 27.0* 

```erlang
-spec jaro_similarity(String1, String2) -> Similarity
                         when
                             String1 :: unicode:chardata(),
                             String2 :: unicode:chardata(),
                             Similarity :: float().
```

Returns a float between `+0.0` and `1.0` representing the
[Jaro similarity](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)
between the given strings. Strings with a higher similarity will score closer
to `1.0`, with `+0.0` meaning no similarity and `1.0` meaning an exact match.

_Example:_

```erlang
1> string:jaro_similarity("ditto", "ditto").
1.0
2> string:jaro_similarity("foo", "bar").
+0.0
3> string:jaro_similarity("michelle", "michael").
0.8690476190476191
4> string:jaro_similarity(<<"Édouard"/utf8>>, <<"Claude">>).
0.5317460317460317
```

The Jaro distance between two strings can be calculated with
`JaroDistance = 1.0 - JaroSimilarity`.

# `length`
*since OTP 20.0* 

```erlang
-spec length(String :: unicode:chardata()) -> non_neg_integer().
```

Returns the number of grapheme clusters in `String`.

_Example:_

```erlang
1> string:length("ß↑e̊").
3
2> string:length(<<195,159,226,134,145,101,204,138>>).
3
```

# `lexemes`
*since OTP 20.0* 

```erlang
-spec lexemes(String :: unicode:chardata(), SeparatorList :: [grapheme_cluster()]) ->
                 [unicode:chardata()].
```

Returns a list of lexemes in `String`, separated by the grapheme clusters in
`SeparatorList`.

Notice that, as shown in this example, two or more adjacent separator graphemes
clusters in `String` are treated as one. That is, there are no empty strings in
the resulting list of lexemes. See also `split/3` which returns empty strings.

Notice that `[$\r,$\n]` is one grapheme cluster.

_Example:_

```erlang
1> string:lexemes("abc de̊fxxghix jkl\r\nfoo", "x e" ++ [[$\r,$\n]]).
["abc","de̊f","ghi","jkl","foo"]
2> string:lexemes(<<"abc de̊fxxghix jkl\r\nfoo"/utf8>>, "x e" ++ [$\r,$\n]).
[<<"abc">>,<<"de̊f"/utf8>>,<<"ghi">>,<<"jkl\r\nfoo">>]
```

# `lowercase`
*since OTP 20.0* 

```erlang
-spec lowercase(String :: unicode:chardata()) -> unicode:chardata().
```

Converts `String` to lowercase.

Notice that function `casefold/1` should be used when converting a string to be
tested for equality.

_Example:_

```erlang
2> string:lowercase(string:uppercase("Michał")).
"michał"
```

# `next_codepoint`
*since OTP 20.0* 

```erlang
-spec next_codepoint(String :: unicode:chardata()) ->
                        maybe_improper_list(char(), unicode:chardata()) | {error, unicode:chardata()}.
```

Returns the first codepoint in `String` and the rest of `String` in the tail.
Returns an empty list if `String` is empty or an `{error, String}` tuple if the
next byte is invalid.

_Example:_

```erlang
1> string:next_codepoint(unicode:characters_to_binary("e̊fg")).
[101|<<"̊fg"/utf8>>]
```

# `next_grapheme`
*since OTP 20.0* 

```erlang
-spec next_grapheme(String :: unicode:chardata()) ->
                       maybe_improper_list(grapheme_cluster(), unicode:chardata()) |
                       {error, unicode:chardata()}.
```

Returns the first grapheme cluster in `String` and the rest of `String` in the
tail. Returns an empty list if `String` is empty or an `{error, String}` tuple
if the next byte is invalid.

_Example:_

```erlang
1> string:next_grapheme(unicode:characters_to_binary("e̊fg")).
["e̊"|<<"fg">>]
```

# `nth_lexeme`
*since OTP 20.0* 

```erlang
-spec nth_lexeme(String, N, SeparatorList) -> unicode:chardata()
                    when
                        String :: unicode:chardata(),
                        N :: non_neg_integer(),
                        SeparatorList :: [grapheme_cluster()].
```

Returns lexeme number `N` in `String`, where lexemes are separated by the
grapheme clusters in `SeparatorList`.

_Example:_

```erlang
1> string:nth_lexeme("abc.de̊f.ghiejkl", 3, ".e").
"ghi"
```

# `pad`
*since OTP 20.0* 

```erlang
-spec pad(String, Length) -> unicode:charlist() when String :: unicode:chardata(), Length :: integer().
```

# `pad`
*since OTP 20.0* 

```erlang
-spec pad(String, Length, Dir) -> unicode:charlist()
             when String :: unicode:chardata(), Length :: integer(), Dir :: direction() | both.
```

# `pad`
*since OTP 20.0* 

```erlang
-spec pad(String, Length, Dir, Char) -> unicode:charlist()
             when
                 String :: unicode:chardata(),
                 Length :: integer(),
                 Dir :: direction() | both,
                 Char :: grapheme_cluster().
```

Pads `String` to `Length` with grapheme cluster `Char`. `Dir`, which can be
`leading`, `trailing`, or `both`, indicates where the padding should be added.

_Example:_

```erlang
1> string:pad(<<"He̊llö"/utf8>>, 8).
[<<72,101,204,138,108,108,195,182>>,32,32,32]
2> io:format("'~ts'~n",[string:pad("He̊llö", 8, leading)]).
'   He̊llö'
3> io:format("'~ts'~n",[string:pad("He̊llö", 8, both)]).
' He̊llö  '
```

# `prefix`
*since OTP 20.0* 

```erlang
-spec prefix(String :: unicode:chardata(), Prefix :: unicode:chardata()) -> nomatch | unicode:chardata().
```

If `Prefix` is the prefix of `String`, removes it and returns the remainder of
`String`, otherwise returns `nomatch`.

_Example:_

```erlang
1> string:prefix(<<"prefix of string">>, "pre").
<<"fix of string">>
2> string:prefix("pre", "prefix").
nomatch
```

# `replace`
*since OTP 20.0* 

```erlang
-spec replace(String, SearchPattern, Replacement) -> [unicode:chardata()]
                 when
                     String :: unicode:chardata(),
                     SearchPattern :: unicode:chardata(),
                     Replacement :: unicode:chardata().
```

# `replace`
*since OTP 20.0* 

```erlang
-spec replace(String, SearchPattern, Replacement, Where) -> [unicode:chardata()]
                 when
                     String :: unicode:chardata(),
                     SearchPattern :: unicode:chardata(),
                     Replacement :: unicode:chardata(),
                     Where :: direction() | all.
```

Replaces `SearchPattern` in `String` with `Replacement`. `Where`, indicates whether
the `leading`, the `trailing` or `all` encounters of `SearchPattern` are to be replaced.

Can be implemented as:

```erlang
lists:join(Replacement, split(String, SearchPattern, Where)).
```

_Example:_

```erlang
1> string:replace(<<"ab..cd..ef">>, "..", "*").
[<<"ab">>,"*",<<"cd..ef">>]
2> string:replace(<<"ab..cd..ef">>, "..", "*", all).
[<<"ab">>,"*",<<"cd">>,"*",<<"ef">>]
```

# `reverse`
*since OTP 20.0* 

```erlang
-spec reverse(String :: unicode:chardata()) -> [grapheme_cluster()].
```

Returns the reverse list of the grapheme clusters in `String`.

_Example:_

```erlang
1> Reverse = string:reverse(unicode:characters_to_nfd_binary("ÅÄÖ")).
[[79,776],[65,776],[65,778]]
2> io:format("~ts~n",[Reverse]).
ÖÄÅ
```

# `slice`
*since OTP 20.0* 

```erlang
-spec slice(String, Start) -> Slice
               when
                   String :: unicode:chardata(), Start :: non_neg_integer(), Slice :: unicode:chardata().
```

# `slice`
*since OTP 20.0* 

```erlang
-spec slice(String, Start, Length) -> Slice
               when
                   String :: unicode:chardata(),
                   Start :: non_neg_integer(),
                   Length :: infinity | non_neg_integer(),
                   Slice :: unicode:chardata().
```

Returns a substring of `String` of at most `Length` grapheme clusters, starting
at position `Start`.

_Example:_

```erlang
1> string:slice(<<"He̊llö Wörld"/utf8>>, 4).
<<"ö Wörld"/utf8>>
2> string:slice(["He̊llö ", <<"Wörld"/utf8>>], 4,4).
"ö Wö"
3> string:slice(["He̊llö ", <<"Wörld"/utf8>>], 4,50).
"ö Wörld"
```

# `split`
*since OTP 20.0* 

```erlang
-spec split(String, SearchPattern) -> [unicode:chardata()]
               when String :: unicode:chardata(), SearchPattern :: unicode:chardata().
```

# `split`
*since OTP 20.0* 

```erlang
-spec split(String, SearchPattern, Where) -> [unicode:chardata()]
               when
                   String :: unicode:chardata(),
                   SearchPattern :: unicode:chardata(),
                   Where :: direction() | all.
```

Splits `String` where `SearchPattern` is encountered and return the remaining
parts. `Where`, default `leading`, indicates whether the `leading`, the
`trailing` or `all` encounters of `SearchPattern` will split `String`.

_Example:_

```erlang
0> string:split("ab..bc..cd", "..").
["ab","bc..cd"]
1> string:split(<<"ab..bc..cd">>, "..", trailing).
[<<"ab..bc">>,<<"cd">>]
2> string:split(<<"ab..bc....cd">>, "..", all).
[<<"ab">>,<<"bc">>,<<>>,<<"cd">>]
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(String, Characters) -> {Leading, Trailing}
              when
                  String :: unicode:chardata(),
                  Characters :: [grapheme_cluster()],
                  Leading :: unicode:chardata(),
                  Trailing :: unicode:chardata().
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(String, Characters, Complement) -> {Leading, Trailing}
              when
                  String :: unicode:chardata(),
                  Characters :: [grapheme_cluster()],
                  Complement :: boolean(),
                  Leading :: unicode:chardata(),
                  Trailing :: unicode:chardata().
```

# `take`
*since OTP 20.0* 

```erlang
-spec take(String, Characters, Complement, Dir) -> {Leading, Trailing}
              when
                  String :: unicode:chardata(),
                  Characters :: [grapheme_cluster()],
                  Complement :: boolean(),
                  Dir :: direction(),
                  Leading :: unicode:chardata(),
                  Trailing :: unicode:chardata().
```

Takes characters from `String` as long as the characters are members of set
`Characters` or the complement of set `Characters`. `Dir`, which can be
`leading` or `trailing`, indicates from which direction characters are to be
taken.

_Example:_

```erlang
5> string:take("abc0z123", lists:seq($a,$z)).
{"abc","0z123"}
6> string:take(<<"abc0z123">>, lists:seq($0,$9), true, leading).
{<<"abc">>,<<"0z123">>}
7> string:take("abc0z123", lists:seq($0,$9), false, trailing).
{"abc0z","123"}
8> string:take(<<"abc0z123">>, lists:seq($a,$z), true, trailing).
{<<"abc0z">>,<<"123">>}
```

# `titlecase`
*since OTP 20.0* 

```erlang
-spec titlecase(String :: unicode:chardata()) -> unicode:chardata().
```

Converts `String` to titlecase.

_Example:_

```erlang
1> string:titlecase("ß is a SHARP s").
"Ss is a SHARP s"
```

# `to_float`

```erlang
-spec to_float(String) -> {Float, Rest} | {error, Reason}
                  when
                      String :: unicode:chardata(),
                      Float :: float(),
                      Rest :: unicode:chardata(),
                      Reason :: no_float | badarg.
```

Argument `String` is expected to start with a valid text represented float (the
digits are ASCII values). Remaining characters in the string after the float are
returned in `Rest`.

_Example:_

```erlang
1> {F1,Fs} = string:to_float("1.0-1.0e-1"),
1> {F2,[]} = string:to_float(Fs),
1> F1+F2.
0.9
2> string:to_float("3/2=1.5").
{error,no_float}
3> string:to_float("-1.5eX").
{-1.5,"eX"}
```

# `to_graphemes`
*since OTP 20.0* 

```erlang
-spec to_graphemes(String :: unicode:chardata()) -> [grapheme_cluster()].
```

Converts `String` to a list of grapheme clusters.

_Example:_

```erlang
1> string:to_graphemes("ß↑e̊").
[223,8593,[101,778]]
2> string:to_graphemes(<<"ß↑e̊"/utf8>>).
[223,8593,[101,778]]
```

# `to_integer`

```erlang
-spec to_integer(String) -> {Int, Rest} | {error, Reason}
                    when
                        String :: unicode:chardata(),
                        Int :: integer(),
                        Rest :: unicode:chardata(),
                        Reason :: no_integer | badarg.
```

Argument `String` is expected to start with a valid text represented integer
(the digits are ASCII values). Remaining characters in the string after the
integer are returned in `Rest`.

_Example:_

```erlang
1> {I1,Is} = string:to_integer("33+22"),
1> {I2,[]} = string:to_integer(Is),
1> I1-I2.
11
2> string:to_integer("0.5").
{0,".5"}
3> string:to_integer("x=2").
{error,no_integer}
```

# `trim`
*since OTP 20.0* 

```erlang
-spec trim(String) -> unicode:chardata() when String :: unicode:chardata().
```

# `trim`
*since OTP 20.0* 

```erlang
-spec trim(String, Dir) -> unicode:chardata()
              when String :: unicode:chardata(), Dir :: direction() | both.
```

Equivalent to [`trim(String, Dir, Whitespace})`](`trim/3`) where 
`Whitespace` is the set of nonbreakable whitespace codepoints, defined
as Pattern_White_Space in
[Unicode Standard Annex #31](http://unicode.org/reports/tr31/).

# `trim`
*since OTP 20.0* 

```erlang
-spec trim(String, Dir, Characters) -> unicode:chardata()
              when
                  String :: unicode:chardata(),
                  Dir :: direction() | both,
                  Characters :: [grapheme_cluster()].
```

Returns a string, where leading or trailing, or both, `Characters` have been
removed.

`Dir` which can be `leading`, `trailing`, or `both`, indicates from
which direction characters are to be removed.

Note that `[$\r,$\n]` is one grapheme cluster according to the Unicode
Standard.

_Example:_

```erlang
1> string:trim("\t  Hello  \n").
"Hello"
2> string:trim(<<"\t  Hello  \n">>, leading).
<<"Hello  \n">>
3> string:trim(<<".Hello.\n">>, trailing, "\n.").
<<".Hello">>
```

# `uppercase`
*since OTP 20.0* 

```erlang
-spec uppercase(String :: unicode:chardata()) -> unicode:chardata().
```

Converts `String` to uppercase.

See also `titlecase/1`.

_Example:_

```erlang
1> string:uppercase("Michał").
"MICHAŁ"
```

# `centre`

```erlang
-spec centre(String, Number) -> Centered
                when String :: string(), Centered :: string(), Number :: non_neg_integer().
```

# `centre`

```erlang
-spec centre(String, Number, Character) -> Centered
                when
                    String :: string(),
                    Centered :: string(),
                    Number :: non_neg_integer(),
                    Character :: char().
```

Returns a string, where `String` is centered in the string and surrounded by
blanks or `Character`. The resulting string has length `Number`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `pad/3`.

# `chars`

```erlang
-spec chars(Character, Number) -> String
               when Character :: char(), Number :: non_neg_integer(), String :: string().
```

# `chars`

```erlang
-spec chars(Character, Number, Tail) -> String
               when
                   Character :: char(),
                   Number :: non_neg_integer(),
                   Tail :: string(),
                   String :: string().
```

Returns a string consisting of `Number` characters `Character`. Optionally, the
string can end with string `Tail`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use
`lists:duplicate/2`.

# `chr`

```erlang
-spec chr(String, Character) -> Index
             when String :: string(), Character :: char(), Index :: non_neg_integer().
```

Returns the index of the first occurrence of `Character` in `String`. Returns
`0` if `Character` does not occur.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `find/2`.

# `concat`

```erlang
-spec concat(String1, String2) -> String3
                when String1 :: string(), String2 :: string(), String3 :: string().
```

Concatenates `String1` and `String2` to form a new string `String3`, which is
returned.

This function is [obsolete](`m:string#obsolete-api-functions`). Use
`[String1, String2]` as `Data` argument, and call `unicode:characters_to_list/2`
or `unicode:characters_to_binary/2` to flatten the output.

# `copies`

```erlang
-spec copies(String, Number) -> Copies
                when String :: string(), Copies :: string(), Number :: non_neg_integer().
```

Returns a string containing `String` repeated `Number` times.

This function is [obsolete](`m:string#obsolete-api-functions`). Use
`lists:duplicate/2`.

# `cspan`

```erlang
-spec cspan(String, Chars) -> Length
               when String :: string(), Chars :: string(), Length :: non_neg_integer().
```

Returns the length of the maximum initial segment of `String`, which consists
entirely of characters not from `Chars`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `take/3`.

_Example:_

```erlang
1> string:cspan("\t    abcdef", " \t").
0
```

# `join`

```erlang
-spec join(StringList, Separator) -> String
              when StringList :: [string()], Separator :: string(), String :: string().
```

Returns a string with the elements of `StringList` separated by the string in
`Separator`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use
`lists:join/2`.

_Example:_

```erlang
1> join(["one", "two", "three"], ", ").
"one, two, three"
```

# `left`

```erlang
-spec left(String, Number) -> Left
              when String :: string(), Left :: string(), Number :: non_neg_integer().
```

# `left`

```erlang
-spec left(String, Number, Character) -> Left
              when
                  String :: string(), Left :: string(), Number :: non_neg_integer(), Character :: char().
```

Returns `String` with the length adjusted in accordance with `Number`. The left
margin is fixed. If [`length(String)`](`length/1`) < `Number`, then `String` is
padded with blanks or `Character`s.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `pad/2` or
`pad/3`.

_Example:_

```erlang
1> string:left("Hello",10,$.).
"Hello....."
```

# `len`

```erlang
-spec len(String) -> Length when String :: string(), Length :: non_neg_integer().
```

Returns the number of characters in `String`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `length/1`.

# `rchr`

```erlang
-spec rchr(String, Character) -> Index
              when String :: string(), Character :: char(), Index :: non_neg_integer().
```

Returns the index of the last occurrence of `Character` in `String`. Returns `0`
if `Character` does not occur.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `find/3`.

# `right`

```erlang
-spec right(String, Number) -> Right
               when String :: string(), Right :: string(), Number :: non_neg_integer().
```

# `right`

```erlang
-spec right(String, Number, Character) -> Right
               when
                   String :: string(),
                   Right :: string(),
                   Number :: non_neg_integer(),
                   Character :: char().
```

Returns `String` with the length adjusted in accordance with `Number`. The right
margin is fixed. If the length of `(String)` < `Number`, then `String` is padded
with blanks or `Character`s.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `pad/3`.

_Example:_

```erlang
1> string:right("Hello", 10, $.).
".....Hello"
```

# `rstr`

```erlang
-spec rstr(String, SubString) -> Index
              when String :: string(), SubString :: string(), Index :: non_neg_integer().
```

Returns the position where the last occurrence of `SubString` begins in
`String`. Returns `0` if `SubString` does not exist in `String`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `find/3`.

_Example:_

```erlang
1> string:rstr(" Hello Hello World World ", "Hello World").
8
```

# `span`

```erlang
-spec span(String, Chars) -> Length
              when String :: string(), Chars :: string(), Length :: non_neg_integer().
```

Returns the length of the maximum initial segment of `String`, which consists
entirely of characters from `Chars`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `take/2`.

_Example:_

```erlang
1> string:span("\t    abcdef", " \t").
5
```

# `str`

```erlang
-spec str(String, SubString) -> Index
             when String :: string(), SubString :: string(), Index :: non_neg_integer().
```

Returns the position where the first occurrence of `SubString` begins in
`String`. Returns `0` if `SubString` does not exist in `String`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `find/2`.

_Example:_

```erlang
1> string:str(" Hello Hello World World ", "Hello World").
8
```

# `strip`

```erlang
-spec strip(string()) -> string().
```

# `strip`

```erlang
-spec strip(String, Direction) -> Stripped
               when String :: string(), Stripped :: string(), Direction :: left | right | both.
```

# `strip`

```erlang
-spec strip(String, Direction, Character) -> Stripped
               when
                   String :: string(),
                   Stripped :: string(),
                   Direction :: left | right | both,
                   Character :: char().
```

Returns a string, where leading or trailing, or both, blanks or a number of
`Character` have been removed.

`Direction`, which can be `left`, `right`, or
`both`, indicates from which direction blanks are to be removed.
[`strip/1`](`strip/1`) is equivalent to [`strip(String, both)`](`strip/2`).

This function is [obsolete](`m:string#obsolete-api-functions`). Use `trim/3`.

_Example:_

```erlang
1> string:strip("...Hello.....", both, $.).
"Hello"
```

# `sub_string`

```erlang
-spec sub_string(String, Start) -> SubString
                    when String :: string(), SubString :: string(), Start :: pos_integer().
```

# `sub_string`

```erlang
-spec sub_string(String, Start, Stop) -> SubString
                    when
                        String :: string(),
                        SubString :: string(),
                        Start :: pos_integer(),
                        Stop :: pos_integer().
```

Returns a substring of `String`, starting at position `Start` to the end of the
string, or to and including position `Stop`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `slice/3`.

_Example:_

```erlang
1> sub_string("Hello World", 4, 8).
"lo Wo"
```

# `sub_word`

```erlang
-spec sub_word(String, Number) -> Word when String :: string(), Word :: string(), Number :: integer().
```

# `sub_word`

```erlang
-spec sub_word(String, Number, Character) -> Word
                  when String :: string(), Word :: string(), Number :: integer(), Character :: char().
```

Returns the word in position `Number` of `String`. Words are separated by blanks
or `Character`s.

This function is [obsolete](`m:string#obsolete-api-functions`). Use
`nth_lexeme/3`.

_Example:_

```erlang
1> string:sub_word(" Hello old boy !",3,$o).
"ld b"
```

# `substr`

```erlang
-spec substr(String, Start) -> SubString
                when String :: string(), SubString :: string(), Start :: pos_integer().
```

# `substr`

```erlang
-spec substr(String, Start, Length) -> SubString
                when
                    String :: string(),
                    SubString :: string(),
                    Start :: pos_integer(),
                    Length :: non_neg_integer().
```

Returns a substring of `String`, starting at position `Start`, and ending at the
end of the string or at length `Length`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `slice/3`.

_Example:_

```erlang
1> substr("Hello World", 4, 5).
"lo Wo"
```

# `to_lower`

```erlang
-spec to_lower(String) -> Result when String :: io_lib:latin1_string(), Result :: io_lib:latin1_string();
              (Char) -> CharResult when Char :: char(), CharResult :: char().
```

The specified string or character is case-converted. Notice that the supported
character set is ISO/IEC 8859-1 (also called Latin 1); all values outside this
set are unchanged.

This function is [obsolete](`m:string#obsolete-api-functions`) use
`lowercase/1`, `titlecase/1` or `casefold/1`.

# `to_upper`

```erlang
-spec to_upper(String) -> Result when String :: io_lib:latin1_string(), Result :: io_lib:latin1_string();
              (Char) -> CharResult when Char :: char(), CharResult :: char().
```

The specified string or character is case-converted. Notice that the supported
character set is ISO/IEC 8859-1 (also called Latin 1); all values outside this
set are unchanged.

This function is [obsolete](`m:string#obsolete-api-functions`) use
`uppercase/1`, `titlecase/1` or `casefold/1`.

# `tokens`

```erlang
-spec tokens(String, SeparatorList) -> Tokens
                when
                    String :: string(),
                    SeparatorList :: string(),
                    Tokens :: [Token :: nonempty_string()].
```

Returns a list of tokens in `String`, separated by the characters in
`SeparatorList`.

_Example:_

```erlang
1> tokens("abc defxxghix jkl", "x ").
["abc", "def", "ghi", "jkl"]
```

Notice that, as shown in this example, two or more adjacent separator characters
in `String` are treated as one. That is, there are no empty strings in the
resulting list of tokens.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `lexemes/2`.

# `words`

```erlang
-spec words(String) -> Count when String :: string(), Count :: pos_integer().
```

# `words`

```erlang
-spec words(String, Character) -> Count
               when String :: string(), Character :: char(), Count :: pos_integer().
```

Returns the number of words in `String`, separated by blanks or `Character`.

This function is [obsolete](`m:string#obsolete-api-functions`). Use `lexemes/2`.

_Example:_

```erlang
1> words(" Hello old boy!", $o).
4
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
