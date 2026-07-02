# `prettypr`
[🔗](https://github.com/erlang/otp/blob/master/lib/syntax_tools/src/prettypr.erl#L35)

A generic pretty printer library.

This module uses a strict-style context passing implementation of John
Hughes algorithm, described in "The design of a Pretty-printing
Library". The paragraph-style formatting, empty documents, floating
documents, and null strings are my own additions to the algorithm.

To get started, you should read about the [`document()`](`t:document/0`) data
type; the main constructor functions: `text/1`, `above/2`, `beside/2`, `nest/2`,
`sep/1`, and `par/2`; and the main layout function `format/3`.

If you simply want to format a paragraph of plain text, you probably want to use
the `text_par/2` function, as in the following example:

```text
  prettypr:format(prettypr:text_par("Lorem ipsum dolor sit amet"), 20)
```

# `deep_string`
*not exported* 

```erlang
-type deep_string() :: [char() | deep_string()].
```

# `document`

```erlang
-type document() ::
          null |
          #text{s :: deep_string()} |
          #nest{n :: integer(), d :: document()} |
          #beside{d1 :: document(), d2 :: document()} |
          #above{d1 :: document(), d2 :: document()} |
          #sep{ds :: [document()], i :: integer(), p :: boolean()} |
          #float{d :: document(), h :: integer(), v :: integer()} |
          #union{d1 :: document(), d2 :: document()} |
          #fit{d :: document()}.
```

An abstract character-based "document" representing
a number of possible layouts, which can be processed to produce a
single concrete layout.

A concrete layout can then be rendered as a sequence of characters
containing linebreaks, which can be passed to a printer or terminal
that uses a fixed-width font.

For example, a document `sep([text("foo"), text("bar")])`
represents the two layouts:

```text
foo bar
```

and:

```text
foo
bar
```

Which layout is chosen depends on the available horizontal space.
When processing a document, the main parameters are the _paper
width_ and the _line width_ (also known as the "ribbon
width"). In the resulting layout, no text should be printed beyond
the paper width (which by default is 80 characters) as long as it can
be avoided, and each single line of text (its indentation not
counted, hence "ribbon") should preferably be no wider than the
specified line width (which by default is 65).

Documents can be joined into a single new document using the
constructor functions of this module. Note that the new document
often represents a larger number of possible layouts than just the
sum of the components.

# `above`

```erlang
-spec above(document(), document()) -> #above{d1 :: document(), d2 :: document()}.
```

Concatenates documents vertically.

Returns a document representing the concatenation of the documents
`D1` and `D2` such that the first line of `D2` follows directly below
the last line of `D1`, and the first character of `D2` is in the same
horizontal column as the first character of `D1`, in all possible
layouts.

Examples:

```text
ab  cd  =>  ab
            cd

               abc
abc   fgh  =>   de
 de    ij      fgh
                ij
```

# `beside`

```erlang
-spec beside(document(), document()) -> #beside{d1 :: document(), d2 :: document()}.
```

Concatenates documents horizontally.

Returns a document representing the concatenation of the documents
`D1` and `D2` such that the last character of `D1` is horizontally
adjacent to the first character of `D2`, in all possible
layouts. (Note: any indentation of `D2` is lost.)

Examples:

```text
ab  cd  =>  abcd

ab  ef      ab
cd  gh  =>  cdef
              gh
```

# `best`

```erlang
-spec best(document(), integer(), integer()) -> empty | document().
```

Selects a "best" layout for a document, creating a corresponding fixed-layout
document.

If no layout could be produced, the atom `empty` is returned instead.
For details about `PaperWidth` and `LineWidth`, see `format/3`. The
function is idempotent.

One possible use of this function is to compute a fixed layout for a document,
which can then be included as part of a larger document. For example:

```text
     above(text("Example:"), nest(8, best(D, W - 12, L - 6)))
```

will format `D` as a displayed-text example indented by 8, whose right margin is
indented by 4 relative to the paper width `W` of the surrounding document, and
whose maximum individual line length is shorter by 6 than the line length `L` of
the surrounding document.

This function is used by the `format/3` function to prepare a document before
being laid out as text.

# `break`

```erlang
-spec break(document()) -> #above{d1 :: document(), d2 :: document()}.
```

Forces a line break at the end of the given document.

This is a utility function; see `empty/0` for details.

# `empty`

```erlang
-spec empty() -> null.
```

Yields the empty document, which has neither height nor width.

(`empty` is thus different from an empty [`text`](`text/1`) string,
which has zero width but height 1.)

Empty documents are occasionally useful; in particular, they have the property
that [`above(X, empty())`](`above/2`) will force a new line after `X` without
leaving an empty line below it; since this is a common idiom, the utility
function `break/1` will place a given document in such a context.

_See also: _`text/1`.

# `floating`

```erlang
-spec floating(document()) -> #float{d :: document(), h :: integer(), v :: integer()}.
```

# `floating`

```erlang
-spec floating(document(), integer(), integer()) ->
                  #float{d :: document(), h :: integer(), v :: integer()}.
```

Creates a "floating" document.

The result represents the same set of layouts as `D`; however, a
floating document may be moved relative to other floating documents
immediately beside or above it, according to their relative horizontal
and vertical priorities. These priorities are set with the `Hp` and
`Vp` parameters; if omitted, both default to zero.

> #### Note {: .info }
>
> Floating documents appear to work well, but are currently less general
> than you might wish, losing effect when embedded in certain contexts. It is
> possible to nest floating-operators (even with different priorities), but the
> effects may be difficult to predict. In any case, note that the way the
> algorithm reorders floating documents amounts to a "bubblesort", so don't expect
> it to be able to sort large sequences of floating documents quickly.

# `follow`

```erlang
-spec follow(document(), document()) -> #beside{d1 :: document(), d2 :: document()}.
```

# `follow`

```erlang
-spec follow(document(), document(), integer()) -> #beside{d1 :: document(), d2 :: document()}.
```

Separates two documents by either a single space, or a line break and
indentation.

In other words, one of the layouts:

```text
abc def
```

or:

```text
abc
 def
```

will be generated, using the optional offset in the latter case. This is often
useful for typesetting programming language constructs.

This is a utility function; see `par/2` for further details.

_See also: _`follow/2`.

# `format`

```erlang
-spec format(document()) -> string().
```

# `format`

```erlang
-spec format(document(), integer()) -> string().
```

# `format`

```erlang
-spec format(document(), integer(), integer()) -> string().
```

Computes a layout for a document and returns the corresponding text.

See [`document()`](`t:document/0`) for further information. Throws
`no_layout` if no layout could be selected.

`PaperWidth` specifies the total width (in character positions) of the field for
which the text is to be laid out. `LineWidth` specifies the desired maximum
width (in number of characters) of the text printed on any single line,
disregarding leading and trailing white space. These parameters need to be
properly balanced in order to produce good layouts. By default, `PaperWidth` is
80 and `LineWidth` is 65.

_See also: _`best/3`.

# `nest`

```erlang
-spec nest(integer(), document()) -> document().
```

Indents a document a number of character positions to the right.

Note that `N` can be negative, shifting the text to the left, or zero,
in which case `D` is returned unchanged.

# `null_text`

```erlang
-spec null_text(string()) -> #text{s :: deep_string()}.
```

Similar to `text/1`, but the result is treated as having zero width.

This is regardless of the actual length of the string. Null text is
typically used for markup, which is supposed to have no effect on the
actual layout.

The standard example is when formatting source code as HTML to be
placed within `<pre>...</pre>` markup, and using elements like `<i>`
and `<b>` to make parts of the source code stand out. In this case,
the markup does not add to the width of the text when viewed in an
HTML browser, so the layout engine should simply pretend that the
markup has zero width.

_See also: _`empty/0`, `text/1`.

# `par`

```erlang
-spec par([document()]) -> #sep{ds :: [document()], i :: integer(), p :: boolean()}.
```

# `par`

```erlang
-spec par([document()], integer()) -> #sep{ds :: [document()], i :: integer(), p :: boolean()}.
```

Arranges documents in a paragraph-like layout.

Returns a document representing all possible left-aligned
paragraph-like layouts of the (nonempty) sequence `Docs` of
documents. Elements in `Docs` are separated horizontally by a single
space character and vertically with a single line break. All lines
following the first (if any) are indented to the same left column,
whose indentation is specified by the optional `Offset` parameter
relative to the position of the first element in `Docs`. For example,
with an offset of -4, the following layout can be produced, for a list
of documents representing the numbers 0 to 15:

```text
    0 1 2 3
4 5 6 7 8 9
10 11 12 13
14 15
```

or with an offset of +2:

```text
0 1 2 3 4 5 6
  7 8 9 10 11
  12 13 14 15
```

The utility function `text_par/2` can be used to easily transform a string of
text into a `par` representation by splitting it into words.

Note that whenever a document in `Docs` contains a line break, it will be placed
on a separate line. Thus, neither a layout such as:

```text
ab cd
   ef
```

nor:

```text
ab
cd ef
```

will be generated. However, a useful idiom for making the former variant
possible (when wanted) is [`beside(par([D1, text("")], N), D2)`](`beside/2`) for
two documents `D1` and `D2`. This will break the line between `D1` and `D2` if
`D1` contains a line break (or if otherwise necessary), and optionally further
indent `D2` by `N` character positions. The utility function `follow/3` creates
this context for two documents `D1` and `D2`, and an optional integer `N`.

_See also: _`par/1`, `text_par/2`.

# `sep`

```erlang
-spec sep([document()]) -> #sep{ds :: [document()], i :: integer(), p :: boolean()}.
```

Arranges documents horizontally or vertically, separated by whitespace.

Returns a document representing two alternative layouts of the
(nonempty) sequence `Docs` of documents, such that either all elements
in `Docs` are concatenated horizontally, and separated by a space
character, or all elements are concatenated vertically (without extra
separation).

> #### Note {: .info }
>
> If some document in `Docs` contains a line break, the vertical layout
> will always be selected.

Examples:

```text
                             ab
ab  cd  ef  =>  ab cd ef  |  cd
                             ef

ab           ab
cd  ef  =>   cd
             ef
```

_See also: _`par/2`.

# `text`

```erlang
-spec text(string()) -> #text{s :: deep_string()}.
```

Yields a document representing a fixed, unbreakable sequence of characters.

The string should contain only _printable_ characters (tabs allowed
but not recommended), and _not_ newline, line feed, vertical tab,
and so on. A tab character (`\t`) is interpreted as padding of 1-8 space
characters to the next column of 8 characters _within the string_.

_See also: _`empty/0`, `null_text/1`, `text_par/2`.

# `text_par`

```erlang
-spec text_par(string()) -> document().
```

# `text_par`

```erlang
-spec text_par(string(), integer()) -> document().
```

Yields a document representing paragraph-formatted plain text.

The `Indentation` parameter specifies the extra indentation
of the first line of the paragraph. For example, [`text_par("Lorem
ipsum dolor sit amet", N)`](`text_par/2`) could represent:

```text
Lorem ipsum dolor
sit amet
```

if N = 0, or:

```text
  Lorem ipsum
dolor sit amet
```

if `N` = 2, or:

```text
Lorem ipsum dolor
  sit amet
```

if `N` = -2.

(The sign of the indentation is thus reversed compared to the `par/2` function,
and the behaviour varies slightly depending on the sign in order to match the
expected layout of a paragraph of text.)

Note that this is just a utility function, which does all the work of splitting
the given string into words separated by whitespace and setting up a
[`par`](`par/2`) with the proper indentation, containing a list of
[`text`](`text/1`) elements.

_See also: _`par/2`, `text/1`, `text_par/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
