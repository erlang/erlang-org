# `erl_anno`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_anno.erl#L23)

Abstract datatype for the annotations of the Erlang Compiler.

This module provides an abstract type that is used by the Erlang Compiler and
its helper modules for holding data such as column, line number, and text. The
data type is a collection of _annotations_{: #annotations } as described in the
following.

The Erlang Token Scanner returns tokens with a subset of the following
annotations, depending on the options:

- **`column`** - The column where the token begins.

- **`location`** - The line and column where the token begins, or just the line
  if the column is unknown.

- **`text`** - The token's text.

From this, the following annotation is derived:

- **`line`** - The line where the token begins.

This module also supports the following annotations, which are used by various
modules:

- **`file`** - A filename.

- **`generated`** - A Boolean indicating if the abstract code is
  compiler-generated. The Erlang Compiler does not emit warnings for such code.

- **`record`** - A Boolean indicating if the origin of the abstract code is a
  record. Used by [Dialyzer](`m:dialyzer`) to assign types to tuple elements.

The functions [`column()`](`erl_scan:column/1`),
[`end_location()`](`erl_scan:end_location/1`), [`line()`](`erl_scan:line/1`),
[`location()`](`erl_scan:location/1`), and [`text()`](`erl_scan:text/1`) in the
`erl_scan` module can be used for inspecting annotations in tokens.

The functions [`anno_from_term()`](`erl_parse:anno_from_term/1`),
[`anno_to_term()`](`erl_parse:anno_to_term/1`),
[`fold_anno()`](`erl_parse:fold_anno/3`),
[`map_anno()`](`erl_parse:map_anno/2`),
[`mapfold_anno()`](`erl_parse:mapfold_anno/3`), and
[`new_anno()`](`erl_parse:new_anno/1`), in the `erl_parse` module can be used
for manipulating annotations in abstract code.

### See Also

`m:erl_parse`, `m:erl_scan`

# `anno`
*since OTP 18.0* 

```erlang
-opaque anno()
```

A collection of annotations.

# `anno_term`
*since OTP 18.0* 

```erlang
-type anno_term() :: term().
```

The term representing a collection of annotations. It is either a `t:location/0`
or a list of key-value pairs.

# `column`
*since OTP 18.0* 

```erlang
-type column() :: pos_integer().
```

# `filename`
*not exported* *since OTP 18.0* 

```erlang
-type filename() :: file:filename_all().
```

# `generated`
*not exported* *since OTP 18.0* 

```erlang
-type generated() :: boolean().
```

# `line`
*since OTP 18.0* 

```erlang
-nominal line() :: non_neg_integer().
```

# `location`
*since OTP 18.0* 

```erlang
-nominal location() :: line() | {line(), column()}.
```

# `record_local`
*not exported* *since OTP 18.0* 

```erlang
-type record_local() :: boolean().
```

# `text`
*since OTP 18.0* 

```erlang
-type text() :: string().
```

# `column`
*since OTP 18.0* 

```erlang
-spec column(Anno) -> column() | undefined when Anno :: anno().
```

Returns the column of the annotations Anno.

# `end_location`
*since OTP 18.0* 

```erlang
-spec end_location(Anno) -> location() | undefined when Anno :: anno().
```

Returns the end location of the annotations Anno.

If the end location annotation is present, its value is returned. Otherwise,
if the text annotation is present, the end location is inferred from the
location and the text. Finally, if there is no text, `undefined` is returned.

# `file`
*since OTP 18.0* 

```erlang
-spec file(Anno) -> filename() | undefined when Anno :: anno().
```

Returns the filename of the annotations Anno. If there is no filename,
`undefined` is returned.

# `from_term`
*since OTP 18.0* 

```erlang
-spec from_term(Term) -> Anno when Term :: anno_term(), Anno :: anno().
```

Returns annotations with representation Term.

See also [to_term()](`to_term/1`).

# `generated`
*since OTP 18.0* 

```erlang
-spec generated(Anno) -> generated() when Anno :: anno().
```

Returns `true` if annotations Anno is marked as generated. The default is to
return `false`.

# `is_anno`
*since OTP 18.0* 

```erlang
-spec is_anno(Term) -> boolean() when Term :: any().
```

Returns `true` if Term is a collection of annotations, otherwise `false`.

# `line`
*since OTP 18.0* 

```erlang
-spec line(Anno) -> line() when Anno :: anno().
```

Returns the line of the annotations Anno.

# `location`
*since OTP 18.0* 

```erlang
-spec location(Anno) -> location() when Anno :: anno().
```

Returns the location of the annotations Anno. If there is no location,
a zero line number is returned.

# `new`
*since OTP 18.0* 

```erlang
-spec new(Location) -> anno() when Location :: location().
```

Creates a new collection of annotations given a location.

# `set_end_location`
*since OTP 28.0* 

```erlang
-spec set_end_location(Location, Anno) -> Anno when Location :: location(), Anno :: anno().
```

Modifies the end location of the annotations Anno.

# `set_file`
*since OTP 18.0* 

```erlang
-spec set_file(File, Anno) -> Anno when File :: filename(), Anno :: anno().
```

Modifies the filename of the annotations Anno.

# `set_generated`
*since OTP 18.0* 

```erlang
-spec set_generated(Generated, Anno) -> Anno when Generated :: generated(), Anno :: anno().
```

Modifies the generated marker of the annotations Anno.

# `set_line`
*since OTP 18.0* 

```erlang
-spec set_line(Line, Anno) -> Anno when Line :: line(), Anno :: anno().
```

Modifies the line of the annotations Anno.

# `set_location`
*since OTP 18.0* 

```erlang
-spec set_location(Location, Anno) -> Anno when Location :: location(), Anno :: anno().
```

Modifies the location of the annotations Anno.

# `set_record`
*since OTP 18.0* 

```erlang
-spec set_record(Record, Anno) -> Anno when Record :: record_local(), Anno :: anno().
```

Modifies the record marker of the annotations Anno.

# `set_text`
*since OTP 18.0* 

```erlang
-spec set_text(Text, Anno) -> Anno when Text :: text(), Anno :: anno().
```

Modifies the text of the annotations Anno.

# `text`
*since OTP 18.0* 

```erlang
-spec text(Anno) -> text() | undefined when Anno :: anno().
```

Returns the text of the annotations Anno. If there is no text, `undefined` is
returned.

# `to_term`
*since OTP 18.0* 

```erlang
-spec to_term(Anno) -> anno_term() when Anno :: anno().
```

Returns the term representing the annotations Anno.

See also [from_term()](`from_term/1`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
