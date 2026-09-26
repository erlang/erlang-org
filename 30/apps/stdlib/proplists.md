# `proplists`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/proplists.erl#L23)

Support functions for property lists.

Property lists are ordinary lists containing entries in the form of either
tuples, whose first elements are keys used for lookup and insertion, or atoms,
which work as shorthand for tuples `{Atom, true}`. (Other terms are allowed in
the lists, but are ignored by this module.) If there is more than one entry in a
list for a certain key, the first occurrence normally overrides any later
(irrespective of the arity of the tuples).

Property lists are useful for representing inherited properties, such as options
passed to a function where a user can specify options overriding the default
settings, object properties, annotations, and so on.

Two keys are considered equal if they match (`=:=`). That is, numbers are
compared literally rather than by value, so that, for example, `1` and `1.0` are
different keys.

# `property`

```erlang
-type property() :: atom() | tuple().
```

A property item within a list

# `proplist`

```erlang
-type proplist() :: [property()].
```

A list of `t:property/0`, also knows as a proplist.

# `append_values`

```erlang
-spec append_values(Key, ListIn) -> ListOut when Key :: term(), ListIn :: [term()], ListOut :: [term()].
```

Similar to `get_all_values/2`, but each value is wrapped in a list unless it is
already itself a list. The resulting list of lists is concatenated. This is
often useful for "incremental" options.

_Example:_

```erlang
append_values(a, [{a, [1,2]}, {b, 0}, {a, 3}, {c, -1}, {a, [4]}])
```

returns:

```erlang
[1,2,3,4]
```

# `compact`

```erlang
-spec compact(ListIn) -> ListOut when ListIn :: [property()], ListOut :: [property()].
```

Minimizes the representation of all entries in the list. This is equivalent to
`[property(P) || P <- ListIn]`.

See also `property/1`, `unfold/1`.

# `delete`

```erlang
-spec delete(Key, List) -> List when Key :: term(), List :: [term()].
```

Deletes all entries associated with `Key` from `List`.

# `expand`

```erlang
-spec expand(Expansions, ListIn) -> ListOut
                when
                    Expansions :: [{Property :: property(), Expansion :: [term()]}],
                    ListIn :: [term()],
                    ListOut :: [term()].
```

Expands particular properties to corresponding sets of properties (or other
terms).

For each pair `{Property, Expansion}` in `Expansions`: if `E` is the
first entry in `ListIn` with the same key as `Property`, and `E` and `Property`
have equivalent normal forms, then `E` is replaced with the terms in
`Expansion`, and any following entries with the same key are deleted from
`ListIn`.

For example, the following expressions all return `[fie, bar, baz, fum]`:

```erlang
expand([{foo, [bar, baz]}], [fie, foo, fum])
expand([{{foo, true}, [bar, baz]}], [fie, foo, fum])
expand([{{foo, false}, [bar, baz]}], [fie, {foo, false}, fum])
```

However, no expansion is done in the following call because `{foo, false}`
shadows `foo`:

```erlang
expand([{{foo, true}, [bar, baz]}], [{foo, false}, fie, foo, fum])
```

Notice that if the original property term is to be preserved in the result when
expanded, it must be included in the expansion list. The inserted terms are not
expanded recursively. If `Expansions` contains more than one property with the
same key, only the first occurrence is used.

See also `normalize/2`.

# `from_map`
*since OTP 24.0* 

```erlang
-spec from_map(Map) -> List
                  when Map :: #{Key => Value}, List :: [{Key, Value}], Key :: term(), Value :: term().
```

Converts the map `Map` to a property list.

# `get_all_values`

```erlang
-spec get_all_values(Key, List) -> [term()] when Key :: term(), List :: [term()].
```

Similar to `get_value/2`, but returns the list of values for _all_ entries
`{Key, Value}` in `List`. If no such entry exists, the result is the empty list.

# `get_bool`

```erlang
-spec get_bool(Key, List) -> boolean() when Key :: term(), List :: [term()].
```

Returns the value of a boolean key/value option. If
[`lookup(Key, List)`](`lookup/2`) would yield `{Key, true}`, this function
returns `true`, otherwise `false`.

See also `get_value/2`, `lookup/2`.

# `get_keys`

```erlang
-spec get_keys(List) -> [term()] when List :: [term()].
```

Returns an unordered list of the keys used in `List`, not containing duplicates.

# `get_value`

```erlang
-spec get_value(Key, List) -> term() when Key :: term(), List :: [term()].
```

# `get_value`

```erlang
-spec get_value(Key, List, Default) -> term() when Key :: term(), List :: [term()], Default :: term().
```

Returns the value of a simple key/value property in `List`. If
[`lookup(Key, List)`](`lookup/2`) would yield `{Key, Value}`, this function
returns the corresponding `Value`, otherwise `Default`.

See also `get_all_values/2`, `get_bool/2`, `get_value/2`, `lookup/2`.

# `is_defined`

```erlang
-spec is_defined(Key, List) -> boolean() when Key :: term(), List :: [term()].
```

Returns `true` if `List` contains at least one entry associated with `Key`,
otherwise `false`.

# `lookup`

```erlang
-spec lookup(Key, List) -> none | tuple() when Key :: term(), List :: [term()].
```

Returns the first entry associated with `Key` in `List`, if one exists,
otherwise returns `none`. For an atom `A` in the list, the tuple `{A, true}` is
the entry associated with `A`.

See also `get_bool/2`, `get_value/2`, `lookup_all/2`.

# `lookup_all`

```erlang
-spec lookup_all(Key, List) -> [tuple()] when Key :: term(), List :: [term()].
```

Returns the list of all entries associated with `Key` in `List`. If no such
entry exists, the result is the empty list.

See also `lookup/2`.

# `normalize`

```erlang
-spec normalize(ListIn, Stages) -> ListOut
                   when
                       ListIn :: [term()],
                       Stages :: [Operation],
                       Operation :: {aliases, Aliases} | {negations, Negations} | {expand, Expansions},
                       Aliases :: [{Key, Key}],
                       Negations :: [{Key, Key}],
                       Expansions :: [{Property :: property(), Expansion :: [term()]}],
                       ListOut :: [term()].
```

Passes `ListIn` through a sequence of substitution/expansion stages. For an
`aliases` operation, function `substitute_aliases/2` is applied using the
specified list of aliases:

- For a `negations` operation,
  [`substitute_negations/2`](`substitute_negations/2`) is applied using the
  specified negation list.
- For an `expand` operation, function `expand/2` is applied using the specified
  list of expansions.

The final result is automatically compacted (compare `compact/1`).

Typically you want to substitute negations first, then aliases, then perform one
or more expansions (sometimes you want to pre-expand particular entries before
doing the main expansion). You might want to substitute negations and/or aliases
repeatedly, to allow such forms in the right-hand side of aliases and expansion
lists.

See also `substitute_negations/2`.

# `property`

```erlang
-spec property(PropertyIn) -> PropertyOut when PropertyIn :: property(), PropertyOut :: property().
```

Creates a normal form (minimal) representation of a property. If `PropertyIn` is
`{Key, true}`, where `Key` is an atom, `Key` is returned, otherwise the whole
term `PropertyIn` is returned.

See also `property/2`.

# `property`

```erlang
-spec property(Key, Value) -> Property
                  when Key :: term(), Value :: term(), Property :: atom() | {term(), term()}.
```

Creates a normal form (minimal) representation of a simple key/value property.
Returns `Key` if `Value` is `true` and `Key` is an atom, otherwise a tuple
`{Key, Value}` is returned.

See also `property/1`.

# `split`

```erlang
-spec split(List, Keys) -> {Lists, Rest}
               when List :: [term()], Keys :: [term()], Lists :: [[term()]], Rest :: [term()].
```

Partitions `List` into a list of sublists and a remainder.

`Lists` contains one sublist for each key in `Keys`, in the corresponding order.
The relative order of the elements in each sublist is preserved from the original `List`.
`Rest` contains the elements in `List` that are not associated with any of the
specified keys, also with their original relative order preserved.

_Example:_

```erlang
split([{c, 2}, {e, 1}, a, {c, 3, 4}, d, {b, 5}, b], [a, b, c])
```

returns:

```erlang
{[[a], [{b, 5}, b],[{c, 2}, {c, 3, 4}]], [{e, 1}, d]}
```

# `substitute_aliases`

```erlang
-spec substitute_aliases(Aliases, ListIn) -> ListOut
                            when
                                Aliases :: [{Key, Key}],
                                Key :: term(),
                                ListIn :: [term()],
                                ListOut :: [term()].
```

Substitutes keys of properties. For each entry in `ListIn`, if it is associated
with some key `K1` such that `{K1, K2}` occurs in `Aliases`, the key of the
entry is changed to `K2`. If the same `K1` occurs more than once in `Aliases`,
only the first occurrence is used.

For example,
[`substitute_aliases([{color, colour}], L)`](`substitute_aliases/2`) replaces
all tuples `{color, ...}` in `L` with `{colour, ...}`, and all atoms `color`
with `colour`.

See also `normalize/2`, `substitute_negations/2`.

# `substitute_negations`

```erlang
-spec substitute_negations(Negations, ListIn) -> ListOut
                              when
                                  Negations :: [{Key1, Key2}],
                                  Key1 :: term(),
                                  Key2 :: term(),
                                  ListIn :: [term()],
                                  ListOut :: [term()].
```

Substitutes keys of boolean-valued properties and simultaneously negates their
values.

For each entry in `ListIn`, if it is associated with some key `K1` such
that `{K1, K2}` occurs in `Negations`: if the entry was `{K1, true}`, it is
replaced with `{K2, false}`, otherwise with `K2`, thus changing the name of the
option and simultaneously negating the value specified by
[`get_bool(Key, ListIn)`](`get_bool/2`). If the same `K1` occurs more than once
in `Negations`, only the first occurrence is used.

For example,
[`substitute_negations([{no_foo, foo}], L)`](`substitute_negations/2`) replaces
any atom `no_foo` or tuple `{no_foo, true}` in `L` with `{foo, false}`, and any
other tuple `{no_foo, ...}` with `foo`.

See also `get_bool/2`, `normalize/2`, `substitute_aliases/2`.

# `to_map`
*since OTP 24.0* 

```erlang
-spec to_map(List) -> Map
                when
                    List :: [Shorthand | {Key, Value} | term()],
                    Map :: #{Shorthand => true, Key => Value},
                    Shorthand :: atom(),
                    Key :: term(),
                    Value :: term().
```

Converts the property list `List` to a map.

Shorthand atom values in `List` will be expanded to an association of the form
`Atom => true`. Tuples of the form `{Key, Value}` in `List` will be converted to
an association of the form `Key => Value`. Anything else will be silently
ignored.

If the same key appears in `List` multiple times, the value of the one appearing
nearest to the head of `List` will be in the result map, that is the value that
would be returned by a call to [`get_value(Key, List)`](`get_value/2`).

_Example:_

```erlang
to_map([a, {b, 1}, {c, 2}, {c, 3}])
```

returns:

```erlang
#{a => true, b => 1, c => 2}
```

# `to_map`
*since OTP 24.0* 

```erlang
-spec to_map(List, Stages) -> Map
                when
                    List :: [term()],
                    Stages :: [Operation],
                    Operation :: {aliases, Aliases} | {negations, Negations} | {expand, Expansions},
                    Aliases :: [{Key, Key}],
                    Negations :: [{Key, Key}],
                    Expansions :: [{Property :: property(), Expansion :: [term()]}],
                    Map :: #{term() => term()}.
```

Converts the property list `List` to a map after applying the normalizations
given in `Stages`.

See also `normalize/2`, `to_map/1`.

# `unfold`

```erlang
-spec unfold(ListIn) -> ListOut when ListIn :: [term()], ListOut :: [term()].
```

Unfolds all occurrences of atoms in `ListIn` to tuples `{Atom, true}`.

See also `compact/1`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
