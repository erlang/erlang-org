# `snmp_index`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_index.erl#L22)

Abstract Data Type for SNMP Indexing

The module `snmp_index` implements an Abstract Data Type (ADT) for an SNMP index
structure for SNMP tables. It is implemented as an ets table of the ordered_set
data-type, which means that all operations are O(log n). In the table, the key
is an ASN.1 OBJECT IDENTIFIER.

This index is used to separate the implementation of the SNMP ordering from the
actual implementation of the table. The SNMP ordering, that is implementation of
GET NEXT, is implemented in this module.

For example, suppose there is an SNMP table, which is best implemented in Erlang
as one process per SNMP table row. Suppose further that the INDEX in the SNMP
table is an OCTET STRING. The index structure would be created as follows:

```text
snmp_index:new(string)
```

For each new process we create, we insert an item in an `snmp_index` structure:

```erlang
new_process(Name, SnmpIndex) ->
  Pid = start_process(),
  NewSnmpIndex =
    snmp_index:insert(SnmpIndex, Name, Pid),
  <...>
```

With this structure, we can now map an OBJECT IDENTIFIER in e.g. a GET NEXT
request, to the correct process:

```erlang
get_next_pid(Oid, SnmpIndex) ->
  {ok, {_, Pid}} = snmp_index:get_next(SnmpIndex, Oid),
  Pid.
```

## Warnings

> #### Warning {: .warning }
>
> [](){: #1 } All API functions that update the index return a `NewIndex` term.
> This is for backward compatibility with a previous implementation that used a
> B+ tree written purely in Erlang for the index. The `NewIndex` return value
> can now be ignored. The return value is now the unchanged table identifier for
> the ets table.
>
> The implementation using ets tables introduces a semantic incompatibility with
> older implementations. In those older implementations, using pure Erlang
> terms, the index was garbage collected like any other Erlang term and did not
> have to be deleted when discarded. An ets table is deleted only when the
> process creating it explicitly deletes it or when the creating process
> terminates.
>
> A new interface [`delete/1`](`delete/1`) is now added to handle the case when
> a process wants to discard an index table (i.e. to build a completely new).
> Any application using transient snmp indexes has to be modified to handle
> this.
>
> As an snmp adaption usually keeps the index for the whole of the systems
> lifetime, this is rarely a problem.

# `index`

```erlang
-opaque index()
```

This type denotes an snmp index structure.

# `key`

```erlang
-type key() :: key_spec() | tuple().
```

This type correlates to the `t:key_types/0` type. If the `t:key_types/0` is a
single atom, the corresponding `t:key/0` is a single type as well, but if the
`t:key_types/0` is a tuple, `t:key/0` must be a tuple of the same size.

In the example above, valid `keys` could be `{"hi", "mom"}` and
`{"no", "thanks"}`, whereas `"hi"`, `{"hi", 42}` and `{"hello", "there"}` would
be invalid.

There is no way to propely describe this type in the erlang type language, which
is why `t:tuple/0` was used above. The proper definition looks like:

`key() = key_spec() | {key_spec(), key_spec(), ...}`

# `key_spec`

```erlang
-type key_spec() :: string() | integer().
```

# `key_types`

```erlang
-type key_types() :: type_spec() | tuple().
```

This type is used when creating the index structure, and the `t:key/0` type is
used when inserting and deleting items from the structure.

If the INDEX column is of type INTEGER, or derived from INTEGER, the
corresponding type should be `integer`. If it is a variable length type (e.g.
OBJECT IDENTIFIER, OCTET STRING), the corresponding type should be `string`.
Finally, if the type is of variable length, but with a fixed size restriction
(e.g. IpAddress), the corresponding type should be `fix_string`.

There is no way to propely describe this type in the erlang type language, which
is why `t:tuple/0` was used above. The proper definition looks like:

`key_types = type_spec() | {type_spec(), type_spec(), ...}`

# `type_spec`

```erlang
-type type_spec() :: fix_string | string | integer.
```

# `delete`

```erlang
-spec delete(Index) -> true when Index :: index().
```

Deletes a complete index structure (i.e. the ets table holding the index). The
index can no longer be referenced after this call. See the
[warning note](`m:snmp_index#1`) above.

# `delete`

```erlang
-spec delete(Index, Key) -> NewIndex when Index :: index(), Key :: key(), NewIndex :: index().
```

Deletes a key and its value from the index structure. Returns a new structure.

# `get`

```erlang
-spec get(Index, KeyOid) -> {ok, {KeyOid, Value}} | undefined
             when Index :: index(), KeyOid :: snmp:oid(), Value :: term().
```

Gets the item with key `KeyOid`. Could be used from within an SNMP
instrumentation function.

# `get_last`

```erlang
-spec get_last(Index) -> {ok, {KeyOid, Value}} | undefined
                  when Index :: index(), KeyOid :: snmp:oid(), Value :: term().
```

Gets the last item in the index structure.

# `get_next`

```erlang
-spec get_next(Index, KeyOid) -> {ok, {NextKeyOid, Value}} | undefined
                  when Index :: index(), KeyOid :: snmp:oid(), NextKeyOid :: snmp:oid(), Value :: term().
```

Gets the next item in the SNMP lexicographic ordering, after `KeyOid` in the
index structure. `KeyOid` does not have to refer to an existing item in the
index.

# `insert`

```erlang
-spec insert(Index, Key, Value) -> NewIndex
                when Index :: index(), Key :: key(), Value :: term(), NewIndex :: index().
```

Inserts a new key value tuple into the index structure. If an item with the same
key already exists, the new `Value` overwrites the old value.

# `key_to_oid`

```erlang
-spec key_to_oid(Index, Key) -> KeyOid when Index :: index(), Key :: key(), KeyOid :: snmp:oid().
```

Converts `Key` to an OBJECT IDENTIFIER.

# `new`

```erlang
-spec new(KeyTypes) -> Index when KeyTypes :: key_types(), Index :: index().
```

Create an new anonymous snmp index structure.

# `new`
*since OTP 27.0* 

```erlang
-spec new(KeyTypes, Name) -> Index when KeyTypes :: key_types(), Name :: atom(), Index :: index().
```

Creates a new named snmp index structure.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
