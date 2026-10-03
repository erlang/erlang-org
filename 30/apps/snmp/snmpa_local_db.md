# `snmpa_local_db`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_local_db.erl#L22)

The SNMP built-in database

The module `snmpa_local_db` contains functions for implementing tables (and
variables) using the SNMP built-in database. The database exists in two
instances, one volatile and one persistent. The volatile database is implemented
with ets. The persistent database is implemented with dets.

There is a scaling problem with this database.

- Insertions and deletions are inefficient for large tables.

This problem is best solved by using Mnesia instead.

The following functions describe the interface to `snmpa_local_db`. Each
function has a Mnesia equivalent. The argument `NameDb` is a tuple `{Name, Db}`
where `Name` is the symbolic name of the managed object (as defined in the MIB),
and `Db` is either `volatile` or `persistent`. `mnesia` is not possible since
all these functions are `snmpa_local_db` specific.

## Common Data Types

In the functions defined below, the following limitation applies:

- `Db = volatile | persistent`

### See Also

ets(3), dets(3), snmp_generic(3)

# `dump`

```erlang
-spec dump() -> ok | {error, Reason} when Reason :: term().
```

This function can be used to manually dump the database to file.

# `match`

```erlang
-spec match(Table, Pattern) -> [Match]
               when
                   Table :: snmpa:name_db() | snmpa:name(),
                   Pattern :: ets:match_pattern(),
                   Match :: term().
```

Performs an ets/dets matching on the table.

See `ets:match/2` for a description of `Pattern` and the return values.

# `print`

```erlang
-spec print() -> term().
```

# `print`

```erlang
-spec print(Table) -> term() when Table :: snmpa:name().
```

# `print`

```erlang
-spec print(Table, Db) -> term() when Table :: snmpa:name(), Db :: volatile | persistent.
```

Prints the contents of the database on screen. This is useful for debugging
since the `STANDARD-MIB` and `OTP-SNMPEA-MIB` (and maybe your own MIBs) are
stored in `snmpa_local_db`.

`Table` is an atom for a table in the database. When no name is supplied, the
whole database is shown.

Note that these functions does not actually print, using io:format/2, instead
they (just) return the information. If executed in a shell, the information will
then be displayed (probably truncated) there.

A better use would be:

```text
	  io:format("~p~n", [snmpa_local_db:print()]).
```

# `table_create`

```erlang
-spec table_create(Table) -> boolean() when Table :: snmpa:name_db() | snmpa:name().
```

Creates a table. If the table already exist, the old copy is destroyed.

Returns `false` if the `NameDb` argument is incorrectly specified, `true`
otherwise.

Database (only table name specified) defaults to `volatile`.

# `table_create_row`

```erlang
-spec table_create_row(Table, RowIndex, Row) -> boolean()
                          when
                              Table :: snmpa:name_db() | snmpa:name(),
                              RowIndex :: snmp:row_index(),
                              Row :: tuple().
```

Creates a row in a table. `Row` is a tuple with values for all columns,
including the index columns.

Database (only table name specified) defaults to `volatile`.

# `table_delete`

```erlang
-spec table_delete(Table) -> true when Table :: snmpa:name_db() | snmpa:name().
```

Deletes a table.

Database (only table name specified) defaults to `volatile`.

# `table_delete_row`

```erlang
-spec table_delete_row(Table, RowIndex) -> boolean()
                          when Table :: snmpa:name_db() | snmpa:name(), RowIndex :: snmp:row_index().
```

Deletes the row in the table.

Database (only table name specified) defaults to `volatile`.

# `table_exists`

```erlang
-spec table_exists(Table) -> boolean() when Table :: snmpa:name_db() | snmpa:name().
```

Checks if a table exists.

Database (only table name specified) defaults to `volatile`.

# `table_get_element`
*since OTP 27.0* 

```erlang
-spec table_get_element(Table, RowIndex, Col) -> {value, Value} | undefined
                           when
                               Table :: snmpa:name_db() | snmpa:name(),
                               RowIndex :: snmp:row_index(),
                               Col :: snmp:column(),
                               Value :: term().
```

Get a column value (element) from a row of the table.

Database (only table name specified) defaults to `volatile`.

This function has existed for long time,
but not had a proper since tag, so to simplify
we set the since tag to when it was documented.

# `table_get_row`

```erlang
-spec table_get_row(Table, RowIndex) -> Row | undefined
                       when
                           Table :: snmpa:name_db() | snmpa:name(),
                           RowIndex :: snmp:row_index(),
                           Row :: tuple().
```

`Row` is a tuple with values for all columns, including the index columns.

Database (only table name specified) defaults to `volatile`.

# `table_set_elements`
*since OTP 27.0* 

```erlang
-spec table_set_elements(Table, RowIndex, Cols) -> boolean()
                            when
                                Table :: snmpa:name_db() | snmpa:name(),
                                RowIndex :: snmp:row_index(),
                                Cols :: [{Col, Value}],
                                Col :: snmp:column(),
                                Value :: term().
```

Update the specified columnar objects of the row of this table.

Database (only table name specified) defaults to `volatile`.

This function has existed for long time,
but not had a proper since tag, so to simplify
we set the since tag to when it was documented.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
