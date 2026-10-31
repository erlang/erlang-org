# `snmp_generic`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmp_generic.erl#L22)

Generic Functions for Implementing SNMP Objects in a Database

The module `snmp_generic` contains generic functions for implementing tables
(and variables) using the SNMP built-in database or Mnesia. These default
functions are used if no instrumentation function is provided for a managed
object in a MIB. Sometimes, it might be necessary to customize the behaviour of
the default functions. For example, in some situations a trap should be sent if
a row is deleted or modified, or some hardware is to be informed, when
information is changed.

The overall structure is shown in the following figure:

```c
         +---------------+
         |   SNMP Agent  |
         +- - - - - - - -+
         |      MIB      |
         +---------------+
                 |
         Association file       (associates a MIB object with
                 |               snmp_generic:table_funct
                 |               snmp_generic:variable_func)
+--------------------------------------+
|           snmp_generic               |  Support for get-next,
|                                      |  RowStatus operations
+----------------------+---------------+
|    snmpa_local_db    |    Mnesia     |  Database
+--------------+-------+---------------+
|     dets     |  ets  |
| (persistent) |       |
+--------------+-------+
```

Each function takes the argument `NameDb`, which is a tuple `{Name, Db}`, to
identify which database the functions should use. `Name` is the symbolic name of
the managed object as defined in the MIB, and `Db` is either `volatile`,
`persistent`, or `mnesia`. If it is `mnesia`, all variables are stored in the
Mnesia table `snmp_variables` which must be a table with two attributes (not a
Mnesia SNMP table). The SNMP tables are stored in Mnesia tables with the same
names as the SNMP tables. All functions assume that a Mnesia table exists with
the correct name and attributes. It is the programmer's responsibility to ensure
this. Specifically, if variables are stored in Mnesia, the table
`snmp_variables` must be created by the programmer. The record definition for
this table is defined in the file `snmp/include/snmp_types.hrl`.

If an instrumentation function in the association file for a variable `myVar`
does not have a name when compiling an MIB, the compiler generates an entry.

```erlang
{myVar, {snmp_generic, variable_func, [{myVar, Db]}}.
```

And for a table:

```erlang
{myTable, {snmp_generic, table_func, [{myTable, Db]}}.
```

## Example

The following example shows an implementation of a table which is stored in
Mnesia, but with some checks performed at set-request operations.

```erlang
myTable_func(new, NameDb) ->   % pass unchanged
  snmp_generic:table_func(new, NameDb).

myTable_func(delete, NameDb) ->   % pass unchanged
  snmp_generic:table_func(delete, NameDb).

%% change row
myTable_func(is_set_ok, RowIndex, Cols, NameDb) ->
  case snmp_generic:table_func(is_set_ok, RowIndex,
                               Cols, NameDb) of
    {noError, 0} ->
      myApplication:is_set_ok(RowIndex, Cols);
    Err ->
      Err
  end;

myTable_func(set, RowIndex, Cols, NameDb) ->
  case snmp_generic:table_func(set, RowIndex, Cols,
                               NameDb),
    {noError, 0} ->
      % Now the row is updated, tell the application
      myApplication:update(RowIndex, Cols);
    Err ->
      Err
  end;

myTable_func(Op, RowIndex, Cols, NameDb) ->   % pass unchanged
  snmp_generic:table_func(Op, RowIndex, Cols, NameDb).
```

The `.funcs` file would look like:

```erlang
{myTable, {myModule, myTable_func, [{myTable, mnesia}]}}.
```

# `column`

```erlang
-type column() :: pos_integer().
```

# `columns`

```erlang
-type columns() :: [column()] | [{column(), Value :: term()}].
```

Is a list of column numbers in the case of a get operation, and a list of column
numbers and values in the case of a set operation.

# `table_info_item`
*not exported* 

```erlang
-type table_info_item() ::
          nbr_of_cols | defvals | status_col | not_accessible | index_types | first_accessible |
          first_own_index.
```

For an ordinary table, the types will be the following:

- **`nbr_of_cols`** - Number of columns.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`defvals`** - A list of default values, ordered by column.

  Value type: [\{Col :: [pos_integer()](`t:erlang:pos_integer/0`), DefVal ::
  [term()](`t:erlang:term/0`)\}]

- **`status_col`** - Column number of the status column.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`not_accessible`** - A sorted list of columns (> first_accessible) that are
  'not-accessible'.

  Value type: [[pos_integer()](`t:erlang:pos_integer/0`)]

- **`index_types`** - A list of [asn1_type()](`t:snmp:asn1_type/0`) for the
  index columns, ordered by column number or an "augment"-tuple (see below).

  Value type: [[asn1_type()](`t:snmp:asn1_type/0`)]

- **`first_accessible`** - The first accessible column.

  Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`first_own_index`** - Column number of the first own index. Will be `0` if
  there is no such index for this table.

  Value type: [non_neg_integer()](`t:erlang:non_neg_integer/0`)

For a augmented table, it will instead look like this:

- **`index_types`** - Value type: \{augments, \{[atom()](`t:erlang:atom/0`),
  [asn1_type()](`t:snmp:asn1_type/0`)\}\}

- **`nbr_of_cols`** - Value type: [pos_integer()](`t:erlang:pos_integer/0`)

- **`not_accessible`** - Value type: [[pos_integer()](`t:erlang:pos_integer/0`)]

- **`first_accessible`** - Value type: [pos_integer()](`t:erlang:pos_integer/0`)

# `get_index_types`

```erlang
-spec get_index_types(Name) -> IndexTypes
                         when Name :: snmpa:name() | snmpa:name_db(), IndexTypes :: [snmp:asn1_type()].
```

Gets the index types of `Name`

This function can be used in instrumentation functions to retrieve the index
types part of the table info.

# `get_status_col`

```erlang
-spec get_status_col(Name, Cols) -> false | {value, StatusCol}
                        when
                            Name :: snmpa:name() | snmpa:name_db(),
                            Cols :: columns(),
                            StatusCol :: term().
```

Gets the value of the status column from `Cols`.

This function can be used in instrumentation functions for `is_set_ok`, `undo`
or `set` to check if the status column of a table is modified.

# `get_table_info`
*since OTP R15B01* 

```erlang
-spec get_table_info(Name, Item :: nbr_of_cols) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: pos_integer();
                    (Name, Item :: defvals) -> Result
                        when
                            Name :: snmpa:name() | snmpa:name_db(),
                            Result :: [{Col, DefVal}],
                            Col :: pos_integer(),
                            DefVal :: term();
                    (Name, Item :: status_col) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: pos_integer();
                    (Name, Item :: not_accessible) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: [pos_integer()];
                    (Name, Item :: index_types) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: [snmp:asn1_type()];
                    (Name, Item :: first_accessible) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: pos_integer();
                    (Name, Item :: first_own_index) -> Result
                        when Name :: snmpa:name() | snmpa:name_db(), Result :: non_neg_integer();
                    (Name, Item :: all) -> Result
                        when
                            Name :: snmpa:name() | snmpa:name_db(),
                            Result :: [{table_info_item(), term()}].
```

Get a specific table info item or, if `Item` has the
value 'all', a two tuple list (property list) is instead 
returned with all the items and their respctive values of the 
given table.

This function can be used in instrumentation functions to
retrieve a given part of the table info.

# `table_func`

```erlang
-spec table_func(Op, NameDb) -> Return
                    when Op :: new | delete, NameDb :: snmpa:name_db(), Return :: term().
```

This is the default instrumentation function for tables.

- The `new` operation creates the table if it does not exist, but only if the
  database is the SNMP internal db.
- The `delete` operation does not delete the table from the database since
  unloading an MIB does not necessarily mean that the table should be destroyed.

If it is possible for a manager to create or delete rows in the table, there
must be a `RowStatus` column for `is_set_ok`, `set` and `undo` to work properly.

The function returns according to the specification of an instrumentation
function.

# `table_func`

```erlang
-spec table_func(Op, RowIndex, Cols, NameDb) -> Return
                    when
                        Op :: get | get_next | is_set_ok | set | undo,
                        RowIndex :: snmp:row_index(),
                        Cols :: columns(),
                        NameDb :: snmpa:name_db(),
                        Return :: term().
```

This is the default instrumentation function for tables.

- The `is_set_ok` operation checks that a row which is to be modified or deleted
  exists, and that a row which is to be created does not exist.
- The `undo` operation does nothing.
- The `set` operation checks if it has enough information to make the row change
  its status from `notReady` to `notInService` (when a row has been been set to
  `createAndWait`). If a row is set to `createAndWait`, columns without a value
  are set to `noinit`. If Mnesia is used, the set functionality is handled
  within a transaction.

If it is possible for a manager to create or delete rows in the table, there
must be a `RowStatus` column for `is_set_ok`, `set` and `undo` to work properly.

The function returns according to the specification of an instrumentation
function.

# `table_get_elements`

```erlang
-spec table_get_elements(NameDb, RowIndex, Cols) -> Values
                            when
                                NameDb :: snmpa:name_db(),
                                RowIndex :: snmp:row_index(),
                                Cols :: columns(),
                                Values :: [noinit | Value],
                                Value :: term().
```

Returns a list with values for all columns in `Cols`. If a column is undefined,
its value is `noinit`.

# `table_next`

```erlang
-spec table_next(NameDb, RestOid) -> Result
                    when
                        NameDb :: snmpa:name_db(),
                        RestOid :: [integer()],
                        Result :: RowIndex | endOfTable,
                        RowIndex :: snmp:row_index().
```

Finds the indices of the next row in the table. `RestOid` does not have to
specify an existing row.

# `table_row_exists`

```erlang
-spec table_row_exists(NameDb, RowIndex) -> Result
                          when
                              NameDb :: snmpa:name_db(),
                              RowIndex :: snmp:row_index(),
                              Result :: boolean().
```

Checks if a row in a table exists.

# `table_set_elements`

```erlang
-spec table_set_elements(NameDb, RowIndex, Columns) -> Result
                            when
                                NameDb :: snmpa:name_db(),
                                RowIndex :: snmp:row_index(),
                                Columns :: [{Column, Value}],
                                Column :: column(),
                                Value :: term(),
                                Result :: boolean().
```

Sets the elements in `Cols` to the row specified by `RowIndex`. No checks are
performed on the new values.

If the Mnesia database is used, this function calls `mnesia:write` to store the
values. This means that this function must be called from within a transaction
(`mnesia:transaction/1`).

# `variable_func`

```erlang
-spec variable_func(Op :: new, Name) -> Result
                       when Name :: snmpa:name() | snmpa:name_db(), Result :: ok | boolean();
                   (Op :: delete, Name) -> Result
                       when Name :: snmpa:name() | snmpa:name_db(), Result :: ok;
                   (Op :: get, Name) -> Result
                       when
                           Name :: snmpa:name() | snmpa:name_db(),
                           Result :: {value, Value} | genErr,
                           Value :: term().
```

This is the default instrumentation function for variables.

- The `new` opeation creates a new variable in the database with a
  default value as defined in the MIB, or a zero value (depending on
  the type).
- The `delete` function does not delete the variable from the database.

The function returns according to the specification of an instrumentation
function.

# `variable_func`

```erlang
-spec variable_func(Op :: is_set_ok, Value, Name) -> Result
                       when Value :: term(), Name :: snmpa:name() | snmpa:name_db(), Result :: noError;
                   (Op :: set, Value, Name) -> Result
                       when
                           Value :: term(),
                           Name :: snmpa:name() | snmpa:name_db(),
                           Result :: noError | commitFailed;
                   (Op :: undo, Value, Name) -> Result
                       when Value :: term(), Name :: snmpa:name() | snmpa:name_db(), Result :: noError.
```

This is the default instrumentation function for variables with operations;
`is_set_ok | set | undo`.

- The `is_set_ok` operation does nothing.
- The `set` operation return `noError` if successful or `commitFailed` otherwise.
- The `undo` operation does nothing.

The function returns according to the specification of an instrumentation
function.

# `variable_get`

```erlang
-spec variable_get(Name) -> {value, Value} | undefined
                      when Name :: snmpa:name() | snmpa:name_db(), Value :: term().
```

Gets the value of a variable.

# `variable_set`

```erlang
-spec variable_set(Name, Value) -> boolean()
                      when Name :: snmpa:name() | snmpa:name_db(), Value :: term().
```

Sets a new value to a variable. The variable is created if it does not exist. No
checks are made on the type of the new value.

Returns `false` if the `NameDb` argument is incorrectly specified, otherwise
`true`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
