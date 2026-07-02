# `odbc`
[🔗](https://github.com/erlang/otp/blob/master/lib/odbc/src/odbc.erl#L25)

Erlang ODBC application

This application provides an Erlang interface to communicate with relational
SQL-databases. It is built on top of Microsofts ODBC interface and therefore
requires that you have an ODBC driver to the database that you want to connect
to.

> #### Note {: .info }
>
> The functions `first/[1,2]`, `last/[1,2]`, `next/[1,2]`, `prev[1,2]` and
> `select/[3,4]` assumes there is a result set associated with the connection to
> work on. Calling the function `select_count/[2,3]` associates such a result
> set with the connection. Calling select_count again will remove the current
> result set association and create a new one. Calling a function which dose not
> operate on an associated result sets, such as `sql_query/[2,3]`, will remove
> the current result set association.
>
> Alas some drivers only support sequential traversal of the result set, e.i.
> they do not support what in the ODBC world is known as scrollable cursors.
> This will have the effect that functions such as `first/[1,2]`, `last/[1,2]`,
> `prev[1,2]`, etc will return `{error, driver_does_not_support_function}`

## Error Handling

The error handling strategy and possible errors sources are described in the
Erlang ODBC [User's Guide.](error_handling.md)

## References

\[1]: Microsoft ODBC 3.0, Programmer's Reference and SDK Guide  
See also http://msdn.microsoft.com/

# `col_name`

```erlang
-type col_name() :: string().
```

Name of column in the result set.

# `common_reason`

```erlang
-type common_reason() :: connection_closed | extended_error() | term().
```

An explanation of what went wrong. For common errors there will be atom
decriptions.

# `connection_reference`

```erlang
-opaque connection_reference()
```

Opaque reference to an ODBC connection as returnded by connect/2.

# `extended_error`

```erlang
-type extended_error() :: {string(), integer(), term()}.
```

extended error type with ODBC and native database error codes, as well as the
base reason that would have been returned had extended_errors not been enabled.

# `n_rows`

```erlang
-type n_rows() :: integer().
```

The number of affected rows for UPDATE, INSERT, or DELETE queries. For other
query types the value is driver defined, and hence should be ignored.

# `odbc_data_type`

```erlang
-type odbc_data_type() ::
          sql_integer | sql_smallint | sql_tinyint |
          {sql_decimal, Precision :: integer(), Scale :: integer()} |
          {sql_numeric, Precision :: integer(), Scale :: integer()} |
          {sql_char, Size :: integer()} |
          {sql_wchar, Size :: integer()} |
          {sql_varchar, Size :: integer()} |
          {sql_wvarchar, Size :: integer()} |
          {sql_float, Precision :: integer()} |
          {sql_wlongvarchar, Size :: integer()} |
          {sql_float, Precision :: integer()} |
          sql_real | sql_double | sql_bit |
          atom().
```

Data type used by ODBC, to learn which Erlang data type corresponds to an ODBC
data type see the Erlang to ODBC data type [mapping](databases.md#type) in the
User's Guide.

# `row`

```erlang
-type row() :: tuple() | list().
```

A tuple, with the number of elements selected from columns in a database row,
containg the values of the columns such as `{value(), value() ... value()}`.

When the `tuple_row` option is set to `off` this is a list containing the values
of the columns in a database row such as `[value(), value() ... value()]`.

Please see `connect/2`.

# `selected`

```erlang
-type selected() :: {selected, [col_name()], [row()]}.
```

Return value for queries that select data from database tabels.

# `updated`

```erlang
-type updated() :: {updated, n_rows()}.
```

Return value for queries that update database tables.

# `value`
*not exported* 

```erlang
-type value() :: null | term().
```

Erlang data type that corresponds to the ODBC data type being handled.

# `commit`

> This function is deprecated. odbc:commit/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec commit(ConnectionReference, CommitMode) -> ok | {error, Reason}
                when
                    ConnectionReference :: connection_reference(),
                    CommitMode :: commit | rollback,
                    Reason ::
                        not_an_explicit_commit_connection | process_not_owner_of_odbc_connection |
                        common_reason().
```

# `commit`

> This function is deprecated. odbc:commit/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec commit(ConnectionReference, CommitMode, TimeOut) -> ok | {error, Reason}
                when
                    ConnectionReference :: connection_reference(),
                    CommitMode :: commit | rollback,
                    TimeOut :: erlang:timeout(),
                    Reason ::
                        not_an_explicit_commit_connection | process_not_owner_of_odbc_connection |
                        common_reason().
```

Commits or rollbacks a transaction. Needed on connections where automatic commit
is turned off.

# `connect`

> This function is deprecated. odbc:connect/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec connect(ConnectionStr, Options) -> {ok, ConnectionReferense} | {error, Reason}
                 when
                     ConnectionStr :: string(),
                     Options ::
                         [{auto_commit, on | off} |
                          {timeout, timeout()} |
                          {binary_strings, on | off} |
                          {tuple_row, on | off} |
                          {scrollable_cursors, on | off} |
                          {trace_driver, on | off} |
                          {extended_errors, on | off}],
                     ConnectionReferense :: connection_reference(),
                     Reason :: port_program_executable_not_found | common_reason().
```

Opens a connection to the database. The connection is associated with the
process that created it and can only be accessed through it. This function may
spawn new processes to handle the connection. These processes will terminate if
the process that created the connection dies or if you call disconnect/1.

If automatic commit mode is turned on, each query will be considered as an
individual transaction and will be automatically committed after it has been
executed. If you want more than one query to be part of the same transaction the
automatic commit mode should be turned off. Then you will have to call commit/3
explicitly to end a transaction.

The default timeout is infinity

If the option binary_strings is turned on all strings will be returned as
binaries and strings inputted to param_query will be expected to be binaries.
The user needs to ensure that the binary is in an encoding that the database
expects. By default this option is turned off.

As default result sets are returned as a lists of tuples. The `TupleMode` option
still exists to keep some degree of backwards compatibility. If the option is
set to off, result sets will be returned as a lists of lists instead of a lists
of tuples.

Scrollable cursors are nice but causes some overhead. For some connections speed
might be more important than flexible data access and then you can disable
scrollable cursor for a connection, limiting the API but gaining speed.

> #### Note {: .info }
>
> Turning the scrollable_cursors option off is noted to make old odbc-drivers
> able to connect that will otherwise fail.

If trace mode is turned on this tells the ODBC driver to write a trace log to
the file SQL.LOG that is placed in the current directory of the erlang emulator.
This information may be useful if you suspect there might be a bug in the erlang
ODBC application, and it might be relevant for you to send this file to our
support. Otherwise you will probably not have much use of this.

> #### Note {: .info }
>
> For more information about the `ConnectStr` see description of the function
> SQLDriverConnect in \[1].

The `extended_errors` option enables extended ODBC error information when an
operation fails. Rather than returning `{error, Reason}`, the failing function
will return `{error, {ODBCErrorCode, NativeErrorCode, Reason}}`. Note that this
information is probably of little use when writing database-independent code,
but can be of assistance in providing more sophisticated error handling when
dealing with a known underlying database.

- `ODBCErrorCode` is the ODBC error string returned by the ODBC driver.
- `NativeErrorCode` is the numeric error code returned by the underlying
  database. The possible values and their meanings are dependent on the database
  being used.
- `Reason` is as per the `Reason` field when extended errors are not enabled.

> #### Note {: .info }
>
> The current implementation spawns a port program written in C that utilizes
> the actual ODBC driver. There is a default timeout of 5000 msec for this port
> program to connect to the Erlang ODBC application. This timeout can be changed
> by setting an application specific environment variable 'port_timeout' with
> the number of milliseconds for the ODBC application. E.g.: \[\{odbc,
> [\{port_timeout, 60000\}]\}] to set it to 60 seconds.

# `describe_table`

> This function is deprecated. odbc:describe_table/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec describe_table(ConnectionReference, Table) -> {ok, Description} | {error, Reason}
                        when
                            ConnectionReference :: connection_reference(),
                            Table :: string(),
                            Description :: [{col_name(), odbc_data_type()}],
                            Reason :: process_not_owner_of_odbc_connection | common_reason().
```

# `describe_table`

> This function is deprecated. odbc:describe_table/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec describe_table(ConnectionReference, Table, TimeOut) -> {ok, Description} | {error, Reason}
                        when
                            ConnectionReference :: connection_reference(),
                            Table :: string(),
                            TimeOut :: erlang:timeout(),
                            Description :: [{col_name(), odbc_data_type()}],
                            Reason :: process_not_owner_of_odbc_connection | common_reason().
```

Queries the database to find out the ODBC data types of the columns of the table
`Table`.

# `disconnect`

> This function is deprecated. odbc:disconnect/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec disconnect(ConnectionReferense) -> ok | {error, Reason}
                    when
                        ConnectionReferense :: connection_reference(),
                        Reason :: process_not_owner_of_odbc_connection | extended_error().
```

Closes a connection to a database. This will also terminate all processes that
may have been spawned when the connection was opened. This call will always
succeed. If the connection cannot be disconnected gracefully it will be brutally
killed. However you may receive an error message as result if you try to
disconnect a connection started by another process.

# `first`

> This function is deprecated. odbc:first/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec first(ConnectionReference) -> Result | {error, Reason}
               when
                   ConnectionReference :: connection_reference(),
                   Result :: selected(),
                   Reason ::
                       result_set_does_not_exist | driver_does_not_support_function |
                       scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                       common_reason().
```

# `first`

> This function is deprecated. odbc:first/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec first(ConnectionReference, TimeOut) -> Result | {error, Reason}
               when
                   ConnectionReference :: connection_reference(),
                   TimeOut :: erlang:timeout(),
                   Result :: selected(),
                   Reason ::
                       result_set_does_not_exist | driver_does_not_support_function |
                       scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                       common_reason().
```

Returns the first row of the result set and positions a cursor at this row.

# `last`

> This function is deprecated. odbc:last/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec last(ConnectionReference) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

# `last`

> This function is deprecated. odbc:last/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec last(ConnectionReference, TimeOut) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  TimeOut :: erlang:timeout(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

Returns the last row of the result set and positions a cursor at this row.

# `next`

> This function is deprecated. odbc:next/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec next(ConnectionReference) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

# `next`

> This function is deprecated. odbc:next/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec next(ConnectionReference, TimeOut) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  TimeOut :: erlang:timeout(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

Returns the next row of the result set relative the current cursor position and
positions the cursor at this row. If the cursor is positioned at the last row of
the result set when this function is called the returned value will be
`{selected, ColNames,[]}` e.i. the list of row values is empty indicating that
there is no more data to fetch.

# `param_query`

> This function is deprecated. odbc:param_query/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec param_query(ConnectionReference, SQLQuery, Params) -> Result | {error, Reason}
                     when
                         ConnectionReference :: connection_reference(),
                         SQLQuery :: string(),
                         Params ::
                             [{odbc_data_type(), [value()]}] |
                             [{odbc_data_type(), in | out | inout, [value()]}],
                         Result :: selected() | updated(),
                         Reason ::
                             driver_does_not_support_function | process_not_owner_of_odbc_connection |
                             common_reason().
```

# `param_query`

> This function is deprecated. odbc:param_query/4 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec param_query(ConnectionReference, SQLQuery, Params, TimeOut) -> Result | {error, Reason}
                     when
                         ConnectionReference :: connection_reference(),
                         SQLQuery :: string(),
                         Params ::
                             [{odbc_data_type(), [value()]}] |
                             [{odbc_data_type(), in | out | inout, [value()]}],
                         TimeOut :: erlang:timeout(),
                         Result :: selected() | updated(),
                         Reason ::
                             driver_does_not_support_function | process_not_owner_of_odbc_connection |
                             common_reason().
```

Executes a parameterized SQL query. For an example see the
["Using the Erlang API"](getting_started.md#param_query) in the Erlang ODBC
User's Guide.

> #### Note {: .info }
>
> Use the function describe_table/\[2,3] to find out which ODBC data type that
> is expected for each column of that table. If a column has a data type that is
> described with capital letters, alas it is not currently supported by the
> param_query function. To learn which Erlang data type corresponds to an ODBC
> data type see the Erlang to ODBC data type [mapping](databases.md#type) in the
> User's Guide.

# `prev`

> This function is deprecated. odbc:prev/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec prev(ConnectionReference) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

# `prev`

> This function is deprecated. odbc:prev/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec prev(ConnectionReference, TimeOut) -> Result | {error, Reason}
              when
                  ConnectionReference :: connection_reference(),
                  TimeOut :: erlang:timeout(),
                  Result :: selected(),
                  Reason ::
                      result_set_does_not_exist | driver_does_not_support_function |
                      scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                      common_reason().
```

Returns the previous row of the result set relative the current cursor position
and positions the cursor at this row.

# `select`

> This function is deprecated. odbc:select/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec select(ConnectionReference, Position, N) -> Result | {error, Reason}
                when
                    ConnectionReference :: connection_reference(),
                    Position :: next | {relative, integer()} | {absolute, integer()},
                    N :: integer(),
                    Result :: selected(),
                    Reason ::
                        result_set_does_not_exist | driver_does_not_support_function |
                        scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                        common_reason().
```

# `select`

> This function is deprecated. odbc:select/4 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec select(ConnectionReference, Position, N, TimeOut) -> Result | {error, Reason}
                when
                    ConnectionReference :: connection_reference(),
                    Position :: next | {relative, integer()} | {absolute, integer()},
                    N :: integer(),
                    TimeOut :: erlang:timeout(),
                    Result :: selected(),
                    Reason ::
                        result_set_does_not_exist | driver_does_not_support_function |
                        scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                        common_reason().
```

Selects `N` consecutive rows of the result set. If `Position` is `next` it is
semantically equivalent of calling `next/[1,2]` `N` times. If `Position` is
`{relative, Pos}`, `Pos` will be used as an offset from the current cursor
position to determine the first selected row. If `Position` is
`{absolute, Pos}`, `Pos` will be the number of the first row selected. After
this function has returned the cursor is positioned at the last selected row. If
there is less then `N` rows left of the result set the length of `Rows` will be
less than `N`. If the first row to select happens to be beyond the last row of
the result set, the returned value will be `{selected, ColNames,[]}` e.i. the
list of row values is empty indicating that there is no more data to fetch.

# `select_count`

> This function is deprecated. odbc:select_count/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec select_count(ConnectionReference, SQLQuery) -> {ok, NrRows} | {error, Reason}
                      when
                          ConnectionReference :: connection_reference(),
                          SQLQuery :: string(),
                          NrRows :: n_rows(),
                          Reason :: process_not_owner_of_odbc_connection | common_reason().
```

# `select_count`

> This function is deprecated. odbc:select_count/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec select_count(ConnectionReference, SQLQuery, TimeOut) -> {ok, NrRows} | {error, Reason}
                      when
                          ConnectionReference :: connection_reference(),
                          SQLQuery :: string(),
                          TimeOut :: erlang:timeout(),
                          NrRows :: n_rows(),
                          Reason :: process_not_owner_of_odbc_connection | common_reason().
```

Executes a SQL SELECT query and associates the result set with the connection. A
cursor is positioned before the first row in the result set and the tuple
`{ok, NrRows}` is returned.

> #### Note {: .info }
>
> Some drivers may not have the information of the number of rows in the result
> set, then `NrRows` will have the value `undefined`.

# `sql_query`

> This function is deprecated. odbc:sql_query/2 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec sql_query(ConnectionReference, SQLQuery) -> Result | {error, Reason}
                   when
                       ConnectionReference :: connection_reference(),
                       SQLQuery :: string(),
                       Result :: updated() | selected(),
                       Reason :: process_not_owner_of_odbc_connection | common_reason().
```

# `sql_query`

> This function is deprecated. odbc:sql_query/3 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec sql_query(ConnectionReference, SQLQuery, TimeOut) -> Result | {error, Reason}
                   when
                       ConnectionReference :: connection_reference(),
                       SQLQuery :: string(),
                       TimeOut :: erlang:timeout(),
                       Result :: updated() | selected(),
                       Reason :: process_not_owner_of_odbc_connection | common_reason().
```

Executes a SQL query or a batch of SQL queries. If it is a SELECT query the
result set is returned, on the format `{selected, ColNames, Rows}`. For other
query types the tuple `{updated, NRows}` is returned, and for batched queries,
if the driver supports them, this function can also return a list of result
tuples.

> #### Note {: .info }
>
> Some drivers may not have the information of the number of affected rows
> available and then the return value may be `{updated, undefined} `.
>
> The list of column names is ordered in the same way as the list of values of a
> row, e.g. the first `ColName` is associated with the first `Value` in a `Row`.

# `start`

> This function is deprecated. odbc:start/0 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec start() -> ok | {error, Reason} when Reason :: term().
```

# `start`

> This function is deprecated. odbc:start/1 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec start(Type) -> ok | {error, Reason}
               when Type :: permanent | transient | temporary, Reason :: term().
```

Starts the odbc application. Default type is temporary.
[See application(3)](`m:application`)

# `stop`

> This function is deprecated. odbc:stop/0 is deprecated; Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low..

```erlang
-spec stop() -> ok.
```

Stops the odbc application. [See application(3)](`m:application`)

---

*Consult [api-reference.md](api-reference.md) for complete listing*
