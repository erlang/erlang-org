# `records`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/records.erl#L22)

Native records processing functions.

This module contains functions for creating and inspecting native records.

> #### Warning {: .warning }
>
> The main purpose of the functions in this module is for debugging,
> implementing library functions (such as printing of native records),
> and implementing tools (such as the Debugger). Use with care in
> production code.

# `create_options`
*not exported* *since OTP 29.0* 

```erlang
-type create_options() :: #{is_exported := boolean()}.
```

Options that can be used when creating a native record.

- The value for the `is_exported` key should be a boolean indicating
whether this record value is exported.

Consumed by:

- [`records:create/4`](`create/4`)

# `create`
*since OTP 29.0* 

```erlang
-spec create(Module :: module(),
             RecordName :: atom(),
             Fields :: [{atom(), term()}],
             Options :: create_options()) ->
                record().
```

Takes `Fields` and creates a native record `Record` with module
`Module` and name `RecordName`.

The native record definition does not have to exist in the given module,
and if it exists, it will not be used in any way.

> #### Warning {: .warning }
>
> Because this function can create records that are inconsistent with
> the record definition in module `Module`, it can cause very subtle
> bugs; therefore, its use in applications is strongly discouraged.

The call can fail in the following ways:

- With a `{badrecord,Record}` exception if `Module` or `RecordName` are not atoms.
- With a `{badmap, OptionsMap}` exception if `OptionsMap` is not a map.
- With a `{badfield, Field}` exception if a field name in `Fields` is not an atom.
- With a `badarg` exception if `Options` does not have the `is_exported` key.
- With a `badarg` exception for other invalid arguments, such as a field name
being repeated in `Fields`.

## Examples

```erlang
1> R = records:create(test, a, [{z,3}, {x,1}, {y,2}], #{is_exported => true}).
#test:a{z = 3,x = 1,y = 2}
2> records:is_exported(R).
true
3> records:create(test, a, [{42,1}], #{is_exported => true}).
** exception error: bad field name: 42 in #test:a{}
     in function  records:create/4
        called as records:create(test,a,[{42,1}],#{is_exported => true})
4> records:create(test, a, [{x,1}, {x,2}], #{is_exported => true}).
** exception error: bad argument
     in function  records:create/4
        called as records:create(test,a,[{x,1},{x,2}],#{is_exported => true})
```

# `get`
*since OTP 29.0* 

```erlang
-spec get(Key, Record) -> dynamic() when Key :: atom(), Record :: record().
```

Returns value `Value` associated with `Key` if native record `Record`
contains `Key`.

This call fails with a `badarg` exception if `Record` is not a native record
or if `Key` does not exist in `Record`.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}], #{is_exported=>false}).
#test:a{x = 1}
2> records:get(x, R).
1
3> records:get(y, R).
** exception error: bad argument
     in function  records:get/2
        called as records:get(y,#test:a{x = 1})
```

# `get_definition`
*since OTP 29.0* 

```erlang
-spec get_definition(Module :: module(), RecordName :: atom()) ->
                        {create_options(),
                         [{FieldName :: atom(), Default :: dynamic()} | (FieldName :: atom())]}.
```

Retrieves the definition for native record `Name` in module `Module`.

# `get_field_names`
*since OTP 29.0* 

```erlang
-spec get_field_names(Record) -> [Name] when Record :: record(), Name :: atom().
```

Returns a complete list of field names (keys) in native record `Record`, in
the order of declaration.

This call fails with a `{badrecord,Record}` exception if `Record` is not a
native record.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}], #{is_exported=>false}).
#test:a{x = 1}
2> records:get_field_names(R).
[x]
3> records:get_field_names({x,y}).
** exception error: {badrecord,{x,y}}
     in function  records:get_field_names/1
        called as records:get_field_names({x,y})
```

# `get_module`
*since OTP 29.0* 

```erlang
-spec get_module(Record) -> Module when Record :: record(), Module :: module().
```

Returns the module `Module` in which the native record `Record` is defined.

This call fails with a `badarg` exception if `Record` is not a native record.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}], #{is_exported=>false}).
#test:a{x = 1}
2> records:get_module(R).
test
3> records:get_module(#{}).
** exception error: bad argument
     in function  records:get_module/1
        called as records:get_module(#{})
```

# `get_name`
*since OTP 29.0* 

```erlang
-spec get_name(Record) -> Name when Record :: record(), Name :: atom().
```

Returns the name `Name` of the native record `Record`.

This call fails with a `badarg` exception if `Record` is not a native record.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}], #{is_exported=>false}).
#test:a{x = 1}
2> records:get_name(R).
a
3> records:get_name(#{}).
** exception error: bad argument
     in function  records:get_name/1
        called as records:get_name(#{})
```

# `is_exported`
*since OTP 29.0* 

```erlang
-spec is_exported(record()) -> boolean().
```

Returns `true` if native record `Record` is exported; otherwise, returns
`false`.

This call fails with a `{badrecord,Record}` exception if `Record` is not a
native record.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}], #{is_exported=>false}).
#test:a{x = 1}
2> records:is_exported(R).
false
3> records:is_exported({x,y}).
** exception error: {badrecord,{x,y}}
     in function  records:is_exported/1
        called as records:is_exported({x,y})
```

# `update`
*since OTP 29.0* 

```erlang
-spec update(Src :: record(),
             Module :: module(),
             RecordName :: atom(),
             FieldsMap :: #{atom() => term()}) ->
                record().
```

Takes a map `FieldsMap` and updates the values in native record `Src`
as defined in module `Module` with name `RecordName`.

The call can fail in the following ways:

- With a `{badrecord,Record}` exception if `Src` is not a native
record defined in module `Module` with name `RecordName`.
- With a `{badmap, FieldsMap}` if `FieldsMap` is not a map.
- With a `{badfield, Field}` if `Field` in `FieldsMap` does not
exist in `Record`.

## Examples

```erlang
1> R = records:create(test, a, [{x,1}, {y,2}, {z,3}], #{is_exported => false}).
#test:a{x = 1,y = 2,z = 3}
2> Updated = records:update(R, test, a, #{x => 10, y => 20}).
#test:a{x = 10,y = 20,z = 3}
3> records:update(R, test, a, #{w => 42}).
** exception error: bad field name: w in #test:a{}
     in function  records:update/4
        called as records:update(#test:a{x = 1,y = 2,z = 3},test,a,#{w => 42})
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
