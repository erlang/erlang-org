# `win32reg`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/win32reg.erl#L22)

Provides access to the registry on Windows.

This module provides read and write access to the registry on Windows. It is
essentially a port driver wrapped around the Win32 API calls for accessing the
registry.

The registry is a hierarchical database, used to store various system and
software information in Windows. It contains installation data, and is updated
by installers and system programs. The Erlang installer updates the registry by
adding data that Erlang needs.

The registry contains keys and values. Keys are like the directories in a file
system, they form a hierarchy. Values are like files, they have a name and a
value, and also a type.

Paths to keys are left to right, with subkeys to the right and backslash between
keys. (Remember that backslashes must be doubled in Erlang strings.) Case is
preserved but not significant.

For example, `"\\hkey_local_machine\\software\\Ericsson\\Erlang\\5.0"` is the
key for the installation data for the latest Erlang release.

There are six entry points in the Windows registry, top-level keys. They can be
abbreviated in this module as follows:

```text
Abbreviation     Registry key
============     ============
hkcr             HKEY_CLASSES_ROOT
current_user     HKEY_CURRENT_USER
hkcu             HKEY_CURRENT_USER
local_machine    HKEY_LOCAL_MACHINE
hklm             HKEY_LOCAL_MACHINE
users            HKEY_USERS
hku              HKEY_USERS
current_config   HKEY_CURRENT_CONFIG
hkcc             HKEY_CURRENT_CONFIG
dyn_data         HKEY_DYN_DATA
hkdd             HKEY_DYN_DATA
```

The key above can be written as `"\\hklm\\software\\ericsson\\erlang\\5.0"`.

This module uses a current key. It works much like the current directory. From
the current key, values can be fetched, subkeys can be listed, and so on.

Under a key, any number of named values can be stored. They have names, types,
and data.

`win32reg` supports storing of the following types:

- `REG_DWORD`, which is an integer
- `REG_SZ`, which is a string
- `REG_BINARY`, which is a binary

Other types can be read, and are returned as binaries.

There is also a "default" value, which has the empty string as name. It is read
and written with the atom `default` instead of the name.

Some registry values are stored as strings with references to environment
variables, for example, `%SystemRoot%Windows`. `SystemRoot` is an environment
variable, and is to be replaced with its value. Function `expand/1` is provided
so that environment variables surrounded by `%` can be expanded to their values.

For more information on the Windows registry, see consult the Win32 Programmer's
Reference.

### See Also

`erl_posix_msg`, The Windows 95 Registry (book from O'Reilly), Win32
Programmer's Reference (from Microsoft)

# `name`
*not exported* 

```erlang
-type name() :: string() | default.
```

# `reg_handle`

```erlang
-opaque reg_handle()
```

As returned by `open/1`.

# `value`
*not exported* 

```erlang
-type value() :: string() | integer() | binary().
```

# `change_key`

```erlang
-spec change_key(RegHandle, Key) -> ReturnValue
                    when
                        RegHandle :: reg_handle(),
                        Key :: string(),
                        ReturnValue :: ok | {error, ErrorId :: atom()}.
```

Changes the current key to another key. Works like `cd`. The key can be
specified as a relative path or as an absolute path, starting with `\.`

# `change_key_create`

```erlang
-spec change_key_create(RegHandle, Key) -> ReturnValue
                           when
                               RegHandle :: reg_handle(),
                               Key :: string(),
                               ReturnValue :: ok | {error, ErrorId :: atom()}.
```

Creates a key, or just changes to it, if it is already there. Works like a
combination of `mkdir` and `cd`. Calls the Win32 API function
`RegCreateKeyEx()`.

The registry must have been opened in write mode.

# `close`

```erlang
-spec close(RegHandle) -> ok when RegHandle :: reg_handle().
```

Closes the registry. After that, the `RegHandle` cannot be used.

# `current_key`

```erlang
-spec current_key(RegHandle) -> ReturnValue
                     when RegHandle :: reg_handle(), ReturnValue :: {ok, string()}.
```

Returns the path to the current key. This is the equivalent of `pwd`.

Notice that the current key is stored in the driver, and can be invalid (for
example, if the key has been removed).

# `delete_key`

```erlang
-spec delete_key(RegHandle) -> ReturnValue
                    when RegHandle :: reg_handle(), ReturnValue :: ok | {error, ErrorId :: atom()}.
```

Deletes the current key, if it is valid. Calls the Win32 API function
`RegDeleteKey()`. Notice that this call does not change the current key (unlike
`change_key_create/2`). This means that after the call, the current key is
invalid.

# `delete_value`

```erlang
-spec delete_value(RegHandle, Name) -> ReturnValue
                      when
                          RegHandle :: reg_handle(),
                          Name :: name(),
                          ReturnValue :: ok | {error, ErrorId :: atom()}.
```

Deletes a named value on the current key. The atom `default` is used for the
default value.

The registry must have been opened in write mode.

# `expand`

```erlang
-spec expand(String) -> ExpandedString when String :: string(), ExpandedString :: string().
```

Expands a string containing environment variables between percent characters.
Anything between two `%` is taken for an environment variable, and is replaced
by the value. Two consecutive `%` are replaced by one `%`.

A variable name that is not in the environment results in an error.

# `format_error`

```erlang
-spec format_error(ErrorId) -> ErrorString when ErrorId :: atom(), ErrorString :: string().
```

Converts a POSIX error code to a string (by calling `file:format_error/1`).

# `open`

```erlang
-spec open(OpenModeList) -> ReturnValue
              when
                  OpenModeList :: [OpenMode],
                  OpenMode :: read | write,
                  ReturnValue :: {ok, RegHandle} | {error, ErrorId :: enotsup},
                  RegHandle :: reg_handle().
```

Opens the registry for reading or writing. The current key is the root
(`HKEY_CLASSES_ROOT`). Flag `read` in the mode list can be omitted.

Use `change_key/2` with an absolute path after [`open`](`open/1`).

# `set_value`

```erlang
-spec set_value(RegHandle, Name, Value) -> ReturnValue
                   when
                       RegHandle :: reg_handle(),
                       Name :: name(),
                       Value :: value(),
                       ReturnValue :: ok | {error, ErrorId :: atom()}.
```

Sets the named (or default) value to `value`. Calls the Win32 API function
`RegSetValueEx()`. The value can be of three types, and the corresponding
registry type is used. The supported types are the following:

- `REG_DWORD` for integers
- `REG_SZ` for strings
- `REG_BINARY` for binaries

Other types cannot be added or changed.

The registry must have been opened in write mode.

# `sub_keys`

```erlang
-spec sub_keys(RegHandle) -> ReturnValue
                  when
                      RegHandle :: reg_handle(),
                      ReturnValue :: {ok, [SubKey]} | {error, ErrorId :: atom()},
                      SubKey :: string().
```

Returns a list of subkeys to the current key. Calls the Win32 API function
`EnumRegKeysEx()`.

Avoid calling this on the root keys, as it can be slow.

# `value`

```erlang
-spec value(RegHandle, Name) -> ReturnValue
               when
                   RegHandle :: reg_handle(),
                   Name :: name(),
                   ReturnValue :: {ok, Value :: value()} | {error, ErrorId :: atom()}.
```

Retrieves the named value (or default) on the current key. Registry values of
type `REG_SZ` are returned as strings. Type `REG_DWORD` values are returned as
integers. All other types are returned as binaries.

# `values`

```erlang
-spec values(RegHandle) -> ReturnValue
                when
                    RegHandle :: reg_handle(),
                    ReturnValue :: {ok, [ValuePair]} | {error, ErrorId :: atom()},
                    ValuePair :: {Name :: name(), Value :: value()}.
```

Retrieves a list of all values on the current key. The values have types
corresponding to the registry types, see `value/2`. Calls the Win32 API function
`EnumRegValuesEx()`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
