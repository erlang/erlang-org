# `versions`
[🔗](https://github.com/erlang/otp/blob/master/lib/runtime_tools/src/versions.erl#L22)

Utility functions for versions using the
[OTP Versions Scheme](`e:system:versions.md#version-scheme`).

# `vsn_branch_string`
*since OTP @OTP-20352@* 

```erlang
-type vsn_branch_string() :: unicode:chardata().
```

A branch identifier identifying a specific branch in a version tree.

The branch identifier format differs from the `t:vsn_string/0` type in that:
*   trailing `0` components are allowed.
*   it is always ended by a trailing dot (`.`) after the last component.
*   no branch identifiers with less than three components exist except for
    `~"0."` which identifies the trunk of the tree.

See the [OTP Versions Scheme](`e:system:versions.md#version-scheme`) for
more information about branches.

# `vsn_list`
*since OTP @OTP-20352@* 

```erlang
-type vsn_list() :: [non_neg_integer()].
```

A list representation of the `t:vsn_string/0` type.

A list of components of actual non-negative integers as elements in the list
separated as elements instead of by the dot character. The order of the
components, amount of components, and content of the components is the same as
 for the `t:vsn_string/0` type.

# `vsn_string`
*since OTP @OTP-20352@* 

```erlang
-type vsn_string() :: unicode:chardata().
```

A version string formatted according to the
[OTP Versions Scheme](`e:system:versions.md#version-scheme`).

The string should be formatted as `<V(1)>.<V(2)> ... <V(N)>` where:
*   each `<V(X)>` component is the string representation of a non-negative
    decimal integer. No leading `0` digits except for the number zero which
    should be exactly one `0` digit.
*   each `<V(X)>` component is separated by a dot (`.`) character. No leading
    or trailing dot characters are allowed.
*   at least the `<V(1)>` and `<V(2)>` components exist.
*   trailing `0` are only allowed in the `<V(1)>` and `<V(2)>` components.

See the [OTP Versions Scheme](`e:system:versions.md#version-scheme`) for
more information about versions.

# `branch`
*since OTP @OTP-20352@* 

```erlang
-spec branch(V :: vsn_string()) -> vsn_branch_string().
```

Calculates the branch identifier of the branch that the version `V` exists on.

Returns the branch identifier, of the type described in the
 [OTP Versions Scheme](`e:system:versions.md#version-scheme`),
that the version `V` exists on. If the version `V` exists on the trunk of the
tree, `~"0."` is returned.

Note that the returned branch identifier does not correspond to any of the
branch names used in the OTP git repository.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_string/0` type.

## Examples
```erlang
1> versions:branch(~"18.0").
<<"0.">>
2> versions:branch(~"18.2.4").
<<"0.">>
3> versions:branch(~"18.2.4.1").
<<"18.2.4.">>
4> versions:branch(~"18.2.4.0.1").
<<"18.2.4.0.">>
```

# `branch_base`
*since OTP @OTP-20352@* 

```erlang
-spec branch_base(B :: vsn_branch_string()) -> vsn_string().
```

Calculates the base version of the branch `B`.

Returns the version which is the base version of the branch identified by the
argument. If the passed argument identifies the trunk of the version tree
(`"~0."`), the base version returned is `~"0.0"`.

A `badarg` `error` exception will be thrown if the branch identifier is not a
valid branch identifier adhering to the description of the
`t:vsn_branch_string/0` type.

## Examples
```erlang
1> versions:branch_base(~"0.").
<<"0.0">>
2> versions:branch_base(~"18.2.4.").
<<"18.2.4">>
3> versions:branch_base(~"18.2.4.0.").
<<"18.2.4">>
```

# `check`
*since OTP @OTP-20352@* 

```erlang
-spec check(V :: vsn_string()) -> true | false.
```

Checks whether or not the version `V` is a valid version.

Returns true if the version is a valid version adhering to the description
of the `t:vsn_string/0` type; otherwise `false`.

Examples:
```erlang
1> versions:check(~"30.0").
true
2> versions:check(~"30.0.1").
true
3> versions:check(~"30.0.1.2").
true
4> versions:check(~"30.0.1.").
false
5> versions:check(~"30.0-rc1").
false
```

# `compare`
*since OTP @OTP-20352@* 

```erlang
-spec compare(V1 :: vsn_string(), V2 :: vsn_string()) -> same | ancestor | descendant | undefined.
```

Compares the versions `V1` and `V2`. The return value tells you how `V1`
compares to `V2`.

The return value:
*   `same` tells you that `V1` is the *same* as `V2`.
*   `ancestor` tells you that `V1` is an *ancestor* of `V2`.
*   `descendant` tells you that `V1` is a *descendant* of `V2`.
*   `undefined` tells you that the order between `V1` and `V2` is *undefined*.

A `badarg` `error` exception will be thrown if a version is not a valid
version adhering to the description of the `t:vsn_string/0` type.

See the description of the
[order between versions](`e:system:versions.md#order-of-versions`) in the OTP
version scheme for more information.

## Examples
```erlang
1> versions:compare(~"23.3", ~"23.3").
same
2> versions:compare(~"23.3", ~"23.2.7").
descendant
3> versions:compare(~"23.2.7", ~"23.3").
ancestor
4> versions:compare(~"23.2.7.1", ~"23.3").
undefined
```

# `list_check`
*since OTP @OTP-20352@* 

```erlang
-spec list_check(VL :: vsn_list()) -> true | false.
```

Checks whether or not the version `VL` is a valid version list representation.

Returns true if the version is a valid version adhering to the description
of the `t:vsn_list/0` type; otherwise `false`.

# `list_compare`
*since OTP @OTP-20352@* 

```erlang
-spec list_compare(VL1 :: vsn_list(), VL2 :: vsn_list()) -> same | ancestor | descendant | undefined.
```

Compare two versions on the version list format.

Works exactly the same way as `compare/2` with the only difference that the
versions passed as input are represented using the `t:vsn_list/0` type instead
of using the `t:vsn_string/0` type.

A `badarg` `error` exception will be thrown if a version is not a valid
version adhering to the description of the `t:vsn_list/0` type.

## Examples
```erlang
1> versions:list_compare([23, 3], [23, 3]).
same
2> versions:list_compare([23, 3], [23, 2, 7]).
descendant
3> versions:list_compare([23, 2, 7], [23, 3]).
ancestor
4> versions:list_compare([23, 2, 7, 1], [23, 3]).
undefined
```

# `list_to_string`
*since OTP @OTP-20352@* 

```erlang
-spec list_to_string(VL :: vsn_list()) -> vsn_string().
```

Convert a version from the `t:vsn_list/0` type to the `t:vsn_string/0` type.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_list/0` type.

# `string_to_list`
*since OTP @OTP-20352@* 

```erlang
-spec string_to_list(V :: vsn_string()) -> vsn_list().
```

Convert a version from the `t:vsn_string/0` type to the `t:vsn_list/0` type.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_string/0` type.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
