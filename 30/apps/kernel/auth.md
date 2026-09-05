# `auth`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/auth.erl#L22)

> This module is deprecated. See each function for what to use instead.

Erlang network authentication server.

For a description of the Magic Cookie system, refer
to [Distributed Erlang](`e:system:distributed.md`) in the Erlang Reference
Manual.

# `cookie`
*not exported* 

```erlang
-type cookie() :: atom().
```

# `cookie`

> This function is deprecated. auth:cookie/0 is deprecated; use erlang:get_cookie/0 instead.

```erlang
-spec cookie() -> Cookie when Cookie :: cookie().
```

Use [`erlang:get_cookie()`](`erlang:get_cookie/0`) in ERTS instead.

# `cookie`

> This function is deprecated. auth:cookie/1 is deprecated; use erlang:set_cookie/2 instead.

```erlang
-spec cookie(TheCookie) -> true when TheCookie :: Cookie | [Cookie], Cookie :: cookie().
```

Use [`erlang:set_cookie(node(), Cookie)` in ERTS](`erlang:set_cookie/2`)
instead.

# `is_auth`

> This function is deprecated. auth:is_auth/1 is deprecated; use net_adm:ping/1 instead.

```erlang
-spec is_auth(Node) -> yes | no when Node :: node().
```

Returns `yes` if communication with `Node` is authorized.

Use [`net_adm:ping(Node)`](`net_adm:ping/1`) instead.

Notice that a connection to `Node` is established in this case. Returns `no` if
`Node` does not exist or communication is not authorized (it has another cookie
than `auth` thinks it has).

# `node_cookie`

> This function is deprecated. auth:node_cookie/1 is deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead.

```erlang
-spec node_cookie(Cookies :: [node() | cookie(), ...]) -> yes | no.
```

Equivalent to [`node_cookie(Node, Cookie)`](`node_cookie/2`).

# `node_cookie`

> This function is deprecated. auth:node_cookie/2 is deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead.

```erlang
-spec node_cookie(Node, Cookie) -> yes | no when Node :: node(), Cookie :: cookie().
```

Sets the magic cookie of `Node` to `Cookie` and verifies the status of the
authorization.

Equivalent to calling [`erlang:set_cookie(Node, Cookie)`](`erlang:set_cookie/2`),
followed by [`auth:is_auth(Node)`](`is_auth/1`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
