# `eldap`
[🔗](https://github.com/erlang/otp/blob/master/lib/eldap/src/eldap.erl#L29)

LDAP Client

This module provides a client api to the Lightweight Directory Access Protocol
(LDAP).

References:

- RFC 4510 - RFC 4519
- RFC 2830

The above publications can be found at [IETF](http://www.ietf.org).

Terminology abbreviations:

- Dn. An LDAPDN is defined to be the representation of a Distinguished Name
   (DN) after encoding according to the specification in [RFC4514].

# `attribute`
*not exported* *since OTP R15B01* 

```erlang
-type attribute() :: {Type :: string(), Value :: [string()]}.
```

The attributes of an entry

# `connection_info`
*not exported* *since OTP R15B01* 

```erlang
-type connection_info() :: #{socket := ssl:sslsocket() | gen_tcp:socket(), socket_type := tcp | ssl}.
```

The LDAP socket and the transport protocol, TCP or TLS (SSL), used by the ldap connection

# `dereference`
*since OTP R15B01* 

```erlang
-opaque dereference()
```

How to handle aliases during a search.
See `neverDerefAliases/0`, `derefInSearching/0`, `derefFindingBaseObj/0`, `derefAlways/0`

# `filter`
*not exported* *since OTP R15B01* 

```erlang
-type filter() :: term().
```

An opaque type representing a filter operation.

See the filter creation functions for more info, including,
`present/1`, `substrings/2`, `equalityMatch/2`,
`greaterOrEqual/2`, `lessOrEqual/2`,
`approxMatch/2`, `extensibleMatch/2`,
`'and'/1`, `'or'/1`, `'not'/1`

# `handle`
*not exported* *since OTP R15B01* 

```erlang
-type handle() :: term().
```

An opaque handle unique for the connection

# `modify_op`
*not exported* *since OTP R15B01* 

```erlang
-type modify_op() :: term().
```

Entry modification operations.
See `mod_add/2`, `mod_delete/2`, `mod_replace/2`

# `referrals`
*not exported* *since OTP R15B01* 

```erlang
-type referrals() :: [Address :: string()].
```

The LDAP Server Address.
The contents of `Address` is server dependent.

# `scope`
*since OTP R15B01* 

```erlang
-opaque scope()
```

Scope of a search.
See `baseObject/0`, `singleLevel/0`, `wholeSubtree/0`

# `search_option`
*not exported* *since OTP R15B01* 

```erlang
-type search_option() ::
          {base, string()} |
          {filter, filter()} |
          {scope, scope()} |
          {attributes, [string()]} |
          {deref, dereference()} |
          {types_only, boolean()} |
          {timeout, integer()}.
```

A requested set of serach options for seaching the directory

# `add`
*since OTP R15B01* 

```erlang
-spec add(Handle, Dn, Attributes) -> ok | {ok, Refs} | {error, Reason}
             when
                 Handle :: handle(),
                 Dn :: string(),
                 Attributes :: [attribute()],
                 Refs :: {referral, referrals()},
                 Reason :: term().
```

Add an entry. The entry must not exist.

```erlang
  add(Handle,
      "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com",
       [{"objectclass", ["person"]},
        {"cn", ["Bill Valentine"]},
        {"sn", ["Valentine"]},
        {"telephoneNumber", ["545 555 00"]}]
     )
```

# `and`
*since OTP R15B01* 

```erlang
-spec 'and'(ListOfFilters) -> filter() when ListOfFilters :: [filter()].
```

Creates a filter where all `Filter` must be true.

# `approxMatch`
*since OTP R15B01* 

```erlang
-spec approxMatch(Type, Value) -> filter() when Type :: string(), Value :: string().
```

Create a approximation match filter.

# `baseObject`
*since OTP R15B01* 

```erlang
-spec baseObject() -> scope().
```

Search baseobject only.

# `close`
*since OTP R15B01* 

```erlang
-spec close(Handle) -> ok when Handle :: handle().
```

Shutdown the connection after sending an unbindRequest to the server.

If the connection is TLS the connection will be closed with `ssl:close/1`, otherwise
with `gen_tcp:close/1`.

# `delete`
*since OTP R15B01* 

```erlang
-spec delete(Handle, Dn) -> ok | {ok, Refs} | {error, Reason}
                when
                    Handle :: handle(),
                    Dn :: string(),
                    Refs :: {referral, referrals()},
                    Reason :: term().
```

Delete an entry.

```text
  delete(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com")
```

# `derefAlways`
*since OTP R15B01* 

```erlang
-spec derefAlways() -> dereference().
```

Always dereference aliases.

# `derefFindingBaseObj`
*since OTP R15B01* 

```erlang
-spec derefFindingBaseObj() -> dereference().
```

Dereference aliases only in finding the base.

# `derefInSearching`
*since OTP R15B01* 

```erlang
-spec derefInSearching() -> dereference().
```

Dereference aliases only when searching.

# `equalityMatch`
*since OTP R15B01* 

```erlang
-spec equalityMatch(Type, Value) -> filter() when Type :: string(), Value :: string().
```

Create a equality filter.

# `extensibleMatch`
*since OTP 17.4* 

```erlang
-spec extensibleMatch(MatchValue, OptionalAttrs) -> filter()
                         when
                             MatchValue :: string(),
                             OptionalAttrs ::
                                 [{matchingRule, string()} |
                                  {type, string()} |
                                  {dnAttributes, boolean()}].
```

Creates an extensible match filter. For example,

```erlang
  eldap:extensibleMatch("Bar", [{type,"sn"}, {matchingRule,"caseExactMatch"}]))
```

creates a filter which performs a `caseExactMatch` on the attribute `sn` and
matches with the value `"Bar"`. The default value of `dnAttributes` is `false`.

# `greaterOrEqual`
*since OTP R15B01* 

```erlang
-spec greaterOrEqual(Type, Value) -> filter() when Type :: string(), Value :: string().
```

Create a greater or equal filter.

# `info`
*since OTP 25.3.1* 

```erlang
-spec info(Handle) -> connection_info() when Handle :: handle().
```

Currently available information reveals the socket and the transport protocol,
TCP or TLS (SSL), used by the LDAP connection.

# `lessOrEqual`
*since OTP R15B01* 

```erlang
-spec lessOrEqual(Type, Value) -> filter() when Type :: string(), Value :: string().
```

Create a less or equal filter.

# `mod_add`
*since OTP R15B01* 

```erlang
-spec mod_add(Type, Values) -> modify_op() when Type :: string(), Values :: [string()].
```

Create an add modification operation.

# `mod_delete`
*since OTP R15B01* 

```erlang
-spec mod_delete(Type, Values) -> modify_op() when Type :: string(), Values :: [string()].
```

Create a delete modification operation.

# `mod_replace`
*since OTP R15B01* 

```erlang
-spec mod_replace(Type, Values) -> modify_op() when Type :: string(), Values :: [string()].
```

Create a replace modification operation.

# `modify`
*since OTP R15B01* 

```erlang
-spec modify(Handle, Dn, ModifyOps) -> ok | {ok, Refs} | {error, Reason}
                when
                    Handle :: handle(),
                    Dn :: string(),
                    ModifyOps :: [term()],
                    Refs :: {referral, referrals()},
                    Reason :: term().
```

Modify an entry.

```erlang
  modify(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com",
         [eldap:mod_replace("telephoneNumber", ["555 555 00"]),
	  eldap:mod_add("description", ["LDAP Hacker"]) ])
```

# `modify_dn`
*since OTP R15B01* 

```erlang
-spec modify_dn(Handle, Dn, NewRDN, DeleteOldRDN, NewSupDN) -> ok | {ok, Refs} | {error, Reason}
                   when
                       Handle :: handle(),
                       Dn :: string(),
                       NewRDN :: string(),
                       DeleteOldRDN :: boolean(),
                       NewSupDN :: string(),
                       Refs :: {referral, referrals()},
                       Reason :: term().
```

Modify the DN of an entry.

`DeleteOldRDN` indicates whether the current RDN
should be removed from the attribute list after the operation. `NewSupDN` is the
new parent that the RDN shall be moved to. If the old parent should remain as
parent, `NewSupDN` shall be "".

```text
  modify_dn(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com ",
            "cn=Bill Jr Valentine", true, "")
```

# `modify_password`
*since OTP 18.0* 

```erlang
-spec modify_password(Handle, Dn, NewPasswd) -> ok | {ok, Refs} | {error, term()} | {ok, GenPasswd}
                         when
                             Handle :: handle(),
                             Dn :: string(),
                             NewPasswd :: string(),
                             Refs :: {referral, referrals()},
                             GenPasswd :: string().
```

Modify the password of a user. See `modify_password/4`.

# `modify_password`
*since OTP 18.0* 

```erlang
-spec modify_password(Handle, Dn, NewPasswd, OldPasswd) ->
                         ok | {ok, Refs} | {error, term()} | {ok, GenPasswd}
                         when
                             Handle :: handle(),
                             Dn :: string(),
                             NewPasswd :: string(),
                             OldPasswd :: string(),
                             Refs :: {referral, referrals()},
                             GenPasswd :: string().
```

Modify the password of a user.

- `Dn`. The user to modify. Should be "" if the modify request is for the user
  of the LDAP session.
- `NewPasswd`. The new password to set. Should be "" if the server is to
  generate the password. In this case, the result will be `{ok, GenPasswd}`.
- `OldPasswd`. Sometimes required by server policy for a user to change their
  password. If not required, use `modify_password/3`.

# `neverDerefAliases`
*since OTP R15B01* 

```erlang
-spec neverDerefAliases() -> dereference().
```

Never dereference aliases, treat aliases as entries.

# `not`
*since OTP R15B01* 

```erlang
-spec 'not'(Filter) -> filter() when Filter :: filter().
```

Negate a filter.

# `open`
*since OTP R15B01* 

```erlang
-spec open(Hosts) -> {ok, Handle} | {error, Reason}
              when
                  Hosts :: [Host],
                  Host :: inet:socket_address() | inet:hostname(),
                  Handle :: handle(),
                  Reason :: term().
```

Setup a connection to an LDAP server, the `HOST`'s are tried in order.

# `open`
*since OTP R15B01* 

```erlang
-spec open(Hosts, Options) -> {ok, Handle} | {error, Reason}
              when
                  Hosts :: [Host],
                  Host :: inet:socket_address() | inet:hostname(),
                  Options ::
                      [{port, integer()} |
                       {log, function()} |
                       {timeout, timeout()} |
                       {ssl, boolean()} |
                       {sslopts, [ssl:tls_client_option()]} |
                       {tcpopts, [inet:inet_backend() | gen_tcp:connect_option()]}],
                  Handle :: handle(),
                  Reason :: term().
```

Setup a connection to an LDAP server, the `HOST`'s are tried in order.

The log function takes three arguments,
`fun(Level, FormatString, [FormatArg]) end`.

Timeout set the maximum time in milliseconds that each server request may take.

All TCP socket options are accepted except `active`, `binary`, `deliver`,
`list`, `mode` and `packet`

# `or`
*since OTP R15B01* 

```erlang
-spec 'or'(ListOfFilters) -> filter() when ListOfFilters :: [filter()].
```

Create a filter where at least one of the `Filter` must be true.

# `paged_result_control`
*since OTP 24.3* 

```erlang
-spec paged_result_control(PageSize) -> {control, ControlOID, true, binary()}
                              when PageSize :: integer(), ControlOID :: string().
```

Paged results is an extension to the LDAP protocol specified by RFC2696

This function creates a control with the specified page size for use in
`search/3`, for example:

```erlang
Control = eldap:paged_result_control(50),
{ok, SearchResults} = search(Handle, [{base, "dc=example, dc=com"}], [Control]),
```

# `paged_result_control`
*since OTP 24.3* 

```erlang
-spec paged_result_control(PageSize, Cookie) -> {control, ControlOID, true, binary()}
                              when PageSize :: integer(), Cookie :: string(), ControlOID :: string().
```

Paged results is an extension to the LDAP protocol specified by RFC2696

This function creates a control with the specified page size and cookie for use
in `search/3` to retrieve the next results page.

For example:

```erlang
PageSize = 50,
Control1 = eldap:paged_result_control(PageSize),
{ok, SearchResults1} = search(Handle, [{base, "dc=example, dc=com"}], [Control1]),
%% retrieve the returned cookie from the search results
{ok, Cookie1} = eldap:paged_result_cookie(SearchResults1),
Control2 = eldap:paged_result_control(PageSize, Cookie1),
{ok, SearchResults2} = eldap:search(Handle, [{base, "dc=example,dc=com"}], [Control2]),
%% etc
```

# `paged_result_cookie`
*since OTP 24.3* 

```erlang
-spec paged_result_cookie(#eldap_search_result{controls :: maybe_improper_list(),
                                               entries :: term(),
                                               referrals :: term()}) ->
                             {error, no_cookie} |
                             {ok,
                              asn1_NOVALUE |
                              [[any()] | byte() | {_, _} | {_, _, _}] |
                              #'AttributeValueAssertion'{attributeDesc :: [any()],
                                                         assertionValue :: [any()]}}.
```

Paged results is an extension to the LDAP protocol specified by RFC2696.

This function extracts the cookie returned from the server as a result of a
paged search result.

If the returned cookie is the empty string `""`, then these search results
represent the last in the series.

# `present`
*since OTP R15B01* 

```erlang
-spec present(Type) -> filter() when Type :: string().
```

Create a filter which filters on attribute type presence.

# `search`
*since OTP R15B01* 

```erlang
-spec search(Handle, SearchOptions) ->
                {ok, #eldap_search_result{entries :: term(), referrals :: term(), controls :: term()}} |
                {ok, Refs} |
                {error, Reason}
                when
                    Handle :: handle(),
                    SearchOptions ::
                        #eldap_search{base :: term(),
                                      filter :: term(),
                                      size_limit :: term(),
                                      scope :: term(),
                                      deref :: term(),
                                      attributes :: term(),
                                      types_only :: term(),
                                      timeout :: term()} |
                        [search_option()],
                    Refs :: {referral, referrals()},
                    Reason :: term().
```

Search the directory with the supplied the SearchOptions.

The base and filter
options must be supplied. Default values: scope is `wholeSubtree/0`, deref is
`derefAlways/0`, types_only is `false` and timeout is `0` (meaning infinity).

```erlang
  Filter = eldap:substrings("cn", [{any,"V"}]),
  search(Handle, [{base, "dc=example, dc=com"}, {filter, Filter}, {attributes, ["cn"]}]),
```

The `timeout` option in the `SearchOptions` is for the ldap server, while the
timeout in [eldap:open/2](`open/2`) is used for each individual request in the
search operation.

# `simple_bind`
*since OTP R15B01* 

```erlang
-spec simple_bind(Handle, Dn, Password) -> ok | {ok, Refs} | {error, Reason}
                     when
                         Handle :: handle(),
                         Dn :: string(),
                         Password :: string(),
                         Refs :: {referral, referrals()},
                         Reason :: term().
```

Authenticate the connection using simple authentication.

# `singleLevel`
*since OTP R15B01* 

```erlang
-spec singleLevel() -> scope().
```

Search the specified level only, i.e. do not recurse.

# `start_tls`
*since OTP R16B03* 

```erlang
-spec start_tls(Handle, TlsOptions) -> ok | {ok, Refs} | {error, Reason}
                   when
                       Handle :: handle(),
                       TlsOptions :: [ssl:tls_client_option()],
                       Refs :: {referral, referrals()},
                       Reason :: term().
```

Same as start_tls(Handle, Options, infinity)

# `start_tls`
*since OTP R16B03* 

```erlang
-spec start_tls(Handle, TlsOptions, Timeout) -> ok | {ok, Refs} | {error, Reason}
                   when
                       Handle :: handle(),
                       TlsOptions :: [ssl:tls_client_option()],
                       Timeout :: infinity | pos_integer(),
                       Refs :: {referral, referrals()},
                       Reason :: term().
```

Upgrade the connection associated with `Handle` to a TLS connection if possible.

The upgrade is done in two phases: first the server is asked for permission to
upgrade. Second, if the request is acknowledged, the upgrade to TLS is
performed.

Error responses from phase one will not affect the current encryption state of
the connection. Those responses are:

- **`tls_already_started`** - The connection is already encrypted. The
  connection is not affected.

- **`{response,ResponseFromServer}`** - The upgrade was refused by the LDAP
  server. The `ResponseFromServer` is an atom delivered byt the LDAP server
  explained in section 2.3 of rfc 2830. The connection is not affected, so it is
  still un-encrypted.

Errors in the second phase will however end the connection:

- **`Error`** - Any error responded from ssl:connect/3

The `Timeout` parameter is for the actual TLS upgrade (phase 2) while the
timeout in [eldap:open/2](`open/2`) is used for the initial negotiation about
upgrade (phase 1).

# `substrings`
*since OTP R15B01* 

```erlang
-spec substrings(Type, SubStrings) -> filter()
                    when
                        Type :: string(),
                        SubStrings :: [{initial, string()} | {any, string()} | {final, string()}].
```

Create a filter which filters on substrings.

# `wholeSubtree`
*since OTP R15B01* 

```erlang
-spec wholeSubtree() -> scope().
```

Search the entire subtree.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
