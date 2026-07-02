# `inet_res`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/kernel/src/inet_res.erl#L24)

A rudimentary DNS client.

This module performs DNS name resolving towards recursive name servers.

See also [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`)
or more information about how to configure an Erlang runtime system for IP
communication, and how to enable this DNS client by defining `'dns'`
as a lookup method. The DNS client then acts as a backend for
the resolving functions in `m:inet`.

This DNS client can resolve DNS records even if it is not used
for normal name resolving in the node.

> #### Warning {: .warning }
>
> This is not a full-fledged resolver, only a DNS client
> that relies on being in trusted network shielded from
> DNS reply spoofing by firewalls, and on asking only trusted
> recursive name servers.
>
> If the Crypto application is loaded, this implementation
> does its best to not be fooled by spoofed DNS replies.
> This is nevertheless a meager remedy compared to using
> encrypted and signed DNS methods like TSIG or DNSSEC,
> as full fledged resolvers do.

> #### Note {: .info }
>
> If the Crypto application is loaded, this resolver client uses
> cryptographically random transaction IDs and port numbers,
> which should render the probability for a sucessfully brute force
> spoofed reply to be too low to be a usable exploit.
> An Erlang node that is installed in an exposed network environment
> should already have the Crypto application loaded for example
> to run the SSL application.  If this is not the case,
> the Crypto application can be explicitly loaded with
> `application:load(crypto)`.

## Name Resolving

UDP queries are used unless resolver option `usevc` is `true`,
which forces TCP queries.  If the query is too large for UDP,
TCP is used instead. For regular DNS queries, 512 bytes is the size limit.

When EDNS is enabled (resolver option `edns` is set to the EDNS version
(that is; `0` instead of `false`), resolver option `udp_payload_size`
sets the payload size limit.  If a name server replies with the TC bit set
(truncation), indicating that the answer is incomplete, the query is retried
towards the same name server using TCP.  Resolver option `udp_payload_size`
also sets the advertised size for the maximum allowed reply size,
if EDNS is enabled, otherwise the name server uses the limit 512 bytes.
If the reply is larger, it gets truncated, forcing a TCP requery.

For UDP queries, resolver options `timeout` and `retry` control
retransmission.  Each name server in the `nameservers` list is tried
with a time-out of `timeout`/`retry`. Then all name servers are tried again,
doubling the time-out, for a total of `retry` times.

[](){: #servfail_retry_timeout }

But before all name servers are tried again, there is a (user configurable)
time-out, `servfail_retry_timeout`. The point of this is to prevent
the new query to be handled by a server's servfail cache (a client
that is too eager will actually only get what is in the servfail cache).
If there is too little time left of the resolver call's time-out
to do a retry, the resolver call may return before the call's time-out
has expired.

For queries not using the `search` list, if the query to all `nameservers`
results in `{error,nxdomain}` or an empty answer, the same query is tried for
`alt_nameservers`.

If randomization of transaction ID:s and port numbers is too costly
for an Erlang node in a trusted network shielded from DNS reply spoofing,
the legacy behaviour to reuse the UDP socket on retries and use sequential
transaction ID:s can be configured by setting the resolver option
`random` to `false`.

## Resolver Types

The following data types concern the resolver:

## DNS Types

The following data types concern the DNS client:

## Example

This access functions example shows how `lookup/3` can be implemented using
`resolve/3` from outside the module:

```erlang
example_lookup(Name, Class, Type) ->
    case inet_res:resolve(Name, Class, Type) of
        {ok,Msg} ->
            [inet_dns:rr(RR, data)
             || RR <- inet_dns:msg(Msg, anlist),
                 inet_dns:rr(RR, type) =:= Type,
                 inet_dns:rr(RR, class) =:= Class];
        {error,_} ->
            []
     end.
```

# `dns_class`
*not exported* 

```erlang
-type dns_class() :: in | chaos | hs | any.
```

# `dns_data`
*not exported* 

```erlang
-type dns_data() ::
          dns_name() |
          inet:ip4_address() |
          inet:ip6_address() |
          {MName :: dns_name(),
           RName :: dns_name(),
           Serial :: integer(),
           Refresh :: integer(),
           Retry :: integer(),
           Expiry :: integer(),
           Minimum :: integer()} |
          {inet:ip4_address(), Proto :: integer(), BitMap :: binary()} |
          {CpuString :: string(), OsString :: string()} |
          {RM :: dns_name(), EM :: dns_name()} |
          {Prio :: integer(), dns_name()} |
          {Prio :: integer(), Weight :: integer(), Port :: integer(), dns_name()} |
          {Order :: integer(),
           Preference :: integer(),
           Flags :: string(),
           Services :: string(),
           Regexp :: string(),
           dns_name()} |
          [string()] |
          binary().
```

DNS record data (content)

The basic type of each data element is specified in this type.

`Regexp` is a UTF-8 `t:string/0`.  The other `t:string/0`s
are actually Latin-1 strings.

# `dns_msg`
*not exported* 

```erlang
-type dns_msg() :: term().
```

A DNS message.

This is the start of a hierarchy of opaque data structures that can be
examined with access functions in `inet_dns`, which return lists of
`{Field,Value}` tuples. The arity 2 functions return the value
for a specified field.

```erlang
dns_msg() = DnsMsg
    inet_dns:msg(DnsMsg) ->
        [ {header, dns_header()}
        | {qdlist, dns_query()}
        | {anlist, dns_rr()}
        | {nslist, dns_rr()}
        | {arlist, dns_rr()} ]
    inet_dns:msg(DnsMsg, header) -> dns_header() % for example
    inet_dns:msg(DnsMsg, Field) -> Value

dns_header() = DnsHeader
    inet_dns:header(DnsHeader) ->
        [ {id, integer()}
        | {qr, boolean()}
        | {opcode, query | iquery | status | integer()}
        | {aa, boolean()}
        | {tc, boolean()}
        | {rd, boolean()}
        | {ra, boolean()}
        | {pr, boolean()}
        | {rcode, integer(0..16)} ]
    inet_dns:header(DnsHeader, Field) -> Value

query_type() = axfr | mailb | maila | any | dns_rr_type()

dns_query() = DnsQuery
    inet_dns:dns_query(DnsQuery) ->
        [ {domain, dns_name()}
        | {type, query_type()}
        | {class, dns_class()} ]
    inet_dns:dns_query(DnsQuery, Field) -> Value

dns_rr() = DnsRr
    inet_dns:rr(DnsRr) -> DnsRrFields | DnsRrOptFields
    DnsRrFields = [ {domain, dns_name()}
                  | {type, dns_rr_type()}
                  | {class, dns_class()}
                  | {ttl, integer()}
                  | {data, dns_data()} ]
    DnsRrOptFields = [ {domain, dns_name()}
                     | {type, opt}
                     | {udp_payload_size, integer()}
                     | {ext_rcode, integer()}
                     | {version, integer()}
                     | {z, integer()}
                     | {data, dns_data()} ]
    inet_dns:rr(DnsRr, Field) -> Value
```

There is an information function for the types above:

```erlang
inet_dns:record_type(dns_msg()) -> msg;
inet_dns:record_type(dns_header()) -> header;
inet_dns:record_type(dns_query()) -> dns_query;
inet_dns:record_type(dns_rr()) -> rr;
inet_dns:record_type(_) -> undefined.
```

So, `inet_dns:(inet_dns:record_type(X))(X)` converts any of these data
structures into a `{Field,Value}` list.

# `dns_name`
*not exported* 

```erlang
-type dns_name() :: string().
```

A string with no adjacent dots.

# `dns_rr_type`
*not exported* 

```erlang
-type dns_rr_type() ::
          a | aaaa | caa | cname | gid | hinfo | ns | mb | md | mg | mf | minfo | mx | naptr | null |
          ptr | soa | spf | srv | txt | uid | uinfo | unspec | uri | wks.
```

# `hostent`

```erlang
-type hostent() ::
          inet:hostent() |
          {hostent,
           H_name :: inet:hostname(),
           H_aliases :: [inet:hostname()],
           H_addrtype :: dns_rr_type(),
           H_length :: non_neg_integer(),
           H_addr_list :: [dns_data()]}.
```

Extended variant of `t:inet:hostent/0`.

Allows `t:dns_rr_type/0` for the
[`#hostent{}.h_addrtype`](`t:inet:hostent/0`) field, and
`[`[`dns_data/0`](`t:dns_data/0`)`]` for the
[`#hostent{}.h_addr_list`](`t:inet:hostent/0`) field.

# `nameserver`

```erlang
-type nameserver() :: {inet:ip_address(), Port :: 1..65535}.
```

# `res_error`

```erlang
-type res_error() :: formerr | qfmterror | servfail | nxdomain | notimp | refused | badvers | timeout.
```

# `res_option`

```erlang
-type res_option() ::
          {alt_nameservers, [nameserver()]} |
          {edns, 0 | false} |
          {inet6, boolean()} |
          {nameservers, [nameserver()]} |
          {recurse, boolean()} |
          {retry, integer()} |
          {timeout, integer()} |
          {udp_payload_size, integer()} |
          {dnssec_ok, boolean()} |
          {usevc, boolean()} |
          {random, boolean()} |
          {nxdomain_reply, boolean()}.
```

# `nnslookup`

```erlang
-spec nnslookup(Name, Class, Type, Nameservers) -> {ok, dns_msg()} | {error, Reason}
                   when
                       Name :: dns_name() | inet:ip_address(),
                       Class :: dns_class(),
                       Type :: dns_rr_type(),
                       Nameservers :: [nameserver()],
                       Reason :: inet:posix().
```

# `nnslookup`

```erlang
-spec nnslookup(Name, Class, Type, Nameservers, Timeout) -> {ok, dns_msg()} | {error, Reason}
                   when
                       Name :: dns_name() | inet:ip_address(),
                       Class :: dns_class(),
                       Type :: dns_rr_type(),
                       Timeout :: timeout(),
                       Nameservers :: [nameserver()],
                       Reason :: inet:posix().
```

Resolve a DNS query.

Like `nslookup/4` but calls `resolve/5` with both the arguments
`Opts = [{nameservers, Nameservers}]` and `Timeout`.

# `nslookup`

```erlang
-spec nslookup(Name, Class, Type) -> {ok, dns_msg()} | {error, Reason}
                  when
                      Name :: dns_name() | inet:ip_address(),
                      Class :: dns_class(),
                      Type :: dns_rr_type(),
                      Reason :: inet:posix() | res_error().
```

# `nslookup`

```erlang
-spec nslookup(Name, Class, Type, Timeout) -> {ok, dns_msg()} | {error, Reason}
                  when
                      Name :: dns_name() | inet:ip_address(),
                      Class :: dns_class(),
                      Type :: dns_rr_type(),
                      Timeout :: timeout(),
                      Reason :: inet:posix() | res_error();
              (Name, Class, Type, Nameservers) -> {ok, dns_msg()} | {error, Reason}
                  when
                      Name :: dns_name() | inet:ip_address(),
                      Class :: dns_class(),
                      Type :: dns_rr_type(),
                      Nameservers :: [nameserver()],
                      Reason :: inet:posix() | res_error().
```

Resolve a DNS query.

This function is a legacy wrapper to `resolve/5` that simplifies
errors matching `{error, {Reason, _}}` into `{error, Reason}`
or `{error, einval}`.

With argument `Timeout` calls `resolve/5` with `Opts = []`.

With argument `Nameservers` calls `resolve/5` with
`Opts = [{nameservers, Nameservers}]` and `Timeout = infinity`.

# `getbyname`

```erlang
-spec getbyname(Name, Type) -> {ok, Hostent} | {error, Reason}
                   when
                       Name :: dns_name(),
                       Type :: dns_rr_type(),
                       Hostent :: inet:hostent() | hostent(),
                       Reason :: inet:posix() | res_error().
```

# `getbyname`

```erlang
-spec getbyname(Name, Type, Timeout) -> {ok, Hostent} | {error, Reason}
                   when
                       Name :: dns_name(),
                       Type :: dns_rr_type(),
                       Timeout :: timeout(),
                       Hostent :: inet:hostent() | hostent(),
                       Reason :: inet:posix() | res_error().
```

# `getbyname`
*since OTP 28.1* 

```erlang
-spec getbyname(Name, Type, Opts, Timeout) -> {ok, Hostent} | {error, Reason}
                   when
                       Name :: dns_name(),
                       Type :: dns_rr_type(),
                       Opts :: [Opt],
                       Opt :: res_option() | verbose | atom(),
                       Timeout :: timeout(),
                       Hostent :: inet:hostent() | hostent(),
                       Reason :: inet:posix() | res_error().
```

Resolve a DNS query.

Resolves a DNS query of the specified `Type` for the specified host,
of class`in`.  Returns, on success, when resolving a `Type = a|aaaa`
DNS record, a `#hostent{}` record with `#hostent.h_addrtype = inet|inet6`,
respectively; see `t:inet:hostent/0`.

When resolving other `Type = dns_rr_type()`:s (of class `in`), also returns
a `#hostent{}` record but with `t:dns_rr_type/0` in `#hostent.h_addrtype`,
and the resolved `t:dns_data/0` in `#hostent.h_addr_list`; see `t:hostent/0`.

This function uses resolver option `search` that is a list of domain names.
If the name to resolve contains no dots, it is prepended to each domain
name in the search list, and they are tried in order.  If the name
contains dots, it is first tried as an absolute name and if that fails,
the search list is used. If the name has a trailing dot, it is supposed
to be an absolute name and the search list is not used.

See `resolve/5` about `Opts`.

# `gethostbyaddr`

```erlang
-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, Reason}
                       when
                           Address :: inet:ip_address(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

# `gethostbyaddr`

```erlang
-spec gethostbyaddr(Address, Timeout) -> {ok, Hostent} | {error, Reason}
                       when
                           Address :: inet:ip_address(),
                           Timeout :: timeout(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

Backend function used by `inet:gethostbyaddr/1`.

# `gethostbyaddr`
*since OTP 28.1* 

```erlang
-spec gethostbyaddr(Address, Opts, Timeout) -> {ok, Hostent} | {error, Reason}
                       when
                           Address :: inet:ip_address(),
                           Opts :: [Opt],
                           Opt :: res_option() | verbose | atom(),
                           Timeout :: timeout(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

Backend function used by `inet:gethostbyaddr/1`.

# `gethostbyname`

```erlang
-spec gethostbyname(Name) -> {ok, Hostent} | {error, Reason}
                       when
                           Name :: dns_name(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

Backend functions used by [`inet:gethostbyname/1,2`](`inet:gethostbyname/1`).

If resolver option `inet6` is `true`, equivalent to
[`gethostbyname(Name, inet6, infinity)`](`gethostbyname/3`),
otherwise [`gethostbyname(Name, inet, infinity)`](`gethostbyname/3`).

# `gethostbyname`

```erlang
-spec gethostbyname(Name, Family) -> {ok, Hostent} | {error, Reason}
                       when
                           Name :: dns_name(),
                           Family :: inet:address_family(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

# `gethostbyname`

```erlang
-spec gethostbyname(Name, Family, Timeout) -> {ok, Hostent} | {error, Reason}
                       when
                           Name :: dns_name(),
                           Family :: inet:address_family(),
                           Timeout :: timeout(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

# `gethostbyname`
*since OTP 28.1* 

```erlang
-spec gethostbyname(Name, Family, Opts, Timeout) -> {ok, Hostent} | {error, Reason}
                       when
                           Name :: dns_name(),
                           Family :: inet:address_family(),
                           Opts :: [Opt],
                           Opt :: res_option() | verbose | atom(),
                           Timeout :: timeout(),
                           Hostent :: inet:hostent(),
                           Reason :: inet:posix() | res_error().
```

Backend functions used by [`inet:gethostbyname/1,2`](`inet:gethostbyname/1`).

This function uses resolver option `search` just like
[`getbyname/2,3`](`getbyname/2`).

# `lookup`

```erlang
-spec lookup(Name, Class, Type) -> [dns_data()]
                when Name :: dns_name() | inet:ip_address(), Class :: dns_class(), Type :: dns_rr_type().
```

# `lookup`

```erlang
-spec lookup(Name, Class, Type, Opts) -> [dns_data()]
                when
                    Name :: dns_name() | inet:ip_address(),
                    Class :: dns_class(),
                    Type :: dns_rr_type(),
                    Opts :: [res_option() | verbose].
```

# `lookup`

```erlang
-spec lookup(Name, Class, Type, Opts, Timeout) -> [dns_data()]
                when
                    Name :: dns_name() | inet:ip_address(),
                    Class :: dns_class(),
                    Type :: dns_rr_type(),
                    Opts :: [res_option() | verbose],
                    Timeout :: timeout().
```

Look up DNS data.

Resolves the DNS data for the record `Name` of the specified
`Type` and `Class`. On success, filters out the answer records
with the correct `Class` and `Type`, and returns a list of their data fields.
So, a lookup for type `any` gives an empty answer, as the answer records
have specific types that are not `any`. An empty answer or a failed lookup
returns an empty list.

Calls [`resolve/*`](`resolve/3`) with the same arguments and filters the result,
so `Opts` is described for those functions.

# `resolve`

```erlang
-spec resolve(Name, Class, Type) -> {ok, dns_msg()} | Error
                 when
                     Name :: dns_name() | inet:ip_address(),
                     Class :: dns_class(),
                     Type :: dns_rr_type(),
                     Error :: {error, Reason} | {error, {Reason, dns_msg()}},
                     Reason :: inet:posix() | res_error().
```

# `resolve`

```erlang
-spec resolve(Name, Class, Type, Opts) -> {ok, dns_msg()} | Error
                 when
                     Name :: dns_name() | inet:ip_address(),
                     Class :: dns_class(),
                     Type :: dns_rr_type(),
                     Opts :: [Opt],
                     Opt :: res_option() | verbose | atom(),
                     Error :: {error, Reason} | {error, {Reason, dns_msg()}},
                     Reason :: inet:posix() | res_error().
```

# `resolve`

```erlang
-spec resolve(Name, Class, Type, Opts, Timeout) -> {ok, dns_msg()} | Error
                 when
                     Name :: dns_name() | inet:ip_address(),
                     Class :: dns_class(),
                     Type :: dns_rr_type(),
                     Opts :: [Opt],
                     Opt :: res_option() | verbose | atom(),
                     Timeout :: timeout(),
                     Error :: {error, Reason} | {error, {Reason, dns_msg()}},
                     Reason :: inet:posix() | res_error().
```

Resolve a DNS query.

Resolves a DNS query for the specified `Type`, `Class`, and  `Name`,
into a DNS message possibly containing Resource Records.
The returned `t:dns_msg/0` can be examined using access functions
in `inet_db`, as described in section in [DNS Types](#module-dns-types).

If `Name` is an `ip_address()`, the domain name to query about is generated
as the standard reverse `".IN-ADDR.ARPA."` name for an IPv4 address, or the
`".IP6.ARPA."` name for an IPv6 address.  In this case, you most probably
want to use `Class = in` and `Type = ptr`, but it is not done automatically.

`Opts` overrides the corresponding resolver options. If option `nameservers`
is specified, it is assumed that it is the complete list of name serves,
so resolver option `alt_nameserves` is ignored. However, if option
`alt_nameserves` is also specified to this function, it is used.

Option `verbose` (or rather `{verbose,true}`) causes diagnostics printout
through [`io:format/2`](`io:format/3`) of queries, replies, retransmissions,
and so on, similar to utilities such as `dig` and `nslookup`.

Option `nxdomain_reply` (or rather `{nxdomain_reply, true}`) causes NXDOMAIN
errors from DNS servers to be returned as `{error, {nxdomain, dns_msg()}}`.
`t:dns_msg/0` contains the additional sections that where included by the
answering server. This is mainly useful to inspect the SOA record
to get the TTL for negative caching.

If `Opt` is any atom, it is interpreted as `{Opt,true}` unless
the atom string starts with `"no"`, making the interpretation `{Opt,false}`.
For example, `usevc` is an alias for `{usevc, true}` and `nousevc`
is an alias for `{usevc, false}`.

Option `inet6` has no effect on this function. You probably want to use
`Type = a | aaaa` instead.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
