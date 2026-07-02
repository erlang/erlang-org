# `ssl_crl_cache_api`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/ssl/src/ssl_crl_cache_api.erl#L25)

API for a TLS CRL (Certificate Revocation List) cache.

When TLS performs certificate path validation according to
[RFC 5280 ](http://www.ietf.org/rfc/rfc5280.txt)it should also perform CRL
validation checks. To enable the CRL checks the application needs access to
CRLs. A database of CRLs can be set up in many different ways. This module
provides the behavior of the API needed to integrate an arbitrary CRL cache with
the erlang ssl application. It is also used by the application itself to provide
a simple default implementation of a CRL cache.

# `crl_cache_ref`
*since OTP 18.0* 

```elixir
-type crl_cache_ref() :: any().
```

Reference to the CRL cache.

# `dist_point`
*since OTP 18.0* 

```elixir
-type dist_point() ::
          #'DistributionPoint'{distributionPoint :: term(), reasons :: term(), cRLIssuer :: term()}.
```

For description see
[X509 certificates records](`e:public_key:public_key_records.md`)

# `logger_info`
*since OTP 18.0* 

```elixir
-type logger_info() ::
          {logger:level(), Report :: #{description => string(), reason => term()}, logger:metadata()}.
```

Information for ssl applications use of [Logger(3)](`m:logger`)

# `fresh_crl`
*since OTP 18.0* 

```elixir
-callback fresh_crl(DistPoint :: dist_point(), CRL :: public_key:der_encoded()) ->
                       public_key:der_encoded() | {logger, logger_info(), public_key:der_encoded()}.
```

`fun fresh_crl/2` will be used as input option `update_crl` to
`public_key:pkix_crls_validate/3`.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.

# `lookup`
*since OTP 18.0* *optional* 

```elixir
-callback lookup(DistPoint :: dist_point(), CacheRef :: crl_cache_ref()) ->
                    not_available |
                    [public_key:der_encoded()] |
                    {{logger, logger_info()}, [public_key:der_encoded()]}.
```

Backwards compatibility, replaced by lookup/3

# `lookup`
*since OTP 19.0* 

```elixir
-callback lookup(Distpoint :: dist_point(), Issuer :: public_key:issuer_name(), CacheRef :: crl_cache_ref()) ->
                    not_available |
                    [public_key:der_encoded()] |
                    {{logger, logger_info()}, [public_key:der_encoded()]}.
```

Lookup the CRLs belonging to the distribution point `Distributionpoint`. This
function may choose to only look in the cache or to follow distribution point
links depending on how the cache is administrated.

The `Issuer` argument contains the issuer name of the certificate to
be checked.  Normally the returned CRL should be issued by this
issuer, except if the `cRLIssuer` field of `DistributionPoint` has a
value, in which case that value should be used instead.

In an earlier version of this API, the `lookup` function received two
arguments, omitting `Issuer`. For compatibility, this is still
supported: if there is no [`lookup/3`](`c:lookup/3`) function in the
callback module,[`lookup/2`](`c:lookup/2`) is called instead.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.

# `select`
*since OTP 18.0* 

```elixir
-callback select(IssuerOrDPLocations, CacheRef) -> [CRL] | {logger, logger_info(), [CRL]}
                    when
                        CRL :: public_key:der_encoded(),
                        IssuerOrDPLocations :: public_key:issuer_name() | list(),
                        CacheRef :: crl_cache_ref().
```

Select the CRLs in the cache that are issued by `Issuer` unless the value is a
list of so called general names, see
[X509 certificates records](`e:public_key:public_key_records.md`), originating
form `#'DistributionPoint'.cRLissuer` and representing different mechanism to
obtain the CRLs. The cache callback needs to use the appropriate entry to
retrieve the CRLs or return an empty list if it does not exist.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
