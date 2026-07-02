# `ssl_crl_cache`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/ssl/src/ssl_crl_cache.erl#L26)

CRL cache

Implements an internal CRL (Certificate Revocation List) cache. In addition to
implementing the `m:ssl_crl_cache_api` behaviour the following functions are
available.

# `crl_src`
*since OTP 18.0* 

```elixir
-type crl_src() :: {file, file:filename()} | {der, public_key:der_encoded()}.
```

A source to input CRLs

# `delete`
*since OTP 18.0* 

```elixir
-spec delete(Entries) -> ok | {error, Reason}
                when Entries :: crl_src() | uri_string:uri_string(), Reason :: ssl:reason().
```

Delete CRLs from the ssl applications local cache.

# `insert`
*since OTP 18.0* 

```elixir
-spec insert(CRLSrc) -> ok | {error, Reason} when CRLSrc :: crl_src(), Reason :: ssl:reason().
```

# `insert`
*since OTP 18.0* 

```elixir
-spec insert(DistPointURI, CRLSrc) -> ok | {error, Reason}
                when
                    DistPointURI :: uri_string:uri_string(), CRLSrc :: crl_src(), Reason :: ssl:reason().
```

Insert CRLs into the ssl applications local cache, with or without a
distribution point reference URI

---

*Consult [api-reference.md](api-reference.md) for complete listing*
