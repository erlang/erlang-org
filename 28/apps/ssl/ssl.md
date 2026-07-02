# `ssl`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/ssl/src/ssl.erl#L28)

Interface functions for TLS (Transport Layer Security)
and DTLS (Datagram Transport Layer Security).

> #### Note {: .info }
The application's name is still SSL because the first versions of the
TLS protocol were named SSL (Secure Socket Layer). However, no version
of the old SSL protocol is supported by this application.

Example:
```erlang
1> ssl:start(), ssl:connect("google.com", 443, [{verify, verify_peer},
    {cacerts, public_key:cacerts_get()}]).
{ok,{sslsocket, [...]}}
```

See [Examples](using_ssl.md) for detailed usage and more examples of
this API.

Special Erlang node configuration for the application can be found in
[SSL Application](ssl_app.md).

# `cipher`

```elixir
-type cipher() ::
          aes_256_gcm | aes_128_gcm | aes_256_ccm | aes_128_ccm | chacha20_poly1305 | aes_256_ccm_8 |
          aes_128_ccm_8 | aes_128_cbc | aes_256_cbc |
          legacy_cipher().
```

Cipher algorithms that can be used for payload encryption.

# `cipher_filters`

```elixir
-type cipher_filters() ::
          [{key_exchange | cipher | mac | prf,
            fun((kex_algo() | cipher() | hash() | aead | default_prf) -> true | false)}].
```

Filter that allows you to customize cipher suite list.

# `cipher_suites`
*not exported* 

```elixir
-type cipher_suites() :: ciphers().
```

A list of cipher suites that should be supported.

Function [ssl:cipher_suites/2 ](`cipher_suites/2`) can be used to find all
cipher suites that are supported by default and all cipher suites that can be
configured.

If you compose your own `t:cipher_suites/0` make sure they are
filtered for crypto library support using [ssl:filter_cipher_suites/2
](`filter_cipher_suites/2`).

The following function can help creating customized cipher suite lists:

- [ssl:append_cipher_suites/2 ](`append_cipher_suites/2`)
- [ssl:prepend_cipher_suites/2](`prepend_cipher_suites/2`)
- [ssl:suite_to_str/1](`suite_to_str/1`)
- [ssl:str_to_suite/1](`str_to_suite/1`)
- [ssl:suite_to_openssl_str/1](`suite_to_openssl_str/1`)

> #### Note {: .info }
Note that TLS-1.3 and TLS-1.2 use different sets of cipher suites. To
support both versions, cipher suites from both sets need to be
included. If the supplied list does not comply with the configured
versions or crypto library, that is, resulting in an empty list, the option
will fall back to its appropriate default value for the configured
versions.

Non-default cipher suites, including anonymous cipher suites (prior to
TLS 1.3), are supported for interoperability and testing
purposes. These can be used by adding them to your cipher suite
list. Note that they also need to be supported and enabled by the peer
to be actually used, and they may require additional configuration;
see [`srp_param_type()`](`t:srp_param_type/0`).

# `ciphers`

```elixir
-type ciphers() :: [erl_cipher_suite()] | string().
```

Cipher suite formats.

For backwards compatibility, cipher suites can be configured as a
colon-separated string of cipher suite RFC names (or even old OpenSSL
names). However, a more flexible approach is to use utility functions
together with [`cipher_filters()`](`t:cipher_filters/0`) if a customized
cipher suite option is needed.

# `erl_cipher_suite`

```elixir
-type erl_cipher_suite() ::
          #{key_exchange := kex_algo(),
            cipher := cipher(),
            mac := hash() | aead,
            prf := hash() | default_prf}.
```

Erlang cipher suite representation

> #### Warning {: .warning }
Enabling cipher suites using RSA as a key exchange algorithm is
strongly discouraged (only available prior to TLS-1.3). For some
configurations software preventions may exist, and can make them
usable if they work, but relying on them to work is risky. There
exists more reliable cipher suites that can be used instead.

# `group`

```elixir
-type group() ::
          x25519mlkem768 | secp384r1mlkem1024 | secp256r1mlkem768 | mlkem1024 | mlkem768 | mlkem512 |
          x25519 | x448 | secp256r1 | secp384r1 | secp521r1 | brainpoolP512r1tls13 |
          brainpoolP384r1tls13 | brainpoolP256r1tls13 | ffdhe2048 | ffdhe3072 | ffdhe4096 | ffdhe6144 |
          ffdhe8192.
```

TLS-1.3 key exchange configuration.

# `hash`

```elixir
-type hash() :: sha2() | legacy_hash().
```

Hash algorithms used together with signing and encryption functions.

# `kex_algo`

```elixir
-type kex_algo() ::
          ecdhe_ecdsa | ecdh_ecdsa | ecdh_rsa | rsa | dhe_rsa | dhe_dss | srp_rsa | srp_dss | dhe_psk |
          rsa_psk | psk | ecdh_anon | dh_anon | srp_anon | any.
```

Cipher Suite Key Exchange Algorithm will be `any`
in TLS-1.3 as key exchange is no longer part of cipher suite
configuration in TLS-1.3.

# `named_curve`

```elixir
-type named_curve() ::
          x25519 | x448 | secp521r1 | brainpoolP512r1 | brainpoolP384r1 | secp384r1 | brainpoolP256r1 |
          secp256r1 |
          legacy_named_curve().
```

Key exchange configuration prior to TLS-1.3.

# `post_quantum_schemes`
*not exported* 

```elixir
-type post_quantum_schemes() :: crypto:mldsa() | crypto:slh_dsa().
```

Supported in TLS-1.3 only. ML-DSA since 28.1, SLH-DSA since 28.3.

# `rsassa_pss_scheme`
*not exported* 

```elixir
-type rsassa_pss_scheme() ::
          rsa_pss_rsae_sha512 | rsa_pss_rsae_sha384 | rsa_pss_rsae_sha256 | rsa_pss_pss_sha512 |
          rsa_pss_pss_sha384 | rsa_pss_pss_sha256.
```

Supported in TLS-1.3 and TLS-1.2.

# `sha2`
*not exported* 

```elixir
-type sha2() :: sha512 | sha384 | sha256.
```

SHA2 hash algorithms.

# `sign_algo`

```elixir
-type sign_algo() :: eddsa | ecdsa | rsa | legacy_sign_algo().
```

Signature algorithms.

# `sign_scheme`

```elixir
-type sign_scheme() ::
          eddsa_ed25519 | eddsa_ed448 | ecdsa_secp521r1_sha512 | ecdsa_secp384r1_sha384 |
          ecdsa_secp256r1_sha256 | ecdsa_brainpoolP512r1tls13_sha512 |
          ecdsa_brainpoolP384r1tls13_sha384 | ecdsa_brainpoolP256r1tls13_sha256 |
          post_quantum_schemes() |
          rsassa_pss_scheme() |
          legacy_sign_scheme().
```

Signature schemes, defined by TLS-1.3, and replacing signature algorithms from TLS-1.2.

Explicitly list acceptable signature schemes in the preferred
order.

Overrides the algorithms supplied in
[`signature_algs`](`t:signature_algs/0`) option for certificates.
In addition to the `signature_algorithms` extension from TLS 1.2,
[TLS 1.3 (RFC 5246 Section 4.2.3)](http://www.ietf.org/rfc/rfc8446.txt#section-4.2.3)
adds the `signature_algorithms_cert` extension which enables having special
requirements on the signatures used in the certificates that differs from the
requirements on digital signatures as a whole. If this is not required this
extension is not needed.

The client will send a `signature_algorithms_cert` extension (in the
client hello message), if TLS version 1.2 (back-ported to TLS 1.2 in
24.1) or later is used, and the signature_algs_cert option is
explicitly specified.  By default, only the
[signature_algs](`t:signature_algs/0`) extension is sent with the
exception of when signature_algs option is not explicitly specified,
in which case it will append the rsa_pkcs1_sha1 algorithm to the
default value of signature_algs and use it as value for
signature_algs_cert to allow certificates to have this signature but
still disallow sha1 use in the TLS protocol, since 27.0.1 and 26.2.5.2.

> #### Note {: .info }
>
> Note that supported signature schemes for TLS-1.2 are
[`legacy_sign_scheme()`](`t:legacy_sign_scheme/0`)
> and [`rsassa_pss_scheme()`](`t:rsassa_pss_scheme/0`).

# `signature_algs`

```elixir
-type signature_algs() :: [{hash(), sign_algo()} | sign_scheme()].
```

Explicitly list acceptable signature algorithms for certificates and handshake
messages in the preferred order.

The client will send its list as the client hello
`signature_algorithm` extension introduced in TLS-1.2; see [Section
7.4.1.4.1 in RFC 5246](http://www.ietf.org/rfc/rfc5246.txt). Before
TLS-1.2, these algorithms where implicitly chosen and partly derived
from the cipher suite.

In TLS-1.2 a somewhat more explicit negotiation is made possible using a list of
`{HashAlgo, SignAlgo}` tuples.

In TLS-1.3, these algorithm pairs are replaced by [signature
schemes](`t:sign_scheme/0`) that are completely decoupled from the
cipher suite.

Signature algorithms used for certificates can be overridden by the
[signature schemes](`t:sign_scheme/0`) supplied by the
`signature_algs_cert` option.

The TLS-1.2 default is `Default_TLS_12_Alg_Pairs` interleaved with
`rsa_pss_schemes` since ssl-11.0 (Erlang/OTP 25). `pss_pss` is
preferred over `pss_rsae`, which in turn is preferred over `rsa`.

The list for `Default_TLS_12_Alg_Pairs` is defined as follows:

```erlang
[
{sha512, ecdsa},
{sha512, rsa},
{sha384, ecdsa},
{sha384, rsa},
{sha256, ecdsa},
{sha256, rsa}
]
```

> #### Change {: .info }
>
> - Support for `{md5, rsa}` was removed from the TLS-1.2 default in
>   ssl-8.0 (Erlang/OTP 22).
> - Support for `{sha, _}` (SHA1) and `{sha224, _}` was removed
>   from the TLS-1.2 default in ssl-11.0 (Erlang/OTP 26).

The list for `rsa_pss_schemes` is defined as follows:

```erlang
[rsa_pss_pss_sha512,
rsa_pss_pss_sha384,
rsa_pss_pss_sha256,
rsa_pss_rsae_sha512,
rsa_pss_rsae_sha384,
rsa_pss_rsae_sha256]
```

The list of `TLS_13_Legacy_Schemes` is defined as follows:

```erlang
[
%% Legacy algorithms only applicable to certificate signatures
rsa_pkcs1_sha512, %% Corresponds to {sha512, rsa}
rsa_pkcs1_sha384, %% Corresponds to {sha384, rsa}
rsa_pkcs1_sha256, %% Corresponds to {sha256, rsa}
]
```

The list of `Default_TLS_13_Schemes` is defined as follows:

```text
[
%% EDDSA
eddsa_ed25519,
eddsa_ed448

%% ECDSA
ecdsa_secp521r1_sha512,
ecdsa_secp384r1_sha384,
ecdsa_secp256r1_sha256] ++

%% RSASSA-PSS
rsa_pss_schemes()
```

> #### Change {: .info }
>
> EDDSA was made highest priority in ssl-10.8 (Erlang/OTP 25).

The TLS-1.3 default is `Default_TLS_13_Schemes`.

If both TLS-1.3 and TLS-1.2 are supported the default is:

```erlang
Default_TLS_13_Schemes ++ TLS_13_Legacy_Schemes ++
Default_TLS_12_Alg_Pairs %% not represented in TLS_13_Legacy_Schemes
```

to ensure that appropriate algorithms can be chosen for the negotiated
version.

> #### Note {: .info }
TLS-1.2 algorithms will not be negotiated for TLS-1.3, but the TLS-1.3
RSASSA-PSS ([`rsassa_pss_scheme()`](`t:rsassa_pss_scheme/0`))
signature schemes can be negotiated also for TLS-1.2 from Erlang/OTP
24.1 (fully working from Erlang/OTP 24.1.3). However, if both TLS 1.3
and TLS 1.2 are supported using defaults, and TLS 1.3 is negotiated,
the corresponding TLS 1.2 algorithms for TLS 1.3 legacy signature
schemes will be treated as legacy schemes and applied only to
certificate signatures.

# `srp_param_type`

```elixir
-type srp_param_type() :: srp_8192 | srp_6144 | srp_4096 | srp_3072 | srp_2048 | srp_1536 | srp_1024.
```

SRP cipher suite configuration prior to TLS-1.3.

# `legacy_cipher`
*not exported* 

```elixir
-type legacy_cipher() :: '3des_ede_cbc' | des_cbc | rc4_128.
```

Cipher algorithms that are no longer supported by default for security reasons.

# `legacy_hash`
*not exported* 

```elixir
-type legacy_hash() :: sha224 | sha | md5.
```

Hash algorithms that are no longer supported by default for security reasons.

# `legacy_named_curve`
*not exported* 

```elixir
-type legacy_named_curve() ::
          sect571r1 | sect571k1 | sect409k1 | sect409r1 | sect283k1 | sect283r1 | secp256k1 |
          sect239k1 | sect233k1 | sect233r1 | secp224k1 | secp224r1 | sect193r1 | sect193r2 |
          secp192k1 | secp192r1 | sect163k1 | sect163r1 | sect163r2 | secp160k1 | secp160r1 | secp160r2.
```

Key exchange configuration prior to TLS-1.3.

These curves have been deprecated by RFC 8422.

# `legacy_sign_algo`
*not exported* 

```elixir
-type legacy_sign_algo() :: dsa.
```

Signature algorithms that are no longer supported by default for security reasons.

# `legacy_sign_scheme`
*not exported* 

```elixir
-type legacy_sign_scheme() ::
          rsa_pkcs1_sha512 | rsa_pkcs1_sha384 | rsa_pkcs1_sha256 | ecdsa_sha1 | rsa_pkcs1_sha1.
```

This is only used for certificate signatures if TLS-1.2 is negotiated,
meaning that the peer only supports TLS-1.2, but we also support
TLS-1.3.

# `old_cipher_suite`

```elixir
-type old_cipher_suite() ::
          {kex_algo(), cipher(), hash()} | {kex_algo(), cipher(), hash() | aead, hash()}.
```

For backwards compatibility only; do not use.

# `anchor_fun`
*not exported* 

```elixir
-type anchor_fun() :: fun().
```

Claim an intermediate CA in the chain as trusted.

```erlang
fun(Chain::[public_key:der_encoded()]) ->
      {trusted_ca, DerCert::public_key:der_encoded()} | unknown_ca.
```

TLS then uses `public_key:pkix_path_validation/3` with the selected CA
as the trusted anchor and verifies the rest of the chain.

# `cert_key_conf`

```elixir
-type cert_key_conf() ::
          #{cert => public_key:der_encoded() | [public_key:der_encoded()],
            key => key(),
            certfile => file:filename(),
            keyfile => file:filename(),
            password => iodata() | fun(() -> iodata())}.
```

Configuration of the entity certificate and its corresponding key.

A certificate (or possibly a list including the certificate and its
chain certificates, where the entity certificate must be the first
element in the list or the first entry in the file) and its associated
key. For the PEM file format, there can also be a password associated
with the file containing the key.

For maximum interoperability, the certificates in the chain should be
in the correct order, as the chain will be sent as-is to the peer. If
chain certificates are not provided, certificates from the configured
trusted CA certificates will be used to construct the chain. See
[`client_option_cert()`](`t:client_option_cert/0`) and
[`server_option_cert()`](`t:server_option_cert/0`) for more
information.

# `crl_cache_opts`
*not exported* 

```elixir
-type crl_cache_opts() :: {Module :: atom(), {DbHandle :: internal | term(), Args :: list()}}.
```

Options for using built-in CRL cache support.

Specify how to perform lookup and caching of certificate revocation
lists (CRLs). `Module` defaults to `m:ssl_crl_cache` with `DbHandle`
being `internal`, and `Args` being `[]`.

There are two implementations available:

- **`ssl_crl_cache`** - Implementation 1

  This module maintains a cache of CRLs. CRLs can be added to the
  cache using `ssl_crl_cache:insert/1`, and can optionally be
  automatically fetched through HTTP if the following argument is
  specified:

- **`{http, timeout()}`**

    Enables fetching of CRLs specified as http URIs in [X.509 certificate
    extensions](`e:public_key:public_key_records.md`). Requires the
    [Inets](`e:inets:introduction.md`) application.

- **`ssl_crl_hash_dir`** - Implementation 2

  This module makes use of a directory where CRLs are
  stored in files named by the hash of the issuer name.

  The file names consist of eight hexadecimal digits followed by `.rN`, where
  `N` is an integer, for example `1a2b3c4d.r0`. For the first version of the CRL, `N`
  starts at zero, and for each new version, `N` is incremented by one. The
  OpenSSL utility `c_rehash` creates symlinks according to this pattern.

  For a given hash value, this module finds all consecutive `.r*`
  files starting from zero, and those files taken together make up the
  revocation list. CRL files with `nextUpdate` fields in the past or
  issued by a different CA that happens to have the same name hash
  are excluded.

  The following argument is required:

  - **`{dir, string()}`**

  Specifies the directory in which the CRLs can be found.

# `key`

```elixir
-type key() ::
          {'RSAPrivateKey' | 'DSAPrivateKey' | 'ECPrivateKey' | 'PrivateKeyInfo',
           public_key:der_encoded()} |
          #{algorithm := sign_algo(),
            engine := crypto:engine_ref(),
            key_id := crypto:key_id(),
            password => crypto:password()} |
          #{algorithm := sign_algo(),
            sign_fun := fun(),
            sign_opts => list(),
            encrypt_fun => fun(),
            encrypt_opts => list()}.
```

The user's private key.

The key can be provided either directly as a DER-encoded entity,
indirectly using a crypto engine/provider (with key reference
information), or as an Erlang fun (with possible custom options).
The latter two options can be used for customized signing with
hardware security modules (HSM) or trusted platform modules (TPM).

- A DER encoded key will need to specify the ASN-1 type used to create the
  encoding.
- An engine/provider needs to specify specific information to support this
  concept and can optionally be password protected; see also
  [crypto:engine_load/3 ](`crypto:engine_load/3`) and
  [Crypto User's Guide](`e:crypto:engine_load.md`).
- A fun option should include a fun that mimics `public_key:sign/4` and possibly
  [public_key:private_encrypt/4](`public_key:encrypt_private/3`) if legacy
  versions TLS-1.0 and TLS-1.1 must be supported.

# `client_option`
*not exported* 

```elixir
-type client_option() ::
          client_option_cert() |
          common_option_cert() |
          {psk_groups, [group()]} |
          {alpn_advertised_protocols, AppProtocols :: [AppProto :: binary()]} |
          {max_fragment_length, MaxLen :: undefined | 512 | 1024 | 2048 | 4096} |
          client_option_tls13() |
          common_option_tls13() |
          client_option_pre_tls13() |
          common_option_pre_tls13() |
          common_option_dtls() |
          client_option_legacy() |
          common_option_legacy().
```

The following options are specific to the client side, or have
different semantics for the client and server:

- **`{psk_groups, Groups}`** - key exchange groups that the client
will send pre share keys for, defaults to first group in
supported_groups. Must be a subset of supported_groups and will
be sent in the same order as they appear in supported_groups.

- **`{alpn_advertised_protocols, AppProtocols}`** - Application layer protocol

  The list of protocols supported by the client to be sent to the server to be
  used for an Application-Layer Protocol Negotiation (ALPN). If the server
  supports ALPN, it will choose a protocol from this list; otherwise it will
  fail the connection with a `no_application_protocol` alert. A server that does
  not support ALPN will ignore this value. The list of protocols must not contain
  an empty binary.

- **`{max_fragment_length, MaxLen}`** - Max fragment length extension

  Specifies the maximum fragment length the client is prepared to accept from the
  server. See [RFC 6066](http://www.ietf.org/rfc/rfc6066.txt).

# `client_option_cert`

```elixir
-type client_option_cert() ::
          {verify, Verify :: verify_peer | verify_none} |
          {cacerts, CACerts :: [public_key:der_encoded()] | [public_key:combined_cert()]} |
          {cacertfile, CACertFile :: file:filename()} |
          {server_name_indication, SNI :: inet:hostname() | disable} |
          {customize_hostname_check, HostNameCheckOpts :: list()} |
          {certificate_authorities, boolean()} |
          {stapling, Stapling :: staple | no_staple | map()}.
```

Certificate-related options specific to the client side, or with
different semantics for the client and server.

- **`{verify, Verify}`** - Verification of certificates

  This option specifies whether certificates are to be verified.

  If `Verify` is `verify_peer`, which is the default, it is required
  to also provide one of the options `cacerts` or `cacertfile` in
  order for the certificate verification to succeed. For example, an
  HTTPS client can use option `{cacerts, public_key:cacerts_get()}` to
  use the trusted CA certificates provided by the operating system.

  If `Verify` is `verify_none`, all X.509-certificate path
  validation errors will be ignored.

  > #### Change {: .info }
  >
  > The default for `Verify` was changed to `verify_peer` in
  > Erlang/OTP 26.

- **`{cacerts, CACerts}`** - Trusted certificates

  The DER-encoded trusted certificates. If this option is supplied it overrides
  option `cacertfile`.

  Function `public_key:cacerts_get/0` can be used to retrieve to the
  trusted CA certificates provided by the operating system.

- **`{cacertfile, CertFile}`** - End entity certificate

   Path to a file containing PEM-encoded CA certificates. The CA certificates are
   used during server authentication and when building the client certificate
   chain.

  > #### Note {: .info }
  >
  > When PEM caching is enabled, files provided with this option will be checked
  > for updates at fixed time intervals specified by the
  > [ssl_pem_cache_clean](ssl_app.md#configuration) environment parameter.

- **`{server_name_indication, SNI}`** - Server Name Indication extension

  Specify the hostname to be used in TLS Server Name Indication extension. If not
  specified it will default to the `Host` argument of
  [connect/3,4](`connect/3`) unless it is of type [`inet:ip_address()`](`t:inet:ip_address/0`).
  The hostname will also be used in the hostname verification of the peer
  certificate using `public_key:pkix_verify_hostname/2`.
  The special value `disable` prevents the Server Name Indication extension from
  being sent and disables the hostname verification check.

- **`{customize_hostname_check, HostNameCheckOpts}`** - Customization option

  Customizes the hostname verification of the peer certificate, as various
  protocols that use TLS, such as HTTP or LDAP, may require different approaches. For
  example, here is how to use standard hostname checking for HTTPS implemented in
  [Public_Key](`e:public_key:public_key_app.md`):

  ```erlang
  {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
  ```

  For futher description of the customize options, see
  `public_key:pkix_verify_hostname/3`.

- **`{client_certificate_authorities, UseCertAuth}`** - Inter-op hint option

  If `UseCertAuth` is set to `true`, sends the certificate authorities
  extension in the TLS-1.3 client hello. The default is `false`. Note
  that setting `UseCertAuth` to `true` can result in a significant
  overhead if there exists many trusted CA certificates. (Since
  Erlang/OTP 24.3.)

- **`{stapling, Stapling}`** - Certificate revocation check option

  If `Stapling` is atom `staple` or a map, OCSP stapling will be
  enabled, meaning that an extension of type "status_request" will be
  included in the client hello to indicate the desire to receive
  certificate status information.

  If `Stapling` is set to `no_staple` (the default), OCSP stapling will be disabled.

  > #### Note {: .info }
  >
  > Even if requested by the client, the OCSP response might not be
  > provided by the server. In such event, the certificate validation
  > will fail with reason `missing_ocsp_staple`. Capturing this
  > failure with a custom `verify_fun` enables the user to implement
  > their own fallback validation, for example by performing a direct
  > OCSP query or a CRL check. Note however that accepting
  > `{bad_cert, missing_ocsp_staple}` without performing alternative
  > revocation checking is insecure, as it allows a MITM attacker to
  > suppress revocation information by omitting the OCSP staple.
  >
  > ```erlang
  > {verify_fun, {fun(_, _, {bad_cert, missing_ocsp_staple} = R, _St) ->
  >                       %% Implement fallback revocation check here,
  >                       %% e.g. a direct OCSP query or CRL check.
  >                       %% Simply returning {valid, St} skips
  >                       %% revocation checking entirely.
  >                       {fail, R};
  >                  (_, _, {bad_cert, _} = R, _) ->
  >                       {fail, R};
  >                  (_, _, {extension, _}, St) ->
  >                       {unknown, St};
  >                  (_, _, valid, St) ->
  >                       {valid, St};
  >                  (_, _, valid_peer, St) ->
  >                       {valid, St}
  >               end, []}}
  > ```

  When `Stapling` is given as a map, boolean `ocsp_nonce` key can
  indicate whether an OCSP nonce should be requested by the client
  (default is `false`).

  > #### Note {: .info }
  >
  > The OCSP response can be provided without a nonce value — even if it was requested
  > by the client. In such cases SSL will proceed with the handshake and generate
  > a `{missing, ocsp_nonce}` logger event.

# `client_option_legacy`
*not exported* 

```elixir
-type client_option_legacy() ::
          {client_preferred_next_protocols,
           NextAppProtocols ::
               {Precedence :: server | client, ClientPrefs :: [AppProto :: binary()]} |
               {Precedence :: server | client,
                ClientPrefs :: [AppProto :: binary()],
                Default :: (AppProto :: binary())}}.
```

Legacy client options.

- **`{client_preferred_next_protocols, NextAppProtocols}`** - Next Protocol Negotiation

  ALPN (Application-Layer Protocol Negotiation)
  deprecates NPN (Next Protocol Negotiation) and this option.

  Indicates that the client wants to perform Next Protocol Negotiation.

  If `Precedence` is `server`, the negotiated protocol is the first protocol to be
  shown on the server advertised list that is also on the client preference
  list.

  If `Precedence` is `client`, the negotiated protocol is the first protocol to be
  shown on the client preference list that is also on the server advertised
  list.

  If the client does not support any of the server advertised protocols or the
  server does not advertise any protocols, the client falls back to the first
  protocol in its list or to the default protocol (if a default is supplied). If
  the server does not support Next Protocol Negotiation, the connection terminates
  if no default protocol is supplied.

# `client_option_pre_tls13`
*not exported* 

```elixir
-type client_option_pre_tls13() ::
          {reuse_session, SessionRef :: session_id() | {session_id(), SessionData :: binary()}} |
          {reuse_sessions, Reuse :: boolean() | save} |
          {psk_identity, PskID :: string()} |
          {srp_identity, SrpID :: {Username :: string(), Password :: string()}} |
          {fallback, LegacyFallback :: boolean()}.
```

Options only relevant to TLS versions prior to TLS-1.3.

- **`{reuse_session, SessionRef}`** - Explicit session reuse

  Reuses a specific session.

  Since Erlang/OTP 21.3, if the session was saved earlier using option
  `{reuse_sessions, save}`, the session can be referred by its session ID.

  Since Erlang/OTP 22.3, the session can be explicitly specified by
  its session ID and associated data.

  See also
  [SSL User's Guide, Session Reuse pre TLS 1.3.](using_ssl.md#session-reuse-prior-to-tls-1-3)

- **`{reuse_sessions, Reuse}`** - Enables later session reuse

  When `Reuse` is set to `save`, a new connection will be negotiated and saved for later
  reuse. The session ID can be fetched with `connection_information/2` and used
  with the client option `reuse_session`.

  When `Reuse` is set to `true`, automated session reuse will be
  performed, if possible. If a new session is created, and is unique in regard to previous
  stored sessions, it will be saved for possible later reuse.

  Since: OTP 21.3.

- **`{psk_identity, PskID}`** - Option for use with PSK cipher suites

  Specifies the identity the client presents to the server. The matching secret is
  found by the fun given in the `user_lookup_fun` option.

- **`{srp_identity, SrpID}`**  - Option for use SRP cipher suites

  Specifies the username and password to use to authenticate to the server.

- **`{fallback, LegacyFallback}`** - Inter-op legacy client option

  Send special cipher suite TLS_FALLBACK_SCSV to avoid an undesired TLS version
  downgrade. Defaults to `false`.

  > #### Warning {: .warning }
  >
  > This option is not needed in normal TLS usage and must not be used to
  > implement new clients. However, legacy clients that retries connections in the
  > following manner:
  >
  > `ssl:connect(Host, Port, [...{versions, ['tlsv2', 'tlsv1.1', 'tlsv1']}])`
  >
  > `ssl:connect(Host, Port, [...{versions, [tlsv1.1', 'tlsv1']}, {fallback, true}])`
  >
  > `ssl:connect(Host, Port, [...{versions, ['tlsv1']}, {fallback, true}])`
  >
  > can use it to avoid undesired TLS version downgrade. Note that
  > TLS_FALLBACK_SCSV must also be supported by the server for the prevention to
  > work.

# `client_option_tls13`
*not exported* 

```elixir
-type client_option_tls13() ::
          {session_tickets, SessionTickets :: disabled | manual | auto} |
          {use_ticket, Tickets :: [session_ticket()]} |
          {early_data, binary()} |
          {middlebox_comp_mode, MiddleBoxMode :: boolean()}.
```

Options only relevant for TLS-1.3.

- **`{session_tickets, SessionTickets}`** - Use of session tickets

  Configures the session ticket functionality. Allowed values are `disabled`,
  `manual`, and `auto`. If it is set to `manual` the client will send the ticket
  information to user process in a 3-tuple:

  ```erlang
  {ssl, session_ticket, `Ticket::`(`t:session_ticket/0`)}
  ```

  where `Ticket` is a map with information about the created TLS-1.3 session ticket.
  The only key that the user needs to consider is `sni` key to be able to
  provide it as a value in the use_ticket option list of possible tickets
  to use it to attempt session resumption to a server identified by the
  server name indication in `manual` session ticket mode.

  If it is set to `auto`, the client automatically handles received tickets and
  tries to use them when making new TLS connections (session resumption with
  pre-shared keys).

  Ticket lifetime, the number of tickets sent by the server, and the
  maximum number of tickets stored by the server in stateful mode are configured
  by [application variables](ssl_app.md#configuration).

  See also
  [SSL User's Guide, Session Tickets and Session Resumption in TLS 1.3](using_ssl.md#session-tickets-and-session-resumption-in-tls-1-3).

- **`{use_ticket, Tickets}`**

  Configures the session tickets to be used for session resumption. It is a
  mandatory option in `manual` mode (`{session_tickets, manual}`).

  > #### Note {: .info }
  >
  > Session tickets are only sent to the user if option `session_tickets` is set to
  > `manual`
  >
  > This option is supported by TLS-1.3. See also
  > [SSL User's Guide, Session Tickets and Session Resumption in TLS 1.3](using_ssl.md#session-tickets-and-session-resumption-in-tls-1-3).

- **`{early_data, EarlyData}`**

  Configures the early data to be sent by the client.

  To verify that the server has the intention to process the early
  data, the following tuple is sent to the user process:

  ```erlang
  {ssl, SslSocket, {early_data, Result}}
  ```

  where `Result` is either `accepted` or `rejected`.

  > #### Warning {: .warning }
  >
  > It is the responsibility of the user to handle rejected `EarlyData` and to
  > resend when appropriate.

- **`{middlebox_comp_mode, MiddleBoxMode}`**

  Configures the middlebox compatibility mode for a TLS-1.3 connection.

  A significant number of middleboxes misbehave when a TLS-1.3
  connection is negotiated. Implementations can increase the chance of
  making connections through those middleboxes by adapting the TLS-1.3
  handshake to resemble that of a TLS-1.2 handshake.

  The middlebox compatibility mode is enabled (`true`) by default.

# `session_ticket`

```elixir
-nominal session_ticket() :: #{sni := inet:hostname()}.
```

# `common_option`
*not exported* 

```elixir
-type common_option() ::
          {protocol, tls | dtls} |
          {handshake, hello | full} |
          {ciphers, cipher_suites()} |
          {signature_algs, signature_algs()} |
          {signature_algs_cert, [sign_scheme()]} |
          {keep_secrets,
           KeepSecrets ::
               boolean() |
               {keylog_hs, fun((Info :: keylog_info()) -> any())} |
               {keylog, fun((Info :: keylog_info()) -> any())}} |
          {max_handshake_size, HandshakeSize :: pos_integer()} |
          {versions, [protocol_version()]} |
          {log_level, Level :: logger:level() | none | all} |
          {hibernate_after, HibernateTimeout :: timeout()} |
          {receiver_spawn_opts, SpawnOpts :: [erlang:spawn_opt_option()]} |
          {sender_spawn_opts, SpawnOpts :: [erlang:spawn_opt_option()]}.
```

Options common to both client and server side.

- **`{protocol, Protocol}`** - Choose TLS or DTLS protocol for the transport layer security.

  Defaults to `tls`.

- **`{handshake, Completion}`** - Possibly pause handshake at hello stage.

  Defaults to `full`. If `hello` is specified the handshake will pause
  after the hello message, allowing the user to make decisions based
  on hello extensions before continuing or aborting the handshake by
  calling `handshake_continue/3` or `handshake_cancel/1`.

- **`{keep_secrets, KeepSecrets}`** - Configures a TLS connection for keylogging.

  In order to be able retrieve all keylog information on a TLS connection, it must be
  configured in advance.

  > #### Warning {: .warning }
  > The keylog information defeats the purpose of the protocol
  > and enabling it makes the user responsible for the information
  > not ending up compromising security, it is intended for debugging.

  The `keep_secrets` functionality is disabled (`false`) by default.
  If set to legacy value `true` keylog information can be retrieved from the connection
  using connection_information/2.

  Added in OTP 23.2.

  > #### Note {: .info }
  > Note that having to ask the connection has some drawbacks
  > as for instance you can not get keylog information for
  > failed connections, and other keylog items have
  > to be retrieved in a polling manner and are not correctly
  > formatted for key_updates.

  Since OTP 27.3.1 you may instead of true provide a callback fun
  providing keylog information for either just failing handshakes or
  for entire connections, by setting `keep_secrets` option to
  {keylog_hs, fun()} or {keylog, fun()}.  The fun is of arity one and
  will be called with keylog information
  [`keylog_info()`](`t:keylog_info/0`) as an argument. `keylog_hs fun`
  will only be called if the handshake fails, and is only relevant for
  `TLS-1.3` that has encrypted messages before the first handshake is
  complete.`keylog fun` will be called every time some secrets are
  updated and provide keylog for that update that is during the
  connection establishment and after that at `renegotiation` or `key
  update` (depending on TLS protocol version).  When a fun is used the
  connection_information/2 can not be used to retrieve key log
  information.  For more information see [NSS
  keylog](using_ssl.md#nss-keylog).

- **`{max_handshake_size, HandshakeSize}`** - Limit the acceptable handshake packet size.

  Used to limit the size of valid TLS handshake packets to avoid DoS
  attacks.

  Integer (24 bits, unsigned). Defaults to `256*1024` before OTP-26 or if SLH-DSA algorithms
  are configured, otherwise the default is `256*1024`/2.

- **`{hibernate_after, HibernateTimeout}`** - Hibernate inactive connection processes.

  When an integer-value is specified, the TLS/DTLS connection goes into hibernation
  after the specified number of milliseconds of inactivity, thus reducing its
  memory footprint. When not specified the process never goes into hibernation.

- **`{log_level, Level}`** - Specifies the log level for a TLS/DTLS connection.

  Alerts are logged on `notice`
  level, which is the default level. The level `debug` triggers verbose logging of
  TLS/DTLS protocol messages. See also [SSL Application](ssl_app.md)

- **`{receiver|sender_spawn_opts, SpawnOpts}`** - Configure erlang spawn opts.

  Configures spawn options of TLS sender and receiver processes.

  Setting up garbage collection options can be helpful for trade-offs between CPU
  usage and memory usage. See `erlang:spawn_opt/2`.

  For connections using Erlang distribution, the default sender option
  is `[...{priority, max}]`; this priority option cannot be changed. For all
  connections, `...link` is added to receiver and cannot be changed.

# `common_option_cert`
*not exported* 

```elixir
-type common_option_cert() ::
          {certs_keys, CertsKeys :: [cert_key_conf()]} |
          {depth, AllowedCertChainLen :: pos_integer()} |
          {verify_fun, Verify :: {Verifyfun :: fun(), InitialUserState :: any()}} |
          {cert_policy_opts,
           PolicyOpts ::
               [{policy_set, [public_key:oid()]} |
                {explicit_policy, boolean()} |
                {inhibit_policy_mapping, boolean()} |
                {inhibit_any_policy, boolean()}]} |
          {allow_any_ca_purpose, Allow :: boolean()} |
          {crl_check, Check :: boolean() | peer | best_effort} |
          {crl_cache, crl_cache_opts()} |
          {partial_chain, anchor_fun()}.
```

Common certificate related options to both client and server.

- **`{certs_keys, CertsKeys}`** - At least one certificate and key pair.

  A list of a certificate (or possible a certificate and its chain)
  and the associated key of the certificate that can be used to
  authenticate the client or the server. The certificate key pair that
  is considered best and matches negotiated parameters for the
  connection will be selected.

  The different signature algorithms are prioritized in the following
  order: `eddsa`, `ecdsa`, `rsa_pss_pss`, `rsa`, and `dsa`. If more
  than one key is supplied for the same signature algorithm, they will
  be prioritized by strength (except for _engine keys_; see the next
  paragraph). This offers flexibility to, for instance, configure a
  newer certificate that is expected to be used in most cases, and an
  older but acceptable certificate that will only be used to
  communicate with legacy systems. Note that there is a trade off
  between the induced overhead and the flexibility; thus, alternatives
  should be chosen for good reasons.

  _Engine keys_ will be favored over other keys. As engine keys cannot
  be inspected, supplying more than one engine key makes no sense.

  When this option is specified it overrides all single certificate
  and key options. For examples, see the [User's Guide](using_ssl.md).

  > #### Note {: .info }
  >
  > `eddsa` certificates are only supported by TLS-1.3 implementations that do not support `dsa`
  > certificates. `rsa_pss_pss` (RSA certificates using Probabilistic Signature
  > Scheme) are supported in TLS-1.2 and TLS-1.3, but some TLS-1.2 implementations
  > do not support `rsa_pss_pss`.

- **`{depth, AllowedCertChainLen}`** - Limits the accepted number of certificates in the certificate chain.

  Maximum number of non-self-issued intermediate certificates that can follow the
  peer certificate in a valid certification path. So, if depth is 0 the PEER must
  be signed by the trusted ROOT-CA directly; if 1 the path can be PEER, CA,
  ROOT-CA; if 2 the path can be PEER, CA, CA, ROOT-CA, and so on. The default
  value is 10. Used to mitigate DoS attack possibilities.

- **`{verify_fun,  Verify}`** - Customize certificate path validation

  The verification fun is to be defined as follows:

  ```erlang
  fun(OtpCert :: #'OTPCertificate'{},
      Event, InitialUserState :: term()) ->
	{valid, UserState :: term()} |
	{fail, Reason :: term()} | {unknown, UserState :: term()}.

  fun(OtpCert :: #'OTPCertificate'{}, DerCert :: public_key:der_encoded(),
      Event, InitialUserState :: term()) ->
	{valid, UserState :: term()} |
	{fail, Reason :: term()} | {unknown, UserState :: term()}.

  Types:
        Event = {bad_cert, Reason :: atom() |
                {revoked, atom()}} |
		{extension, #'Extension'{}} |
                valid |
                valid_peer
  ```

  The verification fun is called during the X.509-path validation when
  an error occurs or an extension unknown to the SSL application is
  encountered. It is also called when a certificate is considered
  valid by the path validation to allow access to each certificate in
  the path to the user application. It differentiates between the peer
  certificate and the CA certificates by using `valid_peer` or `valid`
  as `Event` argument to the verification fun. See the [Public_Key
  User's Guide](`e:public_key:public_key_records.md`) for definition
  of `#'OTPCertificate'{}` and `#'Extension'{}`.

  - If the verify callback fun returns `{fail, Reason}`, the verification process
    is immediately stopped, an alert is sent to the peer, and the TLS/DTLS
    handshake terminates.
  - If the verify callback fun returns `{valid, UserState}`, the verification
    process continues.
  - If the verify callback fun always returns `{valid, UserState}`, the TLS/DTLS
    handshake does not terminate regardless of verification failures, and the
    connection is established.
  - If called with an extension unknown to the user application, the fun is to
    return `{unknown, UserState}`.

  Note that if the fun returns `unknown` for an extension marked as critical,
  validation will fail.

  Default option `verify_fun` in `verify_peer mode`:

  ```erlang
  {fun(_, _, {bad_cert, _} = Reason, _) ->
	 {fail, Reason};
      (_, _, {extension, _}, UserState) ->
	 {unknown, UserState};
      (_, _, valid, UserState) ->
	 {valid, UserState};
      (_, _, valid_peer, UserState) ->
         {valid, UserState}
   end, []}
  ```

  Default option `verify_fun` in mode `verify_none`:

  ```erlang
   {fun(_, _, {bad_cert, _}, UserState) ->
	 {valid, UserState};
      (_, _, {extension, #'Extension'{critical = true}}, UserState) ->
	 {valid, UserState};
      (_, _, {extension, _}, UserState) ->
	 {unknown, UserState};
      (_, _, valid, UserState) ->
	 {valid, UserState};
      (_, _, valid_peer, UserState) ->
         {valid, UserState}
   end, []}
  ```

  The possible path validation errors are given in the form `{bad_cert, Reason}`,
  where `Reason` is:

  - **`unknown_ca`**

    No trusted CA was found in the trusted store. The trusted
    CA is normally a so-called ROOT CA, which is a self-signed certificate. Trust
    can be claimed for an intermediate CA (the trusted anchor does not have to be
    self-signed according to X-509) by using option `partial_chain`.

  - **`selfsigned_peer`**

    The chain consisted only of one self-signed certificate.

  - **{invalid_ext_keyusage, [public_key:oid()]} **

   If the peer certificate specifies the extended keyusage extension and does
   not include the purpose for either being a TLS server (id-kp-ServerAuth) or
   TLS client (id-kp-ClientAuth) depending on the peers role.

  - **{ca_invalid_ext_keyusage, [public_key:oid()]} **

   If a CA certificate specifies the extended keyusage extension and does
   not include the purpose for either being a TLS server
   (id-kp-ServerAuth) or TLS client (id-kp-ClientAuth) depending
   on the role of the peer chained with this CA, or the option allow_any_ca_purpose is set to `true`
   but the special any-value (anyExtendedKeyUsage) is not included in the CA cert purposes.

  - **`PKIX X-509-path validation error`**

    For possible reasons, see `public_key:pkix_path_validation/3`.

- **`{cert_policy_opts, PolicyOpts}`** - Handle certificate policies.

  Configure X.509 certificate policy handling for the certificate path validation process;
  see [public_key:pkix_path_validation/3](`public_key:pkix_path_validation/3`) for
  more details.

- **`{allow_any_ca_purpose, boolean()}`** - Handle certificate extended key usages extension

  If a CA certificate has an extended key usage extension but it does not want to
  restrict the usages of the key it can include a special `anyExtendedKeyUsage` purpose.
  If this is option is set to `true` all key usage purposes is automatically
  accepted for a CA that include that purpose, the options default to false.

- **`{crl_check, Check}`**  - Handle certificate revocation lists.

  Perform CRL (Certificate Revocation List) verification
  [(public_key:pkix_crls_validate/3)](`public_key:pkix_crls_validate/3`) on all
  the certificates during the path validation
  [(public_key:pkix_path_validation/3) ](`public_key:pkix_path_validation/3`)of
  the certificate chain. `Check` defaults to `false`.

  The meaning of `Check` is as follows:

  - **`false`**

    No checks are performed.

  - **`peer`**

    Check is only performed on the peer certificate.

  - **`best_effort`**

    If certificate revocation status cannot be determined it will be accepted as valid.

    The CA certificates specified for the connection will be used to construct the
    certificate chain validating the CRLs.

    The CRLs will be fetched from a local or external cache. See
    `m:ssl_crl_cache_api`.

# `common_option_dtls`
*not exported* 

```elixir
-type common_option_dtls() ::
          {use_srtp, UseSrtp :: #{protection_profiles := [binary()], mki => binary()}}.
```

Common options to client and server only valid for DTLS.

- **`{use_srtp, UseSrtp}`** - Configures the `use_srtp` DTLS hello extension.

  In order to negotiate the use of SRTP data protection, clients include an
  extension of type "use_srtp" in the DTLS extended client hello. This extension
  MUST only be used when the data being transported is RTP or RTCP.

  The value is a map with a mandatory `protection_profiles` parameter
  and an optional `mki` parameter.

  `protection_profiles` configures the list of the client's acceptable SRTP
  Protection Profiles. Each profile is a 2-byte binary. Example:
  `#{protection_profiles => [<<0,2>>, <<0,5>>]}`

  `mki` configures the SRTP Master Key Identifier chosen by the client.

  The `srtp_mki` field contains the value of the SRTP MKI which is associated with
  the SRTP master keys derived from this handshake. Each SRTP session MUST have
  exactly one master key that is used to protect packets at any given time. The
  client MUST choose the MKI value so that it is distinct from the last MKI value
  that was used, and it SHOULD make these values unique for the duration of the
  TLS session.

  > #### Note {: .info }
  >
  > OTP does not handle SRTP, so an external implementations of SRTP
  > encoder/decoder and a packet demultiplexer are needed to make use of the
  > `use_srtp` extension. See also option [transport_option](`t:transport_option/0`).

  Servers that receive an extended hello containing a "use_srtp" extension can
  agree to use SRTP by including an extension of type "use_srtp", with the chosen
  protection profile in the extended server hello. This extension MUST only be
  used when the data being transported is RTP or RTCP.

# `common_option_legacy`
*not exported* 

```elixir
-type common_option_legacy() ::
          {cert, Cert :: public_key:der_encoded() | [public_key:der_encoded()]} |
          {certfile, CertPem :: file:filename()} |
          {key, Key :: key()} |
          {keyfile, KeyPem :: file:filename()} |
          {password, KeyPemPasswd :: iodata() | fun(() -> iodata())} |
          {log_alert, LogAlert :: boolean()} |
          {padding_check, PaddingCheck :: boolean()} |
          {beast_mitigation, one_n_minus_one | zero_n | disabled} |
          {ssl_imp, Imp :: new | old}.
```

Legacy options considered deprecated in favor of other options,
insecure to use, or plainly not relevant anymore.

- **`{cert, Certs}`**

  Use option `certs_keys` instead.

- **`{certfile, CertPem}`**

  Use option `certs_keys` instead.

- **`{keyfile, KeyPem}`**

  Use option `certs_keys` instead.

- **`{password, KeyPemPasswd}`**

  Use option `certs_keys` instead.

- **`{log_alert, LogAlert}`**

  If `LogAlert` is `false`, TLS/DTLS Alert reports are not displayed. Deprecated in OTP
  22; use `{log_level, Level}` instead.

- **`{padding_check, PaddingCheck}`** - Inter-op trade-off option

  Affects TLS-1.0 connections only. If set to `false`, it disables the block
  cipher padding check to be able to interoperate with legacy software.

  > #### Warning {: .warning }
  >
  > Using `{padding_check, false}` makes TLS vulnerable to the Poodle attack.

- **`{beast_mitigation, BeastMitigation}`**  - Inter-op trade-off option

  Affects TLS-1.0 connections only. Used to change the BEAST mitigation strategy
  to interoperate with legacy software. Defaults to `one_n_minus_one`.

  `one_n_minus_one` - Perform `1/n-1` BEAST mitigation.

  `zero_n` - Perform `0/n` BEAST mitigation.

  `disabled` - Disable BEAST mitigation.

  > #### Warning {: .warning }
  >
  > Using `{beast_mitigation, disabled}` makes TLS-1.0 vulnerable to the BEAST
  > attack.

- **`{ssl_imp, Imp}`**

  Deprecated since OTP 17; has no effect.

# `common_option_pre_tls13`
*not exported* 

```elixir
-type common_option_pre_tls13() ::
          {eccs, NamedCurves :: [named_curve()]} |
          {secure_renegotiate, SecureRenegotiate :: boolean()} |
          {user_lookup_fun, {Lookupfun :: fun(), UserState :: any()}}.
```

Options common to client and server side prior to TLS-1.3.

- **`{eccs, NamedCurves}`** - Named Elliptic Curves

  Elliptic curves that can be used in pre TLS-1.3 key exchange.

- **`{secure_renegotiate, SecureRenegotiate}`** - Inter-operate trade-off option

  Specifies whether to reject renegotiation attempt that does not live
  up to [RFC 5746](http://www.ietf.org/rfc/rfc5746.txt). By default,
  `SecureRenegotiate` is `true`, meaning that secure renegotiation is
  enforced. If `SecureRenegotiate` is `false` secure renegotiation
  will still be used if possible, but it falls back to insecure
  renegotiation if the peer does not support if [RFC
  5746](http://www.ietf.org/rfc/rfc5746.txt).

- **`{user_lookup_fun, {LookupFun, UserState}}`** - PSK/SRP cipher suite option

  The lookup fun is to be defined as follows:

  ```erlang
  fun(psk, PSKIdentity :: binary(), UserState :: term()) ->
	{ok, SharedSecret :: binary()} | error;
  fun(srp, Username :: binary(), UserState :: term()) ->
	{ok, {SRPParams :: srp_param_type(), Salt :: binary(),
	      DerivedKey :: binary()}} | error.
  ```

  For Pre-Shared Key (PSK) cipher suites, the lookup fun is called by the client
  and server to determine the shared secret. When called by the client,
  `PSKIdentity` is the hint presented by the server or `undefined`. When
  called by the server, `PSKIdentity` is the identity presented by the client.

  For Secure Remote Password (SRP), the fun is only used by the server to obtain
  parameters that it uses to generate its session keys. `DerivedKey` is to be
  derived according to [RFC 2945](http://tools.ietf.org/html/rfc2945#section/3)
  and [RFC 5054](http://tools.ietf.org/html/rfc5054#section-2.4):
  `crypto:sha([Salt, crypto:sha([Username, <<$:>>, Password])])`

# `common_option_tls13`

```elixir
-type common_option_tls13() ::
          {supported_groups, [group()]} | {key_update_at, KeyUpdateAt :: pos_integer()}.
```

Common options to both client and server for TLS-1.3.

- **`{supported_groups, Groups}`** - Key exchange option

  TLS 1.3 introduces the "supported_groups" extension, which is used for negotiating
  the key-exchange parameters in a TLS 1.3 handshake. Both client and server can
  specify a list of parameters that they are willing to use.

  If not specified it will use a default list that can be obtained
  by calling `ssl:groups(default).` that is filtered based on the installed crypto library version.

- **`{key_update_at, KeyUpdateAt}`** - Session key renewal

  Configures the maximum amount of bytes that can be sent on a TLS 1.3 connection
  before an automatic key update is performed.

  There are cryptographic limits on the amount of plaintext which can be safely
  encrypted under a given set of keys. The current default ensures that data
  integrity will not be breached with probability greater than `1/2^57`. For more
  information see
  [Limits on Authenticated Encryption Use in TLS](https://eprint.iacr.org/2024/051.pdf).

# `prf_random`

> This type is deprecated. the type ssl:prf_random() is deprecated; Only used in deprecated function prf/5 and will no longer be needed..

```elixir
-type prf_random() :: client_random | server_random.
```

# `connection_info`

```elixir
-type connection_info() ::
          [{protocol, protocol_version()} |
           {session_resumption, boolean()} |
           {selected_cipher_suite, erl_cipher_suite()} |
           {sni_hostname, term()} |
           {ciphers, [erl_cipher_suite()]}] |
          connection_info_pre_tls13() |
          security_info().
```

Key value list convening some information about the established connection.

# `connection_info_keys`

```elixir
-type connection_info_keys() ::
          [protocol | selected_cipher_suite | sni_hostname | session_resumption | ciphers |
           client_random | server_random | master_secret | keylog | session_id | session_data | ecc |
           srp_username].
```

TLS connection keys for which information can be retrieved.

# `connection_info_pre_tls13`
*not exported* 

```elixir
-type connection_info_pre_tls13() ::
          [{session_id, session_id()} |
           {session_data, binary()} |
           {ecc, {named_curve, term()}} |
           {srp_username, term()}].
```

TLS connection information relevant prior to TLS-1.3.

# `security_info`
*not exported* 

```elixir
-type security_info() ::
          [{client_random, binary()} |
           {server_random, binary()} |
           {master_secret, binary()} |
           {keylog, [keylog_item()]}].
```

TLS connection information that can be used for NSS key logging.

# `server_option`
*not exported* 

```elixir
-type server_option() ::
          server_option_cert() |
          common_option_cert() |
          {alpn_preferred_protocols, AppProtocols :: [binary()]} |
          {sni_hosts, SNIHosts :: [{inet:hostname(), [server_option() | common_option()]}]} |
          {sni_fun,
           SNIFun :: fun((string()) -> [server_option() | common_option()] | unrecognized | undefined)} |
          server_option_pre_tls13() |
          common_option_pre_tls13() |
          server_option_tls13() |
          common_option_tls13() |
          common_option_dtls() |
          server_option_legacy() |
          common_option_legacy().
```

Options specific to the server side, or with different semantics for the client and server.

- **`{alpn_preferred_protocols, AppProtocols}`** - Application Layer Protocol Negotiation

  Indicates that the server will try to perform Application-Layer
  Protocol Negotiation (ALPN).

  The list of protocols is in order of preference. The protocol negotiated will be
  the first in the list that matches one of the protocols advertised by the
  client. If no protocol matches, the server will fail the connection with a
  `no_application_protocol` alert.

  The negotiated protocol can be retrieved using the
  [`negotiated_protocol/1`](`negotiated_protocol/1`) function.

- **`{sni_fun, SNIFun}`**

  If the server receives a SNI (Server Name Indication) from the
  client, the given fun `SNIFun` will be called to retrieve
  [`server_option()`](`t:server_option/0`) for the indicated
  server. These options will override previously specified server options.
  The sni_fun can indicate that it does not recognize the server name by
  returning `unrecognized` in which case the connection will be closed with an
  `unrecognized_name` alert. If the
  sni_fun returns `undefined` the connection will be attempted with the default
  options supplied to `listen/2` or  [`handshake/2,3`](`handshake/2`).

  > #### Note {: .info }
  The options `sni_fun` and `sni_hosts` are mutually exclusive.

- **`{sni_hosts, SNIHosts}`**

  If the server receives a SNI (Server Name Indication) from the client matching a
  host listed in the `sni_hosts` option, the specific options for that host will
  override previously specified options. If no match is found it behaves as
  option sni_fun that returns `undefined`.

  > #### Note {: .info }
  The options `sni_fun` and `sni_hosts` are mutually exclusive.

# `server_option_cert`

```elixir
-type server_option_cert() ::
          {cacerts, CACerts :: [public_key:der_encoded()] | [public_key:combined_cert()]} |
          {cacertfile, CACertFile :: file:filename()} |
          {verify, Verify :: verify_none | verify_peer} |
          {fail_if_no_peer_cert, FailNoPeerCert :: boolean()} |
          {certificate_authorities, ServerCertAuth :: boolean()}.
```

Certificate related options for a server.

- **`{cacerts, CACerts}`** - Trusted certificates.

  The DER-encoded trusted certificates. If this option is supplied, it overrides
  the `cacertfile` option.

- **`{verify, Verify}`** - Verify certificates.

  Client certificates are an optional part of the TLS protocol. A server performs
  X.509 certificate path validation only in `verify_peer` mode. By default the server
  is in `verify_none` mode and, hence, will not send an certificate request to the
  client. When using `verify_peer` you may also want to specify the options
  `fail_if_no_peer_cert` and `certificate_authorities`.

- **`{fail_if_no_peer_cert, FailNoPeerCert}`** - Legacy trade-off option

  Used together with `{verify, verify_peer}` by an TLS/DTLS server. If set to
  `true`, the server fails if the client does not have a certificate to send, that
  is, sends an empty certificate. If set to `false`, it fails only if the client
  sends an invalid certificate (an empty certificate is considered valid).
  Defaults to `true`, the default value was changed in OTP-26.0.

- **`{certificate_authorities, ServerCertAuth}`** - Inter-operate hint option

  Determines whether a TLS-1.3 server should include the authorities extension in its
  certificate request message that is sent when the option `verify` is set to
  `verify_peer`. Defaults to `true`.

  If set to `false` for older TLS versions its corresponding certificate authorities
  definition in its certificate request will be set to the empty list instead of
  including the appropriate certificate authorities. This has the same affect
  as excluding the TLS-1.3 extension.

  A reason to exclude the extension would be if the server wants to communicate
  with clients incapable of sending complete certificate chains that adhere to the
  extension, but the server still has the capability to recreate a chain that it
  can verify.

# `server_option_legacy`
*not exported* 

```elixir
-type server_option_legacy() :: {next_protocols_advertised, NextAppProtocols :: [binary()]}.
```

Legacy server options.

- **`{next_protocols_advertised, NextAppProtocols}`**

  ALPN (Application-Layer Protocol Negotiation)
  deprecates NPN (Next Protocol Negotiation) described here.

  List of protocols to send to the client if the client indicates that it supports
  the Next Protocol extension. The client can select a protocol that is not on
  this list. The list of protocols must not contain an empty binary. If the server
  negotiates a Next Protocol, it can be accessed using the
  `negotiated_protocol/1` method.

# `server_option_pre_tls13`
*not exported* 

```elixir
-type server_option_pre_tls13() ::
          {client_renegotiation, ClientRengotiation :: boolean()} |
          {reuse_sessions, ReuseSessions :: boolean()} |
          {reuse_session, ReuseSession :: fun()} |
          {honor_cipher_order, HonorServerCipherOrder :: boolean()} |
          {honor_ecc_order, HonorServerECCOrder :: boolean()} |
          {dh, DHDer :: public_key:der_encoded()} |
          {dhfile, DhFile :: file:filename()} |
          {psk_identity, PSKHint :: string()}.
```

Options only relevant to TLS versions prior to TLS-1.3.

- **`{client_renegotiation, ClientRengotiation}`** - DoS attack avoidance option

  In protocols that support client-initiated renegotiation, the resource cost
  of such an operation is higher for the server than the client. This can act as a
  vector for denial-of-service (DoS) attacks. The SSL application already takes measures
  to counter-act such attempts, but client-initiated renegotiation can be completely
  disabled by setting this option to `false`. The default value is `true`. Note
  that disabling renegotiation can result in long-lived connections becoming
  unusable due to limits on the number of messages the underlying cipher suite can
  encipher.

- **`{reuse_sessions, ReuseSessions}`** - Enable session reuse

  The boolean value `true` specifies that the server will agree to reuse sessions.
  Setting it to `false` will result in an empty session table, meaning that no sessions
  will be reused.

- **`{reuse_session, ReuseSession}`** - Local server reuse policy

  Enables the TLS/DTLS server to have a local policy for deciding whether a session
  is to be reused. Meaningful only if `reuse_sessions` is set to `true`.

  `ReuseSession` should be a fun:

   `fun(SuggestedSessionId, PeerCert, Compression, CipherSuite)`

  `SuggestedSessionId` is a [`binary()`](`t:binary/0`),
  `PeerCert` is a DER-encoded certificate,
  `Compression` is an enumeration integer, and `CipherSuite` is of type
  [`erl_cipher_suite()`](`t:erl_cipher_suite/0`).

- **`{psk_identity, PSKHint}`** - Inter-operate hint option

  Specifies the server identity hint that the server presents to the client.

- **`{honor_cipher_order, HonorServerCipherOrder}`** - Trade-off option alters protocol defined behavior

  If `true`, use the server's preference for ECC curve selection. If `false` (the
  default), use the client's preference.

- **`{honor_ecc_order, HonorServerECCOrder}`** - Trade-off option alters protocol defined behavior

  If `true`, use the server's preference for ECC curve selection. If `false` (the
  default), use the client's preference.

- **`{dh, DHder}`** - Affects DH key exchange cipher suites

  The DER-encoded Diffie-Hellman parameters. If specified, it overrides option
  `dhfile`.

- **`{dh_file, DHfile}`** - Affects DH key exchange cipher suites

  Path to a file containing PEM-encoded Diffie Hellman parameters to be used by
  the server if a cipher suite using Diffie Hellman key exchange is negotiated. If
  not specified, default parameters are used.

# `server_option_tls13`
*not exported* 

```elixir
-type server_option_tls13() ::
          {session_tickets,
           SessionTickets :: disabled | stateful | stateless | stateful_with_cert | stateless_with_cert} |
          {stateless_tickets_seed, TicketSeed :: binary()} |
          {anti_replay,
           '10k' | '100k' |
           {BloomFilterWindowSize :: pos_integer(),
            BloomFilterHashFunctions :: pos_integer(),
            BloomFilterBits :: pos_integer()}} |
          {cookie, Cookie :: boolean()} |
          {early_data, EarlyData :: enabled | disabled}.
```

Options only relevant for TLS-1.3.

- **`{session_tickets, SessionTickets}`**

  Configures the session ticket functionality. Allowed values for `SessionTickets` are:

  * `disabled`
  * `stateful`
  * `stateless`
  * `stateful_with_cert`
  * `stateless_with_cert`

  If `SessionTickets` is not set to `disabled`, session resumption with pre-shared
  keys is enabled and the server will send stateful or stateless session tickets to the
  client after successful connections.

  > #### Note {: .info }
  In pre-shared key session ticket resumption, there is no certificate
  exchange involved. Therefore, `ssl:peercert/1` will not return the
  peer certificate, as it is only communicated during the initial
  handshake. To associate the client certificate from the original
  handshake with the tickets it issues, the server options
  `stateful_with_cert` or `stateless_with_cert` can be used.

  A stateful session ticket is a database reference to internal state information.
  A stateless session ticket is a self-encrypted binary that contains both
  cryptographic keying material and state data.

  > #### Warning {: .warning }
  When `SessionTickets` is set to `stateful_with_cert`, the client
  certificate is stored along with the internal state information,
  leading to increased memory consumption. Conversely, when it is set
  to `stateless_with_cert`, the client certificate is encoded in the
  self-encrypted binary sent to the client, resulting in an increase
  in payload size.

  See also [SSL User's Guide, Session Tickets and Session Resumption in TLS 1.3](using_ssl.md#session-tickets-and-session-resumption-in-tls-1-3).

- **`{stateless_tickets_seed, TicketSeed}`** - Option for statless tickets

  Configures the seed used for the encryption of stateless session tickets.
  Allowed values are any randomly generated `t:binary/0`. If this option is not
  configured, an encryption seed will be randomly generated.

  > #### Warning {: .warning }
  >
  > Reusing the ticket encryption seed between multiple server instances enables
  > stateless session tickets to work across multiple server instances, but it
  > breaks anti-replay protection across instances.
  >
  > Inaccurate time synchronization between server instances can also affect
  > session ticket freshness checks, potentially causing false negatives as well
  > as false positives.

- **`{anti_replay, AntiReplay}`** - Option for statless tickets

  Configures the server's built-in anti replay feature based on Bloom filters.

  Allowed values for `AntiReplay` are the pre-defined `'10k'`,
  `'100k'`, or a custom 3-tuple that defines the properties of the
  bloom filters:
  `{WindowSize, HashFunctions, Bits}`. `WindowSize` is the number of seconds after
  the current Bloom filter is rotated and also the window size used for freshness
  checks of ClientHello. `HashFunctions` is the number hash functions and `Bits`
  is the number of bits in the bit vector. `'10k'` and `'100k'` are simple
  defaults with the following properties:

  - `'10k'`: Bloom filters can hold 10000 elements with 3% probability of false
    positives. `WindowSize`: 10, `HashFunctions`: 5, `Bits:` 72985 (8.91 KiB).
  - `'100k'`: Bloom filters can hold 100000 elements with 3% probability of false
  positives. `WindowSize`: 10, `HashFunctions`: 5, `Bits`: 729845 (89.09 KiB).

  See also [SSL User's Guide, Anti-Replay Protection in TLS
  1.3](using_ssl.md#anti-replay-protection-in-tls-1-3).

- **`{cookie, Cookie}`** - Option for `HelloRetryRequest` behavior

  If `Cookie` is `true`, which is the default, the server sends a
  cookie extension in its `HelloRetryRequest` messages.

  The cookie extension has two main purposes. It allows the server to force the
  client to demonstrate reachability at their apparent network address (thus
  providing a measure of DoS protection). This is primarily useful for
  non-connection-oriented transports. It also allows offloading the server's
  state to the client. The cookie extension is enabled by default as it is a
  mandatory extension in RFC8446.

- **`{early_data, EarlyData}`** - Option for accepting or rejecting Early Data

  Configures if the server accepts (`enabled`) or rejects (`disabled`) early data
  sent by a client. The default value is `disabled`.

# `active_msgs`

```elixir
-type active_msgs() ::
          {ssl, sslsocket(), Data :: binary() | list()} |
          {ssl_closed, sslsocket()} |
          {ssl_error, sslsocket(), Alert :: error_alert() | (Reason :: any())} |
          {ssl_passive, sslsocket()}.
```

The type for the messages that are delivered to the owner of a
TLS/DTLS socket in active mode.

The `ssl_error` reason may convey a TLS protocol alert if such an event occurs
after the connection has been established. The most common case when this will
happen is on the client side when a TLS-1.3 server requests a client certificate
and the provided certificate is not accepted by the server, as it will be
verified after the server has sent its last handshake message.

The `ssl_passive` message is sent only when the socket is in `{active, N}` mode
and the counter has dropped to 0. It indicates that the socket has transitioned
to passive (`{active, false}`) mode.

# `dtls_legacy_version`
*not exported* 

```elixir
-type dtls_legacy_version() :: dtlsv1.
```

A DTLS protocol version that are no longer supported by default for security reasons.

# `dtls_version`
*not exported* 

```elixir
-type dtls_version() :: 'dtlsv1.2' | dtls_legacy_version().
```

DTLS protocol version.

# `error_alert`

```elixir
-type error_alert() :: {tls_alert, {tls_alert(), Description :: string()}}.
```

If a TLS connection fails a TLS protocol ALERT will be sent/received.

An atom reflecting the raised alert, according to the TLS protocol, and a description string
with some further details will be returned.

# `host`

```elixir
-type host() :: inet:hostname() | inet:ip_address().
```

A name or address to a host.

# `protocol_extensions`

```elixir
-type protocol_extensions() ::
          #{renegotiation_info => binary(),
            signature_algs => signature_algs(),
            alpn => binary(),
            srp => binary(),
            next_protocol => binary(),
            max_frag_enum => 1..4,
            ec_point_formats => [0..2],
            elliptic_curves => [public_key:oid()],
            sni => inet:hostname()}.
```

Client hello extensions.

# `protocol_version`

```elixir
-type protocol_version() :: tls_version() | dtls_version().
```

TLS or DTLS protocol version.

# `reason`

```elixir
-type reason() :: term().
```

Error reason for debug purposes.

Not to be matched.

# `session_id`

```elixir
-type session_id() :: binary().
```

Identifies a TLS session prior to TLS-1.3.

# `socket`

```elixir
-type socket() :: gen_tcp:socket() | socket:socket().
```

A socket that can be used to perform a so-called "START-TLS", which
means using an already connected socket previously used for plain TCP
traffic and upgrading it to use TLS.

Both sides needs to agree on the upgrade.

# `socket_option`

```elixir
-type socket_option() :: gen_tcp:connect_option() | gen_tcp:listen_option() | gen_udp:option().
```

Options for the transport socket.

The default socket options are
`[{mode, list}, {packet, 0}, {header, 0}, {active, true}]`.

For valid options, see `m:inet`, `m:gen_tcp`, and `m:gen_udp`
in Kernel. Note that stream-oriented options such as `packet` are
only relevant for TLS and not DTLS.

# `sslsocket`

```elixir
-type sslsocket() :: any().
```

An opaque reference to the TLS/DTLS connection.

Note that despite being opaque, matching `sslsocket()` instances is allowed.

# `tls_alert`

```elixir
-type tls_alert() ::
          close_notify | unexpected_message | bad_record_mac | record_overflow | handshake_failure |
          bad_certificate | unsupported_certificate | certificate_revoked | certificate_expired |
          certificate_unknown | illegal_parameter | unknown_ca | access_denied | decode_error |
          decrypt_error | export_restriction | protocol_version | insufficient_security |
          internal_error | inappropriate_fallback | user_canceled | no_renegotiation |
          unsupported_extension | certificate_unobtainable | unrecognized_name |
          bad_certificate_status_response | bad_certificate_hash_value | unknown_psk_identity |
          no_application_protocol.
```

TLS Alert Protocol reasons.

# `tls_client_option`

```elixir
-type tls_client_option() :: client_option() | common_option() | socket_option() | transport_option().
```

An option that can be supplied to a TLS client.

# `tls_legacy_version`
*not exported* 

```elixir
-type tls_legacy_version() :: tlsv1 | 'tlsv1.1'.
```

A TLS protocol version that are no longer supported by default for security reasons.

# `tls_option`

```elixir
-type tls_option() :: tls_client_option() | tls_server_option().
```

An option related to the TLS/DTLS protocol.

# `tls_server_option`

```elixir
-type tls_server_option() :: server_option() | common_option() | socket_option() | transport_option().
```

An option that can be supplied to a TLS server.

# `tls_version`
*not exported* 

```elixir
-type tls_version() :: 'tlsv1.2' | 'tlsv1.3' | tls_legacy_version().
```

TLS protocol version.

# `transport_option`
*not exported* 

```elixir
-type transport_option() ::
          {cb_info,
           {CallbackModule :: atom(), DataTag :: atom(), ClosedTag :: atom(), ErrTag :: atom()}} |
          {cb_info,
           {CallbackModule :: atom(),
            DataTag :: atom(),
            ClosedTag :: atom(),
            ErrTag :: atom(),
            PassiveTag :: atom()}}.
```

Transport option defines a callback module and message tags to handle the underlying transport socket.

Can be used to customize the transport layer. The tag
values should be the values used by the underlying transport in its active mode
messages.

Defaults to `{gen_tcp, tcp, tcp_closed, tcp_error, tcp_passive}` for TLS.

> #### Note {: .info }
For backward compatibility a tuple of size four will be converted to a
tuple of size five, where `PassiveTag` is the `DataTag` element with
`_passive` appended.

For TLS the callback module must implement a reliable transport
protocol, behave as `m:gen_tcp`, and have functions corresponding to
`inet:setopts/2`, `inet:getopts/2`, `inet:peername/1`, `inet:sockname/1`, and
`inet:port/1`. The callback `m:gen_tcp` is treated specially and calls `m:inet`
directly. For DTLS this feature is considered experimental.

# `keylog_info`

```elixir
-type keylog_info() :: #{items => [keylog_item()], client_random => binary()}.
```

# `keylog_item`
*not exported* 

```elixir
-type keylog_item() :: unicode:chardata().
```

# `connect`
*since OTP R14B* 

```elixir
-spec connect(TCPSocket, TLSOptions) -> {ok, sslsocket()} | {error, Reason}
                 when
                     TCPSocket :: socket(),
                     TLSOptions :: [tls_client_option()],
                     Reason :: closed | {options, any()} | error_alert() | reason().
```

# `connect`

```elixir
-spec connect(TCPSocketOrHost, TLSOptionsOrPort, TimeoutOrTLSOptions) ->
                 {ok, sslsocket()} | {ok, sslsocket(), Ext :: protocol_extensions()} | {error, Reason}
                 when
                     TCPSocketOrHost :: socket() | host(),
                     TLSOptionsOrPort :: [tls_client_option()] | inet:port_number(),
                     TimeoutOrTLSOptions :: [tls_client_option()] | timeout(),
                     Reason :: closed | timeout | {options, any()} | error_alert() | reason().
```

Opens a TLS/DTLS connection.

```erlang
connect(TCPSocket, TLSOptions, Timeout).
```

Upgrades a `gen_tcp` (or equivalent) connected socket to a TLS socket by
performing the client-side TLS handshake.

```erlang
connect(Host, Port, TLSOptions).
```

Opens a TLS/DTLS connection to `Host`, `Port`. This call is equivalent to:

```erlang
connect(Host, Port, TLSOptions, infinity).
```

# `connect`

```elixir
-spec connect(Host, Port, TLSOptions, Timeout) ->
                 {ok, sslsocket()} | {ok, sslsocket(), Ext :: protocol_extensions()} | {error, Reason}
                 when
                     Host :: host(),
                     Port :: inet:port_number(),
                     TLSOptions :: [tls_client_option()],
                     Timeout :: timeout(),
                     Reason :: closed | timeout | {options, any()} | error_alert() | reason().
```

Opens a TLS/DTLS connection to `Host`, `Port`.

When the `verify` option is set to `verify_peer`, the
`public_key:pkix_verify_hostname/2` check will be performed in addition to the usual
X.509-path validation checks. If the check fails, the error `{bad_cert,
hostname_check_failed}` will be propagated to the path validation fun,
where it is possible to do customized checks
by using the full possibilities of the `public_key:pkix_verify_hostname/3` API.
When the `server_name_indication` option is provided, its value (the DNS name)
will be used as `ReferenceID` to `public_key:pkix_verify_hostname/2`. When no
`server_name_indication` option is given, the `Host` argument will be used as
Server Name Indication extension. The `Host` argument will also be used for the
`public_key:pkix_verify_hostname/2` check. If the `Host` argument is an
[`inet:ip_address()`](`t:inet:ip_address/0`) the `ReferenceID` used for the
check will be `{ip, Host}`; otherwise `dns_id` will be assumed with a fallback to
`ip` if that fails.

> #### Note {: .info }
>
> According to good practices, certificates should not use IP addresses as
> "server names", especially outside a closed network.

If the `{handshake, hello}` option is used, the handshake is paused after
receiving the server hello message and the success response is
`{ok, SslSocket, Ext}` instead of `{ok, SslSocket}`. Thereafter the handshake is
continued or canceled by calling `handshake_continue/3` or `handshake_cancel/1`.

If the `active` option is set to `once`, `true`, or an integer value, the process
owning the SSL socket will receive messages of type
[`active_msgs()`](`t:active_msgs/0`).

# `close`

```elixir
-spec close(SslSocket) -> ok | {error, Reason} when SslSocket :: sslsocket(), Reason :: any().
```

Closes a TLS/DTLS connection.

# `close`
*since OTP 18.1* 

```elixir
-spec close(SslSocket, How) -> ok | {ok, port()} | {ok, port(), Data} | {error, Reason}
               when
                   SslSocket :: sslsocket(),
                   How :: timeout() | {NewController :: pid(), timeout()},
                   Data :: binary(),
                   Reason :: any().
```

Closes or downgrades a TLS connection.

In the latter case the transport connection will be handed over to the
`NewController` process after receiving the TLS close alert from the
peer. The returned transport socket will have the following options
set: `[{active, false}, {packet, 0}, {mode, binary}]`.

In case of downgrade, the close function might return some binary data that
should be treated by the user as the first bytes received on the downgraded
connection.

# `controlling_process`

```elixir
-spec controlling_process(SslSocket, NewController) -> ok | {error, Reason}
                             when SslSocket :: sslsocket(), NewController :: pid(), Reason :: any().
```

Assigns a new controlling process to the SSL socket.

A controlling process is the owner of an SSL socket and receives all
messages from the socket.

# `handshake_cancel`
*since OTP 21.0* 

```elixir
-spec handshake_cancel(#sslsocket{socket_handle :: term(),
                                  connection_handler :: term(),
                                  payload_sender :: term(),
                                  transport_cb :: term(),
                                  connection_cb :: term(),
                                  tab :: term(),
                                  listener_config :: term()}) ->
                          any().
```

Cancel the handshake with a fatal `USER_CANCELED` alert.

# `handshake_continue`
*since OTP 21.0* 

```elixir
-spec handshake_continue(HsSocket, Options) -> {ok, SslSocket} | {error, Reason}
                            when
                                HsSocket :: sslsocket(),
                                Options :: [client_option() | server_option() | common_option()],
                                SslSocket :: sslsocket(),
                                Reason :: closed | timeout | error_alert().
```

# `handshake_continue`
*since OTP 21.0* 

```elixir
-spec handshake_continue(HsSocket, Options, Timeout) -> {ok, SslSocket} | {error, Reason}
                            when
                                HsSocket :: sslsocket(),
                                Options :: [client_option() | server_option() | common_option()],
                                Timeout :: timeout(),
                                SslSocket :: sslsocket(),
                                Reason :: closed | timeout | error_alert().
```

Continue the TLS handshake, possibly with new, additional, or changed options.

# `recv`

```elixir
-spec recv(SslSocket, Length) -> {ok, Data} | {error, reason()}
              when
                  SslSocket :: sslsocket(),
                  Length :: non_neg_integer(),
                  Data :: binary() | list() | HttpPacket,
                  HttpPacket :: any().
```

# `recv`

```elixir
-spec recv(SslSocket, Length, Timeout) -> {ok, Data} | {error, reason()}
              when
                  SslSocket :: sslsocket(),
                  Length :: non_neg_integer(),
                  Data :: binary() | list() | HttpPacket,
                  Timeout :: timeout(),
                  HttpPacket :: any().
```

Receives a packet from a socket in passive mode.

A closed socket is indicated by return value `{error, closed}`.
Argument `Length` is meaningful only when the socket is in mode `raw`
and denotes the number of bytes to read. If `Length` is zero, all
available bytes are returned. If `Length` is greater than zero,
exactly `Length` bytes are returned, or an error; possibly discarding
less than `Length` bytes of data when the socket gets closed from
the other side.

Optional argument `Timeout` specifies a time-out in milliseconds. The default
value is `infinity`.

# `send`

```elixir
-spec send(SslSocket, Data) -> ok | {error, reason()} when SslSocket :: sslsocket(), Data :: iodata().
```

Writes `Data` to `SslSocket`.

A notable return value is `{error, closed}` indicating that the socket is
closed.

# `setopts`

```elixir
-spec setopts(SslSocket, Options) -> ok | {error, reason()}
                 when SslSocket :: sslsocket(), Options :: [gen_tcp:option()].
```

Sets options according to `Options` for socket `SslSocket`.

> #### Note {: .info }
Note that setting low level transport protocol specific options
may or may not work in a TLS-environment. And even if they work it
may make your code platform specific, or stop working in later
version of this application. Such options could make sense for
some special corner cases although it is not a general viable option.
Such trade-offs must be carefully considered by the user that need
to fully understand the consequences of setting such options.

# `shutdown`
*since OTP R14B* 

```elixir
-spec shutdown(SslSocket, How) -> ok | {error, reason()}
                  when SslSocket :: sslsocket(), How :: read | write | read_write.
```

Immediately closes a socket in one or two directions.

`How == write` means closing the socket for writing, but reading from
it is still possible.

To handle siutations where the peer has performed a shutdown on the
write side, option `{exit_on_close, false}` is useful.

# `prf`
*since OTP R15B01* 

> This function is deprecated. ssl:prf/5 is deprecated; Use export_key_materials/4 instead. Note that in OTP 28 the 'testing' way of calling this function will no longer be supported..

```elixir
-spec prf(SslSocket, Secret, Label, Seed, WantedLength) -> {ok, binary()} | {error, reason()}
             when
                 SslSocket :: sslsocket(),
                 Secret :: binary() | master_secret,
                 Label :: binary(),
                 Seed :: [binary() | prf_random()],
                 WantedLength :: non_neg_integer().
```

Uses the Pseudo-Random Function (PRF) of a TLS session to generate extra key
material.

It either takes user-generated values for `Secret` and `Seed` or atoms
directing it to use a specific value from the session security parameters.

> #### Note {: .info }

This function is replaced by `export_key_materials/4`, the officially
documented API function since OTP 27, which is equivalent to
[`prf(TLSSocket, master_secret, Label, [client_random, server_random,
Context], WantedLength)`](`prf/5`). Other ways of calling this
function were for testing purposes only and has no use case. When
called in a TLS-1.3 context it will now behave as
[`export_key_materials(TLSSocket, [Label], [Context],
[WantedLength])`](`export_key_materials/4`).

# `eccs`
*since OTP 19.2* 

```elixir
-spec eccs() -> NamedCurves when NamedCurves :: [named_curve()].
```

Returns a list of all supported elliptic curves, including legacy
curves, for all TLS/DTLS versions prior to TLS-1.3.

# `eccs`
*since OTP 19.2* 

```elixir
-spec eccs(Version) -> NamedCurves
              when
                  Version :: 'tlsv1.2' | 'tlsv1.1' | tlsv1 | 'dtlsv1.2' | dtlsv1,
                  NamedCurves :: [named_curve()].
```

Returns the elliptic curves supported by default for `Version`.

This is a subset of what `eccs/0` returns.

# `renegotiate`
*since OTP R14B* 

```elixir
-spec renegotiate(SslSocket) -> ok | {error, reason()} when SslSocket :: sslsocket().
```

Initiates a new handshake.

A notable return value is `{error, renegotiation_rejected}` indicating
that the peer refused to go through with the renegotiation, but the
connection is still active using the previously negotiated session.

TLS-1.3 has removed the renegotiation feature from earlier TLS
versions and instead adds a new feature called key update, which
replaces the most important part of renegotiation: the refreshing of
session keys. This is triggered automatically after reaching a
plaintext limit and can be configured using the `key_update_at` option
in `t:common_option_tls13/0`.

# `handshake`
*since OTP 21.0* 

```elixir
-spec handshake(HsSocket) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason}
                   when
                       HsSocket :: sslsocket(),
                       SslSocket :: sslsocket(),
                       Ext :: protocol_extensions(),
                       Reason :: closed | timeout | error_alert().
```

# `handshake`
*since OTP 21.0* 

```elixir
-spec handshake(HsSocket, OptionsOrTimeout) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason}
                   when
                       HsSocket :: sslsocket(),
                       OptionsOrTimeout :: timeout() | [server_option()],
                       SslSocket :: sslsocket(),
                       Ext :: protocol_extensions(),
                       Reason :: closed | timeout | error_alert().
```

Performs the TLS/DTLS server-side handshake.

If the second argument is a timeout value:

```erlang
handshake(HsSocket, Timeout).
```

this call is equivalent to:

```erlang
handshake(HsSocket, [], Timeout).
```

Otherwise, if the second argument is a list of options:

```erlang
handshake(HsSocket, Options).
```
this call is equivalent to:

```erlang
handshake(HsSocket, Options, infinity).
```

# `handshake`
*since OTP 21.0* 

```elixir
-spec handshake(Socket, Options, Timeout) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason}
                   when
                       Socket :: socket() | sslsocket(),
                       SslSocket :: sslsocket(),
                       Options :: [server_option() | common_option()],
                       Timeout :: timeout(),
                       Ext :: protocol_extensions(),
                       Reason :: closed | timeout | {options, any()} | error_alert().
```

Performs the TLS/DTLS server-side handshake.

Returns a new TLS/DTLS socket if the handshake is successful.

If `Socket` is a ordinary [`socket()`](`t:socket/0`), upgrades a
`gen_tcp` or equivalent socket to an SSL socket by performing the
TLS server-side handshake and returning a TLS socket.

> #### Note {: .info }
The ordinary `Socket` must be in passive mode (`{active, false}`)
before calling this function and before the client tries to connect
with TLS; otherwise, the behavior of this function is undefined. The
best way to ensure this is to create the ordinary listen socket in
passive mode.

If `Socket` is an [`sslsocket()`](`t:sslsocket/0`), provides extra
TLS/DTLS options to those specified in `listen/2` and then performs
the TLS/DTLS handshake. Returns a new TLS/DTLS socket if the handshake
is successful.

> #### Warning {: .warning }
Not setting the timeout makes the server more vulnerable to Denial of
Service (DoS) attacks.

If option `{handshake, hello}` is specified the handshake is paused after
receiving the client hello message and the success response is
`{ok, SslSocket, Ext}` instead of `{ok, SslSocket}`. Thereafter the handshake is
continued or canceled by calling `handshake_continue/3` or `handshake_cancel/1`.

If option `active` is set to `once`, `true`, or an integer value, the process
owning the [`sslsocket()`](`t:sslsocket/0`) will receive messages of type
[`active_msgs()`](`t:active_msgs/0`).

# `listen`

```elixir
-spec listen(Port, Options) -> {ok, ListenSocket} | {error, Reason}
                when
                    Port :: inet:port_number(),
                    Options :: [tls_server_option()],
                    ListenSocket :: sslsocket(),
                    Reason :: {options, any()} | reason().
```

Creates an SSL listen socket.

# `transport_accept`

```elixir
-spec transport_accept(ListenSocket) -> {ok, SslSocket} | {error, Reason}
                          when ListenSocket :: sslsocket(), SslSocket :: sslsocket(), Reason :: reason().
```

# `transport_accept`

```elixir
-spec transport_accept(ListenSocket, Timeout) -> {ok, SslSocket} | {error, Reason}
                          when
                              ListenSocket :: sslsocket(),
                              Timeout :: timeout(),
                              SslSocket :: sslsocket(),
                              Reason :: reason().
```

Accepts an incoming connection request on a listen socket.

`ListenSocket` must be a socket returned from `listen/2`. The socket
returned is to be passed to [`handshake/1,2,3`](`handshake/3`) to
complete the handshake and establish the TLS/DTLS connection.

> #### Warning {: .warning }
>
> Most API functions require that the TLS/DTLS connection is established to work
> as expected.

The accepted socket inherits the options set for `ListenSocket` in `listen/2`.

The default value for `Timeout` is `infinity`. If `Timeout` is specified and no
connection is accepted within the given time, `{error, timeout}` is returned.

# `groups`
*since OTP 27.0* 

```elixir
-spec groups() -> [group()].
```

Returns all supported groups in TLS 1.3.

Existed since OTP 22.0; documented as of OTP 27.

# `groups`
*since OTP 27.0* 

```elixir
-spec groups(Description) -> [group()] when Description :: default.
```

Returns default supported groups in TLS 1.3.

Existed since OTP 22.0; documented as of OTP 27.

# `update_keys`
*since OTP 22.3* 

```elixir
-spec update_keys(SslSocket, Type) -> ok | {error, reason()}
                     when SslSocket :: sslsocket(), Type :: write | read_write.
```

Create new session keys.

There are cryptographic limits on the amount of plaintext which can be safely
encrypted under a given set of keys. If the amount of data surpasses those
limits, a key update is triggered and a new set of keys are installed. See also
the `key_update_at` option in `t:common_option_tls13/0`.

This function can be used to explicitly start a key update on a TLS-1.3
connection. There are two types of key updates: if `Type` is `write`,
only the writing key is updated; if `Type` is `read_write`, both the
reading and writing keys are updated.

# `append_cipher_suites`
*since OTP 20.3* 

```elixir
-spec append_cipher_suites(Deferred, Suites) -> ciphers()
                              when Deferred :: ciphers() | cipher_filters(), Suites :: ciphers().
```

Make `Deferred` suites become the least preferred suites.

The `Deferred` suites will be put at the end of the cipher suite list
`Suites` after removing them from `Suites` if present. `Deferred` can
be a list of cipher suites or a list of filters in which case the
filters are used on `Suites` to extract the deferred cipher list.

# `cipher_suites`
*since OTP 20.3* 

```elixir
-spec cipher_suites(Description, Version) -> ciphers()
                       when
                           Description :: default | all | exclusive | anonymous | exclusive_anonymous,
                           Version :: protocol_version().
```

Lists all available cipher suites corresponding to `Description`.

The `exclusive` and `exclusive_anonymous` option will exclusively
list cipher suites first supported in `Version`, whereas the other options are
inclusive from the lowest possible version to `Version`. The `all` option
includes all suites except anonymous suites. No anonymous suites are supported
by default.

> #### Note {: .info }
>
> TLS-1.3 has no overlapping cipher suites with previous TLS versions, meaning that
> the result of [`cipher_suites(all, 'tlsv1.3')`](`cipher_suites/2`) contains a separate
> set of suites that can be used with TLS-1.3 and another set that can be used if a lower
> version is negotiated. The so-called `PSK` and `SRP` suites (prior to TLS-1.3)
> need extra configuration to work; namely the option `user_lookup_function`. No
> anonymous suites are supported by TLS-1.3.
>
> Also note that the cipher suites returned by this function are the cipher
> suites that the OTP SSL application can support provided that they are
> supported by the crypto library linked with the OTP Crypto application. Use
> [`ssl:filter_cipher_suites(Suites, [])`](`filter_cipher_suites/2`) to filter
> the list for the current crypto library. Note that cipher suites may be filtered
> out because they are too old or too new depending on the crypto library.

# `cipher_suites`
*since OTP 22.0* 

```elixir
-spec cipher_suites(Description, Version, StringType) -> [string()]
                       when
                           Description :: default | all | exclusive | anonymous,
                           Version :: protocol_version(),
                           StringType :: rfc | openssl.
```

Equivalent to `cipher_suites/2`, but lists RFC or OpenSSL string names instead of
[`erl_cipher_suite()`](`t:erl_cipher_suite/0`).

# `clear_pem_cache`
*since OTP 17.5* 

```elixir
-spec clear_pem_cache() -> ok.
```

Clears the PEM cache.

PEM files, used by SSL API-functions, are cached for performance
reasons. The cache is automatically checked at regular intervals to
determine whether any cache entries should be invalidated.

This function provides a way to unconditionally clear the entire cache, thereby
forcing a reload of previously cached PEM files.

# `connection_information`
*since OTP 18.0* 

```elixir
-spec connection_information(SslSocket) -> {ok, Result} | {error, reason()}
                                when SslSocket :: sslsocket(), Result :: connection_info().
```

Returns the most relevant information about the connection.

Some items that are undefined will be filtered out. No values
that affect the security of the connection will be returned.

> #### Note {: .info }
>
> The legacy `cipher_suite` item was removed in OTP 23. Previously it returned
> the cipher suite in its (undocumented) legacy format. It is replaced by
> `selected_cipher_suite`.

# `connection_information`
*since OTP 18.0* 

```elixir
-spec connection_information(SslSocket, Items) -> {ok, Result} | {error, reason()}
                                when
                                    SslSocket :: sslsocket(),
                                    Items :: connection_info_keys(),
                                    Result :: connection_info().
```

Returns the requested information items about the connection if they are
defined.

Note that the values for `client_random`, `server_random`, `master_secret`, and `keylog`
affect the security of connection.

In order to retrieve `keylog` information from a TLS
connection, the `keep_secrets` option must be configured in advance.

> #### Note {: .info }
>
> If only undefined options are requested the resulting list can be empty.

# `export_key_materials`
*since OTP 27.0* 

```elixir
-spec export_key_materials(SslSocket, Labels, Contexts, WantedLengths) ->
                              {ok, ExportKeyMaterials} | {error, reason()}
                              when
                                  SslSocket :: sslsocket(),
                                  Labels :: [binary()],
                                  Contexts :: [binary() | no_context],
                                  WantedLengths :: [non_neg_integer()],
                                  ExportKeyMaterials :: [binary()].
```

# `export_key_materials`
*since OTP 27.0* 

```elixir
-spec export_key_materials(SslSocket, Labels, Contexts, WantedLengths, ConsumeSecret) ->
                              {ok, ExportKeyMaterials} |
                              {error, exporter_master_secret_already_consumed | bad_input}
                              when
                                  SslSocket :: sslsocket(),
                                  Labels :: [binary()],
                                  Contexts :: [binary() | no_context],
                                  WantedLengths :: [non_neg_integer()],
                                  ConsumeSecret :: boolean(),
                                  ExportKeyMaterials :: [binary()].
```

Uses a Pseudo-Random Function (PRF prior to TLS-1.3) or a Key
Derivation Function (HKDF in TLS-1.3) for a TLS connection to
generate and export keying materials.

In TLS-1.3, using `no_context` is equivalent to specifying an empty
context (an empty binary). Prior to TLS-1.3, `no_context` and an empty
context will produce different results.

The `ConsumeSecret` argument is relevant only in TLS-1.3, causing the
TLS-1.3 `exporter_master_secret` to be consumed, thereby making it
unavailable and increasing security. Further attempts to call this
function will fail.

# `filter_cipher_suites`
*since OTP 20.3* 

```elixir
-spec filter_cipher_suites(Suites, Filters) -> Ciphers
                              when
                                  Suites :: ciphers(), Filters :: cipher_filters(), Ciphers :: ciphers().
```

Removes cipher suites if any of the filter functions returns `false` for any part
of the cipher suite.

If no filter function is supplied for some part, the default behavior
treats it as a filter function that returns `true`. For
examples, see [Customizing cipher suites
](using_ssl.md#customizing-cipher-suites). Additionally, this function
also filters the cipher suites to exclude cipher suites not supported
by the crypto library used by the OTP Crypto application, meaning that
[`ssl:filter_cipher_suites(Suites, [])`](`filter_cipher_suites/2`)
is equivalent to applying only the filters for crypto library support.

# `format_error`

```elixir
-spec format_error(Error) -> ReasonStr when Error :: {error, reason()} | reason(), ReasonStr :: string().
```

Presents the error returned by an SSL function as a printable string.

# `getopts`

```elixir
-spec getopts(SslSocket, OptionNames) -> {ok, [gen_tcp:option()]} | {error, reason()}
                 when SslSocket :: sslsocket(), OptionNames :: [gen_tcp:option_name()].
```

Gets the values of the specified socket options.

# `getstat`
*since OTP 19.0* 

```elixir
-spec getstat(SslSocket) -> {ok, OptionValues} | {error, inet:posix()}
                 when SslSocket :: sslsocket(), OptionValues :: [{inet:stat_option(), integer()}].
```

Get statistics for the underlying socket.

# `getstat`
*since OTP 19.0* 

```elixir
-spec getstat(SslSocket, Options) -> {ok, OptionValues} | {error, inet:posix()}
                 when
                     SslSocket :: sslsocket(),
                     Options :: [inet:stat_option()],
                     OptionValues :: [{inet:stat_option(), integer()}].
```

Get one or more statistic values for the underlying socket.

See `inet:getstat/2` for further details.

# `negotiated_protocol`
*since OTP 18.0* 

```elixir
-spec negotiated_protocol(SslSocket) -> {ok, Protocol} | {error, Reason}
                             when
                                 SslSocket :: sslsocket(),
                                 Protocol :: binary(),
                                 Reason :: protocol_not_negotiated | closed.
```

Returns the protocol negotiated through ALPN or NPN extensions.

# `peercert`

```elixir
-spec peercert(SslSocket) -> {ok, Cert} | {error, reason()}
                  when SslSocket :: sslsocket(), Cert :: public_key:der_encoded().
```

The peer certificate is returned as a DER-encoded binary.

The certificate can be
decoded with `public_key:pkix_decode_cert/2`. Suggested further reading about
certificates is [Public_Key User's Guide](`e:public_key:public_key_records.md`)
and [SSL User's Guide](standards_compliance.md).

# `peername`

```elixir
-spec peername(SslSocket) -> {ok, {Address, Port}} | {error, reason()}
                  when
                      SslSocket :: sslsocket(), Address :: inet:ip_address(), Port :: inet:port_number().
```

Returns the address and port number of the peer.

# `prepend_cipher_suites`
*since OTP 20.3* 

```elixir
-spec prepend_cipher_suites(Preferred, Suites) -> ciphers()
                               when Preferred :: ciphers() | cipher_filters(), Suites :: ciphers().
```

Make `Preferred` suites become the most preferred suites.

The `Preferred` suites will be put at the head of the cipher suite
list `Suites` after removing them from `Suites` if
present. `Preferred` can be a list of cipher suites or a list of
filters in which case the filters are used on `Suites` to extract the
preferred cipher list.

# `signature_algs`
*since OTP 26.0* 

```elixir
-spec signature_algs(Description, Version) -> signature_algs()
                        when Description :: default | all | exclusive, Version :: protocol_version().
```

Lists all available signature algorithms corresponding to `Description`.

The `exclusive` option will exclusively list algorithms or algorithm schemes for
that protocol version, whereas the `default` and `all` options lists the
combined list to support the range of protocols from (D)TLS-1.2, the first
version to support configuration of the signature algorithms, to `Version`.

Example:

```erlang
1> ssl:signature_algs(default, 'tlsv1.3').
[eddsa_ed25519,eddsa_ed448,ecdsa_secp521r1_sha512,
 ecdsa_secp384r1_sha384,ecdsa_secp256r1_sha256,
 ecdsa_brainpoolP512r1tls13_sha512,
 ecdsa_brainpoolP384r1tls13_sha384,
 ecdsa_brainpoolP256r1tls13_sha256,rsa_pss_pss_sha512,
 rsa_pss_pss_sha384,rsa_pss_pss_sha256,rsa_pss_rsae_sha512,
 rsa_pss_rsae_sha384,rsa_pss_rsae_sha256,mldsa44,mldsa65,
 mldsa87,rsa_pkcs1_sha512,rsa_pkcs1_sha384,rsa_pkcs1_sha256,
 {sha512,ecdsa},
 {sha384,ecdsa},
 {sha256,ecdsa}]

2> ssl:signature_algs(all, 'tlsv1.3').
[eddsa_ed25519,eddsa_ed448,ecdsa_secp521r1_sha512,ecdsa_secp384r1_sha384,
 ecdsa_secp256r1_sha256,ecdsa_brainpoolP512r1tls13_sha512,
 ecdsa_brainpoolP384r1tls13_sha384,ecdsa_brainpoolP256r1tls13_sha256,
 rsa_pss_pss_sha512,rsa_pss_pss_sha384,rsa_pss_pss_sha256,rsa_pss_rsae_sha512,
 rsa_pss_rsae_sha384,rsa_pss_rsae_sha256,mldsa44,mldsa65,mldsa87,
 rsa_pkcs1_sha512,rsa_pkcs1_sha384,rsa_pkcs1_sha256,
 {sha512,ecdsa},
 {sha384,ecdsa},
 {sha256,ecdsa},
 slh_dsa_shake_256f,slh_dsa_shake_256s,slh_dsa_sha2_256f,slh_dsa_sha2_256s,
 slh_dsa_shake_192f,slh_dsa_shake_192s,slh_dsa_sha2_192f,slh_dsa_sha2_192s,
 slh_dsa_shake_128f,slh_dsa_shake_128s,slh_dsa_sha2_128f,slh_dsa_sha2_128s,
 ecdsa_sha1,rsa_pkcs1_sha1,
 {sha224,ecdsa},
 {sha224,rsa},
 {sha,dsa}]

3> [ssl:signature_algs(exclusive, 'tlsv1.3').
[eddsa_ed25519,eddsa_ed448,ecdsa_secp521r1_sha512,ecdsa_secp384r1_sha384,
 ecdsa_secp256r1_sha256,ecdsa_brainpoolP512r1tls13_sha512,
 ecdsa_brainpoolP384r1tls13_sha384,ecdsa_brainpoolP256r1tls13_sha256,
 rsa_pss_pss_sha512,rsa_pss_pss_sha384,rsa_pss_pss_sha256,rsa_pss_rsae_sha512,
 rsa_pss_rsae_sha384,rsa_pss_rsae_sha256,mldsa44,mldsa65,mldsa87,
 rsa_pkcs1_sha512,rsa_pkcs1_sha384,rsa_pkcs1_sha256,slh_dsa_shake_256f,
 slh_dsa_shake_256s,slh_dsa_sha2_256f,slh_dsa_sha2_256s,slh_dsa_shake_192f,
 slh_dsa_shake_192s,slh_dsa_sha2_192f,slh_dsa_sha2_192s,slh_dsa_shake_128f,
 slh_dsa_shake_128s,slh_dsa_sha2_128f,slh_dsa_sha2_128s]
```

> #### Note {: .info }
>
> Some TLS-1-3 scheme names overlap with TLS-1.2 algorithm-tuple-pair-names and
> then TLS-1.3 names will be used, for example `rsa_pkcs1_sha256` instead of
> `{sha256, rsa}`. These are legacy algorithms in TLS-1.3 that apply only to
> certificate signatures in this version of the protocol.

# `sockname`

```elixir
-spec sockname(SslSocket) -> {ok, {Address, Port}} | {error, reason()}
                  when
                      SslSocket :: sslsocket(), Address :: inet:ip_address(), Port :: inet:port_number().
```

Returns the local address and port number of socket `SslSocket`.

# `start`
*since OTP R14B* 

```elixir
-spec start() -> ok | {error, reason()}.
```

# `start`
*since OTP R14B* 

```elixir
-spec start(permanent | transient | temporary) -> ok | {error, reason()}.
```

Starts the SSL application.

# `stop`
*since OTP R14B* 

```elixir
-spec stop() -> ok.
```

Stops the SSL application.

# `str_to_suite`
*since OTP 22.0* 

```elixir
-spec str_to_suite(CipherSuiteName) -> erl_cipher_suite() | {error, {not_recognized, CipherSuiteName}}
                      when CipherSuiteName :: string().
```

Converts an RFC or OpenSSL name string to an `t:erl_cipher_suite/0`

Returns an error if the cipher suite is not supported or the name is
not a valid cipher suite name.

# `suite_to_openssl_str`
*since OTP 22.0* 

```elixir
-spec suite_to_openssl_str(CipherSuite) -> string() when CipherSuite :: erl_cipher_suite().
```

Converts an [`erl_cipher_suite()`](`t:erl_cipher_suite/0`) value to
an OpenSSL name string.

PRE TLS-1.3 these names differ for RFC names

# `suite_to_str`
*since OTP 21.0* 

```elixir
-spec suite_to_str(CipherSuite) -> string() when CipherSuite :: erl_cipher_suite().
```

Converts an [`erl_cipher_suite()`](`t:erl_cipher_suite/0`) value to an RFC
name string.

# `versions`
*since OTP R14B* 

```elixir
-spec versions() -> [VersionInfo]
                  when
                      VersionInfo ::
                          {ssl_app, string()} |
                          {supported | available | implemented, [tls_version()]} |
                          {supported_dtls | available_dtls | implemented_dtls, [dtls_version()]}.
```

Lists information, mainly concerning TLS/DTLS versions, in runtime for debugging
and testing purposes.

- **`app_vsn`** - The application version of the SSL application.

- **`supported`** - TLS versions supported with current application environment
  and crypto library configuration. Overridden by a version option on
  [`connect/2,3,4`](`connect/2`), `listen/2`, and
  [`handshake/2,3`](`handshake/2`). For the negotiated TLS version, see
  [`connection_information/1`](`connection_information/1`).

- **`supported_dtls`** - DTLS versions supported with current application
  environment and crypto library configuration. Overridden by a version option
  on [`connect/2,3,4`](`connect/2`), `listen/2`, and
  [`handshake/2,3`](`handshake/2`). For the negotiated DTLS version, see
  [`connection_information/1`](`connection_information/1`).

- **`available`** - All TLS versions supported with the linked crypto library.

- **`available_dtls`** - All DTLS versions supported with the linked crypto
  library.

- **`implemented`** - All TLS versions supported by the SSL application if
  linked with a crypto library with the necessary support.

- **`implemented_dtls`** - All DTLS versions supported by the SSL application if
  linked with a crypto library with the necessary support.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
