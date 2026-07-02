# `crypto`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/crypto/src/crypto.erl#L25)

Crypto Functions

This module provides a set of cryptographic functions.

- **Hash functions** -

  - **SHA1, SHA2** - [Secure Hash Standard (FIPS PUB180-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)
  - **SHA3** - [SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions (FIPS PUB 202)](https://www.nist.gov/publications/sha-3-standard-permutation-based-hash-and-extendable-output-functions?pub_id=919061)

  - **BLAKE2** - [BLAKE2 — fast secure hashing](https://blake2.net/)

  - **SM3** - [The SM3 Hash Function (GM/T 0004-2012)](https://datatracker.ietf.org/doc/html/draft-sca-cfrg-sm3-02)

  - **MD5** - [The MD5 Message Digest Algorithm (RFC 1321)](http://www.ietf.org/rfc/rfc1321.txt)

  - **MD4** - [The MD4 Message Digest Algorithm (RFC 1320)](http://www.ietf.org/rfc/rfc1320.txt)

- **MACs - Message Authentication Codes** -

  - **Hmac functions** - [Keyed-Hashing for Message Authentication (RFC 2104)](http://www.ietf.org/rfc/rfc2104.txt)

  - **Cmac functions** - [The AES-CMAC Algorithm (RFC 4493)](http://www.ietf.org/rfc/rfc4493.txt)

  - **POLY1305** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

- **Symmetric Ciphers** - 

  - **DES, 3DES and AES** - [Block Cipher Techniques (NIST)](https://csrc.nist.gov/projects/block-cipher-techniques)

  - **Blowfish** -
    [Fast Software Encryption, Cambridge Security Workshop Proceedings (December 1993), Springer-Verlag, 1994, pp. 191-204.](https://www.schneier.com/academic/archives/1994/09/description_of_a_new.html)

  - **Chacha20** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

  - **Chacha20_poly1305** - [ChaCha20 and Poly1305 for IETF Protocols (RFC 7539)](http://www.ietf.org/rfc/rfc7539.txt)

  - **SM4** - [The SM4 Block Cipher Algorithm](https://www.iso.org/standard/81564.html)

- **Modes** -
  - **ECB, CBC, CFB, OFB and CTR** - [Recommendation for Block Cipher Modes of
    Operation: Methods and Techniques (NIST SP 800-38A)](https://csrc.nist.gov/publications/detail/sp/800-38a/final)

  - **GCM** - [Recommendation for Block Cipher Modes of Operation:
    Galois/Counter Mode (GCM) and GMAC (NIST SP 800-38D)](https://csrc.nist.gov/publications/detail/sp/800-38d/final)

  - **CCM** - [Recommendation for Block Cipher Modes of Operation: The CCM Mode
    for Authentication and Confidentiality (NIST SP 800-38C)](https://nvlpubs.nist.gov/nistpubs/legacy/sp/nistspecialpublication800-38c.pdf)

- **Asymmetric Ciphers - Public Key Techniques** -

  - **RSA** - [PKCS #1: RSA Cryptography Specifications (RFC 3447)](http://www.ietf.org/rfc/rfc3447.txt)

  - **DSS** - [Digital Signature Standard (DSS) (FIPS 186-4)](https://csrc.nist.gov/publications/detail/fips/186/4/final)

  - **ECDSA** - [Elliptic Curve Digital Signature Algorithm (ECDSA)](http://csrc.nist.gov/groups/STM/cavp/documents/dss2/ecdsa2vs.pdf)

  - **SRP** - [The SRP Authentication and Key Exchange System (RFC 2945)](http://www.ietf.org/rfc/rfc2945.txt)

> #### Note {: .info }
>
> The actual supported algorithms and features depends on their availability in
> the actual libcrypto used. See the [crypto (App)](crypto_app.md) about
> dependencies.
>
> Enabling FIPS mode will also disable algorithms and features.

The [CRYPTO User's Guide](index.html) has more information on FIPS, Engines and
Algorithm Details like key lengths.

## Exceptions

[](){: #error_old }

### Atoms - the older style

The exception `error:badarg` signifies that one or more arguments are of wrong
data type, or are otherwise badly formed.

The exception `error:notsup` signifies that the algorithm is known but is not
supported by current underlying libcrypto or explicitly disabled when building
that.

For a list of supported algorithms, see [supports(ciphers)](`supports/1`).

[](){: #error_3tup }

### 3-tuples - the new style

The exception is:

```text
error:{Tag, C_FileInfo, Description}

Tag = badarg | notsup | error
C_FileInfo = term()    % Usually only useful for the OTP maintainer
Description = string() % Clear text, sometimes only useful for the OTP maintainer
```

The exception tags are:

- **`badarg`** - Signifies that one or more arguments are of wrong data type or
  are otherwise badly formed.

- **`notsup`** - Signifies that the algorithm is known but is not supported by
  current underlying libcrypto or explicitly disabled when building that one.

- **`error`** - An error condition that should not occur, for example a memory
  allocation failed or the underlying cryptolib returned an error code, for
  example `"Can't initialize context, step 1"`. Those text usually needs
  searching the C-code to be understood.

Usually there are more information in the call stack about which argument caused
the exception and what the values where.

To catch the exception, use for example:

```text
try crypto:crypto_init(Ciph, Key, IV, true)
    catch
        error:{Tag, _C_FileInfo, Description} ->
            do_something(......)
         .....
end
```

# `cipher`
*not exported* 

```erlang
-type cipher() :: cipher_no_iv() | cipher_iv() | cipher_aead().
```

# `cipher_aead`
*not exported* 

```erlang
-type cipher_aead() ::
          aes_128_ccm | aes_192_ccm | aes_256_ccm | aes_ccm | aes_128_gcm | aes_192_gcm | aes_256_gcm |
          aes_gcm | sm4_gcm | sm4_ccm | chacha20_poly1305.
```

Ciphers known by the CRYPTO application.

Note that this list might be reduced if the underlying libcrypto does not
support all of them.

# `cipher_iv`
*not exported* 

```erlang
-type cipher_iv() ::
          aes_128_cbc | aes_192_cbc | aes_256_cbc | aes_cbc | aes_128_ofb | aes_192_ofb | aes_256_ofb |
          aes_128_cfb128 | aes_192_cfb128 | aes_256_cfb128 | aes_cfb128 | aes_128_cfb8 | aes_192_cfb8 |
          aes_256_cfb8 | aes_cfb8 | aes_128_ctr | aes_192_ctr | aes_256_ctr | aes_ctr | sm4_cbc |
          sm4_ofb | sm4_cfb | sm4_ctr | blowfish_cbc | blowfish_cfb64 | blowfish_ofb64 | chacha20 |
          des_ede3_cbc | des_ede3_cfb | des_cbc | des_cfb | rc2_cbc.
```

# `cipher_no_iv`
*not exported* 

```erlang
-type cipher_no_iv() ::
          aes_128_ecb | aes_192_ecb | aes_256_ecb | aes_ecb | blowfish_ecb | des_ecb | sm4_ecb | rc4.
```

# `crypto_opt`
*not exported* 

```erlang
-type crypto_opt() :: {encrypt, boolean()} | {padding, padding()}.
```

Selects encryption (`{encrypt,true}`) or decryption (`{encrypt,false}`).

# `crypto_opts`
*not exported* 

```erlang
-type crypto_opts() :: boolean() | [crypto_opt()].
```

# `cryptolib_padding`
*not exported* 

```erlang
-type cryptolib_padding() :: none | pkcs_padding.
```

The `cryptolib_padding` are paddings that may be present in the underlying
cryptolib linked to the Erlang/OTP crypto app.

For OpenSSL, see the [OpenSSL documentation](https://openssl.org). and find
`EVP_CIPHER_CTX_set_padding()` in cryptolib for your linked version.

# `otp_padding`
*not exported* 

```erlang
-type otp_padding() :: zero | random.
```

Erlang/OTP adds a either padding of zeroes or padding with random bytes.

# `padding`
*not exported* 

```erlang
-type padding() :: cryptolib_padding() | otp_padding().
```

This option handles padding in the last block. If not set, no padding is done
and any bytes in the last unfilled block is silently discarded.

# `dh_params`
*not exported* 

```erlang
-type dh_params() :: [key_integer()].
```

```text
dh_params() = [P, G] | [P, G, MaxPrivateKeyBitLength]
```

# `dh_private`

```erlang
-type dh_private() :: key_integer().
```

# `dh_public`

```erlang
-type dh_public() :: key_integer().
```

# `ecdh_params`
*not exported* 

```erlang
-type ecdh_params() :: ec_named_curve() | edwards_curve_dh() | ec_explicit_curve().
```

# `ecdh_private`
*not exported* 

```erlang
-type ecdh_private() :: key_integer().
```

# `ecdh_public`
*not exported* 

```erlang
-type ecdh_public() :: key_integer().
```

# `blake2`
*not exported* 

```erlang
-type blake2() :: blake2b | blake2s.
```

# `cmac_cipher_algorithm`

```erlang
-type cmac_cipher_algorithm() ::
          aes_128_cbc | aes_192_cbc | aes_256_cbc | aes_cbc | blowfish_cbc | des_cbc | des_ede3_cbc |
          rc2_cbc.
```

# `compatibility_only_hash`
*not exported* 

```erlang
-type compatibility_only_hash() :: md5 | md4.
```

The `t:compatibility_only_hash/0` algorithms are recommended only for
compatibility with existing applications.

# `dss_digest_type`

```erlang
-type dss_digest_type() :: sha1() | sha2().
```

# `ecdsa_digest_type`

```erlang
-type ecdsa_digest_type() :: sha1() | sha2().
```

# `hash_algorithm`
*not exported* 

```erlang
-type hash_algorithm() ::
          sha1() | sha2() | sha3() | sha3_xof() | blake2() | ripemd160 | sm3 | compatibility_only_hash().
```

# `hash_xof_algorithm`
*not exported* 

```erlang
-type hash_xof_algorithm() :: sha3_xof().
```

# `hmac_hash_algorithm`

```erlang
-type hmac_hash_algorithm() :: sha1() | sha2() | sha3() | sm3 | compatibility_only_hash().
```

# `rsa_digest_type`

```erlang
-type rsa_digest_type() :: sha1() | sha2() | md5 | ripemd160.
```

# `sha1`

```erlang
-type sha1() :: sha.
```

# `sha2`

```erlang
-type sha2() :: sha224 | sha256 | sha384 | sha512 | sha512_224 | sha512_256.
```

# `sha3`

```erlang
-type sha3() :: sha3_224 | sha3_256 | sha3_384 | sha3_512.
```

# `sha3_xof`
*not exported* 

```erlang
-type sha3_xof() :: shake128 | shake256.
```

# `ec_basis`
*not exported* 

```erlang
-type ec_basis() ::
          {tpbasis, K :: non_neg_integer()} |
          {ppbasis, K1 :: non_neg_integer(), K2 :: non_neg_integer(), K3 :: non_neg_integer()} |
          onbasis.
```

Curve definition details.

# `ec_characteristic_two_field`
*not exported* 

```erlang
-type ec_characteristic_two_field() :: {characteristic_two_field, M :: integer(), Basis :: ec_basis()}.
```

# `ec_curve`
*not exported* 

```erlang
-type ec_curve() :: {A :: binary(), B :: binary(), Seed :: none | binary()}.
```

Parametric curve definition.

# `ec_explicit_curve`
*not exported* 

```erlang
-type ec_explicit_curve() ::
          {Field :: ec_field(),
           Curve :: ec_curve(),
           BasePoint :: binary(),
           Order :: binary(),
           CoFactor :: none | binary()}.
```

# `ec_field`
*not exported* 

```erlang
-type ec_field() :: ec_prime_field() | ec_characteristic_two_field().
```

# `ec_named_curve`

```erlang
-type ec_named_curve() ::
          brainpoolP160r1 | brainpoolP160t1 | brainpoolP192r1 | brainpoolP192t1 | brainpoolP224r1 |
          brainpoolP224t1 | brainpoolP256r1 | brainpoolP256t1 | brainpoolP320r1 | brainpoolP320t1 |
          brainpoolP384r1 | brainpoolP384t1 | brainpoolP512r1 | brainpoolP512t1 | c2pnb163v1 |
          c2pnb163v2 | c2pnb163v3 | c2pnb176v1 | c2pnb208w1 | c2pnb272w1 | c2pnb304w1 | c2pnb368w1 |
          c2tnb191v1 | c2tnb191v2 | c2tnb191v3 | c2tnb239v1 | c2tnb239v2 | c2tnb239v3 | c2tnb359v1 |
          c2tnb431r1 | ipsec3 | ipsec4 | prime192v1 | prime192v2 | prime192v3 | prime239v1 |
          prime239v2 | prime239v3 | prime256v1 | secp112r1 | secp112r2 | secp128r1 | secp128r2 |
          secp160k1 | secp160r1 | secp160r2 | secp192k1 | secp192r1 | secp224k1 | secp224r1 |
          secp256k1 | secp256r1 | secp384r1 | secp521r1 | sect113r1 | sect113r2 | sect131r1 |
          sect131r2 | sect163k1 | sect163r1 | sect163r2 | sect193r1 | sect193r2 | sect233k1 |
          sect233r1 | sect239k1 | sect283k1 | sect283r1 | sect409k1 | sect409r1 | sect571k1 |
          sect571r1 | wtls1 | wtls10 | wtls11 | wtls12 | wtls3 | wtls4 | wtls5 | wtls6 | wtls7 | wtls8 |
          wtls9.
```

# `ec_prime_field`
*not exported* 

```erlang
-type ec_prime_field() :: {prime_field, Prime :: integer()}.
```

# `edwards_curve_dh`
*not exported* 

```erlang
-type edwards_curve_dh() :: x25519 | x448.
```

# `edwards_curve_ed`
*not exported* 

```erlang
-type edwards_curve_ed() :: ed25519 | ed448.
```

Note that some curves are disabled if FIPS is enabled.

# `crypto_state`

```erlang
-opaque crypto_state()
```

# `hash_state`

```erlang
-opaque hash_state()
```

# `mac_state`

```erlang
-opaque mac_state()
```

Contexts with an internal state that should not be manipulated but passed
between function calls.

# `kem`

```erlang
-type kem() :: mlkem512 | mlkem768 | mlkem1024.
```

Key encapsulation mechanisms.

# `key_integer`
*not exported* 

```erlang
-type key_integer() :: integer() | binary().
```

Always `t:binary/0` when used as return value

# `pk_encrypt_decrypt_algs`
*not exported* 

```erlang
-type pk_encrypt_decrypt_algs() :: rsa.
```

Algorithms for public key encrypt/decrypt. Only RSA is supported.

# `pk_encrypt_decrypt_opts`

```erlang
-type pk_encrypt_decrypt_opts() :: [rsa_opt()] | rsa_compat_opts().
```

# `rsa_compat_opts`
*not exported* 

```erlang
-type rsa_compat_opts() :: [{rsa_pad, rsa_padding()}] | rsa_padding().
```

Those option forms are kept only for compatibility and should not be used in new
code.

# `rsa_opt`
*not exported* 

```erlang
-type rsa_opt() ::
          {rsa_padding, rsa_padding()} |
          {signature_md, atom()} |
          {rsa_mgf1_md, sha} |
          {rsa_oaep_label, binary()} |
          {rsa_oaep_md, sha}.
```

# `rsa_padding`
*not exported* 

```erlang
-type rsa_padding() :: rsa_pkcs1_padding | rsa_pkcs1_oaep_padding | rsa_x931_padding | rsa_no_padding.
```

Options for public key encrypt/decrypt. Only RSA is supported.

> #### Warning {: .warning }
>
> The RSA options are experimental.
>
> The exact set of options and there syntax _may_ be changed without prior
> notice.

# `pk_sign_verify_algs`
*not exported* 

```erlang
-type pk_sign_verify_algs() :: rsa | dss | ecdsa | eddsa | mldsa() | slh_dsa().
```

Algorithms for sign and verify.

# `pk_sign_verify_opts`

```erlang
-type pk_sign_verify_opts() :: [rsa_sign_verify_opt()].
```

# `rsa_sign_verify_opt`
*not exported* 

```erlang
-type rsa_sign_verify_opt() ::
          {rsa_padding, rsa_sign_verify_padding()} |
          {rsa_pss_saltlen, integer()} |
          {rsa_mgf1_md, sha2()}.
```

# `rsa_sign_verify_padding`
*not exported* 

```erlang
-type rsa_sign_verify_padding() ::
          rsa_pkcs1_padding | rsa_pkcs1_pss_padding | rsa_x931_padding | rsa_no_padding.
```

Options for sign and verify.

> #### Warning {: .warning }
>
> The RSA options are experimental.
>
> The exact set of options and there syntax _may_ be changed without prior
> notice.

# `dss_private`
*not exported* 

```erlang
-type dss_private() :: [key_integer()].
```

```text
dss_public() = [P, Q, G, Y]
```

Where P, Q and G are the dss parameters and Y is the public key.

```text
dss_private() = [P, Q, G, X]
```

Where P, Q and G are the dss parameters and X is the private key.

# `dss_public`
*not exported* 

```erlang
-type dss_public() :: [key_integer()].
```

# `ecdsa_params`
*not exported* 

```erlang
-type ecdsa_params() :: ec_named_curve() | ec_explicit_curve().
```

# `ecdsa_private`
*not exported* 

```erlang
-type ecdsa_private() :: key_integer().
```

# `ecdsa_public`
*not exported* 

```erlang
-type ecdsa_public() :: key_integer().
```

# `eddsa_params`
*not exported* 

```erlang
-type eddsa_params() :: edwards_curve_ed().
```

# `eddsa_private`
*not exported* 

```erlang
-type eddsa_private() :: key_integer().
```

# `eddsa_public`
*not exported* 

```erlang
-type eddsa_public() :: key_integer().
```

# `rsa_params`
*not exported* 

```erlang
-type rsa_params() :: {ModulusSizeInBits :: integer(), PublicExponent :: key_integer()}.
```

```text
rsa_public() = [E, N]
```

```erlang
rsa_private() = [E, N, D] | [E, N, D, P1, P2, E1, E2, C]
```

Where E is the public exponent, N is public modulus and D is the private
exponent. The longer key format contains redundant information that will make
the calculation faster. P1 and P2 are first and second prime factors. E1 and E2
are first and second exponents. C is the CRT coefficient. The terminology is
taken from [RFC 3447](http://www.ietf.org/rfc/rfc3447.txt).

# `rsa_private`
*not exported* 

```erlang
-type rsa_private() :: [key_integer()].
```

# `rsa_public`
*not exported* 

```erlang
-type rsa_public() :: [key_integer()].
```

# `srp_comp_params`
*not exported* 

```erlang
-type srp_comp_params() :: {user, srp_user_comp_params()} | {host, srp_host_comp_params()}.
```

# `srp_gen_params`
*not exported* 

```erlang
-type srp_gen_params() :: {user, srp_user_gen_params()} | {host, srp_host_gen_params()}.
```

# `srp_host_comp_params`
*not exported* 

```erlang
-type srp_host_comp_params() :: [binary() | atom()].
```

Where Verifier is `v`, Generator is `g` and Prime is` N`, DerivedKey is `X`, and
Scrambler is `u` (optional will be generated if not provided) from
[SRP design](http://srp.stanford.edu/design.html) Version = '3' | '6' | '6a'

# `srp_host_gen_params`
*not exported* 

```erlang
-type srp_host_gen_params() :: [binary() | atom() | list()].
```

# `srp_private`
*not exported* 

```erlang
-type srp_private() :: key_integer().
```

```text
srp_public() = key_integer()
```

Where is `A` or `B` from [SRP design](http://srp.stanford.edu/design.html)

```text
srp_private() = key_integer()
```

Where is `a` or `b` from [SRP design](http://srp.stanford.edu/design.html)

# `srp_public`
*not exported* 

```erlang
-type srp_public() :: key_integer().
```

# `srp_user_comp_params`
*not exported* 

```erlang
-type srp_user_comp_params() :: [binary() | atom()].
```

# `srp_user_gen_params`
*not exported* 

```erlang
-type srp_user_gen_params() :: [binary() | atom() | list()].
```

# `engine_cmnd`
*not exported* 

```erlang
-type engine_cmnd() :: {unicode:chardata(), unicode:chardata()}.
```

Pre and Post commands for [engine_load/3 and /4](`engine_load/3`).

# `engine_key_ref`
*not exported* 

```erlang
-type engine_key_ref() ::
          #{engine := engine_ref(), key_id := key_id(), password => password(), term() => term()}.
```

# `engine_method_type`
*not exported* 

```erlang
-type engine_method_type() ::
          engine_method_rsa | engine_method_dsa | engine_method_dh | engine_method_rand |
          engine_method_ecdh | engine_method_ecdsa | engine_method_ciphers | engine_method_digests |
          engine_method_store | engine_method_pkey_meths | engine_method_pkey_asn1_meths |
          engine_method_ec.
```

# `engine_ref`

```erlang
-type engine_ref() :: term().
```

The result of a call to `engine_load/3`.

# `key_id`

```erlang
-type key_id() :: string() | binary().
```

Identifies the key to be used. The format depends on the loaded engine. It is
passed to the `ENGINE_load_(private|public)_key` functions in libcrypto.

# `password`

```erlang
-type password() :: string() | binary().
```

The password of the key stored in an engine.

# `crypto_integer`
*not exported* 

```erlang
-type crypto_integer() :: binary() | integer().
```

# `mldsa`

```erlang
-type mldsa() :: mldsa44 | mldsa65 | mldsa87.
```

# `mldsa_private`
*not exported* 

```erlang
-type mldsa_private() :: {seed | expandedkey, binary()}.
```

# `mldsa_public`
*not exported* 

```erlang
-type mldsa_public() :: binary().
```

# `rand_cache_state`
*not exported* 

```erlang
-type rand_cache_state() :: binary().
```

# `rand_plugin_aes_state`
*not exported* 

```erlang
-type rand_plugin_aes_state() :: dynamic().
```

# `rand_plugin_prng1_init_state`
*not exported* 

```erlang
-type rand_plugin_prng1_init_state() :: {cipher_iv(), binary(), binary(), binary()}.
```

# `rand_plugin_prng1_state`
*not exported* 

```erlang
-type rand_plugin_prng1_state() ::
          rand_plugin_prng1_init_state() | maybe_improper_list(binary(), {crypto_state(), binary()}).
```

# `rand_plugin_state`
*not exported* 

```erlang
-type rand_plugin_state() :: no_state.
```

# `slh_dsa`

```erlang
-type slh_dsa() ::
          slh_dsa_shake_128s | slh_dsa_shake_128f | slh_dsa_sha2_128s | slh_dsa_sha2_128f |
          slh_dsa_shake_192s | slh_dsa_shake_192f | slh_dsa_sha2_192s | slh_dsa_sha2_192f |
          slh_dsa_shake_256s | slh_dsa_shake_256f | slh_dsa_sha2_256s | slh_dsa_sha2_256f.
```

# `slh_dsa_private`
*not exported* 

```erlang
-type slh_dsa_private() :: binary().
```

# `slh_dsa_public`
*not exported* 

```erlang
-type slh_dsa_public() :: binary().
```

# `crypto_final`
*since OTP 23.0* 

```erlang
-spec crypto_final(State) -> FinalResult when State :: crypto_state(), FinalResult :: binary().
```

Finalize a streaming encryptions or decryptions operation and delivers the final
bytes of the final block.

The data returned from this function may be empty if no padding was enabled in
`crypto_init/3` or `crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `crypto_get_data`
*since OTP 23.0* 

```erlang
-spec crypto_get_data(State) -> Result when State :: crypto_state(), Result :: map().
```

Return information about a `t:crypto_state/0`.

The information returned is a map, which currently contains at least:

- **`size`** - The number of bytes encrypted or decrypted so far.

- **`padding_size`** - After a call to `crypto_final/1` it contains the number
  of bytes padded. Otherwise 0.

- **`padding_type`** - The type of the padding as provided in the call to
  `crypto_init/3` or `crypto_init/4`.

- **`encrypt`** - Is `true` if encryption is performed. It is `false` otherwise.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `crypto_init`
*since OTP 22.0* 

```erlang
-spec crypto_init(Cipher, Key, FlagOrOptions) -> State
                     when
                         Cipher :: cipher_no_iv(),
                         Key :: iodata(),
                         FlagOrOptions :: crypto_opts() | boolean(),
                         State :: crypto_state().
```

Initialize the state for a streaming encryption or decryption
operation.

Equivalent to the call
[`crypto_init(Cipher, Key, <<>>, FlagOrOptions)`](`crypto_init/4`). It is
intended for ciphers without an IV (nounce).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `crypto_init`
*since OTP 22.0* 

```erlang
-spec crypto_init(Cipher, Key, IV, FlagOrOptions) -> State
                     when
                         Cipher :: cipher_iv(),
                         Key :: iodata(),
                         IV :: iodata(),
                         FlagOrOptions :: crypto_opts(),
                         State :: crypto_state().
```

Initialize the state for a streaming encryptions or decryptions operation.

The returned state should be used as argument to `crypto_update/2` and
`crypto_final/1` to do the actual encryption or decryption.

If `IV = <<>>`, no IV is used. This is intended for ciphers without an IV
(nounce). See `crypto_init/3`.

For encryption, set the `FlagOrOptions` to `true` or `[{encrypt,true}]`. For
decryption, set it to `false` or `[{encrypt,false}]`.

Padding could be enabled with the option [\{padding,Padding\}](`t:padding/0`).
The [cryptolib_padding](`t:cryptolib_padding/0`) enables `pkcs_padding` or no
padding (`none`). The paddings `zero` or `random` fills the last part of the
last block with zeroes or random bytes. If the last block is already full,
nothing is added.

In decryption, the [cryptolib_padding](`t:cryptolib_padding/0`) removes such
padding, if present. The [otp_padding](`t:otp_padding/0`) is not removed - it
has to be done elsewhere.

If padding is `{padding,none}` or not specified and the total data from all
subsequent [crypto_updates](`crypto_update/2`) does not fill the last block
fully, that last data is lost. In case of `{padding,none}` there will be an
error in this case. If padding is not specified, the bytes of the unfilled block
is silently discarded.

The actual padding is performed by `crypto_final/1`.

For blocksizes call `cipher_info/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#examples-of-crypto_init-4-and-crypto_update-2)

# `crypto_one_time`
*since OTP 22.0* 

```erlang
-spec crypto_one_time(Cipher, Key, Data, FlagOrOptions) -> Result
                         when
                             Cipher :: cipher_no_iv(),
                             Key :: iodata(),
                             Data :: iodata(),
                             FlagOrOptions :: crypto_opts() | boolean(),
                             Result :: binary().
```

Do a complete encrypt or decrypt of the full text.

As `crypto_one_time/5` but for ciphers without IVs.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `crypto_one_time`
*since OTP 22.0* 

```erlang
-spec crypto_one_time(Cipher, Key, IV, Data, FlagOrOptions) -> Result
                         when
                             Cipher :: cipher_iv(),
                             Key :: iodata(),
                             IV :: iodata(),
                             Data :: iodata(),
                             FlagOrOptions :: crypto_opts() | boolean(),
                             Result :: binary().
```

Do a complete encrypt or decrypt of the full text.

Argument `Data` is the text to be encrypted or decrypted.

For encryption, set the `FlagOrOptions` to `true`. For decryption, set it to
`false`. For setting other options, see `crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See [examples in the User's Guide.](new_api.md#example-of-crypto_one_time-5)

# `crypto_one_time_aead`
*since OTP 28.0* 

```erlang
-spec crypto_one_time_aead(State, IV, InText, AAD) -> Result
                              when
                                  State :: crypto_state(),
                                  IV :: iodata(),
                                  InText :: iodata(),
                                  AAD :: iodata(),
                                  Result :: EncryptResult | DecryptResult,
                                  EncryptResult :: binary(),
                                  DecryptResult :: binary() | error.
```

Do a complete encrypt or decrypt with an AEAD cipher of the full text.

Similar to 'crypto_one_time_aead/7' but uses the handle from 'crypto_one_time_aead_init/4'.

Appends the tag of the specified 'TagLength' to the end of the encrypted data, when doing encryption.
Strips the tag from the end of 'InText' and verifies it when doing decryption.

# `crypto_one_time_aead`
*since OTP 22.0* 

```erlang
-spec crypto_one_time_aead(Cipher, Key, IV, InText, AAD, EncFlag :: true) -> Result
                              when
                                  Cipher :: cipher_aead(),
                                  Key :: iodata(),
                                  IV :: iodata(),
                                  InText :: iodata(),
                                  AAD :: iodata(),
                                  Result :: EncryptResult,
                                  EncryptResult :: {OutCryptoText, OutTag},
                                  OutCryptoText :: binary(),
                                  OutTag :: binary().
```

Do a complete encrypt with an AEAD cipher of the full text
with the default tag length.

Equivalent to
`crypto_one_time_aead(Cipher, Key, IV, InText, AAD, TagLength, true)`
where `TagLength` is the default tag length for the given `Cipher`.

# `crypto_one_time_aead`
*since OTP 22.0* 

```erlang
-spec crypto_one_time_aead(Cipher, Key, IV, InText, AAD, TagOrTagLength, EncFlag) -> Result
                              when
                                  Cipher :: cipher_aead(),
                                  Key :: iodata(),
                                  IV :: iodata(),
                                  InText :: iodata(),
                                  AAD :: iodata(),
                                  TagOrTagLength :: EncryptTagLength | DecryptTag,
                                  EncryptTagLength :: non_neg_integer(),
                                  DecryptTag :: iodata(),
                                  EncFlag :: boolean(),
                                  Result :: EncryptResult | DecryptResult,
                                  EncryptResult :: {OutCryptoText, OutTag},
                                  DecryptResult :: OutPlainText | error,
                                  OutCryptoText :: binary(),
                                  OutTag :: binary(),
                                  OutPlainText :: binary().
```

Do a complete encrypt or decrypt with an AEAD cipher of the full text.

For encryption, set the `EncryptFlag` to `true` and set the `TagOrTagLength` to
the wanted size (in bytes) of the tag, that is, the tag length. If the default
length is wanted, the `crypto_one_time_aead/6` form may be used.

For decryption, set the `EncryptFlag` to `false` and put the tag to be checked
in the argument `TagOrTagLength`.

> #### Warning {: .warning }
>
> The length of the tag at decryption is not checked by the function. It is the
> caller's responsibility to ensure that the length of the tag matches the
> length of the tag used when the data was encrypted. Otherwise the decryption
> may succeed if the given tag only matches the start of the proper tag.

Additional Authentication Data (AAD) is plaintext data that will not be
encrypted, but will be covered by authenticity protection. It should be provided
through the `AAD` argument, but can be an empty binary as well (`<<>>`) if not
needed. In that case, a plain AE (Authenticated Encryption) is performed instead
of AEAD (Authenticated Encryption with Associated Data). This function only
supports ciphers that can be used both with and without AAD.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#example-of-crypto_one_time_aead-6)

# `crypto_one_time_aead_init`
*since OTP 28.0* 

```erlang
-spec crypto_one_time_aead_init(Cipher, Key, TagLength, EncFlag) -> Result
                                   when
                                       Cipher :: cipher_aead(),
                                       Key :: iodata(),
                                       TagLength :: non_neg_integer(),
                                       EncFlag :: boolean(),
                                       Result :: crypto_state().
```

Initializes AEAD cipher.

Similar to 'crypto_one_time_aead/7' but only does the initialization part,
returns a handle that can be used with 'crypto_one_time_aead/4' serveral times.

# `crypto_update`
*since OTP 22.0* 

```erlang
-spec crypto_update(State, Data) -> Result
                       when State :: crypto_state(), Data :: iodata(), Result :: binary().
```

Add data to a streaming encryption or decryption operation.

If the part is less than a number of full blocks, only the full blocks (possibly
none) are encrypted or decrypted and the remaining bytes are saved to the next
`crypto_update` operation. The `State` should be created with `crypto_init/3` or
`crypto_init/4`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#examples-of-crypto_init-4-and-crypto_update-2)

# `enable_fips_mode`
*since OTP 21.1* 

> This function is deprecated. crypto:enable_fips_mode/1 is deprecated; use config parameter fips_mode.

```erlang
-spec enable_fips_mode(Enable) -> Result when Enable :: boolean(), Result :: boolean().
```

Enable or disable FIPs mode.

Argument `Enable` should be `true` to enable and `false` to disable FIPS mode.
Returns `true` if the operation was successful or `false` otherwise.

Note that to enable FIPS mode successfully, OTP must be built with the configure
option `--enable-fips`, and the underlying libcrypto must also support FIPS.

See also `info_fips/0`.

# `start`

> This function is deprecated. crypto:start/0 is deprecated; use application:start(crypto) instead.

```erlang
-spec start() -> ok | {error, Reason :: term()}.
```

Use [`application:start(crypto)`](`application:start/1`) instead.

> #### Warning {: .warning }
>
> This function does not work if FIPS mode is to be enabled. FIPS mode will be
> disabled even if configuration parameter `fips_mode` is set to `true`. Use
> [`application:start(crypto)`](`application:start/1`) instead.

# `stop`

> This function is deprecated. crypto:stop/0 is deprecated; use application:stop(crypto) instead.

```erlang
-spec stop() -> ok | {error, Reason :: term()}.
```

Use [`application:stop(crypto)`](`application:stop/1`) instead.

# `engine_add`
*since OTP 21.0.6* 

```erlang
-spec engine_add(Engine) -> Result when Engine :: engine_ref(), Result :: ok | {error, Reason :: term()}.
```

Add the engine to OpenSSL's internal list.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_by_id`
*since OTP 21.0.6* 

```erlang
-spec engine_by_id(EngineId) -> Result
                      when
                          EngineId :: unicode:chardata(),
                          Result :: {ok, Engine :: engine_ref()} | {error, Reason :: term()}.
```

Get a reference to an already loaded engine with `EngineId`. An error tuple is
returned if the engine cannot be unloaded.

The function raises a `error:badarg` if the parameter is in wrong format. It may
also raise the exception `error:notsup` in case there is no engine support in
the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

# `engine_ctrl_cmd_string`
*since OTP 20.2* 

```erlang
-spec engine_ctrl_cmd_string(Engine, CmdName, CmdArg) -> Result
                                when
                                    Engine :: term(),
                                    CmdName :: unicode:chardata(),
                                    CmdArg :: unicode:chardata(),
                                    Result :: ok | {error, Reason :: term()}.
```

Send ctrl commands to an OpenSSL engine.

This function is the same as calling `engine_ctrl_cmd_string/4` with `Optional`
set to `false`.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_ctrl_cmd_string`
*since OTP 20.2* 

```erlang
-spec engine_ctrl_cmd_string(Engine, CmdName, CmdArg, Optional) -> Result
                                when
                                    Engine :: term(),
                                    CmdName :: unicode:chardata(),
                                    CmdArg :: unicode:chardata(),
                                    Optional :: boolean(),
                                    Result :: ok | {error, Reason :: term()}.
```

Send ctrl commands to an OpenSSL engine.

`Optional` is a
boolean argument that can relax the semantics of the function. If set to `true`
it will only return failure if the ENGINE supported the given command name but
failed while executing it, if the ENGINE does not support the command name it
will simply return success without doing anything. In this case we assume the
user is only supplying commands specific to the given ENGINE so we set this to
`false`.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_get_all_methods`
*since OTP 20.2* 

```erlang
-spec engine_get_all_methods() -> Result when Result :: [engine_method_type()].
```

Return a list of all possible engine methods.

May raise exception `error:notsup` in case there is no engine support in the
underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

# `engine_get_id`
*since OTP 21.0.6* 

```erlang
-spec engine_get_id(Engine) -> EngineId when Engine :: engine_ref(), EngineId :: unicode:chardata().
```

Return the ID for the engine, or an empty binary if there is no id set.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_get_name`
*since OTP 21.0.6* 

```erlang
-spec engine_get_name(Engine) -> EngineName
                         when Engine :: engine_ref(), EngineName :: unicode:chardata().
```

Return the name (eg a description) for the engine, or an empty binary if there
is no name set.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_list`
*since OTP 20.2* 

```erlang
-spec engine_list() -> Result when Result :: [EngineId :: unicode:chardata()].
```

List the id's of all engines in OpenSSL's internal list.

It may also raise the exception `error:notsup` in case there is no engine
support in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

May raise exception `error:notsup` in case engine functionality is not supported
by the underlying OpenSSL implementation.

# `engine_load`
*since OTP 20.2* 

```erlang
-spec engine_load(EngineId, PreCmds, PostCmds) -> Result
                     when
                         EngineId :: unicode:chardata(),
                         PreCmds :: [engine_cmnd()],
                         PostCmds :: [engine_cmnd()],
                         Result :: {ok, Engine :: engine_ref()} | {error, Reason :: term()}.
```

Load an OpenSSL engine.

Loads the OpenSSL engine given by `EngineId` if it is available and intialize
it. Returns `ok` and an engine handle, or if the engine cannot be loaded
an error tuple is returned.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

# `engine_register`
*since OTP 25.1* 

```erlang
-spec engine_register(Engine, EngineMethods) -> Result
                         when
                             Engine :: engine_ref(),
                             EngineMethods :: [engine_method_type()],
                             Result :: ok | {error, Reason :: term()}.
```

Register engine to handle some type of methods, for example
engine_method_digests.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_remove`
*since OTP 21.0.6* 

```erlang
-spec engine_remove(Engine) -> Result
                       when Engine :: engine_ref(), Result :: ok | {error, Reason :: term()}.
```

Remove the engine from OpenSSL's internal list.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `engine_unload`
*since OTP 20.2* 

```erlang
-spec engine_unload(Engine) -> Result
                       when Engine :: engine_ref(), Result :: ok | {error, Reason :: term()}.
```

Unload an OpenSSL engine.

Unloads the OpenSSL engine given by `Engine`. An error tuple is returned if the
engine cannot be unloaded.

The function raises a `error:badarg` if the parameter is in wrong format. It may
also raise the exception `error:notsup` in case there is no engine support in
the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

# `engine_unregister`
*since OTP 25.1* 

```erlang
-spec engine_unregister(Engine, EngineMethods) -> Result
                           when
                               Engine :: engine_ref(),
                               EngineMethods :: [engine_method_type()],
                               Result :: ok | {error, Reason :: term()}.
```

Unregister engine so it does not handle some type of methods.

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

# `ensure_engine_loaded`
*since OTP 21.0.6* 

```erlang
-spec ensure_engine_loaded(EngineId, LibPath) -> Result
                              when
                                  EngineId :: unicode:chardata(),
                                  LibPath :: unicode:chardata(),
                                  Result :: {ok, Engine :: engine_ref()} | {error, Reason :: term()}.
```

Load a dynamic engine if not already done.

Loada the engine given by `EngineId` and the path to the dynamic library
implementing the engine. An error tuple is returned if the engine cannot
be loaded.

This function differs from the normal engine_load in the sense that it also add
the engine id to OpenSSL's internal engine list. The difference between the
first call and the following is that the first loads the engine with the
dynamical engine and the following calls fetch it from the OpenSSL's engine
list. All references that is returned are equal.

Use [`engine_unload/1`](`engine_unload/1`) function to remove the references.
But remember that [`engine_unload/1`](`engine_unload/1`) just removes the
references to the engine and not the tag in OpenSSL's engine list. That has to
be done with the [`engine_remove/1`](`engine_remove/1`) function when needed
(just called once, from any of the references you got).

The function raises a `error:badarg` if the parameters are in wrong format. It
may also raise the exception `error:notsup` in case there is no engine support
in the underlying OpenSSL implementation.

See also the chapter [Engine Load](engine_load.md#engine_load) in the User's
Guide.

# `pbkdf2_hmac`
*since OTP 24.2* 

```erlang
-spec pbkdf2_hmac(Digest, Pass, Salt, Iter, KeyLen) -> Result
                     when
                         Digest :: sha | sha224 | sha256 | sha384 | sha512 | sha512_224 | sha512_256,
                         Pass :: binary(),
                         Salt :: binary(),
                         Iter :: pos_integer(),
                         KeyLen :: pos_integer(),
                         Result :: binary().
```

PKCS #5 PBKDF2 (Password-Based Key Derivation Function 2) in combination with
HMAC.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `privkey_to_pubkey`
*since OTP 20.2* 

```erlang
-spec privkey_to_pubkey(Type, EnginePrivateKeyRef) -> PublicKey
                           when
                               Type :: rsa | dss,
                               EnginePrivateKeyRef :: engine_key_ref(),
                               PublicKey :: rsa_public() | dss_public().
```

Fetch public key from a private key stored in an Engine.

The key must be of the type indicated by the Type parameter.

# `hash`
*since OTP R15B02* 

```erlang
-spec hash(Type, Data) -> Digest when Type :: hash_algorithm(), Data :: iodata(), Digest :: binary().
```

Compute a message digest.

Argument `Type` is the digest type and argument `Data` is the full message.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `hash_final`
*since OTP R15B02* 

```erlang
-spec hash_final(State) -> Digest when State :: hash_state(), Digest :: binary().
```

Finalize a streaming hash calculation.

Argument `State` as returned from the last call to
[hash_update](`hash_update/2`). The size of `Digest` is determined by
the type of hash function used to generate it.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `hash_init`
*since OTP R15B02* 

```erlang
-spec hash_init(Type) -> State when Type :: hash_algorithm(), State :: hash_state().
```

Initialize the state for a streaming hash digest calculation.

Argument `Type` determines which digest to use. The returned state should be
used as argument to `hash_update/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `hash_update`
*since OTP R15B02* 

```erlang
-spec hash_update(State, Data) -> NewState
                     when State :: hash_state(), NewState :: hash_state(), Data :: iodata().
```

Add data to a streaming digest calculation.

Update the digest using the given `Data` of any length.

Argument `State` must have been generated by [hash_init](`hash_init/1`) or a
previous call to this function.

Returns `NewState` that must be passed into the next call to `hash_update/2` or
`hash_final/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `hash_xof`
*since OTP 26.0* 

```erlang
-spec hash_xof(Type, Data, Length) -> Digest
                  when
                      Type :: hash_xof_algorithm(),
                      Data :: iodata(),
                      Length :: non_neg_integer(),
                      Digest :: binary().
```

Compute a message digest for an `xof_algorithm`.

Argument `Type` is the type of digest, `Data` is the full text and `Length` is
the digest length in bits.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

May raise exception `error:notsup` in case the chosen `Type` is not supported by
the underlying libcrypto implementation.

# `compute_key`
*since OTP R16B01* 

```erlang
-spec compute_key(Type, OthersPublicKey, MyPrivateKey, Params) -> SharedSecret
                     when
                         Type :: dh | ecdh | eddh | srp,
                         SharedSecret :: binary(),
                         OthersPublicKey :: dh_public() | ecdh_public() | srp_public(),
                         MyPrivateKey :: dh_private() | ecdh_private() | {srp_public(), srp_private()},
                         Params :: dh_params() | ecdh_params() | srp_comp_params().
```

Compute the shared secret from the private key and the other party's public
key.

See also `public_key:compute_key/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `decapsulate_key`
*since OTP 28.1* 

```erlang
-spec decapsulate_key(Type, MyPrivKey, EncapSecret) -> Secret
                         when
                             Type :: kem(),
                             MyPrivKey :: binary(),
                             EncapSecret :: binary(),
                             Secret :: binary().
```

Regenerate shared secret from encapsulated secret and private key.

Returns a shared secret for encryption/decryption by local party.

Supported encapsulation methods can be obtained with
[`supports(kems)`](`supports/1`).

# `encapsulate_key`
*since OTP 28.1* 

```erlang
-spec encapsulate_key(Type, OthersPublicKey) -> {Secret, EncapSecret}
                         when
                             Type :: kem(),
                             OthersPublicKey :: binary(),
                             Secret :: binary(),
                             EncapSecret :: binary().
```

Generate encapsulated shared secret from the other party's public key.

Returns both a shared secret for encryption/decryption by local party and an
encapsulated format of the same secret to be safely sent to the other
party. With its private key, the other party can decapsulate the received secret
(with `decapsulate_key/3` for example) to regenerate the same shared secret.

Supported encapsulation methods can be obtained with
[`supports(kems)`](`supports/1`).

# `generate_key`
*since OTP R16B01* 

```erlang
-spec generate_key(Type, Params) -> {PublicKey, PrivKeyOut}
                      when
                          Type ::
                              dh | ecdh | eddh | eddsa | rsa |
                              mldsa() |
                              mlkem512 | mlkem768 | mlkem1024 |
                              slh_dsa() |
                              srp,
                          PublicKey :: dh_public() | ecdh_public() | rsa_public() | srp_public(),
                          PrivKeyOut ::
                              dh_private() |
                              ecdh_private() |
                              rsa_private() |
                              {srp_public(), srp_private()},
                          Params ::
                              dh_params() |
                              ecdh_params() |
                              eddsa_params() |
                              rsa_params() |
                              srp_gen_params() |
                              [].
```

# `generate_key`
*since OTP R16B01* 

```erlang
-spec generate_key(Type, Params, PrivKeyIn) -> {PublicKey, PrivKeyOut}
                      when
                          Type ::
                              dh | ecdh | eddh | eddsa | rsa |
                              mldsa() |
                              mlkem512 | mlkem768 | mlkem1024 | srp |
                              slh_dsa(),
                          PublicKey :: dh_public() | ecdh_public() | rsa_public() | srp_public(),
                          PrivKeyIn ::
                              undefined |
                              dh_private() |
                              ecdh_private() |
                              rsa_private() |
                              {srp_public(), srp_private()},
                          PrivKeyOut ::
                              dh_private() |
                              ecdh_private() |
                              rsa_private() |
                              {srp_public(), srp_private()},
                          Params ::
                              dh_params() |
                              ecdh_params() |
                              eddsa_params() |
                              rsa_params() |
                              srp_comp_params() |
                              [].
```

Generate a public key.

See also `public_key:generate_key/1`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Note {: .info }
>
> If the linked version of cryptolib is OpenSSL 3.0
>
> - and the `Type` is `dh` (diffie-hellman)
> - and the parameter `P` (in `t:dh_params/0`) is one of the MODP groups (see
>   [RFC 3526](https://tools.ietf.org/html/rfc3526))
> - and the optional `MaxPrivateKeyBitLength` parameter (in `t:dh_params/0`) is
>   present,
>
> then the optional key length parameter must be at least 224, 256, 302, 352 and
> 400 for group sizes of 2048, 3072, 4096, 6144 and 8192, respectively.

# `private_decrypt`
*since OTP R16B01* 

```erlang
-spec private_decrypt(Algorithm, CipherText, PrivateKey, Options) -> PlainText
                         when
                             Algorithm :: pk_encrypt_decrypt_algs(),
                             CipherText :: binary(),
                             PrivateKey :: rsa_private() | engine_key_ref(),
                             Options :: pk_encrypt_decrypt_opts(),
                             PlainText :: binary().
```

Decrypt using a private key.

Decrypts the `CipherText`, encrypted with `public_encrypt/4` (or equivalent
function) using the `PrivateKey`, and returns the plaintext (message digest).
This is a low level signature verification operation used for instance by older
versions of the SSL protocol. See also
[public_key:decrypt_private/2,3](`public_key:decrypt_private/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.

# `private_encrypt`
*since OTP R16B01* 

```erlang
-spec private_encrypt(Algorithm, PlainText, PrivateKey, Options) -> CipherText
                         when
                             Algorithm :: pk_encrypt_decrypt_algs(),
                             PlainText :: binary(),
                             PrivateKey :: rsa_private() | engine_key_ref(),
                             Options :: pk_encrypt_decrypt_opts(),
                             CipherText :: binary().
```

Encrypt using a private key.

Encrypts the `PlainText` using the `PrivateKey` and returns the ciphertext. This
is a low level signature operation used for instance by older versions of the
SSL protocol. See also
[public_key:encrypt_private/2,3](`public_key:encrypt_private/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

Public-key decryption using the private key. See also `crypto:private_decrypt/4`

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
> For digital signatures use of [`sign/4`](`sign/4`) together
> with [`verify/5`](`verify/5`) is the prefered solution.

# `public_decrypt`
*since OTP R16B01* 

```erlang
-spec public_decrypt(Algorithm, CipherText, PublicKey, Options) -> PlainText
                        when
                            Algorithm :: pk_encrypt_decrypt_algs(),
                            CipherText :: binary(),
                            PublicKey :: rsa_public() | engine_key_ref(),
                            Options :: pk_encrypt_decrypt_opts(),
                            PlainText :: binary().
```

Decrypt using a public key.

Decrypts the `CipherText`, encrypted with `private_encrypt/4`(or equivalent
function) using the `PublicKey`, and returns the plaintext (message digest).
This is a low level signature verification operation used for instance by older
versions of the SSL protocol. See also
[public_key:decrypt_public/2,3](`public_key:decrypt_public/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use with rsa_pkcs1_padding.
> For digital signatures use of [`verify/5`](`verify/5`) together
> with [`sign/4`](`sign/4`) is the prefered solution.

# `public_encrypt`
*since OTP R16B01* 

```erlang
-spec public_encrypt(Algorithm, PlainText, PublicKey, Options) -> CipherText
                        when
                            Algorithm :: pk_encrypt_decrypt_algs(),
                            PlainText :: binary(),
                            PublicKey :: rsa_public() | engine_key_ref(),
                            Options :: pk_encrypt_decrypt_opts(),
                            CipherText :: binary().
```

Encrypt using a public key.

Encrypts the `PlainText` (message digest) using the `PublicKey` and returns the
`CipherText`. This is a low level signature operation used for instance by older
versions of the SSL protocol. See also
[public_key:encrypt_public/2,3](`public_key:encrypt_public/2`)

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

> #### Warning {: .warning }
>
> This is a legacy function, for security reasons do not use together with rsa_pkcs1_padding.

# `mac`
*since OTP 22.1* 

```erlang
-spec mac(Type :: poly1305, Key, Data) -> Mac when Key :: iodata(), Data :: iodata(), Mac :: binary().
```

Compute a `poly1305` MAC (Message Authentication Code).

Same as [`mac(Type, undefined, Key, Data)`](`mac/4`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `mac`
*since OTP 22.1* 

```erlang
-spec mac(Type, SubType, Key, Data) -> Mac
             when
                 Type :: hmac | cmac | poly1305,
                 SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                 Key :: iodata(),
                 Data :: iodata(),
                 Mac :: binary().
```

Compute a MAC (Message Authentication Code).

Argument `Type` is the type of MAC and `Data` is the full message.

`SubType` depends on the MAC `Type`:

- For `hmac` it is a hash algorithm, see
  [Algorithm Details](algorithm_details.md#hmac) in the User's Guide.
- For `cmac` it is a cipher suitable for cmac, see
  [Algorithm Details](algorithm_details.md#cmac) in the User's Guide.
- For `poly1305` it should be set to `undefined` or the [mac/2](`mac_init/2`)
  function could be used instead, see
  [Algorithm Details](algorithm_details.md#poly1305) in the User's Guide.

`Key` is the authentication key with a length according to the `Type` and
`SubType`. The key length could be found with the `hash_info/1` (`hmac`) for and
`cipher_info/1` (`cmac`) functions. For `poly1305` the key length is 32 bytes.
Note that the cryptographic quality of the key is not checked.

The `Mac` result will have a default length depending on the `Type` and
`SubType`. To set a shorter length, use `macN/4` or `macN/5` instead. The
default length is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `mac_final`
*since OTP 22.1* 

```erlang
-spec mac_final(State) -> Mac when State :: mac_state(), Mac :: binary().
```

Finalize a streaming MAC operation.

Argument `State` is the state as returned by the last call to `mac_update/2`.

The `Mac` result will have a default length depending on the `Type` and `SubType` in the
[mac_init/2,3](`mac_init/3`) call. To set a shorter length, use `mac_finalN/2`
instead. The default length is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `mac_finalN`
*since OTP 22.1* 

```erlang
-spec mac_finalN(State, MacLength) -> Mac
                    when State :: mac_state(), MacLength :: pos_integer(), Mac :: binary().
```

Finalize a MAC operation with a custom length.

Argument `State` is the state as returned by the last call to `mac_update/2`.

`Mac` will be a binary with at most `MacLength` bytes. Note that if `MacLength`
is greater than the actual number of bytes returned from the underlying hash,
the returned hash will have that shorter length instead.

The max `MacLength` is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `mac_init`
*since OTP 22.1* 

```erlang
-spec mac_init(Type :: poly1305, Key) -> State when Key :: iodata(), State :: mac_state().
```

Initialize a state for streaming `poly1305` MAC calculation.

Same as [`mac_init(Type, undefined, Key)`](`mac_init/3`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `mac_init`
*since OTP 22.1* 

```erlang
-spec mac_init(Type, SubType, Key) -> State
                  when
                      Type :: hmac | cmac | poly1305,
                      SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                      Key :: iodata(),
                      State :: mac_state().
```

Initialize the state for streaming MAC calculation.

`Type` determines which mac algorithm to use in the MAC operation.

`SubType` depends on the MAC `Type`:

- For `hmac` it is a hash algorithm, see
  [Algorithm Details](algorithm_details.md#hmac) in the User's Guide.
- For `cmac` it is a cipher suitable for cmac, see
  [Algorithm Details](algorithm_details.md#cmac) in the User's Guide.
- For `poly1305` it should be set to `undefined` or the [mac/2](`mac_init/2`)
  function could be used instead, see
  [Algorithm Details](algorithm_details.md#poly1305) in the User's Guide.

`Key` is the authentication key with a length according to the `Type` and
`SubType`. The key length could be found with the `hash_info/1` (`hmac`) for and
`cipher_info/1` (`cmac`) functions. For `poly1305` the key length is 32 bytes.
Note that the cryptographic quality of the key is not checked.

The returned `State` should be used in one or more subsequent calls to
`mac_update/2`. The MAC value is finally returned by calling `mac_final/1` or
`mac_finalN/2`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See
[examples in the User's Guide.](new_api.md#example-of-mac_init-mac_update-and-mac_final)

# `mac_update`
*since OTP 22.1* 

```erlang
-spec mac_update(State0, Data) -> State
                    when Data :: iodata(), State0 :: mac_state(), State :: mac_state().
```

Add data to a streaming MAC calculation.

Update the MAC represented by `State0` using the given `Data` which could be of
any length.

The `State0` is the State value originally from a MAC init function, that is
`mac_init/2`, `mac_init/3` or the last call to `mac_update/2`. The value
`State0` is returned unchanged by the function as a reference to a mutated
internal state. Hence, it is not possible to branch off a data stream by reusing
old states.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `macN`
*since OTP 22.1* 

```erlang
-spec macN(Type :: poly1305, Key, Data, MacLength) -> Mac
              when Key :: iodata(), Data :: iodata(), Mac :: binary(), MacLength :: pos_integer().
```

Compute a `poly1305` MAC (Message Authentication Code) with a limited length.

Same as [`macN(Type, undefined, Key, Data, MacLength)`](`macN/5`).

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

# `macN`
*since OTP 22.1* 

```erlang
-spec macN(Type, SubType, Key, Data, MacLength) -> Mac
              when
                  Type :: hmac | cmac | poly1305,
                  SubType :: hmac_hash_algorithm() | cmac_cipher_algorithm() | undefined,
                  Key :: iodata(),
                  Data :: iodata(),
                  Mac :: binary(),
                  MacLength :: pos_integer().
```

Compute a MAC (Message Authentication Code) with a limited length.

Works like `mac/3` and `mac/4` but `MacLength` will limit the size of the
resultant `Mac` to at most `MacLength` bytes. Note that if `MacLength` is
greater than the actual number of bytes returned from the underlying hash, the
returned hash will have that shorter length instead.

The max `MacLength` is documented in
[Algorithm Details](algorithm_details.md#message-authentication-codes-macs) in
the User's Guide.

# `rand_seed`
*since OTP 20.0* 

```erlang
-spec rand_seed() -> {rand:alg_handler(crypto), rand_plugin_state()}.
```

Create a generator for `m:rand` and save it in the process dictionary.

Equivalent to `rand_seed_s/0` but also saves the returned
state object (generator) in the process dictionary.  That is,
it is equivalent to `rand:seed(rand_seed_s())`.

See `rand:seed/1` and `rand_seed_s/0`.

#### _Example_

```erlang
_ = crypto:rand_seed(),
IntegerValue = rand:uniform(42), % 1 .. 42
FloatValue = rand:uniform().     % [0.0, 1.0)
```

> ### Note {: .info }
>
> Note that when using the process dictionary for cryptographically
> secure random numbers one has to ensure that no code called
> between initializing the generator and between generating numbers
> accidentally alters the generator state in the process dictionary.
>
> The safe approach is to use the `m:rand` functions that
> do not use the process dictionary but take an explicit state argument:
> the ones suffixed `_s`.  Thereby it is rather `rand_seed_s/0`
> that should be used instead of this function.

# `rand_seed_alg`
*since OTP 21.0* 

```erlang
-spec rand_seed_alg(Alg :: crypto) -> {rand:alg_handler(crypto), rand_plugin_state()};
                   (Alg :: crypto_cache) -> {rand:alg_handler(crypto_cache), rand_cache_state()};
                   (ExportState :: {crypto, rand_plugin_state()}) ->
                       {rand:alg_handler(crypto), rand_plugin_state()};
                   (ExportState :: {crypto_cache, rand_cache_state()}) ->
                       {rand:alg_handler(crypto_cache), rand_cache_state()};
                   (ExportState :: {crypto_aes, rand_plugin_aes_state()}) ->
                       {rand:alg_handler(crypto_aes), rand_plugin_aes_state()};
                   (ExportState :: {crypto_prng1, rand_plugin_prng1_init_state()}) ->
                       {rand:alg_handler(crypto_prng1), rand_plugin_prng1_init_state()};
                   (State) -> State
                       when
                           State ::
                               {rand:alg_handler(crypto), rand_plugin_state()} |
                               {rand:alg_handler(crypto_cache), rand_cache_state()} |
                               {rand:alg_handler(crypto_aes), rand_plugin_aes_state()} |
                               {rand:alg_handler(crypto_prng1), rand_plugin_prng1_state()}.
```

Create a generator for `m:rand` with specified algorithm,
and save it in the process dictionary.

Equivalent to `rand_seed_alg_s/1` but also saves the returned
state object (generator) in the process dictionary.  That is,
equivalent to `rand:seed(rand_seed_alg_s(Alg))`.

See `rand:seed/1` and `rand_seed_alg_s/1`.
Note the warning about the usage of the process dictionary in `rand_seed/0`.

#### _Example_

```erlang
_ = crypto:rand_seed_alg(crypto_cache),
IntegerValue = rand:uniform(42), % 1 .. 42
FloatValue = rand:uniform().     % [0.0, 1.0)
```

# `rand_seed_alg`
*since OTP-22.0* 

```erlang
-spec rand_seed_alg(Alg :: crypto_aes, Seed :: iodata()) ->
                       {rand:alg_handler(crypto_aes), rand_plugin_aes_state()};
                   (Alg :: crypto_prng1, Seed :: iodata()) ->
                       {rand:alg_handler(crypto_prng1), rand_plugin_prng1_state()}.
```

Create and seed a generator for `m:rand` with specified algorithm,
and save it in the process dictionary.

Equivalent to `rand_seed_alg_s/2` but also saves the returned
state object (generator) in the process dictionary.  That is,
equivalent to `rand:seed(rand_seed_alg_s(Alg, Seed))`.

See `rand:seed/1` and `rand_seed_alg_s/2`.
Note the warning about the usage of the process dictionary in `rand_seed/0`.

#### _Example_

```erlang
_ = crypto:rand_seed_alg(crypto_aes, "my seed"),
IntegerValue = rand:uniform(42), % 1 .. 42
FloatValue = rand:uniform(),     % [0.0, 1.0)
_ = crypto:rand_seed_alg(crypto_aes, "my seed"),
IntegerValue = rand:uniform(42), % Same values
FloatValue = rand:uniform().     % again
```

# `rand_seed_alg_s`
*since OTP 21.0* 

```erlang
-spec rand_seed_alg_s(Alg :: crypto) -> {rand:alg_handler(crypto), rand_plugin_state()};
                     (Alg :: crypto_cache) -> {rand:alg_handler(crypto_cache), rand_cache_state()};
                     (ExportState :: {crypto, rand_plugin_state()}) ->
                         {rand:alg_handler(crypto), rand_plugin_state()};
                     (ExportState :: {crypto_cache, rand_cache_state()}) ->
                         {rand:alg_handler(crypto_cache), rand_cache_state()};
                     (ExportState :: {crypto_aes, rand_plugin_aes_state()}) ->
                         {rand:alg_handler(crypto_aes), rand_plugin_aes_state()};
                     (ExportState :: {crypto_prng1, rand_plugin_prng1_init_state()}) ->
                         {rand:alg_handler(crypto_prng1), rand_plugin_prng1_init_state()};
                     (State) -> State
                         when
                             State ::
                                 {rand:alg_handler(crypto), rand_plugin_state()} |
                                 {rand:alg_handler(crypto_cache), rand_cache_state()} |
                                 {rand:alg_handler(crypto_aes), rand_plugin_aes_state()} |
                                 {rand:alg_handler(crypto_prng1), rand_plugin_prng1_state()}.
```

Create a generator for `m:rand` with specified algorithm.

Create a state object (generator) for [random number generation](`m:rand`),
which when used by the `m:rand` functions produce
**cryptographically strong** random number.

See `rand:seed_s/1` and for example `rand:uniform_s/2`.

#### With `Alg = crypto`

The created generator uses OpenSSL's `BN_rand_range`
for uniform integers and floats.

The generator also implements generating bytes efficiently
with OpenSSL's `RAND_bytes`, just like `strong_rand_bytes/1`.
See also `rand:bytes_s/2`.  *Since OTP 29.0*.

Because the OpenSSL library is called for every request,
this generator has got a very small generator state, but a large
call overhead, so for numbers and small numbers of bytes (about 10),
it becomes *very* slow compared to the default PRNG
in the `m:rand` module.  This is an unfair comparison because a PRNG
is not cryptographically strong.  Still, for a larger numbers of bytes,
(about 1 000 or more) this generator is the fastest.

This function is equivalent to `rand_seed_s/0`.

### With `Alg = crypto_cache`

The created generator fetches random data with OpenSSL's `RAND_bytes`,
just like `strong_rand_bytes/1`, and caches it in the generator's state.
Then 56 bit numbers are extracted from the cache, which makes operations
in module `m:rand` fast on 64 bit machines.

The generator also implements extracting bytes efficiently.
See also `rand:bytes_s/2`.  *Since OTP 29.0*.

Caching random data improves *amortized* performance a lot
so for numbers it becomes less than a factor 2 slower than
the default PRNG in the `m:rand` module.  For bytes
it performs very much like for `Alg = crypto` above.

Since this generator caches random data it is a bad idea
to copy its state in an attempt to fork into multiple generators.
The forked generators will produce the same numbers
until their caches are empty, which cannot be regarded as
cryptographically strong, and is probably never useful.

#### _Example_

```erlang
S0 = crypto:rand_seed_alg_s(crypto_cache),
{IntegerValue, S1} = rand:uniform(42, S0), % 1 .. 42
{FloatValue, S2} = rand:uniform(S1).       % [0.0, 1.0)
```

These generators may cause the `m:rand` functions using the returned
state object to raise the exception `error:low_entropy` in case
the random generator failed due to lack of secure "randomness".

The cache size can be changed from its default value using the
[crypto app's ](crypto_app.md) configuration parameter `rand_cache_size`.

> #### Note {: .info }
>
> The state returned from this function cannot be used to get a reproducible
> random sequence as from the other `m:rand` functions, since that would
> not be cryptographically safe.
>
> In fact when random data is cached some numbers may get reproduced,
> but this is unpredictable.
>
> The only supported usage is to generate one distinct random sequence.

#### With argument `ExportState`

For completeness, this function accepts an `ExportState`
from `rand:export_seed_s/1` used on one of this module's generators.
This function can probably only be useful for algorithm `crypto_aes`.

For algorithm `crypto` it is not very useful since the produced numbers
are as unpredictable for a new generator as for one re-created
with this function.

The same goes for algorithm `crypto_cache`, but its exported state
may contain cached random numbers which might delay having to call
OpenSSL the first time, so there is a possible slight performance gain.

For algorithm `crypto_aes` this function works as described in
`rand:export_seed_s/1` and `rand:seed_s/1`.

For algorithm `crypto_prng1` this function only works as described in
`rand:export_seed_s/1` and `rand:seed_s/1` on the initial state
after creation (seeding).  After the first random number
has been created, the state contains a `t:crypto_state/0`
that does not survive a roundtrip through Erlang's external term format.

### With argument `State`

For completeness, this function accepts a `State` just as `rand:seed_s` does.
Calling this function with a `State` from one of the algorithms
in this module only passes the state through, it is a no-op.

# `rand_seed_alg_s`
*since OTP 22.0* 

```erlang
-spec rand_seed_alg_s(Alg :: crypto_aes, Seed :: iodata()) ->
                         {rand:alg_handler(crypto_aes), rand_plugin_aes_state()};
                     (Alg :: crypto_prng1, Seed :: iodata()) ->
                         {rand:alg_handler(crypto_prng1), rand_plugin_prng1_state()}.
```

Create and seed a generator for `m:rand` with specified algorithm.

Create a state object (generator) for [random number generation](`m:rand`),
which when used by the `m:rand` functions produce
**cryptographically unpredictable** random numbers,
that can be reproduced by re-using the same `Seed`.

See `rand:seed_s/1`, and for example `rand:uniform_s/2`,
and compare to `rand_seed_alg/1`.

The state objects created by this function has cached data so they use
much more memory than the generators in the `m:rand` module.

#### With `Alg = crypto_aes`

The Xoroshiro928 generator from the `m:rand` module is used as a counter.
The generator's state is scrambled through AES-256 to create a 58-bit
pseudo random value.  This gives a long period (2^928 - 1).

The result should be statistically completely unpredictable random values,
since the scrambling is cryptographically strong and the period is
extremely long.  But the generated numbers are not to be regarded as
cryptographically strong since there is no re-keying schedule,
and since the sequence is repeated for the same seed.

- If you need cryptographically strong random numbers use `rand_seed_alg_s/1`
  with `Alg =:= crypto` or `Alg =:= crypto_cache`.
- If you need to be able to repeat the sequence use this function
  with `Alg =:= crypto_aes` (or `Alg =:= crypto_prng1`, below).
- If you do not need the statistical quality of these generators,
  there are faster generators in the `m:rand` module.
  The *amortized* speed of this generator is about 3 times slower than
  the `rand` module's [_default algorithm_](`m:rand#default-algorithm`).

#### _Example_

```erlang
1> S0 = crypto:rand_seed_alg_s(crypto_aes, "my seed").
2> %% 1..42
   {IntegerValue, S1} = rand:uniform_s(42, S0).
3> %% [0.0, 1.0)
   {FloatValue, S2} = rand:uniform_s(S1).
4> {IntegerValue,FloatValue}.
{9,0.7624867055217882}
5> S3 = crypto:rand_seed_alg_s(crypto_aes, "my seed").
6> %% Same values
   {IntegerValue, S4} = rand:uniform_s(42, S3).
7> %% again
   {FloatValue, S5} = rand:uniform_s(S4).
```

Thanks to the used generator the state object supports the
[`rand:jump/0,1`](`rand:jump/0`) function with distance 2^512.

Numbers are generated in batches and for speed reasons cached
in the generator's state. The cache size can be changed from its default
value using the [crypto app's ](crypto_app.md) configuration parameter
`rand_cache_size`.

Generating bytes, see `rand:bytes_s/2`, is done from the cached numbers,
which limits the performance as for generating numbers.  `Alg = crypto`,
is faster, for larger numbers of bytes significantly faster,
but cannot be used to reproduce a sequence.  Another alternative
is `Alg = crypto_prng1` that follows here.

#### With `Alg = crypto_prng1` *Since OTP 29.0*.

The created generator uses a stream cipher to encrypt data blocks of zeros,
which effectively results in the stream cipher's key stream as binary data.
The binary data is cached in the generator's state to achieve good
*amortized* speed.  From the cached data 58 bit numbers are extracted,
to facilitate fast operations in the `m:rand` module.
The cache size can be changed from its default value using the
[crypto app's ](crypto_app.md) configuration parameter `rand_cache_size`.

This generator also implements extracting bytes efficiently
through `rand:bytes_s/2`.

The key stream from a stream cipher is cryptographically unpredictable,
which should result in statistically completely unpredictable random values,
but the generated numbers are not to be regarded as
cryptographically strong since there is no re-keying schedule,
and since the sequence is repeated for the same seed.

For generating numbers, this generator is about 2 times slower
than the default PRNG in the `m:rand` module.  For generating bytes,
it is significantly faster, for any number of bytes.
Compared to `Alg = crypto`, this generator has much less overhead,
so for small numbers of bytes it is much faster.  Break-even is
a bit above the cache size, and over that `Alg = crypto` is faster,
but cannot be used to reproduce a sequence.

#### _Example_

```erlang
1> S0 = crypto:rand_seed_alg_s(crypto_prng1, "my seed").
2> %% 1..42
   {IntegerValue, S1} = rand:uniform_s(42, S0).
3> {Bytes, S2} = rand:bytes_s(7, S1).
4> {IntegerValue,Bytes}.
{20,<<52,185,212,38,248,228,127>>}
5> S3 = crypto:rand_seed_alg_s(crypto_prng1, "my seed").
6> %% Same values
   {IntegerValue, S4} = rand:uniform_s(42, S3).
7> %% again
   {Bytes, S5} = rand:bytes_s(7, S4).
```

The generator's state contains a `crypto_state/0` which refers to
the same encryption state even when copied, and the generator's state
contains *cached* random data.  It is therefore a bad idea to copy
the state in an attempt to fork into multiple generators.
The forked generators will produce the same numbers
until their caches are empty, and then refill their caches
with different sections of the keystream.
This is probably never useful.

The created initial state, however, can be copied and exported,
as descrided for `rand:export_seed_s/1` and `rand:seed_s/1`,
since the `crypto_state/0` is not created until the first
random value is generated.  An exported subsequent generator state
cannot be passed intact through Erlang's external term format,
and `rand_seed_alg_s/1` will fail for an exported state
of this generator that is not an initial state.

`rand:jump/1` is implemented for this generator, but also only for
its initial state.  A jump, for this generator, is implemented
by incrementing the cipher's IV to create a distinct keystream.
This is not much different from using different seed values,
but avoids a call to the hash function that is used when seeding.

#### _Example_

```erlang
1> Sa0 = crypto:rand_seed_alg_s(crypto_prng1, "my seed").
2> Sb0 = rand:jump(Sa0).
3> {BytesA, Sa1} = rand:bytes_s(7, Sa0).
4> {BytesB, Sb1} = rand:bytes_s(7, Sb0).
5> BytesA.
<<77,185,41,162,118,82,190>>
6> BytesB.
<<160,61,224,29,177,30,68>>
7> SA0 = crypto:rand_seed_alg_s(crypto_prng1, "my seed").
8> SB0 = rand:jump(SA0).
%% Same values again
9> {BytesA, SA1} = rand:bytes_s(7, SA0).
10> {BytesB, SB1} = rand:bytes_s(7, SB0).
```

#### _Algorithm details_

The `Seed` is hashed with SHA-384 to create a Key and IV
for AES-256 that is run in CTR mode over blocks of zero data.

# `rand_seed_s`
*since OTP 20.0* 

```erlang
-spec rand_seed_s() -> {rand:alg_handler(crypto), rand_plugin_state()}.
```

Create a generator for `m:rand`.

Create a state object (generator) for [random number generation](`m:rand`),
which when used by the `m:rand` functions produce
**cryptographically strong** random numbers (based on OpenSSL's
`BN_rand_range` function). See `rand:seed_s/1`, and for example
`rand:uniform_s/2`.

This generator also implements generating bytes efficiently
(based on OpenSSL's `RAND_bytes` function).
See `rand:bytes_s/2` and `strong_rand_bytes/1`.
*Since OTP 29.0*.

#### _Example_

``` erlang
S0 = crypto:rand_seed_s(),
{RandomInteger, S1} = rand:uniform_s(1000, S0).
```

May cause the `m:rand` functions using this state object
to raise the exception `error:low_entropy` in case
the random generator failed due to lack of secure "randomness".

> #### Note {: .info }
>
> The state returned from this function cannot be used to get a reproducible
> random sequence as from the other `m:rand` functions, since that would
> not be cryptographically safe.
>
> The only supported usage is to generate one distinct random sequence.

# `rand_seed`
*since OTP 17.0* 

```erlang
-spec rand_seed(binary()) -> ok.
```

Mixes in the bytes of the given binary into the internal state
of OpenSSL's random number generator.

This calls the RAND_seed function from OpenSSL. Only use this if
the system you are running on does not have enough "randomness" built in.
Normally this is when `strong_rand_bytes/1` or a generator
from `rand_seed_alg_s/1` raises `error:low_entropy`.

# `rand_uniform`

> This function is deprecated. crypto:rand_uniform/2 is deprecated; use strong_rand_range/1 instead.

```erlang
-spec rand_uniform(crypto_integer(), crypto_integer()) -> crypto_integer().
```

Generate a random integer number.

The interval is `From =< N < To`. Uses the `crypto` library
pseudo-random number generator. `To` must be larger than `From`.

> #### Note {: .info }
>
> This function is deprecated because it originally used
> the OpenSSL method BN_pseudo_rand_range that was not
> cryptographically strong and could not run out of entropy.
> That behaviour changed in OpenSSL and this function
> cannot be fixed without making it raise `error:low_entropy`,
> which is not backwards compatible.
>
> Instead, use `strong_rand_range(To - From) + From`
>
> Be aware of the possible `error:low_entropy` exception.

# `strong_rand_bytes`
*since OTP R14B03* 

```erlang
-spec strong_rand_bytes(N :: non_neg_integer()) -> binary().
```

Generate bytes with randomly uniform values 0..255.

Returns the result in a binary with `N` bytes.

Uses a cryptographically secure PRNG seeded and periodically mixed with
operating system provided entropy. By default this is the `RAND_bytes` method
from OpenSSL.

May raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

# `strong_rand_range`
*since OTP 28.3* 

```erlang
-spec strong_rand_range(Range :: pos_integer()) -> N :: non_neg_integer();
                       (Range :: binary()) -> N :: binary().
```

Generate a random integer in a specified range.

The returned random integer is in the interval is `0` =< `N` < `Range`.

Uses the `crypto` library random number generator `BN_rand_range`.

If the `Range` argument is a `pos_integer/0` the return value
is a `non_neg_integer/0`.  If the `Range` argument is a positive integer
in a `binary/0`, the return value is a non-negative integer in a `binary/0`.

May raise exception `error:low_entropy` in case the random generator failed due
to lack of secure "randomness".

# `sign`
*since OTP R16B01* 

```erlang
-spec sign(Algorithm, DigestType, Msg, Key) -> Signature
              when
                  Algorithm :: pk_sign_verify_algs(),
                  DigestType :: rsa_digest_type() | dss_digest_type() | ecdsa_digest_type() | none,
                  Msg :: iodata() | {digest, iodata()},
                  Key ::
                      rsa_private() |
                      dss_private() |
                      [ecdsa_private() | ecdsa_params()] |
                      [eddsa_private() | eddsa_params()] |
                      mldsa_private() |
                      slh_dsa_private() |
                      engine_key_ref(),
                  Signature :: binary().
```

# `sign`
*since OTP 20.1* 

```erlang
-spec sign(Algorithm, DigestType, Msg, Key, Options) -> Signature
              when
                  Algorithm :: pk_sign_verify_algs(),
                  DigestType :: rsa_digest_type() | dss_digest_type() | ecdsa_digest_type() | none,
                  Msg :: iodata() | {digest, iodata()},
                  Key ::
                      rsa_private() |
                      dss_private() |
                      [ecdsa_private() | ecdsa_params()] |
                      [eddsa_private() | eddsa_params()] |
                      mldsa_private() |
                      slh_dsa_private() |
                      engine_key_ref(),
                  Options :: pk_sign_verify_opts(),
                  Signature :: binary().
```

Create a digital signature.

The msg is either the binary "cleartext" data to be signed or it is the hashed
value of "cleartext" i.e. the digest (plaintext).

Algorithm `dss` can only be used together with digest type `sha`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See also `public_key:sign/3`.

# `verify`
*since OTP R16B01* 

```erlang
-spec verify(Algorithm, DigestType, Msg, Signature, Key) -> Result
                when
                    Algorithm :: pk_sign_verify_algs(),
                    DigestType :: rsa_digest_type() | dss_digest_type() | ecdsa_digest_type() | none,
                    Msg :: iodata() | {digest, iodata()},
                    Signature :: binary(),
                    Key ::
                        rsa_public() |
                        dss_public() |
                        [ecdsa_public() | ecdsa_params()] |
                        [eddsa_public() | eddsa_params()] |
                        mldsa_public() |
                        slh_dsa_public() |
                        engine_key_ref(),
                    Result :: boolean().
```

# `verify`
*since OTP 20.1* 

```erlang
-spec verify(Algorithm, DigestType, Msg, Signature, Key, Options) -> Result
                when
                    Algorithm :: pk_sign_verify_algs(),
                    DigestType :: rsa_digest_type() | dss_digest_type() | ecdsa_digest_type() | none,
                    Msg :: iodata() | {digest, iodata()},
                    Signature :: binary(),
                    Key ::
                        rsa_public() |
                        dss_public() |
                        [ecdsa_public() | ecdsa_params()] |
                        [eddsa_public() | eddsa_params()] |
                        mldsa_public() |
                        slh_dsa_public() |
                        engine_key_ref(),
                    Options :: pk_sign_verify_opts(),
                    Result :: boolean().
```

Verify a digital signature.

The msg is either the binary "cleartext" data to be signed or it is the hashed
value of "cleartext" i.e. the digest (plaintext).

Algorithm `dss` can only be used together with digest type `sha`.

Uses the [3-tuple style](`m:crypto#error_3tup`) for error handling.

See also `public_key:verify/4`.

# `bytes_to_integer`
*since OTP R16B01* 

```erlang
-spec bytes_to_integer(binary()) -> integer().
```

Convert binary representation, of an integer, to an Erlang integer.

# `cipher_info`
*since OTP 22.0* 

```erlang
-spec cipher_info(Type) -> Result
                     when
                         Type :: cipher(),
                         Result ::
                             #{key_length := integer(),
                               iv_length := integer(),
                               block_size := integer(),
                               mode := CipherModes,
                               type := undefined | integer(),
                               prop_aead := boolean()},
                         CipherModes ::
                             undefined | cbc_mode | ccm_mode | cfb_mode | ctr_mode | ecb_mode |
                             gcm_mode | ige_mode | ocb_mode | ofb_mode | wrap_mode | xts_mode.
```

Get information about a cipher algorithm.

Returns a map with information about block size, key length, IV length, aead
support and possibly other properties of the cipher algorithm in question.

> #### Note {: .info }
>
> The ciphers `aes_cbc`, `aes_cfb8`, `aes_cfb128`, `aes_ctr`, `aes_ecb`,
> `aes_gcm` and `aes_ccm` has no keylength in the `Type` as opposed to for
> example `aes_128_ctr`. They adapt to the length of the key provided in the
> encrypt and decrypt function. Therefore it is impossible to return a valid
> keylength in the map.
>
> Always use a `Type` with an explicit key length,

For a list of supported cipher algorithms, see
[supports(ciphers)](`supports/1`).

# `ec_curve`
*since OTP 17.0* 

```erlang
-spec ec_curve(CurveName) -> ExplicitCurve
                  when CurveName :: ec_named_curve(), ExplicitCurve :: ec_explicit_curve().
```

Return the defining parameters of a elliptic curve.

# `ec_curves`
*since OTP 17.0* 

```erlang
-spec ec_curves() -> [EllipticCurve]
                   when EllipticCurve :: ec_named_curve() | edwards_curve_dh() | edwards_curve_ed().
```

Return all supported named elliptic curves.

# `exor`

```erlang
-spec exor(iodata(), iodata()) -> binary().
```

Perform bit-wise XOR (exclusive or) on the data supplied.

The two byte sequences must be of equal length.

# `hash_equals`
*since OTP 25.0* 

```erlang
-spec hash_equals(BinA, BinB) -> Result when BinA :: binary(), BinB :: binary(), Result :: boolean().
```

Compare two binaries in constant time, such as results of HMAC computations.

Returns true if the binaries are identical, false if they are of the same length
but not identical. The function raises an `error:badarg` exception if the
binaries are of different size.

# `hash_info`
*since OTP 22.0* 

```erlang
-spec hash_info(Type) -> Result
                   when
                       Type :: hash_algorithm(),
                       Result :: #{size := integer(), block_size := integer(), type := integer()}.
```

Get information about a hash algorithm.

Returns a map with information about block_size, size and possibly other
properties of the hash algorithm in question.

For a list of supported hash algorithms, see [supports(hashs)](`supports/1`).

# `info`
*since OTP 24.2* 

```erlang
-spec info() ->
              #{compile_type := normal | debug | valgrind | asan,
                cryptolib_version_compiled := string() | undefined,
                cryptolib_version_linked := string(),
                link_type := dynamic | static,
                otp_crypto_version := string(),
                fips_provider_available => boolean(),
                fips_provider_buildinfo => string()}.
```

Get information about crypto and the OpenSSL backend.

Returns a map with information about the compilation and linking of crypto.

Example:

```erlang

 1> crypto:info().
 #{compile_type => normal,
   cryptolib_version_compiled => "OpenSSL 3.0.0 7 sep 2021",
   cryptolib_version_linked => "OpenSSL 3.0.0 7 sep 2021",
   link_type => dynamic,
   otp_crypto_version => "5.0.2",
   fips_provider_available => true,
   fips_provider_buildinfo => "3.0.0"}
 2>
```

More association types than documented may be present in the map. Some of the
associations (like fips) may be absent if not supported.

# `info_fips`
*since OTP 20.0* 

```erlang
-spec info_fips() -> not_supported | not_enabled | enabled.
```

Get information about the operating status of FIPS.

Returns the FIPS operating status of crypto and the underlying libcrypto
library. If crypto was built with FIPS support this can be either `enabled`
(when running in FIPS mode) or `not_enabled`. For other builds
this value is always `not_supported`.

See configuration parameter [fips_mode](`e:crypto:crypto_app.md#fips_mode`)
about how to enable FIPS mode.

> #### Warning {: .warning }
>
> In FIPS mode all non-FIPS compliant algorithms are disabled and raise
> exception `error:notsup`. Check [supports(ciphers)](`supports/1`) that in FIPS
> mode returns the restricted list of available algorithms.

# `info_lib`

```erlang
-spec info_lib() -> [{Name, VerNum, VerStr}]
                  when Name :: binary(), VerNum :: integer(), VerStr :: binary().
```

Get the name and version of the libraries used by crypto.

`Name` is the name of the library. `VerNum` is the numeric version according to
the library's own versioning scheme. `VerStr` contains a text variant of the
version.

```erlang
> info_lib().
[{<<"OpenSSL">>,269484095,<<"OpenSSL 1.1.0c  10 Nov 2016"">>}]
```

> #### Note {: .info }
>
> From OTP R16 the _numeric version_ represents the version of the OpenSSL
> _header files_ (`openssl/opensslv.h`) used when crypto was compiled. The text
> variant represents the libcrypto library used at runtime. In earlier OTP
> versions both numeric and text was taken from the library.

# `mod_pow`
*since OTP R16B01* 

```erlang
-spec mod_pow(N, P, M) -> Result
                 when
                     N :: binary() | integer(),
                     P :: binary() | integer(),
                     M :: binary() | integer(),
                     Result :: binary() | error.
```

Compute the function `N^P mod M`.

# `supports`
*since OTP 22.0* 

```erlang
-spec supports(Type) -> Support
                  when
                      Type :: hashs | ciphers | kems | public_keys | macs | curves | rsa_opts,
                      Support :: Hashs | Ciphers | KEMs | PKs | Macs | Curves | RSAopts,
                      Hashs ::
                          [sha1() |
                           sha2() |
                           sha3() |
                           sha3_xof() |
                           blake2() |
                           ripemd160 |
                           compatibility_only_hash()],
                      Ciphers :: [cipher()],
                      KEMs :: [kem()],
                      PKs :: [rsa | dss | ecdsa | dh | ecdh | eddh | ec_gf2m],
                      Macs :: [hmac | cmac | poly1305],
                      Curves :: [ec_named_curve() | edwards_curve_dh() | edwards_curve_ed()],
                      RSAopts :: [rsa_sign_verify_opt() | rsa_opt()].
```

Get which crypto algorithms that are supported by the underlying libcrypto
library.

See `hash_info/1` and `cipher_info/1` for information about the hash and cipher
algorithms.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
