# `crashdump`
[🔗](https://github.com/erlang/otp/blob/master/lib/tools/src/crashdump.erl#L23)

A Tool for Handling Erlang Crash Dumps

This module contains functions for decrypting encrypted Erlang crash dumps,
which can be enabled with the `--enable-encrypted-crash-dumps` build flag.

# `decrypt`
*since OTP 29.0* 

```erlang
-spec decrypt(EncryptedFile, PrivateKey) -> {ok, erlang:iovec()} | {error, Reason}
                 when
                     EncryptedFile :: file:name(),
                     PrivateKey :: public_key:private_key(),
                     Reason :: dynamic().
```

Decrypts the Erlang crash dump at `EncryptedFile` using `PrivateKey`.

# `decrypt`
*since OTP 29.0* 

```erlang
-spec decrypt(EncryptedFile, PrivateKey, DecryptedFile) -> ok | {error, Reason}
                 when
                     EncryptedFile :: file:name(),
                     PrivateKey :: public_key:private_key(),
                     DecryptedFile :: file:name(),
                     Reason :: dynamic().
```

Decrypts the Erlang crash dump at `EncryptedFile` using `PrivateKey`, writing
the result to `DecryptedFile`.

# `pem_decrypt`
*since OTP 29.0* 

```erlang
-spec pem_decrypt(EncryptedFile, KeyFile) -> {ok, erlang:iovec()} | {error, Reason}
                     when EncryptedFile :: file:name(), KeyFile :: file:name(), Reason :: dynamic().
```

This is a convenience wrapper around `crashdump:decrypt/2` that handles key
extraction from a given PEM file.

# `pem_decrypt`
*since OTP 29.0* 

```erlang
-spec pem_decrypt(EncryptedFile, KeyFile, DecryptedFile) -> ok | {error, Reason}
                     when
                         EncryptedFile :: file:name(),
                         KeyFile :: file:name(),
                         DecryptedFile :: file:name(),
                         Reason :: dynamic().
```

This is a convenience wrapper around `crashdump:decrypt/3` that handles key
extraction from a given PEM file.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
