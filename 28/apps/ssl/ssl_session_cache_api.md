# `ssl_session_cache_api`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/ssl/src/ssl_session_cache_api.erl#L25)

TLS session cache API

Defines the API for the TLS session cache (pre TLS-1.3) so that the data storage
scheme can be replaced by defining a new callback module implementing this API.

# `partial_key`
*since OTP R14B* 

```elixir
-opaque partial_key() :: {ssl:host(), inet:port_number()} | inet:port_number().
```

The opaque part of the key. Does not need to be handled by the callback.

# `session`
*since OTP R14B* 

```elixir
-opaque session() ::
            #session{session_id :: term(),
                     internal_id :: term(),
                     peer_certificate :: term(),
                     own_certificates :: term(),
                     private_key :: term(),
                     cipher_suite :: term(),
                     master_secret :: term(),
                     srp_username :: term(),
                     max_frag_enum :: term(),
                     is_resumable :: term(),
                     time_stamp :: term(),
                     ecc :: term(),
                     sign_alg :: term(),
                     dh_public_value :: term()}.
```

The session data that is stored for each session.

# `session_cache_key`
*since OTP R14B* 

```elixir
-type session_cache_key() :: {partial_key(), ssl:session_id()}.
```

A key to an entry in the session cache.

# `session_cache_ref`
*since OTP R14B* 

```elixir
-type session_cache_ref() :: any().
```

A term that can be used to reference the cache.

# `delete`
*since OTP R14B* 

```elixir
-callback delete(CacheRef, Key) -> DoNotCare
                    when CacheRef :: session_cache_ref(), Key :: session_cache_key(), DoNotCare :: any().
```

Deletes a cache entry.

Is only called from the cache handling process.

# `foldl`
*since OTP R14B* *optional* 

```elixir
-callback foldl(Fun, Acc0, CacheRef) -> Acc
                   when Fun :: fun(), Acc0 :: term(), CacheRef :: session_cache_ref(), Acc :: term().
```

Calls `Fun(Elem, AccIn)` on successive elements of the cache, starting with
 `AccIn == Acc0`.

`Fun/2` must return a new accumulator, which is passed to the
next call. The function returns the final value of the accumulator. `Acc0` is
returned if the cache is empty.

> #### Note {: .info }
>
> Since OTP-23.3 this functions is only used on the client side and does not
> need to implemented for a server cache.

# `init`
*since OTP 18.0* 

```elixir
-callback init(InitArgs) -> CacheRef when InitArgs :: list(), CacheRef :: session_cache_ref().
```

Performs possible initializations of the cache and returns a reference to it
that is used as parameter to the other API functions. 

Is called by the cache handling processes `init` function, hence
putting the same requirements on it as a normal process `init`
function. This function is called twice when starting the SSL
application, once with the role client and once with the role server,
as the SSL application must be prepared to take on both roles.

Includes property `{role, client | server}` in init argument list. 
Currently this is the only predefined property, there can also be
user-defined properties. See also application environment variable
[session_cb_init_args](ssl_app.md).

# `lookup`
*since OTP R14B* 

```elixir
-callback lookup(CacheRef, Key) -> Session
                    when
                        CacheRef :: session_cache_ref(),
                        Key :: session_cache_key(),
                        Session :: session() | undefined.
```

Looks up a cache entry. Is to be callable from any process.

# `select_session`
*since OTP R14B* *optional* 

```elixir
-callback select_session(CacheRef, Server) -> Sessions
                            when
                                CacheRef :: session_cache_ref(),
                                Server :: {ssl:host(), inet:port_number()} | inet:port_number(),
                                Sessions :: [session()].
```

Selects sessions that can be reused, that is sessions that include `PartialKey`
in its key. Is to be callable from any process.

> #### Note {: .info }
>
> Since OTP-23.3 This functions is only used on the client side and does not
> need to implemented for a server cache.

# `size`
*since OTP 19.3* 

```elixir
-callback size(CacheRef) -> Size when CacheRef :: session_cache_ref(), Size :: pos_integer().
```

Returns the number of sessions in the cache.

If size exceeds the maximum number of sessions, the current cache
entries will be invalidated regardless of their remaining lifetime. Is
to be callable from any process.

# `terminate`
*since OTP R14B* 

```elixir
-callback terminate(CacheRef) -> DoNotCare when CacheRef :: session_cache_ref(), DoNotCare :: any().
```

Takes care of possible cleanup that is needed when the cache handling process
terminates.

# `update`
*since OTP R14B* 

```elixir
-callback update(CacheRef, Key, Session) -> DoNotCare
                    when
                        CacheRef :: session_cache_ref(),
                        Key :: session_cache_key(),
                        Session :: session() | undefined,
                        DoNotCare :: any().
```

Caches a new session or updates an already cached one.

Is only called from the cache handling process.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
