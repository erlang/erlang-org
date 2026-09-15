# `erl_features`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_features.erl#L22)

This module contains functions for supporting features that can be
enabled/disabled in Erlang.

It should be considered as mostly for internal use, although there are some
functions that might be useful when writing tools.

# `feature`
*not exported* *since OTP 25.0* 

```erlang
-type feature() :: atom().
```

# `release`
*not exported* *since OTP 25.0* 

```erlang
-type release() :: non_neg_integer().
```

# `status`
*not exported* *since OTP 25.0* 

```erlang
-type status() :: experimental | approved | permanent | rejected.
```

# `type`
*not exported* *since OTP 25.0* 

```erlang
-type type() :: extension | backwards_incompatible_change.
```

# `all`
*since OTP 25.0* 

```erlang
-spec all() -> [feature()].
```

Return a list of all known features. This list will include features that have
been removed (status `rejected`) and features that are no longer configurable
(status `permanent`).

# `configurable`
*since OTP 25.1* 

```erlang
-spec configurable() -> [feature()].
```

Return a list of all configurable features, that is, features with status
`experimental` or `approved`. These are the features that can be enabled or
disabled.

# `enabled`
*since OTP 25.0* 

```erlang
-spec enabled() -> [feature()].
```

Return a list of the features that are currently enabled. Note that the set of
enabled is set during startup and can then not be changed.

# `info`
*since OTP 25.0* 

```erlang
-spec info(feature()) -> FeatureInfoMap | no_return()
              when
                  Description :: string(),
                  FeatureInfoMap ::
                      #{description := Description,
                        short := Description,
                        type := type(),
                        keywords := [atom()],
                        status := status(),
                        experimental => release(),
                        approved => release(),
                        permanent => release(),
                        rejected => release()}.
```

Return a map containing information about the given feature.

# `used`
*since OTP 25.0* 

```erlang
-spec used(module() | file:filename()) -> [feature()].
```

Return the list of features enabled when compiling the module. The module need
not be loaded, but is found if it exists in the loadpath. If not all features
used by the module are enabled in the runtime, loading the module is not
allowed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
