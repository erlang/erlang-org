# `mnesia_frag_hash`
[🔗](https://github.com/erlang/otp/blob/master/lib/mnesia/src/mnesia_frag_hash.erl#L29)

Defines mnesia_frag_hash callback behavior

This module defines a callback behavior for user-defined hash functions of
fragmented tables.

Which module that is selected to implement the `mnesia_frag_hash` behavior for a
particular fragmented table is specified together with the other
`frag_properties`. The `hash_module` defines the module name. The `hash_state`
defines the initial hash state.

This module implements dynamic hashing, which is a kind of hashing that grows
nicely when new fragments are added. It is well suited for scalable hash tables.

### See Also

`m:mnesia`

# `add_frag`

```erlang
-spec add_frag(State :: term()) -> {NewState, IterFrags, AdditionalLockFrags}
                  when NewState :: term(), IterFrags :: [integer()], AdditionalLockFrags :: [integer()].
```

To scale well, it is a good idea to ensure that the records are evenly
distributed over all fragments, including the new one.

`NewState` is stored as `hash_state` among the other `frag_properties`.

As a part of the `add_frag` procedure, Mnesia iterates over all fragments
corresponding to the `IterFrags` numbers and starts
[`key_to_frag_number(NewState,RecordKey)`](`key_to_frag_number/2`) for each
record. If the new fragment differs from the old fragment, the record is moved
to the new fragment.

As the `add_frag` procedure is a part of a schema transaction, Mnesia acquires
write locks on the affected tables. That is, both the fragments corresponding to
`IterFrags` and those corresponding to `AdditionalLockFrags`.

# `del_frag`

```erlang
-spec del_frag(State :: term()) -> {NewState, IterFrags, AdditionalLockFrags}
                  when NewState :: term(), IterFrags :: [integer()], AdditionalLockFrags :: [integer()].
```

`NewState` is stored as `hash_state` among the other `frag_properties`.

As a part of the `del_frag` procedure, Mnesia iterates over all fragments
corresponding to the `IterFrags` numbers and starts
[`key_to_frag_number(NewState,RecordKey)`](`key_to_frag_number/2`) for each
record. If the new fragment differs from the old fragment, the record is moved
to the new fragment.

Notice that all records in the last fragment must be moved to another fragment,
as the entire fragment is deleted.

As the `del_frag` procedure is a part of a schema transaction, Mnesia acquires
write locks on the affected tables. That is, both the fragments corresponding to
`IterFrags` and those corresponding to `AdditionalLockFrags`.

# `init_state`

```erlang
-spec init_state(Tab, State) -> NewState when Tab :: atom(), State :: term(), NewState :: term().
```

Starts when a fragmented table is created with the function
`mnesia:create_table/2` or when a normal (unfragmented) table is converted to be
a fragmented table with `mnesia:change_table_frag/2`.

Notice that the function `add_frag/2` is started one time for each of the other
fragments (except number 1) as a part of the table creation procedure.

`State` is the initial value of the `hash_state` `frag_property`. `NewState` is
stored as `hash_state` among the other `frag_properties`.

# `key_to_frag_number`

```erlang
-spec key_to_frag_number(State, Key) -> Fragnum
                            when State :: term(), Key :: term(), Fragnum :: integer().
```

Starts whenever Mnesia needs to determine which fragment a certain record
belongs to. It is typically started at `read`, `write`, and `delete`.

# `match_spec_to_frag_numbers`

```erlang
-spec match_spec_to_frag_numbers(State, MatchSpec) -> Fragnums
                                    when
                                        State :: term(),
                                        MatchSpec :: ets:match_spec(),
                                        Fragnums :: [integer()].
```

This function is called whenever Mnesia needs to determine which fragments that
need to be searched for a `MatchSpec`. It is typically called by `select` and
`match_object`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
