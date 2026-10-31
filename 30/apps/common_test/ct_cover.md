# `ct_cover`
[🔗](https://github.com/erlang/otp/blob/master/lib/common_test/src/ct_cover.erl#L23)

`Common Test` framework code coverage support module.

This module exports help functions for performing code coverage analysis.

# `add_nodes`

```erlang
-spec add_nodes(Nodes) -> {ok, StartedNodes} | {error, Reason}
                   when
                       Nodes :: node() | [node()],
                       StartedNodes :: [node()],
                       Reason :: cover_not_running | term().
```

Adds nodes to current cover test. Notice that this only works if cover support
is active.

To have effect, this function is to be called from `init_per_suite/1` (see
`m:ct_suite`) before any tests are performed.

# `cross_cover_analyse`
*since OTP R16B* 

```erlang
-spec cross_cover_analyse(Level, Tests) -> ok
                             when
                                 Level :: overview | details,
                                 Tests :: [{Tag :: atom(), Dir :: file:name_all()}].
```

Accumulates cover results over multiple tests. See section
[Cross Cover Analysis](cover_chapter.md#cross_cover) in the User's Guide.

# `remove_nodes`

```erlang
-spec remove_nodes(Nodes) -> ok | {error, Reason}
                      when
                          Nodes :: node() | [node()],
                          Reason :: cover_not_running | not_main_node | term().
```

Removes nodes from the current cover test.

Call this function to stop cover test on nodes previously added with
[`ct_cover:add_nodes/1`](`add_nodes/1`). Results on the remote node are
transferred to the `Common Test` node.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
