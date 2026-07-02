# `digraph`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/digraph.erl#L22)

This module provides a version of labeled directed graphs ("digraphs").

The digraphs managed by this module are stored in [ETS tables](`m:ets`). That
implies the following:

- Only the process that created the digraph is allowed to update it.
- Digraphs will not be garbage collected. The ETS tables used for a digraph will
  only be deleted when `delete/1` is called or the process that created the
  digraph terminates.
- A digraph is a mutable data structure.

What makes the graphs provided here non-proper directed graphs is that multiple
edges between vertices are allowed. However, the customary definition of
directed graphs is used here.

- A _directed graph_{: #digraph } (or just "digraph") is a pair (V, E) of a
  finite set V of _vertices_{: #vertex } and a finite set E of _directed
  edges_{: #edge } (or just "edges"). The set of edges E is a subset of V × V
  (the Cartesian product of V with itself).

  In this module, V is allowed to be empty. The so obtained unique digraph is
  called the _empty digraph_{: #empty_digraph }. Both vertices and edges are
  represented by unique Erlang terms.

- Digraphs can be annotated with more information. Such information can be
  attached to the vertices and to the edges of the digraph. An annotated digraph
  is called a _labeled digraph_, and the information attached to a vertex or an
  edge is called a _label_{: #label }. Labels are Erlang terms.
- An edge e = (v, w) is said to _emanate_{: #emanate } from vertex v and to be
  _incident_{: #incident } on vertex w.
- The _out-degree_{: #out_degree } of a vertex is the number of edges emanating
  from that vertex.
- The _in-degree_{: #in_degree } of a vertex is the number of edges incident on
  that vertex.
- If an edge is emanating from v and incident on w, then w is said to be an
  _out-neighbor_{: #out_neighbour } of v, and v is said to be an _in-neighbor_{:
  #in_neighbour } of w.
- A _path_{: #path } P from v\[1] to v\[k] in a digraph (V, E) is a non-empty
  sequence v\[1], v\[2], ..., v\[k] of vertices in V such that there is an edge
  (v\[i],v\[i+1]) in E for 1 <= i < k.
- The _length_{: #length } of path P is k-1.
- Path P is _simple_{: #simple_path } if all vertices are distinct, except that
  the first and the last vertices can be the same.
- Path P is a _cycle_{: #cycle } if the length of P is not zero and v\[1] =
  v\[k].
- A _loop_{: #loop } is a cycle of length one.
- A _simple cycle_{: #simple_cycle } is a path that is both a cycle and simple.
- An _acyclic digraph_{: #acyclic_digraph } is a digraph without cycles.

### See Also

`m:digraph_utils`, `m:ets`

# `add_edge_err_rsn`
*not exported* 

```erlang
-type add_edge_err_rsn() :: {bad_edge, Path :: [vertex()]} | {bad_vertex, V :: vertex()}.
```

The error reason for when an edge could not be added to a graph.

If the edge would create a cycle in an
[acyclic digraph](`m:digraph#acyclic_digraph`), `{error, {bad_edge, Path}}` is
returned. If `G` already has an edge with value `E` connecting a different pair
of vertices, `{error, {bad_edge, [V1, V2]}}` is returned. If either of `V1` or
`V2` is not a vertex of digraph `G`, `{error, {bad_vertex, `V`}}` is returned,
V = `V1` or V = `V2`.

# `d_cyclicity`
*not exported* 

```erlang
-type d_cyclicity() :: acyclic | cyclic.
```

# `d_protection`
*not exported* 

```erlang
-type d_protection() :: private | protected.
```

# `d_type`

```erlang
-type d_type() :: d_cyclicity() | d_protection().
```

# `edge`

```erlang
-type edge() :: term().
```

Serves as the identifier or "name" of an edge. This is distinct from an edge
"label" which attaches ancillary information to the edge rather than identifying
the edge itself.

# `graph`

```erlang
-opaque graph()
```

A digraph as returned by [`new/0,1`](`new/0`).

# `label`

```erlang
-type label() :: term().
```

# `vertex`

```erlang
-type vertex() :: term().
```

# `add_edge`

```erlang
-spec add_edge(G, V1, V2) -> edge() | {error, add_edge_err_rsn()}
                  when G :: graph(), V1 :: vertex(), V2 :: vertex().
```

# `add_edge`

```erlang
-spec add_edge(G, V1, V2, Label) -> edge() | {error, add_edge_err_rsn()}
                  when G :: graph(), V1 :: vertex(), V2 :: vertex(), Label :: label().
```

Equivalent to [`add_edge(G, E, V1, V2, Label)`](`add_edge/5`), where `E` is a created edge.

The created edge is represented by term `['$e' | N]`, where `N` is an integer >= 0.

See `t:add_edge_err_rsn/0` for details on possible errors.

# `add_edge`

```erlang
-spec add_edge(G, E, V1, V2, Label) -> edge() | {error, add_edge_err_rsn()}
                  when G :: graph(), E :: edge(), V1 :: vertex(), V2 :: vertex(), Label :: label().
```

Creates (or modifies) an edge with the identifier
`E` of digraph `G`, using `Label` as the (new) [label](`m:digraph#label`) of the
edge. The edge is [emanating](`m:digraph#emanate`) from `V1` and
[incident](`m:digraph#incident`) on `V2`. Returns `E`.

See `t:add_edge_err_rsn/0` for details on possible errors.

# `add_vertex`

```erlang
-spec add_vertex(G) -> vertex() when G :: graph().
```

Creates a vertex using the empty list as label, and returns the created vertex.

The created vertex is represented by term `['$v' | N]`, where `N` is an integer >= 0.

# `add_vertex`

```erlang
-spec add_vertex(G, V) -> vertex() when G :: graph(), V :: vertex().
```

# `add_vertex`

```erlang
-spec add_vertex(G, V, Label) -> vertex() when G :: graph(), V :: vertex(), Label :: label().
```

Creates (or modifies) vertex `V` of digraph `G`, using `Label` as the (new)
[label](`m:digraph#label`) of the vertex. Returns the new vertex `V`.

# `del_edge`

```erlang
-spec del_edge(G, E) -> true when G :: graph(), E :: edge().
```

Deletes edge `E` from digraph `G`.

# `del_edges`

```erlang
-spec del_edges(G, Edges) -> true when G :: graph(), Edges :: [edge()].
```

Deletes the edges in list `Edges` from digraph `G`.

# `del_path`

```erlang
-spec del_path(G, V1, V2) -> true when G :: graph(), V1 :: vertex(), V2 :: vertex().
```

Deletes edges from digraph `G` until there are no [paths](`m:digraph#path`) from
vertex `V1` to vertex `V2`.

A sketch of the procedure employed:

- Find an arbitrary [simple path](`m:digraph#simple_path`)
  v\[1], v\[2], ..., v\[k] from `V1` to `V2` in `G`.
- Remove all edges of `G` [emanating](`m:digraph#emanate`) from v\[i] and
  [incident](`m:digraph#incident`) to v\[i+1] for 1 <= i < k (including multiple
  edges).
- Repeat until there is no path between `V1` and `V2`.

# `del_vertex`

```erlang
-spec del_vertex(G, V) -> true when G :: graph(), V :: vertex().
```

Deletes vertex `V` from digraph `G`. Any edges [emanating](`m:digraph#emanate`)
from `V` or [incident](`m:digraph#incident`) on `V` are also deleted.

# `del_vertices`

```erlang
-spec del_vertices(G, Vertices) -> true when G :: graph(), Vertices :: [vertex()].
```

Deletes the vertices in list `Vertices` from digraph `G`.

# `delete`

```erlang
-spec delete(G) -> true when G :: graph().
```

Deletes digraph `G`. This call is important as digraphs are implemented with
ETS. There is no garbage collection of ETS tables. However, the digraph is
deleted if the process that created the digraph terminates.

# `edge`

```erlang
-spec edge(G, E) -> {E, V1, V2, Label} | false
              when G :: graph(), E :: edge(), V1 :: vertex(), V2 :: vertex(), Label :: label().
```

Returns `{E, V1, V2, Label}`, where `Label` is the [label](`m:digraph#label`) of
edge `E` [emanating](`m:digraph#emanate`) from `V1` and
[incident](`m:digraph#incident`) on `V2` of digraph `G`. If no edge `E` of
digraph `G` exists, `false` is returned.

# `edges`

```erlang
-spec edges(G) -> Edges when G :: graph(), Edges :: [edge()].
```

Returns a list of all edges of digraph `G`, in some unspecified order.

# `edges`

```erlang
-spec edges(G, V) -> Edges when G :: graph(), V :: vertex(), Edges :: [edge()].
```

Returns a list of all edges [emanating](`m:digraph#emanate`) from or
[incident](`m:digraph#incident`) on `V` of digraph `G`, in some unspecified
order.

# `get_cycle`

```erlang
-spec get_cycle(G, V) -> Vertices | false when G :: graph(), V :: vertex(), Vertices :: [vertex(), ...].
```

If a [simple cycle](`m:digraph#simple_cycle`) of length two or more exists
through vertex `V`, the cycle is returned as a list `[V, ..., V]` of vertices.
If a [loop](`m:digraph#loop`) through `V` exists, the loop is returned as a list
`[V]`. If no cycles through `V` exist, `false` is returned.

`get_path/3` is used for finding a simple cycle through `V`.

# `get_path`

```erlang
-spec get_path(G, V1, V2) -> Vertices | false
                  when G :: graph(), V1 :: vertex(), V2 :: vertex(), Vertices :: [vertex(), ...].
```

Tries to find a [simple path](`m:digraph#simple_path`) from vertex `V1` to
vertex `V2` of digraph `G`. Returns the path as a list `[V1, ..., V2]` of
vertices, or `false` if no simple path from `V1` to `V2` of length one or more
exists.

Digraph `G` is traversed in a depth-first manner, and the first found path is
returned.

# `get_short_cycle`

```erlang
-spec get_short_cycle(G, V) -> Vertices | false
                         when G :: graph(), V :: vertex(), Vertices :: [vertex(), ...].
```

Tries to find an as short as possible [simple cycle](`m:digraph#simple_cycle`)
through vertex `V` of digraph `G`. Returns the cycle as a list `[V, ..., V]` of
vertices, or `false` if no simple cycle through `V` exists. Notice that a
[loop](`m:digraph#loop`) through `V` is returned as list `[V, V]`.

`get_short_path/3` is used for finding a simple cycle through `V`.

# `get_short_path`

```erlang
-spec get_short_path(G, V1, V2) -> Vertices | false
                        when G :: graph(), V1 :: vertex(), V2 :: vertex(), Vertices :: [vertex(), ...].
```

Tries to find an as short as possible [simple path](`m:digraph#simple_path`)
from vertex `V1` to vertex `V2` of digraph `G`. Returns the path as a list
`[V1, ..., V2]` of vertices, or `false` if no simple path from `V1` to `V2` of
length one or more exists.

Digraph `G` is traversed in a breadth-first manner, and the first found path is
returned.

# `in_degree`

```erlang
-spec in_degree(G, V) -> non_neg_integer() when G :: graph(), V :: vertex().
```

Returns the [in-degree](`m:digraph#in_degree`) of vertex `V` of digraph `G`.

# `in_edges`

```erlang
-spec in_edges(G, V) -> Edges when G :: graph(), V :: vertex(), Edges :: [edge()].
```

Returns a list of all edges [incident](`m:digraph#incident`) on `V` of digraph
`G`, in some unspecified order.

# `in_neighbours`

```erlang
-spec in_neighbours(G, V) -> Vertex when G :: graph(), V :: vertex(), Vertex :: [vertex()].
```

Returns a list of all [in-neighbors](`m:digraph#in_neighbour`) of `V` of digraph
`G`, in some unspecified order.

# `info`

```erlang
-spec info(G) -> InfoList
              when
                  G :: graph(),
                  InfoList ::
                      [{cyclicity, Cyclicity :: d_cyclicity()} |
                       {memory, NoWords :: non_neg_integer()} |
                       {protection, Protection :: d_protection()}].
```

Returns a list of `{Tag, Value}` pairs describing digraph `G`. The following
pairs are returned:

- `{cyclicity, Cyclicity}`, where `Cyclicity` is `cyclic` or `acyclic`,
  according to the options given to `new`.
- `{memory, NoWords}`, where `NoWords` is the number of words allocated to the
  ETS tables.
- `{protection, Protection}`, where `Protection` is `protected` or `private`,
  according to the options given to `new`.

# `new`

```erlang
-spec new() -> graph().
```

# `new`

```erlang
-spec new(Type) -> graph() when Type :: [d_type()].
```

Returns an [empty digraph](`m:digraph#empty_digraph`) with properties according
to the options in `Type`:

- **`cyclic`** - Allows [cycles](`m:digraph#cycle`) in the digraph (default).

- **`acyclic`** - The digraph is to be kept
  [acyclic](`m:digraph#acyclic_digraph`).

- **`protected`** - Other processes can read the digraph (default).

- **`private`** - The digraph can be read and modified by the creating process
  only.

If an unrecognized type option `T` is specified or `Type` is not a proper list,
a `badarg` exception is raised.

# `no_edges`

```erlang
-spec no_edges(G) -> non_neg_integer() when G :: graph().
```

Returns the number of edges of digraph `G`.

# `no_vertices`

```erlang
-spec no_vertices(G) -> non_neg_integer() when G :: graph().
```

Returns the number of vertices of digraph `G`.

# `out_degree`

```erlang
-spec out_degree(G, V) -> non_neg_integer() when G :: graph(), V :: vertex().
```

Returns the [out-degree](`m:digraph#out_degree`) of vertex `V` of digraph `G`.

# `out_edges`

```erlang
-spec out_edges(G, V) -> Edges when G :: graph(), V :: vertex(), Edges :: [edge()].
```

Returns a list of all edges [emanating](`m:digraph#emanate`) from `V` of digraph
`G`, in some unspecified order.

# `out_neighbours`

```erlang
-spec out_neighbours(G, V) -> Vertices when G :: graph(), V :: vertex(), Vertices :: [vertex()].
```

Returns a list of all [out-neighbors](`m:digraph#out_neighbour`) of `V` of
digraph `G`, in some unspecified order.

# `sink_vertices`
*since OTP 29.0* 

```erlang
-spec sink_vertices(graph()) -> [vertex()].
```

Returns a list of all vertices of graph `G` with
[out-degree](`m:graph#in_degree`) zero.

# `source_vertices`
*since OTP 29.0* 

```erlang
-spec source_vertices(graph()) -> [vertex()].
```

Returns a list of all vertices of graph `G` with
[in-degree](`m:graph#in_degree`) zero.

# `vertex`

```erlang
-spec vertex(G, V) -> {V, Label} | false when G :: graph(), V :: vertex(), Label :: label().
```

Returns `{V, Label}`, where `Label` is the [label](`m:digraph#label`) of the
vertex `V` of digraph `G`, or `false` if no vertex `V` of digraph `G` exists.

# `vertices`

```erlang
-spec vertices(G) -> Vertices when G :: graph(), Vertices :: [vertex()].
```

Returns a list of all vertices of digraph `G`, in some unspecified order.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
