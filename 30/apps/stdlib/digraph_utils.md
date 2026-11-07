# `digraph_utils`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/digraph_utils.erl#L22)

This module provides algorithms based on depth-first traversal of directed
graphs.

For basic functions on directed graphs, see the `m:digraph` module.

- A _directed graph_{: #digraph } (or just "digraph") is a pair (V, E) of a
  finite set V of _vertices_{: #vertex } and a finite set E of _directed
  edges_{: #edge } (or just "edges"). The set of edges E is a subset of V × V
  (the Cartesian product of V with itself).
- Digraphs can be annotated with more information. Such information can be
  attached to the vertices and to the edges of the digraph. An annotated digraph
  is called a _labeled digraph_, and the information attached to a vertex or an
  edge is called a _label_{: #label }.
- An edge e = (v, w) is said to _emanate_{: #emanate } from vertex v and to be
  _incident_{: #incident } on vertex w.
- If an edge is emanating from v and incident on w, then w is said to be an
  _out-neighbor_{: #out_neighbour } of v, and v is said to be an _in-neighbor_{:
  #in_neighbour } of w.
- A _path_{: #path } P from v\[1] to v\[k] in a digraph (V, E) is a non-empty
  sequence v\[1], v\[2], ..., v\[k] of vertices in V such that there is an edge
  (v\[i],v\[i+1]) in E for 1 <= i < k.
- The _length_{: #length } of path P is k-1.
- Path P is a _cycle_{: #cycle } if the length of P is not zero and v\[1] =
  v\[k].
- A _loop_{: #loop } is a cycle of length one.
- An _acyclic digraph_{: #acyclic_digraph } is a digraph without cycles.
- A _depth-first traversal_{: #depth_first_traversal } of a directed digraph can
  be viewed as a process that visits all vertices of the digraph. Initially, all
  vertices are marked as unvisited. The traversal starts with an arbitrarily
  chosen vertex, which is marked as visited, and follows an edge to an unmarked
  vertex, marking that vertex. The search then proceeds from that vertex in the
  same fashion, until there is no edge leading to an unvisited vertex. At that
  point the process backtracks, and the traversal continues as long as there are
  unexamined edges. If unvisited vertices remain when all edges from the first
  vertex have been examined, some so far unvisited vertex is chosen, and the
  process is repeated.
- A _partial ordering_{: #partial_ordering } of a set S is a transitive,
  antisymmetric, and reflexive relation between the objects of S.
- The problem of [_topological sorting_](https://en.wikipedia.org/wiki/Topological_sorting) {: #topsort }
  is to find a total ordering of S that is a superset of the partial ordering.
  A digraph G = (V, E) is equivalent to a relation E on V (we neglect that
  the version of directed graphs provided by the `digraph` module allows
  multiple edges between vertices). If the digraph has no cycles of length
  two or more, the reflexive and transitive closure of E is a partial ordering.
- A _subgraph_{: #subgraph } G' of G is a digraph whose vertices and edges form
  subsets of the vertices and edges of G.
- G' is _maximal_ with respect to a property P if all other subgraphs that
  include the vertices of G' do not have property P.
- A [_strongly connected component_](https://en.wikipedia.org/wiki/Strongly_connected_component) {: #strong_components }
  is a maximal subgraph such that there is a path between each pair of vertices
- A _connected component_{: #components } is a maximal subgraph such that there
  is a path between each pair of vertices, considering all edges undirected.
- An _arborescence_{: #arborescence } is an acyclic digraph with a vertex V, the
  _root_{: #root }, such that there is a unique path from V to every other
  vertex of G.
- A _tree_{: #tree } is an acyclic non-empty digraph such that there is a unique
  path between every pair of vertices, considering all edges undirected.

### See Also

`m:digraph`

# `arborescence_root`

```erlang
-spec arborescence_root(Digraph) -> no | {yes, Root}
                           when Digraph :: digraph:graph(), Root :: digraph:vertex().
```

Returns `{yes, Root}` if `Root` is the [root](`m:digraph_utils#root`) of the
arborescence `Digraph`, otherwise `no`.

# `components`

```erlang
-spec components(Digraph) -> [Component]
                    when Digraph :: digraph:graph(), Component :: [digraph:vertex()].
```

Returns a list of [connected components](`m:digraph_utils#components`). Each
component is represented by its vertices. The order of the vertices and the
order of the components are arbitrary. Each vertex of digraph `Digraph` occurs
in exactly one component.

# `condensation`

```erlang
-spec condensation(Digraph) -> CondensedDigraph
                      when Digraph :: digraph:graph(), CondensedDigraph :: digraph:graph().
```

Creates a digraph where the vertices are the
[strongly connected components](`m:digraph_utils#strong_components`) of
`Digraph` as returned by `strong_components/1`. If X and Y are two different
strongly connected components, and vertices x and y exist in X and Y,
respectively, such that there is an edge [emanating](`m:digraph_utils#emanate`)
from x and [incident](`m:digraph_utils#incident`) on y, then an edge emanating
from X and incident on Y is created.

The created digraph has the same type as `Digraph`. All vertices and edges have
the default [label](`m:digraph_utils#label`) `[]`.

Each [cycle](`m:digraph_utils#cycle`) is included in some strongly connected
component, which implies that a
[topological ordering](`m:digraph_utils#topsort`) of the created digraph always
exists.

# `cyclic_strong_components`

```erlang
-spec cyclic_strong_components(Digraph) -> [StrongComponent]
                                  when Digraph :: digraph:graph(), StrongComponent :: [digraph:vertex()].
```

Returns a list of
[strongly connected components](`m:digraph_utils#strong_components`). Each
strongly component is represented by its vertices. The order of the vertices and
the order of the components are arbitrary. Only vertices that are included in
some [cycle](`m:digraph_utils#cycle`) in `Digraph` are returned, otherwise the
returned list is equal to that returned by `strong_components/1`.

# `is_acyclic`

```erlang
-spec is_acyclic(Digraph) -> boolean() when Digraph :: digraph:graph().
```

Returns `true` if and only if digraph `Digraph` is
[acyclic](`m:digraph_utils#acyclic_digraph`).

# `is_arborescence`

```erlang
-spec is_arborescence(Digraph) -> boolean() when Digraph :: digraph:graph().
```

Returns `true` if and only if digraph `Digraph` is an
[arborescence](`m:digraph_utils#arborescence`).

# `is_tree`

```erlang
-spec is_tree(Digraph) -> boolean() when Digraph :: digraph:graph().
```

Returns `true` if and only if digraph `Digraph` is a
[tree](`m:digraph_utils#tree`).

# `loop_vertices`

```erlang
-spec loop_vertices(Digraph) -> Vertices when Digraph :: digraph:graph(), Vertices :: [digraph:vertex()].
```

Returns a list of all vertices of `Digraph` that are included in some
[loop](`m:digraph_utils#loop`).

# `postorder`

```erlang
-spec postorder(Digraph) -> Vertices when Digraph :: digraph:graph(), Vertices :: [digraph:vertex()].
```

Returns all vertices of digraph `Digraph`. The order is given by a
[depth-first traversal](`m:digraph_utils#depth_first_traversal`) of the digraph,
collecting visited vertices in postorder. More precisely, the vertices visited
while searching from an arbitrarily chosen vertex are collected in postorder,
and all those collected vertices are placed before the subsequently visited
vertices.

# `preorder`

```erlang
-spec preorder(Digraph) -> Vertices when Digraph :: digraph:graph(), Vertices :: [digraph:vertex()].
```

Returns all vertices of digraph `Digraph`. The order is given by a
[depth-first traversal](`m:digraph_utils#depth_first_traversal`) of the digraph,
collecting visited vertices in preorder.

# `reachable`

```erlang
-spec reachable(Vertices, Digraph) -> Reachable
                   when
                       Digraph :: digraph:graph(),
                       Vertices :: [digraph:vertex()],
                       Reachable :: [digraph:vertex()].
```

Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) in `Digraph` from some vertex of
`Vertices` to the vertex. In particular, as paths can have length zero, the
vertices of `Vertices` are included in the returned list.

# `reachable_neighbours`

```erlang
-spec reachable_neighbours(Vertices, Digraph) -> Reachable
                              when
                                  Digraph :: digraph:graph(),
                                  Vertices :: [digraph:vertex()],
                                  Reachable :: [digraph:vertex()].
```

Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) in `Digraph` of length one or
more from some vertex of `Vertices` to the vertex. As a consequence, only those
vertices of `Vertices` that are included in some
[cycle](`m:digraph_utils#cycle`) are returned.

# `reaching`

```erlang
-spec reaching(Vertices, Digraph) -> Reaching
                  when
                      Digraph :: digraph:graph(),
                      Vertices :: [digraph:vertex()],
                      Reaching :: [digraph:vertex()].
```

Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) from the vertex to some vertex
of `Vertices`. In particular, as paths can have length zero, the vertices of
`Vertices` are included in the returned list.

# `reaching_neighbours`

```erlang
-spec reaching_neighbours(Vertices, Digraph) -> Reaching
                             when
                                 Digraph :: digraph:graph(),
                                 Vertices :: [digraph:vertex()],
                                 Reaching :: [digraph:vertex()].
```

Returns an unsorted list of digraph vertices such that for each vertex in the
list, there is a [path](`m:digraph_utils#path`) of length one or more from the
vertex to some vertex of `Vertices`. Therefore only those vertices of `Vertices`
that are included in some [cycle](`m:digraph_utils#cycle`) are returned.

# `strong_components`

```erlang
-spec strong_components(Digraph) -> [StrongComponent]
                           when Digraph :: digraph:graph(), StrongComponent :: [digraph:vertex()].
```

Returns a list of
[strongly connected components](`m:digraph_utils#strong_components`). Each
strongly component is represented by its vertices. The order of the vertices and
the order of the components are arbitrary. Each vertex of digraph `Digraph`
occurs in exactly one strong component.

# `subgraph`

```erlang
-spec subgraph(Digraph, Vertices) -> SubGraph
                  when
                      Digraph :: digraph:graph(),
                      Vertices :: [digraph:vertex()],
                      SubGraph :: digraph:graph().
```

# `subgraph`

```erlang
-spec subgraph(Digraph, Vertices, Options) -> SubGraph
                  when
                      Digraph :: digraph:graph(),
                      SubGraph :: digraph:graph(),
                      Vertices :: [digraph:vertex()],
                      Options :: [{type, SubgraphType} | {keep_labels, boolean()}],
                      SubgraphType :: inherit | [digraph:d_type()].
```

Creates a maximal [subgraph](`m:digraph_utils#subgraph`) of `Digraph` having as
vertices those vertices of `Digraph` that are mentioned in `Vertices`.

If the value of option `type` is `inherit`, which is the default, the type of
`Digraph` is used for the subgraph as well. Otherwise the option value of `type`
is used as argument to `digraph:new/1`.

If the value of option `keep_labels` is `true`, which is the default, the
[labels](`m:digraph_utils#label`) of vertices and edges of `Digraph` are used
for the subgraph as well. If the value is `false`, default label `[]` is used
for the vertices and edges of the subgroup.

[`subgraph(Digraph, Vertices)`](`subgraph/2`) is equivalent to
[`subgraph(Digraph, Vertices, [])`](`subgraph/3`).

If any of the arguments are invalid, a `badarg` exception is raised.

# `topsort`

```erlang
-spec topsort(Digraph) -> Vertices | false
                 when Digraph :: digraph:graph(), Vertices :: [digraph:vertex()].
```

Returns a [topological ordering](`m:digraph_utils#topsort`) of the vertices of
digraph `Digraph` if such an ordering exists, otherwise `false`. For each vertex
in the returned list, no [out-neighbors](`m:digraph_utils#out_neighbour`) occur
earlier in the list.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
