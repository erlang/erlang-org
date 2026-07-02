# `graph`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/graph.erl#L21)

A functional implementation of labeled directed graphs.

This module is closely modelled on the `digraph` and `digraph_utils`
modules, which represent graphs using mutable ETS tables. This functional
implementation is more lightweight and does not involve mutable state and
table ownership, and makes it easy to keep multiple versions of a graph, but
for large tables with a long lifetime, the ETS based implementation can be
more suitable. In the context of this module, we only use the term "digraph"
when referring to the `digraph` implementation, and not to directed graphs
in general.

When rewriting code from using `digraph` to using `graph`, keep in mind that:

- Graphs are immutable: each modifying operation returns the new graph,
  which needs to be saved in a variable and passed to the next operation,
  for example `G0 = graph:new(), G1 = graph:add_vertex(G0, v1), G2 =
  graph:add_vertex(G1, v2)`.
- Graphs are garbage collected and do not need to be explicitly deleted.
- There are no `protected` or `private` options and no `memory` info key.
- Edges are not objects with identity and state. An edge is uniquely
  identified by the triple `{From, To, Label}` where the default label is
  `[]`. There can be multiple edges with the same `From` and `To` but only
  if they have different values for `Label`.
- Vertices, however, have a unique identifier just as in `digraph`. The
  label of an existing vertex can be replaced by a new call to
  `add_vertex(Id, Label)`. The label defaults to `[]`.
- The functions in `digraph_utils` have been included directly in the
  `graph` module for simplicity.

Some graph theoretical definitions:

- A _directed graph_{: #graph } (here simply called "graph") is a pair (V, E)
  of a finite set V of _vertices_{: #vertex } and a finite set E of _directed
  edges_{: #edge } (here simply called "edges"). The set of edges E is a
  subset of V × V (the Cartesian product of V with itself).

  In this module, V is allowed to be empty. The so obtained unique graph is
  called the _empty graph_{: #empty_graph }. Each vertex has a unique Erlang
  term as identifier.

- Graphs can be annotated with more information. Such information can be
  attached to the vertices and to the edges of the graph. An annotated graph
  is called a _labeled graph_, and the information attached to a vertex or an
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

- A _subgraph_{: #subgraph } G' of G is a graph whose vertices and edges form
  subsets of the vertices and edges of G.

- G' is _maximal_ with respect to a property P if all other subgraphs that
  include the vertices of G' do not have property P.

- A _path_{: #path } P from v\[1] to v\[k] in a graph (V, E) is a non-empty
  sequence v\[1], v\[2], ..., v\[k] of vertices in V such that there is an edge
  (v\[i], v\[i+1]) in E for 1 <= i < k.

- The _length_{: #length } of path P is k-1.

- Path P is _simple_{: #simple_path } if all vertices are distinct, except that
  the first and the last vertices can be the same.

- Path P is a _cycle_{: #cycle } if the length of P is not zero and v\[1] =
  v\[k].

- A _loop_{: #loop } is a cycle of length one.

- A _simple cycle_{: #simple_cycle } is a path that is both a cycle and simple.

- An _acyclic graph_{: #acyclic_graph } is a graph without cycles.

- A _tree_{: #tree } is an acyclic non-empty graph such that there is a unique
  path between every pair of vertices, considering all edges undirected. In an
  undirected tree, any vertex can be used as root. Informally however, "tree"
  is often used to refer to mean an _out-tree_ (arborescence), in particular
  in computer science.

- An _arborescence_{: #arborescence } or _directed rooted tree_ or _out-tree_
  is an acyclic directed graph with a vertex V, the _root_{: #root }, such that
  there is a unique path from V to every other vertex of G.

- A _forest_{: #forest } is a disjoint union of trees.

- A [_strongly connected component_](https://en.wikipedia.org/wiki/Strongly_connected_component) {: #strong_components }
  is a maximal subgraph such that there is a path between each pair of vertices.

- A _connected component_{: #components } is a maximal subgraph such that there
  is a path between each pair of vertices, considering all edges undirected.

This module also provides algorithms based on depth-first traversal of
directed graphs.

- A _depth-first traversal_{: #depth_first_traversal } of a directed graph can
  be viewed as a process that visits all vertices of the graph. Initially, all
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
  A graph G = (V, E) is equivalent to a relation E on V (we neglect that
  the version of directed graphs provided by the `digraph` module allows
  multiple edges between vertices). If the graph has no cycles of length
  two or more, the reflexive and transitive closure of E is a partial ordering.

# `edge`
*since OTP 29.0* 

```erlang
-type edge() :: {vertex(), vertex(), label()}.
```

# `edge_map`
*not exported* *since OTP 29.0* 

```erlang
-type edge_map() :: #{vertex() => ordsets:ordset(vertex())}.
```

# `graph`
*since OTP 29.0* 

```erlang
-type graph() ::
          #graph{vs :: vertice_map(),
                 in_es :: edge_map(),
                 out_es :: edge_map(),
                 cyclic :: boolean(),
                 next_vid :: non_neg_integer()}.
```

# `graph_cyclicity`
*since OTP 29.0* 

```erlang
-type graph_cyclicity() :: acyclic | cyclic.
```

# `graph_type`
*since OTP 29.0* 

```erlang
-type graph_type() :: graph_cyclicity().
```

# `label`
*since OTP 29.0* 

```erlang
-type label() :: term().
```

# `vertex`
*since OTP 29.0* 

```erlang
-type vertex() :: term().
```

# `vertice_map`
*not exported* *since OTP 29.0* 

```erlang
-type vertice_map() :: #{vertex() => label()}.
```

# `add_edge`
*since OTP 29.0* 

```erlang
-spec add_edge(graph(), vertex(), vertex()) -> graph().
```

# `add_edge`
*since OTP 29.0* 

```erlang
-spec add_edge(G, V1, V2, L) -> graph() when G :: graph(), V1 :: vertex(), V2 :: vertex(), L :: label().
```

Creates an edge `{V1, V2, L}` in graph `G`.

The edge is [emanating](`m:graph#emanate`) from `V1` and
[incident](`m:graph#incident`) on `V2`, and has [label](`m:graph#label`)
`L`. The edge is uniquely identified by this triple. A graph can have
multiple edges between the same vertices `V1` and `V2` but only if the
edges have different labels.

If `G` was created with option `acyclic`, then attempting to add an edge that
would introduce a cycle will raise an error `{bad_edge, {From, To}}`. Note
that checking for cyclicity slows down the adding of edges.

# `add_vertex`
*since OTP 29.0* 

```erlang
-spec add_vertex(graph()) -> {vertex(), graph()}.
```

Adds a new vertex to graph `G`, returning the created vertex id.

The new vertex will have the empty list `[]` as [label](`m:graph#label`).

Note: Vertex ID:s are assigned as integers in increasing order starting from
zero. If you use `add_vertex/2` or `add_vertex/3` to insert vertices with
your own identifiers, this function could generate an ID that already exists
in the graph.

# `add_vertex`
*since OTP 29.0* 

```erlang
-spec add_vertex(graph(), vertex()) -> graph().
```

# `add_vertex`
*since OTP 29.0* 

```erlang
-spec add_vertex(graph(), vertex(), label()) -> graph().
```

Creates or modifies vertex `V` of graph `G`, using `L` as the (new)
[label](`m:graph#label`) of the vertex.

# `arborescence_root`
*since OTP 29.0* 

```erlang
-spec arborescence_root(graph()) -> no | {yes, vertex()}.
```

Returns `{yes, V}` if `G` is an [arborescence](`m:graph#arborescence`) (a
directed tree) with vertex `V` as the [root](`m:graph#root`), otherwise `no`.

# `components`
*since OTP 29.0* 

```erlang
-spec components(graph()) -> [[vertex()]].
```

Returns a list of [connected components](`m:graph#components`).

Each component is represented by its vertices. The order of the vertices
and the order of the components are arbitrary. Each vertex of graph `G`
occurs in exactly one component.

# `condensation`
*since OTP 29.0* 

```erlang
-spec condensation(graph()) -> graph().
```

Creates a graph where the vertices are the [strongly connected
components](`m:graph#strong_components`) of `G` as returned by
`strong_components/1`.

If X and Y are two different strongly connected components, and vertices x
and y exist in X and Y, respectively, such that there is an edge
[emanating](`m:graph#emanate`) from x and [incident](`m:graph#incident`) on
y, then an edge emanating from X and incident on Y is created.

The created graph has the same type as `G`. All vertices and edges have
the default [label](`m:graph#label`) `[]`.

Each [cycle](`m:graph#cycle`) is included in some strongly connected
component, which implies that a
[topological ordering](`m:graph#topsort`) of the created graph always
exists.

# `cyclic_strong_components`
*since OTP 29.0* 

```erlang
-spec cyclic_strong_components(graph()) -> [[vertex()]].
```

Returns a list of cyclic [strongly connected
components](`m:graph#strong_components`).

Each strongly component is represented by its vertices. The order of the
vertices and the order of the components are arbitrary. Only vertices
that are included in some [cycle](`m:graph#cycle`) in `G` are returned,
otherwise the returned list is equal to that returned by
`strong_components/1`.

# `del_edge`
*since OTP 29.0* 

```erlang
-spec del_edge(G :: graph(), E :: edge()) -> graph().
```

Deletes edge `E` from graph `G`.

# `del_edges`
*since OTP 29.0* 

```erlang
-spec del_edges(graph(), [edge()]) -> graph().
```

Deletes the edges in list `Es` from graph `G`.

# `del_edges`
*since OTP 29.0* 

```erlang
-spec del_edges(graph(), vertex(), vertex()) -> graph().
```

Deletes all edges from vertex `V1` to vertex `V2` in graph `G`.

# `del_path`
*since OTP 29.0* 

```erlang
-spec del_path(graph(), vertex(), vertex()) -> graph().
```

Deletes edges from graph `G` until there are no [paths](`m:graph#path`) from
vertex `V1` to vertex `V2`.

A sketch of the procedure employed:

- Find an arbitrary [simple path](`m:graph#simple_path`)
  v\[1], v\[2], ..., v\[k] from `V1` to `V2` in `G`.
- Remove all edges of `G` [emanating](`m:graph#emanate`) from v\[i] and
  [incident](`m:graph#incident`) to v\[i+1] for 1 <= i < k (including multiple
  edges).
- Repeat until there is no path between `V1` and `V2`.

# `del_vertex`
*since OTP 29.0* 

```erlang
-spec del_vertex(graph(), vertex()) -> graph().
```

Deletes vertex `V` from graph `G`.

Any edges [emanating](`m:graph#emanate`) from `V` or
[incident](`m:graph#incident`) on `V` are also deleted.

# `del_vertices`
*since OTP 29.0* 

```erlang
-spec del_vertices(G :: graph(), Vs :: [vertex()]) -> graph().
```

Deletes the vertices in list `Vs` from graph `G`.

# `edges`
*since OTP 29.0* 

```erlang
-spec edges(G :: graph()) -> [edge()].
```

Returns a list of all edges of graph `G`, in some unspecified order.

# `edges`
*since OTP 29.0* 

```erlang
-spec edges(G :: graph(), V :: vertex()) -> [edge()].
```

Returns a list of all edges [emanating](`m:graph#emanate`) from or
[incident](`m:graph#incident`) on `V` of graph `G`, in some unspecified
order.

Edges may occur twice in the list. Use `ordsets:from_list/1` on the
result if you need to remove duplicates.

# `edges`
*since OTP 29.0* 

```erlang
-spec edges(G :: graph(), V1 :: vertex(), V2 :: vertex()) -> ordsets:ordset(edge()).
```

Returns the ordered set of edges from V1 to V2.

# `fold_vertices`
*since OTP 29.0* 

```erlang
-spec fold_vertices(G, Fun, Acc) -> any()
                       when G :: graph(), Fun :: fun((vertex(), label(), any()) -> any()), Acc :: any().
```

Fold `Fun` over the vertices of graph `G`, in some unspecified order.

# `get_cycle`
*since OTP 29.0* 

```erlang
-spec get_cycle(graph(), vertex()) -> [vertex(), ...] | false.
```

Tries to find a cycle in `G` which includes vertex `V`.

If a [simple cycle](`m:graph#simple_cycle`) of length two or more exists
through vertex `V`, the cycle is returned as a list `[V, ..., V]` of vertices.
If a [loop](`m:graph#loop`) through `V` exists, the loop is returned as a list
`[V]`. If no cycles through `V` exist, `false` is returned.

# `get_path`
*since OTP 29.0* 

```erlang
-spec get_path(graph(), vertex(), vertex()) -> [vertex(), ...] | false.
```

Tries to find a [simple path](`m:graph#simple_path`) from vertex `V1` to
vertex `V2` of graph `G`.

Returns the path as a list `[V1, ..., V2]` of vertices, or `false` if no
simple path from `V1` to `V2` of length one or more exists.

The graph is traversed in a depth-first manner, and the first found path is
returned.

# `get_short_cycle`
*since OTP 29.0* 

```erlang
-spec get_short_cycle(graph(), vertex()) -> [vertex(), ...] | false.
```

Like `get_cycle/2`, but a cycle of length one is preferred.

Tries to find an as short as possible [simple
cycle](`m:graph#simple_cycle`) through vertex `V` of graph `G`. Returns
the cycle as a list `[V, ..., V]` of vertices, or `false` if no simple
cycle through `V` exists. Notice that a [loop](`m:graph#loop`) through
`V` is returned as list `[V, V]`.

# `get_short_path`
*since OTP 29.0* 

```erlang
-spec get_short_path(graph(), vertex(), vertex()) -> [vertex(), ...] | false.
```

Like `get_path/3`, but using a breadth-first search to find a short path.

Tries to find an as short as possible [simple path](`m:graph#simple_path`)
from vertex `V1` to vertex `V2` of graph `G`. Returns the path as a list
`[V1, ..., V2]` of vertices, or `false` if no simple path from `V1` to `V2` of
length one or more exists.

Graph `G` is traversed in a breadth-first manner, and the first found path is
returned.

# `has_edge`
*since OTP 29.0* 

```erlang
-spec has_edge(G :: graph(), E :: edge()) -> boolean().
```

Returns `true` if and only if `G` contains edge `E`.

Note that the identity of an edge includes its label. To check for an
arbitrary edge between two vertices, use `has_edge/3`.

# `has_edge`
*since OTP 29.0* 

```erlang
-spec has_edge(G :: graph(), V1 :: vertex(), V2 :: vertex()) -> boolean().
```

Returns `true` if and only if `G` contains some edge from `V1` to `V2`.

# `has_path`
*since OTP 29.0* 

```erlang
-spec has_path(G :: graph(), V1 :: vertex(), V2 :: vertex()) -> boolean().
```

Returns `true` if and only if there is a [path](`m:graph#path`) in `G` from
vertex `V1` to vertex `V2`.

# `has_vertex`
*since OTP 29.0* 

```erlang
-spec has_vertex(G :: graph(), V :: vertex()) -> boolean().
```

Returns `true` if and only if `G` contains vertex `V`.

# `in_degree`
*since OTP 29.0* 

```erlang
-spec in_degree(G :: graph(), V :: vertex()) -> non_neg_integer().
```

Returns the [in-degree](`m:graph#in_degree`) of vertex `V` of graph `G`.

# `in_edges`
*since OTP 29.0* 

```erlang
-spec in_edges(G :: graph(), V :: vertex()) -> [edge()].
```

Returns a list of all edges [incident](`m:graph#incident`) on `V` of graph
`G`, in some unspecified order.

# `in_neighbours`
*since OTP 29.0* 

```erlang
-spec in_neighbours(G :: graph(), V :: vertex()) -> [vertex()].
```

Returns a list of all [in-neighbors](`m:graph#in_neighbour`) of `V` of graph
`G`, in some unspecified order.

# `info`
*since OTP 29.0* 

```erlang
-spec info(graph()) -> [{cyclicity, graph_cyclicity()}].
```

Returns a list of `{Tag, Value}` pairs describing graph `G`.

The following pairs are returned:

- `{cyclicity, Cyclicity}`, where `Cyclicity` is `cyclic` or `acyclic`,
  according to the options given to `new`.

# `is_acyclic`
*since OTP 29.0* 

```erlang
-spec is_acyclic(graph()) -> boolean().
```

Returns `true` if and only if graph `G` is
[acyclic](`m:graph#acyclic_graph`).

# `is_arborescence`
*since OTP 29.0* 

```erlang
-spec is_arborescence(graph()) -> boolean().
```

Returns `true` if and only if graph `G` is an
[arborescence](`m:graph#arborescence`) (a directed tree with a unique root).

# `is_tree`
*since OTP 29.0* 

```erlang
-spec is_tree(graph()) -> boolean().
```

Returns `true` if and only if graph `G` is a
[tree](`m:graph#tree`), considering all edges undirected.

# `loop_vertices`
*since OTP 29.0* 

```erlang
-spec loop_vertices(graph()) -> [vertex()].
```

Returns a list of all vertices of `G` that are included in some
[loop](`m:graph#loop`).

# `new`
*since OTP 29.0* 

```erlang
-spec new() -> graph().
```

# `new`
*since OTP 29.0* 

```erlang
-spec new([graph_type()]) -> graph().
```

Creates a new graph.

Returns an [empty graph](`m:graph#empty_graph`) with properties according to
the options in `Options`:

- **`cyclic`** - Allows [cycles](`m:graph#cycle`) in the graph (default).

- **`acyclic`** - The graph is to be kept [acyclic](`m:graph#acyclic_graph`).
  Attempting to add an edge that would introduce a cycle will raise an error
  `{bad_edge, {From, To}}`. Note that this slows down the adding of edges.

If an unrecognized option is specified or `Options` is not a proper list, a
`badarg` exception is raised.

# `no_edges`
*since OTP 29.0* 

```erlang
-spec no_edges(G :: graph()) -> non_neg_integer().
```

Returns the number of edges of graph `G`.

# `no_vertices`
*since OTP 29.0* 

```erlang
-spec no_vertices(G :: graph()) -> non_neg_integer().
```

Returns the number of vertices of graph `G`.

# `out_degree`
*since OTP 29.0* 

```erlang
-spec out_degree(G :: graph(), V :: vertex()) -> non_neg_integer().
```

Returns the [out-degree](`m:graph#out_degree`) of vertex `V` of graph `G`.

# `out_edges`
*since OTP 29.0* 

```erlang
-spec out_edges(G :: graph(), V :: vertex()) -> [edge()].
```

Returns a list of all edges [emanating](`m:graph#emanate`) from `V` of graph
`G`, in some unspecified order.

# `out_neighbours`
*since OTP 29.0* 

```erlang
-spec out_neighbours(G :: graph(), V :: vertex()) -> [vertex()].
```

Returns a list of all [out-neighbors](`m:graph#out_neighbour`) of `V` of
graph `G`, in some unspecified order.

# `postorder`
*since OTP 29.0* 

```erlang
-spec postorder(graph()) -> [vertex()].
```

# `postorder`
*since OTP 29.0* 

```erlang
-spec postorder(graph(), [vertex()]) -> [vertex()].
```

Returns the vertices of graph `G` reachable from `Vs`, listed in post-order.

The order is given by a [depth-first
traversal](`m:graph#depth_first_traversal`) of the graph, collecting visited
vertices in postorder. More precisely, the vertices visited while searching
from an arbitrarily chosen vertex are collected in postorder, and all those
collected vertices are placed before the subsequently visited vertices.

# `preorder`
*since OTP 29.0* 

```erlang
-spec preorder(graph()) -> [vertex()].
```

# `preorder`
*since OTP 29.0* 

```erlang
-spec preorder(graph(), [vertex()]) -> [vertex()].
```

Returns all vertices of graph `G` reachable from `Vs`, listed in pre-order.

The order is given by a [depth-first
traversal](`m:graph#depth_first_traversal`) of the graph, collecting visited
vertices in preorder.

# `reachable`
*since OTP 29.0* 

```erlang
-spec reachable(graph(), [vertex()]) -> [vertex()].
```

Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) in `G` from some vertex of `Vs` to
the vertex.

In particular, as paths can have length zero, the vertices of `Vs` are all
included in the returned list.

# `reachable_via_neighbours`
*since OTP 29.0* 

```erlang
-spec reachable_via_neighbours(graph(), [vertex()]) -> [vertex()].
```

Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) in `G` of length one or more from
some vertex of `Vs` to the vertex.

Hence, vertices in `Vs` will only be included in the result if they are part
of some [cycle](`m:graph#cycle`).

# `reaching`
*since OTP 29.0* 

```erlang
-spec reaching(graph(), [vertex()]) -> [vertex()].
```

Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) from the vertex to some vertex of
`Vs`.

In particular, as paths can have length zero, the vertices of `Vs` are all
included in the returned list.

# `reaching_via_neighbours`
*since OTP 29.0* 

```erlang
-spec reaching_via_neighbours(graph(), [vertex()]) -> [vertex()].
```

Returns an unsorted list of graph vertices such that for each vertex in the
list, there is a [path](`m:graph#path`) of length one or more from the
vertex to some vertex of `Vs`.

Hence, vertices in `Vs` will only be included in the result if they are part
of some [cycle](`m:graph#cycle`).

# `reverse_postorder`
*since OTP 29.0* 

```erlang
-spec reverse_postorder(graph()) -> [vertex()].
```

# `reverse_postorder`
*since OTP 29.0* 

```erlang
-spec reverse_postorder(graph(), [vertex()]) -> [vertex()].
```

Returns the vertices of graph `G` reachable from `Vs`, listed in reverse
post-order.

This effectively performs a topological sort of the reachable nodes.

The graph is traversed as for `postorder/2`, but producing the result in
reverse order.

# `roots`
*since OTP 29.0* 

```erlang
-spec roots(graph()) -> [vertex()].
```

Returns a minimal list of vertices of `G` from which all vertices of `G` can
be reached.

# `sink_vertices`
*since OTP 29.0* 

```erlang
-spec sink_vertices(G :: graph()) -> [vertex()].
```

Returns a list of all vertices of graph `G` with
[out-degree](`m:graph#in_degree`) zero.

# `source_vertices`
*since OTP 29.0* 

```erlang
-spec source_vertices(G :: graph()) -> [vertex()].
```

Returns a list of all vertices of graph `G` with
[in-degree](`m:graph#in_degree`) zero.

# `strong_components`
*since OTP 29.0* 

```erlang
-spec strong_components(graph()) -> [[vertex()]].
```

Returns a list of [strongly connected
components](`m:graph#strong_components`).

Each strongly component is represented by its vertices. The order of the
vertices and the order of the components are arbitrary. Each vertex of
graph `G` occurs in exactly one strong component.

# `subgraph`
*since OTP 29.0* 

```erlang
-spec subgraph(graph(), [vertex()]) -> graph().
```

# `subgraph`
*since OTP 29.0* 

```erlang
-spec subgraph(graph(), [vertex()], Options) -> graph()
                  when
                      Options :: [{type, SubgraphType} | {keep_labels, boolean()}],
                      SubgraphType :: inherit | [graph_type()].
```

Creates a maximal [subgraph](`m:graph#subgraph`) of `G` restricted to the
vertices listed in `Vs`.

If the value of option `type` is `inherit`, which is the default, the type
of `G` is used for the subgraph as well (for example, whether the graph
allows cycles). Otherwise the value of the `type` option is used as argument
to `new/1`.

If the value of option `keep_labels` is `true`, which is the default, the
[labels](`m:graph#label`) of vertices and edges of `G` are used for the
subgraph as well. If the value is `false`, the vertices and edges of the
subgraph will have the default labels.

If any of the arguments are invalid, a `badarg` exception is raised.

# `topsort`
*since OTP 29.0* 

```erlang
-spec topsort(graph()) -> [vertex()].
```

Returns a [topological ordering](`m:graph#topsort`) of the vertices of graph
`G` if such an ordering exists, otherwise `false`.

For each vertex in the returned list, no
[out-neighbors](`m:graph#out_neighbour`) occur earlier in the list.

This is currently implemented simply as `reverse_postorder(G)`, but this
detail is subject to change and should not be relied on.

# `vertex`
*since OTP 29.0* 

```erlang
-spec vertex(G :: graph(), V :: vertex()) -> label().
```

Returns the [label](`m:graph#label`) of the vertex `V` of graph `G`.

An exception is raised if `V` does not exist in `G`.

# `vertex`
*since OTP 29.0* 

```erlang
-spec vertex(G :: graph(), V :: vertex(), Default :: label()) -> label().
```

Returns the [label](`m:graph#label`) of the vertex `V` of graph `G`,
or returns `Default` if `V` does not exist in `G`.

# `vertices`
*since OTP 29.0* 

```erlang
-spec vertices(G :: graph()) -> [vertex()].
```

Returns a list of all vertices of graph `G`, in some unspecified order.

# `vertices_with_labels`
*since OTP 29.0* 

```erlang
-spec vertices_with_labels(G :: graph()) -> [{vertex(), label()}].
```

Returns a list of all pairs `{V, L}` of vertices of graph `G` and their
respective labels, in some unspecified order.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
