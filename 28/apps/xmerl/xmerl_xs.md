# `xmerl_xs`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/xmerl/src/xmerl_xs.erl#L50)

XSLT-like XML document transformations.

Erlang has similarities to XSLT since both languages have a functional
programming approach. Using `xmerl_xpath` it is possible to write XSLT like
transforms in Erlang.

XSLT stylesheets are often used when transforming XML documents, to other XML
documents or (X)HTML for presentation. XSLT contains quite many functions and
learning them all may take some effort. This document assumes a basic level of
understanding of XSLT.

Since XSLT is based on a functional programming approach with pattern matching
and recursion it is possible to write similar style sheets in Erlang. At least
for basic transforms. This document describes how to use the XPath
implementation together with Erlangs pattern matching and a couple of functions
to write XSLT like transforms.

This approach is probably easier for an Erlanger but if you need to use real
XSLT stylesheets in order to "comply to the standard" there is an adapter
available to the Sablotron XSLT package which is written i C++. See also the
[XSLT-like Transformations tutorial](`e:xmerl:xmerl_xs_examples.html`).

# `built_in_rules`

```elixir
-spec built_in_rules(Fun, E :: xmerl:element()) -> io_lib:chars()
                        when Fun :: fun((xmerl:element()) -> io_lib:chars()).
```

The default fallback behaviour.

Template funs should end with:
`template(E) -> built_in_rules(fun template/1, E)`.

# `select`

```elixir
-spec select(String, E) -> Result
                when
                    String :: term(),
                    E :: xmerl:element(),
                    Result ::
                        [xmerl:xmlElement() |
                         xmerl:xmlAttribute() |
                         xmerl:xmlText() |
                         xmerl:xmlPI() |
                         xmerl:xmlComment() |
                         xmerl:xmlNsNode() |
                         xmerl:xmlDocument()] |
                        Scalar,
                    Scalar :: #xmlObj{type :: term(), value :: term()}.
```

Extract the nodes from the xml tree according to XPath.

Equivalent to [`xmerl_xpath:string(Str, E)`](`xmerl_xpath:string/2`).

_See also:_ `value_of/1`.

# `value_of`

```elixir
-spec value_of(E) -> io_lib:chars() when E :: xmerl:element() | [xmerl:element()].
```

Concatenate all text nodes within the tree.

Example:

```text
  <xsl:template match="title">
    <div align="center">
      <h1><xsl:value-of select="." /></h1>
    </div>
  </xsl:template>

```

becomes:

```text
   template(E = #xmlElement{name='title'}) ->
     ["<div align="center"><h1>",
       value_of(select(".", E)), "</h1></div>"]

```

# `xslapply`

```elixir
-spec xslapply(Fun, ElementList) -> io_lib:chars()
                  when
                      Fun :: fun((xmerl:element()) -> io_lib:chars()),
                      ElementList :: [xmerl:element()] | xmerl:element().
```

Lookalike to xsl:apply-templates.

A wrapper to make things look similar to xsl:apply-templates.

Example, original XSLT:

```text
  <xsl:template match="doc/title">
    <h1>
      <xsl:apply-templates/>
    </h1>
  </xsl:template>

```

becomes in Erlang:

```text
  template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
    ["<h1>",
     xslapply(fun template/1, E),
     "</h1>"];

```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
