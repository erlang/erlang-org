# `xmerl_xpath`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/xmerl/src/xmerl_xpath.erl#L23)

Xpath 1.0 search implementation.

The `xmerl_xpath` module handles the entire XPath 1.0 spec. XPath expressions
typically occur in XML attributes and are used to address parts of an XML
document. The grammar is defined in `xmerl_xpath_parse.yrl`. The core functions
are defined in `xmerl_xpath_pred.erl`.

Some useful shell commands for debugging the XPath parser

```text
 c(xmerl_xpath_scan).
 yecc:yecc("xmerl_xpath_parse.yrl", "xmerl_xpath_parse", true, []).
 c(xmerl_xpath_parse).

 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("position() > -1")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6 div 2")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 + 6 mod 2")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("-----6")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::node()")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("descendant-or-self::node()")).
 xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::processing-instruction('foo')")).

```

# `nodeEntity`
*not exported* 

```elixir
-type nodeEntity() ::
          xmerl:xmlElement() |
          xmerl:xmlAttribute() |
          xmerl:xmlText() |
          xmerl:xmlPI() |
          xmerl:xmlComment() |
          xmerl:xmlNsNode() |
          xmerl:xmlDocument().
```

# `option_list`
*not exported* 

```elixir
-type option_list() :: [{atom(), term()}].
```

Options to customize the behaviour of the XPath scanner.

Possible options are:

<dl>
 <dt><code>{namespace, #xmlNamespace}</code></dt>
   <dd>Set namespace nodes, from XmlNamspace, in xmlContext</dd>
 <dt><code>{namespace, Nodes}</code></dt>
   <dd>Set namespace nodes in xmlContext.</dd>
</dl>

# `parentList`
*not exported* 

```elixir
-type parentList() :: [{atom(), integer()}].
```

# `xPathString`
*not exported* 

```elixir
-type xPathString() :: string().
```

# `string`

```elixir
-spec string(String, Doc) -> [nodeEntity()] | Scalar
                when
                    String :: xPathString(),
                    Doc :: nodeEntity(),
                    Scalar :: #xmlObj{type :: term(), value :: term()}.
```

# `string`

```elixir
-spec string(String, Doc, Options) -> [nodeEntity()] | Scalar
                when
                    String :: xPathString(),
                    Doc :: nodeEntity(),
                    Options :: option_list(),
                    Scalar :: #xmlObj{type :: term(), value :: term()}.
```

# `string`

```elixir
-spec string(String, Node, Parents, Doc, Options) -> [nodeEntity()] | Scalar
                when
                    String :: xPathString(),
                    Node :: nodeEntity(),
                    Parents :: parentList(),
                    Doc :: nodeEntity(),
                    Options :: option_list(),
                    Scalar :: #xmlObj{type :: term(), value :: term()}.
```

Extract nodes from a parsed XML tree.

Extracts the nodes from the parsed XML tree according the XPath `String`.

`Scalar` is an `#xmlObj{}` record record with the fields `type` and `value`,
where `#xmlObj.type` is `boolean | number | string`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
