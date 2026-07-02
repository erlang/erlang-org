# `xmerl`
[🔗](https://github.com/erlang/otp/blob/master/lib/xmerl/src/xmerl.erl#L23)

Functions for exporting XML data to an external format.

# `callback`
*not exported* 

```erlang
-type callback() :: module() | [module()].
```

A callback module or a list of inherited callback modules.

# `element`

```erlang
-type element() :: xmlText() | xmlElement() | xmlPI() | xmlComment() | xmlDecl().
```

Normal, well-formed, XML content element.

# `simple_attribute`
*not exported* 

```erlang
-type simple_attribute() :: xmlAttribute() | {fun((_) -> _), term()} | {Key :: atom(), Value :: term()}.
```

"Simple-form" XML attribute.

# `simple_element`
*not exported* 

```erlang
-type simple_element() ::
          {Tag :: atom(),
           Attributes :: [{Name :: atom(), Value :: iolist() | atom() | integer()}],
           Content :: [simple_element()]} |
          {Tag :: atom(), Content :: [simple_element()]} |
          (Tag :: atom() | (IOString :: iolist() | element())).
```

"Simple-form" XML content element.

# `xmlAttribute`

```erlang
-type xmlAttribute() ::
          #xmlAttribute{name :: term(),
                        expanded_name :: term(),
                        nsinfo :: term(),
                        namespace :: term(),
                        parents :: term(),
                        pos :: term(),
                        language :: term(),
                        value :: term(),
                        normalized :: term()}.
```

Record `#xmlAttribute{}`.

# `xmlComment`

```erlang
-type xmlComment() :: #xmlComment{parents :: term(), pos :: term(), language :: term(), value :: term()}.
```

Record `#xmlComment{}`.

# `xmlDecl`

```erlang
-type xmlDecl() ::
          #xmlDecl{vsn :: term(), encoding :: term(), standalone :: term(), attributes :: term()}.
```

Record `#xmlDecl{}`.

# `xmlDocument`

```erlang
-type xmlDocument() :: #xmlDocument{content :: term()}.
```

Record `#xmlDocument{}`.

# `xmlElement`

```erlang
-type xmlElement() ::
          #xmlElement{name :: term(),
                      expanded_name :: term(),
                      nsinfo :: term(),
                      namespace :: term(),
                      parents :: term(),
                      pos :: term(),
                      attributes :: term(),
                      content :: term(),
                      language :: term(),
                      xmlbase :: term(),
                      elementdef :: term()}.
```

Record `#xmlElement{}`.

# `xmlNamespace`

```erlang
-type xmlNamespace() :: #xmlNamespace{default :: term(), nodes :: term()}.
```

Record `#xmlNamespace{}`.

# `xmlNsNode`

```erlang
-type xmlNsNode() :: #xmlNsNode{parents :: term(), pos :: term(), prefix :: term(), uri :: term()}.
```

Record `#xmlNsNode{}`.

# `xmlPI`

```erlang
-type xmlPI() :: #xmlPI{name :: term(), parents :: term(), pos :: term(), value :: term()}.
```

Record `#xmlPI{}`.

# `xmlText`

```erlang
-type xmlText() ::
          #xmlText{parents :: term(),
                   pos :: term(),
                   language :: term(),
                   value :: term(),
                   type :: term()}.
```

Record `#xmlText{}`.

# `callbacks`

```erlang
-spec callbacks(Module :: module()) -> [module()].
```

Find the list of inherited callback modules for a given module.

# `export`

```erlang
-spec export(Content :: term(), Callback :: term()) -> ExportedFormat :: term().
```

# `export`

```erlang
-spec export(Content, Callback, RootAttributes) -> ExportedFormat :: term()
                when
                    Content :: [Element],
                    Element :: element(),
                    Callback :: callback(),
                    RootAttributes :: [simple_attribute()].
```

Export XML content.

Exports normal, well-formed XML content, using the specified callback module.

`Element` is any of:

- `#xmlText{}`
- `#xmlElement{}`
- `#xmlPI{}`
- `#xmlComment{}`
- `#xmlDecl{}`

(See `xmerl.hrl` for the record definitions.) Text in `#xmlText{}` elements can
be deep lists of characters and/or binaries.

`RootAttributes` is a list of `#xmlAttribute{}` attributes for the `#root#`
element, which implicitly becomes the parent of the given `Content`. The
tag-handler function for `#root#` is thus called with the complete exported data
of `Content`. Root attributes can be used to specify e.g. encoding or other
metadata of an XML or HTML document.

The `Callback` module should contain hook functions for all tags present in the
data structure. A hook function must have the following format:

```text
    Tag(Data, Attributes, Parents, E)
```

where `E` is the corresponding `#xmlElement{}`, `Data` is the already-exported
contents of `E` and `Attributes` is the list of `#xmlAttribute{}` records of
`E`. Finally, `Parents` is the list of parent nodes of `E`, on the form
`[{ParentTag::atom(), ParentPosition::integer()}]`.

The hook function should return either the data to be exported, or a tuple
`{'#xml-alias#', NewTag::atom()}`, or a tuple `{'#xml-redefine#', Content}`,
where `Content` is a content list (which can be on simple-form; see
[`export_simple/2`](`export_simple/2`) for details).

A callback module can inherit definitions from other callback modules, through
the required function `'#xml-interitance#'() -> [ModuleName::atom()]`.

_See also:_ `export/2`, `export_simple/3`.

# `export_content`

```erlang
-spec export_content(Content, Callbacks) -> _ when Content :: [element()], Callbacks :: [module()].
```

Export normal XML content directly, without further context.

# `export_element`

```erlang
-spec export_element(Element, Callback) -> _ when Element :: element(), Callback :: callback().
```

Exports a normal XML element directly, without further context.

# `export_element`

```erlang
-spec export_element(Element, Callback, CallbackState) -> ExportedFormat
                        when
                            Element :: element(),
                            Callback :: callback(),
                            CallbackState :: term(),
                            ExportedFormat :: term().
```

For on-the-fly exporting during parsing (SAX style) of the XML document.

# `export_simple`

```erlang
-spec export_simple(Content, Callback) -> ExportedFormat :: term()
                       when Content :: [Element], Element :: simple_element(), Callback :: callback().
```

# `export_simple`

```erlang
-spec export_simple(Content, Callback, RootAttributes) -> ExportedFormat :: term()
                       when
                           Content :: [Element],
                           Element :: simple_element(),
                           Callback :: callback(),
                           RootAttributes :: [simple_attribute()].
```

Export "simple-form" XML content.

Exports "simple-form" XML content, using the specified callback-module.

`Element` is any of:

- `{Tag, Attributes, Content}`
- `{Tag, Content}`
- `Tag`
- `IOString`
- `#xmlText{}`
- `#xmlElement{}`
- `#xmlPI{}`
- `#xmlComment{}`
- `#xmlDecl{}`

where

- `Tag = atom()`
- `Attributes = [{Name, Value}]`
- `Name = atom()`
- `Value = IOString | atom() | integer()`

Normal-form XML elements can thus be included in the simple-form representation.
Note that content lists must be flat. An `IOString` is a (possibly deep) list of
characters and/or binaries.

`RootAttributes` is a list of:

- `XmlAttributes = #xmlAttribute{}`

See [`export/3`](`export/3`) for details on the callback module and the root
attributes. The XML-data is always converted to normal form before being passed
to the callback module.

_See also:_ `export/3`, `export_simple/2`.

# `export_simple_content`

```erlang
-spec export_simple_content(Content, Callback) -> _
                               when Content :: [simple_element()], Callback :: callback().
```

Exports simple XML content directly, without further context.

# `export_simple_element`

```erlang
-spec export_simple_element(Element, Callback) -> _
                               when Element :: simple_element(), Callback :: callback().
```

Export a simple XML element directly, without further context.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
