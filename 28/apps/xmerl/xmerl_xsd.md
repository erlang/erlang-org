# `xmerl_xsd`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/xmerl/src/xmerl_xsd.erl#L23)

Interface module for XML Schema validation.

It handles the W3.org [specifications](http://www.w3.org/XML/Schema#dev) of
XML Schema second edition 28 october 2004.  For an introduction to
XML Schema please study [part 0](http://www.w3.org/TR/xmlschema-0/).

An XML structure is validated by [`xmerl_xsd:validate/[2,3]`](`validate/2`).

# `filename`
*not exported* 

```elixir
-type filename() :: string().
```

# `global_state`
*not exported* 

```elixir
-type global_state() :: xsd_state().
```

The global state of the validator.

It is represented by the `#xsd_state{}` record.

# `option_list`
*not exported* 

```elixir
-type option_list() :: [{xsdbase, filename()} | {atom(), term()}].
```

Options that allow to customize the behaviour of the validation.

Possible options are :

<dl>
  <dt><code>{tab2file,boolean()}</code></dt>
     <dd>Enables saving of abstract structure on file for debugging
        purpose.</dd>
  <dt><code>{xsdbase,filename()}</code></dt>
     <dd>XSD Base directory.</dd>
  <dt><code>{fetch_fun,FetchFun}</code></dt>
     <dd>Call back function to fetch an external resource.</dd>
  <dt><code>{fetch_path,PathList}</code></dt>
     <dd>PathList is a list of directories to search when fetching files.
         If the file in question is not in the fetch_path, the URI will
         be used as a file name.</dd>
  <dt><code>{state,State}</code></dt>
     <dd>It is possible by this option to provide a state with process
         information from an earlier validation.</dd>
</dl>

# `file2state`

```elixir
-spec file2state(FileName :: string()) -> {ok, State} | {error, Reason}
                    when State :: global_state(), Reason :: term().
```

Read the schema processing state from a file.

Reads the schema state with all information of the processed schema from
a file created with `state2file/[1,2]`. The format of this file is internal.
The state can then be used validating an XML document.

# `format_error`

```elixir
-spec format_error(Reasons) -> io_lib:chars() when Reasons :: [Reason :: term()] | (Reason :: term()).
```

Format error descriptions to human readable strings.

# `process_schema`

```elixir
-spec process_schema(Schema :: string()) -> _.
```

# `process_schema`

```elixir
-spec process_schema(Schema :: string(), Options :: option_list()) -> {ok, State} | {error, Reasons}
                        when State :: global_state(), Reasons :: [Reason :: term()] | (Reason :: term()).
```

Read an XML schema and check that it is valid.

Reads the referenced XML schema and checks that it is valid. Returns the
`t:global_state/0` with schema info or an error reason. The error reason
may be a list of several errors or a single error encountered
during the processing.

# `process_schemas`

```elixir
-spec process_schemas(Schemas :: list()) -> _.
```

# `process_schemas`

```elixir
-spec process_schemas(Schemas, Options) -> {ok, State} | {error, Reasons}
                         when
                             Schemas :: [{NameSpace, Schema}, ...],
                             Options :: option_list(),
                             NameSpace :: term(),
                             Schema :: string(),
                             State :: global_state(),
                             Reasons :: [Reason :: term()] | (Reason :: term()).
```

Read XML schemas and check that they are valid.

Reads the referenced XML schemas and controls they are valid. Returns the
`t:global_state/0` with schema info or an error reason. The error reason may be
a list of several errors or a single error encountered during the processing.

# `process_validate`

```elixir
-spec process_validate(Schema :: string(), Element :: xmerl:element()) -> _.
```

# `process_validate`

```elixir
-spec process_validate(Schema, Element, Options) -> Result
                          when
                              Schema :: string(),
                              Element :: xmerl:element(),
                              Options :: option_list(),
                              Result :: {ValidElement, global_state()} | {error, Reasons},
                              ValidElement :: xmerl:element(),
                              Reasons :: [Reason :: term()] | (Reason :: term()).
```

Validate a parsed well-formed XML element towards an XML schema.

Validates in two steps. First it processes the schema, saves the type and
structure info in an ETS table and then validates the element towards the
schema.

Usage example:

`1>{E,_} = xmerl_scan:file("my_XML_document.xml").`
`2>{E2,_} = xmerl_xsd:process_validate("my_XML_Schema.xsd",E).`

Observe that E2 may differ from E if for instance there are default values
defined in `my_XML_Schema.xsd`.

# `state2file`

```elixir
-spec state2file(State :: global_state()) -> ok | {error, Reason} when Reason :: term().
```

Equivalent to [`state2file(State, SchemaName)`](`state2file/2`).

`SchemaName` is the name of the schema in `State`.

# `state2file`

```elixir
-spec state2file(global_state(), FileName :: string()) -> ok | {error, Reason} when Reason :: term().
```

Save the schema processing state to a file.

Saves the schema state with all information of the processed schema in a file.
You can provide the file name for the saved state. FileName is saved with the
`.xss` extension added.

# `validate`

```elixir
-spec validate(Element :: xmerl:element(), State :: global_state()) -> _.
```

# `validate`

```elixir
-spec validate(Element, global_state(), Options) -> Result
                  when
                      Element :: xmerl:xmlElement(),
                      Options :: option_list(),
                      Result :: {ValidElement, global_state()} | {error, Reasons},
                      ValidElement :: xmerl:xmlElement(),
                      Reasons :: [ErrorReason] | ErrorReason,
                      ErrorReason :: term().
```

Validate a parsed well-formed XML element (`Element`).

A call to `validate/2` or `validate/3` must provide a well formed
parsed XML [`Element :: #xmlElement{}`](`t:xmerl:xmlElement/0`)
and a State, `t:global_state/0`, which holds necessary information
from an already processed schema.  Thus validate enables reuse
of the schema information and therefore if one shall validate
several times towards the same schema it reduces time consumption.

The result, `ValidElement`, is the valid element that conforms to the
post-schema-validation infoset. When the validator finds an error it tries to
continue and reports a list of all errors found. In those cases an unexpected
error is found it may cause a single error reason.

Usage example:
``` erlang
1>{E,_} = xmerl_scan:file("my_XML_document.xml").
2>{ok,S} = xmerl_xsd:process_schema("my_XML_Schema.xsd").
3>{E2,_} = xmerl_xsd:validate(E,S).
```

Observe that E2 may differ from E if for instance there are default values
defined in `my_XML_Schema.xsd`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
