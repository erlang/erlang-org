# `xmerl_eventp`
[🔗](https://github.com/erlang/otp/blob/master/lib/xmerl/src/xmerl_eventp.erl#L23)

Simple event-based processors (front-ends to `m:xmerl_scan`).

Implements processing XML documents in streams for parsing in SAX style.

Each front-end contains more elaborate settings of
`m:xmerl_scan` that makes usage of the customization functions.

# `option_list`
*not exported* 

```erlang
-type option_list() :: [{atom(), term()} | {atom(), fun(), term()} | {atom(), fun(), fun(), term()}].
```

Options allow to customize the behaviour of the scanner.  See also the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

Possible options are:

- **`{acc_fun, Fun}`** - Call back function to accumulate contents of entity.
- **`{continuation_fun, Fun} | {continuation_fun, Fun, ContinuationState}`** - Call back function to decide what to do if the scanner runs into EOF
   before the document is complete.
- **`{event_fun, Fun} | {event_fun, Fun, EventState}`** - Call back function to handle scanner events.
- **`{fetch_fun, Fun} |{fetch_fun, Fun, FetchState}`** - Call back function to fetch an external resource.
- **`{hook_fun, Fun} | {hook_fun, Fun, HookState}`** - Call back function to process the document entities once
   identified.
- **`{close_fun, Fun}`** - Called when document has been completely parsed.
- **`{rules, ReadFun, WriteFun, RulesState} | {rules, Rules}`** - Handles storing of scanner information when parsing.
- **`{user_state, UserState}`** - Global state variable accessible from all customization functions
- **`{fetch_path, PathList}`** - PathList is a list of directories to search when fetching files.
  If the file in question is not in the fetch_path, the URI will be used as a file name.
- **`{space, Flag}`** - `preserve` (default) to preserve spaces,
  `normalize` to accumulate consecutive whitespace and replace it with one space.
- **`{line, Line}`** - To specify starting line for scanning in document which contains fragments of XML.
- **`{namespace_conformant, Flag}`** - Controls whether to behave as a namespace conformant XML parser,
  `false` (default) to not otherwise `true`.
- **`{validation, Flag}`** - Controls whether to process as a validating XML parser:
  `off` (default) no validation, or validation `dtd` by DTD or `schema` by XML Schema.
  `false` and `true` options are obsolete (i.e. they may be removed in a future release),
  if used `false` equals `off` and `true` equals `dtd`.
- **`{schemaLocation, [{Namespace,Link}|...]}`** - Tells explicitly which XML Schema documents to use to validate
  the XML document. Used together with the `{validation,schema}` option.
- **`{quiet, Flag}`** - Set to `true` if Xmerl should behave quietly
  and not output any information to standard output (default `false`).
- **`{doctype_DTD, DTD}`** - Allows to specify DTD name when it isn't available in the XML
  document. This option has effect only together with `{validation,dtd}` option.
- **`{xmlbase, Dir}`** - XML Base directory. If using string/1 default is current directory.
  If using file/1 default is directory of given file.
- **`{encoding, Enc}`** - Set default character set used (default UTF-8).
  This character set is used only if not explicitly given by the XML declaration. 
- **`{document, Flag}`** - Set to `true` if Xmerl should return a complete XML document
  as an xmlDocument record (default `false`).
- **`{comments, Flag}`** - Set to `false` if Xmerl should skip comments
  otherwise they will be returned as xmlComment records (default `true`).
- **`{default_attrs, Flag}`** - Set to `true` if Xmerl should add to elements
  missing attributes with a defined default value (default `false`).

# `xmlElement`
*not exported* 

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

Record `#xmlElement{}`

# `file_sax`

```erlang
-spec file_sax(Fname, CallBackModule, UserState, Options) -> NewUserState
                  when
                      Fname :: string(),
                      CallBackModule :: module(),
                      UserState :: term(),
                      Options :: option_list(),
                      NewUserState :: term().
```

Parse file containing an XML document, SAX style.

Wrapper for a call to the XML parser `m:xmerl_scan` with
a [`hook_fun`](`t:xmerl_scan:option_list/0`) for
using Xmerl export functionality directly after an entity is parsed.

# `stream`

```erlang
-spec stream(Fname, Options) -> {xmlElement(), list()} | {error, Reason}
                when Fname :: string(), Options :: option_list(), Reason :: term().
```

Parse file containing an XML document as a stream, DOM style.

Wrapper for a call to the XML parser `m:xmerl_scan` with
a [`continuation_fun`](`t:xmerl_scan:option_list/0`) for handling
streams of XML data. Note that the `continuation_fun`, `acc_fun`,
`fetch_fun`, `rules` and `close_fun` options cannot be user defined
using this parser.

# `stream_sax`

```erlang
-spec stream_sax(Fname, CallBackModule, UserState, Options) ->
                    {xmerl_scan:document(), Rest} | {error, Reason}
                    when
                        Fname :: string(),
                        CallBackModule :: module(),
                        UserState :: term(),
                        Options :: option_list(),
                        Rest :: string(),
                        Reason :: term().
```

Parse file containing an XML document as a stream, SAX style.

Wrapper for a call to the XML parser `m:xmerl_scan` with
a [`continuation_fun`](`t:xmerl_scan:option_list/0`) for handling
streams of XML data.  Note that the `continuation_fun`, `acc_fun`,
`fetch_fun`, `rules`, `hook_fun`, `close_fun` and `user_state` options
cannot be user defined using this parser.

# `string_sax`

```erlang
-spec string_sax(String, CallBackModule, UserState, Options) ->
                    {xmerl_scan:document(), Rest} | {error, Reason}
                    when
                        String :: list(),
                        CallBackModule :: module(),
                        UserState :: term(),
                        Options :: option_list(),
                        Rest :: string(),
                        Reason :: term().
```

Parse file containing an XML document, SAX style.

Wrapper for a call to the XML parser `m:xmerl_scan`
with a [`hook_fun`](`t:xmerl_scan:option_list/0`) for using
Xmerl export functionality directly after an entity is parsed.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
