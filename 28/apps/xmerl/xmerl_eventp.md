# `xmerl_eventp`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/xmerl/src/xmerl_eventp.erl#L23)

Simple event-based processors (front-ends to `m:xmerl_scan`).

Implements processing XML documents in streams for parsing in SAX style.

Each front-end contains more elaborate settings of
`m:xmerl_scan` that makes usage of the customization functions.

# `option_list`
*not exported* 

```elixir
-type option_list() :: [{atom(), term()} | {atom(), fun(), term()} | {atom(), fun(), fun(), term()}].
```

Options allow to customize the behaviour of the scanner.  See also the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

<p>
Possible options are:
</p>
<dl>
 <dt><code>{acc_fun, Fun}</code></dt>
   <dd>Call back function to accumulate contents of entity.</dd>
 <dt><code>{continuation_fun, Fun} |
           {continuation_fun, Fun, ContinuationState}</code></dt>
   <dd>Call back function to decide what to do if the scanner runs into EOF
    before the document is complete.</dd>
 <dt><code>{event_fun, Fun} |
           {event_fun, Fun, EventState}</code></dt>
   <dd>Call back function to handle scanner events.</dd>
 <dt><code>{fetch_fun, Fun} |
           {fetch_fun, Fun, FetchState}</code></dt>
   <dd>Call back function to fetch an external resource.</dd>
 <dt><code>{hook_fun, Fun} |
           {hook_fun, Fun, HookState}</code></dt>
   <dd>Call back function to process the document entities once
    identified.</dd>
 <dt><code>{close_fun, Fun}</code></dt>
   <dd>Called when document has been completely parsed.</dd>
 <dt><code>{rules, ReadFun, WriteFun, RulesState} |
           {rules, Rules}</code></dt>
   <dd>Handles storing of scanner information when parsing.</dd>
 <dt><code>{user_state, UserState}</code></dt>
   <dd>Global state variable accessible from all customization functions</dd>

 <dt><code>{fetch_path, PathList}</code></dt>
   <dd>PathList is a list of
    directories to search when fetching files. If the file in question
    is not in the fetch_path, the URI will be used as a file
    name.</dd>
 <dt><code>{space, Flag}</code></dt>
   <dd><code>preserve</code> (default) to preserve spaces,
   <code>normalize</code> to accumulate consecutive whitespace
   and replace it with one space.</dd>
 <dt><code>{line, Line}</code></dt>
   <dd>To specify starting line for scanning in document which contains
   fragments of XML.</dd>
 <dt><code>{namespace_conformant, Flag}</code></dt>
   <dd>Controls whether to behave as a namespace conformant XML parser,
   <code>false</code> (default) to not otherwise <code>true</code>.</dd>
 <dt><code>{validation, Flag}</code></dt>
   <dd>Controls whether to process as a validating XML parser:
   <code>off</code> (default) no validation, or validation <code>dtd</code>
   by DTD or <code>schema</code> by XML Schema.
   <code>false</code> and <code>true</code> options are obsolete
   (i.e. they may be removed in a future release), if used <code>false</code>
   equals <code>off</code> and <code>true</code> equals <code>dtd</code>.</dd>
 <dt><code>{schemaLocation, [{Namespace,Link}|...]}</code></dt>
   <dd>Tells explicitly which XML Schema documents to use to validate
   the XML document. Used together with the
   <code>{validation,schema}</code> option.</dd>
 <dt><code>{quiet, Flag}</code></dt>
   <dd>Set to <code>true</code> if Xmerl should behave quietly
   and not output any information to standard output
   (default <code>false</code>).</dd>
 <dt><code>{doctype_DTD, DTD}</code></dt>
   <dd>Allows to specify DTD name when it isn't available in the XML
   document. This option has effect only together with
   <code>{validation,<code>dtd</code></code> option.</dd>
 <dt><code>{xmlbase, Dir}</code></dt>
   <dd>XML Base directory. If using string/1 default is current directory.
   If using file/1 default is directory of given file.</dd>
 <dt><code>{encoding, Enc}</code></dt>
   <dd>Set default character set used (default UTF-8).
   This character set is used only if not explicitly given by the XML
   declaration. </dd>
 <dt><code>{document, Flag}</code></dt>
   <dd>Set to <code>true</code> if Xmerl should return a complete XML document
   as an xmlDocument record (default <code>false</code>).</dd>
 <dt><code>{comments, Flag}</code></dt>
   <dd>Set to <code>false</code> if Xmerl should skip comments
   otherwise they will be returned as xmlComment records
   (default <code>true</code>).</dd>
 <dt><code>{default_attrs, Flag}</code></dt>
   <dd>Set to <code>true</code> if Xmerl should add to elements
   missing attributes with a defined default value
   (default <code>false</code>).</dd>
</dl>

# `xmlElement`
*not exported* 

```elixir
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

```elixir
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

```elixir
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

```elixir
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

```elixir
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
