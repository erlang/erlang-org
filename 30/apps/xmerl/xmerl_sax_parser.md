# `xmerl_sax_parser`
[🔗](https://github.com/erlang/otp/blob/master/lib/xmerl/src/xmerl_sax_parser.erl#L27)

XML SAX parser API

A SAX parser for XML that sends the events through a callback interface. SAX is
the _Simple API for XML_, originally a Java-only API. SAX was the first widely
adopted API for XML in Java, and is a _de facto_ standard where there are
versions for several programming language environments other than Java.

# `continuation_fun`
*not exported* 

```erlang
-type continuation_fun() ::
          fun((continuation_state()) -> {NewBytes :: binary() | list(), continuation_state()}).
```

Callback `t:function/0` that is called whenever
the parser runs out if input data.

If the function can't get hold of more input an empty list or binary
(depends on start input in stream/2) is returned.  Other types of errors
are handled through  exceptions. Use `throw/1` to send the following tuple:
`{Tag = atom(), Reason = string()}` if the continuation function encounters
a fatal error.  [`Tag`](`t:atom/0`) is an atom that identifies
the functional entity that sends the exception and [`Reason`](`t:string/0`)
is a string that describes the problem.

# `continuation_state`
*not exported* 

```erlang
-type continuation_state() :: term().
```

# `event`
*not exported* 

```erlang
-type event() ::
          startDocument | endDocument |
          {startPrefixMapping, Prefix :: string(), Uri :: string()} |
          {endPrefixMapping, Prefix :: string()} |
          {startElement,
           Uri :: string(),
           LocalName :: string(),
           QualifiedName :: string(),
           Attributes :: string()} |
          {endElement, Uri :: string(), LocalName :: string(), QualifiedName :: string()} |
          {characters, string()} |
          {ignorableWhitespace, string()} |
          {processingInstruction, Target :: string(), Data :: string()} |
          {comment, string()} |
          startCDATA | endCDATA |
          {startDTD, Name :: string(), PublicId :: string(), SystemId :: string()} |
          endDTD |
          {startEntity, SysId :: string()} |
          {endEntity, SysId :: string()} |
          {elementDecl, Name :: string(), Model :: string()} |
          {attributeDecl,
           ElementName :: string(),
           AttributeName :: string(),
           Type :: string(),
           Mode :: string(),
           Value :: string()} |
          {internalEntityDecl, Name :: string(), Value :: string()} |
          {externalEntityDecl, Name :: string(), PublicId :: string(), SystemId :: string()} |
          {unparsedEntityDecl,
           Name :: string(),
           PublicId :: string(),
           SystemId :: string(),
           Ndata :: string()} |
          {notationDecl, Name :: string(), PublicId :: string(), SystemId :: string()}.
```

SAX events that are sent to the user via the
[event callback](`t:event_fun/0`).

- **`startDocument`** - Receive notification of the beginning of a document. The
  SAX parser will send this event only once before any other event callbacks.

- **`endDocument`** - Receive notification of the end of a document. The SAX
  parser will send this event only once, and it will be the last event during
  the parse.

- **`{startPrefixMapping, Prefix, Uri}`** - Begin the scope of a prefix-URI
  Namespace mapping. Note that `start/endPrefixMapping` events
  aren't guaranteed to be properly nested relative to each other:
  all `startPrefixMapping` events will occur immediately before
  the corresponding `startElement` event, and all `endPrefixMapping` events
  will occur immediately after the corresponding
  `endElement` event, but their order is not otherwise guaranteed.
  There will not be `start/endPrefixMapping` events for the "xml" prefix,
  since it is predeclared and immutable.

- **`{endPrefixMapping, Prefix}`** - End the scope of a prefix-URI mapping.

- **`{startElement, Uri, LocalName, QualifiedName, Attributes}`** - Receive
  notification of the beginning of an element.  The Parser will send this event
  at the beginning of every element in the XML document; there will be a
  corresponding `endElement` event for every `startElement` event
  (even when the element is empty).  All of the element's content
  will be reported, in order, before the corresponding `endElement` event.

- **`{endElement, Uri, LocalName, QualifiedName}`** - Receive notification of
  the end of an element. The SAX parser will send this event at the end of every
  element in the XML document; there will be a corresponding `startElement`
  event for every `endElement` event (even when the element is empty).

- **`{characters, string()}`** - Receive notification of character data.

- **`{ignorableWhitespace, string()}`** - Receive notification of ignorable
  whitespace in element content.

- **`{processingInstruction, Target, Data}`** - Receive notification of a
  processing instruction. The Parser will send this event once for each
  processing instruction found: note that processing instructions may occur
  before or after the main document element.

- **`{comment, string()}`** - Report an XML comment anywhere in the document
  (both inside and outside of the document element).

- **`startCDATA`** - Report the start of a CDATA section. The contents of the
  CDATA section will be reported through the regular characters event.

- **`endCDATA`** - Report the end of a CDATA section.

- **`{startDTD, Name, PublicId, SystemId}`** - Report the start of DTD
  declarations, it's reporting the start of the DOCTYPE declaration. If the
  document has no DOCTYPE declaration, this event will not be sent.

- **`endDTD`** - Report the end of DTD declarations, it's reporting the end of
  the DOCTYPE declaration.

- **`{startEntity, SysId}`** - Report the beginning of some internal and
  external XML entities.

- **`{endEntity, SysId}`** - Report the end of an entity.

- **`{elementDecl, Name, Model}`** - Report an element type declaration. The
  content model will consist of the string "EMPTY", the string "ANY", or a
  parenthesised group, optionally followed by an occurrence indicator. The model
  will be normalized so that all parameter entities are fully resolved and all
  whitespace is removed,and will include the enclosing parentheses. Other
  normalization (such as removing redundant parentheses or simplifying
  occurrence indicators) is at the discretion of the parser.

- **`{attributeDecl, ElementName, AttributeName, Type, Mode, Value}`** - Report
  an attribute type declaration.

- **`{internalEntityDecl, Name, Value}`** - Report an internal entity
  declaration.

- **`{externalEntityDecl, Name, PublicId, SystemId}`** - Report a parsed
  external entity declaration.

- **`{unparsedEntityDecl, Name, PublicId, SystemId, Ndata}`** - Receive
  notification of an unparsed entity declaration event.

- **`{notationDecl, Name, PublicId, SystemId}`** - Receive notification of a
  notation declaration event.

# `event_fun`
*not exported* 

```erlang
-type event_fun() :: fun((event(), event_location(), event_state()) -> event_state()).
```

Callback `t:function/0` that is called for every event sent by the parser.

The error handling is done through exceptions. Use `throw/1` to send
the following tuple: `{Tag = atom(), Reason = string()}` if the application
encounters a fatal error.  [`Tag`](`t:atom/0`) is an atom that identifies
the functional entity that sends the exception and [`Reason`](`t:string/0`)
is a string that describes the problem.

# `event_location`
*not exported* 

```erlang
-type event_location() :: {CurrentLocation :: string(), Entityname :: string(), LineNo :: integer()}.
```

# `event_state`
*not exported* 

```erlang
-type event_state() :: term().
```

# `latin1_binary`

```erlang
-type latin1_binary() :: unicode:latin1_binary().
```

Binary with characters encoded in iso-latin-1.

# `options`

```erlang
-type options() ::
          [{continuation_fun, continuation_fun()} |
           {continuation_state, continuation_state()} |
           {event_fun, event_fun()} |
           {event_state, event_state()} |
           {file_type, normal | dtd} |
           {encoding, utf | {utf16, big} | {utf16, little} | latin1 | list} |
           skip_external_dtd | disallow_entities |
           {entity_recurse_limit, non_neg_integer()} |
           {external_entities, all | file | none} |
           {fail_undeclared_ref, boolean()} |
           {discard_ws_before_xml_document, boolean()}].
```

Options used to customize the behaviour of the parser.

Possible options are:

- **`{continuation_fun, ContinuationFun}`** -
  [ContinuationFun](`t:continuation_fun/0`) is a callback function to decide
  what to do if the parser runs into EOF before the document is complete.

- **`{continuation_state, ContinuationState}`** - State that is accessible
  in the continuation callback function.

- **`{event_fun, EventFun}`** - [EventFun](`t:event_fun/0`) is
  the callback function for parser events.

- **`{event_state, EventState}`** - State that is accessible
  in the event callback function.

- **`{file_type, FileType}`** - Flag that tells the parser
  if it's parsing a DTD or a normal XML file (default `normal`).

- **`{encoding, Encoding}`** - Set default character set used
  (default `utf`, that is: UTF-8).  This character set is used
  only if not explicitly given by the XML document.

- **`skip_external_dtd`** - Skips the external DTD during parsing. This option
  is the same as `{external_entities, none}` and `{fail_undeclared_ref, false}`
  but just for the DTD.

- **`disallow_entities`** - Implies that parsing fails if an ENTITY declaration
  is found.

- **`{entity_recurse_limit, N}`** - Sets how many levels of recursion that is
  allowed for entities. Default is 3 levels.

- **`{external_entities, Allowed}`** - Sets which types of external entities
  that should be allowed; if not allowed it is just skipped. Default is `none`.

- **`{fail_undeclared_ref, Boolean}`** - Decides how the parser should behave
  when an undeclared reference is found. Can be useful if one has turned of
  external entities so that an external DTD is not parsed. Default is `true`.

- **`{discard_ws_before_xml_document, Boolean}`** - Discard whitespace before
  `xml` tag instead of returning a fatal error. Default is `false`.

# `unicode_binary`

```erlang
-type unicode_binary() :: binary().
```

Binary with characters encoded in UTF-8 or UTF-16.

# `unicode_char`

```erlang
-type unicode_char() :: char().
```

Integer representing valid unicode codepoint.

# `file`

```erlang
-spec file(Name, Options) -> {ok, EventState, Rest} | ErrorOrUserReturn
              when
                  Name :: file:filename(),
                  Options :: options(),
                  EventState :: event_state(),
                  Rest :: unicode_binary() | latin1_binary(),
                  ErrorOrUserReturn ::
                      {Tag, Location, Reason, EndTags, EventState} | {error, {Name, Reason}},
                  Tag :: fatal_error | atom(),
                  Location :: event_location(),
                  Reason :: term(),
                  EndTags :: term().
```

Parse a file containing an XML document.

This functions uses a default continuation function to read the file in blocks.

# `stream`

```erlang
-spec stream(Xml, Options) -> {ok, EventState, Rest} | ErrorOrUserReturn
                when
                    Xml :: unicode_binary() | latin1_binary() | [unicode_char],
                    Options :: options(),
                    EventState :: event_state(),
                    Rest :: unicode_binary() | latin1_binary(),
                    ErrorOrUserReturn :: {Tag, Location, Reason, EndTags, EventState},
                    Tag :: fatal_error | atom(),
                    Location :: event_location(),
                    Reason :: term(),
                    EndTags :: term().
```

Parse a stream containing an XML document.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
