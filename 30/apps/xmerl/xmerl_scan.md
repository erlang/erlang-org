# `xmerl_scan`
[🔗](https://github.com/erlang/otp/blob/master/lib/xmerl/src/xmerl_scan.erl#L23)

Single pass XML scanner.

This module is the interface to the XML parser, it handles XML 1.0.
The XML parser is activated through
[`xmerl_scan:string/[1,2]`](`string/1`) or
[`xmerl_scan:file/[1,2]`](`file/1`). It returns records
of the type defined in `xmerl.hrl`.

See also the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `document`

```erlang
-type document() :: xmlElement() | xmlDocument().
```

An XML document.

The document returned by [`xmerl_scan:string/[1,2]`](`string/1`) and
[`xmerl_scan:file/[1,2]`](`file/1`). The type of the returned record
depends on the value of the [`document`](`t:option_list/0`) option
passed to the function.

# `global_state`
*not exported* 

```erlang
-type global_state() :: xmerl_scanner().
```

The global state of the scanner, represented by the `#xmerl_scanner{}` record.

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
- **`{fetch_fun, Fun} | {fetch_fun, Fun, FetchState}`** - Call back function to fetch an external resource.
- **`{hook_fun, Fun} | {hook_fun, Fun, HookState}`** - Call back function to process the document entities once
   identified.
- **`{close_fun, Fun}`** - Called when document has been completely parsed.
- **`{rules, ReadFun, WriteFun, RulesState} | {rules, Rules}`** - Handles storing of scanner information when parsing.
- **`{user_state, UserState}`** - Global state variable accessible from all customization functions.
- **`{fetch_path, PathList}`** - PathList is a list of
   directories to search when fetching files. If the file in question is not in the fetch_path,
   the URI will be used as a file name.
- **`{space, Flag}`** - `preserve` (default) to preserve spaces,
  `normalize` to accumulate consecutive whitespace and replace it with one space.
- **`{line, Line}`** - To specify starting line for scanning in document which contains
  fragments of XML.
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
- **`{comments, Flag}`** - Set to `false` if Xmerl should skip comments otherwise
  they will be returned as xmlComment records (default `true`).
- **`{default_attrs, Flag}`** - Set to `true` if Xmerl should add to elements
  missing attributes with a defined default value (default `false`).
- **`{allow_entities, Flag}`** - Set to `true` if `xmerl_scan` shouldn't fail
  when there is an ENTITY declaration in the XML document (default `false`).

# `xmlDocument`
*not exported* 

```erlang
-type xmlDocument() :: xmerl:xmlDocument().
```

Record `#xmlDocument{}`.

The record definition is found in `xmerl.hrl`.

# `xmlElement`

```erlang
-type xmlElement() :: xmerl:xmlElement().
```

Record `#xmlElement{}`.

The record definition is found in `xmerl.hrl`.

# `accumulate_whitespace`

```erlang
-spec accumulate_whitespace(Text, global_state(), How, Acc) -> {NewAcc, NewText, global_state()}
                               when
                                   Text :: string(),
                                   How :: preserve | normalize,
                                   Acc :: string(),
                                   NewAcc :: string(),
                                   NewText :: string().
```

Accumulate and normalize whitespace.

# `cont_state`

```erlang
-spec cont_state(global_state()) -> ContinuationState when ContinuationState :: term().
```

Fetch the `ContinuationState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `cont_state`

```erlang
-spec cont_state(ContState :: term(), global_state()) -> global_state().
```

Set the ContinuationState, to be used in a continuation function.

The continuation function is called when the parser encounters
the end of the byte stream. See the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `event_state`

```erlang
-spec event_state(global_state()) -> EventState when EventState :: term().
```

Fetch the `EventState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `event_state`

```erlang
-spec event_state(EventState :: term(), global_state()) -> global_state().
```

Set the EventState, to be used in an event function.

The event function is called at the beginning and at the end
of a parsed entity. See the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `fetch_state`

```erlang
-spec fetch_state(global_state()) -> FetchState when FetchState :: term().
```

Fetch the `FetchState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `fetch_state`

```erlang
-spec fetch_state(FetchState :: term(), global_state()) -> global_state().
```

Set the FetchState, to be used in a fetch function.

The fetch function is and called when the parser fetches
an external resource (eg. a DTD). See the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `file`

```erlang
-spec file(Filename :: string()) -> {xmlElement(), Rest} | {error, Reason}
              when Rest :: string(), Reason :: term().
```

# `file`

```erlang
-spec file(Filename :: string(), option_list()) -> {dynamic(), Rest} | {error, Reason}
              when Rest :: string(), Reason :: term().
```

Parse a file containing an XML document

# `hook_state`

```erlang
-spec hook_state(global_state()) -> HookState when HookState :: term().
```

Fetch the `HookState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `hook_state`

```erlang
-spec hook_state(HookState :: term(), global_state()) -> global_state().
```

Set the HookState, to be used in a hook function.

The hook function is and called when the parser has parsed
a complete entity.  See the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `rules_state`

```erlang
-spec rules_state(global_state()) -> RulesState when RulesState :: term().
```

Fetch the `RulesState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `rules_state`

```erlang
-spec rules_state(RulesState :: term(), global_state()) -> global_state().
```

Set the RulesState, to be used in a rules function.

The rules function is and called when the parser store scanner information
in a rules database. See the
["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `string`

```erlang
-spec string(Text :: string()) -> {xmlElement(), Rest} when Rest :: string().
```

# `string`

```erlang
-spec string(Text :: string(), option_list()) -> {dynamic(), Rest} when Rest :: string().
```

Parse a string containing an XML document

# `user_state`

```erlang
-spec user_state(global_state()) -> UserState when UserState :: term().
```

Fetch the `UserState`.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

# `user_state`

```erlang
-spec user_state(UserState :: term(), G :: global_state()) -> global_state().
```

Set the `UserState`, to be used in a user function.

See the ["Customization functions" tutorial](`e:xmerl:xmerl_examples.html`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
