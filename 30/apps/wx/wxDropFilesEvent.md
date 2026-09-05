# `wxDropFilesEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDropFilesEvent.erl#L58)

This class is used for drop files events, that is, when files have been dropped onto the
window.

The window must have previously been enabled for dropping by calling `wxWindow:dragAcceptFiles/2`.

Important note: this is a separate implementation to the more general drag and drop
implementation documented in the overview_dnd. It uses the older, Windows message-based
approach of dropping files.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:dragAcceptFiles/2`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxDropFilesEvent](https://docs.wxwidgets.org/3.2/classwx_drop_files_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxDropFilesEventType` to subscribe to events of this type.

# `wxDropFiles`

```erlang
-type wxDropFiles() ::
          #wxDropFiles{type :: wxDropFilesEvent:wxDropFilesEventType(),
                       pos :: {X :: integer(), Y :: integer()},
                       files :: [unicode:chardata()]}.
```

# `wxDropFilesEvent`

```erlang
-type wxDropFilesEvent() :: wx:wx_object().
```

# `wxDropFilesEventType`

```erlang
-type wxDropFilesEventType() :: drop_files.
```

# `getFiles`

```erlang
-spec getFiles(This) -> [unicode:charlist()] when This :: wxDropFilesEvent().
```

Returns an array of filenames.

# `getNumberOfFiles`

```erlang
-spec getNumberOfFiles(This) -> integer() when This :: wxDropFilesEvent().
```

Returns the number of files dropped.

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxDropFilesEvent().
```

Returns the position at which the files were dropped.

Returns an array of filenames.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
