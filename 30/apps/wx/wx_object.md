# `wx_object`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/wx_object.erl#L113)

wx_object - Generic wx object behaviour

This is a behaviour module that can be used for "sub classing" wx objects. It
works like a regular gen_server module and creates a server per object.

NOTE: Currently no form of inheritance is implemented.

The user module should export:

init(Args) should return  
\{wxWindow, State\} | \{wxWindow, State, Timeout\} | ignore | \{stop, Reason\}

Asynchronous window event handling:  
handle_event(#wx\{\}, State) should return  
\{noreply, State\} | \{noreply, State, Timeout\} | \{stop, Reason, State\}

The user module can export the following callback functions:

handle_call(Msg, \{From, Tag\}, State) should return  
\{reply, Reply, State\} | \{reply, Reply, State, Timeout\} | \{noreply, State\}
| \{noreply, State, Timeout\} | \{stop, Reason, Reply, State\}

handle_cast(Msg, State) should return  
\{noreply, State\} | \{noreply, State, Timeout\} | \{stop, Reason, State\}

If the above are not exported but called, the wx_object process will crash. The
user module can also export:

Info is message e.g. \{'EXIT', P, R\}, \{nodedown, N\}, ...  
handle_info(Info, State) should return , ...  
\{noreply, State\} | \{noreply, State, Timeout\} | \{stop, Reason, State\}

If a message is sent to the wx_object process when handle_info is not exported,
the message will be dropped and ignored.

When stop is returned in one of the functions above with Reason = normal |
shutdown | Term, terminate(State) is called. It lets the user module clean up,
it is always called when server terminates or when wx_object() in the driver is
deleted. If the Parent process terminates the Module:terminate/2 function is
called.  
terminate(Reason, State)

Example:

```erlang
  -module(myDialog).
  -export([new/2, show/1, destroy/1]).  %% API
  -export([init/1, handle_call/3, handle_event/2,
           handle_info/2, code_change/3, terminate/2]).
           new/2, showModal/1, destroy/1]).  %% Callbacks

  %% Client API
  new(Parent, Msg) ->
     wx_object:start(?MODULE, [Parent,Id], []).

  show(Dialog) ->
     wx_object:call(Dialog, show_modal).

  destroy(Dialog) ->
     wx_object:call(Dialog, destroy).

  %% Server Implementation ala gen_server
  init([Parent, Str]) ->
     Dialog = wxDialog:new(Parent, 42, "Testing", []),
     ...
     wxDialog:connect(Dialog, command_button_clicked),
     {Dialog, MyState}.

  handle_call(show, _From, State) ->
     wxDialog:show(State#state.win),
     {reply, ok, State};
  ...
  handle_event(#wx{}, State) ->
     io:format("Users clicked button~n",[]),
     {noreply, State};
  ...
```

## DATA TYPES

- **[](){: #type-request_id } request_id() = term()**

- **[](){: #type-server_ref } server_ref() =
  [wx:wx_object()](`m:wx#type-wx_object`) | atom() | pid()**

# `event`
*not exported* 

```erlang
-type event() ::
          wxActivateEvent:wxActivate() |
          wxAuiManagerEvent:wxAuiManager() |
          wxAuiNotebookEvent:wxAuiNotebook() |
          wxBookCtrlEvent:wxBookCtrl() |
          wxCalendarEvent:wxCalendar() |
          wxChildFocusEvent:wxChildFocus() |
          wxClipboardTextEvent:wxClipboardText() |
          wxCloseEvent:wxClose() |
          wxColourPickerEvent:wxColourPicker() |
          wxCommandEvent:wxCommand() |
          wxContextMenuEvent:wxContextMenu() |
          wxDateEvent:wxDate() |
          wxDisplayChangedEvent:wxDisplayChanged() |
          wxDropFilesEvent:wxDropFiles() |
          wxEraseEvent:wxErase() |
          wxFileDirPickerEvent:wxFileDirPicker() |
          wxFocusEvent:wxFocus() |
          wxFontPickerEvent:wxFontPicker() |
          wxGridEvent:wxGrid() |
          wxHelpEvent:wxHelp() |
          wxHtmlLinkEvent:wxHtmlLink() |
          wxIconizeEvent:wxIconize() |
          wxIdleEvent:wxIdle() |
          wxInitDialogEvent:wxInitDialog() |
          wxJoystickEvent:wxJoystick() |
          wxKeyEvent:wxKey() |
          wxListEvent:wxList() |
          wxMaximizeEvent:wxMaximize() |
          wxMenuEvent:wxMenu() |
          wxMouseCaptureChangedEvent:wxMouseCaptureChanged() |
          wxMouseCaptureLostEvent:wxMouseCaptureLost() |
          wxMouseEvent:wxMouse() |
          wxMoveEvent:wxMove() |
          wxNavigationKeyEvent:wxNavigationKey() |
          wxPaintEvent:wxPaint() |
          wxPaletteChangedEvent:wxPaletteChanged() |
          wxQueryNewPaletteEvent:wxQueryNewPalette() |
          wxSashEvent:wxSash() |
          wxScrollEvent:wxScroll() |
          wxScrollWinEvent:wxScrollWin() |
          wxSetCursorEvent:wxSetCursor() |
          wxShowEvent:wxShow() |
          wxSizeEvent:wxSize() |
          wxSpinEvent:wxSpin() |
          wxSplitterEvent:wxSplitter() |
          wxStyledTextEvent:wxStyledText() |
          wxSysColourChangedEvent:wxSysColourChanged() |
          wxTaskBarIconEvent:wxTaskBarIcon() |
          wxTreeEvent:wxTree() |
          wxUpdateUIEvent:wxUpdateUI() |
          wxWebViewEvent:wxWebView() |
          wxWindowCreateEvent:wxWindowCreate() |
          wxWindowDestroyEvent:wxWindowDestroy().
```

# `request_id`
*not exported* 

```erlang
-type request_id() :: term().
```

# `server_ref`
*not exported* 

```erlang
-type server_ref() :: Obj :: wx:wx_object() | atom() | pid().
```

# `code_change`
*optional* 

```erlang
-callback code_change(OldVsn :: term() | {down, term()}, State :: term(), Extra :: term()) ->
                         {ok, NewState :: term()} | {error, Reason :: term()}.
```

# `handle_call`
*optional* 

```erlang
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
```

# `handle_cast`
*optional* 

```erlang
-callback handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
```

# `handle_event`

```erlang
-callback handle_event(Request ::
                           #wx{id :: integer(), obj :: wx:wx_object(), userData :: term(), event :: event()},
                       State :: term()) ->
                          {noreply, NewState :: term()} |
                          {noreply, NewState :: term(), timeout() | hibernate} |
                          {stop, Reason :: term(), NewState :: term()}.
```

# `handle_info`
*optional* 

```erlang
-callback handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), timeout() | hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
```

# `handle_sync_event`
*optional* 

```erlang
-callback handle_sync_event(Request ::
                                #wx{id :: integer(),
                                    obj :: wx:wx_object(),
                                    userData :: term(),
                                    event :: event()},
                            Ref :: #wx_ref{ref :: term(), type :: term(), state :: term()},
                            State :: term()) ->
                               ok.
```

# `init`

```erlang
-callback init(Args :: term()) ->
                  {#wx_ref{ref :: term(), type :: term(), state :: term()}, State :: term()} |
                  {#wx_ref{ref :: term(), type :: term(), state :: term()},
                   State :: term(),
                   timeout() | hibernate} |
                  {stop, Reason :: term()} |
                  ignore.
```

# `terminate`
*optional* 

```erlang
-callback terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: term()) -> term().
```

# `call`

```erlang
-spec call(Obj, Request) -> term() when Obj :: wx:wx_object() | atom() | pid(), Request :: term().
```

Make a call to a wx_object server. The call waits until it gets a result.
Invokes handle_call(Request, From, State) in the server

# `call`

```erlang
-spec call(Obj, Request, Timeout) -> term()
              when Obj :: wx:wx_object() | atom() | pid(), Request :: term(), Timeout :: integer().
```

Make a call to a wx_object server with a timeout. Invokes handle_call(Request,
From, State) in server

# `cast`

```erlang
-spec cast(Obj, Request) -> ok when Obj :: wx:wx_object() | atom() | pid(), Request :: term().
```

Make a cast to a wx_object server. Invokes handle_cast(Request, State) in the
server

# `check_response`

```erlang
-spec check_response(Msg :: term(), Key :: request_id()) ->
                        {reply, Reply :: term()} | false | {error, {term(), server_ref()}}.
```

Check if a received message was a reply to a RequestId

# `get_pid`

```erlang
-spec get_pid(Obj) -> pid() when Obj :: wx:wx_object() | atom() | pid().
```

Get the pid of the object handle.

# `reply`

```erlang
-spec reply({pid(), Tag :: term()}, Reply :: term()) -> pid().
```

Get the pid of the object handle.

# `send_request`

```erlang
-spec send_request(Obj, Request :: term()) -> request_id() when Obj :: wx:wx_object() | atom() | pid().
```

Make an send_request to a generic server. and return a RequestId which
can/should be used with wait_response/\[1|2]. Invokes handle_call(Request, From,
State) in server.

# `set_pid`

```erlang
-spec set_pid(Obj, Pid :: pid()) -> wx:wx_object() when Obj :: wx:wx_object() | atom() | pid().
```

Sets the controlling process of the object handle.

# `start`

```erlang
-spec start(Name, Mod, Args, Options) -> wxWindow:wxWindow() | {error, term()}
               when
                   Name :: {local, atom()},
                   Mod :: atom(),
                   Args :: term(),
                   Flag :: trace | log | {logfile, string()} | statistics | debug,
                   Options :: [{timeout, timeout()} | {debug, [Flag]}].
```

Starts a generic wx_object server and invokes Mod:init(Args) in the new process.

# `start_link`

```erlang
-spec start_link(Mod, Args, Options) -> wxWindow:wxWindow() | {error, term()}
                    when
                        Mod :: atom(),
                        Args :: term(),
                        Flag :: trace | log | {logfile, string()} | statistics | debug,
                        Options :: [{timeout, timeout()} | {debug, [Flag]}].
```

Starts a generic wx_object server and invokes Mod:init(Args) in the new process.

# `start_link`

```erlang
-spec start_link(Name, Mod, Args, Options) -> wxWindow:wxWindow() | {error, term()}
                    when
                        Name :: {local, atom()},
                        Mod :: atom(),
                        Args :: term(),
                        Flag :: trace | log | {logfile, string()} | statistics | debug,
                        Options :: [{timeout, timeout()} | {debug, [Flag]}].
```

Starts a generic wx_object server and invokes Mod:init(Args) in the new process.

# `stop`

```erlang
-spec stop(Obj) -> ok when Obj :: wx:wx_object() | atom() | pid().
```

Stops a generic wx_object server with reason 'normal'. Invokes
terminate(Reason,State) in the server. The call waits until the process is
terminated. If the process does not exist, an exception is raised.

# `stop`

```erlang
-spec stop(Obj, Reason, Timeout) -> ok
              when Obj :: wx:wx_object() | atom() | pid(), Reason :: term(), Timeout :: timeout().
```

Stops a generic wx_object server with the given Reason. Invokes
terminate(Reason,State) in the server. The call waits until the process is
terminated. If the call times out, or if the process does not exist, an
exception is raised.

# `wait_response`

```erlang
-spec wait_response(RequestId :: request_id()) ->
                       {reply, Reply :: term()} | {error, {term(), server_ref()}}.
```

Wait infinitely for a reply from a generic server.

# `wait_response`

```erlang
-spec wait_response(Key :: request_id(), Timeout :: timeout()) ->
                       {reply, Reply :: term()} | timeout | {error, {term(), server_ref()}}.
```

Wait 'timeout' for a reply from a generic server.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
