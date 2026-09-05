# `wx`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/wx.erl#L102)

A port of [wxWidgets](http://www.wxwidgets.org/).

This is the base api of [wxWidgets](http://www.wxwidgets.org/). This module
contains functions for starting and stopping the wx-server, as well as other
utility functions.

wxWidgets is object oriented, and not functional. Thus, in Wx a module
represents a class, and the object created by this class has an own type,
wxCLASS(). This module represents the base class, and all other wxMODULE's are
sub-classes of this class.

Objects of a class are created with wxCLASS:new(...) and destroyed with
wxCLASS:destroy(). Member functions are called with wxCLASS:member(Object, ...)
instead of as in C++ Object.member(...).

Sub class modules inherit (non static) functions from their parents. The
inherited functions are not documented in the sub-classes.

This erlang port of wxWidgets tries to be a one-to-one mapping with the original
wxWidgets library. Some things are different though, as the optional arguments
use property lists and can be in any order. The main difference is the event
handling which is different from the original library. See `m:wxEvtHandler`.

The following classes are implemented directly as erlang types:  
wxPoint=\{x,y\},wxSize=\{w,h\},wxRect=\{x,y,w,h\},wxColour=\{r,g,b \[,a]\},
wxString=[unicode:chardata()](`t:unicode:chardata/0`),
wxGBPosition=\{r,c\},wxGBSpan=\{rs,cs\},wxGridCellCoords=\{r,c\}.

wxWidgets uses a process specific environment, which is created by
[wx:new/0](`new/0`). To be able to use the environment from other processes,
call `get_env/0` to retrieve the environment and `set_env/1` to assign the
environment in the other process.

Global (classless) functions are located in the wx_misc module.

## DATA TYPES

- **[](){: #type-wx_colour } wx_colour() = \{R::byte(), G::byte(), B::byte()\} |
  [wx_colour4()](`m:wx#type-wx_colour4`)**

- **[](){: #type-wx_colour4 } wx_colour4() = \{R::byte(), G::byte(), B::byte(),
  A::byte()\}**

- **[](){: #type-wx_datetime } wx_datetime() = \{\{Year::integer(),
  Month::integer(), Day::integer()\}, \{Hour::integer(), Minute::integer(),
  Second::integer()\}\}**

  In Local Timezone

- **[](){: #type-wx_enum } wx_enum() = integer()**

  Constant defined in wx.hrl

- **[](){: #type-wx_env } wx_env() = #wx_env\{\}**

  Opaque process environment

- **[](){: #type-wx_memory } wx_memory() = binary() | #wx_mem\{\}**

  Opaque memory reference

- **[](){: #type-wx_object } wx_object() = #wx_ref\{\}**

  Opaque object reference

- **[](){: #type-wx_wxHtmlLinkInfo } wx_wxHtmlLinkInfo() =
  #wxHtmlLinkInfo\{href=[unicode:chardata()](`t:unicode:chardata/0`),
  target=[unicode:chardata()](`t:unicode:chardata/0`)\}**

- **[](){: #type-wx_wxMouseState } wx_wxMouseState() =
  #wxMouseState\{x=integer(), y=integer(), leftDown=boolean(),
  middleDown=boolean(), rightDown=boolean(), controlDown=boolean(),
  shiftDown=boolean(), altDown=boolean(), metaDown=boolean(),
  cmdDown=boolean()\}**

  See #wxMouseState\{\} defined in wx.hrl

# `wx_colour4`

```erlang
-type wx_colour4() :: {R :: byte(), G :: byte(), B :: byte(), A :: byte()}.
```

# `wx_colour`

```erlang
-type wx_colour() :: {R :: byte(), G :: byte(), B :: byte()} | wx_colour4().
```

# `wx_datetime`

```erlang
-type wx_datetime() ::
          {{Year :: integer(), Month :: integer(), Day :: integer()},
           {Hour :: integer(), Minute :: integer(), Second :: integer()}}.
```

# `wx_enum`

```erlang
-type wx_enum() :: integer().
```

# `wx_env`

```erlang
-type wx_env() :: #wx_env{ref :: term(), sv :: term(), debug :: term()}.
```

# `wx_memory`

```erlang
-type wx_memory() :: binary() | #wx_mem{bin :: term(), size :: term()}.
```

# `wx_object`

```erlang
-type wx_object() :: #wx_ref{ref :: term(), type :: term(), state :: term()}.
```

# `wx_wxHtmlLinkInfo`

```erlang
-type wx_wxHtmlLinkInfo() :: #wxHtmlLinkInfo{href :: unicode:chardata(), target :: unicode:chardata()}.
```

# `wx_wxMouseState`

```erlang
-type wx_wxMouseState() ::
          #wxMouseState{x :: integer(),
                        y :: integer(),
                        leftDown :: boolean(),
                        middleDown :: boolean(),
                        rightDown :: boolean(),
                        controlDown :: boolean(),
                        shiftDown :: boolean(),
                        altDown :: boolean(),
                        metaDown :: boolean(),
                        cmdDown :: boolean(),
                        aux1Down :: boolean(),
                        aux2Down :: boolean()}.
```

# `batch`

```erlang
-spec batch(function()) -> term().
```

Batches all `wx` commands used in the fun. Improves performance of the command
processing by grabbing the wxWidgets thread so that no event processing will be
done before the complete batch of commands is invoked.

_See also:_ `foldl/3`, `foldr/3`, `foreach/2`, `map/2`.

# `create_memory`

```erlang
-spec create_memory(Size :: integer()) -> wx_memory().
```

Creates a memory area (of Size in bytes) which can be used by an external
library (i.e. opengl). It is up to the client to keep a reference to this object
so it does not get garbage collected by erlang while still in use by the
external library.

This is far from erlang's intentional usage and can crash the erlang emulator.
Use it carefully.

# `debug`

```erlang
-spec debug(Level | [Level]) -> ok when Level :: none | verbose | trace | driver | integer().
```

Sets debug level. If debug level is 'verbose' or 'trace' each call is printed on
console. If Level is 'driver' each allocated object and deletion is printed on
the console.

# `demo`

```erlang
-spec demo() -> ok | {error, atom()}.
```

Starts a Wx demo if examples directory exists and is compiled

# `destroy`

```erlang
-spec destroy() -> ok.
```

Stops a wx server.

# `equal`

```erlang
-spec equal(Ref1 :: wx_object(), Ref2 :: wx_object()) -> boolean().
```

Returns true if both arguments references the same object, false otherwise

# `foldl`

```erlang
-spec foldl(function(), term(), list()) -> term().
```

Behaves like `lists:foldl/3` but batches wx commands. See `batch/1`.

# `foldr`

```erlang
-spec foldr(function(), term(), list()) -> term().
```

Behaves like `lists:foldr/3` but batches wx commands. See `batch/1`.

# `foreach`

```erlang
-spec foreach(function(), list()) -> ok.
```

Behaves like `lists:foreach/2` but batches wx commands. See `batch/1`.

# `get_env`

```erlang
-spec get_env() -> wx_env().
```

Gets this process's current wx environment. Can be sent to other processes to
allow them use this process wx environment.

_See also:_ `set_env/1`.

# `get_memory_bin`

```erlang
-spec get_memory_bin(Wx_mem :: wx_memory()) -> binary().
```

Returns the memory area as a binary.

# `getObjectType`

```erlang
-spec getObjectType(Wx_ref :: wx_object()) -> atom().
```

Returns the object type

# `is_null`

```erlang
-spec is_null(Wx_ref :: wx_object()) -> boolean().
```

Returns true if object is null, false otherwise

# `map`

```erlang
-spec map(function(), list()) -> list().
```

Behaves like `lists:map/2` but batches wx commands. See `batch/1`.

# `new`

```erlang
-spec new() -> wx_object().
```

Starts a wx server.

# `new`

```erlang
-spec new([Option]) -> wx_object() when Option :: {debug, list() | atom()} | {silent_start, boolean()}.
```

Starts a wx server.

Option may be `{debug, Level}`, see `debug/1`. Or `{silent_start, Bool}`,
which causes error messages at startup to be suppressed.
The latter can be used as a silent test of whether wx is properly installed or
not.

# `null`

```erlang
-spec null() -> wx_object().
```

Returns the null object

# `parent_class`

```erlang
-spec parent_class(Wx_ref :: wx_object()) -> boolean().
```

# `release_memory`

```erlang
-spec release_memory(Wx_mem :: wx_memory()) -> ok.
```

Releases the memory retained by `retain_memory/1`

# `retain_memory`

```erlang
-spec retain_memory(Wx_mem :: wx_memory()) -> ok.
```

Saves the memory from deletion until `release_memory/1` is called. If
`release_memory/1` is not called the memory will not be garbage collected.

# `set_env`

```erlang
-spec set_env(Wx_env :: wx_env()) -> ok.
```

Sets the process wx environment, allows this process to use another process wx
environment.

# `subscribe_events`

```erlang
-spec subscribe_events() -> ok.
```

Adds the calling process to the list of of processes that are listening to wx
application events.

At the moment these are all MacOSX specific events corresponding to
`MacNewFile()` and friends from wxWidgets
[wxApp](https://docs.wxwidgets.org/trunk/classwx_app.html):

- `{new_file, ""}`
- `{open_file, Filename}`
- `{print_file, Filename}`
- `{open_url, Url}`
- `{reopen_app, ""}`

The call always returns ok but will have sent any already received events to the
calling process.

# `typeCast`

```erlang
-spec typeCast(wx_object(), atom()) -> wx_object().
```

Casts the object to class NewType. It is needed when using functions like
wxWindow:findWindow/2, which returns a generic wxObject type.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
