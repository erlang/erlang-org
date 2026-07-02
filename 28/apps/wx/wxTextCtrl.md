# `wxTextCtrl`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxTextCtrl.erl#L58)

A text control allows text to be displayed and edited.

It may be single line or multi-line. Notice that a lot of methods of the text controls
are found in the base `wxTextEntry` (not implemented in wx) class which is a common base
class for `m:wxTextCtrl` and other controls using a single line text entry field (e.g. `m:wxComboBox`).

## Styles

This class supports the following styles:

* wxTE_PROCESS_ENTER: The control will generate the event `wxEVT_TEXT_ENTER` that can be
handled by the program. Otherwise, i.e. either if this style not specified at all, or it
is used, but there is no event handler for this event or the event handler called `wxEvent:skip/2` to
avoid overriding the default handling, pressing Enter key is either processed internally
by the control or used to activate the default button of the dialog, if any.

* wxTE_PROCESS_TAB: Normally, TAB key is used for keyboard navigation and pressing it in a
control switches focus to the next one. With this style, this won't happen and if the TAB
is not otherwise processed (e.g. by `wxEVT_CHAR` event handler), a literal TAB character
is inserted into the control. Notice that this style has no effect for single-line text
controls when using wxGTK.

* wxTE_MULTILINE: The text control allows multiple lines. If this style is not specified,
line break characters should not be used in the controls value.

* wxTE_PASSWORD: The text will be echoed as asterisks.

* wxTE_READONLY: The text will not be user-editable.

* wxTE_RICH: Use rich text control under MSW, this allows having more than 64KB of text in
the control. This style is ignored under other platforms.

* wxTE_RICH2: Use rich text control version 2.0 or higher under MSW, this style is ignored
under other platforms

* wxTE_AUTO_URL: Highlight the URLs and generate the wxTextUrlEvents when mouse events
occur over them.

* wxTE_NOHIDESEL: By default, the Windows text control doesn't show the selection when it
doesn't have focus - use this style to force it to always show it. It doesn't do anything
under other platforms.

* wxHSCROLL: A horizontal scrollbar will be created and used, so that text won't be
wrapped. No effect under wxGTK1.

* wxTE_NO_VSCROLL: For multiline controls only: vertical scrollbar will never be created.
This limits the amount of text which can be entered into the control to what can be
displayed in it under wxMSW but not under wxGTK or wxOSX. Currently not implemented for
the other platforms.

* wxTE_LEFT: The text in the control will be left-justified (default).

* wxTE_CENTRE: The text in the control will be centered (wxMSW, wxGTK, wxOSX).

* wxTE_RIGHT: The text in the control will be right-justified (wxMSW, wxGTK, wxOSX).

* wxTE_DONTWRAP: Same as wxHSCROLL style: don't wrap at all, show horizontal scrollbar
instead.

* wxTE_CHARWRAP: For multiline controls only: wrap the lines too long to be shown entirely
at any position (wxUniv, wxGTK, wxOSX).

* wxTE_WORDWRAP: For multiline controls only: wrap the lines too long to be shown entirely
at word boundaries (wxUniv, wxMSW, wxGTK, wxOSX).

* wxTE_BESTWRAP: For multiline controls only: wrap the lines at word boundaries or at any
other character if there are words longer than the window width (this is the default).

* wxTE_CAPITALIZE: On PocketPC and Smartphone, causes the first letter to be capitalized.
Note that alignment styles (wxTE_LEFT, wxTE_CENTRE and wxTE_RIGHT) can be changed
dynamically after control creation on wxMSW, wxGTK and wxOSX. wxTE_READONLY, wxTE_PASSWORD
and wrapping styles can be dynamically changed under wxGTK but not wxMSW. The other styles
can be only set during control creation.

wxTextCtrl Text Format

The multiline text controls always store the text as a sequence of lines separated by `'\n'`
characters, i.e. in the Unix text format even on non-Unix platforms. This allows the user
code to ignore the differences between the platforms but at a price: the indices in the
control such as those returned by `getInsertionPoint/1` or `getSelection/1` can `not` be used as indices into the string
returned by `getValue/1` as they're going to be slightly off for platforms using `"\\r\\n"` as
separator (as Windows does).

Instead, if you need to obtain a substring between the 2 indices obtained from the
control with the help of the functions mentioned above, you should use `getRange/3`. And the indices
themselves can only be passed to other methods, for example `setInsertionPoint/2` or `setSelection/3`.

To summarize: never use the indices returned by (multiline) `m:wxTextCtrl` as indices
into the string it contains, but only as arguments to be passed back to the other `m:wxTextCtrl`
methods. This problem doesn't arise for single-line platforms however where the indices
in the control do correspond to the positions in the value string.

wxTextCtrl Positions and Coordinates

It is possible to use either linear positions, i.e. roughly (but `not` always exactly, as
explained in the previous section) the index of the character in the text contained in the
control or X-Y coordinates, i.e. column and line of the character when working with this
class and it provides the functions `positionToXY/2` and `xYToPosition/3` to convert between the two.

Additionally, a position in the control can be converted to its coordinates in pixels
using `PositionToCoords()` (not implemented in wx) which can be useful to e.g. show a
popup menu near the given character. And, in the other direction, `HitTest()` (not
implemented in wx) can be used to find the character under, or near, the given pixel coordinates.

To be more precise, positions actually refer to the gaps between characters and not the
characters themselves. Thus, position 0 is the one before the very first character in the
control and so is a valid position even when the control is empty. And if the control
contains a single character, it has two valid positions: 0 before this character and 1 -
after it. This, when the documentation of various functions mentions "invalid position",
it doesn't consider the position just after the last character of the line to be invalid,
only the positions beyond that one (e.g. 2 and greater in the single character example)
are actually invalid.

wxTextCtrl Styles.

Multi-line text controls support styling, i.e. provide a possibility to set colours and
font for individual characters in it (note that under Windows `wxTE_RICH` style is
required for style support). To use the styles you can either call `setDefaultStyle/2` before inserting the
text or call `setStyle/4` later to change the style of the text already in the control (the first
solution is much more efficient).

In either case, if the style doesn't specify some of the attributes (for example you only
want to set the text colour but without changing the font nor the text background), the
values of the default style will be used for them. If there is no default style, the
attributes of the text control itself are used.

So the following code correctly describes what it does: the second call to `setDefaultStyle/2` doesn't
change the text foreground colour (which stays red) while the last one doesn't change the
background colour (which stays grey):

wxTextCtrl and C++ Streams

This class multiply-inherits from `std::streambuf` (except for some really old compilers
using non-standard iostream library), allowing code such as the following:

Note that even if your build of wxWidgets doesn't support this (the symbol `wxHAS_TEXT_WINDOW_STREAM`
has value of 0 then) you can still use `m:wxTextCtrl` itself in a stream-like manner:

However the possibility to create a `std::ostream` associated with `m:wxTextCtrl` may be
useful if you need to redirect the output of a function taking a `std::ostream` as
parameter to a text control.

Another commonly requested need is to redirect `std::cout` to the text control. This may
be done in the following way:

But wxWidgets provides a convenient class to make it even simpler so instead you may just do

See `wxStreamToTextRedirector` (not implemented in wx) for more details.

Event Handling.

The following commands are processed by default event handlers in `m:wxTextCtrl`: `wxID_CUT`, `wxID_COPY`, `wxID_PASTE`, `wxID_UNDO`, `wxID_REDO`.
The associated UI update events are also processed automatically, when the control has the focus.

See: `create/4`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTextCtrl](https://docs.wxwidgets.org/3.2/classwx_text_ctrl.html)

## Events

Event types emitted from this class:

* [`command_text_updated`](`m:wxCommandEvent`)

* [`command_text_enter`](`m:wxCommandEvent`)

* [`text_maxlen`](`m:wxCommandEvent`)

# `wxTextCtrl`

```elixir
-type wxTextCtrl() :: wx:wx_object().
```

# `appendText`

```elixir
-spec appendText(This, Text) -> ok when This :: wxTextCtrl(), Text :: unicode:chardata().
```

Appends the text to the end of the text control.

Remark: After the text is appended, the insertion point will be at the end of the text
control. If this behaviour is not desired, the programmer should use `getInsertionPoint/1` and `setInsertionPoint/2`.

See: `writeText/2`

# `canCopy`

```elixir
-spec canCopy(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if the selection can be copied to the clipboard.

# `canCut`

```elixir
-spec canCut(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if the selection can be cut to the clipboard.

# `canPaste`

```elixir
-spec canPaste(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if the contents of the clipboard can be pasted into the text control.

On some platforms (Motif, GTK) this is an approximation and returns true if the control
is editable, false otherwise.

# `canRedo`

```elixir
-spec canRedo(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if there is a redo facility available and the last operation can be redone.

# `canUndo`

```elixir
-spec canUndo(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if there is an undo facility available and the last operation can be undone.

# `changeValue`

```elixir
-spec changeValue(This, Value) -> ok when This :: wxTextCtrl(), Value :: unicode:chardata().
```

Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would return
false immediately after the call to `changeValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this function.

This functions does not generate the `wxEVT_TEXT` event but otherwise is identical to `setValue/2`.

See overview_events_prog for more information.

Since: 2.7.1

# `clear`

```elixir
-spec clear(This) -> ok when This :: wxTextCtrl().
```

Clears the text in the control.

Note that this function will generate a `wxEVT_TEXT` event, i.e. its effect is identical
to calling `SetValue`("").

# `copy`

```elixir
-spec copy(This) -> ok when This :: wxTextCtrl().
```

Copies the selected text to the clipboard.

# `create`

```elixir
-spec create(This, Parent, Id) -> boolean()
                when This :: wxTextCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```elixir
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxTextCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {value, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the text control for two-step construction.

This method should be called if the default constructor was used for the control
creation. Its parameters have the same meaning as for the non-default constructor.

# `cut`

```elixir
-spec cut(This) -> ok when This :: wxTextCtrl().
```

Copies the selected text to the clipboard and removes it from the control.

# `destroy`

```elixir
-spec destroy(This :: wxTextCtrl()) -> ok.
```

Destroys the object

# `discardEdits`

```elixir
-spec discardEdits(This) -> ok when This :: wxTextCtrl().
```

Resets the internal modified flag as if the current changes had been saved.

# `emulateKeyPress`

```elixir
-spec emulateKeyPress(This, Event) -> boolean()
                         when This :: wxTextCtrl(), Event :: wxKeyEvent:wxKeyEvent().
```

This function inserts into the control the character which would have been inserted if
the given key event had occurred in the text control.

The `event` object should be the same as the one passed to `EVT_KEY_DOWN` handler
previously by wxWidgets. Please note that this function doesn't currently work correctly
for all keys under any platform but MSW.

Return: true if the event resulted in a change to the control, false otherwise.

# `getDefaultStyle`

```elixir
-spec getDefaultStyle(This) -> wxTextAttr:wxTextAttr() when This :: wxTextCtrl().
```

Returns the style currently used for the new text.

See: `setDefaultStyle/2`

# `getInsertionPoint`

```elixir
-spec getInsertionPoint(This) -> integer() when This :: wxTextCtrl().
```

Returns the insertion point, or cursor, position.

This is defined as the zero based index of the character position to the right of the
insertion point. For example, if the insertion point is at the end of the single-line text
control, it is equal to `getLastPosition/1`.

Notice that insertion position is, in general, different from the index of the character
the cursor position at in the string returned by `getValue/1`. While this is always the case for the
single line controls, multi-line controls can use two characters `"\\r\\n"` as line
separator (this is notably the case under MSW) meaning that indices in the control and its
string value are offset by 1 for every line.

Hence to correctly get the character at the current cursor position, taking into account
that there can be none if the cursor is at the end of the string, you could do the following:

# `getLastPosition`

```elixir
-spec getLastPosition(This) -> integer() when This :: wxTextCtrl().
```

Returns the zero based index of the last position in the text control, which is equal to
the number of characters in the control.

# `getLineLength`

```elixir
-spec getLineLength(This, LineNo) -> integer() when This :: wxTextCtrl(), LineNo :: integer().
```

Gets the length of the specified line, not including any trailing newline character(s).

Return: The length of the line, or -1 if `lineNo` was invalid.

# `getLineText`

```elixir
-spec getLineText(This, LineNo) -> unicode:charlist() when This :: wxTextCtrl(), LineNo :: integer().
```

Returns the contents of a given line in the text control, not including any trailing
newline character(s).

Return: The contents of the line.

# `getNumberOfLines`

```elixir
-spec getNumberOfLines(This) -> integer() when This :: wxTextCtrl().
```

Returns the number of lines in the text control buffer.

The returned number is the number of logical lines, i.e. just the count of the number of
newline characters in the control + 1, for wxGTK and wxOSX/Cocoa ports while it is the
number of physical lines, i.e. the count of lines actually shown in the control, in wxMSW.
Because of this discrepancy, it is not recommended to use this function.

Remark: Note that even empty text controls have one line (where the insertion point is),
so `getNumberOfLines/1` never returns 0.

# `getRange`

```elixir
-spec getRange(This, From, To) -> unicode:charlist()
                  when This :: wxTextCtrl(), From :: integer(), To :: integer().
```

Returns the string containing the text starting in the positions `from` and up to `to` in
the control.

The positions must have been returned by another `m:wxTextCtrl` method. Please note that
the positions in a multiline `m:wxTextCtrl` do `not` correspond to the indices in the
string returned by `getValue/1` because of the different new line representations (`CR` or `CR` LF)
and so this method should be used to obtain the correct results instead of extracting
parts of the entire value. It may also be more efficient, especially if the control
contains a lot of data.

# `getSelection`

```elixir
-spec getSelection(This) -> {From :: integer(), To :: integer()} when This :: wxTextCtrl().
```

Gets the current selection span.

If the returned values are equal, there was no selection. Please note that the indices
returned may be used with the other `m:wxTextCtrl` methods but don't necessarily represent
the correct indices into the string returned by `getValue/1` for multiline controls under Windows (at
least,) you should use `getStringSelection/1` to get the selected text.

# `getStringSelection`

```elixir
-spec getStringSelection(This) -> unicode:charlist() when This :: wxTextCtrl().
```

Gets the text currently selected in the control.

If there is no selection, the returned string is empty.

# `getStyle`

```elixir
-spec getStyle(This, Position, Style) -> boolean()
                  when This :: wxTextCtrl(), Position :: integer(), Style :: wxTextAttr:wxTextAttr().
```

Returns the style at this position in the text control.

Not all platforms support this function.

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See:
* `setStyle/4`

* `m:wxTextAttr`

# `getValue`

```elixir
-spec getValue(This) -> unicode:charlist() when This :: wxTextCtrl().
```

Gets the contents of the control.

Notice that for a multiline text control, the lines will be separated by (Unix-style) `\n`
characters, even under Windows where they are separated by a `\r\n` sequence in the
native control.

# `isEditable`

```elixir
-spec isEditable(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if the controls contents may be edited by user (note that it always can be
changed by the program).

In other words, this functions returns true if the control hasn't been put in read-only
mode by a previous call to `setEditable/2`.

# `isModified`

```elixir
-spec isModified(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if the text has been modified by user.

Note that calling `setValue/2` doesn't make the control modified.

See: `markDirty/1`

# `isMultiLine`

```elixir
-spec isMultiLine(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if this is a multi line edit control and false otherwise.

See: `isSingleLine/1`

# `isSingleLine`

```elixir
-spec isSingleLine(This) -> boolean() when This :: wxTextCtrl().
```

Returns true if this is a single line edit control and false otherwise.

See:
* `isSingleLine/1`

* `isMultiLine/1`

# `loadFile`

```elixir
-spec loadFile(This, Filename) -> boolean() when This :: wxTextCtrl(), Filename :: unicode:chardata().
```

# `loadFile`

```elixir
-spec loadFile(This, Filename, [Option]) -> boolean()
                  when
                      This :: wxTextCtrl(),
                      Filename :: unicode:chardata(),
                      Option :: {fileType, integer()}.
```

Loads and displays the named file, if it exists.

Return: true if successful, false otherwise.

# `markDirty`

```elixir
-spec markDirty(This) -> ok when This :: wxTextCtrl().
```

Mark text as modified (dirty).

See: `isModified/1`

# `new`

```elixir
-spec new() -> wxTextCtrl().
```

Default ctor.

# `new`

```elixir
-spec new(Parent, Id) -> wxTextCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```elixir
-spec new(Parent, Id, [Option]) -> wxTextCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {value, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a text control.

Remark: The horizontal scrollbar (wxHSCROLL style flag) will only be created for
multi-line text controls. Without a horizontal scrollbar, text lines that don't fit in the
control's size will be wrapped (but no newline character is inserted). Single line
controls don't have a horizontal scrollbar, the text is automatically scrolled so that the
insertion point is always visible.

See: `create/4`

# `paste`

```elixir
-spec paste(This) -> ok when This :: wxTextCtrl().
```

Pastes text from the clipboard to the text item.

# `positionToXY`

```elixir
-spec positionToXY(This, Pos) -> Result
                      when
                          Result :: {Res :: boolean(), X :: integer(), Y :: integer()},
                          This :: wxTextCtrl(),
                          Pos :: integer().
```

Converts given position to a zero-based column, line number pair.

Return: true on success, false on failure (most likely due to a too large position parameter).

See: `xYToPosition/3`

# `redo`

```elixir
-spec redo(This) -> ok when This :: wxTextCtrl().
```

If there is a redo facility and the last operation can be redone, redoes the last
operation.

Does nothing if there is no redo facility.

# `remove`

```elixir
-spec remove(This, From, To) -> ok when This :: wxTextCtrl(), From :: integer(), To :: integer().
```

Removes the text starting at the first given position up to (but not including) the
character at the last position.

This function puts the current insertion point position at `to` as a side effect.

# `replace`

```elixir
-spec replace(This, From, To, Value) -> ok
                 when
                     This :: wxTextCtrl(),
                     From :: integer(),
                     To :: integer(),
                     Value :: unicode:chardata().
```

Replaces the text starting at the first position up to (but not including) the character
at the last position with the given text.

This function puts the current insertion point position at `to` as a side effect.

# `saveFile`

```elixir
-spec saveFile(This) -> boolean() when This :: wxTextCtrl().
```

# `saveFile`

```elixir
-spec saveFile(This, [Option]) -> boolean()
                  when
                      This :: wxTextCtrl(), Option :: {file, unicode:chardata()} | {fileType, integer()}.
```

Saves the contents of the control in a text file.

Return: true if the operation was successful, false otherwise.

# `setDefaultStyle`

```elixir
-spec setDefaultStyle(This, Style) -> boolean()
                         when This :: wxTextCtrl(), Style :: wxTextAttr:wxTextAttr().
```

Changes the default style to use for the new text which is going to be added to the
control.

This applies both to the text added programmatically using `writeText/2` or `appendText/2` and to the text entered
by the user interactively.

If either of the font, foreground, or background colour is not set in `style`, the values
of the previous default style are used for them. If the previous default style didn't set
them neither, the global font or colours of the text control itself are used as fall back.

However if the `style` parameter is the default `m:wxTextAttr`, then the default style is
just reset (instead of being combined with the new style which wouldn't change it at all).

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See: `getDefaultStyle/1`

# `setEditable`

```elixir
-spec setEditable(This, Editable) -> ok when This :: wxTextCtrl(), Editable :: boolean().
```

Makes the text item editable or read-only, overriding the `wxTE\_READONLY` flag.

See: `isEditable/1`

# `setInsertionPoint`

```elixir
-spec setInsertionPoint(This, Pos) -> ok when This :: wxTextCtrl(), Pos :: integer().
```

Sets the insertion point at the given position.

# `setInsertionPointEnd`

```elixir
-spec setInsertionPointEnd(This) -> ok when This :: wxTextCtrl().
```

Sets the insertion point at the end of the text control.

This is equivalent to calling `setInsertionPoint/2` with `getLastPosition/1` argument.

# `setMaxLength`

```elixir
-spec setMaxLength(This, Len) -> ok when This :: wxTextCtrl(), Len :: integer().
```

This function sets the maximum number of characters the user can enter into the control.

In other words, it allows limiting the text value length to `len` not counting the
terminating `NUL` character.

If `len` is 0, the previously set max length limit, if any, is discarded and the user may
enter as much text as the underlying native text control widget supports (typically at
least 32Kb). If the user tries to enter more characters into the text control when it
already is filled up to the maximal length, a `wxEVT_TEXT_MAXLEN` event is sent to notify
the program about it (giving it the possibility to show an explanatory message, for
example) and the extra input is discarded.

Note that in wxGTK this function may only be used with single line text controls.

# `setSelection`

```elixir
-spec setSelection(This, From, To) -> ok when This :: wxTextCtrl(), From :: integer(), To :: integer().
```

Selects the text starting at the first position up to (but not including) the character
at the last position.

If both parameters are equal to -1 all text in the control is selected.

Notice that the insertion point will be moved to `from` by this function.

# `setStyle`

```elixir
-spec setStyle(This, Start, End, Style) -> boolean()
                  when
                      This :: wxTextCtrl(),
                      Start :: integer(),
                      End :: integer(),
                      Style :: wxTextAttr:wxTextAttr().
```

Changes the style of the given range.

If any attribute within `style` is not set, the corresponding attribute from `getDefaultStyle/1` is used.

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See:
* `getStyle/3`

* `m:wxTextAttr`

# `setValue`

```elixir
-spec setValue(This, Value) -> ok when This :: wxTextCtrl(), Value :: unicode:chardata().
```

Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would return
false immediately after the call to `setValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this function
unless the control value doesn't change at all, in which case the insertion point is left
at its original position.

Note that, unlike most other functions changing the controls values, this function
generates a `wxEVT_TEXT` event. To avoid this you can use `changeValue/2` instead.

# `showPosition`

```elixir
-spec showPosition(This, Pos) -> ok when This :: wxTextCtrl(), Pos :: integer().
```

Makes the line containing the given position visible.

# `undo`

```elixir
-spec undo(This) -> ok when This :: wxTextCtrl().
```

If there is an undo facility and the last operation can be undone, undoes the last
operation.

Does nothing if there is no undo facility.

# `writeText`

```elixir
-spec writeText(This, Text) -> ok when This :: wxTextCtrl(), Text :: unicode:chardata().
```

Writes the text into the text control at the current insertion position.

Remark: Newlines in the text string are the only control characters allowed, and they
will cause appropriate line breaks. See operator<<() and `appendText/2` for more convenient ways of
writing to the window. After the write operation, the insertion point will be at the end
of the inserted text, so subsequent write operations will be appended. To append text
after the user may have interacted with the control, call `setInsertionPointEnd/1` before writing.

# `xYToPosition`

```elixir
-spec xYToPosition(This, X, Y) -> integer() when This :: wxTextCtrl(), X :: integer(), Y :: integer().
```

Converts the given zero based column and line number to a position.

Return: The position value, or -1 if x or y was invalid.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
