# `wxStyledTextCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStyledTextCtrl.erl#L58)

A wxWidgets implementation of the Scintilla source code editing component.

As well as features found in standard text editing components, Scintilla includes
features especially useful when editing and debugging source code. These include support
for syntax styling, error indicators, code completion and call tips.

The selection margin can contain markers like those used in debuggers to indicate
breakpoints and the current line. Styling choices are more open than with many editors,
allowing the use of proportional fonts, bold and italics, multiple foreground and
background colours and multiple fonts.

`m:wxStyledTextCtrl` is a 1 to 1 mapping of "raw" scintilla interface, whose
documentation can be found in the Scintilla website ([http://www.scintilla.org/](http://www.scintilla.org/)).

Please see `m:wxStyledTextEvent` for the documentation of all event types you can use
with `m:wxStyledTextCtrl`.

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStyledTextCtrl](https://docs.wxwidgets.org/3.2/classwx_styled_text_ctrl.html)

# `wxStyledTextCtrl`

```erlang
-type wxStyledTextCtrl() :: wx:wx_object().
```

# `addText`

```erlang
-spec addText(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Add text to the document at current position.

# `addTextRaw`

```erlang
-spec addTextRaw(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: binary().
```

# `addTextRaw`

```erlang
-spec addTextRaw(This, Text, [Option]) -> ok
                    when This :: wxStyledTextCtrl(), Text :: binary(), Option :: {length, integer()}.
```

Add text to the document at current position.

# `allocate`

```erlang
-spec allocate(This, Bytes) -> ok when This :: wxStyledTextCtrl(), Bytes :: integer().
```

Enlarge the document to a particular size of text bytes.

# `appendText`

```erlang
-spec appendText(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Append a string to the end of the document without changing the selection.

# `appendTextRaw`

```erlang
-spec appendTextRaw(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: binary().
```

# `appendTextRaw`

```erlang
-spec appendTextRaw(This, Text, [Option]) -> ok
                       when This :: wxStyledTextCtrl(), Text :: binary(), Option :: {length, integer()}.
```

Append a string to the end of the document without changing the selection.

# `autoCompActive`

```erlang
-spec autoCompActive(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is there an auto-completion list visible?

# `autoCompCancel`

```erlang
-spec autoCompCancel(This) -> ok when This :: wxStyledTextCtrl().
```

Remove the auto-completion list from the screen.

# `autoCompComplete`

```erlang
-spec autoCompComplete(This) -> ok when This :: wxStyledTextCtrl().
```

User has selected an item so remove the list and insert the selection.

# `autoCompGetAutoHide`

```erlang
-spec autoCompGetAutoHide(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether or not autocompletion is hidden automatically when nothing matches.

# `autoCompGetCancelAtStart`

```erlang
-spec autoCompGetCancelAtStart(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether auto-completion cancelled by backspacing before start.

# `autoCompGetChooseSingle`

```erlang
-spec autoCompGetChooseSingle(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether a single item auto-completion list automatically choose the item.

# `autoCompGetCurrent`

```erlang
-spec autoCompGetCurrent(This) -> integer() when This :: wxStyledTextCtrl().
```

Get currently selected item position in the auto-completion list.

# `autoCompGetDropRestOfWord`

```erlang
-spec autoCompGetDropRestOfWord(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether or not autocompletion deletes any word characters after the inserted
text upon completion.

# `autoCompGetIgnoreCase`

```erlang
-spec autoCompGetIgnoreCase(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve state of ignore case flag.

# `autoCompGetMaxHeight`

```erlang
-spec autoCompGetMaxHeight(This) -> integer() when This :: wxStyledTextCtrl().
```

Set the maximum height, in rows, of auto-completion and user lists.

# `autoCompGetMaxWidth`

```erlang
-spec autoCompGetMaxWidth(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the maximum width, in characters, of auto-completion and user lists.

# `autoCompGetSeparator`

```erlang
-spec autoCompGetSeparator(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the auto-completion list separator character.

# `autoCompGetTypeSeparator`

```erlang
-spec autoCompGetTypeSeparator(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the auto-completion list type-separator character.

# `autoCompPosStart`

```erlang
-spec autoCompPosStart(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the position of the caret when the auto-completion list was displayed.

# `autoCompSelect`

```erlang
-spec autoCompSelect(This, Select) -> ok when This :: wxStyledTextCtrl(), Select :: unicode:chardata().
```

Select the item in the auto-completion list that starts with a string.

# `autoCompSetAutoHide`

```erlang
-spec autoCompSetAutoHide(This, AutoHide) -> ok when This :: wxStyledTextCtrl(), AutoHide :: boolean().
```

Set whether or not autocompletion is hidden automatically when nothing matches.

# `autoCompSetCancelAtStart`

```erlang
-spec autoCompSetCancelAtStart(This, Cancel) -> ok when This :: wxStyledTextCtrl(), Cancel :: boolean().
```

Should the auto-completion list be cancelled if the user backspaces to a position before
where the box was created.

# `autoCompSetChooseSingle`

```erlang
-spec autoCompSetChooseSingle(This, ChooseSingle) -> ok
                                 when This :: wxStyledTextCtrl(), ChooseSingle :: boolean().
```

Should a single item auto-completion list automatically choose the item.

# `autoCompSetDropRestOfWord`

```erlang
-spec autoCompSetDropRestOfWord(This, DropRestOfWord) -> ok
                                   when This :: wxStyledTextCtrl(), DropRestOfWord :: boolean().
```

Set whether or not autocompletion deletes any word characters after the inserted text
upon completion.

# `autoCompSetFillUps`

```erlang
-spec autoCompSetFillUps(This, CharacterSet) -> ok
                            when This :: wxStyledTextCtrl(), CharacterSet :: unicode:chardata().
```

Define a set of characters that when typed will cause the autocompletion to choose the
selected item.

# `autoCompSetIgnoreCase`

```erlang
-spec autoCompSetIgnoreCase(This, IgnoreCase) -> ok
                               when This :: wxStyledTextCtrl(), IgnoreCase :: boolean().
```

Set whether case is significant when performing auto-completion searches.

# `autoCompSetMaxHeight`

```erlang
-spec autoCompSetMaxHeight(This, RowCount) -> ok when This :: wxStyledTextCtrl(), RowCount :: integer().
```

Set the maximum height, in rows, of auto-completion and user lists.

The default is 5 rows.

# `autoCompSetMaxWidth`

```erlang
-spec autoCompSetMaxWidth(This, CharacterCount) -> ok
                             when This :: wxStyledTextCtrl(), CharacterCount :: integer().
```

Set the maximum width, in characters, of auto-completion and user lists.

Set to 0 to autosize to fit longest item, which is the default.

# `autoCompSetSeparator`

```erlang
-spec autoCompSetSeparator(This, SeparatorCharacter) -> ok
                              when This :: wxStyledTextCtrl(), SeparatorCharacter :: integer().
```

Change the separator character in the string setting up an auto-completion list.

Default is space but can be changed if items contain space.

# `autoCompSetTypeSeparator`

```erlang
-spec autoCompSetTypeSeparator(This, SeparatorCharacter) -> ok
                                  when This :: wxStyledTextCtrl(), SeparatorCharacter :: integer().
```

Change the type-separator character in the string setting up an auto-completion list.

Default is '?' but can be changed if items contain '?'.

# `autoCompShow`

```erlang
-spec autoCompShow(This, LengthEntered, ItemList) -> ok
                      when
                          This :: wxStyledTextCtrl(),
                          LengthEntered :: integer(),
                          ItemList :: unicode:chardata().
```

Display a auto-completion list.

The lengthEntered parameter indicates how many characters before the caret should be used
to provide context.

# `autoCompStops`

```erlang
-spec autoCompStops(This, CharacterSet) -> ok
                       when This :: wxStyledTextCtrl(), CharacterSet :: unicode:chardata().
```

Define a set of character that when typed cancel the auto-completion list.

# `backTab`

```erlang
-spec backTab(This) -> ok when This :: wxStyledTextCtrl().
```

Dedent the selected lines.

# `beginUndoAction`

```erlang
-spec beginUndoAction(This) -> ok when This :: wxStyledTextCtrl().
```

Start a sequence of actions that is undone and redone as a unit.

May be nested.

# `braceBadLight`

```erlang
-spec braceBadLight(This, Pos) -> ok when This :: wxStyledTextCtrl(), Pos :: integer().
```

Highlight the character at a position indicating there is no matching brace.

# `braceHighlight`

```erlang
-spec braceHighlight(This, PosA, PosB) -> ok
                        when This :: wxStyledTextCtrl(), PosA :: integer(), PosB :: integer().
```

Highlight the characters at two positions.

# `braceMatch`

```erlang
-spec braceMatch(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Find the position of a matching brace or wxSTC_INVALID_POSITION if no match.

# `callTipActive`

```erlang
-spec callTipActive(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is there an active call tip?

# `callTipCancel`

```erlang
-spec callTipCancel(This) -> ok when This :: wxStyledTextCtrl().
```

Remove the call tip from the screen.

# `callTipPosAtStart`

```erlang
-spec callTipPosAtStart(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the position where the caret was before displaying the call tip.

Since: 3.1.0

# `callTipSetBackground`

```erlang
-spec callTipSetBackground(This, Back) -> ok when This :: wxStyledTextCtrl(), Back :: wx:wx_colour().
```

Set the background colour for the call tip.

# `callTipSetForeground`

```erlang
-spec callTipSetForeground(This, Fore) -> ok when This :: wxStyledTextCtrl(), Fore :: wx:wx_colour().
```

Set the foreground colour for the call tip.

# `callTipSetForegroundHighlight`

```erlang
-spec callTipSetForegroundHighlight(This, Fore) -> ok
                                       when This :: wxStyledTextCtrl(), Fore :: wx:wx_colour().
```

Set the foreground colour for the highlighted part of the call tip.

# `callTipSetHighlight`

```erlang
-spec callTipSetHighlight(This, HighlightStart, HighlightEnd) -> ok
                             when
                                 This :: wxStyledTextCtrl(),
                                 HighlightStart :: integer(),
                                 HighlightEnd :: integer().
```

Highlight a segment of the definition.

# `callTipShow`

```erlang
-spec callTipShow(This, Pos, Definition) -> ok
                     when This :: wxStyledTextCtrl(), Pos :: integer(), Definition :: unicode:chardata().
```

Show a call tip containing a definition near position pos.

# `callTipUseStyle`

```erlang
-spec callTipUseStyle(This, TabSize) -> ok when This :: wxStyledTextCtrl(), TabSize :: integer().
```

Enable use of wxSTC_STYLE_CALLTIP and set call tip tab size in pixels.

# `cancel`

```erlang
-spec cancel(This) -> ok when This :: wxStyledTextCtrl().
```

Cancel any modes such as call tip or auto-completion list display.

# `canPaste`

```erlang
-spec canPaste(This) -> boolean() when This :: wxStyledTextCtrl().
```

Will a paste succeed?

# `canRedo`

```erlang
-spec canRedo(This) -> boolean() when This :: wxStyledTextCtrl().
```

Are there any redoable actions in the undo history?

# `canUndo`

```erlang
-spec canUndo(This) -> boolean() when This :: wxStyledTextCtrl().
```

Are there any undoable actions in the undo history?

# `charLeft`

```erlang
-spec charLeft(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one character.

# `charLeftExtend`

```erlang
-spec charLeftExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one character extending selection to new caret position.

# `charLeftRectExtend`

```erlang
-spec charLeftRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one character, extending rectangular selection to new caret position.

# `charRight`

```erlang
-spec charRight(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one character.

# `charRightExtend`

```erlang
-spec charRightExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one character extending selection to new caret position.

# `charRightRectExtend`

```erlang
-spec charRightRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one character, extending rectangular selection to new caret position.

# `chooseCaretX`

```erlang
-spec chooseCaretX(This) -> ok when This :: wxStyledTextCtrl().
```

Set the last x chosen value to be the caret x position.

# `clear`

```erlang
-spec clear(This) -> ok when This :: wxStyledTextCtrl().
```

Clear the selection.

# `clearAll`

```erlang
-spec clearAll(This) -> ok when This :: wxStyledTextCtrl().
```

Delete all text in the document.

# `clearDocumentStyle`

```erlang
-spec clearDocumentStyle(This) -> ok when This :: wxStyledTextCtrl().
```

Set all style bytes to 0, remove all folding information.

# `clearRegisteredImages`

```erlang
-spec clearRegisteredImages(This) -> ok when This :: wxStyledTextCtrl().
```

Clear all the registered images.

# `cmdKeyAssign`

```erlang
-spec cmdKeyAssign(This, Key, Modifiers, Cmd) -> ok
                      when
                          This :: wxStyledTextCtrl(),
                          Key :: integer(),
                          Modifiers :: integer(),
                          Cmd :: integer().
```

When key+modifier combination keyDefinition is pressed perform sciCommand.

The second argument should be a bit list containing one or more of the ?wxSTC\_KEYMOD\_\*
constants and the third argument should be one of the ?wxSTC\_CMD\_\* constants.

# `cmdKeyClear`

```erlang
-spec cmdKeyClear(This, Key, Modifiers) -> ok
                     when This :: wxStyledTextCtrl(), Key :: integer(), Modifiers :: integer().
```

When key+modifier combination keyDefinition is pressed do nothing.

The second argument should be a bit list containing one or more of the ?wxSTC\_KEYMOD\_\*
constants.

# `cmdKeyClearAll`

```erlang
-spec cmdKeyClearAll(This) -> ok when This :: wxStyledTextCtrl().
```

Drop all key mappings.

# `cmdKeyExecute`

```erlang
-spec cmdKeyExecute(This, Cmd) -> ok when This :: wxStyledTextCtrl(), Cmd :: integer().
```

Perform one of the operations defined by the wxSTC_CMD_* constants.

# `colourise`

```erlang
-spec colourise(This, Start, End) -> ok
                   when This :: wxStyledTextCtrl(), Start :: integer(), End :: integer().
```

Colourise a segment of the document using the current lexing language.

# `convertEOLs`

```erlang
-spec convertEOLs(This, EolMode) -> ok when This :: wxStyledTextCtrl(), EolMode :: integer().
```

Convert all line endings in the document to one mode.

# `copy`

```erlang
-spec copy(This) -> ok when This :: wxStyledTextCtrl().
```

Copy the selection to the clipboard.

# `copyRange`

```erlang
-spec copyRange(This, Start, End) -> ok
                   when This :: wxStyledTextCtrl(), Start :: integer(), End :: integer().
```

Copy a range of text to the clipboard.

Positions are clipped into the document.

# `copyText`

```erlang
-spec copyText(This, Length, Text) -> ok
                  when This :: wxStyledTextCtrl(), Length :: integer(), Text :: unicode:chardata().
```

Copy argument text to the clipboard.

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxStyledTextCtrl(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxStyledTextCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Create the UI elements for a STC that was created with the default ctor.

(For 2-phase create.)

# `cut`

```erlang
-spec cut(This) -> ok when This :: wxStyledTextCtrl().
```

Cut the selection to the clipboard.

# `deleteBack`

```erlang
-spec deleteBack(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the selection or if no selection, the character before the caret.

# `deleteBackNotLine`

```erlang
-spec deleteBackNotLine(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the selection or if no selection, the character before the caret.

Will not delete the character before at the start of a line.

# `delLineLeft`

```erlang
-spec delLineLeft(This) -> ok when This :: wxStyledTextCtrl().
```

Delete back from the current position to the start of the line.

# `delLineRight`

```erlang
-spec delLineRight(This) -> ok when This :: wxStyledTextCtrl().
```

Delete forwards from the current position to the end of the line.

# `delWordLeft`

```erlang
-spec delWordLeft(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the word to the left of the caret.

# `delWordRight`

```erlang
-spec delWordRight(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the word to the right of the caret.

# `destroy`

```erlang
-spec destroy(This :: wxStyledTextCtrl()) -> ok.
```

Destroys the object

# `docLineFromVisible`

```erlang
-spec docLineFromVisible(This, DisplayLine) -> integer()
                            when This :: wxStyledTextCtrl(), DisplayLine :: integer().
```

Find the document line of a display line taking hidden lines into account.

# `documentEnd`

```erlang
-spec documentEnd(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position in document.

# `documentEndExtend`

```erlang
-spec documentEndExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position in document extending selection to new caret position.

# `documentStart`

```erlang
-spec documentStart(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position in document.

# `documentStartExtend`

```erlang
-spec documentStartExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position in document extending selection to new caret position.

# `doDragOver`

```erlang
-spec doDragOver(This, X, Y, DefaultRes) -> wx:wx_enum()
                    when
                        This :: wxStyledTextCtrl(),
                        X :: integer(),
                        Y :: integer(),
                        DefaultRes :: wx:wx_enum().
```

Allow for simulating a DnD DragOver.

# `doDropText`

```erlang
-spec doDropText(This, X, Y, Data) -> boolean()
                    when
                        This :: wxStyledTextCtrl(),
                        X :: integer(),
                        Y :: integer(),
                        Data :: unicode:chardata().
```

Allow for simulating a DnD DropText.

# `editToggleOvertype`

```erlang
-spec editToggleOvertype(This) -> ok when This :: wxStyledTextCtrl().
```

Switch from insert to overtype mode or the reverse.

# `emptyUndoBuffer`

```erlang
-spec emptyUndoBuffer(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the undo history.

# `endUndoAction`

```erlang
-spec endUndoAction(This) -> ok when This :: wxStyledTextCtrl().
```

End a sequence of actions that is undone and redone as a unit.

# `ensureCaretVisible`

```erlang
-spec ensureCaretVisible(This) -> ok when This :: wxStyledTextCtrl().
```

Ensure the caret is visible.

# `ensureVisible`

```erlang
-spec ensureVisible(This, Line) -> ok when This :: wxStyledTextCtrl(), Line :: integer().
```

Ensure a particular line is visible by expanding any header line hiding it.

# `ensureVisibleEnforcePolicy`

```erlang
-spec ensureVisibleEnforcePolicy(This, Line) -> ok when This :: wxStyledTextCtrl(), Line :: integer().
```

Ensure a particular line is visible by expanding any header line hiding it.

Use the currently set visibility policy to determine which range to display.

# `findColumn`

```erlang
-spec findColumn(This, Line, Column) -> integer()
                    when This :: wxStyledTextCtrl(), Line :: integer(), Column :: integer().
```

Find the position of a column on a line taking into account tabs and multi-byte
characters.

If beyond end of line, return line end position.

# `findText`

```erlang
-spec findText(This, MinPos, MaxPos, Text) -> integer()
                  when
                      This :: wxStyledTextCtrl(),
                      MinPos :: integer(),
                      MaxPos :: integer(),
                      Text :: unicode:chardata().
```

# `findText`

```erlang
-spec findText(This, MinPos, MaxPos, Text, [Option]) -> integer()
                  when
                      This :: wxStyledTextCtrl(),
                      MinPos :: integer(),
                      MaxPos :: integer(),
                      Text :: unicode:chardata(),
                      Option :: {flags, integer()}.
```

` Find some text in the document. @param minPos The position (starting from zero) in the
document at which to begin the search @param maxPos The last position (starting from zero)
in the document to which the search will be restricted. @param text The text to search
for. @param flags (Optional) The search flags. This should be a bit list containing one or
more of the @link wxStyledTextCtrl::wxSTC_FIND_WHOLEWORD wxSTC_FIND_* @endlink constants. `

Return: The position (starting from zero) in the document at which the text was found or
wxSTC_INVALID_POSITION if the search fails.

Remark: A backwards search can be performed by setting minPos to be greater than maxPos.

# `formatRange`

```erlang
-spec formatRange(This, DoDraw, StartPos, EndPos, Draw, Target, RenderRect, PageRect) -> integer()
                     when
                         This :: wxStyledTextCtrl(),
                         DoDraw :: boolean(),
                         StartPos :: integer(),
                         EndPos :: integer(),
                         Draw :: wxDC:wxDC(),
                         Target :: wxDC:wxDC(),
                         RenderRect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                         PageRect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

On Windows, will draw the document into a display context such as a printer.

# `formFeed`

```erlang
-spec formFeed(This) -> ok when This :: wxStyledTextCtrl().
```

Insert a Form Feed character.

# `getAnchor`

```erlang
-spec getAnchor(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the position of the opposite end of the selection to the caret.

# `getBackSpaceUnIndents`

```erlang
-spec getBackSpaceUnIndents(This) -> boolean() when This :: wxStyledTextCtrl().
```

Does a backspace pressed when caret is within indentation unindent?

# `getBufferedDraw`

```erlang
-spec getBufferedDraw(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is drawing done first into a buffer or direct to the screen?

# `getCaretForeground`

```erlang
-spec getCaretForeground(This) -> wx:wx_colour4() when This :: wxStyledTextCtrl().
```

Get the foreground colour of the caret.

# `getCaretLineBackAlpha`

```erlang
-spec getCaretLineBackAlpha(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the background alpha of the caret line.

# `getCaretLineBackground`

```erlang
-spec getCaretLineBackground(This) -> wx:wx_colour4() when This :: wxStyledTextCtrl().
```

Get the colour of the background of the line containing the caret.

# `getCaretLineVisible`

```erlang
-spec getCaretLineVisible(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is the background of the line containing the caret in a different colour?

# `getCaretPeriod`

```erlang
-spec getCaretPeriod(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the time in milliseconds that the caret is on and off.

# `getCaretSticky`

```erlang
-spec getCaretSticky(This) -> integer() when This :: wxStyledTextCtrl().
```

Can the caret preferred x position only be changed by explicit movement commands?

The return value will be one of the ?wxSTC\_CARETSTICKY\_\* constants.

# `getCaretWidth`

```erlang
-spec getCaretWidth(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the width of the insert mode caret.

# `getCharAt`

```erlang
-spec getCharAt(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Returns the character byte at the position.

# `getCodePage`

```erlang
-spec getCodePage(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the code page used to interpret the bytes of the document as characters.

# `getColumn`

```erlang
-spec getColumn(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Retrieve the column number of a position, taking tab width into account.

# `getControlCharSymbol`

```erlang
-spec getControlCharSymbol(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the way control characters are displayed.

# `getCurLine`

```erlang
-spec getCurLine(This) -> Result
                    when
                        Result :: {Res :: unicode:charlist(), LinePos :: integer()},
                        This :: wxStyledTextCtrl().
```

Retrieve the text of the line containing the caret.

linePos can optionally be passed in to receive the index of the caret on the line.

# `getCurLineRaw`

```erlang
-spec getCurLineRaw(This) -> Result
                       when
                           Result :: {Res :: binary(), LinePos :: integer()}, This :: wxStyledTextCtrl().
```

Retrieve the text of the line containing the caret.

Returns the index of the caret on the line.

# `getCurrentLine`

```erlang
-spec getCurrentLine(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the line number of the line with the caret.

# `getCurrentPos`

```erlang
-spec getCurrentPos(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the position of the caret.

# `getEdgeColour`

```erlang
-spec getEdgeColour(This) -> wx:wx_colour4() when This :: wxStyledTextCtrl().
```

Retrieve the colour used in edge indication.

# `getEdgeColumn`

```erlang
-spec getEdgeColumn(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the column number which text should be kept within.

# `getEdgeMode`

```erlang
-spec getEdgeMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the edge highlight mode.

The return value will be one of the ?wxSTC\_EDGE\_\* constants.

# `getEndAtLastLine`

```erlang
-spec getEndAtLastLine(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether the maximum scroll position has the last line at the bottom of the view.

# `getEndStyled`

```erlang
-spec getEndStyled(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the position of the last correctly styled character.

# `getEOLMode`

```erlang
-spec getEOLMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the current end of line mode - one of wxSTC\_EOL\_CRLF, wxSTC\_EOL\_CR, or
wxSTC\_EOL\_LF.

# `getFirstVisibleLine`

```erlang
-spec getFirstVisibleLine(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the display line at the top of the display.

# `getFoldExpanded`

```erlang
-spec getFoldExpanded(This, Line) -> boolean() when This :: wxStyledTextCtrl(), Line :: integer().
```

Is a header line expanded?

# `getFoldLevel`

```erlang
-spec getFoldLevel(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the fold level of a line.

# `getFoldParent`

```erlang
-spec getFoldParent(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Find the parent line of a child line.

# `getHighlightGuide`

```erlang
-spec getHighlightGuide(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the highlighted indentation guide column.

# `getIndent`

```erlang
-spec getIndent(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve indentation size.

# `getIndentationGuides`

```erlang
-spec getIndentationGuides(This) -> integer() when This :: wxStyledTextCtrl().
```

Are the indentation guides visible?

The return value will be one of the ?wxSTC\_IV\_\* constants.

# `getLastChild`

```erlang
-spec getLastChild(This, Line, Level) -> integer()
                      when This :: wxStyledTextCtrl(), Line :: integer(), Level :: integer().
```

Find the last child line of a header line.

# `getLastKeydownProcessed`

```erlang
-spec getLastKeydownProcessed(This) -> boolean() when This :: wxStyledTextCtrl().
```

Can be used to prevent the EVT_CHAR handler from adding the char.

# `getLayoutCache`

```erlang
-spec getLayoutCache(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the degree of caching of layout information.

The return value will be one of the ?wxSTC\_CACHE\_\* constants.

# `getLength`

```erlang
-spec getLength(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the number of bytes in the document.

# `getLexer`

```erlang
-spec getLexer(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the lexing language of the document.

The return value will be one of the ?wxSTC\_LEX\_\* constants.

# `getLine`

```erlang
-spec getLine(This, Line) -> unicode:charlist() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the contents of a line.

# `getLineCount`

```erlang
-spec getLineCount(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the number of lines in the document.

There is always at least one.

# `getLineEndPosition`

```erlang
-spec getLineEndPosition(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Get the position after the last visible characters on a line.

# `getLineIndentation`

```erlang
-spec getLineIndentation(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the number of columns that a line is indented.

# `getLineIndentPosition`

```erlang
-spec getLineIndentPosition(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the position before the first non indentation character on a line.

# `getLineRaw`

```erlang
-spec getLineRaw(This, Line) -> binary() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the contents of a line.

# `getLineState`

```erlang
-spec getLineState(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the extra styling information for a line.

# `getLineVisible`

```erlang
-spec getLineVisible(This, Line) -> boolean() when This :: wxStyledTextCtrl(), Line :: integer().
```

Is a line visible?

# `getMarginLeft`

```erlang
-spec getMarginLeft(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the size in pixels of the left margin.

# `getMarginMask`

```erlang
-spec getMarginMask(This, Margin) -> integer() when This :: wxStyledTextCtrl(), Margin :: integer().
```

Retrieve the marker mask of a margin.

# `getMarginRight`

```erlang
-spec getMarginRight(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the size in pixels of the right margin.

# `getMarginSensitive`

```erlang
-spec getMarginSensitive(This, Margin) -> boolean() when This :: wxStyledTextCtrl(), Margin :: integer().
```

Retrieve the mouse click sensitivity of a margin.

# `getMarginType`

```erlang
-spec getMarginType(This, Margin) -> integer() when This :: wxStyledTextCtrl(), Margin :: integer().
```

Retrieve the type of a margin.

The return value will be one of the ?wxSTC\_MARGIN\_\* constants.

# `getMarginWidth`

```erlang
-spec getMarginWidth(This, Margin) -> integer() when This :: wxStyledTextCtrl(), Margin :: integer().
```

Retrieve the width of a margin in pixels.

# `getMaxLineState`

```erlang
-spec getMaxLineState(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the last line number that has line state.

# `getModEventMask`

```erlang
-spec getModEventMask(This) -> integer() when This :: wxStyledTextCtrl().
```

Get which document modification events are sent to the container.

The return value will wxSTC_MODEVENTMASKALL if all changes generate events. Otherwise it
will be a bit list containing one or more of the ?wxSTC\_MOD\_\* constants, the
?wxSTC\_PERFORMED\_\* constants, wxSTC_STARTACTION, wxSTC_MULTILINEUNDOREDO,
wxSTC_MULTISTEPUNDOREDO, and wxSTC_LASTSTEPINUNDOREDO.

# `getModify`

```erlang
-spec getModify(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is the document different from when it was last saved?

# `getMouseDownCaptures`

```erlang
-spec getMouseDownCaptures(This) -> boolean() when This :: wxStyledTextCtrl().
```

Get whether mouse gets captured.

# `getMouseDwellTime`

```erlang
-spec getMouseDwellTime(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the time the mouse must sit still to generate a mouse dwell event.

The return value will be a time in milliseconds or wxSTC_TIME_FOREVER.

# `getOvertype`

```erlang
-spec getOvertype(This) -> boolean() when This :: wxStyledTextCtrl().
```

Returns true if overtype mode is active otherwise false is returned.

# `getPasteConvertEndings`

```erlang
-spec getPasteConvertEndings(This) -> boolean() when This :: wxStyledTextCtrl().
```

Get convert-on-paste setting.

# `getPrintColourMode`

```erlang
-spec getPrintColourMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the print colour mode.

The return value will be one of the ?wxSTC\_PRINT\_\* constants.

# `getPrintMagnification`

```erlang
-spec getPrintMagnification(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the print magnification.

# `getPrintWrapMode`

```erlang
-spec getPrintWrapMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Is printing line wrapped?

The return value will be one of the ?wxSTC\_WRAP\_\* constants.

# `getProperty`

```erlang
-spec getProperty(This, Key) -> unicode:charlist()
                     when This :: wxStyledTextCtrl(), Key :: unicode:chardata().
```

Retrieve a "property" value previously set with SetProperty.

# `getReadOnly`

```erlang
-spec getReadOnly(This) -> boolean() when This :: wxStyledTextCtrl().
```

In read-only mode?

# `getScrollWidth`

```erlang
-spec getScrollWidth(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the document width assumed for scrolling.

# `getSearchFlags`

```erlang
-spec getSearchFlags(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the search flags used by SearchInTarget.

The return value will be a bit list containing one or more of the ?wxSTC\_FIND\_\*
constants.

# `getSelAlpha`

```erlang
-spec getSelAlpha(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the alpha of the selection.

# `getSelectedText`

```erlang
-spec getSelectedText(This) -> unicode:charlist() when This :: wxStyledTextCtrl().
```

Retrieve the selected text.

# `getSelectedTextRaw`

```erlang
-spec getSelectedTextRaw(This) -> binary() when This :: wxStyledTextCtrl().
```

Retrieve the selected text.

# `getSelection`

```erlang
-spec getSelection(This) -> {From :: integer(), To :: integer()} when This :: wxStyledTextCtrl().
```

Gets the current selection span.

If the returned values are equal, there was no selection. Please note that the indices
returned may be used with the other `m:wxTextCtrl` methods but don't necessarily represent
the correct indices into the string returned by `wxComboBox:getValue/1` for multiline controls under Windows (at
least,) you should use `wxTextCtrl:getStringSelection/1` to get the selected text.

# `getSelectionEnd`

```erlang
-spec getSelectionEnd(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the position at the end of the selection.

# `getSelectionMode`

```erlang
-spec getSelectionMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the mode of the current selection.

The return value will be one of the ?wxSTC\_SEL\_\* constants.

# `getSelectionStart`

```erlang
-spec getSelectionStart(This) -> integer() when This :: wxStyledTextCtrl().
```

Returns the position at the start of the selection.

# `getSTCCursor`

```erlang
-spec getSTCCursor(This) -> integer() when This :: wxStyledTextCtrl().
```

Get cursor type.

The return value will be one of the ?wxSTC\_CURSOR\* constants.

# `getSTCFocus`

```erlang
-spec getSTCFocus(This) -> boolean() when This :: wxStyledTextCtrl().
```

Get internal focus flag.

# `getStatus`

```erlang
-spec getStatus(This) -> integer() when This :: wxStyledTextCtrl().
```

Get error status.

The return value will be one of the ?wxSTC\_STATUS\_\* constants.

# `getStyleAt`

```erlang
-spec getStyleAt(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Returns the style byte at the position.

# `getStyleBits`

```erlang
-spec getStyleBits(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve number of bits in style bytes used to hold the lexical state.

Deprecated:

# `getStyleBitsNeeded`

```erlang
-spec getStyleBitsNeeded(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the number of bits the current lexer needs for styling.

Deprecated:

# `getTabIndents`

```erlang
-spec getTabIndents(This) -> boolean() when This :: wxStyledTextCtrl().
```

Does a tab pressed when caret is within indentation indent?

# `getTabWidth`

```erlang
-spec getTabWidth(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the visible size of a tab.

# `getTargetEnd`

```erlang
-spec getTargetEnd(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the position that ends the target.

# `getTargetStart`

```erlang
-spec getTargetStart(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the position that starts the target.

# `getText`

```erlang
-spec getText(This) -> unicode:charlist() when This :: wxStyledTextCtrl().
```

Retrieve all the text in the document.

# `getTextLength`

```erlang
-spec getTextLength(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the number of characters in the document.

# `getTextRange`

```erlang
-spec getTextRange(This, StartPos, EndPos) -> unicode:charlist()
                      when This :: wxStyledTextCtrl(), StartPos :: integer(), EndPos :: integer().
```

Retrieve a range of text.

# `getTextRangeRaw`

```erlang
-spec getTextRangeRaw(This, StartPos, EndPos) -> binary()
                         when This :: wxStyledTextCtrl(), StartPos :: integer(), EndPos :: integer().
```

Retrieve a range of text.

# `getTextRaw`

```erlang
-spec getTextRaw(This) -> binary() when This :: wxStyledTextCtrl().
```

Retrieve all the text in the document.

# `getTwoPhaseDraw`

```erlang
-spec getTwoPhaseDraw(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is drawing done in two phases with backgrounds drawn before foregrounds?

# `getUndoCollection`

```erlang
-spec getUndoCollection(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is undo history being collected?

# `getUseAntiAliasing`

```erlang
-spec getUseAntiAliasing(This) -> boolean() when This :: wxStyledTextCtrl().
```

Returns the current UseAntiAliasing setting.

# `getUseHorizontalScrollBar`

```erlang
-spec getUseHorizontalScrollBar(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is the horizontal scroll bar visible?

# `getUseTabs`

```erlang
-spec getUseTabs(This) -> boolean() when This :: wxStyledTextCtrl().
```

Retrieve whether tabs will be used in indentation.

# `getUseVerticalScrollBar`

```erlang
-spec getUseVerticalScrollBar(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is the vertical scroll bar visible?

# `getViewEOL`

```erlang
-spec getViewEOL(This) -> boolean() when This :: wxStyledTextCtrl().
```

Are the end of line characters visible?

# `getViewWhiteSpace`

```erlang
-spec getViewWhiteSpace(This) -> integer() when This :: wxStyledTextCtrl().
```

Are white space characters currently visible? Returns one of wxSTC_WS_* constants.

# `getWrapMode`

```erlang
-spec getWrapMode(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve whether text is word wrapped.

The return value will be one of the ?wxSTC\_WRAP\_\* constants.

# `getWrapStartIndent`

```erlang
-spec getWrapStartIndent(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the start indent for wrapped lines.

# `getWrapVisualFlags`

```erlang
-spec getWrapVisualFlags(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the display mode of visual flags for wrapped lines.

The return value will be a bit list containing one or more of the
?wxSTC\_WRAPVISUALFLAG\_\* constants.

# `getWrapVisualFlagsLocation`

```erlang
-spec getWrapVisualFlagsLocation(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the location of visual flags for wrapped lines.

The return value will be a bit list containing one or more of the
?wxSTC\_WRAPVISUALFLAGLOC\_\* constants.

# `getXOffset`

```erlang
-spec getXOffset(This) -> integer() when This :: wxStyledTextCtrl().
```

Get the xOffset (ie, horizontal scroll position).

# `getZoom`

```erlang
-spec getZoom(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieve the zoom level.

# `gotoLine`

```erlang
-spec gotoLine(This, Line) -> ok when This :: wxStyledTextCtrl(), Line :: integer().
```

Set caret to start of a line and ensure it is visible.

# `gotoPos`

```erlang
-spec gotoPos(This, Caret) -> ok when This :: wxStyledTextCtrl(), Caret :: integer().
```

Set caret to a position and ensure it is visible.

# `hideLines`

```erlang
-spec hideLines(This, LineStart, LineEnd) -> ok
                   when This :: wxStyledTextCtrl(), LineStart :: integer(), LineEnd :: integer().
```

Make a range of lines invisible.

# `hideSelection`

```erlang
-spec hideSelection(This, Hide) -> ok when This :: wxStyledTextCtrl(), Hide :: boolean().
```

Draw the selection in normal style or with selection highlighted.

# `home`

```erlang
-spec home(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position on line.

# `homeDisplay`

```erlang
-spec homeDisplay(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position on display line.

# `homeDisplayExtend`

```erlang
-spec homeDisplayExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position on display line extending selection to new caret position.

# `homeExtend`

```erlang
-spec homeExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position on line extending selection to new caret position.

# `homeRectExtend`

```erlang
-spec homeRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to first position on line, extending rectangular selection to new caret
position.

# `homeWrapExtend`

```erlang
-spec homeWrapExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Like HomeExtend but when word-wrap is enabled extends first to start of display line
HomeDisplayExtend, then to start of document line HomeExtend.

# `indicatorGetForeground`

```erlang
-spec indicatorGetForeground(This, Indicator) -> wx:wx_colour4()
                                when This :: wxStyledTextCtrl(), Indicator :: integer().
```

Retrieve the foreground colour of an indicator.

# `indicatorGetStyle`

```erlang
-spec indicatorGetStyle(This, Indicator) -> integer()
                           when This :: wxStyledTextCtrl(), Indicator :: integer().
```

Retrieve the style of an indicator.

The return value will be one of the ?wxSTC\_INDIC\_\* constants.

# `indicatorSetForeground`

```erlang
-spec indicatorSetForeground(This, Indicator, Fore) -> ok
                                when
                                    This :: wxStyledTextCtrl(),
                                    Indicator :: integer(),
                                    Fore :: wx:wx_colour().
```

Set the foreground colour of an indicator.

# `indicatorSetStyle`

```erlang
-spec indicatorSetStyle(This, Indicator, IndicatorStyle) -> ok
                           when
                               This :: wxStyledTextCtrl(),
                               Indicator :: integer(),
                               IndicatorStyle :: integer().
```

Set an indicator to plain, squiggle or TT.

The second argument should be one of the ?wxSTC\_INDIC\_\* constants.

# `insertText`

```erlang
-spec insertText(This, Pos, Text) -> ok
                    when This :: wxStyledTextCtrl(), Pos :: integer(), Text :: unicode:chardata().
```

Insert string at a position.

# `insertTextRaw`

```erlang
-spec insertTextRaw(This, Pos, Text) -> ok
                       when This :: wxStyledTextCtrl(), Pos :: integer(), Text :: binary().
```

Insert string at a position.

# `lineCopy`

```erlang
-spec lineCopy(This) -> ok when This :: wxStyledTextCtrl().
```

Copy the line containing the caret.

# `lineCut`

```erlang
-spec lineCut(This) -> ok when This :: wxStyledTextCtrl().
```

Cut the line containing the caret.

# `lineDelete`

```erlang
-spec lineDelete(This) -> ok when This :: wxStyledTextCtrl().
```

Delete the line containing the caret.

# `lineDown`

```erlang
-spec lineDown(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret down one line.

# `lineDownExtend`

```erlang
-spec lineDownExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret down one line extending selection to new caret position.

# `lineDownRectExtend`

```erlang
-spec lineDownRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret down one line, extending rectangular selection to new caret position.

# `lineDuplicate`

```erlang
-spec lineDuplicate(This) -> ok when This :: wxStyledTextCtrl().
```

Duplicate the current line.

# `lineEnd`

```erlang
-spec lineEnd(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position on line.

# `lineEndDisplay`

```erlang
-spec lineEndDisplay(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position on display line.

# `lineEndDisplayExtend`

```erlang
-spec lineEndDisplayExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position on display line extending selection to new caret position.

# `lineEndExtend`

```erlang
-spec lineEndExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position on line extending selection to new caret position.

# `lineEndRectExtend`

```erlang
-spec lineEndRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to last position on line, extending rectangular selection to new caret
position.

# `lineEndWrap`

```erlang
-spec lineEndWrap(This) -> ok when This :: wxStyledTextCtrl().
```

Like LineEnd but when word-wrap is enabled goes first to end of display line
LineEndDisplay, then to start of document line LineEnd.

# `lineEndWrapExtend`

```erlang
-spec lineEndWrapExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Like LineEndExtend but when word-wrap is enabled extends first to end of display line
LineEndDisplayExtend, then to start of document line LineEndExtend.

# `lineFromPosition`

```erlang
-spec lineFromPosition(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Retrieve the line containing a position.

# `lineLength`

```erlang
-spec lineLength(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

How many characters are on a line, including end of line characters?

# `lineScroll`

```erlang
-spec lineScroll(This, Columns, Lines) -> ok
                    when This :: wxStyledTextCtrl(), Columns :: integer(), Lines :: integer().
```

Scroll horizontally and vertically.

# `lineScrollDown`

```erlang
-spec lineScrollDown(This) -> ok when This :: wxStyledTextCtrl().
```

Scroll the document down, keeping the caret visible.

# `lineScrollUp`

```erlang
-spec lineScrollUp(This) -> ok when This :: wxStyledTextCtrl().
```

Scroll the document up, keeping the caret visible.

# `linesJoin`

```erlang
-spec linesJoin(This) -> ok when This :: wxStyledTextCtrl().
```

Join the lines in the target.

# `linesOnScreen`

```erlang
-spec linesOnScreen(This) -> integer() when This :: wxStyledTextCtrl().
```

Retrieves the number of lines completely visible.

# `linesSplit`

```erlang
-spec linesSplit(This, PixelWidth) -> ok when This :: wxStyledTextCtrl(), PixelWidth :: integer().
```

Split the lines in the target into lines that are less wide than pixelWidth where
possible.

# `lineTranspose`

```erlang
-spec lineTranspose(This) -> ok when This :: wxStyledTextCtrl().
```

Switch the current line with the previous.

# `lineUp`

```erlang
-spec lineUp(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret up one line.

# `lineUpExtend`

```erlang
-spec lineUpExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret up one line extending selection to new caret position.

# `lineUpRectExtend`

```erlang
-spec lineUpRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret up one line, extending rectangular selection to new caret position.

# `loadFile`

```erlang
-spec loadFile(This, Filename) -> boolean()
                  when This :: wxStyledTextCtrl(), Filename :: unicode:chardata().
```

Load the contents of filename into the editor.

# `lowerCase`

```erlang
-spec lowerCase(This) -> ok when This :: wxStyledTextCtrl().
```

Transform the selection to lower case.

# `markerAdd`

```erlang
-spec markerAdd(This, Line, MarkerNumber) -> integer()
                   when This :: wxStyledTextCtrl(), Line :: integer(), MarkerNumber :: integer().
```

Add a marker to a line, returning an ID which can be used to find or delete the marker.

# `markerAddSet`

```erlang
-spec markerAddSet(This, Line, MarkerSet) -> ok
                      when This :: wxStyledTextCtrl(), Line :: integer(), MarkerSet :: integer().
```

Add a set of markers to a line.

# `markerDefine`

```erlang
-spec markerDefine(This, MarkerNumber, MarkerSymbol) -> ok
                      when
                          This :: wxStyledTextCtrl(),
                          MarkerNumber :: integer(),
                          MarkerSymbol :: integer().
```

# `markerDefine`

```erlang
-spec markerDefine(This, MarkerNumber, MarkerSymbol, [Option]) -> ok
                      when
                          This :: wxStyledTextCtrl(),
                          MarkerNumber :: integer(),
                          MarkerSymbol :: integer(),
                          Option :: {foreground, wx:wx_colour()} | {background, wx:wx_colour()}.
```

Set the symbol used for a particular marker number, and optionally the fore and
background colours.

The second argument should be one of the ?wxSTC\_MARK\_\* constants.

# `markerDefineBitmap`

```erlang
-spec markerDefineBitmap(This, MarkerNumber, Bmp) -> ok
                            when
                                This :: wxStyledTextCtrl(),
                                MarkerNumber :: integer(),
                                Bmp :: wxBitmap:wxBitmap().
```

Define a marker with a `m:wxBitmap`.

# `markerDelete`

```erlang
-spec markerDelete(This, Line, MarkerNumber) -> ok
                      when This :: wxStyledTextCtrl(), Line :: integer(), MarkerNumber :: integer().
```

Delete a marker from a line.

# `markerDeleteAll`

```erlang
-spec markerDeleteAll(This, MarkerNumber) -> ok
                         when This :: wxStyledTextCtrl(), MarkerNumber :: integer().
```

Delete all markers with a particular number from all lines.

# `markerDeleteHandle`

```erlang
-spec markerDeleteHandle(This, MarkerHandle) -> ok
                            when This :: wxStyledTextCtrl(), MarkerHandle :: integer().
```

Delete a marker.

# `markerGet`

```erlang
-spec markerGet(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Get a bit mask of all the markers set on a line.

# `markerLineFromHandle`

```erlang
-spec markerLineFromHandle(This, MarkerHandle) -> integer()
                              when This :: wxStyledTextCtrl(), MarkerHandle :: integer().
```

Retrieve the line number at which a particular marker is located.

# `markerNext`

```erlang
-spec markerNext(This, LineStart, MarkerMask) -> integer()
                    when This :: wxStyledTextCtrl(), LineStart :: integer(), MarkerMask :: integer().
```

Find the next line at or after lineStart that includes a marker in mask.

Return -1 when no more lines.

# `markerPrevious`

```erlang
-spec markerPrevious(This, LineStart, MarkerMask) -> integer()
                        when This :: wxStyledTextCtrl(), LineStart :: integer(), MarkerMask :: integer().
```

Find the previous line before lineStart that includes a marker in mask.

# `markerSetAlpha`

```erlang
-spec markerSetAlpha(This, MarkerNumber, Alpha) -> ok
                        when This :: wxStyledTextCtrl(), MarkerNumber :: integer(), Alpha :: integer().
```

Set the alpha used for a marker that is drawn in the text area, not the margin.

# `markerSetBackground`

```erlang
-spec markerSetBackground(This, MarkerNumber, Back) -> ok
                             when
                                 This :: wxStyledTextCtrl(),
                                 MarkerNumber :: integer(),
                                 Back :: wx:wx_colour().
```

Set the background colour used for a particular marker number.

# `markerSetForeground`

```erlang
-spec markerSetForeground(This, MarkerNumber, Fore) -> ok
                             when
                                 This :: wxStyledTextCtrl(),
                                 MarkerNumber :: integer(),
                                 Fore :: wx:wx_colour().
```

Set the foreground colour used for a particular marker number.

# `moveCaretInsideView`

```erlang
-spec moveCaretInsideView(This) -> ok when This :: wxStyledTextCtrl().
```

Move the caret inside current view if it's not there already.

# `new`

```erlang
-spec new() -> wxStyledTextCtrl().
```

Default ctor.

# `new`

```erlang
-spec new(Parent) -> wxStyledTextCtrl() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxStyledTextCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Ctor.

# `newLine`

```erlang
-spec newLine(This) -> ok when This :: wxStyledTextCtrl().
```

Insert a new line, may use a CRLF, CR or LF depending on EOL mode.

# `pageDown`

```erlang
-spec pageDown(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page down.

# `pageDownExtend`

```erlang
-spec pageDownExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page down extending selection to new caret position.

# `pageDownRectExtend`

```erlang
-spec pageDownRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page down, extending rectangular selection to new caret position.

# `pageUp`

```erlang
-spec pageUp(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page up.

# `pageUpExtend`

```erlang
-spec pageUpExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page up extending selection to new caret position.

# `pageUpRectExtend`

```erlang
-spec pageUpRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret one page up, extending rectangular selection to new caret position.

# `paraDownExtend`

```erlang
-spec paraDownExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Extend selection down one paragraph (delimited by empty lines).

# `paraUp`

```erlang
-spec paraUp(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret up one paragraph (delimited by empty lines).

# `paraUpExtend`

```erlang
-spec paraUpExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Extend selection up one paragraph (delimited by empty lines).

# `paste`

```erlang
-spec paste(This) -> ok when This :: wxStyledTextCtrl().
```

Paste the contents of the clipboard into the document replacing the selection.

# `pointFromPosition`

```erlang
-spec pointFromPosition(This, Pos) -> {X :: integer(), Y :: integer()}
                           when This :: wxStyledTextCtrl(), Pos :: integer().
```

Retrieve the point in the window where a position is displayed.

# `positionAfter`

```erlang
-spec positionAfter(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Given a valid document position, return the next position taking code page into account.

Maximum value returned is the last position in the document.

# `positionBefore`

```erlang
-spec positionBefore(This, Pos) -> integer() when This :: wxStyledTextCtrl(), Pos :: integer().
```

Given a valid document position, return the previous position taking code page into
account.

Returns 0 if passed 0.

# `positionFromLine`

```erlang
-spec positionFromLine(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the position at the start of a line.

# `positionFromPoint`

```erlang
-spec positionFromPoint(This, Pt) -> integer()
                           when This :: wxStyledTextCtrl(), Pt :: {X :: integer(), Y :: integer()}.
```

Find the position from a point within the window.

# `positionFromPointClose`

```erlang
-spec positionFromPointClose(This, X, Y) -> integer()
                                when This :: wxStyledTextCtrl(), X :: integer(), Y :: integer().
```

Find the position from a point within the window but return wxSTC\_INVALID\_POSITION if
not close to text.

# `redo`

```erlang
-spec redo(This) -> ok when This :: wxStyledTextCtrl().
```

Redoes the next action on the undo history.

# `registerImage`

```erlang
-spec registerImage(This, Type, Bmp) -> ok
                       when This :: wxStyledTextCtrl(), Type :: integer(), Bmp :: wxBitmap:wxBitmap().
```

Register an image for use in autocompletion lists.

# `replaceSelection`

```erlang
-spec replaceSelection(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Replace the selected text with the argument text.

# `replaceTarget`

```erlang
-spec replaceTarget(This, Text) -> integer() when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Replace the target text with the argument text.

Text is counted so it can contain NULs. Returns the length of the replacement text.

# `saveFile`

```erlang
-spec saveFile(This, Filename) -> boolean()
                  when This :: wxStyledTextCtrl(), Filename :: unicode:chardata().
```

Write the contents of the editor to filename.

# `scrollToColumn`

```erlang
-spec scrollToColumn(This, Column) -> ok when This :: wxStyledTextCtrl(), Column :: integer().
```

Scroll enough to make the given column visible.

# `scrollToLine`

```erlang
-spec scrollToLine(This, Line) -> ok when This :: wxStyledTextCtrl(), Line :: integer().
```

Scroll enough to make the given line visible.

# `searchAnchor`

```erlang
-spec searchAnchor(This) -> ok when This :: wxStyledTextCtrl().
```

Sets the current caret position to be the search anchor.

# `searchInTarget`

```erlang
-spec searchInTarget(This, Text) -> integer()
                        when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Search for a counted string in the target and set the target to the found range.

Text is counted so it can contain NULs. Returns length of range or -1 for failure in
which case target is not moved.

# `searchNext`

```erlang
-spec searchNext(This, SearchFlags, Text) -> integer()
                    when
                        This :: wxStyledTextCtrl(), SearchFlags :: integer(), Text :: unicode:chardata().
```

Find some text starting at the search anchor.

Does not ensure the selection is visible.

# `searchPrev`

```erlang
-spec searchPrev(This, SearchFlags, Text) -> integer()
                    when
                        This :: wxStyledTextCtrl(), SearchFlags :: integer(), Text :: unicode:chardata().
```

Find some text starting at the search anchor and moving backwards.

Does not ensure the selection is visible.

# `selectAll`

```erlang
-spec selectAll(This) -> ok when This :: wxStyledTextCtrl().
```

Select all the text in the document.

# `selectionDuplicate`

```erlang
-spec selectionDuplicate(This) -> ok when This :: wxStyledTextCtrl().
```

Duplicate the selection.

If selection empty duplicate the line containing the caret.

# `selectionIsRectangle`

```erlang
-spec selectionIsRectangle(This) -> boolean() when This :: wxStyledTextCtrl().
```

Is the selection rectangular? The alternative is the more common stream selection.

# `setAnchor`

```erlang
-spec setAnchor(This, Anchor) -> ok when This :: wxStyledTextCtrl(), Anchor :: integer().
```

Set the selection anchor to a position.

The anchor is the opposite end of the selection from the caret.

# `setBackSpaceUnIndents`

```erlang
-spec setBackSpaceUnIndents(This, BsUnIndents) -> ok
                               when This :: wxStyledTextCtrl(), BsUnIndents :: boolean().
```

Sets whether a backspace pressed when caret is within indentation unindents.

# `setBufferedDraw`

```erlang
-spec setBufferedDraw(This, Buffered) -> ok when This :: wxStyledTextCtrl(), Buffered :: boolean().
```

If drawing is buffered then each line of text is drawn into a bitmap buffer before
drawing it to the screen to avoid flicker.

# `setCaretForeground`

```erlang
-spec setCaretForeground(This, Fore) -> ok when This :: wxStyledTextCtrl(), Fore :: wx:wx_colour().
```

Set the foreground colour of the caret.

# `setCaretLineBackAlpha`

```erlang
-spec setCaretLineBackAlpha(This, Alpha) -> ok when This :: wxStyledTextCtrl(), Alpha :: integer().
```

Set background alpha of the caret line.

# `setCaretLineBackground`

```erlang
-spec setCaretLineBackground(This, Back) -> ok when This :: wxStyledTextCtrl(), Back :: wx:wx_colour().
```

Set the colour of the background of the line containing the caret.

# `setCaretLineVisible`

```erlang
-spec setCaretLineVisible(This, Show) -> ok when This :: wxStyledTextCtrl(), Show :: boolean().
```

Display the background of the line containing the caret in a different colour.

# `setCaretPeriod`

```erlang
-spec setCaretPeriod(This, PeriodMilliseconds) -> ok
                        when This :: wxStyledTextCtrl(), PeriodMilliseconds :: integer().
```

Get the time in milliseconds that the caret is on and off.

0 = steady on.

# `setCaretSticky`

```erlang
-spec setCaretSticky(This, UseCaretStickyBehaviour) -> ok
                        when This :: wxStyledTextCtrl(), UseCaretStickyBehaviour :: integer().
```

Stop the caret preferred x position changing when the user types.

The input should be one of the ?wxSTC\_CARETSTICKY\_\* constants.

# `setCaretWidth`

```erlang
-spec setCaretWidth(This, PixelWidth) -> ok when This :: wxStyledTextCtrl(), PixelWidth :: integer().
```

Set the width of the insert mode caret.

# `setCharsDefault`

```erlang
-spec setCharsDefault(This) -> ok when This :: wxStyledTextCtrl().
```

Reset the set of characters for whitespace and word characters to the defaults.

# `setCodePage`

```erlang
-spec setCodePage(This, CodePage) -> ok when This :: wxStyledTextCtrl(), CodePage :: integer().
```

Set the code page used to interpret the bytes of the document as characters.

# `setControlCharSymbol`

```erlang
-spec setControlCharSymbol(This, Symbol) -> ok when This :: wxStyledTextCtrl(), Symbol :: integer().
```

Change the way control characters are displayed: If symbol is *< 32, keep the drawn way,
else, use the given character.

# `setCurrentPos`

```erlang
-spec setCurrentPos(This, Caret) -> ok when This :: wxStyledTextCtrl(), Caret :: integer().
```

Sets the position of the caret.

# `setEdgeColour`

```erlang
-spec setEdgeColour(This, EdgeColour) -> ok
                       when This :: wxStyledTextCtrl(), EdgeColour :: wx:wx_colour().
```

Change the colour used in edge indication.

# `setEdgeColumn`

```erlang
-spec setEdgeColumn(This, Column) -> ok when This :: wxStyledTextCtrl(), Column :: integer().
```

Set the column number of the edge.

If text goes past the edge then it is highlighted.

# `setEdgeMode`

```erlang
-spec setEdgeMode(This, EdgeMode) -> ok when This :: wxStyledTextCtrl(), EdgeMode :: integer().
```

The edge may be displayed by a line (wxSTC\_EDGE\_LINE/wxSTC\_EDGE\_MULTILINE) or by
highlighting text that goes beyond it (wxSTC\_EDGE\_BACKGROUND) or not displayed at all
(wxSTC\_EDGE\_NONE).

The input should be one of the ?wxSTC\_EDGE\_\* constants.

# `setEOLMode`

```erlang
-spec setEOLMode(This, EolMode) -> ok when This :: wxStyledTextCtrl(), EolMode :: integer().
```

Set the current end of line mode.

The input should be one of the ?wxSTC\_EOL\_\* constants.

# `setFoldExpanded`

```erlang
-spec setFoldExpanded(This, Line, Expanded) -> ok
                         when This :: wxStyledTextCtrl(), Line :: integer(), Expanded :: boolean().
```

Show the children of a header line.

# `setFoldFlags`

```erlang
-spec setFoldFlags(This, Flags) -> ok when This :: wxStyledTextCtrl(), Flags :: integer().
```

Set some style options for folding.

The second argument should be a bit list containing one or more of the
?wxSTC\_FOLDFLAG\_\* constants.

# `setFoldLevel`

```erlang
-spec setFoldLevel(This, Line, Level) -> ok
                      when This :: wxStyledTextCtrl(), Line :: integer(), Level :: integer().
```

Set the fold level of a line.

This encodes an integer level along with flags indicating whether the line is a header
and whether it is effectively white space.

# `setFoldMarginColour`

```erlang
-spec setFoldMarginColour(This, UseSetting, Back) -> ok
                             when
                                 This :: wxStyledTextCtrl(),
                                 UseSetting :: boolean(),
                                 Back :: wx:wx_colour().
```

Set one of the colours used as a chequerboard pattern in the fold margin.

# `setFoldMarginHiColour`

```erlang
-spec setFoldMarginHiColour(This, UseSetting, Fore) -> ok
                               when
                                   This :: wxStyledTextCtrl(),
                                   UseSetting :: boolean(),
                                   Fore :: wx:wx_colour().
```

Set the other colour used as a chequerboard pattern in the fold margin.

# `setHighlightGuide`

```erlang
-spec setHighlightGuide(This, Column) -> ok when This :: wxStyledTextCtrl(), Column :: integer().
```

Set the highlighted indentation guide column.

0 = no highlighted guide.

# `setHotspotActiveBackground`

```erlang
-spec setHotspotActiveBackground(This, UseSetting, Back) -> ok
                                    when
                                        This :: wxStyledTextCtrl(),
                                        UseSetting :: boolean(),
                                        Back :: wx:wx_colour().
```

Set a back colour for active hotspots.

# `setHotspotActiveForeground`

```erlang
-spec setHotspotActiveForeground(This, UseSetting, Fore) -> ok
                                    when
                                        This :: wxStyledTextCtrl(),
                                        UseSetting :: boolean(),
                                        Fore :: wx:wx_colour().
```

Set a fore colour for active hotspots.

# `setHotspotActiveUnderline`

```erlang
-spec setHotspotActiveUnderline(This, Underline) -> ok
                                   when This :: wxStyledTextCtrl(), Underline :: boolean().
```

Enable / Disable underlining active hotspots.

# `setHotspotSingleLine`

```erlang
-spec setHotspotSingleLine(This, SingleLine) -> ok
                              when This :: wxStyledTextCtrl(), SingleLine :: boolean().
```

Limit hotspots to single line so hotspots on two lines don't merge.

# `setHScrollBar`

```erlang
-spec setHScrollBar(This, Bar) -> ok when This :: wxStyledTextCtrl(), Bar :: wxScrollBar:wxScrollBar().
```

Set the horizontal scrollbar to use instead of the one that's built-in.

# `setIndent`

```erlang
-spec setIndent(This, IndentSize) -> ok when This :: wxStyledTextCtrl(), IndentSize :: integer().
```

Set the number of spaces used for one level of indentation.

# `setIndentationGuides`

```erlang
-spec setIndentationGuides(This, IndentView) -> ok
                              when This :: wxStyledTextCtrl(), IndentView :: integer().
```

Show or hide indentation guides.

The input should be one of the ?wxSTC\_IV\_\* constants.

# `setKeyWords`

```erlang
-spec setKeyWords(This, KeyWordSet, KeyWords) -> ok
                     when
                         This :: wxStyledTextCtrl(),
                         KeyWordSet :: integer(),
                         KeyWords :: unicode:chardata().
```

Set up the key words used by the lexer.

# `setLastKeydownProcessed`

```erlang
-spec setLastKeydownProcessed(This, Val) -> ok when This :: wxStyledTextCtrl(), Val :: boolean().
```

Returns the line number of the line with the caret.

# `setLayoutCache`

```erlang
-spec setLayoutCache(This, CacheMode) -> ok when This :: wxStyledTextCtrl(), CacheMode :: integer().
```

Sets the degree of caching of layout information.

The input should be one of the ?wxSTC\_CACHE\_\* constants.

# `setLexer`

```erlang
-spec setLexer(This, Lexer) -> ok when This :: wxStyledTextCtrl(), Lexer :: integer().
```

Set the lexing language of the document.

The input should be one of the ?wxSTC\_LEX\_\* constants.

# `setLexerLanguage`

```erlang
-spec setLexerLanguage(This, Language) -> ok
                          when This :: wxStyledTextCtrl(), Language :: unicode:chardata().
```

Set the lexing language of the document based on string name.

# `setLineIndentation`

```erlang
-spec setLineIndentation(This, Line, Indentation) -> ok
                            when This :: wxStyledTextCtrl(), Line :: integer(), Indentation :: integer().
```

Change the indentation of a line to a number of columns.

# `setLineState`

```erlang
-spec setLineState(This, Line, State) -> ok
                      when This :: wxStyledTextCtrl(), Line :: integer(), State :: integer().
```

Used to hold extra styling information for each line.

# `setMarginLeft`

```erlang
-spec setMarginLeft(This, PixelWidth) -> ok when This :: wxStyledTextCtrl(), PixelWidth :: integer().
```

Sets the size in pixels of the left margin.

# `setMarginMask`

```erlang
-spec setMarginMask(This, Margin, Mask) -> ok
                       when This :: wxStyledTextCtrl(), Margin :: integer(), Mask :: integer().
```

Set a mask that determines which markers are displayed in a margin.

# `setMarginRight`

```erlang
-spec setMarginRight(This, PixelWidth) -> ok when This :: wxStyledTextCtrl(), PixelWidth :: integer().
```

Sets the size in pixels of the right margin.

# `setMarginSensitive`

```erlang
-spec setMarginSensitive(This, Margin, Sensitive) -> ok
                            when This :: wxStyledTextCtrl(), Margin :: integer(), Sensitive :: boolean().
```

Make a margin sensitive or insensitive to mouse clicks.

# `setMargins`

```erlang
-spec setMargins(This, Left, Right) -> ok
                    when This :: wxStyledTextCtrl(), Left :: integer(), Right :: integer().
```

Set the left and right margin in the edit area, measured in pixels.

# `setMarginType`

```erlang
-spec setMarginType(This, Margin, MarginType) -> ok
                       when This :: wxStyledTextCtrl(), Margin :: integer(), MarginType :: integer().
```

Set a margin to be either numeric or symbolic.

The second argument should be one of the ?wxSTC\_MARGIN\_\* constants.

# `setMarginWidth`

```erlang
-spec setMarginWidth(This, Margin, PixelWidth) -> ok
                        when This :: wxStyledTextCtrl(), Margin :: integer(), PixelWidth :: integer().
```

Set the width of a margin to a width expressed in pixels.

# `setModEventMask`

```erlang
-spec setModEventMask(This, EventMask) -> ok when This :: wxStyledTextCtrl(), EventMask :: integer().
```

Set which document modification events are sent to the container.

The input should be a bit list containing one or more of the ?wxSTC\_MOD\_\* constants,
the ?wxSTC\_PERFORMED\_\* constants, wxSTC_STARTACTION, wxSTC_MULTILINEUNDOREDO,
wxSTC_MULTISTEPUNDOREDO, and wxSTC_LASTSTEPINUNDOREDO. The input can also be
wxSTC_MODEVENTMASKALL to indicate that all changes should generate events.

# `setMouseDownCaptures`

```erlang
-spec setMouseDownCaptures(This, Captures) -> ok when This :: wxStyledTextCtrl(), Captures :: boolean().
```

Set whether the mouse is captured when its button is pressed.

# `setMouseDwellTime`

```erlang
-spec setMouseDwellTime(This, PeriodMilliseconds) -> ok
                           when This :: wxStyledTextCtrl(), PeriodMilliseconds :: integer().
```

Sets the time the mouse must sit still to generate a mouse dwell event.

The input should be a time in milliseconds or wxSTC_TIME_FOREVER.

# `setPasteConvertEndings`

```erlang
-spec setPasteConvertEndings(This, Convert) -> ok when This :: wxStyledTextCtrl(), Convert :: boolean().
```

Enable/Disable convert-on-paste for line endings.

# `setPrintColourMode`

```erlang
-spec setPrintColourMode(This, Mode) -> ok when This :: wxStyledTextCtrl(), Mode :: integer().
```

Modify colours when printing for clearer printed text.

The input should be one of the ?wxSTC\_PRINT\_\* constants.

# `setPrintMagnification`

```erlang
-spec setPrintMagnification(This, Magnification) -> ok
                               when This :: wxStyledTextCtrl(), Magnification :: integer().
```

Sets the print magnification added to the point size of each style for printing.

# `setProperty`

```erlang
-spec setProperty(This, Key, Value) -> ok
                     when
                         This :: wxStyledTextCtrl(),
                         Key :: unicode:chardata(),
                         Value :: unicode:chardata().
```

Set up a value that may be used by a lexer for some optional feature.

# `setReadOnly`

```erlang
-spec setReadOnly(This, ReadOnly) -> ok when This :: wxStyledTextCtrl(), ReadOnly :: boolean().
```

Set to read only or read write.

# `setSavePoint`

```erlang
-spec setSavePoint(This) -> ok when This :: wxStyledTextCtrl().
```

Remember the current position in the undo history as the position at which the document
was saved.

# `setScrollWidth`

```erlang
-spec setScrollWidth(This, PixelWidth) -> ok when This :: wxStyledTextCtrl(), PixelWidth :: integer().
```

Sets the document width assumed for scrolling.

# `setSearchFlags`

```erlang
-spec setSearchFlags(This, SearchFlags) -> ok when This :: wxStyledTextCtrl(), SearchFlags :: integer().
```

Set the search flags used by SearchInTarget.

The input should be a bit list containing one or more of the ?wxSTC\_FIND\_\* constants.

# `setSelAlpha`

```erlang
-spec setSelAlpha(This, Alpha) -> ok when This :: wxStyledTextCtrl(), Alpha :: integer().
```

Set the alpha of the selection.

# `setSelBackground`

```erlang
-spec setSelBackground(This, UseSetting, Back) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              UseSetting :: boolean(),
                              Back :: wx:wx_colour().
```

Set the background colour of the main and additional selections and whether to use this
setting.

# `setSelection`

```erlang
-spec setSelection(This, From, To) -> ok
                      when This :: wxStyledTextCtrl(), From :: integer(), To :: integer().
```

Selects the text starting at the first position up to (but not including) the character
at the last position.

If both parameters are equal to -1 all text in the control is selected.

Notice that the insertion point will be moved to `from` by this function.

See: `selectAll/1`

# `setSelectionEnd`

```erlang
-spec setSelectionEnd(This, Caret) -> ok when This :: wxStyledTextCtrl(), Caret :: integer().
```

Sets the position that ends the selection - this becomes the caret.

# `setSelectionMode`

```erlang
-spec setSelectionMode(This, SelectionMode) -> ok
                          when This :: wxStyledTextCtrl(), SelectionMode :: integer().
```

Set the selection mode to stream (wxSTC\_SEL\_STREAM) or rectangular
(wxSTC\_SEL\_RECTANGLE/wxSTC\_SEL\_THIN) or by lines (wxSTC\_SEL\_LINES).

# `setSelectionStart`

```erlang
-spec setSelectionStart(This, Anchor) -> ok when This :: wxStyledTextCtrl(), Anchor :: integer().
```

Sets the position that starts the selection - this becomes the anchor.

# `setSelForeground`

```erlang
-spec setSelForeground(This, UseSetting, Fore) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              UseSetting :: boolean(),
                              Fore :: wx:wx_colour().
```

Set the foreground colour of the main and additional selections and whether to use this
setting.

# `setSTCCursor`

```erlang
-spec setSTCCursor(This, CursorType) -> ok when This :: wxStyledTextCtrl(), CursorType :: integer().
```

Sets the cursor to one of the wxSTC_CURSOR* values.

# `setSTCFocus`

```erlang
-spec setSTCFocus(This, Focus) -> ok when This :: wxStyledTextCtrl(), Focus :: boolean().
```

Change internal focus flag.

# `setStatus`

```erlang
-spec setStatus(This, Status) -> ok when This :: wxStyledTextCtrl(), Status :: integer().
```

Change error status - 0 = OK.

The input should be one of the ?wxSTC\_STATUS\_\* constants.

# `setStyleBytes`

```erlang
-spec setStyleBytes(This, Length) -> integer() when This :: wxStyledTextCtrl(), Length :: integer().
```

Set the styles for a segment of the document.

# `setStyling`

```erlang
-spec setStyling(This, Length, Style) -> ok
                    when This :: wxStyledTextCtrl(), Length :: integer(), Style :: integer().
```

Change style from current styling position for length characters to a style and move the
current styling position to after this newly styled segment.

# `setTabIndents`

```erlang
-spec setTabIndents(This, TabIndents) -> ok when This :: wxStyledTextCtrl(), TabIndents :: boolean().
```

Sets whether a tab pressed when caret is within indentation indents.

# `setTabWidth`

```erlang
-spec setTabWidth(This, TabWidth) -> ok when This :: wxStyledTextCtrl(), TabWidth :: integer().
```

Change the visible size of a tab to be a multiple of the width of a space character.

# `setTargetEnd`

```erlang
-spec setTargetEnd(This, End) -> ok when This :: wxStyledTextCtrl(), End :: integer().
```

Sets the position that ends the target which is used for updating the document without
affecting the scroll position.

# `setTargetStart`

```erlang
-spec setTargetStart(This, Start) -> ok when This :: wxStyledTextCtrl(), Start :: integer().
```

Sets the position that starts the target which is used for updating the document without
affecting the scroll position.

# `setText`

```erlang
-spec setText(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: unicode:chardata().
```

Replace the contents of the document with the argument text.

# `setTextRaw`

```erlang
-spec setTextRaw(This, Text) -> ok when This :: wxStyledTextCtrl(), Text :: binary().
```

Replace the contents of the document with the argument text.

# `setTwoPhaseDraw`

```erlang
-spec setTwoPhaseDraw(This, TwoPhase) -> ok when This :: wxStyledTextCtrl(), TwoPhase :: boolean().
```

In twoPhaseDraw mode, drawing is performed in two phases, first the background and then
the foreground.

This avoids chopping off characters that overlap the next run.

# `setUndoCollection`

```erlang
-spec setUndoCollection(This, CollectUndo) -> ok
                           when This :: wxStyledTextCtrl(), CollectUndo :: boolean().
```

Choose between collecting actions into the undo history and discarding them.

# `setUseHorizontalScrollBar`

```erlang
-spec setUseHorizontalScrollBar(This, Visible) -> ok
                                   when This :: wxStyledTextCtrl(), Visible :: boolean().
```

Show or hide the horizontal scroll bar.

# `setUseTabs`

```erlang
-spec setUseTabs(This, UseTabs) -> ok when This :: wxStyledTextCtrl(), UseTabs :: boolean().
```

Indentation will only use space characters if useTabs is false, otherwise it will use a
combination of tabs and spaces.

# `setUseVerticalScrollBar`

```erlang
-spec setUseVerticalScrollBar(This, Visible) -> ok when This :: wxStyledTextCtrl(), Visible :: boolean().
```

Show or hide the vertical scroll bar.

# `setViewEOL`

```erlang
-spec setViewEOL(This, Visible) -> ok when This :: wxStyledTextCtrl(), Visible :: boolean().
```

Make the end of line characters visible or invisible.

# `setViewWhiteSpace`

```erlang
-spec setViewWhiteSpace(This, ViewWS) -> ok when This :: wxStyledTextCtrl(), ViewWS :: integer().
```

Make white space characters invisible, always visible or visible outside indentation.

The input should be one of the ?wxSTC\_WS\_\* constants.

# `setVisiblePolicy`

```erlang
-spec setVisiblePolicy(This, VisiblePolicy, VisibleSlop) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              VisiblePolicy :: integer(),
                              VisibleSlop :: integer().
```

Set the way the display area is determined when a particular line is to be moved to by
Find, FindNext, GotoLine, etc.

The first argument should be a bit list containing one or more of the ?wxSTC\_VISIBLE\_\*
constants.

# `setVScrollBar`

```erlang
-spec setVScrollBar(This, Bar) -> ok when This :: wxStyledTextCtrl(), Bar :: wxScrollBar:wxScrollBar().
```

Set the vertical scrollbar to use instead of the one that's built-in.

# `setWhitespaceBackground`

```erlang
-spec setWhitespaceBackground(This, UseSetting, Back) -> ok
                                 when
                                     This :: wxStyledTextCtrl(),
                                     UseSetting :: boolean(),
                                     Back :: wx:wx_colour().
```

Set the background colour of all whitespace and whether to use this setting.

# `setWhitespaceChars`

```erlang
-spec setWhitespaceChars(This, Characters) -> ok
                            when This :: wxStyledTextCtrl(), Characters :: unicode:chardata().
```

Set the set of characters making up whitespace for when moving or selecting by word.

Should be called after SetWordChars.

# `setWhitespaceForeground`

```erlang
-spec setWhitespaceForeground(This, UseSetting, Fore) -> ok
                                 when
                                     This :: wxStyledTextCtrl(),
                                     UseSetting :: boolean(),
                                     Fore :: wx:wx_colour().
```

Set the foreground colour of all whitespace and whether to use this setting.

# `setWordChars`

```erlang
-spec setWordChars(This, Characters) -> ok
                      when This :: wxStyledTextCtrl(), Characters :: unicode:chardata().
```

Set the set of characters making up words for when moving or selecting by word.

First sets defaults like SetCharsDefault.

# `setWrapMode`

```erlang
-spec setWrapMode(This, WrapMode) -> ok when This :: wxStyledTextCtrl(), WrapMode :: integer().
```

Sets whether text is word wrapped.

The input should be one of the ?wxSTC\_WRAP\_\* constants.

# `setWrapStartIndent`

```erlang
-spec setWrapStartIndent(This, Indent) -> ok when This :: wxStyledTextCtrl(), Indent :: integer().
```

Set the start indent for wrapped lines.

# `setWrapVisualFlags`

```erlang
-spec setWrapVisualFlags(This, WrapVisualFlags) -> ok
                            when This :: wxStyledTextCtrl(), WrapVisualFlags :: integer().
```

Set the display mode of visual flags for wrapped lines.

The input should be a bit list containing one or more of the ?wxSTC\_WRAPVISUALFLAG\_\*
constants.

# `setWrapVisualFlagsLocation`

```erlang
-spec setWrapVisualFlagsLocation(This, WrapVisualFlagsLocation) -> ok
                                    when
                                        This :: wxStyledTextCtrl(), WrapVisualFlagsLocation :: integer().
```

Set the location of visual flags for wrapped lines.

The input should be a bit list containing one or more of the
?wxSTC\_WRAPVISUALFLAGLOC\_\* constants.

# `setXCaretPolicy`

```erlang
-spec setXCaretPolicy(This, CaretPolicy, CaretSlop) -> ok
                         when
                             This :: wxStyledTextCtrl(),
                             CaretPolicy :: integer(),
                             CaretSlop :: integer().
```

Set the way the caret is kept visible when going sideways.

The exclusion zone is given in pixels.

The first argument should be a bit list containing one or more of the ?wxSTC\_CARET\_\*
constants.

# `setYCaretPolicy`

```erlang
-spec setYCaretPolicy(This, CaretPolicy, CaretSlop) -> ok
                         when
                             This :: wxStyledTextCtrl(),
                             CaretPolicy :: integer(),
                             CaretSlop :: integer().
```

Set the way the line the caret is on is kept visible.

The exclusion zone is given in lines.

The first argument should be a bit list containing one or more of the ?wxSTC\_CARET\_\*
constants.

# `setZoom`

```erlang
-spec setZoom(This, ZoomInPoints) -> ok when This :: wxStyledTextCtrl(), ZoomInPoints :: integer().
```

Set the zoom level.

This number of points is added to the size of all fonts. It may be positive to magnify or
negative to reduce.

# `showLines`

```erlang
-spec showLines(This, LineStart, LineEnd) -> ok
                   when This :: wxStyledTextCtrl(), LineStart :: integer(), LineEnd :: integer().
```

Make a range of lines visible.

# `startRecord`

```erlang
-spec startRecord(This) -> ok when This :: wxStyledTextCtrl().
```

Start notifying the container of all key presses and commands.

# `startStyling`

```erlang
-spec startStyling(This, Start) -> ok when This :: wxStyledTextCtrl(), Start :: integer().
```

Set the current styling position to start.

# `stopRecord`

```erlang
-spec stopRecord(This) -> ok when This :: wxStyledTextCtrl().
```

Stop notifying the container of all key presses and commands.

# `stutteredPageDown`

```erlang
-spec stutteredPageDown(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to bottom of page, or one page down if already at bottom of page.

# `stutteredPageDownExtend`

```erlang
-spec stutteredPageDownExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to bottom of page, or one page down if already at bottom of page, extending
selection to new caret position.

# `stutteredPageUp`

```erlang
-spec stutteredPageUp(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to top of page, or one page up if already at top of page.

# `stutteredPageUpExtend`

```erlang
-spec stutteredPageUpExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to top of page, or one page up if already at top of page, extending selection
to new caret position.

# `styleClearAll`

```erlang
-spec styleClearAll(This) -> ok when This :: wxStyledTextCtrl().
```

Clear all the styles and make equivalent to the global default style.

# `styleResetDefault`

```erlang
-spec styleResetDefault(This) -> ok when This :: wxStyledTextCtrl().
```

Reset the default style to its state at startup.

# `styleSetBackground`

```erlang
-spec styleSetBackground(This, Style, Back) -> ok
                            when This :: wxStyledTextCtrl(), Style :: integer(), Back :: wx:wx_colour().
```

Set the background colour of a style.

# `styleSetBold`

```erlang
-spec styleSetBold(This, Style, Bold) -> ok
                      when This :: wxStyledTextCtrl(), Style :: integer(), Bold :: boolean().
```

Set a style to be bold or not.

# `styleSetCase`

```erlang
-spec styleSetCase(This, Style, CaseVisible) -> ok
                      when This :: wxStyledTextCtrl(), Style :: integer(), CaseVisible :: integer().
```

Set a style to be mixed case, or to force upper or lower case.

The second argument should be one of the ?wxSTC\_CASE\_\* constants.

# `styleSetCharacterSet`

```erlang
-spec styleSetCharacterSet(This, Style, CharacterSet) -> ok
                              when
                                  This :: wxStyledTextCtrl(),
                                  Style :: integer(),
                                  CharacterSet :: integer().
```

Set the character set of the font in a style.

Converts the Scintilla character set values to a wxFontEncoding.

# `styleSetEOLFilled`

```erlang
-spec styleSetEOLFilled(This, Style, EolFilled) -> ok
                           when This :: wxStyledTextCtrl(), Style :: integer(), EolFilled :: boolean().
```

Set a style to have its end of line filled or not.

# `styleSetFaceName`

```erlang
-spec styleSetFaceName(This, Style, FontName) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              Style :: integer(),
                              FontName :: unicode:chardata().
```

Set the font of a style.

# `styleSetFont`

```erlang
-spec styleSetFont(This, StyleNum, Font) -> ok
                      when This :: wxStyledTextCtrl(), StyleNum :: integer(), Font :: wxFont:wxFont().
```

Set style size, face, bold, italic, and underline attributes from a `m:wxFont`'s
attributes.

# `styleSetFontAttr`

```erlang
-spec styleSetFontAttr(This, StyleNum, Size, FaceName, Bold, Italic, Underline) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              StyleNum :: integer(),
                              Size :: integer(),
                              FaceName :: unicode:chardata(),
                              Bold :: boolean(),
                              Italic :: boolean(),
                              Underline :: boolean().
```

# `styleSetFontAttr`

```erlang
-spec styleSetFontAttr(This, StyleNum, Size, FaceName, Bold, Italic, Underline, [Option]) -> ok
                          when
                              This :: wxStyledTextCtrl(),
                              StyleNum :: integer(),
                              Size :: integer(),
                              FaceName :: unicode:chardata(),
                              Bold :: boolean(),
                              Italic :: boolean(),
                              Underline :: boolean(),
                              Option :: {encoding, wx:wx_enum()}.
```

Set all font style attributes at once.

# `styleSetFontEncoding`

```erlang
-spec styleSetFontEncoding(This, Style, Encoding) -> ok
                              when
                                  This :: wxStyledTextCtrl(),
                                  Style :: integer(),
                                  Encoding :: wx:wx_enum().
```

Set the font encoding to be used by a style.

# `styleSetForeground`

```erlang
-spec styleSetForeground(This, Style, Fore) -> ok
                            when This :: wxStyledTextCtrl(), Style :: integer(), Fore :: wx:wx_colour().
```

Set the foreground colour of a style.

# `styleSetHotSpot`

```erlang
-spec styleSetHotSpot(This, Style, Hotspot) -> ok
                         when This :: wxStyledTextCtrl(), Style :: integer(), Hotspot :: boolean().
```

Set a style to be a hotspot or not.

# `styleSetItalic`

```erlang
-spec styleSetItalic(This, Style, Italic) -> ok
                        when This :: wxStyledTextCtrl(), Style :: integer(), Italic :: boolean().
```

Set a style to be italic or not.

# `styleSetSize`

```erlang
-spec styleSetSize(This, Style, SizePoints) -> ok
                      when This :: wxStyledTextCtrl(), Style :: integer(), SizePoints :: integer().
```

Set the size of characters of a style.

# `styleSetSpec`

```erlang
-spec styleSetSpec(This, StyleNum, Spec) -> ok
                      when This :: wxStyledTextCtrl(), StyleNum :: integer(), Spec :: unicode:chardata().
```

Extract style settings from a spec-string which is composed of one or more of the
following comma separated elements:

bold turns on bold italic turns on italics fore:[name or #RRGGBB] sets the foreground
colour back:[name or #RRGGBB] sets the background colour face:[facename] sets the font
face name to use size:[num] sets the font size in points eol turns on eol filling
underline turns on underlining

# `styleSetUnderline`

```erlang
-spec styleSetUnderline(This, Style, Underline) -> ok
                           when This :: wxStyledTextCtrl(), Style :: integer(), Underline :: boolean().
```

Set a style to be underlined or not.

# `styleSetVisible`

```erlang
-spec styleSetVisible(This, Style, Visible) -> ok
                         when This :: wxStyledTextCtrl(), Style :: integer(), Visible :: boolean().
```

Set a style to be visible or not.

# `tab`

```erlang
-spec tab(This) -> ok when This :: wxStyledTextCtrl().
```

If selection is empty or all on one line replace the selection with a tab character.

If more than one line selected, indent the lines.

# `targetFromSelection`

```erlang
-spec targetFromSelection(This) -> ok when This :: wxStyledTextCtrl().
```

Make the target range start and end be the same as the selection range start and end.

# `textHeight`

```erlang
-spec textHeight(This, Line) -> integer() when This :: wxStyledTextCtrl(), Line :: integer().
```

Retrieve the height of a particular line of text in pixels.

# `textWidth`

```erlang
-spec textWidth(This, Style, Text) -> integer()
                   when This :: wxStyledTextCtrl(), Style :: integer(), Text :: unicode:chardata().
```

Measure the pixel width of some text in a particular style.

Does not handle tab or control characters.

# `toggleCaretSticky`

```erlang
-spec toggleCaretSticky(This) -> ok when This :: wxStyledTextCtrl().
```

Switch between sticky and non-sticky: meant to be bound to a key.

# `toggleFold`

```erlang
-spec toggleFold(This, Line) -> ok when This :: wxStyledTextCtrl(), Line :: integer().
```

Switch a header line between expanded and contracted.

# `undo`

```erlang
-spec undo(This) -> ok when This :: wxStyledTextCtrl().
```

Undo one action in the undo history.

# `upperCase`

```erlang
-spec upperCase(This) -> ok when This :: wxStyledTextCtrl().
```

Transform the selection to upper case.

# `usePopUp`

```erlang
-spec usePopUp(This, PopUpMode) -> ok when This :: wxStyledTextCtrl(), PopUpMode :: integer().
```

Set whether a pop up menu is displayed automatically when the user presses the wrong
mouse button on certain areas.

The input should be one of the ?wxSTC\_POPUP\_\* constants.

Remark: When `m:wxContextMenuEvent` is used to create a custom popup menu, this function
should be called with wxSTC_POPUP_NEVER. Otherwise the default menu will be shown instead
of the custom one.

# `userListShow`

```erlang
-spec userListShow(This, ListType, ItemList) -> ok
                      when
                          This :: wxStyledTextCtrl(),
                          ListType :: integer(),
                          ItemList :: unicode:chardata().
```

Display a list of strings and send notification when user chooses one.

# `vCHome`

```erlang
-spec vCHome(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to before first visible character on line.

If already there move to first character on line.

# `vCHomeExtend`

```erlang
-spec vCHomeExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Like VCHome but extending selection to new caret position.

# `vCHomeRectExtend`

```erlang
-spec vCHomeRectExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret to before first visible character on line.

If already there move to first character on line. In either case, extend rectangular
selection to new caret position.

# `vCHomeWrap`

```erlang
-spec vCHomeWrap(This) -> ok when This :: wxStyledTextCtrl().
```

Like VCHome but when word-wrap is enabled goes first to start of display line
VCHomeDisplay, then behaves like VCHome.

# `vCHomeWrapExtend`

```erlang
-spec vCHomeWrapExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Like VCHomeExtend but when word-wrap is enabled extends first to start of display line
VCHomeDisplayExtend, then behaves like VCHomeExtend.

# `visibleFromDocLine`

```erlang
-spec visibleFromDocLine(This, DocLine) -> integer()
                            when This :: wxStyledTextCtrl(), DocLine :: integer().
```

Find the display line of a document line taking hidden lines into account.

# `wordEndPosition`

```erlang
-spec wordEndPosition(This, Pos, OnlyWordCharacters) -> integer()
                         when
                             This :: wxStyledTextCtrl(),
                             Pos :: integer(),
                             OnlyWordCharacters :: boolean().
```

Get position of end of word.

# `wordLeft`

```erlang
-spec wordLeft(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one word.

# `wordLeftEnd`

```erlang
-spec wordLeftEnd(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one word, position cursor at end of word.

# `wordLeftEndExtend`

```erlang
-spec wordLeftEndExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one word, position cursor at end of word, extending selection to new
caret position.

# `wordLeftExtend`

```erlang
-spec wordLeftExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret left one word extending selection to new caret position.

# `wordPartLeft`

```erlang
-spec wordPartLeft(This) -> ok when This :: wxStyledTextCtrl().
```

Move to the previous change in capitalisation.

# `wordPartLeftExtend`

```erlang
-spec wordPartLeftExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move to the previous change in capitalisation extending selection to new caret position.

# `wordPartRight`

```erlang
-spec wordPartRight(This) -> ok when This :: wxStyledTextCtrl().
```

Move to the change next in capitalisation.

# `wordPartRightExtend`

```erlang
-spec wordPartRightExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move to the next change in capitalisation extending selection to new caret position.

# `wordRight`

```erlang
-spec wordRight(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one word.

# `wordRightEnd`

```erlang
-spec wordRightEnd(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one word, position cursor at end of word.

# `wordRightEndExtend`

```erlang
-spec wordRightEndExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one word, position cursor at end of word, extending selection to new
caret position.

# `wordRightExtend`

```erlang
-spec wordRightExtend(This) -> ok when This :: wxStyledTextCtrl().
```

Move caret right one word extending selection to new caret position.

# `wordStartPosition`

```erlang
-spec wordStartPosition(This, Pos, OnlyWordCharacters) -> integer()
                           when
                               This :: wxStyledTextCtrl(),
                               Pos :: integer(),
                               OnlyWordCharacters :: boolean().
```

Get position of start of word.

# `wrapCount`

```erlang
-spec wrapCount(This, DocLine) -> integer() when This :: wxStyledTextCtrl(), DocLine :: integer().
```

The number of display lines needed to wrap a document line.

# `zoomIn`

```erlang
-spec zoomIn(This) -> ok when This :: wxStyledTextCtrl().
```

Magnify the displayed text by increasing the sizes by 1 point.

# `zoomOut`

```erlang
-spec zoomOut(This) -> ok when This :: wxStyledTextCtrl().
```

Make the displayed text smaller by decreasing the sizes by 1 point.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
