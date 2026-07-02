# `io_ansi`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/stdlib/src/io_ansi.erl#L30)

Controlling the terminal using virtual terminal sequences (aka [ANSI escape codes]).

This module provides an interface to emit and parse virtual terminal sequences (VTS),
also known as [ANSI escape codes]. VTS can be used to:

- change the style of text or background in the terminal by adding color or emphasis.
- delete printed characters or lines.
- move, hide or show the cursor

and more things. As different terminals are interpret VTSs slightly
differently, `m:io_ansi` uses the local [terminfo] database together with
predefined sequences to emit the correct sequence for the terminal that is
currently used. To fetch values directly from the [terminfo] database you can use
`tput/2`, `tigetnum/1` and `tigetflag/1`.

`m:io_ansi` provides two interfaces to emit sequences. You can either call the
function representing the sequence you want to emit, for example `io_ansi:blue()`
and it will return the sequence representing blue.

```erlang
1> io_ansi:blue().
<<"\e[34m">>
```

This will use the [terminfo] database locally where the call is made, so it may
not be correct if used across nodes.

You can also use the [`io_ansi:format/1,2,3`](`io_ansi:format/3`) functions
which works just as `io_lib:bformat/3`, except that it also accepts atoms and
tuples that represent VTSs. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red]).
<<"\e[34mred\e(B\e[m">>
```

`io_ansi:format/3` will automatically reset the terminal to its original state
and strip any VTSs that are not supported by the terminal. It can also be disabled
through an option. For example:

```erlang
1> io_ansi:format([blue,"~p"], [red], [{enabled, false}]).
<<"red">>
```

Finally there is [`io_ansi:fwrite/1,2,3,4`](`io_ansi:fwrite/4`) which does not
return the string to be printed, but instead sends it to the `t:io:device/0`
that should handle it. `io_ansi:fwrite/4` works across nodes and will use the
[terminfo] database where the data is outputted to decide what to emit.

[terminfo]: https://man7.org/linux/man-pages/man5/terminfo.5.html
[ANSI escape codes]: https://en.wikipedia.org/wiki/ANSI_escape_code

# `background_color`
*not exported* *since OTP 29.0* 

```erlang
-type background_color() ::
          black_background | blue_background | cyan_background | green_background | magenta_background |
          red_background | white_background | yellow_background | default_background |
          light_black_background | light_blue_background | light_cyan_background |
          light_green_background | light_magenta_background | light_red_background |
          light_white_background | light_yellow_background |
          {background, 0..255} |
          {background, R :: 0..255, G :: 0..255, B :: 0..255}.
```

Virtual terminal sequences that control the background color.

# `color`
*not exported* *since OTP 29.0* 

```erlang
-type color() ::
          foreground_color() |
          background_color() |
          underline_color() |
          {modify_color, Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255}.
```

Virtual terminal sequences that control color.

# `color_atom`
*not exported* *since OTP 29.0* 

```erlang
-type color_atom() ::
          black | blue | cyan | green | magenta | red | white | yellow | light_black | light_blue |
          light_cyan | light_green | light_magenta | light_red | light_white | light_yellow.
```

# `cursor`
*not exported* *since OTP 29.0* 

```erlang
-type cursor() ::
          {cursor, Line :: non_neg_integer(), Column :: non_neg_integer()} |
          {cursor_move, DeltaLine :: integer(), DeltaColumn :: integer()} |
          cursor_down | cursor_up | cursor_backward | cursor_forward |
          {cursor_down | cursor_backward | cursor_forward | cursor_up, N :: non_neg_integer()} |
          cursor_home | reverse_index | cursor_save | cursor_restore | cursor_show | cursor_hide |
          cursor_next_line | cursor_previous_line | cursor_horizontal_absolute |
          cursor_vertical_absolute | cursor_horizontal_vertical | cursor_report_position.
```

Virtual terminal sequences that controls the cursor.

# `foreground_color`
*not exported* *since OTP 29.0* 

```erlang
-type foreground_color() ::
          color_atom() |
          {color, 0..255} |
          {color, R :: 0..255, G :: 0..255, B :: 0..255} |
          default_color.
```

Virtual terminal sequences that control the foreground (aka text) color.

# `format`
*not exported* *since OTP 29.0* 

```erlang
-type format() :: [string() | vts()].
```

The format string that can be passed to `format/3` and `fwrite/4`

# `hyperlink`
*not exported* *since OTP 29.0* 

```erlang
-type hyperlink() ::
          {hyperlink, URL :: uri_string:uri_string(), Text :: unicode:chardata()} |
          {hyperlink, URL :: uri_string:uri_string(), hyperlink_params(), Text :: unicode:chardata()} |
          {hyperlink_start, URL :: uri_string:uri_string()} |
          {hyperlink_start, URL :: uri_string:uri_string(), hyperlink_params()} |
          hyperlink_reset.
```

Virtual terminal sequences that control whether emitted text shall be a hyper link or not.

# `hyperlink_params`
*not exported* *since OTP 29.0* 

```erlang
-type hyperlink_params() :: [{Key :: unicode:chardata(), Value :: unicode:chardata()}].
```

# `input`
*not exported* *since OTP 29.0* 

```erlang
-type input() ::
          keypad_transmit_mode | keypad_transmit_mode_off | kcursor_down | kcursor_up |
          kcursor_backward | kcursor_forward | kcursor_home | kcursor_end.
```

Virtual terminal sequences for cursor input.

# `option`
*not exported* *since OTP 29.0* 

```erlang
-type option() ::
          {reset, boolean()} | {enabled, boolean()} | {color, boolean()} | io_lib:format_options().
```

# `options`
*not exported* *since OTP 29.0* 

```erlang
-type options() :: [option()].
```

# `style`
*not exported* *since OTP 29.0* 

```erlang
-type style() ::
          bold | bold_off | blink | blink_off | dim | dim_off | invisible | invisible_off | italic |
          italic_off | inverse | inverse_off | overline | overline_off | strikethrough |
          strikethrough_off |
          underline_style().
```

Virtual terminal sequences that control text style.

# `tab`
*not exported* *since OTP 29.0* 

```erlang
-type tab() :: tab | tab_backward | tab_set | tab_clear | tab_clear_all.
```

Virtual terminal sequences that works with tabs.

# `text`
*not exported* *since OTP 29.0* 

```erlang
-type text() ::
          text_formatting() |
          text_modification() |
          alternate_character_set_mode | alternate_character_set_mode_off.
```

Virtual terminal sequences that works on text.

# `text_formatting`
*not exported* *since OTP 29.0* 

```erlang
-type text_formatting() :: color() | style() | hyperlink().
```

Virtual terminal sequences that control text formatting.

# `text_modification`
*not exported* *since OTP 29.0* 

```erlang
-type text_modification() ::
          clear | erase_display | insert_character | delete_character | erase_character | insert_line |
          delete_line | erase_line.
```

Virtual terminal sequences that can erase or overwrite text.

# `underline_color`
*not exported* *since OTP 29.0* 

```erlang
-type underline_color() ::
          black_underline | blue_underline | cyan_underline | green_underline | magenta_underline |
          red_underline | white_underline | yellow_underline | default_underline |
          light_black_underline | light_blue_underline | light_cyan_underline | light_green_underline |
          light_magenta_underline | light_red_underline | light_white_underline |
          light_yellow_underline |
          {underline_color, 0..255} |
          {underline_color, R :: 0..255, G :: 0..255, B :: 0..255}.
```

Virtual terminal sequences that control underline color.

# `underline_style`
*not exported* *since OTP 29.0* 

```erlang
-type underline_style() ::
          underline | underline_off | double_underline | curly_underline | dotted_underline |
          dashed_underline.
```

Virtual terminal sequences that control underline style.

# `vts`
*since OTP 29.0* 

```erlang
-type vts() :: text() | cursor() | window() | tab() | input() | reset | device_report_attributes.
```

Virtual terminal sequences.

# `window`
*not exported* *since OTP 29.0* 

```erlang
-type window() ::
          alternate_screen | alternate_screen_off | scroll_forward | scroll_backward |
          scroll_change_region.
```

Virtual terminal sequences that controls the screen.

# `alternate_character_set_mode`
*since OTP 29.0* 

```erlang
-spec alternate_character_set_mode() -> unicode:chardata().
```

Enable the alternate characters set mode

Example:
```erlang
1> io_ansi:alternate_character_set_mode().
<<"\e(0">>
2> io_ansi:fwrite(["%%", alternate_character_set_mode, " tqqu\n", alternate_character_set_mode_off]).
%% ├──┤
ok
```

# `alternate_character_set_mode_off`
*since OTP 29.0* 

```erlang
-spec alternate_character_set_mode_off() -> unicode:chardata().
```

Disable the alternate characters set mode

Example:
```erlang
1> io_ansi:alternate_character_set_mode_off().
<<"\e(B">>
```

# `alternate_screen`
*since OTP 29.0* 

```erlang
-spec alternate_screen() -> unicode:chardata().
```

Activate the alternate screen buffer.

The alternate screen buffer is a separate screen buffer that full-screen terminal
applications (like `vim`, `less`, `htop`, or `man`) use to display their content.
When activated, the current screen content and cursor position are saved. When
deactivated with `alternate_screen_off/0`, the original screen content is restored,
making it appear as if the full-screen application was never there.

Example:
```erlang
1> io_ansi:alternate_screen().
<<"\e[?1049h\e[22;0;0t">>
```

# `alternate_screen_off`
*since OTP 29.0* 

```erlang
-spec alternate_screen_off() -> unicode:chardata().
```

Deactivate the alternate screen.

Example:
```erlang
1> io_ansi:alternate_screen_off().
<<"\e[?1049l\e[23;0;0t">>
```

# `background`
*since OTP 29.0* 

```erlang
-spec background(Index :: 0..255 | 0..87 | color_atom()) -> unicode:chardata().
```

Change background color to index color. `Index` 0-15 are equivilant to
the named colors in `t:background_color/0` in the order that they are listed.

Example:
```erlang
1> io_ansi:background(2).
<<"\e[42m">>
2> io_ansi:background(80).
<<"\e[48;5;80m">>
```

# `background`
*since OTP 29.0* 

```erlang
-spec background(0..255, 0..255, 0..255) -> unicode:chardata().
```

Change background color to RGB color.

Example:
```erlang
1> io_ansi:background(255, 255, 0).
<<"\e[48;2;255;255;0m">>
```

# `black`
*since OTP 29.0* 

```erlang
-spec black() -> unicode:chardata().
```

Change foreground (aka text) color to black.

Example:
```erlang
1> io_ansi:black().
<<"\e[30m">>
```

# `black_background`
*since OTP 29.0* 

```erlang
-spec black_background() -> unicode:chardata().
```

Change background color to black.

Example:
```erlang
1> io_ansi:black_background().
<<"\e[40m">>
```

# `black_underline`
*since OTP 29.0* 

```erlang
-spec black_underline() -> unicode:chardata().
```

Change underline color to black. Not widely supported.

Example:
```erlang
1> io_ansi:black_underline().
<<"\e[58;5;0m">>
```

# `blink`
*since OTP 29.0* 

```erlang
-spec blink() -> unicode:chardata().
```

Turn on blink text style. Not widely supported.

Example:
```erlang
1> io_ansi:blink().
<<"\e[5m">>
```

# `blink_off`
*since OTP 29.0* 

```erlang
-spec blink_off() -> unicode:chardata().
```

Turn off blink text style.

Example:
```erlang
1> io_ansi:blink_off().
<<"\e[25m">>
```

# `blue`
*since OTP 29.0* 

```erlang
-spec blue() -> unicode:chardata().
```

Change foreground (aka text) color to blue.

Example:
```erlang
1> io_ansi:blue().
<<"\e[34m">>
```

# `blue_background`
*since OTP 29.0* 

```erlang
-spec blue_background() -> unicode:chardata().
```

Change background color to blue.

Example:
```erlang
1> io_ansi:blue_background().
<<"\e[44m">>
```

# `blue_underline`
*since OTP 29.0* 

```erlang
-spec blue_underline() -> unicode:chardata().
```

Change underline color to blue. Not widely supported.

Example:
```erlang
1> io_ansi:blue_underline().
<<"\e[58;5;4m">>
```

# `bold`
*since OTP 29.0* 

```erlang
-spec bold() -> unicode:chardata().
```

Turn on bold text style.

Example:
```erlang
1> io_ansi:bold().
<<"\e[1m">>
```

# `bold_off`
*since OTP 29.0* 

```erlang
-spec bold_off() -> unicode:chardata().
```

Turn off bold text style.

Example:
```erlang
1> io_ansi:bold_off().
<<"\e[22m">>
```

# `clear`
*since OTP 29.0* 

```erlang
-spec clear() -> unicode:chardata().
```

Clear screen and set cursor to home.

Example:
```erlang
1> io_ansi:clear().
<<"\e[H\e[2J">>
```

# `color`
*since OTP 29.0* 

```erlang
-spec color(Index :: 0..255 | 0..87 | color_atom()) -> unicode:chardata().
```

Change foreground (aka text) color to index color. `Index` 0-15 are equivalent to
the named colors in `t:foreground_color/0` in the order that they are listed.

Example:
```erlang
1> io_ansi:color(5).
<<"\e[35m">>
2> io_ansi:color(80).
<<"\e[38;5;80m">>
```

# `color`
*since OTP 29.0* 

```erlang
-spec color(0..255, 0..255, 0..255) -> unicode:chardata().
```

Change foreground (aka text) color to RGB color.

Example:
```erlang
1> io_ansi:color(255, 0, 0).
<<"\e[38;2;255;0;0m">>
```

# `color_name_to_index`
*since OTP 29.0* 

```erlang
-spec color_name_to_index(Color :: color_atom()) -> 0..15.
```

Convert a color atom to its corresponding index (0-15).

Example:
```erlang
1> io_ansi:color_name_to_index(red).
1
```

# `curly_underline`
*since OTP 29.0* 

```erlang
-spec curly_underline() -> unicode:chardata().
```

Turn on curly underline text style. Not widely supported.

Example:
```erlang
1> io_ansi:curly_underline().
<<"\e[4:3m">>
```

# `cursor`
*since OTP 29.0* 

```erlang
-spec cursor(Line :: integer(), Column :: integer()) -> unicode:chardata().
```

Move the cursor to the given position. Position 0,0 is at the top left of the
terminal.

Example:
```erlang
1> io_ansi:cursor(5, 10).
<<"\e[6;11H">>
```

# `cursor_backward`
*since OTP 29.0* 

```erlang
-spec cursor_backward() -> unicode:chardata().
```

Move the cursor backward `N` characters.

Example:
```erlang
1> io_ansi:cursor_backward().
<<"\b">>
```

# `cursor_backward`
*since OTP 29.0* 

```erlang
-spec cursor_backward(N :: integer()) -> unicode:chardata().
```

Move the cursor backward `N` characters.

Example:
```erlang
1> io_ansi:cursor_backward(42).
<<"\e[42D">>
```

# `cursor_down`
*since OTP 29.0* 

```erlang
-spec cursor_down() -> unicode:chardata().
```

Move the cursor down one line.

Example:
```erlang
1> io_ansi:cursor_down().
<<"\n">>
```

# `cursor_down`
*since OTP 29.0* 

```erlang
-spec cursor_down(N :: integer()) -> unicode:chardata().
```

Move the cursor down `N` lines.

Example:
```erlang
1> io_ansi:cursor_down(42).
<<"\e[42B">>
```

# `cursor_forward`
*since OTP 29.0* 

```erlang
-spec cursor_forward() -> unicode:chardata().
```

Move the cursor forward one character.

Example:
```erlang
1> io_ansi:cursor_forward().
<<"\e[C">>
```

# `cursor_forward`
*since OTP 29.0* 

```erlang
-spec cursor_forward(N :: integer()) -> unicode:chardata().
```

Move the cursor forward `N` characters.

Example:
```erlang
1> io_ansi:cursor_forward().
<<"\e[C">>
```

# `cursor_get_position`
*since OTP 29.0* 

```erlang
-spec cursor_get_position() ->
                             {Row :: non_neg_integer(), Column :: non_neg_integer()} | {error, string()}.
```

Get the current cursor position as {Row, Column}.
Example:
```bash
## Enter noshell-raw mode and request cursor location and then print
## the reply to stdout.
$ erl -noshell -eval 'shell:start_interactive({noshell,raw}),
    {Row,Col} = io_ansi:cursor_get_position(),
    io:format("~p",[{Row,Col}])' -s init stop
{58,1}
```

# `cursor_hide`
*since OTP 29.0* 

```erlang
-spec cursor_hide() -> unicode:chardata().
```

Hide the cursor.

Example:
```erlang
1> io_ansi:cursor_hide().
<<"\e[?25l">>
```

# `cursor_home`
*since OTP 29.0* 

```erlang
-spec cursor_home() -> unicode:chardata().
```

Move the cursor to the start of the current line.

Example:
```erlang
1> io_ansi:cursor_home().
<<"\e[H">>
```

# `cursor_horizontal_absolute`
*since OTP 29.0* 

```erlang
-spec cursor_horizontal_absolute(X :: integer()) -> unicode:chardata().
```

Move the cursor to column `X`.

Example:
```erlang
1> io_ansi:cursor_horizontal_absolute(10).
<<"\e[11G">>
```

# `cursor_horizontal_vertical`
*since OTP 29.0* 

```erlang
-spec cursor_horizontal_vertical(X :: integer(), Y :: integer()) -> unicode:chardata().
```

Move the cursor to line `X` and column `Y`. Position 1,1 is at the top left of the
terminal.

Example:
```erlang
1> io_ansi:cursor_horizontal_vertical(10, 20).
<<"\e[10;20f">>
```

# `cursor_move`
*since OTP 29.0* 

```erlang
-spec cursor_move(DeltaLine :: integer(), DeltaColumn :: integer()) -> unicode:chardata().
```

Move the cursor relative to its current position.

Positive `DeltaLine` moves down, negative moves up.
Positive `DeltaColumn` moves right (forward), negative moves left (backward).

Example:
```erlang
1> io_ansi:cursor_move(-5, 0).
<<"\e[5A">>
2> io_ansi:cursor_move(0, 10).
<<"\e[10C">>
3> io_ansi:cursor_move(3, -2).
<<"\e[3B\e[2D">>
4> io_ansi:cursor_move(0, 0).
<<>>
```

# `cursor_next_line`
*since OTP 29.0* 

```erlang
-spec cursor_next_line() -> unicode:chardata().
```

Move the cursor down one line and then returns it to home.

Example:
```erlang
> io_ansi:cursor_next_line().
<<"\e[E">>
```

# `cursor_previous_line`
*since OTP 29.0* 

```erlang
-spec cursor_previous_line() -> unicode:chardata().
```

Move the cursor up one line and then returns it to home.

Example:
```erlang
1> io_ansi:cursor_previous_line().
<<"\e[F">>
```

# `cursor_report_position`
*since OTP 29.0* 

```erlang
-spec cursor_report_position() -> unicode:chardata().
```

Instruct the terminal to report the current cursor position.

Examples:

```erlang
1> io_ansi:cursor_report_position().
~"\e[6n"
```

```bash
## Enter noshell-raw mode and request cursor location and then print
## the reply to stdout.
$ erl -noshell -eval 'shell:start_interactive({noshell,raw}),
    io_ansi:fwrite([cursor_report_position]),
    io:format("~p",[io:get_chars("",20)])' -s init stop
"\e[58;1R"
```
The reported cursor position in the example is row 58 and column 1 both are 1 index based.

# `cursor_restore`
*since OTP 29.0* 

```erlang
-spec cursor_restore() -> unicode:chardata().
```

Restore a saved cursor position.

Example:
```erlang
1> io_ansi:cursor_restore().
<<"\e8">>
```

# `cursor_save`
*since OTP 29.0* 

```erlang
-spec cursor_save() -> unicode:chardata().
```

Save the current cursor position.

Example:
```erlang
1> io_ansi:cursor_save().
<<"\e7">>
```

# `cursor_show`
*since OTP 29.0* 

```erlang
-spec cursor_show() -> unicode:chardata().
```

Show the cursor.

Example:
```erlang
1> io_ansi:cursor_show().
<<"\e[?12;25h">>
```

# `cursor_up`
*since OTP 29.0* 

```erlang
-spec cursor_up() -> unicode:chardata().
```

Move the cursor up one line.

Example:
```erlang
1> io_ansi:cursor_up().
<<"\e[A">>
```

# `cursor_up`
*since OTP 29.0* 

```erlang
-spec cursor_up(N :: integer()) -> unicode:chardata().
```

Move the cursor up `N` lines.

Example:
```erlang
1> io_ansi:cursor_up(42).
<<"\e[42A">>
```

# `cursor_vertical_absolute`
*since OTP 29.0* 

```erlang
-spec cursor_vertical_absolute(X :: integer()) -> unicode:chardata().
```

Move the cursor to line `X`.

Example:
```erlang
1> io_ansi:cursor_vertical_absolute(20).
<<"\e[21d">>
```

# `cyan`
*since OTP 29.0* 

```erlang
-spec cyan() -> unicode:chardata().
```

Change foreground (aka text) color to cyan.

Example:
```erlang
1> io_ansi:cyan().
<<"\e[36m">>
```

# `cyan_background`
*since OTP 29.0* 

```erlang
-spec cyan_background() -> unicode:chardata().
```

Change background color to cyan.

Example:
```erlang
1> io_ansi:cyan_background().
<<"\e[46m">>
```

# `cyan_underline`
*since OTP 29.0* 

```erlang
-spec cyan_underline() -> unicode:chardata().
```

Change underline color to cyan. Not widely supported.

Example:
```erlang
1> io_ansi:cyan_underline().
<<"\e[58;5;6m">>
```

# `dashed_underline`
*since OTP 29.0* 

```erlang
-spec dashed_underline() -> unicode:chardata().
```

Turn on dashed underline text style. Not widely supported.

Example:
```erlang
1> io_ansi:dashed_underline().
<<"\e[4:5m">>
```

# `default_background`
*since OTP 29.0* 

```erlang
-spec default_background() -> unicode:chardata().
```

Change background color to the default color.

Example:
```erlang
1> io_ansi:default_background().
<<"\e[49m">>
```

# `default_color`
*since OTP 29.0* 

```erlang
-spec default_color() -> unicode:chardata().
```

Change foreground (aka text) color to the default color.

Example:
```erlang
1> io_ansi:default_color().
<<"\e[39m">>
```

# `default_underline_color`
*since OTP 29.0* 

```erlang
-spec default_underline_color() -> unicode:chardata().
```

Reset underline color to the default.

Example:
```erlang
1> io_ansi:default_underline_color().
<<"\e[59m">>
```

# `delete_character`
*since OTP 29.0* 

```erlang
-spec delete_character() -> unicode:chardata().
```

Delete 1 character at cursor.

Example:
```erlang
1> io_ansi:delete_character().
<<"\e[P">>
```

# `delete_character`
*since OTP 29.0* 

```erlang
-spec delete_character(Chars :: integer()) -> unicode:chardata().
```

Delete `Chars` characters at cursor by shifting the text `Chars` characters to the left.

Example:
```erlang
1> io_ansi:delete_character(2).
<<"\e[2P">>
```

# `delete_line`
*since OTP 29.0* 

```erlang
-spec delete_line() -> unicode:chardata().
```

Delete 1 line at cursor.

Example:
```erlang
1> io_ansi:delete_line().
<<"\e[M">>
```

# `delete_line`
*since OTP 29.0* 

```erlang
-spec delete_line(Lines :: integer()) -> unicode:chardata().
```

Delete `Lines` lines at cursor.

Example:
```erlang
1> io_ansi:delete_line(3).
<<"\e[3M">>
```

# `device_report_attributes`
*since OTP 29.0* 

```erlang
-spec device_report_attributes() -> unicode:chardata().
```

Tell the terminal emulator to report its device attributes.

Examples:

```erlang
1> io_ansi:device_report_attributes().
<<"\e[0c">>
```

```sh
## Enter noshell-raw mode and request device attributes and then print
## the reply to stdout.
$ erl -noshell -eval 'shell:start_interactive({noshell,raw}),
     io_ansi:fwrite([device_report_attributes]),
     io:format("~p",[io:get_chars("",20)])' -s init stop
"\e[?65;1;9c"
```

# `dim`
*since OTP 29.0* 

```erlang
-spec dim() -> unicode:chardata().
```

Turn on dim text style. Not widely supported.

Example:
```erlang
1> io_ansi:dim().
<<"\e[2m">>
```

# `dim_off`
*since OTP 29.0* 

```erlang
-spec dim_off() -> unicode:chardata().
```

Turn off dim text style.

Example:
```erlang
1> io_ansi:dim_off().
<<"\e[22m">>
```

# `dotted_underline`
*since OTP 29.0* 

```erlang
-spec dotted_underline() -> unicode:chardata().
```

Turn on dotted underline text style. Not widely supported.

Example:
```erlang
1> io_ansi:dotted_underline().
<<"\e[4:4m">>
```

# `double_underline`
*since OTP 29.0* 

```erlang
-spec double_underline() -> unicode:chardata().
```

Turn on double underline text style. Not widely supported.

Example:
```erlang
1> io_ansi:double_underline().
<<"\e[4:2m">>
```

# `enabled`
*since OTP 29.0* 

```erlang
-spec enabled() -> boolean().
```

Check if `t:io:user/0` can interpret ANSI escape sequences.

Example:
```erlang
1> io_ansi:enabled().
true
```

# `enabled`
*since OTP 29.0* 

```erlang
-spec enabled(io:device()) -> boolean().
```

Check if `Device` can interpret ANSI escape sequences.

This is done by checking if `Device` represents a terminal and if the `TERM`
environment variable is set to a terminal type that supports virtual terminal
sequences.

Example:
```erlang
1> io_ansi:enabled(standard_error).
true
2> {ok, File} = file:open("tmp",[write]), io_ansi:enabled(File).
false
```

# `erase_character`
*since OTP 29.0* 

```erlang
-spec erase_character(Chars :: integer()) -> unicode:chardata().
```

Erase `Chars` characters at cursor by making `Chars` characters before the cursor blank.

Example:
```erlang
1> io_ansi:erase_character(4).
<<"\e[4X">>
```

# `erase_display`
*since OTP 29.0* 

```erlang
-spec erase_display() -> unicode:chardata().
```

Clear screen after cursor.

Example:
```erlang
1> io_ansi:erase_display().
<<"\e[J">>
```

# `erase_line`
*since OTP 29.0* 

```erlang
-spec erase_line() -> unicode:chardata().
```

Erase line at cursor.

Example:
```erlang
1> io_ansi:erase_line().
<<"\e[K">>
```

# `format`
*since OTP 29.0* 

```erlang
-spec format(format()) -> unicode:unicode_binary().
```

# `format`
*since OTP 29.0* 

```erlang
-spec format(format(), Data :: [term()]) -> unicode:unicode_binary().
```

# `format`
*since OTP 29.0* 

```erlang
-spec format(format(), Data :: [term()], options()) -> unicode:unicode_binary().
```

Returns a character list that represents `Data` formatted in accordance with
`Format`.

This function works just as `io_lib:bformat/2`, where `Data` is a list of strings
as well as atoms and tuples representing virtual terminal sequences as part of the
`Format` string.

Calling `format/3` will always emit a `reset/0` VTS at the end of the returned
string. To not emit this, set the `reset` option to `false`.

To force enabling or disabling of emitting VTSs set the `enabled` option to
`true` or `false`. By default the emitting of VTSs is enabled if `enabled/0` returns `true`
and disabled otherwise.

To disable emitting of color VTSs but still emit other VTSs, set the `color` option to `false`.
The default color option is `true` unless the `NO_COLOR` environment variable is set to a non-empty value,
in which case the default is `false`.

Example:

```erlang
1> io_ansi:format([blue, underline, "Hello world"]).
~"\e[34m\e[4mHello world\e(B\e[m"
2> io_ansi:format([blue, underline, "Hello ~p"],[world]).
~"\e[34m\e[4mHello world\e(B\e[m"
3> io_ansi:format([blue, underline, "Hello ~p"],[world],[{reset,false}]).
~"\e[34m\e[4mHello world"
4> io_ansi:format([blue, underline, "Hello ~p"],[world],[{enabled,false}]).
~"Hello world"
5> io_ansi:format([blue, underline, "Hello ~p"],[world],[{color,false}]).
~"\e[4mHello world\e(B\e[m"
```

For a detailed description of the available formatting options, see `io:fwrite/3`.

# `fwrite`
*since OTP 29.0* 

```erlang
-spec fwrite(Format :: format()) -> ok.
```

# `fwrite`
*since OTP 29.0* 

```erlang
-spec fwrite(Format :: format(), [term()]) -> ok.
```

# `fwrite`
*since OTP 29.0* 

```erlang
-spec fwrite(Format :: format(), [term()], options()) -> ok.
```

# `fwrite`
*since OTP 29.0* 

```erlang
-spec fwrite(IODevice :: io:device(), Format :: format(), [term()], options()) -> ok.
```

Writes the items in `Data` on the [`IoDevice`](`t:io:device/0`) in accordance with `Format`.

This function works just as `io:fwrite/2`, except that it also allows atoms and
tuples representing virtual terminal sequences (VTS) as part of the `Format` string.

See `format/3` for details on how the different `Options` can be used.

Example:

```erlang
1> io_ansi:fwrite([blue, "%% Hello world\n"]).
%% Hello world
ok
2> io_ansi:fwrite([underline, "%% Hello ~p\n"], [world]).
%% Hello world
ok
3> io_ansi:fwrite([invalid_code, "%% Hello ~p\n"], [world]).
** exception error: {error,{put_ansi,unicode,invalid_code}}
```

The decision what each VTS should be converted to is done by the destination I/O
device. This means that if the I/O device is on a remote node, the terminfo
database loaded into that remote node will be used.

All VTSs are stripped if the target I/O device does not support handling VTSs,
either because it is not implemented by the device (for example if the device
is a `t:file:io_server/0`) or if the device does not support a certain VTS.
If you want to force usage of VTSs you can pass `{enabled, true}` and that will
use the local defintions to translate.

# `green`
*since OTP 29.0* 

```erlang
-spec green() -> unicode:chardata().
```

Change foreground (aka text) color to green.

Example:
```erlang
1> io_ansi:green().
<<"\e[32m">>
```

# `green_background`
*since OTP 29.0* 

```erlang
-spec green_background() -> unicode:chardata().
```

Change background color to green.

Example:
```erlang
1> io_ansi:green_background().
<<"\e[42m">>
```

# `green_underline`
*since OTP 29.0* 

```erlang
-spec green_underline() -> unicode:chardata().
```

Change underline color to green. Not widely supported.

Example:
```erlang
1> io_ansi:green_underline().
<<"\e[58;5;2m">>
```

# `hyperlink_reset`
*since OTP 29.0* 

```erlang
-spec hyperlink_reset() -> unicode:chardata().
```

Stop emitting a hyperlink.

Example:
```erlang
1> io_ansi:hyperlink_reset().
<<"\e]8;;\e\\">>
```

# `hyperlink_start`
*since OTP 29.0* 

```erlang
-spec hyperlink_start(uri_string:uri_string()) -> unicode:chardata().
```

# `hyperlink_start`
*since OTP 29.0* 

```erlang
-spec hyperlink_start(uri_string:uri_string(),
                      [{Key :: unicode:chardata(), Value :: unicode:chardata()}]) ->
                         unicode:chardata().
```

Start a hyperlink pointing to the given `URL` using `Params`.

The hyperlink can be any type of URL, but typically it would be a file or http
URL.

Example:
```erlang
1> io_ansi:hyperlink_start("https://erlang.org").
<<"\e]8;https://erlang.org;\e\\">>
2> io_ansi:format([{hyperlink_start, "file://tmp/debug.log"},"debug log",hyperlink_reset]).
~"\e]8;file://tmp/debug.log;\e\\debug log\e]8;;\e\\\e(B\e[m"
```

See [Hyperlinks (a.k.a. HTML-like anchors) in terminal emulators](https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda)
for more details on limitations and usage of terminal hyperlinks.

# `insert_character`
*since OTP 29.0* 

```erlang
-spec insert_character(Chars :: integer()) -> unicode:chardata().
```

Insert `Chars` at cursor.

Example:
```erlang
1> io_ansi:insert_character(3).
<<"\e[3@">>
```

# `insert_line`
*since OTP 29.0* 

```erlang
-spec insert_line() -> unicode:chardata().
```

Insert 1 line at cursor.

Example:
```erlang
1> io_ansi:insert_line().
<<"\e[L">>
```

# `insert_line`
*since OTP 29.0* 

```erlang
-spec insert_line(Lines :: integer()) -> unicode:chardata().
```

Insert `Lines` lines at cursor.

Example:
```erlang
1> io_ansi:insert_line(2).
<<"\e[2L">>
```

# `inverse`
*since OTP 29.0* 

```erlang
-spec inverse() -> unicode:chardata().
```

Turn on inverse text style.

Inverse is also called reverse video or standout mode.

Example:
```erlang
1> io_ansi:inverse().
<<"\e[7m">>
```

# `inverse_off`
*since OTP 29.0* 

```erlang
-spec inverse_off() -> unicode:chardata().
```

Turn off inverse text style.

Inverse is also called reverse video or standout mode.

Example:
```erlang
1> io_ansi:inverse_off().
<<"\e[27m">>
```

# `invisible`
*since OTP 29.0* 

```erlang
-spec invisible() -> unicode:chardata().
```

Turn on invisible text style. Not widely supported.

Example:
```erlang
1> io_ansi:invisible().
<<"\e[8m">>
```

# `invisible_off`
*since OTP 29.0* 

```erlang
-spec invisible_off() -> unicode:chardata().
```

Turn off invisible text style.

Example:
```erlang
1> io_ansi:invisible_off().
<<"\e[28m">>
```

# `italic`
*since OTP 29.0* 

```erlang
-spec italic() -> unicode:chardata().
```

Turn on italic text style. Not widely supported.

Example:
```erlang
1> io_ansi:italic().
<<"\e[3m">>
```

# `italic_off`
*since OTP 29.0* 

```erlang
-spec italic_off() -> unicode:chardata().
```

Turn off italic text style.

Example:
```erlang
1> io_ansi:italic_off().
<<"\e[23m">>
```

# `keypad_transmit_mode`
*since OTP 29.0* 

```erlang
-spec keypad_transmit_mode() -> unicode:chardata().
```

Enable keypad transmit mode.

Example:
```erlang
1> io_ansi:keypad_transmit_mode().
<<"\e[?1h\e=">>
```

# `keypad_transmit_mode_off`
*since OTP 29.0* 

```erlang
-spec keypad_transmit_mode_off() -> unicode:chardata().
```

Disable keypad transmit mode.

Example:
```erlang
1> io_ansi:keypad_transmit_mode_off().
<<"\e[?1l\e>">>
```

# `light_black`
*since OTP 29.0* 

```erlang
-spec light_black() -> unicode:chardata().
```

Change foreground (aka text) color to light black.

Example:
```erlang
1> io_ansi:light_black().
<<"\e[90m">>
```

# `light_black_background`
*since OTP 29.0* 

```erlang
-spec light_black_background() -> unicode:chardata().
```

Change background color to light black.

Example:
```erlang
1> io_ansi:light_black_background().
<<"\e[100m">>
```

# `light_black_underline`
*since OTP 29.0* 

```erlang
-spec light_black_underline() -> unicode:chardata().
```

Change underline color to light black. Not widely supported.

Example:
```erlang
1> io_ansi:light_black_underline().
<<"\e[58;5;8m">>
```

# `light_blue`
*since OTP 29.0* 

```erlang
-spec light_blue() -> unicode:chardata().
```

Change foreground (aka text) color to light blue.

Example:
```erlang
1> io_ansi:light_blue().
<<"\e[94m">>
```

# `light_blue_background`
*since OTP 29.0* 

```erlang
-spec light_blue_background() -> unicode:chardata().
```

Change background color to light blue.

Example:
```erlang
1> io_ansi:light_blue_background().
<<"\e[104m">>
```

# `light_blue_underline`
*since OTP 29.0* 

```erlang
-spec light_blue_underline() -> unicode:chardata().
```

Change underline color to light blue. Not widely supported.

Example:
```erlang
1> io_ansi:light_blue_underline().
<<"\e[58;5;12m">>
```

# `light_cyan`
*since OTP 29.0* 

```erlang
-spec light_cyan() -> unicode:chardata().
```

Change foreground (aka text) color to light cyan.

Example:
```erlang
1> io_ansi:light_cyan().
<<"\e[96m">>
```

# `light_cyan_background`
*since OTP 29.0* 

```erlang
-spec light_cyan_background() -> unicode:chardata().
```

Change background color to light cyan.

Example:
```erlang
1> io_ansi:light_cyan_background().
<<"\e[106m">>
```

# `light_cyan_underline`
*since OTP 29.0* 

```erlang
-spec light_cyan_underline() -> unicode:chardata().
```

Change underline color to light cyan. Not widely supported.

Example:
```erlang
1> io_ansi:light_cyan_underline().
<<"\e[58;5;14m">>
```

# `light_green`
*since OTP 29.0* 

```erlang
-spec light_green() -> unicode:chardata().
```

Change foreground (aka text) color to light green.

Example:
```erlang
1> io_ansi:light_green().
<<"\e[92m">>
```

# `light_green_background`
*since OTP 29.0* 

```erlang
-spec light_green_background() -> unicode:chardata().
```

Change background color to light green.

Example:
```erlang
1> io_ansi:light_green_background().
<<"\e[102m">>
```

# `light_green_underline`
*since OTP 29.0* 

```erlang
-spec light_green_underline() -> unicode:chardata().
```

Change underline color to light green. Not widely supported.

Example:
```erlang
1> io_ansi:light_green_underline().
<<"\e[58;5;10m">>
```

# `light_magenta`
*since OTP 29.0* 

```erlang
-spec light_magenta() -> unicode:chardata().
```

Change foreground (aka text) color to light magenta.

Example:
```erlang
1> io_ansi:light_magenta().
<<"\e[95m">>
```

# `light_magenta_background`
*since OTP 29.0* 

```erlang
-spec light_magenta_background() -> unicode:chardata().
```

Change background color to light magenta.

Example:
```erlang
1> io_ansi:light_magenta_background().
<<"\e[105m">>
```

# `light_magenta_underline`
*since OTP 29.0* 

```erlang
-spec light_magenta_underline() -> unicode:chardata().
```

Change underline color to light magenta. Not widely supported.

Example:
```erlang
1> io_ansi:light_magenta_underline().
<<"\e[58;5;13m">>
```

# `light_red`
*since OTP 29.0* 

```erlang
-spec light_red() -> unicode:chardata().
```

Change foreground (aka text) color to light red.

Example:
```erlang
1> io_ansi:light_red().
<<"\e[91m">>
```

# `light_red_background`
*since OTP 29.0* 

```erlang
-spec light_red_background() -> unicode:chardata().
```

Change background color to light red.

Example:
```erlang
1> io_ansi:light_red_background().
<<"\e[101m">>
```

# `light_red_underline`
*since OTP 29.0* 

```erlang
-spec light_red_underline() -> unicode:chardata().
```

Change underline color to light red. Not widely supported.

Example:
```erlang
1> io_ansi:light_red_underline().
<<"\e[58;5;9m">>
```

# `light_white`
*since OTP 29.0* 

```erlang
-spec light_white() -> unicode:chardata().
```

Change foreground (aka text) color to light white.

Example:
```erlang
1> io_ansi:light_white().
<<"\e[97m">>
```

# `light_white_background`
*since OTP 29.0* 

```erlang
-spec light_white_background() -> unicode:chardata().
```

Change background color to light white.

Example:
```erlang
1> io_ansi:light_white_background().
<<"\e[107m">>
```

# `light_white_underline`
*since OTP 29.0* 

```erlang
-spec light_white_underline() -> unicode:chardata().
```

Change underline color to light white. Not widely supported.

Example:
```erlang
1> io_ansi:light_white_underline().
<<"\e[58;5;15m">>
```

# `light_yellow`
*since OTP 29.0* 

```erlang
-spec light_yellow() -> unicode:chardata().
```

Change foreground (aka text) color to light yellow.

Example:
```erlang
1> io_ansi:light_yellow().
<<"\e[93m">>
```

# `light_yellow_background`
*since OTP 29.0* 

```erlang
-spec light_yellow_background() -> unicode:chardata().
```

Change background color to light yellow.

Example:
```erlang
1> io_ansi:light_yellow_background().
<<"\e[103m">>
```

# `light_yellow_underline`
*since OTP 29.0* 

```erlang
-spec light_yellow_underline() -> unicode:chardata().
```

Change underline color to light yellow. Not widely supported.

Example:
```erlang
1> io_ansi:light_yellow_underline().
<<"\e[58;5;11m">>
```

# `magenta`
*since OTP 29.0* 

```erlang
-spec magenta() -> unicode:chardata().
```

Change foreground (aka text) color to magenta.

Example:
```erlang
1> io_ansi:magenta().
<<"\e[35m">>
```

# `magenta_background`
*since OTP 29.0* 

```erlang
-spec magenta_background() -> unicode:chardata().
```

Change background color to magenta.

Example:
```erlang
1> io_ansi:magenta_background().
<<"\e[45m">>
```

# `magenta_underline`
*since OTP 29.0* 

```erlang
-spec magenta_underline() -> unicode:chardata().
```

Change underline color to magenta. Not widely supported.

Example:
```erlang
1> io_ansi:magenta_underline().
<<"\e[58;5;5m">>
```

# `modify_color`
*since OTP 29.0* 

```erlang
-spec modify_color(Index :: 0..255, R :: 0..255, G :: 0..255, B :: 0..255) -> unicode:chardata().
```

Modify the color referenced by `Index` to be RGB.

Calling this function for `Index` 0-15 will change the color of the named colors
in `t:foreground_color/0` and `t:background_color/0`.

Example:
```erlang
1> io_ansi:modify_color(1, 255, 100, 0).
<<"\e]4;1;rgb:41/19/00\e\\">>
```

# `overline`
*since OTP 29.0* 

```erlang
-spec overline() -> unicode:chardata().
```

Turn on overline text style.

Example:
```erlang
1> io_ansi:overline().
<<"\e[53m">>
```

# `overline_off`
*since OTP 29.0* 

```erlang
-spec overline_off() -> unicode:chardata().
```

Turn off overline text style.

Example:
```erlang
1> io_ansi:overline_off().
<<"\e[55m">>
```

# `red`
*since OTP 29.0* 

```erlang
-spec red() -> unicode:chardata().
```

Change foreground (aka text) color to red.

Example:
```erlang
1> io_ansi:red().
<<"\e[31m">>
```

# `red_background`
*since OTP 29.0* 

```erlang
-spec red_background() -> unicode:chardata().
```

Change background color to red.

Example:
```erlang
1> io_ansi:red_background().
<<"\e[41m">>
```

# `red_underline`
*since OTP 29.0* 

```erlang
-spec red_underline() -> unicode:chardata().
```

Change underline color to red. Not widely supported.

Example:
```erlang
1> io_ansi:red_underline().
<<"\e[58;5;1m">>
```

# `render`
*since OTP 29.0* 

```erlang
-spec render([vts() | unicode:chardata()]) -> unicode:chardata().
```

# `render`
*since OTP 29.0* 

```erlang
-spec render([vts() | unicode:chardata()], options()) -> unicode:chardata().
```

Renders terminal sequences in `Data`.

`Data` may contain either virtual terminal sequences, which are rendered,
or `unicode:chardata()`, which are left as is.

It accepts the same options as `format/3`.

Example:

```erlang
1> io_ansi:render([blue, underline, "Hello world"]).
[~"\e[34m",~"\e[4m","Hello world",~"\e(B\e[m"]
2> io_ansi:render([blue, underline, ~"Hello world"],[{reset,false}]).
[~"\e[34m",~"\e[4m",~"Hello world"]
3> io_ansi:render([blue, underline, ~"Hello world"],[{enabled,false}]).
[~"Hello world"]
4> io_ansi:render([blue, underline, "Hello ", $\n, ~"world"],[{color,false}]).
[~"\e[4m","Hello ", $\n, ~"world",~"\e(B\e[m"]
5> io_ansi:render([invalid_code, "Hello world"]).
** exception error: {invalid_code,invalid_code}
     in function  io_ansi:render_internal/5
```

# `reset`
*since OTP 29.0* 

```erlang
-spec reset() -> unicode:chardata().
```

Reset virtual terminal sequences to their original state.

This only resets the things supported by the loaded terminfo database,
which means that OSCs such as `hyperlink_start/2` are not reset but have
to be reset by emitting `hyperlink_reset/0`.

Example:
```erlang
1> io_ansi:reset().
<<"\e(B\e[m">>
```

# `reverse_index`
*since OTP 29.0* 

```erlang
-spec reverse_index() -> unicode:chardata().
```

Move the cursor up one line, but keeps the cursor on the same location on the
screen by scrolling the screen down.

Example:
```erlang
1> io_ansi:reverse_index().
<<"\eM">>
```

# `scan`
*since OTP 29.0* 

```erlang
-spec scan(unicode:chardata()) -> [unicode:unicode_binary() | vts() | {csi, unicode:unicode_binary()}].
```

Scan the string for virtial terminal sequences.

The recognized VTSs will be converted into the corresponding `t:vts/0`.

If you intend to parse arrow keys it is recommended that you first set the terminal in
application mode by using `keypad_transmit_mode/0`. This will make it easier for
`m:io_ansi` to correctly detect arrow keys.

Any unrecognized [control sequence introducers](https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands),
will be placed in a tuple tagged with `csi`.

Example:

```erlang
1> io_ansi:scan("\eOA").
[kcursor_up]
2> io_ansi:scan("\eOB").
[kcursor_down]
3> io_ansi:scan(io_ansi:format([bold, "text"])).
[bold, ~"text", reset]
4> io_ansi:scan(io_ansi:format([{cursor, 0, 0}])).
[{csi, ~"\e[1;1H"}, reset]
```

# `scroll_backward`
*since OTP 29.0* 

```erlang
-spec scroll_backward() -> unicode:chardata().
```

Scroll the screen backward 1 step.

Example:
```erlang
1> io_ansi:scroll_backward().
<<"\e[1T">>
```

# `scroll_backward`
*since OTP 29.0* 

```erlang
-spec scroll_backward(Steps :: integer()) -> unicode:chardata().
```

Scroll the screen backward `N` step.

Example:
```erlang
1> io_ansi:scroll_backward(42).
<<"\e[42T">>
```

# `scroll_change_region`
*since OTP 29.0* 

```erlang
-spec scroll_change_region(Line1 :: integer(), Line2 :: integer()) -> unicode:chardata().
```

Change the scolling region to be from `Line1` to `Line2`.

Example:
```erlang
1> io_ansi:scroll_change_region(10, 20).
<<"\e[11;21r">>
```

# `scroll_forward`
*since OTP 29.0* 

```erlang
-spec scroll_forward() -> unicode:chardata().
```

Scroll the screen forward 1 step.

Example:
```erlang
1> io_ansi:scroll_forward().
<<"\e[1S">>
```

# `scroll_forward`
*since OTP 29.0* 

```erlang
-spec scroll_forward(N :: integer()) -> unicode:chardata().
```

Scroll the screen forward `N` step.

Example:
```erlang
1> io_ansi:scroll_forward(42).
<<"\e[42S">>
```

# `strikethrough`
*since OTP 29.0* 

```erlang
-spec strikethrough() -> unicode:chardata().
```

Turn on strikethrough text style. Not widely supported.

Example:
```erlang
1> io_ansi:strikethrough().
<<"\e[9m">>
```

# `strikethrough_off`
*since OTP 29.0* 

```erlang
-spec strikethrough_off() -> unicode:chardata().
```

Turn off strikethrough text style.

Example:
```erlang
1> io_ansi:strikethrough_off().
<<"\e[29m">>
```

# `tab`
*since OTP 29.0* 

```erlang
-spec tab() -> unicode:chardata().
```

Move cursor one tab forward.

Example:
```erlang
1> io_ansi:tab().
<<"\t">>
```

# `tab_backward`
*since OTP 29.0* 

```erlang
-spec tab_backward() -> unicode:chardata().
```

Move cursor one tab backward.

Example:
```erlang
1> io_ansi:tab_backward().
<<"\e[Z">>
```

# `tab_clear`
*since OTP 29.0* 

```erlang
-spec tab_clear() -> unicode:chardata().
```

Clear any tab location at the current cursor location.

Example:
```erlang
1> io_ansi:tab_clear().
<<"\e[0g">>
```

# `tab_clear_all`
*since OTP 29.0* 

```erlang
-spec tab_clear_all() -> unicode:chardata().
```

Clear all tab locations.

Example:
```erlang
1> io_ansi:tab_clear_all().
<<"\e[3g">>
```

# `tab_set`
*since OTP 29.0* 

```erlang
-spec tab_set() -> unicode:chardata().
```

Set a new tab location at the current cursor location.

Example:
```erlang
1> io_ansi:tab_set().
<<"\eH">>
```

# `underline`
*since OTP 29.0* 

```erlang
-spec underline() -> unicode:chardata().
```

Turn on underline text style.

Example:
```erlang
1> io_ansi:underline().
<<"\e[4m">>
```

# `underline_color`
*since OTP 29.0* 

```erlang
-spec underline_color(Index :: 0..255 | color_atom()) -> unicode:chardata().
```

Set underline color to a named color or index color. Not widely supported.

Accepts color atoms (`red`, `blue`, `light_green`, etc.) or index colors (0-255).
Index 0-15 correspond to the named colors in `t:color_atom/0`.

Example:
```erlang
1> io_ansi:underline_color(red).
<<"\e[58;5;1m">>
2> io_ansi:underline_color(light_cyan).
<<"\e[58;5;14m">>
3> io_ansi:underline_color(196).
<<"\e[58;5;196m">>
```

# `underline_color`
*since OTP 29.0* 

```erlang
-spec underline_color(0..255, 0..255, 0..255) -> unicode:chardata().
```

Set underline color to RGB color. Not widely supported.

Example:
```erlang
1> io_ansi:underline_color(255, 0, 0).
<<"\e[58;2;255;0;0m">>
```

# `underline_off`
*since OTP 29.0* 

```erlang
-spec underline_off() -> unicode:chardata().
```

Turn off underline text style.

Example:
```erlang
1> io_ansi:underline_off().
<<"\e[24m">>
```

# `white`
*since OTP 29.0* 

```erlang
-spec white() -> unicode:chardata().
```

Change foreground (aka text) color to white.

Example:
```erlang
1> io_ansi:white().
<<"\e[37m">>
```

# `white_background`
*since OTP 29.0* 

```erlang
-spec white_background() -> unicode:chardata().
```

Change background color to white.

Example:
```erlang
1> io_ansi:white_background().
<<"\e[47m">>
```

# `white_underline`
*since OTP 29.0* 

```erlang
-spec white_underline() -> unicode:chardata().
```

Change underline color to white. Not widely supported.

Example:
```erlang
1> io_ansi:white_underline().
<<"\e[58;5;7m">>
```

# `yellow`
*since OTP 29.0* 

```erlang
-spec yellow() -> unicode:chardata().
```

Change foreground (aka text) color to yellow.

Example:
```erlang
1> io_ansi:yellow().
<<"\e[33m">>
```

# `yellow_background`
*since OTP 29.0* 

```erlang
-spec yellow_background() -> unicode:chardata().
```

Change background color to yellow.

Example:
```erlang
1> io_ansi:yellow_background().
<<"\e[43m">>
```

# `yellow_underline`
*since OTP 29.0* 

```erlang
-spec yellow_underline() -> unicode:chardata().
```

Change underline color to yellow. Not widely supported.

Example:
```erlang
1> io_ansi:yellow_underline().
<<"\e[58;5;3m">>
```

# `tigetflag`
*since OTP 29.0* 

```erlang
-spec tigetflag(TermInfoCapName :: string()) -> boolean().
```

Returns true if the terminfo capability is available, otherwise false.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetflag/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Example:

```erlang
1> io_ansi:tigetflag("xn").
true
2> io_ansi:tigetflag("foobar").
false
```

# `tigetnum`
*since OTP 29.0* 

```erlang
-spec tigetnum(TermInfoCapName :: string()) -> -1 | non_neg_integer().
```

Returns the number representing a terminfo capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tigetnum/1` will use the terminfo
definition associated with the `TERM` environment variable when the Erlang VM is
started. It is not possible to change after startup.

Returns `-1` if the capability is not available.

Example:

```erlang
1> io_ansi:tigetnum("co").
80
2> io_ansi:tigetnum("foobar").
-1
```

# `tinfo`
*since OTP 29.0* 

```erlang
-spec tinfo() -> #{bool := [#{code := string(), name := string(), full_name := string()}]}.
```

Returns information about all available terminfo capabilities. See
the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation for details on each.

`tinfo/0` will use the terminfo definition associated with the `TERM` environment
variable when the Erlang VM is started. It is not possible to change after startup.

When calling `tput/2`, `tigetnum/1` and `tigetflag/1` you should provide the `name`
of the capability you want.

Example:

```erlang
1> io_ansi:tinfo().
#{ bool => [#{code => "xr",name => "OTxr",full_name => "return_does_clr_eol"} | ...],
   str => [#{code => "bx",name => "box1",full_name => "box_chars_1"} | ...],
   num => [#{code => "kn",name => "OTkn", full_name => "number_of_function_keys"} | ...]
 }
```

# `tput`
*since OTP 29.0* 

```erlang
-spec tput(TermInfoCap :: string()) -> unicode:unicode_binary().
```

# `tput`
*since OTP 29.0* 

```erlang
-spec tput(TermInfoCapName :: string(), Args :: [integer()]) -> unicode:unicode_binary().
```

Returns the string representing the action taken by the given terminal capability.

The names of the terminal capabilities can be found in the [terminfo](https://man7.org/linux/man-pages/man5/terminfo.5.html)
documentation, or by calling `tinfo/0`. `tput/2` will use the terminfo definition
associated with the `TERM` environment variable when the Erlang VM is started.
It is not possible to change after startup.

If the given capability is not defined in the terminfo database an `enotsup`
error is generated, if the given capability is invalid a `badarg` error is 
generated.

This function does not work on Windows and will always generate a `badarg`
exception.

Example:

```erlang
%% Set the foreground color to 3
1> io_ansi:tput("setaf",[3]).
<<"\e[33m">>
%% Move the cursor up 2 spaces
2> io_ansi:tput("cuu",[2]).
<<"\e[2A">>
%% Move the cursor down 1 space
3> io_ansi:tput("cud1").
<<"\n">>
%% unsupported capability
4> io_ansi:tput("slm").
** exception error: {enotsup,"slm"}
     in function  io_ansi:tput/2
%% unknown capability
5> io_ansi:tput("foobar").
** exception error: {einval,"foobar",[]}
     in function  io_ansi:tput/2
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
