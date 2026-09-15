# `edlin`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/edlin.erl#L22)

Line and input interpretter for the erlang shell.

This module reads input, handles any escape sequences that have been configured
via edlin_key and outputs action requests. The action requests are handled
either by modules `group` or the `user_drv`.

## Key configuration

You can setup a custom key configuration that overrides the default key
configuration. This is done by setting the stdlib application parameter
[`shell_keymap`](stdlib_app.md#shell_keymap) before Erlang is started. If you
want to have the same keymap in all Erlang shells you can do so by putting a
[config](`e:kernel:config.md`) file in your user's home directory and then set
[ERL_FLAGS](`e:erts:erl_cmd.md#ERL_FLAGS`) to load it at startup. For example:

```text
$ cat $HOME/.erlang_keymap.config
[{stdlib,
  [{shell_keymap,
    #{ normal => #{ "\^[A" => clear } }
  }]
}].
$ ERL_FLAGS="-config $HOME/.erlang_keymap" erl
```

The current keymap configuration can be fetched through
[edlin:keymap()](`keymap/0`). If a custom keymap or keymap file is specified,
then it will be merged with the default keymap.

The keymap is a map of maps where the keys in the parent map corresponds to
different editing modes in the shell. The valid modes currently supported are
`normal` and `search`.

The keys in the child maps are the escape sequences that are sent from the
terminal when a key is pressed and each value is a valid action as seen below.

The default atom is used to specify that an action should happen when a key is
pressed that does not have any mapping. Typically used to exit a mode.

See [tty - A Command-Line Interface](`e:erts:tty.md`) for more information about
the default keymap.

## Actions

The commands below are the built-in action requests for switching input modes on
the normal shell or navigating, or manipulating the line feed. The line feed
supports multiple lines.

- **`auto_blink`** - Automatically close the closest matching opening
  parenthesis.

- **`backward_char`** - Move backward one character.

- **`backward_delete_char`** - Delete the character behind the cursor.

- **`backward_delete_word`** - Delete the word behind the cursor.

- **`backward_kill_line`** - Delete all characters from the cursor to the
  beginning of the line and save them in the kill buffer.

- **`backward_kill_word`** - Delete the word behind the cursor and save it in
  the kill buffer.

- **`backward_line`** - Move backward one line.

- **`backward_word`** - Move backward one word.

- **`beginning_of_expression`** - Move to the beginning of the expression.

- **`beginning_of_line`** - Move to the beginning of the line.

- **`clear`** - Clear the screen.

- **`clear_line`** - Clear the current expression.

- **`end_of_expression`** - Move to the end of the expression.

- **`end_of_line`** - Move to the end of the line.

- **`forward_char`** - Move forward one character.

- **`forward_delete_char`** - Delete the character under the cursor.

- **`forward_line`** - Move forward one line.

- **`forward_word`** - Move forward one word.

- **`help`** - Display help for the module or function closest on the left of
  the cursor.

- **`help_full`** - Display the whole help text for the module or function closest on the left of
  the cursor.

- **`history_down`** - Move to the next item in the history.

- **`history_up`** - Move to the previous item in the history.

- **`kill_line`** - Delete all characters from the cursor to the end of the line
  and save them in the kill buffer.

- **`kill_word`** - Delete the word under the cursor and save it in the kill
  buffer.

- **`move_expand_down`** - Move down one line in the expand area e.g. help or
  tab completion pager.

- **`move_expand_up`** - Move up one line in the expand area e.g. help or tab
  completion pager.

- **`new_line_finish`** - Add a newline at the end of the line and try to
  evaluate the current expression.

- **`newline`** - Add a newline at the cursor position.

- **`open_editor`** - Open the current line in an editor e.g. EDITOR="code -w"
  opens a buffer in vs code. Note that you need to pass a flag to the editor so
  that it signals the shell when you close the buffer.

- **`redraw_line`** - Redraw the current line.

- **`scroll_expand_down`** - Scroll down five lines in the expand area e.g. help
  or tab completion pager.

- **`scroll_expand_up`** - Scroll up five lines in the expand area e.g. help or
  tab completion pager.

- **`search_cancel`** - Cancel the current search.

- **`search_found`** - Accept the current search result and submit it.

- **`search_quit`** - Accept the current search result, but edit it before
  submitting.

- **`search`** - Enter search mode, search the history.

- **`skip_down`** - Skip to the next line in the history that matches the
  current search expression.

- **`skip_up`** - Skip to the previous line in the history that matches the
  current search expression.

- **`tab_expand_full`** - Output all possible tab completions.

- **`tab_expand_quit`** - Go back to normal mode.

- **`tab_expand`** - Autocomplete the current word, or show 5 lines of possible
  completions.

- **`transpose_char`** - Swap the character behind the cursor with the one in
  front of it.

- **`transpose_word`** - Swap the word behind the cursor with the one in front
  of it.

- **`yank`** - Insert the contents of the kill buffer at the cursor position.

# `keymap`
*not exported* *since OTP 26.1* 

```erlang
-type keymap() :: #{atom() => #{string() | default => atom()}}.
```

A map of maps for each shell mode containing key, action pairs.

# `keymap`
*since OTP 26.1* 

```erlang
-spec keymap() -> keymap().
```

Get the current keymap used in the shell. Each key in the parent map represents
a _shell mode_ e.g. `normal` or `search`. Each map associated with the _shell
modes_ contains _key sequences_ represented as strings, paired with an _action_,
which is one of the valid actions mentioned above.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
