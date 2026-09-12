# `argparse`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/argparse.erl#L24)

Command line arguments parser.

This module implements command line parser. Parser operates with _commands_ and
_arguments_ represented as a tree. Commands are branches, and arguments are
leaves of the tree. Parser always starts with the root command, named after
`progname` (the name of the program which started Erlang).

A [`command specification`](`t:command/0`) may contain handler definition for
each command, and a number argument specifications. When parser is successful,
`argparse` calls the matching handler, passing arguments extracted from the
command line. Arguments can be positional (occupying specific position in the
command line), and optional, residing anywhere but prefixed with a specified
character.

`argparse` automatically generates help and usage messages. It will also issue
errors when users give the program invalid arguments.

## Quick start

`argparse` is designed to work with [`escript`](`e:erts:escript_cmd.md`). The
example below is a fully functioning Erlang program accepting two command line
arguments and printing their product.

```erlang
#!/usr/bin/env escript

main(Args) ->
    argparse:run(Args, cli(), #{progname => mul}).

cli() ->
    #{
        arguments => [
            #{name => left, type => integer},
            #{name => right, type => integer}
        ],
        handler =>
            fun (#{left := Left, right := Right}) ->
                io:format("~b~n", [Left * Right])
            end
    }.
```

Running this script with no arguments results in an error, accompanied by the
usage information.

The `cli` function defines a single command with embedded handler accepting a
map. Keys of the map are argument names as defined by the `argument` field of
the command, `left` and `right` in the example. Values are taken from the
command line, and converted into integers, as requested by the type
specification. Both arguments in the example above are required (and therefore
defined as positional).

## Command hierarchy

A command may contain nested commands, forming a hierarchy. Arguments defined at
the upper level command are automatically added to all nested commands. Nested
commands example (assuming `progname` is `nested`):

```erlang
cli() ->
  #{
    %% top level argument applicable to all commands
    arguments => [#{name => top}],
      commands => #{
        "first" => #{
          %% argument applicable to "first" command and
          %%  all commands nested into "first"
          arguments => [#{name => mid}],
          commands => #{
            "second" => #{
              %% argument only applicable for "second" command
              arguments => [#{name => bottom}],
              handler => fun (A) -> io:format("~p~n", [A]) end
          }
        }
      }
    }
  }.
```

In the example above, a 3-level hierarchy is defined. First is the script itself
(`nested`), accepting the only argument `top`. Since it has no associated
handler, `run/3` will not accept user input omitting nested command selection.
For this example, user has to supply 5 arguments in the command line, two being
command names, and another 3 - required positional arguments:

```text
./nested.erl one first second two three
#{top => "one",mid => "two",bottom => "three"}
```

Commands have preference over positional argument values. In the example above,
commands and positional arguments are interleaving, and `argparse` matches
command name first.

## Arguments

`argparse` supports positional and optional arguments. Optional arguments, or
options for short, must be prefixed with a special character (`-` is the default
on all operating systems). Both options and positional arguments have 1 or more
associated values. See [`argument specification`](`t:argument/0`) to find more
details about supported combinations.

In the user input, short options may be concatenated with their values. Long
options support values separated by `=`. Consider this definition:

```erlang
cli() ->
  #{
    arguments => [
      #{name => long, long => "-long"},
      #{name => short, short => $s}
    ],
    handler => fun (Args) -> io:format("~p~n", [Args]) end
  }.
```

Running `./args --long=VALUE` prints `#{long => "VALUE"}`, running
`./args -sVALUE` prints `#{short => "VALUE"}`

`argparse` supports boolean flags concatenation: it is possible to shorten
`-r -f -v` to `-rfv`.

Shortened option names are not supported: it is not possible to use `--my-argum`
instead of `--my-argument-name` even when such option can be unambiguously
found.

# `arg_map`
*since OTP 26.0* 

```erlang
-type arg_map() :: #{argument_name() => term()}.
```

Arguments map is the map of argument names to the values extracted from the
command line. It is passed to the matching command handler. If an argument is
omitted, but has the default value is specified, it is added to the map. When no
default value specified, and argument is not present in the command line,
corresponding key is not present in the resulting map.

# `arg_type`
*since OTP 26.0* 

```erlang
-type arg_type() ::
          boolean | float |
          {float, Choice :: [float()]} |
          {float, [{min, float()} | {max, float()}]} |
          integer |
          {integer, Choices :: [integer()]} |
          {integer, [{min, integer()} | {max, integer()}]} |
          string |
          {string, Choices :: [string()]} |
          {string, Re :: string()} |
          {string, Re :: string(), ReOptions :: [term()]} |
          binary |
          {binary, Choices :: [binary()]} |
          {binary, Re :: binary()} |
          {binary, Re :: binary(), ReOptions :: [term()]} |
          atom |
          {atom, Choices :: [atom()]} |
          {atom, unsafe} |
          {custom, fun((string()) -> term())}.
```

Defines type conversion applied to the string retrieved from the user input. If
the conversion is successful, resulting value is validated using optional
`Choices`, or minimums and maximums (for integer and floating point values
only). Strings and binary values may be validated using regular expressions.
It's possible to define custom type conversion function, accepting a string and
returning Erlang term. If this function raises error with `badarg` reason,
argument is treated as invalid.

# `args`
*since OTP 26.0* 

```erlang
-type args() :: [string() | unicode:chardata()].
```

List of command line arguments to be parsed.

# `argument`
*since OTP 26.0* 

```erlang
-type argument() ::
          #{name := argument_name(),
            short => char(),
            long => string(),
            required => boolean(),
            default => term(),
            type => arg_type(),
            action => store | {store, term()} | append | {append, term()} | count | extend,
            nargs => pos_integer() | 'maybe' | {'maybe', term()} | list | nonempty_list | all,
            help => hidden | unicode:chardata() | argument_help()}.
```

Argument specification. Defines a single named argument that is returned in the
[`argument map`](`t:arg_map/0`). The only required field is `name`, all other
fields have defaults.

If either of the `short` or `long` fields is specified, the argument is treated
as optional. Optional arguments do not have specific order and may appear
anywhere in the command line. Positional arguments are ordered the same way as
they appear in the arguments list of the command specification.

By default, all positional arguments must be present in the command line. The
parser will return an error otherwise. Options, however, may be omitted, in
which case resulting argument map will either contain the default value, or not
have the key at all.

- **`name`** - Sets the argument name in the parsed argument map. If `help` is
  not defined, name is also used to generate the default usage message.

- **`short`** - Defines a short (single character) form of an optional argument.

  ```erlang
  %% Define a command accepting argument named myarg, with short form $a:
  1> Cmd = #{arguments => [#{name => myarg, short => $a}]}.
  %% Parse command line "-a str":
  2> {ok, ArgMap, _, _} = argparse:parse(["-a", "str"], Cmd), ArgMap.

  #{myarg => "str"}

  %% Option value can be concatenated with the switch: "-astr"
  3> {ok, ArgMap, _, _} = argparse:parse(["-astr"], Cmd), ArgMap.

  #{myarg => "str"}
  ```

  By default all options expect a single value following the option switch. The
  only exception is an option of a boolean type.

- **`long`** - Defines a long form of an optional argument.

  ```erlang
  1> Cmd = #{arguments => [#{name => myarg, long => "name"}]}.
  %% Parse command line "-name Erlang":
  2> {ok, ArgMap, _, _} = argparse:parse(["-name", "Erlang"], Cmd), ArgMap.

  #{myarg => "Erlang"}
  %% Or use "=" to separate the switch and the value:
  3> {ok, ArgMap, _, _} = argparse:parse(["-name=Erlang"], Cmd), ArgMap.

  #{myarg => "Erlang"}
  ```

  If neither `short` not `long` is defined, the argument is treated as
  positional.

- **`required`** - Forces the parser to expect the argument to be present in the
  command line. By default, all positional argument are required, and all
  options are not.

- **`default`** - Specifies the default value to put in the parsed argument map
  if the value is not supplied in the command line.

  ```erlang
  1> argparse:parse([], #{arguments => [#{name => myarg, short => $m}]}).

  {ok,#{}, ...
  2> argparse:parse([], #{arguments => [#{name => myarg, short => $m, default => "def"}]}).

  {ok,#{myarg => "def"}, ...
  ```

- **`type`** - Defines type conversion and validation routine. The default is
  `string`, assuming no conversion.

- **`nargs`** - Defines the number of following arguments to consume from the
  command line. By default, the parser consumes the next argument and converts
  it into an Erlang term according to the specified type.

  - **`t:pos_integer/0`** - Consume exactly this number of positional arguments,
    fail if there is not enough. Value in the argument map contains a list of
    exactly this length. Example, defining a positional argument expecting 3
    integer values:

    ```erlang
    1> Cmd = #{arguments => [#{name => ints, type => integer, nargs => 3}]},
    argparse:parse(["1", "2", "3"], Cmd).

    {ok, #{ints => [1, 2, 3]}, ...
    ```

    Another example defining an option accepted as `-env` and expecting two
    string arguments:

    ```erlang
    1> Cmd = #{arguments => [#{name => env, long => "env", nargs => 2}]},
    argparse:parse(["-env", "key", "value"], Cmd).

    {ok, #{env => ["key", "value"]}, ...
    ```

  - **`list`** - Consume all following arguments until hitting the next option
    (starting with an option prefix). May result in an empty list added to the
    arguments map.

    ```erlang
    1> Cmd = #{arguments => [
      #{name => nodes, long => "nodes", nargs => list},
      #{name => verbose, short => $v, type => boolean}
    ]},
    argparse:parse(["-nodes", "one", "two", "-v"], Cmd).

    {ok, #{nodes => ["one", "two"], verbose => true}, ...
    ```

  - **`nonempty_list`** - Same as `list`, but expects at least one argument.
    Returns an error if the following command line argument is an option switch
    (starting with the prefix).

  - **`'maybe'`** - Consumes the next argument from the command line, if it does
    not start with an option prefix. Otherwise, adds a default value to the
    arguments map.

    ```erlang
    1> Cmd = #{arguments => [
      #{name => level, short => $l, nargs => 'maybe', default => "error"},
      #{name => verbose, short => $v, type => boolean}
    ]},
    argparse:parse(["-l", "info", "-v"], Cmd).

    {ok,#{level => "info",verbose => true}, ...

    %% When "info" is omitted, argument maps receives the default "error"
    2> argparse:parse(["-l", "-v"], Cmd).

    {ok,#{level => "error",verbose => true}, ...
    ```

  - **`{'maybe', term()}`** - Consumes the next argument from the command line,
    if it does not start with an option prefix. Otherwise, adds a specified
    Erlang term to the arguments map.

  - **`all`** - Fold all remaining command line arguments into a list, ignoring
    any option prefixes or switches. Useful for proxying arguments into another
    command line utility.

    ```erlang
    1> Cmd = #{arguments => [
        #{name => verbose, short => $v, type => boolean},
        #{name => raw, long => "-", nargs => all}
    ]},
    argparse:parse(["-v", "--", "-kernel", "arg", "opt"], Cmd).

    {ok,#{raw => ["-kernel","arg","opt"],verbose => true}, ...
    ```

- **`action`** - Defines an action to take when the argument is found in the
  command line. The default action is `store`.

  - **`store`** - Store the value in the arguments map. Overwrites the value
    previously written.

    ```erlang
    1> Cmd = #{arguments => [#{name => str, short => $s}]},
    argparse:parse(["-s", "one", "-s", "two"], Cmd).

    {ok, #{str => "two"}, ...
    ```

  - **`{store, term()}`** - Stores the specified term instead of reading the
    value from the command line.

    ```erlang
    1> Cmd = #{arguments => [#{name => str, short => $s, action => {store, "two"}}]},
    argparse:parse(["-s"], Cmd).

    {ok, #{str => "two"}, ...
    ```

  - **`append`** - Appends the repeating occurrences of the argument instead of
    overwriting.

    ```erlang
    1> Cmd = #{arguments => [#{name => node, short => $n, action => append}]},
    argparse:parse(["-n", "one", "-n", "two", "-n", "three"], Cmd).

    {ok, #{node => ["one", "two", "three"]}, ...

    %% Always produces a list - even if there is one occurrence
    2> argparse:parse(["-n", "one"], Cmd).

    {ok, #{node => ["one"]}, ...
    ```

  - **`{append, term()}`** - Same as `append`, but instead of consuming the
    argument from the command line, appends a provided `t:term/0`.

  - **`count`** - Puts a counter as a value in the arguments map. Useful for
    implementing verbosity option:

    ```erlang
    1> Cmd = #{arguments => [#{name => verbose, short => $v, action => count}]},
    argparse:parse(["-v"], Cmd).

    {ok, #{verbose => 1}, ...

    2> argparse:parse(["-vvvv"], Cmd).

    {ok, #{verbose => 4}, ...
    ```

  - **`extend`** - Works as `append`, but flattens the resulting list. Valid
    only for `nargs` set to `list`, `nonempty_list`, `all` or `t:pos_integer/0`.

    ```erlang
    1> Cmd = #{arguments => [#{name => duet, short => $d, nargs => 2, action => extend}]},
    argparse:parse(["-d", "a", "b", "-d", "c", "d"], Cmd).

    {ok, #{duet => ["a", "b", "c", "d"]}, ...

    %% 'append' would result in {ok, #{duet => [["a", "b"],["c", "d"]]},
    ```

- **`help`** - Specifies help/usage text for the argument. `argparse` provides
  automatic generation based on the argument name, type and default value, but
  for better usability it is recommended to have a proper description. Setting
  this field to `hidden` suppresses usage output for this argument.

# `argument_help`
*since OTP 26.0* 

```erlang
-type argument_help() ::
          {unicode:chardata(), [unicode:chardata() | type | default] | fun(() -> unicode:chardata())}.
```

User-defined help template to print in the command usage. First element of a
tuple must be a string. It is printed as a part of the usage header. Second
element of the tuple can be either a list containing strings, `type` and
`default` atoms, or a user-defined function that must return a string. A plain
string should be wrapped as a list such as `["string is nested"]`.

# `argument_name`
*not exported* *since OTP 26.0* 

```erlang
-type argument_name() :: atom() | string() | binary().
```

Argument name is used to populate argument map.

# `cmd_path`
*since OTP 26.0* 

```erlang
-type cmd_path() :: [string()].
```

Path to the nested command. First element is always the `progname`, subsequent
elements are nested command names.

# `command`
*since OTP 26.0* 

```erlang
-type command() ::
          #{commands => #{string() => command()},
            arguments => [argument()],
            help => hidden | unicode:chardata() | command_help(),
            handler => handler()}.
```

Command specification. May contain nested commands, forming a hierarchy.

- **`commands`** - Maps of nested commands. Keys must be strings, matching
  command line input. Basic utilities do not need to specify any nested
  commands.

- **`arguments`** - List of arguments accepted by this command, and all nested
  commands in the hierarchy.

- **`help`** - Specifies help/usage text for this command. Pass `hidden` to
  remove this command from the usage output.

- **`handler`** - Specifies a callback function to call by `run/3` when the
  parser is successful.

# `command_help`
*not exported* *since OTP 26.0* 

```erlang
-type command_help() :: [unicode:chardata() | usage | commands | arguments | options].
```

User-defined help template. Use this option to mix custom and predefined usage
text. Help template may contain unicode strings, and following atoms:

- **usage** - Formatted command line usage text, e.g. `rm [-rf] <directory>`.

- **commands** - Expanded list of sub-commands.

- **arguments** - Detailed description of positional arguments.

- **options** - Detailed description of optional arguments.

# `handler`
*since OTP 26.0* 

```erlang
-type handler() ::
          optional |
          fun((arg_map()) -> term()) |
          {module(), Fn :: atom()} |
          {fun(() -> term()), term()} |
          {module(), atom(), term()}.
```

Command handler specification. Called by [`run/3` ](`run/3`)upon successful
parser return.

- **`fun((arg_map()) -> term())`** - Function accepting
  [`argument map`](`t:arg_map/0`). See the basic example in the
  [Quick Start](`m:argparse#module-quick-start`) section.

- **`{Module :: module(), Function :: atom()}`** - Function named `Function`,
  exported from `Module`, accepting [`argument map`](`t:arg_map/0`).

- **`{fun(() -> term()), Default :: term()}`** - Function accepting as many
  arguments as there are in the `arguments` list for this command. Arguments
  missing from the parsed map are replaced with the `Default`. Convenient way to
  expose existing functions.

  ```erlang
  1> Cmd = #{arguments => [
          #{name => x, type => float},
          #{name => y, type => float, short => $p}],
      handler => {fun math:pow/2, 1}},
  argparse:run(["2", "-p", "3"], Cmd, #{}).

  8.0

  %% default term 1 is passed to math:pow/2
  2> argparse:run(["2"], Cmd, #{}).

  2.0
  ```

- **`{Module :: module(), Function :: atom(), Default :: term()}`** - Function
  named `Function`, exported from `Module`, accepting as many arguments as
  defined for this command. Arguments missing from the parsed map are replaced
  with the `Default`. Effectively, just a different syntax to the same
  functionality as demonstrated in the code above.

# `parse_result`
*not exported* *since OTP 26.0* 

```erlang
-type parse_result() :: {ok, arg_map(), Path :: cmd_path(), command()} | {error, parser_error()}.
```

Returned from [`parse/2,3`](`parse/3`). Contains arguments extracted from the
command line, path to the nested command (if any), and a (potentially nested)
command specification that was considered when the parser finished successfully.
It is expected that the command contains a handler definition, that will be
called passing the argument map.

# `parser_error`
*not exported* *since OTP 26.0* 

```erlang
-type parser_error() ::
          {Path :: cmd_path(),
           Expected :: argument() | undefined,
           Actual :: string() | undefined,
           Details :: unicode:chardata()}.
```

Returned from [`parse/2,3`](`parse/3`) when the user input cannot be parsed
according to the command specification.

First element is the path to the command that was considered when the parser
detected an error. Second element, `Expected`, is the argument specification
that caused an error. It could be `undefined`, meaning that `Actual` argument
had no corresponding specification in the arguments list for the current
command.

When `Actual` is set to `undefined`, it means that a required argument is
missing from the command line. If both `Expected` and `Actual` have values, it
means validation error.

Use `format_error/1` to generate a human-readable error description, unless
there is a need to provide localised error messages.

# `parser_options`
*not exported* *since OTP 26.0* 

```erlang
-type parser_options() ::
          #{prefixes => [char()],
            default => term(),
            progname => string() | atom(),
            command => cmd_path(),
            columns => pos_integer()}.
```

Options changing parser behaviour.

- **`prefixes`** - Changes the option prefix (the default is `-`).

- **`default`** - Specifies the default value for all optional arguments. When
  this field is set, resulting argument map will contain all argument names.
  Useful for easy pattern matching on the argument map in the handler function.

- **`progname`** - Specifies the program (root command) name. Returned as the
  first element of the command path, and printed in help/usage text. It is
  recommended to have this value set, otherwise the default one is determined
  with `init:get_argument(progname)` and is often set to `erl` instead of the
  actual script name.

- **`command`** - Specifies the path to the nested command for `help/2`. Useful
  to limit output for complex utilities with multiple commands, and used by the
  default error handling logic.

- **`columns`** - Specifies the help/usage text width (characters) for `help/2`.
  Default value is 80.

# `format_error`
*since OTP 26.0* 

```erlang
-spec format_error(Reason :: parser_error()) -> unicode:chardata().
```

Generates human-readable text for [`parser error`](`t:parser_error/0`). Does not
include help/usage information, and does not provide localisation.

# `help`
*since OTP 26.0* 

```erlang
-spec help(command()) -> string().
```

# `help`
*since OTP 26.0* 

```erlang
-spec help(command(), parser_options()) -> unicode:chardata().
```

Generates help/usage information text for the command supplied, or any nested
command when `command` option is specified. Arguments are displayed in the same
order as specified in `Command`. Does not provide localisation. Expects
`progname` to be set, otherwise defaults to return value of
`init:get_argument(progname)`.

# `parse`
*since OTP 26.0* 

```erlang
-spec parse(args(), command()) -> parse_result().
```

# `parse`
*since OTP 26.0* 

```erlang
-spec parse(args(), command(), Options :: parser_options()) -> parse_result().
```

Parses command line arguments according to the command specification. Raises an
exception if the command specification is not valid. Use
[`erl_error:format_exception/3,4` ](`erl_error:format_exception/3`)to see a
friendlier message. Invalid command line input does not raise an exception, but
makes `parse/2,3` to return a tuple
[`{error, parser_error()}`](`t:parser_error/0`).

This function does not call command handler.

# `run`
*since OTP 26.0* 

```erlang
-spec run(args(), command(), parser_options()) -> term().
```

Parses command line arguments and calls the matching command handler. Prints
human-readable error, help/usage information for the discovered command, and
halts the emulator with code 1 if there is any error in the command
specification or user-provided command line input.

> #### Warning {: .warning }
>
> This function is designed to work as an entry point to a standalone
> [`escript`](`e:erts:escript_cmd.md`). Therefore, it halts the emulator for any
> error detected. Do not use this function through remote procedure call, or it
> may result in an unexpected shutdown of a remote node.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
