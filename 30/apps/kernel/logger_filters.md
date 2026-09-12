# `logger_filters`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/logger_filters.erl#L22)

Filters to use with Logger.

All functions exported from this module can be used as primary or handler
filters. See `logger:add_primary_filter/2` and `logger:add_handler_filter/3` for
more information about how filters are added.

Filters are removed with `logger:remove_primary_filter/1` and
`logger:remove_handler_filter/2`.

### See Also

`m:logger`

# `domain`
*since OTP 21.0* 

```erlang
-spec domain(LogEvent, Extra) -> logger:filter_return()
                when
                    LogEvent :: logger:log_event(),
                    Extra :: {Action, Compare, MatchDomain},
                    Action :: log | stop,
                    Compare :: super | sub | equal | not_equal | undefined,
                    MatchDomain :: [atom()].
```

This filter provides a way of filtering log events based on a `domain` field in
`Metadata`. This field is optional, and the purpose of using it is to group log
events from, for example, a specific functional area. This allows filtering or
other specialized treatment in a Logger handler.

A domain field must be a list of atoms, creating smaller and more specialized
domains as the list grows longer. The greatest domain is `[]`, which comprises
all possible domains.

For example, consider the following domains:

```erlang
D1 = [otp]
D2 = [otp, sasl]
```

`D1` is the greatest of the two, and is said to be a super-domain of `D2`. `D2`
is a sub-domain `D1`. Both `D1` and `D2` are sub-domains of `[]`.

The above domains are used for logs originating from Erlang/OTP. D1 specifies
that the log event comes from Erlang/OTP in general, and D2 indicates that the
log event is a so called [SASL report](logger_chapter.md#sasl_reports).

The `Extra` parameter to the [`domain/2`](`domain/2`) function is specified when
adding the filter via `logger:add_primary_filter/2` or
`logger:add_handler_filter/3`.

The filter compares the value of the `domain` field in the log event's metadata
(`Domain`) against `MatchDomain`. The filter matches if the value of `Compare`
is:

- **`sub`** - and `Domain` is equal to or a sub-domain of `MatchDomain`, that
  is, if `MatchDomain` is a prefix of `Domain`.

- **`super`** - and `Domain` is equal to or a super-domain of `MatchDomain`,
  that is, if `Domain` is a prefix of `MatchDomain`.

- **`equal`** - and `Domain` is equal to `MatchDomain`.

- **`not_equal`** - and `Domain` differs from `MatchDomain`, or if there is no
  domain field in metadata.

- **`undefined`** - and there is no domain field in metadata. In this case
  `MatchDomain` must be set to `[]`.

If the filter matches and `Action` is `log`, the log event is allowed. If the
filter matches and `Action` is `stop`, the log event is stopped.

If the filter does not match, it returns `ignore`, meaning that other filters,
or the value of the configuration parameter `filter_default`, decide if the
event is allowed or not.

Log events that do not contain any domain field, match only when `Compare` is
equal to `undefined` or `not_equal`.

Example: stop all events with domain `[otp, sasl | _]`

```erlang
1> logger:set_handler_config(h1, filter_default, log). % this is the default
ok
2> Filter = {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}.
...
3> logger:add_handler_filter(h1, no_sasl, Filter).
ok
```

# `level`
*since OTP 21.0* 

```erlang
-spec level(LogEvent, Extra) -> logger:filter_return()
               when
                   LogEvent :: logger:log_event(),
                   Extra :: {Action, Operator, MatchLevel},
                   Action :: log | stop,
                   Operator :: neq | eq | lt | gt | lteq | gteq,
                   MatchLevel :: logger:level().
```

This filter provides a way of filtering log events based on the log level. It
matches log events by comparing the log level with a specified `MatchLevel`

The `Extra` parameter is specified when adding the filter via
`logger:add_primary_filter/2` or `logger:add_handler_filter/3`.

The filter compares the value of the event's log level (`Level`) to `MatchLevel`
by calling
[`logger:compare_levels(Level, MatchLevel)`](`logger:compare_levels/2`). The
filter matches if the value of `Operator` is:

- **`neq`** - and the compare function returns `lt` or `gt`.

- **`eq`** - and the compare function returns `eq`.

- **`lt`** - and the compare function returns `lt`.

- **`gt`** - and the compare function returns `gt`.

- **`lteq`** - and the compare function returns `lt` or `eq`.

- **`gteq`** - and the compare function returns `gt` or `eq`.

If the filter matches and `Action` is `log`, the log event is allowed. If the
filter matches and `Action` is `stop`, the log event is stopped.

If the filter does not match, it returns `ignore`, meaning that other filters,
or the value of the configuration parameter `filter_default`, will decide if the
event is allowed or not.

Example: only allow debug level log events

```erlang
logger:set_handler_config(h1, filter_default, stop).
Filter = {fun logger_filters:level/2, {log, eq, debug}}.
logger:add_handler_filter(h1, debug_only, Filter).
ok
```

# `progress`
*since OTP 21.0* 

```erlang
-spec progress(LogEvent, Extra) -> logger:filter_return()
                  when LogEvent :: logger:log_event(), Extra :: log | stop.
```

This filter matches all progress reports from `m:supervisor` and
[`application_controller`](`m:application`).

If `Extra` is `log`, the progress reports are allowed. If `Extra` is `stop`, the
progress reports are stopped.

The filter returns `ignore` for all other log events.

# `remote_gl`
*since OTP 21.0* 

```erlang
-spec remote_gl(LogEvent, Extra) -> logger:filter_return()
                   when LogEvent :: logger:log_event(), Extra :: log | stop.
```

This filter matches all events originating from a process that has its group
leader on a remote node.

If `Extra` is `log`, the matching events are allowed. If `Extra` is `stop`, the
matching events are stopped.

The filter returns `ignore` for all other log events.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
