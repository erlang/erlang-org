# `snmpa_notification_filter`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_notification_filter.erl#L22)

Behaviour module for the SNMP agent notification filters.

This module defines the behaviour of the agent notification filters. A
`snmpa_notification_filter` compliant module must export the following
functions:

- handle_notification/2

The semantics of them and their exact signatures are explained below.

The purpose of notification filters is to allow for modification and/or
suppression of a notification.

A misbehaving filter will be removed.

# `notification`
*not exported* 

```erlang
-type notification() :: term().
```

# `trap`
*not exported* 

```erlang
-type trap() :: term().
```

# `handle_notification`

```erlang
-callback handle_notification(Notif, Data) -> Reply
                                 when
                                     Notif :: notification() | trap(),
                                     Data :: term(),
                                     Reply :: send | {send, NewNotif} | dont_send,
                                     NewNotif :: notification() | trap().
```

Handle a notification to be sent. The filter can either accept the notification
as is, return `send`, modify the notification, return `{send, NewNotif}` or
suppress the notification, return `dont_send`.

`Data` is supplied at filter registration time, see
`snmpa:register_notification_filter/5`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
