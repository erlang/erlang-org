# `snmpm_network_interface_filter`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/manager/snmpm_network_interface_filter.erl#L22)

Behaviour module for the SNMP manager network-interface filter.

This module defines the behaviour of the manager network interface filter. A
`snmpm_network_interface_filter` compliant module must export the following
functions:

- [`accept_recv/2`](`c:accept_recv/2`)
- [`accept_send/2`](`c:accept_send/2`)
- [`accept_recv_pdu/3`](`c:accept_recv_pdu/3`)
- [`accept_send_pdu/3`](`c:accept_send_pdu/3`)

The semantics of them and their exact signatures are explained below.

The purpose of the network interface filter is to allow for filtering of
messages (accept or reject) receive and send. This is done on two levels:

- The first level is at the UDP entry / exit point, i.e. immediately after the
  receipt of the message, before any message processing is done (accept_recv)
  and immediately before sending the message, after all message processing is
  done (accept_send).
- The second level is at the MPD entry / exit point, i.e. immediately after the
  basic message processing (accept_recv_pdu) / immediately before the basic
  message processing (accept_send_pdu).

Note that the network interface filter is something which is used by the network
interface implementation provided by the application (`snmpm_net_if` and
`snmpm_net_if_mt`). The default filter accepts all messages.

A network interface filter can e.g. be used during testing or for load
regulation.

Legacy network interface filter modules used arguments on the form
`(IpAddr, PortNumber,...)` instead of `(Domain, Addr, ...)`, and if the SNMP
manager is run without changing the configuration to use transport domains the
network interface filter will still get the old arguments and work as before.

# `pdu_type`
*not exported* 

```erlang
-type pdu_type() :: snmpm:pdu_type().
```

# `transportAddressWithPort`
*not exported* 

```erlang
-type transportAddressWithPort() :: snmpa_conf:transportAddressWithPort().
```

# `transportDomain`
*not exported* 

```erlang
-type transportDomain() :: snmpa_conf:transportDomain().
```

# `accept_recv`

```erlang
-callback accept_recv(Domain, Addr) -> boolean()
                         when Domain :: transportDomain(), Addr :: transportAddressWithPort().
```

Called at the reception of a message (before _any_ processing has been done).

For the message to be rejected, the function _must_ return _false_.

# `accept_recv_pdu`

```erlang
-callback accept_recv_pdu(Domain, Addr, PduType) -> boolean()
                             when
                                 Domain :: transportDomain(),
                                 Addr :: transportAddressWithPort(),
                                 PduType :: pdu_type().
```

Called after the basic message processing (MPD) has been done, but before the
pdu is handed over to the server for primary processing.

For the pdu to be rejected, the function _must_ return _false_.

# `accept_send`

```erlang
-callback accept_send(Domain, Addr) -> boolean()
                         when Domain :: transportDomain(), Addr :: transportAddressWithPort().
```

Called before the sending of a message (after _all_ processing has been done).

For the message to be rejected, the function _must_ return _false_.

# `accept_send_pdu`

```erlang
-callback accept_send_pdu(Domain, Addr, PduType) -> boolean()
                             when
                                 Domain :: transportDomain(),
                                 Addr :: transportAddressWithPort(),
                                 PduType :: pdu_type().
```

Called before the basic message processing (MPD) is done, when a pdu has been
received from the master-agent.

For the message to be rejected, the function _must_ return _false_.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
