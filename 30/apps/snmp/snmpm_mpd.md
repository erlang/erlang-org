# `snmpm_mpd`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/manager/snmpm_mpd.erl#L23)

Message Processing and Dispatch module for the SNMP manager

The module `snmpm_mpd` implements the version independent Message Processing and
Dispatch functionality in SNMP for the manager. It is supposed to be used from a
Network Interface process
([Definition of Manager Net if](snmp_manager_netif.md)).

Legacy API function `process_msg/7` that has got separate `IpAddr` and
`PortNumber` arguments still works as before for backwards compatibility
reasons.

# `logger`

```erlang
-type logger() ::
          fun((Data ::
                   binary() |
                   snmp_pdus:pdu() |
                   snmp_pdus:trappdu() |
                   snmp_pdus:message() |
                   {V3Hdr :: snmp_pdus:v3_hdr(), ScopedPDUBytes :: binary()}) ->
                  snmp:void()).
```

A `fun` that handles audit trail logging.

# `mpd_state`

```erlang
-opaque mpd_state()
```

# `msg_data_acm`

```erlang
-opaque msg_data_acm()
```

Is an opaque data structure containing necessary security information for
(incoming) v3 messages.

# `msg_data_cmy`

```erlang
-opaque msg_data_cmy()
```

Is an opaque data structure containing necessary security information for v1 and
v2 messages.

# `msg_data_cmyt`

```erlang
-opaque msg_data_cmyt()
```

Is an opaque data structure containing necessary security and transport
information for v1 and v2 messages.

# `msg_data_v3`

```erlang
-opaque msg_data_v3()
```

Is an opaque data structure containing necessary security information for v3
messages.

# `generate_msg`

```erlang
-spec generate_msg(Vsn, NoteStore, Pdu, MsgData, Log) -> {ok, Packet} | {discarded, Reason}
                      when
                          Vsn :: snmp_pdus:version(),
                          NoteStore :: pid(),
                          Pdu :: snmp_pdus:pdu(),
                          MsgData :: msg_data_cmy() | msg_data_v3(),
                          Log :: logger(),
                          Packet :: binary(),
                          Reason :: term().
```

Generates a possibly encrypted packet to be sent to the network.

`NoteStore` is the [`pid()`](`t:pid/0`) of the `note-store` process.

`MsgData` is the message specific data used in the SNMP message. In SNMPv1 and
SNMPv2c, this message data is the community string. In SNMPv3, it is the context
information.

`Logger` is the function used for audit trail logging.

# `generate_response_msg`

```erlang
-spec generate_response_msg(Vsn, Pdu, MsgData, Log) ->
                               {ok, Packet} | {discarded, Reason} | {error, Reason}
                               when
                                   Vsn :: snmp_pdus:version(),
                                   Pdu :: snmp_pdus:pdu(),
                                   MsgData :: msg_data_cmy() | msg_data_cmyt() | msg_data_v3(),
                                   Log :: logger(),
                                   Packet :: binary(),
                                   Reason :: term().
```

Generates a possibly encrypted response packet to be sent to the network.

`MsgData` is the message specific data used in the SNMP message. This value is
received from the [`process_msg/6`](`snmpm_mpd:process_msg/6`) function.

# `init`

```erlang
-spec init(Vsns) -> MPDState when Vsns :: [snmp:version()], MPDState :: mpd_state().
```

This function can be called from the `net-if` process at start-up.
The options list defines which versions to use.

It also initializes some SNMP counters.

# `process_msg`
*since OTP 17.3* 

```erlang
-spec process_msg(Msg, Domain, Addr, State, NoteStore, Log) ->
                     {ok, Vsn, PduV2, PduMS, MsgDataV2} |
                     {ok, 'version-3', PduV3, PduMS, MsgDataV3} |
                     {discarded, Reason}
                     when
                         Msg :: binary(),
                         Domain :: snmpUDPDomain | snmp:tdomain(),
                         Addr :: {Ip, Port},
                         Ip :: inet:ip_address(),
                         Port :: inet:port_number(),
                         State :: mpd_state(),
                         NoteStore :: pid(),
                         Log :: logger(),
                         Vsn :: 'version-1' | 'version-2',
                         PduV2 :: snmp_pdus:pdu() | snmp_pdus:trappdu(),
                         PduV3 :: snmp_pdus:pdu(),
                         PduMS :: pos_integer(),
                         MsgDataV2 :: msg_data_cmyt(),
                         MsgDataV3 :: ok | {error, ReqId, ACM} | undefined | msg_data_acm(),
                         ReqId :: snmpm:request_id(),
                         ACM :: term(),
                         Reason :: term().
```

Processes an incoming message. Performs authentication and decryption as
necessary. The return values should be passed the manager server.

`NoteStore` is the [`pid()`](`t:pid/0`) of the `note-store` process.

`Logger` is the function used for audit trail logging.

In the case when the pdu type is `report`, `MsgData` is either `ok` or
`{error, ReqId, Reason}`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
