# `snmpa_mpd`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/snmp/src/agent/snmpa_mpd.erl#L22)

Message Processing and Dispatch module for the SNMP agent

The module `snmpa_mpd` implements the version independent Message Processing and
Dispatch functionality in SNMP for the agent. It is supposed to be used from a
Network Interface process ([Definition of Agent Net if](snmp_agent_netif.md)).

## DATA TYPES

For more information, see the
[data types in `snmpa_conf`](`m:snmpa_conf#types`).

# `acm_data`

```erlang
-type acm_data() :: acm_data_cmy() | acm_data_v3().
```

This is the message specific data used in the SNMP message. This value is
received in a [`send_pdu`](snmp_agent_netif.md#im_send_pdu) or
[`send_pdu_req`](snmp_agent_netif.md#im_send_pdu_req) message from the agent (by
the net-if process).

# `acm_data_cmy`

```erlang
-opaque acm_data_cmy()
```

This is the message specific data used in the SNMPv1 and SNMPv2c message.

# `acm_data_v3`

```erlang
-opaque acm_data_v3()
```

This is the message specific data used in the SNMPv3 message.

# `logger`

```erlang
-type logger() ::
          fun((Type :: snmp_pdus:pdu_type(),
               Data :: binary() | {V3Hdr :: snmp_pdus:v3_hdr(), ScopedPDUBytes :: binary()}) ->
                  snmp:void()).
```

A `fun` that handles audit trail logging.

# `mpd_state`

```erlang
-opaque mpd_state()
```

# `msg_data`

```erlang
-type msg_data() :: msg_data_cmy() | msg_data_ctx().
```

This is the message specific data used in the SNMP message. This value is
received in a [`send_pdu`](snmp_agent_netif.md#im_send_pdu) or
[`send_pdu_req`](snmp_agent_netif.md#im_send_pdu_req) message from the agent (by
the net-if process).

# `msg_data_cmy`

```erlang
-opaque msg_data_cmy()
```

This is the message specific data used in the SNMP message. In SNMPv1 and
SNMPv2c, this message data is the community string.

# `msg_data_ctx`

```erlang
-opaque msg_data_ctx()
```

This is the message specific data used in the SNMP message. In SNMPv3, it is the
context information.

# `discarded_pdu`

```erlang
-spec discarded_pdu(Variable) -> snmp:void() when Variable :: snmpa:name() | false.
```

Increments the variable associated with a discarded pdu. This function can be
used when the net_if process receives a `discarded_pdu` message from the agent.

# `generate_msg`
*since OTP R14B* 

```erlang
-spec generate_msg(Vsn, NoteStore, Pdu, MsgData, To) -> {ok, PacketsAndAddresses} | {discarded, Reason}
                      when
                          Vsn :: snmp_pdus:version(),
                          NoteStore :: pid(),
                          Pdu :: snmp_pdus:pdu(),
                          MsgData :: msg_data(),
                          To :: [{Domain, Address}],
                          PacketsAndAddresses :: [{Domain, Address, Packet}],
                          Domain :: snmpa_conf:transportDomain(),
                          Address :: snmpa_conf:transportAddress(),
                          Packet :: binary(),
                          Reason :: term().
```

# `generate_msg`
*since OTP R14B* 

```erlang
-spec generate_msg(Vsn, NoteStore, Pdu, MsgData, LocalEngineID, To) ->
                      {ok, PacketsAndAddresses} | {discarded, Reason}
                      when
                          Vsn :: snmp_pdus:version(),
                          NoteStore :: pid(),
                          Pdu :: snmp_pdus:pdu(),
                          MsgData :: msg_data(),
                          LocalEngineID :: snmp_framework_mib:engine_id(),
                          To :: [DestAddr],
                          DestAddr :: {Domain, Address} | {{Domain, Address}, SecData},
                          SecData :: term(),
                          PacketsAndAddresses :: [{Domain, Address, Packet}],
                          Domain :: snmpa_conf:transportDomain(),
                          Address :: snmpa_conf:transportAddress(),
                          Packet :: binary(),
                          Reason :: term().
```

Generates a possibly encrypted request packet to be sent to the network.

`MsgData` is the message specific data used in the SNMP message. This value is
received in a [`send_pdu`](snmp_agent_netif.md#im_send_pdu) or
[`send_pdu_req`](snmp_agent_netif.md#im_send_pdu_req) message from the agent. In
SNMPv1 and SNMPv2c, this message data is the [community](`t:msg_data_cmy/0`)
string. In SNMPv3, it is the [context](`t:msg_data_ctx/0`) information.

`To` is a list of destination addresses and their corresponding security
parameters. This value is received in the same message from the agent and then
transformed through [`process_taddrs`](`process_taddrs/1`) before
passed to this function.

> #### Note {: .info }
>
> Note that the use of the LocalEngineID argument is only intended for special
> cases, if the agent is to "emulate" multiple EngineIDs\! By default, the agent
> uses the value of `SnmpEngineID` (see SNMP-FRAMEWORK-MIB).

# `generate_response_msg`
*since OTP R14B* 

```erlang
-spec generate_response_msg(Vsn, RePdu, Type, ACMData, Log) -> {ok, Packet} | {discarded, Reason}
                               when
                                   Vsn :: snmp_pdus:version(),
                                   RePdu :: snmp_pdus:pdu(),
                                   Type :: snmp_pdus:pdu_type(),
                                   ACMData :: acm_data(),
                                   Log :: logger(),
                                   Packet :: binary(),
                                   Reason :: term().
```

# `generate_response_msg`
*since OTP R14B* 

```erlang
-spec generate_response_msg(Vsn, RePdu, Type, ACMData, LocalEngineID, Log) ->
                               {ok, Packet} | {discarded, Reason}
                               when
                                   Vsn :: snmp_pdus:version(),
                                   RePdu :: snmp_pdus:pdu(),
                                   Type :: snmp_pdus:pdu_type(),
                                   ACMData :: acm_data(),
                                   LocalEngineID :: snmp_framework_mib:engine_id(),
                                   Log :: logger(),
                                   Packet :: binary(),
                                   Reason :: term().
```

Generates a possibly encrypted response packet to be sent to the network. `Type`
is the `#pdu.type` of the original request.

> #### Note {: .info }
>
> Note that the use of the LocalEngineID argument is only intended for special
> cases, if the agent is to "emulate" multiple EngineIDs\! By default, the agent
> uses the value of `SnmpEngineID` (see SNMP-FRAMEWORK-MIB).

# `init`

```erlang
-spec init(Vsns) -> MPDState when Vsns :: [snmp:version()], MPDState :: mpd_state().
```

This function can be called from the net_if process at start-up. The options
list defines which versions to use.

It also initializes some SNMP counters.

# `process_packet`
*since OTP 17.3* 

```erlang
-spec process_packet(Packet, From, State, NoteStore, Log) ->
                        {ok, Vsn, Pdu, PduMS, ACMData} | {discarded, Reason} | {discovery, DiscoPacket}
                        when
                            Packet :: binary(),
                            From :: {TDomain, TAddress},
                            TDomain :: snmpa_conf:transportDomain(),
                            TAddress :: {IpAddr, IpPort},
                            IpAddr :: inet:ip_address(),
                            IpPort :: inet:port_number(),
                            State :: mpd_state(),
                            NoteStore :: pid(),
                            Log :: logger(),
                            Vsn :: snmp_pdus:version(),
                            Pdu :: snmp_pdus:pdu(),
                            PduMS :: pos_integer(),
                            ACMData :: acm_data(),
                            Reason :: term(),
                            DiscoPacket :: binary().
```

# `process_packet`
*since OTP R14B* 

```erlang
-spec process_packet(Packet, TDomain, TAddress, State, NoteStore, Log) ->
                        {ok, Vsn, Pdu, PduMS, ACMData} | {discarded, Reason} | {discovery, DiscoPacket}
                        when
                            Packet :: binary(),
                            TDomain :: snmpa_conf:transportDomain(),
                            TAddress :: {IpAddr, IpPort},
                            IpAddr :: inet:ip_address(),
                            IpPort :: inet:port_number(),
                            State :: mpd_state(),
                            NoteStore :: pid(),
                            Log :: logger(),
                            Vsn :: snmp_pdus:version(),
                            Pdu :: snmp_pdus:pdu(),
                            PduMS :: pos_integer(),
                            ACMData :: acm_data(),
                            Reason :: term(),
                            DiscoPacket :: binary();
                    (Packet, From, LocalEngineID, State, NoteStore, Log) ->
                        {ok, Vsn, Pdu, PduMS, ACMData} | {discarded, Reason} | {discovery, DiscoPacket}
                        when
                            Packet :: binary(),
                            From :: {TDomain, TAddress},
                            TDomain :: snmpa_conf:transportDomain(),
                            TAddress :: {IpAddr, IpPort},
                            IpAddr :: inet:ip_address(),
                            IpPort :: inet:port_number(),
                            LocalEngineID :: snmp_framework_mib:engine_id(),
                            State :: mpd_state(),
                            NoteStore :: pid(),
                            Log :: logger(),
                            Vsn :: snmp_pdus:version(),
                            Pdu :: snmp_pdus:pdu(),
                            PduMS :: pos_integer(),
                            ACMData :: acm_data(),
                            Reason :: term(),
                            DiscoPacket :: binary().
```

Processes an incoming packet. Performs authentication and decryption as
necessary. The return values should be passed to the agent.

> #### Note {: .info }
>
> Note that the use of the LocalEngineID argument is only intended for special
> cases, if the agent is to "emulate" multiple EngineIDs\! By default, the agent
> uses the value of `SnmpEngineID` (see SNMP-FRAMEWORK-MIB).

# `process_taddrs`
*since OTP 17.3* 

```erlang
-spec process_taddrs(InDests) -> OutDests
                        when
                            InDests :: [InDest],
                            InDest :: {{InDomain, InAddress}, SecData} | {InDomain, InAddress},
                            InDomain :: term(),
                            InAddress :: term(),
                            SecData :: term(),
                            OutDests :: [OutDest],
                            OutDest :: {{OutDomain, OutAddress}, SecData} | {OutDomain, OutAddress},
                            OutDomain :: snmpa_conf:transportDomain(),
                            OutAddress :: snmpa_conf:transportAddress().
```

Transforms addresses from internal MIB format to one more useful to
[Agent Net if](snmp_agent_netif.md).

See also [`generate_msg`.](`generate_msg/6`)

---

*Consult [api-reference.md](api-reference.md) for complete listing*
