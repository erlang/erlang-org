# `snmp_pdus`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/misc/snmp_pdus.erl#L23)

Encode and Decode Functions for SNMP PDUs

RFC1157, RFC1905 and/or RFC2272 should be studied carefully before using this
module, `snmp_pdus`.

The module `snmp_pdus` contains functions for encoding and decoding of SNMP
protocol data units (PDUs). In short, this module converts a list of bytes to
Erlang record representations and vice versa. The record definitions can be
found in the file `snmp/include/snmp_types.hrl`. If snmpv3 is used, the module
that includes `snmp_types.hrl` must define the constant `SNMP_USE_V3` before the
header file is included. Example:

```erlang
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").
```

Encoding and decoding must be done explicitly when writing your own Net if
process.

# `message`

```erlang
-type message() :: #message{version :: term(), vsn_hdr :: term(), data :: term()}.
```

The message is version dependent. 'vsn_hdr' is either a community string (v1 and
v2) or a 'v3_hdr' record (v3). 'data' is either a PDU (v1 and v2c) or a
(possibly encrypted) 'scopedPdu'.

# `msg_id`

```erlang
-type msg_id() :: 0..2147483647.
```

# `msg_security_model`

```erlang
-type msg_security_model() :: 0..2147483647.
```

# `pdu`

```erlang
-type pdu() ::
          #pdu{type :: term(),
               request_id :: term(),
               error_status :: term(),
               error_index :: term(),
               varbinds :: term()}.
```

# `pdu_type`

```erlang
-type pdu_type() ::
          'get-request' | 'get-next-request' | 'get-bulk-request' | 'get-response' | 'set-request' |
          'inform-request' | 'snmpv2-trap' | report.
```

# `scoped_pdu`

```erlang
-type scoped_pdu() :: #scopedPdu{contextEngineID :: term(), contextName :: term(), data :: term()}.
```

# `trappdu`

```erlang
-type trappdu() ::
          #trappdu{enterprise :: term(),
                   agent_addr :: term(),
                   generic_trap :: term(),
                   specific_trap :: term(),
                   time_stamp :: term(),
                   varbinds :: term()}.
```

# `usm_security_parameters`

```erlang
-type usm_security_parameters() ::
          #usmSecurityParameters{msgAuthoritativeEngineID :: term(),
                                 msgAuthoritativeEngineBoots :: term(),
                                 msgAuthoritativeEngineTime :: term(),
                                 msgUserName :: term(),
                                 msgAuthenticationParameters :: term(),
                                 msgPrivacyParameters :: term()}.
```

# `v3_hdr`

```erlang
-type v3_hdr() ::
          #v3_hdr{msgID :: term(),
                  msgMaxSize :: term(),
                  msgFlags :: term(),
                  msgSecurityModel :: term(),
                  msgSecurityParameters :: term(),
                  hdr_size :: term()}.
```

# `version`

```erlang
-type version() :: 'version-1' | 'version-2' | 'version-3'.
```

# `dec_message`

```erlang
-spec dec_message(Bytes) -> Message when Bytes :: [byte()], Message :: message().
```

Decodes a list of bytes into an SNMP Message. Note, if there is a v3 message,
the `msgSecurityParameters` are not decoded. They must be explicitly decoded by
a call to a security model specific decoding function, e.g.
[`dec_usm_security_parameters/1`](`dec_usm_security_parameters/1`). Also note,
if the `scopedPDU` is encrypted, the OCTET STRING encoded `encryptedPDU` will be
present in the `data` field.

# `dec_message_only`

```erlang
-spec dec_message_only(Bytes) -> Message when Bytes :: [byte()], Message :: message().
```

Decodes a list of bytes into an SNMP Message, but does not decode the data part
of the Message. That means, data is still a list of bytes, normally an encoded
`PDU` (v1 and V2) or an encoded and possibly encrypted `scopedPDU` (v3).

# `dec_pdu`

```erlang
-spec dec_pdu(Bytes) -> Pdu when Bytes :: [byte()], Pdu :: trappdu() | pdu().
```

Decodes a list of bytes into an SNMP Pdu.

# `dec_scoped_pdu`

```erlang
-spec dec_scoped_pdu(Bytes) -> ScopedPDU when Bytes :: [byte()], ScopedPDU :: scoped_pdu().
```

Decodes a list of bytes into an SNMP ScopedPdu.

# `dec_scoped_pdu_data`

```erlang
-spec dec_scoped_pdu_data(Bytes) -> ScopedPduData
                             when
                                 Bytes :: [byte()],
                                 ScopedPduData :: scoped_pdu() | EncryptedPDU,
                                 EncryptedPDU :: [byte()].
```

Decodes a list of bytes into either a scoped pdu record, or - if the scoped pdu
was encrypted - to a list of bytes.

# `dec_usm_security_parameters`

```erlang
-spec dec_usm_security_parameters(Bytes) -> UsmSecParams
                                     when Bytes :: [byte()], UsmSecParams :: usm_security_parameters().
```

Decodes a list of bytes into an SNMP UsmSecurityParameters.

# `enc_message`

```erlang
-spec enc_message(Message) -> Bytes when Message :: message(), Bytes :: [byte()].
```

Encodes a message record to a list of bytes.

# `enc_message_only`

```erlang
-spec enc_message_only(Message) -> Bytes when Message :: message(), Bytes :: [byte()].
```

`Message` is a record where the `data` field is assumed to be encoded (a list of
bytes). If there is a v1 or v2 message, the `data` field is an encoded `PDU`,
and if there is a v3 message, `data` is an encoded and possibly encrypted
`scopedPDU`.

# `enc_pdu`

```erlang
-spec enc_pdu(Pdu) -> Bytes when Pdu :: pdu(), Bytes :: [byte()].
```

Encodes an SNMP Pdu into a list of bytes.

# `enc_scoped_pdu`

```erlang
-spec enc_scoped_pdu(ScopedPdu) -> Bytes when ScopedPdu :: scoped_pdu(), Bytes :: [byte()].
```

Encodes an SNMP ScopedPdu into a list of bytes, which can be encrypted, and
after encryption, encoded with a call to `enc_encrypted_scoped_pdu/1`; or it can
be used as the `data` field in a `message` record, which then can be encoded
with [`enc_message_only/1`](`enc_message_only/1`).

# `enc_usm_security_parameters`

```erlang
-spec enc_usm_security_parameters(UsmSecParams) -> Bytes
                                     when UsmSecParams :: usm_security_parameters(), Bytes :: [byte()].
```

Encodes SNMP UsmSecurityParameters into a list of bytes.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
