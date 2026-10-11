# `megaco_encoder`
[🔗](https://github.com/erlang/otp/blob/master/lib/megaco/src/engine/megaco_encoder.erl#L28)

Megaco encoder behaviour.

The following functions should be exported from a `megaco_encoder` callback
module:

## DATA TYPES

> #### Note {: .info }
>
> Note that the actual definition of (some of) these records depend on the
> megaco protocol version used. For instance, the `'TransactionReply'` record
> has two more fields in version 3, so a simple erlang type definition cannot be
> made here.

```text
protocol_version() = integer()
segment_no()       = integer()
megaco_message() = #'MegacoMessage{}'
transaction() = {transactionRequest,     transaction_request()}      |
                {transactionPending,     transaction_reply()}        |
                {transactionReply,       transaction_pending()}      |
                {transactionResponseAck, transaction_response_ack()} |
                {segmentReply,           segment_reply()}
transaction_request() = #'TransactionRequest'{}
transaction_pending() = #'TransactionPending'{}
transaction_reply() = #'TransactionReply'{}
transaction_response_ack() = [transaction_ack()]
transaction_ack() = #'TransactionAck'{}
segment_reply() = #'SegmentReply'{}
action_request() = #'ActionRequest'{}
action_reply() = #'ActionReply'{}
command_request() = #'CommandRequest'{}
error_desc()   = #'ErrorDescriptor'{}
```

# `action_reply`

```erlang
-type action_reply() :: {'ActionReply', _, _, _, _}.
```

# `action_request`

```erlang
-type action_request() :: {'ActionRequest', _, _, _, _}.
```

# `alpha`

```erlang
-type alpha() :: 65..90 | 97..122.
```

Alpha Numeric characters: `A..Z | a..z`

# `command_request`

```erlang
-type command_request() :: {'CommandRequest', _, _, _}.
```

# `deviceName`

```erlang
-type deviceName() :: pathName().
```

# `digit`

```erlang
-type digit() :: 48..57.
```

Decimal digits: `0..9`

# `domainName`

```erlang
-type domainName() :: #'DomainName'{name :: term(), portNumber :: term()}.
```

# `error_desc`

```erlang
-type error_desc() :: #'ErrorDescriptor'{errorCode :: term(), errorText :: term()}.
```

# `ip4Address`

```erlang
-type ip4Address() :: #'IP4Address'{address :: term(), portNumber :: term()}.
```

# `ip6Address`

```erlang
-type ip6Address() :: #'IP6Address'{address :: term(), portNumber :: term()}.
```

# `megaco_message`

```erlang
-type megaco_message() :: #'MegacoMessage'{authHeader :: term(), mess :: term()}.
```

# `mtpAddress`

```erlang
-type mtpAddress() :: octet_string().
```

There is no way to properly express this type in the Erlang type system, so this
is the best we can do.

A proper definition would be: `-type mtpAddress() :: octet_string(2..4).`

# `octet`

```erlang
-type octet() :: 0..255.
```

# `octet_string`

```erlang
-type octet_string() :: [octet()].
```

# `pathName`

```erlang
-type pathName() :: [$* | alpha() | digit() | $_ | $/ | $$ | $@ | $- | $.].
```

There is no way to properly express this type in the Erlang type system, so this
is the best we can do. The minimum length is 1 and the maximum length is 64.

Here is the ABNF (copied from the megaco standard) to fill in the blanks:

`# Total length of pathNAME must not exceed 64 chars.`

`pathNAME = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) ["@" pathDomainName ]`

`# ABNF allows two or more consecutive "." although it is meaningless in a path domain name.`

`pathDomainName = (ALPHA / DIGIT / "*" ) *63(ALPHA / DIGIT / "-" / "*" / ".")`

`NAME = ALPHA *63(ALPHA / DIGIT / "_" )`

# `protocol_version`

```erlang
-type protocol_version() :: pos_integer().
```

# `segment_no`

```erlang
-type segment_no() :: 0..65535.
```

# `segment_reply`

```erlang
-type segment_reply() ::
          #'SegmentReply'{transactionId :: term(),
                          segmentNumber :: term(),
                          segmentationComplete :: term()}.
```

# `transaction`

```erlang
-type transaction() ::
          {transactionRequest, transaction_request()} |
          {transactionPending, transaction_reply()} |
          {transactionReply, transaction_pending()} |
          {transactionResponseAck, transaction_response_ack()} |
          {segmentReply, segment_reply()}.
```

# `transaction_ack`

```erlang
-type transaction_ack() :: #'TransactionAck'{firstAck :: term(), lastAck :: term()}.
```

# `transaction_pending`

```erlang
-type transaction_pending() :: #'TransactionPending'{transactionId :: term()}.
```

# `transaction_reply`

```erlang
-type transaction_reply() :: {'TransactionReply', _, _} | {'TransactionReply', _, _, _, _}.
```

The problem with TransactionReply is that its definition depend on which version
of the protocol we are using. As of version 3, it has two more fields.

# `transaction_request`

```erlang
-type transaction_request() :: #'TransactionRequest'{transactionId :: term(), actions :: term()}.
```

# `transaction_response_ack`

```erlang
-type transaction_response_ack() :: [transaction_ack()].
```

# `decode_message`

```erlang
-callback decode_message(EncodingConfig, Version, Bin) -> {ok, Message} | Error
                            when
                                EncodingConfig :: list(),
                                Version :: protocol_version() | dynamic,
                                Bin :: binary(),
                                Message :: megaco_message(),
                                Error :: term().
```

Decode a megaco message.

Note that if the Version argument is `dynamic`, the decoder should try to figure
out the actual version from the message itself and then use the proper decoder,
e.g. version 1.  
If on the other hand the Version argument is an integer, it means that this is
the expected version of the message and the decoder for that version should be
used.

# `decode_mini_message`

```erlang
-callback decode_mini_message(EncodingConfig, Version, Bin) -> {ok, Message} | Error
                                 when
                                     EncodingConfig :: list(),
                                     Version :: protocol_version() | dynamic,
                                     Bin :: binary(),
                                     Message :: megaco_message(),
                                     Error :: term().
```

Perform a minimal decode of a megaco message.

The purpose of this function is to do a minimal decode of Megaco message. A
successfull result is a `'MegacoMessage'` in which only version and mid has been
initiated. This function is used by the megaco_messenger module when the
[`decode_message/3`](`c:decode_message/3`) function fails to figure out the mid
(the actual sender) of the message.

Note again that a successfull decode only returns a partially initiated message.

# `encode_action_reply`
*optional* 

```erlang
-callback encode_action_reply(EncodingConfig, Version, AR) -> {ok, Bin} | {error, Reason}
                                 when
                                     EncodingConfig :: list(),
                                     Version :: protocol_version(),
                                     AR :: action_reply(),
                                     Bin :: binary(),
                                     Reason :: not_implemented | term().
```

Encode a megaco action reply. If this, for whatever reason, is not supported,
the function should return the error reason `not_implemented`.

This function is used when segmentation has been configured. So, for this to
work, this function _must_ be fully supported\!

# `encode_action_requests`

```erlang
-callback encode_action_requests(EncodingConfig, Version, ARs) -> {ok, Bin} | {error, Reason}
                                    when
                                        EncodingConfig :: list(),
                                        Version :: protocol_version(),
                                        ARs :: [action_request()],
                                        Bin :: binary(),
                                        Reason :: not_implemented | term().
```

Encode megaco action requests. This function is called when the user calls the
function [encode_actions/3](`megaco:encode_actions/3`). If that function is
never used or if the codec cannot support this (the encoding of individual
actions), then return with error reason `not_implemented`.

# `encode_message`

```erlang
-callback encode_message(EncodingConfig, Version, Message) -> {ok, Bin} | Error
                            when
                                EncodingConfig :: list(),
                                Version :: protocol_version(),
                                Message :: megaco_message(),
                                Bin :: binary(),
                                Error :: term().
```

Encode a megaco message.

# `encode_transaction`

```erlang
-callback encode_transaction(EncodingConfig, Version, Transaction) -> {ok, Bin} | {error, Reason}
                                when
                                    EncodingConfig :: list(),
                                    Version :: protocol_version(),
                                    Transaction :: transaction(),
                                    Bin :: binary(),
                                    Reason :: not_implemented | term().
```

Encode a megaco transaction. If this, for whatever reason, is not supported, the
function should return the error reason `not_implemented`.

This functionality is used both when the transaction sender is used and for
segmentation. So, for either of those to work, this function _must_ be fully
supported\!

---

*Consult [api-reference.md](api-reference.md) for complete listing*
