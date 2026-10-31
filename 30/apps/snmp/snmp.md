# `snmp`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/app/snmp.erl#L22)

Interface functions to the SNMP toolkit

The module `snmp` contains interface functions to the SNMP toolkit.

### See Also

calendar(3)

# `algorithm`
*not exported* 

```erlang
-type algorithm() :: md5 | sha | sha224 | sha256 | sha384 | sha512.
```

# `asn1_type`

```erlang
-type asn1_type() ::
          #asn1_type{bertype :: term(),
                     lo :: term(),
                     hi :: term(),
                     assocList :: term(),
                     imported :: term(),
                     aliasname :: term(),
                     implied :: term(),
                     display_hint :: term()}.
```

# `atl_type`

```erlang
-type atl_type() :: read | write | read_write.
```

# `bits`

```erlang
-type bits() :: integer().
```

The Erlang representation of the SNMP BITS (pseudo) data type.

# `column`

```erlang
-type column() :: pos_integer().
```

# `community`

```erlang
-type community() :: snmp_community_mib:name().
```

# `context_name`

```erlang
-type context_name() :: snmp_community_mib:context_name().
```

# `date_and_time_validator`

```erlang
-type date_and_time_validator() ::
          fun((Kind :: date_and_time_validator_kind(), Data :: term()) -> boolean()).
```

The input to the validator fun looks like this:

```text
	  Kind             Data
	  --------------   ----------------------
	  year             {Year1, Year2}
	  month            Month
	  day              Day
	  hour             Hour
	  minute           Minute
	  seconds          Seconds
	  deci_seconds     DeciSeconds
	  diff             [Sign, Hour, Minute]
	  valid_date       {Year, Month, Day}
```

# `date_and_time_validator_kind`

```erlang
-type date_and_time_validator_kind() ::
          year | month | day | hour | minute | seconds | deci_seconds | diff | valid_date.
```

# `dir`

```erlang
-type dir() :: string().
```

A string, that is a file path to a directory.

# `engine_id`

```erlang
-type engine_id() :: snmp_framework_mib:engine_id().
```

# `error_index`

```erlang
-type error_index() :: non_neg_integer().
```

`0` is used when error status is `noError` and when error status is an actual
error; error index is `t:pos_integer/0`.

# `error_status`

```erlang
-type error_status() :: noError | atom().
```

We should really specify all of these, but they are so numerous... Also,
normally all you need to know is that `'noError'` is ok and everything else is
an error.

# `ivarbind`

```erlang
-type ivarbind() :: #ivarbind{status :: term(), mibentry :: term(), varbind :: term()}.
```

# `log_size`

```erlang
-type log_size() ::
          infinity | pos_integer() | {MaxNoBytes :: pos_integer(), MaxNoFiles :: pos_integer()}.
```

This is basically a copy of the [dlog_size()](`t:disk_log:dlog_size/0`).

# `log_time`

```erlang
-type log_time() ::
          calendar:datetime() |
          {local_time, calendar:datetime()} |
          {universal_time, calendar:datetime()}.
```

# `me`

```erlang
-type me() ::
          #me{oid :: term(),
              entrytype :: term(),
              aliasname :: term(),
              asn1_type :: term(),
              access :: term(),
              mfa :: term(),
              imported :: term(),
              assocList :: term(),
              description :: term(),
              units :: term()}.
```

# `mib`

```erlang
-type mib() ::
          #mib{misc :: term(),
               mib_format_version :: term(),
               name :: term(),
               module_identity :: term(),
               mes :: term(),
               asn1_types :: term(),
               traps :: term(),
               variable_infos :: term(),
               table_infos :: term(),
               imports :: term()}.
```

# `mib_name`

```erlang
-type mib_name() :: string().
```

# `mms`

```erlang
-type mms() :: snmp_framework_mib:max_message_size().
```

# `notification`

```erlang
-type notification() ::
          #notification{trapname :: term(), oid :: term(), oidobjects :: term(), description :: term()}.
```

# `octet`

```erlang
-type octet() :: 0..255.
```

# `octet_string`

```erlang
-type octet_string() :: [octet()].
```

# `oid`

```erlang
-type oid() :: [non_neg_integer()].
```

Represent an ASN.1 OBJECT IDENTIFIER.

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
-type pdu_type() :: snmp_pdus:pdu_type().
```

# `rfc1903_date_and_time`

```erlang
-type rfc1903_date_and_time() :: octet_string().
```

The data type DateAndTime, an OCTET STRING, as specified in RFC1903.

# `row_index`

```erlang
-type row_index() :: oid().
```

Denotes the last part of the OID which specifies the index of the row in the
table (see RFC1212, 4.1.6 for more information about INDEX).

# `row_pointer`

```erlang
-type row_pointer() :: oid().
```

> #### Note {: .info }
>
> "Represents a pointer to a conceptual row. The value is the name of the
> instance of the first accessible columnar object in the conceptual row."

`OBJECT IDENTIFIER`

Defined by SNMPv2-TC.

# `sec_level`

```erlang
-type sec_level() :: snmp_framework_mib:security_level().
```

# `sec_model`

```erlang
-type sec_model() :: snmp_framework_mib:security_model().
```

# `sec_name`

```erlang
-type sec_name() :: snmp_framework_mib:admin_string().
```

# `snmp_timer`

```erlang
-type snmp_timer() ::
          #snmp_incr_timer{wait_for :: term(), factor :: term(), incr :: term(), max_retries :: term()}.
```

# `table_info`

```erlang
-type table_info() ::
          #table_info{nbr_of_cols :: term(),
                      defvals :: term(),
                      status_col :: term(),
                      not_accessible :: term(),
                      index_types :: term(),
                      first_accessible :: term(),
                      first_own_index :: term()}.
```

# `taddress`

```erlang
-type taddress() :: snmpa_conf:transportAddress().
```

# `tdomain`

```erlang
-type tdomain() :: transportDomainUdpIpv4 | transportDomainUdpIpv6.
```

# `time_interval`

```erlang
-type time_interval() :: 0..2147483647.
```

> #### Note {: .info }
>
> "A period of time, measured in units of 0.01 seconds."

`INTEGER (0..2147483647)`

Defined by SNMPv2-TC.

# `trap`

```erlang
-type trap() ::
          #trap{trapname :: term(),
                enterpriseoid :: term(),
                specificcode :: term(),
                oidobjects :: term(),
                description :: term()}.
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

# `usm_auth_key`

```erlang
-type usm_auth_key() :: snmp_user_based_sm_mib:auth_key().
```

# `usm_auth_protocol`

```erlang
-type usm_auth_protocol() :: snmp_user_based_sm_mib:auth_protocol().
```

# `usm_name`

```erlang
-type usm_name() :: snmp_user_based_sm_mib:name().
```

# `usm_priv_key`

```erlang
-type usm_priv_key() :: snmp_user_based_sm_mib:priv_key().
```

# `usm_priv_protocol`

```erlang
-type usm_priv_protocol() :: snmp_user_based_sm_mib:priv_protocol().
```

# `varbind`

```erlang
-type varbind() :: #varbind{oid :: term(), variabletype :: term(), value :: term(), org_index :: term()}.
```

# `variable_info`

```erlang
-type variable_info() :: #variable_info{defval :: term()}.
```

# `verbosity`

```erlang
-type verbosity() :: silence | info | log | debug | trace.
```

For the lowest verbosity `silence`, nothing is printed. The higher the
verbosity, the more is printed.

# `version`

```erlang
-type version() :: v1 | v2 | v3.
```

# `void`

```erlang
-type void() :: term().
```

The type is used when a functions return is to be ignored.

# `bits_to_octet_string`

```erlang
-spec bits_to_octet_string(B) -> octet_string() when B :: bits().
```

Utility function for converting a value of type `BITS` to `OCTET-STRING`,
according to RFC1906, section 8.

# `change_log_size`

```erlang
-spec change_log_size(LogName, NewSize) -> ok | {error, Reason}
                         when LogName :: string(), NewSize :: log_size(), Reason :: term().
```

Changes the log size of the Audit Trail Log. The application must be configured
to use the audit trail log function. Please refer to disk_log(3) in Kernel
Reference Manual for a description of how to change the log size.

The change is permanent, as long as the log is not deleted. That means, the log
size is remembered across reboots.

# `config`

```erlang
-spec config() -> ok | {error, Reason} when Reason :: term().
```

A simple interactive configuration tool. Simple configuration files can be
generated, but more complex configurations still have to be edited manually.

The tool is a textual based tool that asks some questions and generates
`sys.config` and `*.conf` files.

_Note_ that if the application shall support version 3, then the crypto app must
be started before running this function (password generation).

_Note_ also that some of the configuration files for the agent and manager share
the same names. This means that they have to be stored in _different_
directories\!

# `date_and_time`

```erlang
-spec date_and_time() -> DateAndTime when DateAndTime :: rfc1903_date_and_time().
```

Returns current date and time as the data type DateAndTime, as specified in
RFC1903. This is an OCTET STRING.

# `date_and_time_to_string2`

```erlang
-spec date_and_time_to_string2(DAT) -> string() when DAT :: rfc1903_date_and_time().
```

Converts a DateAndTime list to a printable string, according to the DISPLAY-HINT
definition in RFC2579, with the extension that it also allows the values "hours
from UTC" = 14 together with "minutes from UTC" = 0.

# `date_and_time_to_string`

```erlang
-spec date_and_time_to_string(DAT) -> string() when DAT :: rfc1903_date_and_time().
```

# `date_and_time_to_string`

```erlang
-spec date_and_time_to_string(DAT, Validate) -> string()
                                 when
                                     DAT :: rfc1903_date_and_time(),
                                     Validate :: date_and_time_validator().
```

Converts a DateAndTime list to a printable string, according to the DISPLAY-HINT
definition in RFC2579.

The validation fun, `Validate`, allows for a more "flexible" validation of the
`DateAndTime` argument. Whenever the data is found to not follow RFC2579, the
fun is called to allow a more "lax" validation. See the
[`validate_date_and_time/2`](`snmp:validate_date_and_time/2`) function for
more info on the `Validate` fun.

# `date_and_time_to_universal_time_dst`

```erlang
-spec date_and_time_to_universal_time_dst(DAT) -> UTCs
                                             when
                                                 DAT :: rfc1903_date_and_time(),
                                                 UTCs :: [calendar:datetime1970()].
```

Converts a DateAndTime list to a list of possible universal time(s). The
universal time value on the same format as defined in calendar(3).

# `disable_trace`

```erlang
-spec disable_trace() -> void().
```

Stop the tracer.

# `enable_trace`

```erlang
-spec enable_trace() -> void().
```

Starts a dbg tracer that prints trace events to stdout (using plain io:format
after a minor formatting).

# `local_time_to_date_and_time_dst`

```erlang
-spec local_time_to_date_and_time_dst(Local) -> DATs
                                         when
                                             Local :: calendar:datetime1970(),
                                             DATs :: [rfc1903_date_and_time()].
```

Converts a local time value to a list of possible DateAndTime list(s). The local
time value on the same format as defined in calendar(3).

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile, Start) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Start :: null | log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R15B01* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Start :: null | log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term();
               (LogDir, Mibs, LogName, LogFile, Start, Stop) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Start :: null | log_time(),
                       Stop :: null | log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

# `log_to_io`
*since OTP R16B03* 

```erlang
-spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> ok | {ok, Cnt} | {error, Reason}
                   when
                       LogDir :: string(),
                       Mibs :: [mib_name()],
                       LogName :: string(),
                       LogFile :: string(),
                       Block :: boolean(),
                       Start :: null | log_time(),
                       Stop :: null | log_time(),
                       Cnt :: {NumOK, NumERR},
                       NumOK :: non_neg_integer(),
                       NumERR :: pos_integer(),
                       Reason :: term().
```

Converts an Audit Trail Log to a readable format and prints it on stdio. See
[`log_to_txt/8`](`snmp:log_to_txt/8`) for more info.

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block) -> ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term();
                (LogDir, Mibs, OutFile, LogName, LogFile, Start) -> ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Start :: null | log_time(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term().
```

# `log_to_txt`

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start) ->
                    ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Start :: null | log_time(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term();
                (LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) ->
                    ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Start :: null | log_time(),
                        Stop :: null | log_time(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term().
```

# `log_to_txt`
*since OTP R16B03* 

```erlang
-spec log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) ->
                    ok | {ok, Cnt} | {error, Reason}
                    when
                        LogDir :: string(),
                        Mibs :: [mib_name()],
                        OutFile :: string(),
                        LogName :: string(),
                        LogFile :: string(),
                        Block :: boolean(),
                        Start :: null | log_time(),
                        Stop :: null | log_time(),
                        Cnt :: {NumOK, NumERR},
                        NumOK :: non_neg_integer(),
                        NumERR :: pos_integer(),
                        Reason :: term().
```

Converts an Audit Trail Log to a readable text file, where each item has a
trailing TAB character, and any TAB character in the body of an item has been
replaced by ESC TAB.

The function can be used on a running system, or by copying the entire log
directory and calling this function. SNMP must be running in order to provide
MIB information.

`LogDir` is the name of the directory where the audit trail log is stored.
`Mibs` is a list of Mibs to be used. The function uses the information in the
Mibs to convert for example object identifiers to their symbolic name. `OutFile`
is the name of the generated text-file. `LogName` is the name of the log,
`LogFile` is the name of the log file. `Start` is the start (first) date and
time from which log events will be converted and `Stop` is the stop (last) date
and time to which log events will be converted. The `Block` argument indicates
if the log should be blocked during conversion. This could be useful when
converting large logs (when otherwise the log could wrap during conversion).
Defaults to `true`.

The format of an audit trail log text item is as follows:

`Tag Addr - Community [TimeStamp] Vsn`  
`PDU`

where `Tag` is `request`, `response`, `report`, `trap` or `inform`; Addr is
`IP:Port` (or comma space separated list of such); `Community` is the community
parameter (SNMP version v1 and v2), or `SecLevel:"AuthEngineID":"UserName"`
(SNMP v3); `TimeStamp` is a date and time stamp, and `Vsn` is the SNMP version.
`PDU` is a textual version of the protocol data unit. There is a new line
between `Vsn` and `PDU`.

If the entire log is successfully converted, the function will return `ok`. If
one of more entries fail to convert, the function will instead return
`{ok, {NumOK, NumERR}}`, where the counters indicate how many valid and
erroneous entries where found. If instead `{error, Reason}` is returned, the
conversion encountered a fatal error and where either never done of aborted
midway.

# `octet_string_to_bits`

```erlang
-spec octet_string_to_bits(S) -> bits() when S :: octet_string().
```

Utility function for converting a value of type `OCTET-STRING` to `BITS`,
according to RFC1906, section 8.

# `passwd2localized_key`

```erlang
-spec passwd2localized_key(Algorithm, Passwd, EngineID) -> Key
                              when
                                  Algorithm :: algorithm(),
                                  Passwd :: string(),
                                  EngineID :: string(),
                                  Key :: list().
```

Generates a key that can be used as an authentication or privacy key using MD5,
SHA, SHA224, SHA256, SHA384 or SHA512. The key is localized for EngineID.

# `print_version_info`

```erlang
-spec print_version_info() -> void().
```

# `print_version_info`

```erlang
-spec print_version_info(Prefix) -> void() when Prefix :: string() | non_neg_integer().
```

Utility function(s) to produce a formatted printout of the versions info
generated by the `versions1` function

This is the same as doing, e.g.:

```erlang
           {ok, V} = snmp:versions1(),
           snmp:print_versions(V).
```

# `print_versions`

```erlang
-spec print_versions(Versions) -> void() when Versions :: [VersionInfo], VersionInfo :: term().
```

# `print_versions`

```erlang
-spec print_versions(Prefix, Versions) -> void()
                        when
                            Prefix :: string() | non_neg_integer(),
                            Versions :: [VersionInfo],
                            VersionInfo :: term().
```

Utility function to produce a formatted printout of the versions info generated
by the `versions1` and `versions2` functions

Example:

```erlang
           {ok, V} = snmp:versions1(),
           snmp:print_versions(V).
```

# `read_mib`

```erlang
-spec read_mib(FileName) -> {ok, Mib} | {error, Reason}
                  when FileName :: string(), Mib :: mib(), Reason :: term().
```

Read a compiled mib.

# `reset_trace`

```erlang
-spec reset_trace(Targets) -> void() when Targets :: module() | [module()].
```

This function is used to reset (disable) trace for the given module(s).

# `set_trace`

```erlang
-spec set_trace(Targets) -> void()
                   when
                       Targets :: module() | [module() | {module(), [TargetOpt]}],
                       TargetOpt :: {return_trace, boolean()} | {scope, Scope},
                       Scope ::
                           all_functions | exported_functions | FunctionName |
                           {FunctionName, FunctionArity},
                       FunctionName :: atom(),
                       FunctionArity :: non_neg_integer().
```

This function is used to set up default trace on function(s) for the given
module or modules. The scope of the trace will be all _exported_ functions (both
the call info and the return value). Timestamp info will also be included.

# `set_trace`

```erlang
-spec set_trace(Targets, TraceOpts) -> void()
                   when
                       Targets :: module() | [module() | {module(), [TargetOpt]}],
                       TargetOpt :: {return_trace, boolean()} | {scope, Scope},
                       Scope ::
                           all_functions | exported_functions | FunctionName |
                           {FunctionName, FunctionArity},
                       FunctionName :: atom(),
                       FunctionArity :: non_neg_integer(),
                       TraceOpts :: disable | [TraceOpt],
                       TraceOpt :: {timestamp, boolean()} | TargetOpt.
```

This function is used to set up trace on function(s) for the given module or
modules.

The example below sets up trace on the exported functions (default) of module
`snmp_generic` and all functions of module `snmp_generic_mnesia`. With return
values (which is default) and timestamps in both cases (which is also default):

```erlang
	  snmp:enable_trace(),
	  snmp:set_trace([snmp_generic,
                          {snmp_generic_mnesia, [{scope, all_functions}]}]),
	  .
	  .
	  .
          snmp:set_trace(snmp_generic, disable),
	  .
	  .
	  .
	  snmp:disable_trace(),
```

# `start`

```erlang
-spec start() -> ok | {error, Reason} when Reason :: term().
```

# `start`

```erlang
-spec start(Type) -> ok | {error, Reason}
               when Type :: p | permanent | tr | transient | te | temporary, Reason :: term().
```

Starts the SNMP application.

See `m:application` for more info.

# `start_agent`

```erlang
-spec start_agent() -> ok | {error, Reason} when Reason :: term().
```

# `start_agent`

```erlang
-spec start_agent(Type) -> ok | {error, Reason} when Type :: application:start_type(), Reason :: term().
```

The SNMP application consists of several entities, of which the agent is one.
This function starts the agent entity of the application.

Note that the only way to actually start the agent in this way is to add the
agent related config after starting the application (e.g it cannot be part of
the normal application config; sys.config). This is done by calling:
`application:set_env(snmp, agent, Conf)`.

The default value for `Type` is `normal`.

# `start_manager`

```erlang
-spec start_manager() -> ok | {error, Reason} when Reason :: term().
```

# `start_manager`

```erlang
-spec start_manager(Type) -> ok | {error, Reason}
                       when Type :: application:start_type(), Reason :: term().
```

The SNMP application consists of several entities, of which the manager is one.
This function starts the manager entity of the application.

Note that the only way to actually start the manager in this way is to add the
manager related config after starting the application (e.g it cannot be part of
the normal application config; sys.config). This is done by calling:
`application:set_env(snmp, manager, Conf)`.

The default value for `Type` is `normal`.

# `stop`
*since OTP 27.0* 

```erlang
-spec stop() -> ok | {error, Reason} when Reason :: term().
```

Stops the SNMP application.

See `m:application` for more info.

This function has existed for long time,
but not had a proper since tag, so to simplify
we set the since tag to when it was documented.

# `universal_time_to_date_and_time`

```erlang
-spec universal_time_to_date_and_time(UTC) -> DateAndTime
                                         when
                                             UTC :: calendar:datetime(),
                                             DateAndTime :: rfc1903_date_and_time().
```

Converts a universal time value to a DateAndTime list. The universal time value
on the same format as defined in calendar(3).

# `validate_date_and_time`

```erlang
-spec validate_date_and_time(DateAndTime) -> boolean() when DateAndTime :: rfc1903_date_and_time().
```

# `validate_date_and_time`

```erlang
-spec validate_date_and_time(DateAndTime, Validate) -> boolean()
                                when
                                    DateAndTime :: rfc1903_date_and_time(),
                                    Validate :: date_and_time_validator().
```

Checks if `DateAndTime` is a correct DateAndTime value, as specified in RFC2579.
This function can be used in instrumentation functions to validate a DateAndTime
value.

The validation fun, `Validate`, allows for a more "flexible" validation of the
`DateAndTime` argument. Whenever the data is found to not follow RFC2579, the
fun is called to allow a more "lax" validation.

# `versions1`

```erlang
-spec versions1() -> {ok, VersionsInfo} | {error, Reason}
                   when VersionsInfo :: [VersionInfo], VersionInfo :: term(), Reason :: term().
```

# `versions2`

```erlang
-spec versions2() -> {ok, VersionsInfo} | {error, Reason}
                   when VersionsInfo :: [VersionInfo], VersionInfo :: term(), Reason :: term().
```

Utility functions used to retrieve some system and application info.

The difference between the two functions is in how they get the modules to
check. `versions1` uses the app-file and `versions2` uses the function
`application:get_key`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
