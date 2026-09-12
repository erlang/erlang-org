# `snmpa_mib_data`
[🔗](https://github.com/erlang/otp/blob/master/lib/snmp/src/agent/snmpa_mib_data.erl#L22)

Behaviour module for the SNMP agent mib-server data module.

This module defines the behaviour of the SNMP agent mib-server data module. A
`snmpa_mib_data` compliant module must export the following functions:

- [new/1](`c:snmpa_mib_data:new/1`)
- [close/1](`c:snmpa_mib_data:close/1`)
- [sync/1](`c:snmpa_mib_data:sync/1`)
- [load_mib/4](`c:snmpa_mib_data:load_mib/4`)
- [unload_mib/4](`c:snmpa_mib_data:unload_mib/4`)
- [lookup/2](`c:snmpa_mib_data:lookup/2`)
- [next/3](`c:snmpa_mib_data:next/3`)
- [register_subagent/3](`c:snmpa_mib_data:register_subagent/3`)
- [unregister_subagent/2](`c:snmpa_mib_data:unregister_subagent/2`)
- [which_mib/2](`c:snmpa_mib_data:which_mib/2`)
- [which_mibs/1](`c:snmpa_mib_data:which_mibs/1`)
- [whereis_mib/2](`c:snmpa_mib_data:whereis_mib/2`)
- [dump/2](`c:snmpa_mib_data:dump/2`)
- [info/1](`c:snmpa_mib_data:info/1`)
- [backup/2](`c:snmpa_mib_data:backup/2`)
- [code_change/4](`c:snmpa_mib_data:code_change/4`)

The semantics of them and their exact signatures are explained below.

Note that the data extracted from the imported (loaded) mibs are stored partly
by the mib-server and partly by the symbolic-store server. See the default
mib-server data module, `snmpa_mib_data_tttn` for details.

# `filename`
*not exported* *since OTP R16B01* 

```erlang
-type filename() :: file:filename().
```

# `mib_view`
*since OTP R16B01* 

```erlang
-type mib_view() :: [mib_view_elem()].
```

# `mib_view_elem`
*since OTP R16B01* 

```erlang
-type mib_view_elem() ::
          {SubTree :: snmp:oid(), Mask :: [non_neg_integer()], Inclusion :: mib_view_inclusion()}.
```

# `mib_view_inclusion`
*since OTP R16B01* 

```erlang
-type mib_view_inclusion() :: 1 | 2.
```

# `mib_view_mask`
*since OTP R16B01* 

```erlang
-type mib_view_mask() :: [non_neg_integer()].
```

# `backup`
*since OTP R16B01* 

```erlang
-callback backup(State :: term(), BackupDir :: string()) -> ok | {error, Reason :: term()}.
```

Perform a backup of the mib-server data.

Note that its implementation dependent (and also dependent on mib-storage is
used) if a backup is possible.

# `close`
*since OTP R16B01* 

```erlang
-callback close(State :: term()) -> ok.
```

Close the mib-storage.

# `code_change`
*since OTP R16B01* 

```erlang
-callback code_change(Direction :: up | down, Vsn :: term(), Extra :: term(), State :: term()) ->
                         NewState :: term().
```

Perform a code-change (upgrade or downgrade).

See `m:gen_server` for more info regarding the `Vsn` and `Extra` arguments.

# `dump`
*since OTP R16B01* 

```erlang
-callback dump(State :: term(), Destination :: io | filename()) -> ok | {error, Reason :: term()}.
```

Dump the mib-server data to `stdio` (Destination = `io`) or the specified file.

# `info`
*since OTP R16B01* 

```erlang
-callback info(State :: term()) -> list().
```

Retrieve misc info for the mib data.

This is a utility function used to inspect, for instance, memory usage, in a
simple way.

# `load_mib`
*since OTP R16B01* 

```erlang
-callback load_mib(State :: term(),
                   FileName :: filename(),
                   MeOverride :: boolean(),
                   TeOverride :: boolean()) ->
                      {ok, NewState :: term()} | {error, Reason :: already_loaded | term()}.
```

Load the mib specified by the `Filename` argument into the mib-server. The
`MeOverride` and `TeOverride` arguments specifies how the mib-server shall
handle duplicate mib- and trap- entries.

# `lookup`
*since OTP R16B01* 

```erlang
-callback lookup(State :: term(), Oid :: snmp:oid()) ->
                    {false, Reason :: term()} |
                    {variable, MibEntry :: snmpa:me()} |
                    {table_column, MibEntry :: snmpa:me(), TableEntryOid :: snmp:oid()} |
                    {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()}.
```

Find the mib-entry corresponding to the `Oid`. If it is a variable, the `Oid`
must be <Oid for var>.0 and if it is a table, `Oid` must be

# `new`
*since OTP R16B01* 

```erlang
-callback new(MibStorage :: snmpa:mib_storage()) -> State :: term().
```

Create a new mib-server data instance.

# `next`
*since OTP R16B01* 

```erlang
-callback next(State :: term(), Oid :: snmp:oid(), MibView :: mib_view()) ->
                  endOfView | false |
                  {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()} |
                  {variable, MibEntry :: snmpa:me(), VarOid :: snmp:oid()} |
                  {table, TableOid :: snmp:oid(), TableRestOid :: snmp:oid(), MibEntry :: snmpa:me()}.
```

Finds the lexicographically next oid.

# `register_subagent`
*since OTP R16B01* 

```erlang
-callback register_subagent(State :: term(), Oid :: snmp:oid(), Pid :: pid()) ->
                               {ok, NewState :: term()} | {error, Reason :: term()}.
```

Register the subagent, process, handling part of the mib-tree.

# `sync`
*since OTP R16B01* 

```erlang
-callback sync(State :: term()) -> ok.
```

Synchronize (write to disc, if possible) the mib-server data. This depends on
the `mib_storage` option, and will only have an effect if the mib-storage option
has an actual disc component (such as dets, or ets with a file).

# `unload_mib`
*since OTP R16B01* *optional* 

```erlang
-callback unload_mib(State, Filename) -> {ok, NewState} | {error, Reason}
                        when
                            State :: term(),
                            Filename :: filename(),
                            NewState :: term(),
                            Reason :: not_loaded | term().
```

# `unload_mib`
*since OTP R16B01* 

```erlang
-callback unload_mib(State :: term(),
                     FileName :: filename(),
                     MeOverride :: boolean(),
                     TeOverride :: boolean()) ->
                        {ok, NewState :: term()} | {error, Reason :: not_loaded | term()}.
```

Unload the mib specified by the `Filename` argument from the mib-server. The
`MeOverride` and `TeOverride` arguments specifies how the mib-server shall
handle duplicate mib- and trap- entries.

# `unregister_subagent`
*since OTP R16B01* 

```erlang
-callback unregister_subagent(State :: term(), PidOrOid :: pid() | snmp:oid()) ->
                                 {ok, NewState :: term()} |
                                 {ok, NewState :: term(), Pid :: pid()} |
                                 {error, Reason :: term()}.
```

Unregister the subagent, handling part of the mib-tree, as specified by the
`oid()` or `t:pid/0` (`PidOrOid`).

When unregister the subagent using an `oid()`, the `t:pid/0` of the process
handling the sub-tree is also returned.

# `whereis_mib`
*since OTP R16B01* 

```erlang
-callback whereis_mib(State :: term(), MibName :: atom()) ->
                         {ok, Filename :: filename()} | {error, Reason :: term()}.
```

Retrieve the mib file for the mib.

# `which_mib`
*since OTP R16B01* 

```erlang
-callback which_mib(State :: term(), Oid :: snmp:oid()) -> {ok, Mib :: string()} | {error, Reason :: term()}.
```

Retrieve the mib-file to which an given `oid()` belongs.

# `which_mibs`
*since OTP R16B01* 

```erlang
-callback which_mibs(State :: term()) -> [{MibName :: atom(), Filename :: filename()}].
```

Retrieve all loaded mib-files.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
