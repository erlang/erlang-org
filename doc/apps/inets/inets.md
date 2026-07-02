# `inets`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/inets/src/inets_app/inets.erl#L27)

The Inets services API.

This module provides the most basic API to the clients and servers that are part
of the `Inets` application, such as start and stop.

[](){: #common_data_types }

### Data types

Type definitions that are used more than once in this module:

`service() = httpc | httpd`

`property() = atom()`

### See also

`m:httpc`, `m:httpd`

# `inets_service`
*not exported* 

```erlang
-type inets_service() :: httpd | httpc.
```

# `service_info`
*not exported* 

```erlang
-type service_info() ::
          {inets_service(), pid(), [{profile, atom()}] | no_such_service | service_not_available}.
```

# `service_names`

```erlang
-spec service_names() -> [inets_service()].
```

Returns a list of available service names.

# `services`

```erlang
-spec services() -> [{inets_service(), pid()}] | {error, inets_not_started}.
```

Returns a list of currently running services.

> #### Note {: .info }
>
> Services started as `stand_alone` are not listed.

# `services_info`

```erlang
-spec services_info() -> [service_info()] | {error, inets_not_started}.
```

Returns a list of currently running services where each service is described by
an `[{Option, Value}]` list. The information in the list is specific for each
service and each service has probably its own info function that gives more
details about the service. If specific service info returns `{error, Reason}`,
Info will contain Reason term.

# `start`

```erlang
-spec start() -> ok | {error, Reason} when Reason :: term().
```

# `start`

```erlang
-spec start(Type) -> ok | {error, Reason} when Type :: application:restart_type(), Reason :: term().
```

Starts the `Inets` application. Default type is `temporary`. See also
`m:application`.

# `start`

```erlang
-spec start(Service, ServiceConfig) -> Result
               when
                   Service :: inets_service(),
                   ServiceConfig :: ConfPropList | ConfFile,
                   ConfPropList :: [{Property, Value}],
                   ConfFile :: string(),
                   Property :: term(),
                   Value :: term(),
                   Result :: {ok, pid()} | {error, term()}.
```

# `start`

```erlang
-spec start(Service, ServiceConfig, How) -> Result
               when
                   Service :: inets_service(),
                   ServiceConfig :: ConfPropList | ConfFile,
                   How :: inets | stand_alone,
                   ConfPropList :: [{Property, Value}],
                   ConfFile :: string(),
                   Property :: term(),
                   Value :: term(),
                   Result :: {ok, pid()} | {error, term()}.
```

Dynamically starts an `Inets` service after the `Inets` application has been
started.

> #### Note {: .info }
>
> Dynamically started services are not handled by application takeover and
> failover behavior when `Inets` is run as a distributed application. Nor are
> they automatically restarted when the `Inets` application is restarted. As
> long as the `Inets` application is operational, they are supervised and can be
> soft code upgraded.
>
> A service started as `stand_alone`, that is, the service is not started as
> part of the `Inets` application, lose all OTP application benefits, such as
> soft upgrade. The `stand_alone`\-service is linked to the process that started
> it. Usually some supervision functionality is still in place and in some sense
> the calling process becomes the top supervisor.
>
> #### Warning {: .warning }
> The stand_alone option is considered deprecated.
>

# `stop`

```erlang
-spec stop() -> ok.
```

Stops the `Inets` application. See also `m:application`.

# `stop`

```erlang
-spec stop(Service, Reference) -> ok | {error, Reason}
              when
                  Service :: inets_service() | stand_alone,
                  Reference :: pid() | term(),
                  Reason :: term().
```

Stops a started service of the `Inets` application or takes down a
`stand_alone`\-service gracefully. When option `stand_alone` is used in start,
only the pid is a valid argument to stop.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
