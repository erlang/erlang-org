# `data_publisher`
[🔗](https://github.com/erlang/otp/blob/master/lib/kernel/src/data_publisher.erl#L27)

A behavior for building eventually consistent, replicated data stores
across distributed nodes.

This module provides the infrastructure for:
- Replicating data across all nodes running the same scope
- Automatic peer discovery
- Subscription-based notifications for data changes
- Version translation for rolling upgrades

Implementing modules define how data is stored, updated, and how changes
are calculated and propagated. This is a generalization of the `pg` module.

# `global_view`
*not exported* *since OTP 29.0* 

```erlang
-type global_view() :: dynamic().
```

# `local_data`
*not exported* *since OTP 29.0* 

```erlang
-type local_data() :: dynamic().
```

# `options`
*not exported* *since OTP 29.0* 

```erlang
-type options() :: #{standalone => boolean(), dynamic() => dynamic()}.
```

# `scope`
*not exported* *since OTP 29.0* 

```erlang
-type scope() :: atom().
```

# `state`
*not exported* *since OTP 29.0* 

```erlang
-type state() ::
          #state{scope :: atom(),
                 module :: module(),
                 options :: options(),
                 version :: version(),
                 global_view :: global_view(),
                 local_data :: local_data(),
                 peers :: #{pid() => {reference(), version(), local_data()}},
                 subscriptions :: subscriptions(),
                 subscribe_refs :: #{reference() => subscription()}}.
```

# `subscribe_result`
*not exported* *since OTP 29.0* 

```erlang
-type subscribe_result() :: dynamic().
```

# `subscription`
*not exported* *since OTP 29.0* 

```erlang
-type subscription() :: dynamic().
```

# `subscriptions`
*not exported* *since OTP 29.0* 

```erlang
-type subscriptions() :: #{subscription() => #{reference() => pid()}}.
```

# `update`
*not exported* *since OTP 29.0* 

```erlang
-type update() :: dynamic().
```

# `version`
*not exported* *since OTP 29.0* 

```erlang
-type version() :: dynamic().
```

# `data_diff`
*since OTP 29.0* 

```erlang
-callback data_diff(Old :: local_data(), New :: local_data()) -> update().
```

# `init_global_view`
*since OTP 29.0* 

```erlang
-callback init_global_view(scope(), options()) -> global_view().
```

# `init_local_data`
*since OTP 29.0* 

```erlang
-callback init_local_data(options()) -> local_data().
```

# `new_subscription`
*since OTP 29.0* 

```erlang
-callback new_subscription(subscription(), global_view()) -> subscribe_result().
```

# `stop_global_view`
*since OTP 29.0* 

```erlang
-callback stop_global_view(global_view()) -> term().
```

# `translate_local_data`
*since OTP 29.0* 

```erlang
-callback translate_local_data(MyVersion :: version(), PeerVersion :: version(), local_data()) ->
                                  local_data() | {'$plain_message', dynamic()}.
```

# `translate_message`
*since OTP 29.0* *optional* 

```erlang
-callback translate_message(dynamic(), global_view()) ->
                               {update, update(), global_view()} |
                               {drop, global_view()} |
                               {update, pid(), update(), global_view()} |
                               {local_data, pid(), version(), local_data(), global_view()} |
                               {discover, pid(), version(), global_view()}.
```

# `translate_update`
*since OTP 29.0* 

```erlang
-callback translate_update(MyVersion :: version(), PeerVersion :: version(), update()) ->
                              update() | {'$plain_messages', [dynamic()]}.
```

# `update_global_view_and_notify`
*since OTP 29.0* 

```erlang
-callback update_global_view_and_notify(node(), update(), subscriptions(), global_view()) -> global_view().
```

# `update_local_data`
*since OTP 29.0* 

```erlang
-callback update_local_data(update(), local_data()) -> local_data().
```

# `version`
*since OTP 29.0* 

```erlang
-callback version() -> version().
```

# `handle_call`
*since OTP 29.0* 

```erlang
-spec handle_call({update, update()}, gen_server:from(), state()) -> {reply, ok, state()};
                 ({subscribe, subscription()}, gen_server:from(), state()) ->
                     {reply, {reference(), subscribe_result()}, state()};
                 ({unsubscribe, reference()}, gen_server:from(), state()) -> {reply, ok, state()}.
```

# `handle_cast`
*since OTP 29.0* 

```erlang
-spec handle_cast(Message, state()) -> {noreply, state()}
                     when Message :: {local_data, pid(), version(), local_data()} | dynamic().
```

# `handle_info`
*since OTP 29.0* 

```erlang
-spec handle_info(Message, state()) -> {noreply, state()}
                     when
                         Message ::
                             {discover, pid(), version()} |
                             {local_data, pid(), version(), local_data()} |
                             {update, pid(), update()} |
                             {nodeup, node()} |
                             {nodedown, node()} |
                             {{'DOWN', subscribe}, reference(), process, pid(), dynamic()} |
                             {{'DOWN', peer}, reference(), process, pid(), dynamic()} |
                             dynamic().
```

# `init`
*since OTP 29.0* 

```erlang
-spec init({scope(), module(), options()}) -> {ok, state()}.
```

# `start`
*since OTP 29.0* 

```erlang
-spec start(scope(), module()) -> {ok, pid()} | {error, term()}.
```

# `start`
*since OTP 29.0* 

```erlang
-spec start(scope(), module(), options()) -> {ok, pid()} | {error, term()}.
```

# `start_link`
*since OTP 29.0* 

```erlang
-spec start_link(scope(), module()) -> {ok, pid()} | {error, term()}.
```

# `start_link`
*since OTP 29.0* 

```erlang
-spec start_link(scope(), module(), options()) -> {ok, pid()} | {error, term()}.
```

# `subscribe`
*since OTP 29.0* 

```erlang
-spec subscribe(scope(), subscription()) -> {reference(), subscribe_result()}.
```

# `terminate`
*since OTP 29.0* 

```erlang
-spec terminate(dynamic(), state()) -> term().
```

# `unsubscribe`
*since OTP 29.0* 

```erlang
-spec unsubscribe(scope(), reference()) -> ok.
```

# `update`
*since OTP 29.0* 

```erlang
-spec update(scope(), update()) -> ok.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
