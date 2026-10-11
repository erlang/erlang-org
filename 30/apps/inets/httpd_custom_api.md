# `httpd_custom_api`
[🔗](https://github.com/erlang/otp/blob/master/lib/inets/src/http_server/httpd_custom_api.erl#L23)

Behaviour with optional callbacks to customize the inets HTTP server.

The module implementing this behaviour shall be supplied to to the servers
configuration with the option [customize](`m:httpd#prop_customize`)

# `request_header`
*since OTP 17.5.6* *optional* 

```erlang
-callback request_header({Key :: string(), Value :: string()}) ->
                            {true, {Key :: string(), Value :: string()}} | false.
```

Filter and possible alter HTTP request headers before they are processed by the
server.

# `response_default_headers`
*since OTP 18.1.1* *optional* 

```erlang
-callback response_default_headers() -> [{Key :: string(), Value :: string()}].
```

Provide default headers for the HTTP servers responses. Note that this option
may override built-in defaults.

# `response_header`
*since OTP 17.5.6* *optional* 

```erlang
-callback response_header({Key :: string(), Value :: string()}) ->
                             {true, {Key :: string(), Value :: string()}} | false | {true, string()}.
```

Filter and possible alter HTTP response headers before they are sent to the
client.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
