# `wxLogNull`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxLogNull.erl#L58)

This class allows you to temporarily suspend logging.

All calls to the log functions during the life time of an object of this class are just ignored.

In particular, it can be used to suppress the log messages given by wxWidgets itself but
it should be noted that it is rarely the best way to cope with this problem as `all` log
messages are suppressed, even if they indicate a completely different error than the one
the programmer wanted to suppress.

For instance, the example of the overview:

would be better written as:

wxWidgets docs: [wxLogNull](https://docs.wxwidgets.org/3.2/classwx_log_null.html)

# `wxLogNull`

```erlang
-type wxLogNull() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxLogNull()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxLogNull().
```

Suspends logging.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
