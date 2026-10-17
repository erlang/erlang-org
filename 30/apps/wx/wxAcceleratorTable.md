# `wxAcceleratorTable`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxAcceleratorTable.erl#L58)

An accelerator table allows the application to specify a table of keyboard shortcuts for
menu or button commands.

The object ?wxNullAcceleratorTable is defined to be a table with no data, and is the
initial accelerator table for a window.

Example:

Remark: An accelerator takes precedence over normal processing and can be a convenient
way to program some event handling. For example, you can use an accelerator table to
enable a dialog with a multi-line text control to accept CTRL-Enter as meaning 'OK'.

Predefined objects (include wx.hrl): ?wxNullAcceleratorTable

See:
* `m:wxAcceleratorEntry`

* `wxWindow:setAcceleratorTable/2`

wxWidgets docs: [wxAcceleratorTable](https://docs.wxwidgets.org/3.2/classwx_accelerator_table.html)

# `wxAcceleratorTable`

```erlang
-type wxAcceleratorTable() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxAcceleratorTable()) -> ok.
```

Destroys the object

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxAcceleratorTable().
```

Returns true if the accelerator table is valid.

# `new`

```erlang
-spec new() -> wxAcceleratorTable().
```

Default ctor.

# `new`

```erlang
-spec new(N, Entries) -> wxAcceleratorTable()
             when N :: integer(), Entries :: [wxAcceleratorEntry:wxAcceleratorEntry()].
```

Initializes the accelerator table from an array of `m:wxAcceleratorEntry`.

# `ok`

```erlang
-spec ok(This) -> boolean() when This :: wxAcceleratorTable().
```

Equivalent to: `isOk/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
