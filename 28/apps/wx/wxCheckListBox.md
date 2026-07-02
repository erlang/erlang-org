# `wxCheckListBox`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxCheckListBox.erl#L58)

A `m:wxCheckListBox` is like a `m:wxListBox`, but allows items to be checked or
unchecked.

When using this class under Windows wxWidgets must be compiled with wxUSE_OWNER_DRAWN set
to 1.

See:
* `m:wxListBox`

* `m:wxChoice`

* `m:wxComboBox`

* `m:wxListCtrl`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxListBox`

* `m:wxControlWithItems`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxCheckListBox](https://docs.wxwidgets.org/3.2/classwx_check_list_box.html)

## Events

Event types emitted from this class:

* [`command_checklistbox_toggled`](`m:wxCommandEvent`)

# `wxCheckListBox`

```elixir
-type wxCheckListBox() :: wx:wx_object().
```

# `check`

```elixir
-spec check(This, Item) -> ok when This :: wxCheckListBox(), Item :: integer().
```

# `check`

```elixir
-spec check(This, Item, [Option]) -> ok
               when This :: wxCheckListBox(), Item :: integer(), Option :: {check, boolean()}.
```

Checks the given item.

Note that calling this method does not result in a `wxEVT_CHECKLISTBOX` event being emitted.

# `destroy`

```elixir
-spec destroy(This :: wxCheckListBox()) -> ok.
```

Destroys the object

# `isChecked`

```elixir
-spec isChecked(This, Item) -> boolean() when This :: wxCheckListBox(), Item :: integer().
```

Returns true if the given item is checked, false otherwise.

# `new`

```elixir
-spec new() -> wxCheckListBox().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id) -> wxCheckListBox() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```elixir
-spec new(Parent, Id, [Option]) -> wxCheckListBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {choices, [unicode:chardata()]} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a list box.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
