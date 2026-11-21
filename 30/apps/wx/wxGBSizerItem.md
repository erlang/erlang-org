# `wxGBSizerItem`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGBSizerItem.erl#L58)

The `m:wxGBSizerItem` class is used by the `m:wxGridBagSizer` for tracking the items in
the sizer.

It adds grid position and spanning information to the normal `m:wxSizerItem` by adding `wxGBPosition`
(not implemented in wx) and `wxGBSpan` (not implemented in wx) attributes. Most of the
time you will not need to use a `m:wxGBSizerItem` directly in your code, but there are a
couple of cases where it is handy.

This class is derived, and can use functions, from:

* `m:wxSizerItem`

wxWidgets docs: [wxGBSizerItem](https://docs.wxwidgets.org/3.2/classwx_g_b_sizer_item.html)

# `wxGBSizerItem`

```erlang
-type wxGBSizerItem() :: wx:wx_object().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
