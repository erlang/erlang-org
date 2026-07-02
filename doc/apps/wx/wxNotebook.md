# `wxNotebook`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxNotebook.erl#L58)

This class represents a notebook control, which manages multiple windows with associated
tabs.

To use the class, create a `m:wxNotebook` object and call `wxBookCtrlBase:addPage/4` or `wxBookCtrlBase:insertPage/5`, passing a window to be
used as the page. Do not explicitly delete the window for a page that is currently managed
by `m:wxNotebook`.

`wxNotebookPage` is a typedef for `m:wxWindow`.

## Styles

This class supports the following styles:

* wxNB_TOP: Place tabs on the top side.

* wxNB_LEFT: Place tabs on the left side.

* wxNB_RIGHT: Place tabs on the right side.

* wxNB_BOTTOM: Place tabs under instead of above the notebook pages.

* wxNB_FIXEDWIDTH: (Windows only) All tabs will have same width.

* wxNB_MULTILINE: (Windows only) There can be several rows of tabs.

* wxNB_NOPAGETHEME: (Windows only) Display a solid colour on notebook pages, and not a
gradient, which can reduce performance. The styles wxNB_LEFT, RIGHT and BOTTOM are not
supported under Microsoft Windows when using visual themes.

Page backgrounds

On Windows, the default theme paints a background on the notebook's pages. If you wish to
suppress this theme, for aesthetic or performance reasons, there are three ways of doing
it. You can use `wxNB_NOPAGETHEME` to disable themed drawing for a particular notebook,
you can call `wxSystemOptions:setOption/2` to disable it for the whole application, or you can disable it for
individual pages by using `wxWindow:setBackgroundColour/2`.

To disable themed pages globally:

Set the value to 1 to enable it again. To give a single page a solid background that more
or less fits in with the overall theme, use:

On platforms other than Windows, or if the application is not using Windows themes, `getThemeBackgroundColour/1` will
return an uninitialised colour object, and the above code will therefore work on all platforms.

See:
* ?wxBookCtrl

* `m:wxBookCtrlEvent`

* `m:wxImageList`

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_notebook)

This class is derived, and can use functions, from:

* `m:wxBookCtrlBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxNotebook](https://docs.wxwidgets.org/3.2/classwx_notebook.html)

## Events

Event types emitted from this class:

* [`command_notebook_page_changed`](`m:wxBookCtrlEvent`)

* [`command_notebook_page_changing`](`m:wxBookCtrlEvent`)

# `wxNotebook`

```erlang
-type wxNotebook() :: wx:wx_object().
```

# `assignImageList`

```erlang
-spec assignImageList(This, ImageList) -> ok
                         when This :: wxNotebook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list for the page control and takes ownership of the list.

See:
* `m:wxImageList`

* `setImageList/2`

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxNotebook(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxNotebook(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates a notebook control.

See `new/3` for a description of the parameters.

# `destroy`

```erlang
-spec destroy(This :: wxNotebook()) -> ok.
```

Destroys the object

# `getImageList`

```erlang
-spec getImageList(This) -> wxImageList:wxImageList() when This :: wxNotebook().
```

Returns the associated image list, may be NULL.

See:
* `m:wxImageList`

* `setImageList/2`

# `getPageImage`

```erlang
-spec getPageImage(This, NPage) -> integer() when This :: wxNotebook(), NPage :: integer().
```

Returns the image index for the given page.

# `getRowCount`

```erlang
-spec getRowCount(This) -> integer() when This :: wxNotebook().
```

Returns the number of rows in the notebook control.

# `getThemeBackgroundColour`

```erlang
-spec getThemeBackgroundColour(This) -> wx:wx_colour4() when This :: wxNotebook().
```

If running under Windows and themes are enabled for the application, this function
returns a suitable colour for painting the background of a notebook page, and can be
passed to `wxWindow:setBackgroundColour/2`.

Otherwise, an uninitialised colour will be returned.

# `new`

```erlang
-spec new() -> wxNotebook().
```

Constructs a notebook control.

# `new`

```erlang
-spec new(Parent, Id) -> wxNotebook() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxNotebook()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructs a notebook control.

Note that sometimes you can reduce flicker by passing the wxCLIP_CHILDREN window style.

# `setImageList`

```erlang
-spec setImageList(This, ImageList) -> ok
                      when This :: wxNotebook(), ImageList :: wxImageList:wxImageList().
```

Sets the image list to use.

It does not take ownership of the image list, you must delete it yourself.

See:
* `m:wxImageList`

* `assignImageList/2`

# `setPadding`

```erlang
-spec setPadding(This, Padding) -> ok
                    when This :: wxNotebook(), Padding :: {W :: integer(), H :: integer()}.
```

Sets the amount of space around each page's icon and label, in pixels.

Note: The vertical padding cannot be changed in wxGTK.

# `setPageImage`

```erlang
-spec setPageImage(This, Page, Image) -> boolean()
                      when This :: wxNotebook(), Page :: integer(), Image :: integer().
```

Sets the image index for the given page.

`image` is an index into the image list which was set with `setImageList/2`.

# `setPageSize`

```erlang
-spec setPageSize(This, Size) -> ok when This :: wxNotebook(), Size :: {W :: integer(), H :: integer()}.
```

Sets the width and height of the pages.

Note: This method is currently not implemented for wxGTK.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
