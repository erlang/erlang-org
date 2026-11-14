# `wxFileDataObject`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFileDataObject.erl#L58)

`m:wxFileDataObject` is a specialization of `m:wxDataObject` for file names.

The program works with it just as if it were a list of absolute file names, but
internally it uses the same format as Explorer and other compatible programs under Windows
or GNOME/KDE file manager under Unix which makes it possible to receive files from them
using this class.

See:
* `m:wxDataObject`

* `m:wxTextDataObject`

* `m:wxBitmapDataObject`

* `m:wxDataObject`

This class is derived, and can use functions, from:

* `m:wxDataObject`

wxWidgets docs: [wxFileDataObject](https://docs.wxwidgets.org/3.2/classwx_file_data_object.html)

# `wxFileDataObject`

```erlang
-type wxFileDataObject() :: wx:wx_object().
```

# `addFile`

```erlang
-spec addFile(This, File) -> ok when This :: wxFileDataObject(), File :: unicode:chardata().
```

Adds a file to the file list represented by this data object (Windows only).

# `destroy`

```erlang
-spec destroy(This :: wxFileDataObject()) -> ok.
```

Destroys the object

# `getFilenames`

```erlang
-spec getFilenames(This) -> [unicode:charlist()] when This :: wxFileDataObject().
```

Returns the array of file names.

# `new`

```erlang
-spec new() -> wxFileDataObject().
```

Constructor.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
