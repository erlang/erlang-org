# `wxImage`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxImage.erl#L62)

This class encapsulates a platform-independent image.

An image can be created from data, or using `wxBitmap:convertToImage/1`. An image can be loaded from a file in a
variety of formats, and is extensible to new formats via image format handlers. Functions
are available to set and get image bits, so it can be used for basic image manipulation.

A `m:wxImage` cannot (currently) be drawn directly to a `m:wxDC`. Instead, a
platform-specific `m:wxBitmap` object must be created from it using the
wxBitmap::wxBitmap(wxImage,int depth) constructor. This bitmap can then be drawn in a
device context, using `wxDC:drawBitmap/4`.

More on the difference between `m:wxImage` and `m:wxBitmap`: `m:wxImage` is just a buffer
of RGB bytes with an optional buffer for the alpha bytes. It is all generic, platform
independent and image file format independent code. It includes generic code for scaling,
resizing, clipping, and other manipulations of the image data. OTOH, `m:wxBitmap` is
intended to be a wrapper of whatever is the native image format that is quickest/easiest
to draw to a DC or to be the target of the drawing operations performed on a `m:wxMemoryDC`.
By splitting the responsibilities between wxImage/wxBitmap like this then it's easier to
use generic code shared by all platforms and image types for generic operations and
platform specific code where performance or compatibility is needed.

One colour value of the image may be used as a mask colour which will lead to the
automatic creation of a `m:wxMask` object associated to the bitmap object.

Alpha channel support

Starting from wxWidgets 2.5.0 `m:wxImage` supports alpha channel data, that is in
addition to a byte for the red, green and blue colour components for each pixel it also
stores a byte representing the pixel opacity.

An alpha value of 0 corresponds to a transparent pixel (null opacity) while a value of
255 means that the pixel is 100% opaque. The constants ?wxIMAGE\_ALPHA\_TRANSPARENT and
?wxIMAGE\_ALPHA\_OPAQUE can be used to indicate those values in a more readable form.

While all images have RGB data, not all images have an alpha channel. Before using `getAlpha/3` you
should check if this image contains an alpha channel with `hasAlpha/1`. Currently the BMP, PNG, TGA,
and TIFF format handlers have full alpha channel support for loading so if you want to use
alpha you have to use one of these formats. If you initialize the image alpha channel
yourself using `setAlpha/4`, you should save it in either PNG, TGA, or TIFF format to avoid losing it
as these are the only handlers that currently support saving with alpha.

Available image handlers

The following image handlers are available. wxBMPHandler is always installed by default.
To use other image formats, install the appropriate handler with `wxImage::AddHandler`
(not implemented in wx) or call ?wxInitAllImageHandlers().

* wxBMPHandler: For loading (including alpha support) and saving, always installed.

* `wxPNGHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxJPEGHandler` (not implemented in wx): For loading and saving.

* `wxGIFHandler` (not implemented in wx): For loading and saving (see below).

* `wxPCXHandler` (not implemented in wx): For loading and saving (see below).

* `wxPNMHandler` (not implemented in wx): For loading and saving (see below).

* `wxTIFFHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxTGAHandler` (not implemented in wx): For loading and saving. Includes alpha support.

* `wxIFFHandler` (not implemented in wx): For loading only.

* `wxXPMHandler` (not implemented in wx): For loading and saving.

* wxICOHandler: For loading and saving.

* wxCURHandler: For loading and saving.

* wxANIHandler: For loading only.

When saving in PCX format, `wxPCXHandler` (not implemented in wx) will count the number
of different colours in the image; if there are 256 or less colours, it will save as 8
bit, else it will save as 24 bit.

Loading PNMs only works for ASCII or raw RGB images. When saving in PNM format, `wxPNMHandler`
(not implemented in wx) will always save as raw RGB.

Saving GIFs requires images of maximum 8 bpp (see `wxQuantize` (not implemented in wx)),
and the alpha channel converted to a mask (see `convertAlphaToMask/5`). Saving an animated GIF requires images of
the same size (see `wxGIFHandler::SaveAnimation` (not implemented in wx))

Predefined objects (include wx.hrl): ?wxNullImage

See:
* `m:wxBitmap`

* ?wxInitAllImageHandlers()

wxWidgets docs: [wxImage](https://docs.wxwidgets.org/3.2/classwx_image.html)

# `wxImage`

```elixir
-type wxImage() :: wx:wx_object().
```

# `blur`

```elixir
-spec blur(This, BlurRadius) -> wxImage() when This :: wxImage(), BlurRadius :: integer().
```

Blurs the image in both horizontal and vertical directions by the specified pixel `blurRadius`.

This should not be used when using a single mask colour for transparency.

See:
* `blurHorizontal/2`

* `blurVertical/2`

# `blurHorizontal`

```elixir
-spec blurHorizontal(This, BlurRadius) -> wxImage() when This :: wxImage(), BlurRadius :: integer().
```

Blurs the image in the horizontal direction only.

This should not be used when using a single mask colour for transparency.

See:
* `blur/2`

* `blurVertical/2`

# `blurVertical`

```elixir
-spec blurVertical(This, BlurRadius) -> wxImage() when This :: wxImage(), BlurRadius :: integer().
```

Blurs the image in the vertical direction only.

This should not be used when using a single mask colour for transparency.

See:
* `blur/2`

* `blurHorizontal/2`

# `convertAlphaToMask`

```elixir
-spec convertAlphaToMask(This) -> boolean() when This :: wxImage().
```

# `convertAlphaToMask`

```elixir
-spec convertAlphaToMask(This, [Option]) -> boolean()
                            when This :: wxImage(), Option :: {threshold, integer()}.
```

If the image has alpha channel, this method converts it to mask.

If the image has an alpha channel, all pixels with alpha value less than `threshold` are
replaced with the mask colour and the alpha channel is removed. Otherwise nothing is done.

The mask colour is chosen automatically using `findFirstUnusedColour/2`, see the overload below if this is not appropriate.

Return: Returns true on success, false on error.

# `convertAlphaToMask`

```elixir
-spec convertAlphaToMask(This, Mr, Mg, Mb) -> boolean()
                            when This :: wxImage(), Mr :: integer(), Mg :: integer(), Mb :: integer().
```

# `convertAlphaToMask`

```elixir
-spec convertAlphaToMask(This, Mr, Mg, Mb, [Option]) -> boolean()
                            when
                                This :: wxImage(),
                                Mr :: integer(),
                                Mg :: integer(),
                                Mb :: integer(),
                                Option :: {threshold, integer()}.
```

If the image has alpha channel, this method converts it to mask using the specified
colour as the mask colour.

If the image has an alpha channel, all pixels with alpha value less than `threshold` are
replaced with the mask colour and the alpha channel is removed. Otherwise nothing is done.

Since: 2.9.0

Return: Returns true on success, false on error.

# `convertToGreyscale`

```elixir
-spec convertToGreyscale(This) -> wxImage() when This :: wxImage().
```

Returns a greyscale version of the image.

Since: 2.9.0

# `convertToGreyscale`

```elixir
-spec convertToGreyscale(This, Weight_r, Weight_g, Weight_b) -> wxImage()
                            when
                                This :: wxImage(),
                                Weight_r :: number(),
                                Weight_g :: number(),
                                Weight_b :: number().
```

Returns a greyscale version of the image.

The returned image uses the luminance component of the original to calculate the
greyscale. Defaults to using the standard ITU-T BT.601 when converting to YUV, where every
pixel equals (R * `weight_r`) + (G * `weight_g`) + (B * `weight_b`).

# `convertToMono`

```elixir
-spec convertToMono(This, R, G, B) -> wxImage()
                       when This :: wxImage(), R :: integer(), G :: integer(), B :: integer().
```

Returns monochromatic version of the image.

The returned image has white colour where the original has (r,g,b) colour and black
colour everywhere else.

# `copy`

```elixir
-spec copy(This) -> wxImage() when This :: wxImage().
```

Returns an identical copy of this image.

# `create`

```elixir
-spec create(This, Sz) -> boolean() when This :: wxImage(), Sz :: {W :: integer(), H :: integer()}.
```

# `create`

```elixir
-spec create(This, Width, Height) -> boolean()
                when This :: wxImage(), Width :: integer(), Height :: integer();
            (This, Sz, Data) -> boolean()
                when This :: wxImage(), Sz :: {W :: integer(), H :: integer()}, Data :: binary();
            (This, Sz, [Option]) -> boolean()
                when
                    This :: wxImage(),
                    Sz :: {W :: integer(), H :: integer()},
                    Option :: {clear, boolean()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `create`

```elixir
-spec create(This, Width, Height, Data) -> boolean()
                when This :: wxImage(), Width :: integer(), Height :: integer(), Data :: binary();
            (This, Width, Height, [Option]) -> boolean()
                when
                    This :: wxImage(),
                    Width :: integer(),
                    Height :: integer(),
                    Option :: {clear, boolean()};
            (This, Sz, Data, Alpha) -> boolean()
                when
                    This :: wxImage(),
                    Sz :: {W :: integer(), H :: integer()},
                    Data :: binary(),
                    Alpha :: binary().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `create`

```elixir
-spec create(This, Width, Height, Data, Alpha) -> boolean()
                when
                    This :: wxImage(),
                    Width :: integer(),
                    Height :: integer(),
                    Data :: binary(),
                    Alpha :: binary().
```

Creates a fresh image.

See `new/4` for more info.

Return: true if the call succeeded, false otherwise.

# `Destroy`

```elixir
-spec 'Destroy'(This) -> ok when This :: wxImage().
```

Destroys the image data.

# `destroy`

```elixir
-spec destroy(This :: wxImage()) -> ok.
```

Destroys the object

# `findFirstUnusedColour`

```elixir
-spec findFirstUnusedColour(This) -> Result
                               when
                                   Result ::
                                       {Res :: boolean(),
                                        R :: integer(),
                                        G :: integer(),
                                        B :: integer()},
                                   This :: wxImage().
```

# `findFirstUnusedColour`

```elixir
-spec findFirstUnusedColour(This, [Option]) -> Result
                               when
                                   Result ::
                                       {Res :: boolean(),
                                        R :: integer(),
                                        G :: integer(),
                                        B :: integer()},
                                   This :: wxImage(),
                                   Option ::
                                       {startR, integer()} | {startG, integer()} | {startB, integer()}.
```

Finds the first colour that is never used in the image.

The search begins at given initial colour and continues by increasing R, G and B
components (in this order) by 1 until an unused colour is found or the colour space exhausted.

The parameters `r`, `g`, `b` are pointers to variables to save the colour.

The parameters `startR`, `startG`, `startB` define the initial values of the colour. The
returned colour will have RGB values equal to or greater than these.

Return: Returns false if there is no unused colour left, true on success.

Note: This method involves computing the histogram, which is a computationally intensive
operation.

# `getAlpha`

```elixir
-spec getAlpha(This) -> binary() when This :: wxImage().
```

Returns pointer to the array storing the alpha values for this image.

This pointer is NULL for the images without the alpha channel. If the image does have it,
this pointer may be used to directly manipulate the alpha values which are stored as the
RGB ones.

# `getAlpha`

```elixir
-spec getAlpha(This, X, Y) -> integer() when This :: wxImage(), X :: integer(), Y :: integer().
```

Return alpha value at given pixel location.

# `getBlue`

```elixir
-spec getBlue(This, X, Y) -> integer() when This :: wxImage(), X :: integer(), Y :: integer().
```

Returns the blue intensity at the given coordinate.

# `getData`

```elixir
-spec getData(This) -> binary() when This :: wxImage().
```

Returns the image data as an array.

This is most often used when doing direct image manipulation. The return value points to
an array of characters in RGBRGBRGB... format in the top-to-bottom, left-to-right order,
that is the first RGB triplet corresponds to the first pixel of the first row, the second
one - to the second pixel of the first row and so on until the end of the first row, with
second row following after it and so on.

You should not delete the returned pointer nor pass it to `setData/4`.

# `getGreen`

```elixir
-spec getGreen(This, X, Y) -> integer() when This :: wxImage(), X :: integer(), Y :: integer().
```

Returns the green intensity at the given coordinate.

# `getHeight`

```elixir
-spec getHeight(This) -> integer() when This :: wxImage().
```

Gets the height of the image in pixels.

See: `getWidth/1`

# `getImageCount`

```elixir
-spec getImageCount(Filename) -> integer() when Filename :: unicode:chardata().
```

# `getImageCount`

```elixir
-spec getImageCount(Filename, [Option]) -> integer()
                       when Filename :: unicode:chardata(), Option :: {type, wx:wx_enum()}.
```

If the image file contains more than one image and the image handler is capable of
retrieving these individually, this function will return the number of available images.

For the overload taking the parameter `filename`, that's the name of the file to query.
For the overload taking the parameter `stream`, that's the opened input stream with image data.

See `wxImageHandler::GetImageCount()` (not implemented in wx) for more info.

The parameter `type` may be one of the following values:

* wxBITMAP_TYPE_BMP: Load a Windows bitmap file.

* wxBITMAP_TYPE_GIF: Load a GIF bitmap file.

* wxBITMAP_TYPE_JPEG: Load a JPEG bitmap file.

* wxBITMAP_TYPE_PNG: Load a PNG bitmap file.

* wxBITMAP_TYPE_PCX: Load a PCX bitmap file.

* wxBITMAP_TYPE_PNM: Load a PNM bitmap file.

* wxBITMAP_TYPE_TIFF: Load a TIFF bitmap file.

* wxBITMAP_TYPE_TGA: Load a TGA bitmap file.

* wxBITMAP_TYPE_XPM: Load a XPM bitmap file.

* wxBITMAP_TYPE_ICO: Load a Windows icon file (ICO).

* wxBITMAP_TYPE_CUR: Load a Windows cursor file (CUR).

* wxBITMAP_TYPE_ANI: Load a Windows animated cursor file (ANI).

* wxBITMAP_TYPE_ANY: Will try to autodetect the format.

Return: Number of available images. For most image handlers, this is 1 (exceptions are
TIFF and ICO formats as well as animated GIFs for which this function returns the number
of frames in the animation).

# `getImageExtWildcard`

```elixir
-spec getImageExtWildcard() -> unicode:charlist().
```

Iterates all registered `wxImageHandler` (not implemented in wx) objects, and returns a
string containing file extension masks suitable for passing to file open/save dialog
boxes.

Return: The format of the returned string is `"(*.ext1;*.ext2)|*.ext1;*.ext2"`. It is
usually a good idea to prepend a description before passing the result to the dialog.
Example:

# `getMaskBlue`

```elixir
-spec getMaskBlue(This) -> integer() when This :: wxImage().
```

Gets the blue value of the mask colour.

# `getMaskGreen`

```elixir
-spec getMaskGreen(This) -> integer() when This :: wxImage().
```

Gets the green value of the mask colour.

# `getMaskRed`

```elixir
-spec getMaskRed(This) -> integer() when This :: wxImage().
```

Gets the red value of the mask colour.

# `getOption`

```elixir
-spec getOption(This, Name) -> unicode:charlist() when This :: wxImage(), Name :: unicode:chardata().
```

Gets a user-defined string-valued option.

Generic options:

* `wxIMAGE_OPTION_FILENAME:` The name of the file from which the image was loaded.

Options specific to `wxGIFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_GIF_COMMENT:` The comment text that is read from or written to the GIF
file. In an animated GIF each frame can have its own comment. If there is only a comment
in the first frame of a GIF it will not be repeated in other frames.

Return: The value of the option or an empty string if not found. Use `hasOption/2` if an empty string
can be a valid option value.

See:
* `setOption/3`

* `getOptionInt/2`

* `hasOption/2`

# `getOptionInt`

```elixir
-spec getOptionInt(This, Name) -> integer() when This :: wxImage(), Name :: unicode:chardata().
```

Gets a user-defined integer-valued option.

The function is case-insensitive to `name`. If the given option is not present, the
function returns 0. Use `hasOption/2` if 0 is a possibly valid value for the option.

Generic options:

* `wxIMAGE_OPTION_MAX_WIDTH` and `wxIMAGE_OPTION_MAX_HEIGHT:` If either of these options is
specified, the loaded image will be scaled down (preserving its aspect ratio) so that its
width is less than the max width given if it is not 0 `and` its height is less than the
max height given if it is not 0. This is typically used for loading thumbnails and the
advantage of using these options compared to calling `rescale/4` after loading is that some handlers
(only JPEG one right now) support rescaling the image during loading which is vastly more
efficient than loading the entire huge image and rescaling it later (if these options are
not supported by the handler, this is still what happens however). These options must be
set before calling `loadFile/4` to have any effect.

* `wxIMAGE_OPTION_ORIGINAL_WIDTH` and `wxIMAGE_OPTION_ORIGINAL_HEIGHT:` These options will
return the original size of the image if either `wxIMAGE_OPTION_MAX_WIDTH` or `wxIMAGE_OPTION_MAX_HEIGHT`
is specified.

Since: 2.9.3

* `wxIMAGE_OPTION_QUALITY:` JPEG quality used when saving. This is an integer in 0..100
range with 0 meaning very poor and 100 excellent (but very badly compressed). This option
is currently ignored for the other formats.

* `wxIMAGE_OPTION_RESOLUTIONUNIT:` The value of this option determines whether the
resolution of the image is specified in centimetres or inches, see wxImageResolution enum elements.

* `wxIMAGE_OPTION_RESOLUTION`, `wxIMAGE_OPTION_RESOLUTIONX` and `wxIMAGE_OPTION_RESOLUTIONY:`
These options define the resolution of the image in the units corresponding to `wxIMAGE_OPTION_RESOLUTIONUNIT`
options value. The first option can be set before saving the image to set both horizontal
and vertical resolution to the same value. The X and Y options are set by the image
handlers if they support the image resolution (currently BMP, JPEG and TIFF handlers do)
and the image provides the resolution information and can be queried after loading the image.

Options specific to `wxPNGHandler` (not implemented in wx):

* `wxIMAGE_OPTION_PNG_FORMAT:` Format for saving a PNG file, see wxImagePNGType for the
supported values.

* `wxIMAGE_OPTION_PNG_BITDEPTH:` Bit depth for every channel (R/G/B/A).

* `wxIMAGE_OPTION_PNG_FILTER:` Filter for saving a PNG file, see libpng ([http://www.libpng.org/pub/png/libpng-1.2.5-manual.html](http://www.libpng.org/pub/png/libpng-1.2.5-manual.html))
for possible values (e.g. PNG_FILTER_NONE, PNG_FILTER_SUB, PNG_FILTER_UP, etc).

* `wxIMAGE_OPTION_PNG_COMPRESSION_LEVEL:` Compression level (0..9) for saving a PNG file.
An high value creates smaller-but-slower PNG file. Note that unlike other formats (e.g.
JPEG) the PNG format is always lossless and thus this compression level doesn't tradeoff
the image quality.

* `wxIMAGE_OPTION_PNG_COMPRESSION_MEM_LEVEL:` Compression memory usage level (1..9) for
saving a PNG file. An high value means the saving process consumes more memory, but may
create smaller PNG file.

* `wxIMAGE_OPTION_PNG_COMPRESSION_STRATEGY:` Possible values are 0 for default strategy, 1
for filter, and 2 for Huffman-only. You can use OptiPNG ([http://optipng.sourceforge.net/](http://optipng.sourceforge.net/))
to get a suitable value for your application.

* `wxIMAGE_OPTION_PNG_COMPRESSION_BUFFER_SIZE:` Internal buffer size (in bytes) for saving
a PNG file. Ideally this should be as big as the resulting PNG file. Use this option if
your application produces images with small size variation.

Options specific to `wxTIFFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE:` Number of bits per sample (channel). Currently
values of 1 and 8 are supported. A value of 1 results in a black and white image. A value
of 8 (the default) can mean greyscale or RGB, depending on the value of `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL`.

* `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL:` Number of samples (channels) per pixel. Currently
values of 1 and 3 are supported. A value of 1 results in either a greyscale (by default)
or black and white image, depending on the value of `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE`. A
value of 3 (the default) will result in an RGB image.

* `wxIMAGE_OPTION_TIFF_COMPRESSION:` Compression type. By default it is set to 1
(COMPRESSION_NONE). Typical other values are 5 (COMPRESSION_LZW) and 7 (COMPRESSION_JPEG).
See tiff.h for more options.

* `wxIMAGE_OPTION_TIFF_PHOTOMETRIC:` Specifies the photometric interpretation. By default
it is set to 2 (PHOTOMETRIC_RGB) for RGB images and 0 (PHOTOMETRIC_MINISWHITE) for
greyscale or black and white images. It can also be set to 1 (PHOTOMETRIC_MINISBLACK) to
treat the lowest value as black and highest as white. If you want a greyscale image it is
also sufficient to only specify `wxIMAGE_OPTION_TIFF_PHOTOMETRIC` and set it to either
PHOTOMETRIC_MINISWHITE or PHOTOMETRIC_MINISBLACK. The other values are taken care of.

Options specific to `wxGIFHandler` (not implemented in wx):

* `wxIMAGE_OPTION_GIF_TRANSPARENCY:` How to deal with transparent pixels. By default, the
color of transparent pixels is changed to bright pink, so that if the image is
accidentally drawn without transparency, it will be obvious. Normally, this would not be
noticed, as these pixels will not be rendered. But in some cases it might be useful to
load a GIF without making any modifications to its colours. Use `wxIMAGE_OPTION_GIF_TRANSPARENCY_UNCHANGED`
to keep the colors correct. Use `wxIMAGE_OPTION_GIF_TRANSPARENCY_HIGHLIGHT` to convert
transparent pixels to pink (default). This option has been added in wxWidgets 3.1.1.

Note: Be careful when combining the options `wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL`, `wxIMAGE_OPTION_TIFF_BITSPERSAMPLE`,
and `wxIMAGE_OPTION_TIFF_PHOTOMETRIC`. While some measures are taken to prevent illegal
combinations and/or values, it is still easy to abuse them and come up with invalid
results in the form of either corrupted images or crashes.

Return: The value of the option or 0 if not found. Use `hasOption/2` if 0 can be a valid option value.

See:
* `setOption/3`

* `getOption/2`

# `getOrFindMaskColour`

```elixir
-spec getOrFindMaskColour(This) -> Result
                             when
                                 Result ::
                                     {Res :: boolean(), R :: integer(), G :: integer(), B :: integer()},
                                 This :: wxImage().
```

Get the current mask colour or find a suitable unused colour that could be used as a mask
colour.

Returns true if the image currently has a mask.

# `getPalette`

```elixir
-spec getPalette(This) -> wxPalette:wxPalette() when This :: wxImage().
```

Returns the palette associated with the image.

Currently the palette is only used when converting to `m:wxBitmap` under Windows.

Some of the `m:wxImage` handlers have been modified to set the palette if one exists in
the image file (usually 256 or less colour images in GIF or PNG format).

# `getRed`

```elixir
-spec getRed(This, X, Y) -> integer() when This :: wxImage(), X :: integer(), Y :: integer().
```

Returns the red intensity at the given coordinate.

# `getSubImage`

```elixir
-spec getSubImage(This, Rect) -> wxImage()
                     when
                         This :: wxImage(),
                         Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

Returns a sub image of the current one as long as the rect belongs entirely to the image.

# `getWidth`

```elixir
-spec getWidth(This) -> integer() when This :: wxImage().
```

Gets the width of the image in pixels.

See: `getHeight/1`

# `hasAlpha`

```elixir
-spec hasAlpha(This) -> boolean() when This :: wxImage().
```

Returns true if this image has alpha channel, false otherwise.

See:
* `getAlpha/3`

* `setAlpha/4`

# `hasMask`

```elixir
-spec hasMask(This) -> boolean() when This :: wxImage().
```

Returns true if there is a mask active, false otherwise.

# `hasOption`

```elixir
-spec hasOption(This, Name) -> boolean() when This :: wxImage(), Name :: unicode:chardata().
```

Returns true if the given option is present.

The function is case-insensitive to `name`.

The lists of the currently supported options are in `getOption/2` and `getOptionInt/2` function docs.

See:
* `setOption/3`

* `getOption/2`

* `getOptionInt/2`

# `initAlpha`

```elixir
-spec initAlpha(This) -> ok when This :: wxImage().
```

Initializes the image alpha channel data.

It is an error to call it if the image already has alpha data. If it doesn't, alpha data
will be by default initialized to all pixels being fully opaque. But if the image has a
mask colour, all mask pixels will be completely transparent.

# `initStandardHandlers`

```elixir
-spec initStandardHandlers() -> ok.
```

Internal use only.

Adds standard image format handlers. It only install wxBMPHandler for the time being,
which is used by `m:wxBitmap`.

This function is called by wxWidgets on startup, and shouldn't be called by the user.

See: ?wxInitAllImageHandlers()

# `isOk`

```elixir
-spec isOk(This) -> boolean() when This :: wxImage().
```

Returns true if image data is present.

# `isTransparent`

```elixir
-spec isTransparent(This, X, Y) -> boolean() when This :: wxImage(), X :: integer(), Y :: integer().
```

# `isTransparent`

```elixir
-spec isTransparent(This, X, Y, [Option]) -> boolean()
                       when
                           This :: wxImage(),
                           X :: integer(),
                           Y :: integer(),
                           Option :: {threshold, integer()}.
```

Returns true if the given pixel is transparent, i.e. either has the mask colour if this
image has a mask or if this image has alpha channel and alpha value of this pixel is
strictly less than `threshold`.

# `loadFile`

```elixir
-spec loadFile(This, Name) -> boolean() when This :: wxImage(), Name :: unicode:chardata().
```

# `loadFile`

```elixir
-spec loadFile(This, Name, [Option]) -> boolean()
                  when
                      This :: wxImage(),
                      Name :: unicode:chardata(),
                      Option :: {type, wx:wx_enum()} | {index, integer()}.
```

Loads an image from a file.

If no handler type is provided, the library will try to autodetect the format.

# `loadFile`

```elixir
-spec loadFile(This, Name, Mimetype, [Option]) -> boolean()
                  when
                      This :: wxImage(),
                      Name :: unicode:chardata(),
                      Mimetype :: unicode:chardata(),
                      Option :: {index, integer()}.
```

Loads an image from a file.

If no handler type is provided, the library will try to autodetect the format.

# `mirror`

```elixir
-spec mirror(This) -> wxImage() when This :: wxImage().
```

# `mirror`

```elixir
-spec mirror(This, [Option]) -> wxImage() when This :: wxImage(), Option :: {horizontally, boolean()}.
```

Returns a mirrored copy of the image.

The parameter `horizontally` indicates the orientation.

# `new`

```elixir
-spec new() -> wxImage().
```

Creates an empty `m:wxImage` object without an alpha channel.

# `new`

```elixir
-spec new(Name) -> wxImage() when Name :: unicode:chardata();
         (Sz) -> wxImage() when Sz :: {W :: integer(), H :: integer()}.
```

Equivalent to: `new/2`

# `new`

```elixir
-spec new(Width, Height) -> wxImage() when Width :: integer(), Height :: integer();
         (Name, [Option]) -> wxImage()
             when Name :: unicode:chardata(), Option :: {type, wx:wx_enum()} | {index, integer()};
         (Sz, Data) -> wxImage() when Sz :: {W :: integer(), H :: integer()}, Data :: binary();
         (Sz, [Option]) -> wxImage()
             when Sz :: {W :: integer(), H :: integer()}, Option :: {clear, boolean()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `new`

```elixir
-spec new(Width, Height, Data) -> wxImage()
             when Width :: integer(), Height :: integer(), Data :: binary();
         (Width, Height, [Option]) -> wxImage()
             when Width :: integer(), Height :: integer(), Option :: {clear, boolean()};
         (Name, Mimetype, [Option]) -> wxImage()
             when
                 Name :: unicode:chardata(),
                 Mimetype :: unicode:chardata(),
                 Option :: {index, integer()};
         (Sz, Data, Alpha) -> wxImage()
             when Sz :: {W :: integer(), H :: integer()}, Data :: binary(), Alpha :: binary().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `new`

```elixir
-spec new(Width, Height, Data, Alpha) -> wxImage()
             when Width :: integer(), Height :: integer(), Data :: binary(), Alpha :: binary().
```

Creates an image from data in memory.

If `static_data` is false then the `m:wxImage` will take ownership of the data and free
it afterwards. For this, it has to be allocated with `malloc`.

# `ok`

```elixir
-spec ok(This) -> boolean() when This :: wxImage().
```

Equivalent to: `isOk/1`

# `removeHandler`

```elixir
-spec removeHandler(Name) -> boolean() when Name :: unicode:chardata().
```

Finds the handler with the given name, and removes it.

The handler is also deleted.

Return: true if the handler was found and removed, false otherwise.

# `replace`

```elixir
-spec replace(This, R1, G1, B1, R2, G2, B2) -> ok
                 when
                     This :: wxImage(),
                     R1 :: integer(),
                     G1 :: integer(),
                     B1 :: integer(),
                     R2 :: integer(),
                     G2 :: integer(),
                     B2 :: integer().
```

Replaces the colour specified by `r1`,g1,b1 by the colour `r2`,g2,b2.

# `rescale`

```elixir
-spec rescale(This, Width, Height) -> wxImage()
                 when This :: wxImage(), Width :: integer(), Height :: integer().
```

# `rescale`

```elixir
-spec rescale(This, Width, Height, [Option]) -> wxImage()
                 when
                     This :: wxImage(),
                     Width :: integer(),
                     Height :: integer(),
                     Option :: {quality, wx:wx_enum()}.
```

Changes the size of the image in-place by scaling it: after a call to this function,the
image will have the given width and height.

For a description of the `quality` parameter, see the `scale/4` function. Returns the (modified)
image itself.

See: `scale/4`

# `resize`

```elixir
-spec resize(This, Size, Pos) -> wxImage()
                when
                    This :: wxImage(),
                    Size :: {W :: integer(), H :: integer()},
                    Pos :: {X :: integer(), Y :: integer()}.
```

# `resize`

```elixir
-spec resize(This, Size, Pos, [Option]) -> wxImage()
                when
                    This :: wxImage(),
                    Size :: {W :: integer(), H :: integer()},
                    Pos :: {X :: integer(), Y :: integer()},
                    Option :: {r, integer()} | {g, integer()} | {b, integer()}.
```

Changes the size of the image in-place without scaling it by adding either a border with
the given colour or cropping as necessary.

The image is pasted into a new image with the given `size` and background colour at the
position `pos` relative to the upper left of the new image.

If `red` = green = blue = -1 then use either the current mask colour if set or find, use,
and set a suitable mask colour for any newly exposed areas.

Return: The (modified) image itself.

See: `size/4`

# `rotate90`

```elixir
-spec rotate90(This) -> wxImage() when This :: wxImage().
```

# `rotate90`

```elixir
-spec rotate90(This, [Option]) -> wxImage() when This :: wxImage(), Option :: {clockwise, boolean()}.
```

Returns a copy of the image rotated 90 degrees in the direction indicated by `clockwise`.

# `rotate`

```elixir
-spec rotate(This, Angle, RotationCentre) -> wxImage()
                when
                    This :: wxImage(),
                    Angle :: number(),
                    RotationCentre :: {X :: integer(), Y :: integer()}.
```

# `rotate`

```elixir
-spec rotate(This, Angle, RotationCentre, [Option]) -> wxImage()
                when
                    This :: wxImage(),
                    Angle :: number(),
                    RotationCentre :: {X :: integer(), Y :: integer()},
                    Option ::
                        {interpolating, boolean()} |
                        {offset_after_rotation, {X :: integer(), Y :: integer()}}.
```

Rotates the image about the given point, by `angle` radians.

Passing true to `interpolating` results in better image quality, but is slower.

If the image has a mask, then the mask colour is used for the uncovered pixels in the
rotated image background. Else, black (rgb 0, 0, 0) will be used.

Returns the rotated image, leaving this image intact.

# `rotateHue`

```elixir
-spec rotateHue(This, Angle) -> ok when This :: wxImage(), Angle :: number().
```

Rotates the hue of each pixel in the image by `angle`, which is a double in the range of
-1.0 to +1.0, where -1.0 corresponds to -360 degrees and +1.0 corresponds to +360 degrees.

# `saveFile`

```elixir
-spec saveFile(This, Name) -> boolean() when This :: wxImage(), Name :: unicode:chardata().
```

Saves an image in the named file.

File type is determined from the extension of the file name. Note that this function may
fail if the extension is not recognized! You can use one of the forms above to save images
to files with non-standard extensions.

# `saveFile`

```elixir
-spec saveFile(This, Name, Type) -> boolean()
                  when This :: wxImage(), Name :: unicode:chardata(), Type :: wx:wx_enum();
              (This, Name, Mimetype) -> boolean()
                  when This :: wxImage(), Name :: unicode:chardata(), Mimetype :: unicode:chardata().
```

Saves an image in the named file.

# `scale`

```elixir
-spec scale(This, Width, Height) -> wxImage()
               when This :: wxImage(), Width :: integer(), Height :: integer().
```

# `scale`

```elixir
-spec scale(This, Width, Height, [Option]) -> wxImage()
               when
                   This :: wxImage(),
                   Width :: integer(),
                   Height :: integer(),
                   Option :: {quality, wx:wx_enum()}.
```

Returns a scaled version of the image.

This is also useful for scaling bitmaps in general as the only other way to scale bitmaps
is to blit a `m:wxMemoryDC` into another `m:wxMemoryDC`.

The parameter `quality` determines what method to use for resampling the image, see
wxImageResizeQuality documentation.

It should be noted that although using `wxIMAGE_QUALITY_HIGH` produces much nicer looking
results it is a slower method. Downsampling will use the box averaging method which seems
to operate very fast. If you are upsampling larger images using this method you will most
likely notice that it is a bit slower and in extreme cases it will be quite substantially
slower as the bicubic algorithm has to process a lot of data.

It should also be noted that the high quality scaling may not work as expected when using
a single mask colour for transparency, as the scaling will blur the image and will
therefore remove the mask partially. Using the alpha channel will work.

Example:

See: `rescale/4`

# `setAlpha`

```elixir
-spec setAlpha(This, Alpha) -> ok when This :: wxImage(), Alpha :: binary().
```

This function is similar to `setData/4` and has similar restrictions.

The pointer passed to it may however be NULL in which case the function will allocate the
alpha array internally - this is useful to add alpha channel data to an image which
doesn't have any.

If the pointer is not NULL, it must have one byte for each image pixel and be allocated
with malloc(). `m:wxImage` takes ownership of the pointer and will free it unless `static_data`
parameter is set to true - in this case the caller should do it.

# `setAlpha`

```elixir
-spec setAlpha(This, X, Y, Alpha) -> ok
                  when This :: wxImage(), X :: integer(), Y :: integer(), Alpha :: integer().
```

Sets the alpha value for the given pixel.

This function should only be called if the image has alpha channel data, use `hasAlpha/1` to check
for this.

# `setData`

```elixir
-spec setData(This, Data) -> ok when This :: wxImage(), Data :: binary().
```

Sets the image data without performing checks.

The data given must have the size (width*height*3) or results will be unexpected. Don't
use this method if you aren't sure you know what you are doing.

The data must have been allocated with `malloc()`, `NOT` with `operator` new.

If `static_data` is false, after this call the pointer to the data is owned by the `m:wxImage`
object, that will be responsible for deleting it. Do not pass to this function a pointer
obtained through `getData/1`.

# `setData`

```elixir
-spec setData(This, Data, New_width, New_height) -> ok
                 when
                     This :: wxImage(),
                     Data :: binary(),
                     New_width :: integer(),
                     New_height :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setMask`

```elixir
-spec setMask(This) -> ok when This :: wxImage().
```

# `setMask`

```elixir
-spec setMask(This, [Option]) -> ok when This :: wxImage(), Option :: {mask, boolean()}.
```

Specifies whether there is a mask or not.

The area of the mask is determined by the current mask colour.

# `setMaskColour`

```elixir
-spec setMaskColour(This, Red, Green, Blue) -> ok
                       when This :: wxImage(), Red :: integer(), Green :: integer(), Blue :: integer().
```

Sets the mask colour for this image (and tells the image to use the mask).

# `setMaskFromImage`

```elixir
-spec setMaskFromImage(This, Mask, Mr, Mg, Mb) -> boolean()
                          when
                              This :: wxImage(),
                              Mask :: wxImage(),
                              Mr :: integer(),
                              Mg :: integer(),
                              Mb :: integer().
```

Sets image's mask so that the pixels that have RGB value of mr,mg,mb in mask will be
masked in the image.

This is done by first finding an unused colour in the image, setting this colour as the
mask colour and then using this colour to draw all pixels in the image who corresponding
pixel in mask has given RGB value.

The parameter `mask` is the mask image to extract mask shape from. It must have the same
dimensions as the image.

The parameters `mr`, `mg`, `mb` are the RGB values of the pixels in mask that will be
used to create the mask.

Return: Returns false if mask does not have same dimensions as the image or if there is
no unused colour left. Returns true if the mask was successfully applied.

Note: Note that this method involves computing the histogram, which is a computationally
intensive operation.

# `setOption`

```elixir
-spec setOption(This, Name, Value) -> ok
                   when This :: wxImage(), Name :: unicode:chardata(), Value :: integer();
               (This, Name, Value) -> ok
                   when This :: wxImage(), Name :: unicode:chardata(), Value :: unicode:chardata().
```

Sets a user-defined option.

The function is case-insensitive to `name`.

For example, when saving as a JPEG file, the option `quality` is used, which is a number
between 0 and 100 (0 is terrible, 100 is very good).

The lists of the currently supported options are in `getOption/2` and `getOptionInt/2` function docs.

See:
* `getOption/2`

* `getOptionInt/2`

* `hasOption/2`

# `setPalette`

```elixir
-spec setPalette(This, Palette) -> ok when This :: wxImage(), Palette :: wxPalette:wxPalette().
```

Associates a palette with the image.

The palette may be used when converting `m:wxImage` to `m:wxBitmap` (MSW only at present)
or in file save operations (none as yet).

# `setRGB`

```elixir
-spec setRGB(This, Rect, Red, Green, Blue) -> ok
                when
                    This :: wxImage(),
                    Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                    Red :: integer(),
                    Green :: integer(),
                    Blue :: integer().
```

Sets the colour of the pixels within the given rectangle.

This routine performs bounds-checks for the coordinate so it can be considered a safe way
to manipulate the data.

# `setRGB`

```elixir
-spec setRGB(This, X, Y, R, G, B) -> ok
                when
                    This :: wxImage(),
                    X :: integer(),
                    Y :: integer(),
                    R :: integer(),
                    G :: integer(),
                    B :: integer().
```

Set the color of the pixel at the given x and y coordinate.

# `size`

```elixir
-spec size(This, Size, Pos) -> wxImage()
              when
                  This :: wxImage(),
                  Size :: {W :: integer(), H :: integer()},
                  Pos :: {X :: integer(), Y :: integer()}.
```

# `size`

```elixir
-spec size(This, Size, Pos, [Option]) -> wxImage()
              when
                  This :: wxImage(),
                  Size :: {W :: integer(), H :: integer()},
                  Pos :: {X :: integer(), Y :: integer()},
                  Option :: {r, integer()} | {g, integer()} | {b, integer()}.
```

Returns a resized version of this image without scaling it by adding either a border with
the given colour or cropping as necessary.

The image is pasted into a new image with the given `size` and background colour at the
position `pos` relative to the upper left of the new image.

If `red` = green = blue = -1 then the areas of the larger image not covered by this image
are made transparent by filling them with the image mask colour (which will be allocated
automatically if it isn't currently set).

Otherwise, the areas will be filled with the colour with the specified RGB components.

See: `resize/4`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
