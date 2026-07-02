# `zlib`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/erts/preloaded/src/zlib.erl#L23)

zlib compression interface.

This module provides an API for the zlib library
([www.zlib.net](http://www.zlib.net)). It is used to compress and decompress
data. The data format is described by
[RFC 1950](https://www.ietf.org/rfc/rfc1950.txt),
[RFC 1951](https://www.ietf.org/rfc/rfc1951.txt), and
[RFC 1952](https://www.ietf.org/rfc/rfc1952.txt).

A typical (compress) usage is as follows:

```erlang
Z = zlib:open(),
ok = zlib:deflateInit(Z,default),

Compress = fun F(end_of_data) ->
                 zlib:deflate(Z, [], finish);
               F(Data) ->
                 [zlib:deflate(Z, Data)|F(Read())]
           end,
Compressed = Compress(Read()),
ok = zlib:deflateEnd(Z),
zlib:close(Z),
list_to_binary(Compressed)
```

In all functions errors, `{'EXIT',{Reason,Backtrace}}`, can be thrown, where
`Reason` describes the error.

Typical `Reason`s:

- **`badarg`** - Bad argument.

- **`not_initialized`** - The stream hasn't been initialized, eg. if
  `inflateInit/1` wasn't called prior to a call to `inflate/2`.

- **`not_on_controlling_process`** - The stream was used by a process that
  doesn't control it. Use `set_controlling_process/2` if you need to transfer a
  stream to a different process.

- **`data_error`** - The data contains errors.

- **`stream_error`** - Inconsistent stream state.

- **`{need_dictionary,Adler32}`** - See `inflate/2`.

# `zflush`

```erlang
-type zflush() :: none | sync | full | finish.
```

# `zlevel`

```erlang
-type zlevel() :: none | default | best_compression | best_speed | 0..9.
```

# `zmemlevel`

```erlang
-type zmemlevel() :: 1..9.
```

# `zmethod`

```erlang
-type zmethod() :: deflated.
```

# `zstrategy`

```erlang
-type zstrategy() :: default | filtered | huffman_only | rle.
```

# `zstream`

```erlang
-type zstream() :: reference().
```

A zlib stream, see `open/0`.

# `zwindowbits`

```erlang
-type zwindowbits() :: -15..-8 | 8..47.
```

Normally in the range `-15..-8 | 8..15`.

# `close`

```erlang
-spec close(Z) -> ok when Z :: zstream().
```

Closes the stream referenced by `Z`.

# `compress`

```erlang
-spec compress(Data) -> Compressed when Data :: iodata(), Compressed :: binary().
```

Compresses data with zlib headers and checksum.

# `deflate`

```erlang
-spec deflate(Z, Data) -> Compressed when Z :: zstream(), Data :: iodata(), Compressed :: iolist().
```

Same as [`deflate(Z, Data, none)`](`deflate/3`).

# `deflate`

```erlang
-spec deflate(Z, Data, Flush) -> Compressed
                 when Z :: zstream(), Data :: iodata(), Flush :: zflush(), Compressed :: iolist().
```

Compresses as much data as possible, and stops when the input buffer becomes
empty.

It can introduce some output latency (reading input without producing any
output) except when forced to flush.

If `Flush` is set to `sync`, all pending output is flushed to the output buffer
and the output is aligned on a byte boundary, so that the decompressor can get
all input data available so far. Flushing can degrade compression for some
compression algorithms; thus, use it only when necessary.

If `Flush` is set to `full`, all output is flushed as with `sync`, and the
compression state is reset so that decompression can restart from this point if
previous compressed data has been damaged or if random access is desired. Using
`full` too often can seriously degrade the compression.

If `Flush` is set to `finish`, pending input is processed, pending output is
flushed, and [`deflate/3`](`deflate/3`) returns. Afterwards the only possible
operations on the stream are `deflateReset/1` or `deflateEnd/1`.

`Flush` can be set to `finish` immediately after
[`deflateInit`](`deflateInit/1`) if all compression is to be done in one step.

Example:

```erlang
zlib:deflateInit(Z),
B1 = zlib:deflate(Z,Data),
B2 = zlib:deflate(Z,<< >>,finish),
zlib:deflateEnd(Z),
list_to_binary([B1,B2])
```

# `deflateEnd`

```erlang
-spec deflateEnd(Z) -> ok when Z :: zstream().
```

Ends the deflate session and cleans all data used.

Notice that this function throws a `data_error` exception if the last call to
`deflate/3` was not called with `Flush` set to `finish`.

# `deflateInit`

```erlang
-spec deflateInit(Z) -> ok when Z :: zstream().
```

Same as `zlib:deflateInit(Z, default)`.

# `deflateInit`

```erlang
-spec deflateInit(Z, Level) -> ok when Z :: zstream(), Level :: zlevel().
```

Initializes a zlib stream for compression.

`Level` decides the compression level to be used:

- `default` gives default compromise between speed and compression
- `none` (0) gives no compression
- `best_speed` (1) gives best speed
- `best_compression` (9) gives best compression

# `deflateInit`

```erlang
-spec deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) -> ok
                     when
                         Z :: zstream(),
                         Level :: zlevel(),
                         Method :: zmethod(),
                         WindowBits :: zwindowbits(),
                         MemLevel :: zmemlevel(),
                         Strategy :: zstrategy().
```

Initiates a zlib stream for compression.

- **`Level`** - Compression level to use:

  - `default` gives default compromise between speed and compression
  - `none` (0) gives no compression
  - `best_speed` (1) gives best speed
  - `best_compression` (9) gives best compression

- **`Method`** - Compression method to use, currently the only supported method
  is `deflated`.

- **`WindowBits`** - The base two logarithm of the window size (the size of the
  history buffer). It is to be in the range 8 through 15. Larger values result
  in better compression at the expense of memory usage. Defaults to 15 if
  `deflateInit/2` is used. A negative `WindowBits` value suppresses the zlib
  header (and checksum) from the stream. Notice that the zlib source mentions
  this only as a undocumented feature.

  > #### Warning {: .warning }
  >
  > Due to a known bug in the underlying zlib library, `WindowBits` values 8 and
  > -8 do not work as expected. In zlib versions before 1.2.9 values 8 and -8
  > are automatically changed to 9 and -9. _From zlib version 1.2.9 value -8 is
  > rejected_ causing `zlib:deflateInit/6` to fail (8 is still changed to 9). It
  > also seem possible that future versions of zlib may fix this bug and start
  > accepting 8 and -8 as is.
  >
  > Conclusion: Avoid values 8 and -8 unless you know your zlib version supports
  > them.

- **`MemLevel`** - Specifies how much memory is to be allocated for the internal
  compression state: `MemLevel`=1 uses minimum memory but is slow and reduces
  compression ratio; `MemLevel`=9 uses maximum memory for optimal speed.
  Defaults to 8.

- **`Strategy`** - Tunes the compression algorithm. Use the following values:

  - `default` for normal data
  - `filtered` for data produced by a filter (or predictor)
  - `huffman_only` to force Huffman encoding only (no string match)
  - `rle` to limit match distances to one (run-length encoding)

  Filtered data consists mostly of small values with a somewhat random
  distribution. In this case, the compression algorithm is tuned to compress
  them better. The effect of `filtered` is to force more Huffman coding and less
  string matching; it is somewhat intermediate between `default` and
  `huffman_only`. `rle` is designed to be almost as fast as `huffman_only`, but
  gives better compression for PNG image data.

  `Strategy` affects only the compression ratio, but not the correctness of the
  compressed output even if it is not set appropriately.

# `deflateParams`

```erlang
-spec deflateParams(Z, Level, Strategy) -> ok
                       when Z :: zstream(), Level :: zlevel(), Strategy :: zstrategy().
```

Dynamically updates the compression level and compression strategy.

The interpretation of `Level` and `Strategy` is as in `deflateInit/6`. This can be
used to switch between compression and straight copy of the input data, or to
switch to a different kind of input data requiring a different strategy. If the
compression level is changed, the input available so far is compressed with the
old level (and can be flushed); the new level takes effect only at the next call
of `deflate/3`.

Before the call of `deflateParams`, the stream state must be set as for a call
of [`deflate/3`](`deflate/3`), as the currently available input may have to be
compressed and flushed.

# `deflateReset`

```erlang
-spec deflateReset(Z) -> ok when Z :: zstream().
```

Equivalent to `deflateEnd/1` followed by [`deflateInit/1,2,6`](`deflateInit/1`),
but does not free and reallocate all the internal compression state.

The stream keeps the same compression level and any other attributes.

# `deflateSetDictionary`

```erlang
-spec deflateSetDictionary(Z, Dictionary) -> Adler32
                              when Z :: zstream(), Dictionary :: iodata(), Adler32 :: non_neg_integer().
```

Initializes the compression dictionary from the specified byte sequence without
producing any compressed output.

This function must be called immediately after
[`deflateInit/1,2,6`](`deflateInit/1`) or `deflateReset/1`, before any call of
`deflate/3`.

The compressor and decompressor must use the same dictionary (see
`inflateSetDictionary/2`).

The Adler checksum of the dictionary is returned.

# `gunzip`

```erlang
-spec gunzip(Data) -> Decompressed when Data :: iodata(), Decompressed :: binary().
```

Uncompresses data with gz headers and checksum.

# `gzip`

```erlang
-spec gzip(Data) -> Compressed when Data :: iodata(), Compressed :: binary().
```

Compresses data with gz headers and checksum.

# `inflate`

```erlang
-spec inflate(Z, Data) -> Decompressed when Z :: zstream(), Data :: iodata(), Decompressed :: iolist().
```

Equivalent to [`inflate(Z, Data, [])`](`inflate/3`)

# `inflate`
*since OTP 20.1* 

```erlang
-spec inflate(Z, Data, Options) -> Decompressed
                 when
                     Z :: zstream(),
                     Data :: iodata(),
                     Options :: [{exception_on_need_dict, boolean()}],
                     Decompressed ::
                         iolist() | {need_dictionary, Adler32 :: non_neg_integer(), Output :: iolist()}.
```

Decompresses as much data as possible. It can introduce some output latency
(reading input without producing any output).

Currently the only available option is `{exception_on_need_dict,boolean()}`
which controls whether the function should throw an exception when a preset
dictionary is required for decompression. When set to false, a `need_dictionary`
tuple will be returned instead. See `inflateSetDictionary/2` for details.

> #### Warning {: .warning }
>
> This option defaults to `true` for backwards compatibility but we intend to
> remove the exception behavior in a future release. New code that needs to
> handle dictionaries manually should always specify
> `{exception_on_need_dict,false}`.

# `inflateEnd`

```erlang
-spec inflateEnd(Z) -> ok when Z :: zstream().
```

Ends the inflate session and cleans all data used.

Notice that this function throws a `data_error` exception if no end of stream
was found (meaning that not all data has been uncompressed).

# `inflateGetDictionary`
*since OTP 20.0* 

```erlang
-spec inflateGetDictionary(Z) -> Dictionary when Z :: zstream(), Dictionary :: binary().
```

Returns the decompression dictionary currently in use by the stream.

This function must be called between [`inflateInit/1,2`](`inflateInit/1`) and
[`inflateEnd`](`inflateEnd/1`).

Only supported if ERTS was compiled with zlib >= 1.2.8.

# `inflateInit`

```erlang
-spec inflateInit(Z) -> ok when Z :: zstream().
```

Initializes a zlib stream for decompression.

# `inflateInit`

```erlang
-spec inflateInit(Z, WindowBits) -> ok when Z :: zstream(), WindowBits :: zwindowbits().
```

Initializes a decompression session on zlib stream.

`WindowBits` is the base two logarithm of the maximum window size (the size of
the history buffer). It is to be in the range 8 through 15. Default to 15 if
`inflateInit/1` is used.

If a compressed stream with a larger window size is specified as input,
`inflate/2` throws the `data_error` exception.

A negative `WindowBits` value makes zlib ignore the zlib header (and checksum)
from the stream. Notice that the zlib source mentions this only as a
undocumented feature.

# `inflateReset`

```erlang
-spec inflateReset(Z) -> ok when Z :: zstream().
```

Equivalent to `inflateEnd/1` followed by `inflateInit/1`, but does not free and
reallocate all the internal decompression state. The stream will keep attributes
that could have been set by `inflateInit/1,2`.

# `inflateSetDictionary`

```erlang
-spec inflateSetDictionary(Z, Dictionary) -> ok when Z :: zstream(), Dictionary :: iodata().
```

Initializes the decompression dictionary from the specified uncompressed byte
sequence.

This function must be called as a response to an inflate operation
(eg. `safeInflate/2`) returning `{need_dictionary,Adler,Output}` or in the case
of deprecated functions, throwing an
`{'EXIT',{{need_dictionary,Adler},_StackTrace}}` exception.

The dictionary chosen by the compressor can be determined from the Adler value
returned or thrown by the call to the inflate function. The compressor and
decompressor must use the same dictionary (See `deflateSetDictionary/2`).

After setting the dictionary the inflate operation should be retried without new
input.

Example:

```erlang
deprecated_unpack(Z, Compressed, Dict) ->
     case catch zlib:inflate(Z, Compressed) of
          {'EXIT',{{need_dictionary,_DictID},_}} ->
                 ok = zlib:inflateSetDictionary(Z, Dict),
                 Uncompressed = zlib:inflate(Z, []);
          Uncompressed ->
                 Uncompressed
     end.

new_unpack(Z, Compressed, Dict) ->
    case zlib:inflate(Z, Compressed, [{exception_on_need_dict, false}]) of
        {need_dictionary, _DictId, Output} ->
            ok = zlib:inflateSetDictionary(Z, Dict),
            [Output | zlib:inflate(Z, [])];
        Uncompressed ->
            Uncompressed
    end.
```

# `open`

```erlang
-spec open() -> zstream().
```

Opens a zlib stream.

# `safeInflate`
*since OTP 20.1* 

```erlang
-spec safeInflate(Z, Data) -> Result
                     when
                         Z :: zstream(),
                         Data :: iodata(),
                         Result ::
                             {continue, Output :: iolist()} |
                             {finished, Output :: iolist()} |
                             {need_dictionary, Adler32 :: non_neg_integer(), Output :: iolist()}.
```

Like `inflate/2`, but returns once it has expanded beyond a small
implementation-defined threshold. It's useful when decompressing untrusted input
which could have been maliciously crafted to expand until the system runs out of
memory.

This function returns `{continue | finished, Output}`, where Output is the data
that was decompressed in this call. New input can be queued up on each call if
desired, and the function will return `{finished, Output}` once all queued data
has been decompressed.

This function can introduce some output latency (reading input without producing
any output).

If a preset dictionary is required for further decompression, this function
returns a `need_dictionary` tuple. See `inflateSetDictionary/2`) for details.

Example:

```erlang
walk(Compressed, Handler) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    loop(Z, Handler, zlib:safeInflate(Z, Compressed)),
    zlib:inflateEnd(Z),
    zlib:close(Z).

loop(Z, Handler, {continue, Output}) ->
    Handler(Output),
    loop(Z, Handler, zlib:safeInflate(Z, []));
loop(Z, Handler, {finished, Output}) ->
    Handler(Output).
```

# `set_controlling_process`
*since OTP 20.1.3* 

```erlang
-spec set_controlling_process(Z, Pid) -> ok when Z :: zstream(), Pid :: pid().
```

Changes the controlling process of `Z` to `Pid`, which must be a local process.

# `uncompress`

```erlang
-spec uncompress(Data) -> Decompressed when Data :: iodata(), Decompressed :: binary().
```

Uncompresses data with zlib headers and checksum.

# `unzip`

```erlang
-spec unzip(Data) -> Decompressed when Data :: iodata(), Decompressed :: binary().
```

Uncompresses data without zlib headers and checksum.

# `zip`

```erlang
-spec zip(Data) -> Compressed when Data :: iodata(), Compressed :: binary().
```

Compresses data without zlib headers and checksum.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
