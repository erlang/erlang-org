# `wxLocale`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxLocale.erl#L58)

`m:wxLocale` class encapsulates all language-dependent settings and is a generalization
of the C locale concept.

In wxWidgets this class manages current locale. It also initializes and activates `wxTranslations`
(not implemented in wx) object that manages message catalogs.

For a list of the supported languages, please see ?wxLanguage enum values. These
constants may be used to specify the language in `init/3` and are returned by `getSystemLanguage/0`.

See:
* [Overview i18n](https://docs.wxwidgets.org/3.2/overview_i18n.html#overview_i18n)

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_internat)

wxWidgets docs: [wxLocale](https://docs.wxwidgets.org/3.2/classwx_locale.html)

# `wxLocale`

```erlang
-type wxLocale() :: wx:wx_object().
```

# `addCatalog`

```erlang
-spec addCatalog(This, Domain) -> boolean() when This :: wxLocale(), Domain :: unicode:chardata().
```

Calls wxTranslations::AddCatalog(const wxString&).

# `addCatalog`

```erlang
-spec addCatalog(This, Domain, MsgIdLanguage) -> boolean()
                    when This :: wxLocale(), Domain :: unicode:chardata(), MsgIdLanguage :: wx:wx_enum().
```

Calls `wxTranslations::AddCatalog(const wxString&, wxLanguage)` (not implemented in wx).

# `addCatalog`

```erlang
-spec addCatalog(This, Domain, MsgIdLanguage, MsgIdCharset) -> boolean()
                    when
                        This :: wxLocale(),
                        Domain :: unicode:chardata(),
                        MsgIdLanguage :: wx:wx_enum(),
                        MsgIdCharset :: unicode:chardata().
```

Calls `wxTranslations::AddCatalog(const wxString&, wxLanguage, const wxString&)` (not
implemented in wx).

# `addCatalogLookupPathPrefix`

```erlang
-spec addCatalogLookupPathPrefix(Prefix) -> ok when Prefix :: unicode:chardata().
```

Calls `wxFileTranslationsLoader::AddCatalogLookupPathPrefix()` (not implemented in wx).

# `destroy`

```erlang
-spec destroy(This :: wxLocale()) -> ok.
```

Destroys the object

# `getCanonicalName`

```erlang
-spec getCanonicalName(This) -> unicode:charlist() when This :: wxLocale().
```

Returns the canonical form of current locale name.

Canonical form is the one that is used on UNIX systems: it is a two- or five-letter
string in xx or xx_YY format, where xx is ISO 639 code of language and YY is ISO 3166 code
of the country. Examples are "en", "en_GB", "en_US" or "fr_FR". This form is internally
used when looking up message catalogs. Compare `getSysName/1`.

# `getHeaderValue`

```erlang
-spec getHeaderValue(This, Header) -> unicode:charlist()
                        when This :: wxLocale(), Header :: unicode:chardata().
```

# `getHeaderValue`

```erlang
-spec getHeaderValue(This, Header, [Option]) -> unicode:charlist()
                        when
                            This :: wxLocale(),
                            Header :: unicode:chardata(),
                            Option :: {szDomain, unicode:chardata()}.
```

Calls `wxTranslations::GetHeaderValue()` (not implemented in wx).

# `getLanguage`

```erlang
-spec getLanguage(This) -> integer() when This :: wxLocale().
```

Returns the ?wxLanguage constant of current language.

Note that you can call this function only if you used the form of `init/3` that takes ?wxLanguage
argument.

# `getLanguageName`

```erlang
-spec getLanguageName(Lang) -> unicode:charlist() when Lang :: integer().
```

Returns English name of the given language or empty string if this language is unknown.

See `GetLanguageInfo()` (not implemented in wx) for a remark about special meaning of `wxLANGUAGE_DEFAULT`.

# `getLocale`

```erlang
-spec getLocale(This) -> unicode:charlist() when This :: wxLocale().
```

Returns the locale name as passed to the constructor or `init/3`.

This is a full, human-readable name, e.g. "English" or "French".

# `getName`

```erlang
-spec getName(This) -> unicode:charlist() when This :: wxLocale().
```

Returns the current short name for the locale (as given to the constructor or the `init/3`
function).

# `getString`

```erlang
-spec getString(This, OrigString) -> unicode:charlist()
                   when This :: wxLocale(), OrigString :: unicode:chardata().
```

# `getString`

```erlang
-spec getString(This, OrigString, [Option]) -> unicode:charlist()
                   when
                       This :: wxLocale(),
                       OrigString :: unicode:chardata(),
                       Option :: {szDomain, unicode:chardata()}.
```

Calls wxGetTranslation(const wxString&, const wxString&).

# `getString`

```erlang
-spec getString(This, OrigString, OrigString2, N) -> unicode:charlist()
                   when
                       This :: wxLocale(),
                       OrigString :: unicode:chardata(),
                       OrigString2 :: unicode:chardata(),
                       N :: integer().
```

# `getString`

```erlang
-spec getString(This, OrigString, OrigString2, N, [Option]) -> unicode:charlist()
                   when
                       This :: wxLocale(),
                       OrigString :: unicode:chardata(),
                       OrigString2 :: unicode:chardata(),
                       N :: integer(),
                       Option :: {szDomain, unicode:chardata()}.
```

Calls wxGetTranslation(const wxString&, const wxString&, unsigned, const wxString&).

# `getSysName`

```erlang
-spec getSysName(This) -> unicode:charlist() when This :: wxLocale().
```

Returns current platform-specific locale name as passed to setlocale().

Compare `getCanonicalName/1`.

# `getSystemEncoding`

```erlang
-spec getSystemEncoding() -> wx:wx_enum().
```

Tries to detect the user's default font encoding.

Returns ?wxFontEncoding() value or `wxFONTENCODING_SYSTEM` if it couldn't be determined.

# `getSystemEncodingName`

```erlang
-spec getSystemEncodingName() -> unicode:charlist().
```

Tries to detect the name of the user's default font encoding.

This string isn't particularly useful for the application as its form is
platform-dependent and so you should probably use `getSystemEncoding/0` instead.

Returns a user-readable string value or an empty string if it couldn't be determined.

# `getSystemLanguage`

```erlang
-spec getSystemLanguage() -> integer().
```

Tries to detect the user's default locale setting.

Returns the ?wxLanguage value or `wxLANGUAGE_UNKNOWN` if the language-guessing algorithm failed.

Note: This function works with `locales` and returns the user's default locale. This may
be, and usually is, the same as their preferred UI language, but it's not the same thing.
Use wxTranslation to obtain `language` information.

# `init`

```erlang
-spec init(This) -> boolean() when This :: wxLocale().
```

# `init`

```erlang
-spec init(This, [Option]) -> boolean()
              when This :: wxLocale(), Option :: {language, integer()} | {flags, integer()}.
```

Initializes the `m:wxLocale` instance.

The call of this function has several global side effects which you should understand:
first of all, the application locale is changed - note that this will affect many of
standard C library functions such as printf() or strftime(). Second, this `m:wxLocale`
object becomes the new current global locale for the application and so all subsequent
calls to ?wxGetTranslation() will try to translate the messages using the message catalogs
for this locale.

Return: true on success or false if the given locale couldn't be set.

# `init`

```erlang
-spec init(This, Name, [Option]) -> boolean()
              when
                  This :: wxLocale(),
                  Name :: unicode:chardata(),
                  Option ::
                      {shortName, unicode:chardata()} |
                      {locale, unicode:chardata()} |
                      {bLoadDefault, boolean()}.
```

Deprecated:

This form is deprecated, use the other one unless you know what you are doing.

# `isLoaded`

```erlang
-spec isLoaded(This, Domain) -> boolean() when This :: wxLocale(), Domain :: unicode:chardata().
```

Calls `wxTranslations::IsLoaded()` (not implemented in wx).

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxLocale().
```

Returns true if the locale could be set successfully.

# `new`

```erlang
-spec new() -> wxLocale().
```

This is the default constructor and it does nothing to initialize the object: `init/3`
must be used to do that.

# `new`

```erlang
-spec new(Language) -> wxLocale() when Language :: integer();
         (Name) -> wxLocale() when Name :: unicode:chardata().
```

Equivalent to: `new/2`

# `new`

```erlang
-spec new(Language, [Option]) -> wxLocale() when Language :: integer(), Option :: {flags, integer()};
         (Name, [Option]) -> wxLocale()
             when
                 Name :: unicode:chardata(),
                 Option ::
                     {shortName, unicode:chardata()} |
                     {locale, unicode:chardata()} |
                     {bLoadDefault, boolean()}.
```

See `init/3` for parameters description.

The call of this function has several global side effects which you should understand:
first of all, the application locale is changed - note that this will affect many of
standard C library functions such as printf() or strftime(). Second, this `m:wxLocale`
object becomes the new current global locale for the application and so all subsequent
calls to ?wxGetTranslation() will try to translate the messages using the message catalogs
for this locale.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
