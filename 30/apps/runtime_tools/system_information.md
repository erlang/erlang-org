# `system_information`
[🔗](https://github.com/erlang/otp/blob/master/lib/runtime_tools/src/system_information.erl#L29)

System Information

# `sanity_check`
*since OTP 17.0* 

```erlang
-spec sanity_check() -> ok | {failed, Failures}
                      when
                          Application :: atom(),
                          ApplicationVersion :: string(),
                          MissingRuntimeDependencies ::
                              {missing_runtime_dependencies, ApplicationVersion, [ApplicationVersion]},
                          InvalidApplicationVersion :: {invalid_application_version, ApplicationVersion},
                          InvalidAppFile :: {invalid_app_file, Application},
                          Failure ::
                              MissingRuntimeDependencies | InvalidApplicationVersion | InvalidAppFile,
                          Failures :: [Failure].
```

Performs a sanity check on the system.

If no issues were found, `ok` is returned. If issues were found,
`{failed, Failures}` is returned. All failures found will be part of
the `Failures` list. Currently defined `Failure` elements in the
`Failures` list:

- **`InvalidAppFile`** - An application has an invalid `.app` file. The second
  element identifies the application which has the invalid `.app` file.

- **`InvalidApplicationVersion`** - An application has an invalid application
  version. The second element identifies the application version that is
  invalid.

- **`MissingRuntimeDependencies`** - An application is missing
  [runtime dependencies](`e:kernel:app.md#runtime_dependencies`). The second
  element identifies the application (with version) that has missing
  dependencies. The third element contains the missing dependencies.

  Note that this check use application versions that are loaded, or will be
  loaded when used. You might have application versions that satisfies all
  dependencies installed in the system, but if those are not loaded this check
  will fail. Of course, the system will also fail when used like this. This can
  happen when you have multiple [branched versions](`e:system:versions.md`) of
  the same application installed in the system, but there does not exist a
  [boot script](`e:system:system_principles.md#BOOTSCRIPT`) identifying the
  correct application version.

Currently the sanity check is limited to verifying runtime dependencies found in
the `.app` files of all applications. More checks will be introduced in the
future. This implies that the return type _will_ change in the future.

> #### Note {: .info }
>
> An `ok` return value only means that `sanity_check/0` did not find any issues,
> _not_ that no issues exist.

# `to_file`
*since OTP 17.0* 

```erlang
-spec to_file(FileName) -> ok | {error, Reason}
                 when
                     FileName :: file:name_all(),
                     Reason :: file:posix() | badarg | terminated | system_limit.
```

Writes miscellaneous system information to file. This information will typically
be requested by the Erlang/OTP team at Ericsson AB when reporting an issue.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
