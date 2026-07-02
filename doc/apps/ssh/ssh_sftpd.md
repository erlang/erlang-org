# `ssh_sftpd`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/ssh/src/ssh_sftpd.erl#L27)

Specifies the channel process to handle an SFTP subsystem.

Specifies a channel process to handle an SFTP subsystem.

# `subsystem_spec`

```erlang
-spec subsystem_spec(Options) -> Spec
                        when
                            Options ::
                                [{cwd, string()} |
                                 {file_handler, CbMod | {CbMod, FileState}} |
                                 {max_files, integer()} |
                                 {max_handles, integer()} |
                                 {max_path, integer()} |
                                 {root, string()} |
                                 {sftpd_vsn, integer()}],
                            Spec :: {Name, {CbMod, Options}},
                            Name :: string(),
                            CbMod :: atom(),
                            FileState :: term().
```

Is to be used together with `ssh:daemon/[1,2,3]`

The `Name` is `"sftp"` and `CbMod` is the name of the Erlang module implementing
the subsystem using the `m:ssh_server_channel` (replaces ssh_daemon_channel)
behaviour.

Options:

- **`cwd`** - Sets the initial current working directory for the server.

- **`file_handler`** - Determines which module to call for accessing the file
  server. The default value is `ssh_sftpd_file`, which uses the `m:file` and
  `m:filelib` APIs to access the standard OTP file server. This option can be
  used to plug in other file servers.

- **`max_files`** - The default value is `0`, which means that there is no upper
  limit. If supplied, the number of filenames returned to the SFTP client per
  `READDIR` request is limited to at most the given value.

- **`max_handles`** - The default value is `1000`. Positive integer
  value represents the maximum number of file handles allowed for a
  connection.

  (Note: separate limitation might be also enforced by underlying
  operating system)

- **`max_path`** - The default value is `4096`. Positive integer value
    represents the maximum path length which cannot be exceeded in
    data provided by the SFTP client. (Note: limitations might be also
    enforced by underlying operating system)

- **`root`** - Sets the SFTP root directory. Must be an absolute path (e.g., `/tmp`).
  Then the user cannot see any files above this root. If, for example, the root
  directory is set to `/tmp`, then the user sees this directory as `/`. If the
  user then writes `cd /etc`, the user moves to `/tmp/etc`.

  Note: This provides application-level isolation. For additional security,
  consider using OS-level chroot or similar mechanisms. See the
  [SFTP subsystem](hardening.md#sftp-subsystem) section in the Hardening guide
  for deployment recommendations.

- **`sftpd_vsn`** - Sets the SFTP version to use. Defaults to 5. Version 6 is
  under development and limited.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
