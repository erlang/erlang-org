# ssh v6.0.2 - API Reference

## Modules

- [ssh](ssh.md): Main API of the ssh application
- [ssh_agent](ssh_agent.md): Callback module for using an SSH agent instead of the default ssh_file callback.
- [ssh_client_channel](ssh_client_channel.md): SSH services (clients and servers) are implemented as channels that are
multiplexed over an SSH connection and communicates over the
[SSH Connection Protocol](http://www.ietf.org/rfc/rfc4254.txt). This module
provides a callback API that takes care of generic channel aspects for clients,
such as flow control and close messages. It lets the callback functions take
care of the service (application) specific parts. This behavior also ensures
that the channel process honors the principal of an OTP-process so that it can
be part of a supervisor tree. This is a requirement of channel processes
implementing a subsystem that will be added to the `ssh` applications supervisor
tree.
- [ssh_client_key_api](ssh_client_key_api.md): \-behaviour(ssh_client_key_api).
- [ssh_connection](ssh_connection.md): This module provides API functions to send SSH Connection Protocol events to the
other side of an SSH channel.
- [ssh_file](ssh_file.md): Default callback module for the client's and server's database operations in the
ssh application
- [ssh_server_channel](ssh_server_channel.md): \-behaviour(ssh_server_channel). (Replaces ssh_daemon_channel)
- [ssh_server_key_api](ssh_server_key_api.md): \-behaviour(ssh_server_key_api).
- [ssh_sftp](ssh_sftp.md): SFTP client.
- [ssh_sftpd](ssh_sftpd.md): Specifies the channel process to handle an SFTP subsystem.

