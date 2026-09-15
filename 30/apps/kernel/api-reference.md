# kernel v11.0.3-rc0 - API Reference

## Modules

- [application](application.md): Generic OTP application functions
- [data_publisher](data_publisher.md): A behavior for building eventually consistent, replicated data stores
across distributed nodes.
- [error_handler](error_handler.md): Default system error handler.
- [file](file.md): File interface module.
- [heart](heart.md): Heartbeat monitoring of an Erlang runtime system.
- [os](os.md): Operating system-specific functions.

- NETWORKING
  - [gen_sctp](gen_sctp.md): Interface to SCTP sockets.
  - [gen_tcp](gen_tcp.md): Interface to TCP/IP sockets.
  - [gen_udp](gen_udp.md): Interface to UDP sockets.
  - [inet](inet.md): Access to Network protocols.
  - [inet_res](inet_res.md): A rudimentary DNS client.
  - [net](net.md): Network interface.
  - [socket](socket.md): Socket interface.

- LOGGING
  - [disk_log](disk_log.md): `disk_log` is a disk-based term logger that enables efficient logging of items
on files.
  - [error_logger](error_logger.md): Erlang error logger.
  - [logger](logger.md): API module for Logger, the standard logging facility in Erlang/OTP.
  - [logger_disk_log_h](logger_disk_log_h.md): A disk_log based handler for Logger
  - [logger_filters](logger_filters.md): Filters to use with Logger.
  - [logger_formatter](logger_formatter.md): Default formatter for Logger.
  - [logger_handler](logger_handler.md): logger_handler behavior module.
  - [logger_std_h](logger_std_h.md): Standard handler for Logger.
  - [wrap_log_reader](wrap_log_reader.md): A service to read internally formatted wrap disk logs.

- DISTRIBUTION
  - [auth](auth.md): Erlang network authentication server.
  - [erl_boot_server](erl_boot_server.md): Boot server for other Erlang machines.
  - [erl_epmd](erl_epmd.md): Erlang interface towards epmd
  - [erpc](erpc.md): Enhanced Remote Procedure Call
  - [global](global.md): A global name registration facility.
  - [global_group](global_group.md): Grouping nodes to global name registration groups.
  - [net_adm](net_adm.md): Various Erlang net administration routines.
  - [net_kernel](net_kernel.md): Erlang networking kernel.
  - [pg](pg.md): Distributed named process groups.
  - [rpc](rpc.md): Remote Procedure Call services.

- CODE
  - [code](code.md): Interface to the Erlang code server process.
  - [erl_ddll](erl_ddll.md): Dynamic driver loader and linker.
  - [erl_debugger](erl_debugger.md): Erlang debugger support (EXPERIMENTAL).

- TRACING
  - [seq_trace](seq_trace.md): Sequential tracing of information transfers.
  - [trace](trace.md): The Erlang trace interface.

