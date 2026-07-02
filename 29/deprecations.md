<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
# Deprecations

## Introduction

This document lists all deprecated functionality in Erlang/OTP. For more
information regarding the strategy regarding deprecations see the documentation
of
[Support, Compatibility, Deprecations, and Removal](`e:system:misc.md#deprecation`).

## OTP 29

<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

SPDX-FileCopyrightText: Copyright (C) 2025 Richard Carlsson <carlsson.richard@gmail.com>
Copyright Ericsson AB 2026. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->

### LEGACY (D)TLS Versions TLS-1.0, TLS-1.1 and DTLS-1.0

TLS-1.0, TLS-1.1 and DTLS-1.0 are effectively end-of-life, with major
industry players disabling them by late 2025 due to security
vulnerabilities, these legacy protocol versions have not been
supported by default for many years and are hereby formally
deprecated.

### LEGACY TLS Option Handling in Erlang Distribution over TLS

All TLS options specified for Erlang distribution as `ERL_FLAGS` on
the format `{ssl_dist_opt, Value}`.  After OTP 20 only the
`ssl_dist_optfile` flag should be used to configure TLS for Erlang
distribution and most options can only be configured that way anyway.

### SSH zlib Compression Algorithm

The use of the `zlib` compression algorithm in the SSH application is
deprecated. Use `none` or `zlib@openssh.com` instead. The `zlib`
algorithm has not been included in the default SSH algorithms for some
time and support for it in SSH is scheduled for removal in OTP 30.0.

### Functions Deprecated in OTP 29

-   `ct_ftp:_/_`  (Legacy protocol support will be dropped in OTP-30)
-   `ftp:_/_`  (Legacy protocol support will be dropped in OTP-30, use more modern approach for file transfer as for instance SFTP (SSH File Transfer Protocol).)
-   Undocumented function lists:zf/2  (use filtermap/2 instead)
-   `mod_actions:_/_`  (use 'mod_esi' for dynamic page generation)
-   `mod_cgi:_/_`  (use 'mod_esi' for dynamic page generation)
-   `odbc:_/_`  (Legacy protocol support will be dropped in OTP-30, does not really provide backend transparency and known usage is low.)

## OTP 28

<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

%CopyrightEnd%
-->

### Distribution Control Messages

#### `ALIAS_SEND` and `ALIAS_SEND_TT`

The [`ALIAS_SEND`](`e:erts:erl_dist_protocol.md#ALIAS_SEND`) and
[`ALIAS_SEND_TT`](`e:erts:erl_dist_protocol.md#ALIAS_SEND_TT`) distribution
control messages are as of OTP 28 deprecated and have been scheduled for removal in
OTP 30. The support for these control messages are indicated by the
[`DFLAG_ALIAS`](`e:erts:erl_dist_protocol.md#DFLAG_ALIAS`) distribution flag.

The `ALIAS_SEND` and `ALIAS_SEND_TT` control messages are as of OTP 28 replaced
by the [`ALTACT_SIG_SEND`](`e:erts:erl_dist_protocol.md#ALTACT_SIG_SEND`)
control message. Support for the `ALTACT_SIG_SEND` control message is indicated
by the [`DFLAG_ALTACT_SIG`](`e:erts:erl_dist_protocol.md#DFLAG_ALTACT_SIG`)
distribution flag.

### Functions Deprecated in OTP 28

-   `crypto:enable_fips_mode/1`  (use config parameter fips_mode)
-   `crypto:start/0`  (use application:start(crypto) instead)
-   `crypto:stop/0`  (use application:stop(crypto) instead)

## OTP 27

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### Archives

The following features for archives are deprecated:

* Using archives for packaging a single application or parts of a
  single application into an archive file that is included in the code
  path.

* All functionality to handle archives in module
  [`erl_prim_loader`](https://www.erlang.org/doc/man/erl_prim_loader).

* The `-code_path_choice` flag for `erl`.

Using a single archive file for holding BEAM files and other data
files in an Escript is **not** deprecated. However, to access files in
the archive the `escript:extract/2` function has to be used.

### erl flags

The following erl flags are deprecated:

* `-epmd_module Module` - deprecated in favour of the `kernel` application
  parameter `epmd_module`.

* `-erl_epmd_port Port` - deprecated in favour of the `kernel` application
  parameter `erl_epmd_node_listen_port`.

### Functions Deprecated in OTP 27

-   `code:lib_dir/2`  (this functionality will be removed in a future release)
-   `ssl:prf/5`  (Use export_key_materials/4 instead. Note that in OTP 28 the 'testing' way of calling this function will no longer be supported.)

## OTP 26

### Functions Deprecated in OTP 26

-   `dbg:stop_clear/0`  (use dbg:stop/0 instead)
-   `disk_log:inc_wrap_file/1`  (use disk_log:next_file/1 instead)

## OTP 25

### Functions Deprecated in OTP 25

-   `ct_slave:_/_`  (use ?CT_PEER(), or the 'peer' module instead)
-   `slave:_/_`  (use the 'peer' module instead)

## OTP 24

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### Erlang Distribution Without Large Node Container Support

Communication over the Erlang distribution without support for large
[node container data types (version 4)](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`)
is as of OTP 24 deprecated and is scheduled for removal in OTP 26. That is, as
of OTP 26, support for large node container data types will become mandatory.

### Old Link Protocol

The old link protocol used when communicating over the Erlang distribution is as
of OTP 24 deprecated and support for it is scheduled for removal in OTP 26. As
of OTP 26, the
[new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) will become
mandatory. That is, Erlang nodes will then refuse to connect to nodes not
implementing the new link protocol. If you implement the Erlang distribution
yourself, you are, however, encouraged to implement the new link protocol as
soon as possible since the old protocol can cause links to enter an inconsistent
state.

### ?NO_APP macro

The ?NO_APP macro in the edoc include file `edoc_doclet.hrl` has been
deprecated.

### Functions Deprecated in OTP 24

-   `erlang:phash/2` (use erlang:phash2/2 instead)
-   `zlib:adler32/2` (use erlang:adler32/1 instead)
-   `zlib:adler32/3` (use erlang:adler32/2 instead)
-   `zlib:adler32_combine/4` (use erlang:adler_combine/3 instead)
-   `zlib:crc32/1` (use erlang:crc32/1 on the uncompressed data instead)
-   `zlib:crc32/2` (use erlang:crc32/1 instead)
-   `zlib:crc32/3` (use erlang:crc32/2 instead)
-   `zlib:crc32_combine/4` (use erlang:crc32_combine/3 instead)
-   `zlib:getBufSize/1` (this function will be removed in a future release)
-   `zlib:inflateChunk/1` (use safeInflate/2 instead)
-   `zlib:inflateChunk/2` (use safeInflate/2 instead)
-   `zlib:setBufSize/2` (this function will be removed in a future release)

### Functions Deprecated in OTP 24

-   `erlang:phash/2`  (use erlang:phash2/2 instead)

## OTP 23

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### Crypto Old API

The [Old API](`e:crypto:new_api.md#the-old-api`) is deprecated as of OTP 23 and
has been [removed in OTP 24](removed.md#otp-24).

For replacement functions see the [New API](`e:crypto:new_api.md#the-new-api`).

### http_uri

Since OTP 21 the recommended module to handle URIs is `m:uri_string`. The module
http_uri does not provide a implementation that satisfies the RFC.

### ssh

The public key algorithm `'ssh-rsa` is regarded as insecure due to its usage of
SHA1, and is therefore deprecated. It will not be available by default from
OTP-24.

The public key algorithm `'ssh-dss` is regarded as insecure due to its usage of
SHA1 and its short key length, and is therefore deprecated. It is not available
by default from OTP-23.

### Distributed Disk Logs

As of OTP 23, the distributed `m:disk_log` feature has been deprecated and it
has also been [removed in OTP 24](removed.md#otp-24).

### erl_interface registry

As of OTP 23, the `registry` functionality part of `erl_interface` has been
deprecated and it has also been [removed in OTP 24](removed.md#otp-24).

### Functions Deprecated in OTP 23

-   `http_uri:decode/1` (use uri_string:unquote function instead)
-   `http_uri:encode/1` (use uri_string:quote function instead)
-   `httpd:parse_query/1` (use uri_string:dissect_query/1 instead)

### Functions Deprecated in OTP 23

-   `http_uri:decode/1`  (use uri_string:unquote function instead)
-   `http_uri:encode/1`  (use uri_string:quote function instead)
-   `httpd:parse_query/1`  (use uri_string:dissect_query/1 instead)

## OTP 22

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### VxWorks Support

Some parts of OTP has had limited VxWorks support, such as for example
[`erl_interface`](`e:erl_interface:index.html`). This support is as of OTP 22
formally deprecated and has also been [removed in OTP 23](removed.md#otp-23).

### Legacy parts of erl_interface

The old legacy [`erl_interface`](`e:erl_interface:index.html`) library
(functions with prefix `erl_`) is deprecated as of OTP 22. These parts of
`erl_interface` has been informally deprecated for a very long time. You
typically want to replace the usage of the `erl_interface` library with the use
of the `ei` library which also is part of the `erl_interface` application. The
old legacy [`erl_interface`](`e:erl_interface:index.html`) library has also been
[removed in OTP 23](removed.md#otp-23).

### System Events

The format of "System Events" as defined in the man page for `m:sys` has been
clarified and cleaned up. Due to this, code that relied on the internal badly
documented previous (before this change) format of OTP's "System Events", needs
to be changed.

In the wake of this the function `sys:get_debug/3` that returns data with
undocumented and internal format (and therefore is practically useless) has been
deprecated, and a new function `sys:get_log/1` has been added, that hopefully
does what the deprecated function was intended for.

### Functions Deprecated in OTP 22

-   `net:broadcast/3` (use rpc:eval_everywhere/3 instead)
-   `net:call/4` (use rpc:call/4 instead)
-   `net:cast/4` (use rpc:cast/4 instead)
-   `net:ping/1` (use net_adm:ping/1 instead)
-   `net:sleep/1` (use 'receive after T -> ok end' instead)
-   `sys:get_debug/3` (incorrectly documented and only for internal use. Can
    often be replaced with sys:get_log/1)

### Functions Deprecated in OTP 22

-   `net:broadcast/3`  (use rpc:eval_everywhere/3 instead)
-   `net:call/4`  (use rpc:call/4 instead)
-   `net:cast/4`  (use rpc:cast/4 instead)
-   `net:ping/1`  (use net_adm:ping/1 instead)
-   `net:sleep/1`  (use 'receive after T -> ok end' instead)
-   `sys:get_debug/3`  (incorrectly documented and only for internal use. Can often be replaced with sys:get_log/1)

## OTP 20

### Functions Deprecated in OTP 20

-   `crypto:rand_uniform/2`  (use strong_rand_range/1 instead)
-   `gen_fsm:_/_`  (use the 'gen_statem' module instead)

## OTP 19

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### SSL/TLS

For security reasons SSL-3.0 is no longer supported by default, but can be
configured.

### Functions Deprecated in OTP 19

-   `queue:lait/1`  (use queue:liat/1 instead)
-   `random:_/_`  (use the 'rand' module instead)

## OTP 18

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### erlang:now/0

New time functionality and a new time API was introduced. For more information
see the [Time and Time Correction](`e:erts:time_correction.md`) chapter in the
ERTS User's guide and specifically the
[Dos and Donts](`e:erts:time_correction.md#Dos_and_Donts`) section on how to
replace usage of `erlang:now/0`.

### httpd_conf module

API functions in the module `httpd_conf` was deprecated in favor of standard
modules such as `lists`, `string`, `filelib`, and `erlang`.

### Functions Deprecated in OTP 18

-   `erlang:now/0`  (see the "Time and Time Correction in Erlang" chapter of the ERTS User's Guide for more information)

## OTP 16

### Functions Deprecated in OTP 16

-   `wxCalendarCtrl:enableYearChange/1`  (not available in wxWidgets-2.9 and later)
-   `wxCalendarCtrl:enableYearChange/2`  (not available in wxWidgets-2.9 and later)

## OTP 12

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
### inets - httpd Apache config files

A new config file format was introduced.

### Functions Deprecated in OTP 12

-   `auth:cookie/0`  (use erlang:get_cookie/0 instead)
-   `auth:cookie/1`  (use erlang:set_cookie/2 instead)
-   `auth:is_auth/1`  (use net_adm:ping/1 instead)
-   `auth:node_cookie/_`  (use erlang:set_cookie/2 and net_adm:ping/1 instead)
-   `calendar:local_time_to_universal_time/1`  (use calendar:local_time_to_universal_time_dst/1 instead)

