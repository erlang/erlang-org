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
# Scheduled for Removal

## Introduction

This document list all functionality in Erlang/OTP that currently are scheduled
for removal. For more information regarding the strategy regarding removal of
functionality see the documentation of
[Support, Compatibility, Deprecations, and Removal](`e:system:misc.md#removal`).

## OTP 31

### Functions Scheduled for Removal in OTP 31

-   `ct_slave:_/_`  (use ?CT_PEER(), or the 'peer' module instead)
-   `slave:_/_`  (use the 'peer' module instead)

## OTP 30

<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2026. All Rights Reserved.
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

### Archives

The following features of archives will be removed:

* Using archives for packaging a single application or parts of a single application
  into an archive file that is included in the code path.

* All functionality to handle archives in module `m:erl_prim_loader`.

* The `-code_path_choice` flag for `erl`.

The functionality to use a single archive file in Escripts is **not**
deprecated and will continue to work.  However, to access files in the
archive, the `escript:extract/2` function has to be used.

## OTP 29

### Functions Scheduled for Removal in OTP 29

-   `mnesia_registry:create_table/_`  (use mnesia:create_table/2 instead)

## OTP 28

### Functions Scheduled for Removal in OTP 28

-   `disk_log:inc_wrap_file/1`  (use disk_log:next_file/1 instead)

## OTP 27

### Functions Scheduled for Removal in OTP 27

-   `dbg:stop_clear/0`  (use dbg:stop/0 instead)

