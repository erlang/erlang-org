# stdlib v8.0.2-rc0 - API Reference

## Modules

- [argparse](argparse.md): Command line arguments parser.
- [calendar](calendar.md): Local and universal time, day of the week, date and time conversions.
- [erl_error](erl_error.md): This module provides functions for pretty-printing errors and exceptions. It is
used by both the `m:shell` and by `m:proc_lib` to print exceptions.
- [escript](escript.md): This module provides functions to create and inspect escripts.
- [json](json.md): A library for encoding and decoding JSON.
- [math](math.md): Mathematical functions.
- [peer](peer.md): <!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2022-2026. All Rights Reserved.
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
Start and control linked Erlang nodes.
- [rand](rand.md): Pseudo random number generation
- [timer](timer.md): Timer functions.
- [win32reg](win32reg.md): Provides access to the registry on Windows.

- BASIC DATA TYPES
  - [binary](binary.md): Library for handling binary data.
  - [lists](lists.md): List processing functions.
  - [maps](maps.md): Maps processing functions.
  - [records](records.md): Native records processing functions.

- HIGH-LEVEL DATA STRUCTURES
  - [array](array.md): Functional, extendable arrays.
  - [dict](dict.md): A Key-value dictionary.
  - [digraph](digraph.md): This module provides a version of labeled directed graphs ("digraphs").
  - [digraph_utils](digraph_utils.md): This module provides algorithms based on depth-first traversal of directed
graphs.
  - [gb_sets](gb_sets.md): Sets represented by general balanced trees.
  - [gb_trees](gb_trees.md): General balanced trees.
  - [graph](graph.md): A functional implementation of labeled directed graphs.
  - [orddict](orddict.md): Key-value dictionary as ordered list.
  - [ordsets](ordsets.md): Functions for manipulating sets as ordered lists.
  - [proplists](proplists.md): Support functions for property lists.
  - [queue](queue.md): Abstract data type for FIFO queues.
  - [sets](sets.md): Sets are collections of elements with no duplicate elements.
  - [sofs](sofs.md): <!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2001-2026. All Rights Reserved.
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
Functions for manipulating sets of sets.

- STRINGS
  - [base64](base64.md): Provides base64 encode and decode, see
[RFC 2045](https://www.ietf.org/rfc/rfc2045.txt).
  - [re](re.md): <!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
This module contains regular expression matching functions for strings and
binaries.
  - [string](string.md): String processing functions.
  - [unicode](unicode.md): Functions for converting and classifying Unicode characters.
  - [uri_string](uri_string.md): URI processing functions.

- PROCESSES
  - [gen_event](gen_event.md): Generic event handling behavior.
  - [gen_fsm](gen_fsm.md): Deprecated and replaced by `m:gen_statem` in OTP 20.
  - [gen_server](gen_server.md): Generic server behavior.
  - [gen_statem](gen_statem.md): Generic state machine behavior.
  - [log_mf_h](log_mf_h.md): An event handler that logs events to disk.
  - [pool](pool.md): Load distribution facility.
  - [proc_lib](proc_lib.md): Functions for asynchronous and synchronous start of processes adhering to the
OTP design principles.
  - [supervisor](supervisor.md): Generic supervisor behavior.
  - [supervisor_bridge](supervisor_bridge.md): Generic supervisor bridge behavior.
  - [sys](sys.md): A functional interface to system messages.

- IO
  - [file_sorter](file_sorter.md): File sorter.
  - [filelib](filelib.md): File utilities, such as wildcard matching of filenames.
  - [filename](filename.md): Filename manipulation functions.
  - [io](io.md): Standard I/O server interface functions.
  - [io_ansi](io_ansi.md): Controlling the terminal using virtual terminal sequences (aka [ANSI escape codes]).
  - [io_lib](io_lib.md): I/O library functions.

- TERM STORAGE
  - [dets](dets.md): A disk-based term storage.
  - [ets](ets.md): Built-in term storage.
  - [qlc](qlc.md): <!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2026. All Rights Reserved.
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
This module provides a query interface to [Mnesia](`m:mnesia`), [ETS](`m:ets`),
[Dets](`m:dets`), and other data structures that provide an iterator style
traversal of objects.

- CODE
  - [beam_lib](beam_lib.md): This module provides an interface to files created by the BEAM Compiler ("BEAM
files").
  - [epp](epp.md): An Erlang code preprocessor.
  - [erl_anno](erl_anno.md): Abstract datatype for the annotations of the Erlang Compiler.
  - [erl_eval](erl_eval.md): The Erlang meta interpreter.
  - [erl_expand_records](erl_expand_records.md): This module expands records in a module.
  - [erl_features](erl_features.md): This module contains functions for supporting features that can be
enabled/disabled in Erlang.
  - [erl_id_trans](erl_id_trans.md): This module performs an identity parse transformation of Erlang code.
  - [erl_internal](erl_internal.md): Internal Erlang definitions.
  - [erl_lint](erl_lint.md): The Erlang code linter.
  - [erl_parse](erl_parse.md): This module is the basic Erlang parser that converts tokens into the abstract
form of either forms (that is, top-level constructs), expressions, or terms.
  - [erl_pp](erl_pp.md): The Erlang pretty printer.
  - [erl_scan](erl_scan.md): The Erlang token scanner.
  - [ms_transform](ms_transform.md): <!--
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
A parse transformation that translates fun syntax into match specifications.

- COMPRESSION
  - [erl_tar](erl_tar.md): Unix 'tar' utility for reading and writing tar archives.
  - [zip](zip.md): Utility for reading and creating 'zip' archives.
  - [zstd](zstd.md): Zstandard compression interface.

- SHELL
  - [c](c.md): Command line interface module.
  - [edlin](edlin.md): Line and input interpretter for the erlang shell.
  - [edlin_expand](edlin_expand.md): Shell expansion and formatting of expansion suggestions.
  - [shell](shell.md): <!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2026. All Rights Reserved.
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
The Erlang shell.
  - [shell_default](shell_default.md): Customizing the Erlang environment.
  - [shell_docs](shell_docs.md): Functions used to render [EEP-48](`e:kernel:eep48_chapter.md`) style documentation for a shell.

- Deprecated
  - [random](random.md): Pseudo-random number generation.
  - [slave](slave.md): This module provides functions for starting Erlang slave nodes.

