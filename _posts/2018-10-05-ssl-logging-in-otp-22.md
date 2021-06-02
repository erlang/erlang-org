---
layout: post
title: TLS logging improvements in OTP 22
tags: ssl logger
author: Péter Dimitrov
---
Erlang/OTP 22 will be an important release for the ```ssl``` application. We are working on
several new features and improvements such as support for TLS 1.3, some of those are already
on the master branch. This blog post presents the new ssl debug logging built on the new
logger API.

## Usage

As the ```ssl``` application undergoes a lot of changes the release of the new logger API
presented the opportunity to level up its debug logging capabilities to be on par with
OpenSSL.

We have introduced a new option ```log_level``` that specifies the log level for the ```ssl```
application. It can take the following values (ordered by increasing verbosity level):
```emergency```, ```alert```, ```critical```, ```error```, ```warning```, ```notice```,
```info``` and ```debug```. At verbosity level ```notice``` and above error reports are
displayed in TLS. The level ```debug``` triggers verbose logging of TLS protocol messages
in a similar style as in OpenSSL.

The verbose debug logging can be turned on by two simple steps: the ```log_level``` shall
be set to ```debug``` and the logger shall be configured to enable ```debug``` logging
for the ssl application. The following code snippet is a sample module with a simple TLS server
and client:

```erlang
-module(ssltest).

-compile(export_all).

-define(PORT, 11000).

server() ->
    application:load(ssl),
    logger:set_application_level(ssl, debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    LOpts = [{certfile, "server.pem"},
             {keyfile, "server.key"},
             {versions, ['tlsv1.2']},
             {log_level, debug}
            ],
    {ok, LSock} = ssl:listen(Port, LOpts),
    {ok, CSock} = ssl:transport_accept(LSock),
    {ok, _} = ssl:handshake(CSock).

client() ->
    application:load(ssl),
    logger:set_application_level(ssl, debug),
    {ok, _} = application:ensure_all_started(ssl),
    Port = ?PORT,
    COpts = [{verify, verify_peer},
             {cacertfile, "ca.pem"},
             {versions, ['tlsv1.2']},
             {log_level, debug}
            ],
    {ok, Sock} = ssl:connect("localhost", Port, COpts).

```

Starting the server and client in their respective erlang shells produces the following
verbose logging of TLS protocol messages:


```
1> ssltest:server().
reading (238 bytes) TLS 1.2 Record Protocol, handshake
0000 - 16 03 03 00 e9 01 00 00  e5 03 03 5b ab 42 7a ee    ...........[.Bz.
0010 - 91 23 df 70 30 fb 41 b9  c5 14 79 d7 02 48 74 c9    .#.p0.A...y..Ht.
0020 - b9 a9 8f e0 e9 04 1a f9  a8 21 49 00 00 4a 00 ff    .........!I..J..
0030 - c0 2c c0 30 c0 24 c0 28  c0 2e c0 32 c0 26 c0 2a    .,.0.$.(...2.&.*
0040 - 00 9f 00 a3 00 6b 00 6a  c0 2b c0 2f c0 23 c0 27    .....k.j.+./.#.'
0050 - c0 2d c0 31 c0 25 c0 29  00 9e 00 a2 00 67 00 40    .-.1.%.).....g.@
0060 - c0 0a c0 14 00 39 00 38  c0 05 c0 0f c0 09 c0 13    .....9.8........
0070 - 00 33 00 32 c0 04 c0 0e  01 00 00 72 00 00 00 0e    .3.2.......r....
0080 - 00 0c 00 00 09 6c 6f 63  61 6c 68 6f 73 74 00 0a    .....localhost..
0090 - 00 3a 00 38 00 0e 00 0d  00 19 00 1c 00 0b 00 0c    .:.8............
00a0 - 00 1b 00 18 00 09 00 0a  00 1a 00 16 00 17 00 08    ................
00b0 - 00 06 00 07 00 14 00 15  00 04 00 05 00 12 00 13    ................
00c0 - 00 01 00 02 00 03 00 0f  00 10 00 11 00 0b 00 02    ................
00d0 - 01 00 00 0d 00 18 00 16  06 03 06 01 05 03 05 01    ................
00e0 - 04 03 04 01 03 03 03 01  02 03 02 01 02 02          ..............
<<< TLS 1.2 Handshake, ClientHello
[{client_version,{3,3}},
 {random,
     <<91,171,66,122,238,145,35,223,112,48,251,65,185,197,20,121,215,2,72,116,
       201,185,169,143,224,233,4,26,249,168,33,73>>},
 {session_id,<<>>},
 {cipher_suites,
     [<<0,255>>,
      <<"À,">>,<<"À0">>,<<"À$">>,<<"À(">>,<<"À.">>,<<"À2">>,<<"À&">>,<<"À*">>,
      <<0,159>>,
      <<0,163>>,
      <<0,107>>,
      <<0,106>>,
      <<"À+">>,<<"À/">>,<<"À#">>,<<"À'">>,<<"À-">>,<<"À1">>,<<"À%">>,<<"À)">>,
      <<0,158>>,
      <<0,162>>,
      <<0,103>>,
      <<0,64>>,
      <<"À\n">>,
      <<192,20>>,
      <<0,57>>,
      <<0,56>>,
      <<192,5>>,
      <<192,15>>,
      <<"À\t">>,
      <<192,19>>,
      <<0,51>>,
      <<0,50>>,
      <<192,4>>,
      <<192,14>>]},
 {compression_methods,[0]},
...
[Truncated for brevity]
```

This is not the final format as there are many ways to further improve the representation
of the handshake protocol messages such as converting the cipher suites to a human-readable
erlang representation.

As a comparison this is the debug output from an OpenSSL server when the same erlang client
connects to it:


```
$ /usr/bin/openssl s_server -accept 11000 -tls1_2 -cert server.pem -key server.key -msg -debug
Using default temp DH parameters
ACCEPT
read from 0x16f0040 [0x16f56b3] (5 bytes => 5 (0x5))
0000 - 16 03 03 00 a1                                    .....
<<< ??? [length 0005]
    16 03 03 00 a1
read from 0x16f0040 [0x16f56b8] (161 bytes => 161 (0xA1))
0000 - 01 00 00 9d 03 03 5b ac-a1 cc 20 4c 4d 52 d0 d4   ......[... LMR..
0010 - c8 fc dd 95 b0 fa 65 97-57 9e 44 aa dd 0e 46 10   ......e.W.D...F.
0020 - 6c 14 57 9c ce a0 00 00-04 00 ff c0 14 01 00 00   l.W.............
0030 - 70 00 2b 00 06 00 04 03-04 03 03 00 00 00 0e 00   p.+.............
0040 - 0c 00 00 09 6c 6f 63 61-6c 68 6f 73 74 00 0a 00   ....localhost...
0050 - 3a 00 38 00 0e 00 0d 00-19 00 1c 00 0b 00 0c 00   :.8.............
0060 - 1b 00 18 00 09 00 0a 00-1a 00 16 00 17 00 08 00   ................
0070 - 06 00 07 00 14 00 15 00-04 00 05 00 12 00 13 00   ................
0080 - 01 00 02 00 03 00 0f 00-10 00 11 00 0b 00 02 01   ................
0090 - 00 00 32 00 04 00 02 02-03 00 0d 00 04 00 02 02   ..2.............
00a0 - 01                                                .
<<< TLS 1.2 Handshake [length 00a1], ClientHello
    01 00 00 9d 03 03 5b ac a1 cc 20 4c 4d 52 d0 d4
    c8 fc dd 95 b0 fa 65 97 57 9e 44 aa dd 0e 46 10
    6c 14 57 9c ce a0 00 00 04 00 ff c0 14 01 00 00
    70 00 2b 00 06 00 04 03 04 03 03 00 00 00 0e 00
    0c 00 00 09 6c 6f 63 61 6c 68 6f 73 74 00 0a 00
    3a 00 38 00 0e 00 0d 00 19 00 1c 00 0b 00 0c 00
    1b 00 18 00 09 00 0a 00 1a 00 16 00 17 00 08 00
    06 00 07 00 14 00 15 00 04 00 05 00 12 00 13 00
    01 00 02 00 03 00 0f 00 10 00 11 00 0b 00 02 01
    00 00 32 00 04 00 02 02 03 00 0d 00 04 00 02 02
    01
...
[Truncated for brevity]
```

The verbose debug logging proved to be especially useful during the development of new
extensions as previously we had to use wireshark captures to validate TLS protocol
messages.

## Implementation

In the ```ssl``` application, we needed a way to handle two types of protocol
messages, tls_record and handshake, each with a custom formatter.

The most straightforward solution was to add a new handler instance to the logger with a
special formatter function that filters out all the "noise" coming from other
modules of the system.

The handler itself could reuse the standard handler for logger, ```logger_std_h```, as it
could print logs to ```standard_io```. You can add multiple standard handler instances to
logger if your application requires it.

```erlang
logger:add_handler(ssl_handler, logger_std_h, Config),
```

The new ssl_handler is configured with a formatter that is implemented by the ```ssl_logger```
module.

```erlang
Config = #{level => debug,
           filter_default => stop,
           formatter => {ssl_logger, #{}}},
```

Handler filter level is set to ```debug``` with ```stop``` as the default filter action. We also
need a filter that lets the log events pass to the formatter if the source of the log event is the
ssl application. In other words, we need a domain filter with the action ```log``` on all sub-domains
matching ```[otp,ssl]```.

```erlang
Filter = {fun logger_filters:domain/2,{log,sub,[otp,ssl]}},
```

Putting it all together we get the following function.

```erlang
start_logger() ->
    Config = #{level => debug,
               filter_default => stop,
               formatter => {ssl_logger, #{}}},
    Filter = {fun logger_filters:domain/2,{log,sub,[otp,ssl]}},
    logger:add_handler(ssl_handler, logger_std_h, Config),
    logger:add_handler_filter(ssl_handler, filter_non_ssl, Filter).
```

The function ```format``` is called in ssl_logger when an event gets through all the filters:

```erlang
format(#{level:= _Level, msg:= {report, Msg}, meta:= _Meta},
       _Config0) ->
     #{direction := Direction,
       protocol := Protocol,
       message := BinMsg0} = Msg,
    case Protocol of
        'tls_record' ->
            BinMsg = lists:flatten(BinMsg0),
            format_tls_record(Direction, BinMsg);
        'handshake' ->
            format_handshake(Direction, BinMsg0);
        _Other ->
            []
    end.
```

There are two more helper functions that wrap around the logging macros. They were added in order
to be able to set logging level per TLS session.

```erlang
debug(Level, Report, Meta) ->
    case logger:compare_levels(Level, debug) of
        lt ->
            ?LOG_DEBUG(Report, Meta);
        eq ->
            ?LOG_DEBUG(Report, Meta);
        _ ->
            ok
    end.

notice(Level, Report) ->
    case logger:compare_levels(Level, notice) of
        lt ->
            ?LOG_NOTICE(Report);
        eq ->
            ?LOG_NOTICE(Report);
        _ ->
            ok
    end.
```

To print a log event, the above functions are called with the configured ssl log level and
the domain parameter.

```erlang
ssl_logger:debug(Opts#ssl_options.log_level,
	         Report,
		 #{domain => [otp,ssl,handshake]}),
```

Those who are interested in the current state of development can already play with the
```'tlsv1.3'``` atom in the ```versions``` option.
