sidebarNodes={"modules":[{"id":"diameter","deprecated":false,"group":"","title":"diameter","sections":[{"id":"DATA TYPES","anchor":"module-data-types"},{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Types","nodes":[{"id":"Address/0","deprecated":false,"title":"'Address'()","anchor":"t:Address/0"},{"id":"app_alias/0","deprecated":false,"title":"app_alias()","anchor":"t:app_alias/0"},{"id":"app_module/0","deprecated":false,"title":"app_module()","anchor":"t:app_module/0"},{"id":"application_opt/0","deprecated":false,"title":"application_opt()","anchor":"t:application_opt/0"},{"id":"call_opt/0","deprecated":false,"title":"call_opt()","anchor":"t:call_opt/0"},{"id":"capability/0","deprecated":false,"title":"capability()","anchor":"t:capability/0"},{"id":"common_opt/0","deprecated":false,"title":"common_opt()","anchor":"t:common_opt/0"},{"id":"DiameterIdentity/0","deprecated":false,"title":"'DiameterIdentity'()","anchor":"t:DiameterIdentity/0"},{"id":"DiameterURI/0","deprecated":false,"title":"'DiameterURI'()","anchor":"t:DiameterURI/0"},{"id":"decode_format/0","deprecated":false,"title":"decode_format()","anchor":"t:decode_format/0"},{"id":"Enumerated/0","deprecated":false,"title":"'Enumerated'()","anchor":"t:Enumerated/0"},{"id":"elapsed_time/0","deprecated":false,"title":"elapsed_time()","anchor":"t:elapsed_time/0"},{"id":"eval/0","deprecated":false,"title":"eval()","anchor":"t:eval/0"},{"id":"evaluable/0","deprecated":false,"title":"evaluable()","anchor":"t:evaluable/0"},{"id":"Float32/0","deprecated":false,"title":"'Float32'()","anchor":"t:Float32/0"},{"id":"Float64/0","deprecated":false,"title":"'Float64'()","anchor":"t:Float64/0"},{"id":"Grouped/0","deprecated":false,"title":"'Grouped'()","anchor":"t:Grouped/0"},{"id":"Integer32/0","deprecated":false,"title":"'Integer32'()","anchor":"t:Integer32/0"},{"id":"Integer64/0","deprecated":false,"title":"'Integer64'()","anchor":"t:Integer64/0"},{"id":"IPFilterRule/0","deprecated":false,"title":"'IPFilterRule'()","anchor":"t:IPFilterRule/0"},{"id":"message_length/0","deprecated":false,"title":"message_length()","anchor":"t:message_length/0"},{"id":"OctetString/0","deprecated":false,"title":"'OctetString'()","anchor":"t:OctetString/0"},{"id":"peer_filter/0","deprecated":false,"title":"peer_filter()","anchor":"t:peer_filter/0"},{"id":"peer_ref/0","deprecated":false,"title":"peer_ref()","anchor":"t:peer_ref/0"},{"id":"QoSFilterRule/0","deprecated":false,"title":"'QoSFilterRule'()","anchor":"t:QoSFilterRule/0"},{"id":"remotes/0","deprecated":false,"title":"remotes()","anchor":"t:remotes/0"},{"id":"restriction/0","deprecated":false,"title":"restriction()","anchor":"t:restriction/0"},{"id":"sequence/0","deprecated":false,"title":"sequence()","anchor":"t:sequence/0"},{"id":"service_name/0","deprecated":false,"title":"service_name()","anchor":"t:service_name/0"},{"id":"service_opt/0","deprecated":false,"title":"service_opt()","anchor":"t:service_opt/0"},{"id":"strict_arities/0","deprecated":false,"title":"strict_arities()","anchor":"t:strict_arities/0"},{"id":"Time/0","deprecated":false,"title":"'Time'()","anchor":"t:Time/0"},{"id":"transport_opt/0","deprecated":false,"title":"transport_opt()","anchor":"t:transport_opt/0"},{"id":"transport_pred/0","deprecated":false,"title":"transport_pred()","anchor":"t:transport_pred/0"},{"id":"transport_ref/0","deprecated":false,"title":"transport_ref()","anchor":"t:transport_ref/0"},{"id":"Unsigned32/0","deprecated":false,"title":"'Unsigned32'()","anchor":"t:Unsigned32/0"},{"id":"Unsigned64/0","deprecated":false,"title":"'Unsigned64'()","anchor":"t:Unsigned64/0"},{"id":"UTF8String/0","deprecated":false,"title":"'UTF8String'()","anchor":"t:UTF8String/0"}],"key":"types"},{"name":"Functions","nodes":[{"id":"add_transport/2","deprecated":false,"title":"add_transport(SvcName, Transport)","anchor":"add_transport/2"},{"id":"call/4","deprecated":false,"title":"call(SvcName, App, Request, CallOpts)","anchor":"call/4"},{"id":"origin_state_id/0","deprecated":false,"title":"origin_state_id()","anchor":"origin_state_id/0"},{"id":"remove_transport/2","deprecated":false,"title":"remove_transport(SvcName, Pred)","anchor":"remove_transport/2"},{"id":"service_info/2","deprecated":false,"title":"service_info(SvcName, Option)","anchor":"service_info/2"},{"id":"services/0","deprecated":false,"title":"services()","anchor":"services/0"},{"id":"session_id/1","deprecated":false,"title":"session_id(Ident)","anchor":"session_id/1"},{"id":"start/0","deprecated":false,"title":"start()","anchor":"start/0"},{"id":"start_service/2","deprecated":false,"title":"start_service(SvcName, Opts)","anchor":"start_service/2"},{"id":"stop/0","deprecated":false,"title":"stop()","anchor":"stop/0"},{"id":"stop_service/1","deprecated":false,"title":"stop_service(SvcName)","anchor":"stop_service/1"},{"id":"subscribe/1","deprecated":false,"title":"subscribe(SvcName)","anchor":"subscribe/1"},{"id":"unsubscribe/1","deprecated":false,"title":"unsubscribe(SvcName)","anchor":"unsubscribe/1"},{"id":"which_connections/0","deprecated":false,"title":"which_connections()","anchor":"which_connections/0"},{"id":"which_connections/1","deprecated":false,"title":"which_connections(SvcName)","anchor":"which_connections/1"},{"id":"which_transports/0","deprecated":false,"title":"which_transports()","anchor":"which_transports/0"},{"id":"which_transports/1","deprecated":false,"title":"which_transports(SvcName)","anchor":"which_transports/1"},{"id":"which_watchdogs/0","deprecated":false,"title":"which_watchdogs()","anchor":"which_watchdogs/0"},{"id":"which_watchdogs/1","deprecated":false,"title":"which_watchdogs(SvcName)","anchor":"which_watchdogs/1"}],"key":"functions"}]},{"id":"diameter_app","deprecated":false,"group":"","title":"diameter_app","sections":[],"nodeGroups":[{"name":"Types","nodes":[{"id":"capabilities/0","deprecated":false,"title":"capabilities()","anchor":"t:capabilities/0"},{"id":"message/0","deprecated":false,"title":"message()","anchor":"t:message/0"},{"id":"packet/0","deprecated":false,"title":"packet()","anchor":"t:packet/0"},{"id":"peer/0","deprecated":false,"title":"peer()","anchor":"t:peer/0"},{"id":"peer_ref/0","deprecated":false,"title":"peer_ref()","anchor":"t:peer_ref/0"},{"id":"state/0","deprecated":false,"title":"state()","anchor":"t:state/0"}],"key":"types"},{"name":"Callbacks","nodes":[{"id":"handle_answer/4","deprecated":false,"title":"handle_answer(Packet, Request, SvcName, Peer)","anchor":"c:handle_answer/4"},{"id":"handle_error/4","deprecated":false,"title":"handle_error(Reason, Request, SvcName, Peer)","anchor":"c:handle_error/4"},{"id":"handle_request/3","deprecated":false,"title":"handle_request(Packet, SvcName, Peer)","anchor":"c:handle_request/3"},{"id":"peer_down/3","deprecated":false,"title":"peer_down(SvcName, Peer, State)","anchor":"c:peer_down/3"},{"id":"peer_up/3","deprecated":false,"title":"peer_up(SvcName, Peer, State)","anchor":"c:peer_up/3"},{"id":"pick_peer/4","deprecated":false,"title":"pick_peer(LocalCandidates, RemoteCandidates, SvcName, State)","anchor":"c:pick_peer/4"},{"id":"prepare_request/3","deprecated":false,"title":"prepare_request(Packet, SvcName, Peer)","anchor":"c:prepare_request/3"},{"id":"prepare_retransmit/3","deprecated":false,"title":"prepare_retransmit(Packet, SvcName, Peer)","anchor":"c:prepare_retransmit/3"}],"key":"callbacks"}]},{"id":"diameter_codec","deprecated":false,"group":"","title":"diameter_codec","sections":[{"id":"DATA TYPES","anchor":"module-data-types"},{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Types","nodes":[{"id":"dictionary/0","deprecated":false,"title":"dictionary()","anchor":"t:dictionary/0"},{"id":"message/0","deprecated":false,"title":"message()","anchor":"t:message/0"},{"id":"packet/0","deprecated":false,"title":"packet()","anchor":"t:packet/0"},{"id":"record/0","deprecated":false,"title":"record()","anchor":"t:record/0"}],"key":"types"},{"name":"Functions","nodes":[{"id":"decode/2","deprecated":false,"title":"decode(Mod, Bin)","anchor":"decode/2"},{"id":"encode/2","deprecated":false,"title":"encode(Mod, Msg)","anchor":"encode/2"}],"key":"functions"}]},{"id":"diameter_make","deprecated":false,"group":"","title":"diameter_make","sections":[{"id":"BUGS","anchor":"module-bugs"},{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Types","nodes":[{"id":"dict/0","deprecated":false,"title":"dict()","anchor":"t:dict/0"},{"id":"opt/0","deprecated":false,"title":"opt()","anchor":"t:opt/0"},{"id":"parsed/0","deprecated":false,"title":"parsed()","anchor":"t:parsed/0"}],"key":"types"},{"name":"Functions","nodes":[{"id":"codec/2","deprecated":false,"title":"codec(File, Opts)","anchor":"codec/2"},{"id":"flatten/1","deprecated":false,"title":"flatten(Parsed)","anchor":"flatten/1"},{"id":"format/1","deprecated":false,"title":"format(Parsed)","anchor":"format/1"},{"id":"format_error/1","deprecated":false,"title":"format_error(Reason)","anchor":"format_error/1"}],"key":"functions"}]},{"id":"diameter_sctp","deprecated":false,"group":"","title":"diameter_sctp","sections":[{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Types","nodes":[{"id":"connect_option/0","deprecated":false,"title":"connect_option()","anchor":"t:connect_option/0"},{"id":"listen_option/0","deprecated":false,"title":"listen_option()","anchor":"t:listen_option/0"},{"id":"match/0","deprecated":false,"title":"match()","anchor":"t:match/0"},{"id":"option/0","deprecated":false,"title":"option()","anchor":"t:option/0"}],"key":"types"},{"name":"Functions","nodes":[{"id":"start/3","deprecated":false,"title":"start(TypeRef, Svc, Options)","anchor":"start/3"}],"key":"functions"}]},{"id":"diameter_service","deprecated":false,"group":"","title":"diameter_service","sections":[],"nodeGroups":[{"name":"Types","nodes":[{"id":"wd_state/0","deprecated":false,"title":"wd_state()","anchor":"t:wd_state/0"}],"key":"types"}]},{"id":"diameter_tcp","deprecated":false,"group":"","title":"diameter_tcp","sections":[{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Types","nodes":[{"id":"connect_option/0","deprecated":false,"title":"connect_option()","anchor":"t:connect_option/0"},{"id":"listen_option/0","deprecated":false,"title":"listen_option()","anchor":"t:listen_option/0"},{"id":"match/0","deprecated":false,"title":"match()","anchor":"t:match/0"},{"id":"option/0","deprecated":false,"title":"option()","anchor":"t:option/0"}],"key":"types"},{"name":"Functions","nodes":[{"id":"start/3","deprecated":false,"title":"start/3","anchor":"start/3"}],"key":"functions"}]},{"id":"diameter_transport","deprecated":false,"group":"","title":"diameter_transport","sections":[{"id":"DATA TYPES","anchor":"module-data-types"},{"id":"MESSAGES","anchor":"module-messages"},{"id":"SEE ALSO","anchor":"module-see-also"}],"nodeGroups":[{"name":"Callbacks","nodes":[{"id":"start/3","deprecated":false,"title":"start/3","anchor":"c:start/3"}],"key":"callbacks"}]}],"extras":[{"id":"api-reference","group":"","title":"API Reference","headers":[{"id":"Modules","anchor":"modules"}]},{"id":"notes","group":"","title":"Release Notes","headers":[{"id":"diameter 2.4.1","anchor":"diameter-2-4-1"},{"id":"diameter 2.4","anchor":"diameter-2-4"},{"id":"diameter 2.3.2.2","anchor":"diameter-2-3-2-2"},{"id":"diameter 2.3.2.1","anchor":"diameter-2-3-2-1"},{"id":"diameter 2.3.2","anchor":"diameter-2-3-2"},{"id":"diameter 2.3.1","anchor":"diameter-2-3-1"},{"id":"diameter 2.3","anchor":"diameter-2-3"},{"id":"diameter 2.2.7.2","anchor":"diameter-2-2-7-2"},{"id":"diameter 2.2.7.1","anchor":"diameter-2-2-7-1"},{"id":"diameter 2.2.7","anchor":"diameter-2-2-7"},{"id":"diameter 2.2.6","anchor":"diameter-2-2-6"},{"id":"diameter 2.2.5","anchor":"diameter-2-2-5"},{"id":"diameter 2.2.4","anchor":"diameter-2-2-4"},{"id":"diameter 2.2.3","anchor":"diameter-2-2-3"},{"id":"diameter 2.2.2","anchor":"diameter-2-2-2"},{"id":"diameter 2.2.1","anchor":"diameter-2-2-1"},{"id":"diameter 2.2","anchor":"diameter-2-2"},{"id":"diameter 2.1.6","anchor":"diameter-2-1-6"},{"id":"diameter 2.1.5","anchor":"diameter-2-1-5"},{"id":"diameter 2.1.4.1","anchor":"diameter-2-1-4-1"},{"id":"diameter 2.1.4","anchor":"diameter-2-1-4"},{"id":"diameter 2.1.3","anchor":"diameter-2-1-3"},{"id":"diameter 2.1.2","anchor":"diameter-2-1-2"},{"id":"diameter 2.1.1","anchor":"diameter-2-1-1"},{"id":"diameter 2.1","anchor":"diameter-2-1"},{"id":"diameter 2.0","anchor":"diameter-2-0"},{"id":"diameter 1.12.2","anchor":"diameter-1-12-2"},{"id":"diameter 1.12.1","anchor":"diameter-1-12-1"},{"id":"diameter 1.12","anchor":"diameter-1-12"},{"id":"diameter 1.11.2","anchor":"diameter-1-11-2"},{"id":"diameter 1.11.1","anchor":"diameter-1-11-1"},{"id":"diameter 1.11","anchor":"diameter-1-11"},{"id":"diameter 1.10","anchor":"diameter-1-10"},{"id":"diameter 1.9.2","anchor":"diameter-1-9-2"},{"id":"diameter 1.9.1","anchor":"diameter-1-9-1"},{"id":"diameter 1.9","anchor":"diameter-1-9"},{"id":"diameter 1.8","anchor":"diameter-1-8"},{"id":"diameter 1.7.1","anchor":"diameter-1-7-1"},{"id":"diameter 1.7","anchor":"diameter-1-7"},{"id":"diameter 1.6","anchor":"diameter-1-6"},{"id":"diameter 1.5","anchor":"diameter-1-5"},{"id":"diameter 1.4.4","anchor":"diameter-1-4-4"},{"id":"diameter 1.4.3","anchor":"diameter-1-4-3"},{"id":"diameter 1.4.2","anchor":"diameter-1-4-2"},{"id":"diameter 1.4.1.1","anchor":"diameter-1-4-1-1"},{"id":"diameter 1.4.1","anchor":"diameter-1-4-1"},{"id":"diameter 1.4","anchor":"diameter-1-4"},{"id":"diameter 1.3.1","anchor":"diameter-1-3-1"},{"id":"diameter 1.3","anchor":"diameter-1-3"},{"id":"diameter 1.2","anchor":"diameter-1-2"},{"id":"diameter 1.1","anchor":"diameter-1-1"},{"id":"diameter 1.0","anchor":"diameter-1-0"},{"id":"diameter 0.10","anchor":"diameter-0-10"},{"id":"diameter 0.9","anchor":"diameter-0-9"}]},{"id":"diameter_intro","group":"User's Guides","title":"Introduction","headers":[]},{"id":"diameter_using","group":"User's Guides","title":"Usage","headers":[]},{"id":"diameter_examples","group":"User's Guides","title":"Examples","headers":[]},{"id":"diameter_soc","group":"User's Guides","title":"Standards Compliance","headers":[{"id":"RFC 6733 - Diameter Base Protocol","anchor":"rfc-6733-diameter-base-protocol"}]},{"id":"diameterc_cmd","group":"Command Line Tools","title":"diameterc","headers":[{"id":"Synopsis","anchor":"synopsis"},{"id":"Description","anchor":"description"}]},{"id":"diameter_dict","group":"References","title":"diameter_dict","headers":[{"id":"Description","anchor":"description"},{"id":"FILE FORMAT","anchor":"file-format"},{"id":"MESSAGE RECORDS","anchor":"message-records"},{"id":"DATA TYPES","anchor":"data-types"},{"id":"SEE ALSO","anchor":"see-also"}]}],"tasks":[]}