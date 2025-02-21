-module(otp_readme).
-include_lib("kernel/include/logger.hrl").
-export([parse/1, render/1, render_yaml/1]).

parse(Readme) ->
    try
        gulp(Readme)
    catch invalid ->
            Readme;
          E:R:ST ->
            [Line,_] = string:split(Readme,"\n"),
            ?LOG_ERROR("Failed to parse: ~p~n~p:~p:~p",[Line, E,R,ST]),
            Readme
    end.

render(Readme) when is_binary(Readme) ->
    ["<code><pre>",
     re:replace(Readme,"<","\\&lt;",[global]),
     "</pre></code>"];
render(Readme) ->
    ["## ", maps:get(<<"Git Tag">>,Readme),"\n"].

render_yaml(Readme) when is_binary(Readme) ->
    "";
render_yaml(Readme) ->
    lists:map(
      fun({Key,Value}) when is_binary(Key), length(Value) =:= 1 ->
              [yaml_key(Key),": ",Value,"\n"];
         ({Key,Values}) when is_binary(Key) ->
              [yaml_key(Key),":\n",
               [["  - ", Value,"\n"] || Value <- Values]];
         ({applications,Apps}) ->
              ["Applications:\n",
               lists:map(
                 fun({App,MD}) when App =/= <<"Thanks To">> ->
                         ["  ",yaml_key(App),":\n",
                          "    pre: ",yaml_str(maps:get(pre,MD,"")),"\n",
                          "    post: ",yaml_str(maps:get(post,MD,"")),"\n",
                          "    mode: ",yaml_str(atom_to_list(maps:get(mode,MD))),"\n",
                          "    tickets:\n",
                          lists:map(
                            fun(Ticket) ->
                                    ["      ",maps:get(id,Ticket),":\n",
                                     [["        ", yaml_key(Key),": ",yaml_str(Value),"\n"]
                                      || {Key,Value} <- maps:to_list(maps:remove(id,Ticket)),
                                         Value =/= undefined]
                                    ]
                            end, maps:get(tickets, MD))
                         ];
                    (_) ->
                         ""
                 end,Apps)];
         (_) ->
              []
      end, maps:to_list(Readme)).


yaml_str(Str) ->
    ["\"",addnl(s(Str)),"\""].
yaml_key(Key) ->
    re:replace(re:replace(s(Key)," ","",[global]),
               "[()]","",[global]).
s(Atom) when is_atom(Atom) ->
    s(atom_to_list(Atom));
s(Else) ->
    re:replace(re:replace(Else,"\\\\","\\\\\\\\",[global]),
               "\"","\\\\\"",[global]).

%% Change "a\nb" -> "a\\nb" for better yaml parsing goodness
addnl(String) ->
    lists:join("\\n",string:split(String,"\n",all)).

%% The new format of readmes started at vsn 18.0.1 and they all
%% start with this line first.
gulp(<<"```\n", Readme/binary>>) ->
    {Rest, MD} = gulp_metadata(Readme,undefined,#{}),
    MD#{ applications => gulp_applications_markdown(Rest) };
gulp(<<"Patch Package",_/binary>> = Readme) ->
    {Rest, MD} = gulp_metadata(Readme,undefined,#{}),
    MD#{ applications => gulp_applications(Rest) };
gulp(<<"Inital Release",_/binary>> = Readme) ->
    {Rest, MD} = gulp_metadata(Readme,undefined,#{}),
    MD#{ applications => gulp_applications(Rest) };
gulp(_) ->
    throw(invalid).

gulp_metadata(Readme, Current, MD) ->
    [Line,Rest] = string:split(Readme, "\n"),
    case re:run(Line, "^([A-Z][^:]+):\\s+(.*)$",[{capture,all_but_first,binary}, unicode]) of
        {match,[Name,Value]} ->
            gulp_metadata(Rest, Name, MD#{ Name => Value });
        nomatch ->
            case re:run(Line,"^\\s+(.*)$",[{capture,all_but_first,binary}, unicode]) of
                {match, [Value]} ->
                    gulp_metadata(Rest, Current, MD#{ Current := [maps:get(Current, MD),Value]});
                nomatch ->
                    {Readme, parse_metadata(MD)}
            end
    end.


parse_metadata(MD) ->
    maps:map(
      fun(_,TRs) ->
              [string:trim(Str) || Str <- string:split(TRs,",",all)]
      end, MD).


-type ticket() :: #{ id := unicode:chardata(),
                     release_note := unicode:chardata(),
                     type => unicode:chardata(), %% Improvements and New Features
                     % <<"Application(s)">> => unicode:chardata()
                     % <<"Related Id(s)">> => unicode:chardata()
                     unicode:chardata() => unicode:chardata() }.

-type application() :: {ApplicationName :: unicode:chardata(),
                        #{ pre => unicode:chardata(),
                           post => unicode:chardata(),
                           mode := markdown | txt,
                           tickets := [ticket()] }}.

-spec gulp_application_markdown(unicode:chardata(), term()) -> [application()].
gulp_applications_markdown(Readme) ->
    LinksStart =
        case re:run(Readme, "^\\[", [multiline, unicode]) of
            {match, [{Start, _}]} -> Start;
            nomatch -> byte_size(Readme)
        end,

    AppsReadme = binary:part(Readme, 0, LinksStart),
    LinksReadme = binary:part(Readme, LinksStart - 1, byte_size(Readme) - LinksStart + 1),

    Apps =
        case re:run(AppsReadme, "\n(.+)\n=+\n", [global, unicode]) of
            {match, Applications} ->
                Applications;
            nomatch ->
                {match, Applications} = re:run(AppsReadme, "\n#\\s(.+)", [global, unicode]),
                Applications
        end,

    Links =
        case re:run(LinksReadme, "\\[([^]]+)\\]:(?: |\n).*", [global, {capture, all, binary}, unicode]) of
            {match, LS} -> LS;
            nomatch -> []
        end,

%    throw({LinksReadme, Links}),

    split_applications(Readme, fun gulp_application_markdown/2, Links, Apps).

gulp_application_markdown(App, Links) ->
    {Pre, PreRest} = gulp_until(App,["^(?:##|-|\\*) "]),
    {Tickets, TicketRest} = gulp_tickets_markdown(PreRest, undefined, Links, []),
    #{ pre => string:trim(Pre), mode => markdown, tickets => Tickets, post => string:trim(TicketRest) }.

gulp_tickets_markdown(App, Type, Links, Acc) ->
    {Pre, PreRest} = gulp_until(App,["^## ","^(\\*|\\-)", "^>"]),
    <<>> = string:trim(Pre),
    case string:split(PreRest, "\n") of
        [<<"## ",_/binary>> = NewType, Rest] ->
            gulp_tickets_markdown(Rest, NewType, Links, Acc);
        [<<Bullet," ",Line/binary>>, Rest] when Bullet =:= $* orelse Bullet =:= $- ->
            {Ticket, TRest} = gulp_until(Rest, ["^## ","^(\\*|\\-)", "^>"]),
            true = Ticket =/= <<>>,
            TicketStr = re:replace([Line,$\n,Ticket], "^  ", "", [multiline, global]),
            gulp_tickets_markdown(TRest, Type, Links, [gulp_ticket_markdown(TicketStr, Type, Links) | Acc]);
        _ ->
            {lists:reverse(Acc), PreRest}
    end.

gulp_ticket_markdown(T, undefined, Links) ->
    gulp_ticket_markdown(T,#{}, Links);
gulp_ticket_markdown(T, Type, Links) when not is_map(Type) ->
    gulp_ticket_markdown(T,#{ type => Type }, Links);
gulp_ticket_markdown(T, Ticket, Links) ->
    case re:run(T, "((?:\n|.)+)\n\nOwn Id: (OTP-[0-9]+)\\s*((?:\n|.)*)",
                [{capture,all_but_first,binary}, unicode]) of
        nomatch -> throw({invalid, T});
        {match,[ReleaseNote,Id, MDStr]} ->
            MD = maps:merge(Ticket, gulp_ticket_metadata_markdown(string:trim(MDStr),undefined,#{})),
            fix_markdown_refs(MD#{ id => Id, release_note => ReleaseNote }, Links)
    end.

%% Insert any markdown references into the texts
fix_markdown_refs(MD, Links) ->
    maps:map(
      fun(_Key, Value) ->
              Refs =
                  case re:run(Value, "\\[([^]]+)\\]", [unicode, {capture, all_but_first, binary}, global]) of
                      nomatch ->
                          "";
                      {match, References} ->
                          Rs = [ re:replace(Ref,"\n\\s*"," ", [unicode]) ||
                                   [Ref, Key] <- Links, lists:member([Key], References)],
                          ["\n\n", lists:join("\n",Rs)]
                  end,
              [Value, Refs]
      end, MD).

gulp_ticket_metadata_markdown(<<>>, _Current, MD) ->
    MD;
gulp_ticket_metadata_markdown(Ticket, Current, MD) ->
    case re:run(Ticket, "^([A-Z][^:]+):(.*)",[{capture,all_but_first,binary},dotall, unicode]) of
        {match,[Label,Rest]} ->
            gulp_ticket_metadata_markdown(Rest,Label,MD);
        nomatch ->
            case re:run(Ticket, "\n(?:\\\\\\*){3} (.*) (?:\\\\\\*){3}(.*)$",
                        [{capture, all_but_first, binary}, dotall, ungreedy, unicode]) of
                {match,[Tag, Rest]} ->
                    NewMD =
                        case maps:find(<<"Tags">>, MD) of
                            error ->
                                MD#{ <<"Tags">> => string:trim([maps:get(<<"Tags">>, MD, []), " ", string:trim(Tag)]) };
                            {ok, MDItem} ->
                                MD#{ <<"Tags">> => string:trim([MDItem, ", ", string:trim(Tag)]) }
                        end,
                    gulp_ticket_metadata_markdown(Rest, undefined, NewMD);
                nomatch ->
                    MDItem = maps:get(Current, MD, []),
                    case string:split(Ticket,"\n") of
                        [Line,Rest] ->
                            gulp_ticket_metadata_markdown(
                              Rest, Current,
                              MD#{ Current => string:trim([MDItem, " ", string:trim(Line)]) });
                        [Line] ->
                            MD#{ Current => string:trim([MDItem, " ", string:trim(Line)]) }
                    end
            end
    end.

%% #{applications =>
%%       [{<<"POTENTIAL INCOMPATIBILITIES">>,
%%         #{pre => <<>>,post => <<>>,
%%           tickets =>
%%               [#{id => <<"OTP-18728">>,
%%                  release_note =>
%%                      [[<<"With this change, common_test returns an error when">>,
%%                        " ",<<"suite with a badly defined group is executed.">>],
%%                       "\n\n",[]],
%%                  <<"Application(s)">> => <<"common_test">>,
%%                  <<"Related Id(s)">> => <<"PR-7487, PR-7674">>}]}},

-spec gulp_application(unicode:chardata(), term()) -> [application()].
gulp_applications(Readme) ->
    %% This regexp matches the ---- headers in the readme and extracts the app-vsn
    {match,Applications} = re:run(Readme," [-]{69}\n [-]{3}(?: (.+) )?[-]+\n [-]{69}",[global, unicode]),
    split_applications(Readme, fun gulp_application/2, undefined, Applications).

%% We extract the app-vsn and the content in between each header
split_applications(Readme, Fun, State, [[{Start,Len},AppVsn]|[[{End,_}|_]|_]=T]) ->
    [{binary:part(Readme,AppVsn), Fun(binary:part(Readme,Start+Len,End-(Start+Len)), State)}
    | split_applications(Readme, Fun, State, T)];
%% The last element for txt is just ---\n----\n--- without version
split_applications(_,_, _, [[_]]) ->
    [];
%% The last element for md, we look for "^[", i.e. the first of all reference links
split_applications(Readme, Fun, State, [[{Start, Len}, AppVsn]]) ->
    Rest = binary:part(Readme, Start, byte_size(Readme) - Start),
    [{binary:part(Readme, AppVsn),
      case re:run(Rest,"^\\[",[multiline]) of
          nomatch ->
              Fun(Rest, State);
          {match, [{RefStart,_}]} ->
              Fun(binary:part(Rest, Len, RefStart - Len), State)
      end}].

gulp_application(App, _) ->
    {Pre, PreRest} = gulp_until(App,["^ --- ","^  OTP-"]),
    {Tickets, TicketRest} = gulp_tickets(PreRest, undefined, []),
    #{ pre => string:trim(Pre), mode => txt, tickets => Tickets, post => string:trim(TicketRest) }.

gulp_tickets(App, Type, Acc) ->
    {Pre, PreRest} = gulp_until(App,["^ --- ","^  OTP-","^ [A-Z]"]),
    <<>> = string:trim(Pre),
    case string:split(PreRest, "\n") of
        [<<" --- ",_/binary>> = Line, Rest] ->
            gulp_tickets(Rest, Line, Acc);
        [<<"  OTP-",_/binary>> = Line, Rest] ->
            {Ticket, TRest} = gulp_until(Rest,["^ --- ","^  OTP-","^ [A-Z]"]),
            true = Ticket =/= <<>>,
            gulp_tickets(TRest, Type, [gulp_ticket([Line,$\n,Ticket], Type) | Acc]);
        _ ->
            {lists:reverse(Acc), PreRest}
    end.

gulp_ticket(T, undefined) ->
    gulp_ticket(T,#{});
gulp_ticket(T, Type) when not is_map(Type) ->
    gulp_ticket(T,#{ type => Type });
gulp_ticket(T, Ticket) ->
    {match,[Id,Body]} = re:run(T, "\s+(OTP-[0-9]+)(.*)",
                               [{capture,all_but_first,binary},dotall, unicode]),
    [MDStr, ReleaseNote] = string:split(Body,"\n\n"),
    MD = maps:merge(Ticket, gulp_ticket_metadata(MDStr,undefined,#{})),
    MD#{ id => Id, release_note => format_release_note(ReleaseNote) }.

gulp_ticket_metadata(Ticket, Current, MD) ->
    case re:run(Ticket, "^\s\s+([A-Z][^:]+):(.*)",[{capture,all_but_first,binary},dotall, unicode]) of
        {match,[Label,Rest]} ->
            gulp_ticket_metadata(Rest,Label,MD);
        nomatch when Current =/= undefined ->
            MDItem = maps:get(Current, MD, []),
            case string:split(Ticket,"\n") of
                [Line,Rest] ->
                    gulp_ticket_metadata(
                      Rest, Current,
                      MD#{ Current => string:trim([MDItem, " ", string:trim(Line)]) });
                [Line] ->
                    MD#{ Current => string:trim([MDItem, " ", string:trim(Line)]) }
            end
    end.

%%% Release notes have their lines split so that it never
%%% becomes larger that 70 chars. However, there are also
%%% lines that should break. So this function looks for places
%%% where the automatic line break seems to have happened
%%% and collapses those breaks.
format_release_note(ReleaseNote) ->
    Paragraphs = string:split(ReleaseNote,"\n\n",all),
    lists:join("\n\n",[format_release_paragraph(string:split(P,"\n",all))
                       || P <- Paragraphs]).

format_release_paragraph([Line|[Next|_]=Rest]) ->
    [Word | _] = string:split(string:trim(Next)," "),
    Length = string:length([Line," ",Word]),
    [string:trim(Line),
     if Length >= 70 ->
             " ";
        true ->
             "\n"
     end,
     format_release_paragraph(Rest)];
format_release_paragraph(Line) ->
    string:trim(Line).

gulp_until(Str,Patterns) ->
    gulp_until(Str,Patterns,[]).
gulp_until(Str,Patterns,Acc) ->
    case string:split(Str, "\n") of
        [S] when S =:= <<>>; S =:= [] ->
            {iolist_to_binary(lists:join($\n,lists:reverse(Acc))),<<>>};
        [Line, Rest] ->
            case lists:any(
                   fun(Pattern) ->
                           re:run(Line, Pattern, [unicode]) =/= nomatch
                   end, Patterns) of
                true ->
                    {iolist_to_binary(lists:join($\n,lists:reverse(Acc))), Str};
                false ->
                    gulp_until(Rest,Patterns,[Line|Acc])
            end
    end.
    
