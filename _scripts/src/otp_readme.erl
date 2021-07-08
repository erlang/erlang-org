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
    case re:run(Line, "^([A-Z][^:]+):\\s+(.*)$",[{capture,all_but_first,binary}]) of
        {match,[Name,Value]} ->
            gulp_metadata(Rest, Name, MD#{ Name => Value });
        nomatch ->
            case re:run(Line,"^\\s+(.*)$",[{capture,all_but_first,binary}]) of
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


gulp_applications(Readme) ->
    %% This regexp matches the ---- headers in the readme and extracts the version
    {match,Applications} = re:run(Readme," [-]{69}\n [-]{3}(?: (.+) )?[-]+\n [-]{69}",[global]),
    split_applications(Readme, Applications).

%% We extract the version and the content in between each header
split_applications(Readme, [[{Start,Len},Vsn]|[[{End,_}|_]|_]=T]) ->
    [{binary:part(Readme,Vsn), gulp_application(binary:part(Readme,Start+Len,End-(Start+Len)))}
    | split_applications(Readme, T)];
%% The last element is just ---\n----\n--- without version
split_applications(_,[[_]]) ->
    [].

gulp_application(App) ->
    {Pre, PreRest} = gulp_until(App,["^ --- ","^  OTP-"]),
    {Tickets, TicketRest} = gulp_tickets(PreRest, undefined, []),
    #{ pre => string:trim(Pre), tickets => Tickets, post => string:trim(TicketRest) }.

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
                               [{capture,all_but_first,binary},dotall]),
    [MDStr, ReleaseNote] = string:split(Body,"\n\n"),
    StrippedReleaseNote =
        lists:join($\n, [ string:trim(Line) || Line <- string:split(ReleaseNote,"\n",all)]),
    MD = maps:merge(Ticket, gulp_ticket_metadata(MDStr,undefined,#{})),
    MD#{ id => Id, release_note => string:trim(StrippedReleaseNote) }.

gulp_ticket_metadata(Ticket, Current, MD) ->
    case re:run(Ticket, "^\s\s+([A-Z][^:]+):(.*)",[{capture,all_but_first,binary},dotall]) of
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

gulp_until(Str,Patterns) ->
    gulp_until(Str,Patterns,[]).
gulp_until(Str,Patterns,Acc) ->
    case string:split(Str, "\n") of
        [S] when S =:= <<>>; S =:= [] ->
            {iolist_to_binary(lists:join($\n,lists:reverse(Acc))),<<>>};
        [Line, Rest] ->
            case lists:any(
                   fun(Pattern) ->
                           re:run(Line, Pattern) =/= nomatch
                   end, Patterns) of
                true ->
                    {iolist_to_binary(lists:join($\n,lists:reverse(Acc))), Str};
                false ->
                    gulp_until(Rest,Patterns,[Line|Acc])
            end
    end.
    
