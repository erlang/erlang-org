#!/usr/bin/env escript
%%
%% This escript converts a set of EEPs from the style in the
%% EEP repository to the style needed by jekyll. Mostly it is the
%% metadata that is needed, but also som minor things in the styling
%% is fixed.
-mode(compile).

main(EEPs) ->
    [fix_eep(EEP) || EEP <- EEPs].

fix_eep(EEP) ->
%    io:format("Parse eep: ~p~n",[EEP]),
    {ok, Content} = file:read_file(EEP),
    "eep-" ++ Num = filename:rootname(filename:basename(EEP)),
    NewContent = gulp(Num, Content),
    file:write_file(EEP, NewContent).

gulp(Num, B) ->
    {FrontMatterStr, Matter} = gulp_frontmatter(B),
    FrontMatter = parse_frontmatter(Num, iolist_to_binary(FrontMatterStr)),
    [render_frontmatter(FrontMatter), gulp_matter(Matter)].

%%
%% Replace the EEP style front matter (i.e. RFC 822):
%%
%%     Author: John Doe
%% ****
%%
%% with jekyll style frontmatter (i.e. yaml)
%%
%% ---
%% Author: John Doe
%% ---
%%
gulp_frontmatter(B) ->
    [Line | Lines] = string:split(string:trim(B),"\n",all),
    gulp_frontmatter(Lines, string:trim(Line),[]).
%% RFC 2822 specifies that if the next line start with " " or "\t"
%% it is a continuation of the previous line.
gulp_frontmatter([<<"    ",WSC,Line/binary>>|Rest],CurrLine,Acc)
  when WSC =:= $ ; WSC =:= $\t ->
    gulp_frontmatter(Rest, [CurrLine, " ", string:trim(Line)],Acc);
gulp_frontmatter([<<"    ",Line/binary>>|Rest],CurrLine,Acc) when Line =/= <<>> ->
    gulp_frontmatter(Rest, string:trim(Line),[CurrLine|Acc]);
gulp_frontmatter(Rest, CurrLine, Acc) ->
    {[[Line,$\n] || Line <- lists:reverse([CurrLine | Acc])], lists:join($\n,Rest)}.

%% Escape any '{{' with {% raw %} in order for liquid templates to work
gulp_matter(Matter) ->
    gulp_erlang_code(
      iolist_to_binary(
        re:replace(Matter,"{{","{% raw %}{{{% endraw %}",[global]))).

%% Convert all code blocks to erlang code blocks.
%% i.e.
%%     foo() ->
%%       bar.
%%
%% to
%% ```erlang
%% foo() ->
%%   bar().
%% ```
gulp_erlang_code(<<"    ",_/binary>> = Matter) ->
    %% Some EEPs (like EEP 2) are one large code block. We do not want
    %% to make those into an erlang code block.
    Matter;
gulp_erlang_code(Matter) when is_binary(Matter) ->
    [FirstLine | Rest] = string:split(string:trim(Matter),"\n",all),
    Lines =
        case re:run(FirstLine,"^\\*+$") of
            {match,_} ->
                Rest;
            nomatch ->
                [FirstLine | Rest]
        end,
    lists:join($\n,gulp_erlang_code(Lines, [])).
gulp_erlang_code([],[]) ->
    [];
%% Handle when we are inside a list
gulp_erlang_code([<<"    ",_/binary>> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([<<"\t",_/binary>> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([Line|Rest],list) ->
    case is_bullet_list(Line) of
        true ->
            [Line | gulp_erlang_code(Rest,list)];
        false ->
            [Line | gulp_erlang_code(Rest,[])]
    end;
gulp_erlang_code([<<"    ",Line/binary>>|Rest],CodeBlock) ->
    gulp_erlang_code(Rest,[Line|CodeBlock]);
gulp_erlang_code([<<"\t",Line/binary>>|Rest],CodeBlock) ->
    gulp_erlang_code(Rest,[Line|CodeBlock]);
gulp_erlang_code([Line|Rest],[]) ->
    case {is_bullet_list(Line),is_md_reference(Line)} of
        {true, false} ->
            [Line | gulp_erlang_code(Rest,list)];
        {false, true} ->
            case Rest of
                [<<"    ",Title/binary>> | T] ->
                    [[Line," ",Title] | gulp_erlang_code(T, [])];
                [<<"\t",Title/binary>> | T] ->
                    [[Line," ",Title] | gulp_erlang_code(T, [])];
                _ ->
                    [Line | gulp_erlang_code(Rest, [])]
            end;
        {false, false} ->
            [Line | gulp_erlang_code(Rest,[])]
    end;
gulp_erlang_code(Rest,CodeBlock) ->
    %% Check if the codeblock is only whitespace, if so ignore it
    case re:run(CodeBlock,"^\\s*$") of
        {match,_} ->
            lists:reverse(CodeBlock) ++ gulp_erlang_code(Rest,[]);
        nomatch ->
            %% Fence the code block with ```erlang
            ["```erlang"]++ lists:reverse(CodeBlock) ++ ["```"]
                ++ gulp_erlang_code(Rest,[])
    end.

is_bullet_list(Line) ->
    case re:run(Line,"^\\s*([0-9]+\\.|\\*|-)") of
        {match,_} ->
            true;
        nomatch ->
            false
    end.

is_md_reference(Line) ->
    case re:run(Line,"^\\[[^\\]]+\\]:") of
        {match,_} ->
            true;
        nomatch ->
            false
    end.

parse_frontmatter(NumStr, FrontMatterStr) ->

    %% Strip any leading 0s from NumStr
    Num = case string:trim(NumStr, leading, "0") of
        "" -> "0";
        N -> N
    end,
    FrontMatter
        = lists:foldl(
            fun
                (<<>>,M) ->
                    M;
                (Line,M) ->
                    case string:split(string:trim(Line),":") of
                        [Key,Value] ->
                            M#{ Key => parse_frontmatter_value(
                                         string:trim(Key),
                                         string:trim(Value)) }
                    end
            end, #{ <<"Num">> => Num, <<"layout">> => "eep" },
            string:split(FrontMatterStr,"\n",all)),
    maps:fold(
      fun(<<"Author">>,[{Owner,_}|_], M) ->
              M#{ <<"Owner">> => Owner };
         (<<"Type">>,Type, M) ->
              M#{ <<"ShortType">> => short_type(Type) };
         (<<"Status">>, Status, M) ->
              maps:merge(M, Status);
         (_,_,M) ->
              M
      end, FrontMatter, FrontMatter).

parse_frontmatter_value(<<"Author">>, Value) ->
    Authors = string:split(Value,", ",all),
    lists:map(
      fun(Author) ->
              case re:run(Author, "^([^<]+)(<([^>]+)>)?$",[{capture,all_but_first,binary}]) of
                  {match,[Name,_,Email]} ->
                      { string:trim(Name), string:trim(Email) };
                  {match,[Name]} ->
                      { string:trim(Name), undefined }
              end
      end, Authors);
parse_frontmatter_value(<<"Status">>, Value) ->
    %% This regexp matches "SomeStatus/SomeTag Some Description
    case re:run(Value,"^([^/]+)(/?[^\s]*)\s*(.*)$",[{capture,all_but_first,binary}]) of
        {match,[Status, Tag, Descr]} ->
            #{ <<"Status">> => Status,
               <<"ShortStatus">> => short_status(Status),
               <<"StatusTag">> => Tag,
               <<"StatusDescription">> => Descr }
    end;
parse_frontmatter_value(_, Value) ->
    Value.

short_status(Status) ->
    maps:get(Status,
             #{ <<"Active">> => "",
                <<"Draft">> => "",
                <<"Accepted">> => "A",
                <<"Rejected">> => "R",
                <<"Withdrawn">> => "W",
                <<"Deferred">> => "D",
                <<"Final">> => "F"}).

short_type(Type) ->
    maps:get(Type,
             #{ <<"Standards Track">> => "S",
                <<"Process">> => "P" }).

render_frontmatter(FrontMatter) ->
    io:format("~p~n",[FrontMatter]),
    FM =
        ["---"] ++
         lists:map(
           fun({<<"Author">> = Key,Value}) ->
                   %% We create a yaml array out of the authors
                   [Key,":",$\n,
                    lists:join(
                      $\n,
                      lists:map(
                        fun({Name,Email}) ->
                                ["  - ",Name," <",Email,">"]
                        end,Value))
                    ];
              ({Key,Value}) ->
                   [Key,": ",Value]
           end, maps:to_list(FrontMatter)) ++
        ["---"],
    [[Line,$\n] || Line <- FM].
