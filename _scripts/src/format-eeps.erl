%% This escript converts a set of EEPs from the style in the
%% EEP repository to the style needed by jekyll. Mostly it is the
%% metadata that is needed, but also som minor things in the styling
%% is fixed.
-module('format-eeps').
-export([main/1]).

main([Target,EEP0|EEPs]) ->
    file:make_dir(Target),
    [fix_eep(Target, EEP0, EEP) || EEP <- EEPs].

fix_eep(Target, EEP0, EEP) ->
    % io:format("Parse eep: ~p~n",[EEP]),
    {ok, Content} = file:read_file(EEP),
    Basename = filename:basename(EEP),
    "eep-" ++ Num = filename:rootname(Basename),
    {FrontMatterStr, Matter} = gulp_frontmatter(Content),
    FrontMatter = parse_frontmatter(Num, iolist_to_binary(FrontMatterStr)),
    NewContent =
        case list_to_integer(Num) =/= 0 of
            true ->
                [render_frontmatter(FrontMatter), gulp_matter(Matter)];
            false ->
                {ok, EEP0Bin} = file:read_file(EEP0),
                [_IgnoreFrontMatter, HtmlMatter] = string:split(EEP0Bin,"<hr />"),
                [render_frontmatter(FrontMatter),
                 re:replace(HtmlMatter,"(href=\")(eep-[0-9]+.md\")",
                            "\\1https://github.com/erlang/eep/blob/master/eeps/\\2")]
        end,

    %% Check if we can output the new content
    try iolist_to_binary(NewContent) of
        _ -> ok
    catch _:_ ->
            %% Flatten binaries and integers
            Flat =
                lists:foldl(
                  fun(E,{Curr,Acc}) when is_binary(E) ->
                          {<<Curr/binary,E/binary>>,Acc};
                     (E,{Curr,Acc}) when E =< 255, 0 =< E ->
                          {<<Curr/binary,E>>,Acc};
                     (E,{<<>>,Acc}) ->
                          {<<>>,Acc ++ [E]};
                     (E,{Curr,Acc}) ->
                          {<<>>,Acc ++ [Curr,E]}
                  end,{<<>>,[]},lists:flatten(NewContent)),
            io:format("~p~n",[Flat])
    end,
    ok = file:write_file(filename:join(Target, Basename), NewContent).

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
    gulp_erlang_code(iolist_to_binary(add_raw_tags(Matter))).

add_raw_tags(Matter) ->
    re:replace(Matter,"{{","{% raw %}{{{% endraw %}",[global]).

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
gulp_erlang_code([<<"\t",B/binary>>|Rest],list) ->
    gulp_erlang_code([<<"        ",B/binary>>|Rest],list);
%% Handle when we are inside a list
gulp_erlang_code([<<"  ",_/binary>> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([<<"   ">> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([<<"  ">> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([<<" ">> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([<<"">> = Line|Rest],list) ->
    [Line | gulp_erlang_code(Rest,list)];
gulp_erlang_code([Line|Rest],list) ->
    case is_bullet_list(Line) of
        true ->
            [Line | gulp_erlang_code(Rest,list)];
        false ->
            [Line | gulp_erlang_code(Rest,[])]
    end;
gulp_erlang_code([<<"```",_/binary>> = Line|Rest],[]) ->
    [Line | gulp_erlang_code(Rest,fence)];
gulp_erlang_code([<<"```",_/binary>> = Line|Rest],fence) ->
    [Line | gulp_erlang_code(Rest,[])];
gulp_erlang_code([Line|Rest],fence) ->
    [Line | gulp_erlang_code(Rest,fence)];
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
    case length(Rest) > 0 andalso re:run(hd(Rest),"^\\s*$") of
        {match,_} ->
            gulp_erlang_code(tl(Rest),[hd(Rest)|CodeBlock]);
        _ ->
            %% Check if the codeblock is only whitespace, if so ignore it
            case re:run(CodeBlock,"^\\s*$") of
                {match,_} ->
                    lists:reverse(CodeBlock) ++ gulp_erlang_code(Rest,[]);
                nomatch ->
                    {TrimmedBlock, Tail} = trim(CodeBlock, []),
                    %% Fence the code block with ```erlang
                    ["```erlang"] ++ TrimmedBlock ++ ["```"]
                        ++ gulp_erlang_code(Tail ++ Rest,[])
            end
    end.

%% This trim expects a list in reverse and returns all trimmed
%% whitespace elements in the front of the list
%% (i.e. the back of the string)
trim([Line | Rest], Trimmed) ->
    case re:run(Line,"^\\s*$") of
        {match,_} ->
            trim(Rest, [Line | Trimmed]);
        _ ->
            {lists:dropwhile(
               fun(L) -> is_tuple(re:run(L,"^\\s*$")) end,
               lists:reverse([Line|Rest])),
             lists:reverse(Trimmed)}
    end;
trim(List, Trimmed) ->
    {lists:reverse(List), lists:reverse(Trimmed)}.

is_bullet_list(Line) ->
    case re:run(Line,"^\\s*([0-9]+\\.|\\*|-)\\s{1,4}") of
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
    FM =
        ["---"] ++
         lists:map(
           fun({<<"Author">> = Key,Value}) ->
                   %% We create a yaml array out of the authors
                   [Key,":",$\n,
                    lists:join(
                      $\n,
                      lists:map(
                        fun
                            ({Name,undefined}) ->
                                ["  - ",Name];
                            ({Name,Email}) ->
                                ["  - ",Name," <",Email,">"]
                        end,Value))
                    ];
              ({Key,Value}) ->
                   [Key,": ",Value]
           end, maps:to_list(FrontMatter)) ++
        ["---"],
    [[Line,$\n] || Line <- FM].
