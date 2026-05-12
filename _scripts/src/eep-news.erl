%% Walks the git history of each EEP source file in _clones/eep and
%% emits a small `_news/eep-NNNN-YYYY-MM-DD-<status>.md` stub for every
%% commit where the leading word of the `Status:` field changed
%% (e.g. Draft -> Accepted -> Final). The stubs use the existing
%% `redirect` layout and link out to the EEP page, so they slot into
%% the news reel and RSS feed without needing their own content.
-module('eep-news').
-export([main/1]).

main([NewsDir, EEPRepo]) ->
    ok = filelib:ensure_dir(filename:join(NewsDir, "_")),
    EEPFiles = filelib:wildcard(filename:join([EEPRepo, "eeps", "eep-*.md"])),
    lists:foreach(
      fun(EEPFile) -> process_eep(NewsDir, EEPRepo, EEPFile) end,
      EEPFiles).

process_eep(NewsDir, EEPRepo, EEPFile) ->
    Basename = filename:basename(EEPFile),
    "eep-" ++ NumStr = filename:rootname(Basename),
    case list_to_integer(NumStr) of
        0 ->
            %% Skip the index EEP — no meaningful status.
            ok;
        _ ->
            RelPath = "eeps/" ++ Basename,
            case get_status_transitions(EEPRepo, RelPath) of
                [] -> ok;
                Transitions ->
                    Title = current_title(EEPFile),
                    [emit_stub(NewsDir, NumStr, Title, T) || T <- Transitions],
                    ok
            end
    end.

%% Returns [{ISODate, OldStatus, NewStatus}] in chronological order.
get_status_transitions(Repo, RelPath) ->
    Cmd = io_lib:format("git -C ~s log --reverse --pretty='format:%H|%cI' -- ~s",
                        [Repo, RelPath]),
    Output = os:cmd(lists:flatten(Cmd)),
    Lines = string:tokens(Output, "\n"),
    Commits = [parse_commit_line(L) || L <- Lines],
    Statuses0 = [{Date, status_at(Repo, Hash, RelPath)} || {Hash, Date} <- Commits],
    Statuses = [{D, S} || {D, S} <- Statuses0, S =/= undefined],
    find_transitions(Statuses, undefined, []).

parse_commit_line(Line) ->
    [Hash, Date] = string:split(Line, "|"),
    {Hash, Date}.

status_at(Repo, Hash, RelPath) ->
    Cmd = io_lib:format("git -C ~s show ~s:~s 2>/dev/null", [Repo, Hash, RelPath]),
    %% os:cmd returns a list which may contain UTF-8 codepoints > 255 for
    %% non-ASCII characters in commit content. Convert to binary so re:run
    %% with the unicode option can handle it.
    Content = unicode:characters_to_binary(os:cmd(lists:flatten(Cmd))),
    extract_status(Content).

%% The upstream EEP frontmatter is RFC-822-style with 4-space indent.
%% Match the first Status: line and capture only the leading word
%% (Draft, Accepted, Final, etc.) — the `/tag Description` tail is
%% noise for transition detection.
extract_status(Content) ->
    case re:run(Content,
                "^\\s{4}Status:\\s*([A-Za-z]+)",
                [multiline, unicode, {capture, all_but_first, list}]) of
        {match, [Status]} -> string:lowercase(Status);
        nomatch -> undefined
    end.

find_transitions([], _Prev, Acc) ->
    lists:reverse(Acc);
find_transitions([{_Date, Status} | Rest], undefined, Acc) ->
    find_transitions(Rest, Status, Acc);
find_transitions([{_Date, Status} | Rest], Prev, Acc) when Status =:= Prev ->
    find_transitions(Rest, Prev, Acc);
find_transitions([{Date, Status} | Rest], Prev, Acc) ->
    find_transitions(Rest, Status, [{Date, Prev, Status} | Acc]).

current_title(EEPFile) ->
    {ok, Content} = file:read_file(EEPFile),
    %% Prefer a frontmatter `Title:` (used by EEPs 2 and 3); otherwise
    %% extract the body's first heading.
    case re:run(Content,
                "^\\s{4}Title:\\s*([^\\n]+)",
                [multiline, {capture, all_but_first, binary}]) of
        {match, [T1]} -> string:trim(T1);
        nomatch ->
            case re:run(Content,
                        "(?:\\*+\\s*\\n\\s*)?EEP\\s+[0-9]+\\s*:\\s*([^\\n]+)",
                        [{capture, all_but_first, binary}]) of
                {match, [T2]} -> string:trim(T2);
                nomatch -> <<>>
            end
    end.

emit_stub(NewsDir, NumStr, Title, {ISODate, OldStatus, NewStatus}) ->
    DateOnly = string:slice(ISODate, 0, 10),
    PaddedNum = lists:flatten(io_lib:format("~4..0s", [NumStr])),
    %% Strip leading zeros for display ("EEP 70", not "EEP 0070")
    DisplayNum = string:trim(NumStr, leading, "0"),
    Filename = filename:join(NewsDir,
                             "eep-" ++ PaddedNum ++ "-" ++ DateOnly
                             ++ "-" ++ NewStatus ++ ".md"),
    Content =
        ["---\n",
         "layout: redirect\n",
         "title: \"EEP ", DisplayNum, ": ", escape_yaml(Title),
              <<" — "/utf8>>, capitalize(NewStatus), "\"\n",
         "lead: \"Status changed from ", capitalize(OldStatus),
              " to ", capitalize(NewStatus), ".\"\n",
         "date: ", DateOnly, "\n",
         "html_url: /eeps/eep-", PaddedNum, "\n",
         "---\n"],
    file:write_file(Filename, iolist_to_binary(Content)).

capitalize([C | Rest]) when C >= $a, C =< $z ->
    [C - 32 | Rest];
capitalize(L) -> L.

escape_yaml(Bin) when is_binary(Bin) ->
    B1 = binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
    binary:replace(B1, <<"\"">>, <<"\\\"">>, [global]).
