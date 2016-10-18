-module(erlorg_mailinglists).

-behaviour(gen_server).

-export([
         start_link/0,
         list/2
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec list(string(), integer()) -> [map()].
list(Name, Limit) ->
  case ets:member(?MODULE, Name) of
    true ->
      lists:sublist(ets:lookup_element(?MODULE, Name, 2), Limit);
    false -> []
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  ets:new(?MODULE, [named_table,
                    {keypos, 1},
                    set,
                    protected,
                    {write_concurrency, false},
                    {read_concurrency, true}]),

  {ok, MailingLists} = application:get_env(erlorg, mailinglists),
  {ok, Timeout} = application:get_env(erlorg, mailinglists_timeout),
  {ok, MaxLimit} = application:get_env(erlorg, mailinglists_maxlimit),
  {ok, BaseUrl} = application:get_env(erlorg, mailinglists_baseurl),

  State = #{timeout => Timeout,
            max => MaxLimit,
            base_url => BaseUrl,
            mailinglists => MailingLists},

  {ok, State, 0}.

handle_info(timeout, State) ->
  #{timeout := Timeout,
    max := MaxLimit,
    base_url := BaseUrl,
    mailinglists := MailingLists} = State,

  [fetch_mailinglist(Name, BaseUrl, MaxLimit) || Name <- MailingLists],
  lager:info("Done"),
  {noreply, State, Timeout}.

handle_call(_, _From, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_, _, State)-> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_mailinglist(MailingListName, BaseUrl, MaxLimit) ->
  lager:info("Getting threads for ~p", [MailingListName]),
  ListBaseUrl = BaseUrl ++ MailingListName ++ "/",
  Months = fetch_months(ListBaseUrl),
  Threads = fetch_threads(MailingListName, ListBaseUrl, Months, MaxLimit),
  ReversedThreads = lists:reverse(Threads),

  ets:insert(?MODULE, {MailingListName, ReversedThreads}).

-spec fetch_months(string()) -> [string()].
fetch_months(BaseUrl) ->
  Url = BaseUrl ++ "index.html",
  Root = parse_html(Url),
  RootZipper = zipper_default:map_tree(Root, children),

  [Table] = zipper:filter(by_name("table"), RootZipper),
  TableZipper = zipper_default:map_tree(Table, children),

  Rows = zipper:filter(by_name("tr"), TableZipper),
  YearRows = tl(lists:reverse(Rows)),
  lists:map(fun extract_month_href/1, YearRows).

-spec fetch_threads(string(), string(), [string()], integer()) -> [map()].
fetch_threads(MailingListName, BaseUrl, Months, Limit) ->
  fetch_threads(MailingListName, BaseUrl, Months, Limit, []).

-spec fetch_threads(string(), string(), [string()], integer(), [map()]) ->
  [map()].
fetch_threads(_MailingListName, _BaseUrl, [], _Limit, Threads) ->
  Threads;
fetch_threads(MailingListName, BaseUrl,
              [Month | Months], Limit, Threads) ->
  lager:info("Threads from ~p", [Month]),
  Url = BaseUrl ++ Month,
  Root = parse_html(Url),
  RootZipper = zipper_default:map_tree(Root, children),

  {MonthName, _} = lists:splitwith(fun(X) -> X =/= $/ end, Month),
  MonthBaseUrl = BaseUrl ++ MonthName ++ "/",
  {ok, Regex} = re:compile("^\\s*\\[" ++ MailingListName ++ "\\]\\s*"),

  [_, UL| _] = filter(fun is_top_level_ul/1, RootZipper),
  NewThreads = [build_thread_map(LI, Regex, MonthBaseUrl)
                || LI <- children(UL)] ++ Threads,

  case length(NewThreads) > Limit of
    true ->
      NewThreads;
    false ->
      fetch_threads(MailingListName, BaseUrl, Months, Limit, NewThreads)
  end.

-spec is_top_level_ul(zipper:zipper()) -> boolean().
is_top_level_ul(Zipper) ->
  Parent = zipper:up(Zipper),
  Node = zipper:node(Zipper),
  name(Node) == "ul" andalso name(Parent) =/= "li".

-spec build_thread_map(zipper:zipper(), re:mp(), string()) -> map().
build_thread_map(LI, Regex, BaseUrl) ->
  [Link, _, Author | _] = children(LI),
  #{title => re:replace(content(Link), Regex, ""),
    url => BaseUrl ++ attr("href", Link),
    author => content(Author)}.

-spec extract_month_href(map())-> string().
extract_month_href(Row) ->
  [_, Col, _] = children(Row),
  [Link | _] = children(Col),
  attr("href", Link).

-spec by_name(string())-> fun((map()) -> boolean()).
by_name(NameArg) ->
  fun (#{name := Name}) -> NameArg == Name; (_) -> false end.

-spec wget(string())-> string().
wget(Url) ->
  {ok, {_Rc, _Hdrs, Body}} = httpc:request(get, {Url, []}, [], []),
  Body.

-spec parse_html(string())-> map().
parse_html(Url) ->
  Body = wget(Url),
  Tokens = trane:sax(Body, fun(T, A)-> A ++ [T] end, []),
  build_tree(Tokens, build_node(root)).

%% Node & Tree manipulation functions

-spec build_tree(list(), map())-> map().
build_tree([], Node) ->
  Node;
build_tree([{text, Text} | Rest], Current) ->
  Pattern = [<<" ">>, <<"\n">>, <<"\r">>, <<"\t">>],
  case binary:replace(Text, Pattern, <<>>, [global]) of
    <<>> ->
      build_tree(Rest, Current);
    _->
      NewNode = build_node("text", #{}, [Text]),
      build_tree(Rest, add_child(NewNode, Current))
  end;
build_tree([{tag, "br", _} | Rest], Current) ->
  Node = build_node("br"),
  build_tree(Rest, add_child(Node, Current));
build_tree([{tag, "li", _} | _] = Rest, #{name := "li"} = Current) ->
  %% There shouldn't be two nested li elements.
  {reverse_children(Current), Rest};
build_tree([{tag, Name, Attrs} | Rest], Current) ->
  NewNode = build_node(Name, maps:from_list(Attrs)),
  {Node, NewRest} = build_tree(Rest, NewNode),
  build_tree(NewRest, add_child(Node, Current));
build_tree([{end_tag, Name} | Rest], #{name := Name} = Current) ->
  {reverse_children(Current), Rest};
build_tree([{end_tag, _} | Rest], Current) ->
  build_tree(Rest, Current);
build_tree([{'!', _} | Rest], Current) ->
  build_tree(Rest, Current);
build_tree([{comment, _} | Rest], Current) ->
  build_tree(Rest, Current).

-spec build_node(string())-> map().
build_node(Name) ->
  build_node(Name, #{}, []).

-spec build_node(string(), map())-> map().
build_node(Name, Attrs) ->
  build_node(Name, Attrs, []).

-spec build_node(string(), map(), list())-> map().
build_node(Name, Attrs, Children) ->
  #{name => Name, attrs => Attrs, children => Children}.

-spec add_child(term(), map())-> map().
add_child(Child, #{children := Children} = Node) ->
  Node#{children => [Child | Children]}.

-spec reverse_children(map())-> map().
reverse_children(#{children := Children} = Node) ->
  Node#{children => lists:reverse(Children)}.

-spec name(map())-> string().
name(#{name := Name}) ->  Name;
name(_) -> undefined.

-spec children(map())-> [map()].
children(#{children := X}) -> X;
children(_) -> [].

-spec content(map())-> binary().
content(Node) ->
  Zipper = zipper_default:map_tree(Node, children),
  TextNodes = zipper:filter(by_name("text"), Zipper),
  Text = lists:map(fun children/1, TextNodes),
  iolist_to_binary(lists:reverse(Text)).

-spec attr(term(), map())-> term().
attr(AttrName, #{attrs := Attrs}) ->
  maps:get(AttrName, Attrs, undefined).

-spec filter(fun((zipper:zipper()) -> boolean()), map())-> [map()].
filter(Pred, Zipper) ->
  do_filter(Pred, [], Zipper).

-spec do_filter(fun((zipper:zipper()) -> boolean()), [map()], map())-> [map()].
do_filter(Pred, Result, Zipper) ->
  case zipper:is_end(Zipper) of
    true -> lists:reverse(Result);
    false ->
      NewResult =
        case Pred(Zipper) of
          true -> [zipper:node(Zipper) | Result];
          false -> Result
        end,
      do_filter(Pred, NewResult, zipper:next(Zipper))
  end.
