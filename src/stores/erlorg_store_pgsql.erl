%%% @doc PostgreSql store for erlang.org, based on sumo_store_pgsql
-module(erlorg_store_pgsql).
-author('juan@inaka.net').

-behaviour(sumo_store).

-include_lib("epgsql/include/pgsql.hrl").

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {sumo_store_pgsql,
         [ init/1
         , create_schema/2
         , persist/2
         , delete_by/3
         , delete_all/2
         , find_all/2
         , find_all/5
         , find_by/3
         , find_by/5
         ]}
       ]).
-export([find_by/6]).

-export([
         link_categories_with_links/3,
         list/4,
         by_id/3,
         by_user/4,
         by_year/4,
         by_tag/4,
         tag_filters/3,
         user_filters/3,
         year_filters/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% redefines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @todo move this to mixer once mixer supports functions with more than 5 args
-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              sumo:sort(),
              non_neg_integer(),
              non_neg_integer(),
              State) ->
  sumo_store:result([sumo_internal:doc()], State).
find_by(DocName, Conditions, SortFields, Limit, Offset, State) ->
  sumo_store_pgsql:find_by(
    DocName, Conditions, SortFields, Limit, Offset, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extensions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec link_categories_with_links(binary(), atom(), State) ->
  sumo_store:result(ok, State).
link_categories_with_links(GroupName, DocName, #{conn := Conn} = State) ->
  Sql = ["SELECT "
         "  lc.*, l.title, l.url, l.description "
         "FROM "
         "  erlorg_link_groups lg "
         "  INNER JOIN erlorg_link_categories AS lc ON "
         "    (lg.id = lc.group_id)"
         "  LEFT JOIN erlorg_links AS l ON "
         "    (l.category_id = lc.id) "
         "WHERE "
         "  lower(lg.name) = $1 "
         "ORDER BY "
         "   lc.order, "
         %% Descending order to avoid reversing the links list later.
         "   l.order DESC"],

  Values = [GroupName],

  case get_docs(Conn, Sql, Values, DocName) of
    {ok, Docs} ->
      CategoriesWithLinks = lists:foldl(fun build_categories/2, #{}, Docs),
      Categories = lists:sort(fun sort_by_order/2,
                              maps:values(CategoriesWithLinks)),
      {ok, {docs, Categories}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec list(erlorg_articles:type(), integer(), atom(), State) ->
  sumo_store:result(ok, State).
list(Type, Limit, DocName, #{conn := Conn} = State) ->
  Sql = [ "SELECT "
          "  a.*, u.name AS author "
          "FROM "
          "  erlorg_articles AS a "
          "  JOIN admin_users u ON "
          "    u.id = a.admin_user_id "
          "  JOIN erlorg_article_types AS t ON "
          "    t.id = a.article_type_id "
          "    AND lower(t.name) = $1  "
          "WHERE "
          "  a.visible = true "
          "ORDER BY "
          "  a.article_date DESC "],

    {Sql1, Values} =
        case Limit of
            0 -> {Sql, [Type]};
            _ -> {[Sql, "LIMIT $2 OFFSET 0"], [Type, Limit]}
        end,

    case get_docs(Conn, Sql1, Values, DocName) of
        {ok, Docs} ->
            {ok, {docs, Docs}, State};
        {error, Error} ->
            {error, Error, State}
    end.

-spec by_id(integer(), atom(), State) ->
  sumo_store:result(ok, State).
by_id(Id, DocName, #{conn := Conn} = State) ->
  Sql = [ "SELECT "
          "  a.*, u.name AS author "
          "FROM "
          "  erlorg_articles AS a "
          "  JOIN admin_users u ON "
          "    u.id = a.admin_user_id "
          "WHERE "
          "   a.id = $1"],
  Values = [Id],

  case get_docs(Conn, Sql, Values, DocName) of
    {ok, Docs} ->
      {ok, {docs, Docs}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec by_user(erlorg_articles:type(), binary(), atom(), State) ->
  sumo_store:result(ok, State).
by_user(Type, Username, DocName, #{conn := Conn} = State) ->
  Sql = [ "SELECT "
          "  a.*, u.name AS author "
          "FROM "
          "  erlorg_articles AS a "
          "  JOIN admin_users u ON "
          "    u.id = a.admin_user_id "
          "  JOIN erlorg_article_types AS t ON "
          "    t.id = a.article_type_id "
          "    AND lower(t.name) = $1  "
          "WHERE "
          "  u.name = $2 "
          "  AND a.visible = true "
          "ORDER BY "
          "   a.article_date DESC "],

  Values = [Type, Username],

  case get_docs(Conn, Sql, Values, DocName) of
    {ok, Docs} ->
      {ok, {docs, Docs}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec by_year(erlorg_articles:type(), integer(), atom(), State) ->
  sumo_store:result(ok, State).
by_year(Type, Year, DocName, #{conn := Conn} = State) ->
  Sql = ["SELECT "
         "  a.*, u.name AS author "
         "FROM "
         "  erlorg_articles AS a "
         "  JOIN admin_users u ON "
         "    u.id = a.admin_user_id "
         "  JOIN erlorg_article_types AS t ON "
         "    t.id = a.article_type_id "
         "    AND lower(t.name) = $1  "
         "WHERE "
         "  $2 = CAST(date_part('year', a.article_date) AS INT) "
         "  AND a.visible = true "
         "ORDER BY "
         "   a.article_date DESC "],
  Values = [Type, Year],

  case get_docs(Conn, Sql, Values, DocName) of
    {ok, Docs} ->
      {ok, {docs, Docs}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec by_tag(erlorg_articles:type(), binary(), atom(), State) ->
  sumo_store:result(ok, State).
by_tag(Type, Tag, DocName, #{conn := Conn} = State) ->
  Sql = ["SELECT "
         "  a.*, u.name AS author "
         "FROM "
         "  erlorg_articles AS a "
         "  JOIN admin_users u ON "
         "    u.id = a.admin_user_id "
         "  JOIN erlorg_article_types AS t ON "
         "    t.id = a.article_type_id "
         "    AND lower(t.name) = $1  "
         "WHERE "
         "  a.tags SIMILAR TO $2 "
         "  AND a.visible = true "
         "ORDER BY "
         "   a.article_date  DESC"],

  Regex = <<"(", Tag/binary,
            ",%|%,", Tag/binary,
            "|%,", Tag/binary,
            ",%|", Tag/binary,")">>,
  Values = [Type, Regex],

  case get_docs(Conn, Sql, Values, DocName) of
    {ok, Docs} ->
      {ok, {docs, Docs}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec tag_filters(erlorg_articles:type(), atom(), State) ->
  sumo_store:result(ok, State).
tag_filters(Type, _DocName, #{conn := Conn} = State) ->
  Sql = ["SELECT "
         "  'tag' AS \"type\", "
         "  tag_name AS value, "
         "  lower(tag_name) AS value_lower, "
         "  COUNT(1) AS \"count\" "
         "FROM ( "
         "  SELECT unnest(regexp_split_to_array(tags, ',')) AS tag_name"
         "  FROM "
         "    erlorg_articles AS a "
         "    JOIN erlorg_article_types AS t ON "
         "      t.id = a.article_type_id "
         "      AND lower(t.name) = $1 "
         "  WHERE "
         "    a.visible = true "
         "    AND a.tags <> '' "
         ") AS tags "
         "WHERE tag_name <> '' "
         "GROUP BY value "
         "ORDER BY value_lower "],
  Values = [Type],
  case get_maps(Conn, Sql, Values) of
    {ok, Maps} ->
      {ok, {raw, Maps}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec user_filters(erlorg_articles:type(), atom(), State) ->
  sumo_store:result(ok, State).
user_filters(Type, _DocName, #{conn := Conn} = State) ->
  Sql = ["SELECT "
         "  'user' AS \"type\", "
         "  u.name AS value, "
         "  COUNT(1) AS \"count\" "
         "FROM erlorg_articles a "
         "  JOIN admin_users u ON "
         "    u.id = a.admin_user_id "
         "  JOIN erlorg_article_types AS t ON "
         "    t.id = a.article_type_id "
         "    AND lower(t.name) = $1 "
         "WHERE a.visible = true "
         "GROUP BY u.name "
         "ORDER BY u.name "],
  Values = [Type],
  case get_maps(Conn, Sql, Values) of
    {ok, Maps} ->
      {ok, {raw, Maps}, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec year_filters(erlorg_articles:type(), atom(), State) ->
  sumo_store:result(ok, State).
year_filters(Type, _DocName, #{conn := Conn} = State) ->
  DatePart = ["CAST(date_part('year', article_date) AS INT) "],
  Sql = ["SELECT "
         " 'year' AS \"type\", "
         " ", DatePart, " AS value, "
         " COUNT(1) AS \"count\" "
         "FROM "
         "  erlorg_articles AS a "
         "  JOIN erlorg_article_types AS t ON "
         "    t.id = a.article_type_id "
         "    AND lower(t.name) = $1 "
         "WHERE a.visible = true "
         "GROUP BY ", DatePart, " "
         "ORDER BY ", DatePart, " DESC "],
  Values = [Type],
  case get_maps(Conn, Sql, Values) of
    {ok, Maps} ->
      {ok, {raw, Maps}, State};
    {error, Error} ->
      {error, Error, State}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_by_order(DocA, DocB) ->
  OrderA = sumo_internal:get_field(order, DocA),
  OrderB = sumo_internal:get_field(order, DocB),

  OrderA =< OrderB.

build_categories(Doc, DocsMap) ->
  Title = sumo_internal:get_field(title, Doc),
  Url = sumo_internal:get_field(url, Doc),
  Description = sumo_internal:get_field(description, Doc),

  Link = #{title => Title,
           url => Url,
           description => Description},

  CategoryId = sumo_internal:get_field(id, Doc),

  OldDoc =
    case maps:is_key(CategoryId, DocsMap) of
      true ->
        maps:get(CategoryId, DocsMap);
      false ->
        sumo_internal:set_field(links, [], Doc)
    end,

  Links = sumo_internal:get_field(links, OldDoc),

  DocWithLink =
    case Title of
      null -> OldDoc;
      _ -> sumo_internal:set_field(links, [Link | Links], Doc)
    end,

  maps:put(CategoryId, DocWithLink, DocsMap).

-spec get_docs(pid(), iodata(), [term()], atom()) ->
  {ok, sumo:docs()} | {error, term()}.
get_docs(Conn, Sql, Values, DocName) ->
  case pgsql:equery(Conn, stringify(Sql), Values) of
    {ok, Columns, Rows} ->
      ColFun = fun(Col) -> binary_to_atom(Col#column.name, utf8) end,
      ColumnNames = lists:map(ColFun, Columns),

      FoldFun = fun({Name, Value}, Doc) ->
                    sumo_internal:set_field(Name, Value, Doc)
                end,
      RowFun = fun(Row) ->
                   Fields = tuple_to_list(Row),
                   Pairs = lists:zip(ColumnNames, Fields),
                   NewDoc = sumo_internal:new_doc(DocName),
                   lists:foldl(FoldFun, NewDoc, Pairs)
               end,
      Docs = lists:map(RowFun, Rows),
      {ok, Docs};
    {error, Error} ->
      {error, Error}
  end.

-spec get_maps(pid(), iodata(), [term()]) ->
  {ok, sumo:docs()} | {error, term()}.
get_maps(Conn, Sql, Values) ->
  case pgsql:equery(Conn, stringify(Sql), Values) of
    {ok, Columns, Rows} ->
      ColFun = fun(Col) -> binary_to_atom(Col#column.name, utf8) end,
      ColumnNames = lists:map(ColFun, Columns),

      RowFun = fun(Row) ->
                   Fields = tuple_to_list(Row),
                   Pairs = lists:zip(ColumnNames, Fields),
                   maps:from_list(Pairs)
               end,
      {ok, lists:map(RowFun, Rows)};
    {error, Error} ->
      {error, Error}
  end.

%% @todo remove this once pgsql specs are fixed to support iodata and make
%%       dialyzer happy
stringify(Sql) -> binary_to_list(iolist_to_binary(Sql)).
