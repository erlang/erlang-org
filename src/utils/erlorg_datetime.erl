%%% @doc Datetime utils
-module(erlorg_datetime).

-type datetime() ::
        {
          {non_neg_integer(), 1..12, 1..31},
          {0..23, 0..59, 0..59}
        }.

-export_type([datetime/0]).

-export([
         now/0,
         seconds_to_integer/1,
         printable_date/1,
         full_printable_date/2,
         full_printable_date/1
        ]
       ).

-spec now() -> datetime().
now() -> calendar:universal_time().

-spec seconds_to_integer(datetime()) -> datetime().
seconds_to_integer({{Y, M, D}, {H, Mi, S}}) ->
  {{Y, M, D}, {H, Mi, erlang:trunc(S)}}.

-spec printable_date(map() | [map()]) -> map() | [map()].
printable_date(Elements) when is_list(Elements) ->
  [printable_date(E) || E <- Elements];
printable_date(Article) when is_map(Article) ->
  ArticleDate = erlorg_articles:article_date(Article),
  erlorg_articles:article_date(printable_date(ArticleDate), Article);
printable_date(Date) when is_tuple(Date) ->
  qdate:to_string("j M Y", Date).

-spec full_printable_date(map() | [map()], article | release) ->
  map() | [map()].
full_printable_date(Elements, Type) when is_list(Elements) ->
  [full_printable_date(E, Type) || E <- Elements];
full_printable_date(Article, article) when is_map(Article) ->
  ArticleDate = erlorg_articles:article_date(Article),
  erlorg_articles:article_date(full_printable_date(ArticleDate), Article);
full_printable_date(Release, release) when is_map(Release) ->
  ReleaseDate = erlorg_releases:date(Release),
  erlorg_releases:date(full_printable_date(ReleaseDate), Release).

-spec full_printable_date(tuple()) -> string().
full_printable_date(Date) when is_tuple(Date) ->
  qdate:to_string("D, j M Y H:i:s e", Date).
