%%% @doc Articles repository
-module(erlorg_articles_repo).
-author('juan@inaka.net').

-export([
         create/10,
         list/1,
         list/2,
         fetch_by_type/1,
         by_tag/2,
         by_user/2,
         by_year/2,
         by_id/1,
         filters/2
        ]
       ).

-type filter() :: #{type => tag | user | year,
                    value => binary(),
                    count => integer()
                   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create (integer(), binary(), binary(), binary(), binary(),
              binary(), binary(), integer(), boolean(),
              erlorg_datetime:datetime()) -> erlorg_articles:article().
create(TypeId, Title, Lead, TwitterStatus, Content,
       Attachment, Tags, UserId, Visible,
       ArticleDate) ->
  Article = erlorg_articles:new(TypeId, Title, Lead, TwitterStatus, Content,
                                Attachment, Tags, UserId, Visible,
                                ArticleDate),
  SavedArticle = sumo:persist(erlorg_articles, Article),
  by_id(erlorg_articles:id(SavedArticle)).

-spec list(erlorg_articles:type()) -> [erlorg_articles:article()].
list(Type) -> list(Type, 0).

-spec list(erlorg_articles:type(), non_neg_integer()) ->
  [erlorg_articles:article()].
list(Type, Limit) ->
  sumo:call(erlorg_articles, list, [Type, Limit]).

-spec fetch_by_type(erlorg_articles:type()) ->
  erlorg_articles:article() | undefined.
fetch_by_type(Type) ->
  case erlorg_articles_repo:list(Type, 1) of
    [Article] -> Article;
    [] -> undefined
  end.

-spec by_tag(erlorg_articles:type(), binary()) ->
  [erlorg_articles:article()].
by_tag(Type, Tag) ->
  sumo:call(erlorg_articles, by_tag, [Type, Tag]).

-spec by_year(erlorg_articles:type(), binary()) ->
  [erlorg_articles:article()].
by_year(Type, Year) ->
  sumo:call(erlorg_articles, by_year, [Type, Year]).

-spec by_user(erlorg_articles:type(), binary()) ->
  [erlorg_articles:article()].
by_user(Type, User) ->
  sumo:call(erlorg_articles, by_user, [Type, User]).

-spec by_id(integer()) -> erlorg_articles:article().
by_id(Id) ->
  case sumo:call(erlorg_articles, by_id, [Id]) of
    [] -> not_found;
    [Article] -> Article
  end.

-spec filters(erlorg_articles:type(), tag | year | users) -> [filter()].
filters(Type, tag) ->
  sumo:call(erlorg_articles, tag_filters, [Type]);
filters(Type, year) ->
  sumo:call(erlorg_articles, year_filters, [Type]);
filters(Type, user) ->
  sumo:call(erlorg_articles, user_filters, [Type]).
