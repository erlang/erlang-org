%%% @doc Events model
-module(erlorg_articles).
-author('juan@inaka.net').

-behaviour(sumo_doc).

-opaque article() ::
          #{
             id              => integer(),
             title           => binary(),
             lead            => binary(),
             twitter_status  => binary(),
             content         => binary(),
             attachment      => binary(),
             tags            => [binary()],
             author          => binary(),
             admin_user_id   => integer(),
             article_type_id => integer(),
             visible         => boolean(),
             article_date    => erlorg_datetime:datetime(),
             created_at      => erlorg_datetime:datetime(),
             updated_at      => erlorg_datetime:datetime()
           }.

-opaque type() ::
          news | event | workshop | euc | community | about |
          'home header' | 'home getting started' | 'home footer'.

-export_type([article/0, type/0]).

-export([new/10]).
-export([id/1, article_date/1, article_date/2, article_type/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id,              integer,  [id, auto_increment, not_null])
    , sumo:new_field(title,           string,   [{length, 255}, not_null])
    , sumo:new_field(lead,            string,   [{length, 255}])
    , sumo:new_field(twitter_status,  string,   [{length, 255}])
    , sumo:new_field(content,         text,     [not_null])
    , sumo:new_field(attachment,      string,   [{length, 255}])
    , sumo:new_field(tags,            string,   [{length, 255}])
    , sumo:new_field(admin_user_id,   integer,  [not_null])
    , sumo:new_field(article_type_id, integer,  [not_null])
    , sumo:new_field(visible,         integer,  [not_null])
    , sumo:new_field(article_date,    datetime, [not_null])
    , sumo:new_field(created_at,      datetime, [not_null])
    , sumo:new_field(updated_at,      datetime, [not_null])
    ]).

-spec sumo_sleep(article()) -> sumo:doc().
sumo_sleep(Article) ->
  #{tags := Tags,
    visible := Visible} = Article,

  TagsBin = erlorg_binary:join(Tags, <<",">>),
  VisibleInt = case Visible of true -> 1; false -> 0 end,

  Article#{tags => TagsBin, visible => VisibleInt}.

-spec sumo_wakeup(sumo:doc()) -> article().
sumo_wakeup(Doc) ->
  #{ tags := Tags
   , article_date := ArticleDate
   , created_at := CreatedAt
   , updated_at := UpdatedAt} = Doc,


  TagsList = case Tags of
               <<>> -> [];
               _ -> binary:split(Tags, <<",">>, [global])
             end,

  Doc#{ tags => TagsList
      , article_date => erlorg_datetime:seconds_to_integer(ArticleDate)
      , created_at => erlorg_datetime:seconds_to_integer(CreatedAt)
      , updated_at => erlorg_datetime:seconds_to_integer(UpdatedAt)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (integer(), binary(), binary(), binary(), binary(),
           binary(), [binary()], integer(), boolean(),
           erlorg_datetime:datetime()) -> article().
new(TypeId, Title, Lead, TwitterStatus, Content,
    Attachment, Tags, UserId, Visible, ArticleDate) ->
  Now = erlorg_datetime:now(),
  #{ id              => undefined
   , title           => Title
   , lead            => Lead
   , twitter_status  => TwitterStatus
   , content         => Content
   , attachment      => Attachment
   , tags            => Tags
   , admin_user_id   => UserId
   , article_type_id => TypeId
   , visible         => Visible
   , article_date    => ArticleDate
   , created_at      => Now
   , updated_at      => Now
   }.

-spec id(article()) -> integer().
id(#{id := Id}) -> Id.

-spec article_date(article()) -> integer().
article_date(#{article_date := ArticleDate}) -> ArticleDate.

-spec article_date(erlorg_datetime:datetime(), article()) -> article().
article_date(ArticleDate, Article) ->
  Article#{article_date => ArticleDate}.

-spec article_type(binary(), [article()]) -> article().
article_type(ArticleType, Articles) when is_list(Articles) ->
  [article_type(ArticleType, A) || A <- Articles];
article_type(ArticleType, Article) when is_map(Article) ->
  Article#{article_type => ArticleType}.
