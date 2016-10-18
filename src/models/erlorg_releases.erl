%%% @doc Releases model
-module(erlorg_releases).
-author('juan@inaka.net').

-behaviour(sumo_doc).

-opaque file_info() ::
          #{
             name => binary(),
             size => float(),
             label => binary()
           }.

-opaque files() ::
          #{
             source   => file_info(),
             binary   => file_info(),
             doc_html => file_info(),
             doc_man  => file_info()
           }.

-opaque release() ::
          #{
             id         => integer(),
             title      => binary(),
             version    => binary(),
             note       => binary(),
             content    => binary(),
             date       => erlorg_datetime:datetime(),
             files      => files(),
             created_at => erlorg_datetime:datetime(),
             updated_at => erlorg_datetime:datetime()
           }.

-export_type([release/0, files/0, file_info/0]).

-export([new/6]).
-export([version/1, date/1, date/2, updated_at/1, updated_at/2]).
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id,             integer,  [id, auto_increment, not_null])
    , sumo:new_field(title,          string,   [{length, 255}, not_null])
    , sumo:new_field(version,        string,   [{length, 255}, not_null])
    , sumo:new_field(note,           string,   [{length, 1024}, not_null])
    , sumo:new_field(content,        text,     [not_null])
    , sumo:new_field(date,           datetime, [not_null])
    , sumo:new_field(readme,         string,   [{length, 255}, not_null])
    , sumo:new_field(readme_label,   string,   [{length, 255}, not_null])
    , sumo:new_field(source,         string,   [{length, 255}, not_null])
    , sumo:new_field(source_size,    string,   [{length, 255}, not_null])
    , sumo:new_field(source_label,   string,   [{length, 255}, not_null])
    , sumo:new_field(binary,         string,   [{length, 255}, not_null])
    , sumo:new_field(binary_size,    string,   [{length, 255}, not_null])
    , sumo:new_field(binary_label,   string,   [{length, 255}, not_null])
    , sumo:new_field(binary64,       string,   [{length, 255}])
    , sumo:new_field(binary64_size,  string,   [{length, 255}])
    , sumo:new_field(binary64_label, string,   [{length, 255}, not_null])
    , sumo:new_field(doc_html,       string,   [{length, 255}, not_null])
    , sumo:new_field(doc_html_size,  string,   [{length, 255}, not_null])
    , sumo:new_field(doc_html_label, string,   [{length, 255}, not_null])
    , sumo:new_field(doc_man,        string,   [{length, 255}, not_null])
    , sumo:new_field(doc_man_size,   string,   [{length, 255}, not_null])
    , sumo:new_field(doc_man_label,  string,   [{length, 255}, not_null])
    , sumo:new_field(created_at,     datetime, [not_null])
    , sumo:new_field(updated_at,     datetime, [not_null])
    ]).

-spec sumo_sleep(release()) -> sumo:doc().
sumo_sleep(Release) ->
  #{ id          := Id
   , title       := Title
   , version     := Version
   , note        := Note
   , content     := Content
   , date        := Date
   , files       := Files
   , created_at  := CreatedAt
   , updated_at  := UpdatedAt
   } = Release,

  #{ readme   := #{name := Readme, label := ReadmeLabel}
   , source   := #{name := Source, size := SourceSize, label := SourceLabel}
   , binary   := #{name := Binary, size := BinarySize, label := BinaryLabel}
   , doc_html := #{name := DocHtml, size := DocHtmlSize, label := DocHtmlLabel}
   , doc_man  := #{name := DocMan, size := DocManSize, label := DocManLabel}
   } = Files,

  {Binary64, Binary64Size, Binary64Label} =
    case maps:get(binary64, Files, undefined) of
      undefined ->
        {undefined, undefined, <<"">>};
      #{name := Name, size := Size, label := Label} ->
        {Name, Size, Label}
    end,

  #{ id             => Id
   , title          => Title
   , version        => Version
   , note           => Note
   , content        => Content
   , date           => Date
   , readme         => Readme
   , readme_label   => ReadmeLabel
   , source         => Source
   , source_size    => SourceSize
   , source_label   => SourceLabel
   , binary         => Binary
   , binary_size    => BinarySize
   , binary_label   => BinaryLabel
   , binary64       => Binary64
   , binary64_size  => Binary64Size
   , binary64_label => Binary64Label
   , doc_html       => DocHtml
   , doc_html_size  => DocHtmlSize
   , doc_html_label => DocHtmlLabel
   , doc_man        => DocMan
   , doc_man_size   => DocManSize
   , doc_man_label  => DocManLabel
   , created_at     => CreatedAt
   , updated_at     => UpdatedAt
   }.

-spec sumo_wakeup(sumo:doc()) -> release().
sumo_wakeup(Doc) ->
  #{ id             := Id
   , title          := Title
   , version        := Version
   , note           := Note
   , content        := Content
   , date           := Date
   , readme         := Readme
   , readme_label   := ReadmeLabel
   , source         := Source
   , source_size    := SourceSize
   , source_label   := SourceLabel
   , binary         := Binary
   , binary_size    := BinarySize
   , binary_label   := BinaryLabel
   , binary64       := Binary64
   , binary64_size  := Binary64Size
   , binary64_label := Binary64Label
   , doc_html       := DocHtml
   , doc_html_size  := DocHtmlSize
   , doc_html_label := DocHtmlLabel
   , doc_man        := DocMan
   , doc_man_size   := DocManSize
   , doc_man_label  := DocManLabel
   , created_at     := CreatedAt
   , updated_at     := UpdatedAt
   } = Doc,

  Files = #{ readme   => #{name => Readme,
                           label => ReadmeLabel}
           , source   => #{name => Source,
                           size => SourceSize,
                           label => SourceLabel}
           , binary   => #{name => Binary,
                           size => BinarySize,
                           label => BinaryLabel}
           , binary64 => #{name => Binary64,
                           size => Binary64Size,
                           label => Binary64Label}
           , doc_html => #{name => DocHtml,
                           size => DocHtmlSize,
                           label => DocHtmlLabel}
           , doc_man  => #{name => DocMan,
                           size => DocManSize,
                           label => DocManLabel}
           },

  #{ id          => Id
   , title       => Title
   , version     => Version
   , note        => Note
   , content     => Content
   , date        => erlorg_datetime:seconds_to_integer(Date)
   , files       => Files
   , created_at  => erlorg_datetime:seconds_to_integer(CreatedAt)
   , updated_at  => erlorg_datetime:seconds_to_integer(UpdatedAt)
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new (binary(), binary(), binary(), binary(),
           erlorg_datetime:datetime(), files()) -> release().
new(Title, Version, Note, Content, Date, Files) ->
  Now = erlorg_datetime:now(),
  #{ id          => undefined
   , title       => Title
   , version     => Version
   , note        => Note
   , content     => Content
   , date        => Date
   , files       => Files
   , created_at  => Now
   , updated_at  => Now
   }.

-spec version(release()) -> binary().
version(#{version := Version}) -> Version.

-spec date(release()) -> binary().
date(#{date := Date}) -> Date.

-spec date(erlorg_datetime:datetime(), release()) -> release().
date(ReleaseDate, Release) ->
  Release#{date => ReleaseDate}.

-spec updated_at(release()) -> erlorg_datetime:datetime().
updated_at(#{updated_at := UpdatedAt}) -> UpdatedAt.

-spec updated_at(erlorg_datetime:datetime(), release()) -> release().
updated_at(ReleaseUpdatedAt, Release) ->
  Release#{updated_at => ReleaseUpdatedAt}.
