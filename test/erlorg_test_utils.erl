-module(erlorg_test_utils).

-export([
         create_release/1,
         create_release/2,
         create_release/3,
         update_release_updateat/2,
         create_user/1,
         delete_all_users/0,
         article_type_id/1,
         link_group_id/1
        ]).

-spec create_release(binary()) -> erlorg_releases:release().
create_release(Version) ->
  create_release(Version, erlorg_datetime:now(), false).

-spec create_release(binary(), erlorg_datetime:datetime()) ->
  erlorg_releases:release().
create_release(Version, Date) ->
  create_release(Version, Date, false).

-spec create_release(binary(), erlorg_datetime:datetime(), boolean()) ->
  erlorg_releases:release().
create_release(Version, Date, IncludeBin64) ->
  Files = #{ readme   => #{name => <<"otp_src_", Version/binary, ".readme">>,
                           label => <<"Readme ", Version/binary>>}
           , source   => #{name => <<"otp_src_", Version/binary, ".tar.gz">>,
                           size => <<"64.0 MB">>,
                           label => <<"Source ", Version/binary>>}
           , binary   => #{name => <<"otp_win32_", Version/binary, ".exe">>,
                           size => <<"91.1 MB">>,
                           label => <<"Binary ", Version/binary>>}
           , doc_html => #{name => <<"otp_doc_html_",
                                     Version/binary, ".tar.gz">>,
                           size => <<"31.9 MB">>,
                           label => <<"Doc Html ", Version/binary>>}
           , doc_man  => #{name => <<"otp_doc_man_",
                                     Version/binary, ".tar.gz">>,
                           size => <<"1.2 MB">>,
                           label => <<"Doc Man ", Version/binary>>}
           },

  Files1 =
    case IncludeBin64 of
      true ->
        Binary64Map =
          #{binary64 => #{name => <<"otp_win64_", Version/binary, ".exe">>,
                          size => <<"120.7 MB">>,
                          label => <<"Binary64 ", Version/binary>>}},
        maps:merge(Files, Binary64Map);
      false ->
        Files
    end,
  erlorg_releases_repo:create(<<"OTP ", Version/binary>>, Version,
                              <<"OTP ", Version/binary, " has been released!">>,
                              <<"Content">>,
                              Date,
                              Files1).

-spec update_release_updateat(erlorg_datetime:datetime(),
                          erlorg_releases:release()) ->
  erlorg_releases:release().
update_release_updateat(Date, Release) ->
  NewRelease = erlorg_releases:updated_at(Date, Release),
  sumo:persist(erlorg_releases, NewRelease).

-spec create_user(binary()) -> integer().
create_user(Username) ->
  Sql = ["INSERT INTO admin_users "
         "  (email, name, created_at, updated_at) "
         "VALUES "
         "  ($1, $2, $3, $4) "
        "RETURNING id"],
  Now = erlorg_datetime:now(),
  Values = [Username, Username, Now, Now],
  Conn = sumo_backend_pgsql:get_connection(erlorg_pgsql_backend),
  case pgsql:equery(Conn, stringify(Sql), Values) of
    {ok, _Count, _Columns, Rows} ->
      {LastId} = hd(Rows),
      {ok, LastId};
    {error, Error} ->
      {error, Error}
  end.

-spec delete_all_users() -> integer().
delete_all_users() ->
  Sql = "DELETE FROM admin_users",
  Conn = sumo_backend_pgsql:get_connection(erlorg_pgsql_backend),
  case pgsql:equery(Conn, Sql, []) of
    {error, Error} ->
      {error, Error};
    _ ->
      ok
  end.

-spec article_type_id(atom()) -> integer().
article_type_id(TypeName) ->
  Sql = ["SELECT t.id "
         "FROM   erlorg_article_types AS t "
         "WHERE  lower(t.name) = $1 "],
  Values = [TypeName],
  Conn = sumo_backend_pgsql:get_connection(erlorg_pgsql_backend),
  case pgsql:equery(Conn, stringify(Sql), Values) of
    {ok, _Count, [{ArticleTypeId}]} ->
      {ok, ArticleTypeId};
    {ok, _Count, _Rows} ->
      {error, notfound};
    {error, Error} ->
      {error, Error}
  end.

-spec link_group_id(atom()) -> integer().
link_group_id(GroupName) ->
  Sql = ["SELECT lg.id "
         "FROM   erlorg_link_groups AS lg "
         "WHERE  lower(lg.name) = $1 "],
  Values = [GroupName],
  Conn = sumo_backend_pgsql:get_connection(erlorg_pgsql_backend),
  case pgsql:equery(Conn, stringify(Sql), Values) of
    {ok, _Count, [{LinkGroupId}]} ->
      {ok, LinkGroupId};
    {ok, _Count, _Rows} ->
      {error, notfound};
    {error, Error} ->
      {error, Error}
  end.

%% @todo remove this once pgsql specs are fixed to support iodata and make
%%       dialyzer happy
stringify(Sql) -> binary_to_list(iolist_to_binary(Sql)).
