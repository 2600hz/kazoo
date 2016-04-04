%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_db).


%% DB operations
-export([db_compact/2
         ,db_create/2
         ,db_create/3
         ,db_delete/2
         ,db_replicate/2
         ,db_view_cleanup/2
         ,db_info/1
         ,db_info/2
         ,db_exists/2
         ,db_archive/3
         ,db_list/2
        ]).


-include("kz_data.hrl").


%%% DB-related functions ---------------------------------------------
-spec db_compact(map(), ne_binary()) -> boolean().
db_compact(#{server := {App, Conn}}, DbName) ->
    App:db_compact(Conn, DbName).

-spec db_create(map(), ne_binary()) -> boolean().
db_create(Server, DbName) ->
    db_create(Server, DbName, []).

-spec db_create(map(), ne_binary(), db_create_options()) -> boolean().
db_create(#{}=Map, DbName, Options) ->
    %%TODO storage policy
    Others = maps:get('others', Map, []),
    do_db_create(Map, DbName, Options) andalso
        lists:all(fun({_Tag, M1}) ->
                          do_db_create(#{server => M1}, DbName, Options)
                  end, Others) andalso
        kzs_publish:maybe_publish_db(DbName, 'created') =:= 'ok'.

-spec do_db_create(map(), ne_binary(), db_create_options()) -> boolean().
do_db_create(#{server := {App, Conn}}, DbName, Options) ->
    case App:db_exists(Conn, DbName) of
        'false' -> App:db_create(Conn, DbName, Options);
        'true' -> 'true'
    end.

-spec db_delete(map(), ne_binary()) -> boolean().
db_delete(#{}=Map, DbName) ->
    Others = maps:get('others', Map, []),
    do_db_delete(Map, DbName) andalso
        lists:all(fun({_Tag, M1}) ->
                          do_db_delete(#{server => M1}, DbName)
                  end, Others) andalso
        kzs_publish:maybe_publish_db(DbName, 'deleted') =:= 'ok'.

-spec do_db_delete(map(), ne_binary()) -> boolean().
do_db_delete(#{server := {App, Conn}}, DbName) ->
    App:db_delete(Conn, DbName).

-spec db_replicate(map(), wh_json:object() | wh_proplist()) ->
                                {'ok', wh_json:object()} |
                                data_error().
db_replicate(#{server := {App, Conn}}, Prop) ->
    App:db_replicate(Conn,Prop).

-spec db_view_cleanup(map(), ne_binary()) -> boolean().
db_view_cleanup(#{}=Map, DbName) ->
    Others = maps:get('others', Map, []),
    do_db_view_cleanup(Map, DbName) andalso
        lists:all(fun({_Tag, M1}) ->
                          do_db_view_cleanup(#{server => M1}, DbName)
                  end, Others).

-spec do_db_view_cleanup(map(), ne_binary()) -> boolean().
do_db_view_cleanup(#{server := {App, Conn}}, DbName) ->
    App:db_view_cleanup(Conn, DbName).

-spec db_info(map()) -> {'ok', ne_binaries()} |data_error().
db_info(#{server := {App, Conn}}) -> App:db_info(Conn).

-spec db_info(map(), ne_binary()) -> {'ok', wh_json:object()} | data_error().
db_info(#{server := {App, Conn}}, DbName) -> App:db_info(Conn, DbName).

-spec db_exists(map(), ne_binary()) -> boolean().
db_exists(#{server := {App, Conn}}=Map, DbName) ->
    App:db_exists(Conn, DbName) andalso
        lists:all(fun({_Tag, M}) -> db_exists(#{server => M}, DbName) end, maps:get('others', Map, [])).

-spec db_archive(map(), ne_binary(), ne_binary()) -> boolean().
db_archive(#{server := {App, Conn}}, DbName, Filename) -> App:db_archive(Conn, DbName, Filename).


-spec db_list(map(), view_options()) -> {'ok', ne_binaries()} | data_error().
db_list(#{server := {App, Conn}}, Options) ->
    App:db_list(Conn, Options).
