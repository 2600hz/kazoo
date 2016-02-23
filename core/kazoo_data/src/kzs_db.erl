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
        ]).


-include("kz_data.hrl").


%%% DB-related functions ---------------------------------------------
-spec db_compact(server(), ne_binary()) -> boolean().
db_compact({App, Conn}, DbName) ->
    App:db_compact(Conn, DbName).

-spec db_create(server(), ne_binary()) -> boolean().
db_create(Server, DbName) ->
    db_create(Server, DbName, []).

-spec db_create(server(), ne_binary(), db_create_options()) -> boolean().
db_create({App, Conn}, DbName, Options) ->
    %%TODO storage policy
    App:db_create(Conn, DbName, Options) andalso
        kzs_publish:maybe_publish_db(DbName, 'created') =:= 'ok'.

-spec db_delete(server(), ne_binary()) -> boolean().
db_delete({App, Conn}, DbName) ->
    App:db_delete(Conn, DbName) andalso
        kzs_publish:maybe_publish_db(DbName, 'deleted') =:= 'ok'.

-spec db_replicate(server(), wh_json:object() | wh_proplist()) ->
                                {'ok', wh_json:object()} |
                                data_error().
db_replicate({App, Conn}, Prop) ->
    App:db_replicate(Conn,Prop).

-spec db_view_cleanup(server(), ne_binary()) -> boolean().
db_view_cleanup({App, Conn}, DbName) -> App:db_view_cleanup(Conn, DbName).

-spec db_info(server()) -> {'ok', ne_binaries()} |data_error().
db_info({App, Conn}) -> App:db_info(Conn).

-spec db_info(server(), ne_binary()) -> {'ok', wh_json:object()} | data_error().
db_info({App, Conn}, DbName) -> App:db_info(Conn, DbName).

-spec db_exists(server(), ne_binary()) -> boolean().
db_exists({App, Conn}, DbName) -> App:db_exists(Conn, DbName).

-spec db_archive(server(), ne_binary(), ne_binary()) -> boolean().
db_archive({App, Conn}, DbName, Filename) -> App:db_archive(Conn, DbName, Filename).
