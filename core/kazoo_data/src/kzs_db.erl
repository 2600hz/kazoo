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
         ,db_view_update/4
         ,db_info/1
         ,db_info/2
         ,db_exists/2, db_exists_all/2
         ,db_archive/3
         ,db_list/2
        ]).

-include_lib("kazoo_data/src/kz_data.hrl").

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
    do_db_create(Map, DbName, Options)
        andalso lists:all(fun({_Tag, M1}) ->
                                  do_db_create(#{server => M1}, DbName, Options)
                          end, Others)
        andalso kzs_publish:maybe_publish_db(DbName, 'created') =:= 'ok'.

-spec do_db_create(map(), ne_binary(), db_create_options()) -> boolean().
do_db_create(#{server := {App, Conn}}, DbName, Options) ->
    case App:db_exists(Conn, DbName) of
        'false' -> App:db_create(Conn, DbName, Options);
        'true' -> 'true'
    end.

-spec db_delete(map(), ne_binary()) -> boolean().
db_delete(#{}=Map, DbName) ->
    Others = maps:get('others', Map, []),
    do_db_delete(Map, DbName)
        andalso lists:all(fun({_Tag, M1}) ->
                                  do_db_delete(#{server => M1}, DbName)
                          end, Others)
        andalso kzs_publish:maybe_publish_db(DbName, 'deleted') =:= 'ok'.

-spec do_db_delete(map(), ne_binary()) -> boolean().
do_db_delete(#{server := {App, Conn}}, DbName) ->
    App:db_delete(Conn, DbName).

-spec db_replicate(map(), kz_json:object() | kz_proplist()) ->
                                {'ok', kz_json:object()} |
                                data_error().
db_replicate(#{server := {App, Conn}}, Prop) ->
    App:db_replicate(Conn,Prop).

-spec db_view_cleanup(map(), ne_binary()) -> boolean().
db_view_cleanup(#{}=Map, DbName) ->
    Others = maps:get('others', Map, []),
    do_db_view_cleanup(Map, DbName)
        andalso lists:all(fun({_Tag, M1}) ->
                                  do_db_view_cleanup(#{server => M1}, DbName)
                          end, Others).

-spec do_db_view_cleanup(map(), ne_binary()) -> boolean().
do_db_view_cleanup(#{server := {App, Conn}}, DbName) ->
    App:db_view_cleanup(Conn, DbName).

-spec db_info(map()) -> {'ok', ne_binaries()} |data_error().
db_info(#{server := {App, Conn}}) -> App:db_info(Conn).

-spec db_info(map(), ne_binary()) -> {'ok', kz_json:object()} | data_error().
db_info(#{server := {App, Conn}}, DbName) -> App:db_info(Conn, DbName).

-spec db_exists(map(), ne_binary()) -> boolean().
db_exists(#{server := {App, Conn}}, DbName) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {'database', {App, Conn}, DbName}) of
        {'ok', Exists} -> Exists;
        _ ->
            Exists = App:db_exists(Conn, DbName),
            Props = [{'origin', {'db', DbName}}],
            kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {'database', {App, Conn}, DbName}, Exists, Props),
            Exists
    end.

-spec db_exists_all(map(), ne_binary()) -> boolean().
db_exists_all(Map, DbName) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {'database', DbName}) of
        {'ok', Exists} -> Exists;
        _ ->
            Exists = db_exists(Map, DbName)
                andalso db_exists_others(DbName, maps:get('others', Map, [])),
            Props = [{'origin', {'db', DbName}}],
            kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {'database', DbName}, Exists, Props),
            Exists
    end.

-spec db_exists_others(ne_binary(), list()) -> boolean().
db_exists_others(_, []) -> 'true';
db_exists_others(DbName, Others) ->
    lists:all(fun({_Tag, M}) -> db_exists(#{server => M}, DbName) end, Others).

-spec db_archive(map(), ne_binary(), ne_binary()) -> boolean().
db_archive(#{server := {App, Conn}}=Server, DbName, Filename) ->
    case db_exists(Server, DbName) of
        'true' -> App:db_archive(Conn, DbName, Filename);
        'false' -> 'ok'
    end.

-spec db_list(map(), view_options()) -> {'ok', ne_binaries()} | data_error().
db_list(#{server := {App, Conn}}=Map, Options) ->
    db_list_all(App:db_list(Conn, Options), Options, maps:get('others', Map, [])).

db_list_all(DBs, _Options, []) -> DBs;
db_list_all({'ok', DBs}, Options, Others) ->
    {_, DBList} = lists:foldl(fun db_list_all_fold/2, {Options, DBs}, Others),
    DBList.

db_list_all_fold({_Tag, Server}, {Options, DBs}) ->
    {'ok', DBList} = db_list(Server, Options),
    {Options, lists:usort(DBs ++ DBList)}.

-spec db_view_update(map(), ne_binary(), kz_proplist(), boolean()) -> boolean().
db_view_update(#{}=Map, DbName, Views, Remove) ->
    Others = maps:get('others', Map, []),
    do_db_view_update(Map, DbName, Views, Remove)
        andalso lists:all(fun({_Tag, M1}) ->
                                  do_db_view_update(#{server => M1}, DbName, Views, Remove)
                          end, Others).

-spec do_db_view_update(map(), ne_binary(), kz_proplist(), boolean()) -> boolean().
do_db_view_update(#{server := {App, Conn}}=Server, Db, Views, Remove) ->
    case kzs_view:all_design_docs(Server, Db, ['include_docs']) of
        {'ok', Found} -> update_views(Found, Db, Views, Remove, Server);
        {'error', _R} ->
            case App:db_exists(Conn, Db) of
                'true' -> update_views([], Db, Views, Remove, Server);
                'false' -> lager:error("error fetching current views for db ~s", [Db]),
                           'true'
            end
    end.

-spec update_views(kz_json:objects(), ne_binary(), kz_proplist(), boolean(), map()) -> boolean().

update_views([], _, [], _, _) -> 'true';
update_views([], Db, [{Id,View}|Views], Remove, Server) ->
    lager:debug("adding view '~s' to '~s'", [Id, Db]),
    _ = kzs_doc:ensure_saved(Server, Db, View, []),
    update_views([], Db, Views, Remove, Server);
update_views([Found|Finds], Db, Views, Remove, Server) ->
    Id = kz_doc:id(Found),
    Doc = kz_json:get_value(<<"doc">>, Found),
    RawDoc = kz_doc:delete_revision(Doc),
    case props:get_value(Id, Views) of
        'undefined' when Remove ->
            lager:debug("removing view '~s' from '~s'", [Id, Db]),
            _ = kzs_doc:del_doc(Server, Db, Doc, []),
            update_views(Finds, Db, props:delete(Id, Views), Remove, Server);
        'undefined' ->
            update_views(Finds, Db, props:delete(Id, Views), Remove, Server);
        View1 when View1 =:= RawDoc ->
            lager:debug("view '~s' matches the raw doc, skipping", [Id]),
            update_views(Finds, Db, props:delete(Id, Views), Remove, Server);
        View2 ->
            lager:debug("updating view '~s' in '~s'", [Id, Db]),
            Rev = kz_doc:revision(Doc),
            _ = kzs_doc:ensure_saved(Server, Db, kz_doc:set_revision(View2, Rev), []),
            update_views(Finds, Db, props:delete(Id, Views), Remove, Server)
    end.
