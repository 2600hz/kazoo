%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
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
        ,db_import/3
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
            case App:db_exists(Conn, DbName) of
                {'error', 'resource_not_available'} -> 'true';
                Exists ->
                    Props = [{'origin', {'db', DbName}}],
                    kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {'database', {App, Conn}, DbName}, Exists, Props),
                    Exists
            end
    end.

-spec db_exists_all(map(), ne_binary()) -> boolean().
db_exists_all(Map, DbName) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {'database', DbName}) of
        {'ok', Exists} -> Exists;
        _ -> db_exists(Map, DbName)
                 andalso db_exists_others(DbName, maps:get('others', Map, []))
    end.

-spec db_exists_others(ne_binary(), list()) -> boolean().
db_exists_others(_, []) -> 'true';
db_exists_others(DbName, Others) ->
    lists:all(fun({_Tag, M}) -> db_exists(#{server => M}, DbName) end, Others).

-spec db_archive(map(), ne_binary(), ne_binary()) -> 'ok' | data_error().
db_archive(#{server := {App, Conn}}=Server, DbName, Filename) ->
    case db_exists(Server, DbName) of
        'true' -> App:db_archive(Conn, DbName, Filename);
        'false' -> 'ok'
    end.

-spec db_import(map(), ne_binary(), ne_binary()) -> 'ok' | data_error().
db_import(#{server := {App, Conn}}=Server, DbName, Filename) ->
    case db_exists(Server, DbName) of
        'true' -> App:db_import(Conn, DbName, Filename);
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
do_db_view_update(#{server := {App, Conn}}=Server, Db, NewViews, Remove) ->
    case kzs_view:all_design_docs(Server, Db, ['include_docs']) of
        {'ok', JObjs} ->
            CurrentViews = proplistize_current_views(JObjs),
            add_update_remove_views(Server, Db, CurrentViews, NewViews, Remove);
        {'error', _R} ->
            case App:db_exists(Conn, Db) of
                'true' -> add_update_remove_views(Server, Db, [], NewViews, Remove);
                'false' -> lager:error("error fetching current views for db ~s", [Db]),
                           'true'
            end
    end.

-spec proplistize_current_views(kz_json:objects()) -> kz_proplist().
proplistize_current_views(JObjs) ->
    [{kz_doc:id(JObj), kz_json:get_value(<<"doc">>, JObj)} || JObj <- JObjs].

-spec add_update_remove_views(map(), ne_binary(), kz_proplist(), kz_proplist(), boolean()) -> 'true'.
add_update_remove_views(Server, Db, CurrentViews, NewViews, Remove) ->
    Current = sets:from_list([Id || {Id, _} <- CurrentViews]),
    New = sets:from_list([Id || {Id, _} <- NewViews]),
    Add = sets:to_list(sets:subtract(New, Current)),
    Update = sets:to_list(sets:intersection(Current, New)),
    Delete = sets:to_list(sets:subtract(Current, New)),
    lager:debug("view updates found ~p new, ~p possible updates and ~p potential removals for db ~s"
               ,[length(Add), length(Update), length(Delete), Db]
               ),
    Conflicts = add_views(Server, Db, Add, NewViews),
    lager:debug("view additions resulted in ~p conflicts", [length(Conflicts)]),
    Errors = update_views(Server, Db, Update ++ Conflicts, CurrentViews, NewViews),
    lager:debug("view updates resulted in ~p conflicts", [length(Errors)]),
    correct_view_errors(Server, Db, Errors, NewViews),
    case Remove of
        'true' -> delete_views(Server, Db, Delete, CurrentViews);
        'false' -> 'true'
    end.

-spec add_views(map(), ne_binary(), ne_binaries(), kz_proplist()) ->
                       api_binaries().
add_views(Server, Db, Add, NewViews) ->
    Views = [props:get_value(Id, NewViews) || Id <- Add],
    {'ok', JObjs} = kzs_doc:save_docs(Server, Db, Views, []),
    [kz_doc:id(JObj)
     || JObj <- JObjs,
        <<"conflict">> =:= kz_json:get_value(<<"error">>, JObj)
    ].

-spec update_views(map(), ne_binary(), ne_binaries(), kz_proplist(), kz_proplist()) ->
                          api_binaries().
update_views(Server, Db, Update, CurrentViews, NewViews) ->
    Views = lists:foldl(fun(Id, Acc) ->
                                update_views_fold(CurrentViews, NewViews, Id, Acc)
                        end, [], Update),
    {'ok', JObjs} = kzs_doc:save_docs(Server, Db, Views, []),
    [kz_json:get_value(<<"id">>, JObj)
     || JObj <- JObjs,
        <<"conflict">> =:= kz_json:get_value(<<"error">>, JObj)
    ].

-spec update_views_fold(kz_proplist(), kz_proplist(), ne_binary(), kz_json:objects()) ->
                               kz_json:objects().
update_views_fold(CurrentViews, NewViews, Id, Acc) ->
    NewView = props:get_value(Id, NewViews),
    CurrentView = props:get_value(Id, CurrentViews),
    Rev = kz_doc:revision(CurrentView),
    RawView = kz_doc:delete_revision(CurrentView),
    case NewView =:= RawView of
        'true' ->
            lager:debug("view ~s does not require update", [Id]),
            Acc;
        'false' ->
            lager:debug("staging update of view ~s with rev ~s", [Id, Rev]),
            [kz_doc:set_revision(NewView, Rev) | Acc]
    end.

-spec correct_view_errors(map(), ne_binary(), ne_binaries(), kz_proplist()) -> 'true'.
correct_view_errors(Server, Db, Errors, NewViews) ->
    Views = [props:get_value(Id, NewViews) || Id <- Errors],
    correct_view_errors(Server, Db, Views).

-spec correct_view_errors(map(), ne_binary(), kz_json:objects()) -> 'true'.
correct_view_errors(_, _, []) -> 'true';
correct_view_errors(Server, Db, [View|Views]) ->
    lager:debug("ensuring view ~s is saved to ~s", [kz_doc:id(View), Db]),
    _ = kzs_doc:ensure_saved(Server, Db, View, []),
    correct_view_errors(Server, Db, Views).

-spec delete_views(map(), ne_binary(), ne_binaries(), kz_proplist()) -> 'true'.
delete_views(Server, Db, Delete, CurrentViews) ->
    Views = [props:get_value(Id, CurrentViews) || Id <- Delete],
    delete_views(Server, Db, Views).

-spec delete_views(map(), ne_binary(), kz_json:objects()) -> 'true'.
delete_views(_, _, []) -> 'true';
delete_views(Server, Db, [View|Views]) ->
    lager:debug("deleting view ~s from ~s", [kz_doc:id(View), Db]),
    _ = kzs_doc:del_doc(Server, Db, View, []),
    delete_views(Server, Db, Views).
