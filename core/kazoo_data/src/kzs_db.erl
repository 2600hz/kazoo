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
        ,db_delete/3
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
    case do_db_create(Map, DbName, Options) of
        exists -> true;
        true -> db_create_others(Map, DbName, Options)
                    andalso kzs_publish:publish_db(DbName, 'created');
        false -> false
    end.

-spec db_create_others(map(), ne_binary(), db_create_options()) -> boolean().
db_create_others(#{}=Map, DbName, Options) ->
    EnsureOthers = props:get_value('ensure_other_dbs', Options, 'false'),
    case do_db_create_others(Map, DbName, Options) of
        'false' when EnsureOthers -> 'false';
        _ -> 'true'
    end.

-spec do_db_create_others(map(), ne_binary(), db_create_options()) -> boolean().
do_db_create_others(Map, DbName, Options) ->
    Others = maps:get('others', Map, []),
    lists:all(fun({_Tag, M1}) ->
                      do_db_create(#{server => M1}, DbName, Options) =/= 'false'
              end, Others).

-spec do_db_create(map(), ne_binary(), db_create_options()) -> boolean() | 'exists'.
do_db_create(#{server := {App, Conn}}, DbName, Options) ->
    case App:db_exists(Conn, DbName) of
        'false' -> App:db_create(Conn, DbName, Options);
        'true' -> 'exists'
    end.

-spec db_delete(map(), ne_binary(), db_delete_options()) -> boolean().
db_delete(#{}=Map, DbName, Options) ->
    do_db_delete(Map, DbName)
        andalso db_delete_others(Map, DbName, Options)
        andalso kzs_publish:publish_db(DbName, 'deleted').

-spec db_delete_others(map(), ne_binary(), db_delete_options()) -> boolean().
db_delete_others(#{}=Map, DbName, Options) ->
    EnsureOthers = props:get_value('ensure_other_dbs', Options, 'false'),
    case do_db_delete_others(Map, DbName) of
        'false' when EnsureOthers -> 'false';
        _ -> 'true'
    end.

-spec do_db_delete_others(map(), ne_binary()) -> boolean().
do_db_delete_others(Map, DbName) ->
    Others = maps:get('others', Map, []),
    lists:all(fun({_Tag, M1}) ->
                      do_db_delete(#{server => M1}, DbName)
              end, Others).

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
db_exists(#{server := {App, Conn}}=Server, DbName) ->
    case kz_cache:fetch_local(?KAZOO_DATA_PLAN_CACHE, {'database', {App, Conn}, DbName}) of
        {'ok', Exists} -> Exists;
        _ ->
            case App:db_exists(Conn, DbName) of
                {'error', 'resource_not_available'} -> 'true';
                Exists -> maybe_cache_db_exists(Exists, Server, DbName)
            end
    end.

-spec maybe_cache_db_exists(boolean(), map(), ne_binary()) -> boolean().
maybe_cache_db_exists('false', _, _) -> 'false';
maybe_cache_db_exists('true', #{server := {App, Conn}}, DbName) ->
    Props = [{'origin', {'db', DbName}}],
    kz_cache:store_local(?KAZOO_DATA_PLAN_CACHE, {'database', {App, Conn}, DbName}, 'true', Props),
    'true'.

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

-spec db_view_update(map(), ne_binary(), views_listing(), boolean()) -> boolean().
db_view_update(#{}=Map, DbName, Views, Remove) ->
    Others = maps:get('others', Map, []),
    do_db_view_update(Map, DbName, Views, Remove)
        andalso lists:all(fun({_Tag, M1}) ->
                                  do_db_view_update(#{server => M1}, DbName, Views, Remove)
                          end
                         ,Others
                         ).

-spec do_db_view_update(map(), ne_binary(), views_listing(), boolean()) -> boolean().
do_db_view_update(#{server := {App, Conn}}=Server, Db, NewViews, Remove) ->
    case kzs_view:all_design_docs(Server, Db, ['include_docs']) of
        {'ok', JObjs} ->
            CurrentViews = [{kz_doc:id(JObj), kz_json:get_value(<<"doc">>, JObj)}
                            || JObj <- JObjs
                           ],
            add_update_remove_views(Server, Db, CurrentViews, NewViews, Remove);
        {'error', _R} ->
            case App:db_exists(Conn, Db) of
                'true' -> add_update_remove_views(Server, Db, [], NewViews, Remove);
                'false' ->
                    lager:error("error fetching current views for db ~s", [Db]),
                    'true'
            end
    end.

-spec add_update_remove_views(map(), ne_binary(), views_listing(), views_listing(), boolean()) -> 'true'.
add_update_remove_views(Server, Db, CurrentViews, NewViews, ShouldRemoveDangling) ->
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
    _ = ShouldRemoveDangling
        andalso delete_views(Server, Db, Delete, CurrentViews),
    true.

-spec add_views(map(), ne_binary(), ne_binaries(), views_listing()) -> api_ne_binaries().
add_views(Server, Db, Add, NewViews) ->
    Views = [props:get_value(Id, NewViews) || Id <- Add],
    {'ok', JObjs} = kzs_doc:save_docs(Server, Db, Views, []),
    [kz_doc:id(JObj)
     || JObj <- JObjs,
        <<"conflict">> =:= kz_json:get_value(<<"error">>, JObj)
    ].

-spec update_views(map(), ne_binary(), ne_binaries(), views_listing(), views_listing()) -> api_ne_binaries().
update_views(Server, Db, Update, CurrentViews, NewViews) ->
    Views = lists:flatten(
              [kz_doc:set_revision(NewView, kz_doc:revision(CurrentView))
               || Id <- Update,
                  CurrentView <- [props:get_value(Id, CurrentViews)],
                  NewView <- [props:get_value(Id, NewViews)],
                  should_update(Id, NewView, CurrentView)
              ]),
    {'ok', JObjs} = kzs_doc:save_docs(Server, Db, Views, []),
    [kz_doc:id(JObj)
     || JObj <- JObjs,
        <<"conflict">> =:= kz_json:get_value(<<"error">>, JObj)
    ].

-spec should_update(ne_binary(), kz_json:object(), kz_json:object()) -> boolean().
should_update(_Id, _, undefined) ->
    lager:warning("view ~p does not exist to update", [_Id]),
    false;
should_update(_Id, NewView, OldView) ->
    case kz_json:are_equal(kz_doc:delete_revision(NewView), kz_doc:delete_revision(OldView)) of
        true ->
            lager:debug("view ~s does not require update", [_Id]),
            false;
        false ->
            lager:debug("staging update of view ~s with rev ~s", [_Id, kz_doc:revision(OldView)]),
            true
    end.

-spec correct_view_errors(map(), ne_binary(), ne_binaries(), views_listing()) -> 'true'.
correct_view_errors(Server, Db, Errors, NewViews) ->
    Views = [props:get_value(Id, NewViews) || Id <- Errors],
    correct_view_errors(Server, Db, Views).

-spec correct_view_errors(map(), ne_binary(), kz_json:objects()) -> 'true'.
correct_view_errors(_, _, []) -> 'true';
correct_view_errors(Server, Db, [View|Views]) ->
    lager:debug("ensuring view ~s is saved to ~s", [kz_doc:id(View), Db]),
    _ = kzs_doc:ensure_saved(Server, Db, View, []),
    correct_view_errors(Server, Db, Views).

-spec delete_views(map(), ne_binary(), ne_binaries(), views_listing()) -> 'true'.
delete_views(Server, Db, Delete, CurrentViews) ->
    Views = [props:get_value(Id, CurrentViews) || Id <- Delete],
    delete_views(Server, Db, Views).

-spec delete_views(map(), ne_binary(), kz_json:objects()) -> 'true'.
delete_views(_, _, []) -> 'true';
delete_views(Server, Db, [View|Views]) ->
    lager:debug("deleting view ~s from ~s", [kz_doc:id(View), Db]),
    _ = kzs_doc:del_doc(Server, Db, View, []),
    delete_views(Server, Db, Views).
