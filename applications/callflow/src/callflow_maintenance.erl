%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(callflow_maintenance).

-export([lookup_endpoint/1
         ,lookup_endpoint/2
        ]).
-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).
-export([migrate_menus/0, migrate_menus/1]).
-export([migrate_recorded_name/0, migrate_recorded_name/1]).
-export([show_calls/0]).
-export([flush/0]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_endpoint(ne_binary()) -> 'no_return'.
lookup_endpoint(URI) ->
    case binary:split(URI, <<"@">>) of
        [Username, Realm] -> lookup_endpoint(Username, Realm);
        _Else -> io:format("invalid SIP URI~n", [])
    end.

-spec lookup_endpoint(ne_binary(), ne_binary()) -> 'no_return'.
lookup_endpoint(Username, Realm) ->
    _ = case whapps_util:get_account_by_realm(Realm) of
            {'ok', AccountDb} ->
                case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
                    {'ok', EndpointId} ->
                        Endpoint = cf_endpoint:get(EndpointId, AccountDb),
                        io:format("~p~n", [Endpoint]);
                    _Else -> io:format("unable to find username ~s in ~s~n"
                                       ,[Username, AccountDb])
                end;
            _Else -> io:format("unable to find account with realm ~s~n", [Realm])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?CALLFLOW_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec show_calls() -> 'ok'.
show_calls() ->
    do_show_calls(cf_exe_sup:workers(), 0).

do_show_calls([], Total) ->
    io:format("Total: ~p~n", [Total]);
do_show_calls([Srv|Srvs], Total) ->
    case catch(cf_exe:get_call(Srv)) of
        {'ok', Call} ->
            io:format("CF_EXE(~p): ~p~n", [Srv, whapps_call:to_proplist(Call)]);
        _ -> 'ok'
    end,
    do_show_calls(Srvs, Total + 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh() -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun(AccountDb) ->
                          refresh(AccountDb)
                  end, whapps_util:get_all_accounts()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'started'.
-spec refresh(binary() | string()) -> 'ok'.

refresh() ->
    spawn(fun() ->
                  lists:foreach(fun(AccountDb) ->
                                        refresh(AccountDb)
                                end, whapps_util:get_all_accounts())
          end),
    'started'.

refresh(<<Account/binary>>) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Views = whapps_util:get_views_json('callflow', "views"),
    whapps_util:update_views(AccountDb, Views);
refresh(Account) ->
    refresh(wh_util:to_binary(Account)).

-spec migrate_recorded_name() -> any().
-spec migrate_recorded_name(ne_binary()) -> any().
migrate_recorded_name() ->
    [catch migrate_recorded_name(AccountDb) || AccountDb <- whapps_util:get_all_accounts('encoded')].
migrate_recorded_name(Db) ->
    lager:info("migrating all name recordings from vmboxes w/ owner_id in ~s", [Db]),

    case couch_mgr:get_results(Db, <<"vmboxes/crossbar_listing">>, ['include_docs']) of
        {'ok', []} -> lager:info("no vmboxes in ~s", [Db]);
        {'error', _E} -> lager:info("unable to get vm box list: ~p", [_E]);
        {'ok', VMBoxes} ->
            [do_recorded_name_migration(Db, wh_json:get_value(<<"doc">>, VMBox))
             || VMBox <- VMBoxes
            ]
    end.

-spec do_recorded_name_migration(ne_binary(), wh_json:object()) -> any().
-spec do_recorded_name_migration(ne_binary(), wh_json:object(), api_binary()) -> any().
do_recorded_name_migration(Db, VMBox) ->
    VMBoxId = wh_json:get_value(<<"_id">>, VMBox),
    case wh_json:get_value(?RECORDED_NAME_KEY, VMBox) of
        'undefined' -> lager:info("vm box ~s has no recorded name to migrate", [VMBoxId]);
        MediaId ->
            lager:info("vm box ~s has recorded name in doc ~s", [VMBoxId, MediaId]),
            do_recorded_name_migration(Db, MediaId, wh_json:get_value(<<"owner_id">>, VMBox)),
            {'ok', _} = couch_mgr:save_doc(Db, wh_json:delete_key(?RECORDED_NAME_KEY, VMBox))
    end.

do_recorded_name_migration(_Db, _MediaId, 'undefined') ->
    lager:info("no owner id on vm box");
do_recorded_name_migration(Db, MediaId, OwnerId) ->
    {'ok', Owner} = couch_mgr:open_doc(Db, OwnerId),
    case wh_json:get_value(?RECORDED_NAME_KEY, Owner) of
        'undefined' ->
            lager:info("no recorded name on owner, setting to ~s", [MediaId]),
            {'ok', _} = couch_mgr:save_doc(Db, wh_json:set_value(?RECORDED_NAME_KEY, MediaId, Owner)),
            lager:info("updated owner doc with recorded name doc id ~s", [MediaId]);
        MediaId ->
            lager:info("owner already has recorded name at ~s", [MediaId]);
        OwnerMediaId ->
            lager:info("owner has recorded name at ~s(not ~s), using owners", [OwnerMediaId, MediaId]),
            couch_mgr:del_doc(Db, MediaId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will migrate all the menus mailbox documents to
%% the latest version.
%% @end
%%--------------------------------------------------------------------
-spec migrate_menus() -> ['done' | 'error',...].
-spec migrate_menus(ne_binary()) -> 'done' | 'error'.
migrate_menus() ->
    [migrate_menus(Account) || Account <- whapps_util:get_all_accounts('raw')].
migrate_menus(Account) ->
    Db = wh_util:format_account_id(Account, 'encoded'),
    lager:info("migrating all menus in ~s", [Db]),
    case couch_mgr:get_results(Db, <<"menus/crossbar_listing">>, ['include_docs']) of
        {'ok', []} ->
            lager:info("db ~s has no menus", [Db]),
            'done';
        {'ok', Menus} ->
            [do_menu_migration(Menu, Db) || Menu <- Menus];
        {'error', _E} ->
            lager:info("unable to get a list of menus: ~p", [_E]),
            'error'
    end.

do_menu_migration(Menu, Db) ->
    Doc = wh_json:get_value(<<"doc">>, Menu),
    MenuId = wh_json:get_value(<<"_id">>, Doc),
    VSN = wh_json:get_integer_value(<<"pvt_vsn">>, Doc, 1),
    case couch_mgr:fetch_attachment(Db, MenuId, <<"prompt.mp3">>) of
        {'ok', _} when VSN =/= 1 ->
            lager:info("menu ~s in ~s already migrated", [MenuId, Db]);
        {'ok', Bin} ->
            Name = <<(wh_json:get_value(<<"name">>, Doc, <<>>))/binary, " menu greeting">>,
            MediaId = create_media_doc(Name, <<"menu">>, MenuId, Db),
            AName = <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".mp3">>,
            {'ok', _} = couch_mgr:put_attachment(Db, MediaId, AName, Bin),
            'ok' = update_doc([<<"media">>, <<"greeting">>], MediaId, MenuId, Db),
            'ok' = update_doc([<<"pvt_vsn">>], <<"2">>, MenuId, Db),
            lager:info("migrated menu ~s in ~s prompt to /~s/~s/~s", [MenuId, Db, Db, MediaId, AName]);
        _ ->
            lager:info("menu ~s in ~s has no greeting or prompt", [MenuId, Db])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_media_doc(binary(), binary(), binary(), binary()) -> binary().
create_media_doc(Name, SourceType, SourceId, Db) ->
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<SourceType/binary, " recorded/prompt media">>}
             ,{<<"source_type">>, SourceType}
             ,{<<"source_id">>, SourceId}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, 'true'}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{'type', <<"media">>}]),
    {'ok', JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc(list() | binary(), wh_json:json_term(), binary(), binary()) ->
                        'ok' |
                        {'error', atom()}.
update_doc(Key, Value, Id, Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {'ok', JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {'error', 'conflict'} -> update_doc(Key, Value, Id, Db);
                {'ok', _} -> 'ok';
                {'error', _}=E -> lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
            end;
        {'error', _}=E ->
            lager:info("unable to update ~s in ~s, ~p", [Id, Db, E])
    end.
