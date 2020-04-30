%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_converter_migrate_maintenance).

-export([migrate/0
        ,migrate_account/1
        ,migrate_invite_format/1, migrate_invite_format/3
        ,migrate_devices/1, migrate_devices/3
        ,migrate_global_resources/0, migrate_global_resources/2
        ,migrate_local_resources/1, migrate_local_resources/3
        ,migrate_trunkstore/1, migrate_trunkstore/3
        ]).

-define(LEGACY_STRIP_PLUS, <<"e164_without_plus">>).
-define(ONE_NPAN, <<"1npan">>).
-define(NPAN, <<"npan">>).
-define(STRIP_PLUS, <<"strip_plus">>).
-define(DEFAULT_FROM, <<"npan">>).
-define(DEFAULT_TO, <<"strip_plus">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    AccountIds = kz_term:shuffle_list(kapps_util:get_all_accounts('raw')),
    Total = length(AccountIds),
    io:format("Start migrating from ~b databases~n~n", [Total]),
    lists:foldl(fun(A, C) -> migrate_account_fold(A, C, Total) end, 1, AccountIds),
    migrate_global_resources(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_account_fold(kz_term:ne_binary(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
migrate_account_fold(AccountId, Current, Total) ->
    io:format("(~p/~p) migrating items within account '~s'~n", [Current, Total, AccountId]),
    _ = migrate_account(AccountId),
    io:format("[~s] finished migrating items~n~n", [AccountId]),
    Current + 1.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_account(kz_term:ne_binary()) -> 'ok'.
migrate_account(Account) ->
    lists:foreach(fun(Fun) -> Fun(Account) end
                 ,[fun migrate_invite_format/1]
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_invite_format(kz_term:ne_binary()) -> 'ok'.
migrate_invite_format(Account) ->
    migrate_invite_format(Account, ?DEFAULT_FROM, ?DEFAULT_TO).

-spec migrate_invite_format(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_invite_format(Account, From, To) ->
    io:format("migrating INVITE formatter from ~s to ~s account ~s~n", [From, To, Account]),
    lists:foreach(fun(Fun) -> Fun(Account, From, To) end
                 ,[fun migrate_devices/3
                  ,fun migrate_local_resources/3
                  ,fun migrate_trunkstore/3
                  ]
                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_devices(kz_term:ne_binary()) -> 'ok'.
migrate_devices(Account) ->
    migrate_devices(Account, ?DEFAULT_FROM, ?DEFAULT_TO).

-spec migrate_devices(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_devices(Account, From, To) ->
    AccountDb = kzs_util:format_account_db(Account),
    Total = get_view_count(AccountDb, <<"devices/crossbar_listing">>),
    migrate_devices(AccountDb, From, To, Total).

-spec migrate_devices(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_devices(AccountDb, _From, _To, 0) ->
    io:format("[~s] no devices found~n", [kzs_util:format_account_id(AccountDb)]);
migrate_devices(AccountDb, From, To, Total) ->
    Limit = kz_datamgr:max_bulk_read(),
    io:format("[~s] start migrating total ~b devices with batch size ~b~n", [kzs_util:format_account_id(AccountDb), Total, Limit]),
    Stats = #{total => Total
             ,processed => 0
             ,updated => 0
             ,result => #{}
             ,skip => 0
             },
    maybe_migrate_devices(AccountDb, From, To, Stats, [{'limit', Limit}]).

-spec maybe_migrate_devices(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map(), kz_term:proplist()) -> 'ok'.
maybe_migrate_devices(AccountDb, From, To, #{total := Total, processed := _Processed, updated := Updated, skip := _Skip}=Stats, ViewOptions) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case kz_datamgr:get_results(AccountDb, <<"devices/crossbar_listing">>, ViewOptions) of
        {'ok', []} ->
            io:format("[~s] device migration finished, (~b/~b) devices has been updated~n"
                     ,[kzs_util:format_account_id(AccountId), Updated, Total]
                     );
        {'ok', ViewResults} ->
            Length = length(ViewResults),
            io:format("[~s] processing ~b devices~n", [AccountId, Length]),
            UpdateFun = fun(JObj, Map) ->
                                maybe_update_device(AccountDb, kz_json:get_ne_binary_value(<<"key">>, JObj), From, To, Map)
                        end,
            ProcessMap = lists:foldl(UpdateFun, Stats, ViewResults),
            io:format("[~s] device migration finished, (~b/~b) devices has been updated~n"
                     ,[AccountId, maps:get('updated', ProcessMap), Total]
                     );
        {'error', _R} ->
            io:format("[~s] failed to fetch devices: ~p~n~n", [AccountId, _R])
    end.

maybe_update_device(AccountDb, DeviceId, From, To, #{total := _Total, processed := Processed, updated := Updated, skip := Skip}=Map) ->
    {'ok', Doc} = kzd_devices:fetch(AccountDb, DeviceId),
    case kzd_devices:sip_invite_format(Doc) of
        From ->
            NewDevice = kzd_devices:set_sip_invite_format(Doc, To),
            maybe_save_doc(NewDevice, Map);
        _ ->
            Map#{processed => Processed+1, updated => Updated, skip => Skip+1}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_local_resources(kz_term:ne_binary()) -> 'ok'.
migrate_local_resources(Account) ->
    migrate_local_resources(Account, ?DEFAULT_FROM, ?DEFAULT_TO).

-spec migrate_local_resources(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_local_resources(Account, From, To) ->
    AccountDb = kzs_util:format_account_db(Account),
    Total = get_view_count(AccountDb, <<"resources/crossbar_listing">>),
    migrate_resources(AccountDb, From, To, Total).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_global_resources() -> 'ok'.
migrate_global_resources() ->
    migrate_global_resources(?DEFAULT_FROM, ?DEFAULT_TO).

-spec migrate_global_resources(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_global_resources(From, To) ->
    Db = <<"offnet">>,
    Total = get_view_count(Db, <<"resources/crossbar_listing">>),
    migrate_resources(Db, From, To, Total).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_resources(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_resources(AccountId, _From, _To, 0) ->
    io:format("[~s] no resources found~n", [AccountId]);
migrate_resources(<<"offnet">>=AccountDb, From, To, Total) ->
    maybe_migrate_resources(AccountDb, From, To, Total);
migrate_resources(AccountId, From, To, Total) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    maybe_migrate_resources(AccountDb, From, To, Total).

-spec maybe_migrate_resources(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
maybe_migrate_resources(AccountDb, From, To, Total) ->
    Limit = kz_datamgr:max_bulk_read(),
    io:format("[~s] start migrating total ~b resources with batch size ~b~n", [kzs_util:format_account_id(AccountDb), Total, Limit]),
    Stats = #{total => Total
             ,processed => 0
             ,updated => 0
             ,result => #{}
             ,skip => 0
             },
    maybe_migrate_resources(AccountDb, From, To, Stats, [{'limit', Limit}]).

-spec maybe_migrate_resources(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map(), kz_term:proplist()) -> 'ok'.
maybe_migrate_resources(AccountDb, From, To, #{total := Total, processed := _Processed, updated := Updated, skip := _Skip}=Stats, ViewOptions) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case kz_datamgr:get_results(AccountDb, <<"resources/crossbar_listing">>, ViewOptions) of
        {'ok', []} ->
            io:format("[~s] resource migration finished, (~b/~b) resources has been updated~n"
                     ,[AccountId, Updated, Total]
                     );
        {'ok', ViewResults} ->
            Length = length(ViewResults),
            io:format("[~s] processing ~b resources~n", [AccountId, Length]),
            UpdateFun = fun(JObj, Map) ->
                                maybe_update_resource(AccountDb, kz_json:get_ne_binary_value(<<"key">>, JObj), From, To, Map)
                        end,
            ProcessMap = lists:foldl(UpdateFun, Stats, ViewResults),
            io:format("[~s] resource migration finished, (~b/~b) resources has been updated~n"
                     ,[AccountId, maps:get('updated', ProcessMap), Total]
                     );
        {'error', _R} ->
            io:format("[~s] failed to fetch resources: ~p~n~n", [AccountId, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_resource(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map()) -> map().
maybe_update_resource(AccountDb, ResourceId, From, To, #{total := _Total, processed := Processed, updated := _Updated, skip := Skip}=Map) ->
    {'ok', Doc} = kz_datamgr:open_doc(AccountDb, ResourceId),
    UpdateFun = fun(JObj, Acc) ->
                        maybe_update_gateways(JObj, From, To, Acc)
                end,
    Gateways = kzd_resources:gateways(Doc),
    NewGateways = lists:foldl(UpdateFun, [], Gateways),
    NewResource = kzd_resources:set_gateways(Doc, NewGateways),
    case kz_json:are_equal(Doc, NewResource) of
        'true' ->
            Map#{processed => Processed+1, skip => Skip+1};
        'false' ->
            maybe_save_doc(NewResource, Map)
    end.
-spec maybe_update_gateways(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_update_gateways(Gateway, From, To, Acc) ->
    case kz_json:get_ne_binary_value(<<"invite_format">>, Gateway, 'undefined') of
        From ->
            NewGateway = kz_json:set_value(<<"invite_format">>, To, Gateway),
            [NewGateway | Acc];
        _ -> [Gateway | Acc]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_trunkstore(kz_term:ne_binary()) -> 'ok'.
migrate_trunkstore(Account) ->
    migrate_trunkstore(Account, ?DEFAULT_FROM, ?DEFAULT_TO).

-spec migrate_trunkstore(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
migrate_trunkstore(Account, From, To) ->
    AccountDb = kzs_util:format_account_db(Account),
    Total = get_view_count(AccountDb, <<"trunkstore/crossbar_listing">>, []),
    migrate_trunkstore(AccountDb, From, To, Total),
    'ok'.

-spec migrate_trunkstore(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) -> 'ok'.
migrate_trunkstore(AccountDb, _From, _To, 0) ->
    io:format("[~s] no trunks found~n", [kzs_util:format_account_id(AccountDb)]);
migrate_trunkstore(AccountDb, From, To, Total) ->
    Limit = kz_datamgr:max_bulk_read(),
    io:format("[~s] start migrating total ~b trunks with batch size ~b~n", [kzs_util:format_account_id(AccountDb), Total, Limit]),
    Stats = #{total => Total
             ,processed => 0
             ,updated => 0
             ,result => #{}
             ,skip => 0
             },
    maybe_migrate_trunkstore(AccountDb, From, To, Stats, [{'limit', Limit}, {'reduce', 'false'}]).

-spec maybe_migrate_trunkstore(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map(), kz_term:proplist()) -> 'ok'.
maybe_migrate_trunkstore(AccountDb, From, To, #{total := Total, processed := _Processed, updated := Updated, skip := _Skip}=Stats, ViewOptions) ->
    AccountId =kzs_util:format_account_id(AccountDb),
    case kz_datamgr:get_results(AccountDb, <<"trunkstore/crossbar_listing">>, ViewOptions) of
        {'ok', []} ->
            io:format("[~s] trunk migration finished, (~b/~b) trunks has been updated~n"
                     ,[AccountId, Updated, Total]
                     );
        {'ok', ViewResults} ->
            Length = length(ViewResults),
            io:format("[~s] processing ~b trunks~n", [AccountId, Length]),
            UpdateFun = fun(JObj, Map) ->
                                maybe_update_trunk(AccountDb, kz_json:get_ne_binary_value(<<"id">>, JObj), From, To, Map)
                        end,
            ProcessMap = lists:foldl(UpdateFun, Stats, ViewResults),
            io:format("[~s] trunkstore migration finished, (~b/~b) trunks has been updated~n"
                     ,[AccountId, maps:get('updated', ProcessMap), Total]
                     );
        {'error', _R} ->
            io:format("[~s] failed to fetch trunks: ~p~n~n", [AccountId, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_trunk(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), map()) -> map().
maybe_update_trunk(AccountDb, TrunkId, From, To, #{total := _Total, processed := Processed, updated := _Updated, skip := Skip}=Map) ->
    {'ok', Doc} = kz_datamgr:open_doc(AccountDb, TrunkId),
    UpdateFun = fun(JObj, Acc) ->
                        maybe_update_servers(JObj, From, To, Acc)
                end,
    Servers = kzd_trunkstore:servers(Doc),
    NewServers = lists:foldl(UpdateFun, [], Servers),
    NewTrunkstore = kzd_trunkstore:set_servers(Doc, NewServers),
    case kz_json:are_equal(Doc, NewTrunkstore) of
        'true' ->
            Map#{processed => Processed+1, skip => Skip+1};
        'false' ->
            maybe_save_doc(NewTrunkstore, Map)
    end.

-spec maybe_update_servers(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_update_servers(Server, From, To, Acc) ->
    case kz_json:get_ne_binary_value([<<"options">>, <<"inbound_format">>], Server, 'undefined') of
        From ->
            NewServer = kz_json:set_value([<<"options">>, <<"inbound_format">>], To, Server),
            [NewServer | Acc];
        _ -> [Server | Acc]
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_view_count(kz_term:ne_binary(), kz_term:ne_binary()) -> non_neg_integer().
get_view_count(AccountDb, View) ->
    get_view_count(AccountDb, View, 2).

-spec get_view_count(kz_term:ne_binary(), kz_term:ne_binary(), integer()) -> non_neg_integer().
get_view_count(AccountDb, _View, Retry) when Retry < 0 ->
    io:format("[~s] failed to fetch view ~s count~n", [kzs_util:format_account_id(AccountDb), _View]),
    0;
get_view_count(AccountDb, View, Retry) ->
    case kz_datamgr:get_results_count(AccountDb, View, [{'reduce', 'false'}]) of
        {'ok', Total} -> Total;
        {'error', 'not_found'} ->
            _ = kapps_maintenance:refresh(AccountDb),
            get_view_count(AccountDb, View, Retry-1);
        {'error', _} ->
            get_view_count(AccountDb, View, Retry-1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_save_doc(kz_doc:doc(), map()) -> map().
maybe_save_doc(Doc, #{total := _Total, processed := Processed, updated := Updated, skip := Skip}=Map) ->
    case kz_datamgr:save_doc(kz_doc:account_db(Doc), Doc) of
        {'ok', _} ->
            Map#{processed => Processed+1, updated => Updated+1, skip => Skip};
        {'error', _Reason} ->
            io:format("[~s] failed to update ~p: ~p~n", [kz_doc:account_id(Doc), kz_doc:id(Doc), _Reason]),
            Map#{processed => Processed+1, updated => Updated, skip => Skip}
    end.
