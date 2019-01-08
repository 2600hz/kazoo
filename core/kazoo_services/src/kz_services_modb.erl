%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_modb).

-export([start_link/0
        ,modb/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_transactions/include/kazoo_transactions.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    _Pid = kz_util:spawn(fun kazoo_modb:add_routine/1, [?MODULE]),
    io:format("started services modb add_rountine in ~p~n", [_Pid]),
    'ignore'.

-spec modb(kz_term:ne_binary()) -> 'ok'.
modb(?MATCH_MODB_SUFFIX_ENCODED(_AccountId, _Year, _Month) = AccountMODb) ->
    modb(kz_util:format_account_modb(AccountMODb, 'raw'));
modb(?MATCH_MODB_SUFFIX_RAW(AccountId, _Year, _Month) = AccountMODb) ->
    ServicesJObj = kz_services:to_json(kz_services:fetch(AccountId)),
    save_services_to_modb(AccountMODb, ServicesJObj, ?SERVICES_BOM),
    maybe_save_to_previous_modb(AccountMODb, ServicesJObj).

-spec save_services_to_modb(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
save_services_to_modb(AccountMODb, ServicesJObj, Id) ->
    MODbDoc = update_pvts(AccountMODb, ServicesJObj, Id),
    kazoo_modb:save_doc(AccountMODb, MODbDoc),
    'ok'.

-spec maybe_save_to_previous_modb(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_save_to_previous_modb(NewMODb, ServicesJObj) ->
    PrevMODb = kazoo_modb_util:prev_year_month_mod(NewMODb),
    AccountDb = kz_util:format_account_modb(PrevMODb, 'encoded'),
    case kz_datamgr:db_exists(AccountDb) of
        'true' -> save_services_to_modb(PrevMODb, ServicesJObj, ?SERVICES_EOM);
        'false' -> 'ok'
    end.

-spec update_pvts(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
update_pvts(?MATCH_MODB_SUFFIX_RAW(AccountId, _Year, _Month) = AccountMODb, ServicesJObj, Id) ->
    kz_doc:update_pvt_parameters(kz_json:delete_key(<<"_rev">>, kz_doc:set_id(ServicesJObj, Id))
                                ,AccountMODb
                                ,[{'account_db', AccountMODb}
                                 ,{'account_id', AccountId}
                                 ]
                                ).
