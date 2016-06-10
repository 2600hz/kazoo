%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_services_modb).

-export([start_link/0
         ,modb/1
        ]).

-include("kazoo_services.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    _Pid = kz_util:spawn(fun kazoo_modb:add_routine/1, [?MODULE]),
    io:format("started add_rountine in ~p~n", [_Pid]),
    'ignore'.

-spec modb(ne_binary()) -> 'ok'.
modb(?MATCH_MODB_SUFFIX_ENCODED(_AccountId, _Year, _Month) = AccountMODb) ->
    modb(kz_util:format_account_modb(AccountMODb, 'raw'));
modb(?MATCH_MODB_SUFFIX_RAW(AccountId, _Year, _Month) = AccountMODb) ->
    ServicesJObj = kz_services:to_json(kz_services:fetch(AccountId)),
    save_services_to_modb(AccountMODb, ServicesJObj, <<"services_bom">>),
    maybe_save_to_previous_modb(AccountMODb, ServicesJObj).

-spec save_services_to_modb(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
save_services_to_modb(AccountMODb, ServicesJObj, Id) ->
    MODbDoc = update_pvts(AccountMODb, ServicesJObj, Id),
    kazoo_modb:save_doc(AccountMODb, MODbDoc),
    'ok'.

-spec maybe_save_to_previous_modb(ne_binary(), kz_json:object()) -> 'ok'.
maybe_save_to_previous_modb(NewMODb, ServicesJObj) ->
    PrevMODb = kazoo_modb_util:prev_year_month_mod(NewMODb),
    AccountDb = kz_util:format_account_modb(PrevMODb, 'encoded'),
    case kz_datamgr:db_exists(AccountDb) of
        'true' -> save_services_to_modb(PrevMODb, ServicesJObj, <<"services_eom">>);
        'false' -> 'ok'
    end.

-spec update_pvts(ne_binary(), kz_json:object(), ne_binary()) -> kz_json:object().
update_pvts(?MATCH_MODB_SUFFIX_RAW(AccountId, _Year, _Month) = AccountMODb, ServicesJObj, Id) ->
    kz_doc:update_pvt_parameters(kz_json:delete_key(<<"_rev">>, kz_doc:set_id(ServicesJObj, Id))
                                 ,AccountMODb
                                 ,[{'account_db', AccountMODb}
                                   ,{'account_id', AccountId}
                                  ]
                                ).
