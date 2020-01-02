%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_modb).

-export([rollover/3]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

%%------------------------------------------------------------------------------
%% @doc Rolls an account over into the new MODb (indicated by Year/Month)
%% @end
%%------------------------------------------------------------------------------
-spec rollover(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
rollover(AccountId, Year, Month) ->
    AccountMODb = kzs_util:format_account_mod_id(AccountId, Year, Month),
    lager:debug("creating snapshot for account ~s services in month ~p-~p"
               ,[AccountId, Year, Month]
               ),
    FetchOptions = ['hydrate_account_quantities'
                   ,'hydrate_cascade_quantities'
                   ,'skip_cache'
                   ],
    Services = kz_services:fetch(AccountId, FetchOptions),
    ServicesJObj = kz_doc:public_fields(
                     kz_services:services_jobj(Services)
                    ),
    save_services_to_modb(AccountMODb, ServicesJObj, ?SERVICES_BOM),
    maybe_save_to_previous_modb(AccountMODb, ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_services_to_modb(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
save_services_to_modb(AccountMODb, ServicesJObj, Id) ->
    MODbDoc = update_pvts(AccountMODb, kz_doc:set_id(ServicesJObj, Id)),
    case kazoo_modb:save_doc(AccountMODb, MODbDoc) of
        {'ok', JObj} ->
            lager:debug("saved services snapshot as ~s in ~s"
                       ,[kz_doc:id(JObj), AccountMODb]
                       );
        {'error', 'conflict'} ->
            lager:info("conflict when saving services snapshot ~s in ~s", [Id, AccountMODb]);
        {'error', _R} ->
            lager:warning("failed to store services snapshot ~s in ~s: ~p"
                         ,[Id, AccountMODb, _R]
                         )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_save_to_previous_modb(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_save_to_previous_modb(NewMODb, ServicesJObj) ->
    PrevMODb = kazoo_modb_util:prev_year_month_mod(NewMODb),
    AccountDb = kzs_util:format_account_modb(PrevMODb, 'encoded'),
    case kz_datamgr:db_exists(AccountDb) of
        'true' -> save_services_to_modb(PrevMODb, ServicesJObj, ?SERVICES_EOM);
        'false' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_pvts(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
update_pvts(AccountMODb, ServicesJObj) ->
    WithoutRev = kz_doc:delete_revision(ServicesJObj),
    kz_doc:update_pvt_parameters(WithoutRev
                                ,AccountMODb
                                ,[{'account_db', AccountMODb}
                                 ,{'account_id', kzs_util:format_account_id(AccountMODb)}
                                 ]
                                ).
