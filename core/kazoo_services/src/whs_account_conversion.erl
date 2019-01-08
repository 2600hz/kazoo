%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(whs_account_conversion).

-export([promote/1
        ,force_promote/1
        ]).
-export([demote/1
        ,force_demote/1
        ]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).
-export([has_reseller_descendants/1]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%------------------------------------------------------------------------------
-spec promote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
promote(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kapps_util:is_master_account(AccountId) of
        'true' -> {'error', 'master_account'};
        'false' ->
            case has_reseller_descendants(AccountId) of
                'true' -> {'error', 'reseller_descendants'};
                'false' -> do_promote(AccountId)
            end
    end.

-spec force_promote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
force_promote(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    do_promote(AccountId).

-spec do_promote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
do_promote(AccountId) ->
    _ = kz_util:account_update(AccountId, fun kzd_accounts:promote/1),
    _ = maybe_update_services(AccountId, ?SERVICES_PVT_IS_RESELLER, 'true'),
    io:format("promoting account ~s to reseller status, updating sub accounts~n", [AccountId]),
    cascade_reseller_id(AccountId, AccountId).

%%------------------------------------------------------------------------------
%% @doc Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%------------------------------------------------------------------------------
-spec demote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
demote(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kapps_util:is_master_account(AccountId) of
        'true' -> {'error', 'master_account'};
        'false' ->
            case has_reseller_descendants(AccountId) of
                'true' -> {'error', 'reseller_descendants'};
                'false' -> do_demote(AccountId)
            end
    end.

-spec force_demote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
force_demote(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    do_demote(AccountId).

-spec do_demote(kz_term:ne_binary()) -> {'error', _} | 'ok'.
do_demote(AccountId) ->
    _ = kz_util:account_update(AccountId, fun kzd_accounts:demote/1),
    _ = maybe_update_services(AccountId, ?SERVICES_PVT_IS_RESELLER, 'false'),
    ResellerId = kz_services:find_reseller_id(AccountId),
    io:format("demoting reseller status for account ~s, and now belongs to reseller ~s~n", [AccountId, ResellerId]),
    cascade_reseller_id(ResellerId, AccountId).

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%------------------------------------------------------------------------------
-spec cascade_reseller_id(kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', _} | 'ok'.
cascade_reseller_id(Reseller, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ResellerId = kz_util:format_account_id(Reseller, 'raw'),
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'error', _R}=Error ->
            lager:debug("unable to determin descendants of ~s: ~p", [AccountId, _R]),
            Error;
        {'ok', JObjs} ->
            _ = [set_reseller_id(ResellerId, SubAccountId)
                 || JObj <- JObjs
                        ,(SubAccountId = kz_doc:id(JObj)) =/= AccountId
                ],
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Set teh reseller_id to the provided value on the provided account
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', _} | 'ok'.
set_reseller_id(Reseller, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ResellerId = kz_util:format_account_id(Reseller, 'raw'),
    io:format("setting account ~s reseller id to ~s~n", [AccountId, ResellerId]),
    _ = kz_util:account_update(AccountId, fun(JObj) -> kzd_accounts:set_reseller_id(JObj, ResellerId) end),
    maybe_update_services(AccountId, ?SERVICES_PVT_RESELLER_ID, ResellerId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_services(kz_term:ne_binary(), kz_term:ne_binary(), any()) -> {'error', _} | 'ok'.
maybe_update_services(AccountId, Key, Value) ->
    case kz_services:fetch_services_doc(AccountId, true) of
        {'error', _R}=Error ->
            io:format("unable to open services doc ~s: ~p~n", [AccountId, _R]),
            Error;
        {'ok', JObj} ->
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_value(Key, Value, JObj)) of
                {'ok', _} -> 'ok';
                {'error', _R}=Error ->
                    io:format("unable to set ~s on services doc ~s: ~p~n", [Key, AccountId, _R]),
                    Error
            end
    end.

-spec has_reseller_descendants(kz_term:ne_binary()) -> boolean().
has_reseller_descendants(AccountId) ->
    %% its very important that this check not operate against stale data!
    _ = kz_datamgr:flush_cache_docs(?KZ_SERVICES_DB),
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    {'ok', JObjs} = kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions),
    lists:any(fun is_reseller/1, JObjs).

-spec is_reseller(kz_json:object()) -> boolean().
is_reseller(JObj) ->
    kz_services:is_reseller(kz_doc:id(JObj)).
