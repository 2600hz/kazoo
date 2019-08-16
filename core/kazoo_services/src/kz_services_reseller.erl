%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_reseller).

-export([is_reseller/1]).
-export([get_id/1
        ,find_id/1
        ]).
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
%% @doc Returns true if the account id or kz_services:services belongs to a
%% reseller otherwise returns false.
%% @end
%%------------------------------------------------------------------------------
-spec is_reseller(kz_term:api_ne_binary() | kz_services:services()) -> boolean().
is_reseller('undefined') -> 'false';
is_reseller(Account=?NE_BINARY) ->
    AccountId = kz_util:format_account_id(Account),
    case kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, AccountId) of
        {'ok', JObj} -> kzd_services:is_reseller(JObj);
        {'error', _} -> 'false'
    end;
is_reseller(Services) ->
    kzd_services:is_reseller(
      kz_services:services_jobj(Services)
     ).

%%------------------------------------------------------------------------------
%% @doc Fetches the reseller_id property from the services document,
%% an 'undefined' account id returns the master account (if present)
%% @end
%%------------------------------------------------------------------------------
-spec get_id(kz_term:api_ne_binary() | kz_services:services()) -> kz_term:api_ne_binary().
get_id('undefined') ->
    case kapps_util:get_master_account_id() of
        {'error', _} -> 'undefined';
        {'ok', MasterAccountId} -> MasterAccountId
    end;
get_id(Account=?NE_BINARY) ->
    get_id(kz_services:fetch(Account));
get_id(Services) ->
    kzd_services:reseller_id(kz_services:services_jobj(Services)).

%%------------------------------------------------------------------------------
%% @doc Ignores the reseller_id property on the services document (if any)
%% and walks the account tree until it finds the first reseller
%% @end
%%------------------------------------------------------------------------------
-spec find_id(kz_term:ne_binaries() | kz_term:api_binary() | kz_services:services()) -> kz_term:ne_binary().
find_id('undefined') -> find_id([]);
find_id(Account=?NE_BINARY) ->
    case kzd_accounts:fetch(Account) of
        {'ok', AccountJObj} ->
            find_id(lists:reverse(kzd_accounts:tree(AccountJObj)));
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [Account, _R]),
            find_id([])
    end;
find_id([]) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    MasterAccountId;
find_id([Parent|Ancestors]) ->
    case is_reseller(Parent) of
        'false' -> find_id(Ancestors);
        'true' -> Parent
    end;
find_id(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    find_id(lists:reverse(kzd_services:tree(ServicesJObj))).

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
    Update = [{kzd_accounts:path_reseller(), 'true'}],
    {'ok', _A} = kzd_accounts:update(AccountId, Update),
    _ = maybe_update_services(AccountId, ?SERVICES_PVT_IS_RESELLER, 'true'),
    io:format("promoting account ~s to reseller status, updating sub accounts~n", [AccountId]),
    lager:debug("promoting account ~s to reseller status, updating sub accounts~n", [AccountId]),
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
    Update = [{kzd_accounts:path_reseller(), 'false'}],
    _ = kzd_accounts:update(AccountId, Update),
    _ = maybe_update_services(AccountId, ?SERVICES_PVT_IS_RESELLER, 'false'),
    ResellerId = find_id(AccountId),
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
            lager:debug("unable to determine descendants of ~s: ~p", [AccountId, _R]),
            Error;
        {'ok', JObjs} ->
            _ = [set_reseller_id(ResellerId, SubAccountId)
                 || JObj <- JObjs,
                    SubAccountId <- [kz_doc:id(JObj)],
                    SubAccountId =/= AccountId
                ],
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on the provided account
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(kz_term:ne_binary(), kz_term:ne_binary()) -> {'error', _} | 'ok'.
set_reseller_id(Reseller, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ResellerId = kz_util:format_account_id(Reseller, 'raw'),
    io:format("setting account ~s reseller id to ~s~n", [AccountId, ResellerId]),

    Update = [{kzd_accounts:path_reseller_id(), ResellerId}],

    _ = kzd_accounts:update(AccountId, Update),
    maybe_update_services(AccountId, ?SERVICES_PVT_RESELLER_ID, ResellerId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_services(kz_term:ne_binary(), kz_term:ne_binary(), any()) -> {'error', _} | 'ok'.
maybe_update_services(AccountId, Key, Value) ->
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId) of
        {'error', _R}=Error ->
            io:format("unable to open services doc ~s: ~p~n", [AccountId, _R]),
            lager:debug("unable to open services doc ~s: ~p", [AccountId, _R]),
            Error;
        {'ok', JObj} ->
            case kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_value(Key, Value, JObj)) of
                {'ok', _} -> lager:debug("updated services doc successfully");
                {'error', _R}=Error ->
                    io:format("unable to set ~s on services doc ~s: ~p~n", [Key, AccountId, _R]),
                    lager:debug("unable to set ~s on services doc ~s: ~p", [Key, AccountId, _R]),
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
    check_descendants(JObjs).

-spec check_descendants(kz_json:objects()) -> boolean().
check_descendants([]) -> 'false';
check_descendants([JObj|JObjs]) ->
    case is_reseller(kz_doc:id(JObj)) of
        'false' -> check_descendants(JObjs);
        'true' -> 'true'
    end.
