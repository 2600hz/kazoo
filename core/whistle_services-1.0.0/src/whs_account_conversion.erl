%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whs_account_conversion).

-export([make_reseller/1
         ,force_make_reseller/1
        ]).
-export([demote_reseller/1
         ,force_demote_reseller/1
        ]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).
-export([has_reseller_descendants/1]).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%--------------------------------------------------------------------
-spec make_reseller(ne_binary()) -> {'error', _} | 'ok'.
make_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case whapps_util:is_master_account(AccountId) of
        'true' -> {'error', 'master_account'};
        'false' ->
            case has_reseller_descendants(AccountId) of
                'true' -> {'error', 'reseller_descendants'};
                'false' -> do_make_reseller(AccountId)
            end
    end.

-spec force_make_reseller(ne_binary()) -> {'error', _} | 'ok'.
force_make_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    do_make_reseller(AccountId).

-spec do_make_reseller(ne_binary()) -> {'error', _} | 'ok'.
do_make_reseller(AccountId) ->
    _ = update_account_definition(AccountId, <<"pvt_reseller">>, 'true'),
    _ = maybe_update_services(AccountId, <<"pvt_reseller">>, 'true'),
    io:format("promoting account ~s to reseller status, updating sub accounts~n", [AccountId]),
    cascade_reseller_id(AccountId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%--------------------------------------------------------------------
-spec demote_reseller(ne_binary()) -> {'error', _} | 'ok'.
demote_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case whapps_util:is_master_account(AccountId) of
        'true' -> {'error', 'master_account'};
        'false' ->
            case has_reseller_descendants(AccountId) of
                'true' -> {'error', 'reseller_descendants'};
                'false' -> do_demote_reseller(AccountId)
            end
    end.

-spec force_demote_reseller(ne_binary()) -> {'error', _} | 'ok'.
force_demote_reseller(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    do_demote_reseller(AccountId).

-spec do_demote_reseller(ne_binary()) -> {'error', _} | 'ok'.
do_demote_reseller(AccountId) ->
    _ = update_account_definition(AccountId, <<"pvt_reseller">>, 'false'),
    _ = maybe_update_services(AccountId, <<"pvt_reseller">>, 'false'),
    ResellerId = wh_services:find_reseller_id(AccountId),
    io:format("demoting reseller status for account ~s, and now belongs to reseller ~s~n", [AccountId, ResellerId]),
    cascade_reseller_id(ResellerId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%--------------------------------------------------------------------
-spec cascade_reseller_id(ne_binary(), ne_binary()) -> {'error', _} | 'ok'.
cascade_reseller_id(Reseller, Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ResellerId = wh_util:format_account_id(Reseller, 'raw'),
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'error', _R}=Error ->
            lager:debug("unable to determin descendants of ~s: ~p", [AccountId, _R]),
            Error;
        {'ok', JObjs} ->
            _ = [set_reseller_id(ResellerId, SubAccountId)
                 || JObj <- JObjs
                        ,(SubAccountId = wh_doc:id(JObj)) =/= AccountId
                ],
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set teh reseller_id to the provided value on the provided account
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(ne_binary(), ne_binary()) -> {'error', _} | 'ok'.
set_reseller_id(Reseller, Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ResellerId = wh_util:format_account_id(Reseller, 'raw'),
    io:format("setting account ~s reseller id to ~s~n", [AccountId, ResellerId]),
    _ = update_account_definition(AccountId, <<"pvt_reseller_id">>, ResellerId),
    maybe_update_services(AccountId, <<"pvt_reseller_id">>, ResellerId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_services(ne_binary(), ne_binary(), any()) -> {'error', _} | 'ok'.
maybe_update_services(AccountId, Key, Value) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R}=Error ->
            io:format("unable to open services doc ~s: ~p~n", [AccountId, _R]),
            Error;
        {'ok', JObj} ->
            case couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(Key, Value, JObj)) of
                {'ok', _} -> 'ok';
                {'error', _R}=Error ->
                    io:format("unable to set ~s on services doc ~s: ~p~n", [Key, AccountId, _R]),
                    Error
            end
    end.

-spec update_account_definition(ne_binary(), ne_binary(), any()) -> {'error', _} | 'ok'.
update_account_definition(AccountId, Key, Value) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', _R}=Error ->
            io:format("unable to open account ~s defintion: ~p~n", [AccountId, _R]),
            Error;
        {'ok', JObj} ->
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(Key, Value, JObj)) of
                {'ok', NewJObj} ->
                    _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, NewJObj),
                    'ok';
                {'error', _R}=Error ->
                    io:format("unable to set pvt_reseller on account ~s defintion: ~p~n", [AccountId, _R]),
                    Error
            end
    end.

-spec has_reseller_descendants(ne_binary()) -> boolean().
has_reseller_descendants(AccountId) ->
    %% its very important that this check not operate against stale data!
    _ = couch_mgr:flush_cache_docs(),
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    {'ok', JObjs} = couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions),
    lists:any(fun(JObj) ->
                  wh_services:is_reseller(kz_account:id(JObj))
              end, JObjs).
