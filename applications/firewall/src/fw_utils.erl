%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(fw_utils).

-export([get_global_action/1]).
-export([get_account_action/2]).

-export([update_account_cache/1]).

-define(ACCOUNT_CACHE_KEY(AccountId, CallerId), {AccountId, CallerId}).
-define(DEFAULT_ACTION, <<"hangup">>).

-include("firewall.hrl").



get_global_action(_CallerId) ->
    {'error', 'not_handle'}.

get_account_action(CallerId, AccountId) ->
    case wh_cache:fetch_local(?FIREWALL_CACHE, ?ACCOUNT_CACHE_KEY(AccountId, CallerId)) of
        {'ok', _}=R ->
            lager:debug("got action from cache"),
            R;
        {'error', 'not_found'} -> get_action_from_account(AccountId, CallerId)
    end.

get_action_from_account(AccountId, CallerId) ->
    lager:debug("getting action from account doc ~s for ~s", [AccountId, CallerId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _E}=R ->
            lager:error("failed to load ~s in ~s : ~p", [AccountId, AccountDb, _E]),
            R;
        {'ok', JObj} ->
            case wh_json:get_value([<<"firewall">>, <<"caller_id_numbers">>], JObj) of
                'undefined' -> {'error', 'undefined'};
                CallerIds ->
                    case lists:member(CallerId, CallerIds) of
                        'false' -> {'error', 'undefined'};
                        'true' ->
                            spawn(?MODULE, 'update_account_cache', [AccountId]),
                            {'ok', wh_json:get_value([<<"firewall">>, <<"action">>], JObj, ?DEFAULT_ACTION)}
                    end
            end
    end.

update_account_cache(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _E} ->
            lager:error("failed to load ~s in ~s : ~p", [AccountId, AccountDb, _E]);
        {'ok', JObj} ->
            case wh_json:get_value([<<"firewall">>, <<"caller_id_numbers">>], JObj) of
                'undefined' ->
                    lager:error("caller_id_numbers is not set for account ~s", [AccountId]);
                CallerIds ->
                    lists:foreach(
                        fun(CallerId) ->
                            Action = wh_json:get_value([<<"firewall">>, <<"action">>], JObj, ?DEFAULT_ACTION),
                            Key = ?ACCOUNT_CACHE_KEY(AccountId, CallerId),
                            wh_cache:store_local(?FIREWALL_CACHE, Key, Action),
                            lager:debug("stored ~p with action ~p", [Key, Action])
                        end
                        ,CallerIds
                    )
            end
    end.
