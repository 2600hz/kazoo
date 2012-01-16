%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%%-------------------------------------------------------------------
-module(whistle_number_manager_maintenance).

-export([reconcile/0, reconcile/1]).

-include("../include/wh_number_manager.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%% These are temporary until the viewing of numbers in an account can
%% be standardized
-define(TS_DB, <<"ts">>).

%% TODO: This makes stepswitch dependent on callflow view... This is safe-ish
%% beacuse if you reconcile without the callflow view then they will never
%% run anyway (no callflow whapp connected to the db to execute). But it is
%% still nasty...
-define(CALLFLOW_VIEW, {<<"callflow">>, <<"listing_by_number">>}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Seach the accounts for number assignements and ensure the routes
%% exist
%% @end
%%--------------------------------------------------------------------
-spec reconcile/0 :: () -> 'done'.
-spec reconcile/1 :: (string() | ne_binary() | all) -> 'done'.
-spec reconcile/2 :: (string() | ne_binary() | all, undefined | boolean()) -> 'done'.

reconcile() ->
    reconcile(all).

reconcile(all) ->
    reconcile(all, undefined);
reconcile(AccountId) when not is_binary(AccountId) ->
    reconcile(wh_util:to_binary(AccountId));
reconcile(AccountId) ->
    case couch_mgr:lookup_doc_rev(?TS_DB, AccountId) of
        {ok, _} ->
            reconcile(AccountId, true);
        {error, _} ->
            reconcile(AccountId, false)
    end.

reconcile(all, undefined) ->
    reconcile_accounts(),
    ok = reconcile_trunkstore(),
    done;
reconcile(AccountId, TSAccount) when not is_binary(AccountId) ->
    reconcile(wh_util:to_binary(AccountId), TSAccount);
reconcile(AccountId, true) ->
    Numbers = get_trunkstore_account_numbers(AccountId),
    _ = reconcile_numbers(Numbers, wh_util:format_account_id(AccountId, raw)),
    done;
reconcile(AccountId, false) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Numbers = get_callflow_account_numbers(AccountDb),
    _ = reconcile_numbers(Numbers, wh_util:format_account_id(AccountId, raw)),
    done.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the accounts and try to reconcile the stepswitch routes
%% with the numbers assigned in the account
%% @end
%%--------------------------------------------------------------------
-spec reconcile_accounts/0 :: () -> ok.
reconcile_accounts() ->
    _ = [begin
             Numbers = get_callflow_account_numbers(AccountId),
             reconcile_numbers(Numbers, AccountId)
         end
         || AccountId <- whapps_util:get_all_accounts(raw)],
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given an account create a json object of all numbers that look to
%% external (TODO: currently just uses US rules).
%% @end
%%--------------------------------------------------------------------
-spec get_callflow_account_numbers/1 :: (ne_binary()) -> json_object().
get_callflow_account_numbers(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_all_results(AccountDb, ?CALLFLOW_VIEW) of
        {ok, Numbers} ->
            [wh_json:get_value(<<"key">>, Number) || Number <- Numbers];
        {error, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reconciles number assignments in trunkstore with stepswitch
%% route documents (tmp solution until trunkstore follows the
%% account db structure)
%% @end
%%--------------------------------------------------------------------
-spec reconcile_trunkstore/0 :: () -> ok | {error, atom()}.
reconcile_trunkstore() ->
    case couch_mgr:all_docs(?TS_DB) of
        {ok, JObj} ->
            _ = [begin
                     AccountId = wh_json:get_value(<<"id">>, Account),
                     Numbers = get_trunkstore_account_numbers(AccountId),
                     reconcile_numbers(Numbers, AccountId)
                 end
                 || Account <- JObj],
            ok;
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a document info json object from trunkstore returns true if
%% it is a 'info_' document (IE: trunkstore account)
%% @end
%%--------------------------------------------------------------------
-spec is_trunkstore_account/1 :: (json_object()) -> boolean().
is_trunkstore_account(JObj) ->
    wh_json:get_value(<<"type">>, JObj) =:= <<"sys_info">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a trunkstore account id this function builds a json object
%% containing all numbers assigned to it
%% @end
%%--------------------------------------------------------------------
-spec get_trunkstore_account_numbers/1 :: (ne_binary()) -> json_object().
get_trunkstore_account_numbers(Account) ->
    case couch_mgr:open_doc(?TS_DB, Account) of
        {ok, JObj} ->
            case is_trunkstore_account(JObj) of
                true ->
                    ?LOG("account ~s is a trunkstore doc...", [Account]),
                    Assigned = [wh_json:get_value(<<"DIDs">>, Server, ?EMPTY_JSON_OBJECT)
                                || Server <- wh_json:get_value(<<"servers">>, JObj, ?EMPTY_JSON_OBJECT)],
                    Unassigned = [wh_json:get_value(<<"DIDs_Unassigned">>, JObj, ?EMPTY_JSON_OBJECT)],
                    lists:foldr(fun({struct, _} = Numbers, Acc) ->
                                        wh_json:get_keys(Numbers) ++ Acc;
                                   (_, Acc) -> Acc
                                end, [], Assigned ++ Unassigned);
                false -> []
            end;
        {error, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates or creates a route document for the given account with the
%% provided numbers
%% @end
%%--------------------------------------------------------------------
-spec reconcile_numbers/2 :: (list(), ne_binary()) -> ok.
reconcile_numbers([], _) ->
    ok;
reconcile_numbers([Number|Numbers], AccountId) ->
    wh_number_manager:reconcile_number(Number, AccountId),
    reconcile_numbers(Numbers, AccountId).
