%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whistle_number_manager_maintenance).

-export([reconcile/0, reconcile/1]).
-export([reconcile_numbers/0, reconcile_numbers/1]).
-export([reconcile_accounts/0, reconcile_accounts/1]).

-include("wh_number_manager.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%% These are temporary until the viewing of numbers in an account can
%% be standardized
-define(TS_DB, <<"ts">>).

%% TODO: This makes stepswitch dependent on callflow view... This is safe-ish
%% beacuse if you reconcile without the callflow view then they will never
%% run anyway (no callflow whapp connected to the db to execute). But it is
%% still nasty...
-define(CALLFLOW_VIEW, <<"callflow/listing_by_number">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Seach the accounts for number assignements and ensure the routes
%% exist
%% @end
%%--------------------------------------------------------------------
-spec reconcile/0 :: () -> 'done'.
-spec reconcile/1 :: (string() | ne_binary() | 'all') -> 'done'.
 
reconcile() ->
    io_lib:format("This command is depreciated, please use reconcile_numbers() or for older systems reconcile_accounts(). See the wiki for details on the differences.", []).

reconcile(Arg) ->
    io_lib:format("This command is depreciated, please use reconcile_numbers() or for older systems reconcile_accounts(~s). See the wiki for details on the differences.", [Arg]).    

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Seach the number databases and ensure all assignments are reflected
%% in the accounts
%% @end
%%--------------------------------------------------------------------
-spec reconcile_numbers/0 :: () -> 'done'.
-spec reconcile_numbers/1 :: (string() | ne_binary() | 'all') -> 'done'.
 
reconcile_numbers() ->
    reconcile_numbers(all).

reconcile_numbers(all) ->
    _ = [reconcile_numbers(Db) 
         || Db <- wnm_util:get_all_number_dbs()
        ],
    ok;
reconcile_numbers(NumberDb) when not is_binary(NumberDb) ->
    reconcile_numbers(wh_util:to_binary(NumberDb));
reconcile_numbers(NumberDb) ->
    Db = wh_util:to_binary(http_uri:encode(wh_util:to_list(NumberDb))),
    case couch_mgr:all_docs(Db) of
        {error, _R}=E -> E;
        {ok, JObjs} ->
            Numbers = [Number 
                       || JObj <- JObjs
                              ,case (Number = wh_json:get_value(<<"id">>, JObj)) of
                                   <<"_design/", _/binary>> -> false;
                                   _Else -> true
                               end
                      ],
            reconcile_numbers(Numbers, undefined)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Seach the accounts for phone numbers and ensure the number is routed
%% to the account (first account found with a duplicate number will win)
%% @end
%%--------------------------------------------------------------------
-spec reconcile_accounts/0 :: () -> 'done'.
-spec reconcile_accounts/1 :: (string() | ne_binary() | 'all') -> 'done'.
 
reconcile_accounts() ->
    reconcile_accounts(all).

reconcile_accounts(all) ->
    _ = [reconcile_accounts(AccountId) || AccountId <- whapps_util:get_all_accounts(raw)],
    done;
reconcile_accounts(AccountId) when not is_binary(AccountId) ->
    reconcile_accounts(wh_util:to_binary(AccountId));
reconcile_accounts(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Numbers = get_callflow_account_numbers(AccountDb),
    Numbers1 = get_trunkstore_account_numbers(AccountId, AccountDb) ++ Numbers,
    _ = reconcile_numbers(Numbers1, wh_util:format_account_id(AccountId, raw)),
    done.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given an account create a json object of all numbers that look to
%% external (TODO: currently just uses US rules).
%% @end
%%--------------------------------------------------------------------
-spec get_callflow_account_numbers/1 :: (ne_binary()) -> wh_json:json_object().
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
%% Given a document info json object from trunkstore returns true if
%% it is a 'info_' document (IE: trunkstore account)
%% @end
%%--------------------------------------------------------------------
-spec is_trunkstore_account/1 :: (wh_json:json_object()) -> boolean().
is_trunkstore_account(JObj) ->
    wh_json:get_value(<<"type">>, JObj) =:= <<"sys_info">> orelse
        wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"sys_info">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a trunkstore account id this function builds a json object
%% containing all numbers assigned to it
%% @end
%%--------------------------------------------------------------------
-spec get_trunkstore_account_numbers/2 :: (ne_binary(), ne_binary()) -> [ne_binary(),...] | [].
get_trunkstore_account_numbers(AccountId, AccountDb) ->
    lager:debug("looking in ~s for trunkstore DIDs", [AccountDb]),
    case couch_mgr:get_results(AccountDb, <<"trunkstore/LookUpDID">>, []) of
        {ok, []} ->
            lager:debug("no trunkstore DIDs listed in account ~s, trying ts db", [AccountDb]),
            get_trunkstore_account_numbers(AccountId);
        {ok, JObjs} ->
            lager:debug("account db ~s has trunkstore DIDs", [AccountDb]),
            Assigned = [wh_json:get_value(<<"key">>, JObj) || JObj <- JObjs],

            TSDocId = wh_json:get_value(<<"id">>, hd(JObjs)),
            {ok, TSDoc} = couch_mgr:open_doc(AccountDb, TSDocId),
            lager:debug("fetched ts doc ~s from ~s", [TSDocId, AccountDb]),

            wh_json:get_keys(wh_json:get_value(<<"DIDs_Unassigned">>, TSDoc, wh_json:new())) ++ Assigned;
        {error, _} ->
            lager:debug("failed to find DIDs in account db, trying ts doc"),
            get_trunkstore_account_numbers(AccountId)
    end.

-spec get_trunkstore_account_numbers/1 :: (ne_binary()) -> [ne_binary(),...] | [].
get_trunkstore_account_numbers(AccountId) ->
    case couch_mgr:open_doc(?TS_DB, AccountId) of
        {ok, JObj} ->
            case is_trunkstore_account(JObj) of
                true ->
                    lager:debug("account ~s is a trunkstore doc...", [AccountId]),
                    Assigned = [wh_json:get_value(<<"DIDs">>, Server, wh_json:new())
                                || Server <- wh_json:get_value(<<"servers">>, JObj, wh_json:new())],
                    Unassigned = [wh_json:get_value(<<"DIDs_Unassigned">>, JObj, wh_json:new())],
                    lists:foldr(fun(Numbers, Acc) ->
                                        case wh_json:is_json_object(Numbers) of
                                            true ->
                                                wh_json:get_keys(Numbers) ++ Acc;
                                            false ->
                                                Acc
                                        end
                                end, [], Assigned ++ Unassigned);
                false -> []
            end;
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates or creates a route document for the given account with the
%% provided numbers
%% @end
%%--------------------------------------------------------------------
-spec reconcile_numbers/2 :: ([ne_binary(),...] | [], 'undefined' | ne_binary()) -> 'ok'.
reconcile_numbers([Number|Numbers], AccountId) ->
    try wh_number_manager:reconcile_number(Number, AccountId, AccountId) of
        _ ->
            reconcile_numbers(Numbers, AccountId)
    catch
        _E:_R ->
            lager:debug("error reconciling ~s: ~p:~p", [Number, _E, _R]),
            reconcile_numbers(Numbers, AccountId)
    end;
reconcile_numbers([], _) ->
    ok.
