%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_reseller).

-export([get_reseller_id/1]).
-export([assign/1]).
-export([unassign/1]).
-export([assign_representative/1]).
-export([unassign_representative/1]).
-export([get_represenative/1]).
-export([admins/1]).
-export([settings/2]).

-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/wh_types.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account definition see if the reseller is set, if not
%% assume it is belongs to the master account
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id/1 :: (wh_json:json_object()) -> ne_binary() | 'undefined'.
get_reseller_id(JObj) ->
    wh_json:get_value(<<"pvt_reseller_id">>, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign the first account in the pvt_tree who is a reseller of the 
%% master account
%% @end
%%--------------------------------------------------------------------
-spec assign/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
-spec assign/2 :: (ne_binary(), wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.

assign(JObj) ->
    case whapps_util:get_master_account_id() of
        {error, _R}=E ->
            lager:debug("unable to assign reseller, master account is not known: ~p", [_R]),
            E;
        {ok, MasterAccountId} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
            ResellerId = find_reseller(Tree, MasterAccountId),
            assign(ResellerId, JObj)
    end.            
        
assign(ResellerId, JObj) ->
    ResellerDb = wh_util:format_account_id(ResellerId, encoded),
    case couch_mgr:db_exist(ResellerDb) of
        false -> {error, bad_reseller_id};
        true ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            _ =  assign_representative(JObj),
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_reseller_id">>, ResellerId, JObj)) of
                {error, _}=E -> E;
                {ok, JObj} -> 
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign the master account as the reseller for this account
%% @end
%%--------------------------------------------------------------------
-spec unassign/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
unassign(JObj) ->
    case whapps_util:get_master_account_id() of
        {error, _R}=E ->
            lager:debug("unable to unassign reseller, master account is not known: ~p", [_R]),
            E;
        {ok, MasterAccountId} ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            _ = unassign_representative(JObj),
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_reseller_id">>, MasterAccountId, JObj)) of
                {error, _}=E -> E;
                {ok, JObj} -> 
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to assign to an account rep in the resellers account
%% @end
%%--------------------------------------------------------------------
-spec assign_representative/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
assign_representative(JObj) ->
    case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),
            ViewOptions = [{<<"limit">>, 1}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"reseller/count_assignments">>, ViewOptions) of
                {error, _R}=E ->
                    lager:debug("failed to assign reseller representative: ~p", [_R]),
                    E;
                {ok, [Results]} ->
                    AccountId = wh_json:get_value(<<"_id">>, JObj),
                    Rep = wh_json:get_value(<<"doc">>, Results),
                    Assignments = wh_json:get_value(<<"pvt_account_assignments">>, Rep, []), 
                    couch_mgr:save_doc(ResellerDb, wh_json:set_value(<<"pvt_account_assignments">>, [AccountId|Assignments], Rep))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to remove any assignments to a resellers rep
%% @end
%%--------------------------------------------------------------------
-spec unassign_representative/1 :: (json:json_object()) -> ok.
unassign_representative(JObj) ->
    case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
        undefined -> ok;
        ResellerId ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),
            ViewOptions = [{<<"include_docs">>, true}
                           ,{<<"key">>, AccountId}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"reseller/find_assignments">>, ViewOptions) of
                {error, _R} ->
                    lager:debug("failed to find reseller representatives: ~p", [_R]),
                    ok;
                {ok, Results} ->
                    _ = [begin
                             Rep = wh_json:get_value(<<"doc">>, Result),
                             Assignments = wh_json:get_value(<<"pvt_account_assignments">>, Rep, []),                          
                             couch_mgr:save_doc(ResellerDb
                                                ,wh_json:set_value(<<"pvt_account_assignments">>
                                                                       ,lists:delete(AccountId, Assignments)
                                                                   ,Rep)
                                               )
                         end || Result <- Results],
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_represenative/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
get_represenative(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    case get_reseller_id(JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),    
            ViewOptions = [{<<"include_docs">>, true}
                           ,{<<"key">>, AccountId}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"reseller/find_assignments">>, ViewOptions) of
                {error, _R}=E -> 
                    lager:debug("unable to find reseller account represenatives: ~p", [_R]),
                    E;
                {ok, []} -> assign_representative(JObj);
                {ok, [Rep|_]} -> {ok, wh_json:get_value(<<"doc">>, Rep, wh_json:new())}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a list of users who are admins in the resellers account
%% @end
%%--------------------------------------------------------------------
-spec admins/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_objects()} | {'error', _}.
admins(JObj) ->
    case get_reseller_id(JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),
            ViewOptions = [{<<"key">>, <<"user">>}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"maintenance/listing_by_type">>, ViewOptions) of
                {ok, Users} -> 
                    Admins = [wh_json:get_value(<<"doc">>, User) 
                              || User <- Users
                                     ,wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>
                             ],
                    {ok, Admins};
                {error, _R}=E -> 
                    lager:debug("failed to find reseller ~s account admins: ~p", [ResellerId, _R]),
                    E
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec settings/2 :: (ne_binary(), wh_json:json_object()) -> wh_json:json_objects().
settings(Key, JObj) ->
    case get_reseller_id(JObj) of
        undefined -> wh_json:new();
        ResellerId -> whapps_account_config:get(ResellerId, Key)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Scan up the parent tree until one of the accounts belongs to the 
%% system account, that account will be the reseller.
%% @end
%%--------------------------------------------------------------------
-spec find_reseller/2 :: ([ne_binary(),...] | [], ne_binary()) -> ne_binary().
find_reseller([], MasterAccountId) ->             
    MasterAccountId;
find_reseller([ParentId|Tree], MasterAccountId) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, ParentId) of
        {ok, JObj} ->
            case get_reseller_id(JObj) =:= MasterAccountId of
                true -> ParentId;
                false -> find_reseller(Tree, MasterAccountId)
            end;
        {error, _R} ->
            lager:debug("ignoring the ancestor ~s during reseller hunt, unable to open the account definition: ~p", [ParentId, _R]),
            find_reseller(Tree, MasterAccountId)
    end.
