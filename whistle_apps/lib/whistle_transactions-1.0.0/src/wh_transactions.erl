%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_transactions).

-include_lib("whistle/include/wh_types.hrl").

-export([save/1
         ,fetch_since/2
         ,fetch_last/2
        ]).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wh_transaction:wh_transactions()) -> wh_transaction:wh_transactions().
save(L) ->
    save(L, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save list of record
%% @end
%%--------------------------------------------------------------------
-spec save/2 :: (wh_transaction:wh_transactions(), wh_transaction:wh_transactions()) -> wh_transaction:wh_transactions().
save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | L], Acc) ->
    Res = wh_transaction:save(Transaction),
    save(L, [Res | Acc]).
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transactions
%% @end
%%--------------------------------------------------------------------
-spec fetch_last/2 :: (ne_binary(), integer()) -> wh_transaction:wh_transactions().
fetch_last(AccountId, Num) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    ViewOptions = [{limit, Num}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {ok, []} -> 
            lager:debug("no transactions for that account ~p", [AccountId]),
            [];
        {ok, ViewRes} -> 
            viewres_to_recordlist(ViewRes)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% fetch last transaction from date to now
%% @end
%%--------------------------------------------------------------------
-spec fetch_since/2 :: (ne_binary(), integer()) -> wh_transaction:wh_transactions().
fetch_since(AccountId, Date) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    Now = wh_util:current_tstamp(),
    ViewOptions = [{startkey, Date}
                   ,{endkey, Now}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {ok, []} -> 
            lager:debug("no transactions for that range from ~p to ~p on ~p", [Date, Now, AccountId]),
            [];
        {ok, ViewRes} -> 
            viewres_to_recordlist(ViewRes)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% fetch last transaction
%% @end
%%--------------------------------------------------------------------
-spec viewres_to_recordlist/1 :: (list()) -> wh_transaction:wh_transactions().
viewres_to_recordlist(ViewRes) ->
    L = [wh_json:get_value(<<"doc">>, Tr) || Tr <- ViewRes],
    [wh_transaction:from_json(Tr) || Tr <- L].
    

