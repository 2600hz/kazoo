  -module(wh_transactions).

-export([save/1
         ,fetch_since/2
         ,fetch_last/2
        ]).

save(L) ->
    save(L, []).

save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | L], Acc) ->
    Res = wh_transaction:save(Transaction),
    save(L, [Res | Acc]).
    
fetch_last(AccountId, Num) ->
    AccountDB = wh_util:format_account_id(AccountId, encoded),
    ViewOptions = [{limit, Num}
                   ,include_docs],
    case couch_mgr:get_results(AccountDB, <<"transactions/by_timestamp">>, ViewOptions) of
        {ok, []} -> 
            lager:debug("no transactions for that account ~p", [AccountId]),
            [];
        {ok, ViewRes} -> 
            viewres_to_recordlist(ViewRes);
        {error, _R} -> 
            lager:debug("unable to get transactions for that account ~p. ~n Reason: ~p", [AccountId, _R]),
            []
    end.

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
            viewres_to_recordlist(ViewRes);
        {error, _R} -> 
            lager:debug("unable to get transactions for that range from ~p to ~p on ~p. ~n Reason: ~p", [Date, Now, AccountId, _R]),
            []
    end.

viewres_to_recordlist(ViewRes) ->
    L = [wh_json:get_value(<<"doc">>, Tr) || Tr <- ViewRes],
    [wh_transaction:from_json(Tr) || Tr <- L].
    

