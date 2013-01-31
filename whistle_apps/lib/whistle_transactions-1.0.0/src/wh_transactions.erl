  -module(wh_transactions).

-export([save/1
         ,fetch_by_date/2
        ]).

save(L) ->
    save(L, []).

save([], Acc) ->
    lists:reverse(Acc);
save([Transaction | L], Acc) ->
    Res = wh_transaction:save(Transaction),
    save(L, [Res | Acc]).
    
fetch_by_date(AccountId, Date) ->
    ok.
