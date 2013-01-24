-module(wh_transactions).

-include_lib("whistle/include/wh_types.hrl").

-export([create/3
         ,save/1
        ]).

-export([set_account_id/2
         ,set_ff_description/2
         ,set_pvt_account_id/2
        ]).

-record(wh_transaction, {
          % or customer ID and pvt ?
          account_id :: ne_binary()
         ,ff_description :: binary()
         ,pvt_reason :: ne_binary()
         ,pvt_amount :: integer()
         ,pvt_type :: 'credit' | 'debit'
         ,pvt_created :: wh_now()
         ,pvt_account_id :: ne_binary()
         }).

-type wh_transaction() :: #wh_transaction{}.
-export_type([wh_transaction/0]).

create(Amount, Op, Reason) when not is_integer(Amount) ->
    create(wh_util:to_integer(Amount), Op, Reason);
create(Amount, Op, Reason) when Amount < 0 ->
    create(Amount*-1, Op, Reason);
create(Amount, Op, Reason) ->
    #wh_transaction{pvt_reason=Reason
                    ,pvt_type=Op
                    ,pvt_amount=Amount
                   }.

set_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{account_id=AccountId}.


set_ff_description(Desc, Transaction) ->
    Transaction#wh_transaction{ff_description=Desc}.


set_pvt_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{pvt_account_id=AccountId}.


save(#wh_transaction{}=Transaction) ->
    Errors = validate_funs(Transaction),
    case validate(Errors) of
        {true, _} ->
            _Transaction1 = set_private_properties(Transaction),
            save;
        {false, _R} ->
            dont_save
    end.    

validate(Errors) ->
    validate(Errors, true, []).

validate([], Valid, Acc) ->
    {Valid, Acc};
validate([ok | Errors], Valid, Acc) ->
    validate(Errors, Valid, Acc);
validate([{error, Reason} | Errors], _, Acc) ->
    validate(Errors, false, [Reason | Acc]).


validate_funs(#wh_transaction{}=Tr) ->  
    Funs = [fun validate_account_id/1],
    lists:foldl(fun(F, Acc) -> [F(Tr) | Acc] end, [], Funs).

            
validate_account_id(#wh_transaction{account_id=AccountId}) ->
    case is_binary(AccountId) of
        true -> ok;
        false -> {error, account_id}
    end.

            
set_private_properties(Transaction) ->
    PvtFuns = [fun set_pvt_created/1 ],
    lists:foldl(fun(F, C) -> F(C) end, Transaction, PvtFuns).

    
set_pvt_created(Transaction) ->
    Transaction#wh_transaction{pvt_created=wh_util:current_tstamp()}.

