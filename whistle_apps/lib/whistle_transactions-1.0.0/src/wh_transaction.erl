-module(wh_transaction).

-export([credit/2
         ,debit/2
        ]).

-export([set_account_id/2
         ,set_pvt_account_id/2
         ,set_ff_description/2
        ]).

credit(Amount, Reason) ->
    wh_transactions:create(Amount, 'credit', Reason).


debit(Amount, Reason) ->
    wh_transactions:create(Amount, 'debit', Reason).


set_account_id(AccountId, Transaction) ->
    wh_transactions:set_account_id(AccountId, Transaction).


set_pvt_account_id(AccountId, Transaction) ->
    wh_transactions:set_pvt_account_id(AccountId, Transaction).


set_ff_description(Description, Transaction) ->
    wh_transactions:set_ff_description(Description, Transaction).

