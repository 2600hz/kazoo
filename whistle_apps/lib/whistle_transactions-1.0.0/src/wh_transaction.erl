-module(wh_transaction).

-export([credit/1
         ,debit/1
        ]).


credit(Amount) ->
    wh_transactions:create(Amount, 'credit').

debit(Amount) ->
    wh_transactions:create(Amount, 'debit').


