-module(wh_transactions).

-include_lib("whistle/include/wh_types.hrl").

-export([
         create/2
         ,save/1
        ]).

-record(wh_transaction, {
          account_id :: ne_binary()
         ,ff_description :: binary()
         ,pvt_amount :: integer()
         ,pvt_type :: 'credit' | 'debit'
         ,pvt_created :: wh_now()
         ,pvt_account_id :: ne_binary()
         }).

-type wh_transaction() :: #wh_transaction{}.
-export_type([wh_transaction/0]).

create(Amount, Op) when not is_integer(Amount) ->
    create(wh_util:to_float(Amount), Op);
create(Amount, Op) ->
    #wh_transaction{pvt_type=Op
                ,pvt_amount=Amount
               }.

save(#wh_transaction{}=Transaction) ->
    check_before_save(Transaction).

check_before_save(#wh_transaction{}=Tr) ->  
    case Tr#wh_transaction{} of
        ok ->
            ok
    end.


