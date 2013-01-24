%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(wh_transaction).

-include_lib("whistle/include/wh_types.hrl").

-export([debit/2
         ,credit/2
        ]).

-export([set_account_id/2
         ,set_ff_description/2
         ,set_pvt_account_id/2
        ]).

-export([save/1
         ,fetch/1
        ]).

-export([to_json/1
         ,from_json/1
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform transaction record to Json object
%% @end
%%--------------------------------------------------------------------
to_json(#wh_transaction{}=T) ->
    wh_json:from_list([{<<"account_id">>, T#wh_transaction.account_id}
                       ,{<<"ff_description">>, T#wh_transaction.ff_description}
                       ,{<<"pvt_reason">>, T#wh_transaction.pvt_reason}
                       ,{<<"pvt_amount">>, T#wh_transaction.pvt_amount}
                       ,{<<"pvt_type">>, T#wh_transaction.pvt_type}
                       ,{<<"pvt_created">>, T#wh_transaction.pvt_created}
                       ,{<<"pvt_account_id">>, T#wh_transaction.pvt_account_id}
                      ]).
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Transform Json Object to transaction record
%% @end
%%--------------------------------------------------------------------
from_json(JObj) ->
    #wh_transaction{
      account_id = wh_json:get_ne_value(<<"account_id">>, JObj)
      ,ff_description = wh_json:get_ne_value(<<"ff_description">>, JObj)
      ,pvt_reason = wh_json:get_ne_value(<<"pvt_reason">>, JObj)
      ,pvt_amount = wh_json:get_ne_value(<<"pvt_amount">>, JObj)
      ,pvt_type = wh_json:get_ne_value(<<"pvt_type">>, JObj)
      ,pvt_created = wh_json:get_ne_value(<<"pvt_created">>, JObj)
      ,pvt_account_id = wh_json:get_ne_value(<<"pvt_account_id">>, JObj)
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type credit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
credit(Amount, Reason) ->
    create(Amount, 'credit', Reason).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create transaction record of type debit (with Amount & Reason)
%% @end
%%--------------------------------------------------------------------
debit(Amount, Reason) ->
    create(Amount, 'debit', Reason).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the account ID
%% @end
%%--------------------------------------------------------------------
set_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{account_id=AccountId}.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set free form description
%% @end
%%--------------------------------------------------------------------
set_ff_description(Desc, Transaction) ->
    Transaction#wh_transaction{ff_description=Desc}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set private account ID
%% @end
%%--------------------------------------------------------------------
set_pvt_account_id(AccountId, Transaction) ->
    Transaction#wh_transaction{pvt_account_id=AccountId}.

%%--------------------------------------------------------------------
%%
%% DATABASE FUNCTIONS
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Save transaction to database
%% @end
%%--------------------------------------------------------------------
save(#wh_transaction{}=Transaction) ->
    Errors = validate_funs(Transaction),
    case validate(Errors) of
        {true, _} ->
            _Transaction1 = set_private_properties(Transaction),
            save;
        {false, _R} ->
            dont_save
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetch a transaction from the database
%% @end
%%--------------------------------------------------------------------
fetch(ID) ->
    ok.

%%--------------------------------------------------------------------
%%
%% PRIVATE FUNCTIONS
%%
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create transaction record
%% @end
%%--------------------------------------------------------------------
create(Amount, Op, Reason) when not is_integer(Amount) ->
    create(wh_util:to_integer(Amount), Op, Reason);
create(Amount, Op, Reason) when Amount < 0 ->
    create(Amount*-1, Op, Reason);
create(Amount, Op, Reason) ->
    #wh_transaction{pvt_reason=Reason
                    ,pvt_type=Op
                    ,pvt_amount=Amount
                   }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return Errors of record when trying to save
%% @end
%%--------------------------------------------------------------------
validate(Errors) ->
    validate(Errors, true, []).

validate([], Valid, Acc) ->
    {Valid, Acc};
validate([ok | Errors], Valid, Acc) ->
    validate(Errors, Valid, Acc);
validate([{error, Reason} | Errors], _, Acc) ->
    validate(Errors, false, [Reason | Acc]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call all the different validate function
%% @end
%%--------------------------------------------------------------------
validate_funs(#wh_transaction{}=Tr) ->  
    Funs = [fun validate_account_id/1],
    lists:foldl(fun(F, Acc) -> [F(Tr) | Acc] end, [], Funs).

            
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the account ID
%% @end
%%--------------------------------------------------------------------
validate_account_id(#wh_transaction{account_id=AccountId}) ->
    case is_binary(AccountId) of
        true -> ok;
        false -> {error, account_id}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call functions to set private properties 
%% @end
%%--------------------------------------------------------------------    
set_private_properties(Transaction) ->
    PvtFuns = [fun set_pvt_created/1 ],
    lists:foldl(fun(F, C) -> F(C) end, Transaction, PvtFuns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set creation date
%% @end
%%--------------------------------------------------------------------
set_pvt_created(Transaction) ->
    Transaction#wh_transaction{pvt_created=wh_util:current_tstamp()}.

