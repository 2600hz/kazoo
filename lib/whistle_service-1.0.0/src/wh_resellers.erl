%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% Service plans may be applied in a hierarchy such as:
%%%
%%% 2600hz (Resller: 2600hz, Billing ID: 2600hz)
%%%   | -- Account 1 (Resller: 2600hz, Billing ID: Account 1)
%%%   | -- Account 2 (Resller: 2600hz, Billing ID: Account 2)
%%%   | -- Account 3 (Reseller: 2600hz, Billing ID: Reseller 1)
%%%   |      | -- Account 4 (Reseller: Account 3, Billing ID: Account 4)
%%%   |      | -- Account 5 (Reseller: Account 3, Billing ID: Account 5)
%%%   |      |      | -- Account 6 (Reseller: Account 3, Billing ID: Account 5)
%%%   |      |      | -- Account 7 (Reseller: Account 3, Billing ID: Account 7)
%%%  ...    ...    ...
%%%
%%% Currently only one additional level is supported; 
%%% the master account and one reseller.  This is due to 
%%% limitations of the jonny5 whapp and additional PDD 
%%% incured when enforcing limits of deeply nested resellers.
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_resellers).

-export([fetch/1]).
-export([update_quantity/4]).
-export([increment_quantity/3]).
-export([reset_category/2]).
-export([commit_changes/1]).

-include("wh_service.hrl").

-type(resellers() :: [wh_reseller:reseller()] | []).
-export_type([resellers/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account id this will create a list of service plans that
%% the account and possibly a reseller are subscribed to.
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> {'ok', resellers()}.
-spec fetch/2 :: (ne_binary(), resellers()) -> {'ok', resellers()}.

fetch(Account) ->
    fetch(Account, []).

fetch(Account, Resellers) ->
    case wh_reseller:fetch(Account) of
        {error, no_service_plan} -> {ok, Resellers};
        {ok, Reseller} ->
            case wh_reseller:is_master_reseller(Reseller) of
                true -> {ok, [Reseller|Resellers]};
                false ->
                    fetch(wh_reseller:get_reseller_id(Reseller), [Reseller|Resellers])
            end
    end.
        
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the plans that apply to this account update the quanity, and
%% accumulate any transactions as required.
%% @end
%%--------------------------------------------------------------------
-spec update_quantity/4 :: (ne_binary(), ne_binary(), ne_binary() | integer(), resellers()) -> resellers().
update_quantity(Category, Name, Quantity, Resellers) ->
    [wh_reseller:update_quantity(Category, Name, Quantity, Reseller) 
     || Reseller <- Resellers
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the plans that apply to this account update the quanity, and
%% accumulate any transactions as required.
%% @end
%%--------------------------------------------------------------------
-spec increment_quantity/3 :: (ne_binary(), ne_binary(), resellers()) -> resellers().
increment_quantity(Category, Name, Resellers) ->
    [wh_reseller:increment_quantity(Category, Name, Reseller) 
     || Reseller <- Resellers
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the quantity of all addons on a given category to 0
%% @end
%%--------------------------------------------------------------------
-spec reset_category/2 :: (ne_binary(), resellers()) -> resellers().
reset_category(Category, Resellers) ->
    [wh_reseller:reset_category(Category, Reseller) 
     || Reseller <- Resellers
    ].    

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Commit any billing changes to the resellers.
%% @end
%%--------------------------------------------------------------------
-spec commit_changes/1 :: (resellers()) -> ok | {'error', wh_json:json_object()}.
commit_changes(Resellers) ->
    try
        [wh_reseller:commit_changes(Reseller) 
         || Reseller <- Resellers
        ],
        ok
    catch
        throw:Error ->
            {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
