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
-export([set_created_flag/2]).
-export([set_deleted_flag/2]).
-export([process_activation_charges/3]).
-export([update_quantity/4]).
-export([increment_quantity/3]).
-export([reset_category_addons/2]).
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
-spec fetch/1 :: (ne_binary()) -> {'ok', resellers()} | {'error', 'no_service_plan'}.
-spec fetch/3 :: ('undefined'| ne_binary(), ne_binary(), resellers()) -> {'ok', resellers()} | {'error', 'no_service_plan'}.

fetch(AccountId) ->
    fetch(AccountId, AccountId, []).
                            
fetch(undefined, _, []) ->  
    {error, no_service_plan};
fetch(undefined, _, Resellers) ->  
    {ok, Resellers};
fetch(AccountId, InvokingAccountId, Resellers) ->  
    case wh_reseller:fetch(AccountId, InvokingAccountId) of
        {error, {no_service_plan, AccountId}} ->
            fetch(undefined, InvokingAccountId, Resellers); 
        {error, {no_service_plan, ResellerId}} ->
            fetch(ResellerId, InvokingAccountId, Resellers);
        {error, _R} ->
            fetch(undefined, InvokingAccountId, Resellers);
        {ok, Reseller} ->
            case  wh_reseller:get_reseller_id(Reseller) of
                AccountId ->
                    fetch(undefined, InvokingAccountId, [Reseller|Resellers]);
                ResellerId ->
                    fetch(ResellerId, InvokingAccountId, [Reseller|Resellers])
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_created_flag/2 :: (ne_binary(), resellers()) -> resellers().
set_created_flag(Created, Resellers) ->
    [wh_reseller:set_created_flag(Created, Reseller) 
     || Reseller <- Resellers
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_deleted_flag/2 :: (ne_binary(), resellers()) -> resellers().
set_deleted_flag(Deleted, Resellers) ->
    [wh_reseller:set_deleted_flag(Deleted, Reseller) 
     || Reseller <- Resellers
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_activation_charges/3 :: (ne_binary(), ne_binary(), resellers()) -> resellers().
process_activation_charges(Category, Name, Resellers) ->
    [wh_reseller:process_activation_charges(Category, Name, Reseller)
     || Reseller <- Resellers
    ].    

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
-spec reset_category_addons/2 :: (ne_binary(), resellers()) -> resellers().
reset_category_addons(Category, Resellers) ->
    [wh_reseller:reset_category_addons(Category, Reseller) 
     || Reseller <- Resellers
    ].    

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Commit any billing changes to the resellers.
%% @end
%%--------------------------------------------------------------------
-spec commit_changes/1 :: (resellers()) -> 'ok'.
commit_changes(Resellers) ->
    _ = [wh_reseller:commit_changes(Reseller) 
         || Reseller <- Resellers
        ],
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
