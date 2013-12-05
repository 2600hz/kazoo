%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_bookkeeper_local).

-export ([sync/2]).
-export([commit_transactions/2]).
-export ([charge_transactions/2]).

-include("../whistle_services.hrl").


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
sync(_Items, _AccountId) -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(ne_binary(), wh_transactions:wh_transactions()) -> ok.
commit_transactions(_BillingId, Transactions) ->
    wh_transactions:save(Transactions),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(ne_binary(), wh_transactions:wh_transactions()) -> ok.
charge_transactions(BillingId, Transactions) -> 'ok'.

