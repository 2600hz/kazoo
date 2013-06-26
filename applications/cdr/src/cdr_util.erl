%%%-------------------------------------------------------------------
%%% @author  <bwann@kazoodev.bwann-kazoo.local>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Jun 2013 by  <bwann@kazoodev.bwann-kazoo.local>
%%%-------------------------------------------------------------------
-module(cdr_util).

%% API
-export([format_account_id/3]).

%%%===================================================================
%%% API
%%%===================================================================

%-spec format_account_id(binary(), integer(), integer()) -> 'ok'.

format_account_id(AccountId, CdrYear, CdrMonth) ->
    wh_util:format_account_id(list_to_binary([AccountId, <<"-">>, wh_util:to_binary(CdrYear), wh_util:to_binary(CdrMonth)]), 'encoded').

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
