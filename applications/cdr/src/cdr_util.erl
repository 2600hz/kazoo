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

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%-spec format_account_id(binary(), integer(), integer()) -> 'ok'.

format_account_id(AccountId, CDRYear, CDRMonth) ->
    <<(wh_util:format_account_id(AccountId, 'encoded'))/binary,"-",(wh_util:to_binary(CDRYear))/binary,(wh_util:to_binary(CDRMonth))/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
