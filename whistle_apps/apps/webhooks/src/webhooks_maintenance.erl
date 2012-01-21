%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_maintenance).

-export([local_summary/0, local_summary/1]).

-include("webhooks.hrl").

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% look up a cached registration by realm and optionally username
%% @end
%%-----------------------------------------------------------------------------
-spec local_summary/0 :: () -> 'ok'.
local_summary() ->
    ok.

-spec local_summary/1 :: (ne_binary()) -> 'ok'.
local_summary(AcctId) when not is_binary(AcctId) ->
    local_summary(wh_util:to_binary(AcctId));
local_summary(AcctId) ->
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec do_summary/2 :: (wh_json:json_objects(), fun()) -> 'ok'.
do_summary(Accts, PrintFun) ->
    [PrintFun(Accts) || Acct <- Accts].

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary/1 :: (wh_json:json_object()) -> 'ok'.
print_summary(Acct) ->
    io:format("No Summary~n", []).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_details/1 :: (wh_json:json_object()) -> 'ok'.
print_details(Acct) ->
    [io:format("~s: ~s~n", [K, wh_util:to_list(V)]) || {K, V} <- wh_json:to_proplist(Acct)].
