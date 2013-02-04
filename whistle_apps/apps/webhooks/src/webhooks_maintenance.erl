%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
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
-spec local_summary() -> 'ok'.
local_summary() ->
    ok.

-spec local_summary(ne_binary()) -> 'ok'.
local_summary(AcctId) when not is_binary(AcctId) ->
    local_summary(wh_util:to_binary(AcctId));
local_summary(_AcctId) ->
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec do_summary(wh_json:json_objects(), fun()) -> 'ok'.
do_summary(Accts, PrintFun) ->
    [PrintFun(Acct) || Acct <- Accts].

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary(wh_json:json_object()) -> 'ok'.
print_summary(_Acct) ->
    io:format("No Summary~n", []).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_details(wh_json:json_object()) -> 'ok'.
print_details(Acct) ->
    [io:format("~s: ~s~n", [K, wh_util:to_list(V)]) || {K, V} <- wh_json:to_proplist(Acct)].
