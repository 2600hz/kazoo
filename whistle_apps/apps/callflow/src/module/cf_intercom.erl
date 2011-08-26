%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_intercom).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [set/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(_, #cf_call{capture_group=undefined, cf_pid=CFPid, call_id=CallId}) ->
    put(callid, CallId),
    CFPid ! { continue };
handle(_, #cf_call{capture_group=Digits, account_id=AccountId, cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    set(undefined, {struct, [{<<"Auto-Answer">>, <<"true">>}]}, Call),
    case gen_server:call(cf_responder, {find_flow, wh_util:to_binary(Digits), AccountId}, 2000) of
        {ok, Flow, false} ->
            CFPid ! {branch, wh_json:get_value(<<"flow">>, Flow, ?EMPTY_JSON_OBJECT)};
        _ ->
            CFPid ! { continue }
    end.
