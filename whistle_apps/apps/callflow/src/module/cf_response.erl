%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_response).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, ctrl_q=CtrlQ}) ->
    put(callid, CallId),
    Code = wh_json:get_value(<<"code">>, Data, <<"486">>),
    Cause = wh_json:get_value(<<"message">>, Data),
    Media = wh_json:get_value(<<"media">>, Data),
    ?LOG("repsonding to call with ~s ~s", [Code, Cause]),
    wh_util:call_response(CallId, CtrlQ, Code, Cause, Media),
    CFPid ! { stop }.
