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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Code = wh_json:get_value(<<"code">>, Data, <<"486">>),
    Cause = wh_json:get_ne_value(<<"message">>, Data),
    Media = wh_json:get_ne_value(<<"media">>, Data),
    lager:info("responding to call with ~s ~s", [Code, Cause]),
    _ = whapps_call_command:response(Code, Cause, Media, Call),
    cf_exe:stop(Call).
