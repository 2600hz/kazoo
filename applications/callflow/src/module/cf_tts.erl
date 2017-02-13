%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "text":"This is what should be said"
%%%   // optional
%%%   ,"voice":"male" // or "female"
%%%   ,"language":"en"
%%%   ,"engine":"flite" // or "ispeech if configured
%%% }
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_tts).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    NoopId = whapps_call_command:tts(
               wh_json:get_value(<<"text">>, Data)
               ,wh_json:get_value(<<"voice">>, Data)
               ,wh_json:get_value(<<"language">>, Data)
               ,?ANY_DIGIT
               ,wh_json:get_value(<<"engine">>, Data)
               ,Call
              ),
    handle_noop_recv(Call, cf_util:wait_for_noop(Call, NoopId)).

-spec handle_noop_recv(whapps_call:call(), {'ok', whapps_call:call()} | {'error', _}) -> 'ok'.
handle_noop_recv(_OldCall, {'ok', Call}) ->
    cf_exe:set_call(Call),
    cf_exe:continue(Call);
handle_noop_recv(Call, {'error', 'channel_hungup'}) ->
    cf_exe:hard_stop(Call);
handle_noop_recv(Call, {'error', _E}) ->
    lager:debug("failure executing tts: ~p", [_E]),
    cf_exe:continue(Call).
