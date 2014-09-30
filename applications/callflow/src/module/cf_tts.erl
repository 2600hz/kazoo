%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
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

    {'ok', Call1} = wait_for_tts(Call, NoopId),

    %% Give control back to cf_exe process
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).

-spec wait_for_tts(whapps_call:call(), ne_binary()) -> {'ok', whapps_call:call()}.
wait_for_tts(Call, NoopId) ->
    case whapps_call_command:receive_event(?MILLISECONDS_IN_DAY) of
        {'ok', JObj} ->
            process_event(Call, NoopId, JObj);
        {'error', 'timeout'} ->
            lager:debug("timed out waiting for tts(~s) to complete", [NoopId]),
            {'ok', Call}
    end.

-spec process_event(whapps_call:call(), ne_binary(), wh_json:object()) ->
                           {'ok', whapps_call:call()} |
                           {'error', _}.
process_event(Call, NoopId, JObj) ->
    case whapps_call_command:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            lager:debug("channel was destroyed"),
            {'error', 'channel_destroy'};
        {<<"error">>, _, <<"noop">>} ->
            lager:debug("channel execution error while waiting for ~s: ~s", [NoopId, wh_json:encode(JObj)]),
            {'error', JObj};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"noop">>} ->
            lager:debug("tts has finished"),
            {'ok', Call};
        {<<"call_event">>, <<"DTMF">>, _} ->
            DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
            lager:debug("recv DTMF ~s, adding to default", [DTMF]),
            wait_for_tts(whapps_call:add_to_dtmf_collection(DTMF, Call), NoopId);
        _Ignore ->
            wait_for_tts(Call, NoopId)
    end.
