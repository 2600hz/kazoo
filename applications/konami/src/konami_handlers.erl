%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_handlers).

-export([handle_metaflow/2
         ,handle_dtmf/2
        ]).

-include("konami.hrl").

-define(KONAMI_REG(CallId), {'p', 'l', {'dtmf', CallId}}).

-spec handle_dtmf(wh_json:object(), wh_proplist()) -> any().
handle_dtmf(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("recv DTMF ~s", [DTMF]),
    [konami_code_fsm:dtmf(FSM, CallId, DTMF) || FSM <- gproc:lookup_pids(?KONAMI_REG(CallId))].

-spec handle_metaflow(wh_json:object(), wh_proplist()) -> no_return().
handle_metaflow(JObj, Props) ->
    'true' = wapi_dialplan:metaflow_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    whapps_call:put_callid(Call),

    gproc:reg(?KONAMI_REG(whapps_call:call_id_direct(Call))),
    konami_dtmf_listener:add_call_binding(Call),

    try konami_code_fsm:start_fsm(
          whapps_call:kvs_store('consumer_pid', props:get_value('server', Props), Call)
          ,JObj
         )
    of
        _ -> 'ok'
    catch
        'exit':'normal' -> 'ok';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed to start FSM: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

