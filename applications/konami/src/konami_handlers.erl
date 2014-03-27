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
         ,handle_destroy/2
        ]).

-include("konami.hrl").

-define(KONAMI_REG(CallId), {'p', 'l', {'dtmf', CallId}}).
-define(KONAMI_DTMF(DTMF), {'dtmf', DTMF}).
-define(KONAMI_DESTROY, 'destroy').

-spec handle_dtmf(wh_json:object(), wh_proplist()) -> any().
handle_dtmf(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    DTMF = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("recv DTMF ~s", [DTMF]),
    gproc:send(?KONAMI_REG(CallId), ?KONAMI_DTMF(DTMF)).

handle_destroy(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    wh_util:put_callid(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    gproc:send(?KONAMI_REG(CallId), ?KONAMI_DESTROY).

handle_metaflow(JObj, _Props) ->
    'true' = wapi_dialplan:metaflow_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    whapps_call:put_callid(Call),

    lager:debug("call: ~s", [whapps_call:control_queue(Call)]),

    gproc:reg(?KONAMI_REG(whapps_call:call_id_direct(Call))),
    lager:debug("registered for DTMF"),
    wait_for_binding_key(Call, binding_key(Call, JObj)).

-spec binding_key(whapps_call:call(), wh_json:object()) -> ne_binary().
binding_key(Call, JObj) ->
    case wh_json:get_value(<<"Binding-Key">>, JObj) of
        'undefined' ->
            konami_config:binding_key(whapps_call:account_id(Call));
        BindingKey ->
            lager:debug("using custom binding key '~s'", [BindingKey]),
            BindingKey
    end.

wait_for_binding_key(Call, BindingKey) ->
    receive
        ?KONAMI_DTMF(BindingKey) ->
            lager:debug("binding key '~s' recv", [BindingKey]),
            wait_for_binding_key(Call, BindingKey);
        ?KONAMI_DTMF(_DTMF) ->
            lager:debug("ignoring DTMF: '~s'", [_DTMF]),
            wait_for_binding_key(Call, BindingKey);
        ?KONAMI_DESTROY ->
            lager:debug("channel has gone down, done");
        _Other ->
            lager:debug("recv other: ~p", [_Other]),
            wait_for_binding_key(Call, BindingKey)
    end.
