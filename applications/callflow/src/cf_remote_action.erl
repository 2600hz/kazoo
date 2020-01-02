%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Executes remote callflow actions.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_remote_action).

-export([handle/2
        ,handle/3
        ]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Action = kapps_call:kvs_fetch('remote-action', Call),
    handle(Action, Data, Call).

-spec handle(atom() | kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Action, Data, Call) ->
    Q = kapps_call:controller_queue(Call),
    ActionId = kz_binary:rand_hex(16),
    API = [{<<"Call">>, call_to_jobj(Call)}
          ,{<<"Data">>, Data}
          ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
          ,{?KEY_MSG_ID, ActionId}
           | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
          ],
    kapi_callflow:publish_action_execute(kz_term:to_binary(Action), API),
    wait_for_msg(ActionId, Call).

-spec call_to_jobj(kapps_call:call()) -> kz_json:object().
call_to_jobj(Call) ->
    Flow = kapps_call:kvs_fetch('cf_flow', kz_json:new(), Call),
    Keys = kz_json:get_keys(kz_json:get_json_value(<<"children">>, Flow, kz_json:new())),

    Routines = [{fun kapps_call:kvs_erase/2, ['cf_flow']}
               ,{fun kapps_call:kvs_store/3, 'action-exit-keys', Keys}
               ,fun kapps_call:clear_helpers/1
               ],
    kapps_call:to_json(kapps_call:exec(Routines, Call)).

-spec wait_for_msg(atom() | kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
wait_for_msg(ActionId, Call) ->
    receive
        {'amqp_msg', {'amqp_return', _JObj, _Returned}} ->
            cf_exe:continue(<<"REMOTE_UNAVAILABLE">>, Call);

        {'amqp_msg', JObj} ->
            Evt = kz_util:get_event_type(JObj),
            CallId = {kz_call_event:call_id(JObj), kapps_call:call_id(Call)},
            MsgId = {kz_api:msg_id(JObj), ActionId},
            handle_msg(Evt, CallId, MsgId, JObj, Call);
        _Msg ->
            lager:debug_unsafe("unhandled msg => ~p", [_Msg]),
            wait_for_msg(ActionId, Call)
    end.

-spec handle_msg(tuple(), tuple(), tuple(), kz_json:object(), kapps_call:call()) -> any().
handle_msg({<<"call_event">>, <<"CHANNEL_DESTROY">>} = _Evt, _CallId, _MsgId, _JObj, Call) ->
    cf_exe:continue(Call);
handle_msg({<<"callflow">>, <<"action.accepted">>}, {CallId, CallId}, {ActionId, ActionId}, JObj, Call) ->
    lager:debug("remote action accepted by ~s", [kz_api:node(JObj)]),
    wait_for_msg(ActionId, Call);
handle_msg({<<"callflow">>, <<"action.result">>}, {CallId, CallId}, {ActionId, ActionId}, JObj, Call) ->
    Result = binary_to_term(base64:decode(kz_json:get_binary_value(<<"Result">>, JObj))),
    lager:debug("remote action result => ~p", [Result]),
    handle_action_return(Result, Call);
handle_msg(_Evt, _CallId, {_, ActionId} = _MsgId, _JObj, Call) ->
    wait_for_msg(ActionId, Call).

-spec handle_action_return(atom() | {'continue', kz_term:ne_binary()}, kapps_call:call()) -> any().
handle_action_return({'continue', Key}, Call) ->
    cf_exe:continue(Key, Call);
handle_action_return('continue', Call) ->
    cf_exe:continue(Call);
handle_action_return({'continue_with_flow', Flow}, Call) ->
    cf_exe:continue_with_flow(Flow, Call);
handle_action_return({'stop', Reason}, Call) ->
    cf_exe:stop(Call, Reason);
handle_action_return('stop', Call) ->
    cf_exe:stop(Call);
handle_action_return('transfer', Call) ->
    cf_exe:transfer(Call);
handle_action_return('usurp', Call) ->
    cf_exe:control_usurped(Call);
handle_action_return(_Return, Call) ->
    lager:warning("unhandled return from remote action : ~p", [_Return]),
    cf_exe:continue(Call).
