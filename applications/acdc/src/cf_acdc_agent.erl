%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handles changing an agent's status
%%% "data":{
%%%   "action":["login","logout","paused","resume"] // one of these
%%%   ,"timeout":600 // in seconds, for "paused" status
%%%   ,"presence_id":"abc123" // id of the button
%%%   ,"presence_state":["early", "confirmed","terminated"
%%%                      ,"red_flash", "red_solid", "green"
%%%                     ]
%%% }
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_agent).

-export([handle/2
        ,find_agent/1
        ,find_agent_status/2
        ,play_not_an_agent/1
        ,play_agent_invalid/1
        ,login_agent/2
        ,logout_agent/2
        ]).

-include_lib("callflow/src/callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),
    _ = case find_agent(Call) of
            {'ok', 'undefined'} ->
                lager:info("no owner on this device == no agent"),
                play_not_an_agent(Call);
            {'ok', AgentId} ->
                Status = find_agent_status(Call, AgentId),
                NewStatus = fix_data_status(kz_json:get_value(<<"action">>, Data)),
                lager:info("agent ~s maybe changing status from ~s to ~s", [AgentId, Status, NewStatus]),

                maybe_update_status(Call, AgentId, Status, NewStatus, Data);
            {'error', 'multiple_owners'} ->
                lager:info("too many owners of device ~s, not logging in", [kapps_call:authorizing_id(Call)]),
                play_agent_invalid(Call)
        end,
    lager:info("finished with acdc agent callflow"),
    cf_exe:continue(Call).

-spec find_agent_status(kapps_call:call() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
find_agent_status(?NE_BINARY = AcctId, AgentId) ->
    fix_agent_status(acdc_agent_util:most_recent_status(AcctId, AgentId));
find_agent_status(Call, AgentId) ->
    find_agent_status(kapps_call:account_id(Call), AgentId).

fix_agent_status({'ok', <<"resume">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"wrapup">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"busy">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"logout">>}) -> <<"logged_out">>;
fix_agent_status({'ok', <<"login">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"outbound">>}) -> <<"ready">>;
fix_agent_status({'ok', Status}) -> Status.

fix_data_status(<<"pause">>) -> <<"paused">>;
fix_data_status(Status) -> Status.

maybe_update_status(Call, AgentId, _Curr, <<"logout">>, Data) ->
    lager:info("agent ~s wants to log out (currently: ~s)", [AgentId, _Curr]),
    logout_agent(Call, AgentId, Data),
    play_agent_logged_out(Call);
maybe_update_status(Call, AgentId, <<"logged_out">>, <<"resume">>, _Data) ->
    lager:debug("agent ~s is logged out, resuming doesn't make sense", [AgentId]),
    play_agent_invalid(Call);
maybe_update_status(Call, AgentId, <<"logged_out">>, <<"login">>, Data) ->
    maybe_login_agent(Call, AgentId, Data);
maybe_update_status(Call, AgentId, <<"unknown">>, <<"login">>, Data) ->
    maybe_login_agent(Call, AgentId, Data);
maybe_update_status(Call, AgentId, <<"ready">>, <<"login">>, Data) ->
    lager:info("agent ~s is already logged in", [AgentId]),
    _ = play_agent_logged_in_already(Call),
    send_new_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_login/1, 'undefined');
maybe_update_status(Call, AgentId, FromStatus, <<"paused">>, Data) ->
    maybe_pause_agent(Call, AgentId, FromStatus, Data);
maybe_update_status(Call, AgentId, <<"paused">>, <<"ready">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"paused">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"outbound">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"ready">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, _AgentId, _Status, _NewStatus, _Data) ->
    lager:info("agent ~s: invalid status change from ~s to ~s", [_AgentId, _Status, _NewStatus]),
    play_agent_invalid(Call).

maybe_login_agent(Call, AgentId, Data) ->
    lager:debug("agent ~s wants to log in", [AgentId]),
    case login_agent(Call, AgentId, Data) of
        <<"success">> -> play_agent_logged_in(Call);
        <<"failed">> -> play_agent_invalid(Call)
    end.

maybe_pause_agent(Call, AgentId, <<"ready">>, Data) ->
    pause_agent(Call, AgentId, Data);
maybe_pause_agent(Call, _AgentId, FromStatus, _Data) ->
    lager:info("unable to go from ~s to paused", [FromStatus]),
    play_agent_invalid(Call).

-spec login_agent(kapps_call:call(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
login_agent(Call, AgentId) ->
    login_agent(Call, AgentId, kz_json:new()).

-spec login_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_ne_binary().
login_agent(Call, AgentId, Data) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, kapps_call:account_id(Call)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Presence-ID">>, presence_id(Data)}
               ,{<<"Presence-State">>, presence_state(Data)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    case kz_amqp_worker:call(Update
                            ,fun kapi_acdc_agent:publish_login/1
                            ,fun kapi_acdc_agent:login_resp_v/1
                            )
    of
        {'ok', RespJObj} ->
            kz_json:get_value(<<"Status">>, RespJObj);
        {'error', _E} ->
            lager:debug("failed to hear back about login: ~p", [_E]),
            <<"failed">>
    end.

-spec logout_agent(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
logout_agent(Call, AgentId) ->
    logout_agent(Call, AgentId, kz_json:new()).

-spec logout_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
logout_agent(Call, AgentId, Data) ->
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_logout/1).

pause_agent(Call, AgentId, Data, Timeout) when is_integer(Timeout) ->
    _ = play_agent_pause(Call),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_pause/1, Timeout).
pause_agent(Call, AgentId, Data) ->
    Timeout = kz_json:get_integer_value(<<"timeout">>
                                       ,Data
                                       ,kapps_config:get_integer(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)
                                       ),
    lager:info("agent ~s is pausing work for ~b s", [AgentId, Timeout]),
    pause_agent(Call, AgentId, Data, Timeout).

resume_agent(Call, AgentId, Data) ->
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_resume/1).

update_agent_status(Call, AgentId, Data, PubFun) ->
    update_agent_status(Call, AgentId, Data, PubFun, 'undefined').
update_agent_status(Call, AgentId, Data, PubFun, Timeout) ->
    send_new_status(Call, AgentId, Data, PubFun, Timeout).

-spec send_new_status(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_amqp_worker:publish_fun(), kz_term:api_integer()) -> 'ok'.
send_new_status(Call, AgentId, Data, PubFun, Timeout) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, kapps_call:account_id(Call)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Time-Limit">>, Timeout}
               ,{<<"Presence-ID">>, presence_id(Data)}
               ,{<<"Presence-State">>, presence_state(Data)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

-spec presence_id(kz_json:object()) -> kz_term:api_ne_binary().
presence_id(Data) -> kz_json:get_ne_binary_value(<<"presence_id">>, Data).

-spec presence_state(kz_json:object()) -> kz_term:api_ne_binary().
presence_state(Data) ->
    format_presence_state(kz_json:get_ne_binary_value(<<"presence_state">>, Data)).

format_presence_state(<<"green">>) -> <<"terminated">>;
format_presence_state(<<"terminated">> = T) -> T;
format_presence_state(<<"red_flash">>) -> <<"early">>;
format_presence_state(<<"early">> = E) -> E;
format_presence_state(<<"red_solid">>) -> <<"confirmed">>;
format_presence_state(<<"confirmed">> = C) -> C;
format_presence_state(_) -> 'undefined'.

-type find_agent_error() :: 'unknown_endpoint' | 'multiple_owners'.
-spec find_agent(kapps_call:call()) ->
          {'ok', kz_term:api_binary()} |
          {'error', find_agent_error()}.
find_agent(Call) ->
    find_agent(Call, kapps_call:authorizing_id(Call)).

find_agent(_Call, 'undefined') ->
    {'error', 'unknown_endpoint'};
find_agent(Call, EndpointId) ->
    {'ok', Endpoint} = kz_datamgr:open_doc(kapps_call:account_db(Call), EndpointId),
    find_agent(Call, Endpoint, kz_json:get_value([<<"hotdesk">>, <<"users">>], Endpoint)).

find_agent(Call, Endpoint, 'undefined') ->
    find_agent_owner(Call, kz_json:get_value(<<"owner_id">>, Endpoint));
find_agent(Call, Endpoint, Owners) ->
    case kz_json:get_keys(Owners) of
        [] -> find_agent_owner(Call, kz_json:get_value(<<"owner_id">>, Endpoint));
        [OwnerId] -> {'ok', OwnerId};
        _ -> {'error', 'multiple_owners'}
    end.

find_agent_owner(Call, 'undefined') -> {'ok', kapps_call:owner_id(Call)};
find_agent_owner(_Call, EPOwnerId) -> {'ok', EPOwnerId}.


-spec play_not_an_agent(kapps_call:call()) -> kapps_call:kapps_api_std_return().
play_not_an_agent(Call) -> kapps_call_command:b_prompt(<<"agent-not_call_center_agent">>, Call).
play_agent_logged_in_already(Call) -> kapps_call_command:b_prompt(<<"agent-already_logged_in">>, Call).
play_agent_logged_in(Call) -> kapps_call_command:b_prompt(<<"agent-logged_in">>, Call).
play_agent_logged_out(Call) -> kapps_call_command:b_prompt(<<"agent-logged_out">>, Call).
play_agent_resume(Call) -> kapps_call_command:b_prompt(<<"agent-resume">>, Call).
play_agent_pause(Call) -> kapps_call_command:b_prompt(<<"agent-pause">>, Call).

-spec play_agent_invalid(kapps_call:call()) -> kapps_call:kapps_api_std_return().
play_agent_invalid(Call) -> kapps_call_command:b_prompt(<<"agent-invalid_choice">>, Call).
