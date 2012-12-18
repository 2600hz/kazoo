%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles changing an agent's status
%%%
%%% "data":{
%%%   "action":["login","logout","paused","resume"] // one of these
%%%   ,"timeout":600 // in seconds, for "paused" status
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_agent).

-export([handle/2
         ,find_agent/1
         ,play_not_an_agent/1
         ,play_agent_invalid/1
        ]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),
    _ = case find_agent(Call) of
            {ok, undefined} ->
                lager:debug("no owner on this device == no agent"),
                play_not_an_agent(Call);
            {ok, AgentId} ->
                Status = find_agent_status(Call, AgentId),
                NewStatus = fix_data_status(wh_json:get_value(<<"action">>, Data)),
                lager:debug("agent ~s maybe changing status from ~s to ~s", [AgentId, Status, NewStatus]),

                maybe_update_status(Call, AgentId, Status, NewStatus, Data)
        end,
    cf_exe:continue(Call).

-spec find_agent_status/2 :: (whapps_call:call() | ne_binary(), ne_binary()) -> ne_binary().
find_agent_status(?NE_BINARY = AcctId, AgentId) ->
    fix_agent_status(acdc_util:agent_status(AcctId, AgentId));
find_agent_status(Call, AgentId) ->
    find_agent_status(whapps_call:account_id(Call), AgentId).

fix_agent_status(<<"resume">>) -> <<"login">>;
fix_agent_status(<<"ready">>) -> <<"login">>;
fix_agent_status(<<"wrapup">>) -> <<"login">>;
fix_agent_status(Status) -> Status.

fix_data_status(<<"pause">>) -> <<"paused">>;
fix_data_status(Status) -> Status.

maybe_update_status(Call, AgentId, _Curr, <<"logout">>, _Data) ->
    lager:debug("agent ~s wants to log out (currently: ~s)", [AgentId, _Curr]),
    logout_agent(Call, AgentId),
    play_agent_logged_out(Call);

maybe_update_status(Call, AgentId, <<"login">>, <<"login">>, _Data) ->
    lager:debug("agent ~s is already logged in", [AgentId]),
    _ = play_agent_logged_in_already(Call),
    send_new_status(Call, AgentId, fun wapi_acdc_agent:publish_login/1, undefined);
maybe_update_status(Call, AgentId, <<"login">>, <<"paused">>, Data) ->
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, whapps_config:get(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)),
    lager:debug("agent ~s is pausing work for ~b s", [AgentId, Timeout]),
    pause_agent(Call, AgentId, Timeout),
    play_agent_pause(Call);

maybe_update_status(Call, AgentId, <<"logout">>, <<"login">>, _Data) ->
    lager:debug("agent ~s wants to log in", [AgentId]),
    login_agent(Call, AgentId),
    play_agent_logged_in(Call);

maybe_update_status(Call, AgentId, <<"paused">>, <<"resume">>, _Data) ->
    lager:debug("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"paused">>, <<"login">>, _Data) ->
    lager:debug("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId),
    play_agent_resume(Call);
maybe_update_status(Call, _AgentId, _Status, _NewStatus, _Data) ->
    lager:debug("agent ~s: invalid status change from ~s to ~s", [_AgentId, _Status, _NewStatus]),
    play_agent_invalid(Call).

login_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"login">>, fun wapi_acdc_agent:publish_login/1).

logout_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"logout">>, fun wapi_acdc_agent:publish_logout/1).

pause_agent(Call, AgentId, Timeout) ->
    update_agent_status(Call, AgentId, <<"paused">>, fun wapi_acdc_agent:publish_pause/1, Timeout).

resume_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"resume">>, fun wapi_acdc_agent:publish_resume/1).

update_agent_status(Call, AgentId, Status, PubFun) ->
    update_agent_status(Call, AgentId, Status, PubFun, undefined).
update_agent_status(Call, AgentId, Status, PubFun, Timeout) ->
    AcctId = whapps_call:account_id(Call),

    Extra = [{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"method">>, <<"callflow">>}
             ,{<<"wait_time">>, Timeout}
            ],

    {ok, _D} = acdc_util:update_agent_status(AcctId, AgentId, Status, Extra),
    send_new_status(Call, AgentId, PubFun, Timeout).

-spec send_new_status/4 :: (whapps_call:call(), ne_binary(), wh_amqp_worker:publish_fun(), integer() | 'undefined') -> 'ok'.
send_new_status(Call, AgentId, PubFun, Timeout) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, whapps_call:account_id(Call)}
                ,{<<"Agent-ID">>, AgentId}
                ,{<<"Time-Limit">>, Timeout}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

find_agent(Call) ->
    case whapps_call:owner_id(Call) of
        undefined -> find_agent_by_authorization(Call
                                                 ,whapps_call:authorizing_id(Call)
                                                 ,whapps_call:authorizing_type(Call)
                                                );
        OwnerId -> {ok, OwnerId}
    end.

find_agent_by_authorization(Call, DeviceId, <<"device">>) ->
    {ok, Device} = couch_mgr:open_doc(whapps_call:account_db(Call), DeviceId),
    {ok, wh_json:get_value(<<"owner_id">>, Device)}.

play_not_an_agent(Call) ->
    whapps_call_command:b_prompt(<<"agent-not_call_center_agent">>, Call).
play_agent_logged_in_already(Call) ->
    whapps_call_command:b_prompt(<<"agent-already_logged_in">>, Call).
play_agent_logged_in(Call) ->
    whapps_call_command:b_prompt(<<"agent-logged_in">>, Call).
play_agent_logged_out(Call) ->
    whapps_call_command:b_prompt(<<"agent-logged_out">>, Call).
play_agent_resume(Call) ->
    whapps_call_command:b_prompt(<<"agent-resume">>, Call).
play_agent_pause(Call) ->
    whapps_call_command:b_prompt(<<"agent-pause">>, Call).
play_agent_invalid(Call) ->
    whapps_call_command:b_prompt(<<"agent-invalid_choice">>, Call).
