%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles changing an agent's status
%%%
%%% "data":{
%%%   "action":["login","logout","pause","resume"] // one of these
%%%   ,"timeout":600 // in seconds, for "pause" status
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_agent).

-export([handle/2
         ,find_agent_status/2
        ]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case find_agent(Call) of
        {ok, AgentId} ->
            Status = find_agent_status(Call, AgentId),
            NewStatus = wh_json:get_value(<<"action">>, Data),
            lager:debug("agent ~s maybe changing status from ~s to ~s", [AgentId, Status, NewStatus]),

            maybe_update_status(Call, AgentId, Status, NewStatus, Data);
        {error, _E} ->
            lager:debug("agent was not found: ~p", [_E]),
            play_not_an_agent(Call)
    end,
    cf_exe:continue(Call).

-spec find_agent_status/2 :: (whapps_call:call() | ne_binary(), ne_binary()) -> ne_binary().
find_agent_status(?NE_BINARY = AcctDb, AgentId) ->
    Opts = [{endkey, [AgentId, 0]}
            ,{startkey, [AgentId, wh_json:new()]}
            ,{limit, 1}
            ,descending
           ],
    case couch_mgr:get_results(AcctDb, <<"agents/agent_status">>, Opts) of
        {ok, []} -> <<"logout">>;
        {error, _E} -> <<"logout">>;
        {ok, [StatusJObj|_]} -> maybe_fix_resume(wh_json:get_value(<<"value">>, StatusJObj))
    end;
find_agent_status(Call, AgentId) ->
    find_agent_status(whapps_call:account_db(Call), AgentId).

maybe_fix_resume(<<"resume">>) -> <<"login">>;
maybe_fix_resume(Status) -> Status.

maybe_update_status(Call, AgentId, _Curr, <<"logout">>, _Data) ->
    lager:debug("agent ~s wants to log out (currently: ~s)", [AgentId, _Curr]),
    logout_agent(Call, AgentId),
    play_agent_logged_out(Call);

maybe_update_status(Call, _AgentId, <<"login">>, <<"login">>, _Data) ->
    lager:debug("agent ~s is already logged in", [_AgentId]),
    play_agent_logged_in_already(Call);
maybe_update_status(Call, AgentId, <<"login">>, <<"pause">>, _Data) ->
    lager:debug("agent ~s is pausing work", [AgentId]),
    pause_agent(Call, AgentId),
    play_agent_pause(Call);

maybe_update_status(Call, AgentId, <<"logout">>, <<"login">>, _Data) ->
    lager:debug("agent ~s wants to log in", [AgentId]),
    login_agent(Call, AgentId),
    play_agent_logged_in(Call);

maybe_update_status(Call, AgentId, <<"pause">>, <<"resume">>, _Data) ->
    lager:debug("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"pause">>, <<"login">>, _Data) ->
    lager:debug("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId),
    play_agent_resume(Call);
maybe_update_status(Call, _AgentId, _Status, _NewStatus, _Data) ->
    lager:debug("agent ~s: invalid status change from ~s to ~s", [_AgentId, _Status, _NewStatus]),
    play_agent_invalid(Call).

login_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"login">>).

logout_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"logout">>).

pause_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"pause">>).

resume_agent(Call, AgentId) ->
    update_agent_status(Call, AgentId, <<"resume">>).

update_agent_status(Call, AgentId, Status) ->
    AcctDb = whapps_call:account_db(Call),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Status}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                            ]),
    {ok, _D} = couch_mgr:save_doc(AcctDb, wh_doc:update_pvt_parameters(Doc, AcctDb)),
    lager:debug("saved ~p", [_D]),
    send_new_status(Call, AgentId, Status),
    ok.

send_new_status(Call, AgentId, Status) ->
    Update = [{<<"Account-ID">>, whapps_call:account_id(Call)}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"New-Status">>, Status}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wapi_acdc_agent:publish_status_update(Update).

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
