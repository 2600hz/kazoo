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

-export([handle/2]).

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
            lager:debug("agent ~s maybe changing status from ~s to ~s", [Status, NewStatus]),

            maybe_update_status(Call, AgentId, Status, NewStatus, Data);
        {error, _E} ->
            lager:debug("agent was not found: ~p", [_E]),
            play_not_an_agent(Call)
    end,
    cf_exe:continue(Call).

find_agent_status(Call, AgentId) ->
    ok.

login_agent(Call, AgentId) ->
    ok.

maybe_update_status(Call, _AgentId, <<"login">>, <<"login">>, _Data) ->
    lager:debug("agent ~s is already logged in", [_AgentId]),
    play_agent_logged_in_already(Call);
maybe_update_status(Call, AgentId, <<"login">>, <<"logout">>, _Data) ->
    lager:debug("agent ~s can be logged in", [AgentId]),
    login_agent(Call, AgentId),
    play_agent_logged_in(Call);
maybe_update_status(Call, AgentId, Status, NewStatus, Data) ->
    ok.

find_agent(Call) ->
    case whapps_call:owner_id(Call) of
        undefined -> find_agent_by_authorization(Call
                                                 ,whapps_call:authorizing_id(Call)
                                                 ,whapps_call:authorizing_type(Call)
                                                );
        OwnerId -> OwnerId
    end.

find_agent_by_authorization(Call, DeviceId, <<"device">>) ->
    {ok, Device} = couch_mgr:open_doc(whapps_call:account_db(Call), DeviceId),
    wh_json:get_value(<<"owner_id">>, Device).

play_not_an_agent(Call) ->
    whapps_call_command:b_prompt(<<"agent-not_call_center_agent">>, Call).
play_agent_logged_in_already(Call) ->
    whapps_call_command:b_prompt(<<"agent-logged_already_in">>, Call).
play_agent_logged_in(Call) ->
    whapps_call_command:b_prompt(<<"agent-logged_in">>, Call).
