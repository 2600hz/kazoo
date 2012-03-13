%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_agent).

-export([handle/2]).

-include("../callflow.hrl").

-define(PROMPT_PIN, <<"/system_media/hotdesk-enter_pin">>).
-define(PROMPT_INVALID_PIN, <<"/system_media/conf-bad_pin">>).
-define(PROMPT_RETRIES_EXCEEDED, <<"/system_media/conf-to_many_attempts">>).
-define(PROMPT_LOGGED_IN, <<"/system_media/hotdesk-logged_in">>).
-define(PROMPT_LOGGED_OUT, <<"/system_media/hotdesk-logged_out">>).

-define(TIMEOUT_DTMF, 2000).

-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    Retries = wh_json:get_integer_value(<<"retries">>, Data, 3),

    Action = wh_json:get_value(<<"action">>, Data, <<"toggle">>),

    lager:debug("performing ~s on agent", [Action]),

    case Action of
        <<"login">> -> login_agent(Call, Retries);
        <<"logout">> -> logout_agent(Call, Retries);
        <<"break">> -> break_agent(Call, Retries);
        <<"resume">> -> resume_agent(Call, Retries);
        _ -> toggle_agent(Call, Retries)
    end.

toggle_agent(Call, Retries) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
toggle_agent(Call, Retries) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, Pin} ->
            case find_agent_by_pin(Call, Pin) of
                {ok, AgentId} ->
                    lager:debug("found agent ~s, toggling", [AgentId]),
                    toggle_agent_status(Call, AgentId);
                {error, _} ->
                    lager:debug("failed to find agent by pin: ~s", [Pin]),
                    whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
                    toggle_agent(Call, Retries-1)
            end;
        {error, _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            toggle_agent(Call, Retries-1)
    end.

login_agent(Call, Retries) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
login_agent(Call, Retries) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, Pin} ->
            case find_agent_by_pin(Call, Pin) of
                {ok, AgentId} ->
                    log_agent_activity(Call, <<"login">>, AgentId);
                {error, _} ->
                    lager:debug("failed to find agent by pin: ~s", [Pin]),
                    whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
                    login_agent(Call, Retries-1)
            end;
        {error, _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            login_agent(Call, Retries-1)
    end.

logout_agent(Call, Retries) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
logout_agent(Call, Retries) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, Pin} ->
            case find_agent_by_pin(Call, Pin) of
                {ok, AgentId} ->
                    log_agent_activity(Call, <<"logout">>, AgentId);
                {error, _} ->
                    lager:debug("failed to find agent by pin: ~s", [Pin]),
                    whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
                    logout_agent(Call, Retries-1)
            end;
        {error, _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            logout_agent(Call, Retries-1)
    end.

break_agent(Call, Retries) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
break_agent(Call, Retries) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, Pin} ->
            case find_agent_by_pin(Call, Pin) of
                {ok, AgentId} ->
                    log_agent_activity(Call, <<"break">>, AgentId);
                {error, _} ->
                    lager:debug("failed to find agent by pin: ~s", [Pin]),
                    whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
                    break_agent(Call, Retries-1)
            end;
        {error, _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            break_agent(Call, Retries-1)
    end.

resume_agent(Call, Retries) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
resume_agent(Call, Retries) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, Pin} ->
            case find_agent_by_pin(Call, Pin) of
                {ok, AgentId} ->
                    log_agent_activity(Call, <<"resume">>, AgentId);
                {error, _} ->
                    lager:debug("failed to find agent by pin: ~s", [Pin]),
                    whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
                    resume_agent(Call, Retries-1)
            end;
        {error, _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            resume_agent(Call, Retries-1)
    end.

toggle_agent_status(Call, AgentId) ->
    case couch_mgr:get_results(whapps_call:account_db(Call), <<"agents/agent_status">>, [{<<"startkey">>, [wh_json:new(), AgentId]}
                                                                                         ,{<<"endkey">>, [0, AgentId]}
                                                                                         ,{<<"descending">>, true}
                                                                                         ,{<<"limit">>, 1}
                                                                                         ]) of
        {ok, []} ->
            lager:debug("no status available, logging in"),
            log_agent_activity(Call, <<"login">>, AgentId),
            cf_exe:stop(Call);
        {ok, [Status]} ->
            lager:debug("current status: ~s", [Status]),
            log_agent_activity(Call, opposite(Status), AgentId),
            cf_exe:stop(Call);
        {error, _E} ->
            lager:debug("error looking up status: ~p", [_E]),
            cf_exe:stop(Call)
    end.

opposite(<<"login">>) ->
    <<"logout">>;
opposite(<<"logout">>) ->
    <<"login">>;
opposite(<<"resume">>) ->
    <<"tmpaway">>;
opposite(<<"tmpaway">>) ->
    <<"resume">>.

play_and_collect(Call, Prompt) ->
    NoopID = whapps_call_command:audio_macro([{play, Prompt}], Call),
    {ok, Bin} = whapps_call_command:collect_digits(1, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call),
    collect(Call, Bin).
collect(Call, DTMFs) ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            lager:debug("failed to collect more digits, returning"),
            {ok, DTMFs};
        {ok, DTMF} ->
            lager:debug("another dtmf: ~s", [DTMF]),
            collect(Call, <<DTMFs/binary, DTMF/binary>>)
    end.

find_agent_by_pin(Call, Pin) ->
    case couch_mgr:get_results(whapps_call:account_db(Call), <<"agents/agent_pins">>, [{<<"key">>, wh_util:to_integer(Pin)}]) of
        {ok, []} ->
            {error, no_agents_found};
        {ok, [AgentJObj]} ->
            {ok, wh_json:get_value(<<"id">>, AgentJObj)};
        {ok, _Agents} ->
            lager:debug("more than one agent with pin ~s", [Pin]),
            {error, duplicate_pins_found};
        {error, _E}=E ->
            lager:debug("error looking up pins: ~p", [_E]),
            E
    end.

log_agent_activity(Call, Action, AgentId) ->
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Action}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                            ]),
    couch_mgr:save_doc(whapps_call:account_db(Call), Doc).
