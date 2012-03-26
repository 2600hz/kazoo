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
-define(PROMPT_BREAK, <<"/system_media/temporal-marked_disabled">>).
-define(PROMPT_RESUME, <<"/system_media/temporal-marked_enabled">>).
-define(PROMPT_ERROR, <<"/system_media/menu-invalid_entry">>).

-define(TIMEOUT_DTMF, 2000).

-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    Retries = wh_json:get_integer_value(<<"retries">>, Data, 3),

    Action = wh_json:get_value(<<"action">>, Data, <<"toggle">>),

    OwnerId = whapps_call:kvs_fetch(owner_id, Call),
    {ok, Owner} = couch_mgr:open_doc(whapps_call:account_db(Call), OwnerId),

    lager:debug("performing ~s on agent ~s", [Action, OwnerId]),

    case Action of
        <<"login">> -> update_agent(Call, Retries, Action, Owner);
        <<"logout">> -> update_agent(Call, Retries, Action, Owner);
        <<"break">> -> update_agent(Call, Retries, Action, Owner);
        <<"resume">> -> update_agent(Call, Retries, Action, Owner);
        _ -> toggle_agent(Call, Retries, Owner)
    end.

update_agent(Call, Retries, Action, Owner) ->
    update_agent(Call, Retries, Action
                 ,wh_json:get_value(<<"_id">>, Owner)
                 ,wh_json:get_binary_value(<<"queue_pin">>, Owner)
                ).

update_agent(Call, _, _, _, undefined) ->
    lager:debug("no pin set on agent's doc, not an agent"),
    cf_exe:continue(Call);
update_agent(Call, _, Action, Id, <<>>) ->
    lager:debug("agent's pin is empty, performing action ~s", [Action]),
    update_agent_status(Call, Id, Action);
update_agent(Call, Retries, _, _, _)  when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
update_agent(Call, Retries, Action, AgentId, AgentPin) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, AgentPin} ->
            lager:debug("matched pin to ~s", [AgentPin]),
            update_agent_status(Call, AgentId, Action);
        {ok, Pin} ->
            lager:debug("pin ~s doesn't match ~s", [Pin, AgentPin]),
            update_agent(Call, Retries-1, Action, AgentId, AgentPin)
    end.

update_agent_status(Call, AgentId, Action) ->
    Current = case current_status(Call, AgentId) of
                  undefined -> <<"logout">>;
                  error ->
                      lager:debug("error finding agent status for ~s", [AgentId]),
                      play_action(Call, ok),
                      cf_exe:stop(Call);
                  C -> C
              end,
    Transitions = transitions(Current),
    case lists:member(Action, Transitions) of
        true ->
            lager:debug("action ~s is a valid transition of current state(~s), updating", [Action, Current]),
            log_agent_activity(Call, Action, AgentId),
            cf_exe:stop(Call);
        false ->
            lager:debug("action ~s not a valid transition from ~s", [Action, Current]),
            play_action(Call, ok),
            cf_exe:stop(Call)
    end.

toggle_agent(Call, Retries, _) when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
toggle_agent(Call, Retries, Owner) ->
    toggle_agent(Call, Retries, wh_json:get_value(<<"_id">>, Owner), wh_json:get_value(<<"queue_pin">>, Owner)).

toggle_agent(Call, _, _, undefined) ->
    lager:debug("caller is not an agent (no queue_pin defined)"),
    cf_exe:continue(Call);
toggle_agent(Call, _, AgentId, <<>>) ->
    lager:debug("agent has no pin, performing toggle"),
    toggle_agent_status(Call, AgentId);
toggle_agent(Call, Retries, _, _)  when Retries =< 0 ->
    whapps_call_command:b_play(?PROMPT_RETRIES_EXCEEDED, Call),
    cf_exe:stop(Call);
toggle_agent(Call, Retries, AgentId, AgentPin) ->
    case play_and_collect(Call, ?PROMPT_PIN) of
        {ok, AgentPin} ->
            lager:debug("found agent, toggling"),
            toggle_agent_status(Call, AgentId);
        {ok, Pin} ->
            lager:debug("pin ~s doesn't match ~s", [Pin, AgentPin]),
            whapps_call_command:b_play(?PROMPT_INVALID_PIN, Call),
            toggle_agent(Call, Retries-1, AgentId, AgentPin)
    end.

toggle_agent_status(Call, AgentId) ->
    case current_status(Call, AgentId) of
        undefined ->
            lager:debug("no status available, logging in"),
            log_agent_activity(Call, <<"login">>, AgentId),
            cf_exe:stop(Call);
        error ->
            lager:debug("error looking up status"),
            play_action(Call, ok),
            cf_exe:stop(Call);
        Status ->
            [Opp|_] = transitions(Status),
            lager:debug("current status: ~s transitioning to ~s", [Status, Opp]),
            log_agent_activity(Call, Opp, AgentId),
            cf_exe:stop(Call)
    end.

%% transition in first position is the preferred, for toggling
transitions(<<"login">>) ->
    [<<"logout">>, <<"break">>];
transitions(<<"logout">>) ->
    [<<"login">>];
transitions(<<"resume">>) ->
    [<<"break">>, <<"logout">>];
transitions(<<"break">>) ->
    [<<"resume">>, <<"logout">>];
transitions(_) ->
    [<<"logout">>].

-spec play_and_collect/2 :: (whapps_call:call(), ne_binary()) -> {'ok', binary()}.
play_and_collect(Call, Prompt) ->
    NoopID = whapps_call_command:audio_macro([{play, Prompt}], Call),
    {ok, Bin} = whapps_call_command:collect_digits(1, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call),
    collect(Call, Bin).

-spec collect/2 :: (whapps_call:call(), binary()) -> {'ok', binary()}.
collect(Call, DTMFs) ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            lager:debug("failed to collect more digits, returning"),
            {ok, DTMFs};
        {ok, DTMF} ->
            lager:debug("another dtmf: ~s", [DTMF]),
            collect(Call, <<DTMFs/binary, DTMF/binary>>)
    end.

current_status(Call, AgentId) ->
    case couch_mgr:get_results(whapps_call:account_db(Call), <<"agents/agent_status">>, [{<<"startkey">>, [AgentId, wh_json:new()]}
                                                                                         ,{<<"endkey">>, [AgentId, 0]}
                                                                                         ,{<<"descending">>, true}
                                                                                         ,{<<"limit">>, 1}
                                                                                         ,{<<"reduce">>, false}
                                                                                         ,{<<"include_docs">>, true}
                                                                                        ]) of
        {ok, []} ->
            undefined;
        {ok, [StatusJObj|_]} ->
            wh_json:get_value([<<"doc">>, <<"action">>], StatusJObj);
        {error, _E} ->
            error
    end.    

log_agent_activity(Call, Action, AgentId) ->
    lager:debug("setting action for agent ~s to ~s", [AgentId, Action]),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Action}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                            ]),
    {ok, _} = couch_mgr:save_doc(whapps_call:account_db(Call), Doc),
    play_action(Call, Action).

play_action(Call, <<"login">>) ->
    whapps_call_command:b_play(?PROMPT_LOGGED_IN, Call);
play_action(Call, <<"logout">>) ->
    whapps_call_command:b_play(?PROMPT_LOGGED_OUT, Call);
play_action(Call, <<"break">>) ->
    whapps_call_command:b_play(?PROMPT_BREAK, Call);
play_action(Call, <<"resume">>) ->
    whapps_call_command:b_play(?PROMPT_RESUME, Call);
play_action(Call, _) ->
    whapps_call_command:b_play(?PROMPT_ERROR, Call).

