%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
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
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_agent).

-export([handle/2
        ,find_agent/1
        ,find_agent_status/2
        ,play_not_an_agent/1
        ,play_agent_invalid/1
        ]).

-include("acdc_config.hrl").
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
                Action = fix_data_action(kz_json:get_value(<<"action">>, Data)),
                lager:info("agent ~s maybe action ~s from status ~s", [AgentId, Action, Status]),
                maybe_update_status(Call, AgentId, Action, Status, Data);
            {'error', 'multiple_owners'} ->
                lager:info("too many owners of device ~s, not logging in", [kapps_call:authorizing_id(Call)]),
                play_agent_invalid(Call)
        end,
    lager:info("finished with acdc agent callflow"),
    cf_exe:continue(Call).

%%------------------------------------------------------------------------------
%% @doc Get a normalized current agent status value.
%% @end
%%------------------------------------------------------------------------------
-spec find_agent_status(kapps_call:call() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
find_agent_status(?NE_BINARY = AcctId, AgentId) ->
    fix_agent_status(acdc_agent_util:most_recent_status(AcctId, AgentId));
find_agent_status(Call, AgentId) ->
    find_agent_status(kapps_call:account_id(Call), AgentId).

%%------------------------------------------------------------------------------
%% @doc Normalizes agent status values.
%% @end
%%------------------------------------------------------------------------------
-spec fix_agent_status({'ok', kz_term:ne_binary()}) -> kz_term:ne_binary().
fix_agent_status({'ok', <<"resume">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"busy">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"logout">>}) -> <<"logged_out">>;
fix_agent_status({'ok', <<"login">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"outbound">>}) -> <<"ready">>;
fix_agent_status({'ok', Status}) -> Status.

%%------------------------------------------------------------------------------
%% @doc Normalizes action values.
%% @end
%%------------------------------------------------------------------------------
-spec fix_data_action(kz_term:ne_binary()) -> kz_term:ne_binary().
fix_data_action(<<"paused">>) -> <<"pause">>;
fix_data_action(Status) -> Status.

%%------------------------------------------------------------------------------
%% @doc Update an agent's status if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_status(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_update_status(Call, AgentId, <<"logout">>, _Status, Data) ->
    logout_agent(Call, AgentId, Data);
maybe_update_status(Call, AgentId, <<"login">>, Status, Data) ->
    maybe_login_agent(Call, AgentId, Status, Data);
maybe_update_status(Call, AgentId, <<"pause">>, Status, Data) ->
    maybe_pause_agent(Call, AgentId, Status, Data);
maybe_update_status(Call, AgentId, <<"ready">>, <<"paused">>, Data) ->
    maybe_update_status(Call, AgentId, <<"resume">>, <<"paused">>, Data);
maybe_update_status(Call, AgentId, <<"resume">>, Status, Data) ->
    maybe_resume_agent(Call, AgentId, Status, Data);
maybe_update_status(Call, AgentId, Action, _Status, _Data) ->
    lager:info("agent ~s: action ~s is invalid", [AgentId, Action]),
    play_agent_invalid(Call).

%%------------------------------------------------------------------------------
%% @doc Login an agent if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_login_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_login_agent(Call, AgentId, Status, Data) ->
    case lists:member(Status, [<<"logged_out">>, <<"unknown">>]) of
        'true' ->
            maybe_login_agent(Call, AgentId, Data);
        'false' ->
            lager:info("agent ~s is already logged in when status is ~s", [AgentId, Status]),
            send_new_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_login/1, 'undefined'),
            play_agent_logged_in_already(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Attempt to login an agent.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_login_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_login_agent(Call, AgentId, Data) ->
    case login_agent(Call, AgentId, Data) of
        <<"success">> -> play_agent_logged_in(Call);
        <<"failed">> -> play_agent_invalid(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Pause an agent if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_pause_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_pause_agent(Call, AgentId, Status, Data) ->
    case lists:member(Status, [<<"ready">>, <<"wrapup">>]) of
        'true' ->
            pause_agent(Call, AgentId, Data);
        'false' ->
            lager:info("agent ~s cannot pause when status is ~s", [AgentId, Status]),
            play_agent_invalid(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Resume an agent if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_resume_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_resume_agent(Call, AgentId, Status, Data) ->
    case lists:member(Status, [<<"paused">>, <<"outbound">>, <<"ready">>, <<"wrapup">>]) of
        'true' ->
            resume_agent(Call, AgentId, Data);
        'false' ->
            lager:info("agent ~s cannot resume when status is ~s", [AgentId, Status]),
            play_agent_invalid(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `login' message.
%% @end
%%------------------------------------------------------------------------------
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
            lager:info("agent ~s is logging in", [AgentId]),
            kz_json:get_value(<<"Status">>, RespJObj);
        {'error', _E} ->
            lager:debug("failed to hear back about login: ~p", [_E]),
            <<"failed">>
    end.

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `logout' message.
%% @end
%%------------------------------------------------------------------------------
-spec logout_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
logout_agent(Call, AgentId, Data) ->
    lager:info("agent ~s is logging out", [AgentId]),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_logout/1),
    play_agent_logged_out(Call).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `pause' message.
%% @end
%%------------------------------------------------------------------------------
-spec pause_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_term:api_integer()) ->
          kapps_call:kapps_api_std_return().
pause_agent(Call, AgentId, Data, Timeout) ->
    lager:info("agent ~s is pausing work for ~p s", [AgentId, Timeout]),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_pause/1, Timeout),
    play_agent_pause(Call).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `pause' message.
%% @end
%%------------------------------------------------------------------------------
-spec pause_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
pause_agent(Call, AgentId, Data) ->
    Timeout = kz_json:get_value(<<"timeout">>, Data, ?DEFAULT_AGENT_PAUSE_TIMEOUT),
    pause_agent(Call, AgentId, Data, Timeout).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `resume' message.
%% @end
%%------------------------------------------------------------------------------
-spec resume_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
resume_agent(Call, AgentId, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_resume/1),
    play_agent_resume(Call).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent status-change message.
%% @end
%%------------------------------------------------------------------------------
-spec update_agent_status(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_amqp_worker:publish_fun()) -> 'ok'.
update_agent_status(Call, AgentId, Data, PubFun) ->
    update_agent_status(Call, AgentId, Data, PubFun, 'undefined').
update_agent_status(Call, AgentId, Data, PubFun, Timeout) ->
    send_new_status(Call, AgentId, Data, PubFun, Timeout).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent status-change message.
%% @end
%%------------------------------------------------------------------------------
-spec send_new_status(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_amqp_worker:publish_fun(), kz_term:api_integer() | kz_term:ne_binary()) -> 'ok'.
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
