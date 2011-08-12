%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Responsible for runnning the basic call task
%%% @end
%%% Created : 2 Dec 2010 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(monitor_call_basic).

-include("monitor_amqp.hrl").

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start/3]).

-define(SERVER, ?MODULE).
-define(FREQ, <<"2600">>).

-define(APP_NAME, <<"monitor_call_basic">>).
-define(APP_VERSION, <<"0.8">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start(AHost, Msg, Route) ->
    {ok, TaskQ} = create_task_q(AHost),
    originate_call(AHost, TaskQ, Route, get_value(<<"Msg-ID">>, Msg)),
    case wait_for_resource(5000) of 
        {error, E} ->
            {error, [{<<"Error">>, E}, {<<"Success">>, <<"false">>}]};
        {ok, Resource} ->
            CtrlQ = get_value(<<"Control-Queue">>, Resource),
            CallId = get_value(<<"Call-ID">>, Resource),
            amqp_util:bind_q_to_callevt(AHost, TaskQ, CallId),
            try 
                {ok, _} = wait_for_answer(20000),
                arm_tone_detector(AHost, CtrlQ, CallId),
                generate_tones(AHost, CtrlQ, CallId),    
                determine_results()
            catch
                _:{_, {error, Reason} } ->
                    {error, [{<<"Error">>, Reason}, {<<"Success">>, <<"false">>}]};
                _:_ ->
                    {error, [{<<"Error">>, <<"undefined">>}, {<<"Success">>, <<"false">>}]}
            after
                hangup_call(AHost, CtrlQ, CallId)
            end                
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_task_q(AHost) ->
    Q = amqp_util:new_monitor_queue(AHost),
    amqp_util:bind_q_to_targeted(AHost, Q),
    amqp_util:basic_consume(AHost, Q),
    {ok, Q}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
originate_call(AHost, ServerID, Route, MsgId) ->
    format_log(info, "MONITOR_CALL_BASIC(~p): Originate call to ~p", [self(), Route]),
    Command = [
                {<<"Msg-ID">>, MsgId}
               ,{<<"Resource-Type">>, <<"audio">>}
               ,{<<"Invite-Format">>, <<"route">>}
               ,{<<"Caller-ID-Name">>, <<"2600hz Monitoring">>}
               ,{<<"Caller-ID-Number">>, <<"4158867900">>}
               ,{<<"Route">>, Route}           
               | wh_api:default_headers(ServerID, <<"originate">>, <<"resource_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = wh_api:resource_req(Command),
    amqp_util:callmgr_publish(AHost, Json, <<"application/json">>, ?KEY_RESOURCE_REQ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
wait_for_resource(Timeout) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg) } of
                { <<"originate">>, <<"resource_resp">> } ->
                    {ok, Msg};
                { <<"originate">>, <<"originate_error">> } ->
                    Error = get_value(<<"Failure-Message">>, Msg),
                    {error, Error};
                { <<"originate">>, <<"resource_error">> } ->
                    {error, resources_unavaliable};
                _ ->
                    wait_for_resource(Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
wait_for_answer(Timeout) ->
    case wait_for_call_event(<<"CALL_UPDATE">>, undefined, Timeout) of
        {ok, Msg} ->
            try
                <<"ACTIVE">> = get_value(<<"Channel-Call-State">>, Msg),
                {ok, Msg}
            catch
                _:_ ->
                    wait_for_answer(Timeout)                             
            end;
        Else ->
            Else                
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
arm_tone_detector(AHost, CtrlQ, CallId) ->
    Command = [
                {<<"Call-ID">>, CallId}
               ,{<<"Application-Name">>, <<"tone_detect">>}
               ,{<<"Tone-Detect-Name">>, CallId}
               ,{<<"Frequencies">>, [?FREQ]}
               ,{<<"Sniff-Direction">>, <<"read">>}
               ,{<<"Timeout">>, <<"0">>}
               | wh_api:default_headers(CallId, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = wh_api:tone_detect_req(Command),
    send_callctrl(Json, AHost, CtrlQ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
generate_tones(AHost, CtrlQ, CallId) ->
    Command = [
                {<<"Call-ID">>, CallId}
               ,{<<"Application-Name">>, <<"tones">>}
               ,{<<"Tones">>, [
                               {struct, [
                                          {<<"Frequencies">>, [?FREQ]}
                                         ,{<<"Duration-ON">>, <<"5000">>}
                                         ,{<<"Duration-OFF">>, <<"10">>}
                                        ]}
                          ]
                }
               | wh_api:default_headers(CallId, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = wh_api:tones_req(Command),
    send_callctrl(Json, AHost, CtrlQ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
determine_results() ->
    try
        {ok, StartMsg} = wait_for_call_event(<<"CHANNEL_EXECUTE">>, <<"play">>, 5000),
        {ok, EndMsg} = wait_for_call_event(<<"CHANNEL_EXECUTE">>, <<"park">>, 5000),
        Delay = wh_util:to_integer(get_value(<<"Timestamp">>, EndMsg)) 
            - wh_util:to_integer(get_value(<<"Timestamp">>, StartMsg)),
        {ok, [{<<"Delay">>, Delay}, {<<"Success">>, <<"true">>}]}
    catch
        _:{_, {error, E}} ->
            {error, [{<<"Error">>, E}, {<<"Success">>, <<"false">>}]};
        _:_ ->
            {error, [{<<"Error">>, <<"undefined">>}, {<<"Success">>, <<"false">>}]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
hangup_call(AHost, CtrlQ, CallId) ->
    Command = [
                {<<"Call-ID">>, CallId}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Application-Name">>, <<"hangup">>}
               | wh_api:default_headers(CallId, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],    
    {ok, Json} = wh_api:hangup_req(Command),
    send_callctrl(Json, AHost, CtrlQ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
wait_for_call_event(Name, Application, Timeout) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, Name, Application } ->                
                    {ok, Msg};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _Name } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_call_event(Name, Application, Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
send_callctrl(Json, AHost, CtrlQ) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
