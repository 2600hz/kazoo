%%%============================================================================
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%% This module is responsible for the second stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(conf_service).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([add_member/3, add_moderator/3]).
-export([remove_participant/2]).
-export([set_participant_id/3]).
-export([mute_participant/3, unmute_participant/3, toggle_participant_mute/2]).
-export([deaf_participant/3, undeaf_participant/3, toggle_participant_deaf/2]).
-export([add_bridge/4]).
-export([set_route/2]).
-export([set_control_queue/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("conference.hrl").

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% @public
% @doc
% Starts the server
%
% @end
%------------------------------------------------------------------------------
start_link(Conference) ->
    start_link(Conference, undefined).

start_link(Conference, Caller) ->
    gen_server:start_link(?MODULE, [Conference, Caller], []).

add_member(Srv, CallId, ControlQ) ->
    gen_server:cast(Srv, {add_member, CallId, ControlQ}).

add_moderator(Srv, CallId, ControlQ) ->
    gen_server:cast(Srv, {add_moderator, CallId, ControlQ}).

set_participant_id(Srv, CallId, ParticipantId) ->
    gen_server:cast(Srv, {set_participant_id, CallId, ParticipantId}).

mute_participant(Srv, CallId, Notify) ->
    gen_server:cast(Srv, {mute_participant, CallId, Notify}).

unmute_participant(Srv, CallId, Notify) ->
    gen_server:cast(Srv, {unmute_participant, CallId, Notify}).

toggle_participant_mute(Srv, CallId) ->
    gen_server:cast(Srv, {toggle_participant_mute, CallId}).

deaf_participant(Srv, CallId, Notify) ->
    gen_server:cast(Srv, {deaf_participant, CallId, Notify}).

undeaf_participant(Srv, CallId, Notify) ->
    gen_server:cast(Srv, {undeaf_participant, CallId, Notify}).

toggle_participant_deaf(Srv, CallId) ->
    gen_server:cast(Srv, {toggle_participant_deaf, CallId}).

remove_participant(Srv, CallId) ->
    gen_server:cast(Srv, {remove_participant, CallId}).

add_bridge(Srv, CallId, BridgeId, ControlQ) ->
    gen_server:cast(Srv, {add_bridge, CallId, BridgeId, ControlQ}).

set_route(Srv, Node) ->
    gen_server:cast(Srv, {set_route, Node}).

set_control_queue(Srv, CtrlQ) ->
    gen_server:cast(Srv, {set_control_queue, CtrlQ}).

%%-----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS
%%-----------------------------------------------------------------------------

%------------------------------------------------------------------------------
% @private
% @doc
% Initializes the server
%
% @end
%------------------------------------------------------------------------------
init([Conference, Caller]) ->
    Conf = load_conference_profile(Conference),
    {ok, Q} = start_amqp(Conf#conf.conf_id),
    add_caller(self(), Caller),
    {ok, Conf#conf{service=self(), amqp_q=Q}}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
handle_call(_Request, _From, Conf) ->
    {reply, ok, Conf}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
handle_cast({set_route, Node}, #conf{route=undefined, conf_id=ConfId}=Conf) ->
    [_, Focus] = binary:split(Node, <<"@">>),
    Route = <<"sip:conference_", ConfId/binary, "@", Focus/binary>>,
    ?LOG("set conference route to ~s", [Route]),
    {noreply, Conf#conf{route=Route, focus=Focus}};

handle_cast({set_control_queue, error}, Conf) ->
    {noreply, Conf};

handle_cast({set_control_queue, CtrlQ}, #conf{ctrl_q=CtrlQs}=Conf) ->
    ?LOG("set conference control queue ~s", [CtrlQ]),
    {noreply, Conf#conf{ctrl_q=[CtrlQ|CtrlQs]}};

handle_cast({Action, CallId, ControlQ}, #conf{route=undefined, amqp_q=Q}=Conf) ->
    find_conference_route(CallId, Q),
    %% Put the request back into to mailbox after a brief timeout (hopefully we get the
    %% conference route by then) and re-processes it
    ?LOG("delaying ~s ~s until the route is known", [Action, CallId]),
    {ok, _} = timer:apply_after(500, ?MODULE, Action, [self(), CallId, ControlQ]),
    {noreply, Conf};

handle_cast({add_member, CallId, ControlQ}, #conf{participants=Participants, amqp_q=Q}=Conf) ->
    ?LOG("adding new conference memeber ~s", [CallId]),
    amqp_util:bind_q_to_callevt(Q, CallId),
    request_call_status(CallId, Q),
    {noreply, Conf#conf{participants =
                            dict:store(CallId, #participant{call_id=CallId
                                                            ,control_q=ControlQ}, Participants)}};

handle_cast({add_moderator, CallId, ControlQ}, #conf{participants=Participants, amqp_q=Q}=Conf) ->
    ?LOG("adding new conference moderator ~s", [CallId]),
    amqp_util:bind_q_to_callevt(Q, CallId),
    request_call_status(CallId, Q),
    {noreply, Conf#conf{participants =
                            dict:store(CallId, #participant{call_id=CallId
                                                            ,control_q=ControlQ
                                                            ,moderator=true}, Participants)}};

handle_cast({mute_participant, CallId, Notify}, #conf{participants=Participants, prompts=#prompts{muted=Muted}}=Conf) ->
    case find_participant(CallId, Conf) of
        #participant{call_id=Id, participant_id=ParticipantId}=Participant ->
            ?LOG("muting participant ~s", [ParticipantId]),
            conf_command:mute(ParticipantId, Conf),
            Notify andalso conf_command:play(Muted, ParticipantId, Conf),
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{muted=true}, Participants)}};
        error ->
            {noreply, Conf}
    end;

handle_cast({unmute_participant, CallId, Notify}, #conf{participants=Participants, prompts=#prompts{unmuted=Unmuted}}=Conf) ->
    case find_participant(CallId, Conf) of
        #participant{call_id=Id, participant_id=ParticipantId}=Participant ->
            ?LOG("unmuting participant ~s", [ParticipantId]),
            conf_command:unmute(ParticipantId, Conf),
            Notify andalso conf_command:play(Unmuted, ParticipantId, Conf),
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{muted=false}, Participants)}};
        error ->
            {noreply, Conf}
    end;

handle_cast({toggle_participant_mute, CallId}, Conf) ->
    case find_participant(CallId, Conf) of
        #participant{muted=true} ->
            unmute_participant(self(), CallId, true),
            {noreply, Conf};
        #participant{muted=false} ->
            mute_participant(self(), CallId, true),
            {noreply, Conf};
        error ->
            {noreply, Conf}
    end;

handle_cast({deaf_participant, CallId, Notify}, #conf{participants=Participants, prompts=#prompts{deaf=Deaf}}=Conf) ->
    case find_participant(CallId, Conf) of
        #participant{call_id=Id, participant_id=ParticipantId}=Participant ->
            ?LOG("deaf participant ~s", [ParticipantId]),
            conf_command:deaf(ParticipantId, Conf),
            Notify andalso conf_command:play(Deaf, ParticipantId, Conf),
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{deaf=true}, Participants)}};
        error ->
            {noreply, Conf}
    end;

handle_cast({undeaf_participant, CallId, Notify}, #conf{participants=Participants, prompts=#prompts{undeaf=Undeaf}}=Conf) ->
    case find_participant(CallId, Conf) of
        #participant{call_id=Id, participant_id=ParticipantId}=Participant ->
            ?LOG("undeaf participant ~s", [ParticipantId]),
            conf_command:undeaf(ParticipantId, Conf),
            Notify andalso conf_command:play(Undeaf, ParticipantId, Conf),
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{deaf=false}, Participants)}};
        error ->
            {noreply, Conf}
    end;

handle_cast({toggle_participant_deaf, CallId}, Conf) ->
    case find_participant(CallId, Conf) of
        #participant{deaf=true} ->
            undeaf_participant(self(), CallId, true),
            {noreply, Conf};
        #participant{deaf=false} ->
            deaf_participant(self(), CallId, true),
            {noreply, Conf};
        error ->
            {noreply, Conf}
    end;

handle_cast({remove_participant, CallId}, #conf{participants=Participants, ctrl_q=CtrlQs}=Conf) ->
    {NewPtcp, NewQs} = case find_participant(CallId, Conf) of
                           error ->
                               ?LOG("could not find active participant ~s for removal", [CallId]),
                               {Participants, CtrlQs};
                           #participant{call_id=Id} ->
                               ?LOG("removing conference participant ~s (~s)", [Id, CallId]),
                               remove_participant_event(CallId, Conf),
                               {dict:erase(Id, Participants), lists:delete(find_ctrl_q(Id, Conf), CtrlQs)}
              end,
    case dict:size(NewPtcp) of
        0 ->
            ?LOG("last participant has left, shuting down"),
            {stop, shutdown, Conf#conf{participants=NewPtcp, ctrl_q=NewQs}};
        _ ->
            {noreply, Conf#conf{participants=NewPtcp, ctrl_q=NewQs}}
    end;

handle_cast({set_participant_id, CallId, ParticipantId}, #conf{participants=Participants}=Conf) ->
    case find_participant(CallId, Conf) of
        #participant{call_id=Id}=Participant ->
            ?LOG("setting participant id for ~s (~s) to ~s", [Id, CallId, ParticipantId]),
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{call_id=CallId
                                                                           ,participant_id=ParticipantId}, Participants)}};
        error ->
            {noreply, Conf}
    end;

handle_cast({add_bridge, CallId, BridgeId, ControlQ}, #conf{participants=Participants}=Conf) ->
    ?LOG("adding new bridge ~s to ~s", [BridgeId, CallId]),
    case find_participant(CallId, Conf) of
        #participant{call_id=Id}=Participant ->
            {noreply, Conf#conf{participants =
                                    dict:store(Id, Participant#participant{call_id=CallId
                                                                           ,bridge_id=BridgeId
                                                                           ,bridge_ctrl=ControlQ}, Participants)}};
        _ ->
            {noreply, Conf#conf{participants =
                                    dict:store(CallId, #participant{call_id=CallId
                                                                    ,bridge_id=BridgeId
                                                                    ,bridge_ctrl=ControlQ}, Participants)}}
    end;

handle_cast(_Msg, Conf) ->
    {noreply, Conf}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
handle_info({amqp_reconnect, T}, #conf{conf_id=ConfId}=Conf) ->
    try
	{ok, NewQ} = start_amqp(ConfId),
	{noreply, Conf#conf{amqp_q=NewQ}}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, Conf};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, Conf}
            end
    end;

handle_info({amqp_host_down, _}, Conf) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, Conf#conf{amqp_q = <<>>}};

handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload=Payload}}, #conf{conf_id=ConfId}=Conf) ->
    spawn(fun() ->
                  put(callid, ConfId),
                  JObj = mochijson2:decode(Payload),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, Conf)
          end),
    {noreply, Conf};

%% TODO: Temporary, since we dont have a reliable control channel for the conference yet some commands may bounce back
%%  since the callers control channel we were using has gone away.  When his happens take the control channel of the next
%%  participant and replay the conference command.
handle_info({#'basic.return'{}, #amqp_msg{}}, #conf{ctrl_q=[]}=Conf) ->
    ?LOG("no control channels left to replay conference command, dropping"),
    {noreply, Conf};

handle_info({#'basic.return'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload=Payload}}
            ,#conf{conf_id=ConfId, ctrl_q=CtrlQs}=Conf) ->
    try
        JObj = mochijson2:decode(Payload),
        {<<"conference">>, <<"command">>} = whapps_util:get_event_type(JObj),
        ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
        case tl(CtrlQs) of
            [] ->
                ?LOG("no control channels left to replay conference command, dropping"),
                {noreply, Conf#conf{ctrl_q=[]}};
            NewQ ->
                ?LOG("replay a conference command on new control channel"),
                amqp_util:callctl_publish(NewQ, Payload, <<"application/json">>, [{mandatory, true}]),
                {noreply, Conf#conf{ctrl_q=NewQ}}
        end
    catch
        _:_ ->
            {noreply, Conf}
    end;

handle_info(_Info, Conf) ->
    {noreply, Conf}.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
terminate( _Reason, _Conf) ->
    ?LOG_END("conference service ~p termination", [_Reason]),
    ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
code_change(_OldVsn, Conf, _Extra) ->
    {ok, Conf}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/1 :: (ConfId) -> tuple(ok, binary()) when
      ConfId :: binary().
start_amqp(ConfId) ->
    try
        _ = amqp_util:conference_exchange(),
        _ = amqp_util:targeted_exchange(),
        _ = amqp_util:callmgr_exchange(),
        _ = amqp_util:callevt_exchange(),
        true = is_binary(Q = amqp_util:new_conference_queue(ConfId)),
	ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHN_REQ),
        ok = amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
        ok = amqp_util:bind_q_to_conference(Q, service, ConfId),
        ok = amqp_util:bind_q_to_conference(Q, events, ConfId),
        ok = amqp_util:bind_q_to_targeted(Q),
	ok = amqp_util:basic_consume(Q),
        amqp_util:register_return_handler(),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec process_req/3 :: (MsgType, JObj, Conf) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object(),
      Conf :: #conf{}.
process_req({<<"conference">>, <<"add_caller">>}, JObj, #conf{service=Srv}) ->
    %% When we receive a request to add a caller to the conference, pass it to the logic that
    %% will determine if they should be added as a moderator and then places the request into
    %% the mailbox of the appropriate conference server.
    ?LOG("recieved AMQP request to add a caller to the conference"),
    add_caller(Srv, JObj);

process_req({<<"call_event">>, <<"status_resp">>}, JObj, #conf{service=Srv, route=undefined}) ->
    %% Currently, when the conference has no focus and hence no route we will use the node of
    %% the first call status response. Since no callers can 'join' the conference till the
    %% route is known, the call status will be re-requested and the logic bellow applied
    case wh_json:get_value(<<"Node">>, JObj) of
        undefined -> ok;
        Node ->
            ?LOG("recieved call status, but hijacking it for the route"),
            set_route(Srv, Node)
    end;

process_req({<<"call_event">>, <<"status_resp">>}, JObj, #conf{focus=Focus}=Conf) ->
    %% Determine the node the call is currently on
    %% - Bridge to the conference node
    %% - Join the conference
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    ?LOG("recieved call status for ~s", [CallId]),
    case binary:split(wh_json:get_value(<<"Node">>, JObj), <<"@">>) of
        [_, Focus] ->
            join_local_conference(CallId, Conf);
        [_, Loc] ->
            ?LOG("call ~s is at ~p but focus is ~s", [CallId, Loc, Focus]),
            bridge_to_conference(CallId, Conf)
    end;

process_req({<<"call_event">>, <<"CHANNEL_EXECUTE">>}, JObj, #conf{service=Srv, prompts=Prompts
                                                                   ,member_join_muted=MBJM, member_join_deaf=MBJD
                                                                   ,moderator_join_muted=MDJM, moderator_join_deaf=MDJD}=Conf) ->
    %% If we get notification that a caller has executed the conference application then
    %% fire off an event that they are now part of the conference. Also, notify the other
    %% conference participants that they have joined.
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"conference">> ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            ParticipantId = wh_json:get_value(<<"Application-Response">>, JObj),
            set_control_queue(Srv, find_ctrl_q(CallId, Conf)),
            set_participant_id(Srv, CallId, ParticipantId),
            added_participant_event(CallId, Conf),
            %% This could be done (and originally was) by the join_local conference
            %% but by doing this it syncs for toggles and notifies the caller of
            %% their intial state in the conference.
            case is_moderator(CallId, Conf) of
                true ->
                    MDJM andalso mute_participant(Srv, CallId, true),
                    MDJD andalso deaf_participant(Srv, CallId, true);
                false ->
                    MBJM andalso mute_participant(Srv, CallId, true),
                    MBJD andalso deaf_participant(Srv, CallId, true)
            end,
            conf_command:play(Prompts#prompts.announce_join, Conf),
            conf_command:participants(Conf);
        _ -> ok
    end;

process_req({<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}, JObj, #conf{prompts=Prompts, service=Srv}=Conf) ->
    %% If we get notification that a caller has executed the conference application then
    %% fire off an event that they are now part of the conference. Also, notify the other
    %% conference participants that they have joined.
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"conference">> ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            remove_participant(Srv, CallId),
            conf_command:play(Prompts#prompts.announce_leave, Conf),
            conf_command:participants(Conf);
        _ -> ok
    end;

process_req({<<"call_event">>, <<"DTMF">>}, JObj, Conf) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Digit = wh_json:get_value(<<"DTMF-Digit">>, JObj),
    in_conf_control(CallId, Digit, Conf);

process_req({<<"directory">>, <<"authn_req">>}, JObj, #conf{conf_id=ConfId, auth_pwd=AuthPwd, amqp_q=Q}=Conf) ->
    %% If we receive an auth requests for an invite to our conference then respond with the sip auth password
    %% so if it was a bridge that we built it will be accepted.
    try
        <<"INVITE">> = wh_json:get_value(<<"Method">>, JObj),
        [<<"conference_", ConfId/binary>>, _] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
        AuthUser = wh_json:get_value(<<"Auth-User">>, JObj),
        true = is_participant(AuthUser, Conf),
        ?LOG("recieved auth request for a bridge on behalf of ~s", [AuthUser]),
        send_auth_response(JObj, ConfId, AuthUser, AuthPwd, Q)
    catch
        _:_ -> ok
    end;

process_req({<<"dialplan">>, <<"route_req">>}, JObj, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    %% If a bridge that we built is requesting a route for this conference repond
    try
        [<<"conference_", ConfId/binary>>, _] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
        true = is_participant(wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Username">>], JObj), Conf),
        ?LOG("recieved route request for conference"),
        send_route_response(JObj, Q)
    catch
        _:_ -> ok
    end;

process_req({_, <<"route_win">>}, JObj, #conf{service=Srv, amqp_q=Q}=Conf) ->
    %% If we get the route win then determine (based on the original A-leg on the other node)
    %% whether this is a moderator and have the new call join the conference locally
    BridgeId = wh_json:get_value(<<"Call-ID">>, JObj),
    CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    CallId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Username">>], JObj),

    ?LOG("recieved route win for ~s", [BridgeId]),

    amqp_util:unbind_q_from_callevt(Q, CallId, events),
    amqp_util:bind_q_to_callevt(Q, BridgeId),

    add_bridge(Srv, CallId, BridgeId, CtrlQ),
    join_local_conference(BridgeId, CtrlQ, Conf);

%% process_req({<<"conference">>, <<"participants">>}, JObj, Conf) ->
%%    ok;

process_req(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute a local command to place the caller in a conference on the
%% node the call is currently on
%% @end
%%--------------------------------------------------------------------
-spec join_local_conference/2 :: (CallId, Conf) -> no_return() when
      CallId :: binary(),
      Conf :: #conf{}.
-spec join_local_conference/3 :: (CallId, CtrlQ, Conf) -> no_return() when
      CallId :: binary(),
      CtrlQ :: error | binary(),
      Conf :: #conf{}.

join_local_conference(CallId, Conf) ->
    join_local_conference(CallId, find_ctrl_q(CallId, Conf), Conf).

join_local_conference(_, error, _) ->
    ok;
join_local_conference(CallId, CtrlQ, #conf{conf_id=ConfId, amqp_q=Q}) ->
    ?LOG("attempting to have ~s join the conference locally via ~p", [CallId, CtrlQ]),
    Answer = [{<<"Application-Name">>, <<"answer">>}
              ,{<<"Call-ID">>, CallId}
              | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
             ],
    Conference = [{<<"Application-Name">>, <<"conference">>}
                  ,{<<"Conference-ID">>, ConfId}
                  ,{<<"Moderator">>, <<"false">>}
                  ,{<<"Call-ID">>, CallId}
                  | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                 ],
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Commands">>, [{struct, Conference}, {struct, Answer}]}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:queue_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute a local command to bridge the caller to the node the
%% conference is hosted on. The information is shared as follows:
%% - Route request will be conference_{conf_id}@{conference_focus}
%% - Auth user name will be the call id on the originating server
%%     (the one that is trying to join the conference)
%% - Auth password will be predetermined and in the conf record
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_conference/2 :: (CallId, Conf) -> no_return() when
      CallId :: binary(),
      Conf :: #conf{}.
-spec bridge_to_conference/3 :: (CallId, CtrlQ, Conf) -> no_return() when
      CallId :: binary(),
      CtrlQ :: error | binary(),
      Conf :: #conf{}.

bridge_to_conference(CallId, Conf) ->
    bridge_to_conference(CallId, find_ctrl_q(CallId, Conf), Conf).

bridge_to_conference(_, error, _) ->
    ok;
bridge_to_conference(CallId, CtrlQ, #conf{amqp_q=Q, route=Route, auth_pwd=AuthPwd}) ->
    ?LOG("bridging participant ~s to ~s", [CallId, Route]),
    Endpoint = [{<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Route}
                ,{<<"Auth-User">>, CallId}
                ,{<<"Auth-Password">>, AuthPwd}
               ],
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [{struct, Endpoint}]}
               ,{<<"Timeout">>, <<"20">>}
               ,{<<"Media">>, <<"bypass">>}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:bridge_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provides in conference functionality, via DTMF
%% @end
%%--------------------------------------------------------------------
-spec in_conf_control/3 :: (CallId, Key, Conf) -> no_return() when
      CallId :: binary(),
      Key :: binary(),
      Conf :: #conf{}.
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{mute=Key}}) ->
    mute_participant(Srv, CallId, true);
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{unmute=Key}}) ->
    unmute_participant(Srv, CallId, true);
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{toggle_mute=Key}}) ->
    toggle_participant_mute(Srv, CallId);
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{deaf=Key}}) ->
    deaf_participant(Srv, CallId, true);
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{undeaf=Key}}) ->
    undeaf_participant(Srv, CallId, true);
in_conf_control(CallId, Key, #conf{service=Srv, controls=#control{toggle_deaf=Key}}) ->
    toggle_participant_deaf(Srv, CallId);
in_conf_control(_, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a call-id find the caller, returning whether it was found in
%% the moderators list and the control queue.
%% @end
%%--------------------------------------------------------------------
-spec find_participant/2 :: (CallId, Conf) -> error | #participant{}  when
      CallId :: binary(),
      Conf :: #conf{}.
find_participant(CallId, #conf{participants=Participants}) ->
    Id = dict:fold(fun(C, #participant{bridge_id=Acc}, Acc) -> C;
                      (_, _, Acc) -> Acc end, CallId, Participants),
    case dict:find(Id, Participants) of
        {ok, #participant{}=P} -> P;
        error -> error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a call-id find the caller, returning true if they are a
%% moderator otherwise return false. If the call-id is not known
%% return error.
%% @end
%%--------------------------------------------------------------------
-spec is_moderator/2 :: (CallId, Conf) -> error | boolean() when
      CallId :: binary(),
      Conf :: #conf{}.
is_moderator(CallId, Conf) ->
    case find_participant(CallId, Conf) of
        error -> false;
        #participant{moderator=Mod} -> Mod
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a call-id return true if it is known to this conference
%% service, otherwise return false
%% @end
%%--------------------------------------------------------------------
-spec is_participant/2 :: (CallId, Conf) -> boolean() when
      CallId :: binary(),
      Conf :: #conf{}.
is_participant(CallId, Conf) ->
    find_participant(CallId, Conf) =/= error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a call-id find the control q
%% @end
%%--------------------------------------------------------------------
-spec find_ctrl_q/2 :: (CallId, Conf) -> error | binary() when
      CallId :: binary(),
      Conf :: #conf{}.
find_ctrl_q(CallId, Conf) ->
    case find_participant(CallId, Conf) of
        error -> error;
        #participant{bridge_ctrl=undefined, control_q=CtrlQ} -> CtrlQ;
        #participant{bridge_ctrl=CtrlQ} -> CtrlQ
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When a conference service is spun up it is passed the JSON profile,
%% this function breaks out the parameters into the conference record
%% populating missing properties with their default value.
%% @end
%%--------------------------------------------------------------------
-spec load_conference_profile/1 :: (Conference) -> #conf{} when
      Conference :: json_object().
load_conference_profile(JObj) ->
    ConfId = wh_json:get_value(<<"_id">>, JObj),
    put(callid, ConfId),
    Default=#conf{},
    #conf{conf_id =
               ConfId
          ,member_pins =
               wh_json:get_value(<<"member_pins">>, JObj, [])
          ,moderator_pins =
               wh_json:get_value(<<"moderator_pins">>, JObj, [])
          ,member_join_muted =
               wh_json:is_true(<<"member_join_muted">>, JObj, Default#conf.member_join_muted)
          ,member_join_deaf =
               wh_json:is_true(<<"member_join_deaf">>, JObj, Default#conf.member_join_deaf)
          ,moderator_join_muted =
               wh_json:is_true(<<"moderator_join_muted">>, JObj, Default#conf.moderator_join_muted)
          ,moderator_join_deaf =
               wh_json:is_true(<<"moderator_join_deaf">>, JObj, Default#conf.moderator_join_deaf)
          ,max_members =
               wh_json:get_integer_value(<<"max_members">>, JObj, Default#conf.max_members)
          ,require_moderator =
               wh_json:is_true(<<"require_moderator">>, JObj, Default#conf.require_moderator)
          ,wait_for_moderator =
               wh_json:is_true(<<"wait_for_moderator">>, JObj, Default#conf.wait_for_moderator)
          ,auth_pwd =
               wh_json:get_value([<<"sip">>, <<"password">>], JObj, Default#conf.auth_pwd)
         }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extracts the caller call-id and control-queue then determines if
%% it is a moderator and invokes the appropriate API to add them
%% to a conference service.
%% @end
%%--------------------------------------------------------------------
-spec add_caller/2 :: (Srv, Caller) -> ok when
      Srv :: pid(),
      Caller :: undefined | json_object().
add_caller(_, undefined) ->
    %% Allow the conference service to be started with no inital caller, for
    %% later features such as timed auto-dialing conferences
    ok;
add_caller(Srv, JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    CtrlQ = wh_json:get_value(<<"Control-Queue">>, JObj),
    case wh_json:is_true(<<"Moderator">>, JObj) of
        true -> add_moderator(Srv, CallId, CtrlQ);
        false -> add_member(Srv, CallId, CtrlQ)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds an avaliable conference resource, eventually... Currently
%% it requests the call status of the given call-id.  The response
%% will contain the node the call is currently on which we will
%% adopt as our conference focus.  This works because a special case
%% in process_req that, if the route is undefined, redirects a call
%% status response to set_route.  Furthermore, since requests to add
%% callers loop every half second until the conference has a route
%% we are just 'borrowing' this response...Once the route is defined
%% and the request loops back it will be processed normally.
%% @end
%%--------------------------------------------------------------------
-spec find_conference_route/2 :: (CallId, Q) -> ok when
      CallId :: binary(),
      Q :: binary().
find_conference_route(CallId, Q) ->
    request_call_status(CallId, Q).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produce the low level call status request, to determine the node
%% of the givent call-id.  When the service recieves a call status
%% request it understands that it is part of the caller addition
%% mechanism (except for the special case when the conference focus
%% is not known) and based on the caller's node will perform the
%% appropriate action to connect them to the conferece.
%% @end
%%--------------------------------------------------------------------
-spec request_call_status/2 :: (CallId, Q) -> ok when
      CallId :: binary(),
      Q :: binary().
request_call_status(CallId, Q) ->
    ?LOG("requesting the status of participant ~s", [CallId]),
    Command = [{<<"Call-ID">>, CallId}
               | wh_api:default_headers(Q, <<"call_event">>, <<"status_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:call_status_req({struct, Command}),
    amqp_util:callevt_publish(CallId, Payload, status_req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% publish a conference event anouncing the addtion of a caller to
%% this conference
%% @end
%%--------------------------------------------------------------------
-spec added_participant_event/2 :: (CallId, Conf) -> ok when
      CallId :: binary(),
      Conf :: #conf{}.
added_participant_event(CallId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    #participant{moderator=Moderator, call_id=Id} = find_participant(CallId, Conf),
    ?LOG("sending event that ~s (~s) has been added to ~s", [Id, CallId, ConfId]),
    Command = [{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, Id}
               ,{<<"Moderator">>, wh_util:to_binary(Moderator)}
               | wh_api:default_headers(Q, <<"conference">>, <<"added_participant">>, ?APP_NAME, ?APP_VERSION)
              ],
    Payload = mochijson2:encode({struct, Command}),
    amqp_util:conference_publish(Payload, events, ConfId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% publish a conference event anouncing the removal of a caller to
%% this conference
%% @end
%%--------------------------------------------------------------------
-spec remove_participant_event/2 :: (CallId, Conf) -> no_return() when
      CallId :: binary(),
      Conf :: #conf{}.
remove_participant_event(CallId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    #participant{moderator=Moderator, call_id=Id} = find_participant(CallId, Conf),
    ?LOG("sending event that ~s (~s) has been removed from ~s", [Id, CallId, ConfId]),
    Command = [{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, Id}
               ,{<<"Moderator">>, wh_util:to_binary(Moderator)}
               | wh_api:default_headers(Q, <<"conference">>, <<"removed_participant">>, ?APP_NAME, ?APP_VERSION)
              ],
    Payload = mochijson2:encode({struct, Command}),
    amqp_util:conference_publish(Payload, events, ConfId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a response for a route request
%% @end
%%--------------------------------------------------------------------
-spec send_route_response/2 :: (JObj, Q) -> no_return() when
      JObj :: json_object(),
      Q :: binary().
send_route_response(JObj, Q) ->
    ?LOG("sending route response"),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Routes">>, []}
                ,{<<"Method">>, <<"park">>}
                ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT)}
                | wh_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = wh_api:route_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a response to an auth request
%% @end
%%--------------------------------------------------------------------
-spec send_auth_response/5 :: (JObj, ConfId, AuthUser, AuthPwd, Q) -> no_return() when
      JObj :: json_object(),
      ConfId :: binary(),
      AuthUser :: binary(),
      AuthPwd :: binary(),
      Q :: binary().
send_auth_response(JObj, ConfId, AuthUser, AuthPwd, Q) ->
    ?LOG("sending auth response for ~s to join ~s", [AuthUser, ConfId]),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Auth-Password">>, AuthPwd}
                ,{<<"Auth-Method">>, <<"password">>}
                ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Username">>, AuthUser}
                                                       ,{<<"Realm">>, wh_json:get_value(<<"Auth-Realm">>, JObj)}
                                                       ,{<<"Authorizing-ID">>, ConfId}
                                                       ,{<<"Conference-ID">>, ConfId}]}}
                | wh_api:default_headers(Q, <<"directory">>, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = wh_api:authn_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).
