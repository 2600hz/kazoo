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
-export([add_member/3, remove_member/2]).
-export([add_moderator/3, remove_moderator/2]).
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

remove_member(Srv, CallId) ->
    gen_server:cast(Srv, {remove_member, CallId}).

add_moderator(Srv, CallId, ControlQ) ->
    gen_server:cast(Srv, {add_moderator, CallId, ControlQ}).

remove_moderator(Srv, CallId) ->
    gen_server:cast(Srv, {remove_moderator, CallId}).

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
    add_caller(Caller, self()),
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

handle_cast({set_control_queue, CtrlQ}, Conf) ->
    ?LOG("set conference control queue ~s", [CtrlQ]),
    {noreply, Conf#conf{ctrl_q=CtrlQ}};

handle_cast({Action, CallId, ControlQ}, #conf{route=undefined, amqp_q=Q}=Conf) ->
    find_conference_route(CallId, Q),
    %% Put the request back into to mailbox after a brief timeout (hopefully we get the
    %% conference route by then) and re-processes it
    ?LOG("delaying ~s ~s until the route is known", [Action, CallId]),
    timer:apply_after(500, ?MODULE, Action, [self(), CallId, ControlQ]),
    {noreply, Conf};

handle_cast({add_member, CallId, ControlQ}, #conf{members=Members, amqp_q=Q}=Conf) ->
    ?LOG("adding new conference memeber ~s", [CallId]),
    amqp_util:bind_q_to_callevt(Q, CallId),
    request_call_status(CallId, Q),
    {noreply, Conf#conf{members=dict:store(CallId, {struct, [{<<"Control-Queue">>, ControlQ}]}, Members)}};

handle_cast({remove_member, CallId}, #conf{moderators=Moderators, members=Members, bridges=Bridges, conf_id=ConfId, amqp_q=Q}=Conf) ->
    ?LOG("removing conference member ~s", [CallId]),
    NewMembers = dict:erase(CallId, Members),
    remove_participant_event(CallId, ConfId, <<"false">>, Q),
    case dict:size(Moderators) + dict:size(NewMembers) of
        0 ->
            ?LOG("last participant has left, shuting down"),
            {stop, shutdown, Conf#conf{members=NewMembers, bridges=dict:erase(CallId, Bridges)}};
        _ ->
            {noreply, Conf#conf{members=NewMembers, bridges=dict:erase(CallId, Bridges)}}
    end;

handle_cast({add_moderator, CallId, ControlQ}, #conf{moderators=Moderators, amqp_q=Q}=Conf) ->
    ?LOG("adding new conference moderator ~s", [CallId]),
    amqp_util:bind_q_to_callevt(Q, CallId),
    request_call_status(CallId, Q),
    {noreply, Conf#conf{moderators=dict:store(CallId, {struct, [{<<"Control-Queue">>, ControlQ}]}, Moderators)}};

handle_cast({remove_moderator, CallId}, #conf{moderators=Moderators, members=Members, bridges=Bridges, conf_id=ConfId, amqp_q=Q}=Conf) ->
    ?LOG("removing conference moderator ~s", [CallId]),
    NewModerators = dict:erase(CallId, Moderators),
    remove_participant_event(CallId, ConfId, <<"true">>, Q),
    case dict:size(NewModerators) + dict:size(Members) of
        0 ->
            ?LOG("last participant has left, shuting down"),
            {stop, shutdown, Conf#conf{moderators=NewModerators, bridges=dict:erase(CallId, Bridges)}};
        _ ->
            {noreply, Conf#conf{moderators=NewModerators, bridges=dict:erase(CallId, Bridges)}}
    end;

handle_cast({add_bridge, CallId, BridgeId, ControlQ}, #conf{bridges=Bridges}=Conf) ->
    ?LOG("adding new bridge ~s to ~s", [BridgeId, CallId]),
    {noreply, Conf#conf{bridges=dict:store(CallId, {BridgeId, ControlQ}, Bridges)}};

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
                    timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, Conf};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, Conf}
            end
    end;

handle_info({amqp_host_down, _}, Conf) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, Conf#conf{amqp_q = <<>>}};

%%handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">>}, payload=Payload}}, #conf{conf_id=ConfId}=Conf) ->
handle_info({#'basic.deliver'{}, #amqp_msg{payload=Payload}}, #conf{conf_id=ConfId}=Conf) ->
    spawn(fun() ->
                  put(callid, ConfId),
                  JObj = mochijson2:decode(Payload),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, Conf)
          end),
    {noreply, Conf};

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
-spec(start_amqp/1 :: (ConfId :: binary()) -> tuple(ok, binary())).
start_amqp(ConfId) ->
    try
        _ = amqp_util:conference_exchange(),
        _ = amqp_util:targeted_exchange(),
        _ = amqp_util:callmgr_exchange(),
        _ = amqp_util:callevt_exchange(),
        Q = amqp_util:new_conference_queue(ConfId),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTH_REQ),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
        amqp_util:bind_q_to_conference(Q, service, ConfId),
        amqp_util:bind_q_to_conference(Q, events, ConfId),
        amqp_util:bind_q_to_targeted(Q),
	amqp_util:basic_consume(Q),
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
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), Conf :: #conf{}) -> no_return()).
process_req({<<"conference">>, <<"add_caller">>}, JObj, #conf{service=Srv}) ->
    %% When we receive a request to add a caller to the conference, pass it to the logic that
    %% will determine if they should be added as a moderator and then places the request into
    %% the mailbox of the appropriate conference server.
    ?LOG("recieved AMQP request to add a caller to the conference"),
    add_caller(JObj, Srv);

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
        _ ->
            bridge_to_conference(CallId, Conf)
    end;

process_req({<<"call_event">>, <<"CHANNEL_EXECUTE">>}, JObj, #conf{service=Srv, ctrl_q=undefined}=Conf) ->
    %% When a participant joins the conference but we have no control queue use the new new calls control queue
    %% as the conference q
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"conference">> ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            {_, CtrlQ} = find_ctrl_q(CallId, Conf),
            set_control_queue(Srv, CtrlQ),
            process_req({<<"call_event">>, <<"CHANNEL_EXECUTE">>}, JObj, Conf#conf{ctrl_q=CtrlQ});
        _ ->
            ok
    end;

process_req({<<"call_event">>, <<"CHANNEL_EXECUTE">>}, JObj, #conf{conf_id=ConfId, amqp_q=Q, bridges=Bridges, prompts=Prompts}=Conf) ->
    %% If we get notification that a caller has executed the conference application then
    %% fire off an event that they are now part of the conference. Also, notify the other
    %% conference participants that they have joined.
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"conference">> ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            ?LOG("recieved call event that ~s has been placed in the conference", [CallId]),
            Id = dict:fold(fun(C, {Acc, _}, Acc) -> C; (_, _, Acc) -> Acc end, CallId, Bridges),
            added_participant_event(Id, ConfId, is_moderator(Id, Conf), Q),
            conf_command:play(Prompts#prompts.announce_join, Conf),
            conf_command:participants(Conf);
        _ -> ok
    end;

process_req({<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}, JObj, #conf{bridges=Bridges, prompts=Prompts, service=Srv}=Conf) ->
    %% If we get notification that a caller has executed the conference application then
    %% fire off an event that they are now part of the conference. Also, notify the other
    %% conference participants that they have joined.
    case wh_json:get_value(<<"Application-Name">>, JObj) of
        <<"conference">> ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            Id = dict:fold(fun(C, {Acc, _}, Acc) -> C; (_, _, Acc) -> Acc end, CallId, Bridges),
            case find_ctrl_q(Id, Conf) of
                {false, _} -> remove_member(Srv, Id);
                {true, _} -> remove_moderator(Srv, Id)
            end,
            conf_command:play(Prompts#prompts.announce_leave, Conf),
            conf_command:participants(Conf);
        _ -> ok
    end;

process_req({<<"directory">>, <<"auth_req">>}, JObj, #conf{conf_id=ConfId, auth_pwd=AuthPwd, amqp_q=Q}=Conf) ->
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

    %% TODO: fix the unbind helper functions so they have the same spec as the bind
    amqp_util:unbind_q_from_callevt(Q, <<?KEY_CALL_EVENT/binary, CallId/binary>>),
    amqp_util:bind_q_to_callevt(Q, BridgeId),

    add_bridge(Srv, CallId, BridgeId, CtrlQ),
    ?LOG("recieved route win for ~s", [BridgeId]),
    join_local_conference(BridgeId, {is_moderator(CallId, Conf), CtrlQ}, Conf);

process_req({<<"conference">>, <<"participants">>}, JObj, Conf) ->
    io:format("Participants: ~p~n", [JObj]),
    ok;

process_req(_, T, _) ->
    io:format("~p~n", [T]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute a local command to place the caller in a conference on the
%% node the call is currently on
%% @end
%%--------------------------------------------------------------------
join_local_conference(CallId, Conf) ->
    join_local_conference(CallId, find_ctrl_q(CallId, Conf), Conf).

join_local_conference(CallId, {false, CtrlQ}, #conf{conf_id=ConfId
                                                    ,member_join_muted=MJM
                                                    ,member_join_deaf=MJD
                                                    ,amqp_q=Q}) ->
    ?LOG("attempting to have member ~s join the conference locally", [CallId]),
    Command = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, whistle_util:to_binary(MJM)}
               ,{<<"Deaf">>, whistle_util:to_binary(MJD)}
               ,{<<"Moderator">>, <<"false">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload);
join_local_conference(CallId, {true, CtrlQ}, #conf{conf_id=ConfId
                                                   ,moderator_join_muted=MJM
                                                   ,moderator_join_deaf=MJD
                                                   ,amqp_q=Q}) ->
    ?LOG("attempting to have moderator ~s join the conference locally", [CallId]),
    Command = [{<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Mute">>, whistle_util:to_binary(MJM)}
               ,{<<"Deaf">>, whistle_util:to_binary(MJD)}
               ,{<<"Moderator">>, <<"false">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_req(Command),
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
bridge_to_conference(CallId, Conf) ->
    bridge_to_conference(CallId, find_ctrl_q(CallId, Conf), Conf).

bridge_to_conference(CallId, {_, CtrlQ}, #conf{amqp_q=Q, route=Route, auth_pwd=AuthPwd}) ->
    ?LOG("bridging participant ~s to ~s", [CallId, Route]),
    Endpoint = [{<<"Invite-Format">>, <<"route">>}
                ,{<<"Route">>, Route}
                ,{<<"Auth-User">>, CallId}
                ,{<<"Auth-Password">>, AuthPwd}
               ],
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [{struct, Endpoint}]}
               ,{<<"Timeout">>, <<"20">>}
               ,{<<"Ignore-Early-Media">>, <<"false">>}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:bridge_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a call-id find the caller, returning whether it was found in
%% the moderators list and the control queue.
%% @end
%%--------------------------------------------------------------------
find_participant(CallId, #conf{members=Members, moderators=Moderators}) ->
    case dict:find(CallId, Members) of
        {ok, JObj} ->
            {false, JObj};
        error ->
            case dict:find(CallId, Moderators) of
                {ok, JObj} ->
                    {true, JObj};
                error ->
                    error
            end
    end.

is_moderator(CallId, Conf) ->
    case find_participant(CallId, Conf) of
        {Moderator, _} -> Moderator;
        error -> false
    end.

is_participant(CallId, Conf) ->
    find_participant(CallId, Conf) =/= error.

find_member_id(CallId, Conf) ->
    case find_participant(CallId, Conf) of
        {Moderator, JObj} ->
            {Moderator, wh_json:get_value(<<"Member-ID">>, JObj)};
        error -> error
    end.

find_ctrl_q(CallId, Conf) ->
    case find_participant(CallId, Conf) of
        {Moderator, JObj} ->
            {Moderator, wh_json:get_value(<<"Control-Queue">>, JObj)};
        error -> error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When a conference service is spun up it is passed the JSON profile,
%% this function breaks out the parameters into the conference record
%% populating missing properties with their default value.
%% @end
%%--------------------------------------------------------------------
-spec(load_conference_profile/1 :: (Conference :: json_object()) -> #conf{}).
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
-spec(add_caller/2 :: (Caller :: json_object() | undefined, Srv :: pid()) -> ok).
add_caller(undefined, _) ->
    %% Allow the conference service to be started with no inital caller, for
    %% later features such as timed auto-dialing conferences
    ok;
add_caller(JObj, Srv) ->
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
request_call_status(CallId, Q) ->
    ?LOG("requesting the status of participant ~s", [CallId]),
    Command = [{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(Q, <<"call_event">>, <<"status_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:call_status_req({struct, Command}),
    amqp_util:callevt_publish(CallId, Payload, status_req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% publish a conference event anouncing the addtion of a caller to
%% this conference
%% @end
%%--------------------------------------------------------------------
added_participant_event(CallId, ConfId, Moderator, Q) ->
    ?LOG("sending event that member ~s has been added to ~s", [CallId, ConfId]),
    Command = [{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Moderator">>, Moderator}
               | whistle_api:default_headers(Q, <<"conference">>, <<"added_participant">>, ?APP_NAME, ?APP_VERSION)
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
remove_participant_event(CallId, ConfId, Moderator, Q) ->
    ?LOG("sending event that moderator ~s has been removed from ~s", [CallId, ConfId]),
    Command = [{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Moderator">>, Moderator}
               | whistle_api:default_headers(Q, <<"conference">>, <<"removed_participant">>, ?APP_NAME, ?APP_VERSION)
              ],
    Payload = mochijson2:encode({struct, Command}),
    amqp_util:conference_publish(Payload, events, ConfId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a response for a route request
%% @end
%%--------------------------------------------------------------------
send_route_response(JObj, Q) ->
    ?LOG("sending route response"),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Routes">>, []}
                ,{<<"Method">>, <<"park">>}
                | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = whistle_api:route_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a response to an auth request
%% @end
%%--------------------------------------------------------------------
send_auth_response(JObj, ConfId, AuthUser, AuthPwd, Q) ->
    ?LOG("sending auth response for ~s to join ~s", [AuthUser, ConfId]),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Auth-Password">>, AuthPwd}
                ,{<<"Auth-Method">>, <<"password">>}
                ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Username">>, AuthUser}
                                                       ,{<<"Realm">>, wh_json:get_value(<<"Auth-Domain">>, JObj)}
                                                       ,{<<"Authorizing-ID">>, ConfId}
                                                       ,{<<"Conference-ID">>, ConfId}]}}
                | whistle_api:default_headers(Q, <<"directory">>, <<"auth_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = whistle_api:auth_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).
