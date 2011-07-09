%%%============================================================================
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%% This module is responsible for accepting conference calls, it is the
%%% the first stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(conf_responder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("conference.hrl").

-define(SERVER, ?MODULE).

-record(state, {
            self :: pid()
	   ,amqp_q = <<>> :: binary()
         }).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    ?LOG_SYS("starting new conference responder"),
    {ok, #state{self=self()}, 0}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
handle_info(timeout, #state{amqp_q = <<>>}=State) ->
    try
	{ok, Q} = start_amqp(),
	{noreply, State#state{amqp_q=Q}}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};




handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
terminate( _Reason, _State) ->
    ?LOG_SYS("conference responder ~p termination", [_Reason]),
    ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    try
        _ = amqp_util:callmgr_exchange(),
        _ = amqp_util:targeted_exchange(),
        _ = amqp_util:callevt_exchange(),
	Q = amqp_util:new_queue(),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTH_REQ),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
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
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, JObj, #state{amqp_q=Q}) ->
    try
        <<"INVITE">> = wh_json:get_value(<<"Method">>, JObj),
        io:format("INVITE AUTH ~p~n", [JObj]),
        [<<"conference">>, _] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
        AccountId = wh_json:get_value(<<"Auth-User">>, JObj),
        true = couch_mgr:db_exists(whapps_util:get_db_name(AccountId, encoded)),
        send_auth_response(JObj, Q)
    catch
        _:_ ->
            ok
    end;

process_req({<<"dialplan">>, <<"route_req">>}, JObj, #state{amqp_q=Q}) ->
    try
        [<<"conference">>, _] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
        _ = send_route_response(JObj, Q)
    catch
        _:_ ->
            ok
    end;

process_req({_, <<"route_win">>}, JObj, #state{amqp_q=Q}) ->
    io:format("route win!!!!~n", []);

process_req(_, _, _) ->
    ok.

send_route_response(JObj, Q) ->
    io:format("sending route response~n", []),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Routes">>, []}
                ,{<<"Method">>, <<"park">>}
                | whistle_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = whistle_api:route_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

send_auth_response(JObj, Q) ->
    io:format("sending auth response~n", []),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                ,{<<"Auth-Password">>, <<"test">>}
                ,{<<"Auth-Method">>, <<"password">>}
                | whistle_api:default_headers(Q, <<"directory">>, <<"auth_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = whistle_api:auth_resp(Response),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).
